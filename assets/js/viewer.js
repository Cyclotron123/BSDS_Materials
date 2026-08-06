/**
 * Lumina PDF Web – Core Viewer
 * Compatible with PDF.js 3.11.174 (cdnjs)
 * Drop into BSDS_Materials and open via viewer.html?file=...
 */

(() => {
  'use strict';

  // --------------------------------------------------
  // Constants
  // --------------------------------------------------
  const WORKER = 'https://cdnjs.cloudflare.com/ajax/libs/pdf.js/3.11.174/pdf.worker.min.js';
  const SCALE_STEP = 0.15;
  const MIN_SCALE = 0.4;
  const MAX_SCALE = 4.0;
  const THEMES = ['dark', 'light', 'invert', 'sepia', 'amoled', 'eye-comfort', 'smart-dark'];

  // --------------------------------------------------
  // DOM helpers
  // --------------------------------------------------
  const $ = (id) => document.getElementById(id);
  const els = {
    back: $('btn-back'),
    title: $('doc-title'),
    prev: $('btn-prev'),
    next: $('btn-next'),
    pageInput: $('page-input'),
    pageTotal: $('page-total'),
    zoomOut: $('btn-zoom-out'),
    zoomIn: $('btn-zoom-in'),
    zoomLabel: $('zoom-label'),
    fitWidth: $('btn-fit-width'),
    fitPage: $('btn-fit-page'),
    searchInput: $('search-input'),
    searchPrev: $('btn-search-prev'),
    searchNext: $('btn-search-next'),
    theme: $('btn-theme'),
    sidebarBtn: $('btn-sidebar'),
    fullscreen: $('btn-fullscreen'),
    download: $('btn-download'),
    print: $('btn-print'),
    sidebar: $('sidebar'),
    sidebarContent: $('sidebar-content'),
    panelThumbs: $('panel-thumbs'),
    panelOutline: $('panel-outline'),
    viewerWrap: $('viewer-wrap'),
    viewer: $('viewer'),
    status: $('status'),
  };

  // --------------------------------------------------
  // State
  // --------------------------------------------------
  const state = {
    pdfDoc: null,
    filePath: '',
    fileName: '',
    pageCount: 0,
    currentPage: 1,
    scale: 1,
    zoomMode: 'fit-width', // fit-width | fit-page | custom
    theme: localStorage.getItem('lumina-theme') || 'dark',
    sidebarOpen: false,
    searchTerm: '',
    searchMatches: [], // {page, start, end}
    currentMatch: -1,
    pageStates: [],
    renderToken: 0,
    baseViewport: null,
    observer: null,
  };

  // --------------------------------------------------
  // Utilities
  // --------------------------------------------------
  function setStatus(msg) {
    els.status.textContent = msg;
  }

  function setTitle(name) {
    els.title.textContent = name;
    document.title = `${name} · Lumina PDF`;
  }

  function clamp(v, min, max) {
    return Math.max(min, Math.min(max, v));
  }

  function normalizePage(n) {
    return clamp(n || 1, 1, state.pageCount || 1);
  }

  // Allowed paths (same security as original reader)
  function sanitizePath(raw) {
    if (!raw) return null;
    let v = String(raw).trim();
    try { v = decodeURIComponent(v); } catch {}
    v = v.replace(/\\/g, '/');
    if (/^(https?:|data:|javascript:)/i.test(v)) return null;
    if (v.includes('..')) return null;
    // Allow BSDS folders + any .pdf under them
    if (!/^(BSDS_1|BSDS_2|BSDS_3)\/.+\.pdf$/i.test(v) && !v.endsWith('.pdf')) {
      // also allow relative paths that end with .pdf for flexibility
      if (!/\.pdf$/i.test(v)) return null;
    }
    return v;
  }

  function getParams() {
    const p = new URLSearchParams(location.search);
    const file = sanitizePath(p.get('file'));
    const page = Math.max(1, parseInt(p.get('page') || '1', 10) || 1);
    return { file, page };
  }

  // --------------------------------------------------
  // Theme
  // --------------------------------------------------
  function applyTheme(theme) {
    state.theme = theme;
    localStorage.setItem('lumina-theme', theme);

    // UI theme
    document.documentElement.classList.toggle('light', theme === 'light');
    document.documentElement.classList.toggle('dark', theme !== 'light');

    // Canvas color modes
    document.querySelectorAll('.page-canvas').forEach((c) => {
      c.classList.remove('invert', 'sepia', 'amoled', 'eye-comfort', 'smart-dark');
      if (['invert', 'sepia', 'amoled', 'eye-comfort', 'smart-dark'].includes(theme)) {
        c.classList.add(theme);
      }
    });

    const icons = { dark: '☾', light: '☀', invert: '◐', sepia: '棕', amoled: '⬤', 'eye-comfort': '👁', 'smart-dark': '◑' };
    els.theme.textContent = icons[theme] || '☾';
    setStatus(`Theme: ${theme}`);
  }

  function cycleTheme() {
    const idx = THEMES.indexOf(state.theme);
    applyTheme(THEMES[(idx + 1) % THEMES.length]);
  }

  // --------------------------------------------------
  // Page state helpers
  // --------------------------------------------------
  function getPageState(n) {
    if (!state.pageStates[n]) {
      state.pageStates[n] = {
        canvas: null,
        textLayer: null,
        container: null,
        renderedScale: 0,
        rendering: null,
        viewport: null,
        textContent: null,
      };
    }
    return state.pageStates[n];
  }

  function buildPageSkeleton(n) {
    const page = document.createElement('article');
    page.className = 'page';
    page.id = `page-${n}`;
    page.dataset.page = String(n);

    const loading = document.createElement('div');
    loading.className = 'page-loading';
    loading.textContent = `Page ${n}`;
    page.appendChild(loading);

    const canvas = document.createElement('canvas');
    canvas.className = 'page-canvas';
    page.appendChild(canvas);

    const textLayer = document.createElement('div');
    textLayer.className = 'textLayer';
    page.appendChild(textLayer);

    const ps = getPageState(n);
    ps.canvas = canvas;
    ps.textLayer = textLayer;
    ps.container = page;
    return page;
  }

  // --------------------------------------------------
  // Text layer – Chrome/Edge quality selection
  // --------------------------------------------------
  async function buildTextLayer(pageNum, page, viewport) {
    const ps = getPageState(pageNum);
    const layer = ps.textLayer;
    if (!layer) return;

    layer.innerHTML = '';
    layer.style.width = Math.floor(viewport.width) + 'px';
    layer.style.height = Math.floor(viewport.height) + 'px';

    // Critical for correct font sizing & selection alignment
    layer.style.setProperty('--total-scale-factor', viewport.scale);

    const textContent = await page.getTextContent({
      includeMarkedContent: true,
      disableNormalization: true,
    });
    ps.textContent = textContent;

    // Prefer official renderTextLayer (PDF.js 3.11)
    if (typeof pdfjsLib.renderTextLayer === 'function') {
      try {
        const textDivs = [];
        const task = pdfjsLib.renderTextLayer({
          textContentSource: textContent,
          container: layer,
          viewport,
          textDivs,
        });
        await task.promise;

        // Required for multi-line / drag selection (same as official viewer)
        const end = document.createElement('div');
        end.className = 'endOfContent';
        layer.appendChild(end);
        return;
      } catch (e) {
        console.warn('renderTextLayer failed, using fallback', e);
        layer.innerHTML = '';
      }
    }

    // High-quality manual fallback (still very close to native)
    for (const item of textContent.items) {
      if (!item.str) continue;
      const span = document.createElement('span');
      span.textContent = item.str;

      const tx = pdfjsLib.Util.transform(viewport.transform, item.transform);
      const fontHeight = Math.hypot(tx[2], tx[3]) || 12;
      const angle = Math.atan2(tx[1], tx[0]);

      span.style.left = tx[4] + 'px';
      span.style.top = (tx[5] - fontHeight) + 'px';
      span.style.fontSize = fontHeight + 'px';
      span.style.fontFamily = 'sans-serif';
      span.style.transformOrigin = '0% 0%';
      span.style.setProperty('--font-height', fontHeight + 'px');

      let transform = '';
      if (angle !== 0) {
        transform += `rotate(${angle}rad) `;
      }
      const scaleX = item.width
        ? (item.width * viewport.scale) / (Math.hypot(tx[0], tx[1]) || 1)
        : 1;
      if (Math.abs(scaleX - 1) > 0.01) {
        transform += `scaleX(${scaleX})`;
      }
      if (transform) span.style.transform = transform;

      layer.appendChild(span);
    }

    // endOfContent for selection behavior
    const end = document.createElement('div');
    end.className = 'endOfContent';
    layer.appendChild(end);
  }

  // --------------------------------------------------
  // Render single page
  // --------------------------------------------------
  async function renderPage(pageNum, { force = false } = {}) {
    const ps = getPageState(pageNum);
    if (!state.pdfDoc || !ps.container) return;
    if (ps.rendering) return ps.rendering;
    if (!force && ps.renderedScale === state.scale && ps.viewport) return;

    const token = ++state.renderToken;
    ps.rendering = (async () => {
      try {
        const page = await state.pdfDoc.getPage(pageNum);
        const viewport = page.getViewport({ scale: state.scale });
        ps.viewport = viewport;

        const canvas = ps.canvas;
        const ctx = canvas.getContext('2d', { alpha: false });
        const dpr = window.devicePixelRatio || 1;

        canvas.width = Math.floor(viewport.width * dpr);
        canvas.height = Math.floor(viewport.height * dpr);
        canvas.style.width = viewport.width + 'px';
        canvas.style.height = viewport.height + 'px';

        // Apply current theme class
        canvas.classList.remove('invert', 'sepia', 'amoled', 'eye-comfort', 'smart-dark');
        if (['invert', 'sepia', 'amoled', 'eye-comfort', 'smart-dark'].includes(state.theme)) {
          canvas.classList.add(state.theme);
        }

        const renderCtx = {
          canvasContext: ctx,
          viewport,
          transform: dpr !== 1 ? [dpr, 0, 0, dpr, 0, 0] : null,
        };

        ps.container.style.width = viewport.width + 'px';
        ps.container.style.height = viewport.height + 'px';
        ps.textLayer.style.width = viewport.width + 'px';
        ps.textLayer.style.height = viewport.height + 'px';

        const loading = ps.container.querySelector('.page-loading');
        if (loading) loading.remove();

        await page.render(renderCtx).promise;
        if (token !== state.renderToken) return;

        await buildTextLayer(pageNum, page, viewport);
        ps.renderedScale = state.scale;

        // Re-apply search highlights if needed
        if (state.searchTerm) applyHighlightsOnPage(pageNum);
      } catch (err) {
        console.error('Render error', pageNum, err);
        ps.container.innerHTML = `<div class="page-loading">Failed to render page ${pageNum}</div>`;
      } finally {
        ps.rendering = null;
      }
    })();

    return ps.rendering;
  }

  // --------------------------------------------------
  // Zoom / Fit
  // --------------------------------------------------
  function getFitScale(mode) {
    if (!state.baseViewport || !els.viewerWrap) return state.scale;
    const availW = Math.max(280, els.viewer.clientWidth - 40);
    const availH = Math.max(360, els.viewerWrap.clientHeight - 40);
    const { width, height } = state.baseViewport;

    if (mode === 'fit-page') {
      return clamp(Math.min(availW / width, availH / height) * 0.97, MIN_SCALE, MAX_SCALE);
    }
    return clamp(availW / width, MIN_SCALE, MAX_SCALE);
  }

  function setZoom(scale, mode = 'custom') {
    state.scale = clamp(scale, MIN_SCALE, MAX_SCALE);
    state.zoomMode = mode;
    els.zoomLabel.textContent = Math.round(state.scale * 100) + '%';
    if (state.pdfDoc) renderVisible(true);
  }

  // --------------------------------------------------
  // Navigation
  // --------------------------------------------------
  function scrollToPage(n, behavior = 'smooth') {
    const el = els.viewer.querySelector(`[data-page="${n}"]`);
    if (el) el.scrollIntoView({ behavior, block: 'start' });
  }

  function setCurrentPage(n, syncInput = true) {
    state.currentPage = normalizePage(n);
    if (syncInput) els.pageInput.value = String(state.currentPage);
    // Update URL hash lightly
    const url = new URL(location.href);
    url.searchParams.set('page', String(state.currentPage));
    history.replaceState(null, '', url);
    // Highlight thumb
    document.querySelectorAll('.thumb-item').forEach((t) => {
      t.classList.toggle('active', Number(t.dataset.page) === state.currentPage);
    });
  }

  // --------------------------------------------------
  // Search
  // --------------------------------------------------
  function clearHighlights() {
    document.querySelectorAll('.textLayer .highlight').forEach((el) => {
      el.classList.remove('highlight', 'selected');
    });
  }

  function applyHighlightsOnPage(pageNum) {
    if (!state.searchTerm) return;
    const ps = getPageState(pageNum);
    if (!ps.textLayer) return;

    const match = state.searchMatches[state.currentMatch];
    const spans = ps.textLayer.querySelectorAll('span');
    let charPos = 0;

    spans.forEach((span) => {
      const len = (span.textContent || '').length;
      const start = charPos;
      const end = charPos + len;
      charPos = end;

      // Any match that overlaps this span
      const hits = state.searchMatches.filter(
        (m) => m.page === pageNum && end > m.start && start < m.end
      );
      if (hits.length) {
        span.classList.add('highlight');
        if (match && match.page === pageNum && end > match.start && start < match.end) {
          span.classList.add('selected');
        }
      }
    });
  }

  async function runSearch(term) {
    state.searchTerm = (term || '').trim().toLowerCase();
    state.searchMatches = [];
    state.currentMatch = -1;
    clearHighlights();

    if (!state.searchTerm || !state.pdfDoc) {
      setStatus(state.pageCount ? `Loaded ${state.pageCount} pages` : 'Ready');
      return;
    }

    setStatus(`Searching “${state.searchTerm}”…`);

    for (let p = 1; p <= state.pageCount; p++) {
      let ps = getPageState(p);
      if (!ps.textContent) {
        try {
          const page = await state.pdfDoc.getPage(p);
          ps.textContent = await page.getTextContent();
        } catch { continue; }
      }

      let pageText = '';
      for (const item of ps.textContent.items) {
        pageText += item.str || '';
      }
      const lower = pageText.toLowerCase();
      let from = 0;
      let pos;
      while ((pos = lower.indexOf(state.searchTerm, from)) !== -1) {
        state.searchMatches.push({ page: p, start: pos, end: pos + state.searchTerm.length });
        from = pos + 1;
      }
    }

    if (!state.searchMatches.length) {
      setStatus(`No matches for “${state.searchTerm}”`);
      return;
    }

    state.currentMatch = 0;
    const first = state.searchMatches[0];
    setCurrentPage(first.page);
    scrollToPage(first.page);
    await renderPage(first.page);
    applyHighlightsOnPage(first.page);
    setStatus(`${state.searchMatches.length} match${state.searchMatches.length > 1 ? 'es' : ''} found`);
  }

  function jumpMatch(dir) {
    if (!state.searchMatches.length) return;
    state.currentMatch =
      (state.currentMatch + dir + state.searchMatches.length) % state.searchMatches.length;
    const m = state.searchMatches[state.currentMatch];
    setCurrentPage(m.page);
    scrollToPage(m.page);
    renderPage(m.page).then(() => {
      clearHighlights();
      // re-highlight all + current
      state.searchMatches.forEach((match) => {
        if (match.page === m.page) applyHighlightsOnPage(match.page);
      });
      applyHighlightsOnPage(m.page);
    });
    setStatus(`Match ${state.currentMatch + 1} of ${state.searchMatches.length}`);
  }

  // --------------------------------------------------
  // Visible page rendering + observer
  // --------------------------------------------------
  async function renderVisible(force = false) {
    if (!state.pdfDoc) return;
    const candidates = new Set([state.currentPage, state.currentPage - 1, state.currentPage + 1]);
    els.viewer.querySelectorAll('.page').forEach((el) => {
      const n = Number(el.dataset.page);
      if (n) candidates.add(n);
    });
    for (const n of candidates) {
      if (n >= 1 && n <= state.pageCount) await renderPage(n, { force });
    }
  }

  function observePages() {
    if (state.observer) state.observer.disconnect();
    state.observer = new IntersectionObserver(
      (entries) => {
        let best = { page: state.currentPage, ratio: 0 };
        for (const e of entries) {
          const n = Number(e.target.dataset.page);
          if (e.isIntersecting && e.intersectionRatio >= best.ratio) {
            best = { page: n, ratio: e.intersectionRatio };
            renderPage(n);
          }
        }
        if (best.page) setCurrentPage(best.page);
      },
      { root: els.viewerWrap, threshold: [0.25, 0.5, 0.7], rootMargin: '200px 0px' }
    );
    els.viewer.querySelectorAll('.page').forEach((p) => state.observer.observe(p));
  }

  // --------------------------------------------------
  // Build viewer + thumbs
  // --------------------------------------------------
  function buildViewer() {
    els.viewer.innerHTML = '';
    state.pageStates = new Array(state.pageCount + 1).fill(null);
    for (let i = 1; i <= state.pageCount; i++) {
      els.viewer.appendChild(buildPageSkeleton(i));
    }
    observePages();

    // Thumbnails
    els.panelThumbs.innerHTML = '';
    for (let i = 1; i <= state.pageCount; i++) {
      const btn = document.createElement('button');
      btn.className = 'thumb-item' + (i === state.currentPage ? ' active' : '');
      btn.dataset.page = String(i);
      btn.textContent = `Page ${i}`;
      btn.addEventListener('click', () => {
        setCurrentPage(i);
        scrollToPage(i);
      });
      els.panelThumbs.appendChild(btn);
    }
  }

  // --------------------------------------------------
  // Load PDF
  // --------------------------------------------------
  async function loadPdf() {
    const { file, page } = getParams();
    if (!file) {
      setTitle('No document');
      setStatus('Missing or invalid ?file= parameter');
      return;
    }

    state.filePath = file;
    state.fileName = file.split('/').pop() || 'document.pdf';
    setTitle(state.fileName);
    setStatus('Loading PDF…');

    try {
      if (!window.pdfjsLib) throw new Error('PDF.js failed to load');
      pdfjsLib.GlobalWorkerOptions.workerSrc = WORKER;

      const task = pdfjsLib.getDocument({ url: file });
      task.onProgress = (p) => {
        if (p.total) setStatus(`Loading… ${Math.round((p.loaded / p.total) * 100)}%`);
      };

      state.pdfDoc = await task.promise;
      state.pageCount = state.pdfDoc.numPages;
      els.pageTotal.textContent = `/ ${state.pageCount}`;
      els.pageInput.max = String(state.pageCount);

      buildViewer();

      const first = await state.pdfDoc.getPage(1);
      state.baseViewport = first.getViewport({ scale: 1 });
      setZoom(getFitScale('fit-width'), 'fit-width');

      const startPage = normalizePage(page);
      await renderPage(startPage, { force: true });
      if (startPage > 1) await renderPage(1, { force: true });

      scrollToPage(startPage, 'auto');
      setCurrentPage(startPage);
      setStatus(`Loaded ${state.pageCount} pages`);
    } catch (err) {
      console.error(err);
      setTitle('Error');
      setStatus('Failed to load PDF – check the file path');
      els.viewer.innerHTML = `<div class="page-loading">Could not open this document.</div>`;
    }
  }

  // --------------------------------------------------
  // Controls
  // --------------------------------------------------
  function initControls() {
    els.back.addEventListener('click', () => {
      if (document.referrer && new URL(document.referrer).origin === location.origin) {
        history.back();
      } else {
        location.href = 'index.html';
      }
    });

    els.prev.addEventListener('click', () => {
      const p = normalizePage(state.currentPage - 1);
      setCurrentPage(p);
      scrollToPage(p);
    });
    els.next.addEventListener('click', () => {
      const p = normalizePage(state.currentPage + 1);
      setCurrentPage(p);
      scrollToPage(p);
    });

    els.pageInput.addEventListener('change', () => {
      const p = normalizePage(parseInt(els.pageInput.value, 10));
      setCurrentPage(p);
      scrollToPage(p);
    });

    els.zoomOut.addEventListener('click', () => setZoom(state.scale - SCALE_STEP));
    els.zoomIn.addEventListener('click', () => setZoom(state.scale + SCALE_STEP));
    els.fitWidth.addEventListener('click', () => setZoom(getFitScale('fit-width'), 'fit-width'));
    els.fitPage.addEventListener('click', () => setZoom(getFitScale('fit-page'), 'fit-page'));

    let searchTimer;
    els.searchInput.addEventListener('input', () => {
      clearTimeout(searchTimer);
      searchTimer = setTimeout(() => runSearch(els.searchInput.value), 280);
    });
    els.searchPrev.addEventListener('click', () => jumpMatch(-1));
    els.searchNext.addEventListener('click', () => jumpMatch(1));

    els.theme.addEventListener('click', cycleTheme);
    els.sidebarBtn.addEventListener('click', () => {
      state.sidebarOpen = !state.sidebarOpen;
      els.sidebar.classList.toggle('hidden', !state.sidebarOpen);
    });

    els.fullscreen.addEventListener('click', async () => {
      try {
        if (!document.fullscreenElement) await document.documentElement.requestFullscreen();
        else await document.exitFullscreen();
      } catch {}
    });

    els.download.addEventListener('click', () => {
      const a = document.createElement('a');
      a.href = state.filePath;
      a.download = state.fileName;
      a.click();
    });

    els.print.addEventListener('click', () => {
      const iframe = document.createElement('iframe');
      iframe.style.cssText = 'position:fixed;right:0;bottom:0;width:0;height:0;border:0';
      iframe.src = state.filePath;
      document.body.appendChild(iframe);
      iframe.onload = () => {
        try {
          iframe.contentWindow.focus();
          iframe.contentWindow.print();
        } catch {
          window.open(state.filePath, '_blank');
        }
        setTimeout(() => iframe.remove(), 1000);
      };
    });

    // Sidebar tabs
    document.querySelectorAll('.tab').forEach((tab) => {
      tab.addEventListener('click', () => {
        document.querySelectorAll('.tab').forEach((t) => t.classList.remove('active'));
        tab.classList.add('active');
        const name = tab.dataset.tab;
        document.querySelectorAll('.panel').forEach((p) => p.classList.add('hidden'));
        const panel = document.getElementById(`panel-${name}`);
        if (panel) panel.classList.remove('hidden');
      });
    });

    // Resize
    window.addEventListener('resize', () => {
      if (state.zoomMode === 'fit-width' || state.zoomMode === 'fit-page') {
        setZoom(getFitScale(state.zoomMode), state.zoomMode);
      }
    });

    // Keyboard
    document.addEventListener('keydown', (e) => {
      const tag = document.activeElement?.tagName?.toLowerCase();
      const typing = tag === 'input' || tag === 'textarea';

      if (e.ctrlKey || e.metaKey) {
        if (e.key.toLowerCase() === 'f') {
          e.preventDefault();
          els.searchInput.focus();
          els.searchInput.select();
        }
        if (e.key === '=' || e.key === '+') {
          e.preventDefault();
          setZoom(state.scale + SCALE_STEP);
        }
        if (e.key === '-') {
          e.preventDefault();
          setZoom(state.scale - SCALE_STEP);
        }
        if (e.key === '0') {
          e.preventDefault();
          setZoom(getFitScale('fit-width'), 'fit-width');
        }
        if (e.key.toLowerCase() === 'd') {
          e.preventDefault();
          cycleTheme();
        }
        if (e.key.toLowerCase() === 'p') {
          e.preventDefault();
          els.print.click();
        }
        return;
      }

      if (e.key === 'Escape' && document.fullscreenElement) {
        document.exitFullscreen();
        return;
      }
      if (typing) return;

      if (e.key === 'ArrowDown' || e.key === 'PageDown' || e.key === ' ') {
        e.preventDefault();
        els.next.click();
      }
      if (e.key === 'ArrowUp' || e.key === 'PageUp') {
        e.preventDefault();
        els.prev.click();
      }
      if (e.key === 'Home') {
        e.preventDefault();
        setCurrentPage(1);
        scrollToPage(1);
      }
      if (e.key === 'End') {
        e.preventDefault();
        setCurrentPage(state.pageCount);
        scrollToPage(state.pageCount);
      }
      if (e.key === 'F11') {
        e.preventDefault();
        els.fullscreen.click();
      }
    });
  }

  // --------------------------------------------------
  // Boot
  // --------------------------------------------------
  function boot() {
    applyTheme(state.theme);
    initControls();
    loadPdf();
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot);
  } else {
    boot();
  }
})();
