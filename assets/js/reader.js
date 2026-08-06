(() => {
  const WORKER_SRC = 'https://cdnjs.cloudflare.com/ajax/libs/pdf.js/3.11.174/pdf.worker.min.js';
  const SCALE_STEP = 0.15;
  const MIN_SCALE = 0.45;
  const MAX_SCALE = 3.0;

  const $ = (id) => document.getElementById(id);
  const els = {
    backBtn: $('backBtn'),
    docTitle: $('docTitle'),
    prevPage: $('prevPage'),
    nextPage: $('nextPage'),
    pageInput: $('pageInput'),
    pageTotal: $('pageTotal'),
    zoomOut: $('zoomOut'),
    zoomIn: $('zoomIn'),
    zoomLabel: $('zoomLabel'),
    fitWidth: $('fitWidth'),
    fitPage: $('fitPage'),
    searchInput: $('searchInput'),
    searchPrev: $('searchPrev'),
    searchNext: $('searchNext'),
    themeToggle: $('themeToggle'),
    fullscreenBtn: $('fullscreenBtn'),
    downloadBtn: $('downloadBtn'),
    printBtn: $('printBtn'),
    statusBar: $('statusBar'),
    viewerWrap: $('viewerWrap'),
    viewer: $('viewer')
  };

  const state = {
    pdfDoc: null,
    filePath: '',
    fileName: '',
    pageCount: 0,
    currentScale: 1,
    zoomMode: 'fit-width',
    renderToken: 0,
    pageStates: [],
    baseViewport: null,
    observer: null,
    activePage: 1,
    searchTerm: '',
    searchMatches: [],
    currentMatchIndex: -1,
    darkMode: false,
    initialPage: 1,
    loadingDone: 0,
  };

  const sanitizeFilePath = (raw) => {
    if (!raw) return null;
    let value = String(raw).trim();
    try { value = decodeURIComponent(value); } catch {}
    value = value.replace(/\\/g, '/');

    if (/^(https?:|data:|javascript:)/i.test(value)) return null;
    if (value.includes('..')) return null;

    const allowed = /^(BSDS_1|BSDS_2|BSDS_3)\/[A-Za-z0-9._\-\/%()+, ]+\.pdf$/i;
    if (!allowed.test(value)) return null;

    return value;
  };

  const fileFromLocation = () => {
    const params = new URLSearchParams(location.search);
    const raw = params.get('file');
    const page = Math.max(1, parseInt(params.get('page') || '1', 10) || 1);
    return { file: sanitizeFilePath(raw), page };
  };

  const setStatus = (text) => {
    els.statusBar.textContent = text;
  };

  const setDocTitle = (title) => {
    els.docTitle.textContent = title;
    document.title = `${title} · PDF Reader`;
  };

  const updateTheme = (dark) => {
    state.darkMode = !!dark;
    document.body.classList.toggle('theme-dark', state.darkMode);
    els.themeToggle.textContent = state.darkMode ? '☀' : '☾';
    localStorage.setItem('bsds_pdf_theme', state.darkMode ? 'dark' : 'light');
  };

  const loadTheme = () => {
    const saved = localStorage.getItem('bsds_pdf_theme');
    if (saved === 'dark') return updateTheme(true);
    if (saved === 'light') return updateTheme(false);
    const preferDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
    updateTheme(preferDark);
  };

  const getPdfUrl = () => state.filePath;

  const getScaleForMode = (mode = state.zoomMode) => {
    if (!state.pdfDoc || !state.baseViewport) return state.currentScale;
    const containerWidth = Math.max(320, els.viewer.clientWidth - 32);
    const containerHeight = Math.max(420, els.viewerWrap.clientHeight - 40);

    const baseWidth = state.baseViewport.width;
    const baseHeight = state.baseViewport.height;

    if (mode === 'fit-page') {
      const scaleW = containerWidth / baseWidth;
      const scaleH = containerHeight / baseHeight;
      return Math.max(MIN_SCALE, Math.min(MAX_SCALE, Math.min(scaleW, scaleH) * 0.97));
    }
    return Math.max(MIN_SCALE, Math.min(MAX_SCALE, containerWidth / baseWidth));
  };

  const setZoom = (scale, { mode = 'custom', rerender = true } = {}) => {
    state.currentScale = Math.max(MIN_SCALE, Math.min(MAX_SCALE, scale));
    state.zoomMode = mode;
    els.zoomLabel.textContent = `${Math.round(state.currentScale * 100)}%`;

    if (state.pdfDoc && rerender) {
      renderAllVisiblePages(true);
    }
  };

  const normalizePageNum = (num) => Math.min(state.pageCount, Math.max(1, num || 1));

  const pageUrl = (pageNum) => `#page-${pageNum}`;

  const scrollToPage = (pageNum, behavior = 'smooth') => {
    const page = els.viewer.querySelector(`[data-page-number="${pageNum}"]`);
    if (page) page.scrollIntoView({ behavior, block: 'start', inline: 'nearest' });
  };

  const setActivePage = (pageNum, syncInput = true) => {
    state.activePage = normalizePageNum(pageNum);
    if (syncInput) els.pageInput.value = String(state.activePage);
    history.replaceState(null, '', `${location.pathname}${location.search}${pageUrl(state.activePage)}`);
  };

  const updateProgress = () => {
    if (!state.pageCount) return;
    const pct = Math.round((state.loadingDone / state.pageCount) * 100);
    setStatus(`Rendering pages… ${state.loadingDone}/${state.pageCount} (${pct}%)`);
  };

  const getPageState = (pageNum) => {
    if (!state.pageStates[pageNum]) {
      state.pageStates[pageNum] = {
        canvas: null,
        textLayer: null,
        container: null,
        renderedScale: 0,
        rendering: null,
        viewport: null,
        textContent: null,
        textSpans: [], // for search
      };
    }
    return state.pageStates[pageNum];
  };

  const buildPageSkeleton = (pageNum) => {
    const page = document.createElement('article');
    page.className = 'page';
    page.id = `page-${pageNum}`;
    page.dataset.pageNumber = String(pageNum);

    const loading = document.createElement('div');
    loading.className = 'page-loading';
    loading.textContent = `Page ${pageNum}`;
    page.appendChild(loading);

    const canvas = document.createElement('canvas');
    canvas.className = 'page-canvas hidden';
    page.appendChild(canvas);

    const textLayer = document.createElement('div');
    textLayer.className = 'text-layer';
    page.appendChild(textLayer);

    const stateObj = getPageState(pageNum);
    stateObj.canvas = canvas;
    stateObj.textLayer = textLayer;
    stateObj.container = page;

    return page;
  };

  // ========== ROBUST TEXT LAYER (compatible with PDF.js 3.11) ==========
  // Uses the classic renderTextLayer when available, falls back to high-quality manual spans.
  const buildTextLayer = async (pageNum, page, viewport) => {
    const stateObj = getPageState(pageNum);
    const layer = stateObj.textLayer;
    if (!layer) return;

    // Clear previous
    layer.innerHTML = '';
    layer.style.width = Math.floor(viewport.width) + 'px';
    layer.style.height = Math.floor(viewport.height) + 'px';

    const textContent = await page.getTextContent();
    stateObj.textContent = textContent;
    stateObj.textSpans = [];

    // Prefer the official renderTextLayer if it exists (PDF.js 3.11 has it)
    if (typeof pdfjsLib.renderTextLayer === 'function') {
      try {
        const textDivs = [];
        const task = pdfjsLib.renderTextLayer({
          textContentSource: textContent,
          container: layer,
          viewport: viewport,
          textDivs: textDivs,
        });
        await task.promise;
        stateObj.textSpans = textDivs;
        return;
      } catch (e) {
        console.warn('renderTextLayer failed, falling back to manual layer', e);
        layer.innerHTML = '';
      }
    }

    // ========== High-quality manual fallback ==========
    // Much more accurate than the original implementation
    for (const item of textContent.items) {
      if (!item.str || !item.str.trim()) continue;

      const span = document.createElement('span');
      span.textContent = item.str;

      // Apply the full transform from PDF.js
      const tx = pdfjsLib.Util.transform(viewport.transform, item.transform);

      // Font height from the transform matrix
      const fontHeight = Math.hypot(tx[2], tx[3]);
      const fontAscent = fontHeight; // good enough for most PDFs

      span.style.left = tx[4] + 'px';
      span.style.top = (tx[5] - fontAscent) + 'px';
      span.style.fontSize = fontHeight + 'px';
      span.style.fontFamily = 'sans-serif';
      span.style.transformOrigin = '0% 0%';

      // Horizontal scaling to match the actual glyph width
      const scaleX = item.width ? (item.width * viewport.scale) / (Math.hypot(tx[0], tx[1]) || 1) : 1;
      if (Math.abs(scaleX - 1) > 0.01) {
        span.style.transform = `scaleX(${scaleX})`;
      }

      layer.appendChild(span);
      stateObj.textSpans.push(span);
    }
  };

  // ========== IMPROVED SEARCH ==========
  const clearHighlights = () => {
    document.querySelectorAll('.text-layer span.highlight').forEach(el => {
      el.classList.remove('highlight', 'selected');
    });
  };

  const runSearch = async (term) => {
    state.searchTerm = (term || '').trim().toLowerCase();
    state.searchMatches = [];
    state.currentMatchIndex = -1;
    clearHighlights();

    if (!state.searchTerm) {
      setStatus(`Loaded ${state.pageCount} pages.`);
      return;
    }

    setStatus(`Searching “${state.searchTerm}”…`);

    for (let pageNum = 1; pageNum <= state.pageCount; pageNum++) {
      let stateObj = getPageState(pageNum);

      if (!stateObj.textContent) {
        try {
          const page = await state.pdfDoc.getPage(pageNum);
          stateObj.textContent = await page.getTextContent();
        } catch (e) {
          continue;
        }
      }

      const items = stateObj.textContent.items;
      let pageText = '';
      const ranges = []; // {start, end, itemIndex}

      for (let i = 0; i < items.length; i++) {
        const start = pageText.length;
        pageText += items[i].str;
        ranges.push({ start, end: pageText.length, itemIndex: i });
      }

      const lower = pageText.toLowerCase();
      let from = 0;
      let pos;

      while ((pos = lower.indexOf(state.searchTerm, from)) !== -1) {
        state.searchMatches.push({
          page: pageNum,
          start: pos,
          end: pos + state.searchTerm.length,
          ranges
        });
        from = pos + 1;
      }
    }

    if (state.searchMatches.length === 0) {
      setStatus(`No matches for “${state.searchTerm}”.`);
      return;
    }

    state.currentMatchIndex = 0;
    await highlightCurrentMatch(true);
    setStatus(`${state.searchMatches.length} match${state.searchMatches.length === 1 ? '' : 'es'} found.`);
  };

  const highlightCurrentMatch = async (scroll = true) => {
    // Remove previous selected
    document.querySelectorAll('.text-layer span.highlight.selected')
      .forEach(el => el.classList.remove('selected'));

    if (state.currentMatchIndex < 0 || !state.searchMatches.length) return;

    const match = state.searchMatches[state.currentMatchIndex];
    const pageNum = match.page;

    // Make sure the page is rendered so spans exist
    await renderPage(pageNum);
    const stateObj = getPageState(pageNum);
    if (!stateObj.textLayer) return;

    // Find spans that overlap the character range of the match
    const spans = stateObj.textLayer.querySelectorAll('span');
    let charPos = 0;

    spans.forEach(span => {
      const len = (span.textContent || '').length;
      const start = charPos;
      const end = charPos + len;
      charPos = end;

      if (end > match.start && start < match.end) {
        span.classList.add('highlight');
        if (scroll) span.classList.add('selected');
      }
    });

    if (scroll) {
      scrollToPage(pageNum);
      const selected = stateObj.textLayer.querySelector('span.highlight.selected');
      if (selected) {
        selected.scrollIntoView({ behavior: 'smooth', block: 'center' });
      }
    }
  };

  const jumpToSearchMatch = (direction) => {
    if (!state.searchMatches.length) return;
    state.currentMatchIndex =
      (state.currentMatchIndex + direction + state.searchMatches.length) %
      state.searchMatches.length;
    highlightCurrentMatch(true);
    setStatus(`Match ${state.currentMatchIndex + 1} of ${state.searchMatches.length}`);
  };

  // ========== RENDER PAGE ==========
  const renderPage = async (pageNum, { force = false } = {}) => {
    const stateObj = getPageState(pageNum);
    if (!state.pdfDoc || !stateObj.container) return;
    if (stateObj.rendering) return stateObj.rendering;
    if (!force && stateObj.renderedScale === state.currentScale && stateObj.viewport) {
      return;
    }

    const taskToken = ++state.renderToken;
    stateObj.rendering = (async () => {
      const page = await state.pdfDoc.getPage(pageNum);
      const viewport = page.getViewport({ scale: state.currentScale });
      stateObj.viewport = viewport;

      const canvas = stateObj.canvas;
      const ctx = canvas.getContext('2d', { alpha: false });

      const outputScale = window.devicePixelRatio || 1;
      canvas.width = Math.floor(viewport.width * outputScale);
      canvas.height = Math.floor(viewport.height * outputScale);
      canvas.style.width = viewport.width + 'px';
      canvas.style.height = viewport.height + 'px';

      const renderCtx = {
        canvasContext: ctx,
        viewport,
        transform: outputScale !== 1 ? [outputScale, 0, 0, outputScale, 0, 0] : null
      };

      stateObj.container.style.width = viewport.width + 'px';
      stateObj.container.style.height = viewport.height + 'px';
      stateObj.container.style.minHeight = viewport.height + 'px';

      stateObj.textLayer.style.width = viewport.width + 'px';
      stateObj.textLayer.style.height = viewport.height + 'px';

      canvas.classList.remove('hidden');
      const loadingEl = stateObj.container.querySelector('.page-loading');
      if (loadingEl) loadingEl.remove();

      const renderTask = page.render(renderCtx);
      await renderTask.promise;

      // Build accurate text layer
      await buildTextLayer(pageNum, page, viewport);

      stateObj.renderedScale = state.currentScale;
      state.loadingDone = Math.max(state.loadingDone, pageNum);
      if (taskToken === state.renderToken) updateProgress();

      // Re-apply highlights if this page has matches
      if (state.searchTerm && state.searchMatches.some(m => m.page === pageNum)) {
        highlightCurrentMatch(false);
      }
    })().catch(err => {
      console.error('Render error page', pageNum, err);
      stateObj.container.innerHTML =
        `<div class="page-loading">Could not render page ${pageNum}.</div>`;
    }).finally(() => {
      stateObj.rendering = null;
    });

    return stateObj.rendering;
  };

  const renderAllVisiblePages = async (force = false) => {
    if (!state.pdfDoc) return;
    const visible = Array.from(els.viewer.querySelectorAll('.page'))
      .map((el) => Number(el.dataset.pageNumber))
      .filter(Boolean);

    const candidates = new Set();
    const active = state.activePage;
    [active, active - 1, active + 1].forEach((n) => {
      if (n >= 1 && n <= state.pageCount) candidates.add(n);
    });
    visible.forEach((n) => candidates.add(n));

    for (const pageNum of candidates) {
      await renderPage(pageNum, { force });
    }
  };

  const ensurePageVisible = async (pageNum) => {
    if (!state.pdfDoc) return;
    await renderPage(pageNum);
    const prev = pageNum - 1;
    const next = pageNum + 1;
    if (prev >= 1) renderPage(prev);
    if (next <= state.pageCount) renderPage(next);
  };

  const observePages = () => {
    if (state.observer) state.observer.disconnect();

    state.observer = new IntersectionObserver((entries) => {
      let best = { page: state.activePage, ratio: 0 };
      for (const entry of entries) {
        const pageNum = Number(entry.target.dataset.pageNumber);
        if (entry.isIntersecting && entry.intersectionRatio >= best.ratio) {
          best = { page: pageNum, ratio: entry.intersectionRatio };
          renderPage(pageNum);
        }
      }
      if (best.page) setActivePage(best.page);
    }, {
      root: els.viewerWrap,
      threshold: [0.25, 0.5, 0.75],
      rootMargin: '250px 0px 250px 0px'
    });

    els.viewer.querySelectorAll('.page').forEach((page) => state.observer.observe(page));
  };

  const buildViewer = () => {
    els.viewer.innerHTML = '';
    state.pageStates = new Array(state.pageCount + 1).fill(null);
    for (let i = 1; i <= state.pageCount; i++) {
      els.viewer.appendChild(buildPageSkeleton(i));
    }
    observePages();
  };

  const updateFitMode = () => {
    if (!state.pdfDoc) return;
    if (state.zoomMode === 'fit-width') {
      setZoom(getScaleForMode('fit-width'), { mode: 'fit-width' });
    } else if (state.zoomMode === 'fit-page') {
      setZoom(getScaleForMode('fit-page'), { mode: 'fit-page' });
    }
  };

  const openBackTarget = () => {
    const fromSameOrigin = document.referrer && new URL(document.referrer).origin === location.origin;
    if (fromSameOrigin) {
      history.back();
      return;
    }
    location.href = 'index.html';
  };

  const downloadCurrentPdf = () => {
    const a = document.createElement('a');
    a.href = getPdfUrl();
    a.download = state.fileName;
    a.click();
  };

  const printCurrentPdf = async () => {
    const frame = document.createElement('iframe');
    frame.style.position = 'fixed';
    frame.style.right = '0';
    frame.style.bottom = '0';
    frame.style.width = '0';
    frame.style.height = '0';
    frame.style.border = '0';
    frame.src = getPdfUrl();
    document.body.appendChild(frame);

    const cleanup = () => {
      setTimeout(() => frame.remove(), 1000);
    };

    frame.onload = () => {
      try {
        frame.contentWindow.focus();
        frame.contentWindow.print();
      } catch {
        window.open(getPdfUrl(), '_blank', 'noopener,noreferrer');
      }
      cleanup();
    };

    setTimeout(cleanup, 5000);
  };

  const setFullscreen = async () => {
    try {
      if (!document.fullscreenElement) {
        await document.documentElement.requestFullscreen();
      } else {
        await document.exitFullscreen();
      }
    } catch (err) {
      console.warn(err);
    }
  };

  const initPdfJs = () => {
    if (!window.pdfjsLib) throw new Error('PDF.js did not load.');
    pdfjsLib.GlobalWorkerOptions.workerSrc = WORKER_SRC;
  };

  const initControls = () => {
    els.backBtn.addEventListener('click', openBackTarget);
    els.prevPage.addEventListener('click', () => scrollToPage(normalizePageNum(state.activePage - 1)));
    els.nextPage.addEventListener('click', () => scrollToPage(normalizePageNum(state.activePage + 1)));

    els.pageInput.addEventListener('change', () => {
      const pageNum = normalizePageNum(parseInt(els.pageInput.value, 10));
      els.pageInput.value = String(pageNum);
      scrollToPage(pageNum);
    });
    els.pageInput.addEventListener('keydown', (e) => {
      if (e.key === 'Enter') {
        const pageNum = normalizePageNum(parseInt(els.pageInput.value, 10));
        els.pageInput.value = String(pageNum);
        scrollToPage(pageNum);
      }
    });

    els.zoomOut.addEventListener('click', () => {
      state.zoomMode = 'custom';
      setZoom(state.currentScale - SCALE_STEP);
    });
    els.zoomIn.addEventListener('click', () => {
      state.zoomMode = 'custom';
      setZoom(state.currentScale + SCALE_STEP);
    });
    els.fitWidth.addEventListener('click', () => {
      setZoom(getScaleForMode('fit-width'), { mode: 'fit-width' });
      renderAllVisiblePages(true);
    });
    els.fitPage.addEventListener('click', () => {
      setZoom(getScaleForMode('fit-page'), { mode: 'fit-page' });
      renderAllVisiblePages(true);
    });

    let searchTimer = null;
    els.searchInput.addEventListener('input', () => {
      clearTimeout(searchTimer);
      searchTimer = setTimeout(() => runSearch(els.searchInput.value), 280);
    });
    els.searchPrev.addEventListener('click', () => jumpToSearchMatch(-1));
    els.searchNext.addEventListener('click', () => jumpToSearchMatch(1));

    els.themeToggle.addEventListener('click', () => updateTheme(!state.darkMode));
    els.fullscreenBtn.addEventListener('click', setFullscreen);
    els.downloadBtn.addEventListener('click', downloadCurrentPdf);
    els.printBtn.addEventListener('click', printCurrentPdf);

    window.addEventListener('resize', () => {
      if (state.zoomMode === 'fit-width' || state.zoomMode === 'fit-page') {
        updateFitMode();
      } else {
        renderAllVisiblePages(true);
      }
    });

    document.addEventListener('keydown', (e) => {
      const tag = document.activeElement?.tagName?.toLowerCase();
      const typing = tag === 'input' || tag === 'textarea';
      if (e.ctrlKey || e.metaKey) {
        if (e.key.toLowerCase() === 'f') {
          e.preventDefault();
          els.searchInput.focus();
          els.searchInput.select();
        }
        return;
      }

      if (e.key === 'Escape' && document.fullscreenElement) {
        document.exitFullscreen();
        return;
      }

      if (typing) return;

      if (e.key === 'PageDown' || e.key === 'ArrowDown') {
        e.preventDefault();
        scrollToPage(normalizePageNum(state.activePage + 1));
      } else if (e.key === 'PageUp' || e.key === 'ArrowUp') {
        e.preventDefault();
        scrollToPage(normalizePageNum(state.activePage - 1));
      } else if (e.key === 'Home') {
        e.preventDefault();
        scrollToPage(1);
      } else if (e.key === 'End') {
        e.preventDefault();
        scrollToPage(state.pageCount);
      } else if (e.key === '+' || (e.shiftKey && e.key === '=')) {
        e.preventDefault();
        state.zoomMode = 'custom';
        setZoom(state.currentScale + SCALE_STEP);
      } else if (e.key === '-') {
        e.preventDefault();
        state.zoomMode = 'custom';
        setZoom(state.currentScale - SCALE_STEP);
      }
    });
  };

  const loadPdf = async () => {
    const { file, page } = fileFromLocation();
    if (!file) {
      setDocTitle('Invalid document');
      setStatus('This PDF path is invalid or not allowed.');
      throw new Error('Invalid file path');
    }

    state.filePath = file;
    state.fileName = file.split('/').pop() || 'document.pdf';
    state.initialPage = page;

    setDocTitle(state.fileName);
    els.downloadBtn.setAttribute('aria-label', `Download ${state.fileName}`);
    els.printBtn.setAttribute('aria-label', `Print ${state.fileName}`);

    setStatus('Loading PDF…');
    const loadingTask = pdfjsLib.getDocument({ url: file });
    loadingTask.onProgress = (p) => {
      if (p && p.total) {
        const pct = Math.round((p.loaded / p.total) * 100);
        setStatus(`Loading PDF… ${pct}%`);
      }
    };

    state.pdfDoc = await loadingTask.promise;
    state.pageCount = state.pdfDoc.numPages;
    els.pageTotal.textContent = `/ ${state.pageCount}`;
    els.pageInput.max = String(state.pageCount);
    state.loadingDone = 0;

    buildViewer();

    const firstPage = await state.pdfDoc.getPage(1);
    state.baseViewport = firstPage.getViewport({ scale: 1 });
    setZoom(getScaleForMode('fit-width'), { mode: 'fit-width', rerender: false });
    els.zoomLabel.textContent = `${Math.round(state.currentScale * 100)}%`;

    state.loadingDone = 0;
    updateProgress();

    await renderPage(1, { force: true });
    if (state.initialPage > 1) {
      await renderPage(state.initialPage, { force: true });
    }

    await ensurePageVisible(state.initialPage);
    updateFitMode();

    if (state.initialPage > 1) {
      scrollToPage(state.initialPage, 'auto');
    } else {
      scrollToPage(1, 'auto');
    }

    state.loadingDone = state.pageCount;
    setStatus(`Loaded ${state.pageCount} pages.`);
    setActivePage(normalizePageNum(state.initialPage));
  };

  const boot = async () => {
    loadTheme();
    initPdfJs();
    initControls();

    try {
      await loadPdf();
    } catch (err) {
      console.error(err);
      setDocTitle('PDF Reader');
      setStatus('Unable to load this PDF. Check the file path and try again.');
      els.viewer.innerHTML = '<div class="page-loading">Failed to load document.</div>';
    }
  };

  boot();
})();
