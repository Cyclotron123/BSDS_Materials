(() => {
  const isPdfPath = (path) => typeof path === 'string' && /\.pdf(\?|#|$)/i.test(path);

  // Use Lumina PDF viewer
  const toReaderUrl = (path) => `viewer.html?file=${encodeURIComponent(path)}`;

  function extractRepoPathFromUrl(url) {
    try {
      const u = new URL(url, location.href);
      const markers = [
        '/CyclotronPulsar/BSDS_Materials/main/',
        '/CyclotronPulsar/BSDS_Materials/master/',
        '/Cyclotron123/BSDS_Materials/main/',
        '/BSDS_Materials/main/',
      ];
      for (const marker of markers) {
        const idx = u.pathname.indexOf(marker);
        if (idx !== -1) return u.pathname.slice(idx + marker.length);
      }
      if (/^\/?(BSDS_[123]\/).+\.pdf$/i.test(u.pathname)) {
        return u.pathname.replace(/^\//, '');
      }
      return null;
    } catch {
      return null;
    }
  }

  function isDownloadButton(a) {
    if (!a) return false;
    if (a.hasAttribute('download')) return true;
    const title = (a.getAttribute('title') || '').toLowerCase();
    if (title.includes('download')) return true;
    if (a.classList.contains('btn-download') || a.dataset.action === 'download') return true;
    return false;
  }

  // Only rewrite "Open" links — never the Download button
  function rewritePdfLinks(root = document) {
    root.querySelectorAll('a.action-icon-btn, a[href*=".pdf"]').forEach((a) => {
      if (isDownloadButton(a)) return;

      const href = a.getAttribute('href') || '';
      if (!/\.pdf(\?|#|$)/i.test(href)) return;
      const path = extractRepoPathFromUrl(href) || (href.match(/(BSDS_[123]\/[^?#]+\.pdf)/i) || [])[1];
      if (!path) return;
      a.href = toReaderUrl(path);
      a.target = '_blank';
      a.rel = 'noopener noreferrer';
      a.title = 'Open in Lumina PDF';
    });
  }

  const patchOpenFile = () => {
    if (typeof window.openFile !== 'function') return false;
    const original = window.openFile.bind(window);
    window.openFile = async function (path) {
      if (isPdfPath(path)) {
        window.location.href = toReaderUrl(path);
        return;
      }
      return original(path);
    };
    return true;
  };

  const boot = () => {
    patchOpenFile();

    document.addEventListener('click', (e) => {
      const hit = e.target.closest?.('.file-row .file-name, .file-row .action-icon-btn');
      if (!hit) return;

      const maybeBtn = e.target.closest?.('a.action-icon-btn');
      // Never intercept the download icon
      if (maybeBtn && isDownloadButton(maybeBtn)) return;

      const row = hit.closest('.file-row');
      if (!row) return;

      const openLink =
        row.querySelector('a.action-icon-btn[title="Open in new tab"]') ||
        row.querySelector('a.action-icon-btn[title="Open in Lumina PDF"]') ||
        Array.from(row.querySelectorAll('a.action-icon-btn')).find((a) => !isDownloadButton(a));

      if (!openLink) return;

      const href = openLink.href || openLink.getAttribute('href') || '';
      if (!/\.pdf(\?|#|$)/i.test(href) && !href.includes('viewer.html')) return;

      const clickedOpen =
        hit.classList?.contains('file-name') ||
        (maybeBtn && !isDownloadButton(maybeBtn));
      if (!clickedOpen) return;

      let path = extractRepoPathFromUrl(href);
      if (!path && href.includes('viewer.html')) {
        try {
          path = new URL(href, location.href).searchParams.get('file');
        } catch {}
      }
      if (!path) {
        const m = href.match(/(BSDS_[123]\/[^?#]+\.pdf)/i);
        if (m) path = m[1];
      }
      if (!path) return;

      e.preventDefault();
      e.stopImmediatePropagation();
      if (e.metaKey || e.ctrlKey || (maybeBtn && !isDownloadButton(maybeBtn))) {
        window.open(toReaderUrl(path), '_blank', 'noopener,noreferrer');
      } else {
        window.location.href = toReaderUrl(path);
      }
    }, true);

    const observer = new MutationObserver((mutations) => {
      for (const mutation of mutations) {
        mutation.addedNodes.forEach((node) => {
          if (!(node instanceof Element)) return;
          rewritePdfLinks(node);
        });
      }
    });

    observer.observe(document.documentElement, { childList: true, subtree: true });
    rewritePdfLinks();
  };

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', boot, { once: true });
  } else {
    boot();
  }
})();
