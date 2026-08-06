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

  function rewritePdfLinks(root = document) {
    root.querySelectorAll('a.action-icon-btn, a[href*=".pdf"]').forEach((a) => {
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
      const fileName = e.target.closest?.('.file-row .file-name, .file-row .action-icon-btn');
      if (!fileName) return;
      const row = fileName.closest('.file-row');
      if (!row) return;

      const openLink = row.querySelector('a.action-icon-btn');
      if (!openLink) return;

      const href = openLink.href || openLink.getAttribute('href') || '';
      if (!/\.pdf(\?|#|$)/i.test(href) && !href.includes('viewer.html')) return;

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
      if (e.metaKey || e.ctrlKey || fileName.matches?.('a.action-icon-btn')) {
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
