(() => {
  const isPdfPath = (path) => typeof path === 'string' && /\.pdf(\?|#|$)/i.test(path);

  const toReaderUrl = (path) => `viewer.html?file=${encodeURIComponent(path)}`;

  function extractRepoPathFromUrl(url) {
    try {
      const u = new URL(url, location.href);
      const marker = '/Cyclotron123/BSDS_Materials/main/';
      const idx = u.pathname.indexOf(marker);
      if (idx !== -1) return u.pathname.slice(idx + marker.length);
      return null;
    } catch {
      return null;
    }
  }

  function rewritePdfLinks(root = document) {
    root.querySelectorAll('a.action-icon-btn').forEach((a) => {
      const href = a.getAttribute('href') || '';
      if (!/\.pdf(\?|#|$)/i.test(href)) return;
      const path = extractRepoPathFromUrl(href);
      if (!path) return;
      a.href = toReaderUrl(path);
      a.target = '_blank';
      a.rel = 'noopener noreferrer';
      a.title = 'Open in PDF reader';
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
      const fileName = e.target.closest?.('.file-row .file-name, .file-row .action-icon-btn[title="Open in new tab"]');
      if (!fileName) return;
      const row = fileName.closest('.file-row');
      if (!row) return;

      const openLink = row.querySelector('a.action-icon-btn[title="Open in new tab"]');
      if (!openLink) return;

      const href = openLink.href || '';
      if (!/\.pdf(\?|#|$)/i.test(href)) return;

      const path = extractRepoPathFromUrl(href);
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
