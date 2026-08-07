/* 
  BSDS Materials - Application Logic
*/

const CONFIG = {
  owner: 'Cyclotron123',
  repo: 'BSDS_Materials',
  branch: 'main',
  token: '' // Optional: Add a GitHub PAT to increase rate limits
};

const RAW_BASE = `https://raw.githubusercontent.com/${CONFIG.owner}/${CONFIG.repo}/${CONFIG.branch}/`;
const VIEW_BASE = `https://raw.githack.com/${CONFIG.owner}/${CONFIG.repo}/${CONFIG.branch}/`;

function apiTreeUrl(branch) {
  return `https://api.github.com/repos/${CONFIG.owner}/${CONFIG.repo}/git/trees/${branch}?recursive=1`;
}

function getHeaders() {
  const h = { 'Accept': 'application/vnd.github.v3+json' };
  if (CONFIG.token && CONFIG.token.trim()) h['Authorization'] = `token ${CONFIG.token.trim()}`;
  return h;
}

// --- ICONS (Lucide SVGs) ---
const ICONS = {
  folder: `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M4 20h16a2 2 0 0 0 2-2V8a2 2 0 0 0-2-2h-7.93a2 2 0 0 1-1.66-.9l-1.22-1.8A2 2 0 0 0 8.53 3H4a2 2 0 0 0-2 2v13a2 2 0 0 0 2 2Z"/></svg>`,
  fileText: `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M14.5 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V7.5L14.5 2z"/><polyline points="14 2 14 8 20 8"/><line x1="16" y1="13" x2="8" y2="13"/><line x1="16" y1="17" x2="8" y2="17"/><line x1="10" y1="9" x2="8" y2="9"/></svg>`,
  fileCode: `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M14.5 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V7.5L14.5 2z"/><polyline points="14 2 14 8 20 8"/><polyline points="10 13 8 15 10 17"/><polyline points="14 13 16 15 14 17"/></svg>`,
  filePdf: `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M14.5 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V7.5L14.5 2z"/><polyline points="14 2 14 8 20 8"/><path d="M9 15v-4h2a1 1 0 0 1 1 1v2a1 1 0 0 1-1 1H9z"/><path d="M16 15v-4h-2"/></svg>`,
  external: `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6"/><polyline points="15 3 21 3 21 9"/><line x1="10" y1="14" x2="21" y2="3"/></svg>`,
  download: `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M21 15v4a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2v-4"/><polyline points="7 10 12 15 17 10"/><line x1="12" y1="15" x2="12" y2="3"/></svg>`,
  arrowRight: `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><line x1="5" y1="12" x2="19" y2="12"/><polyline points="12 5 19 12 12 19"/></svg>`,
  chevronRight: `<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="9 18 15 12 9 6"/></svg>`,
};

// --- STATE ---
let rawTree = null;
let treeMap = {};
let currentPath = '';

// --- DOM ELEMENTS ---
const elements = {
  status: document.getElementById('status'),
  mainContent: document.getElementById('mainContent'),
  folderView: document.getElementById('folderView'),
  subjectsContainer: document.getElementById('subjects-container'),
  folderContents: document.getElementById('folderContents'),
  breadcrumbs: document.getElementById('breadcrumbs'),
  backToMainBtn: document.getElementById('backToMain'),
  searchInput: document.getElementById('search'),
  
  modalBackdrop: document.getElementById('modalBackdrop'),
  modalBody: document.getElementById('modalBody'),
  modalTitle: document.getElementById('modalTitle'),
  modalClose: document.getElementById('modalClose')
};

let lastFocusedBeforeModal = null;

// --- FORMATTING HELPERS ---
function formatName(raw) {
  let name = raw.replace(/\/$/, '');
  const bsdsMatch = name.match(/^BSDS[_\s-]?(\d+)$/i);
  if (bsdsMatch) return 'BSDS Year ' + bsdsMatch[1];
  const semMatch = name.match(/^SEM[_\s-]?(\d+)$/i);
  if (semMatch) return 'Semester ' + semMatch[1];
  return name.replace(/_/g, ' ');
}

function detectFileType(name) {
  const low = name.toLowerCase();
  if (low.endsWith('.pdf')) return { type: 'PDF', icon: ICONS.filePdf, class: 'pdf' };
  if (low.match(/\.(zip|rar|7z|tar|gz|bz2|xz)$/)) return { type: 'archive', label: 'Archive', icon: ICONS.download, class: 'text' };
  if (low.match(/\.(js|py|r|java|c|cpp|json|xml|html|css)$/)) return { type: 'Code', icon: ICONS.fileCode, class: 'code' };
  return { type: 'Text', icon: ICONS.fileText, class: 'text' };
}

// --- MODAL LOGIC ---
function openModal(title, contentNode, isPdf = false) {
  elements.modalTitle.innerHTML = `${ICONS.fileText} <span>${escapeHtml(title)}</span>`;
  elements.modalBody.innerHTML = '';
  
  if (isPdf) {
    const iframe = document.createElement('iframe');
    iframe.className = 'modal-iframe';
    iframe.src = contentNode;
    elements.modalBody.appendChild(iframe);
  } else {
    elements.modalBody.appendChild(contentNode);
  }
  
  elements.modalBackdrop.style.display = 'flex';
  setTimeout(() => elements.modalBackdrop.classList.add('show'), 10);
  elements.modalBackdrop.setAttribute('aria-hidden', 'false');
  lastFocusedBeforeModal = document.activeElement;
  elements.modalClose.focus();
  document.body.style.overflow = 'hidden';
}

function closeModal() {
  elements.modalBackdrop.classList.remove('show');
  setTimeout(() => {
    elements.modalBackdrop.style.display = 'none';
    elements.modalBackdrop.setAttribute('aria-hidden', 'true');
    elements.modalBody.innerHTML = '';
  }, 300);
  document.body.style.overflow = '';
  if (lastFocusedBeforeModal) lastFocusedBeforeModal.focus();
}

elements.modalClose.addEventListener('click', closeModal);
elements.modalBackdrop.addEventListener('click', (e) => {
  if (e.target === elements.modalBackdrop) closeModal();
});
window.addEventListener('keydown', (e) => {
  if (e.key === 'Escape' && elements.modalBackdrop.classList.contains('show')) closeModal();
  if (e.key === '/' && elements.folderView.style.display === 'block' && document.activeElement !== elements.searchInput) {
    e.preventDefault(); elements.searchInput.focus();
  }
});

// --- GITHUB API ---
async function fetchTree() {
  // Check sessionStorage cache first to avoid hitting GitHub rate limits
  const CACHE_KEY = 'bsds_tree';
  const cached = sessionStorage.getItem(CACHE_KEY);
  if (cached) {
    try {
      elements.status.style.display = 'none';
      return JSON.parse(cached);
    } catch (_) {
      sessionStorage.removeItem(CACHE_KEY);
    }
  }

  try {
    elements.status.innerHTML = `${ICONS.arrowRight} Fetching repository...`;
    let res = await fetch(apiTreeUrl(CONFIG.branch), { headers: getHeaders() });
    
    if (res.status === 404) {
      const repoInfo = await fetch(`https://api.github.com/repos/${CONFIG.owner}/${CONFIG.repo}`, { headers: getHeaders() });
      if (repoInfo.ok) {
        const repoData = await repoInfo.json();
        CONFIG.branch = repoData.default_branch || CONFIG.branch;
        res = await fetch(apiTreeUrl(CONFIG.branch), { headers: getHeaders() });
      }
    }
    
    if (res.status === 403) {
      elements.status.className = 'status-message error';
      elements.status.innerHTML = '<strong>Rate limit reached.</strong> Please try again later.';
      return null;
    }
    
    if (!res.ok) throw new Error(`API error: ${res.status}`);
    const data = await res.json();
    if (!data.tree) throw new Error('Unexpected response');
    
    // Cache the result for the duration of this browser session
    try { sessionStorage.setItem(CACHE_KEY, JSON.stringify(data.tree)); } catch (_) { /* storage full, skip */ }
    
    elements.status.style.display = 'none';
    return data.tree;
  } catch (err) {
    console.error(err);
    elements.status.className = 'status-message error';
    elements.status.innerHTML = '<strong>Error fetching data.</strong> Check network connection.';
    return null;
  }
}

function buildTreeMap(tree) {
  const map = { '': { folders: new Set(), files: [] } };
  
  // Define files/folders to hide from the UI also the hidden folders
  const hiddenPaths = ['index.html', 'assets', 'css', 'js', 'README.md', '.git'];

  for (const item of tree) {
    // Check if the item path starts with any of our hidden paths
    const shouldHide = hiddenPaths.some(hp => item.path === hp || item.path.startsWith(hp + '/'));
    if (shouldHide || item.path.includes('/.')) continue;

    const parts = item.path.split('/');
    let accum = '';
    for (let i = 0; i < parts.length; i++) {
      const part = parts[i];
      const isLast = i === parts.length - 1;
      const parent = accum;
      
      if (!map[parent]) map[parent] = { folders: new Set(), files: [] };
      
      if (isLast) {
        if (item.type === 'tree') {
          map[parent].folders.add(part + '/');
          const childFull = parent + part + '/';
          if (!map[childFull]) map[childFull] = { folders: new Set(), files: [] };
        } else if (item.type === 'blob') {
          map[parent].files.push({ name: part, path: parent + part });
        }
      } else {
        map[parent].folders.add(part + '/');
        accum += part + '/';
        if (!map[accum]) map[accum] = { folders: new Set(), files: [] };
      }
    }
  }
  for (const k of Object.keys(map)) {
    map[k].folders = Array.from(map[k].folders).sort();
    map[k].files = map[k].files.sort((a, b) => a.name.localeCompare(b.name));
  }
  return map;
}

// --- RENDER LOGIC ---
function renderSubjects() {
  elements.subjectsContainer.innerHTML = '';
  const top = treeMap[''];
  
  if (!top || top.folders.length === 0) {
    elements.subjectsContainer.innerHTML = '<div class="message">No subjects found.</div>';
    return;
  }

  for (const folder of top.folders) {
    const rawName = folder.replace(/\/$/, '');
    const path = folder;

    const block = document.createElement('div');
    block.className = 'glass-panel subject-block';

    const h3 = document.createElement('h3');
    h3.textContent = formatName(rawName);
    block.appendChild(h3);

    const childNode = treeMap[path];
    if (childNode) {
      const sCount = childNode.folders.length;
      const fCount = childNode.files.length;
      const parts = [];
      if (sCount > 0) parts.push(`${sCount} semester${sCount > 1 ? 's' : ''}`);
      if (fCount > 0) parts.push(`${fCount} file${fCount > 1 ? 's' : ''}`);
      
      const info = document.createElement('div');
      info.className = 'subject-info';
      info.textContent = parts.join(' • ') || 'Empty';
      block.appendChild(info);
    }

    const btn = document.createElement('button');
    btn.className = 'btn-primary';
    btn.innerHTML = `Browse ${ICONS.arrowRight}`;
    btn.dataset.path = path;
    btn.addEventListener('click', (e) => openFolder(e.currentTarget.dataset.path));
    
    block.appendChild(btn);
    elements.subjectsContainer.appendChild(block);
  }
}

function renderFolderContents(path) {
  const node = treeMap[path];
  if (!node) { elements.folderContents.innerHTML = '<div class="message">Folder not found.</div>'; return; }
  elements.folderContents.innerHTML = '';

  // Render Folders
  if (node.folders.length) {
    const title = document.createElement('div');
    title.className = 'hint';
    title.textContent = 'Folders';
    elements.folderContents.appendChild(title);
    
    const ul = document.createElement('ul');
    ul.className = 'folder-list';
    
    for (const f of node.folders) {
      const li = document.createElement('li');
      li.className = 'folder-item';
      
      const left = document.createElement('div');
      left.className = 'left';
      
      const expandBtn = document.createElement('button');
      expandBtn.className = 'expand-btn';
      expandBtn.innerHTML = ICONS.chevronRight;
      
      const iconWrap = document.createElement('div');
      iconWrap.className = 'folder-icon';
      iconWrap.innerHTML = ICONS.folder;
      
      const name = document.createElement('div');
      name.className = 'folder-name';
      name.textContent = formatName(f);
      name.setAttribute('tabindex', '0');
      name.setAttribute('role', 'button');
      name.setAttribute('aria-label', `Open folder: ${formatName(f)}`);
      const _folderPath = nodePath(path, f);
      name.addEventListener('click', () => openFolder(_folderPath));
      name.addEventListener('keydown', (e) => {
        if (e.key === 'Enter' || e.key === ' ') {
          e.preventDefault();
          openFolder(_folderPath);
        }
      });
      
      expandBtn.addEventListener('click', () => {
        const existing = Array.from(li.children).find(c => c.classList.contains('child-contents'));
        if (existing) {
          li.removeChild(existing);
          expandBtn.classList.remove('open');
        } else {
          expandBtn.classList.add('open');
          const childContainer = document.createElement('div');
          childContainer.className = 'child-contents fade-enter';
          renderMini(nodePath(path, f), childContainer);
          li.appendChild(childContainer);
        }
      });
      
      left.appendChild(expandBtn);
      left.appendChild(iconWrap);
      left.appendChild(name);
      li.appendChild(left);
      ul.appendChild(li);
    }
    elements.folderContents.appendChild(ul);
  }

  // Render Files
  if (node.files.length) {
    const title = document.createElement('div');
    title.className = 'hint';
    title.textContent = 'Files';
    elements.folderContents.appendChild(title);
    
    const div = document.createElement('div');
    for (const file of node.files) {
      const fileInfo = detectFileType(file.name);
      
      const row = document.createElement('div');
      row.className = 'file-row';
      
      const left = document.createElement('div');
      left.className = 'left';
      
      const iconWrap = document.createElement('div');
      iconWrap.className = `file-icon ${fileInfo.class}`;
      iconWrap.innerHTML = fileInfo.icon;
      
      const name = document.createElement('div');
      name.className = 'file-name';
      name.textContent = file.name;
      name.setAttribute('tabindex', '0');
      name.setAttribute('role', 'button');
      name.setAttribute('aria-label', `Preview file: ${file.name}`);
      const _filePath = file.path;
      name.addEventListener('click', () => openFile(_filePath));
      name.addEventListener('keydown', (e) => {
        if (e.key === 'Enter' || e.key === ' ') {
          e.preventDefault();
          openFile(_filePath);
        }
      });
      
      const meta = document.createElement('div');
      meta.className = 'file-meta';
      meta.textContent = fileInfo.type;
      
      left.appendChild(iconWrap);
      left.appendChild(name);
      left.appendChild(meta);
      
      const right = document.createElement('div');
      right.className = 'right';
      
      const btnOpen = document.createElement('a');
      btnOpen.href = encodeURI(VIEW_BASE + file.path);
      btnOpen.target = '_blank';
      btnOpen.className = 'action-icon-btn';
      btnOpen.title = 'Open in new tab';
      btnOpen.innerHTML = ICONS.external;
      
      const btnDownload = document.createElement('a');
      btnDownload.href = encodeURI(RAW_BASE + file.path);
      btnDownload.className = 'action-icon-btn';
      btnDownload.title = 'Download';
      btnDownload.download = file.name;
      btnDownload.innerHTML = ICONS.download;
      
      right.appendChild(btnOpen);
      right.appendChild(btnDownload);
      
      row.appendChild(left);
      row.appendChild(right);
      div.appendChild(row);
    }
    elements.folderContents.appendChild(div);
  }

  if (node.folders.length === 0 && node.files.length === 0) {
    elements.folderContents.innerHTML = '<div class="message">This folder is empty.</div>';
  }
}

function renderMini(path, container) {
  const node = treeMap[path];
  if (!node) { container.innerHTML = '<div class="hint">(empty)</div>'; return; }
  
  const frag = document.createDocumentFragment();
  if (node.folders.length) {
    const ul = document.createElement('ul');
    ul.style.listStyle = 'none'; ul.style.padding = '0'; ul.style.margin = '0 0 8px';
    for (const f of node.folders) {
      const li = document.createElement('li');
      li.style.display = 'flex'; li.style.alignItems = 'center'; li.style.gap = '8px'; li.style.padding = '4px 0';
      const icon = document.createElement('div');
      icon.innerHTML = ICONS.folder; icon.style.width = '14px'; icon.style.color = 'var(--text-secondary)';
      const span = document.createElement('span');
      span.textContent = formatName(f);
      span.className = 'folder-name';
      span.style.fontSize = '0.9rem';
      span.addEventListener('click', () => openFolder(nodePath(path, f)));
      li.appendChild(icon); li.appendChild(span); ul.appendChild(li);
    }
    frag.appendChild(ul);
  }
  
  if (node.files.length) {
    const ul = document.createElement('ul');
    ul.style.listStyle = 'none'; ul.style.padding = '0'; ul.style.margin = '0';
    for (const file of node.files) {
      const li = document.createElement('li');
      li.style.display = 'flex'; li.style.alignItems = 'center'; li.style.gap = '8px'; li.style.padding = '4px 0';
      const info = detectFileType(file.name);
      const icon = document.createElement('div');
      icon.innerHTML = info.icon; icon.style.width = '14px'; icon.className = `file-icon ${info.class}`;
      const link = document.createElement('span');
      link.textContent = file.name;
      link.className = 'file-name';
      link.style.fontSize = '0.9rem';
      link.addEventListener('click', () => openFile(file.path));
      li.appendChild(icon); li.appendChild(link); ul.appendChild(li);
    }
    frag.appendChild(ul);
  }
  container.appendChild(frag);
}

// --- NAVIGATION ---
function openFolder(path, pushHistory = true) {
  currentPath = path;
  elements.mainContent.style.display = 'none';
  elements.folderView.style.display = 'block';
  elements.folderView.classList.remove('fade-enter');
  void elements.folderView.offsetWidth;
  elements.folderView.classList.add('fade-enter');

  renderBreadcrumbs(path);
  renderFolderContents(path);
  elements.searchInput.value = '';

  if (pushHistory) {
    history.pushState({ path: path }, '', '#' + encodeURIComponent(path));
  }
  window.scrollTo({ top: 0, behavior: 'smooth' });
}

function showMain(pushHistory = true) {
  currentPath = '';
  elements.folderView.style.display = 'none';
  elements.mainContent.style.display = 'block';
  elements.mainContent.classList.remove('fade-enter');
  void elements.mainContent.offsetWidth;
  elements.mainContent.classList.add('fade-enter');

  if (pushHistory) history.pushState({ path: '' }, '', window.location.pathname);
  window.scrollTo({ top: 0, behavior: 'smooth' });
}

function renderBreadcrumbs(path) {
  elements.breadcrumbs.innerHTML = '';
  const crumbs = [{ name: 'Home', path: '' }];
  let accum = '';
  const parts = path.split('/').filter(Boolean);
  
  for (const p of parts) { 
    accum += p + '/'; 
    crumbs.push({ name: formatName(p), path: accum }); 
  }
  
  crumbs.forEach((c, i) => {
    const span = document.createElement('div');
    span.className = 'breadcrumb';
    span.textContent = c.name;
    span.addEventListener('click', () => c.path === '' ? showMain() : openFolder(c.path));
    
    elements.breadcrumbs.appendChild(span);
    if (i < crumbs.length - 1) {
      const sep = document.createElement('div');
      sep.className = 'breadcrumb-sep';
      sep.innerHTML = ICONS.chevronRight;
      elements.breadcrumbs.appendChild(sep);
    }
  });
}

// --- HELPERS & EVENTS ---
function nodePath(parent, child) { return (parent || '') + child; }

function escapeHtml(s) { 
  return s.replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c])); 
}

async function openFile(path) {
  const name = path.split('/').pop();

  // Archive files cannot be previewed — trigger a direct browser download instead
  if (detectFileType(name).type === 'archive') {
    const a = document.createElement('a');
    a.href = encodeURI(RAW_BASE + path);
    a.download = name;
    a.style.display = 'none';
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    return;
  }

  if (name.toLowerCase().endsWith('.pdf')) {
    openModal(name, VIEW_BASE + path, true);
    return;
  }
  
  const url = encodeURI(RAW_BASE + path);
  try {
    const container = document.createElement('div');
    openModal(name, container, false);
    container.innerHTML = '<div class="message" style="margin:20px">Loading preview...</div>';
    
    const res = await fetch(url);
    if (!res.ok) throw new Error('Failed to fetch');
    const text = await res.text();
    
    const pre = document.createElement('pre');
    pre.textContent = text;
    container.innerHTML = '';
    container.appendChild(pre);
  } catch (err) {
    const fallback = document.createElement('div');
    fallback.className = 'message';
    fallback.style.margin = '20px';
    fallback.innerHTML = `Unable to load preview. <a href="${encodeURI(VIEW_BASE + path)}" target="_blank" style="text-decoration:underline;margin-left:8px">Open in new tab</a>`;
    elements.modalBody.innerHTML = '';
    elements.modalBody.appendChild(fallback);
  }
}

elements.searchInput.addEventListener('input', (e) => {
  const term = e.target.value.trim().toLowerCase();
  if (!currentPath) return;
  renderFolderContents(currentPath);
  if (!term) return;
  const items = elements.folderContents.querySelectorAll('.folder-item, .file-row');
  items.forEach(it => { 
    it.style.display = it.textContent.toLowerCase().includes(term) ? '' : 'none'; 
  });
});

elements.backToMainBtn.addEventListener('click', () => {
  if (currentPath) {
    const parts = currentPath.replace(/\/$/, '').split('/');
    parts.pop();
    parts.length === 0 ? showMain() : openFolder(parts.join('/') + '/');
  } else {
    showMain();
  }
});

window.addEventListener('popstate', (e) => {
  e.state && e.state.path ? openFolder(e.state.path, false) : showMain(false);
});

// --- INITIALIZE ---
(async function init() {
  const tree = await fetchTree();
  if (!tree) return;
  
  treeMap = buildTreeMap(tree);
  renderSubjects();
  
  history.replaceState({ path: '' }, '');
  const hash = window.location.hash.slice(1);
  if (hash) {
    const path = decodeURIComponent(hash);
    if (treeMap[path]) openFolder(path, false);
  }
})();
