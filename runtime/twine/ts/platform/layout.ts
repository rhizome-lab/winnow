/** Browser layout — sidebar/UIBar rendering. */

export interface SidebarConfig {
  storyTitle: string;
  passageTitle: string;
  captionContent?: DocumentFragment;
  onSaves?: () => void;
  onSettings?: () => void;
  onRestart?: () => void;
  onBack?: () => void;
  onForward?: () => void;
}

let sidebarEl: HTMLDivElement | null = null;

export function renderSidebar(config: SidebarConfig): void {
  if (!sidebarEl) {
    sidebarEl = document.createElement("div");
    sidebarEl.id = "reincarnate-sidebar";
    sidebarEl.style.cssText = `
      position: fixed; top: 0; left: 0; width: 16em; height: 100vh;
      background: #1a1a1a; border-right: 1px solid #333;
      padding: 1em; overflow-y: auto; z-index: 1000;
      font-size: 0.9em; color: #ccc;
    `;
    document.body.appendChild(sidebarEl);
    // Offset the main content
    document.body.style.marginLeft = "17em";
  }

  sidebarEl.innerHTML = "";
  sidebarEl.style.display = "";

  const title = document.createElement("h2");
  title.style.cssText = "margin: 0 0 0.5em; font-size: 1.2em; color: #eee;";
  title.textContent = config.storyTitle;
  sidebarEl.appendChild(title);

  if (config.captionContent) {
    const caption = document.createElement("div");
    caption.style.cssText = "margin-bottom: 1em;";
    caption.appendChild(config.captionContent);
    sidebarEl.appendChild(caption);
  }

  const nav = document.createElement("nav");
  nav.style.cssText = "display: flex; flex-direction: column; gap: 0.4em;";

  const links: [string, (() => void) | undefined][] = [
    ["\u25C4 Back", config.onBack],
    ["Saves", config.onSaves],
    ["Settings", config.onSettings],
    ["Restart", config.onRestart],
  ];

  for (const [label, handler] of links) {
    if (!handler) continue;
    const a = document.createElement("a");
    a.textContent = label;
    a.style.cssText = "color: #4ea6ca; cursor: pointer; text-decoration: none; padding: 0.2em 0;";
    a.addEventListener("click", (e) => { e.preventDefault(); handler(); });
    nav.appendChild(a);
  }

  sidebarEl.appendChild(nav);
}

export function stowSidebar(): void {
  if (sidebarEl) {
    sidebarEl.style.display = "none";
    document.body.style.marginLeft = "";
  }
}

export function unstowSidebar(): void {
  if (sidebarEl) {
    sidebarEl.style.display = "";
    document.body.style.marginLeft = "17em";
  }
}

export function toggleSidebar(): void {
  if (sidebarEl?.style.display === "none") unstowSidebar();
  else stowSidebar();
}

export function initCommands(register: (id: string, binding: string, handler: () => void) => void): void {
  register("toggle-sidebar", "", toggleSidebar);
}

export function destroySidebar(): void {
  if (sidebarEl) {
    sidebarEl.remove();
    sidebarEl = null;
    document.body.style.marginLeft = "";
  }
}
