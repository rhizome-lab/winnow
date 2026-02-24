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

export class LayoutManager {
  private sidebarEl: HTMLDivElement | null = null;

  renderSidebar(config: SidebarConfig): void {
    if (!this.sidebarEl) {
      this.sidebarEl = document.createElement("div");
      this.sidebarEl.id = "reincarnate-sidebar";
      this.sidebarEl.style.cssText = `
        position: fixed; top: 0; left: 0; width: 16em; height: 100vh;
        background: #1a1a1a; border-right: 1px solid #333;
        padding: 1em; overflow-y: auto; z-index: 1000;
        font-size: 0.9em; color: #ccc;
      `;
      document.body.appendChild(this.sidebarEl);
      // Offset the main content
      document.body.style.marginLeft = "17em";
    }

    this.sidebarEl.innerHTML = "";
    this.sidebarEl.style.display = "";

    const title = document.createElement("h2");
    title.style.cssText = "margin: 0 0 0.5em; font-size: 1.2em; color: #eee;";
    title.textContent = config.storyTitle;
    this.sidebarEl.appendChild(title);

    if (config.captionContent) {
      const caption = document.createElement("div");
      caption.style.cssText = "margin-bottom: 1em;";
      caption.appendChild(config.captionContent);
      this.sidebarEl.appendChild(caption);
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

    this.sidebarEl.appendChild(nav);
  }

  stowSidebar(): void {
    if (this.sidebarEl) {
      this.sidebarEl.style.display = "none";
      document.body.style.marginLeft = "";
    }
  }

  unstowSidebar(): void {
    if (this.sidebarEl) {
      this.sidebarEl.style.display = "";
      document.body.style.marginLeft = "17em";
    }
  }

  toggleSidebar(): void {
    if (this.sidebarEl?.style.display === "none") this.unstowSidebar();
    else this.stowSidebar();
  }

  initCommands(register: (id: string, binding: string, handler: () => void) => void): void {
    register("toggle-sidebar", "", () => this.toggleSidebar());
  }

  destroySidebar(): void {
    if (this.sidebarEl) {
      this.sidebarEl.remove();
      this.sidebarEl = null;
      document.body.style.marginLeft = "";
    }
  }
}
