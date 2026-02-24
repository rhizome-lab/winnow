/** Shared modal overlay for dialog, save UI, and settings UI.
 *
 * This is a browser implementation detail — not part of the platform contract.
 * Each concern module imports from here; deployers swap individual concern
 * files and can ignore or replace this helper.
 */

export class OverlayManager {
  private dialogOverlay: HTMLDivElement | null = null;

  getOrCreateOverlay(onClose: () => void): HTMLDivElement {
    if (this.dialogOverlay) {
      // Update the click-to-close handler for the current consumer
      this.dialogOverlay.onclick = (e) => {
        if (e.target === this.dialogOverlay) onClose();
      };
      return this.dialogOverlay;
    }
    const overlay = document.createElement("div");
    overlay.id = "reincarnate-dialog-overlay";
    overlay.style.cssText = `
      position: fixed; inset: 0; z-index: 10000;
      background: rgba(0,0,0,0.6); display: flex;
      justify-content: center; align-items: center;
    `;
    overlay.onclick = (e) => {
      if (e.target === overlay) onClose();
    };
    document.body.appendChild(overlay);
    this.dialogOverlay = overlay;
    return overlay;
  }

  hideOverlay(): void {
    if (this.dialogOverlay) {
      this.dialogOverlay.style.display = "none";
      this.dialogOverlay.innerHTML = "";
    }
  }

  isOverlayVisible(): boolean {
    return this.dialogOverlay !== null && this.dialogOverlay.style.display !== "none";
  }

  buildDialogChrome(
    title: string,
    content: DocumentFragment | HTMLElement,
    onClose: () => void,
  ): void {
    const overlay = this.getOrCreateOverlay(onClose);
    overlay.style.display = "flex";
    overlay.innerHTML = "";

    const dialog = document.createElement("div");
    dialog.style.cssText = `
      background: #1a1a1a; color: #eee; border: 1px solid #555;
      border-radius: 6px; max-width: 600px; width: 90%; max-height: 80vh;
      overflow-y: auto; padding: 0;
    `;

    const header = document.createElement("div");
    header.style.cssText = `
      display: flex; justify-content: space-between; align-items: center;
      padding: 0.8em 1em; border-bottom: 1px solid #333;
    `;
    const titleEl = document.createElement("h3");
    titleEl.style.cssText = "margin: 0; font-size: 1.1em;";
    titleEl.textContent = title;
    const closeBtn = document.createElement("button");
    closeBtn.textContent = "\u00D7";
    closeBtn.style.cssText = `
      background: none; border: none; color: #aaa; font-size: 1.4em;
      cursor: pointer; padding: 0 0.3em; line-height: 1;
    `;
    closeBtn.addEventListener("click", onClose);
    header.appendChild(titleEl);
    header.appendChild(closeBtn);

    const body = document.createElement("div");
    body.style.cssText = "padding: 1em;";
    body.appendChild(content);

    dialog.appendChild(header);
    dialog.appendChild(body);
    overlay.appendChild(dialog);
  }
}
