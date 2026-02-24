/** Browser dialog — modal dialogue boxes. */

import { OverlayManager } from "./_overlay";

export class DialogManager {
  private overlay: OverlayManager;

  constructor(overlay: OverlayManager) {
    this.overlay = overlay;
  }

  showDialog(title: string, content: DocumentFragment | HTMLElement): void {
    this.overlay.buildDialogChrome(title, content, () => this.closeDialog());
  }

  closeDialog(): void {
    this.overlay.hideOverlay();
  }

  isDialogOpen(): boolean {
    return this.overlay.isOverlayVisible();
  }

  initCommands(register: (id: string, binding: string, handler: () => void) => void): void {
    register("close-dialog", "escape", () => this.closeDialog());
  }
}
