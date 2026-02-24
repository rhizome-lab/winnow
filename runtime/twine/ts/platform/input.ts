/** Browser input — keybinds-backed command registry with UI components. */

import keybindsInit, { type Command, registerComponents, executeCommand } from "keybinds";

// Register web components (<command-palette>, <keybind-cheatsheet>, etc.)
// This is idempotent and browser-global — safe to call at module load time.
registerComponents();

export class InputManager {
  private commands: Command[] = [];
  private cleanup: (() => void) | null = null;
  private palette: HTMLElement | null = null;
  private cheatsheet: HTMLElement | null = null;

  private ensureUI(): void {
    if (this.palette) return;
    this.palette = document.createElement("command-palette");
    this.palette.setAttribute("auto-trigger", "");
    document.body.appendChild(this.palette);

    this.cheatsheet = document.createElement("keybind-cheatsheet");
    this.cheatsheet.setAttribute("auto-trigger", "");
    document.body.appendChild(this.cheatsheet);
  }

  private syncUI(): void {
    if (this.palette) (this.palette as any).commands = this.commands;
    if (this.cheatsheet) (this.cheatsheet as any).commands = this.commands;
  }

  private rebind(): void {
    if (this.cleanup) this.cleanup();
    if (this.commands.length > 0) {
      this.cleanup = keybindsInit(this.commands);
    }
    this.syncUI();
  }

  registerCommand(id: string, defaultBinding: string, handler: () => void): void {
    this.ensureUI();
    this.commands = this.commands.filter(c => c.id !== id);
    this.commands.push({
      id,
      label: id.replace(/-/g, " ").replace(/\b\w/g, c => c.toUpperCase()),
      keys: defaultBinding ? [defaultBinding] : [],
      execute: handler,
    });
    this.rebind();
  }

  removeCommand(id: string): void {
    this.commands = this.commands.filter(c => c.id !== id);
    this.rebind();
  }

  triggerCommand(id: string): void {
    executeCommand(this.commands, id);
  }

  getCommands(): Command[] {
    return this.commands;
  }
}
