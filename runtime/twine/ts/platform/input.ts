/** Browser input — keybinds-backed command registry with UI components. */

import keybindsInit, { type Command, registerComponents, executeCommand } from "keybinds";

// Register web components (<command-palette>, <keybind-cheatsheet>, etc.)
registerComponents();

let commands: Command[] = [];
let cleanup: (() => void) | null = null;

// Web component elements — created lazily on first registerCommand
let palette: HTMLElement | null = null;
let cheatsheet: HTMLElement | null = null;

function ensureUI(): void {
  if (palette) return;
  palette = document.createElement("command-palette");
  palette.setAttribute("auto-trigger", "");
  document.body.appendChild(palette);

  cheatsheet = document.createElement("keybind-cheatsheet");
  cheatsheet.setAttribute("auto-trigger", "");
  document.body.appendChild(cheatsheet);
}

function syncUI(): void {
  if (palette) (palette as any).commands = commands;
  if (cheatsheet) (cheatsheet as any).commands = commands;
}

function rebind(): void {
  if (cleanup) cleanup();
  if (commands.length > 0) {
    cleanup = keybindsInit(commands);
  }
  syncUI();
}

export function registerCommand(
  id: string,
  defaultBinding: string,
  handler: () => void,
): void {
  ensureUI();
  commands = commands.filter(c => c.id !== id);
  commands.push({
    id,
    label: id.replace(/-/g, " ").replace(/\b\w/g, c => c.toUpperCase()),
    keys: defaultBinding ? [defaultBinding] : [],
    execute: handler,
  });
  rebind();
}

export function removeCommand(id: string): void {
  commands = commands.filter(c => c.id !== id);
  rebind();
}

export function triggerCommand(id: string): void {
  executeCommand(commands, id);
}

export function getCommands(): Command[] {
  return commands;
}
