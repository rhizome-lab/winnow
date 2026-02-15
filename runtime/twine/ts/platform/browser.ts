/** Browser platform implementation for Twine runtime.
 *
 * Hookable primitives — deployers can swap this module for custom
 * persistence backends, audio engines, or timing behavior by changing
 * the re-export in platform/index.ts.
 */

// --- Persistence (localStorage wrapper) ---

export function loadLocal(key: string): string | null {
  try {
    return localStorage.getItem(key);
  } catch {
    return null;
  }
}

export function saveLocal(key: string, value: string): void {
  try {
    localStorage.setItem(key, value);
  } catch {
    // Storage full or unavailable — silent fail
  }
}

export function removeLocal(key: string): void {
  try {
    localStorage.removeItem(key);
  } catch {
    // Unavailable — silent fail
  }
}

// --- Audio (HTMLAudioElement wrapper) ---

export type AudioHandle = HTMLAudioElement;

export function createAudio(sources: string[]): AudioHandle {
  const el = document.createElement("audio");
  for (const src of sources) {
    const source = document.createElement("source");
    source.src = src;
    // Infer type from extension
    const ext = src.split(".").pop()?.toLowerCase();
    if (ext === "mp3") source.type = "audio/mpeg";
    else if (ext === "ogg") source.type = "audio/ogg";
    else if (ext === "wav") source.type = "audio/wav";
    else if (ext === "m4a" || ext === "aac") source.type = "audio/mp4";
    else if (ext === "webm") source.type = "audio/webm";
    else if (ext === "flac") source.type = "audio/flac";
    el.appendChild(source);
  }
  el.preload = "auto";
  return el;
}

export function playAudio(el: AudioHandle): Promise<void> {
  return el.play();
}

export function pauseAudio(el: AudioHandle): void {
  el.pause();
}

export function stopAudio(el: AudioHandle): void {
  el.pause();
  el.currentTime = 0;
}

export function setVolume(el: AudioHandle, vol: number): void {
  el.volume = Math.max(0, Math.min(1, vol));
}

export function setMuted(el: AudioHandle, muted: boolean): void {
  el.muted = muted;
}

export function setLoop(el: AudioHandle, loop: boolean): void {
  el.loop = loop;
}

export function seekAudio(el: AudioHandle, time: number): void {
  el.currentTime = time;
}

export function getAudioDuration(el: AudioHandle): number {
  return el.duration;
}

export function getAudioTime(el: AudioHandle): number {
  return el.currentTime;
}

export function fadeAudio(
  el: AudioHandle,
  to: number,
  duration: number,
): Promise<void> {
  return new Promise((resolve) => {
    const from = el.volume;
    const steps = Math.max(1, Math.round(duration / 25));
    const delta = (to - from) / steps;
    let step = 0;
    const id = setInterval(() => {
      step++;
      if (step >= steps) {
        el.volume = Math.max(0, Math.min(1, to));
        clearInterval(id);
        resolve();
      } else {
        el.volume = Math.max(0, Math.min(1, from + delta * step));
      }
    }, 25);
  });
}

export function isAudioReady(el: AudioHandle): boolean {
  return el.readyState >= HTMLMediaElement.HAVE_ENOUGH_DATA;
}

// --- Timing (setTimeout/setInterval wrapper) ---

export function scheduleTimeout(fn: () => void, ms: number): number {
  return window.setTimeout(fn, ms);
}

export function cancelTimeout(id: number): void {
  window.clearTimeout(id);
}

export function scheduleInterval(fn: () => void, ms: number): number {
  return window.setInterval(fn, ms);
}

export function cancelInterval(id: number): void {
  window.clearInterval(id);
}

// --- Dialog UI ---

let dialogOverlay: HTMLDivElement | null = null;

function getOrCreateOverlay(): HTMLDivElement {
  if (dialogOverlay) return dialogOverlay;
  const overlay = document.createElement("div");
  overlay.id = "reincarnate-dialog-overlay";
  overlay.style.cssText = `
    position: fixed; inset: 0; z-index: 10000;
    background: rgba(0,0,0,0.6); display: flex;
    justify-content: center; align-items: center;
  `;
  overlay.addEventListener("click", (e) => {
    if (e.target === overlay) closeDialog();
  });
  document.body.appendChild(overlay);
  dialogOverlay = overlay;
  return overlay;
}

export function showDialog(title: string, content: DocumentFragment | HTMLElement): void {
  const overlay = getOrCreateOverlay();
  overlay.style.display = "flex";
  // Clear previous dialog content
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
  closeBtn.addEventListener("click", closeDialog);
  header.appendChild(titleEl);
  header.appendChild(closeBtn);

  const body = document.createElement("div");
  body.style.cssText = "padding: 1em;";
  body.appendChild(content);

  dialog.appendChild(header);
  dialog.appendChild(body);
  overlay.appendChild(dialog);
}

export function closeDialog(): void {
  if (dialogOverlay) {
    dialogOverlay.style.display = "none";
    dialogOverlay.innerHTML = "";
  }
}

export function isDialogOpen(): boolean {
  return dialogOverlay !== null && dialogOverlay.style.display !== "none";
}

// --- Save UI ---

export interface SaveSlotInfo {
  index: number;
  title: string | null;
  date: string | null;
  isEmpty: boolean;
}

export function showSaveUI(
  slots: SaveSlotInfo[],
  onSave: (i: number) => void,
  onLoad: (i: number) => void,
  onDelete: (i: number) => void,
): void {
  const frag = document.createDocumentFragment();
  const table = document.createElement("table");
  table.style.cssText = "width: 100%; border-collapse: collapse;";

  for (const slot of slots) {
    const row = document.createElement("tr");
    row.style.cssText = "border-bottom: 1px solid #333;";

    const labelCell = document.createElement("td");
    labelCell.style.cssText = "padding: 0.5em;";
    labelCell.textContent = slot.isEmpty
      ? `Slot ${slot.index + 1}: (empty)`
      : `Slot ${slot.index + 1}: ${slot.title || "Untitled"} ${slot.date ? `(${slot.date})` : ""}`;

    const actionCell = document.createElement("td");
    actionCell.style.cssText = "padding: 0.5em; text-align: right; white-space: nowrap;";

    const saveBtn = document.createElement("button");
    saveBtn.textContent = "Save";
    saveBtn.style.cssText = "margin: 0 0.2em;";
    saveBtn.addEventListener("click", () => { onSave(slot.index); closeDialog(); });

    actionCell.appendChild(saveBtn);

    if (!slot.isEmpty) {
      const loadBtn = document.createElement("button");
      loadBtn.textContent = "Load";
      loadBtn.style.cssText = "margin: 0 0.2em;";
      loadBtn.addEventListener("click", () => { onLoad(slot.index); closeDialog(); });

      const delBtn = document.createElement("button");
      delBtn.textContent = "Delete";
      delBtn.style.cssText = "margin: 0 0.2em;";
      delBtn.addEventListener("click", () => { onDelete(slot.index); closeDialog(); });

      actionCell.appendChild(loadBtn);
      actionCell.appendChild(delBtn);
    }

    row.appendChild(labelCell);
    row.appendChild(actionCell);
    table.appendChild(row);
  }

  frag.appendChild(table);
  showDialog("Saves", frag);
}

export function closeSaveUI(): void {
  closeDialog();
}

// --- Sidebar / UIBar ---

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

export function destroySidebar(): void {
  if (sidebarEl) {
    sidebarEl.remove();
    sidebarEl = null;
    document.body.style.marginLeft = "";
  }
}

// --- Settings UI ---

export interface SettingUIEntry {
  name: string;
  type: "toggle" | "list" | "range";
  label: string;
  desc?: string;
  value: any;
  // list-specific
  list?: any[];
  // range-specific
  min?: number;
  max?: number;
  step?: number;
}

export function showSettingsUI(
  entries: SettingUIEntry[],
  onSet: (name: string, value: any) => void,
  onReset: () => void,
): void {
  const frag = document.createDocumentFragment();
  const form = document.createElement("div");
  form.style.cssText = "display: flex; flex-direction: column; gap: 0.8em;";

  for (const entry of entries) {
    const row = document.createElement("div");

    const label = document.createElement("label");
    label.style.cssText = "display: flex; align-items: center; gap: 0.5em; flex-wrap: wrap;";

    const labelText = document.createElement("span");
    labelText.textContent = entry.label;
    label.appendChild(labelText);

    if (entry.type === "toggle") {
      const input = document.createElement("input");
      input.type = "checkbox";
      input.checked = !!entry.value;
      input.addEventListener("change", () => onSet(entry.name, input.checked));
      label.appendChild(input);
    } else if (entry.type === "list") {
      const select = document.createElement("select");
      for (const opt of entry.list || []) {
        const option = document.createElement("option");
        option.value = String(opt);
        option.textContent = String(opt);
        if (opt === entry.value) option.selected = true;
        select.appendChild(option);
      }
      select.addEventListener("change", () => onSet(entry.name, select.value));
      label.appendChild(select);
    } else if (entry.type === "range") {
      const input = document.createElement("input");
      input.type = "range";
      input.min = String(entry.min ?? 0);
      input.max = String(entry.max ?? 100);
      input.step = String(entry.step ?? 1);
      input.value = String(entry.value);
      const valueLabel = document.createElement("span");
      valueLabel.textContent = String(entry.value);
      input.addEventListener("input", () => {
        const val = Number(input.value);
        valueLabel.textContent = String(val);
        onSet(entry.name, val);
      });
      label.appendChild(input);
      label.appendChild(valueLabel);
    }

    row.appendChild(label);

    if (entry.desc) {
      const desc = document.createElement("div");
      desc.style.cssText = "font-size: 0.85em; color: #999; margin-top: 0.2em;";
      desc.textContent = entry.desc;
      row.appendChild(desc);
    }

    form.appendChild(row);
  }

  // Reset button
  const resetBtn = document.createElement("button");
  resetBtn.textContent = "Reset to Defaults";
  resetBtn.style.cssText = "margin-top: 0.5em; align-self: flex-start;";
  resetBtn.addEventListener("click", () => { onReset(); closeDialog(); });
  form.appendChild(resetBtn);

  frag.appendChild(form);
  showDialog("Settings", frag);
}
