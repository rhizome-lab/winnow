/** Platform interface — re-exports from per-concern implementation modules.
 *
 * Swap any concern by replacing its module with a different implementation.
 * For example, swap persistence by pointing at "./cloud-persistence" instead
 * of "./persistence", or add gamepad input alongside keyboard by swapping
 * "./input" for a module that merges both.
 *
 * PlatformBundle constructs and wires all stateful concern objects. Callers
 * create one PlatformBundle per runtime instance — no module-level singletons.
 */

import { OverlayManager } from "./_overlay";
import { DialogManager } from "./dialog";
import { InputManager } from "./input";
import { LayoutManager } from "./layout";
import { SaveUIManager } from "./save-ui";
import { SettingsUIManager } from "./settings-ui";
import { SaveManager, type SaveBackend, type SaveableState } from "./save";
import { type SaveSlotInfo } from "./save-ui";
import { type SettingUIEntry } from "./settings-ui";
import { type SidebarConfig } from "./layout";
import {
  createAudio, playAudio, pauseAudio, stopAudio,
  setVolume, setMuted, setLoop,
  seekAudio, getAudioDuration, getAudioTime,
  fadeAudio, isAudioReady,
} from "./audio";
import type { Command } from "keybinds";

export { loadLocal, saveLocal, removeLocal, localStorageBackend } from "./persistence";

export {
  type HistoryStrategy,
  snapshotHistory, diffHistory,
} from "./history";

export {
  type SaveBackend, type SaveableState, type PersistenceOpts,
  tee, debounced, rolling,
} from "./save";

export {
  type AudioHandle,
  createAudio, playAudio, pauseAudio, stopAudio,
  setVolume, setMuted, setLoop,
  seekAudio, getAudioDuration, getAudioTime,
  fadeAudio, isAudioReady,
} from "./audio";

export {
  scheduleTimeout, cancelTimeout,
  scheduleInterval, cancelInterval,
} from "./timing";

export { type SaveSlotInfo } from "./save-ui";
export { type SettingUIEntry } from "./settings-ui";
export { type SidebarConfig } from "./layout";

/** All stateful platform concerns bundled into a single instance.
 *
 * Create one PlatformBundle per runtime instance. Wires cross-concern
 * dependencies (dialog ↔ input, layout ↔ input, save-ui ↔ dialog) in its
 * constructor. Call initSave() once engine state is available.
 */
export class PlatformBundle {
  private _overlay: OverlayManager;
  private _dialog: DialogManager;
  private _input: InputManager;
  private _layout: LayoutManager;
  private _saveUI: SaveUIManager;
  private _settingsUI: SettingsUIManager;
  private _save: SaveManager | null = null;

  // Flat delegates — input
  readonly registerCommand: (id: string, binding: string, handler: () => void) => void;
  readonly removeCommand: (id: string) => void;
  readonly triggerCommand: (id: string) => void;
  readonly getCommands: () => Command[];

  // Flat delegates — dialog
  readonly showDialog: (title: string, content: DocumentFragment | HTMLElement) => void;
  readonly closeDialog: () => void;
  readonly isDialogOpen: () => boolean;

  // Flat delegates — save UI
  readonly showSaveUI: (
    slots: SaveSlotInfo[],
    onSave: (i: number) => void,
    onLoad: (i: number) => void,
    onDelete: (i: number) => void,
  ) => void;
  readonly closeSaveUI: () => void;

  // Flat delegates — settings UI
  readonly showSettingsUI: (
    entries: SettingUIEntry[],
    onSet: (name: string, value: boolean | number | string) => void,
    onReset: () => void,
  ) => void;

  // Flat delegates — layout
  readonly renderSidebar: (config: SidebarConfig) => void;
  readonly stowSidebar: () => void;
  readonly unstowSidebar: () => void;
  readonly toggleSidebar: () => void;
  readonly destroySidebar: () => void;

  // Audio re-exports (pure functions, stateless)
  readonly createAudio = createAudio;
  readonly playAudio = playAudio;
  readonly pauseAudio = pauseAudio;
  readonly stopAudio = stopAudio;
  readonly setVolume = setVolume;
  readonly setMuted = setMuted;
  readonly setLoop = setLoop;
  readonly seekAudio = seekAudio;
  readonly getAudioDuration = getAudioDuration;
  readonly getAudioTime = getAudioTime;
  readonly fadeAudio = fadeAudio;
  readonly isAudioReady = isAudioReady;

  constructor() {
    this._overlay = new OverlayManager();
    this._dialog = new DialogManager(this._overlay);
    this._input = new InputManager();
    this._layout = new LayoutManager();
    this._saveUI = new SaveUIManager(
      this._dialog.showDialog.bind(this._dialog),
      this._dialog.closeDialog.bind(this._dialog),
    );
    this._settingsUI = new SettingsUIManager(
      this._dialog.showDialog.bind(this._dialog),
      this._dialog.closeDialog.bind(this._dialog),
    );

    // Wire cross-concern callbacks (dialog and layout register their commands)
    this._dialog.initCommands(this._input.registerCommand.bind(this._input));
    this._layout.initCommands(this._input.registerCommand.bind(this._input));

    // Bind flat delegates
    this.registerCommand = this._input.registerCommand.bind(this._input);
    this.removeCommand = this._input.removeCommand.bind(this._input);
    this.triggerCommand = this._input.triggerCommand.bind(this._input);
    this.getCommands = this._input.getCommands.bind(this._input);

    this.showDialog = this._dialog.showDialog.bind(this._dialog);
    this.closeDialog = this._dialog.closeDialog.bind(this._dialog);
    this.isDialogOpen = this._dialog.isDialogOpen.bind(this._dialog);

    this.showSaveUI = this._saveUI.showSaveUI.bind(this._saveUI);
    this.closeSaveUI = this._saveUI.closeSaveUI.bind(this._saveUI);

    this.showSettingsUI = this._settingsUI.showSettingsUI.bind(this._settingsUI);

    this.renderSidebar = this._layout.renderSidebar.bind(this._layout);
    this.stowSidebar = this._layout.stowSidebar.bind(this._layout);
    this.unstowSidebar = this._layout.unstowSidebar.bind(this._layout);
    this.toggleSidebar = this._layout.toggleSidebar.bind(this._layout);
    this.destroySidebar = this._layout.destroySidebar.bind(this._layout);
  }

  /** Initialize the save service. Must be called once engine state is available. */
  initSave(
    state: SaveableState,
    backend: SaveBackend,
    gotoFn: (passage: string) => void,
    prefix?: string,
    autosave?: boolean,
  ): void {
    this._save = new SaveManager(
      state,
      backend,
      gotoFn,
      this._input.registerCommand.bind(this._input),
      this._saveUI.showSaveUI.bind(this._saveUI),
      prefix,
      autosave,
    );
  }

  // Save delegates (require initSave() to have been called first)
  commitSave(): void { this._save!.commit(); }
  tryResume(): string | undefined { return this._save!.tryResume(); }
  clearAutosave(): void { this._save!.clearAutosave(); }
  saveSlot(name: string): boolean { return this._save!.saveSlot(name); }
  loadSlot(name: string): string | undefined { return this._save!.loadSlot(name); }
  deleteSlot(name: string): void { this._save!.deleteSlot(name); }
  hasSlot(name: string): boolean { return this._save!.hasSlot(name); }
  slotCount(): number { return this._save!.slotCount(); }
  totalSlots(): number { return this._save!.totalSlots(); }
}
