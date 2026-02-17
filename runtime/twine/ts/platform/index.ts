/** Platform interface — re-exports from per-concern implementation modules.
 *
 * Swap any concern by replacing its module with a different implementation.
 * For example, swap persistence by pointing at "./cloud-persistence" instead
 * of "./persistence", or add gamepad input alongside keyboard by swapping
 * "./input" for a module that merges both.
 */

export { loadLocal, saveLocal, removeLocal, localStorageBackend } from "./persistence";

export {
  type SaveBackend, type SaveableState,
  init as initSave,
  saveSlot, loadSlot, deleteSlot, hasSlot, slotCount, totalSlots,
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

export { registerCommand, removeCommand, triggerCommand, getCommands } from "./input";

export { showDialog, closeDialog, isDialogOpen } from "./dialog";

export { type SaveSlotInfo, showSaveUI, closeSaveUI } from "./save-ui";

export { type SettingUIEntry, showSettingsUI } from "./settings-ui";

export {
  type SidebarConfig,
  renderSidebar, stowSidebar, unstowSidebar, toggleSidebar, destroySidebar,
} from "./layout";

// Wire cross-concern dependencies via init callbacks.
import { registerCommand } from "./input";
import { showDialog, closeDialog } from "./dialog";
import { initCommands as initDialogCommands } from "./dialog";
import { initCommands as initLayoutCommands } from "./layout";
import { init as initSaveUI } from "./save-ui";
import { init as initSettingsUI } from "./settings-ui";

initSaveUI(showDialog, closeDialog);
initSettingsUI(showDialog, closeDialog);
initDialogCommands(registerCommand);
initLayoutCommands(registerCommand);
