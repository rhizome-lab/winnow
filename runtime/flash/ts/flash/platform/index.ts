/**
 * Platform interface — re-exports from per-concern implementation modules.
 *
 * To swap a concern, change its import source below. The bundler resolves
 * at build time; tree-shaking eliminates unused implementations.
 */
export {
  initCanvas,
  createMeasureContext,
} from "./graphics";

export {
  addCanvasEventListener,
  addDocumentEventListener,
  getCanvasBounds,
} from "./input";

export {
  fetchResource,
  hasFetch,
} from "./network";

export {
  loadLocal,
  saveLocal,
  removeLocal,
} from "./persistence";

export {
  scheduleInterval,
  cancelScheduledInterval,
} from "./timing";

export { loadImageBitmap } from "./images";

export { triggerDownload } from "./files";
