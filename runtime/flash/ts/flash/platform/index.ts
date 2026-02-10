/**
 * Platform interface — re-exports from the active platform implementation.
 *
 * To swap platforms, change the import source below. The bundler resolves
 * this at build time; tree-shaking eliminates the unused implementation.
 * The result is direct function calls with zero indirection.
 *
 * Implementations:
 *   ./browser  — Canvas 2D, DOM events, fetch, localStorage (default)
 *   ./null     — No-op stubs for testing (future)
 */
export {
  initCanvas,
  createMeasureContext,
  addCanvasEventListener,
  addDocumentEventListener,
  getCanvasBounds,
  fetchResource,
  hasFetch,
  loadLocal,
  saveLocal,
  removeLocal,
  scheduleInterval,
  cancelScheduledInterval,
  loadImageBitmap,
  triggerDownload,
} from "./browser";
