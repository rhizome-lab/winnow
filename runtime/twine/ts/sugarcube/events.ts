/** SugarCube event bus.
 *
 * Handles SugarCube-specific events like :passageinit, :passageend,
 * :passagestart, :passagerender, :passagedisplay, :storyready.
 *
 * Triggered from navigation at the appropriate points. Also bridges
 * to jQuery's event system on $(document) so user scripts that register
 * handlers via $(document).on(":passageinit", fn) receive them.
 */

interface HandlerEntry {
  fn: Function;
  once: boolean;
}

export class SCEvents {
  private handlers: Map<string, HandlerEntry[]> = new Map();

  /** Register a handler for an event. */
  on(event: string, fn: Function): void {
    if (!this.handlers.has(event)) this.handlers.set(event, []);
    this.handlers.get(event)!.push({ fn, once: false });
  }

  /** Register a one-shot handler for an event. */
  one(event: string, fn: Function): void {
    if (!this.handlers.has(event)) this.handlers.set(event, []);
    this.handlers.get(event)!.push({ fn, once: true });
  }

  /** Remove a specific handler for an event. */
  off(event: string, fn: Function): void {
    const list = this.handlers.get(event);
    if (!list) return;
    const idx = list.findIndex(h => h.fn === fn);
    if (idx >= 0) list.splice(idx, 1);
  }

  /** Trigger an event, calling all registered handlers and bridging to jQuery. */
  trigger(event: string, ...args: any[]): void {
    const list = this.handlers.get(event);
    if (list) {
      const snapshot = [...list];
      for (const entry of snapshot) {
        try {
          entry.fn(...args);
        } catch (e) {
          console.error(`[events] error in handler for "${event}":`, e);
        }
        if (entry.once) {
          const idx = list.indexOf(entry);
          if (idx >= 0) list.splice(idx, 1);
        }
      }
    }

    const $ = (globalThis as any).jQuery;
    if ($) {
      try {
        const jqEvent = $.Event(event);
        if (args.length > 0 && typeof args[0] === "object") {
          Object.assign(jqEvent, args[0]);
        }
        $(document).trigger(jqEvent);
      } catch (e) {
        console.error(`[events] error in jQuery trigger for "${event}":`, e);
      }
    }
  }
}
