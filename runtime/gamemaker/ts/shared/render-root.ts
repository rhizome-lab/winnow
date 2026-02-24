/** RenderRoot — composable mount + isolation for engine runtimes.
 *
 * A RenderRoot provides a container element and a document-like factory
 * for creating DOM nodes. Wrapper functions compose via plain function
 * application:
 *
 *   const root = shadowDOM()(mount("#game"));
 *   const root = iframe()(mount("#game"));
 *
 * Each wrapper is `(RenderRoot) => RenderRoot`. `mount()` produces the
 * initial root. The runtime constructor receives the final RenderRoot —
 * it doesn't know how it was built.
 */

/** Document-like factory for creating DOM nodes. */
export interface DocumentFactory {
  createElement(tag: string): HTMLElement;
  createTextNode(data: string): Text;
  createDocumentFragment(): DocumentFragment;
}

/** Where to render content + how to create DOM nodes. */
export interface RenderRoot {
  /** Where to append content (canvas, passages, tw-story children). */
  container: Element | ShadowRoot;
  /** Document-like factory for createElement/createTextNode/createDocumentFragment. */
  doc: DocumentFactory;
}

/** A wrapper that transforms a RenderRoot into another. */
export type RenderRootWrapper = (root: RenderRoot) => RenderRoot;

/**
 * Produce the initial RenderRoot from a selector or element.
 *
 * Resolves the container and uses its ownerDocument as the factory.
 */
export function mount(selectorOrElement: string | Element): RenderRoot {
  const el =
    typeof selectorOrElement === "string"
      ? document.querySelector(selectorOrElement)
      : selectorOrElement;
  if (!el) {
    throw new Error(`mount: element not found: ${selectorOrElement}`);
  }
  const doc = el.ownerDocument;
  return { container: el, doc };
}

/**
 * Wrap the root's container in a shadow DOM boundary.
 *
 * Content is rendered inside the shadow root. The factory uses the
 * shadow root's ownerDocument (same document, just scoped rendering).
 */
export function shadowDOM(mode: ShadowRootMode = "open"): RenderRootWrapper {
  return (root: RenderRoot) => {
    const host =
      root.container instanceof Element
        ? root.container
        : (root.container as ShadowRoot).host;
    const shadow = host.attachShadow({ mode });
    return { container: shadow, doc: root.doc };
  };
}

/**
 * Wrap the root's container in an iframe.
 *
 * The iframe is appended to the current container. The factory switches
 * to the iframe's contentDocument so all created elements belong to the
 * iframe's document context.
 */
export function iframe(opts?: { sandbox?: string }): RenderRootWrapper {
  return (root: RenderRoot) => {
    const frame = root.doc.createElement("iframe") as HTMLIFrameElement;
    if (opts?.sandbox !== undefined) {
      frame.setAttribute("sandbox", opts.sandbox);
    }
    // Style to fill container by default.
    frame.style.border = "none";
    frame.style.width = "100%";
    frame.style.height = "100%";
    root.container.appendChild(frame);
    const contentDoc = frame.contentDocument!;
    return { container: contentDoc.body, doc: contentDoc };
  };
}

/**
 * Identity wrapper — CSS scoping is a style-injection concern, not DOM
 * structure. This exists so composition reads naturally:
 *
 *   scopedCSS()(shadowDOM()(mount("#game")))
 */
export function scopedCSS(): RenderRootWrapper {
  return (root: RenderRoot) => root;
}
