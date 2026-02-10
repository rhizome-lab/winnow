/**
 * flash.events package — EventDispatcher, Event, and all standard event
 * subclasses.
 */

// ---------------------------------------------------------------------------
// EventDispatcher
// ---------------------------------------------------------------------------

interface ListenerEntry {
  listener: (event: Event) => void;
  useCapture: boolean;
  priority: number;
}

export class EventDispatcher {
  private _listeners: Map<string, ListenerEntry[]> = new Map();

  addEventListener(
    type: string,
    listener: (event: Event) => void,
    useCapture = false,
    priority = 0,
    _useWeakReference = false,
  ): void {
    let list = this._listeners.get(type);
    if (!list) {
      list = [];
      this._listeners.set(type, list);
    }
    // Avoid duplicate registrations with the same listener+capture combo.
    for (const entry of list) {
      if (entry.listener === listener && entry.useCapture === useCapture) return;
    }
    list.push({ listener, useCapture, priority });
    // Stable sort by descending priority.
    list.sort((a, b) => b.priority - a.priority);
  }

  removeEventListener(
    type: string,
    listener: (event: Event) => void,
    useCapture = false,
  ): void {
    const list = this._listeners.get(type);
    if (!list) return;
    const idx = list.findIndex(
      (e) => e.listener === listener && e.useCapture === useCapture,
    );
    if (idx !== -1) list.splice(idx, 1);
  }

  dispatchEvent(event: Event): boolean {
    event.target = this;
    event.eventPhase = 2;
    event.currentTarget = this;
    const list = this._listeners.get(event.type);
    if (list) {
      for (const entry of [...list]) {
        entry.listener(event);
        if (event._isImmediateStopped()) break;
      }
    }
    return !event.isDefaultPrevented();
  }

  /** @internal Fire listeners on this object for a given phase. */
  _fireListeners(event: Event, capturePhaseOnly: boolean): void {
    event.currentTarget = this;
    const list = this._listeners.get(event.type);
    if (!list) return;
    for (const entry of [...list]) {
      if (capturePhaseOnly && !entry.useCapture) continue;
      if (!capturePhaseOnly && event.eventPhase === 3 && entry.useCapture) continue;
      entry.listener(event);
      if (event._isImmediateStopped()) break;
    }
  }

  hasEventListener(type: string): boolean {
    const list = this._listeners.get(type);
    return !!list && list.length > 0;
  }

  willTrigger(type: string): boolean {
    return this.hasEventListener(type);
  }
}

// ---------------------------------------------------------------------------
// Event
// ---------------------------------------------------------------------------

export class Event {
  // Core constants
  static readonly ACTIVATE = "activate";
  static readonly ADDED = "added";
  static readonly ADDED_TO_STAGE = "addedToStage";
  static readonly CANCEL = "cancel";
  static readonly CHANGE = "change";
  static readonly CLEAR = "clear";
  static readonly CLOSE = "close";
  static readonly COMPLETE = "complete";
  static readonly CONNECT = "connect";
  static readonly COPY = "copy";
  static readonly CUT = "cut";
  static readonly DEACTIVATE = "deactivate";
  static readonly ENTER_FRAME = "enterFrame";
  static readonly EXIT_FRAME = "exitFrame";
  static readonly FRAME_CONSTRUCTED = "frameConstructed";
  static readonly FRAME_LABEL = "frameLabel";
  static readonly FULLSCREEN = "fullScreen";
  static readonly ID3 = "id3";
  static readonly INIT = "init";
  static readonly MOUSE_LEAVE = "mouseLeave";
  static readonly OPEN = "open";
  static readonly PASTE = "paste";
  static readonly REMOVED = "removed";
  static readonly REMOVED_FROM_STAGE = "removedFromStage";
  static readonly RENDER = "render";
  static readonly RESIZE = "resize";
  static readonly SCROLL = "scroll";
  static readonly SELECT = "select";
  static readonly SELECT_ALL = "selectAll";
  static readonly SOUND_COMPLETE = "soundComplete";
  static readonly TAB_CHILDREN_CHANGE = "tabChildrenChange";
  static readonly TAB_ENABLED_CHANGE = "tabEnabledChange";
  static readonly TAB_INDEX_CHANGE = "tabIndexChange";
  static readonly UNLOAD = "unload";

  type: string;
  bubbles: boolean;
  cancelable: boolean;
  target: any = null;
  currentTarget: any = null;
  eventPhase: number = 0;

  private _defaultPrevented = false;
  private _stopImmediate = false;
  private _stopPropagation = false;

  constructor(type: string, bubbles = false, cancelable = false) {
    this.type = type;
    this.bubbles = bubbles;
    this.cancelable = cancelable;
  }

  clone(): Event {
    return new Event(this.type, this.bubbles, this.cancelable);
  }

  formatToString(className: string, ...args: string[]): string {
    const props = args.map((p) => `${p}=${(this as any)[p]}`).join(", ");
    return `[${className}${props ? " " + props : ""}]`;
  }

  isDefaultPrevented(): boolean {
    return this._defaultPrevented;
  }

  preventDefault(): void {
    if (this.cancelable) this._defaultPrevented = true;
  }

  stopImmediatePropagation(): void {
    this._stopImmediate = true;
    this._stopPropagation = true;
  }

  stopPropagation(): void {
    this._stopPropagation = true;
  }

  /** @internal */
  _isImmediateStopped(): boolean {
    return this._stopImmediate;
  }

  /** @internal */
  _isPropagationStopped(): boolean {
    return this._stopPropagation;
  }

  toString(): string {
    return this.formatToString("Event", "type", "bubbles", "cancelable");
  }
}

// ---------------------------------------------------------------------------
// TextEvent
// ---------------------------------------------------------------------------

export class TextEvent extends Event {
  static readonly LINK = "link";
  static readonly TEXT_INPUT = "textInput";

  text: string;

  constructor(type: string, bubbles = false, cancelable = false, text = "") {
    super(type, bubbles, cancelable);
    this.text = text;
  }

  override clone(): TextEvent {
    return new TextEvent(this.type, this.bubbles, this.cancelable, this.text);
  }

  override toString(): string {
    return this.formatToString("TextEvent", "type", "bubbles", "cancelable", "text");
  }
}

// ---------------------------------------------------------------------------
// ErrorEvent
// ---------------------------------------------------------------------------

export class ErrorEvent extends TextEvent {
  static readonly ERROR = "error";

  errorID: number;

  constructor(
    type: string,
    bubbles = false,
    cancelable = false,
    text = "",
    errorID = 0,
  ) {
    super(type, bubbles, cancelable, text);
    this.errorID = errorID;
  }

  override clone(): ErrorEvent {
    return new ErrorEvent(this.type, this.bubbles, this.cancelable, this.text, this.errorID);
  }

  override toString(): string {
    return this.formatToString("ErrorEvent", "type", "bubbles", "cancelable", "text", "errorID");
  }
}

// ---------------------------------------------------------------------------
// MouseEvent
// ---------------------------------------------------------------------------

export class MouseEvent extends Event {
  static readonly CLICK = "click";
  static readonly CONTEXT_MENU = "contextMenu";
  static readonly DOUBLE_CLICK = "doubleClick";
  static readonly MIDDLE_CLICK = "middleClick";
  static readonly MIDDLE_MOUSE_DOWN = "middleMouseDown";
  static readonly MIDDLE_MOUSE_UP = "middleMouseUp";
  static readonly MOUSE_DOWN = "mouseDown";
  static readonly MOUSE_MOVE = "mouseMove";
  static readonly MOUSE_OUT = "mouseOut";
  static readonly MOUSE_OVER = "mouseOver";
  static readonly MOUSE_UP = "mouseUp";
  static readonly MOUSE_WHEEL = "mouseWheel";
  static readonly RELEASE_OUTSIDE = "releaseOutside";
  static readonly RIGHT_CLICK = "rightClick";
  static readonly RIGHT_MOUSE_DOWN = "rightMouseDown";
  static readonly RIGHT_MOUSE_UP = "rightMouseUp";
  static readonly ROLL_OUT = "rollOut";
  static readonly ROLL_OVER = "rollOver";

  localX: number;
  localY: number;
  stageX: number = 0;
  stageY: number = 0;
  relatedObject: any = null;
  ctrlKey: boolean;
  altKey: boolean;
  shiftKey: boolean;
  buttonDown: boolean;
  delta: number;
  clickCount: number = 0;
  commandKey: boolean = false;
  controlKey: boolean = false;

  constructor(
    type: string,
    bubbles = true,
    cancelable = false,
    localX = 0,
    localY = 0,
    relatedObject: any = null,
    ctrlKey = false,
    altKey = false,
    shiftKey = false,
    buttonDown = false,
    delta = 0,
  ) {
    super(type, bubbles, cancelable);
    this.localX = localX;
    this.localY = localY;
    this.relatedObject = relatedObject;
    this.ctrlKey = ctrlKey;
    this.altKey = altKey;
    this.shiftKey = shiftKey;
    this.buttonDown = buttonDown;
    this.delta = delta;
  }

  override clone(): MouseEvent {
    return new MouseEvent(
      this.type,
      this.bubbles,
      this.cancelable,
      this.localX,
      this.localY,
      this.relatedObject,
      this.ctrlKey,
      this.altKey,
      this.shiftKey,
      this.buttonDown,
      this.delta,
    );
  }

  updateAfterEvent(): void {
    // In a real Flash runtime this forces a screen update.
  }

  override toString(): string {
    return this.formatToString(
      "MouseEvent",
      "type",
      "bubbles",
      "cancelable",
      "localX",
      "localY",
      "stageX",
      "stageY",
    );
  }
}

// ---------------------------------------------------------------------------
// KeyboardEvent
// ---------------------------------------------------------------------------

export class KeyboardEvent extends Event {
  static readonly KEY_DOWN = "keyDown";
  static readonly KEY_UP = "keyUp";

  charCode: number;
  keyCode: number;
  keyLocation: number;
  ctrlKey: boolean;
  altKey: boolean;
  shiftKey: boolean;

  constructor(
    type: string,
    bubbles = true,
    cancelable = false,
    charCode = 0,
    keyCode = 0,
    keyLocation = 0,
    ctrlKey = false,
    altKey = false,
    shiftKey = false,
  ) {
    super(type, bubbles, cancelable);
    this.charCode = charCode;
    this.keyCode = keyCode;
    this.keyLocation = keyLocation;
    this.ctrlKey = ctrlKey;
    this.altKey = altKey;
    this.shiftKey = shiftKey;
  }

  override clone(): KeyboardEvent {
    return new KeyboardEvent(
      this.type,
      this.bubbles,
      this.cancelable,
      this.charCode,
      this.keyCode,
      this.keyLocation,
      this.ctrlKey,
      this.altKey,
      this.shiftKey,
    );
  }

  updateAfterEvent(): void {}

  override toString(): string {
    return this.formatToString(
      "KeyboardEvent",
      "type",
      "bubbles",
      "cancelable",
      "charCode",
      "keyCode",
      "keyLocation",
    );
  }
}

// ---------------------------------------------------------------------------
// FocusEvent
// ---------------------------------------------------------------------------

export class FocusEvent extends Event {
  static readonly FOCUS_IN = "focusIn";
  static readonly FOCUS_OUT = "focusOut";
  static readonly KEY_FOCUS_CHANGE = "keyFocusChange";
  static readonly MOUSE_FOCUS_CHANGE = "mouseFocusChange";

  relatedObject: any;
  shiftKey: boolean;
  keyCode: number;
  direction: string;
  isRelatedObjectInaccessible: boolean;

  constructor(
    type: string,
    bubbles = true,
    cancelable = false,
    relatedObject: any = null,
    shiftKey = false,
    keyCode = 0,
    direction = "none",
    isRelatedObjectInaccessible = false,
  ) {
    super(type, bubbles, cancelable);
    this.relatedObject = relatedObject;
    this.shiftKey = shiftKey;
    this.keyCode = keyCode;
    this.direction = direction;
    this.isRelatedObjectInaccessible = isRelatedObjectInaccessible;
  }

  override clone(): FocusEvent {
    return new FocusEvent(
      this.type,
      this.bubbles,
      this.cancelable,
      this.relatedObject,
      this.shiftKey,
      this.keyCode,
      this.direction,
      this.isRelatedObjectInaccessible,
    );
  }

  override toString(): string {
    return this.formatToString(
      "FocusEvent",
      "type",
      "bubbles",
      "cancelable",
      "relatedObject",
      "shiftKey",
      "keyCode",
    );
  }
}

// ---------------------------------------------------------------------------
// ProgressEvent
// ---------------------------------------------------------------------------

export class ProgressEvent extends Event {
  static readonly PROGRESS = "progress";
  static readonly SOCKET_DATA = "socketData";

  bytesLoaded: number;
  bytesTotal: number;

  constructor(
    type: string,
    bubbles = false,
    cancelable = false,
    bytesLoaded = 0,
    bytesTotal = 0,
  ) {
    super(type, bubbles, cancelable);
    this.bytesLoaded = bytesLoaded;
    this.bytesTotal = bytesTotal;
  }

  override clone(): ProgressEvent {
    return new ProgressEvent(
      this.type,
      this.bubbles,
      this.cancelable,
      this.bytesLoaded,
      this.bytesTotal,
    );
  }

  override toString(): string {
    return this.formatToString(
      "ProgressEvent",
      "type",
      "bubbles",
      "cancelable",
      "bytesLoaded",
      "bytesTotal",
    );
  }
}

// ---------------------------------------------------------------------------
// IOErrorEvent
// ---------------------------------------------------------------------------

export class IOErrorEvent extends ErrorEvent {
  static readonly IO_ERROR = "ioError";

  constructor(
    type: string,
    bubbles = false,
    cancelable = false,
    text = "",
    errorID = 0,
  ) {
    super(type, bubbles, cancelable, text, errorID);
  }

  override clone(): IOErrorEvent {
    return new IOErrorEvent(this.type, this.bubbles, this.cancelable, this.text, this.errorID);
  }

  override toString(): string {
    return this.formatToString("IOErrorEvent", "type", "bubbles", "cancelable", "text");
  }
}

// ---------------------------------------------------------------------------
// SecurityErrorEvent
// ---------------------------------------------------------------------------

export class SecurityErrorEvent extends ErrorEvent {
  static readonly SECURITY_ERROR = "securityError";

  constructor(
    type: string,
    bubbles = false,
    cancelable = false,
    text = "",
    errorID = 0,
  ) {
    super(type, bubbles, cancelable, text, errorID);
  }

  override clone(): SecurityErrorEvent {
    return new SecurityErrorEvent(
      this.type,
      this.bubbles,
      this.cancelable,
      this.text,
      this.errorID,
    );
  }

  override toString(): string {
    return this.formatToString("SecurityErrorEvent", "type", "bubbles", "cancelable", "text");
  }
}

// ---------------------------------------------------------------------------
// HTTPStatusEvent
// ---------------------------------------------------------------------------

export class HTTPStatusEvent extends Event {
  static readonly HTTP_STATUS = "httpStatus";
  static readonly HTTP_RESPONSE_STATUS = "httpResponseStatus";

  status: number;
  responseHeaders: any[] = [];
  responseURL: string = "";

  constructor(type: string, bubbles = false, cancelable = false, status = 0) {
    super(type, bubbles, cancelable);
    this.status = status;
  }

  override clone(): HTTPStatusEvent {
    const e = new HTTPStatusEvent(this.type, this.bubbles, this.cancelable, this.status);
    e.responseHeaders = this.responseHeaders;
    e.responseURL = this.responseURL;
    return e;
  }

  override toString(): string {
    return this.formatToString("HTTPStatusEvent", "type", "bubbles", "cancelable", "status");
  }
}

// ---------------------------------------------------------------------------
// AsyncErrorEvent
// ---------------------------------------------------------------------------

export class AsyncErrorEvent extends ErrorEvent {
  static readonly ASYNC_ERROR = "asyncError";

  error: any;

  constructor(
    type: string,
    bubbles = false,
    cancelable = false,
    text = "",
    error: any = null,
  ) {
    super(type, bubbles, cancelable, text);
    this.error = error;
  }

  override clone(): AsyncErrorEvent {
    return new AsyncErrorEvent(
      this.type,
      this.bubbles,
      this.cancelable,
      this.text,
      this.error,
    );
  }

  override toString(): string {
    return this.formatToString("AsyncErrorEvent", "type", "bubbles", "cancelable", "text");
  }
}

// ---------------------------------------------------------------------------
// TimerEvent
// ---------------------------------------------------------------------------

export class TimerEvent extends Event {
  static readonly TIMER = "timer";
  static readonly TIMER_COMPLETE = "timerComplete";

  constructor(type: string, bubbles = false, cancelable = false) {
    super(type, bubbles, cancelable);
  }

  override clone(): TimerEvent {
    return new TimerEvent(this.type, this.bubbles, this.cancelable);
  }

  updateAfterEvent(): void {}

  override toString(): string {
    return this.formatToString("TimerEvent", "type", "bubbles", "cancelable");
  }
}
