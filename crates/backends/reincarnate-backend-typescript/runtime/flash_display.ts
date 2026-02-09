/**
 * Flash standard library class stubs.
 *
 * Minimal implementations of the Flash display hierarchy and other base
 * classes so that emitted code can `extends` them without a ReferenceError.
 */

// ---------------------------------------------------------------------------
// Events
// ---------------------------------------------------------------------------

export class Event {
  static readonly ENTER_FRAME = "enterFrame";
  static readonly ADDED_TO_STAGE = "addedToStage";
  static readonly REMOVED_FROM_STAGE = "removedFromStage";
  static readonly COMPLETE = "complete";
  static readonly ACTIVATE = "activate";
  static readonly DEACTIVATE = "deactivate";

  type: string;
  bubbles: boolean;
  cancelable: boolean;

  constructor(type: string, bubbles = false, cancelable = false) {
    this.type = type;
    this.bubbles = bubbles;
    this.cancelable = cancelable;
  }

  clone(): Event {
    return new Event(this.type, this.bubbles, this.cancelable);
  }

  toString(): string {
    return `[Event type="${this.type}"]`;
  }
}

// ---------------------------------------------------------------------------
// EventDispatcher
// ---------------------------------------------------------------------------

type Listener = (event: Event) => void;

export class EventDispatcher {
  private _listeners: Map<string, Listener[]> = new Map();

  addEventListener(type: string, listener: Listener): void {
    let list = this._listeners.get(type);
    if (!list) {
      list = [];
      this._listeners.set(type, list);
    }
    list.push(listener);
  }

  removeEventListener(type: string, listener: Listener): void {
    const list = this._listeners.get(type);
    if (!list) return;
    const idx = list.indexOf(listener);
    if (idx !== -1) list.splice(idx, 1);
  }

  dispatchEvent(event: Event): boolean {
    const list = this._listeners.get(event.type);
    if (!list) return false;
    for (const fn of list) fn(event);
    return true;
  }

  hasEventListener(type: string): boolean {
    const list = this._listeners.get(type);
    return !!list && list.length > 0;
  }
}

// ---------------------------------------------------------------------------
// Display hierarchy
// ---------------------------------------------------------------------------

export class DisplayObject extends EventDispatcher {
  x = 0;
  y = 0;
  width = 0;
  height = 0;
  scaleX = 1;
  scaleY = 1;
  rotation = 0;
  alpha = 1;
  visible = true;
  name = "";
  parent: DisplayObjectContainer | null = null;
}

export class InteractiveObject extends DisplayObject {
  tabEnabled = false;
  tabIndex = -1;
  mouseEnabled = true;
}

export class DisplayObjectContainer extends InteractiveObject {
  private _children: DisplayObject[] = [];

  get numChildren(): number {
    return this._children.length;
  }

  addChild(child: DisplayObject): DisplayObject {
    this._children.push(child);
    child.parent = this;
    return child;
  }

  addChildAt(child: DisplayObject, index: number): DisplayObject {
    this._children.splice(index, 0, child);
    child.parent = this;
    return child;
  }

  removeChild(child: DisplayObject): DisplayObject {
    const idx = this._children.indexOf(child);
    if (idx !== -1) {
      this._children.splice(idx, 1);
      child.parent = null;
    }
    return child;
  }

  removeChildAt(index: number): DisplayObject {
    const child = this._children[index];
    if (child) {
      this._children.splice(index, 1);
      child.parent = null;
    }
    return child;
  }

  getChildAt(index: number): DisplayObject {
    return this._children[index];
  }

  getChildByName(name: string): DisplayObject | null {
    return this._children.find((c) => c.name === name) ?? null;
  }

  contains(child: DisplayObject): boolean {
    return this._children.includes(child);
  }
}

export class Sprite extends DisplayObjectContainer {
  graphics: any = {};
}

export class MovieClip extends Sprite {
  currentFrame = 1;
  totalFrames = 1;

  play(): void {}
  stop(): void {}

  gotoAndPlay(frame: number | string): void {
    if (typeof frame === "number") this.currentFrame = frame;
  }

  gotoAndStop(frame: number | string): void {
    if (typeof frame === "number") this.currentFrame = frame;
  }

  nextFrame(): void {
    this.currentFrame++;
  }

  prevFrame(): void {
    this.currentFrame--;
  }
}

// ---------------------------------------------------------------------------
// ByteArray
// ---------------------------------------------------------------------------

export class ByteArray {
  private _buffer: Uint8Array = new Uint8Array(0);
  position = 0;

  get length(): number {
    return this._buffer.length;
  }

  set length(value: number) {
    const next = new Uint8Array(value);
    next.set(this._buffer.subarray(0, Math.min(this._buffer.length, value)));
    this._buffer = next;
  }

  readByte(): number {
    return this._buffer[this.position++] ?? 0;
  }

  writeByte(value: number): void {
    if (this.position >= this._buffer.length) {
      this.length = this.position + 1;
    }
    this._buffer[this.position++] = value & 0xff;
  }
}

// ---------------------------------------------------------------------------
// Font
// ---------------------------------------------------------------------------

export class Font {
  fontName = "";

  static registerFont(_fontClass: any): void {}
}

// ---------------------------------------------------------------------------
// Proxy
// ---------------------------------------------------------------------------

export class Proxy {}
