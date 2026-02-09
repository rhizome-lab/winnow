/**
 * flash.display package — EventDispatcher, display hierarchy, Graphics,
 * Loader, LoaderInfo, Stage.
 */

import { Point, Rectangle, Matrix } from "./flash_geom";
import { Event, ProgressEvent, IOErrorEvent } from "./flash_events";

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
    event.currentTarget = this;
    const list = this._listeners.get(event.type);
    if (!list) return false;
    for (const entry of [...list]) {
      entry.listener(event);
      if (event._isImmediateStopped()) break;
    }
    return !event.isDefaultPrevented();
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
// Graphics
// ---------------------------------------------------------------------------

interface DrawCommand {
  kind: string;
  args: any[];
}

export class Graphics {
  /** @internal */
  _commands: DrawCommand[] = [];

  beginBitmapFill(
    _bitmap: any,
    _matrix: Matrix | null = null,
    _repeat = true,
    _smooth = false,
  ): void {
    this._commands.push({ kind: "beginBitmapFill", args: [_bitmap, _matrix, _repeat, _smooth] });
  }

  beginFill(color: number, alpha = 1): void {
    this._commands.push({ kind: "beginFill", args: [color, alpha] });
  }

  beginGradientFill(
    type: string,
    colors: number[],
    alphas: number[],
    ratios: number[],
    matrix: Matrix | null = null,
    spreadMethod = "pad",
    interpolationMethod = "rgb",
    focalPointRatio = 0,
  ): void {
    this._commands.push({
      kind: "beginGradientFill",
      args: [type, colors, alphas, ratios, matrix, spreadMethod, interpolationMethod, focalPointRatio],
    });
  }

  clear(): void {
    this._commands.length = 0;
  }

  copyFrom(sourceGraphics: Graphics): void {
    this._commands = [...sourceGraphics._commands];
  }

  curveTo(controlX: number, controlY: number, anchorX: number, anchorY: number): void {
    this._commands.push({ kind: "curveTo", args: [controlX, controlY, anchorX, anchorY] });
  }

  drawCircle(x: number, y: number, radius: number): void {
    this._commands.push({ kind: "drawCircle", args: [x, y, radius] });
  }

  drawEllipse(x: number, y: number, width: number, height: number): void {
    this._commands.push({ kind: "drawEllipse", args: [x, y, width, height] });
  }

  drawPath(commands: number[], data: number[], winding = "evenOdd"): void {
    this._commands.push({ kind: "drawPath", args: [commands, data, winding] });
  }

  drawRect(x: number, y: number, width: number, height: number): void {
    this._commands.push({ kind: "drawRect", args: [x, y, width, height] });
  }

  drawRoundRect(
    x: number,
    y: number,
    width: number,
    height: number,
    ellipseWidth: number,
    ellipseHeight?: number,
  ): void {
    this._commands.push({
      kind: "drawRoundRect",
      args: [x, y, width, height, ellipseWidth, ellipseHeight ?? ellipseWidth],
    });
  }

  drawTriangles(
    vertices: number[],
    indices: number[] | null = null,
    uvtData: number[] | null = null,
    culling = "none",
  ): void {
    this._commands.push({ kind: "drawTriangles", args: [vertices, indices, uvtData, culling] });
  }

  endFill(): void {
    this._commands.push({ kind: "endFill", args: [] });
  }

  lineGradientStyle(
    type: string,
    colors: number[],
    alphas: number[],
    ratios: number[],
    matrix: Matrix | null = null,
    spreadMethod = "pad",
    interpolationMethod = "rgb",
    focalPointRatio = 0,
  ): void {
    this._commands.push({
      kind: "lineGradientStyle",
      args: [type, colors, alphas, ratios, matrix, spreadMethod, interpolationMethod, focalPointRatio],
    });
  }

  lineStyle(
    thickness?: number,
    color = 0,
    alpha = 1,
    pixelHinting = false,
    scaleMode = "normal",
    caps: string | null = null,
    joints: string | null = null,
    miterLimit = 3,
  ): void {
    this._commands.push({
      kind: "lineStyle",
      args: [thickness, color, alpha, pixelHinting, scaleMode, caps, joints, miterLimit],
    });
  }

  lineTo(x: number, y: number): void {
    this._commands.push({ kind: "lineTo", args: [x, y] });
  }

  moveTo(x: number, y: number): void {
    this._commands.push({ kind: "moveTo", args: [x, y] });
  }
}

// ---------------------------------------------------------------------------
// DisplayObject
// ---------------------------------------------------------------------------

export class DisplayObject extends EventDispatcher {
  alpha = 1;
  blendMode = "normal";
  cacheAsBitmap = false;
  filters: any[] = [];
  height = 0;
  loaderInfo: LoaderInfo | null = null;
  mask: DisplayObject | null = null;
  mouseX = 0;
  mouseY = 0;
  name = "";
  opaqueBackground: number | null = null;
  parent: DisplayObjectContainer | null = null;
  root: DisplayObject | null = null;
  rotation = 0;
  rotationX = 0;
  rotationY = 0;
  rotationZ = 0;
  scale9Grid: Rectangle | null = null;
  scaleX = 1;
  scaleY = 1;
  scaleZ = 1;
  scrollRect: Rectangle | null = null;
  stage: Stage | null = null;
  transform: any = null;
  visible = true;
  width = 0;
  x = 0;
  y = 0;
  z = 0;

  getBounds(targetCoordinateSpace: DisplayObject): Rectangle {
    void targetCoordinateSpace;
    return new Rectangle(this.x, this.y, this.width, this.height);
  }

  getRect(targetCoordinateSpace: DisplayObject): Rectangle {
    return this.getBounds(targetCoordinateSpace);
  }

  globalToLocal(point: Point): Point {
    return new Point(point.x - this.x, point.y - this.y);
  }

  hitTestObject(obj: DisplayObject): boolean {
    const a = this.getBounds(this);
    const b = obj.getBounds(obj);
    return a.intersects(b);
  }

  hitTestPoint(x: number, y: number, _shapeFlag = false): boolean {
    return this.getBounds(this).contains(x, y);
  }

  localToGlobal(point: Point): Point {
    return new Point(point.x + this.x, point.y + this.y);
  }
}

// ---------------------------------------------------------------------------
// InteractiveObject
// ---------------------------------------------------------------------------

export class InteractiveObject extends DisplayObject {
  contextMenu: any = null;
  doubleClickEnabled = false;
  focusRect: any = null;
  mouseEnabled = true;
  tabEnabled = false;
  tabIndex = -1;
}

// ---------------------------------------------------------------------------
// DisplayObjectContainer
// ---------------------------------------------------------------------------

export class DisplayObjectContainer extends InteractiveObject {
  mouseChildren = true;
  tabChildren = true;
  private _children: DisplayObject[] = [];

  get numChildren(): number {
    return this._children.length;
  }

  addChild(child: DisplayObject): DisplayObject {
    if (child.parent) {
      (child.parent as DisplayObjectContainer).removeChild(child);
    }
    this._children.push(child);
    child.parent = this;
    child.stage = this.stage;
    return child;
  }

  addChildAt(child: DisplayObject, index: number): DisplayObject {
    if (child.parent) {
      (child.parent as DisplayObjectContainer).removeChild(child);
    }
    this._children.splice(index, 0, child);
    child.parent = this;
    child.stage = this.stage;
    return child;
  }

  contains(child: DisplayObject): boolean {
    return this._children.includes(child);
  }

  getChildAt(index: number): DisplayObject {
    return this._children[index];
  }

  getChildByName(name: string): DisplayObject | null {
    return this._children.find((c) => c.name === name) ?? null;
  }

  getChildIndex(child: DisplayObject): number {
    return this._children.indexOf(child);
  }

  removeChild(child: DisplayObject): DisplayObject {
    const idx = this._children.indexOf(child);
    if (idx !== -1) {
      this._children.splice(idx, 1);
      child.parent = null;
      child.stage = null;
    }
    return child;
  }

  removeChildAt(index: number): DisplayObject {
    const child = this._children[index];
    if (child) {
      this._children.splice(index, 1);
      child.parent = null;
      child.stage = null;
    }
    return child;
  }

  removeChildren(beginIndex = 0, endIndex = 0x7fffffff): void {
    const end = Math.min(endIndex, this._children.length);
    for (let i = end - 1; i >= beginIndex; i--) {
      this.removeChildAt(i);
    }
  }

  setChildIndex(child: DisplayObject, index: number): void {
    const cur = this._children.indexOf(child);
    if (cur === -1) return;
    this._children.splice(cur, 1);
    this._children.splice(index, 0, child);
  }

  swapChildren(child1: DisplayObject, child2: DisplayObject): void {
    const i1 = this._children.indexOf(child1);
    const i2 = this._children.indexOf(child2);
    if (i1 === -1 || i2 === -1) return;
    this._children[i1] = child2;
    this._children[i2] = child1;
  }

  swapChildrenAt(index1: number, index2: number): void {
    const tmp = this._children[index1];
    this._children[index1] = this._children[index2];
    this._children[index2] = tmp;
  }
}

// ---------------------------------------------------------------------------
// Sprite
// ---------------------------------------------------------------------------

export class Sprite extends DisplayObjectContainer {
  buttonMode = false;
  dropTarget: DisplayObject | null = null;
  graphics: Graphics = new Graphics();
  hitArea: Sprite | null = null;
  soundTransform: any = null;
  useHandCursor = true;

  startDrag(lockCenter = false, bounds: Rectangle | null = null): void {
    void lockCenter;
    void bounds;
  }

  stopDrag(): void {}
}

// ---------------------------------------------------------------------------
// MovieClip
// ---------------------------------------------------------------------------

export class MovieClip extends Sprite {
  currentFrame = 1;
  currentFrameLabel: string | null = null;
  currentLabel: string | null = null;
  currentLabels: any[] = [];
  currentScene: any = null;
  enabled = true;
  framesLoaded = 1;
  isPlaying = false;
  scenes: any[] = [];
  totalFrames = 1;
  trackAsMenu = false;

  addFrameScript(..._args: any[]): void {}

  gotoAndPlay(frame: number | string, _scene: string | null = null): void {
    if (typeof frame === "number") this.currentFrame = frame;
    this.isPlaying = true;
  }

  gotoAndStop(frame: number | string, _scene: string | null = null): void {
    if (typeof frame === "number") this.currentFrame = frame;
    this.isPlaying = false;
  }

  nextFrame(): void {
    this.currentFrame++;
    this.isPlaying = false;
  }

  nextScene(): void {}

  play(): void {
    this.isPlaying = true;
  }

  prevFrame(): void {
    this.currentFrame--;
    this.isPlaying = false;
  }

  prevScene(): void {}

  stop(): void {
    this.isPlaying = false;
  }
}

// ---------------------------------------------------------------------------
// LoaderInfo
// ---------------------------------------------------------------------------

export class LoaderInfo extends EventDispatcher {
  actionScriptVersion = 3;
  applicationDomain: any = null;
  bytes: any = null;
  bytesLoaded = 0;
  bytesTotal = 0;
  childAllowsParent = true;
  content: DisplayObject | null = null;
  contentType = "";
  frameRate = 24;
  height = 0;
  loader: Loader | null = null;
  loaderURL = "";
  parameters: Record<string, string> = {};
  sameDomain = true;
  sharedEvents: EventDispatcher = new EventDispatcher();
  swfVersion = 0;
  url = "";
  width = 0;
}

// ---------------------------------------------------------------------------
// Loader
// ---------------------------------------------------------------------------

export class Loader extends DisplayObjectContainer {
  content: DisplayObject | null = null;
  contentLoaderInfo: LoaderInfo = new LoaderInfo();

  constructor() {
    super();
    this.contentLoaderInfo.loader = this;
  }

  close(): void {}

  load(request: any, _context?: any): void {
    const url: string = typeof request === "string" ? request : request?.url ?? "";
    this.contentLoaderInfo.url = url;
    // Attempt to load as image via fetch.
    if (typeof globalThis.fetch === "function") {
      globalThis
        .fetch(url)
        .then((res) => {
          this.contentLoaderInfo.bytesTotal = Number(
            res.headers.get("content-length") ?? 0,
          );
          return res.blob();
        })
        .then((blob) => {
          this.contentLoaderInfo.bytesLoaded = blob.size;
          this.contentLoaderInfo.bytesTotal = blob.size;
          this.contentLoaderInfo.dispatchEvent(
            new ProgressEvent(ProgressEvent.PROGRESS, false, false, blob.size, blob.size),
          );
          this.contentLoaderInfo.dispatchEvent(new Event(Event.COMPLETE));
        })
        .catch((err) => {
          this.contentLoaderInfo.dispatchEvent(
            new IOErrorEvent(IOErrorEvent.IO_ERROR, false, false, String(err)),
          );
        });
    }
  }

  loadBytes(bytes: any, _context?: any): void {
    void bytes;
    this.contentLoaderInfo.dispatchEvent(new Event(Event.COMPLETE));
  }

  unload(): void {
    this.content = null;
  }

  unloadAndStop(_gc = true): void {
    this.content = null;
  }
}

// ---------------------------------------------------------------------------
// Stage
// ---------------------------------------------------------------------------

export class Stage extends DisplayObjectContainer {
  stageWidth = 550;
  stageHeight = 400;
  frameRate = 24;
  quality = "HIGH";
  focus: InteractiveObject | null = null;
  align = "";
  scaleMode = "showAll";
  showDefaultContextMenu = true;
  stageFocusRect = true;
  color = 0xffffff;
  displayState = "normal";
  fullScreenSourceRect: Rectangle | null = null;
  fullScreenWidth = 0;
  fullScreenHeight = 0;

  invalidate(): void {}
}
