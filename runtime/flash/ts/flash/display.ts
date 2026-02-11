/**
 * flash.display package — display hierarchy, Graphics, Loader, LoaderInfo,
 * Stage.
 */

import { Point, Rectangle, Matrix, Transform, _getConcatenatedMatrix } from "./geom";
import { EventDispatcher, Event, ProgressEvent, IOErrorEvent } from "./events";
import { fetchResource, hasFetch, loadImageBitmap } from "./platform";

// ---------------------------------------------------------------------------
// AS3 display interfaces
// ---------------------------------------------------------------------------

/** AS3 `flash.display.IBitmapDrawable` — can be drawn to a BitmapData. */
export abstract class IBitmapDrawable {}

/** AS3 `flash.display.IGraphicsData` — parameter for Graphics.drawGraphicsData(). */
export abstract class IGraphicsData {}

/** AS3 `flash.display.IGraphicsFill` — fill specification for Graphics methods. */
export abstract class IGraphicsFill {}

/** AS3 `flash.display.IGraphicsPath` — path specification for Graphics methods. */
export abstract class IGraphicsPath {}

/** AS3 `flash.display.IGraphicsStroke` — stroke specification for Graphics methods. */
export abstract class IGraphicsStroke {}

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
  transform: Transform;
  visible = true;
  width = 0;
  x = 0;
  y = 0;
  z = 0;

  constructor() {
    super();
    this.transform = new Transform(this);
  }

  getBounds(targetCoordinateSpace: DisplayObject): Rectangle {
    const selfMatrix = _getConcatenatedMatrix(this);
    const targetMatrix = _getConcatenatedMatrix(targetCoordinateSpace);
    // Compute the matrix that maps from self's local space to target's local space.
    const inv = targetMatrix.clone();
    inv.invert();
    const combined = selfMatrix.clone();
    combined.concat(inv);
    // Transform 4 corners of local AABB.
    const corners = [
      combined.transformPoint(new Point(0, 0)),
      combined.transformPoint(new Point(this.width, 0)),
      combined.transformPoint(new Point(0, this.height)),
      combined.transformPoint(new Point(this.width, this.height)),
    ];
    let minX = corners[0].x, minY = corners[0].y;
    let maxX = corners[0].x, maxY = corners[0].y;
    for (let i = 1; i < 4; i++) {
      if (corners[i].x < minX) minX = corners[i].x;
      if (corners[i].y < minY) minY = corners[i].y;
      if (corners[i].x > maxX) maxX = corners[i].x;
      if (corners[i].y > maxY) maxY = corners[i].y;
    }
    return new Rectangle(minX, minY, maxX - minX, maxY - minY);
  }

  getRect(targetCoordinateSpace: DisplayObject): Rectangle {
    return this.getBounds(targetCoordinateSpace);
  }

  globalToLocal(point: Point): Point {
    const m = _getConcatenatedMatrix(this);
    m.invert();
    return m.transformPoint(point);
  }

  hitTestObject(obj: DisplayObject): boolean {
    const a = this.getBounds(this);
    const b = obj.getBounds(obj);
    return a.intersects(b);
  }

  hitTestPoint(x: number, y: number, _shapeFlag = false): boolean {
    const m = _getConcatenatedMatrix(this);
    m.invert();
    const local = m.transformPoint(new Point(x, y));
    return local.x >= 0 && local.x <= this.width && local.y >= 0 && local.y <= this.height;
  }

  localToGlobal(point: Point): Point {
    const m = _getConcatenatedMatrix(this);
    return m.transformPoint(point);
  }

  override dispatchEvent(event: Event): boolean {
    event.target = this;

    if (event.bubbles) {
      // Build ancestor chain (root first).
      const ancestors: DisplayObject[] = [];
      let node: DisplayObjectContainer | null = this.parent;
      while (node) {
        ancestors.push(node);
        node = node.parent;
      }
      ancestors.reverse();

      // Phase 1: CAPTURING_PHASE (eventPhase = 1)
      event.eventPhase = 1;
      for (const ancestor of ancestors) {
        if (event._isPropagationStopped()) break;
        ancestor._fireListeners(event, true);
      }

      // Phase 2: AT_TARGET (eventPhase = 2)
      if (!event._isPropagationStopped()) {
        event.eventPhase = 2;
        this._fireListeners(event, false);
      }

      // Phase 3: BUBBLING_PHASE (eventPhase = 3)
      event.eventPhase = 3;
      for (let i = ancestors.length - 1; i >= 0; i--) {
        if (event._isPropagationStopped()) break;
        ancestors[i]._fireListeners(event, false);
      }

      return !event.isDefaultPrevented();
    }

    return super.dispatchEvent(event);
  }

  override willTrigger(type: string): boolean {
    if (this.hasEventListener(type)) return true;
    let node: DisplayObjectContainer | null = this.parent;
    while (node) {
      if (node.hasEventListener(type)) return true;
      node = node.parent;
    }
    return false;
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
    if (this.stage) {
      setStageRecursive(child, this.stage);
    }
    return child;
  }

  addChildAt(child: DisplayObject, index: number): DisplayObject {
    if (child.parent) {
      (child.parent as DisplayObjectContainer).removeChild(child);
    }
    this._children.splice(index, 0, child);
    child.parent = this;
    if (this.stage) {
      setStageRecursive(child, this.stage);
    }
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
      if (child.stage) {
        clearStageRecursive(child);
      }
    }
    return child;
  }

  removeChildAt(index: number): DisplayObject {
    const child = this._children[index];
    if (child) {
      this._children.splice(index, 1);
      child.parent = null;
      if (child.stage) {
        clearStageRecursive(child);
      }
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
// Shape
// ---------------------------------------------------------------------------

export class Shape extends DisplayObject {
  graphics: Graphics = new Graphics();
}

// ---------------------------------------------------------------------------
// Bitmap
// ---------------------------------------------------------------------------

export class Bitmap extends DisplayObject {
  bitmapData: any = null;
  pixelSnapping = "auto";
  smoothing = false;

  constructor(bitmapData: any = null, pixelSnapping = "auto", smoothing = false) {
    super();
    this.bitmapData = bitmapData;
    this.pixelSnapping = pixelSnapping;
    this.smoothing = smoothing;
  }
}

// ---------------------------------------------------------------------------
// Drag state (module-level singleton)
// ---------------------------------------------------------------------------

/** @internal */
export let _dragTarget: Sprite | null = null;
/** @internal */
export let _dragBounds: Rectangle | null = null;
/** @internal */
export let _dragLockCenter = false;
/** @internal */
export let _dragOffsetX = NaN;
/** @internal */
export let _dragOffsetY = NaN;

/** @internal */
export function _setDragOffset(ox: number, oy: number): void {
  _dragOffsetX = ox;
  _dragOffsetY = oy;
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
    _dragTarget = this;
    _dragLockCenter = lockCenter;
    _dragBounds = bounds;
    // NaN offset = first-move sentinel; computed on first mouse move.
    _dragOffsetX = NaN;
    _dragOffsetY = NaN;
  }

  stopDrag(): void {
    if (_dragTarget === this) {
      _dragTarget = null;
      _dragBounds = null;
    }
  }
}

// ---------------------------------------------------------------------------
// FrameLabel + Scene
// ---------------------------------------------------------------------------

export class FrameLabel {
  name: string;
  frame: number;

  constructor(name: string, frame: number) {
    this.name = name;
    this.frame = frame;
  }
}

export class Scene {
  name: string;
  labels: FrameLabel[];
  numFrames: number;

  constructor(name: string, labels: FrameLabel[], numFrames: number) {
    this.name = name;
    this.labels = labels;
    this.numFrames = numFrames;
  }
}

// ---------------------------------------------------------------------------
// MovieClip
// ---------------------------------------------------------------------------

export class MovieClip extends Sprite {
  currentFrame = 1;
  currentFrameLabel: string | null = null;
  currentLabel: string | null = null;
  currentLabels: FrameLabel[] = [];
  currentScene: Scene | null = null;
  enabled = true;
  framesLoaded = 1;
  isPlaying = false;
  scenes: Scene[] = [];
  totalFrames = 1;
  trackAsMenu = false;

  /** @internal */
  _frameScripts: Map<number, Function> = new Map();
  /** @internal */
  _prevFrame = 1;

  addFrameScript(...args: any[]): void {
    // Arguments are pairs: (0-based frameIndex, callback | null)
    for (let i = 0; i < args.length - 1; i += 2) {
      const frameIndex = args[i] as number;
      const callback = args[i + 1];
      if (callback == null) {
        this._frameScripts.delete(frameIndex + 1);
      } else {
        this._frameScripts.set(frameIndex + 1, callback as Function);
      }
    }
  }

  /** @internal Resolve a frame number or label string to a 1-based frame number. */
  _resolveFrame(frame: number | string, scene: string | null = null): number {
    if (typeof frame === "number") return frame;
    // Search for matching label in scenes.
    const searchScenes = scene
      ? this.scenes.filter((s) => s.name === scene)
      : this.scenes.length > 0 ? this.scenes : [null];
    for (const sc of searchScenes) {
      const labels = sc ? sc.labels : this.currentLabels;
      for (const label of labels) {
        if (label.name === frame) return label.frame;
      }
    }
    // Fallback: try parsing as number.
    const n = parseInt(frame, 10);
    return isNaN(n) ? this.currentFrame : n;
  }

  /** @internal Execute frame script for current frame if one exists. */
  _executeFrameScript(): void {
    const script = this._frameScripts.get(this.currentFrame);
    if (script) script.call(this);
  }

  gotoAndPlay(frame: number | string, scene: string | null = null): void {
    this.currentFrame = this._resolveFrame(frame, scene);
    this.isPlaying = true;
    this._updateFrameLabel();
    this._executeFrameScript();
  }

  gotoAndStop(frame: number | string, scene: string | null = null): void {
    this.currentFrame = this._resolveFrame(frame, scene);
    this.isPlaying = false;
    this._updateFrameLabel();
    this._executeFrameScript();
  }

  nextFrame(): void {
    if (this.currentFrame < this.totalFrames) this.currentFrame++;
    this.isPlaying = false;
    this._updateFrameLabel();
    this._executeFrameScript();
  }

  nextScene(): void {
    if (this.scenes.length === 0) return;
    const idx = this.currentScene ? this.scenes.indexOf(this.currentScene) : -1;
    if (idx < this.scenes.length - 1) {
      const next = this.scenes[idx + 1];
      this.currentScene = next;
      // Jump to first frame of next scene.
      if (next.labels.length > 0) {
        this.currentFrame = next.labels[0].frame;
      }
    }
  }

  play(): void {
    this.isPlaying = true;
  }

  prevFrame(): void {
    if (this.currentFrame > 1) this.currentFrame--;
    this.isPlaying = false;
    this._updateFrameLabel();
    this._executeFrameScript();
  }

  prevScene(): void {
    if (this.scenes.length === 0) return;
    const idx = this.currentScene ? this.scenes.indexOf(this.currentScene) : -1;
    if (idx > 0) {
      const prev = this.scenes[idx - 1];
      this.currentScene = prev;
      if (prev.labels.length > 0) {
        this.currentFrame = prev.labels[0].frame;
      }
    }
  }

  stop(): void {
    this.isPlaying = false;
  }

  /** @internal Update currentFrameLabel and currentLabel from the labels list. */
  private _updateFrameLabel(): void {
    this.currentFrameLabel = null;
    this.currentLabel = null;
    const labels = this.currentScene?.labels ?? this.currentLabels;
    for (let i = labels.length - 1; i >= 0; i--) {
      if (labels[i].frame <= this.currentFrame) {
        this.currentLabel = labels[i].name;
        if (labels[i].frame === this.currentFrame) {
          this.currentFrameLabel = labels[i].name;
        }
        break;
      }
    }
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
  private _abortController: AbortController | null = null;

  constructor() {
    super();
    this.contentLoaderInfo.loader = this;
  }

  close(): void {
    if (this._abortController) {
      this._abortController.abort();
      this._abortController = null;
    }
  }

  load(request: any, _context?: any): void {
    const url: string = typeof request === "string" ? request : request?.url ?? "";
    this.contentLoaderInfo.url = url;
    this._abortController = new AbortController();
    const signal = this._abortController.signal;
    if (hasFetch()) {
      fetchResource(url, { signal })
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
          if (!signal.aborted) {
            this.contentLoaderInfo.dispatchEvent(
              new IOErrorEvent(IOErrorEvent.IO_ERROR, false, false, String(err)),
            );
          }
        });
    }
  }

  loadBytes(bytes: any, _context?: any): void {
    // Extract data from ByteArray (or ArrayBuffer) and attempt to create an image.
    const data: ArrayBuffer = bytes._buffer
      ? (bytes._buffer as ArrayBuffer).slice(0, bytes.length ?? bytes._length ?? 0)
      : bytes;
    const blob = new Blob([data]);
    loadImageBitmap(blob)
      .then((bmp) => {
        const sprite = new Sprite();
        (sprite as any)._bitmap = bmp;
        sprite.width = bmp.width;
        sprite.height = bmp.height;
        this.content = sprite;
        this.addChild(sprite);
        this.contentLoaderInfo.content = sprite;
        this.contentLoaderInfo.bytesLoaded = data.byteLength;
        this.contentLoaderInfo.bytesTotal = data.byteLength;
        this.contentLoaderInfo.dispatchEvent(new Event(Event.COMPLETE));
      })
      .catch(() => {
        // Not an image — still dispatch COMPLETE (some content types don't produce bitmaps).
        this.contentLoaderInfo.bytesLoaded = data.byteLength;
        this.contentLoaderInfo.bytesTotal = data.byteLength;
        this.contentLoaderInfo.dispatchEvent(new Event(Event.COMPLETE));
      });
  }

  unload(): void {
    if (this.content) {
      this.removeChild(this.content);
      this.content = null;
    }
  }

  unloadAndStop(_gc = true): void {
    this.close();
    if (this.content) {
      this.removeChild(this.content);
      this.content = null;
    }
    this.contentLoaderInfo.content = null;
  }
}

// ---------------------------------------------------------------------------
// Stage propagation helpers
// ---------------------------------------------------------------------------

function setStageRecursive(node: DisplayObject, stage: Stage): void {
  node.stage = stage;
  node.dispatchEvent(new Event(Event.ADDED_TO_STAGE, false, false));
  if (node instanceof DisplayObjectContainer) {
    const n = node.numChildren;
    for (let i = 0; i < n; i++) {
      setStageRecursive(node.getChildAt(i), stage);
    }
  }
}

function clearStageRecursive(node: DisplayObject): void {
  node.dispatchEvent(new Event(Event.REMOVED_FROM_STAGE, false, false));
  node.stage = null;
  if (node instanceof DisplayObjectContainer) {
    const n = node.numChildren;
    for (let i = 0; i < n; i++) {
      clearStageRecursive(node.getChildAt(i));
    }
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

  /** @internal */
  _invalidated = false;

  invalidate(): void {
    this._invalidated = true;
  }
}
