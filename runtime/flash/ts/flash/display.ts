/**
 * flash.display package — display hierarchy, Graphics, Loader, LoaderInfo,
 * Stage.
 */

import { Point, Rectangle, Matrix, Transform, _getConcatenatedMatrix } from "./geom";
import { EventDispatcher, Event, ProgressEvent, IOErrorEvent } from "./events";
import { fetchResource, hasFetch, loadImageBitmap } from "./platform";
import { getInstanceTraits, getDefinitionByName } from "./utils";

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
  // All DisplayObject properties are native in AVM2 (getter/setter provided
  // by the VM).  We use backing-field + getter/setter so that field
  // initialisers never trigger subclass setters during construction.
  _alpha = 1;
  _blendMode: string = "normal";
  _cacheAsBitmap = false;
  _filters: any[] = [];
  _height = 0;
  _loaderInfo: LoaderInfo | null = null;
  _mask: DisplayObject | null = null;
  _mouseX = 0;
  _mouseY = 0;
  _name = "";
  _opaqueBackground: number | null = null;
  _parent: DisplayObjectContainer | null = null;
  _root: DisplayObject | null = null;
  _rotation = 0;
  _rotationX = 0;
  _rotationY = 0;
  _rotationZ = 0;
  _scale9Grid: Rectangle | null = null;
  _scaleX = 1;
  _scaleY = 1;
  _scaleZ = 1;
  _scrollRect: Rectangle | null = null;
  _stage: Stage | null = null;
  _transform: Transform;
  _visible = true;
  _width = 0;
  _x = 0;
  _y = 0;
  _z = 0;

  get alpha() { return this._alpha; }
  set alpha(v: number) { this._alpha = v; }
  get blendMode() { return this._blendMode; }
  set blendMode(v: string) { this._blendMode = v; }
  get cacheAsBitmap() { return this._cacheAsBitmap; }
  set cacheAsBitmap(v: boolean) { this._cacheAsBitmap = v; }
  get filters() { return this._filters; }
  set filters(v: any[]) { this._filters = v; }
  get height() { return this._height; }
  set height(v: number) { this._height = v; }
  get loaderInfo() { return this._loaderInfo; }
  set loaderInfo(v: LoaderInfo | null) { this._loaderInfo = v; }
  get mask() { return this._mask; }
  set mask(v: DisplayObject | null) { this._mask = v; }
  get mouseX() { return this._mouseX; }
  set mouseX(v: number) { this._mouseX = v; }
  get mouseY() { return this._mouseY; }
  set mouseY(v: number) { this._mouseY = v; }
  get name() { return this._name; }
  set name(v: string) { this._name = v; }
  get opaqueBackground() { return this._opaqueBackground; }
  set opaqueBackground(v: number | null) { this._opaqueBackground = v; }
  get parent() { return this._parent; }
  set parent(v: DisplayObjectContainer | null) { this._parent = v; }
  get root() { return this._root; }
  set root(v: DisplayObject | null) { this._root = v; }
  get rotation() { return this._rotation; }
  set rotation(v: number) { this._rotation = v; }
  get rotationX() { return this._rotationX; }
  set rotationX(v: number) { this._rotationX = v; }
  get rotationY() { return this._rotationY; }
  set rotationY(v: number) { this._rotationY = v; }
  get rotationZ() { return this._rotationZ; }
  set rotationZ(v: number) { this._rotationZ = v; }
  get scale9Grid() { return this._scale9Grid; }
  set scale9Grid(v: Rectangle | null) { this._scale9Grid = v; }
  get scaleX() { return this._scaleX; }
  set scaleX(v: number) { this._scaleX = v; }
  get scaleY() { return this._scaleY; }
  set scaleY(v: number) { this._scaleY = v; }
  get scaleZ() { return this._scaleZ; }
  set scaleZ(v: number) { this._scaleZ = v; }
  get scrollRect() { return this._scrollRect; }
  set scrollRect(v: Rectangle | null) { this._scrollRect = v; }
  get stage() { return this._stage; }
  set stage(v: Stage | null) { this._stage = v; }
  get transform() { return this._transform; }
  set transform(v: Transform) { this._transform = v; }
  get visible() { return this._visible; }
  set visible(v: boolean) { this._visible = v; }
  get width() { return this._width; }
  set width(v: number) { this._width = v; }
  get x() { return this._x; }
  set x(v: number) { this._x = v; }
  get y() { return this._y; }
  set y(v: number) { this._y = v; }
  get z() { return this._z; }
  set z(v: number) { this._z = v; }

  constructor() {
    super();
    this._transform = new Transform(this);
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
  _contextMenu: object | null = null;
  _doubleClickEnabled = false;
  _focusRect: boolean | null = null;
  _mouseEnabled = true;
  _tabEnabled = false;
  _tabIndex = -1;

  get contextMenu() { return this._contextMenu; }
  set contextMenu(v: object | null) { this._contextMenu = v; }
  get doubleClickEnabled() { return this._doubleClickEnabled; }
  set doubleClickEnabled(v: boolean) { this._doubleClickEnabled = v; }
  get focusRect() { return this._focusRect; }
  set focusRect(v: boolean | null) { this._focusRect = v; }
  get mouseEnabled() { return this._mouseEnabled; }
  set mouseEnabled(v: boolean) { this._mouseEnabled = v; }
  get tabEnabled() { return this._tabEnabled; }
  set tabEnabled(v: boolean) { this._tabEnabled = v; }
  get tabIndex() { return this._tabIndex; }
  set tabIndex(v: number) { this._tabIndex = v; }
}

// ---------------------------------------------------------------------------
// DisplayObjectContainer
// ---------------------------------------------------------------------------

export class DisplayObjectContainer extends InteractiveObject {
  _mouseChildren = true;
  _tabChildren = true;
  private _children: DisplayObject[] = [];

  get mouseChildren() { return this._mouseChildren; }
  set mouseChildren(v: boolean) { this._mouseChildren = v; }
  get tabChildren() { return this._tabChildren; }
  set tabChildren(v: boolean) { this._tabChildren = v; }

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
  _graphics: Graphics = new Graphics();

  get graphics() { return this._graphics; }
  set graphics(v: Graphics) { this._graphics = v; }
}

// ---------------------------------------------------------------------------
// Bitmap
// ---------------------------------------------------------------------------

export class Bitmap extends DisplayObject {
  _bitmapData: any /* BitmapData */ | null = null;
  _pixelSnapping = "auto";
  _smoothing = false;

  get bitmapData() { return this._bitmapData; }
  set bitmapData(v: any /* BitmapData */ | null) { this._bitmapData = v; }
  get pixelSnapping() { return this._pixelSnapping; }
  set pixelSnapping(v: string) { this._pixelSnapping = v; }
  get smoothing() { return this._smoothing; }
  set smoothing(v: boolean) { this._smoothing = v; }

  constructor(bitmapData: any /* BitmapData */ | null = null, pixelSnapping = "auto", smoothing = false) {
    super();
    this._bitmapData = bitmapData;
    this._pixelSnapping = pixelSnapping;
    this._smoothing = smoothing;
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
  _buttonMode = false;
  _dropTarget: DisplayObject | null = null;
  _graphics: Graphics = new Graphics();
  _hitArea: Sprite | null = null;
  _soundTransform: object | null = null;
  _useHandCursor = true;

  get buttonMode() { return this._buttonMode; }
  set buttonMode(v: boolean) { this._buttonMode = v; }
  get dropTarget() { return this._dropTarget; }
  set dropTarget(v: DisplayObject | null) { this._dropTarget = v; }
  get graphics() { return this._graphics; }
  set graphics(v: Graphics) { this._graphics = v; }
  get hitArea() { return this._hitArea; }
  set hitArea(v: Sprite | null) { this._hitArea = v; }
  get soundTransform() { return this._soundTransform; }
  set soundTransform(v: object | null) { this._soundTransform = v; }
  get useHandCursor() { return this._useHandCursor; }
  set useHandCursor(v: boolean) { this._useHandCursor = v; }

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
  _currentFrame = 1;
  _currentFrameLabel: string | null = null;
  _currentLabel: string | null = null;
  _currentLabels: FrameLabel[] = [];
  _currentScene: Scene | null = null;
  _enabled = true;
  _framesLoaded = 1;
  _isPlaying = false;
  _scenes: Scene[] = [];
  _totalFrames = 1;
  _trackAsMenu = false;

  get currentFrame() { return this._currentFrame; }
  set currentFrame(v: number) { this._currentFrame = v; }
  get currentFrameLabel() { return this._currentFrameLabel; }
  set currentFrameLabel(v: string | null) { this._currentFrameLabel = v; }
  get currentLabel() { return this._currentLabel; }
  set currentLabel(v: string | null) { this._currentLabel = v; }
  get currentLabels() { return this._currentLabels; }
  set currentLabels(v: FrameLabel[]) { this._currentLabels = v; }
  get currentScene() { return this._currentScene; }
  set currentScene(v: Scene | null) { this._currentScene = v; }
  get enabled() { return this._enabled; }
  set enabled(v: boolean) { this._enabled = v; }
  get framesLoaded() { return this._framesLoaded; }
  set framesLoaded(v: number) { this._framesLoaded = v; }
  get isPlaying() { return this._isPlaying; }
  set isPlaying(v: boolean) { this._isPlaying = v; }
  get scenes() { return this._scenes; }
  set scenes(v: Scene[]) { this._scenes = v; }
  get totalFrames() { return this._totalFrames; }
  set totalFrames(v: number) { this._totalFrames = v; }
  get trackAsMenu() { return this._trackAsMenu; }
  set trackAsMenu(v: boolean) { this._trackAsMenu = v; }

  /** @internal */
  _frameScripts: Map<number, Function> = new Map();
  /** @internal */
  _prevFrame = 1;

  constructor() {
    super();
    _initTimelineChildren(this);
  }

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
// Timeline child auto-creation
// ---------------------------------------------------------------------------

/** Factory map for built-in display types that can be auto-created as timeline children. */
const _timelineFactories = new Map<string, () => DisplayObject>();

/** Register a factory for a display type name (used by text.ts for TextField). */
export function registerTimelineFactory(typeName: string, factory: () => DisplayObject): void {
  _timelineFactories.set(typeName, factory);
}

/** Check whether a constructor's prototype chain includes DisplayObject. */
function _isDisplayObjectSubclass(ctor: Function): boolean {
  let proto = ctor.prototype;
  while (proto != null) {
    if (proto === DisplayObject.prototype) return true;
    proto = Object.getPrototypeOf(proto);
  }
  return false;
}

/**
 * Auto-create timeline children for a MovieClip instance.
 *
 * In Flash, children placed on the FLA timeline are created and added to the
 * display list BEFORE the AS3 constructor runs.  This function replicates that
 * behaviour by reading the trait registry (populated by registerClassTraits)
 * and instantiating "variable"-kind traits whose types are DisplayObject
 * subclasses.
 */
function _initTimelineChildren(obj: MovieClip): void {
  const traits = getInstanceTraits(obj.constructor);
  if (!traits) return;

  for (const trait of traits) {
    if (trait.kind !== "variable" || !trait.type) continue;

    // Skip if already initialised by the class body or a super constructor.
    if ((obj as any)[trait.name] != null) continue;

    // Try built-in factory first (MovieClip, Sprite, Shape, Bitmap, TextField).
    let factory = _timelineFactories.get(trait.type);

    // Fall back to getDefinitionByName for user-defined types (UIScrollBar, etc.).
    if (!factory) {
      try {
        const ctor = getDefinitionByName(trait.type);
        if (typeof ctor === "function" && _isDisplayObjectSubclass(ctor)) {
          factory = () => new (ctor as any)();
        }
      } catch {
        // Type not registered — skip this trait.
      }
    }

    if (!factory) continue;

    try {
      const child = factory();
      child.name = trait.name;
      obj.addChild(child);
      (obj as any)[trait.name] = child;
    } catch {
      // Constructor may require arguments we can't provide — skip.
    }
  }
}

// Register built-in display type factories.
_timelineFactories.set("MovieClip", () => new MovieClip());
_timelineFactories.set("Sprite", () => new Sprite());
_timelineFactories.set("Shape", () => new Shape());
_timelineFactories.set("Bitmap", () => new Bitmap());

// ---------------------------------------------------------------------------
// LoaderInfo
// ---------------------------------------------------------------------------

export class LoaderInfo extends EventDispatcher {
  _actionScriptVersion = 3;
  _applicationDomain: object | null = null;
  _bytes: ArrayBuffer | null = null;
  _bytesLoaded = 0;
  _bytesTotal = 0;
  _childAllowsParent = true;
  _content: DisplayObject | null = null;
  _contentType = "";
  _frameRate = 24;
  _height = 0;
  _loader: Loader | null = null;
  _loaderURL = "";
  _parameters: Record<string, string> = {};
  _sameDomain = true;
  _sharedEvents: EventDispatcher = new EventDispatcher();
  _swfVersion = 0;
  _url = "";
  _width = 0;

  get actionScriptVersion() { return this._actionScriptVersion; }
  set actionScriptVersion(v: number) { this._actionScriptVersion = v; }
  get applicationDomain() { return this._applicationDomain; }
  set applicationDomain(v: object | null) { this._applicationDomain = v; }
  get bytes() { return this._bytes; }
  set bytes(v: ArrayBuffer | null) { this._bytes = v; }
  get bytesLoaded() { return this._bytesLoaded; }
  set bytesLoaded(v: number) { this._bytesLoaded = v; }
  get bytesTotal() { return this._bytesTotal; }
  set bytesTotal(v: number) { this._bytesTotal = v; }
  get childAllowsParent() { return this._childAllowsParent; }
  set childAllowsParent(v: boolean) { this._childAllowsParent = v; }
  get content() { return this._content; }
  set content(v: DisplayObject | null) { this._content = v; }
  get contentType() { return this._contentType; }
  set contentType(v: string) { this._contentType = v; }
  get frameRate() { return this._frameRate; }
  set frameRate(v: number) { this._frameRate = v; }
  get height() { return this._height; }
  set height(v: number) { this._height = v; }
  get loader() { return this._loader; }
  set loader(v: Loader | null) { this._loader = v; }
  get loaderURL() { return this._loaderURL; }
  set loaderURL(v: string) { this._loaderURL = v; }
  get parameters() { return this._parameters; }
  set parameters(v: Record<string, string>) { this._parameters = v; }
  get sameDomain() { return this._sameDomain; }
  set sameDomain(v: boolean) { this._sameDomain = v; }
  get sharedEvents() { return this._sharedEvents; }
  set sharedEvents(v: EventDispatcher) { this._sharedEvents = v; }
  get swfVersion() { return this._swfVersion; }
  set swfVersion(v: number) { this._swfVersion = v; }
  get url() { return this._url; }
  set url(v: string) { this._url = v; }
  get width() { return this._width; }
  set width(v: number) { this._width = v; }
}

// ---------------------------------------------------------------------------
// Loader
// ---------------------------------------------------------------------------

export class Loader extends DisplayObjectContainer {
  _content: DisplayObject | null = null;
  _contentLoaderInfo: LoaderInfo = new LoaderInfo();
  private _abortController: AbortController | null = null;

  get content() { return this._content; }
  set content(v: DisplayObject | null) { this._content = v; }
  get contentLoaderInfo() { return this._contentLoaderInfo; }
  set contentLoaderInfo(v: LoaderInfo) { this._contentLoaderInfo = v; }

  constructor() {
    super();
    this._contentLoaderInfo.loader = this;
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
  _stageWidth = 550;
  _stageHeight = 400;
  _frameRate = 24;
  _quality = "HIGH";
  _focus: InteractiveObject | null = null;
  _align = "";
  _scaleMode = "showAll";
  _showDefaultContextMenu = true;
  _stageFocusRect = true;
  _color = 0xffffff;
  _displayState = "normal";
  _fullScreenSourceRect: Rectangle | null = null;
  _fullScreenWidth = 0;
  _fullScreenHeight = 0;

  get stageWidth() { return this._stageWidth; }
  set stageWidth(v: number) { this._stageWidth = v; }
  get stageHeight() { return this._stageHeight; }
  set stageHeight(v: number) { this._stageHeight = v; }
  get frameRate() { return this._frameRate; }
  set frameRate(v: number) { this._frameRate = v; }
  get quality() { return this._quality; }
  set quality(v: string) { this._quality = v; }
  get focus() { return this._focus; }
  set focus(v: InteractiveObject | null) { this._focus = v; }
  get align() { return this._align; }
  set align(v: string) { this._align = v; }
  get scaleMode() { return this._scaleMode; }
  set scaleMode(v: string) { this._scaleMode = v; }
  get showDefaultContextMenu() { return this._showDefaultContextMenu; }
  set showDefaultContextMenu(v: boolean) { this._showDefaultContextMenu = v; }
  get stageFocusRect() { return this._stageFocusRect; }
  set stageFocusRect(v: boolean) { this._stageFocusRect = v; }
  get color() { return this._color; }
  set color(v: number) { this._color = v; }
  get displayState() { return this._displayState; }
  set displayState(v: string) { this._displayState = v; }
  get fullScreenSourceRect() { return this._fullScreenSourceRect; }
  set fullScreenSourceRect(v: Rectangle | null) { this._fullScreenSourceRect = v; }
  get fullScreenWidth() { return this._fullScreenWidth; }
  set fullScreenWidth(v: number) { this._fullScreenWidth = v; }
  get fullScreenHeight() { return this._fullScreenHeight; }
  set fullScreenHeight(v: number) { this._fullScreenHeight = v; }

  /** @internal */
  _invalidated = false;

  invalidate(): void {
    this._invalidated = true;
  }
}
