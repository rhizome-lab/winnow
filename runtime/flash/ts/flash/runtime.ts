/**
 * Flash runtime — FlashRuntime instance owns Canvas2D, Stage,
 * ENTER_FRAME dispatch, and DOM → Flash event bridging.
 */

import {
  Stage,
  DisplayObject,
  DisplayObjectContainer,
  InteractiveObject,
  Sprite,
  MovieClip,
  Graphics,
  _dragStateByStage,
  _setConstructingRoot,
} from "./display";
import {
  Event,
  MouseEvent as FlashMouseEvent,
  KeyboardEvent as FlashKeyboardEvent,
} from "./events";
import { Point, Rectangle, Matrix } from "./geom";
import {
  initCanvas,
  createCanvas,
  addCanvasEventListener,
  addDocumentEventListener,
  getCanvasBounds,
} from "./platform";
import type { RenderRoot } from "../../../shared/ts/render-root";

// ---------------------------------------------------------------------------
// Constants
// ---------------------------------------------------------------------------

const DEG_TO_RAD = Math.PI / 180;

// ---------------------------------------------------------------------------
// FlashRuntime
// ---------------------------------------------------------------------------

export interface FlashRuntimeOptions {
  root?: RenderRoot;
}

export class FlashRuntime {
  canvas: HTMLCanvasElement;
  ctx: CanvasRenderingContext2D;
  stage: Stage;

  // Timing state (per-instance).
  private _lastTime = 0;
  private _dt = 0;
  private _frames = 0;

  constructor(canvasId: string, opts?: FlashRuntimeOptions) {
    if (opts?.root) {
      const canvas = createCanvas(opts.root.doc);
      canvas.id = canvasId;
      opts.root.container.appendChild(canvas);
      this.canvas = canvas;
      this.ctx = canvas.getContext("2d")!;
    } else {
      const { canvas, ctx } = initCanvas(canvasId);
      this.canvas = canvas;
      this.ctx = ctx;
    }
    this.stage = new Stage();
    this.stage.stageWidth = this.canvas.width;
    this.stage.stageHeight = this.canvas.height;
    // Stage's own .stage points to itself (Flash behaviour).
    this.stage.stage = this.stage;
  }

  /**
   * Construct the document class (root timeline MovieClip) and add it to the
   * stage.  In Flash, the document class has `this.stage` available during its
   * constructor — this function emulates that by setting a pending stage
   * reference that DisplayObject's constructor picks up.
   */
  constructRoot<T extends MovieClip>(cls: new () => T): T {
    _setConstructingRoot(this.stage);
    const root = new cls();
    _setConstructingRoot(null);
    this.stage.addChild(root);
    root._executeFrameScript();
    return root;
  }

  /**
   * Start the runtime: construct the root, bind DOM events, and run the
   * requestAnimationFrame game loop.
   */
  start(rootClass: new () => MovieClip): void {
    this.constructRoot(rootClass);
    this._lastTime = performance.now() / 1000;
    this._bindEvents();

    const loop = () => {
      const now = performance.now() / 1000;
      this._dt = now - this._lastTime;
      this._lastTime = now;
      this._frames++;
      this._tick();
      requestAnimationFrame(loop);
    };
    requestAnimationFrame(loop);
  }

  private _tick(): void {
    // Sync stage dimensions to canvas.
    this.stage.stageWidth = this.canvas.width;
    this.stage.stageHeight = this.canvas.height;

    // Dispatch ENTER_FRAME first (matches Flash frame lifecycle).
    dispatchEnterFrame(this.stage);

    // Advance playing MovieClips and execute frame scripts.
    advanceMovieClips(this.stage);

    // If Stage.invalidate() was called, dispatch RENDER before rendering.
    if (this.stage._invalidated) {
      this.stage._invalidated = false;
      this.stage.dispatchEvent(new Event(Event.RENDER, false, false));
    }

    // Clear canvas then render.
    this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
    renderDisplayList(this.stage, this.ctx);
  }

  private _bindEvents(): void {
    const canvas = this.canvas;
    const stage = this.stage;

    // -----------------------------------------------------------------------
    // DOM → Flash mouse event routing
    // -----------------------------------------------------------------------

    const dispatchMouse = (type: string, e: MouseEvent): void => {
      const [sx, sy] = canvasCoords(canvas, e);

      // Update drag target position on mouse move.
      if (type === FlashMouseEvent.MOUSE_MOVE) {
        updateDrag(stage, sx, sy);
      }

      // Find the deepest interactive object under the mouse.
      const target = hitTest(stage, sx, sy) ?? stage;
      // Compute local coordinates for the target.
      const local = target.globalToLocal(new Point(sx, sy));
      const evt = new FlashMouseEvent(
        type,
        true,
        false,
        local.x,
        local.y,
        null,
        e.ctrlKey,
        e.altKey,
        e.shiftKey,
        e.buttons > 0,
        0,
      );
      evt.stageX = sx;
      evt.stageY = sy;
      target.dispatchEvent(evt);
    };

    addCanvasEventListener(canvas, "click", (e: MouseEvent) => dispatchMouse(FlashMouseEvent.CLICK, e));
    addCanvasEventListener(canvas, "mousedown", (e: MouseEvent) => dispatchMouse(FlashMouseEvent.MOUSE_DOWN, e));
    addCanvasEventListener(canvas, "mouseup", (e: MouseEvent) => dispatchMouse(FlashMouseEvent.MOUSE_UP, e));
    addCanvasEventListener(canvas, "mousemove", (e: MouseEvent) => dispatchMouse(FlashMouseEvent.MOUSE_MOVE, e));
    addCanvasEventListener(canvas, "dblclick", (e: MouseEvent) => dispatchMouse(FlashMouseEvent.DOUBLE_CLICK, e));
    addCanvasEventListener(canvas, "mouseover", (e: MouseEvent) => dispatchMouse(FlashMouseEvent.MOUSE_OVER, e));
    addCanvasEventListener(canvas, "mouseout", (e: MouseEvent) => dispatchMouse(FlashMouseEvent.MOUSE_OUT, e));

    addCanvasEventListener(canvas, "wheel", (e: WheelEvent) => {
      const [sx, sy] = canvasCoords(canvas, e);
      const target = hitTest(stage, sx, sy) ?? stage;
      const local = target.globalToLocal(new Point(sx, sy));
      const evt = new FlashMouseEvent(
        FlashMouseEvent.MOUSE_WHEEL,
        true,
        false,
        local.x,
        local.y,
        null,
        e.ctrlKey,
        e.altKey,
        e.shiftKey,
        false,
        e.deltaY > 0 ? -1 : e.deltaY < 0 ? 1 : 0,
      );
      evt.stageX = sx;
      evt.stageY = sy;
      target.dispatchEvent(evt);
    });

    // -----------------------------------------------------------------------
    // DOM → Flash keyboard event routing
    // -----------------------------------------------------------------------

    const dispatchKey = (type: string, e: KeyboardEvent): void => {
      const target = stage.focus ?? stage;
      const evt = new FlashKeyboardEvent(
        type,
        true,
        false,
        e.key.length === 1 ? e.key.charCodeAt(0) : 0,
        e.keyCode,
        0,
        e.ctrlKey,
        e.altKey,
        e.shiftKey,
      );
      target.dispatchEvent(evt);
    };

    addDocumentEventListener("keydown", (e: KeyboardEvent) => dispatchKey(FlashKeyboardEvent.KEY_DOWN, e));
    addDocumentEventListener("keyup", (e: KeyboardEvent) => dispatchKey(FlashKeyboardEvent.KEY_UP, e));
  }
}

export function createFlashRuntime(canvasId = "reincarnate-canvas", opts?: FlashRuntimeOptions): FlashRuntime {
  return new FlashRuntime(canvasId, opts);
}

// ---------------------------------------------------------------------------
// Display list renderer
// ---------------------------------------------------------------------------

function renderDisplayList(
  node: DisplayObject,
  c: CanvasRenderingContext2D,
): void {
  if (!node.visible || node.alpha <= 0) return;

  c.save();

  // Apply transforms.
  c.translate(node.x, node.y);
  if (node.rotation !== 0) {
    c.rotate(node.rotation * DEG_TO_RAD);
  }
  if (node.scaleX !== 1 || node.scaleY !== 1) {
    c.scale(node.scaleX, node.scaleY);
  }
  c.globalAlpha *= node.alpha;

  // scrollRect clipping.
  if (node.scrollRect) {
    const r = node.scrollRect;
    c.beginPath();
    c.rect(r.x, r.y, r.width, r.height);
    c.clip();
    c.translate(-r.x, -r.y);
  }

  // Render graphics if present (Sprite, MovieClip).
  if ((node as any).graphics) {
    renderGraphics((node as any).graphics as Graphics, c);
  }

  // Recurse into children.
  if (node instanceof DisplayObjectContainer) {
    const n = node.numChildren;
    for (let i = 0; i < n; i++) {
      renderDisplayList(node.getChildAt(i), c);
    }
  }

  c.restore();
}

// ---------------------------------------------------------------------------
// Graphics command replay
// ---------------------------------------------------------------------------

function buildGradient(
  c: CanvasRenderingContext2D,
  type: string,
  colors: number[],
  alphas: number[],
  ratios: number[],
  matrix: Matrix | null,
): CanvasGradient {
  let grad: CanvasGradient;
  if (type === "radial") {
    // Flash radial gradient: centered circle mapped through matrix.
    if (matrix) {
      grad = c.createRadialGradient(matrix.tx, matrix.ty, 0, matrix.tx, matrix.ty,
        Math.max(Math.abs(matrix.a), Math.abs(matrix.d)) * 819.2);
    } else {
      grad = c.createRadialGradient(0, 0, 0, 0, 0, 100);
    }
  } else {
    // Linear gradient.
    if (matrix) {
      const hw = Math.abs(matrix.a) * 819.2;
      grad = c.createLinearGradient(matrix.tx - hw, matrix.ty, matrix.tx + hw, matrix.ty);
    } else {
      grad = c.createLinearGradient(-100, 0, 100, 0);
    }
  }
  for (let i = 0; i < colors.length; i++) {
    const stop = (ratios[i] ?? 0) / 255;
    const alpha = alphas[i] ?? 1;
    grad.addColorStop(stop, colorToCSS(colors[i]!, alpha));
  }
  return grad;
}

function renderGraphics(gfx: Graphics, c: CanvasRenderingContext2D): void {
  const cmds = gfx._commands;
  if (cmds.length === 0) return;

  let fillActive = false;
  let strokeActive = false;

  for (const cmd of cmds) {
    switch (cmd.kind) {
      case "beginFill": {
        if (fillActive) c.fill();
        c.beginPath();
        c.fillStyle = colorToCSS(cmd.args[0], cmd.args[1]);
        fillActive = true;
        break;
      }
      case "beginGradientFill": {
        if (fillActive) c.fill();
        c.beginPath();
        const [gType, gColors, gAlphas, gRatios, gMatrix] = cmd.args;
        c.fillStyle = buildGradient(c, gType, gColors, gAlphas, gRatios, gMatrix);
        fillActive = true;
        break;
      }
      case "beginBitmapFill": {
        if (fillActive) c.fill();
        c.beginPath();
        const [bitmap, bMatrix, bRepeat, bSmooth] = cmd.args;
        const source = bitmap as HTMLImageElement | HTMLCanvasElement | ImageBitmap;
        if (source) {
          const repeat = bRepeat !== false ? "repeat" : "no-repeat";
          const pattern = c.createPattern(source, repeat);
          if (pattern) {
            if (bMatrix) {
              const m = bMatrix as Matrix;
              pattern.setTransform(new DOMMatrix([m.a, m.b, m.c, m.d, m.tx, m.ty]));
            }
            c.fillStyle = pattern;
          }
          c.imageSmoothingEnabled = bSmooth !== false;
        }
        fillActive = true;
        break;
      }
      case "endFill": {
        if (fillActive) {
          c.fill();
          fillActive = false;
        }
        if (strokeActive) {
          c.stroke();
        }
        break;
      }
      case "lineStyle": {
        const [thickness, color, alpha] = cmd.args;
        if (thickness == null || thickness < 0) {
          strokeActive = false;
        } else {
          c.lineWidth = thickness;
          c.strokeStyle = colorToCSS(color, alpha);
          strokeActive = true;
        }
        break;
      }
      case "lineGradientStyle": {
        const [lgType, lgColors, lgAlphas, lgRatios, lgMatrix] = cmd.args;
        c.strokeStyle = buildGradient(c, lgType, lgColors, lgAlphas, lgRatios, lgMatrix);
        break;
      }
      case "moveTo": {
        c.moveTo(cmd.args[0], cmd.args[1]);
        break;
      }
      case "lineTo": {
        c.lineTo(cmd.args[0], cmd.args[1]);
        break;
      }
      case "curveTo": {
        c.quadraticCurveTo(cmd.args[0], cmd.args[1], cmd.args[2], cmd.args[3]);
        break;
      }
      case "drawRect": {
        c.rect(cmd.args[0], cmd.args[1], cmd.args[2], cmd.args[3]);
        break;
      }
      case "drawCircle": {
        c.moveTo(cmd.args[0] + cmd.args[2], cmd.args[1]);
        c.arc(cmd.args[0], cmd.args[1], cmd.args[2], 0, Math.PI * 2);
        break;
      }
      case "drawEllipse": {
        const [ex, ey, ew, eh] = cmd.args;
        c.ellipse(ex + ew / 2, ey + eh / 2, ew / 2, eh / 2, 0, 0, Math.PI * 2);
        break;
      }
      case "drawRoundRect": {
        const [rx, ry, rw, rh, rew, reh] = cmd.args;
        c.roundRect(rx, ry, rw, rh, [rew / 2, reh / 2]);
        break;
      }
      case "drawPath": {
        const [pathCmds, pathData, winding] = cmd.args as [number[], number[], string];
        let di = 0;
        c.beginPath();
        const pd = pathData as number[];
        for (const pcmd of pathCmds) {
          switch (pcmd) {
            case 1: // moveTo
              c.moveTo(pd[di++]!, pd[di++]!);
              break;
            case 2: // lineTo
              c.lineTo(pd[di++]!, pd[di++]!);
              break;
            case 3: // curveTo
              c.quadraticCurveTo(pd[di++]!, pd[di++]!, pd[di++]!, pd[di++]!);
              break;
            case 4: // wideMoveTo (extra 0,0 pair)
              di += 2;
              c.moveTo(pd[di++]!, pd[di++]!);
              break;
            case 5: // wideLineTo (extra 0,0 pair)
              di += 2;
              c.lineTo(pd[di++]!, pd[di++]!);
              break;
            case 6: // cubicCurveTo
              c.bezierCurveTo(
                pd[di++]!, pd[di++]!,
                pd[di++]!, pd[di++]!,
                pd[di++]!, pd[di++]!,
              );
              break;
          }
        }
        if (fillActive) c.fill(winding === "nonZero" ? "nonzero" : "evenodd");
        if (strokeActive) c.stroke();
        break;
      }
      case "drawTriangles": {
        const [verts, indices, _uvtData, _culling] = cmd.args as [number[], number[] | null, any, any];
        // Draw flat-color triangles (no UV texture mapping).
        if (indices) {
          for (let i = 0; i < indices.length; i += 3) {
            const i0 = indices[i]! * 2, i1 = indices[i + 1]! * 2, i2 = indices[i + 2]! * 2;
            c.beginPath();
            c.moveTo(verts[i0]!, verts[i0 + 1]!);
            c.lineTo(verts[i1]!, verts[i1 + 1]!);
            c.lineTo(verts[i2]!, verts[i2 + 1]!);
            c.closePath();
            if (fillActive) c.fill();
            if (strokeActive) c.stroke();
          }
        } else {
          for (let i = 0; i < verts.length; i += 6) {
            c.beginPath();
            c.moveTo(verts[i]!, verts[i + 1]!);
            c.lineTo(verts[i + 2]!, verts[i + 3]!);
            c.lineTo(verts[i + 4]!, verts[i + 5]!);
            c.closePath();
            if (fillActive) c.fill();
            if (strokeActive) c.stroke();
          }
        }
        break;
      }
    }
  }

  // Close any open path.
  if (fillActive) c.fill();
  if (strokeActive) c.stroke();
}

function colorToCSS(color: number, alpha: number): string {
  const r = (color >> 16) & 0xff;
  const g = (color >> 8) & 0xff;
  const b = color & 0xff;
  if (alpha >= 1) {
    return `rgb(${r},${g},${b})`;
  }
  return `rgba(${r},${g},${b},${alpha})`;
}

// ---------------------------------------------------------------------------
// ENTER_FRAME dispatch
// ---------------------------------------------------------------------------

function dispatchEnterFrame(node: DisplayObject): void {
  node.dispatchEvent(new Event(Event.ENTER_FRAME, false, false));
  if (node instanceof DisplayObjectContainer) {
    const n = node.numChildren;
    for (let i = 0; i < n; i++) {
      dispatchEnterFrame(node.getChildAt(i));
    }
  }
}

// ---------------------------------------------------------------------------
// MovieClip frame advancement
// ---------------------------------------------------------------------------

function advanceMovieClips(node: DisplayObject): void {
  if (node instanceof MovieClip) {
    const mc = node;
    if (mc.isPlaying && mc.totalFrames > 1) {
      mc._prevFrame = mc.currentFrame;
      mc.currentFrame++;
      if (mc.currentFrame > mc.totalFrames) mc.currentFrame = 1;
      if (mc.currentFrame !== mc._prevFrame) {
        mc._executeFrameScript();
      }
    }
  }
  if (node instanceof DisplayObjectContainer) {
    const n = node.numChildren;
    for (let i = 0; i < n; i++) {
      advanceMovieClips(node.getChildAt(i));
    }
  }
}

// ---------------------------------------------------------------------------
// Hit-testing
// ---------------------------------------------------------------------------

/**
 * Depth-first reverse-child-order hit test. Returns the deepest
 * InteractiveObject under (stageX, stageY), respecting visible,
 * mouseEnabled, and mouseChildren flags.
 */
function hitTest(
  container: DisplayObjectContainer,
  stageX: number,
  stageY: number,
): InteractiveObject | null {
  if (!container.visible) return null;
  // Walk children back-to-front (highest depth first).
  for (let i = container.numChildren - 1; i >= 0; i--) {
    const child = container.getChildAt(i);
    if (!child.visible) continue;
    if (child instanceof DisplayObjectContainer) {
      if (child.mouseChildren !== false) {
        const hit = hitTest(child as DisplayObjectContainer, stageX, stageY);
        if (hit) return hit;
      }
      // If mouseChildren is false, the container itself is the target.
      if (child instanceof InteractiveObject && child.mouseEnabled !== false) {
        if (child.hitTestPoint(stageX, stageY)) return child;
      }
    } else if (child instanceof InteractiveObject) {
      if ((child as InteractiveObject).mouseEnabled !== false && child.hitTestPoint(stageX, stageY)) {
        return child;
      }
    }
  }
  // Check the container itself.
  if (container instanceof InteractiveObject && (container as InteractiveObject).mouseEnabled !== false) {
    if (container.hitTestPoint(stageX, stageY)) return container;
  }
  return null;
}

// ---------------------------------------------------------------------------
// Coordinate helpers
// ---------------------------------------------------------------------------

function canvasCoords(canvas: HTMLCanvasElement, e: MouseEvent): [number, number] {
  const rect = getCanvasBounds(canvas);
  return [e.clientX - rect.left, e.clientY - rect.top];
}

function updateDrag(stage: Stage, sx: number, sy: number): void {
  const drag = _dragStateByStage.get(stage) ?? null;
  if (!drag) return;
  // Compute offset on first move.
  if (isNaN(drag.offsetX)) {
    if (drag.lockCenter) {
      drag.offsetX = 0;
      drag.offsetY = 0;
    } else {
      drag.offsetX = drag.target.x - sx;
      drag.offsetY = drag.target.y - sy;
    }
  }
  let nx = sx + drag.offsetX;
  let ny = sy + drag.offsetY;
  // Apply bounds constraint.
  if (drag.bounds) {
    if (nx < drag.bounds.x) nx = drag.bounds.x;
    if (ny < drag.bounds.y) ny = drag.bounds.y;
    if (nx > drag.bounds.right) nx = drag.bounds.right;
    if (ny > drag.bounds.bottom) ny = drag.bounds.bottom;
  }
  drag.target.x = nx;
  drag.target.y = ny;
}
