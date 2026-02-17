/**
 * GML Runtime — game loop, GMLObject base class, room system.
 */

import { GraphicsContext, initCanvas, createCanvas, resizeCanvas, loadImage, scheduleFrame } from "./platform";
import type { RenderRoot } from "../../../shared/ts/render-root";
import { DrawState, createDrawAPI } from "./draw";
import { InputState, createInputAPI } from "./input";
import { StorageState, createStorageAPI } from "./storage";
import { MathState, createMathAPI } from "./math";
import { createGlobalAPI } from "./global";
import { createInstanceAPI } from "./instance";
import { ACTIVE, noop } from "./constants";
import type { Sprite } from "../../data/sprites";
import type { Texture } from "../../data/textures";
import type { Font } from "../../data/fonts";
import type { Room } from "../../data/rooms";

// Re-exports for class_preamble
export { Colors, HAligns, VAligns } from "./color";
export { ACTIVE } from "./constants";

// ---- GMLObject ----

const __baseproto = Object.getPrototypeOf(class {});

export class GMLObject {
  _rt!: GameRuntime;
  x = 0;
  y = 0;
  xstart = 0;
  ystart = 0;
  xprevious = 0;
  yprevious = 0;
  image_xscale = 1;
  image_yscale = 1;
  sprite_index: number | undefined = undefined;
  image_index = 0;
  image_alpha = 1;
  persistent = false;
  depth = 0;
  #alarm: number[] | null = null;
  [ACTIVE] = false;
  visible = true;

  get alarm(): number[] {
    if (this.#alarm === null) {
      this.#alarm = [];
    }
    return this.#alarm;
  }
  set alarm(val: number[]) {
    this.#alarm = val;
  }

  get id(): GMLObject { return this; }

  create(): void {}
  destroy(): void {}

  draw(): void {
    if (this.sprite_index === undefined || !this.visible) return;
    this._rt.drawSprite(this.sprite_index, this.image_index, this.x, this.y, this);
  }

  mouseenter(): void {}
  mouseleave(): void {}
  roomstart(): void {}
  roomend(): void {}

  // Event stubs — overridden by subclasses
  beginstep(): void {}
  step(): void {}
  endstep(): void {}
  drawgui(): void {}
}

// Alarm stubs
for (let i = 0; i < 12; i++) {
  (GMLObject.prototype as any)["alarm" + i] = noop;
}
// Key press / keyboard / key release stubs
for (let i = 0; i <= 0xff; i++) {
  (GMLObject.prototype as any)["keypress" + i] = noop;
  (GMLObject.prototype as any)["keyboard" + i] = noop;
  (GMLObject.prototype as any)["keyrelease" + i] = noop;
}
// View event stubs
for (let i = 0; i < 8; i++) {
  (GMLObject.prototype as any)["outsideview" + i] = noop;
  (GMLObject.prototype as any)["boundaryview" + i] = noop;
}
// User event stubs
for (let i = 0; i < 16; i++) {
  (GMLObject.prototype as any)["user" + i] = noop;
}
// Draw variant stubs
for (const ev of ["drawbegin", "drawend", "drawguibegin", "drawguiend", "drawpre", "drawpost", "drawresize"]) {
  (GMLObject.prototype as any)[ev] = noop;
}
// Mouse button stubs
for (const ev of [
  "mouseleftbutton", "mouserightbutton", "mousemiddlebutton", "mousenobutton",
  "mouseleftpressed", "mouserightpressed", "mousemiddlepressed",
  "mouseleftreleased", "mouserightreleased", "mousemiddlereleased",
  "globalleftbutton", "globalrightbutton", "globalmiddlebutton",
  "globalleftpressed", "globalrightpressed", "globalmiddlepressed",
  "globalleftreleased", "globalrightreleased", "globalmiddlereleased",
]) {
  (GMLObject.prototype as any)[ev] = noop;
}

// Sprites enum — populated per-instance by createGameRuntime
export const Sprites: Record<string, number> = {};

// ---- GMLRoom ----

class GMLRoom {
  constructor(private rt: GameRuntime) {}

  draw(): void {
    const rt = this.rt;
    const ctx = rt._gfx.ctx;
    ctx.fillStyle = "black";
    ctx.fillRect(0, 0, rt._gfx.canvas.width, rt._gfx.canvas.height);

    const oldRoom = rt.room;
    rt._isStepping = true;

    // Alarms
    for (const instance of rt.roomVariables) {
      if (instance.alarm.length !== 0) {
        for (let i = 0; i < 12; i++) {
          if (instance.alarm[i]) {
            instance.alarm[i]--;
            if (instance.alarm[i] === 0) {
              delete instance.alarm[i];
              const method = (instance as any)["alarm" + i];
              if (method !== noop) method.call(instance);
              if (oldRoom !== rt.room) break;
            }
          }
        }
      }
    }

    // Begin step
    let toStep: GMLObject[] = rt.roomVariables;
    while (toStep.length !== 0) {
      rt._pendingStep = [];
      for (const instance of toStep) {
        if ((instance as any).beginstep === noop) continue;
        instance.xprevious = instance.x;
        instance.yprevious = instance.y;
        instance.beginstep();
        if (oldRoom !== rt.room) break;
      }
      toStep = rt._pendingStep;
    }

    // Step
    toStep = rt.roomVariables;
    while (toStep.length !== 0) {
      rt._pendingStep = [];
      for (const instance of toStep) {
        if ((instance as any).step === noop) continue;
        instance.step();
        if (oldRoom !== rt.room) break;
      }
      toStep = rt._pendingStep;
    }

    // End step
    toStep = rt.roomVariables;
    while (toStep.length !== 0) {
      rt._pendingStep = [];
      for (const instance of toStep) {
        if ((instance as any).endstep === noop) continue;
        instance.endstep();
        if (oldRoom !== rt.room) break;
      }
      toStep = rt._pendingStep;
    }

    rt._isStepping = false;

    // Draw (sorted by depth, descending)
    const sorted = rt.roomVariables.slice().sort((a, b) => b.depth - a.depth);
    for (const instance of sorted) {
      if ((instance as any).draw === noop) continue;
      instance.draw();
      if (oldRoom !== rt.room) break;
    }

    // Draw GUI
    if (rt._drawguiUsed) {
      for (const instance of sorted) {
        if ((instance as any).drawgui === noop) continue;
        instance.drawgui();
        if (oldRoom !== rt.room) break;
      }
    }

    rt.resetFrameInput();
  }

  create(restart = false): void {
    const rt = this.rt;
    const idx = rt._roomInstances.indexOf(this);
    const data = rt._roomDatas[idx];
    if (!data) return;

    const instances: GMLObject[] = [];
    for (const obj of data.objs) {
      const clazz = rt.classes[obj.obj];
      if (!clazz) continue;
      const proto = clazz.prototype;
      if (!proto.persistent || rt._instanceNumber(clazz) === 0) {
        instances.push(rt._instanceCreate(obj.pos.x, obj.pos.y, clazz, true));
      }
    }
    for (const instance of instances) {
      instance.create();
    }
    // Room creation code runs after all instance creation events (GML semantics).
    const creationCode = rt.roomCreationCode[idx];
    if (creationCode) creationCode(rt);
  }

  destroy(restart = false): void {
    const rt = this.rt;
    for (const obj of rt.roomVariables.slice()) {
      if (restart || !obj.persistent) {
        rt._instanceDestroy(obj);
      }
    }
  }
}

// ---- GameRuntime ----

export class GameRuntime {
  // Sub-state containers
  _draw = new DrawState();
  _input = new InputState();
  _storage = new StorageState();
  _math = new MathState();
  _gfx = new GraphicsContext();
  _root?: RenderRoot;

  // Runtime state
  _drawHandle = 0;
  _currentRoom: GMLRoom | null = null;
  _isStepping = false;
  _pendingStep: GMLObject[] = [];
  _drawguiUsed = false;
  room = 0;
  room_speed = 60;
  fps_real = 1;
  roomVariables: GMLObject[] = [];
  classes: (typeof GMLObject)[] = [];
  _roomDatas: Room[] = [];
  roomCreationCode: ((_rt: GameRuntime) => void)[] = [];
  sprites: Sprite[] = [];
  textures: Texture[] = [];
  textureSheets: HTMLImageElement[] = [];
  fonts: Font[] = [];
  _classesEnum: Record<string, number> = {};
  _roomInstances: GMLRoom[] = [];
  _instancesByClass = new Map<Function, GMLObject[]>();

  // Global variable object
  global: Record<string, any> = { score: 0, health: 0, lives: 0, async_load: -1 };

  // Sprites enum (per-runtime)
  Sprites: Record<string, number> = {};

  // ---- API functions populated by factories ----
  // Explicitly declare internally-used factory functions for type safety.
  drawSprite!: (
    spriteIndex: number, imageIndex: number, x: number, y: number,
    opts?: { image_alpha?: number; image_xscale?: number; image_yscale?: number },
  ) => void;
  resetFrameInput!: () => void;
  activateMouse!: (ax: number, ay: number, override?: boolean) => void;
  setupInput!: () => void;
  mouse_x!: () => number;
  mouse_y!: () => number;

  constructor() {
    Object.assign(this, createDrawAPI(this));
    Object.assign(this, createInputAPI(this));
    Object.assign(this, createStorageAPI(this));
    Object.assign(this, createMathAPI(this));
    Object.assign(this, createGlobalAPI(this));
    Object.assign(this, createInstanceAPI(this));

    // Bind core methods for destructuring
    this.instance_create = this.instance_create.bind(this);
    this.instance_destroy = this.instance_destroy.bind(this);
    this.instance_exists = this.instance_exists.bind(this);
    this.instance_number = this.instance_number.bind(this);
    this.room_goto = this.room_goto.bind(this);
    this.room_goto_next = this.room_goto_next.bind(this);
    this.room_goto_previous = this.room_goto_previous.bind(this);
    this.room_restart = this.room_restart.bind(this);
    this.game_restart = this.game_restart.bind(this);
  }

  // ---- Per-runtime instance tracking ----

  _getInstances(clazz: Function): GMLObject[] {
    let arr = this._instancesByClass.get(clazz);
    if (!arr) {
      arr = [];
      this._instancesByClass.set(clazz, arr);
    }
    return arr;
  }

  // ---- Instance management (internal) ----

  _instanceCreate(x: number, y: number, clazz: typeof GMLObject, roomStart = false): GMLObject {
    const instance = new (clazz as any)();
    instance._rt = this;
    // Walk prototype chain and push to per-runtime instance tracking
    let c: any = instance.constructor;
    while (c !== __baseproto) {
      this._getInstances(c).push(instance);
      c = Object.getPrototypeOf(c);
    }
    instance.xstart = instance.x = x;
    instance.ystart = instance.y = y;
    this.roomVariables.push(instance);
    if (!roomStart) {
      instance.create();
    }
    if (!this._drawguiUsed && (instance as any).drawgui !== noop) {
      this._drawguiUsed = true;
    }
    if (this._isStepping) {
      this._pendingStep.push(instance);
    }
    return instance;
  }

  _instanceDestroy(instance: GMLObject): void {
    instance.destroy();
    let c: any = instance.constructor;
    while (c !== __baseproto) {
      const arr = this._getInstances(c);
      const idx = arr.indexOf(instance);
      if (idx > -1) arr.splice(idx, 1);
      c = Object.getPrototypeOf(c);
    }
    const idx = this.roomVariables.indexOf(instance);
    if (idx > -1) this.roomVariables.splice(idx, 1);
  }

  _instanceNumber(clazz: typeof GMLObject): number {
    return this._getInstances(clazz).reduce(
      (p: number, c: GMLObject) => p + (c.constructor === clazz ? 1 : 0), 0,
    );
  }

  // ---- Public instance API (called from emitted code) ----

  instance_create(x: number, y: number, classIndex: number): GMLObject {
    const clazz = this.classes[classIndex];
    return this._instanceCreate(x, y, clazz);
  }

  instance_destroy(instance?: GMLObject): void {
    if (instance === undefined) return;
    this._instanceDestroy(instance);
  }

  instance_exists(classIndex: number): boolean {
    const clazz = this.classes[classIndex];
    if (!clazz) return false;
    return this._getInstances(clazz).length > 0;
  }

  instance_number(classIndex: number): number {
    const clazz = this.classes[classIndex];
    if (!clazz) return 0;
    return this._instanceNumber(clazz);
  }

  // ---- Room navigation ----

  room_goto(id: number, restart = false): void {
    const oldRoom = this._currentRoom;
    if (oldRoom !== null) {
      for (const instance of this.roomVariables) {
        instance.roomend();
      }
      oldRoom.destroy(restart);
    }
    const newRoom = this._roomInstances[id];
    this._currentRoom = newRoom;
    this.room = id;
    this.room_speed = this._roomDatas[id]?.speed ?? 60;
    resizeCanvas(this._gfx, this._roomDatas[id]?.size.width ?? 800, this._roomDatas[id]?.size.height ?? 600);
    newRoom.create(restart);
    for (const instance of this.roomVariables) {
      instance.roomstart();
    }
    this.activateMouse(this.mouse_x(), this.mouse_y(), true);
  }

  room_goto_next(): void { this.room_goto(this.room + 1); }
  room_goto_previous(): void { this.room_goto(this.room - 1); }
  room_restart(): void { this.room_goto(this.room); }
  game_restart(): void { this.room_goto(0, true); }

  // ---- Game loop ----

  private _runFrame(): void {
    const start = performance.now();
    if (this._currentRoom) this._currentRoom.draw();
    const end = performance.now();
    const elapsed = end - start;
    const newfps = 1000 / Math.max(0.01, elapsed);
    this.fps_real = 0.9 * this.fps_real + 0.1 * newfps;
    this._drawHandle = scheduleFrame(
      () => this._runFrame(),
      Math.max(0, 1000 / this.room_speed - elapsed),
    );
  }

  // ---- Game startup ----

  async start(config: GameConfig): Promise<void> {
    this._roomDatas = config.rooms;
    this.sprites = config.sprites;
    this.textures = config.textures;
    this.fonts = config.fonts;
    this.classes = config.classes;
    this._classesEnum = config.Classes;
    this.roomCreationCode = config.roomCreationCode ?? [];

    // Populate Sprites enum from sprite data
    for (let i = 0; i < config.sprites.length; i++) {
      this.Sprites[config.sprites[i].name] = i;
    }
    // Also populate the module-level Sprites for backwards compatibility
    Object.assign(Sprites, this.Sprites);

    // Set up collision stubs (need class count)
    for (let i = 0; i < config.classes.length; i++) {
      (GMLObject.prototype as any)["collision" + i] = noop;
    }

    // Create room instances
    for (let i = 0; i < config.rooms.length; i++) {
      this._roomInstances.push(new GMLRoom(this));
    }

    // Init canvas and input
    if (this._root) {
      const canvas = createCanvas(this._root.doc);
      canvas.id = "reincarnate-canvas";
      this._root.container.appendChild(canvas);
      initCanvas(this._gfx, "reincarnate-canvas", this._root.doc);
    } else {
      initCanvas(this._gfx, "reincarnate-canvas");
    }
    this._gfx.canvas.tabIndex = 0;
    this._gfx.canvas.focus();
    this.setupInput();

    // Load texture sheets
    const sheetCount = Math.max(0, ...config.textures.map((t) => t.sheetId)) + 1;
    const sheetPromises: Promise<HTMLImageElement>[] = [];
    for (let i = 0; i < sheetCount; i++) {
      sheetPromises.push(loadImage(`assets/textures/texture_${i}.png`));
    }
    const sheets = await Promise.all(sheetPromises);
    this.textureSheets.push(...sheets);

    // Start
    this.room_goto(config.initialRoom);
    this._runFrame();
  }
}

// ---- Game config ----

export interface GameConfig {
  rooms: Room[];
  sprites: Sprite[];
  textures: Texture[];
  fonts: Font[];
  classes: (typeof GMLObject)[];
  Classes: Record<string, number>;
  initialRoom: number;
  roomCreationCode?: ((_rt: GameRuntime) => void)[];
}

// ---- Factory function ----

export function createGameRuntime(opts?: { root?: RenderRoot }): GameRuntime {
  const rt = new GameRuntime();
  if (opts?.root) rt._root = opts.root;
  return rt;
}

// ---- Timing stub (for compatibility) ----

export const timing = {
  tick() {
    // GameMaker uses its own setTimeout-based loop, not rAF
  },
};
