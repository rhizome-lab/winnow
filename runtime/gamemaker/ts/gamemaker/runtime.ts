/**
 * GML Runtime — game loop, GMLObject base class, room system.
 */

import { GraphicsContext, initCanvas, createCanvas, resizeCanvas, loadImage, scheduleFrame, saveItem, loadItem } from "./platform";
import type { RenderRoot } from "../../../shared/ts/render-root";
import { DrawState, createDrawAPI } from "./draw";
import { InputState, createInputAPI } from "./input";
import { gmlColorToCss } from "./color";
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
  // GML objects are open — instance variables are set dynamically in event handlers.
  [key: string]: any;
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

// ---- GMLRoom ----

class GMLRoom {
  constructor(private rt: GameRuntime)  { throw new Error("constructor: not yet implemented"); }

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
          const alarmVal = instance.alarm[i];
          if (alarmVal) {
            instance.alarm[i] = alarmVal - 1;
            if (alarmVal - 1 === 0) {
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
  roomCreationCode: (((_rt: GameRuntime) => void) | undefined)[] = [];
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
  getInstanceField!: (objId: number, field: string) => any;
  setInstanceField!: (objId: number, field: string, value: any) => void;
  setInstanceFieldIndex!: (objId: number, field: string, index: number, value: any) => void;
  getAllField!: (field: string) => any;
  setAllField!: (field: string, value: any) => void;
  withInstances!: (target: number, callback: (inst: GMLObject) => void) => void;
  drawSprite!: (
    spriteIndex: number, imageIndex: number, x: number, y: number,
    opts?: { image_alpha?: number; image_xscale?: number; image_yscale?: number },
  ) => void;
  resetFrameInput!: () => void;
  activateMouse!: (ax: number, ay: number, override?: boolean) => void;
  setupInput!: () => void;
  mouse_x!: () => number;
  mouse_y!: () => number;

  // Math API (from createMathAPI)
  random!: (max: number) => number;
  randomize!: () => void;
  random_range!: (min: number, max: number) => number;
  irandom!: (max: number) => number;
  irandom_range!: (min: number, max: number) => number;
  choose!: (...args: any[]) => any;

  // Draw API (from createDrawAPI)
  draw_set_color!: (color: number) => void;
  draw_set_font!: (font: number) => void;
  draw_set_halign!: (halign: number) => void;
  draw_set_valign!: (valign: number) => void;
  draw_set_alpha!: (alpha: number) => void;
  draw_get_alpha!: () => number;
  draw_sprite!: (spriteIndex: number, imageIndex: number, x: number, y: number) => void;
  draw_sprite_ext!: (spriteIndex: number, imageIndex: number, x: number, y: number, xscale: number, yscale: number, rot: number, color: number, alpha: number) => void;
  draw_self!: () => void;
  draw_rectangle!: (x1: number, y1: number, x2: number, y2: number, outline: boolean) => void;
  draw_text!: (x: number, y: number, text: string) => void;
  draw_text_color!: (x: number, y: number, text: string, c1: number, c2: number, c3: number, c4: number, alpha: number) => void;
  draw_text_transformed!: (x: number, y: number, text: string, xscale: number, yscale: number, angle: number) => void;
  draw_text_ext!: (x: number, y: number, text: string, sep: number, w: number) => void;
  draw_text_ext_color!: (x: number, y: number, text: string, sep: number, w: number, c1: number, c2: number, c3: number, c4: number, alpha: number) => void;
  draw_text_ext_transformed!: (x: number, y: number, text: string, sep: number, w: number, xscale: number, yscale: number, angle: number) => void;
  draw_text_transformed_color!: (x: number, y: number, text: string, xscale: number, yscale: number, angle: number, c1: number, c2: number, c3: number, c4: number, alpha: number) => void;
  draw_text_ext_transformed_color!: (x: number, y: number, text: string, sep: number, w: number, xscale: number, yscale: number, angle: number, c1: number, c2: number, c3: number, c4: number, alpha: number) => void;
  sprite_get_width!: (spriteIndex: number) => number;
  sprite_get_height!: (spriteIndex: number) => number;
  string_height_ext!: (text: string, sep: number, w: number) => number;

  // Input API (from createInputAPI)
  mouse_check_button!: (button: number) => boolean;
  mouse_check_button_pressed!: (button: number) => boolean;
  mouse_check_button_released!: (button: number) => boolean;
  mouse_wheel_up!: () => boolean;
  mouse_wheel_down!: () => boolean;

  // Storage API (from createStorageAPI)
  ini_open!: (path: string) => void;
  ini_close!: () => string;
  ini_write_real!: (section: string, key: string, value: number) => void;

  // Global API (from createGlobalAPI)
  variable_global_exists!: (key: string) => boolean;
  variable_global_get!: (key: string) => any;
  variable_global_set!: (key: string, value: any) => void;

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

  /** GML `other` — the "other" instance in collision/with events. Set by withInstances. */
  other: any = null;

  instance_create(x: number, y: number, classIndex: number): GMLObject {
    const clazz = this.classes[classIndex]!;
    return this._instanceCreate(x, y, clazz);
  }

  instance_create_depth(x: number, y: number, depth: number, classIndex: number): GMLObject {
    const clazz = this.classes[classIndex]!;
    const inst = this._instanceCreate(x, y, clazz);
    inst.depth = depth;
    return inst;
  }

  instance_create_layer(x: number, y: number, _layer: any, classIndex: number): GMLObject {
    const clazz = this.classes[classIndex]!;
    return this._instanceCreate(x, y, clazz);
  }

  instance_nearest(x: number, y: number, classIndex: number): GMLObject | null {
    const clazz = this.classes[classIndex];
    if (!clazz) return null;
    const instances = this._getInstances(clazz);
    if (instances.length === 0) return null;
    let nearest = instances[0]!;
    let minDist = Math.hypot(nearest.x - x, nearest.y - y);
    for (let i = 1; i < instances.length; i++) {
      const inst = instances[i]!;
      const d = Math.hypot(inst.x - x, inst.y - y);
      if (d < minDist) { minDist = d; nearest = inst; }
    }
    return nearest;
  }

  object_is_ancestor(classIndex: number, parentIndex: number): boolean {
    const clazz = this.classes[classIndex];
    const parent = this.classes[parentIndex];
    if (!clazz || !parent) return false;
    let proto = Object.getPrototypeOf(clazz);
    while (proto && proto !== __baseproto) {
      if (proto === parent) return true;
      proto = Object.getPrototypeOf(proto);
    }
    return false;
  }

  layer_get_id(_name: string): number { throw new Error("layer_get_id: not yet implemented"); }

  // ---- Sprite API extensions ----

  sprite_get_xoffset(spr: number): number { return this.sprites[spr]?.origin.x ?? 0; }
  sprite_get_yoffset(spr: number): number { return this.sprites[spr]?.origin.y ?? 0; }
  sprite_get_number(spr: number): number { return this.sprites[spr]?.textures.length ?? 1; }
  sprite_get_speed(_spr: number): number { throw new Error("sprite_get_speed: not yet implemented"); }
  sprite_set_offset(spr: number, xoff: number, yoff: number): void {
    const s = this.sprites[spr];
    if (s) { s.origin.x = xoff; s.origin.y = yoff; }
  }
  sprite_get_texture(spr: number, sub: number): number { return this.sprites[spr]?.textures[sub] ?? -1; }
  sprite_add_from_surface(_index: number, _srf: number, _x: number, _y: number, _w: number, _h: number, _removeback: boolean, _smooth: boolean): void { throw new Error("sprite_add_from_surface: not yet implemented"); }
  sprite_get_bbox_left(spr: number): number { return this.sprites[spr]?.bbox?.left ?? 0; }
  sprite_get_bbox_right(spr: number): number { return this.sprites[spr]?.bbox?.right ?? 0; }
  sprite_get_bbox_top(spr: number): number { return this.sprites[spr]?.bbox?.top ?? 0; }
  sprite_get_bbox_bottom(spr: number): number { return this.sprites[spr]?.bbox?.bottom ?? 0; }
  sprite_exists(spr: number): boolean { return spr >= 0 && spr < this.sprites.length; }

  // ---- Alarm API ----

  alarm_set(_alarm: number, _steps: number): void { throw new Error("alarm_set: not yet implemented"); }
  alarm_get(inst: any, alarm: number): number {
    return inst?.alarm?.[alarm] ?? -1;
  }

  // ---- Misc pure utility ----

  angle_difference(a: number, b: number): number {
    // +540 normalizes JS's sign-preserving modulo for negative values.
    return ((((a - b) % 360) + 540) % 360) - 180;
  }

  approach(value: number, target: number, amount: number): number {
    if (value < target) return Math.min(value + amount, target);
    return Math.max(value - amount, target);
  }

  asset_get_index(_name: string): number { throw new Error("asset_get_index: not yet implemented"); }
  asset_get_tags(_asset: number, _type: number = -1): string[] { throw new Error("asset_get_tags: not yet implemented"); }
  asset_has_tags(_asset: number, _tags: string | string[], _not?: boolean | number): boolean { throw new Error("asset_has_tags: not yet implemented"); }

  // ---- Array API (GMS2 style) ----

  array_create(size: number, defaultVal: any = 0): any[] { return new Array(size).fill(defaultVal); }
  array_copy(dest: any[], destIndex: number, src: any[], srcIndex: number, count: number): void {
    for (let i = 0; i < count; i++) dest[destIndex + i] = src[srcIndex + i];
  }
  array_equals(a: any[], b: any[]): boolean {
    if (a.length !== b.length) return false;
    for (let i = 0; i < a.length; i++) if (a[i] !== b[i]) return false;
    return true;
  }
  array_concat(...arrs: any[]): any[] { return ([] as any[]).concat(...arrs); }
  array_delete(arr: any[], index: number, count: number): void { arr.splice(index, count); }
  array_insert(arr: any[], index: number, ...vals: any[]): void { arr.splice(index, 0, ...vals); }
  array_get(arr: any[], index: number): any { return arr[index]; }
  array_set(arr: any[], index: number, val: any): void { arr[index] = val; }
  array_sort(arr: any[], ascending: boolean): void { arr.sort((a, b) => ascending ? a - b : b - a); }
  array_shuffle(arr: any[], _start?: number, _count?: number): void {
    for (let i = arr.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [arr[i], arr[j]] = [arr[j], arr[i]];
    }
  }
  array_shuffle_ext(arr: any[], _start?: number, _count?: number): void { this.array_shuffle(arr, _start, _count); }

  // ---- Type-check functions ----

  is_array(val: any): boolean { return Array.isArray(val); }
  is_string(val: any): boolean { return typeof val === 'string'; }
  is_real(val: any): boolean { return typeof val === 'number'; }
  is_undefined(val: any): boolean { return val === undefined; }
  is_nan(val: any): boolean { return typeof val === 'number' && isNaN(val); }
  is_infinity(val: any): boolean { return val === Infinity || val === -Infinity; }
  is_numeric(val: any): boolean { return !isNaN(Number(val)); }

  // ---- Surface API (unimplemented — requires WebGL offscreen rendering) ----

  surface_exists(_surf: number): boolean { throw new Error("surface_exists: surfaces require WebGL implementation"); }
  surface_create(_w: number, _h: number, _format: number = 0): number { throw new Error("surface_create: surfaces require WebGL implementation"); }
  surface_free(_surf: number): void { throw new Error("surface_free: surfaces require WebGL implementation"); }
  surface_set_target(_surf: number): void { throw new Error("surface_set_target: surfaces require WebGL implementation"); }
  surface_reset_target(): void { throw new Error("surface_reset_target: surfaces require WebGL implementation"); }
  draw_surface(_surf: number, _x: number, _y: number): void { throw new Error("draw_surface: surfaces require WebGL implementation"); }
  draw_surface_ext(_surf: number, _x: number, _y: number, _xs: number, _ys: number, _rot: number, _col: number, _alpha: number): void { throw new Error("draw_surface_ext: surfaces require WebGL implementation"); }
  draw_surface_part(_surf: number, _left: number, _top: number, _w: number, _h: number, _x: number, _y: number): void { throw new Error("draw_surface_part: surfaces require WebGL implementation"); }
  surface_get_width(_surf: number): number { throw new Error("surface_get_width: surfaces require WebGL implementation"); }
  surface_get_height(_surf: number): number { throw new Error("surface_get_height: surfaces require WebGL implementation"); }
  surface_getpixel(_surf: number, _x: number, _y: number): number { throw new Error("surface_getpixel: surfaces require WebGL implementation"); }

  // ---- Shader API (unimplemented — requires WebGL shaders) ----

  shader_is_compiled(_sh: number): boolean { throw new Error("shader_is_compiled: shaders require WebGL implementation"); }
  shader_set(_sh: number): void { throw new Error("shader_set: shaders require WebGL implementation"); }
  shader_reset(): void { throw new Error("shader_reset: shaders require WebGL implementation"); }
  shader_get_uniform(_sh: number, _name: string): number { throw new Error("shader_get_uniform: shaders require WebGL implementation"); }
  shader_set_uniform_f(_handle: number, ..._vals: number[]): void { throw new Error("shader_set_uniform_f: shaders require WebGL implementation"); }
  shader_set_uniform_i(_handle: number, ..._vals: number[]): void { throw new Error("shader_set_uniform_i: shaders require WebGL implementation"); }
  shader_get_sampler_index(_sh: number, _name: string): number { throw new Error("shader_get_sampler_index: shaders require WebGL implementation"); }
  shader_set_uniform_f_array(_handle: number, _arr: number[]): void { throw new Error("shader_set_uniform_f_array: shaders require WebGL implementation"); }

  // ---- GPU state API (unimplemented — requires WebGL) ----

  gpu_set_colorwriteenable(_r: boolean, _g: boolean, _b: boolean, _a: boolean): void { throw new Error("gpu_set_colorwriteenable: requires WebGL implementation"); }
  gpu_get_colorwriteenable(): [boolean, boolean, boolean, boolean] { throw new Error("gpu_get_colorwriteenable: requires WebGL implementation"); }
  gpu_set_fog(_enabled: boolean, _color: number, _start: number, _end: number): void { throw new Error("gpu_set_fog: requires WebGL implementation"); }
  gpu_set_blendmode(_mode: number): void { throw new Error("gpu_set_blendmode: requires WebGL implementation"); }
  gpu_set_blendmode_ext(_src: number, _dst: number): void { throw new Error("gpu_set_blendmode_ext: requires WebGL implementation"); }
  gpu_set_alphatestenable(_enabled: boolean): void { throw new Error("gpu_set_alphatestenable: requires WebGL implementation"); }
  gpu_set_alphatestref(_ref: number): void { throw new Error("gpu_set_alphatestref: requires WebGL implementation"); }
  gpu_set_ztestenable(_enabled: boolean): void { throw new Error("gpu_set_ztestenable: requires WebGL implementation"); }
  gpu_set_zwriteenable(_enabled: boolean): void { throw new Error("gpu_set_zwriteenable: requires WebGL implementation"); }
  gpu_set_cullmode(_mode: number): void { throw new Error("gpu_set_cullmode: requires WebGL implementation"); }

  // ---- Audio API (unimplemented — requires platform audio layer) ----
  //
  // Audio belongs in the platform layer (platform/audio.ts), not here.
  // These methods need to be wired to a platform audio implementation
  // that abstracts HTMLAudioElement or Web Audio API.
  // See: docs/architecture.md "Runtime Architecture" → Platform Interface.

  audio_play_sound(_sound: number, _priority: number, _loop: boolean, _gain?: number, _offset?: number, _pitch?: number): number { throw new Error("audio_play_sound: implement in platform/audio.ts"); }
  audio_play_sound_at(_sound: number, _x: number, _y: number, _z: number, _falloff: number, _min: number, _max: number, _priority: number, _loop: boolean): number { throw new Error("audio_play_sound_at: implement in platform/audio.ts"); }
  audio_is_playing(_handle: number): boolean { throw new Error("audio_is_playing: implement in platform/audio.ts"); }
  audio_stop_sound(_handle: number): void { throw new Error("audio_stop_sound: implement in platform/audio.ts"); }
  audio_stop_all(): void { throw new Error("audio_stop_all: implement in platform/audio.ts"); }
  audio_pause_sound(_handle: number): void { throw new Error("audio_pause_sound: implement in platform/audio.ts"); }
  audio_resume_sound(_handle: number): void { throw new Error("audio_resume_sound: implement in platform/audio.ts"); }
  audio_resume_all(): void { throw new Error("audio_resume_all: implement in platform/audio.ts"); }
  audio_exists(_sound: number): boolean { throw new Error("audio_exists: implement in platform/audio.ts"); }
  audio_get_name(_sound: number): string { throw new Error("audio_get_name: implement in platform/audio.ts"); }
  audio_sound_gain(_handle: number, _gain: number, _time: number): void { throw new Error("audio_sound_gain: implement in platform/audio.ts"); }
  audio_sound_get_gain(_handle: number): number { throw new Error("audio_sound_get_gain: implement in platform/audio.ts"); }
  audio_sound_pitch(_handle: number, _pitch: number): void { throw new Error("audio_sound_pitch: implement in platform/audio.ts"); }
  audio_master_gain(_gain: number): void { throw new Error("audio_master_gain: implement in platform/audio.ts"); }
  audio_group_load(_group: number): void { throw new Error("audio_group_load: implement in platform/audio.ts"); }
  audio_group_stop_all(_group: number): void { throw new Error("audio_group_stop_all: implement in platform/audio.ts"); }
  audio_group_set_gain(_group: number, _gain: number, _time: number): void { throw new Error("audio_group_set_gain: implement in platform/audio.ts"); }

  // ---- Particle API (unimplemented — requires particle simulation) ----

  part_system_create(): number { throw new Error("part_system_create: particle system not yet implemented"); }
  part_system_destroy(_sys: number): void { throw new Error("part_system_destroy: particle system not yet implemented"); }
  part_type_create(): number { throw new Error("part_type_create: particle system not yet implemented"); }
  part_type_destroy(_type: number): void { throw new Error("part_type_destroy: particle system not yet implemented"); }
  part_type_life(_type: number, _min: number, _max: number): void { throw new Error("part_type_life: particle system not yet implemented"); }
  part_type_direction(_type: number, _dir1: number, _dir2: number, _inc: number, _wiggle: number): void { throw new Error("part_type_direction: particle system not yet implemented"); }
  part_type_speed(_type: number, _min: number, _max: number, _inc: number, _wiggle: number): void { throw new Error("part_type_speed: particle system not yet implemented"); }
  part_type_sprite(_type: number, _spr: number, _anim: boolean, _stretch: boolean, _random: boolean): void { throw new Error("part_type_sprite: particle system not yet implemented"); }
  part_type_color1(_type: number, _col: number): void { throw new Error("part_type_color1: particle system not yet implemented"); }
  part_type_color2(_type: number, _col1: number, _col2: number): void { throw new Error("part_type_color2: particle system not yet implemented"); }
  part_type_color3(_type: number, _col1: number, _col2: number, _col3: number): void { throw new Error("part_type_color3: particle system not yet implemented"); }
  part_type_alpha1(_type: number, _alpha: number): void { throw new Error("part_type_alpha1: particle system not yet implemented"); }
  part_type_alpha2(_type: number, _a1: number, _a2: number): void { throw new Error("part_type_alpha2: particle system not yet implemented"); }
  part_type_alpha3(_type: number, _a1: number, _a2: number, _a3: number): void { throw new Error("part_type_alpha3: particle system not yet implemented"); }
  part_type_scale(_type: number, _xs: number, _ys: number): void { throw new Error("part_type_scale: particle system not yet implemented"); }
  part_type_size(_type: number, _minSize: number, _maxSize: number, _inc: number, _wiggle: number): void { throw new Error("part_type_size: particle system not yet implemented"); }
  part_type_orientation(_type: number, _min: number, _max: number, _inc: number, _wiggle: number, _relative: boolean): void { throw new Error("part_type_orientation: particle system not yet implemented"); }
  part_particles_create(_sys: number, _x: number, _y: number, _type: number, _count: number): void { throw new Error("part_particles_create: particle system not yet implemented"); }
  part_particles_create_color(_sys: number, _x: number, _y: number, _type: number, _color: number, _count: number): void { throw new Error("part_particles_create_color: particle system not yet implemented"); }
  part_emitter_create(_sys: number): number { throw new Error("part_emitter_create: particle system not yet implemented"); }
  part_emitter_destroy(_sys: number, _emit: number): void { throw new Error("part_emitter_destroy: particle system not yet implemented"); }
  part_emitter_region(_sys: number, _emit: number, _x1: number, _y1: number, _x2: number, _y2: number, _shape: number, _dist: number): void { throw new Error("part_emitter_region: particle system not yet implemented"); }
  part_emitter_burst(_sys: number, _emit: number, _type: number, _count: number): void { throw new Error("part_emitter_burst: particle system not yet implemented"); }

  // ---- DS (Data Structure) API — backed by JS Map / Array ----

  private _dsLists = new Map<number, any[]>();
  private _dsMaps = new Map<number, Map<any, any>>();
  private _dsGrids = new Map<number, { w: number; h: number; data: any[] }>();
  private _dsStacks = new Map<number, any[]>();
  private _dsQueues = new Map<number, any[]>();
  private _dsNextId = 1;
  private _cameras = new Map<number, { x: number; y: number; w: number; h: number }>();
  private _nextCamId = 0;
  private _varHashMap = new Map<string, number>();
  private _hashVarMap = new Map<number, string>();
  private _nextVarHash = 1;
  private _gamepadDeadzones = new Map<number, number>();
  private _psnTrophies = new Set<number>();

  ds_list_create(): number { const id = this._dsNextId++; this._dsLists.set(id, []); return id; }
  ds_list_destroy(list: number): void { this._dsLists.delete(list); }
  ds_list_add(list: number, ...vals: any[]): void { this._dsLists.get(list)?.push(...vals); }
  ds_list_size(list: number): number { return this._dsLists.get(list)?.length ?? 0; }
  ds_list_find_value(list: number, pos: number): any { return this._dsLists.get(list)?.[pos]; }
  ds_list_set(list: number, pos: number, val: any): void { const l = this._dsLists.get(list); if (l) l[pos] = val; }
  ds_list_delete(list: number, pos: number): void { this._dsLists.get(list)?.splice(pos, 1); }
  ds_list_clear(list: number): void { const l = this._dsLists.get(list); if (l) l.length = 0; }
  ds_list_exists(list: number): boolean { return this._dsLists.has(list); }
  ds_list_find_index(list: number, val: any): number { return this._dsLists.get(list)?.indexOf(val) ?? -1; }
  ds_list_sort(list: number, ascending: boolean): void { this._dsLists.get(list)?.sort((a, b) => ascending ? a - b : b - a); }

  ds_map_create(): number { const id = this._dsNextId++; this._dsMaps.set(id, new Map()); return id; }
  ds_map_destroy(map: number): void { this._dsMaps.delete(map); }
  ds_map_add(map: number, key: any, val: any): void { this._dsMaps.get(map)?.set(key, val); }
  ds_map_set(map: number, key: any, val: any): void { this._dsMaps.get(map)?.set(key, val); }
  ds_map_find_value(map: number, key: any): any { return this._dsMaps.get(map)?.get(key); }
  ds_map_exists(map: number, key: any): boolean { return this._dsMaps.get(map)?.has(key) ?? false; }
  ds_map_delete(map: number, key: any): void { this._dsMaps.get(map)?.delete(key); }
  ds_map_size(map: number): number { return this._dsMaps.get(map)?.size ?? 0; }
  ds_map_clear(map: number): void { this._dsMaps.get(map)?.clear(); }
  ds_map_is_map(map: number): boolean { return this._dsMaps.has(map); }
  ds_map_find_first(map: number): any { return this._dsMaps.get(map)?.keys().next().value; }
  ds_map_find_next(map: number, key: any): any {
    const m = this._dsMaps.get(map);
    if (!m) return undefined;
    let found = false;
    for (const k of m.keys()) { if (found) return k; if (k === key) found = true; }
    return undefined;
  }

  ds_grid_create(w: number, h: number): number {
    const id = this._dsNextId++;
    this._dsGrids.set(id, { w, h, data: new Array(w * h).fill(0) });
    return id;
  }
  ds_grid_destroy(grid: number): void { this._dsGrids.delete(grid); }
  ds_grid_get(grid: number, x: number, y: number): any {
    const g = this._dsGrids.get(grid);
    return g ? g.data[y * g.w + x] : undefined;
  }
  ds_grid_set(grid: number, x: number, y: number, val: any): void {
    const g = this._dsGrids.get(grid);
    if (g) g.data[y * g.w + x] = val;
  }
  ds_grid_width(grid: number): number { return this._dsGrids.get(grid)?.w ?? 0; }
  ds_grid_height(grid: number): number { return this._dsGrids.get(grid)?.h ?? 0; }
  ds_grid_clear(grid: number, val: any): void { const g = this._dsGrids.get(grid); if (g) g.data.fill(val); }
  ds_grid_add(grid: number, x: number, y: number, val: any): void {
    const g = this._dsGrids.get(grid);
    if (g) g.data[y * g.w + x] = (g.data[y * g.w + x] ?? 0) + val;
  }

  ds_stack_create(): number { const id = this._dsNextId++; this._dsStacks.set(id, []); return id; }
  ds_stack_destroy(stack: number): void { this._dsStacks.delete(stack); }
  ds_stack_push(stack: number, val: any): void { this._dsStacks.get(stack)?.push(val); }
  ds_stack_pop(stack: number): any { return this._dsStacks.get(stack)?.pop(); }
  ds_stack_top(stack: number): any { const s = this._dsStacks.get(stack); return s?.[s.length - 1]; }
  ds_stack_size(stack: number): number { return this._dsStacks.get(stack)?.length ?? 0; }
  ds_stack_empty(stack: number): boolean { return (this._dsStacks.get(stack)?.length ?? 0) === 0; }

  ds_queue_create(): number { const id = this._dsNextId++; this._dsQueues.set(id, []); return id; }
  ds_queue_destroy(queue: number): void { this._dsQueues.delete(queue); }
  ds_queue_enqueue(queue: number, val: any): void { this._dsQueues.get(queue)?.push(val); }
  ds_queue_dequeue(queue: number): any { return this._dsQueues.get(queue)?.shift(); }
  ds_queue_head(queue: number): any { return this._dsQueues.get(queue)?.[0]; }
  ds_queue_size(queue: number): number { return this._dsQueues.get(queue)?.length ?? 0; }
  ds_queue_empty(queue: number): boolean { return (this._dsQueues.get(queue)?.length ?? 0) === 0; }

  // ---- GML static-variable housekeeping (no-ops in TypeScript) ----
  // GMS2.3+ emits __SetStatic__() / __CopyStatic__(n) calls to manage per-function
  // static-variable scopes.  In our translation we discard static semantics, so
  // these are safe no-ops.
  __SetStatic__(): void { /* no-op: static-variable scoping not needed in TypeScript */ }
  __CopyStatic__(_n: number): void { /* no-op */ }

  // ---- Method binding ----
  // GML method(instance, func) binds `instance` as the self context for func.
  // Our functions take (_rt, self, ...args), so we partially-apply both.
  method(_instance: any, func: (...args: any[]) => any): (...args: any[]) => any {
    const rt = this;
    return (...args: any[]) => func(rt, _instance, ...args);
  }

  // ---- Variable struct helpers ----
  variable_struct_exists(struct: any, name: string): boolean {
    return struct != null && Object.prototype.hasOwnProperty.call(struct, name);
  }
  variable_struct_set(struct: any, name: string, value: any): void {
    if (struct != null) struct[name] = value;
  }
  variable_struct_get(struct: any, name: string): any {
    return struct?.[name];
  }
  variable_struct_remove(struct: any, name: string): void {
    if (struct != null) delete struct[name];
  }
  variable_struct_names_count(struct: any): number {
    return struct != null ? Object.keys(struct).length : 0;
  }
  variable_struct_get_names(struct: any): string[] {
    return struct != null ? Object.keys(struct) : [];
  }
  is_instanceof(value: any, constructor: any): boolean {
    if (value == null || constructor == null) return false;
    const proto = Object.getPrototypeOf(value);
    return proto !== null && (proto.constructor === constructor || value instanceof constructor);
  }

  // ---- Point / geometry helpers ----
  point_in_rectangle(px: number, py: number, x1: number, y1: number, x2: number, y2: number): boolean {
    return px >= x1 && px <= x2 && py >= y1 && py <= y2;
  }
  distance_to_object(_classIndex: number): number {
    throw new Error("distance_to_object: implement using instance spatial data");
  }
  collision_line(_x1: number, _y1: number, _x2: number, _y2: number, _classIndex: number, _prec: boolean, _notme: boolean): any {
    throw new Error("collision_line: requires collision system implementation");
  }
  collision_rectangle(_x1: number, _y1: number, _x2: number, _y2: number, _classIndex: number, _prec: boolean, _notme: boolean): any {
    throw new Error("collision_rectangle: requires collision system implementation");
  }
  place_meeting(_x: number, _y: number, _classIndex: number): boolean {
    throw new Error("place_meeting: requires collision system implementation");
  }
  instance_place(_x: number, _y: number, _classIndex: number): any {
    throw new Error("instance_place: requires collision system implementation");
  }
  instance_find(classIndex: number, n: number): any {
    const clazz = this.classes[classIndex];
    if (!clazz) return -4; // noone
    const instances = this._getInstances(clazz);
    return instances[n] ?? -4;
  }

  // ---- Object metadata ----
  object_get_name(classIndex: number): string {
    const clazz = this.classes[classIndex];
    return clazz?.name ?? `object_${classIndex}`;
  }
  object_exists(classIndex: number): boolean {
    return classIndex >= 0 && classIndex < this.classes.length && this.classes[classIndex] != null;
  }

  // ---- Color helpers ----
  make_color_hsv(h: number, s: number, v: number): number {
    // h: 0-255, s: 0-255, v: 0-255  →  BGR packed int (GML color format)
    const hf = (h / 255) * 360;
    const sf = s / 255;
    const vf = v / 255;
    const c = vf * sf;
    const x = c * (1 - Math.abs(((hf / 60) % 2) - 1));
    const m = vf - c;
    let r = 0, g = 0, b = 0;
    if (hf < 60)       { r = c; g = x; b = 0; }
    else if (hf < 120) { r = x; g = c; b = 0; }
    else if (hf < 180) { r = 0; g = c; b = x; }
    else if (hf < 240) { r = 0; g = x; b = c; }
    else if (hf < 300) { r = x; g = 0; b = c; }
    else               { r = c; g = 0; b = x; }
    const ri = Math.round((r + m) * 255);
    const gi = Math.round((g + m) * 255);
    const bi = Math.round((b + m) * 255);
    return (bi << 16) | (gi << 8) | ri; // GML: BGR packed
  }

  // ---- String helpers ----
  string_width(_str: string): number {
    // Approximate — requires canvas font metrics for accuracy
    return (_str?.length ?? 0) * 8;
  }
  string_height(_str: string): number {
    // Approximate — requires canvas font metrics for accuracy
    return 16;
  }
  string_format(n: number, tot: number, dec: number): string {
    const s = n.toFixed(dec);
    return s.length < tot ? s.padStart(tot) : s;
  }
  font_get_name(font: number): string { return this.fonts[font]?.name ?? ""; }
  font_exists(font: number): boolean { return font >= 0 && font < this.fonts.length && this.fonts[font] != null; }

  // ---- Camera API ----
  camera_get_view_x(cam: number): number { return this._cameras.get(cam)?.x ?? 0; }
  camera_get_view_y(cam: number): number { return this._cameras.get(cam)?.y ?? 0; }
  camera_set_view_pos(cam: number, x: number, y: number): void { const c = this._cameras.get(cam); if (c) { c.x = x; c.y = y; } }
  camera_get_view_width(cam: number): number { return this._cameras.get(cam)?.w ?? 0; }
  camera_get_view_height(cam: number): number { return this._cameras.get(cam)?.h ?? 0; }

  // ---- Layer API ----
  layer_background_get_id(_layer: any): number { throw new Error("layer_background_get_id: not yet implemented"); }
  layer_background_get_sprite(_id: number): number { throw new Error("layer_background_get_sprite: not yet implemented"); }
  layer_background_set_sprite(_id: number, _spr: number): void { throw new Error("layer_background_set_sprite: not yet implemented"); }

  // ---- Keyboard (delegated to createInputAPI) ----
  keyboard_check!: (key: number) => boolean;
  keyboard_check_pressed!: (key: number) => boolean;
  keyboard_check_released!: (key: number) => boolean;

  // ---- Window ----
  window_get_width(): number { return window.innerWidth; }
  window_get_height(): number { return window.innerHeight; }
  window_get_fullscreen(): boolean { return !!document.fullscreenElement; }

  // ---- Draw extras ----
  draw_clear_alpha(colour: number, alpha: number): void {
    const r = colour & 0xff;
    const g = (colour >> 8) & 0xff;
    const b = (colour >> 16) & 0xff;
    const ctx = this._gfx.ctx;
    ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
    ctx.fillStyle = `rgba(${r},${g},${b},${alpha})`;
    ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
  }
  draw_sprite_stretched_ext(spr: number, sub: number, x: number, y: number, w: number, h: number, _col: number, alpha: number): void {
    const ctx = this._gfx.ctx;
    const prev = this._draw.alpha;
    if (alpha !== prev) ctx.globalAlpha = this._draw.alpha = alpha;
    this.draw_sprite_stretched(spr, sub, x, y, w, h);
    if (alpha !== prev) ctx.globalAlpha = this._draw.alpha = prev;
  }
  draw_circle(x: number, y: number, r: number, outline: boolean): void {
    const ctx = this._gfx.ctx;
    const css = gmlColorToCss(this._draw.config.color);
    ctx.beginPath();
    ctx.arc(x, y, r, 0, Math.PI * 2);
    if (outline) { ctx.strokeStyle = css; ctx.stroke(); }
    else { ctx.fillStyle = css; ctx.fill(); }
  }
  draw_vertex(_x: number, _y: number): void { throw new Error("draw_vertex: not yet implemented"); }
  draw_primitive_begin(_kind: number): void { throw new Error("draw_primitive_begin: not yet implemented"); }
  draw_primitive_end(): void { throw new Error("draw_primitive_end: not yet implemented"); }
  draw_rectangle_color(x1: number, y1: number, x2: number, y2: number, c1: number, _c2: number, _c3: number, _c4: number, outline: boolean): void {
    const ctx = this._gfx.ctx;
    if (outline) { ctx.strokeStyle = gmlColorToCss(c1); ctx.strokeRect(x1, y1, x2 - x1 + 1, y2 - y1 + 1); }
    else { ctx.fillStyle = gmlColorToCss(c1); ctx.fillRect(x1, y1, x2 - x1 + 1, y2 - y1 + 1); }
  }
  draw_sprite_tiled_ext(spr: number, sub: number, x: number, y: number, xscale: number, yscale: number, _col: number, alpha: number): void {
    const ctx = this._gfx.ctx;
    ctx.save();
    if (alpha !== this._draw.alpha) ctx.globalAlpha = alpha;
    ctx.scale(xscale, yscale);
    this.draw_sprite_tiled(spr, sub, x / xscale, y / yscale);
    ctx.restore();
  }
  draw_ellipse_color(x1: number, y1: number, x2: number, y2: number, col1: number, col2: number, outline: boolean): void {
    const ctx = this._gfx.ctx;
    const cx = (x1 + x2) / 2, cy = (y1 + y2) / 2;
    const rx = Math.abs(x2 - x1) / 2, ry = Math.abs(y2 - y1) / 2;
    ctx.beginPath();
    ctx.ellipse(cx, cy, rx, ry, 0, 0, Math.PI * 2);
    if (outline) { ctx.strokeStyle = gmlColorToCss(col1); ctx.stroke(); }
    else {
      const g = ctx.createRadialGradient(cx, cy, 0, cx, cy, Math.max(rx, ry));
      g.addColorStop(0, gmlColorToCss(col1)); g.addColorStop(1, gmlColorToCss(col2));
      ctx.fillStyle = g; ctx.fill();
    }
  }
  draw_sprite_part(spr: number, sub: number, left: number, top: number, w: number, h: number, x: number, y: number): void {
    const sprite = this.sprites[spr];
    if (!sprite) return;
    const texIdx = sprite.textures[sub] ?? sprite.textures[0];
    if (texIdx === undefined) return;
    const tex = this.textures[texIdx];
    if (!tex) return;
    const sheet = this.textureSheets[tex.sheetId];
    if (!sheet) return;
    this._gfx.ctx.drawImage(sheet, tex.src.x + left, tex.src.y + top, w, h, x, y, w, h);
  }
  draw_get_color(): number { return this._draw.config.color; }
  draw_get_halign(): number { return this._draw.config.halign; }
  draw_get_valign(): number { return this._draw.config.valign; }

  // ---- Other-instance field setter (used by setOther rewrite) ----
  setOtherField(other: any, field: string, value: any): void {
    if (other != null) other[field] = value;
  }

  // ---- Object name registry (set by generated code) ----
  getObjectName(classIndex: number): string {
    return this.object_get_name(classIndex);
  }

  // ---- Particle extra ----
  part_system_automatic_draw(_syst: number, _on: boolean): void { /* no-op: particle system not implemented */ }
  part_type_exists(_part: number): boolean { return false; }
  part_type_shape(_type: number, _shape: number): void { throw new Error("part_type_shape: particle system not yet implemented"); }
  part_type_death(_type: number, _kind: number, _step: number): void { throw new Error("part_type_death: particle system not yet implemented"); }
  part_type_color_hsv(_type: number, _hmin: number, _hmax: number, _smin: number, _smax: number, _vmin: number, _vmax: number): void { throw new Error("part_type_color_hsv: particle system not yet implemented"); }
  part_type_clear(_type: number): void { throw new Error("part_type_clear: particle system not yet implemented"); }
  part_type_size_x(_type: number, _xmin: number, _xmax: number, _xinc: number, _xwiggle: number): void { throw new Error("part_type_size_x: particle system not yet implemented"); }
  part_type_size_y(_type: number, _ymin: number, _ymax: number, _yinc: number, _ywiggle: number): void { throw new Error("part_type_size_y: particle system not yet implemented"); }

  // ---- Instance activation/deactivation ----
  instance_activate_object(_classIndex: number): void { throw new Error("instance_activate_object: not yet implemented"); }
  instance_deactivate_object(_classIndex: number, _notme?: boolean): void { throw new Error("instance_deactivate_object: not yet implemented"); }
  instance_deactivate_layer(_layer: any): void { throw new Error("instance_deactivate_layer: not yet implemented"); }
  instance_deactivate_region(_x1: number, _y1: number, _x2: number, _y2: number, _inside: boolean, _notme?: boolean): void { throw new Error("instance_deactivate_region: not yet implemented"); }

  // ---- Variable instance helpers ----
  variable_instance_get(inst: any, name: string): any { return inst?.[name]; }
  variable_instance_exists(inst: any, name: string): boolean { return inst != null && name in Object(inst); }

  // ---- Surface API (stubs — requires WebGL offscreen rendering) ----
  surface_resize(_srf: number, _w: number, _h: number): void { throw new Error("surface_resize: not yet implemented"); }
  surface_get_target(): number { throw new Error("surface_get_target: not yet implemented"); }

  // ---- Misc ----
  show_error(str: string, _abort: boolean): void { console.error("GML show_error:", str); }
  event_user(_n: number): void { throw new Error("event_user: not yet implemented"); }
  sprite_create_from_surface(_srf: number, _x: number, _y: number, _w: number, _h: number, _removeback: boolean, _smooth: boolean, _xorig: number, _yorig: number): number { throw new Error("sprite_create_from_surface: not yet implemented"); }
  vertex_normal(_vbuf: number, _x: number, _y: number, _z: number): void { throw new Error("vertex_normal: not yet implemented"); }

  // ---- ds_exists ----
  ds_exists(id: number, type: number): boolean {
    switch (type) {
      case 1: return this._dsLists.has(id);
      case 2: return this._dsMaps.has(id);
      case 3: return this._dsGrids.has(id);
      case 5: return this._dsStacks.has(id);
      case 6: return this._dsQueues.has(id);
      default: return false;
    }
  }

  // ---- Buffer API (stubs — TODO: implement in platform layer) ----
  buffer_create(_size: number, _type: number, _alignment: number): number {
    throw new Error("buffer_create: implement in platform layer");
  }
  buffer_write(_buffer: number, _type: number, _value: any): void {
    throw new Error("buffer_write: implement in platform layer");
  }
  buffer_read(_buffer: number, _type: number): any {
    throw new Error("buffer_read: implement in platform layer");
  }
  buffer_delete(_buffer: number): void {
    throw new Error("buffer_delete: implement in platform layer");
  }

  // ---- File API (stubs — TODO: implement in platform layer) ----
  file_text_write_string(_file: number, _str: string): void {
    throw new Error("file_text_write_string: implement in platform layer");
  }
  file_text_close(_file: number): void {
    throw new Error("file_text_close: implement in platform layer");
  }
  file_exists(_path: string): boolean {
    throw new Error("file_exists: implement in platform layer");
  }

  // ---- Steam API (platform-provided or no-op) ----

  steam_current_game_language(): string { throw new Error("steam_current_game_language: not yet implemented"); }
  steam_inventory_result_destroy(_result: number): void { throw new Error("steam_inventory_result_destroy: not yet implemented"); }
  steam_ugc_get_item_install_info(_id: number, _arr: any): boolean { throw new Error("steam_ugc_get_item_install_info: not yet implemented"); }
  steam_ugc_get_subscribed_items(_arr: any): number { throw new Error("steam_ugc_get_subscribed_items: not yet implemented"); }
  steam_lobby_get_lobby_id(): number { throw new Error("steam_lobby_get_lobby_id: not yet implemented"); }
  steam_lobby_join_id(_id: number): void { throw new Error("steam_lobby_join_id: not yet implemented"); }
  steam_lobby_set_data(_key: string, _val: string, _lobby?: number): void { throw new Error("steam_lobby_set_data: not yet implemented"); }
  steam_lobby_get_data(_key: string, _lobby?: number): string { throw new Error("steam_lobby_get_data: not yet implemented"); }
  steam_activate_overlay_store(_app: number): void { throw new Error("steam_activate_overlay_store: not yet implemented"); }
  steam_input_get_digital_action_handle(_name: string): number { throw new Error("steam_input_get_digital_action_handle: not yet implemented"); }
  steam_is_cloud_enabled_for_account(): boolean { throw new Error("steam_is_cloud_enabled_for_account: not yet implemented"); }
  steam_inventory_result_get_items(_result: number, _arr?: any[]): any[] { throw new Error("steam_inventory_result_get_items: not yet implemented"); }
  steam_lobby_get_member_id(_index: number, _lobby?: number): number { throw new Error("steam_lobby_get_member_id: not yet implemented"); }
  steam_input_get_action_set_handle(_name: string): number { throw new Error("steam_input_get_action_set_handle: not yet implemented"); }
  steam_get_stat_float(_name: string): number { throw new Error("steam_get_stat_float: not yet implemented"); }
  steam_get_global_stat_int(_name: string): number { throw new Error("steam_get_global_stat_int: not yet implemented"); }
  steam_get_user_account_id(): number { throw new Error("steam_get_user_account_id: not yet implemented"); }
  steam_image_get_rgba(_image: number, _buf: number, _size: number): boolean { throw new Error("steam_image_get_rgba: not yet implemented"); }
  steam_input_enable_device_callbacks(): void { throw new Error("steam_input_enable_device_callbacks: not yet implemented"); }
  steam_lobby_get_chat_message_text(_index: number, _lobby?: number): string { throw new Error("steam_lobby_get_chat_message_text: not yet implemented"); }
  steam_lobby_get_owner_id(_lobby?: number): number { throw new Error("steam_lobby_get_owner_id: not yet implemented"); }
  steam_request_friend_rich_presence(_steamid: number): void { throw new Error("steam_request_friend_rich_presence: not yet implemented"); }
  steam_ugc_get_item_update_info(_handle: number, _arr: any): boolean { throw new Error("steam_ugc_get_item_update_info: not yet implemented"); }
  steam_ugc_submit_item_update(_handle: number, _note: string): void { throw new Error("steam_ugc_submit_item_update: not yet implemented"); }

  // ---- More collision ----
  collision_point(_x: number, _y: number, _classIndex: number, _prec: boolean, _notme: boolean): any { throw new Error("collision_point: requires collision system implementation"); }
  collision_circle(_x: number, _y: number, _r: number, _classIndex: number, _prec: boolean, _notme: boolean): any { throw new Error("collision_circle: requires collision system implementation"); }
  collision_ellipse(_x1: number, _y1: number, _x2: number, _y2: number, _classIndex: number, _prec: boolean, _notme: boolean): any { throw new Error("collision_ellipse: requires collision system implementation"); }
  collision_line_list(_x1: number, _y1: number, _x2: number, _y2: number, _classIndex: number, _prec: boolean, _notme: boolean, _list: number, _ordered: number | boolean = 0): number { throw new Error("collision_line_list: requires collision system implementation"); }
  collision_rectangle_list(_x1: number, _y1: number, _x2: number, _y2: number, _classIndex: number, _prec: boolean, _notme: boolean, _list: number, _ordered: boolean = false): number { throw new Error("collision_rectangle_list: requires collision system implementation"); }
  distance_to_point(_x: number, _y: number): number {
    throw new Error("distance_to_point: implement using instance spatial data");
  }

  // ---- More draw ----
  draw_line(x1: number, y1: number, x2: number, y2: number): void {
    const ctx = this._gfx.ctx;
    ctx.strokeStyle = gmlColorToCss(this._draw.config.color);
    ctx.beginPath(); ctx.moveTo(x1, y1); ctx.lineTo(x2, y2); ctx.stroke();
  }
  draw_line_color(x1: number, y1: number, x2: number, y2: number, col1: number, col2: number): void {
    const ctx = this._gfx.ctx;
    const g = ctx.createLinearGradient(x1, y1, x2, y2);
    g.addColorStop(0, gmlColorToCss(col1)); g.addColorStop(1, gmlColorToCss(col2));
    ctx.strokeStyle = g;
    ctx.beginPath(); ctx.moveTo(x1, y1); ctx.lineTo(x2, y2); ctx.stroke();
  }
  draw_point_color(x: number, y: number, col: number): void {
    const ctx = this._gfx.ctx;
    ctx.fillStyle = gmlColorToCss(col);
    ctx.fillRect(x, y, 1, 1);
  }
  draw_triangle(x1: number, y1: number, x2: number, y2: number, x3: number, y3: number, outline: boolean): void {
    const ctx = this._gfx.ctx;
    const css = gmlColorToCss(this._draw.config.color);
    ctx.beginPath(); ctx.moveTo(x1, y1); ctx.lineTo(x2, y2); ctx.lineTo(x3, y3); ctx.closePath();
    if (outline) { ctx.strokeStyle = css; ctx.stroke(); }
    else { ctx.fillStyle = css; ctx.fill(); }
  }
  draw_triangle_color(x1: number, y1: number, x2: number, y2: number, x3: number, y3: number, c1: number, _c2: number, _c3: number, outline: boolean): void {
    const ctx = this._gfx.ctx;
    ctx.beginPath(); ctx.moveTo(x1, y1); ctx.lineTo(x2, y2); ctx.lineTo(x3, y3); ctx.closePath();
    if (outline) { ctx.strokeStyle = gmlColorToCss(c1); ctx.stroke(); }
    else { ctx.fillStyle = gmlColorToCss(c1); ctx.fill(); }
  }
  draw_primitive_begin_texture(_kind: number, _tex: number): void { throw new Error("draw_primitive_begin_texture: not yet implemented"); }
  draw_set_circle_precision(_n: number): void { /* canvas uses native arcs */ }
  draw_sprite_part_ext(spr: number, sub: number, left: number, top: number, w: number, h: number, x: number, y: number, xscale: number, yscale: number, _col: number, alpha: number): void {
    const ctx = this._gfx.ctx;
    ctx.save();
    ctx.translate(x, y);
    ctx.scale(xscale, yscale);
    if (alpha !== this._draw.alpha) ctx.globalAlpha = this._draw.alpha = alpha;
    const sprite = this.sprites[spr];
    if (sprite) {
      const texIdx = sprite.textures[sub] ?? sprite.textures[0];
      if (texIdx !== undefined) {
        const tex = this.textures[texIdx];
        if (tex) {
          const sheet = this.textureSheets[tex.sheetId];
          if (sheet) ctx.drawImage(sheet, tex.src.x + left, tex.src.y + top, w, h, 0, 0, w, h);
        }
      }
    }
    ctx.restore();
  }
  draw_sprite_pos(_spr: number, _sub: number, _x1: number, _y1: number, _x2: number, _y2: number, _x3: number, _y3: number, _x4: number, _y4: number, _alpha: number): void { throw new Error("draw_sprite_pos: not yet implemented"); }
  draw_sprite_tiled(spr: number, sub: number, x: number, y: number): void {
    const sprite = this.sprites[spr];
    if (!sprite) return;
    const texIdx = sprite.textures[sub] ?? sprite.textures[0];
    if (texIdx === undefined) return;
    const tex = this.textures[texIdx];
    if (!tex) return;
    const sheet = this.textureSheets[tex.sheetId];
    if (!sheet) return;
    const ctx = this._gfx.ctx;
    const sw = tex.dest.w, sh = tex.dest.h;
    const cw = ctx.canvas.width, ch = ctx.canvas.height;
    const ox = ((x % sw) + sw) % sw, oy = ((y % sh) + sh) % sh;
    for (let ty = -oy; ty < ch; ty += sh) {
      for (let tx = -ox; tx < cw; tx += sw) {
        ctx.drawImage(sheet, tex.src.x, tex.src.y, tex.src.w, tex.src.h, tx, ty, sw, sh);
      }
    }
  }
  draw_surface_stretched_ext(_surf: number, _x: number, _y: number, _w: number, _h: number, _col: number, _alpha: number): void {
    throw new Error("draw_surface_stretched_ext: requires WebGL implementation");
  }
  draw_get_font(): number { return this._draw.config.font; }

  // ---- DS extras ----
  ds_grid_read(grid: number, str: string): void {
    const g = this._dsGrids.get(grid);
    if (!g) return;
    try {
      const obj = JSON.parse(str) as { w: number; h: number; data: any[] };
      if (obj.w === g.w && obj.h === g.h) {
        for (let i = 0; i < g.data.length; i++) g.data[i] = obj.data[i];
      }
    } catch { /* ignore */ }
  }
  ds_grid_value_exists(grid: number, x1: number, y1: number, x2: number, y2: number, val: any): boolean {
    const g = this._dsGrids.get(grid);
    if (!g) return false;
    for (let y = y1; y <= y2; y++) for (let x = x1; x <= x2; x++) if (g.data[y * g.w + x] === val) return true;
    return false;
  }
  ds_grid_value_x(grid: number, x1: number, y1: number, x2: number, y2: number, val: any): number {
    const g = this._dsGrids.get(grid);
    if (!g) return -1;
    for (let y = y1; y <= y2; y++) for (let x = x1; x <= x2; x++) if (g.data[y * g.w + x] === val) return x;
    return -1;
  }
  ds_grid_value_y(grid: number, x1: number, y1: number, x2: number, y2: number, val: any): number {
    const g = this._dsGrids.get(grid);
    if (!g) return -1;
    for (let y = y1; y <= y2; y++) for (let x = x1; x <= x2; x++) if (g.data[y * g.w + x] === val) return y;
    return -1;
  }
  ds_list_copy(dst: number, src: number): void {
    const s = this._dsLists.get(src);
    if (s) this._dsLists.set(dst, [...s]);
  }
  ds_list_empty(list: number): boolean { return (this._dsLists.get(list)?.length ?? 0) === 0; }
  ds_list_replace(list: number, index: number, val: any): void {
    const l = this._dsLists.get(list);
    if (l) l[index] = val;
  }
  ds_map_read(map: number, str: string): void {
    const m = this._dsMaps.get(map);
    if (!m) return;
    try { const obj = JSON.parse(str); m.clear(); for (const [k, v] of Object.entries(obj)) m.set(k, v); } catch { /* ignore */ }
  }
  ds_map_secure_load(_filename: string): number { return this.ds_map_create(); }
  ds_priority_clear(id: number): void { this._dsMaps.get(id)?.clear(); }
  ds_priority_delete_max(id: number): void {
    const m = this._dsMaps.get(id);
    if (!m || m.size === 0) return;
    let bestKey: any, bestPri = -Infinity;
    for (const [k, v] of m) { if (v > bestPri) { bestPri = v; bestKey = k; } }
    if (bestKey !== undefined) m.delete(bestKey);
  }

  // ---- File extras ----
  file_text_open_read(_path: string): number { throw new Error("file_text_open_read: not yet implemented"); }
  file_text_read_string(_file: number): string { throw new Error("file_text_read_string: not yet implemented"); }
  file_delete(_path: string): void { throw new Error("file_delete: not yet implemented"); }
  file_find_first(_mask: string, _attr: number): string { throw new Error("file_find_first: not yet implemented"); }
  file_find_next(): string { throw new Error("file_find_next: not yet implemented"); }

  // ---- Directory ----
  directory_create(_path: string): void { throw new Error("directory_create: not yet implemented"); }
  directory_exists(_path: string): boolean { throw new Error("directory_exists: not yet implemented"); }

  // ---- Audio extras ----
  audio_sound_length(_sound: number): number { throw new Error("audio_sound_length: not yet implemented"); }
  audio_sound_get_pitch(_handle: number): number { throw new Error("audio_sound_get_pitch: implement in platform/audio.ts"); }

  // ---- Buffer extras ----
  buffer_base64_decode(_str: string): number { throw new Error("buffer_base64_decode: implement in platform layer"); }
  buffer_load(_filename: string, _buf: number = -1, _offset: number = 0, _size: number = 0): number { throw new Error("buffer_load: implement in platform layer"); }
  buffer_load_async(_path: string, _buf: number, _offset: number, _size: number): number { throw new Error("buffer_load_async: implement in platform layer"); }
  buffer_save_async(_buf: number, _path: string, _offset: number, _size: number): number { throw new Error("buffer_save_async: implement in platform layer"); }
  buffer_set_surface(_buf: number, _surf: number, _offset: number): void {
    throw new Error("buffer_set_surface: requires WebGL implementation");
  }
  buffer_async_group_begin(_groupname: string): void { throw new Error("buffer_async_group_begin: not yet implemented"); }
  buffer_async_group_end(): number { throw new Error("buffer_async_group_end: not yet implemented"); }

  // ---- Display extras ----
  display_get_gui_width(): number { return window.innerWidth; }
  display_get_gui_height(): number { return window.innerHeight; }
  display_reset(_antialias: number, _vsync: boolean): void { /* no-op */ }

  // ---- Font extras ----
  font_add_sprite_ext(_spr: number, _str: string, _prop: boolean, _sep: number): number { throw new Error("font_add_sprite_ext: not yet implemented"); }

  // ---- String extras ----
  string_byte_length(str: string): number { return new TextEncoder().encode(str).length; }
  string_digits(str: string): string { return str.replace(/\D/g, ""); }
  string_letters(str: string): string { return str.replace(/[^a-zA-Z]/g, ""); }

  // ---- Struct extras ----
  struct_get(struct: any, name: string): any { return struct?.[name]; }
  variable_get_hash(name: string): number {
    let h = this._varHashMap.get(name);
    if (h === undefined) {
      h = this._nextVarHash++;
      this._varHashMap.set(name, h);
      this._hashVarMap.set(h, name);
    }
    return h;
  }
  struct_get_from_hash(struct: any, hash: number): any {
    const name = this._hashVarMap.get(hash);
    return name != null && struct != null ? struct[name] : undefined;
  }

  // ---- Surface extras ----
  surface_copy(_dest: number, _x: number, _y: number, _src: number): void {
    throw new Error("surface_copy: requires WebGL implementation");
  }

  // ---- Tags / misc ----
  tag_get_assets(_tag: string, _kind?: number): any[] { throw new Error("tag_get_assets: not yet implemented"); }
  url_open_ext(url: string, _target: string): void { window.open(url, "_blank"); }

  // ---- View extras ----
  view_set_hport(_view: number, _h: number): void { /* no-op */ }
  view_set_wport(_view: number, _w: number): void { /* no-op */ }

  // ---- Window extras ----
  window_set_caption(_caption: string): void { document.title = _caption; }
  window_set_cursor(cursor: number): void {
    const map: Record<number, string> = {
      [-1]: "none", 0: "default", 1: "default", 2: "crosshair", 3: "text",
      4: "nesw-resize", 5: "ns-resize", 6: "nwse-resize", 7: "ew-resize",
      8: "n-resize", 9: "wait", 10: "grab", 11: "no-drop", 12: "col-resize", 13: "row-resize",
    };
    this._gfx.canvas.style.cursor = map[cursor] ?? "default";
  }
  window_set_fullscreen(enable: boolean): void {
    if (enable) { document.documentElement.requestFullscreen?.(); }
    else { document.exitFullscreen?.(); }
  }

  // ---- More Steam API ----
  steam_activate_overlay(_type: string): void { throw new Error("steam_activate_overlay: not yet implemented"); }
  steam_activate_overlay_user(_type: string, _steamid: number): void { throw new Error("steam_activate_overlay_user: not yet implemented"); }
  steam_get_app_id(): number { throw new Error("steam_get_app_id: not yet implemented"); }
  steam_get_user_persona_name_sync(_steamid?: number): string { throw new Error("steam_get_user_persona_name_sync: not yet implemented"); }
  steam_get_stat_int(_name: string): number { throw new Error("steam_get_stat_int: not yet implemented"); }
  steam_get_global_stat_history_int(_name: string, _days?: number): number { throw new Error("steam_get_global_stat_history_int: not yet implemented"); }
  steam_is_overlay_activated(): boolean { throw new Error("steam_is_overlay_activated: not yet implemented"); }
  steam_image_get_size(_image: number): [number, number] { throw new Error("steam_image_get_size: not yet implemented"); }
  steam_lobby_get_member_count(_lobby?: number): number { throw new Error("steam_lobby_get_member_count: not yet implemented"); }
  steam_lobby_list_add_string_filter(_key: string, _val: string, _type: number): void { throw new Error("steam_lobby_list_add_string_filter: not yet implemented"); }
  steam_lobby_get_chat_message_data(_msg: number, _buf: number, _lobby?: number): number { throw new Error("steam_lobby_get_chat_message_data: not yet implemented"); }
  steam_ugc_subscribe_item(_id: number): void { throw new Error("steam_ugc_subscribe_item: not yet implemented"); }
  steam_input_run_frame(): void { throw new Error("steam_input_run_frame: not yet implemented"); }
  steam_file_write(_path: string, _data: string, _length?: number): boolean { throw new Error("steam_file_write: not yet implemented"); }
  steam_file_exists(_path: string): boolean { throw new Error("steam_file_exists: not yet implemented"); }
  /** UDS (User Data System) is PS4-specific telemetry — no browser equivalent. */
  psn_post_uds_event(_evtype: number, ..._args: any[]): void { /* no-op — PS4 telemetry, no browser equivalent */ }

  // ---- More file/buffer API ----
  file_text_open_write(_path: string): number {
    throw new Error("file_text_open_write: implement in platform layer");
  }
  buffer_seek(_buffer: number, _base: number, _offset: number): void {
    throw new Error("buffer_seek: implement in platform layer");
  }
  buffer_async_group_option(_option: string, _value: any): void { throw new Error("buffer_async_group_option: not yet implemented"); }

  // ---- Array/struct GML internals ----
  // GMS2.3+ runtime sentinels.  As values they act as: null object = null,
  // new-array constructor = function that returns its args, new-object constructor = empty struct.
  __NullObject__: null = null;
  __NewGMLArray__ = (...args: any[]): any[] => args;
  __NewGMLObject__ = (): Record<string, any> => ({});
  // GML throw (called as a function in generated try/catch patterns).
  __throw__(msg: any): never { throw new Error(String(msg)); }

  // ---- Array helpers ----
  array_length(arr: any[]): number { return arr?.length ?? 0; }

  // ---- Misc missing built-ins ----
  draw_clear(colour: number): void {
    const r = colour & 0xff;
    const g = (colour >> 8) & 0xff;
    const b = (colour >> 16) & 0xff;
    const ctx = this._gfx.ctx;
    ctx.fillStyle = `rgb(${r},${g},${b})`;
    ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height);
  }
  draw_vertex_texture(_x: number, _y: number, _xtex: number, _ytex: number): void { throw new Error("draw_vertex_texture: not yet implemented"); }
  vertex_position(_vbuf: number, _x: number, _y: number, _z: number = 0): void { throw new Error("vertex_position: not yet implemented"); }
  vertex_colour(_vbuf: number, _col: number, _alpha: number): void { throw new Error("vertex_colour: not yet implemented"); }
  position_meeting(_x: number, _y: number, _classIndex: number): boolean {
    throw new Error("position_meeting: requires collision system implementation");
  }
  variable_instance_set(instance: any, name: string, value: any): void {
    if (instance != null) instance[name] = value;
  }
  sprite_delete(_spr: number): void { /* no-op */ }
  surface_get_texture(_surf: number): number {
    throw new Error("surface_get_texture: requires WebGL implementation");
  }
  object_get_sprite(classIndex: number): number {
    const clazz = this.classes[classIndex];
    if (!clazz) return -1;
    const inst = this._getInstances(clazz)[0];
    return inst?.sprite_index ?? -1;
  }
  room_duplicate(_room: number): number {
    throw new Error("room_duplicate: not implemented");
  }
  room_set_persistent(_room: number, _persistent: boolean): void { throw new Error("room_set_persistent: not yet implemented"); }
  event_perform(_type: number, _n: number): void { throw new Error("event_perform: not yet implemented"); }
  is_debug_overlay_open(): boolean { return false; }
  path_exists(_path: number): boolean { return false; }
  path_delete(_path: number): void { /* no-op */ }
  part_type_gravity(_part: number, _gx: number, _gy: number): void { /* no-op */ }
  part_system_depth(_syst: number, _depth: number): void { /* no-op */ }
  layer_background_visible(_bg: number, _visible: boolean): void { /* no-op */ }
  layer_sequence_create(_layer: any, _x: number, _y: number, _seq: number): number { throw new Error("layer_sequence_create: not yet implemented"); }
  layer_sequence_is_finished(_seq: number): boolean { throw new Error("layer_sequence_is_finished: not yet implemented"); }
  string_repeat(str: string, count: number): string { return str.repeat(count); }

  // ---- Gamepad API (stubs) ----
  gamepad_get_device_count(): number { return navigator.getGamepads().filter(g => g != null).length; }
  gamepad_axis_value(device: number, axis: number): number {
    const raw = navigator.getGamepads()[device]?.axes[axis] ?? 0;
    const dz = this._gamepadDeadzones.get(device) ?? 0;
    return Math.abs(raw) < dz ? 0 : raw;
  }
  gamepad_button_check_pressed(_device: number, _button: number): boolean { throw new Error("gamepad_button_check_pressed: requires per-frame gamepad state tracking"); }
  gamepad_button_check_released(_device: number, _button: number): boolean { throw new Error("gamepad_button_check_released: requires per-frame gamepad state tracking"); }
  gamepad_button_check(device: number, button: number): boolean { return navigator.getGamepads()[device]?.buttons[button]?.pressed ?? false; }
  gamepad_is_connected(device: number): boolean { return navigator.getGamepads()[device]?.connected ?? false; }
  gamepad_set_vibration(_device: number, _left: number, _right: number): void { /* no-op */ }

  // ---- Display ----
  display_get_width(): number { return window.innerWidth; }
  display_get_height(): number { return window.innerHeight; }

  // ---- JSON ----
  json_stringify(val: any): string { return JSON.stringify(val) ?? "undefined"; }
  json_parse(str: string): any { return JSON.parse(str); }

  // ---- More draw functions ----
  draw_surface_stretched(_surf: number, _x: number, _y: number, _w: number, _h: number): void {
    throw new Error("draw_surface_stretched: requires WebGL implementation");
  }
  draw_circle_color(x: number, y: number, r: number, col1: number, col2: number, outline: boolean): void {
    const ctx = this._gfx.ctx;
    ctx.beginPath();
    ctx.arc(x, y, r, 0, Math.PI * 2);
    if (outline) { ctx.strokeStyle = gmlColorToCss(col1); ctx.stroke(); }
    else {
      const g = ctx.createRadialGradient(x, y, 0, x, y, r);
      g.addColorStop(0, gmlColorToCss(col1)); g.addColorStop(1, gmlColorToCss(col2));
      ctx.fillStyle = g; ctx.fill();
    }
  }
  draw_path(_path: number, _x: number, _y: number, _absolute: boolean): void { throw new Error("draw_path: not yet implemented"); }

  // ---- More buffer functions ----
  buffer_get_size(_buffer: number): number {
    throw new Error("buffer_get_size: implement in platform layer");
  }
  buffer_exists(_buffer: number): boolean { throw new Error("buffer_exists: not yet implemented"); }

  // ---- Layer extras ----
  layer_get_x(_layer: any): number { throw new Error("layer_get_x: not yet implemented"); }
  layer_get_y(_layer: any): number { throw new Error("layer_get_y: not yet implemented"); }
  layer_depth(_layer: any, _depth?: number): number { throw new Error("layer_depth: not yet implemented"); }
  layer_sequence_destroy(_seq: number): void { throw new Error("layer_sequence_destroy: not yet implemented"); }

  // ---- More collision ----
  collision_ellipse_list(_x1: number, _y1: number, _x2: number, _y2: number, _classIndex: number, _prec: boolean, _notme: boolean, _list: number, _ordered: boolean = false): number { throw new Error("collision_ellipse_list: requires collision system implementation"); }

  // ---- Instance change ----
  instance_change(_classIndex: number, _performEvents: boolean): void { throw new Error("instance_change: not yet implemented"); }

  // ---- More Steam ----
  steam_initialised(): boolean { throw new Error("steam_initialised: not yet implemented"); }
  steam_indicate_achievement_progress(_name: string, _cur: number, _max: number): void { throw new Error("steam_indicate_achievement_progress: not yet implemented"); }
  steam_get_user_steam_id(): number { throw new Error("steam_get_user_steam_id: not yet implemented"); }
  steam_get_persona_name(): string { throw new Error("steam_get_persona_name: not yet implemented"); }
  steam_set_achievement(_name: string): void { throw new Error("steam_set_achievement: not yet implemented"); }
  steam_request_global_achievement_percentages(): void { throw new Error("steam_request_global_achievement_percentages: not yet implemented"); }
  steam_get_achievement(_name: string): boolean { throw new Error("steam_get_achievement: not yet implemented"); }
  steam_store_stats(): void { throw new Error("steam_store_stats: not yet implemented"); }
  steam_set_stat_int(_name: string, _val: number): void { throw new Error("steam_set_stat_int: not yet implemented"); }
  steam_net_packet_get_sender_id(): number { throw new Error("steam_net_packet_get_sender_id: not yet implemented"); }
  steam_is_cloud_enabled_for_app(): boolean { throw new Error("steam_is_cloud_enabled_for_app: not yet implemented"); }
  steam_ugc_create_query_user(_account_id: number, _list_type: number, _matching_type: number, _sort_order: number, _creator_app_id?: number, _consumer_app_id?: number, _page?: number): number { throw new Error("steam_ugc_create_query_user: not yet implemented"); }

  // ---- Misc ----
  show_message_async(_str: string): void { console.log("GML show_message_async:", _str); }
  sprite_get_name(_spr: number): string { return `sprite_${_spr}`; }
  room_exists(_room: number): boolean { return _room >= 0 && _room < this._roomInstances.length; }
  string_byte_at(str: string, n: number): number { return str.charCodeAt(n - 1) || 0; }
  video_seek_to(_time: number): void { throw new Error("video_seek_to: not yet implemented"); }
  video_get_position(): number { throw new Error("video_get_position: not yet implemented"); }

  // ---- GMS2.3+ self reference sentinel ----
  // @@This@@ is a pushref to the current instance; in our model it's always `self`.
  // This property is never actually called at runtime — it's a fallback for cases
  // where the IR emits it as a global_ref value.
  __This__: null = null;

  // ---- More geometry / collision ----
  rectangle_in_rectangle(sx1: number, sy1: number, sx2: number, sy2: number, dx1: number, dy1: number, dx2: number, dy2: number): number {
    // Returns: 0=no overlap, 1=partial, 2=fully inside
    if (sx2 < dx1 || sx1 > dx2 || sy2 < dy1 || sy1 > dy2) return 0;
    if (sx1 >= dx1 && sx2 <= dx2 && sy1 >= dy1 && sy2 <= dy2) return 2;
    return 1;
  }
  collision_circle_list(_x: number, _y: number, _r: number, _classIndex: number, _prec: boolean, _notme: boolean, _list: number, _ordered: boolean = false): number { throw new Error("collision_circle_list: requires collision system implementation"); }

  // ---- More draw ----
  draw_roundrect(x1: number, y1: number, x2: number, y2: number, outline: number | boolean = 0): void {
    const ctx = this._gfx.ctx;
    const css = gmlColorToCss(this._draw.config.color);
    const r = Math.round(Math.min(Math.abs(x2 - x1), Math.abs(y2 - y1)) / 6);
    ctx.beginPath();
    ctx.roundRect(Math.min(x1, x2), Math.min(y1, y2), Math.abs(x2 - x1), Math.abs(y2 - y1), r);
    if (outline) { ctx.strokeStyle = css; ctx.stroke(); }
    else { ctx.fillStyle = css; ctx.fill(); }
  }
  draw_roundrect_color(x1: number, y1: number, x2: number, y2: number, col1: number, _col2: number, outline: number | boolean): void {
    const ctx = this._gfx.ctx;
    const r = Math.round(Math.min(Math.abs(x2 - x1), Math.abs(y2 - y1)) / 6);
    ctx.beginPath();
    ctx.roundRect(Math.min(x1, x2), Math.min(y1, y2), Math.abs(x2 - x1), Math.abs(y2 - y1), r);
    if (outline) { ctx.strokeStyle = gmlColorToCss(col1); ctx.stroke(); }
    else { ctx.fillStyle = gmlColorToCss(col1); ctx.fill(); }
  }
  draw_sprite_stretched(spr: number, sub: number, x: number, y: number, w: number, h: number): void {
    const sprite = this.sprites[spr];
    if (!sprite) return;
    const texIdx = sprite.textures[sub] ?? sprite.textures[0];
    if (texIdx === undefined) return;
    const tex = this.textures[texIdx];
    if (!tex) return;
    const sheet = this.textureSheets[tex.sheetId];
    if (!sheet) return;
    this._gfx.ctx.drawImage(sheet, tex.src.x, tex.src.y, tex.src.w, tex.src.h, x, y, w, h);
  }
  draw_line_width(x1: number, y1: number, x2: number, y2: number, w: number): void {
    const ctx = this._gfx.ctx;
    ctx.strokeStyle = gmlColorToCss(this._draw.config.color);
    ctx.lineWidth = w;
    ctx.beginPath(); ctx.moveTo(x1, y1); ctx.lineTo(x2, y2); ctx.stroke();
    ctx.lineWidth = 1;
  }
  draw_ellipse(x1: number, y1: number, x2: number, y2: number, outline: boolean): void {
    const ctx = this._gfx.ctx;
    const css = gmlColorToCss(this._draw.config.color);
    const cx = (x1 + x2) / 2, cy = (y1 + y2) / 2;
    const rx = Math.abs(x2 - x1) / 2, ry = Math.abs(y2 - y1) / 2;
    ctx.beginPath(); ctx.ellipse(cx, cy, rx, ry, 0, 0, Math.PI * 2);
    if (outline) { ctx.strokeStyle = css; ctx.stroke(); }
    else { ctx.fillStyle = css; ctx.fill(); }
  }

  // ---- More string ----
  string_width_ext(_str: string, _sep: number, _w: number): number { return (_str?.length ?? 0) * 8; }
  string_replace(str: string, sub: string, rep: string): string {
    return str.replace(sub, rep);
  }

  // ---- More audio ----
  audio_sound_set_track_position(_sound: number, _pos: number): void { throw new Error("audio_sound_set_track_position: not yet implemented"); }
  audio_sound_get_track_position(_sound: number): number { throw new Error("audio_sound_get_track_position: not yet implemented"); }

  // ---- Clipboard ----
  clipboard_set_text(str: string): void { navigator.clipboard?.writeText(str); }
  clipboard_get_text(): string { throw new Error("clipboard_get_text: not yet implemented"); }

  // ---- Window extras ----
  window_mouse_get_x(): number { return this.mouse_x(); }
  window_mouse_get_y(): number { return this.mouse_y(); }
  window_mouse_set(_x: number, _y: number): void { /* no-op: browser disallows programmatic cursor movement */ }
  window_get_x(): number { return window.screenX; }
  window_get_y(): number { return window.screenY; }
  window_set_size(_w: number, _h: number): void { /* no-op */ }
  window_set_position(_x: number, _y: number): void { /* no-op */ }

  // ---- View helpers ----
  view_set_visible(_view: number, _vis: boolean): void { /* no-op */ }
  view_set_camera(_view: number, _cam: number): void { /* no-op */ }

  // ---- Display extras ----
  display_set_gui_size(_w: number, _h: number): void { /* no-op */ }

  // ---- Room extras ----
  room_get_name(room: number): string { return `room_${room}`; }

  // ---- Font extras ----
  font_get_size(font: number): number { return this.fonts[font]?.size ?? 0; }

  // ---- JSON legacy names ----
  json_encode(val: any): string { return JSON.stringify(val) ?? "undefined"; }
  json_decode(str: string): any { try { return JSON.parse(str); } catch { return undefined; } }

  // ---- Game lifecycle ----
  game_end(): never { throw new Error("game_end"); }

  // ---- DS extras ----
  ds_priority_create(): number { const id = this._dsNextId++; this._dsMaps.set(id, new Map()); return id; }
  ds_priority_find_max(id: number): any {
    const m = this._dsMaps.get(id);
    if (!m || m.size === 0) return undefined;
    let best: any, bestPri = -Infinity;
    for (const [k, v] of m) { if (v > bestPri) { bestPri = v; best = k; } }
    return best;
  }
  ds_priority_add(id: number, val: any, pri: number): void { this._dsMaps.get(id)?.set(val, pri); }
  ds_priority_delete_value(id: number, val: any): void { this._dsMaps.get(id)?.delete(val); }
  ds_priority_size(id: number): number { return this._dsMaps.get(id)?.size ?? 0; }
  ds_priority_empty(id: number): boolean { return (this._dsMaps.get(id)?.size ?? 0) === 0; }
  ds_priority_destroy(id: number): void { this._dsMaps.delete(id); }
  ds_map_write(map: number): string { const m = this._dsMaps.get(map); return m ? JSON.stringify(Object.fromEntries(m)) : "{}"; }
  ds_map_keys_to_array(map: number): any[] { return [...(this._dsMaps.get(map)?.keys() ?? [])]; }
  ds_map_replace(map: number, key: any, val: any): void { this._dsMaps.get(map)?.set(key, val); }
  ds_map_secure_save(_map: number, _filename: string): void { throw new Error("ds_map_secure_save: not yet implemented"); }
  ds_map_values_to_array(map: number): any[] { return [...(this._dsMaps.get(map)?.values() ?? [])]; }
  ds_list_read(list: number, str: string): void {
    const l = this._dsLists.get(list);
    if (!l) return;
    try { const arr = JSON.parse(str); if (Array.isArray(arr)) { l.length = 0; l.push(...arr); } } catch { /* ignore */ }
  }
  ds_list_write(list: number): string { return JSON.stringify(this._dsLists.get(list) ?? []); }
  ds_grid_write(grid: number): string { const g = this._dsGrids.get(grid); return g ? JSON.stringify({ w: g.w, h: g.h, data: g.data }) : "{}"; }

  // ---- Instance extras ----
  method_get_self(func: any): any { return func?._self ?? null; }

  // ---- Sprite extras ----
  sprite_duplicate(_spr: number): number { throw new Error("sprite_duplicate: not yet implemented"); }
  sprite_collision_mask(_spr: number, ..._args: any[]): void { throw new Error("sprite_collision_mask: not yet implemented"); }

  // ---- Path ----
  path_add(): number { throw new Error("path_add: not yet implemented"); }
  path_end(): void { /* no-op */ }

  // ---- Particle system extras ----
  part_system_exists(_syst: number): boolean { return false; }
  part_system_position(_syst: number, _x: number, _y: number): void { /* no-op */ }
  part_system_draw_order(_syst: number, _order: boolean): void { /* no-op */ }

  // ---- Layer extras ----
  layer_exists(_layer: any): boolean { return false; }
  layer_vspeed(_layer: any, _speed?: number): any { if (_speed !== undefined) return; return 0; }
  layer_background_sprite(_bg: number, _spr?: number): any { if (_spr !== undefined) return; return -1; }
  layer_background_index(_bg: number, _sprite: number): void { /* no-op */ }
  layer_background_get_index(_bg: number): number { throw new Error("layer_background_get_index: not yet implemented"); }

  // ---- GPU extras ----
  gpu_set_texfilter(_enable: boolean): void { throw new Error("gpu_set_texfilter: not yet implemented"); }

  // ---- Texture extras ----
  texture_prefetch(_tex: number): void { /* no-op: textures preloaded */ }
  texture_set_stage(_stage: number, _tex: number): void { /* no-op: 2D canvas has no texture stages */ }
  texture_is_ready(_tex: number): boolean { return true; }
  texture_get_texel_height(tex: number): number {
    const t = this.textures[tex]; if (!t) return 1;
    const sheet = this.textureSheets[t.sheetId]; return sheet ? 1 / sheet.naturalHeight : 1;
  }
  texture_get_texel_width(tex: number): number {
    const t = this.textures[tex]; if (!t) return 1;
    const sheet = this.textureSheets[t.sheetId]; return sheet ? 1 / sheet.naturalWidth : 1;
  }

  // ---- Vertex buffer (stubs) ----
  vertex_create_buffer(): number { throw new Error("vertex_create_buffer: requires WebGL implementation"); }
  vertex_delete_buffer(_buf: number): void { throw new Error("vertex_delete_buffer: not yet implemented"); }
  vertex_begin(_buf: number, _format: number): void { throw new Error("vertex_begin: not yet implemented"); }
  vertex_end(_buf: number): void { throw new Error("vertex_end: not yet implemented"); }
  vertex_submit(_buf: number, _prim: number, _tex: number): void { throw new Error("vertex_submit: not yet implemented"); }
  vertex_format_begin(): void { throw new Error("vertex_format_begin: not yet implemented"); }
  vertex_format_end(): number { throw new Error("vertex_format_end: not yet implemented"); }

  // ---- Video (stubs) ----
  video_open(_path: string, ..._args: any[]): void { throw new Error("video_open: not yet implemented"); }
  video_close(): void { throw new Error("video_close: not yet implemented"); }
  video_draw(): void { throw new Error("video_draw: not yet implemented"); }
  video_get_status(): number { throw new Error("video_get_status: not yet implemented"); }
  video_get_duration(): number { throw new Error("video_get_duration: not yet implemented"); }
  video_set_volume(_vol: number): void { throw new Error("video_set_volume: not yet implemented"); }

  // mouse_wheel_up / mouse_wheel_down delegated to createInputAPI

  // ---- Navigation mesh ----
  mp_grid_path(_grid: number, _path: number, _xstart: number, _ystart: number, _xgoal: number, _ygoal: number, _allowDiag: boolean): boolean { throw new Error("mp_grid_path: requires pathfinding implementation"); }

  // ---- struct alias ----
  struct_exists(struct: any, name: string): boolean {
    return struct != null && Object.prototype.hasOwnProperty.call(struct, name);
  }
  struct_get_names(struct: any): string[] {
    return struct != null ? Object.keys(struct) : [];
  }

  // ---- Camera extras ----
  camera_create_view(x: number, y: number, w: number, h: number, ..._args: any[]): number {
    const id = this._nextCamId++;
    this._cameras.set(id, { x, y, w, h });
    return id;
  }

  // ---- GC ----
  gc_collect(): void { /* no-op: browser handles GC */ }

  // ---- Screen save ----
  screen_save(_filename: string): void { throw new Error("screen_save: not yet implemented"); }

  // ---- Async dialogs ----
  get_integer_async(_message: string, _default: number): number { return _default; }
  show_message(_str: string): void { console.log("GML show_message:", _str); }

  // ---- char ----
  chr(code: number): string { return String.fromCharCode(code); }

  // ---- More buffer ----
  buffer_peek(_buffer: number, _offset: number, _type: number): any {
    throw new Error("buffer_peek: implement in platform layer");
  }

  // ---- More Steam ----
  steam_utils_enable_callbacks(_enable?: boolean): void { throw new Error("steam_utils_enable_callbacks: not yet implemented"); }
  steam_upload_score_buffer_ext(_name: string, _score: number, _buf: number, ..._args: any[]): void { throw new Error("steam_upload_score_buffer_ext: not yet implemented"); }
  steam_upload_score_ext(_name: string, _score: number, ..._args: any[]): void { throw new Error("steam_upload_score_ext: not yet implemented"); }
  steam_ugc_start_item_update(_appId: number, _fileId: number): number { throw new Error("steam_ugc_start_item_update: not yet implemented"); }
  steam_ugc_set_item_description(_handle: number, _desc: string): void { throw new Error("steam_ugc_set_item_description: not yet implemented"); }
  steam_net_packet_get_data(_buf: number): void { throw new Error("steam_net_packet_get_data: not yet implemented"); }
  steam_net_packet_receive(): boolean { throw new Error("steam_net_packet_receive: not yet implemented"); }
  steam_net_packet_send(_steamid: number, _buf: number, _size?: number, _type?: number): void { throw new Error("steam_net_packet_send: not yet implemented"); }
  steam_music_play(): void { throw new Error("steam_music_play: not yet implemented"); }
  steam_music_is_enabled(): boolean { throw new Error("steam_music_is_enabled: not yet implemented"); }
  steam_music_get_status(): number { throw new Error("steam_music_get_status: not yet implemented"); }
  steam_lobby_list_request(): void { throw new Error("steam_lobby_list_request: not yet implemented"); }
  steam_lobby_list_get_lobby_id(_index: number): number { throw new Error("steam_lobby_list_get_lobby_id: not yet implemented"); }
  steam_lobby_list_get_count(): number { throw new Error("steam_lobby_list_get_count: not yet implemented"); }
  steam_get_most_achieved_achievement_info(_info?: any[]): boolean { throw new Error("steam_get_most_achieved_achievement_info: not yet implemented"); }
  steam_get_local_file_change_count(): number { throw new Error("steam_get_local_file_change_count: not yet implemented"); }
  steam_available_languages(): string { throw new Error("steam_available_languages: not yet implemented"); }
  steam_inventory_get_all_items(_arr?: any): number { throw new Error("steam_inventory_get_all_items: not yet implemented"); }
  steam_get_quota_total(): number { throw new Error("steam_get_quota_total: not yet implemented"); }
  steam_get_global_stat_history_real(_name: string, _days?: number): number { throw new Error("steam_get_global_stat_history_real: not yet implemented"); }
  steam_file_read(_path: string): string { throw new Error("steam_file_read: not yet implemented"); }
  steam_set_rich_presence(_key: string, _val: string): void { throw new Error("steam_set_rich_presence: not yet implemented"); }
  steam_user_get_auth_session_ticket(_arr?: any): number { throw new Error("steam_user_get_auth_session_ticket: not yet implemented"); }

  // ---- PS5 stubs ----
  ps5_gamepad_set_vibration_mode(_port: number, _mode: number): void { /* no-op */ }
  ps5_gamepad_set_trigger_effect_vibration(_port: number, _trigger: number, _start: number, _end: number, _str: number): void { /* no-op */ }

  // ---- pass (no-op — used in some GML contexts) ----
  pass(): void { /* no-op: GMS2.3+ no-op statement */ }

  // ---- More Steam (third batch) ----
  steam_update(): void { throw new Error("steam_update: not yet implemented"); }
  steam_ugc_set_item_tags(_handle: number, _tags: string[]): void { throw new Error("steam_ugc_set_item_tags: not yet implemented"); }
  steam_ugc_set_item_content(_handle: number, _path: string): void { throw new Error("steam_ugc_set_item_content: not yet implemented"); }
  steam_ugc_send_query(_handle: number): void { throw new Error("steam_ugc_send_query: not yet implemented"); }
  steam_ugc_request_item_details(_id: number, _maxAge: number): void { throw new Error("steam_ugc_request_item_details: not yet implemented"); }
  steam_ugc_num_subscribed_items(): number { throw new Error("steam_ugc_num_subscribed_items: not yet implemented"); }
  steam_ugc_create_item(_appId: number, _type: number): void { throw new Error("steam_ugc_create_item: not yet implemented"); }
  steam_ugc_create_query_all(_queryType: number, _matchingType: number, _creatorAppId?: number, _consumerAppId?: number, _page?: number): number { throw new Error("steam_ugc_create_query_all: not yet implemented"); }
  steam_ugc_delete_item(_id: number): void { throw new Error("steam_ugc_delete_item: not yet implemented"); }
  steam_ugc_unsubscribe_item(_id: number): void { throw new Error("steam_ugc_unsubscribe_item: not yet implemented"); }
  steam_ugc_set_item_preview(_handle: number, _path: string): void { throw new Error("steam_ugc_set_item_preview: not yet implemented"); }
  steam_ugc_set_item_title(_handle: number, _title: string): void { throw new Error("steam_ugc_set_item_title: not yet implemented"); }
  steam_ugc_set_item_visibility(_handle: number, _vis: number): void { throw new Error("steam_ugc_set_item_visibility: not yet implemented"); }
  steam_stats_ready(): boolean { throw new Error("steam_stats_ready: not yet implemented"); }
  steam_send_screenshot(_path?: string, _w?: number, _h?: number): void { throw new Error("steam_send_screenshot: not yet implemented"); }
  steam_request_global_stats(_days: number): void { throw new Error("steam_request_global_stats: not yet implemented"); }
  steam_reset_all_stats_achievements(_also_achievements?: boolean): void { throw new Error("steam_reset_all_stats_achievements: not yet implemented"); }
  steam_set_stat_avg_rate(_name: string, _session: number, _session_len: number): void { throw new Error("steam_set_stat_avg_rate: not yet implemented"); }
  steam_set_stat_float(_name: string, _val: number): void { throw new Error("steam_set_stat_float: not yet implemented"); }
  steam_show_floating_gamepad_text_input(_mode: number, _x: number, _y: number, _w: number, _h: number): void { throw new Error("steam_show_floating_gamepad_text_input: not yet implemented"); }
  steam_shutdown(): void { throw new Error("steam_shutdown: not yet implemented"); }
  steam_lobby_set_owner_id(_steamid: number, _lobby?: number): void { throw new Error("steam_lobby_set_owner_id: not yet implemented"); }
  steam_lobby_send_chat_message_buffer(_buf: number, _size?: number, _lobby?: number): boolean { throw new Error("steam_lobby_send_chat_message_buffer: not yet implemented"); }
  steam_lobby_is_owner(_lobby?: number): boolean { throw new Error("steam_lobby_is_owner: not yet implemented"); }
  steam_lobby_create(_type: number, _max_members: number): void { throw new Error("steam_lobby_create: not yet implemented"); }
  steam_lobby_activate_invite_overlay(_lobby?: number): void { throw new Error("steam_lobby_activate_invite_overlay: not yet implemented"); }
  steam_lobby_list_get_data(_index: number, _key: string): string { throw new Error("steam_lobby_list_get_data: not yet implemented"); }
  steam_lobby_list_join(_lobby: number): void { throw new Error("steam_lobby_list_join: not yet implemented"); }
  steam_is_user_logged_on(): boolean { throw new Error("steam_is_user_logged_on: not yet implemented"); }
  steam_is_screenshot_requested(): boolean { throw new Error("steam_is_screenshot_requested: not yet implemented"); }
  steam_is_overlay_enabled(): boolean { throw new Error("steam_is_overlay_enabled: not yet implemented"); }
  steam_get_quota_free(): number { throw new Error("steam_get_quota_free: not yet implemented"); }
  steam_get_number_of_current_players(): void { throw new Error("steam_get_number_of_current_players: not yet implemented"); }
  steam_get_app_ownership_ticket_data(_appId: number): string { throw new Error("steam_get_app_ownership_ticket_data: not yet implemented"); }
  steam_file_read_buffer(_path: string, _buf?: number): boolean { throw new Error("steam_file_read_buffer: not yet implemented"); }
  steam_file_persisted(_path: string): boolean { throw new Error("steam_file_persisted: not yet implemented"); }
  steam_download_scores_around_user(_board: string, _range: number, _range2?: number): void { throw new Error("steam_download_scores_around_user: not yet implemented"); }
  steam_download_scores(_board: string, _start: number, _end: number): void { throw new Error("steam_download_scores: not yet implemented"); }
  steam_download_friends_scores(_board: string): void { throw new Error("steam_download_friends_scores: not yet implemented"); }
  steam_music_pause(): void { throw new Error("steam_music_pause: not yet implemented"); }
  steam_music_play_next(): void { throw new Error("steam_music_play_next: not yet implemented"); }
  steam_music_play_previous(): void { throw new Error("steam_music_play_previous: not yet implemented"); }
  steam_music_set_volume(_vol: number): void { throw new Error("steam_music_set_volume: not yet implemented"); }
  steam_music_is_playing(): boolean { throw new Error("steam_music_is_playing: not yet implemented"); }
  steam_user_cancel_auth_ticket(_ticket: number): void { throw new Error("steam_user_cancel_auth_ticket: not yet implemented"); }
  steam_user_installed_dlc(_appId: number): boolean { throw new Error("steam_user_installed_dlc: not yet implemented"); }
  steam_user_owns_dlc(_appId: number): boolean { throw new Error("steam_user_owns_dlc: not yet implemented"); }
  steam_user_request_encrypted_app_ticket(_extra: any): void { throw new Error("steam_user_request_encrypted_app_ticket: not yet implemented"); }
  steam_utils_get_server_real_time(): number { return Math.floor(Date.now() / 1000); }
  steam_utils_is_steam_running_on_steam_deck(): boolean { throw new Error("steam_utils_is_steam_running_on_steam_deck: not yet implemented"); }

  // ---- More sprite/font ----
  sprite_save(_spr: number, _sub: number, _fname: string): void { throw new Error("sprite_save: not yet implemented"); }
  sprite_add(_path: string, _frames: number, _removebg: boolean, _smooth: boolean, _xorig: number, _yorig: number): number { throw new Error("sprite_add: not yet implemented"); }
  sprite_get_uvs(spr: number, sub: number): number[] {
    const sprite = this.sprites[spr]; if (!sprite) return [0, 0, 1, 1, 0, 0, 1, 1];
    const texIdx = sprite.textures[sub] ?? sprite.textures[0]; if (texIdx === undefined) return [0, 0, 1, 1, 0, 0, 1, 1];
    const tex = this.textures[texIdx]; if (!tex) return [0, 0, 1, 1, 0, 0, 1, 1];
    const sheet = this.textureSheets[tex.sheetId]; if (!sheet) return [0, 0, 1, 1, 0, 0, 1, 1];
    const tw = sheet.naturalWidth, th = sheet.naturalHeight;
    const u0 = tex.src.x / tw, v0 = tex.src.y / th;
    const u1 = (tex.src.x + tex.src.w) / tw, v1 = (tex.src.y + tex.src.h) / th;
    return [u0, v0, u1, v1, 0, 0, tex.dest.w, tex.dest.h];
  }
  sprite_prefetch(_spr: number): void { /* no-op */ }
  sprite_set_speed(_spr: number, _speed: number, _type: number): void { throw new Error("sprite_set_speed: not yet implemented"); }

  // ---- ord / chr extras ----
  ord(char: string): number { return char.charCodeAt(0) || 0; }

  // ---- Type checks ----
  is_struct(val: any): boolean {
    return val !== null && typeof val === "object" && !Array.isArray(val) && !(val instanceof Object.getPrototypeOf(Object).constructor);
  }
  is_method(val: any): boolean { return typeof val === "function"; }
  _instanceof(val: any, constructor?: any): any {
    if (constructor === undefined) {
      // 1-arg form: returns the constructor name as a string (like `typeof` for structs).
      if (val === null || val === undefined) return "undefined";
      const ctor = Object.getPrototypeOf(val)?.constructor;
      return ctor?.name ?? "struct";
    }
    return this.is_instanceof(val, constructor);
  }

  // ---- Game speed ----
  game_get_speed(_type: number): number { return this.room_speed; }

  // ---- Layer extras ----
  layer_get_depth(_layer: any): number { throw new Error("layer_get_depth: not yet implemented"); }
  layer_set_visible(_layer: any, _visible: boolean): void { /* no-op */ }
  layer_x(_layer: any, _x?: number): any { if (_x !== undefined) return; return 0; }
  layer_y(_layer: any, _y?: number): any { if (_y !== undefined) return; return 0; }

  // ---- More instance ----
  instance_deactivate_all(_notme: boolean): void { throw new Error("instance_deactivate_all: requires instance activation system"); }
  instance_furthest(_x: number, _y: number, _classIndex: number): any { throw new Error("instance_furthest: requires spatial instance index"); }
  instance_position(_x: number, _y: number, _classIndex: number): any { throw new Error("instance_position: requires collision system implementation"); }

  // ---- Object extras ----
  object_get_parent(classIndex: number): number {
    const clazz = this.classes[classIndex];
    if (!clazz) return -1;
    const parent = Object.getPrototypeOf(clazz.prototype)?.constructor;
    const idx = this.classes.indexOf(parent);
    return idx >= 0 ? idx : -1;
  }

  // ---- More geometry ----
  point_in_circle(px: number, py: number, cx: number, cy: number, r: number): boolean {
    return ((px - cx) ** 2 + (py - cy) ** 2) <= r * r;
  }
  point_distance_3d(x1: number, y1: number, z1: number, x2: number, y2: number, z2: number): number {
    return Math.sqrt((x2 - x1) ** 2 + (y2 - y1) ** 2 + (z2 - z1) ** 2);
  }

  // ---- Room extras ----
  room_instance_clear(_room: number): void { throw new Error("room_instance_clear: not yet implemented"); }

  // ---- GPU extras ----
  gpu_set_blendmode_ext_sepalpha(_src: number, _dest: number, _srcA: number, _destA: number): void { throw new Error("gpu_set_blendmode_ext_sepalpha: not yet implemented"); }

  // ---- Gamepad extras ----
  gamepad_get_description(device: number): string { return navigator.getGamepads()[device]?.id ?? ""; }
  gamepad_is_supported(): boolean { return "getGamepads" in navigator; }
  gamepad_set_axis_deadzone(device: number, deadzone: number): void { this._gamepadDeadzones.set(device, deadzone); }
  gamepad_set_color(_device: number, _col: number): void { /* no-op */ }

  // ---- GC extras ----
  gc_enable(_enable: boolean): void { /* no-op */ }

  // ---- MP grid (pathfinding) ----
  mp_grid_create(_left: number, _top: number, _hcells: number, _vcells: number, _cellw: number, _cellh: number): number { throw new Error("mp_grid_create: requires pathfinding implementation"); }
  mp_grid_add_rectangle(_id: number, _x1: number, _y1: number, _x2: number, _y2: number): void { throw new Error("mp_grid_add_rectangle: requires pathfinding implementation"); }
  mp_grid_add_instances(_id: number, _classIndex: number, _prec: boolean): void { throw new Error("mp_grid_add_instances: requires pathfinding implementation"); }
  mp_grid_clear_all(_id: number): void { throw new Error("mp_grid_clear_all: requires pathfinding implementation"); }
  mp_grid_clear_rectangle(_id: number, _x1: number, _y1: number, _x2: number, _y2: number): void { throw new Error("mp_grid_clear_rectangle: requires pathfinding implementation"); }
  mp_grid_draw(_id: number): void { throw new Error("mp_grid_draw: not yet implemented"); }

  // ---- Particle extras ----
  part_emitter_stream(_syst: number, _emit: number, _type: number, _num: number): void { /* no-op */ }
  part_particles_clear(_syst: number): void { throw new Error("part_particles_clear: not yet implemented"); }
  part_system_automatic_update(_syst: number, _on: boolean): void { /* no-op */ }
  part_system_drawit(_syst: number): void { /* no-op */ }

  // ---- Path extras ----
  path_get_length(_path: number): number { throw new Error("path_get_length: requires path system implementation"); }
  path_start(_path: number, _speed: number, _end: number, _abs: boolean): void { throw new Error("path_start: requires path system implementation"); }

  // ---- Debug ----
  show_debug_log(_str: string): void { console.debug("GML debug:", _str); }
  show_debug_overlay(_enable: boolean): void { /* no-op */ }

  // ---- More async ----
  get_string_async(_message: string, _default: string): string { return _default; }

  // ---- Video extras ----
  video_get_format(): string { throw new Error("video_get_format: not yet implemented"); }

  // ---- PSN — mapped to browser equivalents ----
  // Trophy state is stored under __psn_trophy_<gameName> (distinct from __gml_fs_ ini saves).
  // Save data backup is a no-op: localStorage is already persistent; no cloud sync needed.
  // Communication restrictions and commerce dialogs have no browser equivalent.

  /** Load trophy unlock state from storage into the in-memory set. */
  psn_init_trophy(_pad_index?: number): void {
    const gameName = this._storage.gameName;
    const raw = loadItem("__psn_trophy_" + gameName);
    if (raw) {
      try {
        const ids: number[] = JSON.parse(raw);
        for (const id of ids) this._psnTrophies.add(id);
      } catch { /* corrupt data — start fresh */ }
    }
  }

  /** Unlock a trophy by ID; persist immediately. */
  psn_unlock_trophy(id: number, _slot: number = 0): void {
    this._psnTrophies.add(id);
    saveItem("__psn_trophy_" + this._storage.gameName, JSON.stringify([...this._psnTrophies]));
  }

  /**
   * Return trophy unlock state: 0 = locked, 1 = unlocked.
   * (GML also defines 2 = "unlock pending"; we use 0/1 since our unlocks are synchronous.)
   */
  psn_get_trophy_unlock_state(id: number): number {
    return this._psnTrophies.has(id) ? 1 : 0;
  }

  /** Process pending PSN async callbacks. All our PSN ops are synchronous, so this is a no-op. */
  psn_tick(): void { /* no-op — all PSN operations are synchronous in browser */ }
  psn_tick_error_dialog(): void { /* no-op — no PSN errors in browser */ }
  psn_np_commerce_dialog_tick(): void { /* no-op — PSN commerce not available in browser */ }

  /**
   * Request a cloud backup of save data. localStorage is already persistent,
   * so this is a no-op in the browser (data is not lost between sessions).
   */
  psn_save_data_backup(_slot?: any, _id?: any): void { /* no-op — localStorage is already persistent */ }

  /** Return communication restriction status. 0 = unrestricted (no PSN parental controls in browser). */
  psn_communication_restriction_status(_pad_index?: number): number { return 0; }

  // ---- More Steam ----
  steam_activate_overlay_browser(_url: string): void { throw new Error("steam_activate_overlay_browser: not yet implemented"); }
  steam_clear_achievement(_name: string): void { throw new Error("steam_clear_achievement: not yet implemented"); }
  steam_file_delete(_path: string): void { throw new Error("steam_file_delete: not yet implemented"); }
  steam_file_get_list(): string[] { throw new Error("steam_file_get_list: not yet implemented"); }
  steam_file_share(_path: string): void { throw new Error("steam_file_share: not yet implemented"); }
  steam_file_size(_path: string): number { throw new Error("steam_file_size: not yet implemented"); }
  steam_file_write_buffer(_path: string, _buf: number, _size?: number): boolean { throw new Error("steam_file_write_buffer: not yet implemented"); }
  steam_file_write_file(_path: string, _srcpath: string): boolean { throw new Error("steam_file_write_file: not yet implemented"); }
  steam_get_achievement_progress_limits_int(_name: string): [number, number] { throw new Error("steam_get_achievement_progress_limits_int: not yet implemented"); }
  steam_get_global_stat_real(_name: string): number { throw new Error("steam_get_global_stat_real: not yet implemented"); }
  steam_get_local_file_change(_index: number): string { throw new Error("steam_get_local_file_change: not yet implemented"); }
  steam_input_activate_action_set(_handle: number, _setHandle: number): void { throw new Error("steam_input_activate_action_set: not yet implemented"); }
  steam_input_get_action_origin_from_xbox_origin(_handle: number, _origin: number): number { throw new Error("steam_input_get_action_origin_from_xbox_origin: not yet implemented"); }
  steam_input_get_analog_action_handle(_name: string): number { throw new Error("steam_input_get_analog_action_handle: not yet implemented"); }
  steam_input_get_connected_controllers(): number[] { throw new Error("steam_input_get_connected_controllers: not yet implemented"); }
  steam_input_get_digital_action_data(_controller: number, _action: number): boolean { throw new Error("steam_input_get_digital_action_data: not yet implemented"); }
  steam_input_get_digital_action_origins(_controller: number, _action_set: number, _action: number): number[] { throw new Error("steam_input_get_digital_action_origins: not yet implemented"); }
  steam_input_get_glyph_png_for_action_origin(_origin: number, _style: number, _flags: number): string { throw new Error("steam_input_get_glyph_png_for_action_origin: not yet implemented"); }
  steam_input_init(_explicit: boolean): void { throw new Error("steam_input_init: not yet implemented"); }
  steam_inventory_trigger_item_drop(_id: number): void { throw new Error("steam_inventory_trigger_item_drop: not yet implemented"); }
  steam_is_subscribed(): boolean { throw new Error("steam_is_subscribed: not yet implemented"); }
  steam_lobby_leave(_lobby?: number): void { throw new Error("steam_lobby_leave: not yet implemented"); }

  // ---- Instance position/collision with DS list ----

  instance_place_list(_x: number, _y: number, _classIndex: number, _list: number, _notme: boolean): number { throw new Error("instance_place_list: not yet implemented"); }
  instance_position_list(_x: number, _y: number, _classIndex: number, _list: number, _notme: boolean): number { throw new Error("instance_position_list: not yet implemented"); }

  instance_destroy(instance: GMLObject): void {
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
    const newRoom = this._roomInstances[id]!;
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

  /** Called once per frame. Override to hook pause/resume, speed control, etc. */
  onTick?: () => void;

  private _runFrame(): void {
    const start = performance.now();
    this.onTick?.();
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
      this.Sprites[config.sprites[i]!.name] = i;
    }
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
  roomCreationCode?: (((_rt: GameRuntime) => void) | undefined)[];
}

// ---- Factory function ----

export function createGameRuntime(opts?: { root?: RenderRoot }): GameRuntime {
  const rt = new GameRuntime();
  if (opts?.root) rt._root = opts.root;
  return rt;
}

