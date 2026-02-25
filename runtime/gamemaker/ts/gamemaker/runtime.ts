/**
 * GML Runtime — game loop, GMLObject base class, room system.
 */

import { GraphicsContext, initCanvas, createCanvas, resizeCanvas, loadImage, scheduleTimeout } from "../shared/platform";
import { PersistenceState, init as initPersistence, save, load as loadItem, remove } from "../shared/platform/persistence";
import type { RenderRoot } from "../shared/render-root";
import { DrawState, createDrawAPI } from "./draw";
import { InputState, createInputAPI } from "./input";
import { gmlColorToCss } from "./color";
import { StorageState, createStorageAPI } from "./storage";
import {
  AudioState, loadAudio,
  play as audioPlay, stop as audioStop, stopAll as audioStopAll,
  pause as audioPause, resume as audioResume, resumeAll as audioResumeAll,
  isPlaying as audioIsPlaying, isPaused as audioIsPaused,
  setVoiceGain as audioSetGain, getVoiceGain as audioGetGain,
  setVoicePitch as audioSetPitch, getVoicePitch as audioGetPitch,
  setVoicePan as audioSetPan, getVoicePan as audioGetPan,
  setMasterGain as audioSetMasterGain,
  getPosition as audioGetPosition, setPosition as audioSetPosition,
  soundLength as audioSoundLength,
  setNodeParam as audioSetNodeParam,
} from "../shared/platform/audio";
import { MathState, createMathAPI } from "./math";
import { createGlobalAPI } from "./global";
import { createInstanceAPI } from "./instance";
import { ACTIVE, noop } from "./constants";
import type { Sprite } from "../../data/sprites";
import type { Texture } from "../../data/textures";
import type { Font } from "../../data/fonts";
import type { Room } from "../../data/rooms";
import type { Sound } from "../../data/sounds";

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
  constructor(private rt: GameRuntime) {}

  draw(): void {
    const rt = this.rt;
    const ctx = rt._gfx.ctx;
    ctx.fillStyle = "black";
    ctx.fillRect(0, 0, rt._gfx.canvas.width, rt._gfx.canvas.height);

    const oldRoom = rt.room;
    rt._isStepping = true;

    const deact = rt._deactivatedInstances;

    // Alarms
    for (const instance of rt.roomVariables) {
      if (deact.has(instance)) continue;
      if (instance.alarm.length !== 0) {
        for (let i = 0; i < 12; i++) {
          const alarmVal = instance.alarm[i];
          if (alarmVal) {
            instance.alarm[i] = alarmVal - 1;
            if (alarmVal - 1 === 0) {
              delete instance.alarm[i];
              const method = (instance as any)["alarm" + i];
              if (method !== noop) { rt._self = instance; method.call(instance); rt._self = null; }
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
        if (deact.has(instance)) continue;
        if ((instance as any).beginstep === noop) continue;
        instance.xprevious = instance.x;
        instance.yprevious = instance.y;
        rt._self = instance; instance.beginstep(); rt._self = null;
        if (oldRoom !== rt.room) break;
      }
      toStep = rt._pendingStep;
    }

    // Step
    toStep = rt.roomVariables;
    while (toStep.length !== 0) {
      rt._pendingStep = [];
      for (const instance of toStep) {
        if (deact.has(instance)) continue;
        if ((instance as any).step === noop) continue;
        rt._self = instance; instance.step(); rt._self = null;
        if (oldRoom !== rt.room) break;
      }
      toStep = rt._pendingStep;
    }

    // End step
    toStep = rt.roomVariables;
    while (toStep.length !== 0) {
      rt._pendingStep = [];
      for (const instance of toStep) {
        if (deact.has(instance)) continue;
        if ((instance as any).endstep === noop) continue;
        rt._self = instance; instance.endstep(); rt._self = null;
        if (oldRoom !== rt.room) break;
      }
      toStep = rt._pendingStep;
    }

    rt._isStepping = false;

    // Draw (sorted by depth, descending; skip deactivated)
    const sorted = rt.roomVariables.slice().sort((a, b) => b.depth - a.depth);
    for (const instance of sorted) {
      if (deact.has(instance)) continue;
      if ((instance as any).draw === noop) continue;
      rt._self = instance; instance.draw(); rt._self = null;
      if (oldRoom !== rt.room) break;
    }

    // Draw GUI
    if (rt._drawguiUsed) {
      for (const instance of sorted) {
        if (deact.has(instance)) continue;
        if ((instance as any).drawgui === noop) continue;
        rt._self = instance; instance.drawgui(); rt._self = null;
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
      rt._self = instance; instance.create(); rt._self = null;
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

// ---- Buffer helpers ----

/** Returns the byte size of a GML buffer type constant, or 0 for string types. */
function bufferTypeSize(type: number): number {
  switch (type) {
    case 1: case 2: case 13: return 1;   // buffer_u8, buffer_s8, buffer_bool
    case 3: case 4: return 2;            // buffer_u16, buffer_s16
    case 5: case 6: case 7: return 4;    // buffer_u32, buffer_s32, buffer_f32
    case 8: case 16: return 8;           // buffer_f64, buffer_u64
    default: return 0;                   // buffer_string, buffer_text (variable)
  }
}

// ---- GameRuntime ----

export class GameRuntime {
  // Sub-state containers
  _draw = new DrawState();
  _input = new InputState();
  _storage = new StorageState();
  _audio = new AudioState();
  _math = new MathState();
  _persistence = new PersistenceState();
  _gfx = new GraphicsContext();
  _root?: RenderRoot;

  // Surface state
  _surfaces = new Map<number, OffscreenCanvas>();
  _surfaceCtxStack: (CanvasRenderingContext2D | OffscreenCanvasRenderingContext2D)[] = [];
  _surfaceIdStack: number[] = [];
  _nextSurfaceId = 1;

  // Buffer state: each entry is { data, pos, kind, align }
  _buffers = new Map<number, { data: Uint8Array; pos: number; kind: number; align: number }>();
  _nextBufferId = 1;

  // Current "self" instance for alarm_set / event_user dispatch
  _self: GMLObject | null = null;

  // Deactivated instances (skipped in game loop until reactivated)
  _deactivatedInstances = new Set<GMLObject>();

  // Draw primitive accumulator
  _primKind = 0;
  _primVerts: { x: number; y: number }[] = [];

  // Per-sprite speed overrides (sprite_set_speed); default is room_speed
  _spriteSpeedOverrides = new Map<number, number>();

  // Video element for video_open/draw
  private _video: HTMLVideoElement | null = null;

  // Vertex buffer state
  private _vbufs = new Map<number, { verts: { x: number; y: number; col: number; alpha: number }[]; recording: boolean }>();
  private _nextVbufId = 1;
  private _vbufCurrent = -1;
  private _vbufFormatDummy = 0;

  // Layer background sprite assignments (layer_background_set_sprite)
  _layerBackgroundSprites = new Map<number, number>();

  // File text handles: id → { path, content, pos, mode: 'r'|'w' }
  _textFiles = new Map<number, { path: string; content: string; pos: number; mode: 'r' | 'w' }>();
  _nextTextFileId = 1;

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
  sounds: Sound[] = [];
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
      this._self = instance; instance.create(); this._self = null;
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

  layer_get_id(name: string): number {
    // Layers are not parsed from the data file; return a stable integer handle derived from the name.
    // Games that only use the returned value as an opaque handle (e.g. layer_background_get_id) will work correctly.
    let h = 0x811c9dc5;
    for (let i = 0; i < name.length; i++) {
      h ^= name.charCodeAt(i);
      h = (h * 0x01000193) >>> 0;
    }
    // Keep in positive range (GML uses positive layer IDs)
    return (h & 0x7fffffff) || 1;
  }

  // ---- Sprite API extensions ----

  sprite_get_xoffset(spr: number): number { return this.sprites[spr]?.origin.x ?? 0; }
  sprite_get_yoffset(spr: number): number { return this.sprites[spr]?.origin.y ?? 0; }
  sprite_get_number(spr: number): number { return this.sprites[spr]?.textures.length ?? 1; }
  sprite_get_speed(spr: number): number {
    return this._spriteSpeedOverrides.get(spr) ?? this.room_speed;
  }
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

  alarm_set(alarm: number, steps: number): void {
    if (this._self) this._self.alarm[alarm] = steps;
  }
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

  asset_get_index(name: string): number {
    // Search objects, sprites, sounds, rooms, fonts in that order.
    if (name in this._classesEnum) return this._classesEnum[name]!;
    if (name in this.Sprites) return (this.Sprites as Record<string, number>)[name]!;
    const snd = this.sounds.findIndex((s) => s.name === name);
    if (snd !== -1) return snd;
    const room = this._roomDatas.findIndex((r) => r.name === name);
    if (room !== -1) return room;
    const font = this.fonts.findIndex((f) => f.name === name);
    if (font !== -1) return font;
    return -1;
  }
  asset_get_tags(_asset: number, _type: number = -1): string[] { return []; }
  asset_has_tags(_asset: number, _tags: string | string[], _not?: boolean | number): boolean { return false; }

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

  // ---- Surface API (Canvas 2D offscreen rendering) ----
  // Surfaces are OffscreenCanvas objects. surface_set_target redirects all
  // draw calls by swapping _gfx.ctx; surface_reset_target restores it.

  surface_create(w: number, h: number, _format: number = 0): number {
    const id = this._nextSurfaceId++;
    this._surfaces.set(id, new OffscreenCanvas(Math.max(1, Math.round(w)), Math.max(1, Math.round(h))));
    return id;
  }
  surface_exists(surf: number): boolean { return this._surfaces.has(surf); }
  surface_free(surf: number): void { this._surfaces.delete(surf); }
  surface_set_target(surf: number): void {
    const canvas = this._surfaces.get(surf);
    if (!canvas) return;
    this._surfaceCtxStack.push(this._gfx.ctx);
    this._surfaceIdStack.push(surf);
    (this._gfx as any).ctx = canvas.getContext("2d")!;
  }
  surface_reset_target(): void {
    const prev = this._surfaceCtxStack.pop();
    this._surfaceIdStack.pop();
    if (prev !== undefined) (this._gfx as any).ctx = prev;
  }
  surface_get_width(surf: number): number { return this._surfaces.get(surf)?.width ?? 0; }
  surface_get_height(surf: number): number { return this._surfaces.get(surf)?.height ?? 0; }
  surface_getpixel(surf: number, x: number, y: number): number {
    const canvas = this._surfaces.get(surf);
    if (!canvas) return 0;
    const ctx = canvas.getContext("2d")!;
    const px = ctx.getImageData(Math.round(x), Math.round(y), 1, 1).data;
    return px[0]! | (px[1]! << 8) | (px[2]! << 16);  // GML BGR color
  }
  draw_surface(surf: number, x: number, y: number): void {
    const canvas = this._surfaces.get(surf);
    if (canvas) this._gfx.ctx.drawImage(canvas, x, y);
  }
  draw_surface_ext(surf: number, x: number, y: number, xs: number, ys: number, rot: number, _col: number, alpha: number): void {
    const canvas = this._surfaces.get(surf);
    if (!canvas) return;
    const ctx = this._gfx.ctx;
    ctx.save();
    ctx.translate(x, y);
    if (xs !== 1 || ys !== 1) ctx.scale(xs, ys);
    if (rot !== 0) ctx.rotate(-rot * Math.PI / 180);
    if (alpha !== 1) ctx.globalAlpha = alpha;
    ctx.drawImage(canvas, 0, 0);
    ctx.restore();
  }
  draw_surface_part(surf: number, left: number, top: number, w: number, h: number, x: number, y: number): void {
    const canvas = this._surfaces.get(surf);
    if (canvas) this._gfx.ctx.drawImage(canvas, left, top, w, h, x, y, w, h);
  }

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

  // ---- Audio API ----

  audio_play_sound(sound: number, _priority: number, loop: boolean, gain = 1, offset = 0, pitch = 1): number {
    return audioPlay(this._audio, sound, 0, loop, gain, pitch, 0, offset);
  }
  audio_play_sound_at(sound: number, _x: number, _y: number, _z: number, _falloff: number, _min: number, _max: number, _priority: number, loop: boolean): number {
    return audioPlay(this._audio, sound, 0, loop, 1, 1, 0, 0);
  }
  audio_stop_sound(handle: number): void { audioStop(this._audio, handle); }
  audio_stop_all(): void { audioStopAll(this._audio); }
  audio_pause_sound(handle: number): void { audioPause(this._audio, handle); }
  audio_resume_sound(handle: number): void { audioResume(this._audio, handle); }
  audio_resume_all(): void { audioResumeAll(this._audio); }
  audio_is_playing(handle: number): boolean { return audioIsPlaying(this._audio, handle); }
  audio_is_paused(handle: number): boolean { return audioIsPaused(this._audio, handle); }
  audio_sound_gain(handle: number, gain: number, timeMs: number): void { audioSetGain(this._audio, handle, gain, timeMs); }
  audio_sound_get_gain(handle: number): number { return audioGetGain(this._audio, handle); }
  audio_sound_pitch(handle: number, pitch: number): void { audioSetPitch(this._audio, handle, pitch); }
  audio_sound_get_pitch(handle: number): number { return audioGetPitch(this._audio, handle); }
  audio_sound_set_pan(handle: number, pan: number): void { audioSetPan(this._audio, handle, pan); }
  audio_sound_get_pan(handle: number): number { return audioGetPan(this._audio, handle); }
  audio_master_gain(gain: number): void { audioSetMasterGain(this._audio, gain); }
  audio_sound_length(sound: number): number { return audioSoundLength(this._audio, sound); }
  audio_sound_set_track_position(handle: number, pos: number): void { audioSetPosition(this._audio, handle, pos); }
  audio_sound_get_track_position(handle: number): number { return audioGetPosition(this._audio, handle); }
  audio_exists(sound: number): boolean { return sound >= 0 && sound < this.sounds.length && this.sounds[sound]!.url !== ""; }
  audio_get_name(sound: number): string { return this.sounds[sound]?.name ?? ""; }
  audio_group_load(_group: number): void { /* no-op — all audio loaded at startup */ }
  audio_group_stop_all(_group: number): void { audioStopAll(this._audio); }
  audio_group_set_gain(_group: number, gain: number, timeMs: number): void { audioSetNodeParam(this._audio, 0, "gain", gain, timeMs); }

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
  // Previous-frame gamepad button state for pressed/released detection
  private _gamepadPrev = new Map<number, boolean[]>();
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
  distance_to_object(classIndex: number): number {
    if (!this._self) return 0;
    const sx = this._self.x, sy = this._self.y;
    const clazz = this.classes[classIndex];
    if (!clazz) return 0;
    let best = Infinity;
    for (const inst of this.roomVariables) {
      if (inst === this._self) continue;
      if (!(inst instanceof clazz)) continue;
      const d = Math.hypot(inst.x - sx, inst.y - sy);
      if (d < best) best = d;
    }
    return best === Infinity ? 0 : best;
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
  // Layers are not parsed from the data file — layer_get_id returns a stable hash-based handle.
  // Background element IDs use a derived handle (layerId | 0x80000000 pattern).
  layer_background_get_id(layer: any): number { return (typeof layer === "number" ? layer : this.layer_get_id(String(layer))) | 0x80000000; }
  layer_background_get_sprite(id: number): number { return this._layerBackgroundSprites.get(id) ?? -1; }
  layer_background_set_sprite(id: number, spr: number): void { this._layerBackgroundSprites.set(id, spr); }

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
  draw_primitive_begin(kind: number): void { this._primKind = kind; this._primVerts = []; }
  draw_primitive_begin_texture(kind: number, _tex: number): void { this._primKind = kind; this._primVerts = []; }
  draw_vertex(x: number, y: number): void { this._primVerts.push({ x, y }); }
  draw_vertex_texture(x: number, y: number, _xtex: number, _ytex: number): void { this._primVerts.push({ x, y }); }
  draw_primitive_end(): void {
    const verts = this._primVerts;
    if (verts.length === 0) return;
    const ctx = this._gfx.ctx;
    const css = gmlColorToCss(this._draw.config.color);
    // GML primitive kinds: 1=pr_pointlist, 2=pr_linelist, 3=pr_linestrip, 4=pr_trianglelist, 5=pr_trianglestrip, 6=pr_trianglefan
    if (this._primKind === 1) {
      // Point list: draw 1×1 rect at each vertex
      ctx.fillStyle = css;
      for (const v of verts) ctx.fillRect(v.x, v.y, 1, 1);
    } else if (this._primKind === 2) {
      // Line list: pairs of vertices
      ctx.strokeStyle = css;
      ctx.beginPath();
      for (let i = 0; i + 1 < verts.length; i += 2) {
        ctx.moveTo(verts[i]!.x, verts[i]!.y); ctx.lineTo(verts[i + 1]!.x, verts[i + 1]!.y);
      }
      ctx.stroke();
    } else if (this._primKind === 3) {
      // Line strip
      ctx.strokeStyle = css;
      ctx.beginPath();
      ctx.moveTo(verts[0]!.x, verts[0]!.y);
      for (let i = 1; i < verts.length; i++) ctx.lineTo(verts[i]!.x, verts[i]!.y);
      ctx.stroke();
    } else if (this._primKind === 4) {
      // Triangle list
      ctx.fillStyle = css;
      for (let i = 0; i + 2 < verts.length; i += 3) {
        ctx.beginPath();
        ctx.moveTo(verts[i]!.x, verts[i]!.y);
        ctx.lineTo(verts[i + 1]!.x, verts[i + 1]!.y);
        ctx.lineTo(verts[i + 2]!.x, verts[i + 2]!.y);
        ctx.closePath(); ctx.fill();
      }
    } else if (this._primKind === 5) {
      // Triangle strip
      ctx.fillStyle = css;
      for (let i = 0; i + 2 < verts.length; i++) {
        ctx.beginPath();
        const [a, b, c] = i % 2 === 0
          ? [verts[i]!, verts[i + 1]!, verts[i + 2]!]
          : [verts[i + 1]!, verts[i]!, verts[i + 2]!];
        ctx.moveTo(a.x, a.y); ctx.lineTo(b.x, b.y); ctx.lineTo(c.x, c.y);
        ctx.closePath(); ctx.fill();
      }
    } else if (this._primKind === 6) {
      // Triangle fan
      ctx.fillStyle = css;
      const origin = verts[0]!;
      for (let i = 1; i + 1 < verts.length; i++) {
        ctx.beginPath();
        ctx.moveTo(origin.x, origin.y);
        ctx.lineTo(verts[i]!.x, verts[i]!.y);
        ctx.lineTo(verts[i + 1]!.x, verts[i + 1]!.y);
        ctx.closePath(); ctx.fill();
      }
    }
    this._primVerts = [];
  }
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
  instance_activate_object(classIndex: number): void {
    const clazz = this.classes[classIndex];
    if (!clazz) return;
    for (const inst of this._deactivatedInstances) {
      if (inst instanceof clazz) this._deactivatedInstances.delete(inst);
    }
  }
  instance_deactivate_object(classIndex: number, notme: boolean = false): void {
    const clazz = this.classes[classIndex];
    if (!clazz) return;
    for (const inst of this.roomVariables) {
      if (notme && inst === this._self) continue;
      if (inst instanceof clazz) this._deactivatedInstances.add(inst);
    }
  }
  instance_deactivate_layer(_layer: any): void { /* no-op: layer data not available */ }
  instance_deactivate_region(x1: number, y1: number, x2: number, y2: number, inside: boolean, notme: boolean = false): void {
    for (const inst of this.roomVariables) {
      if (notme && inst === this._self) continue;
      const inRegion = inst.x >= x1 && inst.x <= x2 && inst.y >= y1 && inst.y <= y2;
      if (inRegion === inside) this._deactivatedInstances.add(inst);
    }
  }

  // ---- Variable instance helpers ----
  variable_instance_get(inst: any, name: string): any { return inst?.[name]; }
  variable_instance_exists(inst: any, name: string): boolean { return inst != null && name in Object(inst); }

  // ---- Surface API (stubs — requires WebGL offscreen rendering) ----
  surface_resize(surf: number, w: number, h: number): void {
    const canvas = this._surfaces.get(surf);
    if (!canvas) return;
    const newCanvas = new OffscreenCanvas(Math.max(1, Math.round(w)), Math.max(1, Math.round(h)));
    newCanvas.getContext("2d")!.drawImage(canvas, 0, 0);
    this._surfaces.set(surf, newCanvas);
    // Update active ctx if this surface is on top of the stack
    if (this._surfaceIdStack[this._surfaceIdStack.length - 1] === surf) {
      (this._gfx as any).ctx = newCanvas.getContext("2d")!;
    }
  }
  surface_get_target(): number {
    return this._surfaceIdStack[this._surfaceIdStack.length - 1] ?? -1;
  }

  // ---- Misc ----
  show_error(str: string, _abort: boolean): void { console.error("GML show_error:", str); }
  event_user(n: number): void {
    if (!this._self) return;
    const method = (this._self as any)["user" + n];
    if (method && method !== noop) { const prev = this._self; method.call(prev); this._self = prev; }
  }
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

  // ---- Buffer API ----
  // GML buffer kind constants: 0=fixed, 1=grow, 2=wrap, 3=fast(grow), 4=vbuffer
  // GML buffer type constants: 1=u8,2=s8,3=u16,4=s16,5=u32,6=s32,7=f32,8=f64,13=bool,14=string,15=text,16=u64

  buffer_create(size: number, kind: number, alignment: number): number {
    const id = this._nextBufferId++;
    this._buffers.set(id, {
      data: new Uint8Array(Math.max(size, 1)),
      pos: 0,
      kind,
      align: Math.max(1, alignment),
    });
    return id;
  }

  _bufferAlignPos(buf: { pos: number; align: number }, typeSize: number): void {
    const align = Math.min(buf.align, typeSize);
    if (align > 1) {
      const rem = buf.pos % align;
      if (rem !== 0) buf.pos += align - rem;
    }
  }

  _bufferGrow(buf: { data: Uint8Array; pos: number; kind: number }, needed: number): void {
    if (buf.kind === 0 || buf.kind === 4) return;  // fixed/vbuffer: no grow
    if (buf.data.length >= needed) return;
    let newSize = Math.max(buf.data.length * 2, needed);
    const newData = new Uint8Array(newSize);
    newData.set(buf.data);
    buf.data = newData;
  }

  buffer_write(bufId: number, type: number, value: any): void {
    const buf = this._buffers.get(bufId);
    if (!buf) return;
    if (type === 14 || type === 15) {  // string types
      const str = String(value ?? "");
      const encoded = new TextEncoder().encode(str);
      const needed = buf.pos + encoded.length + (type === 14 ? 1 : 0);
      this._bufferGrow(buf, needed);
      if (buf.pos + encoded.length > buf.data.length) return;
      buf.data.set(encoded, buf.pos);
      buf.pos += encoded.length;
      if (type === 14 && buf.pos < buf.data.length) { buf.data[buf.pos++] = 0; }
      return;
    }
    const sz = bufferTypeSize(type);
    if (sz === 0) return;
    this._bufferAlignPos(buf, sz);
    this._bufferGrow(buf, buf.pos + sz);
    if (buf.pos + sz > buf.data.length) return;  // fixed buffer full
    const view = new DataView(buf.data.buffer, buf.data.byteOffset, buf.data.byteLength);
    switch (type) {
      case 1:  view.setUint8(buf.pos, (value as number) & 0xFF); break;
      case 2:  view.setInt8(buf.pos, value as number); break;
      case 3:  view.setUint16(buf.pos, (value as number) & 0xFFFF, true); break;
      case 4:  view.setInt16(buf.pos, value as number, true); break;
      case 5:  view.setUint32(buf.pos, (value as number) >>> 0, true); break;
      case 6:  view.setInt32(buf.pos, (value as number) | 0, true); break;
      case 7:  view.setFloat32(buf.pos, value as number, true); break;
      case 8:  view.setFloat64(buf.pos, value as number, true); break;
      case 13: view.setUint8(buf.pos, value ? 1 : 0); break;
      case 16: try { view.setBigUint64(buf.pos, BigInt(Math.trunc(value as number)), true); } catch { view.setUint32(buf.pos, (value as number) >>> 0, true); } break;
    }
    buf.pos += sz;
  }

  buffer_read(bufId: number, type: number): any {
    const buf = this._buffers.get(bufId);
    if (!buf) return 0;
    if (typeof type !== 'number') return 0;  // guard against non-type args
    if (type === 14) {  // buffer_string: read until null terminator
      let end = buf.pos;
      while (end < buf.data.length && buf.data[end] !== 0) end++;
      const str = new TextDecoder().decode(buf.data.subarray(buf.pos, end));
      buf.pos = Math.min(end + 1, buf.data.length);
      return str;
    }
    if (type === 15) {  // buffer_text: read remaining bytes
      const str = new TextDecoder().decode(buf.data.subarray(buf.pos));
      buf.pos = buf.data.length;
      return str;
    }
    const sz = bufferTypeSize(type);
    if (sz === 0) return 0;
    this._bufferAlignPos(buf, sz);
    if (buf.pos + sz > buf.data.length) return 0;
    const view = new DataView(buf.data.buffer, buf.data.byteOffset, buf.data.byteLength);
    let result: any;
    switch (type) {
      case 1:  result = view.getUint8(buf.pos); break;
      case 2:  result = view.getInt8(buf.pos); break;
      case 3:  result = view.getUint16(buf.pos, true); break;
      case 4:  result = view.getInt16(buf.pos, true); break;
      case 5:  result = view.getUint32(buf.pos, true); break;
      case 6:  result = view.getInt32(buf.pos, true); break;
      case 7:  result = view.getFloat32(buf.pos, true); break;
      case 8:  result = view.getFloat64(buf.pos, true); break;
      case 13: result = view.getUint8(buf.pos) !== 0; break;
      case 16: try { result = Number(view.getBigUint64(buf.pos, true)); } catch { result = view.getUint32(buf.pos, true); } break;
      default: return 0;
    }
    buf.pos += sz;
    return result;
  }

  buffer_delete(bufId: number): void {
    this._buffers.delete(bufId);
  }

  // ---- File API (localStorage-backed text file simulation) ----
  private _fileKey(path: string): string { return `gml_file:${path}`; }
  file_text_write_string(file: number, str: string): void {
    const f = this._textFiles.get(file); if (f && f.mode === 'w') f.content += str;
  }
  file_text_writeln(file: number): void {
    const f = this._textFiles.get(file); if (f && f.mode === 'w') f.content += "\n";
  }
  file_text_close(file: number): void {
    const f = this._textFiles.get(file);
    if (f && f.mode === 'w') {
      try { localStorage.setItem(this._fileKey(f.path), f.content); } catch { /* storage full */ }
    }
    this._textFiles.delete(file);
  }
  file_exists(path: string): boolean {
    return localStorage.getItem(this._fileKey(path)) !== null;
  }

  // ---- Steam API (platform-provided or no-op) ----

  steam_current_game_language(): string {
    const code = (navigator.language ?? "en").split("-")[0]!.toLowerCase();
    const map: Record<string, string> = {
      en: "english", fr: "french", de: "german", es: "spanish",
      pt: "portuguese", it: "italian", nl: "dutch", ru: "russian",
      ja: "japanese", ko: "korean", zh: "schinese", pl: "polish",
      cs: "czech", da: "danish", fi: "finnish", nb: "norwegian",
      sv: "swedish", ro: "romanian", tr: "turkish", hu: "hungarian",
      sk: "slovak", uk: "ukrainian", bg: "bulgarian", el: "greek",
      th: "thai", vi: "vietnamese",
    };
    return map[code] ?? "english";
  }
  steam_inventory_result_destroy(_result: number): void { throw new Error("steam_inventory_result_destroy: not yet implemented"); }
  steam_ugc_get_item_install_info(_id: number, _arr: any): boolean { throw new Error("steam_ugc_get_item_install_info: not yet implemented"); }
  steam_ugc_get_subscribed_items(_arr: any): number { throw new Error("steam_ugc_get_subscribed_items: not yet implemented"); }
  steam_lobby_get_lobby_id(): number { throw new Error("steam_lobby_get_lobby_id: not yet implemented"); }
  steam_lobby_join_id(_id: number): void { throw new Error("steam_lobby_join_id: not yet implemented"); }
  steam_lobby_set_data(_key: string, _val: string, _lobby?: number): void { throw new Error("steam_lobby_set_data: not yet implemented"); }
  steam_lobby_get_data(_key: string, _lobby?: number): string { throw new Error("steam_lobby_get_data: not yet implemented"); }
  steam_activate_overlay_store(_app: number): void { /* no-op */ }
  steam_input_get_digital_action_handle(_name: string): number { return 0; }
  steam_is_cloud_enabled_for_account(): boolean { return false; }
  steam_inventory_result_get_items(_result: number, _arr?: any[]): any[] { throw new Error("steam_inventory_result_get_items: not yet implemented"); }
  steam_lobby_get_member_id(_index: number, _lobby?: number): number { throw new Error("steam_lobby_get_member_id: not yet implemented"); }
  steam_input_get_action_set_handle(_name: string): number { return 0; }
  steam_get_stat_float(_name: string): number { return parseFloat(loadItem(this._persistence, this._steamStatKey(_name)) ?? "0"); }
  steam_get_global_stat_int(_name: string): number { return 0; }
  steam_get_user_account_id(): number { return 0; }
  steam_image_get_rgba(_image: number, _buf: number, _size: number): boolean { return false; }
  steam_input_enable_device_callbacks(): void { /* no-op */ }
  steam_lobby_get_chat_message_text(_index: number, _lobby?: number): string { throw new Error("steam_lobby_get_chat_message_text: not yet implemented"); }
  steam_lobby_get_owner_id(_lobby?: number): number { throw new Error("steam_lobby_get_owner_id: not yet implemented"); }
  steam_request_friend_rich_presence(_steamid: number): void { /* no-op */ }
  steam_ugc_get_item_update_info(_handle: number, _arr: any): boolean { throw new Error("steam_ugc_get_item_update_info: not yet implemented"); }
  steam_ugc_submit_item_update(_handle: number, _note: string): void { throw new Error("steam_ugc_submit_item_update: not yet implemented"); }

  // ---- More collision ----
  collision_point(_x: number, _y: number, _classIndex: number, _prec: boolean, _notme: boolean): any { throw new Error("collision_point: requires collision system implementation"); }
  collision_circle(_x: number, _y: number, _r: number, _classIndex: number, _prec: boolean, _notme: boolean): any { throw new Error("collision_circle: requires collision system implementation"); }
  collision_ellipse(_x1: number, _y1: number, _x2: number, _y2: number, _classIndex: number, _prec: boolean, _notme: boolean): any { throw new Error("collision_ellipse: requires collision system implementation"); }
  collision_line_list(_x1: number, _y1: number, _x2: number, _y2: number, _classIndex: number, _prec: boolean, _notme: boolean, _list: number, _ordered: number | boolean = 0): number { throw new Error("collision_line_list: requires collision system implementation"); }
  collision_rectangle_list(_x1: number, _y1: number, _x2: number, _y2: number, _classIndex: number, _prec: boolean, _notme: boolean, _list: number, _ordered: boolean = false): number { throw new Error("collision_rectangle_list: requires collision system implementation"); }
  distance_to_point(x: number, y: number): number {
    if (!this._self) return 0;
    return Math.hypot(x - this._self.x, y - this._self.y);
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
  draw_sprite_pos(spr: number, sub: number, x1: number, y1: number, x2: number, y2: number, x3: number, y3: number, x4: number, y4: number, alpha: number): void {
    // GML corner order: top-left, top-right, bottom-right, bottom-left
    const sprite = this.sprites[spr]; if (!sprite) return;
    const texIdx = sprite.textures[sub] ?? sprite.textures[0]; if (texIdx === undefined) return;
    const tex = this.textures[texIdx]; if (!tex) return;
    const sheet = this.textureSheets[tex.sheetId]; if (!sheet) return;
    const ctx = this._gfx.ctx;
    ctx.save();
    if (alpha !== this._draw.alpha) ctx.globalAlpha = alpha;
    // Use a canvas transform to map from sprite rect to four corners (top-left→top-right→bottom-right→bottom-left).
    // We approximate by computing the affine transform that maps the sprite rectangle's corners to x1/x2/x3/x4.
    const sw = tex.dest.w, sh = tex.dest.h;
    // Top-left corner anchor: translate to (x1, y1), scale by (x2-x1)/sw, (y4-y1)/sh.
    const dw = Math.hypot(x2 - x1, y2 - y1);
    const dh = Math.hypot(x4 - x1, y4 - y1);
    const angleX = Math.atan2(y2 - y1, x2 - x1);
    ctx.translate(x1, y1);
    ctx.rotate(angleX);
    ctx.scale(dw / sw, dh / sh);
    ctx.drawImage(sheet, tex.src.x, tex.src.y, tex.src.w, tex.src.h, 0, 0, sw, sh);
    ctx.restore();
  }
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
  draw_surface_stretched_ext(surf: number, x: number, y: number, w: number, h: number, _col: number, alpha: number): void {
    const canvas = this._surfaces.get(surf);
    if (!canvas) return;
    const ctx = this._gfx.ctx;
    if (alpha !== 1) { ctx.save(); ctx.globalAlpha = alpha; }
    ctx.drawImage(canvas, x, y, w, h);
    if (alpha !== 1) ctx.restore();
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
  file_text_open_read(path: string): number {
    const content = localStorage.getItem(this._fileKey(path)) ?? "";
    const id = this._nextTextFileId++;
    this._textFiles.set(id, { path, content, pos: 0, mode: 'r' });
    return id;
  }
  file_text_read_string(file: number): string {
    const f = this._textFiles.get(file); if (!f || f.mode !== 'r') return "";
    const nl = f.content.indexOf('\n', f.pos);
    const line = nl === -1 ? f.content.slice(f.pos) : f.content.slice(f.pos, nl);
    f.pos = nl === -1 ? f.content.length : nl + 1;
    return line;
  }
  file_text_eof(file: number): boolean {
    const f = this._textFiles.get(file); return !f || f.pos >= f.content.length;
  }
  file_delete(path: string): void { localStorage.removeItem(this._fileKey(path)); }
  file_find_first(_mask: string, _attr: number): string { return ""; /* no filesystem enumeration in browser */ }
  file_find_next(): string { return ""; }
  file_find_close(): void { /* no-op */ }

  // ---- Directory ----
  directory_create(_path: string): void { /* no-op: no filesystem access in browser */ }
  directory_exists(_path: string): boolean { return false; }


  // ---- Buffer extras ----
  buffer_base64_decode(str: string): number {
    const binary = atob(str);
    const data = new Uint8Array(binary.length);
    for (let i = 0; i < binary.length; i++) data[i] = binary.charCodeAt(i);
    const id = this._nextBufferId++;
    this._buffers.set(id, { data, pos: 0, kind: 0, align: 1 });
    return id;
  }
  buffer_base64_encode(bufId: number, offset: number, size: number): string {
    const buf = this._buffers.get(bufId);
    if (!buf) return "";
    const slice = buf.data.subarray(offset, offset + size);
    let binary = "";
    for (const byte of slice) binary += String.fromCharCode(byte);
    return btoa(binary);
  }
  buffer_load(_filename: string, _buf: number = -1, _offset: number = 0, _size: number = 0): number { throw new Error("buffer_load: not yet implemented"); }
  buffer_load_async(_path: string, _buf: number, _offset: number, _size: number): number { throw new Error("buffer_load_async: not yet implemented"); }
  buffer_save_async(_buf: number, _path: string, _offset: number, _size: number): number { throw new Error("buffer_save_async: not yet implemented"); }
  buffer_set_surface(bufId: number, surf: number, offset: number): void {
    // Copy surface RGBA pixels into buffer at given offset (BGRA → RGBA conversion).
    const canvas = this._surfaces.get(surf);
    if (!canvas) return;
    const buf = this._buffers.get(bufId);
    if (!buf) return;
    const ctx = canvas.getContext("2d")!;
    const px = ctx.getImageData(0, 0, canvas.width, canvas.height).data;
    const needed = offset + px.length;
    this._bufferGrow(buf, needed);
    if (needed <= buf.data.length) {
      for (let i = 0; i < px.length; i += 4) {
        buf.data[offset + i]     = px[i]!;      // R
        buf.data[offset + i + 1] = px[i + 1]!;  // G
        buf.data[offset + i + 2] = px[i + 2]!;  // B
        buf.data[offset + i + 3] = px[i + 3]!;  // A
      }
    }
  }
  buffer_async_group_begin(_groupname: string): void { /* no-op — async buffer groups not needed in browser */ }
  buffer_async_group_end(): number { return 0; }

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
  surface_copy(dest: number, x: number, y: number, src: number): void {
    const srcCanvas = this._surfaces.get(src);
    const destCanvas = this._surfaces.get(dest);
    if (srcCanvas && destCanvas) destCanvas.getContext("2d")!.drawImage(srcCanvas, x, y);
  }

  // ---- Tags / misc ----
  tag_get_assets(_tag: string, _kind?: number): any[] { return []; /* no tag data available */ }
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
  steam_activate_overlay(_type: string): void { /* no-op — no Steam overlay in browser */ }
  steam_activate_overlay_user(_type: string, _steamid: number): void { /* no-op */ }
  steam_get_app_id(): number { return 0; }
  steam_get_user_persona_name_sync(_steamid?: number): string { return ""; }
  steam_get_stat_int(_name: string): number { return parseInt(loadItem(this._persistence, this._steamStatKey(_name)) ?? "0", 10); }
  steam_get_global_stat_history_int(_name: string, _days?: number): number { return 0; }
  steam_is_overlay_activated(): boolean { return false; }
  steam_image_get_size(_image: number): [number, number] { return [0, 0]; }
  steam_lobby_get_member_count(_lobby?: number): number { throw new Error("steam_lobby_get_member_count: not yet implemented"); }
  steam_lobby_list_add_string_filter(_key: string, _val: string, _type: number): void { throw new Error("steam_lobby_list_add_string_filter: not yet implemented"); }
  steam_lobby_get_chat_message_data(_msg: number, _buf: number, _lobby?: number): number { throw new Error("steam_lobby_get_chat_message_data: not yet implemented"); }
  steam_ugc_subscribe_item(_id: number): void { throw new Error("steam_ugc_subscribe_item: not yet implemented"); }
  steam_input_run_frame(): void { /* no-op */ }
  steam_file_write(_path: string, _data: string, _length?: number): boolean {
    const data = _length !== undefined ? _data.slice(0, _length) : _data;
    save(this._persistence, this._steamCloudKey(_path), data);
    this._steamCloudAddToIndex(_path);
    return true;
  }
  steam_file_exists(_path: string): boolean { return loadItem(this._persistence, this._steamCloudKey(_path)) !== null; }
  /** UDS (User Data System) is PS4-specific telemetry — no browser equivalent. */
  psn_post_uds_event(_evtype: number, ..._args: any[]): void { /* no-op — PS4 telemetry, no browser equivalent */ }

  // ---- More file/buffer API ----
  file_text_open_write(path: string): number {
    const id = this._nextTextFileId++;
    this._textFiles.set(id, { path, content: "", pos: 0, mode: 'w' });
    return id;
  }
  file_text_open_append(path: string): number {
    const existing = localStorage.getItem(this._fileKey(path)) ?? "";
    const id = this._nextTextFileId++;
    this._textFiles.set(id, { path, content: existing, pos: existing.length, mode: 'w' });
    return id;
  }
  buffer_seek(bufId: number, base: number, offset: number): void {
    const buf = this._buffers.get(bufId);
    if (!buf) return;
    // base: 0=buffer_seek_start, 1=buffer_seek_relative, 2=buffer_seek_end
    switch (base) {
      case 0: buf.pos = offset; break;
      case 1: buf.pos += offset; break;
      case 2: buf.pos = buf.data.length - offset; break;
    }
    buf.pos = Math.max(0, Math.min(buf.pos, buf.data.length));
  }
  buffer_async_group_option(_option: string, _value: any): void { /* no-op — async buffer groups not needed in browser */ }

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
    // Returns a texture handle for use with draw_primitive_begin_texture.
    // In our 2D canvas model, texture primitives are not supported.
    return -1;
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
  room_set_persistent(_room: number, _persistent: boolean): void { /* no-op: persistence state not tracked per-room */ }
  event_perform(type: number, n: number): void {
    // type 10 = user event; type 7 = alarm — dispatch on _self if available
    if (!this._self) return;
    if (type === 10) { const m = (this._self as any)["user" + n]; if (m && m !== noop) m.call(this._self); }
    else if (type === 7) { const m = (this._self as any)["alarm" + n]; if (m && m !== noop) m.call(this._self); }
  }
  is_debug_overlay_open(): boolean { return false; }
  path_exists(_path: number): boolean { return false; }
  path_delete(_path: number): void { /* no-op */ }
  part_type_gravity(_part: number, _gx: number, _gy: number): void { /* no-op */ }
  part_system_depth(_syst: number, _depth: number): void { /* no-op */ }
  layer_background_visible(_bg: number, _visible: boolean): void { /* no-op */ }
  layer_sequence_create(_layer: any, _x: number, _y: number, _seq: number): number { return -1; }
  layer_sequence_is_finished(_seq: number): boolean { return true; }
  string_repeat(str: string, count: number): string { return str.repeat(count); }

  // ---- Gamepad API (stubs) ----
  gamepad_get_device_count(): number { return navigator.getGamepads().filter(g => g != null).length; }
  gamepad_axis_value(device: number, axis: number): number {
    const raw = navigator.getGamepads()[device]?.axes[axis] ?? 0;
    const dz = this._gamepadDeadzones.get(device) ?? 0;
    return Math.abs(raw) < dz ? 0 : raw;
  }
  gamepad_button_check_pressed(device: number, button: number): boolean {
    const curr = navigator.getGamepads()[device]?.buttons[button]?.pressed ?? false;
    const prev = this._gamepadPrev.get(device)?.[button] ?? false;
    return curr && !prev;
  }
  gamepad_button_check_released(device: number, button: number): boolean {
    const curr = navigator.getGamepads()[device]?.buttons[button]?.pressed ?? false;
    const prev = this._gamepadPrev.get(device)?.[button] ?? false;
    return !curr && prev;
  }
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
  draw_surface_stretched(surf: number, x: number, y: number, w: number, h: number): void {
    const canvas = this._surfaces.get(surf);
    if (canvas) this._gfx.ctx.drawImage(canvas, x, y, w, h);
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
  draw_path(_path: number, _x: number, _y: number, _absolute: boolean): void { /* no-op: path system not implemented */ }

  // ---- More buffer functions ----
  buffer_get_size(bufId: number): number {
    return this._buffers.get(bufId)?.data.length ?? 0;
  }
  buffer_exists(bufId: number): boolean { return this._buffers.has(bufId); }

  // ---- Layer extras ----
  layer_get_x(_layer: any): number { return 0; }
  layer_get_y(_layer: any): number { return 0; }
  layer_depth(_layer: any, _depth?: number): number { return 0; }
  layer_sequence_destroy(_seq: number): void { /* no-op */ }

  // ---- More collision ----
  collision_ellipse_list(_x1: number, _y1: number, _x2: number, _y2: number, _classIndex: number, _prec: boolean, _notme: boolean, _list: number, _ordered: boolean = false): number { throw new Error("collision_ellipse_list: requires collision system implementation"); }

  // ---- Instance change ----
  instance_change(_classIndex: number, _performEvents: boolean): void { throw new Error("instance_change: not yet implemented"); }

  // ---- More Steam ----
  steam_initialised(): boolean { return false; }
  steam_indicate_achievement_progress(_name: string, _cur: number, _max: number): void { /* no-op — progress display only */ }
  steam_get_user_steam_id(): number { return 0; }
  steam_get_persona_name(): string { return ""; }
  steam_set_achievement(_name: string): void {
    const set = this._steamAchSet();
    set.add(_name);
    this._steamAchSave(set);
  }
  steam_request_global_achievement_percentages(): void { /* no-op */ }
  steam_get_achievement(_name: string): boolean { return this._steamAchSet().has(_name); }
  steam_store_stats(): void { /* no-op — stats are already persisted to localStorage immediately */ }
  steam_set_stat_int(_name: string, _val: number): void { save(this._persistence, this._steamStatKey(_name), String(Math.trunc(_val))); }
  steam_net_packet_get_sender_id(): number { throw new Error("steam_net_packet_get_sender_id: not yet implemented"); }
  steam_is_cloud_enabled_for_app(): boolean { return false; }
  steam_ugc_create_query_user(_account_id: number, _list_type: number, _matching_type: number, _sort_order: number, _creator_app_id?: number, _consumer_app_id?: number, _page?: number): number { throw new Error("steam_ugc_create_query_user: not yet implemented"); }

  // ---- Misc ----
  show_message_async(_str: string): void { console.log("GML show_message_async:", _str); }
  sprite_get_name(_spr: number): string { return `sprite_${_spr}`; }
  room_exists(_room: number): boolean { return _room >= 0 && _room < this._roomInstances.length; }
  string_byte_at(str: string, n: number): number { return str.charCodeAt(n - 1) || 0; }

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


  // ---- Clipboard ----
  clipboard_set_text(str: string): void { this._clipboardCache = str; navigator.clipboard?.writeText(str); }
  clipboard_get_text(): string {
    // Clipboard API is async; return last known value cached by clipboard_set_text.
    // Games that call get_text after set_text in the same frame will get the correct value.
    return this._clipboardCache;
  }
  private _clipboardCache = "";

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
  game_end(): never {
    // Stop the game loop and close the window (if allowed).
    clearTimeout(this._drawHandle);
    this._drawHandle = 0;
    try { window.close(); } catch { /* may not be permitted */ }
    // Halt execution: throw a special marker that won't be caught as a game error.
    throw Object.assign(new Error("game_end"), { isGameEnd: true });
  }

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
  ds_map_secure_save(map: number, filename: string): void {
    // "Secure" save in GML just writes an encoded file — use same localStorage approach as ds_map_write.
    const m = this._dsMaps.get(map); if (!m) return;
    const json = JSON.stringify(Object.fromEntries(m));
    try { localStorage.setItem(this._fileKey(filename), json); } catch { /* storage full */ }
  }
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
  sprite_duplicate(spr: number): number {
    const src = this.sprites[spr]; if (!src) return -1;
    // Shallow-copy the sprite entry and append to the sprites array.
    const copy = { ...src, origin: { ...src.origin }, bbox: src.bbox ? { ...src.bbox } : src.bbox, textures: [...src.textures] };
    const newIdx = this.sprites.length;
    this.sprites.push(copy);
    return newIdx;
  }
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
  layer_background_get_index(bg: number): number { return this._layerBackgroundSprites.get(bg) ?? -1; }

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

  // ---- Vertex buffer (2D Canvas approximation — no WebGL) ----
  vertex_create_buffer(): number {
    const id = this._nextVbufId++;
    this._vbufs.set(id, { verts: [], recording: false });
    return id;
  }
  vertex_delete_buffer(buf: number): void { this._vbufs.delete(buf); }
  vertex_begin(buf: number, _format: number): void {
    const b = this._vbufs.get(buf); if (!b) return;
    b.verts = []; b.recording = true; this._vbufCurrent = buf;
    this._draw._vbufColor = this._draw.config.color;
    this._draw._vbufAlpha = this._draw.alpha;
  }
  vertex_end(buf: number): void {
    const b = this._vbufs.get(buf); if (!b) return;
    b.recording = false; if (this._vbufCurrent === buf) this._vbufCurrent = -1;
  }
  vertex_submit(buf: number, prim: number, _tex: number): void {
    const b = this._vbufs.get(buf); if (!b || b.verts.length === 0) return;
    // Reuse draw_primitive machinery
    this.draw_primitive_begin(prim);
    for (const v of b.verts) this._primVerts.push({ x: v.x, y: v.y });
    this.draw_primitive_end();
  }
  vertex_format_begin(): void { /* no-op: format not used in Canvas 2D */ }
  vertex_format_end(): number { return ++this._vbufFormatDummy; }
  vertex_format_add_position_3d(): void { /* no-op */ }
  vertex_format_add_position(): void { /* no-op */ }
  vertex_format_add_color(): void { /* no-op */ }
  vertex_format_add_colour(): void { /* no-op */ }
  vertex_format_add_texcoord(): void { /* no-op */ }
  vertex_format_add_normal(): void { /* no-op */ }
  vertex_format_add_custom(_type: number, _usage: number): void { /* no-op */ }
  vertex_format_delete(_fmt: number): void { /* no-op */ }
  vertex_position(buf: number, x: number, y: number, _z: number = 0): void {
    const b = this._vbufs.get(buf); if (!b || !b.recording) return;
    b.verts.push({ x, y, col: this._draw._vbufColor, alpha: this._draw._vbufAlpha });
  }
  vertex_colour(buf: number, col: number, alpha: number): void {
    const b = this._vbufs.get(buf); if (!b || !b.recording) return;
    this._draw._vbufColor = col; this._draw._vbufAlpha = alpha;
  }
  vertex_color(buf: number, col: number, alpha: number): void { this.vertex_colour(buf, col, alpha); }
  vertex_normal(buf: number, _x: number, _y: number, _z: number): void {
    // No-op for Canvas 2D: normals not used.
    void buf;
  }
  vertex_texcoord(_buf: number, _u: number, _v: number): void { /* no-op: texture coords ignored in Canvas 2D */ }

  // ---- Video API (HTML <video> element) ----
  video_open(path: string, ..._args: any[]): void {
    this.video_close();
    const v = document.createElement("video");
    v.src = path; v.style.display = "none";
    v.autoplay = true; v.preload = "auto";
    document.body?.appendChild(v);
    this._video = v;
  }
  video_close(): void {
    if (!this._video) return;
    this._video.pause(); this._video.remove(); this._video = null;
  }
  video_draw(): void {
    const v = this._video; if (!v || v.readyState < 2) return;
    const ctx = this._gfx.ctx;
    ctx.drawImage(v, 0, 0, ctx.canvas.width, ctx.canvas.height);
  }
  video_get_status(): number {
    if (!this._video) return 0; // video_status_none
    if (this._video.ended) return 3; // video_status_finished
    if (this._video.paused) return 2; // video_status_paused
    return 1; // video_status_playing
  }
  video_get_duration(): number { return this._video?.duration ?? 0; }
  video_seek_to(time: number): void { if (this._video) this._video.currentTime = time; }
  video_get_position(): number { return this._video?.currentTime ?? 0; }
  video_set_volume(vol: number): void { if (this._video) this._video.volume = Math.max(0, Math.min(1, vol)); }

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
  screen_save(filename: string): void {
    // Trigger a download of the current canvas content.
    try {
      const a = document.createElement("a");
      a.href = this._gfx.canvas.toDataURL("image/png");
      a.download = filename.replace(/\\/g, "/").split("/").pop() ?? filename;
      a.click();
    } catch { /* ignore: may fail in non-browser environments */ }
  }

  // ---- Async dialogs ----
  get_integer_async(_message: string, _default: number): number { return _default; }
  show_message(_str: string): void { console.log("GML show_message:", _str); }

  // ---- char ----
  chr(code: number): string { return String.fromCharCode(code); }

  // ---- More buffer ----
  buffer_peek(bufId: number, offset: number, type: number): any {
    const buf = this._buffers.get(bufId);
    if (!buf) return 0;
    // Read at absolute offset without moving pos
    const savedPos = buf.pos;
    buf.pos = offset;
    const result = this.buffer_read(bufId, type);
    buf.pos = savedPos;
    return result;
  }
  buffer_poke(bufId: number, offset: number, type: number, value: any): void {
    const buf = this._buffers.get(bufId);
    if (!buf) return;
    const savedPos = buf.pos;
    buf.pos = offset;
    this.buffer_write(bufId, type, value);
    buf.pos = savedPos;
  }
  buffer_tell(bufId: number): number {
    return this._buffers.get(bufId)?.pos ?? 0;
  }
  buffer_copy(src: number, srcOff: number, size: number, dest: number, destOff: number): void {
    const srcBuf = this._buffers.get(src);
    const destBuf = this._buffers.get(dest);
    if (!srcBuf || !destBuf) return;
    const slice = srcBuf.data.subarray(srcOff, srcOff + size);
    this._bufferGrow(destBuf, destOff + size);
    if (destOff + size <= destBuf.data.length) destBuf.data.set(slice, destOff);
  }

  // ---- More Steam ----
  steam_utils_enable_callbacks(_enable?: boolean): void { /* no-op */ }
  steam_upload_score_buffer_ext(_name: string, _score: number, _buf: number, ..._args: any[]): void { throw new Error("steam_upload_score_buffer_ext: not yet implemented"); }
  steam_upload_score_ext(_name: string, _score: number, ..._args: any[]): void { throw new Error("steam_upload_score_ext: not yet implemented"); }
  steam_ugc_start_item_update(_appId: number, _fileId: number): number { throw new Error("steam_ugc_start_item_update: not yet implemented"); }
  steam_ugc_set_item_description(_handle: number, _desc: string): void { throw new Error("steam_ugc_set_item_description: not yet implemented"); }
  steam_net_packet_get_data(_buf: number): void { throw new Error("steam_net_packet_get_data: not yet implemented"); }
  steam_net_packet_receive(): boolean { throw new Error("steam_net_packet_receive: not yet implemented"); }
  steam_net_packet_send(_steamid: number, _buf: number, _size?: number, _type?: number): void { throw new Error("steam_net_packet_send: not yet implemented"); }
  steam_music_play(): void { /* no-op — browser does not control Steam Music */ }
  steam_music_is_enabled(): boolean { return false; }
  steam_music_get_status(): number { return 0; }
  steam_lobby_list_request(): void { throw new Error("steam_lobby_list_request: not yet implemented"); }
  steam_lobby_list_get_lobby_id(_index: number): number { throw new Error("steam_lobby_list_get_lobby_id: not yet implemented"); }
  steam_lobby_list_get_count(): number { throw new Error("steam_lobby_list_get_count: not yet implemented"); }
  steam_get_most_achieved_achievement_info(_info?: any[]): boolean { return false; }
  steam_get_local_file_change_count(): number { return 0; }
  steam_available_languages(): string { return this.steam_current_game_language(); }
  steam_inventory_get_all_items(_arr?: any): number { throw new Error("steam_inventory_get_all_items: not yet implemented"); }
  steam_get_quota_total(): number { return 104857600; /* 100 MB typical Steam Cloud quota */ }
  steam_get_global_stat_history_real(_name: string, _days?: number): number { return 0; }
  steam_file_read(_path: string): string { return loadItem(this._persistence, this._steamCloudKey(_path)) ?? ""; }
  steam_set_rich_presence(_key: string, _val: string): void { /* no-op — Steam rich presence not available in browser */ }
  steam_user_get_auth_session_ticket(_arr?: any): number { throw new Error("steam_user_get_auth_session_ticket: not yet implemented"); }

  // ---- PS5 stubs ----
  ps5_gamepad_set_vibration_mode(_port: number, _mode: number): void { /* no-op */ }
  ps5_gamepad_set_trigger_effect_vibration(_port: number, _trigger: number, _start: number, _end: number, _str: number): void { /* no-op */ }

  // ---- pass (no-op — used in some GML contexts) ----
  pass(): void { /* no-op: GMS2.3+ no-op statement */ }

  // ---- More Steam (third batch) ----
  steam_update(): void { /* no-op — no Steamworks runtime in browser */ }
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
  steam_stats_ready(): boolean { return true; }
  steam_send_screenshot(_path?: string, _w?: number, _h?: number): void { /* no-op */ }
  steam_request_global_stats(_days: number): void { /* no-op — no server-side stats in browser */ }
  steam_reset_all_stats_achievements(_also_achievements?: boolean): void {
    // Clear all stats: scan localStorage for __steam_stat_<gameName>_ prefix
    const prefix = "__steam_stat_" + this._storage.gameName + "_";
    const keysToRemove: string[] = [];
    for (let i = 0; i < localStorage.length; i++) {
      const k = localStorage.key(i);
      if (k && k.startsWith(prefix)) keysToRemove.push(k);
    }
    for (const k of keysToRemove) localStorage.removeItem(k);
    if (_also_achievements !== false) {
      this._steamAchSave(new Set());
    }
  }
  steam_set_stat_avg_rate(_name: string, _session: number, _session_len: number): void { /* no-op — complex running-average stat */ }
  steam_set_stat_float(_name: string, _val: number): void { save(this._persistence, this._steamStatKey(_name), String(_val)); }
  steam_show_floating_gamepad_text_input(_mode: number, _x: number, _y: number, _w: number, _h: number): void { throw new Error("steam_show_floating_gamepad_text_input: not yet implemented"); }
  steam_shutdown(): void { /* no-op */ }
  steam_lobby_set_owner_id(_steamid: number, _lobby?: number): void { throw new Error("steam_lobby_set_owner_id: not yet implemented"); }
  steam_lobby_send_chat_message_buffer(_buf: number, _size?: number, _lobby?: number): boolean { throw new Error("steam_lobby_send_chat_message_buffer: not yet implemented"); }
  steam_lobby_is_owner(_lobby?: number): boolean { throw new Error("steam_lobby_is_owner: not yet implemented"); }
  steam_lobby_create(_type: number, _max_members: number): void { throw new Error("steam_lobby_create: not yet implemented"); }
  steam_lobby_activate_invite_overlay(_lobby?: number): void { /* no-op */ }
  steam_lobby_list_get_data(_index: number, _key: string): string { throw new Error("steam_lobby_list_get_data: not yet implemented"); }
  steam_lobby_list_join(_lobby: number): void { throw new Error("steam_lobby_list_join: not yet implemented"); }
  steam_is_user_logged_on(): boolean { return false; }
  steam_is_screenshot_requested(): boolean { return false; }
  steam_is_overlay_enabled(): boolean { return false; }
  steam_get_quota_free(): number { return 104857600; }
  steam_get_number_of_current_players(): void { throw new Error("steam_get_number_of_current_players: not yet implemented"); }
  steam_get_app_ownership_ticket_data(_appId: number): string { throw new Error("steam_get_app_ownership_ticket_data: not yet implemented"); }
  steam_file_read_buffer(_path: string, _buf?: number): boolean { throw new Error("steam_file_read_buffer: not yet implemented"); }
  steam_file_persisted(_path: string): boolean { return this.steam_file_exists(_path); }
  steam_download_scores_around_user(_board: string, _range: number, _range2?: number): void { throw new Error("steam_download_scores_around_user: not yet implemented"); }
  steam_download_scores(_board: string, _start: number, _end: number): void { throw new Error("steam_download_scores: not yet implemented"); }
  steam_download_friends_scores(_board: string): void { throw new Error("steam_download_friends_scores: not yet implemented"); }
  steam_music_pause(): void { /* no-op */ }
  steam_music_play_next(): void { /* no-op */ }
  steam_music_play_previous(): void { /* no-op */ }
  steam_music_set_volume(_vol: number): void { /* no-op */ }
  steam_music_is_playing(): boolean { return false; }
  steam_user_cancel_auth_ticket(_ticket: number): void { /* no-op */ }
  steam_user_installed_dlc(_appId: number): boolean { return true; /* assume all DLC available when running from extracted files */ }
  steam_user_owns_dlc(_appId: number): boolean { return true; }
  steam_user_request_encrypted_app_ticket(_extra: any): void { /* no-op */ }
  steam_utils_get_server_real_time(): number { return Math.floor(Date.now() / 1000); }
  steam_utils_is_steam_running_on_steam_deck(): boolean { return false; }

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
  sprite_set_speed(spr: number, speed: number, _type: number): void {
    this._spriteSpeedOverrides.set(spr, speed);
  }

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
  layer_get_depth(_layer: any): number { return 0; }
  layer_set_visible(_layer: any, _visible: boolean): void { /* no-op */ }
  layer_x(_layer: any, _x?: number): any { if (_x !== undefined) return; return 0; }
  layer_y(_layer: any, _y?: number): any { if (_y !== undefined) return; return 0; }

  // ---- More instance ----
  instance_deactivate_all(notme: boolean): void {
    for (const inst of this.roomVariables) {
      if (notme && inst === this._self) continue;
      this._deactivatedInstances.add(inst);
    }
  }
  instance_activate_all(): void {
    this._deactivatedInstances.clear();
  }
  instance_activate_region(x1: number, y1: number, x2: number, y2: number, inside: boolean): void {
    for (const inst of this._deactivatedInstances) {
      const inRegion = inst.x >= x1 && inst.x <= x2 && inst.y >= y1 && inst.y <= y2;
      if (inRegion === inside) this._deactivatedInstances.delete(inst);
    }
  }
  instance_activate_layer(_layer: any): void { /* no-op: layer data not available */ }
  instance_furthest(x: number, y: number, classIndex: number): any {
    const clazz = this.classes[classIndex];
    if (!clazz) return -4;
    let best = -1, bestDist = -1;
    for (const inst of this.roomVariables) {
      if (!(inst instanceof clazz)) continue;
      const d = Math.hypot(inst.x - x, inst.y - y);
      if (d > bestDist) { bestDist = d; best = inst as any; }
    }
    return best === -1 ? -4 : best;
  }
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
  room_instance_clear(room: number): void {
    // Clears all pre-defined instances from a room's data (not the currently running room).
    const data = this._roomDatas[room]; if (!data) return;
    data.objs = [];
  }

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
    const raw = loadItem(this._persistence, "__psn_trophy_" + gameName);
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
    save(this._persistence, "__psn_trophy_" + this._storage.gameName, JSON.stringify([...this._psnTrophies]));
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
  // ---- Steam Cloud VFS helpers ----
  private _steamCloudKey(path: string): string {
    return "__steam_cloud_" + this._storage.gameName + "_" + path;
  }
  private _steamCloudIndex(): string[] {
    const raw = loadItem(this._persistence, "__steam_cloud_" + this._storage.gameName + "__index");
    if (!raw) return [];
    try { return JSON.parse(raw) as string[]; } catch { return []; }
  }
  private _steamCloudAddToIndex(path: string): void {
    const idx = this._steamCloudIndex();
    if (!idx.includes(path)) {
      idx.push(path);
      save(this._persistence, "__steam_cloud_" + this._storage.gameName + "__index", JSON.stringify(idx));
    }
  }
  private _steamCloudRemoveFromIndex(path: string): void {
    const idx = this._steamCloudIndex().filter(p => p !== path);
    save(this._persistence, "__steam_cloud_" + this._storage.gameName + "__index", JSON.stringify(idx));
  }
  private _steamAchSet(): Set<string> {
    const raw = loadItem(this._persistence, "__steam_ach_" + this._storage.gameName);
    if (!raw) return new Set();
    try { return new Set(JSON.parse(raw) as string[]); } catch { return new Set(); }
  }
  private _steamAchSave(set: Set<string>): void {
    save(this._persistence, "__steam_ach_" + this._storage.gameName, JSON.stringify([...set]));
  }
  private _steamStatKey(name: string): string {
    return "__steam_stat_" + this._storage.gameName + "_" + name;
  }


  // ---- More Steam ----
  steam_activate_overlay_browser(_url: string): void { window.open(_url, "_blank"); }
  steam_clear_achievement(_name: string): void {
    const set = this._steamAchSet();
    set.delete(_name);
    this._steamAchSave(set);
  }
  steam_file_delete(_path: string): void {
    remove(this._persistence, this._steamCloudKey(_path));
    this._steamCloudRemoveFromIndex(_path);
  }
  steam_file_get_list(): string[] { return this._steamCloudIndex(); }
  steam_file_share(_path: string): void { /* no-op — Steam file sharing not available in browser */ }
  steam_file_size(_path: string): number { return (loadItem(this._persistence, this._steamCloudKey(_path)) ?? "").length; }
  steam_file_write_buffer(_path: string, _buf: number, _size?: number): boolean { throw new Error("steam_file_write_buffer: not yet implemented"); }
  steam_file_write_file(_path: string, _srcpath: string): boolean { throw new Error("steam_file_write_file: not yet implemented"); }
  steam_get_achievement_progress_limits_int(_name: string): [number, number] { throw new Error("steam_get_achievement_progress_limits_int: not yet implemented"); }
  steam_get_global_stat_real(_name: string): number { return 0; }
  steam_get_local_file_change(_index: number): string { throw new Error("steam_get_local_file_change: not yet implemented"); }
  steam_input_activate_action_set(_handle: number, _setHandle: number): void { /* no-op */ }
  steam_input_get_action_origin_from_xbox_origin(_handle: number, _origin: number): number { return 0; }
  steam_input_get_analog_action_handle(_name: string): number { return 0; }
  steam_input_get_connected_controllers(): number[] { return []; }
  steam_input_get_digital_action_data(_controller: number, _action: number): boolean { return false; }
  steam_input_get_digital_action_origins(_controller: number, _action_set: number, _action: number): number[] { return []; }
  steam_input_get_glyph_png_for_action_origin(_origin: number, _style: number, _flags: number): string { return ""; }
  steam_input_init(_explicit: boolean): void { /* no-op — no Steam Input in browser */ }
  steam_inventory_trigger_item_drop(_id: number): void { throw new Error("steam_inventory_trigger_item_drop: not yet implemented"); }
  steam_is_subscribed(): boolean { return false; }
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
    // Snapshot current gamepad state into _gamepadPrev before the frame runs
    // so pressed/released queries compare last-frame vs current-frame.
    const pads = navigator.getGamepads();
    for (let i = 0; i < pads.length; i++) {
      const gp = pads[i];
      this._gamepadPrev.set(i, gp ? gp.buttons.map((b) => b.pressed) : []);
    }
    const start = performance.now();
    this.onTick?.();
    if (this._currentRoom) this._currentRoom.draw();
    const end = performance.now();
    const elapsed = end - start;
    const newfps = 1000 / Math.max(0.01, elapsed);
    this.fps_real = 0.9 * this.fps_real + 0.1 * newfps;
    this._drawHandle = scheduleTimeout(
      () => this._runFrame(),
      Math.max(0, 1000 / this.room_speed - elapsed),
    );
  }

  // ---- Game startup ----

  async start(config: GameConfig): Promise<void> {
    // Init persistence before anything else — preloads OPFS into sync cache (falls back to localStorage).
    await initPersistence(this._persistence);

    this._roomDatas = config.rooms;
    this.sprites = config.sprites;
    this.textures = config.textures;
    this.fonts = config.fonts;
    this.sounds = config.sounds ?? [];
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

    // Load texture sheets and audio in parallel
    const sheetCount = Math.max(0, ...config.textures.map((t) => t.sheetId)) + 1;
    const sheetPromises: Promise<HTMLImageElement>[] = [];
    for (let i = 0; i < sheetCount; i++) {
      sheetPromises.push(loadImage(`assets/textures/texture_${i}.png`));
    }
    const [sheets] = await Promise.all([
      Promise.all(sheetPromises),
      loadAudio(this._audio, config.sounds),
    ]);
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
  sounds?: Sound[];
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

