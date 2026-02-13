/**
 * GML Runtime — game loop, GMLObject base class, room system.
 */

import { initCanvas, getCanvas, getCtx, resizeCanvas, loadImage, scheduleFrame } from "./platform";
import { setupInput, mouse_x, mouse_y, resetFrameInput, dispatchKeyPress, activateMouse } from "./input";
import type { Sprite } from "../../data/sprites";
import type { Texture } from "../../data/textures";
import type { Font } from "../../data/fonts";
import type { Room } from "../../data/rooms";
import { drawSprite } from "./draw";

// Re-exports for class_preamble
export { Colors, HAligns, VAligns } from "./color";

// ---- Global state ----

const noop = function () {};

let __gml_draw_handle = 0;
let __gml_current_room: GMLRoom | null = null;
let __gml_is_stepping = false;
let __gml_to_step: GMLObject[] = [];
let __gml_drawgui_used = false;

export let room = 0;
export let room_speed = 60;
export let fps_real = 1;

/** All live instances in the current room. */
export const __gml_room_variables: GMLObject[] = [];

/** Registered class constructors (indexed by OBJT order). */
let __gml_classes: (typeof GMLObject)[] = [];

/** Room data array. */
let __gml_room_datas: Room[] = [];

/** Sprite data array. */
export let __gml_sprites: Sprite[] = [];

/** Texture data array. */
export let __gml_textures: Texture[] = [];

/** Loaded texture sheet images. */
export const __gml_texture_sheets: HTMLImageElement[] = [];

/** Font data array. */
export let __gml_fonts: Font[] = [];

/** Classes enum (name→index). */
let __gml_classes_enum: Record<string, number> = {};

/** Room instances list. */
const __gml_room_instances: GMLRoom[] = [];

// ---- Timing ----

export const timing = {
  tick() {
    // GameMaker uses its own setTimeout-based loop, not rAF
  },
};

// ---- GMLObject ----

const __baseproto = Object.getPrototypeOf(class {});

export class GMLObject {
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
  __alarm: number[] | null = null;
  __active = false;
  __visible = true;

  static __instances: GMLObject[] = [];

  static get instances(): GMLObject[] {
    if (!this.hasOwnProperty("__instances")) {
      (this as any).__instances = [];
    }
    return (this as any).__instances;
  }

  get alarm(): number[] {
    if (this.__alarm === null) {
      this.__alarm = [];
    }
    return this.__alarm;
  }
  set alarm(val: number[]) {
    this.__alarm = val;
  }

  get id(): GMLObject { return this; }

  create(): void {}
  destroy(): void {}

  draw(): void {
    if (this.sprite_index === undefined || !this.__visible) return;
    drawSprite(this.sprite_index, this.image_index, this.x, this.y, this);
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

  [key: string]: any;
}

// Alarm stubs
for (let i = 0; i < 12; i++) {
  (GMLObject.prototype as any)["alarm" + i] = noop;
}
// Key press stubs
for (let i = 0; i <= 0xff; i++) {
  (GMLObject.prototype as any)["keypress" + i] = noop;
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

// Sprites enum — populated by startGame
export const Sprites: Record<string, number> = {};

// ---- GMLRoom ----

class GMLRoom {
  draw(): void {
    const ctx = getCtx();
    ctx.fillStyle = "black";
    ctx.fillRect(0, 0, getCanvas().width, getCanvas().height);

    const oldRoom = room;
    __gml_is_stepping = true;

    // Alarms
    for (const instance of __gml_room_variables) {
      if (instance.alarm.length !== 0) {
        for (let i = 0; i < 12; i++) {
          if (instance.alarm[i]) {
            instance.alarm[i]--;
            if (instance.alarm[i] === 0) {
              delete instance.alarm[i];
              const method = (instance as any)["alarm" + i];
              if (method !== noop) method.call(instance);
              if (oldRoom !== room) break;
            }
          }
        }
      }
    }

    // Begin step
    let toStep: GMLObject[] = __gml_room_variables;
    while (toStep.length !== 0) {
      __gml_to_step = [];
      for (const instance of toStep) {
        if ((instance as any).beginstep === noop) continue;
        instance.xprevious = instance.x;
        instance.yprevious = instance.y;
        instance.beginstep();
        if (oldRoom !== room) break;
      }
      toStep = __gml_to_step;
    }

    // Step
    toStep = __gml_room_variables;
    while (toStep.length !== 0) {
      __gml_to_step = [];
      for (const instance of toStep) {
        if ((instance as any).step === noop) continue;
        instance.step();
        if (oldRoom !== room) break;
      }
      toStep = __gml_to_step;
    }

    // End step
    toStep = __gml_room_variables;
    while (toStep.length !== 0) {
      __gml_to_step = [];
      for (const instance of toStep) {
        if ((instance as any).endstep === noop) continue;
        instance.endstep();
        if (oldRoom !== room) break;
      }
      toStep = __gml_to_step;
    }

    __gml_is_stepping = false;

    // Draw (sorted by depth, descending)
    const sorted = __gml_room_variables.slice().sort((a, b) => b.depth - a.depth);
    for (const instance of sorted) {
      if ((instance as any).draw === noop) continue;
      instance.draw();
      if (oldRoom !== room) break;
    }

    // Draw GUI
    if (__gml_drawgui_used) {
      for (const instance of sorted) {
        if ((instance as any).drawgui === noop) continue;
        instance.drawgui();
        if (oldRoom !== room) break;
      }
    }

    resetFrameInput();
  }

  create(restart = false): void {
    const idx = __gml_room_instances.indexOf(this);
    const data = __gml_room_datas[idx];
    if (!data) return;

    const instances: GMLObject[] = [];
    for (const obj of data.objs) {
      const clazz = __gml_classes[obj.obj];
      if (!clazz) continue;
      const proto = clazz.prototype;
      if (!proto.persistent || instance_number_internal(clazz) === 0) {
        instances.push(instance_create_internal(obj.pos.x, obj.pos.y, clazz, true));
      }
    }
    for (const instance of instances) {
      instance.create();
    }
  }

  destroy(restart = false): void {
    for (const obj of __gml_room_variables.slice()) {
      if (restart || !obj.persistent) {
        instance_destroy_internal(obj);
      }
    }
  }
}

// ---- Instance management (internal) ----

function instance_create_internal(x: number, y: number, clazz: typeof GMLObject, roomStart = false): GMLObject {
  const instance = new (clazz as any)();
  // Walk prototype chain and push to each class's instances array
  let c: any = instance.constructor;
  while (c !== __baseproto) {
    c.instances.push(instance);
    c = Object.getPrototypeOf(c);
  }
  instance.xstart = instance.x = x;
  instance.ystart = instance.y = y;
  __gml_room_variables.push(instance);
  if (!roomStart) {
    instance.create();
  }
  if (!__gml_drawgui_used && (instance as any).drawgui !== noop) {
    __gml_drawgui_used = true;
  }
  if (__gml_is_stepping) {
    __gml_to_step.push(instance);
  }
  return instance;
}

function instance_destroy_internal(instance: GMLObject): void {
  instance.destroy();
  let c: any = instance.constructor;
  while (c !== __baseproto) {
    const arr: GMLObject[] = c.instances;
    const idx = arr.indexOf(instance);
    if (idx > -1) arr.splice(idx, 1);
    c = Object.getPrototypeOf(c);
  }
  const idx = __gml_room_variables.indexOf(instance);
  if (idx > -1) __gml_room_variables.splice(idx, 1);
}

function instance_number_internal(clazz: typeof GMLObject): number {
  return clazz.instances.reduce(
    (p: number, c: GMLObject) => p + (c.constructor === clazz ? 1 : 0), 0,
  );
}

// ---- Public instance API (called from emitted code) ----

export function instance_create(x: number, y: number, classIndex: number): GMLObject {
  const clazz = __gml_classes[classIndex];
  return instance_create_internal(x, y, clazz);
}

export function instance_destroy(instance?: GMLObject): void {
  if (instance === undefined) return;
  instance_destroy_internal(instance);
}

export function instance_exists(classIndex: number): boolean {
  const clazz = __gml_classes[classIndex];
  if (!clazz) return false;
  return clazz.instances.length > 0;
}

export function instance_number(classIndex: number): number {
  const clazz = __gml_classes[classIndex];
  if (!clazz) return 0;
  return instance_number_internal(clazz);
}

// ---- Room navigation ----

export function room_goto(id: number, restart = false): void {
  const oldRoom = __gml_current_room;
  if (oldRoom !== null) {
    for (const instance of __gml_room_variables) {
      instance.roomend();
    }
    oldRoom.destroy(restart);
  }
  const newRoom = __gml_room_instances[id];
  __gml_current_room = newRoom;
  room = id;
  room_speed = __gml_room_datas[id]?.speed ?? 60;
  resizeCanvas(__gml_room_datas[id]?.size.width ?? 800, __gml_room_datas[id]?.size.height ?? 600);
  newRoom.create(restart);
  for (const instance of __gml_room_variables) {
    instance.roomstart();
  }
  activateMouse(mouse_x(), mouse_y(), true);
}

export function room_goto_next(): void { room_goto(room + 1); }
export function room_goto_previous(): void { room_goto(room - 1); }
export function room_restart(): void { room_goto(room); }
export function game_restart(): void { room_goto(0, true); }

// ---- Game loop ----

function drawit(): void {
  const start = performance.now();
  if (__gml_current_room) __gml_current_room.draw();
  const end = performance.now();
  const elapsed = end - start;
  const newfps = 1000 / Math.max(0.01, elapsed);
  fps_real = 0.9 * fps_real + 0.1 * newfps;
  __gml_draw_handle = scheduleFrame(drawit, Math.max(0, 1000 / room_speed - elapsed));
}

// ---- Game startup ----

export interface GameConfig {
  rooms: Room[];
  sprites: Sprite[];
  textures: Texture[];
  fonts: Font[];
  classes: (typeof GMLObject)[];
  Classes: Record<string, number>;
  initialRoom: number;
}

export async function startGame(config: GameConfig): Promise<void> {
  __gml_room_datas = config.rooms;
  __gml_sprites = config.sprites;
  __gml_textures = config.textures;
  __gml_fonts = config.fonts;
  __gml_classes = config.classes;
  __gml_classes_enum = config.Classes;

  // Populate Sprites enum from sprite data
  for (let i = 0; i < config.sprites.length; i++) {
    Sprites[config.sprites[i].name] = i;
  }

  // Set up collision stubs (need class count)
  for (let i = 0; i < config.classes.length; i++) {
    (GMLObject.prototype as any)["collision" + i] = noop;
  }

  // Create room instances
  for (let i = 0; i < config.rooms.length; i++) {
    __gml_room_instances.push(new GMLRoom());
  }

  // Init canvas and input
  const { canvas } = initCanvas("reincarnate-canvas");
  canvas.tabIndex = 0;
  canvas.focus();
  setupInput();

  // Load texture sheets
  const sheetCount = Math.max(0, ...config.textures.map((t) => t.sheetId)) + 1;
  const sheetPromises: Promise<HTMLImageElement>[] = [];
  for (let i = 0; i < sheetCount; i++) {
    sheetPromises.push(loadImage(`assets/textures/texture_${i}.png`));
  }
  const sheets = await Promise.all(sheetPromises);
  __gml_texture_sheets.push(...sheets);

  // Start
  room_goto(config.initialRoom);
  drawit();
}
