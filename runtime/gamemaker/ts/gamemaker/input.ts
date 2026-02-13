/** GML input handling — mouse, keyboard. */

import { getCanvas, onMouseMove, onMouseDown, onMouseUp, onKeyDown } from "./platform";
import { __gml_room_variables, __gml_sprites, GMLObject } from "./runtime";

let _mouse_x = 0;
let _mouse_y = 0;

let __gml_left_pressed = false;
let __gml_right_pressed = false;
let __gml_middle_pressed = false;
let __gml_left_released = false;
let __gml_right_released = false;
let __gml_middle_released = false;
let __gml_left_held = false;
let __gml_right_held = false;

export let keyboard_string = "";

const noop = function () {};

export function mouse_x(): number { return _mouse_x; }
export function mouse_y(): number { return _mouse_y; }

export function mouse_check_button(button: number): boolean {
  switch (button) {
    case 1: return __gml_left_held;
    case 2: return __gml_right_held;
    default: return false;
  }
}

export function mouse_check_button_pressed(button: number): boolean {
  switch (button) {
    case 1: return __gml_left_pressed;
    case 2: return __gml_right_pressed;
    default: return false;
  }
}

export function mouse_check_button_released(button: number): boolean {
  switch (button) {
    case 1: return __gml_left_released;
    case 2: return __gml_right_released;
    default: return false;
  }
}

export function resetFrameInput(): void {
  __gml_left_pressed = false;
  __gml_right_pressed = false;
  __gml_middle_pressed = false;
  __gml_left_released = false;
  __gml_right_released = false;
  __gml_middle_released = false;
}

export function activateMouse(ax: number, ay: number, override = false): void {
  for (const obj of __gml_room_variables) {
    if (obj.sprite_index === undefined) continue;
    const sprite = __gml_sprites[obj.sprite_index];
    if (!sprite) continue;
    const bx = obj.x;
    const by = obj.y;
    const lx = (ax - bx + sprite.origin.x) / obj.image_xscale;
    const ly = (ay - by + sprite.origin.y) / obj.image_yscale;
    if (lx >= 0 && ly >= 0 && lx < sprite.size.width && ly < sprite.size.height) {
      if (override || !obj.__active) {
        obj.__active = true;
        obj.mouseenter();
      }
    } else {
      if (override || obj.__active) {
        obj.__active = false;
        obj.mouseleave();
      }
    }
  }
}

export function setupInput(): void {
  onMouseMove((x, y) => {
    _mouse_x = x;
    _mouse_y = y;
    activateMouse(x, y);
  });

  onMouseDown((button) => {
    switch (button) {
      case 0: __gml_left_pressed = true; __gml_left_held = true; break;
      case 1: __gml_middle_pressed = true; break;
      case 2: __gml_right_pressed = true; __gml_right_held = true; break;
    }
  });

  onMouseUp((button) => {
    switch (button) {
      case 0: __gml_left_released = true; __gml_left_held = false; break;
      case 1: __gml_middle_released = true; break;
      case 2: __gml_right_released = true; __gml_right_held = false; break;
    }
  });

  onKeyDown((key, keyCode) => {
    if (key.length === 1) {
      keyboard_string += key;
      if (keyboard_string.length > 1024) {
        keyboard_string = keyboard_string.slice(keyboard_string.length - 1024);
      }
    } else if (keyCode === 8) {
      if (keyboard_string.length > 0) {
        keyboard_string = keyboard_string.slice(0, -1);
      }
    }
    dispatchKeyPress(keyCode);
  });
}

export function dispatchKeyPress(keyCode: number): void {
  const id = "keypress" + keyCode;
  for (const obj of __gml_room_variables) {
    if ((obj as any)[id] !== noop) {
      (obj as any)[id]();
    }
  }
}
