/** GML input handling — mouse, keyboard. */

import type { GameRuntime } from "./runtime";
import { onMouseMove, onMouseDown, onMouseUp, onKeyDown } from "./platform";
import { ACTIVE, noop } from "./constants";

interface ButtonState { pressed: boolean; released: boolean; held: boolean; }

export class InputState {
  mouse = {
    x: 0,
    y: 0,
    buttons: [
      { pressed: false, released: false, held: false }, // mb_left (GML 1)
      { pressed: false, released: false, held: false }, // mb_right (GML 2)
      { pressed: false, released: false, held: false }, // mb_middle (GML 3)
    ] as ButtonState[],
  };
  domButtonMap = [0, 2, 1];
  keyboard_string = "";
}

export function createInputAPI(rt: GameRuntime) {
  const input = rt._input;

  function mouse_x(): number { return input.mouse.x; }
  function mouse_y(): number { return input.mouse.y; }

  function mouse_check_button(button: number): boolean {
    return input.mouse.buttons[button - 1]?.held ?? false;
  }

  function mouse_check_button_pressed(button: number): boolean {
    return input.mouse.buttons[button - 1]?.pressed ?? false;
  }

  function mouse_check_button_released(button: number): boolean {
    return input.mouse.buttons[button - 1]?.released ?? false;
  }

  function resetFrameInput(): void {
    for (const b of input.mouse.buttons) {
      b.pressed = false;
      b.released = false;
    }
  }

  function activateMouse(ax: number, ay: number, override = false): void {
    for (const obj of rt.roomVariables) {
      if (obj.sprite_index === undefined) continue;
      const sprite = rt.sprites[obj.sprite_index];
      if (!sprite) continue;
      const bx = obj.x;
      const by = obj.y;
      const lx = (ax - bx + sprite.origin.x) / obj.image_xscale;
      const ly = (ay - by + sprite.origin.y) / obj.image_yscale;
      if (lx >= 0 && ly >= 0 && lx < sprite.size.width && ly < sprite.size.height) {
        if (override || !obj[ACTIVE]) {
          obj[ACTIVE] = true;
          obj.mouseenter();
        }
      } else {
        if (override || obj[ACTIVE]) {
          obj[ACTIVE] = false;
          obj.mouseleave();
        }
      }
    }
  }

  function setupInput(): void {
    const canvas = rt._gfx.canvas;

    onMouseMove(canvas, (x, y) => {
      input.mouse.x = x;
      input.mouse.y = y;
      activateMouse(x, y);
    });

    onMouseDown(canvas, (button) => {
      const b = input.mouse.buttons[input.domButtonMap[button]];
      if (b) { b.pressed = true; b.held = true; }
    });

    onMouseUp(canvas, (button) => {
      const b = input.mouse.buttons[input.domButtonMap[button]];
      if (b) { b.released = true; b.held = false; }
    });

    onKeyDown(canvas, (key, keyCode) => {
      if (key.length === 1) {
        input.keyboard_string += key;
        if (input.keyboard_string.length > 1024) {
          input.keyboard_string = input.keyboard_string.slice(input.keyboard_string.length - 1024);
        }
      } else if (keyCode === 8) {
        if (input.keyboard_string.length > 0) {
          input.keyboard_string = input.keyboard_string.slice(0, -1);
        }
      }
      dispatchKeyPress(keyCode);
    });
  }

  function dispatchKeyPress(keyCode: number): void {
    const id = "keypress" + keyCode;
    for (const obj of rt.roomVariables) {
      if ((obj as any)[id] !== noop) {
        (obj as any)[id]();
      }
    }
  }

  return {
    mouse_x, mouse_y,
    mouse_check_button, mouse_check_button_pressed, mouse_check_button_released,
    resetFrameInput, activateMouse, setupInput, dispatchKeyPress,
  };
}
