/** GML input handling — mouse, keyboard. */

import type { GameRuntime } from "./runtime";
import { InputState as PlatformInputState, onMouseMove, onMouseDown, onMouseUp, onKeyDown, onKeyUp, onScroll } from "../shared/platform";
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
    wheelUp: false,
    wheelDown: false,
  };
  domButtonMap = [0, 2, 1];
  keyboard_string = "";
  keysDown = new Set<number>();
  keysPressed = new Set<number>();
  keysReleased = new Set<number>();
}

export function createInputAPI(rt: GameRuntime) {
  const input = rt._input;

  function mouse_x(): number { return input.mouse.x; }
  function mouse_y(): number { return input.mouse.y; }
  function mouse_wheel_up(): boolean { return input.mouse.wheelUp; }
  function mouse_wheel_down(): boolean { return input.mouse.wheelDown; }

  function mouse_check_button(button: number): boolean {
    return input.mouse.buttons[button - 1]?.held ?? false;
  }

  function mouse_check_button_pressed(button: number): boolean {
    return input.mouse.buttons[button - 1]?.pressed ?? false;
  }

  function mouse_check_button_released(button: number): boolean {
    return input.mouse.buttons[button - 1]?.released ?? false;
  }

  function keyboard_check(key: number): boolean {
    return input.keysDown.has(key);
  }

  function keyboard_check_pressed(key: number): boolean {
    return input.keysPressed.has(key);
  }

  function keyboard_check_released(key: number): boolean {
    return input.keysReleased.has(key);
  }

  function resetFrameInput(): void {
    for (const b of input.mouse.buttons) {
      b.pressed = false;
      b.released = false;
    }
    input.mouse.wheelUp = false;
    input.mouse.wheelDown = false;
    input.keysPressed.clear();
    input.keysReleased.clear();
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

  function codeToGmlKeyCode(code: string, key: string): number {
    if (code.startsWith("Key")) {
      const ch = code.slice(3);
      if (ch.length === 1) return ch.charCodeAt(0);
    }
    if (code.startsWith("Digit")) return code.charCodeAt(5);
    if (code.startsWith("Numpad")) {
      const rest = code.slice(6);
      if (rest >= "0" && rest <= "9") return 96 + parseInt(rest);
      switch (rest) {
        case "Add": return 107;
        case "Subtract": return 109;
        case "Multiply": return 106;
        case "Divide": return 111;
        case "Decimal": return 110;
        case "Enter": return 13;
      }
    }
    if (code.startsWith("F") && code.length <= 3) {
      const n = parseInt(code.slice(1));
      if (n >= 1 && n <= 12) return 111 + n;
    }
    switch (code) {
      case "Backspace": return 8;
      case "Tab": return 9;
      case "Enter": return 13;
      case "ShiftLeft": case "ShiftRight": return 16;
      case "ControlLeft": case "ControlRight": return 17;
      case "AltLeft": case "AltRight": return 18;
      case "Pause": return 19;
      case "CapsLock": return 20;
      case "Escape": return 27;
      case "Space": return 32;
      case "PageUp": return 33;
      case "PageDown": return 34;
      case "End": return 35;
      case "Home": return 36;
      case "ArrowLeft": return 37;
      case "ArrowUp": return 38;
      case "ArrowRight": return 39;
      case "ArrowDown": return 40;
      case "Insert": return 45;
      case "Delete": return 46;
      case "MetaLeft": case "MetaRight": return 91;
      case "ContextMenu": return 93;
      case "NumLock": return 144;
      case "ScrollLock": return 145;
      case "Semicolon": return 186;
      case "Equal": return 187;
      case "Comma": return 188;
      case "Minus": return 189;
      case "Period": return 190;
      case "Slash": return 191;
      case "Backquote": return 192;
      case "BracketLeft": return 219;
      case "Backslash": return 220;
      case "BracketRight": return 221;
      case "Quote": return 222;
    }
    if (key.length === 1) return key.toUpperCase().charCodeAt(0);
    return 0;
  }

  function setupInput(): void {
    const canvas = rt._gfx.canvas;
    const platformInput = new PlatformInputState();

    onMouseMove(platformInput, canvas, (_device, x, y) => {
      input.mouse.x = x;
      input.mouse.y = y;
      activateMouse(x, y);
    });

    onMouseDown(platformInput, canvas, (_device, button) => {
      const b = input.mouse.buttons[input.domButtonMap[button]!];
      if (b) { b.pressed = true; b.held = true; }
    });

    onMouseUp(platformInput, canvas, (_device, button) => {
      const b = input.mouse.buttons[input.domButtonMap[button]!];
      if (b) { b.released = true; b.held = false; }
    });

    onKeyDown(platformInput, canvas, (_device, code, key) => {
      const keyCode = codeToGmlKeyCode(code, key);
      input.keysPressed.add(keyCode);
      input.keysDown.add(keyCode);
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

    onKeyUp(platformInput, canvas, (_device, code, key) => {
      const keyCode = codeToGmlKeyCode(code, key);
      input.keysReleased.add(keyCode);
      input.keysDown.delete(keyCode);
    });

    onScroll(platformInput, canvas, (_device, _dx, dy) => {
      if (dy < 0) input.mouse.wheelUp = true;
      else input.mouse.wheelDown = true;
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
    mouse_wheel_up, mouse_wheel_down,
    mouse_check_button, mouse_check_button_pressed, mouse_check_button_released,
    keyboard_check, keyboard_check_pressed, keyboard_check_released,
    resetFrameInput, activateMouse, setupInput, dispatchKeyPress,
  };
}
