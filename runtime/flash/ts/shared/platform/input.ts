/** Browser input — keyboard, mouse, touch, gamepad, and text input event binding. */

export type DeviceKind = "keyboard" | "mouse" | "touch" | "gamepad";

export class InputState {
  keysDown = new Set<string>();
  mouseButtons = new Set<number>();
  mouseX = 0;
  mouseY = 0;
  pointerLocked = false;
  touches = new Map<number, { x: number; y: number }>();
  gamepads = new Map<number, Gamepad>();
}

// --- Device enumeration ---

export function devices(state: InputState, kind: DeviceKind): number[] {
  switch (kind) {
    case "keyboard":
    case "mouse":
      return [0];
    case "touch":
      return [0];
    case "gamepad": {
      const pads = navigator.getGamepads();
      const indices: number[] = [];
      for (let i = 0; i < pads.length; i++) {
        if (pads[i] != null) indices.push(i);
      }
      return indices;
    }
  }
}

export function onDeviceConnect(
  _state: InputState,
  cb: (device: number, kind: DeviceKind) => void,
): void {
  window.addEventListener("gamepadconnected", (e) => {
    cb((e as GamepadEvent).gamepad.index, "gamepad");
  });
}

export function onDeviceDisconnect(
  _state: InputState,
  cb: (device: number) => void,
): void {
  window.addEventListener("gamepaddisconnected", (e) => {
    cb((e as GamepadEvent).gamepad.index);
  });
}

// --- Keyboard ---

export function onKeyDown(
  state: InputState,
  _canvas: HTMLCanvasElement,
  cb: (device: number, code: string, key: string) => void,
): void {
  window.addEventListener("keydown", (e) => {
    state.keysDown.add(e.code);
    cb(0, e.code, e.key);
  });
}

export function onKeyUp(
  state: InputState,
  _canvas: HTMLCanvasElement,
  cb: (device: number, code: string, key: string) => void,
): void {
  window.addEventListener("keyup", (e) => {
    state.keysDown.delete(e.code);
    cb(0, e.code, e.key);
  });
}

export function isKeyDown(state: InputState, _device: number, code: string): boolean {
  return state.keysDown.has(code);
}

// --- Mouse ---

export function onMouseDown(
  state: InputState,
  canvas: HTMLCanvasElement,
  cb: (device: number, button: number) => void,
): void {
  canvas.addEventListener("mousedown", (e) => {
    state.mouseButtons.add(e.button);
    cb(0, e.button);
  });
  canvas.addEventListener("contextmenu", (e) => {
    e.preventDefault();
    e.stopPropagation();
  });
}

export function onMouseUp(
  state: InputState,
  canvas: HTMLCanvasElement,
  cb: (device: number, button: number) => void,
): void {
  canvas.addEventListener("mouseup", (e) => {
    state.mouseButtons.delete(e.button);
    cb(0, e.button);
  });
}

export function onMouseMove(
  state: InputState,
  canvas: HTMLCanvasElement,
  cb: (device: number, x: number, y: number) => void,
): void {
  canvas.addEventListener("mousemove", (e) => {
    const rect = canvas.getBoundingClientRect();
    const x = e.clientX - rect.left;
    const y = e.clientY - rect.top;
    state.mouseX = x;
    state.mouseY = y;
    cb(0, x, y);
  });
}

export function onScroll(
  _state: InputState,
  canvas: HTMLCanvasElement,
  cb: (device: number, dx: number, dy: number) => void,
): void {
  canvas.addEventListener(
    "wheel",
    (e) => {
      e.preventDefault();
      cb(0, e.deltaX, e.deltaY);
    },
    { passive: false },
  );
}

export function isMouseDown(state: InputState, _device: number, button: number): boolean {
  return state.mouseButtons.has(button);
}

export function mouseX(state: InputState, _device: number): number {
  return state.mouseX;
}

export function mouseY(state: InputState, _device: number): number {
  return state.mouseY;
}

// --- Pointer lock ---

export function requestPointerLock(canvas: HTMLCanvasElement): void {
  canvas.requestPointerLock();
}

export function releasePointerLock(): void {
  document.exitPointerLock();
}

export function isPointerLocked(state: InputState): boolean {
  return state.pointerLocked;
}

export function onMouseDelta(
  _state: InputState,
  canvas: HTMLCanvasElement,
  cb: (device: number, dx: number, dy: number) => void,
): void {
  canvas.addEventListener("mousemove", (e) => {
    cb(0, e.movementX, e.movementY);
  });
}

// --- Touch ---

function touchPos(
  touch: Touch,
  canvas: HTMLCanvasElement,
): { x: number; y: number } {
  const rect = canvas.getBoundingClientRect();
  return { x: touch.clientX - rect.left, y: touch.clientY - rect.top };
}

export function onTouchStart(
  state: InputState,
  canvas: HTMLCanvasElement,
  cb: (device: number, id: number, x: number, y: number) => void,
): void {
  canvas.addEventListener("touchstart", (e) => {
    e.preventDefault();
    for (const touch of Array.from(e.changedTouches)) {
      const pos = touchPos(touch, canvas);
      state.touches.set(touch.identifier, pos);
      cb(0, touch.identifier, pos.x, pos.y);
    }
  }, { passive: false });
}

export function onTouchMove(
  state: InputState,
  canvas: HTMLCanvasElement,
  cb: (device: number, id: number, x: number, y: number) => void,
): void {
  canvas.addEventListener("touchmove", (e) => {
    e.preventDefault();
    for (const touch of Array.from(e.changedTouches)) {
      const pos = touchPos(touch, canvas);
      state.touches.set(touch.identifier, pos);
      cb(0, touch.identifier, pos.x, pos.y);
    }
  }, { passive: false });
}

export function onTouchEnd(
  state: InputState,
  canvas: HTMLCanvasElement,
  cb: (device: number, id: number, x: number, y: number) => void,
): void {
  canvas.addEventListener("touchend", (e) => {
    for (const touch of Array.from(e.changedTouches)) {
      const pos = touchPos(touch, canvas);
      state.touches.delete(touch.identifier);
      cb(0, touch.identifier, pos.x, pos.y);
    }
  });
}

export function touchCount(state: InputState, _device: number): number {
  return state.touches.size;
}

export function touchX(state: InputState, _device: number, id: number): number {
  return state.touches.get(id)?.x ?? 0;
}

export function touchY(state: InputState, _device: number, id: number): number {
  return state.touches.get(id)?.y ?? 0;
}

// --- Gamepad (analog) ---

export function deviceAxis(
  _state: InputState,
  device: number,
  axis: number,
): number {
  return navigator.getGamepads()[device]?.axes[axis] ?? 0;
}

// --- Text input ---

export function onTextInput(
  _state: InputState,
  _canvas: HTMLCanvasElement,
  cb: (device: number, text: string) => void,
): void {
  window.addEventListener("input", (e) => {
    cb(0, (e as InputEvent).data ?? "");
  });
}

export function onCompositionStart(
  _state: InputState,
  _canvas: HTMLCanvasElement,
  cb: (device: number) => void,
): void {
  window.addEventListener("compositionstart", () => {
    cb(0);
  });
}

export function onCompositionUpdate(
  _state: InputState,
  _canvas: HTMLCanvasElement,
  cb: (device: number, text: string) => void,
): void {
  window.addEventListener("compositionupdate", (e) => {
    cb(0, (e as CompositionEvent).data);
  });
}

export function onCompositionEnd(
  _state: InputState,
  _canvas: HTMLCanvasElement,
  cb: (device: number, text: string) => void,
): void {
  window.addEventListener("compositionend", (e) => {
    cb(0, (e as CompositionEvent).data);
  });
}
