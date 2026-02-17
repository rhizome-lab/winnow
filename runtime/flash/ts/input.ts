class InputState {
  keysDown: Set<string> = new Set();
  keysPressed: Set<string> = new Set();
  keysReleased: Set<string> = new Set();
  mouseButtons: Set<number> = new Set();
  mouseButtonsPressed: Set<number> = new Set();
  mouseButtonsReleased: Set<number> = new Set();
  mx = 0;
  my = 0;
}

const state = new InputState();

document.addEventListener("keydown", (e) => {
  if (!state.keysDown.has(e.code)) {
    state.keysPressed.add(e.code);
  }
  state.keysDown.add(e.code);
});

document.addEventListener("keyup", (e) => {
  state.keysDown.delete(e.code);
  state.keysReleased.add(e.code);
});

const canvas = document.getElementById("reincarnate-canvas") as HTMLCanvasElement;

canvas.addEventListener("mousemove", (e) => {
  const rect = canvas.getBoundingClientRect();
  state.mx = e.clientX - rect.left;
  state.my = e.clientY - rect.top;
});

canvas.addEventListener("mousedown", (e) => {
  if (!state.mouseButtons.has(e.button)) {
    state.mouseButtonsPressed.add(e.button);
  }
  state.mouseButtons.add(e.button);
});

canvas.addEventListener("mouseup", (e) => {
  state.mouseButtons.delete(e.button);
  state.mouseButtonsReleased.add(e.button);
});

function mapButton(button: number): number {
  switch (button) {
    case 0: return 0; // Left
    case 2: return 1; // Right
    case 1: return 2; // Middle
    default: return button;
  }
}

export const input = {
  is_key_down(key: string): boolean {
    return state.keysDown.has(key);
  },

  is_key_pressed(key: string): boolean {
    return state.keysPressed.has(key);
  },

  is_key_released(key: string): boolean {
    return state.keysReleased.has(key);
  },

  is_mouse_down(button: number): boolean {
    return state.mouseButtons.has(mapButton(button));
  },

  is_mouse_pressed(button: number): boolean {
    return state.mouseButtonsPressed.has(mapButton(button));
  },

  is_mouse_released(button: number): boolean {
    return state.mouseButtonsReleased.has(mapButton(button));
  },

  mouse_x(): number {
    return state.mx;
  },

  mouse_y(): number {
    return state.my;
  },

  update(): void {
    state.keysPressed.clear();
    state.keysReleased.clear();
    state.mouseButtonsPressed.clear();
    state.mouseButtonsReleased.clear();
  },
};
