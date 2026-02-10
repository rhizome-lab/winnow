const keysDown: Set<string> = new Set();
const keysPressed: Set<string> = new Set();
const keysReleased: Set<string> = new Set();

let mouseButtons: Set<number> = new Set();
let mouseButtonsPressed: Set<number> = new Set();
let mouseButtonsReleased: Set<number> = new Set();
let mx = 0;
let my = 0;

document.addEventListener("keydown", (e) => {
  if (!keysDown.has(e.code)) {
    keysPressed.add(e.code);
  }
  keysDown.add(e.code);
});

document.addEventListener("keyup", (e) => {
  keysDown.delete(e.code);
  keysReleased.add(e.code);
});

const canvas = document.getElementById("reincarnate-canvas") as HTMLCanvasElement;

canvas.addEventListener("mousemove", (e) => {
  const rect = canvas.getBoundingClientRect();
  mx = e.clientX - rect.left;
  my = e.clientY - rect.top;
});

canvas.addEventListener("mousedown", (e) => {
  if (!mouseButtons.has(e.button)) {
    mouseButtonsPressed.add(e.button);
  }
  mouseButtons.add(e.button);
});

canvas.addEventListener("mouseup", (e) => {
  mouseButtons.delete(e.button);
  mouseButtonsReleased.add(e.button);
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
    return keysDown.has(key);
  },

  is_key_pressed(key: string): boolean {
    return keysPressed.has(key);
  },

  is_key_released(key: string): boolean {
    return keysReleased.has(key);
  },

  is_mouse_down(button: number): boolean {
    return mouseButtons.has(mapButton(button));
  },

  is_mouse_pressed(button: number): boolean {
    return mouseButtonsPressed.has(mapButton(button));
  },

  is_mouse_released(button: number): boolean {
    return mouseButtonsReleased.has(mapButton(button));
  },

  mouse_x(): number {
    return mx;
  },

  mouse_y(): number {
    return my;
  },

  update(): void {
    keysPressed.clear();
    keysReleased.clear();
    mouseButtonsPressed.clear();
    mouseButtonsReleased.clear();
  },
};
