/** Flash input shim — keyboard and mouse state, per-frame pressed/released tracking. */

function mapButton(button: number): number {
  switch (button) {
    case 0: return 0; // Left
    case 2: return 1; // Right
    case 1: return 2; // Middle
    default: return button;
  }
}

export class InputShim {
  private _keysDown = new Set<string>();
  private _keysPressed = new Set<string>();
  private _keysReleased = new Set<string>();
  private _mouseButtons = new Set<number>();
  private _mouseButtonsPressed = new Set<number>();
  private _mouseButtonsReleased = new Set<number>();
  private _mx = 0;
  private _my = 0;

  constructor(canvas: HTMLCanvasElement) {
    document.addEventListener("keydown", (e) => {
      if (!this._keysDown.has(e.code)) this._keysPressed.add(e.code);
      this._keysDown.add(e.code);
    });
    document.addEventListener("keyup", (e) => {
      this._keysDown.delete(e.code);
      this._keysReleased.add(e.code);
    });
    canvas.addEventListener("mousemove", (e) => {
      const rect = canvas.getBoundingClientRect();
      this._mx = e.clientX - rect.left;
      this._my = e.clientY - rect.top;
    });
    canvas.addEventListener("mousedown", (e) => {
      if (!this._mouseButtons.has(e.button)) this._mouseButtonsPressed.add(e.button);
      this._mouseButtons.add(e.button);
    });
    canvas.addEventListener("mouseup", (e) => {
      this._mouseButtons.delete(e.button);
      this._mouseButtonsReleased.add(e.button);
    });
  }

  is_key_down(key: string): boolean { return this._keysDown.has(key); }
  is_key_pressed(key: string): boolean { return this._keysPressed.has(key); }
  is_key_released(key: string): boolean { return this._keysReleased.has(key); }

  is_mouse_down(button: number): boolean { return this._mouseButtons.has(mapButton(button)); }
  is_mouse_pressed(button: number): boolean { return this._mouseButtonsPressed.has(mapButton(button)); }
  is_mouse_released(button: number): boolean { return this._mouseButtonsReleased.has(mapButton(button)); }

  mouse_x(): number { return this._mx; }
  mouse_y(): number { return this._my; }

  update(): void {
    this._keysPressed.clear();
    this._keysReleased.clear();
    this._mouseButtonsPressed.clear();
    this._mouseButtonsReleased.clear();
  }
}
