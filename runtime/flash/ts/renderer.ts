/** Flash renderer shim — Canvas2D drawing primitives. */

export class RendererShim {
  private _canvas: HTMLCanvasElement;
  private _ctx: CanvasRenderingContext2D;

  constructor(canvas: HTMLCanvasElement) {
    this._canvas = canvas;
    this._ctx = canvas.getContext("2d")!;
  }

  clear(r: number, g: number, b: number, a: number): void {
    this._ctx.clearRect(0, 0, this._canvas.width, this._canvas.height);
    this._ctx.fillStyle = `rgba(${r * 255}, ${g * 255}, ${b * 255}, ${a})`;
    this._ctx.fillRect(0, 0, this._canvas.width, this._canvas.height);
  }

  draw_sprite(texture: HTMLImageElement, x: number, y: number): void {
    this._ctx.drawImage(texture, x, y);
  }

  draw_sprite_scaled(
    texture: HTMLImageElement,
    x: number,
    y: number,
    scale_x: number,
    scale_y: number,
  ): void {
    this._ctx.save();
    this._ctx.translate(x, y);
    this._ctx.scale(scale_x, scale_y);
    this._ctx.drawImage(texture, 0, 0);
    this._ctx.restore();
  }

  draw_sprite_rotated(
    texture: HTMLImageElement,
    x: number,
    y: number,
    angle: number,
  ): void {
    this._ctx.save();
    this._ctx.translate(x, y);
    this._ctx.rotate(angle);
    this._ctx.drawImage(texture, -texture.width / 2, -texture.height / 2);
    this._ctx.restore();
  }

  draw_rect(
    x: number,
    y: number,
    w: number,
    h: number,
    r: number,
    g: number,
    b: number,
    a: number,
  ): void {
    this._ctx.fillStyle = `rgba(${r * 255}, ${g * 255}, ${b * 255}, ${a})`;
    this._ctx.fillRect(x, y, w, h);
  }

  draw_text(text: string, x: number, y: number, size: number): void {
    this._ctx.font = `${size}px sans-serif`;
    this._ctx.fillStyle = "white";
    this._ctx.fillText(text, x, y);
  }

  load_texture(src: string): Promise<HTMLImageElement> {
    return new Promise((resolve, reject) => {
      const img = new Image();
      img.onload = () => resolve(img);
      img.onerror = reject;
      img.src = src;
    });
  }

  surface_width(): number { return this._canvas.width; }
  surface_height(): number { return this._canvas.height; }

  present(): void {
    // Canvas2D is immediate-mode; no explicit present needed.
  }
}
