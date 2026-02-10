const canvas = document.getElementById("reincarnate-canvas") as HTMLCanvasElement;
const ctx = canvas.getContext("2d")!;

export const renderer = {
  clear(r: number, g: number, b: number, a: number): void {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    ctx.fillStyle = `rgba(${r * 255}, ${g * 255}, ${b * 255}, ${a})`;
    ctx.fillRect(0, 0, canvas.width, canvas.height);
  },

  draw_sprite(texture: HTMLImageElement, x: number, y: number): void {
    ctx.drawImage(texture, x, y);
  },

  draw_sprite_scaled(
    texture: HTMLImageElement,
    x: number,
    y: number,
    scale_x: number,
    scale_y: number,
  ): void {
    ctx.save();
    ctx.translate(x, y);
    ctx.scale(scale_x, scale_y);
    ctx.drawImage(texture, 0, 0);
    ctx.restore();
  },

  draw_sprite_rotated(
    texture: HTMLImageElement,
    x: number,
    y: number,
    angle: number,
  ): void {
    ctx.save();
    ctx.translate(x, y);
    ctx.rotate(angle);
    ctx.drawImage(texture, -texture.width / 2, -texture.height / 2);
    ctx.restore();
  },

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
    ctx.fillStyle = `rgba(${r * 255}, ${g * 255}, ${b * 255}, ${a})`;
    ctx.fillRect(x, y, w, h);
  },

  draw_text(text: string, x: number, y: number, size: number): void {
    ctx.font = `${size}px sans-serif`;
    ctx.fillStyle = "white";
    ctx.fillText(text, x, y);
  },

  load_texture(src: string): Promise<HTMLImageElement> {
    return new Promise((resolve, reject) => {
      const img = new Image();
      img.onload = () => resolve(img);
      img.onerror = reject;
      img.src = src;
    });
  },

  surface_width(): number {
    return canvas.width;
  },

  surface_height(): number {
    return canvas.height;
  },

  present(): void {
    // Canvas2D is immediate-mode; no explicit present needed.
  },
};
