/** GML color constants and utilities. */

// GML uses BGR byte order (R in low bits).
export const Colors: Record<string, number> = {
  c_white: 0xffffff,
  c_silver: 0xc0c0c0,
  c_ltgray: 0xc0c0c0,
  c_gray: 0x808080,
  c_dkgray: 0x404040,
  c_black: 0x000000,
  c_aqua: 0xffff00,
  c_fuchsia: 0xff00ff,
  c_yellow: 0x00ffff,
  c_teal: 0x808000,
  c_purple: 0x800080,
  c_olive: 0x008080,
  c_red: 0x0000ff,
  c_lime: 0x00ff00,
  c_blue: 0xff0000,
  c_maroon: 0x000080,
  c_green: 0x008000,
  c_navy: 0x800000,
  c_orange: 0x40a0ff,
};

export const HAligns: Record<string, number> = {
  fa_left: 0,
  fa_center: 1,
  fa_right: 2,
};

export const VAligns: Record<string, number> = {
  fa_top: 0,
  fa_middle: 1,
  fa_bottom: 2,
};

export function color_get_red(color: number): number { return color & 0xff; }
export function color_get_green(color: number): number { return (color >> 8) & 0xff; }
export function color_get_blue(color: number): number { return color >> 16; }

export function make_color_rgb(r: number, g: number, b: number): number {
  return (b << 16) | (g << 8) | r;
}

export function merge_color(col1: number, col2: number, amount: number): number {
  return make_color_rgb(
    Math.round(color_get_red(col1) * (1 - amount) + color_get_red(col2) * amount),
    Math.round(color_get_green(col1) * (1 - amount) + color_get_green(col2) * amount),
    Math.round(color_get_blue(col1) * (1 - amount) + color_get_blue(col2) * amount),
  );
}

export function color_get_hue(color: number): number {
  const r = (color & 0xff) / 255;
  const g = ((color >> 8) & 0xff) / 255;
  const b = (color >> 16) / 255;
  const max = Math.max(r, g, b);
  const min = Math.min(r, g, b);
  const d = max - min;
  if (d === 0) return 0;
  let h: number;
  if (max === r) h = ((g - b) / d) % 6;
  else if (max === g) h = (b - r) / d + 2;
  else h = (r - g) / d + 4;
  h = Math.round(h * 255 / 6);
  if (h < 0) h += 255;
  return h;
}

export function make_color_hsv(h: number, s: number, v: number): number {
  const hf = (h / 255) * 6;
  const sf = s / 255;
  const vf = v / 255;
  const c = vf * sf;
  const x = c * (1 - Math.abs(hf % 2 - 1));
  const m = vf - c;
  let r: number, g: number, b: number;
  if (hf < 1) { r = c; g = x; b = 0; }
  else if (hf < 2) { r = x; g = c; b = 0; }
  else if (hf < 3) { r = 0; g = c; b = x; }
  else if (hf < 4) { r = 0; g = x; b = c; }
  else if (hf < 5) { r = x; g = 0; b = c; }
  else { r = c; g = 0; b = x; }
  return make_color_rgb(
    Math.round((r + m) * 255),
    Math.round((g + m) * 255),
    Math.round((b + m) * 255),
  );
}

/** Convert a GML BGR color to a CSS hex string. */
export function gmlColorToCss(color: number): string {
  const r = color & 0xff;
  const g = (color >> 8) & 0xff;
  const b = color >> 16;
  return "#" + ((1 << 24) | (r << 16) | (g << 8) | b).toString(16).slice(1);
}
