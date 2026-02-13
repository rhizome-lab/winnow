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

/** Convert a GML BGR color to a CSS hex string. */
export function gmlColorToCss(color: number): string {
  const r = color & 0xff;
  const g = (color >> 8) & 0xff;
  const b = color >> 16;
  return "#" + ((1 << 24) | (r << 16) | (g << 8) | b).toString(16).slice(1);
}
