/** GML drawing functions — sprites, text, primitives. */

import { getCtx, getTintCanvas, getTintCtx } from "./platform";
import { __gml_sprites, __gml_textures, __gml_texture_sheets, __gml_fonts } from "./runtime";
import { Colors, HAligns, VAligns, gmlColorToCss } from "./color";

let __gml_alpha = 1;

const __gml_draw_config = {
  color: Colors.c_white,
  font: 0,
  valign: VAligns.fa_top,
  halign: HAligns.fa_left,
  ext: { sep: -1, w: -1 },
  transform: { xscale: 1, yscale: 1, angle: 0 },
};

// ---- Draw config setters ----

export function draw_set_color(color: number): void { __gml_draw_config.color = color; }
export function draw_set_font(font: number): void { __gml_draw_config.font = font; }
export function draw_set_halign(halign: number): void { __gml_draw_config.halign = halign; }
export function draw_set_valign(valign: number): void { __gml_draw_config.valign = valign; }

export function draw_set_alpha(alpha: number): void { __gml_alpha = alpha; }
export function draw_get_alpha(): number { return __gml_alpha; }
export function draw_get_colour(): number { return __gml_draw_config.color; }

// ---- Sprite drawing ----

export function drawSprite(
  spriteIndex: number, imageIndex: number, x: number, y: number,
  opts?: { image_alpha?: number; image_xscale?: number; image_yscale?: number },
): void {
  const ctx = getCtx();
  const alpha = opts?.image_alpha ?? 1;
  if (alpha !== __gml_alpha) {
    ctx.globalAlpha = __gml_alpha = alpha;
  }
  const sprite = __gml_sprites[spriteIndex];
  if (!sprite) return;
  const texIdx = sprite.textures[imageIndex] ?? sprite.textures[0];
  if (texIdx === undefined) return;
  const tex = __gml_textures[texIdx];
  if (!tex) return;
  const sheet = __gml_texture_sheets[tex.sheetId];
  if (!sheet) return;

  const xscale = opts?.image_xscale;
  const yscale = opts?.image_yscale;
  if (xscale !== undefined || yscale !== undefined) {
    const sx = xscale ?? 1;
    const sy = yscale ?? 1;
    ctx.save();
    ctx.scale(sx, sy);
    ctx.drawImage(
      sheet, tex.src.x, tex.src.y, tex.src.w, tex.src.h,
      x / sx - sprite.origin.x, y / sy - sprite.origin.y,
      tex.dest.w, tex.dest.h,
    );
    ctx.restore();
  } else {
    ctx.drawImage(
      sheet, tex.src.x, tex.src.y, tex.src.w, tex.src.h,
      x - sprite.origin.x, y - sprite.origin.y,
      tex.dest.w, tex.dest.h,
    );
  }
}

export function draw_sprite(spriteIndex: number, imageIndex: number, x: number, y: number): void {
  drawSprite(spriteIndex, imageIndex, x, y);
}

export function draw_sprite_ext(
  spriteIndex: number, imageIndex: number, x: number, y: number,
  xscale: number, yscale: number, rot: number, _color: number, alpha: number,
): void {
  const ctx = getCtx();
  ctx.save();
  ctx.translate(x, y);
  ctx.scale(xscale, yscale);
  if (rot !== 0) ctx.rotate(-rot * Math.PI / 180);
  if (alpha !== __gml_alpha) ctx.globalAlpha = __gml_alpha = alpha;
  const sprite = __gml_sprites[spriteIndex];
  if (sprite) {
    const texIdx = sprite.textures[imageIndex] ?? sprite.textures[0];
    if (texIdx !== undefined) {
      const tex = __gml_textures[texIdx];
      if (tex) {
        const sheet = __gml_texture_sheets[tex.sheetId];
        if (sheet) {
          ctx.drawImage(
            sheet, tex.src.x, tex.src.y, tex.src.w, tex.src.h,
            -sprite.origin.x, -sprite.origin.y,
            tex.dest.w, tex.dest.h,
          );
        }
      }
    }
  }
  ctx.restore();
}

export function draw_self(): void {
  // `this` is bound via .call() from the game loop
}

export function sprite_get_width(spriteIndex: number): number {
  return __gml_sprites[spriteIndex]?.size.width ?? 0;
}

export function sprite_get_height(spriteIndex: number): number {
  return __gml_sprites[spriteIndex]?.size.height ?? 0;
}

// ---- Text drawing ----

// Font lookup tables (char code → glyph), built on first use.
const fontLookups: Map<number, Map<number, any>>[] = [];

function getFontLookup(fontIdx: number): Map<number, any> {
  if (!fontLookups[fontIdx]) {
    const map = new Map<number, any>();
    const font = __gml_fonts[fontIdx];
    if (font) {
      for (const c of font.chars) {
        map.set(c.char, c);
      }
    }
    fontLookups[fontIdx] = [map];
  }
  return fontLookups[fontIdx][0];
}

// Color-tinted font cache: [fontIdx][color] → ImageBitmap
const colorFontCache: Map<number, any>[][] = [];

export function draw_text(x: number, y: number, text: string): void {
  const ctx = getCtx();
  const font = __gml_fonts[__gml_draw_config.font];
  if (!font) return;
  const lookup = getFontLookup(__gml_draw_config.font);
  const lines = String(text).split("#");
  const mGlyph = lookup.get(77); // 'M'
  const lineHeight = __gml_draw_config.ext.sep === -1
    ? (mGlyph?.frame.height ?? 16)
    : __gml_draw_config.ext.sep;
  const height = lineHeight * lines.length;

  if (__gml_alpha !== 1) {
    ctx.globalAlpha = __gml_alpha = 1;
  }

  let y2 = y;
  if (__gml_draw_config.valign === VAligns.fa_middle) y2 -= Math.ceil(height / 2);
  else if (__gml_draw_config.valign === VAligns.fa_bottom) y2 -= height;

  // Word-wrap if w > 0
  if (__gml_draw_config.ext.w > 0) {
    wrapLines(lines, lookup, __gml_draw_config.ext.w);
  }

  const texIdx = font.texture;
  const tex = __gml_textures[texIdx];
  if (!tex) return;
  let sheet: CanvasImageSource = __gml_texture_sheets[tex.sheetId];
  if (!sheet) return;
  let bx = tex.src.x;
  let by = tex.src.y;

  // Color tinting
  if (__gml_draw_config.color !== 0xffffff) {
    bx = 0;
    by = 0;
    const cached = getCachedColorFont(__gml_draw_config.font, __gml_draw_config.color, sheet, tex);
    if (cached) sheet = cached;
  }

  for (const line of lines) {
    let width = 0;
    for (const ch of line) {
      const g = lookup.get(ch.charCodeAt(0));
      if (g) width += g.shift;
    }
    let x2 = x;
    if (__gml_draw_config.halign === HAligns.fa_center) x2 -= Math.ceil(width / 2);
    else if (__gml_draw_config.halign === HAligns.fa_right) x2 -= width;

    for (const ch of line) {
      const g = lookup.get(ch.charCodeAt(0));
      if (g) {
        ctx.drawImage(
          sheet, bx + g.frame.x, by + g.frame.y, g.frame.width, g.frame.height,
          x2 + g.offset, y2, g.frame.width, g.frame.height,
        );
        x2 += g.shift;
      }
    }
    y2 += lineHeight;
  }
}

export function draw_text_ext(x: number, y: number, text: string, sep: number, w: number): void {
  const old = __gml_draw_config.ext;
  __gml_draw_config.ext = { sep, w };
  draw_text(x, y, text);
  __gml_draw_config.ext = old;
}

export function draw_text_color(
  x: number, y: number, text: string,
  c1: number, _c2: number, _c3: number, _c4: number, alpha: number,
): void {
  const oldColor = __gml_draw_config.color;
  const oldAlpha = __gml_alpha;
  __gml_draw_config.color = c1;
  __gml_alpha = alpha;
  draw_text(x, y, text);
  __gml_draw_config.color = oldColor;
  __gml_alpha = oldAlpha;
}

export function draw_text_transformed(
  x: number, y: number, text: string,
  xscale: number, yscale: number, angle: number,
): void {
  const ctx = getCtx();
  ctx.save();
  ctx.translate(x, y);
  ctx.scale(xscale, yscale);
  if (angle !== 0) ctx.rotate(-angle * Math.PI / 180);
  draw_text(0, 0, text);
  ctx.restore();
}

export function draw_text_ext_color(
  x: number, y: number, text: string, sep: number, w: number,
  c1: number, _c2: number, _c3: number, _c4: number, alpha: number,
): void {
  const oldColor = __gml_draw_config.color;
  const oldAlpha = __gml_alpha;
  const oldExt = __gml_draw_config.ext;
  __gml_draw_config.color = c1;
  __gml_alpha = alpha;
  __gml_draw_config.ext = { sep, w };
  draw_text(x, y, text);
  __gml_draw_config.color = oldColor;
  __gml_alpha = oldAlpha;
  __gml_draw_config.ext = oldExt;
}

export function string_height_ext(text: string, sep: number, w: number): number {
  const font = __gml_fonts[__gml_draw_config.font];
  if (!font) return 0;
  const lookup = getFontLookup(__gml_draw_config.font);
  const mGlyph = lookup.get(77);
  const h = sep === -1 ? (mGlyph?.frame.height ?? 16) : sep;
  const lines = String(text).split("#");
  if (w > 0) wrapLines(lines, lookup, w);
  return lines.length * h;
}

// ---- Rectangle drawing ----

export function draw_rectangle(x1: number, y1: number, x2: number, y2: number, outline: boolean): void {
  const ctx = getCtx();
  const css = gmlColorToCss(__gml_draw_config.color);
  if (outline) {
    ctx.strokeStyle = css;
    ctx.strokeRect(x1, y1, x2 - x1 + 1, y2 - y1 + 1);
  } else {
    ctx.fillStyle = css;
    ctx.fillRect(x1, y1, x2 - x1 + 1, y2 - y1 + 1);
  }
}

export function draw_set_ext(sep: number, w: number): void {
  __gml_draw_config.ext = { sep, w };
}

// ---- Helpers ----

function wrapLines(lines: string[], lookup: Map<number, any>, maxWidth: number): void {
  for (let i = 0; i < lines.length; i++) {
    const line = lines[i];
    let j = 0;
    let width = 0;
    for (; j < line.length; j++) {
      const g = lookup.get(line.charCodeAt(j));
      if (g) width += g.shift;
      if (width > maxWidth) {
        let breakAt = j;
        while (breakAt > 0 && line[breakAt] !== " ") breakAt--;
        if (breakAt === 0) break;
        lines[i] = line.slice(0, breakAt);
        lines.splice(i + 1, 0, line.slice(breakAt + 1));
        break;
      }
    }
  }
}

function getCachedColorFont(
  fontIdx: number, color: number,
  sheet: CanvasImageSource, tex: any,
): ImageBitmap | null {
  if (!colorFontCache[fontIdx]) colorFontCache[fontIdx] = [];
  const cached = colorFontCache[fontIdx][color as any];
  if (cached) return cached;

  const tc = getTintCanvas();
  const tcx = getTintCtx();
  const w = (sheet as any).width ?? tex.src.w;
  const h = (sheet as any).height ?? tex.src.h;
  if ("width" in tc) (tc as any).width = w;
  if ("height" in tc) (tc as any).height = h;
  tcx.clearRect(0, 0, w, h);
  tcx.drawImage(
    sheet, tex.src.x, tex.src.y, tex.src.w, tex.src.h,
    0, 0, tex.dest.w, tex.dest.h,
  );
  const imageData = tcx.getImageData(0, 0, tex.src.w, tex.src.h);
  const data = imageData.data;
  const r = color & 0xff;
  const g = (color >> 8) & 0xff;
  const b = color >> 16;
  for (let i = 0; i < data.length; i += 4) {
    data[i] = data[i] * r / 255;
    data[i + 1] = data[i + 1] * g / 255;
    data[i + 2] = data[i + 2] * b / 255;
  }
  tcx.clearRect(0, 0, w, h);
  tcx.putImageData(imageData, 0, 0);
  if ("transferToImageBitmap" in tc) {
    const bm = (tc as OffscreenCanvas).transferToImageBitmap();
    colorFontCache[fontIdx][color as any] = bm;
    return bm;
  }
  return null;
}
