/** GML drawing functions — sprites, text, primitives. */

import type { GameRuntime } from "./runtime";
import { Colors, HAligns, VAligns, gmlColorToCss } from "./color";

export class DrawState {
  alpha = 1;
  config = {
    color: Colors.c_white,
    font: 0,
    valign: VAligns.fa_top,
    halign: HAligns.fa_left,
    ext: { sep: -1, w: -1 },
    transform: { xscale: 1, yscale: 1, angle: 0 },
  };
  fontLookups: Map<number, Map<number, any>>[] = [];
  colorFontCache: Map<number, any>[][] = [];
}

export function createDrawAPI(rt: GameRuntime) {
  const draw = rt._draw;

  // ---- Helpers ----

  function getFontLookup(fontIdx: number): Map<number, any> {
    if (!draw.fontLookups[fontIdx]) {
      const map = new Map<number, any>();
      const font = rt.fonts[fontIdx];
      if (font) {
        for (const c of font.chars) {
          map.set(c.char, c);
        }
      }
      draw.fontLookups[fontIdx] = [map];
    }
    return draw.fontLookups[fontIdx][0];
  }

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
    if (!draw.colorFontCache[fontIdx]) draw.colorFontCache[fontIdx] = [];
    const cached = draw.colorFontCache[fontIdx][color as any];
    if (cached) return cached;

    const tc = rt._gfx.tcanvas;
    const tcx = rt._gfx.tctx;
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
      draw.colorFontCache[fontIdx][color as any] = bm;
      return bm;
    }
    return null;
  }

  // ---- Draw config setters ----

  function draw_set_color(color: number): void { draw.config.color = color; }
  function draw_set_font(font: number): void { draw.config.font = font; }
  function draw_set_halign(halign: number): void { draw.config.halign = halign; }
  function draw_set_valign(valign: number): void { draw.config.valign = valign; }

  function draw_set_alpha(alpha: number): void { draw.alpha = alpha; }
  function draw_get_alpha(): number { return draw.alpha; }
  function draw_get_colour(): number { return draw.config.color; }

  // ---- Sprite drawing ----

  function drawSprite(
    spriteIndex: number, imageIndex: number, x: number, y: number,
    opts?: { image_alpha?: number; image_xscale?: number; image_yscale?: number },
  ): void {
    const ctx = rt._gfx.ctx;
    const alpha = opts?.image_alpha ?? 1;
    if (alpha !== draw.alpha) {
      ctx.globalAlpha = draw.alpha = alpha;
    }
    const sprite = rt.sprites[spriteIndex];
    if (!sprite) return;
    const texIdx = sprite.rt.textures[imageIndex] ?? sprite.rt.textures[0];
    if (texIdx === undefined) return;
    const tex = rt.textures[texIdx];
    if (!tex) return;
    const sheet = rt.textureSheets[tex.sheetId];
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

  function draw_sprite(spriteIndex: number, imageIndex: number, x: number, y: number): void {
    drawSprite(spriteIndex, imageIndex, x, y);
  }

  function draw_sprite_ext(
    spriteIndex: number, imageIndex: number, x: number, y: number,
    xscale: number, yscale: number, rot: number, _color: number, alpha: number,
  ): void {
    const ctx = rt._gfx.ctx;
    ctx.save();
    ctx.translate(x, y);
    ctx.scale(xscale, yscale);
    if (rot !== 0) ctx.rotate(-rot * Math.PI / 180);
    if (alpha !== draw.alpha) ctx.globalAlpha = draw.alpha = alpha;
    const sprite = rt.sprites[spriteIndex];
    if (sprite) {
      const texIdx = sprite.rt.textures[imageIndex] ?? sprite.rt.textures[0];
      if (texIdx !== undefined) {
        const tex = rt.textures[texIdx];
        if (tex) {
          const sheet = rt.textureSheets[tex.sheetId];
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

  function draw_self(): void {}

  function sprite_get_width(spriteIndex: number): number {
    return rt.sprites[spriteIndex]?.size.width ?? 0;
  }

  function sprite_get_height(spriteIndex: number): number {
    return rt.sprites[spriteIndex]?.size.height ?? 0;
  }

  // ---- Text drawing ----

  function draw_text(x: number, y: number, text: string): void {
    const ctx = rt._gfx.ctx;
    const font = rt.fonts[draw.config.font];
    if (!font) return;
    const lookup = getFontLookup(draw.config.font);
    const lines = String(text).split("#");
    const mGlyph = lookup.get(77); // 'M'
    const lineHeight = draw.config.ext.sep === -1
      ? (mGlyph?.frame.height ?? 16)
      : draw.config.ext.sep;
    const height = lineHeight * lines.length;

    if (draw.alpha !== 1) {
      ctx.globalAlpha = draw.alpha = 1;
    }

    let y2 = y;
    if (draw.config.valign === VAligns.fa_middle) y2 -= Math.ceil(height / 2);
    else if (draw.config.valign === VAligns.fa_bottom) y2 -= height;

    // Word-wrap if w > 0
    if (draw.config.ext.w > 0) {
      wrapLines(lines, lookup, draw.config.ext.w);
    }

    const texIdx = font.texture;
    const tex = rt.textures[texIdx];
    if (!tex) return;
    let sheet: CanvasImageSource = rt.textureSheets[tex.sheetId];
    if (!sheet) return;
    let bx = tex.src.x;
    let by = tex.src.y;

    // Color tinting
    if (draw.config.color !== 0xffffff) {
      bx = 0;
      by = 0;
      const cached = getCachedColorFont(draw.config.font, draw.config.color, sheet, tex);
      if (cached) sheet = cached;
    }

    for (const line of lines) {
      let width = 0;
      for (const ch of line) {
        const g = lookup.get(ch.charCodeAt(0));
        if (g) width += g.shift;
      }
      let x2 = x;
      if (draw.config.halign === HAligns.fa_center) x2 -= Math.ceil(width / 2);
      else if (draw.config.halign === HAligns.fa_right) x2 -= width;

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

  function draw_text_ext(x: number, y: number, text: string, sep: number, w: number): void {
    const old = draw.config.ext;
    draw.config.ext = { sep, w };
    draw_text(x, y, text);
    draw.config.ext = old;
  }

  function draw_text_color(
    x: number, y: number, text: string,
    c1: number, _c2: number, _c3: number, _c4: number, alpha: number,
  ): void {
    const oldColor = draw.config.color;
    const oldAlpha = draw.alpha;
    draw.config.color = c1;
    draw.alpha = alpha;
    draw_text(x, y, text);
    draw.config.color = oldColor;
    draw.alpha = oldAlpha;
  }

  function draw_text_transformed(
    x: number, y: number, text: string,
    xscale: number, yscale: number, angle: number,
  ): void {
    const ctx = rt._gfx.ctx;
    ctx.save();
    ctx.translate(x, y);
    ctx.scale(xscale, yscale);
    if (angle !== 0) ctx.rotate(-angle * Math.PI / 180);
    draw_text(0, 0, text);
    ctx.restore();
  }

  function draw_text_ext_color(
    x: number, y: number, text: string, sep: number, w: number,
    c1: number, _c2: number, _c3: number, _c4: number, alpha: number,
  ): void {
    const oldColor = draw.config.color;
    const oldAlpha = draw.alpha;
    const oldExt = draw.config.ext;
    draw.config.color = c1;
    draw.alpha = alpha;
    draw.config.ext = { sep, w };
    draw_text(x, y, text);
    draw.config.color = oldColor;
    draw.alpha = oldAlpha;
    draw.config.ext = oldExt;
  }

  function draw_text_ext_transformed(
    x: number, y: number, text: string, sep: number, w: number,
    xscale: number, yscale: number, angle: number,
  ): void {
    const ctx = rt._gfx.ctx;
    const oldExt = draw.config.ext;
    draw.config.ext = { sep, w };
    ctx.save();
    ctx.translate(x, y);
    ctx.scale(xscale, yscale);
    if (angle !== 0) ctx.rotate(-angle * Math.PI / 180);
    draw_text(0, 0, text);
    ctx.restore();
    draw.config.ext = oldExt;
  }

  function draw_text_transformed_color(
    x: number, y: number, text: string,
    xscale: number, yscale: number, angle: number,
    c1: number, _c2: number, _c3: number, _c4: number, alpha: number,
  ): void {
    const ctx = rt._gfx.ctx;
    const oldColor = draw.config.color;
    const oldAlpha = draw.alpha;
    draw.config.color = c1;
    draw.alpha = alpha;
    ctx.save();
    ctx.translate(x, y);
    ctx.scale(xscale, yscale);
    if (angle !== 0) ctx.rotate(-angle * Math.PI / 180);
    draw_text(0, 0, text);
    ctx.restore();
    draw.config.color = oldColor;
    draw.alpha = oldAlpha;
  }

  function draw_text_ext_transformed_color(
    x: number, y: number, text: string, sep: number, w: number,
    xscale: number, yscale: number, angle: number,
    c1: number, _c2: number, _c3: number, _c4: number, alpha: number,
  ): void {
    const ctx = rt._gfx.ctx;
    const oldColor = draw.config.color;
    const oldAlpha = draw.alpha;
    const oldExt = draw.config.ext;
    draw.config.color = c1;
    draw.alpha = alpha;
    draw.config.ext = { sep, w };
    ctx.save();
    ctx.translate(x, y);
    ctx.scale(xscale, yscale);
    if (angle !== 0) ctx.rotate(-angle * Math.PI / 180);
    draw_text(0, 0, text);
    ctx.restore();
    draw.config.color = oldColor;
    draw.alpha = oldAlpha;
    draw.config.ext = oldExt;
  }

  function string_height_ext(text: string, sep: number, w: number): number {
    const font = rt.fonts[draw.config.font];
    if (!font) return 0;
    const lookup = getFontLookup(draw.config.font);
    const mGlyph = lookup.get(77);
    const h = sep === -1 ? (mGlyph?.frame.height ?? 16) : sep;
    const lines = String(text).split("#");
    if (w > 0) wrapLines(lines, lookup, w);
    return lines.length * h;
  }

  // ---- Rectangle drawing ----

  function draw_rectangle(x1: number, y1: number, x2: number, y2: number, outline: boolean): void {
    const ctx = rt._gfx.ctx;
    const css = gmlColorToCss(draw.config.color);
    if (outline) {
      ctx.strokeStyle = css;
      ctx.strokeRect(x1, y1, x2 - x1 + 1, y2 - y1 + 1);
    } else {
      ctx.fillStyle = css;
      ctx.fillRect(x1, y1, x2 - x1 + 1, y2 - y1 + 1);
    }
  }

  function draw_set_ext(sep: number, w: number): void {
    draw.config.ext = { sep, w };
  }

  return {
    drawSprite, draw_sprite, draw_sprite_ext, draw_self,
    sprite_get_width, sprite_get_height,
    draw_text, draw_text_ext, draw_text_color, draw_text_transformed,
    draw_text_ext_color, draw_text_ext_transformed,
    draw_text_transformed_color, draw_text_ext_transformed_color,
    string_height_ext,
    draw_rectangle,
    draw_set_color, draw_set_font, draw_set_halign, draw_set_valign,
    draw_set_alpha, draw_get_alpha, draw_get_colour,
    draw_set_ext,
  };
}
