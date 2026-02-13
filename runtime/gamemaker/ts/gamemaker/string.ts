/** GML string functions. */

export function string_length(s: string): number { return s.length; }

export function string_copy(s: string, index: number, count: number): string {
  return s.slice(index - 1, index - 1 + count);
}

export function string_insert(sub: string, s: string, index: number): string {
  return s.slice(0, index) + sub + s.slice(index);
}

export function string_replace_all(content: string, replacee: string, replacer: string): string {
  return content.split(replacee).join(replacer);
}

export function string_lower(s: string): string { return s.toLowerCase(); }
export function string_upper(s: string): string { return s.toUpperCase(); }

export function string_char_at(s: string, index: number): string { return s.charAt(index - 1); }
export function string_pos(sub: string, s: string): number { return s.indexOf(sub) + 1; }
export function string_delete(s: string, index: number, count: number): string {
  return s.slice(0, index - 1) + s.slice(index - 1 + count);
}
export function string_count(sub: string, s: string): number {
  return s.split(sub).length - 1;
}

// GML's string() is just String() — emitted code calls String() directly.
