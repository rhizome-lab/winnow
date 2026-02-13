/** GML ini file functions — backed by localStorage. */

let __gml_ini_path = "";
let __gml_ini_contents: Record<string, Record<string, string>> = {};
let __gml_game_name = "";

export function __gml_set_game_name(name: string): void {
  __gml_game_name = name;
}

export function ini_open(path: string): void {
  __gml_ini_path = path;
  const raw = localStorage.getItem("__gml_fs_" + __gml_game_name + "_" + path);
  ini_open_from_string(raw);
}

export function ini_open_from_string(str: string | null): void {
  if (!str) {
    __gml_ini_contents = {};
    return;
  }
  const sections: Record<string, Record<string, string>> = {};
  const sectionList = str.split(/\s+(?=\[[^\]]+\])/g);
  for (const sectionStr of sectionList) {
    const m = sectionStr.match(/^(?:\[([^\]]+)\])([\s\S]+)/);
    if (!m) continue;
    const [, name, contents] = m;
    const section: Record<string, string> = {};
    const keyList = contents.trim().split(/\s+(?=.+=.+)/g);
    for (const kv of keyList) {
      const km = kv.match(/(.+?)=(.+)/);
      if (km) section[km[1]] = km[2];
    }
    sections[name] = section;
  }
  __gml_ini_contents = sections;
}

export function ini_read_real(section: string, key: string, defaultVal: number): number {
  return +ini_read_string(section, key, String(defaultVal));
}

export function ini_read_string(section: string, key: string, defaultVal: string): string {
  const val = (__gml_ini_contents[section] || {})[key];
  return val === undefined ? defaultVal : val;
}

export function ini_write_real(section: string, key: string, value: number): void {
  ini_write_string(section, key, String(value));
}

export function ini_write_string(section: string, key: string, value: string): void {
  if (__gml_ini_contents[section] === undefined) {
    __gml_ini_contents[section] = {};
  }
  __gml_ini_contents[section][key] = String(value);
}

export function ini_section_exists(section: string): boolean {
  return __gml_ini_contents[section] !== undefined;
}

export function ini_key_exists(section: string, key: string): boolean {
  return __gml_ini_contents[section] !== undefined && __gml_ini_contents[section][key] !== undefined;
}

export function ini_section_delete(section: string): void {
  delete __gml_ini_contents[section];
}

export function ini_key_delete(section: string, key: string): void {
  if (__gml_ini_contents[section]) {
    delete __gml_ini_contents[section][key];
  }
}

export function ini_close(): string {
  let result = "";
  for (const section in __gml_ini_contents) {
    result += `[${section}]\n`;
    for (const key in __gml_ini_contents[section]) {
      result += `${key}=${__gml_ini_contents[section][key]}\n`;
    }
    result += "\n";
  }
  localStorage.setItem("__gml_fs_" + __gml_game_name + "_" + __gml_ini_path, result);
  __gml_ini_path = "";
  __gml_ini_contents = {};
  return result;
}
