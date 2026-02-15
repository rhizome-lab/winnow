/** SugarCube Settings API.
 *
 * Manages user-configurable settings registered by story authors.
 * Settings are persisted to localStorage via the platform layer.
 */

import { loadLocal, saveLocal, removeLocal, type SettingUIEntry, showSettingsUI } from "../platform";

const STORAGE_KEY = "reincarnate-settings";

// --- Setting definitions ---

interface ToggleDef {
  type: "toggle";
  label: string;
  default: boolean;
  desc?: string;
  onChange?: (value: any) => void;
}

interface ListDef {
  type: "list";
  label: string;
  default: any;
  list: any[];
  desc?: string;
  onChange?: (value: any) => void;
}

interface RangeDef {
  type: "range";
  label: string;
  default: number;
  min: number;
  max: number;
  step: number;
  desc?: string;
  onChange?: (value: any) => void;
}

type SettingDef = ToggleDef | ListDef | RangeDef;

const definitions: Map<string, SettingDef> = new Map();
const values: Record<string, any> = {};

let register: ((id: string, binding: string, handler: () => void) => void) | null = null;

// --- Registration ---

/** Register a toggle (boolean) setting. */
export function addToggle(name: string, def: {
  label: string;
  default?: boolean;
  desc?: string;
  onChange?: (value: any) => void;
}): void {
  const setting: ToggleDef = {
    type: "toggle",
    label: def.label,
    default: def.default ?? false,
    desc: def.desc,
    onChange: def.onChange,
  };
  definitions.set(name, setting);
  if (!(name in values)) {
    values[name] = setting.default;
  }
  if (register) {
    register(`toggle-${name}`, "", () => {
      set(name, !get(name));
      save();
    });
  }
}

/** Register a list (select) setting. */
export function addList(name: string, def: {
  label: string;
  list: any[];
  default?: any;
  desc?: string;
  onChange?: (value: any) => void;
}): void {
  const setting: ListDef = {
    type: "list",
    label: def.label,
    default: def.default ?? def.list[0],
    list: def.list,
    desc: def.desc,
    onChange: def.onChange,
  };
  definitions.set(name, setting);
  if (!(name in values)) {
    values[name] = setting.default;
  }
  if (register) {
    for (const opt of def.list) {
      register(`set-${name}-${opt}`, "", () => {
        set(name, opt);
        save();
      });
    }
  }
}

/** Register a range (slider) setting. */
export function addRange(name: string, def: {
  label: string;
  min: number;
  max: number;
  step: number;
  default?: number;
  desc?: string;
  onChange?: (value: any) => void;
}): void {
  const setting: RangeDef = {
    type: "range",
    label: def.label,
    default: def.default ?? def.min,
    min: def.min,
    max: def.max,
    step: def.step,
    desc: def.desc,
    onChange: def.onChange,
  };
  definitions.set(name, setting);
  if (!(name in values)) {
    values[name] = setting.default;
  }
}

// --- Value access ---

/** Get the current value of a setting. */
export function get(name: string): any {
  return values[name];
}

/** Set a setting value and fire its onChange callback. */
export function set(name: string, value: any): void {
  values[name] = value;
  const def = definitions.get(name);
  if (def?.onChange) {
    def.onChange(value);
  }
}

// --- Persistence ---

/** Load settings from localStorage, applying saved values over defaults. */
export function load(): void {
  const raw = loadLocal(STORAGE_KEY);
  if (raw) {
    try {
      const saved = JSON.parse(raw);
      for (const [key, val] of Object.entries(saved)) {
        if (definitions.has(key)) {
          values[key] = val;
        }
      }
    } catch {
      // Corrupt data — ignore
    }
  }
}

/** Save current settings to localStorage. */
export function save(): void {
  const toSave: Record<string, any> = {};
  for (const [key] of definitions) {
    if (key in values) {
      toSave[key] = values[key];
    }
  }
  saveLocal(STORAGE_KEY, JSON.stringify(toSave));
}

/** Reset all settings to their defaults. */
export function reset(): void {
  for (const [key, def] of definitions) {
    const prev = values[key];
    values[key] = def.default;
    if (def.onChange && prev !== def.default) {
      def.onChange(def.default);
    }
  }
  removeLocal(STORAGE_KEY);
}

/** Get all registered setting definitions (for UI rendering). */
export function getDefinitions(): Map<string, SettingDef> {
  return definitions;
}

/** Register commands for settings management. */
export function initCommands(registerCommand: (id: string, binding: string, handler: () => void) => void): void {
  register = registerCommand;
  registerCommand("open-settings", "", () => {
    const entries: SettingUIEntry[] = [];
    for (const [name, def] of definitions) {
      entries.push({
        name,
        type: def.type,
        label: def.label,
        desc: def.desc,
        value: get(name),
        ...(def.type === "list" ? { list: (def as any).list } : {}),
        ...(def.type === "range" ? { min: (def as any).min, max: (def as any).max, step: (def as any).step } : {}),
      });
    }
    showSettingsUI(
      entries,
      (n, v) => { set(n, v); save(); },
      () => reset(),
    );
  });
  registerCommand("reset-settings", "", () => reset());
}
