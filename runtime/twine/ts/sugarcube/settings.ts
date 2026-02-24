/** SugarCube Settings API.
 *
 * Manages user-configurable settings registered by story authors.
 * Settings are persisted to localStorage via the platform layer.
 */

import { loadLocal, saveLocal, removeLocal, type SettingUIEntry } from "../platform";

const STORAGE_KEY = "reincarnate-settings";

// --- Setting definitions ---

interface ToggleDef {
  type: "toggle";
  label: string;
  default: boolean;
  desc?: string;
  onChange?: (value: boolean) => void;
}

interface ListDef {
  type: "list";
  label: string;
  default: string;
  list: string[];
  desc?: string;
  onChange?: (value: string) => void;
}

interface RangeDef {
  type: "range";
  label: string;
  default: number;
  min: number;
  max: number;
  step: number;
  desc?: string;
  onChange?: (value: number) => void;
}

type SettingDef = ToggleDef | ListDef | RangeDef;

export class SCSettings {
  private definitions: Map<string, SettingDef> = new Map();
  private values: Record<string, boolean | number | string> = {};
  private register: ((id: string, binding: string, handler: () => void) => void) | null = null;

  // --- Registration ---

  /** Register a toggle (boolean) setting. */
  addToggle(name: string, def: {
    label: string;
    default?: boolean;
    desc?: string;
    onChange?: (value: boolean) => void;
  }): void {
    const setting: ToggleDef = {
      type: "toggle",
      label: def.label,
      default: def.default ?? false,
      ...(def.desc !== undefined ? { desc: def.desc } : {}),
      ...(def.onChange !== undefined ? { onChange: def.onChange } : {}),
    };
    this.definitions.set(name, setting);
    if (!(name in this.values)) {
      this.values[name] = setting.default;
    }
    if (this.register) {
      this.register(`toggle-${name}`, "", () => {
        this.set(name, !this.get(name));
        this.save();
      });
    }
  }

  /** Register a list (select) setting. */
  addList(name: string, def: {
    label: string;
    list: string[];
    default?: string;
    desc?: string;
    onChange?: (value: string) => void;
  }): void {
    const setting: ListDef = {
      type: "list",
      label: def.label,
      default: def.default ?? def.list[0]!,
      list: def.list,
      ...(def.desc !== undefined ? { desc: def.desc } : {}),
      ...(def.onChange !== undefined ? { onChange: def.onChange } : {}),
    };
    this.definitions.set(name, setting);
    if (!(name in this.values)) {
      this.values[name] = setting.default;
    }
    if (this.register) {
      for (const opt of def.list) {
        this.register(`set-${name}-${opt}`, "", () => {
          this.set(name, opt);
          this.save();
        });
      }
    }
  }

  /** Register a range (slider) setting. */
  addRange(name: string, def: {
    label: string;
    min: number;
    max: number;
    step: number;
    default?: number;
    desc?: string;
    onChange?: (value: number) => void;
  }): void {
    const setting: RangeDef = {
      type: "range",
      label: def.label,
      default: def.default ?? def.min,
      min: def.min,
      max: def.max,
      step: def.step,
      ...(def.desc !== undefined ? { desc: def.desc } : {}),
      ...(def.onChange !== undefined ? { onChange: def.onChange } : {}),
    };
    this.definitions.set(name, setting);
    if (!(name in this.values)) {
      this.values[name] = setting.default;
    }
  }

  // --- Value access ---

  /** Get the current value of a setting. */
  get(name: string): boolean | number | string | undefined {
    return this.values[name];
  }

  /** Set a setting value and fire its onChange callback. */
  set(name: string, value: boolean | number | string): void {
    this.values[name] = value;
    const def = this.definitions.get(name);
    if (def?.onChange) {
      (def.onChange as (v: boolean | number | string) => void)(value);
    }
  }

  // --- Persistence ---

  /** Load settings from localStorage, applying saved values over defaults. */
  load(): void {
    const raw = loadLocal(STORAGE_KEY);
    if (raw) {
      try {
        const saved = JSON.parse(raw);
        for (const [key, val] of Object.entries(saved)) {
          if (this.definitions.has(key)) {
            this.values[key] = val as boolean | number | string;
          }
        }
      } catch {
        // Corrupt data — ignore
      }
    }
  }

  /** Save current settings to localStorage. */
  save(): void {
    const toSave: Record<string, boolean | number | string> = {};
    for (const [key] of this.definitions) {
      if (key in this.values) {
        toSave[key] = this.values[key]!;
      }
    }
    saveLocal(STORAGE_KEY, JSON.stringify(toSave));
  }

  /** Reset all settings to their defaults. */
  reset(): void {
    for (const [key, def] of this.definitions) {
      const prev = this.values[key];
      this.values[key] = def.default;
      if (def.onChange && prev !== def.default) {
        (def.onChange as (v: boolean | number | string) => void)(def.default);
      }
    }
    removeLocal(STORAGE_KEY);
  }

  /** Get all registered setting definitions (for UI rendering). */
  getDefinitions(): Map<string, SettingDef> {
    return this.definitions;
  }

  /** Register commands for settings management. */
  initCommands(
    registerCommand: (id: string, binding: string, handler: () => void) => void,
    showSettingsUI: (
      entries: SettingUIEntry[],
      onSet: (name: string, value: boolean | number | string) => void,
      onReset: () => void,
    ) => void,
  ): void {
    this.register = registerCommand;
    registerCommand("open-settings", "", () => {
      const entries: SettingUIEntry[] = [];
      for (const [name, def] of this.definitions) {
        entries.push({
          name,
          type: def.type,
          label: def.label,
          ...(def.desc !== undefined ? { desc: def.desc } : {}),
          value: this.get(name)!,
          ...(def.type === "list" ? { list: (def as ListDef).list } : {}),
          ...(def.type === "range" ? { min: (def as RangeDef).min, max: (def as RangeDef).max, step: (def as RangeDef).step } : {}),
        });
      }
      showSettingsUI(
        entries,
        (n, v) => { this.set(n, v); this.save(); },
        () => this.reset(),
      );
    });
    registerCommand("reset-settings", "", () => this.reset());
  }
}
