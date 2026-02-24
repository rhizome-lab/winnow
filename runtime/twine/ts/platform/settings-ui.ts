/** Browser settings UI — settings/preferences form presentation. */

export interface SettingUIEntry {
  name: string;
  type: "toggle" | "list" | "range";
  label: string;
  desc?: string;
  value: boolean | number | string;
  // list-specific
  list?: string[];
  // range-specific
  min?: number;
  max?: number;
  step?: number;
}

export class SettingsUIManager {
  private _showDialog: (title: string, content: DocumentFragment | HTMLElement) => void;
  private _closeDialog: () => void;

  constructor(
    showDialog: (title: string, content: DocumentFragment | HTMLElement) => void,
    closeDialog: () => void,
  ) {
    this._showDialog = showDialog;
    this._closeDialog = closeDialog;
  }

  showSettingsUI(
    entries: SettingUIEntry[],
    onSet: (name: string, value: boolean | number | string) => void,
    onReset: () => void,
  ): void {
    const frag = document.createDocumentFragment();
    const form = document.createElement("div");
    form.style.cssText = "display: flex; flex-direction: column; gap: 0.8em;";

    for (const entry of entries) {
      const row = document.createElement("div");

      const label = document.createElement("label");
      label.style.cssText = "display: flex; align-items: center; gap: 0.5em; flex-wrap: wrap;";

      const labelText = document.createElement("span");
      labelText.textContent = entry.label;
      label.appendChild(labelText);

      if (entry.type === "toggle") {
        const input = document.createElement("input");
        input.type = "checkbox";
        input.checked = !!entry.value;
        input.addEventListener("change", () => onSet(entry.name, input.checked));
        label.appendChild(input);
      } else if (entry.type === "list") {
        const select = document.createElement("select");
        for (const opt of entry.list || []) {
          const option = document.createElement("option");
          option.value = String(opt);
          option.textContent = String(opt);
          if (opt === entry.value) option.selected = true;
          select.appendChild(option);
        }
        select.addEventListener("change", () => onSet(entry.name, select.value));
        label.appendChild(select);
      } else if (entry.type === "range") {
        const input = document.createElement("input");
        input.type = "range";
        input.min = String(entry.min ?? 0);
        input.max = String(entry.max ?? 100);
        input.step = String(entry.step ?? 1);
        input.value = String(entry.value);
        const valueLabel = document.createElement("span");
        valueLabel.textContent = String(entry.value);
        input.addEventListener("input", () => {
          const val = Number(input.value);
          valueLabel.textContent = String(val);
          onSet(entry.name, val);
        });
        label.appendChild(input);
        label.appendChild(valueLabel);
      }

      row.appendChild(label);

      if (entry.desc) {
        const desc = document.createElement("div");
        desc.style.cssText = "font-size: 0.85em; color: #999; margin-top: 0.2em;";
        desc.textContent = entry.desc;
        row.appendChild(desc);
      }

      form.appendChild(row);
    }

    // Reset button
    const resetBtn = document.createElement("button");
    resetBtn.textContent = "Reset to Defaults";
    resetBtn.style.cssText = "margin-top: 0.5em; align-self: flex-start;";
    resetBtn.addEventListener("click", () => { onReset(); this._closeDialog(); });
    form.appendChild(resetBtn);

    frag.appendChild(form);
    this._showDialog("Settings", frag);
  }
}
