/** SugarCube input macros.
 *
 * Form elements that bind to story variables via State.set/get.
 * Each creates a DOM element and appends it to the current output buffer.
 */

import type { SugarCubeRuntime } from "./runtime";

export class SCInput {
  private rt: SugarCubeRuntime;

  constructor(rt: SugarCubeRuntime) {
    this.rt = rt;
  }

  private appendToOutput(el: HTMLElement): void {
    const container = this.rt.Output.container ?? document.getElementById("passages");
    if (container) {
      container.appendChild(el);
    }
  }

  /** <<textbox "$var" "default" ["PassageName"]>> */
  textbox(varName: string, defaultValue?: string, passageName?: string): void {
    const input = this.rt.Output.doc.createElement("input");
    input.type = "text";
    input.value = defaultValue ?? (this.rt.State.get(varName) as string) ?? "";

    input.addEventListener("input", () => {
      this.rt.State.set(varName, input.value);
    });

    if (passageName) {
      input.addEventListener("keydown", (e) => {
        if (e.key === "Enter") {
          e.preventDefault();
          this.rt.State.set(varName, input.value);
          this.rt.Navigation.goto(passageName);
        }
      });
    }

    this.rt.State.set(varName, input.value);
    this.appendToOutput(input);
  }

  /** <<textarea "$var" "default" ["PassageName"]>> */
  textarea(varName: string, defaultValue?: string, passageName?: string): void {
    const el = this.rt.Output.doc.createElement("textarea");
    el.value = defaultValue ?? (this.rt.State.get(varName) as string) ?? "";
    el.rows = 4;

    el.addEventListener("input", () => {
      this.rt.State.set(varName, el.value);
    });

    this.rt.State.set(varName, el.value);
    this.appendToOutput(el);
  }

  /** <<numberbox "$var" default ["PassageName"]>> */
  numberbox(varName: string, defaultValue?: number, passageName?: string): void {
    const input = this.rt.Output.doc.createElement("input");
    input.type = "number";
    input.value = String(defaultValue ?? (this.rt.State.get(varName) as number) ?? "");

    input.addEventListener("input", () => {
      this.rt.State.set(varName, Number(input.value));
    });

    if (passageName) {
      input.addEventListener("keydown", (e) => {
        if (e.key === "Enter") {
          e.preventDefault();
          this.rt.State.set(varName, Number(input.value));
          this.rt.Navigation.goto(passageName);
        }
      });
    }

    if (defaultValue !== undefined) {
      this.rt.State.set(varName, defaultValue);
    }
    this.appendToOutput(input);
  }

  /** <<checkbox "$var" checkedValue uncheckedValue>> */
  checkbox(varName: string, checkedValue: string | number | boolean, uncheckedValue: string | number | boolean): void {
    const input = this.rt.Output.doc.createElement("input");
    input.type = "checkbox";

    const current = this.rt.State.get(varName);
    input.checked = current === checkedValue;

    input.addEventListener("change", () => {
      this.rt.State.set(varName, input.checked ? checkedValue : uncheckedValue);
    });

    this.rt.State.set(varName, input.checked ? checkedValue : uncheckedValue);
    this.appendToOutput(input);
  }

  /** <<radiobutton "$var" checkedValue>> */
  radiobutton(varName: string, checkedValue: string | number | boolean): void {
    const input = this.rt.Output.doc.createElement("input");
    input.type = "radio";
    input.name = varName;

    const current = this.rt.State.get(varName);
    input.checked = current === checkedValue;

    input.addEventListener("change", () => {
      if (input.checked) {
        this.rt.State.set(varName, checkedValue);
      }
    });

    this.appendToOutput(input);
  }

  /** <<listbox "$var" items...>> */
  listbox(varName: string, ...items: any[]): void {
    const select = this.rt.Output.doc.createElement("select");

    for (const item of items) {
      const option = this.rt.Output.doc.createElement("option");
      option.value = String(item);
      option.textContent = String(item);
      select.appendChild(option);
    }

    const current = this.rt.State.get(varName);
    if (current !== undefined) {
      select.value = String(current);
    } else if (items.length > 0) {
      this.rt.State.set(varName, items[0]);
    }

    select.addEventListener("change", () => {
      const idx = select.selectedIndex;
      this.rt.State.set(varName, idx >= 0 && idx < items.length ? items[idx] : select.value);
    });

    this.appendToOutput(select);
  }

  /** <<cycle "$var" items...>> */
  cycle(varName: string, ...items: any[]): void {
    if (items.length === 0) return;

    let currentIndex = 0;
    const current = this.rt.State.get(varName);
    if (current !== undefined) {
      const idx = items.indexOf(current);
      if (idx >= 0) currentIndex = idx;
    }

    const btn = this.rt.Output.doc.createElement("button");
    btn.textContent = String(items[currentIndex]);
    this.rt.State.set(varName, items[currentIndex]);

    btn.addEventListener("click", (e) => {
      e.preventDefault();
      currentIndex = (currentIndex + 1) % items.length;
      btn.textContent = String(items[currentIndex]);
      this.rt.State.set(varName, items[currentIndex]);
    });

    this.appendToOutput(btn);
  }

  /** <<button "text" ["PassageName"]>> */
  button(text: string, passageName?: string): void {
    const btn = this.rt.Output.doc.createElement("button");
    btn.textContent = text;

    if (passageName) {
      btn.addEventListener("click", (e) => {
        e.preventDefault();
        this.rt.Navigation.goto(passageName);
      });
    }

    this.appendToOutput(btn);
  }
}
