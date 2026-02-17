/** SugarCube Macro system shim.
 *
 * Supports Macro.add(), Macro.has(), Macro.get(), Macro.delete() and provides
 * a MacroContext (the `this` inside a handler) with args, output, payload,
 * name, error(), addShadow(), createShadowWrapper(), createDebugView().
 *
 * Handlers are invoked via invokeMacro() which is called from widget.ts when
 * a widget lookup fails.
 */

import type { SugarCubeRuntime } from "./runtime";

export interface MacroDef {
  tags?: string[] | null;
  skipArgs?: boolean;
  handler: Function;
  [key: string]: any;
}

export class SCMacro {
  private macros: Map<string, MacroDef> = new Map();
  private rt: SugarCubeRuntime;

  constructor(rt: SugarCubeRuntime) {
    this.rt = rt;
  }

  /** Register one or more macros. */
  add(nameOrNames: string | string[], definition: MacroDef): void {
    const names = Array.isArray(nameOrNames) ? nameOrNames : [nameOrNames];
    for (const name of names) {
      this.macros.set(name, definition);
    }
  }

  /** Check if a macro is registered. */
  has(name: string): boolean {
    return this.macros.has(name);
  }

  /** Get a macro definition. */
  get(name: string): MacroDef | null {
    return this.macros.get(name) || null;
  }

  /** Delete one or more macros. */
  delete(nameOrNames: string | string[]): void {
    const names = Array.isArray(nameOrNames) ? nameOrNames : [nameOrNames];
    for (const name of names) {
      this.macros.delete(name);
    }
  }

  /** Build a MacroContext and invoke a macro handler.
   *
   * Called from widget when a widget lookup fails and the name exists
   * in the Macro registry.
   */
  invokeMacro(def: MacroDef, name: string, args: any[], output?: DocumentFragment): void {
    const argsArray: any[] = [...args];
    (argsArray as any).full = args.join(" ");
    (argsArray as any).raw = args.join(" ");

    const shadows: string[] = [];
    const State = this.rt.State;

    const context = {
      name,
      args: argsArray,
      output: output || this.rt.Output.doc.createDocumentFragment(),
      payload: [] as { name: string; contents: string }[],
      error(msg: string): string {
        return `Error in macro <<${name}>>: ${msg}`;
      },
      addShadow(...varNames: string[]): void {
        for (const v of varNames) {
          for (const part of v.split(/[,\s]+/)) {
            const trimmed = part.trim();
            if (trimmed) shadows.push(trimmed);
          }
        }
      },
      createShadowWrapper(fn: Function): Function {
        if (shadows.length === 0) return fn;
        const captured = new Map<string, any>();
        for (const varName of shadows) {
          captured.set(varName, State.get(varName));
        }
        return function(this: any, ...fnArgs: any[]) {
          const saved = new Map<string, any>();
          for (const varName of shadows) {
            saved.set(varName, State.get(varName));
            State.set(varName, captured.get(varName));
          }
          try {
            return fn.apply(this, fnArgs);
          } finally {
            for (const varName of shadows) {
              State.set(varName, saved.get(varName));
            }
          }
        };
      },
      createDebugView(): void {},
      self: def,
      parser: { source: "", matchStart: 0 },
    };

    try {
      def.handler.call(context);
    } catch (e) {
      console.error(`[macro] error in <<${name}>>:`, e);
    }
  }
}
