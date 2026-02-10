/**
 * Flash.Scope — AVM2 scope chain operations.
 *
 * Scope lookups (findPropStrict, findProperty) are resolved at compile time
 * by the ast_printer into `this.field`, `ClassName.field`, or bare names.
 * They cannot be correctly resolved at runtime in ES modules because
 * module-scoped names aren't accessible via string lookup.
 *
 * If a scope lookup reaches the runtime, it indicates a missing case in the
 * compiler's scope resolution — the fix belongs in ast_printer.rs.
 *
 * newActivation is the only function called at runtime: it creates plain
 * objects used as closure capture containers.
 */

export function getOuterScope(): any {
  return globalThis;
}

export function findPropStrict(name: string): never {
  throw new ReferenceError(
    `Flash_Scope.findPropStrict("${name}"): scope lookup was not resolved ` +
    `at compile time — this is a compiler bug in ast_printer.rs`,
  );
}

export function findProperty(name: string): never {
  throw new ReferenceError(
    `Flash_Scope.findProperty("${name}"): scope lookup was not resolved ` +
    `at compile time — this is a compiler bug in ast_printer.rs`,
  );
}

export function findDef(name: string): never {
  throw new ReferenceError(
    `Flash_Scope.findDef("${name}"): scope lookup was not resolved ` +
    `at compile time — this is a compiler bug in ast_printer.rs`,
  );
}

export function newActivation(): Record<string, any> {
  return {};
}
