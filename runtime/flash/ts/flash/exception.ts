/** Flash.Exception — AVM2 exception handling. */

function throwValue(value: any): never {
  throw value;
}

export function newCatchScope(exceptionIndex: number): Record<string, any> {
  // In AVM2, a catch scope is an activation that holds the caught
  // exception. Return a plain object; the catch block will assign
  // the exception value to it.
  return {};
}

// `throw` is a reserved keyword — re-export with the reserved name
// so namespace imports work: `Flash_Exception.throw(...)`.
export { throwValue as throw };
