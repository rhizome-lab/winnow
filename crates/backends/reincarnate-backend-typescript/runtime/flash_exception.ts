/** Flash.Exception — AVM2 exception handling. */

// eslint-disable-next-line @typescript-eslint/naming-convention
export const Flash_Exception = {
  throw(value: any): never {
    throw value;
  },

  newCatchScope(exceptionIndex: number): Record<string, any> {
    // In AVM2, a catch scope is an activation that holds the caught
    // exception. Return a plain object; the catch block will assign
    // the exception value to it.
    return {};
  },
};
