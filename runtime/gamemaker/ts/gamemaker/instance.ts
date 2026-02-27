/** GML instance helpers — field access on specific object types. */

import type { GameRuntime } from "./runtime";
import { GMLObject } from "./runtime";

export function createInstanceAPI(rt: GameRuntime) {
  /** Get a field value from the first instance of a given object type. */
  function getInstanceField(cls: typeof GMLObject | number, field: string): any {
    const clazz = typeof cls === 'function' ? cls : rt.classes[cls];
    if (!clazz) return undefined;
    const inst = rt.roomVariables.find((o) => o instanceof clazz);
    return inst ? (inst as any)[field] : undefined;
  }

  /** Set a field value on the first instance of a given object type. */
  function setInstanceField(cls: typeof GMLObject | number, field: string, value: any): void {
    const clazz = typeof cls === 'function' ? cls : rt.classes[cls];
    if (!clazz) return;
    const inst = rt.roomVariables.find((o) => o instanceof clazz);
    if (inst) (inst as any)[field] = value;
  }

  /** Set an indexed element of a field on the first instance of a given object type. */
  function setInstanceFieldIndex(cls: typeof GMLObject | number, field: string, index: number, value: any): void {
    const clazz = typeof cls === 'function' ? cls : rt.classes[cls];
    if (!clazz) return;
    const inst = rt.roomVariables.find((o) => o instanceof clazz);
    if (inst) (inst as any)[field][index] = value;
  }

  /** Get a field value from ALL instances. */
  function getAllField(field: string): any {
    for (const inst of rt.roomVariables) {
      return (inst as any)[field];
    }
    return undefined;
  }

  /** Set a field value on ALL instances. */
  function setAllField(field: string, value: any): void {
    for (const inst of rt.roomVariables) {
      (inst as any)[field] = value;
    }
  }

  /** Execute a block for each instance of a given type (or all).
   * Sets rt._self to the current with-target so alarm_set/event_user work correctly. */
  function withInstances<T extends GMLObject>(
    target: (new(...args: any[]) => T) | T | number,
    callback: (inst: T) => void,
  ): void {
    const prevSelf = rt._self;
    if (typeof target === 'function') {
      // class constructor — iterate all instances of this class
      for (const inst of rt.roomVariables.slice()) {
        if (inst instanceof (target as Function)) {
          rt._self = inst; callback(inst as T);
        }
      }
    } else if (target === -1) {
      for (const inst of rt.roomVariables.slice()) {
        rt._self = inst; callback(inst as T);
      }
    } else if (target === -2) {
      // other — handled by caller
    } else if (target instanceof GMLObject) {
      // specific instance
      rt._self = target; callback(target as T);
    }
    rt._self = prevSelf;
  }

  return {
    getInstanceField, setInstanceField, setInstanceFieldIndex,
    getAllField, setAllField, withInstances,
  };
}
