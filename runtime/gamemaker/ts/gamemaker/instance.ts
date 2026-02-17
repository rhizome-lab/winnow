/** GML instance helpers — field access on specific object types. */

import type { GameRuntime, GMLObject } from "./runtime";

export function createInstanceAPI(rt: GameRuntime) {
  /** Get a field value from the first instance of a given object type. */
  function getInstanceField(objId: number, field: string): any {
    const clazz = rt.classes[objId];
    if (!clazz) return undefined;
    const inst = rt.roomVariables.find((o) => o instanceof clazz);
    return inst ? (inst as any)[field] : undefined;
  }

  /** Set a field value on the first instance of a given object type. */
  function setInstanceField(objId: number, field: string, value: any): void {
    const clazz = rt.classes[objId];
    if (!clazz) return;
    const inst = rt.roomVariables.find((o) => o instanceof clazz);
    if (inst) (inst as any)[field] = value;
  }

  /** Set an indexed element of a field on the first instance of a given object type. */
  function setInstanceFieldIndex(objId: number, field: string, index: number, value: any): void {
    const clazz = rt.classes[objId];
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

  /** Execute a block for each instance of a given type (or all). */
  function withInstances(target: number, callback: (inst: GMLObject) => void): void {
    if (target === -1) {
      // all
      for (const inst of rt.roomVariables.slice()) {
        callback(inst);
      }
    } else if (target === -2) {
      // other — handled by caller
    } else if (target >= 0) {
      const clazz = rt.classes[target];
      if (!clazz) return;
      for (const inst of rt.roomVariables.slice()) {
        if (inst instanceof clazz) callback(inst);
      }
    }
  }

  return {
    getInstanceField, setInstanceField, setInstanceFieldIndex,
    getAllField, setAllField, withInstances,
  };
}
