/**
 * flash.security package — security interfaces.
 */

import type { IDataInput } from "./utils";

/** AS3 `flash.security.IURIDereferencer` — resolves URIs in XML signatures. */
export abstract class IURIDereferencer {
  abstract dereference(uri: string): IDataInput;
}
