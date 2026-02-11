/**
 * flash.desktop package — AIR desktop interfaces.
 */

import type { IDataInput } from "./utils";

/** AS3 `flash.desktop.IFilePromise` — deferred file data for drag-and-drop. */
export abstract class IFilePromise {
  abstract get isAsync(): boolean;
  abstract get relativePath(): string;
  abstract close(): void;
  abstract open(): IDataInput;
  abstract reportError(e: any): void;
}
