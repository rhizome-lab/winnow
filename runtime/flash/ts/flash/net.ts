/**
 * flash.net package — URLRequest, URLLoader, URLLoaderDataFormat,
 * FileReference, SharedObject.
 */

import { EventDispatcher, Event, IOErrorEvent, ProgressEvent, SecurityErrorEvent } from "./events";
import {
  fetchResource,
  hasFetch,
  loadLocal,
  saveLocal,
  removeLocal,
  triggerDownload,
} from "./platform";

// ---------------------------------------------------------------------------
// AS3 net interfaces
// ---------------------------------------------------------------------------

/** AS3 `flash.net.IDynamicPropertyOutput` — receives dynamic property key/value pairs during serialization. */
export abstract class IDynamicPropertyOutput {
  abstract writeDynamicProperty(name: string, value: any): void;
}

/** AS3 `flash.net.IDynamicPropertyWriter` — controls serialization of dynamic properties. */
export abstract class IDynamicPropertyWriter {
  abstract writeDynamicProperties(obj: any, output: IDynamicPropertyOutput): void;
}

// ---------------------------------------------------------------------------
// URLRequest
// ---------------------------------------------------------------------------

export class URLRequest {
  _url: string;
  _method = "GET";
  _data: object | string | null = null;
  _contentType = "application/x-www-form-urlencoded";
  _requestHeaders: object[] = [];
  _digest: string | null = null;

  get url() { return this._url; }
  set url(v: string) { this._url = v; }
  get method() { return this._method; }
  set method(v: string) { this._method = v; }
  get data() { return this._data; }
  set data(v: object | string | null) { this._data = v; }
  get contentType() { return this._contentType; }
  set contentType(v: string) { this._contentType = v; }
  get requestHeaders() { return this._requestHeaders; }
  set requestHeaders(v: object[]) { this._requestHeaders = v; }
  get digest() { return this._digest; }
  set digest(v: string | null) { this._digest = v; }

  constructor(url = "") {
    this._url = url;
  }
}

// ---------------------------------------------------------------------------
// URLLoaderDataFormat
// ---------------------------------------------------------------------------

export class URLLoaderDataFormat {
  static readonly BINARY = "binary";
  static readonly TEXT = "text";
  static readonly VARIABLES = "variables";
}

// ---------------------------------------------------------------------------
// URLLoader
// ---------------------------------------------------------------------------

export class URLLoader extends EventDispatcher {
  _bytesLoaded = 0;
  _bytesTotal = 0;
  _data: any = null;
  _dataFormat: string = URLLoaderDataFormat.TEXT;

  get bytesLoaded() { return this._bytesLoaded; }
  set bytesLoaded(v: number) { this._bytesLoaded = v; }
  get bytesTotal() { return this._bytesTotal; }
  set bytesTotal(v: number) { this._bytesTotal = v; }
  get data() { return this._data; }
  set data(v: any) { this._data = v; }
  get dataFormat() { return this._dataFormat; }
  set dataFormat(v: string) { this._dataFormat = v; }

  constructor(request?: URLRequest) {
    super();
    if (request) this.load(request);
  }

  close(): void {}

  load(request: URLRequest): void {
    const url = request.url;
    if (!hasFetch()) return;
    fetchResource(url, { method: request.method })
      .then((res) => {
        this.bytesTotal = Number(res.headers.get("content-length") ?? 0);
        if (this.dataFormat === URLLoaderDataFormat.BINARY) {
          return res.arrayBuffer().then((buf) => {
            this.data = buf;
            return buf.byteLength;
          });
        }
        return res.text().then((txt) => {
          this.data = txt;
          return txt.length;
        });
      })
      .then((size) => {
        this.bytesLoaded = size as number;
        this.bytesTotal = size as number;
        this.dispatchEvent(new Event(Event.COMPLETE));
      })
      .catch((err) => {
        this.dispatchEvent(
          new IOErrorEvent(IOErrorEvent.IO_ERROR, false, false, String(err)),
        );
      });
  }
}

// ---------------------------------------------------------------------------
// FileReference
// ---------------------------------------------------------------------------

export class FileReference extends EventDispatcher {
  _creationDate: Date | null = null;
  _creator: string | null = null;
  _data: ArrayBuffer | null = null;
  _extension: string | null = null;
  _modificationDate: Date | null = null;
  _name: string | null = null;
  _size = 0;
  _type: string | null = null;

  get creationDate() { return this._creationDate; }
  set creationDate(v: Date | null) { this._creationDate = v; }
  get creator() { return this._creator; }
  set creator(v: string | null) { this._creator = v; }
  get data() { return this._data; }
  set data(v: ArrayBuffer | null) { this._data = v; }
  get extension() { return this._extension; }
  set extension(v: string | null) { this._extension = v; }
  get modificationDate() { return this._modificationDate; }
  set modificationDate(v: Date | null) { this._modificationDate = v; }
  get name() { return this._name; }
  set name(v: string | null) { this._name = v; }
  get size() { return this._size; }
  set size(v: number) { this._size = v; }
  get type() { return this._type; }
  set type(v: string | null) { this._type = v; }

  browse(_typeFilter?: any[]): boolean {
    return false;
  }

  cancel(): void {}

  download(_request: URLRequest, _defaultFileName?: string): void {}

  load(): void {
    this.dispatchEvent(new Event(Event.COMPLETE));
  }

  save(data: any, defaultFileName?: string): void {
    // Best-effort download via platform API.
    try {
      const blob = new Blob([typeof data === "string" ? data : JSON.stringify(data)]);
      triggerDownload(blob, defaultFileName ?? "download");
      this.dispatchEvent(new Event(Event.COMPLETE));
    } catch {
      this.dispatchEvent(
        new SecurityErrorEvent(SecurityErrorEvent.SECURITY_ERROR),
      );
    }
  }

  upload(_request: URLRequest, _uploadDataFieldName?: string, _testUpload?: boolean): void {}
}

// ---------------------------------------------------------------------------
// SharedObject
// ---------------------------------------------------------------------------

const SHARED_OBJECT_PREFIX = "flash_so_";

export class SharedObject {
  private _name = "";
  private _data: Record<string, any> = {};
  _size = 0;
  _client: object | null = null;
  _objectEncoding = 3;

  get size() { return this._size; }
  set size(v: number) { this._size = v; }
  get client() { return this._client; }
  set client(v: object | null) { this._client = v; }
  get objectEncoding() { return this._objectEncoding; }
  set objectEncoding(v: number) { this._objectEncoding = v; }

  get data(): Record<string, any> {
    return this._data;
  }

  clear(): void {
    this._data = {};
    this.size = 0;
    removeLocal(SHARED_OBJECT_PREFIX + this._name);
  }

  close(): void {}

  flush(_minDiskSpace = 0): string {
    const json = JSON.stringify(this._data);
    this.size = json.length;
    saveLocal(SHARED_OBJECT_PREFIX + this._name, json);
    return "flushed";
  }

  setDirty(propertyName: string): void {
    void propertyName;
  }

  setProperty(propertyName: string, value: any): void {
    this._data[propertyName] = value;
  }

  static getLocal(name: string, _localPath: string | null = null, _secure = false): SharedObject {
    const so = new SharedObject();
    so._name = name;
    const stored = loadLocal(SHARED_OBJECT_PREFIX + name);
    if (stored) {
      try {
        so._data = JSON.parse(stored);
        so.size = stored.length;
      } catch {
        so._data = {};
      }
    }
    return so;
  }
}
