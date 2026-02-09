/**
 * flash.net package — URLRequest, SharedObject.
 */

// ---------------------------------------------------------------------------
// URLRequest
// ---------------------------------------------------------------------------

export class URLRequest {
  url: string;
  method = "GET";
  data: any = null;
  contentType = "application/x-www-form-urlencoded";
  requestHeaders: any[] = [];
  digest: string | null = null;

  constructor(url = "") {
    this.url = url;
  }
}

// ---------------------------------------------------------------------------
// SharedObject
// ---------------------------------------------------------------------------

const SHARED_OBJECT_PREFIX = "flash_so_";

export class SharedObject {
  private _name = "";
  private _data: Record<string, any> = {};
  size = 0;
  client: any = null;
  objectEncoding = 3;

  get data(): Record<string, any> {
    return this._data;
  }

  clear(): void {
    this._data = {};
    this.size = 0;
    if (typeof localStorage !== "undefined") {
      localStorage.removeItem(SHARED_OBJECT_PREFIX + this._name);
    }
  }

  close(): void {}

  flush(_minDiskSpace = 0): string {
    const json = JSON.stringify(this._data);
    this.size = json.length;
    if (typeof localStorage !== "undefined") {
      localStorage.setItem(SHARED_OBJECT_PREFIX + this._name, json);
    }
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
    if (typeof localStorage !== "undefined") {
      const stored = localStorage.getItem(SHARED_OBJECT_PREFIX + name);
      if (stored) {
        try {
          so._data = JSON.parse(stored);
          so.size = stored.length;
        } catch {
          so._data = {};
        }
      }
    }
    return so;
  }
}
