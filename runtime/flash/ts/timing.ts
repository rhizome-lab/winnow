/** Flash timing shim — delta time, frame counter, target FPS. */

export class TimingShim {
  private _lastTime = performance.now() / 1000;
  private _startTime = performance.now() / 1000;
  private _dt = 0;
  private _frames = 0;
  private _fps = 60;

  delta_time(): number { return this._dt; }
  elapsed(): number { return performance.now() / 1000 - this._startTime; }
  frame_count(): number { return this._frames; }
  target_fps(): number { return this._fps; }
  set_target_fps(value: number): void { this._fps = value; }

  tick(): void {
    const now = performance.now() / 1000;
    this._dt = now - this._lastTime;
    this._lastTime = now;
    this._frames++;
  }
}
