let lastTime = performance.now() / 1000;
let startTime = lastTime;
let dt = 0;
let frames: number = 0;
let fps = 60;

export const timing = {
  delta_time(): number {
    return dt;
  },

  elapsed(): number {
    return performance.now() / 1000 - startTime;
  },

  frame_count(): number {
    return frames;
  },

  target_fps(): number {
    return fps;
  },

  set_target_fps(value: number): void {
    fps = value;
  },

  tick(): void {
    const now = performance.now() / 1000;
    dt = now - lastTime;
    lastTime = now;
    frames++;
  },
};
