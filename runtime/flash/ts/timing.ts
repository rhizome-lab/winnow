class TimingState {
  lastTime = performance.now() / 1000;
  startTime = performance.now() / 1000;
  dt = 0;
  frames = 0;
  fps = 60;
}

const state = new TimingState();

export const timing = {
  delta_time(): number {
    return state.dt;
  },

  elapsed(): number {
    return performance.now() / 1000 - state.startTime;
  },

  frame_count(): number {
    return state.frames;
  },

  target_fps(): number {
    return state.fps;
  },

  set_target_fps(value: number): void {
    state.fps = value;
  },

  tick(): void {
    const now = performance.now() / 1000;
    state.dt = now - state.lastTime;
    state.lastTime = now;
    state.frames++;
  },
};
