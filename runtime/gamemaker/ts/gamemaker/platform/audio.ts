/** Web Audio API — platform audio implementation. */

interface BusState {
  gainNode: GainNode;
  paused: boolean;
}

interface PlayingSound {
  source: AudioBufferSourceNode;
  gainNode: GainNode;
  pannerNode: StereoPannerNode;
  busId: number;
  soundIndex: number;
  loop: boolean;
  pitch: number;
  pan: number;
  /** audioCtx.currentTime at which playback started (adjusted for offset). */
  startTime: number;
  paused: boolean;
  /** Playback position when paused, in seconds. */
  pauseOffset: number;
}

export class AudioState {
  ctx: AudioContext | null = null;
  masterGainNode: GainNode | null = null;
  /** Decoded buffers indexed by sound index (SOND order). */
  buffers: (AudioBuffer | null)[] = [];
  playing = new Map<number, PlayingSound>();
  buses = new Map<number, BusState>();
  nextHandle = 1;
  nextBus = 1;
}

export async function loadAudio(
  state: AudioState,
  sounds: { name: string; url: string }[],
): Promise<void> {
  state.ctx = new AudioContext();
  state.masterGainNode = state.ctx.createGain();
  state.masterGainNode.connect(state.ctx.destination);

  // Bus 0 is the default bus — all sounds route through it unless another bus is specified.
  const defaultGain = state.ctx.createGain();
  defaultGain.connect(state.masterGainNode);
  state.buses.set(0, { gainNode: defaultGain, paused: false });

  const promises = sounds.map(async (s, i) => {
    if (!s.url) { state.buffers[i] = null; return; }
    try {
      const res = await fetch(s.url);
      if (!res.ok) { state.buffers[i] = null; return; }
      state.buffers[i] = await state.ctx!.decodeAudioData(await res.arrayBuffer());
    } catch {
      state.buffers[i] = null;
    }
  });
  await Promise.all(promises);
}

function _makeSource(
  state: AudioState,
  handle: number,
  buffer: AudioBuffer,
  gainNode: GainNode,
  loop: boolean,
  pitch: number,
  offset: number,
): AudioBufferSourceNode {
  const source = state.ctx!.createBufferSource();
  source.buffer = buffer;
  source.loop = loop;
  source.playbackRate.value = pitch;
  source.connect(gainNode);
  source.start(0, offset);
  source.onended = () => {
    const p = state.playing.get(handle);
    if (p && !p.paused) state.playing.delete(handle);
  };
  return source;
}

export function play(
  state: AudioState,
  soundIndex: number,
  busId: number,
  loop: boolean,
  gain: number,
  pitch: number,
  pan: number,
  offset: number,
): number {
  if (!state.ctx || !state.masterGainNode) return -1;
  const buffer = state.buffers[soundIndex];
  if (!buffer) return -1;
  const bus = state.buses.get(busId);
  if (!bus) return -1;
  if (state.ctx.state === "suspended") void state.ctx.resume();

  const gainNode = state.ctx.createGain();
  gainNode.gain.value = gain;
  const pannerNode = state.ctx.createStereoPanner();
  pannerNode.pan.value = pan;
  gainNode.connect(pannerNode);
  pannerNode.connect(bus.gainNode);

  const handle = state.nextHandle++;
  const source = _makeSource(state, handle, buffer, gainNode, loop, pitch, offset);
  state.playing.set(handle, {
    source, gainNode, pannerNode, busId, soundIndex, loop, pitch, pan,
    startTime: state.ctx.currentTime - offset,
    paused: false, pauseOffset: 0,
  });
  return handle;
}

export function stop(state: AudioState, handle: number): void {
  const p = state.playing.get(handle);
  if (!p) return;
  try { p.source.stop(); } catch { /* already ended */ }
  state.playing.delete(handle);
}

export function stopAll(state: AudioState): void {
  for (const handle of [...state.playing.keys()]) stop(state, handle);
}

export function pause(state: AudioState, handle: number): void {
  const p = state.playing.get(handle);
  if (!p || !state.ctx || p.paused) return;
  p.pauseOffset = state.ctx.currentTime - p.startTime;
  p.paused = true;
  try { p.source.stop(); } catch { /* already ended */ }
}

export function resume(state: AudioState, handle: number): void {
  const p = state.playing.get(handle);
  if (!p || !state.ctx || !p.paused) return;
  const buffer = state.buffers[p.soundIndex];
  if (!buffer) return;
  p.source = _makeSource(state, handle, buffer, p.gainNode, p.loop, p.pitch, p.pauseOffset);
  p.startTime = state.ctx.currentTime - p.pauseOffset;
  p.paused = false;
}

export function resumeAll(state: AudioState): void {
  for (const handle of state.playing.keys()) resume(state, handle);
}

export function isPlaying(state: AudioState, handle: number): boolean {
  const p = state.playing.get(handle);
  return p !== undefined && !p.paused;
}

export function isPaused(state: AudioState, handle: number): boolean {
  return state.playing.get(handle)?.paused ?? false;
}

export function setGain(state: AudioState, handle: number, gain: number, fadeMs = 0): void {
  const p = state.playing.get(handle);
  if (!p || !state.ctx) return;
  if (fadeMs > 0) {
    p.gainNode.gain.linearRampToValueAtTime(gain, state.ctx.currentTime + fadeMs / 1000);
  } else {
    p.gainNode.gain.value = gain;
  }
}

export function getGain(state: AudioState, handle: number): number {
  return state.playing.get(handle)?.gainNode.gain.value ?? 0;
}

export function setPitch(state: AudioState, handle: number, pitch: number, fadeMs = 0): void {
  const p = state.playing.get(handle);
  if (!p || !state.ctx) return;
  if (fadeMs > 0) {
    p.source.playbackRate.linearRampToValueAtTime(pitch, state.ctx.currentTime + fadeMs / 1000);
  } else {
    p.source.playbackRate.value = pitch;
  }
  p.pitch = pitch;
}

export function getPitch(state: AudioState, handle: number): number {
  return state.playing.get(handle)?.pitch ?? 1;
}

export function setPan(state: AudioState, handle: number, pan: number): void {
  const p = state.playing.get(handle);
  if (!p) return;
  p.pannerNode.pan.value = pan;
  p.pan = pan;
}

export function getPan(state: AudioState, handle: number): number {
  return state.playing.get(handle)?.pan ?? 0;
}

export function setMasterGain(state: AudioState, gain: number): void {
  if (state.masterGainNode) state.masterGainNode.gain.value = gain;
}

export function getPosition(state: AudioState, handle: number): number {
  const p = state.playing.get(handle);
  if (!p || !state.ctx) return 0;
  return p.paused ? p.pauseOffset : state.ctx.currentTime - p.startTime;
}

export function setPosition(state: AudioState, handle: number, pos: number): void {
  const p = state.playing.get(handle);
  if (!p || !state.ctx) return;
  // Capture fields before stop() removes the entry.
  const { soundIndex, loop, gainNode, pannerNode, busId, pitch, pan } = p;
  stop(state, handle);
  const buffer = state.buffers[soundIndex];
  if (!buffer) return;
  const bus = state.buses.get(busId);
  if (!bus) return;
  if (state.ctx.state === "suspended") void state.ctx.resume();
  // Reuse the existing gainNode/pannerNode — they're still connected to the bus chain.
  const source = _makeSource(state, handle, buffer, gainNode, loop, pitch, pos);
  state.playing.set(handle, {
    source, gainNode, pannerNode, busId, soundIndex, loop, pitch, pan,
    startTime: state.ctx.currentTime - pos,
    paused: false, pauseOffset: 0,
  });
}

export function soundLength(state: AudioState, soundIndex: number): number {
  return state.buffers[soundIndex]?.duration ?? 0;
}

// ---- Bus management ----

export function createBus(state: AudioState, gain = 1): number {
  if (!state.ctx || !state.masterGainNode) return -1;
  const gainNode = state.ctx.createGain();
  gainNode.gain.value = gain;
  gainNode.connect(state.masterGainNode);
  const id = state.nextBus++;
  state.buses.set(id, { gainNode, paused: false });
  return id;
}

export function setBusGain(state: AudioState, bus: number, gain: number, fadeMs = 0): void {
  const b = state.buses.get(bus);
  if (!b || !state.ctx) return;
  if (fadeMs > 0) {
    b.gainNode.gain.linearRampToValueAtTime(gain, state.ctx.currentTime + fadeMs / 1000);
  } else {
    b.gainNode.gain.value = gain;
  }
}

export function getBusGain(state: AudioState, bus: number): number {
  return state.buses.get(bus)?.gainNode.gain.value ?? 1;
}

export function pauseBus(state: AudioState, bus: number): void {
  const b = state.buses.get(bus);
  if (!b || b.paused) return;
  b.paused = true;
  for (const handle of [...state.playing.keys()]) {
    const p = state.playing.get(handle);
    if (p?.busId === bus && !p.paused) pause(state, handle);
  }
}

export function resumeBus(state: AudioState, bus: number): void {
  const b = state.buses.get(bus);
  if (!b || !b.paused) return;
  b.paused = false;
  for (const handle of [...state.playing.keys()]) {
    const p = state.playing.get(handle);
    if (p?.busId === bus && p.paused) resume(state, handle);
  }
}

export function stopBus(state: AudioState, bus: number): void {
  for (const handle of [...state.playing.keys()]) {
    const p = state.playing.get(handle);
    if (p?.busId === bus) stop(state, handle);
  }
}
