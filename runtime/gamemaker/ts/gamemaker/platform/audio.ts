/**
 * Web Audio API — platform audio implementation.
 *
 * Signal graph: Buffer → Voice(gain+pan) → NodeGraph → Master
 *
 * The node graph is a user-defined DAG of DSP nodes. Node 0 is the implicit
 * master output (always valid). User-created nodes connect anywhere in the
 * graph via connect(from, to). Playing a voice routes its output to a
 * designated sink node.
 *
 * Two tiers:
 *   Setup tier  — graph construction at init time: createNode, connect, disconnect, setNodeParam
 *   Hot tier    — per-frame voice control: play, stop, pause, resume, setVoiceGain, ...
 *
 * All functions use only primitive arguments — no object allocation at call site.
 */

// ---- Public types ----

export type NodeKind =
  | "gain" | "pan"
  | "low_pass" | "high_pass" | "band_pass" | "notch"
  | "compressor"
  | "reverb" | "delay"
  | "mixer";

/** Named parameter for a DSP node. All param values are floats. */
export type ParamKind =
  | "gain"       // linear amplitude (0..∞)
  | "pan"        // stereo position (-1=L, 0=C, 1=R)
  | "cutoff"     // filter cutoff frequency (Hz)
  | "resonance"  // filter Q factor
  | "wet_mix"    // wet/dry blend (0=dry, 1=wet)
  | "decay"      // reverb tail length (seconds)
  | "delay_time" // echo delay (seconds)
  | "feedback"   // echo feedback (0..1)
  | "threshold"  // compressor threshold (dBFS, negative)
  | "ratio"      // compressor ratio (e.g. 4 = 4:1)
  | "attack"     // attack time (seconds)
  | "release"    // release time (seconds)
  | "knee";      // compressor knee width (dB)

// ---- Internal interfaces ----

interface GraphNode {
  kind: NodeKind;
  /** Entry point — connect incoming signals here. */
  inputNode: globalThis.AudioNode;
  /** Exit point — connects to downstream nodes' inputNode. */
  outputNode: globalThis.AudioNode;
  /** AudioParam references for linearRamp fade support. */
  audioParams: Map<ParamKind, AudioParam>;
}

interface PlayingVoice {
  source: AudioBufferSourceNode;
  gainNode: GainNode;
  panNode: StereoPannerNode;
  bufferId: number;
  sinkId: number;
  loop: boolean;
  pitch: number;
  pan: number;
  /** audioCtx.currentTime at which playback started (adjusted for offset). */
  startTime: number;
  paused: boolean;
  /** Playback position when paused, in seconds. */
  pauseOffset: number;
}

// ---- AudioState ----

export class AudioState {
  ctx: AudioContext | null = null;
  /** Decoded buffers indexed by sound index (SOND order). BufferId = index. */
  buffers: (AudioBuffer | null)[] = [];
  /** Node graph. NodeId 0 = master output (always valid after loadAudio). */
  nodes = new Map<number, GraphNode>();
  /** Playing voices. VoiceId 0 = invalid. */
  voices = new Map<number, PlayingVoice>();
  nextNode = 1;
  nextVoice = 1;
}

// ---- Initialization ----

export async function loadAudio(
  state: AudioState,
  sounds: { name: string; url: string }[],
): Promise<void> {
  state.ctx = new AudioContext();

  // Node 0 = master output — a gain node wired to ctx.destination.
  const master = state.ctx.createGain();
  master.gain.value = 1;
  master.connect(state.ctx.destination);
  state.nodes.set(0, {
    kind: "gain",
    inputNode: master,
    outputNode: master,
    audioParams: new Map([["gain", master.gain]]),
  });

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

// ---- Setup tier: node graph construction ----

/** Create a DSP node. Returns its NodeId, or -1 if the audio system is not initialized. */
export function createNode(state: AudioState, kind: NodeKind): number {
  if (!state.ctx) return -1;
  const ctx = state.ctx;
  let node: GraphNode;

  switch (kind) {
    case "gain": {
      const g = ctx.createGain();
      node = { kind, inputNode: g, outputNode: g, audioParams: new Map([["gain", g.gain]]) };
      break;
    }
    case "pan": {
      const p = ctx.createStereoPanner();
      node = { kind, inputNode: p, outputNode: p, audioParams: new Map([["pan", p.pan]]) };
      break;
    }
    case "low_pass": case "high_pass": case "band_pass": case "notch": {
      const f = ctx.createBiquadFilter();
      f.type = kind === "low_pass" ? "lowpass"
             : kind === "high_pass" ? "highpass"
             : kind === "band_pass" ? "bandpass"
             : "notch";
      node = { kind, inputNode: f, outputNode: f, audioParams: new Map([["cutoff", f.frequency], ["resonance", f.Q]]) };
      break;
    }
    case "compressor": {
      const c = ctx.createDynamicsCompressor();
      node = {
        kind, inputNode: c, outputNode: c,
        audioParams: new Map([
          ["threshold", c.threshold], ["ratio", c.ratio],
          ["attack", c.attack], ["release", c.release], ["knee", c.knee],
        ]),
      };
      break;
    }
    case "mixer": {
      // Web Audio naturally sums multiple inputs; a gain=1 node is the canonical mixer.
      const g = ctx.createGain();
      g.gain.value = 1;
      node = { kind, inputNode: g, outputNode: g, audioParams: new Map() };
      break;
    }
    case "reverb":
    case "delay":
      throw new Error(`createNode: "${kind}" not yet implemented`);
    default:
      throw new Error(`createNode: unknown kind "${kind as string}"`);
  }

  const id = state.nextNode++;
  state.nodes.set(id, node);
  return id;
}

/** Add a directed edge: from.output → to.input. */
export function connect(state: AudioState, from: number, to: number): void {
  const fromNode = state.nodes.get(from);
  const toNode = state.nodes.get(to);
  if (!fromNode || !toNode) return;
  fromNode.outputNode.connect(toNode.inputNode);
}

/** Remove a directed edge. Safe to call if not connected. */
export function disconnect(state: AudioState, from: number, to: number): void {
  const fromNode = state.nodes.get(from);
  const toNode = state.nodes.get(to);
  if (!fromNode || !toNode) return;
  try { fromNode.outputNode.disconnect(toNode.inputNode); } catch { /* not connected */ }
}

/**
 * Set or animate a node's parameter. Throws if kind is not valid for this node's kind.
 */
export function setNodeParam(state: AudioState, nodeId: number, kind: ParamKind, value: number, fadeMs = 0): void {
  const node = state.nodes.get(nodeId);
  if (!state.ctx || !node) return;
  const ap = node.audioParams.get(kind);
  if (ap === undefined) {
    throw new Error(`setNodeParam: node kind "${node.kind}" does not support param "${kind}"`);
  }
  if (fadeMs > 0) {
    ap.linearRampToValueAtTime(value, state.ctx.currentTime + fadeMs / 1000);
  } else {
    ap.value = value;
  }
}

/** Get the current value of a node parameter. Returns 0 if node or param not found. */
export function getNodeParam(state: AudioState, nodeId: number, kind: ParamKind): number {
  return state.nodes.get(nodeId)?.audioParams.get(kind)?.value ?? 0;
}

// ---- Internal voice helper ----

function _makeSource(
  state: AudioState,
  voiceId: number,
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
    const v = state.voices.get(voiceId);
    if (v && !v.paused) state.voices.delete(voiceId);
  };
  return source;
}

// ---- Hot tier: voice lifecycle ----

/**
 * Play a buffer, routing output through sinkId (a NodeId).
 * Returns a VoiceId, or -1 on failure.
 * All arguments are primitives — no object allocation at the call site.
 */
export function play(
  state: AudioState,
  bufferId: number,
  sinkId: number,
  loop: boolean,
  gain: number,
  pitch: number,
  pan: number,
  offset: number,
): number {
  if (!state.ctx) return -1;
  const buffer = state.buffers[bufferId];
  if (!buffer) return -1;
  const sink = state.nodes.get(sinkId);
  if (!sink) return -1;
  if (state.ctx.state === "suspended") void state.ctx.resume();

  const gainNode = state.ctx.createGain();
  gainNode.gain.value = gain;
  const panNode = state.ctx.createStereoPanner();
  panNode.pan.value = pan;
  gainNode.connect(panNode);
  panNode.connect(sink.inputNode);

  const voiceId = state.nextVoice++;
  const source = _makeSource(state, voiceId, buffer, gainNode, loop, pitch, offset);
  state.voices.set(voiceId, {
    source, gainNode, panNode, bufferId, sinkId, loop, pitch, pan,
    startTime: state.ctx.currentTime - offset,
    paused: false, pauseOffset: 0,
  });
  return voiceId;
}

export function stop(state: AudioState, voiceId: number): void {
  const v = state.voices.get(voiceId);
  if (!v) return;
  try { v.source.stop(); } catch { /* already ended */ }
  state.voices.delete(voiceId);
}

export function stopAll(state: AudioState): void {
  for (const id of [...state.voices.keys()]) stop(state, id);
}

export function pause(state: AudioState, voiceId: number): void {
  const v = state.voices.get(voiceId);
  if (!v || !state.ctx || v.paused) return;
  v.pauseOffset = state.ctx.currentTime - v.startTime;
  v.paused = true;
  try { v.source.stop(); } catch { /* already ended */ }
}

export function resume(state: AudioState, voiceId: number): void {
  const v = state.voices.get(voiceId);
  if (!v || !state.ctx || !v.paused) return;
  const buffer = state.buffers[v.bufferId];
  if (!buffer) return;
  v.source = _makeSource(state, voiceId, buffer, v.gainNode, v.loop, v.pitch, v.pauseOffset);
  v.startTime = state.ctx.currentTime - v.pauseOffset;
  v.paused = false;
}

export function resumeAll(state: AudioState): void {
  for (const id of state.voices.keys()) resume(state, id);
}

export function isPlaying(state: AudioState, voiceId: number): boolean {
  const v = state.voices.get(voiceId);
  return v !== undefined && !v.paused;
}

export function isPaused(state: AudioState, voiceId: number): boolean {
  return state.voices.get(voiceId)?.paused ?? false;
}

// ---- Hot tier: per-voice parameter control ----

export function setVoiceGain(state: AudioState, voiceId: number, gain: number, fadeMs = 0): void {
  const v = state.voices.get(voiceId);
  if (!v || !state.ctx) return;
  if (fadeMs > 0) {
    v.gainNode.gain.linearRampToValueAtTime(gain, state.ctx.currentTime + fadeMs / 1000);
  } else {
    v.gainNode.gain.value = gain;
  }
}

export function getVoiceGain(state: AudioState, voiceId: number): number {
  return state.voices.get(voiceId)?.gainNode.gain.value ?? 0;
}

export function setVoicePitch(state: AudioState, voiceId: number, pitch: number, fadeMs = 0): void {
  const v = state.voices.get(voiceId);
  if (!v || !state.ctx) return;
  if (fadeMs > 0) {
    v.source.playbackRate.linearRampToValueAtTime(pitch, state.ctx.currentTime + fadeMs / 1000);
  } else {
    v.source.playbackRate.value = pitch;
  }
  v.pitch = pitch;
}

export function getVoicePitch(state: AudioState, voiceId: number): number {
  return state.voices.get(voiceId)?.pitch ?? 1;
}

export function setVoicePan(state: AudioState, voiceId: number, pan: number): void {
  const v = state.voices.get(voiceId);
  if (!v) return;
  v.panNode.pan.value = pan;
  v.pan = pan;
}

export function getVoicePan(state: AudioState, voiceId: number): number {
  return state.voices.get(voiceId)?.pan ?? 0;
}

/** Convenience: set master gain (equivalent to setNodeParam(state, 0, "gain", gain)). */
export function setMasterGain(state: AudioState, gain: number): void {
  setNodeParam(state, 0, "gain", gain);
}

export function getPosition(state: AudioState, voiceId: number): number {
  const v = state.voices.get(voiceId);
  if (!v || !state.ctx) return 0;
  return v.paused ? v.pauseOffset : state.ctx.currentTime - v.startTime;
}

export function setPosition(state: AudioState, voiceId: number, pos: number): void {
  const v = state.voices.get(voiceId);
  if (!v || !state.ctx) return;
  const { bufferId, loop, gainNode, panNode, sinkId, pitch, pan } = v;
  stop(state, voiceId);
  const buffer = state.buffers[bufferId];
  if (!buffer) return;
  const sink = state.nodes.get(sinkId);
  if (!sink) return;
  if (state.ctx.state === "suspended") void state.ctx.resume();
  // Reuse the existing gainNode/panNode — they're still connected to the sink.
  const source = _makeSource(state, voiceId, buffer, gainNode, loop, pitch, pos);
  state.voices.set(voiceId, {
    source, gainNode, panNode, bufferId, sinkId, loop, pitch, pan,
    startTime: state.ctx.currentTime - pos,
    paused: false, pauseOffset: 0,
  });
}

export function soundLength(state: AudioState, bufferId: number): number {
  return state.buffers[bufferId]?.duration ?? 0;
}

// ---- Node-level bulk operations ----

/** Stop all voices currently routed to nodeId. */
export function stopNode(state: AudioState, nodeId: number): void {
  for (const [id, v] of [...state.voices.entries()]) {
    if (v.sinkId === nodeId) stop(state, id);
  }
}

/** Pause all non-paused voices currently routed to nodeId. */
export function pauseNode(state: AudioState, nodeId: number): void {
  for (const [id, v] of state.voices.entries()) {
    if (v.sinkId === nodeId && !v.paused) pause(state, id);
  }
}

/** Resume all paused voices currently routed to nodeId. */
export function resumeNode(state: AudioState, nodeId: number): void {
  for (const [id, v] of state.voices.entries()) {
    if (v.sinkId === nodeId && v.paused) resume(state, id);
  }
}
