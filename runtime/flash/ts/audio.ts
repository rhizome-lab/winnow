/**
 * Flash audio shim — implements the load_sound/play/stop API used by emitted
 * Flash code, backed by the shared platform audio (node graph, Web Audio API).
 *
 * Handles are opaque numbers: BufferHandle for loaded sounds, VoiceHandle for
 * playing instances. Emitted code treats them as opaque and passes them back
 * to these functions — no assumption about their internal representation.
 *
 * TODO: this module uses a module-level AudioState singleton, which violates
 * the "no module-level mutable state" rule and prevents multiple Flash game
 * instances from coexisting on the same page. All Flash shims (audio, renderer,
 * input, timing, save, ui) have this same issue. Fix by consolidating all shim
 * state onto a FlashRuntime instance and threading it through emitted code.
 */

import {
  AudioState, loadAudio,
  play, stop, stopAll, isPlaying,
  setVoiceGain, getVoiceGain, setVoicePan, getVoicePan,
  setVoicePitch, getVoicePitch,
  setMasterGain,
  pause, resume,
  getPosition, bufferDuration,
} from "./shared/platform/audio";

const _audio = new AudioState();

export const audio = {
  /**
   * Decode an audio file from a URL and return a BufferHandle.
   * Returns -1 on failure.
   */
  async load_sound(url: string): Promise<number> {
    try {
      return await loadAudio(_audio, url, url);
    } catch {
      return -1;
    }
  },

  /** Play a loaded sound. Returns a VoiceHandle, or -1 on failure. */
  play(bufferId: number, loop = false, gain = 1, pan = 0, pitch = 1, offset = 0): number {
    return play(_audio, bufferId, 0, loop, gain, pitch, pan, offset);
  },

  /** Stop a playing voice. */
  stop(voiceId: number): void { stop(_audio, voiceId); },

  /** Stop all playing voices. */
  stop_all(): void { stopAll(_audio); },

  pause(voiceId: number): void { pause(_audio, voiceId); },
  resume(voiceId: number): void { resume(_audio, voiceId); },

  is_playing(voiceId: number): boolean { return isPlaying(_audio, voiceId); },

  set_volume(voiceId: number, volume: number): void { setVoiceGain(_audio, voiceId, volume); },
  get_volume(voiceId: number): number { return getVoiceGain(_audio, voiceId); },

  set_pan(voiceId: number, pan: number): void { setVoicePan(_audio, voiceId, pan); },
  get_pan(voiceId: number): number { return getVoicePan(_audio, voiceId); },

  set_pitch(voiceId: number, pitch: number): void { setVoicePitch(_audio, voiceId, pitch); },
  get_pitch(voiceId: number): number { return getVoicePitch(_audio, voiceId); },

  set_master_volume(volume: number): void { setMasterGain(_audio, volume); },

  get_position(voiceId: number): number { return getPosition(_audio, voiceId); },
  sound_length(bufferId: number): number { return bufferDuration(_audio, bufferId); },
};
