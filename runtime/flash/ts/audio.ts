/**
 * Flash audio shim — implements the load_sound/play/stop API used by emitted
 * Flash code, backed by the shared platform audio (node graph, Web Audio API).
 *
 * TODO: this shim (like all Flash shims) uses a module-level singleton via
 * initFlash(). Fix by threading a FlashShims instance through the Flash emitter
 * so emitted code receives the runtime as a parameter rather than importing a
 * singleton.
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

export class AudioShim {
  private _state = new AudioState();

  /**
   * Decode an audio file from a URL and return a BufferHandle.
   * Returns -1 on failure.
   */
  async load_sound(url: string): Promise<number> {
    try {
      return await loadAudio(this._state, url, url);
    } catch {
      return -1;
    }
  }

  /** Play a loaded sound. Returns a VoiceHandle, or -1 on failure. */
  play(bufferId: number, loop = false, gain = 1, pan = 0, pitch = 1, offset = 0): number {
    return play(this._state, bufferId, 0, loop, gain, pitch, pan, offset);
  }

  stop(voiceId: number): void { stop(this._state, voiceId); }
  stop_all(): void { stopAll(this._state); }
  pause(voiceId: number): void { pause(this._state, voiceId); }
  resume(voiceId: number): void { resume(this._state, voiceId); }
  is_playing(voiceId: number): boolean { return isPlaying(this._state, voiceId); }

  set_volume(voiceId: number, volume: number): void { setVoiceGain(this._state, voiceId, volume); }
  get_volume(voiceId: number): number { return getVoiceGain(this._state, voiceId); }
  set_pan(voiceId: number, pan: number): void { setVoicePan(this._state, voiceId, pan); }
  get_pan(voiceId: number): number { return getVoicePan(this._state, voiceId); }
  set_pitch(voiceId: number, pitch: number): void { setVoicePitch(this._state, voiceId, pitch); }
  get_pitch(voiceId: number): number { return getVoicePitch(this._state, voiceId); }
  set_master_volume(volume: number): void { setMasterGain(this._state, volume); }
  get_position(voiceId: number): number { return getPosition(this._state, voiceId); }
  sound_length(bufferId: number): number { return bufferDuration(this._state, bufferId); }
}
