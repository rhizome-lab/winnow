/** SugarCube audio playback.
 *
 * All audio operations go through the platform layer so deployers
 * can swap HTMLAudioElement for Web Audio API or custom backends.
 */

import {
  createAudio,
  playAudio,
  pauseAudio,
  stopAudio,
  setVolume,
  setMuted,
  setLoop,
  seekAudio,
  fadeAudio,
  isAudioReady,
  type AudioHandle,
} from "../platform";

export interface Playlist {
  tracks: string[];
  currentIndex: number;
  loop: boolean;
}

export class SCAudio {
  readonly audioCache: Map<string, AudioHandle> = new Map();
  readonly playlists: Map<string, Playlist> = new Map();
  readonly audioGroups: Map<string, string[]> = new Map();
  masterVolume = 1;
  masterMuted = false;

  /** <<cacheaudio "name" "source1" "source2" ...>> */
  cacheaudio(name: string, ...sources: string[]): void {
    const handle = createAudio(sources);
    this.audioCache.set(name, handle);
  }

  /** <<audio "name" action ...args>> */
  audio(name: string, action: string, ...args: any[]): void {
    const handle = this.audioCache.get(name);
    if (!handle) {
      console.warn(`[audio] track not found: "${name}"`);
      return;
    }

    switch (action) {
      case "play":
        playAudio(handle);
        break;
      case "pause":
        pauseAudio(handle);
        break;
      case "stop":
        stopAudio(handle);
        break;
      case "fadeIn":
      case "fadein": {
        const duration = typeof args[0] === "number" ? args[0] : 5000;
        setVolume(handle, 0);
        playAudio(handle);
        fadeAudio(handle, 1, duration);
        break;
      }
      case "fadeOut":
      case "fadeout": {
        const duration = typeof args[0] === "number" ? args[0] : 5000;
        fadeAudio(handle, 0, duration).then(() => pauseAudio(handle));
        break;
      }
      case "fadeTo":
      case "fadeto": {
        const vol = typeof args[0] === "number" ? args[0] : 1;
        const duration = typeof args[1] === "number" ? args[1] : 5000;
        fadeAudio(handle, vol, duration);
        break;
      }
      case "volume": {
        const vol = typeof args[0] === "number" ? args[0] : 1;
        setVolume(handle, vol);
        break;
      }
      case "mute":
        setMuted(handle, true);
        break;
      case "unmute":
        setMuted(handle, false);
        break;
      case "time": {
        const time = typeof args[0] === "number" ? args[0] : 0;
        seekAudio(handle, time);
        break;
      }
      case "loop":
        setLoop(handle, true);
        break;
      case "unloop":
        setLoop(handle, false);
        break;
      default:
        console.warn(`[audio] unknown action: "${action}"`);
    }
  }

  /** <<masteraudio action ...args>> */
  masteraudio(action: string, ...args: any[]): void {
    switch (action) {
      case "volume": {
        this.masterVolume = typeof args[0] === "number" ? args[0] : 1;
        this.applyMasterVolume();
        break;
      }
      case "mute":
        this.masterMuted = true;
        this.applyMasterMute();
        break;
      case "unmute":
        this.masterMuted = false;
        this.applyMasterMute();
        break;
      case "stop":
        for (const handle of this.audioCache.values()) {
          stopAudio(handle);
        }
        break;
      default:
        console.warn(`[masteraudio] unknown action: "${action}"`);
    }
  }

  private applyMasterVolume(): void {
    for (const handle of this.audioCache.values()) {
      setVolume(handle, this.masterVolume);
    }
  }

  private applyMasterMute(): void {
    for (const handle of this.audioCache.values()) {
      setMuted(handle, this.masterMuted);
    }
  }

  /** <<createplaylist "name" "track1" "track2" ...>> */
  createplaylist(name: string, ...tracks: string[]): void {
    this.playlists.set(name, { tracks, currentIndex: 0, loop: false });
  }

  /** <<playlist "name" action>> */
  playlist(name: string, action: string): void {
    const pl = this.playlists.get(name);
    if (!pl) {
      console.warn(`[playlist] not found: "${name}"`);
      return;
    }

    switch (action) {
      case "play":
        this.playPlaylistTrack(pl);
        break;
      case "stop":
        for (const track of pl.tracks) {
          const handle = this.audioCache.get(track);
          if (handle) stopAudio(handle);
        }
        break;
      case "loop":
        pl.loop = true;
        break;
      case "unloop":
        pl.loop = false;
        break;
      default:
        console.warn(`[playlist] unknown action: "${action}"`);
    }
  }

  private playPlaylistTrack(pl: Playlist): void {
    if (pl.currentIndex >= pl.tracks.length) {
      if (pl.loop) {
        pl.currentIndex = 0;
      } else {
        return;
      }
    }
    const trackName = pl.tracks[pl.currentIndex];
    const handle = this.audioCache.get(trackName);
    if (handle) {
      handle.addEventListener(
        "ended",
        () => {
          pl.currentIndex++;
          this.playPlaylistTrack(pl);
        },
        { once: true },
      );
      playAudio(handle);
    }
  }

  /** <<createaudiogroup "name" "track1" "track2" ...>> */
  createaudiogroup(name: string, ...tracks: string[]): void {
    this.audioGroups.set(name, tracks);
  }

  /** <<removeaudio "name">> */
  removeaudio(name: string): void {
    const handle = this.audioCache.get(name);
    if (handle) {
      stopAudio(handle);
      this.audioCache.delete(name);
    }
  }

  /** <<removeplaylist "name">> */
  removeplaylist(name: string): void {
    this.playlists.delete(name);
  }

  /** <<removeaudiogroup "name">> */
  removeaudiogroup(name: string): void {
    this.audioGroups.delete(name);
  }

  /** <<waitforaudio>> — returns a promise that resolves when all audio is ready. */
  waitforaudio(): Promise<void> {
    const promises: Promise<void>[] = [];
    for (const handle of this.audioCache.values()) {
      if (!isAudioReady(handle)) {
        promises.push(
          new Promise((resolve) => {
            handle.addEventListener("canplaythrough", () => resolve(), {
              once: true,
            });
          }),
        );
      }
    }
    return Promise.all(promises).then(() => {});
  }
}
