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

// --- Audio cache ---

const audioCache: Map<string, AudioHandle> = new Map();

/** <<cacheaudio "name" "source1" "source2" ...>> */
export function cacheaudio(name: string, ...sources: string[]): void {
  const handle = createAudio(sources);
  audioCache.set(name, handle);
}

/** <<audio "name" action ...args>> */
export function audio(name: string, action: string, ...args: any[]): void {
  const handle = audioCache.get(name);
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

// --- Master audio ---

let masterVolume = 1;
let masterMuted = false;

/** <<masteraudio action ...args>> */
export function masteraudio(action: string, ...args: any[]): void {
  switch (action) {
    case "volume": {
      masterVolume = typeof args[0] === "number" ? args[0] : 1;
      applyMasterVolume();
      break;
    }
    case "mute":
      masterMuted = true;
      applyMasterMute();
      break;
    case "unmute":
      masterMuted = false;
      applyMasterMute();
      break;
    case "stop":
      for (const handle of audioCache.values()) {
        stopAudio(handle);
      }
      break;
    default:
      console.warn(`[masteraudio] unknown action: "${action}"`);
  }
}

function applyMasterVolume(): void {
  for (const handle of audioCache.values()) {
    setVolume(handle, masterVolume);
  }
}

function applyMasterMute(): void {
  for (const handle of audioCache.values()) {
    setMuted(handle, masterMuted);
  }
}

// --- Playlists ---

interface Playlist {
  tracks: string[];
  currentIndex: number;
  loop: boolean;
}

const playlists: Map<string, Playlist> = new Map();

/** <<createplaylist "name" "track1" "track2" ...>> */
export function createplaylist(name: string, ...tracks: string[]): void {
  playlists.set(name, { tracks, currentIndex: 0, loop: false });
}

/** <<playlist "name" action>> */
export function playlist(name: string, action: string): void {
  const pl = playlists.get(name);
  if (!pl) {
    console.warn(`[playlist] not found: "${name}"`);
    return;
  }

  switch (action) {
    case "play":
      playPlaylistTrack(pl);
      break;
    case "stop":
      for (const track of pl.tracks) {
        const handle = audioCache.get(track);
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

function playPlaylistTrack(pl: Playlist): void {
  if (pl.currentIndex >= pl.tracks.length) {
    if (pl.loop) {
      pl.currentIndex = 0;
    } else {
      return;
    }
  }
  const trackName = pl.tracks[pl.currentIndex];
  const handle = audioCache.get(trackName);
  if (handle) {
    // Listen for track end to play next
    handle.addEventListener(
      "ended",
      () => {
        pl.currentIndex++;
        playPlaylistTrack(pl);
      },
      { once: true },
    );
    playAudio(handle);
  }
}

// --- Audio groups ---

const audioGroups: Map<string, string[]> = new Map();

/** <<createaudiogroup "name" "track1" "track2" ...>> */
export function createaudiogroup(name: string, ...tracks: string[]): void {
  audioGroups.set(name, tracks);
}

/** <<removeaudio "name">> */
export function removeaudio(name: string): void {
  const handle = audioCache.get(name);
  if (handle) {
    stopAudio(handle);
    audioCache.delete(name);
  }
}

/** <<removeplaylist "name">> */
export function removeplaylist(name: string): void {
  playlists.delete(name);
}

/** <<removeaudiogroup "name">> */
export function removeaudiogroup(name: string): void {
  audioGroups.delete(name);
}

/** <<waitforaudio>> — returns a promise that resolves when all audio is ready. */
export function waitforaudio(): Promise<void> {
  const promises: Promise<void>[] = [];
  for (const handle of audioCache.values()) {
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
