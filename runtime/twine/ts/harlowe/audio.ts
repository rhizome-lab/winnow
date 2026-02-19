/** Harlowe Audio (HAL) runtime module.
 *
 * Implements `Harlowe.Audio.*` system calls for the HAL (Harlowe Audio
 * Library) macros: `(track:)`, `(masteraudio:)`, `(newtrack:)`,
 * `(newplaylist:)`, `(newgroup:)`, `(playlist:)`, `(group:)`.
 *
 * HAL is a third-party library by ChapelR (https://github.com/ChapelR/harlowe-audio).
 * Games can opt out of HAL translation via `"hal_audio": false` in the
 * `frontend_options` section of `reincarnate.json`, which causes the macros to
 * fall through to `unknown_macro()` instead.
 */

export interface HarloweAudioOpts {
  /** Disable all audio (no-op all methods). Defaults to true (enabled). */
  enabled?: boolean;
}

export class HarloweAudio {
  private enabled: boolean;
  private tracks = new Map<string, HTMLAudioElement>();
  private groups = new Map<string, Set<string>>();
  private playlists = new Map<string, string[]>();
  private masterVolume = 0.5;
  private masterMuted = false;

  constructor(opts?: HarloweAudioOpts) {
    this.enabled = opts?.enabled !== false;
    // Built-in HAL groups
    this.groups.set("playing", new Set());
    this.groups.set("looping", new Set());
  }

  // --- Configuration (called from __user_script_hal_init) ---

  /** Apply a key/value pair from `hal.config`. */
  configure(key: string, value: string): void {
    if (!this.enabled) return;
    switch (key) {
      case "startingVol": {
        const vol = Number(value);
        if (!isNaN(vol)) this.masterVolume = Math.min(1, Math.max(0, vol));
        break;
      }
      // muteOnBlur, showControls, preload, etc. — informational; no runtime effect
    }
  }

  /** Register a track from `hal.tracks` or `(newtrack:)`. */
  define_track(name: string, ...urls: string[]): void {
    if (!this.enabled || urls.length === 0 || this.tracks.has(name)) return;
    const audio = new Audio();
    audio.preload = "auto";
    // Multi-format fallback via <source> elements
    for (const url of urls) {
      const src = document.createElement("source");
      src.src = url;
      audio.appendChild(src);
    }
    audio.volume = this.masterVolume;
    audio.muted = this.masterMuted;
    // Maintain the built-in "playing" group automatically
    audio.addEventListener("play", () => this.groups.get("playing")!.add(name));
    audio.addEventListener("pause", () => this.groups.get("playing")!.delete(name));
    audio.addEventListener("ended", () => this.groups.get("playing")!.delete(name));
    this.tracks.set(name, audio);
  }

  /** Register a group from `(newgroup:)`. */
  define_group(name: string, ...trackNames: string[]): void {
    if (!this.enabled) return;
    this.groups.set(name, new Set(trackNames));
  }

  /** Register a playlist from `(newplaylist:)`. */
  define_playlist(name: string, ...trackNames: string[]): void {
    if (!this.enabled) return;
    this.playlists.set(name, [...trackNames]);
  }

  // --- Track control: (track: name, command, ...args) ---

  track(name: string, command: string, ...args: any[]): any {
    if (!this.enabled) return;
    const audio = this.tracks.get(name);
    if (!audio) {
      console.warn(`[harlowe-audio] unknown track: "${name}"`);
      return;
    }
    return this._runCommand(name, audio, command, args);
  }

  // --- Master audio: (masteraudio: command, ...args) ---

  master_audio(command: string, ...args: any[]): void {
    if (!this.enabled) return;
    switch (command) {
      case "stopall":
        for (const [, audio] of this.tracks) {
          audio.pause();
          audio.currentTime = 0;
        }
        break;
      case "mute":
        this.masterMuted = true;
        for (const [, audio] of this.tracks) audio.muted = true;
        break;
      case "unmute":
        this.masterMuted = false;
        for (const [, audio] of this.tracks) audio.muted = false;
        break;
      case "volume": {
        const vol = Number(args[0]);
        if (!isNaN(vol)) {
          this.masterVolume = Math.min(1, Math.max(0, vol));
          for (const [, audio] of this.tracks) audio.volume = this.masterVolume;
        }
        break;
      }
      default:
        console.warn(`[harlowe-audio] unknown masteraudio command: "${command}"`);
    }
  }

  // --- Group control: (group: name, command, ...args) ---

  group(name: string, command: string, ...args: any[]): void {
    if (!this.enabled) return;
    const members = this.groups.get(name);
    if (!members) {
      console.warn(`[harlowe-audio] unknown group: "${name}"`);
      return;
    }
    for (const trackName of members) {
      const audio = this.tracks.get(trackName);
      if (audio) this._runCommand(trackName, audio, command, args);
    }
  }

  // --- Playlist control: (playlist: name, command, ...args) ---

  playlist(name: string, command: string, ...args: any[]): void {
    if (!this.enabled) return;
    const trackNames = this.playlists.get(name);
    if (!trackNames) {
      console.warn(`[harlowe-audio] unknown playlist: "${name}"`);
      return;
    }
    for (const trackName of trackNames) {
      const audio = this.tracks.get(trackName);
      if (audio) this._runCommand(trackName, audio, command, args);
    }
  }

  // --- Internal command dispatch ---

  private _runCommand(
    name: string,
    audio: HTMLAudioElement,
    command: string,
    args: any[],
  ): any {
    switch (command) {
      case "play":
        audio.loop = args[1] === true || args[1] === "true";
        audio.play().catch(() => {});
        break;
      case "playwhenpossible":
        this._playWhenPossible(audio);
        break;
      case "stop":
        audio.pause();
        audio.currentTime = 0;
        break;
      case "pause":
        audio.pause();
        break;
      case "loop": {
        const on = args[0] === true || args[0] === "true";
        audio.loop = on;
        const looping = this.groups.get("looping")!;
        on ? looping.add(name) : looping.delete(name);
        break;
      }
      case "volume": {
        const vol = Number(args[0]);
        if (!isNaN(vol)) audio.volume = Math.min(1, Math.max(0, vol));
        break;
      }
      case "mute":
        audio.muted = true;
        break;
      case "unmute":
        audio.muted = false;
        break;
      case "fade": {
        // fade(targetVol, durationSecs)
        const target = Math.min(1, Math.max(0, Number(args[0] ?? 0)));
        const duration = Number(args[1] ?? 1) * 1000;
        const start = audio.volume;
        const t0 = performance.now();
        const step = (now: number): void => {
          const progress = Math.min(1, (now - t0) / duration);
          audio.volume = start + (target - start) * progress;
          if (progress < 1) requestAnimationFrame(step);
        };
        requestAnimationFrame(step);
        break;
      }
      case "isplaying":
        return !audio.paused && !audio.ended;
      default:
        console.warn(`[harlowe-audio] unknown track command: "${command}"`);
    }
  }

  private _playWhenPossible(audio: HTMLAudioElement): void {
    const tryPlay = (): void => { audio.play().catch(() => {}); };
    audio.play().catch(() => {
      // Autoplay blocked — wait for first user gesture
      const handler = (): void => {
        audio.play().catch(() => {});
      };
      document.addEventListener("click", handler, { once: true });
      document.addEventListener("keydown", handler, { once: true });
    });
    void tryPlay; // keep reference alive
  }
}
