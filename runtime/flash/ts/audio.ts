const audioCtx = new AudioContext();

interface SoundHandle {
  buffer: AudioBuffer;
}

interface ChannelHandle {
  source: AudioBufferSourceNode;
  gain: GainNode;
}

const channels: Set<ChannelHandle> = new Set();

export const audio = {
  async load_sound(url: string): Promise<SoundHandle> {
    const response = await fetch(url);
    const data = await response.arrayBuffer();
    const buffer = await audioCtx.decodeAudioData(data);
    return { buffer };
  },

  play(sound: SoundHandle): ChannelHandle {
    const source = audioCtx.createBufferSource();
    const gain = audioCtx.createGain();
    source.buffer = sound.buffer;
    source.connect(gain).connect(audioCtx.destination);
    const channel: ChannelHandle = { source, gain };
    channels.add(channel);
    source.onended = () => channels.delete(channel);
    source.start();
    return channel;
  },

  play_looped(sound: SoundHandle): ChannelHandle {
    const source = audioCtx.createBufferSource();
    const gain = audioCtx.createGain();
    source.buffer = sound.buffer;
    source.loop = true;
    source.connect(gain).connect(audioCtx.destination);
    const channel: ChannelHandle = { source, gain };
    channels.add(channel);
    source.start();
    return channel;
  },

  stop(channel: ChannelHandle): void {
    try {
      channel.source.stop();
    } catch {
      // Already stopped.
    }
    channels.delete(channel);
  },

  set_volume(channel: ChannelHandle, volume: number): void {
    channel.gain.gain.value = volume;
  },

  is_playing(channel: ChannelHandle): boolean {
    return channels.has(channel);
  },

  stop_all(): void {
    for (const channel of channels) {
      try {
        channel.source.stop();
      } catch {
        // Already stopped.
      }
    }
    channels.clear();
  },
};
