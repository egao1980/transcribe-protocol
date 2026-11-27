# transcribe-protocol

Monophonic audio-to-notes transcription for [cl-stack](https://github.com/egao1980/cl-stack): sung or hummed melodies (a recorded take, 16 kHz mono) to note events with onsets and offsets in seconds and MIDI pitches with cents, the f0 contour with voicing, the take's tuning offset and per-phrase drift, and a key fit. Builds on [audio-protocol](https://github.com/egao1980/audio-protocol) (PCM buffers, resampling, DSP) and [music-theory](https://github.com/egao1980/music-theory) (key fit with a tuning offset).

**Status:** skeleton (0.0.1). The design is in the capability brief [cl-stack/docs/capabilities/transcribe.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/transcribe.md) (slice T0). The API and tests land through contract branches and team-solve.

```sh
cl-repo test --system transcribe-protocol
```

## License

MIT, see [LICENSE](LICENSE).
