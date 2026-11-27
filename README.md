# transcribe-protocol

Monophonic audio-to-notes transcription for [cl-stack](https://github.com/egao1980/cl-stack): one sung or hummed line (a recorded take, 16 kHz mono) in, note events out, with onsets and offsets in seconds and MIDI pitches with cents. The result also carries the f0 contour with voicing, the take's tuning offset, its drift per phrase, and a key fit. Batch only.

- **Backends detect, the protocol finishes.** A backend subclasses `transcriber` and writes `detect-notes`, which returns the contour and the notes as sung. `transcribe` checks and normalizes the hints (`:key :meter :tempo :pitch-range`). It downmixes the audio, checks it for length and silence, and resamples it to the backend's rate. Then `finish-transcription` estimates the take's tuning and per-phrase drift, removes them, and fits the key over the corrected notes with [music-theory](https://github.com/egao1980/music-theory)'s `fit-key`. So a take sung a semitone below its key hint reads hint distance −1.
- **Evaluation:** `note-f-measure` and `match-notes`, mir_eval's note metrics: onsets within 50 ms, pitches within 50 cents, offsets optional, maximum bipartite matching, exact arithmetic.
- **Reference backends:** `oracle-transcriber` returns given notes, for consumers' tests. `yin-transcriber` is a plain YIN baseline on [audio-protocol](https://github.com/egao1980/audio-protocol)'s DSP package.
- **Test support** (`transcribe-protocol/testing`): melodies as note events, detuned takes, and synthesis of note events to PCM (sine or band-limited sawtooth, vibrato, drift, a global offset, an envelope, seeded noise), so every backend tests against known notes.

```lisp
(asdf:load-system "transcribe-protocol")

(let ((take (audio-protocol:read-wav #p"take.wav")))
  (stack-transcribe:transcribe (make-instance 'stack-transcribe:yin-transcriber) take
                               :hints '(:key "F" :meter "3/4" :tempo 90)))
;; → a TRANSCRIPTION: NOTES (retuned), RAW-NOTES (as sung), CONTOUR,
;;   TUNING-OFFSET, DRIFT, KEY-FIT, HINTS, BACKEND, DURATION
;; (note-plists (transcription-notes tr)) is the event list of
;; score-backend-midi's NOTES-TO-SCORE.
```

**Status:** slice T0 (0.1.0) is **implemented**: every function below is written, and the Rove suite passes (225 tests). The design is in the capability brief [cl-stack/docs/capabilities/transcribe.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/transcribe.md). Each file header in `src/` states its rules, and the Rove suite in `tests/` is the spec. The pYIN backend comes next, in its own repository (`transcribe-backend-pyin`, slice T1).

| File | What | Status |
|------|------|--------|
| `src/conditions.lisp` | `transcribe-error`, `unusable-audio` (`audio-too-short`, `silent-audio`), `invalid-hint`, `invalid-notes`, `transcriber-error`; restarts are CLHS `continue` and `use-value` | T0, given |
| `src/notes.lisp` | `note-event` (rational seconds, double MIDI pitch, confidence), `midi-to-hz`, `hz-to-midi` | T0, given |
| | `check-notes` (one line in time order), `note-plists` | T0, implemented |
| `src/contour.lisp` | `f0-contour` (frame i at i · hop / rate, f0 in Hz or NIL, voicing) | T0, given |
| | `notes-contour` | T0, implemented |
| `src/hints.lisp` | `+hint-keys+`, `+default-pitch-range+` | T0, given |
| | `normalize-hints` (with `use-value` and `continue`), `hint-pitch-range` | T0, implemented |
| `src/tuning.lisp` | `+tuning-offsets+`, `+phrase-gap+`, `+min-phrase-notes+`, `phrase-drift` | T0, given |
| | `estimate-tuning` (a duration-weighted circular median on a 1-cent grid), `segment-phrases`, `estimate-drift`, `remove-drift` | T0, implemented |
| `src/transcription.lisp` | `transcription` | T0, given |
| | `finish-transcription` (tuning, drift, key fit) | T0, implemented |
| `src/protocol.lisp` | `transcriber`, `transcriber-sample-rate`, `transcriber-info`, `*minimum-duration*`, `*silence-peak-db*` | T0, given |
| | `transcribe`, `detect-notes` (the default method), `prepare-audio` | T0, implemented |
| `src/evaluate.lisp` | `match-notes`, `note-f-measure` | T0, implemented |
| `src/backends.lisp` | the `oracle-transcriber` and `yin-transcriber` classes, initargs and their checks | T0, given |
| | their `detect-notes` methods, and the oracle's note check | T0, implemented |
| `src/testing.lisp` | system `transcribe-protocol/testing`: `make-park-miller`, `melody-notes`, `detune-notes`, `synthesize-notes` | T0, implemented |

```sh
CL_REPO_DYNAMIC_SPACE_MB=2048 cl-repo test --system transcribe-protocol
```

## License

MIT, see [LICENSE](LICENSE).
