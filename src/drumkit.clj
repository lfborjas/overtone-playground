(ns overtone-playground.drumkit
  (:use [overtone.live]))

;; from:
;; https://github.com/overtone/overtone/wiki/Metronome-and-sequencing

;; warning: loading samples crashes my mac in the current version, had to
;; do this: https://github.com/overtone/overtone/issues/448#issuecomment-587891076
(def kick (sample (freesound-path 2086)))
(def default-bpm (metronome 120))

(defn looper [metro sound]
  (let [beat (metro)]
    (at (metro beat) (sound))
    (apply-by (metro (inc beat))
              looper
              metro
              sound
              [])))

(comment
  (looper default-bpm kick)
  (stop))

;; And from:
;; https://github.com/overtone/overtone/wiki/Live-coding

(definst kick [freq 120 dur 0.3 width 0.5]
  (let [freq-env (* freq (env-gen (perc 0 (* 0.99 dur))))
        env (env-gen (perc 0.01 dur) 1 1 0 1 FREE)
        sqr (* (env-gen (perc 0 0.01)) (pulse (* 2 freq) width))
        src (sin-osc freq-env)
        drum (+ sqr (* env src))]
    (compander drum drum 0.2 1 0.1 0.01 0.01)))

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(defn player [metro beat]
  (at (metro beat) (kick))
  (at (metro (+ 0.5 beat)) (c-hat))
  (apply-by (metro (inc beat))
            #'player
            metro
            (inc beat)
            []))

(player default-bpm (default-bpm))
(stop)

;; from https://github.com/overtone/overtone/wiki/Pitches-and-Chords

(definst saw-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4] 
  (* (env-gen (env-lin attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))

(comment
  (saw-wave (midi->hz (note :C5))))

(defn note->hz [music-note]
  (-> music-note
      (note)
      (midi->hz)))

(defn saw2 [music-note]
  (-> music-note
      (note->hz)
      (saw-wave)))

(comment
  (saw2 :c5))

(defn play-chord [a-chord]
  (doseq [note a-chord]
    (saw2 note)))

(comment
  (play-chord (chord :C4 :major)))

(defn chord-progression-time []
  (let [time (now)]
    (at time (play-chord (chord :C4 :major)))
    (at (+ 2000 time) (play-chord (chord :G3 :major)))
    (at (+ 3000 time) (play-chord (chord :F3 :sus4)))
    (at (+ 4300 time) (play-chord (chord :F3 :major)))
    (at (+ 5000 time) (play-chord (chord :G3 :major)))))

(comment (chord-progression-time))

(defn chord-progression-beat [metro beat]
  (let [time (now)]
    (at (metro beat)  (play-chord (chord :C4 :major)))
    (at (metro (+ 4 beat)) (play-chord (chord :G3 :major)))
    (at (metro (+ 8 beat)) (play-chord (chord :A3 :minor)))
    (at (metro (+ 14 beat)) (play-chord (chord :F3 :major)))
    (apply-at (metro (+ 16 beat))
              chord-progression-beat
              metro
              (+ 16 beat)
              [])))

(comment
  (let [faster (metronome 240)]
    (chord-progression-beat faster (faster)))
  (stop)

  ;; from
  ;; https://github.com/overtone/overtone/blob/6ac57556014875357ef0c3355a11602069420a50/src/overtone/music/pitch.clj
  ;; this is kinda life-changing: could write a little `:midi-on`
  ;; handler for my bluetooth piano, that groups notes together per time/flag/permutation
  ;; and tries to apply `find-chord`? (it returns `nil` if it can't identify it,
  ;; perhaps 
  (find-chord (chord :C4 :major)))



