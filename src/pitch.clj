(ns overtone-playground.pitch
  (:use [overtone.live]))

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
