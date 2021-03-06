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

(definst kick2 [freq 120 dur 0.3 width 0.5]
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
  (at (metro beat) (kick2))
  (at (metro (+ 0.5 beat)) (c-hat))
  (apply-by (metro (inc beat))
            #'player
            metro
            (inc beat)
            []))

(player default-bpm (default-bpm))
(stop)

;; ideas: live sequencer from launchpad.clj, but using a :^dynamic metronome
;; which can be made faster/slower with midi control events (by replacing
;; the current definition of the metronome, or perhaps looking into an atom?


