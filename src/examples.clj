(ns overtone-playground.examples
  (:use [overtone.live]
        [overtone.inst.piano]
        [overtone.inst.drum]))

;; From the "getting started" page:
;; https://github.com/overtone/overtone/wiki/Getting-Started
(comment
  (definst foo [] (saw 200))

  (foo)
  (kill foo)

  (definst bar [freq 220] (saw freq))
  (bar 110)
  (kill bar)

  (definst baz [freq 440] (* 0.3 (saw freq)))

  (do (baz 220)
      (baz 660))

  (kill baz)

  (do (foo)
      (bar)
      (baz))

  (stop)


  (definst quux [freq 440]
    (* 0.3 (saw freq)))

  (quux)
  (ctl quux :freq 440)

  (stop)

  (do (foo)
      (bar 440)
      (baz 880)
      (quux 220))

  (definst trem [freq 440
                 depth 10
                 rate 6
                 length 6]
    (* 0.3
       (line:kr 0 1 length FREE)
       (saw (+ freq (* depth (sin-osc:kr rate))))))

  (do (trem)
      (trem 200 60 0.8)
      (trem 60 30 0.2))

  ;; From:
  ;; https://github.com/overtone/overtone/blob/master/src/overtone/examples/getting_started/pragpub-article.clj

  (piano)
  (piano 63)

  (def c4-minor [(note :C4)
                 (note :Eb4)
                 (note :G4)])

  (doseq [note c4-minor]
    (piano note))

  ;; Reworking the drum bits with the included drum)
  )


(defn beat [curr-t sep-t]
  (at curr-t (dub-kick))
  (let [new-t (+ curr-t sep-t)]
    (apply-by new-t #'beat [new-t sep-t])))

#_(beat (now) 600)


;; musical recursion!
;; https://github.com/overtone/overtone/blob/master/src/overtone/examples/getting_started/pragpub-article.clj

(defn simple-sequencer [curr-t sep-t pattern]
  (at curr-t (when (= 1 (first pattern))
               (dub-kick)))
  (let [new-t (+ curr-t sep-t)]
    (apply-by new-t #'simple-sequencer [new-t sep-t (rest pattern)])))

(comment (simple-sequencer (now) 200 (cycle [1 0 1 1 0 0 0 1]))
         (simple-sequencer (now) 200 (cycle [1 1 0 1 0 1 0 0]))
         (stop))

(def pats {dub-kick [1 1 0 1 0 1 0 0]
           snare    [1 0 0 1 0 0 1 0]
           open-hat [1 0 0 0 0 0 0 1]})

(defn play-pattern [curr-t sep-t pattern sound]
  (at curr-t (when (= 1 (first pattern))
               (sound)))
  (let [new-t (+ curr-t sep-t)]
    (apply-by new-t #'play-pattern [new-t sep-t (rest pattern) sound])))

(defn sequencer [sep-t sequences]
  (let [t (+ (now) 200)]
    (doseq [[sound pattern] sequences]
      (play-pattern t sep-t (cycle pattern) sound))))

(comment (sequencer 200 pats)
         (stop))

(def live-pats (atom pats))

(defn live-sequencer
  ([curr-t sep-t live-patterns]
   (live-sequencer curr-t sep-t live-patterns 0))
  ([curr-t sep-t live-patterns beat]
   (doseq [[sound pattern] @live-patterns
           :when (= 1 (nth pattern (mod beat (count pattern))))]
     (at curr-t (sound)))
   (let [new-t (+ curr-t sep-t)]
     (apply-by new-t #'live-sequencer [new-t sep-t live-patterns (inc beat)]))))

(comment
  (live-sequencer (+ 200 (now))
                  200
                  live-pats)

  (swap! live-pats assoc dub-kick [1 1 0 1 0 0 1 1])
  (swap! live-pats assoc snare    [1 1 0 0 0 1 0 0])
  (swap! live-pats assoc open-hat [1 0 1 0 1 0 0 1])
  (stop)

  (dance-kick))

;; skipping some of the argument-list based ones, since we're not using freesound sounds

(defn flatten1
  "Takes a map and returns a seq of all the key val pairs:
      (flatten1 {:a 1 :b 2 :c 3}) ;=> (:b 2 :c 3 :a 1)"
  [m]
  (reduce (fn [r [arg val]] (cons arg (cons val r))) [] m))

(defn live-sequencer
  ([curr-t sep-t live-patterns] (live-sequencer curr-t sep-t live-patterns 0))
  ([curr-t sep-t live-patterns beat]
   (doseq [[sound pattern] @live-patterns
           :let [v (nth pattern (mod beat (count pattern)))
                 v (cond
                     (= 1 v)
                     []

                     (map? v)
                     (flatten1 v)

                     :else
                     nil)]
           :when v]
     (at curr-t (apply sound v)))
   (let [new-t (+ curr-t sep-t)]
     (apply-by new-t #'live-sequencer [new-t sep-t live-patterns (inc beat)]))))


(def new-pats {kick [1 1 0 1 0 1 0 0]
               snare    [1 0 0 1 0 0 1 0]
               open-hat [1 0 0 0 0 0 0 1]})

(def new-live (atom new-pats))

(def a {:amp 0.5})
(def b {:amp 0.7})
(def c {:amp 0.1})

(comment
  (live-sequencer (+ 200 (now)) 200 new-live)

  (swap! new-live assoc kick [1 1 0 b 0 1 a c])
  (swap! new-live assoc snare [1 1 c c 1 a b c])
  (swap! new-live assoc open-hat [c c 1 0 0 0 a c])
  (stop))

(defn normalise-beat-info
  [beat]
  (cond
    (= 1 beat)         {}
    (map? beat)        beat
    (sequential? beat) beat
    :else              {}))

(defn schedule-pattern
  [curr-t pat-dur sound pattern]
  {:pre [(sequential? pattern)]}
  (let [beat-sep-t (/ pat-dur (count pattern))]
    (doseq [[beat-info idx] (partition 2 (interleave pattern (range)))]
      (let [beat-t    (+ curr-t (* idx beat-sep-t))
            beat-info (normalise-beat-info beat-info)]
        (if (sequential? beat-info)
          (schedule-pattern beat-t beat-sep-t sound beat-info)
          (at beat-t (apply sound (flatten1 beat-info))))))))

(defn live-sequencer
  [curr-t pat-dur live-patterns]
  (doseq [[sound pattern] @live-patterns]
    (schedule-pattern curr-t pat-dur sound pattern))
  (let [new-t (+ curr-t pat-dur)]
    (apply-by new-t #'live-sequencer [new-t pat-dur live-patterns])))

(comment
  (live-sequencer (now) 2000 new-live)
  (swap! new-live assoc kick [1 1 0 b 0 1 [1 1 1] [1 1 1 0 1 1 1]])
  (swap! new-live assoc snare [1 1 c c 1 a [1 a c 1] c])
  (swap! new-live assoc open-hat [c a 0 0 a c c c])
  (def wb (wobble-bass))
  (ctl wb :amp 0.5 :note 50 :wobble 2)
  (ctl wb :amp 0.5 :note 62 :wobble 1)
  (ctl wb :note 64)
  (ctl wb :wobble 1)
  (ctl wb :amp 0.7)
  (ctl wb :wob-hi 7000)
  (ctl wb :wob-lo 2000)
  (stop))



(defsynth wobble-bass [amp 1 note 52 wobble 1 detune 1.01 wob-lo 200 wob-hi 20000 pan 0]
  (let [saws          (mix (saw [note (* note detune)]))
        wob-freq      (lin-exp (lf-saw wobble) -1 1 wob-lo wob-hi)
        wob-freq      (lag wob-freq 0.05)
        filtered-saws (lpf saws wob-freq)
        normalized    (normalizer filtered-saws)
        amplified     (* amp normalized)]
    (out 0 (pan2 amplified pan))))

#_(show-graphviz-synth wobble-bass)

(comment (wobble-bass)
         (stop)
         (wobble-bass :amp 0.5 :note 30 :wob-hi 2000))



;; from

(def m (metronome 128))

(defn player
  [beat]
  (let [next-beat (inc beat)]
    (at (m beat)
        (quick-kick :amp 0.5)
        (if (zero? (mod beat 2))
          (open-hat :amp 0.1)))
    (at (m (+ 0.5 beat))
        (haziti-clap :decay 0.05 :amp 0.3))

    (when (zero? (mod beat 3))
      (at (m (+ 0.75 beat))
          (soft-hat :decay 0.03 :amp 0.2)))

    (when (zero? (mod beat 8))
      (at (m (+ 1.25 beat))
          (soft-hat :decay 0.03)))

    (apply-by (m next-beat) #'player [next-beat])))

;; from
;; https://github.com/overtone/overtone/blob/master/src/overtone/examples/getting_started/rhythm.clj

(player (m))
(stop)

;; from
;; https://github.com/overtone/overtone/blob/6ac57556014875357ef0c3355a11602069420a50/src/overtone/examples/compositions/auto_dubstep.clj

(demo 60
      (let [bpm     120
            ;; create pool of notes as seed for random base line sequence
            notes   [40 41 28 28 28 27 25 35 78]
            ;; create an impulse trigger firing once per bar
            trig    (impulse:kr (/ bpm 120))
            ;; create frequency generator for a randomly picked note
            freq    (midicps (lag (demand trig 0 (dxrand notes INF)) 0.25))
            ;; switch note durations
            swr     (demand trig 0 (dseq [1 6 6 2 1 2 4 8 3 3] INF))
            ;; create a sweep curve for filter below
            sweep   (lin-exp (lf-tri swr) -1 1 40 3000)
            ;; create a slightly detuned stereo sawtooth oscillator
            wob     (mix (saw (* freq [0.99 1.01])))
            ;; apply low pass filter using sweep curve to control cutoff freq
            wob     (lpf wob sweep)
            ;; normalize to 80% volume
            wob     (* 0.8 (normalizer wob))
            ;; apply band pass filter with resonance at 5kHz
            wob     (+ wob (bpf wob 1500 2))
            ;; mix in 20% reverb
            wob     (+ wob (* 0.2 (g-verb wob 9 0.7 0.7)))

            ;; create impulse generator from given drum pattern
            kickenv (decay (t2a (demand (impulse:kr (/ bpm 30)) 0 (dseq [1 0 0 0 0 0 1 0 1 0 0 1 0 0 0 0] INF))) 0.7)
            ;; use modulated sine wave oscillator
            kick    (* (* kickenv 7) (sin-osc (+ 40 (* kickenv kickenv kickenv 200))))
            ;; clip at max volume to create distortion
            kick    (clip2 kick 1)

            ;; snare is just using gated & over-amplified pink noise
            snare   (* 3 (pink-noise) (apply + (* (decay (impulse (/ bpm 240) 0.5) [0.4 2]) [1 0.05])))
            ;; send through band pass filter with peak @ 2kHz
            snare   (+ snare (bpf (* 4 snare) 2000))
            ;; also clip at max vol to distort
            snare   (clip2 snare 1)]
        ;; mixdown & clip
        (clip2 (+ wob kick snare) 1)))

(stop)



(demo 30
      (let [trig (coin-gate 0.5 (impulse:kr 2))
            note (demand trig 0 (dseq (shuffle (map midi->hz (conj (range 24 45) 22))) INF))
            sweep (lin-exp (lf-saw (demand trig 0 (drand [1 2 2 3 4 5 6 8 16] INF))) -1 1 40 5000)

            son (mix (lf-saw (* note [0.99 1 1.01])))
            son (lpf son sweep)
            son (normalizer son)
            son (+ son (bpf son 2000 2))

            ;;special flavours
            ;;hi manster
            son (select (< (t-rand:kr :trig trig) 0.05) [son (* 4 (hpf son 1000))])

            ;;sweep manster
            son (select (< (t-rand:kr :trig trig) 0.05) [son (* 4 (hpf son sweep))])

            ;;decimate
            son (select (< (t-rand:kr :trig trig) 0.05) [son (round son 0.1)])

            son (tanh (* son 5))
            son (+ son (* 0.3 (g-verb son 10 0.1 0.7)))
            son (* 0.3 son)]

        [son son]))





