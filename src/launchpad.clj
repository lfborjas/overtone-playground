(ns overtone-playground.launchpad
  (:use [overtone.live]
        [overtone.inst.drum]))

;; Make sure that the Launchpad is connected _before_ starting the JVM!

(def launchpad (midi-find-connected-device "Launchpad"))
(def launchpad-ctl (midi-find-connected-receiver "Launchpad"))

;; create a vector of 8 zeroes.

(defn empty-8-bar []
  (into [] (take 8 (repeat 0))))

;; let's start with the instruments quiet for 8 bars

(def pats {kick        (empty-8-bar)
           open-hat    (empty-8-bar)
           snare       (empty-8-bar)
           tom         (empty-8-bar)
           clap        (empty-8-bar)
           bing        (empty-8-bar)
           haziti-clap (empty-8-bar)
           noise-snare (empty-8-bar)})

(def live-pats (atom pats))


;; recursive sound function: given a time and an interval,
;; play the sound if the beat is active at that time.
(defn play-pattern [curr-t sep-t pattern sound]
  (at curr-t (when (= 1 (first pattern))
               (sound)))
  (let [new-t (+ curr-t sep-t)]
    (apply-by new-t #'play-pattern [new-t sep-t (rest pattern) sound])))

(comment
  (play-pattern (+ (now) 200) 200 [1 0 1 0 1 1 0 1] bing))

;; given patterns of instrument->beats, cycle over the patterns _as they currently are_
;; and play the instruments.
(defn live-sequencer
  ([curr-t sep-t live-patterns]
   (live-sequencer curr-t sep-t live-patterns 0))
  ([curr-t sep-t live-patterns beat]
   (doseq [[sound pattern] @live-patterns
           :when (= 1 (nth pattern (mod beat (count pattern))))]
     (at curr-t (sound)))
   (let [new-t (+ curr-t sep-t)]
     (apply-by new-t #'live-sequencer [new-t sep-t live-patterns (inc beat)]))))

;; Example midi event handler: log the note and velocity received
(defn debug-midi [{:keys [note velocity]}]
  (println "Note: " note ", Velocity" velocity ))

(comment
  ;; bind the `note-on` event to the unique key assigned to the launchpad
  (on-event (conj (midi-mk-full-device-key launchpad)
                  :note-on)
            debug-midi
            ::note-printer)
  (remove-event-handler ::note-printer))

(defn on-beat
  "Given a receiver device, and a map of instrument->pattern, turn a beat
  on or off given a MIDI note from the Launchpad, using the X-Y layout."
  [device performance {:keys [note]}]
  (let [instrument (cond
                     ;; values are the MIDI notes assigned by launchpad
                     ;; to the X-Y layout, in decimal (see manual)
                     (<= 0 note 7) kick
                     (<= 16 note 23) open-hat
                     (<= 32 note 39) snare
                     (<= 48 note 55) tom
                     (<= 64 note 71) clap
                     (<= 80 note 87) bing
                     (<= 96 note 103) haziti-clap
                     (<= 112 note 119) noise-snare)
        beat (mod note 8)
        current-pattern  (get @performance instrument)
        off? (zero? (nth current-pattern beat))
        new-pattern (if off?
                      (do
                        ;; send a note-on to Launchpad with a velocity of 56 (green)
                        (midi-note-on device note 56)
                        (assoc current-pattern beat 1))
                      (do
                        ;; send a note-on to the given note, with an "off" velocity
                        (midi-note-on device note 12)
                        (assoc current-pattern beat 0)))]
    (swap! performance assoc instrument new-pattern)))

(comment
  (on-event (conj (midi-mk-full-device-key launchpad)
                  :note-on)
            (partial on-beat launchpad-ctl live-pats)
            ::sequencer-ctl)
  (remove-event-handler ::sequencer-ctl))

(comment
  (live-sequencer (+ 200 (now))
                  200
                  live-pats)

  (stop)
  ;; set the performance back to the original state
  (reset! live-pats pats)
  ;; turn all the lights off:
  (midi-control launchpad-ctl 0 0))

