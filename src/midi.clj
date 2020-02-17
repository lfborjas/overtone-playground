(ns overtone-playground.midi
  (:use [overtone.live]
        [overtone.synth.sts :only [prophet]]))

(comment
  (prophet :freq 110 :decay 5 :rq 0.6 :cutoff-freq 2000)

  ;; Make sure to connect the device _before_ launching CIDER
  ;; if successful, this call will return a non-empty list
  (midi-connected-devices)

  ;; We know 'Launchpad' is part of the identifier:
  (def launchpad (midi-find-connected-device "Launchpad"))
  ;; we can see what unique key will be assigned to it:
  (midi-mk-full-device-key launchpad)

  ;; receive events from Launchpad and translate midi notes
  ;; to freq:
  (on-event (conj (midi-mk-full-device-key launchpad)
                  :note-on)
            (fn [m]
              (let [note (:note m)]
                (prophet :freq (midi->hz note)
                         :decay 5
                         :rq 0.6
                         :cutoff-freq 1000)))
            ::prophet-midi)
  (remove-event-handler ::prophet-midi))

(defsynth pad1 [freq 110 amp 1 gate 1 out-bus 0]
  (out out-bus
       (* (saw [freq (* freq 1.01)])
          (env-gen (adsr 0.01 0.1 0.7 0.5) :gate gate :action FREE))))


;; Of course, we could also build a more sophisticated synth with the
;; same properties:

(defsynth pad2 [freq 440 amp 0.4 amt 0.3 gate 1.0 out-bus 0]
  (let [vel        (+ 0.5 (* 0.5 amp))
        env        (env-gen (adsr 0.01 0.1 0.7 0.5) gate 1 0 1 FREE)
        f-env      (env-gen (perc 1 3))
        src        (saw [freq (* freq 1.01)])
        signal     (rlpf (* 0.3 src)
                         (+ (* 0.6 freq) (* f-env 2 freq)) 0.2)
        k          (/ (* 2 amt) (- 1 amt))
        distort    (/ (* (+ 1 k) signal) (+ 1 (* k (abs signal))))
        gate       (pulse (* 2 (+ 1 (sin-osc:kr 0.05))))
        compressor (compander distort gate 0.01 1 0.5 0.01 0.01)
        dampener   (+ 1 (* 0.5 (sin-osc:kr 0.5)))
        reverb     (free-verb compressor 0.5 0.5 dampener)
        echo       (comb-n reverb 0.4 0.3 0.5)]
    (out out-bus
         (* vel env echo))))

;; From:

;; https://github.com/overtone/overtone/blob/master/src/overtone/examples/midi/basic.clj

(comment
  (def pad-s (pad2))
  (ctl pad-s :gate 0)

  (defonce memory (agent {}))

  ;; set the mapping mode to "drum rack"

  (midi-control receiver 0 1)

  ;; modifying the basic example to also turn lights on and off:
  (on-event (conj (midi-mk-full-device-key launchpad)
                  :note-on)
            (fn [m]
              (midi-note receiver
                         (:note m)
                         60 ;; full green
                         1000 ;; 1 second (1000ms)
                         )
              (send memory
                    (fn [mem]
                      (let [n (:note m)
                            s (pad2 :freq (midi->hz n))]
                        (assoc mem n s)))))
            ::play-note)
  (stop)
  
  (on-event (conj (midi-mk-full-device-key launchpad)
                  :note-off)
            (fn [m]
              (send memory
                    (fn [mem]
                      (let [n (:note m)]
                        (when-let [s (get mem n)]
                          (ctl s :gate 0))
                        (dissoc mem n)))))
            ::release-note)

  (remove-event-handler ::play-note)
  (remove-event-handler ::release-note)

  (stop))


;; From

;; https://github.com/overtone/overtone/blob/master/src/overtone/examples/midi/keyboard.clj

(definst poly-ding
  [note 60 amp 1 gate 1]
  (let [freq (midicps note)
        snd  (sin-osc freq)
        env  (env-gen (adsr 0.001 0.1 0.6 0.3) gate :action FREE)]
    (* amp env snd)))

(comment (def ding-player (midi-poly-player poly-ding))
         
         (midi-player-stop ding-player)

         ;; how to send MIDI messages:

         ;; https://github.com/overtone/overtone/wiki/MIDI

         (def receiver (first (midi-connected-receivers)))

         ;; turn a specific key on for 1000ms
         (midi-note receiver 68 11 1000)
         ;; turn all the lights on in medium brightness
         (midi-control receiver 0 125)
         ;; turn all the lights off:
         (midi-control receiver 0 0)
         
         (stop))


;; from:
;; https://github.com/overtone/overtone/wiki/MIDI#simple-midi-keyboard-control

(definst steel-drum [note 60 amp 0.8]
  (let [freq (midicps note)]
    (* amp
       (env-gen (perc 0.01 0.2) 1 1 0 1 :action FREE)
       (+ (sin-osc (/ freq 2))
          (rlpf (saw freq) (* 1.1 freq) 0.4)))))

(def player (midi-poly-player steel-drum))

(midi-player-stop player)



;; sequencer controlled by launchpad:

(comment
  )
