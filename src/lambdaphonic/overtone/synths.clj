(ns lambdaphonic.overtone.synths
    (:use [overtone.live :refer :all]
          [mud.core :refer :all])
    (:require [mud.timing :as time]))

;; SynthDef taken from Tutorial 6 by Eli Fieldsteel
;; https://youtu.be/bMGXYEg1gJo
;; SynthDef.new(\iter, {
;;     arg freq=40
;;     var temp, sum, env
;;     sum = 0;
;;     env = EnvGen.kr(
;;         Env.perc(0.01, 5 1 -2
;;         doneAction:2
;;     );
;;     10.do {
;;         temp = VarSaw.ar(
;;             frequ * {Rand(0.99,1.02)}!2,
;;             {Rand(0.0, 1.0)}!2,
;;             {ExpRand(0.005, 0.05)}!2
;;         );
;;         sum = sum + temp;
;;     );
;;     sum = sum * 0.05 * env
;;     Out.ar(0, sum);
;;}).add;
(defsynth darkbass [freq 110 attack 1 release 4 out-bus 0]
  (let [f (/ freq 2.009924)
        env (env-gen (perc attack release 1 -2) :action FREE)
        sig (reduce (fn [left right] (+ left right))
                    (map (fn [x] (var-saw:ar (repeat 2 (* f (ranged-rand 0.99 1.02)))
                                             (repeat 2 (ranged-rand 0.0 1.0))
                                             (repeat 2 (exp-rand 0.005 0.05)))) (range 0 10)))]
    (out:ar out-bus (* env 0.05 sig))))

;; SynthDef taken from SuperCollider Tutorial 11 by Eli FieldSteel
;; https://youtu.be/ZVTbRNu2BI0
;; (
;; SynthDef.new(\tone, {
;;   arg freq=40, nharm=12, detune=0.2, gate=0, pan=0, amp=1, out=0;
;;   var sig, env;
;;   env = EnvGen.kr(Env.adsr(0.05, 0.1, 0.5, 3), gate);
;;   sig = Blip.ar(
;;     freq *
;;     LFNoise1.kr(0.2!16).bipolar(detune.neg, detune).midiratio,
;;     nharm
;;   );
;;   sig = sig * LFNoise1.kr(0.5!16).exprange(0.1, 1);
;;   sig = Splay.ar(sig);
;;   sig = Balance2.ar(sig[0], sig[1], pan);
;;   sig = sig * env * amp;
;;   Out.ar(out, sig);
;; }).add
;; )
;;
;; x = Synth.new(\tone, [\gate, 1]);
;; x.set(\gate, 1);
;; x.set(\nharm, 12);
;; x.set(\detune, 0.2);
;; x.set(\pan, -0);
;; x.set(\amp, 1);
;; x.set(\gate, 0);

(defsynth tone [freq 40 nharm 12 detune 0.2 gate 0 pan 0 amp 1 out-bus 0]
  (let [env (env-gen (adsr 0.05 0.1 0.5 3) gate)
        sig (blip:ar (* freq (midiratio (* detune (lf-noise1 (repeat 16 0.2))))) nharm)
        sig (* sig (lin-exp (lf-noise1 (repeat 16 0.5)) 0.1 1 0.1 1))
        sig (splay:ar sig)
        sig (balance2:ar (nth sig 0) (nth sig 1) pan)
        sig (* sig env 20 amp)]
    (out:ar out-bus sig)))

(defsynth short-tone [freq 40 nharm 12 detune 0.2 dur 3 pan 0 amp 1 out-bus 0]
  (let [env (env-gen (adsr 0.05 0.1 0.5 3) (line:kr 1 0 dur) :action FREE)
        sig (blip:ar (* freq (midiratio (* detune (lf-noise1 (repeat 16 0.2))))) nharm)
        sig (* sig (lin-exp (lf-noise1 (repeat 16 0.5)) 0.1 1 0.1 1))
        sig (splay:ar sig)
        sig (balance2:ar (nth sig 0) (nth sig 1) pan)
        sig (* sig env amp 20)]
    (out:ar out-bus sig)))

(comment
  (kill x)
  (ctl x :gate 1)
  (ctl x :amp 20)

  (do
    (use 'launchkey-mini.core)
    (boot-launchkey-mini!)
    (def x (tone))
    (bind :0x0 #(ctl x :gate 1))
    (bind :0x1 #(ctl x :gate 0))
    (bind :knob1 #(ctl x :amp (* 20 %)))
    (bind :knob2 #(ctl x :nharm (+ 8 (* 100 %))))
    (bind :knob3 #(ctl x :detune (* 10 %)))
    (bind :knob4 #(ctl x :pan (scale-range % 0 1 -1 1)))
    (bind :knob7 #(volume (* 50 %)))
    (defn n [noteid] (ctl x :freq (/ (midi->hz (note noteid)) 2.75)))
  )
)

;; Small changed example by alextiberiuskirk at http://sccode.org/1-4UT#c389
;; SynthDef(\Synth3,
;;   {arg freq = 440, ress = 4;
;;     var klank, env;
;;     klank = Klank.ar(`[{freq}!7, {Rand(0.128,0.700)}!7],BrownNoise.ar(0.5));
;;     klank = klank;
;;     env = EnvGen.kr(Env.perc(0.07, ress), doneAction:2);
;;     Out.ar(0, klank*env.dup*0.0128);
;; }).add;
;; )
(defsynth bellpad [freq 440 dur 4 amp 0.005 out-bus 0]
  (let [env (env-gen (perc 0.07 dur) :action FREE)
        sig (klank:ar [(repeat 7 freq) (repeat 7 (n-rand 0.128 0.7 1))] (* (brown-noise:ar) 0.5))]
    (out:ar out-bus (* sig (repeat 2 env) amp))))


;; SynthDef(\sinWide, { |out, amp=0.1, freq=440, sustain=0.01, pan, width=0.5|
;;  var holdT = sustain * width;
;;  var fadeT = 1 - width * sustain * 0.5;
;;  var snd = FSinOsc.ar(freq);
;;  var env = EnvGen.ar(Env([0, 1, 1, 0], [fadeT, holdT, fadeT], \sin),
;;                                 levelScale: amp * AmpComp.ir(freq) * 0.5,
;;                                      doneAction: 2);
;;  OffsetOut.ar(out, Pan2.ar(snd * env, pan));
;;}, \ir ! 5).add;
(defsynth sin-blip [freq 6000 sustain 0.01 pan 0 width 0.5 amp 0.1 out-bus 0]
  (let [hold-time (* sustain width)
        fade-time (* (- 1 width) sustain 0.5)
        snd (f-sin-osc:ar freq)
        env (env-gen:ar (envelope [0 1 1 0] [fade-time hold-time fade-time] :sine) :level-scale (* amp (amp-comp freq) 0.5) :action FREE)]
    (offset-out:ar out-bus (pan2 (* snd env) pan))))

(definst overpad [note 60 amp 0.7 attack 0.001 release 2]
  (let [freq  (midicps note)
        env   (env-gen (perc attack release) :action FREE)
        f-env (+ freq (* 3 freq (env-gen (perc 0.012 (- release 0.1)))))
        bfreq (/ freq 2)
        sig   (apply +
                    (concat (* 0.7 (sin-osc [bfreq (* 0.99 bfreq)]))
                    (lpf (saw [freq (* freq 1.01)]) f-env)))]
          (* amp env sig)))

(definst dark-kick []
  (let [src (sin-osc 80)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* 0.7 src env)))

