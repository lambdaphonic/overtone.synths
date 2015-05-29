(ns lambdaphonic.overtone.synths.blips
    (:use [overtone.live :refer :all]
          [lambdaphonic.constants]
          [mud.core :refer :all])
    (:require [mud.timing :as time]))

(def mul overtone.sc.ugen-collide/*)
(defsynth blips [amp 1 out-bus 0]
  (let [d (duty:kr [1/40] 0 (dseq:dr (map #(+ 96 %) (take 1000 pi)) INF))
        sigs (blip:ar (mul (midicps d) [1 2 4]) 4)
        sig  (sum sigs)
        fx   (g-verb:ar sig 200 8)]
    (out:ar out-bus (mul fx 0.01 amp))))

