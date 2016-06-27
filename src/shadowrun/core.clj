(ns shadowrun.core)

(def filter-hits (partial filter #(>= % 5)))

(defn roll-dice [n & {:keys [edge]}]
  (if edge
    (let [rolls (roll-dice (+ n edge))   ;Add edge-rating to dice pool
          sixes (filter #(= 6 %) rolls)] ;Rule of sixes, sixes get re-rolled
      (into rolls (filter-hits (roll-dice (count sixes))))) ;Don't let failures in, would affect glitch %
    ;Default, no edge
    (take n (repeatedly (comp inc #(rand-int 6))))))

(defn second-chance [rolls]
  (let [hit-dice (filter-hits rolls)]
    (into hit-dice (roll-dice (- (count rolls) (count hit-dice))))))

(defn hits 
  ([rolls limit]
    (min (or limit Integer/MAX_VALUE) (count (filter-hits rolls))))
  ([rolls] (hits rolls nil)))

(defn glitch? [rolls]
  (let [limit (int (Math/ceil (/ (count rolls) 2)))]
    (>= (count (filter #(= 1 %) rolls)) limit)))

(defn crit-glitch? [rolls]
  (and (glitch? rolls)
       (= 0 (hits rolls))))


(defn aid-another->hits 
  "Argument assist is per person, so 4 people would be (3 11 8 5)"
  [leader-skill leader-attribute limit assist & {:keys [edge]}]
  (let [ass-rolls (map roll-dice assist)
        new-limit (when limit
                    (if (some crit-glitch? ass-rolls)
                      limit
                      (+ limit (count (filter #(> (hits %) 0) ass-rolls)))))
        additional-dice (min leader-skill (reduce + (map hits (remove glitch? ass-rolls))))]
    (hits (roll-dice (+ leader-skill leader-attribute additional-dice) :edge edge) new-limit)))

(defn opposed-test
  [skill attribute opposed-dice & {:keys [limit edge assist]}]
  (let [self-hits (if assist (aid-another->hits skill attribute limit assist :edge edge) (hits (roll-dice (+ skill attribute) :edge edge) limit))
        opposed-hits (hits (roll-dice opposed-dice))]
    {:hits self-hits
     :opposed-hits opposed-hits
     :net-hits (- self-hits opposed-hits)}))

(defn compile-sprite [compiling resonance level & {:keys [edge assist]}]
  (let [test (opposed-test compiling resonance level :limit level :edge edge :assist assist)]
    {:fade (max 2 (* 2 (:opposed-hits test)))
     :tasks (:net-hits test)}))

(defn register-sprite [registering resonance level & {:keys [edge assist]}]
  (let [test (opposed-test registering resonance (+ level level) :limit level :edge edge :assist assist)]
    {:fade (max 2 (* 2 (:opposed-hits test)))
     :tasks (:net-hits test)}))

(defn resist-fade [willpower resonance fade-dv]
  (max 0 (- fade-dv (hits (roll-dice (+ willpower resonance))))))

(defn create-sprite
  [compiling registering resonance willpower level & {:keys [edge assist]}]
  (let [c-roll  (compile-sprite compiling resonance level :edge edge :assist assist)
        r-roll  (register-sprite registering resonance level :edge edge :assist assist)
        c-dmg   (resist-fade willpower resonance (:fade c-roll))
        r-dmg   (resist-fade willpower resonance (:fade r-roll))]
    (if (> (:tasks c-roll) 0)
      (if (> (:tasks r-roll) 0)
        {:tasks (+ (:tasks c-roll) (:tasks r-roll))
         :damage (+ c-dmg r-dmg)}
        {:tasks 0
         :damage (+ c-dmg r-dmg)})
      {:tasks 0
       :damage c-dmg})))

(defn sim-max-register-damage [n]
  (let [results   (take n (repeatedly #(resist-fade 6 6 (:fade (register-sprite 6 6 6)))))]
    (reduce max results)))



(defn simulate-sprite [f n]
  (let [results   (take n (repeatedly f))
        avg-dmg   (float (/ (reduce + (map :damage results)) n))
        avg-task  (float (/ (reduce + (map :task   results)) n))
        successes (* 100 (float (/ (count (filter pos? (map :tasks results))) n)))]
    (println (format "Successes: %.2f%% Failures: %.2f%%" successes (- 100 successes)))
    (println "Average Damage: " avg-dmg)
    (println "Average Tasks: " avg-task)
    results))

(defn simulate [f n]
  (let [results (take n (repeatedly f))
        outcomes (group-by identity results)
        percents (map (fn [[k v]] [k (* 100 (float (/ (count v) n)))]) outcomes)
        average (float (/ (reduce + results) n))
        successes (* 100 (float (/ (count (filter pos? results)) n)))]
    (println (format "Successes: %.2f%% Failures: %.2f%%" successes (- 100 successes)))
    (println "Average: " average)
    (sort-by first percents)))
