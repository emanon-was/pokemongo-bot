(ns pokemongo-bot.core
  (:gen-class)
  (:require
   [pokemongo-bot.authenticate :as auth]
   [pokemongo-bot.coordinate   :as coord]
   [pokemongo-bot.pokemon      :as pokemon]
   [pokemongo-bot.item         :as item]
   [pokemongo-bot.api          :as api]))

(def item-limits
  {1 300  ;; ITEM_POKE_BALL
   2 150  ;; ITEM_GREAT_BALL
   3 150  ;; ITEM_ULTRA_BALL
   101 0  ;; ITEM_POTION
   102 0  ;; ITEM_SUPER_POTION
   103 0  ;; ITEM_HYPER_POTION
   104 30 ;; ITEM_MAX_POTION
   201 15 ;; ITEM_REVIVE
   })

(defn remove-item-set [i]
  (let [item   (item/status i)
        number (-> item :item-id :number)
        limit  (item-limits number)]
    (if (and limit (> (item :count) limit))
      (list i (- (item :count) limit)))))

(defn remove-items []
  (->> (api/inventory-items)
       (map remove-item-set)
       (filter identity)
       (map #(apply api/remove-item %))
       doall))

(defn release-pokemon [pk]
  (if (and (some #(= % (:number (pokemon/pokemon-id pk))) pokemon/evolves)
           (< 100 (-> (api/inventories)
                      .getCandyjar
                      (.getCandies (.getPokemonFamily pk)))))
    (do (Thread/sleep 3000)
        (api/evolve-pokemon pk)
        (Thread/sleep 3000)
        (api/transfer-pokemon pk))
    (do (Thread/sleep 3000)
        (api/transfer-pokemon pk))))

(defn release-pokemons [attack defense stamina]
  (if (api/inventories)
    (->> (-> (api/inventories) .getPokebank .getPokemons)
         (filter #(let [status (pokemon/status %)]
                    (if (< 1500 (:cp status))
                      false
                      (or (> attack  (:individual-attack  status))
                          (> defense (:individual-defense status))
                          (> stamina (:individual-stamina status))))))
         (map release-pokemon)
         doall)))

(defn distance-with-center [center current spot]
  (+ (-> (coord/distance current spot) (* 15))
     (-> (coord/distance center spot))))

(defn next-spot [center routes]
  (let [current (api/current-location)
        spots (->> (api/search-pokestops)
                   (filter #(not (some (fn [_] (= _ (coord/coord %))) routes)))
                   (filter #(= 0 (.getCooldownCompleteTimestampMs %)))
                   (sort-by #(distance-with-center center current %)))
        best (first spots)]
    (if best
      (if (= current (coord/coord best))
        (second spots)
        best))))

(defn priority-loot []
  (->> (api/inventory-items)
       (map item/status)
       (some #(or (and (= 1  (-> % :item-id :number))
                       (> 80 (-> % :count)))))))

(defn play-game []
  (->> (api/catchable-pokemons)
       (map api/catch-pokemon)
       doall)
  (if (priority-loot)
    (->> (api/lootable-pokestops)
         (map api/loot-pokestop)
         doall)))

(defn walking [spot cooltime]
  (let [current (api/current-location)]
    (->> (coord/jogging-route current spot cooltime)
         (map #(do (Thread/sleep (* cooltime 1000))
                   (api/location %)
                   (play-game)))
         doall)))

(defn spot-pool [x coll]
  (cons (coord/coord x) (take 4 coll)))

(defn walk-around [center]
  (loop [routes []]
    (api/update-inventory)
    (release-pokemons 13 13 13)
    (remove-items)
    (let [next (next-spot center routes)]
      (if next
        (do (println (str "Next -> " (coord/coord-format next)))
            (println (str "Dist -> "
                          (coord/distance (api/current-location) next)
                          " Meter"))
            (walking next 1)
            (api/loot-pokestop next)
            (recur (spot-pool next routes)))
        (do
          (Thread/sleep 20000)
          (println (str "Not Found PokeStops."))
          (recur routes))))))

(defn walking-start [account center]
  (api/start account center)
  (walk-around center))

(defn -main [& args]
  (let [email    (-> (System/console) (.readLine " Gmail : " nil) String/valueOf)
        password (-> (System/console) (.readPassword " Password : " nil) String/valueOf)
        latlong  (do (println " - Shibuya109           : 35.65955,139.699068")
                     (println " - 東京ディズニーランド : 35.6328964,139.8803943")
                     (println " - 東京ディズニーシー   : 35.6267108 139.8850779")
                     (-> (System/console) (.readLine " Coord : " nil) String/valueOf))]
    (walking-start
     (auth/->GoogleAccount email password)
     (apply coord/->Coord (map read-string (clojure.string/split latlong #",")))
     )))

;; (coord/->Coord 35.6328964 139.8803943) ;; TDL
;; (coord/->Coord 35.65955 139.699068)    ;; Shibuya 109
;; (coord/->Coord 35.6267108 139.8850779) ;; TDS

;; {:number 1,   :name "ITEM_POKE_BALL"}
;; {:number 2,   :name "ITEM_GREAT_BALL"}
;; {:number 3,   :name "ITEM_ULTRA_BALL"}
;; {:number 101, :name "ITEM_POTION"}
;; {:number 102, :name "ITEM_SUPER_POTION"}
;; {:number 103, :name "ITEM_HYPER_POTION"}
;; {:number 104, :name "ITEM_MAX_POTION"}
;; {:number 201, :name "ITEM_REVIVE"}
;; {:number 301, :name "ITEM_LUCKY_EGG"}
;; {:number 401, :name "ITEM_INCENSE_ORDINARY"}
;; {:number 501, :name "ITEM_TROY_DISK"}
;; {:number 701, :name "ITEM_RAZZ_BERRY"}
;; {:number 901, :name "ITEM_INCUBATOR_BASIC_UNLIMITED"}
;; {:number 902, :name "ITEM_INCUBATOR_BASIC"}

