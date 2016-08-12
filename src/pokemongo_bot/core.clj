(ns pokemongo-bot.core
  (:gen-class)
  (:use
   [clojure.core.async :only [go chan <! <!! >! >!! close! timeout go-loop]]
   [pokemongo-bot.authenticate :only [IAccount]]
   [pokemongo-bot.coordinate   :only [ICoord]]
   [pokemongo-bot.pokemon      :only [IPokemonId]])
  (:require
   [pokemongo-bot.authenticate :as auth]
   [pokemongo-bot.coordinate   :as coord]
   [pokemongo-bot.pokemon      :as pokemon]
   [pokemongo-bot.api          :as api]))

(def evolves
  [10  ;; キャタピー
   13  ;; ビードル
   16  ;; ポッポ
   19  ;; コラッタ
   21  ;; オニスズメ
   23  ;; アーボ
   25  ;; ピカチュウ
   35  ;; ピッピ
   37  ;; ロコン
   39  ;; プリン
   41  ;; ズバット
   43  ;; ナゾノクサ
   46  ;; パラス
   48  ;; コンパン
   50  ;; ディグダ
   52  ;; ニャース
   56  ;; マンキー
   54  ;; コダック
   69  ;; マダツボミ
   72  ;; メノクラゲ
   74  ;; イシツブテ
   77  ;; ポニータ
   79  ;; ヤドン
   81  ;; コイル
   86  ;; パウワウ
   88  ;; ベトベター
   90  ;; シェルダー
   96  ;; スリープ
   98  ;; クラブ
   100 ;; ビリリダマ
   102 ;; タマタマ
   104 ;; カラカラ
   109 ;; ドガース
   116 ;; タッツー
   118 ;; トサキント
   138 ;; オムナイト
   140 ;; カブト
   ])

(defn- transfer-pokemon [pk]
  (try
    (.transferPokemon pk)
    (catch com.pokegoapi.exceptions.AsyncPokemonGoException e
      (println "ポケモンをアメに替えようとしたら同期エラー"))))

(defn- evolve-pokemon [pk]
  (try
    (.evolve pk)
    (catch com.pokegoapi.exceptions.AsyncPokemonGoException e
      (println "ポケモンを進化させようとしたら同期エラー"))))

(defn release-pokemon [pk]
  (if (and (some #(= % (:number (pokemon/pokemon-id pk))) evolves)
           (< 50 (-> (dosync @api/pokemon-inventory) .getCandyjar
                     (.getCandies (.getPokemonFamily pk)))))
    (do (Thread/sleep 5000)
        (evolve-pokemon pk)
        (Thread/sleep 5000)
        (transfer-pokemon pk))
    (do (Thread/sleep 5000)
        (transfer-pokemon pk))))

(defn release-pokemons [attack defense stamina]
  (if (dosync @api/pokemon-inventory)
    (->> (-> (dosync @api/pokemon-inventory) .getPokebank .getPokemons)
         (filter #(let [status (pokemon/status %)]
                    (if (< 1500 (:cp status))
                      false
                      (or (> attack  (:individual-attack  status))
                          (> defense (:individual-defense status))
                          (> stamina (:individual-stamina status))))))
         (map release-pokemon)
         doall)))

(defn search-pokestops []
  (if (dosync @api/pokemon-points)
    (-> (dosync @api/pokemon-points) .getPokestops)
    []))

(defn loot-pokestop [pokestop]
  (try
    (.loot pokestop)
    (catch com.pokegoapi.exceptions.AsyncPokemonGoException e
      (println "ポケストップから戦利品を取得しようとして同期エラー"))))

(defn search-spawnpoints []
  (if (dosync @api/pokemon-points)
    (-> (dosync @api/pokemon-points) .getSpawnPoints)
    []))

(defn distance-with-center [center current point]
  (+ (-> (coord/distance current point) (* 15))
     (-> (coord/distance center point))))

(defn next-point [center routes]
  (let [current (dosync @api/current-location)
        points (->> (search-pokestops)
                    (filter #(not (some (partial = %) routes)))
                    (filter #(= 0 (.getCooldownCompleteTimestampMs %)))
                    (sort-by #(distance-with-center center current %)))
        best (first points)]
    (if best
      (if (= current (coord/coord best))
        (second points)
        best))))

(defn walking [point cooltime]
  (let [current (dosync @api/current-location)]
    (->> (coord/jogging-route current point cooltime)
         (map #(do (Thread/sleep (* 1000 cooltime))
                   (api/location %)))
         doall)))

(defn point-pool [x coll]
  (cons x (take 4 coll)))

(defn walk-around [center]
  (loop [routes []]
    (release-pokemons 13 13 13)
    ;;(remove-items 1 101 102 103 201)
    (let [next (next-point center routes)]
      (if next
        (do (println (str "next -> " (coord/coord-format next)))
            (walking next 1)
            (loot-pokestop next)
            (recur (point-pool next routes)))
        (do
          (Thread/sleep 3000)
          (println (str "Not Found PokeStops."))
          (recur routes))))))
;;
;; TODO: retry
;;
(defn walking-dead [account center]
  (let [channel (api/start account center)]
    (walk-around center)))

(defn -main [& args]
  (let [email    (-> (System/console) (.readLine " Email: " nil) String/valueOf)
        password (-> (System/console) (.readPassword " Password: " nil) String/valueOf)
        latlong  (-> (System/console) (.readLine " Coord: " nil) String/valueOf)]
    (walking-dead
     (auth/->GoogleAccount email password)
     (apply coord/->Coord (map read-string (clojure.string/split latlong #","))))))

     ;;(coord/->Coord 35.6328964 139.8803943) ;; TDL
     ;; (coord/->Coord 35.65955 139.699068) ;; Shibuya 109
     ;; (coord/->Coord 35.6267108 139.8850779) ;; TDS



;; (defn item-id [item]
;;   (-> item bean :itemId))

;; (defn some-item-id [item ids]
;;   (let [num (-> item item-id bean :valueDescriptor bean :number)]
;;     (some #(= num %) ids)))

;; (defn remove-items [api & rest]
;;   (-> api :origin .getInventories (.updateInventories true))
;;   (let [bag   (-> api :origin .getInventories .getItemBag)
;;         items (-> bag bean :items)]
;;     (->> items
;;          (filter #(some-item-id % rest))
;;          (map #(-> bag (.removeItem (item-id %) (.getCount %))))
;;          doall)))
