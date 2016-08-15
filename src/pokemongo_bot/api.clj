(ns pokemongo-bot.api
  (:use [clojure.core.async :only [go go-loop chan timeout <! <!! >! >!! close!]])
  (:require
   [pokemongo-bot.authenticate :as auth]
   [pokemongo-bot.coordinate   :as coord]
   [pokemongo-bot.pokemon      :as pokemon]
   [pokemongo-bot.item         :as item])
  (:import
   [okhttp3 OkHttpClient]
   [com.pokegoapi.api PokemonGo]))

(def pokemon-client (ref nil))

(defn client [account]
  (let [http-client (OkHttpClient.)
        api-client  (PokemonGo. (auth/login account http-client) http-client)]
    (dosync (ref-set pokemon-client api-client))
    api-client))

(defn location [point]
  (let [latlong (coord/coord point)]
    (-> @pokemon-client (.setLocation (:latitude  latlong) (:longitude latlong) 1))
    latlong))

(defn current-location []
  (coord/coord @pokemon-client))

(defn inventories []
  (.getInventories @pokemon-client))

(defn inventory-items []
  (try (->> (inventories) .getItemBag .getItems)
       (catch com.pokegoapi.exceptions.AsyncPokemonGoException pge
         (println "アイテムバッグを参照する際の同期処理エラー"))))

(defn remove-item [item count]
  (try (-> (inventories) .getItemBag (.removeItem (.getItemId item) count))
       (catch com.pokegoapi.exceptions.AsyncPokemonGoException pge
         (println "アイテムを捨てる際の同期処理エラー"))))

(defn update-inventory []
  (try
    (-> (inventories) .updateInventories)
    (catch com.pokegoapi.exceptions.AsyncPokemonGoException pge
      (println "インベントリを更新する際の同期処理エラー"))
    (catch java.util.ConcurrentModificationException cme
      (println "インベントリを更新する際に同期処理をしようとしてエラー"))))

(defn transfer-pokemon [pk]
  (try
    (.transferPokemon pk)
    (catch com.pokegoapi.exceptions.AsyncPokemonGoException e
      (println "ポケモンをアメに替えようとしたら同期処理エラー"))))

(defn evolve-pokemon [pk]
  (try
    (.evolve pk)
    (catch com.pokegoapi.exceptions.AsyncPokemonGoException e
      (println "ポケモンを進化させようとしたら同期処理エラー"))))


(defn pokeballs []
  (->> (inventory-items)
       (map item/status)
       (filter #(some (fn [_] (= _ (-> % :item-id :number))) [1 2 3 4]))))

(defn bestball []
  (let [balls (pokeballs)]
    (cond (->> balls (some #(and (= 2  (-> % :item-id :number)) (< 20 (-> % :count)))))
          com.pokegoapi.api.inventory.Pokeball/GREATBALL
          (->> balls (some #(and (= 3  (-> % :item-id :number)) (< 20 (-> % :count)))))
          com.pokegoapi.api.inventory.Pokeball/ULTRABALL)))

(defn catch-pokemon [cpk]
  (let [pokemon-name (-> cpk pokemon/pokemon-id :jname)]
    (print (str pokemon-name "を発見した！"))
    (Thread/sleep 5000)
    (let [encount (try (if (.encounterPokemon cpk) true false)
                     (catch com.pokegoapi.exceptions.AsyncPokemonGoException pge
                       (println "ポケモンとエンカウントしようとしたら同期処理エラー")))]
    (Thread/sleep 10000)
    (try (let [select-ball (bestball)
               result (if select-ball
                        (.catchPokemon cpk select-ball)
                        (.catchPokemon cpk))]
           (println (str pokemon-name (pokemon/result-format result))))
         (catch java.lang.NullPointerException e
           (println "ポケモンを捕まえようとしたら結果が空でパース出来ずエラー"))
         (catch com.pokegoapi.exceptions.AsyncPokemonGoException pge
           (println "ポケモンを捕まえようとしたら同期処理エラー"))))))

(defn pokemon-map []
  (-> @pokemon-client .getMap))

(defn pokemon-spots []
  (try (-> (pokemon-map) .getMapObjects)
       (catch com.pokegoapi.exceptions.AsyncPokemonGoException pge
         (println "マップ情報を取得しようとして同期処理エラー"))))

(defn catchable-pokemons []
  (try (-> (pokemon-map) .getCatchablePokemon)
       (catch com.pokegoapi.exceptions.AsyncPokemonGoException pge
         (println "捕獲可能なポケモンを取得しようとして同期処理エラー"))))

(defn search-pokestops []
  (try (or (-> (pokemon-spots) .getPokestops) [])
       (catch java.lang.NullPointerException e [])))

(defn lootable-pokestops []
  (->> (search-pokestops)
       (filter #(.canLoot %))))

(defn loot-pokestop [pokestop]
  (try
    (.loot pokestop)
    (catch com.pokegoapi.exceptions.AsyncPokemonGoException e
      (println "ポケストップから戦利品を取得しようとして同期処理エラー"))))


(defn start [account coord]
  (client account)
  (location coord)
  (go-loop []
    (<! (timeout (* 1800 1000)))
    (let [current (current-location)]
      (client account)
      (location current))
    (recur)))
