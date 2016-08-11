(ns pokemongo-bot.api
  (:use [clojure.core.async :only [go chan <! <!! >! >!! close! timeout go-loop]])
  (:require
   [pokemongo-bot.authenticate :as auth]
   [pokemongo-bot.coordinate   :as coord]
   [pokemongo-bot.pokemon      :as pokemon])
  (:import
   [okhttp3 OkHttpClient]
   [com.pokegoapi.api PokemonGo]))

(def pokemon-client    (ref nil))
(def current-location  (ref nil))
(def pokemon-map       (ref nil))
(def pokemon-points    (ref nil))
(def pokemon-inventory (ref nil))

(defmacro setq [sym val]
  `(dosync (ref-set ~sym ~val)))

(defn client [account]
  (let [http-client (OkHttpClient.)
        api-client  (PokemonGo. (auth/login account http-client) http-client)]
    (setq pokemon-client api-client)
    api-client))

(defn location [point]
  (let [latlong (coord/coord point)]
    (-> @pokemon-client (.setLocation (:latitude  latlong) (:longitude latlong) 1))
    (setq current-location latlong)
    latlong))

(defn update-map []
  (try
    (let [current-map (.getMap @pokemon-client)]
      (setq pokemon-map current-map)
      (setq pokemon-points (.getMapObjects current-map)))
    (catch com.pokegoapi.exceptions.AsyncPokemonGoException pge
      (println "マップ情報を更新する際に同期エラー"))))

(defn update-inventory []
  (try
    (let [inventory (or @pokemon-inventory (.getInventories @pokemon-client))]
      (.updateInventories inventory)
      (setq pokemon-inventory inventory))
    (catch com.pokegoapi.exceptions.AsyncPokemonGoException pge
      (println "インベントリを更新する際に同期エラー"))))

(defn catch-pokemon [cpk]
  (println (pokemon/pokemon-id cpk))
  (.encounterPokemon cpk)
  (Thread/sleep 1000)
  (try
    (.catchPokemonWithBestBall cpk)
    (catch java.lang.NullPointerException e
      (println "ポケモンを捕まえようとしたらぬるぽ"))))

(defn start [account point]
  ;; update map
  (client account)
  (location point)
  (update-map)
  (update-inventory)
  (go-loop []
    (<! (timeout 12000))
    (update-map)
    (recur))
  (go-loop []
    (<! (timeout 90000))
    (update-inventory)
    (recur))
  (go-loop []
    (<! (timeout 1000))
    (->> @pokemon-map
         .getCatchablePokemon
         (map catch-pokemon)
         doall)
    (recur)))

