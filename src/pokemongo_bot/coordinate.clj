(ns pokemongo-bot.coordinate
  (:import
   [javax.measure.unit SI NonSI]
   [org.jscience.geography.coordinates.crs ReferenceEllipsoid]
   [com.pokegoapi.api PokemonGo]
   [POGOProtos.Map SpawnPointOuterClass$SpawnPoint]
   [com.pokegoapi.api.map.fort Pokestop]))


(defrecord Coord [latitude longitude])

(defrecord UTM [easting northing])

(defn utm [latlong]
  (let [jutm
        (org.jscience.geography.coordinates.UTM/latLongToUtm
         (org.jscience.geography.coordinates.LatLong/valueOf
          (:latitude  latlong)
          (:longitude latlong)
          NonSI/DEGREE_ANGLE)
         ReferenceEllipsoid/WGS84)
        position (.getPosition jutm)]
    (UTM. (-> position (.eastingValue SI/METER))
          (-> position (.northingValue SI/METER)))))


(defprotocol ICoord
  (coord [p])
  (distance [p1 p2])
  (coord-format [p]))

(extend-protocol ICoord
  Coord
  (coord [c] c)
  (distance [c1 c2]
    (let [u1 (utm c1) u2 (utm (coord c2))]
      (Math/sqrt
       (+ (-> (- (:easting  u1) (:easting  u2)) (Math/pow 2))
          (-> (- (:northing u1) (:northing u2)) (Math/pow 2))))))
  (coord-format [c] (str (:latitude c) "," (:longitude c)))

  PokemonGo
  (coord [pg] (coord (Coord. (.getLatitude pg) (.getLongitude pg))))
  (distance [pg p1] (distance (coord pg) (coord p1)))
  (coord-format [pg] (str "Current: " (coord-format (coord pg))))

  SpawnPointOuterClass$SpawnPoint
  (coord [sp] (coord (Coord. (.getLatitude sp) (.getLongitude sp))))
  (distance [sp p1] (distance (coord sp) (coord p1)))
  (coord-format [sp] (str "SpawnPoint: " (coord-format (coord sp))))

  Pokestop
  (coord [pks] (coord (Coord. (-> pks .getFortData .getLatitude)
                              (-> pks .getFortData .getLongitude))))
  (distance [pks p1] (distance (coord pks) (coord p1)))
  (coord-format [sp] (str "PokeStop: " (coord-format (coord sp)))))

(defn route [start end km-h cooltime]
  (let [m-s (-> km-h (* 1000) (/ 3600))
        start (coord start)
        end   (coord end)
        times (Math/ceil (/ (distance start end) (* m-s cooltime)))
        move-lat  (-> (:latitude  end) (- (:latitude  start)) (/ times))
        move-long (-> (:longitude end) (- (:longitude start)) (/ times))]
    (conj
     (vec (map #(Coord. %1 %2)
               (range (:latitude  start) (:latitude  end) move-lat)
               (range (:longitude start) (:longitude end) move-long)))
     end)))

(defn walking-route [start end cooltime]
  (route start end 5 cooltime))

(defn jogging-route [start end cooltime]
  (route start end 10 cooltime))


