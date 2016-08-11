(defproject pokemongo-bot "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.385"]
                 [org.jscience/jscience "4.3.1"]
                 [com.pokegoapi/PokeGOAPI-library "0.4.0"]]
  :repositories {"jcenter" "http://jcenter.bintray.com"}
  :main pokemongo-bot.core
)
