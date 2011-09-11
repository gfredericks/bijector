(defproject bijector "0.0.1"
  :description "Bijections in Clojure"
  :dependencies [[org.clojure/clojure "1.2.1"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [compojure "0.6.5"]],
  :dev-dependencies [[lein-ring "0.4.5"]]
  :main bijector.core
  :ring {:handler bijector.server/app})
