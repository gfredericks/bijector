(ns bijector.server
  (:use compojure.core)
  (:require [compojure.route :as route]
            [compojure.handler :as handler]))

(defroutes tommy-routes
  (route/resources "/"))

(def app (handler/site tommy-routes))
