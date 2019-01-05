(ns alexis-texas.util
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn resource
  [path]
  (-> path
      io/resource
      slurp
      str/trim))
