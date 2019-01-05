(ns alexis-texas.mafia.state
  (:use
   com.rpl.specter)
  (:require
   [clojure.spec.alpha :as s]))

(defn active-game?
  [state guild-id]
  (select-first [ATOM :state guild-id :game-active?] state))
