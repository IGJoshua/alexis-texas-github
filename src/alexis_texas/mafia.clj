(ns alexis-texas.mafia
  "Contains all the business logic and data definitions to run a game of mafia."
  (:require
   [clojure.spec.alpha :as s]
   [clojure.set :as set]))

(def ^:private test-players {"1234567890" {:alive? true
                                           :role :mafia}
                             "0987654321" {:alive? true
                                           :role :villager}
                             "6789054321" {:alive? true
                                           :role :healer}
                             "5432167890" {:alive? true
                                           :role :investigator}
                             "1029384756" {:alive? false
                                           :role :villager}})
(def ^:private test-night [{:type :kill
                            :target "0987654321"}
                           {:type :kill
                            :target "5432167890"}
                           {:type :save
                            :target "0987654321"}])
(def ^:private test-aftermath {"1234567890" {:alive? true
                                             :role :mafia}
                               "0987654321" {:alive? true
                                             :role :villager}
                               "6789054321" {:alive? true
                                             :role :healer}
                               "5432167890" {:alive? false
                                             :role :investigator}
                               "1029384756" {:alive? false
                                             :role :villager}})

(s/def ::alive? boolean?)
(s/def ::role keyword?)
(s/def ::player (s/keys :req-un [::alive? ::role]))

(s/def ::user-id string?)

(s/def ::game-state (s/map-of ::user-id ::player))

(s/def ::type keyword?)
(s/def ::event (s/keys :req-un [::type]))

(defn update-player
  [players player k f]
  (update-in players [player k] f))
(s/fdef update-player
  :args (s/cat :players ::game-state
               :player ::user-id
               :key keyword?
               :function fn?)
  :ret ::game-state)

(defn kill-player
  [players player]
  (update-player players player :alive? (constantly false)))
(s/fdef kill-player
  :args (s/cat :players ::game-state
               :player ::user-id)
  :ret ::game-state)

(defn kill-players
  [players to-kill]
  (reduce kill-player players to-kill))
(s/fdef kill-players
  :args (s/cat :players ::game-state
               :to-kill (s/coll-of ::user-id))
  :ret ::game-state)

(defn filter-events
  [event-type]
  (comp (filter (comp (partial = event-type) :type))
        (distinct)))
(s/fdef filter-events
  :args (s/cat :event-type ::type)
  :ret fn?)

(defmulti process-event
  ""
  (fn [update-map event]
    (:type event)))

(s/def ::attacked (s/coll-of ::user-id))
(s/def ::saved (s/coll-of ::user-id))

(s/fdef process-event
  :args (s/cat :update-map (s/keys :opt-un [::attacked ::saved])))

(defmethod process-event :kill
  [update-map event]
  (update update-map :attacked #(conj (or %1 #{}) %2) (:target event)))

(defmethod process-event :save
  [update-map event]
  (update update-map :saved #(conj (or %1 #{}) %2) (:target event)))

(defn process-events
  [events]
  (reduce process-event {} events))
(s/fdef process-events
  :args (s/cat :events (s/coll-of ::event)))

(defn killed-players
  [events]
  (let [updated-map (process-events events)]
    (set/difference (:attacked updated-map)
                    (:saved updated-map))))
(s/fdef killed-players
  :args (s/cat :events (s/coll-of ::event))
  :ret (s/coll-of ::user-id))

(defn resolve-night
  [players events]
  (kill-players players (killed-players events)))
(s/fdef resolve-night
  :args (s/cat :players ::game-state
               :events (s/coll-of ::event))
  :ret ::game-state)
