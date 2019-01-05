(ns alexis-texas.core
  (:require
   [alexis-texas.commands :refer [process-message]]
   [alexis-texas.events :refer [add-guild-members add-member-info add-role delete-role disconnect-bot
                                remove-guild remove-guild-member update-guild update-guild-member
                                update-role update-user state]]
   [alexis-texas.util :refer [resource]]
   [clojure.core.async :as a]
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]
   [discljord.connections :as c]
   [discljord.events :as e]
   [discljord.messaging :as m])
  (:import
   (java.io FileNotFoundException))
  (:gen-class))


(def token (resource "token.txt"))

(defonce ^:dynamic *events* (atom nil))
(defonce ^:dynamic *connection* (atom nil))
(defonce ^:dynamic *messaging* (atom nil))

(defn handler
  [& {:as handlers}]
  (fn [event-type event-data]
    (doseq [f (event-type handlers)]
      (f event-data))))

(def handle-event
  (handler
   :guild-create [#'update-guild]
   :guild-update [#'update-guild]
   :guild-remove [#'remove-guild]
   :guild-member-add [#'add-member-info]
   :guild-members-chunk [#'add-guild-members]
   :guild-member-update [#'update-guild-member]
   :guild-member-remove [#'remove-guild-member]
   :guild-role-create [#'add-role]
   :guild-role-update [#'update-role]
   :guild-role-delete [#'delete-role]
   :user-update [#'update-user]
   :message-create [#'process-message]
   :disconnect [#'disconnect-bot]))

(defn run-bot
  []
  (let [init-state (or (try (edn/read-string (slurp "quotes.edn"))
                            (catch FileNotFoundException e
                              (log/info e "No quotes file exists, starting with empty map.")
                              nil))
                       {})
        events (reset! *events* (a/chan 100))
        connection (reset! *connection* (c/connect-bot! token events
                                                        :buffer-size 500000))
        messaging (reset! *messaging* (m/start-connection! token))]
    (reset! state {:connection connection
                   :events events
                   :messaging messaging
                   :state init-state
                   :roles {}
                   :users {}
                   :guilds {}
                   :running true})
    (a/go-loop []
      (a/<! (a/timeout 300000))
      (spit "quotes.edn" (pr-str (:state @state)))
      (when (:running @state)
        (recur)))
    (e/message-pump! events #'handle-event)))

(defn -main
  "Starts the alexis-texas bot."
  []
  (run-bot)
  (shutdown-agents))

(defn start-bot!
  []
  (a/thread (run-bot)))

(defn stop-bot!
  []
  (when @*connection*
    (c/disconnect-bot! @*connection*))
  (reset! *events* nil)
  (reset! *connection* nil)
  (reset! *messaging* nil))
