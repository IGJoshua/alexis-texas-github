(ns alexis-texas.events
  (:use
   com.rpl.specter)
  (:require
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [discljord.messaging :as m]))

(defonce state (atom nil))

(def ^:private roles-xf (map (fn [role]
                               [(:id role)
                                (dissoc role :id)])))
(def ^:private users-xf (map (fn [member]
                               [(:id (:user member))
                                (dissoc (:user member) :id)])))
(def ^:private members-xf (map (fn [member]
                                 [(:id (:user member))
                                  (dissoc member :user)])))

(defn update-guild
  [{:keys [id roles members] :as guild}]
  (transform [ATOM :guilds (keypath id)]
             #(merge % (dissoc guild :id :roles :members))
             state)
  (let [roles (into {} roles-xf roles)
        users (into {} users-xf members)
        members (into {} members-xf members)]
    (doseq [[role-id role] roles]
      (setval [ATOM :roles (keypath id) (keypath role-id)] role state))
    (doseq [[user-id user] users]
      (when-not (select-any [ATOM :users (keypath user-id)] state)
        (setval [ATOM :users (keypath user-id)] user state)))
    (doseq [[user-id member] members]
      (setval [ATOM :users (keypath user-id) :guilds (keypath id)] member state))))

(defn remove-guild
  [{:keys [id unavailable] :as event}]
  (when-not unavailable
    (setval [ATOM :guilds (keypath id)] NONE state)
    (setval [ATOM :users ALL :guilds (keypath id)] NONE state)
    (setval [ATOM :roles (keypath id)] NONE state)))

(defn add-member-info
  [{:keys [guild-id] {:keys [id bot username] :as user} :user :as event}]
  (when-not bot
    (if-let  [blacklisted (some #(if (instance? java.util.regex.Pattern %)
                                   (re-find % username)
                                   (when (str/includes? (str/lower-case username)
                                                        (str/lower-case %))
                                     %))
                                (select [ATOM :guilds guild-id :blacklist ALL] state))]
      ;; if blacklisted, ban them
      (m/create-guild-ban! (:messaging @state) guild-id id
                           :reason (format
                                    (str "Alexis Texas auto-ban: had blacklisted pattern %s,"
                                         " in username \"%s\"")
                                    (prn-str blacklisted)
                                    username))
      ;; otherwise, add them to the members
      (let [member (dissoc event :user :guild-id)]
        (when-not (select-any [ATOM :users (keypath id)] state)
          (setval [ATOM :users (keypath id)] (dissoc user :id) state))
        (setval [ATOM :users (keypath id) :guilds (keypath guild-id)] member state)))))

(defn add-guild-members
  [{:keys [guild-id members] :as event}]
  (let [members (into {}
                      members-xf
                      members)]
    (doseq [[user-id member] members]
      (setval [ATOM :users (keypath user-id) :guilds (keypath guild-id)] member state))))

(defn update-guild-member
  [{:keys [guild-id roles nick] {:keys [id]} :user :as event}]
  (setval [ATOM :users (keypath id) :guilds (keypath guild-id) :roles] roles state)
  (setval [ATOM :users (keypath id) :guilds (keypath guild-id) :nick] nick state))

(defn remove-guild-member
  [{:keys [guild-id] {:keys [id]} :user}]
  (setval [ATOM :users (keypath id) :guilds (keypath guild-id)] NONE state))

(defn add-role
  [{:keys [guild-id role]}]
  (setval [ATOM :roles (keypath guild-id) (keypath (:id role))] (dissoc role :id) state))

(defn update-role
  [{:keys [guild-id role]}]
  (transform [ATOM :roles (keypath guild-id) (keypath (:id role))]
             #(merge % (dissoc role :id))
             state))

(defn delete-role
  [{:keys [guild-id role-id]}]
  (setval [ATOM :roles (keypath guild-id) (keypath role-id)] NONE state)
  (transform [ATOM :users ALL :guilds (keypath guild-id) :roles]
             (partial remove #(= % role-id))
             state))

(defn update-user
  [{:keys [id] :as user}]
  (transform [ATOM :users (keypath id)] #(merge % (dissoc user :id)) state))

(defn disconnect-bot
  [event-data]
  (log/fatal "Disconnecting from Discord.")
  (m/stop-connection! (:messaging @state))
  (swap! state assoc :running false)
  (spit "quotes.edn" (pr-str (:state @state))))
