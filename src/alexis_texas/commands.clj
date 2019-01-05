(ns alexis-texas.commands
  (:use
   com.rpl.specter)
  (:require
   [alexis-texas.events :refer [state]]
   [alexis-texas.macros :refer [commands]]
   [alexis-texas.mafia :as mafia]
   [alexis-texas.mafia.state :as mafia.s]
   [alexis-texas.permissions :refer [user-has-permission?]]
   [alexis-texas.util :refer [resource]]
   [clojure.string :as str]
   [clojure.tools.logging :as log]
   [discljord.connections :as c]
   [discljord.messaging :as m])
  (:import
   (java.util.regex Pattern)))

(defn display-help-message
  "Takes a core.async channel for communicating with the messaging process,
  the channel ID on which to send a help message, and the current guild's prefix,
  and sends a help message to Discord."
  [messaging channel-id prefix admin?]
  (m/send-message! messaging channel-id
                   (str "Hi! I'm Alexis Texas, a Discord Bot meant to help you keep"
                        " track of funny and interesting quotes in your server.\n\n"

                        "I only have a couple of commands to remember.\n"
                        "`" prefix "help` displays this message!\n"
                        "`" prefix "quote` will get a random quote from your server's list.\n"
                        "`" prefix "quote <name>` will get a random quote said by the"
                        " named user.\n"
                        "`" prefix "quote add <name> <quote>` will add a new quote"
                        " to your server.\n"
                        "`" prefix "quote remove <name> <quote>` removes the given quote."
                        " Later this will be done by an ID instead of the copied and"
                        " pasted quote.\n\n"

                        "Recently I got to add playing the game Mafia to my features. If"
                        " you'd like to see how to play, use this command:\n"
                        "`" prefix "mafia help`\n\n"
                        (when admin?
                          (str "Since you are an admin on the server, feel free to make"
                               " use of these additional commands:\n"
                               "`" prefix "prefix <new prefix>` changes the bot's prefix for this server.\n"
                               "`" prefix "blacklist add <name>` bans members joining the server with this in"
                               " their name.\n"
                               "`" prefix "blacklist add regex <regex>` bans members joining the server with "
                               "names that match the regular expression.\n"
                               "`" prefix "blacklist remove <listed number>` removes the blacklist name or "
                               "pattern based on its index as shown by the list command.\n"
                               "`" prefix "blacklist list` lists all the names and patterns"
                               " blacklisted by this bot.\n"
                               "`" prefix "blacklist clear` clears the blacklist of both names and"
                               " patterns.\n")))))

(def owner (resource "owner.txt"))
(def bot-id (resource "bot.txt"))

(defn process-message
  [{{:keys [bot id]} :author
    {:keys [roles]} :member
    :keys [content channel-id guild-id mentions] :as event-data}]
  (when-not bot
    (let [prefix (or (select-first [ATOM :state (keypath guild-id) :prefix] state)
                     "!")
          admin? (or (= id owner)
                     (= id (select-any [ATOM :guilds (keypath guild-id) :owner-id] state))
                     (user-has-permission? id guild-id :manage-guild))]
      (commands prefix content
        ;; Owner commands
        (#"disconnect"
          (when (= id owner)
            (m/send-message! (:messaging @state) channel-id "Goodbye!")
            (c/disconnect-bot! (:connection @state))))

        ;; Quote commands
        (#"quote\s+add\s+(\S+)\s+([\s\S]+)" [user q]
          (m/send-message! (:messaging @state) channel-id
                           (str "Adding quote to user " user))
          (transform [ATOM :state (keypath guild-id) :quotes (keypath user)] #(conj (or % []) q) state))
        (#"quote\s+remove\s+(\S+)\s+([\s\S]+)" [user q]
          (m/send-message! (:messaging @state) channel-id
                           (str "Removing quote from user " user))
          (transform [ATOM :state (keypath guild-id) :quotes (keypath user)]
                     #(filter (partial not= q) %)
                     state))
        (#"quote\s+(\S+)" [user]
          (let [quotes-vec (select [ATOM :state (keypath guild-id) :quotes (keypath user) ALL] state)]
            (if-not (empty? quotes-vec)
              (m/send-message! (:messaging @state) channel-id
                               (str user ": " (rand-nth quotes-vec)))
              (m/send-message! (:messaging @state) channel-id
                               (str "No quotes found for user " user "!")))))
        (#"quote\s*$"
          (let [quotes-vec (filter #(pos? (count (second %)))
                                   (select [ATOM :state (keypath guild-id) :quotes ALL] state))]
            (if-not (empty? quotes-vec)
              (let [[user quotes] (rand-nth quotes-vec)]
                (m/send-message! (:messaging @state) channel-id
                                 (str user ": " (rand-nth quotes))))
              (m/send-message! (:messaging @state) channel-id
                               "No quotes in this server! Get to talking!"))))

        ;; Mafia commands
        (#"mafia\s+help"
          (m/send-message! (:messaging @state) channel-id
                           (str "Thanks for requesting help with mafia!")))
        (#"mafia\s+start"
          (when-not (mafia.s/active-game? state guild-id)
              (m/send-message! (:messaging @state) channel-id
                               (str "Starting new mafia game"))))
        (#"mafia\s+join"
          (m/send-message! (:messaging @state) channel-id
                           (str "User " (:username ((:users @state) id)) " joined the mafia game!")))
        (#"mafia\s+leave"
          (m/send-message! (:messaging @state) channel-id
                           (str "User " (:username ((:users @state) id)) " left the mafia game.")))

        ;; Admin commands
        (#"prefix\s+(\S+)" [new-prefix]
          (if admin?
            (do (m/send-message! (:messaging @state) channel-id
                                 (str "Using new prefix: " new-prefix))
                (setval [ATOM :state guild-id :prefix] new-prefix state))
            (m/send-message! (:messaging @state) channel-id
                             "You don't have permissions to change that!")))
        (#"blacklist\s+add\s+regex\s+([\S\s]+)" [blacklist-item]
          (when admin?
            (transform [ATOM :state (keypath guild-id) :blacklist]
                       #(conj (or % []) (re-pattern blacklist-item))
                       state)
            (m/send-message! (:messaging @state) channel-id
                             "Adding new blacklisted pattern")))
        (#"blacklist\s+add\s+([\S\s]+)" [blacklist-item]
          (when admin?
            (transform [ATOM :state (keypath guild-id) :blacklist]
                       #(conj (or % []) blacklist-item)
                       state)
            (m/send-message! (:messaging @state) channel-id
                             "Adding new blacklisted name")))
        (#"blacklist\s+list"
          (when admin?
            (m/send-message! (:messaging @state) channel-id
                             (apply str "Blacklisted names:\n"
                                    (interpose
                                     "\n"
                                     (map-indexed
                                      #(str %1 ": " %2)
                                      (select [ATOM :state (keypath guild-id) :blacklist ALL]
                                              state)))))))
        (#"blacklist\s+clear"
          (when admin?
            (setval [ATOM :state guild-id :blacklist ALL] NONE state)
            (m/send-message! (:messaging @state) channel-id
                             "Clearing the blacklist!")))
        (#"blacklist\s+remove\s+(\d+)" [idx]
          (when admin?
            (let [num (Long/parseLong idx)
                  item (select-first [ATOM :state (keypath guild-id) :blacklist (keypath num)] state)]
              (setval [ATOM :state guild-id :blacklist num] NONE state)
              (m/send-message! (:messaging @state) channel-id
                               (str "Removing blacklist item: "
                                    item)))))

        ;; Get help
        (#"help"
          (display-help-message (:messaging @state) channel-id prefix admin?))
        :default
        (when (and (= (count mentions) 1)
                   (= (:id (first mentions)) bot-id))
          (display-help-message (:messaging @state) channel-id prefix
                                admin?))))))
