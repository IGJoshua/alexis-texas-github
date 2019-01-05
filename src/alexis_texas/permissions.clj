(ns alexis-texas.permissions
  (:require
   [alexis-texas.events :refer [state]]))

(def permissions-bit {:create-instant-invite 0x1
                      :kick-members 0x2
                      :ban-members 0x4
                      :administrator 0x8
                      :manage-channels 0x10
                      :manage-guild 0x20
                      :add-reactions 0x40
                      :view-audit-log 0x80
                      :view-channel 0x400
                      :send-messages 0x800
                      :send-tts-messages 0x1000
                      :manage-messages 0x2000
                      :embed-links 0x4000
                      :attach-files 0x8000
                      :read-message-history 0x10000
                      :mention-everyone 0x20000
                      :use-external-emojis 0x40000
                      :connect 0x100000
                      :speak 0x200000
                      :mute-members 0x400000
                      :deafen-members 0x800000
                      :move-members 0x1000000
                      :use-vad 0x2000000
                      :priority-speaker 0x100
                      :change-nickname 0x4000000
                      :manage-nicknames 0x8000000
                      :manage-roles 0x10000000
                      :manage-webooks 0x20000000
                      :manage-emojis 0x40000000})

(defn has-permission?
  [perm perms-int]
  (when perms-int
    (when-let [bit (permissions-bit perm)]
      (not (zero? (bit-and bit perms-int))))))

(defn has-permissions?
  [perms perms-int]
  (every? #(has-permission? % perms-int) perms))

(defn user-has-permission?
  [user guild-id perm]
  (let [user-roles (conj (:roles (get (:guilds (get (:users @state) user)) guild-id))
                         guild-id)
        roles-permissions (map (fn [[_ {:keys [permissions]}]]
                                 permissions)
                               (select-keys (get (:roles @state) guild-id) user-roles))
        permissions-int (if (> (count roles-permissions) 1)
                          (apply bit-or roles-permissions)
                          (first roles-permissions))]
    (has-permission? perm permissions-int)))

(defn user-has-permissions?
  [user guild-id perms]
  (every? #(user-has-permission? user guild-id %) perms))
