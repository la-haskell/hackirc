(ns hackirc.server
  (:require [lamina.core :as lamina]
            [aleph.tcp :as tcp]
            [gloss.core :as gloss]
            [clojure.string :as s]))

(def number-of-args (into {}
                          (for [[n verbs] {0 [:logout]
                                           1 [:login :join :leave]
                                           2 [:say :whisper]}
                                verb verbs]
                            [verb n])))

(defn format-outgoing-message [{:keys [action source] :as event}]
  (s/join " " (cons (.toUpperCase (name action))
                    (case action
                      (:leave :join) [(:room event) source]
                      :whisper [source (:message event)]
                      :message [(:room event) source (:message event)]
                      :ok nil))))

(defn parse-incoming-message [s]
  (when-let [[_ action s] (re-find #"([A-Z]+) (.*)" s)]
    (let [action (keyword (.toLowerCase action))]
      (if-let [num-args (number-of-args action)]
        (if (= 0 num-args)
          [action]
          (cons action (s/split s #" " num-args)))
        [action s]))))

(defn error [ch msg]
  (lamina/enqueue ch (str "ERROR " msg)))

(defn ack [ch msg]
  (lamina/enqueue ch (str "OK " msg)))

(defmacro with-args [ch action n args params & body]
  `(let [n# ~n, args# ~args,
         c# (count args#)]
     (cond (= c# n#) (let [~params args#] ~@body)
           (< c# n#) (error ~ch (format "not enough args to %s" ~action))
           (> c# n#) (error ~ch (format "too many args to %s" ~action)))))

(defn serve-user [{:keys [events users] :as config} ch username]
  (let [rooms (ref #{})
        relevant-events (->> events
                             (lamina/filter* (fn [{:keys [action source] :as event}]
                                               [event]
                                               (and (not= source username)
                                                    (contains? #{:join :leave :message :whisper}
                                                               action)
                                                    (case action
                                                      (:whisper) (= username (:target event))
                                                      (contains? @rooms (:room event)))))))
        event (fn [action opts]
                (assoc opts :action action, :source username))
        publish! (fn [& args]
                   (lamina/enqueue events (apply event args)))
        ack! (partial ack ch)]
    (lamina/join (lamina/map* format-outgoing-message relevant-events) ch)
    (lamina/on-closed ch (fn logout []
                           (dosync (alter users disj username))
                           (doseq [room @rooms]
                             (publish! :leave {:room room}))))
    (lamina/receive-all ch (fn [msg]
                             (let [[action & args] (parse-incoming-message msg)]
                               (case action
                                 :logout (do (ack! "LOGOUT") (lamina/close ch))
                                 :login (error ch "already logged in")
                                 :join (with-args ch "JOIN" 1 args [room]
                                         (ack! "JOIN")
                                         (when (dosync (when-not (contains? @rooms room)
                                                         (alter rooms conj room)))
                                           (publish! :join {:room room})))
                                 :leave (with-args ch "LEAVE" 1 args [room]
                                          (if (dosync (when (contains? @rooms room)
                                                        (alter rooms disj room)))
                                            (do (ack! "LEAVE")
                                                (publish! :leave {:room room}))
                                            (error ch (format "you are not in room %s" room))))
                                 :say (with-args ch "SAY" 2 args [room message]
                                        (if (contains? @rooms room)
                                          (do (ack! "SAY")
                                              (publish! :message {:room room, :message message}))
                                          (error ch (format "you are not in room %s" room))))
                                 :whisper (with-args ch "WHISPER" 2 args [user message]
                                            (if (contains? @users user)
                                              (do (ack! "WHISPER")
                                                  (publish! :whisper {:target user, :message message}))
                                              (error ch (format "user %s is not logged in" user))))
                                 (error ch (format "unrecognized command %s" action))))))))

(defn tcp-handler [{:keys [users] :as config}]
  (fn [ch _]
    (lamina/receive ch (fn login [msg]
                         (let [[action username & more] (parse-incoming-message msg)]
                           (if (or (not= action :login)
                                   (not username)
                                   more)
                             (do (error ch "first action must be LOGIN <username>")
                                 (lamina/receive ch login))
                             (if-let [login-successful (dosync
                                                        (if (contains? @users username)
                                                          false
                                                          (do (alter users conj username)
                                                              true)))]
                               (do (ack ch "LOGIN")
                                   (serve-user config ch username))
                               (do (error ch (format "username %s is already taken" username))
                                   (lamina/receive ch login)))))))))

(defn init [{:keys [port] :as config}]
  (let [events (lamina/channel* :permanent? true :grounded? true)
        config {:events events, :users (ref #{})}
        tcp (tcp/start-tcp-server (tcp-handler config)
                                  {:port port, :frame (gloss/string :ascii :delimiters ["\n"])})]
    {:tcp tcp, :config config}))

(defn destroy! [server]
  ((:tcp server)))
