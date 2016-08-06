(ns smbot.core
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [irclj.core :as irc])
  (:gen-class))

(declare server start)

(defn is-channel? [target]
  (= (first target) \#))

(defn reply [connection target nick message]
  (if (is-channel? target)
    (irc/message connection target (str nick ":") message)
    (irc/message connection nick message)))

(defn rand-nth-n [coll n]
  (let [coll (vec (set coll))]
    (if (> (count coll) n)
      (loop [result (set (take n (repeatedly #(rand-nth coll))))]
        (if (= (count result) n)
          result
          (recur (conj result (rand-nth coll)))))
      coll)))

(defn time-now [] (java.util.Date.))

(defn two-minutes-later []
  (java.util.Date. (+ (.getTime (time-now)) (* 2 60000))))

;; 结构如下：
;; {:all 10                       ; 循环时间内总命令使用次数
;;  :users {"name" 10}            ; 个人命令使用次数
;;  :channels {"#channel" 5}      ; 频道内命令使用次数
;;  :black-list {"name" XXX}}     ; 超出某个使用次数被拉黑的人，XXX 为解封时间
;; update-flood-counter-loop 里有定义循环时间，每次循环会清空所有使用次数，以及更新黑名单
(def flood-counter (ref {:all 0
                         :users {}
                         :channels {}
                         :black-list {}}))

(defn update-flood-counter-loop []
  (future
    (loop []
      (Thread/sleep 60000)
      (dosync
       (let [black-list (reduce #(if (.after (time-now) (second %2))
                                   %1
                                   (assoc %1 (first %2) (second %2)))
                                {} (:black-list @flood-counter))]
         (ref-set flood-counter {:all 0
                                 :users {}
                                 :channels {}
                                 :black-list black-list})))
      (recur))))

(defn flood [user channel]
  (dosync
   (alter flood-counter
          update-in [:users user] #(if % (inc %) 1))
   (when (is-channel? channel)
     (alter flood-counter
            update-in [:channels channel] #(if % (inc %) 1)))
   (alter flood-counter
          update :all inc)
   (cond
     (get-in @flood-counter [:black-list user])
     (do (alter flood-counter
                assoc-in [:black-list user] (two-minutes-later))
         :user-blocked)

     (> (get-in @flood-counter [:users user] 0) 10)
     (do (alter flood-counter
                assoc-in [:black-list user] (two-minutes-later))
         :user)

     (when (is-channel? channel)
       (> (get-in @flood-counter [:channels channel] 0) 20))
     :channel

     (> (:all @flood-counter) 1000)
     :all)))

(defmacro match-cmd [msg on-flood & clauses]
  (when clauses
    (let [m (first clauses)
          regex (first m)
          v (gensym 'v)
          values (vec (cons '_ (next m)))
          flood-type (gensym 'flood-type)]
      `(let [~v (re-find ~regex (:text ~msg))]
         (if ~v
           (let [~flood-type (flood (:nick ~msg) (:target ~msg))]
             (if ~flood-type
               (~on-flood ~flood-type)
               (let [~values ~v]
                 (future ~(second clauses)))))
           (match-cmd ~msg ~on-flood ~@(next (next clauses))))))))

(defn callback [connection args]
  (let [{:keys [target nick text]} args]
    (match-cmd args
      (fn [flood-type]
        (case flood-type
          :user (do (printf "WARNING! user flood by %s: %s\n" nick @flood-counter)
                    (reply connection target nick "⊂彡☆))д´) 拉黑你！"))
          :user-blocked nil
          :channel (printf "WARNING! channel flood in %s: %s\n" target @flood-counter)
          :all (printf "WARNING! all flood: %s\b" @flood-counter)))
      [#"\.sm ([^ ]+) *$" key]
      (let [values (get (:data server) (keyword key))]
        (if values
          (reply connection target nick (rand-nth values))
          (reply connection target nick "not found")))

      [#"\.sm ([^ ]+) (.+)" key new-value]
      (let [key (keyword key)
            values (get (:data server) key)]
        (if (some #(= % new-value) values)
          (reply connection target nick "duplicate value")
          (do (alter-var-root (var server)
                              #(assoc-in % [:data key] (cons new-value values)))
              (reply connection target nick "pushed!"))))

      [#"\.small ([^ ]+) *$" key]
      (let [values (get (:data server) (keyword key))]
        (if values
          (doseq [value (rand-nth-n values 5)]
            (reply connection target nick value)
            (Thread/sleep 500))
          (reply connection target nick "not found"))))))

(defn on-shutdown [connection]
  (irc/kill connection)
  (future (start)))

(defn on-ready [connection args]
  (doseq [channel (server :channels)]
    (irc/join connection channel)))

(defn on-nick-already-in-use [connection args]
  (irc/set-nick connection (str (second (args :params)) \_)))

(defn print-log [a b c]
  (println (.toString (new java.util.Date)))
  (irclj.events/stdout-callback a b c))

(defn start []
  (irc/connect (server :host) (server :port) (server :nick)
               :ssl? (server :ssl) :timeout (* (server :timeout) 1000)
               :real-name (server :real-name)
               :callbacks {:privmsg callback
                           :on-shutdown on-shutdown
                           :001 on-ready
                           :433 on-nick-already-in-use
                           :raw-log print-log}))

(defn -main [& args]
  (let [data-file (if (first args)
                    (first args)
                    "smbot.json")]

    (alter-var-root (var server)
                    (fn [_] (json/read-str (slurp data-file) :key-fn keyword)))
    (start)
    (update-flood-counter-loop)
    (loop []
      (Thread/sleep 100000)
      (with-open [w (io/writer data-file :append false)]
        (binding [*out* w]
          (json/pprint server)))
      (recur))))
