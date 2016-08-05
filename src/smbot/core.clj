(ns smbot.core
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [irclj.core :as irc])
  (:gen-class))

(declare server start)

(defn reply [connection target nick message]
  (if (= target (:nick @connection))
    (irc/message connection nick message)
    (irc/message connection target (str nick ":") message)))

(defn rand-nth-n [coll n]
  (let [coll (vec (set coll))]
    (if (> (count coll) n)
      (loop [result (set (take n (repeatedly #(rand-nth coll))))]
        (if (= (count result) n)
          result
          (recur (conj result (rand-nth coll)))))
      coll)))

(defmacro match-cmd [msg & clauses]
  (when clauses
    (let [m (first clauses)
          regex (first m)
          v (gensym 'v)
          values (vec (cons '_ (next m)))]
      `(let [~v (re-find ~regex (:text ~msg))]
         (if ~v
           (let [~values ~v]
             ~(second clauses))
           (match-cmd ~msg ~@(next (next clauses))))))))

(defn callback [connection args]
  (future
    (let [{:keys [target nick text]} args]
      (match-cmd args
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
           (reply connection target nick "not found")))))))

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
    (loop []
      (Thread/sleep 100000)
      (with-open [w (io/writer data-file :append false)]
        (binding [*out* w]
          (json/pprint server)))
      (recur))))
