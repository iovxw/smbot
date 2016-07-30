(ns smbot.core
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [irclj.core :as irc])
  (:gen-class))

(declare server start)

(defmacro cond-let [& clauses]
  (when clauses
    (if (= (first clauses) :else)
      (second clauses)
      `(let ~(first clauses)
         (if ~(first (first clauses))
           ~(second clauses)
           ~(cons 'cond-let (next (next clauses))))))))

(defn reply [connection target nick message]
  (if (= target (:nick @connection))
    (irc/message connection nick message)
    (irc/message connection target (str nick ":") message)))

(defn callback [connection args]
  (let [{:keys [target nick text]} args]
    (cond-let
     [r (re-find #"\.sm ([^ ]+)(?: (.+))?" text)]
     (let [[_ key new-value] r
           key (keyword key)
           values (get (:data server) key)]
       (if new-value
         (if (some #(= % new-value) values)
           (reply connection target nick "duplicate value")
           (do (alter-var-root (var server)
                               #(assoc-in % [:data key] (cons new-value values)))
               (reply connection target nick "pushed!")))
         (if values
           (reply connection target nick (rand-nth values))
           (reply connection target nick "not found"))))
     [r (re-find #"\.small ([^ ]+)" text)]
     (let [key (keyword (second r))
           values (get (:data server) key)]
       (if values
         (doseq [value values]
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
    (loop []
      (Thread/sleep 100000)
      ;(spit data-file (json/write-str server))
      (with-open [w (io/writer data-file :append false)]
        (binding [*out* w]
          (json/pprint server)))
      (recur))))
