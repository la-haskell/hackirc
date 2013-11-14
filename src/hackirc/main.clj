(ns hackirc.main
  (:require [hackirc.server :as server]))

(defn -main [& args]
  (let [port (if args
               (Long/parseLong (first args))
               1801)]
    (def server (server/init {:port port}))))