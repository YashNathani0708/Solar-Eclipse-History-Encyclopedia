(ns main.core
    (:gen-class)
    (:require [clojure.string :as str]
      [clojure.java.io :as io])
    (:import [java.io File])
    (:import [java.util.regex Pattern]))


(defn read-eclipse-events [file-path]
      (with-open [rdr (io/reader file-path)]
                 (let [lines (doall (line-seq rdr))
                       events (partition 5 lines)]
                      (vec (map (fn [block]
                                    (zipmap [:date :location :type :significance] block))
                                events)))))

(defn display-eclipse-events [events]
      (println "\nTotal events found:" (count events))
      (println "")
      (doseq [event events]
             (println (:date event))
             (println (:location event))
             (println (:type event))
             (println (:significance event))
             (println "----------------------------------------------------------------------------------------")))

(defn add-eclipse-event [file-path]
      (print "Enter date:") (flush)
      (let [date (str "Date: " (read-line) "\n")
            location (str "Location: " (do (print "Enter location:") (flush) (read-line)) "\n")
            type (str "Type: " (do (print "Enter type:") (flush) (read-line)) "\n")
            significance (str "Significance: " (do (print "Enter significance:") (flush) (read-line)) "\n\n")]
           (spit file-path (str date location type significance) :append true)
           (println "Event added successfully.")))

(defn list-eclipse-events [events]
      (println "")
      (doseq [[index event] (map-indexed vector events)]
             (println (str "Event Index: " (inc index)))
             (println (:date event))
             (println (:location event))
             (println (:type event))
             (println (:significance event))
             (println "----------------------------------------------------------------------------------------")))

(defn search-eclipse-events [events]
      (print "Enter search type (date/location): ") (flush)
      (let [search-type (str/lower-case (read-line))
            query-prompt (if (= "date" search-type) "Enter search query for date: " "Enter search query for location: ")
            search-field (if (= "date" search-type) :date :location)]
           (print query-prompt) (flush)
           (let [search-query (str/lower-case (read-line))
                 regex-pattern (re-pattern (str "." (Pattern/quote search-query) "."))
                 matching-events (filter (fn [event]
                                             (re-find regex-pattern
                                                      (str/lower-case (event search-field))))
                                         events)]
                (if (empty? matching-events)
                  (println "No matching events found.")
                  (display-eclipse-events matching-events)))))

(defn modify-eclipse-event [events file-path]
      (print "Enter the index of the event you want to modify: ") (flush)
      (let [index (Integer/parseInt (read-line))]
           (if (and (>= index 0) (< index (count events)))
             (do
               (let [event-to-modify (nth events index)]
                    (println "Selected Event:")
                    (println (:date event-to-modify))
                    (println (:location event-to-modify))
                    (println (:type event-to-modify))
                    (println (:significance event-to-modify))
                    (println "----------------------------------------------------------------------------------------")
                    (print "Enter new date (leave blank to keep current): ") (flush)
                    (let [new-date (read-line)
                          updated-date (if (empty? (str/trim new-date)) (:date event-to-modify) new-date)]
                         (print "Enter new location (leave blank to keep current): ") (flush)
                         (let [new-location (read-line)
                               updated-location (if (empty? (str/trim new-location)) (:location event-to-modify) new-location)]
                              (print "Enter new type (leave blank to keep current): ") (flush)
                              (let [new-type (read-line)
                                    updated-type (if (empty? (str/trim new-type)) (:type event-to-modify) new-type)]
                                   (print "Enter new significance (leave blank to keep current): ") (flush)
                                   (let [new-significance (read-line)
                                         updated-significance (if (empty? (str/trim new-significance)) (:significance event-to-modify) new-significance)]
                                        (let [updated-event (assoc event-to-modify :date updated-date
                                                                   :location updated-location
                                                                   :type updated-type
                                                                   :significance updated-significance)]
                                             (let [updated-events (vec (assoc events index updated-event))]
                                                  (spit file-path (apply str (interpose "\n\n" (map #(apply str (interpose "\n" (vals %))) updated-events))) :truncate true)
                                                  (println "Event modified successfully.")))))))))
             (println "Invalid event index."))))


(defn modify-eclipse-event [events file-path]
      (println "=== Modify Eclipse Event ===")
      (list-eclipse-events events) ; Assuming you have a function to list events with indices

      (print "Enter the index of the event you want to modify: ") (flush)
      (let [index (Integer/parseInt (read-line))]
           (if (and (>= index 1) (<= index (count events)))
             (let [selected-event (nth events (dec index))] ; Adjust index to zero-based
                  (println "\nSelected Event:")
                  (println "Date: " (:date selected-event))
                  (println "Location: " (:location selected-event))
                  (println "Type: " (:type selected-event))
                  (println "Significance: " (:significance selected-event))

                  (print "\nEnter new date (leave blank to keep current): ") (flush)
                  (let [new-date (str/trim (read-line))
                        updated-date (if (empty? new-date) (:date selected-event) new-date)]
                       (print "Enter new location (leave blank to keep current): ") (flush)
                       (let [new-location (str/trim (read-line))
                             updated-location (if (empty? new-location) (:location selected-event) new-location)]
                            (print "Enter new type (leave blank to keep current): ") (flush)
                            (let [new-type (str/trim (read-line))
                                  updated-type (if (empty? new-type) (:type selected-event) new-type)]
                                 (print "Enter new significance (leave blank to keep current): ") (flush)
                                 (let [new-significance (str/trim (read-line))
                                       updated-significance (if (empty? new-significance) (:significance selected-event) new-significance)]
                                      (let [updated-event (assoc selected-event
                                                                 :date updated-date
                                                                 :location updated-location
                                                                 :type updated-type
                                                                 :significance updated-significance)]
                                           (let [updated-events (assoc events (dec index) updated-event)] ; Adjust index to zero-based
                                                (spit file-path (apply str (interpose "\n\n" (map #(apply str (interpose "\n" (vals %))) updated-events))) :truncate true)
                                                (println "\nEvent modified successfully.")
                                                updated-events)))))))))) ; Close all the let bindings
(println "Invalid event index.")


(defn display-menu []
      (println "\n=== Eclipse History Encyclopedia ===")
      (println "1. View Eclipse Events")
      (println "2. Add New Eclipse Event")
      (println "3. Modify Eclipse Event")
      (println "4. Search for Eclipse Events")
      (println "5. Exit")
      (print "\nEnter your choice (1-5): ") (flush))

(defn main-loop [file-path]
      (let [events (atom (read-eclipse-events file-path))]
           (loop []
                 (display-menu)
                 (let [choice (Integer/parseInt (read-line))]
                      (cond
                        (= choice 1) (display-eclipse-events @events)
                        (= choice 2) (do (add-eclipse-event file-path)
                                         (reset! events (read-eclipse-events file-path)))
                        (= choice 3) (modify-eclipse-event @events file-path) ; Added this line for option 3
                        (= choice 4) (search-eclipse-events @events)
                        (= choice 5) (do (println "Exiting...") (System/exit 0))
                        :else (println "Invalid choice, please enter a number from 1 to 5")))
                 (recur))))

(defn -main [& args]
      (let [file-path (if (empty? args) "/Users/vasuchampaneria/IdeaProjects/clojure_testing/resources/Eclipse-event.txt" (first args))]
           (println "Starting Eclipse History Encyclopedia...")
           (main-loop file-path)))

(-main)