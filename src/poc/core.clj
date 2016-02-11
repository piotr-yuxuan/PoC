(ns poc.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [poc.commons :refer :all])
  (:gen-class))

(def cli-options
  [;; First three strings describe a short-option, long-option with optional
   ;; example argument description, and a description. All three are optional
   ;; and positional.
   ["-f" "--file FILE" "File to parse"
    :validate [#(.exists (io/as-file %)) "Must point to an existing file"]]
   ["-w" "--white PLAYER1" "Name of the player to play white side"
    :default "Barry White"
    :validate [empty? "Name for white player must not be empty string"]]
   ["-b" "--black PLAYER2" "Name of the player to play black side"
    :default "Frank Black"
    :validate [empty? "Name for black player must not be empty string"]]
   ["-h" "--help" "Give you some basic hints"]])

(def required-opts
  "Minimum options each argument need to run properly."
  {"parse" [[:file] "needs a file to parse"]
   "play" [[:white :black] "needs two names for black and white. You can let them to their default values." ]})

(defn required-summary
  [arguments]
  (reduce str-n (map #(let [indent "  "
                            arg %
                            sep1 "\t"
                            desc (second (get required-opts %))]
                        (str indent arg sep1 desc))
                     arguments)))

(defn usage [options-summary]
  (str-n (str "This is merely a proof-of-concept whilst I am looking "
              "for job. Hope I will help me proove I can handle my "
              "dream job once I find it!" )
         ""
         "Usage: program-name action [options]"
         ""
         "Actions:"
         "  parse  Parse a file"
         "  play   Start a game"
         ""
         "Options:"
         options-summary
         ""
         (str "Please refer to the GitHub repository or even the "
              "source code for further information." )))

(defn error-msg [errors]
  (str-n "The following errors occurred while parsing your command:\n\n"
         errors))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn missing-required?
  "Returns true if opts is missing any of the required-opts"
  [arguments opts]
  (reduce (fn [answer arg] (and answer (every? #(incoll? % (keys opts))
                                              (first (get required-opts arg)))))
          true
          arguments))

(declare play-loop parse-startpoint)

(defn -main [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options)
      (exit 0 (usage summary))
      (not= (count arguments) 1)
      (exit 1 (usage summary))
      errors (exit 1 (error-msg errors))
      (not (missing-required? arguments options))
      (exit 1 (usage (required-summary arguments))))
    (case (first arguments)
      "parse" (parse-startpoint options)
      "play" (play-loop options)
      (exit 1 (usage summary)))))

(defn play-loop
  [options]
  (println "Let's play!"))

(defn parse-startpoint
  [options]
  (println "Let's parse!"))
