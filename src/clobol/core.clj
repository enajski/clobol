(ns clobol.core
  (:require [clobol.compiler :as compiler]
            [clojure.string :as str]))

(def substitutes {:clobol/indent         "    "
                  :clobol/end-row        "."
                  :clobol/header-padding "      "
                  :clobol/spc            " "})


(defn interpolate [row]
  (->> row
       (cons :clobol/header-padding)
       (map (fn [token]
              (if-let [substitution (get substitutes token)]
                substitution
                token)))
       (map (fn [token]
              (if (keyword? token)
                (name token)
                token)))))


(defn build-string [rows]
  (str/join "\n" (for [row rows]
                   (str/join "" (interpolate row)))))


(defn identification-division [identification-data]
  (let [{:keys [program-id]} identification-data]
    [["IDENTIFICATION DIVISION" :clobol/end-row]
     ["PROGRAM-ID" :clobol/end-row :clobol/spc program-id :clobol/end-row]]))


(defn data-division [division-data]
  (let [{:keys [working-storage]} division-data]
    (concat [["DATA DIVISION" :clobol/end-row]]
            (when (seq working-storage)
              (into [["WORKING-STORAGE SECTION" :clobol/end-row]]
                    (for [storage-entry working-storage]
                      (concat ["01"
                               :clobol/spc
                               (:name storage-entry)
                               :clobol/spc
                               (:type storage-entry)]
                              (when-let [val (:value storage-entry)]
                                [:clobol/spc
                                 "VALUE"
                                 :clobol/spc
                                 val])
                              [:clobol/end-row])))))))


(defn procedure-division [procedure-data]
  (cons ["PROCEDURE DIVISION" :clobol/end-row]
        procedure-data))


(defn assemble-clobol-ast [clobol-ast]
  (let [{:keys [identification
                data
                procedure]} clobol-ast]
    (build-string (concat (identification-division identification)
                          (data-division data)
                          (procedure-division procedure)))))


(defn compile-clojure-ns [ns]
  (-> ns
      (compiler/compile-ns)
      (assemble-clobol-ast)))


(defn save [path text]
  (spit path text))


(comment

  (def sum-of-integers {:identification {:program-id "SumOfIntegers"}
                        :data {:working-storage [{:name "n"
                                                  :type "BINARY-LONG"}
                                                 {:name "i"
                                                  :type "BINARY-LONG"}
                                                 {:name "suma"
                                                  :type "BINARY-LONG"
                                                  :value 0}]}
                        :procedure [["DISPLAY" :clobol/spc "\"Enter a positive integer\""]
                                    ["ACCEPT" :clobol/spc :n]
                                    ["perform" :clobol/spc "varying" :clobol/spc "i" :clobol/spc "from" :clobol/spc 1 :clobol/spc "by" :clobol/spc 1 :clobol/spc "until" :clobol/spc "i" :clobol/spc ">" :clobol/spc "n"]
                                    [:clobol/indent "add" :clobol/spc :i :clobol/spc :to :clobol/spc :suma]
                                    [:end-perform]
                                    [:display :clobol/spc "\"The sum is \"" :clobol/spc :suma :clobol/end-row]
                                    []]})


  (save "src/clobol/cobol/hello_world.cob" (compile-clojure-ns 'clobol.clojure.hello-world))

  (save "src/clobol/cobol/let.cob" (compile-clojure-ns 'clobol.clojure.let))

  )
