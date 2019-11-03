(ns clobol.compiler
  (:require [clojure.string :as str]
            [clojure.tools.analyzer.jvm :as ana.jvm]))


(def compilation-result (atom {}))


(defn update-source-map [ast-data]
  (let [{:keys [identification procedure data]
         :or   {identification {}
                procedure      []
                data           {}}} ast-data]
    (swap! compilation-result
           (fn [acc]
             (-> acc
                 (update :identification #(merge % identification))
                 (update :procedure #(vec (concat % procedure)))
                 (update-in [:data :working-storage] #(concat % (:working-storage data))))))))


(defn emit [ast]
  (update-source-map ast)
  @compilation-result)


(defmulti invoke* (fn [ast] (get-in ast [:fn :var])))


(defmulti emit* :op)


(defmulti static-call* (fn [ast] (:method ast)))


(defn- wrap-in-double-quotes [x]
  (str \" x \"))


(defn emit-string [ast]
  (wrap-in-double-quotes (:val ast)))


(defmethod static-call* 'inc
  [{args :args}]
  (let [arg (first args)]
    (cond
      (= :number (:type arg))
      (inc arg)

      (= :local (:op arg))
      (let [local (:form arg)]
        (emit {:procedure [["SET"
                            :clobol/spc
                            local
                            :clobol/spc
                            "UP BY"
                            :clobol/spc
                            1]]})
        (emit* arg)))))


(defmethod static-call* 'add
  [{args :args}]
  (apply + (map emit* args)))


(defmethod invoke* #'clojure.core/println
  [{args :args}]
  (emit {:procedure [(vec (flatten ["DISPLAY"
                                    (for [arg args]
                                      [:clobol/spc
                                       (emit* arg)])
                                    :clobol/end-row]))]}))


(defmethod emit* :invoke
  [ast]
  (invoke* ast))


(defmethod emit* :static-call
  [ast]
  (static-call* ast))


(defmethod emit* :local
  [ast]
  (get-in ast [:form]))


(defmethod emit* :const
  [ast]
  (condp = (:type ast)
    :string (emit-string ast)

    :else
    (get-in ast [:form])))


(defn- is-ns-declaration? [ast]
  (= 'ns (some-> ast :raw-forms first first)))


(defn emit-ns [ast]
  (let [ns-sym (-> ast :raw-forms first second str)]
    (emit {:identification {:program-id (last (str/split ns-sym #"\."))}})))


(defmethod emit* :do
  [ast]
  (if (is-ns-declaration? ast)
    (emit-ns ast)))


(defn get-cobol-type [clj-tag]
  (get {:number "BINARY-LONG"} clj-tag))


(defmethod emit* :let
  [ast]
  (let [variables (for [binding (:bindings ast)]
                    {:name  (:form binding)
                     :type  (get-cobol-type (get-in binding [:init :type]))
                     :value (get-in binding [:init :val])})]
    (emit {:data {:working-storage (vec variables)}})
    (emit* (:body ast))))


(defn- is-defn-declaration? [ast]
  (some-> ast :init :expr))


(defn emit-defn [ast]
  (emit {:procedure [[(:name ast)
                      :clobol/end-row]
                     ["DISPLAY" (emit-string {:val (:name ast)})]]}))


(defmethod emit* :def
  [ast]
  (if (is-defn-declaration? ast)
    (emit-defn ast)))


(defn emit-all [ast]
  (emit* ast))


(defn compile-asts [asts]
  (doseq [ast asts]
    (emit-all ast))
  (let [res @compilation-result]
    (reset! compilation-result {})
    res))


(defn compile-ns [ns]
  (compile-asts (ana.jvm/analyze-ns ns)))


(comment

  (reset! compilation-result {})

  (-> (ana.jvm/analyze '(defn sum-of-integers [n]
                          (reduce + (range (inc n))))))


  (compile-asts (ana.jvm/analyze-ns 'clobol.clojure.hello-world))

  (compile-asts (ana.jvm/analyze-ns 'clobol.clojure.let))

  )
