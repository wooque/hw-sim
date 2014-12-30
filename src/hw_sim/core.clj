(ns hw-sim.core
  (:require [clojure.string :refer [join]]
            [cheshire.core :as json :refer [parse-string]])
  (:gen-class))
  
(set! *warn-on-reflection* true)

; record for Pin with its state and connected components aka children
(defrecord Pin [value children])

(defn get-pin-value
  "Gets pin value from given components and pin name."
  [components pin-name]
  (:value (get-in components pin-name)))
    
(defn get-pins-values
  "Return pin values from given components and names"
  [components pin-names]
  (if (coll? (first pin-names))
    (map #(get-pin-value components %) pin-names)
    (cons (get-pin-value components pin-names) '()) ))

(defprotocol LogicComponent
  "Protocol of logic component"
  (logic-func [this components] "Function logic component implements"))

(defn get-comp-children
  "Gets all compoenents theat are connected on output of component"
  [comp]
  (get-in comp [:out :children]))

(defn simple-logical-func
  "Simplest logical function.
  Get all :in pins, apply function and set output to :out pin"
  [func comp comps]
  [(->> (:in comp)
        (get-pins-values comps)
        (apply func)
        (assoc-in comp [:out :value]))
   
   (get-comp-children comp)])

(defmacro defcomponent
  [kind func & additional-pins]
  `(defrecord ~kind [~'in ~'out ~@additional-pins]
     LogicComponent
     (logic-func [this# components#]
       (~func this# components#))))

; definition for AND, OR, XOR logic components
(defcomponent AND (partial simple-logical-func bit-and))
(defcomponent OR (partial simple-logical-func bit-or))
(defcomponent XOR (partial simple-logical-func bit-xor))

(defn not-func
  "Logical NOT function"
  [value]
  (if (= value 0) 1 0))
  
; definition for NOT logical component
(defcomponent NOT (partial simple-logical-func not-func))

(defn bit-func
  "BIT function, returns unmodified component and value of output pin"
  [comp comps]
  [comp (get-comp-children comp)])

; Standalone Pin definition
(defcomponent BIT bit-func)

(defn simple-reg-func
  "Simple register function"
  [comp comps]
  [(let [load-pin (get-pin-value comps (:ld comp))
         input-pin (get-pin-value comps (:in comp))]
         
         (when (= load-pin 1) (assoc-in comp [:out :value] input-pin)))
   
   (get-comp-children comp)])

; definition for REG logic component
(defcomponent REG simple-reg-func ld clk)

(defn exec-logic-func
  "Execute component logic function"
  [comp-name components]
  (let [[updated-comp children] (-> (comp-name @components)
                                    (logic-func @components))]
    (swap! components #(assoc % comp-name updated-comp))
    children))

(defn get-components-by-property
  "Gets components from components map by property"
   [comps property]
   (->> comps
        (filter #(property (val %)))
        (keys)
        (into [])))

(defn get-next-comp
  "Gets next component for execution"
  [comp-order]
  (let [next-comp (first @comp-order)]
    (swap! comp-order #(pop %))
    next-comp))

(defn update-comp-order
  "Adds new component for execution"
  [comp-order next-comps components]
  (let [non-seq-comps (filter #(not (:clk (% components))) next-comps)] 
    (swap! comp-order #(into % non-seq-comps))))

(defn update-history
  "Updates history with new state"
  [history new-state]
  (swap! history #(conj % new-state)))

(defn get-starting-components
  "Gets components from which simulation is starting.
   These are the one that are sequential"
  [comps]
  (get-components-by-property comps :clk))

(defn exec-one-comp
  "Execute one harware component"
  [components comp-order]
  (when-let [next-comp (get-next-comp comp-order)]
    (when-let [next-comps (exec-logic-func next-comp components)]
      (update-comp-order comp-order next-comps @components))))

(defn exec-clk
  "Execute one hardware clock"
  [comps history]
  (let [comp-order (atom clojure.lang.PersistentQueue/EMPTY)]
        (swap! comp-order #(into % (get-starting-components @comps)))
        
    (while (not (empty? @comp-order))
      (exec-one-comp comps comp-order))
    (update-history history @comps)))
    
(defn exec-num-clks
  "Execute given number of hardware clocks"
  [num comps history]
  (dotimes [_ num] (exec-clk comps history)))

(defn load-scheme-data
  "Load hardware scheme from JSON file"
  [file]
    (-> (slurp file)
        (json/parse-string true)))

(defn make-comp-name
  "Makes component name in format parent/component-name"
  [parent component]
    (if parent
      (str parent "/" component)
      component))
      
(defn norm-comp-name
  "For names from local scheme put prefix, also keywordize all names"
  [^String comp-name scheme]
    (-> (if (.contains comp-name "/")
          (comp-name)
          (make-comp-name scheme comp-name))
       
        (keyword)))

; function for making components from raw data 
(defmulti process-comp (fn [component scheme comps] (:type component)))

(defmethod process-comp "scheme"
  [scheme parent-scheme comps]
    (let [scheme-prefix (make-comp-name parent-scheme (:name scheme))]
      (->> (:components scheme)
           (reduce #(process-comp %2 scheme-prefix %1) comps))))
    
(defmethod process-comp "not"
  [not-data scheme comps]
    (let [input (:in not-data)
          in-comp (norm-comp-name (:component input) scheme)
          in-pin (norm-comp-name (:pin input) scheme)
          out-pin (map #(norm-comp-name % scheme) (:out not-data))
          not-comp (->NOT [in-comp in-pin] (->Pin 1 (set out-pin)))]
      
      (->> (norm-comp-name (:name not-data) scheme)
           (#(assoc comps % not-comp)))))
    
(defmethod process-comp "bit"
  [bit-data scheme comps]
    comps)
    
(defmethod process-comp "reg"
  [reg-data scheme comps]
    comps)

(defn comps-to-string
  "Makes string from map of components"
  [comps]
  (->> (map #(str (key %)
              " "
              (get-in (val %) [:out :value]))
                      
        comps)
        
        (join \newline)))
        
(defn print-history
  "Pretty print history"
  [history]
  (->> (map #(comps-to-string %) @history)
       (join "\n--------------\n")
       (println)))

;(def comps (atom {:bit_one (->BIT nil
                                  ;(->Pin 1 #{:reg_main}))
                  ;
                  ;:reg_neg (->NOT [:reg_main :out]
                                  ;(->Pin 1 #{:reg_main}))
                  ;
                  ;:reg_main (->REG [:reg_neg :out]
                                   ;(->Pin 0 #{:reg_neg})
                                   ;[:bit_one :out] 1)}))
;(def history (atom [@comps]))
;
;(exec-num-clks 5 comps history)
;
;(print-history history)

(def scheme-data (load-scheme-data "schemes/schemes.json"))
(def scheme (process-comp scheme-data nil {}))
(println scheme)

(defn -main
  "Main"
  [& args]
  (println "Main"))
