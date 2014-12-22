(ns hw-sim.core
  (:use [clojure.string :only [join]])
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
    '( (get-pin-value components pin-names) )))

(defprotocol LogicComponent
  "Protocol of logic component"
  (logic-func [this components] "Function that logic component implements"))

(defn simple-logical-func
  "Simplest logical function.
  Get all :in pins, apply function and set output to :out pin"
  [func comp comps]
  [(->> (:in comp)
        (get-pins-values comps)
        (apply func)
        (assoc-in comp [:out :value]))
   
   (get-in comp [:out :children])])

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

; Standalone Pin definition
(defrecord BIT [out]
  LogicComponent
  (logic-func [this components]
    ([this (get-in comp [:out :children])])))

(defn simple-reg-func
  "Simple register function"
  [comp comps]
  [(let [load-pin (get-pin-value comps (:ld comp))
         input-pin (get-pin-value comps (:in comp))]
         
         (when (= load-pin 1) (assoc-in comp [:out :value] input-pin)))
   
   (get-in comp [:out :children])])

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
  "Execute clk"
  [comps history]
  (let [comp-order (atom clojure.lang.PersistentQueue/EMPTY)]
        (swap! comp-order #(into % (get-starting-components @comps)))
        (while (not (empty? @comp-order))
          (exec-one-comp comps comp-order))
        (update-history history @comps)))
             
(defn comps-to-string
  "components to string"
  [comps]
  (->> (map #(str (key %)
              " "
              (get-in (val %) [:out :value]))
                      
        comps)
        
        (join \newline)))
        
(defn print-history
  "Print history"
  [history]
  (->> (map #(comps-to-string %) @history)
       (join "\n--------------\n")
       (println)))

(def comps (atom {:bit (->BIT (->Pin 1 #{:reg}))
            :not (->NOT [:reg :out] (->Pin 1 #{:reg}))
            :reg (->REG [:not :out] (->Pin 0 #{:not}) [:bit :out] 1)}))
(def history (atom [@comps]))

(exec-clk comps history)
(exec-clk comps history)
(exec-clk comps history)
(exec-clk comps history)

(print-history history)

(defn -main
  "Main"
  [& args]
  (println "Hello"))
