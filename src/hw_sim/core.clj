(ns hw-sim.core
  (:require [clojure.string :refer [join]]
            [cheshire.core :as json :refer [parse-string]])
  (:gen-class))

(set! *warn-on-reflection* true)

(defn get-pin-value
  "Gets pin value from given components and pin name."
  [components pin-name]
  (:value (get-in components pin-name)))
    
(defn get-pins-values
  "Return pin values from given components and names"
  [components pin-names]
  (if (coll? (first pin-names))
    (map #(get-pin-value components %) pin-names)
    (-> (get-pin-value components pin-names) 
        (cons '() ))))

(defn get-comp-children
  "Gets all compoenents theat are connected on output of component"
  [comp]
  (get-in comp [:out :children]))

(defn simple-logic-func
  "Simplest logical function.
  Get all :in pins, apply function and set output to :out pin"
  [func comp comps]
  [(->> (:in comp)
        (get-pins-values comps)
        (apply func)
        (assoc-in comp [:out :value]))
   
   (get-comp-children comp)])

(defn not-func
  "Logical NOT function"
  [value]
  (if (= value 0) 1 0))

(defn bit-func
  "BIT function, returns unmodified component and value of output pin"
  [comp comps]
  [comp (get-comp-children comp)])

(defn simple-reg-func
  "Simple register function"
  [comp comps]
  [(let [load-pin (get-pin-value comps (:ld comp))
         input-pin (get-pin-value comps (:in comp))]
         
         (when (= load-pin 1) (assoc-in comp [:out :value] input-pin)))
   
   (get-comp-children comp)])

(defmulti logic-func (fn [comp comps] (:type comp)))

(defmethod logic-func :and [comp comps] (simple-logic-func bit-and comp comps))
(defmethod logic-func :or [comp comps] (simple-logic-func bit-or comp comps))
(defmethod logic-func :xor [comp comps] (simple-logic-func bit-xor comp comps))
(defmethod logic-func :not [comp comps] (simple-logic-func not-func comp comps))
(defmethod logic-func :bit [comp comps] (bit-func comp comps))
(defmethod logic-func :reg [comp comps] (simple-reg-func comp comps))

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

(defn process-pin
  [pin-data scheme]
  (let [[pin-path pin-name] pin-data
        norm-pin-path (norm-comp-name pin-path scheme)
        norm-pin-name (keyword pin-name)]
    
    [norm-pin-path norm-pin-name]))

(defn process-pins 
  [pin-data scheme]
  (when pin-data
    (if (coll? (first pin-data))
      (-> (map #(process-pin % scheme) pin-data)
          (into []))
      (process-pin pin-data scheme))))
        
(defn process-simple-comp
  [comp-type comp-data scheme comps]
  (let [{comp-name :name input :in output :out} comp-data
        in-pins (process-pins input scheme)
        out-pins (map #(norm-comp-name % scheme) output)
        component-init {:type comp-type
                        :out {:value 1 :children (set out-pins)}}
        component (if in-pins
                    (assoc component-init :in in-pins)
                    component-init)
        norm-name (norm-comp-name comp-name scheme)]
    
    [norm-name component]))

; function for making components from raw data 
(defmulti process-comp (fn [component scheme comps] (:type component)))

(defmethod process-comp "scheme"
  [scheme parent-scheme comps]
  (let [scheme-prefix (make-comp-name parent-scheme (:name scheme))]
    (->> (:components scheme)
         (reduce #(process-comp %2 scheme-prefix %1) comps))))
    
(defmethod process-comp "not"
  [not-data scheme comps]
  (let [[comp-name component] (process-simple-comp :not not-data scheme comps)]
    (assoc comps comp-name component)))

(defmethod process-comp "and"
  [and-data scheme comps]
  (let [[comp-name component] (process-simple-comp :and and-data scheme comps)]
    (assoc comps comp-name component)))

(defmethod process-comp "or"
  [or-data scheme comps]
  (let [[comp-name component] (process-simple-comp :or or-data scheme comps)]
    (assoc comps comp-name component)))

(defmethod process-comp "xor"
  [xor-data scheme comps]
  (let [[comp-name component] (process-simple-comp :xor xor-data scheme comps)]
    (assoc comps comp-name component)))
    
(defmethod process-comp "bit"
  [bit-data scheme comps]
  (let [[comp-name component] (process-simple-comp :bit bit-data scheme comps)]
    (assoc comps comp-name component)))
    
(defmethod process-comp "reg"
  [reg-data scheme comps]
  (let [[comp-name component] (process-simple-comp :reg reg-data scheme comps)
        ld-pin (process-pin (:ld reg-data) scheme)]
        
    (-> (assoc component :ld ld-pin)
        (assoc :clk 1)
        (#(assoc comps comp-name %)))))

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


(def scheme-data (load-scheme-data "schemes/schemes.json"))
(def scheme (process-comp scheme-data nil {}))
(def comps (atom scheme))
(def history (atom [@comps]))

(exec-num-clks 5 comps history)
(print-history history)

(defn -main
  "Main"
  [& args]
  (println "Main"))
