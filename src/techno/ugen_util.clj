(ns techno.ugen-util
  (:use [overtone.sc.machinery.ugen.fn-gen]
        [overtone.sc.machinery.ugen.specs]
        [clojure.pprint]
        [clojure.set :only [difference]]
        [overtone.helpers lib]
        [overtone.libs counters]
        [overtone.helpers seq]
        [overtone.sc bindings]
        [overtone.sc.node :only [idify]]
        [overtone.sc.machinery.ugen sc-ugen defaults specs special-ops intern-ns common categories]
        [overtone.sc.machinery.ugen.metadata unaryopugen binaryopugen])
  (:require [overtone.sc.machinery.ugen.doc :as doc]))

(defn- spec-arg-names
  "Returns a list of keywords representing the valid argument names for
   the specified ugen spec"
  [spec]
  (map #(keyword (:name %)) (:args spec)))

(defn- specs-from-namespaces
  "Gathers all ugen spec metadata (stored in the vars spec and specs-collide)
  from the specified namespaces into a single vector of maps.

  Takes a seq of namespace endings (see UGEN-NAMESPACES) and returns a vector
  of maps containing ugen metadata."
  [namespaces]
  (reduce (fn [mem ns]
            (let [full-ns (symbol (str "overtone.sc.machinery.ugen.metadata." ns))
                  _ (require [full-ns :only '[specs specs-collide]])
                  specs (var-get (ns-resolve full-ns 'specs))]

              ;; TODO: Currently colliders must be loaded before specs in order
              ;; for this to run properly, because some ugens in specs derive
              ;; from the 'index' ugen in colliders.  Maybe the derivation
              ;; process should get smarter...
              (if-let [colliders (ns-resolve full-ns 'specs-collide)]
                (concat mem (var-get colliders) specs)
                (concat mem specs))))
          []
          namespaces))

(defn- args-with-specs
  "Creates a list of (arg-value, arg-spec-item) pairs."
  [args spec property]
  {:pre [(keyword? property)]}
  (partition 2 (interleave args (map property (:args spec)))))

(defn- ugen-arg-rates [ugen]
  (map REVERSE-RATES (map :rate (filter sc-ugen? (:args ugen)))))

(defn- auto-rate-setter [spec ugen]
  (if (= :auto (:rate ugen))
    (let [arg-rates (ugen-arg-rates ugen)
          fastest-rate (first (reverse (sort-by UGEN-RATE-SPEED arg-rates)))
          new-rate (get RATES (or fastest-rate :ir))]
      (assoc ugen :rate new-rate :rate-name (REVERSE-RATES new-rate)))
    ugen))

(defn- with-ugen-metadata-init
  "Calls init fn fun. Ifs init fn returns a map, merges it with the ugen
  otherwise if the result is a new arg list and simply assocs it to the ugen
  under the key :args, else throws an exception."
  [spec fun ugen]
  (let [rate (:rate ugen)
        args (:args ugen)
        new-args (fun rate args spec)]
    (cond
     (associative? new-args) (merge ugen new-args)
     (sequential? new-args) (assoc ugen :args new-args)
     :else (throw (Exception. (str "Unexpected return type from a ugen metadata init fn. Expected either a map or a list, got " new-args))))))

(defn- placebo-ugen-init-fn
  "The default ugen init fn (used if an :init key is not present in the ugen
  metadata). Simply returns the args unchanged."
  [rate args spec] args)

(defn- placebo-ugen-checker-fn
  "The default ugen checker n (used if a :check key is not present in the ugen
  metadata). Simply returns nil."
  [rate num-outs args ugen spec] nil)


(defn- with-debugging [f ugen]
  (when *debugging*
    (f ugen))
  ugen)

(defn- ugen-arg-info
  "Returns a string with debug information about the ugen's arguments"
  [spec ugen]
  (str "Supplied args: "
       (with-out-str (pr (:orig-args ugen)))
       "\nExpected arg keys: "
       (with-out-str (pr (spec-arg-names spec)))
       "\nMerged args: "
       (with-out-str (pr (:arg-map ugen)))
       (when *debugging*
         (str
          "\nFinal arglist: "
          (with-out-str (pr (:args ugen)))))))

(defn- with-ugen-checker-fn
  "Calls the checker fn. If checker fn returns a string, throws an exception
  using the string as a message. Otherwise returns ugen unchanged. If the
  checker fn is a list, it will assume it's a list of fns and will call all of
  them. If any of the results are strings it will concatanate them to produce
  a list of errors separated with AND."
  [spec fun ugen]
  (let [rate (:rate ugen)
        args (:args ugen)
        num-outs (:n-outputs ugen)

        result (if (sequential? fun)
                 (let [results (map #(% rate num-outs args ugen spec) fun)]
                   (if (some string? results)
                     (reduce (fn [s el]
                               (if (string? el)
                                 (if (empty? s)
                                   el
                                   (str s "\nAND\n" el))
                                 s))
                             ""
                             results)
                     nil))
                 (fun rate num-outs args ugen spec))]

    (when (string? result)
      (let [error-message (str "Error in checker for ugen ==> "
                               (overtone-ugen-name (:name spec))
                               "\n"
                               result
                               "\n"
                               (ugen-arg-info spec ugen)
                               (when *debugging*
                                 (str
                                  "\n\nUgen:\n"
                                  (with-out-str (pprint (simplify-ugen ugen))))))]
        (if *checking*
          (throw (IllegalArgumentException. error-message))
          (println error-message))))
    ugen))

(defn- check-arg-rates [spec ugen]
  (let [cur-rate (REVERSE-RATES (:rate ugen))
        ugen-args (filter sc-ugen? (:args ugen))]
    (when-let [bad-input (some
                          (fn [ug]
                            (if (< (UGEN-RATE-SPEED cur-rate)
                                   (UGEN-RATE-SPEED (get REVERSE-RATES (:rate ug))))
                              ug false))
                          ugen-args)]
      ;;special cases
      (when-not (or
                 ;; Special case the a2k ugen
                 (and (= "A2K" (:name ugen))
                      (= :ar (:rate-name bad-input)))
                 ;; Special case the FFT ugen which may have ar ugens plugged into it
                 (and (= "FFT" (:name ugen))
                      (= :ar (:rate-name bad-input)))
                 ;; Special case demand rate ugens which may have kr ugens plugged into them
                 (and (= :dr cur-rate)
                      (= :kr (:rate-name bad-input)))
                 ;; Special case Amplitude ugen which may have ar ugens plugged into it
                 (and (= "Amplitude" (:name ugen))
                      (= :ar (:rate-name bad-input)))
                 ;; Special case Coyote ugen which may have ar ugens plugged into it
                 (and (= "Coyote" (:name ugen))
                      (= :ar (:rate-name bad-input)))
                 ;; Special case Pitch ugen which may have ar ugens plugged into it
                 (and (= "Pitch" (:name ugen))
                      (= :ar (:rate-name bad-input)))

                 ;; Special case LocalBuf which may have kr ugens plugged in
                 ;; but further modifications aren't honoured
                 (and (= "LocalBuf" (:name ugen))
                      (= :kr (:rate-name bad-input))))

        (let [ugen-name     (real-ugen-name ugen)
              in-name       (real-ugen-name bad-input)
              cur-rate-name (get HUMAN-RATES cur-rate)
              in-rate-name  (get HUMAN-RATES (:rate-name bad-input))]
          (throw (Exception.
                  (format "Invalid ugen rate.  The %s ugen is %s rate, but it has a %s input ugen running at the faster %s rate.  Besides special cases, the a2k ugen and demand rate ugens (which are allowed kr inputs), all ugens must be the same speed or faster than their inputs."
                          ugen-name cur-rate-name
                          in-name in-rate-name))))))
    ;;simply return the ugen if there's no problem with rates
    ugen))

(defn- ensure-num-out-arg-is-number!
  [val ug]
  (when-not (number? val)
    (throw (IllegalArgumentException. (str "Argument for ugen " (:name ug) " must be a number, yet found: " (with-out-str (pr val)))))))

(defn- with-num-outs-mode [spec ugen]
  (let [args-specs    (args-with-specs (:args ugen) spec :mode)
        [args n-outs] (reduce (fn [[args n-outs] [arg mode]]
                                (if (= :num-outs mode)
                                  (do
                                    (ensure-num-out-arg-is-number! arg ugen)
                                    [args arg])
                                  [(conj args arg) n-outs]))
                              [[] (:n-outputs ugen)]
                              args-specs)]
    (assoc ugen
      :n-outputs n-outs
      :args args)))

(defn- append-seq-args
  "Handles argument modes :append-sequence and :append-sequence-set-num-outs,
  and :append-string  where some ugens take a seq or string for one argument
  which needs to be appended to the end of the argument list when sent to SC.
  (and in the case of strings need to be converted to a list of char ints)"
  [spec ugen]
  (let [args-specs     (args-with-specs (:args ugen) spec :mode)
        pred           #(ugen-sequence-mode? (second %))
        normal-args    (map first (remove pred args-specs))
        to-append      (filter pred args-specs)
        intify-strings (map (fn [[arg spec]]
                              (if (= :append-string spec)
                                (if (or (string? arg)
                                        (keyword? arg))
                                  [(cons (count (name arg)) (map int (name arg))) spec]
                                  (throw (IllegalArgumentException.
                                          (str "The following param: " arg " passed to ugen " (:name ugen) " should either be a string or a keyword" ))))
                                [arg spec]))
                            to-append)
        to-append-args (map first intify-strings)
        args           (flatten (concat normal-args to-append-args))
        ugen           (assoc ugen :args args)]
    (if-let [n-outs-arg (first (filter #(= :append-sequence-set-num-outs (second %))
                                       to-append))]
      (assoc ugen :n-outputs (count (flatten [(first n-outs-arg)])))
      ugen)))

;;TODO check to see if this can be removed. Args can not take keywords as vals
(defn- map-ugen-args
  "Perform argument mappings for ugen args that take a keyword but need to be
  looked up in a map supplied in the spec. (e.g. envelope actions)"
  [spec ugen]
  (let [args (:args ugen)
        args-specs (args-with-specs args spec :map)
        mapped-args (map (fn [[arg map-val]] (if (and (map? map-val)
                                                     (keyword? arg))
                                              (arg map-val)
                                              arg))
                         args-specs)]
    (assoc ugen :args mapped-args)))

(defn- with-floated-args [spec ugen]
  (assoc ugen :args (floatify (:args ugen))))

(defn- with-rates
  "Add the default ugen rates to any ugen that doesn't explicitly set it."
  [spec]
  (assoc spec :rates (get spec :rates UGEN-DEFAULT-RATES)))

(defn- with-default-rate
  "Calculates the default rate which will be used when the rate isn't explicitly
  used in the fn name (i.e. ugen:kr) or if :ir is available in the rate options"
  [spec]
  (let [rates (:rates spec)
        rate (cond
              (contains? spec :default-rate) (:default-rate spec)
              (= 1 (count rates)) (first rates)
              :default (first (filter rates
                                      UGEN-DEFAULT-RATE-PRECEDENCE)))
        rate (if (or (= :ir rate) (:auto-rate spec))
               :auto
               rate)]
    (assoc spec :default-rate rate)))

(defn- with-categories
  "Adds a :categories attribute to a ugen-spec for later use in documentation
  GUI and REPL interaction."
  [spec]
  (assoc spec :categories (get UGEN-CATEGORIES (overtone-ugen-name (:name spec)) [])))

(defn- with-expands
  "Sets the :expands? attribute for ugen-spec arguments, which will inform the
  automatic channel expansion system when to expand argument."
  [spec]
  (assoc spec :args
         (map (fn [arg]
                (let [expands? (if (:array arg)
                                 false
                                 (get UGEN-SPEC-EXPANSION-MODES
                                      (get arg :mode :standard)))]
                  (assoc arg :expands? expands?)))
              (:args spec))))

(defn- nil-arg-checker-fn
  [rate num-outs inputs ugen spec]
  (let [args (:args ugen)]
    (if (some nil? args)
      (str "Error - attempted to call the " (:name ugen) " ugen with one or more nil arguments. This usually happens when the ugen contains arguments without defaults which haven't been explicitly called. \nUgen:\n" (ugen-arg-info spec ugen))
      ugen)))

(defn- sanity-checker-fn
  "Ensure all inputs are either a number or a gen. Return an error string if not"
  [rate num-outs inputs ugen spec]
  (when (some #(and (not (number? %))
                    (not (sc-ugen? %)))
              inputs)
    (str "Error: after initialisation, not all inputs to this ugen were numbers or other ugens (inputs which are explicitly allowed to be other data types (i.e strings) will have been converted to numbers at this point): " (vec inputs))))

(defn- arg-name-checker-fn
  "Ensure that no extra arguments or arguments with unknown keys are
   passed to a given ugen"
  [rate num-outs inputs ugen spec]
  (let [valid-arg-keys      (spec-arg-names spec)
        extra-args          (difference (into #{} (keys (:arg-map ugen)))
                                        (into #{} valid-arg-keys))
        nil-present?        (contains? extra-args nil)
        extra-args-sans-nil (filter identity extra-args)
        error1              (if nil-present?
                              (str "You supplied too many arguments. ")
                              "")
        error2              (if-not (empty? extra-args-sans-nil)
                              (str "You supplied the following unexpected keys: " (apply str (interpose ", " extra-args-sans-nil)))
                              "")
        errors              (str error1 error2)]
    (when-not (empty? errors)
      errors)))

(defn- print-args-pre-processing [spec ugen]
  (let [ug-name (overtone-ugen-name (:name spec))]
    (println "==== Pre-Processing =====")
    (println "Ugen " ug-name )
    (println "Args: " (with-out-str (pr (:args ugen))))
    (println "=========================\n")))

(defn- print-args-post-processing [spec ugen]
  (let [ug-name (overtone-ugen-name (:name spec))]
    (println "==== Post-Processing ====")
    (println "Ugen " ug-name )
    (println (ugen-arg-info spec ugen))
    (println "=========================\n")))

(defn- with-init-fn
  "Creates the final argument initialization function which is applied to
  arguments at runtime to do things like re-ordering and automatic filling in
  of arguments. Typically appending input arrays as the last argument and
  filling in the number of in or out channels for those ugens that need it.

  If an init function is already present it will get called after doing the
  mapping and mode transformations."
  [spec]
  (let [defaulter        (partial add-default-args spec)
        mapper           (partial map-ugen-args spec)
        init-fn          (if (contains? spec :init)
                           (:init spec)
                           placebo-ugen-init-fn)
        initer           (partial with-ugen-metadata-init spec init-fn)
        n-outputer       (partial with-num-outs-mode spec)
        floater          (partial with-floated-args spec)
        appender         (partial append-seq-args spec)
        auto-rater       (partial auto-rate-setter spec)
        rate-checker     (partial check-arg-rates spec)
        checker-fn       (if (contains? spec :check)
                           (:check spec)
                           placebo-ugen-checker-fn)
        nil-arg-checker  (partial with-ugen-checker-fn spec nil-arg-checker-fn)
        bespoke-checker  (partial with-ugen-checker-fn spec checker-fn)
        sanity-checker   (partial with-ugen-checker-fn spec sanity-checker-fn)
        arg-name-checker (partial with-ugen-checker-fn spec arg-name-checker-fn)]

    (assoc spec :init

           (fn [ugen]
             (->> ugen
                  (with-debugging (partial print-args-pre-processing spec))
                  defaulter
                  mapper
                  initer
                  n-outputer
                  floater
                  appender
                  auto-rater
                  nil-arg-checker
                  bespoke-checker
                  associative->id
                  rate-checker
                  sanity-checker
                  arg-name-checker
                  (with-debugging (partial print-args-post-processing spec)))))))

(defn- with-fn-names
  "Generates all the function names for this ugen and adds a :fn-names map
  that maps function names to rates, representing the output rate.

  All available rates get an explicit function name of the form <fn-name>:<rate>
  like this:
  * (env-gen:ar ...)
  * (env-gen:kr ...)

  UGens will also have a base-name without a rate suffix that uses the default
  rate. If the ugen spec contains the key :internal-name with a true value,
  the base-name will contain the prefix internal: This is to allow cgens with
  the same name to subsume the role of a specific ugen whilst allowing it to
  reference the original via the prefixed name."
  [spec]
  (let [rates (:rates spec)
        rate-vec (vec rates)
        base-name (overtone-ugen-name (:name spec))
        internal-name? (:internal-name spec)
        base-name (if internal-name? (str "internal:" base-name) base-name)
        base-rate (:default-rate spec)
        name-rates (zipmap (map #(str base-name %) rate-vec)
                           rate-vec)]
    (assoc spec
      :fn-names (assoc name-rates base-name base-rate))))

(defn- decorate-ugen-spec
  "Interpret a ugen-spec and add in additional, computed meta-data."
  [spec]
  (-> spec
      (with-rates)
      (with-categories)
      (with-expands)
      (with-init-fn)
      (with-default-rate)
      (with-fn-names)
      (doc/with-arg-defaults)
      (doc/with-full-doc)))

(defn- derived?
  "Determines whether the supplied ugen spec is derived from another ugen spec.

   This means that the ugen needs to inherit some properties from its parent.
   (The ugen spec's parent is specified using the key :extends)"
  [spec]
  (contains? spec :extends))

(defn- derive-ugen-specs
  "Merge the specified ugen spec maps to give children their parent's attributes
   by recursively reducing the specs to support arbitrary levels of derivation."
  ([specs] (derive-ugen-specs specs {} 0))
  ([children adults depth]
     ;; Make sure a bogus UGen doesn't spin us off into infinity... ;-)
     {:pre [(< depth 8)]}

     (let [[adults children]
           (reduce (fn [[full-specs new-children] spec]
                     (if (derived? spec)
                       (if (contains? full-specs (:extends spec))
                         [(assoc full-specs (:name spec)
                                 (merge (get full-specs (:extends spec)) spec))
                          new-children]
                         [full-specs (conj new-children spec)])
                       [(assoc full-specs (:name spec) spec) new-children]))
                   [adults []]
                   children)]
       (if (empty? children)
         (vals adults)
         (recur children adults (inc depth))))))

(defn- load-ugen-specs [namespaces]
  "Perform the derivations and setup defaults for rates, names
  argument initialization functions, and channel expansion flags."
  (let [specs   (specs-from-namespaces namespaces)
        derived (derive-ugen-specs specs)]
    (map decorate-ugen-spec derived)))


(defn- op-rate
  "Lookup the rate of an input ugen, otherwise use IR because the operand
  must be a constant float."
  [arg]
  (if (sc-ugen? arg)
    (:rate arg)
    (get RATES :ir)))


(defn- inf? [obj]
  (:infinite-sequence (meta obj)))

(defn- expandable? [arg]
  (sequential? arg))

(defn- multichannel-expand
  "Does sc style multichannel expansion.
  * does not expand seqs flagged infinite
  * note that this returns a list even in the case of a single expansion

  Takes expand-flags, a seq of boolean values representing whether a given arg
  should be expanded. Each nth expand-flag boolean corresponds to each nth arg
  where arg is a seq of arguments passed into the ugen fn.
  "
  [expand-flags args]
  (if (zero? (count args))
    [[]]
    (let [gc-seqs (fn [[gcount seqs flags] arg]
                    (cond
                     ;; Infinite seqs can be used to generate values for expansion
                     (inf? arg) [gcount
                                 (conj seqs arg)
                                 (next flags)]

                     ;; Regular, non-infinite and non-map collections get expanded
                     (and (expandable? arg)
                          (first flags)) [(max gcount (count arg))
                                          (conj seqs (cycle arg))
                                          (next flags)]

                          :else ;; Basic values get used for all expansions
                          [gcount
                           (conj seqs (repeat arg))
                           (next flags)]))
          [greatest-count seqs _] (reduce gc-seqs [1 [] expand-flags] args)]
      (take greatest-count (parallel-seqs seqs)))))

(defn- ugen-base-fn [spec rate special]
  (fn [& args]
    (mk-scugen spec rate special args)))

(defn- make-ugen
  "Create a callable map representing a ugen spec and fn for creating a
   sc-ugen (a datastructure representing a specific instantiation of a
   ugen given a set of arguments)."
  [spec rate ugen-fn]
  (callable-map {:name       (overtone-ugen-name (:name spec))
                 :summary    (:summary spec)
                 :doc        (:doc spec)
                 :full-doc   (:full-doc spec)
                 :categories (:categories spec)
                 :rate       rate
                 :src        "Implemented in C code"
                 :type       ::ugen
                 :params     (:args spec)}
                ugen-fn))

(defn- make-ugen-fn
  "Make a function representing the given ugen that will fill in default
  arguments, rates, etc."
  [spec rate special]
  (let [expand-flags (map #(:expands? %) (:args spec))]
    (idify-args
     (unwrap-map-arg
      (make-expanding
       (ugen-base-fn spec rate special) expand-flags)))))

;; TODO: Figure out the complete list of control types
;; This is used to determine the controls we list in the synthdef, so we need
;; all ugens which should be specified as external controls.


(defn- args-list-is-a-map?
  "Returns true if args list contains one element which is a map"
  [args]
  (and (= (count args) 1)
       (map? (first args))))

(defn- foldable-binary-ugen?
  "Is this binary ugen foldable? i.e. should it be allowed to take more than
  two arguments, resulting in a folded nesting of linked ugens. Ugen must be
  defined as being foldable and the args must not be a map or a combination of
  ordered params keyword args"
  [ug-name args]
  (and
   (FOLDABLE-BINARY-OPS (str ug-name))
   (not (args-list-is-a-map? args))
   (not (some keyword? args)))  )

(defn- unary-compatible-binary-ugen?
  [ugen-name args]
  (and (= 1 (count args)) (contains? binary-op-unary-modes (str ugen-name))))

(defn- binary-ugen->unary
  [ugen-name ugen-fn args]
  (let [unary-impl (get binary-op-unary-modes (str ugen-name))]
    (unary-impl ugen-fn (first args))))

(defn- foldable-binary-ugen
  "Create a foldable binary ugen - a binary ugen which may accept more than two
  args - each additional arg will result in an extra ugen added/folded into the
  chain of ugens."
  [ugen-name ugen-fn args]
  (when (< (count args) 2)
      (throw (IllegalArgumentException. (str "Attempted to call foldable binary op ugen with fewer than 2 args (" (count args) "). You passed: " [args] ))))
  (let [x    (first args)
        y    (second args)
        more (drop 2 args)]
    (reduce ugen-fn (ugen-fn x y) more)))

(defn- mk-overloaded-ugen-fn
  "Returns a fn which implements an overloaded binary ugen. Checks whether the
  ugen is foldable and the returning fn either implements folding or ensures
  that there are only 2 params."
  [ugen-name ugen-fn]
  (fn [& args]
    (cond
      (unary-compatible-binary-ugen? ugen-name args) (binary-ugen->unary ugen-name ugen-fn args)
      (foldable-binary-ugen? ugen-name args) (foldable-binary-ugen ugen-name ugen-fn args)
      :else (apply ugen-fn args))))

(defn- mk-multi-ugen-fn
  "Create a fn which representing an overloaded ugen which checks its args to
  see which fn to call - the original or the ugen. As a convenience, if the
  final arg is :force-ugen then the ugen fn is always called."
  [ugen-name overloaded-fn original-fn]
  (fn [& args]
    (let [force-ugen? (and (sequential? args)
                           (= :force-ugen (last args)))
          args        (if force-ugen?
                        (drop-last args)
                        args)]
      (if (or (treat-as-ugen? ugen-name args)
              force-ugen?)
        (apply overloaded-fn args)
        (apply original-fn args)))))

(defn- overload-ugen-op
  "Overload the binary op by placing the overloaded fn definition in a separate
  namespace. This overloaded fn will check incoming args on application to
  determine whether the original fn or overloaded fn should be called. The
  overloaded fns are then made available through the use of the macro
  with-overloaded-ugens"
  [src-ns target-ns ugen-name ugen-fn kind]
  (let [original-fn   (ns-resolve src-ns ugen-name)
        overload-name (if (= '/ ugen-name) 'binary-div-op ugen-name)
        ugen-name-str (str ugen-name)
        overloaded-fn (case kind
                        :unary (mk-overloaded-ugen-fn ugen-name-str ugen-fn)
                        :binary (mk-overloaded-ugen-fn ugen-name-str ugen-fn))]
    (swap! overloaded-ugens* assoc ugen-name overload-name)
    (ns-unmap target-ns overload-name)
    (intern target-ns overload-name (mk-multi-ugen-fn ugen-name-str overloaded-fn original-fn))))

(defn def-ugen
  "Create and intern a set of functions for a given ugen-spec.
    * base name function using default rate and no suffix (e.g. env-gen )
    * base-name plus rate suffix functions for each rate (e.g. env-gen:ar,
      env-gen:kr)"
  [to-ns spec special]
  (let [spec (decorate-ugen-spec spec)
        metadata {:doc (:full-doc spec)
                  :arglists (list (vec (map #(symbol (:name %))
                                            (:args spec))))}
        ugen-fns (map (fn [[uname rate]] [(with-meta (symbol uname) metadata)
                                            (make-ugen
                                             spec
                                             rate
                                             (make-ugen-fn spec rate special))])
                      (:fn-names spec))]
    (doseq [[ugen-name ugen-fn] ugen-fns]
      (intern to-ns ugen-name ugen-fn))))

(defn- add-extra-collider-info
  "Add information about colliding ugens to a spec's documentation"
  [doc-spec collider op-name]
  (assoc doc-spec :doc
         (str
          (:doc doc-spec)
          "\n\nThis ugen's name collides with the existing fn " collider ". When calling this fn within a synth definition, " collider " will be called unless the argument list suggests that this is a ugen call. " ugen-collide-ns-str  "/" (str op-name) " will therefore only be called if the arg list is a single map or at least one of the args is a ugen and the rest consist only of numbers, sequentials, keywords and other ugens. "
          (when (NUMERICAL-CLOJURE-FNS (str op-name))
            "Also, as this fn has been labelled as numerical, it will also be treated as a ugen if any of the args are not numbers."))))

(defn- def-unary-op
  "def a unary op ugen (this is handled separately due to the fact that the
  unaryopugen represents multiple functionality represented by multple fns
  in overtone)."
  [to-ns op-name special]
  (let [ugen-name (symbol (overtone-ugen-name op-name))
        collider?    (ns-resolve to-ns ugen-name)
        normalized-n (normalize-ugen-name op-name)
        orig-spec (get UGEN-SPECS "unaryopugen")
        doc-spec  (get unaryopugen-docspecs normalized-n {})
        doc-spec     (if collider?
                       (add-extra-collider-info doc-spec collider? op-name)
                       doc-spec)
        full-spec (merge orig-spec doc-spec {:name op-name
                                             :categories [["Unary Operations"]]})
        full-spec (doc/with-full-doc full-spec)
        metadata  {:doc (:full-doc full-spec)
                   :arglists (list (vec (map #(symbol (:name %))
                                             (:args full-spec))))}

        ugen-name (with-meta ugen-name metadata)
        ugen-fn   (make-ugen-fn orig-spec :auto special)
        ugen      (make-ugen full-spec :auto ugen-fn)]

    (swap! special-op-specs* assoc normalized-n full-spec)
    (if collider?
      (overload-ugen-op to-ns ugen-collide-ns ugen-name ugen :unary)
      (intern to-ns ugen-name ugen))))

(defn- def-binary-op
  "def a binary op ugen (this is handled separately due to the fact that the
  binaryopugen represents multiple functionality represented by multple fns
  in overtone)."
  [to-ns op-name special]
  (let [
        ugen-name    (symbol (overtone-ugen-name op-name))
        collider?    (ns-resolve to-ns ugen-name)
        normalized-n (normalize-ugen-name op-name)
        orig-spec    (get UGEN-SPECS "binaryopugen")
        doc-spec     (get binaryopugen-docspecs normalized-n {})
        doc-spec     (if (FOLDABLE-BINARY-OPS (str op-name))
                       (assoc doc-spec :doc
                              (str (:doc doc-spec) "\n\nThis binary op ugen is foldable. i.e. may take multiple args and fold them into a tree of ugens."))
                       doc-spec)
        doc-spec     (if collider?
                       (add-extra-collider-info doc-spec collider? op-name)
                       doc-spec)
        full-spec    (merge orig-spec doc-spec {:name op-name
                                                :categories [["Binary Operations"]]})
        full-spec    (doc/with-full-doc full-spec)
        metadata     {:doc (:full-doc full-spec)
                      :arglists (list (vec (map #(symbol (:name %))
                                                (:args full-spec))))}

        ugen-name    (with-meta ugen-name metadata)
        ugen-fn      (make-ugen-fn orig-spec :auto special)
        ugen         (make-ugen full-spec :auto ugen-fn)]
    (swap! special-op-specs* assoc normalized-n full-spec)
    (if collider?
      (overload-ugen-op to-ns ugen-collide-ns ugen-name ugen :binary)
      (intern to-ns ugen-name ugen))))

;;ensure overloaded-ugens* is populated
