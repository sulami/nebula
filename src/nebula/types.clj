(ns nebula.types)

(def int-types #{:i8 :i16 :i32 :i64 :i128})
(def uint-types #{:u8 :u16 :u32 :u64 :u128})
(def default-int-type :i32)

(def float-types #{:f8 :f16 :f32 :f64 :f128})
(def default-float-type :f32)

(def function-type {:arguments []
                    :return :f32})

(defn- kind->type
  [expression-kind]
  (get {:integer default-int-type
        :float default-float-type
        :nil :nil
        :boolean :boolean
        :keyword :keyword
        :symbol :symbol
        :string :string}
       expression-kind
       :error))

(def scalar-type? keyword?)

(defn- infer-type
  [expression]
  (->> expression
       :expression-kind
       kind->type
       (assoc expression :type)))

(defn infer-types
  "Attaches the `:type` to `expressions`."
  [expressions]
  (map infer-type expressions))
