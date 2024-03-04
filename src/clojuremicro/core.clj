(ns clojuremicro.core)

(defn not-to-nor [expr]
  (let [arg (first (rest expr))]
    (list 'nor arg)))

(defn or-to-nor [expr]
  (list 'nor (cons 'nor (rest expr))))

(defn and-to-nor [expr]
  (cons 'nor (map #(list 'nor %) (rest expr))))

(defn nor-single [expr]
  (cond
    (and (list? expr) (= 'not (first expr)))
    (not-to-nor expr)
    (and (list? expr) (= 'or (first expr)))
    (or-to-nor expr)
    (and (list? expr) (= 'and (first expr)))
    (and-to-nor expr)
    :else expr))

(defn nor-convert [expr]
  (let [converted-expr (nor-single expr)]
    (if (seq? converted-expr)
      (map #(if (seq? %) (nor-convert %) %) converted-expr)
      converted-expr)))