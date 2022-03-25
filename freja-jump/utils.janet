(defn case-insensitive-peg
  [str]
  ~(sequence
     ,;(map (fn [c]
              ~(choice ,(string/ascii-upper (string/from-bytes c))
                       ,(string/ascii-lower (string/from-bytes c))))
            str)))

(comment

  (peg/match (case-insensitive-peg "cat") "CAT")
  #=>
  @[]

  (peg/match (case-insensitive-peg "cAT") "Cat")
  #=>
  @[]

  )

(defn search-peg
  ``
  Given a string `search`, returns a peg that finds start positions of
  that string.

  Matches by splitting `search` by spaces, and where each space was,
  anything matches.
  ``
  [search]
  (def parts (string/split " " search))
  (var parts-peg @[])
  #
  (loop [i :range [0 (length parts)]
         :let [p (in parts i)
               p-peg (case-insensitive-peg p)
               p2 (get parts (inc i))
               p2-peg (when p2
                        (case-insensitive-peg p2))]]
    (array/push parts-peg p-peg)
    (array/push parts-peg
                (if p2-peg
                  ~(any (if-not ,p2-peg 1))
                  ~(any 1))))
  #
  ~{:main (any (choice :parts
                       1))
    :parts (sequence (position)
                     ,;parts-peg)})

(comment

  (peg/match (search-peg "fi do") "fine dog")
  #=>
  @[0]

  (peg/match (search-peg "f f") "firefox")
  #=>
  @[0]

  )

(defn line-to-item
  [line]
  (->> line
       (peg/match
         ~(sequence "("
                    (any " ")
                    (choice "def"
                            "var")
                    (to " ")
                    (some " ")
                    # XXX: not handling things like (def [a b] [1 2])
                    (capture (some (if-not (set " \n") 1)))))
       first))

(comment

  (line-to-item "(def arcadia")
  # =>
  "arcadia"

  (line-to-item "(var brackeys")
  # =>
  "brackeys"

  (line-to-item "(defn- calvin")
  # =>
  "calvin"

  (line-to-item "(defmacro dunkey")
  # =>
  "dunkey"

  (line-to-item " (def erdos")
  # =>
  nil

  )

(defn enumerate-candidates
  [src]
  (def lines
    (string/split "\n" src))
  (def cands @[])
  #
  (for i 0 (length lines)
    (when-let [item (line-to-item (get lines i))]
      (array/push cands [i item])))
  #
  cands)

(comment

  (def src
    ``
    # XXX: for investigation
    (defn current-gb
      []
      (get-in state/editor-state [:stack 0 1 :editor :gb]))

    (var a 1)

    (def x 0)
    ``)

  (enumerate-candidates src)
  # =>
  '@[(1 "current-gb")
     (5 "a")
     (7 "x")]

  )

