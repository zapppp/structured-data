(ns structured-data)

(defn do-a-thing [x]
  (let [doubleX (+ x x)]
    (Math/pow doubleX doubleX))
  )

(defn spiff [v]
  (+ (get v 0) (get v 2))
  )

(defn cutify [v]
  (conj v "<3")
  )

(defn spiff-destructuring [v]
    (let [[a b c] v]
      (+ a c))
  )

(defn point [x y]
  [x y])

(defn rectangle [bottom-left top-right]
  [bottom-left top-right])

(defn width [rectangle]
  (let [[[bottomLeftX bottomLeftY] [topRightX topRightY]] rectangle]
    (- topRightX bottomLeftX))
  )

(defn height [rectangle]
  (let [[[bottomLeftX bottomLeftY] [topRightX topRightY]] rectangle]
    (- topRightY bottomLeftY))
  )

(defn square? [rectangle]
    (if (== (- (width rectangle) (height rectangle)) 0) true false)
  )

(defn area [rectangle]
  (* (width rectangle) (height rectangle))
  )

(defn contains-point? [[[bottomLeftX bottomLeftY] [topRightX topRightY]] [x1 y1]]
  (if
    (and
      (<= bottomLeftX x1 topRightX)
      (<= bottomLeftY y1 topRightY)
      ) true false)
  )

(defn contains-rectangle? [outer inner]
  (let [[[x1 y1] [x2 y2]] inner]
      (if (and (contains-point? outer [x1 y1]) (contains-point? outer [x1 y2]) (contains-point? outer [x2 y1]) (contains-point? outer [x2 y2])) true false)
    )
  )

(defn title-length [book]
  (count (:title book))
  )

(defn author-count [book]
  (count (:authors book))
  )

(defn multiple-authors? [book]
    (if (> (count (:authors book)) 1) true false)
  )

(defn add-author [book new-author]
  (let [originalAuthors (:authors book)
        new (conj originalAuthors new-author)]
    (assoc book :authors new)
    )

  )

(defn alive? [author]
  (if (contains? author :death-year) false true)
  )

(defn element-lengths [collection]
  (map count collection)
  )

(defn second-elements [collection]
  (let [second (fn [v] (get v 1))]
    (map second collection))
  )

(defn titles [books]
  (map :title books)
  )

(defn monotonic? [a-seq]
  (cond
    (apply <= a-seq) true
    (apply >= a-seq) true
    :else false)
)

(defn stars [n]
  (apply str (repeat n "*"))
)

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem))
  )

(defn contains-duplicates? [a-seq]
  (if (== (count a-seq) (count (set a-seq))) false true)
  )

(defn old-book->new-book [book]
  (let [originalAuthors (:authors book)
        authorsSet (set originalAuthors)]
    (assoc book :authors authorsSet))
  )

(defn has-author? [book author]
  (contains? (:authors book) author)
  )

(defn authors [books]
  (let [bookAuthors (map :authors books)]
    (apply clojure.set/union bookAuthors))
)

(defn all-author-names [books]
  (let [authorNames (map :name (authors books))]
    (set authorNames))
  )

(defn author->string [author]
  (let [name (:name author)
        years (cond
                (contains? author :death-year) (str " (" (:birth-year author) " - " (:death-year author) ")")
                (contains? author :birth-year) (str " (" (:birth-year author) " - )")
                :else "")]
    (str name years))
  )

(defn authors->string [authors]
  (let [authorSequence (map author->string authors)]
    (apply str (interpose ", " authorSequence))
  ))

(defn book->string [book]
  (let [bookName (:title book)
        authorNames (:authors book)]
    (str bookName ", written by " (authors->string authorNames))
  ))

(defn books->string [books]
  (let [bookCount (count books)]
    (cond
      (== bookCount 0) "No books."
      (== bookCount 1) (str "1 book. " (apply str (map book->string books)) ".")
       :else (str bookCount " books. " (apply str (interpose ". " (map book->string books))) ".")))
  )

(defn books-by-author [author books]
  (filter (fn [b] (has-author? b author)) books)
  )

(defn author-by-name [name authors]
  (first (filter (fn [a] (= (:name a) name)) authors))
  )

(defn living-authors [authors]
  (filter (fn [a] (if (contains? a :death-year) false true)) authors)
  )

(defn has-a-living-author? [book]
  (if (empty? (living-authors (:authors book))) false true)
  )

(defn books-by-living-authors [books]
    (filter (fn [a] (has-a-living-author? a)) books)
  )

; %________%
