(import freja/new_gap_buffer :as gb)
(import freja/render_new_gap_buffer :as rgb)
(import freja/events :as e)
(import freja/hiccup :as h)
(import freja/state)
(import freja/theme)
(import freja/textarea :as ta)

(import ./utils :as u)

(defn jump-list-component
  [props]
  (def {:candidates candidates
        :cleanup cleanup
        :gb gb
        :input input
        :offset offset}
    props)
  #
  (default offset 0)
  #
  (default input "")
  #
  (defn confirm
    [line]
    (rgb/goto-line-number gb (inc line))
    (cleanup))
  #
  (def peg
    (if (or (nil? input)
            (empty? input))
      ~(capture (any 1))
      (u/search-peg input)))
  #
  (def filtered-candidates
    (filter |(not (empty? (peg/match peg (get $ 1))))
            candidates))
  #
  (def offset
    (-> offset
        (max 0)
        (min (dec (length filtered-candidates)))))
  #
  (def selected-candidate
    (get filtered-candidates offset))
  (def selected-line (get selected-candidate 0))
  (def selected-item (get selected-candidate 1))
  #
  [:padding {:all 0}
   [:column {}
    [:block {:weight 0.05}]
    [:row {}
     [:block {:weight 0.5}]
     [:block {:weight 1}
      [:clickable {:on-click
                   # only done to stop clicks from passing through
                   (fn [_])}
       [:background {:color (theme/comp-cols :background)}
        [:padding {:all 4}
         [:block {} [:text {:size 24
                            :color (theme/comp-cols :text/color)
                            :text "Jump to"}]]
         [:padding {:top 6 :bottom 6}
          [ta/textarea
           @{:extra-binds
             @{:escape (fn [_] (cleanup))
               :down (fn [_]
                       (let [new (inc offset)
                             new (if (>= new (length filtered-candidates))
                                   0
                                   new)]
                         (e/put! props :offset new)))
               :up (fn [_]
                     (let [new (dec offset)
                           new (if (< new 0)
                                 (dec (length filtered-candidates))
                                 new)]
                             (e/put! props :offset new)))
               :enter (fn [_]
                        (confirm selected-line))}
             :height 22
             :init (fn [self _]
                     (e/put! state/focus :focus (self :state)))
             :on-change |(e/put! props :input $)
             :text/color :white
             :text/size 20}]]
         [:background {:color (theme/comp-cols :bar-bg)}
          ;(seq [cand :in filtered-candidates
                 :let [[line item] cand
                       selected (deep= cand selected-candidate)]]
             [:clickable {:on-click (fn [_] (confirm line))}
              (if selected
                [:background {:color 0xffffff99}
                 [:block {}
                  [:padding {:all 2}
                   [:text {:color 0x111111ff
                           :size 16
                           :text (or selected-item item)}]]]]
                [:block {}
                 [:padding {:all 2}
                  [:text {:text item
                          :size 16
                          :color :white}]]])])]]]]]
     [:block {:weight 0.5}]]
    [:block {:weight 1}]]])

(defn make-cleanup-fn
  [layer-name]
  (def editor-state
    (state/focus :focus))
  #
  (fn []
    (h/remove-layer layer-name nil)
    # restore focus
    (e/put! state/focus :focus editor-state)))

(varfn jump-list
  [gb]
  (def src
    (gb/content gb))
  #
  (def layer-name :candidates)
  #
  (h/new-layer layer-name
               jump-list-component
               @{:candidates (u/enumerate-candidates src)
                 # XXX: if remove-layer starts using 2nd arg, put in component?
                 :cleanup (make-cleanup-fn layer-name)
                 :gb gb})
  #
  gb)

(varfn jump
  [gb]
  (def caret (gb :caret))
  (def chars
    {(chr " ") 1
     (chr "\t") 1
     (chr "\n") 1
     (chr "(") 1
     (chr ")") 1
     (chr "{") 1
     (chr "}") 1
     (chr "[") 1
     (chr "]") 1
     (chr `"`) 1
     (chr "'") 1
     (chr "`") 1})
  (def start
    (gb/search-backward gb
                        (fn [c] (get chars c))
                        caret))
  (when (>= start caret)
    (eprintf "%p not less than %p" start caret)
    (break gb))
  (def end
    (gb/search-forward gb
                       (fn [c] (get chars c))
                       caret))
  #
  (def input
    (gb/gb-slice gb start end))
  #
  (def src
    (gb/content gb))
  #
  (def candidates
    (u/enumerate-candidates src))
  #
  (def filtered
    (filter |(string/has-prefix? (string input)
                                 (get $ 1))
            candidates))
  #
  (def layer-name :candidates)
  #
  (cond
    (and (= 1 (length filtered))
         (= (string input)
            (get-in filtered [0 1])))
    (rgb/goto-line-number gb (inc (get-in filtered [0 0])))
    #
    (< 1 (length filtered))
    (h/new-layer layer-name
                 jump-list-component
                 @{:candidates filtered
                   # XXX: if remove-layer starts using 2nd arg, put in component?
                   :cleanup (make-cleanup-fn layer-name)
                   :gb gb
                   :input input})
    #
    (eprintf "No candidates found"))
  #
  gb)
