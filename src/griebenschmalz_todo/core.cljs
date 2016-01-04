(ns griebenschmalz-todo.core
  (:require
    [clojure.set :as set]
    [clojure.string :as str]
    [datascript.core :as d]
    [rum.core :as rum]
    [datascript.transit :as dt]
    [beicon.core :as s]
    [griebenschmalz-todo.dom :as dom]
    [griebenschmalz-todo.util :as u]
    [griebenschmalz.core :refer [start do!] :as g])
  (:require-macros
    [griebenschmalz-todo.core :refer [profile]]))

(enable-console-print!)

(def schema {:todo/tags    {:db/cardinality :db.cardinality/many}
             :todo/project {:db/valueType :db.type/ref}
             :todo/done    {:db/index true}
             :todo/due     {:db/index true}})

(declare render persist)

;; History

(def ^:const history-limit 10)

;;; ACTIONS

;; Entity with id=0 is used for storing auxilary view information
;; like filter value and selected group

(defn update-db-history
  [state {:keys [db-before db-after] :as tx-report}]
  (-> state
      (update
        :history (fn [h]
                   (-> h
                       (u/drop-tail #(identical? % db-before))
                       (conj db-after)
                       (u/trim-head history-limit))))
      (assoc :db db-after)))

(defn set-system-attrs
  [attrs {:keys [db] :as state}]
  (update-db-history state (d/with db
                                   (for [[attr value] attrs]
                                     (if value
                                       [:db/add 0 attr value]
                                       [:db.fn/retractAttribute 0 attr])))))

;; This transaction function swaps the value of :todo/done attribute.
;; Transaction funs are handy in situations when to decide what to do
;; you need to analyse db first. They deliver atomicity and linearizeability
;; to such calculations
(defn toggle-todo-tx [db eid]
  (let [done? (:todo/done (d/entity db eid))]
    [[:db/add eid :todo/done (not done?)]]))

(defn toggle-todo
  [eid {:keys [db] :as state}]
  (update-db-history state (d/with db [[:db.fn/call toggle-todo-tx eid]])))

(defn change-task-field
  [field value {:keys [db] :as state}]
  (assoc-in state [:todo field] value))

(defn add-todo
  [{:keys [db todo] :as state}]
  (let [project (:project todo)
        project-id (when project (u/e-by-av db :project/name project))
        project-tx (when (and project (nil? project-id))
                     [[:db/add -1 :project/name project]])
        entity (->> {:todo/text    (:text todo)
                     :todo/done    false
                     :todo/project (when project (or project-id -1))
                     :todo/due     (let [val (js/Date.parse (:due todo))]
                                     (when-not (js/isNaN val)
                                       (js/Date. val)))
                     :todo/tags    (:tags todo)}
                    (u/remove-vals nil?))
        tx-report (d/with db (concat project-tx [entity]))]
    (assoc (update-db-history state tx-report)
      :todo {:text    nil
             :project nil
             :due     nil
             :tags    nil})))

(defn reset-db
  [db state]
  (assoc state :db db))

;;; VIEWS
;; Keyword filter

(defn system-attr
  ([db attr]
   (get (d/entity db 0) attr))
  ([db attr & attrs]
   (mapv #(system-attr db %) (concat [attr] attrs))))

(rum/defc filter-pane [message-bus db]
          [:.filter-pane
           [:input.filter {:type        "text"
                           :value       (or (system-attr db :system/filter) "")
                           :on-change   (fn [_]
                                          (do! message-bus (partial set-system-attrs {:system/filter (dom/value (dom/q ".filter"))})))
                           :placeholder "Filter"}]])

;; Rules are used to implement OR semantic of a filter
;; ?term must match either :project/name OR :todo/tags
(def filter-rule
  '[[(match ?todo ?term)
     [?todo :todo/project ?p]
     [?p :project/name ?term]]
    [(match ?todo ?term)
     [?todo :todo/tags ?term]]])

;; terms are passed as a collection to query,
;; each term futher interpreted with OR semantic
(defn todos-by-filter [db terms]
  (d/q '[:find [?e ...]
         :in $ % [?term ...]
         :where [?e :todo/text]
         (match ?e ?term)]
       db filter-rule terms))

(defn filter-terms [db]
  (not-empty
    (str/split (system-attr db :system/filter) #"\s+")))

(defn filtered-db [db]
  (if-let [terms (filter-terms db)]
    (let [whitelist (set (todos-by-filter db terms))
          pred (fn [db datom]
                 (or (not= "todo" (namespace (:a datom)))
                     (contains? whitelist (:e datom))))]
      (d/filter db pred))
    db))

;; Groups

(defmulti todos-by-group (fn [db group item] group))

;; Datalog has no negative semantic (NOT IN), we emulate it
;; with get-else (get attribute with default value), and then
;; filtering by that attribute, keeping only todos that resulted
;; into default value
(defmethod todos-by-group :inbox [db _ _]
  (d/q '[:find [?todo ...]
         :where [?todo :todo/text]
         [(get-else $ ?todo :todo/project :none) ?project]
         [(get-else $ ?todo :todo/due :none) ?due]
         [(= ?project :none)]
         [(= ?due :none)]]
       db))

(defmethod todos-by-group :completed [db _ _]
  (d/q '[:find [?todo ...]
         :where [?todo :todo/done true]]
       db))

(defmethod todos-by-group :all [db _ _]
  (d/q '[:find [?todo ...]
         :where [?todo :todo/text]]
       db))

(defmethod todos-by-group :project [db _ pid]
  (d/q '[:find [?todo ...]
         :in $ ?pid
         :where [?todo :todo/project ?pid]]
       db pid))

;; Since todos do not store month directly, we pass in
;; month boundaries and then filter todos with <= predicate
(defmethod todos-by-group :month [db _ [year month]]
  (d/q '[:find [?todo ...]
         :in $ ?from ?to
         :where [?todo :todo/due ?due]
         [(<= ?from ?due ?to)]]
       db (u/month-start month year) (u/month-end month year)))

(rum/defc group-item [message-bus db title group item]
          ;; Joining DB with a collection
          (let [todos (todos-by-group db group item)
                count (d/q '[:find (count ?todo) .
                             :in $ [?todo ...]
                             :where [$ ?todo :todo/done false]]
                           db todos)]
            [:.group-item {:class (when (= [group item]
                                           (system-attr db :system/group :system/group-item))
                                    "group-item_selected")}
             [:span {:on-click (fn [_]
                                 (do! message-bus (partial set-system-attrs {:system/group      group
                                                                             :system/group-item item})))}
              title]
             (when count
               [:span.group-item-count count])]))

(rum/defc plan-group [message-bus db]
          [:.group
           [:.group-title "Plan"]
           ;; Here we’re calculating month inside a query via passed in function
           (for [[year month] (->> (d/q '[:find [?month ...]
                                          :in $ ?date->month
                                          :where [?todo :todo/due ?date]
                                          [(?date->month ?date) ?month]]
                                        db u/date->month)
                                   sort)]
             (group-item message-bus db (u/format-month month year) :month [year month]))])

(rum/defc projects-group [message-bus db]
          [:.group
           [:.group-title "Projects"]
           (for [[pid name] (->> (d/q '[:find ?pid ?project
                                        :where [?todo :todo/project ?pid]
                                        [?pid :project/name ?project]]
                                      db)
                                 (sort-by second))]
             (group-item message-bus db name :project pid))])

(rum/defc overview-pane [message-bus db]
          [:.overview-pane
           [:.group
            (group-item message-bus db "Inbox" :inbox nil)
            (group-item message-bus db "Completed" :completed nil)
            (group-item message-bus db "All" :all nil)]
           (plan-group message-bus db)
           (projects-group message-bus db)])

(rum/defc todo-pane [message-bus db]
          [:.todo-pane
           (let [todos (let [[group item] (system-attr db :system/group :system/group-item)]
                         (todos-by-group db group item))]
             (for [eid (sort todos)
                   :let [td (d/entity db eid)]]
               ^{:key (str eid)}
               [:.todo {:class (if (:todo/done td) "todo_done" "")}
                [:.todo-checkbox {:on-click #(do! message-bus (partial toggle-todo eid))} "✔︎"]
                [:.todo-text (:todo/text td)]
                [:.todo-subtext
                 (when-let [due (:todo/due td)]
                   [:span (.toDateString due)])
                 ;; here we’re using entity ref navigation, going from
                 ;; todo (td) to project to project/name
                 (when-let [project (:todo/project td)]
                   [:span (:project/name project)])
                 (for [tag (:todo/tags td)]
                   ^{:key tag} [:span tag])]]))])

(rum/defc add-view [message-bus {:keys [text project due tags] :as todo}]
          [:div.add-view #_{:on-submit (fn [_] (do! message-bus {:type ::add-todo}) false)}
           [:input.add-text {:type      "text" :placeholder "New task" :value text
                             :on-change #(do! message-bus (partial change-task-field :text (dom/value (dom/q ".add-text"))))}]
           [:input.add-project {:type      "text" :placeholder "Project" :value project
                                :on-change #(do! message-bus (partial change-task-field :project (dom/value (dom/q ".add-project"))))}]
           [:input.add-tags {:type      "text" :placeholder "Tags" :value tags
                             :on-change #(do! message-bus (partial change-task-field :tags (dom/value (dom/q ".add-tags"))))}]
           [:input.add-due {:type      "text" :placeholder "Due date" :value due
                            :on-change #(do! message-bus (partial change-task-field :due (dom/value (dom/q ".add-due"))))}]
           [:button.add-submit {:on-click (fn [_] (do! message-bus add-todo) false)} "Add Task"]])

(rum/defc history-view [message-bus {:keys [db todo history] :as state}]
          [:.history-view
           (for [state history]
             [:.history-state
              {:class    (when (identical? state db) "history-selected")
               :on-click (fn [_] (do! message-bus (partial reset-db state)))}])
           (if-let [prev (u/find-prev history #(identical? db %))]
             [:button.history-btn {:on-click (fn [_] (do! message-bus (partial reset-db prev)))} "‹ undo"]
             [:button.history-btn {:disabled true} "‹ undo"])
           (if-let [next (u/find-next history #(identical? db %))]
             [:button.history-btn {:on-click (fn [_] (do! message-bus (partial reset-db next)))} "redo ›"]
             [:button.history-btn {:disabled true} "redo ›"])])

(rum/defc canvas [message-bus {:keys [db todo] :as state}]
          [:.canvas
           [:.main-view
            (filter-pane message-bus db)
            (let [db (filtered-db db)]
              (list
                ^{:key "1"} (overview-pane message-bus db)
                ^{:key "2"} (todo-pane message-bus db)))]
           (add-view message-bus todo)
           (history-view message-bus state)])

(defn render
  ([message-bus state]
   (profile "render"
            (rum/mount (canvas message-bus state) (.querySelector js/document "div#todo")))))

;; logging of all transactions (prettified)
#_(d/listen! conn :log
             (fn [tx-report]
               (let [tx-id (get-in tx-report [:tempids :db/current-tx])
                     datoms (:tx-data tx-report)
                     datom->str (fn [d] (str (if (:added d) "+" "−")
                                             "[" (:e d) " " (:a d) " " (pr-str (:v d)) "]"))]
                 (println
                   (str/join "\n" (concat [(str "tx " tx-id ":")] (map datom->str datoms)))))))

;; transit serialization

(defn db->string [db]
  (profile "db serialization"
           (dt/write-transit-str db)))

(defn string->db [s]
  (profile "db deserialization"
           (dt/read-transit-str s)))

;; persisting DB between page reloads
(defn persist [db]
  (js/localStorage.setItem "datascript-todo/DB" (db->string db)))

#_(d/listen! conn :persistence
             (fn [tx-report]                                ;; FIXME do not notify with nil as db-report
               ;; FIXME do not notify if tx-data is empty
               (when-let [db (:db-after tx-report)]
                 (js/setTimeout #(persist db) 0))))

;; restoring once persisted DB on page load
(def initial-state
  (let [db (or
             (when-let [stored (js/localStorage.getItem "datascript-todo/DB")]
               (let [stored-db (string->db stored)]
                 (when (= (:schema stored-db) schema)       ;; check for code update
                   stored-db)))
             (d/db-with (d/empty-db schema) u/fixtures))]
    {:db      db
     :todo    {:text    nil
               :project nil
               :due     nil
               :tags    nil}
     :history [db]}))

#_(js/localStorage.clear)

;; for interactive re-evaluation
(start initial-state render)



