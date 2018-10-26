(ns ecs-testbed.core
  (:require
    [clojure.core.async :as a]
    [clojure.set :as set]
    [clojure.spec.alpha :as s])
  (:import
    (java.util UUID)))

(s/def ::component any?)
(s/def ::component-id keyword?)
(s/def ::entity (s/map-of ::component-id ::component))
(s/def ::entity-id (partial instance? UUID))

(s/def :system/components (s/coll-of ::component-id))
(s/def :system/run (s/fspec
                     :args (s/cat :scene ::scene
                                  :entity ::entity)
                     :ret (s/tuple (s/nilable ::entity)
                                   (s/map-of ::entity-id
                                             ::entity))));;self-mutating
(s/def ::system (s/keys :req [:system/components :system/run]))
(s/def ::systems (s/coll-of ::system))
(s/def ::entities (s/map-of ::entity-id ::entity))
(s/def ::scene (s/keys :req [::systems ::entities]))

(s/def ::health number?)
(s/def ::mana number?)
(s/def ::spawner number?)

(s/def ::position (s/tuple number? number? number?))

(defn complete-system
  "Completes a system run function to never spawn things."
  [f]
  (fn [scene entity]
    [(f scene entity)
     {}]))

(def test-data {::systems
                  [{:system/components #{::health}
                    :system/run (complete-system
                                  (fn [scene entity]
                                    (when (pos? (dec (::health entity)))
                                      (update entity ::health dec))))} ;; runs for every entity in the scene that has a health component and that health value is positive
                   {:system/components #{::spawner}
                    :system/run (fn [scene entity]
                                  (if (pos? (::spawner entity))
                                    [(update entity ::spawner dec)
                                     {(UUID/randomUUID)
                                      {::enemy-health 5
                                       ::position (::position entity)}}]
                                    [entity {}]))}]
                ::entities
                  {#uuid "954a33c4-c389-48a9-afd3-d0020e482eb3";;Player
                    {::health 1
                     ::mana 42
                     ::position [0 0 0]}
                   #uuid "f080b4c3-8515-4247-a1f8-838761fd033b";;Enemy 1
                    {::enemy-health 17
                     ::spawner 1
                     ::position [4 10 0]}}})

;; After running apply-systems once over test-data
{#uuid "954a33c4-c389-48a9-afd3-d0020e482eb3"
 #:ecs-testbed.core{:health 6, :mana 42, :position [0 0 0]},
 #uuid "f080b4c3-8515-4247-a1f8-838761fd033b"
 #:ecs-testbed.core{:health 16, :mana 42, :position [4 10 0]}}

{#uuid "954a33c4-c389-48a9-afd3-d0020e482eb3"
 #:ecs-testbed.core{:health 6, :mana 42, :position [0 0 0]},
 #uuid "f080b4c3-8515-4247-a1f8-838761fd033b"
 #:ecs-testbed.core{:enemy-health 17, :mana 42, :position [4 10 0]}}

{#uuid "f080b4c3-8515-4247-a1f8-838761fd033b"
 #:ecs-testbed.core{:enemy-health 17, :mana 42, :position [4 10 0]}}

 ;;interesting stuff below
{#uuid "9270a488-1960-4451-af2c-795d365d0349"
 #:ecs-testbed.core{:enemy-health 5, :position [4 10 0]},
 #uuid "f080b4c3-8515-4247-a1f8-838761fd033b"
 #:ecs-testbed.core{:enemy-health 17, :spawner 0, :position [4 10 0]}}

 ;;Test Case below
#:ecs-testbed.core{
  :systems [#:system{:components #{:ecs-testbed.core/health},
                     :run identity}
            #:system{:components #{:ecs-testbed.core/spawner},
                     :run identity}]
  :entities {#uuid "06763617-fce2-4a88-96ba-7e3ee57ebef6"
             #:ecs-testbed.core{:enemy-health 5, :position [4 10 0]},
             #uuid "f080b4c3-8515-4247-a1f8-838761fd033b"
             #:ecs-testbed.core{:enemy-health 17, :spawner 0, :position [4 10 0]}}}

;;Expansion Below

(defn apply-systems
  [{::keys [systems entities] :as scene}]
  (assoc
    scene
    ::entities
    (into {}
      (filter (comp not nil? second)
        (mapcat (fn [[id entity]]
                  (let [[e spawns]
                          (reduce (fn [[entity spawns] system]
                                        (let [[new-entity new-spawns]
                                                (if (set/subset?
                                                      (:system/components system)
                                                      (set (keys entity)));;if an entity has the subset of components the system requires then run the system
                                                  ((:system/run system) scene  entity)
                                                  [entity {}])]
                                          [new-entity (merge spawns new-spawns)]))
                                  [entity {}]
                                  systems)]
                    (assoc spawns id e)))
             entities)))))

(defmacro defprefab
  [name & components]
  (let [components-map (into {} (map vec (partition 2 components)))
        components-keys (keys components-map)
        components-syms (repeatedly (count components-keys) gensym)
        components-vals (map (fn [sym val]
                               `(or ~sym ~val))
                             components-syms
                             (vals components-map))
        destructure-seq (zipmap components-syms components-keys)]
    `(defn ~name
       [& ~destructure-seq]
       [(UUID/randomUUID)
        ~(zipmap
           components-keys
           components-vals)])))

(defprefab player
  ::health 5
  ::position [0 0 0]
  ::symbol \@)

(defn -main
  "I don't do a whole lot ... YET."
  [& args]
  (first (drop 2 (iterate apply-systems test-data))))
