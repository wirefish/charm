(in-package :mistmarsh)

#|

Sinkholes lead to an underground network of caves that connect otherwise
unreachable parts of the marsh.

The river is too wide and deep to cross without high swimming skill.

The creeping mist moves around the marsh, maybe only at night. It is a blob of
entities that move slowly but are always adjacent. In the mist, you cannot see
beyond the current location. Also, the mist spawns rare creatures including a
high-level boss-type creature if certain conditions are met.

The marsh is home to poisonous frogs and giant dragonflies on the surface, and
slimes/oozes in the caverns.

The section east of the river has a rare mini-boss type creature with special
loot.

The surface has some low-level herbs.

|#

(defregion mistmarsh)

(defproto swamp-portal (portal))

(defproto marshy-weeds (location)
  (surface 'grass))

(defproto stagnant-water (location)
  (surface 'shallow-water))

(defproto stunted-trees (location)
  (surface 'forest))

(defproto sinkhole (location)
  (surface 'dirt))

(defproto muddy-path (location)
  (surface 'dirt))

(defproto river (location)
  (surface 'shallow-wateR))

(deflocation marshy-weeds-2-0 (marshy-weeds)
  (exits ((swamp-portal :south marshy-weeds-2-1 :east marshy-weeds-3-0))))

(deflocation marshy-weeds-3-0 (marshy-weeds)
  (exits ((swamp-portal :south marshy-weeds-3-1 :west marshy-weeds-2-0 :east marshy-weeds-4-0))))

(deflocation marshy-weeds-4-0 (marshy-weeds)
  (exits ((swamp-portal :south marshy-weeds-4-1 :west marshy-weeds-3-0 :east stagnant-water-5-0))))

(deflocation stagnant-water-5-0 (stagnant-water)
  (exits ((swamp-portal :south stagnant-water-5-1 :west marshy-weeds-4-0 :east stagnant-water-6-0))))

(deflocation stagnant-water-6-0 (stagnant-water)
  (exits ((swamp-portal :south marshy-weeds-6-1 :west stagnant-water-5-0 :east marshy-weeds-7-0))))

(deflocation marshy-weeds-7-0 (marshy-weeds)
  (exits ((swamp-portal :south sinkhole-7-1 :west stagnant-water-6-0 :east marshy-weeds-8-0))))

(deflocation marshy-weeds-8-0 (marshy-weeds)
  (exits ((swamp-portal :south stunted-trees-8-1 :west marshy-weeds-7-0 :east marshy-weeds-9-0))))

(deflocation marshy-weeds-9-0 (marshy-weeds)
  (exits ((swamp-portal :south stunted-trees-9-1 :west marshy-weeds-8-0 :east marshy-weeds-10-0))))

(deflocation marshy-weeds-10-0 (marshy-weeds)
  (exits ((swamp-portal :south stunted-trees-10-1 :west marshy-weeds-9-0 :east marshy-weeds-11-0))))

(deflocation marshy-weeds-11-0 (marshy-weeds)
  (exits ((swamp-portal :south marshy-weeds-11-1 :west marshy-weeds-10-0 :east river-12-0))))

(deflocation river-12-0 (river)
  (exits ((swamp-portal :south river-12-1 :west marshy-weeds-11-0))))

(deflocation marshy-weeds-2-1 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-2-0 :south marshy-weeds-2-2 :east marshy-weeds-3-1))))

(deflocation marshy-weeds-3-1 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-3-0 :south marshy-weeds-3-2 :west marshy-weeds-2-1 :east marshy-weeds-4-1))))

(deflocation marshy-weeds-4-1 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-4-0 :south marshy-weeds-4-2 :west marshy-weeds-3-1 :east stagnant-water-5-1))))

(deflocation stagnant-water-5-1 (stagnant-water)
  (exits ((swamp-portal :north stagnant-water-5-0 :south stagnant-water-5-2 :west marshy-weeds-4-1 :east marshy-weeds-6-1))))

(deflocation marshy-weeds-6-1 (marshy-weeds)
  (exits ((swamp-portal :north stagnant-water-6-0 :south stunted-trees-6-2 :west stagnant-water-5-1 :east sinkhole-7-1))))

(deflocation sinkhole-7-1 (sinkhole)
  (exits ((swamp-portal :north marshy-weeds-7-0 :south stunted-trees-7-2 :west marshy-weeds-6-1 :east stunted-trees-8-1))))

(deflocation stunted-trees-8-1 (stunted-trees)
  (exits ((swamp-portal :north marshy-weeds-8-0 :south stunted-trees-8-2 :west sinkhole-7-1 :east stunted-trees-9-1))))

(deflocation stunted-trees-9-1 (stunted-trees)
  (exits ((swamp-portal :north marshy-weeds-9-0 :south stunted-trees-9-2 :west stunted-trees-8-1 :east stunted-trees-10-1))))

(deflocation stunted-trees-10-1 (stunted-trees)
  (exits ((swamp-portal :north marshy-weeds-10-0 :south marshy-weeds-10-2 :west stunted-trees-9-1 :east marshy-weeds-11-1))))

(deflocation marshy-weeds-11-1 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-11-0 :south marshy-weeds-11-2 :west stunted-trees-10-1 :east river-12-1))))

(deflocation river-12-1 (river)
  (exits ((swamp-portal :north river-12-0 :south river-12-2 :west marshy-weeds-11-1))))

(deflocation marshy-weeds-1-2 (marshy-weeds)
  (exits ((swamp-portal :south marshy-weeds-1-3 :east marshy-weeds-2-2))))

(deflocation marshy-weeds-2-2 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-2-1 :south marshy-weeds-2-3 :west marshy-weeds-1-2 :east marshy-weeds-3-2))))

(deflocation marshy-weeds-3-2 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-3-1 :south marshy-weeds-3-3 :west marshy-weeds-2-2 :east marshy-weeds-4-2))))

(deflocation marshy-weeds-4-2 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-4-1 :south marshy-weeds-4-3 :west marshy-weeds-3-2 :east stagnant-water-5-2))))

(deflocation stagnant-water-5-2 (stagnant-water)
  (exits ((swamp-portal :north stagnant-water-5-1 :south stagnant-water-5-3 :west marshy-weeds-4-2 :east stunted-trees-6-2))))

(deflocation stunted-trees-6-2 (stunted-trees)
  (exits ((swamp-portal :north marshy-weeds-6-1 :south stagnant-water-6-3 :west stagnant-water-5-2 :east stunted-trees-7-2))))

(deflocation stunted-trees-7-2 (stunted-trees)
  (exits ((swamp-portal :north sinkhole-7-1 :south stagnant-water-7-3 :west stunted-trees-6-2 :east stunted-trees-8-2))))

(deflocation stunted-trees-8-2 (stunted-trees)
  (exits ((swamp-portal :north stunted-trees-8-1 :south stunted-trees-8-3 :west stunted-trees-7-2 :east stunted-trees-9-2))))

(deflocation stunted-trees-9-2 (stunted-trees)
  (exits ((swamp-portal :north stunted-trees-9-1 :south muddy-path-9-3 :west stunted-trees-8-2 :east marshy-weeds-10-2))))

(deflocation marshy-weeds-10-2 (marshy-weeds)
  (exits ((swamp-portal :north stunted-trees-10-1 :south muddy-path-10-3 :west stunted-trees-9-2 :east marshy-weeds-11-2))))

(deflocation marshy-weeds-11-2 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-11-1 :south muddy-path-11-3 :west marshy-weeds-10-2 :east river-12-2))))

(deflocation river-12-2 (river)
  (exits ((swamp-portal :north river-12-1 :south river-12-3 :west marshy-weeds-11-2))))

(deflocation marshy-weeds-0-3 (marshy-weeds)
  (exits ((swamp-portal :south muddy-path-0-4 :east marshy-weeds-1-3))))

(deflocation marshy-weeds-1-3 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-1-2 :south muddy-path-1-4 :west marshy-weeds-0-3 :east marshy-weeds-2-3))))

(deflocation marshy-weeds-2-3 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-2-2 :south muddy-path-2-4 :west marshy-weeds-1-3 :east marshy-weeds-3-3))))

(deflocation marshy-weeds-3-3 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-3-2 :south muddy-path-3-4 :west marshy-weeds-2-3 :east marshy-weeds-4-3))))

(deflocation marshy-weeds-4-3 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-4-2 :south muddy-path-4-4 :west marshy-weeds-3-3 :east stagnant-water-5-3))))

(deflocation stagnant-water-5-3 (stagnant-water)
  (exits ((swamp-portal :north stagnant-water-5-2 :south muddy-path-5-4 :west marshy-weeds-4-3 :east stagnant-water-6-3))))

(deflocation stagnant-water-6-3 (stagnant-water)
  (exits ((swamp-portal :north stunted-trees-6-2 :south marshy-weeds-6-4 :west stagnant-water-5-3 :east stagnant-water-7-3))))

(deflocation stagnant-water-7-3 (stagnant-water)
  (exits ((swamp-portal :north stunted-trees-7-2 :south stagnant-water-7-4 :west stagnant-water-6-3 :east stunted-trees-8-3))))

(deflocation stunted-trees-8-3 (stunted-trees)
  (exits ((swamp-portal :north stunted-trees-8-2 :south marshy-weeds-8-4 :west stagnant-water-7-3 :east muddy-path-9-3))))

(deflocation muddy-path-9-3 (muddy-path)
  (exits ((swamp-portal :north stunted-trees-9-2 :south muddy-path-9-4 :west stunted-trees-8-3 :east muddy-path-10-3))))

(deflocation muddy-path-10-3 (muddy-path)
  (exits ((swamp-portal :north marshy-weeds-10-2 :south marshy-weeds-10-4 :west muddy-path-9-3 :east muddy-path-11-3))))

(deflocation muddy-path-11-3 (muddy-path)
  (exits ((swamp-portal :north marshy-weeds-11-2 :south marshy-weeds-11-4 :west muddy-path-10-3 :east river-12-3))))

(deflocation river-12-3 (river)
  (exits ((swamp-portal :north river-12-2 :south river-12-4 :west muddy-path-11-3 :east muddy-path-13-3))))

(deflocation muddy-path-13-3 (muddy-path)
  (exits ((swamp-portal :west river-12-3))))

(deflocation muddy-path-0-4 (muddy-path)
  (exits ((dirt-road :west arwyck::east-gate)
          (swamp-portal :north marshy-weeds-0-3 :south marshy-weeds-0-5 :east muddy-path-1-4))))

(deflocation muddy-path-1-4 (muddy-path)
  (exits ((swamp-portal :north marshy-weeds-1-3 :south marshy-weeds-1-5 :west muddy-path-0-4 :east muddy-path-2-4))))

(deflocation muddy-path-2-4 (muddy-path)
  (exits ((swamp-portal :north marshy-weeds-2-3 :south marshy-weeds-2-5 :west muddy-path-1-4 :east muddy-path-3-4))))

(deflocation muddy-path-3-4 (muddy-path)
  (exits ((swamp-portal :north marshy-weeds-3-3 :south marshy-weeds-3-5 :west muddy-path-2-4 :east muddy-path-4-4))))

(deflocation muddy-path-4-4 (muddy-path)
  (exits ((swamp-portal :north marshy-weeds-4-3 :south marshy-weeds-4-5 :west muddy-path-3-4 :east muddy-path-5-4))))

(deflocation muddy-path-5-4 (muddy-path)
  (exits ((swamp-portal :north stagnant-water-5-3 :south muddy-path-5-5 :west muddy-path-4-4 :east marshy-weeds-6-4))))

(deflocation marshy-weeds-6-4 (marshy-weeds)
  (exits ((swamp-portal :north stagnant-water-6-3 :south stagnant-water-6-5 :west muddy-path-5-4 :east stagnant-water-7-4))))

(deflocation stagnant-water-7-4 (stagnant-water)
  (exits ((swamp-portal :north stagnant-water-7-3 :south stagnant-water-7-5 :west marshy-weeds-6-4 :east marshy-weeds-8-4))))

(deflocation marshy-weeds-8-4 (marshy-weeds)
  (exits ((swamp-portal :north stunted-trees-8-3 :south stagnant-water-8-5 :west stagnant-water-7-4 :east muddy-path-9-4))))

(deflocation muddy-path-9-4 (muddy-path)
  (exits ((swamp-portal :north muddy-path-9-3 :south muddy-path-9-5 :west marshy-weeds-8-4 :east marshy-weeds-10-4))))

(deflocation marshy-weeds-10-4 (marshy-weeds)
  (exits ((swamp-portal :north muddy-path-10-3 :south marshy-weeds-10-5 :west muddy-path-9-4 :east marshy-weeds-11-4))))

(deflocation marshy-weeds-11-4 (marshy-weeds)
  (exits ((swamp-portal :north muddy-path-11-3 :south marshy-weeds-11-5 :west marshy-weeds-10-4 :east river-12-4))))

(deflocation river-12-4 (river)
  (exits ((swamp-portal :north river-12-3 :south river-12-5 :west marshy-weeds-11-4))))

(deflocation marshy-weeds-0-5 (marshy-weeds)
  (exits ((swamp-portal :north muddy-path-0-4 :east marshy-weeds-1-5))))

(deflocation marshy-weeds-1-5 (marshy-weeds)
  (exits ((swamp-portal :north muddy-path-1-4 :south marshy-weeds-1-6 :west marshy-weeds-0-5 :east marshy-weeds-2-5))))

(deflocation marshy-weeds-2-5 (marshy-weeds)
  (exits ((swamp-portal :north muddy-path-2-4 :south marshy-weeds-2-6 :west marshy-weeds-1-5 :east marshy-weeds-3-5))))

(deflocation marshy-weeds-3-5 (marshy-weeds)
  (exits ((swamp-portal :north muddy-path-3-4 :south marshy-weeds-3-6 :west marshy-weeds-2-5 :east marshy-weeds-4-5))))

(deflocation marshy-weeds-4-5 (marshy-weeds)
  (exits ((swamp-portal :north muddy-path-4-4 :south marshy-weeds-4-6 :west marshy-weeds-3-5 :east muddy-path-5-5))))

(deflocation muddy-path-5-5 (muddy-path)
  (exits ((swamp-portal :north muddy-path-5-4 :south muddy-path-5-6 :west marshy-weeds-4-5 :east stagnant-water-6-5))))

(deflocation stagnant-water-6-5 (stagnant-water)
  (exits ((swamp-portal :north marshy-weeds-6-4 :south muddy-path-6-6 :west muddy-path-5-5 :east stagnant-water-7-5))))

(deflocation stagnant-water-7-5 (stagnant-water)
  (exits ((swamp-portal :north stagnant-water-7-4 :south muddy-path-7-6 :west stagnant-water-6-5 :east stagnant-water-8-5))))

(deflocation stagnant-water-8-5 (stagnant-water)
  (exits ((swamp-portal :north marshy-weeds-8-4 :south muddy-path-8-6 :west stagnant-water-7-5 :east muddy-path-9-5))))

(deflocation muddy-path-9-5 (muddy-path)
  (exits ((swamp-portal :north muddy-path-9-4 :south muddy-path-9-6 :west stagnant-water-8-5 :east marshy-weeds-10-5))))

(deflocation marshy-weeds-10-5 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-10-4 :south marshy-weeds-10-6 :west muddy-path-9-5 :east marshy-weeds-11-5))))

(deflocation marshy-weeds-11-5 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-11-4 :south marshy-weeds-11-6 :west marshy-weeds-10-5 :east river-12-5))))

(deflocation river-12-5 (river)
  (exits ((swamp-portal :north river-12-4 :south river-12-6 :west marshy-weeds-11-5 :east marshy-weeds-13-5))))

(deflocation marshy-weeds-13-5 (marshy-weeds)
  (exits ((swamp-portal :south marshy-weeds-13-6 :west river-12-5 :east marshy-weeds-14-5))))

(deflocation marshy-weeds-14-5 (marshy-weeds)
  (exits ((swamp-portal :south marshy-weeds-14-6 :west marshy-weeds-13-5))))

(deflocation marshy-weeds-1-6 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-1-5 :east marshy-weeds-2-6))))

(deflocation marshy-weeds-2-6 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-2-5 :south marshy-weeds-2-7 :west marshy-weeds-1-6 :east marshy-weeds-3-6))))

(deflocation marshy-weeds-3-6 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-3-5 :south marshy-weeds-3-7 :west marshy-weeds-2-6 :east marshy-weeds-4-6))))

(deflocation marshy-weeds-4-6 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-4-5 :south marshy-weeds-4-7 :west marshy-weeds-3-6 :east muddy-path-5-6))))

(deflocation muddy-path-5-6 (muddy-path)
  (exits ((swamp-portal :north muddy-path-5-5 :south marshy-weeds-5-7 :west marshy-weeds-4-6 :east muddy-path-6-6))))

(deflocation muddy-path-6-6 (muddy-path)
  (exits ((swamp-portal :north stagnant-water-6-5 :south marshy-weeds-6-7 :west muddy-path-5-6 :east muddy-path-7-6))))

(deflocation muddy-path-7-6 (muddy-path)
  (exits ((swamp-portal :north stagnant-water-7-5 :south muddy-path-7-7 :west muddy-path-6-6 :east muddy-path-8-6))))

(deflocation muddy-path-8-6 (muddy-path)
  (exits ((swamp-portal :north stagnant-water-8-5 :south marshy-weeds-8-7 :west muddy-path-7-6 :east muddy-path-9-6))))

(deflocation muddy-path-9-6 (muddy-path)
  (exits ((swamp-portal :north muddy-path-9-5 :south stagnant-water-9-7 :west muddy-path-8-6 :east marshy-weeds-10-6))))

(deflocation marshy-weeds-10-6 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-10-5 :south stagnant-water-10-7 :west muddy-path-9-6 :east marshy-weeds-11-6))))

(deflocation marshy-weeds-11-6 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-11-5 :south marshy-weeds-11-7 :west marshy-weeds-10-6 :east river-12-6))))

(deflocation river-12-6 (river)
  (exits ((swamp-portal :north river-12-5 :south river-12-7 :west marshy-weeds-11-6 :east marshy-weeds-13-6))))

(deflocation marshy-weeds-13-6 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-13-5 :south marshy-weeds-13-7 :west river-12-6 :east marshy-weeds-14-6))))

(deflocation marshy-weeds-14-6 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-14-5 :south marshy-weeds-14-7 :west marshy-weeds-13-6 :east marshy-weeds-15-6))))

(deflocation marshy-weeds-15-6 (marshy-weeds)
  (exits ((swamp-portal :south marshy-weeds-15-7 :west marshy-weeds-14-6 :east sinkhole-16-6))))

(deflocation sinkhole-16-6 (sinkhole)
  (exits ((swamp-portal :south marshy-weeds-16-7 :west marshy-weeds-15-6))))

(deflocation marshy-weeds-2-7 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-2-6 :south marshy-weeds-2-8 :east marshy-weeds-3-7))))

(deflocation marshy-weeds-3-7 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-3-6 :south marshy-weeds-3-8 :west marshy-weeds-2-7 :east marshy-weeds-4-7))))

(deflocation marshy-weeds-4-7 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-4-6 :south sinkhole-4-8 :west marshy-weeds-3-7 :east marshy-weeds-5-7))))

(deflocation marshy-weeds-5-7 (marshy-weeds)
  (exits ((swamp-portal :north muddy-path-5-6 :south marshy-weeds-5-8 :west marshy-weeds-4-7 :east marshy-weeds-6-7))))

(deflocation marshy-weeds-6-7 (marshy-weeds)
  (exits ((swamp-portal :north muddy-path-6-6 :south marshy-weeds-6-8 :west marshy-weeds-5-7 :east muddy-path-7-7))))

(deflocation muddy-path-7-7 (muddy-path)
  (exits ((swamp-portal :north muddy-path-7-6 :south muddy-path-7-8 :west marshy-weeds-6-7 :east marshy-weeds-8-7))))

(deflocation marshy-weeds-8-7 (marshy-weeds)
  (exits ((swamp-portal :north muddy-path-8-6 :south marshy-weeds-8-8 :west muddy-path-7-7 :east stagnant-water-9-7))))

(deflocation stagnant-water-9-7 (stagnant-water)
  (exits ((swamp-portal :north muddy-path-9-6 :south stagnant-water-9-8 :west marshy-weeds-8-7 :east stagnant-water-10-7))))

(deflocation stagnant-water-10-7 (stagnant-water)
  (exits ((swamp-portal :north marshy-weeds-10-6 :south stagnant-water-10-8 :west stagnant-water-9-7 :east marshy-weeds-11-7))))

(deflocation marshy-weeds-11-7 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-11-6 :south marshy-weeds-11-8 :west stagnant-water-10-7 :east river-12-7))))

(deflocation river-12-7 (river)
  (exits ((swamp-portal :north river-12-6 :south river-12-8 :west marshy-weeds-11-7 :east marshy-weeds-13-7))))

(deflocation marshy-weeds-13-7 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-13-6 :south marshy-weeds-13-8 :west river-12-7 :east marshy-weeds-14-7))))

(deflocation marshy-weeds-14-7 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-14-6 :south marshy-weeds-14-8 :west marshy-weeds-13-7 :east marshy-weeds-15-7))))

(deflocation marshy-weeds-15-7 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-15-6 :south marshy-weeds-15-8 :west marshy-weeds-14-7 :east marshy-weeds-16-7))))

(deflocation marshy-weeds-16-7 (marshy-weeds)
  (exits ((swamp-portal :north sinkhole-16-6 :west marshy-weeds-15-7))))

(deflocation marshy-weeds-2-8 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-2-7 :south marshy-weeds-2-9 :east marshy-weeds-3-8))))

(deflocation marshy-weeds-3-8 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-3-7 :south marshy-weeds-3-9 :west marshy-weeds-2-8 :east sinkhole-4-8))))

(deflocation sinkhole-4-8 (sinkhole)
  (exits ((swamp-portal :north marshy-weeds-4-7 :south marshy-weeds-4-9 :west marshy-weeds-3-8 :east marshy-weeds-5-8))))

(deflocation marshy-weeds-5-8 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-5-7 :south marshy-weeds-5-9 :west sinkhole-4-8 :east marshy-weeds-6-8))))

(deflocation marshy-weeds-6-8 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-6-7 :south marshy-weeds-6-9 :west marshy-weeds-5-8 :east muddy-path-7-8))))

(deflocation muddy-path-7-8 (muddy-path)
  (exits ((swamp-portal :north muddy-path-7-7 :south muddy-path-7-9 :west marshy-weeds-6-8 :east marshy-weeds-8-8))))

(deflocation marshy-weeds-8-8 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-8-7 :south marshy-weeds-8-9 :west muddy-path-7-8 :east stagnant-water-9-8))))

(deflocation stagnant-water-9-8 (stagnant-water)
  (exits ((swamp-portal :north stagnant-water-9-7 :south marshy-weeds-9-9 :west marshy-weeds-8-8 :east stagnant-water-10-8))))

(deflocation stagnant-water-10-8 (stagnant-water)
  (exits ((swamp-portal :north stagnant-water-10-7 :south marshy-weeds-10-9 :west stagnant-water-9-8 :east marshy-weeds-11-8))))

(deflocation marshy-weeds-11-8 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-11-7 :south river-11-9 :west stagnant-water-10-8 :east river-12-8))))

(deflocation river-12-8 (river)
  (exits ((swamp-portal :north river-12-7 :south river-12-9 :west marshy-weeds-11-8 :east marshy-weeds-13-8))))

(deflocation marshy-weeds-13-8 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-13-7 :west river-12-8 :east marshy-weeds-14-8))))

(deflocation marshy-weeds-14-8 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-14-7 :west marshy-weeds-13-8 :east marshy-weeds-15-8))))

(deflocation marshy-weeds-15-8 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-15-7 :west marshy-weeds-14-8))))

(deflocation marshy-weeds-2-9 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-2-8 :east marshy-weeds-3-9))))

(deflocation marshy-weeds-3-9 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-3-8 :west marshy-weeds-2-9 :east marshy-weeds-4-9))))

(deflocation marshy-weeds-4-9 (marshy-weeds)
  (exits ((swamp-portal :north sinkhole-4-8 :south marshy-weeds-4-10 :west marshy-weeds-3-9 :east marshy-weeds-5-9))))

(deflocation marshy-weeds-5-9 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-5-8 :south marshy-weeds-5-10 :west marshy-weeds-4-9 :east marshy-weeds-6-9))))

(deflocation marshy-weeds-6-9 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-6-8 :south marshy-weeds-6-10 :west marshy-weeds-5-9 :east muddy-path-7-9))))

(deflocation muddy-path-7-9 (muddy-path)
  (exits ((swamp-portal :north muddy-path-7-8 :south muddy-path-7-10 :west marshy-weeds-6-9 :east marshy-weeds-8-9))))

(deflocation marshy-weeds-8-9 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-8-8 :south muddy-path-8-10 :west muddy-path-7-9 :east marshy-weeds-9-9))))

(deflocation marshy-weeds-9-9 (marshy-weeds)
  (exits ((swamp-portal :north stagnant-water-9-8 :south muddy-path-9-10 :west marshy-weeds-8-9 :east marshy-weeds-10-9))))

(deflocation marshy-weeds-10-9 (marshy-weeds)
  (exits ((swamp-portal :north stagnant-water-10-8 :south muddy-path-10-10 :west marshy-weeds-9-9 :east river-11-9))))

(deflocation river-11-9 (river)
  (exits ((swamp-portal :north marshy-weeds-11-8 :south river-11-10 :west marshy-weeds-10-9 :east river-12-9))))

(deflocation river-12-9 (river)
  (exits ((swamp-portal :north river-12-8 :west river-11-9))))

(deflocation marshy-weeds-4-10 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-4-9 :east marshy-weeds-5-10))))

(deflocation marshy-weeds-5-10 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-5-9 :west marshy-weeds-4-10 :east marshy-weeds-6-10))))

(deflocation marshy-weeds-6-10 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-6-9 :south marshy-weeds-6-11 :west marshy-weeds-5-10 :east muddy-path-7-10))))

(deflocation muddy-path-7-10 (muddy-path)
  (exits ((swamp-portal :north muddy-path-7-9 :south marshy-weeds-7-11 :west marshy-weeds-6-10 :east muddy-path-8-10))))

(deflocation muddy-path-8-10 (muddy-path)
  (exits ((swamp-portal :north marshy-weeds-8-9 :south marshy-weeds-8-11 :west muddy-path-7-10 :east muddy-path-9-10))))

(deflocation muddy-path-9-10 (muddy-path)
  (exits ((swamp-portal :north marshy-weeds-9-9 :south marshy-weeds-9-11 :west muddy-path-8-10 :east muddy-path-10-10))))

(deflocation muddy-path-10-10 (muddy-path)
  (exits ((swamp-portal :north marshy-weeds-10-9 :south muddy-path-10-11 :west muddy-path-9-10 :east river-11-10))))

(deflocation river-11-10 (river)
  (exits ((swamp-portal :north river-11-9 :south river-11-11 :west muddy-path-10-10))))

(deflocation marshy-weeds-6-11 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-6-10 :east marshy-weeds-7-11))))

(deflocation marshy-weeds-7-11 (marshy-weeds)
  (exits ((swamp-portal :north muddy-path-7-10 :west marshy-weeds-6-11 :east marshy-weeds-8-11))))

(deflocation marshy-weeds-8-11 (marshy-weeds)
  (exits ((swamp-portal :north muddy-path-8-10 :south marshy-weeds-8-12 :west marshy-weeds-7-11 :east marshy-weeds-9-11))))

(deflocation marshy-weeds-9-11 (marshy-weeds)
  (exits ((swamp-portal :north muddy-path-9-10 :south marshy-weeds-9-12 :west marshy-weeds-8-11 :east muddy-path-10-11))))

(deflocation muddy-path-10-11 (muddy-path)
  (exits ((swamp-portal :north muddy-path-10-10 :south muddy-path-10-12 :west marshy-weeds-9-11 :east river-11-11))))

(deflocation river-11-11 (river)
  (exits ((swamp-portal :north river-11-10 :south river-11-12 :west muddy-path-10-11))))

(deflocation marshy-weeds-8-12 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-8-11 :east marshy-weeds-9-12))))

(deflocation marshy-weeds-9-12 (marshy-weeds)
  (exits ((swamp-portal :north marshy-weeds-9-11 :west marshy-weeds-8-12 :east muddy-path-10-12))))

(deflocation muddy-path-10-12 (muddy-path)
  (exits ((swamp-portal :north muddy-path-10-11 :west marshy-weeds-9-12 :east river-11-12))))

(deflocation river-11-12 (river)
  (exits ((swamp-portal :north river-11-11 :west muddy-path-10-12))))
