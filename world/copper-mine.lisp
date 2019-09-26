(in-package :copper-mine)

(defregion copper-mine
  (name "Abandoned Copper Mine")
  (full "This mine once yielded plentiful copper ore, but was abandoned years
    ago due to the increasing dangers in the nearby woods.")
  (climate :underground))

(defproto rough-tunnel (portal)
  (brief "a rough-hewn tunnel")
  (pose "leads ~a to another part of the mine."))

(defproto mine (location)
  (domain :underground)
  (surface :stone))

;;; entry

(defentity entry (mine)
  (brief "Mine Entry")
  (full "This narrow tunnel slants steeply down to the north. Its low roof is
    supported by heavy timber beams.")
  (exits ((dirt-path :south silverwood::forest-3-1)
          (rough-tunnel :north shaft-top))))

;;; shaft

(defproto shaft-down (portal)
  (brief "the mine shaft")
  (pose "descends deeper into the earth."))

(defproto shaft-up (portal)
  (brief "the mine shaft")
  (pose "ascends toward the surface."))

(defentity shaft-top (mine)
  (brief "Top of Mine Shaft")
  (full "This is the top of a vertical shaft. Irregular, shallow handholds on
    the wall provide an uncertain means of descent.")
  (exits ((rough-tunnel :south entry)
          (shaft-down :down shaft-middle))))

(defentity shaft-middle (mine)
  (brief "Middle of Mine Shaft")
  (full "This is the middle of a vertical shaft. The handholds on the wall
    provide only a tenuous grip, making movement up or down very difficult.")
  (exits ((shaft-up :up shaft-top) (shaft-down :down shaft-bottom))))

(defentity shaft-bottom (mine)
  (brief "Bottom of Mine Shaft")
  (full "This is the bottom of a vertical shaft. You can just make out handholds
    on the wall that provide a means of climbing upwards.")
  (exits ((shaft-up :up shaft-middle) (rough-tunnel :north tunnel-2))))

;; tunnel

(defproto tunnel-room (mine)
  (brief "Mine Tunnel")
  (full "You are in a narrow tunnel deep underground. Heavy timbers
    support the low ceiling."))

(defproto copper-deposit (entity)
  (brief "a small copper deposit")
  (icon "ore.png")
  (full "This small deposit might yield a chunk of copper ore to someone with
    mining skill."))

(defentity tunnel-1 (tunnel-room)
  (exits ((rough-tunnel :south tunnel-2))))

(defentity tunnel-2 (tunnel-room)
  (exits ((rough-tunnel :west tunnel-3 :north tunnel-1
                        :east tunnel-9 :south shaft-bottom))))

#| FIXME: Make the layout more interesting.

(defentity tunnel-3 tunnel-room
  [:exits [:north tunnel-4 :east tunnel-2]])

(defentity tunnel-4 tunnel-room
  [:exits [:north tunnel-5 :south tunnel-3]])

(defentity tunnel-5 tunnel-room
  [:exits [:south tunnel-4 :east tunnel-6]])

(defentity tunnel-6 tunnel-room
  [:exits [:west tunnel-5 :east tunnel-7]])

(defentity tunnel-7 tunnel-room
  [:exits [:west tunnel-6 :south tunnel-8]])

(defentity tunnel-8 tunnel-room
  [:exits [:north tunnel-7 :south tunnel-9 :east tunnel-15]])

(defentity tunnel-9 tunnel-room
  [:exits [:north tunnel-8 :west tunnel-2 :south tunnel-10]])

(defentity tunnel-10 tunnel-room
  [:exits [:north tunnel-9 :east tunnel-11]])

(defentity tunnel-11 tunnel-room
  [:exits [:west tunnel-10 :north tunnel-12]])

(defentity tunnel-12 tunnel-room
  [:exits [:south tunnel-11 :east tunnel-13]])

(defentity tunnel-13 tunnel-room
  [:exits [:west tunnel-12 :north tunnel-14]])

(defentity tunnel-14 tunnel-room
  [:exits [:south tunnel-13 :west tunnel-15]])

(defentity tunnel-15 tunnel-room
  [:exits [:east tunnel-14 :west tunnel-8]])

|#
