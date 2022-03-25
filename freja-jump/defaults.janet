(import freja/default-hotkeys :as dh)

(import ./freja-jump :as fj)

(dh/set-key dh/gb-binds
            [:alt :.]
            (comp dh/reset-blink fj/jump))

(dh/set-key dh/gb-binds
            [:alt :shift :.]
            (comp dh/reset-blink fj/jump-list))

