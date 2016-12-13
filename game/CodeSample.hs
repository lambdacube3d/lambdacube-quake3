{-
  * Modules *
  audio
    play sample
    stop sample
    adjust sample volume
  graphics
    add md3 to scene
    add character to scene (with skin)

  * Layers *
  game related engine functions
    HUD display
    explosion effect
    bullet impact
  generic engine functions
    md3 loading and caching
    audio loading and caching
    texture loading and caching
    material loading and pipeline construction and caching

  * Implementation *
  free monad of engine commands

  steps:
    1. free monad and interpreter to
      - add arbitrary md3 models to scene every frame
      - add arbitrary bsp surfaces to scene every frame
      - add arbitrary hud element to scene every frame
      - add arbitrary sound to scene every frame
      - precache arbitrary list of resources
-}

