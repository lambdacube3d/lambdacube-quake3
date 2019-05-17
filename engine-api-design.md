# Engine API

### TODO:

- [x] allow attaching MD3s via tags
- [ ] ~~move game character and animation related code to game module ; the engine should handle md3 models in general~~
- [x] add skin, animation resources
- [ ] remove md3character
- [x] expose model and map collision information and query functions

### IDEA:

- allow resource quierying from game logic code ; this follows the c game logic design

## Towards Client-Server Architecture

- associate clients with ID to identify them in WorldSnapshot
- move input to player entity record ; each player should contain its own input
- enhance input handling to cope with multiple clients
- dummy client ; just shoots
- modify renderer to use local client entity (identified by local client ID)

## State Patching

### use-case: weapon / shooting

*basis for the case-study*

- rocket launcher: apply damage to surrounding players
