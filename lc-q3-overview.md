TODO
    - block diagram to describe storage and rendering function
    - blogpost link for tesselation

    rename
        Render.hs - to suggest that it sets up game content to pipeline storage

coding tasks:
  - explicit export lists for each modules
  - type signatures for top level functions
  - comments for functions and data structures
  - descriptive argument and variable names
  - split big modules
      - Render.hs


# Overview
- **straight to the point**
- **use diagrams**
- **shorter is better**

start of technical part, it's for Haskell programmers,
for those who want to hack

overview diagram, containing only few boxes

# Engine
code modules:
- Engine.hs

## Game Content
- Zip.hs
    - describe the process how the pk3/zip entry list constructed and how the content is loaded

## Rendering
### Geometry
- BSP.hs
    - group fields into: visual/visual optimization/collision
- MD3.hs
- Q3Patch.hs
- Render.hs, group or refactor
    - fill storage
    - per-frame visual optimization
    - frustum culling

### Material System
- Material.hs
- ShaderParser.hs
- Graphics.lc

### Character Animation
- Character.hs
    - block diagram

## Collision Detection
- Collision.hs
    - link to collision blogpost
    - brush explanation (level vs point/sphere/box collision)

## Game Logic
- Entity.hs
- Items.hs

# Map Viewer
# Shooter Game

# Future Plans
- networking

# Motivation
for users and developers
- what is this?
- what is the approach?
- what is the goal?
- what is the current state?

high level, non technical, understandable for any programmer

- The goal is to create a id Tech 3 compatible game engine using purely functional programming only.

purpose
  - demonstration
  - good start to build prototypes
  - foundation for commercial gaming
