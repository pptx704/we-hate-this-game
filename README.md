# Brief Intro

A logic-based puzzle game that makes the players hate the game. Lack of proper instructions and out of the box logics make the game hateable.

## Important Links
[Project Description](https://docs.google.com/document/d/1wsvezhbjeXZ0c99UihmvqSuooGYO6Wm_a1-OGeNVvOY/edit?usp=sharing)


## Developer Guide

### Branching Rules
- Keep branch names relevant to the task name (i.e. `Level1`, `StartScreen`, `GameAssets` etc)
- Maintain separate branch for separate tasks

### Commits
- Commit message format - `[Commit Type] - [Brief]`
    Commit types
    - Patch
    - Feature
    - Minor Change
- Keep commit to their respective branch (i.e. `Patch - Increased player jumping height` should not be commited to `level1` branch. It should go to `AssetMovement` branch instead.)

### Pushing
- Pushing to master is forbidden, branches would be merged later
- To be accepted for a merge to master, code should either pass unittest (if applicable) or be checked for bugs by at least one other developer

### Graphic Details
- Screen size is 1600px * 900px
- Consider the screen 16 unit x 9 unit. In that case, it is a set of 16x9 `block`s (This will help you organize game graphics more easily. For example, the ceiling is basically top row having 16 blocks. Or maybe, the next level portal is at row 6, col 15).

### Setting-up
- Have GHC 8.10.7 or 8.8.4 installed with Cabal. Better if you have GHCUP installed.
- Clone this repository
- Run `cabal v2-run --disable-tests`. If everything goes fine, you will see a game level.

### Code
- Work on existing files
- Make sure to not break other peoples code
- Write doctests if possible
- Follow the Haskell assignments code style