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

## Pushing
- Pushing to master is forbidden, branches would be merged later
- To be accepted for a merge to master, code should either pass unittest (if applicable) or be checked for bugs by at least one other developer