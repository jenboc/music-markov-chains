# The Backend
This constitutes the main part of the project.
- `src/app` contains the API logic (which contains some of the worst code I've ever written)
- `src/lib` contains the focal point of the project, which is the music handling and the generation
using Markov Chains

This is the first substantial project that I have written using Haskell so please be nice.

## Running the Frontend
```bash
cd backend
cabal run musicgen
```
