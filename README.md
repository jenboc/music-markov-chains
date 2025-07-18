# Markov Chain Music Generator
A music generation tool based on Markov Chains written in Haskell with a simple
(React) frontend for ease-of-use.

The frontend allows users to pick from 1 of 3 models, and interact with them
with their own midi dataset.

Each end of the project has its own README, so read those if you want more
information.

## Dependencies
1. Haskell (GHC and Cabal)
2. npm

## Running the Project
The front and backend are currently separate, and have to be run as such. But
for convenience, we have provided `run.sh` which does this for you (provided
you have the required dependencies). Simply run
```bash
./run.sh
```

### Running them yourself
To run the **backend:**
```bash
cd backend
cabal run musicgen
```

To run the **frontend:**
```bash
cd frontend
npm run dev
```

## Future Improvements
1. The frontend is quite naff, it was thrown together for the sole purpose of
being able to use the models without having to write Haskell code.
2. At some point I want to separate the `Music` library into its own repository,
there isn't a lot in it which isn't being used by the project currently so I
don't see a point in doing this at this instant.
3. The generation library code does include a random generation method (by which
it just uses flat probabilities) which would be nice to include in the frontend.
