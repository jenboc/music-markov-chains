#!/bin/bash

# Run backend
cd backend
cabal run musicgen &
BACKEND_PID=$!
cd ..

# Run frontend
cd frontend
npm i
npm run dev &
FRONTEND_PID=$!
cd ..

echo "Both frontend and backend are running."
echo "Backend PID: $BACKEND_PID, Frontend PID: $FRONTEND_PID"
wait $BACKEND_PID $FRONTEND_PID
