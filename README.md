Raytracing Image Generator implemented in functional programming.  

To compile/run, use the command:  
`cabal -O2 run --ghc-options='-rtsopts -threaded -fno-liberate-case -fllvm -optlo-O3 -optlo=-mcpu=native -optlc-O3 -optlc=-mcpu=native -pgmlo=/usr/bin/opt-14 -pgmlc=/usr/bin/llc-14' functional-raytracing -- +RTS -s -N16`
  
Note:
- Replace the `-optlo` and `-optlc` arguments with appropriate paths to your optimiser/compiler.
- Replace the `-N` argument with the number of your cores/threads. 
