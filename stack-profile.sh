#!/bin/sh

# stack build --executable-profiling --library-profiling --ghc-options="-fprof-auto -rtsopts" && stack exec -- lsc +RTS -p

# stack build --profile --ghc-options=-fprof-auto && stack exec -- lsc-test +RTS -p

# stack build --profile --ghc-options=-fprof-auto && stack exec -- lsc -J -u ../rocket-chip-verilog/freechips.rocketchip.system.DefaultConfig.v +RTS -p

stack build --profile --ghc-options=-fprof-auto && stack exec -- lsc -J -b ../rocket-chip-verilog/rocket.blif +RTS -p

# stack build --profile --ghc-options=-fprof-auto-top && stack exec -- lsc-test +RTS -p

