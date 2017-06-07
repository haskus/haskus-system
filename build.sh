#!/bin/sh
stack build --install-ghc && stack exec -- haskus-system-build $@
