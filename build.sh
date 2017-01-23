#!/bin/sh
stack build
stack exec -- Build $@
