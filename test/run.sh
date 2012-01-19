#!/bin/bash

cd "`dirname $0`"

runhaskell -i../src Spec.hs $*
