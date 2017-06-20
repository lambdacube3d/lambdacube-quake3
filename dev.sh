#!/bin/bash -e

if [[ -z $1 ]]
then
  MAP=q3dm1
else
  MAP=$1
fi

stack build
stack exec shooter -- $MAP +RTS -M2G
