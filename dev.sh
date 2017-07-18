#!/bin/bash -e

stack build
stack exec shooter -- +RTS -M2G
