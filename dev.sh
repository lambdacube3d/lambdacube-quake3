#!/bin/sh -e

stack build
stack exec shooter -- q3dm1 +RTS -M2G
