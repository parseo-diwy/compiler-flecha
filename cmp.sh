#!/usr/bin/env bash

foones() {
  TEST_INPUT="test/mamarracho/test$1.flecha"
  FOONES="_cmp/foones/test$1.mam"
  FOONES_OUT="_cmp/foones/test$1.output"
  ./bin/flecha_foones "$TEST_INPUT" > "$FOONES"
  ./bin/mamarracho "$FOONES" > "$FOONES_OUT"
}

diwy() {
  TEST_INPUT="test/mamarracho/test$1.flecha"
  DIWY="_cmp/diwy/test$1.mam"
  DIWY_OUT="_cmp/diwy/test$1.output"
  cabal run --verbose=silent flecha -- "$TEST_INPUT" --mam > "$DIWY"
  ./bin/mamarracho "$DIWY" > "$DIWY_OUT"
}

RUN_MODE=$1

if [[ "$RUN_MODE" != "-n" && "$RUN_MODE" != "--full" ]]; then
  echo "Usage: ./foones.sh -n <num>|--full [--foones] [--diwy]"
  exit 1
fi

T=${2:---diwy} # --diwy (default) | --foones
W=${3:-$T}     # --diwy | --foones

if [[ "$RUN_MODE" == '--full' && ("$T" == "--diwy" || "$W" == "--diwy") ]]; then
  for i in {1..9};   do diwy "0$i"; done
  for i in {1..2};   do diwy "09.$i"; done
  for i in {11..31}; do diwy  "$i"; done
fi

if [[ "$RUN_MODE" == '--full' && ("$T" == "--foones" || "$W" == "--foones") ]]; then
  for i in {1..9};   do foones "0$i"; done
  for i in {1..2};   do foones "09.$i"; done
  for i in {11..31}; do foones "$i"; done
fi

if [[ "$RUN_MODE" == "-n" ]]; then
  N=$2            # -n N
  T=${3:---diwy}  # --diwy (default) | --foones
  W=${4:-$T}      # --diwy | --foones

  if [[ "$T" == "--diwy" || "$W" == "--diwy" ]]; then diwy "$N"; fi
  if [[ "$T" == "--foones" || "$W" == "--foones" ]]; then foones "$N"; fi
fi