#!/usr/bin/env bash

SPS_PATH=$1

function get() {
    local key="^${1}="
    tr -d '\r' < "$SPS_PATH" | grep ${key} | sed s/${key}//
}

BASENAME=$(basename "$SPS_PATH" | sed 's/.sps$//')
DIR="presets/$BASENAME"

echo "Creating directory \"$DIR\""
mkdir -p "$DIR"

for N in $(seq 0 3); do
    echo "[label $N]"
    for X in $(seq 0 2); do
        get "labels_${N}_${X}"
    done
    echo
done > "$DIR/labels.txt"

for N in $(seq 0 2); do
    SIZE=$(get code${N}_size)
    get code${N}_data | xxd -r -p | head -c ${SIZE:-0} > "$DIR/code$N.txt"
done

REMAINING_LINES=$(tr -d '\r' < "$SPS_PATH" |
    grep -P -v '^slider[1-4]=' |
    grep -P -v '^labels_[0-3]_[0-2]=' |
    grep -P -v '^code[0-2]_(size|data)=' |
    tail -n +2
)

if [[ ${REMAINING_LINES} ]] ; then
    echo "Unrecognized lines!!!"
    echo "${REMAINING_LINES}"
    exit 1
fi
