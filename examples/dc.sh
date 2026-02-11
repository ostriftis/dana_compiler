#!/bin/bash

EXECUTABLES_DIR="./executables"
COMPILER="../dc"

# Create executables directory if it doesn't exist
if [ ! -d "$EXECUTABLES_DIR" ]; then
    echo "Creating '$EXECUTABLES_DIR' directory..."
    mkdir -p "$EXECUTABLES_DIR"
fi

INPUT_FILE="$1"
PROGRAM_NAME=$(basename "$INPUT_FILE" .dana)
OUTPUT_EXECUTABLE="$EXECUTABLES_DIR/$PROGRAM_NAME"

"$COMPILER" $OPTIMIZE_FLAG -o "$OUTPUT_EXECUTABLE" "$INPUT_FILE" 2> errors.log
