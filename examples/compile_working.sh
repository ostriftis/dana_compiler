#!/bin/bash

# Enhanced compile_working.sh - Compiles only known working programs
# Uses the new compiler CLI with optional optimization

programs=(
    "binarysearch"
    "bsort"
    "calculator"
    "dot_product"
    "def_scopes"
    "evenChecker"
    "factorial"
    "factors"
    "fibonacci"
    "gcd"
    "hanoi"
    "hello"
    "IntXor"
    "knapsack"
    "linemarket"
    "lis"
    "matrix_mul"
    "mergesort"
    "N_Queens"
    "nextRand"
    "palindrome"
    "powint"
    "perceptron"
    "primeFactors"
    "quicksort"
    "reverseNumber"
    "rotatefun"
    "strrev"
    "sudoku"
    "sumOfDigits"
    "tsp"
)

EXAMPLES_DIR="./programs"
EXECUTABLES_DIR="./executables"
COMPILER="../dc"
LOG_FILE="compile_working.log"

if [ ! -d "$EXECUTABLES_DIR" ]; then
    echo "Creating '$EXECUTABLES_DIR' directory..."
    mkdir -p "$EXECUTABLES_DIR"
fi

echo "" > "$LOG_FILE"

total=0
success=0
failed=0

for program in "${programs[@]}"; do
    DANA_FILE="${EXAMPLES_DIR}/${program}.dana"
    OUTPUT_EXECUTABLE="${EXECUTABLES_DIR}/${program}"
    
    if [ -f "$DANA_FILE" ]; then
        total=$((total + 1))
        echo "[$total/${#programs[@]}] Compiling: $program..."
        
        "$COMPILER" -O -o "$OUTPUT_EXECUTABLE" "$DANA_FILE" >> "$LOG_FILE" 2>&1
        
        if [ $? -eq 0 ]; then
            echo "Success: $OUTPUT_EXECUTABLE"
            success=$((success + 1))
            echo "SUCCESS: $program" >> "$LOG_FILE"
        else
            echo "  ✗ Failed: $program (see $LOG_FILE for details)"
            failed=$((failed + 1))
            echo "FAILED: $program" >> "$LOG_FILE"
        fi
        echo "------------------------"
    else
        echo "Warning: $DANA_FILE not found."
    fi
done

echo ""
echo "====================================="
echo "       Compilation Summary"
echo "====================================="
echo "Total programs: $total"
echo "Successful: $success"
echo "Failed: $failed"
echo "====================================="
echo ""
echo "Details saved to: $LOG_FILE"

if [ $failed -gt 0 ]; then
    exit 1
fi

echo "All working programs compiled successfully!"