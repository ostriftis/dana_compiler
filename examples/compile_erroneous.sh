#!/bin/bash

programs=(
    "assign_error"
    "break_err"
    "continue_err"
    "elif_without_if"
    "grammar_error"
    "if_condition_error"
    "if_condition_byte"
    "missing_colon_in_call"
    "missing_end_for_begin"
    "scope_error1"
    "scope_error2"
    "string_array_lvalue"
    "strwrite_err"
    "type_err"
    "type_mismatch1"
    "type_mismatch2"
    "undeclared_id1"
    "undeclared_id2"
    "undef1"
    "undef2"
)

EXAMPLES_DIR="./programs-erroneous"
COMPILER="../dc"
LOG_FILE="compile_erroneous.log"
ERRORS_DIR="./error_outputs"

# Create directory for error outputs if it doesn't exist
if [ ! -d "$ERRORS_DIR" ]; then
    mkdir -p "$ERRORS_DIR"
fi

echo "" > "$LOG_FILE"

# Counters
total=0
failed_as_expected=0
unexpected_success=0

# Loop through the programs and compile them
for program in "${programs[@]}"; do
    DANA_FILE="${EXAMPLES_DIR}/${program}.dana"
    ERROR_OUTPUT="${ERRORS_DIR}/${program}.err"
    
    if [ -f "$DANA_FILE" ]; then
        total=$((total + 1))
        echo "[$total/${#programs[@]}] Testing: $program"
        
        "$COMPILER" "$DANA_FILE" > "$ERROR_OUTPUT" 2>&1
        
        if [ $? -ne 0 ]; then
            echo " Failed as expected"
            failed_as_expected=$((failed_as_expected + 1))
            echo "FAILED AS EXPECTED: $program" >> "$LOG_FILE"
            cat "$ERROR_OUTPUT" >> "$LOG_FILE"
            echo "" >> "$LOG_FILE"
        else
            echo "  ✗ UNEXPECTED SUCCESS - this should have failed!"
            unexpected_success=$((unexpected_success + 1))
            echo "UNEXPECTED SUCCESS: $program" >> "$LOG_FILE"
        fi
        echo "------------------------"
    else
        echo "Warning: $DANA_FILE not found."
    fi
done

echo "    Error Handling Test Summary"
echo "==========================================="
echo "Total erroneous programs: $total"
echo "Failed as expected: $failed_as_expected"
echo "Unexpected successes: $unexpected_success"
echo "==========================================="
echo ""
echo "Details saved to: $LOG_FILE"
echo "Error outputs saved to: $ERRORS_DIR/"