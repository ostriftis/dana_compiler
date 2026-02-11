#!/bin/bash

# A script to automate testing of the compiled Dana programs.
#
# It works by iterating through every program in the ./executables directory.
# For each executable, it looks for a corresponding ".input" and ".result" file
# in the ./programs directory. It then runs the executable, provides the input,
# and compares the program's output to the expected result using the 'diff' command.

# Define colors for PASS/FAIL messages to make them easy to see.
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# Define the directory paths
EXECUTABLES_DIR="./executables"
PROGRAMS_DIR="./programs"
TEMP_OUTPUT_FILE="test_output.tmp"

# Ensure the executables directory exists before we start.
if [ ! -d "$EXECUTABLES_DIR" ]; then
    echo "Error: Directory '$EXECUTABLES_DIR' not found."
    echo "Please compile the programs first using compile_all.sh"
    exit 1
fi

# Initialize counters for the final summary.
passed_count=0
failed_count=0
total_count=0

echo "Starting automated tests..."

# Loop through every file in the executables directory.
for executable_path in "$EXECUTABLES_DIR"/*; do
    
    # Make sure we are only trying to run actual files.
    if [ -f "$executable_path" ]; then
        
        base_name=$(basename "$executable_path")
        total_count=$((total_count + 1))

        echo "-------------------------------------"
        echo "Testing: $base_name"

        # Define the expected paths for the input and result files.
        input_file="$PROGRAMS_DIR/$base_name.input"
        result_file="$PROGRAMS_DIR/$base_name.result"

        # A test is only possible if there is a .result file to compare against.
        if [ ! -f "$result_file" ]; then
            echo "SKIPPING: No '.result' file found."
            continue
        fi

        # Run the program. If an .input file exists, redirect its content
        # to the program's standard input.
        # The program's standard output is captured in a temporary file.
        if [ -f "$input_file" ]; then
            "$executable_path" < "$input_file" > "$TEMP_OUTPUT_FILE"
        else
            "$executable_path" > "$TEMP_OUTPUT_FILE"
        fi

        # Compare the program's actual output with the expected result.
        # 'diff -q' is silent on success and returns a non-zero exit code on failure.
        if diff -q "$TEMP_OUTPUT_FILE" "$result_file" >/dev/null 2>&1; then
            echo -e "Result: ${GREEN}PASS${NC}"
            passed_count=$((passed_count + 1))
        else
            echo -e "Result: ${RED}FAIL${NC}"
            failed_count=$((failed_count + 1))
            # If the test fails, show the difference for easy debugging.
            echo "--- EXPECTED OUTPUT ---"
            cat "$result_file"
            echo "---- ACTUAL OUTPUT ----"
            cat "$TEMP_OUTPUT_FILE"
            echo "-----------------------"
        fi
    fi
done

# Clean up the temporary file used for capturing output.
rm -f "$TEMP_OUTPUT_FILE"

# Print a final summary of the test run.
echo ""
echo "====================================="
echo "           Test Summary"
echo "====================================="
echo -e "Total Tests: $total_count"
echo -e "${GREEN}Passed: $passed_count${NC}"
echo -e "${RED}Failed: $failed_count${NC}"
echo "====================================="
echo ""

# Exit with a non-zero status code if any tests failed.
# This is useful for integration with other scripts or CI/CD systems.
if [ "$failed_count" -gt 0 ]; then
    exit 1
fi