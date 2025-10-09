#!/bin/bash

# Batch test script for all temporal properties
# Output file
OUTPUT_FILE="temporal_verification_results_$(date +%Y%m%d_%H%M%S).txt"
BENCHMARK_DIR="/home/youch/tolin/Smart-Contract-Synthesis/synthesis-benchmark"

echo "=======================================" | tee "$OUTPUT_FILE"
echo "Temporal Property Verification Report" | tee -a "$OUTPUT_FILE"
echo "Started: $(date)" | tee -a "$OUTPUT_FILE"
echo "=======================================" | tee -a "$OUTPUT_FILE"
echo "" | tee -a "$OUTPUT_FILE"

# Find all benchmarks with temporal_properties.txt
BENCHMARKS=($(find "$BENCHMARK_DIR" -name "temporal_properties.txt" | sort))

echo "Found ${#BENCHMARKS[@]} benchmarks with temporal properties" | tee -a "$OUTPUT_FILE"
echo "" | tee -a "$OUTPUT_FILE"

# Counter
TOTAL=0
SUCCESS=0
FAILED=0
PARSE_ERROR=0

# Test each benchmark
for PROP_FILE in "${BENCHMARKS[@]}"; do
    TOTAL=$((TOTAL + 1))
    BENCHMARK_NAME=$(dirname "$PROP_FILE" | xargs basename)
    DL_FILE=$(dirname "$PROP_FILE")/${BENCHMARK_NAME}.dl
    
    echo "=======================================" | tee -a "$OUTPUT_FILE"
    echo "[$TOTAL/${#BENCHMARKS[@]}] Testing: $BENCHMARK_NAME" | tee -a "$OUTPUT_FILE"
    echo "=======================================" | tee -a "$OUTPUT_FILE"
    
    # Check if .dl file exists
    if [ ! -f "$DL_FILE" ]; then
        echo "ERROR: $DL_FILE not found" | tee -a "$OUTPUT_FILE"
        echo "" | tee -a "$OUTPUT_FILE"
        FAILED=$((FAILED + 1))
        continue
    fi
    
    # Run verification with timeout
    echo "Running: sbt \"run verify $DL_FILE $PROP_FILE\"" | tee -a "$OUTPUT_FILE"
    
    TEMP_OUTPUT=$(mktemp)
    timeout 120s sbt "run verify $DL_FILE $PROP_FILE" > "$TEMP_OUTPUT" 2>&1
    EXIT_CODE=$?
    
    if [ $EXIT_CODE -eq 124 ]; then
        echo "TIMEOUT (120s exceeded)" | tee -a "$OUTPUT_FILE"
        FAILED=$((FAILED + 1))
    elif [ $EXIT_CODE -ne 0 ]; then
        echo "FAILED (exit code: $EXIT_CODE)" | tee -a "$OUTPUT_FILE"
        
        # Check if it's a parse error
        if grep -q "Parse error" "$TEMP_OUTPUT"; then
            echo "REASON: Parse error (unsupported syntax)" | tee -a "$OUTPUT_FILE"
            PARSE_ERROR=$((PARSE_ERROR + 1))
        else
            FAILED=$((FAILED + 1))
        fi
        
        # Show error details
        echo "--- Error details ---" | tee -a "$OUTPUT_FILE"
        grep -A 5 "error\|ERROR\|Exception" "$TEMP_OUTPUT" | head -n 10 | tee -a "$OUTPUT_FILE"
    else
        echo "SUCCESS" | tee -a "$OUTPUT_FILE"
        SUCCESS=$((SUCCESS + 1))
        
        # Extract key results
        echo "--- Summary ---" | tee -a "$OUTPUT_FILE"
        grep "Total properties translated:" "$TEMP_OUTPUT" | tee -a "$OUTPUT_FILE"
        grep "ONCE variables created:" "$TEMP_OUTPUT" | tee -a "$OUTPUT_FILE"
        grep "Property VERIFIED\|Property VIOLATED\|Property may be VIOLATED" "$TEMP_OUTPUT" | tee -a "$OUTPUT_FILE"
    fi
    
    rm -f "$TEMP_OUTPUT"
    echo "" | tee -a "$OUTPUT_FILE"
done

# Print summary
echo "=======================================" | tee -a "$OUTPUT_FILE"
echo "SUMMARY" | tee -a "$OUTPUT_FILE"
echo "=======================================" | tee -a "$OUTPUT_FILE"
echo "Total benchmarks: $TOTAL" | tee -a "$OUTPUT_FILE"
echo "Successful: $SUCCESS" | tee -a "$OUTPUT_FILE"
echo "Parse errors: $PARSE_ERROR" | tee -a "$OUTPUT_FILE"
echo "Other failures: $FAILED" | tee -a "$OUTPUT_FILE"
echo "Completed: $(date)" | tee -a "$OUTPUT_FILE"
echo "=======================================" | tee -a "$OUTPUT_FILE"
echo "" | tee -a "$OUTPUT_FILE"
echo "Full results saved to: $OUTPUT_FILE" | tee -a "$OUTPUT_FILE"
