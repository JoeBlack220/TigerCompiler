#!/bin/bash

TESTING_DIR="../../public-class-repo/Testing"

# Uncomment ``PHASE="2"`` when ready to move on to phase 2.
PHASE="1"
# PHASE="2"

TASK="Code-Generation-Phase-${PHASE}"

# Remove entire outputs directory and recreate it.
rm -Rf outputs

directories=( 
              "Initial" \
              "Translation-to-IR-Extras/phase1" \
              "Translation-to-IR-Extras/phase2" \
              "Translation-to-IR-Extras/phase3" \
              "Translation-to-IR-Extras/phase4" \
              "Translation-to-IR-Extras/phase5" \
              "Escape-and-Frames-Extras" \
              "Appel" 
            )

for d in ${directories[*]}; do
    mkdir -p "outputs/${d}"
done

# Build the compiler.
ocamlbuild driver.byte || exit 255

msg="All tests passed."

for d in ${directories[*]}; do

    ./driver.byte "${TESTING_DIR}/TestCases/${d}" "outputs/${d}" "${PHASE}"
    echo "+++ Checking ${d} test results"

    diff -x "*.errs-v" -x "*.frags"  "outputs/${d}" "$TESTING_DIR/CorrectOutput/${TASK}/${d}"

    if [ $? == "0" ]; then
        echo "Success"
    else
        echo "Some tests failed ! ! ! See above."
        msg="Some tests failed."
    fi

done

echo ""
echo "+++ " ${msg}

