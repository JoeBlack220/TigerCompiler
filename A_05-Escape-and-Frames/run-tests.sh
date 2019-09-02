#!/bin/bash

TESTING_DIR="../../public-class-repo/Testing"

TASK="Escape-and-Frames"

# Remove entire outputs directory and recreate it.
rm -Rf outputs

directories=( "Initial" \
              "Semantic-Analysis-Extras/phase1" \
              "Semantic-Analysis-Extras/phase2" \
              "Semantic-Analysis-Extras/phase3" \
              "Semantic-Analysis-Extras/phase4" \
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

    ./driver.byte "${TESTING_DIR}/TestCases/${d}" "outputs/${d}"
    echo "+++ Checking ${d} test results"
    diff -x "*.errs-v" "outputs/${d}" "$TESTING_DIR/CorrectOutput/${TASK}/${d}"

    if [ $? == "0" ]; then
        echo "Success"
    else
        echo "Some tests failed ! ! ! See above."
        msg="Some tests failed."
    fi

done

echo ""
echo "+++ " ${msg}

