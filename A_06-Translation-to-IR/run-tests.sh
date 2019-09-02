#!/bin/bash

TESTING_DIR="../../public-class-repo/Testing"

TASK="Translation-to-IR"

# Remove entire outputs directory and recreate it.
rm -Rf outputs

directories=( "Initial" \
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

    ./driver.byte "${TESTING_DIR}/TestCases/${d}" "outputs/${d}"
    echo "+++ Checking ${d} test results"
    # To avoid checking A-06 parts, add
    #    -x "*.frags-n"
    # to the other excluded files below.  You can then get the 
    # "plumbing" work done first and make sure nothing was broken
    # in that process.  Just don't forget to remove this later
    # so that you do check the fragments.
    diff -x "*.errs-v" -x "*.frags" "outputs/${d}" "$TESTING_DIR/CorrectOutput/${TASK}/${d}"

    if [ $? == "0" ]; then
        echo "Success"
    else
        echo "Some tests failed ! ! ! See above."
        msg="Some tests failed."
    fi

done

echo ""
echo "+++ " ${msg}

