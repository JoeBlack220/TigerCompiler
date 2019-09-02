#!/bin/bash

TESTING_DIR="../../public-class-repo/Testing"

# Remove entire outputs directory and recreate it.
rm -Rf outputs
mkdir outputs
mkdir outputs/Appel
mkdir outputs/Initial
mkdir outputs/Scanning-errors
mkdir outputs/Parsing-extras
# Build and run the compiler.
ocamlbuild driver.byte || exit 255
./driver.byte $TESTING_DIR/TestCases/Initial outputs/Initial

# Check the diffs for the Appel test cases
# Uncomment this when you are ready to try his tests

./driver.byte $TESTING_DIR/TestCases/Appel outputs/Appel
echo "+++ Checking Appel test results"
diff outputs/Appel $TESTING_DIR/CorrectOutput/Scanner/Appel
if [ $? == "0" ]; then
 echo "Success"
else
 echo "Some tests failed ! ! ! See above."
 msg="Some tests failed."
fi

# Check the diffs for the Scanning test cases
# echo "+++ Checking Initial test results"
# diff outputs/Initial $TESTING_DIR/CorrectOutput/Scanner/Initial
# if [ $? == "0" ]; then
#   echo "Success"
# else
#   echo "Some tests failed ! ! ! See above."
#   msg="Some tests failed."
# fi

echo ""
echo "+++ " ${msg}

