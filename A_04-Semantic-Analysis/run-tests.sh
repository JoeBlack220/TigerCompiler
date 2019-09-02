#!/bin/bash

# This bash script is based on a similar testing framework developed
# by Gopalan Nadathur for this course.  Some minor modifications were
# made by Eric Van Wyk.

TESTING_DIR="../../public-class-repo/Testing"

TASK="Semantic-Analysis"

# Remove entire outputs directory and recreate it.
rm -Rf outputs
mkdir outputs
mkdir outputs/Appel
mkdir outputs/Initial
mkdir outputs/Semantic-Analysis-Extras
mkdir outputs/Semantic-Analysis-Extras/phase1
mkdir outputs/Semantic-Analysis-Extras/phase2
mkdir outputs/Semantic-Analysis-Extras/phase3
mkdir outputs/Semantic-Analysis-Extras/phase4

# Build the compiler.
ocamlbuild driver.byte || exit 255


# Run `Initital` tests and check the diffs
./driver.byte $TESTING_DIR/TestCases/Initial outputs/Initial
echo "+++ Checking Initial test results"
diff -x "*.errs-v" outputs/Initial $TESTING_DIR/CorrectOutput/$TASK/Initial
if [ $? == "0" ]; then
  echo "Success"
else
  echo "Some tests failed ! ! ! See above."
  msg="Some tests failed."
fi



# Run the next phase of tests and check the diffs
TEST="Semantic-Analysis-Extras/phase1"
./driver.byte $TESTING_DIR/TestCases/$TEST outputs/$TEST
echo "+++ Checking ${TEST} test results"
diff -x "*.errs-v" outputs/$TEST $TESTING_DIR/CorrectOutput/$TASK/$TEST
if [ $? == "0" ]; then
  echo "Success"
else
  echo "Some tests failed ! ! ! See above."
  msg="Some tests failed."
fi


# Run the next phase of tests and check the diffs
TEST="Semantic-Analysis-Extras/phase2"
./driver.byte $TESTING_DIR/TestCases/$TEST outputs/$TEST
echo "+++ Checking ${TEST} test results"
diff -x "*.errs-v" outputs/$TEST $TESTING_DIR/CorrectOutput/$TASK/$TEST
if [ $? == "0" ]; then
  echo "Success"
else
  echo "Some tests failed ! ! ! See above."
  msg="Some tests failed."
fi


# Run `Phase1` tests and check the diffs
TEST="Semantic-Analysis-Extras/phase3"
./driver.byte $TESTING_DIR/TestCases/$TEST outputs/$TEST
echo "+++ Checking ${TEST} test results"
diff -x "*.errs-v" outputs/$TEST $TESTING_DIR/CorrectOutput/$TASK/$TEST
if [ $? == "0" ]; then
  echo "Success"
else
  echo "Some tests failed ! ! ! See above."
  msg="Some tests failed."
fi


# Run `Phase1` tests and check the diffs
TEST="Semantic-Analysis-Extras/phase4"
./driver.byte $TESTING_DIR/TestCases/$TEST outputs/$TEST
echo "+++ Checking ${TEST} test results"
diff -x "*.errs-v" outputs/$TEST $TESTING_DIR/CorrectOutput/$TASK/$TEST
if [ $? == "0" ]; then
  echo "Success"
else
  echo "Some tests failed ! ! ! See above."
  msg="Some tests failed."
fi


# Run the `Appel` test cases and check the diffs
./driver.byte $TESTING_DIR/TestCases/Appel outputs/Appel
echo "+++ Checking Appel test results"

# Check the diffs for the `Appel` test cases
# Uncomment this when you are ready to try these tests
# and they are ready.
diff -x "*.errs-v" outputs/Appel $TESTING_DIR/CorrectOutput/$TASK/Appel
if [ $? == "0" ]; then
  echo "Success"
else
  echo "Some tests failed ! ! ! See above."
  msg="Some tests failed."
fi
exit 0
echo ""
echo "+++ " ${msg}

