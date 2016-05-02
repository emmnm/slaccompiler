#!/bin/bash
VALID_FILES="tests/valid/*.slac"
INVALID_FILES="tests/invalid/*.slac"
CMD="scala -classpath target/scala-2.11/classes slacc.Main"
echo "Testing valid files"
for f in $VALID_FILES
do
    echo -n "Checking" `basename $f` "..."
    ERR_OUTPUT=$(scala -classpath target/scala-2.11/classes slacc.Main --symid "$f" 2>&1>/dev/null)
    if [[ $ERR_OUTPUT == ""  ]]; then
        echo "PASS"
    else
        echo "FAIL"
        echo $ERR_OUTPUT
    fi
done
echo "Testing invalid files"
for f in $INVALID_FILES
do
    echo -n "Checking" `basename $f` "..."
    ERR_OUTPUT=$(scala -classpath target/scala-2.11/classes slacc.Main --symid "$f" 2>&1>/dev/null)
    if [[ $ERR_OUTPUT != ""  ]]; then
        echo "PASS"
    else
        echo "FAIL"
        echo $ERR_OUTPUT
    fi
done

#echo "Testing $1.slac"
#echo "Generating mytok.ast..."
#echo "Comparing output with given solution"
#rm mytok.ast
