#!/bin/bash
VALID_FILES="tests/valid/*.slac"
INVALID_FILES="tests/invalid/*.slac"

CMD="scala -classpath target/scala-2.11/classes slacc.Main"
echo "Testing valid files"
for f in $VALID_FILES
do
    echo -n "Checking" `basename $f` "..."

    rm -r out
    mkdir out
    rm -r outref
    mkdir outref

    echo -n "Compiling...    "
    scala -classpath lib/cafebabe_2.11-1.2.jar:target/scala-2.11/classes slacc.Main -d out "$f"
    scala -cp ref/cafebabe_2.11-1.2.jar ref/slacc_2.11-1.3.jar -d outref "$f"
    OUR_OUTPUT="$(java -cp out Main)"
    REF_OUTPUT="$(java -cp outref Main)"
    if [[ "${OUR_OUTPUT}" == "${REF_OUTPUT}"  ]]; then
        echo "PASS"
    else
        echo "FAIL"
#        diff <(echo $OUR_OUTPUT) <(echo $REF_OUTPUT)
    fi
done

echo "Testing invalid files"
for f in $INVALID_FILES
do
    echo -n "Checking" `basename $f` "..."

    OUR_OUTPUT=$(scala -classpath lib/cafebabe_2.11-1.2.jar:target/scala-2.11/classes slacc.Main -d out "$f")
    REF_OUTPUT=$(scala -cp ref/cafebabe_2.11-1.2.jar ref/slacc_2.11-1.3.jar -d outref "$f")
    if [[ ($REF_OUTPUT != "" && $OUR_OUTPUT != "") || ( $REF_OUTPUT == "" && $OUR_OUTPUT == "" )  ]]; then
        echo "PASS"
    else
        echo "FAIL"
    fi
done
