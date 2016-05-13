#!/bin/bash
VALID_FILES="tests/*/*.slac"
INVALID_FILES="tests/invalid/*.slac"

CMD="scala -classpath target/scala-2.11/classes slacc.Main"
echo "Testing valid files"
for f in tests/{valid/,invalid/,}*.slac; do
    echo -n "Checking" `basename $f` "..."

    rm -r out
    mkdir out
    rm -r outref
    mkdir outref

    echo -en "Compiling...\t\t"
    OUR_OUT=$(scala -classpath lib/cafebabe_2.11-1.2.jar:target/scala-2.11/classes slacc.Main -d out "$f" 2> out/errFile)
    OUR_ERR=$(<out/errFile)

    REF_OUT=$(scala -cp ref/cafebabe_2.11-1.2.jar ref/slacc_2.11-1.4.jar -d outref "$f" 2> outref/errFile)
    REF_ERR=$(<outref/errFile)

    if [[ -s out/errFile  && -s outref/errFile ]]; then
        echo "PASS (ERR)"
    elif [[ !(-s out/errFile) && !(-s outref/errFile) ]]; then

      if [[ "$(diff -w <(java -cp out Main) <(java -cp outref Main))" ]]; then
        echo "FAIL (DIF)"
      else
        echo "PASS (RUN)"
      fi

    else
        echo "FAIL (ERR)"
    fi

done
