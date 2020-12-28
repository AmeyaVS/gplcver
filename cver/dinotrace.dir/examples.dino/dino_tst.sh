#!/bin/sh 
# install test procedures

CVER="../../bin/cver" 
DINO="../../bin/dinotrace"


if test ! -f $CVER
then
  echo "There is no cver in ../../bin/"
  echo "Make a cver in ../../src" 
  exit;
fi

if test ! -f $DINO
then
  echo "There is no dinotrace"
  echo "Make a dinotrace in ../../dinotrace.dir" 
  exit;
fi

if test $1 
then
  NAME=$1
else
  NAME="dmp.v"
fi

if test ! -f $NAME
then
  echo "no such file: $NAME"
  exit;
fi

if test $NAME = dmp2.v 
then 
  DUMP="xx.dmp"
else
  DUMP="verilog.dump"
fi

$CVER -q $NAME >/dev/null
$DINO $DUMP
rm $DUMP
