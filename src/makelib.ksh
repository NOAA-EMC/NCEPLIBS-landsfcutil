#!/bin/ksh
#-----------------------------------------------------------------
#
# script: makelib.ksh
#    prgmmr: george gayno     ORG: NP2      DATE: 2005-09-15
#
# abstract: 
#    builds sfcutil libraries with either 4 or 8 byte floats.
#
# usage:
#    takes one command line argument. 
#    type "makelib.ksh 4" for 4 byte version
#    type "makelib.ksh 8" for 8 byte version
#    type "makelib.ksh clean to remove both libraries
#
#-----------------------------------------------------------------

if [ `uname -s` == "Linux" ];then
. ./makefile.linux.conf
elif [ `uname -s` == "AIX" ];then 
. ./makefile.aix.conf
else
  exit 99
fi

if [[ $1 = 4 ]]
then
  echo Will build library with 4 byte floats.
  export FCOMP
  export AFLAGS
  export FFLAGS=$FFLAGS_4
  export MOD_DIR=$MOD_DIR_4
  export LIB=../lib/liblandsfcutil_4.a
  make clean
  make
elif [[ $1 = 8 ]]
then
  echo Will build library with 8 byte floats.
  export FCOMP
  export AFLAGS
  export FFLAGS=$FFLAGS_D
  export MOD_DIR=$MOD_DIR_D
  export LIB=../lib/liblandsfcutil_d.a
  make clean
  make
elif [[ $1 = "clean" ]]
then
  export MOD_DIR=$MOD_DIR_4
  export LIB=../lib/liblandsfcutil_4.a
  make clean 
  export MOD_DIR=$MOD_DIR_D
  export LIB=../lib/liblandsfcutil_d.a
  make clean 
else
  echo Must choose either 4 or 8 byte option.
  exit 1
fi

exit 0
