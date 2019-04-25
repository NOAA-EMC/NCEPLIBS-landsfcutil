# *** manually set environments (for gnu compiler) of landsfcutil ***

# !!! module environment (*THEIA*) !!!
 module load gcc/6.2.0

 ANCHORDIR=..
 export COMP=gnu
 export LANDSFCUTIL_VER=v2.1.0
 export LANDSFCUTIL_SRC=
 export LANDSFCUTIL_INC4=$ANCHORDIR/include/landsfcutil_${LANDSFCUTIL_VER}_4
 export LANDSFCUTIL_INCd=$ANCHORDIR/include/landsfcutil_${LANDSFCUTIL_VER}_d
 export LANDSFCUTIL_LIB4=$ANCHORDIR/liblandsfcutil_${LANDSFCUTIL_VER}_4.a
 export LANDSFCUTIL_LIBd=$ANCHORDIR/liblandsfcutil_${LANDSFCUTIL_VER}_d.a

 export CC=gcc
 export FC=gfortran
 export CPP=cpp
 export OMPCC="$CC -fopenmp"
 export OMPFC="$FC -fopenmp"
 export MPICC=mpigcc
 export MPIFC=mpigfortran

 export DEBUG="-g -O0"
 export CFLAGS="-O3 -DUNDERSCORE -DLINUX -fPIC"
 export FFLAGS="-O3 -fPIC"
 export CPPFLAGS="-P -traditional-cpp"
 export MPICFLAGS="-O3 -DUNDERSCORE -DLINUX -fPIC"
 export MPIFFLAGS="-O3 -fPIC"
 export MODPATH="-J"
 export I4R4=""
 export I4R8="-fdefault-real-8"
 export I8R8="-fdefault-integer-8 -fdefault-real-8"

 export CPPDEFS=""
 export CFLAGSDEFS=""
 export FFLAGSDEFS="-ffree-form"

 export USECC=""
 export USEFC="YES"
 export DEPS=""
