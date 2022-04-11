#!/bin/sh
# NOTE: This script compiles the whole L40-LETKF system. It requires 
#       a fortran compiler and several internal libs. The only Fortran
#       compilers that havve been tested so far are ifort and PGI.
#
# AUTH: Chanh Kieu (ckieu@iu.edu)
#=====================================================================
set -x
MAIN_DIR=/N/u/ckieu/Karst/model/da_letkf_L40
#FC="ifort"
FC="pgf90"
MFC="mpif90"
FCFLAG="-traceback"
INC="$MAIN_DIR/registry"
MPI="No"
if [ $MPI == "Yes" ] && [ $FC != "pgf90" ]; then
   echo "MPI mode can only work with PGI ...stop"
   exit 1
fi
#
# compiling letkf directory
#
echo "Compiling letkf"
cd $MAIN_DIR/letkf
rm -rf *.exe *.o *.mod
$FC $FCFLAG -c mt19937ar.f90
$FC $FCFLAG -c common.f90
$FC $FCFLAG -c netlib.f
$FC $FCFLAG -c matrix_inv.f90 
$FC $FCFLAG -c common_mtx.f90
if [ $MPI == "Yes" ]; then
   $MFC $FCFLAG -c common_mpi.f90
   $MFC $FCFLAG -o letkf.exe letkf.f90 common_mtx.f90 common.f90    \
                   mt19937ar.f90 netlib.f matrix_inv.f90
else
   $FC $FCFLAG -o  letkf.exe letkf.f90 common_mtx.f90 common.f90    \
                   mt19937ar.f90 netlib.f matrix_inv.f90
fi
#
# compiling ctl directory
#
echo "Compiling ctl"
cd $MAIN_DIR/ctl
$FC $FCFLAG -c mt19937ar.f90
$FC $FCFLAG -o ctl.exe L40_ctl.f90 mt19937ar.f90
#
# compiling model directory
#
echo "Compiling model"
cd $MAIN_DIR/model
$FC $FCFLAG -c mt19937ar.f90
$FC $FCFLAG -o L40.exe L40.f90 mt19937ar.f90
#
# compiling obs directory
#
echo "Compiling obs"
cd $MAIN_DIR/obs
$FC $FCFLAG -c mt19937ar.f90
$FC $FCFLAG -o obs.exe obs.f90 mt19937ar.f90
#
# compiling diagnostic directory
#
echo "Compiling dig"
cd $MAIN_DIR/dig
$FC $FCFLAG -o dig.exe dig.f90
#
# compiling truth directory
#
echo "Compiling truth"
cd $MAIN_DIR/truth
$FC $FCFLAG -o truth.exe L40_truth.f90
#
# compiling ini directory
#
echo "Compiling ini"
cd $MAIN_DIR/ini
$FC $FCFLAG -c mt19937ar.f90
$FC $FCFLAG -o ini.exe ini.f90 mt19937ar.f90
#
#
# compiling utils directory
#
echo "Compiling utils"
cd $MAIN_DIR/utils
$FC $FCFLAG -o mean_ne.exe mean_ne.f90
#
cd ../
ls -la */*.exe
check_compile=`ls -la */*.exe | wc -l`
if [ $check_compile == 8 ]; then
    echo "Compilation of LETKF-L40 system is successful"
else
    echo "Compilation failed. Check again"
    exit 1
fi
echo "DONE INSTALLING"
