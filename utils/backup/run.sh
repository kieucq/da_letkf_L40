#!/bin/sh
echo ""
echo "################################################################"
echo "##                                                            ##"
echo "##      This is to run an assimilation system that performs   ##"
echo "##      the LETKF cycle using the Lorenz 40-var model.        ##"
echo "##                                                            ##"
echo "##      Author: Chanh Q. Kieu, Research Associate             ##"
echo "##      Dept. of Atmospheric and Oceanic Science              ##"
echo "##      Univ. of Maryland, College Park                       ##"
echo "##      email: kieucq@atmos.umd.edu                           ##"
echo "##                                                            ##"
echo "################################################################"
echo ""
#echo "Enter the brand new restart <Y/N>"
#read restart
#echo "Enter the restart interval (check restart in namelist)"
#read ninterval
ninterval=10
nensemble=30
restart="Y"
nt=1000
ncycle=$(($nt/$ninterval))
cold_start="T"
history="S"
echo "SUMMARY OF ETKF CONFIGURATION"
echo "==========================================================="
echo " Total number of timesteps is                    : $nt"
echo " Number of cycles that the script will run is    : $ncycle"
echo " Number of the ensemble members for this test is : $nensemble"
echo " Interval for doing the analysis is              : $ninterval"
echo " Cold start option is                            : $cold_start"
echo " Re-start option is                              : $restart"
echo " History option is (L-long; S-short)             : $history"       
echo "==========================================================="
sleep 3
#
# set up first some base run
#
if [ "$restart" = "Y" ]; then
 echo " Set up some base runs now ..."
#
# start running the truth first to serve as a base
#
 cd ./truth
 ./truth.exe
 cd ..
#
# now create observational data from the given truth obtained
# above
#
 cd ./obs/
 ln -sf ../truth/truth*.dat ./
 rm truth.dat
 ./obs.exe
 cd ..
#
# run next the control run without assimilation rms
#
 cd ./ctl/
 ln -sf ../truth/truth*.dat ./
 ln -sf ../obs/obs*.dat ./
 rm truth.dat obs.dat
 ./ctl.exe
 cd ..
fi 
#
# copy some initial startup for the cold start mode of KF
# cycle
#
if [ "$cold_start" = "T" ]; then
 echo " creating an ensemble of initial cold starts for the LETKF cycle"
 cd ./ini
 ./ini.exe
 cd ..
fi
cd ./ana
ln -sf ../ini/bgd*.dat ./
cd ..

#
# start running the model with assimilation now. Begin with
# looping over the entire cycles.
#
icycle=1
filetime=1
echo ""
echo "Perfoming a cycle run now ..."
while [ "$icycle" -le "$ncycle" ]
do
 echo ""
 echo " CYCLE: $icycle"th of the Lorenz 40-var model
#
# create an indexing for the file name that indicate
# the cycle we are running. This step is needed only
# for $icycle -eq 1. After that the ofile will be
# updated at the end of this loop
#
 echo " indexing filetime ..."
 if [ "$filetime" -lt 10 ]; then
  ofile="000$filetime"
 elif [ "$filetime" -lt 100 ]; then
  ofile="00$filetime"
 elif [ "$filetime" -lt 1000 ]; then
  ofile="0$filetime"
 else
  ofile="$filetime"
 fi
 echo " file extention is $ofile"
#
# checking the background data first
#
 echo " checking background data ..."
 ie=1
 while [ "$ie" -le "$nensemble" ]
 do
  if [ "$ie" -lt 10 ]; then
   imember="_00$ie"
  elif [ "$ie" -lt 100 ]; then
   imember="_0$ie"
  elif [ "$ie" -lt 1000 ]; then
   imember="_$ie"
  else
   echo " TO MANY MEMBER...exit 10"
   exit 10
  fi
  bgdfile="bgd$imember"_"$ofile.dat"
  if [ -f "./ana/$bgdfile" ]; then
   echo " Background file $bgdfile exists...linking now"
   ln -sf ./ana/$bgdfile ./bgd.dat
  else
   echo " background file $bgdfile does not exist...exit 2"
   exit 2
  fi
  ie=$(($ie + 1))
 done 

 sleep 1800 
#
# checking the observation data now
#
 echo " linking observational file now ..."
 obsfile="obs$ofile.dat"
 if [ -f "./obs/$obsfile" ]; then
  echo " obs $obsfile file exists ...linking now"
  ln -sf ./obs/$obsfile ./obs.dat
 else
  echo " obs file $obsfile does not exist. exit 3"
  exit 3
 fi
#
# perform a Kalman filter assimilation now and remove the links
#
 echo " Kalman filter is called now ..."
 ./letkf.exe
 rm obs.dat bgd.dat
#
# running the model now
#
 ie=1
 echo ""
 while  [ "$ie" -le "$nensemble" ]
 do
  echo " running the model now at the time $ie"th
#
# creat an extension for the member first
#
  if [ "$ie" -lt 10 ]; then
   fcycle="_00$ie"
  elif [ "$ie" -lt 100 ]; then
   fcycle="_0$ie"
  else
   fcycle="_$ie"
  fi
  echo " file extension for the cycle $ie is $fcycle"
 ./L40.exe
#
# add the member extention to the output from the model runs
#
  echo " renaming the forecast output from L40 model"
  fscfile="fsc$ofile$fcycle.dat"
  tlmfile="tlmodel$fcycle.dat"
  bgdfile="bgd$fcycle.dat"
  echo " fsc.dat will be renamed as $fscfile ..."
  echo " tlmodel.dat  will be renamed as $tlmfile ..."
  echo " bgd.dat  will be renamed as $bgdfile ..."
  mv -f fsc.dat ./fsc/$fscfile
  mv -f tlmodel.dat ./$tlmfile
  mv -f bgd.dat ./$bgdfile
  ie=$(($ie + 1))
 done
#
# backup the analysis output now
#
  anafile="ana$ofile.dat"
  echo " analysis file that will be stored is $anafile ..."
  mv ana.dat ./ana/$anafile
#
# calling emean.exe to compute the ensemle mean of background
# and tangetial linear model now. Also cleaning some un-used 
# files
#
  echo " calling emean now..."
  ./emean.exe
#
# update the next cycle now
#
 icycle=$(($icycle+1))
 filetime=$(($filetime+$ninterval))
#
# update the background at the next cycle now
#
 if [ "$filetime" -lt 10 ]; then
  ofile="000$filetime"
 elif [ "$filetime" -lt 100 ]; then
  ofile="00$filetime"
 elif [ "$filetime" -lt 1000 ]; then
  ofile="0$filetime"
 else
  ofile="$filetime"
 fi
 echo " updated file extention is $ofile"
 bgdfile="bgd$ofile.dat"
 echo " background for the next analysis is $bgdfile"
 mv bgd.dat ./ana/$bgdfile
# sleep 3
done
#
# doing some analysis now
#
echo " Doing analysis now ..."
cd ana
ln -sf ../truth/truth*.dat ./
ln -sf ../obs/obs*.dat ./
ln -sf ../ctl/ctl*.dat ./
rm truth.dat obs.dat ctl.dat
./ana.exe
cd ..
#
# cleaning history now
#
if [ "$history" = "S" ]; then
 ./clean.sh
fi
#
# cleaning some mess now
#
echo "Cleaning some unused data"
echo ""
rm -f kf.dat debug.txt log bmatrix.dat tlmodel*.dat mmatrix.dat bgd*.dat
echo "DONE THE ENSEMBLE RUN SUCCESSFULLY"
exit 0
