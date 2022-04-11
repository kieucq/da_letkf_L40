#!/bin/sh
#  NOTE: This is a script to run an assimilation system that performs   
#        the LETKF cycle using the Lorenz 40-var model.       
#                                      
#  HIST: - Oct  9, 2009: Created by CK @ UMD
#        - Apr 11, 2022: Updated by CK for DS class at IU
#                 
#  AUTH: Chanh Kieu, Indiana University (C)      
#==================================================================
homedir="/N/slate/ckieu/da_letkf_L40"
ninterval=10                  # DA cycle interval i.t.s number of steps dt
nensemble=30                  # number of ensemble members
restart="Y"                   # Run the entire system from a fresh start
nt=1000                       # Number of time step integration for L40 model
ncycle=$(($nt/$ninterval))    # Number of DA cycles
cold_start="T"                # Warm/cold start option
history="L"                   # output storage option after finishing the script
echo "              SUMMARY OF LETKF CONFIGURATION               "
echo "==========================================================="
echo " Total number of timesteps is                    : $nt"
echo " Number of cycles that the script will run is    : $ncycle"
echo " Number of the ensemble members for this test is : $nensemble"
echo " Interval for doing the analysis is              : $ninterval"
echo " Cold start option is                            : $cold_start"
echo " Re-start option is                              : $restart"
echo " History option is (L-long; S-short)             : $history"       
echo "==========================================================="
#
# set up first some base run
#
if [ "$restart" = "Y" ]; then
    echo "Set up some base runs now ..."
    #
    # start running the truth first to serve as a base
    #
    cd ${homedir}/truth
    ln -sf ${homedir}/run/namelist.L40 ./
    time ./truth.exe
    #
    # now create observational data from the given truth obtained
    # above
    #
    cd ${homedir}/obs/
    ln -sf ${homedir}/run/namelist.L40 ./
    ln -sf ${homedir}/truth/truth*.dat ./
    rm truth.dat
    time ./obs.exe
fi 
#
# copy some initial startup for the cold start mode of KF
# cycle
#
if [ "$cold_start" = "T" ]; then
    echo " creating an ensemble of initial cold starts for the LETKF cycle"
    cd ${homedir}/ini
    ln -sf ${homedir}/run/namelist.L40 ./
    ./ini.exe
fi
cd ${homedir}/bgd
ln -sf ${homedir}/ini/bgd*.dat ./
#
# start running the model with assimilation now. Begin with
# looping over the entire cycles.
#
cd ${homedir}/run/
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
        if [ -f "${homedir}/bgd/$bgdfile" ]; then
            echo " Background file $bgdfile exists...linking now"
            ln -sf ${homedir}/bgd/$bgdfile ./bgd$imember.dat
            ln -sf ${homedir}/bgd/$bgdfile ./mem$imember.dat
        else
            echo " background file $bgdfile does not exist...exit 2"
            exit 2
        fi
        ie=$(($ie + 1))
    done
    #
    # checking the observation data now
    #
    echo " linking observational file now ..."
    obsfile="obs$ofile.dat"
    if [ -f "${homedir}/obs/$obsfile" ]; then
        echo " obs $obsfile file exists ...linking now"
        ln -sf ${homedir}/obs/$obsfile ./obs.dat
    else
        echo " obs file $obsfile does not exist. exit 3"
        exit 3
    fi
    #
    # perform the LETKF assimilation now and compute the ensemble
    # mean for the background now.
    #
    echo " LETKF is called now ..."
    ${homedir}/letkf/letkf.exe
    ${homedir}/utils/mean_ne.exe
    #if [ "$icycle" = "10" ]; then sleep 1800; fi
    mv mean.dat ${homedir}/dig/bgd$ofile.dat
    mv letkf.dat ${homedir}/letkf/letkf$ofile.dat
    rm obs.dat bgd_*.dat mem_*.dat
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
           imember="_00$ie"
       elif [ "$ie" -lt 100 ]; then
           imember="_0$ie"
       else
           imember="_$ie"
       fi
       echo " file extension for the member $ie is $imember"
       ln -sf ana$imember.dat ./ana.dat
       ${homedir}/model/L40.exe
       #
       # add the member extention to the output from the model runs. Also
       # backup the analysis output at each each
       #
       echo " renaming the forecast output from L40 model"
       fscfile="fsc$imember"_"$ofile.dat"
       bgdfile="bgd$imember.dat"
       anafile="ana$imember"_"$ofile.dat"
       echo " fsc.dat will be renamed as $fscfile ..."
       echo " bgd.dat  will be renamed as $bgdfile ..."
       mv -f ${homedir}/run/fsc.dat ${homedir}/fsc/$fscfile
       mv -f ${homedir}/run/bgd.dat ${homedir}/run/$bgdfile
       mv -f ${homedir}/run/ana$imember.dat ${homedir}/ana/$anafile 
       ln -sf ${homedir}/ana/$anafile ./mem$imember.dat
       ie=$(($ie + 1))
    done
    #
    # compute the ensemble mean for the analysis now
    #
    rm ana.dat
    ./mean.exe
    mv mean.dat ${homedir}/dig/ana$ofile.dat
    rm mem_*.dat
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
    ie=1
    while  [ "$ie" -le "$nensemble" ]
    do
        if [ "$ie" -lt 10 ]; then
            imember="_00$ie"
        elif [ "$ie" -lt 100 ]; then
            imember="_0$ie"
        else
            imember="_$ie"
        fi
        echo " updated file extentions is $ofile and $imember"
        bgdfile="bgd$imember"_"$ofile.dat"
        echo " background for the next analysis is $bgdfile"
        mv bgd$imember.dat ${homedir}/bgd/$bgdfile
        ie=$(($ie+1))
    done 
done
#
# doing some analysis now
#
echo " Doing analysis now ..."
cd ${homedir}/dig
ln -sf ${homedir}/truth/truth*.dat ./
ln -sf ${homedir}/obs/obs*.dat ./
time ./ana.exe
#
# cleaning history now
#
if [ "$history" = "S" ]; then
    cd ${homedir}/
    ./clean.sh
fi
#
# cleaning some residual now
#
echo ""
echo " cleaning some unused data"
cd ${homedir}/run
rm -f letkf.dat debug.txt log bmatrix.dat tlmodel*.dat mmatrix.dat bgd*.dat
echo "DONE THE ENSEMBLE RUN SUCCESSFULLY"
