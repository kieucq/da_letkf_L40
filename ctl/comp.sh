ln -sf ../truth/truth*.dat ./
ln -sf ../obs/obs*.dat ./
rm truth.dat obs.dat
pgf90 -o ctl.exe L40_ctl.f90 mt19937ar.f90
