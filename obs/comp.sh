ln -sf ../truth/truth*.dat ./
rm truth.dat
pgf90 -o obs.exe obs.f90 mt19937ar.f90
