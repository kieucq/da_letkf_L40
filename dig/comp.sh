ln -sf ../truth/truth*.dat ./
ln -sf ../obs/obs*.dat ./
ln -sf ../ctl/ctl*.dat ./
rm truth.dat obs.dat ctl.dat
pgf90 -o ana.exe ana.f90

