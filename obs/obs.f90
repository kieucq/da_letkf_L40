!
! This program is for creating an ensemble of  
! observation that are perturbed from the truth
!
! Author: Chanh Q. Kieu
!
!===================================================================
!
  PROGRAM obs
  IMPLICIT NONE
  INTEGER,PARAMETER :: n = 40 
  REAL              :: obs_err
  REAL              :: x(n),xp(n)
  REAL(8)           :: rnd(n),xr(n)
  CHARACTER*50      :: ofile,ifile,temc
  INTEGER           :: id,i,j,k,itime,irec,restart,nt,ntime,debug,no
  OPEN(91,file='obs.dat',access='direct',form='unformatted',recl=n*4)
!
! reading namelist
!
  CALL input_namelist(debug,restart,nt,obs_err,no,x)
  ntime                  = int(nt/restart) 
  PRINT*,'obs.exe: restart        =',restart
  PRINT*,'obs.exe: nt             =',nt
  PRINT*,'obs.exe: observational error is',obs_err
  PRINT*,'obs.exe: number of opened files will be',ntime
  PRINT*,'obs.exe: number of observation is',no
  IF (debug.eq.1) READ*
!
! start up output files
!
  ifile     = './truth0001.dat'
  ofile     = './obs0001.dat'
  itime     = 1
  id        = 1
  irec      = 1
19 continue
!
! reading the truth
!
  IF (id.lt.10) THEN
   WRITE(ifile(11:11),'(1I1)')id
  ELSEIF (id.lt.100) THEN
   WRITE(ifile(10:11),'(1I2)')id
  ELSEIF (id.lt.1000) THEN
   WRITE(ifile(9:11),'(1I3)')id
  ELSE
   WRITE(ifile(8:11),'(1I4)')id
  ENDIF
  IF (debug.eq.1) PRINT*,'obs.exe: Open file is:  ',ifile(1:30)
  OPEN(71,file=ifile,status='old')
  READ(71,*)(x(i),i=1,n)
  CLOSE(71)
  WRITE(91,rec=irec)(x(i),i=1,n)
  irec     = irec + 1
  CALL com_randn(n,rnd)
  xr       = rnd*obs_err
  xp       = x + xr
  WRITE(91,rec=irec)(xp(i),i=1,n)
  irec     = irec + 1
!
! Output the observation
!
  IF (id.lt.10) THEN
   WRITE(ofile(9:9),'(1I1)')id
  ELSEIF (id.lt.100) THEN
   WRITE(ofile(8:9),'(1I2)')id
  ELSEIF (id.lt.1000) THEN
   WRITE(ofile(7:9),'(1I3)')id
  ELSE
   WRITE(ofile(6:9),'(1I4)')id
  ENDIF
  IF (debug.eq.1) PRINT*,'obs.exe: Output file is:  ',ofile(1:30)
  OPEN(72,file=ofile,status='unknown')
  DO i     = 1,n
   IF (no.eq.40) THEN
    WRITE(72,*)i,xp(i)
   ELSEIF (no.eq.20) THEN
    IF (mod(i,2).eq.0) WRITE(72,'(1I5,E12.4)')i,xp(i) 
   ELSEIF (no.eq.10) THEN
    IF (mod(i,4).eq.0) WRITE(72,'(1I5,E12.4)')i,xp(i)
   ELSEIF (no.eq.8) THEN
    IF (mod(i,5).eq.0) WRITE(72,'(1I5,E12.4)')i,xp(i)
   ELSEIF (no.eq.4) THEN
    IF (mod(i,10).eq.0) WRITE(72,'(1I5,E12.4)')i,xp(i)
   ELSEIF (no.eq.2) THEN
    IF (mod(i,20).eq.0) WRITE(72,'(1I5,E12.4)')i,xp(i)
   ELSE
    PRINT*,'Un-supported the number of obs no...stop'
    STOP
   ENDIF 
  ENDDO
  CLOSE(72)
  itime    = itime + 1
  id       = id + restart
  IF (itime.le.ntime) GOTO 19
  PRINT*,'obs.exe: Program ends perfectly'
  END  


  SUBROUTINE input_namelist(debug,restart,nt,ovar,no,x)
  INCLUDE "../inc/L40.inc"
  DO i               = 1,N
   READ(11,*)temc,temc,x(i,1)
  ENDDO
  RETURN
  END


  SUBROUTINE com_randn(ndim,var)
  USE mt19937
  IMPLICIT NONE
  INTEGER,INTENT(IN) :: ndim
  REAL(8),INTENT(OUT) :: var(1:ndim)
  REAL(8) :: rnd(2),pi
  INTEGER :: idate(8)
  INTEGER :: i,iseed
  LOGICAL,SAVE :: first=.true.
  pi = 4.*atan(1.)

  IF (first) THEN
    CALL DATE_AND_TIME(VALUES=idate)
    iseed = idate(8) + idate(7)*1000
    CALL init_genrand(iseed)
    first=.false.
  END IF

  IF( MOD(ndim,2)==0 ) THEN
    DO i=1,ndim/2
      rnd(1) = genrand_res53()
      rnd(2) = genrand_res53()
      var(i*2-1) = sqrt( -2.0d0 * log( rnd(1) ) ) * sin( 2.0d0*pi*rnd(2) )
      var(i*2) = sqrt( -2.0d0 * log( rnd(1) ) ) * cos( 2.0d0*pi*rnd(2) )
    END DO
  ELSE
    DO i=1,(ndim-1)/2
      rnd(1) = genrand_res53()
      rnd(2) = genrand_res53()
      var(i*2-1) = sqrt( -2.0d0 * log( rnd(1) ) ) * sin( 2.0d0*pi*rnd(2) )
      var(i*2) = sqrt( -2.0d0 * log( rnd(1) ) ) * cos( 2.0d0*pi*rnd(2) )
    END DO
    rnd(1) = genrand_res53()
    rnd(2) = genrand_res53()
    var(ndim) = sqrt( -2.0d0 * log( rnd(1) ) ) * sin( 2.0d0*pi*rnd(2) )
  END IF
  RETURN
END SUBROUTINE com_randn
