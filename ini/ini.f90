!
! This program is for creating an ensemble of input 
! for a cold start run of the breeding method. These
! input is supposed to be analysis at t = 0, so you
! dont need to run the assimilation step at t = 0.
! By default, there will have 11 members created.
!
  PROGRAM cold_start_ini 
  IMPLICIT NONE
  INTEGER,PARAMETER :: nx = 40 
  INTEGER           :: ne 
  REAL,PARAMETER    :: er = 3.0 
  REAL              :: x(nx,2),xr(nx),y(nx)
  REAL(8)           :: rnd(nx)
  CHARACTER*50      :: ofile
  INTEGER           :: id,i,j,k,debug
  ofile     = 'bgd_000_0001.dat'
  y(1:nx)   = 1.0
  call input_namelist(debug,ne,x)
  IF (debug.ge.1) THEN
   print*,'ini.exe: debug  = ',debug
   print*,'ini.exe: ne     = ',ne
   read*
  ENDIF
!
! creating an ensemble of +/- enhanced fields.
! It turns out that this is the germ of singulariy
! for the matrxi inversion later used by etkf
!
  DO k      = 1,ne
   PRINT*,'Creating the ensemble ',k
   CALL com_randn(nx,rnd)
   rnd      = rnd*er
   xr       = y + rnd
   id       = k 
   IF (id.le.9) THEN
    WRITE(ofile(7:7),'(1I1)')id
   ELSEIF (id.le.99)  THEN
    WRITE(ofile(6:7),'(1I2)')id
   ELSEIF (id.le.999) THEN
    WRITE(ofile(5:7),'(1I3)')id
   ELSE
    PRINT*,'Too many ensemble members...stop'
    STOP
   ENDIF
   OPEN(11,file=ofile,status='unknown')
   WRITE(11,*)(xr(i),i=1,nx)
   CLOSE(11)
  ENDDO
  END 


  SUBROUTINE input_namelist(debug,ne,x)
  INCLUDE "../inc/L40.inc"
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
