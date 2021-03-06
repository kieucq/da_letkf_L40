!
! NOTE:     
!      This is the Lorenz 95 model that is used a based
!      model for my various data assimilation systems.
!      The model has 40 vars and time integration is
!      RK-4th scheme. Also, the tangetial model for this 
!      L95 model will also be returned in the form of
!      a matrix. Also, the adjoint model (w.r.t. the 
!      Euler metric) is computed as transpose of the
!      tangential model.
!
! HISTORY:  
!    - Jan 29, 2009: Created  
!    - Feb 11, 2009: add include opt for more flexbility
!
! REFERENCE:
!    - Lorenz 1996: 
!    - Talagran and Courier 1986:
!    - Krishnamurti 1998: Numerical technique
!
! AUTHOR: 
!      Chanh Q. Kieu, Research Associate
!      Dept. of Atmospheric and Oceanic science
!      Univ. of Maryland, College Park, MD
!      email: kieucq@atmos.umd.edu
!
! COPYRIGHT: (C) 2009
!
!=================================================================
!
    PROGRAM L95_model
    IMPLICIT NONE
    INTEGER, PARAMETER      :: N = 40               ! number of vars
    INTEGER, PARAMETER      :: nr = 1000            ! recorded number of obs and truth files
    REAL                    :: Ft                   ! true forcing term
    REAL                    :: F                    ! randominzed forcing term
    REAL, DIMENSION(N,2)    :: x                    ! main model var
    REAL, DIMENSION(N)      :: xt                   ! truth data
    REAL, DIMENSION(N)      :: x1,x2,x3             ! buffer var for RK integration
    REAL, DIMENSION(N)      :: rhs1,rhs2,rhs3,rhs4  ! buffer total right hand side forcing
    REAL, DIMENSION(N,N)    :: M                    ! tangetial model for L95 model
    REAL, DIMENSION(N,N)    :: MT                   ! adjoint model
    REAL                    :: dt                   ! model time step (non-dim)
    INTEGER                 :: i,j,loop,irec,itime  ! indexing
    INTEGER                 :: debug                ! debuging 
    INTEGER                 :: restart              ! restart inverval
    INTEGER                 :: nt                   ! number of total model timesteps
    CHARACTER*100           :: ofile,ifile,temc     ! output restart
    REAL                    :: fa                   ! error amplitude of forcing F
    REAL                    :: ovar                 ! observation error 
    REAL                    :: bvar                 ! background error
    REAL(8)                 :: rnd(3)               ! random seed for forcing
    REAL                    :: rms                  ! root mean square w.r.t truth
    INTEGER                 :: model_flag           ! flag for model: 0-perfect, 1-imperfect
    INTEGER                 :: ini_flag             ! flag for initial condition: 0-perfect, 1-imperfect
    INTEGER                 :: id                   ! id of the initial time
    irec = 1 
    id   = 1
!
! open input file for the control run with no da cycles
!
    OPEN(10,file='ctl.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=N*4)
    OPEN(13,file='ctl.txt')
!
! reading namelist of model parameters
!
    CALL input_namelist(debug,restart,nt,Ft,dt,fa,ovar,bvar,model_flag,ini_flag,x,N)
    PRINT*,'ctl.exe: Reading input namelist and returns'
    PRINT*,'ctl.exe: debug      = ',debug
    PRINT*,'ctl.exe: restart    = ',restart
    PRINT*,'ctl.exe: nt         = ',nt
    PRINT*,'ctl.exe: Ft         = ',Ft
    PRINT*,'ctl.exe: dt         = ',dt
    PRINT*,'ctl.exe: fa         = ',fa
    PRINT*,'ctl.exe: ovar       = ',ovar
    PRINT*,'ctl.exe: bvar       = ',bvar
    PRINT*,'ctl.exe: model_flag = ',model_flag
    PRINT*,'ctl.exe: ini_flag   = ',ini_flag
    IF (debug.eq.1) THEN
     DO i               = 1,N
      PRINT*,'ctl.exe:', x(i,1)
     ENDDO
    ENDIF
    IF (debug.eq.1) READ*
!
! write out first input
!
    ofile       ='./ctl0001.dat'
    ifile       ='truth0001.dat'
    OPEN(80,file=ofile)
    WRITE(80,*)(x(i,1),i=1,N)
    CLOSE(80)
    WRITE(10,rec=irec)(x(i,1),i=1,N)
    irec        = irec + 1
!
! integrate the model now
!
    time_loop: DO loop = 2,nt
     IF (debug.eq.1) PRINT*,'ctl.exe: Loop at time',loop
!
! add random noise for impefect model
!
     IF (model_flag.eq.1) THEN
      CALL com_randn(3,rnd)
      F         = Ft + rnd(1)*fa
      IF (debug.eq.1) THEN
       PRINT*,'ctl.exe: Randomized forcing',F,Ft,rnd(1),rnd(2),rnd(3),fa
       READ*
      ENDIF 
     ELSE
      F         = Ft
     ENDIF
!
! compute forcing and advance modelfor RK step 1
!
     x1(:)      = x(:,1)
     call rhs(F,N,x1,rhs1)
     DO i       = 1,N
      x1(i)     = x(i,1) + rhs1(i)*dt/2
     ENDDO
!
! compute forcing and advance modelfor RK step 2
!
     call rhs(F,N,x1,rhs2)
     DO i       = 1,N
      x2(i)     = x(i,1) + rhs2(i)*dt/2
     ENDDO
!
! compute forcing and advance model for RK step 3
!
     call rhs(F,N,x2,rhs3)
     DO i       = 1,N
      x3(i)     = x(i,1) + rhs3(i)*dt
     ENDDO
!
! compute forcing and advance model for RK step 3
! 
     call rhs(F,N,x3,rhs4)
     DO i       = 1,N
      x(i,2)    = x(i,1) + (rhs1(i)+2*rhs2(i)+2*rhs3(i)+rhs4(i))*dt/6
     ENDDO
     IF (debug.eq.1) THEN
      PRINT*,(x(i,2),i=1,N)
      READ*
     ENDIF
!
! prinout the re-start invertval for creating obs data
!
     IF (mod(loop-id,restart).eq.0) THEN
      xt        = 0
      itime     = loop/restart
      IF (loop.lt.10) THEN
       WRITE(ofile(9:9),'(1I1)')loop
       WRITE(ifile(9:9),'(1I1)')loop
      ELSEIF (loop.lt.100) THEN
       WRITE(ofile(8:9),'(1I2)')loop
       WRITE(ifile(8:9),'(1I2)')loop
      ELSEIF (loop.lt.1000) THEN
       WRITE(ofile(7:9),'(1I3)')loop
       WRITE(ifile(7:9),'(1I3)')loop
      ELSE 
       WRITE(ofile(6:9),'(1I4)')loop
       WRITE(ifile(6:9),'(1I4)')loop
      ENDIF
      OPEN(80,file=ofile)
      OPEN(81,file=ifile)
      WRITE(80,*)(x(i,2),i=1,N)
      READ(81,*)(xt(i),i=1,N)
      CLOSE(80)
      CLOSE(81)
      rms       = 0.
      DO i      = 1,n
       rms      = rms + (x(i,2)-xt(i))**2
      ENDDO
      rms       = sqrt(rms/n)
      WRITE(13,'(I5,10F16.3)')itime,rms
     ENDIF
!
! printout for viewing now
!
     WRITE(10,rec=irec)(x(i,2),i=1,N)
     irec       = irec + 1
!
! update the x(1) now
!
     x(:,1)     = x(:,2)
     x(:,2)     = 0.
    ENDDO time_loop
    PRINT*,'ctl.exe: L95 model finishes perfectly'
    END


    SUBROUTINE input_namelist(debug,restart,nt,F,dt,fa,ovar,bvar,model_flag,ini_flag,x,N)
    INCLUDE "../inc/L40.inc"
!
! reading the input cold start data
! flag for initial condition: 0-perfect, 1-imperfect
!
    OPEN(12,file='fsc_ctl.dat')
    IF (ini_flag.eq.0) THEN
     DO i               = 1,N
      READ(11,*)temc,temc,x(i,1)
     ENDDO
    ELSE
     READ(12,*)(x(i,1),i=1,N)
    ENDIF
    RETURN
    END


    SUBROUTINE rhs(F,N,x,rs)
    IMPLICIT NONE
    INTEGER N,i
    REAL x(N),rs(N),F
    rs(1)       = x(N)*(x(2)-x(N-1))     - x(1) + F
    rs(2)       = x(1)*(x(3)-x(N))       - x(2) + F
    rs(N)       = x(N-1)*(x(1)-x(N-2))   - x(N) + F
    DO i        = 3,N-1
     rs(i)      = x(i-1)*(x(i+1)-x(i-2)) - x(i) + F
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

