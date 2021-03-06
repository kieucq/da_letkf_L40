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
!    - Feb 06, 2009: a randomized forcing has been added. Also
!                    namelist option is included
!    - Feb 09, 2009: update for KF scheme with revised namelist
!                    and output the TLM model.
!    - Feb 11, 2009: found a huge problem with the TLM calculation
!                    due to wrong formulation. Fixed.
!    - Feb 14, 2009: add a second order TML model
!    - Feb 15, 2009: add a theoretical expression for the model error
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
    INTEGER, PARAMETER      :: N  = 40              ! number of vars
    REAL                    :: Ft                   ! true forcing term
    REAL                    :: F                    ! randominzed forcing term
    REAL, DIMENSION(N,2)    :: x                    ! main model var
    REAL, DIMENSION(N)      :: x1,x2,x3             ! buffer var for RK integration
    REAL, DIMENSION(N)      :: rhs1,rhs2,rhs3,rhs4  ! buffer total right hand side forcing
    REAL, DIMENSION(N,N)    :: M                    ! tangetial model for L95 model
    REAL, DIMENSION(N,N)    :: MT                   ! adjoint model
    REAL                    :: dt                   ! model time step (non-dim)
    REAL                    :: dt_tlm               ! time interval for the TLM model
    INTEGER                 :: i,j,loop,irec,itime  ! indexing
    INTEGER                 :: debug                ! debuging 
    INTEGER                 :: restart              ! restart inverval
    INTEGER                 :: nt                   ! total number of model timesteps
    CHARACTER*100           :: ofile,ifile,temc     ! output restart
    REAL                    :: fa,fam(N)            ! error amplitude of forcing F
    REAL                    :: ovar                 ! observation error 
    REAL                    :: bvar                 ! background error
    REAL(8)                 :: rnd(3)               ! random seed for forcing
    REAL                    :: rms                  ! root mean square w.r.t truth
    INTEGER                 :: model_flag           ! flag for model: 0-perfect, 1-imperfect
    INTEGER                 :: ini_flag             ! flag for initial condition: 0-perfect, 1-imperfect
    INTEGER                 :: timeout              ! output interval for the model (steps)
    INTEGER                 :: id                   ! id of the initial time
    INTEGER                 :: tlm_flag             ! option for the TLM model
    irec = 1 
    id   = 1
!
! open input file for the control run with no da cycles
!
    OPEN(10,file='fsc.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=N*4)
    OPEN(11,file='namelist.L40')
    OPEN(13,file='bgd.dat')
    OPEN(14,file='tlmodel.dat')
!
! reading namelist of model parameters
!
    CALL input_namelist(debug,restart,nt,Ft,dt,fa,ovar, &
    bvar,model_flag,ini_flag,x,N,timeout,tlm_flag)
    PRINT*,'L40.exe: Reading input namelist and returns'
    PRINT*,'L40.exe: debug      = ',debug
    PRINT*,'L40.exe: restart    = ',restart
    PRINT*,'L40.exe: nt         = ',nt
    PRINT*,'L40.exe: Ft         = ',Ft
    PRINT*,'L40.exe: dt         = ',dt
    PRINT*,'L40.exe: fa         = ',fa
    PRINT*,'L40.exe: ovar       = ',ovar
    PRINT*,'L40.exe: bvar       = ',bvar
    PRINT*,'L40.exe: model_flag = ',model_flag
    PRINT*,'L40.exe: ini_flag   = ',ini_flag
    PRINT*,'L40.exe: timeout    = ',timeout
    PRINT*,'L40.exe: tlm_flag   = ',tlm_flag
    IF (debug.eq.1) THEN
     DO i               = 1,N
      PRINT*,x(i,1)
     ENDDO
    ENDIF
    IF (debug.eq.1) READ*
!
! write out first input
!
    WRITE(10,rec=irec)(x(i,1),i=1,N)
    irec        = irec + 1
!
! integrate the model now
!
    time_loop: DO loop = 2,nt
     IF (debug.eq.1) PRINT*,'L40.exe: Loop at time',loop
!
! add random noise for impefect model
!
     IF (model_flag.eq.1) THEN
      CALL com_randn(3,rnd)
      F         = Ft + rnd(1)*fa
      DO i      = 1,N
       fam(i)   = rnd(1)*fa
      ENDDO
      IF (debug.eq.1) THEN
       PRINT*,'L40.exe: Randomized forcing',F,Ft,rnd(1),rnd(2),rnd(3),fa
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
     IF (loop-id.eq.restart) THEN
      WRITE(13,*)(x(i,2),i=1,N)
      x1(:)     = x(:,2)
      dt_tlm    = dt*restart
      IF (tlm_flag.eq.1) THEN
       CALL tangential_model(M,x1,N,dt_tlm)
      ELSEIF(tlm_flag.eq.2) THEN
       CALL tangential_model_2sd(M,x1,N,dt_tlm)
      ELSE
       PRINT*,'Dont know tlm_flag option...stop'
       STOP
      ENDIF
      DO i      = 1,N
       WRITE(14,*)(M(i,j),j=1,N)
      ENDDO
     ENDIF
!
! printout for viewing now
!
     IF (mod(loop,timeout).eq.0) THEN
      WRITE(10,rec=irec)(x(i,2),i=1,N)
      irec       = irec + 1
     ENDIF 
!
! update RK step
!
     x(:,1)     = x(:,2)
     x(:,2)     = 0.
    ENDDO time_loop
    PRINT*,'L40.exe: L95 model finishes perfectly'
    END

    SUBROUTINE input_namelist(debug,restart,nt,F,dt,fa,ovar, &
    bvar,model_flag,ini_flag,x,N,timeout,tlm_flag)
    INCLUDE "../inc/L40.inc"
!
! reading the input cold start data
! flag for initial condition: 0-perfect, 1-imperfect
!
    OPEN(12,file='ana.dat',status='old')
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

    SUBROUTINE tangential_model(M,x,N,dt)
    IMPLICIT NONE
    INTEGER N,i,j,k
    REAL M(N,N),x(N),dt
    M           = 0.
    DO i        = 3,N-1
     M(i,i-2)   = -dt*x(i-1)
     M(i,i-1)   = dt*(x(i+1) - x(i-2))
     M(i,i)     = 1-dt
     M(i,i+1)   = dt*x(i-1)
    ENDDO
!
! deal with  i = 1
!
    M(1,N-1)   = -dt*x(N)
    M(1,N)     = dt*(x(2) - x(N-1))
    M(1,1)     = 1 - dt
    M(1,2)     = dt*x(N)
!
! deal with i = 2
!
    M(2,N)     = -dt*x(1)
    M(2,1)     = dt*(x(3) - x(N))
    M(2,2)     = 1 - dt
    M(2,3)     = dt*x(1)
!
! deal with i = N
!
    M(N,N-2)   = -dt*x(N-1)
    M(N,N-1)   = dt*(x(1) - x(N-2))
    M(N,N)     = 1 - dt
    M(N,1)     = dt*x(N-1)
    RETURN
    END

    SUBROUTINE tangential_model_2sd(M,x,N,dt)
    IMPLICIT NONE
    INTEGER N,i,j,k
    REAL M(N,N),x(N),dt,ID(N,N)
    REAL(8) L(N,N)
    M           = 0.
    ID          = 0.  
    DO i        = 1,N
     ID(i,i)    = 1.
    ENDDO
!
! calling jacobien
!
    CALL jacobien_model(L,x,N)
!
! TLM model now
!
!    M           = ID + dt*L + dt*dt*matmul(L,L)/2.
    M           = ID + dt*L + dt*dt*matmul(L,L)
    RETURN
    END


    SUBROUTINE jacobien_model(L,x,N)
    IMPLICIT NONE
    INTEGER N,i,j,k
    REAL(8) L(N,N)
    REAL x(N)
    L           = 0.
    DO i        = 3,N-1
     L(i,i-2)   = -x(i-1)
     L(i,i-1)   = (x(i+1) - x(i-2))
     L(i,i)     = -1
     L(i,i+1)   = x(i-1)
    ENDDO
!
! deal with  i = 1
!
    L(1,N-1)   = -x(N) 
    L(1,N)     = (x(2) - x(N-1))
    L(1,1)     = -1
    L(1,2)     = x(N)
! 
! deal with i = 2
!   
    L(2,N)     = -x(1) 
    L(2,1)     = (x(3) - x(N))
    L(2,2)     = -1
    L(2,3)     = x(1)
! 
! deal with i = N
!   
    L(N,N-2)   = -x(N-1) 
    L(N,N-1)   = (x(1) - x(N-2))
    L(N,N)     = -1
    L(N,1)     = x(N-1)
    RETURN
    END


    SUBROUTINE adjoint_model(MT,x,N,dt)
    IMPLICIT NONE
    INTEGER N,i,j,k
    REAL MT(N,N),M(N,N),x(N),dt
    call tangential_model(M,x,N)
    MT         = transpose(M)
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

