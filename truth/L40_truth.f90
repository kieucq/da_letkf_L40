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
!    - Feb 6, 2009: add namelist option to minimize
!      changes.
!    - Feb 11, 2009: add option include to have more 
!      flexibility
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
    REAL                    :: F                    ! forcing term
    REAL, DIMENSION(N,2)    :: x                    ! main model var
    REAL, DIMENSION(N)      :: x1,x2,x3             ! buffer var for RK integration
    REAL, DIMENSION(N)      :: rhs1,rhs2,rhs3,rhs4  ! buffer total right hand side forcing
    REAL, DIMENSION(N,N)    :: M                    ! tangetial model for L95 model
    REAL, DIMENSION(N,N)    :: MT                   ! adjoint model
    REAL                    :: dt                   ! model time step (non-dim)
    INTEGER                 :: i,j,k,loop,irec      ! indexing
    INTEGER                 :: debug                ! debuging 
    INTEGER                 :: restart              ! restart inverval
    INTEGER                 :: nt                   ! total time steps 
    CHARACTER*100           :: ofile,temc           ! output restart
    REAL                    :: fa                   ! error amplitude of forcing F
    REAL                    :: ovar                 ! observation error 
    REAL                    :: bvar                 ! background error
    INTEGER                 :: model_flag           ! flag for model: 0-perfect, 1-imperfect
    INTEGER                 :: ini_flag             ! flag for initial condition: 0-perfect, 1-imperfect
    INTEGER                 :: id                   ! id of the intial time
    irec = 1 
    id   = 1
!
! open output file for plotting
!
    OPEN(10,FILE='truth.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=N*4)
!
! reading namelist of model parameters
!
    CALL input_namelist(debug,restart,nt,F,dt,fa,ovar,bvar,model_flag,ini_flag,x,N)
    PRINT*,'truth.exe: Reading input namelist and returns'
    PRINT*,'truth.exe: debug      = ',debug
    PRINT*,'truth.exe: restart    = ',restart
    PRINT*,'truth.exe: nt         = ',nt
    PRINT*,'truth.exe: F          = ',F
    PRINT*,'truth.exe: dt         = ',dt
    PRINT*,'truth.exe: fa         = ',fa
    PRINT*,'truth.exe: ovar       = ',ovar
    PRINT*,'truth.exe: bvar       = ',bvar
    PRINT*,'truth.exe: model_flag = ',model_flag
    PRINT*,'truth.exe: ini_flag   = ',ini_flag
    DO i               = 1,N
     IF (debug.eq.1) PRINT*,'truth.exe:',x(i,1)
    ENDDO
    IF (debug.eq.1) READ*
!
! open file to record now
!
    ofile      ='./truth0001.dat'
    OPEN(80,file=ofile)
    WRITE(80,*)(x(i,1),i=1,N)
    CLOSE(80)
    WRITE(10,rec=irec)(x(i,1),i=1,N)
    irec        = irec + 1
!
! integrate the model now
!
    time_loop: DO loop = 2,nt
     IF (debug.eq.1) PRINT*,'truth.exe: Loop at time',loop
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
      IF (loop.lt.10) THEN
       WRITE(ofile(11:11),'(1I1)')loop
      ELSEIF (loop.lt.100) THEN
       WRITE(ofile(10:11),'(1I2)')loop
      ELSEIF (loop.lt.1000) THEN
       WRITE(ofile(9:11),'(1I3)')loop
      ELSE 
       WRITE(ofile(8:11),'(1I4)')loop
      ENDIF
      OPEN(80,file=ofile)
      WRITE(80,*)(x(i,2),i=1,N)
      CLOSE(80)
     ENDIF
!
! printout for viewing now
!
     WRITE(10,rec=irec)(x(i,2),i=1,N)
     irec       = irec + 1
!
! update x(:,1) now
!
     x(:,1)     = x(:,2)
     x(:,2)     = 0.
    ENDDO time_loop
    PRINT*,'truth.exe: L95 model finishes perfectly'
    END


    SUBROUTINE input_namelist(debug,restart,nt,F,dt,fa,ovar,bvar,model_flag,ini_flag,x,N)
    INCLUDE "../inc/L40.inc"
    DO i               = 1,N
     READ(11,*)temc,temc,x(i,1)
    ENDDO
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

