!
! This is the common interface for all codes that use read namelist. 
! If namelist.L40 is changes, this include file has to be modified
! properly.
!
    IMPLICIT NONE              
    INTEGER                 :: N                    ! number of vars
    REAL                    :: F                    ! forcing term
    REAL, DIMENSION(N,2)    :: x                    ! main model var
    REAL                    :: dt                   ! model time step (non-dim)
    INTEGER                 :: debug                ! debuging
    INTEGER                 :: restart              ! restart inverval
    INTEGER                 :: nt                   ! total time steps
    CHARACTER*100           :: temc                 ! tem var
    REAL                    :: fa                   ! error amplitude of forcing F
    REAL                    :: ovar                 ! observation error
    REAL                    :: bvar                 ! background error
    REAL                    :: ifactor              ! inflation factor
    INTEGER                 :: model_flag           ! flag for model: 0-perfect, 1-imperfect
    INTEGER                 :: ini_flag             ! flag for initial condition: 0-perfect, 1-imperfect
    INTEGER                 :: nme                  ! number of ensemble for model error calculation
    INTEGER                 :: timeout              ! output interval for the model (steps)
    REAL                    :: rscale               ! localization scale around a point
    INTEGER                 :: tlm_flag             ! option for the TLM model
    INTEGER                 :: no                   ! number of global obs
    INTEGER                 :: ne                   ! number of ensemble members for LETKF
    INTEGER                 :: nxl                  ! size of local patch
    INTEGER                 :: i,j
    OPEN(11,file='namelist.L40',status='old')
    READ(11,*)temc,temc,debug
    READ(11,*)temc,temc,restart
    READ(11,*)temc,temc,nt
    READ(11,*)temc,temc,F
    READ(11,*)temc,temc,dt
    READ(11,*)temc,temc,fa
    READ(11,*)temc,temc,ovar
    READ(11,*)temc,temc,bvar
    READ(11,*)temc,temc,model_flag
    READ(11,*)temc,temc,ini_flag
    READ(11,*)temc,temc,rscale
    READ(11,*)temc,temc,ifactor
    READ(11,*)temc,temc,nme
    READ(11,*)temc,temc,timeout
    READ(11,*)temc,temc,tlm_flag
    READ(11,*)temc,temc,no
    READ(11,*)temc,temc,ne
    READ(11,*)temc,temc,nxl
    READ(11,*)temc,temc
    READ(11,*)temc,temc
    READ(11,*)temc,temc

