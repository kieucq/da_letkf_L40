!
! NOTE:     
!      This program performs a local ensemble transform Kalman filter
!      for the Lorenz 40-var model.
!
! HISTORY:  
!    - April 13 2009: Created 
!    - April 21 2009: Add an option that allows for couting local
!                     obs at each local patch. Also option for 
!                     reduced obs is added
!    - April 22 2009: re-structure such that I have a common core
!                     for LETKF that can be used later. Also 
!                     all arrays are allocated based on namelist
!                     input of the ensemble members
!    - April 25 2009: Fix a very serious bug in computing the global
!                     obs background. This is supposed to be before
!                     computing the global background mean (i.e., 
!                     steps 1 and 2 in H04 paper was flipped by my
!                     casuality)
!
! REFERENCE:
!    - Hunt et al., 2006: arxiv:physics/0511236
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
  program LETKF
  use common
  use common_mtx
  implicit none
  integer, parameter               :: nx = 40             ! model domain size
  integer                          :: ne                  ! number of ensemble members
  integer                          :: nxl                 ! local patch of model state (need to be odd)
  integer                          :: no                  ! number of observations
  integer                          :: nol                 ! local patch of obs
  real, allocatable                :: Xb_g(:,:)           ! global background (forecast)
  real, allocatable                :: Xb_gm(:)            ! global ensemble mean of background
  real, allocatable                :: Xb(:,:)             ! local background (forecast) 
  real, allocatable                :: Xb_m(:)             ! local ensemble mean of background
  real, allocatable                :: Xa_g(:,:)           ! global analysis
  real, allocatable                :: Xa_gm(:)            ! global ensemble mean of analysis
  real, allocatable                :: Xa(:,:)             ! local analysis
  real, allocatable                :: Xa_m(:)             ! local ensemble mean of analysis
  real, allocatable                :: Yb_g(:,:)           ! global background obs ensemble
  real, allocatable                :: Yb_gm(:)            ! global ensemble mean of ensemble obs
  real, allocatable                :: Yb(:,:)             ! local background obs ensemble
  real, allocatable                :: Yb_m(:)             ! local ensemble mean of ensemble obs
  real, allocatable                :: Yo_g(:)             ! global observation data
  real, allocatable                :: Yo(:)               ! local observation data
  real, allocatable                :: R(:,:)              ! local obs error cov mtx
  real, allocatable                :: olon(:)             ! observation location
  real, allocatable                :: H_g(:,:)            ! global observation operator
  integer                          :: model_flag          ! flag for model: 0-perfect,1-imperfect
  integer                          :: ini_flag            ! flag for initial condition: 0-perfect, 1-imperfect
  integer                          :: nme                 ! number of model ensemble
  real                             :: tem2(nx)            ! temp var
  real                             :: ifactor             ! inflation factor
  character*100                    :: ifile,ofile,temc    ! I/O files
  integer                          :: i,j,i1,j1,k1,ie     ! indexing
  integer                          :: debug               ! debuging
  integer                          :: irec                ! output record
  integer                          :: ofactor             ! factor for printing obs
  real                             :: bvar                ! background variance
  real                             :: ovar                ! observation variance
  real                             :: rscale              ! scale of background variance matrix
  REAL, DIMENSION(nx,2)            :: x                   ! main model var (not needed here)
  irec           = 1
  open(92,file='letkf.dat',access='direct',form='unformatted',recl=nx*4)
!
! reading namelist now
!
  call input_namelist(debug,ovar,bvar,model_flag,ini_flag,ifactor,nme,no,ne,nxl,x)
  print*,'letkf.exe: input namelist is'
  print*,'letkf.exe: debug         =',debug
  print*,'letkf.exe: bvar          =',bvar
  print*,'letkf.exe: ovar          =',ovar
  print*,'letkf.exe: model_flag    =',model_flag
  print*,'letkf.exe: ini_flag      =',ini_flag
  print*,'letkf.exe: ifactor       =',ifactor
  print*,'letkf.exe: nme           =',nme
  print*,'letkf.exe: no            =',no
  print*,'letkf.exe: ne            =',ne
  print*,'letkf.exe: nxl           =',nxl
  if (debug.ge.1) read*
  if (ini_flag.eq.0) then
   print*,'WARNING: INPUT IS PERFECT? WHY NEED ASSIMILATION...STOP'
   stop
  endif 
  if (nme.gt.1.and.ifactor.gt.1e-10) then
   print*,'WARNING: CANNOT HAVE BOTH INFLATION AND PERTURBED FORCING...STOP'
   stop
  endif
!
! allocate arrays now
!
  allocate(Xb_g(nx,ne),Xb_gm(nx),Xb(nxl,ne),Xb_m(nxl))
  allocate(Xa_g(nx,ne),Xa_gm(nx),Xa(nxl,ne),Xa_m(nxl))
  allocate(Yb_g(no,ne),Yb_gm(no),olon(no),Yo_g(no),H_g(no,nx))
!
! reading observation data
!
  open(90,file='obs.dat')
  i              = 1
2 continue
  read(90,*,end=3)olon(i),Yo_g(i)
  if (debug.ge.2) then
   if (i.eq.1) print*,'letkf.exe: Checking the obs'
   print*,i,olon(i),Yo_g(i)
  endif
  i              = i + 1
  if (i.le.no) goto 2
  close(90)
  goto 4
3 print*,'letkf.exe: There is not enough obs data as needed by no...stop'
  stop
4 continue
  close(90)
!
! quick check of obs data
!
  ofactor        = int(nx/no)
  do i           = 1,nx
    if (mod(i,ofactor).eq.0) then
     print*,i,ofactor,int(i/ofactor)
     tem2(i)     = Yo_g(int(i/ofactor))
    else
     tem2(i)     = -99999
    endif
  enddo
  write(92,rec=irec)(tem2(i),i=1,nx)
  irec          = irec + 1
!
! reading forecast files
!
  ifile='bgd_000.dat'
  ie            = 0
8 continue
  ie            = ie + 1
  if (ie.le.9) then
   write(ifile(7:7),'(1I1)')ie
  elseif (ie.le.99) then
   write(ifile(6:7),'(1I2)')ie 
  elseif (ie.le.999) then
   write(ifile(5:7),'(1I3)')ie
  else
   print*,'letkf.exe: Stop as too many ensemble members'
   stop
  endif
  if (debug.ge.1) print*,'letkf.exe: input forecast file: ',ifile(1:20)
  open(91,file=ifile)
  read(91,*)(Xb_g(i,ie),i=1,nx)
  close(91)
  write(92,rec=irec)(Xb_g(i,ie),i=1,nx)
  irec           = irec + 1
  if (ie.lt.ne) goto 8
  Xa_g           = Xb_g
!
! compute the observational operator
!
  call observation_operator(nx,no,olon,H_g) 
  if (debug.ge.2) then
   print*,'letkf.exe: Checking the global H_g mtx'
   do i          = 1,20
    write(0,'(20F6.3)')(H_g(i,j),j=1,20)
   enddo
  endif
! 
! compute the global background observation ensemble (step 1 in H04)
! 
  Yb_g           = matmul(H_g,Xb_g)
!
! compute the global mean and ensebmle of pertubation background.
! Note that the global background will not be needed from now on,
! so the Xb_g array will now store perturbation, not the total.
! This is step 2 in H04.
!
  Xb_gm(:)       = 0.
  do i           = 1,ne
   Xb_gm(:)      = Xb_gm(:) + Xb_g(:,i) 
  enddo
  Xb_gm(:)       = Xb_gm(:)/ne
  do i           = 1,ne
   Xb_g(:,i)     = Xb_g(:,i) - Xb_gm(:)
  enddo
!
! Now loop over all grid point with the corresponding
! local patch.
!
  grid_loop: do i=1,nx 
   if (debug.ge.2) print*,'Working at the local patch around',i
!
! finding the number of local observation within each patch
!
   call local_obs(i,nx,nxl,olon,no,nol,debug)
   if (debug.ge.1) print*,'letkf.exe: Number of local obs is',nol
!
! project from global to local patch at each grid point i. 
!
   allocate(Yb(nol,ne),Yb_m(nol),Yo(nol))
   call global2local(Xb_g,Xb_gm,Yb_g,Yb_gm,Yo_g,nx,ne,no, &
                     Xb,Xb_m,Yb,Yb_m,Yo,nxl,nol,olon,i,debug)
!
! define the local observational error covariance matrix R
!
   allocate(R(nol,nol))
   call observational_err_cov_mtx(R,nol,ovar)
!
! calling the LETKF core now.
!
   call letkf_main(Xa,R,Xb,Xb_m,Yb,Yb_m,Yo,nxl,nol,ne,ifactor,debug)
!
! project from local patch to global patch
!
   call local2global(Xa,nxl,ne,Xa_g,nx,i)
!
! deallocate the arrays
!
   deallocate(Yb,Yb_m,Yo,R)
  enddo grid_loop
!
! output analysis x and updated Pa. Note that bmatrix.dat
! will be over-written by the updated Pa as this file
! will be discarded after performing KF.
!
  ofile='ana_000.dat'
  ie            = 0
9 continue
  ie            = ie + 1
  if (ie.le.9) then
   write(ofile(7:7),'(1I1)')ie
  elseif (ie.le.99) then
   write(ofile(6:7),'(1I2)')ie
  elseif (ie.le.999) then
   write(ofile(5:7),'(1I3)')ie
  else
   print*,'letkf.exe: Stop as too many ensemble members'
   stop
  endif
  if (debug.ge.1) print*,'letkf.exe:  Open output file is: ',ofile(1:20)
  open(12,file=ofile,status='unknown')
  write(12,*)(Xa_g(i,ie),i=1,nx)
  close(12)
  if (ie.lt.ne) goto 9
!
! printout for viewing
!
  do ie          = 1,ne
   write(92,rec=irec)(Xa_g(i,ie),i=1,nx)
   irec          = irec + 1
  enddo 
  print*,'letkf.exe: LETKF finishes safely...!'
  end


  subroutine local_obs(ig,nx,nxl,olon,no,nol,debug)
  implicit none
  integer no,nxl,nol,ig,nx
  real olon(no)
  real xlon
  integer i,j,k,istart,iend,debug,bcyclic
  bcyclic        = 1
!
! defining the start and end position of the local patch around ig
!
  istart         = ig - nxl/2
  iend           = ig + nxl/2
  if (debug.ge.3) print*,'debug: istart,iend',istart,iend
!
! find the local obs within the local obs patch.
! Note that the cyclinc boundary is applied only for +/- 5 points
! at each end of the domain.
!
  j              = 0
  do i           = 1,no
   xlon          = olon(i)
   if (bcyclic.eq.1) then
    if (istart.lt.1.and.int(xlon).ge.nx-5) then
     xlon        = int(xlon) - nx
    endif
    if (iend.gt.nx.and.int(xlon).le.5) then
     xlon        = int(xlon) + nx
    endif
   endif
   if (debug.ge.3) print*,'ig,istart,iend,i,olon(i),xlon',ig,istart,iend,i,olon(i),xlon
   if (xlon.ge.istart.and.xlon.le.iend) then
    j            = j + 1
   endif
  enddo
  nol            = j
  return
  end

  subroutine global2local(Xb_g,Xb_gm,Yb_g,Yb_gm,Yo_g,nx,ne,no, &
                          Xb,Xb_m,Yb,Yb_m,Yo,nxl,nol,olon,ig,debug)
  implicit none
  integer nx,no,nxl,nol,ig,ne
  real Xb_g(nx,ne),Xb_gm(nx),Xb(nxl,ne),Xb_m(nxl)
  real Yb_g(no,ne),Yb_gm(no),Yb(nol,ne),Yb_m(nol)
  real Yo_g(no),Yo(nol),olon(no)
  real Xb_ge(-nxl:nx+nxl,ne),Xb_gme(-nxl:nx+nxl),xlon
  integer i,j,k,istart,iend,debug,bcyclic
  bcyclic        = 1
!
! creating a buffer array now to deal with the boundary points
!
  Xb_ge(1:nx,:)  = Xb_g(1:nx,:)
  Xb_gme(1:nx)   = Xb_gm(1:nx)
  do i           = -nxl,0
   Xb_ge(i,:)    = Xb_g(nx+i,:)
   Xb_gme(i)     = Xb_gm(nx+i)
  enddo
  do i           = 1,nxl
   Xb_ge(nx+i,:) = Xb_g(i,:)
   Xb_gme(nx+i)  = Xb_gm(i)
  enddo
!
! defining the start and end position of the local patch around ig
!
  istart         = ig - nxl/2
  iend           = ig + nxl/2
  if (debug.ge.3) print*,'debug: istart,iend',istart,iend
!
! asign the global array to local patch
!
  do i           = 1,nxl
   j             = istart + i - 1
   Xb(i,1:ne)    = Xb_ge(j,1:ne)   
   Xb_m(i)       = Xb_gme(j)
  enddo
!
! find and assign the global obs ensemble to the local obs patch.
! Note that the cyclinc boundary is applied only for +/- 5 points
! at each end of the domain.
!
  j              = 0
  do i           = 1,no
   xlon          = olon(i)
   if (bcyclic.eq.1) then
    if (istart.lt.1.and.int(xlon).ge.nx-5) then
     xlon        = int(xlon) - nx
    endif
    if (iend.gt.nx.and.int(xlon).le.5) then
     xlon        = int(xlon) + nx 
    endif
   endif
   if (debug.ge.3) print*,'ig,istart,iend,i,olon(i),xlon',ig,istart,iend,i,olon(i),xlon
   if (xlon.ge.istart.and.xlon.le.iend) then
    j            = j + 1
    Yb(j,1:ne)   = Yb_g(i,1:ne)
    Yb_m(j)      = Yb_gm(i)
    Yo(j)        = Yo_g(i)
   endif
  enddo
  if (debug.ge.3) print*,'j,nol',j,nol
  if (j.ne.nol) then
   print*,'letkf.exe: The number of obs in local patch does not match the array shape'
   stop
  endif
  return
  end
  
  subroutine local2global(Xa,nxl,ne,Xa_g,nx,ig)
  implicit none
  integer nx,nxl,ne,ig,ic
  real Xa(nxl,ne),Xa_g(nx,ne)
  ic             = nxl/2 + 1
  Xa_g(ig,1:ne)  = Xa(ic,1:ne) 
  return
  end

  subroutine letkf_main(Xa,R,Xb,Xb_m,Yb,Yb_m,Yo,nxl,nol,ne,ifactor,debug)
  use common
  use common_mtx
  implicit none
  integer nxl,nol,ne,debug
  real R(nol,nol),Rinv(nol,nol)
  real Xb(nxl,ne),Xb_m(nxl),Xa(nxl,ne)
  real Yb(nol,ne),Yb_m(nol),Yo(nol),ifactor
  real C(ne,nol),tem1(ne,ne),dy(nol)
  real Wa(ne,ne),Pat(ne,ne),wa_m(ne)
  integer i1,j1,k1,i,j,k
!
! compute the R inverted first
!
  call compute_Rinv(R,Rinv,nol)
  if (debug.ge.2) then 
   print*,'R matrix'
   do i1        = 1,nol
    write(0,'(20F6.3)')(R(i1,j1),j1=1,nol)
   enddo
  endif 
!
! compute matrix C (step 4 in H06)
!
  C             = matmul(transpose(Yb),Rinv)
  if (debug.ge.2) then 
   print*,'step 4: C matrix'
   do i1        = 1,ne
     write(0,'(20F6.3)')(C(i1,j1),j1=1,nol)
   enddo
  endif
!
! compute matrix Pa tilde (step 5 in H06)
!
  tem1          = matmul(C,Yb)
  do j          = 1,ne
   tem1(j,j)    = tem1(j,j) + (ne-1)/(1+ifactor)
  enddo
  call mtx_inv(ne,tem1,Pat)
  if (debug.ge.2) then
   print*,'step 5: Yb matrix'
   do i1        = 1,nol
    write(0,'(20F6.3)')(Yb(i1,j1),j1=1,20)
   enddo
   print*,'step 5: tem1=C*Yb matrix'
   do i1        = 1,20
    write(0,'(20F6.3)')(tem1(i1,j1),j1=1,20)
   enddo
  endif
!
! compute matrix Wa (step 6 in H06) 
!
  tem1          = (ne-1)*Pat
  call mtx_sqrt(ne,tem1,Wa)
  if (debug.ge.2) then
   print*,'step 6: Wa matrix'
   do i1        = 1,20
    write(0,'(20F6.3)')(Wa(i1,j1),j1=1,20)
   enddo
  endif
!
! compute wa_m and add it to Wa matrix (step 7 in H06)
!
  dy            = Yo - Yb_m
  if (debug.ge.2) print*,'step 7: ok dy'
  wa_m          = matmul(matmul(Pat,C),dy)
  if (debug.ge.2) print*,'step 7: ok wa_m'
  do j          = 1,ne
   Wa(:,j)      = Wa(:,j) + wa_m(:)
  enddo
  if (debug.ge.2) print*,'step 7: ok Wa'
!
! Computing the analysis ensemble members (step 8 in H06)
!
  Xa            = matmul(Xb,Wa)
  if (debug.ge.2) print*,'step 8: ok Xa'
  do j          = 1,ne
   Xa(:,j)      = Xa(:,j) + Xb_m(:)
  enddo
  return
  end

  subroutine observation_operator(nx,no,olon,H)
  implicit none
  integer nx,no,i,j,m,n,k,io,jo
  real H(no,nx),olon(no)
  H         = 0.
  do i      = 1,no
   j        = int(olon(i)+0.001)  
   H(i,j)   = 1.
  enddo
  return
  end

  subroutine localiztion_operator(nx,ny,no,nv,olon,olat,lopt)
  implicit none
  integer nx,ny,no,nv,i,j,m,n
  real lopt(nv,no),olat(no),olon(no),rscale,radi
  rscale    = 10.
  do i      = 1,nv
   do j     = 1,no
    m       = mod(i,nx)
    n       = i/nx + 1
    if (m.eq.0) then
     m      = nx
     n      = n - 1
    endif
    radi    = sqrt((olon(j)-m)**2. + (olat(j)-n)**2.)
    lopt(i,j) = exp(-(radi/rscale)**2)
   enddo
  enddo
  print*,'Checking Pat matrix'
  do i       = 1,20
   write(*,'(20F6.2)')(lopt(i,j),j=1,20)
  enddo
  return
  end

  subroutine background_err_cov_mtx(B,nx,bvar,rscale)
  implicit none
  integer i,j,m,n,nx
  real B(nx,nx),rscale,radi,bvar
  do i      = 1,nx
   do j     = 1,nx
    radi    = sqrt((j-i)**2. + (j-i)**2.)
    B(i,j)  = bvar*bvar*exp(-(radi/rscale)**2)
   enddo
  enddo
  return
  end

  subroutine convert_array_vector(p,nx,ny,X,nxp,nyp,nxp0,nyp0,nvp,i1,j1,ip,jp)
  implicit none
  integer nx,ny,ne,nvp,nxp,nyp,i1,j1,nxp0,nyp0
  real ip(nxp),jp(nyp)
  real p(nx,ny),X(nvp),tem
  integer i,j,k,m,n
!
! convert from an ensemble of 2D arrays to 1D vectors
!
  do i     = 1,nvp
!
! compute the local indices of grid points in the local patch first
!
   m       = mod(i,nxp)
   n       = i/nxp + 1
   if (m.eq.0) then
    m      = nxp
    n      = n - 1
   endif
   print*,'patch:',i1,j1,ip(m),jp(n)
!
! now compute the absolute indices w.r.t. to the global grid. Note 
! that the last patch is not necessarily the same size as the
! other patch. So, nxp0 must be used
!
   m       = (i1-1)*nxp0 + m
   n       = (j1-1)*nyp0 + n
   print*,'patch:',i1,j1,m,n
   read*
   X(i)    = p(m,n)
!  X(i)    = p(ip(m),jp(n)) 
  enddo
  return
  end

  subroutine compute_Htilde(H,no,nv,Xf,ne,Ht)
  implicit none
  integer ne,nv,no
  real H(no,nv),Xf(nv,ne),Ht(no,ne)
  integer i,j,k,m,n
  Ht       = matmul(H,Xf)
  return
  end

  subroutine observational_err_cov_mtx(R,no,ovar)
  implicit none
  integer no,i
  real R(no,no),ovar
  R        = 0
  do i     = 1,no
   R(i,i)  = ovar**2
  enddo
  return
  end


  subroutine compute_Rinv(R,Rinv,no)
  implicit none
  integer no,i
  real R(no,no),Rinv(no,no)
  Rinv     = 0.
  do i     = 1,no
   Rinv(i,i) = 1/R(i,i)
  enddo
  return
  end
  
  subroutine obs_increment(H,no,nv,xfm,po,ne,obs_inc)
  implicit none
  integer no,ne,nv
  real H(no,nv),xfm(nv),po(no),obs_inc(nv)
  real tem
  integer i,j
  do i     = 1,no
   tem     = 0.
   do j    = 1,nv
    tem    = tem + H(i,j)*xfm(j)
   enddo
   obs_inc(i) = po(i) - tem
  enddo
  return
  end

  subroutine analysis_mean(K,lopt,nv,no,xfm,obs_inc,xam)
  implicit none
  integer no,nv
  real K(nv,no),xfm(nv),obs_inc(no),xam(nv),lopt(nv,no)
  integer i,j
  do i     = 1,nv
   xam(i)  = xfm(i)
   do j    = 1,no
    xam(i) = xam(i) + lopt(i,j)*K(i,j)*obs_inc(j)
   enddo
  enddo
  return
  end

  subroutine convert_vector_array(Xa,nx,ny,nv,pa)
  implicit none
  integer nv,nx,ny
  real Xa(nv),pa(nx,ny)
  integer i,j,k,m,n
  do i   = 1,nx
   do j  = 1,ny
    m    = (j-1)*nx + i
    pa(i,j)  = Xa(m)
   enddo
  enddo
  return
  end

  subroutine convert_vector_array1(Xa,nv,pa,nx,ny)
  implicit none
  integer nv,nx,ny
  real Xa(nv),pa(nx,ny)
  integer i,j,k,m,n
  do i       = 1,nx
   do j      = 1,ny
    m        = (j-1)*nx + i
    pa(i,j)  = Xa(m)
   enddo
  enddo
  return
  end

  SUBROUTINE input_namelist(debug,ovar,bvar,model_flag,ini_flag,ifactor,nme,no,ne,nxl,x)
  INCLUDE "../inc/L40.inc"
  RETURN
  END

