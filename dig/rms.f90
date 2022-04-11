    program plot_root_mean_squared_error
    integer,parameter    :: nt1=5000,nt2=7
    real, dimension(nt1) :: no,pf,mf 
    real, dimension(nt1) :: fem,mem,cem,sem
    integer              :: i,j,k,loop,irec,nt
    real                 :: tem1,tem2,tem3
!
! rms for 1-step window with 20 member
!
    open(10,file='1step_20mem.txt')
    read(10,*)
    print*,'open file and read'
    i         = 1
91  read(10,*,end=92,err=92)no(i),pf(i),mf(i)
    print*,i,no(i),pf(i),mf(i)
    i         = i + 1
    goto 91
92  nt        = i - 1
    print*,'number of time is',nt
    read*
    open(4,FILE='rms1step.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nt*4)
    irec      = 1
    write(4,rec=irec)(no(i),i=1,nt)  
    irec      = irec + 1            
    write(4,rec=irec)(pf(i),i=1,nt)  
    irec      = irec + 1  
    write(4,rec=irec)(mf(i),i=1,nt)
    close(10)
    close(4)
!
! rms for 2-step window with 20 member
!
    open(10,file='2step_20mem.txt')
    read(10,*)
    print*,'open file and read'
    i         = 1
81  read(10,*,end=82,err=82)no(i),pf(i),mf(i)
    print*,i,no(i),pf(i),mf(i)
    i         = i + 1
    goto 81
82  nt        = i - 1
    print*,'number of time is',nt
    read*
    open(4,FILE='rms2step.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nt*4)
    irec      = 1
    write(4,rec=irec)(no(i),i=1,nt)  
    irec      = irec + 1            
    write(4,rec=irec)(pf(i),i=1,nt)  
    irec      = irec + 1  
    write(4,rec=irec)(mf(i),i=1,nt)
    close(10)
    close(4)
!
! rms for 4-step window with 20 member
!
    open(10,file='4step_20mem.txt')
    read(10,*)
    print*,'open file and read'
    i         = 1
71  read(10,*,end=72,err=72)no(i),pf(i),mf(i)
    print*,i,no(i),pf(i),mf(i)
    i         = i + 1
    goto 71
72  nt        = i - 1
    print*,'number of time is',nt
    read*
    open(4,FILE='rms4step.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nt*4)
    irec      = 1
    write(4,rec=irec)(no(i),i=1,nt)  
    irec      = irec + 1            
    write(4,rec=irec)(pf(i),i=1,nt)  
    irec      = irec + 1  
    write(4,rec=irec)(mf(i),i=1,nt)
    close(10)
    close(4)
!
! rms for 5-step window with 20 member
!
    open(10,file='5step_20mem.txt')
    read(10,*)
    print*,'open file and read'
    i         = 1
61  read(10,*,end=62,err=62)no(i),pf(i),mf(i)
    print*,i,no(i),pf(i),mf(i)
    i         = i + 1
    goto 61
62  nt        = i - 1
    print*,'number of time is',nt
    read*
    open(4,FILE='rms5step.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nt*4)
    irec      = 1
    write(4,rec=irec)(no(i),i=1,nt)  
    irec      = irec + 1            
    write(4,rec=irec)(pf(i),i=1,nt)  
    irec      = irec + 1  
    write(4,rec=irec)(mf(i),i=1,nt)
    close(10)
    close(4)
!
! rms for 8-step window with 20 member
!
    open(10,file='8step_20mem.txt')
    read(10,*)
    print*,'open file and read'
    i         = 1
51  read(10,*,end=52,err=52)no(i),pf(i),mf(i)
    print*,i,no(i),pf(i),mf(i)
    i         = i + 1
    goto 51
52  nt        = i - 1
    print*,'number of time is',nt
    read*
    open(4,FILE='rms8step.dat',FORM='UNFORMATTED',ACCESS='DIRECT',RECL=nt*4)
    irec      = 1
    write(4,rec=irec)(no(i),i=1,nt)  
    irec      = irec + 1            
    write(4,rec=irec)(pf(i),i=1,nt)  
    irec      = irec + 1  
    write(4,rec=irec)(mf(i),i=1,nt)
    close(10)
    close(4)
    end
