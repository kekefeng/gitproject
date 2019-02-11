module control

  integer  :: prntlev=5
  
  interface report
     module procedure  sys_report
  end interface
  interface printlevl
     module procedure  set_print_levl
  end interface

  interface printinfo
     module procedure print_info
     module procedure print_info2
  end interface

  
  interface getdatetime
     module procedure get_datetime
  end interface  
  interface getdate
     module procedure get_date
  end interface
  interface gettime
     module procedure get_time
  end interface

  interface systemcmd
     module procedure  systemqq
  end interface

  private :: set_print_levl,sys_report,print_info, print_info2 
  private :: systemqq,get_date,get_time,get_datetime
contains
  subroutine err_stop(info,id)
    !
    ! PROGRAM STOPS AT ERROR
    !
    implicit none
    character*(*),intent(in)  :: info
    integer,optional,intent(in)   :: id
    
    if(.not. present(id)) write(*,'(A,A)') 'ERRSTP> '//info
    if(present(id).and.id<1E08) write(*,'(A,I8)') 'ERRSTP> '//info,id
    if(present(id).and.id>=1E08) write(*,'(A,I12)') 'ERRSTP> '//info,id
    stop
    
    return
  end subroutine err_stop
  subroutine err_warning(info,id)
    !
    ! WARNING ISSUED, PROGRAM CONTINUES
    !
    implicit none
    character*(*),intent(in)  :: info
    integer,optional,intent(in)   :: id
    
    if(.not. present(id)) write(*,'(A,A)') 'WARNING> '//info
    if(present(id).and.id<1E08) write(*,'(A,I8)') 'WARNING> '//info,id
    if(present(id).and.id>=1E08) write(*,'(A,I12)') 'WARNING> '//info,id
    write(*,*) 
    !
    return
  end subroutine err_warning

  subroutine print_info(info,id)
    implicit none
    character*(*),intent(in)  :: info
    integer,optional,intent(in)   :: id
    
    if(.not. present(id)) write(*,'(A)') info
    if(present(id).and.id<1E08) write(*,'(A,I8)') info,id
    if(present(id).and.id>=1E08) write(*,'(A,I12)') info,id
    !
    return
  end subroutine print_info
  subroutine print_info2(info,val)
    implicit none
    character*(*),intent(in)     :: info
    real*8,intent(in)            :: val
    
    if(val.gt.1.D10.or.val.lt.-1.D10) then
       write(*,'(A,D25.16)') info,val
    else
       write(*,'(A,F25.16)') info,val
    end if
    !
    return
  end subroutine print_info2

  subroutine sys_report(info,id)
    !
    ! WARNING ISSUED, PROGRAM CONTINUES
    !
    implicit none
    character*(*),intent(in)  :: info
    integer,optional,intent(in)   :: id
    
    if(.not. present(id)) write(*,'(A,A)') ' REPORT> ',info
    if(present(id)) write(*,'(3A,I8)') ' REPORT> ',info, '  _PROGRESS: ',id
    write(*,*) 

    return
  end subroutine sys_report

  subroutine set_print_levl(prntlev_)
    implicit none
    integer,intent(in)  :: prntlev_

    prntlev=prntlev_

    return  
  end subroutine set_print_levl


  subroutine systemcall(info)
    implicit none
    character*(*),intent(in)  :: info

    print*, "SYS> ", trim(info),"<<<<<<<<<<"
    call system(info)
    return

    print*, "--------------"
    call system("/xcommon/bin/msmsp -q -level  -p ./1jef.pdb -o  aaa.msms")
    call system("ls -lta ")

    return
  end subroutine systemcall

  SUBROUTINE PRINT_TIME(STR,T1,T2)
    IMPLICIT NONE
    CHARACTER*(*) :: STR
    REAL*8        :: T1,T2,SECONDS
    INTEGER       :: HOURS,MINUTES
    SECONDS=T2-T1
    HOURS=FLOOR(SECONDS)/3600
    SECONDS=SECONDS-HOURS*3600
    MINUTES=FLOOR(SECONDS)/60
    SECONDS=SECONDS-MINUTES*60
    IF(HOURS>0)THEN
       PRINT'(A20,I2,A,I2,A,F6.3,A/)',ADJUSTL(STR),HOURS,' Hour(s) ',&
            MINUTES,' Minute(s) ',SECONDS,' Second(s)'
    ELSEIF(MINUTES>0)THEN
       PRINT'(A20,I2,A,F6.3,A/)',ADJUSTL(STR),MINUTES,' Minute(s) ',&
            SECONDS,' Second(s)'
    ELSE
       PRINT'(A20,F6.3,A/)',ADJUSTL(STR),SECONDS,' Second(s)'
    END IF
    RETURN
  END SUBROUTINE PRINT_TIME

  subroutine get_datetime(datetime)
    implicit none
    character*(*)  :: datetime
    character      :: date*10,now*8
    !
    call get_date(date)
    call get_time(now)
    datetime=date//" at "//now
    !
    return
  end subroutine get_datetime
  subroutine get_date(date)
    implicit none
    integer*4      :: today(3)  !month,day,year
    character*(*)  :: date
    !
100 format(i4.4,'-',i2.2,'-',i2.2)
    call idate(today)
    write(date,100) today(3),today(2),today(1)
    !
    return
  end subroutine get_date
  subroutine get_time(time)
    implicit none
    integer*4      :: now(3)  !month,day,year
    character*(*)  :: time
    !
100 format(i2.2,':',i2.2,':',i2.2)
    call itime(now)
    write(time,100) now(1),now(2),now(3)
    !
    return
  end subroutine get_time



  subroutine systemqq(cmd)
    implicit none
    character(*),intent(in)  :: cmd
    !
    call system(cmd)
    !
    return
  end subroutine systemqq


end module control

  module constant
    implicit none
    real*8,parameter    :: PI=3.14159265358979323846D0, TWOPI=2.0D0*PI,HALFPI=PI*0.5D0
    real*8,parameter    :: RADDEG=180.0D0/PI, DEGRAD=PI/180.0D0
    real*8,parameter    :: COSMAX=0.9999999999D0
    real*8,parameter    :: SPEEDL=2.99793D-02
    real*8,parameter    :: TOKCAL=627.5095D0
    real*8,parameter    :: JKBOLTZ=1.380662D-23, AVOGADRO=6.022045D23
    real*8,parameter    :: JKCAL=4186.05D0, KCALMOL=JKCAL/AVOGADRO
    real*8,parameter    :: KBOLTZ=JKBOLTZ*AVOGADRO/JKCAL
    real*8,parameter    :: AMU=1.6605655D-27, ANGSTROM=1.0D-10
    real*8,parameter    :: UNDEFVAL=9999.99D0, EPS=1.D-13, EV_ZERO=1.D-8
!
    real*8,parameter    :: CNVFRQ=2045.5D0/(2.99793D0*6.28319D0)
!                          freq=cnvfrq*dsqrt(dabs(ev))
! Machine constants (these are very machine dependent).
!
! RPRECI should be the smallest number you can add to 1.0 and get a number
! that is different than 1.0.  Actually: the following code must pass for
! for all real numbers A (where no overflow or underflow conditions exist).
! 
!         B = A * RPRECI
!         C = A + B
!         IF(C.EQ.A) STOP 'precision variable is too small'
! 
! The RBIGST value should be the smaller of:
! 
!         - The largest real value               
!         - The reciprocal of the smallest real value 
!
! If there is doubt, be conservative.
    real*8,parameter    :: RPRECI=2.22045D-16, RBIGST=4.49423D+307

    real*8, parameter,dimension(12) :: bondlen=(/ &
         1.46,     &            !N(i) CA(i)
         1.53,     &            !CA(i) C(i); C C
         1.33,     &            !C(i) N(i+1); ND1 CE1 (HIS) NE2 CE1 (HIS)
         1.24,     &            !C(i) O(i)
         1.53,     &            !CA(i) CB(i)
         1.42,     &            !CB,OG (SER)
         1.49,     &            !CE,NZ (LYS); CB CG (PRO)
         1.81,     &            !CB,SG (CYS)
         1.38,     &            !CG,CD1 (PHE); CZ,OH (TYR); CG,ND1 (HIS)
         1.50,     &            !CB,CG (TRP,HIS) CG,CD (PRO)
         1.36,     &            !CG,CD1 (TRP)
         1.39/)                 !CE2,CZ2 (TRP) CH2,CZ3 (TRP)


    real*8, parameter,dimension(16) ::  bondangle=(/ &
         (1-110/180.)*pi,    &  !N(i) CA(i) C(i); N(i) CA(i) CB(i); CCC
         (1-114./180.)*pi,   &  !CA(i) C(i) N(i+1) ;CB CG1 CD1;CA CB SG (CYS)
         (1-123./180.)*pi,   &  !C(i) N(i+1) CA(i+1); CB CG ND1(HIS)
         (1-125./180.)*pi,   &  !N(i+1) CA(i) O(i)
         124./180.*pi,       &  !N(i) CA(i) C(i) CB(i)
         120./180.*pi,       &  !CG1,CA,CB,CG2 (VAL)
         !- CG1,CA,CB,CG2 (ILE)
         63.7/180.*pi,       &  !CA,CB,CG (LEU); CB,CG,ND2 (ASN,GLN)
         67.4/180.*pi,       &  !CA,CB,CG (ASP,ASN); CB,CG,CD (GLU,GLN)
         !CB,CG,SD (MET); ND1,CE1,NE2(HIS)
         61.6/180.*pi,       &  !CB,CG,CD1 (ASP); CE2,CZ2,CH2 (TRP)
         59./180.*pi,        &  !CB,CG,CD1 (ASN,GLN,PHE); CE1,CZ,OH (TYR)
         !CZ2,CH2,CZ3 (TYR)
         79./180.*pi,        &  !CG,CD,CE (MET)
         53./180.*pi,        &  !CB,CG,CD1 (TRP)
         70./180.*pi,        &  !CG,CD1,NE1 (TRP); CG,ND1,CE1(HIS)
         50./180.*pi,        &  !NE2,CE2,CZ2 (TRP)
         73./180.*pi,        &  !CE1,NE2,CD2 (HIS); CB,CG,CD (PRO)
         75.5/180.*pi/)         !CA,CB,CG (PRO)
    
    real*8, parameter,dimension(4) :: mass=(/ &
         14.,12.,16.,32./)     !N,C,O,S


  end module constant

  module number
    implicit none
    integer,parameter    :: IZERO=0, IONE=1, ITWO=2, ITHREE=3,IFOUR=4

    real*8,parameter     :: ZERO=0.D0, ONE=1.D0, TWO=2.D0
    real*8,parameter     :: THREE=3.D0, FOUR=4.D0, FIVE=5.D0
    real*8,parameter     :: SIX=6.D0, SEVEN=7.D0, EIGHT=8.D0
    real*8,parameter     :: NINE=9.D0, TEN=10.D0, ELEVEN=11.D0
    real*8,parameter     :: TWELVE=12.D0, THIRTN=13.D0, FIFTN=15.D0
    real*8,parameter     :: NINETN=19.D0, TWENTY=20.D0, THIRTY=30.D0
    real*8,parameter     :: FIFTY=50.D0, SIXTY=60.D0, SVNTY2=72.D0
    real*8,parameter     :: EIGHTY=80.D0, NINETY=90.D0, HUNDRD=100.D0
    real*8,parameter     :: ONE2TY=120.D0, ONE8TY=180.D0, THRHUN=300.D0
    real*8,parameter     :: THR6TY=360.D0, NINE99=999.D0, FIFHUN=1500.D0
    real*8,parameter     :: THOSND=1000.D0, FTHSND=5000.D0, MEGA=1.0D6  
    real*8,parameter     :: TENM20=1.0D-20, TENM14=1.0D-14, TENM8=1.0D-8
    real*8,parameter     :: TENM5=1.0D-5, PT0001=1.0D-4, PT0005=5.0D-4
    real*8,parameter     :: PT001=1.0D-3, PT005=5.0D-3, PT01=0.01D0
    real*8,parameter     :: PT02=0.02D0, PT05=0.05D0, PTONE=0.1D0
    real*8,parameter     :: PT125=0.125D0, SIXTH=ONE/SIX,PT25=0.25D0
    real*8,parameter     :: THIRD=ONE/THREE,PTFOUR=0.4D0, HALF=0.5D0
    real*8,parameter     ::  PTSIX=0.6D0, PT75=0.75D0, PT9999=0.9999D0
    real*8,parameter     ::  ONEPT5=1.5D0, TWOPT4=2.4D0

    real*8,parameter     :: MINUONE=-1.D0

  contains
    function mod2(m,n)
      implicit none
      integer,intent(in)       :: m, n
      integer                  :: mod2
      !
      mod2=mod(m,n)
      if(mod2.eq.0) mod2=n 
      !
      return
    end function mod2

  end module number




module diag
  use constant
  use number
  use control
  !   
!  interface charmmdiag
!     module procedure charmmdiag1,charmmdiag2,charmmdiag3 ,charmmdiag4
!     module procedure charmdiagq
!  end interface

  interface cdiag
     module procedure charmmdiag1,charmmdiag2,charmmdiag3,charmmdiag4
     module procedure charmmdiag1_int8,charmmdiag2_int8,charmmdiag3_int8 !,charmmdiag4_int8
  end interface cdiag
!  interface gdiag
!     module procedure GENERALIZED_DIAG,Use_Dsbgv
!  end interface gdiag
  !
  private  :: charmmdiag1,charmmdiag2,charmmdiag3 ,charmmdiag4,charmm_diagq
  private  :: charmmdiag1_int8,charmmdiag2_int8,charmmdiag3_int8,charmmdiag4_int8
  private  :: charmm_diagq_int8,generate_zero_mode_unit !,GENERALIZED_DIAG,Use_Dsbgv
  !
contains
  !
  subroutine charmmdiag1(N,nmode,hessian,ev,vec) !hessian -- not allocatable
    implicit none
    integer,intent(in)    ::  n,nmode      !N IS THE DIMENSION OF DOF
    real*8,intent(inout)  ::  hessian(*)
    real*8,allocatable,dimension(:),intent(inout)  ::  ev
    real*8,allocatable,dimension(:),optional,intent(inout)  ::  vec
    real*8,allocatable,dimension(:)  ::  vec2
    REAL*8  A(N+1),B(N+1),P(N+1),W(N+1),TA(N+1),TB(N+1),Y(N+1)
    integer               ::  NDD,NADD,ISALLOCATED
    !
    NDD=1                     !NDD is the first mode to be found. 
    NADD=NDD-1                
    !
    if(allocated(ev)) deallocate(ev)
    allocate(ev(n),stat=isallocated)          !create atom link for a residue
    if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR EV(N) IN DIAG');
    !
    if(present(vec)) then
       if(allocated(vec)) deallocate(vec)
       allocate(vec(n*nmode),stat=isallocated)          !create atom link for a residue
       if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR VEC(N*NMODE) IN DIAG');
       call CHARMM_DIAGQ(N,NMODE,A,B,P,W,TA,TB,Y,NADD,hessian,ev,vec)
    else
       allocate(vec2(n*nmode),stat=isallocated)          !create atom link for a residue
       if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR VEC(N*NMODE) IN DIAG');
       call CHARMM_DIAGQ(N,NMODE,A,B,P,W,TA,TB,Y,NADD,hessian,ev,vec2)
       deallocate(vec2)
    end if
    !
    return
  end subroutine charmmdiag1
  subroutine charmmdiag2(N,nmode,SYSA,ev,vec)
    implicit none
    integer,intent(in)    ::  n,nmode      !N IS THE DIMENSION OF DOF
    real*8,intent(in)     ::  SYSA(n,*)
    real*8,allocatable,dimension(:),intent(inout)  ::  ev
    real*8,allocatable,dimension(:),optional,intent(inout)  ::  vec
    real*8,allocatable,dimension(:)  ::  hessian, vec2
    REAL*8  A(N+1),B(N+1),P(N+1),W(N+1),TA(N+1),TB(N+1),Y(N+1)
    integer               ::  NDD,NADD,ISALLOCATED,NDIM
    integer               ::  i,j,kh
    !
    NDD=1                     !NDD is the first mode to be found. 
    NADD=NDD-1                
    !   
    ndim=(n*n+n)/2
    allocate(hessian(ndim),stat=isallocated)          !create atom link for a residue
    if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR EV(N) IN DIAG');
    kh=0
    do i=1,n
       do j=i,n
          kh=kh+1
          hessian(kh)=SYSA(i,j)
       end do
    end do
    !
    if(allocated(ev)) deallocate(ev)
    allocate(ev(n),stat=isallocated)          !create atom link for a residue
    if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR EV(N) IN DIAG');
    !
    if(present(vec)) then
       if(allocated(vec)) deallocate(vec)
       allocate(vec(n*nmode),stat=isallocated)          !create atom link for a residue
       if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR VEC(N*NMODE) IN DIAG');
       call CHARMM_DIAGQ(N,NMODE,A,B,P,W,TA,TB,Y,NADD,hessian,ev,vec)
    else
       allocate(vec2(n*nmode),stat=isallocated)          !create atom link for a residue
       if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR VEC(N*NMODE) IN DIAG');
       call CHARMM_DIAGQ(N,NMODE,A,B,P,W,TA,TB,Y,NADD,hessian,ev,vec2)
       deallocate(vec2)
    end if
    !
    deallocate(hessian)
    !
    return
  end subroutine charmmdiag2
  subroutine charmmdiag3(N,SYSA,ev,vec)
    implicit none
    integer,intent(in)      ::  n      !N IS THE DIMENSION OF DOF
    real*8,intent(in)       ::  SYSA(n,*)
    real*8,intent(inout)    ::  ev(*),vec(n,*)
    REAL*8      A(N+1),B(N+1),P(N+1),W(N+1),TA(N+1),TB(N+1),Y(N+1)
    integer               ::  NDD,NADD,ISALLOCATED,NDIM
    real*8,allocatable,dimension(:)  ::  hessian
    integer               ::  nmode,i,j,kh
    
    !
    NDD=1                     !NDD is the first mode to be found. 
    NADD=NDD-1                
    NMODE=N
    !   
    ndim=(n*n+n)/2
    allocate(hessian(ndim),stat=isallocated)  !create atom link for a residue
    if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR EV(N) IN DIAG');
    kh=0
    do i=1,n
       do j=i,n
          kh=kh+1
          hessian(kh)=SYSA(i,j)
       end do
    end do
    !
    call CHARMM_DIAGQ(N,NMODE,A,B,P,W,TA,TB,Y,NADD,hessian,ev,vec)
    !
    deallocate(hessian)
    !
    return
  end subroutine charmmdiag3
  subroutine charmmdiag4(N,NMODE,hessian,ev,vec)
    implicit none
    integer,intent(in)      ::  n,nmode      !N IS THE DIMENSION OF DOF
    real*8,intent(inout)    ::  hessian(*)
    real*8,intent(inout)    ::  ev(*),vec(n,*)
    REAL*8                  ::  A(N+1),B(N+1),P(N+1),W(N+1),TA(N+1),TB(N+1),Y(N+1)
    integer                 ::  NDD,NADD
    !
    NDD=1                     !NDD is the first mode to be found. 
    NADD=NDD-1
    !
    if(nmode.le.0) then
       write(*,*) 'WARNING> ZERO MODES CLAIMED, NO DIAG CALLED'
       return
    end if
    !   
    call CHARMM_DIAGQ(N,NMODE,A,B,P,W,TA,TB,Y,NADD,hessian,ev,vec)
    !
    return
  end subroutine charmmdiag4
  SUBROUTINE CHARMM_DIAGQ(NX,NFRQX,A,B,P,W,TA,TB,Y,NADD,DD,EV,VEC)
      !
      !     THIS ROUTINE IS A CONGLOMERATION OF GIVEN, HOUSEC, AND EIGEN
      !     WHERE THE BEST FEATURES OF EACH WERE KEPT AND SEVERAL OTHER
      !     MODIFICATIONS HAVE BEEN MADE TO INCREASE EFFICIENCY AND ACCURACY.
      !
      !   By Bernard R. Brooks   1981
      !
      !   NX      - ORDER OF MATRIX
      !   NFRQX   - NUMBER OF ROOTS DESIRED
      !   DD      - SECOND DERIVATIVE MATRIX IN UPPER TRIANGULAR FORM
      !   VEC     - EIGENVECTORS RETURNED (NX,NFRQX)
      !   EV      - EIGENVALUES RETURNED (NX)
      !   A,B,P,W,TA,TB,Y - ALL SCRATCH VECTORS (NX+1)
      !   NADD    - NUMBER OF LOWEST ROOTS TO SKIP
      !
      !
      implicit none
      !
      INTEGER NX,NFRQX,NAD
      REAL*8,INTENT(INOUT)   ::  DD(*)
      REAL*8,INTENT(INOUT)   ::  EV(*),VEC(*)
      REAL*8 A(NX),B(NX),P(NX),W(NX),TA(NX),TB(NX),Y(NX)
      ! 
      REAL*8 ETA,THETA,DEL1,DELTA,SMALL,DELBIG,THETA1,TOLER,ETAR
      REAL*8 RPOWER,RPOW1,RAND1,FACTOR,ANORM,U,ANORMR
      REAL*8 SUM1,BX,S,SGN,TEMP,XKAP,EXPR,ALIMIT,ROOTL,ROOTX,TRIAL,F0
      REAL*8 AROOT,ELIM1,ELIM2,T,EPR,XNORM,XNORM1,EVDIFF
      INTEGER N,NADD,NEV,NEVADD,NTOT,I,IPT,J,IJ,NN,MI,MI1,JI,JI2,II
      INTEGER ML,ML1,L,M,K,MJ,MJ1,NOMTCH,NOM,IA,ITER
      INTEGER J1,MK,MK1,KK
      !
      REAL*8 ANUMX
      !
      ANUMX=ZERO
      DO I=1,NX
         A(I)=ANUMX
         B(I)=ANUMX
         P(I)=ANUMX 
         W(I)=ANUMX
         EV(I)=ANUMX
         TA(I)=ANUMX
         TB(I)=ANUMX
         Y(I)=ANUMX
      END DO
      !
      ETA=RPRECI
      THETA=RBIGST
      !
      N=NX
      NEV=NFRQX
      NEVADD=NEV+NADD
      !
      DEL1=ETA/100.0
      DELTA=ETA**2*100.0
      SMALL=ETA**2/100.0
      DELBIG=THETA*DELTA/1000.0
      THETA1=1000.0/THETA
      TOLER=100.0*ETA
      ETAR=1.0/ETA
      RPOWER=8388608.0
      RPOW1=RPOWER*0.50
      RAND1=RPOWER-3.0
      !
      ! Find largest element.
      FACTOR=ZERO
      NTOT=(N*(N+1))/2
      DO I=1,NTOT
         FACTOR=MAX(FACTOR,ABS(DD(I)))
      ENDDO
      !
      ! Check for zero matrix.
      IF(FACTOR.LE.THETA1) THEN
         WRITE(*,811)
811      FORMAT(' WARNING FROM <DIAGQ>. Zero matrix passed.',&
              ' Identity matrix returned.')
         DO I=1,NEV
            EV(I)=ZERO
            IPT=(I-1)*N
            DO J=1,N
               IPT=IPT+1
               VEC(IPT)=ZERO
               IF(I+NADD.EQ.J) VEC(IPT)=ONE
            ENDDO
         ENDDO
         RETURN
      ENDIF
      !
      ! Compute norm of matrix
      FACTOR=ONE/FACTOR
      IJ=0
      ANORM=ZERO
      DO I=1,N
         DO J=I,N
            IJ=IJ+1
            U=(DD(IJ)*FACTOR)**2
            IF(I.EQ.J) U=U*HALF
            ANORM=ANORM+U
         ENDDO
      ENDDO
      !
      ! Scale the matrix
      ANORM=SQRT(ANORM+ANORM)/FACTOR
      ANORMR=ONE/ANORM
      DO I=1,NTOT
         DD(I)=DD(I)*ANORMR
      ENDDO
      !
      NN=N-1
      MI=0
      MI1=N-1
      !
      ! Perform trigiagonalization
      DO I=1,NN
         SUM1=ZERO
         B(I)=ZERO
         JI=I+1
         IPT=MI+I
         A(I)=DD(IPT)
         IPT=IPT+1
         BX=DD(IPT)
         JI2=JI+1
         DO J=JI2,N
            IPT=IPT+1
            SUM1=SUM1+DD(IPT)*DD(IPT)
         ENDDO
         IF(SUM1.LT.SMALL) THEN
            B(I)=BX
            DD(MI+JI)=ZERO
         ELSE
            S=SQRT(SUM1+BX**2)
            SGN=SIGN(ONE,BX)
            TEMP=ABS(BX)
            W(JI)=SQRT(HALF*(ONE+(TEMP/S)))
            IPT=MI+JI
            DD(IPT)=W(JI)
            II=I+2
            IF(II.LE.N) THEN
               TEMP=SGN/(TWO*W(JI)*S)
               DO J=II,N
                  IPT=IPT+1
                  W(J)=TEMP * DD(IPT)
                  DD(IPT)=W(J)
               ENDDO
            ENDIF
            B(I)=-SGN*S
            !
            DO J=JI,N
               P(J)=ZERO
            ENDDO
            ML=MI + MI1
            ML1=MI1-1
            DO L=JI,N
               IPT=ML+L
               DO M=L,N
                  BX=DD(IPT)
                  P(L)=P(L)+BX*W(M)
                  IF(L.NE.M) P(M)=P(M)+BX*W(L)
                  IPT=IPT+1
               ENDDO
               ML=ML +ML1
               ML1=ML1-1
            ENDDO
            !
            !
            XKAP=ZERO
            DO K=JI,N
               XKAP=XKAP+W(K)*P(K)
            ENDDO
            DO L=JI,N
               P(L)=P(L)-XKAP*W(L)
            ENDDO
            MJ=MI+MI1
            MJ1=MI1-1
            DO J=JI,N
               DO K=J,N
                  EXPR=(P(J)*W(K))+(P(K)*W(J))
                  DD(MJ+K)=DD(MJ+K)-EXPR-EXPR
               ENDDO
               MJ=MJ+MJ1
               MJ1=MJ1-1
            ENDDO
         ENDIF
         MI=MI+MI1
         MI1=MI1-1
      END DO
      !
      ! Begin sturm bisection method.
      !
      A(N)=DD(MI+N)
      B(N)=ZERO
      !
      ALIMIT=ONE
      DO I=1,N
         W(I)=B(I)
         B(I)=B(I)*B(I)
      ENDDO
      DO I=1,NEVADD
         EV(I)=ALIMIT
      ENDDO
      ROOTL=-ALIMIT
      !
      DO I=1,NEVADD
         ROOTX=ALIMIT
         DO J=I,NEVADD
            ROOTX=MIN(ROOTX,EV(J))
         ENDDO
         EV(I)=ROOTX
         !
130      CONTINUE
         TRIAL=(ROOTL+EV(I))*HALF
         !##IF CRAY (oldcode_for_cray)
         !            IF(TRIAL.EQ.ROOTL.OR.TRIAL.EQ.EV(I)) GOTO 200
         !##ELSE (oldcode_for_cray)
         EVDIFF=ABS(ROOTL-EV(I))
         IF(EVDIFF.LT.THETA1) GOTO 200
         IF(EVDIFF*ETAR.LT.ABS(TRIAL)) GOTO 200
         !##ENDIF (oldcode_for_cray)
         NOMTCH=N
         J=1
150      CONTINUE
         F0=A(J)-TRIAL
160      CONTINUE
         IF(ABS(F0).LT.THETA1) GOTO 170
         IF(F0.GE.ZERO) NOMTCH=NOMTCH-1
         J=J+1
         IF(J.GT.N) GOTO 180
         F0=A(J)-TRIAL-B(J-1)/F0
         GOTO160
170      CONTINUE
         J=J+2
         NOMTCH=NOMTCH-1
         IF(J.LE.N) GOTO 150
180      CONTINUE
         IF(NOMTCH.GE.I) GOTO 190
         ROOTL=TRIAL
         GOTO 130
190      CONTINUE
         EV(I)=TRIAL
         NOM=MIN(NEVADD,NOMTCH)
         !            NOM=MIN0(NEVADD,NOMTCH)   		!original 
         EV(NOM)=TRIAL
         GOTO 130
200      CONTINUE
      ENDDO
      !
      ! Finished computing requested eigenvalues
      DO I=1,NEV
         EV(I)=EV(I+NADD)
      ENDDO
      !
      ! Compute eigenvectors (backtransformation)
      DO I=1,NEV
         AROOT=EV(I)
         DO J=1,N
            Y(J)=ONE
         ENDDO
         IA=IA+1
         IF(I.EQ.1) THEN
            IA=0
         ELSE
            IF(ABS(EV(I-1)-AROOT).GE.TOLER) IA=0
         ENDIF
         ELIM1=A(1)-AROOT
         ELIM2=W(1)
         DO J=1,NN
            IF(ABS(ELIM1).LE.ABS(W(J))) THEN
               TA(J)=W(J)
               TB(J)=A(J+1)-AROOT
               P(J)=W(J+1)
               TEMP=ONE
               IF(ABS(W(J)).GT.THETA1) TEMP=ELIM1/W(J)
               ELIM1=ELIM2-TEMP*TB(J)
               ELIM2=-TEMP*W(J+1)
            ELSE
               TA(J)=ELIM1
               TB(J)=ELIM2
               P(J)=ZERO
               TEMP=W(J)/ELIM1
               ELIM1=A(J+1)-AROOT-TEMP*ELIM2
               ELIM2=W(J+1)
            ENDIF
            B(J)=TEMP
         ENDDO
         !
         TA(N)=ELIM1
         TB(N)=ZERO
         P(N)=ZERO
         P(NN)=ZERO
         ITER=1
         IF(IA.NE.0) GOTO 460
         !
320      L=N+1
         DO J=1,N
            L=L-1
330         CONTINUE
            IF(L.EQ.N) THEN
               ELIM1=Y(L)
            ELSE IF(L.EQ.N-1) THEN
               ELIM1=Y(L)-Y(L+1)*TB(L)
            ELSE
               ELIM1=Y(L)-Y(L+1)*TB(L)-Y(L+2)*P(L)
            ENDIF
            !
            ! Overflow check
            IF(ABS(ELIM1).GT.DELBIG) THEN
               DO K=1,N
                  Y(K)=Y(K)/DELBIG
               ENDDO
               GOTO 330
            ENDIF
            TEMP=TA(L)
            IF(ABS(TEMP).LT.DELTA) TEMP=DELTA
            Y(L)=ELIM1/TEMP
         ENDDO
         !
         IF(ITER.EQ.2) GOTO 500
         ITER=ITER+1
         !
420      CONTINUE
         ELIM1=Y(1)
         DO J=1,NN
            IF(TA(J).EQ.W(J)) THEN
               Y(J)=Y(J+1)
               ELIM1=ELIM1-Y(J+1)*B(J)
            ELSE
               Y(J)=ELIM1
               ELIM1=Y(J+1)-ELIM1*B(J)
            ENDIF
         ENDDO
         Y(N)=ELIM1
         GOTO 320
         !
460      CONTINUE
         DO J=1,N
            RAND1=MOD(4099.0*RAND1,RPOWER)
            Y(J)=RAND1/RPOW1-ONE
         ENDDO
         GOTO 320
         !
         ! Orthog to previous
500      IF(IA.EQ.0) GOTO 550
         DO J1=1,IA
            K=I-J1
            TEMP=ZERO
            IPT=(K-1)*N
            DO J=1,N
               IPT=IPT+1
               TEMP=TEMP+Y(J)*VEC(IPT)
            ENDDO
            IPT=(K-1)*N
            DO J=1,N
               IPT=IPT+1
               Y(J)=Y(J)-TEMP*VEC(IPT)
            ENDDO
         ENDDO
550      CONTINUE
         IF(ITER.EQ.1) GOTO 420
         !
         ! Normalize
560      CONTINUE
         ELIM1=ZERO
         DO J=1,N
            ELIM1=MAX(ELIM1,ABS(Y(J)))
         ENDDO
         TEMP=ZERO
         DO J=1,N
            ELIM2=Y(J)/ELIM1
            TEMP=TEMP+ELIM2*ELIM2
         ENDDO
         TEMP=ONE/(SQRT(TEMP)*ELIM1)
         DO J=1,N
            Y(J)=Y(J)*TEMP
            IF(ABS(Y(J)).LT.DEL1) Y(J)=ZERO
         ENDDO
         IPT=(I-1)*N
         DO J=1,N
            IPT=IPT+1
            VEC(IPT)=Y(J)
         ENDDO
      ENDDO
      !   
      DO I=1,NEV
         IPT=(I-1)*N
         DO J=1,N
            IPT=IPT+1
            Y(J)=VEC(IPT)
         ENDDO
         !
         L=N-2
         MK=(N*(N-1))/2-3
         MK1=3
         !
         DO J=1,L
            T=ZERO
            K=N-J-1
            M=K+1
            DO KK=M,N
               T=T+DD(MK+KK)*Y(KK)
            ENDDO
            DO KK=M,N
               EPR=T*DD(MK+KK)
               Y(KK)=Y(KK)-EPR-EPR
            ENDDO
            MK=MK-MK1
            MK1=MK1+1
         ENDDO
         !
         T=ZERO
         DO J=1,N
            T=T+Y(J)*Y(J)
         ENDDO
         XNORM=SQRT(T)
         XNORM1=ONE/XNORM
         DO J=1,N
            Y(J)=Y(J)*XNORM1
         ENDDO
         !
         IPT=(I-1)*N
         DO J=1,N
            IPT=IPT+1
            VEC(IPT)=Y(J)
         ENDDO
      ENDDO
      !
      DO I=1,N
         EV(I)=EV(I)*ANORM
      ENDDO
      !
      RETURN
    END SUBROUTINE CHARMM_DIAGQ
    !
    subroutine charmmdiag1_INT8(N,nmode,hessian,ev,vec)
      implicit none
      integer*8,intent(in)    ::  n,nmode      !N IS THE DIMENSION OF DOF
      real*8,intent(inout)    ::  hessian(*)
      real*8,allocatable,dimension(:),intent(inout)  ::  ev
      real*8,allocatable,dimension(:),optional,intent(inout)  ::  vec
      real*8,allocatable,dimension(:)  ::  vec2
      REAL*8  A(N+1),B(N+1),P(N+1),W(N+1),TA(N+1),TB(N+1),Y(N+1)
      integer*8               ::  NDD,NADD,ISALLOCATED
      !
      NDD=1                     !NDD is the first mode to be found. 
      NADD=NDD-1                
      !
      if(allocated(ev)) deallocate(ev)
      allocate(ev(n),stat=isallocated)          !create atom link for a residue
      if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR EV(N) IN DIAG');
      !
      if(present(vec)) then
         if(allocated(vec)) deallocate(vec)
         allocate(vec(n*nmode),stat=isallocated)          !create atom link for a residue
         if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR VEC(N*NMODE) IN DIAG');
         call CHARMM_DIAGQ_INT8(N,NMODE,A,B,P,W,TA,TB,Y,NADD,hessian,ev,vec)
      else
         allocate(vec2(n*nmode),stat=isallocated)          !create atom link for a residue
         if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR VEC(N*NMODE) IN DIAG');
         call CHARMM_DIAGQ_INT8(N,NMODE,A,B,P,W,TA,TB,Y,NADD,hessian,ev,vec2)
         deallocate(vec2)
      end if
      !
      return
    end subroutine charmmdiag1_INT8
    subroutine charmmdiag2_INT8(N,nmode,SYSA,ev,vec)
      implicit none
      integer*8,intent(in)    ::  n,nmode      !N IS THE DIMENSION OF DOF
      real*8,intent(in)     ::  SYSA(n,*)
      real*8,allocatable,dimension(:),intent(inout)  ::  ev
      real*8,allocatable,dimension(:),optional,intent(inout)  ::  vec
      real*8,allocatable,dimension(:)  ::  hessian, vec2
      REAL*8  A(N+1),B(N+1),P(N+1),W(N+1),TA(N+1),TB(N+1),Y(N+1)
      integer*8               ::  NDD,NADD,ISALLOCATED,NDIM
      integer*8               ::  i,j,kh
      !
      NDD=1                     !NDD is the first mode to be found. 
      NADD=NDD-1                
      !   
      ndim=(n*n+n)/2
      allocate(hessian(ndim),stat=isallocated)          !create atom link for a residue
      if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR EV(N) IN DIAG');
      kh=0
      do i=1,n
         do j=i,n
            kh=kh+1
            hessian(kh)=SYSA(i,j)
         end do
      end do
      !
      if(allocated(ev)) deallocate(ev)
      allocate(ev(n),stat=isallocated)          !create atom link for a residue
      if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR EV(N) IN DIAG');
      !
      if(present(vec)) then
         if(allocated(vec)) deallocate(vec)
         allocate(vec(n*nmode),stat=isallocated)          !create atom link for a residue
         if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR VEC(N*NMODE) IN DIAG');
         call CHARMM_DIAGQ_INT8(N,NMODE,A,B,P,W,TA,TB,Y,NADD,hessian,ev,vec)
      else
         allocate(vec2(n*nmode),stat=isallocated)          !create atom link for a residue
         if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR VEC(N*NMODE) IN DIAG');
         call CHARMM_DIAGQ_INT8(N,NMODE,A,B,P,W,TA,TB,Y,NADD,hessian,ev,vec2)
         deallocate(vec2)
      end if
      !
      deallocate(hessian)
      !
      return
    end subroutine charmmdiag2_INT8
    subroutine charmmdiag3_INT8(N,SYSA,ev,vec)
      implicit none
      integer*8,intent(in)    ::  n      !N IS THE DIMENSION OF DOF
      real*8,intent(out)      ::  SYSA(n,*)
      real*8,intent(out)      ::  ev(*),vec(n,*)
      REAL*8      A(N+1),B(N+1),P(N+1),W(N+1),TA(N+1),TB(N+1),Y(N+1)
      integer*8               ::  NDD,NADD,ISALLOCATED,NDIM
      real*8,allocatable,dimension(:)  ::  hessian
      integer*8               ::  nmode,i,j,kh
      !
      NDD=1                     !NDD is the first mode to be found. 
      NADD=NDD-1                
      NMODE=N
      !   
      ndim=(n*n+n)/2
      allocate(hessian(ndim),stat=isallocated)  !create atom link for a residue
      if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR EV(N) IN DIAG');
      kh=0
      do i=1,n
         do j=i,n
            kh=kh+1
            hessian(kh)=SYSA(i,j)
         end do
      end do
      !
      call CHARMM_DIAGQ_INT8(N,NMODE,A,B,P,W,TA,TB,Y,NADD,hessian,ev,vec)
      !
      deallocate(hessian)
      !
      return
    end subroutine charmmdiag3_INT8
    subroutine charmmdiag4_INT8(N,nmode,hessian,ev,vec)
      implicit none
      integer*8,intent(in)    ::  n,nmode      !N IS THE DIMENSION OF DOF
      real*8,allocatable,dimension(:),intent(inout)  :: hessian, ev
      real*8,allocatable,dimension(:),optional,intent(inout)  ::  vec
      real*8,allocatable,dimension(:)  ::  vec2
      REAL*8  A(N+1),B(N+1),P(N+1),W(N+1),TA(N+1),TB(N+1),Y(N+1)
      integer*8               ::  NDD,NADD,ISALLOCATED
      !
      NDD=1                     !NDD is the first mode to be found. 
      NADD=NDD-1                
      !
      if(allocated(ev)) deallocate(ev)
      allocate(ev(n),stat=isallocated)          !create atom link for a residue
      if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR EV(N) IN DIAG');
      !
      if(present(vec)) then
         if(allocated(vec)) deallocate(vec)
         allocate(vec(n*nmode),stat=isallocated)          !create atom link for a residue
         if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR VEC(N*NMODE) IN DIAG');
         call CHARMM_DIAGQ_INT8(N,NMODE,A,B,P,W,TA,TB,Y,NADD,hessian,ev,vec)
      else
         allocate(vec2(n*nmode),stat=isallocated)          !create atom link for a residue
         if(isallocated /= 0) call err_stop('CAN NOT ALLOCATE MEMORY FOR VEC(N*NMODE) IN DIAG');
         call CHARMM_DIAGQ_INT8(N,NMODE,A,B,P,W,TA,TB,Y,NADD,hessian,ev,vec2)
         deallocate(vec2)
      end if
      !
      return
    end subroutine charmmdiag4_INT8
    SUBROUTINE CHARMM_DIAGQ_INT8(NX,NFRQX,A,B,P,W,TA,TB,Y,NADD,DD,EV,VEC)
      !
      !SAME AS CHARMM_DIAGQ BUT USING INTEGER*8, DESIGNED FOR LARGE SYSTEM
      !
      implicit none
      INTEGER*8 NX,NFRQX,NAD
      REAL*8,INTENT(INOUT)   ::  DD(*)
      REAL*8,INTENT(INOUT)   ::  EV(*),VEC(*)
      REAL*8 A(NX),B(NX),P(NX),W(NX),TA(NX),TB(NX),Y(NX)
      ! 
      REAL*8 ETA,THETA,DEL1,DELTA,SMALL,DELBIG,THETA1,TOLER,ETAR
      REAL*8 RPOWER,RPOW1,RAND1,FACTOR,ANORM,U,ANORMR
      REAL*8 SUM1,BX,S,SGN,TEMP,XKAP,EXPR,ALIMIT,ROOTL,ROOTX,TRIAL,F0
      REAL*8 AROOT,ELIM1,ELIM2,T,EPR,XNORM,XNORM1,EVDIFF
      INTEGER*8 N,NADD,NEV,NEVADD,NTOT,I,IPT,J,IJ,NN,MI,MI1,JI,JI2,II
      INTEGER*8 ML,ML1,L,M,K,MJ,MJ1,NOMTCH,NOM,IA,ITER
      INTEGER*8 J1,MK,MK1,KK
      !
      REAL*8 ANUMX
      !
      ANUMX=ZERO
      DO I=1,NX
         A(I)=ANUMX
         B(I)=ANUMX
         P(I)=ANUMX 
         W(I)=ANUMX
         EV(I)=ANUMX
         TA(I)=ANUMX
         TB(I)=ANUMX
         Y(I)=ANUMX
      END DO
      !
      ETA=RPRECI
      THETA=RBIGST
      !
      N=NX
      NEV=NFRQX
      NEVADD=NEV+NADD
      !
      DEL1=ETA/100.0
      DELTA=ETA**2*100.0
      SMALL=ETA**2/100.0
      DELBIG=THETA*DELTA/1000.0
      THETA1=1000.0/THETA
      TOLER=100.0*ETA
      ETAR=1.0/ETA
      RPOWER=8388608.0
      RPOW1=RPOWER*0.50
      RAND1=RPOWER-3.0
      !
      ! Find largest element.
      FACTOR=ZERO
      NTOT=(N*(N+1))/2
      DO I=1,NTOT
         FACTOR=MAX(FACTOR,ABS(DD(I)))
      ENDDO
      !
      ! Check for zero matrix.
      IF(FACTOR.LE.THETA1) THEN
         WRITE(*,811)
811      FORMAT(' WARNING FROM <DIAGQ>. Zero matrix passed.',&
              ' Identity matrix returned.')
         DO I=1,NEV
            EV(I)=ZERO
            IPT=(I-1)*N
            DO J=1,N
               IPT=IPT+1
               VEC(IPT)=ZERO
               IF(I+NADD.EQ.J) VEC(IPT)=ONE
            ENDDO
         ENDDO
         RETURN
      ENDIF
      !
      ! Compute norm of matrix
      FACTOR=ONE/FACTOR
      IJ=0
      ANORM=ZERO
      DO I=1,N
         DO J=I,N
            IJ=IJ+1
            U=(DD(IJ)*FACTOR)**2
            IF(I.EQ.J) U=U*HALF
            ANORM=ANORM+U
         ENDDO
      ENDDO
      !
      ! Scale the matrix
      ANORM=SQRT(ANORM+ANORM)/FACTOR
      ANORMR=ONE/ANORM
      DO I=1,NTOT
         DD(I)=DD(I)*ANORMR
      ENDDO
      !
      NN=N-1
      MI=0
      MI1=N-1
      !
      ! Perform trigiagonalization
      DO I=1,NN
         SUM1=ZERO
         B(I)=ZERO
         JI=I+1
         IPT=MI+I
         A(I)=DD(IPT)
         IPT=IPT+1
         BX=DD(IPT)
         JI2=JI+1
         DO J=JI2,N
            IPT=IPT+1
            SUM1=SUM1+DD(IPT)*DD(IPT)
         ENDDO
         IF(SUM1.LT.SMALL) THEN
            B(I)=BX
            DD(MI+JI)=ZERO
         ELSE
            S=SQRT(SUM1+BX**2)
            SGN=SIGN(ONE,BX)
            TEMP=ABS(BX)
            W(JI)=SQRT(HALF*(ONE+(TEMP/S)))
            IPT=MI+JI
            DD(IPT)=W(JI)
            II=I+2
            IF(II.LE.N) THEN
               TEMP=SGN/(TWO*W(JI)*S)
               DO J=II,N
                  IPT=IPT+1
                  W(J)=TEMP * DD(IPT)
                  DD(IPT)=W(J)
               ENDDO
            ENDIF
            B(I)=-SGN*S
            !
            DO J=JI,N
               P(J)=ZERO
            ENDDO
            ML=MI + MI1
            ML1=MI1-1
            DO L=JI,N
               IPT=ML+L
               DO M=L,N
                  BX=DD(IPT)
                  P(L)=P(L)+BX*W(M)
                  IF(L.NE.M) P(M)=P(M)+BX*W(L)
                  IPT=IPT+1
               ENDDO
               ML=ML +ML1
               ML1=ML1-1
            ENDDO
            !
            !
            XKAP=ZERO
            DO K=JI,N
               XKAP=XKAP+W(K)*P(K)
            ENDDO
            DO L=JI,N
               P(L)=P(L)-XKAP*W(L)
            ENDDO
            MJ=MI+MI1
            MJ1=MI1-1
            DO J=JI,N
               DO K=J,N
                  EXPR=(P(J)*W(K))+(P(K)*W(J))
                  DD(MJ+K)=DD(MJ+K)-EXPR-EXPR
               ENDDO
               MJ=MJ+MJ1
               MJ1=MJ1-1
            ENDDO
         ENDIF
         MI=MI+MI1
         MI1=MI1-1
      END DO
      !
      ! Begin sturm bisection method.
      !
      A(N)=DD(MI+N)
      B(N)=ZERO
      !
      ALIMIT=ONE
      DO I=1,N
         W(I)=B(I)
         B(I)=B(I)*B(I)
      ENDDO
      DO I=1,NEVADD
         EV(I)=ALIMIT
      ENDDO
      ROOTL=-ALIMIT
      !
      DO I=1,NEVADD
         ROOTX=ALIMIT
         DO J=I,NEVADD
            ROOTX=MIN(ROOTX,EV(J))
         ENDDO
         EV(I)=ROOTX
         !
130      CONTINUE
         TRIAL=(ROOTL+EV(I))*HALF
         !##IF CRAY (oldcode_for_cray)
         !            IF(TRIAL.EQ.ROOTL.OR.TRIAL.EQ.EV(I)) GOTO 200
         !##ELSE (oldcode_for_cray)
         EVDIFF=ABS(ROOTL-EV(I))
         IF(EVDIFF.LT.THETA1) GOTO 200
         IF(EVDIFF*ETAR.LT.ABS(TRIAL)) GOTO 200
         !##ENDIF (oldcode_for_cray)
         NOMTCH=N
         J=1
150      CONTINUE
         F0=A(J)-TRIAL
160      CONTINUE
         IF(ABS(F0).LT.THETA1) GOTO 170
         IF(F0.GE.ZERO) NOMTCH=NOMTCH-1
         J=J+1
         IF(J.GT.N) GOTO 180
         F0=A(J)-TRIAL-B(J-1)/F0
         GOTO160
170      CONTINUE
         J=J+2
         NOMTCH=NOMTCH-1
         IF(J.LE.N) GOTO 150
180      CONTINUE
         IF(NOMTCH.GE.I) GOTO 190
         ROOTL=TRIAL
         GOTO 130
190      CONTINUE
         EV(I)=TRIAL
         NOM=MIN(NEVADD,NOMTCH)
         !            NOM=MIN0(NEVADD,NOMTCH)   		!original 
         EV(NOM)=TRIAL
         GOTO 130
200      CONTINUE
      ENDDO
      !
      ! Finished computing requested eigenvalues
      DO I=1,NEV
         EV(I)=EV(I+NADD)
      ENDDO
      !
      ! Compute eigenvectors (backtransformation)
      DO I=1,NEV
         AROOT=EV(I)
         DO J=1,N
            Y(J)=ONE
         ENDDO
         IA=IA+1
         IF(I.EQ.1) THEN
            IA=0
         ELSE
            IF(ABS(EV(I-1)-AROOT).GE.TOLER) IA=0
         ENDIF
         ELIM1=A(1)-AROOT
         ELIM2=W(1)
         DO J=1,NN
            IF(ABS(ELIM1).LE.ABS(W(J))) THEN
               TA(J)=W(J)
               TB(J)=A(J+1)-AROOT
               P(J)=W(J+1)
               TEMP=ONE
               IF(ABS(W(J)).GT.THETA1) TEMP=ELIM1/W(J)
               ELIM1=ELIM2-TEMP*TB(J)
               ELIM2=-TEMP*W(J+1)
            ELSE
               TA(J)=ELIM1
               TB(J)=ELIM2
               P(J)=ZERO
               TEMP=W(J)/ELIM1
               ELIM1=A(J+1)-AROOT-TEMP*ELIM2
               ELIM2=W(J+1)
            ENDIF
            B(J)=TEMP
         ENDDO
         !
         TA(N)=ELIM1
         TB(N)=ZERO
         P(N)=ZERO
         P(NN)=ZERO
         ITER=1
         IF(IA.NE.0) GOTO 460
         !
320      L=N+1
         DO J=1,N
            L=L-1
330         CONTINUE
            IF(L.EQ.N) THEN
               ELIM1=Y(L)
            ELSE IF(L.EQ.N-1) THEN
               ELIM1=Y(L)-Y(L+1)*TB(L)
            ELSE
               ELIM1=Y(L)-Y(L+1)*TB(L)-Y(L+2)*P(L)
            ENDIF
            !
            ! Overflow check
            IF(ABS(ELIM1).GT.DELBIG) THEN
               DO K=1,N
                  Y(K)=Y(K)/DELBIG
               ENDDO
               GOTO 330
            ENDIF
            TEMP=TA(L)
            IF(ABS(TEMP).LT.DELTA) TEMP=DELTA
            Y(L)=ELIM1/TEMP
         ENDDO
         !
         IF(ITER.EQ.2) GOTO 500
         ITER=ITER+1
         !
420      CONTINUE
         ELIM1=Y(1)
         DO J=1,NN
            IF(TA(J).EQ.W(J)) THEN
               Y(J)=Y(J+1)
               ELIM1=ELIM1-Y(J+1)*B(J)
            ELSE
               Y(J)=ELIM1
               ELIM1=Y(J+1)-ELIM1*B(J)
            ENDIF
         ENDDO
         Y(N)=ELIM1
         GOTO 320
         !
460      CONTINUE
         DO J=1,N
            RAND1=MOD(4099.0*RAND1,RPOWER)
            Y(J)=RAND1/RPOW1-ONE
         ENDDO
         GOTO 320
         !
         ! Orthog to previous
500      IF(IA.EQ.0) GOTO 550
         DO J1=1,IA
            K=I-J1
            TEMP=ZERO
            IPT=(K-1)*N
            DO J=1,N
               IPT=IPT+1
               TEMP=TEMP+Y(J)*VEC(IPT)
            ENDDO
            IPT=(K-1)*N
            DO J=1,N
               IPT=IPT+1
               Y(J)=Y(J)-TEMP*VEC(IPT)
            ENDDO
         ENDDO
550      CONTINUE
         IF(ITER.EQ.1) GOTO 420
         !
         ! Normalize
560      CONTINUE
         ELIM1=ZERO
         DO J=1,N
            ELIM1=MAX(ELIM1,ABS(Y(J)))
         ENDDO
         TEMP=ZERO
         DO J=1,N
            ELIM2=Y(J)/ELIM1
            TEMP=TEMP+ELIM2*ELIM2
         ENDDO
         TEMP=ONE/(SQRT(TEMP)*ELIM1)
         DO J=1,N
            Y(J)=Y(J)*TEMP
            IF(ABS(Y(J)).LT.DEL1) Y(J)=ZERO
         ENDDO
         IPT=(I-1)*N
         DO J=1,N
            IPT=IPT+1
            VEC(IPT)=Y(J)
         ENDDO
      ENDDO
      !   
      DO I=1,NEV
         IPT=(I-1)*N
         DO J=1,N
            IPT=IPT+1
            Y(J)=VEC(IPT)
         ENDDO
         !
         L=N-2
         MK=(N*(N-1))/2-3
         MK1=3
         !
         DO J=1,L
            T=ZERO
            K=N-J-1
            M=K+1
            DO KK=M,N
               T=T+DD(MK+KK)*Y(KK)
            ENDDO
            DO KK=M,N
               EPR=T*DD(MK+KK)
               Y(KK)=Y(KK)-EPR-EPR
            ENDDO
            MK=MK-MK1
            MK1=MK1+1
         ENDDO
         !
         T=ZERO
         DO J=1,N
            T=T+Y(J)*Y(J)
         ENDDO
         XNORM=SQRT(T)
         XNORM1=ONE/XNORM
         DO J=1,N
            Y(J)=Y(J)*XNORM1
         ENDDO
         !
         IPT=(I-1)*N
         DO J=1,N
            IPT=IPT+1
            VEC(IPT)=Y(J)
         ENDDO
      ENDDO
      !
      DO I=1,N
         EV(I)=EV(I)*ANORM
      ENDDO
      !
      RETURN
    END SUBROUTINE CHARMM_DIAGQ_INT8

end module diag

