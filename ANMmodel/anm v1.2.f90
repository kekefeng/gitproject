!anm for second,nearby CA is 40 



module data_module
  real*8,allocatable :: xc(:),yc(:),zc(:),f(:),k(:)
  integer::n
  real*8 ::cutoff
end module data_module
!��װ���飬δ�����С

subroutine configure_arrays
  use data_module
  write(*,*)"input CAatom number:"
  read(*,*) n
end subroutine configure_arrays
!���������С

real*8 function dis2(i,j)
  use data_module
  implicit none
  integer ::i,j
  dis2=(xc(i)-xc(j))**2+(yc(i)-yc(j))**2+(zc(i)-zc(j))**2
  return
end function dis2
!����ռ��е�i�͵�j��ԭ�ӵľ���

real*8 function H(i,j,xi,xj,yi,yj)
implicit none
real*8,external:: dis2
real*8 :: xi,xj,yi,yj
integer :: i,j
if (abs(i-j)==1)  then
   H=-40*(xj-xi)*(yj-yi)/dis2(i,j)
else
   H=-(xj-xi)*(yj-yi)/dis2(i,j)
end if
return
end
!i������j���ǶԽ�H����Ԫ

real*8 function H_dia_1(i,xi)
  use data_module
  implicit none
  real*8,external:: dis2
  real*8 :: xi
  integer :: i,j
  j=1
  H_dia_1=0
  do while (j<=n)
     if(j==i) then
        j=j+1
     else
        if(dis2(i,j)<(cutoff*cutoff)) then
            if (abs(i-j)==1)  then
                H_dia_1=H_dia_1+40*(xc(j)-xi)**2/dis2(i,j)
            	j=j+1
            else
            	H_dia_1=H_dia_1+(xc(j)-xi)**2/dis2(i,j)
            	j=j+1
            end if 
        else 
           j=j+1
        end if
     end if
end do
return
end function H_dia_1
!H����Խ�3*3СԪ �� Ԫ��1 ���㣬i����j

real*8 function H_dia_5(i,yi)
  use data_module
  implicit none
  real*8,external:: dis2
  real*8 :: yi,e
  integer :: i,j
  j=1
  H_dia_5=0
  do while (j<=n)
     if(j==i) then
        j=j+1
     else
        if(dis2(i,j)<(cutoff*cutoff)) then
        	if (abs(i-j)==1)  then
				H_dia_5=H_dia_5+40*(yc(j)-yi)**2/dis2(i,j)
				j=j+1
			else
        		H_dia_5=H_dia_5+(yc(j)-yi)**2/dis2(i,j)
           		j=j+1  
           	end if 
        else 
           j=j+1
        end if
     end if
  end do
  return
end function H_dia_5
!H����Խ�3*3СԪ �� Ԫ��5 ���㣬i����j

real*8 function H_dia_9(i,zi)
  use data_module
  implicit none
  real*8,external:: dis2
  real*8 :: zi
  integer :: i,j
  j=1
  H_dia_9=0
  do while (j<=n)
     if(j==i) then
        j=j+1
     else
        if(dis2(i,j)<(cutoff*cutoff)) then
        	if (abs(i-j)==1)  then
        		H_dia_9=H_dia_9+40*(zc(j)-zi)**2/dis2(i,j)
        		j=j+1
        	else
           		H_dia_9=H_dia_9+(zc(j)-zi)**2/dis2(i,j)
           		j=j+1
           	end if 
        else 
           j=j+1
        end if
     end if
  end do
  return
end function H_dia_9
!H����Խ�3*3Ԫ �� Ԫ��9 ���㣬i����j


real*8 function H_dia_24(i,xi,yi)
  use data_module
  implicit none
  real*8,external:: dis2
  real*8 ::xi,yi
  integer ::i,j
  j=1
  H_dia_24=0
  do while (j<=n)
     if(j==i) then
        j=j+1
     else
        if(dis2(i,j)<(cutoff*cutoff)) then
        	if (abs(i-j)==1)  then
        		H_dia_24=H_dia_24+40*(xc(j)-xi)*(yc(j)-yi)/dis2(i,j)
           		j=j+1
           	else
           		H_dia_24=H_dia_24+(xc(j)-xi)*(yc(j)-yi)/dis2(i,j)
           		j=j+1
           	end if
        else 
           j=j+1
        end if
     end if
  end do
  return
end function H_dia_24
!H����Խ�3*3Ԫ �� �ǶԽ�Ԫ��2/4 ���㣬i����j

real*8 function H_dia_37(i,xi,zi)
use data_module
implicit none
real*8,external:: dis2
real*8 ::xi,zi
integer ::i,j
j=1
H_dia_37=0
do while (j<=n)
   if(j==i) then
      j=j+1
   else
      if(dis2(i,j)<(cutoff*cutoff)) then
      	if (abs(i-j)==1)  then
      		H_dia_37=H_dia_37+40*(xc(j)-xi)*(zc(j)-zi)/dis2(i,j)
         	j=j+1
        else
         	H_dia_37=H_dia_37+(xc(j)-xi)*(zc(j)-zi)/dis2(i,j)
         	j=j+1
        end if 
      else  
         j=j+1
      end if
   end if
end do
return
end function H_dia_37
!H����Խ�3*3Ԫ �� �ǶԽ�Ԫ��3/7 ���㣬i����j

real*8 function H_dia_68(i,yi,zi)
  use data_module
  implicit none
  real*8,external:: dis2
  real*8 ::yi,zi
integer ::i,j
j=1
H_dia_68=0
do while (j<=n)
   if(j==i) then
      j=j+1
   else
      if(dis2(i,j)<(cutoff*cutoff)) then
      	if (abs(i-j)==1)  then
      		H_dia_68=H_dia_68+40*(yc(j)-yi)*(zc(j)-zi)/dis2(i,j)
         	j=j+1
        else
         	H_dia_68=H_dia_68+(yc(j)-yi)*(zc(j)-zi)/dis2(i,j)
         	j=j+1
        end if
      else 
         j=j+1
      end if
   end if
end do
return
end function H_dia_68
!H����Խ�3*3Ԫ �� �ǶԽ�Ԫ��6/8 ���㣬i����j


!��������￪ʼ
program pdb_hessian
  use data_module
  use diag
  
  implicit none
  integer ::distance,CAatoms,i,j,status,p,q,nmode
  real*8,external:: dis2,H,H_dia_1,H_dia_5,H_dia_9,H_dia_24,H_dia_37,H_dia_68
  real*8 :: x,y,z,dispf
  character (len=6) :: atom
  character (len=4) :: atomtype
  character (len=19):: jieguo
  character (len=19):: input
  real*8,allocatable :: ev(:),vec(:)
  
  
  write(*,*)"pdb file:"
  read(*,"(A79)") input
  print*,"the cutoff is:"
  read *,cutoff
  
i=0
open(200,file=input,status='old') 
  do while (.true.)
   read(200,"(A6,6X,A4)",iostat=status) atom,atomtype
      if (status/=0) exit        
      if(atom=='ATOM  '.and.atomtype==" CA ")  i=i+1
  end do
 close (200)
n=i
print*,"n=",n
!read from PDB for CA number

  allocate (xc(n),yc(n),zc(n),f(9*n*n),k(3*n*(3*n+1)/2))


!print*,n
!stop

  open(20,file=input,status='old') 
  
!print*,n
!stop
i=1
  do while(.true.)
     read(20,"(A6,6X,A4)",iostat=status) atom,atomtype
     if (status/=0) exit
     if(atom=="ATOM  ".and.atomtype==" CA ") then
 !print*,i
        backspace(20)
        read(20,"(30X,3F8.3)",iostat=status) x,y,z
        if (status/=0) exit
        xc(i)=x  
        yc(i)=y
        zc(i)=z
        
        i=i+1
     end if
  end do
  close(20)
  !��ȡ������Ϣ
  
  
!print*,x,y,z
!stop

  i=1 
  j=1
  
  do while (i<=n)
     do while (j<=n)
        dispf=dis2(i,j)
        if (i/=j)then
           if (dispf<=cutoff*cutoff)then
              f((i*3-3)*3*n+(j-1)*3+1)=H(i,j,xc(i),xc(j),xc(i),xc(j))    
              f((i*3-3)*3*n+(j-1)*3+2)=H(i,j,xc(i),xc(j),yc(i),yc(j))
              f((i*3-3)*3*n+(j-1)*3+3)=H(i,j,xc(i),xc(j),zc(i),zc(j))  
              f((i*3-2)*3*n+(j-1)*3+1)=H(i,j,yc(i),yc(j),xc(i),xc(j))
              f((i*3-2)*3*n+(j-1)*3+2)=H(i,j,yc(i),yc(j),yc(i),yc(j))
              f((i*3-2)*3*n+(j-1)*3+3)=H(i,j,yc(i),yc(j),zc(i),zc(j))
              f((i*3-1)*3*n+(j-1)*3+1)=H(i,j,zc(i),zc(j),xc(i),xc(j))
              f((i*3-1)*3*n+(j-1)*3+2)=H(i,j,zc(i),zc(j),yc(i),yc(j))
              f((i*3-1)*3*n+(j-1)*3+3)=H(i,j,zc(i),zc(j),zc(i),zc(j))
              !ÿ���ǶԽ�HԪ���Ÿ�Ԫ�أ��������Ҵ��ϵ��±�1��9��f��i����iΪ3N*3N�б������
           else
              f((i*3-3)*3*n+(j-1)*3+1)=0
              f((i*3-3)*3*n+(j-1)*3+2)=0
              f((i*3-3)*3*n+(j-1)*3+3)=0
              f((i*3-2)*3*n+(j-1)*3+1)=0
              f((i*3-2)*3*n+(j-1)*3+2)=0
              f((i*3-2)*3*n+(j-1)*3+3)=0
              f((i*3-1)*3*n+(j-1)*3+1)=0
              f((i*3-1)*3*n+(j-1)*3+2)=0
              f((i*3-1)*3*n+(j-1)*3+3)=0
           endif
        else
           f((i*3-3)*(3*n+1)+1)=H_dia_1(i,xc(i))
           f((i*3-3)*(3*n+1)+2)=H_dia_24(i,xc(i),yc(i))
           f((i*3-3)*(3*n+1)+3)=H_dia_37(i,xc(i),zc(i))
           f((i*3-2)*(3*n+1))=H_dia_24(i,xc(i),yc(i))
           f((i*3-2)*(3*n+1)+1)=H_dia_5(i,yc(i))
           f((i*3-2)*(3*n+1)+2)=H_dia_68(i,yc(i),zc(i))
           f((i*3-1)*(3*n+1)-1)=H_dia_37(i,xc(i),zc(i))
           f((i*3-1)*(3*n+1))=H_dia_68(i,yc(i),zc(i))
           f((i*3-1)*(3*n+1)+1)=H_dia_9(i,zc(i))
        end if
        j=j+1
     end do
     i=i+1
     j=1
  end do
  
  !����ǰ�� 3n*3n hessian�����ʾ��i����˳�����ң��ϵ��µı��
  
  jieguo='jieguo.txt'
  Open(10 , file = jieguo)
  i=1
  p=1
  q=1
  do while (p<=3*n)
     do while (q<=3*n)
        k(i)=f(3*n*p+q-3*n)
        write( 10 , * ) k(i)
        i=i+1
        q=q+1
     end do
     p=p+1
     q=p
  end do
  Close( 10 )
  
  jieguo='ev.txt'
  nmode=100
  open (11,file= jieguo)
  call cdiag(3*n,nmode,k,ev,vec)
  
  do i=1,nmode
     write (11,*) ev(i)
  end do
  close(11)
  
  jieguo='vec.txt'
  open(12,file= jieguo)
  do j=0,nmode-1
     do i=1,3*n
        write(12,*) vec(j*3*n+i)
     end do
  end do
  close(12)

end program pdb_hessian


