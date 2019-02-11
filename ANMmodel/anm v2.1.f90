!a simple version for CA
module data_module
implicit none
  real*8,allocatable :: coor(:),yc(:),zc(:),f(:),k(:)
  integer::n,status
  real*8 ::cutoff
  character (len=6) :: atom
  character (len=4) :: atomtype
  character (len=19):: jieguo
  character (len=19):: input
contains

	subroutine initialize()
	implicit none
	integer	::i=0
	  write(*,*)"pdb file:"
		read(*,"(A79)") input
		print*,"the cutoff is:"
		read *,cutoff

		open(200,file=input,status='old') 
		do while (.true.)
		 read(200,"(A6,6X,A4)",iostat=status) atom,atomtype
		    if (status/=0) exit        
		    if(atom=='ATOM  '.and.atomtype==" CA ")  i=i+1
		end do
		close (200)
		n=i
		print*,"n=",n
  end subroutine

	subroutine read_atom_coordinates()
	implicit none
	integer	::i=1
	real*8	::x,y,z
	  open(20,file=input,status='old') 
		do while(.true.)
		   read(20,"(A6,6X,A4)",iostat=status) atom,atomtype
		   if (status/=0) exit
		   if(atom=="ATOM  ".and.atomtype==" CA ") then
	 !print*,i
		      backspace(20)
		      read(20,"(30X,3F8.3)",iostat=status) x,y,z
		      if (status/=0) exit
		      coor(i)=x  
		      coor(i+n)=y
		      coor(i+2*n)=z
		      i=i+1
		   end if
		end do
		close(20)
  end subroutine
  
	real*8 function dis2(i,j)
		implicit none
		integer ::i,j
		if (i==j) then
		  dis2=1
		else
			dis2=(coor(i)-coor(j))**2+(yc(i)-yc(j))**2+(zc(i)-zc(j))**2
		end if
		return
	end function
	!����ռ��е�i�͵�j��ԭ�ӵľ���.��i��j���ʱ�������Ϊ1����Ϊ�����Ժ�ĸ�Ԫ�ؼ����в����ж�i�Ƿ�Ϊj��
	!��Ӧ��dis2����ʱ���ᷢ���������

	real*8 function H(i,j,xi,xj,yi,yj)
	implicit none
	real*8 :: xi,xj,yi,yj
	integer :: i,j
	H=-(xj-xi)*(yj-yi)/dis2(i,j)
	return
	end function
	!i������j���ǶԽ�H����Ԫ

	subroutine H_dia_dia(i)
	implicit none
	integer	::k1,k2,k,i
	real*8					:: H
		H=0
		do k1=0,2
			do k2=1,n
				k=k2+k1*n
				if(dis2(i,k2)<(cutoff*cutoff)) then
				       H=H+(coor(k)-coor(i))**2/dis2(i,k2)
				 end if
			end do
			f((i*3-3+k1)*(3*n+1)+1)=H
			H=0
		end do
	end subroutine
	!����ԽǾ���Խ�Ԫ��

	subroutine H_dia_notdia1(i)
	integer	::k1,k2,k,i
	real*8					:: H
		H=0
		do k1=0,1
			do k2=1,n
				k=k2+k1*n
				if(dis2(i,k2)<(cutoff*cutoff)) then
				H=H+(coor(k)-coor(i))*(coor(k+n)-coor(i+n))/dis2(i,k2)
				end if
			end do
			f((i*3-3+k1)*(3*n+1)+2)=H
			f((i*3-2+k1)*(3*n+1))=H
			H=0
		end do
	end subroutine

	subroutine H_dia_notdia2(i)
	integer	::k1,k2,k,i
	real*8					:: H
		H=0
		do k2=1,n
			if(dis2(i,k2)<(cutoff*cutoff)) then
			H=H+(coor(k2)-coor(i))*(coor(k2+2*n)-coor(i+2*n))/dis2(i,k2)
			end if
		end do
		f((i*3-3)*(3*n+1)+3)=H
		f((i*3-1)*(3*n+1)-1)=H
	end subroutine

!H�ԽǾ���ĸ�Ԫ�ؼ����ӳ������°���
!  H_dia_dia      H_dia_notdia1  H_dia_notdia2
!  H_dia_notdia1  H_dia_dia      H_dia_notdia1
!  H_dia_notdia2  H_dia_notdia1  H_dia_dia
end module


!��������￪ʼ
program pdb_hessian
  use data_module
  use diag
  
  implicit none
  integer ::distance,CAatoms,i,j,p,q,nmode
  real*8 :: dispf
  real*8,allocatable :: ev(:),vec(:)
  
  call initialize()
!read from PDB for CA number

  allocate (coor(3*n),yc(n),zc(n),f(9*n*n),k(3*n*(3*n+1)/2))
	f=0

  call read_atom_coordinates()
  !��ȡ������Ϣ

  yc=coor(n+1:2*n)
  zc=coor(2*n+1:3*n)
  
!print*,x,y,z
!stop


  do i=1,n
     do j=1,n
        dispf=dis2(i,j)
        if (i/=j)then
           if (dispf<=cutoff*cutoff)then
              f((i*3-3)*3*n+(j-1)*3+1)=H(i,j,coor(i),coor(j),coor(i),coor(j))    
              f((i*3-3)*3*n+(j-1)*3+2)=H(i,j,coor(i),coor(j),yc(i),yc(j))
              f((i*3-3)*3*n+(j-1)*3+3)=H(i,j,coor(i),coor(j),zc(i),zc(j))  
              f((i*3-2)*3*n+(j-1)*3+1)=H(i,j,yc(i),yc(j),coor(i),coor(j))
              f((i*3-2)*3*n+(j-1)*3+2)=H(i,j,yc(i),yc(j),yc(i),yc(j))
              f((i*3-2)*3*n+(j-1)*3+3)=H(i,j,yc(i),yc(j),zc(i),zc(j))
              f((i*3-1)*3*n+(j-1)*3+1)=H(i,j,zc(i),zc(j),coor(i),coor(j))
              f((i*3-1)*3*n+(j-1)*3+2)=H(i,j,zc(i),zc(j),yc(i),yc(j))
              f((i*3-1)*3*n+(j-1)*3+3)=H(i,j,zc(i),zc(j),zc(i),zc(j))
              !ÿ���ǶԽ�HԪ���Ÿ�Ԫ�أ��������Ҵ��ϵ��±�1��9��f��i����iΪ3N*3N�б������
           endif
        else
      		call H_dia_dia(i)
      		call H_dia_notdia1(i)
      		call H_dia_notdia2(i)
        end if     
     end do
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


