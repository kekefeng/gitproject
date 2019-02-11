!produce dynamic pdb files using vec files produced by anm vx.x

program add_moving
real*8,allocatable             ::vec(:)
character(len=15)              ::input,output,vectxt,ac,ab
integer,allocatable            ::iatom(:),resid(:)
integer                        ::i,j,n
integer                        ::status=0
character(len=6),allocatable   :: atom(:)
character(len=4),allocatable   :: atomtype(:),segid(:)
character(len=6)               :: atom_n
character(len=4)               :: atomtype_n
character(len=3),allocatable   :: resname(:)
character(len=1),allocatable   :: chain(:),icode(:)
character(len=2)               ::num1
real*8,allocatable             :: x(:),y(:),z(:),bfactor(:),one(:)
real*8,allocatable             :: dx(:),dy(:),dz(:),xc(:),yc(:),zc(:)
real,parameter                 :: PI=3.14159
integer            	       :: A,imode

print*,"the ? of the mode:"
read (*,*) imode
print*,"A(fluctuation)="
read (*,*) A
print*,"the pdb file is"
read (*,*) input

i=0
open(200,file=input,status='old') 
  do while (.true.)
   read(200,"(A6,6X,A4)",iostat=status) atom_n,atomtype_n
!print*,i
      if (status/=0) exit        
      if(atom_n=='ATOM  '.and.atomtype_n==" CA ")  i=i+1
  end do
 close (200)
n=i
print*,"n=",n
!read from PDB for CA number

!stop

allocate (vec(3*n),iatom(n),resid(n),resname(n),chain(n),atom(n),atomtype(n),segid(n))
allocate (x(n),y(n),z(n),bfactor(n),one(n),dx(n),dy(n),dz(n),xc(n),yc(n),zc(n),icode(n))


i=0
open(200,file=input,status='old') 
  do while (.true.)
   read(200,"(A6,6X,A4)",iostat=status) atom(i),atomtype(i)
      if (status/=0) exit        
      if(atom(i)=='ATOM  '.and.atomtype(i)==" CA ") then 
	i=i+1
        backspace(200)
        read(200,"(A6,I5,1x,A4,1x,A3,1x,A1,I4,A1,3x,3F8.3,2F6.2,6X,A4)",iostat=status) atom(i),iatom(i),&
            atomtype(i),resname(i),chain(i),resid(i),icode(i),x(i),y(i),z(i),one(i),bfactor(i),segid(i)

	!write(*,"(A6,I5,1x,A4,1x,A3,1x,A1,I4,A1,3x,3F8.3,2F6.2,6X,A4)",iostat=status) atom(i),iatom(i),&
	!atomtype(i),resname(i),chain(i),resid(i),icode(i),x(i),y(i),z(i),one(i),bfactor(i),segid(i)

         if (status/=0) exit
     endif
    if(i==n) exit
  end do
  close(200)

!stop


write(num1,"(I2.2)") 0
output="1jef"//num1//".pdb"
open(20,file=output,status="unknown")
i=1 
 do i=1,n
write(20,"(A6,I5,1x,A4,1x,A3,1x,A1,I4,A1,3x,3F8.3,2F6.2,6X,A4)",iostat=status) atom(i),iatom(i),&
atomtype(i),resname(i),chain(i),resid(i),icode(i),x(i),y(i),z(i),one(i),bfactor(i),segid(i)
end do
  close(20)  

!above is to produce a priminary 1jef.pdb ,only extracted CA atoms
!now we have known the coordinates of x,y,z of CA atoms in array x,y,z


vectxt='vec.txt'
open(100,file=vectxt,status='old')
i=1
do  j=1,imode-1
   do i=1,3*n
      read(100,*,iostat=status)
   end do 
end do

do i=1,3*n
   read(100,*,iostat=status) vec(i)
   if (status/=0) exit
end do
close(100)

!stop

!now let's implicit dx dy dz array and add them to x,y,z
forall(i=1:n)
   dx(i)=vec(3*i-2)
   dy(i)=vec(3*i-1)
   dz(i)=vec(3*i)
end forall
!dx=0; dy=0; dz=1/sqrt(float(n));

do i=1,n
   write(101,'(3F12.5)') dx(i),dy(i),dz(i)
end do

do j=1,23
   xc=x+A*cos(j*PI/12)*dx
   yc=y+A*cos(j*PI/12)*dy
   zc=z+A*cos(j*PI/12)*dz
  
   write(num1,"(I2.2)") j
   output="1jef"//num1//".pdb"
   open(20,file=output,status="unknown")
   
   do i=1,n
      write(20,"(A6,I5,1x,A4,1x,A3,1x,A1,I4,A1,3x,3F8.3,2F6.2,6X,A4)",iostat=status) atom(i),iatom(i),&
           atomtype(i),resname(i),chain(i),resid(i),icode(i),xc(i),yc(i),zc(i),one(i),bfactor(i),segid(i)

   end do
   close(20)
end do


stop
end program add_moving
