program walk_test
use walker
use measure
use rules
implicit none
type(node),dimension(:),allocatable  :: walk
real,dimension(:,:),allocatable      :: syn0,y,x
real,dimension(:),allocatable        :: in,out,out_d
integer                              :: n,i,j,k,l,wend,st,n_o,n_i,m
complex                              :: nm
real                                 :: dt,s,g0

st=10000
n_o=2
n_i=3
wend=10
m=2
dt=0.2
s=1.0
g0=1.0

allocate(walk(6),syn0(2,3),in(3),out(2),out_d(2),y(2,3),x(3,3))
x=reshape((/ 1, 0, 0 ,0,1,1,1,0,1/), shape(x))
y=reshape((/ 1,0,0,1,1,0/), shape(y))

do i=1,n_o
  do j=1,n_i
    syn0(j,i)=2*rand()-1
  enddo
enddo


do i=1,3
 allocate(walk(i)%nphi(1),walk(i)%c_e(1),walk(i)%e_of(1))
 walk(i)%n=i
 walk(i)%e=1
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=4
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
 walk(i)%nqphi(:,:)=cmplx(0.0,0.0)
 walk(i)%nqphi(2,2)=cmplx(1.0,0.0)
 walk(i)%e_of(:)=.false.
enddo
allocate(walk(4)%nphi(5),walk(4)%c_e(5),walk(4)%e_of(5))
walk(i)%n=4
walk(i)%e=5
walk(i)%o_f=.false.
do i=1,3
  walk(4)%c_e(i)=i
enddo
walk(4)%c_e(4)=5
walk(4)%c_e(5)=6

do i=5,6
 allocate(walk(i)%nphi(1),walk(i)%c_e(1),walk(i)%e_of(1))
 walk(i)%n=i
 walk(i)%e=1
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=4
enddo


do k=1,st
  do l=1,3
    nm=sqrt(x(1,l)+x(2,l)+x(3,l))
    do i=1,3
      walk(i)%nphi(1)=cmplx(x(i,l)/nm)
      walk(i)%nqphi(:,:)=cmplx(0.0,0.0)
      walk(i)%nqphi(2,2)=cmplx(1.0,0.0)
      walk(i)%e_of(:)=.false.
    enddo
    do i=5,6
      walk(i)%nphi(1)=cmplx(0.0,0.0)
      walk(i)%nqphi(:,:)=cmplx(0.0,0.0)
      walk(i)%nqphi(2,2)=cmplx(1.0,0.0)
      walk(i)%e_of(:)=.false.
    enddo
    do i=1,5
      walk(4)%nphi(i)=cmplx(0.0,0.0)
    enddo
    walk(4)%nqphi(:,:)=cmplx(0.0,0.0)
    walk(4)%nqphi(2,2)=cmplx(1.0,0.0)
    walk(4)%e_of(:)=.false.
    
  
  
    do i=1,wend
      call step(walk,m,n,dt,s,g0)
    enddo
    do i=1,3
      if(walk(i)%o_f.eqv..True.)then
	in(i)=1
      else
	in(i)=0
      endif
      walk(i)%e_of(1)=.true.
      walk(4)%e_of(i)=.true.
    enddo

    out=matmul(syn0,in)

    do i=1,2
      if(out(i).gt.0.5)then
	walk(4)%e_of(3+i)=.true.
	walk(4+i)%e_of(1)=.true.
      endif
    enddo
    do i=1,wend
      call step(walk,m,n,dt,s,g0)
    enddo
    do i=1,2
      if(walk(4+i)%o_f.eqv..True.)then
	out(i)=1
      else
	out(i)=0
      endif
    enddo
    do i=1,n_o
      out_d(i)=(y(i,l)-out(i))*(out(i)*(1-out(i)))
    enddo
  
    do i=1,n_o
      do j=1,n_i
	syn0(i,j)=syn0(i,j)+x(j,l)*out_d(i)
      enddo
    enddo
  enddo
enddo

end program