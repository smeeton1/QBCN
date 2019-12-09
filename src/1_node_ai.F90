program walk_test
use walker
use measure
use rules
implicit none
type(node),dimension(:),allocatable  :: walk
real,dimension(:,:),allocatable      :: syn0,y,x
real,dimension(:),allocatable        :: in,out,out_d
integer                              :: n,i,j,k,l,wend,st,n_o,n_i,m,rn
complex                              :: nm
real                                 :: dt,s,g0

st=10
n_o=2
n_i=3
wend=5
m=2
dt=0.5
s=2.0
g0=0.5
rn=2
n=5

allocate(walk(5),syn0(2,3),in(3),out(2),out_d(2),y(2,3),x(3,3))
x=reshape((/ 1, 0, 0 ,0,1,1,1,0,1/), shape(x))
y=reshape((/ 1,0,0,1,1,0/), shape(y))

do i=1,n_o ! setting up random strength matrix
  do j=1,n_i
    syn0(j,i)=2*rand()-1
  enddo
enddo


do i=1,3  !setting up walker
 allocate(walk(i)%nphi(2),walk(i)%c_e(2),walk(i)%e_of(2))
 walk(i)%n=i
 walk(i)%e=2
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=4
 walk(i)%c_e(2)=5
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo
do i=4,5
 allocate(walk(i)%nphi(3),walk(i)%c_e(3),walk(i)%e_of(3))
 walk(i)%n=i
 walk(i)%e=3
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=1
 walk(i)%c_e(2)=2
 walk(i)%c_e(3)=3
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo

open(9, file='ai_1l.dat', status='replace',action='write')
open(5, file='out_prob.dat', status='replace',action='write')

do k=1,st
  do l=1,3
    nm=sqrt(x(1,l)+x(2,l)+x(3,l))
    do i=1,3  !setting up the walker for ai run
      walk(i)%nphi(1)=cmplx(x(i,l)/sqrt(2.0)*nm)
      walk(i)%nphi(2)=cmplx(x(i,l)/sqrt(2.0)*nm)
      walk(i)%nqphi(:,:)=cmplx(0.0,0.0)
      walk(i)%nqphi(2,2)=cmplx(1.0,0.0)
      walk(i)%e_of(:)=.false.
    enddo
    do i=4,5
      walk(i)%nphi(:)=cmplx(0.0,0.0)
      walk(i)%nqphi(:,:)=cmplx(0.0,0.0)
      walk(i)%nqphi(2,2)=cmplx(1.0,0.0)
      walk(i)%e_of(:)=.false.
    enddo

  
  
    do i=1,wend  !taking the steps
      call step(walk,m,n,dt,s,g0,rn)
    enddo
    do i=1,3 !readinjg out results of qw
      write(*,*)walk(i)%o_f
      if(walk(i)%o_f)then
	    in(i)=1
      else
	    in(i)=0
      endif
    enddo
    write(*,*) in
    out=matmul(syn0,in)

    do i=1,2 ! setting up out puts
      if(out(i).gt.0.5)then
	   walk(1)%e_of(i)=.true.
	   walk(2)%e_of(i)=.true.
	   walk(3)%e_of(i)=.true.
	   walk(3+i)%e_of(:)=.true.
      endif
    enddo
    do i=1,wend !final steps for walker to head to out put 
      call step(walk,m,n,dt,s,g0,rn)
    enddo

    do i=1,n_o
      out_d(i)=(y(i,l)-out(i))*(out(i)*(1-out(i)))
    enddo
    do i=1,2 !checking outputs
      if(walk(3+i)%o_f.eqv..true.)then
	    out(i)=1
      else
	    out(i)=0
      endif
    enddo
    do i=1,n_o
      do j=1,n_i
	    syn0(i,j)=syn0(i,j)+x(j,l)*out_d(i)
      enddo
    enddo
    
  enddo
  write(9,*) (out_d(1)+out_d(2))/2
  call write_p_wave(walk,5,3)
enddo

close(9)

end program
