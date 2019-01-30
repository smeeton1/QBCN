program walk_test
use walker
use measure
implicit none

node,dimension(:),allocatable  :: walk
integer                        :: n,i,j

n=10
allocate(walk(n))

do i=1,n
 walk.n=i
 walk.e=2
 walk.o_f=.false.
 if(i.neq.1)then
 walk.nphi(1,1)=i-1
 else then
  walk.nphi(1,1)=n
 endif
 if(i.neq.n)then
  walk.nphi(1,1)=i+1
 else then
  walk.nphi(1,1)=1
 endif
 do j=1,walk.e
   walk.nphi(j,2)=cmplx(0.0,0.0)
 enddo
 walk.nqphi(:,:)=cmplx(0.0,0.0)
 walk.nqphi(2,2)=cmplx(1.0,0.0)
 walk.e_of(:)=.false.
enddo

end program