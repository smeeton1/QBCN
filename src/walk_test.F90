program walk_test
use walker
use measure
implicit none

type(node),dimension(:),allocatable  :: walk
integer                        :: n,i,j,wend

wend=4
n=10
allocate(walk(n))

do i=1,n
 allocate(walk(i)%nphi(2),walk(i)%c_e(2),walk(i)%e_of(2))
 walk(i)%n=i
 walk(i)%e=2
 walk(i)%o_f=.false.
 if(i.ne.1)then
 walk(i)%c_e(1)=i-1
 else 
  walk(i)%c_e(1)=n
 endif
 if(i.ne.n)then
  walk(i)%c_e(2)=i+1
 else
  walk(i)%c_e(2)=1
 endif
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
 walk(i)%nqphi(:,:)=cmplx(0.0,0.0)
 walk(i)%nqphi(2,2)=cmplx(1.0,0.0)
 walk(i)%e_of(:)=.false.
enddo

do i=1,wend
  call mix(walk,n,2)
  call swap(walk,n)
  do j=1,n
    call qbit_rho_inter(walk(j)%nqphi,walk(j)%nphi,1.0,0.2,1.0)
    call jmes_den(walk(j)%nqphi,walk(i)%o_f,0.2)
  enddo

enddo

end program