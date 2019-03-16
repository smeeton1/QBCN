program walk_test
use walker
use measure
use rules
implicit none

type(node),dimension(:),allocatable  :: walk
integer                        :: n,i,j,wend

wend=400
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

walk(4)%nphi(1)=1/sqrt(2.0);walk(4)%nphi(2)=cmplx(0.0,1.0)/sqrt(2.0)

walk(4)%e_of(2)=.true.;walk(5)%e_of(1)=.true.
walk(3)%e_of(2)=.true.;walk(4)%e_of(1)=.true.
walk(3)%o_f=.true.;walk(4)%o_f=.true.
walk(3)%nqphi(1,1)=cmplx(1.0,0.0)
walk(3)%nqphi(2,2)=cmplx(0.0,0.0)
walk(4)%nqphi(1,1)=cmplx(1.0,0.0)
walk(4)%nqphi(2,2)=cmplx(0.0,0.0)


open(9, file='cycle_1', status='replace',action='write')
! do i=1,n
!  call write_node_full(walk(i), 9)
! enddo
  write(9,*) norm(walk,n)
  call write_p_wave(walk,n,9)
  do j=1,n
    write(9,'(L2)',advance='no') walk(j)%o_f
  enddo
  write(9,*)' '


do i=1,wend
  call mix(walk,n,2)
  call swap(walk,n)
  do j=1,n
    call qbit_rho_inter(walk(j)%nqphi,walk(j)%nphi,1.0,0.2,1.0)
    call jmes_den(walk(j)%nqphi,walk(j)%o_f,0.2)
  enddo

  call cycle_graph(walk)
    write(9,*) 'step',i

  write(9,*) norm(walk,n)
  call write_p_wave(walk,n,9)
  do j=1,n
    write(9,'(L2)',advance='no') walk(j)%o_f
  enddo
  write(9,*)' '

enddo
close(9)


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

walk(4)%nphi(1)=1/sqrt(2.0);walk(4)%nphi(2)=cmplx(0.0,1.0)/sqrt(2.0)

walk(4)%e_of(2)=.true.;walk(5)%e_of(1)=.true.
walk(3)%e_of(2)=.true.;walk(4)%e_of(1)=.true.
walk(3)%o_f=.true.;walk(4)%o_f=.true.
walk(3)%nqphi(1,1)=cmplx(1.0,0.0)
walk(3)%nqphi(2,2)=cmplx(0.0,0.0)
walk(4)%nqphi(1,1)=cmplx(1.0,0.0)
walk(4)%nqphi(2,2)=cmplx(0.0,0.0)


open(9, file='cycle_2', status='replace',action='write')
! do i=1,n
!  call write_node_full(walk(i), 9)
! enddo
  write(9,*) norm(walk,n)
  call write_p_wave(walk,n,9)
  do j=1,n
    write(9,'(L2)',advance='no') walk(j)%o_f
  enddo
  write(9,*)' '


do i=1,wend
  call mix(walk,n,2)
  call swap(walk,n)
  do j=1,n
    call qbit_rho_inter(walk(j)%nqphi,walk(j)%nphi,1.0,0.2,1.0)
    call jmes_den(walk(j)%nqphi,walk(j)%o_f,0.2)
  enddo

  call cycle_graph(walk)
    write(9,*) 'step',i

  write(9,*) norm(walk,n)
  call write_p_wave(walk,n,9)
  do j=1,n
    write(9,'(L2)',advance='no') walk(j)%o_f
  enddo
  write(9,*)' '

enddo
close(9)

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

walk(4)%nphi(1)=1/sqrt(2.0);walk(4)%nphi(2)=cmplx(0.0,1.0)/sqrt(2.0)

walk(4)%e_of(2)=.true.;walk(5)%e_of(1)=.true.
walk(3)%e_of(2)=.true.;walk(4)%e_of(1)=.true.
walk(3)%o_f=.true.;walk(4)%o_f=.true.
walk(3)%nqphi(1,1)=cmplx(1.0,0.0)
walk(3)%nqphi(2,2)=cmplx(0.0,0.0)
walk(4)%nqphi(1,1)=cmplx(1.0,0.0)
walk(4)%nqphi(2,2)=cmplx(0.0,0.0)


open(9, file='cycle_3', status='replace',action='write')
! do i=1,n
!  call write_node_full(walk(i), 9)
! enddo
  write(9,*) norm(walk,n)
  call write_p_wave(walk,n,9)
  do j=1,n
    write(9,'(L2)',advance='no') walk(j)%o_f
  enddo
  write(9,*)' '


do i=1,wend
  call mix(walk,n,2)
  call swap(walk,n)
  do j=1,n
    call qbit_rho_inter(walk(j)%nqphi,walk(j)%nphi,1.0,0.2,1.0)
    call jmes_den(walk(j)%nqphi,walk(j)%o_f,0.2)
  enddo

  call cycle_graph(walk)
    write(9,*) 'step',i

  write(9,*) norm(walk,n)
  call write_p_wave(walk,n,9)
  do j=1,n
    write(9,'(L2)',advance='no') walk(j)%o_f
  enddo
  write(9,*)' '

enddo
close(9)

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

walk(4)%nphi(1)=1/sqrt(2.0);walk(4)%nphi(2)=cmplx(0.0,1.0)/sqrt(2.0)

walk(4)%e_of(2)=.true.;walk(5)%e_of(1)=.true.
walk(3)%e_of(2)=.true.;walk(4)%e_of(1)=.true.
walk(3)%o_f=.true.;walk(4)%o_f=.true.
walk(3)%nqphi(1,1)=cmplx(1.0,0.0)
walk(3)%nqphi(2,2)=cmplx(0.0,0.0)
walk(4)%nqphi(1,1)=cmplx(1.0,0.0)
walk(4)%nqphi(2,2)=cmplx(0.0,0.0)


open(9, file='cycle_4', status='replace',action='write')
! do i=1,n
!  call write_node_full(walk(i), 9)
! enddo
  write(9,*) norm(walk,n)
  call write_p_wave(walk,n,9)
  do j=1,n
    write(9,'(L2)',advance='no') walk(j)%o_f
  enddo
  write(9,*)' '


do i=1,wend
  call mix(walk,n,2)
  call swap(walk,n)
  do j=1,n
    call qbit_rho_inter(walk(j)%nqphi,walk(j)%nphi,1.0,0.2,1.0)
    call jmes_den(walk(j)%nqphi,walk(j)%o_f,0.2)
  enddo

  call cycle_graph(walk)
    write(9,*) 'step',i

  write(9,*) norm(walk,n)
  call write_p_wave(walk,n,9)
  do j=1,n
    write(9,'(L2)',advance='no') walk(j)%o_f
  enddo
  write(9,*)' '

enddo
close(9)






end program