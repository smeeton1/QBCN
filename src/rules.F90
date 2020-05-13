module rules
 use walker
 use measure
 implicit none
 contains 
 
 subroutine step(phi,coin,n,dt,s,g0,rn)
  type(node),dimension(:),intent(inout)  :: phi
  integer,intent(inout)                  :: coin,n,rn
  real,intent(inout)                     :: dt,s,g0
  integer                                :: j
  
  call mix(phi,n,coin)
  call swap(phi,n)
  do j=1,n
    call qbit_rho_inter(phi(j)%nqphi,phi(j)%nphi,s,dt,g0)
    call jmes_den(phi(j)%nqphi,phi(j)%o_f,dt,rn)
  enddo
 
 end subroutine
 
 subroutine cycle_graph(phi)
 ! rules for a cycle where the right conection is openned when the node is measured as on
  type(node),dimension(:),intent(inout)  :: phi
  integer                                :: i,n
  
  n=size(phi)
  
  do i=1,n-1
   if(phi(i)%o_f.eqv..true.)then
    phi(i)%e_of(2)=.true.
    phi(i+1)%e_of(1)=.true.
   endif
   if(phi(i)%o_f.eqv..false.)then
    phi(i)%e_of(2)=.false.
    phi(i+1)%e_of(1)=.false.
   endif
  enddo
  if(phi(n)%o_f.eqv..true.)then
    phi(n)%e_of(2)=.true.
    phi(1)%e_of(1)=.true.
  endif
  if(phi(n)%o_f.eqv..false.)then
    phi(n)%e_of(2)=.false.
    phi(1)%e_of(1)=.false.
  endif
 
 end subroutine
 
 subroutine random_matrix(A)
 ! setting up random strength matrix
 real,dimension(:,:),intent(inout) :: A
 integer                           :: i,j
  do i=1,size(A,2)
   do j=1,size(A,1)
     A(j,i)=2*rand()-1
   enddo
 enddo
 
 end subroutine
 
 
 subroutine ai(phi)
    type(node),dimension(:),intent(inout)  :: phi
    integer                                :: i,n
  
    n=size(phi)
 
 
 
 end subroutine
 
 
 
end module
