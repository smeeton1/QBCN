module rules
 use walker
 implicit none
 contains 
 
 subroutine cycle_graph(phi)
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
 
 
 
 
 
end module