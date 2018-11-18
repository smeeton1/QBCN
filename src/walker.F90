module walker

 implicit none
 
 type,public :: node
  integer  :: n,e
  logical  :: o_f
  complex*16, dimension(e,2) :: nphi
  complex*16, dimension(2)   :: nqphi
  logical,dimension(e)       :: e_of
  
!   contains
!     procedure,
!     procedure,
 
 
 end type node
 
 
!functions for walker with out interaction 
 subroutine swap(phi)
 !swaps the probablities on each edge
 !for each node swaps all edges with m grater then n
 node,dimension(:),intent(inout) :: phi
 integer                         :: n,i,j
 complex*16                      :: hold
 
 n = size(phi)
 
 do i=1,n
  do j=1,phi(i).e
   if(i.lt.phi(i).nphi(1,j))then
   
   
   endif
  
  enddo
  
 end do
 
 
 end subroutine
 
 
 subroutine mix(phi)
 !apply coin to each node
 
 end subroutine
!end of walking functions
 
 
!function to gereate coins of size mxm
 subroutine Hcoin(C)
 
 end subroutine
 
 subroutine Gcoin(C)
 
 end subroutine
 
 subroutine Ccoin(C)
 
 end subroutine
!end of coin functions




end module