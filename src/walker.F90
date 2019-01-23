module walker

 implicit none
 
 type,public :: node
  integer  :: n,e
  logical  :: o_f
  complex*16, dimension(e,2) :: nphi
  complex*16, dimension(2,2) :: nqphi
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
 integer                         :: n,i,j,k
 complex*16                      :: hold
 
 n = size(phi)
 
 do i=1,n
  do j=1,phi(i).e
   if(i.lt.phi(i).nphi(1,j))then
    do k=1,phi(phi(i).nphi(1,j)).e
      if(phi(phi(i).nphi(1,j)).nphi(1,k).eq.phi(i).n)then
        hold=phi(i).nphi(2,j)
        phi(phi(i).nphi(2,j))=phi(phi(i).nphi(1,j)).nphi(2,k)
        phi(phi(i).nphi(1,j)).nphi(2,k)=hold
      endif
    
    enddo
   
   endif
  
  enddo
  
 end do
 
 
 end subroutine
 
 
 subroutine mix(phi,Ct)
 !apply coin to each node
 node,dimension(:),intent(inout)  :: phi
 integer,intent(in)               :: Ct
 integer                          :: n,i,j
 complex*16,dimension,allocatable :: C
 
 n = size(phi)
 
 do i=1,n
 
  allocate(C(phi(i).e,phi(i).e))
 
  if(Ct.eq.1)then
   call Hcion(C)
  elseif(Ct.eq.2)then
   call Gcoin(C)
  else
   call Ccoin(C)
  endif
 
  matmul(C,phi(i).nphi(2,:))
 
  deallocate(C)
 enddo
 
 end subroutine
!end of walking functions
 
 
!function to gereate coins of size mxm
 subroutine Hcoin(C)
 complex*16,intent(inout)  :: C
 integer                   :: i,n
 n = size(C,1)
 
 end subroutine
 
 subroutine Gcoin(C)
 complex*16,intent(inout)  :: C
 integer                   :: i,n
 n = size(C,1)
 
 c(:,:)=cmplx(2.0/real(n))
 do i=1,n
  c(i,i)=c(i,i)-1.0
 enddo
 
 end subroutine
 
 subroutine Ccoin(C)
 complex*16,intent(inout)  :: C
 integer                   :: i,n
 n = size(C,1)
 
 end subroutine
!end of coin functions




end module