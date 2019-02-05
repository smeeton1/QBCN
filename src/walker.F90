module walker

 implicit none
 
 type node
  integer                                :: n,e   ! n number assigned to node in gragh and e number of edges
  logical                                :: o_f   ! weather the node is on or off
  complex*16, dimension(:),allocatable   :: nphi  ! part of the wave function on the node, should be of length e
  complex*16, dimension(2,2)             :: nqphi ! density matrix for the qubit on the node
  logical,dimension(:),allocatable       :: e_of  ! weatrher the edge are on or off, should be of length e
  integer,dimension(:),allocatable       :: c_e   ! nodes each edge conects to, should be of length e
  
!   contains
!     procedure,
!     procedure,
 
 
 end type
 
 contains
 
!functions for walker with 


 subroutine swap(phi,n)
 !swaps the probablities on each edge
 !for each node swaps all edges with m less then then n
 type(node),dimension(:),intent(inout) :: phi
 integer,intent(in)                    :: n
 integer                               :: i,j,k
 complex*16                            :: hold
 
 do i=1,n
  do j=1,phi(i)%e
   if(i.lt.phi(i)%c_e(j))then
    do k=1,phi(phi(i)%c_e(j))%e
      if(phi(phi(i)%c_e(j))%c_e(k).eq.phi(i)%n)then
        hold=phi(i)%nphi(j)
        phi(i)%nphi(j)=phi(phi(i)%c_e(j))%nphi(k)
        phi(phi(i)%c_e(j))%nphi(k)=hold
      endif
    
    enddo
   
   endif
  
  enddo
  
 enddo
 
 
 end subroutine
 
 
 subroutine mix(phi,n,Ct)
 !apply coin to each node
 type(node),dimension(:),intent(inout) :: phi
 integer,intent(in)                    :: n,Ct
 integer                               :: i,j
 complex*16,dimension(:,:),allocatable :: C

 
 do i=1,n
 
  allocate(C(phi(i)%e,phi(i)%e))
 
  if(Ct.eq.1)then
   !call Hcion(C)
  elseif(Ct.eq.2)then
   call Gcoin(C)
  else
   !call Ccoin(C)
  endif
 
  phi(i)%nphi=matmul(C,phi(i)%nphi)
 
  deallocate(C)
 enddo
 
 end subroutine
 
 
!end of walking functions
 
 
!function to gereate coins of size mxm
 subroutine Hcoin(C)
 complex*16,dimension(:,:),intent(inout)  :: C
 integer                                  :: i,n
 n = size(C,1)
 
 end subroutine
 
 subroutine Gcoin(C)
 ! grover coin
 complex*16,dimension(:,:),intent(inout)  :: C
 integer                                  :: i,n
 n = size(C,1)
 
 c(:,:)=cmplx(2.0/real(n))
 do i=1,n
  c(i,i)=c(i,i)-1.0
 enddo
 
 end subroutine
 
 subroutine Ccoin(C)
 complex*16,dimension(:,:),intent(inout)  :: C
 integer                                  :: i,n
 n = size(C,1)
 
 end subroutine
!end of coin functions


 subroutine write_node(ele, un)
 type(node),intent(in)      :: ele
 integer,intent(in)         :: un
 
 write(un,*)ele%n
 write(un,*)ele%o_f
 write(un,*)'phi'
 write(un,*)ele%nphi
 write(un,*)'qubit'
 write(un,*)ele%nqphi
 write(un,*)'edges'
 write(un,*)ele%e_of
 write(un,*)' '
 
 end subroutine
 
 subroutine write_node_full(ele, un)
 type(node),intent(in)      :: ele
 integer,intent(in)         :: un
 
 write(un,*)ele%n
 write(un,*)ele%o_f
 write(un,*)'number of edges'
 write(un,*)ele%e
 write(un,*)'phi'
 write(un,*)ele%nphi
 write(un,*)'qubit'
 write(un,*)ele%nqphi
 write(un,*)'edges'
 write(un,*)ele%e_of
 write(un,*)'conections'
 write(un,*)ele%c_e
 write(un,*)' '
 
 end subroutine



end module