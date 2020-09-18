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
 
 subroutine random_matrix(A,seed)
 ! setting up random strength matrix
 real,dimension(:,:),intent(inout) :: A
 integer                           :: i,j
 integer,intent(in)                :: seed
  do i=1,size(A,2)
   do j=1,size(A,1)
     A(j,i)=4*rand(seed*i*j)-2
   enddo
 enddo
 
 end subroutine
 
 
 subroutine ai_forward(phi,N_S,N_E,out,syn,openlevel,in_t)
    type(node),dimension(:),intent(inout)  :: phi
    real,dimension(:,:),intent(in)         :: syn
    real,dimension(:),intent(inout)        :: out,in_t
    real,intent(in)                        :: openlevel
    real,dimension(:),allocatable          :: in
    integer,intent(in)                     :: N_S,N_E
    integer                                :: i,j
  
    allocate(in(N_E-N_S+1))
    
    !write(*,*) N_E-N_S+1
    do i=1,N_E-N_S+1 !readinjg out results of qw
     ! write(*,*)walk(i)%o_f
      if(phi(i)%o_f)then
	    in(i)=1
      else
	    in(i)=0
      endif
    enddo
    !write(*,*) in
    out=matmul(syn,in_t)
    !write(*,*) N_E-N_S
    do i=1,N_E-N_S ! setting up out puts
      if(out(i).gt.openlevel)then
       do j=N_S,N_E
	     phi(j)%e_of(i)=.true.
	   enddo
	   phi(N_S+i)%e_of(1:N_E-N_S+1)=.true.
      endif
    enddo
    
    deallocate(in)
 
 end subroutine
 
 
 subroutine ai_backward(out,out2,out_d,syn1,syn2)
    real,dimension(:,:),intent(inout)      :: syn1
    real,dimension(:),intent(inout)        :: out
    real,dimension(:,:),intent(in)         :: syn2
    real,dimension(:),intent(in)           :: out2,out_d
    real,dimension(:),allocatable          :: syn_d
    integer                                :: i,j,n_1,n_2
  
    n_1=size(syn1,1)
    n_2=size(syn1,2)
    allocate(syn_d(n_1))
    
    !write(*,*) "10"
    syn_d = matmul(out_d,syn2)
    !write(*,*) "10.1"
    do i=1,n_1
     out(i) = 1/(1+exp(-out(i)))
     out(i)= syn_d(i)*out(i)*(1-out(i))
    enddo
    !write(*,*) "10.2"
    do i=1,n_1
      do j=1,n_2
	    syn1(i,j)=syn1(i,j)+out(i)*out2(j)
      enddo
    enddo  
    
    deallocate(syn_d)
 
 end subroutine
 
 
 
end module
