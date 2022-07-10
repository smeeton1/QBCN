program walk_test
use walker
use measure
use rules
implicit none
type(node),dimension(:),allocatable  :: walk
real,dimension(:,:),allocatable      :: syn0,syn1,y,x
real,dimension(:),allocatable        :: in,out,out_d,out_d1,out_p,syn0_d,syn1_d,syn2_d,syn3_d,syn4_d,out_d2,out_d3,out_d4,out_d5
real,dimension(:),allocatable        :: out_d6, out2,out3,out4,out5,out6,in2,in3,in4,in5,in6
integer                              :: n,i,j,k,l,wend,st,n_e,n_s,m,rn,runs,w,n_1
complex                              :: nm
real                                 :: dt,s,g0,openlevel,Err,sum1

openlevel =0.5
runs=10000
st=10
n_s=4
n_1=7
n_e=9
wend=30
m=2
dt=0.5
s=2.0
g0=0.5
rn=2
n=9

allocate(walk(n),y(2,10),x(n_s,10),out_p(st))
allocate(out(3),out2(2),out3(2))
allocate(out_d(2),out_d1(2),out_d2(3))
allocate(in(4),in2(3),in3(2))
allocate(syn0(3,4),syn1(2,3))
allocate(syn0_d(4),syn1_d(3))
x=reshape((/ 1,0,0,0, 0,1,1,1, 1,0,0,0, 1,1,1,1, 0,1,0,1, &
1,0,1,0, 0,0,0,0, 1,1,0,0, 0,0,0,0, 0,1,1,0/), shape(x))
y=reshape((/ 1,0, 0,1, 1,0, 1,0, 0,1, 1,0, 0,1, 1,0, 0,1, 0,1/), shape(y))
out_p(:)=0.0
! do i=1,n_1 ! setting up random strength matrix layer 1
!   do j=1,n_s
!     syn0(j,i)=abs(2*rand()-1)
!   enddo
! enddo
! do i=1,n_2 ! setting up random strength matrix layer 2
!   do j=1,n_1
!     syn1(j,i)=abs(2*rand()-1)
!   enddo
! enddo
! do i=1,n_3 ! setting up random strength matrix layer 3
!   do j=1,n_2
!     syn2(j,i)=abs(2*rand()-1)
!   enddo
! enddo
! do i=1,n_4 ! setting up random strength matrix layer 4
!   do j=1,n_3
!     syn3(j,i)=abs(2*rand()-1)
!   enddo
! enddo
! do i=1,n_5 ! setting up random strength matrix layer 5
!   do j=1,n_4
!     syn4(j,i)=abs(2*rand()-1)
!   enddo
! enddo
! do i=1,n_e ! setting up random strength matrix layer 6
!   do j=1,n_5
!     syn5(j,i)=abs(2*rand()-1)
!   enddo
! enddo
!write(*,*) syn0
!write(*,*) "1"

do i=1,n_s  !setting up walker
 allocate(walk(i)%nphi(n_1-n_s),walk(i)%c_e(n_1-n_s),walk(i)%e_of(n_1-n_s))
 walk(i)%n=i
 walk(i)%e=n_1-n_s
 walk(i)%o_f=.false.
 do j=i,n_1-n_s
    walk(i)%c_e(j)=j+n_s
 enddo
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo
do i=n_s+1,n_1
 allocate(walk(i)%nphi(n_s+n_e-n_1),walk(i)%c_e(n_s+n_e-n_1),walk(i)%e_of(n_s+n_e-n_1))
 walk(i)%n=i
 walk(i)%e=n_s+n_e-n_1
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=1
 walk(i)%c_e(2)=2
 walk(i)%c_e(3)=3
 walk(i)%c_e(4)=4
 walk(i)%c_e(5)=8
 walk(i)%c_e(6)=9
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo
do i=n_1+1,n_e
 allocate(walk(i)%nphi(10),walk(i)%c_e(10),walk(i)%e_of(10))
 walk(i)%n=i
 walk(i)%e=10
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=5
 walk(i)%c_e(2)=6
 walk(i)%c_e(3)=7
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo



!write(*,*) "2"
open(9, file='ai.dat', status='replace',action='write')
open(5, file='out_prob.dat', status='replace',action='write')
do w=1,runs
call random_matrix(syn0,w*TIME())
call random_matrix(syn1,w*TIME())
! call random_matrix(syn5,w*TIME())
!write(*,*) "3"
do k=1,st
  do l=1,10
    do i=1,n
      walk(i)%nphi(:)=cmplx(0.0,0.0)
    enddo
    nm=sqrt(sum(x(:,l)))
    do i=1,n_s  !setting up the walker for ai run
      do j=1,n_1-n_s
      walk(i)%nphi(j)=cmplx(x(i,l)/(sqrt(float(n_1-n_s))*nm))
      enddo
      walk(i)%nqphi(:,:)=cmplx(0.0,0.0)
      walk(i)%nqphi(2,2)=cmplx(1.0,0.0)
      walk(i)%e_of(:)=.false.
    enddo
    do i=n_s+1,n
      walk(i)%nphi(:)=cmplx(0.0,0.0)
      walk(i)%nqphi(:,:)=cmplx(0.0,0.0)
      walk(i)%nqphi(2,2)=cmplx(1.0,0.0)
      walk(i)%e_of(:)=.false.
    enddo
    !write(*,*) norm(walk,n),float(n_1-n_s)
    !write(*,*) "4"
  
    do i=1,wend  !taking the steps
      call step(walk,m,n,dt,s,g0,rn)
    enddo
    !write(*,*) norm(walk,n)
    !write(*,*) n_s+1,n_1, n_1-n_s+1, size(out2),size(syn1,2)
    call ai_forward(walk,1,n_s,out,syn0,openlevel,x(:,l))
    
    !write(*,*) "5"
    do i=1,wend !final steps for walker to head to out put 
      call step(walk,m,n,dt,s,g0,rn)
    enddo

    !write(*,*) "5.1"
    call ai_forward(walk,n_s+1,n_1,out2,syn1,openlevel,out)

    !write(*,*) "6"
    do i=1,wend !final steps for walker to head to out put 
      call step(walk,m,n,dt,s,g0,rn)
    enddo

    do i=1,n_e
      out2(i)=1/(1+exp(-out2(i)))
      out_d(i)=(y(i,l)-out2(i))*(out2(i)*(1-out2(i)))
    enddo

    !write(*,*) "9"
    
    Err=abs((y(1,l)-out2(1))+(y(2,l)-out2(2)))
    !write(*,*) "9.1"
    do i=1,2 !checking outputs
      if(walk(12+i)%o_f.eqv..true.)then
	    out3(i)=1
      else
	    out3(i)=0
      endif
    enddo
    !write(*,*) "9.2"
    if((out3(1).eq.y(1,l)).or.(out3(2).eq.y(2,l)).and.((walk(8)%o_f).or.(walk(9)%o_f)))then
     out_p(k)=out_p(k)+1
    endif
    
    !write(*,*) "9.3"
    do i=1,2
      do j=1,3
	    syn1(i,j)=syn1(i,j)+out_d(i)*out2(j)
      enddo
    enddo
    !write(*,*) "10"
    out_d2(:)=0.0
    do i=1,2
      do j=1,3
	    out_d2(j)=out_d2(j)+out_d(i)*syn1(i,j)+out2(j)*out2(j)
      enddo
    enddo
    !write(*,*) "11"
    do i=1,3
      do j=1,4
	    syn0(i,j)=syn0(i,j)+out_d2(i)*X(j,k)
      enddo
    enddo
    
    !write(*,*) "12"
   
    

 

    
  enddo
  write(9,*) Err!(out_d(1)+out_d(2))/2
  write(5,*) node_prob(walk(n))!+node_prob(walk(5))
  !write(*,*) norm(walk,n)
enddo
  
enddo

open(6,file='ai_endwave.dat', status='replace',action='write')
call write_p_wave(walk,n,6)
write(6,*) norm(walk,n)
do i=1,n
 write(6,*) walk(i)%o_f
enddo

close(6)


open(2, file='ai_1o_P10000.dat', status='replace',action='write')

do i=1,st
  write(2,*) out_p(i)/(st*runs)
enddo

close(2)
close(9)
close(5)

open(2, file='ai_po_10000.dat', status='replace',action='write')
sum1=0.0
do i=1,st
   sum1=sum1 + out_p(i)/(st*runs)
enddo
write(2,*) sum1/10.0
close(2)

end program
