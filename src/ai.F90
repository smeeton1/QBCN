program walk_test
use walker
use measure
use rules
implicit none
type(node),dimension(:),allocatable  :: walk
real,dimension(:,:),allocatable      :: syn0,syn1,syn2,syn3,syn4,syn5,y,x
real,dimension(:),allocatable        :: in,out,out_d,out_d1,out_p,syn0_d,syn1_d,syn2_d,syn3_d,syn4_d,out_d2,out_d3,out_d4,out_d5
real,dimension(:),allocatable        :: out_d6, out2,out3,out4,out5,out6,in2,in3,in4,in5,in6
integer                              :: n,i,j,k,l,wend,st,n_e,n_s,m,rn,runs,w,n_1,n_2,n_3,n_4,n_5
complex                              :: nm
real                                 :: dt,s,g0,openlevel,Err

openlevel =0.5
runs=1
st=100
n_s=7
n_1=13
n_2=18
n_3=22
n_4=25
n_e=27
wend=30
m=2
dt=0.5
s=2.0
g0=0.5
rn=2
n=27

allocate(walk(n),y(2,10),x(n_s,10),out_p(st))
allocate(out(6),out2(5),out3(4),out4(3),out5(2))
allocate(out_d(2),out_d1(3),out_d2(4))
allocate(in(5),in2(4),in3(3))
allocate(syn0(6,7),syn1(5,6),syn2(4,5),syn3(3,4),syn4(2,3))
allocate(syn0_d(5),syn1_d(4),syn2_d(3))
x=reshape((/ 1,0,0,0,1,1,1, 0,1,1,1,1,0,1, 1,0,0,0,0,0,0, 1,1,1,1,0,1,1, 0,1,0,1,0,1,0, &
1,0,1,0,1,0,1, 0,0,0,0,1,1,1, 1,1,0,0,1,1,0, 0,0,0,0,1,1,1, 0,1,1,0,1,1,0/), shape(x))
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
 allocate(walk(i)%nphi(n_s+n_2-n_1),walk(i)%c_e(n_s+n_2-n_1),walk(i)%e_of(n_s+n_2-n_1))
 walk(i)%n=i
 walk(i)%e=n_s+n_2-n_1
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=1
 walk(i)%c_e(2)=2
 walk(i)%c_e(3)=3
 walk(i)%c_e(4)=4
 walk(i)%c_e(5)=5
 walk(i)%c_e(6)=6
 walk(i)%c_e(7)=7
 walk(i)%c_e(8)=14
 walk(i)%c_e(9)=15
 walk(i)%c_e(10)=16
 walk(i)%c_e(11)=17
 walk(i)%c_e(12)=18
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo
do i=n_1+1,n_2
 allocate(walk(i)%nphi(10),walk(i)%c_e(10),walk(i)%e_of(10))
 walk(i)%n=i
 walk(i)%e=10
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=8
 walk(i)%c_e(2)=9
 walk(i)%c_e(3)=10
 walk(i)%c_e(4)=11
 walk(i)%c_e(5)=12
 walk(i)%c_e(6)=13
 walk(i)%c_e(7)=19
 walk(i)%c_e(8)=20
 walk(i)%c_e(9)=21
 walk(i)%c_e(10)=22
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo
do i=n_2+1,n_3
 allocate(walk(i)%nphi(8),walk(i)%c_e(8),walk(i)%e_of(8))
 walk(i)%n=i
 walk(i)%e=8
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=14
 walk(i)%c_e(2)=15
 walk(i)%c_e(3)=16
 walk(i)%c_e(4)=17
 walk(i)%c_e(5)=18
 walk(i)%c_e(6)=23
 walk(i)%c_e(7)=24
 walk(i)%c_e(8)=26
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo
do i=n_3+1,n_4
 allocate(walk(i)%nphi(6),walk(i)%c_e(6),walk(i)%e_of(6))
 walk(i)%n=i
 walk(i)%e=6
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=19
 walk(i)%c_e(2)=20
 walk(i)%c_e(3)=21
 walk(i)%c_e(4)=22
 walk(i)%c_e(5)=26
 walk(i)%c_e(6)=27
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo
do i=n_4+1,n_e
 allocate(walk(i)%nphi(3),walk(i)%c_e(3),walk(i)%e_of(3))
 walk(i)%n=i
 walk(i)%e=3
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=23
 walk(i)%c_e(2)=24
 walk(i)%c_e(3)=27
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
call random_matrix(syn2,w*TIME())
call random_matrix(syn3,w*TIME())
call random_matrix(syn4,w*TIME())
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
    
    call ai_forward(walk,n_1+1,n_2,out3,syn2,openlevel,out2)
    !write(*,*) "6.1"

    !write(*,*) "7"
    do i=1,wend !final steps for walker to head to out put 
      call step(walk,m,n,dt,s,g0,rn)
    enddo
    
    call ai_forward(walk,n_2+1,n_3,out4,syn3,openlevel,out3)
    !write(*,*) "6.1"

    !write(*,*) "7"
    do i=1,wend !final steps for walker to head to out put 
      call step(walk,m,n,dt,s,g0,rn)
    enddo
    
    call ai_forward(walk,n_3+1,n_4,out5,syn4,openlevel,out4)
    !write(*,*) "6.1"

    !write(*,*) "7"
    do i=1,wend !final steps for walker to head to out put 
      call step(walk,m,n,dt,s,g0,rn)
    enddo

    do i=1,n_e
      out5(i)=1/(1+exp(-out5(i)))
      out_d(i)=(y(i,l)-out5(i))*(out5(i)*(1-out5(i)))
    enddo

    !write(*,*) "9"
    
    Err=abs((y(1,l)-out5(1))+(y(2,l)-out5(2)))
    !write(*,*) "9.1"
    do i=1,2 !checking outputs
      if(walk(12+i)%o_f.eqv..true.)then
	    out3(i)=1
      else
	    out3(i)=0
      endif
    enddo
    !write(*,*) "9.2"
    if((out5(1).eq.y(1,l)).or.(out5(2).eq.y(2,l)).and..NOT.((walk(34)%o_f).and.(walk(35)%o_f)))then
     out_p(k)=out_p(k)+1
    endif
    
    !write(*,*) "9.3"
    do i=1,n_e
      do j=1,n_2
	    syn4(i,j)=syn4(i,j)+out_d(i)*out4(j)
      enddo
    enddo
    !write(*,*) "10"
    call ai_backward(out4,out3,out_d,syn3,syn4)
    
    call ai_backward(out3,out2,out4,syn2,syn3)
    
    call ai_backward(out2,out,out3,syn1,syn2)
    !write(*,*) "11"
    call ai_backward(out,X(:,l),out2,syn0,syn1)
    
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


open(2, file='ai_1o_P.dat', status='replace',action='write')

do i=1,st
  write(2,*) out_p(i)/(10*runs)
enddo

close(2)
close(9)
close(5)

end program
