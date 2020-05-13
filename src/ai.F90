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
real                                 :: dt,s,g0,Err


runs=1000
st=100
n_s=8
n_1=7
n_2=6
n_3=5
n_4=4
n_5=3
n_e=2
wend=10
m=2
dt=0.5
s=2.0
g0=0.5
rn=2
n=5

allocate(walk(35),y(2,10),x(8,10),out_p(runs))
allocate(out(7),out2(6),out3(5),out4(4),out5(3),out6(2))
allocate(out_d(2),out_d1(3),out_d2(4),out_d3(5),out_d4(6),out_d5(7),out_d6(8))
allocate(in(8),in2(7),in3(6),in4(5),in5(4),in6(3))
allocate(syn0(7,8),syn1(6,7),syn2(5,6),syn3(4,5),syn4(3,4),syn5(2,3))
allocate(syn0_d(7),syn1_d(6),syn2_d(5),syn3_d(4),syn4_d(3))
x=reshape((/ 1,0,0,0,1,1,1,0, 0,1,1,1,1,1,1,1, 1,0,0,0,0,0,0,0, 1,1,1,1,0,0,0,0, 0,1,0,1,0,1,0,1, &
1,0,1,0,1,0,1,0, 0,0,0,0,1,1,1,1, 1,1,0,0,1,1,0,0, 0,0,0,0,1,0,0,0, 0,1,1,0,1,1,0,1/), shape(x))
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

do i=1,8  !setting up walker
 allocate(walk(i)%nphi(7),walk(i)%c_e(7),walk(i)%e_of(7))
 walk(i)%n=i
 walk(i)%e=7
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=9
 walk(i)%c_e(2)=10
 walk(i)%c_e(3)=11
 walk(i)%c_e(4)=12
 walk(i)%c_e(5)=13
 walk(i)%c_e(6)=14
 walk(i)%c_e(7)=15
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo
do i=9,15
 allocate(walk(i)%nphi(14),walk(i)%c_e(14),walk(i)%e_of(14))
 walk(i)%n=i
 walk(i)%e=14
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=1
 walk(i)%c_e(2)=2
 walk(i)%c_e(3)=3
 walk(i)%c_e(4)=4
 walk(i)%c_e(5)=5
 walk(i)%c_e(6)=6
 walk(i)%c_e(7)=7
 walk(i)%c_e(8)=8
 walk(i)%c_e(9)=16
 walk(i)%c_e(10)=17
 walk(i)%c_e(11)=18
 walk(i)%c_e(12)=19
 walk(i)%c_e(13)=20
 walk(i)%c_e(14)=21
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo
do i=16,21
 allocate(walk(i)%nphi(12),walk(i)%c_e(12),walk(i)%e_of(12))
 walk(i)%n=i
 walk(i)%e=12
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=9
 walk(i)%c_e(2)=10
 walk(i)%c_e(3)=11
 walk(i)%c_e(4)=12
 walk(i)%c_e(5)=13
 walk(i)%c_e(6)=14
 walk(i)%c_e(7)=15
 walk(i)%c_e(8)=22
 walk(i)%c_e(9)=23
 walk(i)%c_e(10)=24
 walk(i)%c_e(11)=25
 walk(i)%c_e(12)=26
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo
do i=22,26
 allocate(walk(i)%nphi(10),walk(i)%c_e(10),walk(i)%e_of(10))
 walk(i)%n=i
 walk(i)%e=10
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=16
 walk(i)%c_e(2)=17
 walk(i)%c_e(3)=18
 walk(i)%c_e(4)=19
 walk(i)%c_e(5)=20
 walk(i)%c_e(6)=21
 walk(i)%c_e(7)=27
 walk(i)%c_e(8)=28
 walk(i)%c_e(9)=29
 walk(i)%c_e(10)=30
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo
do i=27,30
 allocate(walk(i)%nphi(8),walk(i)%c_e(8),walk(i)%e_of(8))
 walk(i)%n=i
 walk(i)%e=8
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=22
 walk(i)%c_e(2)=23
 walk(i)%c_e(3)=24
 walk(i)%c_e(4)=25
 walk(i)%c_e(5)=26
 walk(i)%c_e(6)=31
 walk(i)%c_e(7)=32
 walk(i)%c_e(8)=33
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo
do i=31,33
 allocate(walk(i)%nphi(6),walk(i)%c_e(6),walk(i)%e_of(6))
 walk(i)%n=i
 walk(i)%e=6
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=27
 walk(i)%c_e(2)=28
 walk(i)%c_e(3)=29
 walk(i)%c_e(4)=30
 walk(i)%c_e(5)=34
 walk(i)%c_e(6)=35
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo
do i=34,35
 allocate(walk(i)%nphi(3),walk(i)%c_e(3),walk(i)%e_of(3))
 walk(i)%n=i
 walk(i)%e=3
 walk(i)%o_f=.false.
 walk(i)%c_e(1)=31
 walk(i)%c_e(2)=32
 walk(i)%c_e(3)=33
 do j=1,walk(i)%e
   walk(i)%nphi(j)=cmplx(0.0,0.0)
 enddo
enddo


open(9, file='ai.dat', status='replace',action='write')
open(5, file='out_prob.dat', status='replace',action='write')
do w=1,runs
call random_matrix(syn0)
call random_matrix(syn1)
call random_matrix(syn2)
call random_matrix(syn3)
call random_matrix(syn4)
call random_matrix(syn5)
do k=1,st
  do l=1,10
    do i=1,35
      walk(i)%nphi(:)=cmplx(0.0,0.0)
    enddo
    nm=sqrt(x(1,l)+x(2,l)+x(3,l)+x(4,l)+x(5,l)+x(6,l)+x(7,l)+x(8,l))
    do i=1,8  !setting up the walker for ai run
      walk(i)%nphi(1)=cmplx((x(i,l)/nm)/sqrt(2.0))
      walk(i)%nphi(2)=cmplx((x(i,l)/nm)/sqrt(2.0))
      walk(i)%nphi(3)=cmplx((x(i,l)/nm)/sqrt(2.0))
      walk(i)%nphi(4)=cmplx((x(i,l)/nm)/sqrt(2.0))
      walk(i)%nphi(5)=cmplx((x(i,l)/nm)/sqrt(2.0))
      walk(i)%nphi(6)=cmplx((x(i,l)/nm)/sqrt(2.0))
      walk(i)%nphi(7)=cmplx((x(i,l)/nm)/sqrt(2.0))
      walk(i)%nqphi(:,:)=cmplx(0.0,0.0)
      walk(i)%nqphi(2,2)=cmplx(1.0,0.0)
      walk(i)%e_of(:)=.false.
    enddo
    do i=9,35
      walk(i)%nphi(:)=cmplx(0.0,0.0)
      walk(i)%nqphi(:,:)=cmplx(0.0,0.0)
      walk(i)%nqphi(2,2)=cmplx(1.0,0.0)
      walk(i)%e_of(:)=.false.
    enddo
    !write(*,*) norm(walk,n)
  
  
    do i=1,wend  !taking the steps
      call step(walk,m,n,dt,s,g0,rn)
    enddo
    !write(*,*) norm(walk,n)
    do i=1,8 !readinjg out results of qw
     ! write(*,*)walk(i)%o_f
      if(walk(i)%o_f)then
	    in(i)=1
      else
	    in(i)=0
      endif
    enddo
    !write(*,*) in
    out=matmul(syn0,in)
    !write(*,*) out
    do i=1,7 ! setting up out puts
      if(out(i).gt.0.5)then
	   walk(1)%e_of(i)=.true.
	   walk(2)%e_of(i)=.true.
	   walk(3)%e_of(i)=.true.
	   walk(4)%e_of(i)=.true.
	   walk(5)%e_of(i)=.true.
	   walk(6)%e_of(i)=.true.
	   walk(7)%e_of(i)=.true.
	   walk(8)%e_of(i)=.true.
	   walk(8+i)%e_of(:)=.true.
      endif
    enddo
    do i=1,wend !final steps for walker to head to out put 
      call step(walk,m,n,dt,s,g0,rn)
    enddo

    do i=9,15 !readinjg out results of qw
     ! write(*,*)walk(i)%o_f
      if(walk(i)%o_f)then
	    in2(i-8)=1
      else
	    in2(i-8)=0
      endif
    enddo
    !write(*,*) in
    out2=matmul(syn1,in2)
    !write(*,*) out
    do i=1,6 ! setting up out puts
      if(out2(i).gt.0.5)then
	   walk(9)%e_of(i)=.true.
	   walk(10)%e_of(i)=.true.
	   walk(11)%e_of(i)=.true.
	   walk(12)%e_of(i)=.true.
	   walk(13)%e_of(i)=.true.
	   walk(14)%e_of(i)=.true.
	   walk(15)%e_of(i)=.true.
	   walk(15+i)%e_of(:)=.true.
      endif
    enddo
    
    do i=1,wend !final steps for walker to head to out put 
      call step(walk,m,n,dt,s,g0,rn)
    enddo
    
    do i=16,21 !readinjg out results of qw
     ! write(*,*)walk(i)%o_f
      if(walk(i)%o_f)then
	    in3(i)=1
      else
	    in3(i)=0
      endif
    enddo
    !write(*,*) in
    out3=matmul(syn2,in3)
    !write(*,*) out
    do i=1,5 ! setting up out puts
      if(out3(i).gt.0.5)then
	   walk(16)%e_of(i)=.true.
	   walk(17)%e_of(i)=.true.
	   walk(18)%e_of(i)=.true.
	   walk(19)%e_of(i)=.true.
	   walk(20)%e_of(i)=.true.
	   walk(21)%e_of(i)=.true.
	   walk(21+i)%e_of(:)=.true.
      endif
    enddo
    
    do i=1,wend !final steps for walker to head to out put 
      call step(walk,m,n,dt,s,g0,rn)
    enddo
    
    do i=22,26 !readinjg out results of qw
     ! write(*,*)walk(i)%o_f
      if(walk(i)%o_f)then
	    in4(i)=1
      else
	    in4(i)=0
      endif
    enddo
    !write(*,*) in
    out4=matmul(syn3,in4)
    !write(*,*) out
    do i=1,4 ! setting up out puts
      if(out4(i).gt.0.5)then
	   walk(22)%e_of(i)=.true.
	   walk(23)%e_of(i)=.true.
	   walk(24)%e_of(i)=.true.
	   walk(25)%e_of(i)=.true.
	   walk(26)%e_of(i)=.true.
	   walk(26+i)%e_of(:)=.true.
      endif
    enddo
    
    do i=1,wend !final steps for walker to head to out put 
      call step(walk,m,n,dt,s,g0,rn)
    enddo
    
    
    do i=27,30 !readinjg out results of qw
     ! write(*,*)walk(i)%o_f
      if(walk(i)%o_f)then
	    in5(i)=1
      else
	    in5(i)=0
      endif
    enddo
    !write(*,*) in
    out5=matmul(syn4,in5)
    !write(*,*) out
    do i=1,3 ! setting up out puts
      if(out5(i).gt.0.5)then
	   walk(27)%e_of(i)=.true.
	   walk(28)%e_of(i)=.true.
	   walk(29)%e_of(i)=.true.
	   walk(30)%e_of(i)=.true.
	   walk(30+i)%e_of(:)=.true.
      endif
    enddo
    
    do i=1,wend !final steps for walker to head to out put 
      call step(walk,m,n,dt,s,g0,rn)
    enddo
    
    
    do i=31,33 !readinjg out results of qw
     ! write(*,*)walk(i)%o_f
      if(walk(i)%o_f)then
	    in6(i)=1
      else
	    in6(i)=0
      endif
    enddo
    !write(*,*) in
    out6=matmul(syn5,in6)
    !write(*,*) out
    do i=1,2 ! setting up out puts
      if(out2(i).gt.0.5)then
	   walk(31)%e_of(i)=.true.
	   walk(32)%e_of(i)=.true.
	   walk(33)%e_of(i)=.true.
	   walk(33+i)%e_of(:)=.true.
      endif
    enddo
    
    
    do i=1,wend !final steps for walker to head to out put 
      call step(walk,m,n,dt,s,g0,rn)
    enddo
        do i=1,n_e
      out6(i)=1/(1+exp(-out6(i)))
      out_d(i)=(y(i,l)-out6(i))*(out6(i)*(1-out6(i)))
    enddo

    
    
    err=abs((y(1,l)-out6(1))+(y(2,l)-out6(2)))
    do i=1,2 !checking outputs
      if(walk(33+i)%o_f.eqv..true.)then
	    out6(i)=1
      else
	    out6(i)=0
      endif
    enddo
    if((out6(1).eq.y(1,l)).or.(out6(2).eq.y(2,l)).and..NOT.((walk(34)%o_f).and.(walk(35)%o_f)))then
     out_p(w)=out_p(w)+1
    endif
    

    do i=1,n_e
      do j=1,n_5
	    syn5(i,j)=syn5(i,j)+out_d(j)*out5(i)
      enddo
    enddo
    
    syn4_d = matmul(out_d,syn5)
    
    do i=1,n_5
     out5(i) = 1/(1+exp(-out5(i)))
     out_d1(i)= syn4_d(i)*out5(i)*(1-out5(i))
    enddo

    do i=1,n_5
      do j=1,n_4
	    syn4(i,j)=syn4(i,j)+out_d1(j)*out4(i)
      enddo
    enddo
    
    syn3_d = matmul(out_d1,syn4)
    
    do i=1,n_4
     out4(i) = 1/(1+exp(-out4(i)))
     out_d2(i)= syn4_d(i)*out4(i)*(1-out4(i))
    enddo

    do i=1,n_4
      do j=1,n_3
	    syn3(i,j)=syn3(i,j)+out_d2(j)*out3(i)
      enddo
    enddo

    syn2_d = matmul(out_d2,syn3)
    
    do i=1,n_3
     out3(i) = 1/(1+exp(-out3(i)))
     out_d3(i)= syn2_d(i)*out3(i)*(1-out3(i))
    enddo

    do i=1,n_3
      do j=1,n_2
	    syn2(i,j)=syn2(i,j)+out_d3(j)*out2(i)
      enddo
    enddo      
    
    
    syn1_d = matmul(out_d3,syn2)
    
    do i=1,n_2
     out2(i) = 1/(1+exp(-out2(i)))
     out_d4(i)= syn1_d(i)*out2(i)*(1-out2(i))
    enddo

    do i=1,n_2
      do j=1,n_1
	    syn2(i,j)=syn2(i,j)+out_d4(j)*out(i)
      enddo
    enddo    
    
    
    syn0_d = matmul(out_d5,syn0)
    
    do i=1,n_1
     out(i) = 1/(1+exp(-out(i)))
     out_d6(i)= syn0_d(i)*out(i)*(1-out(i))
    enddo

    do i=1,n_1
      do j=1,n_s
	    syn0(i,j)=syn0(i,j)+out_d6(j)*X(i,l)
      enddo
    enddo 
    
    
    
  enddo
  write(9,*) err!(out_d(1)+out_d(2))/2
  write(5,*) node_prob(walk(4))!+node_prob(walk(5))
  !write(*,*) norm(walk,n)
enddo
  
enddo

open(2, file='ai_1o_P.dat', status='replace',action='write')

do i=1,runs
  write(2,*) out_p(i)/(3*runs)
enddo

close(2)
close(9)
close(5)

end program
