program walk_test
use walker
use measure
use rules
implicit none
type(node),dimension(:),allocatable  :: walk
real,dimension(:,:),allocatable      :: syn0
real,dimension(:),allocatable        :: in,out,out_d,y
integer                              :: n,i,j,wend



allocate(walk(6),syn0(2,3),in(3),out(2),out_d(2),y(2))


do i=1,n_o
  do j=1,n_i
    syn0(j,i)=2*rand()-1
  enddo
enddo




do i=1,wend
  call step(walk,2,n,0.2,1.0,1.0)
enddo
do i=1,3
  if(walk(i).o_f.eq..True.)then
   in(i)=1
  else
   in(i)=0
  endif
  walk(i).e_of(1)=.true.
  walk(4).e_of(i)=.true.
enddo

out=matmul(syn0,in)

do i=1,2
 if(out(i).qt.0.5)then
    walk(4).e_of(3+i)=.true.
    walk(4+i).e_of(1)=.true.
 endif
enddo
do i=1,wend
  call step(walk,2,n,0.2,1.0,1.0)
enddo
do i=1,2
  if(walk(4+i).o_f.eq..True.)then
   out(i)=1
  else
   out(i)=0
  endif
enddo
do i=1,n_o
    out_d(i)=(y(i,l)-out(i))*(out(i)*(1-out(i)))
enddo
  
do i=1,n_o
   do j=1,n_i
     syn0(i,j)=syn0(i,j)+x(j,l)*out_d(i)
   enddo
enddo



end program