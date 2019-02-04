module measure
  implicit none
  
 contains 

subroutine jmes(Q,Res,dt)
  complex*16,dimension(:),intent(inout) :: Q
  logical,intent(out)                   :: Res
  real,intent(in)                       :: dt
  real*8                                :: tsum,ran,ran1
  integer                               :: t(12),n,i
  
  t(1)=TIME()
  
  tsum=real(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))*dt
  call random_seed(PUT=t)
  call random_number(ran)! = RAND(t)

  
  if(ran.lt.tsum)then
    call random_number(ran1)! = RAND(t(1))
    do while((ran.gt.real(CONJG(Q(1))*Q(1)).and.ran1.gt.real(CONJG(Q(2))*Q(2))))
      do while((ran.lt.real(CONJG(Q(1))*Q(1)).and.ran1.lt.real(CONJG(Q(2))*Q(2))))
        call random_number(ran)!  = RAND(t(1))!
        call random_number(ran1)! = RAND(t(1))
      end do
    enddo
    
    if(ran.lt.real(CONJG(Q(1))*Q(1)))then
      Res=.true.
    else
      Res=.false.
    endif
    
    do i=1,10
     Q(1)=Q(1)+(-0.5*Q(1)+cmplx(0.0,1.0)*Q(2))*dt/10
     Q(2)=Q(2)+(-0.5*Q(2)+cmplx(0.0,1.0)*Q(1))*dt/10
     Q(1)=Q(1)/sqrt(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
     Q(2)=Q(2)/sqrt(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
    enddo
  endif
  

end subroutine



subroutine qbit_rho_inter(qbit,phi,s,dt,g0)
  complex*16,dimension(:,:),intent(inout) :: qbit
  complex*16,dimension(:),intent(in)      :: phi
  real,intent(in)                         :: dt,s,g0
  complex*16                              :: hold,hold2,hold3,hold4,norm
  integer                                 :: l,i
  real*8                                  :: g

 l=size(phi)
 g=0.0
 do i=1,l
   g=g+abs(real(phi(i)*conjg(phi(i))))
 enddo

 if(abs(g).gt.0.00001)then
 
  do i=1,10
   hold =qbit(1,1)+(dt/10.0)*s*g*(2.0*qbit(2,2))
   hold2=qbit(1,2)-(dt/10.0)*s*g*(qbit(1,2))
   hold3=qbit(2,1)-(dt/10.0)*s*g*(qbit(2,1))
   hold4=qbit(2,2)-(dt/10.0)*s*g*(2.0*qbit(2,2))
 
   norm=hold+hold4
 
   qbit(1,1)=hold/norm
   qbit(1,2)=hold2/norm
   qbit(2,1)=hold3/norm
   qbit(2,2)=hold4/norm
  enddo
 
 else
 
  do i=1,10
   hold =qbit(1,1)-(dt/10.0)*s*g0*(2.0*qbit(1,1))
   hold2=qbit(1,2)-(dt/10.0)*s*g0*(qbit(1,2))
   hold3=qbit(2,1)-(dt/10.0)*s*g0*(qbit(2,1))
   hold4=qbit(2,2)+(dt/10.0)*s*g0*(2.0*qbit(1,1))
 
   norm=hold+hold4
 
   qbit(1,1)=hold/norm
   qbit(1,2)=hold2/norm
   qbit(2,1)=hold3/norm
   qbit(2,2)=hold4/norm
  enddo
 
 endif


end subroutine


subroutine jmes_den(Q,Res,dt)
  complex*16,dimension(:,:),intent(inout) :: Q
  logical,intent(out)                   :: Res
  real,intent(in)                       :: dt
  complex*16                            :: hold,hold2
  real*8                                :: tsum,ran,ran1
  integer                               :: t(12),n,i
  
  t(1)=TIME()
  ran=-1.0
  do while(ran.lt.0.000001)
   tsum=real(Q(1,1)+Q(2,2))*dt
   call random_seed(PUT=t)
   call random_number(ran)! = RAND(t)
  end do

  
  if(ran.lt.tsum)then
    call random_number(ran1)! = RAND(t(1))
    do while((ran.gt.real(Q(1,1)).and.ran1.gt.real(Q(2,2))))
      do while((ran.lt.real(Q(1,1)).and.ran1.lt.real(Q(2,2))))
       do while(ran.lt.0.000001.and.ran1.lt.0.000001)
        call random_number(ran)!  = RAND(t(1))!
        call random_number(ran1)! = RAND(t(1))
       end do
      end do
    enddo
    
    if(ran.lt.real(Q(1,1)))then
      Res=.true.
    else
      Res=.false.
    endif
    
    do i=1,10
     hold=Q(1,1)
     hold2=Q(2,2)
     Q(1,1)=hold+(-0.5*hold+cmplx(0.0,1.0)*hold2)*dt/10
     Q(2,2)=hold2+(-0.5*hold2+cmplx(0.0,1.0)*hold)*dt/10
     hold=Q(1,1)+Q(2,2)
     Q(1,1)=Q(1,1)/hold
     Q(2,2)=Q(2,2)/hold
    enddo
  endif
  

end subroutine


end module


! program j_test
! use jump
! !use IFCORE
! implicit none
! complex,dimension(:),allocatable :: Q
! integer                          :: R
! Logical(4)                       :: S
! 
! allocate(Q(2))
! Q(1)=cmplx(1/sqrt(2.0),0.0)
! Q(2)=cmplx(1/sqrt(2.0),0.0)
! Q(1)=Q(1)/(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
! Q(2)=Q(2)/(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
! S=.True.
! 
! Write(*,*)Q(1)
! Write(*,*)(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
! call jmes(Q,S)
! Write(*,*)S
! Write(*,*)(CONJG(Q(1))*Q(1)+CONJG(Q(2))*Q(2))
! 
! end program