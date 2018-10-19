module walker

 implicit none
 
 type,public :: node
  integer  :: n,e
  logical  :: o_f
  complex*16, dimension(:,:), allocatable:: nphi
  complex*16, dimension(:), allocatable  :: nqphi
  logical,dimension(:),allocatable       :: e_of
  
  contains
    procedure,
    procedure,
 
 
 end type node





end module