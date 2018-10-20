module walker

 implicit none
 
 type,public :: node
  integer  :: n,e
  logical  :: o_f
  complex*16, dimension(e,2) :: nphi
  complex*16, dimension(2)   :: nqphi
  logical,dimension(e)       :: e_of
  
  contains
    procedure,
    procedure,
 
 
 end type node





end module