!*****************************************************************
! program to print whether the rank of the process is even or odd
!*****************************************************************
program odd_even
  ! load mpi library
  use MPI
  implicit none
  integer :: rank,ierror

  ! initialise mpi enviornment
  call MPI_INIT(ierror)

  ! get process rank
  call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)

  ! find if the rank is odd or even and
  ! print the information
  if(mod(rank,2).eq.0) then
    write(*,*)"My rank is ", rank, " and I am even-ranked"
  else
    write(*,*)"My rank is ", rank, " and I am odd-ranked"
  endif

  ! finalize mpi
  call MPI_FINALIZE(ierror)
end program odd_even
