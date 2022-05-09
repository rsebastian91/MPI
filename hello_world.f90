!*************************************************
! program to print Hello World by each proc
!*************************************************

program hello_world
  ! includes the MPI library in order to use the MPI functionalities 
  use MPI
  implicit none   ! all variables are explicitly defined
  integer :: rank,ierror

  ! initialise mpi enviornment
  call MPI_INIT(ierror) 

  ! get rank of each proc
  call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)

  ! print hello world from each process
    write(*,*)"Hello World !!!, My rank is ", rank

  ! finalize mpi
  call MPI_FINALIZE(ierror)
end program hello_world
