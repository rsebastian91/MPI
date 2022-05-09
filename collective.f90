!***********************************************************
! program to integrate a function using multiple processor
!***********************************************************
program collective

  ! load MPI library
  use MPI

  implicit none
 
  ! calculation variables
  integer, parameter                     :: dp = kind(1.d0) 
  integer, parameter                     :: li = selected_int_kind(15)
  integer(kind=li)                       :: nb_pts,i
  real(kind=dp)                          :: dx,sum_local,ans
  real(kind=dp),allocatable,dimension(:) :: scatter_data,grid_points
  integer                                :: block_length
 
  ! MPI variable
  integer                                :: rank,ierror,nb_procs

  ! number of points
  nb_pts = 3*1000*1000_li*100  

  ! grid size
  dx = 1_dp / real(nb_pts,dp)  

  sum_local = 0_dp

  ! initialise MPI
  call MPI_INIT(ierror)
 
  ! get number of processors
  call MPI_COMM_SIZE(MPI_COMM_WORLD,nb_procs,ierror)

 ! get rank of the procs
  call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)

  ! number of points handled by each procs
  block_length=nb_pts/nb_procs
  ! array containing data for transfer 
  allocate(scatter_data(block_length))

  ! global array containing grid points for rectangular integration
  allocate(grid_points(nb_pts))

  ! point defined at the center of the rectangle
  do i=1,nb_pts
    grid_points(i)=dx*(i-0.5_dp)
  enddo

  ! scatter grid points to all procs with size block_length
  call MPI_SCATTER(grid_points,block_length,MPI_REAL8,scatter_data,block_length,&
                      MPI_REAL8,0,MPI_COMM_WORLD,ierror)

  ! each proc computes the following integration using the scatter_data
  ! available to them independently
  do i=1, block_length 
    sum_local = sum_local + dx*(4_dp / (1_dp + scatter_data(i)**2))
  end do

  ! advanced collective operation to add local integration <<sum_local>> of 
  ! each proc and save the result to variable <<ans>> for all procs
  call MPI_ALLREDUCE(sum_local,ans,1,MPI_REAL8,MPI_SUM,MPI_COMM_WORLD,ierror)

  ! print results with proc number, local integration result and global result
  write(*,900) 'proc ',rank,' : partial sum = ',sum_local, " Pi =", ans

  ! finalize mpi
  call MPI_FINALIZE(ierror)
900 format(1x,a,i2,1x,a,f6.4,1x,a,f6.4)
end program collective
