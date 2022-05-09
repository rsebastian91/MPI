!*******************************************************************
! program to perform point-to-point communication between 2 proc
! proc 0 sends information to proc 1 and then proc 1 sends back 
! the same information to proc 0
! size of the information send and recieved back is varied between
! 1 and 10000000 to check the speed and efficiency of communication
!******************************************************************
program point_to_point
  ! load MPI library
  use MPI
  implicit none

  integer, dimension(MPI_STATUS_SIZE)          :: comm_status
  integer, parameter                           :: nb_test=8,tag=99
  integer, dimension(nb_test)                  :: nb_value_to_send
  integer                                      :: rank,ierror,i,j
  integer, parameter                           :: dp = kind(1.d0)
  real(kind=dp), allocatable, dimension(:)     :: value
  real(kind=dp)                                :: time_start,time_end

  ! number of values to send nb_test times
  nb_value_to_send = (/ 1,10,100,1000,10000,100000,1000000,10000000/)

  ! initialise MPI enviornment
  call MPI_INIT(ierror)
  
  ! get rank of the process
  call MPI_COMM_RANK(MPI_COMM_WORLD,rank,ierror)
 
  ! loop to send and recv information in increasing size
  do i=1,nb_test

     ! allocate array at each loop passage
     allocate(value(nb_value_to_send(i)))

     if (rank == 0) then
        !defines array for each loop passage
        ! real type array containing values from 1 to nb_value_to_send(i)
        value=1.0*(/(j,j=1,nb_value_to_send(i))/)

        ! get time
        time_start=MPI_WTIME()

        ! send information from proc 0 to proc 1
        call MPI_SEND(value,nb_value_to_send(i),MPI_REAL8,1,tag, &
                              MPI_COMM_WORLD,ierror)
        
        ! set array to 0 inorder to receive the data sent from proc 1
        value=0.0
        
     elseif (rank == 1) then
        ! recv information from proc 0
        call MPI_RECV(value,nb_value_to_send(i),MPI_REAL8,0,tag,&
             MPI_COMM_WORLD,comm_status,ierror)
     end if ! end sending from proc 0 to 1


     ! send information back from proc 1 to proc 0
     if(rank==1) then
     call MPI_SEND(value,nb_value_to_send(i),MPI_REAL8,0,tag, &
                                   MPI_COMM_WORLD,ierror)
     
     ! recv information from proc 1
     elseif(rank==0) then
     call MPI_RECV(value,nb_value_to_send(i),MPI_REAL8,1,tag,&
                  MPI_COMM_WORLD,comm_status,ierror)

     ! get time
     time_end=MPI_WTIME()
  
     ! print results
     ! last value of the array should be equal to the size of the information transfered
     write(*,900) "Proc ",rank," send and received ",nb_value_to_send(i),&
                  " values (last value of the array is ",value(nb_value_to_send(i)),&
                  ") from proc ",rank+1," in ",time_end-time_start," seconds with transfer rate ",&
                   real(2*nb_value_to_send(i)*8)/1000000./(time_end-time_start)," Mb/s"

     endif ! end sending back from proc 1 to 0

     ! deallocate array
     deallocate(value)
     enddo ! end of loop over nb_test 
     
     !format for printing result
900 format(1x,a,i2,1x,a,i8,1x,a,f12.2,1x,a,i2,1x,a,f8.6,1x,a,f7.2,1x,a)

     ! finalize mpi
     call MPI_FINALIZE(ierror)
end program point_to_point
