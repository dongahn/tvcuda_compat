program hello
   include 'mpif.h'
   integer ierror
   logical flag
   
   call MPI_INITIALIZED(flag, ierror)
   call MPI_INIT(ierror)
   print*, 'node', rank, ': Hello world'
   call MPI_FINALIZE(ierror)
   if (.NOT. flag .OR. ierror .NE. 0) then
       stop 1
   endif
end
