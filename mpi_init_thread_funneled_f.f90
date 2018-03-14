program hello
   include 'mpif.h'
   integer ierror
   integer provided
   logical flag
   
   call MPI_INITIALIZED(flag, ierror)
   call MPI_INIT_THREAD(MPI_THREAD_FUNNELED, provided, ierror)
   print*, 'node', rank, ': Hello world'
   call MPI_FINALIZE(ierror)
   if (.NOT. flag .OR. provided .NE. MPI_THREAD_FUNNELED) then
       stop 1
   endif
end
