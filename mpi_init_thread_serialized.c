#include <mpi.h>

int main (int argc, char *argv[])
{
    int rc = 0;
    int flag = 0; 
    int required = MPI_THREAD_SERIALIZED;
    int provided = 0;
    MPI_Initialized (&flag);
    rc = MPI_Init_thread (&argc, &argv, required, &provided);
    MPI_Finalize ();
    return (flag && rc == 0 && provided == MPI_THREAD_SERIALIZED) ? 0 : 1; 
}

/*
 *  vi:tabstop=4 shiftwidth=4 expandtab
 */
