#include <mpi.h>

int main (int argc, char *argv[])
{
    int rc = 0;
    int flag = 0; 
    MPI_Initialized (&flag);
    rc = MPI_Init (&argc, &argv);
    MPI_Finalize ();
    return (flag && rc == 0) ? 0 : 1; 
}

/*
 *  vi:tabstop=4 shiftwidth=4 expandtab
 */
