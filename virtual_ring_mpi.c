#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>
#include <signal.h>
#define COMM_TAG 1000

double tst_abc;

void pass_its_neighbor(const int rank, const int size, int* buf)
{  
  MPI_Request request[2];
  MPI_Status status[2];
  int rcvbuf;
  char *hn = malloc(8);
  char *bigblock = malloc(1024);
  gethostname (hn, 32);
 
  tst_abc = 0.3;

  MPI_Irecv((void*)buf, 1, MPI_INT, ((rank+size-1)%size), COMM_TAG, MPI_COMM_WORLD, &request[0]);
  MPI_Isend((void*)&rank, 1, MPI_INT, ((rank+1)%size), COMM_TAG, MPI_COMM_WORLD, &request[1]);
  MPI_Waitall(2, request, status);

  if (rank == 0) {
    fprintf(stdout, "size of this program is %d\n", size); 
  }

  MPI_Allreduce((void *) &rank, (void *) &rcvbuf, 1, MPI_INT, MPI_MAX, MPI_COMM_WORLD); 

  if (rank==0) {
    fprintf(stdout, "rcvbuf: %d \n", rcvbuf);
  }

  //bigblock[1024]='d';
  //bigblock[1025]='e';

  //raise (SIGSEGV);

  MPI_Barrier(MPI_COMM_WORLD);
}

int main(int argc, char* argv[])
{
  int size, rank, i;  
  int *buf = (int*) malloc(sizeof(int));

  tst_abc = 0.2; 

  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  *buf=rank; /* we only pass around rank*/
  pass_its_neighbor(rank, size, buf);
  free (buf);

  MPI_Finalize();

  return 0;
}
