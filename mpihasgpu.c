/*
 * Prints out one line for each rank:
 * the MPI rank, 
 * host name of the node being run on,
 * whether or not the GPU specified by CUDA_VISIBLE_DEVICES is usable, 
 * and what CPUs the MPI task is bound to.
 *
 * Rewritten by John Gyllenhaal at LLNL 6Oct2015
 * in order to add CPU binding info based on omp_hello.c
 * written by Edgar A. Leon 
 * Lawrence Livermore National Laboratory
 */ 

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>           // sysconf
#include <stdint.h>
#include <omp.h>
#include <string.h>

/* __USE_GNU is needed for CPU_ISSET definition */ 
#ifndef __USE_GNU
#define __USE_GNU 1                
#endif
#include <sched.h>            // sched_getaffinity

#include "mpi.h"

/* Print out info line for each MPI task */
int main(int argc, char *argv[]) 
{
    int rank, np; 
    int rc;
    char host[1024] = "unknown";
    char gpuid[10];
    const char *device=NULL;
    char cpus[1024 * 6] = "none";  
    cpu_set_t cpumask;
    int i, nc;
    int runningOnGPU = 0;
    
    rc = MPI_Init(&argc,&argv);
    if (rc != MPI_SUCCESS) {
	printf ("Error starting MPI program. Terminating.\n");
	MPI_Abort(MPI_COMM_WORLD, rc);
    }
    
    MPI_Comm_rank(MPI_COMM_WORLD, &rank); 
    MPI_Comm_size(MPI_COMM_WORLD, &np); 
    
    /* Which host are we running on */
    gethostname (host, sizeof(host));
    
    /* From env, get CUDA_VISIBLE_DEVICES which determines which
     * GPU to use, defaults to 0 if not set
     */
    device = getenv("CUDA_VISIBLE_DEVICES");
    if (device == NULL)
	device = "0";
    strncpy (gpuid, device, sizeof(gpuid));

    /* Get CPUs bound to.  Based on Edgar Leon's omp_hello.c code */
    /* Use static cpu_set_t mask, 1024 max size */
    CPU_ZERO_S(sizeof(cpumask), &cpumask);
    if (sched_getaffinity(0, sizeof(cpumask), &cpumask) == -1)
    {
	fprintf (stderr, "Error during sched_getaffinity\n");
	exit (1);
    } 

    /* Use hardcoded default max of 1024 cpus for cpumask
     * in order to build up 'cpus' string of bound cpus
     */
    nc =0;
    for (i=0; i < 1024; i++)
    {
        if (CPU_ISSET_S(i, sizeof(cpumask), &cpumask))
        {
	    nc += sprintf (cpus+nc, "%d ", i);
            /* printf ("Found CPU %i\n", i); */
        }
    }
    
    /* Determine if GPU is available but no longer use
     * printfs on the GPU since it breaks the Cray Compiler 
     * in Nov 2015. 
     */
#pragma omp target map(runningOnGPU)
    {
        if (omp_is_initial_device() == 0)
            runningOnGPU = 1;
    }
 
    /* If still running on CPU, GPU must not be available */
    if (!runningOnGPU)
        printf("Rank %3i Host %-12s UNABLE to use GPU %s  CPUs %s\n", 
               rank, host, gpuid, cpus); 
    else
        printf("Rank %3i Host %-12s   Able to use GPU %s  CPUs %s\n", 
       	       rank, host, gpuid, cpus);
    
    MPI_Finalize(); 
    
    return 0; 
}
