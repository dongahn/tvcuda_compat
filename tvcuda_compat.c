/*
 * Author: Dong H. Ahn and John DelSingore
 *
 * PMPI wrapper to make TotalView debug GPU codes on CORAL systems.
 * Essentially, this shifts the invocatoin of MPI_Init to load time from
 * the application's runtime.
 * This W/R should be undone when RWS and NVIDIA put in a permanent fix.
 *
 * Control:
 *     If the application calls threaded init (e.g., MPI_Init_thread ()),
 *     users must set TVCUDA_COMPAT_MPI_INIT_THREAD environment variable
 *     to the required MPI thread support level:
 *         TVCUDA_COMPAT_MPI_INIT_THREAD=SINGLE
 *         TVCUDA_COMPAT_MPI_INIT_THREAD=FUNNELED
 *         TVCUDA_COMPAT_MPI_INIT_THREAD=SERIALIZED
 *         TVCUDA_COMPAT_MPI_INIT_THREAD=MULTIPLE
 */

#define _GNU_SOURCE 1
#define DEBUG 0
#include <mpi.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <dlfcn.h>

/*************************************************************************
 *                        Function Declaration                           *
 *************************************************************************/

extern void pmpi_init (MPI_Fint *ierr);
extern void PMPI_INIT (MPI_Fint *ierr);
extern void pmpi_init_ (MPI_Fint *ierr);
extern void pmpi_init__ (MPI_Fint *ierr);
extern void pmpi_init_thread (MPI_Fint *required, MPI_Fint *provided,
                              MPI_Fint *ierr);
extern void PMPI_INIT_THREAD (MPI_Fint *required, MPI_Fint *provided,
                              MPI_Fint *ierr);
extern void pmpi_init_thread_ (MPI_Fint *required, MPI_Fint *provided,
                               MPI_Fint *ierr);
extern void pmpi_init_thread__ (MPI_Fint *required, MPI_Fint *provided,
                                MPI_Fint *ierr);

int MPI_Init (int *argc, char ***argv);
void MPI_INIT (MPI_Fint *ierr, int argv_len);
void mpi_init (MPI_Fint *ierr, int argv_len);
void mpi_init_ (MPI_Fint *ierr, int argv_len);
void mpi_init__ (MPI_Fint *ierr, int argv_len);
int MPI_Init_thread (int *argc, char ***argv, int required, int *provided);
void MPI_INIT_THREAD (MPI_Fint *required, MPI_Fint *provided,
                      MPI_Fint *ierr, int argv_len);
void mpi_init_thread (MPI_Fint *required, MPI_Fint *provided,
                      MPI_Fint *ierr, int argv_len);
void mpi_init_thread_ (MPI_Fint *required, MPI_Fint *provided,
                       MPI_Fint *ierr, int argv_len);
void mpi_init_thread__ (MPI_Fint *required, MPI_Fint *provided,
                        MPI_Fint *ierr, int argv_len);


/*************************************************************************
 *                         Static Data                                   *
 *************************************************************************/
static int is_init_called = 0;
static int fortran_init = 0;
static int fortran_init_thread = 0;
static int thread_support = MPI_THREAD_SINGLE;


/*************************************************************************
 *                         Static Functions                              *
 *************************************************************************/

/* Potential work around based on cudbgApiAttach */
static int call_cudbgApiAttach ()
{
    void (*fptr) () = NULL;
    if ( (fptr = (void(*) ())dlsym (RTLD_DEFAULT, "cudbgApiAttach")) != NULL)
        (*fptr) ();
    return (fptr)? 0 : -1;
}

static int core_mpi_init (int *argc, char ***argv, int fortran_init)
{
    int rc = -1;

    if (fortran_init && !is_init_called) {
      is_init_called = 1;

      switch (fortran_init) {
      case 1:
          PMPI_INIT (&rc);
         break;
      case 2:
          pmpi_init (&rc);
          break;
      case 3:
          pmpi_init_ (&rc);
          break;
      case 4:
          pmpi_init__ (&rc);
          break;
      default:
          fprintf (stderr, "Error: Function binding doesn't exist");
          break;
      }
    } else {
        rc = PMPI_Init (argc, argv);
    }
  return rc;
}

static int core_mpi_init_thread (int *argc, char ***argv, int required,
                                 int *provided, int fortran_init_thread)
{
    int rc = -1;

    if (fortran_init_thread && !is_init_called) {
        is_init_called = 1;

        switch (fortran_init_thread) {
        case 1:
            PMPI_INIT_THREAD (&required, provided, &rc);
           break;
        case 2:
            pmpi_init_thread (&required, provided, &rc);
            break;
        case 3:
            pmpi_init_thread_ (&required, provided, &rc);
            break;
        case 4:
            pmpi_init_thread__ (&required, provided, &rc);
            break;
        default:
            fprintf (stderr, "Error: Function binding doesn't exist");
            break;
        }
    } else {
        rc = PMPI_Init_thread (argc, argv, required, provided);
    }
    return rc;
}

static void MPI_Init_fortran_wrapper (MPI_Fint *ierr, int argv_len)
{
    int rc = 0;
    int argc = 0;
    char **argv = NULL;

#if DEBUG
    fprintf (stdout, "F Wrapper for TV/CUDA workaround  start\n");
#endif

    // NOOP
    // rc = MPI_Init (&argc, &argv);
    // call_cudbgApiAttach ();
    *ierr = rc;

#if DEBUG
    fprintf (stdout, "F Wrapper for TV/CUDA workaround end\n");
#endif
}

static void MPI_Init_thread_fortran_wrapper (MPI_Fint *required,
                                             MPI_Fint *provided,
                                             MPI_Fint *ierr,
                                             int argv_len)
{
    int rc = 0;
    int argc = 0;
    char **argv = NULL;

#if DEBUG
    fprintf (stdout, "F Wrapper for TV/CUDA workaround  start\n");
#endif

    // NOOP
    // rc = MPI_Init_thread (&argc, &argv, *required, provided);
    // call_cudbgApiAttach ();
    *provided = thread_support;
    if (*required != *provided) {
        fprintf (stderr, "Warn: required != provided\n");
        fprintf (stderr, "Warn: Set TVCUDA_COMPAT_MPI_INIT_THREAD envVar:\n");
        fprintf (stderr, "Warn:   TVCUDA_COMPAT_MPI_INIT_THREAD=SINGLE\n");
        fprintf (stderr, "Warn:   TVCUDA_COMPAT_MPI_INIT_THREAD=FUNNELED\n");
        fprintf (stderr, "Warn:   TVCUDA_COMPAT_MPI_INIT_THREAD=SERIALIZED\n");
        fprintf (stderr, "Warn:   TVCUDA_COMPAT_MPI_INIT_THREAD=MULTIPLE\n");
    }
    *ierr = rc;

#if DEBUG
    fprintf (stdout, "F Wrapper for TV/CUDA workaround end\n");
#endif
}

static int get_required_level (const char *level)
{
    int rc = MPI_THREAD_SINGLE;

    if (level == NULL)
        return rc;

    if (strcmp (level, "SINGLE") == 0)
        rc = MPI_THREAD_SINGLE;
    else if (strcmp (level, "FUNNELED") == 0)
        rc = MPI_THREAD_FUNNELED;
    else if (strcmp (level, "SERIALIZED") == 0)
        rc = MPI_THREAD_SERIALIZED;
    else if (strcmp (level, "MULTIPLE") == 0)
        rc = MPI_THREAD_MULTIPLE;

    return rc;
}


/*************************************************************************
 *            Public: library initialization function                    *
 *************************************************************************/
int tvcuda_compat_init ()
{
    int p = 0;
    int rc = 0;
    char *thread_level = NULL;

    if (getenv ("PMIX_RANK") == NULL)
        return rc;

    thread_level = getenv ("TVCUDA_COMPAT_MPI_INIT_THREAD");
    if (thread_level == NULL) {
        rc = core_mpi_init (NULL, NULL, fortran_init);
    } else {
        int l = get_required_level (thread_level);
        thread_support = l; 
        rc = core_mpi_init_thread (NULL, NULL, l, &p, fortran_init_thread);
        if (rc != 0)
            fprintf (stderr, "Error: MPI_Init returned %d\n", rc);
        else if (l != p)
            fprintf (stderr, "Warning: required (%d) provided (%d)\n", l, p);
    }
    return rc;
}


/*************************************************************************
 *            Public: C/C++ Wrappers for MPI_Init                        *
 *************************************************************************/

int MPI_Init (int *argc, char ***argv)
{
    int rc = 0;

#if DEBUG
    fprintf (stdout, "C/C++ Wrapper for MPI_Init start\n");    
#endif

    // NOOP 
    // rc = core_mpi_init (argc, argv, fortran_init);
    // call_cudbgApiAttach ();

#if DEBUG
    fprintf (stdout, "C/C++ Wrapper for MPI_Init end\n");
#endif
    return rc;
}

int MPI_Init_thread (int *argc, char ***argv, int required, int *provided)
{
    int rc = 0;

#if DEBUG
    fprintf (stdout, "C/C++ Wrapper for MPI_Init_thread start\n");
#endif

    *provided = thread_support;

    // NOOP
    // rc = core_mpi_init_thread (argc, argv, required, provided,
    //                            fortran_init_thread);
    // call_cudbgApiAttach ();

#if DEBUG
    fprintf (stdout, "C/C++ Wrapper for MPI_Init_thread end\n");
#endif
    return rc;
}


/*************************************************************************
 *            Public: Fortran Wrappers for MPI_Init                      *
 *************************************************************************/

void MPI_INIT (MPI_Fint *ierr, int argv_len)
{
    fortran_init = 1;
    MPI_Init_fortran_wrapper (ierr, argv_len);
}

void mpi_init (MPI_Fint *ierr, int argv_len)
{
    fortran_init = 2;
    MPI_Init_fortran_wrapper (ierr, argv_len);
}

void mpi_init_ (MPI_Fint *ierr, int argv_len)
{
    fortran_init = 3;
    MPI_Init_fortran_wrapper(ierr, argv_len);
}

void mpi_init__ (MPI_Fint *ierr, int argv_len)
{
    fortran_init = 4;
    MPI_Init_fortran_wrapper(ierr, argv_len);
}

void MPI_INIT_THREAD (MPI_Fint *required, MPI_Fint *provided,
                      MPI_Fint *ierr, int argv_len)
{
    fortran_init_thread = 1;
    MPI_Init_thread_fortran_wrapper (required, provided, ierr, argv_len);
}

void mpi_init_thread (MPI_Fint *required, MPI_Fint *provided,
                      MPI_Fint *ierr, int argv_len)
{
    fortran_init_thread = 2;
    MPI_Init_thread_fortran_wrapper (required, provided, ierr, argv_len);
}

void mpi_init_thread_ (MPI_Fint *required, MPI_Fint *provided,
                       MPI_Fint *ierr, int argv_len)
{
    fortran_init_thread = 3;
    MPI_Init_thread_fortran_wrapper (required, provided, ierr, argv_len);
}

void mpi_init_thread__ (MPI_Fint *required, MPI_Fint *provided,
                        MPI_Fint *ierr, int argv_len)
{
    fortran_init_thread = 4;
    MPI_Init_thread_fortran_wrapper (required, provided, ierr, argv_len);
}

/*
 *  vi:tabstop=4 shiftwidth=4 expandtab
 */
