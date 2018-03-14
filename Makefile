CC     := mpixlc
CFLAGS := -g -O0 -fPIC
FC     := mpixlf
FFLAGS := -g -O0
OMP_CFLAGS := $(CFLAGS) -qsmp=omp:noopt -qoffload -qfullpath -qnoinline -Xptxas -O0 -Xllvm2ptx -nvvm-compile-options=-opt=0
TEST_PROGS := virtual_ring_mpi \
	      mpihasgpu \
	      mpi_init  \
	      mpi_init_thread_serialized \
	      mpi_init_thread_funneled \
              mpi_init_thread_multiple \
              mpi_init_thread_single \
              mpi_init_f \
              mpi_init_thread_funneled_f \

all: libtvcuda_compat.so $(TEST_PROGS)

libtvcuda_compat.so: tvcuda_compat.o
	$(CC) $(CFLAGS) -shared -Wl,-init,tvcuda_compat_init  $^ -o $@

virtual_ring_mpi: virtual_ring_mpi.o
	$(CC) $(CFLAGS) $^ -o $@

mpihasgpu: mpihasgpu.o
	$(CC) $(OMP_CFLAGS) $^ -o $@

mpihasgpu.o: mpihasgpu.c
	$(CC) $(OMP_CFLAGS) $^ -c -o $@

mpi_init: mpi_init.o
	$(CC) $(OMP_CFLAGS) $^ -o $@

mpi_init_thread_funneled: mpi_init_thread_funneled.o
	$(CC) $(OMP_CFLAGS) $^ -o $@

mpi_init_thread_multiple: mpi_init_thread_multiple.o
	$(CC) $(OMP_CFLAGS) $^ -o $@

mpi_init_thread_serialized: mpi_init_thread_serialized.o
	$(CC) $(OMP_CFLAGS) $^ -o $@

mpi_init_thread_single: mpi_init_thread_single.o
	$(CC) $(OMP_CFLAGS) $^ -o $@

mpi_init_f: mpi_init_f.o
	$(FC) $(FFLAGS) $^ -o $@

mpi_init_f.o: mpi_init_f.f90
	$(FC) $(FFLAGS) $^ -c -o $@

mpi_init_thread_funneled_f: mpi_init_thread_funneled_f.o
	$(FC) $(FFLAGS) $^ -o $@

mpi_init_thread_funneled_f.o: mpi_init_thread_funneled_f.f90
	$(FC) $(FFLAGS) $^ -c -o $@

.c.o:
	$(CC) $(CFLAGS) -c $^ -o $@

.PHONY:

clean: .PHONY
	rm -f *~ *.o libtvcuda_compat.so *breakpoint* virtual_ring_mpi mpihasgpu $(TEST_PROGS)
