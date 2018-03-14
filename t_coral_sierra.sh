#!/bin/sh

test_description="Test libtvcuda_compat.so"

base_dir=`dirname $0`
base_dir=`ap ${base_dir}`

. ./sharness.sh

run_job () {
    jsrun -p 4 ${base_dir}/$1
    return $?
}

test_expect_success "Test MPI_Init is shifted to load time" "
    export LD_PRELOAD=${base_dir}/libtvcuda_compat.so &&
    run_job mpi_init
    unset LD_PRELOAD
"

test_expect_success "Must fail without libtvcuda_compat.so" "
    test_expect_code 1 run_job mpi_init
"

test_expect_success "Test Fortran MPI_INIT is shifted to load time" "
    export LD_PRELOAD=${base_dir}/libtvcuda_compat.so &&
    run_job mpi_init_f
    unset LD_PRELOAD
"

test_expect_success "Must fail without libtvcuda_compat.so" "
    test_expect_code 1 run_job mpi_init_f
"

test_expect_success "Test MPI_Init_thread (MPI_THREAD_SINGLE)" "
    export LD_PRELOAD=${base_dir}/libtvcuda_compat.so &&
    export TVCUDA_COMPAT_MPI_INIT_THREAD=SINGLE &&
    run_job mpi_init_thread_single &&
    unset LD_PRELOAD &&
    unset TVCUDA_COMPAT_MPI_INIT_THREAD
"

test_expect_success "Must fail without libtvcuda_compat.so" "
    export TVCUDA_COMPAT_MPI_INIT_THREAD=SINGLE &&
    test_expect_code 1 run_job mpi_init_thread_single &&
    unset TVCUDA_COMPAT_MPI_INIT_THREAD
"

test_expect_success "Test MPI_Init_thread (MPI_THREAD_FUNNELED)" "
    export LD_PRELOAD=${base_dir}/libtvcuda_compat.so &&
    export TVCUDA_COMPAT_MPI_INIT_THREAD=FUNNELED &&
    run_job mpi_init_thread_funneled &&
    unset LD_PRELOAD &&
    unset TVCUDA_COMPAT_MPI_INIT_THREAD
"

test_expect_success "Must fail without libtvcuda_compat.so" "
    export TVCUDA_COMPAT_MPI_INIT_THREAD=FUNNELED &&
    test_expect_code 1 run_job mpi_init_thread_funneled &&
    unset TVCUDA_COMPAT_MPI_INIT_THREAD
"

test_expect_success "Test Fortran MPI_INIT_THREAD (MPI_THREAD_FUNNELED)" "
    export LD_PRELOAD=${base_dir}/libtvcuda_compat.so &&
    export TVCUDA_COMPAT_MPI_INIT_THREAD=FUNNELED &&
    run_job mpi_init_thread_funneled_f &&
    unset LD_PRELOAD &&
    unset TVCUDA_COMPAT_MPI_INIT_THREAD
"

test_expect_success "Must fail without libtvcuda_compat.so" "
    export TVCUDA_COMPAT_MPI_INIT_THREAD=FUNNELED &&
    test_expect_code 1 run_job mpi_init_thread_funneled &&
    unset TVCUDA_COMPAT_MPI_INIT_THREAD
"

test_expect_success "Test MPI_Init_thread (MPI_THREAD_SERIALIZED)" "
    export LD_PRELOAD=${base_dir}/libtvcuda_compat.so &&
    export TVCUDA_COMPAT_MPI_INIT_THREAD=SERIALIZED &&
    run_job mpi_init_thread_serialized &&
    unset LD_PRELOAD &&
    unset TVCUDA_COMPAT_MPI_INIT_THREAD
"

test_expect_success "Must fail without libtvcuda_compat.so" "
    export TVCUDA_COMPAT_MPI_INIT_THREAD=SERIALIZED &&
    test_expect_code 1 run_job mpi_init_thread_serialized &&
    unset TVCUDA_COMPAT_MPI_INIT_THREAD
"

test_expect_success "Test MPI_Init_thread (MPI_THREAD_MULTIPLE)" "
    export LD_PRELOAD=${base_dir}/libtvcuda_compat.so &&
    export TVCUDA_COMPAT_MPI_INIT_THREAD=MULTIPLE &&
    run_job mpi_init_thread_multiple &&
    unset LD_PRELOAD &&
    unset TVCUDA_COMPAT_MPI_INIT_THREAD
"

test_expect_success "Must fail without libtvcuda_compat.so" "
    export TVCUDA_COMPAT_MPI_INIT_THREAD=MULTIPLE &&
    test_expect_code 1 run_job mpi_init_thread_multiple &&
    unset TVCUDA_COMPAT_MPI_INIT_THREAD
"

test_expect_success "Must fail on thread support level mismatch" "
    export LD_PRELOAD=${base_dir}/libtvcuda_compat.so &&
    export TVCUDA_COMPAT_MPI_INIT_THREAD=FUNNELED &&
    test_expect_code 1 run_job mpi_init_thread_multiple &&
    unset LD_PRELOAD &&
    unset TVCUDA_COMPAT_MPI_INIT_THREAD
"

test_expect_success "Must fail on incorrect thread support name " "
    export LD_PRELOAD=${base_dir}/libtvcuda_compat.so &&
    export TVCUDA_COMPAT_MPI_INIT_THREAD=MULTI &&
    test_expect_code 1 run_job mpi_init_thread_multiple &&
    unset LD_PRELOAD &&
    unset TVCUDA_COMPAT_MPI_INIT_THREAD
"
test_done
