module benchmark_suite
    implicit none
    private

    public :: benchmark_suite_init
    public :: benchmark_suite_run
    public :: benchmark_suite_finalize

    integer, parameter :: log_out_unit = 6

    abstract interface
        subroutine benchmark_func() 
        end subroutine benchmark_func
    end interface

    type(benchmark_type) 
        character(256) :: name = 'A benchmark'
        type(benchmark_type), pointer :: next => null()
        integer :: iterations
        real :: time
        procedure(benchmark_func), pointer :: func => null()
    end type benchmark_type

    type(benchmark_suite_type)
        character(256) :: name = 'A benchmark suite'
        integer :: num_benchmarks = 0
        type(benchmark_type), pointer :: benchmarks_head => null()
        type(benchmark_type), pointer :: benchmarks_tail => null()
    end type benchmark_suite_type
    
    type(benchmark_suite_type), target :: default_benchmark_suite

contains
    subroutine benchmark_suite_init(name, suite)
        character(*), intent(in) :: name
        type(benchmark_suite_type), intent(in), optional, target :: suite
        type(benchmark_suite_type), pointer :: suite_ptr

        ! Initialize the benchmark suite
        if(present(suite)) then
            suite_ptr => suite
        else
            suite_ptr => default_benchmark_suite
        end if

        suite_ptr%name = name
    end subroutine benchmark_suite_init

    subroutine benchmark_suite_run(suite)
        type(benchmark_suite_type), intent(in), target :: suite
        type(benchmark_suite_type), pointer :: suite_ptr

        ! Run the benchmark suite
        if(present(suite)) then
            suite_ptr => suite
        else
            suite_ptr => default_benchmark_suite
        end if

        call write_header(log_out_unit, "Benchmark Suite: " // trim(suite_ptr%name))
        
    end subroutine benchmark_suite_run

    subroutine benchmark_suite_finalize(suite)
        type(benchmark_suite_type), intent(in), target :: suite
        type(benchmark_suite_type), pointer :: suite_ptr

        ! Finalize the benchmark suite
        if(present(suite)) then
            suite_ptr => suite
        else
            suite_ptr => default_benchmark_suite
        end if

        suite_ptr%name = ""
    end subroutine benchmark_suite_finalize
end module benchmark_suite
