module benchmark_mod

    implicit none

    private
    public :: benchmark, benchmark_data, benchmark_create, benchmark_display, benchmark_proc

    type :: benchmark_data
        real :: time, min_time, max_time, mean_time, variance_time
        integer :: iterations
    end type benchmark_data

    type :: benchmark
       procedure(benchmark_proc), nopass, pointer :: proc => null()
       character(:), allocatable :: name
       type(benchmark_data), public :: results
       contains
            procedure, public :: run => benchmark_run
            procedure, public :: display => benchmark_display
    end type benchmark

    abstract interface
        subroutine benchmark_proc()
        end subroutine benchmark_proc
    end interface

    contains

    function benchmark_create(name, proc) result(bench)
        character(*), intent(in) :: name
        procedure(benchmark_proc), pointer, intent(in) :: proc
        type(benchmark) :: bench

        bench%name = name
        bench%proc => proc
    end function benchmark_create

    subroutine benchmark_run(this, iterations)
        class(benchmark), intent(inout) :: this
        type(benchmark_data) :: benchmark_result
        integer, optional, intent(in) :: iterations
        real :: time_start, time_end, time_error
        real, allocatable :: times(:)
        integer :: i

        if(present(iterations)) then
            benchmark_result%iterations = iterations
        else
            benchmark_result%iterations = 100
        end if


        allocate(times(benchmark_result%iterations))
        do i = 1, benchmark_result%iterations
            call cpu_time(time_start)
            call this%proc()
            call cpu_time(time_end)
            times(i) = time_end - time_start
        end do

        benchmark_result%time = sum(times)
        benchmark_result%min_time = minval(times)
        benchmark_result%max_time = maxval(times)
        benchmark_result%mean_time = sum(times) / real(benchmark_result%iterations)
        benchmark_result%variance_time = sum((times - benchmark_result%mean_time)**2) / real(benchmark_result%iterations)
        deallocate(times)

        this%results = benchmark_result
    end subroutine benchmark_run

    subroutine benchmark_display(this)
        class(benchmark), intent(in) :: this

        print *, "Benchmark: ", this%name
        print *, "Iterations: ", this%results%iterations
        print *, "Total time: ", this%results%time
        print *, "Min time: ", this%results%min_time
        print *, "Max time: ", this%results%max_time
        print *, "Mean time: ", this%results%mean_time
        print *, "Variance time: ", this%results%variance_time
    end subroutine benchmark_display

end module benchmark_mod
