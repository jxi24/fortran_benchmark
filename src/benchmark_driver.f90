module benchmark_driver

    implicit none

    private
    public :: init_benchmark_driver, benchmark_driver
    public :: benchmark_runner
    public :: benchmark_selection

    type, abstract :: benchmark_runner
        contains
        procedure(benchmark_runner_set_up_suite), deferred :: set_up_suite
        procedure(benchmark_runner_tear_down_suite), deferred :: tear_down_suite
        procedure(benchmark_runner_run_benchmark), deferred :: run_benchmark
    end type benchmark_runner

    abstract interface
        subroutine benchmark_runner_set_up_suite(this, benchmark_suite, ctx)
            import :: benchmark_context, benchmark_runner, benchmark_suite_base
            class(benchmark_runner), intent(in) :: this
            class(benchmark_suite_base), pointer, intent(in) :: benchmark_suite 
            class(benchmark_context), pointer, intent(in) :: ctx
        end subroutine benchmark_runner_set_up_suite

        subroutine benchmark_runner_tear_down_suite(this, benchmark_suite, ctx)
            import :: benchmark_context, benchmark_runner, benchmark_suite_base
            class(benchmark_runner), intent(in) :: this
            class(benchmark_suite_base), pointer, intent(in) :: benchmark_suite 
            class(benchmark_context), pointer, intent(in) :: ctx
        end subroutine benchmark_runner_tear_down_suite

        subroutine benchmark_runner_run_benchmark(this, benchmark, ctx)
            import :: benchmark_context, benchmark_runner, benchmark_base
            class(benchmark_runner), intent(in) :: this
            class(benchmark_base), pointer, intent(in) :: benchmark
            class(benchmark_context), pointer, intent(in) :: ctx
        end subroutine benchmark_runner_run_benchmark
    end interface

    type :: benchmark_driver
        private
        type(driver_result), public :: driver_result

        class(benchmark_runner), allocatable :: runner
        class(context_factory), allocatable :: ctx_factory
        class(benchmark_logger), allocatable :: logger
        type(benchmark_suite), allocatable :: suites(:)
        integer :: num_suites, capacity_suites
        contains
        procedure :: register_suite => benchmark_driver_register_suite
        procedure :: run => benchmark_driver_run_suites
        procedure :: get_suite_names => benchmark_driver_get_suite_names
        procedure :: get_benchmark_names => benchmark_driver_get_benchmark_names
        procedure :: resize => benchmark_driver_resize
    end type benchmark_driver

    contains

        subroutine init_benchmark_driver(this, ctx_factory, runner, logger)
            type(benchmark_driver), intent(out) :: this
            class(context_factory), intent(in) :: ctx_factory
            class(benchmark_runner), intent(in) :: runner
            class(benchmark_logger), intent(in) :: logger

            this%runner => runner
            this%ctx_factory => ctx_factory
            this%logger => logger
            this%num_suites = 0
            this%capacity_suites = 1
            allocate(this%suites(this%capacity_suites))
        end subroutine init_benchmark_driver

        subroutine benchmark_driver_resize(this, new_size)
        class(benchmark_driver), intent(inout) :: this
            integer(c_size_t), intent(in), optional :: new_size
            integer :: i
            type(benchmark_suite), allocatable :: tmp(:)

            if(allocated(this%suites)) then
                call move_alloc(this%suites, tmp)
            endif

            if(present(new_size)) then
                this%capacity = new_size
            else
                this%capacity = 2*this%capacity
            endif

            allocate(this%suites(this%capacity))

            if(allocated(tmp)) then
                do i=1,this%size
                this%suites(i) = tmp(i)
                enddo
            end if
        end subroutine

        subroutine benchmark_driver_register_suite(this, suite)
            class(benchmark_driver), intent(inout) :: this
            type(benchmark_suite), intent(in) :: suite

            if (this%num_suites == this%capacity_suites) then
                call this%resize()
            end if
            this%num_suites = this%num_suites + 1
            this%suites(this%num_suites) = suite 
        end subroutine benchmark_driver_register_benchmarks

        subroutine benchmark_driver_run_suites(this)
            class(benchmark_driver), intent(inout) :: this
            integer :: i

            do i=1,this%num_suites
                call this%runner%set_up_suite(this%suites(i), this%ctx_factory%create_context())
                call this%suites(i)%run(this%runner)
                call this%runner%tear_down_suite(this%suites(i), this%ctx_factory%create_context())
            end do
        end subroutine benchmark_driver_run_suites

        subroutine benchmark_driver_get_suite_names(this, names)
            class(benchmark_driver), intent(in) :: this
            character(:), allocatable, intent(out) :: names(:)
            integer :: i

            allocate(names(this%num_suites))
            do i=1,this%num_suites
                names(i) = this%suites(i)%name
            end do
        end subroutine benchmark_driver_get_suite_names

        subroutine benchmark_driver_get_benchmark_names(this, suite_name, names)
            class(benchmark_driver), intent(in) :: this
            character(:), intent(in) :: suite_name
            character(:), allocatable, intent(out) :: names(:)
            integer :: i, j

            do i=1,this%num_suites
                if(this%suites(i)%name == suite_name) then
                    allocate(names(this%suites(i)%num_benchmarks))
                    do j=1,this%suites(i)%num_benchmarks
                        names(j) = this%suites(i)%benchmarks(j)%name
                    end do
                    return
                end if
            end do
        end subroutine benchmark_driver_get_benchmark_names

end module benchmark_driver
