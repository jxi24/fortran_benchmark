module test

    use benchmark_mod
    implicit none

contains
    subroutine sleep(time)
        implicit none
        real, intent(in) :: time
        real :: start_time, end_time, current_time

        call cpu_time(start_time)
        end_time = start_time + time

        do while (current_time < end_time)
            call cpu_time(current_time)
        end do
    end subroutine

subroutine test_function()
  integer :: a, b, c
  real :: sleeptime
  a = 1
  b = 2
  c = a + b

  call random_number(sleeptime)
  call sleep(sleeptime/10.0)
end subroutine
end module test

program main
use benchmark_mod
use test

implicit none

type(benchmark) :: bench
procedure(test_function), pointer :: test_function_ptr

test_function_ptr => test_function

bench = benchmark_create("test_function", test_function_ptr)
call bench%run()
call bench%display()

end program
