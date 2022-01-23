program main

    use iso_fortran_env, only: i64 => int64, dp => real64
    implicit none

    real(dp), parameter :: pi = 3.141592653589_dp
    integer(i64), parameter :: trials = 10_i64**8_i64
    real(dp), parameter :: unit = 1._dp
    real(dp) :: answer, time(2)

    call cpu_time(time(1))
    answer = monte_carlo(trials)
    call cpu_time(time(2))

    if (this_image() == 1) then
        print "(a)", repeat('-', 30)
        print "('Trials', t15, '=', i15)", trials
        print "('Cores', t15, '=', i15)", num_images()
        print "('Tspan', t15, '=', f14.4, 's')", time(2) - time(1)
        print "('Pi Computed', t15, '=', f15.8)", answer
        print "('Error', t15, '=', f14.8, '%')", percent_error(answer)
        print "(a)", repeat('-', 30)
    end if

contains

    elemental real(dp) function percent_error(val)
        real(dp), intent(in) :: val

        percent_error = abs(val - pi)/pi*100._dp
    end function percent_error

    pure logical function in_circle(x)
        real(dp), intent(in) :: x(2)

        in_circle = merge(.true., .false., sqrt(sum(x*x)) <= unit)
    end function in_circle

    real(dp) function monte_carlo(num_trials)
        integer(i64), intent(in) :: num_trials
        real(dp) :: x(2)
        integer(i64) :: i, count_

        count_ = 0_i64
        monte_carlo = 0._dp
        do i = 1_i64, num_trials, num_images()
            call random_number(x)
            if (in_circle(x)) count_ = count_ + 1
        end do

        sync all
        call co_sum(count_, result_image=1)
        if (this_image() == 1) monte_carlo = 4._dp*count_/num_trials
    end function monte_carlo

end program main
