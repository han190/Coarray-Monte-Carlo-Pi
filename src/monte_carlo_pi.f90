program monte_carlo_pi

    use iso_fortran_env, only: i64 => int64, dp => real64
    implicit none

    real(dp), parameter :: pi = 3.141592653589_dp
    integer(i64), parameter :: trials = 10**7, unit = 1._dp
    real(dp) :: answer, time(2)

    if (this_image() == 1) call cpu_time(time(1))
    answer = monte_carlo_test(trials)
    if (this_image() == 1) call cpu_time(time(2))

    if (this_image() == 1) then
        print "(a)", " Calculate Pi with CAF "
        print "(a)", "-----------------------"
        print "('trials  = ', i13)", trials
        print "('cores   = ', i13)", num_images()
        print "('tspan   = ', f11.4, ' s')", time(2) - time(1)
        print "('pi_cal  = ', f13.8)", answer
        print "('error   = ', f12.8, '%')", percent_error(answer)
    end if

contains

    elemental real(dp) function percent_error(val)
        real(dp), intent(in) :: val

        percent_error = abs(val - pi)/pi*100._dp
    end function percent_error

    pure function in_circle(x) result(ret)
        real(dp), intent(in) :: x(2)
        logical :: ret

        if (sqrt(sum(x**2)) <= unit) then
            ret = .true.
        else
            ret = .false.
        end if
    end function in_circle

    function monte_carlo_test(num_trials) result(ret)
        integer(i64), intent(in) :: num_trials
        real(dp) :: ret
        real(dp) :: x(2)
        integer(i64) :: i, counter

        counter = 0_i64; ret = 0._dp
        do i = 1, num_trials, num_images()
            call random_number(x)
            if (in_circle(x)) counter = counter + 1
        end do

        sync all
        call co_sum(counter, result_image=1)
        if (this_image() == 1) ret = 4._dp*counter/num_trials
    end function monte_carlo_test

end program monte_carlo_pi
