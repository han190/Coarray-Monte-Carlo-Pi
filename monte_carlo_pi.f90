module monte_carlo_m

    use iso_fortran_env, only: int64, real64
    implicit none

    private
    integer, public, parameter :: i64 = int64, dp = real64
    public :: parallel_monte_carlo

contains

    pure function in_circle(x) result(ret)
        real(dp), intent(in) :: x(2)
        logical :: ret
        real(dp) :: radius

        radius = sqrt(x(1)**2 + x(2)**2)
        ret = .false.
        if (radius <= 1._dp) ret = .true.
    end function in_circle

    function parallel_monte_carlo(trials) result(ret)
        integer(i64), intent(in) :: trials
        real(dp) :: ret
        real(dp) :: x(2)
        integer(i64) :: idx, total, njob, nrem

        total = 0_i64
        njob = trials/num_images()
        do idx = (this_image() - 1)*njob + 1, this_image()*njob
            call random_number(x)
            if (in_circle(x)) total = total + 1_i64
        end do

        nrem = mod(trials, num_images())
        if (nrem /= 0 .and. this_image() <= nrem) then
            idx = num_images()*njob + this_image()
            call random_number(x)
            if (in_circle(x)) total = total + 1_i64
        end if

        sync all
        call co_sum(total, result_image=1)
        if (this_image() == 1) ret = 4._dp*total/trials
    end function parallel_monte_carlo

end module monte_carlo_m

program monte_carlo_pi

    use monte_carlo_m
    implicit none

    real(dp), parameter :: pi = 3.141592653589_dp
    real(dp) :: answer, t(2)
    integer(i64), parameter :: trials = 10**7

    call cpu_time(t(1))
    answer = parallel_monte_carlo(trials)
    call cpu_time(t(2))

    if (this_image() == 1) then
        print "(a)", "======================="
        print "(a)", " Calculate Pi with CAF "
        print "(a)", "======================="
        print "('trials  = ', i13)", trials
        print "('cores   = ', i13)", num_images()
        print "('tspan   = ', f11.4, ' s')", t(2) - t(1)
        print "('pi_cal  = ', f13.8)", answer
        print "('error   = ', f12.8, '%')", percent_err(answer)
    end if

contains

    real(dp) function percent_err(val)
        real(dp), intent(in) :: val

        percent_err = abs(val - pi)/pi*100._dp
    end function percent_err

end program monte_carlo_pi
