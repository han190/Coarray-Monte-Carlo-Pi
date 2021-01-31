module monte_carlo_m

    use iso_fortran_env, only: int64, real64
    implicit none

    integer, public, parameter :: i64 = int64, dp = real64
    public :: sequential_monte_carlo, parallel_monte_carlo
    private :: in_the_circle

contains

    pure function in_the_circle(x) result(ret)
        real(dp), intent(in) :: x(2)
        logical :: ret
        real(dp) :: radius
        real(dp), parameter :: unit = 1._dp

        radius = sqrt(x(1)**2 + x(2)**2)
        ret = .false.
        if (radius <= unit) ret = .true.
    end function in_the_circle

    function sequential_monte_carlo(trials) result(ret)
        integer(i64), intent(in) :: trials
        real(dp) :: ret, tot, x(2)
        integer(i64) :: i

        tot = 0._dp
        do i = 1_i64, trials
            call random_number(x)
            if (in_the_circle(x)) tot = tot + 1._dp
        end do
        ret = 4._dp*tot/trials
    end function sequential_monte_carlo

    function parallel_monte_carlo(trials) result(ret)
        integer(i64), intent(in) :: trials
        real(dp) :: ret, x(2)
        integer(i64) :: i
        real(dp), allocatable :: tot[:]

        allocate (tot[*])
        if (mod(trials, num_images()) /= 0_i64) stop
        tot = 0._dp
        do i = 1_i64, trials/num_images()
            call random_number(x)
            if (in_the_circle(x)) tot = tot + 1._dp
        end do
        sync all

        call co_sum(tot)
        ret = 4._dp*tot[1]/trials
    end function parallel_monte_carlo

end module monte_carlo_m

program monte_carlo_pi

    use monte_carlo_m
    implicit none

    real(dp), parameter :: pi = 3.141592653589_dp
    real(dp) :: ans_(2), clock_(2), time_(2)
    integer(i64), parameter :: scale = 10**7
    integer(i64) :: trials

    trials = scale*num_images()
    call cpu_time(clock_(1))
    ans_(1) = parallel_monte_carlo(trials)
    call cpu_time(clock_(2))
    time_(1) = clock_(2) - clock_(1)

    if (this_image() == 1) then
        call cpu_time(clock_(1))
        ans_(2) = sequential_monte_carlo(trials)
        call cpu_time(clock_(2))
        time_(2) = clock_(2) - clock_(1)
    end if

    if (this_image() == 1) then
        print "(a)", "======================="
        print "(a)", " Calculate Pi with CAF "
        print "(a)", "======================="
        print "('trials  = ', i13)", trials
        print "('cores   = ', i13)", num_images()
        print "('t_par   = ', f11.4, ' s')", time_(1)
        print "('t_seq   = ', f11.4, ' s')", time_(2)
        print "('pi_par  = ', f13.8)", ans_(1)
        print "('pi_seq  = ', f13.8)", ans_(2)
        print "('err_par = ', f12.8, '%')", percent_err(ans_(1))
        print "('err_seq = ', f12.8, '%')", percent_err(ans_(2))
    end if

contains

    real(dp) function percent_err(val)
        real(dp), intent(in) :: val

        percent_err = abs(val - pi)/pi*100._dp
    end function percent_err

end program monte_carlo_pi
