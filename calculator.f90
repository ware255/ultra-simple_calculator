module m_usc
    use, intrinsic :: iso_fortran_env, only: int64, real64, real128
    implicit none
    real(real128), parameter :: pi = 3.1415926535897932384626433832795028840_real128
    real(real128), parameter :: g = 9.806650_real128
    integer(int64), parameter, private :: mal = 100000000!10 ** 8
    integer(int64) prec
    integer(int64) err
    real(real128) z
    character(len=256), private :: str
    real(real128), private :: x
    integer(int64), save :: seed(4) = [123456789, 362436069, 521288629, 88675123]

    interface operator(.plus.)    ! +
        module procedure add
    end interface

    interface operator(.minus.)   ! -
        module procedure subtract
    end interface

    interface operator(.times.)   ! *
        module procedure multiply
    end interface

    interface operator(.div.)     ! /
        module procedure divide
    end interface
contains
    subroutine Memory()
        implicit none
        print*, new_line(' '), z
        print*, new_line(' '), 'Enterを押してください。'
        read *
    end subroutine Memory

    subroutine M_A()
        implicit none
        print '(A)', '値を入力してください。'
        read (*, '(A)') str
        read (str, *, iostat=err) x
        if (err .eq. 0) then
            print*, new_line(' '), '答え'
            print '(3X, F0.36)', z + x
            write(*, '("\n", 3X, Z0)') int(z + x, 16)
            z = z + x
            print*, new_line(' '), 'Enterを押してください。'
            read *
        else
            print*, new_line(' '), char(7), 'Error!'
            read *
        end if
    end subroutine M_A
    
    subroutine M_S()
        implicit none
        print '(A)', '値を入力してください。'
        read (*, '(A)') str
        read (str, *, iostat=err) x
        if (err .eq. 0) then
            print*, new_line(' '), '答え'
            print '(3X, F0.36)', z - x
            write(*, '("\n", 3X, Z0)') int(z - x, 16)
            z = z - x
            print*, new_line(' '), 'Enterを押してください。'
            read *
        else
            print*, new_line(' '), char(7), 'Error!'
            read *
        end if
    end subroutine M_S
    
    subroutine M_M()
        implicit none
        print '(A)', '値を入力してください。'
        read (*, '(A)') str
        if (str .eq. '') then
            print '(3X, F0.36)', z * z
            write(*, '("\n", 3X, Z0)') int(z * z, 16)
            z = z * z
            print*, new_line(' '), 'Enterを押してください。'
            read *
            return
        end if
        read (str, *, iostat=err) x
        if (err .eq. 0) then
            print*, new_line(' '), '答え'
            print '(3X, F0.36)', z * x
            write(*, '("\n", 3X, Z0)') int(z * x, 16)
            z = z * x
            print*, new_line(' '), 'Enterを押してください。'
            read *
        else
            print*, new_line(' '), char(7), 'Error!'
            read *
        end if
    end subroutine M_M
    
    subroutine M_D()
        implicit none
        print '(A)', '値を入力してください。'
        read (*, '(A)') str
        read (str, *, iostat=err) x
        if (err .eq. 0) then
            print*, new_line(' '), '答え'
            print '(3X, F0.36)', z / x
            write(*, '("\n", 3X, Z0)') int(z / x, 16)
            z = z / x
            print*, new_line(' '), 'Enterを押してください。'
            read *
        else
            print*, new_line(' '), char(7), 'Error!'
            read *
        end if
    end subroutine M_D

    function add(x_, y) result(z_)
        implicit none
        integer(int64), intent(in) :: x_(0:prec), y(0:prec)
        integer(int64) z_(0:prec), i, zi, r
        r = 0
        do concurrent (i = prec: 0: -1)
            block
            zi = x_(i) + y(i) + r
            r = zi / mal
            z_(i) = zi - r * mal
            end block
        end do
    end function add

    pure function subtract(x_, y) result(z_)
        implicit none
        integer(int64), intent(in) :: x_(0:prec), y(0:prec)
        integer(int64) z_(0:prec), i, zi, r
        r = 1
        do concurrent (i = prec: 0: -1)
            block
            zi = x_(i) + (mal - 1 - y(i)) + r
            r = zi / mal
            z_(i) = zi - r * mal
            end block
        end do
    end function subtract

    function multiply(x_, s) result(z_)
        implicit none
        integer(int64), intent(in) :: x_(0:prec), s
        integer(int64) z_(0:prec), i, r, zi
        r = 0
        !$omp parallel do num_threads(4), private(i, zi)
        do i = prec, 0, -1
            zi = x_(i) * s + r
            r = zi / mal
            z_(i) = zi - r * mal
        end do
        !$omp end parallel do
    end function multiply

    pure function divide(x_, s) result(z_)
        implicit none
        integer(int64), intent(in) :: x_(0:prec), s
        integer(int64) z_(0:prec), i, r, zi
        r = 0
        do concurrent (i = 0: prec: 1)
            block
            zi = x_(i) + r * mal
            z_(i) = zi / s
            r = zi - s * z_(i)
            end block
        end do
    end function divide
end module m_usc

subroutine tasizan()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x, y
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print '(A)', '値を入力してください。'
        read (*, *, iostat=err) y
        if (err .ne. 0) then
            print*, char(7), '\nError!'
            read *
            return
        end if
        print*, '\n答え'
        if (x <= 999 .and. y <= 999) then
            print '(F9.4, "  +  ", F0.4, "\n")', x, y
        else if (x <= 99999 .and. y <= 99999) then
            print '(F19.4, "  +  ", F0.4, "\n")', x, y
        else
            print '(F29.4, "  +  ", F0.4, "\n")', x, y
        end if
        print '(3X, F0.36)', x + y
        write(*, '("\n", 3X, Z0)') int(x + y, 16)
        z = x + y
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine tasizan

subroutine hikizan()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x, y
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print '(A)', '値を入力してください。'
        read (*, *, iostat=err) y
        if (err .ne. 0) then
            print*, char(7), '\nError!'
            read *
            return
        end if
        print*, '\n答え'
        if (x <= 999 .and. y <= 999) then
            print '(F9.4, "  -  ", F0.4, "\n")', x, y
        else if (x <= 99999 .and. y <= 99999) then
            print '(F19.4, "  -  ", F0.4, "\n")', x, y
        else
            print '(F29.4, "  -  ", F0.4, "\n")', x, y
        end if
        print '(3X, F0.36)', x - y
        write(*, '("\n", 3X, Z0)') int(x - y, 16)
        z = x - y
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine hikizan

subroutine kakezan()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x, y
    character(256) str
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print '(A)', '値を入力してください。'
        read (*, '(A)') str
        if (str .eq. '') then
            print*, '\n答え'
            if (x <= 999) then
                print '(F9.4, "  ^  ", I0, "\n")', x, 2
            else if (x <= 99999) then
                print '(F19.4, "  ^  ", I0, "\n")', x, 2
            else
                print '(F29.4, "  ^  ", I0, "\n")', x, 2
            end if
            print '(3X, F0.36)', x * x
            write(*, '("\n", 3X, Z0)') int(x * x, 16)
            z = x * x
            print*, '\nEnterを押してください。'
            read *
            return
        end if
        read (str, *, iostat=err) y
        if (err .ne. 0) then
            print*, char(7), '\nError!'
            read *
            return
        end if
        print*, '\n答え'
        if (x <= 999 .and. y <= 999) then
            print '(F9.4, "  *  ", F0.4, "\n")', x, y
        else if (x <= 99999 .and. y <= 99999) then
            print '(F19.4, "  *  ", F0.4, "\n")', x, y
        else
            print '(F29.4, "  *  ", F0.4, "\n")', x, y
        end if
        print '(3X, F0.36)', x * y
        write(*, '("\n", 3X, Z0)') int(x * y, 16)
        z = x * y
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine kakezan

subroutine warizan()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x, y
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print '(A)', '値を入力してください。'
        read (*, *, iostat=err) y
        if (err .ne. 0) then
            print*, char(7), '\nError!'
            read *
            return
        end if
        print*, '\n答え'
        if (x <= 999 .and. y <= 999) then
            print '(F9.4, "  /  ", F0.4, "\n")', x, y
        else if (x <= 99999 .and. y <= 99999) then
            print '(F19.4, "  /  ", F0.4, "\n")', x, y
        else
            print '(F29.4, "  /  ", F0.4, "\n")', x, y
        end if
        print '(3X, F0.36)', x / y
        write(*, '("\n", 3X, Z0)') int(x / y, 16)
        z = x / y
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine warizan

subroutine heihoukon()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n近似値'
        print*, sqrt(x)
        z = sqrt(x)
        if (x .eq. 2.) then
            print*, '  一夜一夜に月見ごろ          <= 覚え方'
        else if (x .eq. 3.) then
            print*, '  人並みにおごれや            <= 覚え方'
        end if
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine heihoukon

subroutine ensyuritu()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128), parameter :: pi = 4.0_real128 * atan(1.0_real128)
    real(real128) r
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) r
    if (err .eq. 0) then
        print*, '\n答え'
        print '(3X, F0.36)', r * r * pi
        z = r * r * pi
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine ensyuritu

subroutine syutyou()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128), parameter :: pi = 4.0_real128 * atan(1.0_real128)
    real(real128) r
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) r
    if (err .eq. 0) then
        print*, '\n答え'
        print '(3X, F0.36)', (pi + pi) * r
        z = (pi + pi) * r
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine syutyou

subroutine nizyou()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x, y
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'べき乗する値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print '(A)', 'n乗する値を入力してください。'
        read (*, *, iostat=err) y
        if (err .ne. 0) then
            print*, char(7), '\nError!'
            read *
            return
        end if
        print*, '\n答え'
        if (x <= 999 .and. y <= 999) then
            print '(F9.4, "  ^  ", F0.4, "\n")', x, y
        else if (x <= 99999 .and. y <= 99999) then
            print '(F19.4, "  ^  ", F0.4, "\n")', x, y
        else
            print '(F29.4, "  ^  ", F0.4, "\n")', x, y
        end if
        print '(3X, F0.36)', x ** y
        write(*, '("\n", 3X, Z0)') int(x + y, 16)
        z = x ** y
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine nizyou

subroutine game_1()
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    implicit none
    character(10) d
    integer(int64) hero_hp, hero_mp, enemy1_hp, enemy1_mp, n, x
    integer(int64) :: mp = 0, y
    n = 0;x = 0
    hero_hp = 5;enemy1_hp = 10
    hero_mp = 5;enemy1_mp = 10
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print*, '\n超戦略ゲーム  ~ shit video game ~\n\n\n\n\nEnterを押してください。'
    read *
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    j:do
        if (hero_hp .le. 0) then
            print*, '\nGAME OVER'
            read *
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit j
        else if (enemy1_hp .le. 0) then
            print '(A)', '\n敵を撲殺することが出来た。ワイの勝利！！！'
            print '(A)', 'Ураааааааааааааааа!'
            read *
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit j
        end if
        print '(A)', '\n敵が現れた！'
        print *, 'HP: ', enemy1_hp
        print *, 'MP: ', enemy1_mp
        print '(A)', ''
        print '(A)', '\nワイのステータス'
        print *, 'HP: ', hero_hp
        print *, 'MP: ', hero_mp
        print '(A)', '1:攻撃, 2:魔法, 3:防御, 4:逃げる'
        write (*, '(A)', advance='no') ':'
        read (*, '(A)') d
        if (d .eq. '1') then
            n = add(5)
            select case(n)
            case (1)
                enemy1_hp = enemy1_hp - 1
                print '(A)', '\n敵のダメージ1'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ', I0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy1_hp = enemy1_hp - 2
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ2'
                print "('ワイのダメージ', I0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy1_hp = enemy1_hp - 3
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ3'
                print "('ワイのダメージ', I0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (4)
                enemy1_hp = enemy1_hp - 4
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ4'
                print "('ワイのダメージ', I0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (5)
                enemy1_hp = enemy1_hp - 5
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ5'
                print "('ワイのダメージ', I0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy1_hp = enemy1_hp - 0
                print '(A)', '\n敵のダメージ0'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ', I0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '2') then
            n = add(3)
            select case(n)
            case (1)
                enemy1_hp = enemy1_hp - 5
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ5'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ', I0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy1_hp = enemy1_hp - 6
                hero_mp = hero_mp - 1
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ6'
                print '(A)', 'MP: 1消費'
                print "('ワイのダメージ', I0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy1_hp = enemy1_hp - 7
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ7'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy1_hp = enemy1_hp - 0
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ0'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '3') then
            mp = mp + 1
            n = add(1)
            select case(n)
            case (1)
                write (*, '(A)', advance='no') '\n防御成功\n'
                write (*, '(A)', advance='no') 'HP: 1回復\n'
                k:&
                &do y = 5, 100000000, 5
                    if (mp .eq. y) then
                        print '(A)', 'MP: 1回復'
                        hero_mp = hero_mp + 1
                        exit k
                    else if (mp .lt. y) then
                        exit k
                    end if
                end do k
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                print '(A)', '\n防御失敗'
                hero_hp = hero_hp - 1
                print '(A)', 'ワイのダメージ1'
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '4') then
            n = add(1)
            select case(n)
            case (1)
                print '(A)', '\n逃げ切れた'
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
                exit j
            case default
                print '(A)', '\n逃走失敗'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else
            print '(A)', '\nそんなもんねぇよｗ'
            read *
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        end if
    end do j
contains
    integer(int32) function add(n_)
        implicit none
        integer(int32) rad, n_
        integer(int32) seedsize, c
        real(real64) y_, x_
        integer, allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        f:do
            call random_seed(get=seed)
            call system_clock(count=c)
            seed(1) = c
            call random_seed(put=seed)
            call random_number(x_)
            y_ = x_ * 100
            rad = int(y_)
            if (rad .lt. n_) exit f
        end do f
        deallocate(seed)
        add = rad
    end function
end subroutine game_1

subroutine game_2()
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    implicit none
    character(10) d
    integer(int64) hero_hp, hero_mp, enemy2_hp, enemy2_mp, n, x
    integer(int64) :: mp = 0, y
    n = 0;x = 0
    hero_hp = 5;enemy2_hp = 15
    hero_mp = 5;enemy2_mp = 15
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print *, '\n超戦略ゲーム  ~ shit video game ~\n\n\n\n\nEnterを押してください。'
    read *
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    j:do
        if (hero_hp .le. 0) then
            print *, '\nGAME OVER'
            read *
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit j
        else if (enemy2_hp .le. 0) then
            print '(A)', '\n敵を撲殺することが出来た。ワイの勝利！！！'
            print '(A)', 'Ураааааааааааааааа!'
            read *
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit j
        end if
        print*, ''
        print '(A)', '敵が現れた！'
        print *, 'HP: ', enemy2_hp
        print *, 'MP: ', enemy2_mp
        print '(A)', ''
        print '(A)', '\nワイのステータス'
        print *, 'HP: ', hero_hp
        print *, 'MP: ', hero_mp
        print '(A)', '1:攻撃, 2:魔法, 3:防御, 4:逃げる'
        write (*, '(A)', advance='no') ':'
        read (*, '(A)') d
        if (d .eq. '1') then
            n = add(5)
            select case(n)
            case (1)
                enemy2_hp = enemy2_hp - 1
                print '(A)', '\n敵のダメージ1'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy2_hp = enemy2_hp - 2
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ2'
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy2_hp = enemy2_hp - 3
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ3'
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (4)
                enemy2_hp = enemy2_hp - 4
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ4'
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (5)
                enemy2_hp = enemy2_hp - 5
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ5'
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy2_hp = enemy2_hp - 0
                print '(A)', '\n敵のダメージ0'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '2') then
            n = add(3)
            select case(n)
            case (1)
                enemy2_hp = enemy2_hp - 5
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ5'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy2_hp = enemy2_hp - 6
                hero_mp = hero_mp - 1
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ6'
                print '(A)', 'MP: 1消費'
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy2_hp = enemy2_hp - 7
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ7'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy2_hp = enemy2_hp - 0
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ0'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '3') then
            mp = mp + 1
            n = add(1)
            select case(n)
            case (1)
                print '(A)', '\n防御成功'
                print '(A)', 'HP: 1回復'
                k:do y = 5, 100000000, 5
                    if (mp .eq. y) then
                        print '(A)', 'MP: 1回復'
                        hero_mp = hero_mp + 2
                        exit k
                    else if (mp .lt. y) then
                        exit k
                    end if
                end do k
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                print '(A)', '\n防御失敗'
                hero_hp = hero_hp - 1
                print '(A)', 'ワイのダメージ1'
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '4') then
            n = add(1)
            select case(n)
            case (1)
                print '(A)', '\n逃げ切れた'
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
                exit j
            case default
                print '(A)', '\n逃走失敗'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else
            print '(A)', '\nそんなもんねぇよｗ'
            read *
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        end if
    end do j
contains
    integer(int32) function add(n_)
        implicit none
        integer(int32) rad, n_
        integer(int32) seedsize, c
        real(real64) y_, x_
        integer, allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        f:do
            call random_seed(get=seed)
            call system_clock(count=c)
            seed(1) = c
            call random_seed(put=seed)
            call random_number(x_)
            y_ = x_ * 100
            rad = int(y_)
            if (rad .lt. n_) exit f
        end do f
        deallocate(seed)
        add = rad
    end function
end subroutine game_2

subroutine game_3()
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    implicit none
    character(10) d
    integer(16) hero_hp, hero_mp, enemy3_hp, enemy3_mp, n, x
    integer(int64) :: mp = 0, y, level
    open(1, file='data/.level', status='old')
        read (1, *) level
        flush(1)
    close(1)
    select case(level)
    case (0)
        n = 0;x = 0
        hero_hp = 5;enemy3_hp = 20
        hero_mp = 5;enemy3_mp = 20
    case (1)
        n = 0;x = 0
        hero_hp = 10;enemy3_hp = 30
        hero_mp = 10;enemy3_mp = 20
    case (2)
        n = 0;x = 0
        hero_hp = 20;enemy3_hp = 40
        hero_mp = 20;enemy3_mp = 20
    case (3)
        n = 0;x = 0
        hero_hp = 30;enemy3_hp = 50
        hero_mp = 30;enemy3_mp = 20
    case (4)
        n = 0;x = 0
        hero_hp = 40;enemy3_hp = 60
        hero_mp = 40;enemy3_mp = 20
    case (5)
        n = 0;x = 0
        hero_hp = 50;enemy3_hp = 70
        hero_mp = 50;enemy3_mp = 20
    case (6)
        n = 0;x = 0
        hero_hp = 60;enemy3_hp = 80
        hero_mp = 60;enemy3_mp = 20
    case (7)
        n = 0;x = 0
        hero_hp = 70;enemy3_hp = 90
        hero_mp = 70;enemy3_mp = 20
    case (8)
        n = 0;x = 0
        hero_hp = 80;enemy3_hp = 100
        hero_mp = 75;enemy3_mp = 20
    case (9)
        n = 0;x = 0
        hero_hp = 99;enemy3_hp = 110
        hero_mp = 99;enemy3_mp = 20
    case (10)
        n = 0;x = 0
        hero_hp = 99999999999999999999_16;enemy3_hp = 999999999999999999999999999_16
        hero_mp = 99999999999999999999_16;enemy3_mp = 15
    case default
        error stop "\nError: Invalid string.\n"
    end select
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print*, '\n超戦略ゲーム  ~ shit video game ~\n\n\n\n\nEnterを押してください。'
    read *
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    j:do
        if (hero_hp .le. 0) then
            print*, '\nGAME OVER'
            read *
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit j
        else if (enemy3_hp .le. 0) then
            print '(A)', '\n敵を撲殺することが出来た。ワイの勝利！！！'
            print '(A)', 'Ураааааааааааааааа!'
            if (level .eq. 10) then
                print '(A)', '\nレベルMaxです。'
                goto 110
            end if
            open(2, file='data/.level', status='old')
                level = level + 1
                write(2, *) level
                flush(2)
            close(2)
            print '(A)', '\nレベル1上がった。'
110         read *
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit j
        end if
        print '(A)', '\n敵が現れた！'
        print *, 'HP: ', enemy3_hp
        print *, 'MP: ', enemy3_mp
        print '(A)', '\nワイのステータス'
        print *, 'HP: ', hero_hp
        print *, 'MP: ', hero_mp
        print '(A)', '1:攻撃, 2:魔法, 3:防御, 4:逃げる(q:止める)'
        write(*, '(A)', advance='no') ':'
        read (*, '(A)') d
        if (d .eq. 'q') then
            exit j
        else if (d .eq. '1') then
            n = add(5)
            select case(n)
            case (1)
                enemy3_hp = enemy3_hp - 1
                print '(A)', '\n敵のダメージ1'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy3_hp = enemy3_hp - 2
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ2'
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy3_hp = enemy3_hp - 3
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ3'
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (4)
                enemy3_hp = enemy3_hp - 4
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ4'
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (5)
                enemy3_hp = enemy3_hp - 5
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ5'
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy3_hp = enemy3_hp - 0
                print '(A)', '\n敵のダメージ0'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '2') then
            n = add(3)
            select case(n)
            case (1)
                enemy3_hp = enemy3_hp - 5
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ5'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy3_hp = enemy3_hp - 6
                hero_mp = hero_mp - 1
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ6'
                print '(A)', 'MP: 1消費'
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy3_hp = enemy3_hp - 7
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ7'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy3_hp = enemy3_hp - 0
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ0'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '3') then
            mp = mp + 1
            n = add(1)
            select case(n)
            case (1)
                print '(A)', '\n防御成功'
                print '(A)', 'HP: 1回復'
                k:do y = 5, 100000000, 5
                    if (mp .eq. y) then
                        print '(A)', 'MP: 1回復'
                        hero_mp = hero_mp + 1
                        exit k
                    else if (mp .lt. y) then
                        exit k
                    end if
                end do k
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                print '(A)', '\n防御失敗'
                hero_hp = hero_hp - 1
                print '(A)', 'ワイのダメージ1'
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '4') then
            n = add(9)
            select case(n)
            case (1)
                print '(A)', '\n逃げ切れた'
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
                exit j
            case default
                print '(A)', '\n逃走失敗'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else
            print '(A)', '\nそんなもんねぇよｗ'
            read *
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        end if
    end do j
contains
    integer(int32) function add(n_)
        implicit none
        integer(int32) rad, n_
        integer(int32) seedsize, c
        real(real64) y_, x_
        integer, allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        l:do
            call random_seed(get=seed)
            call system_clock(count=c)
            seed(1) = c
            call random_seed(put=seed)
            call random_number(x_)
            y_ = x_ * 100
            rad = int(y_)
            if (rad .lt. n_) exit l
        end do l
        deallocate(seed)
        add = rad
    end function
end subroutine game_3

subroutine game()
    use, intrinsic :: iso_fortran_env, only: int32, real64
    implicit none
    integer(int32) n
    n = add()
    open(1, file='data/.level', status='old', err=110)
    close(1)
    select case(n)
    case (1)
        call game_1()
    case (2)
        call game_2()
    case default
        call game_3()
    end select
    goto 120
110 open(2, file='data/.level', status='new')
        write(2, *) 0
        flush(2)
    close(2)
120 write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
contains
    integer(int32) function add()
        implicit none
        integer(int32) rad, c
        integer(int32) seedsize
        real(real64) y, x
        integer(int32), allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        m:do
            call random_seed(get=seed)
            call system_clock(count=c)
            seed(1) = c
            call random_seed(put=seed)
            call random_number(x)
            y = x * 10
            rad = int(y)
            if (rad .lt. 9) exit m
        end do m
        deallocate(seed)
        add = rad
    end function
end subroutine game

subroutine nizihoutei()
    use m_usc, only: err
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) a, b, c
    complex(real128) k(2), a_, b_, c_
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '一般: ax^2 + bx + c = 0 (a /= 0)\n'
    print '(A)', 'a, b, c値(係数)を入力してください。'
    read (*, *, iostat=err) a, b, c
    if (err .eq. 0) then
        a_ = a; b_ = b; c_ = c
        k(1) = (-b_ + sqrt((b_ * b_) - 4 * a_ * c_)) / (a_ + a_)
        k(2) = (-b_ - sqrt((b_ * b_) - 4 * a_ * c_)) / (a_ + a_)
        open (11, file='data/nizihoutei.txt', status='replace')
            write (11, *) k(1)
            flush(11)
            write (11, *) k(2)
            flush(11)
        close (11)
        print '(A)', '\n答え'
        print *, k(1)
        print *, k(2)
        print '(A)', '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
        return
    end if
end subroutine nizihoutei

subroutine n_sin()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print*, '\n答え'
        z = sin(n)
        print*, z
        print '(A)', '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
        return
    end if
end subroutine n_sin

subroutine n_cos()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print '(A)', '\n答え'
        z = cos(n)
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
        return
    end if
end subroutine n_cos

subroutine  n_tan()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print*, '\n答え'
        z = tan(n)
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
        return
    end if
end subroutine n_tan

subroutine n_asin()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print*, '\n答え'
        z = asin(n)
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
        return
    end if
end subroutine n_asin

subroutine n_acos()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print*, '\n答え'
        z = acos(n)
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
        return
    end if
end subroutine n_acos

subroutine n_atan()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print*, '\n答え'
        z = atan(n)
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
        return
    end if
end subroutine n_atan

subroutine n_atan2()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x, y
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'y値を入力してください。'
    read (*, *, iostat=err) y
    if (err .eq. 0) then
        print '(A)', 'x値を入力してください。'
        read (*, *, iostat=err) x
        if (err .eq. 0) then
            print*, '\n答え'
            z = atan2(y, x)
            print*, z
            print*, '\nEnterを押してください。'
            read *
        else
            print*, char(7), '\nError!'
            read *
            return
        end if
    else
        print*, char(7), '\nError!'
        read *
        return
    end if
end subroutine n_atan2

subroutine n_aimag()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    complex(real128) im
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '入力例: (2.71, 0.99)\n※()も入力してください。\n'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) im
    if (err .eq. 0) then
        print*, '\n答え'
        z = aimag(im)
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
        return
    end if
end subroutine n_aimag

subroutine n_log10()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print*, '\n答え'
        z = log10(n)
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
        return
    end if
end subroutine n_log10

subroutine n_log()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print*, '\n答え'
        z = log(n)
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
        return
    end if
end subroutine n_log

subroutine mozuro
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) a, n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値aを入力してください。'
    read (*, *, iostat=err) a
    if (err .eq. 0) then
        print '(A)', '値nを入力してください。'
        read (*, *, iostat=err) n
        if (err .eq. 0) then
            print*, '\n答え'
            z = mod(a, n)
            print*, z
            print*, '\nEnterを押してください。'
            read *
        else
            print*, char(7), '\nError!'
            read *
            return
        end if
    else
        print*, char(7), '\nError!'
        read *
        return
    end if
end subroutine mozuro

subroutine randsu()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: int32, int64, real128
    implicit none
    integer(int64) n, x, seed
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'xを入力してください。(1～x)'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        if (x .le. 0) then
            print '(A)', '\n1以上にしてください。\n'
            print*, '\nEnterを押してください。'
            read *
            return
        end if
        seed = time()
        call init_xor128(seed)
        n = mod(xor128(), x + 1)
        print*, '\n出力'
        print*, n
        write (*, '("\n", 3X, Z0)') int(n, 16)
        z = n
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
contains
    subroutine init_xor128(s)
        use m_usc, only: seed
        implicit none
        integer(int64), intent(inout) :: s
        integer i
        do i = 1, 4
            seed(i) = 1812433253 * xor(s, rshift(s, 30)) + i
            s = 1812433253 * xor(s, rshift(s, 30)) + i
        end do
    end subroutine

    integer(int64) function xor128()
        use m_usc, only: seed
        implicit none
        integer(int64) a(4)
        integer(int64) t, i
        do i = 1, 4
            a(i) = seed(i)
        end do
        t = xor(a(1), lshift(a(1), 11))
        a(1) = a(2)
        a(2) = a(3)
        a(3) = a(4)
        a(3) = xor(xor(a(3), rshift(a(3), 19)), xor(t, rshift(t, 8)))
        xor128 = a(3)
    end function xor128
end subroutine randsu

subroutine neipia() ! e = lim n->Infinity (1+1/n)**n | Σn=0 ∞ 1/n!
    use m_usc, only: z
    use, intrinsic :: iso_fortran_env, only: int64, real128
    implicit none
    integer(int64), parameter :: n = 1024
    integer(int64) a
    real(real128) :: b = 1.0_real128, e = 1.0_real128
    print '(A)', '\x1b[2J\x1b[3J\x1b[H'
    do a = 1, n
        b = b * a
        e = e + 1 / b
    end do
    print*, 'ネイピア数(総和は1から1024まで)'
    print '(2F40.36)', e
    z = e
    print*, '\n Wikipediaでは以下(上の桁数に合わせた)'
    print '(2X, A)', '2.718281828459045235360287471352662497'
    print*, '\nEnterを押してください。'
    read *
end subroutine neipia

subroutine y_zyoukon()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x, y
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'n乗根のnの値を入力してください。()'
    read (*, *, iostat=err) y
    if (err .eq. 0) then
        if (y .eq. 1) then
            print '(A)', '\n1は平方根や立方根にならねぇよ！'
            print*, '\nEnterを押してください。'
            read *
            return
        else if (y .eq. 2) then
            print '(A)', '\n2だと平方根になるのでpage_00に移動して&
            &5を押して下さい。'
            print*, '\nEnterを押してください。'
            read *
            return
        end if
        print '(A)', 'n乗根するx値を入力してください。'
        read (*, *, iostat=err) x
        if (err .ne. 0) then
            print*, char(7), '\nError!'
            read *
            return
        end if
        print*, '答え'
        z = x ** (1 / y)
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine y_zyoukon

subroutine zettaiti()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        z = abs(x)
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine zettaiti

subroutine sisu()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        z = exp(x)
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine sisu

subroutine soukyokusin()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        z = sinh(x)
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine soukyokusin

subroutine soukyokucos()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        z = cosh(x)
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine soukyokucos

subroutine soukyokutan()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        z = tanh(x)
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine soukyokutan

subroutine gamma_f()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        z = gamma(x)
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine gamma_f

subroutine joke()
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    implicit none
    integer(int64) x
    x = randon()
    select case(x)
    case (0, 9)
        print*, '\n  women = time * money\n'
        print '(A)', 'Women are the product of time and money.'
        read *
    case (1, 5)
        print*, '\n  time = money\n'
        print '(A)', 'Time is money.'
        read *
    case (2, 6)
        print*, '\n  women = money^2\n'
        print '(A)', 'So women are money squared.'
        read *
    case (3, 7)
        print*, '\n  money = √evil\n'
        print '(A)', 'Money is the root of all evil.'
        read *
    case (4, 8)
        print*, '\n  women = (√evil)^2 = evil\n'
        print '(A)', 'So women are evil.'
        read *
    case default
        error stop '\nError: There''s an anomaly in variable x\n'
    end select
contains
    integer(int32) function randon()
        implicit none
        integer(int32) rad, c
        integer(int32) seedsize
        real(real64) y, x_
        integer, allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        do
            call random_seed(get=seed)
            call system_clock(count=c)
            seed(1) = c
            call random_seed(put=seed)
            call random_number(x_)
            y = x_ * 10
            rad = int(y)
            if (rad .lt. 5) exit
        end do
        deallocate(seed)
        randon = rad
    end function
end subroutine joke

subroutine undouhouteisiki()
    !$ use omp_lib
    use m_usc, only: err
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    real(real128) V, angle
    !$ double precision st, en
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '初期速度 [m/s]'
    read (*, *, iostat=err) V
    if (err .eq. 0) then
        print '(A)', '仰角(0≦θ≦90) [deg]'
        read (*, *, iostat=err) angle
        if (err .ne. 0) then
            print*, '\nError!'
            read *
            return
        else if (0 > angle .and. angle > 90) then
            print*, '変域にしたがってね。'
            read *
            return
        end if
        print '(A)', '\n計算中'
        !$ st = omp_get_wtime()
        block
            use m_usc, only: pi, g
            real(real128) dxdt, dydt, zero, theta, u, w, x, y
            theta = pi / 180.0_real128 * angle
            zero = 0.0_real128
            x = zero
            y = zero
            u = V * cos(theta)
            w = V * sin(theta)
            open(11, file='data/output.txt', status='replace')
            write(11, '("\t", F0.23, "\t", F0.23)') x, y
            zyu:do
                dxdt = u
                dydt = w
                x = x + 0.00050_real128 * dxdt
                y = y + 0.00050_real128 * dydt
                u = u + 0.00050_real128 * zero
                w = w + 0.00050_real128 * (-g)
                write(11, '("\t", F0.23, "\t", F0.23)') x, y
                if (y < 0) exit zyu
            end do zyu
            close(11)
        end block
        !$ en = omp_get_wtime()
        !$ print *, "Elapsed time :", en - st
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine undouhouteisiki

subroutine ziyurakka()
    !$ use omp_lib
    use m_usc, only: err, g
    use, intrinsic :: iso_fortran_env, only: int64, real128
    implicit none
    real(real128) t, y, v0
    integer(int64) i
    !$ double precision st, en
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '初期速度 [m/s]'
    read (*, *, iostat=err) v0
    if (err .eq. 0) then
        print '(A)', '\n計算中'
        !$ st = omp_get_wtime()
        t = 0
        open(11, file='data/ziyurakka.txt', status='replace')
        y = (v0 * t) + ((g * t * t) * 0.50_real128)
        write(11, '("\t", F0.23, "\t", F0.23)') t, y
        do concurrent (i = 1: 600000_int64: 2)
            block
            t = t + 0.00020_real128
            y = (v0 * t) + ((g * t * t) * 0.50_real128)
            write(11, '("\t", F0.23, "\t", F0.23)') t, y
            end block
        end do
        close(11)
        !$ en = omp_get_wtime()
        !$ print *, "Elapsed time :", en - st
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine ziyurakka

subroutine TX()
    use m_usc, only: err, pi, g
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) V, angle, theta, T, L, H, d, V2, H_T
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '初期速度 [m/s]'
    read (*, *, iostat=err) V
    if (err .eq. 0) then
        print '(A)', '仰角(0≦θ≦90) [deg]'
        read (*, *, iostat=err) angle
        if (err .ne. 0) then
            print*, '\nError!'
            read *
            return
        else if (0 > angle .and. angle > 90) then
            print*, '変域にしたがってね。'
            read *
            return
        end if
        theta = pi / 180.0_real128 * angle
        d = sin(theta)
        V2 = V * V

        T = (2.0_real128 * V * d) / g
        L = (V2 * sin(2.0_real128 * theta)) / g
        H = (V2 * (d * d)) / 19.61330_real128
        H_T = (V * d) / g

        print*, '\n滞空時間'
        print '(3X, F0.36, " [sec]")', T
        print*, '\n飛距離'
        print '(3X, F0.36, " [m]")', L
        print*, '\n最高高度時の時間'
        print '(3X, F0.36, " [sec]")', H_T
        print*, '\n最高高度'
        print '(3X, F0.36, " [m]")', H
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine TX

subroutine ensyu()
    !$ use omp_lib
    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none
    integer(int64), parameter :: vmax = 428800, bmax = 25728
    integer(int64), allocatable :: vect(:), buffer(:)
    integer(int64) n, L, more, num, carry, k, d
    !$ double precision st, en
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '\n計算中です。\n終わったあとは、メモ帳を&
    &大画面にした方がええで\n'
    !$ st = omp_get_wtime()
    allocate(vect(vmax), buffer(bmax))
    vect(1:vmax) = 2
    more = 0
    do concurrent (n = 1: bmax: 4)
        block
        carry = 0
        do concurrent (L = vmax: 1: -1)
            block
            num = 100000 * vect(L) + f(carry, L)
            d = ((L + L) - 1)
            carry = num / d
            vect(L) = num - (carry * d)
            end block
        end do
        k = carry / 100000
        buffer(n) = more + k
        more = carry - f(100000_int64, k)

        carry = 0
        do concurrent (L = vmax: 1: -1)
            block
            num = (100000 * vect(L)) + f(carry, L)
            d = ((L + L) - 1)
            carry = num / d
            vect(L) = num - (carry * d)
            end block
        end do
        k = carry / 100000
        buffer(n+1) = more + k
        more = carry - f(100000_int64, k)

        carry = 0
        do concurrent (L = vmax: 1: -1)
            block
            num = (100000 * vect(L)) + f(carry, L)
            d = ((L + L) - 1)
            carry = num / d
            vect(L) = num - (carry * d)
            end block
        end do
        k = carry / 100000
        buffer(n+2) = more + k
        more = carry - f(100000_int64, k)

        carry = 0
        do concurrent (L = vmax: 1: -1)
            block
            num = (100000 * vect(L)) + f(carry, L)
            d = ((L + L) - 1)
            carry = num / d
            vect(L) = num - (carry * d)
            end block
        end do
        k = carry / 100000
        buffer(n+3) = more + k
        more = carry - f(100000_int64, k)
        end block
    end do
    !$ en = omp_get_wtime()
    open(11, file='data/pi.txt', status='replace')
        write(11, "(1X, I1, '.'/(1X, 32I5.5))") buffer
        flush(11)
    close(11)
    write(*, "(1X, I1, '.'/(1X, 12I5.5))") buffer
    !$ print *, "Elapsed time :", en - st
    deallocate(vect, buffer)
    print*, '\nEnterを押してください。'
    read *
contains
    pure integer(int64) function f(x, y) result(z)
        implicit none
        integer(int64), intent(in) :: x, y
        integer(int64) i
        z = 0
        do concurrent (i = 1: x)
            block
            z = z + y
            end block
        end do
    end function f
end subroutine ensyu

subroutine heikin()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    integer(16) i, max
    real(real128), allocatable :: x(:)
    real(real128) y
    character(37) char
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '観測値を入力してください。'
    read (*, *, iostat=err) max
    if (err .eq. 0) then
        print*, ''
        allocate(x(max))
        y = 0.0_real128
        do i = 1, max
            print '(I0, "つ目の値を入力してください。(止めるときはqを入力)")', i
            read (*, '(A)') char
            if (trim(char) .eq. 'q') then
                deallocate(x)
                return
            end if
            read (char, *, iostat=err) x(i)
            if (err .ne. 0) then
                deallocate(x)
                print*, '\nError!'
                read *
                return
            end if
            y = y + x(i)
        end do
        deallocate(x)
        print*, '\n答え'
        print*, y / max
        z = y / max
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine heikin

subroutine kaizyou()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    integer(16) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        if (n >= 38 .or. n <= 0) then
            print*, char(7), '\nError!'
            read *
            return
        end if
        z = f(n)
        print*, '\n答え'
        print '(3X, I0, "! = ", F0.0)', n, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
contains
    pure real(real128) function f(x) result(y)
        implicit none
        integer(16), intent(in) :: x
        integer(16) i
        y = 1
        do concurrent (i = 1: x)
            block
            y = y * i
            end block
        end do
    end function f
end subroutine kaizyou

subroutine zetaf()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: int64, real64
    implicit none
    real(real64) zeta, s
    integer(int64) i
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) s
    if (err .eq. 0) then
        zeta = 0.0_real64
        !$omp parallel do num_threads(4), reduction(+:zeta), private(i), shared(s)
        do i = 41943020_int64, 1, -1
            zeta = zeta + (1.0_real64 / i ** s)
        end do
        !$omp end parallel do
        print*, '\n答え(精度悪いですm(--)m)\n'
        print*, zeta
        z = zeta
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine zetaf

subroutine collatz()
    use m_usc, only: err
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128), parameter :: q = 2.0_real128
    real(real128) n, h
    integer(16) i
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        i = 0
        print*, ''
        l:do
            h = mod(n, q)
            if (n .eq. 1) exit l
            select case(int(h, 16))
            case (1)
                n = (n + n + n) + 1
            case (0)
                n = n * 0.50_real128
            end select
            write (*, '(I0, ", ")', advance='no') int(n, 16)
            i = i + 1
        end do l
        print '("\n\n", I0, " 回の操作で答えが ", I0)', i, int(n)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine collatz

subroutine soinsubunkai()
    use m_usc, only: err
    use, intrinsic :: iso_fortran_env, only: real64, real128
    implicit none
    integer(16) n, c, i, j, t(4)
    t = [2, 3, 5, 7]
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print*, '\n答え'
        j = 0
        if (n < 2) then
            write (*, '("\t", A)', advance='no') 'Not a prime number.'
            goto 110
        end if
        write (*, '(3X, I0, A)', advance='no') n,' = 1'
        l:do i = 1, 4
            z:do while (modulo(n, t(i)) .eq. 0)
                write (*, '(" * ", I0)', advance='no') t(i)
                n = n / t(i)
            end do z
        end do l
        c = int(sqrt(real(n, real128)), 16)
        m:do i = 11, c, 2
            if (modulo(i, 3) .eq. 0 .or. modulo(i, 5) .eq. 0 .or. modulo(i, 7) .eq. 0) cycle
            h:do while (modulo(n, i) .eq. 0)
                write (*, '(" * ", I0)', advance='no') i
                n = n / i
            end do h
        end do m
        if (n .ne. 1) then
            write (*, '(" * ", I0)', advance='no') n
        end if
110     print*, '\n\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine soinsubunkai

subroutine sosuhantei()
    use m_usc, only: err
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    integer(16) p, i
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) p
    if (err .eq. 0) then
        select case(p)
        case (0, 1)
            print*, '\n答え'
            print '("\t", I0, "は素数ではありません。")', p
            print*, '\nEnterを押してください。'
            read *
            return
        case (2, 3)
            print*, '\n答え'
            print '("\t", I0, "は素数です。")', p
            print*, '\nEnterを押してください。'
            read *
            return
        end select
        if (mod(p, 2) .eq. 0 .or. mod(p, 3) .eq. 0) then
            print*, '\n答え'
            print '("\t", I0, "は素数ではありません。")', p
            print*, '\nEnterを押してください。'
            read *
            return
        end if
        i = 5
        do while (f(i, i) <= p)
            if (mod(p, i) .eq. 0 .or. mod(p, (i + 2)) .eq. 0) then
                print*, '\n答え'
                print '("\t", I0, "は素数ではありません。")', p
                print*, '\nEnterを押してください。'
                read *
                return
            end if
            i = i + 6
        end do
        print*, '\n答え'
        print '("\t", I0, "は素数です。")', p
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
contains
    integer(16) function f(x, y) result(z)
        implicit none
        integer(16), intent(in) :: x, y
        integer(16) j
        z = 0
        !$omp parallel do num_threads(4), reduction(+:z), shared(y)
        do j = 1, x
            z = z + y
        end do
        !$omp end parallel do
    end function f
end subroutine sosuhantei

subroutine slot()
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    implicit none
    character char
    integer(int64) i, j, x, a, b, c, k, L, w_
    i = 0; a = 0; b = 0; c = 0; k = 0; L = 0
    w_ = time()
11  loop :&
    &do j = 0, 3
        write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        print '(A)', '┌─────────────────┐'
        print '(A, I0, A, I0, A, I0, A)', '｜  ', a, '  |  ', b, '  |  ', c, ' ｜'
        print '(A)', '└─────────────────┘'
        print '(A)', '\nEnterを押してください。'
        read (*, '(A)') char
        x = randon(w_)
        select case (j)
        case (0)
            a = x
        case (1)
            b = x
        case (2)
            c = x
        end select
        if (j .eq. 3) then
            hantei :&
            &if (a .eq. b .and. b .eq. c) then
                print '(A)', '\n当たりｷﾀ━━━━(ﾟ∀ﾟ)━━━━!!'
                i = i - 2
                k = k + 255
                read (*, '(A)') char
            else if (a .eq. 7 .and. b .eq. 7 .and. c .eq. 7) then
                print '(A)', '\n超極レアスーパーナンバーｷﾀ━━━━(ﾟ∀ﾟ)━━━━!!'
                print '(A)', '当たる超確率up'!(大噓)
                i = i - 3
                k = k + 777
                read (*, '(A)') char
            else if (a .eq. b .or. a .eq. c .or. b .eq. c) then
                print '(A)', '\nリーチ(＞ω＜)/'
                i = i - 1
                k = k + 127
                read (*, '(A)') char
            else
                print '(A)', '\nおしい!'
                i = i + 1
                k = k + 63
                if (i .eq. 5) then
                    print '("\nゲームオーバー\t記録:", I0, "回 ", I0, "pints")', L, k
                    call sleep(2)
                    read (*, '(A)') char
                    exit loop
                end if
                read (*, '(A)') char
            end if hantei
            goto 11
        !else if (mod(, 3) .eq. 0)
        else if (char .eq. 'q') then
            exit loop
        end if
        L = L + 1
    end do loop
contains
    integer(int64) function randon(w)
        implicit none
        integer(int64), intent(inout) :: w
        integer(int64), save :: x_=123456789, y=362436069, z=521288629
        integer(int64) t
        t = xor(x_, lshift(x_, 11))
        x_ = y; y = z; z = w;
        w = xor(xor(w, rshift(w, 19)), xor(t, rshift(t, 8)))
        randon = mod(w, 10)
    end function
end subroutine slot

subroutine kanzensu()
    use m_usc, only: err
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    integer(16) x
    real(real128) p, n, i, j
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'xを入力してください。(0 < x < 1025)'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        if (0 .ge. x .or. 1025 .le. x) then
            print*, char(7), '変域にしたがってください。'
            read *
            return
        end if
        i = 2.0_real128
        j = 0.0_real128
        print*, ''
        l:do
            if (j .eq. x) exit l
            n = (pow(2.0_real128, int(i, 16))) - 1.0_real128
            if (is_prime(int(n, 16))) then
                p = pow(2.0_real128, int(i - 1.0_real128, 16)) * n
                j = j + 1.0_real128
                write (*, '(F0.0, 1X)', advance='no') p
            end if
            i = i + 1.0_real128
        end do l
        print*, '\n\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
contains
    pure logical(int64) function is_prime(n_)
        implicit none
        integer(16), intent(in) :: n_
        integer(16) i_
        i_ = 3
        select case(n_)
        case (0, 1)
            is_prime = .false.
        case (2, 3)
            is_prime = .true.
        end select
        if (mod(n_, 2) .eq. 0 .or. mod(n_, 3) .eq. 0) is_prime = .false.
        i_ = 5
        do while((i_ * i_) .le. n_)
            if (mod(n_, i_) .eq. 0 .or. mod(n_, (i_ + 2)) .eq. 0) is_prime = .false.
            i_ = i_ + 6
        end do
        is_prime = .true.
    end function

    real(real128) function pow(a, b)
        implicit none
        real(real128), intent(in) :: a
        integer(16), intent(in) :: b
        real(real128) k
        integer(16) i_
        k = 1
        !$omp parallel do num_threads(4), reduction(*:k), shared(a)
        do i_ = 1, b
            k = k * a
        end do
        !$omp end parallel do
        pow = k
    end function pow
end subroutine kanzensu

subroutine akkaman
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    integer(16) i
    real(real128) m, n, t, ans
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'm, nの値を入力してください。'
    read (*, *, iostat=err) m, n
    if (err .eq. 0) then
        y:select case (int(m, 16))
        case (0)
            ans = n + 1
        case (1)
            ans = n + 2
        case (2)
            ans = n + n + 3
        case default
            t = 2 ** (m - 2)
            i = 2
            do i = 2, int(n + 3, 16)
                t = t * 2 ** (m - 2)
            end do
            ans = t - 3
        end select y
        print*, '\n答え'
        print '(3X, F0.0)', ans
        z = ans
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine akkaman

subroutine sigmoid()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    real(real128) a, x
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'aとxの値を入力してください。'
    read (*, *, iostat=err) a, x
    if (err .eq. 0) then
        if (a .lt. 0) then
            print*, 'gainの値を大きくしてください。'
            read *
            return
        end if
        print*, '\n答え'
        z = (tanh(a * x * 0.50_real128) + 1) * 0.50_real128
        print*, z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine sigmoid

subroutine furie()
    use m_usc, only: err, pi
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    character(256) filename
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'ファイル名を入力してください。(止める:q)'
    read (*, '(A)') filename
    if (trim(filename) .eq. 'q') return
    open(1, file=trim(filename), status='old', err=404)
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '\n計算中'
    open(2, file='data/furie_im.txt', status='replace')
    open(3, file='data/furie_re.txt', status='replace')
    open(4, file='data/furie_am.txt', status='replace')
    read (1, '()')
    block
        integer(int64), parameter :: max = 1073741824
        integer(int64) i, j, k, n
        real(real128), allocatable :: f(:)
        real(real128) ReF, ImF, dummy, pi2
        allocate(f(max))
        pi2 = pi + pi
        n = 0
        st:do k = 1, max
            read (1, *, iostat=err) f(k)
            if (err .lt. 0) exit st
            n = n + 1
        end do st
        do concurrent (j = 1: n)
            block
            ReF = 0.0_real128
            ImF = 0.0_real128
            do concurrent (i = 1: n)
                block
                dummy = pi2 * i * j / n
                ReF = ReF + ( f(i) * cos(dummy))
                ImF = ImF + (-f(i) * sin(dummy))
                end block
            end do
            write(2, '("\t", I0, "\t", F0.23)') j, ImF
            write(3, '("\t", I0, "\t", F0.23)') j, ReF
            write(4, '("\t", I0, "\t", F0.23)') j, ((ReF * ReF + ImF * ImF) *&
            &* 0.50_real128) / (n * 0.50_real128)
            end block
        end do
        deallocate(f)
    end block
    close(4)
    close(3)
    close(2)
    close(1)
    print*, '\nEnterを押してください。'
    read *
    return
404 print*, '\nそのようなファイルは、ありません(でした(´;ω;｀))'
    read *
end subroutine furie

subroutine tan_h()
    use m_usc, only: err, z, pi
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    real(real128) theta, x, angle
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '距離(隣辺)を入力してください。[m]'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print '(A)', '仰角(0 < θ < 90) [deg]'
        read (*, *, iostat=err) angle
        if (err .ne. 0) then
            print*, char(7), '\nError!'
            read *
            return
        else if (0 .ge. angle .or. angle .ge. 90) then
            print*, '変域にしたがってね。'
            read *
            return
        end if
        theta = pi / 180.0_real128 * angle
        z = x * tan(theta)
        print*, '\n答え'
        print '(3X, "h = ", F0.36)', z
        print*, '\nEnterを押してください。'
        read *
    else
        print*, char(7), '\nError!'
        read *
    end if
end subroutine tan_h

subroutine lifegame()
    use m_usc, only: err
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64
    implicit none
    integer(int64), parameter :: gridsize = 15
    integer(int64), allocatable :: field(:, :)
    integer(int64), allocatable :: neighbors(:, :)
    integer(int64) max, i

    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'n世代まで(数値を入力)'
    read (*, *, iostat=err) max
    print *, 'お待ちください。'

    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*, '(A)', advance='no') '---------------------------\n'
    call init_field(0.50_8)
    call print_field()
    write (*, '(A)', advance='no') '---------------------------'
    call sleep(1)

    do i = 2, max
        write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        write (*, '(A)', advance='no') '---------------------------\n'
        call one_epoch()
        call print_field()
        write (*, '(A)', advance='no') '---------------------------'
        call sleep(1)
    end do
    print '(A)', '\nThe End.'
    read *
contains
    subroutine init_field(start_ratio)
        real(real64), intent(in) :: start_ratio
        real(real64), allocatable :: s(:, :)
        integer(int32), allocatable :: seed(:)
        integer(int32) c, sz
        allocate(field(gridsize, gridsize+10))
        allocate(s(gridsize, gridsize+10))
        allocate(neighbors(gridsize, gridsize+10))
        call random_seed(size=sz)
        allocate(seed(sz))
        call random_seed(get=seed)
        call system_clock(count=c)
        seed(1) = c
        call random_seed(put=seed)
        call random_number(s)
        field = int(s + start_ratio)
    end subroutine init_field

    subroutine print_field()
        implicit none
        integer(int64) j, k, l
        do l = 1, gridsize
            do j = 1, gridsize+10
                k = field(l, j)
                if (k .ne. 0)  then
                    write (*, '(A)', advance='no') '*'
                else
                    write (*, '(A)', advance='no') ' '
                end if
            end do
            print *, '|'
        end do
    end subroutine print_field

    subroutine one_epoch()
        implicit none
        integer(int64) dx, dy
        
        neighbors = 0
        do concurrent (dx = -1: 1, dy = -1: 1)
            neighbors = neighbors + cshift(cshift(field, dx, 1), dy, 2)
        end do
        
        where (neighbors .eq. 3)
            field = 1
        else where (neighbors .eq. 4)
            field = field
        else where
            field = 0
        endwhere
    end subroutine one_epoch
end subroutine lifegame

subroutine lumi_distance()
    use m_usc, only: err, z
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    real(real128) epsilon, a, err_, x, h
    real(real128) trapezoid, midpoint, simpson
    real(real128) new_simpson
    integer(int64) i, n

    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '赤方偏移xを入力してください。(0 < x < 1000)'
    read (*, *, iostat=err) x
    if (err .ne. 0) then
        print*, char(7), '\nError!'
        read *
        return
    else if (x <= 0.0_real128 .or. x >= 1000.0_real128) then
        print '(A)', '\nちゃんと0 < x < 1000にしてください。'
        read *
        return
    end if

    a = 0.0_real128
    simpson = 0.0_real128
    err_ = 1.0_real128
    epsilon = 0.00000010_real128
    h = x - a
    trapezoid = h * (f(a) + f(x)) * 0.50_real128
    n = 1
    
    do while (err_ > epsilon)
        midpoint = 0.0_real128
        do concurrent (i = 1: n)
            midpoint = midpoint + f(a + h * (i - 0.50_real128))
        end do
        midpoint = midpoint * h
        new_simpson = (trapezoid + (midpoint + midpoint)) / 3
        err_ = abs(new_simpson - simpson) / abs(new_simpson)
        simpson = new_simpson
        h = h * 0.50_real128
        trapezoid = (trapezoid + midpoint) * 0.50_real128
        n = n + n
    end do

    z = (1 + x) * simpson
    print*, '\n答え'
    print '("\t", F0.36, " [Mpc]")', z
    print*, '\nEnterを押してください。'
    read *
contains
    pure real(real128) function f(x_) !関数
        implicit none
        real(real128), parameter :: C = 299792.4580_real128
        real(real128), parameter :: OMEGA_M = 0.30_real128
        real(real128), parameter :: OMEGA_L = 0.70_real128
        real(real128), parameter :: H_0 = 70.0_real128
        real(real128), intent(in) :: x_
        f = C / H_0 / sqrt(OMEGA_M * ((1.0_real128 + x_) ** 3_int64) + OMEGA_L)
    end function
end subroutine lumi_distance

subroutine pi_()
    !$ use omp_lib
    use, intrinsic :: iso_fortran_env, only: int64, real64
    use m_usc, only: operator(.minus.), operator(.times.), operator(.div.)&
    &, operator(.plus.), prec, err
    implicit none
    integer(int64) n_
    integer(int64), allocatable :: pi(:)
    !$ real(real64) :: time_begin_s, time_end_s
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'n桁まで表示(50 < n < 1000000)'
    read (*, *, iostat=err) n_
    if (err .ne. 0) then
        print*, char(7), '\nError!'
        read *
        return
    else if (n_ < 50) then
        print*, char(7), '\nError!'
        read *
        return
    end if
    print '(A)', '\n計算中'

    !$ time_begin_s = omp_get_wtime()
    prec = ceiling(n_ * 0.1250_real64) + 1
    allocate(pi(0:prec))
    pi = (Arctan(49_int64) .times. 48_int64) .&
        &plus. (Arctan(57_int64) .times. 128_int64) .&
        &minus. (Arctan(239_int64) .times. 20_int64) .&
        &plus. (Arctan(110443_int64) .times. 48_int64)
    !$ time_end_s = omp_get_wtime()
    
    open (13, file='data/pi_.txt', status='replace')
    write (13, '(" Pi = 3.", I8.8, 4I9.8/(7X, 5I9.8))') pi(1:prec - 1)
    close (13)
    print '(" Pi = 3.", I8.8, 4I9.8/(7X, 5I9.8))', pi(1:prec - 1)
    deallocate(pi)
    !$ print '(A, F13.5, A)', '\ntime:', time_end_s - time_begin_s, ' [sec]\n'
    print '(A)', 'Enterを押してください。'
    read *
contains
    function Arctan(k) result(x)
        implicit none
        integer(int64), intent(in) :: k
        integer(int64) x(0:prec), unity(0:prec), n, t
        unity = [1, (0, n = 1, prec)]
        x = 0

        select case (k) !t = k * k
        case (49_int64)
            t = 2401_int64
        case (57_int64)
            t = 3249_int64
        case (239_int64)
            t = 57121_int64
        case (110443_int64)
            t = 12197656249_int64
        end select

        do concurrent (n = int(0.50_real64 * n_ / log10(real(k, real64))) + 1: 1: -1)
            block
            x = ((unity .div. (n + n + 1)) .minus. x) .div. t
            end block
        end do
        x = (unity .minus. x) .div. k
    end function Arctan
end subroutine pi_

subroutine fibonattisuretu()
    use m_usc, only: err
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    integer(16) r, x, y, z, i
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'xを入力してください。(0 < x < 185)'
    read (*, *, iostat=err) x
    if (err .ne. 0) then
        print*, char(7), '\nError!'
        read *
        return
    else if (x <= 0 .or. x >= 185) then
        print*, char(7),'\nError!'
        read *
        return
    end if
    r = 0
    y = 1
    print*, ''
    l:do i = 1, x
        if (x - 1 .eq. i) then
            write (*, '(I0)', advance='no') y
            exit l
        end if
        write (*, '(I0, ",", 1X)', advance='no') y
        z = r + y
        r = y
        y = z
    end do l
    print*, '\n\nEnterを押してください。'
    read *
end subroutine fibonattisuretu

subroutine sosukaizyou()
    use iso_fortran_env, only: int64
    use m_usc, only: err, z
    implicit none
    integer(16) x, y, i
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'xを入力してください。(0 < x < 97)'
    read (*, *, iostat=err) x
    if (err .ne. 0) then
        print*, char(7),'\nError!'
        read *
        return
    else if (0 >= x .or. x >= 97) then
        print*, char(7),'\nError!'
        read *
        return
    end if
    y = 1
    do i = 1, x
        if (f(i)) then
            y = y * i
        end if
    end do
    print*, '\n答え'
    z = y
    print *, z
    print*, '\nEnterを押してください。'
    read *
contains
    pure logical(int64) function f(x_)
        implicit none
        integer(16), intent(in) :: x_
        integer(16) j
        select case(x_)
        case (0, 1)
            f = .false.
            return
        case (2, 3)
            f = .true.
            return
        end select
        if (mod(x_, 2) .eq. 0 .or. mod(x_, 3) .eq. 0) then
            f = .false.
            return
        end if
        j = 5
        do while (j * j <= x_)
            if (mod(x_, j) .eq. 0 .or. mod(x_, (j + 2)) .eq. 0) then
                f = .false.
                return
            end if
            j = j + 6
        end do
        f = .true.
    end function f
end subroutine sosukaizyou

!subroutine test() !テンプレート
!    use, intrinsic :: iso_fortran_env, only: real128, int64
!    implicit none
!    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
!    print*, '計算中'
!    print*, '\nEnterを押してください。'
!    read *
!end subroutine test

subroutine page_04()
    use m_usc, only: M_A, M_S, M_M, M_D, Memory
    implicit none
    character(256) str
    do
        write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        print '(A)', '\n-----------------------------------------'
        print*, '1 素数階乗'
        print*, '99 終了           03 Back'
        print '(A)', '-----------------------------------------'
        write (*, '(A)', advance='no') ': '
        read (*, '(A)') str
        select case(str)
        case ('1')
            call sosukaizyou()
        case ('00')
            call page_00()
        case ('01')
            call page_01()
        case ('02')
            call page_02()
        case ('03')
            call page_03()
        case ('99')
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            stop
        case ('M+', 'm+')
            call M_A()
        case ('M-', 'm-')
            call M_S()
        case ('M*', 'm*')
            call M_M()
        case ('M/', 'm/')
            call M_D()
        case ('M', 'm')
            call Memory()
        case default
            print*, char(7), 'type miss!'
            read *
        end select
    end do
end subroutine page_04

subroutine page_03()
    use m_usc, only: M_A, M_S, M_M, M_D, Memory
    implicit none
    character(256) str
    do
        write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        print '(A)', '\n-----------------------------------------'
        print*, '1 素数判定'
        print*, '2 完全数'
        print*, '3 アッカーマン関数 A(m, n)'
        print*, '4 シグモイド関数 ςa(x)'
        print*, '5 離散的フーリエ変換'
        print*, '6 tanθで建物の高さ'
        print*, '7 Life Game'
        print*, '8 光度距離計算'
        print*, '9 円周率その２(任意の桁数)'
        print*, '10 フィボナッチ数列'
        print*, '11 スロットゲーム\n'
        print*, '99 終了       02 Back       04 Next_page'
        print '(A)', '-----------------------------------------'
        write (*, '(A)', advance='no') ': '
        read (*, '(A)') str
        select case(str)
        case ('1')
            call sosuhantei()
        case ('2')
            call kanzensu()
        case ('3')
            call akkaman()
        case ('4')
            call sigmoid()
        case ('5')
            call furie()
        case ('6')
            call tan_h()
        case ('7')
            call lifegame()
        case ('8')
            call lumi_distance()
        case ('9')
            call pi_()
        case ('10')
            call fibonattisuretu()
        case ('11')
            call slot()
        case ('00')
            call page_00()
        case ('01')
            call page_01()
        case ('02')
            call page_02()
        case ('04')
            call page_04()
        case ('99')
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            stop
        case ('M+', 'm+')
            call M_A()
        case ('M-', 'm-')
            call M_S()
        case ('M*', 'm*')
            call M_M()
        case ('M/', 'm/')
            call M_D()
        case ('M', 'm')
            call Memory()
        case default
            print*, char(7), 'type miss!'
            read *
        end select
    end do
end subroutine page_03

subroutine page_02()
    use m_usc, only: M_A, M_S, M_M, M_D, Memory
    implicit none
    character(256) str
    do
        write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        print '(A)', '\n-----------------------------------------'
        print*, '1 ガンマ関数 Γ(z)'
        print*, '2 斜方投射での滞空時間と飛距離'
        print*, '3 斜方投射(空気抵抗なし)'
        print*, '4 自由落下(一分後まで)'
        print*, '5 円周率その１(桁数多め?)'
        print*, '6 平均値'
        print*, '7 階乗(n!)'
        print*, '8 リーマンゼータ関数 ζ(s)'
        print*, '9 コラッツ予想(ケチってreal128使ってます。)'
        print*, '10 素因数分解'
        print*, '11 ジョーク\n'
        print*, '99 終了       01 Back       03 Next_page'
        print '(A)', '-----------------------------------------'
        write (*, '(A)', advance='no') ': '
        read (*, '(A)') str
        select case(str)
        case ('1')
            call gamma_f()
        case ('3')
            call undouhouteisiki()
        case ('4')
            call ziyurakka()
        case ('2')
            call TX()
        case ('5')
            call ensyu()
        case ('6')
            call heikin()
        case ('7')
            call kaizyou()
        case ('8')
            call zetaf()
        case ('9')
            call collatz()
        case ('10')
            call soinsubunkai()
        case ('11')
            call joke()
        case ('00')
            call page_00()
        case ('01')
            call page_01()
        case ('03')
            call page_03()
        case ('04')
            call page_04()
        case ('99')
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            stop
        case ('M+', 'm+')
            call M_A()
        case ('M-', 'm-')
            call M_S()
        case ('M*', 'm*')
            call M_M()
        case ('M/', 'm/')
            call M_D()
        case ('M', 'm')
            call Memory()
        case default
            print*, char(7), 'type miss!'
            read *
        end select
    end do
end subroutine page_02

subroutine page_01()
    use m_usc, only: z, M_A, M_S, M_M, M_D, Memory
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    character(256) str
    do
        write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        print '(A)', '\n-----------------------------------------'
        print*, '1 モジュロ演算(a mod n)'
        print*, '2 疑似乱数(1～x)'
        print*, '3 e(ネイピア数)'
        print*, '4 立方根(三乗根)'
        print*, '5 絶対値'
        print*, '6 指数'
        print*, '7 双曲線正弦(sinh)'
        print*, '8 双曲線余弦(cosh)'
        print*, '9 双曲線正接(tanh)'
        print*, '10 φ(黄金数)'
        print*, '11 ???\n'
        print*, '99 終了       00 Back       02 Next_page'
        print '(A)', '-----------------------------------------'
        write (*, '(A)', advance='no') ': '
        read (*, '(A)') str
        select case(str)
        case ('1')
            call mozuro()
        case ('2')
            call randsu()
        case ('3')
            call neipia()
        case ('4')
            call y_zyoukon()
        case ('5')
            call zettaiti()
        case ('6')
            call sisu()
        case ('7')
            call soukyokusin()
        case ('8')
            call soukyokucos()
        case ('9')
            call soukyokutan()
        case ('10')
            print '(A)', '\x1b[2J\x1b[3J\x1b[H'
            block
                real(real128), parameter :: fai = (1.0_real128 + sqrt(5.0_real128)) * 0.5_real128
                print*, 'φ(黄金数)'
                print '(2F40.36)', fai
                z = fai
                print*, '\nEnterを押してください。'
            end block
            read *
        case ('11')
            block
                character(1024) user
                call getlog(user)
                print '(A, A, A)', '\n制作者:ware255(われ)\n\n???ってなんだろ&
                &う、って思ったでしょｗ\n思っちゃったやつソースコード見てね&
                &ぇって\n分かっちまうから''', trim(user),'''さん、気お付けろよｗ'
                read *
            end block
            exit
        case ('00')
            call page_00()
        case ('99')
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            stop
        case ('02')
            call page_02()
        case ('03')
            call page_03()
        case ('04')
            call page_04()
        case ('M+', 'm+')
            call M_A()
        case ('M-', 'm-')
            call M_S()
        case ('M*', 'm*')
            call M_M()
        case ('M/', 'm/')
            call M_D()
        case ('M', 'm')
            call Memory()
        case default
            print*, char(7), 'type miss!'
            read *
        end select
    end do
end subroutine page_01

subroutine page_00()
    use m_usc, only: z, M_A, M_S, M_M, M_D, Memory
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    character(256) str
    do
        write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        print '(A)', '\n-----------------------------------------'
        print*, '1 足し算          12 三角関数(sin)'
        print*, '2 引き算          13 三角関数(cos)'
        print*, '3 掛け算          14 三角関数(tan)'
        print*, '4 割り算          15 逆三角関数(asin)'
        print*, '5 平方根          16 逆三角関数(acos)'
        print*, '6 円周率          17 逆三角関数(atan)'
        print*, '7 円の面積        18 逆三角関数(atan2(y,x))'
        print*, '8 円の周長        19 虚数部(z = (x, iy))'
        print*, '9 べき乗          20 常用対数(log10)'
        print*, '10 二次方程式     21 自然対数(ln)'
        print*, '11 超戦略ゲーム\n'
        print*, '99 終了           01 Next_page'
        print '(A)', '-----------------------------------------'
        write (*, '(A)', advance='no') ': '
        read (*, '(A)') str
        select case(str)
        case ('1')
            call tasizan()
        case ('2')
            call hikizan()
        case ('3')
            call kakezan()
        case ('4')
            call warizan()
        case ('5')
            call heihoukon()
        case ('6')
            print '(A)', '\x1b[2J\x1b[3J\x1b[H'
            block
                real(real128), parameter :: PI = 4.0_real128 * atan(1.0_real128)
                print*, 'π(円周率)'
                print '(2F40.36)', PI
                z = PI
                print*, '\n Wikipediaでは以下(上の桁数に合わせた)'
                print '(A)', '  3.141592653589793238462643383279502884'
                print*, '\nEnterを押してください。'
            end block
            read *
        case ('7')
            call ensyuritu()
        case ('8')
            call syutyou()
        case ('9')
            call nizyou()
        case ('10')
            call nizihoutei()
        case ('11')
            call game()
        case ('12')
            call n_sin()
        case ('13')
            call n_cos()
        case ('14')
            call n_tan()
        case ('15')
            call n_asin()
        case ('16')
            call n_acos()
        case ('17')
            call n_atan()
        case ('18')
            call n_atan2()
        case ('19')
            call n_aimag()
        case ('20')
            call n_log10()
        case ('21')
            call n_log()
        case ('99')
            write(*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            stop
        case ('01')
            call page_01()
        case ('02')
            call page_02()
        case ('03')
            call page_03()
        case ('04')
            call page_04()
        case ('M+', 'm+')
            call M_A()
        case ('M-', 'm-')
            call M_S()
        case ('M*', 'm*')
            call M_M()
        case ('M/', 'm/')
            call M_D()
        case ('M', 'm')
            call Memory()
        case default
            print*, char(7), 'type miss!'
            read *
        end select
    end do
end subroutine page_00

program calculator
    !$ use omp_lib
    use, intrinsic :: iso_fortran_env, only: int32, int64, real64, real128
    implicit none
    character(256) str
    integer(int64) getuid, uid
    uid = getuid()
    if (.not. uid .eq. 0) then !権限なし
        open(0, file='data/.save.dat', status='old', err=1)
        close(0)
        call getarg(1, str)
        if (iargc() .eq. 0) call page_00()
        select case(str)
        case ('help')
            call help()
        case ('page_00', '00')
            call page_00()
        case ('page_01', '01')
            call page_01()
        case ('page_02', '02')
            call page_02()
        case ('page_03', '03')
            call page_03()
        case ('benchmark')
            print '(A)', '\n計算中です。\n'
            block
                integer(int64) i
                real(real128) :: s = 0.0_real128
                !$ real(real64) :: time_begin_s, time_end_s
                !$ time_begin_s = omp_get_wtime()
                !$omp parallel do num_threads(4), reduction(+:s), private(i)
                do i = 100000000_int64, 0, -1
                    s = s + ((-1.0_real128)**i) / ((real(i, real128) + real(i, real128)) + 1.0_real128)
                end do
                !$omp end parallel do
                !$ time_end_s = omp_get_wtime()
                print*, 'Answer:', s + s + s + s!s * 4.0_real128
                !$ print '(A, F13.5, A)', '\ntime:', time_end_s - time_begin_s, ' [sec]\n'
            end block
        case ('level')
            block
                integer(int64) level
                open(11, file='data/.level', status='old', err=110)
                    read (11, *) level
                    flush(11)
                close(11)
                print '("\n現在のレベル:\t", I0)', level
                print*, ''
                stop
110             error stop "\nError: 超戦略ゲームをプレイしてください。\n"
            end block
        case ('time')
            block
                integer(int64) dt(8), tmp, J
                real(real128) MJD, JD
                character(24) string, d(3)
                call date_and_time(d(1), d(2), d(3), dt)
                tmp = dt(2)
                if (tmp < 3) then
                    dt(1) = dt(1) - 1
                    dt(2) = dt(2) + 12
                end if
                J = int(365.250_real128 * dt(1), int64)
                J = J + dt(1) / 400
                J = J - dt(1) / 100
                J = J + int(30.590_real128 * (dt(2) - 2.0_real128), int64)
                J = J + dt(3)
                JD = real(J, real128) + 1721088.50_real128
                JD = JD + real(dt(5), real128) / 24.0_real128
                JD = JD + real(dt(6), real128) / 1440.0_real128
                JD = JD + real(dt(7), real128) / 86400.0_real128
                MJD = JD - 2400000.50_real128
                call fdate(string)
                print '("\n", A)', string
                print '("\nJulian day          : ", F0.16)', JD
                print '("Modified Julius Day :   ", F0.16)', MJD
                print*, ''
            end block
        case ('rsa', 'RSA')
            call rsa()
        case default
            print '(A)', '\nこの引数はありません。\n'
        end select
        stop
1       print '(A)', '\nUltra-Simple_Calculatorをインストールしていただき\nありがとうございます。&
        &あと、一応余計な一言ですが、\nテンキー使ったほうが楽ですよｗ\n'
        open(10, file='data/.save.dat', status='new')
        write(10, *) '1'
        close(10)
        read *
        call page_00()
    else
        if (iargc() .eq. 0) then
            print '(A)', '「大いなる力には大いなる責任が伴う」by ベンおじさん.'
            read *
            call page_00()
        end if
        print '(A)', '「大いなる力には大いなる責任が伴う」by ベンおじさん.'
        read *
        call getarg(1, str)
        select case(str)
        case ('help')
            call help()
        case ('page_00', '00')
            call page_00()
        case ('page_01', '01')
            call page_01()
        case ('page_02', '02')
            call page_02()
        case ('page_03', '03')
            call page_03()
        case ('benchmark')
            print '(A)', '計算中です。\n'
            block
                integer(int64) i
                real(real128) :: s = 0.0_real128
                !$ real(real64) :: time_begin_s, time_end_s
                !$ time_begin_s = omp_get_wtime()
                !$omp parallel do num_threads(4), reduction(+:s), private(i)
                do i = 100000000_int64, 0, -1
                    s = s + ((-1.0_real128) ** i) / ((real(i, real128) + real(i, real128)) + 1.0_real128)
                end do
                !$omp end parallel do
                !$ time_end_s = omp_get_wtime()
                print*, 'Answer:', s + s + s + s!s * 4.0_real128
                !$ print '(A, F13.5, A)', '\ntime:', time_end_s - time_begin_s, ' [sec]\n'
            end block
        case ('level')
            block
                integer(int64) level
                open(11, file='data/.level', status='old', err=111)
                    read (11, *) level
                    flush(11)
                close(11)
                print '("現在のレベル:\t", I0)', level
                print*, ''
                stop
                print*, char(7)
111             error stop "\nError: 超戦略ゲームをプレイしてください。\n"
            end block
        case ('time')
            block
                integer(int64) dt(8), tmp, J
                real(real128) MJD, JD
                character(24) string, d(3)
                call date_and_time(d(1), d(2), d(3), dt)
                tmp = dt(2)
                if (tmp < 3) then
                    dt(1) = dt(1) - 1
                    dt(2) = dt(2) + 12
                end if
                J = int(365.250_real128 * dt(1), int64)
                J = J + dt(1) / 400
                J = J - dt(1) / 100
                J = J + int(30.590_real128 * (dt(2) - 2.0_real128), int64)
                J = J + dt(3)
                JD = real(J, real128) + 1721088.50_real128
                JD = JD + real(dt(5), real128) / 24.0_real128
                JD = JD + real(dt(6), real128) / 1440.0_real128
                JD = JD + real(dt(7), real128) / 86400.0_real128
                MJD = JD - 2400000.50_real128
                call fdate(string)
                print '(A)', string
                print '("\nJulian day          : ", F0.16)', JD
                print '("Modified Julius Day :   ", F0.16)', MJD
                print*, ''
            end block
        case ('rsa', 'RSA')
            call rsa()
        case default
            print '(A)', 'この引数はありません。\n'
        end select
    end if
contains
    subroutine help()
        implicit none
        print '(A)', '使用法: ./calculator [オプション]'
        print '(A)', 'オプションがない場合はそのまま実行します。\n'
        print '(A)', 'オプション:'
        print '(3X, A)', 'page_00    -- 0ページ'
        print '(3X, A)', 'page_01    -- 1ページ'
        print '(3X, A)', 'page_02    -- 2ページ'
        print '(3X, A)', 'page_03    -- 3ページ'
        print '(3X, A)', 'help       -- 助けて'
        print '(3X, A)', 'benchmark  -- ベンチマークのテスト'
        print '(3X, A)', 'level      -- 超戦略ゲームでのレベル'
        print '(3X, A)', 'time       -- 現在の時刻'
        print '(3X, A)', 'rsa        -- RSAによる暗号化, 復号\n'
        print '(A)', '例:'
        print '(A)', '$ ./calculator page_00'
    end subroutine help

    subroutine rsa()
        use m_usc, only: err
        implicit none
        character(256) str_
        l:do
            write(*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            print '(A)', '\nSimple RSA cryptography'
            print '(A)', '-----------------------------------------'
            print *, '1 encryption'
            print *, '2 decryption'
            print *, ''
            print *, '99 終了'
            print '(A)', '-----------------------------------------'
            write (*, '(A)', advance='no') ': '
            read (*, '(A)') str_
            z:select case (str_)
            case ('1')
                write(*, '(A)', advance='no') '\nCalculating...'
                block
                integer(16) p, q, n, L_, e, d, plain_n, tmp, encryp_n!, i

                p = prime_N()
                q = prime_N()
                do while (p .eq. q)
                    p = prime_N()
                    q = prime_N()
                end do

                if (p < q) then
                    tmp = p
                    p = q
                    q = tmp
                endif

                n = p * q

                L_ = Lcm(p - 1, q - 1)

                e = 65537
                do while (extGCD(L_, e) .ne. 1)
                    e = e + 1
                end do

                d = extGCD(e, L_)
                do while (mod(e * d, L_) .ne. 1)
                    d = d + 1
                end do
                !m:do i = 100005, L_, 2
                !    if (mod((e * (i-1)), L_) .eq. 1 .or. mod((e * i), L_) .eq. 1) then
                !        d = i
                !        exit m
                !    end if
                !end do m

                print '("\r              \rN: ", I0&
                &, 3X, "E: ", I0, 3X, "D: ", I0)', n, e, d
                print '(A)', 'Enter the numbers you want to encrypt.'
                read (*, *, iostat=err) plain_n
                if (err .ne. 0) then
                    print *, 'Error!'
                    read *
                    exit z
                else if (plain_n > n) then
                    print *, 'The number to be encrypted is greater than N.'
                    read *
                    exit z
                end if
                encryp_n = extpower(plain_n, e, n)
                print '("\nEncrypted number: ", I0)', encryp_n
                read *
                end block
            case ('2')
                block
                integer(16) encryp_n, decryp_n, d, n
                print '(A)', '\nEnter the ciphertext with your private key and public key.'
                print '(A)', '(Encrypted_number, D, N)'
                read (*, *, iostat=err) encryp_n, d, n
                write(*, '(A)', advance='no') '\nCalculating...'
                if (err .ne. 0) then
                    print *, 'Error!'
                    read *
                    exit z
                end if
                decryp_n = extpower(encryp_n, d, n)
                print '("\r              \rDecrypted number: ", I0)', decryp_n
                read *
                end block
            case ('99')
                write(*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
                exit l
            case default 
                print *, char(7), 'Type miss!'
                read *
            end select z
        end do l
    end subroutine rsa

    integer(16) function randon()
        implicit none
        integer(int32) c, seedsize
        real(real128) y, x
        integer, allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        call random_seed(get=seed)
        call system_clock(count=c)
        seed(1) = c
        call random_seed(put=seed)
        call random_number(x)
        y = 100000_16 * x
        deallocate(seed)
        randon = int(y, 16)
    end function randon

    integer(16) function prime_N()
        implicit none
        integer(16) i, t, num, a(4)
        a = [2, 3, 5, 7]
        l:do while (.true.)
            num = randon()
            z:do i = 1, 4
                do while (modulo(num, a(i)) .eq. 0)
                    cycle l
                end do
            end do z
            t = int(sqrt(real(num, real128)), 16)
            m:do i = 11, t, 2
                if (modulo(i, 3) .eq. 0 .or. modulo(i, 5) .eq. 0 .or. modulo(i, 7) .eq. 0) then
                    cycle m
                else if (modulo(num, i) .eq. 0) then
                    cycle l
                end if
            end do m
            if (num .ne. 1) exit l
            exit l
        end do l
        prime_N = num
    end function prime_N

    pure recursive integer(16) function extGCD(a, b) result(d)
        implicit none
        integer(16), intent(in) :: a, b
        integer(16) x, y
        x = 1; y = 1;
        if (b .eq. 0) then
            x = 1
            y = 0
            d = a
            return
        end if
        d = extGCD(b, mod(a, b))
        y = y - a / b * x
        d = d
    end function extGCD

    pure recursive integer(16) function gcd(x, y) result(z)
        implicit none
        integer(16), intent(in) :: x, y
        if (y .eq. 0) then
            z = x
        else
            z = gcd(y, modulo(x, y))
        end if
    end function gcd

    pure integer(16) function Lcm(x, y)
        implicit none
        integer(16), intent(in) :: x, y
        Lcm = x * y / gcd(x, y)
    end function Lcm

    integer(16) function extpower(a, k, n)
        implicit none
        integer(16), intent(inout) :: a
        integer(16), intent(in) :: k, n
        integer(16) va, i

        a = modulo(a, n)

        if (a .eq. 0 .or. n .eq. 0) then
            extpower = 0
        else if (k .eq. 0) then
            extpower = modulo(1, n)
        end if

        va = 1
        do concurrent (i = 0: k - 1)
            block
            va = va * a
            if (va >= n) then
                va = modulo(va, n)
            end if
            end block
        end do

        extpower = va
    end function extpower
end program calculator
