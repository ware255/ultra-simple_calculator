module m_usc
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    integer, parameter :: LargeInt_K = selected_int_kind(18)
    integer(int64) err
    real(real128) z
end module m_usc

subroutine tasizan()
    use m_usc
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
            print*, '\nError!'
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
        print*, x + y
        z = x + y
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine tasizan

subroutine hikizan()
    use m_usc
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
            print*, '\nError!'
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
        print*, x - y
        z = x - y
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine hikizan

subroutine kakezan()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(real128) x, y
    character(len=256) :: str
    call ieee_set_rounding_mode(ieee_nearest)
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
            print*, x * x
            z = x * x
            print*, '\nEnterを押してください。'
            read *
            return
        end if
        read (str, *, iostat=err) y
        if (err .ne. 0) then
            print*, '\nError!'
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
        print*, x * y
        z = x * y
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine kakezan

subroutine warizan()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(real128) x, y
    call ieee_set_rounding_mode(ieee_nearest)
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print '(A)', '値を入力してください。'
        read (*, *, iostat=err) y
        if (err .ne. 0) then
            print*, '\nError!'
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
        print*, x / y
        z = x / y
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine warizan

subroutine heihoukon()
    use m_usc
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
            print*, '　一夜一夜に月見ごろ          <= 覚え方'
        else if (x .eq. 3.) then
            print*, '　人並みにおごれや            <= 覚え方'
        end if
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine heihoukon

subroutine ensyuritu()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(real128), parameter :: pi = 4.0_real128*atan(1.0_real128)
    real(real128) r
    call ieee_set_rounding_mode(ieee_nearest)
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) r
    if (err .eq. 0) then
        print*, '\n答え'
        print*, r * r * pi
        z = r * r * pi
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine ensyuritu

subroutine syutyou()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(real128), parameter :: pi = 4.0_real128*atan(1.0_real128)
    real(real128) r
    call ieee_set_rounding_mode(ieee_nearest)
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) r
    if (err .eq. 0) then
        print*, '\n答え'
        print*, 2 * pi * r
        z = 2 * pi * r
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine syutyou

subroutine nizyou()
    use m_usc
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
            print*, '\nError!'
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
        print*, x**y
        z = x**y
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine nizyou

subroutine game_1()
    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=10) d
    integer(int64) hero_hp, hero_mp, enemy1_hp, enemy1_mp, n, x
    integer(int64) :: mp = 0, y
    n = 0;x = 0
    hero_hp = 5;enemy1_hp = 10
    hero_mp = 5;enemy1_mp = 10
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print*, '\n超戦略ゲーム  ~ shit video game ~\n\n\n\n\nEnterを押してください。'
    read *
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    do
        if (hero_hp .le. 0) then
            print*, '\nGAME OVER'
            read *
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        else if (enemy1_hp .le. 0) then
            print '(A)', '\n敵を撲殺することが出来た。ワイの勝利！！！'
            print '(A)', 'Ураааааааааааааааа!'
            read *
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
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
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy1_hp = enemy1_hp - 2
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ2'
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy1_hp = enemy1_hp - 3
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ3'
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (4)
                enemy1_hp = enemy1_hp - 4
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ4'
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (5)
                enemy1_hp = enemy1_hp - 5
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ5'
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy1_hp = enemy1_hp - 0
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
                enemy1_hp = enemy1_hp - 5
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ5'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy1_hp = enemy1_hp - 6
                hero_mp = hero_mp - 1
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ6'
                print '(A)', 'MP: 1消費'
                print "('ワイのダメージ',i0)", x
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
                do y = 5, 100000000
                    if (mp .eq. y) then
                        print '(A)', 'MP: 1回復'
                        hero_mp = hero_mp + 1
                        exit
                    else if (mp .lt. y) then
                        exit
                    end if
                end do
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
                exit
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
    end do
    contains
    integer(int32) function add(n)
        implicit none
        integer(int32) rad, n
        integer(int32) seedsize, c
        real(real32) y, x
        integer, allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        do
            call random_seed(get=seed)
            call system_clock(count=c)
            seed(1) = c
            call random_seed(put=seed)
            call random_number(x)
            y = x*100
            rad = int(y)
            if (rad .lt. n) exit
        end do
        deallocate(seed)
        add = rad
    end function
end subroutine game_1

subroutine game_2()
    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=10) d
    integer(int64) hero_hp, hero_mp, enemy2_hp, enemy2_mp, n, x
    integer(int64) :: mp = 0, y
    n = 0;x = 0
    hero_hp = 5;enemy2_hp = 15
    hero_mp = 5;enemy2_mp = 15
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print *, '\n超戦略ゲーム  ~ shit video game ~\n\n\n\n\nEnterを押してください。'
    read *
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    do
        if (hero_hp .le. 0) then
            print *, '\nGAME OVER'
            read *
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        else if (enemy2_hp .le. 0) then
            print '(A)', '\n敵を撲殺することが出来た。ワイの勝利！！！'
            print '(A)', 'Ураааааааааааааааа!'
            read *
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
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
                do y = 5, 100000000
                    if (mp .eq. y) then
                        print '(A)', 'MP: 1回復'
                        hero_mp = hero_mp + 2
                    else if (mp .lt. y) then
                        exit
                    end if
                end do
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
                exit
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
    end do
    contains
    integer(int32) function add(n)
        implicit none
        integer(int32) rad, n
        integer(int32) seedsize, c
        real(real32) y, x
        integer, allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        do
            call random_seed(get=seed)
            call system_clock(count=c)
            seed(1) = c
            call random_seed(put=seed)
            call random_number(x)
            y = x*100
            rad = int(y)
            if (rad .lt. n) exit
        end do
        deallocate(seed)
        add = rad
    end function
end subroutine game_2

subroutine game_3()
    use m_usc, only: LargeInt_K
    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=10) d
    integer(LargeInt_K) hero_hp, hero_mp, enemy3_hp, enemy3_mp, n, x
    integer(int64) :: mp = 0, y, level
    open(1, file='.level', status='old')
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
        hero_hp = 99999999999999999_8;enemy3_hp = 999999999999999999_8
        hero_mp = 99999999999999999_8;enemy3_mp = 20
    end select
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print*, '\n超戦略ゲーム  ~ shit video game ~\n\n\n\n\nEnterを押してください。'
    read *
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    do
        if (hero_hp .le. 0) then
            print*, '\nGAME OVER'
            read *
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        else if (enemy3_hp .le. 0) then
            print '(A)', '\n敵を撲殺することが出来た。ワイの勝利！！！'
            print '(A)', 'Ураааааааааааааааа!'
            if (level .eq. 10) then
                print '(A)', '\nレベルMaxです。'
                goto 110
            end if
            open(2, file='.level', status='old')
                level = level + 1
                write(2, *) level
                flush(2)
            close(2)
            print '(A)', '\nレベル1上がった。'
110         read *
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
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
            exit
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
                do y = 5, 100000000
                    if (mp .eq. y) then
                        print '(A)', 'MP: 1回復'
                        hero_mp = hero_mp + 1
                    else if (mp .lt. y) then
                        exit
                    end if
                end do
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
                exit
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
    end do
    contains
    integer(int32) function add(n)
        implicit none
        integer(int32) rad, n
        integer(int32) seedsize, c
        real(real32) y, x
        integer, allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        do
            call random_seed(get=seed)
            call system_clock(count=c)
            seed(1) = c
            call random_seed(put=seed)
            call random_number(x)
            y = x*100
            rad = int(y)
            if (rad .lt. n) exit
        end do
        deallocate(seed)
        add = rad
    end function
end subroutine game_3

subroutine game()
    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int64) n
    n = add()
    open(1, file='.level', status='old', err=110)
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
110 open(2, file='.level', status='new')
        write(2, *) 0
        flush(2)
    close(2)
120 write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    contains
    integer(int32) function add()
        implicit none
        integer(int32) rad, c
        integer(int32) seedsize
        real(int32) y, x
        integer, allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        do
            call random_seed(get=seed)
            call system_clock(count=c)
            seed(1) = c
            call random_seed(put=seed)
            call random_number(x)
            y = x*100
            rad = int(y)
            if (rad .lt. 9) exit
        end do
        deallocate(seed)
        add = rad
    end function
end subroutine game

subroutine nizihoutei()
    use m_usc, only: err
    use, intrinsic :: iso_fortran_env, only: real128
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(real128) a, b, c, k1, k2
    call ieee_set_rounding_mode(ieee_nearest)
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '一般: ax^2 + bx + c = 0\n'
    print '(A)', 'a値を入力してください。'
    read (*, *, iostat=err) a
    if (err .eq. 0) then
        print '(A)', 'b値を入力してください。'
        read (*, *, iostat=err) b
        if (err .eq. 0) then
            print '(A)', 'c値を入力してください。'
            read (*, *, iostat=err) c
            if (err .eq. 0) then
                print '(A)', '\n答え'
                k1 = (-b+sqrt((b*b)-4*a*c)) / (2*a)
                k2 = (-b-sqrt((b*b)-4*a*c)) / (2*a)
                open (11, file='nizihoutei.txt', status='replace')
                    write (11, *) k1
                    flush(11)
                    write (11, *) k2
                    flush(11)
                close (11)
                print *, k1
                print *, k2
                print '(A)', '\nEnterを押してください。'
                read *
            else
                print*, '\nError!'
                read *
                return
            end if
        else
            print*, '\nError!'
            read *
            return
        end if
    else
        print*, '\nError!'
        read *
        return
    end if
end subroutine nizihoutei

subroutine n_sin()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print*, '\n答え'
        print*, sin(n)
        z = sin(n)
        print '(A)', '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
        return
    end if
end subroutine n_sin

subroutine n_cos()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print '(A)', '\n答え'
        print*, cos(n)
        z = cos(n)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
        return
    end if
end subroutine n_cos

subroutine  n_tan()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print*, '\n答え'
        print*, tan(n)
        z = tan(n)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
        return
    end if
end subroutine n_tan

subroutine n_asin()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print*, '\n答え'
        print*, asin(n)
        z = asin(n)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
        return
    end if
end subroutine n_asin

subroutine n_acos()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print*, '\n答え'
        print*, acos(n)
        z = acos(n)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
        return
    end if
end subroutine n_acos

subroutine n_atan()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print*, '\n答え'
        print*, atan(n)
        z = atan(n)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
        return
    end if
end subroutine n_atan

subroutine n_atan2()
    use m_usc
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
            print*, atan2(y, x)
            z = atan2(y, x)
            print*, '\nEnterを押してください。'
            read *
        else
            print*, '\nError!'
            read *
            return
        end if
    else
        print*, '\nError!'
        read *
        return
    end if
end subroutine n_atan2

subroutine n_aimag()
    use m_usc, only: err
    implicit none
    complex(16) z
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '入力例: (2.71, 0.99)\n※()も入力してください。\n'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) z
    if (err .eq. 0) then
        print*, '\n答え'
        print*, aimag(z)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
        return
    end if
end subroutine n_aimag

subroutine n_log10()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print*, '\n答え'
        print*, log10(n)
        z = log10(n)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
        return
    end if
end subroutine n_log10

subroutine n_log()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) n
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        print*, '\n答え'
        print*, log(n)
        z = log(n)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
        return
    end if
end subroutine n_log

subroutine mozuro
    use m_usc
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
            print*, mod(a, n)
            z = mod(a, n)
            print*, '\nEnterを押してください。'
            read *
        else
            print*, '\nError!'
            read *
            return
        end if
    else
        print*, '\nError!'
        read *
        return
    end if
end subroutine mozuro

subroutine randsu()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128, int64, int32
    implicit none
    integer(int64) n
    real(real128) x
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
        n = randon(x)
        print*, '\n出力'
        print*, n
        z = n
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
    contains
    integer(int64) function randon(n)
        implicit none
        integer(int64) rad
        integer(int32) seedsize, c
        real(real128) y, x, n
        integer, allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        if (n .le. 1024) then
            do
                call random_seed(get=seed)
                call system_clock(count=c)
                seed(1) = c
                call random_seed(put=seed)
                call random_number(x)
                y = x*1024
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else if (n .le. 7812524) then
            do
                call random_seed(get=seed)
                call system_clock(count=c)
                seed(1) = c
                call random_seed(put=seed)
                call random_number(x)
                y = x*7812524
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else if (n .le. 15625024) then
            do
                call random_seed(get=seed)
                call system_clock(count=c)
                seed(1) = c
                call random_seed(put=seed)
                call random_number(x)
                y = x*15625024
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else if (n .le. 31250024) then
            do
                call random_seed(get=seed)
                call system_clock(count=c)
                seed(1) = c
                call random_seed(put=seed)
                call random_number(x)
                y = x*31250024
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else if (n .le. 62500024) then
            do
                call random_seed(get=seed)
                call system_clock(count=c)
                seed(1) = c
                call random_seed(put=seed)
                call random_number(x)
                y = x*62500024
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else if (n .le. 125000024) then
            do
                call random_seed(get=seed)
                call system_clock(count=c)
                seed(1) = c
                call random_seed(put=seed)
                call random_number(x)
                y = x*125000024
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else if (n .le. 250000024) then
            do
                call random_seed(get=seed)
                call system_clock(count=c)
                seed(1) = c
                call random_seed(put=seed)
                call random_number(x)
                y = x*250000024
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else if (n .le. 500000024) then
            do
                call random_seed(get=seed)
                call system_clock(count=c)
                seed(1) = c
                call random_seed(put=seed)
                call random_number(x)
                y = x*500000024
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else
            do
                call random_seed(get=seed)
                call system_clock(count=c)
                seed(1) = c
                call random_seed(put=seed)
                call random_number(x)
                y = x * 9223372036854775807_8
                rad = int(y, 8)
                if (rad .lt. n) exit
            end do
        end if
        deallocate(seed)
        randon = rad
    end function
end subroutine randsu

subroutine neipia() ! e = lim n->Infinity (1+1/n)**n | Σn=0 ∞ 1/n!
    use m_usc, only: z
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    integer(int64), parameter :: n = 1024
    integer(int64) a
    real(real128) :: b = 1.0_16, e = 1.0_16
    print '(A)', '\x1b[2J\x1b[3J\x1b[H'
    do a = 1, n
        b = b * a
        e = e + 1 / b
    end do
    print*, 'ネイピア数(総和は1から1024まで)'
    print '(2F40.36)', e
    z = e
    print*, '\n Wikipediaでは以下(上の桁数に合わせた)'
    print '(A)', '  2.718281828459045235360287471352662497'
    print*, '\nEnterを押してください。'
    read *
end subroutine neipia

subroutine y_zyoukon()
    use m_usc
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
            print*, '\nError!'
            read *
            return
        end if
        print*, '答え'
        print*, x**(1 / y)
        z = x**(1 / y)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine y_zyoukon

subroutine zettaiti()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        print*, abs(x)
        z = abs(x)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine zettaiti

subroutine sisu()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        print*, exp(x)
        z = exp(x)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine sisu

subroutine soukyokusin()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        print*, sinh(x)
        z = sinh(x)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine soukyokusin

subroutine soukyokucos()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        print*, cosh(x)
        z = cosh(x)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine soukyokucos

subroutine soukyokutan()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        print*, tanh(x)
        z = tanh(x)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine soukyokutan

subroutine gamma_f()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128) x
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        print*, gamma(x)
        z = gamma(x)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine gamma_f

subroutine joke()
    use, intrinsic :: iso_fortran_env, only: real128, int64, int32
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
        error stop 'Error: There''s an anomaly in variable x'
    end select
    contains
    integer(int32) function randon()
        implicit none
        integer(int32) rad, c
        integer(int32) seedsize
        real(real128) y, x
        integer, allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        do
            call random_seed(get=seed)
            call system_clock(count=c)
            seed(1) = c
            call random_seed(put=seed)
            call random_number(x)
            y = x*100
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
    integer(int64) i, j
    real(real128), parameter :: pi = 3.1415926535897932384626433832795028840_16
    real(real128) g, V, angle, theta, u, w, x, y
    real(real128) dxdt, dydt, zero
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
        g = 9.806650_16
        theta = pi / 180.0_16 * angle
        zero = 0.0_16
        x = zero
        y = zero
        u = V * cos(theta)
        w = V * sin(theta)
        open(11, file='output.txt', status='replace')
        write(11, '("\t", F0.23, "\t", F0.23)') x, y
        !$ st = omp_get_wtime()
        do i = 1, 600000_8 ! 一分間
            dxdt = u
            dydt = w
            do j = 1, 1_8
                x = x + 0.00010_16 * dxdt
                y = y + 0.00010_16 * dydt
                u = u + 0.00010_16 * zero
                w = w + 0.00010_16 * (- g)
                write(11, '("\t", F0.23, "\t", F0.23)') x, y
            end do
        end do
        !$ en = omp_get_wtime()
        close(11)
        !$ print *, "Elapsed time :", en-st
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine undouhouteisiki

subroutine undouplot()
    implicit none
    open(11, file='output.txt', status='old', err=110)
    close(11)
    call system('gnuplot')
    goto 120
110 print*, '運動方程式を計算してください。'
120 print*, '\nEnterを入力してください。'
    read *
end subroutine undouplot

subroutine TX()
    use m_usc, only: err
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128), parameter :: pi = 3.1415926535897932384626433832795028840_16
    real(real128) g, V, angle, theta, T, L, H, d, V2, H_T
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
        g = 9.806650_16
        theta = pi / 180.0_16 * angle
        d = sin(theta)
        V2 = V * V

        T = (2.0_16 * V * d) / g
        L = (V2 * sin(2.0_16 * theta)) / g
        H = (V2 * (d * d)) / 19.61330_16
        H_T = (V * d) / g

        print*, '\n滞空時間'
        print '("\t", F0.36, " [sec]")', T
        print*, '\n飛距離'
        print '("\t", F0.36, " [m]")', L
        print*, '\n最高高度時の時間'
        print '("\t", F0.36, " [sec]")', H_T
        print*, '\n最高高度'
        print '("\t", F0.36, " [m]")', H
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine TX

subroutine ensyu()
    !$ use omp_lib
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none
    integer(int64), parameter :: vmax = 428800, bmax = 25728
    integer(int64) vect(vmax), buffer(bmax)
    integer(int64) n, L, more, num, carry, k, d
    !$ double precision st, en
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'ちょっと待っててね\n終わったあとは、メモ帳を&
    &大画面にした方がええで\n'
    !$ st = omp_get_wtime()
    vect(1:vmax) = 2
    more = 0
    do n = 1, bmax
        carry = 0
        do L = vmax, 1, -1
            num = (100000 * vect(L)) + (carry * L)
            d = ((2*L) - 1)
            carry = num / d
            vect(L) = num - (carry * d)
        end do
        k = carry / 100000
        buffer(n) = more + k
        more = carry - k * 100000
    end do
    !$ en = omp_get_wtime()
    open(11, file="pi.txt", status="replace")
        write(11, "(1x, I1, '.'/(1x, 32I5.5))") buffer
        flush(11)
    close(11)
    write(*, "(1x, I1, '.'/(1x, 12I5.5))") buffer
    !$ print *, "Elapsed time :", en-st
    print*, '\nEnterを押してください。'
    read *
end subroutine ensyu

subroutine heikin()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    integer(LargeInt_K) i, max
    real(real128), allocatable :: x(:)
    real(real128) :: y = 0.0_16
    character char
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '観測値を入力してください。'
    read (*, *, iostat=err) max
    if (err .eq. 0) then
        print*, ''
        allocate(x(max))
        do i = 1, max
            print '(I0, "つ目の値を入力してください。(止めるときはqを入力)")', i
            read (*, '(A)') char
            if (char .eq. 'q') then
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
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    integer(int64) n, k
    real(real128) ans
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        ans = 1.0_16
        do k = 1, n
            ans = ans * k
        end do
        print*, '\n答え'
        print '("  ", i0, "! = ", F0.0)', n, ans
        z = ans
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine kaizyou

subroutine zetaf()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128, int64
    implicit none
    real(real128), parameter :: e = 2.7182818284590452353602874713526624970_16
    real(real128) zeta, s!, k
    integer(int64) i
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) s
    if (err .eq. 0) then
        zeta = 0.0_16
        do i = 41943020_8, 1, -1
            !k = log(real(i, 16))*s
            zeta = zeta + (1.0_16 / i**s)!e**k
        end do
        print*, '\n答え'
        print*, zeta
        z = zeta
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine zetaf

subroutine collatz()
    use m_usc, only: err, LargeInt_K
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    real(real128), parameter :: q = 2.0_16
    real(real128) n, h
    integer(LargeInt_K) i
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n
    if (err .eq. 0) then
        i = 0
        print*, ''
        do
            h = mod(n, q)
            if (n .eq. 1) exit
            select case(int(h, LargeInt_K))
            case (1)
                n = n * 3 + 1
            case (0)
                n = n * 0.5
            end select
            write (*, '(I0, ", ")', advance='no') int(n, LargeInt_K)
            i = i + 1
        end do
        print '("\n\n", I0, " 回の操作で答えが ", I0)', i, int(n)
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine collatz

subroutine M_A()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    character(len=256) str
    real(real128) x
    print '(A)', '値を入力してください。'
    read (*, '(A)') str
    read (str, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        print*, z + x
        z = z + x
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine M_A

subroutine M_S()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    character(len=256) str
    real(real128) x
    print '(A)', '値を入力してください。'
    read (*, '(A)') str
    read (str, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        print*, z - x
        z = z - x
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine M_S

subroutine M_M()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    character(len=256) str
    real(real128) x
    print '(A)', '値を入力してください。'
    read (*, '(A)') str
    if (str .eq. '') then
        print*, z * z
        z = z * z
        print*, '\nEnterを押してください。'
        read *
        return
    end if
    read (str, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        print*, z * x
        z = z * x
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine M_M

subroutine M_D()
    use m_usc
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    character(len=256) str
    real(real128) x
    print '(A)', '値を入力してください。'
    read (*, '(A)') str
    read (str, *, iostat=err) x
    if (err .eq. 0) then
        print*, '\n答え'
        print*, z / x
        z = z / x
        print*, '\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine M_D

subroutine soinsubunkai()
    use m_usc, only: err, LargeInt_K
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    integer(LargeInt_K) n, i, m, k
    real(real128) n_
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) n_
    n = int(n_, LargeInt_K)
    if (err .eq. 0) then
        print*, '\n答え'
        write (*, '("\t", I0, A)', advance='no') n,' = 1'
        k = n
        i = 2
        do while (i <= k)
            m = mod(k, i)
            if (m .eq. 0)then
                write (*, '(A, I0)', advance='no') ' * ', i 
                k = k / i
                cycle
            else if (k .eq. i) then
                exit
            else
                i = i + 1
                cycle
            endif
        end do
        print*, '\n\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
end subroutine soinsubunkai

subroutine sosuhantei()
    use m_usc, only: err, LargeInt_K
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    integer(LargeInt_K) p, i
    real(real128) q
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *, iostat=err) q
    p = int(q, LargeInt_K)
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
        do while ((i * i) <= p)
            if (mod(p, i) .eq. 0) then
                print*, '\n答え'
                print '("\t", I0, "は素数ではありません。")', p
                print*, '\nEnterを押してください。'
                read *
                return
            else if (mod(p, (i + 2)) .eq. 0) then
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
        print*, '\nError!'
        read *
    end if
end subroutine sosuhantei

subroutine slot()
    use, intrinsic :: iso_fortran_env, only: real128, int64, int32
    implicit none
    character char
    integer(int64) i, j, x, a, b, c, k
    i = 0; a = 0; b = 0; c = 0; k = 0
11  do j = 0, 3
        write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        print '(A)', '┌─────────────────┐'
        print '(A, I0, A, I0, A, I0, A)', '｜  ', a, '  |  ', b, '  |  ', c, ' ｜'
        print '(A)', '└─────────────────┘'
        print '(A)', '\nEnterを押してください。'
        read (*, '(A)') char
        x = randon()
        select case (j)
        case (0)
            a = x
        case (1)
            b = x
        case (2)
            c = x
        end select
        if (j .eq. 3) then
            if (a .eq. b .and. b .eq. c) then
                print '(A)', '\n当たりｷﾀ━━━━(ﾟ∀ﾟ)━━━━!!'
                i = i - 2
                k = k + 2
                read (*, '(A)') char
            else if (a .eq. 7 .and. b .eq. 7 .and. c .eq. 7) then
                print '(A)', '\n超極レアスーパーナンバーｷﾀ━━━━(ﾟ∀ﾟ)━━━━!!'
                print '(A)', '当たる超確率up'!(大噓)
                i = i - 3
                k = k + 3
                read (*, '(A)') char
            else if (a .eq. b .or. a .eq. c .or. b .eq. c) then
                print '(A)', '\nリーチ(＞ω＜)/'
                i = i - 1
                k = k + 1
                read (*, '(A)') char
            else
                print '(A)', '\nおしい！'
                i = i + 1
                k = k + 1
                if (i .eq. 5) then
                    print '(A, I0, "回", I0)', '\nゲームオーバー\t記録:', k
                    read (*, '(A)') char
                    exit
                end if
                read (*, '(A)') char
            end if
            goto 11
        else if (char .eq. 'q') then
            exit
        end if
    end do
    contains
    integer(int32) function randon()
        implicit none
        integer(int32) seedsize, c, rad
        real(real128) y, x
        integer, allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        do
            call random_seed(get=seed)
            call system_clock(count=c)
            seed(1) = c
            call random_seed(put=seed)
            call random_number(x)
            y = x*100
            rad = int(y)
            if (rad .lt. 10) exit
        end do
        deallocate(seed)
        randon = rad
    end function
end subroutine slot

subroutine kanzensu()
    use m_usc, only: err, LargeInt_K
    use, intrinsic :: iso_fortran_env, only: real128
    implicit none
    integer(LargeInt_K) x
    real(real128) p, n, i, j
    write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'xを入力してください。(0 < x < 62)'
    read (*, *, iostat=err) x
    if (err .eq. 0) then
        if (0 >= x .or. 62 <= x) then
            print*, '変域にしたがってください。'
            read *
            return
        end if
        i = 2.0_16; j = 0.0_16
        print*, ''
        do
            if (j .eq. x) exit
            n = (2.0_16 ** i) - 1.0_16
            if (is_prime(int(n, LargeInt_K))) then
                p = (2.0_16 ** (i - 1.0_16)) * n
                j = j + 1.0_16
                write (*, '(F0.0, " ")', advance='no') p
            end if
            i = i + 1.0_16
        end do
        print*, '\n\nEnterを押してください。'
        read *
    else
        print*, '\nError!'
        read *
    end if
    contains
    pure logical function is_prime(n)
        implicit none
        integer(LargeInt_K), intent(in) :: n
        integer(LargeInt_K) i
        select case(n)
        case (0, 1)
            is_prime = .false.
        case (2)
            is_prime = .true.
        end select
        if (mod(n, 2) .eq. 0) is_prime = .false.
        i = 3
        do while((i * i) <= n)
            if (mod(n, i) .eq. 0) is_prime = .false.
            i = i + 2
        end do
        is_prime = .true.
    end function
end subroutine kanzensu

subroutine page_03()
    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=256) str
    do
        write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        print '(A)', '\n-----------------------------------------'
        print*, '1 素数判定'
        print*, '2 完全数'
        !print*, '3 積分'
        print*, '11 スロットゲーム\n'
        print*, '99 終了           02 Back'
        print '(A)', '-----------------------------------------'
        write (*, '(A)', advance='no') ': '
        read (*, '(A)') str
        select case(str)
        case ('1')
            call sosuhantei()
        case ('2')
            call kanzensu()
        !case ('3')
            !call sekibun()
        case ('11')
            call slot()
        case ('00')
            call page_00()
        case ('01')
            call page_01()
        case ('02')
            call page_02()
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
        case default
            print*, 'そんなもんねぇよｗ'
            read *
        end select
    end do
end subroutine page_03

subroutine page_02()
    implicit none
    character(len=256) str
    do
        write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        print '(A)', '\n-----------------------------------------'
        print*, '1 ガンマ関数 Γ(z)'
        print*, '2 斜方投射での滞空時間と飛距離'
        print*, '3 斜方投射(一分後まで)'
        print*, '4 斜方投射(gnuplotでグラフを描画)'
        print*, '5 円周率をtxtファイルで出力(桁数多め)'
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
            write (*, '(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            print '(A)', '以下のコマンドを入力してください。\n'
            print '(A)', '================================'
            print '(A)', 'set style data dots    //グラフを線で表示'
            print '(A)', 'set xrange [0:50]      //x軸を0~50'
            print '(A)', 'set yrange [0:30]      //y軸を0~30'
            print '(A)', 'plot "output.txt"      //グラフを表示'
            print '(A)', '================================'
            print '(A)', 'exit                   //終了コマンド'
            print '(A)', '================================'
            call undouplot()
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
        case default
            print*, 'そんなもんねぇよｗ'
            read * !call sleep(1)
        end select
    end do
end subroutine page_02

subroutine page_01()
    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=256) str
    character(len=1024) user
    real(real128), parameter :: fai = (1.0_real128+sqrt(5.0_real128))*0.5_real128
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
            print*, 'φ(黄金数)'
            print '(2F40.36)', fai
            print*, '\nEnterを押してください。'
            read *
        case ('11')
            call getlog(user)
            print '(A, A, A)', '\n制作者:ware255(われ)\n\n???ってなんだろ&
            &う、って思ったでしょｗ\n思っちゃったやつソースコード見てね&
            &ぇって\n分かっちまうから''', trim(user),'''さん、気お付けろよｗ'
            read *
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
        case ('M+', 'm+')
            call M_A()
        case ('M-', 'm-')
            call M_S()
        case ('M*', 'm*')
            call M_M()
        case ('M/', 'm/')
            call M_D()
        case default
            print*, 'そんなもんねぇよｗ'
            read * !call sleep(1)
        end select
    end do
end subroutine page_01

subroutine page_00()
    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=256) str
    real(real128), parameter :: PI = 4.0_real128*atan(1.0_real128)
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
        print*, '10 2次方程式      21 自然対数(ln)'
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
            print*, 'π(円周率)'
            print '(2F40.36)', PI
            print*, '\n Wikipediaでは以下(上の桁数に合わせた)'
            print '(A)', '  3.141592653589793238462643383279502884'
            print*, '\nEnterを押してください。'
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
        case ('M+', 'm+')
            call M_A()
        case ('M-', 'm-')
            call M_S()
        case ('M*', 'm*')
            call M_M()
        case ('M/', 'm/')
            call M_D()
        case default
            print*, 'そんなもんねぇよｗ'
            read *
        end select
    end do
end subroutine page_00

subroutine help()
    implicit none
    print '(A)', '使用法: ./calculator [オプション]'
    print '(A)', 'オプションがない場合はそのまま実行します。\n'
    print '(A)', 'オプション:'
    print*, 'page_00    -- 0ページ'
    print*, 'page_01    -- 1ページ'
    print*, 'page_02    -- 2ページ'
    print*, 'page_03    -- 3ページ'
    print*, 'help       -- 助けて'
    print*, 'benchmark  -- ベンチマークのテスト'
    print*, 'time       -- 現在の時刻\n'
    print '(A)', '例:'
    print '(A)', '$ ./calculator page_00'
end subroutine help

program calculator
    !$ use omp_lib
    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=256) str
    character(len=24) string
    integer(int64) level, getuid, uid, i
    real(real128) :: s = 0.0_16
    !$ real(real64) :: time_begin_s,time_end_s
    uid = getuid()
    if (.not. uid .eq. 0) then !権限なし
        open(0, file='.save.dat', status='old', err=1)
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
            !$ time_begin_s = omp_get_wtime()
            !$omp parallel num_threads(4)
            !$omp do
            do i = 0, 10*32767
                !$omp critical
                s = s + ((-1.0_16)**i) / (2.0_16 * real(i, 16) + 1.0_16)
                !$omp end critical
            end do
            !$omp end do
            !$omp end parallel
            !$ time_end_s = omp_get_wtime()
            print*, 'Answer:', s * 4
            !$ print '(A, F13.5, A)', '\ntime:', time_end_s - time_begin_s, ' [sec]\n'
        case ('level')
            open(11, file='.level', status='old', err=110)
                read (11, *) level
                flush(11)
            close(11)
            print '("\n現在のレベル:\t", I0)', level
            print*, ''
            stop
110         error stop "\nError: 超戦略ゲームをプレイしてください。\n"
        case ('time')
            call fdate(string)
            print '("\n", A)', string
            print*, ''
        case default
            print '(A)', '\nこの引数はありません。\n'
        end select
        stop
1       print '(A)', '\nUltra-Simple_Calculatorをインストールしていただき\nありがとうございます。&
        &あと、一応余計な一言ですが、\nテンキー使ったほうが楽ｗ\n'
        open(10, file='.save.dat', status='new')
        write(10, *) '1'
        close(10)
        read *
        call page_00()
    else
        print '(A)', '\n※いつでもどこでも電卓が使えるようにして\n&
        &　いるためroot権限は実装しておりません。\n'
    end if
end program calculator
