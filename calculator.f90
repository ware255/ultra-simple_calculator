subroutine tasizan()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: x, y
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *) x
    print '(A)', '値を入力してください。'
    read (*, *) y
    print*, '\n答え'
    print*, x + y
    print*, '\nEnterを押してください。'
    read *
end subroutine tasizan

subroutine hikizan()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: x, y
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *) x
    print '(A)', '値を入力してください。'
    read (*, *) y
    print*, '\n答え'
    print*, x - y
    print*, '\nEnterを押してください。'
    read *
end subroutine hikizan

subroutine kakezan()
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(real128) :: x, y
    character(len=256) :: str
    call ieee_set_rounding_mode(ieee_nearest)
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *) x
    print '(A)', '値を入力してください。'
    read (*, '(A)') str
    if (str .eq. '') then
        print*, '\n答え'
        print*, x * x
        print*, '\nEnterを押してください。'
        read *
        goto 11
    end if
    read (str, *) y
    print*, '\n答え'
    print*, x * y
    print*, '\nEnterを押してください。'
    read *
11  write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine kakezan

subroutine warizan()
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(real128) :: x, y
    call ieee_set_rounding_mode(ieee_nearest)
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *) x
    print '(A)', '値を入力してください。'
    read (*, *) y
    print*, '\n答え'
    print*, x / y
    print*, '\nEnterを押してください。'
    read *
end subroutine warizan

subroutine heihoukon()
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_features, only:ieee_sqrt
    implicit none
    real(real128) :: x
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *) x
    print*, '\n近似値'
    print*, sqrt(x)
    if (x .eq. 2.) then
        print*, '　一夜一夜に月見ごろ          <= 覚え方'
    else if (x .eq. 3.) then
        print*, '　人並みにおごれや            <= 覚え方'
    end if
    print*, '\nEnterを押してください。'
    read *
end subroutine heihoukon

subroutine ensyuritu()
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(real128), parameter :: pi = 4.0_real128*atan(1.0_real128)
    real(real128) :: r
    call ieee_set_rounding_mode(ieee_nearest)
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *) r
    print*, '\n答え'
    print*, r * r * pi
    print*, '\nEnterを押してください。'
    read *
end subroutine ensyuritu

subroutine syutyou()
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(real128), parameter :: pi = 4.0_real128*atan(1.0_real128)
    real(real128) :: r
    call ieee_set_rounding_mode(ieee_nearest)
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *) r
    print*, '\n答え'
    print*, 2*pi*r
    print*, '\nEnterを押してください。'
    read *
end subroutine syutyou

subroutine nizyou()
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(real128) :: x, y
    call ieee_set_rounding_mode(ieee_nearest)
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'べき乗する値を入力してください。'
    read (*, *) x
    print '(A)', 'n乗する値を入力してください。'
    read (*, *) y
    print*, '\n答え'
    print*, x**y
    print*, '\nEnterを押してください。'
    read *
end subroutine nizyou

subroutine game_1()
    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=10) d
    integer(int64) :: hero_hp, hero_mp, enemy1_hp, enemy1_mp, n, x
    integer(int64) :: mp = 0, y
    n = 0;x = 0
    hero_hp = 5;enemy1_hp = 10;
    hero_mp = 5;enemy1_mp = 10;
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print*, '\n超戦略ゲーム  ~ shit video game ~\n\n\nEnterを押してください。'
    read *
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    do
        if (hero_hp .le. 0) then
            print*, '\nGAME OVER'
            read *
            write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        else if (enemy1_hp .le. 0) then
            print '(A)', '\n敵を撲殺することが出来た。ワイの勝利！！！'
            print '(A)', 'Ураааааааааааааааа!'
            read *
            write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
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
        write (*,fmt='(A)', advance='no') ':'
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
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy1_hp = enemy1_hp - 2
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ2'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy1_hp = enemy1_hp - 3
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ3'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (4)
                enemy1_hp = enemy1_hp - 4
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ4'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (5)
                enemy1_hp = enemy1_hp - 5
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ5'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy1_hp = enemy1_hp - 0
                print '(A)', '\n敵のダメージ0'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
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
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy1_hp = enemy1_hp - 6
                hero_mp = hero_mp - 1
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ6'
                print '(A)', 'MP: 1消費'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy1_hp = enemy1_hp - 7
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ7'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy1_hp = enemy1_hp - 0
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ0'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '3') then
            mp = mp + 1
            n = add(1)
            select case(n)
            case (1)
                write (*,fmt='(A)', advance='no') '\n防御成功\n'
                write (*,fmt='(A)', advance='no') 'HP: 1回復\n'
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
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                print '(A)', '\n防御失敗'
                hero_hp = hero_hp - 1
                print '(A)', 'ワイのダメージ1'
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '4') then
            n = add(1)
            select case(n)
            case (1)
                print '(A)', '\n逃げ切れた'
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
                exit
            case default
                print '(A)', '\n逃走失敗'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else
            print '(A)', '\nそんなもんねぇよｗ'
            call sleep(1)
            write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        end if
    end do
    contains
    function add(n)
        implicit none
        integer(int32) :: add, rad, n
        integer(int32) :: seedsize = 3
        real(real32) :: y
        real(real32) :: x
        integer,allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        do
            call random_seed(get=seed)
            call random_number(x)
            y = x*10
            rad = int(y)
            if (rad .lt. n) exit
        end do
        add = rad
    end function
end subroutine game_1

subroutine game_2()
    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=10) d
    integer(int64) :: hero_hp, hero_mp, enemy2_hp, enemy2_mp, n, x
    integer(int64) :: mp = 0, y
    n = 0;x = 0
    hero_hp = 5;enemy2_hp = 15
    hero_mp = 5;enemy2_mp = 15
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print *, '\n超戦略ゲーム  ~ shit video game ~\n\n\nEnterを押してください。'
    read *
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    do
        if (hero_hp .le. 0) then
            print *, '\nGAME OVER'
            read *
            write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
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
        write (*,fmt='(A)', advance='no') ':'
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
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy2_hp = enemy2_hp - 2
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ2'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy2_hp = enemy2_hp - 3
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ3'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (4)
                enemy2_hp = enemy2_hp - 4
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ4'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (5)
                enemy2_hp = enemy2_hp - 5
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ5'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy2_hp = enemy2_hp - 0
                print '(A)', '\n敵のダメージ0'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
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
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy2_hp = enemy2_hp - 6
                hero_mp = hero_mp - 1
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ6'
                print '(A)', 'MP: 1消費'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy2_hp = enemy2_hp - 7
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ7'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy2_hp = enemy2_hp - 0
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ0'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
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
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                print '(A)', '\n防御失敗'
                hero_hp = hero_hp - 1
                print '(A)', 'ワイのダメージ1'
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '4') then
            n = add(1)
            select case(n)
            case (1)
                print '(A)', '\n逃げ切れた'
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
                exit
            case default
                print '(A)', '\n逃走失敗'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else
            print '(A)', '\nそんなもんねぇよｗ'
            call sleep(1)
            write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        end if
    end do
    contains
    function add(n)
        implicit none
        integer(int32) :: add, rad, n
        integer(int32) :: seedsize = 3
        real(real32) :: y
        real(real32) :: x
        integer,allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        do
            call random_seed(get=seed)
            call random_number(x)
            y = x*10
            rad = int(y)
            if (rad .lt. n) exit
        end do
        add = rad
    end function
end subroutine game_2

subroutine game_3()
    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=10) d
    integer(int64) :: hero_hp, hero_mp, enemy3_hp, enemy3_mp, n, x
    integer(int64) :: mp = 0, y
    n = 0;x = 0
    hero_hp = 5;enemy3_hp = 20
    hero_mp = 5;enemy3_mp = 20
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print*, '\n超戦略ゲーム  ~ shit video game ~\n\n\nEnterを押してください。'
    read *
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    do
        if (hero_hp .le. 0) then
            print*, '\nGAME OVER'
            read *
            write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        else if (enemy3_hp .le. 0) then
            print '(A)', '\n敵を撲殺することが出来た。ワイの勝利！！！'
            print '(A)', 'Ураааааааааааааааа!'
            read *
            write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        end if
        print '(A)', '\n敵が現れた！'
        print *, 'HP: ', enemy3_hp
        print *, 'MP: ', enemy3_mp
        print '(A)', '\nワイのステータス'
        print *, 'HP: ', hero_hp
        print *, 'MP: ', hero_mp
        print '(A)', '1:攻撃, 2:魔法, 3:防御, 4:逃げる'
        write(*, fmt='(A)', advance='no') ':'
        read (*, '(A)') d
        if (d .eq. '1') then
            n = add(5)
            select case(n)
            case (1)
                enemy3_hp = enemy3_hp - 1
                print '(A)', '\n敵のダメージ1'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy3_hp = enemy3_hp - 2
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ2'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy3_hp = enemy3_hp - 3
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ3'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (4)
                enemy3_hp = enemy3_hp - 4
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ4'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (5)
                enemy3_hp = enemy3_hp - 5
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ5'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy3_hp = enemy3_hp - 0
                print '(A)', '\n敵のダメージ0'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
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
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy3_hp = enemy3_hp - 6
                hero_mp = hero_mp - 1
                x = add(5)
                hero_hp = hero_hp - x
                print '(A)', '\n敵のダメージ6'
                print '(A)', 'MP: 1消費'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy3_hp = enemy3_hp - 7
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ7'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy3_hp = enemy3_hp - 0
                hero_mp = hero_mp - 1
                print '(A)', '\n敵のダメージ0'
                print '(A)', 'MP: 1消費'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
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
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                print '(A)', '\n防御失敗'
                hero_hp = hero_hp - 1
                print '(A)', 'ワイのダメージ1'
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '4') then
            n = add(9)
            select case(n)
            case (1)
                print '(A)', '\n逃げ切れた'
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
                exit
            case default
                print '(A)', '\n逃走失敗'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else
            print '(A)', '\nそんなもんねぇよｗ'
            call sleep(1)
            write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        end if
    end do
    contains
    function add(n)
        implicit none
        integer(int32) :: add, rad, n
        integer(int32) :: seedsize = 3
        real(real32) :: y
        real(real32) :: x
        integer,allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        do
            call random_seed(get=seed)
            call random_number(x)
            y = x*10
            rad = int(y)
            if (rad .lt. n) exit
        end do
        add = rad
    end function
end subroutine game_3

subroutine game()
    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int64) :: n
    n = add(9)
    select case(n)
    case (1)
        call game_1()
    case (2)
        call game_2()
    case default
        call game_3()
    end select
    contains
    function add(n)
        implicit none
        integer(int32) :: add, rad, n
        integer(int32) :: seedsize = 3
        real(int32) :: y
        real(int32) :: x
        integer,allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        do
            call random_seed(get=seed)
            call random_number(x)
            y = x*100
            rad = int(y)
            if (rad .lt. n) exit
        end do
        add = rad
    end function
end subroutine game

subroutine nizihoutei()
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: ieee_features, only:ieee_divide
    implicit none
    real(real128) :: a, b, c, k1, k2
    call ieee_set_rounding_mode(ieee_nearest)
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '一般: ax^2 + bx + c = 0\n'
    print '(A)', 'a値を入力してください。'
    read (*, *) a
    print '(A)', 'b値を入力してください。'
    read (*, *) b
    print '(A)', 'c値を入力してください。'
    read (*, *) c
    print '(A)', '\n答え'
    k1 = (-b+sqrt((b*b)-4*a*c)) / (2*a)
    k2 = (-b-sqrt((b*b)-4*a*c)) / (2*a)
    open (11, file='nizihoutei.txt', status='replace')
        write (11, *) k1
        write (11, *) k2
    close (11)
    print *, k1
    print *, k2
    print '(A)', '\nEnterを押してください。'
    read *
end subroutine nizihoutei

subroutine n_sin()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: n
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *) n
    print*, '\n答え'
    print*, sin(n)
    print '(A)', '\nEnterを押してください。'
    read *
end subroutine n_sin

subroutine n_cos()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: n
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *) n
    print '(A)', '\n答え'
    print*, cos(n)
    print*, '\nEnterを押してください。'
    read *
end subroutine n_cos

subroutine  n_tan()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: n
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *) n
    print*, '\n答え'
    print*, tan(n)
    print*, '\nEnterを押してください。'
    read *
end subroutine n_tan

subroutine n_asin()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: n
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *) n
    print*, '\n答え'
    print*, asin(n)
    print*, '\nEnterを押してください。'
    read *
end subroutine n_asin

subroutine n_acos()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: n
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *) n
    print*, '\n答え'
    print*, acos(n)
    print*, '\nEnterを押してください。'
    read *
end subroutine n_acos

subroutine n_atan()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: n
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *) n
    print*, '\n答え'
    print*, atan(n)
    print*, '\nEnterを押してください。'
    read *
end subroutine n_atan

subroutine n_atan2()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: x, y
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'y値を入力してください。'
    read (*, *) x
    print '(A)', 'x値を入力してください。'
    read (*, *) y
    print*, '\n答え'
    print*, atan2(y, x)
    print*, '\nEnterを押してください。'
    read *
end subroutine n_atan2

subroutine n_aimag()
    implicit none
    complex(kind=8) :: z
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '入力例: (2.71, 0.99)\n'
    print '(A)', '値を入力してください。'
    read (*, *) z
    print*, '\n答え'
    print*, aimag(z)
    print*, '\nEnterを押してください。'
    read *
end subroutine n_aimag

subroutine n_log10()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: n
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *) n
    print*, '\n答え'
    print*, log10(n)
    print*, '\nEnterを押してください。'
    read *
end subroutine n_log10

subroutine n_log()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: n
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値を入力してください。'
    read (*, *) n
    print*, '\n答え'
    print*, log(n)
    print*, '\nEnterを押してください。'
    read *
end subroutine n_log

subroutine mozuro
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: a, n
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '値aを入力してください。'
    read (*, *) a
    print '(A)', '値nを入力してください。'
    read (*, *) n
    print*, '\n答え'
    print*, mod(a, n)
    print*, '\nEnterを押してください。'
    read *
end subroutine mozuro

subroutine randsu()
    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int64) :: x, n
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'xを入力してください。(1～x)'
    read (*, *) x
    if (x .le. 0) then
        print '(A)', '\n1以上にしてください。\n'
        goto 11
    end if
    n = randon(x)
    print*, '\n出力'
    print*, n
11  print*, '\nEnterを押してください。'
    read *
    contains
    function randon(n)
        implicit none
        integer(int64) :: randon, rad, n
        integer(int32) :: seedsize = 3
        real(real128) :: y, x
        integer,allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        if (n .le. 1024) then
            do
                call random_seed(get=seed)
                call random_number(x)
                y = x*1024
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else if (n .le. 7812524) then
            do
                call random_seed(get=seed)
                call random_number(x)
                y = x*7812524
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else if (n .le. 15625024) then
            do
                call random_seed(get=seed)
                call random_number(x)
                y = x*15625024
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else if (n .le. 31250024) then
            do
                call random_seed(get=seed)
                call random_number(x)
                y = x*31250024
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else if (n .le. 62500024) then
            do
                call random_seed(get=seed)
                call random_number(x)
                y = x*62500024
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else if (n .le. 125000024) then
            do
                call random_seed(get=seed)
                call random_number(x)
                y = x*125000024
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else if (n .le. 250000024) then
            do
                call random_seed(get=seed)
                call random_number(x)
                y = x*250000024
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else if (n .le. 500000024) then
            do
                call random_seed(get=seed)
                call random_number(x)
                y = x*500000024
                rad = int(y)
                if (rad .lt. n) exit
            end do
        else
            do
                call random_seed(get=seed)
                call random_number(x)
                y = x*2147483624!1000000000024.
                rad = int(y)
                if (rad .lt. n) exit
            end do
        end if
        randon = rad
    end function
end subroutine randsu

subroutine neipia() ! e = lim n->Infinity [ (1+1/n)**n ] | Σn=0 ∞ [ 1/n! ]
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    implicit none
    integer(int64), parameter :: n = 1024
    integer(int64) :: a
    real(real128) :: b = 1.0_real128, e = 1.0_real128
    call ieee_set_rounding_mode(ieee_nearest)
    print '(A)', '\x1b[2J\x1b[3J\x1b[H'
    do a = 1, n
        b = b * a
        e = e + 1 / b
    end do
    print*, 'ネイピア数(総和は1から1024まで)'
    print*, e
    print*, '\n Wikipediaでは以下(上の桁数に合わせた)'
    print '(A)', '   2.71828182845904523536028747135266249'
    print*, '\nEnterを押してください。'
    read *
end subroutine neipia

subroutine y_zyoukon()
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(real128) :: x, y
    call ieee_set_rounding_mode(ieee_nearest)
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'n乗根のnの値を入力してください。()'
    read (*, *) y
    if (y .eq. 1) then
        print '(A)', '\n1は平方根や立方根にならねぇよ！'
        goto 19
    else if (y .eq. 2) then
        print '(A)', '\n2だと平方根になるのでpage_00に移動して&
        &5を押して下さい。'
        goto 19
    end if
    print '(A)', 'n乗根するx値を入力してください。'
    read (*, *) x
    print*, '答え'
    print*, x**(1/y)
19  print*, '\nEnterを押してください。'
    read *
end subroutine y_zyoukon

subroutine zettaiti()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: x
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *) x
    print*, '\n答え'
    print*, abs(x)
    print*, '\nEnterを押してください。'
    read *
end subroutine zettaiti

subroutine sisu()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: x
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *) x
    print*, '\n答え'
    print*, exp(x)
    print*, '\nEnterを押してください。'
    read *
end subroutine sisu

subroutine soukyokusin()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: x
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *) x
    print*, '\n答え'
    print*, sinh(x)
    print*, '\nEnterを押してください。'
    read *
end subroutine soukyokusin

subroutine soukyokucos()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: x
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *) x
    print*, '\n答え'
    print*, cosh(x)
    print*, '\nEnterを押してください。'
    read *
end subroutine soukyokucos

subroutine soukyokutan()
    use, intrinsic :: iso_fortran_env
    implicit none
    real(real128) :: x
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *) x
    print*, '\n答え'
    print*, tanh(x)
    print*, '\nEnterを押してください。'
    read *
end subroutine soukyokutan

subroutine gamma_f()
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    implicit none
    integer(int64), parameter :: n = 1024
    integer(int64) :: a
    real(real128), parameter :: pi = 4.0_real128*atan(1.0_real128)
    real(real128) :: z, gamma1, gamma2
    real(real128) :: b = 1.0_real128, e = 1.0_real128
    call ieee_set_rounding_mode(ieee_nearest)
    print '(A)', '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)',  '値を入力してください。'
    read (*, *) z
    do a = 1, n
        b = b * a
        e = e + 1 / b
    end do
    gamma1 = sqrt(2*pi/z)
    gamma2 = (z/e)**z
    print*, '\n答え'
    print*, gamma1 * gamma2
    print*, '\nEnterを押してください。'
    read *
end subroutine gamma_f

subroutine joke()
    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int64) :: x, n
    n = 5
24  x = randon(n)
    select case(x)
    case (0)
        print*, '\n  women = time * money\n'
        print '(A)', 'Women are the product of time and money.'
        read *
    case (1)
        print*, '\n  time = money\n'
        print '(A)', 'Time is money.'
        read *
    case (2)
        print*, '\n  women = money^2\n'
        print '(A)', 'So women are money squared.'
        read *
    case (3)
        print*, '\n  money = √evil\n'
        print '(A)', 'Money is the root of all evil.'
        read *
    case (4)
        print*, '\n  women = (√evil)^2 = evil\n'
        print '(A)', 'So women are evil.'
        read *
    case default
        goto 24
    end select
    contains
    function randon(n)
        implicit none
        integer(int64) :: randon, rad, n
        integer(int32) :: seedsize = 3
        real(real128) :: y, x
        integer,allocatable :: seed(:)
        call random_seed(size=seedsize)
        allocate(seed(seedsize))
        if (n .le. 10) then
            do
                call random_seed(get=seed)
                call random_number(x)
                y = x*10
                rad = int(y)
                if (rad .lt. n) exit
            end do
        end if
        randon = rad
    end function
end subroutine joke

subroutine undouhouteisiki()
    !$ use omp_lib
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    use, intrinsic :: ieee_features, only:ieee_sqrt
    implicit none
    integer(int64) :: i
    real(real128), parameter :: pi = 4.0_real128*atan(1.0_real128)
    real(real128) :: g, V, angle, theta, x, z, u, w&
    &, dxdt, dzdt, dudt, dwdt
    !$ double precision st, en
    call ieee_set_rounding_mode(ieee_nearest)
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '初期速度 [m/s]'
    read (*, *) V
    print '(A)', '仰角 [deg]'
    read (*, *) angle

    g = 9.806

    theta = pi / 180.0 * angle

    x = 0.0_real128
    z = 0.0_real128
    u = V * cos(theta)
    w = V * sin(theta)

    open(11, file='output.txt', status='replace')

    write(11, *) x, z
    !$ st = omp_get_wtime()
    !$omp parallel num_threads(16)
    !$omp do
    do i = 1, 60000 ! 一分間だから60000 * 0.001
        !$omp critical
        dxdt = u
        dzdt = w
        dudt = 0.0
        dwdt = -g

        x = x + 0.001 * dxdt
        z = z + 0.001 * dzdt
        u = u + 0.001 * dudt
        w = w + 0.001 * dwdt
        !$omp end critical

        print*, x, z
        write(11, *) x, z
    end do
    !$omp end do
    !$omp end parallel
    !$ en = omp_get_wtime()

    close(11)

    !$ print *, "Elapsed time :", en-st

    print*, '\nEnterを押してください。'
    read *
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
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(real128), parameter :: pi = 4.0_real128*atan(1.0_real128)
    real(real128) :: g, V, angle, theta, T, X
    call ieee_set_rounding_mode(ieee_nearest)
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '初期速度 [m/s]'
    read (*, *) V
    print '(A)', '仰角 [deg]'
    read (*, *) angle

    g = 9.806

    theta = pi / 180.0 * angle

    T = 2.0 * V * sin(theta) / g
    X = V * V * sin(2.0 * theta)

    print*, '\n滞空時間'
    print*, T
    print*, '\n飛距離'
    print*, X
    print*, '\nEnterを押してください。'
    read *
end subroutine TX

subroutine ensyu()
    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int64), parameter :: vmax = 809600, bmax = 102912
    integer(int64) :: vect(vmax) = 2, buffer(bmax)
    integer(int64) :: carry, n, L, k, more = 0, num
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', 'ちょっと待っててね\n終わったあとは、メモ帳を&
    &大画面にした方がええで\n'
    do n = 1,  bmax!buffer()
        carry = 0
        do L = vmax, 1, -1 !vect()
            num = 100000 * vect(L) + carry * L
            carry = num / (2*L - 1)
            vect(L) = num - carry * (2*L - 1)
        end do
        k = carry / 100000
        buffer(n) = more + k
        more = carry - k * 100000
    end do
    open(11, file="pi.txt", status="replace")
        write(11, "(1x, I1, '.'/(1x, 32I5.5))") buffer
    close(11)
    write(*, "(1x, I1, '.'/(1x, 12I5.5))") buffer
    print*, '\nEnterを押してください。'
    read *
end subroutine ensyu

subroutine heikin()
    use, intrinsic :: iso_fortran_env
    implicit none
    integer(int64) :: i, max
    real(real128) :: x(1024), y = 0.0_real128
    write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print '(A)', '観測値を入力してください。'
    read (*, *) max
    print*, ''
    do i = 1, max
        print '(i0, "つ目の値を入力してください。")', i
        read (*, *) x(i)
        y = y + x(i)
    end do
    print*, '\n答え'
    print*, y / max
    print*, '\nEnterを押してください。'
    read *
end subroutine heikin

subroutine page_02()
    implicit none
    character(len=256) :: str
    do
        write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        print '(A)', '\n-----------------------------------------'
        print*, '1 Γ(z) ~ √2π/z(z/e)^z'
        print*, '2 滞空時間と飛距離'
        print*, '3 運動方程式(一分間での放物運動)'
        print*, '4 運動方程式のグラフをみる(gnuplot)'
        print*, '5 円周率をtxtファイルで出力(桁数多め)'
        print*, '6 平均値'
        print*, '11 ジョーク\n'
        print*, '99 終了           01 Back'
        print '(A)', '-----------------------------------------'
        write (*,fmt='(A)', advance='no') ': '
        read (*, '(A)') str
        select case(str)
        case ('1')
            call gamma_f()
        case ('3')
            call undouhouteisiki()
        case ('4')
            write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
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
        case ('11')
            call joke()
        case ('00')
            call page_00()
        case ('01')
            !call page_01()
            write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        case ('99')
            write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            stop
        case default
            print*, 'そんなもんねぇよｗ'
            call sleep(1)
        end select
    end do
end subroutine page_02

subroutine page_01()
    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=256) :: str
    real(real128), parameter :: fai = (1.0_real128+sqrt(5.0_real128))*0.5_real128
    do
        write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        print '(A)', '\n-----------------------------------------'
        print*, '1 モジュロ演算(a mod n)'
        print*, '2 乱数(1～x)'
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
        write (*,fmt='(A)', advance='no') ': '
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
            print '(A)', '\n制作者:ware255(われ)\n\n???ってなんだろ&
            &う、って思ったでしょｗ\n思っちゃったやつソースコード見てね&
            &ぇって分かっちまうから気お付けろよｗ'
            read *
            exit
        case ('00')
            !call page_00()
            write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        case ('99')
            write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            stop
        case ('02')
            call page_02()
        case default
            print*, 'そんなもんねぇよｗ'
            call sleep(1)
        end select
    end do
end subroutine page_01

subroutine page_00()
    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=256) :: str
    real(real128), parameter :: PI = 4.0_real128*atan(1.0_real128)
    do
        write (*,fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
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
        print*, '10 2次方程式      21 自然対数(log = ln)'
        print*, '11 超戦略ゲーム\n'
        print*, '99 終了           01 Next_page'
        print '(A)', '-----------------------------------------'
        write (*,fmt='(A)', advance='no') ': '
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
            write(*, fmt='(A)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            stop
        case ('01')
            call page_01()
        case ('02')
            call page_02()
        case default
            print*, 'そんなもんねぇよｗ'
            call sleep(1)
        end select
    end do
end subroutine page_00

program calculator
    !$ use omp_lib
    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=256) :: str
    integer(int64) :: i
    real(real128) :: s = 0.0_real128
    !$ real(real64) :: time_begin_s,time_end_s
    if (.not. getuid() .eq. 0) then !権限なし
        call getarg(1, str)
        if (iargc() .eq. 0) then
            call page_00()
        else if (str .eq. 'page_00') then
            call page_00()
        else if (str .eq. 'page_01') then
            call page_01()
        else if (str .eq. 'page_02') then
            call page_02()
        else if (str .eq. 'benchmark') then
            print '(A)', '\n計算中です。\n'!(約2分かかります。)
            !$ time_begin_s = omp_get_wtime()
            !$omp parallel num_threads(32)
            !$omp do
            do i = 0, 10**8!12
                !$omp critical
                s = s + (-1.0_real64)**i / (2.0_real64 * i + 1.0_real64)
                !$omp end critical
            end do
            !$omp end do
            !$omp end parallel
            !$ time_end_s = omp_get_wtime()
            print*, 'Answer:', s * 4
            !$ print '(A, F13.5, A)', '\ntime:', time_end_s - time_begin_s, ' [sec]\n'
        else
            print '(A)', '\nこの引数はありません。\n'
        end if
    else                            !root権限
        print '(A)', '\n※いつでもどこでも電卓が使えるようにして\n&
        &　いるためroot権限は実装しておりません。\n'
    end if
end program calculator
