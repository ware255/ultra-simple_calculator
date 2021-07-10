subroutine tasizan()
    implicit none
    real(kind=16) :: x = 0
    real(kind=16) :: y = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, x
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, y
    print*, '\n答え'
    print*, x + y
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine tasizan

subroutine hikizan()
    implicit none
    real(kind=16) :: x = 0
    real(kind=16) :: y = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, x
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, y
    print*, '\n答え'
    print*, x - y
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine hikizan

subroutine kakezan()
    implicit none
    real(kind=16) :: x = 0
    real(kind=16) :: y = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, x
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, y
    print*, '\n答え'
    print*, x * y
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine kakezan

subroutine warizan()
    implicit none
    real(kind=16) :: x = 0
    real(kind=16) :: y = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, x
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, y
    print*, '\n答え'
    print*, x / y
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine warizan

subroutine heihoukon()
    implicit none
    real(kind=16) :: x = 0
    integer(kind=8) :: i = 0
    integer(kind=4) :: v1, v2
    v1 = 2;v2 = 3
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, x
    print*, '\n近似値'
    print*, sqrt(x)
    i = int(x)
    if (i .eq. v1) then
        print*, '　一夜一夜に月見ごろ          <= 覚え方'
    else if (i .eq. v2) then
        print*, '　人並みにおごれや            <= 覚え方'
    end if
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine heihoukon

subroutine ensyuritu()
    implicit none
    real(kind=16), parameter :: pi = 4.0*atan(1.0)!3.1415926535897932384626433832795028
    real(kind=16) :: r = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, r
    print*, '\n答え'
    print*, r**2 * pi
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine ensyuritu

subroutine syutyou()
    implicit none
    real(kind=16), parameter :: pi = 4.0*atan(1.0)!3.1415926535897932384626433832795028
    real(kind=16) :: r = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, r
    print*, '\n答え'
    print*, 2*pi*r
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine syutyou

subroutine nizyou()
    implicit none
    real(kind=16) :: x = 0
    real(kind=16) :: y = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') 'べき乗する値を入力してください。\n'
    read *, x
    write (*,fmt='(a)', advance='no') 'n乗する値を入力してください。\n'
    read *, y
    print*, '\n答え'
    print*, x ** y
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine nizyou

subroutine game_1()
    implicit none
    character(len=10) d
    integer(kind=8) :: hero_hp, hero_mp, enemy1_hp, enemy1_mp, n, x
    integer(kind=8) :: mp = 0
    n = 0;x = 0
    hero_hp = 5;enemy1_hp = 10;
    hero_mp = 5;enemy1_mp = 10;
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print*, '\n超戦略ゲーム  ~ shit video game ~\n\n\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    do
        if (hero_hp .le. 0) then
            print*, '\nGAME OVER'
            read *
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        else if (enemy1_hp .le. 0) then
            print*, '敵を撲殺することが出来た。ワイの勝利！！！'
            read *
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        end if
        print*, ''
        print*, '敵が現れた！'
        print*, 'HP: ', enemy1_hp
        print*, 'MP: ', enemy1_mp
        print*, ''
        print*, '\nワイのステータス'
        print*, 'HP: ', hero_hp
        print*, 'MP: ', hero_mp
        print*, '1:攻撃, 2:魔法, 3:防御, 4:逃げる'
        write (*,fmt='(a)', advance='no') '\n:'
        read (*, '(a)') d
        if (d .eq. '1') then
            n = add(5)
            select case(n)
            case (1)
                enemy1_hp = enemy1_hp - 1
                write (*,fmt='(a)', advance='no') '\n敵のダメージ1\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy1_hp = enemy1_hp - 2
                x = add(5)
                hero_hp = hero_hp - x
                write (*,fmt='(a)', advance='no') '\n敵のダメージ2\n'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy1_hp = enemy1_hp - 3
                x = add(5)
                hero_hp = hero_hp - x
                write (*,fmt='(a)', advance='no') '\n敵のダメージ3\n'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (4)
                enemy1_hp = enemy1_hp - 4
                x = add(5)
                hero_hp = hero_hp - x
                write (*,fmt='(a)', advance='no') '\n敵のダメージ4\n'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (5)
                enemy1_hp = enemy1_hp - 5
                x = add(5)
                hero_hp = hero_hp - x
                write (*,fmt='(a)', advance='no') '\n敵のダメージ5\n'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy1_hp = enemy1_hp - 0
                write (*,fmt='(a)', advance='no') '\n敵のダメージ0\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '2') then
            n = add(3)
            select case(n)
            case (1)
                enemy1_hp = enemy1_hp - 5
                hero_mp = hero_mp - 1
                write (*,fmt='(a)', advance='no') '\n敵のダメージ5\n'
                write (*,fmt='(a)', advance='no') 'MP: 1消費\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy1_hp = enemy1_hp - 6
                hero_mp = hero_mp - 1
                x = add(5)
                hero_hp = hero_hp - x
                write (*,fmt='(a)', advance='no') '\n敵のダメージ6\n'
                write (*,fmt='(a)', advance='no') 'MP: 1消費\n'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy1_hp = enemy1_hp - 7
                hero_mp = hero_mp - 1
                write (*,fmt='(a)', advance='no') '\n敵のダメージ7\n'
                write (*,fmt='(a)', advance='no') 'MP: 1消費\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy1_hp = enemy1_hp - 0
                hero_mp = hero_mp - 1
                write (*,fmt='(a)', advance='no') '\n敵のダメージ0\n'
                write (*,fmt='(a)', advance='no') 'MP: 1消費\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '3') then
            mp = mp + 1
            n = add(1)
            select case(n)
            case (1)
                write (*,fmt='(a)', advance='no') '\n防御成功\n'
                write (*,fmt='(a)', advance='no') 'HP: 1回復\n'
                if (mp .eq. 5 .or. mp .eq. 10 .or. mp .eq. 15 .or. mp .eq. 20 .or. mp .eq. 25 .or. mp .eq. 30) then
                    write (*,fmt='(a)', advance='no') 'MP: 1回復\n'
                    hero_mp = hero_mp + 1
                end if
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                write (*,fmt='(a)', advance='no') '\n防御失敗\n'
                hero_hp = hero_hp - 1
                write (*,fmt='(a)', advance='no') 'ワイのダメージ1\n'
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '4') then
            n = add(1)
            select case(n)
            case (0)
                write (*,fmt='(a)', advance='no') '\n逃げ切れた\n'
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
                exit
            case default
                write (*,fmt='(a)', advance='no') '\n逃走失敗\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else
            write (*,fmt='(a)', advance='no') '\nそんなもんねぇよｗ\n'
            call sleep(1)
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        end if
    end do
    contains
    function add(n)
        implicit none
        integer(kind=4) :: add, rad, n
        integer(kind=4) :: seedsize = 3
        real(kind=16) :: y
        real(kind=16) :: x
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
    implicit none
    character(len=10) d
    integer(kind=8) :: hero_hp, hero_mp, enemy2_hp, enemy2_mp, n, x
    integer(kind=8) :: mp = 0
    n = 0;x = 0
    hero_hp = 5;enemy2_hp = 15
    hero_mp = 5;enemy2_mp = 15
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print*, '\n超戦略ゲーム  ~ shit video game ~\n\n\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    do
        if (hero_hp .le. 0) then
            print*, '\nGAME OVER'
            read *
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        else if (enemy2_hp .le. 0) then
            print*, '敵を撲殺することが出来た。ワイの勝利！！！'
            read *
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        end if
        print*, ''
        print*, '敵が現れた！'
        print*, 'HP: ', enemy2_hp
        print*, 'MP: ', enemy2_mp
        print*, ''
        print*, '\nワイのステータス'
        print*, 'HP: ', hero_hp
        print*, 'MP: ', hero_mp
        print*, '1:攻撃, 2:魔法, 3:防御, 4:逃げる'
        write (*,fmt='(a)', advance='no') '\n:'
        read (*, '(a)') d
        if (d .eq. '1') then
            n = add(5)
            select case(n)
            case (1)
                enemy2_hp = enemy2_hp - 1
                write (*,fmt='(a)', advance='no') '\n敵のダメージ1\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy2_hp = enemy2_hp - 2
                x = add(5)
                hero_hp = hero_hp - x
                write (*,fmt='(a)', advance='no') '\n敵のダメージ2\n'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy2_hp = enemy2_hp - 3
                x = add(5)
                hero_hp = hero_hp - x
                write (*,fmt='(a)', advance='no') '\n敵のダメージ3\n'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (4)
                enemy2_hp = enemy2_hp - 4
                x = add(5)
                hero_hp = hero_hp - x
                write (*,fmt='(a)', advance='no') '\n敵のダメージ4\n'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (5)
                enemy2_hp = enemy2_hp - 5
                x = add(5)
                hero_hp = hero_hp - x
                write (*,fmt='(a)', advance='no') '\n敵のダメージ5\n'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy2_hp = enemy2_hp - 0
                write (*,fmt='(a)', advance='no') '\n敵のダメージ0\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '2') then
            n = add(3)
            select case(n)
            case (1)
                enemy2_hp = enemy2_hp - 5
                hero_mp = hero_mp - 1
                write (*,fmt='(a)', advance='no') '\n敵のダメージ5\n'
                write (*,fmt='(a)', advance='no') 'MP: 1消費\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy2_hp = enemy2_hp - 6
                hero_mp = hero_mp - 1
                x = add(5)
                hero_hp = hero_hp - x
                write (*,fmt='(a)', advance='no') '\n敵のダメージ6\n'
                write (*,fmt='(a)', advance='no') 'MP: 1消費\n'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy2_hp = enemy2_hp - 7
                hero_mp = hero_mp - 1
                write (*,fmt='(a)', advance='no') '\n敵のダメージ7\n'
                write (*,fmt='(a)', advance='no') 'MP: 1消費\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy2_hp = enemy2_hp - 0
                hero_mp = hero_mp - 1
                write (*,fmt='(a)', advance='no') '\n敵のダメージ0\n'
                write (*,fmt='(a)', advance='no') 'MP: 1消費\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '3') then
            mp = mp + 1
            n = add(1)
            select case(n)
            case (1)
                write (*,fmt='(a)', advance='no') '\n防御成功\n'
                write (*,fmt='(a)', advance='no') 'HP: 1回復\n'
                if (mp .eq. 5 .or. mp .eq. 10 .or. mp .eq. 15 .or. mp .eq. 20 .or. mp .eq. 25 .or. mp .eq. 30) then
                    write (*,fmt='(a)', advance='no') 'MP: 1回復\n'
                    hero_mp = hero_mp + 1
                end if
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                write (*,fmt='(a)', advance='no') '\n防御失敗\n'
                hero_hp = hero_hp - 1
                write (*,fmt='(a)', advance='no') 'ワイのダメージ1\n'
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '4') then
            n = add(1)
            select case(n)
            case (0)
                write (*,fmt='(a)', advance='no') '\n逃げ切れた\n'
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
                exit
            case default
                write (*,fmt='(a)', advance='no') '\n逃走失敗\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else
            write (*,fmt='(a)', advance='no') '\nそんなもんねぇよｗ\n'
            call sleep(1)
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        end if
    end do
    contains
    function add(n)
        implicit none
        integer(kind=4) :: add, rad, n
        integer(kind=4) :: seedsize = 3
        real(kind=16) :: y
        real(kind=16) :: x
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
    implicit none
    character(len=10) d
    integer(kind=8) :: hero_hp, hero_mp, enemy3_hp, enemy3_mp, n, x
    integer(kind=8) :: mp = 0
    n = 0;x = 0
    hero_hp = 5;enemy3_hp = 20
    hero_mp = 5;enemy3_mp = 20
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    print*, '\n超戦略ゲーム  ~ shit video game ~\n\n\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    do
        if (hero_hp .le. 0) then
            print*, '\nGAME OVER'
            read *
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        else if (enemy3_hp .le. 0) then
            print*, '敵を撲殺することが出来た。ワイの勝利！！！'
            read *
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        end if
        print*, ''
        print*, '敵が現れた！'
        print*, 'HP: ', enemy3_hp
        print*, 'MP: ', enemy3_mp
        print*, ''
        print*, '\nワイのステータス'
        print*, 'HP: ', hero_hp
        print*, 'MP: ', hero_mp
        print*, '1:攻撃, 2:魔法, 3:防御, 4:逃げる'
        write (*,fmt='(a)', advance='no') '\n:'
        read (*, '(a)') d
        if (d .eq. '1') then
            n = add(5)
            select case(n)
            case (1)
                enemy3_hp = enemy3_hp - 1
                write (*,fmt='(a)', advance='no') '\n敵のダメージ1\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy3_hp = enemy3_hp - 2
                x = add(5)
                hero_hp = hero_hp - x
                write (*,fmt='(a)', advance='no') '\n敵のダメージ2\n'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy3_hp = enemy3_hp - 3
                x = add(5)
                hero_hp = hero_hp - x
                write (*,fmt='(a)', advance='no') '\n敵のダメージ3\n'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (4)
                enemy3_hp = enemy3_hp - 4
                x = add(5)
                hero_hp = hero_hp - x
                write (*,fmt='(a)', advance='no') '\n敵のダメージ4\n'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (5)
                enemy3_hp = enemy3_hp - 5
                x = add(5)
                hero_hp = hero_hp - x
                write (*,fmt='(a)', advance='no') '\n敵のダメージ5\n'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy3_hp = enemy3_hp - 0
                write (*,fmt='(a)', advance='no') '\n敵のダメージ0\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '2') then
            n = add(3)
            select case(n)
            case (1)
                enemy3_hp = enemy3_hp - 5
                hero_mp = hero_mp - 1
                write (*,fmt='(a)', advance='no') '\n敵のダメージ5\n'
                write (*,fmt='(a)', advance='no') 'MP: 1消費\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (2)
                enemy3_hp = enemy3_hp - 6
                hero_mp = hero_mp - 1
                x = add(5)
                hero_hp = hero_hp - x
                write (*,fmt='(a)', advance='no') '\n敵のダメージ6\n'
                write (*,fmt='(a)', advance='no') 'MP: 1消費\n'
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case (3)
                enemy3_hp = enemy3_hp - 7
                hero_mp = hero_mp - 1
                write (*,fmt='(a)', advance='no') '\n敵のダメージ7\n'
                write (*,fmt='(a)', advance='no') 'MP: 1消費\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                enemy3_hp = enemy3_hp - 0
                hero_mp = hero_mp - 1
                write (*,fmt='(a)', advance='no') '\n敵のダメージ0\n'
                write (*,fmt='(a)', advance='no') 'MP: 1消費\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '3') then
            mp = mp + 1
            n = add(1)
            select case(n)
            case (1)
                write (*,fmt='(a)', advance='no') '\n防御成功\n'
                write (*,fmt='(a)', advance='no') 'HP: 1回復\n'
                if (mp .eq. 5 .or. mp .eq. 10 .or. mp .eq. 15 .or. mp .eq. 20 .or. mp .eq. 25 .or. mp .eq. 30) then
                    write (*,fmt='(a)', advance='no') 'MP: 1回復\n'
                    hero_mp = hero_mp + 1
                end if
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            case default
                write (*,fmt='(a)', advance='no') '\n防御失敗\n'
                hero_hp = hero_hp - 1
                write (*,fmt='(a)', advance='no') 'ワイのダメージ1\n'
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else if (d .eq. '4') then
            n = add(9)
            select case(n)
            case (0)
                write (*,fmt='(a)', advance='no') '\n逃げ切れた\n'
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
                exit
            case default
                write (*,fmt='(a)', advance='no') '\n逃走失敗\n'
                x = add(5)
                hero_hp = hero_hp - x
                print "('ワイのダメージ',i0)", x
                read *
                write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            end select
        else
            write (*,fmt='(a)', advance='no') '\nそんなもんねぇよｗ\n'
            call sleep(1)
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        end if
    end do
    contains
    function add(n)
        implicit none
        integer(kind=4) :: add, rad, n
        integer(kind=4) :: seedsize = 3
        real(kind=16) :: y
        real(kind=16) :: x
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
    implicit none
    integer(kind=8) :: n
    n = add(9)
    select case(n)
    case (1)
        call game_1()
    case (2)
        call game_2()
    case default
        call game_3()
    end select
    contains !乱数
    function add(n)
        implicit none
        integer(kind=4) :: add, rad, n
        integer(kind=4) :: seedsize = 3
        real(kind=16) :: y
        real(kind=16) :: x
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
end subroutine game

subroutine nizihoutei()
    implicit none
    real(kind=16) :: a, b, c, bac, kai1, kai2 = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '公式: ax^2 * bx * c = 0\n\n'
    write (*,fmt='(a)', advance='no') 'a値を入力してください。\n'
    read *, a
    write (*,fmt='(a)', advance='no') 'b値を入力してください。\n'
    read *, b
    write (*,fmt='(a)', advance='no') 'c値を入力してください。\n'
    read *, c
    print*, '\n答え'
    bac = b*b-4*a*c
    kai1 = (-b+sqrt(bac)) / (2*a)
    kai2 = (-b-sqrt(bac)) / (2*a)
    print*, kai1
    print*, kai2
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine nizihoutei

subroutine n_sin()
    implicit none
    real(kind=16) :: n = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, n
    print*, '\n答え'
    print*, sin(n)
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine n_sin

subroutine n_cos()
    implicit none
    real(kind=16) :: n = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, n
    print*, '\n答え'
    print*, cos(n)
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine n_cos

subroutine  n_tan()
    implicit none
    real(kind=16) :: n = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, n
    print*, '\n答え'
    print*, tan(n)
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine n_tan

subroutine n_asin()
    implicit none
    real(kind=16) :: n = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, n
    print*, '\n答え'
    print*, asin(n)
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine n_asin

subroutine n_acos()
    implicit none
    real(kind=16) :: n = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, n
    print*, '\n答え'
    print*, acos(n)
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine n_acos

subroutine n_atan()
    implicit none
    real(kind=16) :: n = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, n
    print*, '\n答え'
    print*, atan(n)
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine n_atan

subroutine n_atan2()
    implicit none
    real(kind=16) :: x = 0
    real(kind=16) :: y = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') 'y値を入力してください。\n'
    read (*, *) x
    write (*,fmt='(a)', advance='no') 'x値を入力してください。\n'
    read (*, *) y
    print*, '\n答え'
    print*, atan2(y, x)
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine n_atan2

subroutine n_aimag()
    implicit none
    complex(kind=8)  :: z
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '入力例: (2.71, 0.99)\n\n'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read (*, *) z
    print*, '\n答え'
    print*, aimag(z)
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine n_aimag

subroutine n_log10()
    implicit none
    real(kind=16) :: n = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read (*, *) n
    print*, '\n答え'
    print*, log10(n)
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine n_log10

subroutine n_log()
    implicit none
    real(kind=16) :: n = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read (*, *) n
    print*, '\n答え'
    print*, log(n)
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine n_log

program calculator
    use, intrinsic :: iso_fortran_env
    implicit none
    character(len=256) :: str
    !integer(kind=8) :: n = 0
    !real(kind=16) :: x = 0
    real(real128), parameter :: PI = 3.14159265358979323846264338327950288_real128
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    do
        print*, '\n-----------------------------------------'
        !write (*,fmt='(a)', advance='no') '何の計算をしますか？\n'
        print*, '1 足し算          12 三角関数(sin)'
        print*, '2 引き算          13 三角関数(cos)'
        print*, '3 掛け算          14 三角関数(tan)'
        print*, '4 割り算          15 逆三角関数(asin)'
        print*, '5 平方根          16 逆三角関数(acos)'
        print*, '6 円周率          17 逆三角関数(atan)'
        print*, '7 円の面積        18 逆三角関数(atan2(y,x))'
        print*, '8 円の周長        19 虚数部(z = (x, iy))'
        print*, '9 べき乗          20 常用対数(log10)'
        print*, '10 2次方程式      21 自然対数(log)'
        print*, '11 超戦略ゲーム\n'
        print*, '99 終了'
        write (*,fmt='(a)', advance='no') '-----------------------------------------\n'
        write(*,fmt='(a)', advance='no') ': '
        read (*, '(a)') str
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
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            print*, '\n円周率'
            print '(2F40.36)', 2.0_real128*asin(1.0_real128)
            print*, '\nEnterを押してください。'
            read *
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
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
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        case default
            print*, 'そんなもんねぇよｗ'
            call sleep(1)
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        end select
    end do
end program calculator
