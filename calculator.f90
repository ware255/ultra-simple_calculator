subroutine tasizan()
    real(8) :: x = 0
    real(8) :: y = 0
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
    real(8) :: x = 0
    real(8) :: y = 0
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
    real(8) :: x = 0
    real(8) :: y = 0
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
    real(8) :: x = 0
    real(8) :: y = 0
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
    double precision :: x = 0
    integer i, v1, v2
    v1 = 2;v2 = 3
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    write (*,fmt='(a)', advance='no') '値を入力してください。\n'
    read *, x
    print*, '\n近似値'
    print*, dsqrt(x)
    i = int(x)
    if (i .eq. v1) then
        print*, '　一夜一夜に月見ごろ   <= 覚え方'
    else if (i .eq. v2) then
        print*, '　人並みにおごれや     <= 覚え方'
    end if
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine heihoukon

program calculator
    integer :: i = 0
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    do
        print*, '\n------------------------'
        write (*,fmt='(a)', advance='no') '何の計算をしますか？\n'
        print*, '1 足し算'
        print*, '2 引き算'
        print*, '3 掛け算'
        print*, '4 割り算'
        print*, '5 平方根\n'
        print*, '99 終了'
        print*, '\n------------------------'
        read *, i
        select case(i)
        case (1)
            call tasizan()
        case (2)
            call hikizan()
        case (3)
            call kakezan()
        case (4)
            call warizan()
        case (5)
            call heihoukon()
        case (99)
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        case default
            print*, 'そんなもんねぇよｗ'
            read *
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        end select
    end do
end program calculator
