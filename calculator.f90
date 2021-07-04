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
    real(kind=16) x
    integer(kind=8) i
    integer v1, v2
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
    real(kind=16), parameter :: pi = 3.1415926535897932384626433832795028
    real(kind=16) r
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
    real(kind=16), parameter :: pi = 3.1415926535897932384626433832795028
    real(kind=16) r
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
    write (*,fmt='(a)', advance='no') '二乗する値を入力してください。\n'
    read *, x
    write (*,fmt='(a)', advance='no') 'n乗する値を入力してください。\n'
    read *, y
    print*, '\n答え'
    print*, x ** y
    print*, '\nEnterを押してください。'
    read *
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
end subroutine nizyou

program calculator
    use, intrinsic :: iso_fortran_env
    implicit none
    integer(kind=8) :: n = 0
    real(kind=16) x
    real(real128), parameter :: PI = 3.14159265358979323846264338327950288_real128
    write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
    do
        print*, '\n------------------------'
        write (*,fmt='(a)', advance='no') '何の計算をしますか？\n'
        print*, '1 足し算'
        print*, '2 引き算'
        print*, '3 掛け算'
        print*, '4 割り算'
        print*, '5 平方根'
        print*, '6 円周率(π)'
        print*, '7 円の面積(πr^2)'
        print*, '8 円の周長(2πr)'
        print*, '9 べき乗'
        print*, '99 終了'
        print*, '\n------------------------'
        read *, x
        n = int(x)
        select case(n)
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
        case (6)
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            print*, '\n円周率'
            print '(2F40.36)', 2.0_real128*asin( 1.0_real128)
            print*, '\nEnterを押してください。'
            read *
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        case (7)
            call ensyuritu()
        case (8)
            call syutyou()
        case (9)
            call nizyou()
        case (99)
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
            exit
        case default
            print*, 'そんなもんねぇよｗ'
            call sleep(1)
            write (*,fmt='(a)', advance='no') '\x1b[2J\x1b[3J\x1b[H'
        end select
    end do
end program calculator
