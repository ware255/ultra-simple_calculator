# ultra-simple_calculator
これは、Fortranで書かれた超簡易計算機です。
四則演算と平方根を見ることしか出来ません。
<br />
2021 7/03　円周率の計算(πr^2)から円の面積(πr^2)ってな感じでちょっと変更した。

2021 6/27　円周率の計算が出来るようにしました。

```
Fortranコンパイラーインストール
$ sudo apt install gfortran
コンパイル
$ gfortran -Ofast -march=native -fbackslash calculator.f90 -o calculator
実行
$ ./calculator
```
