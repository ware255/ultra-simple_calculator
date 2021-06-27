# ultra-simple_calculator
これは、Fortranで書かれた超簡易計算機です。
四則演算と平方根を見ることしか出来ません。

2021 6/27:円周率の計算が出来るようにしました。

```
Fortranコンパイラーインストール
$ sudo apt install gfortran
コンパイル
$ gfortran -Ofast -march=native -fbackslash calculator.f90 -o calculator
実行
$ ./calculator
```
