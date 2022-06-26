# Ultra-Simple_Calculator
これは、Fortranで書かれた超簡易計算機です。<br>
CUIで機能します。「GUIの方がいい」と欲張らないでください。運動方程式や特殊な計算はEuler法を採用しています。なので精度は極端にいいやつと悪いやつがあります。<br>
おまけで、ちょっとしたゲームをプレイすることができます。<br><br>
## インストール
     依存関係などのインストール
     $ sudo apt install gfortran gnuplot
     $ git clone https://github.com/ware255/ultra-simple_calculator
     移動
     $ cd ultra-simple_calculator
     コンパイル(gfortranの場合)
     $ gfortran -Ofast -fopenmp -fbackslash calculator.f90 -o calculator
     コンパイル(ifortの場合)
     $ ifort -assume bscc -Ofast -qopenmp calculator.f90 -o calculator
     実行
     $ ./calculator
## 使用方法
     通常
     $ ./calculator
     
     オプションがわからないとき
     $ ./calculator help
     
     ./calculatorと実行した時と同じ
     $ ./calculator page_00
     
     page_00の次のページ
     $ ./calculator page_01
     
     page_01の次のページ
     $ ./calculator page_02
     
     page_02の次のページ
     $ ./calculator page_03
     
     PCスペックをテストします。
     $ ./calculator benchmark
     
     超戦略ゲームでのレベル
     $ ./calculator level

     現在の時刻を表示
     $ ./calculator time

     簡易RSA暗号
     $ ./calculator rsa
## 動作確認済みOS
     ・WSL Debian (64bit)
     ・Linux Mint (32bit)
     ・その他Linux
※Windowsではエスケープシーケンスが対応しておりませんので使用しないでください。(一応対応はしてるっぽいですが、そういうふうにしてない)
## LICENSE
```
MIT License

Copyright (c) 2021 ware255

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```
<br><br><br>
## メモ
2022 06/26 スロットゲームの乱数アルゴリズムを変更<br>
2022 04/30 1/6の公式を削除<br>
2022 03/17 Setup更新, マイナーな変更<br>
2022 03/15 1/6の公式を追加<br>
2022 03/14 Setupを追加(円周率の日)<br>
2022 02/22 ちょっとした変更(竹島は日本の領土)<br>
2022 02/17 円周率をOpanMPで高速化を図りました。<br>
2022 02/11 素因数分解を高速化/簡易RSA暗号を実装<br>
2022 02/09 フィボナッチ数列を追加<br>
2022 02/06 円周率の計算をさらに高速化<br>
2022 02/05 integer(LargeInt_K)をinteger(16)に変更<br>
2022 02/03 円周率の計算を高野喜久雄の公式に変更<br>
2022 02/02 円周率を桁数指定可能にしました。(ソースコード内の円周率その２(任意の桁数)参照)<br>
2022 01/23 光度距離を赤方偏移だけで計算出来るようにしました。<br>
2022 01/22 ライフゲーム実装<br>
2022 01/15 素因数分解をちょっと高速化<br>
2022 01/05 ちょっと変更<br>
2021 12/09 v1.3.5をReleseしました。<br>
2021 12/05 ちょっとした関数と離散的フーリエ変換を追加<br>
2021 11/23 gnuplotの自動化scriptを無くして自由落下を追加(参考になるかは別)<br>
2021 11/13 コードをちょこっと弄りました。<br>
2021 11/07 初めての人向けコードを追加(おまけ的なやつ)<br>
2021 11/05 余計なところを削除<br>
2021 10/27 完全数を表示する機能を追加<br>
2021 10/25 マイナーな機能を追加&&修正<br>
2021 10/23 文字入力対策的なやつ<br>
2021 10/17 いろいろと修正<br>
2021 10/12 M*の機能に付け足し<br>
2021 10/10 モメリリークのバグを修正<br>
2021 10/06 素数判定を追加&&バグ修正<br>
2021 09/29 ガンマ関数とその他修正<br>
2021 09/22 素因数分解を実装<br>
2021 09/21 ちょっとした、バグ修正<br>
2021 09/20 M+とM-を追加。詳しくはコードを確認！<br>
2021 09/18 コラッツ予想と計算に時間がかかるリーマンゼータ関数(2147483647まで)を追加<br>
2021 09/17 超戦略ゲームにレベル要素を追加したりしました。<br>
2021 09/15 階乗計算を出来るようにしました。<br>
2021 09/14 13日に更新したものをちょっと修正<br>
2021 09/13 どのような順序で計算しているか分かるようにしました。(四則演算とか)<br>
2021 09/12 引数に./calculatorのあとにbenchmarkと入力してください。ベンチマークのテストができます。<br>
2021 09/11 ソースをパクったりパクらなかったり(ちょっといじってるから問題ない?)<br>
2021 09/10 v1.3.4をReleseしました。<br>
2021 09/06 物体の運動を求められるようになりました。(運動方程式など)<br>
2021 09/02 いろいろと<br>
2021 08/27 無駄なコードをなるべく減らしました。&&たくさんの機能を追加<br>
2021 08/19 ~~n/0や0/0でのバグを修正~~ 使い方のところにあるコンパイルオプションとは違う方法でやってしまったのでバグと勘違いをしてしまいました。このままでどうぞ。<br>
2021 08/18 ちょこちょこっと&&Releseしました。<br>
2021 08/16 乱数プログラムをいじりました。<br>
2021 08/09 色々と追加しました。<br>
2021 08/05 無駄なコードをなるべく減らしました。<br>
2021 07/10 関数電卓っぽく機能を追加しました。<br>
2021 07/09 2次方程式が解けるようになりました。<br>
2021 07/05 ゲームが出来るようになりました。(イメージはUndertale)<br>
2021 07/03 円周率の計算(πr^2)から円の面積(πr^2)ってな感じでちょっと変更した。翌日、機能追加<br>
2021 06/27 円周率の計算が出来るようにしました。
