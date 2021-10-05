# Ultra-Simple_Calculator
これは、Fortranで書かれた超簡易計算機です。<br />
CUIで機能します。「GUIの方がいい」と欲張らないでください。運動方程式や特殊な計算はEuler法を採用しています。なので精度は極端にいいやつと悪いやつがあります。<br><br>
## インストール
     依存関係などのインストール
     $ sudo apt install gfortran gnuplot
     $ git clone https://github.com/ware255/ultra-simple_calculator
     移動
     $ cd ultra-simple_calculator
     コンパイル(gfortranの場合)
     $ gfortran -O3 -fopenmp -fbackslash calculator.f90 -o calculator
     コンパイル(ifortの場合)
     $ ifort -assume bscc -O3 -qopenmp calculator.f90 -o calculator
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
## 動作確認済みOS
     ・WSL Debian
     ・Linux Mint
     ・その他Linux
※Windowsではエスケープシーケンスが対応しておりませんので使用しないでください。(一応対応はしてるっぽいですが、そういうふうにしてない)
## LICENSE
      MIT License 
<br><br><br>
## メモ
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
