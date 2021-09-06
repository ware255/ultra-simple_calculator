# ultra-simple_calculator
これは、Fortranで書かれた超簡易計算機です。<br />
CUIで機能します。「GUIの方がいい」と欲張らないでください。あと、小数第三十二まではだいたいあってます。<br /><br />
## 使い方
     インストール
     $ sudo apt install gfortran gnuplot
     $ git clone https://github.com/ware255/ultra-simple_calculator.git
     移動
     $ cd ultra-simple_calculator
     コンパイル
     $ gfortran -O3 -march=native -fbackslash calculator.f90 -o calculator
     実行
     $ ./calculator
## 動作確認済みOS
     ・WSL Debian
     ・Linux Mint
     ・その他Linux
※Windowsではエスケープシーケンスが対応しておりませんので使用しないでください。(バグるかも？)
## LICENSE
      MIT License 
<br /><br />
2021 09/06 物体の運動を求められるようになりました。(運動方程式)<br />
2021 09/02 いろいろと<br />
2021 08/27 無駄なコードをなるべく減らしました。&&たくさんの機能を追加<br />
2021 08/19 ~~n/0や0/0でのバグを修正~~ 使い方のところにあるコンパイルオプションとは違う方法でやってしまったのでバグと勘違いをしてしまいました。このままでどうぞ。<br />
2021 08/18 ちょこちょこっと&&Releseしました。<br />
2021 08/16 乱数プログラムをいじりました。<br />
2021 08/09 色々と追加しました。<br />
2021 08/05 無駄なコードをなるべく減らしました。<br />
2021 07/10 関数電卓っぽく機能を追加しました。<br />
2021 07/09 2次方程式が解けるようになりました。<br />
2021 07/05 ゲームが出来るようになりました。(イメージはUndertale)<br />
2021 07/03 円周率の計算(πr^2)から円の面積(πr^2)ってな感じでちょっと変更した。翌日、機能追加<br />
2021 06/27 円周率の計算が出来るようにしました。
