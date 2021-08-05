# ultra-simple_calculator
これは、Fortranで書かれた超簡易計算機です。<br />
CUIで機能します。「GUIの方がいい」と欲張らないでください。<br /><br />
# 使い方
     Fortranコンパイラーインストール
     $ sudo apt install gfortran
     コンパイル
     $ gfortran -Ofast -march=native -fbackslash calculator.f90 -o calculator
     実行
     $ ./calculator
# 動作確認済みOS
     ・WSL Debian
     ・Linux等
<br /><br />
2021 08/05　無駄なコードをなるべく減らしました。<br />
2021 07/10　関数電卓っぽく機能を追加しました。<br />
2021 07/09　2次方程式が解けるようになりました。<br />
2021 07/05　ゲームが出来るようになりました。(イメージはUndertale)<br />
2021 07/03　円周率の計算(πr^2)から円の面積(πr^2)ってな感じでちょっと変更した。翌日、機能追加<br />
2021 06/27　円周率の計算が出来るようにしました。
