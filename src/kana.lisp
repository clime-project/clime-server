;;------------------------------------------------------------------------------------- BEGIN TURNUP
;;### kana.lisp
;;
;;　xxx
;;
;;
;;<!-- embed:sub-toc -->
;;
;;--------------------------------------------------------------------------------------- END TURNUP
#|
#|ASD|#                             (:file "kana"                  :depends-on ("package"
#|ASD|#                                                                         "utility"))
#|EXPORT|#              ;kana.lisp
 |#

(in-package :clime)



(defparameter *kana-map* #(
#("!" "！" "！")
#("," "、" "、")
#("-" "ー" "ー")
#("." "。" "。")
#("?" "？" "？")
#("a" "あ" "ア")
#("ba" "ば" "バ")
#("be" "べ" "ベ")
#("bi" "び" "ビ")
#("bo" "ぼ" "ボ")
#("bu" "ぶ" "ブ")
#("bya" "びゃ" "ビャ")
#("bye" "びぇ" "ビェ")
#("byi" "びぃ" "ビィ")
#("byo" "びょ" "ビョ")
#("byu" "びゅ" "ビュ")
#("ca" "か" "カ")
#("ce" "せ" "セ")
#("cha" "ちゃ" "チャ")
#("che" "ちぇ" "チェ")
#("chi" "ち" "チ")
#("cho" "ちょ" "チョ")
#("chu" "ちゅ" "チュ")
#("ci" "し" "シ")
#("co" "こ" "コ")
#("cu" "く" "ク")
#("cya" "ちゃ" "チャ")
#("cye" "ちぇ" "チェ")
#("cyi" "ちぃ" "チィ")
#("cyo" "ちょ" "チョ")
#("cyu" "ちゅ" "チュ")
#("da" "だ" "ダ")
#("de" "で" "デ")
#("dha" "でゃ" "デャ")
#("dhe" "でぇ" "デェ")
#("dhi" "でぃ" "ディ")
#("dho" "でょ" "デョ")
#("dhu" "でゅ" "デュ")
#("di" "ぢ" "ヂ")
#("do" "ど" "ド")
#("du" "づ" "ヅ")
#("dya" "ぢゃ" "ヂャ")
#("dye" "ぢぇ" "ヂェ")
#("dyi" "ぢぃ" "ヂィ")
#("dyo" "ぢょ" "ヂョ")
#("dyu" "でゅ" "デュ")
#("e" "え" "エ")
#("fa" "ふぁ" "ファ")
#("fe" "ふぇ" "フェ")
#("fi" "ふぃ" "フィ")
#("fo" "ふぉ" "フォ")
#("fu" "ふ" "フ")
#("ga" "が" "ガ")
#("ge" "げ" "ゲ")
#("gi" "ぎ" "ギ")
#("go" "ご" "ゴ")
#("gu" "ぐ" "グ")
#("gwa" "ぐぁ" "グァ")
#("gwe" "ぐぇ" "グェ")
#("gwi" "ぐぃ" "グィ")
#("gwo" "ぐぉ" "グォ")
#("gwu" "ぐ" "グ")
#("gya" "ぎゃ" "ギャ")
#("gye" "ぎぇ" "ギェ")
#("gyi" "ぎぃ" "ギィ")
#("gyo" "ぎょ" "ギョ")
#("gyu" "ぎゅ" "ギュ")
#("ha" "は" "ハ")
#("he" "へ" "ヘ")
#("hi" "ひ" "ヒ")
#("ho" "ほ" "ホ")
#("hu" "ふ" "フ")
#("hya" "ひゃ" "ヒャ")
#("hye" "ひぇ" "ヒェ")
#("hyi" "ひぃ" "ヒィ")
#("hyo" "ひょ" "ヒョ")
#("hyu" "ひゅ" "ヒュ")
#("i" "い" "イ")
#("ja" "じゃ" "ジャ")
#("je" "じぇ" "ジェ")
#("ji" "じ" "ジ")
#("jo" "じょ" "ジョ")
#("ju" "じゅ" "ジュ")
#("jya" "じゃ" "ジャ")
#("jye" "じぇ" "ジェ")
#("jyi" "じぃ" "ジィ")
#("jyo" "じょ" "ジョ")
#("jyu" "じゅ" "ジュ")
#("ka" "か" "カ")
#("ke" "け" "ケ")
#("ki" "き" "キ")
#("ko" "こ" "コ")
#("ku" "く" "ク")
#("kwa" "くゎ" "クヮ")
#("kwe" "くぇ" "クェ")
#("kwi" "くぃ" "クィ")
#("kwo" "くぉ" "クォ")
#("kwu" "く" "ク")
#("kya" "きゃ" "キャ")
#("kye" "きぇ" "キェ")
#("kyi" "きぃ" "キィ")
#("kyo" "きょ" "キョ")
#("kyu" "きゅ" "キュ")
#("la" "ぁ" "ァ")
#("le" "ぇ" "ェ")
#("li" "ぃ" "ィ")
#("lo" "ぉ" "ォ")
#("lu" "ぅ" "ゥ")
#("lwa" "ゎ" "ヮ")
#("lya" "ゃ" "ャ")
#("lye" "ぇ" "ェ")
#("lyi" "ぃ" "ィ")
#("lyo" "ょ" "ョ")
#("lyu" "ゅ" "ュ")
#("ma" "ま" "マ")
#("me" "め" "メ")
#("mi" "み" "ミ")
#("mo" "も" "モ")
#("mu" "む" "ム")
#("mya" "みゃ" "ミャ")
#("mye" "みぇ" "ミェ")
#("myi" "みぃ" "ミィ")
#("myo" "みょ" "ミョ")
#("myu" "みゅ" "ミュ")
#("n'" "ん" "ン")
#("na" "な" "ナ")
#("ne" "ね" "ネ")
#("ni" "に" "ニ")
#("no" "の" "ノ")
#("nu" "ぬ" "ヌ")
#("nya" "にゃ" "ニャ")
#("nye" "にぇ" "ニェ")
#("nyi" "にぃ" "ニィ")
#("nyo" "にょ" "ニョ")
#("nyu" "にゅ" "ニュ")
#("o" "お" "オ")
#("pa" "ぱ" "パ")
#("pe" "ぺ" "ペ")
#("pi" "ぴ" "ピ")
#("po" "ぽ" "ポ")
#("pu" "ぷ" "プ")
#("pya" "ぴゃ" "ピャ")
#("pye" "ぴぇ" "ピェ")
#("pyi" "ぴぃ" "ピィ")
#("pyo" "ぴょ" "ピョ")
#("pyu" "ぴゅ" "ピュ")
#("qa" "くぁ" "クァ")
#("qe" "くぇ" "クェ")
#("qi" "くぃ" "クィ")
#("qo" "くぉ" "クォ")
#("qu" "く" "ク")
#("qya" "くゃ" "クャ")
#("qye" "くぇ" "クェ")
#("qyi" "くぃ" "クィ")
#("qyo" "くょ" "クョ")
#("qyu" "くゅ" "クュ")
#("ra" "ら" "ラ")
#("re" "れ" "レ")
#("ri" "り" "リ")
#("ro" "ろ" "ロ")
#("ru" "る" "ル")
#("rya" "りゃ" "リャ")
#("rye" "りぇ" "リェ")
#("ryi" "りぃ" "リィ")
#("ryo" "りょ" "リョ")
#("ryu" "りゅ" "リュ")
#("sa" "さ" "サ")
#("se" "せ" "セ")
#("sha" "しゃ" "シャ")
#("she" "しぇ" "シェ")
#("shi" "し" "シ")
#("sho" "しょ" "ショ")
#("shu" "しゅ" "シュ")
#("si" "し" "シ")
#("so" "そ" "ソ")
#("su" "す" "ス")
#("sya" "しゃ" "シャ")
#("sye" "しぇ" "シェ")
#("syi" "しぃ" "シィ")
#("syo" "しょ" "ショ")
#("syu" "しゅ" "シュ")
#("ta" "た" "タ")
#("te" "て" "テ")
#("tha" "てゃ" "テャ")
#("the" "てぇ" "テェ")
#("thi" "てぃ" "ティ")
#("tho" "てょ" "テョ")
#("thu" "てゅ" "テュ")
#("ti" "ち" "チ")
#("to" "と" "ト")
#("tsa" "つぁ" "ツァ")
#("tse" "つぇ" "ツェ")
#("tsi" "つぃ" "ツィ")
#("tso" "つぉ" "ツォ")
#("tsu" "つ" "ツ")
#("tu" "つ" "ツ")
#("tya" "ちゃ" "チャ")
#("tye" "ちぇ" "チェ")
#("tyi" "ちぃ" "チィ")
#("tyo" "ちょ" "チョ")
#("tyu" "ちゅ" "チュ")
#("u" "う" "ウ")
#("va" "ゔぁ" "ヴァ")
#("ve" "ゔぇ" "ヴェ")
#("vi" "ゔぃ" "ヴィ")
#("vo" "ゔぉ" "ヴォ")
#("vu" "ゔ" "ヴ")
#("wa" "わ" "ワ")
#("we" "うぇ" "ウェ")
#("wi" "うぃ" "ウィ")
#("wo" "を" "ヲ")
#("wu" "う" "ウ")
#("xa" "ぁ" "ァ")
#("xe" "ぇ" "ェ")
#("xi" "ぃ" "ィ")
#("xo" "ぉ" "ォ")
#("xtsu" "っ" "ッ")
#("xtu" "っ" "ッ")
#("xu" "ぅ" "ゥ")
#("xwa" "ゎ" "ヮ")
#("xya" "ゃ" "ャ")
#("xye" "ぇ" "ェ")
#("xyi" "ぃ" "ィ")
#("xyo" "ょ" "ョ")
#("xyu" "ゅ" "ュ")
#("ya" "や" "ヤ")
#("ye" "いぇ" "イェ")
#("yi" "い" "イ")
#("yo" "よ" "ヨ")
#("yu" "ゆ" "ユ")
#("za" "ざ" "ザ")
#("ze" "ぜ" "ゼ")
#("zi" "じ" "ジ")
#("zo" "ぞ" "ゾ")
#("zu" "ず" "ズ")
#("zya" "じゃ" "ジャ")
#("zye" "じぇ" "ジェ")
#("zyi" "じぃ" "ジィ")
#("zyo" "じょ" "ジョ")
#("zyu" "じゅ" "ジュ")
))


(defun kana-lower-bound (ch arr top end)
  (if (= top end)
      top
      (let* ((mid     (ash (+ top end) -1))
             (pattern (aref (aref arr mid) 0)))
        ;;(format t "~S : ~C~%" pattern ch)
        (if (char< (char pattern 0) ch)
            (kana-lower-bound ch arr (1+ mid) end)
            (kana-lower-bound ch arr top mid)))))

(defun kana-upper-bound (ch arr top end)
  (if (= top end)
      top
      (let* ((mid     (ash (+ top end) -1))
             (pattern (aref (aref arr mid) 0)))
        ;;(format t "~S : ~C~%" pattern ch)
        (if (char<= (char pattern 0) ch)
            (kana-upper-bound ch arr (1+ mid) end)
            (kana-upper-bound ch arr top mid)))))

(defun kana-find-entry (pattern idx arr)
  (let ((rest-length (- (length pattern) idx)))
    (labels ((recur (cur end)
               (if (= cur end)
                   nil
                   (let* ((entry     (aref arr cur))
                          (entry-len (length (aref entry 0))))
                     (if (and (<= entry-len rest-length)
                              (string= (aref entry 0) pattern :start2 idx :end2 (+ idx entry-len)))
                         entry
                         (recur (1+ cur) end))))))
      (recur (kana-lower-bound (char pattern idx) arr 0 (length arr))
             (kana-upper-bound (char pattern idx) arr 0 (length arr))))))

(let ((entry-n nil))
  (defun kana-extra-n-p (pattern idx arr)
    (when (and (char= #\n (char pattern     idx))
               (or (= idx (1- (length pattern)))
                   (position  (char pattern (1+ idx)) "bcdfghjklmnpqrstvwxz!?,.-")))
      (unless entry-n
        (setf entry-n (kana-find-entry "n'" 0 arr)))
      entry-n)))

(let ((entry-tt nil))
  (defun kana-extra-double-p (pattern idx arr)
    (when (and (position (char pattern idx) "bcdfghjklmpqrstvwxyz")
               (< idx (1- (length pattern)))
               (char= (char pattern idx) (char pattern (1+ idx))))
      (unless entry-tt
        (setf entry-tt (kana-find-entry "xtu" 0 arr)))
      entry-tt)))

(defun kana-convert-impl (pattern idx arr)
  (aif (kana-find-entry pattern idx arr)
       (values (length (aref it 0)) (aref it 1) (aref it 2) 0)
       (aif (kana-extra-n-p pattern idx arr)
            (values 1 (aref it 1) (aref it 2) 0)
            (aif (kana-extra-double-p pattern idx arr)
                 (values 1 (aref it 1) (aref it 2) 0)
                 (let ((tmp (format nil "~C" (char pattern idx))))
                   (values 1 tmp tmp 1))))))

(defun kana-convert-from-pattern (pattern)
  ;; 入力パターンは全部小文字に変換しておく
  (let ((pattern (string-downcase pattern)))
    (labels ((concat (lst)
               (apply #'concatenate 'string (nreverse lst)))
             (recur (idx err acc1 acc2)
               (if (= (length pattern) idx)
                   (values (concat acc1) (concat acc2) pattern err)
                   (multiple-value-bind (length s1 s2 error) (kana-convert-impl pattern idx *kana-map*)
                     (recur (+ idx length) (+ err error) (push s1 acc1) (push s2 acc2))))))
      (recur 0 0 nil nil))))

