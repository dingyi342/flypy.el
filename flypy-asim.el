;;; flypy-asim.el --- auto switch input method. -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <dingyi@dingyi>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code =================================================================
;;; 设置默认的中文,英文输入法和光标颜色
;;都使用的是 quail 框架的,好统一处理
(setq cnim "chinese-flypy")
(setq enim "english-flypy")

;; 不使用默认的输入法
;; (setq default-input-method enim)
(setq default-input-method nil)

;; (setq evil-input-method nil)

;; (set-input-method cnim)
;; (set-input-method enim)
;; (set-input-method "chinese-flypy")
;; (set-input-method "english-flypy")

;; deactivate-input-method 会调用这个, 如果不设置,会显示为 nil
;; (funcall deactivate-current-input-method-function)
;; 在 deactivate-input-method 调用之前设置为 quail-deactivate

;;;; 每次输入前设置光标颜色.
;; 红色中文
(setq cnim-cursor-color
      "#ff1493"
      )
;; 绿色英文
(setq enim-cursor-color
      "#adff2f"
      )
;; 蓝色连续英文
(setq enim-only-cursor-color
      ;; "#00af00"
      ;; "#adff2f"
      "#0087ff"
      )

;;;; 设置中文输入状态下的光标
(defun set-cnim--cursor ()
  (set-cursor-color cnim-cursor-color)
  ;; (setq cursor-type '(bar . 5))
  (setq cursor-type '(hbar . 5))
  )

(defun set-cnim-cursor ()
  ;; (setq evil-insert-state-cursor `((bar . 5) ,cnim-cursor-color))
  (if (or (enim-p)
          ;; (enev-p)
          (force-enev-p))
      (set-enim--cursor)
    (set-cnim--cursor)
    )
  )
;;;; 设置英文输入状态时的光标
(defun set-enim--cursor ()
  (set-cursor-color enim-cursor-color)
  ;; (setq cursor-type '(bar . 5))
  ;; (setq cursor-type '(bar . 5))
  ;; (setq cursor-type 'bar)
  ;; (setq evil-insert-state-cursor `((bar . 5) ,cnim-cursor-color))
  (setq cursor-type '(hbar . 5))
  )

(defun set-enim-only--cursor ()
  (set-cursor-color enim-only-cursor-color)
  ;; (setq cursor-type '(bar . 5))
  ;; (setq cursor-type 'bar)
  ;; (setq cursor-type t)
  (setq cursor-type '(hbar . 5))
  )

(defun set-enim-cursor ()
  (if (or (enim-p)
          ;; (enev-p)
          (force-enev-p))
      (if enim-only-mode
          (set-enim-only--cursor)
        (set-enim--cursor))
    (set-cnim-cursor)
    )
  )

;; (defun set-enim-cursor ()
;;   ;; (setq evil-insert-state-cursor `(bar ,enim-cursor-color))
;;   (if (not (evil-normal-state-p))
;;       (progn
;;         (set-cursor-color enim-cursor-color)
;;         (setq cursor-type 'bar)))
;;   )

(defun set-cursor-color-by-im ()
  (if (or
       (enim-p)
       (force-enev-p)
       )
      ;; (set-cursor-color cnim-cursor-color)
    ;; (set-cursor-color enim-cursor-color)
      (set-enim-cursor)
    (set-cnim-cursor)
    )
  )

;; ;; (advice-add 'quail-self-insert-command :before 'set-cursor-color-by-im)
;; (advice-remove 'quail-self-insert-command 'set-cursor-color-by-im)

;; (setq evil-insert-state-cursor '((bar . 5) "yellow"))
;; (setq evil-insert-state-cursor '(bar "yellow"))
;; (setq evil-insert-state-cursor '((box) "yellow"))
;; (setq evil-insert-state-cursor '(box "yellow"))
;; (setq evil-insert-state-cursor '(hollow "yellow"))

(add-hook 'input-method-activate-hook 'set-cursor-color-by-im)
(add-hook 'evil-insert-state-entry-hook 'set-cursor-color-by-im)

;;; 激活输入法命令
;;;; 激活输入法根据前面的第一个非空格字符
;; 还可设置更多条件的 set-im 情况, 不适合在这个层面上用,在具体情况上加个判断就好了
;; 不应该把是不是在行首在这里来判断吧.
(defun set-im-by-ev ()
  (interactive)
  (setq input-method-function 'quail-input-method)
  ;; (setq current-input-method enim)
  (if (and (char-before-beol-ignore-blank-p)
           (enev-p)
           )
      (set-cnim)
    (if (enev-p) (set-enim) (set-cnim))
    )
  )

(defalias 'set-im 'set-im-by-ev)
;;;; 激活输入法根据 (enev-p)
(defun act-im-by-ev ()
  (if (enev-p)
      (act-enim)
    (act-cnim)
    )
  )
(defalias 'act-im 'act-im-by-ev)

(defun set-im-by-ev-1 ()
  (interactive)
  (setq input-method-function 'quail-input-method)
  ;; (setq current-input-method enim)
  (if (and (char-before-beol-ignore-blank-p)
           (enev-p)
           )
      (set-cnim)
    (if (enev-p) (set-enim) (set-cnim))
    )
  )
;;;; 如果 enev-p 为 t 激活英文输入, 为 nil 不做.

;;; 切换输入法命令
;;;; [弃用]取消所有输入法
;; 直接 deactivate-input-method 即可, 有时后进入 normal 还用中文输入法.
;; 故设置一个无论在什么状态都完全禁用输入法的函数
;; 这个不用了, 进入 normal-state 就用 set-enim ,然后 quail 每次输入之前都检测是不是 normal-state, 强制输入英文
;; 可以万不得已再使用

(defun set-noim ()
  (interactive)
  (if current-input-method
      (progn
        ;; 设置这个因为 deactivate-input-method 会调用这个函数,如果不设置默认为 nil ,就会导致 deactivate-input-metho 执行后报错 void-function nil
        (setq deactivate-current-input-method-function 'quail-deactivate)
        (deactivate-input-method)
        ;; (setq current-input-method nil)
        (setq evil-input-method nil)
        (set-input-method nil)
        (setq input-method-function nil)
        (setq current-input-method-title nil)
        )
    (progn
      (setq evil-input-method nil)
      (setq input-method-function nil)
      (set-input-method nil)
      )
    ))

(defalias 'deactivate-current-input-method 'set-noim)

;;;; 设置当前输入法为中文
;;千万不能在 normal mode 下,  使用 (set-input-mode cnim) 会让在 normal mode 下也使用中文输入法
;; 强制禁用输入法,但复杂了,准备全在 quail 框架下,然后 force-enev-p 加上判断是不是 normal-state

(defun set-cnim ()
  ;; (setq isearch-input-method-function 'quail-input-method)
  (set-input-method cnim)
  (set-cnim-cursor)
  )

;; (defun set-cnim ()
;;   (if (evil-normal-state-p)
;;       (deactivate-current-input-method)
;;     (progn
;;       (setq isearch-input-method-function 'quail-input-method)
;;       (set-input-method cnim)))
;;   )

;;;; 设置当前输入法为英文
(defun set-enim ()
  ;; (setq isearch-input-method-function 'quail-input-method)
  (set-input-method enim)
  (set-enim-cursor)
  )

;; (defun set-enim ()
;;   (if (evil-normal-state-p)
;;   (deactivate-current-input-method)
;;   (progn
;;     (setq isearch-input-method-function 'quail-input-method)
;;     (set-input-method enim)))
;; )

;;;; 激活当前输入法为中文, 激活和设置是不样的.
;; 设置无论什么状态都会激活,比如在 normal state;
;; 激活不会
(defun act-cnim ()
  (activate-input-method cnim)
  (set-cnim-cursor)
  )

;; (defun act-cnim ()
;;   (if (evil-normal-state-p)
;;       (deactivate-current-input-method)
;;     (activate-input-method cnim)))

;;;; 激活当前输入法为英文
(defun act-enim ()
  (activate-input-method enim)
  (set-enim-cursor)
  )

;; (defun act-enim ()
;;   (if (evil-normal-state-p)
;;       (deactivate-current-input-method)
;;     (activate-input-method enim)))
;;;; 在 cnim enim 之间进行切换,
;; 确保能在 M-x 里使用
(defun toggle-cnim-enim ()
  (interactive)
  (if
      ;; (or (evil-insert-state-p)
      ;;     (evil-emacs-state-p)
      ;;     )
      (not (evil-normal-state-p))
      (progn
        (setq input-method-function 'quail-input-method)
        ;; (setq current-input-method t)
        ;; (setq current-input-method enim)
        (if (enim-p) (set-cnim) (set-enim)))))

(global-set-key (kbd "C-\\") 'toggle-cnim-enim)

;; (general-def
;;   :keymaps 'override
;;   "C-\\" 'toggle-cnim-enim
;;   )

(defun toggle--cnim-enim ()
  (if (enim-p) (set-cnim) (set-enim))
  )

;;; 判断当前输入法状态
;;;; 判断当前输入法是英文?
(defun enim-p ()
  (string= current-input-method enim)
  )
;;;; 判断当前输入法为中文?
(defun cnim-p ()
  (string= current-input-method cnim)

  )
;;;; 判断当前输入法未激活
;; (current-input-method) 即可

;; (defun im-p ()
;;   (current-input-method)
;;   )

;;; 判断周围环境的函数
;;;; 辅助函数
(defun pyim-string-match-p (regexp string &optional start)
  "与 `string-match-p' 类似，如果 REGEXP 和 STRING 是非字符串时，
不会报错。"
  (and (stringp regexp)
       (stringp string)
       (string-match-p regexp string start)))

;;;;; 查找前几个字符是什么
(defun pyim-char-before-to-string (num)
  "得到光标前第 `num' 个字符，并将其转换为字符串。"
  (let* ((point (point))
         (point-before (- point num)))
    (when (and (> point-before 0)
               (char-before point-before))
      (char-to-string (char-before point-before)))))

(defun pyim-char-after-to-string (num)
  "得到光标后第 `num' 个字符，并将其转换为字符串。"
  (let* ((point (point))
         (point-after (+ point num)))
    (when (char-after point-after)
      (char-to-string (char-after point-after)))))

(defun str-before-0 ()
  (pyim-char-before-to-string 0))

(defun str-before-1 ()
  (pyim-char-before-to-string 1))

;;;;; 忽略空格查找之前第几个字符
;; 忽略 \n \t \r space
(defun str-before-num-ignore-space (num)
  (save-excursion
    ;; 查找前一个非空格字符.
    (if (re-search-backward "[^[:space:]\n]" nil t)
        (let* ((point (+ (point) 1))
               (point-before (- point num)))
          (when (and (> point-before 0)
                     (char-before point-before))
            (char-to-string (char-before point-before)))))))

;; 只忽略空格
;;"[^[:blank:]\n]"
(defun str-before-num-ignore-blank (num)
  (save-excursion
    ;; 查找前一个非空格字符.
    (if (re-search-backward "[^ ]" nil t)
        (let* ((point (+ (point) 1))
               (point-before (- point num)))
          (when (and (> point-before 0)
                     (char-before point-before))
            (char-to-string (char-before point-before)))))))

;;;; 当前光标前
;;;;; 当前光标前是空格
(defun char-before-space-p ()
  (pyim-string-match-p " " (str-before-0))
  )

(defun char-before-1-space-p ()
  (pyim-string-match-p " " (str-before-1))
  )

(defun char-before-only-one-space-p ()
  (and
   (pyim-string-match-p " " (str-before-0))
   (not (pyim-string-match-p " " (str-before-1)))
   ))

;;;;; 当前光标前是行首
;; 当前光标前的第一个字符, 不能忽略空格
(defun char-before-beol-p ()
  (pyim-string-match-p "\n" (str-before-0))
  )

;; 忽略空格看是不是在行首
(defun char-before-beol-ignore-blank-p ()
  (pyim-string-match-p "\n" (str-before-num-ignore-blank 0)))

;; 忽略 space, 包括 \n \r 等
(defun char-before-beol-ignore-space-p ()
  (pyim-string-match-p "\n" (str-before-num-ignore-space 0)))

;;;;; 当前光标前是中文
(defun char-before-0-cn-p ()
  (pyim-string-match-p "\\cc" (str-before-0))
  )
(defun char-before-1-cn-p ()
  (pyim-string-match-p "\\cc" (str-before-1))
  )

(defun char-before-0-cn-ignore-blank-p ()
  (pyim-string-match-p "\\cc" (str-before-num-ignore-blank 0)))

(defun char-before-0-cn-ignore-blank-p ()
  (pyim-string-match-p "\\cc" (str-before-num-ignore-blank 1)))

(defun char-before-0-cn-ignore-space-p ()
  (pyim-string-match-p "\\cc" (str-before-num-ignore-space 0)))

;;;;; 当前光标前是非中文
(defun char-before-not-cn-p ()
  (not (pyim-string-match-p "\\cc" (str-before-0)))
  )

;;;;; 当前光标前是英文字母
;; 选项是忽略大小写的,所以都一样了.
(defun char-before-en-p ()
  (let ((case-fold-search t))
    (pyim-string-match-p "[a-zA-Z]" (str-before-0))
    )
  )

(defun char-before-0-en-ignore-blank-p  ()
  (pyim-string-match-p "[a-zA-Z]" (str-before-num-ignore-blank 0)))

(defun char-before-1-en-ignore-blank-p  ()
  (pyim-string-match-p "[a-zA-Z]" (str-before-num-ignore-blank 1)))

;;;;; 当前光标前是大写英文字母
(defun char-before-en-upper-p ()
  (let ((case-fold-search nil))
    (pyim-string-match-p "[A-Z]" (str-before-0))
    )
  )

;;;;; 当前光标前是小写英文字母
(defun char-before-en-lower-p ()
  (let ((case-fold-search nil))
    (pyim-string-match-p "[a-z]" (str-before-0))
    )
  )

;;;;; 当前光标前是数字
(defun char-before-digit-p ()
  (pyim-string-match-p "[0-9]" (str-before-0))
  )

;;;;; 当前光标前是标点符号
(defun char-before-punc-p ()
  (pyim-string-match-p "[]{}()""[]" (str-before-0))
  )

(defun point-before-cn-punc-p ()
  (pyim-string-match-p "[,.;\"?]" (str-before-0))
  ;; (pyim-string-match-p "[]!@#$%^&*(){}-=+\|`~<>,.;:\'\"?/[]" (pyim-char-before-to-string 1))
  )

(defun point-before-punc-p ()
  (pyim-string-match-p "[]!@#$%^&*(){}-=+\|`~<>,.;:\'\"?/[]" (str-before-0))
  ;; (pyim-string-match-p "[]!@#$%^&*(){}-=+\|`~<>,.;:\'\"?/[]" (pyim-char-before-to-string 1))
  )

;; , . 不能用
;; - \ | 要转义
(defun point-before-punc-1-p ()
  (pyim-string-match-p "[]!@#$%^&\*(){}\=\+\|`~<>;:\'\"?/\\[\-]" (str-before-0))
  ;; (pyim-string-match-p "[]!@#$%^&*(){}-=+\|`~<>,.;:\'\"?/[]" (pyim-char-before-to-string 1))
)

;;;;; 当前光标前是特殊符号
(defun point-before-special-symbol-p ()
  (pyim-string-match-p "[θφρσβυηνμψϊπκ]" (str-before-0))
  )

;;;; org mode code
;;;;; 前面是block
;; (org-in-block-p)
;;;;; 前面是 src block
;; 在 org mode 的 code block 里只能输入英文;
;; (defun org-src-p ()
;;   (org-babel-get-src-block-info)
;;   )

;; 用这个
;; (org-in-src-block-p)

;; 判断是 src block 里面的 comment.
;; 就是获取一行, 从头往后找,找到第一个注释符就停止, 后面的都是评论内容.
;; (re-search-backward comment-start-skip (line-beginning-position) 'move)
;; (re-search-backward comment-start-skip (line-beginning-position) t)
;; (comment-search-backward (line-beginning-position) t)

(defun org-in-src-code-p ()
  )

(defun org-in-src-comment-p ()
  (require 'org)
  (if (org-in-src-block-p)
      (save-excursion
        (re-search-backward comment-start-skip (line-beginning-position) 'move)
        )
    ))

(defun org-in-src-string-p ()
  ;; (string-match-p "\\`[]*\\'" string))
  ;; "cesdlfas"
  )

(defun org-in-src-no-comment-p ()
  (require 'org)
  (and (org-in-src-block-p)
       (not (org-in-src-comment-p))
       )
  )

(defun org-in-src-no-string-p ()
  )
;;;;; 前面是列表
;; (org-in-item-p)
;;;; 在 prog-mode 下只能在 comment/string 里输入中文
(defun pyim-probe-program-mode ()
  (interactive)
  (when (derived-mode-p 'prog-mode)
    (let* ((pos (point))
           (ppss (syntax-ppss pos)))
      (not
       (or (car (setq ppss (nthcdr 3 ppss)))
           (car (setq ppss (cdr ppss)))
           (nth 3 ppss))))))
;;;; 在标点符号之后使用英语输入
;; 用 post-self-insert-hook
;; (defun flypy-switch-3 ()
;;   (interactive)
;;   (cond
;;    ((and (pyim-string-match-p "[({\\<-[]" (pyim-char-before-to-string 0))
;;          current-input-method)
;;     (deactivate-input-method))
;;    ;; ((and (pyim-string-match-p "[])}" (pyim-char-after-to-string 0))
;;    current-input-method)
;;   (deactivate-input-method))

;; ;; ((pyim-string-match-p "[])}" (pyim-char-before-to-string 1))
;; ;;  (activate-default-input-method))

;;  (add-hook 'post-self-insert-hook 'flypy-switch-3)

(defun point-before-punc-p ()
  (pyim-string-match-p "[]!@#$%^&*(){}-=+\|`~<>,.;:\'\"?/[]" (pyim-char-before-to-string 0))
  ;; (pyim-string-match-p "[]!@#$%^&*(){}-=+\|`~<>,.;:\'\"?/[]" (pyim-char-before-to-string 1))
  )

;;; [弃用]强制禁用输入, 无法切换
;; (set-noim)
;;;; [弃用]进入 normal state 禁用输入法
;; 这个感觉不是很好. 可以用.
;; (add-hook 'evil-normal-state-entry-hook 'set-noim)
;; (remove-hook 'evil-normal-state-entry-hook 'set-noim)

;;; 强制英文输入, 切换不了
;;;; 全局强制英文输入
(defvar-local enim-only-mode nil)
(define-minor-mode enim-only-mode
  "Minor mode to set english input method only"
  :init-value nil
  ;; :global t
  :variable enim-only-mode
  (if enim-only-mode
      (setq enim-only-mode t)
      )
  )

(defun enim-only-mode-toggle-im ()
  (if enim-only-mode
      (progn
        (enim-only-mode -1)
        (set-cnim)
        )
    (progn
      (enim-only-mode 1)
      (set-enim)
      )
    )
  )
;;;; force-enev-p 强制英文输入的探针
;; 返回为 t 的时候, 强制只能输入英文, 不可以切换成中文输入.
;; 返回为 nil 的时候, 默认为输入中文,可以切换成输入英文
;; 是每个输入命令执行之前判断的.
;; 由于是强制属性所以不要覆盖面太广,否则在有些环境下想切换成中文输入都不行
(defun force-enev-p ()
  (or

;;;;; 编程模式下在非字符串和注释里强制输入英文
   (pyim-probe-program-mode)

;;;;; [弃用]标点符号后强制输入英文
   ;; 不能强制,可以切换
   ;; (point-before-punc-p)
   ;; (point-before-punc-1-p)

   ;; (pyim-probe-dynamic-english)
   ;; (flypy-probe-dynamic-english-2)

;;;;; [弃用]前面字符是大写强制输入英文
   ;; (char-before-en-upper-p)

;;;;; 在 org mode 的 code block 里只能输入英文;
   ;; (org-in-src-block-p)
   (org-in-src-no-comment-p)

;;;;; 在 normal 模式下强制英文输入
   (evil-normal-state-p)
;;;;; minor mode 开启强制输入英文
   enim-only-mode
   )
  ;; ()(pyim-string-match-p "\\cc" (pyim-char-before-to-string 0))
  )

;;;; 进入 normal state 强制切换为英文输入法
;; 由于进入 normal state 不是禁用输入法,而是强制使用英文输入法
;; 添加 (evil-normal-state-p) 到 (foce-enev-p) 来强制执行
;; 可以添加这个钩子也可以不添加.
;; (add-hook 'evil-normal-state-entry-hook 'set-enim)
;; (remove-hook 'evil-normal-state-entry-hook 'set-enim)
(add-hook 'evil-normal-state-entry-hook (lambda ()
                                          (set-input-method enim)))
;;;; quail-self-insert-command 根据 force-enev-p 每按键之前判断强制切换英文输入法
;; 由于每次输入之前都会判断,所以使用的 force-enev-p 范围比 enev-p 小
;; 只在强制要输入英文的判定下, 只允许英文输入, 其他情况通过其他方式来智能切换或手动切换
(defun auto-toggle-input-method ()
  (if (force-enev-p)
      (set-input-method enim)
    ;; (if (cnim-p)
    ;;     (act-cnim)
    ;;     )
    )
    ;; (if (cnev-p) (set-cnim))
  )

(advice-add 'quail-self-insert-command :before #'auto-toggle-input-method)
;; (advice-remove 'quail-self-insert-command  #'auto-toggle-input-method)

;;; 默认英文输入, 可以切换
;;;; 判断当前默认的输入环境--为英文
;; 虽然在此环境下应该输入英文,但是可以切换成中文来输入
(defun enev-p ()
  (or
;;;;; 继承强制输入英文的环境
   (force-enev-p)
;;;;; [弃用]前面的字符是非中文,默认输入英文
   ;; (char-before-not-cn-p)
;;;;; 前面的字符是英文
   (char-before-en-p)
;;;;; 前面的字符是标点符号默认输入英文
   (point-before-punc-1-p)
;;;;; 在 org src block 中默认输入英文
   ;; (org-in-src-block-p)
;;;;; 前面是大写字母
   (char-before-en-upper-p)

;;;;; 前面是特殊符号 θ
   (point-before-special-symbol-p)
;;;;; eshell mode shell mode
   (memq major-mode '(eshell-mode shell-mode))
   )
  )
;;;; 判断(enev-p) 为 t, 就 act-enim
(defun default-enim ()
  (if (enev-p)
      (act-enim)
      ))
;; (enev-p) 为 t , 就默认用英文输入
;;;; post-self-insert-hook 输入后自动切换为英文输入法
(add-hook 'post-self-insert-hook 'default-enim)
;; (remove-hook 'post-self-insert-hook 'default-enim)
;;; 强制中文输入, 切换不了
;; 没有这个功能,强制英文就行了,其他地方都是可以切换的
;;; 默认中文输入, 可切换
;;;; 判断当前为中文输入环境
(defun cnev-p ()
  (not (enev-p))
  )
;;;; 在行首默认中文输入
;;;; 在 org-mode 中, C-Ret/M-Ret/o 默认输入中文
;;; 默认切换输入, 可手动或其他方式切换
;;;; helm 进入默认使用英文输入
(add-hook 'helm-minibuffer-set-up-hook 'set-enim)
;; (remove-hook 'helm-minibuffer-set-up-hook 'set-enim)

;; 这个 hook 没用.
;; (add-hook 'helm-mode-hook 'set-enim)

;; 这个有问题会导致提示 void function
;; (add-hook 'helm-before-initialize-hook 'set-enim)
;; TODO Debugger entered--Lisp error: (void-function nil)
;; nil()
;; deactivate-input-method()

;; 看了 debug, 不是这个 hook 的问题, 是进入 helm-M-x 会进入 normal state 从而调用了
;; normal entry 的 hook.
;; 是 hook 函数调用 deactivate-input-method, 该函数又调用一个  (funcall deactivate-current-input-method-function)
;; 该函数默认为 nil ,所以导致 void-function nil
;; 在 deactivate-input-method 调用之前设置为 quail-deactivate ,即可.

;; 一个方法就是在进入 M-x 之前就进入 normal-mode
;; 或者先进入 normal-mode 再执行 M-x
;; 上面的好.

;; 不是禁用输入法, 否则不方便切换到中文输入
;; (add-hook 'helm-minibuffer-set-up-hook 'deactivate-current-input-method)
;; (add-hook 'helm-before-initialize-hook 'deactivate-current-input-method)

;;;; minibuffer 进入默认使用英文输入
;; (add-hook 'minibuffer-setup-hook (lambda ()
;;                                    (set-enim)
;;                                    ;; (run-with-timer 0.2 nil 'evil-insert-state)
;;                                    ))
;; (add-hook 'minibuffer-setup-hook 'set-enim)
;; (remove-hook 'minibuffer-setup-hook 'set-enim)

;; (add-hook 'minibuffer-exit-hook (lambda ()
;;                                   (run-with-timer 0.2 nil 'evil-normal-state)
;;                                   ))

;; (add-hook 'minibuffer-setup-hook (lambda ()
;;                                    (setq current-input-method cnim)
;;                                    ))

;; 禁用输入法不方便切换到中文输入
;; (add-hook 'minibuffer-setup-hook 'deactivate-current-input-method)

;; C-\ runs the command isearch-toggle-input-method (found in
;; 进入 minibuffer set-cnim, 它还是默认禁用输入法,然后按 shift 就可切换到中文了.
;; 虽然 isearch 没事,但是会影响 helm,

;; 本质是进入 mibibuffer 后进入了 normal mode 早成的.

;; (define-key isearch-minibuffer-local-map (kbd "C-\\") 'toggle-cnim-enim)
;; (define-key minibuffer-local-isearch-map (kbd "C-\\") 'toggle-cnim-enim)

;; (add-hook 'isearch-mode-hook 'set-enim)
;; (remove-hook 'isearch-mode-hook 'set-enim)

;; hook 进入 emacs-state
;; 因为只有在 emacs-state 才能正常输入中文,
(add-hook 'isearch-mode-hook 'evil-emacs-state)
(add-hook 'isearch-mode-end-hook 'evil-normal-state)
(setq isearch-input-method-function 'quail-input-method)

(defun my/isearch-toggle-input-method ()
  "Toggle input method in interactive search."
  (interactive)
  (setq isearch-input-method-function 'quail-input-method)
  (setq isearch-input-method-local-p t)
  ;; 用act-cnim, 而不是 set-cnim, 否则到 nromal state 下也用的中文输入
  (if (enim-p) (act-cnim) (act-enim))
  (isearch-update)
  )

(advice-add 'isearch-toggle-input-method :override 'my/isearch-toggle-input-method)

;;;; 插入模式检测环境切换输入法
;; (add-hook 'evil-insert-state-entry-hook 'set-im-by-ev)
;; (remove-hook 'evil-insert-state-entry-hook 'set-im-by-ev)

;; 进入插入模式进入 flypy 输入法, org-mode 单独优化
(add-hook 'evil-insert-state-entry-hook (lambda ()
                                          (if (not (string= major-mode "org-mode"))
                                              (set-im)
                                            )
                                          ))

;; (defun flypy-probe-dynamic-english-2 ()
;;   (and (pyim-probe-dynamic-english)
;;        (not (pyim-string-match-p "\n" (pyim-char-before-to-string 0)))
;;        (enim-p)
;;        ))

;; (defun flypy-switch-2 ()
;;   ;; 如果前面不是中文
;;   (if (pyim-probe-dynamic-english)
;;       ;; 如果在行首, 中间没有空格
;;       (if (pyim-string-match-p "\n" (pyim-char-before-to-string 0))
;;           ;; 用中文输入法
;;           (set-cnim)
;;         ;; 距离行首之间有空格,就用英文输入
;;         (set-enim))
;;     ;; 如果前面是中文,就激活中文输入法
;;     (set-cnim)
;;     )
;;   )

;; (add-hook 'evil-insert-state-entry-hook 'flypy-switch-2)

;;;; 根据空格切换输入法
;; 只在编辑模式下 text-mode prog-mode
;; 在 term 下就不要用了. special-mode
;; 在 force-enim-p 下也不要切换了反正切换还是强制输入英文
;; 在 enim-p 下,默认输入英文,但是空格会切换, 不好.
(general-def
  :states 'insert
  :keymaps '(org-mode-map prog-mode-map)
  "SPC" 'flypy-space-switch
  )

(defun flypy-space-switch ()
  (interactive)
  (if (or
       ;; (force-enev-p)                   ; 强制输入英文的用空格切换输入没意义了
       ;; 在手动开启 enim-only-mode 还是用空格来切换输入法
       (let ((enim-only-mode nil)) (force-enev-p))
       (org-in-src-no-comment-p)
       )
      (insert " ")
    (toggle-im-with-space)
    )
  )

;; (defun enim-only-char-before-space ()
;;   (if (and enim-only-mode
;;            (char-before-space-p))
;;       (progn (enim-only-mode -1)
;;              (set-cnim))))

(defun space-expand-or-self-insert ()
  (cond ((expand-abbrev))
        (t (insert " "))))

(defun toggle-im-with-space ()
  (interactive)
  (cond ((char-before-beol-ignore-blank-p)
         (toggle-cnim-enim))
        ((char-before-0-en-ignore-blank-p)
         (if (char-before-space-p)
             (if enim-only-mode
                 (progn
                   (enim-only-mode -1)
                   (set-cnim)
                   )
               (toggle-cnim-enim))
           (progn (space-expand-or-self-insert)
                  (if (not enim-only-mode) (set-cnim)))))
        ((char-before-0-cn-ignore-blank-p)
         (if (char-before-space-p)
             (toggle-cnim-enim)
           (progn (insert " ")
                  (set-enim))))
        ((point-before-cn-punc-p)
         (if (char-before-space-p)
             (toggle-cnim-enim)
           (progn
             (insert " ")
             (toggle-cnim-enim)
             )
           ))
        (t (if (char-before-space-p)
               (toggle-cnim-enim)
             (progn
               (insert " ")
               ;; (toggle-cnim-enim)
               )))
        ))
;;;;; 如果前面是标点符号, 也是切换输入法

;;;; 行首自动设置为中文输入
;;;; backspace, 删除后根据前面字符切换输入法
;; 删除后跳过空格的之前字符是中文
;; 删除后跳过空格的之前前字符是中文, 都切换到中文输入,否则英文输入
;;
;; (general-def
;;   :states 'insert
;;   "DEL" (lambda ()
;;           (interactive "*")
;;           (if
;;               (or
;;                ;; 如果不只有一个空格
;;                (not (char-before-only-one-space-p))
;;                ;; 如果忽略空格后的当前字符是中文
;;                (not (char-before-0-en-ignore-blank-p))
;;                ;; 如果是在强制英文输入的环境下, 除了是手动开启的情况.
;;                (let ((enim-only-mode nil)) (force-enev-p))
;;                )
;;             (delete-backward-char 1))
;;           ;; (if (not (let ((enim-only-mode nil)) (force-enev-p)))
;;           ;;     (call-interactively 'enim-only-mode))
;;           (if (or
;;                (char-before-0-cn-ignore-blank-p)
;;                ;; (char-before-1-cn-ignore-blank-p)
;;                (char-before-beol-p)
;;                )
;;               (progn
;;                 (enim-only-mode -1)
;;                 (set-cnim)
;;                 ))
;;           ;; 如果之前是空格+英文, 按 alt 就会切换 enim-only-mode 和输入法
;;           (if (and (char-before-1-en-ignore-blank-p)
;;                    (char-before-space-p)
;;                    )
;;               (if (not (let ((enim-only-mode nil)) (force-enev-p)))
;;                   (enim-only-mode-toggle-im)
;;                 )
;;               )
;;           ))

;; ;;;; enter , 换行后设置
;; (general-def
;;   :states 'insert
;;   :keymaps '(text-mode-map prog-mode-map)
;;   "C-j" 'flypy-switch-2
;;   "Ret" 'flypy-switch-2
;;   )
;; (add-hook 'post-self-insert-hook (lambda ()
;;                                    (if (char-before (- (point) 1)))
;;                                    ))
(advice-add 'newline :after (lambda (&rest args)
                              (enim-only-mode -1)
                              ))

;;;; C-w, 删除词之后的设置
;;;; evil-org-mode 优化
;; 默认 o 后切换到中文输入
;; 就是光标前面是 * .
(general-def evil-org-mode-map
  :states 'normal
  "o" (lambda (count)
        (interactive "P")
        (evil-org-open-below count)
        (set-cnim)
        )
  "a" (lambda (count)
        (interactive "P")
        (evil-append count)
        (set-im-by-ev)
        )
  "i" (lambda (count)
        (interactive "P")
        (evil-insert count)
        (set-im-by-ev)
        )
  "C-<return>" (lambda ()
            (interactive)
            (evil-org-org-insert-heading-respect-content-below)
            (evil-insert 1)
            (set-cnim)
            )
  "M-RET" (lambda ()
            (interactive)
            (org-meta-return)
            (evil-insert 1)
            (set-cnim)
            )
  )

(general-def evil-org-mode-map
  :states 'insert
  "C-<return>" (lambda ()
                 (interactive)
                 (evil-org-org-insert-heading-respect-content-below)
                 (evil-insert 1)
                 (set-cnim)
                 )
  "M-RET" (lambda ()
            (interactive)
            (org-meta-return)
            (evil-insert 1)
            (set-cnim)
            )
  )

(general-def org-mode-map
  :states 'insert
  "C-j" (lambda ()
          (interactive)
          (org-return-indent)
          (set-cnim)
          )
  "RET" (lambda (&optional indent)
          (interactive)
          (org-return)
          (if (not (enim-only-mode))
              (set-cnim)
            )
          )
  )
;;; =========================================================================
;; (set-input-method nil)
;; (set-input-method "ucs")
;; (deactivate-input-method)
;; deactivate-current-input-method
;; (ucs-input-deactivate)
;; evil-deactivate-input-method
;; (quail-input-method (kbd "k"))
;; toggle-input-method
;; activate-input-method
;; deactivate-input-method

;; evil-activate-input-method
;; evil-deactivate-input-method
;; activate-default-input-method
;; evil-without-input-method-hooks

;; deactivate-current-input-method

;; set-input-mode
;; current-input-mode
;; input-method-alist
;; input-method-history
;; input-method-function ;;输入法函数
;; input-method-verbose-flag
;; input-method-activate-hook
;; input-method-highlight-flag
;; input-method-inactivate-hook
;; input-method-previous-message
;; input-method-use-echo-area
;; input-method-deactivate-hook
;; evil-input-method
;; current-input-method
;; default-input-method
;; input-method-exit-on-first-char
;; input-method-after-insert-chunk-hook
;; current-input-method-title
;; unread-input-method-events
;; toggle-input-method-active
;; hydra--input-method-function
;; isearch-input-method-function
;; isearch-input-method-local-p
;; helm-input-method-verbose-flag
;; helm-inherit-input-method
;; mode-line-input-method-map
;; unread-post-input-method-events
;; describe-current-input-method-function
;; inactivate-current-input-method-function
;; deactivate-current-input-method-function
;; google-translate-input-method-auto-toggling
;; google-translate-preferable-input-methods-alist
;; input-method-exit-on-invalid-key
;; (defun auto-toggle-input-method ()
;;   (if (and (pyim-probe-auto-english)
;;            (pyim-probe-dynamic-english)
;;            )
;;       (deactivate-input-method)
;;     (activate-default-input-method)
;;       )
;;   )
;; ;; (advice-add 'self-insert-command :after #'auto-toggle-input-method)
;; (advice-add 'quail-self-insert-command :after #'auto-toggle-input-method)
;; (advice-add 'quail-self-insert-command :after #'auto-toggle-input-method)

;; quail-overlay
;; quail-converting
;; quail-translating
;; quail-current-key
;; quail-current-str
;; quail-current-data
;; quail-guidance-buf
;; quail-conv-overlay
;; quail-guidance-str
;; quail-package-alist
;; quail-directory-name
;; quail-conversion-str
;; quail-completion-buf
;; quail-guidance-frame
;; quail-keyboard-layout
;; quail-current-package
;; quail-conversion-keymap
;; quail-translation-keymap
;; quail-current-translations
;; quail-translation-docstring
;; quail-completion-max-depth
;; quail-decode-map-generated
;; quail-simple-translation-keymap
;; quail-guidance-translations-starting-column
;; quail-keyboard-layout-len
;; quail-keyboard-layout-type
;; quail-keyboard-layout-alist
;; quail-keyboard-layout-standard
;; quail-keyboard-layout-substitution

;; quail-map
;; quail-help
;; quail-title
;; quail-error
;; quail-advice
;; quail-simple
;; quail-vunion
;; quail-package
;; quail-defrule
;; quail-guidance
;; quail-activate
;; quail-docstring
;; quail-inactivate
;; quail-completion
;; quail-deactivate
;; quail-deterministic
;; quail-map-p
;; quail-show-key
;; quail-find-key1
;; quail-indent-to
;; quail-decode-map
;; quail-install-map
;; quail-use-package
;; quail-show-layout
;; quail-add-package
;; quail-completion-1
;; quail-input-method
;; quail-define-rules
;; quail-delete-region
;; quail-show-guidance
;; quail-translate-key
;; quail-hide-guidance
;; quail-do-conversion
;; quail-overlay-plist
;; quail-other-command
;; quail-kbd-translate
;; quail-select-current
;; quail-select-package
;; quail-map-definition
;; quail-setup-overlays
;; quail-define-package
;; quail-delete-overlays
;; quail-get-translation
;; quail-update-guidance
;; quail-keyseq-translate
;; quail-translation-help
;; quail-maximum-shortest
;; quail-get-translations
;; quail-prev-translation
;; quail-start-conversion
;; quail-defrule-internal
;; quail-next-translation
;; quail-abort-translation
;; quail-conversion-keymap
;; quail-start-translation
;; quail-minibuffer-message
;; quail-translation-keymap
;; quail-keyboard-translate
;; quail-update-translation
;; quail-terminate-translation
;; quail-char-equal-p
;; quail-map-from-table
;; quail-gen-decode-map
;; quail-gen-decode-map1
;; quail-get-current-str
;; quail-insert-decode-map
;; quail-install-decode-map

;; quail-self-insert-command
;; quail-set-keyboard-layout
;; quail-make-guidance-frame
;; quail-setup-completion-buf
;; quail-require-guidance-buf
;; quail-show-keyboard-layout
;; quail-exit-from-minibuffer
;; quail-overlay-region-events
;; quail-next-translation-block
;; quail-conversion-delete-tail
;; quail-conversion-delete-char
;; quail-update-keyboard-layout
;; quail-mouse-choose-completion
;; quail-conversion-forward-char
;; quail-conversion-backward-char
;; quail-update-translation-function
;; quail-update-current-translations
;; quail-map-from-table-1
;; quail-map-from-table-2
;; quail-store-decode-map-key
;; quail-input-string-to-events
;; quail-conversion-end-of-region
;; quail-point-in-conversion-region
;; quail-conversion-beginning-of-region
;; quail-help-insert-keymap-description
;; quail-conversion-backward-delete-char
;; ad-Advice-quail-show-key
;; quail-lookup-key
;; quail-delete-last-char
;; quail-forget-last-selection
;; quail-completion-list-translations
;; quail-lookup-map-and-concat
;; quail-update-leim-list-file

      ;; 由于使用了pyim+flypy，所以就不单独设置默认输入法了。通过函数来切就解决将计就计换。
; (global-set-key (kbd "C-\\") 'toggle-pyim-and-flypy)

      ;;为 flypy 添加pyim的功能
      ;; 直接把探针函数抄过来，然后触发 toogle-input-method就好了。
      ;; 现在除了不能显示

      ;; (setcar (nth 1 mode-line-mule-info) t)
      ;; (defun my:fix-inp (&rest r)
      ;;   (unless current-input-method-title
      ;;     (setq current-input-method-title " EN ")))
      ;; (advice-add 'toggle-input-method :after 'my:fix-inp)
      ;; (advice-add 'set-input-method :after 'my:fix-inp)
      ;; (advice-add 'deactivate-input-method :after 'my:fix-inp)

       ;; (defun auto-toggle-input-method ()
       ;;   (if (and (pyim-probe-auto-english)
       ;;            (pyim-probe-dynamic-english)
       ;;            )
       ;;       (deactivate-input-method)
       ;;     (activate-default-input-method)
       ;;       )
       ;;   )
       ;; ;; (advice-add 'self-insert-command :after #'auto-toggle-input-method)
       ;; (advice-add 'quail-self-insert-command :after #'auto-toggle-input-method)
      ;; (advice-add 'quail-self-insert-command :after #'auto-toggle-input-method)

      ;;  ;; (pyim-string-match-p "[](){}:\/<>!@#$%^&*-+=_;\'\"[]" (pyim-char-before-to-string 0))
       ;; (pyim-string-match-p "[](){}:\/<>!@#$%^&*-+=_;\'\"[]" (pyim-char-after-to-string 0))
      ;; (add-hook 'post-self-insert-hook 'flypy-switch-2)
     ;; (add-hook 'evil-insert-state-entry-hook 'deactivate-current-input-method)

      ;; (defun flypy-switch-3 (orig-fun &rest args)
      ;;   (interactive "*")
      ;;   (let ((res (apply orig-fun args)))
      ;;     (if (pyim-probe-dynamic-english)
      ;;         (progn
      ;;           (deactivate-input-method)
      ;;           (message "english")
      ;;           res
      ;;           )
      ;;       (progn
      ;;         (activate-default-input-method)
      ;;         (message "chinese")
      ;;         (advice-remove 'quail-self-insert-command  #'flypy-switch-4)
      ;;         (advice-add 'quail-self-insert-command :around #'flypy-switch-4)
      ;;         )
      ;;       )
      ;;     ))

      ;; (defun flypy-switch-3 (args)
      ;;   (interactive "*")
      ;;   (if (pyim-probe-dynamic-english)
      ;;       (deactivate-input-method)
      ;;     (activate-default-input-method)
      ;;     )
      ;;   )

      ;; (defun flypy-switch-4 ()
      ;;   (interactive "*")
      ;;   (if (pyim-probe-dynamic-english)
      ;;       (deactivate-input-method)
      ;;     (activate-default-input-method)
      ;;     )
      ;;   )

      ;; (advice-add 'self-insert-command :before #'flypy-switch-3)
      ;; ;; (advice-remove 'quail-self-insert-command  #'my/self-insert-command)
      ;; (advice-add 'quail-self-insert-command :before #'flypy-switch-4)

            ;; (defun flypy-switch-4 (orig-fun &rest args)
      ;;   (interactive "*")
      ;;   (let ((res (apply orig-fun args)))
      ;;     (if (pyim-probe-dynamic-english)
      ;;         (progn
      ;;           (deactivate-input-method)
      ;;           (message "english")
      ;;           (advice-remove 'self-insert-command  #'flypy-switch-3)
      ;;           (apply 'self-insert-command args)
      ;;           (advice-add 'self-insert-command :around #'flypy-switch-3)
      ;;           )
      ;;       (progn
      ;;         (activate-default-input-method)
      ;;         (message "chinese")
      ;;         res
      ;;         )
      ;;       )
      ;;     ))

      ;; (defun my/self-insert-command (&rest args)
      ;;   (interactive "*")
      ;;     (if (pyim-probe-dynamic-english)
      ;;         (progn
      ;;           (advice-remove 'self-insert-command  #'my/self-insert-command)
      ;;           (apply 'self-insert-command args)
      ;;           (advice-add 'self-insert-command :override #'my/self-insert-command)
      ;;           )
      ;;       (progn
      ;;         (advice-remove 'quail-self-insert-command  #'my/self-insert-command)
      ;;         (activate-default-input-method)
      ;;         ;; (apply 'quail-self-insert-command args)
      ;;         (quail-self-insert-command)
      ;;         (advice-add 'quail-self-insert-command :override #'my/self-insert-command)
      ;;         )
      ;;       )
      ;;     )

      ;; ;; (advice-add 'self-insert-command :override #'my/self-insert-command)
      ;; (advice-remove 'self-insert-command  #'my/self-insert-command)
      ;; (advice-add 'quail-self-insert-command :override #'my/self-insert-command)

      ;; (advice-add 'self-insert-command :around #'flypy-switch-3)
      ;; (advice-add 'quail-self-insert-command :around #'flypy-switch-4)
      ;; (advice-remove 'self-insert-command  #'flypy-switch-3)

(provide 'flypy-asim)
;;;
;;; flypy-asim.el ends here
