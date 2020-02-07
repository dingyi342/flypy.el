;;; flypy-macos.el -*- lexical-binding: t; -*-

;;; Commentary:

;; 安装 hammerspoon 并设置命令行
;; 安装搜狗输入法,登录账号同步.使用的是双拼挂接小鹤的方案.

;;; Code:
;; 确认是否安装
(unless (executable-find "hs")
  (message "please install hammerspoon & set cmi"))


(setq flypy-macos-sogou-im "com.sogou.inputmethod.sogou.pinyin")
(setq flypy-macos-rime-im "im.rime.inputmethod.Squirrel.Rime")
(setq flypy-macos-apple-english-im "com.apple.keylayout.ABC")
(setq flypy-macos-apple-shuangpin-im "com.apple.inputmethod.SCIM.Shuangpin")

;; hs.keycodes.currentSourceID("com.sogou.inputmethod.sogou.pinyin")
;; hs.keycodes.currentSourceID("im.rime.inputmethod.Squirrel.Rime")
;; hs.keycodes.currentSourceID("com.apple.inputmethod.SCIM.Shuangpin")
;; hs.keycodes.currentSourceID("com.apple.keylayout.ABC")


(defun flypy-macos-current-im-name ()
  "太慢了啊,字符串的处理."
  (substring (shell-command-to-string "hs -c \"hs.keycodes.currentSourceID()\"") 0 -1))


(defun flypy-macos-switch-im (im)
  "swith input method to im"
  (call-process-shell-command (format "hs -c \"hs.keycodes.currentSourceID(\'%s\')\"" im) nil 0))

(defun flypy-macos-switch-apple-english ()
  "系统自带的输入法"
  (flypy-macos-switch-im flypy-macos-apple-english-im))

(defun flypy-macos-switch-sogou ()
  "搜狗输入法"
  (flypy-macos-switch-im flypy-macos-sogou-im))


(defun flypy-macos-switch-rime ()
  "rime输入法"
  (flypy-macos-switch-im flypy-macos-rime-im))

(defun flypy-macos-switch-apple-shuangpin ()
  "系统自带输入法的双拼"
  (flypy-macos-switch-im flypy-macos-apple-shuangpin-im))

;;;###autoload
(defun flypy-macos-switch-chinese ()
  (interactive)
  (flypy-macos-switch-sogou))

;;;###autoload
(defun flypy-macos-switch-english ()
  (interactive)
  (flypy-macos-switch-apple-english))

;;; flypy-macos.el ends here.
(provide 'flypy-macos)

