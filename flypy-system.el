;;; flypy-system.el -*- lexical-binding: t; -*-

;;; Commentary:

;;

;;; Code:
(defvar flypy-system-switch-english nil)
(defvar flypy-system-siwtch-chinese nil)


(defun flypy-system-switch-english ()
  (funcall flypy-system-switch-english))

(defun flypy-system-switch-chinese ()
  (funcall flypy-system-switch-chinese))

(defun flypy-system-activate (&optional arg)
  "注册输入法时的函数会调用该函数,如果输入法激活的时候,就会间接调用这个函数
主要就是把 deactivate-current-input-method-function 设置为 flypy-system-deactivate
 这样的话,当切换输入法的时候会调用 deactivate-input-method 函数,该函数会 funcall
deactivate-current-input-method-function 实现关闭系统输入法为英文.
"
  (if (and arg
	   (< (prefix-numeric-value arg) 0))
      (progn
	(setq describe-current-input-method-function nil)
	(flypy-system-switch-english))
    (setq deactivate-current-input-method-function #'flypy-system-deactivate)))

(defun flypy-system-deactivate ()
  (interactive)
  (flypy-system-activate -1))

(defun flypy-system-switch-im (name)
  "注册的时候会用的到,然后激活该输入法的时候会调用该函数."
  (when (string= name "english-system") (flypy-system-switch-english))
  (when (string= name "chinese-system") (flypy-system-switch-chinese))
  ;; 激活函数
  (flypy-system-activate)
  )
;;;###autoload
(defun flypy-system-register ()
  (register-input-method "english-system" "English" 'flypy-system-switch-im "sen" "系统英文输入法")
  (register-input-method "chinese-system" "Chinese" 'flypy-system-switch-im "scn" "系统中文输入法"))



;;; flypy-system.el ends here.
(provide 'flypy-system)
