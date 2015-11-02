
(deftheme smart-mode-line-obvious "Dark theme for smart-mode-line that makes current window obvious.")

(custom-theme-set-faces
 'smart-mode-line-obvious
 '(mode-line-buffer-id ((t :inherit sml/filename :foreground nil :background nil))) 
 '(mode-line-inactive ((t :foreground "gray70" :background "#404045" :inverse-video nil)))
 '(mode-line     ((t :foreground "gray90" :background "DarkSlateGray4" :inverse-video nil)))
 '(sml/global    ((t :foreground "gray90" :inverse-video nil)))
 '(sml/modes     ((t :inherit sml/global :foreground "gray70")))
 '(sml/filename  ((t :inherit sml/global :foreground "gray90" :weight bold)))
 '(sml/prefix    ((t :inherit sml/global :foreground "gray70")))
 '(sml/read-only ((t :inherit sml/not-modified :foreground "DeepSkyBlue")))
 '(persp-selected-face ((t :foreground "ForestGreen" :inherit sml/filename)))
 '(helm-candidate-number ((t :foreground nil :background nil :inherit sml/filename))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'smart-mode-line-obvious)
