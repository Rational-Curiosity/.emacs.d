(with-eval-after-load 'posframe
  (when (bug-check-function-bytecode
         'posframe-show
         "xAHFIkFAxALGIkFAxAPHIkFAxATIIkFAxAXJIkFAxAYGyiJBQMQGB8siQUDEBgjMIkFAxAYJzSJBQMQGCs4iQUDEBgvPIkFAxAYM0CJBQMQGDdEiQUDEBg7SIkFAxAYP0yJBQMQGENQiQUDEBhHVIkFAxAYS1iJBQMQGE9ciQUDEBhTYIkFAxAYV2SJBQMQGFtoiQUDEBhfbIkFAxAYY3CJBQAgGGsYGGSOGrgBgCAYbxwYZIwgGHMgGGSMIBh3JBhkjCAYeygYZI4bOAN0IBh/LBhkjhtkA3QgGIMwGGSOG5ADeCAYhzQYZI4bvAN4IBiLOBhkjCAYjzwYZIwgGJNAGGSMIBiXRBhkjCAYm0gYZIwgGJ9MGGSMIBijUBhkjCAYp1QYZIwgGKtYGGSMIBivXBhkjCAYs2AYZIwgGLdkGGSMIBi7aBhkjCAYv2wYZIwgGMNwGGSPfBjEh4CDhASHiAiHjAyHkBCEGHKiDeQHlBh0GBiKCewEGHOYGBiHnASHoAiHpIHLqBgohcYjrBiIhKewg5O0gIe4GDSHv8CGDqAHwIIKpAd7xGfJyBhBxiAqEwwEGFu8BIYPCAYkgiPESiPMGCiGI9AYR0gYe9QYOzgYmzwYn0AYo0QYp0wYp1AYq2QYn1QYt1gYu2gYsJhmyAfYGQAYXIoj3AQYmBiUGKQYoJYj4AfnGBiv6BhDHBi77Bg/8BhL9Bg7+5wYRIf/oBhMhgUAABiT1Bh+BQQAGIIFCAAYhgUMABiuBRAAGLIFFAAYtgUYABi6BRwAGL4FIAAYqgUkABiuBSgAGLIFLAAYtzAZPzQZQry4hBgsGCySIgUwAAQYUIoiBTQABBhMGJwYmBioGKSYGiIFOAIFPAAsh3iKIgVAACyGIKranhw==")
    (cl-defun posframe-show (buffer-or-name
                             &key
                             string
                             position
                             poshandler
                             width
                             height
                             min-width
                             min-height
                             x-pixel-offset
                             y-pixel-offset
                             left-fringe
                             right-fringe
                             internal-border-width
                             internal-border-color
                             font
                             foreground-color
                             background-color
                             respect-header-line
                             respect-mode-line
                             initialize
                             no-properties
                             keep-ratio
                             override-parameters
                             timeout
                             refresh
                             &allow-other-keys)
      "Pop up a posframe and show STRING at POSITION.

POSITION can be:
1. An integer, meaning point position.
2. A cons of two integers, meaning absolute X and Y coordinates.
3. Other type, in which case the corresponding POSHANDLER should be
   provided.

POSHANDLER is a function of one argument returning an actual
position.  Its argument is a plist of the following form:

  (:position xxx
   :position-info xxx
   :poshandler xxx
   :font-height xxx
   :font-width xxx
   :posframe xxx
   :posframe-width xxx
   :posframe-height xxx
   :posframe-buffer xxx
   :parent-frame xxx
   :parent-window-left xxx
   :parent-window-top xxx
   :parent-frame-width xxx
   :parent-frame-height xxx
   :parent-window xxx
   :parent-window-width  xxx
   :parent-window-height xxx
   :minibuffer-height
   :mode-line-height
   :header-line-height
   :tab-line-height
   :x-pixel-offset xxx
   :y-pixel-offset xxx)

By default, poshandler is auto-selected based on the type of POSITION,
but the selection can be overridden using the POSHANDLER argument.
The builtin poshandler functions are listed below:

1.  `posframe-poshandler-frame-center'
2.  `posframe-poshandler-frame-top-center'
3.  `posframe-poshandler-frame-top-left-corner'
4.  `posframe-poshandler-frame-top-right-corner'
5.  `posframe-poshandler-frame-bottom-center'
6.  `posframe-poshandler-frame-bottom-left-corner'
7.  `posframe-poshandler-frame-bottom-right-corner'
8.  `posframe-poshandler-window-center'
9.  `posframe-poshandler-window-top-center'
10. `posframe-poshandler-window-top-left-corner'
11. `posframe-poshandler-window-top-right-corner'
12. `posframe-poshandler-window-bottom-center'
13. `posframe-poshandler-window-bottom-left-corner'
14. `posframe-poshandler-window-bottom-right-corner'
15. `posframe-poshandler-point-top-left-corner'
16. `posframe-poshandler-point-bottom-left-corner'

This posframe's buffer is BUFFER-OR-NAME, which can be a buffer
or a name of a (possibly nonexistent) buffer.

If NO-PROPERTIES is non-nil, The STRING's properties will
be removed before being shown in posframe.

WIDTH, MIN-WIDTH, HEIGHT and MIN-HEIGHT, specify bounds on the
new total size of posframe.  MIN-HEIGHT and MIN-WIDTH default to
the values of ‘window-min-height’ and ‘window-min-width’
respectively.  These arguments are specified in the canonical
character width and height of posframe.

If LEFT-FRINGE or RIGHT-FRINGE is a number, left fringe or
right fringe with be shown with the specified width.

By default, posframe shows no borders, but users can specify
borders by setting INTERNAL-BORDER-WIDTH to a positive number.
Border color can be specified by INTERNAL-BORDER-COLOR
or via the ‘internal-border’ face.

Posframe's font as well as foreground and background colors are
derived from the current frame by default, but can be overridden
using the FONT, FOREGROUND-COLOR and BACKGROUND-COLOR arguments,
respectively.

By default, posframe will display no header-line or mode-line.
In case a header-line or mode-line is desired, users can set
RESPECT-HEADER-LINE or RESPECT-MODE-LINE to t.

INITIALIZE is a function with no argument.  It will run when
posframe buffer is first selected with `with-current-buffer'
in `posframe-show', and only run once (for performance reasons).
If INITIALIZE is nil, `posframe-default-initialize-function' will
be used as fallback; this variable can be used to set posframe
buffer gobally.

OVERRIDE-PARAMETERS is very powful, *all* the frame parameters
used by posframe's frame can be overridden by it.

TIMEOUT can specify the number of seconds after which the posframe
will auto-hide.

If REFRESH is a number, posframe's frame-size will be re-adjusted
every REFRESH seconds.

You can use `posframe-delete-all' to delete all posframes."
      (let* ((position (or (funcall posframe-arghandler buffer-or-name :position position) (point)))
             (poshandler (funcall posframe-arghandler buffer-or-name :poshandler poshandler))
             (width (funcall posframe-arghandler buffer-or-name :width width))
             (height (funcall posframe-arghandler buffer-or-name :height height))
             (min-width (or (funcall posframe-arghandler buffer-or-name :min-width min-width) 1))
             (min-height (or (funcall posframe-arghandler buffer-or-name :min-height min-height) 1))
             (x-pixel-offset (or (funcall posframe-arghandler buffer-or-name :x-pixel-offset x-pixel-offset) 0))
             (y-pixel-offset (or (funcall posframe-arghandler buffer-or-name :y-pixel-offset y-pixel-offset) 0))
             (left-fringe (funcall posframe-arghandler buffer-or-name :left-fringe left-fringe))
             (right-fringe (funcall posframe-arghandler buffer-or-name :right-fringe right-fringe))
             (internal-border-width (funcall posframe-arghandler buffer-or-name :internal-border-width internal-border-width))
             (internal-border-color (funcall posframe-arghandler buffer-or-name :internal-border-color internal-border-color))
             (font (funcall posframe-arghandler buffer-or-name :font font))
             (foreground-color (funcall posframe-arghandler buffer-or-name :foreground-color foreground-color))
             (background-color (funcall posframe-arghandler buffer-or-name :background-color background-color))
             (respect-header-line (funcall posframe-arghandler buffer-or-name :respect-header-line respect-header-line))
             (respect-mode-line (funcall posframe-arghandler buffer-or-name :respect-mode-line respect-mode-line))
             (initialize (funcall posframe-arghandler buffer-or-name :initialize initialize))
             (no-properties (funcall posframe-arghandler buffer-or-name :no-properties no-properties))
             (keep-ratio (funcall posframe-arghandler buffer-or-name :keep-ratio keep-ratio))
             (override-parameters (funcall posframe-arghandler buffer-or-name :override-parameters override-parameters))
             (timeout (funcall posframe-arghandler buffer-or-name :timeout timeout))
             (refresh (funcall posframe-arghandler buffer-or-name :refresh refresh))
             ;;-----------------------------------------------------
             (buffer (get-buffer-create buffer-or-name))
             (parent-window (selected-window))
             (parent-window-top (window-pixel-top parent-window))
             (parent-window-left (window-pixel-left parent-window))
             (parent-window-width (window-pixel-width parent-window))
             (parent-window-height (window-pixel-height parent-window))
             (position-info
              (if (integerp position)
                  (posn-at-point position parent-window)
                position))
             (parent-frame (window-frame parent-window))
             (parent-frame-width (frame-pixel-width parent-frame))
             (parent-frame-height (frame-pixel-height parent-frame))
             (font-width (default-font-width))
             (font-height (with-current-buffer (window-buffer parent-window)
                            (posframe--get-font-height position)))
             (mode-line-height (window-mode-line-height))
             (minibuffer-height (window-pixel-height (minibuffer-window)))
             (header-line-height (window-header-line-height parent-window))
             (tab-line-height (if (functionp 'window-tab-line-height)
                                  (window-tab-line-height)
                                0))
             (frame-resize-pixelwise t)
             posframe)

        (with-current-buffer buffer

          ;; Initialize
          (unless posframe--initialized-p
            (let ((func initialize))
              (when (functionp func)
                (funcall func)
                (setq posframe--initialized-p t))))

          ;; Move mouse to (0 . 0)
          (posframe--mouse-banish parent-frame)

          ;; Create posframe
          (setq posframe
                (posframe--create-posframe
                 buffer
                 :font font
                 :parent-frame parent-frame
                 :left-fringe left-fringe
                 :right-fringe right-fringe
                 :internal-border-width internal-border-width
                 :internal-border-color internal-border-color
                 :foreground-color foreground-color
                 :background-color background-color
                 :keep-ratio keep-ratio
                 :respect-header-line respect-header-line
                 :respect-mode-line respect-mode-line
                 :override-parameters override-parameters))

          ;; Insert string into the posframe buffer
          (posframe--insert-string string no-properties)

          ;; Set posframe's size
          (posframe--set-frame-size
           posframe height min-height width min-width)

          ;; Move posframe
          (posframe--set-frame-position
           posframe
           (posframe-run-poshandler
            `(;All poshandlers will get info from this plist.
              :position ,position
              :position-info ,position-info
              :poshandler ,poshandler
              :font-height ,font-height
              :font-width ,font-width
              :posframe ,posframe
              :posframe-width ,(frame-pixel-width posframe)
              :posframe-height ,(frame-pixel-height posframe)
              :posframe-buffer ,buffer
              :parent-frame ,parent-frame
              :parent-frame-width ,parent-frame-width
              :parent-frame-height ,parent-frame-height
              :parent-window ,parent-window
              :parent-window-top ,parent-window-top
              :parent-window-left ,parent-window-left
              :parent-window-width ,parent-window-width
              :parent-window-height ,parent-window-height
              :mode-line-height ,mode-line-height
              :minibuffer-height ,minibuffer-height
              :header-line-height ,header-line-height
              :tab-line-height ,tab-line-height
              :x-pixel-offset ,x-pixel-offset
              :y-pixel-offset ,y-pixel-offset))
           parent-frame-width parent-frame-height)

          ;; Delay hide posframe when timeout is a number.
          (posframe--run-timeout-timer posframe timeout)

          ;; Re-adjust posframe's size when buffer's content has changed.
          (posframe--run-refresh-timer
           posframe refresh height min-height width min-width)

          ;; Make sure not hide buffer's content for scroll down.
          (let ((window (frame-root-window posframe--frame)))
            (if (window-live-p window)
                (set-window-point window 0)))

          ;; Force raise the current posframe.
          (raise-frame posframe--frame)

          ;; Return posframe
          posframe)))))