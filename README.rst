bionic-reading-mode.el: Speed reading minor mode
================================================

Highlight word stems in text buffers, thereby providing artificial
fixation points to improve speed reading. Inspired by a website of the
same name.

Use ``M-x bionic-reading-mode`` in a text buffer to turn on (or off) the
minor mode. Customize ``bionic-reading-highlight-face`` to modify the
default highlight. Set ``bionic-reading-overlay`` to add the highlight
to all words unconditionally.

With some modes, instead of highlighting the word stem, it might be
convenient to de-light the remaining part of a word instead. You can
achieve this effect by customizing ``bionic-reading-delight-face``.

On Hi-Dpi displays (and a good font), a configuration similar to the
following can give excellent results:

.. code:: elisp

  (require 'bionic-reading-mode)
  (set-face-attribute 'bionic-reading-highlight-face nil :weight 'unspecified)
  (set-face-attribute 'bionic-reading-delight-face nil :weight 'light)

This package is fully documented in the source and maintained through MELPA:

https://melpa.org/#/bionic-reading-mode
