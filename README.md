# .emacs.d/

```bash
git clone https://github.com/bombblob/dotemacs.git ~/.emacs.d
```

## Included packages:

### [column-marker.el](https://www.emacswiki.org/emacs/ColumnMarker):

Minor mode allowing column highlighting

The 80th character in prog-mode is highlighted as per the 80 column rule

**EDIT**: removed in favor of fci-mode

### [fill-column-indicator.el](https://www.emacswiki.org/emacs/FillColumnIndicator)

Minor mode to display vertical line on some column

**NOTE**: fci-mode can be visually confusing when used in conjunction with auto-complete-mode

### [nlinum.el](http://elpa.gnu.org/packages/nlinum.html)

Minor mode like linum-mode, but uses jit-lock infrastructure to avoid some bugs

About as fast or faster than linum.el (hopefully)

### [escreen.el](https://www.emacswiki.org/emacs/EmacsScreen):

Minor mode that allows multiple tabs with different layouts

### [hl-todo.el](https://github.com/tarsius/hl-todo):

Minor mode to highlight TODO, FIXME, and a few other keywords

### [lorem-ipsum.el](https://www.emacswiki.org/emacs/LoremIpsum):

Inserts lorem ipsum text by sentence, paragraph, etc.

### [xahk-mode.el](http://xahlee.info/mswin/emacs_autohotkey_mode.html):

Major mode for autohotkey programming

## Recommended packages:

* [undo-tree](https://www.emacswiki.org/emacs/UndoTree) - Makes emacs undo work more intuitively

* [neotree](https://github.com/jaypei/emacs-neotree) - File tree plugin

* [all-the-icons](https://github.com/domtronn/all-the-icons.el) - All of the icons

* [auto-complete](https://www.emacswiki.org/emacs/AutoComplete) - Powerful autocompletion with a popup menu :ok_hand:

* [magit](https://magit.vc/) - Git version control interface

* [evil](https://www.emacswiki.org/emacs/Evil) - Provides vim-like features

# Notes:

Automatic backups are redirected to ~/.emacs.d/backups

All backups in ~/.emacs.d/backups are purged after a week

isearch set to case sensitive

dump from 42siliconvalley
