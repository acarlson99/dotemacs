# .emacs.d/

```
clone https://github.com/bombblob/dotemacs.git ~/.emacs.d
```

## Included packages:

### [column-marker.el](https://www.emacswiki.org/emacs/ColumnMarker):

Allows for character highlighting

The 80th character in prog-mode is highlighted as per the 80 column rule

### [escreen.el](https://www.emacswiki.org/emacs/EmacsScreen):

Allows multiple tabs with different layouts

### [hl-todo-mode.el](https://github.com/tarsius/hl-todo):

Highlights TODO, FIXME, and a few other keywords

### [lorem-ipsum.el](https://www.emacswiki.org/emacs/LoremIpsum):

Inserts lorem ipsum text by sentence, paragraph, etc.

```M-x lipsum-load``` loads lorem-ipsum.el

### [xahk-mode.el](http://xahlee.info/mswin/emacs_autohotkey_mode.html):

autohotkey mode

## Recommended packages:

* [undo-tree](https://www.emacswiki.org/emacs/UndoTree) - Makes emacs undo work more intuitively

* [auto-complete](https://www.emacswiki.org/emacs/AutoComplete) - Powerful autocompletion with a popup menu

* [magit](https://magit.vc/) - Git version control interface

* [evil](https://www.emacswiki.org/emacs/Evil) - Provides vim features

# Notes:

Automatic backups are redirected to ~/.emacs.d/backups

All backups in ~/.emacs.d/backups are purged after a week

isearch set to case sensitive