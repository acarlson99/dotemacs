# .emacs.d/

```bash
git clone https://github.com/acarlson99/dotemacs.git ~/.emacs.d
```

## Neat packages

* [undo-tree](https://www.emacswiki.org/emacs/UndoTree) - Makes emacs undo work more intuitively

* [neotree](https://github.com/jaypei/emacs-neotree) - File tree plugin

* [auto-complete](https://www.emacswiki.org/emacs/AutoComplete) - Powerful autocompletion with a popup menu :ok_hand:

* [flycheck](https://www.flycheck.org/) - Linting and syntax checking.  May require other packages (pylint, etc) to be useful depending on language

* [magit](https://magit.vc/) - Git version control interface

* [evil](https://www.emacswiki.org/emacs/Evil) - Provides vim-like features

* [tramp](https://www.emacswiki.org/emacs/TrampMode) - Open and edit remote files

* [counsel spotify](https://github.com/Lautaro-Garcia/counsel-spotify) - Handy Emacs Spotify interface if you're into that sort of thing

* [projectile](https://github.com/bbatsov/projectile) - Excellent project management tool

## Included packages

* [column-marker.el](https://www.emacswiki.org/emacs/ColumnMarker)  
Minor mode allowing column highlighting  
The 80th character in prog-mode is highlighted as per the 80 column rule  
**EDIT**: removed in favor of fci-mode

* [fill-column-indicator.el](https://www.emacswiki.org/emacs/FillColumnIndicator)  
Minor mode to display vertical line on some column  
**NOTE**: fci-mode can be visually confusing when used in conjunction with auto-complete-mode

* [nlinum.el](http://elpa.gnu.org/packages/nlinum.html)  
Minor mode like linum-mode, but uses jit-lock infrastructure to avoid some bugs  
About as fast or faster than linum.el (hopefully)

* [escreen.el](https://www.emacswiki.org/emacs/EmacsScreen)  
Minor mode that allows multiple tabs with different layouts

* [hl-todo.el](https://github.com/tarsius/hl-todo)  
Minor mode to highlight TODO, FIXME, and a few other keywords

* [sql-upcase.el](https://www.emacswiki.org/emacs/SqlUpcase)  
Minor mode to convert lowercase SQL keywords to uppercase

* [xahk-mode.el](http://xahlee.info/mswin/emacs_autohotkey_mode.html)  
Major mode for autohotkey programming

# Notes

__DO NOT WORRY IF YOUR BACKUPS SEEM TO BE MISSING__

Automatic backups are redirected to ~/.emacs.d/backups

All backups in ~/.emacs.d/backups are purged after a week

isearch set to case sensitive

dump from 42siliconvalley

Actually read config files and figure out what is going on

Imports shell PATH from default shell
