(let ((l '("\
;;                              /~~~~~~~~~~~~\\_
;;          _+=+_             _[~  /~~~~~~~~~~~~\\_
;;         {\"\"|\"\"}         [~~~    [~   /~~~~~~~~~\\_
;;          \"\"\":-'~[~[~\"~[~  ((++     [~  _/~~~~~~~~\\_
;;               '=_   [    ,==, ((++    [    /~~~~~~~\\-~~~-.
;;                  ~-_ _=+-(   )/   ((++  .~~~.[~~~~(  {@} \\`.
;;                          /   }\\ /     (     }     (   .   ''}
;;                         (  .+   \\ /  //     )    / .,  \"\"\"\"/
;;                         \\\\  \\     \\ (   .+~~\\_  /.= /'\"\"\"\"
;;        -r.millward-     <\"_V_\">      \\\\  \\    ~~~~~~\\\\  \\
;;                                       \\\\  \\          \\\\  \\
;;                                       <\"_V_\">        <\"_V_\">
;; Ankylosaur
;;
;;
;;              \" Clankity, clankity, clankity clank!
;;                Ankylosaurus was built like a tank.
;;                Its hide was a fortress sturdy as steel,
;;                It tended to be an inedible meal. \"
;;                                       -- Jack Prelutsky
"
		   "\
;;                        (
;;                          )     (
;;                   ___...(-------)-....___
;;               .-\"\"       )    (          \"\"-.
;;         .-'``'|-._             )         _.-|
;;        /  .--.|   `\"\"---...........---\"\"`   |
;;       /  /    |                             |
;;       |  |    |                             |
;;        \\  \\   |                             |
;;         `\\ `\\ |                             |
;;           `\\ `|                             |
;;           _/ /\\                             /
;;          (__/  \\                           /
;;       _..---\"\"` \\                         /`\"\"---.._
;;    .-'           \\                       /          '-.
;;   :               `-.__             __.-'              :
;;   :                  ) \"\"---...---\"\" (                 :
;;    '._               `\"--...___...--\"`              _.'
;;      \\\"\"--..__                              __..--\"\"/
;;       '._     \"\"\"----.....______.....----\"\"\"     _.'
;;          `\"\"--..,,_____            _____,,..--\"\"`
;;                        `\"\"\"----\"\"\"`
"
		   "\
;; ............... .                             ............,
;; .............                    ,(##(/,         .........,
;; ...........                   .#.,%#%%##((((       .......,
;; .........                    * G/%%%%%%%%%###(     ........
;; .......                    #%%%#%%%%%%%%&%%%%%#      ......
;; .....            &&%###(((%%%%%%%%%%%%%%%%%%%%%#.     ...,.
;; ....             %@@@@#%#%%%%%&%%%%%%&&%%%%%%%%%#     ...,.
;; ...                     (%%%%%%%%%@%%%%%%%%%%&&     ...,,
;; ..                      .*/((#%&&&&&&@&&%%%%%%%%&     .,,,.
;; ..                       *//((//(////#&@@@@%%%&&%     .,,,,
;;                          ,/**////****(%&&%%#/&&%.     .,,,,
;;                        .***,,,*,,,***/##(##/##&@..    .,,,,
;;                        *((/(,,,*****/##(#//(&@@&*,    .,,,,
;;                          ,**,,,******//(/*((&@@%&*.,  .,,,,
;;                          ,*/(*******//((((((%@&%/%/.  .,,*,
;;                       *#(#***/**////(((((((((((#%/#((/****,
;;                  .*((##&/***/((###(((((((((/*/(%##%#####/
;;               ./((#%##%&%&&&%%%%(%(#((((((((((/(######%##((
;;              /(##%#&%%%&%&&&&%%#%(####(((/(((#(###%##%##%#(
;;            *(###%%##&%%&&&&%&%%%######(((((##/%%%###%%##%##
;;           /(##%#%&%%%&%&%%&%%%%%%%%##(((((((%%%%%#%%%%%%%##
;; ..       /(##%#%%&%%&%%@%&&%%%&%&%%%%#(((/%%%&%#%%%%%%#%%#%
;; .....   .##%#%%%%%%%&&&&&%&&%%&%%&%%%%%#%%%&%%%%%#%#%%%%%%%
;; ....... (##%#%%%%@%%&&&&&&&&&%%&%&%&&%&%%%%%%%%%%%#%%%&&&&%
;; ........((#%%#%#&&&%&%&%%%%%&&&%%%%&&&%%%##%%%%%%%%%%%@&&%&
;; ....../##(####&&%%&&&&&&&%&&%&&&%%%&&&&%%%#%%%%%%%%%%&@&&&%
;; .....,#%%##%%%%%&&&&&&&&%%%%%&&&&&%&&&&&&%%%%%%%&%%%%&@&&&%
;; ....#%&&%&&&&%%%&@&&&&&%%%%%&&&&@&&&&&&&&&%%%%%&%%%%&&@@@&%
;; ...#(######%%&&@&@@&&&&&&&%&&&&@&&&&&&&&&&&%%%&%%%&&&@@@@&%
"
		   "\
;;                                                        ..       :
;;                     .                  .               .   .  .
;;       .           .                .               .. .  .  *
;;              *          .                    ..        .
;;                            .             .     . :  .   .    .  .
;;             .                         .   .  .  .   .
;;                                          . .  *:. . .
;; .                                 .  .   . .. .         .
;;                          .     . .  . ...    .    .
;;        .              .  .  . .    . .  . .
;;                         .    .     . ...   ..   .       .               .
;;                  .  .    . *.   . .
;;     .                   :.  .           .
;;                  .   .    .    .
;;              .  .  .    ./|\\
;;             .  .. :.    . |             .               .
;;      .   ... .            |
;;  .    :.  . .   *.        |     .               .
;;    .  *.             You are here.
;;  . .    .               .             *.                         .
")))
  (setq initial-scratch-message (nth (random (length l)) l)))

;; Theme
(load-theme 'manoj-dark t)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable scroll bar and toolbar in GUI
(if (window-system)
	(progn
	  (tool-bar-mode -1)
	  (scroll-bar-mode -1)))

;; Highlight current line
(global-hl-line-mode 1)
(set-face-foreground 'hl-line nil)

;; Line wrap
(global-visual-line-mode 1)

;; Disable annoying bell
(setq ring-bell-function 'ignore)

(provide 'cosmetic)
