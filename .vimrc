set nocompatible    "nicht auf vi standards bestehen
syntax on
set tabstop=4       "tab breite auf 4
set backspace=2     "Backspace über zeilenumbrücke und autoeinzug
set autoindent
set noexpandtab     "Tabs nicht in spaces umwandeln
set number          "Zeilennummern anzeigen
"set backup			"Sicherheitskopien der bearbeiteten Dateien anlegen
set hlsearch 		"Markiert bei der Suche alle gefundenen Textstellen
set incsearch		"Während der Eingabe des Suchtextes schon anfangen zu suchen
set laststatus=1	"Statuszeile dauernd anzeigeset ruler
set ruler			"Position des Cursors in der Statuszeile anzeigen
set showmode		"Zeigt den aktuellen Modus am unteren Rand an
set showmatch		"Befindet sich der Cursor über einer Klammer, oder wird eine eingegeben, so wird die dazugehörige Klammer entsprechend farbig hervorgehoben."
set spelllang=en,de	"Angabe der Sprachen für die Rechtschreibprüfung.
filetype on			"Automatische Erkennung des Dateityps
set foldmethod=marker
set mouse=a
"colorscheme elflord

"let g:zenburn_high_Contrast = 1
"let g:zenburn_alternate_Visual = 1
"colors zenburn

filetype plugin on
set ofu=syntaxcomplete#Complete
