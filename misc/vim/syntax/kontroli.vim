" Vim syntax file
" Language:        Kontroli
" Maintainer:      Michael Färber <michael.faerber@gedenkt.at>
" Last Change:     09/03/2020
" Version:         1.0
" Original Author: Michael Färber <michael.faerber@gedenkt.at>

if exists("b:current_syntax")
  finish
endif

" Keywords
syntax keyword Type Type
syntax keyword Keyword def thm
syntax match   Keyword "\["
syntax match   Keyword "\]"
syntax match   Keyword "("
syntax match   Keyword ")"
syntax match   Keyword "!"
syntax match   Keyword "\\"
syntax match   Keyword ":"
syntax match   Keyword ":="
syntax match   Keyword "=>"
syntax match   Keyword "->"
syntax match   Keyword "-->"
syntax match   Keyword ","
syntax match   Keyword "\."

" Comments
syn keyword Todo contained TODO FIXME NOTE
syn region Comment start="(;" end=";)" contains=Todo
