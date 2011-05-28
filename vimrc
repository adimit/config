" .vimrc
" Aleksandar Dimitrov
"
" Tue May 29 17:07:37 CEST 2007
"
" Remove all existing autocmds
autocmd!

let mapleader = ","

"" Pathogen
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

" open files in same directory (courtesy to vimcasts.org)
map <leader>ee :e <C-R>=expand("%:p:h") . "/" <CR>
map <leader>es :sp <C-R>=expand("%:p:h") . "/" <CR>
map <leader>et :tabe <C-R>=expand("%:p:h") . "/" <CR>

""" Environment
"""""""""""""""

set nocompatible
" Enable a nice big viminfo file
set viminfo='1000,f1,:1000,/1000
set history=500
set noerrorbells " No concerts, please
set backup
set lazyredraw

""" Interface
"""""""""""""

set shortmess=ilmnrwxIat

if &term =~ "rxvt-unicode"
     if (&termencoding == "")
	  set termencoding=utf-8
     endif
     " change cursor colour depending on mode state (for term only)
     if exists('&t_SI')
	  let &t_SI = "\<Esc>]12;lightgoldenrod\x7"
	  let &t_EI = "\<Esc>]12;grey80\x7"
     endif
endif

" Scroll with context"
set scrolloff=3
set sidescrolloff=3

""" Graphics

" Stolen from http://docs.google.com/View?docid=dfkkkxv5_65d5p3nk
" This enables you to see tab characters and stray whitespace

" Show tabs and trailing whitespace visually
if (&termencoding == "utf-8") || has("gui_running")
     set list listchars=tab:▸\ ,trail:·,extends:…,nbsp:‗,eol:¶
else
     set list listchars=tab:>\ ,trail:.,extends:>,nbsp:_,eol:¶
endif

nmap <leader>l :set list!<CR>

function! Preserve(command)
     " Preparation: save last search, and cursor position.
     let _s=@/
     let l = line(".")
     let c = col(".")
     " Do the business:
     execute a:command
     " Clean up: restore previous search history, and cursor position
     let @/=_s
     call cursor(l, c)
endfunction
nmap <leader>$ :call Preserve("%s/\\s\\+$//e")<CR>
nmap <leader>= :call Preserve("normal gg=G")<CR>

if ((has('syntax') && (&t_Co > 2)) || has('gui_running'))
     syntax on
endif

" Custom digraphs. See all with :digraphs
if has("digraphs")
     digraph ., 8230
endif

set mouse=""
if !has("gui_running")
     colorscheme default
     set background=dark
endif

set showcmd
set ruler

" Status bar
set laststatus=2
set statusline=%t\ %y
set statusline+=%=                           " right align
set statusline+=%l/%L,%c%m(%P)

""" Tabbing
if exists("&showtabline")
	set showtabline=2 " always show a tabline
	set switchbuf=usetab
endif
" find files in new buffer
nnoremap gf <C-W>gf
" open file under cursor, create if necessary
nnoremap gF :view <cfile><cr>

""" Basic editing
set showmatch mat=3 " Show matching parens for 300ms
set incsearch
set smarttab
set ai " No, not Artificial Intelligence...
set nohlsearch

" Make backspace delete lots of things
set backspace=indent,eol,start

""" Advanced editing

" so far not used.
fun! SwitchTo(f, split) abort
     if ! filereadable(a:f)
	  echoerr "File '" . a:f . "' does not exist"
     else
	  if a:split
	       new
	  endif
	  if bufexists(a:f) != 0
	       exec ':buffer ' . bufnr(a:f)
	  else
	       exec ':edit ' . a:f
	  endif
     endif
endfun
"noremap <F12> :call SwitchTo(, a:split)

""" Searching
set gdefault  " by default, substitute everything on a line for s//
set smartcase

""" Globbing
set wildmenu
set wildignore=*.o,*.obj,*.bak,*.exe,*.so,*.class,*~,*.hi
set wildmode=list:longest,full " Command line completion shows a list first
set suffixes=.bak,~,.o,.h,.info,.swp,.obj,.class

""" Shortcuts
set pastetoggle=<F11>
nmap <C-p> :tabprevious<cr>
nmap <C-n> :tabnext<cr>
nmap <F12> :tabnew 
imap <C-z>n <Esc>:tabnext<cr>
imap <C-z>p <Esc>:tabprev<cr>
" Insert a single character and go back to command mode
noremap S i<Space><Esc>r

""" General Abbreviations
" Command Typos
cabbrev Wq wq
cabbrev Q q
cabbrev W w
cabbrev mke make
cabbrev maek make
cabbrev ant Ant
cabbrev want :w<CR>:Ant
map K k


" Spelling mistakes
abbreviate teh the

""" Code
""""""""

""" Indentation
set ts=8 noet " we want tabs and by default they are 8 spaces long. And TABS!
filetype indent on " Turn on indentation - always
" paren indentation
set cinoptions+=(0,u0

""" Path
set path=$PWD/**

""" Misc
set makeef=##error.log " Generate unique error files for make

""" Folding
set foldmethod=syntax
nmap <space> za
vmap <space> zf

set foldtext=MyFoldText()

" Folds preserving indentation and without clutter
function! MyFoldText()
     let i = indent(v:foldstart)
     return repeat(' ', i) . substitute(getline(v:foldstart), '^\s\+', '', '')
endfunction

" Reload folding and syntax (on entering the buffer)
nmap <F3> :syn sync fromstart<cr>
autocmd BufEnter * syntax sync fromstart

""" Tags & Tagbar
set showfulltag
set tags=tags;/

nnoremap <silent> <F8> :TagbarToggle<CR>
let g:tagbar_width=25
let g:tagbar_compact=1

""" Vim Help Files: make [Return] follow a link
autocmd FileType help nmap <buffer> <Return> <C-]>

""" Java
""autocmd filetype java nmap <F9> :Ant<cr>
let java_highlight_all = 1
" Anonymous classes
set cinoptions+=j1

autocmd FileType java nmap <leader>i :JavaImport<CR>
autocmd FileType java nmap <leader>d :JavaDocSearch<CR>
autocmd FileType java nmap <Leader>c :JavaCorrect<CR>
autocmd FileType java nnoremap <silent> <buffer> <CR> :JavaSearchContext<CR>
autocmd FileType java nnoremap <silent> <buffer> <Leader>j :lne<CR>
autocmd FileType java nnoremap <silent> <buffer> <Leader>k :lpre<CR>
autocmd FileType java nnoremap <silent> <buffer> <Leader><Space> :lopen<CR>
autocmd FileType java nnoremap <silent> <buffer> <Tab> :call eclim#util#FillTemplate("${", "}")<cr>
autocmd FileType java nnoremap <silent> <buffer> <F11> :Sign<CR>

autocmd FileType java set sw=5 ts=5

filetype plugin on
filetype indent on

""" Lua
autocmd FileType lua set sw=5 ts=5

""" Prolog
hi Flicker ctermfg=white cterm=bold
au BufNewFile,BufRead *.pl set filetype=prolog "Perl sucks anyway
au FileType prolog au CursorMoved * exe 'match Flicker /\V\<'.escape(expand('<cword>'), '/').'\>/'
au FileType prolog setlocal suffixesadd=.pl,.plt

""" SfS Website
autocmd BufEnter *.ssi set ft=html

""" Haskell
au FileType haskell au CursorMoved * exe 'match ModeMsg /\V\<'.escape(expand('<cword>'), '/').'\>/'
au BufEnter *.hs set expandtab shiftwidth=4

"" Fruit salad is tasty.
let hs_highlight_types = 1
let hs_highlight_more_types = 1
let hs_highlight_boolean = 1

" Using Claus Reinke's Haskell mode (http://projects.haskell.org/haskellmode-vim/)
au BufEnter *.hs compiler ghc
let g:haddock_browser = "/usr/bin/opera"
let g:haddock_indexfiledir = "/home/adimit/.vim/haddock/"
" WriteAndGHC writes the file and reloads tags and type information
function! WriteAndGHC()
     :write
     GHCi :ctags
     GHCReload
endfunction
au FileType haskell nnoremap <leader>c :exe WriteAndGHC()<CR>

""" Perl
let perl_extended_vars=1 " highlight advanced perl vars inside strings

""" C, C++
autocmd BufEnter  *.c,*.h	abbr FOR for (i = 0; i < 3; ++i)<CR>{<CR>}<Esc>O
autocmd BufLeave  *.c,*.h	unabbr FOR

""" PHP
autocmd FileType php let php_folding=1

""" Vim
let g:vimsyn_folding='af'

" function! ShowSynStack()
" 	for id in synstack(line("."), col("."))
" 		echo synIDattr(id, "name")
" 	endfor
" endfunction


"" Helper functions for writing syntax highlighting code for Vim
nnoremap <Leader><S-S> :call ShowSynStack()<CR>
nnoremap <Leader>s :echo synIDattr(synID(line("."), col("."), 1), "name")<CR>

""" TeX
au BufNewFile *.tex 0read ~/.vim/skellies/tex
let g:tex_flavor = "latex"
autocmd FileType tex,latex,plaintex iabbrev ... \ldots{}
autocmd FileType tex,latex,plaintex set makeprg=xelatex\ -interaction\ nonstopmode\ %
autocmd Filetype tex,latex,plaintex set efm=%E!\ LaTeX\ %trror:\ %m,
	\%E!\ %m,
	\%+WLaTeX\ %.%#Warning:\ %.%#line\ %l%.%#,
	\%+W%.%#\ at\ lines\ %l--%*\\d,
	\%WLaTeX\ %.%#Warning:\ %m,
	\%Cl.%l\ %m,
	\%+C\ \ %m.,
	\%+C%.%#-%.%#,
	\%+C%.%#[]%.%#,
	\%+C[]%.%#,
	\%+C%.%#%[{}\\]%.%#,
	\%+C<%.%#>%.%#,
	\%C\ \ %m,
	\%-GSee\ the\ LaTeX%m,
	\%-GType\ \ H\ <return>%m,
	\%-G\ ...%.%#,
	\%-G%.%#\ (C)\ %.%#,
	\%-G(see\ the\ transcript%.%#),
	\%-G\\s%#,
	\%+O(%f)%r,
	\%+P(%f%r,
	\%+P\ %\\=(%f%r,
	\%+P%*[^()](%f%r,
	\%+P[%\\d%[^()]%#(%f%r,
	\%+Q)%r,
	\%+Q%*[^()])%r,
	\%+Q[%\\d%*[^()])%r


""" Shell Scripts
" Autoexecutable Scripts:
" au BufWritePost * if getline(1) =~ "^#!" | silent !chmod a+x <afile>  | endif

""" Misc
" Set K&R indentation for certain file types
autocmd FileType ant,xml,vim,php,perl setlocal ts=5 sw=5
" Persistent undo (since 7.3)
set undofile
set undodir=~/.vim/undo

""" Text files
autocmd FileType mail,text,html,xhtml,plaintex,tex,latex setlocal textwidth=80 sw=2 " WordWrap for 'text' files @ 80
set ignorecase
set infercase
set complete=.,w,k
autocmd FileType mail,text,plaintex,tex,latex setlocal spell spelllang=en_US
autocmd FileType mail,text nmap <F8> :set spelllang=

""" Tabularize (from vimcasts.org)

if exists(":Tabularize")
	nmap <Leader>t= :Tabularize /=<CR>
	vmap <Leader>t= :Tabularize /=<CR>
endif

""" Plugins
"""""""""""

""" Eclim
let g:EclimBrowser='firefox'

if filereadable("/mach_kernel") " in which case we're on the Mac
     let g:EclimHome = '/Users/aleks/local/eclipse/plugins/org.eclim_1.6.1'
     let g:EclimEclipseHome = '/Users/aleks/local/eclipse'
endif

""" Man pages
runtime! ftplugin/man.vim

" autoreloading of vim config when saving it
autocmd! bufwritepost .vimrc source ~/.vimrc
