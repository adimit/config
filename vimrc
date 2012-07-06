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

" easier use of fugitive
map <leader>gd :Gdiff<CR>
map <leader>gc :Gcommit<CR>
map <leader>gs :Gstatus<CR>
map <leader>gw :Gwrite<CR>
map <leader>gl :Glog<CR>

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

"" Don't hlsearch, but when I do search for something set hlsearch, then
"" remove it when entering insert mode
set nohlsearch
noremap <leader>sss /
map / :set hlsearch<CR><leader>sss
au InsertEnter * :set nohlsearch

noremap <leader>nnn n
noremap <leader>NNN N
map n :set hlsearch<CR><leader>nnn
map N :set hlsearch<CR><leader>NNN

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

set rnu
au InsertEnter * :set nu
au InsertLeave * :set rnu
au FocusLost *   :set nu
au FocusGained * :set rnu

function! g:ToggleNuMode()
     if (&rnu == 1)
	  set nu
     else
	  set rnu
     endif
endfun

" better command line editing
cnoremap <C-j> <t_kd>
cnoremap <C-k> <t_ku>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>

set showtabline=1

" Scroll with context"
set scrolloff=3
set sidescrolloff=3

""" Graphics

set cursorline

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

nmap <F9> :diffup<CR>

if ((has('syntax') && (&t_Co > 2)) || has('gui_running'))
     syntax on
endif

" Custom digraphs. See all with :digraphs
if has("digraphs")
     digraph ., 8230
endif

set mouse="a"

colorscheme lucius
set background=dark

set showcmd
set ruler

""" Utility Functions:
"" Print decimal number in Binary
fun! Dec2Bin(nr)
     return Dec2Base(a:nr,2)
endfun

"" Print decimal number in hexadecimal
fun! Dec2Hex(nr)
     return Dec2Base(a:nr,16)
endfun

"" Print a number in a certain base (max 16)
fun! Dec2Base(nr,base)
     let n=a:nr
     let r = ""
     while n
          let r = '0123456789ABCDEF'[n % a:base] . r
          let n = n / a:base
     endwhile
     return r
endfun

"" Print characters in binary (separated by dashes)
fun! String2Bin(str)
     return PrintChars(a:str,function("Dec2Bin"))
endfun

"" Print characters in hexadecimal
fun! String2Hex(str)
     return PrintChars(a:str,function("Dec2Hex"))
endfun

"" Print characters in given base
fun! PrintChars(str,f)
     let out = ''
     for ix in range(strlen(a:str))
          let out = out . '-' . a:f(char2nr(a:str[ix]))
     endfor
     return out[1:]
endfun

" Status bar
set laststatus=2
set statusline=%t\ %y
set statusline+=%=                           " right align
set statusline+=%{fugitive#statusline()}%l/%L,%c%m(%P)

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

" Make Y behave like other capitals
map Y y$

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

" Save and return to normal mode on FocusLost
au FocusLost * :silent! wall " save
au FocusLost * call feedkeys("\<C-\>\<C-n>") " return to normal mode

""" Tags & Tagbar
set showfulltag
set tags=tags;/

nnoremap <silent> <F8> :TagbarToggle<CR>
let g:tagbar_width=25
let g:tagbar_compact=1

""" Vim Help Files: make [Return] follow a link
autocmd FileType help nmap <buffer> <Return> <C-]>

""" JSON

au! BufRead,BufNewFile *.json set filetype=json 
augroup json_autocmd
  autocmd!
  autocmd FileType json set autoindent
  autocmd FileType json set formatoptions=tcq2l
  autocmd FileType json set textwidth=78 shiftwidth=2
  autocmd FileType json set softtabstop=2 tabstop=8
  autocmd FileType json set expandtab
  autocmd FileType json set foldmethod=syntax
augroup END

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
" WARNING: this seems to fail in Haskell code when you move around the string
" (\()
" au FileType haskell au CursorMoved * exe 'match ModeMsg /\V\<'.escape(expand('<cword>'), '/').'\>/'
au BufEnter *.cabal,*.hs set expandtab shiftwidth=4

nnoremap <leader>ht :GhcModType<CR>
nnoremap <leader>hc :GhcModTypeClear<CR>
nnoremap <leader>hw :GhcModCheckAndLintAsync<CR>
nnoremap <leader>he :GhcModExpand<CR>

let g:ghcmod_use_basedir = getcwd()

"" Fruit salad is tasty.
let hs_highlight_all_types = 1
let hs_highlight_debug = 1
let hs_highlight_toplevel_fundefs = 1

let g:scion_connection_setting = [ 'scion' , '/home/adimit/.cabal/bin/scion-server']
set runtimepath+=/home/adimit/.cabal/share/scion

" Using Claus Reinke's Haskell mode (http://projects.haskell.org/haskellmode-vim/)
" au BufEnter *.hs compiler ghc
" let g:haddock_browser = "/usr/bin/iceweasel"
" let g:haddock_indexfiledir = "/home/adimit/.vim/haddock/"
" WriteAndGHC writes the file and reloads tags and type information

""" Perl
let perl_extended_vars=1 " highlight advanced perl vars inside strings

""" C, C++
autocmd BufEnter  *.c,*.h	abbr FOR for (i = 0; i < 3; ++i)<CR>{<CR>}<Esc>O
autocmd BufLeave  *.c,*.h	unabbr FOR

au BufEnter *.c,*.h set shiftwidth=5 tabstop=5

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
autocmd FileType c,ant,xml,vim,php,perl setlocal ts=5 sw=5

" Persistent undo (since 7.3)
if ('persistent_undo')
	set undofile
	set undodir=~/.vim/undo
endif

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

""" Powerline
let g:Powerline_symbols = 'fancy'

""" Syntastic
let g:syntastic_mode_map = { 'mode' : 'active'
                         \ , 'active_filetypes': ['latex','c']
                         \ , 'passive_filetypes': ['haskell'] }

""" Fugitive
autocmd BufReadPost fugitive://* set bufhidden=delete

""" GUndo
nmap <F5> :GundoToggle<CR>

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
