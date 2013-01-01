" vim: foldmethod=marker foldcolumn=3
" .vimrc
" Aleksandar Dimitrov
"
" Tue May 29 17:07:37 CEST 2007
"
" Remove all existing autocmds
autocmd!

" Keymappings {{{1
"" Misc {{{2
let mapleader = ","
map ; :
nmap <F9> :diffup<CR>
nmap <leader>$ :call Preserve("%s/\\s\\+$//e")<CR>
nmap <leader>= :call Preserve("normal gg=G")<CR>
set mouse="a"
set backspace=indent,eol,start

" find files in new buffer
nnoremap gf <C-W>gf
" open file under cursor, create if necessary
nnoremap gF :view <cfile><cr>

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

map K k

""" Vim Help Files: make [Return] follow a link
autocmd FileType help nmap <buffer> <Return> <C-]>

"" Abbreviations {{{2

cabbrev Wq wq
cabbrev Q q
cabbrev W w
cabbrev mke make
cabbrev maek make
cabbrev ant Ant
cabbrev want :w<CR>:Ant

abbreviate teh the

"" Turn the file into canonical hexadecimal display {{{2
nnoremap <leader>xh :%!xxd<CR>
nnoremap <leader>xH :%!xxd -r<CR>


"" open files in same directory (vimcasts.org) {{{2
map <leader>ee :e <C-R>=expand("%:p:h") . "/" <CR>
map <leader>es :sp <C-R>=expand("%:p:h") . "/" <CR>
map <leader>et :tabe <C-R>=expand("%:p:h") . "/" <CR>

"" better ex line editing {{{2
cnoremap <C-j> <t_kd>
cnoremap <C-k> <t_ku>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>

"" Search {{{2

set gdefault  " by default, substitute everything on a line for s//
set smartcase
set incsearch

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

"" Digraphs {{{2
if has("digraphs")
     digraph ., 8230
endif

" Plugins {{{1
"" Pathogen {{{2

call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

"" Fugitive {{{2

"" Fugitive
autocmd BufReadPost fugitive://* set bufhidden=delete

map <leader>gd :Gdiff<CR>
map <leader>gc :Gcommit<CR>
map <leader>gs :Gstatus<CR>
map <leader>gw :Gwrite<CR>
map <leader>gl :Glog<CR>

"" Tagbar {{{2

set showfulltag
set tags=tags;/

nnoremap <silent> <F8> :TagbarToggle<CR>
let g:tagbar_width=25
let g:tagbar_compact=1

"" Tabularize {{{2

if exists(":Tabularize")
	nmap <Leader>t= :Tabularize /=<CR>
	vmap <Leader>t= :Tabularize /=<CR>
endif

"" Powerline {{{2
let g:Powerline_symbols = 'fancy'

"" Syntastic {{{2
let g:syntastic_mode_map = { 'mode' : 'active'
                         \ , 'active_filetypes': ['latex','c']
                         \ , 'passive_filetypes': ['haskell'] }

"" GUndo {{{2
nmap <F5> :GundoToggle<CR>

" Interface {{{1
"" Misc {{{2
"
" Scroll with context"
set scrolloff=3
set sidescrolloff=3

if ((has('syntax') && (&t_Co > 2)) || has('gui_running'))
     syntax on
endif

set showmatch mat=3 " Show matching parens for 300ms
colorscheme lucius
set background=dark
set showcmd
set ruler


set lazyredraw
set shortmess=ilmnrwxIat
set nocompatible
set cursorline

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


"" Tabbing {{{2
if exists("&showtabline")
	set showtabline=2 " always show a tabline
	set switchbuf=usetab
endif

"" Line numbers {{{2
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

"" Listchars {{{2
" Stolen from http://docs.google.com/View?docid=dfkkkxv5_65d5p3nk
" This enables you to see tab characters and stray whitespace

" Show tabs and trailing whitespace visually
if (&termencoding == "utf-8") || has("gui_running")
     set list listchars=tab:▸\ ,trail:·,extends:…,nbsp:‗,eol:¶
else
     set list listchars=tab:>\ ,trail:.,extends:>,nbsp:_,eol:¶
endif

nmap <leader>l :set list!<CR>

"" Status bar {{{2
set laststatus=2
set statusline=%t\ %y
set statusline+=%=                           " right align
set statusline+=%{fugitive#statusline()}%l/%L,%c%m(%P)

"" Indentation {{{2
filetype indent on " Turn on indentation - always
set smarttab
set ai " No, not Artificial Intelligence...
set ts=8 noet " we want tabs and by default they are 8 spaces long. And TABS!
set cinoptions+=(0,u0 " paren indentation

"" Globbing {{{2
set wildmenu
set wildignore=*.o,*.obj,*.bak,*.exe,*.so,*.class,*~,*.hi
set wildmode=longest,list:longest,full
set suffixes=.bak,~,.o,.h,.info,.swp,.obj,.class

"" Folding {{{2

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

" Languages {{{1
"" JSON {{{2

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

"" Java {{{2
let java_highlight_all = 1
" Anonymous classes
set cinoptions+=j1

autocmd FileType java set sw=5 ts=5

""" Eclim {{{3
let g:EclimBrowser='firefox'

if filereadable("/mach_kernel") " in which case we're on the Mac
     let g:EclimHome = '/Users/aleks/local/eclipse/plugins/org.eclim_1.6.1'
     let g:EclimEclipseHome = '/Users/aleks/local/eclipse'
endif

autocmd FileType java nmap <leader>i :JavaImport<CR>
autocmd FileType java nmap <leader>d :JavaDocSearch<CR>
autocmd FileType java nmap <Leader>c :JavaCorrect<CR>
autocmd FileType java nnoremap <silent> <buffer> <CR> :JavaSearchContext<CR>
autocmd FileType java nnoremap <silent> <buffer> <Leader>j :lne<CR>
autocmd FileType java nnoremap <silent> <buffer> <Leader>k :lpre<CR>
autocmd FileType java nnoremap <silent> <buffer> <Leader><Space> :lopen<CR>
autocmd FileType java nnoremap <silent> <buffer> <Tab> :call eclim#util#FillTemplate("${", "}")<cr>
autocmd FileType java nnoremap <silent> <buffer> <F11> :Sign<CR>

"" Lua {{{2

autocmd FileType lua set sw=5 ts=5

"" Prolog {{{2

hi Flicker ctermfg=white cterm=bold
au BufNewFile,BufRead *.pl set filetype=prolog "Perl sucks anyway
au FileType prolog au CursorMoved * exe 'match Flicker /\V\<'.escape(expand('<cword>'), '/').'\>/'
au FileType prolog setlocal suffixesadd=.pl,.plt

"" Haskell {{{2

""" Haskell
" WARNING: this seems to fail in Haskell code when you move around the string
" (\()
" au FileType haskell au CursorMoved * exe 'match ModeMsg /\V\<'.escape(expand('<cword>'), '/').'\>/'
au BufEnter *.cabal,*.hs set expandtab shiftwidth=4

au FileType haskell let b:ghcmod_use_basedir = getcwd()

nnoremap <leader>ht :GhcModType<CR>
nnoremap <leader>hc :GhcModTypeClear<CR>
nnoremap <leader>hw :GhcModCheckAndLintAsync<CR>
nnoremap <leader>he :GhcModExpand<CR>

let g:ghcmod_use_basedir = getcwd()

"" Fruit salad is tasty.
let hs_highlight_all_types = 1
let hs_highlight_debug = 1
let hs_highlight_toplevel_fundefs = 1

"" Perl {{{2

let perl_extended_vars=1 " highlight advanced perl vars inside strings

"" C, C++ {{{2
autocmd BufEnter  *.c,*.h	abbr FOR for (i = 0; i < 3; ++i)<CR>{<CR>}<Esc>O
autocmd BufLeave  *.c,*.h	unabbr FOR

au BufEnter *.c,*.h set shiftwidth=5 tabstop=5

"" PHP {{{2

autocmd FileType php let php_folding=1

"" Vimscript {{{2

let g:vimsyn_folding='af'

function! ShowSynStack()
	for id in synstack(line("."), col("."))
		echo synIDattr(id, "name")
	endfor
endfunction


"" Helper functions for writing syntax highlighting code for Vim
nnoremap <Leader><S-S> :call ShowSynStack()<CR>
nnoremap <Leader>s :echo synIDattr(synID(line("."), col("."), 1), "name")<CR>

"" TeX {{{2
au BufNewFile *.tex 0read ~/.vim/skellies/tex
let g:tex_flavor = "latex"
autocmd FileType tex,latex,plaintex iabbrev ... \ldots{}
set grepprg=grep\ -nH\ $*
au FileType tex setlocal iskeyword+=:

"" Text files {{{2
autocmd FileType mail,text,html,xhtml,plaintex,tex,latex setlocal textwidth=80 sw=2 " WordWrap for 'text' files @ 80
set ignorecase
set infercase
autocmd FileType mail,text,plaintex,tex,latex setlocal spell spelllang=en_US
autocmd FileType mail,text nmap <F8> :set spelllang=

" Misc {{{1

" Persistent undo (since 7.3)
if ('persistent_undo')
	set undofile
	set undodir=~/.vim/undo
endif

" Enable a nice big viminfo file
set viminfo='1000,f1,:1000,/1000
set history=500
set noerrorbells " No concerts, please
set backup
set complete=.,b,u,],i,d,w

" Save and return to normal mode on FocusLost
au FocusLost * :silent! wall " save
au FocusLost * call feedkeys("\<C-\>\<C-n>") " return to normal mode

" Functions {{{1
" Exec command, preserving search pattern and cursor position. {{{2
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

"" Number conversion functions {{{2
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

