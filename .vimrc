execute pathogen#infect()

let mapleader=","

" appearance, windows & buffers
set hidden
set nowrap
set number
set numberwidth=5
set title
set ruler
set laststatus=2
set statusline=%-20.20F%m%r%h[%{&ff}]%y%=%B[%p%%][%04l/%04L:%04v]
set colorcolumn=80
set listchars=tab:>.,trail:.,extends:#,nbsp:.
set list
set lazyredraw
set relativenumber

" colors
" http://ethanschoonover.com/solarized
syntax enable
" Apparently important to get proper color in the terminal:
let g:solarized_termcolors=256
" other solarized options:
let g:solarized_termtrans=1
" let g:solarized_degrade=
" let g:solarized_italic=0
" let g:solarized_underline=0
" let g:solarized_italic=0
" let g:solarized_contrast=
" let g:solarized_visibility=low
if has('gui_running')
    set background=light
else
    set background=dark
endif
colorscheme solarized
call togglebg#map("<F5>")

" searching
set ignorecase
set smartcase
set hlsearch
set incsearch
set showmatch

" tabs and indentation
set expandtab
set shiftwidth=4
set shiftround
set tabstop=4
set smarttab
set autoindent
set copyindent

" editing
set history=1000
set undolevels=1000
set wildignore=*.swp,*.bak,*.pyc,*.class
set visualbell
set t_vb=
set noerrorbells
set fenc=utf-8
set backspace=indent,eol,start
set whichwrap=b,s,h,l,<,>,[,]
set tags=./tags;
set nobackup
set noswapfile

filetype plugin indent on

" Custom key mappings
" ,ev ,sv ,eb Quick editing of dotfiles
nmap <silent> <leader>ev :e $MYVIMRC<CR>
nmap <silent> <leader>sv :so $MYVIMRC<CR>
nmap <silent> <leader>eb :e $HOME/.bash_profile<CR>
" Toggle relative line numbering with ,n
nmap <silent> <leader>n :set relativenumber!<CR>
" Clear search string (& highlighting) when screen is redrawn with Ctrl-L
nnoremap <C-L> :let @/ = ""<CR><C-L>
" ,rw = clear all trailing whitespace from buffer
" nmap <silent> <leader>rw :%s/[ \t][ \t]*$//<CR>
nmap <silent> <leader>rw call StripTrailingWhitespace()

"
" -------------------------------------------------------------------
"
" StripTrailingWhitespace
" taken from http://spf13.com via https://github.com/thunderboltsid/vim-configuration/blob/master/vimrc
function! StripTrailingWhitespace()
    " save last search & cursor position:
    let _s=@/
    let l = line(".")
    let x = col(".")
    " strip whitespace:
    %/\s\+$//e
    " Clean up:
    let @/=_s
    call cursor(l, c)
endfunction

"
" -------------------------------------------------------------------
"
" View list of current registers
let g:vimdir = $HOME."/.vim/"
let g:register_file = $HOME."/.vim/vim_registers.txt"
let g:registers_shown = 0
function! SaveRegisters()
    execute 'redir! >' fnameescape(g:register_file)
    silent registers
    redir END
endfunction

function! UpdateRegisters()
    if g:registers_shown == 1
        call ShowRegisters()
    endif
endfunction

function! HideRegisters()
    let l:win_nr = bufwinnr(g:register_file)
    if 0 < l:win_nr
        execute l:win_nr."wincmd w"
        silent quit!
        execute "wincmd p"
    endif
    let g:registers_shown = 0
endfunction

function! ShowRegisters()
    call SaveRegisters()
    let l:win_nr = bufwinnr(g:register_file)
    if 0 < l:win_nr
        " buffer is shown in a window, so refresh the view
        execute l:win_nr."wincmd w"
        silent edit!
        execute "wincmd p"
    else
        execute "vertical botright split ".g:register_file." |vertical resize 30|set nonumber|set nowrap"
        setlocal autoread
        execute "wincmd p"
    endif
    let g:registers_shown = 1
endfunction

