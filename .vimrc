" My vimrc

" Vundle plugin manager stuff
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

" Plugins go here

" NERDtree filemanager
Plugin 'scrooloose/nerdtree'
"autocmd vimenter * NERDTree " automatically start nerdtree on vim startup
"autocmd StdinReadPre * let s:std_in=1 " next two lines open nerdtree if no file specified vim
"autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
"line below close nertree if nerdtree is only window open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
map <C-n> :NERDTreeToggle<CR>

" vim-airline status table
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline_theme='gruvbox'
let g:airline_powerline_fonts=1

" gruvbox 
Plugin 'morhetz/gruvbox'

" All plugins must end after this point :o
call vundle#end()
filetype plugin indent on
" Basic stuff
set number	
set linebreak
set showbreak=+++
set textwidth=100
set showmatch
set visualbell
set laststatus=2
set ttimeoutlen=50
set smartcase
set ignorecase
set hlsearch
set incsearch
set wildmenu 
set autoindent	
set smartindent
set smarttab	
set hidden
set history=1000
set wildmenu
set title
set encoding=utf-8
set fileencodings=utf-8
set nohlsearch
set undolevels=1000 
set backspace=indent,eol,start
syntax enable
set background=dark
colorscheme gruvbox 
hi Normal guibg=NONE ctermbg=NONE

" Python specific
autocmd FileType python inoremap ( ()<Esc>i
autocmd FileType python inoremap " ""<Esc>i
autocmd FileType python inoremap { {}<Esc>i
autocmd FileType python inoremap [ []
autocmd FileType python inoremap : :<Esc>o
set tabstop=2
set shiftwidth=2

" Java specific
autocmd FileType java inoremap ( ()<Esc>i
autocmd FileType java inoremap { {<Esc>o}<Esc>O
autocmd FileType java inoremap " ""<Esc>i
autocmd FileType java inoremap ' ''<Esc>i
autocmd FileType java inoremap [ []<Esc>i
set tabstop=2
set shiftwidth=4    

" Javascript specific
autocmd FileType javascript inoremap [ []
autocmd FileType javascript inoremap ( ()<Esc>i
autocmd FileType javascript inoremap { {}<Esc>i
autocmd FileType javascript inoremap " ""<Esc>i
set tabstop=2
set shiftwidth=2

" html specific
"autocmd FileType html inoremap < < > <Esc>i
autocmd FileType html set tabstop=2
set shiftwidth=2
runtime macros/matchit.vim
