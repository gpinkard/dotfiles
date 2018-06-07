"+----------------------------------------------------------+
"| Gabriel Pinkard's .vimrc                                 |
"| This is a highly minimalistic .vimrc. I now use neovim   |
"| instead of vim for actual development (https://neovim.io)|
"+----------------------------------------------------------+

" Basic stuff
set number
set linebreak
set showbreak=+++
set textwidth=100
set showmatch
set laststatus=2
set ttimeoutlen=50
set visualbell
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
" Leader stuff
let mapleader = "\<Space>"
nnoremap <leader><Space> :
nnoremap <leader>R :so ~/.vimrc<Return>
nnoremap <leader>q :q<Return>
nnoremap <leader>w :w<Return>
nnoremap <leader>x :x<Return>
nnoremap <leader>e :e<Space>
nnoremap <leader>T :tabnew<Return>
nnoremap <leader><Tab> gt<Return>
nnoremap <leader>v<Return> <C-w>s 
nnoremap <leader>c<Return> <C-w>v
nnoremap <leader>F gg=G
