" Gabriel Pinkard

" Basic stuff
set number
set relativenumber
set linebreak
set textwidth=150
set showbreak=+++
set showmatch
set visualbell
set laststatus=2
set ttimeoutlen=50
set ignorecase
set smartcase
set hlsearch
set incsearch
set wildmenu 
"set nowrap
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
set updatetime=100
set noshowmode
set encoding=UTF-8
syntax enable

"Leader stuff
let mapleader = "\<Space>"
"Saving, quiting, and such
nnoremap <leader>q :q<Return>
nnoremap <leader>Q :q!<Return>
nnoremap <leader>w :w<Return>
nnoremap <leader>x :x<Return>
nnoremap <leader>e :e<Space>
" Tabs stuff
nnoremap <leader>T :tabnew<Return>
nnoremap <leader><Tab> gt
" Mimic my i3 configs tiling for splitting
nnoremap <leader>v<Return> <C-w>s 
nnoremap <leader>c<Return> <C-w>v
nnoremap <leader>j <C-w><Down>
nnoremap <leader>k <C-w><Up>
nnoremap <leader>h <C-w><Left>
nnoremap <leader>l <C-w><Right>
" Autoformat
nnoremap <leader>F gg=G

" Language specific settings + rebindings

" Python specific
autocmd FileType python inoremap ( ()<Left>
autocmd FileType python inoremap " """<Return>"""<Esc>O
autocmd FileType python inoremap ' ''<Left>
autocmd FileType python inoremap { {}<Left>
autocmd FileType python inoremap [ []<Left>
autocmd Filetype python set tabstop=4
autocmd Filetype python set shiftwidth=4

" Go specific
autocmd FileType go inoremap ( ()<Left>
autocmd FileType go inoremap { {}<Left><Return><Esc>O
autocmd FileType go inoremap " ""<Left>
autocmd FileType go inoremap ' ''<Left>
autocmd FileType go inoremap [ []<Left>
autocmd Filetype go set tabstop=2
autocmd Filetype go set shiftwidth=2

" Java specific
autocmd FileType java inoremap ( ()<Left>
autocmd FileType java inoremap { {}<Left><Return><Esc>O
autocmd FileType java inoremap " ""<Left>
autocmd FileType java inoremap ' ''<Left>
autocmd FileType java inoremap [ []<Left>
autocmd Filetype java set tabstop=2
autocmd Filetype java set shiftwidth=2

" C specific
autocmd FileType c inoremap ( ()<Left>
autocmd FileType c inoremap { {}<Left><Return><Esc>O
autocmd FileType c inoremap " ""<Left>
autocmd FileType c inoremap ' ''<Left>
autocmd FileType c inoremap [ []<Left>
autocmd Filetype c set tabstop=4
autocmd Filetype c set shiftwidth=4

" Javascript specific
autocmd FileType javascript inoremap [ []
autocmd FileType javascript inoremap { {}<Left><Return><Esc>O
autocmd FileType javascript inoremap ( ()<Left>
autocmd FileType javascript inoremap " ""<Left>
autocmd FileType javascript inoremap ' ''<Left>
autocmd Filetype javascript set tabstop=2
autocmd Filetype javascript set shiftwidth=2

" html specific
autocmd FileType html inoremap < <><left>
autocmd FileType html inoremap ' ''<left>
autocmd FileType html inoremap " ""<left>
autocmd FileType html inoremap { {}<left>
autocmd Filetype html set tabstop=2
autocmd Filetype html set shiftwidth=2

" Markdown stuff
autocmd FileType markdown inoremap * **<left>
autocmd FileType markdown set tabstop=4
autocmd Filetype markdown set shiftwidth=4
