" All plugins moved to nvim (~/.config/nvim/init.vim)

" Basic stuff...
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

autocmd FileType python inoremap ( ()<Left>
autocmd FileType python inoremap " """<Return>"""<Esc>O
autocmd FileType python inoremap ' ''<Left>
autocmd FileType python inoremap { {}<Left>
autocmd FileType python inoremap [ []<Left>
set tabstop=2
set shiftwidth=2

" Java specific
autocmd FileType java inoremap ( ()<Left>
autocmd FileType java inoremap { {<CR>}<Esc>O
autocmd FileType java inoremap " ""<Left>
autocmd FileType java inoremap ' ''<Left>
autocmd FileType java inoremap [ []<Left>
set tabstop=2
set shiftwidth=4    

" C specific
autocmd FileType c inoremap ( ()<Left>
autocmd FileType c inoremap { {<CR>}<Esc>O
autocmd FileType c inoremap " ""<Left>
autocmd FileType c inoremap ' ''<Left>
autocmd FileType c inoremap [ []<Left>
set tabstop=2
set shiftwidth=4    

" Javascript specific
autocmd FileType javascript inoremap [ []
autocmd FileType javascript inoremap ( ()<Left>
autocmd FileType javascript inoremap { {}<Left>
autocmd FileType javascript inoremap " ""<Left>
set tabstop=2
set shiftwidth=2

" html specific
autocmd FileType html inoremap < < > <left>x
autocmd FileType html set tabstop=2
set shiftwidth=2
