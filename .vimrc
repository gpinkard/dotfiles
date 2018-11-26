" Gabriel Pinkard's .vimrc

" Basic stuff
set number
set relativenumber
set linebreak
set showbreak=+++
set showmatch
set ttimeoutlen=50
set ignorecase
set smartcase
set hlsearch
set incsearch
set wildmenu 
set autoindent	
set smartindent
set smarttab	
set hidden
set history=1000
set encoding=utf-8
set fileencodings=utf-8
set nohlsearch
set undolevels=1000 
set backspace=indent,eol,start
set updatetime=100
set encoding=UTF-8
syntax enable

"Leader stuff
let mapleader = "\<Space>"
"Saving, quiting, and such
nnoremap <leader>Q :q!<return>
nnoremap <leader>w :w<return>
nnoremap <leader>x :x<return>
nnoremap <leader>e :e<Space>
" Tabs stuff
nnoremap <leader>T :tabnew<return>
nnoremap <leader><Tab> gt
" Mimic my i3 configs tiling for splitting
nnoremap <leader>v<return> <C-w>s 
nnoremap <leader>c<return> <C-w>v
nnoremap <leader>j :resize -5<return>
nnoremap <leader>k :resize +5<return>
nnoremap <leader>h :vertical resize -15<return>
nnoremap <leader>l :vertical resize +15<return>
" Reload config file
nnoremap <leader>R :so ~/.vimrc<return>
" Autoformat
nnoremap <leader>F gg=G
" find and replace
nnoremap <leader>fr :%s///g<left><left><left>
" buffer stuff
nnoremap <leader>b :buffer
nnoremap <leader>B :buffers<return>

" Language specific settings + rebindings
" Python specific
autocmd FileType python inoremap ( ()<left>
autocmd FileType python inoremap " """<return>"""<esc>O
autocmd FileType python inoremap ' ''<left>
autocmd FileType python inoremap { {}<left>
autocmd FileType python inoremap [ []<left>
autocmd Filetype python set tabstop=4
autocmd Filetype python set shiftwidth=4

" Go specific
autocmd FileType go inoremap ( ()<left>
autocmd FileType go inoremap { {<return><backspace>}<esc>O
autocmd FileType go inoremap " ""<left>
autocmd FileType go inoremap ' ''<left>
autocmd FileType go inoremap [ []<left>
autocmd Filetype go set tabstop=8
autocmd Filetype go set shiftwidth=8

" Java specific
autocmd FileType java inoremap ( ()<left>
autocmd FileType java inoremap { {<return><backspace>}<esc>O
autocmd FileType java inoremap " ""<left>
autocmd FileType java inoremap ' ''<left>
autocmd FileType java inoremap [ []<left>
autocmd Filetype java set tabstop=4
autocmd Filetype java set shiftwidth=4

" garbagescript specific
autocmd FileType javascript inoremap ( ()<left>
autocmd FileType javascript inoremap { {<return><backspace>}<esc>O
autocmd FileType javascript inoremap " ""<left>
autocmd FileType javascript inoremap ' ''<left>
autocmd FileType javascript inoremap [ []<left>
autocmd Filetype javascript set tabstop=4
autocmd Filetype javascript set shiftwidth=4

" C specific
autocmd FileType c inoremap ( ()<left>
autocmd FileType c inoremap { {<return><backspace>}<esc>O
autocmd FileType c inoremap " ""<left>
autocmd FileType c inoremap ' ''<left>
autocmd FileType c inoremap [ []<left>
autocmd Filetype c set tabstop=8
autocmd Filetype c set shiftwidth=8

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
