" Vim Plug plugin manager
call plug#begin('~/.local/share/nvim/plugged')

" deoplete
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
let g:deoplete#enable_at_startup = 1

" NERDTree file manager
Plug 'scrooloose/nerdtree'
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
map <C-n> :NERDTreeToggle<CR>
let NERDTreeShowHidden=1

" ale (asynchronous lint engine)
Plug 'w0rp/ale'
let g:ale_sign_column_always = 1
let g:ale_sign_error = '->'
let g:ale_sign_warning = '!!'
let g:airline#extensions#ale#enabled = 1
let g:ale_echo_msg_error_str = 'ERROR'
let g:ale_echo_msg_warning_str = 'warning'
let g:ale_echo_msg_format = '[%linter%] %severity%: %s'
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

" vim-airline + themes
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
let g:airline_powerline_fonts=1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#ale#enabled = 1
let g:airline_theme='base16'

" Colorscheme 
Plug 'mhartington/oceanic-next'
let g:enable_bold_font=1
let g:enable_italic_font=1

" No plugins after this point
call plug#end()

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
syntax on 
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
if (has("termguicolors"))
 set termguicolors
endif
let g:oceanic_next_terminal_bold = 1
let g:oceanic_next_terminal_italic = 1
colorscheme OceanicNext

" Python specific
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
