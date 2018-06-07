" +----------------------------------------+
" | Gabriel Pinkard's neovim configuration |
" +----------------------------------------+

" Plugins and their settings

" Vim Plug plugin manager
call plug#begin('~/.local/share/nvim/plugged')

" deoplete (asynchranous autocomplete)
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
let g:deoplete#enable_at_startup = 1

" NERDTree file manager
Plug 'scrooloose/nerdtree'
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let NERDTreeShowHidden=1
map <C-n> :NERDTreeToggle<CR>

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
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline#extensions#ale#enabled = 1
let g:airline_theme='base16'

" Colorscheme 
Plug 'mhartington/oceanic-next'

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
set wildmenu
set title
set encoding=utf-8
set fileencodings=utf-8
set nohlsearch
set undolevels=1000 
set backspace=indent,eol,start
" Oceanic next colorscheme stuff
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
if (has("termguicolors"))
	set termguicolors
endif

syntax enable
colorscheme OceanicNext
" for ALE gutter color
highlight SignColumn ctermbg=bg
" enable transparency if term has transparency
map <F12> :hi Normal guibg=NONE ctermbg=NONE<CR>

"Leader stuff
let mapleader = "\<Space>"
" faster than holding down shift
nnoremap <leader><Space> :
"Reload config file
nnoremap <leader>R :so ~/.config/nvim/init.vim<Return>
"Saving, quiting, and such
nnoremap <leader>q :q<Return>
nnoremap <leader>w :w<Return>
nnoremap <leader>x :x<Return>
nnoremap <leader>e :e<Space>
" Tabs stuff
nnoremap <leader>T :tabnew<Return>
nnoremap <leader><Tab> gt<Return>
" Mimic my i3 configs tiling for splitting
nnoremap <leader>v<Return> <C-w>s 
nnoremap <leader>c<Return> <C-w>v
" Autoformat
nnoremap <leader>F gg=G
" NERDTree toggle
map <leader>n :NERDTreeToggle<CR>

" Language specific settings + rebindings

" Python specific
let python_highlight_all = 1
autocmd FileType python inoremap ( ()<Left>
autocmd FileType python inoremap " """<Return>"""<Esc>O
autocmd FileType python inoremap ' ''<Left>
autocmd FileType python inoremap { {}<Left>
autocmd FileType python inoremap [ []<Left>
set tabstop=2
set shiftwidth=2

" Java specific
autocmd FileType java inoremap ( ()<Left>
autocmd FileType java inoremap { {}<Left><Return><Esc>O
autocmd FileType java inoremap " ""<Left>
autocmd FileType java inoremap ' ''<Left>
autocmd FileType java inoremap [ []<Left>
set tabstop=2
set shiftwidth=4    

" C specific
autocmd FileType c inoremap ( ()<Left>
autocmd FileType c inoremap { {}<Left><Return><Esc>O
autocmd FileType c inoremap " ""<Left>
autocmd FileType c inoremap ' ''<Left>
autocmd FileType c inoremap [ []<Left>
set tabstop=2
set shiftwidth=4    

" Javascript specific
autocmd FileType javascript inoremap [ []
autocmd FileType javascript inoremap { {}<Left><Return><Esc>O
autocmd FileType javascript inoremap ( ()<Left>
autocmd FileType javascript inoremap " ""<Left>
autocmd FileType javascript inoremap ' ''<Left>
set tabstop=2
set shiftwidth=2

" html specific
autocmd FileType html inoremap < <><left>
set tabstop=2
set shiftwidth=2
