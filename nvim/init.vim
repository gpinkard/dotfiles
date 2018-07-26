" +----------------------------------------+
" | Gabriel Pinkard's neovim configuration |
" +----------------------------------------+

" Plugins and their settings

" Vim Plug plugin manager
call plug#begin('~/.local/share/nvim/plugged')

" deoplete (asynchranous autocomplete)
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
let g:deoplete#enable_at_startup = 1
let g:deoplete#enable_smartcase = 1

" NERDTree file manager
Plug 'scrooloose/nerdtree'
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let NERDTreeShowHidden=1
let NERDTreeHighlightCursorline=0
let NERDTreeShowLineNumbers=1

" vim-gitbranch (gets branch name)
Plug 'itchyny/vim-gitbranch'

" ale (asynchronous lint engine)
Plug 'w0rp/ale'
let g:ale_sign_column_always = 1
let g:ale_sign_error = '->'
let g:ale_sign_warning = '!!' 
let g:ale_echo_msg_error_str = 'ERROR'
let g:ale_echo_msg_warning_str = 'warning'
let g:ale_echo_msg_format = '[%linter%] %severity%: %s'
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

" lightline (status line)
Plug 'itchyny/lightline.vim'
let g:lightline = {
			\ 'colorscheme': 'wombat',
			\ 'separator': {'left': "\ue0b0", 'right': "\ue0b2"},
			\ 'subseparator': {'left': "\ue0b1", 'right': "\ue0b3"},
			\ 'active': {
			\   'left': [ [ 'mode', 'paste' ],
			\             [ 'readonly', 'filename', 'modified' ]],
			\   'right': [['lineinfo'], ['percent'], ['fileformat'], ['fileencoding'], ['branch']]
			\ },
			\ 'component_function': {
			\ 	'branch': 'LightlineBranch'
			\ },
			\ }

function! LightlineBranch()
	let branch = gitbranch#name()
	if branch != ''
		return ': ' . gitbranch#name()
	endif
	return ''
endfunction

" Colorscheme 
Plug 'morhetz/gruvbox'
let g:gruvbox_termcolors=16
let g:gruvbox_italic=1

" No plugins after this point
call plug#end()

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
set nowrap
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
colorscheme gruvbox
" make things mesh well with color scheme
hi Normal guibg=bg ctermbg=bg
hi SignColumn ctermbg=bg
hi CursorLineNr ctermbg=red
hi Pmenu ctermbg=bg
hi Pmenu ctermfg=green
" hi Directory ctermfg=red guibg=red
" hi file ctermfg=red guibg=red

"Leader stuff
let mapleader = "\<Space>"
"Reload config file
nnoremap <leader>R :so ~/.config/nvim/init.vim<Return>
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
" NERDTree toggle
map <leader>n :NERDTreeToggle<CR>

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
autocmd Filetype java set tabstop=4
autocmd Filetype java set shiftwidth=4

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

" neovim terminal stuff
" terminal in new tab
nnoremap <leader><Space> :terminal<Return>
tnoremap <Esc> <C-\><C-N>
