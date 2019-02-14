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

" fzf (fuzzy finder thingi)
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
" use colorscheme colors
let g:fzf_colors =
\ { 'fg':      ['fg', 'Normal'],
  \ 'bg':      ['bg', 'Normal'],
  \ 'hl':      ['fg', 'Comment'],
  \ 'fg+':     ['fg', 'CursorLine', 'CursorColumn', 'Normal'],
  \ 'bg+':     ['bg', 'CursorLine', 'CursorColumn'],
  \ 'hl+':     ['fg', 'Statement'],
  \ 'info':    ['fg', 'PreProc'],
  \ 'border':  ['fg', 'Ignore'],
  \ 'prompt':  ['fg', 'Conditional'],
  \ 'pointer': ['fg', 'Exception'],
  \ 'marker':  ['fg', 'Keyword'],
  \ 'spinner': ['fg', 'Label'],
  \ 'header':  ['fg', 'Comment'] }
" show hidden files (this slows things down) 
let $FZF_DEFAULT_COMMAND = 'find .'

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
let g:ale_sign_error = '✗'
let g:ale_sign_warning = '➜'
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
			\   'right': [[ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ], ['lineinfo'], ['percent'], ['fileformat'], ['fileencoding'], ['branch']]
			\ },
			\ 'component_function': {
			\ 	'branch': 'LightlineBranch'
			\ },
			\ }

let g:lightline.component_expand = {
			\  'linter_checking': 'lightline#ale#checking',
			\  'linter_warnings': 'lightline#ale#warnings',
			\  'linter_errors': 'lightline#ale#errors',
			\  'linter_ok': 'lightline#ale#ok',
			\ } 

let g:lightline.component_type = {
			\     'linter_checking': 'left',
			\     'linter_warnings': 'warning',
			\     'linter_errors': 'error',
			\     'linter_ok': 'left',
			\ }

Plug 'maximbaz/lightline-ale'
let g:lightline#ale#indicator_errors = "\uf071 : "
let g:lightline#ale#indicator_warnings = "\uf05e : "
let g:lightline#ale#indicator_checking = "\uf110 "
let g:lightline#ale#indicator_ok = "\uf00c  "

" smart completion of { [ ' " etc.
Plug 'jiangmiao/auto-pairs'

" vim-go
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries'}

" latex-suite
Plug 'vim-latex/vim-latex'

" perty icons
" Plug 'ryanoasis/vim-devicons'

" Colorscheme 
Plug 'mhartington/oceanic-next'
if (has("termguicolors"))
	set termguicolors
endif
let g:oceanic_next_terminal_bold = 1
let g:oceanic_next_terminal_italic = 1

" No plugins after this point
call plug#end()

" functions
function! LightlineBranch()
	let branch = gitbranch#name()
	if branch != ''
		return 'branch: ' . gitbranch#name()
	endif
	return ''
endfunction

" Basic stuff
colorscheme OceanicNext
set number relativenumber
set linebreak
set showbreak=+++
set showmatch
set ttimeoutlen=50
set ignorecase
set smartcase
set incsearch
set wildmenu 
set autoindent	
set smartindent
set smarttab	
set hidden
set history=1000
set encoding=UTF-8
set fileencodings=utf-8
set nohlsearch
set undolevels=1000 
set backspace=indent,eol,start
set updatetime=100
set splitbelow splitright " fix dumb vim splitting
set encoding=UTF-8
set noshowmode
set cursorline
syntax enable

" LEADER STUFF
let mapleader = "\<Space>"

"Saving, quiting, and such
nnoremap <leader>w :w<return>
nnoremap <leader>q :q<return>
nnoremap <leader>Q :q!<return>
nnoremap <leader>x :x<return>
nnoremap <leader>e :e<Space>

" Tabs stuff
nnoremap <leader>T :tabnew<return>:FZF!<return>
nnoremap <leader><Tab> gt

" Mimic my i3 configs tiling for splitting
nnoremap <leader>v<return> <C-w>s 
nnoremap <leader>c<return> <C-w>v
nnoremap <leader>j :resize -5<return>
nnoremap <leader>k :resize +5<return>
nnoremap <leader>h :vertical resize -15<return>
nnoremap <leader>l :vertical resize +15<return>

" Reload config file
nnoremap <leader>R :so ~/.config/nvim/init.vim<return>

" Autoformat
nnoremap <leader>af gg=G

" find and replace
nnoremap <leader>fr :%s///g<left><left><left>

" buffer list (FZF)
nnoremap <leader>B :Buffers<return>
 
" list files (FZF)
nnoremap <leader>F :Files<return>

" list windows (FZF) 
nnoremap <leader>W :Windows<return>

" use BLines as a better 
nnoremap <leader>/ :BLines<return>

" NERDTree toggle
nnoremap <leader>n :NERDTreeToggle<CR>

" open neovim terminal
nnoremap <leader><space> :terminal<return>
nnoremap <leader>; <C-w>s<return>:terminal<return>i
tnoremap <esc> <C-\><C-N>
