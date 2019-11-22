" Vim Plug plugin manager call plug#begin('~/.local/share/nvim/plugged')

" NERDTree file manager
call plug#begin()
Plug 'scrooloose/nerdtree'
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
let NERDTreeShowHidden=1
let NERDTreeHighlightCursorline=0
let NERDTreeShowLineNumbers=1

" deoplete (asynchranous autocomplete)
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

" ALE (asynchronous lint engine)
Plug 'w0rp/ale'
let g:ale_sign_column_always = 1
let g:ale_sign_error = ''
let g:ale_sign_warning = ''
"let g:ale_sign_error = '>>'
"let g:ale_sign_warning = '!!'
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
			\             [ 'readonly', 'filename', 'modified'], ['branch']],
			\   'right': [[ 'linter_checking', 'linter_errors', 'linter_warnings', 'linter_ok' ], ['lineinfo'], ['fileformat', 'fileencoding', 'percent']]
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
let g:lightline#ale#indicator_errors = ' '
let g:lightline#ale#indicator_warnings = ' '
let g:lightline#ale#indicator_checking = ' '
let g:lightline#ale#indicator_ok = ' '

" smart completion of { [ ' " etc.
Plug 'jiangmiao/auto-pairs'

" vim-go
" Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries'}

" Gets branch name (for lightline)
Plug 'itchyny/vim-gitbranch'

" Colorscheme 
Plug 'mhartington/oceanic-next'
if (has("termguicolors"))
	set termguicolors
endif
let g:oceanic_next_terminal_bold = 1
let g:oceanic_next_terminal_italic = 1

" neovim specific plugins
if has('nvim')
	"let g:deoplete#enable_at_startup = 0
	"let g:deoplete#enable_smartcase = 1
endif

" No plugins after this point
call plug#end()
