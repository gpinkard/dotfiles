" put basic things in .vimrc
source ~/.vimrc

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
let g:ale_sign_error = '✗'
let g:ale_sign_warning = '➜'
let g:ale_echo_msg_error_str = 'ERROR'
let g:ale_echo_msg_warning_str = 'warning'
let g:ale_echo_msg_format = '[%linter%] %severity%: %s'
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)

" lightline (status line)
" \ 'separator': {'left': "\ue0b0", 'right': "\ue0b2"},
" \ 'subseparator': {'left': "\ue0b1", 'right': "\ue0b3"},
Plug 'itchyny/lightline.vim'
let g:lightline = {
			\ 'colorscheme': 'wombat',
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

function! LightlineBranch()
	let branch = gitbranch#name()
	if branch != ''
		return 'branch: ' . gitbranch#name()
	endif
	return ''
endfunction

Plug 'maximbaz/lightline-ale'
let g:lightline#ale#indicator_errors = "\uf071 : "
let g:lightline#ale#indicator_warnings = "\uf05e : "
let g:lightline#ale#indicator_checking = "\uf110 : "
let g:lightline#ale#indicator_ok = "\uf00c  "

" vim-go
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries'}

" latex-suite
Plug 'vim-latex/vim-latex'

" Colorscheme 
Plug 'mhartington/oceanic-next'
if (has("termguicolors"))
	set termguicolors
endif
let g:oceanic_next_terminal_bold = 1
let g:oceanic_next_terminal_italic = 1

" No plugins after this point
call plug#end()
colorscheme OceanicNext

" NVIM leader stuff
" Reload config file
nnoremap <leader>R :so ~/.config/nvim/init.vim<Return>
" NERDTree toggle
map <leader>n :NERDTreeToggle<CR>
" neovim terminal stuff
" terminal in new tab
nnoremap <leader><Space> :terminal<Return>
tnoremap <Esc> <C-\><C-N>
" other basic settings I only want in nvim
set noshowmode
set cursorline
