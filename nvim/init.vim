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

" Plug 'Shougo/denite.nvim', { 'do': ':UpdateRemotePlugins' }

" Plug 'Shougo/defx.nvim', { 'do': ':UpdateRemotePlugins' }

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

" latex-suite
Plug 'vim-latex/vim-latex'

" Colorscheme 
Plug 'morhetz/gruvbox'
let g:gruvbox_termcolors=16
let g:gruvbox_italic=1

" No plugins after this point
call plug#end()

colorscheme gruvbox
" make things mesh well with color scheme
hi Normal guibg=bg ctermbg=bg 
hi SignColumn ctermbg=bg ctermfg=bg
hi ALEErrorSign ctermbg=bg ctermfg=red
hi ALEWarningSign ctermbg=bg ctermfg=yellow
hi Pmenu ctermbg=bg ctermfg=green
hi CursorLineNr ctermbg=bg
" hi Directory ctermfg=red guibg=red
" hi file ctermfg=red guibg=red

" NVIM leader stuff
" Reload config file
nnoremap <leader>R :so ~/.config/nvim/init.vim<Return>
" NERDTree toggle
map <leader>n :NERDTreeToggle<CR>


" neovim terminal stuff
" terminal in new tab
nnoremap <leader><Space> :terminal<Return>
tnoremap <Esc> <C-\><C-N>
