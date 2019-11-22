" LEADER STUFF

let mapleader = "\<Space>"

"Saving, quiting, and such
nnoremap <leader>w :w<return>
nnoremap <leader>q :q<return>
nnoremap <leader>Q :q!<return>
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
nnoremap <leader>R :so ~/.config/nvim/init.vim<return>

" Autoformat
"nnoremap <leader>af gg=G
nnoremap <leader>af :call AutoFormat()<return>

" find and replace
nnoremap <leader>fr :%s///g<left><left><left>

nnoremap <leader>B :buffers

" NERDTree toggle
nnoremap <leader>n :NERDTreeToggle<CR>

" open neovim terminal
nnoremap <leader><space> :terminal<return>
nnoremap <leader>; <C-w>s<return>:terminal<return>i
tnoremap <esc> <C-\><C-N>
