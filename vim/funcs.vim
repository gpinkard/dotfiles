" functions

" gets branch name for Lightline
function! LightlineBranch()
	let branch = gitbranch#name()
	if branch != ''
		" return 'branch: ' . branch
		return ' ' . branch
	endif
	return ''
endfunction

" autoformat and return to current line
function! AutoFormat()
	let curpos = getpos('.')
	normal gg=G
	call setpos('.', curpos)
endfunction
