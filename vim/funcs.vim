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
