autocmd BufEnter * if &ft ==# 'julia'    && !exists('b:entered') | execute('let b:entered = 1 | :ter ++rows=5 julia') | endif
