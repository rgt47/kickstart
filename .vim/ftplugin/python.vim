autocmd BufEnter * if &ft ==# 'python'    && !exists('b:entered') | execute('let b:entered = 1 | :ter ++rows=5 python3') | endif

au BufNewFile,BufRead *.py
    \ set tabstop=4
    \ set softtabstop=4
    \ set shiftwidth=4
    \ set textwidth=79
    \ set expandtab
    \ set autoindent
    \ set fileformat=unix
