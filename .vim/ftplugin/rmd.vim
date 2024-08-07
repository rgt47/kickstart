" tnoremap ZD quarto::quarto_render(output_format = "pdf")<CR>
" tnoremap ZO source("<C-W>"%")
" tnoremap ZQ q('no')<C-\><C-n>:q!<CR>
" tnoremap ZR render("<C-W>"%")<CR>
" nnoremap ZT :!R -e 'render("<C-r>%", output_format="pdf_document")'<CR>
" tnoremap ZS style_dir()<CR>
" tnoremap ZX exit<CR>
" tnoremap ZZ q('no')<C-\><C-n>:q!<CR>

" tnoremap ll ls()<CR>
" nnoremap <localleader>r :vert term R <CR><c-w>:wincmd p<CR>

" nnoremap <localleader>d :let @c=expand("<cword>") \| :let @d="dim(".@c.")"."\n"   \| :call term_sendkeys(term_list()[0], @d)<CR>
" nnoremap <localleader>h :let @c=expand("<cword>") \| :let @d="head(".@c.")"."\n"   \| :call term_sendkeys(term_list()[0], @d)<CR>
" nnoremap <localleader>s :let @c=expand("<cword>") \| :let @d="str(".@c.")"."\n"   \| :call term_sendkeys(term_list()[0], @d)<CR>
" nnoremap <localleader>p :let @c=expand("<cword>") \| :let @d="print(".@c.")"."\n"   \| :call term_sendkeys(term_list()[0], @d)<CR>
" nnoremap <localleader>n :let @c=expand("<cword>") \| :let @d="names(".@c.")"."\n"   \| :call term_sendkeys(term_list()[0], @d)<CR>
" nnoremap <localleader>l :let @c=expand("<cword>") \| :let @d="length(".@c.")"."\n"   \| :call term_sendkeys(term_list()[0], @d)<CR>



" autocmd BufEnter * if &ft ==# 'rmd'  && !exists('b:entered') | execute('let b:entered = 1 | :ter R') | endif
" autocmd BufEnter * if &ft ==# 'rmd'  && !exists('b:entered') | execute('let b:entered = 1 | :ter ++rows=5 R') | endif
