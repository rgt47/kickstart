" tnoremap ZD quarto::quarto_render(output_format = "pdf")<CR>
" tnoremap ZO source("<C-W>"#")<CR>
" tnoremap ZQ q('no')<C-\><C-n>:q!<CR>
" tnoremap ZR render("<C-W>"#")
" tnoremap ZS style_dir()<CR>
" tnoremap ZX exit<CR>
" tnoremap ZZ q('no')<C-\><C-n>:q!<CR>
" tnoremap lf ls()<CR> "list files
" nnoremap <localleader>r :vert term R <CR><c-w>:wincmd p<CR>

" nnoremap <localleader>d :let @c=expand("<cword>") \| :let @d="dim(".@c.")"."\n"   \| :call term_sendkeys(term_list()[0], @d)<CR>
" nnoremap <localleader>h :let @c=expand("<cword>") \| :let @d="head(".@c.")"."\n"   \| :call term_sendkeys(term_list()[0], @d)<CR>
" nnoremap <localleader>s :let @c=expand("<cword>") \| :let @d="str(".@c.")"."\n"   \| :call term_sendkeys(term_list()[0], @d)<CR>
" nnoremap <localleader>p :let @c=expand("<cword>") \| :let @d="print(".@c.")"."\n"   \| :call term_sendkeys(term_list()[0], @d)<CR>
" nnoremap <localleader>n :let @c=expand("<cword>") \| :let @d="names(".@c.")"."\n"   \| :call term_sendkeys(term_list()[0], @d)<CR>
" nnoremap <localleader>l :let @c=expand("<cword>") \| :let @d="length(".@c.")"."\n"   \| :call term_sendkeys(term_list()[0], @d)<CR>



 " autocmd BufEnter * if &ft ==# 'r'    && !exists('b:entered') | execute('let b:entered = 1 | vert :ter  R') | endif
