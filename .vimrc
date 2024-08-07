set runtimepath^=~/vimplugins
syntax enable
filetype plugin indent on
let mapleader = ","
let maplocalleader = " "
" source ~/.vimplugins
call plug#begin('~/vimplugins')
Plug 'rakr/vim-one'
Plug 'mhartington/oceanic-next'
Plug 'jasonccox/vim-wayland-clipboard'
Plug 'junegunn/vim-easy-align'
Plug 'rafi/awesome-vim-colorschemes'
Plug  'mbbill/undotree'
Plug 'jpalardy/vim-slime'
" let g:slime_target = "vimterminal"
" let g:slime_no_mappings = 1
" let g:slime_vimterminal_config = { "vertical": 1 }
" let g:slime_vimterminal_cmd = "R"
" nmap <C-c>v <Plug>SlimeConfig
" nmap <localleader>l <Plug>SlimeCellsSendAndGoToNext
" nmap <localleader>j <Plug>SlimeCellsNext
" nmap <localleader>k <Plug>SlimeCellsPrev
Plug 'lervag/vimtex'
Plug 'tpope/vim-vividchalk'
let g:vimtex_complete_close_braces=1
let g:vimtex_quickfix_mode=0
Plug 'sirver/ultisnips'
let g:UltiSnipsExpandTrigger="<C-tab>"
let g:UltiSnipsJumpForwardTrigger="<tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-tab>"
nnoremap <leader>U <Cmd>call UltiSnips#RefreshSnippets()<CR>
nnoremap <leader>u :UltiSnipsEdit<cr>
Plug 'dense-analysis/ale'
" let g:ale_completion_enabled = 1
let g:ale_virtualtext_cursor = 'disabled'
let g:ale_set_balloons = 1
highlight clear ALEErrorSign
highlight clear ALEWarningSign
let g:ale_sign_error = '●'
let g:ale_sign_warning = '.'
let g:ale_linters = {'python': ['pylsp']}
" Set this variable to 1 to fix files when you save them.
let g:ale_fix_on_save = 1
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'r': ['styler'],
\   'rmd': ['styler'],
\   'quarto': ['styler'],
\   'python': ['black','isort'],
\   'javascript': ['eslint'],
\}
nnoremap <leader>n :ALENext<CR>
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
nnoremap <leader>z :Files<CR>
nnoremap <Leader>' :Marks<CR>
nnoremap <Leader>/ :BLines<CR>
nnoremap <Leader>b :Buffers<CR>
nnoremap <Leader>r :Rg<CR>
nnoremap <Leader>s :Snippets<CR>
tnoremap <leader>b <C-w>:Buffers<cr>
tnoremap <leader>r <C-w>:Rg<cr>
tnoremap <leader>z <C-w>:Files<cr>
Plug 'junegunn/vim-peekaboo'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-commentary'
autocmd FileType quarto setlocal commentstring=#\ %s
Plug 'tpope/vim-surround'
Plug 'justinmk/vim-sneak'
let g:sneak#label = 1
let g:sneak#s_next = 1
let g:sneak#use_ic_scs = 1
Plug 'machakann/vim-highlightedyank'
Plug 'vim-airline/vim-airline'
let g:airline#extensions#ale#enabled = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#fzf#enabled = 1
Plug 'rgt47/rgt-R'




Plug 'girishji/vimcomplete'
call plug#end()
 if $COLORTERM == 'truecolor'
   set termguicolors
 endif
 set scrolloff=7
set iskeyword-=. "add '.' to break word list
set completeopt=menu,menuone,popup,noinsert,noselect
set complete+=k
set dictionary=/usr/share/dict/words
highlight Pmenu  guifg=Black guibg=cyan gui=bold
highlight PmenuSel  gui=bold guifg=White guibg=blue
set gfn=Monaco:h14
set encoding=utf-8
set lazyredraw
set autochdir
set number relativenumber
set clipboard=unnamed
set textwidth=80
set colorcolumn=80
set cursorline
set hlsearch
set incsearch
set ignorecase
set smartcase
set showmatch
set noswapfile
set hidden
set gdefault
set splitright
set wildmenu
set wildignorecase
set wildmode=list:full
" experiment to map TAB to navigate pop ups
inoremap <expr> <tab> pumvisible() ? "\<C-n>" : "\<tab>"
inoremap <expr> <S-tab> pumvisible() ? "\<C-p>" : "\<S-tab>"

nnoremap <leader>o <C-w>:b1<CR>
nnoremap <leader>t <C-w>:b2<CR>
nnoremap <leader>h <C-w>:b3<CR>
nnoremap <leader><leader> <C-w>w
nnoremap <leader>a ggVG
nnoremap <leader>m vipgq
nnoremap <leader>f :tab split<cr>
nnoremap <leader>v :edit ~/.vimrc<cr>
nnoremap <localleader><leader> <C-u>
nnoremap <localleader><localleader> <C-d>
noremap - $
noremap : ;
noremap ; :
inoremap <F10> <C-x><C-k>
inoremap <F12> <C-x><C-o>
inoremap <expr> <TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <silent> <Esc> <Esc>`^

tnoremap <F1> <C-\><C-n>
tnoremap <leader>o <C-w>:b1<CR>
tnoremap <leader>t <C-w>:b2<CR>
tnoremap <leader>h <C-w>:b3<CR>
tnoremap <leader>f <C-w>:tab split
tnoremap <leader>g <C-w>:tabc
tnoremap <leader><leader> <C-w>w
tnoremap lf ls()<CR>
" nnoremap <silent> <C-CR> :let @c = getline(".") . "\n" \|
" 			\ :call term_sendkeys(term_list()[0], @c)<CR><CR>
" vnoremap  <silent> <C-v> y \| :let @c=@" . "\n" <CR> \|
" 	\ :call term_sendkeys(term_list()[0], @c)<CR>
" vmap <silent> <C-CR> <C-v>}
" vnoremap <C-CR> :w temp.R<CR> \| :let @x = "source('temp.R')" \|
" " 		\ :call term_sendkeys(term_list()[0], @x)<CR> \| :+1<CR>}
" vnoremap <F2> :w! temp.R<CR>
" nnoremap <F3> :let @x = "source('temp.R',echo=T)" . "\n"<CR>
" nnoremap <F4> :call term_sendkeys(term_list()[0], @x)<CR>
" nnoremap <F5> :r !R -q --no-echo -e 'options(echo=F); source("temp.R",ec=T)' \|
" 		\ sed 's/^/\# /g'<CR>
" nnoremap <F6> :let @y = "sink('temp2.txt'); source('temp.R',echo=T); sink()" . "\n"<CR>
" nnoremap <F7> :call term_sendkeys(term_list()[0], @y)<CR>
" nnoremap <F8> :r !cat temp2.txt \| sed 's/^/\# /g'<CR>


" " control-j to move to next chunk
" nnoremap <C-j> /```{<CR>j
" " control-l to highlight and run current chunk
" nmap <C-l> ?```{<CR>jV/```<CR>k<C-v>/```{<CR>j/zqzq<CR>

" " control-k to move to prev chunk
" nnoremap <C-k> 2?```{<CR>j
" " control-;' to highlight from cursor to end of chunk
" nnoremap <C-h> V/```<CR>k
" " nnoremap <C-l> V3j
"
au FocusGained * :let @z=@*
 " autocmd TextChanged,TextChangedI <buffer> silent write
set updatetime=1000
autocmd CursorHold,CursorHoldI * update
autocmd FileType rmd setlocal commentstring=#\ %s
" In vim configuration file
let $FZF_DEFAULT_OPTS = '--bind "ctrl-j:down,ctrl-k:up,j:preview-down,k:preview-up"'
" test turning off autocomments
:set formatoptions-=c formatoptions-=r formatoptions-=o

" Experimental -------------------
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
colorscheme gruvbox
set background=light
let g:gruvbox_contrast_light="hard"
" set background=light
" colorscheme one
" colorscheme desert
" colorscheme carbonized-light
" colorscheme  OceanicNextLight
" set clipboard+=unnamedplus
" set background=dark
"
" abbreviations for terminal mode via tmap
tnoremap zq  quartz()
tnoremap zh  head
tnoremap zt  table
tnoremap zl  length
tnoremap \|\|  <Space>\|>
tnoremap \|_  <Space>%>%
ab zq quartz()
ab zh  head
ab zt  table
ab zl  length

" function to save yanks to sequential registers
" https://vi.stackexchange.com/questions/26818/vim-not-storing-numbered-registers
function! SaveLastReg()
    if v:event['regname']==""
        if v:event['operator']=='y'
            for i in range(8,1,-1)
                exe "let @".string(i+1)." = @". string(i)
            endfor
            if exists("g:last_yank")
                let @1=g:last_yank
            endif
            let g:last_yank=@"
        endif
    endif
endfunction

:autocmd TextYankPost * call SaveLastReg()


augroup Hiunicode
  autocmd!
  autocmd BufEnter *
      \ syntax match nonascii "[^\x00-\x7F]" |
      \ highlight nonascii ctermfg=NONE ctermbg=red
augroup END
