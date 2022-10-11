" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

Plug 'neovim/nvim-lspconfig'
Plug 'simrat39/rust-tools.nvim'

" Optional dependencies
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim', { 'tag': '0.1.0' }
" Debugging (needs plenary from above as well)
Plug 'mfussenegger/nvim-dap'
" Plugin outside ~/.vim/plugged with post-update hook
Plug 'Mofiqul/dracula.nvim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'jiangmiao/auto-pairs'
Plug 'liuchengxu/vista.vim'
Plug 'scrooloose/nerdtree'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'fatih/vim-go'
Plug 'tpope/vim-surround'
Plug 'scrooloose/nerdcommenter'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-fugitive'
" rainbow paren
Plug 'luochen1990/rainbow'
Plug 'rust-lang/rust.vim'

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'jremmen/vim-ripgrep'
Plug 'easymotion/vim-easymotion'
Plug 'rhysd/vim-clang-format'
Plug 'honza/vim-snippets'
Plug 'kyazdani42/nvim-web-devicons'
Plug 'romgrk/barbar.nvim'
Plug 'akinsho/toggleterm.nvim', {'tag' : '*'}

" Initialize plugin system
call plug#end()




" Set internal encoding of vim, not needed on neovim, since coc.nvim using some
" unicode characters in the file autoload/float.vim
set encoding=utf-8
set guifont=MonoLisa:h14
"set guifont=MonoLisa:h14
" TextEdit might fail if hidden is not set.
set hidden

set nocompatible      "don't make any effort to be compatible with vi
set clipboard=unnamed "use system default clipboard
set mousehide         "hide mouse when typing
set fileencodings=utf-8,ucs-bom,gb18030,gbk,gb2312,cp936
set linespace=4
set ruler             "show line info in status line
set cursorline        "highlight current line
set tabstop=2         " default indentation option
set shiftwidth=2
set softtabstop=2
set expandtab
set autoindent
set autoread                   " auto update file content edited by another program at the same time
set autowrite                  " Write on :next/:prev/^Z
set swapfile                   " store tmp file in another place
set backspace=indent,eol,start " backspace over pretty much anything
set ignorecase                 " search option
set incsearch
set hlsearch
set smartcase                  " UPPER case search only match upper case
set list    " show trailing whitespace
set listchars=tab:▸\ ,trail:▫
set tags=./tags;/ "search tag file from current dir to root
set showmatch " Hilight matching braces/paren/etc
set shortmess+=A " Don't bother me when a swapfile exists


" Make tab completion for files/buffers act like bash
set wildmenu
set wildignore+=*.o,*~,*.pyc,.git,node_modules  " Ignore certain files in tab-completion
" Disable archive files
set wildignore+=*.zip,*.tar.gz,*.tar.bz2,*.rar,*.tar.xz
" Use emacs-style tab completion when selecting files, etc
set wildmode=longest,list
set wrap " Enable wrapping
"set linebreak " Break long lines by word, not char
"set showbreak=↪\  " Character to precede line wraps
silent! set mouse=nvc       " Use the mouse, but not in insert mode
" none of these should be word dividers, so make them not be
set iskeyword+=_,$,@,%,#
set formatoptions=l
set lbr
" Don't reset cursor to start of line when moving around
set nostartofline
" without this setting, 7.4 is so slow
set regexpengine=2

" http://stackoverflow.com/questions/1005/getting-root-permissions-on-a-file-inside-of-vi
cmap w!! w !sudo tee >/dev/null %

"leader setting
let mapleader=','
" in case miss type
map Q <silent>
map q: <silent>
map K <silent>
"a switch to disable hightlight search result
nmap \q     :nohlsearch<CR>
map  <C-s>  :w<CR>
imap <C-s>  <ESC>:w<CR>a
"like Intellij IDEA
map  <C-F4> :wqall<CR>
imap <C-F4> <ESC>:wqall<CR>
" some emacs like key bindings
imap <C-f>         <Right>
imap <C-e>         <End>
nmap <C-e>         <End>
imap <C-b>         <Left>
imap <C-a>         <ESC>I
" plus 1 is pain in the ass
nmap <C-a>         <ESC>^
imap <A-x>         <ESC>:
nmap <A-x>         <ESC>:
imap <C-backspace> <C-w>
inoremap <C-.> ->
inoremap <D-.> ->
" rust sugar
inoremap <C-=> =>
inoremap <D-=> =>
inoremap <C-;> // 
inoremap <D-;> // 
"You should notice we use 'b' for mark anchor
nmap     <C-l>         mbz.`b
inoremap <C-l>         <ESC>mbz.`ba
"tab switch shortcut
map <F8> :bnext<CR>
map <F7> :bprevious<CR>
"quick quit
nnoremap <silent><leader>z :wq<CR>
"quick copy/paste, often you should select before
map <leader>y "+y
map <leader>p "+p
" delete all trailing spaces in all lines
" nmap <leader><space> :call whitespace#strip_trailing()<CR>
" reselect visual block after indent
vnoremap < <gv
vnoremap > >gv

" hide annoying quit message
nnoremap <C-c> <C-c>:echo<cr>


set autoread

" ====== plug config =====

" fzf
nmap <leader>p :Files<CR>

colorscheme dracula


" global rainbow mode
let g:rainbow_active = 1

" neovide
let g:neovide_transparency=1
let g:neovide_cursor_animation_length=0.13
let g:neovide_cursor_trail_length=0.8
let g:neovide_cursor_antialiasing=v:true
let g:neovide_cursor_vfx_mode = "ripple"
let g:neovide_remember_window_size = v:true
" take my emacs config to here
let g:neovide_input_use_logo = v:true


" ag(silver_searcher) vim bundle
" bind a shorcut for using silver_search to search the word under cursor
nmap <C-k> :Rg "\b<cword>\b" <CR>

" easy motion
map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)
" rust
"let g:rustfmt_autosave = 1
" tag
nmap <leader>e :GoDecls<CR>
" close other window, If you use emacs, you know what I mean
nmap <leader>1 :on<CR>
" kill current window
nmap <leader>0 :q<CR>


autocmd FileType go          inoremap <buffer> <M-=> :=
autocmd FileType go          inoremap <buffer> <D-=> :=
autocmd FileType go           nnoremap <buffer> <Leader>t :GoFmt<CR>:GoImports<CR>
autocmd FileType cpp          nnoremap <buffer> <Leader>t :ClangFormat<CR>
autocmd FileType rust         nnoremap <buffer> <Leader>t :RustFmt<CR>
autocmd FileType rust         nnoremap <buffer> <Leader>b :Cargo build<CR>
autocmd FileType rust         nnoremap <buffer> <Leader>r :Cargo run<CR>

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
" same as my emacs
" disable this two M-n M-p, to make it in myway
let g:AutoPairsShortcutJump = ''
let g:AutoPairsShortcutToggle = ''
inoremap <silent><expr> <D-n>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><D-p> pumvisible() ? "\<C-p>" : "\<C-h>"
let g:coc_snippet_next = '<D-j>'
let g:coc_snippet_prev = '<D-k>'


inoremap <S-Insert> <ESC>"+p
nnoremap <S-Insert> "+p
nnoremap <D-*> <C-o>
nnoremap <C-y> :tabnext<CR>

" golang related, don't be so smart, bitch!
let g:go_fmt_autosave = 0
let g:go_imports_autosave = 0


let g:gitgutter_sign_added = '█'
let g:gitgutter_sign_modified = '█'
highlight GitGutterAdd    guifg=#009900 ctermfg=2
highlight GitGutterChange guifg=#bbbb00 ctermfg=3
highlight GitGutterDelete guifg=#ff2222 ctermfg=1
nmap <leader>l :set nu!<CR>
nmap <leader>r :History<CR>
imap <C-Enter> <ESC>o
imap <C-o> <ESC>O



" coc config
" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: There's always complete item selected by default, you may want to enable
" no select by `"suggest.noselect": true` in your configuration file.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
" same as my emacs
inoremap <silent><expr> <D-n>
      \ coc#pum#visible() ? coc#pum#next(1):
      \ CheckBackspace() ? "\<Tab>" :
      \ coc#refresh()

inoremap <expr><D-p> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"
inoremap <expr><S-TAB> coc#pum#visible() ? coc#pum#prev(1) : "\<C-h>"
let g:coc_snippet_next = '<D-j>'
let g:coc_snippet_prev = '<D-k>'
" Make <CR> to accept selected completion item or notify coc.nvim to format
" <C-g>u breaks current undo, please make your own choice.
inoremap <silent><expr> <TAB> coc#pum#visible() ? coc#pum#confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

function! CheckBackspace() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Use `[g` and `]g` to navigate diagnostics
" Use `:CocDiagnostics` to get all diagnostics of current buffer in location list.
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
nnoremap <silent> K :call ShowDocumentation()<CR>

function! ShowDocumentation()
  if CocAction('hasProvider', 'hover')
    call CocActionAsync('doHover')
  else
    call feedkeys('K', 'in')
  endif
endfunction

" Highlight the symbol and its references when holding the cursor.
autocmd CursorHold * silent call CocActionAsync('highlight')

" Symbol renaming.
nmap <leader>rn <Plug>(coc-rename)

" Formatting selected code.
xmap <leader>f  <Plug>(coc-format-selected)
nmap <leader>f  <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " Setup formatexpr specified filetype(s).
  autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
  " Update signature help on jump placeholder.
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup end

" Applying codeAction to the selected region.
" Example: `<leader>aap` for current paragraph
xmap <leader>a  <Plug>(coc-codeaction-selected)
nmap <leader>a  <Plug>(coc-codeaction-selected)

" Remap keys for applying codeAction to the current buffer.
nmap <leader>ac  <Plug>(coc-codeaction)
" Apply AutoFix to problem on the current line.
nmap <leader>qf  <Plug>(coc-fix-current)

" Run the Code Lens action on the current line.
nmap <leader>cl  <Plug>(coc-codelens-action)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
omap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap af <Plug>(coc-funcobj-a)
xmap ic <Plug>(coc-classobj-i)
omap ic <Plug>(coc-classobj-i)
xmap ac <Plug>(coc-classobj-a)
omap ac <Plug>(coc-classobj-a)

" Remap <C-f> and <C-b> for scroll float windows/popups.
if has('nvim-0.4.0') || has('patch-8.2.0750')
  nnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  nnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
  inoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(1)\<cr>" : "\<Right>"
  inoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? "\<c-r>=coc#float#scroll(0)\<cr>" : "\<Left>"
  vnoremap <silent><nowait><expr> <C-f> coc#float#has_scroll() ? coc#float#scroll(1) : "\<C-f>"
  vnoremap <silent><nowait><expr> <C-b> coc#float#has_scroll() ? coc#float#scroll(0) : "\<C-b>"
endif

" Use CTRL-S for selections ranges.
" Requires 'textDocument/selectionRange' support of language server.
nmap <silent> <C-s> <Plug>(coc-range-select)
xmap <silent> <C-s> <Plug>(coc-range-select)

" Add `:Format` command to format current buffer.
command! -nargs=0 Format :call CocActionAsync('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocActionAsync('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings for CoCList
" Show all diagnostics.
nnoremap <silent><nowait> <space>a  :<C-u>CocList diagnostics<cr>
" Manage extensions.
nnoremap <silent><nowait> <space>e  :<C-u>CocList extensions<cr>
" Show commands.
nnoremap <silent><nowait> <space>c  :<C-u>CocList commands<cr>
" Find symbol of current document.
nnoremap <silent><nowait> <space>o  :<C-u>CocList outline<cr>
" Search workspace symbols.
nnoremap <silent><nowait> <space>s  :<C-u>CocList -I symbols<cr>
" Do default action for next item.
nnoremap <silent><nowait> <space>j  :<C-u>CocNext<CR>
" Do default action for previous item.
nnoremap <silent><nowait> <space>k  :<C-u>CocPrev<CR>
" Resume latest coc list.
nnoremap <silent><nowait> <space>p  :<C-u>CocListResume<CR>




" telescope
" Find files using Telescope command-line sugar.
nnoremap <leader>p <cmd>Telescope find_files<cr>
nnoremap <leader>s <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>


let g:LanguageClient_serverCommands = {
\ 'rust': ['rust-analyzer'],
\ }
