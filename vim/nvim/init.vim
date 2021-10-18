" Specify a directory for plugins
" - For Neovim: stdpath('data') . '/plugged'
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')

Plug 'neovim/nvim-lspconfig'
Plug 'simrat39/rust-tools.nvim'

" Optional dependencies
Plug 'nvim-lua/popup.nvim'
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'
" Debugging (needs plenary from above as well)
Plug 'mfussenegger/nvim-dap'
" Plugin outside ~/.vim/plugged with post-update hook
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'Mofiqul/dracula.nvim'
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'jiangmiao/auto-pairs'
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

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'jremmen/vim-ripgrep'

" Initialize plugin system
call plug#end()




" Set internal encoding of vim, not needed on neovim, since coc.nvim using some
" unicode characters in the file autoload/float.vim
set encoding=utf-8

" TextEdit might fail if hidden is not set.
set hidden

" Some servers have issues with backup files, see #649.
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4 s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("nvim-0.5.0") || has("patch-8.1.1564")
  " Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Use tab for trigger completion with characters ahead and navigate.
" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" other plugin before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~# '\s'
endfunction

" Use <c-space> to trigger completion.
if has('nvim')
  inoremap <silent><expr> <c-space> coc#refresh()
else
  inoremap <silent><expr> <c-@> coc#refresh()
endif

" Make <CR> auto-select the first completion item and notify coc.nvim to
" format on enter, <cr> could be remapped by other vim plugin
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

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
nnoremap <silent> K :call <SID>show_documentation()<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  elseif (coc#rpc#ready())
    call CocActionAsync('doHover')
  else
    execute '!' . &keywordprg . " " . expand('<cword>')
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
command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
command! -nargs=? Fold :call     CocAction('fold', <f-args>)

" Add `:OR` command for organize imports of the current buffer.
command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')


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
set regexpengine=1

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
imap <C-b>         <Left>
imap <C-a>         <Home>
imap <A-x>         <ESC>:
nmap <A-x>         <ESC>:
imap <C-backspace> <C-w>
"You should notice we use 'b' for mark anchor
nmap     <C-l>         mbz.`b
inoremap <C-l>         <ESC>mbz.`ba
"tab switch shortcut
map <F8> :bnext<CR>
map <F7> :bprevious<CR>
"quick quit
nnoremap <silent><leader>z :wq<CR>
"quick copy/paste, often you should select before
map <leader>c "+y
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
nmap <C-P> :FZF<CR>

colorscheme dracula


" global rainbow mode
let g:rainbow_active = 1

" neovide
let g:neovide_cursor_animation_length=0.13
let g:neovide_cursor_trail_length=0.8
let g:neovide_cursor_antialiasing=v:true
let g:neovide_cursor_vfx_mode = "railgun"


" ag(silver_searcher) vim bundle
" bind a shorcut for using silver_search to search the word under cursor
nmap <C-k> :Rg "\b<cword>\b" <CR>
" wait for user input
nmap <leader>s  :Rg 

