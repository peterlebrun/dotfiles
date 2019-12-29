" Information about sessions
"source ~/current_session.vim
"set ssop-=options " do not store global and local values in a session
"set ssop-=folds   " do not store folds

set nocompatible
filetype off
set backspace=indent,eol,start
set relativenumber
set number

let vundle_autoinstall = 0
let vundle_readme = expand('~/.vim/bundle/vundle/README.md')
if !filereadable(vundle_readme)
  echo "Installing Vundle..."
  echo ""
  silent !mkdir -p ~/.vim/bundle
  silent !git clone https://github.com/gmarik/vundle ~/.vim/bundle/vundle
  let vundle_autoinstall = 1
endif

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'

Bundle 'kien/ctrlp.vim'
"Bundle 'ervandew/supertab'
Bundle 'majutsushi/tagbar'
"Bundle 'Raimondi/delimitMate'
Bundle 'sheerun/vim-polyglot'
"Bundle 'scrooloose/syntastic'
Bundle 'tpope/vim-unimpaired'
Bundle 'tpope/vim-surround'
Bundle 'xolox/vim-misc'
Bundle 'xolox/vim-session'

if vundle_autoinstall
  echo "Installing bundles..."
  echo ""
  :BundleInstall
endif

let zenburn_vim = expand('~/.vim/colors/zenburn.vim')
if !filereadable(zenburn_vim)
  echo "Installing zenburn..."
  echo ""
  silent !mkdir -p ~/.vim/colors
  silent !wget https://raw.githubusercontent.com/jnurmine/Zenburn/master/colors/zenburn.vim
  silent !mv zenburn.vim ~/.vim/colors/zenburn.vim
endif
" don't forget to change tmux's settings to 256 colors


" Colors
set term=screen-256color
set t_Co=256
colorscheme zenburn

" Indenting
set smartindent
set expandtab " this turns a tab into spaces (use Ctrl+V for actual tab character
set tabstop=2 " two spaces per tab
set shiftwidth=2 " # spaces uses for indentation (i.e. > or <)
set softtabstop=2

" Show whitespaces
set list listchars=tab:»·,trail:·,eol:¬

" Search
set ignorecase smartcase
set incsearch

" file formats etc
set fileencoding=utf-8
set fileformat=unix
set nobomb " no byte-order marks

syntax on
filetype plugin indent on

" CtrlP Settings
let g:ctrlp_map = '<Leader>f'

let g:ctrlp_user_command = "find %s -type f " .
  \ "-not -iwholename '*/.git*' " .
  \ "-not -iname '*.jpg' " .
  \ "-not -iname '*.gif' " .
  \ "-not -iname '*.pdf' " .
  \ "-not -iname '*.png' " .
  \ "-not -iname '*.pyc' " .
  \ "-not -iname '*.swn' " .
  \ "-not -iname '*.so' " .
  \ "| while read filename; do " .
  \ "echo $#filename $filename; ".
  \ "done | sort -n | awk '{print $2}'"
  "-not -iwholename '*.DS_Store' " .
  "-not -iname '*.swp' " .
  "| sort -d | awk '{print length, $0 }' | sort -nr | awk '{print $2}'"

let g:ctrlp_cache_dir = $HOME.'/eng/tmp/cache/ctrlp'
let g:ctrlp_max_files=0
let g:ctrlp_max_depth=40
let g:ctrlp_clear_cache_on_exit=0

"autocmd BufWritePre * :%s/[\r \t]\+$//e "Automatically remove trailing whitespace when saving
autocmd BufWritePost .vimrc :so ~/.vimrc "Automatically source vimrc (avoid restarting vim)

"easily switch between tabs
let mapleader = ","
map <Leader>n :tabprevious<CR>
map <Leader>m :tabnext<CR>
map <Leader>t :tabedit<CR><Leader>f
map <Leader>v :tabedit ~/.vimrc<CR>
map <Leader>. :CtrlPTag<CR>
map <Leader>b :TagbarToggle<CR>

nnoremap <leader>so :OpenSession
nnoremap <leader>ss :SaveSession
nnoremap <leader>sd :DeleteSession<CR>
nnoremap <leader>sc :CloseSession<CR>

:let g:session_autosave = 'no'
