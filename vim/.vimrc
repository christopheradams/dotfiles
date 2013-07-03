" Enable syntax highlighting
syntax enable

" Allow filetype plugins
filetype plugin on

" Use for dark terminals
"colorscheme wombat256

" Use for white terminals
"colorscheme github

" Use for solarized terminals
set background=dark
" set background=light
colorscheme solarized

" Enable line numbers
set number

" Enable ruler
set ruler

" Spacing
set autoindent
set shiftwidth=4
set tabstop=4

" Disable swap files
set noswapfile

" Incremental search
set incsearch
" Highlight search term
set hlsearch
set ruler
" Highlight current line
set cursorline

" Folding settings
set foldmethod=indent   "fold based on indent
set foldnestmax=10      "deepest fold is 10 levels
set nofoldenable        "dont fold by default
set foldlevel=1

" Handlebars
au BufRead,BufNewFile *.handlebars,*.hbs set ft=html syntax=handlebars
