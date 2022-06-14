" sensible.vim - Defaults everyone can agree on
" Maintainer:   Tim Pope <http://tpo.pe/>
" Version:      1.2

call plug#begin('~/.vim/plugged')

Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'junegunn/fzf', { 'do': { -> fzf#install()  }  }

Plug 'junegunn/fzf.vim'

Plug 'tpope/vim-commentary'

Plug 'jpalardy/vim-slime'

Plug 'tpope/vim-surround'

Plug 'nanotech/jellybeans.vim'

Plug 'vim-airline/vim-airline'

Plug 'tpope/vim-fugitive'

Plug 'christoomey/vim-tmux-navigator'

Plug 'ryanoasis/vim-devicons'

Plug 'yangmillstheory/vim-snipe'

Plug 'voldikss/vim-floaterm'

Plug 'chrisbra/NrrwRgn'

Plug 'jiangmiao/auto-pairs'

Plug 'hylang/vim-hy'

Plug 'liuchengxu/vista.vim'

Plug 'vim-airline/vim-airline-themes'

Plug 'rescript-lang/vim-rescript'

Plug 'guns/vim-sexp'

Plug 'sbdchd/neoformat'

Plug 'farmergreg/vim-lastplace'

call plug#end()

" force encoding as UTF-8
scriptencoding utf-8
set encoding=utf-8
set fileencoding=utf-8
set nocompatible
set hidden
set clipboard^=unnamed,unnamedplus

" Visuals!
set t_Co=256
set background=dark
colorscheme jellybeans
set shm+=I  " disables startup message

let g:airline_theme='jellybeans'
set fillchars+=vert:\│

let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

" coc.nvim config
nnoremap <silent> K :call <SID>show_documentation()<CR>
nnoremap <silent> gd :call CocAction('jumpDefinition')<CR>

function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" map <Space> <Leader>
let mapleader = "\<Space>"
let maplocalleader = ","
map <leader>[ :tabprev<cr>
map <leader>] :tabnext<cr>
map <leader>nt :tabnew<Space>
map <leader>nn :next<cr>
map <leader>pp :prev<cr>
map <leader>fs :w<cr>
map <leader>ff :FZF<cr>
map <leader>fr :History<cr>
map <leader>ee :vert term<cr>
map <leader>ef :FloatermNew<cr>
map <leader>wv :vs<cr>
map <leader>ws :sp<cr>
map <leader>sc :noh<cr>
map <leader>st :CocOutline<cr>
map <leader>si :CocList outline<cr>
map <leader>fed :e ~/.vimrc<cr>
map <leader>feD :e ~/.gvimrc<cr>
map <leader>feR :so ~/.vimrc<cr>:PlugInstall<cr>
map <leader>gs :Git<cr>
map <leader>/ :call CocAction('diagnosticNext')<cr>
map <leader>\ :call CocAction('diagnosticPrevious')<cr>
map <leader>qq :qa!<cr>
map <leader>p/ :Rg
map <leader>; :vs<cr>
map <leader>' :sp<cr>
map <leader>wd :q<cr>
map <leader>wh <C-w>h
map <leader>wj <C-w>j
map <leader>wk <C-w>k
map <leader>wl <C-w>l
map <leader>wr <C-w>r
map <leader>bp :bp<cr>
map <leader>bn :bn<cr>
map <leader>bd :bw<cr>
map <leader>bb :buffers<cr>
map <leader>b= :Format<cr>
map <leader>cdc :lcd %:p:h<cr>
map <leader>cdt :lcd
map <leader>w= <C-w>=
map <leader>. :tabnew<cr>
map <leader>, :tabclose<cr>
map <leader>o :Files .<cr>
map <leader>cC :make<cr>
map <leader>ga :Git add .<cr>
map <leader>gc :Git commit<cr>
map <leader>gpu :Git push<cr>
map <localleader>sl :SlimeSendCurrentLine<cr>
map <localleader>se :SlimeSend<cr>
map <leader><leader> :
map <leader><TAB> <C-^>
map <leader><C-f> :Neoformat<cr>:w<cr>
map <leader><C-r> :e<cr>

nnoremap ZA :wqa!<cr>
nnoremap <C-x><C-c> :wqa!<cr>

inoremap <C-f> <right>
inoremap <C-b> <left>
inoremap <C-l> <esc>zza
nnoremap <C-l> zz

" FZF config
let g:fzf_history_dir = 1

" Neoformat config
let g:neoformat_enabled_python = ['black']

" Spacemacs style window switching
let i = 1
while i <= 9
    execute 'nnoremap <Leader>' . i . ' :' . i . 'wincmd w<CR>'
    let i = i + 1
endwhile

if exists('g:loaded_sensible') || &compatible
  finish
else
  let g:loaded_sensible = 'yes'
endif

if has('autocmd')
  filetype plugin indent on
endif
if has('syntax') && !exists('g:syntax_on')
  syntax enable
endif

" Use :help 'option' to see the documentation for the given option.

" vim-sexp config
let g:sexp_filetypes = "clojure,scheme,lisp,timl,hy"
map <leader>kw <Plug>(sexp_round_tail_wrap_element)
map <leader>kW <Plug>(sexp_round_tail_wrap_list)
map <leader>ks <Plug>(sexp_capture_next_element)
map <leader>kS <Plug>(sexp_capture_prev_element)
map <leader>kb <Plug>(sexp_capture_tail_element)
map <leader>kB <Plug>(sexp_capture_head_element)
map <leader>k[ <Plug>(sexp_square_tail_wrap_list)
map <leader>k{ <Plug>(sexp_curly_tail_wrap_list)

set autoindent
set backspace=indent,eol,start
set complete-=i
set smarttab
set noimd
set nospell
set ttimeout
set ttimeoutlen=100
set nu
set incsearch
set nonumber
set nrformats-=octal
set guioptions=
set splitright
set splitbelow

" No annoying sound on errors
set novisualbell
set t_vb=
set tm=500
set belloff=all

" vim-slime config
let g:slime_python_ipython = 1
if has("gui_running")
    let g:slime_target="vimterminal"
else
    let g:slime_target="tmux"
    let g:slime_default_config = {"socket_name": "default", "target_pane": "0"}
    let g:slime_dont_ask_default = 1
endif

if !has('nvim') && &ttimeoutlen == -1
  set ttimeout
  set ttimeoutlen=100
endif

set incsearch
set hlsearch
set laststatus=2
set ruler
set wildmenu
set cindent
set ignorecase
set smartcase

" <tab> inserts four <space>s
set smartindent
set tabstop=4
set expandtab
set shiftwidth=4

set rulerformat=%55(%{strftime('%a\ %b\ %e\ %I:%M\ %p')}\ %5l,%-6(%c%V%)\ %P%)

" "Aliases" for commonly used commands+lazy shift finger:
command! -bar -nargs=* -complete=file -range=% -bang W         <line1>,<line2>write<bang> <args>
command! -bar -nargs=* -complete=file -range=% -bang Write     <line1>,<line2>write<bang> <args>
command! -bar -nargs=* -complete=file -range=% -bang Wq        <line1>,<line2>wq<bang> <args>
command! -bar                                  -bang Wqall     wqa<bang>
command! -bar -nargs=* -complete=file -range=% -bang We        <line1>,<line2>w<bang> | e <args>
command! -bar -nargs=* -complete=file -count   -bang Wnext     <count>wnext<bang> <args>
command! -bar -nargs=* -complete=file -count   -bang Wprevious <count>wprevious<bang> <args>
command! -bar -nargs=* -complete=file          -bang E         edit<bang> <args>
command! -bar -nargs=* -complete=file          -bang Edit      edit<bang> <args>
command! -bar                                  -bang Q         quit<bang>
command! -bar                                  -bang Quit      quit<bang>
command! -bar                                  -bang Qall      qall<bang>
command! -bar -nargs=? -complete=option              Set       set <args>
command! -bar -nargs=? -complete=help                Help      help <args>
command! -bar -nargs=* -complete=file          -bang Make      make<bang> <args>
command! -bar -nargs=* -complete=buffer        -bang Bdel      bdel<bang> <args>
command! -bar -nargs=* -complete=buffer        -bang Bwipe     bwipe<bang> <args>
command! -bar -nargs=* -complete=file          -bang Mksession mksession<bang> <args>
command! -bar -nargs=* -complete=dir           -bang Cd        cd<bang> <args>
command! -bar                                        Messages  messages
command! -bar -nargs=+ -complete=file          -bang Source    source<bang> <args>

if !&scrolloff
  set scrolloff=1
endif
if !&sidescrolloff
  set sidescrolloff=5
endif
set display+=lastline

if &listchars ==# 'eol:$'
  set listchars=tab:>\ ,trail:-,extends:>,precedes:<,nbsp:+
endif

if v:version > 703 || v:version == 703 && has("patch541")
  set formatoptions+=j " Delete comment character when joining commented lines
endif

if has('path_extra')
  setglobal tags-=./tags tags-=./tags; tags^=./tags;
endif

set fileformats+=mac

set autoread

if &history < 1000
  set history=1000
endif
if &tabpagemax < 50
  set tabpagemax=50
endif

set sessionoptions-=options

" Allow color schemes to do bright colors without forcing bold.
if &t_Co == 8 && $TERM !~# '^linux\|^Eterm'
  set t_Co=16
endif

inoremap <C-U> <C-G>u<C-U>

set noswapfile
set nobackup
set nowritebackup
set noundofile

" Transparent!
hi NonText ctermbg=none
hi Normal guibg=NONE ctermbg=NONE
highlight SignColumn guibg=NONE

function SetupOCaml()
    setlocal shiftwidth=2 tabstop=2
    map <leader>cC :!dune build<cr>
endfunction

autocmd FileType ocaml call SetupOCaml()
autocmd FileType lisp setlocal shiftwidth=2 tabstop=2
autocmd FileType javascript setlocal shiftwidth=4 tabstop=4


