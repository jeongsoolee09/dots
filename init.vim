call plug#begin('~/.config/nvim/plugged')

Plug 'Olical/aniseed'

Plug 'Olical/conjure'

Plug 'Olical/fennel.vim'

Plug 'nvim-treesitter/nvim-treesitter'

Plug 'whatyouhide/gotham'

Plug 'jiangmiao/auto-pairs'

Plug 'nvim-telescope/telescope.nvim'

Plug 'nvim-telescope/telescope-file-browser.nvim'

Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'jpalardy/vim-slime'

Plug 'tpope/vim-commentary'

Plug 'tpope/vim-repeat'

Plug 'tpope/vim-vinegar'

Plug 'tpope/vim-surround'

Plug 'tpope/vim-sexp-mappings-for-regular-people'

Plug 'vim-airline/vim-airline'

Plug 'kyazdani42/nvim-web-devicons'

Plug 'kyazdani42/nvim-tree.lua'

Plug 'tpope/vim-fugitive'

Plug 'christoomey/vim-tmux-navigator'

Plug 'ryanoasis/vim-devicons'

Plug 'yangmillstheory/vim-snipe'

Plug 'nvim-lua/popup.nvim'

Plug 'nvim-lua/plenary.nvim'

Plug 'chrisbra/NrrwRgn'

Plug 'jiangmiao/auto-pairs'

Plug 'hylang/vim-hy'

Plug 'vim-airline/vim-airline-themes'

Plug 'sbdchd/neoformat'

Plug 'rescript-lang/vim-rescript'

Plug 'nvim-orgmode/orgmode'

Plug 'guns/vim-sexp'

Plug 'farmergreg/vim-lastplace'

Plug 'rafcamlet/nvim-luapad'

call plug#end()

lua << EOF
vim.opt.encoding = "utf-8"
vim.opt.fileencoding = "utf-8"
vim.opt.hidden = true
vim.opt.clipboard = unnamed
EOF

" Visuals!
lua << EOF
vim.opt.termguicolors = true
vim.opt.background = "dark"
vim.cmd([[colorscheme gotham]])
vim.g.airline_theme = "gotham"
vim.cmd([[set shm+=I]]) -- disables startup message
EOF

" coc.nvim config DEPRECATED
nnoremap <silent> K :call <SID>show_documentation()<CR>
nnoremap <silent> gd :call CocAction('jumpDefinition')<CR>
function! s:show_documentation()
  if (index(['vim','help'], &filetype) >= 0)
    execute 'h '.expand('<cword>')
  else
    call CocAction('doHover')
  endif
endfunction

" Keybindings FLEX
lua << EOF
local opts = { noremap = true }
vim.g.mapleader = " "
vim.g.maplocalleader = ","

vim.keymap.set("n", "<leader>!", ":!", opts)
vim.keymap.set("n", "<leader>[", ":tabprev<cr>", opts)
vim.keymap.set("n", "<leader>]", ":tabnext<cr>", opts)
vim.keymap.set("n", "<leader>nt", ":tabnew ", opts)
vim.keymap.set("n", "<leader>fn", ":next<cr>", opts)
vim.keymap.set("n", "<leader>fp", ":prev<cr>", opts)
vim.keymap.set("n", "<leader>fs", ":w<cr>", opts)
vim.keymap.set("n", "<leader>ff", ":Telescope fd<cr>", opts)
vim.keymap.set("n", "<leader>fr", ":Telescope oldfiles<cr>", opts)
vim.keymap.set("n", "<leader>wv", ":vs<cr>", opts)
vim.keymap.set("n", "<leader>ws", ":sp<cr>", opts)
vim.keymap.set("n", "<leader>sc", ":noh<cr>", opts)
vim.keymap.set("n", "<leader>saf", ":Telescope grep_string<cr>", opts)
vim.keymap.set("n", "<leader>st", ":CocOutline<cr>", opts)
vim.keymap.set("n", "<leader>si", ":CocList outline<cr>", opts)
vim.keymap.set("n", "<leader>fed", ":e ~/.config/nvim/init.vim<cr>", opts)
vim.keymap.set("n", "<leader>feD", ":e ~/.gvimrc<cr>", opts)
vim.keymap.set("n", "<leader>feR", ":so ~/.config/nvim/init.vim<cr>:PlugInstall<cr>", opts)
vim.keymap.set("n", "<leader>gs", ":Git<cr>", opts)
vim.keymap.set("n", "<leader>/", ":call CocAction('diagnosticNext')<cr>", opts)
vim.keymap.set("n", "<leader>\\", ":call CocAction('diagnosticPrevious')<cr>", opts)
vim.keymap.set("n", "<leader>qq", ":qa!<cr>", opts)
vim.keymap.set("n", "<leader>p/", ":Rg", opts)
vim.keymap.set("n", "<leader>;", ":vs<cr>", opts)
vim.keymap.set("n", "<leader>'", ":sp<cr>", opts)
vim.keymap.set("n", "<leader>wd", ":q<cr>", opts)
vim.keymap.set("n", "<leader>wh", "<C-w>h", opts)
vim.keymap.set("n", "<leader>wj", "<C-w>j", opts)
vim.keymap.set("n", "<leader>wk", "<C-w>k", opts)
vim.keymap.set("n", "<leader>wl", "<C-w>l", opts)
vim.keymap.set("n", "<leader>wr", "<C-w>r", opts)
vim.keymap.set("n", "<leader>bp", ":bp<cr>", opts)
vim.keymap.set("n", "<leader>bn", ":bn<cr>", opts)
vim.keymap.set("n", "<leader>bd", ":bw<cr>", opts)
vim.keymap.set("n", "<leader>bb", ":Telescope buffers<cr>", opts)
vim.keymap.set("n", "<leader>b=", ":Format<cr>", opts)
vim.keymap.set("n", "<leader>cdc", ":lcd %:p:h<cr>", opts)
vim.keymap.set("n", "<leader>cdt", ":lcd", opts)
vim.keymap.set("n", "<leader>w=", "<C-w>=", opts)
vim.keymap.set("n", "<leader>.", ":tabnew<cr>", opts)
vim.keymap.set("n", "<leader>,","  :tabclose<cr>", opts)
vim.keymap.set("n", "<leader>o", ":Telescope fd<cr>", opts)
vim.keymap.set("n", "<leader>cC", ":make<cr>", opts)
vim.keymap.set("n", "<leader>ga", ":Git add .<cr>", opts)
vim.keymap.set("n", "<leader>gc", ":Git commit<cr>", opts)
vim.keymap.set("n", "<leader>gpu", ":Git push<cr>", opts)
vim.keymap.set("n", "<localleader>sl", ":SlimeSendCurrentLine<cr>", opts)
vim.keymap.set("n", "<localleader>se", ":SlimeSend<cr>", opts)
vim.keymap.set("n", "<leader><leader>", ":", opts)
vim.keymap.set("n", "<leader><TAB>", "<C-^>", opts)
vim.keymap.set("n", "<leader><C-f>", ":Neoformat<cr>:w<cr>", opts)
vim.keymap.set("n", "<leader><C-r>", ":e<cr>", opts)
vim.keymap.set("n", "ZA", ":wqa!<cr>", opts)
vim.keymap.set("n", "<C-x><C-c>", ":wqa!<cr>", opts)
vim.keymap.set("i", "<C-f>", "<right>", opts)
vim.keymap.set("i", "<C-b>", "<left>", opts)
vim.keymap.set("i", "<C-l>", "<esc>zza", opts)
vim.keymap.set("n", "<C-l>", "zz", opts)
vim.keymap.set("n", "<C-g>", "<esc>", opts)
vim.keymap.set("i", "<C-g>", "<esc>", opts)
EOF

" Neoformat config
lua << EOF
vim.g.neoformat_enabled_python = {"black"}
EOF

" Spacemacs style window switching
let i = 1
while i <= 9
    execute 'nnoremap <Leader>' . i . ' :' . i . 'wincmd w<CR>'
    let i = i + 1
endwhile

let i = 1
while i <= 9
    execute 'nnoremap <D-' . i . '> :' . i . 'wincmd w<CR>'
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
set noswapfile

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

" "Aliases" for commonly used commands+lazy shift finger:
command! -bar -nargs=* -complete=file -range=% -bang W         <line1>,<line2>write<bang> <args>
command! -bar -nargs=* -complete=file -range=% -bang Write     <line1>,<line2>write<bang> <args>
command! -bar -nargs=* -complete=file -range=% -bang Wq        <line1>,<line2>wq<bang> <args>
command! -bar -nargs=* -complete=file -range=% -bang WQ        <line1>,<line2>wq<bang> <args>
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

set viminfo+=n~/.local/share/nvim/shada/main.shada

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

" let g:aniseed#env = v:true
