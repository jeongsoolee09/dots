lua << EOF
    return require('packer').startup(function()
        use 'wbthomason/packer.nvim'

        use 'Olical/aniseed'

        use 'Olical/conjure'

        use 'Olical/fennel.vim'

        use 'nvim-treesitter/nvim-treesitter'

        use 'whatyouhide/gotham'

        use 'nvim-telescope/telescope.nvim'

        use 'nvim-telescope/telescope-file-browser.nvim'

        use 'jpalardy/vim-slime'

        use 'tpope/vim-commentary'

        use 'tpope/vim-repeat'

        use 'tpope/vim-vinegar'

        use 'tpope/vim-surround'

        use 'tpope/vim-sexp-mappings-for-regular-people'

        use 'feline-nvim/feline.nvim'

        use 'kyazdani42/nvim-web-devicons'

        use 'kyazdani42/nvim-tree.lua'

        use 'tpope/vim-fugitive'

        use 'christoomey/vim-tmux-navigator'

        use 'ryanoasis/vim-devicons'

        use 'yangmillstheory/vim-snipe'

        use 'nvim-lua/popup.nvim'

        use 'nvim-lua/plenary.nvim'

        use 'chrisbra/NrrwRgn'

        use 'hylang/vim-hy'

        use 'sbdchd/neoformat'

        use 'rescript-lang/vim-rescript'

        use 'nvim-orgmode/orgmode'

        use 'guns/vim-sexp'

        use 'farmergreg/vim-lastplace'

        use 'rafcamlet/nvim-luapad'

        use 'junegunn/seoul256.vim'

        use 'rstacruz/vim-closer'

        use 'andymass/vim-matchup'

        use {'iamcco/markdown-preview.nvim',
              run = 'cd app && yarn install', cmd = 'MarkdownPreview'}

        use 'neovim/nvim-lspconfig'

        use 'hrsh7th/cmp-nvim-lsp'

        use 'hrsh7th/cmp-buffer'

        use 'hrsh7th/cmp-path'

        use 'hrsh7th/cmp-cmdline'

        use 'hrsh7th/nvim-cmp'

        use 'SirVer/ultisnips'

        use 'quangnguyen30192/cmp-nvim-ultisnips'

        use { "catppuccin/nvim", as = "catppuccin" }
    end)
EOF

lua << EOF
  vim.cmd([[let g:iced_enable_default_key_mappings = v:true]])

  -- Setup nvim-cmp.
  local cmp = require 'cmp'

  cmp.setup({
    snippet = {
      -- REQUIRED - you must specify a snippet engine
      expand = function(args)
        vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
      end,
    },
    window = {
      completion = cmp.config.window.bordered(),
      documentation = cmp.config.window.bordered(),
    },
    mapping = cmp.mapping.preset.insert({
      ['<C-b>'] = cmp.mapping.scroll_docs(-4),
      ['<C-f>'] = cmp.mapping.scroll_docs(4),
      ['<tab>'] = cmp.mapping.complete(),
      ['<C-e>'] = cmp.mapping.abort(),
      ['<CR>'] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
    }),
    sources = cmp.config.sources({
      -- { name = 'nvim_lsp' },
      -- { name = 'vsnip' }, -- For vsnip users.
      -- { name = 'luasnip' }, -- For luasnip users.
      { name = 'ultisnips' }, -- For ultisnips users.
      -- { name = 'snippy' }, -- For snippy users.
    }, {
      { name = 'buffer' },
    })
  })

  -- Set configuration for specific filetype.
  cmp.setup.filetype('gitcommit', {
    sources = cmp.config.sources({
      { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
    }, {
      { name = 'buffer' },
    })
  })

  -- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline('/', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = {
      { name = 'buffer' }
    }
  })

  -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline(':', {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = 'path' }
    }, {
      { name = 'cmdline' }
    })
  })

EOF


lua << EOF
    vim.opt.encoding = "utf-8"
    vim.opt.fileencoding = "utf-8"
    vim.opt.hidden = true
    vim.opt.clipboard = unnamed
EOF

" catppuccin.nvim config
lua << EOF
require("catppuccin").setup({
	dim_inactive = {
		enabled = false,
		shade = "dark",
		percentage = 0.15,
	},
	transparent_background = false,
	term_colors = false,
	compile = {
		enabled = false,
		path = vim.fn.stdpath "cache" .. "/catppuccin",
	},
	styles = {
		comments = { "italic" },
		conditionals = { "italic" },
		loops = {},
		functions = {},
		keywords = {},
		strings = {},
		variables = {},
		numbers = {},
		booleans = {},
		properties = {},
		types = {},
		operators = {},
	},
	integrations = {
		treesitter = true,
		native_lsp = {
			enabled = true,
			virtual_text = {
				errors = { "italic" },
				hints = { "italic" },
				warnings = { "italic" },
				information = { "italic" },
			},
			underlines = {
				errors = { "underline" },
				hints = { "underline" },
				warnings = { "underline" },
				information = { "underline" },
			},
		},
		coc_nvim = false,
		lsp_trouble = false,
		cmp = true,
		lsp_saga = false,
		gitgutter = false,
		gitsigns = true,
		leap = false,
		telescope = true,
		nvimtree = {
			enabled = true,
			show_root = true,
			transparent_panel = false,
		},
		neotree = {
			enabled = false,
			show_root = true,
			transparent_panel = false,
		},
		dap = {
			enabled = false,
			enable_ui = false,
		},
		which_key = false,
		indent_blankline = {
			enabled = true,
			colored_indent_levels = false,
		},
		dashboard = true,
		neogit = false,
		vim_sneak = false,
		fern = false,
		barbar = false,
		bufferline = true,
		markdown = true,
		lightspeed = false,
		ts_rainbow = false,
		hop = false,
		notify = true,
		telekasten = true,
		symbols_outline = true,
		mini = false,
		aerial = false,
		vimwiki = true,
		beacon = true,
	},
	color_overrides = {},
	highlight_overrides = {},
})

    vim.cmd [[colorscheme catppuccin]]
    vim.g.catppuccin_flavour = "mocha" -- latte, frappe, macchiato, mocha

local ctp_feline = require('catppuccin.groups.integrations.feline')

require("feline").setup({
	components = ctp_feline.get(),
})
EOF


lua << EOF
    vim.opt.termguicolors = false
    vim.opt.background = "dark"
    vim.cmd([[set shm+=I]]) -- disables startup message
EOF

lua << EOF
    local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

    -- Mappings.
    -- See `:help vim.diagnostic.*` for documentation on any of the below functions
    local opts = { noremap=true, silent=true }
    vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, opts)
    vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, opts)
    vim.keymap.set('n', ']d', vim.diagnostic.goto_next, opts)
    vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, opts)

    -- Use an on_attach function to only map the following keys
    -- after the language server attaches to the current buffer
    local on_attach = function(client, bufnr)
      -- Enable completion triggered by <c-x><c-o>
      vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

      -- Mappings.
      -- See `:help vim.lsp.*` for documentation on any of the below functions
      local bufopts = { noremap=true, silent=true, buffer=bufnr }
      vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
      vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
      vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
      vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
      vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
      vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
      vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
      vim.keymap.set('n', '<space>wl', function()
        print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
      end, bufopts)
      vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
      vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
      vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
      vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
      vim.keymap.set('n', '<space>f', vim.lsp.buf.formatting, bufopts)
    end

    local lsp_flags = {
      debounce_text_changes = 150,
    }
    require('lspconfig')['pyright'].setup{
        on_attach = on_attach,
        flags = lsp_flags,
    }
    require('lspconfig')['clojure_lsp'].setup{
        on_attach = on_attach,
        flags = lsp_flags,
    }
    require('lspconfig')['tsserver'].setup{
        on_attach = on_attach,
        flags = lsp_flags,
    }
    require('lspconfig')['rust_analyzer'].setup{
        on_attach = on_attach,
        flags = lsp_flags,
        -- Server-specific settings...
        settings = {
          ["rust-analyzer"] = {}
        }
    }
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
vim.keymap.set("n", "<leader>feR", ":so ~/.config/nvim/init.vim<cr>:PackerInstall<cr>", opts)
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
vim.keymap.set("n", "<leader>Tp", ":colorscheme seoul256<cr>", opts)
vim.keymap.set("n", "<leader>Tn", ":colorscheme seoul256-light<cr>", opts)
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

function SetupOCaml()
    setlocal shiftwidth=2 tabstop=2
    map <leader>cC :!dune build<cr>
endfunction

autocmd FileType ocaml call SetupOCaml()
autocmd FileType lisp setlocal shiftwidth=2 tabstop=2
autocmd FileType javascript setlocal shiftwidth=4 tabstop=4

" let g:aniseed#env = v:true
