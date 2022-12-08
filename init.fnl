(local packer (require "packer"))

(.startup packer 
          (fn []
            (use "wbthomason/packer.nvim")
            (use "Olical/aniseed")
            (use "Olical/conjure")
            (use "Plical/fennel.vim")
            (use "nvim-treesitter/nvim-treesitter")
            (use "nvim-telescope/telescope.nvim")
            (use "nvim-telescope/telescope-file-browser.nvim")
            (use "jpalardy/vim-slime")
            (use "tpope/vim-commentary")
            (use "tpope/vim-repeat")
            (use "tpope/vim-vinegar")
            (use "tpope/vim-surround")
            (use "tpope/vim-sexp-mappings-for-regular-people")
            (use "tpope/vim-fugitive")
            (use "kyazdani42/nvim-web-devicons")
            (use "kyazdani42/nvim-tree.lua")
            (use "christoomey/vim-tmux-navigator")
            (use "ryanoasis/vim-devicons")
            (use "yangmillstheory/vim-snipe")
            (use "nvim-lua/popup.nvim")
            (use "nvim-lua/plenary.nvim")
            (use "chrisbra/NrrwRgn")
            (use "hylang/vim-hy")
            (use "sbdchd/neoformat")
            (use "nvim-orgmode/orgmode")
            (use "guns/vim-sexp")
            (use "jiangmiao/auto-pairs")
            (use "farmergreg/vim-lastplace")
            (use "rafcamlet/nvim-luapad")
            (use "junegunn/seoul256.vim")
            (use "whatyouhide/gotham")
            (use "andymass/vim-matchup")
            (use {1 "imacco/markdown-preview.nvim"
                  :run "cd app && yarn install"
                  :cmd "MarkdownPreview"})
            (use "neovim/nvim-lspconfig")
            (use "hrsh7th/cmp-nvim-lsp")
            (use "hrsh7th/cmp-buffer")
            (use "hrsh7th/cmp-path")
            (use "hrsh7th/cmp-cmdline")
            (use "hrsh7th/cmp-nvim-lsp")
            (use "SirVer/ultisnips")
            (use "quangnguyen30192/cmp-nvim-ultisnips")
            (use {1 "catppuccin/nvim" :as "catppuccin"})))

;; nvim-cmp config ==================================
;; ==================================================

(local cmp (require "cmp"))

(.setup cmp {:snippet
             {:expand (fn []
                        ((. vim.fn :UltiSnips#Anon) args.body))}
             :window
             {:completion (cmp.config.window.bordered)
              :documentation (cmp.config.window.bordered)}
             :mapping
             (cmp.mapping.preset.insert
              {"<C-b>" (cmp.mapping.scroll_docs -4)}
              {"<C-f>" (cmp.mapping.scroll_docs 4)}
              {"<tab>" (cmp.mapping.complete)}
              {"<C-e>" (cmp.mapping.abort)}
              {"<CR>"  (cmp.mapping.confirm
                        {:select true})})
             :sources
             (cmp.config.sources [ {:name "ultisnips"}])})
