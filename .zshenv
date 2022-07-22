export PATH="$PATH:/opt/homebrew/bin"
export PATH="$PATH:/usr/local/sbin"
export PATH="$PATH:/bin:/usr/sbin:/sbin:/usr/bin"
export PATH="$PATH:/Users/jslee/.local/bin"
export PATH="$PATH:/opt/homebrew/opt/python@3.9/libexec/bin"
export PATH="$PATH:/Users/jslee/Library/Python/3.8/bin"
export PATH="$PATH:/Users/jslee/.opam/ocaml-variants.4.12.0+options/bin"
export PATH="$PATH:/opt/homebrew/opt/gradle@6/bin"
export PATH="$PATH:/Users/jslee/.emacs.d.doom/bin"
export PATH="$PATH:/Users/jslee/.cargo/bin"
export PATH="$PATH:/Users/jslee/.ghcup/bin"
export PATH="$PATH:/opt/homebrew/opt/ruby/bin"
export PATH="$PATH:/opt/homebrew/opt/perl/bin"
export PATH="$PATH:/Library/TeX/texbin"
export PATH="$PATH:/Users/jslee/.roswell/bin"
export PATH="$PATH:/Users/jslee/go/bin"
export PATH="$PATH:/Users/jslee/Library/Python/3.9/bin"
export PATH="$PATH:/opt/homebrew/opt/llvm/bin"
export PATH="$PATH:/usr/local/share/dotnet/x64"
export PATH="$PATH:/Users/jslee/perl5/bin"
export PATH="$PATH:/usr/local/bin"
export NODE_PATH=/usr/local/lib/node_modules
export DOOMDIR=/Users/jslee/.doom.d
export BROWSER=w3m
. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
export DOTNET_ROOT="/usr/local/share/dotnet/x64"
export GOPATH="/Users/jslee/.go"

export PKG_CONFIG_PATH="${PKG_CONFIG_PATH}:$(brew --prefix libffi)/lib/pkgconfig/:/usr/local/Cellar/zlib/1.2.8/lib/pkgconfig:/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig"
export DB_USER=jslee
export DB_PASS=1234
export DB_HOST=localhost
export DB_NAME=umamusume

# PERL5LIB="/Users/jslee/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
# PERL_LOCAL_LIB_ROOT="/Users/jslee/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
# PERL_MB_OPT="--install_base \"/Users/jslee/perl5\""; export PERL_MB_OPT;
# PERL_MM_OPT="INSTALL_BASE=/Users/jslee/perl5"; export PERL_MM_OPT;

eval `/opt/homebrew/bin/opam config env`
. "$HOME/.cargo/env"
