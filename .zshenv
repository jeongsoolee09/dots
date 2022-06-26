export PATH="/usr/local/bin:$PATH"
export PATH="/usr/local/sbin:$PATH"
export PATH="~/bin:$PATH"
export PATH="/bin:/usr/sbin:/sbin:/usr/bin:$PATH"
export PATH="/Users/jslee/.local/bin:$PATH"
export PATH="/opt/homebrew/opt/python@3.9/libexec/bin:$PATH"
export PATH="/Users/jslee/Library/Python/3.8/bin:$PATH"
export PATH="/Users/jslee/.opam/ocaml-variants.4.12.0+options/bin:$PATH"
export PATH="/opt/homebrew/opt/gradle@6/bin:$PATH"
export PATH="/Users/jslee/.emacs.d.doom/bin:$PATH"
export PATH="/Users/jslee/.cargo/bin:$PATH"
export PATH="/Users/jslee/.ghcup/bin:$PATH"
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
export PATH="/opt/homebrew/opt/perl/bin:$PATH"
export PATH="/Library/TeX/texbin:$PATH"
export PATH="/Users/jslee/.roswell/bin:$PATH"
export PATH="/Users/jslee/go/bin:$PATH"
export PATH="/Users/jslee/Library/Python/3.9/bin:$PATH"
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export PATH="/usr/local/share/dotnet/x64:$PATH"
export PATH="/Users/jslee/perl5/bin:$PATH"
export NODE_PATH=/usr/local/lib/node_modules
export DOOMDIR=/Users/jslee/.doom.d
export BROWSER=w3m
. ~/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
export DOTNET_ROOT="/usr/local/share/dotnet/x64"
export GOPATH="/Users/jslee/.go"

export DB_USER=jslee
export DB_PASS=1234
export DB_HOST=localhost
export DB_NAME=umamusume

PERL5LIB="/Users/jslee/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/Users/jslee/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/Users/jslee/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/jslee/perl5"; export PERL_MM_OPT;

eval `/opt/homebrew/bin/opam config env`
. "$HOME/.cargo/env"
