# fcd - quickly navigate your filesystem

fcd is a small utility to bookmark directories, then quickly jump to them.
Selecting a bookmark is done by fuzzy matching.

## Demo

![](http://neki.github.io/fcd/fcd_demo.gif)

## Usage example

The full list of commands is available with `fcd help`.

```
# add the current directory to your bookmark list
fcd add

# add the provided directory to bookmarks
fcd add ~/mydirectory

# select a bookmark via fuzzy matching and jump to it
# (will display a prompt
# Use <C-h> and <C-l> to navigate the entries, or type something
# to refine the search
fcd

# select a bookmark via fuzzy matching and delete it
fcd delete

# additional arguments will be used to prepopulate the prompt
fcd select myproject

# list bookmarks
fcd list
```

All commands can be abbreviated to their first letter: `fcd a`, `fcd d`, and so on.

## Installation

To install fcd, you have to build the executable from source and add a small helper function to your `.bashrc` (adapt this if you are using another shell).
Here is a script to install the dependencies and build the project on Ubuntu. If you are using another system, there are more detailed instructions below.

### "Copy and paste" instructions for Ubuntu

Copy the following into your terminal (or to a script, then execute the script):

```bash
# Install the Haskell compiler (GHC), the Cabal build system, and other utilities
sudo apt-get install git build-essential haskell-platform
cabal install cabal-install

# Clone the project
git clone git@github.com:Neki/fcd.git ~/.fcd
cd ~/.fcd
cabal sandbox init
# Install the fcd dependencies
cabal install --only-dependencies
# Compile fcd
cabal build

# Add the fcd executable to your PATH
echo 'export PATH="$HOME/.fcd/dist/build/fcd:$PATH"' >> ~/.bashrc

# Add the fcd function to your shell
cat >> ~/.bashrc <<'EOF'
function fcd () {
  if [ $# -ne 0 ]; then
    command fcd $@
  else
    command fcd && cd "`cat "$HOME/.fcdresult"`" && rm "$HOME/.fcdresult";
  fi
}
EOF

# Reload your shell
$SHELL
```

### Detailed instructions

#### Build

fcd can be built like any other Haskell project using Cabal. For those not familiar with Cabal, don't worry, it should simple:

0. If not already done, install git (http://www.git-scm.com/downloads). You will also need a C compiler (as a library used by fcd includes some bits of C).
1. Install GHC, a Haskell compiler, and Cabal, a build system for Haskell. Installing the Haskell platform (http://www.haskell.org/platform/) will provide you with both.
2. Update Cabal to the latest version: `cabal install cabal-install` (as of this writing, the version shipped with the Haskell platform on most systems is old and does not allow you to create sandboxes. If you do not want to use sandboxes (see below), then this step is probably unecessary.)
3. Clone the project: `git clone git@github.com:Neki/fcd.git`
4. `cd` into the project (`cd fcd`) and create a Cabal sandbox: `cabal sandbox init`. While not strictly required, it may save you from an issue known as "Cabal hell" if you happen to build other projects using Haskell on the same system.
5. Install the dependencies of fcd: `cabal install --only-dependencies`.
6. Build the project: `cabal build`.
7. The compiled executable can be found in `fcd/dist/build/fcd/`. Add this to your PATH or copy the `fcd` executable to a directory in your PATH.

#### Add the fcd function to your shell

fcd is not a shell builtin and thus can not change the current working directory of your shell. Instead, it writes the selected bookmark to the file `~/.fcdresult`. You have to defined a shell function to read this file and `cd`.
To be able to use fcd as intended, add the following function to the file `~/.bashrc` (if you are using another shell, change as needed):

```bash
function fcd () {
  if [ $# -ne 0 ]; then
    command fcd $@
  else
    command fcd && cd "`cat "$HOME/.fcdresult"`" && rm "$HOME/.fcdresult";
  fi
}
```

When calling `fcd` in your shell, the above function is executed (instead of the actual fcd executable). If you provided arguments to your call, then this function will pass these arguments to the fcd executable and return. Otherwise, this function will call the fcd executable without arguments, then change your shell current working directory once the fcd executable has returned.

Do not forget to reload your shell.

## Issues, suggestions or questions?

Use Github issues.

## Motivation and inspiration

`cdargs` is a similar tool, but I do not really like its interface (granted, I have not taken the time to understand it :-) ).
I needed a small project (a couple of hours) to put into practice what I had learned of Haskell, so I built my own tool instead.
