# haskell-chat

Adults writing real-world Haskell.

this uses old version of haskell so this is how you build this:
```shell
# get 8 version of haskell. original 8.6.4 no longer available in ghcup
ghcup install stack
ghcup install 8.10.7 --set

# it will complain about getting llvm under version 14
brew install llvm@13
brew link --overwrite llvm@13

stack build
```
   hope this helps. did not run the chat yet.
