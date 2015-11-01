#!/bin/bash

# A script for automated download and build of 'NaiveBayes' project.
#
# Will create the following files (if not exist) in the directory, from which it was called:
#       * "WekaData" directory, containing the corresponding project.
#       * "NaiveBayes" directory, containing the corresponding project.
#       * "NaiveBayesWeka" executable symbolic link.
#       * "api.html" documentation symbolic link.
#
# Requires:
#       * git           https://git-scm.com/
#       * Haskell:      https://www.haskell.org/platform/
#               - GHC   https://www.haskell.org/ghc/
#               - cabal https://www.haskell.org/cabal/
#

GIT_Weka_Data="https://github.com/fehu/min-dat--weka-data.git"
GIT_Naive_Bayes="https://github.com/fehu/min-dat--naive-bayes.git"

ROOT_DIR=`pwd`


# Configure and build a "cabal" project.
build () {
  cabal configure
  cabal install --enable-tests --dependencies-only
  cabal build
  cabal haddock --hyperlink-source
  }

# Clone or pull a project with "git"; cd into the directory.
fetch() {
  DIR=$1
  GIT=$2
  
  if [ -d "$ROOT_DIR/$DIR" ];
  then
      cd "$ROOT_DIR/$DIR"
      git pull origin master
  else 
      git clone $GIT $DIR
      cd "$ROOT_DIR/$DIR"
  fi
  }
  
# Creates a symbolic link if it doesn't yet exist.
mklink() {
  if [ ! -f $2 ]
  then
      ln -s "$ROOT_DIR/$1" $2     
  fi
  }



  

# Build "WekaData" project.
fetch "WekaData" $GIT_Weka_Data
# build

cd $ROOT_DIR

# Build "NaiveBayes".
fetch "NaiveBayes" $GIT_Naive_Bayes
cabal sandbox init
cabal install "$ROOT_DIR/WekaData" --enable-documentation
build

# Link executable and documentation.
cd $ROOT_DIR
mklink "NaiveBayes/dist/build/NaiveBayesWeka/NaiveBayesWeka"    "NaiveBayesWeka"
mklink "/NaiveBayes/dist/doc/html/NaiveBayes/frames.html"       "NaiveBayesWeka-api.html"

