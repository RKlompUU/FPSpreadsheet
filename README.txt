--------------------------------------------------------------------------------
-- Author information:

Name : Rick Klomp
Email: r.klomp@students.uu.nl

--------------------------------------------------------------------------------
-- Compilation

-- Run the following if using ghc >= 7.10, otherwise this step may be skipped
> cd to the root directory of this project
  cabal install ./uu-tc-2009.2.2

-- Run the following, regardless of the ghc version you're using
> cd to the root directory of this project
  cabal install


--------------------------------------------------------------------------------
-- Running
--
-- To test the lambda evaluation algorithm, send an expression through the stdin
-- to the executable:
> cabal run < in/let1.w
--
-- The executable starts the webserver using threepenny if the stdin is closed:
> cabal run
-- Once the webserver is running, simply go to the link that is printed out in
-- the console using a webbrowser.

--------------------------------------------------------------------------------
-- Creating the haddock documentation
> cd to the root directory of this project
  cabal haddock --executables

--------------------------------------------------------------------------------
-- Additional info

There are other means of compilation using the run.sh and make.sh scripts in the
root directory of this project. For the most part running 'cabal run' is better.
however, the run.sh script has its uses when debugging the code. It has 2 flags
that won't be passed through to the executable:
-m (run make.sh prior to running, if it fails then it asks if it should run the
    old binary)
-i (finds all .hs files in the source directory and asks which one of them
    should be opened in ghci)
