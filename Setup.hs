#!/usr/bin/env runhaskell

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Directory

import AutoAST

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { postConf = generate, postClean = clean }

generate :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
generate _ _ _ _ = do
  writeFile "Language/SMTLIB/Version12.hs" version12

clean :: Args -> CleanFlags -> PackageDescription -> () -> IO ()
clean _ _ _ () = do
  removeFile "Language/SMTLIB/Version12.hs"
