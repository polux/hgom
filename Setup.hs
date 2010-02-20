import Distribution.Simple
import Distribution.Simple.LocalBuildInfo(buildDir)
import System.Cmd(system)
import System.FilePath((</>))

main = defaultMainWithHooks hooks
  where hooks = simpleUserHooks { runTests = runTests' }

runTests' _ _ _ lbi = system testprog >> return ()
  where testprog = (buildDir lbi) </> "hgom" </> "hgom --test 100"
