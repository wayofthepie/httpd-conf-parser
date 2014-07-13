module Main (main) where

import Data.Monoid (mempty)
import Test.Framework.Providers.QuickCheck2
import Test.Framework
import Test.Framework.Providers.API
import ParserQTest (parserTests)

emptyTestOpts = mempty :: TestOptions

testOpts = emptyTestOpts { 
        topt_maximum_generated_tests = Just 500000,
        {- 
            This must be higher than topt_maximum_generated_tests. This 
            prevents a bug where large values of topt_maximum_generated_tests
            will cause the test run to report a failure after 0 tests.
        -}
        topt_maximum_unsuitable_generated_tests = Just 20000000 
    }

emptyRunnerOpts = mempty :: RunnerOptions
runnerOpts = emptyRunnerOpts { ropt_test_options = Just testOpts }

main :: IO()
main = defaultMainWithOpts [
        testGroup "parserTests" parserTests
    ] runnerOpts

