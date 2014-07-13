-------------------------------------------------------------------------------
-- Unit Tests
-------------------------------------------------------------------------------


{-
testDirectiveArg = 
    TestCase (assertEqual' "directiveArg parser: positive" directiveArgp " test" "test")
    -}

commentpTests = test [ 
    "single comment single line" ~: 
        Just () ~=?  run commentp "#test",
            
    "multiple comments multiple lines" ~: 
        Just [(),()] ~=? run (many1 commentp) "#test\n#test\n" ] 


-- dArgAllowed tests
dArgAllowedTests = test [
    "all allowed symbols" ~: 
        Just "/~.-_,\"\\^" ~=? run (many dArgAllowed) "/~.-_,\"\\^" ]
 
 
