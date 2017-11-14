import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "bindBlocks" $ do
        it "splits blocks" $ do
            bindBlocks [PUSH1, PUSH1, PUSH1, JUMP, PUSH1, JUMPDEST, STOP, JUMP] 
                `shouldBe`
                 [Block 0 [PUSH1, PUSH1, PUSH1], Block 1 [PUSH1], BLOCK2 [STOP]]
