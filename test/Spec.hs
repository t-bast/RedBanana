import RedBanana.Bytecode
import RedBanana.Types
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "bindBlocks" $ do
        it "correctly splits blocks" $ do
            bindBlocks [PUSH1, PUSH1, PUSH1, JUMP, PUSH1, JUMPDEST, STOP, JUMP] 
                `shouldBe`
                 [Block 0 [PUSH1, PUSH1, PUSH1], Block 1 [PUSH1], Block 2 [STOP]]

        it "correctly removes empty blocks"$ do
            bindBlocks [PUSH12, JUMP, JUMPDEST, STOP, JUMP]
                `shouldBe`
                [Block 0 [PUSH12], Block 1 [STOP]]