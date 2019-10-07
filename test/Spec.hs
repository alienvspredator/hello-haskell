import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Control.Exception              ( evaluate )

import           BinTree
import           Nat

main :: IO ()
main = hspec $ do
    describe "BinTree" $ do
        describe "depth"
            $          it "can calculate depth of tree"
            $          depth
                           (BinTree
                               ( BinTree (BinTree (List 5, List 9), List 9)
                               , BinTree
                                   ( BinTree (List 1, List 9)
                                   , BinTree
                                       ( BinTree (List 9, List 3)
                                       , BinTree (BinTree (List 9, List 3), List 4)
                                       )
                                   )
                               )
                           )
            `shouldBe` (6 :: Nat)
        describe "leaves"
            $          it "returns list of elements in leaves"
            $          leaves
                           (BinTree
                               ( BinTree (BinTree (List 5, List 9), List 9)
                               , BinTree
                                   ( BinTree (List 1, List 9)
                                   , BinTree
                                       ( BinTree (List 9, List 3)
                                       , BinTree (BinTree (List 9, List 3), List 4)
                                       )
                                   )
                               )
                           )
            `shouldBe` [5, 9, 9, 1, 9, 9, 3, 9, 3, 4]
        describe "reverseBinTree"
            $          it "can reverces BinTree"
            $          reverseBinTree (BinTree (List 5, List 9))
            `shouldBe` BinTree (List 9, List 5)
    describe "Nat" $ do
        describe "natToInteger"
            $          it "can convert to integer"
            $          natToInteger (5 :: Nat)
            `shouldBe` 5
        describe "beside" $ do
            context "when first element is highter then second" $ do
                context "when elements are beside"
                    $          it "returns true if element are beside"
                    $          beside (6 :: Nat) (5 :: Nat)
                    `shouldBe` True
                context "when elements are not beside"
                    $          it "returns false if elements are not beside"
                    $          beside (7 :: Nat) (5 :: Nat)
                    `shouldBe` False
            context "wher firs element is lower then second" $ do
                context "when elements are beside"
                    $          it "returns true if elements are beside"
                    $          beside (5 :: Nat) (6 :: Nat)
                    `shouldBe` True
                context "when elements are not beside"
                    $          it "returns false if elements are not beside"
                    $          beside (5 :: Nat) (7 :: Nat)
                    `shouldBe` False
        describe "abs"
            $          it "returns the same value"
            $          abs (5 :: Nat)
            `shouldBe` (5 :: Nat)
        describe "(+)" $ do
            context "when adds Zero"
                $          it "adds up Nats"
                $          (5 :: Nat)
                +          (0 :: Nat)
                `shouldBe` (5 :: Nat)
            context "when adds non Zero"
                $          it "adds up Nats"
                $          (5 :: Nat)
                +          (1 :: Nat)
                `shouldBe` (6 :: Nat)
        describe "(*)" $ do
            context "when multiply Zero"
                $          it "Multiplies Nats"
                $          (0 :: Nat)
                *          (5 :: Nat)
                `shouldBe` (0 :: Nat)
            context "when multiply Succ (Zero)"
                $          it "Multiplies Nats"
                $          (1 :: Nat)
                *          (5 :: Nat)
                `shouldBe` (5 :: Nat)
            context "when multiply non Zero and non Succ (Zero)"
                $          it "Multiplies Nats"
                $          (5 :: Nat)
                *          (6 :: Nat)
                `shouldBe` (30 :: Nat)
        describe "fromInteger" $ do
            context "when 0"
                $          it "converts to Nat from Integer"
                $          (0 :: Nat)
                `shouldBe` Zero
            context "when not 0"
                $          it "converts to Nat from Integer"
                $          (3 :: Nat)
                `shouldBe` Succ (Succ (Succ Zero))
        describe "signum" $ do
            context "when Zero"
                $          it "returns Succ Zero for positive Nat"
                $          signum (Succ (Succ Zero))
                `shouldBe` Succ Zero
            context "when not Zero"
                $          it "returns Zero for Zero"
                $          signum Zero
                `shouldBe` Zero
        describe "natToInteger" $ do
            context "when Zero"
                $          it "converts Nat to integer"
                $          natToInteger Zero
                `shouldBe` 0
            context "when not Zero"
                $          it "converts Nat to integer"
                $          natToInteger (Succ (Succ Zero))
                `shouldBe` 2
        describe "pow" $ do
            context "when Zero"
                $          it "returns the base to the exponent power"
                $          pow (9 :: Nat) Zero
                `shouldBe` 1
            context "when not Zero"
                $          it "returns the base to the exponent power"
                $          pow (2 :: Nat) (7 :: Nat)
                `shouldBe` (128 :: Nat)
