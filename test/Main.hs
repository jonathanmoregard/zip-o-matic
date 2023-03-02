{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.Zipper (ListZipper, (|:))
import Data.List.Zipper qualified as ListZipper
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.Hspec.Runner (hspec)
import Test.QuickCheck (Arbitrary (arbitrary), Gen)

main :: IO ()
main =
  hspec $
    describe "ListZipperTests" $ do
      prop "toList, |: invariant" $
        \(as :: [Int]) (a :: Int) (as2 :: [Int]) -> ListZipper.toList (as |: a :| as2) `shouldBe` as <> (a : as2)
      prop "toList . singleton = pure" $
        \(a :: Int) -> ListZipper.toList (ListZipper.singleton a) `shouldBe` pure a
      prop "toList . ix = zip [0..] . toList" $
        \(z :: ListZipper Int) -> ListZipper.toList (ListZipper.ix z) `shouldBe` zip [0 ..] (ListZipper.toList z)
      prop "tug Just = id" $ \(i :: Int) -> ListZipper.tug Just i `shouldBe` i

instance Arbitrary a => Arbitrary (ListZipper a) where
  arbitrary :: Arbitrary a => Gen (ListZipper a)
  arbitrary = do
    neck <- arbitrary
    tail <- arbitrary
    focus <- arbitrary
    return $ neck |: focus :| tail
