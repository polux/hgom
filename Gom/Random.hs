------------------------------------------------------------------
-- |
-- Module      : Gom.Random
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Instances of 'Test.QuickCheck.Arbitrary' for 'Gom.Sig' and other
-- generators.
--------------------------------------------------------------------

module Gom.Random () where

import Gom.Sig
import Test.QuickCheck

genId :: Gen String
genId = listOf1 $ oneof [choose ('a','z'), choose ('A','Z')]

genUId :: Gen String
genUId = do c  <- choose ('A','Z') ; cs <- genId ; return $ c:cs

instance Arbitrary SortId  where arbitrary = makeSortId  `fmap` genUId
instance Arbitrary CtorId  where arbitrary = makeCtorId  `fmap` genId
instance Arbitrary FieldId where arbitrary = makeFieldId `fmap` genId

allDiff :: (Eq t) => [t] -> Bool
allDiff []     = True
allDiff (x:xs) = x `notElem` xs && allDiff xs

instance Arbitrary Module where
  arbitrary = do
    modul <- genId
    sorts <- arbitrary `suchThat` allDiff
    -- we need at least one constructor per sort
    cidss <- listOf (listOf1 arbitrary) `suchThat` (allDiff . concat)
    let mix = zip sorts cidss
    defs  <- mapM (genSortDef (map fst mix)) mix
    return $ Module modul [] defs
  shrink (Module m i d) = do
    d' <- shrink d
    return $ Module m i d'

genTypedFields :: (Arbitrary a, Eq a) => [a1] -> Gen [(a, a1)]
genTypedFields sorts = do
  flds <- listOf1 arbitrary `suchThat` allDiff
  doms <- listOf1 (elements sorts)
  return $ zip flds doms

instance Arbitrary SortDef where
  shrink (SortDef s c l) = do
    l' <- shrink l
    return $ SortDef s c l' 

genSortDef :: [SortId] -> (SortId, [CtorId]) -> Gen SortDef
genSortDef sorts (sid,cids) = do
  flds   <- genTypedFields sorts
  ctrs   <- mapM (genCtor sorts flds) cids
  return $ SortDef sid (Just $ makeClassId ("Object","")) ctrs

instance Arbitrary Ctor where
  shrink (Simple c l) = do l' <- shrink l ; return $ Simple c l' 
  shrink x            = return x 

genCtor :: [SortId] -> [(FieldId, SortId)] -> CtorId -> Gen Ctor
genCtor sorts flds cname =
  oneof [genSCtor flds cname, genVCtor sorts cname]

genSCtor :: [(FieldId, SortId)] -> CtorId -> Gen Ctor
genSCtor flds cname = do
  fis <- listOf (elements flds) `suchThat` allDiff
  return $ Simple cname fis

genVCtor :: [SortId] -> CtorId -> Gen Ctor
genVCtor sorts cname = do
  sort <- elements sorts
  return $ Variadic cname sort
