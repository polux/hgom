------------------------------------------------------------------
-- |
-- Module      : Common.Random
-- Copyright   : (c) Paul Brauner 2009
--               (c) Emilie Balland 2009
--               (c) INRIA 2009
-- Licence     : GPL (see COPYING)
--
-- Maintainer  : paul.brauner@inria.fr
-- Stability   : provisional
-- Portability : non-portable (requires generalized newtype deriving)
--
-- Instances of 'Test.QuickCheck.Arbitrary' for 'Gom.Common.Sig' and other
-- generators.
--------------------------------------------------------------------

module Common.Random () where

import Common.Sig
import Test.QuickCheck

genId :: Gen conf String
genId = resize 10 . listOf1 $ oneof [choose ('a','z'), choose ('A','Z')]

genUId :: Gen conf String
genUId = resize 10 $ do c  <- choose ('A','Z') ; cs <- genId ; return $ c:cs

builtins :: [SortId]
builtins = map makeSortId 
  ["boolean","int","char","double","float","long","String"]

instance Arbitrary SortId where arbitrary = makeSortId `fmap` genUId
instance Arbitrary CtorId where arbitrary = makeCtorId `fmap` genId
instance Arbitrary FieldId where arbitrary = makeFieldId `fmap` genId

allDiff :: (Eq t) => [t] -> Bool
allDiff []     = True
allDiff (x:xs) = x `notElem` xs && allDiff xs

instance Arbitrary Module where
  arbitrary = do
    modul <- genId
    sorts <- arbitrary `suchThat` allDiff
    -- we need at least one constructor per sort
    cidss <- resize 10 $ 
      listOf (listOf1 arbitrary) `suchThat` (allDiff . concat)
    let mix = zip sorts cidss
    defs  <- mapM (genSortDef (map fst mix)) mix
    return $ Module modul builtins defs
  shrink (Module m i d) = do
    d' <- shrink d
    return $ Module m i d'

genTypedFields :: [SortId] -> Gen conf [(FieldId,SortId)]
genTypedFields sorts = do
  flds <- listOf1 arbitrary `suchThat` allDiff
  doms <- listOf1 (elements $ sorts ++ builtins)
  return $ zip flds doms

instance Arbitrary SortDef where
  shrink (SortDef s c l) = do
    l' <- shrink l
    return $ SortDef s c l' 

genSortDef :: [SortId] -> (SortId, [CtorId]) -> Gen conf SortDef
genSortDef sorts (sid,cids) = do
  flds   <- genTypedFields sorts
  ctrs   <- mapM (genCtor sorts flds) cids
  return $ SortDef sid (Just $ makeClassId "Object" "") ctrs

instance Arbitrary Ctor where
  shrink (Simple c l) = do l' <- shrink l ; return $ Simple c l' 
  shrink x            = return x 

genCtor :: [SortId] -> [(FieldId, SortId)] -> CtorId -> Gen conf Ctor
genCtor sorts flds cname =
  oneof [genSCtor flds cname, genVCtor sorts cname]

genSCtor :: [(FieldId, SortId)] -> CtorId -> Gen conf Ctor
genSCtor flds cname = do
  fis <- listOf (elements flds) `suchThat` allDiff
  return $ Simple cname fis

genVCtor :: [SortId] -> CtorId -> Gen conf Ctor
genVCtor sorts cname = do
  sort <- elements sorts
  return $ Variadic cname sort
