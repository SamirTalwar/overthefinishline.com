module OverTheFinishLine.Dashboard.Lists where

import Control.Monad
import Data.Functor.Identity
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as Map

groupQueryBy :: (Eq k, Hashable k) => (a -> k) -> (a -> v) -> [a] -> [(k, [v])]
groupQueryBy keyFunction valueFunction =
  runIdentity . liftedGroupQueryBy (,) (Identity . keyFunction) (Identity . valueFunction)

liftedGroupQueryBy :: (Monad m, Eq k, Hashable k) => (k -> [v] -> r) -> (a -> m k) -> (a -> m v) -> [a] -> m [r]
liftedGroupQueryBy constructor keyFunction valueFunction list = do
  grouped <- foldM addToGroup Map.empty list
  return . map (\(k, v) -> constructor k (reverse v)) $ Map.toList grouped
  where
    addToGroup grouped x = do
      k <- keyFunction x
      v <- valueFunction x
      return $ Map.alter (add v) k grouped

    add value Nothing = Just [value]
    add value (Just []) = Just [value]
    add value (Just (v : vs)) = Just (value : v : vs)
