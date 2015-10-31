-- |
--
-- Module      :  Cache
-- Description :  'Cache' typeclass definition.
-- License     :  MIT
--
-- 'Cache' typeclass definition.

module Cache ( Cache(..) ) where


import Data.Maybe (fromMaybe)


class (Show key, Monad m, Functor m) => Cache cache m key v where
    -- | Exists in cache?
    inCache                :: cache key v -> key -> m Bool

    -- | Search for a cached value by given key.
    lookupCache            :: cache key v      -> key -> m (Maybe v)
    -- | Search for a cached value by given key,
    --   returning the default value if none was found.
    findInCacheWithDefault :: cache key v -> v -> key -> m v

    filterCache            :: cache key v -> (key -> v -> Bool) -> m [(key, v)]

    -- | Update a cached value (found by key) using the provided function.
    updateCache            :: cache key v -> key -> (v -> v) -> m (cache key v)
    -- | Insert a key -> value entry in the cache.
    insertInCache          :: cache key v -> key -> v        -> m (cache key v)

    -- | Find a value by key or insert the default value otherwise.
    findOrElseInsert       :: cache key v -> v -> key -> m (v, cache key v)

    findInCacheWithDefault cache def = fmap (fromMaybe def) . lookupCache cache
    findOrElseInsert cache def k = do
            mb <- lookupCache cache k
            case mb of Just v -> return (v, cache)
                       _      -> do cache' <- insertInCache cache k def
                                    return (def, cache)

