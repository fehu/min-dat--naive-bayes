--{-# LANGUAGE FlexibleInstances #-}

-- |
--
-- Module      :  Cache
-- Description :  'Cache' typeclass definition.
-- License     :  MIT
--
-- 'Cache' typeclass definition.

module Cache ( Cache(..), IOCache, newIOCache ) where

import qualified Data.Map.Strict as Map

import Data.Maybe (fromMaybe)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Traversable (sequenceA)
import Control.Monad (liftM)
import Control.Applicative ( (<$>) )

-----------------------------------------------------------------------------


class (Show key, Monad m, Functor m) => Cache cache m key v where
    -- | Exists in cache?
    inCache                :: cache key v -> key -> m Bool
    -- | Lists all cache entries.
    listCache              :: cache key v -> m [(key, v)]

    -- | Search for a cached value by given key.
    lookupCache            :: cache key v -> key -> m (Maybe v)
    -- | Search for a cached value by given key,
    --   returning the default value if none was found.
    findInCacheWithDefault :: cache key v -> v -> key -> m v

    filterCacheByKey       :: cache key v -> (key -> Bool) -> m [(key, v)]

    -- | Update a cached value (found by key) using the provided function.
    updateCache            :: cache key v -> key -> (v -> v) -> m (cache key v)
    -- | Insert a key -> value entry in the cache.
    insertInCache          :: cache key v -> key -> v        -> m (cache key v)

    -- | Find a value by key or insert the default value otherwise.
    findOrElseInsert       :: cache key v -> v -> key -> m (v, cache key v)

    -- | Find a value by key or insert the default value otherwise.
    findOrElseInsertM      :: cache key v -> m v -> key -> m (v, cache key v)

    updateOrInsert         :: cache key v -> (Maybe v -> v)   -> key -> m (cache key v)
    updateOrInsertM        :: cache key v -> (Maybe v -> m v) -> key -> m (cache key v)

    findInCacheWithDefault cache def = fmap (fromMaybe def) . lookupCache cache
    findOrElseInsert cache def = findOrElseInsertM cache (return def)
    findOrElseInsertM cache mdef k = do
            mb <- lookupCache cache k
            case mb of Just v -> return (v, cache)
                       _      -> do def <- mdef
                                    cache' <- insertInCache cache k def
                                    return (def, cache)
    updateOrInsert cache f = updateOrInsertM cache (return . f)


-----------------------------------------------------------------------------

newIOCache :: IO (IOCache key val)
newIOCache = IOCache <$> newIORef Map.empty


newtype IOCache key val = IOCache (IORef (Map key (IORef val)))

instance (Show key, Ord key) =>

    Cache IOCache IO key val where

        inCache     cache k = pureFCache (k `Map.member`) cache

        listCache           = ioFCache (mapM liftKVIO . Map.assocs)

        lookupCache cache k = ioFCache f cache
                        where f = sequenceA . fmap readIORef . Map.lookup k

        filterCacheByKey cache f = ioFCache g cache
                        where g c = sequence . fmap liftKVIO . Map.assocs
                                  $ Map.filterWithKey (\k _ -> f k) c

        updateCache cache k f = ioFCache (g . Map.lookup k) cache
                        where g (Just ref) = do modifyIORef ref f
                                                return cache
                              g _          = return cache

        insertInCache cache@(IOCache cref) k v = do
            vref <- newIORef v
            modifyIORef cref $ Map.insert k vref
            return cache

        updateOrInsertM cache f k = ioFCache (g . Map.lookup k) cache
                        where g (Just ref) = do v  <- readIORef ref
                                                v' <- f (Just v)
                                                writeIORef ref v'
                                                return cache
                              g _          = do v' <- f Nothing
                                                insertInCache cache k v'

liftKVIO (k,vref) = liftM ((,) k) (readIORef vref)

ioFCache   f (IOCache cref) = do cache <- readIORef cref
                                 f cache
pureFCache f (IOCache cref) = do cache <- readIORef cref
                                 return $ f cache
