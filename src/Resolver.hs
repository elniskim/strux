{-# LANGUAGE OverloadedStrings #-}
module Resolver where

import Control.Monad.State 
import qualified Data.Map as Map
import qualified Data.Text as T
import AST

data Resolver = Resolver {
    scopeStack :: [Scope],
    nextId :: SymbolId,
    errors :: [T.Text]
}

type ResolverState = State Resolver
type Scope = Map.Map T.Text ResolvedInfo

freshId :: ResolverState SymbolId
freshId = do
    s <- get
    let newId = nextId s 
    _ <- put s { nextId = newId + 1}
    return newId

enterScope :: ResolverState ()
enterScope = do modify pushScope
    where 
        pushScope :: Resolver -> Resolver
        pushScope old = old { scopeStack = Map.empty : scopeStack old }

exitScope :: ResolverState ()
exitScope = do modify popScope
    where 
        popScope :: Resolver -> Resolver
        popScope old = old { scopeStack = tail $ scopeStack old }

findInScopes :: T.Text -> ResolverState (Maybe SymbolId)
findInScopes name = do 
    s <- get
    let scopes = scopeStack s
    return $ searchScopes name scopes
        where
            searchScopes :: T.Text -> [Scope] -> Maybe SymbolId
            searchScopes _ [] = Nothing
            searchScopes symName (scope:rest) = case Map.lookup symName scope of
                Just info -> Just (symId info)
                Nothing -> searchScopes symName rest

insertInScope :: SymbolKind -> Type -> T.Text -> ResolverState (Either T.Text ResolvedInfo)
insertInScope kind t name = do
    s <- get
    let scope = head $ scopeStack s
    if Map.member name scope
        then return $ Left $ "Double declaration of " <> name <> " in dummy location."
        else do
            newid <- freshId
            let info = ResolvedInfo kind t newid 
            modify $ \currState -> currState { scopeStack = Map.insert name info scope: tail (scopeStack currState) }
            return $ Right info
