{-# LANGUAGE OverloadedStrings #-}
module Codegen where

import IRTypes
import Control.Monad.Writer
import Data.Text as T
import Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as TL
import qualified GHC.Show as T

type CodegenM = Writer B.Builder ()

outputStrux :: QBEIR -> T.Text
outputStrux qbeir = (TL.toStrict . toLazyText . execWriter) $ outputIR qbeir

outputIR :: QBEIR -> CodegenM
outputIR (QBEIR funcs structs decls)= do
    mapM_ outputStruct structs
    mapM_ outputDecl decls
    mapM_ outputFunc funcs

outputStruct :: QBEStruct -> CodegenM
outputStruct (QBEStruct name contents) = do
    let strContents = T.intercalate ", " $ fmap (\(qType, fieldCount) -> if fieldCount == 1 then typeText qType else typeText qType <> " " <> (T.pack . show) fieldCount) contents
    tell $ B.fromText $ "type :" <> name <> " = { " <> strContents <> " }\n"

outputDecl :: QBEDecl -> CodegenM
outputDecl (QBEStrDecl stringVal name) = do
    let strContents = T.pack $ Prelude.unwords (fmap (\c -> '\'' : c : ['\'']) (show stringVal))
    tell $ B.fromText $ "data $" <> name <> " = { " <> strContents <> " 0 }"
outputDecl (QBEGlobalDecl name declSize) = tell $ B.fromText $ "data $" <> name <> " = align 8 { z " <> (T.pack . show) declSize <> " }\n"

outputFunc :: QBEFunc -> CodegenM
outputFunc (QBEFunc funcName retType funcArgs fBody) = do 
    let funcSig = "function " <> maybe "" (\t -> " " <> typeText t) retType <> "\n"
    tell $ B.fromText funcSig

    tell $ B.fromText "}\n"

-- probably write some specialized tell actions, make your life easier so you dont forget newlines and converting to builders and whatnot




typeText :: QBEType -> T.Text
typeText Word = "w"
typeText Long = "l"
typeText Single = "s"
typeText Double = "d"
typeText (Aggregate name) = ":" <> name