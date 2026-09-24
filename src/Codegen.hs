{-# LANGUAGE OverloadedStrings #-}
module Codegen where 

import IRTypes
import Control.Monad.Writer
import Data.Text as T
import Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as TL

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
    tell $ B.fromText $ "type :" <> name <> " = { " <> strContents <> " }"

outputDecl :: QBEDecl -> CodegenM
outputDecl = error "unimplemented"

outputFunc :: QBEFunc -> CodegenM
outputFunc = error "unimplemented"



typeText :: QBEType -> T.Text
typeText Word = "w"
typeText Long = "l"
typeText Single = "s"
typeText Double = "d"
typeText (Aggregate name) = ":" <> name