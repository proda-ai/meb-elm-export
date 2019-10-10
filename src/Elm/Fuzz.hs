{-# LANGUAGE OverloadedStrings #-}

module Elm.Fuzz
  ( toElmFuzzRef
  , toElmFuzzRefWith
  , toElmFuzzSource
  , toElmFuzzSourceWith
  , renderFuzz
  ) where

import Control.Monad.RWS
import qualified Data.Text as T
import Elm.Common
import Elm.Type
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

class HasType a where
  render :: a -> RenderM Doc

class HasRecordType a where
  renderRecord :: a -> RenderM Doc

class HasTypeRef a where
  renderRef :: a -> RenderM Doc

instance HasType ElmDatatype where
  render d@(ElmDatatype typeName constructor@(RecordConstructor _ _)) = do
    name <- renderRef d
    ctor <- render constructor
    return $ name <+> ": Fuzzer" <+> stext typeName
             <$$> name <+> "=" <$$> indent 4 ctor
  render d@(ElmDatatype typeName constructor@(MultipleConstructors _)) = do
    name <- renderRef d
    ctor <- render constructor
    return $ name <+> ": Fuzzer" <+> stext typeName
             <$$> name <+> "=" <$$> (indent 4 $ "oneOf <|" <$$> (indent 4 (newlinebracks ctor)))
  render d@(ElmDatatype typeName constructor) = do
    name <- renderRef d
    ctor <- render constructor
    return $ name <+> ": Fuzzer" <+> stext typeName
             <$$>  name <+> "=" <$$> indent 4 ctor
  render (ElmPrimitive primitive) = renderRef primitive

instance HasTypeRef ElmDatatype where
  renderRef (ElmDatatype typeName _) = pure ("fuzz" <> stext typeName)
  renderRef (ElmPrimitive primitive) = renderRef primitive

instance HasType ElmConstructor where
  render (RecordConstructor name value) = do
    dv <- renderRecord value
    return $ "map" <+> stext name <+> dv
    -- return $ "constant <|" <$$> (indent 4 $ "{" <+> dv <$$> "}")
  render (NamedConstructor constructorName value) = do
    dv <- render value
    case value of
      ElmEmpty -> return $ "constant" <+> stext constructorName
      Values _ _ -> return $ "map" <+> stext constructorName <+> (nest 4 dv)
      _ -> return $ "map" <+> stext constructorName <+> dv
  render (MultipleConstructors constructors) = do
    mintercalate (line <> comma <> space) <$> sequence (render <$> constructors)
    -- constructList <- mintercalate (line <> ", " <> space) <$> sequence (render <$> constructors)
    -- constructorList  <- sequence (render <$> constructors)
    -- return $ (<+>) "oneOf <|" $ squarebracks $ cat constructorList

instance HasType ElmValue where
  render (ElmRef name) = pure ("fuzz" <> stext name)
  render (ElmPrimitiveRef primitive) = elmRefParens primitive <$> renderRef primitive
  render ElmEmpty = pure (text "")
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> "|> andMap" <+> dy
  render (ElmField _ value) = do
    -- fieldModifier <- asks fieldLabelModifier
    dv <- renderRecord value
    return $ parens $ dv

instance HasRecordType ElmValue where
  renderRecord (ElmPrimitiveRef primitive) = renderRef primitive
  renderRecord (Values x y) = do
    dx <- renderRecord x
    dy <- renderRecord y
    return $ dx <$$> "|> andMap" <+> dy
  renderRecord value = render value

instance HasTypeRef ElmPrimitive where
  renderRef (EList (ElmPrimitive EChar)) = renderRef EString
  renderRef (EList datatype) = do
    dt <- renderRef datatype
    return $ "list" <+> parens dt
  renderRef (ETuple2 x y) = do
    dx <- renderRef x
    dy <- renderRef y
    return $ "tuple" <+> (parens $ dx <> comma <+> dy)
  renderRef (EMaybe datatype) = do
    dt <- renderRef datatype
    return $ "maybe" <+> parens dt
  renderRef (EDict k v) = do
    require "Dict"
    dk <- renderRef k
    dv <- renderRef v
    return $ "dict" <+> parens dk <+> parens dv
  renderRef EInt = pure "int"
  renderRef EDate = do
    require "Date"
    pure "date"
  renderRef ETimePosix = do
    require "Iso8601"
    require "Time"
    pure "time"
  renderRef EBool = pure "bool"
  renderRef EChar = pure "char"
  renderRef EString = pure "string"
  renderRef EUnit = pure "unit" -- alias for ()
  renderRef EFloat = pure "float"
  renderRef EFile = pure "fileList"

-- | Puts parentheses around the doc of an elm ref if it contains spaces.
elmRefParens :: ElmPrimitive -> Doc -> Doc
elmRefParens (EList (ElmPrimitive EChar)) = id
elmRefParens (EList _) = parens
elmRefParens (EMaybe _) = parens
elmRefParens (EDict _ _) = parens
elmRefParens _ = id

toElmFuzzRefWith
  :: ElmType a
  => Options -> a -> T.Text
toElmFuzzRefWith options x =
  pprinter . fst $ evalRWS (renderRef (toElmType x)) options ()

toElmFuzzRef
  :: ElmType a
  => a -> T.Text
toElmFuzzRef = toElmFuzzRefWith defaultOptions

toElmFuzzSourceWith
  :: ElmType a
  => Options -> a -> T.Text
toElmFuzzSourceWith options x =
  pprinter . fst $ evalRWS (render (toElmType x)) options ()

toElmFuzzSource
  :: ElmType a
  => a -> T.Text
toElmFuzzSource = toElmFuzzSourceWith defaultOptions

renderFuzz
  :: ElmType a
  => a -> RenderM ()
renderFuzz = collectDeclaration . render . toElmType
