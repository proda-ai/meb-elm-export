{-# LANGUAGE OverloadedStrings #-}

module Elm.Dummy
  ( toElmDummyRef
  , toElmDummyRefWith
  , toElmDummySource
  , toElmDummySourceWith
  , renderDummy
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
    return $ name <+> ":" <+> stext typeName
             <$$> name <+> "=" <$$> indent 4 ctor
  render d@(ElmDatatype typeName constructor@(MultipleConstructors [])) = error "MutlipleConstructors must have constructor"
  render d@(ElmDatatype typeName constructor@(MultipleConstructors (x:_))) = do
    name <- renderRef d
    ctor <- render x
    return $ name <+> ": " <+> stext typeName
             <$$> name <+> "=" <+> ctor
  render d@(ElmDatatype typeName constructor) = do
    name <- renderRef d
    ctor <- render constructor
    return $ name <+> ": " <+> stext typeName
             <$$>  name <+> "=" <$$> indent 4 ctor
  render (ElmPrimitive primitive) = renderRef primitive

instance HasTypeRef ElmDatatype where
  renderRef (ElmDatatype typeName _) = pure ("dummy" <> stext typeName)
  renderRef (ElmPrimitive primitive) = renderRef primitive

instance HasType ElmConstructor where
  render (RecordConstructor name value) = do
    dv <- renderRecord value
    -- return $ "map" <+> stext name <+> dv
    -- return $ "constant <|" <$$> (indent 4 $ "{" <+> dv <$$> "}")
    return $ "{" <+> dv <$$> "}"
  render (NamedConstructor constructorName value) = do
    dv <- render value
    case value of
      ElmEmpty -> return $ stext constructorName
      Values _ _ -> return $ stext constructorName <+> (nest 4 dv)
      _ -> return $ stext constructorName <+> dv
  render (MultipleConstructors []) = error "MultipleConstructors must have a constructor"
  render (MultipleConstructors (x:_)) = do
    ctor <- render x
    return $ ctor
    -- mintercalate (line <> comma <> space) <$> sequence (render <$> constructors)
    -- -- constructList <- mintercalate (line <> ", " <> space) <$> sequence (render <$> constructors)
    -- -- constructorList  <- sequence (render <$> constructors)
    -- -- return $ (<+>) "oneOf <|" $ squarebracks $ cat constructorList

instance HasType ElmValue where
  render (ElmRef name) = pure ("dummy" <> stext name)
  render (ElmPrimitiveRef primitive) = elmRefParens primitive <$> renderRef primitive
  render ElmEmpty = pure (text "")
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> "<| " <+> dy
  render (ElmField name value) = do
    fieldModifier <- asks fieldLabelModifier
    dv <- renderRecord value
    return $ stext (fieldModifier name) <+> "=" <+> dv
    -- -- fieldModifier <- asks fieldLabelModifier
    -- dv <- renderRecord value
    -- return $ parens $ dv

instance HasRecordType ElmValue where
  renderRecord (ElmPrimitiveRef primitive) = renderRef primitive
  renderRecord (Values x y) = do
    dx <- renderRecord x
    dy <- renderRecord y
    -- return $ dx <$$> "|> andMap" <+> dy
    return $ dx <$$> comma <+> dy
  renderRecord value = render value

instance HasTypeRef ElmPrimitive where
  renderRef (EList (ElmPrimitive EChar)) = renderRef EString
  renderRef (EList datatype) = do
    dt <- renderRef datatype
    return $ brackets $ dt
  renderRef (ETuple2 x y) = do
    dx <- renderRef x
    dy <- renderRef y
    return $ parens $ dx <> comma <+> dy
  renderRef (EMaybe datatype) = do
    dt <- renderRef datatype
    return $ "Just" <+> parens dt
  renderRef (EDict k v) = do
    require "Dict"
    dk <- renderRef k
    dv <- renderRef v
    return $ "Dict.fromList" <+> (brackets $ parens $ dk <+> comma <+> dv)
  renderRef EInt = pure "1"
  renderRef EDate = do
    require "Date"
    pure "Date.fromCalendarDate 2020 Time.Jan 1"
  renderRef ETimePosix = do
    require "Iso8601"
    require "Time"
    pure  "Time.millisToPosix 1570695324000"
  renderRef EBool = pure "True"
  renderRef EChar = pure $ squotes $ stext "c"
  renderRef EString = pure $ dquotes $ stext "str"
  renderRef EUnit = pure "()" -- alias for ()
  renderRef EFloat = pure "1.0"
  renderRef EFile = pure "fileList"

-- | Puts parentheses around the doc of an elm ref if it contains spaces.
elmRefParens :: ElmPrimitive -> Doc -> Doc
elmRefParens (EList (ElmPrimitive EChar)) = id
elmRefParens (EList _) = parens
elmRefParens (EMaybe _) = parens
elmRefParens (EDict _ _) = parens
elmRefParens _ = id

toElmDummyRefWith
  :: ElmType a
  => Options -> a -> T.Text
toElmDummyRefWith options x =
  pprinter . fst $ evalRWS (renderRef (toElmType x)) options ()

toElmDummyRef
  :: ElmType a
  => a -> T.Text
toElmDummyRef = toElmDummyRefWith defaultOptions

toElmDummySourceWith
  :: ElmType a
  => Options -> a -> T.Text
toElmDummySourceWith options x =
  pprinter . fst $ evalRWS (render (toElmType x)) options ()

toElmDummySource
  :: ElmType a
  => a -> T.Text
toElmDummySource = toElmDummySourceWith defaultOptions

renderDummy
  :: ElmType a
  => a -> RenderM ()
renderDummy = collectDeclaration . render . toElmType
