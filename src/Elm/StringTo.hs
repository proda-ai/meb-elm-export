{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Elm.StringTo
  ( toElmStringToRef
  , toElmStringToRefWith
  , toElmStringToSource
  , toElmStringToSourceWith
  , renderStringTo
  ) where

import Control.Monad.RWS
import qualified Data.Text as T
import Elm.Common
import Elm.Type
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

class HasStringTo a where
  render :: a -> RenderM Doc

class HasStringToRef a where
  renderRef :: a -> RenderM Doc

instance HasStringTo ElmDatatype where
  render d@(ElmDatatype name constructor) = do
    let typeName = stext name
        parserName = "parser" <> typeName
    fnName <- renderRef d
    ctor <- render constructor
    return $
      (fnName <+> ": String -> Maybe " <+> typeName) <$$>
      (fnName <+> " x = " <$$> indent 4 ctor)

  render m@(ElmPrimitive (EMaybe d@(ElmDatatype name constructor))) = do
    let fnName = "stringToMaybe" <> stext name
    dv <- renderRef m
    return $
      (fnName <+> ": String -> Maybe" <+> parens (stext name)) <$$>
      (fnName <+> "=" <+> dv)
  render (ElmPrimitive primitive) = renderRef primitive

instance HasStringToRef ElmDatatype where
  renderRef (ElmDatatype name _) = pure $ "stringTo" <> stext name
  renderRef (ElmPrimitive primitive) = renderRef primitive

instance HasStringTo ElmConstructor where
  render (NamedConstructor name ElmEmpty) =
    return $ stext name
  render (NamedConstructor name value) = do
    dv <- render value
    return $ dv <$$> indent 4 ("|> map" <+> stext name)
  render (RecordConstructor name value) = do
    dv <- render value
    return $ "succeed" <+> stext name <$$> indent 4 dv

  render mc@(MultipleConstructors constrs) = do
      let rndr = if isEnumeration mc
          then renderSum
          else error "can only stringTo<blah> enumeration constructors"
      cstrs <- mapM rndr constrs
      pure $ "case x of" <$$>
              (indent 4 $ foldl1 (<$+$>) cstrs <$+$>
               "_ ->" <$$> indent 4 "Nothing"
              )

-- | required "contents"
requiredContents :: Doc
requiredContents = "required" <+> dquotes "contents"

-- | "<name>" -> decode <name>
renderSumCondition :: T.Text -> Doc -> RenderM Doc
renderSumCondition name contents =
  pure $ dquotes (stext name) <+> "->" <$$>
    indent 4
      ("Just" <+> stext name <$$> indent 4 contents)

-- | Render a sum type constructor in context of a data type with multiple
-- constructors.
renderSum :: ElmConstructor -> RenderM Doc
renderSum (NamedConstructor name ElmEmpty) = renderSumCondition name mempty
renderSum (NamedConstructor name v@(Values _ _)) = do
  (_, val) <- renderConstructorArgs 0 v
  renderSumCondition name val
renderSum (NamedConstructor name value) = do
  val <- render value
  renderSumCondition name $ "|>" <+> requiredContents <+> val
renderSum (RecordConstructor name value) = do
  val <- render value
  renderSumCondition name val
renderSum (MultipleConstructors constrs) =
  foldl1 (<$+$>) <$> mapM renderSum constrs

-- | Render the decoding of a constructor's arguments. Note the constructor must
-- be from a data type with multiple constructors and that it has multiple
-- constructors itself.
renderConstructorArgs :: Int -> ElmValue -> RenderM (Int, Doc)
renderConstructorArgs i (Values l r) = do
  (iL, rndrL) <- renderConstructorArgs i l
  (iR, rndrR) <- renderConstructorArgs (iL + 1) r
  pure (iR, rndrL <$$> rndrR)
renderConstructorArgs i val = do
  rndrVal <- render val
  let index = parens $ "index" <+> int i <+> rndrVal
  pure (i, "|>" <+> requiredContents <+> index)

instance HasStringTo ElmValue where
  render (ElmRef name) = pure $ "decode" <> stext name
  render (ElmPrimitiveRef primitive) = renderRef primitive
  render (Values x y) = do
    dx <- render x
    dy <- render y
    return $ dx <$$> dy
  render (ElmField name value) = do
    fieldModifier <- asks fieldLabelModifier
    dv <- render value
    return $ "|> required" <+> dquotes (stext (fieldModifier name)) <+> dv
  render ElmEmpty = pure (stext "")

instance HasStringToRef ElmPrimitive where
  renderRef (EList (ElmPrimitive EChar)) = pure "string"
  renderRef (EList datatype) = do
    dt <- renderRef datatype
    return . parens $ "list" <+> dt
  renderRef (EDict EString value) = do
    require "Dict"
    d <- renderRef value
    return . parens $ "dict" <+> d
  renderRef (EDict key value) = do
    require "Dict"
    d <- renderRef (EList (ElmPrimitive (ETuple2 (ElmPrimitive key) value)))
    return . parens $ "map Dict.fromList" <+> d
  renderRef (EMaybe datatype) = do
    dt <- renderRef datatype
    return . parens $ "\\x -> if String.startsWith \"Just \" x then " <+> dt <+> " (String.dropLeft 5 x) else Nothing"
  renderRef (ETuple2 x y) = do
    dx <- renderRef x
    dy <- renderRef y
    return . parens $
      "map2 Tuple.pair" <+> parens ("index 0" <+> dx) <+> parens ("index 1" <+> dy)
  renderRef EUnit = pure $ parens "always ()"
  renderRef EInt = pure "int"
  renderRef EBool = pure "bool"
  renderRef EChar = pure "char"
  renderRef EFloat = pure "float"
  renderRef EString = pure "string"
  renderRef _ = error "Only support primitive or enumeration types for StringTo"

toElmStringToRefWith
  :: ElmType a
  => Options -> a -> T.Text
toElmStringToRefWith options x =
  pprinter . fst $ evalRWS (renderRef (toElmType x)) options ()

toElmStringToRef
  :: ElmType a
  => a -> T.Text
toElmStringToRef = toElmStringToRefWith defaultOptions

toElmStringToSourceWith
  :: ElmType a
  => Options -> a -> T.Text
toElmStringToSourceWith options x =
  pprinter . fst $ evalRWS (render (toElmType x)) options ()

toElmStringToSource
  :: ElmType a
  => a -> T.Text
toElmStringToSource = toElmStringToSourceWith defaultOptions

renderStringTo
  :: ElmType a
  => a -> RenderM ()
renderStringTo x = do
  require "Json.Decode exposing (..)"
  require "Json.Decode.Pipeline exposing (..)"
  collectDeclaration . render . toElmType $ x
