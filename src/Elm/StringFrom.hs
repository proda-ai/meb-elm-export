{-# LANGUAGE OverloadedStrings #-}

module Elm.StringFrom
  ( toElmStringFromRef
  , toElmStringFromRefWith
  , toElmStringFromSource
  , toElmStringFromSourceWith
  , renderStringFrom
  ) where

import Control.Monad.RWS
import qualified Data.Text as T
import Elm.Common
import Elm.Type
import Text.PrettyPrint.Leijen.Text hiding ((<$>), (<>))

class HasStringFrom a where
  render :: a -> RenderM Doc

class HasStringFromRef a where
  renderRef :: Int -> a -> RenderM Doc

instance HasStringFrom ElmDatatype where
  render d@(ElmDatatype name constructor) = do
    fnName <- renderRef 0 d
    ctor <- render constructor
    return $
      (fnName <+> ":" <+> stext name <+> "->" <+> "String") <$$>
      (fnName <+> "x =" <$$> indent 4 ctor)

  render m@(ElmPrimitive (EMaybe d@(ElmDatatype name constructor))) =  do
    let
        fnName = "stringFromMaybe" <> stext name
    dv <- renderRef 0 m
    return $
      (fnName <+> ": Maybe" <+> parens (stext name) <+> "->" <+> "String") <$$>
      (fnName <+> "=" <+> dv)

  render (ElmPrimitive primitive) = renderRef 0 primitive


instance HasStringFromRef ElmDatatype where
  renderRef _ (ElmDatatype name _) = pure $ "stringFrom" <> stext name
  renderRef level (ElmPrimitive primitive) = renderRef level primitive

-- only allow enumerations or just pure primitives
instance HasStringFrom ElmConstructor where
  -- Single constructor, no values: empty array
  render (NamedConstructor _name ElmEmpty) =
    return $ dquotes $ stext _name

  -- Single constructor, one value: skip constructor and r just the value
  render (NamedConstructor name value) = do
    dv <- render value

    let cs = stext name <+> "y0 ->"
    return . nest 4 $ "case x of" <$$>
      nest 4 (cs <$$> nest 4 dv <+> "y0")


  render (RecordConstructor _ value) = do
    dv <- render value
    return . nest 4 $ "Json.Encode.object" <$$> "[" <+> dv <$$> "]"

 
  render mc@(MultipleConstructors constrs) = do
    let rndr = if isEnumeration mc
        then renderEnumeration
        else error "no dice"
    dc <- mapM rndr constrs
    return . nest 4 $ "case x of" <$$> foldl1 (<$+$>) dc

  -- Single constructor, multiple values: create array with values
  render _ = error "no dice"

renderEnumeration :: ElmConstructor -> RenderM Doc
renderEnumeration (NamedConstructor name _) =
  return . nest 4 $ stext name <+> "->" <$$>
      dquotes (stext name)
renderEnumeration (MultipleConstructors constrs) = do
  dc <- mapM renderEnumeration constrs
  return $ foldl1 (<$+$>) dc
renderEnumeration c = render c

instance HasStringFrom ElmValue where
  render (ElmPrimitiveRef primitive) = renderRef 0 primitive
  render _ = error "no dice, stringFRom non primitives or enumeration union types"

instance HasStringFromRef ElmPrimitive where
  renderRef _ EDate = pure "Date.toIsoString"
  renderRef _ ETimePosix = pure "Iso8601.fromTime"
  renderRef _ EUnit = pure "\"()\""
  renderRef _ EInt = pure "String.fromInt"
  renderRef _ EChar = pure "identity"
  renderRef _ EBool = pure "(\v -> if v == True then \"True\" else \"False\")"
  renderRef _ EFloat = pure "String.fromFloat"
  renderRef _ (EList (ElmPrimitive EChar)) = pure "identity"
  renderRef _ EString = pure "identity"
  renderRef level (EMaybe datatype) = do
    dd <- renderRef level datatype
    return . parens $ "Maybe.withDefault "
        <+> dquotes (stext "Nothing")
        <+> " << Maybe.map ((++) \"Just \" <<" <+> dd <+> ")"
  renderRef level (ETuple2 x y) = do
    dx <- renderRef (level + 1) x
    dy <- renderRef (level + 1) y
    let firstName = "m" <> int level
    let secondName = "n" <> int level
    return . parens $ "\\("<> firstName <> "," <+> secondName <> ") -> String.join \",\" [" <+> dx <+> firstName <> "," <+> dy <+> secondName <+> "]"
  renderRef _ dat = error $ "ElmType does not support stringFrom<x>" ++ show dat


toElmStringFromRefWith
  :: ElmType a
  => Options -> a -> T.Text
toElmStringFromRefWith options x =
  pprinter . fst $ evalRWS (renderRef 0 (toElmType x)) options ()

toElmStringFromRef
  :: ElmType a
  => a -> T.Text
toElmStringFromRef = toElmStringFromRefWith defaultOptions

toElmStringFromSourceWith
  :: ElmType a
  => Options -> a -> T.Text
toElmStringFromSourceWith options x =
  pprinter . fst $ evalRWS (render (toElmType x)) options ()

toElmStringFromSource
  :: ElmType a
  => a -> T.Text
toElmStringFromSource = toElmStringFromSourceWith defaultOptions

renderStringFrom
  :: ElmType a
  => a -> RenderM ()
renderStringFrom x = do
  require "Json.Encode"
  collectDeclaration . render . toElmType $ x

-- | Variable names for the members of constructors
-- Used in pattern matches
constructorParameters :: Int -> ElmValue -> [Doc]
constructorParameters _ ElmEmpty = [ empty ]
constructorParameters i (Values l r) =
    left ++ right
  where
    left = constructorParameters i l
    right = constructorParameters (length left + i) r
constructorParameters i _ = [ "y" <> int i ]


-- | Encode variables following the recipe of an ElmValue
renderVariable :: [Doc] -> ElmValue -> RenderM (Doc, [Doc])
renderVariable (d : ds) v@(ElmRef {}) = do
  v' <- render v
  return (v' <+> d, ds)
renderVariable ds ElmEmpty = return (empty, ds)
renderVariable (_ : ds) (ElmPrimitiveRef EUnit) =
  return ("Json.Encode.null", ds)
renderVariable (d : ds) (ElmPrimitiveRef ref) = do
  r <- renderRef 0 ref
  return (r <+> d, ds)
renderVariable ds (Values l r) = do
  (left, dsl) <- renderVariable ds l
  (right, dsr) <- renderVariable dsl r
  return (left <> comma <+> right, dsr)
renderVariable ds f@(ElmField _ _) = do
  f' <- render f
  return (f', ds)
renderVariable [] _ = error "Amount of variables does not match variables"
