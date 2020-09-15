{-# LANGUAGE OverloadedStrings #-}

module Elm.StringFrom
  ( toElmStringFromRef
  , toElmStringFromRefWith
  , toElmStringFromSource
  , toElmStringFromSourceWith
  , renderStringFrom
  )
where

import           Control.Monad.RWS
import qualified Data.Text                     as T
import           Elm.Common
import           Elm.Type
import           Text.PrettyPrint.Leijen.Text
                                         hiding ( (<$>) )
import           Elm.Type                       ( isEnumeration )

class HasStringFrom a where
  render :: a -> RenderM Doc

class HasStringFromRef a where
  renderRef :: Int -> a -> RenderM Doc

instance HasStringFrom ElmDatatype where
  render d@(ElmDatatype name constructor) = if (isEnumeration constructor)
    then do
      fnName <- renderRef 0 d
      ctor   <- render constructor
      return
        $    (fnName <+> ":" <+> stext name <+> "->" <+> "String")
        <$$> (fnName <+> "x =" <$$> indent 4 ctor)
    else error "can only stringFrom enumeration types"

  render (ElmPrimitive primitive) = renderRef 0 primitive


instance HasStringFromRef ElmDatatype where
  renderRef _     (ElmDatatype name _    ) = pure $ "stringFrom" <> stext name
  renderRef level (ElmPrimitive primitive) = renderRef level primitive

-- only allow enumerations or just pure primitives
instance HasStringFrom ElmConstructor where
  -- Single constructor, no values: empty array
  render (NamedConstructor _name ElmEmpty) = return $ dquotes $ stext _name

-- Single constructor, one value: skip constructor and r just the value
  render (NamedConstructor name  value   ) = do
    dv <- render value

    let cs = stext name <+> "y0 ->"
    return . nest 4 $ "case x of" <$$> nest 4 (cs <$$> nest 4 dv <+> "y0")

  render mc@(MultipleConstructors constrs) = do
    let rndr = if isEnumeration mc
          then renderEnumeration
          else error "can only stringFrom enumeration types"
    cstrs <- mapM rndr constrs
    pure $ "case x of" <$$> (indent 4 $ foldl1 (<$+$>) cstrs)

  render (RecordConstructor _ _) = error "String From called on incompatible constructor type"

renderEnumeration :: ElmConstructor -> RenderM Doc
renderEnumeration (NamedConstructor name _) =
  pure . nest 4 $ stext name <+> "->" <$$> dquotes ((stext name))
renderEnumeration (MultipleConstructors constrs) = do
  dc <- mapM renderEnumeration constrs
  pure $ foldl1 (<$+$>) dc
renderEnumeration c = render c

instance HasStringFrom ElmValue where
  render (ElmPrimitiveRef primitive) = renderRef 0 primitive
  render _ =
    error "stringFrom non primitives or enumeration union types"

instance HasStringFromRef ElmPrimitive where
  renderRef _ EDate                        = pure "Date.toIsoString"
  renderRef _ ETimePosix                   = pure "Iso8601.fromTime"
  renderRef _ EUnit                        = pure "\"()\""
  renderRef _ EInt                         = pure "String.fromInt"
  renderRef _ EChar                        = pure "identity"
  renderRef _ EBool                        = pure "String.fromBool"
  renderRef _ EFloat                       = pure "String.fromFloat"
  renderRef _ (EList (ElmPrimitive EChar)) = pure "identity"
  renderRef _ EString                      = pure "identity"
  renderRef _ dat = error $ "ElmType does not support stringFrom" ++ show dat


toElmStringFromRefWith :: ElmType a => Options -> a -> T.Text
toElmStringFromRefWith options x =
  pprinter . fst $ evalRWS (renderRef 0 (toElmType x)) options ()

toElmStringFromRef :: ElmType a => a -> T.Text
toElmStringFromRef = toElmStringFromRefWith defaultOptions

toElmStringFromSourceWith :: ElmType a => Options -> a -> T.Text
toElmStringFromSourceWith options x =
  pprinter . fst $ evalRWS (render (toElmType x)) options ()

toElmStringFromSource :: ElmType a => a -> T.Text
toElmStringFromSource = toElmStringFromSourceWith defaultOptions

renderStringFrom :: ElmType a => a -> RenderM ()
renderStringFrom x = do
  collectDeclaration . render . toElmType $ x
