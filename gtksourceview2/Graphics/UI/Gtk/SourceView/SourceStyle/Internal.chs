{-# OPTIONS_HADDOCK hide #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget SourceView
--
--  Author : Peter Gavin
--  derived from sourceview bindings by Axel Simon and Duncan Coutts
--
--  Created: 18 December 2008
--
--  Copyright (C) 2004-2008 Peter Gavin, Duncan Coutts, Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- #hide

-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
module Graphics.UI.Gtk.SourceView.SourceStyle.Internal (
  sourceStyleFromObject,
  sourceStyleToObject
  ) where

import Control.Monad (liftM, sequence)
import Data.Maybe (catMaybes)

import System.Glib.FFI
{#import System.Glib.GObject#}              (objectNew, constructNewGObject)
import System.Glib.GType (GType)
import qualified System.Glib.GTypeConstants as GType
import System.Glib.GValue (GValue, valueInit, allocaGValue)
import System.Glib.GValueTypes (valueSetString, valueSetBool)
{#import System.Glib.Properties#}
{#import Graphics.UI.Gtk.SourceView.Types#}
import Graphics.UI.Gtk.SourceView.SourceStyle

{# context lib="gtk" prefix="gtk" #}

sourceStyleFromObject :: SourceStyleObject -> IO SourceStyle
sourceStyleFromObject obj = do
  background        <- objectGetPropertyMaybeString "background"          obj
  backgroundSet     <- objectGetPropertyBool        "background-set"      obj
  bold              <- objectGetPropertyBool        "bold"                obj
  boldSet           <- objectGetPropertyBool        "bold-set"            obj
  foreground        <- objectGetPropertyMaybeString "foreground"          obj
  foregroundSet     <- objectGetPropertyBool        "foreground-set"      obj
  italic            <- objectGetPropertyBool        "italic"              obj
  italicSet         <- objectGetPropertyBool        "italic-set"          obj
  lineBackground    <- objectGetPropertyMaybeString "line-background"     obj
  lineBackgroundSet <- objectGetPropertyBool        "line-background-set" obj
  strikethrough     <- objectGetPropertyBool        "strikethrough"       obj
  strikethroughSet  <- objectGetPropertyBool        "strikethrough-set"   obj
  underline         <- objectGetPropertyBool        "underline"           obj
  underlineSet      <- objectGetPropertyBool        "underline-set"       obj
  return $ SourceStyle
           { sourceStyleBackground     = if backgroundSet     then background         else Nothing
           , sourceStyleBold           = if boldSet           then Just bold          else Nothing
           , sourceStyleForeground     = if foregroundSet     then foreground         else Nothing
           , sourceStyleItalic         = if italicSet         then Just italic        else Nothing
           , sourceStyleLineBackground = if lineBackgroundSet then lineBackground     else Nothing
           , sourceStyleStrikethrough  = if strikethroughSet  then Just strikethrough else Nothing
           , sourceStyleUnderline      = if underlineSet      then Just underline     else Nothing
           }

sourceStyleToObject :: SourceStyle -> IO SourceStyleObject
sourceStyleToObject ss =
  allocaGValue $ \backgroundV ->
  allocaGValue $ \backgroundSetV ->
  allocaGValue $ \boldV ->
  allocaGValue $ \boldSetV ->
  allocaGValue $ \foregroundV ->
  allocaGValue $ \foregroundSetV ->
  allocaGValue $ \italicV ->
  allocaGValue $ \italicSetV ->
  allocaGValue $ \lineBackgroundV ->
  allocaGValue $ \lineBackgroundSetV ->
  allocaGValue $ \strikethroughV ->
  allocaGValue $ \strikethroughSetV ->
  allocaGValue $ \underlineV ->
  allocaGValue $ \underlineSetV -> do
    params <- liftM concat . sequence $
              [ makeParam "background"      sourceStyleBackground     backgroundSetV     backgroundV     GType.string valueSetString
              , makeParam "bold"            sourceStyleBold           boldSetV           boldV           GType.bool   valueSetBool
              , makeParam "foreground"      sourceStyleForeground     foregroundSetV     foregroundV     GType.string valueSetString
              , makeParam "italic"          sourceStyleItalic         italicSetV         italicV         GType.bool   valueSetBool
              , makeParam "line-background" sourceStyleLineBackground lineBackgroundSetV lineBackgroundV GType.string valueSetString
              , makeParam "strikethrough"   sourceStyleStrikethrough  strikethroughSetV  strikethroughV  GType.bool   valueSetBool
              , makeParam "underline"       sourceStyleUnderline      underlineSetV      underlineV      GType.bool   valueSetBool
              ]
    constructNewGObject mkSourceStyleObject (liftM castPtr $ objectNew gTypeSourceStyleObject params)
  where makeParam name field setV v gtype valueSet = do
            valueInit setV GType.bool
            case field ss of
               Just field' -> do
                  valueSetBool setV True
                  valueInit v gtype
                  valueSet v field'
                  return [(name ++ "-set", setV), (name, v)]
               Nothing -> do
                  valueSetBool setV False
                  return [(name ++ "-set", setV)]
