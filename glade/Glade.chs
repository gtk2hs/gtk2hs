-- GIMP Toolkit (GTK) Binding for Haskell: binding to Libglade   -*-haskell-*-
--    for loading XML widget specifications
--
--  Author : Manuel M T Chakravarty
--  Created: 13 March 2002
--
--  Version $Revision: 1.4 $ from $Date: 2005/09/25 08:40:26 $
--
--  Copyright (c) 2002 Manuel M T Chakravarty
--  Modified 2003 by Duncan Coutts (gtk2hs port)
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
-- |
--
--  Libglade facilitates loading of XML specifications of whole widget trees
--  that have been interactively designed with the GUI builder Glade.  The
--  present module exports operations for manipulating 'GladeXML' objects.
--
--  @glade_xml_signal_autoconnect()@ is not supported. The C variant is not
--  suitable for Haskell as @-rdynamic@ leads to huge executable and we
--  usually don't want to connect staticly named functions, but closures.
--
-- * @glade_xml_construct()@ is not bound, as it doesn't seem to be useful
--   in Haskell.  As usual, the @signal_connect_data@ variant for
--   registering signal handlers isn't bound either.  Moreover, the
--   @connect_full@ functions are not bound.
--
-- * This binding does not support Libglade functionality that is
--   exclusively meant for extending Libglade with new widgets.  Like new
--   widgets, such functionality is currently expected to be implemented in
--   C.
--

module Glade (

  -- * Data types
  --
  GladeXMLClass, GladeXML,

  -- * Creation operations
  --
  xmlNew, xmlNewWithRootAndDomain,

  -- * Obtaining widget handles
  --
  xmlGetWidget, xmlGetWidgetRaw

) where

import Monad	(liftM)
import FFI
import GType
import Object   (makeNewObject)
import GObject  (makeNewGObject)
{#import Hierarchy#}
{#import GladeType#}
import GList

{#context lib="glade" prefix ="glade"#}


-- | Create a new XML object (and the corresponding
-- widgets) from the given XML file; corresponds to
-- 'xmlNewWithRootAndDomain', but without the ability to specify a root
-- widget or translation domain.
--
xmlNew :: FilePath -> IO (Maybe GladeXML)
xmlNew file =
  withCString file                $ \strPtr1 -> do
  xmlPtr <- {#call unsafe xml_new#} strPtr1 nullPtr nullPtr
  if xmlPtr==nullPtr then return Nothing
                     else liftM Just $ makeNewGObject mkGladeXML (return xmlPtr)

-- | Create a new GladeXML object (and
-- the corresponding widgets) from the given XML file with an optional
-- root widget and translation domain.
--
-- * If the second argument is not @Nothing@, the interface will only be built
--   from the widget whose name is given.  This feature is useful if you only
--   want to build say a toolbar or menu from the XML file, but not the window
--   it is embedded in.  Note also that the XML parse tree is cached to speed
--   up creating another \'XML\' object for the same file.
--
xmlNewWithRootAndDomain :: FilePath -> Maybe String -> Maybe String -> IO (Maybe GladeXML)
xmlNewWithRootAndDomain file rootWidgetName domain =
  withCString file                $ \strPtr1 ->
  withMaybeCString rootWidgetName $ \strPtr2 ->
  withMaybeCString domain         $ \strPtr3 -> do
  xmlPtr <- {#call unsafe xml_new#} strPtr1 strPtr2 strPtr3
  if xmlPtr==nullPtr then return Nothing
                     else liftM Just $ makeNewGObject mkGladeXML (return xmlPtr)

-- | Get the widget that has the given name in
-- the interface description. If the named widget cannot be found
-- or is of the wrong type the result is an error.
--
-- * the second parameter should be a dynamic cast function that
--   returns the type of object that you expect, eg castToButton
--
-- * the third parameter is the ID of the widget in the glade xml
--   file, eg \"button1\".
--
xmlGetWidget :: (WidgetClass widget) => GladeXML -> (GObject -> widget) -> String -> IO widget
xmlGetWidget xml cast name = do
  maybeWidget <- xmlGetWidgetRaw xml name
  return $ case maybeWidget of
    Just widget -> cast (toGObject widget) --the cast will return an error if the object is of the wrong type
    Nothing -> error $ "glade.xmlGetWidget: no object named " ++ show name ++ " in the glade file"

xmlGetWidgetRaw :: GladeXML -> String -> IO (Maybe Widget)
xmlGetWidgetRaw xml name =
  withCString name $ \strPtr1 -> do
  widgetPtr <- {#call unsafe xml_get_widget#} xml strPtr1
  if widgetPtr==nullPtr then return Nothing
                        else liftM Just $ makeNewObject mkWidget (return widgetPtr)

-- Auxilliary routines
-- -------------------

-- Marshall an optional string
--
withMaybeCString :: Maybe String -> (Ptr CChar -> IO a) -> IO a
withMaybeCString  = maybeWith withCString
