-- GIMP Toolkit (GTK) Binding for Haskell: binding to Libglade   -*-haskell-*-
--    for loading XML widget specifications
--
--  Author : Manuel M T Chakravarty
--  Created: 13 March 2002
--
--  Copyright (c) 2002 Manuel M T Chakravarty
--  Modified 2003, 2005 by Duncan Coutts (gtk2hs port)
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
-- Notes:
--
--  glade_xml_signal_autoconnect() is not supported. The C variant is not
--  suitable for Haskell as -rdynamic leads to huge executable and we
--  usually don't want to connect staticly named functions, but closures.
--
--  glade_xml_construct() is not bound, as it doesn't seem to be useful
--  in Haskell.  As usual, the signal_connect_data variant for
--  registering signal handlers isn't bound either.  Moreover, the
--  connect_full functions are not bound.
--
-- |
--
--  Libglade facilitates loading of XML specifications of whole widget trees
--  that have been interactively designed with the GUI builder Glade.  The
--  present module exports operations for manipulating 'GladeXML' objects.
--
-- * This binding does not support Libglade functionality that is exclusively
-- meant for extending Libglade with new widgets. Like new widgets, such
-- functionality is currently expected to be implemented in C.
--
module Graphics.UI.Gtk.Glade (

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

import Control.Monad	(liftM)
import Control.Exception (evaluate)

import System.Glib.FFI
import System.Glib.GType
import Graphics.UI.Gtk.Abstract.Object   (makeNewObject)
import System.Glib.GObject  (constructNewGObject)
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Glade.Types#}
import System.Glib.GList

{#context lib="glade" prefix ="glade"#}


-- | Create a new XML object (and the corresponding widgets) from the given
-- XML file.
--
-- This corresponds to 'xmlNewWithRootAndDomain', but without the ability
-- to specify a root widget or translation domain.
--
xmlNew :: FilePath -> IO (Maybe GladeXML)
xmlNew file =
  withCString file                $ \strPtr1 -> do
  xmlPtr <- {#call unsafe xml_new#} strPtr1 nullPtr nullPtr
  if xmlPtr==nullPtr then return Nothing
                     else liftM Just $ constructNewGObject mkGladeXML (return xmlPtr)

-- | Create a new GladeXML object (and the corresponding widgets) from the
-- given XML file.
--
-- Optionally it will only build the interface from the widget
-- node @root@ (if it is not 'Nothing'). This feature is useful if you only
-- want to build say a toolbar or menu from the XML file, but not the window
-- it is embedded in.
--
-- Note also that the XML parse tree is cached to speed up creating another
-- 'GladeXML' object for the same file.
--
xmlNewWithRootAndDomain ::
    FilePath      -- ^ the XML file name.
 -> Maybe String  -- ^ @root@ - the widget node in fname to start building
                  -- from (or 'Nothing')
 -> Maybe String  -- ^ @domain@ - the translation domain for the XML file (or
                  -- 'Nothing' for default)
 -> IO (Maybe GladeXML)
xmlNewWithRootAndDomain file rootWidgetName domain =
  maybeNull (constructNewGObject mkGladeXML) $
  withCString file $ \filePtr ->
  maybeWith withCString rootWidgetName $ \rootWidgetNamePtr ->
  maybeWith withCString domain $ \domainPtr ->
  {# call unsafe xml_new #}
    filePtr
    rootWidgetNamePtr
    domainPtr

-- | Get the widget that has the given name in
-- the interface description. If the named widget cannot be found
-- or is of the wrong type the result is an error.
--
xmlGetWidget :: (WidgetClass widget) =>
    GladeXML
 -> (GObject -> widget) -- ^ a dynamic cast function that returns the type of
                        -- object that you expect, eg castToButton
 -> String              -- ^ the second parameter is the ID of the widget in
                        -- the glade xml file, eg \"button1\".
 -> IO widget
xmlGetWidget xml cast name = do
  maybeWidget <- xmlGetWidgetRaw xml name
  case maybeWidget of
    Just widget -> evaluate (cast (toGObject widget))
    	--the cast will return an error if the object is of the wrong type
    Nothing -> fail $ "glade.xmlGetWidget: no object named " ++ show name ++ " in the glade file"

-- | Like 'xmlGetWidget' but it does not do any casting and if the named
-- widget is not found then the result is 'Nothing' rather than an error.
--
xmlGetWidgetRaw :: GladeXML -> String -> IO (Maybe Widget)
xmlGetWidgetRaw xml name =
  maybeNull (makeNewObject mkWidget) $
  withCString name $ \namePtr -> do
  {# call unsafe xml_get_widget #} xml namePtr
