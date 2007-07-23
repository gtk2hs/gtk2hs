-- GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer   -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Version $Revision$ from $Date$
--
--  Copyright (c) 2007 Peter Gavin
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 3 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
module Media.Streaming.GStreamer.Core.ElementFactory (
  
  ElementFactory,
  ElementFactoryClass,
  castToElementFactory,
  toElementFactory,
  fromElementFactory,
  
  elementFactoryFind,
  elementFactoryGetElementType,
  elementFactoryGetLongname,
  elementFactoryGetKlass,
  elementFactoryGetDescription,
  elementFactoryGetAuthor,
  elementFactoryGetNumPadTemplates,
  elementFactoryGetURIType,
  elementFactoryGetURIProtocols,
  elementFactoryCreate,
  elementFactoryMake
  
  ) where

import Control.Monad         ( liftM )
import System.Glib.FFI
import System.Glib.UTFString ( withUTFString
                             , peekUTFString
                             , peekUTFStringArray0 )
import System.Glib.GType     ( GType )
{# import Media.Streaming.GStreamer.Core.Types #}

{# context lib = "gstreamer" prefix = "gst" #}

elementFactoryFind :: String
                   -> IO ElementFactory
elementFactoryFind name =
    withUTFString name {# call element_factory_find #} >>= takeElementFactory

elementFactoryGetElementType :: (ElementFactoryClass elementFactory) =>
                                elementFactory
                             -> IO (Maybe GType)
elementFactoryGetElementType factory =
    do gtype <- {# call element_factory_get_element_type #} (toElementFactory factory)
       if gtype == 0
          then return $ Just $ fromIntegral gtype
          else return Nothing

elementFactoryGetLongname :: (ElementFactoryClass elementFactory) =>
                             elementFactory
                          -> IO String
elementFactoryGetLongname factory =
    {# call element_factory_get_longname #} (toElementFactory factory) >>=
        peekUTFString

elementFactoryGetKlass :: (ElementFactoryClass elementFactory) =>
                          elementFactory
                       -> IO String
elementFactoryGetKlass factory =
    {# call element_factory_get_klass #} (toElementFactory factory) >>=
        peekUTFString

elementFactoryGetDescription :: (ElementFactoryClass elementFactory) =>
                                elementFactory
                             -> IO String
elementFactoryGetDescription factory =
    {# call element_factory_get_description #} (toElementFactory factory) >>=
        peekUTFString

elementFactoryGetAuthor :: (ElementFactoryClass elementFactory) =>
                           elementFactory
                        -> IO String
elementFactoryGetAuthor factory =
    {# call element_factory_get_author #} (toElementFactory factory) >>=
        peekUTFString

elementFactoryGetNumPadTemplates :: (ElementFactoryClass elementFactory) =>
                                    elementFactory
                                 -> IO Word
elementFactoryGetNumPadTemplates factory =
    liftM fromIntegral $
        {# call element_factory_get_num_pad_templates #} $ toElementFactory factory

elementFactoryGetURIType :: (ElementFactoryClass elementFactory) =>
                            elementFactory
                         -> IO Word
elementFactoryGetURIType factory =
    liftM fromIntegral $
        {# call element_factory_get_uri_type #} $ toElementFactory factory

elementFactoryGetURIProtocols :: (ElementFactoryClass elementFactory) =>
                                 elementFactory
                              -> IO (Maybe [String])
elementFactoryGetURIProtocols factory =
    {# call element_factory_get_uri_protocols #} (toElementFactory factory) >>=
        maybePeek peekUTFStringArray0

elementFactoryCreate :: (ElementFactoryClass elementFactory) =>
                        elementFactory
                     -> String
                     -> IO (Maybe Element)
elementFactoryCreate factory name =
    withUTFString name $ \cName ->
        {# call element_factory_create #} (toElementFactory factory) cName >>=
            maybePeek takeElement

elementFactoryMake :: String
                   -> String
                   -> IO (Maybe Element)
elementFactoryMake factoryName name =
    withUTFString factoryName $ \cFactoryName ->
        withUTFString name $ \cName ->
            {# call element_factory_make #} cFactoryName cName >>=
                maybePeek takeElement
