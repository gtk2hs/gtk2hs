--  GIMP Toolkit (GTK) Binding for Haskell: binding to gstreamer -*-haskell-*-
--
--  Author : Peter Gavin
--  Created: 1-Apr-2007
--
--  Copyright (c) 2007 Peter Gavin
--
--  This library is free software: you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public License
--  as published by the Free Software Foundation, either version 3 of
--  the License, or (at your option) any later version.
--  
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--  
--  You should have received a copy of the GNU Lesser General Public
--  License along with this program.  If not, see
--  <http://www.gnu.org/licenses/>.
--  
--  GStreamer, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GStreamer documentation.
--  
--  |
--  Maintainer  : gtk2hs-devel@lists.sourceforge.net
--  Stability   : alpha
--  Portability : portable (depends on GHC)
--  
--  A factory for creating 'Element's.
module Media.Streaming.GStreamer.Core.ElementFactory (

-- * Detail

  -- | 'ElementFactory' is used to create instances of 'Element's.
  --   
  --   Use 'elementFactoryFind' and 'elementFactoryCreate' to create
  --   element instances, or use 'elementFactoryMake' as a convenient
  --   shortcut.

-- * Types  
  ElementFactory,
  ElementFactoryClass,
  castToElementFactory,
  toElementFactory,
  
-- * ElementFactory Operations
  elementFactoryFind,
  elementFactoryGetElementType,
  elementFactoryGetLongname,
  elementFactoryGetKlass,
  elementFactoryGetDescription,
  elementFactoryGetAuthor,
  elementFactoryGetNumPadTemplates,
  elementFactoryGetURIType,
  elementFactoryGetURIProtocols,
#if GSTREAMER_CHECK_VERSION(0,10,14)
  elementFactoryHasInterface,
#endif
  elementFactoryCreate,
  elementFactoryMake,
  elementFactoryCanSinkCaps,
  elementFactoryCanSrcCaps,
  elementFactoryGetPadTemplates
  
  ) where

import Control.Monad         ( liftM )
import Data.Maybe            ( fromMaybe )
import System.Glib.FFI
import System.Glib.UTFString ( withUTFString
                             , peekUTFString
                             , peekUTFStringArray0 )
import System.Glib.GType     ( GType )
import System.Glib.GList     ( readGList )
{# import Media.Streaming.GStreamer.Core.Types #}

{# context lib = "gstreamer" prefix = "gst" #}

-- | Search for an element factory with the given name.
elementFactoryFind :: String                    -- ^ @name@ - the name of the desired factory
                   -> IO (Maybe ElementFactory) -- ^ the factory if found, otherwise 'Nothing'
elementFactoryFind name =
    withUTFString name {# call element_factory_find #} >>= maybePeek takeObject

-- | Get the 'GType' for elements managed by the given factory. The type
--   can only be retrieved if the element factory is loaded, which can
--   be assured with
--   'Media.Streaming.GStreamer.Core.PluginFeature.pluginFeatureLoad'.
elementFactoryGetElementType :: (ElementFactoryClass elementFactory)
                             => elementFactory   -- ^ @factory@ - an element factory
                             -> IO (Maybe GType) -- ^ the type of elements managed
                                                 --   by the factory, or 'Nothing' if
                                                 --   the factory is not loaded
elementFactoryGetElementType factory =
    do gtype <- {# call element_factory_get_element_type #} (toElementFactory factory)
       if gtype == 0
          then return $ Just $ fromIntegral gtype
          else return Nothing

-- | Get the long name for the given factory.
elementFactoryGetLongname :: (ElementFactoryClass elementFactory)
                          => elementFactory -- ^ @factory@ - an element factory
                          -> IO String      -- ^ the factory's long name
elementFactoryGetLongname factory =
    {# call element_factory_get_longname #} (toElementFactory factory) >>=
        peekUTFString

-- | Get the class for the given factory.
elementFactoryGetKlass :: (ElementFactoryClass elementFactory)
                       => elementFactory -- ^ @factory@ - an element factory
                       -> IO String      -- ^ the factory's class
elementFactoryGetKlass factory =
    {# call element_factory_get_klass #} (toElementFactory factory) >>=
        peekUTFString

-- | Get the description for the given factory.
elementFactoryGetDescription :: (ElementFactoryClass elementFactory)
                             => elementFactory -- ^ @factory@ - an element factory
                             -> IO String      -- ^ the factory's description
elementFactoryGetDescription factory =
    {# call element_factory_get_description #} (toElementFactory factory) >>=
        peekUTFString

-- | Get the author of the given factory.
elementFactoryGetAuthor :: (ElementFactoryClass elementFactory)
                        => elementFactory -- ^ @factory@ - an element factory
                        -> IO String      -- ^ the factory's author
elementFactoryGetAuthor factory =
    {# call element_factory_get_author #} (toElementFactory factory) >>=
        peekUTFString

-- | Get the number of 'PadTemplate's provided by the given factory.
elementFactoryGetNumPadTemplates :: (ElementFactoryClass elementFactory)
                                 => elementFactory -- ^ @factory@ - an element factory
                                 -> IO Word        -- ^ the number of 'PadTemplate's
elementFactoryGetNumPadTemplates factory =
    liftM fromIntegral $
        {# call element_factory_get_num_pad_templates #} $ toElementFactory factory

-- | Get the type of URIs supported by the given factory.
elementFactoryGetURIType :: (ElementFactoryClass elementFactory)
                         => elementFactory -- ^ @factory@ - an element factory
                         -> IO Int         -- ^ the type of URIs supported by the factory
elementFactoryGetURIType factory =
    liftM fromIntegral $
        {# call element_factory_get_uri_type #} $ toElementFactory factory

-- | Get the list of protocols supported by the given factory.
elementFactoryGetURIProtocols :: (ElementFactoryClass elementFactory)
                              => elementFactory -- ^ @factory@ - an element factory
                              -> IO [String]    -- ^ the supported protocols
elementFactoryGetURIProtocols factory =
    {# call element_factory_get_uri_protocols #} (toElementFactory factory) >>=
        liftM (fromMaybe []) . maybePeek peekUTFStringArray0

#if GSTREAMER_CHECK_VERSION(0,10,14)
-- | Check if the given factory implements the interface with the given name.
elementFactoryHasInterface :: (ElementFactoryClass elementFactory)
                           => elementFactory -- ^ @factory@ - an element factory
                           -> String         -- ^ @name@ - the interface name
                           -> IO Bool        -- ^ true if the interface is implemented
elementFactoryHasInterface factory name =
    liftM toBool .
        withUTFString name .
        {# call element_factory_has_interface #} .
        toElementFactory $
        factory
#endif

-- | Create a new element of the type supplied by the given
--   factory. It will be given the name supplied.
elementFactoryCreate :: (ElementFactoryClass elementFactory)
                     => elementFactory     -- ^ @factory@ - an element factory
                     -> String             -- ^ @name@ - the new element's name
                     -> IO (Maybe Element) -- ^ the new element if it could be created,
                                           --   otherwise 'Nothing'
elementFactoryCreate factory name =
    withUTFString name $ \cName ->
        {# call element_factory_create #} (toElementFactory factory) cName >>=
            maybePeek takeObject

-- | Create a new element of the type supplied by the named
--   factory.
elementFactoryMake :: String             -- ^ @factoryName@ - the name of an element factory
                   -> Maybe String       -- ^ @name@ - the new element's name, or
                                         --   'Nothing' generate a unique name
                   -> IO (Maybe Element) -- ^ the new element if it could be created,
                                         --   otherwise 'Nothing'
elementFactoryMake factoryName name =
    withUTFString factoryName $ \cFactoryName ->
        maybeWith withUTFString name $ \cName ->
            {# call element_factory_make #} cFactoryName cName >>=
                maybePeek takeObject

-- | Check if the given factory can sink the given capabilities.
elementFactoryCanSinkCaps :: (ElementFactoryClass elementFactory)
                          => elementFactory -- ^ @factory@ - an element factory
                          -> Caps           -- ^ @caps@ - the capabilities to check for
                          -> IO Bool        -- ^ 'True' if @factory@ can sink the given capabilities
elementFactoryCanSinkCaps factory caps =
    liftM toBool $ {# call element_factory_can_sink_caps #} (toElementFactory factory) caps

-- | Check if the given factory can source the given capabilities.
elementFactoryCanSrcCaps :: (ElementFactoryClass elementFactory)
                         => elementFactory -- ^ @factory@ - an element factory
                         -> Caps           -- ^ @caps@ - the capabilities to check for
                         -> IO Bool        -- ^ 'True' if @factory@ can source the given capabilities
elementFactoryCanSrcCaps factory caps =
    liftM toBool $ {# call element_factory_can_src_caps #} (toElementFactory factory) caps

-- | Get the pad templates provided by the given factory.
elementFactoryGetPadTemplates :: (ElementFactoryClass elementFactory)
                              => elementFactory   -- ^ @factory@ - an element factory
                              -> IO [PadTemplate] -- ^ the provided pad templates
elementFactoryGetPadTemplates =
    {# call element_factory_get_static_pad_templates #} . toElementFactory >=>
        readGList >=> mapM staticPadTemplateGet
    where infixr 8 >=>
          a >=> b = \x -> a x >>= b
