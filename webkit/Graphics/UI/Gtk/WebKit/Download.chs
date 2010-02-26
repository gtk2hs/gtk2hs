{-# LANGUAGE CPP #-}
-- -*-haskell-*-
-----------------------------------------------------------------------------
--  Module      :  Graphics.UI.Gtk.WebKit.Download
--  Author 		:  Cjacker Huang
--  Copyright   :  (c) 2009 Cjacker Huang <jzhuang@redflag-linux.com>
--  Copyright   :  (c) 2010 Andy Stewart <lazycat.manatee@gmail.com>
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
-- |
-- Maintainer  : gtk2hs-users@lists.sourceforge.net
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Object used to communicate with the application when downloading 
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.Download (
-- * Types
  Download,

-- * Enums
  DownloadError(..),
  DownloadStatus(..),

-- * Constructors
  downloadNew,

-- * Methods
  DownloadError(..),

  downloadStart,
  downloadCancel,
  downloadGetUri,
  downloadGetNetworkRequest,
  downloadGetSuggestedFilename,
  downloadGetDestinationUri,
  downloadGetProgress,
  downloadGetElapsedTime,
  downloadGetTotalSize,
  downloadGetCurrentSize,
  downloadGetStatus,
  downloadSetDestinationUri,

-- * Attributes
  currentSize,
  destinationUri,
  networkRequest,
  networkResponse,
  progress,
  status,
  suggestedFilename,
  totalSize,

-- * Signals
  downloadError,
) where

import Control.Monad		(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.GError 
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Gdk.Events

{#import Graphics.UI.Gtk.Abstract.Object#}	(makeNewObject)
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.WebKit.Types#}
{#import System.Glib.GObject#}

{#context lib="webkit" prefix ="webkit"#}

-- * Enums

{#enum DownloadError {underscoreToCase}#}
{#enum DownloadStatus {underscoreToCase}#}


------------------
-- Constructors


-- | Create a new 'Download' instance for the given 'NetworkRequest'
--
-- Object used to communicate with the application when downloading.
downloadNew :: NetworkRequestClass request => request -> IO Download
downloadNew nr = 
    makeNewGObject mkDownload $ {#call download_new#} (toNetworkRequest nr)

-- | Initiates the 'Download'.
--
-- Notice that you must have set the destination-uri property before
-- calling this function.
downloadStart::
    DownloadClass self => self
 -> IO()
downloadStart dl =
    {#call download_start#} (toDownload dl)

-- | Cancels the 'Download'.
downloadCancel::
    DownloadClass self => self
 -> IO()
downloadCancel dl =
    {#call download_cancel#} (toDownload dl)

-- | Retrieves the URI from 'Download' which is being downloaded.
downloadGetUri::
    DownloadClass self => self
 -> IO (Maybe String) -- ^ the uri or @Nothing@ in case of failed
downloadGetUri dl = 
    {#call download_get_uri#} (toDownload dl)
    >>= maybePeek peekCString

-- | Retrieves the 'NetworkRequest' that backs the download process.
downloadGetNetworkRequest :: 
    DownloadClass self => self
 -> IO NetworkRequest
downloadGetNetworkRequest dl =
    makeNewGObject mkNetworkRequest $ {#call download_get_network_request#} (toDownload dl)

-- | Retrieves the filename that was suggested by the server,
-- or the one derived from the URI.
downloadGetSuggestedFilename :: 
    DownloadClass self => self
 -> IO (Maybe String) -- ^ the suggested filename or @Nothing@ in case of failed
downloadGetSuggestedFilename dl =
    {#call download_get_suggested_filename#} (toDownload dl) >>= maybePeek peekCString

-- | Obtains the URI to which the downloaded file will be written.
--
-- It is set by Application before call 'downloadStart' 
downloadGetDestinationUri :: 
    DownloadClass self => self
 -> IO (Maybe String)
downloadGetDestinationUri dl =
    {#call download_get_destination_uri#} (toDownload dl) >>= maybePeek peekCString

-- | Defines the URI that should be used to save the downloaded file to.
downloadSetDestinationUri :: 
    DownloadClass self => self
 -> String  -- ^ @destination_uri@ - the destination URI
 -> IO()
downloadSetDestinationUri dl dest =
    withCString dest $ \destPtr ->
        {#call download_set_destination_uri#} (toDownload dl) destPtr

-- |Determines the current progress of the 'Download'
downloadGetProgress :: 
    DownloadClass self => self
 -> IO Double -- ^ a 'Double' ranging from 0.0 to 1.0
downloadGetProgress dl =
    liftM realToFrac $ {#call download_get_progress#} (toDownload dl)

-- |Return elapsed time for the 'Download' in seconds.
-- includeing any fractional part.
--
-- If the 'Download' is finished, had an error or was cancelled,
-- this is the time between its start and the event.
downloadGetElapsedTime ::
    DownloadClass self => self
 -> IO Double -- ^ seconds since the 'Download' was started.
downloadGetElapsedTime dl =
    liftM realToFrac $ {#call download_get_elapsed_time#} (toDownload dl)

-- |Returns the excepted total size of the download.
--
-- This is expected because the server may provide incorrect or missing
-- Content-Length. 
--
-- Notice that this may grow over time.
downloadGetTotalSize :: 
    DownloadClass self => self
 -> IO Int -- ^ the expected total size of the downloaded file.
downloadGetTotalSize dl =
    liftM fromIntegral $ {#call download_get_total_size#} (toDownload dl)

-- | Returns the current already downleaded size
downloadGetCurrentSize :: 
    DownloadClass self => self
 -> IO Int -- ^ the already downloaded size.
downloadGetCurrentSize dl =
    liftM fromIntegral $ {#call download_get_current_size#} (toDownload dl)

-- | Obtains the current status of the 'Download' as 'DownloadStatus'
downloadGetStatus :: 
    DownloadClass self => self
 -> IO DownloadStatus -- ^ the current 'DownloadStatus'
downloadGetStatus dl = 
    liftM (toEnum . fromIntegral) $ {#call download_get_status#} (toDownload dl)

-- * Attibutes

-- | The length of the data already downloaded
--
-- Default value: 0
-- 
-- * Since 1.1.2 
--
currentSize :: DownloadClass self => ReadAttr self Int
currentSize = readAttr downloadGetCurrentSize

-- | The URI of the save location for this download.
--
-- Default value: ""
--
-- * Since 1.1.2
destinationUri :: DownloadClass self => Attr self (Maybe String) 
destinationUri = newAttrFromMaybeStringProperty "destination-uri"

-- | The NetworkRequest instance associated with the download.
--
-- * Since 1.1.2
networkRequest :: DownloadClass self => Attr self NetworkRequest
networkRequest = 
  newAttrFromObjectProperty "network-request"
  {#call pure webkit_network_request_get_type#}

-- | The NetworkResponse instance associated with the download.
--
-- * Since 1.1.16
networkResponse :: DownloadClass self => Attr self NetworkResponse
networkResponse = 
  newAttrFromObjectProperty "network-response"
  {#call pure webkit_network_response_get_type#}

-- | Determines the current progress of the download. 
-- Notice that, although the progress changes are reported as soon as possible, 
-- the emission of the notify signal for this property is throttled, for the benefit of download managers. 
-- If you care about every update, use 'Download' : currentSize.
--
-- Allowed values: [0,1]
--
-- Default value: 1
--
-- * Since 1.1.2
progress :: DownloadClass self => ReadAttr self Double
progress = readAttr downloadGetProgress

-- | Determines the current status of the download.
--
-- Default value: 'DownloadStatusCreated'
--
-- * Since 1.1.2
status :: DownloadClass self => ReadAttr self DownloadStatus
status = readAttr downloadGetStatus

-- | The file name suggested as default when saving
--
-- Default value: ""
--
-- * Since 1.1.2
suggestedFilename :: DownloadClass self => ReadAttr self (Maybe String)
suggestedFilename = readAttr downloadGetSuggestedFilename

-- | The total size of the file
--
-- Default value: 0
--
-- * Since 1.1.2
totalSize :: DownloadClass self => ReadAttr self Int
totalSize = readAttr downloadGetTotalSize

-- * Signals

-- | Emitted when download is interrupted either by user action or by network errors, 
-- errorDetail will take any value of 'DownloadError'.
--
-- 'download':    the object on which the signal is emitted                  
-- 'errorCode':   the corresponding error code                               
-- 'errorDetail': detailed error code for the error, see 'DownloadError' 
-- 'reason':       a string describing the error                              
--                                                                          
-- Since 1.1.2
downloadError :: DownloadClass self => Signal self (Int -> Int -> String -> IO Bool)
downloadError = Signal (connect_INT_INT_STRING__BOOL "error")
