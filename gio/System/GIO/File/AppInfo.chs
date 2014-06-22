{-# LANGUAGE CPP #-}
--  GIMP Toolkit (GTK) Binding for Haskell: binding to gio -*-haskell-*-
--
--  Author : Andy Stewart
--  Created: 30-Apirl-2010
--
--  Copyright (c) 2010 Andy Stewart
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
--  GIO, the C library which this Haskell library depends on, is
--  available under LGPL Version 2. The documentation included with
--  this library is based on the original GIO documentation.
--
-- | Maintainer  : gtk2hs-devel@lists.sourceforge.net
--   Stability   : alpha
--   Portability : portable (depends on GHC)
module System.GIO.File.AppInfo (
-- * Details
-- | 'AppInfo' and 'AppLaunchContext' are used for describing and launching applications installed on the
-- system.
--
-- As of GLib 2.20, URIs will always be converted to POSIX paths (using 'fileGetPath' when using
-- 'appInfoLaunch' even if the application requested an URI and not a POSIX path. For example for
-- an desktop-file based application with Exec key totem %U and a single URI, sftp://foo/file.avi, then
-- /home/user/.gvfs/sftp on foo/file.avi will be passed. This will only work if a set of suitable GIO
-- extensions (such as gvfs 2.26 compiled with FUSE support), is available and operational; if this is
-- not the case, the URI will be passed unmodified to the application. Some URIs, such as mailto:, of
-- course cannot be mapped to a POSIX path (in gvfs there's no FUSE mount for it); such URIs will be
-- passed unmodified to the application.
--
-- Specifically for gvfs 2.26 and later, the POSIX URI will be mapped back to the GIO URI in the 'File'
-- constructors (since gvfs implements the GVfs extension point). As such, if the application needs to
-- examine the URI, it needs to use 'fileGetUri' or similar on 'File'. In other words, an
-- application cannot assume that the URI passed to e.g. 'fileNewForCommandlineArg ' is equal to
-- the result of 'fileGetUri'.

-- * Types
  AppInfo,
  AppInfoClass,
  AppLaunchContext,
  AppLaunchContextClass,

-- * Enums
  AppInfoCreateFlags (..),

-- * Methods
  appInfoCreateFromCommandline,
  appInfoDup,
  appInfoEqual,
  appInfoGetId,
  appInfoGetName,
#if GLIB_CHECK_VERSION(2,24,0)
  appInfoGetDisplayName,
#endif
  appInfoGetDescription,
  appInfoGetExecutable,
#if GLIB_CHECK_VERSION(2,20,0)
  appInfoGetCommandline,
#endif
  appInfoGetIcon,
  appInfoLaunch,
  appInfoSupportsFiles,
  appInfoSupportsUris,
  appInfoLaunchUris,
  appInfoShouldShow,
#if GLIB_CHECK_VERSION(2,20,0)
  appInfoCanDelete,
  appInfoDelete,
  appInfoResetTypeAssociations,
#endif
  appInfoSetAsDefaultForType,
  appInfoSetAsDefaultForExtension,
  appInfoAddSupportsType,
  appInfoCanRemoveSupportsType,
  appInfoRemoveSupportsType,
  appInfoGetAll,
  appInfoGetAllForType,
  appInfoGetDefaultForType,
  appInfoGetDefaultForUriScheme,
  appInfoLaunchDefaultForUri,
  appLaunchContextGetDisplay,
  appLaunchContextGetStartupNotifyId,
  appLaunchContextLaunchFailed,
  appLaunchContextNew
  ) where

import Control.Monad
import Data.Maybe (fromMaybe)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafePackCStringFinalizer)
import System.GIO.Enums
import System.Glib.FFI
import System.Glib.Flags
import System.Glib.GError
import System.Glib.GList
import System.Glib.GObject
import System.Glib.UTFString
{#import System.GIO.Types#}

{# context lib = "gio" prefix = "g" #}

-- | Creates a new 'AppInfo' from the given information.
appInfoCreateFromCommandline ::
    GlibString string
 => string -- ^ @commandline@      the commandline to use
 -> Maybe string             -- ^ @applicationName@ the application name, or 'Nothing' to use commandline
 -> [AppInfoCreateFlags] -- ^ @flags@            flags that can specify details of the created 'AppInfo'
 -> IO AppInfo      -- ^ returns          new 'AppInfo' for given command.
appInfoCreateFromCommandline commandline applicationName flags =
    wrapNewGObject mkAppInfo $
    withUTFString commandline $ \ commandlinePtr ->
        maybeWith withUTFString applicationName $ \ applicationNamePtr ->
            propagateGError ({#call g_app_info_create_from_commandline #}
                             commandlinePtr
                             applicationNamePtr
                             ((fromIntegral . fromFlags) flags))

-- | Creates a duplicate of a 'AppInfo'.
appInfoDup :: AppInfoClass appinfo => appinfo -> IO AppInfo
appInfoDup appinfo =
    wrapNewGObject mkAppInfo $
    {#call g_app_info_dup #} (toAppInfo appinfo)

-- | Checks if two 'AppInfo's are equal.
appInfoEqual :: (AppInfoClass info1, AppInfoClass info2) => info1 -> info2
 -> Bool  -- ^ returns  'True' if appinfo1 is equal to appinfo2. 'False' otherwise.
appInfoEqual info1 info2 =
  unsafePerformIO $ liftM toBool $
  {#call g_app_info_equal #} (toAppInfo info1) (toAppInfo info2)

-- | Gets the ID of an application. An id is a string that identifies the application. The exact format
-- of the id is platform dependent. For instance, on Unix this is the desktop file id from the xdg menu
-- specification.
--
-- Note that the returned ID may be 'Nothing', depending on how the appinfo has been constructed.
appInfoGetId :: (AppInfoClass appinfo, GlibString string)
 => appinfo
 -> IO (Maybe string)
appInfoGetId appinfo =
  {#call g_app_info_get_id#} (toAppInfo appinfo)
  >>= maybePeek readUTFString

-- | Gets the installed name of the application.
appInfoGetName :: AppInfoClass appinfo => appinfo
 -> String -- ^ returns the name of the application for appinfo.
appInfoGetName appinfo =
  unsafePerformIO $
  {#call g_app_info_get_name#} (toAppInfo appinfo)
  >>= readCString

#if GLIB_CHECK_VERSION(2,24,0)
-- | Gets the installed name of the application.
appInfoGetDisplayName :: (AppInfoClass appinfo, GlibString string)
 => appinfo
 -> string -- ^ returns the display name of the application for appinfo, or the name if no display name is available.
appInfoGetDisplayName appinfo =
  unsafePerformIO $
  {#call g_app_info_get_display_name#} (toAppInfo appinfo)
  >>= readUTFString
#endif

-- | Gets a human-readable description of an installed application.
appInfoGetDescription :: (AppInfoClass appinfo, GlibString string)
 => appinfo
 -> Maybe string -- ^ returns a string containing a description of the application appinfo, or 'Nothing' if none.
appInfoGetDescription appinfo =
  unsafePerformIO $ do
  {#call g_app_info_get_description#} (toAppInfo appinfo)
  >>= maybePeek peekUTFString

-- | Gets the executable's name for the installed application.
appInfoGetExecutable :: (AppInfoClass appinfo, GlibString string)
 => appinfo
 -> string -- ^ returns the executable of the application for appinfo.
appInfoGetExecutable appinfo =
  unsafePerformIO $
  {#call g_app_info_get_executable#} (toAppInfo appinfo)
  >>= peekUTFString

#if GLIB_CHECK_VERSION(2,20,0)
-- | Gets the commandline with which the application will be started.
appInfoGetCommandline :: AppInfoClass appinfo => appinfo
 -> Maybe ByteString -- ^ returns a string containing the appinfo's commandline, or 'Nothing' if this information is not available
appInfoGetCommandline appinfo =
  unsafePerformIO $ do
  sPtr <- {#call g_app_info_get_commandline#} (toAppInfo appinfo)
  if sPtr == nullPtr
     then return Nothing
     else do
       sLen <- lengthArray0 0 sPtr
       liftM Just $ unsafePackCStringFinalizer (castPtr sPtr) (fromIntegral sLen)
                        ({#call unsafe g_free#} (castPtr sPtr))
#endif

-- | Gets the icon for the application.
appInfoGetIcon :: AppInfoClass appinfo => appinfo
 -> IO (Maybe Icon)              -- ^ returns the default 'Icon' for appinfo or 'Nothing'
appInfoGetIcon appinfo =
  maybeNull (makeNewGObject mkIcon) $
  {#call g_app_info_get_icon#} (toAppInfo appinfo)

-- | Launches the application. Passes files to the launched application as arguments, using the optional
-- @launchContext@ to get information about the details of the launcher (like what screen it is on).
-- Throws a 'GError' if an error occurs
--
-- To lauch the application without arguments pass a emtpy files list.
--
-- Note that even if the launch is successful the application launched can fail to start if it runs
-- into problems during startup. There is no way to detect this.
--
-- Some URIs can be changed when passed through a 'File' (for instance unsupported uris with strange
-- formats like mailto:), so if you have a textual uri you want to pass in as argument, consider using
-- 'appInfoLaunchUris' instead.
appInfoLaunch :: AppInfoClass appinfo => appinfo
 -> [File]  -- ^ @files@          a list of 'File' objects
 -> Maybe AppLaunchContext  -- ^ @launchContext@ a 'AppLaunchContext' or 'Nothing'
 -> IO ()
appInfoLaunch appinfo files launchContext =
    withForeignPtrs (map unFile files) $ \wFilePtr ->
    withGList wFilePtr $ \filesPtr ->
        propagateGError (\gErrorPtr -> do
                          {#call g_app_info_launch#}
                           (toAppInfo appinfo)
                           filesPtr
                           (fromMaybe (AppLaunchContext nullForeignPtr) launchContext)
                           gErrorPtr
                          return ())

-- | Checks if the application accepts files as arguments.
appInfoSupportsFiles :: AppInfoClass appinfo => appinfo
 -> IO Bool  -- ^ returns 'True' if the appinfo supports files.
appInfoSupportsFiles appinfo =
  liftM toBool $
  {#call g_app_info_supports_files #} (toAppInfo appinfo)

-- | Checks if the application accepts uris as arguments.
appInfoSupportsUris :: AppInfoClass appinfo => appinfo
 -> IO Bool  -- ^ returns 'True' if the appinfo supports uris.
appInfoSupportsUris appinfo =
  liftM toBool $
  {#call g_app_info_supports_uris #} (toAppInfo appinfo)

-- | Launches the application. Passes uris to the launched application as arguments, using the optional
-- @launchContext@ to get information about the details of the launcher (like what screen it is on).
-- Throws a 'GError' if an error occurs.
--
-- To lauch the application without arguments pass a empty uris list.
--
-- Note that even if the launch is successful the application launched can fail to start if it runs
-- into problems during startup. There is no way to detect this.
appInfoLaunchUris :: (AppInfoClass appinfo, GlibString string)
 => appinfo
 -> [string] -- ^ @uris@           a list containing URIs to launch.
 -> Maybe AppLaunchContext -- ^ @launchContext@ a 'AppLaunchContext' or 'Nothing'
 -> IO ()
appInfoLaunchUris appinfo uris launchContext =
    withUTFStringArray uris $ \urisPtr ->
        propagateGError (\gErrorPtr -> do
                           {#call g_app_info_launch_uris#}
                             (toAppInfo appinfo)
                             (castPtr urisPtr)
                             (fromMaybe (AppLaunchContext nullForeignPtr) launchContext)
                             gErrorPtr
                           return ())

-- | Checks if the application info should be shown in menus that list available applications.
appInfoShouldShow :: AppInfoClass appinfo => appinfo
 -> IO Bool  -- ^ returns 'True' if the appinfo should be shown, 'False' otherwise.
appInfoShouldShow appinfo =
  liftM toBool $
  {#call g_app_info_should_show#} (toAppInfo appinfo)

#if GLIB_CHECK_VERSION(2,20,0)
-- | Obtains the information whether the 'AppInfo' can be deleted. See 'appInfoDelete'.
appInfoCanDelete :: AppInfoClass appinfo => appinfo
 -> IO Bool -- ^ returns 'True' if appinfo can be deleted
appInfoCanDelete appinfo =
  liftM toBool $
  {#call g_app_info_can_delete#} (toAppInfo appinfo)

-- | Tries to delete a 'AppInfo'.
--
-- On some platforms, there may be a difference between user-defined 'AppInfo's which can be deleted,
-- and system-wide ones which cannot. See 'appInfoCanDelete'.
appInfoDelete :: AppInfoClass appinfo => appinfo
 -> IO Bool -- ^ returns 'True' if appinfo has been deleted
appInfoDelete appinfo =
  liftM toBool $
  {#call g_app_info_delete#} (toAppInfo appinfo)

-- | Removes all changes to the type associations done by 'appInfoSetAsDefaultForType',
-- 'appInfoSetAsDefaultForExtension' or
-- 'appInfoRemoveSupportsType'.
appInfoResetTypeAssociations ::
    GlibString string
 => string  -- ^ @contentType@ a content type
 -> IO ()
appInfoResetTypeAssociations contentType =
  withUTFString contentType $ \ contentTypePtr ->
  {#call g_app_info_reset_type_associations#} (castPtr contentTypePtr)
#endif

-- | Sets the application as the default handler for a given type.
-- Throws a 'GError' if an error occurs.
appInfoSetAsDefaultForType :: (AppInfoClass appinfo, GlibString string)
 => appinfo
 -> string -- ^ @contentType@ the content type.
 -> IO ()
appInfoSetAsDefaultForType appinfo contentType =
  withUTFString contentType $ \ contentTypePtr ->
      propagateGError (\gErrorPtr -> do
                         {#call g_app_info_set_as_default_for_type#}
                            (toAppInfo appinfo)
                            (castPtr contentTypePtr)
                            gErrorPtr
                         return ())

-- | Sets the application as the default handler for a given extension.
-- Throws a 'GError' if an error occurs.
appInfoSetAsDefaultForExtension :: (AppInfoClass appinfo, GlibString string)
 => appinfo
 -> string -- ^ @extension@ a string containing the file extension (without the dot).
 -> IO ()
appInfoSetAsDefaultForExtension appinfo extension =
  withUTFString extension $ \ extensionPtr ->
      propagateGError (\gErrorPtr -> do
                         {#call g_app_info_set_as_default_for_extension #}
                             (toAppInfo appinfo)
                             (castPtr extensionPtr)
                             gErrorPtr
                         return ())

-- | Adds a content type to the application information to indicate the application is capable of opening
-- files with the given content type.
-- Throws a 'GError' if an error occurs.
appInfoAddSupportsType :: (AppInfoClass appinfo, GlibString string)
 => appinfo
 -> string -- ^ @contentType@ a string.
 -> IO ()
appInfoAddSupportsType appinfo extension =
  withUTFString extension $ \ extensionPtr ->
      propagateGError (\gErrorPtr -> do
                         {#call g_app_info_add_supports_type#}
                           (toAppInfo appinfo)
                           (castPtr extensionPtr)
                           gErrorPtr
                         return ())

-- | Checks if a supported content type can be removed from an application.
appInfoCanRemoveSupportsType :: AppInfoClass appinfo => appinfo
 -> IO Bool -- ^ returns 'True' if it is possible to remove supported content types from a given appinfo, 'False' if not.
appInfoCanRemoveSupportsType appinfo =
  liftM toBool $
  {#call g_app_info_can_remove_supports_type#} (toAppInfo appinfo)

-- | Removes a supported type from an application, if possible.
-- Throws a 'GError' if an error occurs.
appInfoRemoveSupportsType :: (AppInfoClass appinfo, GlibString string)
 => appinfo
 -> string -- ^ @contentType@ a string.
 -> IO ()
appInfoRemoveSupportsType appinfo extension =
  withUTFString extension $ \ extensionPtr ->
      propagateGError (\gErrorPtr -> do
                          {#call g_app_info_remove_supports_type#}
                            (toAppInfo appinfo)
                            (castPtr extensionPtr)
                            gErrorPtr
                          return ())

-- | Gets a list of all of the applications currently registered on this system.
--
-- For desktop files, this includes applications that have NoDisplay=true set or are excluded from
-- display by means of OnlyShowIn or NotShowIn. See 'appInfoShouldShow'. The returned list does
-- not include applications which have the Hidden key set.
appInfoGetAll :: IO [AppInfo]
appInfoGetAll = do
  glistPtr <- {# call g_app_info_get_all #}
  list <- fromGList glistPtr
  mapM (wrapNewGObject mkAppInfo . return) list

-- | Gets a list of all 'AppInfo's for a given content type.
appInfoGetAllForType ::
    GlibString string
 => string  -- ^ @contentType@ the content type to find a 'AppInfo' for
 -> IO [AppInfo] -- ^ returns list of 'AppInfo's for given @contentType@ or 'empty' on error.
appInfoGetAllForType contentType =
  withUTFString contentType $ \ contentTypePtr -> do
    glistPtr <- {# call g_app_info_get_all_for_type #} (castPtr contentTypePtr)
    if glistPtr == nullPtr
       then return []
       else do
         list <- fromGList glistPtr
         mapM (wrapNewGObject mkAppInfo . return) list

-- | Gets the 'AppInfo' that corresponds to a given content type.
appInfoGetDefaultForType ::
    GlibString string
 => string  -- ^ @contentType@ the content type to find a 'AppInfo' for
 -> Bool  -- ^ @mustSupportUris@ if 'True', the 'AppInfo' is expected to support URIs
 -> IO (Maybe AppInfo)         -- ^ returns list of 'AppInfo's for given @contentType@ or 'Nothing' on error.
appInfoGetDefaultForType contentType mustSupportUris =
  maybeNull (wrapNewGObject mkAppInfo) $
  withUTFString contentType $ \ contentTypePtr -> do
  {# call g_app_info_get_default_for_type #}
             (castPtr contentTypePtr)
             (fromBool mustSupportUris)

-- | Gets the default application for launching applications using this URI scheme. A URI scheme is the
-- initial part of the URI, up to but not including the ':', e.g. "http", "ftp" or "sip".
appInfoGetDefaultForUriScheme ::
    GlibString string
 => string -- ^ @uriScheme@ a string containing a URI scheme.
 -> IO (Maybe AppInfo)         -- ^ returns list of 'AppInfo's for given @contentType@ or 'Nothing' on error.
appInfoGetDefaultForUriScheme uriScheme =
  maybeNull (wrapNewGObject mkAppInfo) $
  withUTFString uriScheme $ \ uriSchemePtr ->
    {# call g_app_info_get_default_for_uri_scheme #} (castPtr uriSchemePtr)

-- | Utility function that launches the default application registered to handle the specified
-- uri. Synchronous I/O is done on the uri to detect the type of the file if required.
-- Throws a 'GError' if an error occurs.
appInfoLaunchDefaultForUri ::
    GlibString string
 => string  -- ^ @uri@            the uri to show
 -> AppLaunchContext -- ^ @launchContext@ an optional 'AppLaunchContext'.
 -> IO ()
appInfoLaunchDefaultForUri uri launchContext =
  withUTFString uri $ \ uriPtr ->
      propagateGError (\gErrorPtr -> do
                         {#call g_app_info_launch_default_for_uri#}
                            (castPtr uriPtr)
                            launchContext
                            gErrorPtr
                         return ())

-- | Gets the display string for the display. This is used to ensure new applications are started on the
-- same display as the launching application.
appLaunchContextGetDisplay :: (AppInfoClass appinfo, GlibString string)
 => AppLaunchContext
 -> appinfo
 -> [File]  -- ^ @files@          a list of 'File' objects
 -> IO string -- ^ returns a display string for the display.
appLaunchContextGetDisplay launchContext appinfo files =
    withForeignPtrs (map unFile files) $ \wFilePtr ->
    withGList wFilePtr $ \filesPtr ->
        {#call g_app_launch_context_get_display#}
          (toAppLaunchContext launchContext)
          (toAppInfo appinfo)
          filesPtr
        >>= readUTFString

-- | Initiates startup notification for the application and returns the DesktopStartupId for the
-- launched operation, if supported.
--
-- Startup notification IDs are defined in the FreeDesktop.Org Startup Notifications standard.
appLaunchContextGetStartupNotifyId :: (AppInfoClass appinfo, GlibString string)
 => AppLaunchContext
 -> appinfo
 -> [File]  -- ^ @files@          a list of 'File' objects
 -> IO (Maybe string) -- ^ returns a startup notification ID for the application, or 'Nothing' if not supported.
appLaunchContextGetStartupNotifyId launchContext appinfo files =
    withForeignPtrs (map unFile files) $ \wFilePtr ->
    withGList wFilePtr $ \filesPtr ->
        {#call g_app_launch_context_get_startup_notify_id #}
                (toAppLaunchContext launchContext)
                (toAppInfo appinfo)
                filesPtr
        >>= maybePeek readUTFString

-- | Called when an application has failed to launch, so that it can cancel the application startup
-- notification started in 'appLaunchContextGetStartupNotifyId'.
appLaunchContextLaunchFailed :: GlibString string
 => AppLaunchContext
 -> string -- ^ @startupNotifyId@ the startup notification id that was returned by 'appLaunchContextGetStartupNotifyId'.
 -> IO ()
appLaunchContextLaunchFailed launchContext startupNotifyId =
  withUTFString startupNotifyId $ \ startupNotifyIdPtr ->
  {#call g_app_launch_context_launch_failed#}
  (toAppLaunchContext launchContext)
  (castPtr startupNotifyIdPtr)

-- | Creates a new application launch context. This is not normally used, instead you instantiate a
-- subclass of this, such as 'AppLaunchContext'.
appLaunchContextNew :: IO AppLaunchContext
appLaunchContextNew =
    wrapNewGObject mkAppLaunchContext $
    {#call g_app_launch_context_new#}
