{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget AboutDialog
--
--  Author : Duncan Coutts
--
--  Created: 1 March 2005
--
--  Copyright (C) 2005 Duncan Coutts
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
-- Display information about an application
--
-- * Module available since Gtk+ version 2.6
--
module Graphics.UI.Gtk.Windows.AboutDialog (
-- * Detail
--
-- | The 'AboutDialog' offers a simple way to display information about a
-- program like its logo, name, copyright, website and license. It is also
-- possible to give credits to the authors, documenters, translators and
-- artists who have worked on the program. An about dialog is typically opened
-- when the user selects the @About@ option from the @Help@ menu. All parts of
-- the dialog are optional.
--
-- About dialog often contain links and email addresses. 'AboutDialog'
-- supports this by offering global hooks, which are called when the user
-- clicks on a link or email address, see 'aboutDialogSetEmailHook' and
-- 'aboutDialogSetUrlHook'. Email addresses in the authors, documenters and
-- artists properties are recognized by looking for @\<user\@host>@, URLs are
-- recognized by looking for @http:\/\/url@, with @url@ extending to the next
-- space, tab or line break.
-- Since 2.18 'AboutDialog' provides default website and email hooks that
-- use 'showURI'.
--
-- Note that Gtk+ sets a default title of @_(\"About %s\")@ on the dialog
-- window (where %s is replaced by the name of the application, but in order to
-- ensure proper translation of the title, applications should set the title
-- property explicitly when constructing a 'AboutDialog', as shown in the
-- following example:
--
-- Note that prior to Gtk+ 2.12, the 'aboutDialogProgramName' property was called
-- 'aboutDialogName'. Both names may be used in Gtk2Hs.

-- * Class Hierarchy
-- |
-- @
-- |  'GObject'
-- |   +----'Object'
-- |         +----'Widget'
-- |               +----'Container'
-- |                     +----'Bin'
-- |                           +----'Window'
-- |                                 +----'Dialog'
-- |                                       +----AboutDialog
-- @

#if GTK_CHECK_VERSION(2,6,0)
-- * Types
  AboutDialog,
  AboutDialogClass,
  castToAboutDialog, gTypeAboutDialog,
  toAboutDialog,

-- * Constructors
  aboutDialogNew,

-- * Methods
#if GTK_MAJOR_VERSION < 3
  aboutDialogSetEmailHook,
  aboutDialogSetUrlHook,
#endif

-- * Attributes
  aboutDialogProgramName,
  aboutDialogName,
  aboutDialogVersion,
  aboutDialogCopyright,
  aboutDialogComments,
  aboutDialogLicense,
  aboutDialogWebsite,
  aboutDialogWebsiteLabel,
  aboutDialogAuthors,
  aboutDialogDocumenters,
  aboutDialogArtists,
  aboutDialogTranslatorCredits,
  aboutDialogLogo,
  aboutDialogLogoIconName,
#if GTK_CHECK_VERSION(2,8,0)
  aboutDialogWrapLicense,
#endif

-- * Deprecated
#ifndef DISABLE_DEPRECATED
  aboutDialogGetName,
  aboutDialogSetName,
  aboutDialogGetVersion,
  aboutDialogSetVersion,
  aboutDialogGetCopyright,
  aboutDialogSetCopyright,
  aboutDialogGetComments,
  aboutDialogSetComments,
  aboutDialogGetLicense,
  aboutDialogSetLicense,
  aboutDialogGetWebsite,
  aboutDialogSetWebsite,
  aboutDialogGetWebsiteLabel,
  aboutDialogSetWebsiteLabel,
  aboutDialogSetAuthors,
  aboutDialogGetAuthors,
  aboutDialogSetArtists,
  aboutDialogGetArtists,
  aboutDialogSetDocumenters,
  aboutDialogGetDocumenters,
  aboutDialogGetTranslatorCredits,
  aboutDialogSetTranslatorCredits,
  aboutDialogGetLogo,
  aboutDialogSetLogo,
  aboutDialogGetLogoIconName,
  aboutDialogSetLogoIconName,
#if GTK_CHECK_VERSION(2,8,0)
  aboutDialogGetWrapLicense,
  aboutDialogSetWrapLicense,
#endif
#endif
#endif
  ) where

import Control.Monad    (liftM)
import Data.Maybe       (fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.Properties
import Graphics.UI.Gtk.Abstract.Object  (makeNewObject)
{#import Graphics.UI.Gtk.Types#}

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,6,0)
--------------------
-- Constructors

-- | Creates a new 'AboutDialog'.
--
aboutDialogNew :: IO AboutDialog
aboutDialogNew =
  makeNewObject mkAboutDialog $
  liftM (castPtr :: Ptr Widget -> Ptr AboutDialog) $
  {# call gtk_about_dialog_new #}

--------------------
-- Methods

#ifndef DISABLE_DEPRECATED
-- | Returns the program name displayed in the about dialog.
--
aboutDialogGetName :: (AboutDialogClass self, GlibString string) => self
 -> IO string -- ^ returns The program name.
aboutDialogGetName self =
#if GTK_CHECK_VERSION(2,12,0)
  {# call gtk_about_dialog_get_program_name #}
#else
  {# call gtk_about_dialog_get_name #}
#endif
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the name to display in the about dialog. If this is not set, it
-- defaults to the program executable name.
--
aboutDialogSetName :: (AboutDialogClass self, GlibString string) => self
 -> string -- ^ @name@ - the program name
 -> IO ()
aboutDialogSetName self name =
  withUTFString name $ \namePtr ->
#if GTK_CHECK_VERSION(2,12,0)
  {# call gtk_about_dialog_set_program_name #}
#else
  {# call gtk_about_dialog_set_name #}
#endif
    (toAboutDialog self)
    namePtr

-- | Returns the version string.
--
aboutDialogGetVersion :: (AboutDialogClass self, GlibString string) => self -> IO string
aboutDialogGetVersion self =
  {# call gtk_about_dialog_get_version #}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the version string to display in the about dialog.
--
aboutDialogSetVersion :: (AboutDialogClass self, GlibString string) => self -> string -> IO ()
aboutDialogSetVersion self version =
  withUTFString version $ \versionPtr ->
  {# call gtk_about_dialog_set_version #}
    (toAboutDialog self)
    versionPtr

-- | Returns the copyright string.
--
aboutDialogGetCopyright :: (AboutDialogClass self, GlibString string) => self -> IO string
aboutDialogGetCopyright self =
  {# call gtk_about_dialog_get_copyright #}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the copyright string to display in the about dialog. This should be
-- a short string of one or two lines.
--
aboutDialogSetCopyright :: (AboutDialogClass self, GlibString string) => self -> string -> IO ()
aboutDialogSetCopyright self copyright =
  withUTFString copyright $ \copyrightPtr ->
  {# call gtk_about_dialog_set_copyright #}
    (toAboutDialog self)
    copyrightPtr

-- | Returns the comments string.
--
aboutDialogGetComments :: (AboutDialogClass self, GlibString string) => self -> IO string
aboutDialogGetComments self =
  {# call gtk_about_dialog_get_comments #}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the comments string to display in the about dialog. This should be a
-- short string of one or two lines.
--
aboutDialogSetComments :: (AboutDialogClass self, GlibString string) => self -> string -> IO ()
aboutDialogSetComments self comments =
  withUTFString comments $ \commentsPtr ->
  {# call gtk_about_dialog_set_comments #}
    (toAboutDialog self)
    commentsPtr

-- | Returns the license information.
--
aboutDialogGetLicense :: (AboutDialogClass self, GlibString string) => self -> IO (Maybe string)
aboutDialogGetLicense self =
  {# call gtk_about_dialog_get_license #}
    (toAboutDialog self)
  >>= maybePeek peekUTFString

-- | Sets the license information to be displayed in the secondary license
-- dialog. If @license@ is @Nothing@, the license button is hidden.
--
aboutDialogSetLicense :: (AboutDialogClass self, GlibString string) => self
 -> Maybe string -- ^ @license@ - the license information or @Nothing@
 -> IO ()
aboutDialogSetLicense self license =
  maybeWith withUTFString license $ \licensePtr ->
  {# call gtk_about_dialog_set_license #}
    (toAboutDialog self)
    licensePtr

-- | Returns the website URL.
--
aboutDialogGetWebsite :: (AboutDialogClass self, GlibString string) => self -> IO string
aboutDialogGetWebsite self =
  {# call gtk_about_dialog_get_website #}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the URL to use for the website link.
--
aboutDialogSetWebsite :: (AboutDialogClass self, GlibString string) => self
 -> string -- ^ @website@ - a URL string starting with \"http:\/\/\"
 -> IO ()
aboutDialogSetWebsite self website =
  withUTFString website $ \websitePtr ->
  {# call gtk_about_dialog_set_website #}
    (toAboutDialog self)
    websitePtr

-- | Returns the label used for the website link.
--
aboutDialogGetWebsiteLabel :: (AboutDialogClass self, GlibString string) => self -> IO string
aboutDialogGetWebsiteLabel self =
  {# call gtk_about_dialog_get_website_label #}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the label to be used for the website link. It defaults to the
-- website URL.
--
aboutDialogSetWebsiteLabel :: (AboutDialogClass self, GlibString string) => self -> string -> IO ()
aboutDialogSetWebsiteLabel self websiteLabel =
  withUTFString websiteLabel $ \websiteLabelPtr ->
  {# call gtk_about_dialog_set_website_label #}
    (toAboutDialog self)
    websiteLabelPtr
#endif

-- | Sets the strings which are displayed in the authors tab of the secondary
-- credits dialog.
--
aboutDialogSetAuthors :: (AboutDialogClass self, GlibString string) => self
 -> [string] -- ^ @authors@ - a list of author names
 -> IO ()
aboutDialogSetAuthors self authors =
  withUTFStringArray0 authors $ \authorsPtr ->
  {# call gtk_about_dialog_set_authors #}
    (toAboutDialog self)
    authorsPtr

-- | Returns the string which are displayed in the authors tab of the
-- secondary credits dialog.
--
aboutDialogGetAuthors :: (AboutDialogClass self, GlibString string) => self -> IO [string]
aboutDialogGetAuthors self =
  {# call gtk_about_dialog_get_authors #}
    (toAboutDialog self)
  >>= peekUTFStringArray0

-- | Sets the strings which are displayed in the artists tab of the secondary
-- credits dialog.
--
aboutDialogSetArtists :: (AboutDialogClass self, GlibString string) => self
 -> [string] -- ^ @artists@ - a list of artist names
 -> IO ()
aboutDialogSetArtists self artists =
  withUTFStringArray0 artists $ \artistsPtr ->
  {# call gtk_about_dialog_set_artists #}
    (toAboutDialog self)
    artistsPtr

-- | Returns the string which are displayed in the artists tab of the
-- secondary credits dialog.
--
aboutDialogGetArtists :: (AboutDialogClass self, GlibString string) => self -> IO [string]
aboutDialogGetArtists self =
  {# call gtk_about_dialog_get_artists #}
    (toAboutDialog self)
  >>= peekUTFStringArray0

-- | Sets the strings which are displayed in the documenters tab of the
-- secondary credits dialog.
--
aboutDialogSetDocumenters :: (AboutDialogClass self, GlibString string) => self
 -> [string] -- ^ @artists@ - a list of documenter names
 -> IO ()
aboutDialogSetDocumenters self documenters =
  withUTFStringArray0 documenters $ \documentersPtr ->
  {# call gtk_about_dialog_set_documenters #}
    (toAboutDialog self)
    documentersPtr

-- | Returns the string which are displayed in the documenters tab of the
-- secondary credits dialog.
--
aboutDialogGetDocumenters :: (AboutDialogClass self, GlibString string) => self -> IO [string]
aboutDialogGetDocumenters self =
  {# call gtk_about_dialog_get_documenters #}
    (toAboutDialog self)
  >>= peekUTFStringArray0

#ifndef DISABLE_DEPRECATED
-- | Returns the translator credits string which is displayed in the
-- translators tab of the secondary credits dialog.
--
aboutDialogGetTranslatorCredits :: (AboutDialogClass self, GlibString string) => self -> IO string
aboutDialogGetTranslatorCredits self =
  {# call gtk_about_dialog_get_translator_credits #}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the translator credits string which is displayed in the translators
-- tab of the secondary credits dialog.
--
-- The intended use for this string is to display the translator of the
-- language which is currently used in the user interface.
--
aboutDialogSetTranslatorCredits :: (AboutDialogClass self, GlibString string) => self -> string -> IO ()
aboutDialogSetTranslatorCredits self translatorCredits =
  withUTFString translatorCredits $ \translatorCreditsPtr ->
  {# call gtk_about_dialog_set_translator_credits #}
    (toAboutDialog self)
    translatorCreditsPtr
#endif

-- | Returns the pixbuf displayed as logo in the about dialog.
--
aboutDialogGetLogo :: AboutDialogClass self => self -> IO Pixbuf
aboutDialogGetLogo self =
  makeNewGObject mkPixbuf $
  {# call gtk_about_dialog_get_logo #}
    (toAboutDialog self)

-- | Sets the pixbuf to be displayed as logo in the about dialog. If it is
-- @Nothing@, the default window icon set with 'windowSetDefaultIcon' will be
-- used.
--
aboutDialogSetLogo :: AboutDialogClass self => self
 -> Maybe Pixbuf -- ^ @logo@ - a 'Pixbuf', or @Nothing@
 -> IO ()
aboutDialogSetLogo self logo =
  {# call gtk_about_dialog_set_logo #}
    (toAboutDialog self)
    (fromMaybe (Pixbuf nullForeignPtr) logo)

-- | Returns the icon name displayed as logo in the about dialog.
--
aboutDialogGetLogoIconName :: (AboutDialogClass self, GlibString string) => self -> IO string
aboutDialogGetLogoIconName self =
  {# call gtk_about_dialog_get_logo_icon_name #}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the pixbuf to be displayed as logo in the about dialog. If it is
-- @Nothing@, the default window icon set with 'windowSetDefaultIcon' will be
-- used.
--
aboutDialogSetLogoIconName :: (AboutDialogClass self, GlibString string) => self
 -> Maybe string -- ^ @iconName@ - an icon name, or @Nothing@
 -> IO ()
aboutDialogSetLogoIconName self iconName =
  maybeWith withUTFString iconName $ \iconNamePtr ->
  {# call gtk_about_dialog_set_logo_icon_name #}
    (toAboutDialog self)
    iconNamePtr

#if GTK_MAJOR_VERSION < 3
-- | Installs a global function to be called whenever the user activates an
-- email link in an about dialog.
--
-- Removed in Gtk3.
aboutDialogSetEmailHook :: GlibString string
 => (string -> IO ()) -- ^ @(\url -> ...)@ - a function to call when an email
                      -- link is activated.
 -> IO ()
aboutDialogSetEmailHook func = do
  funcPtr <- mkAboutDialogActivateLinkFunc (\_ linkPtr _ -> do
    link <- peekUTFString linkPtr
    func link
    )
  {# call gtk_about_dialog_set_email_hook #}
    funcPtr
    (castFunPtrToPtr funcPtr)
    destroyFunPtr
  return ()

-- | Installs a global function to be called whenever the user activates a URL
-- link in an about dialog.
--
-- Removed in Gtk3.
aboutDialogSetUrlHook ::GlibString string
 => (string -> IO ()) -- ^ @(\url -> ...)@ - a function to call when a URL link
                      -- is activated.
 -> IO ()
aboutDialogSetUrlHook func = do
  funcPtr <- mkAboutDialogActivateLinkFunc (\_ linkPtr _ -> do
    link <- peekUTFString linkPtr
    func link
    )
  {# call gtk_about_dialog_set_url_hook #}
    funcPtr
    (castFunPtrToPtr funcPtr)
    destroyFunPtr
  return ()

{# pointer AboutDialogActivateLinkFunc #}

foreign import ccall "wrapper" mkAboutDialogActivateLinkFunc ::
  (Ptr AboutDialog -> CString -> Ptr () -> IO ()) -> IO AboutDialogActivateLinkFunc
#endif

#ifndef DISABLE_DEPRECATED
#if GTK_CHECK_VERSION(2,8,0)
-- | Returns whether the license text in @about@ is automatically wrapped.
--
-- * Available since Gtk+ version 2.8
--
aboutDialogGetWrapLicense :: AboutDialogClass self => self
 -> IO Bool -- ^ returns @True@ if the license text is wrapped
aboutDialogGetWrapLicense self =
  liftM toBool $
  {# call gtk_about_dialog_get_wrap_license #}
    (toAboutDialog self)

-- | Sets whether the license text in @about@ is automatically wrapped.
--
-- * Available since Gtk+ version 2.8
--
aboutDialogSetWrapLicense :: AboutDialogClass self => self
 -> Bool  -- ^ @wrapLicense@ - whether to wrap the license
 -> IO ()
aboutDialogSetWrapLicense self wrapLicense =
  {# call gtk_about_dialog_set_wrap_license #}
    (toAboutDialog self)
    (fromBool wrapLicense)
#endif
#endif

--------------------
-- Attributes

-- | The name of the program. If this is not set, it defaults to
-- 'gGetApplicationName'.
--
aboutDialogName :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogName = newAttrFromStringProperty "name"

-- | The name of the program. If this is not set, it defaults to
-- 'gGetApplicationName'.
--
#if GTK_CHECK_VERSION(2,12,0)
aboutDialogProgramName :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogProgramName = newAttrFromStringProperty "program-name"
#else
aboutDialogProgramName :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogProgramName = newAttrFromStringProperty "name"
#endif

-- | The version of the program.
--
aboutDialogVersion :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogVersion = newAttrFromStringProperty "version"

-- | Copyright information for the program.
--
aboutDialogCopyright :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogCopyright = newAttrFromStringProperty "copyright"

-- | Comments about the program. This string is displayed in a label in the
-- main dialog, thus it should be a short explanation of the main purpose of
-- the program, not a detailed list of features.
--
aboutDialogComments :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogComments = newAttrFromStringProperty "comments"

-- | The license of the program. This string is displayed in a text view in a
-- secondary dialog, therefore it is fine to use a long multi-paragraph text.
-- Note that the text is only wrapped in the text view if the 'aboutDialogWrapLicense'
-- property is set to @True@; otherwise the text itself must contain the
-- intended linebreaks.
--
-- Default value: @Nothing@
--
aboutDialogLicense :: (AboutDialogClass self, GlibString string) => Attr self (Maybe string)
aboutDialogLicense = newAttrFromMaybeStringProperty "license"

-- | The URL for the link to the website of the program. This should be a
-- string starting with \"http:\/\/.
--
aboutDialogWebsite :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogWebsite = newAttrFromStringProperty "website"

-- | The label for the link to the website of the program. If this is not set,
-- it defaults to the URL specified in the website property.
--
aboutDialogWebsiteLabel :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogWebsiteLabel = newAttrFromStringProperty "website-label"

-- | The authors of the program. Each string may
-- contain email addresses and URLs, which will be displayed as links, see the
-- introduction for more details.
--
aboutDialogAuthors :: (AboutDialogClass self, GlibString string) => Attr self [string]
aboutDialogAuthors = newAttr
  aboutDialogGetAuthors
  aboutDialogSetAuthors

-- | The people documenting the program.
-- Each string may contain email addresses and URLs, which will be displayed as
-- links, see the introduction for more details.
--
aboutDialogDocumenters :: (AboutDialogClass self, GlibString string) => Attr self [string]
aboutDialogDocumenters = newAttr
  aboutDialogGetDocumenters
  aboutDialogSetDocumenters

-- | The people who contributed artwork to the program.
-- Each string may contain email addresses and URLs, which will be
-- displayed as links, see the introduction for more details.
--
aboutDialogArtists :: (AboutDialogClass self, GlibString string) => Attr self [string]
aboutDialogArtists = newAttr
  aboutDialogGetArtists
  aboutDialogSetArtists

-- | Credits to the translators. This string should be marked as translatable.
-- The string may contain email addresses and URLs, which will be displayed as
-- links, see the introduction for more details.
--
aboutDialogTranslatorCredits :: (AboutDialogClass self, GlibString string) => Attr self string
aboutDialogTranslatorCredits = newAttrFromStringProperty "translator-credits"

-- | A logo for the about box. If this is not set, it defaults to
-- 'windowGetDefaultIconList'.
--
aboutDialogLogo :: AboutDialogClass self => ReadWriteAttr self Pixbuf (Maybe Pixbuf)
aboutDialogLogo = newAttr
  aboutDialogGetLogo
  aboutDialogSetLogo

-- | A named icon to use as the logo for the about box. This property
-- overrides the logo property.
--
-- Default value: @Nothing@
--
aboutDialogLogoIconName :: (AboutDialogClass self, GlibString string) => ReadWriteAttr self string (Maybe string)
aboutDialogLogoIconName = newAttr
  aboutDialogGetLogoIconName
  aboutDialogSetLogoIconName
#endif

#if GTK_CHECK_VERSION(2,8,0)
-- | Whether to wrap the text in the license dialog.
--
-- Default value: @False@
--
aboutDialogWrapLicense :: AboutDialogClass self => Attr self Bool
aboutDialogWrapLicense = newAttrFromBoolProperty "wrap-license"
#endif
