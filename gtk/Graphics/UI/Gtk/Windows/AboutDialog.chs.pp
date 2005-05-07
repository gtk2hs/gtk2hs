-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget AboutDialog
--
--  Author : Duncan Coutts
--
--  Created: 1 March 2005
--
--  Version $Revision: 1.3 $ from $Date: 2005/05/07 20:57:31 $
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
  castToAboutDialog,

-- * Constructors
  aboutDialogNew,

-- * Methods
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
  aboutDialogSetEmailHook,
  aboutDialogSetUrlHook,

-- * Attributes
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
#endif
  ) where

import Monad	(liftM)
import Maybe	(fromMaybe)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.Attributes
import System.Glib.GObject      	(makeNewGObject, mkFunPtrDestructor)
import Graphics.UI.Gtk.Abstract.Object	(makeNewObject)
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

-- | Returns the program name displayed in the about dialog.
--
aboutDialogGetName :: AboutDialogClass self => self
 -> IO String -- ^ returns The program name.
aboutDialogGetName self =
  {# call gtk_about_dialog_get_name #}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the name to display in the about dialog. If this is not set, it
-- defaults to the program executable name.
--
aboutDialogSetName :: AboutDialogClass self => self
 -> String -- ^ @name@ - the program name
 -> IO ()
aboutDialogSetName self name =
  withUTFString name $ \namePtr ->
  {# call gtk_about_dialog_set_name #}
    (toAboutDialog self)
    namePtr

-- | Returns the version string.
--
aboutDialogGetVersion :: AboutDialogClass self => self -> IO String
aboutDialogGetVersion self =
  {# call gtk_about_dialog_get_version #}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the version string to display in the about dialog.
--
aboutDialogSetVersion :: AboutDialogClass self => self -> String -> IO ()
aboutDialogSetVersion self version =
  withUTFString version $ \versionPtr ->
  {# call gtk_about_dialog_set_version #}
    (toAboutDialog self)
    versionPtr

-- | Returns the copyright string.
--
aboutDialogGetCopyright :: AboutDialogClass self => self -> IO String
aboutDialogGetCopyright self =
  {# call gtk_about_dialog_get_copyright #}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the copyright string to display in the about dialog. This should be
-- a short string of one or two lines.
--
aboutDialogSetCopyright :: AboutDialogClass self => self -> String -> IO ()
aboutDialogSetCopyright self copyright =
  withUTFString copyright $ \copyrightPtr ->
  {# call gtk_about_dialog_set_copyright #}
    (toAboutDialog self)
    copyrightPtr

-- | Returns the comments string.
--
aboutDialogGetComments :: AboutDialogClass self => self -> IO String
aboutDialogGetComments self =
  {# call gtk_about_dialog_get_comments #}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the comments string to display in the about dialog. This should be a
-- short string of one or two lines.
--
aboutDialogSetComments :: AboutDialogClass self => self -> String -> IO ()
aboutDialogSetComments self comments =
  withUTFString comments $ \commentsPtr ->
  {# call gtk_about_dialog_set_comments #}
    (toAboutDialog self)
    commentsPtr

-- | Returns the license information.
--
aboutDialogGetLicense :: AboutDialogClass self => self -> IO (Maybe String)
aboutDialogGetLicense self =
  {# call gtk_about_dialog_get_license #}
    (toAboutDialog self)
  >>= maybePeek peekUTFString

-- | Sets the license information to be displayed in the secondary license
-- dialog. If @license@ is @Nothing@, the license button is hidden.
--
aboutDialogSetLicense :: AboutDialogClass self => self
 -> Maybe String -- ^ @license@ - the license information or @Nothing@
 -> IO ()
aboutDialogSetLicense self license =
  maybeWith withUTFString license $ \licensePtr ->
  {# call gtk_about_dialog_set_license #}
    (toAboutDialog self)
    licensePtr

-- | Returns the website URL.
--
aboutDialogGetWebsite :: AboutDialogClass self => self -> IO String
aboutDialogGetWebsite self =
  {# call gtk_about_dialog_get_website #}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the URL to use for the website link.
--
aboutDialogSetWebsite :: AboutDialogClass self => self
 -> String -- ^ @website@ - a URL string starting with \"http:\/\/\"
 -> IO ()
aboutDialogSetWebsite self website =
  withUTFString website $ \websitePtr ->
  {# call gtk_about_dialog_set_website #}
    (toAboutDialog self)
    websitePtr

-- | Returns the label used for the website link.
--
aboutDialogGetWebsiteLabel :: AboutDialogClass self => self -> IO String
aboutDialogGetWebsiteLabel self =
  {# call gtk_about_dialog_get_website_label #}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the label to be used for the website link. It defaults to the
-- website URL.
--
aboutDialogSetWebsiteLabel :: AboutDialogClass self => self -> String -> IO ()
aboutDialogSetWebsiteLabel self websiteLabel =
  withUTFString websiteLabel $ \websiteLabelPtr ->
  {# call gtk_about_dialog_set_website_label #}
    (toAboutDialog self)
    websiteLabelPtr

-- | Sets the strings which are displayed in the authors tab of the secondary
-- credits dialog.
--
aboutDialogSetAuthors :: AboutDialogClass self => self
 -> [String] -- ^ @authors@ - a list of author names
 -> IO ()
aboutDialogSetAuthors self authors =
  withUTFStringArray0 authors $ \authorsPtr ->
  {# call gtk_about_dialog_set_authors #}
    (toAboutDialog self)
    authorsPtr

-- | Returns the string which are displayed in the authors tab of the
-- secondary credits dialog.
--
aboutDialogGetAuthors :: AboutDialogClass self => self -> IO [String]
aboutDialogGetAuthors self =
  {# call gtk_about_dialog_get_authors #}
    (toAboutDialog self)
  >>= peekUTFStringArray0

-- | Sets the strings which are displayed in the artists tab of the secondary
-- credits dialog.
--
aboutDialogSetArtists :: AboutDialogClass self => self
 -> [String] -- ^ @artists@ - a list of artist names
 -> IO ()
aboutDialogSetArtists self artists =
  withUTFStringArray0 artists $ \artistsPtr ->
  {# call gtk_about_dialog_set_artists #}
    (toAboutDialog self)
    artistsPtr

-- | Returns the string which are displayed in the artists tab of the
-- secondary credits dialog.
--
aboutDialogGetArtists :: AboutDialogClass self => self -> IO [String]
aboutDialogGetArtists self =
  {# call gtk_about_dialog_get_artists #}
    (toAboutDialog self)
  >>= peekUTFStringArray0

-- | Sets the strings which are displayed in the documenters tab of the
-- secondary credits dialog.
--
aboutDialogSetDocumenters :: AboutDialogClass self => self
 -> [String] -- ^ @artists@ - a list of documenter names
 -> IO ()
aboutDialogSetDocumenters self documenters =
  withUTFStringArray0 documenters $ \documentersPtr ->
  {# call gtk_about_dialog_set_documenters #}
    (toAboutDialog self)
    documentersPtr

-- | Returns the string which are displayed in the documenters tab of the
-- secondary credits dialog.
--
aboutDialogGetDocumenters :: AboutDialogClass self => self -> IO [String]
aboutDialogGetDocumenters self =
  {# call gtk_about_dialog_get_documenters #}
    (toAboutDialog self)
  >>= peekUTFStringArray0

-- | Returns the translator credits string which is displayed in the
-- translators tab of the secondary credits dialog.
--
aboutDialogGetTranslatorCredits :: AboutDialogClass self => self -> IO String
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
aboutDialogSetTranslatorCredits :: AboutDialogClass self => self -> String -> IO ()
aboutDialogSetTranslatorCredits self translatorCredits =
  withUTFString translatorCredits $ \translatorCreditsPtr ->
  {# call gtk_about_dialog_set_translator_credits #}
    (toAboutDialog self)
    translatorCreditsPtr

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
aboutDialogGetLogoIconName :: AboutDialogClass self => self -> IO String
aboutDialogGetLogoIconName self =
  {# call gtk_about_dialog_get_logo_icon_name #}
    (toAboutDialog self)
  >>= peekUTFString

-- | Sets the pixbuf to be displayed as logo in the about dialog. If it is
-- @Nothing@, the default window icon set with 'windowSetDefaultIcon' will be
-- used.
--
aboutDialogSetLogoIconName :: AboutDialogClass self => self
 -> Maybe String -- ^ @iconName@ - an icon name, or @Nothing@
 -> IO ()
aboutDialogSetLogoIconName self iconName =
  maybeWith withUTFString iconName $ \iconNamePtr ->
  {# call gtk_about_dialog_set_logo_icon_name #}
    (toAboutDialog self)
    iconNamePtr

-- | Installs a global function to be called whenever the user activates an
-- email link in an about dialog.
--
aboutDialogSetEmailHook :: 
    (String -> IO ()) -- ^ @(\url -> ...)@ - a function to call when an email
                      -- link is activated.
 -> IO ()
aboutDialogSetEmailHook func = do
  funcPtr <- mkAboutDialogActivateLinkFunc (\_ linkPtr _ -> do
    link <- peekUTFString linkPtr
    func link
    )
  destroyPtr <- mkFunPtrDestructor funcPtr
  {# call gtk_about_dialog_set_email_hook #}
    funcPtr
    nullPtr
    destroyPtr
  return ()

-- | Installs a global function to be called whenever the user activates a URL
-- link in an about dialog.
--
aboutDialogSetUrlHook :: 
    (String -> IO ()) -- ^ @(\url -> ...)@ - a function to call when a URL link
                      -- is activated.
 -> IO ()
aboutDialogSetUrlHook func = do
  funcPtr <- mkAboutDialogActivateLinkFunc (\_ linkPtr _ -> do
    link <- peekUTFString linkPtr
    func link
    )
  destroyPtr <- mkFunPtrDestructor funcPtr
  {# call gtk_about_dialog_set_url_hook #}
    funcPtr
    nullPtr
    destroyPtr
  return ()

{# pointer AboutDialogActivateLinkFunc #}

foreign import ccall "wrapper" mkAboutDialogActivateLinkFunc ::
  (Ptr AboutDialog -> CString -> Ptr () -> IO ()) -> IO AboutDialogActivateLinkFunc

--------------------
-- Attributes

-- | The name of the program. If this is not set, it defaults to
-- 'gGetApplicationName'.
--
aboutDialogName :: AboutDialogClass self => Attr self String
aboutDialogName = newAttr
  aboutDialogGetName
  aboutDialogSetName

-- | The version of the program.
--
aboutDialogVersion :: AboutDialogClass self => Attr self String
aboutDialogVersion = newAttr
  aboutDialogGetVersion
  aboutDialogSetVersion

-- | Copyright information for the program.
--
aboutDialogCopyright :: AboutDialogClass self => Attr self String
aboutDialogCopyright = newAttr
  aboutDialogGetCopyright
  aboutDialogSetCopyright

-- | Comments about the program. This string is displayed in a label in the
-- main dialog, thus it should be a short explanation of the main purpose of
-- the program, not a detailed list of features.
--
aboutDialogComments :: AboutDialogClass self => Attr self String
aboutDialogComments = newAttr
  aboutDialogGetComments
  aboutDialogSetComments

-- | The license of the program. This string is displayed in a text view in a
-- secondary dialog, therefore it is fine to use a long multi-paragraph text.
-- Note that the text is not wrapped in the text view, thus it must contain the
-- intended linebreaks.
--
-- Default value: @Nothing@
--
aboutDialogLicense :: AboutDialogClass self => Attr self (Maybe String)
aboutDialogLicense = newAttr
  aboutDialogGetLicense
  aboutDialogSetLicense

-- | The URL for the link to the website of the program. This should be a
-- string starting with \"http:\/\/.
--
aboutDialogWebsite :: AboutDialogClass self => Attr self String
aboutDialogWebsite = newAttr
  aboutDialogGetWebsite
  aboutDialogSetWebsite

-- | The label for the link to the website of the program. If this is not set,
-- it defaults to the URL specified in the website property.
--
aboutDialogWebsiteLabel :: AboutDialogClass self => Attr self String
aboutDialogWebsiteLabel = newAttr
  aboutDialogGetWebsiteLabel
  aboutDialogSetWebsiteLabel

-- | The authors of the program. Each string may
-- contain email addresses and URLs, which will be displayed as links, see the
-- introduction for more details.
--
aboutDialogAuthors :: AboutDialogClass self => Attr self [String]
aboutDialogAuthors = newAttr
  aboutDialogGetAuthors
  aboutDialogSetAuthors

-- | The people documenting the program.
-- Each string may contain email addresses and URLs, which will be displayed as
-- links, see the introduction for more details.
--
aboutDialogDocumenters :: AboutDialogClass self => Attr self [String]
aboutDialogDocumenters = newAttr
  aboutDialogGetDocumenters
  aboutDialogSetDocumenters

-- | The people who contributed artwork to the program.
-- Each string may contain email addresses and URLs, which will be
-- displayed as links, see the introduction for more details.
--
aboutDialogArtists :: AboutDialogClass self => Attr self [String]
aboutDialogArtists = newAttr
  aboutDialogGetArtists
  aboutDialogSetArtists

-- | Credits to the translators. This string should be marked as translatable.
-- The string may contain email addresses and URLs, which will be displayed as
-- links, see the introduction for more details.
--
aboutDialogTranslatorCredits :: AboutDialogClass self => Attr self String
aboutDialogTranslatorCredits = newAttr
  aboutDialogGetTranslatorCredits
  aboutDialogSetTranslatorCredits

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
aboutDialogLogoIconName :: AboutDialogClass self => ReadWriteAttr self String (Maybe String)
aboutDialogLogoIconName = newAttr
  aboutDialogGetLogoIconName
  aboutDialogSetLogoIconName
#endif
