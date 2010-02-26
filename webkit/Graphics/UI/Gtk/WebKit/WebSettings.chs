{-# LANGUAGE CPP #-}
-- -*-haskell-*-
-----------------------------------------------------------------------------
--  Module      :  Graphics.UI.Gtk.WebKit.WebSettings
--  Author      :  Cjacker Huang
--  Copyright   :  (c) 2009 Cjacker Huang <jzhuang@redflag-linux.com>
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
-- Control the behaviour of a 'WebView'
-----------------------------------------------------------------------------

module Graphics.UI.Gtk.WebKit.WebSettings (
-- * Types
  WebSettings,

-- * Constructors
  webSettingsNew,

-- * Methods
  webSettingsCopy,
  webSettingsGetUserAgent,

-- * Attributes
  webSettingsAutoLoadImages,
  webSettingsAutoShrinkImages,
  webSettingsCursiveFontFamily,
  webSettingsDefaultEncoding,
  webSettingsDefaultFontFamily,
  webSettingsDefaultFontSize,
  webSettingsDefaultMonospaceFontSize,
  webSettingsEditingBehavior,
  webSettingsEnableCaretBrowsing,
  webSettingsEnableDeveloperExtras,
  webSettingsEnableHtml5Database,
  webSettingsEnableHtml5LocalStorage,
  webSettingsEnableOfflineWebApplicationCache,
  webSettingsEnablePlugins,
  webSettingsEnablePrivateBrowsing,
  webSettingsEnableScripts,
  webSettingsEnableSpellChecking,
  webSettingsEnableUniversalAccessFromFileUris,
  webSettingsEnableXssAuditor,
  webSettingsEnforce96Dpi,
  webSettingsFantasyFontFamily,
  webSettingsJSCanOpenWindowAuto,
  webSettingsMinimumFontSize,
  webSettingsMinimumLogicalFontSize,
  webSettingsMonospaceFontFamily,
  webSettingsPrintBackgrounds,
  webSettingsResizableTextAreas,
  webSettingsSansFontFamily,
  webSettingsSerifFontFamily,
  webSettingsSpellCheckingLang,
  webSettingsUserAgent,
  webSettingsUserStylesheetUri,
  webSettingsZoomStep,
  webSettingsEnableSiteSpecificQuirks,
) where

import Control.Monad		(liftM)

import System.Glib.FFI
import System.Glib.UTFString
import System.Glib.GList
import System.Glib.GError 
import System.Glib.Properties
import System.Glib.Attributes
import Graphics.UI.Gtk.Gdk.Events

{#import Graphics.UI.Gtk.Abstract.Object#}	(makeNewObject)
{#import Graphics.UI.Gtk.Signals#}
{#import Graphics.UI.Gtk.WebKit.Types#}
{#import System.Glib.GObject#}

{#context lib="webkit" prefix ="webkit"#}

{#enum EditingBehavior {underscoreToCase}#}
------------------
-- Constructors


-- | Create a new 'WebSettings' instance.
-- 
-- A 'WebSettings' can be applied to a 'WebView'
-- to control the to be used text encoding, color, font size, 
-- printing mode,script support, loading of images and various other things.
webSettingsNew :: IO WebSettings
webSettingsNew = 
    constructNewGObject mkWebSettings $ {#call web_settings_new#}


-- | Copy an existing 'WebSettings' instance.
webSettingsCopy :: 
    WebSettingsClass self => self
 -> IO WebSettings
webSettingsCopy websettings = 
    constructNewGObject mkWebSettings $ {#call web_settings_copy#} (toWebSettings websettings)

-- | Return the User-Agent string currently used.
webSettingsGetUserAgent :: 
    WebSettingsClass self => self
 -> IO (Maybe String) -- ^ User-Agent string or @Nothing@ in case failed.
webSettingsGetUserAgent websettings = 
    {#call web_settings_get_user_agent#} (toWebSettings websettings) >>= maybePeek peekCString

-- | Load images automatically
--
-- Default value: True
webSettingsAutoLoadImages :: (WebSettingsClass self) => Attr self Bool
webSettingsAutoLoadImages = newAttrFromBoolProperty "auto-load-images"

-- | Automatically shrink standalone images to fit
--
-- Default value: True
webSettingsAutoShrinkImages :: (WebSettingsClass self) => Attr self Bool
webSettingsAutoShrinkImages = newAttrFromBoolProperty "auto-shrink-images"

-- | The default Cursive font family used to display text
--
-- Default value "serif"
webSettingsCursiveFontFamily :: (WebSettingsClass self) => Attr self String
webSettingsCursiveFontFamily = newAttrFromStringProperty "cursive-font-family"

-- | The default encoding used to display text
--
-- Default value "iso-8859-1"

webSettingsDefaultEncoding :: (WebSettingsClass self) => Attr self String
webSettingsDefaultEncoding = newAttrFromStringProperty "default-encoding"

-- | The default font family used to display text
--
-- Default value: "sans-serif"

webSettingsDefaultFontFamily :: (WebSettingsClass self) => Attr self String
webSettingsDefaultFontFamily = newAttrFromStringProperty "default-font-family"

-- | The default font size used to display text
--
-- Default value: >=5

webSettingsDefaultFontSize :: (WebSettingsClass self) => Attr self Int
webSettingsDefaultFontSize = newAttrFromIntProperty "default-font-size"

-- | The default font size used to display monospace text
--
-- Allowed values: >= 5
-- 
-- Default value: 10

webSettingsDefaultMonospaceFontSize :: (WebSettingsClass self) => Attr self Int
webSettingsDefaultMonospaceFontSize = newAttrFromIntProperty "default-monospace-font-size"

-- | This settings controls various editing behaviors
webSettingsEditingBehavior :: (WebSettingsClass self) => Attr self EditingBehavior
webSettingsEditingBehavior = newAttrFromEnumProperty "editing-behavior"
        {#call pure unsafe webkit_editing_behavior_get_type#}
-- | Whether to enable caret browsing mode.
webSettingsEnableCaretBrowsing :: (WebSettingsClass self) => Attr self Bool
webSettingsEnableCaretBrowsing = newAttrFromBoolProperty "enable-caret-browsing"

-- | Whether developer extensions should be enabled.
--
-- This enables, for now, the 'WebInspector'
webSettingsEnableDeveloperExtras :: (WebSettingsClass self) => Attr self Bool
webSettingsEnableDeveloperExtras = newAttrFromBoolProperty "enable-developer-extras"

-- | Whether to enable HTML5 client-side SQL database support.
webSettingsEnableHtml5Database :: (WebSettingsClass self) => Attr self Bool
webSettingsEnableHtml5Database = newAttrFromBoolProperty "enable-html5-database"

-- | Whether to enable HTML5 localStorage support.
webSettingsEnableHtml5LocalStorage :: (WebSettingsClass self) => Attr self Bool
webSettingsEnableHtml5LocalStorage = newAttrFromBoolProperty "enable-html5-local-storage"

-- | Whether to enable HTML5 offline web application cache support.
webSettingsEnableOfflineWebApplicationCache :: (WebSettingsClass self) => Attr self Bool
webSettingsEnableOfflineWebApplicationCache = newAttrFromBoolProperty "enable-offline-web-application-cache"

-- | Enable embedded plugin objects.
webSettingsEnablePlugins :: (WebSettingsClass self) => Attr self Bool
webSettingsEnablePlugins = newAttrFromBoolProperty "enable-plugins"

-- | Whether to enable private browsing mode.
webSettingsEnablePrivateBrowsing :: (WebSettingsClass self) => Attr self Bool
webSettingsEnablePrivateBrowsing = newAttrFromBoolProperty "enable-private-browsing"

-- | Enable embedded scripting languages
webSettingsEnableScripts :: (WebSettingsClass self) => Attr self Bool
webSettingsEnableScripts = newAttrFromBoolProperty "enable-scripts"

-- | Whether to enable speel checking while typing.
webSettingsEnableSpellChecking :: (WebSettingsClass self) => Attr self Bool
webSettingsEnableSpellChecking = newAttrFromBoolProperty "enable-spell-checking"

-- | Whether to allow files loaded through file:// URLs universal access to all pages.
webSettingsEnableUniversalAccessFromFileUris :: (WebSettingsClass self) => Attr self Bool
webSettingsEnableUniversalAccessFromFileUris = newAttrFromBoolProperty "enable-universal-access-from-file-uris"

-- | Whether to enable the XSS Auditor.
--
-- This feature filters some kinds of reflective XSS attacks on vulnerable web sites.
webSettingsEnableXssAuditor :: (WebSettingsClass self) => Attr self Bool
webSettingsEnableXssAuditor = newAttrFromBoolProperty "enable-xss-auditor"

-- | Enforce a resolution of 96 DPI.
webSettingsEnforce96Dpi :: (WebSettingsClass self) => Attr self Bool
webSettingsEnforce96Dpi = newAttrFromBoolProperty "enforce-96-dpi"

-- | The default Fantasy font family used to display text
webSettingsFantasyFontFamily :: (WebSettingsClass self) => Attr self Bool
webSettingsFantasyFontFamily = newAttrFromBoolProperty "fantasy-font-family"

-- | Whether JavaScript can open popup windows automatically without user intervention.
webSettingsJSCanOpenWindowAuto :: (WebSettingsClass self) => Attr self Bool
webSettingsJSCanOpenWindowAuto = newAttrFromBoolProperty "javascript-can-open-windows-automatically"

-- | The minimum font size used to display text.
-- 
-- Allowed values: >=1
--
-- Default value: 5
webSettingsMinimumFontSize :: (WebSettingsClass self) => Attr self Int
webSettingsMinimumFontSize = newAttrFromIntProperty "minimum-font-size"

-- | The minimum logical font size used to display text
--
-- Allowed values: >=1
--
-- Default value: 5
webSettingsMinimumLogicalFontSize :: (WebSettingsClass self) => Attr self Int
webSettingsMinimumLogicalFontSize = newAttrFromIntProperty "minimum-logical-font-size"


-- | The default font family used to display monospace text.
-- 
-- Default value: "monospace"
webSettingsMonospaceFontFamily :: (WebSettingsClass self) => Attr self String
webSettingsMonospaceFontFamily = newAttrFromStringProperty "monospace-font-family"

-- | Whether background images should be printed
--
-- Default value: True
webSettingsPrintBackgrounds :: (WebSettingsClass self) => Attr self Bool
webSettingsPrintBackgrounds = newAttrFromBoolProperty "print-backgrounds"

-- | Whether text areas are resizable
--
-- Default value : True
webSettingsResizableTextAreas :: (WebSettingsClass self) => Attr self Bool
webSettingsResizableTextAreas =  newAttrFromBoolProperty "resizable-text-areas"

-- | The default Sans Serif font family used to display text
-- 
-- Default value "sans-serif"
webSettingsSansFontFamily :: (WebSettingsClass self) => Attr self String
webSettingsSansFontFamily = newAttrFromStringProperty "sans-serif-font-family"


-- | The default Serif font family used to display text
--
-- Default value: "serif"
webSettingsSerifFontFamily :: (WebSettingsClass self) => Attr self String
webSettingsSerifFontFamily = newAttrFromStringProperty "serif-font-family"


-- | The languages to be used for spell checking, separated by commas
-- 
-- The locale string typically is in the form lang_COUNTRY,
-- where lang is an ISO-639 language code, and COUNTRY is an ISO-3166 country code. 
-- For instance, sv_FI for Swedish as written in Finland or pt_BR for Portuguese as written in Brazil.
--
-- If no value is specified then the value returned by gtk_get_default_language will be used.
--
-- Default value: @Nothing@
webSettingsSpellCheckingLang :: (WebSettingsClass self) => Attr self (Maybe String)
webSettingsSpellCheckingLang = newAttrFromMaybeStringProperty "spell-checking-languages"

-- | The User-Agent string used by WebKit
-- 
-- This will return a default User-Agent string if a custom string wasn't provided by the application. 
-- Setting this property to a NULL value or an empty string will result in 
-- the User-Agent string being reset to the default value.
--
-- Default value: \"Mozilla/5.0 (X11; U; Linux x86_64; c) AppleWebKit/531.2+ (KHTML, like Gecko) Safari/531.2+\"

webSettingsUserAgent :: (WebSettingsClass self) => Attr self String
webSettingsUserAgent = newAttrFromStringProperty "user-agent"

-- | The URI of a stylesheet that is applied to every page.
--
-- Default value: @Nothing@

webSettingsUserStylesheetUri :: (WebSettingsClass self) => Attr self (Maybe String)
webSettingsUserStylesheetUri = newAttrFromMaybeStringProperty "user-stylesheet-uri"

-- | The value by which the zoom level is changed when zooming in or out
--
-- Allowed values: >= 0
--
-- Default value: 0.1
webSettingsZoomStep :: (WebSettingsClass self) => Attr self Float
webSettingsZoomStep = newAttrFromFloatProperty "zoom-step"
                             
-- | Enables the site-specific compatibility workarounds.
--
-- Default value: False
webSettingsEnableSiteSpecificQuirks :: WebSettingsClass self => Attr self Bool
webSettingsEnableSiteSpecificQuirks = newAttrFromBoolProperty "enable-site-specific-quirks"

