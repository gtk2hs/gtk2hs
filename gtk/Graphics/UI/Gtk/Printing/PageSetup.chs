{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget PageSetup
--
--  Author : Andy Stewart
--
--  Created: 28 Mar 2010
--
--  Copyright (C) 2010 Andy Stewart
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
-- Stores page setup information
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.Printing.PageSetup (

-- * Detail
--
-- | A 'PageSetup' object stores the page size, orientation and margins. The
-- idea is that you can get one of these from the page setup dialog and then
-- pass it to the 'PrintOperation' when printing. The benefit of splitting this
-- out of the 'PrintSettings' is that these affect the actual layout of the
-- page, and thus need to be set long before user prints.
--
-- The margins specified in this object are the \"print margins\", i.e. the
-- parts of the page that the printer cannot print on. These are different from
-- the layout margins that a word processor uses; they are typically used to
-- determine the /minimal/ size for the layout margins.
--
-- To obtain a 'PageSetup' use 'pageSetupNew' to get the defaults, or use
-- 'printRunPageSetupDialog' to show the page setup dialog and receive the
-- resulting page setup.
--
-- Printing support was added in Gtk+ 2.10.
--

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----PageSetup
-- @

#if GTK_CHECK_VERSION(2,10,0)
-- * Types
  PageSetup,
  PageSetupClass,
  castToPageSetup,
  toPageSetup,

-- * Constructors
  pageSetupNew,
#if GTK_CHECK_VERSION(2,12,0)
  pageSetupNewFromFile,
#endif

-- * Methods
  pageSetupCopy,
  pageSetupGetTopMargin,
  pageSetupSetTopMargin,
  pageSetupGetBottomMargin,
  pageSetupSetBottomMargin,
  pageSetupGetLeftMargin,
  pageSetupSetLeftMargin,
  pageSetupGetRightMargin,
  pageSetupSetRightMargin,
  pageSetupSetPaperSizeAndDefaultMargins,
  pageSetupGetPaperWidth,
  pageSetupGetPaperHeight,
  pageSetupGetPageWidth,
  pageSetupGetPageHeight,
#if GTK_CHECK_VERSION(2,14,0)
  pageSetupLoadFile,
#endif
#if GTK_CHECK_VERSION(2,12,0)
  pageSetupToFile,
#endif

-- * Attributes
  pageSetupOrientation,
  pageSetupPaperSize,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.GError
import System.Glib.Attributes
import System.Glib.UTFString
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Printing.PaperSize#} (PaperSize(..), mkPaperSize, Unit(..))
{#import Graphics.UI.Gtk.Printing.PrintSettings#} (PageOrientation (..))

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,10,0)
--------------------
-- Constructors

-- | Creates a new 'PageSetup'.
--
pageSetupNew :: IO PageSetup
pageSetupNew =
  wrapNewGObject mkPageSetup $
  {# call gtk_page_setup_new #}

#if GTK_CHECK_VERSION(2,12,0)
-- | Reads the page setup from the file @fileName@. Returns a new 'PageSetup'
-- object with the restored page setup.
--
-- * Available since Gtk+ version 2.12
--
pageSetupNewFromFile :: GlibString string
 => string -- ^ @fileName@ - the filename to read the page setup from
 -> IO PageSetup
pageSetupNewFromFile fileName =
  propagateGError $ \errorPtr ->
  withUTFString fileName $ \fileNamePtr -> do
  setupPtr <- {# call gtk_page_setup_new_from_file #}
             fileNamePtr
             errorPtr
  wrapNewGObject mkPageSetup (return setupPtr)

#endif

--------------------
-- Methods

-- | Copies a 'PageSetup'.
--
pageSetupCopy :: PageSetupClass self => self
 -> IO PageSetup -- ^ returns a copy of @other@
pageSetupCopy self =
  wrapNewGObject mkPageSetup $
  {# call gtk_page_setup_copy #}
    (toPageSetup self)

-- | Gets the page orientation of the 'PageSetup'.
pageSetupGetOrientation :: PageSetupClass self => self
 -> IO PageOrientation -- ^ returns the page orientation
pageSetupGetOrientation self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_page_setup_get_orientation #}
    (toPageSetup self)

-- | Sets the page orientation of the 'PageSetup'.
pageSetupSetOrientation :: PageSetupClass self => self
 -> PageOrientation -- ^ @orientation@ - a 'PageOrientation' value
 -> IO ()
pageSetupSetOrientation self orientation =
  {# call gtk_page_setup_set_orientation #}
    (toPageSetup self)
    ((fromIntegral . fromEnum) orientation)

-- | Gets the paper size of the 'PageSetup'.
pageSetupGetPaperSize :: PageSetupClass self => self
 -> IO PaperSize -- ^ returns the paper size
pageSetupGetPaperSize self =
  {# call gtk_page_setup_get_paper_size #}
    (toPageSetup self)
  >>= mkPaperSize . castPtr

pageSetupSetPaperSize :: PageSetupClass self => self
 -> PaperSize -- ^ @size@ - a 'PaperSize'
 -> IO ()
pageSetupSetPaperSize self size =
  {# call gtk_page_setup_set_paper_size #}
    (toPageSetup self)
    size

-- | Gets the top margin in units of @unit@.
--
pageSetupGetTopMargin :: PageSetupClass self => self
 -> Unit      -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the top margin
pageSetupGetTopMargin self unit =
  liftM realToFrac $
  {# call gtk_page_setup_get_top_margin #}
    (toPageSetup self)
    ((fromIntegral . fromEnum) unit)

-- | Sets the top margin of the 'PageSetup'.
--
pageSetupSetTopMargin :: PageSetupClass self => self
 -> Double -- ^ @margin@ - the new top margin in units of @unit@
 -> Unit   -- ^ @unit@ - the units for @margin@
 -> IO ()
pageSetupSetTopMargin self margin unit =
  {# call gtk_page_setup_set_top_margin #}
    (toPageSetup self)
    (realToFrac margin)
    ((fromIntegral . fromEnum) unit)

-- | Gets the bottom margin in units of @unit@.
--
pageSetupGetBottomMargin :: PageSetupClass self => self
 -> Unit      -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the bottom margin
pageSetupGetBottomMargin self unit =
  liftM realToFrac $
  {# call gtk_page_setup_get_bottom_margin #}
    (toPageSetup self)
    ((fromIntegral . fromEnum) unit)

-- | Sets the bottom margin of the 'PageSetup'.
--
pageSetupSetBottomMargin :: PageSetupClass self => self
 -> Double -- ^ @margin@ - the new bottom margin in units of @unit@
 -> Unit   -- ^ @unit@ - the units for @margin@
 -> IO ()
pageSetupSetBottomMargin self margin unit =
  {# call gtk_page_setup_set_bottom_margin #}
    (toPageSetup self)
    (realToFrac margin)
    ((fromIntegral . fromEnum) unit)

-- | Gets the left margin in units of @unit@.
--
pageSetupGetLeftMargin :: PageSetupClass self => self
 -> Unit      -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the left margin
pageSetupGetLeftMargin self unit =
  liftM realToFrac $
  {# call gtk_page_setup_get_left_margin #}
    (toPageSetup self)
    ((fromIntegral . fromEnum) unit)

-- | Sets the left margin of the 'PageSetup'.
--
pageSetupSetLeftMargin :: PageSetupClass self => self
 -> Double -- ^ @margin@ - the new left margin in units of @unit@
 -> Unit   -- ^ @unit@ - the units for @margin@
 -> IO ()
pageSetupSetLeftMargin self margin unit =
  {# call gtk_page_setup_set_left_margin #}
    (toPageSetup self)
    (realToFrac margin)
    ((fromIntegral . fromEnum) unit)

-- | Gets the right margin in units of @unit@.
--
pageSetupGetRightMargin :: PageSetupClass self => self
 -> Unit      -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the right margin
pageSetupGetRightMargin self unit =
  liftM realToFrac $
  {# call gtk_page_setup_get_right_margin #}
    (toPageSetup self)
    ((fromIntegral . fromEnum) unit)

-- | Sets the right margin of the 'PageSetup'.
--
pageSetupSetRightMargin :: PageSetupClass self => self
 -> Double -- ^ @margin@ - the new right margin in units of @unit@
 -> Unit   -- ^ @unit@ - the units for @margin@
 -> IO ()
pageSetupSetRightMargin self margin unit =
  {# call gtk_page_setup_set_right_margin #}
    (toPageSetup self)
    (realToFrac margin)
    ((fromIntegral . fromEnum) unit)

-- | Sets the paper size of the 'PageSetup' and modifies the margins according
-- to the new paper size.
--
pageSetupSetPaperSizeAndDefaultMargins :: PageSetupClass self => self
 -> PaperSize -- ^ @size@ - a 'PaperSize'
 -> IO ()
pageSetupSetPaperSizeAndDefaultMargins self size =
  {# call gtk_page_setup_set_paper_size_and_default_margins #}
    (toPageSetup self)
    size

-- | Returns the paper width in units of @unit@.
--
-- Note that this function takes orientation, but not margins into
-- consideration. See 'pageSetupGetPageWidth'.
--
pageSetupGetPaperWidth :: PageSetupClass self => self
 -> Unit      -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the paper width.
pageSetupGetPaperWidth self unit =
  liftM realToFrac $
  {# call gtk_page_setup_get_paper_width #}
    (toPageSetup self)
    ((fromIntegral . fromEnum) unit)

-- | Returns the paper height in units of @unit@.
--
-- Note that this function takes orientation, but not margins into
-- consideration. See 'pageSetupGetPageHeight'.
--
pageSetupGetPaperHeight :: PageSetupClass self => self
 -> Unit      -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the paper height.
pageSetupGetPaperHeight self unit =
  liftM realToFrac $
  {# call gtk_page_setup_get_paper_height #}
    (toPageSetup self)
    ((fromIntegral . fromEnum) unit)

-- | Returns the page width in units of @unit@.
--
-- Note that this function takes orientation and margins into consideration.
-- See 'pageSetupGetPaperWidth'.
--
pageSetupGetPageWidth :: PageSetupClass self => self
 -> Unit      -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the page width.
pageSetupGetPageWidth self unit =
  liftM realToFrac $
  {# call gtk_page_setup_get_page_width #}
    (toPageSetup self)
    ((fromIntegral . fromEnum) unit)

-- | Returns the page height in units of @unit@.
--
-- Note that this function takes orientation and margins into consideration.
-- See 'pageSetupGetPaperHeight'.
--
pageSetupGetPageHeight :: PageSetupClass self => self
 -> Unit      -- ^ @unit@ - the unit for the return value
 -> IO Double -- ^ returns the page height.
pageSetupGetPageHeight self unit =
  liftM realToFrac $
  {# call gtk_page_setup_get_page_height #}
    (toPageSetup self)
    ((fromIntegral . fromEnum) unit)

#if GTK_CHECK_VERSION(2,14,0)
-- | Reads the page setup from the file @fileName@. See 'pageSetupToFile'.
--
-- * Available since Gtk+ version 2.14
--
pageSetupLoadFile :: (PageSetupClass self, GlibString string) => self
 -> string  -- ^ @fileName@ - the filename to read the page setup from
 -> IO Bool -- ^ returns @True@ on success
pageSetupLoadFile self fileName =
  liftM toBool $
  propagateGError $ \errorPtr ->
  withUTFString fileName $ \fileNamePtr ->
  {# call gtk_page_setup_load_file #}
    (toPageSetup self)
    fileNamePtr
    errorPtr

#endif

#if GTK_CHECK_VERSION(2,12,0)
-- | This function saves the information from @setup@ to @fileName@.
--
-- * Available since Gtk+ version 2.12
--
pageSetupToFile :: (PageSetupClass self, GlibString string) => self
 -> string  -- ^ @fileName@ - the file to save to
 -> IO Bool -- ^ returns @True@ on success
pageSetupToFile self fileName =
  liftM toBool $
  propagateGError $ \errorPtr ->
  withUTFString fileName $ \fileNamePtr ->
  {# call gtk_page_setup_to_file #}
    (toPageSetup self)
    fileNamePtr
    errorPtr

#endif
-- | The page orientation of the 'PageSetup'.
pageSetupOrientation :: PageSetupClass self => Attr self PageOrientation
pageSetupOrientation = newAttr
  pageSetupGetOrientation
  pageSetupSetOrientation

-- | The paper size of the 'PageSetup'.
pageSetupPaperSize :: PageSetupClass self => Attr self PaperSize
pageSetupPaperSize = newAttr
  pageSetupGetPaperSize
  pageSetupSetPaperSize

#endif

