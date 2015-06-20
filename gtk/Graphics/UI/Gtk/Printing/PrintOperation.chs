{-# LANGUAGE CPP #-}
-- -*-haskell-*-
--  GIMP Toolkit (GTK) Widget PrintOperation
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
-- High-level Printing API
--
-- * Module available since Gtk+ version 2.10
--
module Graphics.UI.Gtk.Printing.PrintOperation (

-- * Detail
--
-- | 'PrintOperation' is the high-level, portable printing API. It looks a bit
-- different than other Gtk+ dialogs such as the 'FileChooser', since some
-- platforms don't expose enough infrastructure to implement a good print
-- dialog. On such platforms, 'PrintOperation' uses the native print dialog. On
-- platforms which do not provide a native print dialog, Gtk+ uses its own, see
-- 'PrintUnixDialog'.
--
-- The typical way to use the high-level printing API is to create a
-- 'PrintOperation' object with 'printOperationNew' when the user selects to
-- print. Then you set some properties on it, e.g. the page size, any
-- 'PrintSettings' from previous print operations, the number of pages, the
-- current page, etc.
--
-- Then you start the print operation by calling 'printOperationRun'. It
-- will then show a dialog, let the user select a printer and options. When the
-- user finished the dialog various signals will be emitted on the
-- 'PrintOperation', the main one being 'draw-page' signal, which you are supposed to
-- catch and render the page on the provided 'PrintContext' using Cairo.
--
-- By default 'PrintOperation' uses an external application to do print
-- preview. To implement a custom print preview, an application must connect to
-- the preview signal. The functions 'printOperationPrintPreviewRenderPage',
-- 'printOperationPreviewEndPreview' and 'printOperationPreviewIsSelected' are
-- useful when implementing a print preview.
--
-- Printing support was added in Gtk+ 2.10.
--

-- * Class Hierarchy
--
-- |
-- @
-- |  'GObject'
-- |   +----PrintOperation
-- @

#if GTK_CHECK_VERSION(2,10,0)
-- * Types
  PrintOperation,
  PrintOperationClass,
  castToPrintOperation,
  toPrintOperation,

  PrintOperationPreview,
  PrintOperationPreviewClass,
  castToPrintOperationPreview,
  toPrintOperationPreview,

-- * Enums
  PrintStatus(..),
  PrintOperationAction(..),
  PrintOperationResult(..),
  PrintError(..),

-- * Constructors
  printOperationNew,

-- * Methods
  printOperationSetAllowAsync,
  printOperationGetError,
  printOperationSetJobName,
  printOperationSetNPages,
#if GTK_CHECK_VERSION(2,18,0)
  printOperationGetNPagesToPrint,
#endif
  printOperationSetCurrentPage,
  printOperationSetUseFullPage,
  printOperationSetUnit,
  printOperationSetExportFilename,
  printOperationSetShowProgress,
  printOperationSetTrackPrintStatus,
  printOperationSetCustomTabLabel,
  printOperationRun,
  printOperationCancel,
#if GTK_CHECK_VERSION(2,16,0)
  printOperationDrawPageFinish,
  printOperationSetDeferDrawing,
#endif
  printOperationGetStatus,
  printOperationGetStatusString,
  printOperationIsFinished,
  printRunPageSetupDialog,
  printRunPageSetupDialogAsync,

  printOperationPreviewEndPreview,
  printOperationPreviewIsSelected,
  printOperationPreviewRenderPage,

-- * Attributes
  printOperationDefaultPageSetup,
  printOperationPrintSettings,
  printOperationJobName,
  printOperationNPages,
  printOperationCurrentPage,
  printOperationUseFullPage,
  printOperationTrackPrintStatus,
  printOperationUnit,
  printOperationShowProgress,
  printOperationAllowAsync,
  printOperationExportFilename,
  printOperationStatus,
  printOperationStatusString,
  printOperationCustomTabLabel,
#if GTK_CHECK_VERSION(2,18,0)
  printOperationSupportSelection,
  printOperationHasSelection,
  printOperationEmbedPageSetup,
  printOperationNPagesToPrint,
#endif

-- * Signals
  printOptDone,
  printOptBeginPrint,
  printOptPaginate,
  printOptRequestPageSetup,
  printOptDrawPage,
  printOptEndPrint,
  printOptStatusChanged,
  printOptCreateCustomWidget,
#if GTK_CHECK_VERSION(2,18,0)
  printOptUpdateCustomWidget,
#endif
  printOptCustomWidgetApply,
  printOptPreview,
  printOptReady,
  printOptGotPageSize,
#endif
  ) where

import Control.Monad    (liftM)

import System.Glib.FFI
import System.Glib.Attributes
import System.Glib.Properties
import System.Glib.UTFString
import System.Glib.GError
{#import Graphics.UI.Gtk.Types#}
{#import Graphics.UI.Gtk.Signals#}
import Graphics.UI.Gtk.Printing.PaperSize (Unit(..))

{# context lib="gtk" prefix="gtk" #}

#if GTK_CHECK_VERSION(2,10,0)
--------------------
-- Interfaces

instance PrintOperationPreviewClass PrintOperation

--------------------
-- Enums
-- | The status gives a rough indication of the completion of a running print operation.
{#enum PrintStatus {underscoreToCase} deriving (Bounded,Eq,Show)#}

-- | The action parameter to 'printOperationRun' determines what action the print operation should
-- perform.
{#enum PrintOperationAction {underscoreToCase} deriving (Bounded,Eq,Show)#}

-- | A value of this type is returned by 'printOperationRun'.
{#enum PrintOperationResult {underscoreToCase} deriving (Bounded,Eq,Show)#}

-- | Error codes that identify various errors that can occur while using the GTK+ printing support.
{#enum PrintError {underscoreToCase} deriving (Bounded,Eq,Show)#}

--------------------
-- Constructors

-- | Creates a new 'PrintOperation'.
--
printOperationNew :: IO PrintOperation
printOperationNew =
  wrapNewGObject mkPrintOperation $
  {# call gtk_print_operation_new #}

--------------------
-- Methods

-- | Sets whether the 'printOperationRun' may return before the print
-- operation is completed. Note that some platforms may not allow asynchronous
-- operation.
--
printOperationSetAllowAsync :: PrintOperationClass self => self
 -> Bool -- ^ @allowAsync@ - @True@ to allow asynchronous operation
 -> IO ()
printOperationSetAllowAsync self allowAsync =
  {# call gtk_print_operation_set_allow_async #}
    (toPrintOperation self)
    (fromBool allowAsync)

-- | Call this when the result of a print operation is
-- 'PrintOperationResultError', either as returned by 'printOperationRun', or
-- in the 'done' signal handler. The returned
-- 'GError' will contain more details on what went wrong.
--
printOperationGetError :: PrintOperationClass self => self -> IO ()
printOperationGetError self =
  propagateGError $ \errorPtr ->
  {# call gtk_print_operation_get_error #}
    (toPrintOperation self)
    errorPtr

-- | Sets the name of the print job. The name is used to identify the job
-- (e.g. in monitoring applications like eggcups).
--
-- If you don't set a job name, Gtk+ picks a default one by numbering
-- successive print jobs.
--
printOperationSetJobName :: (PrintOperationClass self, GlibString string) => self
 -> string -- ^ @jobName@ - a string that identifies the print job
 -> IO ()
printOperationSetJobName self jobName =
  withUTFString jobName $ \jobNamePtr ->
  {# call gtk_print_operation_set_job_name #}
    (toPrintOperation self)
    jobNamePtr

-- | Sets the number of pages in the document.
--
-- This /must/ be set to a positive number before the rendering starts. It
-- may be set in a 'beginPrint' signal hander.
--
-- Note that the page numbers passed to the 'requestPageSetup'
-- and 'drawPage' signals
-- are 0-based, i.e. if the user chooses to print all pages, the last
-- 'draw-page' signal will be for page @nPages@ - 1.
--
printOperationSetNPages :: PrintOperationClass self => self
 -> Int -- ^ @nPages@ - the number of pages
 -> IO ()
printOperationSetNPages self nPages =
  {# call gtk_print_operation_set_n_pages #}
    (toPrintOperation self)
    (fromIntegral nPages)

#if GTK_CHECK_VERSION(2,18,0)
-- | Returns the number of pages that will be printed.
--
-- Note that this value is set during print preparation phase
-- ('PrintStatusPreparing'), so this function should never be called before the
-- data generation phase ('PrintStatusGeneratingData'). You can connect to the
-- 'statusChanged' signal and call
-- 'printOperationGetNPagesToPrint' when print status is
-- 'PrintStatusGeneratingData'. This is typically used to track the progress of
-- print operation.
--
-- * Available since Gtk+ version 2.18
--
printOperationGetNPagesToPrint :: PrintOperationClass self => self
 -> IO Int -- ^ returns the number of pages that will be printed
printOperationGetNPagesToPrint self =
  liftM fromIntegral $
  {# call gtk_print_operation_get_n_pages_to_print #}
    (toPrintOperation self)
#endif

-- | Sets the current page.
--
-- If this is called before 'printOperationRun', the user will be able to
-- select to print only the current page.
--
-- Note that this only makes sense for pre-paginated documents.
--
printOperationSetCurrentPage :: PrintOperationClass self => self
 -> Int -- ^ @currentPage@ - the current page, 0-based
 -> IO ()
printOperationSetCurrentPage self currentPage =
  {# call gtk_print_operation_set_current_page #}
    (toPrintOperation self)
    (fromIntegral currentPage)

-- | If @fullPage@ is @True@, the transformation for the cairo context
-- obtained from 'PrintContext' puts the origin at the top left corner of the
-- page (which may not be the top left corner of the sheet, depending on page
-- orientation and the number of pages per sheet). Otherwise, the origin is at
-- the top left corner of the imageable area (i.e. inside the margins).
--
printOperationSetUseFullPage :: PrintOperationClass self => self
 -> Bool -- ^ @fullPage@ - @True@ to set up the 'PrintContext' for the full
         -- page
 -> IO ()
printOperationSetUseFullPage self fullPage =
  {# call gtk_print_operation_set_use_full_page #}
    (toPrintOperation self)
    (fromBool fullPage)

-- | Sets up the transformation for the cairo context obtained from
-- 'PrintContext' in such a way that distances are measured in units of @unit@.
--
printOperationSetUnit :: PrintOperationClass self => self
 -> Unit -- ^ @unit@ - the unit to use
 -> IO ()
printOperationSetUnit self unit =
  {# call gtk_print_operation_set_unit #}
    (toPrintOperation self)
    ((fromIntegral . fromEnum) unit)

-- | Sets up the 'PrintOperation' to generate a file instead of showing the
-- print dialog. The indended use of this function is for implementing \"Export
-- to PDF\" actions. Currently, PDF is the only supported format.
--
-- \"Print to PDF\" support is independent of this and is done by letting
-- the user pick the \"Print to PDF\" item from the list of printers in the
-- print dialog.
--
printOperationSetExportFilename :: (PrintOperationClass self, GlibString string) => self
 -> string -- ^ @filename@ - the filename for the exported file
 -> IO ()
printOperationSetExportFilename self filename =
  withUTFString filename $ \filenamePtr ->
  {# call gtk_print_operation_set_export_filename #}
    (toPrintOperation self)
    filenamePtr

-- | If @showProgress@ is @True@, the print operation will show a progress
-- dialog during the print operation.
--
printOperationSetShowProgress :: PrintOperationClass self => self
 -> Bool -- ^ @showProgress@ - @True@ to show a progress dialog
 -> IO ()
printOperationSetShowProgress self showProgress =
  {# call gtk_print_operation_set_show_progress #}
    (toPrintOperation self)
    (fromBool showProgress)

-- | If track_status is @True@, the print operation will try to continue
-- report on the status of the print job in the printer queues and printer.
-- This can allow your application to show things like \"out of paper\" issues,
-- and when the print job actually reaches the printer.
--
-- This function is often implemented using some form of polling, so it
-- should not be enabled unless needed.
--
printOperationSetTrackPrintStatus :: PrintOperationClass self => self
 -> Bool -- ^ @trackStatus@ - @True@ to track status after printing
 -> IO ()
printOperationSetTrackPrintStatus self trackStatus =
  {# call gtk_print_operation_set_track_print_status #}
    (toPrintOperation self)
    (fromBool trackStatus)

-- | Sets the label for the tab holding custom widgets.
--
printOperationSetCustomTabLabel :: (PrintOperationClass self, GlibString string) => self
 -> string -- ^ @label@ - the label to use, or empty to use the default
           -- label
 -> IO ()
printOperationSetCustomTabLabel self label =
  withUTFString label $ \labelPtr ->
  {# call gtk_print_operation_set_custom_tab_label #}
    (toPrintOperation self)
    labelPtr

-- | Runs the print operation, by first letting the user modify print settings
-- in the print dialog, and then print the document.
--
-- Normally that this function does not return until the rendering of all
-- pages is complete. You can connect to the 'statusChanged' signal on @op@ to obtain some information about the
-- progress of the print operation. Furthermore, it may use a recursive
-- mainloop to show the print dialog.
--
-- If you call 'printOperationSetAllowAsync' or set the 'allowAsync'
-- property the operation will run asynchronously
-- if this is supported on the platform. The 'done' signal will be emitted with the result of the operation when
-- the it is done (i.e. when the dialog is canceled, or when the print succeeds
-- or fails).
--
printOperationRun :: (PrintOperationClass self, WindowClass parent) => self
 -> PrintOperationAction    -- ^ @action@ - the action to start
 -> parent                  -- ^ @parent@ - Transient parent of the dialog
 -> IO PrintOperationResult -- ^ returns the result of the print operation. A
                            -- return value of 'PrintOperationResultApply'
                            -- indicates that the printing was completed
                            -- successfully. In this case, it is a good idea to
                            -- obtain the used print settings with
                            -- 'printOperationGetPrintSettings' and store them
                            -- for reuse with the next print operation. A value
                            -- of 'PrintOperationResultInProgress' means the
                            -- operation is running asynchronously, and will
                            -- emit the 'done' signal when done.
printOperationRun self action parent =
  liftM (toEnum . fromIntegral) $
  propagateGError $ \errorPtr ->
  {# call gtk_print_operation_run #}
    (toPrintOperation self)
    ((fromIntegral . fromEnum) action)
    (toWindow parent)
    errorPtr

-- | Cancels a running print operation. This function may be called from a
-- 'beginPrint', 'paginate' or 'drawPage' signal handler
-- to stop the currently running print operation.
--
printOperationCancel :: PrintOperationClass self => self -> IO ()
printOperationCancel self =
  {# call gtk_print_operation_cancel #}
    (toPrintOperation self)

#if GTK_CHECK_VERSION(2,16,0)
-- | Signalize that drawing of particular page is complete.
--
-- It is called after completion of page drawing (e.g. drawing in another
-- thread). If 'printOperationSetDeferDrawing' was called before, then this
-- function has to be called by application. In another case it is called by
-- the library itself.
--
-- * Available since Gtk+ version 2.16
--
printOperationDrawPageFinish :: PrintOperationClass self => self -> IO ()
printOperationDrawPageFinish self =
  {# call gtk_print_operation_draw_page_finish #}
    (toPrintOperation self)

-- | Sets up the 'PrintOperation' to wait for calling of
-- 'printOperationDrawPageFinish' from application. It can be used for drawing
-- page in another thread.
--
-- This function must be called in the callback of \"draw-page\" signal.
--
-- * Available since Gtk+ version 2.16
--
printOperationSetDeferDrawing :: PrintOperationClass self => self -> IO ()
printOperationSetDeferDrawing self =
  {# call gtk_print_operation_set_defer_drawing #}
    (toPrintOperation self)
#endif

-- | Returns the status of the print operation. Also see
-- 'printOperationGetStatusString'.
--
printOperationGetStatus :: PrintOperationClass self => self
 -> IO PrintStatus -- ^ returns the status of the print operation
printOperationGetStatus self =
  liftM (toEnum . fromIntegral) $
  {# call gtk_print_operation_get_status #}
    (toPrintOperation self)

-- | Returns a string representation of the status of the print operation. The
-- string is translated and suitable for displaying the print status e.g. in a
-- 'Statusbar'.
--
-- Use 'printOperationGetStatus' to obtain a status value that is suitable
-- for programmatic use.
--
printOperationGetStatusString :: (PrintOperationClass self, GlibString string) => self
 -> IO string -- ^ returns a string representation of the status of the print
              -- operation
printOperationGetStatusString self =
  {# call gtk_print_operation_get_status_string #}
    (toPrintOperation self)
  >>= peekUTFString

-- | A convenience function to find out if the print operation is finished,
-- either successfully ('PrintStatusFinished') or unsuccessfully
-- ('PrintStatusFinishedAborted').
--
-- Note: when you enable print status tracking the print operation can be in
-- a non-finished state even after done has been called, as the operation
-- status then tracks the print job status on the printer.
--
printOperationIsFinished :: PrintOperationClass self => self
 -> IO Bool -- ^ returns @True@, if the print operation is finished.
printOperationIsFinished self =
  liftM toBool $
  {# call gtk_print_operation_is_finished #}
    (toPrintOperation self)

-- | Runs a page setup dialog, letting the user modify the values from @pageSetup@. If the user cancels
-- the dialog, the returned 'PageSetup' is identical to the passed in @pageSetup@, otherwise it
-- contains the modifications done in the dialog.
--
-- Note that this function may use a recursive mainloop to show the page setup dialog. See
-- 'printRunPageSetupDialogAsync' if this is a problem.
printRunPageSetupDialog :: (WindowClass window, PageSetupClass pageSetup, PrintSettingsClass setting)
                          => window -- ^ @parent@     transient parent.
                          -> pageSetup -- ^ @pageSetup@ an existing 'PageSetup'.
                          -> setting -- ^ @settings@   a 'PrintSettings'
                          -> IO PageSetup  -- ^ returns    a new 'PageSetup'
printRunPageSetupDialog window pageSetup setting =
  wrapNewGObject mkPageSetup $
  {#call gtk_print_run_page_setup_dialog #}
     (toWindow window)
     (toPageSetup pageSetup)
     (toPrintSettings setting)

{#pointer PageSetupDoneFunc#}

foreign import ccall "wrapper" mkGtkPageSetupDoneFunc ::
  (Ptr PageSetup -> Ptr () -> IO ())
  -> IO PageSetupDoneFunc

-- | Runs a page setup dialog, letting the user modify the values from @pageSetup@.
--
-- In contrast to 'printRunPageSetupDialog', this function returns after showing the page setup
-- dialog on platforms that support this, and calls @doneCb@ from a signal handler for the 'response'
-- signal of the dialog.
printRunPageSetupDialogAsync :: (WindowClass window, PageSetupClass pageSetup, PrintSettingsClass setting)
                               => window -- ^ @parent@     transient parent.
                               -> pageSetup -- ^ @pageSetup@ an existing 'PageSetup'.
                               -> setting -- ^ @settings@   a 'PrintSettings'
                               -> (PageSetup -> IO ()) -- ^ @doneCb@    a function to call when the user saves the modified page setup
                               -> IO ()
printRunPageSetupDialogAsync window pageSetup setting doneCb = do
  funcPtr <- mkGtkPageSetupDoneFunc $ \setupPtr _ -> do
              setup <- makeNewGObject mkPageSetup (return setupPtr)
              doneCb setup
  {#call gtk_print_run_page_setup_dialog_async #}
     (toWindow window)
     (toPageSetup pageSetup)
     (toPrintSettings setting)
     funcPtr
     nullPtr

-- | Ends a preview.
--
-- This function must be called to finish a custom print preview.
printOperationPreviewEndPreview :: PrintOperationPreviewClass self
                                  => self
                                  -> IO ()
printOperationPreviewEndPreview self =
  {# call gtk_print_operation_preview_end_preview #}
    (toPrintOperationPreview self)

-- | Returns whether the given page is included in the set of pages that have been selected for printing.
printOperationPreviewIsSelected :: PrintOperationPreviewClass self
                                  => self  -- ^ @preview@ a 'PrintOperationPreview'
                                  -> Int  -- ^ @pageNr@ a page number
                                  -> IO Bool -- ^ returns 'True' if the page has been selected for printing
printOperationPreviewIsSelected self pageNr =
  liftM toBool $
  {# call gtk_print_operation_preview_is_selected #}
    (toPrintOperationPreview self)
    (fromIntegral pageNr)

-- | Renders a page to the preview, using the print context that was passed to the "preview" handler
-- together with preview.
--
-- A custom iprint preview should use this function in its 'expose' handler to render the currently
-- selected page.
--
-- Note that this function requires a suitable cairo context to be associated with the print context.
printOperationPreviewRenderPage :: PrintOperationPreviewClass self
                                  => self  -- ^ @preview@ a 'PrintOperationPreview'
                                  -> Int  -- ^ @pageNr@ the page to render
                                  -> IO ()
printOperationPreviewRenderPage self pageNr =
  {# call gtk_print_operation_preview_render_page #}
    (toPrintOperationPreview self)
    (fromIntegral pageNr)

--------------------
-- Attributes

-- | The 'PageSetup' used by default.
--
-- This page setup will be used by 'printOperationRun', but it can be overridden on a per-page
-- basis by connecting to the 'requestPageSetup' signal.
--
-- Since 2.10
printOperationDefaultPageSetup :: (PrintOperationClass self, PageSetupClass pageSetup) => ReadWriteAttr self PageSetup pageSetup
printOperationDefaultPageSetup = newAttrFromObjectProperty "default-page-setup"
                                   {# call pure unsafe gtk_page_setup_get_type #}

-- | The 'PrintSettings' used for initializing the dialog.
--
-- Setting this property is typically used to re-establish print settings from a previous print
-- operation, see 'printOperationRun'.
--
-- Since 2.10
printOperationPrintSettings :: (PrintOperationClass self, PrintSettingsClass printSettings) => ReadWriteAttr self PrintSettings printSettings
printOperationPrintSettings = newAttrFromObjectProperty "print-settings"
                                {# call pure unsafe gtk_print_settings_get_type #}

-- | A string used to identify the job (e.g. in monitoring applications like eggcups).
--
-- If you don't set a job name, GTK+ picks a default one by numbering successive print jobs.
--
-- Default value: \"\"
--
-- Since 2.10
printOperationJobName :: (PrintOperationClass self, GlibString string) => Attr self string
printOperationJobName = newAttrFromStringProperty "job-name"

-- | The number of pages in the document.
--
-- This must be set to a positive number before the rendering starts. It may be set in a 'beginPrint'
-- signal hander.
--
-- Note that the page numbers passed to the 'requestPageSetup' and 'drawPage' signals are 0-based,
-- i.e. if the user chooses to print all pages, the last 'drawPage' signal will be for page @nPages@ -
-- 1.
--
-- Allowed values: >= 'GMaxulong'
--
-- Default value: -1
--
-- Since 2.10
printOperationNPages :: PrintOperationClass self => Attr self Int
printOperationNPages = newAttrFromIntProperty "n-pages"

-- | The current page in the document.
--
-- If this is set before 'printOperationRun', the user will be able to select to print only the
-- current page.
--
-- Note that this only makes sense for pre-paginated documents.
--
-- Allowed values: >= 'GMaxulong'
--
-- Default value: -1
--
-- Since 2.10
printOperationCurrentPage :: PrintOperationClass self => Attr self Int
printOperationCurrentPage = newAttrFromIntProperty "current-page"

-- | If 'True', the transformation for the cairo context obtained from 'PrintContext' puts the origin at
-- the top left corner of the page (which may not be the top left corner of the sheet, depending on
-- page orientation and the number of pages per sheet). Otherwise, the origin is at the top left corner
-- of the imageable area (i.e. inside the margins).
--
-- Default value: 'False'
--
-- Since 2.10
printOperationUseFullPage :: PrintOperationClass self => Attr self Bool
printOperationUseFullPage = newAttrFromBoolProperty "use-full-page"

-- | If 'True', the print operation will try to continue report on the status of the print job in the
-- printer queues and printer. This can allow your application to show things like "out of paper"
-- issues, and when the print job actually reaches the printer. However, this is often implemented
-- using polling, and should not be enabled unless needed.
--
-- Default value: 'False'
--
-- Since 2.10
printOperationTrackPrintStatus :: PrintOperationClass self => Attr self Bool
printOperationTrackPrintStatus = newAttrFromBoolProperty "track-print-status"

-- | The transformation for the cairo context obtained from 'PrintContext' is set up in such a way that
-- distances are measured in units of unit.
--
-- Default value: ''UnitPixel''
--
-- Since 2.10
--
printOperationUnit :: PrintOperationClass self => Attr self Unit
printOperationUnit = newAttrFromEnumProperty "unit"
                       {# call pure unsafe gtk_unit_get_type #}

-- | Determines whether to show a progress dialog during the print operation.
--
-- Default value: 'False'
--
-- Since 2.10
printOperationShowProgress :: PrintOperationClass self => Attr self Bool
printOperationShowProgress = newAttrFromBoolProperty "show-progress"

-- | Determines whether the print operation may run asynchronously or not.
--
-- Some systems don't support asynchronous printing, but those that do will return
-- ''PrintOperationResultInProgress'' as the status, and emit the "done" signal when the operation
-- is actually done.
--
-- The Windows port does not support asynchronous operation at all (this is unlikely to change). On
-- other platforms, all actions except for ''PrintOperationActionExport'' support asynchronous
-- operation.
--
-- Default value: 'False'
--
-- Since 2.10
printOperationAllowAsync :: PrintOperationClass self => Attr self Bool
printOperationAllowAsync = newAttrFromBoolProperty "allow-async"

-- | The name of a file to generate instead of showing the print dialog. Currently, PDF is the only
-- supported format.
--
-- The intended use of this property is for implementing "Export to PDF" actions.
--
-- "Print to PDF" support is independent of this and is done by letting the user pick the "Print to
-- PDF" item from the list of printers in the print dialog.
--
-- Default value: 'Nothing'
--
-- Since 2.10
printOperationExportFilename :: (PrintOperationClass self, GlibString string) => Attr self string
printOperationExportFilename = newAttrFromStringProperty "export-filename"

-- | The status of the print operation.
--
-- Default value: ''PrintStatusInitial''
--
-- Since 2.10
printOperationStatus :: PrintOperationClass self => ReadAttr self PrintStatus
printOperationStatus = readAttrFromEnumProperty "status"
                         {# call pure unsafe gtk_print_status_get_type #}

-- | A string representation of the status of the print operation. The string is translated and suitable
-- for displaying the print status e.g.  in a 'Statusbar'.
--
-- See the 'printOperationStatus' property for a status value that is suitable for programmatic use.
--
-- Default value: \"\"
--
-- Since 2.10
printOperationStatusString :: (PrintOperationClass self, GlibString string) => ReadAttr self string
printOperationStatusString = readAttrFromStringProperty "status-string"

-- | Used as the label of the tab containing custom widgets. Note that this property may be ignored on
-- some platforms.
--
-- If this is 'Nothing', GTK+ uses a default label.
--
-- Default value: 'Nothing'
--
-- Since 2.10
printOperationCustomTabLabel :: (PrintOperationClass self, GlibString string) => Attr self string
printOperationCustomTabLabel = newAttrFromStringProperty "custom-tab-label"

#if GTK_CHECK_VERSION(2,18,0)
-- | If 'True', the print operation will support print of selection. This allows the print dialog to show a
-- "Selection" button.
--
-- Default value: 'False'
--
-- Since 2.18
printOperationSupportSelection :: PrintOperationClass self => Attr self Bool
printOperationSupportSelection = newAttrFromBoolProperty "support-selection"

-- | Determines whether there is a selection in your application. This can allow your application to
-- print the selection. This is typically used to make a "Selection" button sensitive.
--
-- Default value: 'False'
--
-- Since 2.18
printOperationHasSelection :: PrintOperationClass self => Attr self Bool
printOperationHasSelection = newAttrFromBoolProperty "has-selection"

-- | If 'True', page size combo box and orientation combo box are embedded into page setup page.
--
-- Default value: 'False'
--
-- Since 2.18
printOperationEmbedPageSetup :: PrintOperationClass self => Attr self Bool
printOperationEmbedPageSetup = newAttrFromBoolProperty "embed-page-setup"

-- | The number of pages that will be printed.
--
-- Note that this value is set during print preparation phase (''PrintStatusPreparing''), so this
-- value should never be get before the data generation phase (''PrintStatusGeneratingData''). You
-- can connect to the 'statusChanged' signal and call 'printOperationGetNPagesToPrint' when
-- print status is ''PrintStatusGeneratingData''. This is typically used to track the progress of
-- print operation.
--
-- Allowed values: >= 'GMaxulong'
--
-- Default value: -1
--
-- Since 2.18
printOperationNPagesToPrint :: PrintOperationClass self => ReadAttr self Int
printOperationNPagesToPrint = readAttrFromIntProperty "n-pages-to-print"
#endif

--------------------
-- Signals

-- | Emitted when the print operation run has finished doing everything
-- required for printing.
--
-- @result@ gives you information about what happened during the run. If
-- @result@ is 'PrintOperationResultError' then you can call
-- 'printOperationGetError' for more information.
--
-- If you enabled print status tracking then 'printOperationIsFinished' may
-- still return @False@ after 'done' was
-- emitted.
--
printOptDone :: PrintOperationClass self => Signal self (PrintOperationResult -> IO ())
printOptDone = Signal (connect_ENUM__NONE "done")

-- | Emitted after the user has finished changing print settings in the
-- dialog, before the actual rendering starts.
--
-- A typical use for 'begin-print' is to use the parameters from the
-- 'PrintContext' and paginate the document accordingly, and then set the
-- number of pages with 'printOperationSetNPages'.
--
printOptBeginPrint :: PrintOperationClass self => Signal self (PrintContext -> IO ())
printOptBeginPrint = Signal (connect_OBJECT__NONE "begin_print")

-- | Emitted after the 'beginPrint' signal,
-- but before the actual rendering starts. It keeps getting emitted until a
-- connected signal handler returns @True@.
--
-- The 'paginate' signal is intended to be used for paginating a document in
-- small chunks, to avoid blocking the user interface for a long time. The
-- signal handler should update the number of pages using
-- 'printOperationSetNPages', and return @True@ if the document has been
-- completely paginated.
--
-- If you don't need to do pagination in chunks, you can simply do it all in
-- the 'begin-print handler', and set the number of pages from there.
--
printOptPaginate :: PrintOperationClass self => Signal self (PrintContext -> IO Bool)
printOptPaginate = Signal (connect_OBJECT__BOOL "paginate")

-- | Emitted once for every page that is printed, to give the application a
-- chance to modify the page setup. Any changes done to @setup@ will be in
-- force only for printing this page.
--
printOptRequestPageSetup :: PrintOperationClass self => Signal self (PrintContext -> Int -> PageSetup -> IO ())
printOptRequestPageSetup = Signal (connect_OBJECT_INT_OBJECT__NONE "request_page_setup")

-- | Emitted for every page that is printed. The signal handler must render
-- the @pageNr@'s page onto the cairo context obtained from @context@ using
-- 'printContextGetCairoContext'.
--
-- Use 'printOperationSetUseFullPage' and 'printOperationSetUnit' before
-- starting the print operation to set up the transformation of the cairo
-- context according to your needs.
--
printOptDrawPage :: PrintOperationClass self => Signal self (PrintContext -> Int -> IO ())
printOptDrawPage = Signal (connect_OBJECT_INT__NONE "draw_page")

-- | Emitted after all pages have been rendered. A handler for this signal can
-- clean up any resources that have been allocated in the 'beginPrint' handler.
--
printOptEndPrint :: PrintOperationClass self => Signal self (PrintContext -> IO ())
printOptEndPrint = Signal (connect_OBJECT__NONE "end_print")

-- | Emitted at between the various phases of the print operation. See
-- 'PrintStatus' for the phases that are being discriminated. Use
-- 'printOperationGetStatus' to find out the current status.
--
printOptStatusChanged :: PrintOperationClass self => Signal self (IO ())
printOptStatusChanged = Signal (connect_NONE__NONE "status_changed")

-- | Emitted when displaying the print dialog. If you return a widget in a
-- handler for this signal it will be added to a custom tab in the print
-- dialog. You typically return a container widget with multiple widgets in it.
--
-- The print dialog owns the returned widget, and its lifetime is not
-- controlled by the application. However, the widget is guaranteed to stay
-- around until the 'customWidgetApply'
-- signal is emitted on the operation. Then you can read out any information
-- you need from the widgets.
--
printOptCreateCustomWidget :: PrintOperationClass self => Signal self (IO Widget)
printOptCreateCustomWidget = Signal (connect_NONE__OBJECTPTR "create_custom_widget")

-- | Signal helper functions.
connect_NONE__OBJECTPTR ::
    GObjectClass obj => SignalName ->
    ConnectAfter -> obj ->
    (IO Widget) ->
    IO (ConnectId obj)
connect_NONE__OBJECTPTR signal after obj user =
    connectGeneric signal after obj action
        where action :: Ptr GObject -> IO (Ptr Widget)
              action _ =
                  failOnGError $ do
                    x <- user
                    return $ unsafeForeignPtrToPtr (unWidget (toWidget x))

#if GTK_CHECK_VERSION(2,18,0)
-- | Emitted after change of selected printer. The actual page setup and print
-- settings are passed to the custom widget, which can actualize itself
-- according to this change.
--
-- * Available since Gtk+ version 2.18
--
printOptUpdateCustomWidget :: PrintOperationClass self => Signal self (Widget -> PageSetup -> PrintSettings -> IO ())
printOptUpdateCustomWidget = Signal (connect_OBJECT_OBJECT_OBJECT__NONE "update_custom_widget")
#endif

-- | Emitted right before 'beginPrint' if you
-- added a custom widget in the 'createCustomWidtet' handler. When you get this signal you should read the
-- information from the custom widgets, as the widgets are not guaraneed to be
-- around at a later time.
--
printOptCustomWidgetApply :: PrintOperationClass self => Signal self (Widget -> IO ())
printOptCustomWidgetApply = Signal (connect_OBJECT__NONE "custom_widget_apply")

-- | Gets emitted when a preview is requested from the native dialog.
--
-- The default handler for this signal uses an external viewer application
-- to preview.
--
-- To implement a custom print preview, an application must return @True@
-- from its handler for this signal. In order to use the provided @context@ for
-- the preview implementation, it must be given a suitable cairo context with
-- 'printContextSetCairoContext'.
--
-- The custom preview implementation can use
-- 'printOperationPreviewIsSelected' and 'printOperationPreviewRenderPage' to
-- find pages which are selected for print and render them. The preview must be
-- finished by calling 'printOperationPreviewEndPreview' (typically in response
-- to the user clicking a close button).
--
printOptPreview :: PrintOperationClass self => Signal self (PrintOperationPreview -> PrintContext -> Window -> IO Bool)
printOptPreview = Signal (connect_OBJECT_OBJECT_OBJECT__BOOL "preview")

-- | The 'ready' signal gets emitted once per preview operation, before the first page is rendered.
--
-- A handler for this signal can be used for setup tasks.
printOptReady :: PrintOperationPreviewClass self => Signal self (PrintContext -> IO ())
printOptReady = Signal (connect_OBJECT__NONE "ready")

-- | The 'gotPageSize' signal is emitted once for each page that gets rendered to the preview.
--
-- A handler for this signal should update the context according to @pageSetup@ and set up a suitable
-- cairo context, using 'printContextSetCairoContext'.
printOptGotPageSize :: PrintOperationPreviewClass self => Signal self (PrintContext -> PageSetup -> IO ())
printOptGotPageSize = Signal (connect_OBJECT_OBJECT__NONE "got_page_size")
#endif
