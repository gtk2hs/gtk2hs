-- This module actually contains data. It is specific to the Gtk+ libs that
-- we're playing with. If we really cared enough we could split this
-- information out into seperate data files that get read in at runtime.
-- However for now it's easier just to edit the data here and recompile.

module MarshalFixup where

stripKnownPrefixes :: String -> String
stripKnownPrefixes ('A':'t':'k':remainder) = remainder
stripKnownPrefixes ('G':'t':'k':remainder) = remainder
stripKnownPrefixes ('G':'d':'k':remainder) = remainder
stripKnownPrefixes ('P':'a':'n':'g':'o':remainder) = remainder
stripKnownPrefixes ('G':'n':'o':'m':'e':remainder) = remainder
stripKnownPrefixes other = other

-- some special cases for when converting "gtk_foo_bar" to "GtkFooBar"
-- eg instead of doing gtk_hadjustment -> GtkHadjustment
-- we would prefer     gtk_hadjustment -> GtkHAdjustment
-- so list those special cases here:
fixCFunctionName :: String -> String
fixCFunctionName "hadjustment" = "HAdjustment"
fixCFunctionName "vadjustment" = "VAdjustment"
fixCFunctionName "hscale"  = "HScale"
fixCFunctionName "vscale"  = "VScale"
fixCFunctionName "hbox"    = "HBox"
fixCFunctionName "vbox"    = "VBox"
fixCFunctionName "hbutton" = "HButton"
fixCFunctionName "vbutton" = "VButton"
fixCFunctionName "hpaned"  = "HPaned"
fixCFunctionName "vpaned"  = "VPaned"
fixCFunctionName "hseparator" = "HSeparator"
fixCFunctionName "vseparator" = "VSeparator"
fixCFunctionName "hscrollbar" = "HScrollbar"
fixCFunctionName "vscrollbar" = "VScrollbar"
fixCFunctionName other = other

-- These are ones we have bound and so we can make documentation references to
-- them. Otherwise we generate FIXME messages in the docs.
knownMiscType :: String -> Bool
knownMiscType "GtkTreePath" = True
knownMiscType "GtkTreeIter" = True
knownMiscType "GdkColor"    = True
knownMiscType "GtkTextIter" = True
knownMiscType "GtkIconSet"  = True
knownMiscType _ = False

-- These are classes from which no other class derives or is ever likely to
-- derive. In this case we can use the actual type rather than the type class
-- For example: GtkAdjustment we say
-- > Adjustment -> ...
-- rather than
-- > AdjustmentClass adjustment => adjustment -> ...
leafClass :: String -> Bool
leafClass "GtkAdjustment" = True
leafClass "GdkPixbuf"     = True
leafClass "GtkIconFactory" = True
leafClass _ = False

-- This is a table of fixup information. It lists function parameters that
-- can be null and so should therefore be converted to use a Maybe type.
-- The perameters are identifed by C function name and parameter name.
--
-- Note that if you set this to True for any parameter then the docs for this
-- function will use @Nothing@ in place of NULL rather than a FIXME message. So
-- make sure all possibly-null parameters have been fixed since all NULLs in
-- the function docs are suppressed (since there is no automatic way of working
-- out which function doc NULLs correspond to which parameters).
--
maybeNullParameter :: String -> String -> Bool
maybeNullParameter "gtk_entry_completion_set_model" "model" = True
maybeNullParameter "gtk_label_new" "str" = True
maybeNullParameter _ _ = False

-- similarly for method return values/types.
maybeNullResult :: String -> Bool
maybeNullResult "gtk_entry_completion_get_entry" = True
maybeNullResult "gtk_entry_completion_get_model" = True
maybeNullResult "gtk_accel_label_get_accel_widget" = True
maybeNullResult "gtk_progress_bar_get_text" = True
maybeNullResult "gtk_bin_get_child" = True
maybeNullResult "gtk_container_get_focus_hadjustment" = True
maybeNullResult "gtk_container_get_focus_vadjustment" = True
maybeNullResult "gtk_paned_get_child1" = True
maybeNullResult "gtk_paned_get_child2" = True
maybeNullResult _ = False

-- On win32 for glib/gtk 2.6 they changed the interpretation of functions that
-- take or return system file names (as opposed to user displayable
-- representations of file names). Previously the string encoding of the file
-- name was that of the systems native 'codepage' which was usually ascii but
-- could be one of several obscure multi-byte encodings. For 2.6 they have
-- changed to always use a UTF8 encoding. However to maintain binary backwards
-- compatability they kept the old names and added new ones with a _utf8 suffix
-- for the new interpretation. However the old names are only in the binary,
-- they are not exposed through the C header files so all software building
-- against glib/gtk 2.6 on windows must use the _utf8 versions. Hence we
-- generate code uses the _utf8 version if we're building on windows and using
-- gtk version 2.6 or later. Ugh.

win32FileNameFunctions =
  ["gtk_image_new_from_file"
  ,"gdk_pixbuf_new_from_file"
  ,"gtk_icon_source_get_filename"
  ,"gtk_icon_source_set_filename"
  ,"gtk_file_chooser_get_filename"
  ,"gtk_file_chooser_set_filename"
  ,"gtk_file_chooser_select_filename"
  ,"gtk_file_chooser_unselect_filename"
  ,"gtk_file_chooser_get_filenames"
  ,"gtk_file_chooser_set_current_folder"
  ,"gtk_file_chooser_get_current_folder"
  ,"gtk_file_chooser_get_preview_filename"
  ,"gtk_file_chooser_add_shortcut_folder"
  ,"gtk_file_chooser_remove_shortcut_folder"
  ,"gtk_file_chooser_list_shortcut_folders"
  ,"gtk_file_selection_set_filename"
  ,"gtk_file_selection_get_filename"]
