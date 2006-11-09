-- This module actually contains data. It is specific to the Gtk+ libs that
-- we're playing with. If we really cared enough we could split this
-- information out into seperate data files that get read in at runtime.
-- However for now it's easier just to edit the data here and recompile.

module MarshalFixup where

import Data.Version
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

cTypeNameToHSType :: String -> String
cTypeNameToHSType ('A':'t':'k':remainder) = remainder
cTypeNameToHSType ('G':'t':'k':remainder) = remainder
cTypeNameToHSType "GdkWindow" = "DrawWindow"
cTypeNameToHSType ('G':'d':'k':remainder) = remainder
cTypeNameToHSType "PangoLayout" = "PangoLayout"
cTypeNameToHSType ('P':'a':'n':'g':'o':remainder) = remainder
cTypeNameToHSType ('G':'n':'o':'m':'e':remainder) = remainder
cTypeNameToHSType ('V':'t':'e':remainder) = remainder
cTypeNameToHSType other = other

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
fixCFunctionName "hborder"    = "HBorder"
fixCFunctionName "vborder"    = "VBorder"
fixCFunctionName "hseparator" = "HSeparator"
fixCFunctionName "vseparator" = "VSeparator"
fixCFunctionName "hscrollbar" = "HScrollbar"
fixCFunctionName "vscrollbar" = "VScrollbar"
fixCFunctionName "uri" = "URI"
fixCFunctionName "uris" = "URIs"
fixCFunctionName "xscale" = "XScale"
fixCFunctionName "yscale" = "YScale"
fixCFunctionName "xalign" = "XAlign"
fixCFunctionName "yalign" = "YAlign"
fixCFunctionName "xpad"   = "XPad"
fixCFunctionName "ypad"   = "YPad"
fixCFunctionName other = other

-- In some cases the way we work out the minimum version of the module doesn't
-- work (since the docs sometimes miss marking the version of some functions)
-- So to fix it just specify here the version of the library from which the
-- module is available.
fixModuleAvailableSince :: String -> Maybe Version
fixModuleAvailableSince "GtkComboBox"    = version [2,4]
fixModuleAvailableSince "GtkFileChooser" = version [2,4]
fixModuleAvailableSince "GtkCellView"    = version [2,6]
fixModuleAvailableSince "GtkIconView"    = version [2,6]
fixModuleAvailableSince "GtkMenuToolButton" = version [2,6]
fixModuleAvailableSince _ = Nothing

version v = Just Version { versionBranch = v, versionTags = [] }

-- Most of the time the gtk-doc documentation have titles that correspond to
-- the C name of the object. But sadly sometimes they don't so we fix up that
-- mapping here.
fixModuleDocMapping :: String -> String
fixModuleDocMapping "GtkClipboard" = "Clipboards"
fixModuleDocMapping "GtkSettings"  = "Settings"
fixModuleDocMapping "GtkStyle"     = "Styles"
fixModuleDocMapping other = other

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
leafClass = flip Set.member leafClasses

leafClasses = Set.fromList
  ["GtkAdjustment"
  ,"GdkPixbuf"
  ,"GtkImage"
  ,"GtkIconFactory"
  ,"GtkEntryCompletion"
  ,"GtkFileFilter"
  ,"GtkUIManager"
  ,"GtkActionGroup"
  ,"GtkRadioButton"
  ,"GtkEventBox"
  ,"GtkExpander"
  ,"GtkAccelGroup"
  ,"GtkTooltips"
  ,"GtkTextChildAnchor"
  ,"GdkWindow"
  ,"GdkDisplay"
  ,"GdkScreen"
  ,"GdkColormap"
  ,"GtkTreeViewColumn"
  ,"GtkStyle"
  ]

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
maybeNullParameter fun param =
  case Map.lookup fun maybeNullParameters of
    Nothing -> False
    Just [param'] -> param == param'
    Just  params  -> param `elem` params

maybeNullParameters :: Map String [String]
maybeNullParameters = Map.fromList
  [("gtk_entry_completion_set_model", ["model"])
  ,("gtk_label_new", ["str"])
  ,("gtk_about_dialog_set_license", ["license"])
  ,("gtk_about_dialog_set_logo", ["logo"])
  ,("gtk_about_dialog_set_logo_icon_name", ["logo"])
  ,("gtk_layout_new", ["hadjustment", "vadjustment"])
  ,("gtk_notebook_set_menu_label", ["menuLabel"])
  ,("gtk_scrolled_window_new", ["hadjustment", "vadjustment"])
  ,("gtk_combo_box_set_model", ["model"])
  ,("gtk_menu_set_screen", ["screen"])
  ,("gtk_menu_item_set_accel_path", ["accelPath"])
  ,("gtk_toolbar_set_drop_highlight_item", ["toolItem"])
  ,("gtk_text_buffer_new", ["table"])
  ,("gtk_text_buffer_create_mark", ["markName"])
  ,("gtk_cell_view_set_displayed_row", ["path"])
  ,("gtk_about_dialog_set_logo_icon_name", ["iconName"])
  ,("gtk_widget_modify_fg", ["color"])
  ,("gtk_widget_modify_bg", ["color"])
  ,("gtk_widget_modify_text", ["color"])
  ,("gtk_widget_modify_base", ["color"])
  ,("gtk_action_group_add_action_with_accel", ["accelerator"])
  ,("gtk_radio_tool_button_new", ["group"])
  ,("gtk_radio_tool_button_new_from_stock", ["group"])
  ,("gtk_tool_button_set_label", ["label"])
  ,("gtk_tool_button_set_icon_widget", ["iconWidget"])
  ,("gtk_tool_button_set_label_widget", ["labelWidget"])
  ,("gtk_ui_manager_add_ui", ["action"])
  ,("gtk_menu_tool_button_new", ["iconWidget", "label"])
  ,("gtk_menu_tool_button_set_menu", ["menu"])
  ,("gtk_tool_button_new", ["iconWidget", "label"])
  ,("gtk_tool_button_set_stock_id", ["stockId"])
  ,("gtk_action_new", ["tooltip", "stockId"])
  ,("gtk_toggle_action_new", ["tooltip", "stockId"])
  ,("gtk_radio_action_new", ["tooltip", "stockId"])
  ,("gtk_tree_model_iter_n_children", ["iter"])  
  ,("gtk_tree_model_iter_nth_child", ["parent"])
  ,("gtk_tree_store_insert", ["parent"])
  ,("gtk_tree_store_prepend", ["parent"])
  ,("gtk_tree_store_append", ["parent"])
  ,("gtk_list_store_move_before", ["sibling"])
  ,("gtk_list_store_move_after", ["sibling"])
  ,("gtk_tree_view_set_expander_column", ["column"])
  ,("gtk_tree_view_set_hadjustment", ["adjustment"])
  ,("gtk_tree_view_set_vadjustment", ["adjustment"])
  ]

-- similarly for method return values/types.
maybeNullResult :: String -> Bool
maybeNullResult = flip Set.member maybeNullResults

maybeNullResults :: Set String
maybeNullResults = Set.fromList
  ["gtk_entry_completion_get_entry"
  ,"gtk_entry_completion_get_model"
  ,"gtk_accel_label_get_accel_widget"
  ,"gtk_progress_bar_get_text"
  ,"gtk_bin_get_child"
  ,"gtk_container_get_focus_hadjustment"
  ,"gtk_container_get_focus_vadjustment"
  ,"gtk_paned_get_child1"
  ,"gtk_paned_get_child2"
  ,"gtk_label_get_mnemonic_widget"
  ,"gtk_notebook_get_menu_label"
  ,"gtk_notebook_get_menu_label_text"
  ,"gtk_notebook_get_nth_page"
  ,"gtk_notebook_get_tab_label"
  ,"gtk_notebook_get_tab_label_text"
  ,"gtk_combo_box_get_model"
  ,"gtk_image_menu_item_get_image"
  ,"gtk_menu_get_title"
  ,"gtk_menu_item_get_submenu"
  ,"gtk_tool_item_retrieve_proxy_menu_item"
  ,"gtk_tool_item_get_proxy_menu_item"
  ,"gtk_toolbar_get_nth_item"
  ,"gtk_file_chooser_get_filename"
  ,"gtk_file_chooser_get_current_folder"
  ,"gtk_file_chooser_get_uri"
  ,"gtk_file_chooser_get_preview_widget"
  ,"gtk_file_chooser_get_preview_filename"
  ,"gtk_file_chooser_get_preview_uri"
  ,"gtk_file_chooser_get_extra_widget"
  ,"gtk_file_chooser_get_filter"
  ,"gtk_font_selection_get_font_name"
  ,"gtk_font_selection_dialog_get_font_name"
  ,"gtk_text_mark_get_name"
  ,"gtk_text_mark_get_buffer"
  ,"gtk_text_tag_table_lookup"
  ,"gtk_text_buffer_get_mark"
  ,"gtk_text_view_get_window"
  ,"gtk_icon_view_get_path_at_pos"
  ,"gtk_combo_box_get_active_text"
  ,"gtk_scale_get_layout"
  ,"gtk_button_get_image"
  ,"gtk_image_get_animation"
  ,"gtk_window_get_transient_for"
  ,"gtk_window_get_role"
  ,"gtk_window_get_title"
  ,"gtk_widget_render_icon"
  ,"gtk_widget_get_composite_name"
  ,"gtk_action_get_accel_path"
  ,"gtk_action_group_get_action"
  ,"gtk_tool_button_get_label"
  ,"gtk_tool_button_get_icon_widget"
  ,"gtk_tool_button_get_label_widget"
  ,"gtk_ui_manager_get_widget"
  ,"gtk_ui_manager_get_action"
  ,"gtk_menu_tool_button_get_menu"
  ,"gtk_tool_button_get_stock_id"
  ,"gtk_about_dialog_get_license"
  ,"gtk_menu_get_attach_widget"
  ,"gtk_tree_view_column_get_title"
  ,"gtk_frame_get_label_widget"
  ,"gtk_tree_view_get_model"
  ,"gtk_tree_view_get_hadjustment"
  ,"gtk_tree_view_get_vadjustment"
  ]

-- Often the documentation for parameters or the return value of functions
-- that is included in the gtk-doc docs are just pointless. So this table
-- lists the function and parameter names for which we do not want to use the
-- gtk-doc documentation.
nukeParamDoc :: String -> String -> Bool
nukeParamDoc ('g':'t':'k':'_':'u':'i':'_':'m':'a':'n':'a':'g':'e':'r':'_':_) "self" = True
nukeParamDoc fun param =
  case Map.lookup fun nukeParamDocs of
    Nothing -> False
    Just [param'] -> param == param'
    Just  params  -> param `elem` params

nukeParamDocs :: Map String [String]
nukeParamDocs = Map.fromList
  [("gtk_button_box_get_layout", ["returns"])
  ,("gtk_button_set_label", ["label"])
  ,("gtk_button_get_label", ["returns"]) 
  ,("gtk_toggle_button_get_active", ["returns"])
  ,("gtk_image_new_from_file", ["filename"])
  ,("gtk_image_new_from_pixbuf", ["pixbuf"])
  ,("gtk_label_new", ["str"])
  ,("gtk_label_set_text", ["str"])
  ,("gtk_label_set_label", ["str"])
  ,("gtk_label_set_justify", ["jtype"])
  ,("gtk_label_get_justify", ["returns"])
  ,("gtk_label_set_use_underline", ["setting"])
  ,("gtk_label_get_use_underline", ["returns"])
  ,("gtk_label_get_layout", ["returns"])
  ,("gtk_label_get_text", ["returns"])
  ,("gtk_label_get_label", ["returns"])
  ,("gtk_label_set_text_with_mnemonic", ["str"])
  ,("gtk_progress_bar_set_text", ["text"])
  ,("gtk_progress_bar_get_orientation", ["returns"])
  ,("gtk_progress_bar_set_orientation", ["orientation"])
  ,("gtk_statusbar_set_has_resize_grip", ["setting"])
  ,("gtk_statusbar_get_has_resize_grip", ["returns"])	
  ,("gtk_editable_get_editable", ["returns"])
  ,("gtk_entry_set_text", ["text"])
  ,("gtk_entry_get_text", ["returns"])
  ,("gtk_entry_append_text", ["text"])
  ,("gtk_entry_prepend_text", ["text"])
  ,("gtk_entry_set_invisible_char", ["ch"])
  ,("gtk_entry_set_has_frame", ["setting"])
  ,("gtk_entry_set_completion", ["completion"])
  ,("gtk_spin_button_get_value", ["returns"])
  ,("gtk_spin_button_get_value_as_int", ["returns"])
  ,("gtk_spin_button_set_value", ["value"])
  ,("gtk_expander_new", ["label"])
  ,("gtk_expander_set_expanded", ["expanded"])
  ,("gtk_expander_get_expanded", ["returns"])
  ,("gtk_expander_set_spacing", ["spacing"])
  ,("gtk_expander_set_label", ["label"])
  ,("gtk_expander_get_label", ["returns"])
  ,("gtk_expander_get_use_markup", ["returns"])
  ,("gtk_fixed_set_has_window", ["hasWindow"])
  ,("gtk_fixed_get_has_window", ["returns"])
  ,("gtk_notebook_get_n_pages", ["returns"])
  ,("gtk_adjustment_set_value", ["value"])
  ,("gtk_adjustment_get_value", ["returns"])
  ,("gtk_arrow_new", ["arrowType", "shadowType"])
  ,("gtk_arrow_set", ["arrowType", "shadowType"])
  ,("gtk_calendar_set_display_options", ["flags"])
  ,("gtk_calendar_display_options", ["flags"])
  ,("gtk_calendar_get_display_options", ["returns"])
  ,("gtk_event_box_set_visible_window", ["visibleWindow"])
  ,("gtk_event_box_get_visible_window", ["returns"])
  ,("gtk_event_box_set_above_child", ["aboveChild"])
  ,("gtk_event_box_get_above_child", ["returns"])
  ,("gtk_handle_box_set_shadow_type", ["type"])
  ,("gtk_viewport_get_hadjustment", ["returns"])
  ,("gtk_viewport_get_vadjustment", ["returns"])
  ,("gtk_viewport_set_hadjustment", ["adjustment"])
  ,("gtk_viewport_set_vadjustment", ["adjustment"])
  ,("gtk_frame_set_label_widget", ["labelWidget"])
  ,("gtk_frame_set_shadow_type", ["type"])
  ,("gtk_frame_get_shadow_type", ["returns"])
  ,("gtk_scrolled_window_get_hadjustment", ["returns"])
  ,("gtk_scrolled_window_get_vadjustment", ["returns"])
  ,("gtk_scrolled_window_get_placement", ["returns"])
  ,("gtk_scrolled_window_set_shadow_type", ["type"])
  ,("gtk_scrolled_window_get_shadow_type", ["returns"])
  ,("gtk_scrolled_window_set_hadjustment", ["hadjustment"])
  ,("gtk_scrolled_window_set_vadjustment", ["hadjustment"])
  ,("gtk_window_set_title", ["title"])
  ,("gtk_window_set_resizable", ["resizable"])
  ,("gtk_window_set_position", ["position"])
  ,("gtk_window_set_destroy_with_parent", ["setting"])
  ,("gtk_window_set_decorated", ["setting"])
  ,("gtk_color_selection_is_adjusting", ["returns"])
  ,("gtk_check_menu_item_set_active", ["isActive"])
  ,("gtk_check_menu_item_get_active", ["returns"])
  ,("gtk_check_menu_item_set_inconsistent", ["setting"])
  ,("gtk_check_menu_item_get_inconsistent", ["returns"])
  ,("gtk_check_menu_item_set_draw_as_radio", ["drawAsRadio"])
  ,("gtk_check_menu_item_get_draw_as_radio", ["returns"])
  ,("gtk_combo_set_use_arrows", ["val"])
  ,("gtk_combo_set_use_arrows_always", ["val"])
  ,("gtk_combo_set_case_sensitive", ["val"])
  ,("gtk_combo_box_set_wrap_width", ["width"])
  ,("gtk_combo_box_set_row_span_column", ["rowSpan"])
  ,("gtk_combo_box_set_column_span_column", ["columnSpan"])
  ,("gtk_combo_box_set_model", ["model"])
  ,("gtk_combo_box_append_text", ["text"])
  ,("gtk_combo_box_prepend_text", ["text"])
  ,("gtk_menu_set_title", ["title"])
  ,("gtk_menu_item_set_submenu", ["submenu"])
  ,("gtk_menu_item_get_right_justified", ["returns"])
  ,("gtk_option_menu_get_menu", ["returns"])
  ,("gtk_option_menu_set_menu", ["menu"])
  ,("gtk_tool_item_get_homogeneous", ["returns"])
  ,("gtk_tool_item_set_expand", ["expand"])
  ,("gtk_tool_item_get_expand", ["returns"])
  ,("gtk_tool_item_set_use_drag_window", ["useDragWindow"])
  ,("gtk_tool_item_get_use_drag_window", ["returns"])
  ,("gtk_tool_item_set_visible_horizontal", ["visibleHorizontal"])
  ,("gtk_tool_item_get_visible_horizontal", ["returns"])
  ,("gtk_tool_item_set_visible_vertical", ["visibleVertical"])
  ,("gtk_tool_item_get_visible_vertical", ["returns"])
  ,("gtk_tool_item_set_is_important", ["isImportant"])
  ,("gtk_tool_item_get_icon_size", ["returns"])
  ,("gtk_tool_item_get_orientation", ["returns"])
  ,("gtk_tool_item_get_toolbar_style", ["returns"])
  ,("gtk_tool_item_get_relief_style", ["returns"])
  ,("gtk_tool_item_get_is_important", ["returns"])
  ,("gtk_tool_item_retrieve_proxy_menu_item", ["returns"])
  ,("gtk_toolbar_set_orientation", ["orientation"])
  ,("gtk_toolbar_get_orientation", ["returns"])
  ,("gtk_toolbar_set_style", ["style"])
  ,("gtk_toolbar_get_style", ["returns"])
  ,("gtk_toolbar_get_tooltips", ["returns"])
  ,("gtk_toolbar_get_icon_size", ["returns"])
  ,("gtk_toolbar_get_n_items", ["returns"])
  ,("gtk_toolbar_set_show_arrow", ["showArrow"])
  ,("gtk_toolbar_get_show_arrow", ["returns"])
  ,("gtk_toolbar_get_relief_style", ["returns"])
  ,("gtk_toolbar_set_icon_size", ["iconSize"])
  ,("gtk_file_chooser_get_action", ["returns"])
  ,("gtk_file_chooser_set_local_only", ["localOnly"])
  ,("gtk_file_chooser_get_local_only", ["returns"])
  ,("gtk_file_chooser_set_select_multiple", ["selectMultiple"])
  ,("gtk_file_chooser_get_select_multiple", ["returns"])
  ,("gtk_file_chooser_get_filenames", ["returns"])
  ,("gtk_file_chooser_add_filter", ["filter"])
  ,("gtk_file_chooser_remove_filter", ["filter"])
  ,("gtk_file_chooser_set_filter", ["filter"])
  ,("gtk_file_chooser_add_shortcut_folder", ["returns"])
  ,("gtk_file_chooser_remove_shortcut_folder", ["returns"])
  ,("gtk_file_chooser_add_shortcut_folder_uri", ["returns"])
  ,("gtk_file_chooser_remove_shortcut_folder_uri", ["returns"])
  ,("gtk_file_chooser_get_uris", ["returns"])
  ,("gtk_file_chooser_list_filters", ["returns"])
  ,("gtk_file_chooser_list_shortcut_folders", ["returns"])
  ,("gtk_file_chooser_list_shortcut_folder_uris", ["returns"])
  ,("gtk_font_selection_get_preview_text", ["returns"])
  ,("gtk_font_selection_set_preview_text", ["text"])
  ,("gtk_font_selection_dialog_get_preview_text", ["returns"])
  ,("gtk_font_selection_dialog_set_preview_text", ["text"])
  ,("gtk_text_mark_get_name", ["returns"])
  ,("gtk_text_mark_get_buffer", ["returns"])
  ,("gtk_text_mark_get_visible", ["returns"])
  ,("gtk_text_mark_get_deleted", ["returns"])
  ,("gtk_text_mark_set_visible", ["setting"])
  ,("gtk_text_mark_get_left_gravity", ["returns"])
  ,("gtk_text_tag_new", ["name"])
  ,("gtk_text_tag_get_priority", ["returns"])
  ,("gtk_text_tag_set_priority", ["priority"])
  ,("gtk_text_tag_table_add", ["tag"])
  ,("gtk_text_tag_table_remove", ["tag"])
  ,("gtk_text_tag_table_get_size", ["returns"])
  ,("gtk_text_buffer_get_line_count", ["returns"])
  ,("gtk_text_buffer_get_char_count", ["returns"])
  ,("gtk_text_buffer_get_tag_table", ["returns"])
  ,("gtk_text_buffer_get_text", ["returns"])
  ,("gtk_text_buffer_get_slice", ["returns"])
  ,("gtk_text_buffer_insert_at_cursor", ["text", "len"])
  ,("gtk_text_buffer_get_insert", ["returns"])
  ,("gtk_text_buffer_get_selection_bound", ["returns"])
  ,("gtk_text_buffer_set_modified", ["setting"])
  ,("gtk_text_buffer_get_end_iter", ["iter"])
  ,("gtk_text_view_new_with_buffer", ["buffer"])
  ,("gtk_text_view_set_buffer", ["buffer"])
  ,("gtk_text_view_get_buffer", ["returns"])
  ,("gtk_text_view_get_iter_location", ["iter", "location"])
  ,("gtk_text_view_set_wrap_mode", ["wrapMode"])
  ,("gtk_text_view_get_wrap_mode", ["returns"])
  ,("gtk_text_view_set_editable", ["setting"])
  ,("gtk_text_view_get_editable", ["returns"])
  ,("gtk_text_view_set_cursor_visible", ["setting"])
  ,("gtk_text_view_get_cursor_visible", ["returns"])  
  ,("gtk_text_view_set_pixels_above_lines", ["pixelsAboveLines"])
  ,("gtk_text_view_get_pixels_above_lines", ["returns"])
  ,("gtk_text_view_set_pixels_below_lines", ["pixelsBelowLines"])
  ,("gtk_text_view_get_pixels_below_lines", ["returns"])
  ,("gtk_text_view_set_pixels_inside_wrap", ["pixelsInsideWrap"])
  ,("gtk_text_view_get_pixels_inside_wrap", ["returns"])
  ,("gtk_text_view_set_justification", ["justification"])
  ,("gtk_text_view_get_justification", ["returns"])
  ,("gtk_text_view_get_default_attributes", ["returns"])
  ,("gtk_color_button_get_color", ["color"])
  ,("gtk_combo_box_get_wrap_width", ["returns"])
  ,("gtk_combo_box_get_row_span_column", ["returns"])
  ,("gtk_combo_box_get_column_span_column", ["returns"])
  ,("gtk_combo_box_get_active_text", ["returns"])
  ,("gtk_combo_box_get_add_tearoffs", ["returns"])
  ,("gtk_combo_box_set_focus_on_click", ["returns"])
  ,("gtk_image_get_pixel_size", ["returns"])
  ,("gtk_image_set_from_file", ["filename"])
  ,("gtk_progress_bar_set_ellipsize", ["mode"])
  ,("gtk_progress_bar_get_ellipsize", ["returns"])
  ,("gtk_widget_get_modifier_style", ["returns"])
  ,("gtk_widget_get_default_direction", ["returns"])
  ,("gtk_widget_get_direction", ["returns"])
  ,("gtk_widget_set_direction", ["dir"])
  ,("gtk_widget_get_name", ["returns"])
  ,("gtk_text_view_get_overwrite", ["returns"])
  ,("gtk_action_get_name", ["returns"])
  ,("gtk_toggle_action_get_active", ["returns"])
  ,("gtk_toggle_action_set_draw_as_radio", ["drawAsRadio"])
  ,("gtk_toggle_action_get_draw_as_radio", ["returns"])
  ,("gtk_separator_tool_item_set_draw", ["draw"])
  ,("gtk_separator_tool_item_get_draw", ["returns"])
  ,("gtk_tool_button_get_stock_id", ["returns"])
  ,("gtk_ui_manager_get_action_groups", ["returns"])
  ,("gtk_action_group_set_sensitive", ["sensitive"])
  ,("gtk_action_group_get_sensitive", ["returns"])
  ,("gtk_action_group_get_visible", ["returns"])
  ,("gtk_action_group_set_visible", ["visible"])
  ,("gtk_action_group_remove_action", ["action"])
  ,("gtk_menu_tool_button_get_menu", ["returns"])
  ,("gtk_toggle_tool_button_set_active", ["isActive"])
  ,("gtk_toggle_tool_button_get_active", ["returns"])
  ,("gtk_tool_button_get_label", ["returns"])
  ,("gtk_tool_button_get_use_underline", ["returns"])
  ,("gtk_tool_button_set_use_underline", ["useUnderline"])
  ,("gtk_action_group_add_action", ["action"])
  ,("gtk_action_get_proxies", ["returns"])
  ,("gtk_about_dialog_get_authors", ["returns"])
  ,("gtk_about_dialog_get_artists", ["returns"])
  ,("gtk_about_dialog_get_documenters", ["returns"])
  ,("gtk_about_dialog_get_license", ["returns"])
  ,("gtk_about_dialog_get_version", ["returns"])
  ,("gtk_about_dialog_get_copyright", ["returns"])
  ,("gtk_about_dialog_get_comments", ["returns"])
  ,("gtk_about_dialog_get_website", ["returns"])
  ,("gtk_about_dialog_get_website_label", ["returns"])
  ,("gtk_about_dialog_get_translator_credits", ["returns"])
  ,("gtk_about_dialog_get_logo", ["returns"])
  ,("gtk_about_dialog_get_logo_icon_name", ["returns"])
  ,("gtk_about_dialog_set_version", ["version"])
  ,("gtk_about_dialog_set_copyright", ["copyright"])
  ,("gtk_about_dialog_set_comments", ["comments"])
  ,("gtk_about_dialog_set_website_label", ["websiteLabel"])
  ,("gtk_about_dialog_set_translator_credits", ["translatorCredits"])
  ,("gtk_file_selection_get_selections", ["returns"])
  ,("gtk_tree_model_get_flags", ["returns"])
  ]

nukeParameterDocumentation :: String -> String -> Bool
nukeParameterDocumentation = nukeParamDoc

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

win32FileNameFunctions :: Set String
win32FileNameFunctions = Set.fromList
  ["gtk_image_new_from_file"
  ,"gdk_pixbuf_new_from_file"
  ,"gdk_pixbuf_savev"
  ,"gtk_icon_source_get_filename"
  ,"gtk_icon_source_set_filename"
  ,"gtk_image_set_from_file"
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
  ,"gtk_file_selection_get_filename"
  ,"gtk_file_selection_get_selections"
  ,"gtk_ui_manager_add_ui_from_file"
  ,"gtk_window_set_icon_from_file"]

actionSignalWanted :: String -> String -> Bool
actionSignalWanted "GtkButton" "clicked" = True
actionSignalWanted "GtkWidget" "popup_menu" = True
actionSignalWanted "GtkWidget" "show_help" = True
actionSignalWanted _ _ = False
