-- This module actually contains data. It is specific to the Gtk+ libs that
-- we're playing with. If we really cared enough we could split this
-- information out into seperate data files that get read in at runtime.
-- However for now it's easier just to edit the data here and recompile.

module MarshalFixup where

cTypeNameToHSType :: String -> String
cTypeNameToHSType ('A':'t':'k':remainder) = remainder
cTypeNameToHSType ('G':'t':'k':remainder) = remainder
cTypeNameToHSType "GdkWindow" = "DrawWindow"
cTypeNameToHSType ('G':'d':'k':remainder) = remainder
cTypeNameToHSType "PangoLayout" = "PangoLayout"
cTypeNameToHSType ('P':'a':'n':'g':'o':remainder) = remainder
cTypeNameToHSType ('G':'n':'o':'m':'e':remainder) = remainder
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
fixCFunctionName other = other

-- In some cases the way we work out the minimum version of the module doesn't
-- work (since the docs sometimes miss marking the version of some functions)
-- So to fix it just specify here the version of the library from which the
-- module is available.
fixModuleAvailableSince "GtkComboBox" = "2.4"
fixModuleAvailableSince "GtkFileChooser" = "2.4"
fixModuleAvailableSince "GtkCellView" = "2.6"
fixModuleAvailableSince "GtkIconView" = "2.6"
fixModuleAvailableSince "GtkMenuToolButton" = "2.6"
fixModuleAvailableSince _ = ""

-- Most of the time the gtk-doc documentation have titles that correspond to
-- the C name of the object. But sadly sometimes they don't so we fix up that
-- mapping here.
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
leafClass "GtkAdjustment" = True
leafClass "GdkPixbuf"     = True
leafClass "GtkImage"      = True
leafClass "GtkIconFactory"  = True
leafClass "GtkEntryCompletion" = True
leafClass "GtkFileFilter"   = True
leafClass "GtkUIManager"    = True
leafClass "GtkActionGroup"  = True
leafClass "GtkRadioButton"  = True
leafClass "GtkEventBox"     = True
leafClass "GtkExpander"     = True
leafClass "GtkAccelGroup"   = True
leafClass "GtkTooltips"     = True
leafClass "GtkTextChildAnchor" = True
leafClass "GdkWindow"       = True
leafClass "GdkDisplay"      = True
leafClass "GdkScreen"       = True
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
maybeNullParameter "gtk_entry_completion_set_model" "model"	= True
maybeNullParameter "gtk_label_new" "str"			= True
maybeNullParameter "gtk_about_dialog_set_license" "license"	= True
maybeNullParameter "gtk_about_dialog_set_logo" "logo"		= True
maybeNullParameter "gtk_about_dialog_set_logo_icon_name" "logo" = True
maybeNullParameter "gtk_layout_new" _				= True
maybeNullParameter "gtk_notebook_set_menu_label" "menuLabel"	= True
maybeNullParameter "gtk_scrolled_window_new" "hadjustment"	= True
maybeNullParameter "gtk_scrolled_window_new" "vadjustment"	= True
maybeNullParameter "gtk_combo_box_set_model" "model"		= True
maybeNullParameter "gtk_menu_set_screen" "screen"		= True
maybeNullParameter "gtk_menu_item_set_accel_path" "accelPath"	= True
maybeNullParameter "gtk_toolbar_set_drop_highlight_item" "toolItem"	= True
maybeNullParameter "gtk_text_buffer_new" "table"		= True
maybeNullParameter "gtk_text_buffer_create_mark" "markName"	= True
maybeNullParameter "gtk_cell_view_set_displayed_row" "path"	= True
maybeNullParameter "gtk_about_dialog_set_logo_icon_name" "iconName"	= True
maybeNullParameter "gtk_widget_modify_fg" "color"		= True
maybeNullParameter "gtk_widget_modify_bg" "color"		= True
maybeNullParameter "gtk_widget_modify_text" "color"		= True
maybeNullParameter "gtk_widget_modify_base" "color"		= True
maybeNullParameter "gtk_action_group_add_action_with_accel" "accelerator"	= True
maybeNullParameter "gtk_radio_tool_button_new" "group"		= True
maybeNullParameter "gtk_radio_tool_button_new_from_stock" "group"	= True
maybeNullParameter "gtk_tool_button_set_label" "label"		= True
maybeNullParameter "gtk_tool_button_set_icon_widget" "iconWidget"	= True
maybeNullParameter "gtk_tool_button_set_label_widget" "labelWidget"	= True
maybeNullParameter "gtk_ui_manager_add_ui" "action"		= True
maybeNullParameter "gtk_menu_tool_button_new" "iconWidget"	= True
maybeNullParameter "gtk_menu_tool_button_new" "label"		= True
maybeNullParameter "gtk_menu_tool_button_set_menu" "menu"	= True
maybeNullParameter "gtk_tool_button_new" "iconWidget"		= True
maybeNullParameter "gtk_tool_button_new" "label"		= True
maybeNullParameter "gtk_tool_button_set_stock_id" "stockId"	= True
maybeNullParameter "gtk_action_new" "tooltip"			= True
maybeNullParameter "gtk_action_new" "stockId"			= True
maybeNullParameter "gtk_toggle_action_new" "tooltip"		= True
maybeNullParameter "gtk_toggle_action_new" "stockId"		= True
maybeNullParameter "gtk_radio_action_new" "tooltip"		= True
maybeNullParameter "gtk_radio_action_new" "stockId"		= True
maybeNullParameter "gtk_tree_model_iter_n_children" "iter"  	= True
--maybeNullParameter "" ""	= True
--maybeNullParameter "" ""	= True
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
maybeNullResult "gtk_label_get_mnemonic_widget" = True
maybeNullResult "gtk_notebook_get_menu_label" = True
maybeNullResult "gtk_notebook_get_menu_label_text" = True
maybeNullResult "gtk_notebook_get_nth_page" = True
maybeNullResult "gtk_notebook_get_tab_label" = True
maybeNullResult "gtk_notebook_get_tab_label_text" = True
maybeNullResult "gtk_combo_box_get_model" = True
maybeNullResult "gtk_image_menu_item_get_image" = True
maybeNullResult "gtk_menu_get_title" = True
maybeNullResult "gtk_menu_item_get_submenu" = True
maybeNullResult "gtk_tool_item_retrieve_proxy_menu_item" = True
maybeNullResult "gtk_tool_item_get_proxy_menu_item" = True
maybeNullResult "gtk_toolbar_get_nth_item" = True
maybeNullResult "gtk_file_chooser_get_filename" = True
maybeNullResult "gtk_file_chooser_get_current_folder" = True
maybeNullResult "gtk_file_chooser_get_uri" = True
maybeNullResult "gtk_file_chooser_get_preview_widget" = True
maybeNullResult "gtk_file_chooser_get_preview_filename" = True
maybeNullResult "gtk_file_chooser_get_preview_uri" = True
maybeNullResult "gtk_file_chooser_get_extra_widget" = True
maybeNullResult "gtk_file_chooser_get_filter" = True
maybeNullResult "gtk_font_selection_get_font_name" = True
maybeNullResult "gtk_font_selection_dialog_get_font_name" = True
maybeNullResult "gtk_text_mark_get_name" = True
maybeNullResult "gtk_text_mark_get_buffer" = True
maybeNullResult "gtk_text_tag_table_lookup" = True
maybeNullResult "gtk_text_buffer_get_mark" = True
maybeNullResult "gtk_text_view_get_window" = True
maybeNullResult "gtk_icon_view_get_path_at_pos" = True
maybeNullResult "gtk_combo_box_get_active_text" = True
maybeNullResult "gtk_scale_get_layout" = True
maybeNullResult "gtk_button_get_image" = True
maybeNullResult "gtk_image_get_animation" = True
maybeNullResult "gtk_window_get_transient_for" = True
maybeNullResult "gtk_window_get_role" = True
maybeNullResult "gtk_window_get_title" = True
maybeNullResult "gtk_widget_render_icon" = True
maybeNullResult "gtk_widget_get_composite_name" = True
maybeNullResult "gtk_action_get_accel_path" = True
maybeNullResult "gtk_action_group_get_action" = True
maybeNullResult "gtk_tool_button_get_label" = True
maybeNullResult "gtk_tool_button_get_icon_widget" = True
maybeNullResult "gtk_tool_button_get_label_widget" = True
maybeNullResult "gtk_ui_manager_get_widget" = True
maybeNullResult "gtk_ui_manager_get_action" = True
maybeNullResult "gtk_menu_tool_button_get_menu" = True
maybeNullResult "gtk_tool_button_get_stock_id" = True
maybeNullResult "gtk_about_dialog_get_license" = True
maybeNullResult "gtk_menu_get_attach_widget" = True
--maybeNullResult "" = True
--maybeNullResult "" = True
maybeNullResult _ = False

-- Often the documentation for parameters or the return value of functions
-- that is included in the gtk-doc docs are just pointless. So this table
-- lists the function and parameter names for which we do not want to use the
-- gtk-doc documentation.
nukeParamDoc :: String -> String -> Bool
nukeParamDoc "gtk_button_box_get_layout" "returns"		= True
nukeParamDoc "gtk_button_set_label" "label"			= True
nukeParamDoc "gtk_button_get_label" "returns" 			= True
nukeParamDoc "gtk_toggle_button_get_active" "returns"		= True
nukeParamDoc "gtk_image_new_from_file" "filename"		= True
nukeParamDoc "gtk_image_new_from_pixbuf" "pixbuf"		= True
nukeParamDoc "gtk_label_new" "str"				= True
nukeParamDoc "gtk_label_set_text" "str"				= True
nukeParamDoc "gtk_label_set_label" "str"			= True
nukeParamDoc "gtk_label_set_justify" "jtype"			= True
nukeParamDoc "gtk_label_get_justify" "returns"			= True
nukeParamDoc "gtk_label_set_use_underline" "setting"		= True
nukeParamDoc "gtk_label_get_use_underline" "returns"		= True
nukeParamDoc "gtk_label_get_layout" "returns"			= True
nukeParamDoc "gtk_label_get_text" "returns"			= True
nukeParamDoc "gtk_label_get_label" "returns"			= True
nukeParamDoc "gtk_label_set_text_with_mnemonic" "str"		= True
nukeParamDoc "gtk_progress_bar_set_text" "text"			= True
nukeParamDoc "gtk_progress_bar_get_orientation" "returns"	= True
nukeParamDoc "gtk_progress_bar_set_orientation" "orientation"	= True
nukeParamDoc "gtk_statusbar_set_has_resize_grip" "setting"	= True
nukeParamDoc "gtk_statusbar_get_has_resize_grip" "returns"	= True	
nukeParamDoc "gtk_editable_get_editable" "returns"		= True
nukeParamDoc "gtk_entry_set_text" "text"			= True
nukeParamDoc "gtk_entry_get_text" "returns"			= True
nukeParamDoc "gtk_entry_append_text" "text"			= True
nukeParamDoc "gtk_entry_prepend_text" "text"			= True
nukeParamDoc "gtk_entry_set_invisible_char" "ch"		= True
nukeParamDoc "gtk_entry_set_has_frame" "setting"		= True
nukeParamDoc "gtk_entry_set_completion" "completion"		= True
nukeParamDoc "gtk_spin_button_get_value" "returns"		= True
nukeParamDoc "gtk_spin_button_get_value_as_int" "returns"	= True
nukeParamDoc "gtk_spin_button_set_value" "value"		= True
nukeParamDoc "gtk_expander_new" "label"				= True
nukeParamDoc "gtk_expander_set_expanded" "expanded"		= True
nukeParamDoc "gtk_expander_get_expanded" "returns"		= True
nukeParamDoc "gtk_expander_set_spacing" "spacing"		= True
nukeParamDoc "gtk_expander_set_label" "label"			= True
nukeParamDoc "gtk_expander_get_label" "returns"			= True
nukeParamDoc "gtk_expander_get_use_markup" "returns"		= True
nukeParamDoc "gtk_fixed_set_has_window" "hasWindow"		= True
nukeParamDoc "gtk_fixed_get_has_window" "returns"		= True
nukeParamDoc "gtk_notebook_get_n_pages" "returns"		= True
nukeParamDoc "gtk_adjustment_set_value" "value"			= True
nukeParamDoc "gtk_adjustment_get_value" "returns"		= True
nukeParamDoc "gtk_arrow_new" "arrowType"			= True
nukeParamDoc "gtk_arrow_new" "shadowType"			= True
nukeParamDoc "gtk_arrow_set" "arrowType"			= True
nukeParamDoc "gtk_arrow_set" "shadowType"			= True
nukeParamDoc "gtk_calendar_set_display_options" "flags"		= True
nukeParamDoc "gtk_calendar_display_options" "flags"		= True
nukeParamDoc "gtk_calendar_get_display_options" "returns"	= True
nukeParamDoc "gtk_event_box_set_visible_window" "visibleWindow"	= True
nukeParamDoc "gtk_event_box_get_visible_window" "returns"	= True
nukeParamDoc "gtk_event_box_set_above_child" "aboveChild"	= True
nukeParamDoc "gtk_event_box_get_above_child" "returns"		= True
nukeParamDoc "gtk_handle_box_set_shadow_type" "type"		= True
nukeParamDoc "gtk_viewport_get_hadjustment" "returns"		= True
nukeParamDoc "gtk_viewport_get_vadjustment" "returns"		= True
nukeParamDoc "gtk_viewport_set_hadjustment" "adjustment"	= True
nukeParamDoc "gtk_viewport_set_vadjustment" "adjustment"	= True
nukeParamDoc "gtk_frame_set_label_widget" "labelWidget"		= True
nukeParamDoc "gtk_frame_set_shadow_type" "type"			= True
nukeParamDoc "gtk_frame_get_shadow_type" "returns"		= True
nukeParamDoc "gtk_scrolled_window_get_hadjustment" "returns"	= True
nukeParamDoc "gtk_scrolled_window_get_vadjustment" "returns"	= True
nukeParamDoc "gtk_scrolled_window_get_placement" "returns"	= True
nukeParamDoc "gtk_scrolled_window_set_shadow_type" "type"	= True
nukeParamDoc "gtk_scrolled_window_get_shadow_type" "returns"	= True
nukeParamDoc "gtk_scrolled_window_set_hadjustment" "hadjustment"= True
nukeParamDoc "gtk_scrolled_window_set_vadjustment" "hadjustment"= True
nukeParamDoc "gtk_window_set_title" "title"			= True
nukeParamDoc "gtk_window_set_resizable" "resizable"		= True
nukeParamDoc "gtk_window_set_position" "position"		= True
nukeParamDoc "gtk_window_set_destroy_with_parent" "setting"	= True
nukeParamDoc "gtk_window_set_decorated" "setting"		= True
nukeParamDoc "gtk_color_selection_is_adjusting" "returns"	= True
nukeParamDoc "gtk_check_menu_item_set_active" "isActive"	= True
nukeParamDoc "gtk_check_menu_item_get_active" "returns"		= True
nukeParamDoc "gtk_check_menu_item_set_inconsistent" "setting"	= True
nukeParamDoc "gtk_check_menu_item_get_inconsistent" "returns"	= True
nukeParamDoc "gtk_check_menu_item_set_draw_as_radio" "drawAsRadio"	= True
nukeParamDoc "gtk_check_menu_item_get_draw_as_radio" "returns"	= True
nukeParamDoc "gtk_combo_set_use_arrows" "val"			= True
nukeParamDoc "gtk_combo_set_use_arrows_always" "val"		= True
nukeParamDoc "gtk_combo_set_case_sensitive" "val"		= True
nukeParamDoc "gtk_combo_box_set_wrap_width" "width"		= True
nukeParamDoc "gtk_combo_box_set_row_span_column" "rowSpan"	= True
nukeParamDoc "gtk_combo_box_set_column_span_column" "columnSpan"= True
nukeParamDoc "gtk_combo_box_set_model" "model"			= True
nukeParamDoc "gtk_combo_box_append_text" "text"			= True
nukeParamDoc "gtk_combo_box_prepend_text" "text"		= True
nukeParamDoc "gtk_menu_set_title" "title"			= True
nukeParamDoc "gtk_menu_item_set_submenu" "submenu"		= True
nukeParamDoc "gtk_menu_item_get_right_justified" "returns"	= True
nukeParamDoc "gtk_option_menu_get_menu" "returns"		= True
nukeParamDoc "gtk_option_menu_set_menu" "menu"			= True
nukeParamDoc "gtk_tool_item_get_homogeneous" "returns"		= True
nukeParamDoc "gtk_tool_item_set_expand" "expand"		= True
nukeParamDoc "gtk_tool_item_get_expand" "returns"		= True
nukeParamDoc "gtk_tool_item_set_use_drag_window" "useDragWindow"= True
nukeParamDoc "gtk_tool_item_get_use_drag_window" "returns"	= True
nukeParamDoc "gtk_tool_item_set_visible_horizontal" "visibleHorizontal"	= True
nukeParamDoc "gtk_tool_item_get_visible_horizontal" "returns"	= True
nukeParamDoc "gtk_tool_item_set_visible_vertical" "visibleVertical"	= True
nukeParamDoc "gtk_tool_item_get_visible_vertical" "returns"	= True
nukeParamDoc "gtk_tool_item_set_is_important" "isImportant"	= True
nukeParamDoc "gtk_tool_item_get_icon_size" "returns"		= True
nukeParamDoc "gtk_tool_item_get_orientation" "returns"		= True
nukeParamDoc "gtk_tool_item_get_toolbar_style" "returns"	= True
nukeParamDoc "gtk_tool_item_get_relief_style" "returns"		= True
nukeParamDoc "gtk_tool_item_get_is_important" "returns"		= True
nukeParamDoc "gtk_tool_item_retrieve_proxy_menu_item" "returns"	= True
nukeParamDoc "gtk_toolbar_set_orientation" "orientation"	= True
nukeParamDoc "gtk_toolbar_get_orientation" "returns"		= True
nukeParamDoc "gtk_toolbar_set_style" "style"			= True
nukeParamDoc "gtk_toolbar_get_style" "returns"			= True
nukeParamDoc "gtk_toolbar_get_tooltips" "returns"		= True
nukeParamDoc "gtk_toolbar_get_icon_size" "returns"		= True
nukeParamDoc "gtk_toolbar_get_n_items" "returns"		= True
nukeParamDoc "gtk_toolbar_set_show_arrow" "showArrow"		= True
nukeParamDoc "gtk_toolbar_get_show_arrow" "returns"		= True
nukeParamDoc "gtk_toolbar_get_relief_style" "returns"		= True
nukeParamDoc "gtk_toolbar_set_icon_size" "iconSize"		= True
nukeParamDoc "gtk_file_chooser_get_action" "returns"		= True
nukeParamDoc "gtk_file_chooser_set_local_only" "localOnly"	= True
nukeParamDoc "gtk_file_chooser_get_local_only" "returns"	= True
nukeParamDoc "gtk_file_chooser_set_select_multiple" "selectMultiple"	= True
nukeParamDoc "gtk_file_chooser_get_select_multiple" "returns"	= True
nukeParamDoc "gtk_file_chooser_get_filenames" "returns"		= True
nukeParamDoc "gtk_file_chooser_add_filter" "filter"		= True
nukeParamDoc "gtk_file_chooser_remove_filter" "filter"		= True
nukeParamDoc "gtk_file_chooser_set_filter" "filter"		= True
nukeParamDoc "gtk_file_chooser_add_shortcut_folder" "returns"	= True
nukeParamDoc "gtk_file_chooser_remove_shortcut_folder" "returns"= True
nukeParamDoc "gtk_file_chooser_add_shortcut_folder_uri" "returns"	= True
nukeParamDoc "gtk_file_chooser_remove_shortcut_folder_uri" "returns"	= True
nukeParamDoc "gtk_file_chooser_get_uris" "returns"		= True
nukeParamDoc "gtk_file_chooser_list_filters" "returns"		= True
nukeParamDoc "gtk_file_chooser_list_shortcut_folders" "returns"	= True
nukeParamDoc "gtk_file_chooser_list_shortcut_folder_uris" "returns"	= True
nukeParamDoc "gtk_font_selection_get_preview_text" "returns"	= True
nukeParamDoc "gtk_font_selection_set_preview_text" "text"	= True
nukeParamDoc "gtk_font_selection_dialog_get_preview_text" "returns"	= True
nukeParamDoc "gtk_font_selection_dialog_set_preview_text" "text"	= True
nukeParamDoc "gtk_text_mark_get_name" "returns"			= True
nukeParamDoc "gtk_text_mark_get_buffer" "returns"		= True
nukeParamDoc "gtk_text_mark_get_visible" "returns"		= True
nukeParamDoc "gtk_text_mark_get_deleted" "returns"		= True
nukeParamDoc "gtk_text_mark_set_visible" "setting"		= True
nukeParamDoc "gtk_text_mark_get_left_gravity" "returns"		= True
nukeParamDoc "gtk_text_tag_new" "name"				= True
nukeParamDoc "gtk_text_tag_get_priority" "returns"		= True
nukeParamDoc "gtk_text_tag_set_priority" "priority"		= True
nukeParamDoc "gtk_text_tag_table_add" "tag"			= True
nukeParamDoc "gtk_text_tag_table_remove" "tag"			= True
nukeParamDoc "gtk_text_tag_table_get_size" "returns"		= True
nukeParamDoc "gtk_text_buffer_get_line_count" "returns"		= True
nukeParamDoc "gtk_text_buffer_get_char_count" "returns"		= True
nukeParamDoc "gtk_text_buffer_get_tag_table" "returns"		= True
nukeParamDoc "gtk_text_buffer_get_text" "returns"		= True
nukeParamDoc "gtk_text_buffer_get_slice" "returns"		= True
nukeParamDoc "gtk_text_buffer_insert_at_cursor" "text"		= True
nukeParamDoc "gtk_text_buffer_insert_at_cursor" "len"		= True
nukeParamDoc "gtk_text_buffer_get_insert" "returns"		= True
nukeParamDoc "gtk_text_buffer_get_selection_bound" "returns"	= True
nukeParamDoc "gtk_text_buffer_set_modified" "setting"		= True
nukeParamDoc "gtk_text_buffer_get_end_iter" "iter"		= True
nukeParamDoc "gtk_text_view_new_with_buffer" "buffer"		= True
nukeParamDoc "gtk_text_view_set_buffer" "buffer"		= True
nukeParamDoc "gtk_text_view_get_buffer" "returns"		= True
nukeParamDoc "gtk_text_view_get_iter_location" "iter"		= True
nukeParamDoc "gtk_text_view_get_iter_location" "location"	= True
nukeParamDoc "gtk_text_view_set_wrap_mode" "wrapMode"		= True
nukeParamDoc "gtk_text_view_get_wrap_mode" "returns"		= True
nukeParamDoc "gtk_text_view_set_editable" "setting"		= True
nukeParamDoc "gtk_text_view_get_editable" "returns"		= True
nukeParamDoc "gtk_text_view_set_cursor_visible" "setting"	= True
nukeParamDoc "gtk_text_view_get_cursor_visible" "returns"  	= True
nukeParamDoc "gtk_text_view_set_pixels_above_lines" "pixelsAboveLines"	= True
nukeParamDoc "gtk_text_view_get_pixels_above_lines" "returns"	= True
nukeParamDoc "gtk_text_view_set_pixels_below_lines" "pixelsBelowLines"	= True
nukeParamDoc "gtk_text_view_get_pixels_below_lines" "returns"	= True
nukeParamDoc "gtk_text_view_set_pixels_inside_wrap" "pixelsInsideWrap"	= True
nukeParamDoc "gtk_text_view_get_pixels_inside_wrap" "returns"	= True
nukeParamDoc "gtk_text_view_set_justification" "justification"	= True
nukeParamDoc "gtk_text_view_get_justification" "returns"	= True
nukeParamDoc "gtk_text_view_get_default_attributes" "returns"	= True
nukeParamDoc "gtk_color_button_get_color" "color"		= True
nukeParamDoc "gtk_combo_box_get_wrap_width" "returns"		= True
nukeParamDoc "gtk_combo_box_get_row_span_column" "returns"	= True
nukeParamDoc "gtk_combo_box_get_column_span_column" "returns"	= True
nukeParamDoc "gtk_combo_box_get_active_text" "returns"		= True
nukeParamDoc "gtk_combo_box_get_add_tearoffs" "returns"		= True
nukeParamDoc "gtk_combo_box_set_focus_on_click" "returns"	= True
nukeParamDoc "gtk_image_get_pixel_size" "returns"		= True
nukeParamDoc "gtk_image_set_from_file" "filename"		= True
nukeParamDoc "gtk_progress_bar_set_ellipsize" "mode"		= True
nukeParamDoc "gtk_progress_bar_get_ellipsize" "returns"		= True
nukeParamDoc "gtk_widget_get_modifier_style" "returns"		= True
nukeParamDoc "gtk_widget_get_default_direction" "returns"	= True
nukeParamDoc "gtk_widget_get_direction" "returns"		= True
nukeParamDoc "gtk_widget_set_direction" "dir"			= True
nukeParamDoc "gtk_widget_get_name" "returns"			= True
nukeParamDoc "gtk_text_view_get_overwrite" "returns"		= True
nukeParamDoc "gtk_action_get_name" "returns"			= True
nukeParamDoc ('g':'t':'k':'_':'u':'i':'_':'m':'a':'n':'a':'g':'e':'r':'_':_) "self" = True
nukeParamDoc "gtk_toggle_action_get_active" "returns"		= True
nukeParamDoc "gtk_toggle_action_set_draw_as_radio" "drawAsRadio"= True
nukeParamDoc "gtk_toggle_action_get_draw_as_radio" "returns"	= True
nukeParamDoc "gtk_separator_tool_item_set_draw" "draw"		= True
nukeParamDoc "gtk_separator_tool_item_get_draw" "returns"	= True
nukeParamDoc "gtk_tool_button_get_stock_id" "returns"		= True
nukeParamDoc "gtk_ui_manager_get_action_groups" "returns"	= True
nukeParamDoc "gtk_action_group_set_sensitive" "sensitive"	= True
nukeParamDoc "gtk_action_group_get_sensitive" "returns"		= True
nukeParamDoc "gtk_action_group_get_visible" "returns"		= True
nukeParamDoc "gtk_action_group_set_visible" "visible"		= True
nukeParamDoc "gtk_action_group_remove_action" "action"		= True
nukeParamDoc "gtk_menu_tool_button_get_menu" "returns"		= True
nukeParamDoc "gtk_toggle_tool_button_set_active" "isActive"	= True
nukeParamDoc "gtk_toggle_tool_button_get_active" "returns"	= True
nukeParamDoc "gtk_tool_button_get_label" "returns"		= True
nukeParamDoc "gtk_tool_button_get_use_underline" "returns"	= True
nukeParamDoc "gtk_tool_button_set_use_underline" "useUnderline"	= True
nukeParamDoc "gtk_action_group_add_action" "action"		= True
nukeParamDoc "gtk_action_get_proxies" "returns"			= True
nukeParamDoc "gtk_about_dialog_get_authors" "returns"		= True
nukeParamDoc "gtk_about_dialog_get_artists" "returns"		= True
nukeParamDoc "gtk_about_dialog_get_documenters" "returns"	= True
nukeParamDoc "gtk_about_dialog_get_license" "returns"		= True
nukeParamDoc "gtk_about_dialog_get_version" "returns"		= True
nukeParamDoc "gtk_about_dialog_get_copyright" "returns"		= True
nukeParamDoc "gtk_about_dialog_get_comments" "returns"		= True
nukeParamDoc "gtk_about_dialog_get_website" "returns"		= True
nukeParamDoc "gtk_about_dialog_get_website_label" "returns"	= True
nukeParamDoc "gtk_about_dialog_get_translator_credits" "returns"= True
nukeParamDoc "gtk_about_dialog_get_logo" "returns"		= True
nukeParamDoc "gtk_about_dialog_get_logo_icon_name" "returns"	= True
nukeParamDoc "gtk_about_dialog_set_version" "version"		= True
nukeParamDoc "gtk_about_dialog_set_copyright" "copyright"	= True
nukeParamDoc "gtk_about_dialog_set_comments" "comments"		= True
nukeParamDoc "gtk_about_dialog_set_website_label" "websiteLabel"= True
nukeParamDoc "gtk_about_dialog_set_translator_credits" "translatorCredits"	= True
nukeParamDoc "gtk_file_selection_get_selections" "returns"	= True
nukeParamDoc "gtk_tree_model_get_flags" "returns"		= True
--nukeParamDoc "" ""			= True
--nukeParamDoc "" ""			= True
nukeParamDoc _ _ = False
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
