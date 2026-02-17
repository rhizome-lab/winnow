# GameMaker (GML) API Coverage

Tracks implementation status of GML runtime functions and built-in variables.
Organized by the categories in the [GML Reference](https://manual.gamemaker.io/lts/en/GameMaker_Language/GML_Reference/GML_Reference.htm).

Legend: [x] implemented, [~] partial/stub, [ ] not started

---

## Variable Functions

### Variable Instance/Global
- [x] variable_global_exists
- [x] variable_global_get
- [x] variable_global_set
- [ ] variable_instance_exists
- [ ] variable_instance_get_names
- [ ] variable_instance_names_count
- [ ] variable_instance_get
- [ ] variable_instance_set

### Variable Struct
- [ ] variable_struct_exists
- [ ] variable_struct_get
- [ ] variable_struct_set
- [ ] variable_struct_remove
- [ ] variable_struct_get_names
- [ ] variable_struct_names_count
- [ ] instanceof

### Arrays
- [x] array_create
- [ ] array_copy
- [ ] array_equals
- [ ] array_get
- [ ] array_set
- [ ] array_push
- [ ] array_pop
- [ ] array_insert
- [ ] array_delete
- [ ] array_sort
- [x] array_length (via array_length_1d)
- [ ] array_resize
- [~] array_length_1d (deprecated, type sig only)
- [~] array_length_2d (deprecated, type sig only)

### Data Types
- [x] is_string
- [x] is_real
- [ ] is_numeric
- [ ] is_bool
- [x] is_array
- [ ] is_struct
- [ ] is_method
- [ ] is_ptr
- [ ] is_int32
- [ ] is_int64
- [ ] is_undefined
- [ ] is_nan
- [ ] is_infinity
- [ ] typeof
- [ ] bool
- [ ] ptr
- [x] int64

### Methods
- [ ] method
- [ ] method_get_self
- [ ] method_get_index

---

## Asset Management

### Sprites
- [x] sprite_get_width
- [x] sprite_get_height
- [x] sprite_get_number (image count)
- [x] sprite_get_xoffset
- [x] sprite_get_yoffset
- [x] sprite_get_bbox_left
- [x] sprite_get_bbox_right
- [x] sprite_get_bbox_top
- [x] sprite_get_bbox_bottom
- [ ] sprite_get_name
- [ ] sprite_get_speed
- [ ] sprite_get_speed_type
- [ ] sprite_exists
- [ ] sprite_add / sprite_replace / sprite_delete
- [ ] sprite_duplicate / sprite_merge
- [ ] sprite_set_offset / sprite_set_bbox_mode
- [ ] sprite_create_from_surface
- [ ] sprite_collision_mask

### Audio
- [~] audio_play_sound (type sig only)
- [~] audio_stop_sound (type sig only)
- [~] audio_is_playing (type sig only)
- [~] audio_sound_gain (type sig only)
- [ ] audio_pause_sound / audio_resume_sound
- [ ] audio_stop_all
- [ ] audio_sound_get_gain / audio_sound_pitch
- [ ] audio_sound_set_track_position / audio_sound_get_track_position
- [ ] audio_play_sound_at (3D audio)
- [ ] audio_listener_* (3D audio listener)
- [ ] audio_emitter_* (3D audio emitter)
- [ ] audio_group_* (audio groups)
- [ ] audio_create_stream / audio_destroy_stream
- [ ] audio_system / audio_channel_num
- [ ] audio_master_gain

### Fonts
- [x] font_get_size
- [ ] font_get_name
- [ ] font_get_first / font_get_last
- [ ] font_get_italic / font_get_bold
- [ ] font_exists
- [ ] font_add / font_replace / font_delete
- [ ] font_add_sprite / font_add_sprite_ext
- [ ] font_set_cache_size

### Objects
- [ ] object_exists
- [ ] object_get_name
- [ ] object_get_sprite / object_get_depth / object_get_visible
- [ ] object_get_persistent / object_get_solid
- [ ] object_get_parent / object_is_ancestor
- [ ] object_set_* (sprite, depth, visible, persistent, solid)
- [ ] object_get_mask

### Rooms
- [ ] room_exists
- [ ] room_get_name
- [ ] room_get_info
- [ ] room_add / room_duplicate / room_assign
- [ ] room_set_* (width, height, background, view)

### Instances
- [x] instance_create (instance_create_layer / instance_create_depth)
- [x] instance_destroy
- [x] instance_exists
- [x] instance_number
- [~] instance_find (type sig only)
- [~] instance_nearest (type sig only)
- [~] instance_furthest (type sig only)
- [~] instance_change (type sig only)
- [ ] instance_copy
- [ ] instance_id_get
- [ ] instance_place / instance_place_list
- [ ] instance_position / instance_position_list
- [ ] instance_activate_* / instance_deactivate_*

### Paths
- [ ] path_start / path_end
- [ ] path_get_* (length, speed, x, y, number, etc.)
- [ ] path_set_* (kind, closed, precision, etc.)
- [ ] path_add / path_delete / path_duplicate
- [ ] path_add_point / path_insert_point / path_delete_point

### Timelines
- [ ] timeline_* functions

### Scripts
- [~] script_execute (type sig only)
- [ ] script_exists / script_get_name

### Sequences
- [ ] Entire sequences API (GMS2.3+)

### Animation Curves
- [ ] Entire animation curves API (GMS2.3+)

### Tags
- [ ] asset_get_tags / asset_add_tags / asset_remove_tags
- [ ] tag_get_asset_ids

### Tile Sets
- [ ] tilemap_* functions
- [ ] tileset_* functions

### Shaders
- [ ] shader_set / shader_reset
- [ ] shader_get_uniform / shader_set_uniform_*
- [ ] shader_get_sampler_index
- [ ] shader_is_compiled / shader_current

### Extensions
- [ ] extension_* functions

---

## General Game Control

### Game Functions
- [x] game_restart
- [x] game_end
- [~] game_save (type sig only)
- [~] game_load (type sig only)

### Built-in Variables
- [x] score (via global)
- [x] health (via global)
- [x] lives (via global)
- [ ] game_id
- [ ] game_save_id
- [ ] game_display_name
- [ ] game_project_name

### Speed
- [ ] game_set_speed / game_get_speed
- [ ] fps / fps_real

### High Score
- [ ] highscore_add / highscore_name / highscore_value / highscore_clear

---

## Movement And Collisions

### Movement
- [x] Built-in vars: x, y, speed, direction, hspeed, vspeed, friction, gravity, gravity_direction
- [~] motion_set (type sig only)
- [~] move_towards_point (type sig only)
- [~] move_wrap (type sig only)
- [ ] move_bounce_all / move_bounce_solid
- [ ] move_contact_all / move_contact_solid
- [ ] move_outside_all / move_outside_solid
- [ ] move_snap
- [ ] move_random
- [ ] mp_linear_step / mp_potential_step (motion planning)
- [ ] mp_grid_* (A* pathfinding)

### Collisions
- [~] place_meeting (type sig only)
- [~] place_free (type sig only)
- [~] place_empty (type sig only)
- [~] collision_point (type sig only)
- [~] collision_rectangle (type sig only)
- [~] collision_line (type sig only)
- [~] collision_circle (type sig only)
- [ ] collision_ellipse
- [ ] place_snapped
- [ ] position_meeting / position_empty
- [~] distance_to_object (type sig only)
- [~] distance_to_point (type sig only)

---

## Drawing

### Colour And Alpha
- [x] draw_set_color / draw_get_colour
- [x] draw_set_alpha / draw_get_alpha
- [x] Color constants: c_white, c_black, c_red, c_lime, c_blue, c_yellow, c_aqua, c_fuchsia, c_maroon, c_green, c_navy, c_olive, c_teal, c_purple, c_orange, c_silver, c_ltgray, c_gray, c_dkgray
- [x] make_color_rgb / make_color_hsv
- [x] color_get_red / color_get_green / color_get_blue / color_get_hue
- [x] merge_color
- [ ] color_get_saturation / color_get_value
- [ ] draw_getpixel / draw_getpixel_ext

### Basic Forms
- [x] draw_rectangle
- [ ] draw_roundrect / draw_roundrect_ext
- [~] draw_circle (type sig only)
- [ ] draw_circle_colour
- [ ] draw_ellipse / draw_ellipse_colour
- [~] draw_line (type sig only)
- [ ] draw_line_colour / draw_line_width / draw_line_width_colour
- [ ] draw_point / draw_point_colour
- [ ] draw_arrow
- [ ] draw_button
- [ ] draw_path
- [ ] draw_healthbar
- [ ] draw_rectangle_colour
- [ ] draw_triangle / draw_triangle_colour

### Text
- [x] draw_text
- [x] draw_text_ext
- [x] draw_text_color / draw_text_colour
- [x] draw_text_ext_color / draw_text_ext_colour
- [x] draw_text_transformed
- [x] draw_text_ext_transformed
- [x] draw_text_transformed_color / draw_text_transformed_colour
- [x] draw_text_ext_transformed_color / draw_text_ext_transformed_colour
- [x] draw_set_font
- [x] draw_set_halign / draw_set_valign
- [x] Alignment constants: fa_left, fa_center, fa_right, fa_top, fa_middle, fa_bottom
- [ ] string_width / string_width_ext
- [ ] string_height
- [x] string_height_ext

### Sprites And Tiles
- [x] draw_sprite
- [x] draw_sprite_ext
- [x] draw_self
- [ ] draw_sprite_general
- [ ] draw_sprite_part / draw_sprite_part_ext
- [ ] draw_sprite_stretched / draw_sprite_stretched_ext
- [ ] draw_sprite_pos
- [ ] draw_sprite_tiled / draw_sprite_tiled_ext
- [ ] draw_tile / draw_tilemap

### Surfaces
- [~] surface_create (type sig only)
- [~] surface_free (type sig only)
- [~] surface_exists (type sig only)
- [~] surface_set_target (type sig only)
- [~] surface_reset_target (type sig only)
- [~] surface_get_width / surface_get_height (type sig only)
- [~] draw_surface (type sig only)
- [ ] draw_surface_ext / draw_surface_part / draw_surface_stretched
- [ ] surface_get_texture
- [ ] surface_copy / surface_copy_part
- [ ] surface_save / surface_save_part
- [ ] surface_getpixel / surface_getpixel_ext
- [ ] surface_resize
- [ ] application_surface

### GPU Control
- [ ] gpu_set_* / gpu_get_* (blendmode, alphatestenable, etc.)
- [ ] draw_enable_alphablend / draw_enable_swf_aa

### Primitives And Vertex Formats
- [ ] draw_primitive_begin / draw_vertex / draw_primitive_end
- [ ] draw_vertex_colour / draw_vertex_texture / draw_vertex_texture_colour
- [ ] vertex_format_* / vertex_buffer_*
- [ ] draw_primitive_begin_texture

### Lighting
- [ ] draw_light_* functions

### Particles
- [ ] part_system_* / part_type_* / part_emitter_*

### Textures
- [ ] texture_* functions
- [ ] sprite_get_uvs / font_get_uvs

### Shaders
- [ ] (see Asset Management > Shaders)

### Video Playback
- [ ] video_* functions

---

## Cameras And Display

### The Game Window
- [ ] window_set_* / window_get_* (size, position, fullscreen, caption, cursor)
- [ ] display_get_width / display_get_height
- [ ] display_get_gui_width / display_get_gui_height

### Cameras And View Ports
- [ ] camera_create / camera_destroy
- [ ] camera_set_* / camera_get_* (view_pos, view_size, etc.)
- [ ] view_enabled / view_visible / view_camera
- [ ] Built-in view vars: view_xport, view_yport, view_wport, view_hport

### Room/View Built-ins
- [x] room (current room index — via room_goto)
- [ ] room_width / room_height
- [ ] room_speed
- [ ] view_current

---

## Game Input

### Mouse Input
- [x] mouse_x / mouse_y
- [x] mouse_check_button
- [x] mouse_check_button_pressed
- [x] mouse_check_button_released
- [ ] mouse_clear
- [ ] mouse_wheel_up / mouse_wheel_down
- [x] Button constants: mb_left, mb_right, mb_middle (mapped 1/2/3)
- [ ] mb_none, mb_any

### Keyboard Input
- [~] keyboard_check (type sig only)
- [~] keyboard_check_pressed (type sig only)
- [~] keyboard_check_released (type sig only)
- [~] keyboard_check_direct (type sig only)
- [x] keyboard_string
- [ ] keyboard_clear
- [ ] keyboard_key / keyboard_lastkey / keyboard_lastchar
- [ ] keyboard_set_map / keyboard_get_map / keyboard_unset_map
- [ ] Virtual key constants (vk_left, vk_right, vk_up, vk_down, vk_enter, vk_escape, vk_space, vk_shift, vk_control, vk_alt, vk_tab, vk_backspace, vk_delete, vk_insert, vk_home, vk_end, vk_pageup, vk_pagedown, vk_f1-vk_f12, vk_nokey, vk_anykey)

### Gamepad Input
- [ ] gamepad_is_connected / gamepad_get_device_count
- [ ] gamepad_button_check / gamepad_button_check_pressed / gamepad_button_check_released
- [ ] gamepad_axis_value / gamepad_hat_value
- [ ] gamepad_button_count / gamepad_hat_count / gamepad_axis_count
- [ ] gamepad_set_vibration / gamepad_set_axis_deadzone / gamepad_set_button_threshold
- [ ] gp_* constants

### Device Input
- [ ] device_mouse_* (touch input)
- [ ] device_is_keypad_open

### Gesture Input
- [ ] gesture_* functions

### Virtual Keys
- [ ] virtual_key_add / virtual_key_delete / virtual_key_show / virtual_key_hide

---

## Strings

- [x] string_length
- [x] string_copy
- [x] string_insert
- [x] string_replace_all
- [x] string_lower
- [x] string_upper
- [x] string_char_at
- [x] string_pos
- [x] string_delete
- [x] string_count
- [x] chr
- [x] ord
- [x] string (conversion function)
- [ ] string_byte_at / string_byte_length / string_set_byte_at
- [ ] string_repeat
- [ ] string_replace (single occurrence)
- [ ] string_pos_ext (start position)
- [ ] string_last_pos / string_last_pos_ext
- [ ] string_letters / string_lettersdigits / string_digits
- [ ] string_format
- [ ] string_hash_to_newline
- [ ] string_width / string_width_ext / string_height (drawing measurement)
- [ ] ansi_char / ord_at (GMS2.3+)

---

## Maths And Numbers

### Number Functions
- [x] floor, ceil, round
- [x] abs, sign
- [x] min, max, clamp
- [x] sqrt, sqr, power, exp
- [x] ln, log2, log10, logn
- [x] frac
- [x] lerp, mean, median
- [ ] dot_product / dot_product_3d
- [ ] dot_product_normalised / dot_product_3d_normalised
- [ ] math_set_epsilon / math_get_epsilon

### Trigonometry (Degree-based)
- [x] sin, cos, tan (radians)
- [x] dsin, dcos, dtan (degrees)
- [x] darcsin, darccos, darctan, darctan2
- [x] degtorad, radtodeg
- [ ] arcsin, arccos, arctan, arctan2 (radian-based)

### Angles And Distance
- [x] point_distance
- [x] point_direction
- [x] lengthdir_x, lengthdir_y
- [ ] point_distance_3d
- [ ] angle_difference

### Random
- [x] random, random_range
- [x] irandom, irandom_range
- [x] choose
- [x] randomize, random_set_seed
- [ ] random_get_seed

### Date And Time
- [ ] date_* functions (date_create_datetime, date_current_datetime, etc.)
- [ ] current_time / current_year / current_month / current_day / etc.

### Type Conversion
- [x] real
- [x] int (→ int32 truncation)
- [x] int64
- [x] string (number → string)
- [ ] bool
- [ ] ptr

### Matrix Functions
- [ ] matrix_build / matrix_multiply
- [ ] matrix_set / matrix_get
- [ ] matrix_build_identity / matrix_build_lookat / matrix_build_projection_*
- [ ] matrix_transform_vertex / matrix_stack_*

---

## Data Structures

### Lists (ds_list)
- [~] ds_list_create (type sig only)
- [~] ds_list_destroy (type sig only)
- [~] ds_list_add (type sig only)
- [~] ds_list_size (type sig only)
- [~] ds_list_find_value (type sig only)
- [ ] ds_list_delete / ds_list_insert
- [ ] ds_list_find_index / ds_list_sort
- [ ] ds_list_clear / ds_list_copy
- [ ] ds_list_replace / ds_list_shuffle
- [ ] ds_list_mark_as_list / ds_list_mark_as_map
- [ ] ds_list_read / ds_list_write

### Maps (ds_map)
- [~] ds_map_create (type sig only)
- [~] ds_map_destroy (type sig only)
- [~] ds_map_add (type sig only)
- [~] ds_map_find_value (type sig only)
- [~] ds_map_exists (type sig only)
- [ ] ds_map_delete / ds_map_replace
- [ ] ds_map_size / ds_map_empty
- [ ] ds_map_clear / ds_map_copy
- [ ] ds_map_find_first / ds_map_find_last / ds_map_find_next / ds_map_find_previous
- [ ] ds_map_keys_to_array / ds_map_values_to_array
- [ ] ds_map_read / ds_map_write
- [ ] ds_map_secure_save / ds_map_secure_load
- [ ] json_encode / json_decode (maps to/from JSON)

### Grids (ds_grid)
- [~] ds_grid_create (type sig only)
- [~] ds_grid_destroy (type sig only)
- [ ] ds_grid_set / ds_grid_get
- [ ] ds_grid_width / ds_grid_height / ds_grid_resize
- [ ] ds_grid_clear / ds_grid_copy
- [ ] ds_grid_add / ds_grid_multiply
- [ ] ds_grid_set_region / ds_grid_get_max / ds_grid_get_min / ds_grid_get_sum / ds_grid_get_mean
- [ ] ds_grid_sort / ds_grid_shuffle
- [ ] ds_grid_read / ds_grid_write

### Priority Queues (ds_priority)
- [ ] ds_priority_create / ds_priority_destroy
- [ ] ds_priority_add / ds_priority_delete_min / ds_priority_delete_max
- [ ] ds_priority_find_min / ds_priority_find_max / ds_priority_find_priority
- [ ] ds_priority_change_priority / ds_priority_size / ds_priority_empty / ds_priority_clear

### Queues (ds_queue)
- [ ] ds_queue_create / ds_queue_destroy
- [ ] ds_queue_enqueue / ds_queue_dequeue
- [ ] ds_queue_head / ds_queue_tail / ds_queue_size / ds_queue_empty / ds_queue_clear

### Stacks (ds_stack)
- [ ] ds_stack_create / ds_stack_destroy
- [ ] ds_stack_push / ds_stack_pop / ds_stack_top
- [ ] ds_stack_size / ds_stack_empty / ds_stack_clear

### Utility
- [ ] ds_set_precision
- [ ] ds_exists

---

## File Handling

### INI Files
- [x] ini_open
- [x] ini_close
- [x] ini_read_string / ini_read_real
- [x] ini_write_string / ini_write_real
- [x] ini_section_exists / ini_key_exists
- [x] ini_section_delete / ini_key_delete
- [x] ini_open_from_string

### Text Files
- [ ] file_text_open_read / file_text_open_write / file_text_open_append
- [ ] file_text_close
- [ ] file_text_read_string / file_text_read_real / file_text_readln
- [ ] file_text_write_string / file_text_write_real / file_text_writeln
- [ ] file_text_eof / file_text_eoln

### Binary Files
- [ ] file_bin_open / file_bin_close
- [ ] file_bin_read_byte / file_bin_write_byte
- [ ] file_bin_position / file_bin_seek / file_bin_size / file_bin_rewrite

### File System
- [ ] file_exists / file_delete / file_copy / file_rename
- [ ] directory_exists / directory_create / directory_destroy
- [ ] file_find_first / file_find_next / file_find_close
- [ ] filename_name / filename_path / filename_dir / filename_drive / filename_ext
- [ ] filename_change_ext / temp_directory / working_directory

### Encoding And Hashing
- [ ] base64_encode / base64_decode
- [ ] md5_string_utf8 / md5_string_unicode / md5_file
- [ ] sha1_string_utf8 / sha1_string_unicode / sha1_file
- [ ] json_stringify / json_parse

---

## Buffers

- [ ] buffer_create / buffer_delete
- [ ] buffer_read / buffer_write / buffer_fill
- [ ] buffer_seek / buffer_tell / buffer_get_size / buffer_resize
- [ ] buffer_get_type / buffer_get_alignment / buffer_get_address
- [ ] buffer_copy / buffer_copy_from_vertex_buffer
- [ ] buffer_load / buffer_save / buffer_load_async / buffer_save_async
- [ ] buffer_compress / buffer_decompress
- [ ] buffer_base64_encode / buffer_base64_decode
- [ ] buffer_md5 / buffer_sha1 / buffer_crc32
- [ ] buffer_get_surface / buffer_set_surface

---

## Physics (Box2D)

- [ ] physics_world_create / physics_world_update_speed / physics_world_gravity
- [ ] physics_fixture_create / physics_fixture_bind / physics_fixture_set_*
- [ ] physics_apply_force / physics_apply_impulse / physics_apply_torque
- [ ] physics_joint_* (distance, revolute, prismatic, pulley, gear, rope, weld, friction, spring)
- [ ] physics_particle_* (soft body simulation)
- [ ] phy_* built-in variables

---

## Networking

- [ ] network_create_server / network_create_socket
- [ ] network_connect / network_send_* / network_destroy
- [ ] network_set_timeout / network_set_config
- [ ] http_get / http_post_string / http_request
- [ ] url_open / url_open_ext / url_open_full

---

## Cameras And Display (see above)

---

## Time Sources (GMS2.3+)

- [ ] time_source_create / time_source_destroy
- [ ] time_source_start / time_source_stop / time_source_pause / time_source_resume
- [ ] time_source_reconfigure / time_source_get_*
- [ ] call_later / call_cancel

---

## OS And Compiler

- [ ] os_type / os_device / os_version / os_browser
- [ ] os_is_paused / os_is_network_connected
- [ ] os_get_config / os_get_info / os_get_language / os_get_region
- [ ] os_lock_orientation
- [ ] parameter_count / parameter_string
- [ ] environment_get_variable

---

## Debugging

- [~] show_debug_message (type sig only)
- [~] show_message (type sig only)
- [ ] show_debug_overlay
- [ ] debug_event / debug_get_callstack
- [ ] fps / fps_real
- [ ] code_is_compiled
- [ ] get_timer

---

## Garbage Collection

- [ ] gc_collect / gc_enable / gc_is_enabled
- [ ] gc_get_stats / gc_target_frame_time

---

## Other (Async, Web, Steam, Xbox, IAP)

These categories are mostly irrelevant for offline game lifting but listed for completeness:

### Asynchronous Functions
- [ ] Async HTTP, Dialog, Save/Load, Cloud, Push Notifications, etc.

### Web And HTML5
- [ ] clickable_* functions
- [ ] url_open / browser_*
- [ ] os_is_paused (web focus)

### Steam
- [ ] steam_* functions (achievements, leaderboards, workshop, etc.)

### Xbox Live
- [ ] xboxlive_* functions

### In App Purchases
- [ ] iap_* functions

---

## Built-in Instance Variables (GMLObject)

### Position & Movement
- [x] x, y
- [x] xstart, ystart
- [x] xprevious, yprevious
- [x] speed, direction
- [x] hspeed, vspeed
- [x] friction
- [x] gravity, gravity_direction

### Sprite & Drawing
- [x] sprite_index
- [x] image_index, image_speed
- [x] image_xscale, image_yscale
- [x] image_alpha
- [x] image_angle
- [x] image_blend
- [x] image_number
- [x] sprite_width, sprite_height
- [x] depth
- [x] visible
- [x] mask_index

### Bounding Box
- [x] bbox_left, bbox_right, bbox_top, bbox_bottom

### Object Identity
- [x] object_index
- [x] id
- [x] persistent
- [x] solid

### Alarms
- [x] alarm[0] through alarm[11]

### Path
- [x] path_index, path_position, path_speed, path_scale, path_orientation, path_endaction (fields only, no path system)

---

## Built-in Events (GMLObject)

### Lifecycle
- [x] Create
- [x] Destroy
- [x] Room Start / Room End

### Step
- [x] Begin Step / Step / End Step

### Draw
- [x] Draw / Draw GUI
- [x] Draw Begin / Draw End / Draw GUI Begin / Draw GUI End
- [x] Pre Draw / Post Draw / Resize

### Alarms
- [x] Alarm 0-11

### Keyboard
- [x] Key Press / Keyboard / Key Release (per key code 0-255)

### Mouse
- [x] Left/Right/Middle Button (down/pressed/released)
- [x] No Button
- [x] Mouse Enter / Mouse Leave
- [x] Global Left/Right/Middle (down/pressed/released)

### User Events
- [x] User 0-15

### View
- [x] Outside View 0-7 / Boundary View 0-7

### Other
- [x] Collision events (per object type)
- [~] event_perform (type sig only)
- [~] event_user (type sig only)
- [~] event_inherited (type sig only)

---

## Platform Abstraction Layer

### Graphics
- [x] Canvas 2D initialization (GraphicsContext)
- [x] Canvas resize
- [x] OffscreenCanvas support (font tinting)

### Images
- [x] Async image loading (texture sheets)

### Input
- [x] Mouse position tracking (DOM → GML coordinates)
- [x] Mouse button mapping (DOM button → GML mb_left/mb_right/mb_middle)
- [x] Keyboard input routing

### Timing
- [x] Frame scheduling (setTimeout-based)
- [x] Frame cancellation

### Not Yet Abstracted
- [ ] Audio (no platform module)
- [ ] Persistence/Save (INI uses localStorage directly, not abstracted)
- [ ] Networking
- [ ] Clipboard
- [ ] Dialog/Modal
