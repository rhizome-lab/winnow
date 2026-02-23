meta:
  id: game_maker_data
  title: GameMaker Studio data.win
  application: GameMaker Studio / GameMaker Studio 2
  file-extension: win
  endian: le
  license: CC0-1.0
  ks-version: 0.11
doc: |
  GameMaker Studio compiled game data file (data.win or game.win).
  Contains all compiled game assets: GML bytecode, object definitions,
  room layouts, sprites, sounds, fonts, texture atlases, and more.

  Structure: a FORM container holding named 8-byte-headered chunks.
  Chunk order varies by version; GEN8 always appears first and contains
  the bytecode_version field that governs the format of CODE/FUNC/VARI.

  Bytecode version (GEN8.bytecode_version):
    13 = Early GameMaker: Studio
    14 = GMS 1.x (old instruction format)
    15 = GMS 1.4.x (new instruction format; extended CODE/FUNC/VARI headers)
    16 = GMS 1.4.9999+ (LANG and GLOB chunks may be present)
    17 = GMS 2.x (GMS2 texture layout, OBJT managed field, FUNC operand addressing)

  GMS2.3+ (IDE version major >= 2): adds SEQN chunk; CODE uses shared bytecode
  blobs for child functions (lambdas, struct constructors). See CODE chunk docs.

  String references: most string fields store a u32 absolute file offset pointing
  to the character bytes of a GameMaker string. The 4-byte length prefix is at
  offset-4. To resolve a StringRef value V: seek to (V - 4) and read a gm_string.
  See type gm_string and the STRG chunk doc for full details.

  PE-embedded games: some GM1 games embed data.win inside a Windows PE (.exe).
  To locate the FORM: scan for the first 0x46 0x4F 0x52 0x4D ("FORM") sequence
  where the following u32 size field satisfies (size + 8 <= file_size). Strip
  the PE prefix before parsing.

seq:
  - id: magic
    contents: [0x46, 0x4F, 0x52, 0x4D]  # "FORM"
  - id: size
    type: u4
    doc: Total byte size of all chunks combined (excludes the 8-byte FORM header).
  - id: chunks
    type: chunk
    repeat: eos
    doc: |
      Ordered sequence of chunks. GEN8 is always first. STRG appears early.
      LANG and GLOB present only when bytecode_version >= 16.
      SEQN present only in GMS2.3+ games (IDE major >= 2).

types:

  # ---------------------------------------------------------------------------
  # Top-level container
  # ---------------------------------------------------------------------------

  chunk:
    doc: A single named data chunk within the FORM container.
    seq:
      - id: magic
        size: 4
        type: str
        encoding: ASCII
        doc: 4-byte ASCII chunk identifier (e.g. "GEN8", "CODE", "STRG").
      - id: size
        type: u4
        doc: Size of the chunk body in bytes (excludes this 8-byte header).
      - id: body
        size: size
        type:
          switch-on: magic
          cases:
            '"GEN8"': gen8_body
            '"STRG"': strg_body
            '"CODE"': code_body
            '"FUNC"': func_body
            '"VARI"': vari_body
            '"OBJT"': objt_body
            '"SCPT"': scpt_body
            '"ROOM"': room_body
            '"SPRT"': sprt_body
            '"SOND"': sond_body
            '"BGND"': bgnd_body
            '"FONT"': font_body
            '"TPAG"': tpag_body
            '"TXTR"': txtr_body
            '"AUDO"': audo_body
            '"SHDR"': shdr_body
            '"SEQN"': seqn_body
            '"OPTN"': optn_body
            '"GLOB"': glob_body
            '"LANG"': lang_body

  # ---------------------------------------------------------------------------
  # Shared primitives
  # ---------------------------------------------------------------------------

  gm_string:
    doc: |
      A GameMaker null-terminated string with a u32 length prefix.
      Format: [length: u32][chars: u8 × length][null: u8]
    seq:
      - id: length
        type: u4
      - id: value
        type: str
        size: length
        encoding: UTF-8
      - id: terminator
        contents: [0x00]

  pointer_list:
    doc: |
      A count-prefixed list of absolute file offsets (u32 each).
      The ubiquitous indirection pattern throughout the format:
        [count: u32][offset_0: u32][offset_1: u32]...[offset_{count-1}: u32]
      Each offset is an absolute byte position in the file where a typed
      struct begins. Follow each offset to parse the actual entry.
    seq:
      - id: count
        type: u4
      - id: offsets
        type: u4
        repeat: expr
        repeat-expr: count

  # ---------------------------------------------------------------------------
  # GEN8 — Game metadata (always first chunk)
  # ---------------------------------------------------------------------------

  gen8_body:
    doc: |
      Game metadata: version info, window dimensions, room order, etc.
      Always the first chunk. The bytecode_version field here governs the
      layout of CODE, FUNC, and VARI chunks.
    seq:
      - id: is_debug_disabled
        type: u1
        doc: Non-zero if the in-game debugger is disabled.
      - id: bytecode_version
        type: u1
        doc: |
          GML bytecode format version. Governs CODE/FUNC/VARI chunk layouts.
          13=early GMS, 14=GMS1 old format, 15=GMS1.4.x new format,
          16=GMS1.4.9999+ (LANG/GLOB), 17=GMS2.x.
      - id: padding
        type: u2
      - id: filename
        type: u4
        doc: StringRef — absolute offset of the project filename string.
      - id: config
        type: u4
        doc: StringRef — absolute offset of the build configuration name.
      - id: last_obj
        type: u4
        doc: Last object ID + 1 (total object slots allocated by the IDE).
      - id: last_tile
        type: u4
        doc: Last tile ID + 1.
      - id: game_id
        type: u4
        doc: Unique numeric game identifier assigned by the IDE.
      - id: guid
        size: 16
        doc: DirectPlay GUID (16 bytes; typically zeroed in modern games).
      - id: name
        type: u4
        doc: StringRef — absolute offset of the internal game name string.
      - id: ide_version_major
        type: u4
        doc: IDE version major component (1 for GMS1, 2 for GMS2, etc.).
      - id: ide_version_minor
        type: u4
      - id: ide_version_release
        type: u4
      - id: ide_version_build
        type: u4
      - id: default_window_width
        type: u4
        doc: Default game window width in pixels.
      - id: default_window_height
        type: u4
        doc: Default game window height in pixels.
      - id: info_flags
        type: u4
        doc: Game option flags bitmask (fullscreen, sync, interpolation, etc.).
      - id: license_crc32
        type: u4
      - id: license_md5
        size: 16
      - id: timestamp
        type: u8
        doc: Compilation timestamp (Unix epoch seconds).
      - id: display_name
        type: u4
        doc: StringRef — absolute offset of the game display name string.
      - id: active_targets
        type: u8
        doc: Bitmask of enabled compilation target platforms.
      - id: function_classifications
        type: u8
      - id: steam_app_id
        type: s4
      - id: debugger_port
        type: u4
        doc: |
          Debugger port number. Present when bytecode_version >= 14.
          NOTE: this field is absent for bytecode_version <= 13; consumers
          must check bytecode_version before reading.
      - id: room_count
        type: u4
        doc: Number of entries in room_order.
      - id: room_order
        type: u4
        repeat: expr
        repeat-expr: room_count
        doc: Room indices (into ROOM chunk) defining the startup room order.
      - id: gms2_extra
        size-eos: true
        doc: |
          Additional GMS2-specific data present when ide_version_major >= 2.
          Contains random UID fields and extended option data. Format varies
          by IDE version. Treat as opaque unless parsing a specific GMS2 build.

  # ---------------------------------------------------------------------------
  # STRG — String table
  # ---------------------------------------------------------------------------

  strg_body:
    doc: |
      String table: a pointer_list of absolute file offsets, one per string.
      Each offset points to the START of a gm_string (the length prefix u32).

      StringRef values used elsewhere in the file point to the CHARACTER DATA
      of a string, which is 4 bytes PAST the length prefix. To resolve a
      StringRef value V: seek to (V - 4) and read a gm_string.

      String indices in the STRG table are rarely used directly. Most references
      throughout the file are StringRef absolute offsets, not STRG indices.
    seq:
      - id: strings
        type: pointer_list
        doc: count + count × absolute file offsets. Each offset → gm_string.

  # ---------------------------------------------------------------------------
  # CODE — GML bytecode entries
  # ---------------------------------------------------------------------------

  code_body:
    doc: |
      GML bytecode for all scripts and object events.
      An empty chunk (size == 0) means the game was compiled with YYC
      (GameMaker's native code compiler); no bytecode is available.

      Entry format depends on bytecode_version (from GEN8):
        BC <= 14: code_entry_v14 (simple: name + length, bytecode follows)
        BC >= 15: code_entry_v15 (extended: name + blob_length + locals +
                  args + bc_rel_addr + offset_in_blob)

      GMS2.3+ SHARED BYTECODE BLOBS (ide_version_major >= 2):
        Child functions (lambdas, struct constructors) share a single bytecode
        blob with their parent function. Each entry's blob_length is the TOTAL
        blob size (same value for parent and all its children).

        The actual per-entry bytecode length is NOT stored directly and must
        be computed by post-processing:
          1. Group entries by absolute blob address:
             blob_addr = (file_offset_of_bc_rel_addr_field) + bc_rel_addr
          2. Sort each group by offset_in_blob ascending.
          3. Each entry's length = next_entry.offset_in_blob - this.offset_in_blob
             (or blob_length - offset_in_blob for the last entry in the group).

        This gap-based computation is intentional; there is no stored length
        field that gives individual child function sizes.
    seq:
      - id: entries
        type: pointer_list
        doc: count + count × absolute pointers to code_entry structs.

  code_entry_v14:
    doc: |
      BC <= 14 code entry (12 bytes header; bytecode follows immediately).
      Bytecode begins at: (absolute file offset of this entry) + 8.
    seq:
      - id: name
        type: u4
        doc: |
          StringRef — entry name. Convention:
            "gml_Object_<ObjName>_<EventType>_<Subtype>" for object events
            "gml_Script_<ScriptName>" for scripts
            "gml_Room_<RoomName>_Create" for room creation code
      - id: length
        type: u4
        doc: Bytecode length in bytes.

  code_entry_v15:
    doc: |
      BC >= 15 code entry (24 bytes header).
      Bytecode blob address = (file offset of bc_rel_addr field) + bc_rel_addr.
      bc_rel_addr field is at: (entry pointer) + 12
        (after name:4 + blob_length:4 + locals_count:2 + args_count:2).
      Actual bytecode for this entry starts at blob_addr + offset_in_blob.
      See code_body doc for shared blob length computation.
    seq:
      - id: name
        type: u4
        doc: StringRef — entry name (same conventions as code_entry_v14).
      - id: blob_length
        type: u4
        doc: |
          Total bytecode blob length in bytes.
          For standalone entries: equals this entry's bytecode length.
          For GMS2.3+ shared blobs: same value across parent and all children.
      - id: locals_count
        type: u2
        doc: Number of local variable slots used by this entry.
      - id: args_count
        type: u2
        doc: |
          Number of GML arguments. Bit 15 may be set as a "weird local" flag
          in some tools; mask with 0x7FFF to get the clean argument count.
      - id: bc_rel_addr
        type: s4
        doc: |
          Signed offset from this field's own file position to the bytecode blob.
          Blob absolute address = (file offset of THIS field) + bc_rel_addr.
          This field is located at: entry_pointer + 12.
      - id: offset_in_blob
        type: u4
        doc: |
          Byte offset of this entry's bytecode within the shared blob.
          0 for parent/standalone entries. > 0 for child functions.

  # ---------------------------------------------------------------------------
  # FUNC — Function name/address table and local variable info
  # ---------------------------------------------------------------------------

  func_body:
    doc: |
      Function call site chains and per-entry local variable declarations.
      An empty chunk (size == 0) means YYC-compiled game.

      Layout depends on bytecode_version:
        BC <= 14: flat list of function_entry structs (12 bytes each), no count
                  prefix. Read until end of chunk.
        BC >= 15: [func_count: u32][func_count × function_entry]
                  [locals_count: u32][locals_count × code_locals_entry]

      first_address field semantics differ by version:
        BC <= 16: absolute offset of the Call INSTRUCTION WORD (8-byte instruction).
        BC >= 17: absolute offset of the Call OPERAND WORD (4 bytes into the
                  instruction). Subtract 4 to obtain the instruction address.
    seq:
      - id: data
        size-eos: true
        doc: |
          Raw function + locals data. Parse layout according to bytecode_version
          from GEN8. See function_entry and code_locals_entry for field layouts.

  function_entry:
    doc: |
      A single GML function definition with its call site chain head.
      Appears in both BC<=14 (flat list) and BC>=15 (count-prefixed list) formats.
    seq:
      - id: name
        type: u4
        doc: StringRef — function name.
      - id: occurrences
        type: u4
        doc: Total number of Call instructions referencing this function.
      - id: first_address
        type: s4
        doc: |
          Absolute file offset of the first Call instruction referencing this
          function, or -1 if there are no call sites.
          BC <= 16: offset points to the instruction word itself.
          BC >= 17: offset points to the operand word (subtract 4 for instruction).

  code_locals_entry:
    doc: |
      Local variable declarations for a single code entry (BC >= 15 only).
      Follows the function list in the FUNC chunk.
    seq:
      - id: var_count
        type: u4
        doc: Number of local variables declared in this code entry.
      - id: name
        type: u4
        doc: StringRef — name of the code entry (matches a CODE chunk entry name).
      - id: vars
        type: local_var
        repeat: expr
        repeat-expr: var_count

  local_var:
    seq:
      - id: index
        type: u4
        doc: Local variable slot index (0-based).
      - id: name
        type: u4
        doc: StringRef — variable name.

  # ---------------------------------------------------------------------------
  # VARI — Variable name/address table
  # ---------------------------------------------------------------------------

  vari_body:
    doc: |
      Variable reference chains (instance, global, and local variables).
      An empty chunk (size == 0) means YYC-compiled game.

      Layout depends on bytecode_version:
        BC <= 14: flat list of vari_entry_v14 (12 bytes each), no header.
        BC >= 15: [instance_var_count: u32][instance_var_count_max: u32]
                  [max_local_var_count: u32] then flat list of vari_entry_v15
                  (20 bytes each).

      Entry count for BC<=14: chunk_size / 12
      Entry count for BC>=15: (chunk_size - 12) / 20
    seq:
      - id: data
        size-eos: true
        doc: Raw variable data. See vari_entry_v14 / vari_entry_v15 for layouts.

  vari_header_v15:
    doc: The 3-field header at the start of VARI when bytecode_version >= 15.
    seq:
      - id: instance_var_count
        type: u4
        doc: Number of variables with instance_type >= 0 (object-owned fields).
      - id: instance_var_count_max
        type: u4
        doc: Total instance variable ID slots allocated.
      - id: max_local_var_count
        type: u4
        doc: Maximum local variable count across all code entries.

  vari_entry_v14:
    doc: Variable entry for bytecode_version <= 14 (12 bytes).
    seq:
      - id: name
        type: u4
        doc: StringRef — variable name.
      - id: occurrences
        type: u4
        doc: Number of variable read/write instructions referencing this entry.
      - id: first_address
        type: s4
        doc: Absolute file offset of the first reference instruction, or -1.

  vari_entry_v15:
    doc: Variable entry for bytecode_version >= 15 (20 bytes).
    seq:
      - id: name
        type: u4
        doc: StringRef — variable name.
      - id: instance_type
        type: s4
        doc: |
          Instance type / scope of this variable:
            -1 = self (own instance)
            -2 = other
            -5 = global
            -7 = local
            -16 = static
            >= 0 = specific object index (used by GM compiler for self-references
                   within a specific object type; normalize to -1 for decompilation)
      - id: var_id
        type: s4
        doc: Variable ID within its scope (index among all vars of this instance_type).
      - id: occurrences
        type: u4
        doc: Number of variable read/write instructions referencing this entry.
      - id: first_address
        type: s4
        doc: Absolute file offset of the first reference instruction, or -1.

  # ---------------------------------------------------------------------------
  # OBJT — Object (game class) definitions
  # ---------------------------------------------------------------------------

  objt_body:
    doc: |
      Object definitions — the game's "classes", each with physics properties
      and event handlers (Create, Step, Draw, Collision, etc.).
    seq:
      - id: objects
        type: pointer_list
        doc: count + count × absolute pointers to object_entry structs.

  object_entry:
    doc: |
      A single object definition. Accessed via absolute pointer.

      GMS2 VERSION NOTE (bytecode_version >= 17):
        A `managed` u32 field is inserted between `visible` and `solid`.
        It is NOT present in GMS1 (bytecode_version <= 16).
        Consumers must check bytecode_version from GEN8 before parsing this struct.
        The layout below shows the GMS2 form (with managed); for GMS1 omit it.
    seq:
      - id: name
        type: u4
        doc: StringRef — object name (e.g. "obj_Player", "obj_Enemy").
      - id: sprite_index
        type: s4
        doc: Index into the SPRT chunk for this object's default sprite, or -1 for none.
      - id: visible
        type: u4
        doc: Non-zero if the object is visible (drawn) by default.
      - id: managed
        type: u4
        doc: |
          GMS2 only (bytecode_version >= 17): managed instance flag.
          ABSENT in GMS1. Consumers must version-gate this field.
      - id: solid
        type: u4
        doc: Non-zero if the object is solid (stops other solid objects).
      - id: depth
        type: s4
        doc: Depth layer — higher values are drawn behind lower values.
      - id: persistent
        type: u4
        doc: Non-zero if the object survives room changes.
      - id: parent_index
        type: s4
        doc: Parent object index in OBJT for inheritance, or -100 for none.
      - id: mask_index
        type: s4
        doc: Collision mask sprite index, or -1 to use own sprite.
      - id: physics_enabled
        type: u4
        doc: Non-zero if Box2D physics simulation is active for this object.
      - id: physics_sensor
        type: u4
        doc: Non-zero if the physics body is a sensor (detects but doesn't block).
      - id: physics_shape
        type: u4
        doc: |
          Collision shape for physics:
            0 = circle    1 = box    2 = custom polygon (use physics_vertices)
      - id: physics_density
        type: f4
      - id: physics_restitution
        type: f4
        doc: Bounciness coefficient (0.0 = no bounce, 1.0 = perfectly elastic).
      - id: physics_group
        type: u4
        doc: Collision group index (objects in the same group don't collide).
      - id: physics_linear_damping
        type: f4
      - id: physics_angular_damping
        type: f4
      - id: physics_vertex_count
        type: u4
        doc: Number of polygon vertices for custom physics shapes.
      - id: physics_friction
        type: f4
      - id: physics_awake
        type: u4
        doc: Non-zero if the physics body starts awake (active).
      - id: physics_kinematic
        type: u4
        doc: Non-zero if the physics body is kinematic (moved by code, not forces).
      - id: physics_vertices
        type: physics_vertex
        repeat: expr
        repeat-expr: physics_vertex_count
      - id: event_type_count
        type: u4
        doc: |
          Number of event type slots. Typically 12:
            0=Create, 1=Destroy, 2=Alarm, 3=Step, 4=Collision,
            5=Keyboard, 6=Mouse, 7=Other, 8=Draw, 9=KeyPress,
            10=KeyRelease, 11=Trigger
      - id: event_list_ptrs
        type: u4
        repeat: expr
        repeat-expr: event_type_count
        doc: |
          Absolute file pointers, one per event type.
          Each points to an event_sublist (a pointer_list of event_entry structs).
          An event type with no handlers has an empty pointer_list (count=0).

  physics_vertex:
    doc: A single polygon vertex for custom physics shapes.
    seq:
      - id: x
        type: f4
      - id: y
        type: f4

  event_sublist:
    doc: |
      Pointer list of event_entry structs for one event type category.
      Accessed via an absolute pointer from object_entry.event_list_ptrs.
    seq:
      - id: entries
        type: pointer_list

  event_entry:
    doc: |
      A specific event handler within an event type
      (e.g. Create_0, Alarm_3, Collision_with_obj_Wall).
      Accessed via an absolute pointer from event_sublist.
    seq:
      - id: subtype
        type: u4
        doc: |
          Event sub-index. Meaning depends on event type:
            Create/Destroy: always 0
            Alarm: alarm number (0–11)
            Step: 0=step, 1=begin_step, 2=end_step
            Collision: OBJT index of the other object
            Keyboard/KeyPress/KeyRelease: virtual key code
            Mouse: mouse event code
            Other: other-event sub-code
            Draw: 0=draw, 64=draw_gui, 72=draw_resize, etc.
      - id: actions
        type: pointer_list
        doc: count + count × absolute pointers to action structs.

  action:
    doc: |
      An action within an event. Modern GM games use exactly one action per
      event, with action_kind=7 and exec_type=2 (execute a CODE entry).
      Legacy drag-and-drop actions (action_kind != 7) appear in older games.
    seq:
      - id: lib_id
        type: u4
        doc: Library ID (1 for standard built-in actions).
      - id: action_id
        type: u4
        doc: Action ID within the library.
      - id: action_kind
        type: u4
        doc: Action kind — 7 = execute code (the only kind in modern games).
      - id: has_relative
        type: u4
        doc: Non-zero if the action uses relative positioning values.
      - id: is_question
        type: u4
        doc: Non-zero if this action is a conditional (question) block.
      - id: applies_to
        type: s4
        doc: |
          Target instance: 1=self, -1=other, -2=all instances,
          or a specific object index.
      - id: exec_type
        type: u4
        doc: Execution type — 2 = execute a code entry from the CODE chunk.
      - id: func_name
        type: u4
        doc: StringRef — built-in function name (empty/0 for code-execution actions).
      - id: code_id
        type: u4
        doc: |
          Index into the CODE chunk entry list. Valid when exec_type == 2.
          This is the primary field used for decompilation.
      - id: arg_count
        type: u4
      - id: who
        type: s4
        doc: Secondary target instance (-1 = self).
      - id: relative
        type: u4
      - id: is_not
        type: u4
        doc: Non-zero if a conditional action's result is negated.
      - id: padding
        type: u4

  # ---------------------------------------------------------------------------
  # SCPT — Script definitions
  # ---------------------------------------------------------------------------

  scpt_body:
    doc: |
      Script asset name-to-code mappings.
      In GMS2.3+, constructor functions and nested scripts have code_id values
      with the high bit set (>= 0x80000000). These are not direct CODE indices;
      look up the code entry by canonical name "gml_Script_<name>" instead.
    seq:
      - id: scripts
        type: pointer_list
        doc: count + count × absolute pointers to script_entry structs.

  script_entry:
    seq:
      - id: name
        type: u4
        doc: StringRef — script name (e.g. "scr_Damage", "AnyaSticker2A").
      - id: code_id
        type: u4
        doc: |
          Index into the CODE chunk entry list.
          GMS2.3+: if code_id >= 0x80000000, the high bit is a flag indicating
          this is a constructor/nested function. Look up by name in CODE instead:
          find the CODE entry whose name equals "gml_Script_<script_name>".

  # ---------------------------------------------------------------------------
  # ROOM — Room definitions
  # ---------------------------------------------------------------------------

  room_body:
    doc: Room definitions (level layouts with object placements, views, backgrounds).
    seq:
      - id: rooms
        type: pointer_list
        doc: count + count × absolute pointers to room_entry structs.

  room_entry:
    seq:
      - id: name
        type: u4
        doc: StringRef — room name (e.g. "rm_TitleScreen").
      - id: caption
        type: u4
        doc: StringRef — room display caption (shown in window title in GMS1; often empty).
      - id: width
        type: u4
        doc: Room width in pixels.
      - id: height
        type: u4
        doc: Room height in pixels.
      - id: speed
        type: u4
        doc: Room speed in frames per second (game steps per second).
      - id: persistent
        type: u4
        doc: Non-zero if the room persists (instances are kept when leaving and returning).
      - id: background_color
        type: u4
        doc: Default background fill color (0x00BBGGRR packed, alpha unused).
      - id: draw_background_color
        type: u4
        doc: Non-zero if the background_color fill is drawn before other layers.
      - id: creation_code_id
        type: s4
        doc: CODE chunk index for room creation code, or -1 for none.
      - id: flags
        type: u4
        doc: Room flags bitmask (view enabled bit, etc.).
      - id: background_list_ptr
        type: u4
        doc: |
          Absolute pointer to a pointer_list of background layer structs.
          Background layer format not parsed here (varies by version).
      - id: views_list_ptr
        type: u4
        doc: |
          Absolute pointer to a pointer_list of view structs.
          View format not parsed here.
      - id: objects_list_ptr
        type: u4
        doc: Absolute pointer to a pointer_list of room_object_entry structs.
      - id: tiles_list_ptr
        type: u4
        doc: |
          Absolute pointer to a pointer_list of tile layer structs.
          Tile format not parsed here.
      - id: physics_world
        type: u4
        doc: Non-zero if Box2D physics is active in this room.
      - id: physics_top
        type: u4
      - id: physics_left
        type: u4
      - id: physics_right
        type: u4
      - id: physics_bottom
        type: u4
      - id: physics_gravity_x
        type: f4
      - id: physics_gravity_y
        type: f4
      - id: physics_pixels_to_meters
        type: f4
        doc: Scale factor: how many pixels equal one physics meter.

  room_object_entry:
    doc: |
      An object instance pre-placed in a room.
      Accessed via absolute pointer from room_entry.objects_list_ptr → pointer_list.
    seq:
      - id: x
        type: s4
        doc: X position in room coordinates.
      - id: y
        type: s4
        doc: Y position in room coordinates.
      - id: object_id
        type: s4
        doc: OBJT chunk index of the object type to instantiate.
      - id: instance_id
        type: u4
        doc: Unique instance ID assigned at compile time (unique across the entire data.win).
      - id: creation_code_id
        type: s4
        doc: CODE chunk index for per-instance creation code, or -1 for none.
      - id: scale_x
        type: f4
        doc: Horizontal scale factor (1.0 = original size).
      - id: scale_y
        type: f4
        doc: Vertical scale factor (1.0 = original size).
      - id: color
        type: u4
        doc: Instance blend color (0xAABBGGRR packed).
      - id: rotation
        type: f4
        doc: Rotation angle in degrees (counter-clockwise).

  # ---------------------------------------------------------------------------
  # SPRT — Sprite definitions
  # ---------------------------------------------------------------------------

  sprt_body:
    doc: Sprite asset metadata (dimensions, bounding boxes, per-frame texture refs).
    seq:
      - id: sprites
        type: pointer_list
        doc: count + count × absolute pointers to sprite_entry structs.

  sprite_entry:
    seq:
      - id: name
        type: u4
        doc: StringRef — sprite name (e.g. "spr_Player").
      - id: width
        type: u4
        doc: Sprite width in pixels.
      - id: height
        type: u4
        doc: Sprite height in pixels.
      - id: bbox_left
        type: s4
        doc: Bounding box left edge (pixels from sprite origin).
      - id: bbox_right
        type: s4
        doc: Bounding box right edge.
      - id: bbox_bottom
        type: s4
        doc: Bounding box bottom edge.
      - id: bbox_top
        type: s4
        doc: Bounding box top edge.
      - id: transparent
        type: u4
        doc: Non-zero if the sprite uses a transparent colour key.
      - id: smooth
        type: u4
        doc: Non-zero if edge smoothing is applied.
      - id: preload
        type: u4
        doc: Non-zero if the sprite texture is preloaded at room start.
      - id: bbox_mode
        type: u4
        doc: Bounding box mode — 0=automatic, 1=full image, 2=manual.
      - id: sep_masks
        type: u4
        doc: Collision mask mode — 0=precise, 1=rectangle, 2=rotated rectangle, 3=diamond.
      - id: origin_x
        type: s4
        doc: Horizontal origin (pivot) point in pixels from sprite left.
      - id: origin_y
        type: s4
        doc: Vertical origin (pivot) point in pixels from sprite top.
      - id: tpag_count
        type: s4
        doc: |
          Number of animation frames. -1 means no TPAG entries
          (used by GMS2 skeletal/spine sprites that have no atlas frames).
      - id: tpag_ptrs
        type: u4
        repeat: expr
        repeat-expr: tpag_count
        if: tpag_count >= 0
        doc: |
          Absolute file offsets of texture_page_item structs in TPAG,
          one per animation frame (frame 0, frame 1, ...).

  # ---------------------------------------------------------------------------
  # SOND — Sound definitions
  # ---------------------------------------------------------------------------

  sond_body:
    doc: Sound asset metadata (file references, volume, audio group assignment).
    seq:
      - id: sounds
        type: pointer_list
        doc: count + count × absolute pointers to sound_entry structs.

  sound_entry:
    seq:
      - id: name
        type: u4
        doc: StringRef — sound name (e.g. "snd_Jump").
      - id: flags
        type: u4
        doc: |
          Audio flags bitmask:
            bit 0 (0x01): is_embedded — audio data stored in AUDO chunk
            bit 1 (0x02): is_compressed — OGG/MP3 rather than uncompressed WAV
            bit 2 (0x04): is_streamed — loaded from disk at runtime
            bit 7 (0x80): preload
          Common value: 0x64 (100) = standard embedded uncompressed audio.
      - id: type_name
        type: u4
        doc: StringRef — file extension (e.g. ".ogg", ".wav", ".mp3").
      - id: file_name
        type: u4
        doc: StringRef — audio file name without path.
      - id: effects
        type: u4
        doc: Audio effects bitmask (chorus, echo, flanger, etc.).
      - id: volume
        type: f4
        doc: Playback volume multiplier (0.0–1.0).
      - id: pitch
        type: f4
        doc: Pitch adjustment multiplier (1.0 = original pitch).
      - id: group_id
        type: s4
        doc: Audio group index for streaming/memory management, or -1 for default group.
      - id: audio_id
        type: s4
        doc: |
          Index into the AUDO chunk for embedded audio, or -1 if external
          (audio data stored in a separate file, not in data.win).

  # ---------------------------------------------------------------------------
  # BGND — Background / tileset definitions
  # ---------------------------------------------------------------------------

  bgnd_body:
    doc: |
      Background and tileset asset metadata.
        GMS1: standard background images (referenced in room background layers).
        GMS2: tileset definitions (tile dimensions, border padding, tile count, etc.).

      Only the name field is described here; the remaining tileset geometry
      data in GMS2 varies by IDE version and is not fully parsed.
    seq:
      - id: backgrounds
        type: pointer_list
        doc: count + count × absolute pointers to bgnd_entry structs.

  bgnd_entry:
    doc: Background/tileset entry (name + optional extended GMS2 tileset data).
    seq:
      - id: name
        type: u4
        doc: StringRef — background/tileset name (e.g. "bg_Sky", "tile_WoodFloor").
      - id: extended_data
        size-eos: true
        doc: |
          GMS2 tileset geometry data (tile width/height, border sizes, tile count,
          texture page item pointer, etc.). Layout varies by IDE version.
          Treat as opaque unless parsing a specific GMS2 version.

  # ---------------------------------------------------------------------------
  # FONT — Font definitions
  # ---------------------------------------------------------------------------

  font_body:
    doc: Font asset definitions with per-glyph texture atlas coordinates.
    seq:
      - id: fonts
        type: pointer_list
        doc: count + count × absolute pointers to font_entry structs.

  font_entry:
    seq:
      - id: name
        type: u4
        doc: StringRef — internal code name (e.g. "fnt_Arial14").
      - id: display_name
        type: u4
        doc: StringRef — font family display name (e.g. "Arial").
      - id: size
        type: u4
        doc: Font point size.
      - id: bold
        type: u4
        doc: Non-zero if bold style.
      - id: italic
        type: u4
        doc: Non-zero if italic style.
      - id: range_start
        type: u2
        doc: First Unicode code point included in this font's glyph set.
      - id: charset
        type: u1
        doc: Windows GDI character set / codepage identifier.
      - id: antialias
        type: u1
        doc: Anti-aliasing level (0 = none, 1–3 = increasing smoothness).
      - id: range_end
        type: u4
        doc: Last Unicode code point included in this font's glyph set.
      - id: tpag_ptr
        type: u4
        doc: Absolute file offset of the texture_page_item for this font's atlas texture.
      - id: scale_x
        type: f4
        doc: Horizontal scale factor applied when rendering (typically 1.0).
      - id: scale_y
        type: f4
        doc: Vertical scale factor applied when rendering (typically 1.0).
      - id: glyphs
        type: pointer_list
        doc: count + count × absolute pointers to glyph structs (14 bytes each).

  glyph:
    doc: Per-character rendering data. Located via absolute pointer from font_entry.glyphs.
    seq:
      - id: character
        type: u2
        doc: Unicode code point.
      - id: x
        type: u2
        doc: Source X offset on the font texture atlas.
      - id: y
        type: u2
        doc: Source Y offset on the font texture atlas.
      - id: width
        type: u2
        doc: Glyph width on the atlas in pixels.
      - id: height
        type: u2
        doc: Glyph height on the atlas in pixels.
      - id: shift
        type: s2
        doc: Horizontal shift when rendering (kerning adjustment in pixels).
      - id: advance
        type: s2
        doc: Horizontal advance after rendering this glyph (pen advance in pixels).

  # ---------------------------------------------------------------------------
  # TPAG — Texture page items (atlas sub-regions)
  # ---------------------------------------------------------------------------

  tpag_body:
    doc: |
      Texture page items: rectangular sub-regions on texture atlas pages.
      Each sprite frame, font glyph, background tile, etc. maps to one entry
      describing its source location on a TXTR atlas page and how to blit it.
    seq:
      - id: items
        type: pointer_list
        doc: count + count × absolute pointers to texture_page_item structs (22 bytes each).

  texture_page_item:
    doc: |
      A single 22-byte rectangular region on a texture atlas.
      Accessed via absolute pointer from tpag_body.items.
    seq:
      - id: source_x
        type: u2
        doc: X offset of this region on the source texture atlas page.
      - id: source_y
        type: u2
        doc: Y offset of this region on the source texture atlas page.
      - id: source_width
        type: u2
        doc: Width of this region on the source texture atlas.
      - id: source_height
        type: u2
        doc: Height of this region on the source texture atlas.
      - id: target_x
        type: u2
        doc: X offset when blitting to the destination (for cropped sprites, > 0 means left edge was trimmed).
      - id: target_y
        type: u2
        doc: Y offset when blitting to the destination.
      - id: target_width
        type: u2
        doc: Bounding width of the original un-cropped sprite.
      - id: target_height
        type: u2
        doc: Bounding height of the original un-cropped sprite.
      - id: render_width
        type: u2
        doc: Rendered width (equals target_width for most sprites).
      - id: render_height
        type: u2
        doc: Rendered height.
      - id: texture_page_id
        type: u2
        doc: Index into the TXTR chunk identifying which atlas page this region is on.

  # ---------------------------------------------------------------------------
  # TXTR — Texture atlas pages
  # ---------------------------------------------------------------------------

  txtr_body:
    doc: |
      Texture atlas pages. Each entry points to raw image data (typically PNG,
      or QOI in GMS2023.4+) embedded in data.win.

      GMS2+ games may use external textures: data_offset points into an external
      .png file rather than into data.win. In that case data_offset is 0 or
      points past the file end; treat as absent and load externally.

      Entry layout differs by version — detect by pointer spacing:
        GMS1: (ptr[1] - ptr[0]) <= 12 → txtr_entry_gms1 (8 bytes)
        GMS2: (ptr[1] - ptr[0]) > 12  → txtr_entry_gms2 (28 bytes)
      For single-entry files, default to the simpler GMS1 layout unless
      bytecode_version >= 17.
    seq:
      - id: textures
        type: pointer_list
        doc: count + count × absolute pointers to texture entries.

  txtr_entry_gms1:
    doc: GMS1 texture entry (8 bytes at pointer location).
    seq:
      - id: unknown
        type: u4
      - id: data_offset
        type: u4
        doc: Absolute file offset of the PNG image data.

  txtr_entry_gms2:
    doc: GMS2 texture entry (28 bytes at pointer location).
    seq:
      - id: unknown0
        type: u4
      - id: unknown1
        type: u4
      - id: scaled
        type: u4
        doc: Non-zero if the texture was scaled down during compilation.
      - id: generated
        type: u4
        doc: Texture generation sequence counter.
      - id: unknown2
        type: u4
      - id: width_or_zero
        type: u4
        doc: Texture width in pixels; may be zero in some versions.
      - id: data_offset
        type: u4
        doc: |
          Absolute file offset of the PNG image data in data.win,
          or 0 for external textures (stored in a separate .png file).

  # ---------------------------------------------------------------------------
  # AUDO — Embedded audio data
  # ---------------------------------------------------------------------------

  audo_body:
    doc: |
      Embedded audio files (WAV, OGG, MP3). Indexed by SOND.audio_id.
      Sounds with audio_id == -1 are external (streamed from a file on disk).
    seq:
      - id: entries
        type: pointer_list
        doc: count + count × absolute pointers to audo_entry structs.

  audo_entry:
    doc: A single embedded audio file (length-prefixed byte blob).
    seq:
      - id: length
        type: u4
        doc: Length of the audio data in bytes.
      - id: data
        size: length
        doc: |
          Raw audio file bytes. Format (WAV/OGG/MP3) determined by
          the corresponding SOND entry's flags and type_name fields.

  # ---------------------------------------------------------------------------
  # SHDR — Shader definitions
  # ---------------------------------------------------------------------------

  shdr_body:
    doc: |
      Shader asset definitions. Each entry stores a name followed by
      GLSL vertex and fragment shader source strings (and HLSL equivalents
      for Windows targets). The source string layout varies by GM version
      and is not fully parsed here.
    seq:
      - id: shaders
        type: pointer_list
        doc: count + count × absolute pointers to shdr_entry structs.

  shdr_entry:
    doc: Shader entry (name + inline source strings, not fully parsed).
    seq:
      - id: name
        type: u4
        doc: StringRef — shader asset name (e.g. "shd_Edge").
      - id: shader_source
        size-eos: true
        doc: |
          Shader source strings (GLSL vertex + fragment, HLSL vertex + fragment).
          Stored as inline gm_strings (length-prefixed). Layout varies by GM
          version; treat as opaque unless parsing a specific target.

  # ---------------------------------------------------------------------------
  # SEQN — Sequence definitions (GMS2.3+ only)
  # ---------------------------------------------------------------------------

  seqn_body:
    doc: |
      Animation sequence asset definitions. Present only in GMS2.3+ games
      (ide_version_major >= 2 from GEN8).

      CRITICAL: Unlike every other chunk, SEQN has a 4-byte version field
      BEFORE the standard count+pointer list. This is unique to SEQN.

      Full sequence data (keyframes, tracks, playback settings, embedded curves)
      follows each name entry but is not parsed here.
    seq:
      - id: version
        type: u4
        doc: |
          SEQN format version (e.g. 1). This is NOT the GEN8 bytecode_version.
          Unique to SEQN: no other chunk type has this prefix field.
      - id: sequences
        type: pointer_list
        doc: count + count × absolute pointers to seqn_entry structs.

  seqn_entry:
    doc: Sequence entry (name + opaque keyframe/track data).
    seq:
      - id: name
        type: u4
        doc: StringRef — sequence asset name (e.g. "sqIntro", "sqCutscene").
      - id: sequence_data
        size-eos: true
        doc: |
          Full sequence definition: playback mode, length, origin, tracks,
          keyframes, embedded animation curves, etc.
          Format varies by SEQN version field above. Treat as opaque.

  # ---------------------------------------------------------------------------
  # OPTN — Game options and project constants
  # ---------------------------------------------------------------------------

  optn_body:
    doc: |
      Game options flags and named compile-time constants.
      The constant list starts at a fixed offset: 60 bytes from the start
      of the chunk body (after flags + 56 bytes of reserved option data).
    seq:
      - id: flags
        type: u4
        doc: Game option flags bitmask (fullscreen, sync to monitor, interpolation, etc.).
      - id: reserved
        size: 56
        doc: |
          Additional option fields (window dimensions, splash screen settings,
          platform-specific flags, etc.). Layout varies by GM version.
          Not parsed here.
      - id: constant_count
        type: u4
        doc: Number of named project constants defined in the IDE.
      - id: constants
        type: option_constant
        repeat: expr
        repeat-expr: constant_count

  option_constant:
    doc: A named compile-time project constant (equivalent to a #define).
    seq:
      - id: name
        type: u4
        doc: StringRef — constant name.
      - id: value
        type: u4
        doc: StringRef — constant value as a string.

  # ---------------------------------------------------------------------------
  # GLOB — Global script IDs (bytecode_version >= 16 only)
  # ---------------------------------------------------------------------------

  glob_body:
    doc: |
      List of CODE chunk indices for global init scripts — scripts that execute
      at game startup before the first room loads.
      Present only when bytecode_version >= 16.
    seq:
      - id: count
        type: u4
      - id: script_ids
        type: u4
        repeat: expr
        repeat-expr: count
        doc: CODE chunk entry indices, in execution order.

  # ---------------------------------------------------------------------------
  # LANG — Language / localization settings (bytecode_version >= 16 only)
  # ---------------------------------------------------------------------------

  lang_body:
    doc: |
      Language and localization configuration.
      Present only when bytecode_version >= 16.
    seq:
      - id: entry_count
        type: u4
        doc: Declared entry count header (may differ from actual_count).
      - id: actual_count
        type: u4
        doc: Actual number of lang_entry structs that follow.
      - id: entries
        type: lang_entry
        repeat: expr
        repeat-expr: actual_count

  lang_entry:
    seq:
      - id: name
        type: u4
        doc: StringRef — language name (e.g. "English", "French").
      - id: region
        type: u4
        doc: StringRef — region/locale code (e.g. "en-GB", "fr-FR").
