meta:
  id: gml_bytecode
  title: GameMaker Language (GML) VM Bytecode
  application: GameMaker Studio / GameMaker Studio 2
  license: CC0-1.0
  ks-version: 0.11
  endian: le
doc: |
  GML virtual machine bytecode instruction stream.

  Each instruction is at minimum 4 bytes (one "word"). Some instructions consume
  one or two additional 4-byte words for the operand value.

  Instruction word layout (32 bits, little-endian):
    bits 31-24  opcode (u8)
    bits 23-20  type2  (4-bit DataType, interpretation depends on opcode)
    bits 19-16  type1  (4-bit DataType, interpretation depends on opcode)
    bits 15-0   val16  (16-bit field, interpretation depends on opcode)

  Two instruction encodings exist, distinguished by `GEN8.bytecode_version`:
    v14 (BC <= 14): old opcode values (see enum opcode_v14).
    v15+ (BC >= 15): new opcode values used here (see enum opcode).

  v14 games also lack the Call operand word and use slightly different field
  layouts; see the v14 notes in each instruction type.

  This file describes the v15+ encoding. The `gml_instruction` sequence type
  expects the stream to start at the beginning of a code entry's bytecode blob
  and continue until the entry's byte length is exhausted.

  Bytecode location in data.win:
    - code_entry_v14: bytecode follows the 12-byte header immediately.
    - code_entry_v15+: bytecode is at (entry_header_addr + offset_in_blob) within
      the shared blob. See game_maker_data.ksy for pointer/offset arithmetic.

params:
  - id: bytecode_version
    type: u4
    doc: |
      GEN8.bytecode_version from the containing data.win.
      14 = GMS 1.x old encoding; 15+ = new encoding.
      Used to select opcode decoding and Call operand presence.

seq:
  - id: instructions
    type: gml_instruction
    repeat: eos

types:

  gml_instruction:
    doc: |
      One GML VM instruction. Always starts on a 4-byte boundary.
      The instruction word is followed by 0, 1, or 2 extra words depending on
      the opcode and type fields.
    seq:
      - id: word
        type: u4
        doc: The packed instruction word (opcode | type2 | type1 | val16).
      - id: body
        type:
          switch-on: opcode
          cases:
            'opcode::push':    push_body
            'opcode::push_loc': push_body
            'opcode::push_glb': push_body
            'opcode::push_bltn': push_body
            'opcode::push_i':  push_i_body
            'opcode::pop':     pop_body
            'opcode::call':    call_body
            'opcode::dup':     dup_body
            'opcode::brk':     break_body
            _:                 empty_body
        doc: Extra operand words, if any. Type depends on opcode.

    instances:
      opcode:
        value: '(word >> 24) & 0xFF'
        enum: opcode
        doc: High byte of the instruction word.
      type1:
        value: '(word >> 16) & 0xF'
        enum: data_type
        doc: |
          Lower type nibble (bits 19-16).
          For most instructions: the type of the source/primary operand.
          For branch instructions: bits 19-16 are part of the 23-bit offset, not a type.
      type2:
        value: '(word >> 20) & 0xF'
        enum: data_type
        doc: |
          Upper type nibble (bits 23-20).
          For two-operand arithmetic: destination type (Conv, arithmetic ops).
          For branch instructions: bits 23-20 are part of the 23-bit offset, not a type.
          Unused (0) for most other instructions.
      val16:
        value: 'word & 0xFFFF'
        doc: Low 16 bits of the instruction word. Interpretation depends on opcode.
      branch_offset_raw:
        value: 'word & 0x007FFFFF'
        doc: |
          23-bit raw branch offset (bits 22-0).
          Only meaningful for B / Bt / Bf / PushEnv / PopEnv.
          Sign-extend from bit 22 to obtain a signed offset in 4-byte units.
          Multiply by 4 for byte offset from the start of this instruction.
      cmp_kind:
        value: '(word >> 8) & 0xFF'
        enum: comparison_kind
        doc: |
          Comparison kind byte (bits 15-8).
          Only meaningful for Cmp; bits 15-8 encode the operator.

  push_body:
    doc: |
      Extra operand word(s) for Push / PushLoc / PushGlb / PushBltn.
      The number of words consumed and their interpretation is determined by
      `type1` from the enclosing instruction:
        Double (0x0): 8 bytes (two u32 words) — IEEE 754 f64, little-endian.
        Float  (0x1): 4 bytes (one u32 word)  — IEEE 754 f32, little-endian.
        Int32  (0x2): 4 bytes (one u32 word)  — signed 32-bit integer.
        Int64  (0x3): 8 bytes (two u32 words) — signed 64-bit integer, little-endian.
        Bool   (0x4): 4 bytes (one u32 word)  — boolean (0 = false, non-zero = true).
        String (0x6): 4 bytes (one u32 word)  — absolute file offset of a gm_string.
        Variable (0x5): 4 bytes (one u32 word) — variable_ref; val16 = instance (i16).
        Int16  (0xF): 0 bytes — value is inline in val16 (signed 16-bit).
      NOTE: This type cannot be expressed directly in Kaitai as a conditional-length
      sequence without using `if` expressions. Implementations should branch on
      type1 from the parent instruction word:
        - Double/Int64: read 8 bytes
        - Float/Int32/Bool/String: read 4 bytes
        - Variable: read 4 bytes (the variable_ref word; instance is in val16)
        - Int16: read 0 bytes
    seq:
      - id: raw
        size: 0
        doc: |
          Placeholder — actual parsing is opcode+type1 conditional.
          See parent instance `type1` and the push_body doc for the branching logic.

  push_i_body:
    doc: |
      Extra operand for PushI.
        Int16 (0xF): 0 bytes — value is inline in val16.
        Int32 (0x2): 4 bytes — one signed 32-bit word follows.
        Other: treated as Int16 (inline).
    seq:
      - id: raw
        size: 0
        doc: Placeholder — see push_i_body doc.

  pop_body:
    doc: |
      Extra operand for Pop (variable write).
      val16 of the instruction word = instance (i16).
      The following 4-byte word is a variable_ref.
    seq:
      - id: var_ref
        type: variable_ref
        doc: Variable reference (target of the write).

  call_body:
    doc: |
      Extra operand for Call (direct function call).
      val16 = argument count.
      The following 4-byte word is the FUNC-table index of the callee.
      In BC <= 16, this word points to the instruction itself (absolute offset).
      In BC >= 17, this word points to the operand word (subtract 4 for instruction).
      Reincarnate's decoder normalises this to a zero-based function index.
    seq:
      - id: function_id
        type: u4
        doc: |
          Packed function reference.
          Bits 23-0: zero-based index into the FUNC chunk entry list.
          Bits 31-24: upper byte (reference linkage; irrelevant after patching).

  dup_body:
    doc: |
      Dup instruction — duplicates bytes on the value stack.

      Standard mode (dup_extra == 0):
        Copies (val8 + 1) * sizeof(type1) BYTES from the top of the stack.
        "Copies" means push N additional copies; originals remain.

      Swap mode (GMS2.3+, dup_extra != 0, dup_size > 0):
        Reorders the top portion of the stack (used before popaf to align
        a Variable-sized value for the fixed-size popaf window).

      No-op mode (GMS2.3+, dup_extra != 0, dup_size == 0):
        Struct swap marker; has no stack effect during decompilation.

      GML stack type sizes (bytes per item):
        Variable = 16  (4 u32 units)
        Double   = 8   (2 u32 units)
        Int64    = 8   (2 u32 units)
        Int32    = 4   (1 u32 unit)
        Int16    = 4   (1 u32 unit)
        Bool     = 4   (1 u32 unit)
        String   = 4   (1 u32 unit)
    instances:
      val8:
        value: '_parent.val16 & 0xFF'
        doc: Low byte of val16 — base duplication count/size parameter.
      dup_extra:
        value: '(_parent.val16 >> 8) & 0xFF'
        doc: |
          High byte of val16 (GMS2.3+).
          0 = standard dup.
          Non-zero = swap mode or no-op (combined with dup_size).
    seq:
      - id: raw
        size: 0
        doc: Dup has no extra words; all data is in the instruction word.

  break_body:
    doc: |
      Break instruction — extended VM signals (GMS2.3+).

      The signal number is in val16 (treated as signed i16 = 0xFFFF..0x0000).
      When type1 == Int32 (0x2), one extra 4-byte word follows as the `extra` operand.

      Signal table (signal as signed i16):
        -1  (0xFFFF)  chkindex    — bounds-check array index; no stack effect.
        -2  (0xFFFE)  pushaf      — array get:  pops [index, array]; pushes value.
        -3  (0xFFFD)  popaf       — array set:  pops [value, index]; uses pushac ref.
        -4  (0xFFFC)  pushac      — capture array ref: pops array; stores for popaf.
        -5  (0xFFFB)  setowner    — pops instance ID (owner for next variable access).
        -6  (0xFFFA)  isstaticok  — pushes false (static init not yet done).
        -7  (0xFFF9)  setstatic   — enter static scope; nop for decompilation.
        -8  (0xFFF8)  savearef    — save array ref to temp; nop for decompilation.
        -9  (0xFFF7)  restorearef — restore array ref from temp; nop for decompilation.
        -10 (0xFFF6)  chknullish  — peek TOS; push bool (is TOS nullish?). Used for ?? / ?. .
        -11 (0xFFF5)  pushref     — push asset reference. extra = (type_tag << 24) | asset_index.
                                    type1 == Int32; one extra word follows.

      pushref asset type_tag values (bits 31-24 of extra):
        0  FUNC  — function index (zero-based into FUNC chunk).
        1  SPRT  — sprite index.
        2  SOND  — sound index.
        3  ROOM  — room index.
        4  PATH  — path index (deprecated in GMS2).
        5  SCPT  — script index.
        6  FONT  — font index.
        7  TMLN  — timeline index.
        8  SHDR  — shader index.
        9  SEQN  — sequence index (GMS2.3+).
    seq:
      - id: extra
        type: s4
        if: _parent.type1 == data_type::int32
        doc: |
          Extra signed 32-bit operand. Present only when type1 == Int32.
          For pushref (signal -11): extra = (type_tag << 24) | asset_index.

  empty_body:
    doc: No extra operand words for this instruction.
    seq:
      - id: raw
        size: 0

  variable_ref:
    doc: |
      A packed 32-bit variable reference word (second word of Push/Pop Variable
      instructions). This encoding is used to build a linked list that the GM
      linker uses to patch variable indices. After linking, bits 23-0 are the
      final zero-based VARI table index.
    seq:
      - id: raw
        type: u4
    instances:
      variable_id:
        value: 'raw & 0x00FFFFFF'
        doc: Zero-based index into the VARI chunk entry list.
      ref_type:
        value: '(raw >> 24) & 0xF8'
        doc: |
          Reference type flags (high 5 bits of byte 3).
          Controls array-access mode and scope resolution:
            0x00  Normal variable access (self.field or local).
            0x80  Cross-instance access (target object index in instance field).
            0xA0  Singleton access (no index pops; instance field is object ID).
          Bit patterns observed in GMS1/GMS2 data.win files:
            0x00 + has_self:  self-field (object event handlers use this for own fields).
            0x00 + instance>=0: 2D array access (two indices on stack; dim1 on top).
            0x80 + instance>=0: cross-object field access via dynamic instance.
            0xA0 + instance>=0: singleton field (no stack pops for target resolution).

enums:

  # opcode: GML VM opcode values (v15+ encoding, bytecode_version >= 15).
  # For v14 games, translate at decode time using the opcode_v14 enum.
  opcode:
    0x07: conv
    0x08: mul
    0x09: div
    0x0A: rem
    0x0B: mod
    0x0C: add
    0x0D: sub
    0x0E: and
    0x0F: or
    0x10: xor
    0x11: neg
    0x12: not
    0x13: shl
    0x14: shr
    0x15: cmp
    0x45: pop
    0x84: push_i
    0x86: dup
    0x99: call_v
    0x9C: ret
    0x9D: exit
    0x9E: popz
    0xB6: b
    0xB7: bt
    0xB8: bf
    0xBA: push_env
    0xBB: pop_env
    0xC0: push
    0xC1: push_loc
    0xC2: push_glb
    0xC3: push_bltn
    0xD9: call
    0xFF: brk

  # opcode_v14: GML VM opcode values for bytecode_version <= 14 (old encoding).
  # Translate these to opcode (v15+) before further processing:
  #   conv=0x03→0x07, mul=0x04→0x08, ..., cmp=0x11-0x16→0x15 (six separate ops).
  # The v14 encoding also lacks PushLoc/PushGlb/PushBltn/PushI/CallV variants.
  # v14 has no Call operand word; the function is implicit from the code entry.
  opcode_v14:
    0x03: conv
    0x04: mul
    0x05: div
    0x06: rem
    0x07: mod
    0x08: add
    0x09: sub
    0x0A: and
    0x0B: or
    0x0C: xor
    0x0D: neg
    0x0E: not
    0x0F: shl
    0x10: shr
    0x11: cmp_lt
    0x12: cmp_le
    0x13: cmp_eq
    0x14: cmp_ne
    0x15: cmp_ge
    0x16: cmp_gt
    0x41: pop
    0x82: dup
    0x9D: ret
    0x9E: exit
    0x9F: popz
    0xB7: b
    0xB8: bt
    0xB9: bf
    0xBB: push_env
    0xBC: pop_env
    0xC0: push
    0xDA: call
    0xFF: brk

  # data_type: 4-bit type field embedded in bits 19-16 (type1) and 23-20 (type2)
  # of the instruction word. For branch instructions these bits are
  # part of the 23-bit branch offset, not a type.
  data_type:
    0x0: double
    0x1: float
    0x2: int32
    0x3: int64
    0x4: bool
    0x5: variable
    0x6: string
    0xF: int16

  # comparison_kind: Comparison operator for Cmp instructions, encoded in bits 15-8
  # of the instruction word (the high byte of val16).
  comparison_kind:
    1: less
    2: less_equal
    3: equal
    4: not_equal
    5: greater_equal
    6: greater

  # instance_type: Signed 16-bit instance specifier, stored in val16 of Push/Pop Variable
  # instructions. Negative values are magic constants; non-negative values
  # are object asset indices (resolved at runtime to an actual instance).
  instance_type:
    -1:  own
    -2:  other
    -3:  all
    -4:  noone
    -5:  global
    -6:  builtin
    -7:  local
    -9:  stacktop
    -15: static
    -16: arg
