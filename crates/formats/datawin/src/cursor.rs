use crate::error::{Error, Result};

/// Read cursor over a byte slice. All reads are little-endian.
#[derive(Clone)]
pub struct Cursor<'a> {
    data: &'a [u8],
    pos: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self { data, pos: 0 }
    }

    /// Current byte position.
    pub fn position(&self) -> usize {
        self.pos
    }

    /// Total length of underlying data.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Whether we've reached the end.
    pub fn is_empty(&self) -> bool {
        self.pos >= self.data.len()
    }

    /// Remaining bytes from current position.
    pub fn remaining(&self) -> usize {
        self.data.len().saturating_sub(self.pos)
    }

    /// Seek to an absolute position.
    pub fn seek(&mut self, pos: usize) {
        self.pos = pos;
    }

    /// Skip `n` bytes forward.
    pub fn skip(&mut self, n: usize) -> Result<()> {
        self.ensure(n)?;
        self.pos += n;
        Ok(())
    }

    /// Read a slice of `n` bytes without copying.
    pub fn read_bytes(&mut self, n: usize) -> Result<&'a [u8]> {
        self.ensure(n)?;
        let slice = &self.data[self.pos..self.pos + n];
        self.pos += n;
        Ok(slice)
    }

    /// Read a 4-byte magic/tag.
    pub fn read_magic(&mut self) -> Result<[u8; 4]> {
        let bytes = self.read_bytes(4)?;
        let mut magic = [0u8; 4];
        magic.copy_from_slice(bytes);
        Ok(magic)
    }

    pub fn read_u8(&mut self) -> Result<u8> {
        self.ensure(1)?;
        let v = self.data[self.pos];
        self.pos += 1;
        Ok(v)
    }

    pub fn read_i16(&mut self) -> Result<i16> {
        let bytes = self.read_bytes(2)?;
        Ok(i16::from_le_bytes([bytes[0], bytes[1]]))
    }

    pub fn read_u16(&mut self) -> Result<u16> {
        let bytes = self.read_bytes(2)?;
        Ok(u16::from_le_bytes([bytes[0], bytes[1]]))
    }

    pub fn read_i32(&mut self) -> Result<i32> {
        let bytes = self.read_bytes(4)?;
        Ok(i32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }

    pub fn read_u32(&mut self) -> Result<u32> {
        let bytes = self.read_bytes(4)?;
        Ok(u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }

    pub fn read_i64(&mut self) -> Result<i64> {
        let bytes = self.read_bytes(8)?;
        Ok(i64::from_le_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ]))
    }

    pub fn read_u64(&mut self) -> Result<u64> {
        let bytes = self.read_bytes(8)?;
        Ok(u64::from_le_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ]))
    }

    pub fn read_f32(&mut self) -> Result<f32> {
        let bytes = self.read_bytes(4)?;
        Ok(f32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }

    pub fn read_f64(&mut self) -> Result<f64> {
        let bytes = self.read_bytes(8)?;
        Ok(f64::from_le_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ]))
    }

    /// Read a GameMaker string at the current position: u32 length + bytes + null terminator.
    pub fn read_gm_string(&mut self) -> Result<String> {
        let offset = self.pos;
        let len = self.read_u32()? as usize;
        let bytes = self.read_bytes(len)?;
        // Skip null terminator
        self.skip(1)?;
        String::from_utf8(bytes.to_vec()).map_err(|e| Error::InvalidString {
            offset,
            source: e,
        })
    }

    /// Read a pointer list: u32 count, then count × u32 absolute offsets.
    pub fn read_pointer_list(&mut self) -> Result<Vec<u32>> {
        let count = self.read_u32()? as usize;
        let mut offsets = Vec::with_capacity(count);
        for _ in 0..count {
            offsets.push(self.read_u32()?);
        }
        Ok(offsets)
    }

    /// Access the full underlying data (for absolute offset reads).
    pub fn data(&self) -> &'a [u8] {
        self.data
    }

    /// Create a cursor positioned at an absolute offset into the same data.
    pub fn at_offset(&self, offset: usize) -> Self {
        Self {
            data: self.data,
            pos: offset,
        }
    }

    fn ensure(&self, n: usize) -> Result<()> {
        if self.pos + n > self.data.len() {
            return Err(Error::UnexpectedEof {
                offset: self.pos,
                need: n,
                have: self.remaining(),
            });
        }
        Ok(())
    }
}

/// Writer that builds a byte buffer. All writes are little-endian.
pub struct Writer {
    buf: Vec<u8>,
}

impl Writer {
    pub fn new() -> Self {
        Self { buf: Vec::new() }
    }

    pub fn with_capacity(cap: usize) -> Self {
        Self {
            buf: Vec::with_capacity(cap),
        }
    }

    pub fn position(&self) -> usize {
        self.buf.len()
    }

    pub fn write_bytes(&mut self, bytes: &[u8]) {
        self.buf.extend_from_slice(bytes);
    }

    pub fn write_magic(&mut self, magic: &[u8; 4]) {
        self.buf.extend_from_slice(magic);
    }

    pub fn write_u8(&mut self, v: u8) {
        self.buf.push(v);
    }

    pub fn write_u16(&mut self, v: u16) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }

    pub fn write_i32(&mut self, v: i32) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }

    pub fn write_u32(&mut self, v: u32) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }

    pub fn write_u64(&mut self, v: u64) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }

    pub fn write_f32(&mut self, v: f32) {
        self.buf.extend_from_slice(&v.to_le_bytes());
    }

    /// Write a GameMaker string: u32 length + bytes + null terminator.
    pub fn write_gm_string(&mut self, s: &str) {
        self.write_u32(s.len() as u32);
        self.buf.extend_from_slice(s.as_bytes());
        self.buf.push(0);
    }

    /// Patch a u32 at a specific position (for backpatching sizes).
    pub fn patch_u32(&mut self, pos: usize, v: u32) {
        let bytes = v.to_le_bytes();
        self.buf[pos..pos + 4].copy_from_slice(&bytes);
    }

    /// Pad to 4-byte alignment.
    pub fn align4(&mut self) {
        while !self.buf.len().is_multiple_of(4) {
            self.buf.push(0);
        }
    }

    pub fn into_bytes(self) -> Vec<u8> {
        self.buf
    }
}

impl Default for Writer {
    fn default() -> Self {
        Self::new()
    }
}
