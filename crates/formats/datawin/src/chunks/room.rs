use crate::cursor::Cursor;
use crate::error::Result;
use crate::string_table::StringRef;

/// An object instance placed in a room.
#[derive(Debug, Clone)]
pub struct RoomObject {
    /// X position in the room.
    pub x: i32,
    /// Y position in the room.
    pub y: i32,
    /// Index into the OBJT chunk.
    pub object_id: i32,
    /// Instance ID (unique per data.win).
    pub instance_id: u32,
    /// Index into CODE chunk for instance creation code, or -1.
    pub creation_code_id: i32,
    /// Horizontal scale.
    pub scale_x: f32,
    /// Vertical scale.
    pub scale_y: f32,
    /// Blend color.
    pub color: u32,
    /// Rotation in degrees.
    pub rotation: f32,
}

/// A room entry in the ROOM chunk.
#[derive(Debug)]
pub struct RoomEntry {
    /// Reference to the room name string.
    pub name: StringRef,
    /// Reference to the room caption string.
    pub caption: StringRef,
    /// Room width in pixels.
    pub width: u32,
    /// Room height in pixels.
    pub height: u32,
    /// Room speed (frames per second or microseconds per frame).
    pub speed: u32,
    /// Whether the room is persistent.
    pub persistent: bool,
    /// Background color.
    pub background_color: u32,
    /// Whether to draw the background color.
    pub draw_background_color: bool,
    /// Creation code entry index into the CODE chunk, or -1.
    pub creation_code_id: i32,
    /// Room flags.
    pub flags: u32,
    /// Object instances placed in the room.
    pub objects: Vec<RoomObject>,
    /// Physics world properties.
    pub physics_world: bool,
    pub physics_gravity_x: f32,
    pub physics_gravity_y: f32,
    pub physics_pixels_to_meters: f32,
}

/// Parsed ROOM chunk.
#[derive(Debug)]
pub struct Room {
    /// Room entries.
    pub rooms: Vec<RoomEntry>,
}

impl Room {
    /// Parse the ROOM chunk.
    ///
    /// `chunk_data` is the raw chunk content (after the 8-byte header).
    /// `data` is the full file data (for following absolute pointers).
    pub fn parse(chunk_data: &[u8], data: &[u8]) -> Result<Self> {
        let mut c = Cursor::new(chunk_data);
        let pointers = c.read_pointer_list()?;

        let mut rooms = Vec::with_capacity(pointers.len());
        for ptr in pointers {
            let room = Self::parse_room(data, ptr as usize)?;
            rooms.push(room);
        }

        Ok(Self { rooms })
    }

    fn parse_room_objects(data: &[u8], offset: usize) -> Result<Vec<RoomObject>> {
        let mut c = Cursor::new(data);
        c.seek(offset);
        let pointers = c.read_pointer_list()?;

        let mut objects = Vec::with_capacity(pointers.len());
        for ptr in pointers {
            let mut oc = Cursor::new(data);
            oc.seek(ptr as usize);

            let x = oc.read_i32()?;
            let y = oc.read_i32()?;
            let object_id = oc.read_i32()?;
            let instance_id = oc.read_u32()?;
            let creation_code_id = oc.read_i32()?;
            let scale_x = oc.read_f32()?;
            let scale_y = oc.read_f32()?;
            let color = oc.read_u32()?;
            let rotation = oc.read_f32()?;

            objects.push(RoomObject {
                x,
                y,
                object_id,
                instance_id,
                creation_code_id,
                scale_x,
                scale_y,
                color,
                rotation,
            });
        }

        Ok(objects)
    }

    fn parse_room(data: &[u8], offset: usize) -> Result<RoomEntry> {
        let mut c = Cursor::new(data);
        c.seek(offset);

        let name = StringRef(c.read_u32()?);
        let caption = StringRef(c.read_u32()?);
        let width = c.read_u32()?;
        let height = c.read_u32()?;
        let speed = c.read_u32()?;
        let persistent = c.read_u32()? != 0;
        let background_color = c.read_u32()?;
        let draw_background_color = c.read_u32()? != 0;
        let creation_code_id = c.read_i32()?;
        let flags = c.read_u32()?;

        // Sub-list pointers
        let _bg_ptr = c.read_u32()?;
        let _views_ptr = c.read_u32()?;
        let objs_ptr = c.read_u32()?;
        let _tiles_ptr = c.read_u32()?;

        let physics_world = c.read_u32()? != 0;
        let _physics_top = c.read_u32()?;
        let _physics_left = c.read_u32()?;
        let _physics_right = c.read_u32()?;
        let _physics_bottom = c.read_u32()?;
        let physics_gravity_x = c.read_f32()?;
        let physics_gravity_y = c.read_f32()?;
        let physics_pixels_to_meters = c.read_f32()?;

        // Parse object instances sub-list.
        let objects = Self::parse_room_objects(data, objs_ptr as usize)?;

        Ok(RoomEntry {
            name,
            caption,
            width,
            height,
            speed,
            persistent,
            background_color,
            draw_background_color,
            creation_code_id,
            flags,
            objects,
            physics_world,
            physics_gravity_x,
            physics_gravity_y,
            physics_pixels_to_meters,
        })
    }
}
