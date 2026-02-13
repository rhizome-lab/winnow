use datawin::reader::ChunkIndex;

fn load_if_exists(path: &str) -> Option<Vec<u8>> {
    std::fs::read(path).ok()
}

#[test]
fn parse_bounty_chunks() {
    let Some(data) = load_if_exists(&format!("{}/Bounty/data.win", env!("HOME"))) else {
        eprintln!("skipping: Bounty/data.win not found");
        return;
    };
    let index = ChunkIndex::parse(&data).expect("failed to parse Bounty data.win");
    assert_eq!(index.len(), 22);

    // Verify expected chunk order
    let magics: Vec<&str> = index.chunks().iter().map(|c| c.magic_str()).collect();
    assert_eq!(
        magics,
        [
            "GEN8", "OPTN", "EXTN", "SOND", "AGRP", "SPRT", "BGND", "PATH", "SCPT", "SHDR",
            "FONT", "TMLN", "OBJT", "ROOM", "DAFL", "TPAG", "CODE", "VARI", "FUNC", "STRG",
            "TXTR", "AUDO",
        ]
    );

    // Verify GEN8 is first and has correct size
    let gen8 = index.find(b"GEN8").expect("GEN8 not found");
    assert_eq!(gen8.offset, 8);
    assert_eq!(gen8.size, 252);
}

#[test]
fn parse_undertale_chunks() {
    let Some(data) = load_if_exists(
        "/mnt/ssd/steam/steamapps/common/Undertale/assets/game.unx",
    ) else {
        eprintln!("skipping: Undertale game.unx not found");
        return;
    };
    let index = ChunkIndex::parse(&data).expect("failed to parse Undertale game.unx");
    assert_eq!(index.len(), 24);

    // Verify LANG and GLOB are present (v16 feature)
    assert!(index.find(b"LANG").is_some());
    assert!(index.find(b"GLOB").is_some());
}

#[test]
fn parse_chronicon_chunks() {
    let Some(data) = load_if_exists(
        "/mnt/ssd/steam/steamapps/common/Chronicon/data.win",
    ) else {
        eprintln!("skipping: Chronicon data.win not found");
        return;
    };
    let index = ChunkIndex::parse(&data).expect("failed to parse Chronicon data.win");

    // Chronicon has 31 chunks including GMS2-specific ones
    assert_eq!(index.len(), 31);

    // Verify some GMS2 chunks are present
    assert!(index.find(b"TGIN").is_some());
    assert!(index.find(b"FEAT").is_some());
    assert!(index.find(b"FEDS").is_some());
    assert!(index.find(b"EMBI").is_some());

    // Chronicon has no CODE chunk (GMS2 VM-less export)
    assert!(index.find(b"CODE").is_none());
}

#[test]
fn chunk_data_extraction() {
    let Some(data) = load_if_exists(&format!("{}/Bounty/data.win", env!("HOME"))) else {
        eprintln!("skipping: Bounty/data.win not found");
        return;
    };
    let index = ChunkIndex::parse(&data).expect("failed to parse");

    // Extract GEN8 chunk data
    let gen8_data = index.chunk_data(&data, b"GEN8").expect("GEN8 data");
    assert_eq!(gen8_data.len(), 252);

    // First byte should be debug flag (1), second byte bytecode version (15)
    assert_eq!(gen8_data[0], 1); // debug
    assert_eq!(gen8_data[1], 15); // bytecode version
}
