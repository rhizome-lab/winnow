mod assets;
mod data;
pub mod naming;
mod object;
mod translate;

use std::collections::HashMap;
use std::fs;

use datawin::DataWin;
use reincarnate_core::error::CoreError;
use reincarnate_core::ir::builder::ModuleBuilder;
use reincarnate_core::ir::func::Visibility;
use reincarnate_core::ir::module::Global;
use reincarnate_core::ir::ty::Type;
use reincarnate_core::pipeline::{Frontend, FrontendInput, FrontendOutput};
use reincarnate_core::project::EngineOrigin;

use crate::translate::TranslateCtx;

/// GameMaker frontend — translates data.win files into reincarnate IR.
pub struct GameMakerFrontend;

impl Frontend for GameMakerFrontend {
    fn supported_engines(&self) -> &[EngineOrigin] {
        &[EngineOrigin::GameMaker]
    }

    fn extract(&self, input: FrontendInput) -> Result<FrontendOutput, CoreError> {
        let data = fs::read(&input.source)?;
        let dw = DataWin::parse(data).map_err(|e| CoreError::Parse {
            file: input.source.clone(),
            message: e.to_string(),
        })?;

        let parse_err = |e: datawin::Error| CoreError::Parse {
            file: input.source.clone(),
            message: e.to_string(),
        };

        let gen8 = dw.gen8().map_err(parse_err)?;
        let game_name = dw.resolve_string(gen8.name).map_err(|e| CoreError::Parse {
            file: input.source.clone(),
            message: format!("failed to resolve game name: {e}"),
        })?;

        eprintln!("[gamemaker] extracting: {game_name}");

        let code = dw.code().map_err(parse_err)?;
        let func = dw.func().map_err(parse_err)?;
        let scpt = dw.scpt().map_err(parse_err)?;
        let vari = dw.vari().map_err(parse_err)?;
        let objt = dw.objt().map_err(parse_err)?;

        // Build function name lookup: function_id → resolved name.
        let function_names = build_function_names(&dw, func)?;

        // Build variable lookup: variable_id → (name, instance_type).
        let variables = build_variable_table(&dw, vari)?;

        // Build linked-list reference maps for correct name resolution.
        let func_ref_map = build_func_ref_map(func, dw.data());
        let vari_ref_map = build_vari_ref_map(vari, dw.data());

        // Build code_locals lookup: code entry name → CodeLocals.
        let code_locals_map = build_code_locals_map(&dw, func)?;

        // Pre-resolve object names for event naming and parent resolution.
        let obj_names = resolve_object_names(&dw, objt)?;

        let mut mb = ModuleBuilder::new(&game_name);

        // Register global variables from VARI.
        register_globals(&dw, vari, &mut mb);

        // Translate scripts.
        let (script_ok, script_err) =
            translate_scripts(&dw, code, scpt, &function_names, &variables, &func_ref_map, &vari_ref_map, &code_locals_map, &mut mb, &input)?;
        eprintln!("[gamemaker] translated {script_ok} scripts ({script_err} errors)");

        // Translate objects → ClassDefs with event handler methods.
        let (obj_ok, obj_err) = object::translate_objects(
            &dw,
            code,
            &function_names,
            &variables,
            &func_ref_map,
            &vari_ref_map,
            &code_locals_map,
            &mut mb,
            &obj_names,
        )
        .map_err(|e| CoreError::Parse {
            file: input.source.clone(),
            message: e,
        })?;
        eprintln!("[gamemaker] translated {obj_ok} event handlers ({obj_err} errors) across {} objects", obj_names.len());

        // Translate global init scripts (GLOB chunk).
        let glob_count = translate_global_inits(
            &dw, code, &function_names, &variables, &func_ref_map, &vari_ref_map, &code_locals_map, &mut mb,
        );
        if glob_count > 0 {
            eprintln!("[gamemaker] translated {glob_count} global init scripts");
        }

        // Translate room creation code.
        let room_count = translate_room_creation(
            &dw, code, &function_names, &variables, &func_ref_map, &vari_ref_map, &code_locals_map, &mut mb,
        );
        if room_count > 0 {
            eprintln!("[gamemaker] translated {room_count} room creation scripts");
        }

        // Extract assets (textures, audio).
        let mut assets = assets::extract_assets(&dw);
        if !assets.assets.is_empty() {
            eprintln!("[gamemaker] extracted {} assets", assets.assets.len());
        }

        // Generate data files (sprites, textures, fonts, rooms, objects).
        data::generate_data_files(&dw, &mut assets, &obj_names);
        eprintln!("[gamemaker] generated data files");

        let module = mb.build();

        Ok(FrontendOutput {
            modules: vec![module],
            assets,
        })
    }
}

/// Translate scripts from SCPT chunk.
#[allow(clippy::too_many_arguments)]
fn translate_scripts(
    dw: &DataWin,
    code: &datawin::chunks::code::Code,
    scpt: &datawin::chunks::scpt::Scpt,
    function_names: &HashMap<u32, String>,
    variables: &[(String, i32)],
    func_ref_map: &HashMap<usize, usize>,
    vari_ref_map: &HashMap<usize, usize>,
    code_locals_map: &HashMap<String, &datawin::chunks::func::CodeLocals>,
    mb: &mut ModuleBuilder,
    input: &FrontendInput,
) -> Result<(usize, usize), CoreError> {
    let mut translated = 0;
    let mut errors = 0;

    for script in &scpt.scripts {
        let script_name = dw.resolve_string(script.name).map_err(|e| CoreError::Parse {
            file: input.source.clone(),
            message: format!("failed to resolve script name: {e}"),
        })?;

        let code_idx = script.code_id as usize;
        if code_idx >= code.entries.len() {
            eprintln!("[gamemaker] warn: script {script_name} references invalid code entry {code_idx}");
            continue;
        }

        let bytecode = match code.entry_bytecode(code_idx, dw.data()) {
            Some(bc) => bc,
            None => {
                eprintln!("[gamemaker] warn: no bytecode for script {script_name}");
                continue;
            }
        };

        let code_entry = &code.entries[code_idx];
        let code_name = dw.resolve_string(code_entry.name).unwrap_or_default();
        let clean_name = strip_script_prefix(&script_name);
        let func_name = naming::snake_to_camel(clean_name);

        let locals = code_locals_map.get(&code_name).copied();

        let ctx = TranslateCtx {
            dw,
            function_names,
            variables,
            func_ref_map,
            vari_ref_map,
            bytecode_offset: code_entry.bytecode_offset,
            locals,
            has_self: false,
            has_other: false,
            arg_count: code_entry.args_count & 0x7FFF,
        };

        match translate::translate_code_entry(bytecode, &func_name, &ctx) {
            Ok(func) => {
                mb.add_function(func);
                translated += 1;
            }
            Err(e) => {
                eprintln!("[gamemaker] error translating {clean_name}: {e}");
                errors += 1;
            }
        }
    }

    Ok((translated, errors))
}

/// Translate global init scripts from GLOB chunk.
#[allow(clippy::too_many_arguments)]
fn translate_global_inits(
    dw: &DataWin,
    code: &datawin::chunks::code::Code,
    function_names: &HashMap<u32, String>,
    variables: &[(String, i32)],
    func_ref_map: &HashMap<usize, usize>,
    vari_ref_map: &HashMap<usize, usize>,
    code_locals_map: &HashMap<String, &datawin::chunks::func::CodeLocals>,
    mb: &mut ModuleBuilder,
) -> usize {
    let glob = match dw.glob() {
        Ok(Some(g)) => g,
        _ => return 0,
    };

    let mut count = 0;
    for &script_id in &glob.script_ids {
        let code_idx = script_id as usize;
        if code_idx >= code.entries.len() {
            continue;
        }
        let bytecode = match code.entry_bytecode(code_idx, dw.data()) {
            Some(bc) => bc,
            None => continue,
        };
        let code_entry = &code.entries[code_idx];
        let code_name = dw.resolve_string(code_entry.name).unwrap_or_default();
        let clean_name = strip_script_prefix(&code_name);
        let func_name = format!("_globalInit{}", naming::snake_to_pascal(clean_name));
        let locals = code_locals_map.get(&code_name).copied();

        let ctx = TranslateCtx {
            dw,
            function_names,
            variables,
            func_ref_map,
            vari_ref_map,
            bytecode_offset: code_entry.bytecode_offset,
            locals,
            has_self: false,
            has_other: false,
            arg_count: code_entry.args_count & 0x7FFF,
        };

        if let Ok(func) = translate::translate_code_entry(bytecode, &func_name, &ctx) {
            mb.add_function(func);
            count += 1;
        }
    }
    count
}

/// Translate room creation code from ROOM chunk.
#[allow(clippy::too_many_arguments)]
fn translate_room_creation(
    dw: &DataWin,
    code: &datawin::chunks::code::Code,
    function_names: &HashMap<u32, String>,
    variables: &[(String, i32)],
    func_ref_map: &HashMap<usize, usize>,
    vari_ref_map: &HashMap<usize, usize>,
    code_locals_map: &HashMap<String, &datawin::chunks::func::CodeLocals>,
    mb: &mut ModuleBuilder,
) -> usize {
    let room = match dw.room() {
        Ok(r) => r,
        Err(_) => return 0,
    };

    let mut count = 0;
    for room_entry in &room.rooms {
        if room_entry.creation_code_id < 0 {
            continue;
        }
        let code_idx = room_entry.creation_code_id as usize;
        if code_idx >= code.entries.len() {
            continue;
        }
        let bytecode = match code.entry_bytecode(code_idx, dw.data()) {
            Some(bc) => bc,
            None => continue,
        };
        let code_entry = &code.entries[code_idx];
        let code_name = dw.resolve_string(code_entry.name).unwrap_or_default();
        let room_name = dw.resolve_string(room_entry.name).unwrap_or_else(|_| format!("room_{code_idx}"));
        let func_name = format!("room{}Create", naming::room_name_to_pascal(&room_name));
        let locals = code_locals_map.get(&code_name).copied();

        let ctx = TranslateCtx {
            dw,
            function_names,
            variables,
            func_ref_map,
            vari_ref_map,
            bytecode_offset: code_entry.bytecode_offset,
            locals,
            has_self: false,
            has_other: false,
            arg_count: code_entry.args_count & 0x7FFF,
        };

        if let Ok(func) = translate::translate_code_entry(bytecode, &func_name, &ctx) {
            mb.add_function(func);
            count += 1;
        }
    }
    count
}

/// Register global variables from VARI.
fn register_globals(
    dw: &DataWin,
    vari: &datawin::chunks::vari::Vari,
    mb: &mut ModuleBuilder,
) {
    for entry in &vari.variables {
        // instance_type == -5 means global.
        if entry.instance_type == -5 {
            if let Ok(name) = dw.resolve_string(entry.name) {
                mb.add_global(Global {
                    name,
                    ty: Type::Dynamic,
                    visibility: Visibility::Public,
                    mutable: true,
                    init: None,
                });
            }
        }
    }
}

/// Build function_id → resolved name mapping from FUNC entries.
fn build_function_names(
    dw: &DataWin,
    func: &datawin::chunks::func::Func,
) -> Result<HashMap<u32, String>, CoreError> {
    let mut names = HashMap::new();
    for (idx, entry) in func.functions.iter().enumerate() {
        let name = dw.resolve_string(entry.name).unwrap_or_else(|_| format!("func_{idx}"));
        names.insert(idx as u32, name);
    }
    Ok(names)
}

/// Walk FUNC linked lists to build: absolute_instruction_address → func_entry_index.
///
/// `first_address` points to the Call instruction word; the function_id operand
/// is at `first_address + 4`. The operand's lower 27 bits encode a relative
/// offset to the next occurrence: `next_addr = addr + offset`.
fn build_func_ref_map(
    func: &datawin::chunks::func::Func,
    data: &[u8],
) -> HashMap<usize, usize> {
    let mut map = HashMap::new();
    for (i, entry) in func.functions.iter().enumerate() {
        if entry.first_address < 0 || entry.occurrences == 0 {
            continue;
        }
        let mut addr = entry.first_address as usize;
        for _ in 0..entry.occurrences {
            map.insert(addr, i);
            // The operand (next-pointer) is at addr + 4.
            let operand_addr = addr + 4;
            if operand_addr + 4 > data.len() {
                break;
            }
            let raw = u32::from_le_bytes(
                data[operand_addr..operand_addr + 4].try_into().unwrap(),
            );
            // Lower 27 bits = additive offset to next occurrence.
            let offset = (raw & 0x07FF_FFFF) as usize;
            if offset == 0 {
                break;
            }
            addr += offset;
        }
    }
    map
}

/// Walk VARI linked lists to build: absolute_instruction_address → vari_entry_index.
///
/// `first_address` points to the Push/Pop instruction word; the variable operand
/// is at `first_address + 4`. The operand's lower 27 bits encode a relative
/// offset to the next occurrence: `next_addr = addr + offset`.
fn build_vari_ref_map(
    vari: &datawin::chunks::vari::Vari,
    data: &[u8],
) -> HashMap<usize, usize> {
    let mut map = HashMap::new();
    for (i, entry) in vari.variables.iter().enumerate() {
        if entry.first_address < 0 || entry.occurrences == 0 {
            continue;
        }
        let mut addr = entry.first_address as usize;
        for _ in 0..entry.occurrences {
            map.insert(addr, i);
            // The operand (next-pointer) is at addr + 4.
            let operand_addr = addr + 4;
            if operand_addr + 4 > data.len() {
                break;
            }
            let raw = u32::from_le_bytes(
                data[operand_addr..operand_addr + 4].try_into().unwrap(),
            );
            // Lower 27 bits = additive offset to next occurrence.
            let offset = (raw & 0x07FF_FFFF) as usize;
            if offset == 0 {
                break;
            }
            addr += offset;
        }
    }
    map
}

/// Build variable_id → (name, instance_type) from VARI entries.
fn build_variable_table(
    dw: &DataWin,
    vari: &datawin::chunks::vari::Vari,
) -> Result<Vec<(String, i32)>, CoreError> {
    let mut vars = Vec::with_capacity(vari.variables.len());
    for entry in &vari.variables {
        let name = dw.resolve_string(entry.name).unwrap_or_else(|_| "???".to_string());
        vars.push((name, entry.instance_type));
    }
    Ok(vars)
}

/// Build code entry name → CodeLocals mapping.
fn build_code_locals_map<'a>(
    dw: &DataWin,
    func: &'a datawin::chunks::func::Func,
) -> Result<HashMap<String, &'a datawin::chunks::func::CodeLocals>, CoreError> {
    let mut map = HashMap::new();
    for entry in &func.code_locals {
        let name = dw.resolve_string(entry.name).unwrap_or_default();
        map.insert(name, entry);
    }
    Ok(map)
}

/// Resolve all object names from OBJT, converting to PascalCase.
fn resolve_object_names(
    dw: &DataWin,
    objt: &datawin::chunks::objt::Objt,
) -> Result<Vec<String>, CoreError> {
    let mut names = Vec::with_capacity(objt.objects.len());
    for obj in &objt.objects {
        let raw = dw.resolve_string(obj.name).unwrap_or_else(|_| "???".to_string());
        names.push(naming::object_name_to_pascal(&raw));
    }
    Ok(names)
}

/// Strip common GML script prefixes to get a clean function name.
fn strip_script_prefix(name: &str) -> &str {
    name.strip_prefix("gml_GlobalScript_")
        .or_else(|| name.strip_prefix("gml_Script_"))
        .unwrap_or(name)
}
