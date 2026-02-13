use std::collections::HashMap;

use datawin::chunks::objt::{event_type, ObjectEntry};
use datawin::DataWin;
use reincarnate_core::ir::builder::ModuleBuilder;
use reincarnate_core::ir::func::{MethodKind, Visibility};
use reincarnate_core::ir::module::{ClassDef, StructDef};
use reincarnate_core::ir::{Constant, Type};

use crate::translate::{self, TranslateCtx};

/// Translate all objects from the OBJT chunk into ClassDefs.
pub fn translate_objects(
    dw: &DataWin,
    code: &datawin::chunks::code::Code,
    function_names: &HashMap<u32, String>,
    variables: &[(String, i32)],
    code_locals_map: &HashMap<String, &datawin::chunks::func::CodeLocals>,
    mb: &mut ModuleBuilder,
    obj_names: &[String],
) -> Result<(usize, usize), String> {
    let objt = dw.objt().map_err(|e| e.to_string())?;
    let mut translated = 0;
    let mut errors = 0;

    for (obj_idx, obj) in objt.objects.iter().enumerate() {
        let obj_name = &obj_names[obj_idx];

        // Build instance field defaults from OBJT properties.
        let mut fields = Vec::new();
        if obj.sprite_index >= 0 {
            fields.push((
                "sprite_index".into(),
                Type::Int(32),
                Some(Constant::Int(obj.sprite_index as i64)),
            ));
        }
        if obj.depth != 0 {
            fields.push((
                "depth".into(),
                Type::Int(32),
                Some(Constant::Int(obj.depth as i64)),
            ));
        }

        let ns = vec!["objects".into()];

        let struct_index = mb.struct_count();
        mb.add_struct(StructDef {
            name: obj_name.clone(),
            namespace: ns.clone(),
            fields,
            visibility: Visibility::Public,
        });

        let mut method_ids = Vec::new();

        // Translate event handlers.
        for (event_type_idx, event_entries) in obj.events.iter().enumerate() {
            for event in event_entries {
                for action in &event.actions {
                    let code_idx = action.code_id as usize;
                    if code_idx >= code.entries.len() {
                        continue;
                    }
                    let bytecode = match code.entry_bytecode(code_idx, dw.data()) {
                        Some(bc) => bc,
                        None => continue,
                    };

                    let event_name = make_event_name(
                        event_type_idx,
                        event.subtype,
                        obj_names,
                    );
                    let func_name = format!("{obj_name}::{event_name}");

                    let code_entry = &code.entries[code_idx];
                    let code_name = dw.resolve_string(code_entry.name).unwrap_or_default();
                    let locals = code_locals_map.get(&code_name).copied();

                    let is_collision = event_type_idx == event_type::COLLISION;

                    let ctx = TranslateCtx {
                        dw,
                        function_names,
                        variables,
                        locals,
                        has_self: true,
                        has_other: is_collision,
                        arg_count: code_entry.args_count & 0x7FFF,
                    };

                    match translate::translate_code_entry(bytecode, &func_name, &ctx) {
                        Ok(mut func) => {
                            func.namespace = ns.clone();
                            func.class = Some(obj_name.clone());
                            // All event handlers are instance methods.
                            // `create` is called by the runtime after construction,
                            // not as a JS constructor.
                            func.method_kind = MethodKind::Instance;
                            let fid = mb.add_function(func);
                            method_ids.push(fid);
                            translated += 1;
                        }
                        Err(e) => {
                            eprintln!("[gamemaker] error translating {func_name}: {e}");
                            errors += 1;
                        }
                    }
                }
            }
        }

        // Resolve parent object. Root objects extend GMLObject.
        let super_class = resolve_parent(obj, obj_names)
            .or_else(|| Some("GMLObject".into()));

        mb.add_class(ClassDef {
            name: obj_name.clone(),
            namespace: ns,
            struct_index,
            methods: method_ids,
            super_class,
            visibility: Visibility::Public,
            static_fields: Vec::new(),
            is_interface: false,
            interfaces: Vec::new(),
        });
    }

    Ok((translated, errors))
}

/// Resolve parent object index to a name.
fn resolve_parent(obj: &ObjectEntry, obj_names: &[String]) -> Option<String> {
    // parent_index < 0 means no parent (-100 in most GameMaker versions).
    if obj.parent_index < 0 {
        return None;
    }
    let idx = obj.parent_index as usize;
    obj_names.get(idx).cloned()
}

/// Produce an event handler name that matches the runtime dispatch convention.
///
/// The runtime dispatches events via string-keyed method lookup:
/// - Alarms: `"alarm" + i` → `alarm0`, `alarm1`, …
/// - Key press: `"keypress" + keyCode` → `keypress13`, `keypress32`, …
/// - Keyboard: `"keyboard" + keyCode` → `keyboard13`, `keyboard32`, …
/// - Key release: `"keyrelease" + keyCode` → `keyrelease13`, …
/// - Collision: `"collision" + objectIndex` → `collision0`, `collision5`, …
/// - User events: `"user" + i` → `user0`, `user1`, …
/// - Step variants: `beginstep`, `endstep` (no underscore)
/// - Draw variants: `drawgui`, `drawbegin`, `drawend`, etc.
/// - Mouse: `mouseenter`, `mouseleave`
/// - Other: `roomstart`, `roomend`, `gamestart`, `gameend`, etc.
fn make_event_name(
    event_type_idx: usize,
    subtype: u32,
    _obj_names: &[String],
) -> String {
    match event_type_idx {
        event_type::CREATE => "create".into(),
        event_type::DESTROY => "destroy".into(),
        event_type::ALARM => format!("alarm{subtype}"),
        event_type::STEP => match subtype {
            0 => "step".into(),
            1 => "beginstep".into(),
            2 => "endstep".into(),
            _ => format!("step{subtype}"),
        },
        event_type::COLLISION => format!("collision{subtype}"),
        event_type::KEYBOARD => format!("keyboard{subtype}"),
        event_type::MOUSE => mouse_event_name(subtype),
        event_type::OTHER => other_event_name(subtype),
        event_type::DRAW => match subtype {
            0 => "draw".into(),
            64 => "drawgui".into(),
            65 => "drawresize".into(),
            72 => "drawbegin".into(),
            73 => "drawend".into(),
            74 => "drawguibegin".into(),
            75 => "drawguiend".into(),
            76 => "drawpre".into(),
            77 => "drawpost".into(),
            _ => format!("draw{subtype}"),
        },
        event_type::KEY_PRESS => format!("keypress{subtype}"),
        event_type::KEY_RELEASE => format!("keyrelease{subtype}"),
        event_type::TRIGGER => format!("trigger{subtype}"),
        _ => format!("event{event_type_idx}_{subtype}"),
    }
}

/// Map mouse event subtypes to runtime-compatible method names.
fn mouse_event_name(subtype: u32) -> String {
    match subtype {
        0 => "mouseleftbutton".into(),
        1 => "mouserightbutton".into(),
        2 => "mousemiddlebutton".into(),
        3 => "mousenobutton".into(),
        4 => "mouseleftpressed".into(),
        5 => "mouserightpressed".into(),
        6 => "mousemiddlepressed".into(),
        7 => "mouseleftreleased".into(),
        8 => "mouserightreleased".into(),
        9 => "mousemiddlereleased".into(),
        10 => "mouseenter".into(),
        11 => "mouseleave".into(),
        60 => "globalleftbutton".into(),
        61 => "globalrightbutton".into(),
        62 => "globalmiddlebutton".into(),
        63 => "globalleftpressed".into(),
        64 => "globalrightpressed".into(),
        65 => "globalmiddlepressed".into(),
        66 => "globalleftreleased".into(),
        67 => "globalrightreleased".into(),
        68 => "globalmiddlereleased".into(),
        _ => format!("mouse{subtype}"),
    }
}

/// Map "Other" event subtypes to runtime-compatible method names.
fn other_event_name(subtype: u32) -> String {
    match subtype {
        0 => "outsideroom".into(),
        1 => "intersectboundary".into(),
        2 => "gamestart".into(),
        3 => "gameend".into(),
        4 => "roomstart".into(),
        5 => "roomend".into(),
        6 => "nomorelives".into(),
        7 => "animationend".into(),
        8 => "endofpath".into(),
        9 => "nomorehealth".into(),
        10..=25 => format!("user{}", subtype - 10),
        30 => "closebutton".into(),
        40..=47 => format!("outsideview{}", subtype - 40),
        50..=57 => format!("boundaryview{}", subtype - 50),
        58 => "animationupdate".into(),
        59 => "animationevent".into(),
        60 => "asyncimageloaded".into(),
        62 => "asynchttp".into(),
        63 => "asyncdialog".into(),
        66 => "asynciap".into(),
        67 => "asynccloud".into(),
        68 => "asyncnetworking".into(),
        69 => "asyncsteam".into(),
        70 => "asyncsocial".into(),
        71 => "asyncpushnotification".into(),
        72 => "asyncsaveload".into(),
        73 => "asyncaudiorecording".into(),
        74 => "asyncaudioplayback".into(),
        75 => "asyncsystem".into(),
        _ => format!("other{subtype}"),
    }
}

