use std::fmt::Write;
use std::fs;
use std::path::Path;

use reincarnate_core::entity::EntityRef;
use reincarnate_core::error::CoreError;
use reincarnate_core::ir::{
    Block, CmpKind, Constant, Function, InstId, Module, Op, StructDef, Type, Visibility,
};

use crate::types::ts_type;

/// Emit a single module as a `.ts` file into `output_dir`.
pub fn emit_module(module: &Module, output_dir: &Path) -> Result<(), CoreError> {
    let mut out = String::new();

    emit_imports(module, &mut out);
    emit_structs(module, &mut out);
    emit_enums(module, &mut out);
    emit_globals(module, &mut out);
    emit_functions(module, &mut out)?;

    let path = output_dir.join(format!("{}.ts", module.name));
    fs::write(&path, &out).map_err(CoreError::Io)?;
    Ok(())
}

/// Emit a module to a string (for testing).
pub fn emit_module_to_string(module: &Module) -> Result<String, CoreError> {
    let mut out = String::new();

    emit_imports(module, &mut out);
    emit_structs(module, &mut out);
    emit_enums(module, &mut out);
    emit_globals(module, &mut out);
    emit_functions(module, &mut out)?;

    Ok(out)
}

// ---------------------------------------------------------------------------
// Imports
// ---------------------------------------------------------------------------

fn emit_imports(module: &Module, out: &mut String) {
    for import in &module.imports {
        let name = match &import.alias {
            Some(alias) => format!("{} as {}", import.name, alias),
            None => import.name.clone(),
        };
        let _ = writeln!(out, "import {{ {name} }} from \"./{}\";", import.module);
    }
    if !module.imports.is_empty() {
        out.push('\n');
    }
}

// ---------------------------------------------------------------------------
// Structs → interfaces
// ---------------------------------------------------------------------------

fn emit_structs(module: &Module, out: &mut String) {
    for def in &module.structs {
        emit_struct(def, out);
    }
}

fn emit_struct(def: &StructDef, out: &mut String) {
    let vis = visibility_prefix(def.visibility);
    let _ = writeln!(out, "{vis}interface {} {{", def.name);
    for (name, ty) in &def.fields {
        let _ = writeln!(out, "  {name}: {};", ts_type(ty));
    }
    let _ = writeln!(out, "}}\n");
}

// ---------------------------------------------------------------------------
// Enums → discriminated union types
// ---------------------------------------------------------------------------

fn emit_enums(module: &Module, out: &mut String) {
    for def in &module.enums {
        let vis = visibility_prefix(def.visibility);
        let variants: Vec<String> = def
            .variants
            .iter()
            .map(|v| {
                if v.fields.is_empty() {
                    format!("{{ tag: \"{}\" }}", v.name)
                } else {
                    let fields: Vec<String> = v
                        .fields
                        .iter()
                        .enumerate()
                        .map(|(i, t)| format!("field{i}: {}", ts_type(t)))
                        .collect();
                    format!("{{ tag: \"{}\", {} }}", v.name, fields.join(", "))
                }
            })
            .collect();
        let _ = writeln!(out, "{vis}type {} = {};", def.name, variants.join(" | "));
        out.push('\n');
    }
}

// ---------------------------------------------------------------------------
// Globals
// ---------------------------------------------------------------------------

fn emit_globals(module: &Module, out: &mut String) {
    for global in &module.globals {
        let vis = visibility_prefix(global.visibility);
        let kw = if global.mutable { "let" } else { "const" };
        let _ = writeln!(
            out,
            "{vis}{kw} {}: {};",
            global.name,
            ts_type(&global.ty)
        );
    }
    if !module.globals.is_empty() {
        out.push('\n');
    }
}

// ---------------------------------------------------------------------------
// Functions
// ---------------------------------------------------------------------------

fn emit_functions(module: &Module, out: &mut String) -> Result<(), CoreError> {
    for (_id, func) in module.functions.iter() {
        emit_function(func, out)?;
    }
    Ok(())
}

fn emit_function(func: &Function, out: &mut String) -> Result<(), CoreError> {
    let vis = visibility_prefix(func.visibility);
    let star = if func.coroutine.is_some() { "*" } else { "" };

    // Parameters from entry block.
    let entry = &func.blocks[func.entry];
    let params: Vec<String> = entry
        .params
        .iter()
        .map(|p| format!("{}: {}", val(p.value), ts_type(&p.ty)))
        .collect();
    let ret_ty = ts_type(&func.sig.return_ty);

    let _ = write!(
        out,
        "{vis}function{star} {}({params}): {ret_ty}",
        func.name,
        params = params.join(", "),
    );

    let is_simple = func.blocks.len() == 1;

    let _ = writeln!(out, " {{");

    if is_simple {
        emit_block_body(func, func.entry, &func.blocks[func.entry], out, "  ")?;
    } else {
        // Declare all value variables used as block params (non-entry).
        emit_block_param_declarations(func, out);

        let _ = writeln!(out, "  let $block = {};", func.entry.index());
        let _ = writeln!(out, "  while (true) {{");
        let _ = writeln!(out, "    switch ($block) {{");

        for (block_id, block) in func.blocks.iter() {
            let _ = writeln!(out, "      case {}: {{", block_id.index());
            emit_block_body(func, block_id, block, out, "        ")?;
            let _ = writeln!(out, "      }}");
        }

        let _ = writeln!(out, "    }}");
        let _ = writeln!(out, "  }}");
    }

    let _ = writeln!(out, "}}\n");
    Ok(())
}

/// Declare `let` bindings for block-param values in non-entry blocks.
fn emit_block_param_declarations(func: &Function, out: &mut String) {
    for (block_id, block) in func.blocks.iter() {
        if block_id == func.entry {
            continue;
        }
        for param in &block.params {
            let _ = writeln!(
                out,
                "  let {}: {};",
                val(param.value),
                ts_type(&param.ty)
            );
        }
    }
    // Also declare result values for instructions (non-void).
    for (_inst_id, inst) in func.insts.iter() {
        if let Some(v) = inst.result {
            // Don't re-declare entry block params.
            let is_entry_param = func.blocks[func.entry]
                .params
                .iter()
                .any(|p| p.value == v);
            if !is_entry_param {
                let _ = writeln!(
                    out,
                    "  let {}: {};",
                    val(v),
                    ts_type(&func.value_types[v])
                );
            }
        }
    }
}

fn emit_block_body(
    func: &Function,
    _block_id: reincarnate_core::ir::BlockId,
    block: &Block,
    out: &mut String,
    indent: &str,
) -> Result<(), CoreError> {
    for &inst_id in &block.insts {
        emit_inst(func, inst_id, out, indent)?;
    }
    Ok(())
}

fn emit_inst(
    func: &Function,
    inst_id: InstId,
    out: &mut String,
    indent: &str,
) -> Result<(), CoreError> {
    let inst = &func.insts[inst_id];
    let result = inst.result;

    match &inst.op {
        // -- Constants --
        Op::Const(c) => {
            let r = result.unwrap();
            let _ = writeln!(out, "{indent}{} = {};", val(r), emit_constant(c));
        }

        // -- Arithmetic --
        Op::Add(a, b) => emit_binop(out, indent, result, a, b, "+"),
        Op::Sub(a, b) => emit_binop(out, indent, result, a, b, "-"),
        Op::Mul(a, b) => emit_binop(out, indent, result, a, b, "*"),
        Op::Div(a, b) => emit_binop(out, indent, result, a, b, "/"),
        Op::Rem(a, b) => emit_binop(out, indent, result, a, b, "%"),
        Op::Neg(a) => {
            let r = result.unwrap();
            let _ = writeln!(out, "{indent}{} = -{};", val(r), val(*a));
        }

        // -- Bitwise --
        Op::BitAnd(a, b) => emit_binop(out, indent, result, a, b, "&"),
        Op::BitOr(a, b) => emit_binop(out, indent, result, a, b, "|"),
        Op::BitXor(a, b) => emit_binop(out, indent, result, a, b, "^"),
        Op::BitNot(a) => {
            let r = result.unwrap();
            let _ = writeln!(out, "{indent}{} = ~{};", val(r), val(*a));
        }
        Op::Shl(a, b) => emit_binop(out, indent, result, a, b, "<<"),
        Op::Shr(a, b) => emit_binop(out, indent, result, a, b, ">>"),

        // -- Comparison --
        Op::Cmp(kind, a, b) => {
            let op = match kind {
                CmpKind::Eq => "===",
                CmpKind::Ne => "!==",
                CmpKind::Lt => "<",
                CmpKind::Le => "<=",
                CmpKind::Gt => ">",
                CmpKind::Ge => ">=",
            };
            emit_binop(out, indent, result, a, b, op);
        }

        // -- Logic --
        Op::Not(a) => {
            let r = result.unwrap();
            let _ = writeln!(out, "{indent}{} = !{};", val(r), val(*a));
        }

        // -- Control flow --
        Op::Br { target, args } => {
            emit_branch_args(func, *target, args, out, indent);
            let _ = writeln!(out, "{indent}$block = {}; continue;", target.index());
        }
        Op::BrIf {
            cond,
            then_target,
            then_args,
            else_target,
            else_args,
        } => {
            let _ = writeln!(out, "{indent}if ({}) {{", val(*cond));
            let inner = format!("{indent}  ");
            emit_branch_args(func, *then_target, then_args, out, &inner);
            let _ = writeln!(
                out,
                "{inner}$block = {}; continue;",
                then_target.index()
            );
            let _ = writeln!(out, "{indent}}} else {{");
            emit_branch_args(func, *else_target, else_args, out, &inner);
            let _ = writeln!(
                out,
                "{inner}$block = {}; continue;",
                else_target.index()
            );
            let _ = writeln!(out, "{indent}}}");
        }
        Op::Switch {
            value,
            cases,
            default,
        } => {
            let _ = writeln!(out, "{indent}switch ({}) {{", val(*value));
            for (constant, target, args) in cases {
                let _ = writeln!(
                    out,
                    "{indent}  case {}:",
                    emit_constant(constant)
                );
                let inner = format!("{indent}    ");
                emit_branch_args(func, *target, args, out, &inner);
                let _ = writeln!(
                    out,
                    "{inner}$block = {}; continue;",
                    target.index()
                );
            }
            let _ = writeln!(out, "{indent}  default:");
            let inner = format!("{indent}    ");
            emit_branch_args(func, default.0, &default.1, out, &inner);
            let _ = writeln!(
                out,
                "{inner}$block = {}; continue;",
                default.0.index()
            );
            let _ = writeln!(out, "{indent}}}");
        }
        Op::Return(v) => match v {
            Some(v) => {
                let _ = writeln!(out, "{indent}return {};", val(*v));
            }
            None => {
                let _ = writeln!(out, "{indent}return;");
            }
        },

        // -- Memory / fields --
        Op::Alloc(ty) => {
            let r = result.unwrap();
            let _ = writeln!(
                out,
                "{indent}{} = undefined as unknown as {};",
                val(r),
                ts_type(ty)
            );
        }
        Op::Load(ptr) => {
            let r = result.unwrap();
            let _ = writeln!(out, "{indent}{} = {};", val(r), val(*ptr));
        }
        Op::Store { ptr, value } => {
            let _ = writeln!(out, "{indent}{} = {};", val(*ptr), val(*value));
        }
        Op::GetField { object, field } => {
            let r = result.unwrap();
            let _ = writeln!(out, "{indent}{} = {}.{field};", val(r), val(*object));
        }
        Op::SetField {
            object,
            field,
            value,
        } => {
            let _ = writeln!(
                out,
                "{indent}{}.{field} = {};",
                val(*object),
                val(*value)
            );
        }
        Op::GetIndex { collection, index } => {
            let r = result.unwrap();
            let _ = writeln!(
                out,
                "{indent}{} = {}[{}];",
                val(r),
                val(*collection),
                val(*index)
            );
        }
        Op::SetIndex {
            collection,
            index,
            value,
        } => {
            let _ = writeln!(
                out,
                "{indent}{}[{}] = {};",
                val(*collection),
                val(*index),
                val(*value)
            );
        }

        // -- Calls --
        Op::Call { func: fname, args } => {
            let args_str = args.iter().map(|a| val(*a)).collect::<Vec<_>>().join(", ");
            if let Some(r) = result {
                let _ = writeln!(out, "{indent}{} = {fname}({args_str});", val(r));
            } else {
                let _ = writeln!(out, "{indent}{fname}({args_str});");
            }
        }
        Op::CallIndirect { callee, args } => {
            let args_str = args.iter().map(|a| val(*a)).collect::<Vec<_>>().join(", ");
            if let Some(r) = result {
                let _ = writeln!(
                    out,
                    "{indent}{} = {}({args_str});",
                    val(r),
                    val(*callee)
                );
            } else {
                let _ = writeln!(out, "{indent}{}({args_str});", val(*callee));
            }
        }
        Op::SystemCall {
            system,
            method,
            args,
        } => {
            let args_str = args.iter().map(|a| val(*a)).collect::<Vec<_>>().join(", ");
            if let Some(r) = result {
                let _ = writeln!(
                    out,
                    "{indent}{} = {system}.{method}({args_str});",
                    val(r)
                );
            } else {
                let _ = writeln!(out, "{indent}{system}.{method}({args_str});");
            }
        }

        // -- Type operations --
        Op::Cast(v, ty) => {
            let r = result.unwrap();
            let _ = writeln!(
                out,
                "{indent}{} = {} as {};",
                val(r),
                val(*v),
                ts_type(ty)
            );
        }
        Op::TypeCheck(v, ty) => {
            let r = result.unwrap();
            let check = type_check_expr(*v, ty);
            let _ = writeln!(out, "{indent}{} = {check};", val(r));
        }

        // -- Aggregate construction --
        Op::StructInit { name: _, fields } => {
            let r = result.unwrap();
            let field_strs: Vec<String> = fields
                .iter()
                .map(|(name, v)| format!("{name}: {}", val(*v)))
                .collect();
            let _ = writeln!(
                out,
                "{indent}{} = {{ {} }};",
                val(r),
                field_strs.join(", ")
            );
        }
        Op::ArrayInit(elems) => {
            let r = result.unwrap();
            let elems_str = elems
                .iter()
                .map(|v| val(*v))
                .collect::<Vec<_>>()
                .join(", ");
            let _ = writeln!(out, "{indent}{} = [{elems_str}];", val(r));
        }
        Op::TupleInit(elems) => {
            let r = result.unwrap();
            let elems_str = elems
                .iter()
                .map(|v| val(*v))
                .collect::<Vec<_>>()
                .join(", ");
            let ty_str = ts_type(&func.value_types[r]);
            let _ = writeln!(
                out,
                "{indent}{} = [{elems_str}] as {ty_str};",
                val(r)
            );
        }

        // -- Coroutines --
        Op::Yield(v) => {
            if let Some(r) = result {
                match v {
                    Some(yv) => {
                        let _ =
                            writeln!(out, "{indent}{} = yield {};", val(r), val(*yv));
                    }
                    None => {
                        let _ = writeln!(out, "{indent}{} = yield;", val(r));
                    }
                }
            } else {
                match v {
                    Some(yv) => {
                        let _ = writeln!(out, "{indent}yield {};", val(*yv));
                    }
                    None => {
                        let _ = writeln!(out, "{indent}yield;");
                    }
                }
            }
        }
        Op::CoroutineCreate {
            func: fname,
            args,
        } => {
            let r = result.unwrap();
            let args_str = args.iter().map(|a| val(*a)).collect::<Vec<_>>().join(", ");
            let _ = writeln!(out, "{indent}{} = {fname}({args_str});", val(r));
        }
        Op::CoroutineResume(v) => {
            let r = result.unwrap();
            let _ = writeln!(out, "{indent}{} = {}.next();", val(r), val(*v));
        }

        // -- Misc --
        Op::GlobalRef(name) => {
            let r = result.unwrap();
            let _ = writeln!(out, "{indent}{} = {name};", val(r));
        }
        Op::Copy(src) => {
            let r = result.unwrap();
            let _ = writeln!(out, "{indent}{} = {};", val(r), val(*src));
        }
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn emit_binop(
    out: &mut String,
    indent: &str,
    result: Option<reincarnate_core::ir::ValueId>,
    a: &reincarnate_core::ir::ValueId,
    b: &reincarnate_core::ir::ValueId,
    op: &str,
) {
    let r = result.unwrap();
    let _ = writeln!(out, "{indent}{} = {} {op} {};", val(r), val(*a), val(*b));
}

fn emit_constant(c: &Constant) -> String {
    match c {
        Constant::Null => "null".into(),
        Constant::Bool(b) => b.to_string(),
        Constant::Int(n) => n.to_string(),
        Constant::UInt(n) => n.to_string(),
        Constant::Float(f) => format_float(*f),
        Constant::String(s) => format!("\"{}\"", escape_js_string(s)),
    }
}

fn format_float(f: f64) -> String {
    if f.fract() == 0.0 && f.is_finite() {
        format!("{f:.1}")
    } else {
        format!("{f}")
    }
}

fn escape_js_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for ch in s.chars() {
        match ch {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ => out.push(ch),
        }
    }
    out
}

fn val(v: reincarnate_core::ir::ValueId) -> String {
    format!("v{}", v.index())
}

fn visibility_prefix(vis: Visibility) -> &'static str {
    match vis {
        Visibility::Public => "export ",
        Visibility::Private => "",
    }
}

/// Assign branch args to the target block's parameter variables.
fn emit_branch_args(
    func: &Function,
    target: reincarnate_core::ir::BlockId,
    args: &[reincarnate_core::ir::ValueId],
    out: &mut String,
    indent: &str,
) {
    let target_block = &func.blocks[target];
    for (param, arg) in target_block.params.iter().zip(args.iter()) {
        let _ = writeln!(out, "{indent}{} = {};", val(param.value), val(*arg));
    }
}

/// Generate a TypeScript type-check expression.
fn type_check_expr(v: reincarnate_core::ir::ValueId, ty: &Type) -> String {
    match ty {
        Type::Bool => format!("typeof {} === \"boolean\"", val(v)),
        Type::Int(_) | Type::UInt(_) | Type::Float(_) => {
            format!("typeof {} === \"number\"", val(v))
        }
        Type::String => format!("typeof {} === \"string\"", val(v)),
        Type::Struct(name) | Type::Enum(name) => {
            format!("{} instanceof {name}", val(v))
        }
        _ => format!("typeof {} === \"object\"", val(v)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use reincarnate_core::ir::builder::{FunctionBuilder, ModuleBuilder};
    use reincarnate_core::ir::{
        EnumDef, EnumVariant, FunctionSig, Global, Import, StructDef, Visibility,
    };

    fn build_and_emit(build: impl FnOnce(&mut ModuleBuilder)) -> String {
        let mut mb = ModuleBuilder::new("test");
        build(&mut mb);
        emit_module_to_string(&mb.build()).unwrap()
    }

    #[test]
    fn simple_add_function() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Int(64), Type::Int(64)],
                return_ty: Type::Int(64),
            };
            let mut fb = FunctionBuilder::new("add", sig, Visibility::Public);
            let a = fb.param(0);
            let b = fb.param(1);
            let sum = fb.add(a, b);
            fb.ret(Some(sum));
            mb.add_function(fb.build());
        });

        assert!(out.contains("export function add(v0: number, v1: number): number {"));
        assert!(out.contains("v2 = v0 + v1;"));
        assert!(out.contains("return v2;"));
        // Single block → no dispatch loop.
        assert!(!out.contains("$block"));
    }

    #[test]
    fn branching_with_block_args() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![Type::Bool, Type::Int(64), Type::Int(64)],
                return_ty: Type::Int(64),
            };
            let mut fb = FunctionBuilder::new("choose", sig, Visibility::Public);

            let cond = fb.param(0);
            let x = fb.param(1);
            let y = fb.param(2);

            let (then_block, then_vals) = fb.create_block_with_params(&[Type::Int(64)]);
            let (else_block, else_vals) = fb.create_block_with_params(&[Type::Int(64)]);

            fb.br_if(cond, then_block, &[x], else_block, &[y]);

            fb.switch_to_block(then_block);
            fb.ret(Some(then_vals[0]));

            fb.switch_to_block(else_block);
            fb.ret(Some(else_vals[0]));

            mb.add_function(fb.build());
        });

        assert!(out.contains("$block"));
        assert!(out.contains("switch ($block)"));
        assert!(out.contains("if (v0)"));
        // Block args assignment.
        assert!(out.contains("v3 = v1;"));
        assert!(out.contains("v4 = v2;"));
    }

    #[test]
    fn struct_emission() {
        let out = build_and_emit(|mb| {
            mb.add_struct(StructDef {
                name: "Point".into(),
                fields: vec![
                    ("x".into(), Type::Float(64)),
                    ("y".into(), Type::Float(64)),
                ],
                visibility: Visibility::Public,
            });
        });

        assert!(out.contains("export interface Point {"));
        assert!(out.contains("  x: number;"));
        assert!(out.contains("  y: number;"));
    }

    #[test]
    fn enum_emission() {
        let out = build_and_emit(|mb| {
            mb.add_enum(EnumDef {
                name: "Shape".into(),
                variants: vec![
                    EnumVariant {
                        name: "Circle".into(),
                        fields: vec![Type::Float(64)],
                    },
                    EnumVariant {
                        name: "Rect".into(),
                        fields: vec![Type::Float(64), Type::Float(64)],
                    },
                ],
                visibility: Visibility::Public,
            });
        });

        assert!(out.contains("export type Shape ="));
        assert!(out.contains("tag: \"Circle\""));
        assert!(out.contains("tag: \"Rect\""));
    }

    #[test]
    fn global_variables() {
        let out = build_and_emit(|mb| {
            mb.add_global(Global {
                name: "counter".into(),
                ty: Type::Int(64),
                visibility: Visibility::Public,
                mutable: true,
            });
            mb.add_global(Global {
                name: "MAX_SIZE".into(),
                ty: Type::Int(64),
                visibility: Visibility::Private,
                mutable: false,
            });
        });

        assert!(out.contains("export let counter: number;"));
        assert!(out.contains("const MAX_SIZE: number;"));
    }

    #[test]
    fn imports() {
        let out = build_and_emit(|mb| {
            mb.add_import(Import {
                module: "utils".into(),
                name: "helper".into(),
                alias: None,
            });
            mb.add_import(Import {
                module: "math".into(),
                name: "add".into(),
                alias: Some("mathAdd".into()),
            });
        });

        assert!(out.contains("import { helper } from \"./utils\";"));
        assert!(out.contains("import { add as mathAdd } from \"./math\";"));
    }

    #[test]
    fn system_call() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Void,
            };
            let mut fb = FunctionBuilder::new("init", sig, Visibility::Public);
            let x = fb.const_int(100);
            let y = fb.const_int(200);
            fb.system_call("renderer", "clear", &[x, y], Type::Void);
            fb.ret(None);
            mb.add_function(fb.build());
        });

        assert!(out.contains("renderer.clear(v0, v1);"));
    }

    #[test]
    fn constants_all_types() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Void,
            };
            let mut fb = FunctionBuilder::new("constants", sig, Visibility::Public);
            fb.const_null();
            fb.const_bool(true);
            fb.const_bool(false);
            fb.const_int(42);
            fb.const_float(3.125);
            fb.const_string("hello \"world\"\nnewline");
            fb.ret(None);
            mb.add_function(fb.build());
        });

        assert!(out.contains("= null;"));
        assert!(out.contains("= true;"));
        assert!(out.contains("= false;"));
        assert!(out.contains("= 42;"));
        assert!(out.contains("= 3.125;"));
        assert!(out.contains(r#"= "hello \"world\"\nnewline";"#));
    }

    #[test]
    fn array_and_struct_init() {
        let out = build_and_emit(|mb| {
            let sig = FunctionSig {
                params: vec![],
                return_ty: Type::Void,
            };
            let mut fb = FunctionBuilder::new("init", sig, Visibility::Public);

            let a = fb.const_int(1);
            let b = fb.const_int(2);
            fb.array_init(&[a, b], Type::Int(64));

            let x = fb.const_float(10.0);
            let y = fb.const_float(20.0);
            fb.struct_init("Point", vec![("x".into(), x), ("y".into(), y)]);

            fb.ret(None);
            mb.add_function(fb.build());
        });

        assert!(out.contains("= [v0, v1];"));
        assert!(out.contains("= { x: v3, y: v4 };"));
    }
}
