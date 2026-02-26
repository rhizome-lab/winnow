# Flash (AVM2 / ActionScript 3)

**Status: Active** — The first implemented target. The frontend, transform pipeline, and TypeScript runtime are all functional. Ongoing work focuses on output quality and API coverage.

## Format

Flash applications are distributed as `.swf` files. SWF contains:
- **AVM2 bytecode** — ActionScript 3 compiled to Adobe's ActionScript Virtual Machine 2
- **ABC (ActionScript Bytecode)** — the bytecode container, embedded in DoABC tags
- **Assets** — embedded images, audio, fonts, and binary data in typed tags
- **Display list** — frame-by-frame timeline descriptions referencing symbol library entries

SWF is well-documented (SWF specification from Adobe, open-source parsers including Ruffle's `swf` crate).

## Lifting Strategy

Full recompilation. The `reincarnate-frontend-flash` crate:
1. Parses the SWF file (using the `swf` crate from Ruffle)
2. Decodes ABC bytecode per class/method
3. Emits typed IR — AS3 has full type annotations on locals, parameters, fields, and return types
4. Identifies `flash.*` namespace boundaries (stdlib vs user code)

The replacement runtime in `runtime/flash/ts/` implements the Flash API surface using Canvas 2D + Web Audio + DOM events.

## Implementation Status

The pipeline is fully operational. Test project: `~/reincarnate/flash/cc/`.

### Frontend completeness

- ✅ ABC bytecode decoding (all opcodes)
- ✅ Class/interface/trait extraction
- ✅ Type annotations from ABC metadata
- ✅ Static method resolution, closure compilation
- ✅ AVM2 int/uint semantics
- ✅ CastKind (AsType vs Coerce), rest parameters
- ⚠️ Exception handlers — `from`/`to` byte offsets extracted but no try/catch in IR yet
- ⚠️ Class flags (`is_sealed`, `is_final`) — discarded
- ⚠️ Protected namespace — per-class protected namespace not modeled
- ⚠️ Trait metadata annotations (`[Embed]`, `[Bindable]`, custom) — discarded
- ⚠️ `override`/`final` trait flags — discarded
- ⚠️ `DebugLine` source line info — could enable source maps

### Transform pass completeness

All passes implemented: TypeInference → ConstraintSolve → ConstantFolding → CfgSimplify → CoroutineLowering → Mem2Reg → ConstantFolding(2) → IntToBoolPromotion → RedCastElim → DCE.

Remaining quality issues:
- ⚠️ Some while-loop bodies have unreachable code after `continue`
- ⚠️ Structurizer occasionally inverts conditions (needs heuristic to match original polarity)
- ⚠️ Inline closures: some fall back to `$closureN` field references when `compile_closures()` fails for dynamic features

### Output quality

541 `: any` annotations in the `cc` test project (post-inference):

| Category | Count | Root cause |
|----------|-------|------------|
| `any[]` arrays | 185 | No array element-type inference |
| Parameter `: any` | 186 | Untyped params from ABC metadata gaps |
| Return `: any` | 79 | Functions returning Dynamic |
| Field `: any` | 80 | Empty class defs, external supers |
| `let` locals | 11 | Block params with disagreeing arg types |
| `const` locals | 9 | Genuinely untyped from calls/block params |

### Runtime coverage

See **[API-COVERAGE-FLASH.md](../../API-COVERAGE-FLASH.md)** for full per-class/method tracking.

High-level gaps:
- `flash.display` — 3D types (Matrix3D, Vector3D, PerspectiveProjection), Stage3D pipeline
- `flash.events` — Touch/gesture events, NetStatusEvent, StageVideoEvent
- `flash.text.engine` — TLF text block API (rarely used in games)
- `flash.net` — Socket, XMLSocket, NetConnection/NetStream, LocalConnection
- `flash.system` — System.gc/pause/resume, Worker/WorkerDomain
- `flash.utils` — getTimer, setTimeout/setInterval
- Top-level — Date, RegExp, XML (E4X), parseFloat/parseInt
- `flash.errors` — All error subclasses (stubs needed)
- `flash.external` — ExternalInterface

## Known Limitations

- **AVM1 content** (AS1/AS2 in old SWFs) is not supported and not planned
- **Stage3D** (GPU-accelerated 3D pipeline) requires a WebGL/WebGPU backend — currently out of scope
- **Streaming** — SWF streaming load semantics differ from static load; not modeled
- **Pixel fonts** — Embedded bitmap fonts require glyph atlas extraction
- **Multi-typed locals** — Some AS3 locals are assigned different types in different branches (e.g. sentinel `0.0` then `string`). These stay `: any` today; fixing requires SSA splitting, union types, or sentinel elimination

## References

- [AS3 API Reference (AIRSDK)](https://airsdk.dev/reference/actionscript/3.0/)
- [SWF File Format Specification](https://www.adobe.com/content/dam/acom/en/devnet/pdf/swf-file-format-spec.pdf)
- [Ruffle SWF parser](https://github.com/ruffle-rs/ruffle)
- [API Coverage Tracker](../../API-COVERAGE-FLASH.md)
