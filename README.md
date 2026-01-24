# Resurrect

Legacy software lifting framework.

Part of the [RHI](https://rhi-zone.github.io) ecosystem.

## Overview

Resurrect extracts and transforms applications from obsolete runtimes into modern web-based equivalents. It works on bytecode and script-based software—not native binaries.

### Supported Targets

- **Interactive Media**: Flash, Director/Shockwave, Authorware
- **Enterprise**: Visual Basic 6, Silverlight, Java Applets
- **No-Code Ancestors**: HyperCard, ToolBook
- **Game Engines**: RPG Maker, Ren'Py, GameMaker

### Approach

**Tier 1 (Native Patching)**: For binaries you can't fully lift—pointer relocation, font replacement, hex editing.

**Tier 2 (Runtime Replacement)**: For engines you can shim—hook internal draw calls, cancel original rendering, emit to an HTML/CSS overlay.

## License

MIT
