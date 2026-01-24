# Introduction

Resurrect is a framework for lifting legacy software from obsolete runtimes into modern web-based equivalents.

## The Problem

Millions of interactive applications are trapped in dying runtimes:

- **Flash/Shockwave**: Browser plugins deprecated, content inaccessible
- **Director/Authorware**: Museum kiosks, corporate training, educational CD-ROMs
- **Visual Basic 6**: Enterprise line-of-business apps nobody dares touch
- **HyperCard/ToolBook**: Early interactive media, lost to hardware rot
- **Java Applets/Silverlight**: The brief dark age before HTML5

## The Approach

Resurrect works on **bytecode and script**, not native binaries.

### Tier 1: Native Patching

For targets you *can't* fully lift (obscure compiled binaries):
- Pointer relocation, hex editing
- Font texture replacement
- Limited by original rendering constraints

### Tier 2: Runtime Replacement

For engines you *can* shim (Ren'Py, RPG Maker, Flash, GMS):
1. Hook the internal `draw_text()` function
2. Cancel the original draw call
3. Emit to a modern HTML/CSS overlay
4. Render with real typography, flexbox layout, accessibility

## Components

- **Explant**: Extract bytecode, decompile scripts, dump assets
- **Hypha**: Translation pipeline with UI overlay injection

## What Resurrect Is Not

- Not a VM or emulator (use Wine/DOSBox for native code)
- Not a "remaster" tool (we preserve, not improve)
- Not magic (some targets require significant manual work)
