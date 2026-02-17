# Flash (ActionScript 3.0) API Coverage

Tracks implementation status of AS3 APIs relevant to Flash game/app decompilation.
Only flash.\* packages and top-level builtins are tracked — AIR, Flex (mx.\*),
and LiveCycle (com.adobe.\*) packages are out of scope.

References:
- [AS3 API Reference (AIRSDK)](https://airsdk.dev/reference/actionscript/3.0/)
- [AS3 API Reference (Adobe)](https://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/index.html)

Legend: [x] implemented, [~] partial/stub, [ ] not started

---

## Top Level

### Types & Constructors
- [x] Array
- [x] Boolean
- [ ] Class
- [ ] Date
- [x] Function
- [x] int
- [x] Number
- [x] Object
- [ ] RegExp
- [x] String
- [x] uint
- [x] Vector
- [ ] XML (E4X) — [~] stub with basic XMLList proxy
- [ ] XMLList — [~] stub with property projection

### Global Functions
- [ ] decodeURI / decodeURIComponent
- [ ] encodeURI / encodeURIComponent
- [ ] escape / unescape
- [x] int() / uint() (type coercion)
- [ ] isFinite
- [x] isNaN (via NaN push)
- [ ] isXMLName
- [x] Number() (type coercion)
- [ ] parseFloat / parseInt
- [x] String() (type coercion)
- [x] trace (→ console.log)
- [x] typeof

---

## flash.display

### DisplayObject
- [x] Properties: x, y, width, height, scaleX, scaleY, rotation, alpha, visible, name, parent, stage, root, mouseX, mouseY, transform, filters, blendMode, cacheAsBitmap, loaderInfo, mask, opaqueBackground, scrollRect, scale9Grid, rotationX/Y/Z, scaleZ, z
- [x] Methods: getBounds, getRect, globalToLocal, localToGlobal, hitTestObject, hitTestPoint
- [ ] Properties: accessibilityProperties

### InteractiveObject
- [x] Properties: mouseEnabled, doubleClickEnabled, focusRect, tabEnabled, tabIndex, contextMenu
- [ ] Properties: softKeyboardInputAreaOfInterest, needsSoftKeyboard

### DisplayObjectContainer
- [x] Properties: mouseChildren, numChildren, tabChildren
- [x] Methods: addChild, addChildAt, contains, getChildAt, getChildByName, getChildIndex, removeChild, removeChildAt, removeChildren, setChildIndex, swapChildren, swapChildrenAt
- [ ] Methods: areInaccessibleObjectsUnderPoint, getObjectsUnderPoint

### Sprite
- [x] Properties: graphics, buttonMode, dropTarget, hitArea, soundTransform, useHandCursor
- [x] Methods: startDrag, stopDrag
- [ ] Methods: startTouchDrag, stopTouchDrag

### MovieClip
- [x] Properties: currentFrame, currentFrameLabel, currentLabel, currentLabels, currentScene, enabled, framesLoaded, isPlaying, scenes, totalFrames, trackAsMenu
- [x] Methods: gotoAndPlay, gotoAndStop, nextFrame, nextScene, play, prevFrame, prevScene, stop, addFrameScript

### Stage
- [x] Properties: align, color, displayState, focus, frameRate, fullScreenHeight, fullScreenSourceRect, fullScreenWidth, quality, scaleMode, showDefaultContextMenu, stageFocusRect, stageHeight, stageWidth
- [x] Methods: invalidate
- [ ] Properties: allowsFullScreen, allowsFullScreenInteractive, contentsScaleFactor, mouseLock, wmodeGPU
- [ ] Methods: addEventListener for stage-specific events

### Shape
- [x] Properties: graphics

### Bitmap
- [x] Properties: bitmapData, pixelSnapping, smoothing

### BitmapData
- [x] Properties: width, height, transparent, rect
- [x] Methods: clone, dispose, getPixel, getPixel32, setPixel, setPixel32, fillRect, copyPixels, draw, lock, unlock
- [ ] Methods: applyFilter, colorTransform, compare, copyChannel, floodFill, generateFilterRect, getColorBoundsRect, getPixels, getVector, histogram, hitTest, merge, noise, paletteMap, perlinNoise, pixelDissolve, scroll, setPixels, setVector, threshold

### SimpleButton
- [x] Properties: downState, enabled, hitTestState, overState, soundTransform, trackAsMenu, upState, useHandCursor

### Loader
- [x] Methods: close, load, loadBytes, unload, unloadAndStop
- [x] LoaderInfo properties

### Graphics
- [x] Methods: beginBitmapFill, beginFill, beginGradientFill, clear, copyFrom, curveTo, drawCircle, drawEllipse, drawPath, drawRect, drawRoundRect, drawTriangles, endFill, lineGradientStyle, lineStyle, lineTo, moveTo
- [ ] Methods: beginShaderFill, cubicCurveTo, drawRoundRectComplex, readGraphicsData

### Constants/Enums
- [x] BlendMode
- [ ] StageAlign (partial)
- [ ] StageScaleMode (partial)
- [ ] StageQuality
- [ ] StageDisplayState
- [ ] PixelSnapping
- [ ] LineScaleMode, CapsStyle, JointStyle
- [ ] GradientType, InterpolationMethod, SpreadMethod
- [ ] BitmapDataChannel

---

## flash.events

### EventDispatcher
- [x] addEventListener, removeEventListener, dispatchEvent, hasEventListener, willTrigger

### Event
- [x] Properties: type, bubbles, cancelable, target, currentTarget, eventPhase
- [x] Methods: clone, formatToString, isDefaultPrevented, preventDefault, stopImmediatePropagation, stopPropagation
- [x] Constants: ACTIVATE, ADDED, ADDED_TO_STAGE, CANCEL, CHANGE, CLEAR, CLOSE, COMPLETE, CONNECT, COPY, CUT, DEACTIVATE, ENTER_FRAME, EXIT_FRAME, FRAME_CONSTRUCTED, INIT, MOUSE_LEAVE, OPEN, REMOVED, REMOVED_FROM_STAGE, RENDER, RESIZE, SCROLL, SELECT, SOUND_COMPLETE, UNLOAD, etc.

### MouseEvent
- [x] Properties: localX, localY, stageX, stageY, relatedObject, ctrlKey, altKey, shiftKey, buttonDown, delta, clickCount
- [x] Methods: updateAfterEvent
- [x] Constants: CLICK, DOUBLE_CLICK, MOUSE_DOWN, MOUSE_MOVE, MOUSE_OUT, MOUSE_OVER, MOUSE_UP, MOUSE_WHEEL, ROLL_OUT, ROLL_OVER, CONTEXT_MENU, MIDDLE_CLICK, MIDDLE_MOUSE_DOWN/UP, RIGHT_CLICK, RIGHT_MOUSE_DOWN/UP, RELEASE_OUTSIDE

### KeyboardEvent
- [x] Properties: charCode, keyCode, keyLocation, ctrlKey, altKey, shiftKey
- [x] Constants: KEY_DOWN, KEY_UP

### FocusEvent
- [x] Properties: relatedObject, shiftKey, keyCode, direction
- [x] Constants: FOCUS_IN, FOCUS_OUT, KEY_FOCUS_CHANGE, MOUSE_FOCUS_CHANGE

### TimerEvent
- [x] Constants: TIMER, TIMER_COMPLETE

### ProgressEvent
- [x] Properties: bytesLoaded, bytesTotal
- [x] Constants: PROGRESS, SOCKET_DATA

### IOErrorEvent
- [x] Constants: IO_ERROR

### SecurityErrorEvent
- [x] Constants: SECURITY_ERROR

### HTTPStatusEvent
- [x] Properties: status, responseHeaders, responseURL
- [x] Constants: HTTP_STATUS, HTTP_RESPONSE_STATUS

### AsyncErrorEvent
- [x] Properties: error
- [x] Constants: ASYNC_ERROR

### TextEvent
- [x] Properties: text
- [x] Constants: LINK, TEXT_INPUT

### ErrorEvent
- [x] Properties: errorID

### Not Implemented
- [ ] TouchEvent (TOUCH_BEGIN, TOUCH_END, TOUCH_MOVE, TOUCH_OUT, TOUCH_OVER, TOUCH_ROLL_OUT, TOUCH_ROLL_OVER, TOUCH_TAP)
- [ ] GestureEvent, TransformGestureEvent, PressAndTapGestureEvent
- [ ] NetStatusEvent
- [ ] StatusEvent
- [ ] SyncEvent
- [ ] FullScreenEvent
- [ ] SampleDataEvent
- [ ] ShaderEvent
- [ ] SoftKeyboardEvent
- [ ] StageVideoEvent
- [ ] VideoEvent
- [ ] DataEvent
- [ ] UncaughtErrorEvent / UncaughtErrorEvents
- [ ] ActivityEvent
- [ ] ContextMenuEvent
- [ ] DRMErrorEvent, DRMStatusEvent
- [ ] IMEEvent
- [ ] AccelerometerEvent, DeviceRotationEvent, GeolocationEvent

---

## flash.text

### TextField
- [x] Properties: text, htmlText, textColor, textWidth, textHeight, type, autoSize, multiline, wordWrap, selectable, embedFonts, defaultTextFormat, length, maxChars, restrict, background, backgroundColor, border, borderColor, displayAsPassword, condenseWhite, alwaysShowSelection, antiAliasType, gridFitType, maxScrollH, maxScrollV, scrollH, scrollV, bottomScrollV, mouseWheelEnabled, numLines, caretIndex, selectionBeginIndex, selectionEndIndex, sharpness, thickness, styleSheet, useRichTextClipboard
- [x] Methods: appendText, getTextFormat, setTextFormat, replaceSelectedText, replaceText, setSelection, getCharBoundaries, getCharIndexAtPoint, getLineIndexAtPoint, getLineIndexOfChar, getLineLength, getLineMetrics, getLineOffset, getLineText, getFirstCharInParagraph, getParagraphLength, getImageReference
- [x] Static: isFontCompatible

### TextFormat
- [x] All properties: align, blockIndent, bold, bullet, color, display, font, indent, italic, kerning, leading, leftMargin, letterSpacing, rightMargin, size, tabStops, target, underline, url

### TextLineMetrics
- [x] Properties: x, width, height, ascent, descent, leading

### StyleSheet
- [x] Methods: clear, getStyle, setStyle, parseCSS, transform

### Font
- [x] Properties: fontName, fontStyle, fontType
- [x] Methods: hasGlyphs, enumerateFonts, registerFont

### Constants
- [x] TextFieldType (DYNAMIC, INPUT)
- [x] TextFieldAutoSize (CENTER, LEFT, NONE, RIGHT)
- [x] TextFormatAlign (CENTER, JUSTIFY, LEFT, RIGHT)
- [x] AntiAliasType (ADVANCED, NORMAL)
- [ ] FontStyle, FontType, GridFitType

### Not Implemented
- [ ] StaticText
- [ ] TextSnapshot
- [ ] StageText, StageTextInitOptions
- [ ] flash.text.engine (entire TLF package — TextBlock, TextLine, ElementFormat, FontDescription, etc.)

---

## flash.geom

- [x] Point — full (x, y, length, add, clone, distance, equals, interpolate, normalize, offset, polar, subtract, etc.)
- [x] Rectangle — full (x, y, width, height, top, bottom, left, right, contains, containsPoint, containsRect, inflate, intersection, intersects, isEmpty, offset, union, etc.)
- [x] Matrix — full (a, b, c, d, tx, ty, clone, concat, createBox, createGradientBox, deltaTransformPoint, identity, invert, rotate, scale, transformPoint, translate)
- [x] ColorTransform — full (multipliers, offsets, color property)
- [x] Transform — concatenatedMatrix, concatenatedColorTransform, matrix, colorTransform
- [ ] Matrix3D
- [ ] Vector3D
- [ ] PerspectiveProjection
- [ ] Orientation3D
- [ ] Utils3D

---

## flash.filters

- [x] BitmapFilter (base)
- [x] BlurFilter
- [x] DropShadowFilter
- [x] GlowFilter
- [x] BevelFilter
- [x] ConvolutionFilter
- [x] ColorMatrixFilter
- [x] DisplacementMapFilter
- [x] GradientBevelFilter
- [x] GradientGlowFilter
- [ ] ShaderFilter
- [ ] BitmapFilterQuality, BitmapFilterType constants

---

## flash.media

### Sound
- [x] Properties: bytesLoaded, bytesTotal, id3, isBuffering, isURLInaccessible, length, url
- [x] Methods: close, play, load, extract

### SoundChannel
- [x] Properties: leftPeak, rightPeak, position, soundTransform
- [x] Methods: stop

### SoundTransform
- [x] Properties: volume, pan, leftToLeft, leftToRight, rightToLeft, rightToRight

### SoundMixer
- [x] Methods: computeSpectrum, stopAll

### SoundLoaderContext
- [x] Properties: bufferTime, checkPolicyFile

### ID3Info
- [x] Properties: album, artist, comment, genre, songName, track, year

### Not Implemented
- [~] Microphone (stub)
- [~] Camera (stub)
- [~] Video (stub)
- [ ] StageVideo
- [ ] StageWebView
- [ ] AudioDeviceManager
- [ ] H264VideoStreamSettings, VideoStreamSettings

---

## flash.net

### URLRequest
- [x] Properties: url, method, data, contentType, requestHeaders, digest

### URLRequestHeader
- [x] Properties: name, value

### URLLoader
- [x] Properties: data, dataFormat
- [x] Methods: close, load

### URLLoaderDataFormat
- [x] Constants: TEXT, BINARY, VARIABLES

### SharedObject
- [x] Properties: data, size, defaultObjectEncoding
- [x] Methods: clear, flush, getLocal, getRemote, deleteAll

### FileReference
- [x] Properties: creationDate, modificationDate, name, size
- [x] Methods: browse, cancel, download, save

### FileFilter
- [x] Properties: description, extension, macType

### Not Implemented
- [ ] Socket, XMLSocket, SecureSocket, ServerSocket
- [ ] NetConnection, NetStream, NetGroup
- [ ] LocalConnection
- [ ] URLStream, URLVariables
- [ ] URLRequestDefaults, URLRequestMethod
- [ ] Responder
- [ ] ObjectEncoding (constants only, no AMF)
- [ ] DatagramSocket
- [ ] NetworkInfo, NetworkInterface, InterfaceAddress

---

## flash.ui

### Keyboard
- [x] Key code constants (A-Z, 0-9, F1-F12, arrows, modifiers, numpad, punctuation)
- [x] Methods: isAccessible

### Mouse
- [x] Properties: cursor, supportsCursor, supportsNativeCursor
- [x] Methods: hide, show, registerCursor, unregisterCursor

### ContextMenu
- [x] Properties: customItems
- [x] Methods: hideBuiltInItems, clone

### ContextMenuItem
- [x] Properties: caption, enabled, separatorBefore, visible
- [x] Methods: clone

### Not Implemented
- [ ] GameInput, GameInputControl, GameInputDevice
- [ ] Multitouch, MultitouchInputMode
- [ ] MouseCursorData
- [ ] KeyboardType, KeyLocation (constants only)

---

## flash.utils

### ByteArray
- [x] Properties: length, bytesAvailable, position, endian, objectEncoding
- [x] Methods: read*/write* (Boolean, Byte, Double, Float, Int, Short, UTF, UTFBytes, UnsignedByte, UnsignedInt, UnsignedShort, MultiByte, Object)
- [x] Methods: clear, deflate, inflate, compress, uncompress, toString

### Timer
- [x] (via flash.events.TimerEvent dispatch)

### Dictionary
- [x] (maps to plain object with weak key semantics stubbed)

### Proxy
- [~] Stub

### Endian
- [x] Constants: BIG_ENDIAN, LITTLE_ENDIAN

### Global Functions
- [x] getQualifiedClassName
- [x] getQualifiedSuperclassName
- [x] getDefinitionByName
- [x] describeType
- [ ] getTimer
- [ ] setTimeout / setInterval / clearTimeout / clearInterval
- [ ] escapeMultiByte / unescapeMultiByte
- [ ] flash_proxy namespace

### Not Implemented
- [ ] CompressionAlgorithm (constants only)
- [ ] IDataInput / IDataOutput (interfaces only, used by ByteArray)
- [ ] IExternalizable

---

## flash.system

### Capabilities
- [x] Static properties: avHardwareDisable, hasAccessibility, hasAudio, hasMP3, hasVideoEncoder, isDebugger, language, localFileReadDisable, manufacturer, os, playerType, screenColor, screenDPI, screenResolutionX/Y, version

### Security
- [x] Constants: LOCAL_TRUSTED, LOCAL_WITH_FILE, LOCAL_WITH_NETWORK, REMOTE
- [x] Properties: sandboxType, exactSettings, disableAVM1Loading
- [ ] Methods: allowDomain, allowInsecureDomain, loadPolicyFile, showSettings

### ApplicationDomain
- [x] Methods: getDefinition, hasDefinition
- [x] Static: currentDomain

### LoaderContext
- [x] Properties: checkPolicyFile, applicationDomain

### System
- [ ] gc, pause, resume, exit
- [ ] totalMemory, freeMemory, privateMemory
- [ ] useCodePage, ime

### Not Implemented
- [ ] IME, IMEConversionMode
- [ ] SecurityDomain, SecurityPanel
- [ ] Worker, WorkerDomain, WorkerState
- [ ] MessageChannel, MessageChannelState
- [ ] ImageDecodingPolicy
- [ ] TouchscreenType

---

## flash.errors

- [ ] EOFError
- [ ] IllegalOperationError
- [ ] InvalidSWFError
- [ ] IOError
- [ ] MemoryError
- [ ] ScriptTimeoutError
- [ ] StackOverflowError

---

## flash.external

- [ ] ExternalInterface (callProperty, addCallback, available, objectID, marshallExceptions)

---

## flash.printing

- [ ] PrintJob, PrintJobOptions, PrintJobOrientation, PrintMethod, PrintUIOptions

---

## flash.xml

- [~] XMLDocument (legacy E4X stub)
- [~] XMLNode (legacy E4X stub)
- [ ] XMLNodeType

---

## flash.accessibility

- [ ] Accessibility
- [ ] AccessibilityImplementation
- [ ] AccessibilityProperties

---

## Packages Not Applicable to Game Decompilation

These packages exist in the AS3 reference but are irrelevant for Flash game lifting:

- `flash.concurrent` — Worker threading (rare in games)
- `flash.crypto` — Cryptographic operations
- `flash.data` — SQLite (AIR only)
- `flash.desktop` — Clipboard, NativeApplication (AIR only)
- `flash.display3D` / `flash.display3D.textures` — Stage3D (separate GPU pipeline)
- `flash.filesystem` — File I/O (AIR only)
- `flash.globalization` — Locale formatting
- `flash.html` — HTMLLoader (AIR only)
- `flash.net.dns` / `flash.net.drm` — DNS/DRM (AIR only)
- `flash.notifications` — Push notifications (AIR only)
- `flash.permissions` — Permission manager (AIR/mobile only)
- `flash.profiler` — Telemetry
- `flash.sampler` — Memory sampling
- `flash.security` — X.509, XML signatures
- `flash.sensors` — Accelerometer, Geolocation (mobile only)
- `flash.text.engine` — TLF text engine (rarely used in games)
- `flash.text.ime` — IME composition
- `flashx.textLayout` — TLF framework
- `fl.*` — Flash IDE components (not in SWFs)
