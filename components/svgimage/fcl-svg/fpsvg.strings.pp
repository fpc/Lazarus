{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    The messages the package raises, gathered for translation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpsvg.strings;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

resourcestring

  { Geometry and paths }
  SErrSegmentBeforeMoveTo = 'Path segment before any moveto';
  SErrSegmentIndexOutOfRange = 'Path segment index %d out of range';
  SErrSubPathIndexOutOfRange = 'Subpath index %d out of range';
  SErrPointIndexOutOfRange = 'Point index %d out of range';

  { The backend registry }
  SErrNoBackendNamed = 'No SVG backend named "%s" is registered';
  SErrBackendClassHasNoName = 'Backend class %s has an empty name';
  SErrBackendAlreadyRegistered =
    'An SVG backend named "%s" is already registered';
  SErrNoDefaultBackend = 'No default SVG backend is set';

  { Frames, clips and layers }
  SErrNoImageToWrap = 'No image to wrap';
  SErrBeginFrameInFrame = 'BeginFrame inside a frame';
  SErrEndFrameWithoutBegin = 'EndFrame without BeginFrame';
  SErrFrameHasNoExtent = 'Frame size %d by %d has no extent';
  SErrOutsideFrame = '%s outside a frame';
  SErrEndFrameLayersOpen = 'EndFrame with %d layers open';
  SErrEndFrameClipsOpen = 'EndFrame with %d clips open';
  SErrEndFrameLevelsOpen = 'EndFrame with %d clip or layer levels open';
  SErrPopWithoutPush = '%s without a matching push';
  SErrMismatchedNesting = '%s closing a mismatched nesting level';
  SErrPopClipUnmatched = 'PopClip without a matching PushClip';
  SErrPopLayerUnmatched = 'PopLayer without a matching PushLayer';
  SErrPopLayerAsFilterImageUnmatched =
    'PopLayerAsFilterImage without a matching PushLayer';
  SErrPopLayerAsFilterUnmatched =
    'PopLayerAsFilter without a matching PushLayer';
  SErrPopLayerAsMaskUnmatched =
    'PopLayerAsMask without a matching PushLayer';

  { The node tree }
  SErrElementClassHasNoTag = 'Element class %s has no tag name';
  SErrAppendNilNode = 'Cannot append a nil node';
  SErrNodeHasParent = 'Node already has a parent';
  SErrNodeNotAChild = 'Node is not a child of this element';
  SErrReplaceWithNothing = 'Cannot replace a child with nothing';
  SErrReplacementHasParent = 'Replacement node already has a parent';

  { Reading a document }
  SErrNoXMLDocument = 'No XML document to convert';
  SErrNoRootElement = 'XML document has no root element';
  SErrRootNotSVG = 'Root element is not in the SVG namespace';
  SErrExternalEntity = 'The document declares an entity that reads '
    + 'another file; pass roExternalEntities to allow it';

  { Text and fonts }
  SErrRunIndexOutOfRange = 'Run index %d out of range';
  SErrNoFontElement = 'No font element to read';
  SErrNoFaceToDraw = 'No face to draw with';
  SErrFontIndexOutOfRange = 'Font index %d out of range';

  { Animation }
  SErrAnimationIndexOutOfRange = 'Animation index %d out of range';

  { Rendering }
  SErrNoBackendToRenderWith = 'Cannot render without a backend';

implementation

end.
