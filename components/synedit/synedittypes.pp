{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditTypes.pas, released 2000-04-07.
The Original Code is based on parts of mwCustomEdit.pas by Martin Waldenburg,
part of the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditTypes;

{$I synedit.inc}

interface
uses
  SysUtils, types, Classes, Controls, LCLType, SynEditKeyCmds;

const
  TSynSpecialChars = [#128..#255]; // MG: special chars. Meaning depends on system encoding/codepage.
  TSynValidStringChars = ['_', '0'..'9', 'A'..'Z', 'a'..'z'] + TSynSpecialChars;
  TSynWhiteChars = [' ', #9];
  TSynWordBreakChars = ['.', ',', ';', ':', '"', '''', '`', '!', '?', '[', ']',
     '(', ')', '{', '}', '@', '^', '-', '=', '+', '*', '/', '\', '|','<','>',
     '%', '&', '~'];

type
  ESynEditError = class(Exception);

  TSynIdentChars = set of char;

  TLinePos = type integer; // 1..high(Integer);
  TLineIdx = type integer; // 0..high(Integer);
  IntPos = type integer; // 1..high(Integer);
  IntIdx = type integer; // 0..high(Integer);

  TLogPoint = Types.TPoint;
  TPhysPoint = Types.TPoint;
  TLogCaretPoint = record
    X, Y, Offs: Integer;
  end;

  TSynLineState = (slsNone, slsSaved, slsUnsaved);

  TSynCoordinateMappingFlag = (
    scmLimitToLines,
    scmIncludePartVisible,
    scmForceLeftSidePos   // do return the caret pos to the (logical) left of the char, even if the pixel is over the right half.
                          // TODO: RTL
  );
  TSynCoordinateMappingFlags = set of TSynCoordinateMappingFlag;

  PSynSelectionMode = ^TSynSelectionMode;
  // to be binary (clipboard) compatible with other (Delphi compiled) synedits
  // use {$PACKENUM 1}
{$PACKENUM 1}
  TSynSelectionMode = (smNormal, smLine, smColumn, smCurrent);
{$PACKENUM 4}

  TSynSearchOption =
    ( ssoMatchCase, ssoWholeWord,
      ssoBackwards,
      ssoEntireScope, ssoSelectedOnly,
      ssoReplace, ssoReplaceAll,
      ssoPrompt,
      ssoSearchInReplacement,    // continue search-replace in replacement (with ssoReplaceAll) // replace recursive
      ssoRegExpr, ssoRegExprMultiLine,
      ssoFindContinue      // Assume the current selection is the last match, and start search behind selection
                           // (before if ssoBackward) // Default is to start at caret (Only SearchReplace / SearchReplaceEx has start/end param)
    );
  TSynSearchOptions = set of TSynSearchOption;

  TSynEditRange = pointer;

  TSynStatusChange = (scCaretX, scCaretY,
    scLeftChar, scTopLine, scLinesInWindow, scCharsInWindow,
    scInsertMode, scModified, scSelection, scReadOnly,
    scFocus,     // received or lost focus
    scOptions    // some Options were changed (only triggered by some optinos)
   );
  TSynStatusChanges = set of TSynStatusChange;
  TStatusChangeEvent = procedure(Sender: TObject; Changes: TSynStatusChanges)
    of object;

  TSynPaintEvent = (peBeforePaint, peAfterPaint);
  TSynPaintEvents = set of TSynPaintEvent;
  TSynPaintEventProc = procedure(Sender: TObject; EventType: TSynPaintEvent;
    const rcClip: TRect
  ) of object;

  TSynScrollEvent = (peBeforeScroll, peAfterScroll, peAfterScrollFailed);
  TSynScrollEvents = set of TSynScrollEvent;
  TSynScrollEventProc = procedure(Sender: TObject; EventType: TSynScrollEvent;
    dx, dy: Integer; const rcScroll, rcClip: TRect
  ) of object;

  TSynMouseLocationInfo = record
    LastMouseCaret: TPoint;  // Char; physical (screen)
    LastMousePoint: TPoint;  // Pixel
  end;

  TSynQueryMouseCursorEvent = procedure(Sender: TObject; const AMouseLocation: TSynMouseLocationInfo;
    var AnCursor: TCursor; var APriority: Integer; var AChangedBy: TObject) of object;

  TSynEditorOption = (
    eoAutoIndent,              // Allows to indent the caret, when new line is created with <Enter>, with the same amount of leading white space as the preceding line
    eoBracketHighlight,        // Allows to highlight bracket, which matches bracket under caret
    eoEnhanceHomeKey,          // Toggles behaviour of <Home> key on line with leading spaces. If turned on, key will jump to first non-spacing char, if it's nearer to caret position. (Similar to Visual Studio.)
    eoGroupUndo,               // When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
    eoHalfPageScroll,          // When scrolling with <PageUp> and <PageDown> keys, only scroll a half page at a time
    eoHideRightMargin,         // Hides the vertical "right margin" line
    eoKeepCaretX,              // When moving through lines without "Scroll past EOL" option, keeps the X position of the caret
    eoNoCaret,                 // Hides caret (text blinking cursor) totally
    eoNoSelection,             // Disables any text selection
    eoPersistentCaret,         // Do not hide caret when focus is lost from control. (TODO: Windows still hides caret, if another component sets up a caret.)
    eoScrollByOneLess,         // Scroll vertically, by <PageUp> and <PageDown> keys, less by one line
    eoScrollPastEof,           // When scrolling to end-of-file, show last line at the top of the control, instead of the bottom
    eoScrollPastEol,           // Allows caret to go into empty space beyond end-of-line position
    eoScrollHintFollows,       // The hint, showing vertical scroll position, follows the mouse cursor
    eoShowScrollHint,          // Shows hint, with the current scroll position, when scrolling vertically by dragging the scrollbar slider
    eoShowSpecialChars,        // Shows non-printable characters (spaces, tabulations) with greyed symbols
    eoSmartTabs,               // When using <Tab> key, caret will go to the next non-space character of the previous line
    eoTabIndent,               // Allows keys <Tab> and <Shift+Tab> act as block-indent and block-unindent, for selected blocks
    eoTabsToSpaces,            // Converts tab characters to a specified number of space characters
    eoTrimTrailingSpaces,      // Spaces at the end of lines will be trimmed and not saved to file

    // Not implemented
    eoAutoSizeMaxScrollWidth,  //TODO Automatically resizes the MaxScrollWidth property when inserting text
    eoDisableScrollArrows,     //TODO Disables the scroll bar arrow buttons when you can't scroll in that direction any more
    eoHideShowScrollbars,      //TODO If enabled, then the scrollbars will only show when necessary. If you have "Scroll past EOL" option, then the horizontal bar will always be there (it uses MaxLength instead)
    eoDropFiles,               //TODO Allows control to accept file drag-drop operation
    eoSmartTabDelete,          //TODO Similar to "Smart tabs", but when you delete characters
    eoSpacesToTabs,            // Converts long substrings of space characters to tabs and spaces
    eoAutoIndentOnPaste,       // Allows to indent text pasted from clipboard
    //eoSpecialLineDefaultFg,    //TODO disables the foreground text color override when using the OnSpecialLineColor event

    // Only for compatibility, moved to TSynEditorMouseOptions
    // keep in one block
    eoAltSetsColumnMode,       // Allows to activate "column" selection mode, if <Alt> key is pressed and text is being selected with mouse
    eoDragDropEditing,         // Allows to drag-and-drop text blocks within the control
    eoRightMouseMovesCursor,   // When clicking with the right mouse button, for a popup menu, move the caret to clicked position
    eoDoubleClickSelectsLine,  // Selects entire line with double-click, otherwise double-click selects only current word
    eoShowCtrlMouseLinks       // Pressing <Ctrl> key (SYNEDIT_LINK_MODIFIER) will highlight the word under mouse cursor
    );
  TSynEditorOptions = set of TSynEditorOption;

  TSynEditorOption2 = (
    eoCaretSkipsSelection,     // Allows <Left> and <Right> keys to move caret to selected block edges, without deselecting the block
    eoCaretMoveEndsSelection,  // <Left> and <Right> will clear the selection, but the caret will NOT move.
                               // Combine with eoCaretSkipsSelection, and the caret will move to the other selection bound, if needed
                               // Kind of overrides eoPersistentBlock
    eoCaretSkipTab,            // Disables caret positioning inside tab-characters internal area
    eoAlwaysVisibleCaret,      // Keeps caret on currently visible control area, when scrolling control
    eoEnhanceEndKey,           // Toggles behaviour of <End> key on line with trailing spaces. If turned on, key will jump to last non-spacing char, if it's nearer to caret position.
    eoFoldedCopyPaste,         // Remember folding states of blocks, on Copy/Paste operations
    eoPersistentBlock,         // Keeps selection, even if caret moves away or text is edited
    eoOverwriteBlock,          // Allows to overwrite currently selected block, when pasting or typing new text
    eoAutoHideCursor,          // Hide mouse cursor, when new text is typed
    eoColorSelectionTillEol,   // Colorize selection background only till EOL of each line, not till edge of control
    eoPersistentCaretStopBlink,// only if eoPersistentCaret > do not blink, draw fixed line
    eoNoScrollOnSelectRange,   // SelectALl, SelectParagraph, SelectToBrace will not scroll
    eoAcceptDragDropEditing    // Accept dropping text dragged from a SynEdit (self or other).
                               // OnDragOver: To use OnDragOver, this flag should NOT be set.
                               // WARNING: Currently OnDragOver also works, if drag-source is NOT TSynEdit, this may be change for other drag sources.
                               //          This may in future affect if OnDragOver is called at all or not.
  );
  TSynEditorOptions2 = set of TSynEditorOption2;

  TSynVisibleSpecialChar = (vscSpace, vscTabAtFirst, vscTabAtLast);
  TSynVisibleSpecialChars = set of TSynVisibleSpecialChar;

  TSynLineStyle = (
    slsSolid,  // PS_SOLID pen
    slsDashed, // PS_DASH pen
    slsDotted, // PS_DOT
    slsWaved   // solid wave
  );

  TSynFrameEdges = (
    sfeNone,
    sfeAround,      // frame around
    sfeBottom,      // bottom part of the frame
    sfeLeft         // left part of the frame
  );

  TLazSynBorderSide = (
    bsLeft,
    bsTop,
    bsRight,
    bsBottom
  );
  TLazSynBorderSides = set of TLazSynBorderSide;

  THookedCommandEvent = procedure(Sender: TObject; AfterProcessing: boolean;
    var Handled: boolean; var Command: TSynEditorCommand;
    var AChar: TUtf8Char;
    Data: pointer; HandlerData: pointer) of object;
  THookedCommandFlag = (
    hcfInit,     // run before On[User]CommandProcess (outside UndoBlock / should not do execution)
    hcfPreExec,  // Run before CommandProcessor (unless handled by On[User]CommandProcess)
    hcfPostExec, // Run after CommandProcessor (unless handled by On[User]CommandProcess)
    hcfFinish    // Run at the very end
  );
  THookedCommandFlags = set of THookedCommandFlag;

  THookedKeyTranslationEvent = procedure(Sender: TObject;
    Code: word; SState: TShiftState; var Data: pointer; var IsStartOfCombo: boolean;
    var Handled: boolean; var Command: TSynEditorCommand;
    FinishComboOnly: Boolean; var ComboKeyStrokes: TSynEditKeyStrokes) of object;

const
  SynFrameEdgeToSides: array [TSynFrameEdges] of TLazSynBorderSides =
  ( [],                                       // sfeNone
    [bsLeft, bsTop, bsRight, bsBottom],   // sfeAround
    [bsBottom],                              // sfeBottom
    [bsLeft]                                 // sfeLeft
  );

  SynFrameEdgePriorities: array [TSynFrameEdges] of integer =
  ( 0,    // sfeNone
    1,   // sfeAround
    2,   // sfeBottom
    2    // sfeLeft
  );

  scTextCleared = [scCaretX, scCaretY, scLeftChar, scTopLine, scModified, scSelection];


implementation

end.


