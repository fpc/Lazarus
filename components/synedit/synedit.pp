{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEdit.pas, released 2000-04-07.
The Original Code is based on mwCustomEdit.pas by Martin Waldenburg, part of
the mwEdit component suite.
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

  -DoubleBuffered
  -Font.CharSet
  -THintWindow
  -DragAcceptFiles
  -Font DBCS / MBCS  double, multi byte character set

-------------------------------------------------------------------------------}

unit SynEdit;


{$I synedit.inc}

{$IFDEF LCLGTK1}
{$DEFINE EnableDoubleBuf} // gtk1 does not have double buffering
{$ENDIF}
{$IFDEF LCLGTK2}
{ $DEFINE EnableDoubleBuf} // gtk2.10 paints faster to memory
                           // gtk2.12 paints faster directly
{$ENDIF}

interface

{ $DEFINE VerboseKeys}
{ $DEFINE VerboseSynEditInvalidate}
{ $DEFINE SYNDEBUGPRINT}

uses
  {$IFDEF USE_UTF8BIDI_LCL}
  FreeBIDI, utf8bidi,
  {$ENDIF}
  Types, LCLIntf, LCLType, LMessages, LCLProc,
  SysUtils, Classes, Messages, Controls, Graphics, Forms, StdCtrls, ExtCtrls, Menus,
  {$IFDEF SYN_MBCSSUPPORT}
  Imm,
  {$ENDIF}
  SynEditTypes, SynEditSearch, SynEditKeyCmds, SynEditMouseCmds, SynEditMiscProcs,
  SynEditPointClasses, SynBeautifier, SynEditMarks,
  SynEditMarkup, SynEditMarkupHighAll, SynEditMarkupBracket, SynEditMarkupWordGroup,
  SynEditMarkupCtrlMouseLink, SynEditMarkupSpecialLine, SynEditMarkupSelection,
  SynEditMarkupSpecialChar,
  SynEditTextBase, SynEditTextTrimmer, SynEditFoldedView, SynEditTextTabExpander,
  SynEditTextDoubleWidthChars,
  SynGutterBase, SynGutter, SynGutterCodeFolding, SynGutterChanges,
  SynGutterLineNumber, SynGutterMarks, SynGutterLineOverview,
  SynEditMiscClasses, SynEditTextBuffer, SynEditHighlighter, SynTextDrawer,
  SynEditLines,
  LResources, Clipbrd
  {$IFDEF SYN_COMPILER_4_UP}
  , StdActns
  {$ENDIF}
  ;

const
  ScrollBarWidth=0;

  // SynDefaultFont is determined in InitSynDefaultFont()
  SynDefaultFontName:    String       = '';
  SynDefaultFontHeight:  Integer      = 13;
  SynDefaultFontSize:    Integer      = 10;
  SynDefaultFontPitch:   TFontPitch   = fpFixed;
  SynDefaultFontQuality: TFontQuality = fqNonAntialiased;

  {$IFNDEF SYN_COMPILER_3_UP}
   // not defined in all Delphi versions
  WM_MOUSEWHEEL = $020A;
  {$ENDIF}

  // maximum scroll range
  MAX_SCROLL = 32767;

{$IFDEF SYN_MBCSSUPPORT}
{$IFNDEF SYN_COMPILER_4_UP}
{Windows.pas in D4}
const
  C3_NONSPACING = 1;    { nonspacing character }
  C3_DIACRITIC = 2;     { diacritic mark }
  C3_VOWELMARK = 4;     { vowel mark }
  C3_SYMBOL = 8;        { symbols }
  C3_KATAKANA = $0010;  { katakana character }
  C3_HIRAGANA = $0020;  { hiragana character }
  C3_HALFWIDTH = $0040; { half width character }
  C3_FULLWIDTH = $0080; { full width character }
  C3_IDEOGRAPH = $0100; { ideographic character }
  C3_KASHIDA = $0200;   { Arabic kashida character }
  C3_LEXICAL = $0400;   { lexical character }
  C3_ALPHA = $8000;     { any linguistic char (C1_ALPHA) }
  C3_NOTAPPLICABLE = 0; { ctype 3 is not applicable }
{$ENDIF}
{$ENDIF}

type
  TSynEditMarkupClass = SynEditMarkup.TSynEditMarkupClass;
  TSynReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  TSynDropFilesEvent = procedure(Sender: TObject; X, Y: integer; AFiles: TStrings)
    of object;

  THookedCommandEvent = procedure(Sender: TObject; AfterProcessing: boolean;
    var Handled: boolean; var Command: TSynEditorCommand;
    var AChar: TUTF8Char;
    Data: pointer; HandlerData: pointer) of object;

  THookedKeyTranslationEvent = procedure(Sender: TObject;
    Code: word; SState: TShiftState; var Data: pointer; var IsStartOfCombo: boolean;
    var Handled: boolean; var Command: TSynEditorCommand;
    FinishComboOnly: Boolean; var ComboKeyStrokes: TSynEditKeyStrokes) of object;

  TPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  TChangeUpdatingEvent = procedure(ASender: TObject; AnUpdating: Boolean) of object;

  TProcessCommandEvent = procedure(Sender: TObject;
    var Command: TSynEditorCommand;
    var AChar: TUTF8Char;
    Data: pointer) of object;

  TReplaceTextEvent = procedure(Sender: TObject; const ASearch, AReplace:
    string; Line, Column: integer; var ReplaceAction: TSynReplaceAction) of object;

  TSynCopyPasteAction = (scaContinue, scaPlainText, scaAbort);
  TSynCopyPasteEvent = procedure(Sender: TObject; var AText: String;
    var AMode: TSynSelectionMode; ALogStartPos: TPoint;
    var AnAction: TSynCopyPasteAction) of object;

  TSynEditCaretType = SynEditPointClasses.TSynCaretType;
  TSynCaretAdjustMode = ( // used in TextBetweenPointsEx
    scamIgnore, // Caret stays at the same numeric values, if text is inserted before caret, the text moves, but the caret stays
    scamAdjust, // Caret moves with text, if text is inserted
    scamEnd,
    scamBegin
  );

  TSynStateFlag = (sfCaretChanged, sfHideCursor,
    sfEnsureCursorPos, sfEnsureCursorPosAtResize,
    sfIgnoreNextChar, sfPainting, sfHasScrolled,
    sfScrollbarChanged, sfHorizScrollbarVisible, sfVertScrollbarVisible,
    sfAfterLoadFromFileNeeded,
    // Mouse-states
    sfLeftGutterClick, sfRightGutterClick,
    sfDblClicked, sfTripleClicked, sfQuadClicked,
    sfWaitForDragging, sfIsDragging, sfWaitForMouseSelecting, sfMouseSelecting, sfMouseDoneSelecting,
    sfIgnoreUpClick
    );                                           //mh 2000-10-30
  TSynStateFlags = set of TSynStateFlag;

  TSynEditorOption = (
    eoAutoIndent,              // Will indent the caret on new lines with the same amount of leading white space as the preceding line
    eoBracketHighlight,        // Highlight matching bracket
    eoEnhanceHomeKey,          // home key jumps to line start if nearer, similar to visual studio
    eoGroupUndo,               // When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
    eoHalfPageScroll,          // When scrolling with page-up and page-down commands, only scroll a half page at a time
    eoHideRightMargin,         // Hides the right margin line
    eoKeepCaretX,              // When moving through lines w/o Cursor Past EOL, keeps the X position of the cursor
    eoNoCaret,                 // Makes it so the caret is never visible
    eoNoSelection,             // Disables selecting text
    eoPersistentCaret,         // Do not hide caret when focus lost // TODO: Windows may hide it, if another component sets up a caret
    eoScrollByOneLess,         // Forces scrolling to be one less
    eoScrollPastEof,           // Allows the cursor to go past the end of file marker
    eoScrollPastEol,           // Allows the cursor to go past the last character into the white space at the end of a line
    eoScrollHintFollows,       // The scroll hint follows the mouse when scrolling vertically
    eoShowScrollHint,          // Shows a hint of the visible line numbers when scrolling vertically
    eoShowSpecialChars,        // Shows the special Characters
    eoSmartTabs,               // When tabbing, the cursor will go to the next non-white space character of the previous line
    eoTabIndent,               // When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
    eoTabsToSpaces,            // Converts a tab character to a specified number of space characters
    eoTrimTrailingSpaces,      // Spaces at the end of lines will be trimmed and not saved

    // Not implemented
    eoAutoSizeMaxScrollWidth,  //TODO Automatically resizes the MaxScrollWidth property when inserting text
    eoDisableScrollArrows,     //TODO Disables the scroll bar arrow buttons when you can't scroll in that direction any more
    eoHideShowScrollbars,      //TODO if enabled, then the scrollbars will only show when necessary.  If you have ScrollPastEOL, then it the horizontal bar will always be there (it uses MaxLength instead)
    eoDropFiles,               //TODO Allows the editor accept file drops
    eoSmartTabDelete,          //TODO similar to Smart Tabs, but when you delete characters
    eoSpacesToTabs,            // Converts space characters to tabs and spaces
    eoAutoIndentOnPaste,       // Indent text inserted from clipboard
    //eoSpecialLineDefaultFg,    //TODO disables the foreground text color override when using the OnSpecialLineColor event

    // Only for compatibility, moved to TSynEditorMouseOptions
    // keep in one block
    eoAltSetsColumnMode,       //
    eoDragDropEditing,         // Allows you to select a block of text and drag it within the document to another location
    eoRightMouseMovesCursor,   // When clicking with the right mouse for a popup menu, move the cursor to that location
    eoDoubleClickSelectsLine,  // Select line on double click
    eoShowCtrlMouseLinks       // Pressing Ctrl (SYNEDIT_LINK_MODIFIER) will highlight the word under the mouse cursor
    );
  TSynEditorOptions = set of TSynEditorOption;

  TSynEditorOption2 = (
    eoCaretSkipsSelection,     // Caret skips selection on VK_LEFT/VK_RIGHT
    eoCaretSkipTab,            // Caret can not enter tabs
    eoAlwaysVisibleCaret,      // Move caret to be always visible when scrolling
    eoEnhanceEndKey,           // end key jumps to visual/hard line end whichever is nearer
    eoFoldedCopyPaste,         // Remember folds in copy/paste operations
    eoPersistentBlock,         // Keep block if caret moves away or text is edited
    eoOverwriteBlock,          // Non persitent block, gets overwritten on insert/del
    eoAutoHideCursor           // Hide the mouse cursor, on keyboard action
  );
  TSynEditorOptions2 = set of TSynEditorOption2;

  TSynEditorMouseOption = SynEditMouseCmds.TSynEditorMouseOption;
    //emUseMouseActions,
    //emAltSetsColumnMode,       // Alt modifier, triggers column mode selection
    //emDragDropEditing,         // Allows you to select a block of text and drag it within the document to another location
    //emRightMouseMovesCursor,   // When clicking with the right mouse for a popup menu, move the cursor to that location
    //emDoubleClickSelectsLine,  // Select line on double click
    //emShowCtrlMouseLinks       // Pressing Ctrl (SYNEDIT_LINK_MODIFIER) will highlight the word under the mouse cursor
  TSynEditorMouseOptions = SynEditMouseCmds.TSynEditorMouseOptions;

  // options for textbuffersharing
  TSynEditorShareOption = (
    eosShareMarks              // Shared Editors use the same list of marks
  );
  TSynEditorShareOptions = set of TSynEditorShareOption;

  TSynVisibleSpecialChars = SynEditTypes.TSynVisibleSpecialChars;

const
  // MouseAction related options MUST NOT be included here
  SYNEDIT_DEFAULT_OPTIONS = [
    eoAutoIndent,
    eoScrollPastEol,
    eoSmartTabs,
    eoTabsToSpaces,
    eoTrimTrailingSpaces,
    eoGroupUndo,
    eoBracketHighlight
  ];

  // Those will be prevented from being set => so evtl they may be removed
  SYNEDIT_UNIMPLEMENTED_OPTIONS = [
    eoAutoSizeMaxScrollWidth,  //TODO Automatically resizes the MaxScrollWidth property when inserting text
    eoDisableScrollArrows,     //TODO Disables the scroll bar arrow buttons when you can't scroll in that direction any more
    eoDropFiles,               //TODO Allows the editor accept file drops
    eoHideShowScrollbars,      //TODO if enabled, then the scrollbars will only show when necessary.  If you have ScrollPastEOL, then it the horizontal bar will always be there (it uses MaxLength instead)
    eoSmartTabDelete,          //TODO similar to Smart Tabs, but when you delete characters
    ////eoSpecialLineDefaultFg,    //TODO disables the foreground text color override when using the OnSpecialLineColor event
    eoAutoIndentOnPaste,       // Indent text inserted from clipboard
    eoSpacesToTabs             // Converts space characters to tabs and spaces
  ];

  SYNEDIT_OLD_MOUSE_OPTIONS = [
    eoAltSetsColumnMode,       //
    eoDragDropEditing,         // Allows you to select a block of text and drag it within the document to another location
    eoRightMouseMovesCursor,   // When clicking with the right mouse for a popup menu, move the cursor to that location
    eoDoubleClickSelectsLine,  // Select line on double click
    eoShowCtrlMouseLinks       // Pressing Ctrl (SYNEDIT_LINK_MODIFIER) will highlight the word under the mouse cursor
  ];

  SYNEDIT_OLD_MOUSE_OPTIONS_MAP: array [eoAltSetsColumnMode..eoShowCtrlMouseLinks] of TSynEditorMouseOption = (
    emAltSetsColumnMode,       // eoAltSetsColumnMode
    emDragDropEditing,         // eoDragDropEditing
    emRightMouseMovesCursor,   // eoRightMouseMovesCursor
    emDoubleClickSelectsLine,  // eoDoubleClickSelectsLine
    emShowCtrlMouseLinks       // eoShowCtrlMouseLinks
  );

  SYNEDIT_DEFAULT_SHARE_OPTIONS = [
    eosShareMarks
  ];

  SYNEDIT_DEFAULT_OPTIONS2 = [
    eoFoldedCopyPaste,
    eoOverwriteBlock
  ];

  SYNEDIT_DEFAULT_MOUSE_OPTIONS = [];

  SYNEDIT_DEFAULT_VISIBLESPECIALCHARS = [
    vscSpace,
    vscTabAtLast
  ];

type
// use scAll to update a statusbar when another TCustomSynEdit got the focus
  TSynStatusChange = SynEditTypes.TSynStatusChange;
  TSynStatusChanges = SynEditTypes.TSynStatusChanges;

  TStatusChangeEvent = procedure(Sender: TObject; Changes: TSynStatusChanges)
    of object;

  TCustomSynEdit = class;

  TSynLineState = (slsNone, slsSaved, slsUnsaved);


  { TSynEditPlugin }

  TSynEditPlugin = class(TSynEditFriend)
  protected
    procedure SetEditor(const AValue: TCustomSynEdit); virtual;
    function GetEditor: TCustomSynEdit;
    function OwnedByEditor: Boolean; virtual; // if true, this will be destroyed by synedit
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Editor: TCustomSynEdit read GetEditor write SetEditor;
  end;

  { TSynHookedKeyTranslationList }

  TSynHookedKeyTranslationList = Class(TMethodList)
  public
    procedure CallHookedKeyTranslationHandlers(Sender: TObject;
      Code: word; SState: TShiftState; var Data: pointer;
      var IsStartOfCombo: boolean; var Handled: boolean;
      var Command: TSynEditorCommand; var ComboKeyStrokes: TSynEditKeyStrokes);
  end;

  TSynMouseLinkEvent = procedure (
    Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean) of object;

  TSynHomeMode = (synhmDefault, synhmFirstWord);

  TSynCoordinateMappingFlag = (scmLimitToLines, scmIncludePartVisible);
  TSynCoordinateMappingFlags = set of TSynCoordinateMappingFlag;

  { TCustomSynEdit }

  TCustomSynEdit = class(TSynEditBase)
  private
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
    procedure WMEraseBkgnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure WMGetDlgCode(var Msg: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMHScroll(var Msg: {$IFDEF SYN_LAZARUS}TLMScroll{$ELSE}TWMScroll{$ENDIF}); message WM_HSCROLL;
    {$IFDEF SYN_MBCSSUPPORT}
    procedure WMImeComposition(var Msg: TMessage); message WM_IME_COMPOSITION;
    procedure WMImeNotify(var Msg: TMessage); message WM_IME_NOTIFY;
    {$ENDIF}
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMExit(var Message: TLMExit); message LM_EXIT;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    //procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMSetFocus(var Msg: TLMSetFocus); message WM_SETFOCUS;
    procedure WMVScroll(var Msg: {$IFDEF SYN_LAZARUS}TLMScroll{$ELSE}TWMScroll{$ENDIF}); message WM_VSCROLL;
  private
    FBlockIndent: integer;
    FBlockTabIndent: integer;
    FCaret: TSynEditCaret;
    FInternalCaret: TSynEditCaret;
    FScreenCaret: TSynEditScreenCaret;
    FInternalBlockSelection: TSynEditSelection;
    FOnChangeUpdating: TChangeUpdatingEvent;
    FMouseSelectionMode: TSynSelectionMode;
    fMarkupManager : TSynEditMarkupManager;
    fMarkupHighAll : TSynEditMarkupHighlightAll;
    fMarkupHighCaret : TSynEditMarkupHighlightAllCaret;
    fMarkupBracket : TSynEditMarkupBracket;
    fMarkupWordGroup : TSynEditMarkupWordGroup;
    fMarkupCtrlMouse : TSynEditMarkupCtrlMouseLink;
    fMarkupSpecialLine : TSynEditMarkupSpecialLine;
    fMarkupSelection : TSynEditMarkupSelection;
    fMarkupSpecialChar : TSynEditMarkupSpecialChar;
    fCharsInWindow: Integer;
    fCharWidth: Integer;
    fFontDummy: TFont;
    {$IFDEF SYN_MBCSSUPPORT}
    fImeCount: Integer;
    fMBCSStepAside: Boolean;
    {$ENDIF}
    fInserting: Boolean;
    fLastMouseCaret: TPoint;  // Char; physical (screen)
    FLastMousePoint: TPoint;  // Pixel
    FChangedLinesStart: integer; // 1 based, 0 means invalid
    FChangedLinesEnd: integer; // 1 based, 0 means invalid, -1 means rest of screen
    FBeautifier, FDefaultBeautifier: TSynCustomBeautifier;
    FBeautifyStartLineIdx, FBeautifyEndLineIdx: Integer;

    FFoldedLinesView:  TSynEditFoldedView;
    FShareOptions: TSynEditorShareOptions;
    FVisibleSpecialChars: TSynVisibleSpecialChars;
    FTrimmedLinesView: TSynEditStringTrimmingList;
    FDoubleWidthChrLinesView: SynEditStringDoubleWidthChars;
    FTabbedLinesView:  TSynEditStringTabExpander;
    FTheLinesView: TSynEditStrings;
    FLines: TSynEditStrings;          // The real (un-mapped) line-buffer
    FStrings: TStrings;               // External TStrings based interface to the Textbuffer
    FTopLinesView: TSynEditStrings;   // The linesview that holds the real line-buffer/FLines

    fExtraCharSpacing: integer;
    fLinesInWindow: Integer;// MG: fully visible lines in window
    fLeftChar: Integer;    // first visible screen column
    fMaxLeftChar: Integer; // 1024
    FOldWidth, FOldHeight: Integer;

    FPaintLock: Integer;
    FPaintLockOwnerCnt: Integer;
    FScrollBarUpdateLock: Integer;
    FInvalidateRect: TRect;
    FIsInDecPaintLock: Boolean;
    fReadOnly: Boolean;
    fRightEdge: Integer;
    fRightEdgeColor: TColor;
    FScrollBars: TScrollStyle;
    fTextHeight: Integer;
    fTextOffset: Integer;
    fTopLine: Integer;
    FOldTopLine, FOldTopView: Integer;
    FLastTextChangeStamp: Int64;
    fHighlighter: TSynCustomHighlighter;
    fUndoList: TSynEditUndoList;
    fRedoList: TSynEditUndoList;
    FBookMarks: array[0..9] of TSynEditMark;
    fMouseDownX: integer;
    fMouseDownY: integer;
    fBookMarkOpt: TSynBookMarkOpt;
    FMouseWheelAccumulator: integer;
    fHideSelection: boolean;
    fOverwriteCaret: TSynEditCaretType;
    fInsertCaret: TSynEditCaretType;
    FKeyStrokes, FLastKeyStrokes: TSynEditKeyStrokes;
    FMouseActions, FMouseSelActions, FMouseTextActions: TSynEditMouseInternalActions;
    FMouseActionSearchHandlerList: TSynEditMouseActionSearchList;
    FMouseActionExecHandlerList: TSynEditMouseActionExecList;
    FMarkList: TSynEditMarkList;
    fExtraLineSpacing: integer;
    FUseUTF8: boolean;
    fWantTabs: boolean;
    FLeftGutter, FRightGutter: TSynGutter;
    fTabWidth: integer;
    fTextDrawer: TheTextDrawer;
    FPaintLineColor, FPaintLineColor2: TSynSelectedColor;
    fStateFlags: TSynStateFlags;
    FOptions: TSynEditorOptions;
    FOptions2: TSynEditorOptions2;
    FMouseOptions: TSynEditorMouseOptions;
    fStatusChanges: TSynStatusChanges;
    fTSearch: TSynEditSearch;
    fHookedCommandHandlers: TList;
    FHookedKeyTranslationList: TSynHookedKeyTranslationList;
    FStatusChangedList: TObject;
    FPlugins: TList;
    fScrollTimer: TTimer;
    fScrollDeltaX, fScrollDeltaY: Integer;
    FInMouseClickEvent: Boolean;
    FMouseClickDoPopUp: Boolean;
    // event handlers
    FOnCutCopy: TSynCopyPasteEvent;
    FOnPaste: TSynCopyPasteEvent;
    fOnChange: TNotifyEvent;
    FOnClearMark: TPlaceMarkEvent;                                              // djlp 2000-08-29
    fOnCommandProcessed: TProcessCommandEvent;
    fOnDropFiles: TSynDropFilesEvent;
    fOnPaint: TPaintEvent;
    FOnPlaceMark: TPlaceMarkEvent;
    fOnProcessCommand: TProcessCommandEvent;
    fOnProcessUserCommand: TProcessCommandEvent;
    fOnReplaceText: TReplaceTextEvent;
    fOnSpecialLineColors: TSpecialLineColorsEvent;// needed, because bug fpc 11926
    fOnStatusChange: TStatusChangeEvent;
    FOnSpecialLineMarkup: TSpecialLineMarkupEvent;// needed, because bug fpc 11926
    FOnClickLink: TMouseEvent;
    FOnMouseLink: TSynMouseLinkEvent;
    FPendingFoldState: String;

    procedure AquirePrimarySelection;
    function GetChangeStamp: int64;
    function GetDefSelectionMode: TSynSelectionMode;
    function GetFoldState: String;
    function GetModified: Boolean;
    function GetMouseActions: TSynEditMouseActions;
    function GetMouseSelActions: TSynEditMouseActions;
    function GetMouseTextActions: TSynEditMouseActions;
    function GetPaintLockOwner: TSynEditBase;
    function GetPlugin(Index: Integer): TSynEditPlugin;
    function GetTextBetweenPoints(aStartPoint, aEndPoint: TPoint): String;
    procedure SetBlockTabIndent(AValue: integer);
    procedure SetDefSelectionMode(const AValue: TSynSelectionMode);
    procedure SetFoldState(const AValue: String);
    procedure SetMouseActions(const AValue: TSynEditMouseActions);
    procedure SetMouseSelActions(const AValue: TSynEditMouseActions);
    procedure SetMouseTextActions(AValue: TSynEditMouseActions);
    procedure SetPaintLockOwner(const AValue: TSynEditBase);
    procedure SetShareOptions(const AValue: TSynEditorShareOptions);
    procedure SetTextBetweenPoints(aStartPoint, aEndPoint: TPoint; const AValue: String);
    procedure SetTextBetweenPointsEx(aStartPoint, aEndPoint: TPoint;
      aCaretMode: TSynCaretAdjustMode; const AValue: String);
    procedure SetVisibleSpecialChars(AValue: TSynVisibleSpecialChars);
    procedure SurrenderPrimarySelection;
    procedure BookMarkOptionsChanged(Sender: TObject);
    procedure ComputeCaret(X, Y: Integer);
    procedure DoBlockIndent;
    procedure DoBlockUnindent;
    procedure DoHomeKey(aMode: TSynHomeMode = synhmDefault);
    procedure DoEndKey;
    procedure DoTabKey;
    function FindHookedCmdEvent(AHandlerProc: THookedCommandEvent): integer;
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetBracketHighlightStyle: TSynEditBracketHighlightStyle;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretXY: TPoint;
    function GetFoldedCodeColor: TSynSelectedColor;
    function GetMarkup(Index: integer): TSynEditMarkup;
    function GetMarkupByClass(Index: TSynEditMarkupClass): TSynEditMarkup;
    function GetCaretX : Integer;
    function GetCaretY : Integer;
    function GetCaretUndo: TSynEditUndoItem;
    function GetHighlightAllColor : TSynSelectedColor;
    function GetIncrementColor : TSynSelectedColor;
    function GetLineHighlightColor: TSynSelectedColor;
    function GetOnGutterClick : TGutterClickEvent;
    function GetSelectedColor : TSynSelectedColor;
    function GetBracketMatchColor : TSynSelectedColor;
    function GetMouseLinkColor : TSynSelectedColor;
    function GetTrimSpaceType: TSynEditStringTrimmingType;
    procedure SetBracketHighlightStyle(
      const AValue: TSynEditBracketHighlightStyle);
    procedure SetOnGutterClick(const AValue : TGutterClickEvent);
    procedure SetSelectedColor(const AValue : TSynSelectedColor);
    procedure SetSpecialLineColors(const AValue : TSpecialLineColorsEvent);
    procedure SetSpecialLineMarkup(const AValue : TSpecialLineMarkupEvent);
    function GetHookedCommandHandlersCount: integer;
    function GetLineText: string;
    function GetCharLen(const Line: string; CharStartPos: integer): integer;
    function GetLogicalCaretXY: TPoint;
    procedure SetLogicalCaretXY(const NewLogCaretXY: TPoint);
    procedure SetBeautifier(NewBeautifier: TSynCustomBeautifier);
    function GetMaxUndo: Integer;
    function GetSelAvail: Boolean;
    function GetSelText: string;
    procedure SetTrimSpaceType(const AValue: TSynEditStringTrimmingType);
    function SynGetText: string;
    procedure GutterChanged(Sender: TObject);
    procedure GutterResized(Sender: TObject);
    // x-pixel pos of first char on canvas
    function  TextLeftPixelOffset(IncludeGutterTextDist: Boolean = True): Integer;
    function  TextRightPixelOffset: Integer;
    function IsPointInSelection(Value: TPoint): boolean;
    procedure LockUndo;
    procedure MoveCaretHorz(DX: integer);
    procedure MoveCaretVert(DY: integer);
    procedure PrimarySelectionRequest(const RequestedFormatID: TClipboardFormat;
      Data: TStream);
    procedure ScanRanges(ATextChanged: Boolean = True);
    procedure IdleScanRanges(Sender: TObject; var Done: Boolean);
    procedure DoBlockSelectionChanged(Sender: TObject);
    procedure SetBlockBegin(Value: TPoint);
    procedure SetBlockEnd(Value: TPoint);
    procedure SetBlockIndent(const AValue: integer);
    procedure SetCaretAndSelection(const ptCaret, ptBefore, ptAfter: TPoint;
                                   Mode: TSynSelectionMode = smCurrent;
                                   MakeSelectionVisible: Boolean = False
                                   );
    procedure SetCaretX(const Value: Integer);
    procedure SetCaretY(const Value: Integer);
    procedure SetExtraLineSpacing(const Value: integer);
    procedure SetGutter(const Value: TSynGutter);
    procedure SetRightGutter(const AValue: TSynGutter);
    procedure SetHideSelection(const Value: boolean);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure RemoveHooksFromHighlighter;
    procedure SetInsertCaret(const Value: TSynEditCaretType);
    procedure SetInsertMode(const Value: boolean);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    procedure SetExtraCharSpacing(const Value: integer);
    procedure SetLastMouseCaret(const AValue: TPoint);
    function  CurrentMaxLeftChar: Integer;
    function  CurrentMaxLineLen: Integer;
    procedure SetLeftChar(Value: Integer);
    procedure SetLineText(Value: string);
    procedure SetMaxLeftChar(Value: integer);
    procedure SetMaxUndo(const Value: Integer);
    procedure SetModified(Value: boolean);
    procedure SetOptions(Value: TSynEditorOptions);
    procedure UpdateOptions;
    procedure SetOptions2(const Value: TSynEditorOptions2);
    procedure UpdateOptions2;
    procedure SetMouseOptions(AValue: TSynEditorMouseOptions);
    procedure UpdateMouseOptions;
    procedure SetOverwriteCaret(const Value: TSynEditCaretType);
    procedure SetRightEdge(Value: Integer);
    procedure SetRightEdgeColor(Value: TColor);
    procedure SetScrollBars(const Value: TScrollStyle);
    function  GetSelectionMode : TSynSelectionMode;
    procedure SetSelectionMode(const Value: TSynSelectionMode);
    procedure SetSelTextExternal(const Value: string);
    procedure SetTabWidth(Value: integer);
    procedure SynSetText(const Value: string);
    function  CurrentMaxTopLine: Integer;
    procedure SetTopLine(Value: Integer);
    procedure ScrollAfterTopLineChanged;
    procedure SetWantTabs(const Value: boolean);
    procedure SetWordBlock(Value: TPoint);
    procedure SetLineBlock(Value: TPoint; WithLeadSpaces: Boolean = True);
    procedure SetParagraphBlock(Value: TPoint);
    procedure SizeOrFontChanged(bFont: boolean);
    procedure RecalcCharsAndLinesInWin(CheckCaret: Boolean);
    procedure StatusChanged(AChanges: TSynStatusChanges);
    procedure UndoRedoAdded(Sender: TObject);
    procedure ModifiedChanged(Sender: TObject);
    procedure UnlockUndo;
    procedure UpdateCaret(IgnorePaintLock: Boolean = False);
    procedure UpdateScrollBars;
    procedure ChangeTextBuffer(NewBuffer: TSynEditStringList);
    function  IsMarkListShared: Boolean;
    procedure RecreateMarkList;
    procedure DestroyMarkList;
    procedure RemoveHandlers(ALines: TSynEditStrings = nil);
    procedure ExtraLineCharsChanged(Sender: TObject);
    procedure InternalBeginUndoBlock(aList: TSynEditUndoList = nil); // includes paintlock
    procedure InternalEndUndoBlock(aList: TSynEditUndoList = nil);
  protected
    procedure CreateHandle; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;

    procedure FindAndHandleMouseAction(AButton: TSynMouseButton; AShift: TShiftState;
                                X, Y: Integer; ACCount:TSynMAClickCount;
                                ADir: TSynMAClickDir);
    function DoHandleMouseAction(AnActionList: TSynEditMouseActions;
                                 AnInfo: TSynEditMouseActionInfo): Boolean;

    procedure DoOnResize; override;
    function  RealGetText: TCaption; override;
    procedure RealSetText(const Value: TCaption); override;
    function GetLines: TStrings; override;
    function GetViewedTextBuffer: TSynEditStrings; override;
    function GetFoldedTextBuffer: TObject; override;
    function GetTextBuffer: TSynEditStrings; override;
    procedure SetLines(Value: TStrings);  override;
    function GetMarkupMgr: TObject; override;
    function GetCaretObj: TSynEditCaret; override;
    procedure IncPaintLock;
    procedure DecPaintLock;
    procedure DoIncPaintLock(Sender: TObject);
    procedure DoDecPaintLock(Sender: TObject);
    procedure DoIncForeignPaintLock(Sender: TObject);
    procedure DoDecForeignPaintLock(Sender: TObject);
    procedure SetUpdateState(NewUpdating: Boolean; Sender: TObject); virtual;      // Called *before* paintlock, and *after* paintlock
    procedure DestroyWnd; override;
    procedure DragOver(Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure FontChanged(Sender: TObject); override;
    function GetReadOnly: boolean; virtual;
    procedure HighlighterAttrChanged(Sender: TObject);
    // note: FirstLine and LastLine don't need to be in correct order
    procedure InvalidateGutterLines(FirstLine, LastLine: integer);
    procedure InvalidateLines(FirstLine, LastLine: integer);
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure UTF8KeyPress(var Key: TUTF8Char); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key : Word; Shift : TShiftState); override;
    Procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    Procedure LineTextChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    procedure DoHighlightChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    procedure ListCleared(Sender: TObject);
    procedure FoldChanged(Index: integer);
    function GetTopView : Integer;
    procedure SetTopView(const AValue : Integer);
    procedure Loaded; override;
    procedure MarkListChange(Sender: TSynEditMark; Changes: TSynEditMarkChangeReasons);
{$IFDEF SYN_MBCSSUPPORT}
    procedure MBCSGetSelRangeInLineWhenColumnSelectionMode(const s: string;
      var ColFrom, ColTo: Integer);
{$ENDIF}
    procedure NotifyHookedCommandHandlers(AfterProcessing: boolean;
      var Command: TSynEditorCommand;
      var AChar: TUTF8Char;
      Data: pointer); virtual;
    procedure Paint; override;
    procedure PaintTextLines(AClip: TRect; FirstLine, LastLine,
      FirstCol, LastCol: integer); virtual;
    procedure StartPaintBuffer(const ClipRect: TRect);
    procedure EndPaintBuffer(const ClipRect: TRect);
    function NextWordLogicalPos(WordEndForDelete : Boolean = false): TPoint;
    function PrevWordLogicalPos: TPoint;
    procedure RecalcCharExtent;
    procedure RedoItem(Item: TSynEditUndoItem);
    procedure SetCaretXY(Value: TPoint); virtual;
    procedure CaretChanged(Sender: TObject);
    procedure SetName(const Value: TComponentName); override;
    procedure SetReadOnly(Value: boolean); virtual;
    procedure SetSelTextPrimitive(PasteMode: TSynSelectionMode; Value: PChar;
      AddToUndoList: Boolean = false);
    procedure UndoItem(Item: TSynEditUndoItem);
    procedure UpdateCursor;
    property PaintLockOwner: TSynEditBase read GetPaintLockOwner write SetPaintLockOwner;
  protected
    {$IFDEF EnableDoubleBuf}
    BufferBitmap: TBitmap; // the double buffer
    {$ENDIF}
    SavedCanvas: TCanvas; // the normal TCustomControl canvas during paint
    function GetChildOwner: TComponent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure DoOnCommandProcessed(Command: TSynEditorCommand;
      AChar: TUTF8Char;
      Data: pointer); virtual;
    // no method DoOnDropFiles, intercept the WM_DROPFILES instead
    procedure DoOnPaint; virtual;
    procedure DoOnProcessCommand(var Command: TSynEditorCommand;
      var AChar: TUTF8Char;
      Data: pointer); virtual;
    function DoOnReplaceText(const ASearch, AReplace: string;
      Line, Column: integer): TSynReplaceAction; virtual;
    procedure DoOnStatusChange(Changes: TSynStatusChanges); virtual;
    property LastMouseCaret: TPoint read FLastMouseCaret write SetLastMouseCaret;
    function GetSelEnd: integer;                                                 //L505
    function GetSelStart: integer;
    procedure SetSelEnd(const Value: integer);
    procedure SetSelStart(const Value: integer);
    property TextView : TSynEditFoldedView read FFoldedLinesView;
    property TopView: Integer read GetTopView write SetTopView;  // TopLine converted into Visible(View) lines
    function PasteFromClipboardEx(ClipHelper: TSynClipboardStream): Boolean;
    function FindNextUnfoldedLine(iLine: integer; Down: boolean): Integer;
    // Todo: Reduce the argument list of Creategutter
    function CreateGutter(AOwner : TSynEditBase; ASide: TSynGutterSide;
                          ATextDrawer: TheTextDrawer): TSynGutter; virtual;
  public
    procedure FindMatchingBracket; virtual;
    function FindMatchingBracket(PhysStartBracket: TPoint;
                                 StartIncludeNeighborChars, MoveCaret,
                                 SelectBrackets, OnlyVisible: Boolean
                                 ): TPoint; virtual;
    //code fold
    procedure CodeFoldAction(iLine: integer); deprecated;
    procedure UnfoldAll;
    procedure FoldAll(StartLevel : Integer = 0; IgnoreNested : Boolean = False);
    procedure EraseBackground(DC: HDC); override;

    procedure AddKey(Command: TSynEditorCommand; Key1: word; SS1: TShiftState;
      Key2: word; SS2: TShiftState);
    procedure AfterLoadFromFile;
    procedure BeginUndoBlock;
    procedure BeginUpdate;
    function CaretXPix: Integer;
    function CaretYPix: Integer;
    procedure ClearAll;
    procedure ClearBookMark(BookMark: Integer);
    procedure ClearSelection;
    procedure CommandProcessor(Command:TSynEditorCommand;
      AChar: TUTF8Char;
      Data:pointer); virtual;
    procedure ClearUndo;
    procedure CopyToClipboard;
    constructor Create(AOwner: TComponent); override;
    procedure CutToClipboard;
    destructor Destroy; override;
    procedure DoCopyToClipboard(SText: string; FoldInfo: String = '');
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure EndUndoBlock;
    procedure EndUpdate;
    procedure EnsureCursorPosVisible;
{$IFDEF SYN_COMPILER_4_UP}
    function ExecuteAction(ExeAction: TBasicAction): boolean; override;
{$ENDIF}
    procedure ExecuteCommand(Command: TSynEditorCommand;
      const AChar: TUTF8Char; Data: pointer); virtual;
    function GetBookMark(BookMark: integer; var X, Y: integer): boolean;
    function GetHighlighterAttriAtRowCol(XY: TPoint; var Token: string;
      var Attri: TSynHighlighterAttributes): boolean;
    function GetHighlighterAttriAtRowColEx(XY: TPoint; var Token: string;
      var TokenType, Start: Integer;
      var Attri: TSynHighlighterAttributes): boolean;                           //L505

    procedure GetWordBoundsAtRowCol(const XY: TPoint; var StartX, EndX: integer);
    function GetWordAtRowCol(XY: TPoint): string;
    function NextTokenPos: TPoint; virtual; deprecated; // use next word pos instead
    function NextWordPos: TPoint; virtual;
    function PrevWordPos: TPoint; virtual;
    function IdentChars: TSynIdentChars;
    function IsIdentChar(const c: TUTF8Char): boolean;

    function IsLinkable(Y, X1, X2: Integer): Boolean;
    procedure GotoBookMark(BookMark: Integer);
    procedure InvalidateGutter;
    procedure InvalidateLine(Line: integer);
    function IsBookmark(BookMark: integer): boolean;
    procedure MarkTextAsSaved;
    // Byte to Char
    function LogicalToPhysicalPos(const p: TPoint): TPoint;
    function LogicalToPhysicalCol(const Line: String; Index, LogicalPos
                              : integer): integer;
    // Char to Byte
    function PhysicalToLogicalPos(const p: TPoint): TPoint;
    function PhysicalToLogicalCol(const Line: string;
                                  Index, PhysicalPos: integer): integer;
    function PhysicalLineLength(Line: String; Index: integer): integer;
    function ScreenColumnToXValue(Col: integer): integer;  // map screen column to screen pixel
    procedure MoveCaretToVisibleArea;
    procedure MoveCaretIgnoreEOL(const NewCaret: TPoint);
    procedure MoveLogicalCaretIgnoreEOL(const NewLogCaret: TPoint);
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PasteFromClipboard;
    function PixelsToRowColumn(Pixels: TPoint; aFlags: TSynCoordinateMappingFlags = [scmLimitToLines]): TPoint;
    function PixelsToLogicalPos(const Pixels: TPoint): TPoint;
    function ScreenRowToRow(ScreenRow: integer; LimitToLines: Boolean = True): integer;
    function RowToScreenRow(PhysicalRow: integer): integer;
    procedure Redo;
    procedure RegisterCommandHandler(AHandlerProc: THookedCommandEvent;
      AHandlerData: pointer);
    procedure UnregisterCommandHandler(AHandlerProc: THookedCommandEvent);

    procedure RegisterMouseActionSearchHandler(AHandlerProc: TSynEditMouseActionSearchProc);
    procedure UnregisterMouseActionSearchHandler(AHandlerProc: TSynEditMouseActionSearchProc);
    procedure RegisterMouseActionExecHandler(AHandlerProc: TSynEditMouseActionExecProc);
    procedure UnregisterMouseActionExecHandler(AHandlerProc: TSynEditMouseActionExecProc);

    procedure RegisterKeyTranslationHandler(AHandlerProc: THookedKeyTranslationEvent);
    procedure UnRegisterKeyTranslationHandler(AHandlerProc: THookedKeyTranslationEvent);

    procedure RegisterStatusChangedHandler(AStatusChangeProc: TStatusChangeEvent; AChanges: TSynStatusChanges);
    procedure UnRegisterStatusChangedHandler(AStatusChangeProc: TStatusChangeEvent);

    // RowColumnToPixels: Physical coords
    function RowColumnToPixels(const RowCol: TPoint): TPoint;
    function SearchReplace(const ASearch, AReplace: string;
      AOptions: TSynSearchOptions): integer;
    function SearchReplaceEx(const ASearch, AReplace: string;
      AOptions: TSynSearchOptions; AStart: TPoint): integer;
    procedure SelectAll;
    Procedure SetHighlightSearch(const ASearch: String; AOptions: TSynSearchOptions);
    procedure SelectToBrace;
    procedure SelectWord;
    procedure SelectLine(WithLeadSpaces: Boolean = True);
    procedure SelectParagraph;
    procedure SetUseIncrementalColor(const AValue : Boolean);
    procedure SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
    procedure SetDefaultKeystrokes; virtual;
    procedure ResetMouseActions;  // set mouse-actions according to current Options / may clear them
    procedure SetOptionFlag(Flag: TSynEditorOption; Value: boolean);
    procedure Undo;
    function GetLineState(ALine: Integer): TSynLineState;
{$IFDEF SYN_COMPILER_4_UP}
    function UpdateAction(TheAction: TBasicAction): boolean; override;
{$ENDIF}
    procedure WndProc(var Msg: TMessage); override;
  public
    procedure InsertTextAtCaret(aText: String; aCaretMode : TSynCaretAdjustMode = scamEnd);
    property BlockBegin: TPoint read GetBlockBegin write SetBlockBegin;         // Set Blockbegin. For none persistent also sets Blockend. Setting Caret may undo this and should be done before setting block
    property BlockEnd: TPoint read GetBlockEnd write SetBlockEnd;
    property FoldState: String read GetFoldState write SetFoldState;
    property CanPaste: Boolean read GetCanPaste;
    property CanRedo: boolean read GetCanRedo;
    property CanUndo: boolean read GetCanUndo;
    property CaretX: Integer read GetCaretX write SetCaretX;
    property CaretY: Integer read GetCaretY write SetCaretY;
    property CaretXY: TPoint read GetCaretXY write SetCaretXY;// screen position
    property LogicalCaretXY: TPoint read GetLogicalCaretXY write SetLogicalCaretXY;
    property CharsInWindow: Integer read fCharsInWindow;
    property CharWidth: integer read fCharWidth;
    property Color default clWhite;
    property Beautifier: TSynCustomBeautifier read fBeautifier write SetBeautifier;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelEnd: Integer read GetSelEnd write SetSelEnd;
    property UseIncrementalColor : Boolean write SetUseIncrementalColor;
    property Highlighter: TSynCustomHighlighter
      read fHighlighter write SetHighlighter;
    property LeftChar: Integer read fLeftChar write SetLeftChar;
    property LineHeight: integer read fTextHeight;
    property LinesInWindow: Integer read fLinesInWindow; // MG: fully visible lines
    property LineText: string read GetLineText write SetLineText;
    property Text: string read SynGetText write SynSetText;                     // No uncommited (trailing/trimmable) spaces
    property Marks: TSynEditMarkList read fMarkList;
    property MaxLeftChar: integer read fMaxLeftChar write SetMaxLeftChar
      default 1024;
    property Modified: Boolean read GetModified write SetModified;
    property PaintLock: Integer read fPaintLock;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default FALSE;
    property SelAvail: Boolean read GetSelAvail;
    property SelText: string read GetSelText write SetSelTextExternal;
    // Logical Points
    property TextBetweenPoints[aStartPoint, aEndPoint: TPoint]: String
      read GetTextBetweenPoints write SetTextBetweenPoints;
    property TextBetweenPointsEx[aStartPoint, aEndPoint: TPoint; CaretMode: TSynCaretAdjustMode]: String
      write SetTextBetweenPointsEx;
    property TopLine: Integer read fTopLine write SetTopLine;
    property UseUTF8: boolean read FUseUTF8;
    procedure Update; override;
    procedure Invalidate; override;
    property ChangeStamp: int64 read GetChangeStamp;
    procedure ShareTextBufferFrom(AShareEditor: TCustomSynEdit);
    procedure UnShareTextBuffer;
  public
    property OnKeyDown;
    property OnKeyPress;
    property OnProcessCommand: TProcessCommandEvent
      read FOnProcessCommand write FOnProcessCommand;
    function PluginCount: Integer;
    property Plugin[Index: Integer]: TSynEditPlugin read GetPlugin;
    function MarkupCount: Integer;
    property Markup[Index: integer]: TSynEditMarkup read GetMarkup;
    property MarkupByClass[Index: TSynEditMarkupClass]: TSynEditMarkup
      read GetMarkupByClass;
    property TrimSpaceType: TSynEditStringTrimmingType
      read GetTrimSpaceType write SetTrimSpaceType;
    property BookMarkOptions: TSynBookMarkOpt
      read fBookMarkOpt write fBookMarkOpt;
    property BlockIndent: integer read FBlockIndent write SetBlockIndent default 2;
    property BlockTabIndent: integer read FBlockTabIndent write SetBlockTabIndent default 0;
    property ExtraCharSpacing: integer
      read fExtraCharSpacing write SetExtraCharSpacing default 0;
    property ExtraLineSpacing: integer
      read fExtraLineSpacing write SetExtraLineSpacing default 0;
    property Gutter: TSynGutter read FLeftGutter write SetGutter;
    property RightGutter: TSynGutter read FRightGutter write SetRightGutter;
    property HideSelection: boolean read fHideSelection write SetHideSelection
      default false;
    property InsertCaret: TSynEditCaretType read FInsertCaret
      write SetInsertCaret default ctVerticalLine;
    property InsertMode: boolean read fInserting write SetInsertMode
      default true;
    property Keystrokes: TSynEditKeyStrokes
      read FKeystrokes write SetKeystrokes;
    property MouseActions: TSynEditMouseActions
      read GetMouseActions write SetMouseActions;
    property MouseTextActions: TSynEditMouseActions
      read GetMouseTextActions write SetMouseTextActions;
    property MouseSelActions: TSynEditMouseActions // Mouseactions, if mouse is over selection => fallback to normal
      read GetMouseSelActions write SetMouseSelActions;
    property MaxUndo: Integer read GetMaxUndo write SetMaxUndo default 1024;
    property Options: TSynEditorOptions read FOptions write SetOptions          // See SYNEDIT_UNIMPLEMENTED_OPTIONS for deprecated Values
      default SYNEDIT_DEFAULT_OPTIONS;
    property Options2: TSynEditorOptions2 read FOptions2 write SetOptions2
      default SYNEDIT_DEFAULT_OPTIONS2;
    property MouseOptions: TSynEditorMouseOptions read FMouseOptions write SetMouseOptions
      default SYNEDIT_DEFAULT_MOUSE_OPTIONS;
    property ShareOptions: TSynEditorShareOptions read FShareOptions write SetShareOptions
      default SYNEDIT_DEFAULT_SHARE_OPTIONS; experimental;
    property VisibleSpecialChars: TSynVisibleSpecialChars read FVisibleSpecialChars write SetVisibleSpecialChars;
    property OverwriteCaret: TSynEditCaretType read FOverwriteCaret
      write SetOverwriteCaret default ctBlock;
  protected
    property RightEdge: Integer read fRightEdge write SetRightEdge default 80;
    property RightEdgeColor: TColor
      read fRightEdgeColor write SetRightEdgeColor default clSilver;
    property ScrollBars: TScrollStyle
      read FScrollBars write SetScrollBars default ssBoth;
    property SelectedColor: TSynSelectedColor
    read GetSelectedColor write SetSelectedColor;  // Setter for compatibility
    property IncrementColor: TSynSelectedColor read GetIncrementColor;
    property HighlightAllColor: TSynSelectedColor read GetHighlightAllColor;
    property BracketMatchColor: TSynSelectedColor read GetBracketMatchColor;
    property MouseLinkColor: TSynSelectedColor read GetMouseLinkColor;
    property LineHighlightColor: TSynSelectedColor read GetLineHighlightColor;
    property FoldedCodeColor: TSynSelectedColor read GetFoldedCodeColor;
    property BracketHighlightStyle: TSynEditBracketHighlightStyle
      read GetBracketHighlightStyle write SetBracketHighlightStyle;
    property DefaultSelectionMode: TSynSelectionMode
      read GetDefSelectionMode write SetDefSelectionMode default smNormal;
    property SelectionMode: TSynSelectionMode
      read GetSelectionMode write SetSelectionMode default smNormal;
    property TabWidth: integer read fTabWidth write SetTabWidth default 8;
    property WantTabs: boolean read fWantTabs write SetWantTabs default FALSE;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeUpdating: TChangeUpdatingEvent read FOnChangeUpdating write FOnChangeUpdating;
    property OnCutCopy: TSynCopyPasteEvent read FOnCutCopy write FOnCutCopy;
    property OnPaste: TSynCopyPasteEvent read FOnPaste write FOnPaste;
    property OnCommandProcessed: TProcessCommandEvent
      read fOnCommandProcessed write fOnCommandProcessed;
    property OnDropFiles: TSynDropFilesEvent read fOnDropFiles write fOnDropFiles;
    property OnGutterClick: TGutterClickEvent
      read GetOnGutterClick write SetOnGutterClick;
    property OnPaint: TPaintEvent read fOnPaint write fOnPaint;
    // OnPlaceBookmark only triggers for Bookmarks
    property OnPlaceBookmark: TPlaceMarkEvent read FOnPlaceMark write FOnPlaceMark;
    // OnClearBookmark only triggers for Bookmarks
    property OnClearBookmark: TPlaceMarkEvent read FOnClearMark write FOnClearMark;
    property OnProcessUserCommand: TProcessCommandEvent
      read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnReplaceText: TReplaceTextEvent read fOnReplaceText
      write fOnReplaceText;
    property OnSpecialLineColors: TSpecialLineColorsEvent
      read FOnSpecialLineColors write SetSpecialLineColors;  deprecated;
    property OnSpecialLineMarkup: TSpecialLineMarkupEvent
      read FOnSpecialLineMarkup write SetSpecialLineMarkup;
    property OnStatusChange: TStatusChangeEvent
      read fOnStatusChange write fOnStatusChange;
  end;

  TSynEdit = class(TCustomSynEdit)
  published
    // inherited properties
    property Align;
    property Beautifier;
    property BlockIndent;
    property BlockTabIndent;
    property BorderSpacing;
{$IFDEF SYN_COMPILER_4_UP}
    property Anchors;
    property Constraints;
{$ENDIF}
    property Color;
    property Cursor default crIBeam;
    property Enabled;
    property Font;
    property Height;
    property Name;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property Visible;
    property Width;
    // inherited events
    property OnClick;
    property OnDblClick;
    property OnTripleClick;
    property OnQuadClick;
    property OnDragDrop;
    property OnDragOver;
{$IFDEF SYN_COMPILER_4_UP}
// ToDo Docking
    property OnEndDock;
{$ENDIF}
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnClickLink : TMouseEvent read FOnClickLink write FOnClickLink;
    property OnMouseLink: TSynMouseLinkEvent read FOnMouseLink write FOnMouseLink;
    property OnMouseEnter;
    property OnMouseLeave;
{$IFDEF SYN_COMPILER_4_UP}
// ToDo Docking
    property OnStartDock;
{$ENDIF}
    property OnStartDrag;
    // TCustomSynEdit properties
    property BookMarkOptions;
    property BorderStyle default bsSingle;
    property ExtraCharSpacing;
    property ExtraLineSpacing;
    property Gutter;
    property RightGutter;
    property HideSelection;
    property Highlighter;
    property InsertCaret;
    property InsertMode;
    property Keystrokes;
    property MouseActions;
    property MouseSelActions;
    property Lines;
    property MaxLeftChar;
    property MaxUndo;
    property Options;
    property Options2;
    property MouseOptions;
    property VisibleSpecialChars;
    property OverwriteCaret;
    property ReadOnly;
    property RightEdge;
    property RightEdgeColor;
    property ScrollBars;
    property SelectedColor;
    property IncrementColor;
    property HighlightAllColor;
    property BracketHighlightStyle;
    property BracketMatchColor;
    property FoldedCodeColor;
    property MouseLinkColor;
    property LineHighlightColor;
    property DefaultSelectionMode;
    property SelectionMode;
    property TabWidth;
    property WantTabs;
    // TCustomSynEdit events
    property OnChange;
    property OnChangeUpdating;
    property OnCutCopy;
    property OnPaste;
    property OnClearBookmark;                                                   // djlp 2000-08-29
    property OnCommandProcessed;
    property OnDropFiles;
    property OnGutterClick;
    property OnPaint;
    property OnPlaceBookmark;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnSpecialLineColors; deprecated;
    property OnSpecialLineMarkup;
    property OnStatusChange;
  end;

procedure Register;

implementation

const
  GutterTextDist = 2; //Pixel

type

  { TSynEditMarkListInternal }

  TSynEditMarkListInternal = class(TSynEditMarkList)
  private
    function GetLinesView: TSynEditStrings;
    procedure SetLinesView(const AValue: TSynEditStrings);
  protected
    procedure AddOwnerEdit(AEdit: TSynEditBase);
    procedure RemoveOwnerEdit(AEdit: TSynEditBase);
    property LinesView: TSynEditStrings read GetLinesView write SetLinesView;
  end;

  TSynStatusChangedHandlerList = Class(TSynFilteredMethodList)
  public
    procedure Add(AHandler: TStatusChangeEvent; Changes: TSynStatusChanges);
    procedure Remove(AHandler: TStatusChangeEvent);
    procedure CallStatusChangedHandlers(Sender: TObject; Changes: TSynStatusChanges);
  end;

  { TSynEditUndoCaret }

  TSynEditUndoCaret = class(TSynEditUndoItem)
  private
    FCaretPos: TPoint;
  protected
    function IsEqualContent(AnItem: TSynEditUndoItem): Boolean; override;
    function DebugString: String; override;
  public
    constructor Create(CaretPos: TPoint);
    function IsCaretInfo: Boolean; override;
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynEditUndoSelCaret }

  TSynEditUndoSelCaret = class(TSynEditUndoItem)
  private
    FCaretPos, FBeginPos, FEndPos: TPoint;
    FBlockMode: TSynSelectionMode;
  protected
    function IsEqualContent(AnItem: TSynEditUndoItem): Boolean; override;
    function DebugString: String; override;
  public
    function IsCaretInfo: Boolean; override;
    constructor Create(CaretPos, BeginPos, EndPos: TPoint; BlockMode: TSynSelectionMode);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynEditUndoIndent }

  TSynEditUndoIndent = class(TSynEditUndoItem)
  public
    FPosY1, FPosY2, FCnt, FTabCnt: Integer;
  public
    constructor Create(APosY, EPosY, ACnt, ATabCnt: Integer);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynEditUndoUnIndent }

  TSynEditUndoUnIndent = class(TSynEditUndoItem)
  public
    FPosY1, FPosY2: Integer;
    FText: String;
  public
    constructor Create(APosY, EPosY: Integer; AText: String);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynEditMouseGlobalActions }

  TSynEditMouseGlobalActions = class(TSynEditMouseInternalActions)
  protected
    procedure InitForOptions(AnOptions: TSynEditorMouseOptions); override;
  end;

  { TSynEditMouseTextActions }

  TSynEditMouseTextActions = class(TSynEditMouseInternalActions)
  protected
    procedure InitForOptions(AnOptions: TSynEditorMouseOptions); override;
  end;

  { TSynEditMouseSelActions }

  TSynEditMouseSelActions = class(TSynEditMouseInternalActions)
  protected
    procedure InitForOptions(AnOptions: TSynEditorMouseOptions); override;
  end;

  { THookedCommandHandlerEntry }

  THookedCommandHandlerEntry = class(TObject)
  private
    fEvent: THookedCommandEvent;
    fData: pointer;
    function Equals(AEvent: THookedCommandEvent): boolean; reintroduce;
  public
    constructor Create(AEvent: THookedCommandEvent; AData: pointer);
  end;


{ TSynEditUndoCaret }

function TSynEditUndoCaret.IsEqualContent(AnItem: TSynEditUndoItem): Boolean;
begin
  Result := (FCaretPos.x = TSynEditUndoCaret(AnItem).FCaretPos.x)
        and (FCaretPos.y = TSynEditUndoCaret(AnItem).FCaretPos.y);
end;

function TSynEditUndoCaret.DebugString: String;
begin
  Result := 'CaretPos='+dbgs(FCaretPos);
end;

constructor TSynEditUndoCaret.Create(CaretPos: TPoint);
begin
  FCaretPos := CaretPos;
  {$IFDEF SynUndoDebug}debugln(['---  Undo Insert ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
end;

function TSynEditUndoCaret.IsCaretInfo: Boolean;
begin
  Result := True;
end;

function TSynEditUndoCaret.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TCustomSynEdit;
  if Result then
    {$IFDEF SynUndoDebug}debugln(['---  Undo Perform ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
    with TCustomSynEdit(Caller) do begin
      FCaret.LineCharPos := FCaretPos;
      FTheLinesView.CurUndoList.AddChange(TSynEditUndoCaret.Create(FCaretPos));
    end;
end;

{ TSynEditUndoSelCaret }

constructor TSynEditUndoSelCaret.Create(CaretPos, BeginPos, EndPos: TPoint;
  BlockMode: TSynSelectionMode);
begin
  FCaretPos := CaretPos;
  FBeginPos := BeginPos;
  FEndPos   := EndPos;
  FBlockMode := BlockMode;
  {$IFDEF SynUndoDebug}debugln(['---  Undo Insert ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
end;

function TSynEditUndoSelCaret.IsEqualContent(AnItem: TSynEditUndoItem): Boolean;
begin
  Result := (FCaretPos.x = TSynEditUndoSelCaret(AnItem).FCaretPos.x)
        and (FCaretPos.y = TSynEditUndoSelCaret(AnItem).FCaretPos.y)
        and (FBeginPos.x = TSynEditUndoSelCaret(AnItem).FBeginPos.x)
        and (FBeginPos.y = TSynEditUndoSelCaret(AnItem).FBeginPos.y)
        and (FEndPos.x = TSynEditUndoSelCaret(AnItem).FEndPos.x)
        and (FEndPos.y = TSynEditUndoSelCaret(AnItem).FEndPos.y)
        and (FBlockMode = TSynEditUndoSelCaret(AnItem).FBlockMode);
end;

function TSynEditUndoSelCaret.DebugString: String;
begin
  Result := 'CaretPos='+dbgs(FCaretPos) + ' Begin=' + dbgs(FBeginPos) + ' End=' + dbgs(FEndPos) + ' Mode=' + dbgs(ord(FBlockMode));
end;

function TSynEditUndoSelCaret.IsCaretInfo: Boolean;
begin
  Result := True;
end;

function TSynEditUndoSelCaret.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TCustomSynEdit;
  if Result then
    {$IFDEF SynUndoDebug}debugln(['---  Undo Perform ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
    with TCustomSynEdit(Caller) do begin
      SetCaretAndSelection(FCaretPos, FBeginPos, FEndPos, FBlockMode, True);
      FTheLinesView.CurUndoList.AddChange(TSynEditUndoSelCaret.Create(FCaretPos, FBeginPos,
                                                     FEndPos, FBlockMode));
    end;
end;

{ TSynEditUndoIndent }

constructor TSynEditUndoIndent.Create(APosY, EPosY, ACnt, ATabCnt: Integer);
begin
  FPosY1 := APosY;
  FPosY2 := EPosY;
  FCnt    :=  ACnt;
  FTabCnt := ATabCnt;
end;

function TSynEditUndoIndent.PerformUndo(Caller: TObject): Boolean;
begin
  Result := False;
end;

{ TSynEditUndoUnIndent }

constructor TSynEditUndoUnIndent.Create(APosY, EPosY: Integer; AText: String);
begin
  FPosY1 := APosY;
  FPosY2 := EPosY;
  FText :=  AText;
end;

function TSynEditUndoUnIndent.PerformUndo(Caller: TObject): Boolean;
begin
  Result := False;
end;

function Roundoff(X: Extended): Longint;
begin
  if (x >= 0) then begin
    Result := TruncToInt(x + 0.5)
  end else begin
    Result := TruncToInt(x - 0.5);
  end;
end;

{ TSynEditMouseGlobalActions }

procedure TSynEditMouseGlobalActions.InitForOptions(AnOptions: TSynEditorMouseOptions);
begin
  AddCommand(emcWheelScrollDown,       False,  mbWheelDown, ccAny, cdDown, [], []);
  AddCommand(emcWheelScrollUp,         False,  mbWheelUp, ccAny, cdDown, [], []);
end;

{ TSynEditMouseTextActions }

procedure TSynEditMouseTextActions.InitForOptions(AnOptions: TSynEditorMouseOptions);
var
  rmc: Boolean;
begin
  Clear;
  rmc := (emRightMouseMovesCursor in AnOptions);
  //// eoRightMouseMovesCursor
  //if (eoRightMouseMovesCursor in ChangedOptions) then begin
  //  for i := FMouseActions.Count-1 downto 0 do
  //    if FMouseActions[i].Button = mbRight then
  //      FMouseActions[i].MoveCaret := (eoRightMouseMovesCursor in fOptions);
  //end;

  AddCommand(emcStartSelections,       True,  mbLeft, ccSingle, cdDown, [],        [ssShift, ssAlt], emcoSelectionStart);
  AddCommand(emcStartSelections,       True,  mbLeft, ccSingle, cdDown, [ssShift], [ssShift, ssAlt], emcoSelectionContinue);
  if (emAltSetsColumnMode in AnOptions) then begin
    AddCommand(emcStartColumnSelections, True,  mbLeft, ccSingle, cdDown, [ssAlt],          [ssShift, ssAlt], emcoSelectionStart);
    AddCommand(emcStartColumnSelections, True,  mbLeft, ccSingle, cdDown, [ssShift, ssAlt], [ssShift, ssAlt], emcoSelectionContinue);
  end;
  if (emShowCtrlMouseLinks in AnOptions) then
    AddCommand(emcMouseLink,             False, mbLeft, ccSingle, cdUp, [SYNEDIT_LINK_MODIFIER], [ssShift, ssAlt, ssCtrl]);

  if (emDoubleClickSelectsLine in AnOptions) then begin
    AddCommand(emcSelectLine,            True,  mbLeft, ccDouble, cdDown, [], []);
    AddCommand(emcSelectPara,            True,  mbLeft, ccTriple, cdDown, [], []);
  end
  else begin
    AddCommand(emcSelectWord,            True,  mbLeft, ccDouble, cdDown, [], []);
    AddCommand(emcSelectLine,            True,  mbLeft, ccTriple, cdDown, [], []);
  end;
  AddCommand(emcSelectPara,            True,  mbLeft, ccQuad,   cdDown, [], []);

  AddCommand(emcContextMenu,           rmc,   mbRight, ccSingle, cdUp, [], [], emcoSelectionCaretMoveNever);

  AddCommand(emcPasteSelection,        True,  mbMiddle, ccSingle, cdDown, [], []);
end;

{ TSynEditMouseSelActions }

procedure TSynEditMouseSelActions.InitForOptions(AnOptions: TSynEditorMouseOptions);
begin
  Clear;
  //rmc := (eoRightMouseMovesCursor in AnOptions);

  if (emDragDropEditing in AnOptions) then
    AddCommand(emcStartDragMove, False, mbLeft, ccSingle, cdDown, [], []);
end;

{ THookedCommandHandlerEntry }

constructor THookedCommandHandlerEntry.Create(AEvent: THookedCommandEvent;
  AData: pointer);
begin
  inherited Create;
  fEvent := AEvent;
  fData := AData;
end;

function THookedCommandHandlerEntry.Equals(AEvent: THookedCommandEvent): boolean;
begin
  with TMethod(fEvent) do
    Result := (Code = TMethod(AEvent).Code) and (Data = TMethod(AEvent).Data);
end;

procedure InitSynDefaultFont;
begin
  if SynDefaultFontName <> '' then exit;
  Screen.Fonts;
  {$UNDEF SynDefaultFont}
  {$IFDEF LCLgtk}
    SynDefaultFontName   := '-adobe-courier-medium-r-normal-*-*-140-*-*-*-*-iso10646-1';
    SynDefaultFontHeight := 14;
    {$DEFINE SynDefaultFont}
  {$ENDIF}
  {$IFDEF LCLcarbon}
    SynDefaultFontName   := 'Monaco'; // Note: carbon is case sensitive
    SynDefaultFontHeight := 12;
    {$DEFINE SynDefaultFont}
  {$ENDIF}
  // LCLgtk2 and LCLQt use default settings
  {$IFnDEF SynDefaultFont}
    SynDefaultFontName   := 'Courier New';
    SynDefaultFontHeight := -13;
  {$ENDIF}
  if Screen.Fonts.IndexOf(SynDefaultFontName) >= 0 then
    exit;
  if Screen.Fonts.IndexOf('DejaVu Sans Mono') >= 0 then begin
    SynDefaultFontName   := 'DejaVu Sans Mono';
    SynDefaultFontHeight := 13;
  end;
end;

{ TCustomSynEdit }

procedure TCustomSynEdit.AquirePrimarySelection;
var
  FormatList: Array [0..1] of TClipboardFormat;
begin
  if (not SelAvail)
  or (PrimarySelection.OnRequest=@PrimarySelectionRequest) then exit;
  FormatList[0] := CF_TEXT;
  FormatList[1] := TSynClipboardStream.ClipboardFormatId;
  try
    PrimarySelection.SetSupportedFormats(2, @FormatList[0]);
    PrimarySelection.OnRequest:=@PrimarySelectionRequest;
  except
  end;
end;

function TCustomSynEdit.GetChangeStamp: int64;
begin
  Result := TSynEditStringList(FLines).TextChangeStamp;
end;

function TCustomSynEdit.GetDefSelectionMode: TSynSelectionMode;
begin
  Result := FBlockSelection.SelectionMode;
end;

function TCustomSynEdit.GetFoldState: String;
begin
  Result := FFoldedLinesView.GetFoldDescription(0, 0, -1, -1, True);
end;

function TCustomSynEdit.GetModified: Boolean;
begin
  Result := TSynEditStringList(FLines).Modified;
end;

function TCustomSynEdit.GetMouseActions: TSynEditMouseActions;
begin
  Result := FMouseActions.UserActions;
end;

function TCustomSynEdit.GetMouseSelActions: TSynEditMouseActions;
begin
  Result := FMouseSelActions.UserActions;
end;

function TCustomSynEdit.GetMouseTextActions: TSynEditMouseActions;
begin
  Result := FMouseTextActions.UserActions;
end;

function TCustomSynEdit.GetPaintLockOwner: TSynEditBase;
begin
  Result := TSynEditStringList(FLines).PaintLockOwner;
end;

function TCustomSynEdit.GetPlugin(Index: Integer): TSynEditPlugin;
begin
  Result := TSynEditPlugin(fPlugins[Index]);
end;

function TCustomSynEdit.GetTextBetweenPoints(aStartPoint, aEndPoint: TPoint): String;
begin
  FInternalBlockSelection.SelectionMode := smNormal;
  FInternalBlockSelection.StartLineBytePos := aStartPoint;
  FInternalBlockSelection.EndLineBytePos := aEndPoint;
  Result := FInternalBlockSelection.SelText;
end;

procedure TCustomSynEdit.SetBlockTabIndent(AValue: integer);
begin
  if FBlockTabIndent = AValue then Exit;
  FBlockTabIndent := AValue;
end;

procedure TCustomSynEdit.SetDefSelectionMode(const AValue: TSynSelectionMode);
begin
  FBlockSelection.SelectionMode := AValue; // Includes active
end;

procedure TCustomSynEdit.SurrenderPrimarySelection;
begin
  if PrimarySelection.OnRequest=@PrimarySelectionRequest then
    PrimarySelection.OnRequest:=nil;
end;

function TCustomSynEdit.PixelsToRowColumn(Pixels: TPoint; aFlags: TSynCoordinateMappingFlags = [scmLimitToLines]): TPoint;
// converts the client area coordinate
// to Caret position (screen position, (1,1) based)
// To get the text/physical position use PixelsToLogicalPos
var
  f: Single;
begin
  f := ( (Pixels.X
          + (fLeftChar-1) * fCharWidth
          - TextLeftPixelOffset
         ) / fCharWidth
       )+1;
  if (not(scmIncludePartVisible in aFlags)) and (Pixels.Y >= fLinesInWindow * fTextHeight) then begin
    // don't return a partially visible last line
    Pixels.Y := fLinesInWindow * fTextHeight - 1;
    if Pixels.Y < 0 then Pixels.Y := 0;
  end;
  Result := Point(RoundOff(f), ScreenRowToRow(Pixels.Y div fTextHeight, scmLimitToLines in aFlags));
  {$IFDEF SYN_MBCSSUPPORT}
  if (Result.Y >= 1) and (Result.Y <= Lines.Count) then begin
    s := Lines[Result.Y - 1];
    if (Length(s) >= Result.x) and (ByteType(s, Result.X) = mbTrailByte) then
      if Frac(f) >= 0.5 then
        Dec(Result.X)
      else
        Inc(Result.X);
  end;
  fMBCSStepAside := False;
  {$ENDIF}
end;

function TCustomSynEdit.PixelsToLogicalPos(const Pixels: TPoint): TPoint;
begin
  Result:=PhysicalToLogicalPos(PixelsToRowColumn(Pixels));
end;

function TCustomSynEdit.ScreenRowToRow(ScreenRow: integer; LimitToLines: Boolean = True): integer;
// ScreenRow is 0-base
// result is 1-based
begin
  Result := FFoldedLinesView.ScreenLineToTextIndex(ScreenRow)+1;
  if LimitToLines and (Result >= Lines.Count) then
    Result := Lines.Count;
//  DebugLn(['=== SrceenRow TO Row   In:',ScreenRow,'  out:',Result, ' topline=',TopLine, '  view topline=',FFoldedLinesView.TopLine]);
end;

function TCustomSynEdit.RowToScreenRow(PhysicalRow: integer): integer;
// returns -1 for lines above visible screen (<TopLine)
// 0 for the first line
// 0 to LinesInWindow for visible lines (incl last partial visble line)
// and returns LinesInWindow+1 for lines below visible screen
begin
  Result := FFoldedLinesView.TextIndexToScreenLine(PhysicalRow-1);
  if Result < -1 then Result := -1;
  if Result > LinesInWindow+1 then Result := LinesInWindow+1;
//  DebugLn(['=== Row TO ScreenRow   In:',PhysicalRow,'  out:',Result]);
end;

function TCustomSynEdit.RowColumnToPixels(
  const RowCol: TPoint): TPoint;
// converts screen position (1,1) based
// to client area coordinate (0,0 based on canvas)
begin
  Result:=RowCol;
  Result.X := (Result.X - 1) * fCharWidth + fTextOffset;
  Result.Y := RowToScreenRow(RowCol.Y) * fTextHeight;
end;

procedure TCustomSynEdit.ComputeCaret(X, Y: Integer);
// set caret to pixel position
begin
  FCaret.LineCharPos := PixelsToRowColumn(Point(X,Y));
end;

procedure TCustomSynEdit.DoCopyToClipboard(SText: string; FoldInfo: String = '');
var
  ClipHelper: TSynClipboardStream;
  PasteAction: TSynCopyPasteAction;
  PMode: TSynSelectionMode;
begin
  PasteAction := scaContinue;
  if length(FoldInfo) = 0 then PasteAction := scaPlainText;
  PMode :=  SelectionMode;
  if assigned(FOnCutCopy) then begin
    FOnCutCopy(self, SText, PMode, FBlockSelection.FirstLineBytePos, PasteAction);
    if PasteAction = scaAbort then
      exit;;
  end;

  if SText = '' then exit;
  Clipboard.Clear;
  ClipHelper := TSynClipboardStream.Create;
  try
    ClipHelper.Text := SText;
    ClipHelper.SelectionMode := PMode; // TODO if scaPlainText and smNormal, then avoid synedits own clipboard format

    if PasteAction = scaContinue then begin
      // Fold
      if length(FoldInfo) > 0 then
        ClipHelper.AddTag(synClipTagFold, @FoldInfo[1], length(FoldInfo));
    end;

    if not ClipHelper.WriteToClipboard(Clipboard) then begin
      {$IFDEF SynClipboardExceptions}raise ESynEditError.Create('Clipboard copy operation failed');{$ENDIF}
    end;
  finally
    ClipHelper.Free;
  end;
end;

procedure TCustomSynEdit.CopyToClipboard;
var
  FInfo: String;
begin
  if SelAvail then begin
    if eoFoldedCopyPaste in fOptions2 then
      FInfo := FFoldedLinesView.GetFoldDescription(
        FBlockSelection.FirstLineBytePos.Y - 1, FBlockSelection.FirstLineBytePos.X,
        FBlockSelection.LastLineBytePos.Y - 1,  FBlockSelection.LastLineBytePos.X);
    DoCopyToClipboard(SelText, FInfo);
  end;
end;

procedure TCustomSynEdit.CutToClipboard;
var
  FInfo: String;
begin
  if SelAvail then begin
    if eoFoldedCopyPaste in fOptions2 then
      FInfo := FFoldedLinesView.GetFoldDescription(
        FBlockSelection.FirstLineBytePos.Y - 1, FBlockSelection.FirstLineBytePos.X,
        FBlockSelection.LastLineBytePos.Y - 1,  FBlockSelection.LastLineBytePos.X);
    DoCopyToClipboard(SelText, FInfo);
    SetSelTextExternal('');
  end;
end;

constructor TCustomSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetInline(True);
  ControlStyle:=ControlStyle+[csOwnedChildrenNotSelectable];
  FScrollBarUpdateLock := 0;

  FStatusChangedList := TSynStatusChangedHandlerList.Create;

  FDefaultBeautifier := TSynBeautifier.Create(self);
  FBeautifier := FDefaultBeautifier;

  FLines := TSynEditStringList.Create;
  TSynEditStringList(FLines).AttachSynEdit(Self);

  FCaret := TSynEditCaret.Create;
  FCaret.MaxLeftChar := @CurrentMaxLineLen;
  FCaret.AddChangeHandler({$IFDEF FPC}@{$ENDIF}CaretChanged);
  FInternalCaret := TSynEditCaret.Create;
  FInternalCaret.MaxLeftChar := @CurrentMaxLineLen;

  // Create the lines/views
  FTrimmedLinesView := TSynEditStringTrimmingList.Create(fLines, fCaret);

  FDoubleWidthChrLinesView := SynEditStringDoubleWidthChars.Create
                                                            (FTrimmedLinesView);

  // ftab, currently has LengthOfLongestLine, therefore must be after DoubleWidthChar
  FTabbedLinesView := TSynEditStringTabExpander.Create(FDoubleWidthChrLinesView);

  FFoldedLinesView := TSynEditFoldedView.Create(FTabbedLinesView, fCaret);
  FFoldedLinesView.OnFoldChanged := {$IFDEF FPC}@{$ENDIF}FoldChanged;
  FFoldedLinesView.OnLineInvalidate := {$IFDEF FPC}@{$ENDIF}InvalidateGutterLines;

  // Pointer to the First/Lowest View
  // TODO: this should be Folded...
  FTheLinesView := FTabbedLinesView;
  FTopLinesView := FTrimmedLinesView;
  // External Accessor
  FStrings := TSynEditLines.Create(TSynEditStringList(FLines), {$IFDEF FPC}@{$ENDIF}MarkTextAsSaved);

  FCaret.Lines := FTheLinesView;
  FInternalCaret.Lines := FTheLinesView;
  FFontDummy := TFont.Create;
  FOldWidth := -1;
  FOldHeight := -1;

  with TSynEditStringList(fLines) do begin
    AddChangeHandler(senrLineCount, {$IFDEF FPC}@{$ENDIF}LineCountChanged);
    AddChangeHandler(senrLineChange, {$IFDEF FPC}@{$ENDIF}LineTextChanged);
    AddChangeHandler(senrHighlightChanged, {$IFDEF FPC}@{$ENDIF}DoHighlightChanged);
    AddNotifyHandler(senrCleared, {$IFDEF FPC}@{$ENDIF}ListCleared);
    AddNotifyHandler(senrUndoRedoAdded, {$IFDEF FPC}@{$ENDIF}Self.UndoRedoAdded);
    AddNotifyHandler(senrModifiedChanged, {$IFDEF FPC}@{$ENDIF}ModifiedChanged);
    AddNotifyHandler(senrIncPaintLock, {$IFDEF FPC}@{$ENDIF}DoIncPaintLock);
    AddNotifyHandler(senrDecPaintLock, {$IFDEF FPC}@{$ENDIF}DoDecPaintLock);
    AddNotifyHandler(senrIncOwnedPaintLock, {$IFDEF FPC}@{$ENDIF}DoIncForeignPaintLock);
    AddNotifyHandler(senrDecOwnedPaintLock, {$IFDEF FPC}@{$ENDIF}DoDecForeignPaintLock);
  end;

  FScreenCaret := TSynEditScreenCaret.Create(Self);
  FScreenCaret.OnExtraLineCharsChanged := {$IFDEF FPC}@{$ENDIF}ExtraLineCharsChanged;

  FUndoList := TSynEditStringList(fLines).UndoList;
  FRedoList := TSynEditStringList(fLines).RedoList;
  FUndoList.OnNeedCaretUndo := {$IFDEF FPC}@{$ENDIF}GetCaretUndo;

  FBlockSelection := TSynEditSelection.Create(FTheLinesView, True);
  FBlockSelection.Caret := FCaret;
  FBlockSelection.InvalidateLinesMethod := {$IFDEF FPC}@{$ENDIF}InvalidateLines;
  FBlockSelection.AddChangeHandler({$IFDEF FPC}@{$ENDIF}DoBlockSelectionChanged);

  FInternalBlockSelection := TSynEditSelection.Create(FTheLinesView, False);
  FInternalBlockSelection.InvalidateLinesMethod := {$IFDEF FPC}@{$ENDIF}InvalidateLines;
  // No need for caret, on interanl block

  FFoldedLinesView.BlockSelection := FBlockSelection;

  FWordBreaker := TSynWordBreaker.Create;

  RecreateMarkList;

  {$IFNDEF EnableDoubleBuf}
  DoubleBuffered := True;
  {$ENDIF}

  fTextDrawer := TheTextDrawer.Create([fsBold], fFontDummy);
  FPaintLineColor := TSynSelectedColor.Create;
  FPaintLineColor2 := TSynSelectedColor.Create;
  fBookMarkOpt := TSynBookMarkOpt.Create(Self);
  fBookMarkOpt.OnChange := {$IFDEF FPC}@{$ENDIF}BookMarkOptionsChanged;
// fRightEdge has to be set before FontChanged is called for the first time
  fRightEdge := 80;
  FLeftGutter := CreateGutter(self, gsLeft, FTextDrawer);
  FLeftGutter.OnChange := {$IFDEF FPC}@{$ENDIF}GutterChanged;
  FLeftGutter.OnResize := {$IFDEF FPC}@{$ENDIF}GutterResized;
  FRightGutter := CreateGutter(self, gsRight, FTextDrawer);
  FRightGutter.OnChange := {$IFDEF FPC}@{$ENDIF}GutterChanged;
  FRightGutter.OnResize := {$IFDEF FPC}@{$ENDIF}GutterResized;

  fTextOffset := TextLeftPixelOffset;

  ControlStyle := ControlStyle + [csOpaque, csSetCaption, csTripleClicks, csQuadClicks];
  Height := 150;
  Width := 200;
  Cursor := crIBeam;
  fPlugins := TList.Create;
  FHookedKeyTranslationList := TSynHookedKeyTranslationList.Create;
  // needed before setting color
  fMarkupHighCaret := TSynEditMarkupHighlightAllCaret.Create(self);
  fMarkupHighCaret.Selection := FBlockSelection;
  fMarkupHighAll   := TSynEditMarkupHighlightAll.Create(self);
  fMarkupBracket   := TSynEditMarkupBracket.Create(self);
  fMarkupWordGroup := TSynEditMarkupWordGroup.Create(self);
  fMarkupCtrlMouse := TSynEditMarkupCtrlMouseLink.Create(self);
  fMarkupSpecialLine := TSynEditMarkupSpecialLine.Create(self);
  fMarkupSelection := TSynEditMarkupSelection.Create(self, FBlockSelection);
  fMarkupSpecialChar := TSynEditMarkupSpecialChar.Create(self);

  fMarkupManager := TSynEditMarkupManager.Create(self);
  fMarkupManager.AddMarkUp(fMarkupSpecialChar);
  fMarkupManager.AddMarkUp(fMarkupSpecialLine);
  fMarkupManager.AddMarkUp(fMarkupHighCaret);
  fMarkupManager.AddMarkUp(fMarkupHighAll);
  fMarkupManager.AddMarkUp(fMarkupCtrlMouse);
  fMarkupManager.AddMarkUp(fMarkupBracket);
  fMarkupManager.AddMarkUp(fMarkupWordGroup);
  fMarkupManager.AddMarkUp(fMarkupSelection);
  fMarkupManager.Lines := FTheLinesView;
  fMarkupManager.Caret := FCaret;
  fMarkupManager.InvalidateLinesMethod := @InvalidateLines;

  Color := clWhite;
  fFontDummy.Name := SynDefaultFontName;
  fFontDummy.Height := SynDefaultFontHeight;
  fFontDummy.Pitch := SynDefaultFontPitch;
  fFontDummy.Quality := SynDefaultFontQuality;
  fLastMouseCaret := Point(-1,-1);
  FLastMousePoint := Point(-1,-1);
  fBlockIndent := 2;

  Font.Assign(fFontDummy);
  Font.OnChange := {$IFDEF FPC}@{$ENDIF}FontChanged;
  FontChanged(nil);
  ParentFont := False;
  ParentColor := False;
  TabStop := True;
  fInserting := True;
  fMaxLeftChar := 1024;
  ScrollBars := ssBoth;
  BorderStyle := bsSingle;
  fInsertCaret := ctVerticalLine;
  fOverwriteCaret := ctBlock;
  FKeystrokes := TSynEditKeyStrokes.Create(Self);
  FLastKeyStrokes := nil;
  if assigned(Owner) and not (csLoading in Owner.ComponentState) then begin
    SetDefaultKeystrokes;
  end;

  FMouseActions     := TSynEditMouseGlobalActions.Create(Self);
  FMouseSelActions  := TSynEditMouseSelActions.Create(Self);
  FMouseTextActions := TSynEditMouseTextActions.Create(Self);
  FMouseActionSearchHandlerList := TSynEditMouseActionSearchList.Create;
  FMouseActionExecHandlerList  := TSynEditMouseActionExecList.Create;

  fRightEdgeColor := clSilver;
{$IFDEF SYN_MBCSSUPPORT}
  fImeCount := 0;
  fMBCSStepAside := False;
{$ENDIF}
  fWantTabs := False;
  fTabWidth := 8;
  fLeftChar := 1;
  fTopLine := 1;
  fLinesInWindow := -1;
  fCharsInWindow := -1;
  FOldTopLine := 1;
  FOldTopView := 1;
  FFoldedLinesView.TopLine := 1;
  // find / replace
  fTSearch := TSynEditSearch.Create;
  FOptions := SYNEDIT_DEFAULT_OPTIONS;
  FOptions2 := SYNEDIT_DEFAULT_OPTIONS2;
  FMouseOptions := SYNEDIT_DEFAULT_MOUSE_OPTIONS;
  FShareOptions := SYNEDIT_DEFAULT_SHARE_OPTIONS;
  FVisibleSpecialChars := SYNEDIT_DEFAULT_VISIBLESPECIALCHARS;
  fMarkupSpecialChar.VisibleSpecialChars := SYNEDIT_DEFAULT_VISIBLESPECIALCHARS;
  UpdateOptions;
  UpdateOptions2;
  UpdateMouseOptions;
  fScrollTimer := TTimer.Create(Self);
  fScrollTimer.Enabled := False;
  fScrollTimer.Interval := 100;
  fScrollTimer.OnTimer := {$IFDEF FPC}@{$ENDIF}ScrollTimerHandler;
end;

function TCustomSynEdit.GetChildOwner: TComponent;
begin
  result := self;
end;

procedure TCustomSynEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  if root = self then begin
    Proc(FLeftGutter.Parts);
    // only save right gutter, if it has gutter-parts
    // move to parts-class
    if FRightGutter.Parts.Count > 0 then
      Proc(FRightGutter.Parts);
  end;
end;

procedure TCustomSynEdit.CreateParams(var Params: TCreateParams);
(*
const
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL, WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL);
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
*)
begin
  inherited CreateParams(Params);
(*
  with Params do begin
    {$IFOPT R+}{$DEFINE RangeCheckOn}{$R-}{$ENDIF}
    WindowClass.Style := WindowClass.Style and not Cardinal(ClassStylesOff);
    Style := Style or ScrollBar[FScrollBars] or BorderStyles[BorderStyle]
      or WS_CLIPCHILDREN;
    {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
    if NewStyleControls {$IFNDEF SYN_LAZARUS}and Ctl3D{$ENDIF} and (BorderStyle = bsSingle) then begin
      Style := Style and not Cardinal(WS_BORDER);
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
*)
end;

procedure TCustomSynEdit.IncPaintLock;
begin
  if FIsInDecPaintLock then exit;
  if (PaintLockOwner = nil) then begin
    PaintLockOwner := Self;
    FLines.SendNotification(senrIncOwnedPaintLock, Self);  // DoIncForeignPaintLock
  end;
  inc(FPaintLockOwnerCnt);
  if FPaintLockOwnerCnt = 1 then
    FLines.BeginUpdate(Self);
end;

procedure TCustomSynEdit.DecPaintLock;
begin
  if FIsInDecPaintLock then exit;
  if FPaintLockOwnerCnt = 1 then
    FLines.EndUpdate(Self);
  dec(FPaintLockOwnerCnt);
  if (PaintLockOwner = Self) and (FPaintLockOwnerCnt = 0) then begin
    FLines.SendNotification(senrDecOwnedPaintLock, Self);  // DoDecForeignPaintLock
    PaintLockOwner := nil;
  end;
end;

procedure TCustomSynEdit.DoIncForeignPaintLock(Sender: TObject);
begin
  if Sender = Self then exit;
  FCaret.IncAutoMoveOnEdit;
  FBlockSelection.IncPersistentLock;
end;

procedure TCustomSynEdit.DoDecForeignPaintLock(Sender: TObject);
begin
  if Sender = Self then exit;
  FBlockSelection.DecPersistentLock;
  FCaret.DecAutoMoveOnEdit;
end;

procedure TCustomSynEdit.SetUpdateState(NewUpdating: Boolean; Sender: TObject);
begin
  if assigned(FOnChangeUpdating) then
    FOnChangeUpdating(Self, NewUpdating);
end;

procedure TCustomSynEdit.DoIncPaintLock(Sender: TObject);
begin
  if FIsInDecPaintLock then exit;
  if FPaintLock = 0 then begin
    SetUpdateState(True, Self);
    FInvalidateRect := Rect(-1, -1, -2, -2);
    FOldTopLine := FTopLine;
    FOldTopView := TopView;
    FLastTextChangeStamp := TSynEditStringList(FLines).TextChangeStamp;
  end;
  inc(FPaintLock);
  FMarkupManager.IncPaintLock;
  FFoldedLinesView.Lock; //DecPaintLock triggers ScanFrom, and folds must wait
  FTrimmedLinesView.Lock; // Lock before caret
  FBlockSelection.Lock;
  FCaret.Lock;
  FScreenCaret.Lock;
end;

procedure TCustomSynEdit.DoDecPaintLock(Sender: TObject);
begin
  if FIsInDecPaintLock then exit;
  FIsInDecPaintLock := True;
  try
    if (FPaintLock=1) and HandleAllocated then begin
      ScanRanges(FLastTextChangeStamp <> TSynEditStringList(FLines).TextChangeStamp);
      if sfAfterLoadFromFileNeeded in fStateFlags then
        AfterLoadFromFile;
      if FChangedLinesStart > 0 then begin
        InvalidateLines(FChangedLinesStart, FChangedLinesEnd);
        InvalidateGutterLines(FChangedLinesStart, FChangedLinesEnd);
      end;
      FChangedLinesStart:=0;
      FChangedLinesEnd:=0;
    end;
    FCaret.Unlock;            // Maybe after FFoldedLinesView
    FBlockSelection.Unlock;
    FTrimmedLinesView.UnLock; // Must be unlocked after caret // May Change lines
    FFoldedLinesView.UnLock;  // after ScanFrom, but before UpdateCaret
    FMarkupManager.DecPaintLock;
    Dec(FPaintLock);
    if (FPaintLock = 0) and HandleAllocated then begin
      ScrollAfterTopLineChanged;
      if sfScrollbarChanged in fStateFlags then
        UpdateScrollbars;
      // must be past UpdateScrollbars; but before UpdateCaret (for ScrollBar-Auto-show)
      if sfEnsureCursorPos in fStateFlags then
        EnsureCursorPosVisible;              // TODO: This may call SetTopLine, change order
                                             // This does Paintlock, should be before final decrease
      // Must be after EnsureCursorPosVisible (as it does MoveCaretToVisibleArea)
      if FCaret.LinePos > FLines.Count then
        FCaret.LinePos := FLines.Count;
      if sfCaretChanged in fStateFlags then
        UpdateCaret;
      //if sfScrollbarChanged in fStateFlags then
      //  UpdateScrollbars;
      fMarkupHighCaret.CheckState; // Todo: need a global lock, including the markup
                                   // Todo: Markup can do invalidation, should be before ScrollAfterTopLineChanged;
    end;
    if (FPaintLock = 0) then begin
      FBlockSelection.AutoExtend := False;
      if fStatusChanges <> [] then
        DoOnStatusChange(fStatusChanges);
    end;
  finally
    FScreenCaret.UnLock;
    FIsInDecPaintLock := False;
    if FPaintLock = 0 then begin
      SetUpdateState(False, Self);
      if FInvalidateRect.Bottom > FInvalidateRect.Top then begin
        InvalidateRect(Handle, @FInvalidateRect, False);
        {$IFDEF SynCheckPaintLock}
        debugln('Returning from Paintlock, wich had Paint called while active');
        DumpStack;
        {$ENDIF}
      end;
    end;
  end;
end;

destructor TCustomSynEdit.Destroy;
var
  i: integer;
begin
  Application.RemoveOnIdleHandler(@IdleScanRanges);
  SurrenderPrimarySelection;
  Highlighter := nil;
  Beautifier:=nil;
  FFoldedLinesView.BlockSelection := nil;
  // free listeners while other fields are still valid
  if Assigned(fHookedCommandHandlers) then begin
    for i := 0 to fHookedCommandHandlers.Count - 1 do
      THookedCommandHandlerEntry(fHookedCommandHandlers[i]).Free;
    FreeAndNil(fHookedCommandHandlers);
  end;
  if fPlugins <> nil then begin
    for i := fPlugins.Count - 1 downto 0 do
      if TSynEditPlugin(fPlugins[i]).OwnedByEditor then
        TSynEditPlugin(fPlugins[i]).Free
      else
        TSynEditPlugin(fPlugins[i]).Editor := nil;
    FreeAndNil(fPlugins);
  end;
  RemoveHandlers;

  FreeAndNil(FHookedKeyTranslationList);
  fHookedCommandHandlers:=nil;
  fPlugins:=nil;
  FCaret.Lines := nil;
  FInternalCaret.Lines := nil;
  FMarkList.UnRegisterChangeHandler({$IFDEF FPC}@{$ENDIF}MarkListChange);
  FreeAndNil(fTSearch);
  FreeAndNil(fMarkupManager);
  FreeAndNil(fBookMarkOpt);
  FreeAndNil(fKeyStrokes);
  FreeAndNil(FMouseActionSearchHandlerList);
  FreeAndNil(FMouseActionExecHandlerList);
  FreeAndNil(FMouseActions);
  FreeAndNil(FMouseSelActions);
  FreeAndNil(FMouseTextActions);
  FreeAndNil(FLeftGutter);
  FreeAndNil(FRightGutter);
  FreeAndNil(FPaintLineColor);
  FreeAndNil(FPaintLineColor2);
  FreeAndNil(fTextDrawer);
  FreeAndNil(fFontDummy);
  DestroyMarkList; // before detach from FLines
  FreeAndNil(FWordBreaker);
  FreeAndNil(FFoldedLinesView); // has reference to caret
  FreeAndNil(FInternalBlockSelection);
  FreeAndNil(FBlockSelection);
  FreeAndNil(FStrings);
  FreeAndNil(FTabbedLinesView);
  FreeAndNil(FTrimmedLinesView); // has reference to caret
  FreeAndNil(FDoubleWidthChrLinesView);
  TSynEditStringList(FLines).DetachSynEdit(Self);
  if TSynEditStringList(FLines).AttachedSynEditCount = 0 then
    FreeAndNil(fLines);
  FreeAndNil(fCaret);
  FreeAndNil(fInternalCaret);
  FreeAndNil(FScreenCaret);
  FreeAndNil(FStatusChangedList);
  FBeautifier := nil;
  FreeAndNil(FDefaultBeautifier);
  inherited Destroy;
end;

function TCustomSynEdit.GetBlockBegin: TPoint;
begin
  Result := FBlockSelection.FirstLineBytePos;
end;

function TCustomSynEdit.GetBlockEnd: TPoint;
begin
  Result := FBlockSelection.LastLineBytePos;
end;

function TCustomSynEdit.GetBracketHighlightStyle: TSynEditBracketHighlightStyle;
begin
  Result := fMarkupBracket.HighlightStyle;
end;

function TCustomSynEdit.CaretXPix: Integer;
var
  p: TPoint;
begin
  p := Point(CaretX, CaretY);
  Result := RowColumnToPixels(p).X;
end;

function TCustomSynEdit.CaretYPix: Integer;
begin
  Result := RowColumnToPixels(Point(1, CaretY)).Y;
end;

procedure TCustomSynEdit.FontChanged(Sender: TObject);
begin
  RecalcCharExtent;
  SizeOrFontChanged(TRUE);
end;

function TCustomSynEdit.GetTextBuffer: TSynEditStrings;
begin
  Result := FLines;
end;

function TCustomSynEdit.GetLineText: string;
begin
  Result := FCaret.LineText;
end;

function TCustomSynEdit.GetMarkupByClass(Index: TSynEditMarkupClass): TSynEditMarkup;
begin
  Result := fMarkupManager.MarkupByClass[Index];
end;

function TCustomSynEdit.GetHighlightAllColor : TSynSelectedColor;
begin
  result := fMarkupHighAll.MarkupInfo;
end;

function TCustomSynEdit.GetIncrementColor : TSynSelectedColor;
begin
  result := fMarkupSelection.MarkupInfoIncr;
end;

function TCustomSynEdit.GetLineHighlightColor: TSynSelectedColor;
begin
  Result := fMarkupSpecialLine.MarkupLineHighlightInfo;
end;

function TCustomSynEdit.GetOnGutterClick : TGutterClickEvent;
begin
  Result := FLeftGutter.OnGutterClick;
end;

function TCustomSynEdit.GetSelectedColor : TSynSelectedColor;
begin
  result := fMarkupSelection.MarkupInfoSeletion;
end;

procedure TCustomSynEdit.SetSelectedColor(const AValue : TSynSelectedColor);
begin
  fMarkupSelection.MarkupInfoSeletion.Assign(AValue);
end;

procedure TCustomSynEdit.SetSpecialLineColors(const AValue : TSpecialLineColorsEvent);
begin
  fOnSpecialLineColors:=AValue;
  fMarkupSpecialLine.OnSpecialLineColors := AValue;
end;

procedure TCustomSynEdit.SetSpecialLineMarkup(const AValue : TSpecialLineMarkupEvent);
begin
  FOnSpecialLineMarkup:=AValue;
  fMarkupSpecialLine.OnSpecialLineMarkup := AValue;
end;

function TCustomSynEdit.GetBracketMatchColor : TSynSelectedColor;
begin
  Result := fMarkupBracket.MarkupInfo;
end;

function TCustomSynEdit.GetMouseLinkColor : TSynSelectedColor;
begin
  Result := fMarkupCtrlMouse.MarkupInfo;
end;

function TCustomSynEdit.GetTrimSpaceType: TSynEditStringTrimmingType;
begin
  Result := FTrimmedLinesView.TrimType;
end;

function TCustomSynEdit.GetViewedTextBuffer: TSynEditStrings;
begin
  Result := FTheLinesView;
end;

function TCustomSynEdit.GetFoldedTextBuffer: TObject;
begin
  Result := FFoldedLinesView;
end;

procedure TCustomSynEdit.SetBracketHighlightStyle(
  const AValue: TSynEditBracketHighlightStyle);
begin
  fMarkupBracket.HighlightStyle := AValue;
end;

procedure TCustomSynEdit.SetOnGutterClick(const AValue : TGutterClickEvent);
begin
  FLeftGutter.OnGutterClick := AValue; // Todo: the IDE uses this for the left gutter only
end;

procedure TCustomSynEdit.SetUseIncrementalColor(const AValue : Boolean);
begin
  fMarkupSelection.UseIncrementalColor:=AValue;
end;

function TCustomSynEdit.GetCharLen(const Line: string; CharStartPos: integer
  ): integer;
begin
  if UseUTF8 and (length(Line)>=CharStartPos) then
    Result:=UTF8CharacterLength(@Line[CharStartPos])
  else
    Result:=1;
end;

function TCustomSynEdit.GetLogicalCaretXY: TPoint;
begin
  Result := FCaret.LineBytePos;
end;

procedure TCustomSynEdit.SetLogicalCaretXY(const NewLogCaretXY: TPoint);
begin
  FCaret.ChangeOnTouch;
  FCaret.LineBytePos := NewLogCaretXY;
end;

procedure TCustomSynEdit.SetBeautifier(NewBeautifier: TSynCustomBeautifier);
begin
  if fBeautifier = NewBeautifier then exit;
  if NewBeautifier = nil then
    fBeautifier := FDefaultBeautifier
  else
    fBeautifier := NewBeautifier;
end;

function TCustomSynEdit.GetSelAvail: Boolean;
begin
  Result := FBlockSelection.SelAvail;
end;

function TCustomSynEdit.GetSelText: string;
begin
  Result := FBlockSelection.SelText;
end;

procedure TCustomSynEdit.SetTrimSpaceType(const AValue: TSynEditStringTrimmingType);
begin
  FTrimmedLinesView.TrimType := AValue;
end;

function TCustomSynEdit.SynGetText: string;
begin
  Result := fLines.Text;
end;

function TCustomSynEdit.RealGetText: TCaption;
begin
  if FLines<>nil then
    Result := FLines.Text
  else
    Result := '';
end;

{$IFDEF SYN_MBCSSUPPORT}
procedure TCustomSynEdit.WMImeComposition(var Msg: TMessage);
var
  imc: HIMC;
  p: PChar;
begin
  if ((Msg.LParam and GCS_RESULTSTR) <> 0) then begin
    imc := ImmGetContext(Handle);
    try
      fImeCount := ImmGetCompositionString(imc, GCS_RESULTSTR, nil, 0);
      GetMem(p, fImeCount + 1);
      try
        ImmGetCompositionString(imc, GCS_RESULTSTR, p, fImeCount + 1);
        p[fImeCount] := #0;
        CommandProcessor(ecImeStr, #0, p);
      finally
        FreeMem(p, fImeCount + 1);
      end;
    finally
      ImmReleaseContext(Handle, imc);
    end;
  end;
  inherited;
end;

procedure TCustomSynEdit.WMImeNotify(var Msg: TMessage);
var
  imc: HIMC;
  logFont: TLogFont;
begin
  with Msg do begin
    case WParam of
      IMN_SETOPENSTATUS:
        begin
          imc := ImmGetContext(Handle);
          if (imc <> 0) then begin
            GetObject(Font.Handle, SizeOf(TLogFont), @logFont);
            ImmSetCompositionFont(imc, @logFont);

            ImmReleaseContext(Handle, imc);
          end;
        end;
    end;
  end;
  inherited;
end;
{$ENDIF}

procedure TCustomSynEdit.InvalidateGutter;
begin
  InvalidateGutterLines(-1, -1);
end;

procedure TCustomSynEdit.InvalidateGutterLines(FirstLine, LastLine: integer);   // Todo: move to gutter
var
  rcInval: TRect;
  TopFoldLine: LongInt;
begin
  if sfPainting in fStateFlags then exit;
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then begin
      if FLeftGutter.Visible then begin;
        rcInval := Rect(0, 0, FLeftGutter.Width, ClientHeight - ScrollBarWidth);
        {$IFDEF VerboseSynEditInvalidate}
        DebugLn(['TCustomSynEdit.InvalidateGutterLines ',DbgSName(self),' ALL ',dbgs(rcInval)]);
        {$ENDIF}
        InvalidateRect(Handle, @rcInval, FALSE);
      end;
      // right gutter
      if FRightGutter.Visible then begin
        rcInval := Rect(ClientWidth - FRightGutter.Width - ScrollBarWidth, 0,
                        ClientWidth - ScrollBarWidth, ClientHeight - ScrollBarWidth);
        {$IFDEF VerboseSynEditInvalidate}
        DebugLn(['TCustomSynEdit.InvalidateGutterLines ',DbgSName(self),' ALL ',dbgs(rcInval)]);
        {$ENDIF}
        InvalidateRect(Handle, @rcInval, FALSE);
      end;
    end else begin
      // pretend we haven't scrolled
      TopFoldLine := FFoldedLinesView.TopLine;
      if FOldTopLine <> FTopLine then
        FFoldedLinesView.TopTextIndex := FOldTopLine - 1;

      { find the visible lines first }
      if LastLine >= 0 then begin
        if (LastLine < FirstLine) then SwapInt(LastLine, FirstLine);
        LastLine := RowToScreenRow(Min(LastLine, ScreenRowToRow(LinesInWindow)))+1;
        LastLine := LastLine;
      end
      else
        LastLine := LinesInWindow + 1;
      FirstLine := RowToScreenRow(Max(FirstLine, TopLine));
      FirstLine := Max(0, FirstLine);
      { any line visible? }
      if (LastLine >= FirstLine) then begin
        if FLeftGutter.Visible then begin;
          rcInval := Rect(0, fTextHeight * FirstLine,
            FLeftGutter.Width, fTextHeight * LastLine);
          {$IFDEF VerboseSynEditInvalidate}
          DebugLn(['TCustomSynEdit.InvalidateGutterLines ',DbgSName(self),' PART ',dbgs(rcInval)]);
          {$ENDIF}
          InvalidateRect(Handle, @rcInval, FALSE);
        end;
        // right gutter
        if FRightGutter.Visible then begin
          rcInval.Left := ClientWidth - FRightGutter.Width - ScrollBarWidth;
          rcInval.Right := ClientWidth - ScrollBarWidth;
          {$IFDEF VerboseSynEditInvalidate}
          DebugLn(['TCustomSynEdit.InvalidateGutterLines ',DbgSName(self),' PART ',dbgs(rcInval)]);
          {$ENDIF}
          InvalidateRect(Handle, @rcInval, FALSE);
        end;
      end;

      FFoldedLinesView.TopLine := TopFoldLine;
    end;
end;

procedure TCustomSynEdit.InvalidateLines(FirstLine, LastLine: integer);
var
  rcInval: TRect;
  f, l: Integer;
  TopFoldLine: LongInt;
begin
  if sfPainting in fStateFlags then exit;
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then begin
      rcInval := ClientRect;
      rcInval.Left := TextLeftPixelOffset(False);
      rcInval.Right := ClientWidth - TextRightPixelOffset - ScrollBarWidth;
      {$IFDEF VerboseSynEditInvalidate}
      DebugLn(['TCustomSynEdit.InvalidateLines ',DbgSName(self),' ALL ',dbgs(rcInval)]);
      {$ENDIF}
      InvalidateRect(Handle, @rcInval, FALSE);
    end else begin
      // pretend we haven't scrolled
      TopFoldLine := FFoldedLinesView.TopLine;
      if FOldTopLine <> FTopLine then
        FFoldedLinesView.TopTextIndex := FOldTopLine - 1;

      { find the visible lines first }
      if LastLine >= 0 then begin
        if (LastLine < FirstLine) then SwapInt(LastLine, FirstLine);
        l := RowToScreenRow(Min(LastLine, ScreenRowToRow(LinesInWindow)))+1;
        l := l;
      end
      else
        l := LinesInWindow + 1;
      f := RowToScreenRow(Max(FirstLine, TopLine));
      f := Max(0, f);
      { any line visible? }
      if (l >= f) then begin
        rcInval := Rect(TextLeftPixelOffset(False), fTextHeight * f,
          ClientWidth - TextRightPixelOffset - ScrollBarWidth, fTextHeight * l);
        {$IFDEF VerboseSynEditInvalidate}
        DebugLn(['TCustomSynEdit.InvalidateLines ',DbgSName(self),' PART ',dbgs(rcInval)]);
        {$ENDIF}
        InvalidateRect(Handle, @rcInval, FALSE);
      end;

      FFoldedLinesView.TopLine := TopFoldLine;
    end;
end;

procedure TCustomSynEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Data: pointer;
  C: char;
  Cmd: TSynEditorCommand;
  IsStartOfCombo, Handled: boolean;
begin
  FInMouseClickEvent := False;
  {$IFDEF VerboseKeys}
  DebugLn('[TCustomSynEdit.KeyDown] ',dbgs(Key),' ',dbgs(Shift));
  {$ENDIF}
  inherited;
  if assigned(fMarkupCtrlMouse) then
    fMarkupCtrlMouse.UpdateCtrlState(Shift);
  Data := nil;
  C := #0;
  try
    IsStartOfCombo := False;
    Handled := False;
    // If the translations requires Data, memory will be allocated for it via a
    // GetMem call.  The client must call FreeMem on Data if it is not NIL.
    if FLastKeyStrokes = FKeyStrokes then begin
      Cmd := KeyStrokes.FindKeycodeEx(Key, Shift, Data, IsStartOfCombo, True);
      Handled := Cmd <> ecNone;
    end;
    // Hooked
    if not Handled then
      FHookedKeyTranslationList.CallHookedKeyTranslationHandlers(self,
        Key, Shift, Data, IsStartOfCombo, Handled, Cmd, FLastKeyStrokes);
    if not Handled then begin
      Cmd := KeyStrokes.FindKeycodeEx(Key, Shift, Data, IsStartOfCombo);
      if IsStartOfCombo then
        FLastKeyStrokes := FKeyStrokes;
    end;

    if Cmd <> ecNone then begin
      Include(FStateFlags, sfHideCursor);
      LastMouseCaret := Point(-1,-1);                                           // includes update cursor
      //DebugLn(['[TCustomSynEdit.KeyDown] key translated ',cmd]);
      Key := 0; // eat it.
      Include(fStateFlags, sfIgnoreNextChar);
      CommandProcessor(Cmd, C, Data);
    end else if IsStartOfCombo then begin
      // this key could be the start of a two-key-combo shortcut
      Key := 0; // eat it.
      Include(fStateFlags, sfIgnoreNextChar);
    end else
      Exclude(fStateFlags, sfIgnoreNextChar);
  finally
    if Data <> nil then
      FreeMem(Data);
  end;
  UpdateCursor;
  //DebugLn('[TCustomSynEdit.KeyDown] END ',dbgs(Key),' ',dbgs(Shift));
end;

procedure TCustomSynEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  {$IFDEF VerboseKeys}
  DebugLn(['[TCustomSynEdit.KeyUp] ',Key
    ,' Shift=',ssShift in Shift,' Ctrl=',ssCtrl in Shift,' Alt=',ssAlt in Shift]);
  {$ENDIF}
  inherited KeyUp(Key, Shift);

  if sfIgnoreNextChar in fStateFlags then
    Exclude(FStateFlags, sfIgnoreNextChar);

  if assigned(fMarkupCtrlMouse) then
    fMarkupCtrlMouse.UpdateCtrlState(Shift);
  UpdateCursor;
end;


procedure TCustomSynEdit.Loaded;
begin
  inherited Loaded;
end;

procedure TCustomSynEdit.UTF8KeyPress(var Key: TUTF8Char);
begin
  if Key='' then exit;
  // don't fire the event if key is to be ignored
  if not (sfIgnoreNextChar in fStateFlags) then begin
    Include(FStateFlags, sfHideCursor);
    if Assigned(OnUTF8KeyPress) then OnUTF8KeyPress(Self, Key);
    // The key will be handled in UTFKeyPress always and KeyPress won't be called
    // so we we fire the OnKeyPress here
    if (ord(key[1])< %11000000) and (key[1]<>#0) and Assigned(OnKeyPress) then
      OnKeyPress(Self, Key[1]);
    {$IFDEF VerboseKeys}
    DebugLn('TCustomSynEdit.UTF8KeyPress ',DbgSName(Self),' Key="',DbgStr(Key),'" UseUTF8=',dbgs(UseUTF8));
    {$ENDIF}
    CommandProcessor(ecChar, Key, nil);
    // Check if ecChar has handled the Key; Todo: move the condition, in one common place
    if not ReadOnly and ((Key = #13) or (Key >= #32)) and (Key <> #127) then
      Key:='';
  end else begin
    // don't ignore further keys
    Exclude(fStateFlags, sfIgnoreNextChar);
    // Key was handled anyway, so eat it!
    Key:='';
  end;
end;

procedure TCustomSynEdit.KeyPress(var Key: Char);
begin
  if Key=#0 then exit;
  // don't fire the event if key is to be ignored
  if not (sfIgnoreNextChar in fStateFlags) then begin
    Include(FStateFlags, sfHideCursor);
    {$IFDEF VerboseKeys}
    DebugLn('TCustomSynEdit.KeyPress ',DbgSName(Self),' Key="',DbgStr(Key),'" UseUTF8=',dbgs(UseUTF8));
    {$ENDIF}
    if Assigned(OnKeyPress) then OnKeyPress(Self, Key);
    CommandProcessor(ecChar, Key, nil);
    // Check if ecChar has handled the Key; Todo: move the condition, in one common place
    if not ReadOnly and ((Key = #13) or (Key >= #32)) and (Key <> #127) then
      Key:=#0;
  end else begin
    // don't ignore further keys
    Exclude(fStateFlags, sfIgnoreNextChar);
    // Key was handled anyway, so eat it!
    Key:=#0;
  end;
end;

function TCustomSynEdit.DoHandleMouseAction(AnActionList: TSynEditMouseActions;
  AnInfo: TSynEditMouseActionInfo): Boolean;
var
  CaretDone: Boolean;

  procedure MoveCaret;
  begin
   FCaret.LineCharPos := AnInfo.NewCaret.LineCharPos;
   CaretDone := True;
  end;

var
  ACommand: TSynEditorMouseCommand;
  Handled: Boolean;
  AnAction: TSynEditMouseAction;
  ClipHelper: TSynClipboardStream;
  i: integer;
const
  WHEEL_PAGESCROLL = MAXDWORD;
begin
  AnAction := nil;
  Result := False;
  while not Result do begin
    AnAction := AnActionList.FindCommand(AnInfo, AnAction);

    if AnAction = nil then exit(False);
    ACommand := AnAction.Command;
    AnInfo.CaretDone := False;

    // Opening the context menu must not unset the block selection
    // Therefore if a non persistent block is given, it shall ignore the caret move.
    if (ACommand = emcContextMenu) and FBlockSelection.SelAvail and
       not FBlockSelection.Persistent then
    begin
      case AnAction.Option of
        emcoSelectionCaretMoveOutside:
          AnInfo.CaretDone :=
            (CompareCarets(AnInfo.NewCaret.LineBytePos, FBlockSelection.FirstLineBytePos) <= 0) and
            (CompareCarets(AnInfo.NewCaret.LineBytePos, FBlockSelection.LastLineBytePos) >= 0);
        emcoSelectionCaretMoveAlways:
          AnInfo.CaretDone := False;
        else
          AnInfo.CaretDone := True;
      end;
    end;

    // Plugins/External
    Result := FMouseActionExecHandlerList.CallExecHandlers(AnAction, AnInfo);
    // Gutter
    if not Result then
      Result := FLeftGutter.DoHandleMouseAction(AnAction, AnInfo);
    if not Result then
      Result := FRightGutter.DoHandleMouseAction(AnAction, AnInfo);

    if Result then begin
      if (not AnInfo.CaretDone) and AnAction.MoveCaret then
        MoveCaret;
      exit;
    end;

    Result := True;
    CaretDone := AnInfo.CaretDone;
    MouseCapture := False;

    case ACommand of
      emcNone: ; // do nothing, but result := true
      emcStartSelections, emcStartColumnSelections, emcStartLineSelections:
        begin
          FBlockSelection.AutoExtend := AnAction.Option = emcoSelectionContinue;
          FCaret.ChangeOnTouch;
          MoveCaret;
          case ACommand of
            emcStartColumnSelections:
              FMouseSelectionMode := smColumn;
            emcStartLineSelections:
              begin
                if ACommand = emcStartLineSelections then
                  SetLineBlock(AnInfo.NewCaret.LineBytePos, True);
                FMouseSelectionMode := smLine;
              end;
            else
              FMouseSelectionMode := FBlockSelection.SelectionMode;
          end;
          if (AnAction.Option = emcoSelectionContinue) then begin
            // only set ActiveSelectionMode if we continue an existing selection
            // Otherwise we are just setting the caret, selection will start on mouse move
            FBlockSelection.ActiveSelectionMode := FMouseSelectionMode;
            Include(fStateFlags, sfMouseDoneSelecting);
          end;
          MouseCapture := True;
          Include(fStateFlags, sfWaitForMouseSelecting);
        end;
      emcSelectWord:
        begin
          if AnAction.MoveCaret then
            MoveCaret;
          SetWordBlock(AnInfo.NewCaret.LineBytePos);
          MouseCapture := FALSE;
        end;
      emcSelectLine:
        begin
          if AnAction.MoveCaret then
            MoveCaret;
          SetLineBlock(AnInfo.NewCaret.LineBytePos, AnAction.Option = emcoSelectLineFull);
          MouseCapture := FALSE;
        end;
      emcSelectPara:
        begin
          if AnAction.MoveCaret then
            MoveCaret;
          SetParagraphBlock(AnInfo.NewCaret.LineBytePos);
          MouseCapture := FALSE;
        end;
      emcStartDragMove:
        begin
          if SelAvail and (SelectionMode = smNormal) then begin
            Include(fStateFlags, sfWaitForDragging);
            MouseCapture := True;
          end
          else
            Result := False; // Currently only drags smNormal
        end;
      emcPasteSelection:
        begin
          ClipHelper := TSynClipboardStream.Create;
          try
            ClipHelper.ReadFromClipboard(PrimarySelection);
            if ClipHelper.TextP <> nil then begin
              MoveCaret;
              if (not FBlockSelection.Persistent) then
                FBlockSelection.Clear;
              Result := PasteFromClipboardEx(ClipHelper);
            end
            else
              Result := False;
          finally
            ClipHelper.Free;
          end;
        end;
      emcMouseLink:
        begin
          if assigned(fMarkupCtrlMouse) and fMarkupCtrlMouse.IsMouseOverLink and
             assigned(FOnClickLink)
          then
            FOnClickLink(Self, SynMouseButtonBackMap[AnInfo.Button], AnInfo.Shift, AnInfo.MouseX, AnInfo.MouseY)
          else
            Result := False;
        end;
      emcContextMenu:
        begin
          Handled := False;
          if AnAction.MoveCaret and (not CaretDone) then begin
            MoveCaret;
          end;
          inherited DoContextPopup(Point(AnInfo.MouseX, AnInfo.MouseY), Handled);
          // Open PopUpMenu after DecPaintlock
          if not Handled then
            FMouseClickDoPopUp := True;
        end;
      emcSynEditCommand:
        begin
          if AnAction.MoveCaret then
            MoveCaret;
          CommandProcessor(AnAction.Option, #0, nil);
        end;
      emcWheelScrollDown:
        begin
          i := Mouse.WheelScrollLines;
          if (i = WHEEL_PAGESCROLL) or (i > fLinesInWindow) then
            i := fLinesInWindow;
          TopView := TopView - i;
        end;
      emcWheelScrollUp:
        begin
          i := Mouse.WheelScrollLines;
          if (i = WHEEL_PAGESCROLL) or (i > fLinesInWindow) then
            i := fLinesInWindow;
          TopView := TopView + i;
        end;
      else
        Result := False; // ACommand was not handled => Fallback to parent Context
    end;

    if Result and (not CaretDone) and AnAction.MoveCaret then
      MoveCaret;
  end;
end;

procedure TCustomSynEdit.FindAndHandleMouseAction(AButton: TSynMouseButton;
  AShift: TShiftState; X, Y: Integer; ACCount:TSynMAClickCount;
  ADir: TSynMAClickDir);
var
  Info: TSynEditMouseActionInfo;
begin
  FInternalCaret.AssignFrom(FCaret);
  FInternalCaret.LineCharPos := PixelsToRowColumn(Point(X,Y));
  with Info do begin
    NewCaret := FInternalCaret;
    Button := AButton;
    Shift := AShift;
    MouseX := X;
    MouseY := Y;
    CCount := ACCount;
    Dir := ADir;
    IgnoreUpClick := False;
  end;
  try
    // Check plugins/external handlers
    if FMouseActionSearchHandlerList.CallSearchHandlers(Info,
                                         {$IFDEF FPC}@{$ENDIF}DoHandleMouseAction)
    then
      exit;

    if FLeftGutter.Visible and (X < FLeftGutter.Width) then begin
      // mouse event occured in Gutter ?
      if FLeftGutter.MaybeHandleMouseAction(Info, {$IFDEF FPC}@{$ENDIF}DoHandleMouseAction) then
        exit;
    end
    else
    if FRightGutter.Visible and (X > ClientWidth - FRightGutter.Width) then begin
      // mouse event occured in Gutter ?
      if FRightGutter.MaybeHandleMouseAction(Info, {$IFDEF FPC}@{$ENDIF}DoHandleMouseAction) then
        exit;
    end
    else
    begin
      // mouse event occured in selected block ?
      if SelAvail and (X >= TextLeftPixelOffset) and
         //(x < ClientWidth - TextRightPixelOffset - ScrollBarWidth) and
         IsPointInSelection(FInternalCaret.LineBytePos)
      then
        if DoHandleMouseAction(FMouseSelActions.GetActionsForOptions(FMouseOptions), Info) then
          exit;
      // mouse event occured in text?
      if DoHandleMouseAction(FMouseTextActions.GetActionsForOptions(FMouseOptions), Info) then
        exit;
    end;

    DoHandleMouseAction(FMouseActions.GetActionsForOptions(FMouseOptions), Info);
  finally
    if Info.IgnoreUpClick then
      include(fStateFlags, sfIgnoreUpClick);
  end;
end;

procedure TCustomSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  CType: TSynMAClickCount;
begin
  //DebugLn(['TCustomSynEdit.MouseDown START Mouse=',X,',',Y,' Caret=',CaretX,',',CaretY,', BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y]);
  Exclude(FStateFlags, sfHideCursor);
  FInMouseClickEvent := True;
  if (X>=ClientWidth-ScrollBarWidth) or (Y>=ClientHeight-ScrollBarWidth) then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    exit;
  end;

  LastMouseCaret:=PixelsToRowColumn(Point(X,Y));
  fMouseDownX := X;
  fMouseDownY := Y;

  fStateFlags := fStateFlags - [sfDblClicked, sfTripleClicked, sfQuadClicked,
                                sfLeftGutterClick, sfRightGutterClick,
                                sfWaitForMouseSelecting, sfMouseSelecting, sfMouseDoneSelecting,
                                sfWaitForDragging, sfIgnoreUpClick
                               ];

  if ssQuad in Shift then begin
    CType := ccQuad;
    Include(fStateFlags, sfQuadClicked);
  end
  else if ssTriple in Shift then begin
    CType := ccTriple;
    Include(fStateFlags, sfTripleClicked);
  end
  else if ssDouble in Shift then begin
    CType := ccDouble;
    Include(fStateFlags, sfDblClicked);
  end
  else
    CType := ccSingle;

  FMouseClickDoPopUp := False;
  IncPaintLock;
  try
    if (X < TextLeftPixelOffset(False)) then begin
      Include(fStateFlags, sfLeftGutterClick);
      FLeftGutter.MouseDown(Button, Shift, X, Y);
    end;
    if (X > ClientWidth - TextRightPixelOffset - ScrollBarWidth) then begin
      Include(fStateFlags, sfRightGutterClick);
      FRightGutter.MouseDown(Button, Shift, X, Y);
    end;
    FindAndHandleMouseAction(SynMouseButtonMap[Button], Shift, X, Y, CType, cdDown);
  finally
    DecPaintLock;
  end;
  if FMouseClickDoPopUp and (PopupMenu <> nil) then begin
    PopupMenu.PopupComponent:=self;
    PopupMenu.PopUp;
  end;

  inherited MouseDown(Button, Shift, X, Y);
  LCLIntf.SetFocus(Handle);
  UpdateCaret;
  //debugln('TCustomSynEdit.MouseDown END sfWaitForDragging=',dbgs(sfWaitForDragging in fStateFlags),' ');
end;

procedure TCustomSynEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Z: integer;
begin
  Exclude(FStateFlags, sfHideCursor);
  inherited MouseMove(Shift, x, y);
  if (sfLeftGutterClick in fStateFlags) then
    FLeftGutter.MouseMove(Shift, X, Y);
  if (sfRightGutterClick in fStateFlags) then
    FRightGutter.MouseMove(Shift, X, Y);

  FLastMousePoint := Point(X,Y);
  LastMouseCaret := PixelsToRowColumn(Point(X,Y));
  UpdateCursor;

  if (sfWaitForMouseSelecting in fStateFlags) and MouseCapture and
     ( (abs(fMouseDownX-X) >= MinMax(fCharWidth div 2, 2, 4)) or
       (abs(fMouseDownY-Y) >= MinMax(fTextHeight div 2, 2, 4)) )
  then
    FStateFlags := FStateFlags - [sfWaitForMouseSelecting] + [sfMouseSelecting];

  //debugln('TCustomSynEdit.MouseMove sfWaitForDragging=',dbgs(sfWaitForDragging in fStateFlags),' MouseCapture=',dbgs(MouseCapture),' GetCaptureControl=',DbgSName(GetCaptureControl));
  if MouseCapture and (sfWaitForDragging in fStateFlags) then begin
    if (Abs(fMouseDownX - X) >= GetSystemMetrics(SM_CXDRAG))
      or (Abs(fMouseDownY - Y) >= GetSystemMetrics(SM_CYDRAG))
    then begin
      FStateFlags := FStateFlags
                   -[sfWaitForDragging, sfWaitForMouseSelecting, sfMouseSelecting]
                   + [sfIsDragging];
      //debugln('TCustomSynEdit.MouseMove BeginDrag');
      BeginDrag(true);
    end;
  end
  else
  if (fStateFlags * [sfMouseSelecting, sfIsDragging] <> []) and MouseCapture
  then begin
    //DebugLn(' TCustomSynEdit.MouseMove CAPTURE Mouse=',dbgs(X),',',dbgs(Y),' Caret=',dbgs(CaretXY),', BlockBegin=',dbgs(BlockBegin),' BlockEnd=',dbgs(BlockEnd));
    if sfIsDragging in fStateFlags then
      FBlockSelection.IncPersistentLock;
    FInternalCaret.AssignFrom(FCaret);
    FInternalCaret.LineCharPos := PixelsToRowColumn(Point(X,Y));

    if ((X >= TextLeftPixelOffset(False)) or (fLeftChar <= 1)) and
       ( (X < ClientWidth - TextRightPixelOffset)
         or (LeftChar >= CurrentMaxLeftChar)) and
       ((Y >= 0) or (fTopLine <= 1)) and
       ((Y < ClientHeight-ScrollBarWidth) or (fTopLine >= CurrentMaxTopLine))
    then begin
      if (sfMouseSelecting in fStateFlags) and not FInternalCaret.IsAtPos(FCaret) then
        Include(fStateFlags, sfMouseDoneSelecting);
      FBlockSelection.AutoExtend := sfMouseSelecting in fStateFlags;
      FCaret.LineBytePos := FInternalCaret.LineBytePos;
      FBlockSelection.AutoExtend := False;
    end
    else begin
      // begin scrolling?
      Dec(X, TextLeftPixelOffset(False));
      // calculate chars past right
      Z := X - (fCharsInWindow * fCharWidth);
      if Z > 0 then
        Inc(Z, fCharWidth);
      fScrollDeltaX := Max(Z div fCharWidth, 0);
      if fScrollDeltaX = 0 then begin
        // calculate chars past left
        Z := X;
        if Z < 0 then
          Dec(Z, fCharWidth);
        fScrollDeltaX := Min(Z div fCharWidth, 0);
      end;
      // calculate lines past bottom
      Z := Y - (fLinesInWindow * fTextHeight);
      if Z > 0 then
        Inc(Z, fTextHeight);
      fScrollDeltaY := Max(Z div fTextHeight, 0);
      if fScrollDeltaY = 0 then begin
        // calculate lines past top
        Z := Y;
        if Z < 0 then
          Dec(Z, fTextHeight);
        fScrollDeltaY := Min(Z div fTextHeight, 0);
      end;
      fScrollTimer.Enabled := (fScrollDeltaX <> 0) or (fScrollDeltaY <> 0);
      if (sfMouseSelecting in fStateFlags) and ((fScrollDeltaX <> 0) or (fScrollDeltaY <> 0)) then
        Include(fStateFlags, sfMouseDoneSelecting);
    end;
    if sfMouseDoneSelecting in fStateFlags then
      FBlockSelection.ActiveSelectionMode := FMouseSelectionMode;
    if sfIsDragging in fStateFlags then
      FBlockSelection.DecPersistentLock;
  end
  else
  if MouseCapture and (fStateFlags * [sfIsDragging, sfWaitForMouseSelecting] = [])
  then begin
    MouseCapture:=false;
    fScrollTimer.Enabled := False;
  end;
end;

procedure TCustomSynEdit.ScrollTimerHandler(Sender: TObject);
var
  C: TPoint;
  CurMousePos: TPoint;
  Z: integer;
  X, Y: Integer;
begin
  // changes to line / column in one go
  if sfIsDragging in fStateFlags then
    FBlockSelection.IncPersistentLock;
  DoIncPaintLock(Self); // No editing is taking place
  try
    GetCursorPos(CurMousePos);
    CurMousePos:=ScreenToClient(CurMousePos);
    C := PixelsToLogicalPos(CurMousePos);
    // recalculate scroll deltas
    Dec(CurMousePos.X, TextLeftPixelOffset(False));
    // calculate chars past right
    Z := CurMousePos.X - (fCharsInWindow * fCharWidth);
    if Z > 0 then
      Inc(Z, fCharWidth);
    fScrollDeltaX := Max(Z div fCharWidth, 0);
    if fScrollDeltaX = 0 then begin
      // calculate chars past left
      Z := CurMousePos.X;
      if Z < 0 then
        Dec(Z, fCharWidth);
      fScrollDeltaX := Min(Z div fCharWidth, 0);
    end;
    // calculate lines past bottom
    Z := CurMousePos.Y - (fLinesInWindow * fTextHeight);
    if Z > 0 then
      Inc(Z, fTextHeight);
    fScrollDeltaY := Max(Z div fTextHeight, 0);
    if fScrollDeltaY = 0 then begin
      // calculate lines past top
      Z := CurMousePos.Y;
      if Z < 0 then
        Dec(Z, fTextHeight);
      fScrollDeltaY := Min(Z div fTextHeight, 0);
    end;
    fScrollTimer.Enabled := (fScrollDeltaX <> 0) or (fScrollDeltaY <> 0);
    // now scroll
    if fScrollDeltaX <> 0 then begin
      LeftChar := LeftChar + fScrollDeltaX;
      X := LeftChar;
      if fScrollDeltaX > 0 then  // scrolling right?
        Inc(X, CharsInWindow);
      FCaret.LineCharPos := Point(X, C.Y);
      if (not(sfIsDragging in fStateFlags)) then
        SetBlockEnd(LogicalCaretXY);
    end;
    if fScrollDeltaY <> 0 then begin
      if GetKeyState(VK_SHIFT) < 0 then
        TopView := TopView + fScrollDeltaY * LinesInWindow
      else
        TopView := TopView + fScrollDeltaY;
      if fScrollDeltaY > 0
      then Y := FFoldedLinesView.TextIndex[LinesInWindow-1]+1  // scrolling down
      else Y := TopLine;  // scrolling up
      if Y < 1   // past end of file
      then y := FCaret.LinePos;
      FCaret.LineCharPos := Point(C.X, Y);
      if (not(sfIsDragging in fStateFlags)) then
        SetBlockEnd(LogicalCaretXY);
    end;
  finally
    DoDecPaintLock(Self);
    if sfIsDragging in fStateFlags then
      FBlockSelection.DecPersistentLock;
  end;
end;

procedure TCustomSynEdit.DoContextPopup(MousePos: TPoint; var Handled: Boolean);
begin
  Handled := FInMouseClickEvent;
  if not Handled then
    Exclude(FStateFlags, sfHideCursor);
end;

procedure TCustomSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  wasDragging, wasSelecting, ignoreUp : Boolean;
  CType: TSynMAClickCount;
begin
  Exclude(FStateFlags, sfHideCursor);
//DebugLn('TCustomSynEdit.MouseUp Mouse=',X,',',Y,' Caret=',CaretX,',',CaretY,', BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
  FInMouseClickEvent := True;
  wasDragging := (sfIsDragging in fStateFlags);
  wasSelecting := (sfMouseDoneSelecting in fStateFlags);
  ignoreUp := (sfIgnoreUpClick in fStateFlags);
  Exclude(fStateFlags, sfIsDragging);
  Exclude(fStateFlags, sfWaitForMouseSelecting);
  Exclude(fStateFlags, sfMouseSelecting);
  Exclude(fStateFlags, sfMouseDoneSelecting);
  Exclude(fStateFlags, sfIgnoreUpClick);
  fScrollTimer.Enabled := False;
  inherited MouseUp(Button, Shift, X, Y);
  MouseCapture := False;

  if sfQuadClicked in fStateFlags then begin
    CType := ccQuad;
    Include(fStateFlags, sfQuadClicked);
  end
  else if sfTripleClicked in fStateFlags then begin
    CType := ccTriple;
    Include(fStateFlags, sfTripleClicked);
  end
  else if sfDblClicked in fStateFlags then begin
    CType := ccDouble;
    Include(fStateFlags, sfDblClicked);
  end
  else
    CType := ccSingle;
  fStateFlags:=fStateFlags - [sfDblClicked,sfTripleClicked,sfQuadClicked];

  if sfWaitForDragging in fStateFlags then
  begin
    ComputeCaret(X, Y);
    SetBlockBegin(LogicalCaretXY);
    SetBlockEnd(LogicalCaretXY);
    Exclude(fStateFlags, sfWaitForDragging);
  end;

  if SelAvail then
    AquirePrimarySelection;
  if (X>=ClientWidth-ScrollBarWidth) or (Y>=ClientHeight-ScrollBarWidth) then
    exit;
  LastMouseCaret:=PixelsToRowColumn(Point(X,Y));

  if wasDragging or wasSelecting or ignoreUp then exit;

  FMouseClickDoPopUp := False;
  IncPaintLock;
  try
    if (sfLeftGutterClick in fStateFlags) then begin
      FLeftGutter.MouseUp(Button, Shift, X, Y);
      Exclude(fStateFlags, sfLeftGutterClick);
    end;
    if (sfRightGutterClick in fStateFlags) then begin
      FRightGutter.MouseUp(Button, Shift, X, Y);
      Exclude(fStateFlags, sfRightGutterClick);
    end;
    FindAndHandleMouseAction(SynMouseButtonMap[Button], Shift, X, Y, CType, cdUp);
  finally
    DecPaintLock;
  end;
  if FMouseClickDoPopUp and (PopupMenu <> nil) then begin
    PopupMenu.PopupComponent:=self;
    PopupMenu.PopUp;
  end;
  //DebugLn('TCustomSynEdit.MouseUp END Mouse=',X,',',Y,' Caret=',CaretX,',',CaretY,', BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
end;

procedure TCustomSynEdit.Paint;
var
  rcClip, rcDraw: TRect;
  nL1, nL2, nC1, nC2: integer;
begin
  // Get the invalidated rect. Compute the invalid area in lines / columns.
  rcClip := Canvas.ClipRect;

  If FPaintLock > 0 then begin
    debugln(['Warning: SynEdit.Paint called during PaintLock']);
    // Ensure this will be repainted after PaintLock
    if FInvalidateRect.Top < 0 then
      FInvalidateRect := rcClip
    else
      types.UnionRect(FInvalidateRect, FInvalidateRect, rcClip);
    // Just paint the background
    SetBkColor(Canvas.Handle, ColorToRGB(Color));
    InternalFillRect(Canvas.Handle, rcClip);
    if rcClip.Left <= TextLeftPixelOffset(False) then begin
      rcClip.Right := TextLeftPixelOffset(False)+1;
      SetBkColor(Canvas.Handle, ColorToRGB(FLeftGutter.Color));
      InternalFillRect(Canvas.Handle, rcClip);
    end;
    exit;
  end;

  {$IFDEF EnableDoubleBuf}
  //rcClip:=Rect(0,0,ClientWidth,ClientHeight);
  StartPaintBuffer(rcClip);
  {$ENDIF}
  {$IFDEF SYNSCROLLDEBUG}
  debugln(['PAINT ',DbgSName(self),' sfHasScrolled=',dbgs(sfHasScrolled in fStateFlags),' rect=',dbgs(rcClip)]);
  {$ENDIF}

  Include(fStateFlags,sfPainting);
  Exclude(fStateFlags, sfHasScrolled);
  // columns
  nC1 := LeftChar;
  if (rcClip.Left > TextLeftPixelOffset) then
    Inc(nC1, (rcClip.Left - TextLeftPixelOffset) div CharWidth);
  nC2 := LeftChar +
    ( Min(rcClip.Right, ClientWidth - TextRightPixelOffset - ScrollBarWidth)
     - TextLeftPixelOffset + CharWidth - 1) div CharWidth;
  // lines
  nL1 := Max(rcClip.Top div fTextHeight, 0);
  nL2 := Min((rcClip.Bottom-1) div fTextHeight,
             FFoldedLinesView.Count - FFoldedLinesView.TopLine);
  {$IFDEF SYNSCROLLDEBUG}
  debugln(['PAINT ',DbgSName(self),' rect=',dbgs(rcClip), ' L1=',nL1, '  Nl2=',nL2]);
  {$ENDIF}
  //DebugLn('TCustomSynEdit.Paint LinesInWindow=',dbgs(LinesInWindow),' nL1=',dbgs(nL1),' nL2=',dbgs(nL2));
  // Now paint everything while the caret is hidden.
  FScreenCaret.Hide;
  try
    // First paint the gutter area if it was (partly) invalidated.
    if FLeftGutter.Visible and (rcClip.Left < FLeftGutter.Width) then begin
      rcDraw := rcClip;
      rcDraw.Right := FLeftGutter.Width;
      FLeftGutter.Paint(Canvas, rcDraw, nL1, nL2);
    end;
    // Then paint the text area if it was (partly) invalidated.
    if (rcClip.Right > TextLeftPixelOffset(False)) then begin
      rcDraw := rcClip;
      rcDraw.Left  := Max(rcDraw.Left, TextLeftPixelOffset(False)); // Todo: This is also checked in paintLines (together with right side)
      rcDraw.Right := Min(rcDraw.Right, ClientWidth - TextRightPixelOffset - ScrollBarWidth);
      PaintTextLines(rcDraw, nL1, nL2, nC1, nC2);
    end;
    // right gutter
    if FRightGutter.Visible and (rcClip.Right > ClientWidth - FRightGutter.Width - ScrollBarWidth) then begin
      rcDraw := rcClip;
      rcDraw.Left := ClientWidth - FRightGutter.Width - ScrollBarWidth;
      FRightGutter.Paint(Canvas, rcDraw, nL1, nL2);
    end;
    // If there is a custom paint handler call it.
    DoOnPaint;
  finally
    {$IFDEF EnableDoubleBuf}
    EndPaintBuffer(rcClip);
    {$ENDIF}
    UpdateCaret;
    Exclude(fStateFlags,sfPainting);
  end;
end;

procedure TCustomSynEdit.CodeFoldAction(iLine: integer);
// iLine is 1 based as parameter
begin
  if (iLine<=0) or (iLine>FTheLinesView.Count) then exit;
  dec(iLine);
//DebugLn(['****** FoldAction at ',iLine,' scrline=',FFoldedLinesView.TextIndexToScreenLine(iLine), ' type ', SynEditCodeFoldTypeNames[FFoldedLinesView.FoldType[FFoldedLinesView.TextIndexToScreenLine(iLine)]],  '  view topline=',FFoldedLinesView.TopLine  ]);
  if FFoldedLinesView.FoldType[FFoldedLinesView.TextIndexToScreenLine(iLine)]
     * [cfCollapsedFold, cfCollapsedHide] <> []
  then
    FFoldedLinesView.UnFoldAtTextIndex(iLine)
  else
  if FFoldedLinesView.FoldType[FFoldedLinesView.TextIndexToScreenLine(iLine)]
     * [cfFoldStart] <> []
  then
    FFoldedLinesView.FoldAtTextIndex(iLine);
end;

function TCustomSynEdit.FindNextUnfoldedLine(iLine: integer; Down: boolean
  ): Integer;
// iLine is 1 based
begin
  Result:=iLine;
  if Down then
    while (Result<FTheLinesView.Count) and (FFoldedLinesView.FoldedAtTextIndex[Result-1]) do
      inc(Result)
  else
    while (Result>1) and (FFoldedLinesView.FoldedAtTextIndex[Result-1]) do
      dec(Result);
end;

function TCustomSynEdit.CreateGutter(AOwner : TSynEditBase; ASide: TSynGutterSide;
  ATextDrawer: TheTextDrawer): TSynGutter;
begin
  Result := TSynGutter.Create(AOwner, ASide, ATextDrawer);
end;

procedure TCustomSynEdit.UnfoldAll;
begin
  FFoldedLinesView.UnfoldAll;
  Invalidate;
end;

procedure TCustomSynEdit.FoldAll(StartLevel : Integer = 0; IgnoreNested : Boolean = False);
begin
  FFoldedLinesView.FoldAll(StartLevel, IgnoreNested);
  Invalidate;
end;

procedure TCustomSynEdit.PaintTextLines(AClip: TRect; FirstLine, LastLine,
  FirstCol, LastCol: integer);
// FirstLine, LastLine are based 1
// FirstCol, LastCol are screen based 1 without scrolling (physical position).
//  i.e. the real screen position is fTextOffset+Pred(FirstCol)*CharWidth
var
  bDoRightEdge: boolean; // right edge
  nRightEdge: integer;
  colEditorBG: TColor;
    // painting the background and the text
  rcLine, rcToken: TRect;
  EraseLeft, DrawLeft: Integer;  // LeftSide for EraseBackground, Text
  CurLine: integer;         // Screen-line index for the loop
  CurTextIndex: Integer;    // Current Index in text
  CurPhysPos, CurLogIndex : Integer; // Physical Start Position of next token in current Line
  ForceEto: Boolean;
  TokenAccu: record
    Len, MaxLen: integer;
    PhysicalStartPos, PhysicalEndPos: integer;
    p: PChar;
    FG, BG: TColor;
    Style: TFontStyles;
    FrameColor: array[TSynFrameSide] of TColor;
    FrameStyle: array[TSynFrameSide] of TSynLineStyle;
  end;
  dc: HDC;

  ExpandedPaintToken: string; // used to create the string sent to TextDrawer
  CharWidths: TPhysicalCharWidths;

{ local procedures }

  procedure SetTokenAccuLength;
  begin
    ReAllocMem(TokenAccu.p,TokenAccu.MaxLen+1);
    TokenAccu.p[TokenAccu.MaxLen]:=#0;
  end;

  function ExpandSpecialChars(var p: PChar; var Count: integer;
    PhysicalStartPos: integer): Integer;
  // if there are no tabs or special chars: keep p and Count untouched
  // if there are special chars: copy p into ExpandedPaintToken buffer,
  //                             convert tabs to spaces, and return the buffer
  // Return DisplayCell-Count in Buffer
  var
    i: integer;
    LengthNeeded: Integer;
    DestPos: Integer;
    SrcPos: Integer;
    Dest: PChar;
    c: Char;
    CharLen: Integer;
    Special, SpecialTab1, SpecialTab2, SpecialSpace, HasTabs: boolean;
    Fill: Integer;
  begin
      LengthNeeded := 0;
    Result := 0;
    HasTabs := False;
    SrcPos:=0;
    for i := CurLogIndex to CurLogIndex + Count -1 do begin
      Result := Result + CharWidths[i];
      if CharWidths[i] > 1 then
        LengthNeeded := LengthNeeded + CharWidths[i] - 1;
      if p[SrcPos] = #9 then HasTabs := True;
      inc(SrcPos);
    end;
    Special:=eoShowSpecialChars in Options;
    if (not Special) and (LengthNeeded=0) and (not HasTabs)
    and (FindInvalidUTF8Character(p,Count)<0) then
      exit;
    SpecialTab1 := Special and (vscTabAtFirst in FVisibleSpecialChars);
    SpecialTab2 := Special and (vscTabAtLast in FVisibleSpecialChars);
    SpecialSpace := Special and (vscSpace in FVisibleSpecialChars);
    LengthNeeded := LengthNeeded + Count;
    if Special then LengthNeeded:=LengthNeeded*2;
    if length(ExpandedPaintToken)<LengthNeeded then
      SetLength(ExpandedPaintToken,LengthNeeded+CharsInWindow);
    //DebugLn(['ExpandSpecialChars Count=',Count,' TabCount=',TabCount,' Special=',Special,' LengthNeeded=',LengthNeeded]);
    SrcPos:=0;
    DestPos:=0;
    Dest:=PChar(Pointer(ExpandedPaintToken));
    if UseUTF8 then begin
      while SrcPos<Count do begin
        c:=p[SrcPos];
        Fill := CharWidths[CurLogIndex + SrcPos] - 1;
        if c = #9 then begin
          // tab char, fill with spaces
          if SpecialTab1 then begin
            // #194#187 looks like >>
            Dest[DestPos]   := #194;
            Dest[DestPos+1] := #187;
            inc(DestPos, 2);
            dec(Fill);
          end;
          while Fill >= 0 do begin //for i := 0 to Fill do begin
            Dest[DestPos]:= ' ';
            inc(DestPos);
            dec(Fill);
          end;
          if SpecialTab2 then begin
            // #194#187 looks like >>
            Dest[DestPos-1] := #194;
            Dest[DestPos]   := #187;
            inc(DestPos, 1);
          end;
          inc(SrcPos);
        end
        else begin
          // could be UTF8 char
          if c in [#128..#255]
          then CharLen := UTF8CharacterStrictLength(@p[SrcPos])
          else CharLen := 1;
          if CharLen=0 then begin
            // invalid character
            Dest[DestPos]:='?';
            inc(DestPos);
            inc(SrcPos);
          end else begin
            // normal UTF-8 character
            for i:=1 to CharLen do begin
              Dest[DestPos]:=p[SrcPos];
              inc(DestPos);
              inc(SrcPos);
            end;
            if (c = #32) and SpecialSpace then begin
              // #194#183 looks like .
              Dest[DestPos-1] := #194;
              Dest[DestPos]   := #183;
              inc(DestPos);
            end;
            for i := 1 to Fill do begin
              Dest[DestPos]:= ' ';
              inc(DestPos);
            end;
          end;
          // ToDo: pass the eto with to fTextDrawer, instead of filling with spaces
          if Fill > 0 then ForceEto := True;
        end;
      end;
    end else begin
      // non UTF-8
      while SrcPos<Count do begin
        c:=p[SrcPos];
        Fill := CharWidths[CurLogIndex + SrcPos] - 1;
        if c = #9 then // tab char
          Dest[DestPos] := ' '
        else begin
          Dest[DestPos] := p[SrcPos];
          if Fill > 0 then ForceEto := True;
        end;
        inc(DestPos);
        inc(SrcPos);
        for i := 1 to Fill do begin
          Dest[DestPos]:= ' ';
          inc(DestPos);
        end;
      end;
    end;
    p:=PChar(Pointer(ExpandedPaintToken));
    Count:=DestPos;
    //debugln('ExpandSpecialChars Token with Tabs: "',DbgStr(copy(ExpandedPaintToken,1,Count)),'"');
  end;

  const
    ETOOptions = ETO_OPAQUE; // Note: clipping is slow and not needed

  procedure PaintToken(Token: PChar; TokenLen, FirstPhysical: integer);
  // FirstPhysical is the physical (screen without scrolling)
  // column of the first character
  var
    nX: integer;
    tok: TRect;
  begin
    {debugln('PaintToken A TokenLen=',dbgs(TokenLen),
      ' FirstPhysical=',dbgs(FirstPhysical),
      ' Tok="'+copy(Token, 1, TokenLen),'"',
      ' rcToken='+dbgs(rcToken.Left)+'-'+dbgs(rcToken.Right));}
    if (rcToken.Right <= rcToken.Left) then exit;
    // Draw the right edge under the text if necessary
    nX := ScreenColumnToXValue(FirstPhysical); // == rcToken.Left
    if ForceEto then fTextDrawer.ForceNextTokenWithEto;
    if bDoRightEdge and (not (eoHideRightMargin in Options))
    and (nRightEdge<rcToken.Right) and (nRightEdge>=rcToken.Left)
    then begin
      // draw background (use rcToken, so we do not delete the divider-draw-line)
      if rcToken.Left < nRightEdge then begin
        tok := rcToken;
        tok.Right := nRightEdge;
        InternalFillRect(dc, tok);
      end;
      if rcToken.Right > nRightEdge then begin
        tok := rcToken;
        tok.Left := nRightEdge;
        tok.Bottom := rcLine.Bottom;
        InternalFillRect(dc, tok);
      end;
      // draw edge (use rcLine / rcToken may be reduced)
      LCLIntf.MoveToEx(dc, nRightEdge, rcLine.Top, nil);
      LCLIntf.LineTo(dc, nRightEdge, rcLine.Bottom + 1);
      // draw text
      fTextDrawer.ExtTextOut(nX, rcToken.Top, ETOOptions-ETO_OPAQUE, rcToken,
                             Token, TokenLen, rcLine.Bottom);
    end else begin
      // draw text with background
      //debugln('PaintToken nX=',dbgs(nX),' Token=',dbgstr(copy(Token,1, TokenLen)),' rcToken=',dbgs(rcToken));
      tok := rcToken;
      if rcToken.Right > nRightEdge + 1 then
        tok.Bottom := rcLine.Bottom;
      fTextDrawer.ExtTextOut(nX, rcToken.Top, ETOOptions, tok,
                             Token, TokenLen, rcLine.Bottom);
    end;
    rcToken.Left := rcToken.Right;
  end;

  procedure PaintHighlightToken(bFillToEOL: boolean);
  var
    nX1, eolx: integer;
    NextPos : Integer;
    MarkupInfo, FoldedCodeInfo: TSynSelectedColor;
    FillFCol, FillBCol, FillFrame: TColor;
    FrameStyle: TSynLineStyle;
    FillStyle: TFontStyles;
    tok: TRect;
    Attr: TSynHighlighterAttributes;
    s: TSynFrameSide;
    side: TSynFrameSide;
  begin
    {debugln('PaintHighlightToken A TokenAccu: Len=',dbgs(TokenAccu.Len),
      ' PhysicalStartPos=',dbgs(TokenAccu.PhysicalStartPos),
      ' PhysicalEndPos=',dbgs(TokenAccu.PhysicalEndPos),
      ' "',copy(TokenAccu.p,1,TokenAccu.Len),'"');}

    // Any token chars accumulated?
    if (TokenAccu.Len > 0) then 
    begin
      // Initialize the colors and the font style.
      with fTextDrawer do 
      begin
        SetBackColor(TokenAccu.BG);
        SetForeColor(TokenAccu.FG);
        SetStyle(TokenAccu.Style);
        for s := low(TSynFrameSide) to high(TSynFrameSide) do begin
          FrameColor[s] := TokenAccu.FrameColor[s];
          FrameStyle[s] := TokenAccu.FrameStyle[s];
        end;
      end;
      // Paint the chars
      rcToken.Right := ScreenColumnToXValue(TokenAccu.PhysicalEndPos+1);
      if rcToken.Right > AClip.Right then begin
        rcToken.Right := AClip.Right;
        fTextDrawer.FrameColor[sfdRight] := clNone; // right side of char is not painted
      end;
      with TokenAccu do PaintToken(p, Len, PhysicalStartPos);
    end;

    // Fill the background to the end of this line if necessary.
    if bFillToEOL and (rcToken.Left < rcLine.Right) then begin
      eolx := rcToken.Left; // remeber end of actual line, so we can decide to draw the right edge
      NextPos := Min(LastCol, TokenAccu.PhysicalEndPos+1);
      if Assigned(fHighlighter) then
        Attr := fHighlighter.GetEndOfLineAttribute
      else
        Attr := nil;
      repeat
        MarkupInfo := fMarkupManager.GetMarkupAttributeAtRowCol(FFoldedLinesView.TextIndex[CurLine]+1, NextPos);
        NextPos := fMarkupManager.GetNextMarkupColAfterRowCol(FFoldedLinesView.TextIndex[CurLine]+1, NextPos);

        if Assigned(Attr) then
        begin
          FillFCol := Attr.Foreground;
          FillBCol := Attr.Background;
          FillFrame := Attr.FrameColor;
          FillStyle := Attr.Style;
          FrameStyle := Attr.FrameStyle;
          if FillFCol = clNone then FillFCol := Font.Color;
          if FillBCol = clNone then FillBCol := colEditorBG;
        end else
        begin
          FillFCol := Font.Color;
          FillBCol := colEditorBG;
          FillFrame := clNone;
          FillStyle := Font.Style;
          FrameStyle := slsSolid;
        end;

        if Assigned(MarkupInfo) then
          MarkupInfo.ModifyColors(FillFCol, FillBCol, FillFrame, FillStyle, FrameStyle);

        fTextDrawer.BackColor := FillBCol;
        //fTextDrawer.ForeColor := FillFCol; // for underline
        //fTextDrawer.Style := FillStyle;

        if NextPos < 1
        then nX1 := rcLine.Right
        else begin
          nX1 := ScreenColumnToXValue(NextPos);
          if nX1 > rcLine.Right
          then nX1 := rcLine.Right;
        end;

        if nX1 > nRightEdge then begin
          if rcToken.Left < nRightEdge then begin
            tok := rcToken;
            tok.Right := nRightEdge;
            InternalFillRect(dc, tok);
            rcToken.Left := nRightEdge;
          end;
          rcToken.Bottom := rcLine.Bottom;
        end;
        rcToken.Right := nX1;
        InternalFillRect(dc, rcToken); {TODO: if style underline, then print spaces}
        rcToken.Left := nX1;
      until nX1 >= rcLine.Right;

      // Draw the right edge if necessary.
      if bDoRightEdge and (not (eoHideRightMargin in Options))
      and (nRightEdge >= eolx) then begin // xx rc Token
        LCLIntf.MoveToEx(dc, nRightEdge, rcLine.Top, nil);
        LCLIntf.LineTo(dc, nRightEdge, rcLine.Bottom + 1);
      end;

      if FFoldedLinesView.FoldType[CurLine] * [cfCollapsedFold, cfCollapsedHide] <> [] then
      begin
        FillFCol := Font.Color;
        FillBCol := colEditorBG;
        FillFrame := Font.Color;
        FrameStyle := slsSolid;
        FillStyle  := [];
        MarkupInfo := fMarkupManager.GetMarkupAttributeAtRowCol(FFoldedLinesView.TextIndex[CurLine]+1, CurPhysPos + 3);
        if MarkupInfo <> nil then
          MarkupInfo.ModifyColors(FillFCol, FillBCol, FillFrame, FillStyle, FrameStyle);
        FoldedCodeInfo := FoldedCodeColor;
        if Assigned(FoldedCodeInfo) then
          FoldedCodeInfo.ModifyColors(FillFCol, FillBCol, FillFrame, FillStyle, FrameStyle);
        if (FillBCol = FillFCol) then begin // or if diff(gb,fg) < x
          if FillBCol = colEditorBG
          then FillFCol := not(FillBCol) and $00ffffff // or maybe Font.color ?
          else FillFCol := colEditorBG;
        end;

        rcToken.Left := ScreenColumnToXValue(CurPhysPos+3);
        rcToken.Right := ScreenColumnToXValue(CurPhysPos+6);

        FTextDrawer.ForeColor := FillFCol;
        FTextDrawer.BackColor := FillBCol;
        FTextDrawer.SetStyle(FillStyle);

        if Assigned(FoldedCodeInfo) and (FoldedCodeInfo.FrameColor <> clNone) then
        begin
          for side := low(TSynFrameSide) to high(TSynFrameSide) do begin
            if ( (FoldedCodeInfo.FrameEdges = sfeAround) or
                 ((FoldedCodeInfo.FrameEdges = sfeBottom) and (side = sfdBottom)) or
                 ((FoldedCodeInfo.FrameEdges = sfeLeft) and (side = sfdLeft))
               )
            then
              FTextDrawer.FrameColor[side] := FoldedCodeInfo.FrameColor
            else
              FTextDrawer.FrameColor[side] := clNone;
            FTextDrawer.FrameStyle[side] := FoldedCodeInfo.FrameStyle;
          end;
        end;
        if rcToken.Right > rcLine.Right then begin
          rcToken.Right := rcLine.Right;
          fTextDrawer.FrameColor[sfdRight] := clNone; // right side of char is not painted
        end;
        if rcToken.Right > rcToken.Left then begin
          if ForceEto then fTextDrawer.ForceNextTokenWithEto;
          fTextDrawer.ExtTextOut(rcToken.Left, rcToken.Top, ETOOptions-ETO_OPAQUE,
                                 rcToken, '...', 3, rcLine.Bottom);
        end;
      end;
    end;
  end;

  procedure AddHighlightToken(Token: PChar;
    TokenLen, PhysicalStartPos, PhysicalEndPos: integer;
    MarkupInfo : TSynSelectedColor);
  var
    CanAppend: boolean;
    SpacesTest, IsSpaces: boolean;
    i: integer;
    s: TSynFrameSide;

    function TokenIsSpaces: boolean;
    var
      pTok: PChar;
      Cnt: Integer;
    begin
      if not SpacesTest then begin
        SpacesTest := TRUE;
        IsSpaces := not (eoShowSpecialChars in fOptions) ;
        pTok := PChar(Pointer(Token));
        Cnt := TokenLen;
        while IsSpaces and (Cnt > 0) do begin
          if not (pTok^ in [' ',#9])
          then IsSpaces := False;
          Inc(pTok);
          dec(Cnt);
        end;
      end;
      Result := IsSpaces;
    end;

  begin
    {DebugLn('AddHighlightToken A TokenLen=',dbgs(TokenLen),
      ' PhysicalStartPos=',dbgs(PhysicalStartPos),' PhysicalEndPos=',dbgs(PhysicalEndPos),
      ' Tok="',copy(Token,1,TokenLen),'"');}

    // Do we have to paint the old chars first, or can we just append?
    CanAppend := FALSE;
    SpacesTest := FALSE;

    if (TokenAccu.Len > 0) then
    begin
      CanAppend :=
        // Frame can be continued
        (TokenAccu.FrameColor[sfdTop] = MarkupInfo.FrameSideColors[sfdTop]) and
        (TokenAccu.FrameStyle[sfdTop] = MarkupInfo.FrameSideStyles[sfdTop]) and
        (TokenAccu.FrameColor[sfdBottom] = MarkupInfo.FrameSideColors[sfdBottom]) and
        (TokenAccu.FrameStyle[sfdBottom] = MarkupInfo.FrameSideStyles[sfdBottom]) and
        (TokenAccu.FrameColor[sfdRight] = clNone) and
        (MarkupInfo.FrameSideColors[sfdLeft] = clNone) and
        // colors
        (TokenAccu.BG = MarkupInfo.Background) and
        // space-dependent
        ( ( (TokenAccu.FG = MarkupInfo.Foreground) and
            (TokenAccu.Style = MarkupInfo.Style)
          ) or
          // whitechar only token, can ignore Foreground color and certain styles (yet must match underline)
          ( (TokenAccu.Style - [fsBold, fsItalic] = MarkupInfo.Style - [fsBold, fsItalic]) and
            ( (TokenAccu.Style * [fsUnderline, fsStrikeOut] = []) or
              (TokenAccu.FG = MarkupInfo.Foreground)
            ) and
            TokenIsSpaces
          )
        );
      // If we can't append it, then we have to paint the old token chars first.
      if not CanAppend then
        PaintHighlightToken(FALSE);
    end;

    // Don't use AppendStr because it's more expensive.
    //if (CurLine=TopLine) then debugln('      -t-Accu len ',dbgs(TokenAccu.Len),' pstart ',dbgs(TokenAccu.PhysicalStartPos),' p-end ',dbgs(TokenAccu.PhysicalEndPos));
    if CanAppend then begin
      if (TokenAccu.Len + TokenLen > TokenAccu.MaxLen) then begin
        TokenAccu.MaxLen := TokenAccu.Len + TokenLen + 32;
        SetTokenAccuLength;
      end;
      // use move() ???
      for i := 0 to TokenLen-1 do begin
        TokenAccu.p[TokenAccu.Len + i] := Token[i];
      end;
      Inc(TokenAccu.Len, TokenLen);
      TokenAccu.PhysicalEndPos := PhysicalEndPos;
      TokenAccu.FrameColor[sfdRight] := MarkupInfo.FrameSideColors[sfdRight];
      TokenAccu.FrameStyle[sfdRight] := MarkupInfo.FrameSideStyles[sfdRight];
    end else begin
      TokenAccu.Len := TokenLen;
      if (TokenAccu.Len > TokenAccu.MaxLen) then begin
        TokenAccu.MaxLen := TokenAccu.Len + 32;
        SetTokenAccuLength;
      end;
      for i := 0 to TokenLen-1 do begin
        TokenAccu.p[i] := Token[i];
      end;
      TokenAccu.PhysicalStartPos := PhysicalStartPos;
      TokenAccu.PhysicalEndPos := PhysicalEndPos;
      TokenAccu.FG := MarkupInfo.Foreground;
      TokenAccu.BG := MarkupInfo.Background;
      TokenAccu.Style := MarkupInfo.Style;
      for s := low(TSynFrameSide) to high(TSynFrameSide) do begin
        TokenAccu.FrameColor[s] := MarkupInfo.FrameSideColors[s];
        TokenAccu.FrameStyle[s] := MarkupInfo.FrameSideStyles[s];
      end;
    end;
    {debugln('AddHighlightToken END CanAppend=',dbgs(CanAppend),
      ' Len=',dbgs(TokenAccu.Len),
      ' PhysicalStartPos=',dbgs(TokenAccu.PhysicalStartPos),
      ' PhysicalEndPos=',dbgs(TokenAccu.PhysicalEndPos),
      ' "',copy(TokenAccu.s,1,TokenAccu.Len),'"');}
  end;

  procedure DrawHiLightMarkupToken(attr: TSynHighlighterAttributes;
    sToken: PChar; nTokenByteLen: integer);
  var
    DefaultFGCol, DefaultBGCol: TColor;
    PhysicalStartPos: integer;
    PhysicalEndPos: integer;
    len: Integer;
    SubTokenByteLen, SubCharLen, TokenCharLen : Integer;
    NextPhysPos : Integer;

    function CharToByteLen(aCharLen: Integer) : Integer;
    begin
      if not UseUTF8 then exit(aCharLen);
      Result := UTF8CharToByteIndex(sToken, nTokenByteLen, aCharLen);
      if Result < 0 then begin
        debugln('ERROR: Could not convert CharLen (',dbgs(aCharLen),') to byteLen (maybe invalid UTF8?)',' len ',dbgs(nTokenByteLen),' Line ',dbgs(CurLine),' PhysPos ',dbgs(CurPhysPos));
        Result := aCharLen;
      end;
    end;

    procedure InitTokenColors;
    begin
      FPaintLineColor.Clear;
      if Assigned(attr) then
      begin
        DefaultFGCol := attr.Foreground;
        DefaultBGCol := attr.Background;
        if DefaultBGCol = clNone then DefaultBGCol := colEditorBG;
        if DefaultFGCol = clNone then DefaultFGCol := Font.Color;

        FPaintLineColor.FrameColor := attr.FrameColor;
        FPaintLineColor.FrameStyle := attr.FrameStyle;
        FPaintLineColor.FrameEdges := attr.FrameEdges;
        FPaintLineColor.Style      := attr.Style;
        // TSynSelectedColor.Style and StyleMask describe how to modify a style,
        // but FPaintLineColor contains an actual style
        FPaintLineColor.MergeFinalStyle := True;
        FPaintLineColor.StyleMask  := [];
        if FPaintLineColor.Background = clNone then
          FPaintLineColor.Background := colEditorBG;
        if FPaintLineColor.Foreground = clNone then
          FPaintLineColor.Foreground := Font.Color;
        if attr.FrameColor <> clNone then begin
          FPaintLineColor.StartX := PhysicalStartPos;
          FPaintLineColor.EndX   := PhysicalStartPos + TokenCharLen - 1;
          FPaintLineColor.MergeFrames(nil, PhysicalStartPos, PhysicalStartPos + TokenCharLen - 1);
        end;
      end else
      begin
        DefaultFGCol := Font.Color;
        DefaultBGCol := colEditorBG;
        FPaintLineColor.Style := Font.Style;
      end;

      FPaintLineColor.Foreground := DefaultFGCol;
      FPaintLineColor.Background := DefaultBGCol;
    end;

  begin
    if CurPhysPos > LastCol then exit;

    PhysicalStartPos := CurPhysPos;
    len := nTokenByteLen;
    TokenCharLen := ExpandSpecialChars(sToken, nTokenByteLen, PhysicalStartPos);
    CurLogIndex := CurLogIndex + len;
    // Prepare position for next token
    inc(CurPhysPos, TokenCharLen);
    if CurPhysPos <= FirstCol then exit;

    // Remove any Part of the Token that is before FirstCol
    if PhysicalStartPos < FirstCol then begin
      SubCharLen := FirstCol - PhysicalStartPos;
      len := CharToByteLen(SubCharLen);
      dec(TokenCharLen, SubCharLen);
      inc(PhysicalStartPos, SubCharLen);
      dec(nTokenByteLen, len);
      inc(sToken, len);
    end;

    // Remove any Part of the Token that is after LastCol
    SubCharLen := PhysicalStartPos + TokenCharLen - (LastCol + 1);
    if SubCharLen > 0 then begin
      dec(TokenCharLen, SubCharLen);
      nTokenByteLen := CharToByteLen(TokenCharLen);
    end;

    InitTokenColors;

    // Draw the token
    {TODO: cache NextPhysPos, and MarkupInfo between 2 calls }
    while (nTokenByteLen > 0) do begin
      FPaintLineColor2.Assign(FPaintLineColor);

      // Calculate Token Sublen for current Markup
      NextPhysPos := fMarkupManager.GetNextMarkupColAfterRowCol
                       (FFoldedLinesView.TextIndex[CurLine]+1, PhysicalStartPos);
      if NextPhysPos < 1
      then SubCharLen := TokenCharLen
      else SubCharLen := NextPhysPos - PhysicalStartPos;

      if SubCharLen > TokenCharLen then SubCharLen := TokenCharLen;
      if SubCharLen < 1 then begin // safety for broken input...
        debugln('ERROR: Got invalid SubCharLen ',dbgs(SubCharLen),' len ',dbgs(nTokenByteLen),' Line ',dbgs(CurLine),' PhysPos ',dbgs(CurPhysPos));
        SubCharLen:=1;
      end;

      SubTokenByteLen := CharToByteLen(SubCharLen);
      PhysicalEndPos:= PhysicalStartPos + SubCharLen - 1;

      // Calculate Markup
      fMarkupManager.MergeMarkupAttributeAtRowCol(FFoldedLinesView.TextIndex[CurLine]+1,
        PhysicalStartPos, PhysicalEndPos, FPaintLineColor2);

      // Deal with equal colors
      if (FPaintLineColor2.Background = FPaintLineColor2.Foreground) then begin // or if diff(gb,fg) < x
        if FPaintLineColor2.Background = DefaultBGCol then
          FPaintLineColor2.Foreground := not(FPaintLineColor2.Background) and $00ffffff // or maybe Font.color ?
        else
          FPaintLineColor2.Foreground := DefaultBGCol;
      end;

      // Add to TokenAccu
      AddHighlightToken(sToken, SubTokenByteLen,
        PhysicalStartPos, PhysicalEndPos, FPaintLineColor2);

      PhysicalStartPos:=PhysicalEndPos + 1;
      dec(nTokenByteLen,SubTokenByteLen);
      dec(TokenCharLen, SubCharLen);
      inc(sToken, SubTokenByteLen);
    end;
  end;

  {$IFDEF SYNDEBUGPRINT}
  procedure DebugPrint(Txt: String; MinCol: Integer = 0);
  begin
    if CurPhysPos < MinCol then Txt := StringOfChar(' ', MinCol - CurPhysPos) + txt;
    Setlength(CharWidths, length(CharWidths) + length(Txt));
    FillChar(CharWidths[length(CharWidths)-length(Txt)], length(Txt), #1);
    DrawHiLightMarkupToken(nil, PChar(Pointer(Txt)), Length(Txt));
  end;
  {$ENDIF}

  procedure PaintLines;
  var
    sLine: string; // the current line
    sToken: PChar; // highlighter token info
    nTokenLen: integer; // Pos in Char // Len in Byte ??
    attr: TSynHighlighterAttributes;
    ypos: Integer;
    DividerInfo: TSynDividerDrawConfigSetting;
    cl: Integer;
  begin
    // Initialize rcLine for drawing. Note that Top and Bottom are updated
    // inside the loop. Get only the starting point for this.
    rcLine := AClip;
    rcLine.Bottom := FirstLine * fTextHeight;
    // Make sure the token accumulator string doesn't get reassigned to often.
    if Assigned(fHighlighter) then begin
      TokenAccu.MaxLen := Max(128, fCharsInWindow * 4);
      SetTokenAccuLength;
    end;

    // Now loop through all the lines. The indices are valid for Lines.
    CurLine := FirstLine-1;
    while CurLine<LastLine do begin
      inc(CurLine);
      CurTextIndex := FFoldedLinesView.TextIndex[CurLine];
      //CharWidths := FFoldedLinesView.GetPhysicalCharWidths(CurLine);
      CharWidths := FTheLinesView.GetPhysicalCharWidths(CurTextIndex);

      fMarkupManager.PrepareMarkupForRow(CurTextIndex+1);
      // Get the line.
      // Update the rcLine rect to this line.
      rcLine.Top := rcLine.Bottom;
      Inc(rcLine.Bottom, fTextHeight);
      // Paint the lines depending on the assigned highlighter.
      rcToken := rcLine;
      TokenAccu.Len := 0;
      TokenAccu.PhysicalEndPos := FirstCol - 1; // in case of an empty line
      CurPhysPos := 1;
      CurLogIndex := 0;
      // Delete the whole Line
      fTextDrawer.BackColor := colEditorBG;
      SetBkColor(dc, ColorToRGB(colEditorBG));
      rcLine.Left := EraseLeft;
      InternalFillRect(dc, rcLine);
      rcLine.Left := DrawLeft;
      ForceEto := False;

      if not Assigned(fHighlighter) then begin
        sLine := FFoldedLinesView[CurLine];
        DrawHiLightMarkupToken(nil, PChar(Pointer(sLine)), Length(sLine));
      end else begin
        // draw splitter line
        DividerInfo := fHighlighter.DrawDivider[CurTextIndex];
        if (DividerInfo.Color <> clNone) and (nRightEdge > TextLeftPixelOffset(False) - 1) then
        begin
          ypos := rcToken.Bottom - 1;
          cl := DividerInfo.Color;
          if cl = clDefault then
            cl := fRightEdgeColor;
          fTextDrawer.DrawLine(nRightEdge, ypos, TextLeftPixelOffset(False) - 1, ypos, cl);
          dec(rcToken.Bottom);
        end;
        // Initialize highlighter with line text and range info. It is
        // necessary because we probably did not scan to the end of the last
        // line - the internal highlighter range might be wrong.
        fHighlighter.StartAtLineIndex(CurTextIndex);
        // Try to concatenate as many tokens as possible to minimize the count
        // of ExtTextOut calls necessary. This depends on the selection state
        // or the line having special colors. For spaces the foreground color
        // is ignored as well.
        //debugln('>>>> PaintLines Line=',dbgs(CurLine),' rect=',dbgs(rcToken));
        while not fHighlighter.GetEol do begin
          fHighlighter.GetTokenEx(sToken,nTokenLen);
          attr := fHighlighter.GetTokenAttribute;
          // Add Markup to the token and append it to the TokenAccu
          // record. This will paint any chars already stored if there is
          // a (visible) change in the attributes.
          DrawHiLightMarkupToken(attr,sToken,nTokenLen);
          // Let the highlighter scan the next token.
          fHighlighter.Next;
        end;
      end;
      // Draw anything that's left in the TokenAccu record. Fill to the end
      // of the invalid area with the correct colors.
      PaintHighlightToken(TRUE);

      fMarkupManager.FinishMarkupForRow(CurTextIndex+1);
    end;
    CurLine:=-1;
  end;

{ end local procedures }

begin
  if (AClip.Right < TextLeftPixelOffset(False)) then exit;
  if (AClip.Left > ClientWidth - TextRightPixelOffset) then exit;

  //DebugLn(['TCustomSynEdit.PaintTextLines ',dbgs(AClip)]);
  CurLine:=-1;
  FillChar(TokenAccu,SizeOf(TokenAccu),0);
  //DebugLn('TCustomSynEdit.PaintTextLines ',DbgSName(Self),' TopLine=',dbgs(TopLine),' AClip=',dbgs(AClip));
  colEditorBG := Color;
  if Assigned(fHighlighter) then
    fHighlighter.CurrentLines := FTheLinesView;
  // If the right edge is visible and in the invalid area, prepare to paint it.
  // Do this first to realize the pen when getting the dc variable.
  bDoRightEdge := FALSE;
  if (fRightEdge > 0) then begin // column value
    nRightEdge := fTextOffset + fRightEdge * fCharWidth; // pixel value
    if (nRightEdge >= AClip.Left) and (nRightEdge <= AClip.Right) then
      bDoRightEdge := TRUE;
    if nRightEdge > AClip.Right then
      nRightEdge := AClip.Right; // for divider draw lines (don't draw into right gutter)
  end
  else
    nRightEdge := AClip.Right;

  Canvas.Pen.Color := fRightEdgeColor; // used for code folding too
  Canvas.Pen.Width := 1;
  // Do everything else with API calls. This (maybe) realizes the new pen color.
  dc := Canvas.Handle;
  SetBkMode(dc, TRANSPARENT);

  // Adjust the invalid area to not include the gutter (nor the 2 ixel offset to the guttter).
  EraseLeft := AClip.Left;
  if (AClip.Left < TextLeftPixelOffset) then
    AClip.Left := TextLeftPixelOffset;
  DrawLeft := AClip.Left;

  if (LastLine >= FirstLine) then begin
    // Paint the visible text lines. To make this easier, compute first the
    // necessary information about the selected area: is there any visible
    // selected area, and what are its lines / columns?
    // Moved to two local procedures to make it easier to read.
    fTextDrawer.Style := Font.Style;
    fTextDrawer.BeginDrawing(dc);
    try
      PaintLines;
    finally
      fTextDrawer.EndDrawing;
    end;
  end;

  AClip.Top := (LastLine+1) * fTextHeight;
  if (AClip.Top < AClip.Bottom) then begin
    // Delete the remaining area
    SetBkColor(dc, ColorToRGB(colEditorBG));
    AClip.Left := EraseLeft;
    InternalFillRect(dc, AClip);
    AClip.Left := DrawLeft;

    // Draw the right edge if necessary.
    if bDoRightEdge and (not (eoHideRightMargin in Options)) then begin
      LCLIntf.MoveToEx(dc, nRightEdge, AClip.Top, nil);
      LCLIntf.LineTo(dc, nRightEdge, AClip.Bottom + 1);
    end;
  end;

  fMarkupManager.EndMarkup;
  ReAllocMem(TokenAccu.p,0);
end;

procedure TCustomSynEdit.StartPaintBuffer(const ClipRect: TRect);
{$IFDEF EnableDoubleBuf}
var
  NewBufferWidth: Integer;
  NewBufferHeight: Integer;
{$ENDIF}
begin
  if (SavedCanvas<>nil) then RaiseGDBException('');
  {$IFDEF EnableDoubleBuf}
  if BufferBitmap=nil then
    BufferBitmap:=TBitmap.Create;
  NewBufferWidth:=BufferBitmap.Width;
  NewBufferHeight:=BufferBitmap.Height;
  if NewBufferWidth<ClipRect.Right then
    NewBufferWidth:=ClipRect.Right;
  if NewBufferHeight<ClipRect.Bottom then
    NewBufferHeight:=ClipRect.Bottom;
  BufferBitmap.Width:=NewBufferWidth;
  BufferBitmap.Height:=NewBufferHeight;
  SavedCanvas:=Canvas;
  Canvas:=BufferBitmap.Canvas;
  {$ENDIF}
end;

procedure TCustomSynEdit.EndPaintBuffer(const ClipRect: TRect);
begin
  {$IFDEF EnableDoubleBuf}
  if (SavedCanvas=nil) then RaiseGDBException('');
  if not (SavedCanvas is TControlCanvas) then RaiseGDBException('');
  Canvas:=SavedCanvas;
  SavedCanvas:=nil;
  Canvas.CopyRect(ClipRect,BufferBitmap.Canvas,ClipRect);
  {$ENDIF}
end;

function TCustomSynEdit.NextWordLogicalPos(WordEndForDelete: Boolean): TPoint;
var
  CX, CY, LineLen: integer;
  Line: string;
  LogCaret: TPoint;
  DelSpaces : Boolean;
begin
  LogCaret:=LogicalCaretXY;
  CX := LogCaret.X;
  CY := LogCaret.Y;
  // valid line?
  if (CY >= 1) and (CY <= FTheLinesView.Count) then begin
    Line := FTheLinesView[CY - 1];
    LineLen := Length(Line);

    if CX >= LineLen then begin
      // find first IdentChar in the next line
      if CY < FTheLinesView.Count then begin
        Line := FTheLinesView[CY];
        Inc(CY);
        if WordEndForDelete then
          CX := Max(1, StrScanForCharInSet(Line, 1, [#1..#255] - TSynWhiteChars))
        else
          CX := Max(1, WordBreaker.NextWordStart(Line, 1, True));
      end;
    end else begin
      if WordEndForDelete then begin
        DelSpaces := WordBreaker.IsAtWordStart(Line, CX) or not WordBreaker.IsInWord(Line, CX);
        CX := WordBreaker.NextBoundary(Line, CX);
        if DelSpaces and(cx > 0) then
          CX := StrScanForCharInSet(Line, CX, [#1..#255] - TSynWhiteChars);
      end
      else
        CX := WordBreaker.NextWordStart(Line, CX);
      // if one of those failed just position at the end of the line
      if CX <= 0 then
        CX := LineLen + 1;
    end;
  end;
  Result := Point(CX, CY);
end;

function TCustomSynEdit.PrevWordLogicalPos: TPoint;
var
  CX, CY: integer;
  Line: string;
  LogCaret: TPoint;
begin
  LogCaret:=LogicalCaretXY;
  CX := LogCaret.X;
  CY := LogCaret.Y;
  // valid line?
  if (CY >= 1) and (CY <= FTheLinesView.Count) then begin
    Line := FTheLinesView[CY - 1];
    CX := WordBreaker.PrevWordStart(Line,  Min(CX, Length(Line) + 1));
    if CX <= 0 then
      if CY > 1 then begin
        // just position at the end of the previous line
        Dec(CY);
        Line := FTheLinesView[CY - 1];
        CX := Length(Line) + 1;
      end
      else
        CX := 1;
  end;
  Result := Point(CX, CY);
end;

procedure TCustomSynEdit.EraseBackground(DC: HDC);
begin
  // we are painting everything ourselves, so not need to erase background
end;

procedure TCustomSynEdit.Update;
begin
  Invalidate;
end;

procedure TCustomSynEdit.Invalidate;
begin
  {$IFDEF VerboseSynEditInvalidate}
  DebugLn(['TCustomSynEdit.Invalidate ',DbgSName(self)]);
  {$ENDIF}
  inherited Invalidate;
end;

function TCustomSynEdit.PluginCount: Integer;
begin
  Result := fPlugins.Count;
end;

function TCustomSynEdit.MarkupCount: Integer;
begin
  Result := FMarkupManager.Count;
end;

procedure TCustomSynEdit.PasteFromClipboard;
var
  ClipHelper: TSynClipboardStream;
begin
  ClipHelper := TSynClipboardStream.Create;
  try
    ClipHelper.ReadFromClipboard(Clipboard);
    PasteFromClipboardEx(ClipHelper);
  finally
    ClipHelper.Free;
  end;
end;

function TCustomSynEdit.PasteFromClipboardEx(ClipHelper: TSynClipboardStream) : Boolean;
var
  PTxt: PChar;
  PStr: String;
  PMode: TSynSelectionMode;
  InsStart: TPoint;
  PasteAction: TSynCopyPasteAction;
begin
  Result := False;
  InternalBeginUndoBlock;
  try
    PTxt := ClipHelper.TextP;
    PMode := ClipHelper.SelectionMode;
    PasteAction := scaContinue;
    if assigned(FOnPaste) then begin
      if ClipHelper.IsPlainText then PasteAction := scaPlainText;
      InsStart := FCaret.LineBytePos;
      if SelAvail and (not FBlockSelection.Persistent) and (eoOverwriteBlock in fOptions2) then
        InsStart := FBlockSelection.FirstLineBytePos;
      PStr := PTxt;
      FOnPaste(self, PStr, PMode, InsStart, PasteAction);
      PTxt := PChar(PStr);
      if (PStr = '') or (PasteAction = scaAbort) then
        exit;
    end;

    if ClipHelper.TextP = nil then
      exit;

    Result := True;
    if SelAvail and (not FBlockSelection.Persistent) and (eoOverwriteBlock in fOptions2) then
      FBlockSelection.SelText := '';
    InsStart := FCaret.LineBytePos;
    FInternalBlockSelection.StartLineBytePos := InsStart;
    FInternalBlockSelection.SetSelTextPrimitive(PMode, PTxt);
    FCaret.LineBytePos := FInternalBlockSelection.StartLineBytePos;

    if PasteAction = scaPlainText then
      exit;

    if eoFoldedCopyPaste in fOptions2 then begin
      PTxt := ClipHelper.GetTagPointer(synClipTagFold);
      if PTxt <> nil then begin
        ScanRanges;
        FFoldedLinesView.ApplyFoldDescription(InsStart.Y -1, InsStart.X,
            FInternalBlockSelection.StartLinePos-1, FInternalBlockSelection.StartBytePos,
            PTxt, ClipHelper.GetTagLen(synClipTagFold));
      end;
    end;
  finally
    InternalEndUndoBlock;
  end;
end;

procedure TCustomSynEdit.SelectAll;
var
  LastPt: TPoint;
begin
  DoIncPaintLock(Self); // No editing is taking place
  LastPt := Point(1, FTheLinesView.Count);
  if LastPt.y > 0 then
    Inc(LastPt.x, Length(FTheLinesView[LastPt.y - 1]))
  else
    LastPt.y  := 1;
  SetCaretAndSelection(LogicalToPhysicalPos(LastPt), Point(1, 1), LastPt);
  FBlockSelection.ActiveSelectionMode := smNormal;
  DoDecPaintLock(Self);
end;

procedure TCustomSynEdit.SetHighlightSearch(const ASearch : String; AOptions : TSynSearchOptions);
begin
  fMarkupHighAll.SearchOptions := AOptions;
  fMarkupHighAll.SearchString := ASearch;
end;

procedure TCustomSynEdit.SelectToBrace;
begin
  FindMatchingBracket(CaretXY,true,true,true,false);
end;

procedure TCustomSynEdit.SelectWord;
begin
  SetWordBlock(LogicalCaretXY);
end;

procedure TCustomSynEdit.SelectLine(WithLeadSpaces: Boolean = True);
begin
  SetLineBlock(CaretXY, WithLeadSpaces);
end;

procedure TCustomSynEdit.SelectParagraph;
begin
  SetParagraphBlock(CaretXY);
end;

procedure TCustomSynEdit.DoBlockSelectionChanged(Sender : TObject);
begin
  StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.SetBlockBegin(Value: TPoint); // logical position (byte)
begin
  fBlockSelection.StartLineBytePos := Value;
end;

procedure TCustomSynEdit.SetBlockEnd(Value: TPoint); // logical position (byte)
begin
  fBlockSelection.EndLineBytePos := Value;
end;

procedure TCustomSynEdit.SetBlockIndent(const AValue: integer);
begin
  if fBlockIndent=AValue then exit;
  fBlockIndent:=AValue;
end;

function TCustomSynEdit.GetCaretX : Integer;
begin
  Result:= FCaret.CharPos;
end;

function TCustomSynEdit.GetCaretY : Integer;
begin
  Result:= FCaret.LinePos;
end;

function TCustomSynEdit.GetCaretUndo: TSynEditUndoItem;
begin
  if SelAvail then
    Result := TSynEditUndoSelCaret.Create(FCaret.LineCharPos,
       FBlockSelection.StartLineBytePos, FBlockSelection.EndLineBytePos,
       FBlockSelection.ActiveSelectionMode)
  else
    Result := TSynEditUndoCaret.Create(FCaret.LineCharPos);
end;

function TCustomSynEdit.GetMarkup(Index: integer): TSynEditMarkup;
begin
  Result := fMarkupManager.Markup[Index];
end;

procedure TCustomSynEdit.SetCaretX(const Value: Integer);
begin
  FCaret.CharPos := Value;
end;

procedure TCustomSynEdit.SetCaretY(const Value: Integer);
begin
  FCaret.LinePos := Value;
end;

function TCustomSynEdit.GetCaretXY: TPoint;
begin
  Result := FCaret.LineCharPos;
end;

function TCustomSynEdit.GetFoldedCodeColor: TSynSelectedColor;
begin
  Result := FFoldedLinesView.MarkupInfoFoldedCode;
end;

function TCustomSynEdit.GetLines: TStrings;
begin
  Result := FStrings;
end;

procedure TCustomSynEdit.SetCaretXY(Value: TPoint);
// physical position (screen)
begin
  FCaret.ChangeOnTouch;
  FCaret.LineCharPos:= Value;
end;

procedure TCustomSynEdit.CaretChanged(Sender: TObject);
begin
  Include(fStateFlags, sfCaretChanged);
  if FCaret.OldCharPos <> FCaret.CharPos then
    Include(fStatusChanges, scCaretX);
  if FCaret.OldLinePos <> FCaret.LinePos then begin
    Include(fStatusChanges, scCaretY);
    InvalidateGutterLines(FCaret.OldLinePos, FCaret.OldLinePos);
    InvalidateGutterLines(FCaret.LinePos, FCaret.LinePos);
  end;
  EnsureCursorPosVisible;
  if fPaintLock = 0 then
    fMarkupHighCaret.CheckState; // Todo need a global lock, including the markup
end;

function TCustomSynEdit.CurrentMaxLeftChar: Integer;
begin
  if not HandleAllocated then // don't know chars in window yet
    exit(MaxInt);
  Result := FTheLinesView.LengthOfLongestLine;
  if (eoScrollPastEol in Options) and (Result < fMaxLeftChar) then
    Result := fMaxLeftChar;
  Result := Result - fCharsInWindow + 1 + FScreenCaret.ExtraLineChars;
end;

function TCustomSynEdit.CurrentMaxLineLen: Integer;
begin
  if not HandleAllocated then // don't know chars in window yet
    exit(MaxInt);
  Result := FTheLinesView.LengthOfLongestLine + 1;
  if (eoScrollPastEol in Options) and (Result < fMaxLeftChar) then
    Result := fMaxLeftChar;
end;

procedure TCustomSynEdit.SetLeftChar(Value: Integer);
begin
  Value := Min(Value, CurrentMaxLeftChar);
  Value := Max(Value, 1);
  if Value <> fLeftChar then begin
    fLeftChar := Value;
    fTextOffset := TextLeftPixelOffset - (LeftChar - 1) * fCharWidth;
    UpdateScrollBars;
    InvalidateLines(-1, -1);
    StatusChanged([scLeftChar]);
  end;
end;

procedure TCustomSynEdit.SetLines(Value: TStrings);
begin
  if HandleAllocated then
    FStrings.Assign(Value);
end;

function TCustomSynEdit.GetMarkupMgr: TObject;
begin
  Result := fMarkupManager;
end;

function TCustomSynEdit.GetCaretObj: TSynEditCaret;
begin
  Result := FCaret;
end;

procedure TCustomSynEdit.SetLineText(Value: string);
begin
  FCaret.LineText := Value;
end;

procedure TCustomSynEdit.SetName(const Value: TComponentName);
var
  TextToName: boolean;
begin
  TextToName := (ComponentState * [csDesigning, csLoading] = [csDesigning])
    and (TrimRight(Text) = Name);
  inherited SetName(Value);
  if TextToName then
    Text := Value;
end;

procedure TCustomSynEdit.CreateHandle;
begin
  Application.RemoveOnIdleHandler(@IdleScanRanges);
  DoIncPaintLock(nil);
  try
    inherited CreateHandle;   //SizeOrFontChanged will be called
    FLeftGutter.RecalcBounds;
    FRightGutter.RecalcBounds;
    UpdateScrollBars;
  finally
    DoDecPaintLock(nil);
  end;
end;

procedure TCustomSynEdit.SetScrollBars(const Value: TScrollStyle);
begin
  if (FScrollBars <> Value) then begin
    FScrollBars := Value;
    UpdateScrollBars;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetSelTextPrimitive(PasteMode: TSynSelectionMode;
  Value: PChar; AddToUndoList: Boolean = false);
Begin
  IncPaintLock;
  if not AddToUndoList then begin
    fUndoList.Lock;
    fRedoList.Lock;
  end;
  try
    FBlockSelection.SetSelTextPrimitive(PasteMode, Value);
  finally
    if not AddToUndoList then begin
      fUndoList.Unlock;
      fRedoList.Unlock;
    end;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.SetSelTextExternal(const Value: string);
begin
  // undo entry added
  InternalBeginUndoBlock;
  try
    FBlockSelection.SelText := Value;
  finally
    InternalEndUndoBlock;
  end;
end;

procedure TCustomSynEdit.SynSetText(const Value: string);
begin
  FLines.Text := Value;
end;

procedure TCustomSynEdit.RealSetText(const Value: TCaption);
begin
  FLines.Text := Value; // Do not trim
end;

function TCustomSynEdit.CurrentMaxTopLine: Integer;
begin
  if (eoScrollPastEof in Options) then
    Result := FTheLinesView.Count
  else
    Result := FFoldedLinesView.TextPosAddLines(FTheLinesView.Count+1, -Max(0, fLinesInWindow));
  Result := Max(Result, 1);
end;

procedure TCustomSynEdit.SetTopLine(Value: Integer);
begin
  // don't use MinMax here, it will fail in design mode (Lines.Count is zero,
  // but the painting code relies on TopLine >= 1)
  {$IFDEF SYNSCROLLDEBUG}
  if fPaintLock = 0 then debugln(['SetTopline outside Paintlock']);
  if (sfHasScrolled in fStateFlags) then debugln(['SetTopline with sfHasScrolled Value=',Value, '  FOldTopLine=',FOldTopLine,'  FOldTopView=',FOldTopView ]);
  {$ENDIF}
  Value := Min(Value, CurrentMaxTopLine);
  Value := Max(Value, 1);
  if FFoldedLinesView.FoldedAtTextIndex[Value-1] then
    Value := FindNextUnfoldedLine(Value, False);
  if FFoldedLinesView.FoldedAtTextIndex[Value-1] then
    Value := FindNextUnfoldedLine(Value, True);
  Value := Min(Value, CurrentMaxTopLine);
  FFoldedLinesView.TopTextIndex := fTopLine - 1;
  if Value <> fTopLine then begin
    fTopLine := Value;
    FFoldedLinesView.TopTextIndex := Value-1;
    UpdateScrollBars;
    // call MarkupMgr before ScrollAfterTopLineChanged, in case we aren't in a PaintLock
    fMarkupManager.TopLine:= fTopLine;
    if (sfPainting in fStateFlags) then debugln('SetTopline inside paint');
    ScrollAfterTopLineChanged;
    StatusChanged([scTopLine]);
  end
  else
    fMarkupManager.TopLine:= fTopLine;
  {$IFDEF SYNSCROLLDEBUG}
  if fPaintLock = 0 then debugln('SetTopline outside Paintlock EXIT');
  {$ENDIF}
end;

procedure TCustomSynEdit.ScrollAfterTopLineChanged;
var
  Delta: Integer;
begin
  if (sfPainting in fStateFlags) or (fPaintLock <> 0) or (not HandleAllocated) then
    exit;
  Delta := FOldTopView - TopView;
  {$IFDEF SYNSCROLLDEBUG}
  if (sfHasScrolled in fStateFlags) then debugln(['ScrollAfterTopLineChanged with sfHasScrolled Delta=',Delta,' Ftopline=',FTopLine, '  FOldTopLine=',FOldTopLine,'  FOldTopView=',FOldTopView ]);
  {$ENDIF}
  if Delta <> 0 then begin
    // TODO: SW_SMOOTHSCROLL --> can't get it work
    if (Abs(Delta) >= fLinesInWindow) or (sfHasScrolled in FStateFlags) then begin
      {$IFDEF SYNSCROLLDEBUG}
      debugln(['ScrollAfterTopLineChanged does invalidet Delta=',Delta]);
      {$ENDIF}
      Invalidate;
    end else
    if ScrollWindowEx(Handle, 0, fTextHeight * Delta, nil, nil, 0, nil, SW_INVALIDATE)
    then begin
      {$IFDEF SYNSCROLLDEBUG}
      debugln(['ScrollAfterTopLineChanged did scroll Delta=',Delta]);
      {$ENDIF}
      include(fStateFlags, sfHasScrolled);
    end else begin
      Invalidate;    // scrollwindow failed, invalidate all
      {$IFDEF SYNSCROLLDEBUG}
      debugln(['ScrollAfterTopLineChanged does invalidet (scroll failed) Delta=',Delta]);
      {$ENDIF}
    end;
  end;
  FOldTopLine := FTopLine;
  FOldTopView := TopView;
  if (Delta <> 0) and (eoAlwaysVisibleCaret in fOptions2) then
    MoveCaretToVisibleArea;
end;

procedure TCustomSynEdit.MoveCaretToVisibleArea;
// scroll to make the caret visible
var
  NewCaretXY: TPoint;
  MaxY: LongInt;
begin
  {$IFDEF SYNDEBUG}
  if (sfEnsureCursorPos in fStateFlags) then
    debugln('SynEdit. skip MoveCaretToVisibleArea');
  {$ENDIF}
  if (not HandleAllocated) or (sfEnsureCursorPos in fStateFlags) then
    exit;

  NewCaretXY:=CaretXY;
  if NewCaretXY.X < fLeftChar then
    NewCaretXY.X := fLeftChar
  else if NewCaretXY.X > fLeftChar + fCharsInWindow - FScreenCaret.ExtraLineChars then
    NewCaretXY.X := fLeftChar + fCharsInWindow - FScreenCaret.ExtraLineChars;
  if NewCaretXY.Y < fTopLine then
    NewCaretXY.Y := fTopLine
  else begin
    MaxY:= ScreenRowToRow(Max(0,fLinesInWindow-1));
    if NewCaretXY.Y > MaxY then
      NewCaretXY.Y := MaxY;
  end;
  if CompareCarets(CaretXY,NewCaretXY)<>0 then
  begin
    //DebugLn(['TCustomSynEdit.MoveCaretToVisibleArea Old=',dbgs(CaretXY),' New=',dbgs(NewCaretXY)]);
    FCaret.LineCharPos:=NewCaretXY;
  end;
end;

procedure TCustomSynEdit.MoveCaretIgnoreEOL(const NewCaret: TPoint);
begin
  FCaret.IncForcePastEOL;
  FCaret.LineCharPos := NewCaret;
  FCaret.DecForcePastEOL;
end;

procedure TCustomSynEdit.MoveLogicalCaretIgnoreEOL(const NewLogCaret: TPoint);
begin
  MoveCaretIgnoreEOL(LogicalToPhysicalPos(NewLogCaret));
end;

procedure TCustomSynEdit.UpdateCaret(IgnorePaintLock: Boolean = False);
{$IFDEF SYN_MBCSSUPPORT}
var
  cf: TCompositionForm;
{$ENDIF}
begin
  if ( (PaintLock <> 0) and not IgnorePaintLock ) or (not HandleAllocated)
  then begin
    Include(fStateFlags, sfCaretChanged);
  end else begin
    Exclude(fStateFlags, sfCaretChanged);
    if eoAlwaysVisibleCaret in fOptions2 then
      MoveCaretToVisibleArea;

    FScreenCaret.DisplayPos := Point(CaretXPix, CaretYPix);

{$IFDEF SYN_MBCSSUPPORT}
    if HandleAllocated then begin
      cf.dwStyle := CFS_POINT;
      cf.ptCurrentPos := Point(CX, CY);
      ImmSetCompositionWindow(ImmGetContext(Handle), @cf);
    end;
{$ENDIF}
  end;
end;

procedure TCustomSynEdit.UpdateScrollBars;
var
  ScrollInfo: TScrollInfo;
begin
  if FScrollBarUpdateLock <> 0 then exit;
  if not HandleAllocated or (PaintLock <> 0) then
    Include(fStateFlags, sfScrollbarChanged)
  else begin
    Exclude(fStateFlags, sfScrollbarChanged);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL and not SIF_TRACKPOS;
    ScrollInfo.nMin := 1;
    ScrollInfo.nTrackPos := 0;

    // Horizontal
    ScrollInfo.nMax := FTheLinesView.LengthOfLongestLine + 1;
    if (eoScrollPastEol in Options) and (ScrollInfo.nMax < fMaxLeftChar + 1) then
      ScrollInfo.nMax := fMaxLeftChar + 1;
    inc(ScrollInfo.nMax, FScreenCaret.ExtraLineChars);
    if ((fScrollBars in [ssBoth, ssHorizontal]) or
        ((fScrollBars in [ssAutoBoth, ssAutoHorizontal]) and (ScrollInfo.nMax - 1 > CharsInWindow))
       ) xor (sfHorizScrollbarVisible in fStateFlags)
    then begin
      if (sfHorizScrollbarVisible in fStateFlags)
        then exclude(fStateFlags, sfHorizScrollbarVisible)
        else include(fStateFlags, sfHorizScrollbarVisible);
      if fStateFlags * [sfEnsureCursorPos, sfEnsureCursorPosAtResize] <> [] then
        include(fStateFlags, sfEnsureCursorPosAtResize);
      ShowScrollBar(Handle, SB_Horz, sfHorizScrollbarVisible in fStateFlags);
      RecalcCharsAndLinesInWin(True);
    end;
    ScrollInfo.nPage := CharsInWindow;
    ScrollInfo.nPos := LeftChar;
    SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
    {$IFNDEF LCLWin32} {$IFnDEF SynScrollBarWorkaround}
    if not (sfHorizScrollbarVisible in fStateFlags) then
      ShowScrollBar(Handle, SB_Horz, False);
    {$ENDIF} {$ENDIF}
    //DebugLn('[TCustomSynEdit.UpdateScrollbars] nMin=',ScrollInfo.nMin,' nMax=',ScrollInfo.nMax,
    //' nPage=',ScrollInfo.nPage,' nPos=',ScrollInfo.nPos,' ClientW=',ClientWidth);

    // Vertical
    ScrollInfo.nMax := FFoldedLinesView.Count+1;
    if (eoScrollPastEof in Options) then
      Inc(ScrollInfo.nMax, LinesInWindow - 1);
    if ((fScrollBars in [ssBoth, ssVertical]) or
        ((fScrollBars in [ssAutoBoth, ssAutoVertical]) and (ScrollInfo.nMax - 1 > LinesInWindow))
       ) xor (sfVertScrollbarVisible in fStateFlags)
    then begin
      if (sfVertScrollbarVisible in fStateFlags)
        then exclude(fStateFlags, sfVertScrollbarVisible)
        else include(fStateFlags, sfVertScrollbarVisible);
      if fStateFlags * [sfEnsureCursorPos, sfEnsureCursorPosAtResize] <> [] then
        include(fStateFlags, sfEnsureCursorPosAtResize);
      ShowScrollBar(Handle, SB_Vert, sfVertScrollbarVisible in fStateFlags);
      RecalcCharsAndLinesInWin(True);
    end;
    ScrollInfo.nPage := LinesInWindow;
    ScrollInfo.nPos := FFoldedLinesView.TextIndexToViewPos(TopLine-1);
    SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
    {$IFNDEF LCLWin32} {$IFnDEF SynScrollBarWorkaround}
    if not (sfVertScrollbarVisible in fStateFlags) then
      ShowScrollBar(Handle, SB_Vert, False);
    {$ENDIF} {$ENDIF}
  end;
end;

procedure TCustomSynEdit.WMDropFiles(var Msg: TMessage);
{$IFNDEF SYN_LAZARUS}
// ToDo DropFiles
var
  i, iNumberDropped: integer;
  szPathName: array[0..260] of char;
  Point: TPoint;
  FilesList: TStringList;
{$ENDIF}
begin
  {$IFDEF SYN_LAZARUS}
  LastMouseCaret:=Point(-1,-1);
  {$ELSE}
  try
    if Assigned(fOnDropFiles) then begin
      FilesList := TStringList.Create;
      try
        iNumberDropped := DragQueryFile(THandle(Msg.wParam), Cardinal(-1),
          nil, 0);
        DragQueryPoint(THandle(Msg.wParam), Point);

        for i := 0 to iNumberDropped - 1 do begin
          DragQueryFile(THandle(Msg.wParam), i, szPathName,
            SizeOf(szPathName));
          FilesList.Add(szPathName);
        end;
        fOnDropFiles(Self, Point.X, Point.Y, FilesList);
      finally
        FilesList.Free;
      end;
    end;
  finally
    Msg.Result := 0;
    DragFinish(THandle(Msg.wParam));
  end;
  {$ENDIF}
end;

procedure TCustomSynEdit.WMExit(var Message: TLMExit);
begin
  LastMouseCaret:=Point(-1,-1);
end;

procedure TCustomSynEdit.WMEraseBkgnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;

procedure TCustomSynEdit.WMGetDlgCode(var Msg: TWMGetDlgCode);
begin
  inherited;
  Msg.Result := DLGC_WANTARROWS or DLGC_WANTCHARS or DLGC_WANTALLKEYS;
  if fWantTabs and (GetKeyState(VK_CONTROL) >= 0) then
    Msg.Result := Msg.Result or DLGC_WANTTAB;
end;

procedure TCustomSynEdit.WMHScroll(var Msg: {$IFDEF SYN_LAZARUS}TLMScroll{$ELSE}TWMScroll{$ENDIF});
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the line
    SB_TOP: LeftChar := 1;
    SB_BOTTOM: LeftChar := CurrentMaxLeftChar;
      // Scrolls one char left / right
    SB_LINEDOWN: LeftChar := LeftChar + 1;
    SB_LINEUP: LeftChar := LeftChar - 1;
      // Scrolls one page of chars left / right
    SB_PAGEDOWN: LeftChar := LeftChar
      + Max(1, (fCharsInWindow - Ord(eoScrollByOneLess in fOptions)));
    SB_PAGEUP: LeftChar := LeftChar
      - Max(1, (fCharsInWindow - Ord(eoScrollByOneLess in fOptions)));
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: LeftChar := Msg.Pos;
  end;
end;

procedure TCustomSynEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  if fCaret = nil then exit; // This SynEdit is in Destroy
  Exclude(FStateFlags, sfHideCursor);
  inherited;
  {$IFDEF VerboseFocus}
  DebugLn(['[TCustomSynEdit.WMKillFocus] A ',Name, ' time=', dbgs(Now*86640)]);
  {$ENDIF}
  LastMouseCaret:=Point(-1,-1);
  // Todo: Under Windows, keeping the Caret only works, if no other component creates a caret
  if not (eoPersistentCaret in fOptions) then begin
    FScreenCaret.Visible := False;
    FScreenCaret.DestroyCaret;
  end;
  if FHideSelection and SelAvail then
    Invalidate;
  inherited;
end;

procedure TCustomSynEdit.WMSetFocus(var Msg: TLMSetFocus);
begin
  if fCaret = nil then exit; // This SynEdit is in Destroy
  Exclude(FStateFlags, sfHideCursor);
  LastMouseCaret:=Point(-1,-1);
  {$IFDEF VerboseFocus}
  DebugLn(['[TCustomSynEdit.WMSetFocus] A ',Name,':',ClassName, ' time=', dbgs(Now*86640)]);
  {$ENDIF}
  FScreenCaret.DestroyCaret; // Ensure recreation. On Windows only one caret exists, and it must be moved to the focused editor
  FScreenCaret.Visible := True;
  //if FHideSelection and SelAvail then
  //  Invalidate;
  inherited;
  //DebugLn('[TCustomSynEdit.WMSetFocus] END');
end;

procedure TCustomSynEdit.DoOnResize;
begin
  inherited;
  if (not HandleAllocated) or ((ClientWidth = FOldWidth) and (ClientHeight = FOldHeight)) then exit;
  FOldWidth := ClientWidth;
  FOldHeight := ClientHeight;
  inc(FScrollBarUpdateLock);
  FScreenCaret.Lock;
  try
    FLeftGutter.RecalcBounds;
    FRightGutter.RecalcBounds;
    SizeOrFontChanged(FALSE);
    if sfEnsureCursorPosAtResize in fStateFlags then
      EnsureCursorPosVisible;
    Exclude(fStateFlags, sfEnsureCursorPosAtResize);
  finally
    FScreenCaret.UnLock;
    dec(FScrollBarUpdateLock);
    UpdateScrollBars;
  end;
  //debugln('TCustomSynEdit.Resize ',dbgs(Width),',',dbgs(Height),',',dbgs(ClientWidth),',',dbgs(ClientHeight));
  // SetLeftChar(LeftChar);                                                     //mh 2000-10-19
end;

var
  ScrollHintWnd: THintWindow;

function GetScrollHint: THintWindow;
begin
  if ScrollHintWnd = nil then begin
    ScrollHintWnd := HintWindowClass.Create(Application);
    ScrollHintWnd.Name:='SynEditScrollHintWnd';
    ScrollHintWnd.Visible := FALSE;
  end;
  Result := ScrollHintWnd;
  Result.AutoHide := True;  // Because SB_ENDSCROLL never happens under LCL-GTK2
  Result.HideInterval := 1500;
end;

procedure TCustomSynEdit.WMVScroll(var Msg: {$IFDEF SYN_LAZARUS}TLMScroll{$ELSE}TWMScroll{$ENDIF});
var
  s: ShortString;
  rc: TRect;
  pt: TPoint;
  ScrollHint: THintWindow;
begin
  //debugln('TCustomSynEdit.WMVScroll A ',DbgSName(Self),' Msg.ScrollCode=',dbgs(Msg.ScrollCode),' SB_PAGEDOWN=',dbgs(SB_PAGEDOWN),' SB_PAGEUP=',dbgs(SB_PAGEUP));
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP: TopLine := 1;
    SB_BOTTOM: TopLine := FTheLinesView.Count;
      // Scrolls one line up / down
    SB_LINEDOWN: TopView := TopView + 1;
    SB_LINEUP: TopView := TopView - 1;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN: TopView := TopView
      + Max(1, (fLinesInWindow - Ord(eoScrollByOneLess in fOptions))); // TODO: scroll half page ?
    SB_PAGEUP: TopView := TopView
      - Max(1, (fLinesInWindow - Ord(eoScrollByOneLess in fOptions)));
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
        TopView := Msg.Pos;

        if eoShowScrollHint in fOptions then begin
          ScrollHint := GetScrollHint;
          if not ScrollHint.Visible then begin
            ScrollHint.Color := Application.HintColor;
            ScrollHint.Visible := TRUE;
          end;
          s := Format('line %d', [TopLine]);
          rc := ScrollHint.CalcHintRect(200, s, nil);
          pt := ClientToScreen(Point(
                ClientWidth-ScrollBarWidth
                        - rc.Right - 4, 10));
          if eoScrollHintFollows in fOptions then
            pt.y := Mouse.CursorPos.y - (rc.Bottom div 2);
          OffsetRect(rc, pt.x, pt.y);
          ScrollHint.ActivateHint(rc, s);
          ScrollHint.Invalidate;
          ScrollHint.Update;
        end;
      end;
      // Ends scrolling
    SB_ENDSCROLL:
      if eoShowScrollHint in fOptions then
        with GetScrollHint do begin
          Visible := FALSE;
          ActivateHint(Rect(0, 0, 0, 0), '');
        end;
  end;
end;

procedure TCustomSynEdit.ScanRanges(ATextChanged: Boolean = True);
begin
  if not HandleAllocated then begin
    Application.RemoveOnIdleHandler(@IdleScanRanges); // avoid duplicate add
    if assigned(FHighlighter) then
      Application.AddOnIdleHandler(@IdleScanRanges, False);
    exit;
  end;
  if not assigned(FHighlighter) then begin
    if ATextChanged then begin
      fMarkupManager.TextChanged(FChangedLinesStart, FChangedLinesEnd);
      // TODO: see TSynEditFoldedView.LineCountChanged, this is only needed, because NeedFixFrom does not always work
      FFoldedLinesView.FixFoldingAtTextIndex(FChangedLinesStart, FChangedLinesEnd);
    end;
    Topline := TopLine;
    exit;
  end;
  FHighlighter.CurrentLines := FLines; // Trailing spaces are not needed
  FHighlighter.ScanRanges;

  // Todo: text may not have changed
  if ATextChanged then
    fMarkupManager.TextChanged(FChangedLinesStart, FChangedLinesEnd);
  Topline := TopLine;
end;

procedure TCustomSynEdit.IdleScanRanges(Sender: TObject; var Done: Boolean);
begin
  Application.RemoveOnIdleHandler(@IdleScanRanges);
  if not assigned(FHighlighter) then
    exit;

  FHighlighter.CurrentLines := FLines; // Trailing spaces are not needed
  if not FHighlighter.IdleScanRanges then
    exit;

  // Move to the end; give others a change too
  Application.AddOnIdleHandler(@IdleScanRanges, False);
  Done := False;
end;

procedure TCustomSynEdit.LineCountChanged(Sender: TSynEditStrings;
  AIndex, ACount: Integer);
begin
  {$IFDEF SynFoldDebug}debugln(['FOLD-- LineCountChanged Aindex', AIndex, '  ACount=', ACount]);{$ENDIF}
  if (AIndex < FBeautifyStartLineIdx) or (FBeautifyStartLineIdx < 0) then
    FBeautifyStartLineIdx := AIndex;
  if ACount > 0 then begin
    if (AIndex > FBeautifyEndLineIdx) then
      FBeautifyEndLineIdx := AIndex + ACount - 1
    else
      FBeautifyEndLineIdx := FBeautifyEndLineIdx + ACount;
  end else begin
    FBeautifyEndLineIdx := FBeautifyEndLineIdx + ACount;
    if (FBeautifyEndLineIdx < AIndex) then
      FBeautifyEndLineIdx := AIndex;
  end;

  if PaintLock>0 then begin
    // FChangedLinesStart is also given to Markup.TextChanged; but it is not used there
    if (FChangedLinesStart<1) or (FChangedLinesStart>AIndex+1) then
      FChangedLinesStart:=AIndex+1;
    FChangedLinesEnd := -1; // Invalidate the rest of lines
  end else begin
    ScanRanges;
    InvalidateLines(AIndex + 1, -1);
    InvalidateGutterLines(AIndex + 1, -1);
    if FCaret.LinePos > FLines.Count then FCaret.LinePos := FLines.Count;
  end;
  if TopLine > AIndex + 1 then
    TopLine := TopLine + ACount // will call UpdateScrollBars
  else
    UpdateScrollBars;
end;

procedure TCustomSynEdit.LineTextChanged(Sender: TSynEditStrings;
  AIndex, ACount: Integer);
begin
  {$IFDEF SynFoldDebug}debugln(['FOLD-- LineTextChanged Aindex', AIndex, '  ACount=', ACount]);{$ENDIF}
  if (AIndex < FBeautifyStartLineIdx) or (FBeautifyStartLineIdx < 0) then
    FBeautifyStartLineIdx := AIndex;
  if (AIndex + ACount - 1 > FBeautifyEndLineIdx) then
    FBeautifyEndLineIdx := AIndex + ACount - 1;

  if PaintLock>0 then begin
    if (FChangedLinesStart<1) or (FChangedLinesStart>AIndex+1) then
      FChangedLinesStart:=AIndex+1;
    if (FChangedLinesEnd >= 0) and (FChangedLinesEnd<AIndex+1) then
      FChangedLinesEnd:=AIndex + 1 + MaX(ACount, 0);
  end else begin
    ScanRanges;
    InvalidateLines(AIndex + 1, AIndex + ACount);
    InvalidateGutterLines(AIndex + 1, AIndex + ACount);
  end;
  UpdateScrollBars;
end;

procedure TCustomSynEdit.DoHighlightChanged(Sender: TSynEditStrings; AIndex,
  ACount: Integer);
begin
  InvalidateLines(AIndex + 1, AIndex + 1 + ACount);
  InvalidateGutterLines(AIndex + 1, AIndex + 1 + ACount);
  FFoldedLinesView.FixFoldingAtTextIndex(AIndex, AIndex + ACount);
  if FPendingFoldState <> '' then
    SetFoldState(FPendingFoldState);
end;

procedure TCustomSynEdit.ListCleared(Sender: TObject);
begin
  ClearUndo;
  // invalidate the *whole* client area
  Invalidate;
  // set caret and selected block to start of text
  SetBlockBegin(Point(1, 1));
  SetCaretXY(Point(1, 1));
  // scroll to start of text
  TopLine := 1;
  LeftChar := 1;
  StatusChanged(scTextCleared);
end;

procedure TCustomSynEdit.FoldChanged(Index : integer);
var
  i: Integer;
begin
  {$IFDEF SynFoldDebug}debugln(['FOLD-- FoldChanged; Index=', Index, ' topline=', TopLine, '  ScreenRowToRow(LinesInWindow + 1)=', ScreenRowToRow(LinesInWindow + 1)]);{$ENDIF}
  TopLine := TopLine;
  i := FFoldedLinesView.CollapsedLineForFoldAtLine(CaretY);
  if i > 0 then begin
    SetCaretXY(Point(1, i));
    UpdateCaret;
  end
  else
  if eoAlwaysVisibleCaret in fOptions2 then
    MoveCaretToVisibleArea;
  UpdateScrollBars;
  if Index + 1 > Max(1, ScreenRowToRow(LinesInWindow + 1)) then exit;
  if Index + 1 < TopLine then Index := TopLine - 1;
  InvalidateLines(Index + 1, -1);
  InvalidateGutterLines(Index + 1, -1);
end;

procedure TCustomSynEdit.SetTopView(const AValue : Integer);
begin
  TopLine := FFoldedLinesView.ViewPosToTextIndex(AValue)+1;
end;

function TCustomSynEdit.GetTopView : Integer;
begin
  Result := FFoldedLinesView.TextIndexToViewPos(TopLine-1);
end;

procedure TCustomSynEdit.SetWordBlock(Value: TPoint);
var
  TempString: string;
  x: Integer;
begin
  { Value is the position of the Caret in bytes }
  Value.y := MinMax(Value.y, 1, FTheLinesView.Count);
  TempString := FTheLinesView[Value.Y - 1];
  if TempString = '' then exit;
  x := MinMax(Value.x, 1, Length(TempString)+1);

  Value.X := WordBreaker.PrevWordStart(TempString, x, True);
  if Value.X < 0 then
    Value.X := WordBreaker.NextWordStart(TempString, x);
  if Value.X < 0 then
    exit;

  DoIncPaintLock(Self); // No editing is taking place
  FBlockSelection.StartLineBytePos := Value;
  Value.X := WordBreaker.NextWordEnd(TempString, Value.X);
  FBlockSelection.EndLineBytePos := Value;
  FBlockSelection.ActiveSelectionMode := smNormal;
  FCaret.LineBytePos := Value;
  DoDecPaintLock(Self);
end;

procedure TCustomSynEdit.SetLineBlock(Value: TPoint; WithLeadSpaces: Boolean = True);
var
  ALine: string;
  x, x2: Integer;
begin
  DoIncPaintLock(Self); // No editing is taking place
  FBlockSelection.StartLineBytePos := Point(1,MinMax(Value.y, 1, FTheLinesView.Count));
  FBlockSelection.EndLineBytePos := Point(1,MinMax(Value.y+1, 1, FTheLinesView.Count));
  if (FBlockSelection.StartLinePos >= 1)
  and (FBlockSelection.StartLinePos <= FTheLinesView.Count) then begin
    ALine:=FTheLinesView[FBlockSelection.StartLinePos - 1];
    x2:=length(ALine)+1;
    if not WithLeadSpaces then begin
      x := FBlockSelection.StartBytePos;
      while (x<length(ALine)) and (ALine[x] in [' ',#9]) do
        inc(x);
      FBlockSelection.StartLineBytePos := Point(x,MinMax(Value.y, 1, FTheLinesView.Count));
      while (x2 > x) and (ALine[X2-1] in [' ',#9]) do
        dec(x2);
    end;
    FBlockSelection.EndLineBytePos := Point(x2, MinMax(Value.y, 1, FTheLinesView.Count));
  end;
  FBlockSelection.ActiveSelectionMode := smNormal;
  LogicalCaretXY := FBlockSelection.EndLineBytePos;
  //DebugLn(' FFF2 ',Value.X,',',Value.Y,' BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
  DoDecPaintLock(Self);
end;

procedure TCustomSynEdit.SetParagraphBlock(Value: TPoint);
var
  ParagraphStartLine, ParagraphEndLine, ParagraphEndX: integer;

begin
  DoIncPaintLock(Self); // No editing is taking place
  ParagraphStartLine := MinMax(Value.y,   1, FTheLinesView.Count);
  ParagraphEndLine   := MinMax(Value.y+1, 1, FTheLinesView.Count);
  ParagraphEndX := 1;
  while (ParagraphStartLine > 1) and
        (Trim(FTheLinesView[ParagraphStartLine-1])<>'')
  do
    dec(ParagraphStartLine);
  while (ParagraphEndLine <= FTheLinesView.Count) and
        (Trim(FTheLinesView[ParagraphEndLine-1])<>'')
  do
    inc(ParagraphEndLine);
  if (ParagraphEndLine > FTheLinesView.Count) then begin
    dec(ParagraphEndLine);
    ParagraphEndX := length(FTheLinesView[ParagraphEndLine-1]) + 1;
  end;
  FBlockSelection.StartLineBytePos := Point(1, ParagraphStartLine);
  FBlockSelection.EndLineBytePos   := Point(ParagraphEndX, ParagraphEndLine);
  FBlockSelection.ActiveSelectionMode := smNormal;
  CaretXY := FBlockSelection.EndLineBytePos;
  //DebugLn(' FFF3 ',Value.X,',',Value.Y,' BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
  DoDecPaintLock(Self);
end;

function TCustomSynEdit.GetCanUndo: Boolean;
begin
  result := fUndoList.CanUndo;
end;

function TCustomSynEdit.GetCanRedo: Boolean;
begin
  result := fRedoList.CanUndo;
end;

function TCustomSynEdit.GetCanPaste:Boolean;
begin
  Result := Clipboard.HasFormat(CF_TEXT)
    or Clipboard.HasFormat(TSynClipboardStream.ClipboardFormatId)
end;

procedure TCustomSynEdit.Redo;
var
  Item: TSynEditUndoItem;
  Group: TSynEditUndoGroup;
begin
  Group := fRedoList.PopItem;
  if Group <> nil then begin;
    IncPaintLock;
    FTheLinesView.IsRedoing := True;
    Item := Group.Pop;
    if Item <> nil then begin
      InternalBeginUndoBlock;
      fUndoList.CurrentGroup.Reason := Group.Reason;
      fUndoList.IsInsideRedo := True;
      try
        repeat
          RedoItem(Item);
          Item := Group.Pop;
        until (Item = nil);
      finally
        InternalEndUndoBlock;
      end;
    end;
    FTheLinesView.IsRedoing := False;
    Group.Free;
    if fRedoList.IsTopMarkedAsUnmodified then
      fUndoList.MarkTopAsUnmodified;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.RedoItem(Item: TSynEditUndoItem);
var
  Line, StrToDelete: PChar;
  x, y, Len, Len2: integer;

  function GetLeadWSLen : integer;
  var
    Run : PChar;
  begin
    Run := Line;
    while (Run[0] in [' ', #9]) do
      Inc(Run);
    Result := Run - Line;
  end;

begin
  if Assigned(Item) then
  try
    FCaret.IncForcePastEOL;
    if Item.ClassType = TSynEditUndoIndent then
    begin // re-insert the column
      SetCaretAndSelection(LogicalToPhysicalPos(Point(1,TSynEditUndoIndent(Item).FPosY1)),
        Point(1, TSynEditUndoIndent(Item).FPosY1), Point(2, TSynEditUndoIndent(Item).FPosY2),
        smNormal);
      x := FBlockIndent;
      y := FBlockTabIndent;
      FBlockIndent := TSynEditUndoIndent(Item).FCnt;
      FBlockTabIndent := TSynEditUndoIndent(Item).FTabCnt;
      DoBlockIndent;
      FBlockIndent := x;
      FBlockTabIndent := y;
    end
    else
    if Item.ClassType = TSynEditUndoUnIndent then
    begin // re-delete the (raggered) column
      // add to undo list
      fUndoList.AddChange(TSynEditUndoUnIndent.Create(TSynEditUndoUnIndent(Item).FPosY1,
         TSynEditUndoUnIndent(Item).FPosY2, TSynEditUndoUnIndent(Item).FText));
      // Delete string
      fUndoList.Lock;
      StrToDelete := PChar(TSynEditUndoUnIndent(Item).FText);
      x := -1;
      for y := TSynEditUndoUnIndent(Item).FPosY1 to TSynEditUndoUnIndent(Item).FPosY2 do begin
        Line := PChar(FTheLinesView[y - 1]);
        Len := GetLeadWSLen;
        Len2 := GetEOL(StrToDelete) - StrToDelete;
        if (Len2 > 0) and (Len >= Len2) then
          FTheLinesView.EditDelete(1+Len-Len2, y, Len2);
        inc(StrToDelete, Len2+1);
      end;
      fUndoList.Unlock;
    end
    else
      if not Item.PerformUndo(self) then
        FTheLinesView.EditRedo(Item);
  finally
    FCaret.DecForcePastEOL;
    Item.Free;
  end;
end;

procedure TCustomSynEdit.UpdateCursor;
begin
  if (sfHideCursor in FStateFlags) and (eoAutoHideCursor in fOptions2) then begin
    SetCursor(crNone);
    exit;
  end;

  if (FLastMousePoint.X >= TextLeftPixelOffset(False)) and
     (FLastMousePoint.X < ClientWidth - TextRightPixelOffset - ScrollBarWidth) and
     (FLastMousePoint.Y >= 0) and (FLastMousePoint.Y < ClientHeight - ScrollBarWidth) then
  begin
    if Assigned(FMarkupCtrlMouse) and (FMarkupCtrlMouse.Cursor <> crDefault) then
      Cursor := FMarkupCtrlMouse.Cursor
    else
      Cursor := crIBeam;
  end
  else
    Cursor := crDefault;
end;

procedure TCustomSynEdit.Undo;
var
  Item: TSynEditUndoItem;
  Group: TSynEditUndoGroup;
begin
  Group := fUndoList.PopItem;
  if Group <> nil then begin;
    IncPaintLock;
    FTheLinesView.IsUndoing := True;
    Item := Group.Pop;
    if Item <> nil then begin
      InternalBeginUndoBlock(fRedoList);
      fRedoList.CurrentGroup.Reason := Group.Reason;
      fUndoList.Lock;
      try
        repeat
          UndoItem(Item);
          Item := Group.Pop;
        until (Item = nil);
      finally
        // Todo: Decide what do to, If there are any trimable spaces.
        FTrimmedLinesView.ForceTrim;
        fUndoList.UnLock;
        InternalEndUndoBlock(fRedoList);
      end;
    end;
    FTheLinesView.IsUndoing := False;
    Group.Free;
    if fUndoList.IsTopMarkedAsUnmodified then
      fRedoList.MarkTopAsUnmodified;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.UndoItem(Item: TSynEditUndoItem);
var
  Line, OldText: PChar;
  y, Len, Len2, LenT: integer;

  function GetLeadWSLen : integer;
  var
    Run : PChar;
  begin
    Run := Line;
    while (Run[0] in [' ', #9]) do
      Inc(Run);
    Result := Run - Line;
  end;

begin
  if Assigned(Item) then try
    FCaret.IncForcePastEOL;

    if Item.ClassType = TSynEditUndoIndent then
    begin
      // add to redo list
      fRedoList.AddChange(TSynEditUndoIndent.Create(TSynEditUndoIndent(Item).FPosY1,
         TSynEditUndoIndent(Item).FPosY2, TSynEditUndoIndent(Item).FCnt, TSynEditUndoIndent(Item).FTabCnt));
      // quick unintend (must all be spaces, as inserted...)
      fRedoList.Lock;
      Len2 := TSynEditUndoIndent(Item).FCnt;
      LenT := TSynEditUndoIndent(Item).FTabCnt;
      for y := TSynEditUndoUnIndent(Item).FPosY1 to TSynEditUndoUnIndent(Item).FPosY2 do begin
        Line := PChar(FTheLinesView[y - 1]);
        Len := GetLeadWSLen;
        FTheLinesView.EditDelete(Len+1-Len2, y, Len2);
        FTheLinesView.EditDelete(1, y, LenT);
      end;
      fRedoList.Unlock;
    end
    else

    if Item.ClassType = TSynEditUndoUnIndent then
    begin
      fRedoList.AddChange(TSynEditUndoUnIndent.Create(TSynEditUndoUnIndent(Item).FPosY1,
          TSynEditUndoUnIndent(Item).FPosY2, TSynEditUndoUnIndent(Item).FText));
      // reinsert the string
      fRedoList.Lock;
      OldText := PChar(TSynEditUndoUnIndent(Item).FText);
      for y := TSynEditUndoUnIndent(Item).FPosY1 to TSynEditUndoUnIndent(Item).FPosY2 do begin
        Line := PChar(FTheLinesView[y - 1]);
        Len := GetLeadWSLen;
        Len2 := GetEOL(OldText) - OldText;
        if Len2 > 0 then
          FTheLinesView.EditInsert(Len+1, y, copy(OldText, 1, Len2));
        inc(OldText, Len2+1);
      end;
      fRedoList.Unlock;
    end

    else
      if not Item.PerformUndo(self) then
        FTheLinesView.EditUndo(Item);
  finally
    FTrimmedLinesView.UndoTrimmedSpaces := False;
    FCaret.DecForcePastEOL;
    Item.Free;
  end;
end;

procedure TCustomSynEdit.SetFoldState(const AValue: String);
begin
  if assigned(fHighlighter) then begin
    fHighlighter.CurrentLines := FTheLinesView;
    if fHighlighter.NeedScan then begin
      FPendingFoldState := AValue;
      exit;
    end;
  end;
  if sfAfterLoadFromFileNeeded in fStateFlags then begin
    FPendingFoldState := AValue;
    exit;
  end;
  FFoldedLinesView.Lock;
  FFoldedLinesView.ApplyFoldDescription(0, 0, -1, -1, PChar(AValue), length(AValue), True);
  TopLine := TopLine; // Todo: reset topline on foldedview
  FFoldedLinesView.UnLock;
  FPendingFoldState := '';
end;

procedure TCustomSynEdit.SetMouseActions(const AValue: TSynEditMouseActions);
begin
  FMouseActions.UserActions := AValue;
end;

procedure TCustomSynEdit.SetMouseSelActions(const AValue: TSynEditMouseActions);
begin
  FMouseSelActions.UserActions := AValue;
end;

procedure TCustomSynEdit.SetMouseTextActions(AValue: TSynEditMouseActions);
begin
  FMouseTextActions.UserActions := AValue;
end;

procedure TCustomSynEdit.SetPaintLockOwner(const AValue: TSynEditBase);
begin
  TSynEditStringList(FLines).PaintLockOwner := AValue;
end;

procedure TCustomSynEdit.SetShareOptions(const AValue: TSynEditorShareOptions);
var
  ChangedOptions: TSynEditorShareOptions;
  OldMarkList: TSynEditMarkList;
  it: TSynEditMarkIterator;
  MListShared: Boolean;
begin
  if FShareOptions = AValue then exit;

  ChangedOptions:=(FShareOptions - AValue) + (AValue - FShareOptions);
  FShareOptions := AValue;

  if (eosShareMarks in ChangedOptions) then begin
    MListShared := IsMarkListShared;
    if ( (FShareOptions * [eosShareMarks] = []) and MListShared ) or
       ( (eosShareMarks  in FShareOptions) and (not MListShared) and
         (TSynEditStringList(FLines).AttachedSynEditCount > 1) )
    then begin
      OldMarkList := FMarkList;
      FMarkList := nil;
      RecreateMarkList;
      it := TSynEditMarkIterator.Create(OldMarkList);
      it.GotoBOL;
      while it.Next do begin
        // Todo: prevent notifications
        if it.Mark.OwnerEdit = Self then
          FMarkList.Add(it.Mark);
      end;
      it.Free;
      FreeAndNil(FMarkList);
    end;
  end;
end;

procedure TCustomSynEdit.ChangeTextBuffer(NewBuffer: TSynEditStringList);
var
  OldBuffer: TSynEditStringList;
  LView: TSynEditStrings;
  i: Integer;
  TempPlugins: TList;
begin
  FLines.SendNotification(senrTextBufferChanging, FLines); // Send the old buffer
  DestroyMarkList;

  // Remember all Plugins; Detach from Lines
  TempPlugins := TList.Create;
  for i := FPlugins.Count - 1 downto 0 do begin
    TempPlugins.Add(FPlugins[i]);
    TSynEditPlugin(FPlugins[i]).Editor := nil;
  end;
  // Detach Highlighter
  if FHighlighter <> nil then
    FHighlighter.DetachFromLines(FLines);

  // Set the New Lines
  OldBuffer := TSynEditStringList(FLines);

  Flines := NewBuffer;
  TSynEditStringList(FLines).AttachSynEdit(Self);
  TSynEditStringsLinked(FTopLinesView).NextLines := FLines;

  // Todo: Todo Refactor all classes with events, so they an be told to re-attach
  NewBuffer.CopyHanlders(OldBuffer, self);
  LView := FTheLinesView;
  while (LView is TSynEditStringsLinked) and (LView <> FLines) do begin
    NewBuffer.CopyHanlders(OldBuffer, LView);
    LView := TSynEditStringsLinked(LView).NextLines;
  end;
  NewBuffer.CopyHanlders(OldBuffer, FFoldedLinesView);
  //NewBuffer.CopyHanlders(OldBuffer, FMarkList);
  NewBuffer.CopyHanlders(OldBuffer, FCaret);
  NewBuffer.CopyHanlders(OldBuffer, FInternalCaret);
  NewBuffer.CopyHanlders(OldBuffer, FBlockSelection);
  NewBuffer.CopyHanlders(OldBuffer, FInternalBlockSelection);
  NewBuffer.CopyHanlders(OldBuffer, fMarkupManager);
  for i := 0 to fMarkupManager.Count - 1 do
    NewBuffer.CopyHanlders(OldBuffer, fMarkupManager.Markup[i]);

  FUndoList := NewBuffer.UndoList;
  FRedoList := NewBuffer.RedoList;

  // Recreate te public access to FLines
  FreeAndNil(FStrings);
  FStrings := TSynEditLines.Create(TSynEditStringList(FLines), {$IFDEF FPC}@{$ENDIF}MarkTextAsSaved);

  // Flines has been set to the new buffer; and self is attached to the new FLines
  // FTheLinesView points to new FLines
  RecreateMarkList;

  // Attach Highlighter
  if FHighlighter <> nil then
    FHighlighter.AttachToLines(FLines);

  // Restore Plugins; Attach to Lines
  for i := 0 to TempPlugins.Count - 1 do
    TSynEditPlugin(TempPlugins[i]).Editor := Self;
  TempPlugins.Free;

  RemoveHandlers(OldBuffer);
  OldBuffer.DetachSynEdit(Self);
  FLines.SendNotification(senrTextBufferChanged, OldBuffer); // Send the old buffer
  OldBuffer.SendNotification(senrTextBufferChanged, OldBuffer); // Send the old buffer
  if OldBuffer.AttachedSynEditCount = 0 then
    OldBuffer.Free;
end;

function TCustomSynEdit.IsMarkListShared: Boolean;
var
  i, j: Integer;
begin
  j := 0;
  i := TSynEditStringList(FLines).AttachedSynEditCount - 1;
  while (i >= 0) and (j <= 1) do begin
    if TCustomSynEdit(TSynEditStringList(FLines).AttachedSynEdits[i]).FMarkList = FMarkList then
      inc(j);
    dec(i);
  end;
  Result := j > 1;
end;

procedure TCustomSynEdit.RecreateMarkList;
var
  s: TSynEditBase;
  i: Integer;
begin
  DestroyMarkList;

  if (TSynEditStringList(FLines).AttachedSynEditCount > 1) and
     (eosShareMarks in FShareOptions)
  then begin
    s := TSynEditStringList(FLines).AttachedSynEdits[0];
    if s = Self then
      s := TSynEditStringList(FLines).AttachedSynEdits[1];
    FMarkList := TCustomSynEdit(s).FMarkList;
    TSynEditMarkListInternal(fMarkList).AddOwnerEdit(Self);
    for i := 0 to 9 do
      FBookMarks[i] := TCustomSynEdit(s).fBookMarks[i];
  end
  else begin
    FMarkList := TSynEditMarkListInternal.Create(self, FTheLinesView);
    for i := 0 to 9 do
      FBookMarks[i] := nil;
  end;

  FMarkList.RegisterChangeHandler({$IFDEF FPC}@{$ENDIF}MarkListChange,
    [low(TSynEditMarkChangeReason)..high(TSynEditMarkChangeReason)]);
end;

procedure TCustomSynEdit.DestroyMarkList;
var
  it: TSynEditMarkIterator;
  s: TSynEditBase;
begin
  if FMarkList = nil then
    exit;

  TSynEditMarkListInternal(fMarkList).RemoveOwnerEdit(Self);
  FMarkList.UnRegisterChangeHandler({$IFDEF FPC}@{$ENDIF}MarkListChange);

  if IsMarkListShared then begin
    s := TSynEditStringList(FLines).AttachedSynEdits[0];
    if s = Self then
      s := TSynEditStringList(FLines).AttachedSynEdits[1]; // TODO: find one that shares the MarkList (if someday partial sharing of Marks is avail)

    if TSynEditMarkListInternal(FMarkList).LinesView = FTheLinesView then
      TSynEditMarkListInternal(FMarkList).LinesView := TCustomSynEdit(s).FTheLinesView;

    it := TSynEditMarkIterator.Create(FMarkList);
    it.GotoBOL;
    while it.Next do begin
      // Todo: prevent notifications
      if it.Mark.OwnerEdit = Self then
        it.Mark.OwnerEdit := s;
    end;
    it.Free;
    FMarkList := nil;
  end
  else
    FreeAndNil(FMarkList);
end;

procedure TCustomSynEdit.ShareTextBufferFrom(AShareEditor: TCustomSynEdit);
begin
  if fPaintLock <> 0 then RaiseGDBException('Cannot change TextBuffer while paintlocked');

  ChangeTextBuffer(TSynEditStringList(AShareEditor.FLines));
end;

procedure TCustomSynEdit.UnShareTextBuffer;
begin
  if fPaintLock <> 0 then RaiseGDBException('Cannot change TextBuffer while paintlocked');
  if TSynEditStringList(FLines).AttachedSynEditCount = 1 then
    exit;

  ChangeTextBuffer(TSynEditStringList.Create);
end;

procedure TCustomSynEdit.RemoveHandlers(ALines: TSynEditStrings = nil);
var
  LView: TSynEditStrings;
  i: Integer;
begin
  if not assigned(ALines) then
    ALines := FLines;

  // Todo: aggregated objects, should be responsible themself
  TSynEditStringList(ALines).RemoveHanlders(self);
  LView := FTheLinesView;
  while (LView is TSynEditStringsLinked) and (LView <> ALines) do begin
    TSynEditStringList(ALines).RemoveHanlders(LView);
    LView := TSynEditStringsLinked(LView).NextLines;
  end;
  TSynEditStringList(ALines).RemoveHanlders(FFoldedLinesView);
  TSynEditStringList(ALines).RemoveHanlders(FCaret);
  TSynEditStringList(ALines).RemoveHanlders(FInternalCaret);
  TSynEditStringList(ALines).RemoveHanlders(FBlockSelection);
  TSynEditStringList(ALines).RemoveHanlders(FInternalBlockSelection);
  TSynEditStringList(ALines).RemoveHanlders(fMarkupManager);
  for i := 0 to fMarkupManager.Count - 1 do
    TSynEditStringList(ALines).RemoveHanlders(fMarkupManager.Markup[i]);
end;

procedure TCustomSynEdit.ExtraLineCharsChanged(Sender: TObject);
begin
  UpdateScrollBars;
end;

procedure TCustomSynEdit.SetTextBetweenPoints(aStartPoint, aEndPoint: TPoint;
  const AValue: String);
begin
  InternalBeginUndoBlock;
  try
    FInternalBlockSelection.SelectionMode := smNormal;
    FInternalBlockSelection.StartLineBytePos := aStartPoint;
    FInternalBlockSelection.EndLineBytePos := aEndPoint;
    FInternalBlockSelection.SelText := AValue;
  finally
    InternalEndUndoBlock;
  end;
end;

procedure TCustomSynEdit.SetTextBetweenPointsEx(aStartPoint, aEndPoint: TPoint;
  aCaretMode: TSynCaretAdjustMode; const AValue: String);
begin
  InternalBeginUndoBlock;
  try
    if aCaretMode = scamAdjust then
      FCaret.IncAutoMoveOnEdit;
    FInternalBlockSelection.SelectionMode := smNormal;
    FInternalBlockSelection.StartLineBytePos := aStartPoint;
    FInternalBlockSelection.EndLineBytePos := aEndPoint;
    if aCaretMode = scamBegin then
      FCaret.LineBytePos := FInternalBlockSelection.StartLineBytePos;
    FInternalBlockSelection.SelText := AValue;
    if aCaretMode = scamEnd then
      FCaret.LineBytePos := FInternalBlockSelection.StartLineBytePos;
  finally
    if aCaretMode = scamAdjust then
      FCaret.DecAutoMoveOnEdit;
    InternalEndUndoBlock;
  end;
end;

procedure TCustomSynEdit.SetVisibleSpecialChars(AValue: TSynVisibleSpecialChars);
begin
  if FVisibleSpecialChars = AValue then Exit;
  FVisibleSpecialChars := AValue;
  fMarkupSpecialChar.VisibleSpecialChars := AValue;
  if eoShowSpecialChars in Options then Invalidate;
end;

function TCustomSynEdit.GetLineState(ALine: Integer): TSynLineState;
begin
  with TSynEditStringList(fLines) do
    if [sfModified, sfSaved] * Flags[ALine] = [sfModified] then
      Result := slsUnsaved
    else
    if [sfModified, sfSaved] * Flags[ALine] = [sfModified, sfSaved] then
      Result := slsSaved
    else
      Result := slsNone;
end;

procedure TCustomSynEdit.ClearBookMark(BookMark: Integer);
begin
  if (BookMark in [0..9]) and assigned(fBookMarks[BookMark]) then
    FBookMarks[BookMark].Free;
end;

procedure TCustomSynEdit.GotoBookMark(BookMark: Integer);
var
  LogCaret: TPoint;
begin
  if (BookMark in [0..9]) and assigned(fBookMarks[BookMark])
    and (fBookMarks[BookMark].Line <= fLines.Count)
  then begin
    LogCaret:=Point(fBookMarks[BookMark].Column, fBookMarks[BookMark].Line);
    DoIncPaintLock(Self); // No editing is taking place
    FCaret.LineBytePos := LogCaret;
    SetBlockEnd(LogCaret);
    SetBlockBegin(LogCaret);
    DoDecPaintLock(Self);
  end;
end;

function TCustomSynEdit.IsLinkable(Y, X1, X2: Integer): Boolean;
begin
  Result := X1 <> X2;
  if Result and Assigned(FOnMouseLink) then
    FOnMouseLink(Self, X1, Y, Result);
end;

procedure TCustomSynEdit.SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
var
  i: Integer;
  mark: TSynEditMark;
begin
  if (BookMark in [0..9]) and (Y >= 1) and (Y <= Max(1, fLines.Count)) then
  begin
    mark := TSynEditMark.Create(self);
    X := PhysicalToLogicalPos(Point(X, Y)).x;
    with mark do begin
      Line := Y;
      Column := X;
      ImageIndex := Bookmark;
      BookmarkNumber := Bookmark;
      Visible := true;
      InternalImage := (fBookMarkOpt.BookmarkImages = nil);
    end;
    for i := 0 to 9 do
      if assigned(fBookMarks[i]) and (fBookMarks[i].Line = Y) then
        ClearBookmark(i);
    if assigned(fBookMarks[BookMark]) then
      ClearBookmark(BookMark);
    FMarkList.Add(mark);
  end;
end;

procedure TCustomSynEdit.WndProc(var Msg: TMessage);
// Prevent Alt-Backspace from beeping
const
  ALT_KEY_DOWN = $20000000;
begin
  if (Msg.Msg = WM_SYSCHAR) and (Msg.wParam = VK_BACK) and
    (Msg.lParam and ALT_KEY_DOWN <> 0)
  then
    Msg.Msg := 0
  else
    inherited;
end;

procedure TCustomSynEdit.InsertTextAtCaret(aText: String; aCaretMode : TSynCaretAdjustMode = scamEnd);
begin
  TextBetweenPointsEx[FCaret.LineBytePos, FCaret.LineBytePos, aCaretMode] := aText;
end;

procedure TCustomSynEdit.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited;
  LastMouseCaret:=Point(-1,-1);
  if (Source is TCustomSynEdit) and not TCustomSynEdit(Source).ReadOnly then
  begin
    Accept := True;
    //Ctrl is pressed => change cursor to indicate copy instead of move
    if GetKeyState(VK_CONTROL) < 0 then
      DragCursor := crMultiDrag
    else
      DragCursor := crDrag;
    FBlockSelection.IncPersistentLock;
    if State = dsDragLeave then //restore prev caret position
      ComputeCaret(FMouseDownX, FMouseDownY)
    else //position caret under the mouse cursor
      ComputeCaret(X, Y);
    FBlockSelection.DecPersistentLock;
  end;
end;

procedure TCustomSynEdit.DragDrop(Source: TObject; X, Y: Integer);
var
  NewCaret: TPoint;
  DoDrop, DropAfter, DropMove: boolean;
  BB, BE: TPoint;
  DragDropText: string;
  Adjust: integer;
  FoldInfo: String;
  BlockSel: TSynEditSelection;
begin
  if not ReadOnly  and (Source is TCustomSynEdit)
    and TCustomSynEdit(Source).SelAvail
  then begin
    IncPaintLock;
    try
      inherited;
      ComputeCaret(X, Y);
      NewCaret := CaretXY;
      // if from other control then move when SHIFT, else copy
      // if from Self then copy when CTRL, else move
      if Source <> Self then begin
        DropMove := GetKeyState(VK_SHIFT) < 0;
        DoDrop := TRUE;
        DropAfter := FALSE;
      end else begin
        DropMove := GetKeyState(VK_CONTROL) >= 0;
        BB := BlockBegin;
        BE := BlockEnd;
        DropAfter := (NewCaret.Y > BE.Y)
          or ((NewCaret.Y = BE.Y) and (NewCaret.X > BE.X));
        DoDrop := DropAfter or (NewCaret.Y < BB.Y)
          or ((NewCaret.Y = BB.Y) and (NewCaret.X < BB.X));
      end;
      if DoDrop then begin
        InternalBeginUndoBlock;                                                         //mh 2000-11-20
        try
          DragDropText := TCustomSynEdit(Source).SelText;
          BlockSel := TCustomSynEdit(Source).FBlockSelection;
          if eoFoldedCopyPaste in fOptions2 then
            FoldInfo :=  TCustomSynEdit(Source).FFoldedLinesView.GetFoldDescription(
                  BlockSel.FirstLineBytePos.Y - 1, BlockSel.FirstLineBytePos.X,
                  BlockSel.LastLineBytePos.Y - 1,  BlockSel.LastLineBytePos.X);
          // delete the selected text if necessary
          if DropMove then begin
            if Source <> Self then
              TCustomSynEdit(Source).SelText := ''
            else begin
              SetSelTextExternal('');
              // adjust horizontal drop position
              if DropAfter and (NewCaret.Y = BE.Y) then begin
                if BB.Y = BE.Y then
                  Adjust := BE.X - BB.X
                else
                  Adjust := BE.X - 1;
                Dec(NewCaret.X, Adjust);
              end;
              // adjust vertical drop position
              if DropAfter and (BE.Y > BB.Y) then
                Dec(NewCaret.Y, BE.Y - BB.Y);
            end;
          end;
          // insert the selected text
          FCaret.IncForcePastEOL;
          try
            CaretXY := NewCaret;
            BlockBegin := NewCaret;
            SetSelTextPrimitive(smNormal, PChar(DragDropText), true);
            if FoldInfo <> '' then begin
              ScanRanges;
              FFoldedLinesView.ApplyFoldDescription(NewCaret.Y -1, NewCaret.X,
                    FBlockSelection.StartLinePos-1, FBlockSelection.StartBytePos,
                    PChar(FoldInfo), length(FoldInfo));
            end;
          finally
            FCaret.DecForcePastEOL;
          end;
          FCaret.LineCharPos := NewCaret;
          BlockBegin := PhysicalToLogicalPos(NewCaret);
          BlockEnd := LogicalCaretXY;
        finally
          InternalEndUndoBlock;
        end;
      end;
    finally
      DecPaintLock;
    end;
  end else
    inherited;
end;

procedure TCustomSynEdit.SetRightEdge(Value: Integer);
begin
  if fRightEdge <> Value then begin
    fRightEdge := Value;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetRightEdgeColor(Value: TColor);
var
  nX: integer;
  rcInval: TRect;
begin
  if fRightEdgeColor <> Value then begin
    fRightEdgeColor := Value;
    if HandleAllocated then begin
      nX := fTextOffset + fRightEdge * fCharWidth;
      rcInval := Rect(nX - 1, 0, nX + 1, ClientHeight-ScrollBarWidth);
      {$IFDEF VerboseSynEditInvalidate}
      DebugLn(['TCustomSynEdit.SetRightEdgeColor ',dbgs(rcInval)]);
      {$ENDIF}
      InvalidateRect(Handle, @rcInval, FALSE);
    end;
  end;
end;

function TCustomSynEdit.GetMaxUndo: Integer;
begin
  result := fUndoList.MaxUndoActions;
end;

procedure TCustomSynEdit.SetMaxUndo(const Value: Integer);
begin
  if Value > -1 then begin
    fUndoList.MaxUndoActions := Value;
    fRedoList.MaxUndoActions := Value;
  end;
end;

procedure TCustomSynEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then begin
    if AComponent = fHighlighter then begin
      fHighlighter.DetachFromLines(FLines);
      fHighlighter := nil;
      fMarkupHighCaret.Highlighter := nil;
      fMarkupWordGroup.Highlighter := nil;
      FFoldedLinesView.Highlighter := nil;
{begin}                                                                         //mh 2000-10-01
      if not (csDestroying in ComponentState) then begin
        RecalcCharExtent;
        SizeOrFontChanged(TRUE);                                                //jr 2000-10-01
        Invalidate;
      end;
{end}                                                                           //mh 2000-10-01
    end;
    if (fBookmarkOpt <> nil) then
      if (AComponent = fBookmarkOpt.BookmarkImages) then begin
        fBookmarkOpt.BookmarkImages := nil;
        InvalidateGutterLines(-1, -1);
      end;
  end;
end;

procedure TCustomSynEdit.RemoveHooksFromHighlighter;
begin
  if not Assigned(fHighlighter) then
    exit;
  fHighlighter.UnhookAttrChangeEvent({$IFDEF FPC}@{$ENDIF}HighlighterAttrChanged);
  fHighlighter.DetachFromLines(FLines);
  fHighlighter.RemoveFreeNotification(self);
end;

procedure TCustomSynEdit.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  if Value <> fHighlighter then begin
    FPendingFoldState := '';
    RemoveHooksFromHighlighter;
    if Assigned(Value) then begin
      Value.HookAttrChangeEvent(
        {$IFDEF FPC}@{$ENDIF}HighlighterAttrChanged);
      Value.FreeNotification(Self);
      Value.AttachToLines(FLines);
    end;
    fHighlighter := Value;
    IncPaintLock;
    try
      // Ensure to free all copies in SynEit.Notification too
      fMarkupHighCaret.Highlighter := Value;
      fMarkupWordGroup.Highlighter := Value;
      FFoldedLinesView.Highlighter := Value;
      FWordBreaker.Reset;
      if fHighlighter<>nil then begin
        fTSearch.IdentChars := fHighlighter.IdentChars;
        FWordBreaker.IdentChars     := fHighlighter.IdentChars;
        FWordBreaker.WordBreakChars := fHighlighter.WordBreakChars;
      end else begin
        fTSearch.ResetIdentChars;
      end;
      RecalcCharExtent;
      ScanRanges; // Todo: Skip if paintlocked
    finally
      DecPaintLock;
    end;
    SizeOrFontChanged(TRUE);
  end;
end;

procedure TCustomSynEdit.SetHideSelection(const Value: boolean);
begin
  if fHideSelection <> Value then begin
    FHideSelection := Value;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetInsertMode(const Value: boolean);
begin
  if fInserting <> Value then begin
    fInserting := Value;
    if InsertMode then
      FScreenCaret.DisplayType := FInsertCaret
    else
      FScreenCaret.DisplayType := FOverwriteCaret;
    StatusChanged([scInsertMode]);
  end;
end;

procedure TCustomSynEdit.SetInsertCaret(const Value: TSynEditCaretType);
begin
  if FInsertCaret <> Value then begin
    FInsertCaret := Value;
    if InsertMode then
      FScreenCaret.DisplayType := fInsertCaret;
  end;
end;

procedure TCustomSynEdit.SetOverwriteCaret(const Value: TSynEditCaretType);
begin
  if FOverwriteCaret <> Value then begin
    FOverwriteCaret := Value;
    if not InsertMode then
      FScreenCaret.DisplayType := fOverwriteCaret;
  end;
end;

procedure TCustomSynEdit.SetMaxLeftChar(Value: integer);
begin
  Value := MinMax(Value, 1, MAX_SCROLL); // horz scrolling is only 16 bit
  if fMaxLeftChar <> Value then begin
    fMaxLeftChar := Value;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.EnsureCursorPosVisible;
var
  PhysCaretXY: TPoint;
  MinX: Integer;
  MaxX: Integer;
  PhysBlockBeginXY: TPoint;
  PhysBlockEndXY: TPoint;
begin
  if (PaintLockOwner <> nil) and (PaintLockOwner <> Self) and
     (not (eoAlwaysVisibleCaret in fOptions2))
  then
    exit;

  if (not HandleAllocated) or (fPaintLock > 0) or
     (FWinControlFlags * [wcfInitializing, wcfCreatingHandle] <> [])
  then begin
    include(fStateFlags, sfEnsureCursorPos);
    exit;
  end;

  exclude(fStateFlags, sfEnsureCursorPos);
  DoIncPaintLock(Self); // No editing is taking place
  try
    // Make sure X is visible
    //DebugLn('[TCustomSynEdit.EnsureCursorPosVisible] A CaretX=',CaretX,' LeftChar=',LeftChar,' CharsInWindow=',CharsInWindow,' ClientWidth=',ClientWidth);
    PhysCaretXY:=CaretXY;
    // try to make the current selection visible as well
    MinX:=PhysCaretXY.X;
    MaxX:=PhysCaretXY.X;
    if SelAvail then begin
      PhysBlockBeginXY:=LogicalToPhysicalPos(BlockBegin);
      PhysBlockEndXY:=LogicalToPhysicalPos(BlockEnd);
      if (PhysBlockBeginXY.X<>PhysBlockEndXY.X)
      or (PhysBlockBeginXY.Y<>PhysBlockEndXY.Y) then begin
        if (FBlockSelection.ActiveSelectionMode <> smColumn) and
           (PhysBlockBeginXY.Y<>PhysBlockEndXY.Y) then
          PhysBlockBeginXY.X:=1;
        if MinX>PhysBlockBeginXY.X then
          MinX:=Max(PhysBlockBeginXY.X,PhysCaretXY.X-CharsInWindow+1);
        if MinX>PhysBlockEndXY.X then
          MinX:=Max(PhysBlockEndXY.X,PhysCaretXY.X-CharsInWindow+1);
        if MaxX<PhysBlockBeginXY.X then
          MaxX:=Min(PhysBlockBeginXY.X,MinX+CharsInWindow-1);
        if MaxX<PhysBlockEndXY.X then
          MaxX:=Min(PhysBlockEndXY.X,MinX+CharsInWindow-1);
      end;
    end;
    {DebugLn('TCustomSynEdit.EnsureCursorPosVisible A CaretX=',dbgs(PhysCaretXY.X),
      ' BlockX=',dbgs(PhysBlockBeginXY.X)+'-'+dbgs(PhysBlockEndXY.X),
      ' CharsInWindow='+dbgs(CharsInWindow), MinX='+dbgs(MinX),' MaxX='+dbgs(MaxX),
      ' LeftChar='+dbgs(LeftChar), '');}
    if MinX < LeftChar then
      LeftChar := MinX
    else if LeftChar < MaxX - (CharsInWindow - 1 - FScreenCaret.ExtraLineChars) then
      LeftChar := MaxX - (CharsInWindow - 1 - FScreenCaret.ExtraLineChars)
    else
      LeftChar := LeftChar;                                                     //mh 2000-10-19
    //DebugLn(['TCustomSynEdit.EnsureCursorPosVisible B LeftChar=',LeftChar,' MinX=',MinX,' MaxX=',MaxX,' CharsInWindow=',CharsInWindow]);
    // Make sure Y is visible
    if CaretY < TopLine then
      TopLine := CaretY
    else if CaretY > ScreenRowToRow(Max(1, LinesInWindow) - 1) then             //mh 2000-10-19
      TopLine := FFoldedLinesView.TextPosAddLines(CaretY, -Max(0, LinesInWindow-1))
    else
      TopLine := TopLine;                                                       //mh 2000-10-19
  finally
    DoDecPaintLock(Self);
  end;
end;

procedure TCustomSynEdit.SetKeystrokes(const Value: TSynEditKeyStrokes);
begin
  if Value = nil then
    FKeystrokes.Clear
  else
    FKeystrokes.Assign(Value);
end;

procedure TCustomSynEdit.SetExtraCharSpacing(const Value: integer);
begin
  if fExtraCharSpacing=Value then exit;
  fExtraCharSpacing := Value;
  FontChanged(self);
end;

procedure TCustomSynEdit.SetLastMouseCaret(const AValue: TPoint);
begin
  if (FLastMouseCaret.X=AValue.X) and (FLastMouseCaret.Y=AValue.Y) then exit;
  FLastMouseCaret:=AValue;
  if assigned(fMarkupCtrlMouse) then
    fMarkupCtrlMouse.LastMouseCaret := AValue;
  UpdateCursor;
end;

procedure TCustomSynEdit.SetDefaultKeystrokes;
begin
  FKeystrokes.ResetDefaults;
end;

procedure TCustomSynEdit.ResetMouseActions;
var
  i: Integer;
begin
  FMouseActions.Options := FMouseOptions;
  FMouseActions.ResetUserActions;
  FMouseSelActions.Options := FMouseOptions;
  FMouseSelActions.ResetUserActions;
  FMouseTextActions.Options := FMouseOptions;
  FMouseTextActions.ResetUserActions;

  FLeftGutter.ResetMouseActions;
  FRightGutter.ResetMouseActions;
end;

procedure TCustomSynEdit.CommandProcessor(Command: TSynEditorCommand;
  AChar: TUTF8Char;
  Data: pointer);
var
  InitialCmd: TSynEditorCommand;
begin
  {$IFDEF VerboseKeys}
  DebugLn(['[TCustomSynEdit.CommandProcessor] ',Command
    ,' AChar=',AChar,' Data=',DbgS(Data)]);
  {$ENDIF}
  // first the program event handler gets a chance to process the command
  InitialCmd := Command;
  DoOnProcessCommand(Command, AChar, Data);
  if Command <> ecNone then begin
    try
      InternalBeginUndoBlock;
      FBeautifyStartLineIdx := -1;
      FBeautifyEndLineIdx := -1;
      if assigned(FBeautifier) then begin
        FBeautifier.AutoIndent := (eoAutoIndent in FOptions);
        FBeautifier.BeforeCommand(self, FTheLinesView, FCaret, Command, InitialCmd);
      end;
      // notify hooked command handlers before the command is executed inside of
      // the class
      if Command <> ecNone then
        NotifyHookedCommandHandlers(FALSE, Command, AChar, Data);
      // internal command handler
      if (Command <> ecNone) and (Command < ecUserFirst) then
        ExecuteCommand(Command, AChar, Data);
      // notify hooked command handlers after the command was executed inside of
      // the class
      if Command <> ecNone then
        NotifyHookedCommandHandlers(TRUE, Command, AChar, Data);
      if Command <> ecNone then
        DoOnCommandProcessed(Command, AChar, Data);

      if assigned(FBeautifier) then begin
        tsyneditstringlist(FLines).FlushNotificationCache;
        FBeautifier.AutoIndent := (eoAutoIndent in FOptions);
        FBeautifier.AfterCommand(self, FTheLinesView, FCaret, Command, InitialCmd,
                                 FBeautifyStartLineIdx+1, FBeautifyEndLineIdx+1);
      end;
    finally
      InternalEndUndoBlock;
    end;
  end;
end;

procedure TCustomSynEdit.ExecuteCommand(Command: TSynEditorCommand;
  const AChar: TUTF8Char; Data: pointer);
const
  SEL_MODE: array[ecNormalSelect..ecLineSelect] of TSynSelectionMode = (
    smNormal, smColumn, smLine);
var
  CX: Integer;
  Len: Integer;
  Temp: string;
  Helper: string;
  moveBkm: boolean;
  WP: TPoint;
  Caret: TPoint;
  CaretNew: TPoint;
{$IFDEF SYN_MBCSSUPPORT}
  StartOfBlock: TPoint;
  i: integer;
  s: string;
{$ENDIF}
  counter: Integer;
  LogCounter: integer;
  LogCaretXY: TPoint;
  CY: Integer;

begin
  IncPaintLock;
  try
    fUndoList.CurrentReason := Command;

    if Command in [ecSelColCmdRangeStart..ecSelColCmdRangeEnd] then
      FBlockSelection.ActiveSelectionMode := smColumn;
    if Command in [ecSelCmdRangeStart..ecSelCmdRangeEnd] then
      FBlockSelection.ActiveSelectionMode := FBlockSelection.SelectionMode;

    FBlockSelection.AutoExtend := Command in [ecSelectionStart..ecSelectionEnd];
    if Command in [ecSelectionStart..ecSelectionEnd] then
      AquirePrimarySelection;

    FCaret.ChangeOnTouch;

    case Command of
// horizontal caret movement or selection
      ecLeft, ecSelLeft, ecColSelLeft:
        begin
          if (eoCaretSkipsSelection in Options2) and (Command=ecLeft)
          and SelAvail and FCaret.IsAtLineByte(FBlockSelection.LastLineBytePos) then begin
            FBlockSelection.IgnoreNextCaretMove;
            FCaret.LineBytePos := FBlockSelection.FirstLineBytePos;
          end
          else
            MoveCaretHorz(-1);
        end;
      ecRight, ecSelRight, ecColSelRight:
        begin
          if (eoCaretSkipsSelection in Options2) and (Command=ecRight)
          and SelAvail and FCaret.IsAtLineByte(FBlockSelection.FirstLineBytePos) then begin
            FBlockSelection.IgnoreNextCaretMove;
            FCaret.LineBytePos := FBlockSelection.LastLineBytePos;
          end
          else
            MoveCaretHorz(1);
        end;
      ecPageLeft, ecSelPageLeft, ecColSelPageLeft:
        begin
          MoveCaretHorz(-CharsInWindow);
        end;
      ecPageRight, ecSelPageRight, ecColSelPageRight:
        begin
          MoveCaretHorz(CharsInWindow);
        end;
      ecLineStart, ecSelLineStart, ecColSelLineStart:
        begin
          DoHomeKey;
        end;
      ecLineTextStart, ecSelLineTextStart, ecColSelLineTextStart:
        begin
          DoHomeKey(synhmFirstWord);
        end;
      ecLineEnd, ecSelLineEnd, ecColSelLineEnd:
        begin
          DoEndKey;
        end;
// vertical caret movement or selection
      ecUp, ecSelUp, ecColSelUp:
        begin
          MoveCaretVert(-1);
        end;
      ecDown, ecSelDown, ecColSelDown:
        begin
          MoveCaretVert(1);
        end;
      ecPageUp, ecSelPageUp, ecPageDown, ecSelPageDown, ecColSelPageUp, ecColSelPageDown:
        begin
          counter := fLinesInWindow;
          if (eoHalfPageScroll in fOptions) then counter:=counter div 2;
          if eoScrollByOneLess in fOptions then
            Dec(counter);
          counter := Max(1, counter);
          if (Command in [ecPageUp, ecSelPageUp, ecColSelPageUp]) then
            counter := -counter;
          TopView := TopView + counter;
          MoveCaretVert(counter);
        end;
      ecPageTop, ecSelPageTop, ecColSelPageTop:
        begin
          FCaret.LinePos := TopLine;
        end;
      ecPageBottom, ecSelPageBottom, ecColSelPageBottom:
        begin
          FCaret.LinePos := ScreenRowToRow(LinesInWindow - 1);
        end;
      ecEditorTop, ecSelEditorTop:
        begin
          FCaret.LineCharPos := Point(1, FFoldedLinesView.ViewPosToTextIndex(1)+1);
        end;
      ecEditorBottom, ecSelEditorBottom:
        begin
          CaretNew := Point(1, FFoldedLinesView.ViewPosToTextIndex(FFoldedLinesView.Count)+1);
          if (CaretNew.Y > 0) then
            CaretNew.X := Length(FTheLinesView[CaretNew.Y - 1]) + 1;
          FCaret.LineCharPos := CaretNew;
        end;
      ecColSelEditorTop:
        begin
          FCaret.LinePos := FFoldedLinesView.ViewPosToTextIndex(1)+1;
        end;
      ecColSelEditorBottom:
        begin
          FCaret.LinePos := FFoldedLinesView.ViewPosToTextIndex(FFoldedLinesView.Count)+1;
        end;
        
// goto special line / column position
      ecGotoXY, ecSelGotoXY:
        if Assigned(Data) then begin
          FCaret.LineCharPos := PPoint(Data)^;
        end;
// word selection
      ecWordLeft, ecSelWordLeft, ecColSelWordLeft:
        begin
          CaretNew := PrevWordLogicalPos;
          if FFoldedLinesView.FoldedAtTextIndex[CaretNew.Y - 1] then begin
            CY := FindNextUnfoldedLine(CaretNew.Y, False);
            CaretNew := Point(1 + Length(FTheLinesView[CY-1]), CY);
          end;
          FCaret.LineBytePos := CaretNew;
        end;
      ecWordRight, ecSelWordRight, ecColSelWordRight:
        begin
          CaretNew := NextWordLogicalPos;
          if FFoldedLinesView.FoldedAtTextIndex[CaretNew.Y - 1] then
            CaretNew := Point(1, FindNextUnfoldedLine(CaretNew.Y, True));
          FCaret.LineBytePos := CaretNew;
        end;
      ecSelectAll:
        begin
          SelectAll;
        end;
{begin}                                                                         //mh 2000-10-30
      ecDeleteLastChar:
        if not ReadOnly then begin
          if SelAvail and (not FBlockSelection.Persistent) and (eoOverwriteBlock in fOptions2) then
            SetSelTextExternal('')
          else begin
            Temp := LineText;
            Len := Length(Temp);
            LogCaretXY:=LogicalCaretXY;
            Caret := CaretXY;
            //debugln('ecDeleteLastChar B Temp="',DbgStr(Temp),'" CaretX=',dbgs(CaretX),' LogCaretXY=',dbgs(LogCaretXY));
            if LogCaretXY.X > Len +1
            then begin
              // past EOL; only move caret one column
              FCaret.IncForcePastEOL;
              CaretX := CaretX - 1;
              FCaret.DecForcePastEOL;
            end else if CaretX = 1 then begin
              // join this line with the last line if possible
              if CaretY > 1 then begin
                CaretY := CaretY - 1;
                CaretX := PhysicalLineLength(FTheLinesView[CaretY - 1],
                                             CaretY - 1) + 1;
                FTheLinesView.EditLineJoin(CaretY);
              end;
            end else begin
                // delete char
              {$IFDEF USE_UTF8BIDI_LCL}
              CaretX := CaretX - 1;
              FTheLinesView.EditDelete(CaretX, LogCaretXY.Y, 1);
              {$ELSE USE_UTF8BIDI_LCL}
              LogCaretXY.X:=PhysicalToLogicalCol(Temp, CaretY-1, CaretX - 1);
              LogCounter:=GetCharLen(Temp,LogCaretXY.X);
              CaretX := LogicalToPhysicalCol(Temp, CaretY-1, LogCaretXY.X);
              FTheLinesView.EditDelete(FCaret.BytePos, LogCaretXY.Y, LogCounter);
              {$ENDIF USE_UTF8BIDI_LCL}
              //end;
            end;

          end;
        end;
      ecDeleteChar:
        if not ReadOnly then begin
          if SelAvail and (not FBlockSelection.Persistent) and (eoOverwriteBlock in fOptions2) then
            SetSelTextExternal('')
          else begin
            Temp := LineText;
            Len := Length(Temp);
            LogCaretXY:=LogicalCaretXY;
            if LogCaretXY.X <= Len then
            begin
              // delete char
              Counter:=GetCharLen(Temp,LogCaretXY.X);
              FTheLinesView.EditDelete(LogCaretXY.X, CaretY, Counter);
              SetLogicalCaretXY(LogCaretXY);
            end else begin
              // join line with the line after
              if CaretY < FTheLinesView.Count then begin
                Helper := StringOfChar(' ', LogCaretXY.X - 1 - Len);
                FTheLinesView.EditLineJoin(CaretY, Helper);
              end;
            end;
          end;
        end;
      ecDeleteWord, ecDeleteEOL:
        if not ReadOnly then begin
          Len := LogicalToPhysicalCol(LineText, CaretY-1,Length(LineText)+1)-1;
          Helper := '';
          Caret := CaretXY;
          if Command = ecDeleteWord then begin
            if CaretX > Len + 1 then begin
              Helper := StringOfChar(' ', CaretX - 1 - Len);
              CaretX := 1 + Len;
            end;
            WP := NextWordLogicalPos(True);
          end else
            WP := Point(Len + 1, CaretY);
          if (WP.X <> FCaret.BytePos) or (WP.Y <> FCaret.LinePos) then begin
            FInternalBlockSelection.StartLineBytePos := WP;
            FInternalBlockSelection.EndLineBytePos := LogicalCaretXY;
            FInternalBlockSelection.ActiveSelectionMode := smNormal;
            FInternalBlockSelection.SetSelTextPrimitive(smNormal, nil);
            if Helper <> '' then
              FTabbedLinesView.EditInsert(CaretX, CaretY, Helper);
            FCaret.BytePos := FInternalBlockSelection.StartBytePos + length(Helper);
          end;
        end;
      ecDeleteLastWord, ecDeleteBOL:
        if not ReadOnly then begin
          if Command = ecDeleteLastWord then
            WP := PrevWordLogicalPos
          else
            WP := Point(1, CaretY);
          if (WP.X <> FCaret.BytePos) or (WP.Y <> FCaret.LinePos) then begin
            FInternalBlockSelection.StartLineBytePos := WP;
            FInternalBlockSelection.EndLineBytePos := LogicalCaretXY;
            FInternalBlockSelection.ActiveSelectionMode := smNormal;
            FInternalBlockSelection.SetSelTextPrimitive(smNormal, nil);
            FCaret.LineBytePos := WP;
          end;
        end;
      ecDeleteLine:
        if not ReadOnly
        then begin
          CY := FCaret.LinePos;
          if (Cy < FTheLinesView.Count) then
            FTheLinesView.EditLinesDelete(CaretY, 1)
          else
          if (Cy = FTheLinesView.Count) and (FTheLinesView[CY-1] <> '') then
            FTheLinesView.EditDelete(1, Cy, length(FTheLinesView[Cy-1]));
          CaretXY := Point(1, CaretY); // like seen in the Delphi editor
        end;
      ecClearAll:
        begin
          if not ReadOnly then ClearAll;
        end;
      ecInsertLine,
      ecLineBreak:
        if not ReadOnly then begin
          if SelAvail and (not FBlockSelection.Persistent) and (eoOverwriteBlock in fOptions2) then
            SetSelTextExternal('');
          Temp := LineText;
          LogCaretXY:=LogicalCaretXY;
          Len := Length(Temp);
          if LogCaretXY.X > Len + 1 then
            LogCaretXY.X := Len + 1;
          FTheLinesView.EditLineBreak(LogCaretXY.X, LogCaretXY.Y);
          if Command = ecLineBreak then
            CaretXY := Point(1, CaretY + 1)
          else
            CaretXY := CaretXY;
        end;
      ecTab:
        if not ReadOnly then
        try
          FCaret.IncForcePastEOL;
          DoTabKey;
        finally
          FCaret.DecForcePastEOL;
        end;
      ecShiftTab:
        if not ReadOnly then
          if SelAvail and (eoTabIndent in Options) then
            DoBlockUnindent;
      ecMatchBracket:
        FindMatchingBracket;
      ecChar:
        if not ReadOnly and (AChar >= #32) and (AChar <> #127) then begin
          if SelAvail and (not FBlockSelection.Persistent) and (eoOverwriteBlock in fOptions2) then begin
            SetSelTextExternal(AChar);
          end else begin
            try
              FCaret.IncForcePastEOL;
              FCaret.IncForceAdjustToNextChar;

              LogCaretXY := FCaret.LineBytePos;
              Temp := LineText;
              Len := Length(Temp);
              if (not fInserting) and (LogCaretXY.X - 1 < Len) then begin
                counter := GetCharLen(Temp,LogCaretXY.X);
                FTheLinesView.EditDelete(LogCaretXY.X, LogCaretXY.Y, counter);
                Len := Len - counter;
              end;

              {$IFDEF USE_UTF8BIDI_LCL}
              // TODO: improve utf8bidi for tabs
              Len := VLength(LineText, drLTR);
              (*if Len < CaretX then
                Temp := StringOfChar(' ', CaretX - Len)
              else
                Temp := '' *)
              FTheLinesView.EditInsert(CaretX, LogCaretXY.Y, (*Temp +*) AChar);
              {$ELSE}
              (*if Len < LogCaretXY.X - 1 then begin
                Temp := StringOfChar(' ', LogCaretXY.X - 1 - Len);
                LogCaretXY.X := Len + 1;
              end
              else
                temp := '';*)
              FTheLinesView.EditInsert(LogCaretXY.X, LogCaretXY.Y, (*Temp +*) AChar);
              {$ENDIF}

              CaretX := CaretX + 1;
              if CaretX >= LeftChar + fCharsInWindow then
                LeftChar := LeftChar + Min(25, fCharsInWindow - 1);
            finally
              FCaret.DecForceAdjustToNextChar;
              FCaret.DecForcePastEOL;
            end;
          end;
        end
        else if not ReadOnly and (AChar = #13) then begin
          // ecLineBreak is not assigned
          // Insert a linebreak, but do not apply any other functionality (such as indent)
          if SelAvail and (not FBlockSelection.Persistent) and (eoOverwriteBlock in fOptions2) then
            SetSelTextExternal('');
          LogCaretXY:=LogicalCaretXY;
          FTheLinesView.EditLineBreak(LogCaretXY.X, LogCaretXY.Y);
          CaretXY := Point(1, CaretY + 1);
          EnsureCursorPosVisible;
        end;
      ecUndo:
        begin
          if not ReadOnly then Undo;
        end;
      ecRedo:
        begin
          if not ReadOnly then Redo;
        end;
      ecGotoMarker0..ecGotoMarker9:
        begin
          if BookMarkOptions.EnableKeys then
            GotoBookMark(Command - ecGotoMarker0);
        end;
      ecSetMarker0..ecSetMarker9,ecToggleMarker0..ecToggleMarker9:
        begin
          if BookMarkOptions.EnableKeys then begin
            if (Command >= ecSetMarker0) and (Command <= ecSetMarker9) then
              CX := Command - ecSetMarker0
            else
              CX := Command - ecToggleMarker0;
            if assigned(fBookMarks[CX]) then begin
              moveBkm := ((Command >= ecSetMarker0) and (Command <= ecSetMarker9))
                         or (fBookMarks[CX].Line <> CaretY);
              ClearBookMark(CX);
              if moveBkm then
                SetBookMark(CX, CaretX, CaretY);
            end else
              SetBookMark(CX, CaretX, CaretY);
          end; // if BookMarkOptions.EnableKeys
        end;
      ecCut:
        begin
          if (not ReadOnly) and SelAvail then
            CutToClipboard;
        end;
      ecCopy:
        begin
          CopyToClipboard;
        end;
      ecPaste:
        begin
          if not ReadOnly then PasteFromClipboard;
        end;
      ecScrollUp:
        begin
          TopView := TopView - 1;
          if CaretY > ScreenRowToRow(LinesInWindow-1) then
            CaretY := ScreenRowToRow(LinesInWindow-1);
        end;
      ecScrollDown:
        begin
          TopView := TopView + 1;
          if CaretY < TopLine then
            CaretY := TopLine;
        end;
      ecScrollLeft:
        begin
          LeftChar := LeftChar - 1;
          if CaretX > LeftChar + CharsInWindow then
            CaretX := LeftChar + CharsInWindow;
          Update;
        end;
      ecScrollRight:
        begin
          LeftChar := LeftChar + 1;
          if CaretX < LeftChar then
            CaretX := LeftChar;
          Update;
        end;
      ecInsertMode:
        begin
          InsertMode := TRUE;
        end;
      ecOverwriteMode:
        begin
          InsertMode := FALSE;
        end;
      ecToggleMode:
        begin
          InsertMode := not InsertMode;
        end;
      ecBlockSetBegin:
        begin
          FBlockSelection.Hide :=
            CompareCarets(FCaret.LineBytePos, FBlockSelection.EndLineBytePos) <= 0;
          FBlockSelection.StartLineBytePosAdjusted := FCaret.LineBytePos;
        end;
      ecBlockSetEnd:
        begin
          FBlockSelection.Hide :=
            CompareCarets(FCaret.LineBytePos, FBlockSelection.StartLineBytePos) >= 0;
          FBlockSelection.EndLineBytePos := FCaret.LineBytePos;
        end;
      ecBlockToggleHide:
        begin
          FBlockSelection.Hide := not FBlockSelection.Hide;
        end;
      ecBlockHide:
        begin
          FBlockSelection.Hide := True;
        end;
      ecBlockShow:
        begin
          FBlockSelection.Hide := False;
        end;
      ecBlockMove:
        begin
          if SelAvail then begin
            helper := FBlockSelection.SelText;
            FInternalBlockSelection.AssignFrom(FBlockSelection);
            FBlockSelection.IncPersistentLock;
            FBlockSelection.StartLineBytePos := FCaret.LineBytePos;             // Track the Adjustment of the insert position
            FInternalBlockSelection.SelText := '';
            FCaret.LineBytePos := FBlockSelection.StartLineBytePos;
            Caret := FCaret.LineBytePos;
            FBlockSelection.SelText := Helper;
            FBlockSelection.DecPersistentLock;
            CaretNew := FCaret.LineBytePos;
            FBlockSelection.StartLineBytePos := Caret;
            FBlockSelection.EndLineBytePos := CaretNew;
          end;
        end;
      ecBlockCopy:
        begin
          if SelAvail then
            InsertTextAtCaret(FBlockSelection.SelText, scamEnd);
        end;
      ecBlockDelete:
        begin
          if SelAvail then
            FBlockSelection.SelText := '';
        end;
      ecBlockGotoBegin:
        begin
          FCaret.LineBytePos := FBlockSelection.FirstLineBytePos;
        end;
      ecBlockGotoEnd:
        begin
          FCaret.LineBytePos := FBlockSelection.LastLineBytePos;
        end;

      ecBlockIndent:
        if not ReadOnly then DoBlockIndent;
      ecBlockUnindent:
        if not ReadOnly then DoBlockUnindent;
      ecNormalSelect,
      ecColumnSelect,
      ecLineSelect:
        begin
          DefaultSelectionMode := SEL_MODE[Command];
        end;
{$IFDEF SYN_MBCSSUPPORT}
      ecImeStr:
        if not ReadOnly then begin
          SetString(s, PChar(Data), StrLen(Data));
          if SelAvail then begin
            SetSelTextExternal(s);
          end else begin
            Temp := LineText;
            Len := Length(Temp);
            if Len < CaretX then
              Temp := Temp + StringOfChar(' ', CaretX - Len);
            try
              FCaret.IncForcePastEOL;
              StartOfBlock := CaretXY;
// Processing of case character covers on LeadByte.
              Len := Length(s);
              if not fInserting then begin
                i := (CaretX + Len);
                if (ByteType(Temp, i) = mbTrailByte) then begin
                  s := s + Temp[i - 1];
                  Helper := Copy(Temp, CaretX, Len - 1);
                end else
                  Helper := Copy(Temp, CaretX, Len);
                Delete(Temp, CaretX, Len);
              end;
              Insert(s, Temp, CaretX);
              CaretX := (CaretX + Len);
              FTheLinesView[CaretY - 1] := Temp;
              if fInserting then
                Helper := '';
              fUndoList.AddChange(crInsert, StartOfBlock,
                LogicalCaretXY,
                Helper, smNormal);
              if CaretX >= LeftChar + fCharsInWindow then
                LeftChar := LeftChar + min(25, fCharsInWindow - 1);
            finally
              FCaret.DecForcePastEOL;
            end;
          end;
        end;
{$ENDIF}
      EcFoldLevel1..EcFoldLevel9:
        FoldAll(Command - EcFoldLevel1);
      EcFoldLevel0:
        UnfoldAll;
      EcFoldCurrent:
        begin
          CY := FFoldedLinesView.ExpandedLineForBlockAtLine(CaretY);
          if CY > 0 then begin
            FFoldedLinesView.FoldAtTextIndex(CY-1);
            SetCaretXY(Point(1, CY));
          end;
        end;
      EcUnFoldCurrent:
          FFoldedLinesView.UnFoldAtTextIndex(CaretY-1);
      EcToggleMarkupWord:
          FMarkupHighCaret.ToggleCurrentWord;
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.DoOnCommandProcessed(Command: TSynEditorCommand;
  AChar: TUTF8Char;
  Data: pointer);
begin
  if Assigned(fOnCommandProcessed) then
    fOnCommandProcessed(Self, Command, AChar, Data);
end;

procedure TCustomSynEdit.DoOnProcessCommand(var Command: TSynEditorCommand;
  var AChar: TUTF8Char; Data: pointer);
begin
  //DebugLn(['TCustomSynEdit.DoOnProcessCommand Command=',Command]);
  if Command < ecUserFirst then begin
    if Assigned(FOnProcessCommand) then
      FOnProcessCommand(Self, Command, AChar, Data);
  end else begin
    if Assigned(FOnProcessUserCommand) then
      FOnProcessUserCommand(Self, Command, AChar, Data);
  end;
end;

procedure TCustomSynEdit.ClearAll;
begin
  InternalBeginUndoBlock;
  SelectAll;
  SelText:='';
  InternalEndUndoBlock;
end;

procedure TCustomSynEdit.ClearSelection;
begin
  if SelAvail then
    SelText := '';
end;

function TCustomSynEdit.GetSelectionMode : TSynSelectionMode;
begin
  Result := fBlockSelection.ActiveSelectionMode;
end;

procedure TCustomSynEdit.SetSelectionMode(const Value: TSynSelectionMode);
begin
  fBlockSelection.ActiveSelectionMode := Value;
end;

procedure TCustomSynEdit.InternalBeginUndoBlock(aList: TSynEditUndoList);
begin
  if aList = nil then aList := fUndoList;
  aList.OnNeedCaretUndo := {$IFDEF FPC}@{$ENDIF}GetCaretUndo;
  aList.BeginBlock;
  IncPaintLock;
  FFoldedLinesView.Lock;
end;

procedure TCustomSynEdit.InternalEndUndoBlock(aList: TSynEditUndoList);
begin
  if aList = nil then aList := fUndoList;
  // Write all trimming info to the end of the undo block,
  // so it will be undone first, and other UndoItems do see the expected spaces
  FFoldedLinesView.UnLock;
   // must be last => May call MoveCaretToVisibleArea, which must only happen
   // after unfold
  DecPaintLock;
  aList.EndBlock; // Todo: Doing this after DecPaintLock, can cause duplicate calls to StatusChanged(scModified)
end;

procedure TCustomSynEdit.BeginUndoBlock;
begin
  fUndoList.OnNeedCaretUndo := {$IFDEF FPC}@{$ENDIF}GetCaretUndo;
  fUndoList.BeginBlock;
  ////FFoldedLinesView.Lock;
  //FTrimmedLinesView.Lock;
end;

procedure TCustomSynEdit.BeginUpdate;
begin
  IncPaintLock;
end;

procedure TCustomSynEdit.EndUndoBlock;
begin
  // Write all trimming info to the end of the undo block,
  // so it will be undone first, and other UndoItems do see the expected spaces
  //FTrimmedLinesView.UnLock;
  ////FFoldedLinesView.UnLock;
  fUndoList.EndBlock;
end;

procedure TCustomSynEdit.EndUpdate;
begin
  DecPaintLock;
end;

procedure TCustomSynEdit.AddKey(Command: TSynEditorCommand;
  Key1: word; SS1: TShiftState; Key2: word; SS2: TShiftState);
var
  Key: TSynEditKeyStroke;
begin
  Key := Keystrokes.Add;
  Key.Command := Command;
  Key.Key := Key1;
  Key.Shift := SS1;
  Key.Key2 := Key2;
  Key.Shift2 := SS2;
end;

procedure TCustomSynEdit.AfterLoadFromFile;
begin
  if (not HandleAllocated) or
     ( (FPaintLock > 0) and not((FPaintLock = 1) and FIsInDecPaintLock) )
  then begin
    Include(fStateFlags, sfAfterLoadFromFileNeeded);
    exit;
  end;
  Exclude(fStateFlags, sfAfterLoadFromFileNeeded);
  if assigned(FFoldedLinesView) then begin
    ScanRanges;
    FFoldedLinesView.UnfoldAll;
    FFoldedLinesView.CollapseDefaultFolds;
    if FPendingFoldState <> '' then
      SetFoldState(FPendingFoldState);
    TopLine := TopLine;
  end;
end;

procedure TCustomSynEdit.MarkListChange(Sender: TSynEditMark; Changes: TSynEditMarkChangeReasons);
begin
  if (smcrAdded in Changes) and Sender.IsBookmark then begin
    FBookMarks[Sender.BookmarkNumber] := Sender;
    if Assigned(FOnPlaceMark) then
      FOnPlaceMark(Self, Sender);
  end;
  if (smcrRemoved in Changes) and Sender.IsBookmark then begin
    FBookMarks[Sender.BookmarkNumber] := nil;
    if Assigned(FOnPlaceMark) then
      FOnClearMark(Self, Sender);
  end;

  if (not Sender.Visible) and (not (smcrVisible in Changes)) then
    exit;

  if smcrLine in Changes then begin
    InvalidateLine(Sender.OldLine); // TODO: only if mark has special line color, or other code markup
    InvalidateGutterLines(Sender.OldLine, Sender.OldLine);
  end;
  InvalidateLine(Sender.Line);  // TODO: only if mark has special line color, or other code markup
  InvalidateGutterLines(Sender.Line, Sender.Line);
end;

function TCustomSynEdit.GetSelStart: integer;                                   //L505 begin

  function llen(const data: string): integer;
  begin
    result := length(Data) + length(LineEnding);
  end;

var
  loop: integer;
  p: TPoint;
begin
  if SelAvail then
  begin
    p:=BlockBegin;
  end
  else
  begin
    p:=LogicalCaretXY;
  end;

  result := 0;
  loop := 0;
  while (loop < (p.Y - 1)) and (loop < FTheLinesView.Count) do
  begin
    result := result + llen(FTheLinesView[loop]);
    inc(loop);
  end;
  if loop < FTheLinesView.Count then
    result := result + Min(p.X, length(FTheLinesView[loop]) + 1);
end;

procedure TCustomSynEdit.SetSelStart(const Value: integer);

  function llen(const data: string): integer;
  begin
    result := length(Data) + length(LineEnding);
  end;

var
  loop: integer;
  count: integer;
begin
  loop := 0;
  count := 0;
  while (loop < FTheLinesView.Count) and (count + llen(FTheLinesView[loop]) < value) do begin
    count := count + llen(FTheLinesView[loop]);
    inc(loop);
  end;
{  CaretX := value - count;
  CaretY := loop + 1;

  fBlockBegin.X := CaretX;
  fBlockBegin.Y := CaretY;}

  //This seems the same as above, but uses the other fixes inside of SetCaretXY
  //to adjust the cursor pos correctly.
  FCaret.LineBytePos := Point(value - count, loop + 1);
  BlockBegin := Point(value - count, loop + 1);
end;

function TCustomSynEdit.GetSelEnd: integer;

  function llen(const data: string): integer;
  begin
    result := length(Data) + length(LineEnding);
  end;

var
  loop: integer;
  p: TPoint;
begin
  if SelAvail then
  begin
    p := BlockEnd;
  end else begin
    p := LogicalCaretXY;
  end;

  result := 0;
  loop := 0;
  while (loop < (p.y - 1)) and (loop < FTheLinesView.Count) do begin
    Result := result + llen(FTheLinesView[loop]);
    inc(loop);
  end;
  if loop<FTheLinesView.Count then
    result := result + p.x;
end;

procedure TCustomSynEdit.SetSelEnd(const Value: integer);

  function llen(const data: string): integer;
  begin
    result := length(Data) + length(LineEnding);
  end;

var
  p: TPoint;
  loop: integer;
  count: integer;
begin
  loop := 0;
  count := 0;
  while (loop < FTheLinesView.Count) and (count + llen(FTheLinesView[loop]) < value) do begin
    count := count + llen(FTheLinesView.strings[loop]);
    inc(loop);
  end;
  p.x := value - count; p.y := loop + 1;
  BlockEnd := p;
end;

procedure TCustomSynEdit.SetExtraLineSpacing(const Value: integer);
begin
  if fExtraLineSpacing=Value then exit;
  fExtraLineSpacing := Value;
  FontChanged(self);
end;

function TCustomSynEdit.GetBookMark(BookMark: integer; var X, Y: integer):
  boolean;
var
  i: integer;
begin
  Result := false;
  if assigned(Marks) then
    for i := 0 to Marks.Count - 1 do
      if Marks[i].IsBookmark and (Marks[i].BookmarkNumber = BookMark) then begin
        X := Marks[i].Column;
        Y := Marks[i].Line;
        X := LogicalToPhysicalPos(Point(X, Y)).x;
        Result := true;
        Exit;
      end;
end;

function TCustomSynEdit.IsBookmark(BookMark: integer): boolean;
var
  x, y: integer;
begin
  Result := GetBookMark(BookMark, x, y);
end;

procedure TCustomSynEdit.MarkTextAsSaved;
begin
  TSynEditStringList(fLines).MarkSaved;
  if FLeftGutter.Visible and FLeftGutter.ChangesPart(0).Visible then
    InvalidateGutter; // Todo: Make the ChangeGutterPart an observer
end;

procedure TCustomSynEdit.ClearUndo;
begin
  fUndoList.Clear;
  fRedoList.Clear;
end;

procedure TCustomSynEdit.SetGutter(const Value: TSynGutter);
begin
  FLeftGutter.Assign(Value);
end;

procedure TCustomSynEdit.SetRightGutter(const AValue: TSynGutter);
begin
  FRightGutter.Assign(AValue);
end;

procedure TCustomSynEdit.GutterChanged(Sender: TObject);
begin
  if (csLoading in ComponentState) then exit;
  InvalidateGutter; //Todo: move to gutter
end;

procedure TCustomSynEdit.GutterResized(Sender: TObject);
begin
  if (csLoading in ComponentState) then exit;

  GutterChanged(Sender);
  fTextOffset := TextLeftPixelOffset - (LeftChar - 1) * fCharWidth;

  if HandleAllocated then begin
    RecalcCharsAndLinesInWin(False);
    UpdateScrollBars;
    Invalidate;
  end;
end;

function TCustomSynEdit.TextLeftPixelOffset(IncludeGutterTextDist: Boolean): Integer;
begin
  if FLeftGutter.Visible then begin
    Result := FLeftGutter.Width;
    if IncludeGutterTextDist then
      inc(Result, GutterTextDist);
  end
  else begin
    Result := 0;
    if IncludeGutterTextDist then
      inc(Result, 1);  // include space for caret at pos.x=1 (if FOffsetX = -1)
  end;
end;

function TCustomSynEdit.TextRightPixelOffset: Integer;
begin
  if FRightGutter.Visible then
    Result := FRightGutter.Width
  else
    Result := 0;
end;

procedure TCustomSynEdit.LockUndo;
begin
  fUndoList.Lock;
  fRedoList.Lock
end;

procedure TCustomSynEdit.UnlockUndo;
begin
  fUndoList.Unlock;
  fRedoList.Unlock;
end;

procedure TCustomSynEdit.WMMouseWheel(var Message: TLMMouseEvent);
var
  lState: TShiftState;
const
  WHEEL_DELTA = 120;
begin
  if ((sfHorizScrollbarVisible in fStateFlags) and (Message.Y > ClientHeight)) or
     ((sfVertScrollbarVisible in fStateFlags) and (Message.X > ClientWidth))
   then begin
     inherited;
     exit;
   end;

  lState := Message.State - [ssCaps, ssNum, ssScroll]; // Remove unreliable states, see http://bugs.freepascal.org/view.php?id=20065
  Inc(FMouseWheelAccumulator, Message.WheelDelta);

  FMouseClickDoPopUp := False;
  IncPaintLock;
  try
    while FMouseWheelAccumulator > WHEEL_DELTA do begin
      dec(FMouseWheelAccumulator, WHEEL_DELTA);
      FindAndHandleMouseAction(mbWheelDown, lState, Message.X, Message.Y, ccSingle, cdDown);
    end;

    while FMouseWheelAccumulator < WHEEL_DELTA do begin
      inc(FMouseWheelAccumulator, WHEEL_DELTA);
      FindAndHandleMouseAction(mbWheelUp, lState, Message.X, Message.Y, ccSingle, cdDown);
    end;
  finally
    DecPaintLock;
  end;

  if FMouseClickDoPopUp and (PopupMenu <> nil) then begin
    PopupMenu.PopupComponent:=self;
    PopupMenu.PopUp;
  end;

  Message.Result := 1 // handled, skip further handling by interface
end;

procedure TCustomSynEdit.SetWantTabs(const Value: boolean);
begin
  fWantTabs := Value;
end;

procedure TCustomSynEdit.SetTabWidth(Value: integer);
begin
  Value := MinMax(Value, 1{0}, 256);
  if (Value <> fTabWidth) then begin
    fTabWidth := Value;
    FTabbedLinesView.TabWidth := Value;
    Invalidate; // to redraw text containing tab chars
  end;
end;

// find / replace
function TCustomSynEdit.SearchReplace(const ASearch, AReplace: string;
  AOptions: TSynSearchOptions): integer;
var
  StartPos: TPoint;
begin
  if (ssoFindContinue in AOptions) and SelAvail then begin
    if ssoBackwards in AOptions then
      StartPos := BlockBegin
    else
      StartPos := BlockEnd;
  end
  else
    StartPos := LogicalCaretXY;
  Result := SearchReplaceEx(ASearch, AReplace, AOptions, StartPos);
end;

function TCustomSynEdit.SearchReplaceEx(const ASearch, AReplace: string;
  AOptions: TSynSearchOptions; AStart: TPoint): integer;
var
  ptStart, ptEnd: TPoint; // start and end of the search range
  ptCurrent: TPoint; // current search position
  nFound: integer;
  bBackward, bFromCursor: boolean;
  bPrompt: boolean;
  bReplace, bReplaceAll: boolean;
  nAction: TSynReplaceAction;
  CurReplace: string;
  ptFoundStart, ptFoundEnd: TPoint;

  function InValidSearchRange(First, Last: integer): boolean;
  begin
    Result := TRUE;
    case FBlockSelection.ActiveSelectionMode of
      smNormal:
        if ((ptCurrent.Y = ptStart.Y) and (First < ptStart.X)) or
          ((ptCurrent.Y = ptEnd.Y) and (Last > ptEnd.X)) then Result := FALSE;
      smColumn:
        Result := (First >= ptStart.X) and (Last <= ptEnd.X);
    end;
  end;

begin
  Result := 0;
  // can't search for or replace an empty string
  if Length(ASearch) = 0 then exit;
  // get the text range to search in, ignore the "Search in selection only"
  // option if nothing is selected
  bBackward := (ssoBackwards in AOptions);
  bPrompt := (ssoPrompt in AOptions);
  bReplace := (ssoReplace in AOptions);
  bReplaceAll := (ssoReplaceAll in AOptions);
  bFromCursor := not (ssoEntireScope in AOptions);
  if not SelAvail then Exclude(AOptions, ssoSelectedOnly);
  if (ssoSelectedOnly in AOptions) then begin
    ptStart := BlockBegin;
    ptEnd := BlockEnd;
    // search the whole line in the line selection mode
    if (FBlockSelection.ActiveSelectionMode = smLine) then begin
      ptStart.X := 1;
      ptEnd.X := Length(FTheLinesView[ptEnd.Y - 1]) + 1;
    end else if (FBlockSelection.ActiveSelectionMode = smColumn) then
      // make sure the start column is smaller than the end column
      if (ptStart.X > ptEnd.X) then begin
        nFound := ptStart.X;
        ptStart.X := ptEnd.X;
        ptEnd.X := nFound;
      end;
    // ignore the cursor position when searching in the selection
    if bBackward then ptCurrent := ptEnd else ptCurrent := ptStart;
  end else begin
    ptStart := Point(1, 1);
    ptEnd.Y := FTheLinesView.Count;
    ptEnd.X := Length(FTheLinesView[ptEnd.Y - 1]) + 1;
    if bFromCursor then
      if bBackward then
        ptEnd := AStart
      else
        ptStart := AStart;
    if bBackward then ptCurrent := ptEnd else ptCurrent := ptStart;
  end;
  // initialize the search engine
  fTSearch.Sensitive := ssoMatchCase in AOptions;
  fTSearch.Whole := ssoWholeWord in AOptions;
  fTSearch.Pattern := ASearch;
  fTSearch.RegularExpressions := ssoRegExpr in AOptions;
  fTSearch.RegExprMultiLine := ssoRegExprMultiLine in AOptions;
  fTSearch.Replacement:=AReplace;
  fTSearch.Backwards:=bBackward;
  // search while the current search position is inside of the search range
  IncPaintLock;
  try
    //DebugLn(['TCustomSynEdit.SearchReplace ptStart=',dbgs(ptStart),' ptEnd=',dbgs(ptEnd),' ASearch="',dbgstr(ASearch),'" AReplace="',dbgstr(AReplace),'"']);
    while fTSearch.FindNextOne(FTheLinesView,ptStart,ptEnd,ptFoundStart,ptFoundEnd, True) do
    begin
      //DebugLn(['TCustomSynEdit.SearchReplace FOUND ptStart=',dbgs(ptStart),' ptEnd=',dbgs(ptEnd),' ptFoundStart=',dbgs(ptFoundStart),' ptFoundEnd=',dbgs(ptFoundEnd)]);
      // check if found place is entirely in range
      if (FBlockSelection.ActiveSelectionMode <> smColumn)
      or ((ptFoundStart.Y=ptFoundEnd.Y)
          and (ptFoundStart.X >= ptStart.X) and (ptFoundEnd.X <= ptEnd.X)) then
      begin
        // pattern found
        Inc(Result);
        // Select the text, so the user can see it in the OnReplaceText event
        // handler or as the search result.
        BlockBegin := ptFoundStart;
        if bBackward then LogicalCaretXY := BlockBegin;
        BlockEnd := ptFoundEnd;
        if not bBackward then LogicalCaretXY := ptFoundEnd;
        // If it's a 'search' only we can leave the procedure now.
        if not (bReplace or bReplaceAll) then exit;
        // Prompt and replace or replace all.  If user chooses to replace
        // all after prompting, turn off prompting.
        CurReplace:=AReplace;
        if ssoRegExpr in AOptions then
          CurReplace:=fTSearch.RegExprReplace;
        if bPrompt and Assigned(fOnReplaceText) then begin
          EnsureCursorPosVisible;
          try
            DecPaintLock;
            nAction := DoOnReplaceText(ASearch,CurReplace,
                                       ptFoundStart.Y,ptFoundStart.X);
          finally
            IncPaintLock;
          end;
          if nAction = raCancel then exit;
        end else
          nAction := raReplace;
        if not (nAction = raSkip) then begin
          // user has been prompted and has requested to silently replace all
          // so turn off prompting
          if nAction = raReplaceAll then begin
            bReplaceAll := True;
            bPrompt := False;
          end;
          // replace text
          //DebugLn(['TCustomSynEdit.SearchReplace OldSel="',dbgstr(SelText),'"']);
          SetSelTextExternal(CurReplace);
          //DebugLn(['TCustomSynEdit.SearchReplace NewSel="',dbgstr(SelText),'"']);
          // adjust positions
          ptEnd:=AdjustPositionAfterReplace(ptEnd,ptFoundStart,ptFoundEnd,
                                            CurReplace);
          ptFoundEnd:=AdjustPositionAfterReplace(ptFoundEnd,
                                            ptFoundStart,ptFoundEnd,CurReplace);
        end;
        if not bReplaceAll then
          exit;
      end;
      // shrink search range for next search
      if ssoSearchInReplacement in AOptions then begin
        if bBackward then begin
          ptEnd:=ptFoundEnd;
        end else begin
          ptStart:=ptFoundStart;
        end;
      end else begin
        if bBackward then begin
          ptEnd:=ptFoundStart;
        end else begin
          ptStart:=ptFoundEnd;
        end;
      end;
      //DebugLn(['TCustomSynEdit.SearchReplace FIND NEXT ptStart=',dbgs(ptStart),' ptEnd=',dbgs(ptEnd)]);
    end;
  finally
    DecPaintLock;
  end;
end;

{$IFDEF SYN_MBCSSUPPORT}

procedure TCustomSynEdit.MBCSGetSelRangeInLineWhenColumnSelectionMode(
  const s: string; var ColFrom, ColTo: Integer);
  // --ColFrom and ColTo are in/out parameter. their range
  //    will be from 1 to MaxInt.
  // --a range of selection means:  Copy(s, ColFrom, ColTo - ColFrom);
  //    be careful what ColTo means.
var
  Len: Integer;
begin
  Len := Length(s);
  if (0 < ColFrom) and (ColFrom <= Len) then
    if mbTrailByte = ByteType(s, ColFrom) then
      Inc(ColFrom);
  if (0 < ColTo) and (ColTo <= Len) then
    if mbTrailByte = ByteType(s, ColTo) then
      Inc(ColTo);
end;

{$ENDIF}

function TCustomSynEdit.IsPointInSelection(Value: TPoint): boolean;
var
  ptBegin, ptEnd: TPoint;
begin
  ptBegin := BlockBegin;
  ptEnd := BlockEnd;
  if (Value.Y >= ptBegin.Y) and (Value.Y <= ptEnd.Y) and
    ((ptBegin.Y <> ptEnd.Y) or (ptBegin.X <> ptEnd.X))
    then begin
    if FBlockSelection.SelectionMode = smLine then
      Result := TRUE
    else if (FBlockSelection.ActiveSelectionMode = smColumn) then begin
      if (ptBegin.X > ptEnd.X) then
        Result := (Value.X >= ptEnd.X) and (Value.X < ptBegin.X)
      else if (ptBegin.X < ptEnd.X) then
        Result := (Value.X >= ptBegin.X) and (Value.X < ptEnd.X)
      else
        Result := FALSE;
    end else
      Result := ((Value.Y > ptBegin.Y) or (Value.X >= ptBegin.X)) and
        ((Value.Y < ptEnd.Y) or (Value.X < ptEnd.X));
  end else
    Result := FALSE;
end;

procedure TCustomSynEdit.BookMarkOptionsChanged(Sender: TObject);
begin
  InvalidateGutter;
end;

procedure TCustomSynEdit.SetOptions(Value: TSynEditorOptions);
var
  ChangedOptions: TSynEditorOptions;
  i: Integer;
  m: TSynEditorOption;
  MOpt: TSynEditorMouseOptions;
  f: Boolean;
begin
  Value := Value - SYNEDIT_UNIMPLEMENTED_OPTIONS;
  if (Value = FOptions) then exit;

  ChangedOptions:=(FOptions-Value)+(Value-FOptions);
  FOptions := Value;
  UpdateOptions;

  if not (eoScrollPastEol in Options) then
    LeftChar := LeftChar;
  if (eoScrollPastEol in Options) or (eoScrollPastEof in Options) then begin
    UpdateScrollBars;
    TopLine := TopLine;
  end;
  // (un)register HWND as drop target
  if (eoDropFiles in ChangedOptions) and not (csDesigning in ComponentState) and HandleAllocated then
    ; // ToDo DragAcceptFiles
  if (eoPersistentCaret in ChangedOptions) and HandleAllocated then
    UpdateCaret;
  if (eoShowSpecialChars in ChangedOptions) and HandleAllocated then
    Invalidate;
  fMarkupSpecialChar.Enabled := (eoShowSpecialChars in fOptions);

  (* Deal with deprecated Mouse values
     Those are all controlled by mouse-actions.
     As long as the default mouse actions are set, the below will act as normal
  *)

  MOpt := FMouseOptions;
  f := False;
  for m := low(SYNEDIT_OLD_MOUSE_OPTIONS_MAP) to high(SYNEDIT_OLD_MOUSE_OPTIONS_MAP) do
    if (m in SYNEDIT_OLD_MOUSE_OPTIONS) and (m in ChangedOptions) then begin
      f := True;
      if (m in FOptions)
      then MOpt := MOpt + [SYNEDIT_OLD_MOUSE_OPTIONS_MAP[m]]
      else MOpt := MOpt - [SYNEDIT_OLD_MOUSE_OPTIONS_MAP[m]];
    end;
  if f then
    MouseOptions := MOpt;

  FOptions := Value; // undo changes applied by MouseOptions

end;

procedure TCustomSynEdit.UpdateOptions;
begin
  FTrimmedLinesView.Enabled := eoTrimTrailingSpaces in fOptions;
  FCaret.AllowPastEOL := (eoScrollPastEol in fOptions);
  FCaret.KeepCaretX := (eoKeepCaretX in fOptions);
  FBlockSelection.Enabled := not(eoNoSelection in fOptions);
  FUndoList.GroupUndo := eoGroupUndo in fOptions;
end;

procedure TCustomSynEdit.SetOptions2(const Value: TSynEditorOptions2);
var
  ChangedOptions: TSynEditorOptions2;
begin
  if (Value <> fOptions2) then begin
    ChangedOptions:=(fOptions2 - Value) + (Value - fOptions2);
    fOptions2 := Value;
    UpdateOptions2;
    if eoAlwaysVisibleCaret in fOptions2 then
      MoveCaretToVisibleArea;
    if (eoAutoHideCursor in ChangedOptions) and not(eoAutoHideCursor in fOptions2) then
      UpdateCursor;
  end;
end;

procedure TCustomSynEdit.UpdateOptions2;
begin
  FBlockSelection.Persistent := eoPersistentBlock in fOptions2;
  FCaret.SkipTabs := (eoCaretSkipTab in fOptions2);
end;

procedure TCustomSynEdit.SetMouseOptions(AValue: TSynEditorMouseOptions);
var
  ChangedOptions: TSynEditorMouseOptions;
  m: TSynEditorOption;
  f: Boolean;
begin
  if FMouseOptions = AValue then Exit;

  ChangedOptions := (FMouseOptions-AValue)+(AValue-FMouseOptions);
  FMouseOptions := AValue;
  // changes take effect when MouseActions are accessed

  for m := low(SYNEDIT_OLD_MOUSE_OPTIONS_MAP) to high(SYNEDIT_OLD_MOUSE_OPTIONS_MAP) do
    if (m in SYNEDIT_OLD_MOUSE_OPTIONS) and
       (SYNEDIT_OLD_MOUSE_OPTIONS_MAP[m] in ChangedOptions) and
       not(SYNEDIT_OLD_MOUSE_OPTIONS_MAP[m] in FMouseOptions)
    then
      FOptions := FOptions - [m];

  if (emShowCtrlMouseLinks in ChangedOptions) then begin
    if assigned(fMarkupCtrlMouse) then
      fMarkupCtrlMouse.UpdateCtrlMouse;
    UpdateCursor;
  end;
end;

procedure TCustomSynEdit.UpdateMouseOptions;
begin
  //
end;

procedure TCustomSynEdit.SetOptionFlag(Flag: TSynEditorOption; Value: boolean);
begin
  if (Value <> (Flag in fOptions)) then begin
    if Value then
      Options := Options + [Flag]
    else
      Options := Options - [Flag];
  end;
end;

procedure TCustomSynEdit.SizeOrFontChanged(bFont: boolean);
begin
  if HandleAllocated then begin
    LastMouseCaret:=Point(-1,-1);
    RecalcCharsAndLinesInWin(False);

    //DebugLn('TCustomSynEdit.SizeOrFontChanged fLinesInWindow=',dbgs(fLinesInWindow),' ClientHeight=',dbgs(ClientHeight),' ',dbgs(fTextHeight));
    //debugln('TCustomSynEdit.SizeOrFontChanged A ClientWidth=',dbgs(ClientWidth),' FLeftGutter.Width=',dbgs(FLeftGutter.Width),' ScrollBarWidth=',dbgs(ScrollBarWidth),' fCharWidth=',dbgs(fCharWidth),' fCharsInWindow=',dbgs(fCharsInWindow),' Width=',dbgs(Width));
    if bFont then begin
      UpdateScrollbars;
      Exclude(fStateFlags, sfCaretChanged);
      Invalidate;
    end else
      UpdateScrollbars;
    Exclude(fStateFlags, sfScrollbarChanged);
    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
  end;
end;

procedure TCustomSynEdit.RecalcCharsAndLinesInWin(CheckCaret: Boolean);
var
  NewLinesInWindow: Integer;
  w: Integer;
begin
  w := ClientWidth - TextLeftPixelOffset - TextRightPixelOffset - ScrollBarWidth;
  FCharsInWindow := Max(1, w div fCharWidth);

  NewLinesInWindow := Max(0,ClientHeight - ScrollBarWidth) div Max(1,fTextHeight);
  if NewLinesInWindow <> FLinesInWindow then begin
    if fLinesInWindow >= 0 then
      StatusChanged([scLinesInWindow]);
    FLinesInWindow := NewLinesInWindow;
    FFoldedLinesView.LinesInWindow := fLinesInWindow;
    FMarkupManager.LinesInWindow:= fLinesInWindow;
  end;

  FScreenCaret.Lock;
  FScreenCaret.ClipRect := Rect(TextLeftPixelOffset(False), 0,
                                ClientWidth - TextRightPixelOffset - ScrollBarWidth + 1,
                                ClientHeight - ScrollBarWidth);
  FScreenCaret.ClipExtraPixel := w - fCharsInWindow * fCharWidth;
  FScreenCaret.UnLock;

  if CheckCaret then begin
    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
  end;
end;

procedure TCustomSynEdit.MoveCaretHorz(DX: integer);
var
  NewCaret: TPoint;
  s: String;
  PhysicalLineLen: Integer;
begin
  NewCaret:=Point(CaretX+DX,CaretY);
  if NewCaret.X<1 then begin
    if (eoScrollPastEol in fOptions) or (NewCaret.Y=1) then
      NewCaret.X:=1
    else begin
      // move to end of prev line
      NewCaret.Y:= FFoldedLinesView.TextPosAddLines(NewCaret.Y, -1);
      s:=FTheLinesView[NewCaret.Y-1];
      PhysicalLineLen:=LogicalToPhysicalPos(Point(length(s)+1,NewCaret.Y)).X-1;
      NewCaret.X:=PhysicalLineLen+1;
    end;
  end
  else if not (eoScrollPastEol in fOptions) then begin
    s:=LineText;
    PhysicalLineLen:=LogicalToPhysicalPos(Point(length(s)+1,CaretY)).X-1;
    if (NewCaret.X > PhysicalLineLen+1) and (DX > 0) then begin
      // move to start of next line (if it was a move to the right)
      NewCaret.X := 1;
      NewCaret.Y := FFoldedLinesView.TextPosAddLines(NewCaret.Y, +1);
    end;
  end;
  DoIncPaintLock(Self);  // No editing is taking place
  FCaret.IncForcePastEOL;
  if DX > 0 then
    FCaret.IncForceAdjustToNextChar;
  FCaret.LineCharPos := NewCaret;
  FCaret.DecForcePastEOL;
  if DX > 0 then
    FCaret.DecForceAdjustToNextChar;
  DoDecPaintLock(Self);
end;

procedure TCustomSynEdit.MoveCaretVert(DY: integer);
// moves Caret vertical DY unfolded lines
var
  NewCaret: TPoint;
  OldCaret: TPoint;
begin
  OldCaret:=CaretXY;
  NewCaret:=OldCaret;
  NewCaret.Y:=FFoldedLinesView.TextPosAddLines(NewCaret.Y, DY);
  DoIncPaintLock(Self); // No editing is taking place
  FCaret.LinePos := NewCaret.Y;
  DoDecPaintLock(Self);
end;

procedure TCustomSynEdit.SetCaretAndSelection(const ptCaret, ptBefore,
  ptAfter: TPoint; Mode: TSynSelectionMode = smCurrent; MakeSelectionVisible: Boolean = False);
// caret is physical (screen)
// Before, After is logical (byte)
var
  L1, L2, LBottomLine, LCaretFirst, LCaretLast: Integer;
begin
  DoIncPaintLock(Self); // No editing is taking place

  CaretXY := ptCaret;
  SetBlockBegin(ptBefore);
  SetBlockEnd(ptAfter);
  if Mode <> smCurrent then
    FBlockSelection.ActiveSelectionMode := Mode;
  AquirePrimarySelection;

  if MakeSelectionVisible then begin
    //l1 := FBlockSelection.FirstLineBytePos;;
    LBottomLine := FFoldedLinesView.TextPosAddLines(TopLine, LinesInWindow);

    LCaretFirst := CaretY;
    LCaretLast := Max(1, FFoldedLinesView.TextPosAddLines(CaretY, 1-LinesInWindow));  // Will have caret on last visible line

    l1 := Min(LCaretFirst, FBlockSelection.FirstLineBytePos.y);
    l2 := Max(LCaretFirst, FBlockSelection.LastLineBytePos.y);

    if CaretY < TopLine then begin
      // Scrolling up,  Topline = L1 ; but ensure Caret
      TopLine := Max(LCaretLast,
                 Min(LCaretFirst,
                     L1
                    ));
    end
    else if CaretY > LBottomLine then begin
      // Scrolling down, LastLine = L2
      TopLine := Max(LCaretLast,
                 Min(LCaretFirst,
                     FFoldedLinesView.TextPosAddLines(L2, 1-LinesInWindow)
                    ));
    end
    else begin
      // Caret alreayd visible, check block
      if l1 < TopLine then
        TopLine := Max(LCaretLast,
                   Min(LCaretFirst,
                       L1
                      ))
      else
      if l2 > LBottomLine then
        TopLine := Max(LCaretLast,
                   Min(LCaretFirst,
                       FFoldedLinesView.TextPosAddLines(L2, 1-LinesInWindow)
                      ));
    end;
  end;

  DoDecPaintLock(Self);
end;

procedure TCustomSynEdit.RecalcCharExtent;
var
  i: Integer;
begin
  FFontDummy.Assign(Font);
  with FFontDummy do begin
    // Keep GTK happy => By ensuring a change the XFLD fontname gets cleared
    Pitch := fpVariable;
    Style := [fsBold];
    Pitch := fpDefault; // maybe Fixed
    // TODO: Clear style only, if Highlighter uses styles
    Style := [];        // Reserved for Highlighter
  end;
  //debugln(['TCustomSynEdit.RecalcCharExtent ',fFontDummy.Name,' ',fFontDummy.Size]);
  with fTextDrawer do begin
    //debugln('TCustomSynEdit.RecalcCharExtent A UseUTF8=',dbgs(UseUTF8),
    //  ' Font.CanUTF8='+dbgs(Font.CanUTF8)+' CharHeight=',dbgs(CharHeight));
    BaseFont := FFontDummy;

    if Assigned(fHighlighter) then
      for i := 0 to Pred(fHighlighter.AttrCount) do
        BaseStyle := fHighlighter.Attribute[i].Style;

    CharExtra := fExtraCharSpacing;
    fTextHeight := CharHeight + fExtraLineSpacing;
    fCharWidth := CharWidth;
    FScreenCaret.Lock;
    FScreenCaret.CharWidth := fCharWidth;
    FScreenCaret.CharHeight := fTextHeight;
    FScreenCaret.UnLock;
  end;
  FUseUTF8:=fTextDrawer.UseUTF8;
  FLines.IsUtf8 := FUseUTF8;
  //debugln('TCustomSynEdit.RecalcCharExtent UseUTF8=',dbgs(UseUTF8),' Font.CanUTF8=',dbgs(Font.CanUTF8));
end;

procedure TCustomSynEdit.HighlighterAttrChanged(Sender: TObject);
begin
  RecalcCharExtent;
  SizeOrFontChanged(TRUE);                                                      //jr 2000-10-01
  Invalidate;
  // TODO: obey paintlock
  if fHighlighter.AttributeChangeNeedScan then begin
    FHighlighter.CurrentLines := FTheLinesView;
    FHighlighter.ScanAllRanges;
    fMarkupManager.TextChanged(0, FTheLinesView.Count - 1);
    TopLine := TopLine;
  end;
end;

procedure TCustomSynEdit.StatusChanged(AChanges: TSynStatusChanges);
begin
  fStatusChanges := fStatusChanges + AChanges;
  if PaintLock = 0 then
    DoOnStatusChange(fStatusChanges);
end;

procedure TCustomSynEdit.DoTabKey;
var
  i, iLine: integer;
  PrevLine,
  Spaces: string;
  p: PChar;
  OldCaretX: integer;
begin
  if (eoTabIndent in Options) and SelAvail then begin
    DoBlockIndent;
    exit;
  end;

  InternalBeginUndoBlock;
  try
    i := 0;
    OldCaretX := CaretX;
    SelText := '';
    // With a multi-line block the caret may have advanced, avoid negative spaces
    if CaretX > OldCaretX then
      OldCaretX := CaretX;
    if eoSmartTabs in fOptions then begin
      iLine := CaretY - 1;
      if (iLine > 0) and (iLine < FTheLinesView.Count) then begin
        repeat
          Dec(iLine);
          if iLine < 0 then break;
          PrevLine := FTheLinesView[iLine];
        until PhysicalLineLength(PrevLine, iLine) > OldCaretX - 1;

        if iLine >= 0 then begin
          p := @PrevLine[PhysicalToLogicalCol(PrevLine, iLine, OldCaretX)];
          // scan over non-whitespaces
          while not (p^ in [#0, #9, #32]) do
            inc(p);
          // scan over whitespaces
          while (p^ in [#9, #32]) do
            inc(p);
          i := LogicalToPhysicalCol(PrevLine, iLine, p-@PrevLine[1]+1) - CaretX;
        end;
      end;
    end;
    if i <= 0 then begin
      i := TabWidth - (CaretX - 1) mod TabWidth;
      if i = 0 then i := TabWidth;
    end;
    // i now contains the needed spaces
    Spaces := CreateTabsAndSpaces(CaretX,i,TabWidth,
                                  not (eoTabsToSpaces in Options));
    //debugln('TCustomSynEdit.DoTabKey Spaces="',DbgStr(Spaces),'" TabChar=',DbgStr(TabChar));
    OldCaretX := CaretX;
    //debugln('TCustomSynEdit.DoTabKey Before SetSelText Line="',DbgStr(GetLineText),'"');
    SetSelTextExternal(Spaces);
    //debugln('TCustomSynEdit.DoTabKey After SetSelText Line="',DbgStr(GetLineText),'"');
    CaretX := OldCaretX + i;
    //debugln('TCustomSynEdit.DoTabKey StartOfBlock=',dbgs(StartOfBlock),' fBlockEnd=',dbgs(fBlockEnd),' Spaces="',Spaces,'"');
  finally
    InternalEndUndoBlock;
  end;
  EnsureCursorPosVisible;
end;

procedure TCustomSynEdit.CreateWnd;
begin
  inherited;
  if (eoDropFiles in fOptions) and not (csDesigning in ComponentState) then
    {$IFDEF SYN_LAZARUS}
    // ToDo DragAcceptFiles
    ;
    {$ELSE}
    DragAcceptFiles(Handle, TRUE);
    {$ENDIF}
  SizeOrFontChanged(true);
end;

procedure TCustomSynEdit.DestroyWnd;
begin
  if (eoDropFiles in fOptions) and not (csDesigning in ComponentState) then begin
    {$IFDEF SYN_LAZARUS}
    // ToDo DragAcceptFiles
    ;
    {$ELSE}
    DragAcceptFiles(Handle, FALSE);
    {$ENDIF}
  end;
  {$IFDEF EnableDoubleBuf}
  FreeAndNil(BufferBitmap);
  {$ENDIF}
  SurrenderPrimarySelection;
  inherited DestroyWnd;
end;

procedure TCustomSynEdit.DoBlockIndent;
var
  BB,BE            : TPoint;
  Line : PChar;
  Len, e, y: integer;
  Spaces, Tabs: String;

  function GetLeadWSLen : integer;
  var
    Run : PChar;
  begin
    Run := Line;
    while (Run[0] in [' ', #9]) do
      Inc(Run);
    Result := Run - Line;
  end;

begin
  IncPaintLock;
  FBlockSelection.IncPersistentLock;
  try
    // build text to insert
    if not SelAvail then begin
      BB := CaretXY;
      BE := CaretXY;
      e := BE.y;
    end else begin
      BB := BlockBegin;
      BE := BlockEnd;
      if (BE.X = 1)
      then e := BE.y - 1
      else e := BE.y;
    end;

    Spaces := StringOfChar(#32, FBlockIndent);
    Tabs   := StringOfChar( #9, FBlockTabIndent);
    fUndoList.Lock;
    fRedoList.Lock;
    try
      for y := BB.Y to e do
      begin
        Line := PChar(FTheLinesView[y - 1]);
        Len := GetLeadWSLen;
        FTheLinesView.EditInsert(Len + 1, y, Spaces);
        FTheLinesView.EditInsert(1,       y, Tabs);
      end;
    finally
      fUndoList.Unlock;
      fRedoList.Unlock;
    end;

    fUndoList.AddChange(TSynEditUndoIndent.Create(BB.Y, e, FBlockIndent, FBlockTabIndent));
  finally
    FTrimmedLinesView.ForceTrim; // Otherwise it may reset the block
    FCaret.LineBytePos := FBlockSelection.EndLineBytePos;
    FBlockSelection.DecPersistentLock;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.DoBlockUnindent;
const
  LineEnd = #10;
var
  BB, BE: TPoint;
  FullStrToDelete: PChar;
  Line: PChar;
  Len, LogP1, PhyP1, LogP2, PhyP2, y, StrToDeleteLen, e : integer;
  i, i2, j: Integer;
  SomethingDeleted : Boolean;
  HasTab: Boolean;

  function GetLeadWSLen : integer;
  var
    Run : PChar;
  begin
    Run := Line;
    HasTab := False;
    while (Run[0] in [' ', #9]) do begin
      HasTab := HasTab or (Run[0] = #9);
      Inc(Run);
    end;
    Result := Run - Line;
  end;

begin
  if not SelAvail then begin
    BB := CaretXY;
    BE := CaretXY;
    e := BE.y;
  end else begin
    BB := BlockBegin;
    BE := BlockEnd;
    // convert selection to complete lines
    if BE.X = 1 then
      e := BE.y - 1
    else
      e := BE.y;
  end;

  IncPaintLock;
  FBlockSelection.IncPersistentLock;
  // build string to delete
  StrToDeleteLen := (fBlockIndent+length(LineEnd)) * (e - BB.y + 1) + 1;
  //                 chars per line * lines-1    + last line + null char
  FullStrToDelete := StrAlloc(StrToDeleteLen);
  try
    FullStrToDelete[0] := #0;
    SomethingDeleted := False;

    fUndoList.Lock;
    fRedoList.Lock;

    // before locking the undo list
    for y := BB.Y to e do
    begin
      Line := PChar(FTheLinesView[y - 1]);
      Len := GetLeadWSLen;
      LogP1 := Len + 1;
      if HasTab and (Len > 0) then begin
        // LogP1, PhyP1 log and phys of the first none-whitespace
        PhyP1 := LogicalToPhysicalPos(Point(LogP1, y)).x;
        // LogP2, PhyP2 log and phys of the point to which to delete back
        LogP2 := PhysicalToLogicalPos(Point( Max(PhyP1 - FBlockIndent, 1), y )).x;
        PhyP2 := LogicalToPhysicalPos(Point(LogP2,y)).x;

        if PhyP1 - PhyP2 <> FBlockIndent then begin
          // need tab to space
          StrCat(FullStrToDelete, PChar(copy(Line, LogP2, LogP1 - LogP2)));
          StrCat(FullStrToDelete, PChar(LineEnd));
          FTheLinesView.EditDelete(LogP2, y, LogP1 - LogP2);
          SomethingDeleted := True;

          fUndoList.Unlock;
          fRedoList.Unlock;
          FTheLinesView.EditInsert(LogP2, y, StringOfChar(' ', PhyP1 - PhyP2 - FBlockIndent));
          fUndoList.Lock;
          fRedoList.Lock;
          continue;
        end;
        // tabs present, but no replacement needed (LogP1, LogP2 are correct
      end
      else begin
        // no tabs present
        LogP2 := Max(LogP1 - FBlockIndent, 1);
      end;

      // Remove spaces (or tab)
      if LogP1 - LogP2 > 0 then
        StrCat(FullStrToDelete, PChar(copy(Line, LogP2, LogP1 - LogP2)));
      StrCat(FullStrToDelete, PChar(LineEnd));
      if LogP1 - LogP2 > 0 then
        FTheLinesView.EditDelete(LogP2, y, LogP1 - LogP2);
      SomethingDeleted := SomethingDeleted or (LogP1 - LogP2 > 0);

      // Todo: create FullTabStrToDelete for tabs
      fUndoList.Unlock;
      fRedoList.Unlock;
      Line := PChar(FTheLinesView[y - 1]);
      j := 0;
      for i := 1 to FBlockTabIndent do begin
        i2 := fTabWidth;
        while (i2 > 0) and (Line[j] = #32) do begin
          dec(i2);
          inc(j);
        end;
        if (i2 > 0) and (Line[j] = #9) then inc(j);
      end;
      if j > 0 then
        FTheLinesView.EditDelete(1, y, j);
      fUndoList.Lock;
      fRedoList.Lock;

    end;

    fUndoList.Unlock;
    fRedoList.Unlock;

    if SomethingDeleted then
      fUndoList.AddChange(TSynEditUndoUnIndent.Create(BB.Y, e, FullStrToDelete));

    FTrimmedLinesView.ForceTrim; // Otherwise it may reset the block
  finally
    StrDispose(FullStrToDelete);
    FCaret.LineBytePos := FBlockSelection.EndLineBytePos;
    FBlockSelection.DecPersistentLock;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.DoHomeKey(aMode: TSynHomeMode = synhmDefault);
// jump to start of line (x=1),
// or if already there, jump to first non blank char
// or if blank line, jump to line indent position
// if eoEnhanceHomeKey and behind alternative point then jump first
var
  s: string;
  FirstNonBlank: Integer;
  LineStart: LongInt;
  OldPos: TPoint;
  NewPos: TPoint;
begin
  OldPos := CaretXY;
  NewPos := OldPos;

  if not(eoEnhanceHomeKey in fOptions) and (CaretX > 1) and (aMode in [synhmDefault]) then
  begin
    // not at start of line -> jump to start of line
    NewPos.X := 1;
  end else 
  begin
    // calculate line start position
    FirstNonBlank := -1;
    if CaretY <= FTheLinesView.Count then 
    begin
      s := FTheLinesView[CaretXY.Y - 1];

      // search first non blank char pos
      FirstNonBlank := 1;
      while (FirstNonBlank <= length(s)) and (s[FirstNonBlank] in [#32, #9]) do
        inc(FirstNonBlank);
      if FirstNonBlank > length(s) then
        FirstNonBlank := -1;
    end else
      s := '';

    if (FirstNonBlank >= 1) or (aMode in [synhmFirstWord]) then
    begin
      // this line is not blank
      if FirstNonBlank < 1 then FirstNonBlank := 1;
      LineStart := LogicalToPhysicalPos(Point(FirstNonBlank, CaretY)).x;
    end else 
    begin
      // this line is blank
      // -> use automatic line indent
      LineStart := FBeautifier.GetDesiredIndentForLine(Self, FTheLinesView, FCaret);
    end;

    NewPos.X:=LineStart;
    if (eoEnhanceHomeKey in fOptions)  and (aMode in [synhmDefault]) and
       (OldPos.X>1) and (OldPos.X<=NewPos.X)
    then begin
      NewPos.X:=1;
    end;
  end;
  FCaret.LineCharPos := NewPos;
end;

procedure TCustomSynEdit.DoEndKey;
// jump to start of line (x=1),
// or if already there, jump to first non blank char
// or if blank line, jump to line indent position
// if eoEnhanceHomeKey and behind alternative point then jump first
var
  s: string;
  LastNonBlank: Integer;
  LineEnd: LongInt;
  OldPos: TPoint;
  NewPos: TPoint;
begin
  OldPos := CaretXY;
  NewPos := OldPos;
  s := LineText;

  if not (eoEnhanceEndKey in fOptions2) and (FCaret.BytePos <> Length(s)+1) then begin
    // not at end of real line -> jump to end of line
    FCaret.BytePos := Length(s)+1;
  end else begin
    // calculate line end position
    LastNonBlank := -1;
    if s <> '' then begin
      // search first non blank char pos
      LastNonBlank := Length(s);
      while (LastNonBlank > 0) and (s[LastNonBlank] in [#32, #9]) do
        dec(LastNonBlank);
    end;
    if LastNonBlank >=1 then begin
      // this line is not blank
      LineEnd := LogicalToPhysicalPos(Point(LastNonBlank + 1, CaretY)).x;
    end else begin
      // this line is blank
      // -> use automatic line indent
      LineEnd := FBeautifier.GetDesiredIndentForLine(Self, FTheLinesView, FCaret);
    end;

    NewPos.X:=LineEnd;
    if (eoEnhanceEndKey in fOptions2) and (FCaret.BytePos <> Length(s)+1) and (OldPos.X >= NewPos.X)
    then begin
      FCaret.BytePos := Length(s)+1;
    end
    else
      FCaret.LineCharPos := NewPos;
  end;
end;

{$IFDEF SYN_COMPILER_4_UP}
function TCustomSynEdit.ExecuteAction(ExeAction: TBasicAction): boolean;
begin
  if ExeAction is TEditAction then
  begin
    Result := TRUE;
    if ExeAction is TEditCut then
      CutToClipboard
    else if ExeAction is TEditCopy then
      CopyToClipboard
    else if ExeAction is TEditPaste then
      PasteFromClipboard
{$IFDEF SYN_COMPILER_5_UP}
    else if ExeAction is TEditDelete then
      ClearSelection
    else if ExeAction is TEditUndo then
      Undo
    else if ExeAction is TEditSelectAll then
      SelectAll;
{$ENDIF}
  end else
    Result := inherited ExecuteAction(ExeAction);
end;

function TCustomSynEdit.UpdateAction(TheAction: TBasicAction): boolean;
begin
  if TheAction is TEditAction then
  begin
    Result := Focused;
    if Result then
    begin
      if (TheAction is TEditCut) or (TheAction is TEditCopy) then
        TEditAction(TheAction).Enabled := SelAvail
      else if TheAction is TEditPaste then
        TEditAction(TheAction).Enabled := CanPaste
{$IFDEF SYN_COMPILER_5_UP}
      else if TheAction is TEditDelete then
        TEditAction(TheAction).Enabled := TRUE
      else if TheAction is TEditUndo then
        TEditAction(TheAction).Enabled := CanUndo
      else if TheAction is TEditSelectAll then
        TEditAction(TheAction).Enabled := TRUE;
{$ENDIF}
    end;
  end else
    Result := inherited UpdateAction(TheAction);
end;
{$ENDIF}

procedure TCustomSynEdit.SetModified(Value: boolean);
begin
  TSynEditStringList(FLines).Modified := Value;
end;

procedure TCustomSynEdit.InvalidateLine(Line: integer);
begin
  InvalidateLines(Line, Line);
  InvalidateGutterLines(Line, Line);
end;

function TCustomSynEdit.GetReadOnly: boolean;
begin
  Result := fReadOnly;
end;

procedure TCustomSynEdit.SetReadOnly(Value: boolean);
begin
  if fReadOnly <> Value then begin
    fReadOnly := Value;
    StatusChanged([scReadOnly]);
  end;
end;

procedure TCustomSynEdit.FindMatchingBracket;
begin
  FindMatchingBracket(CaretXY,false,true,false,false);
end;

function TCustomSynEdit.FindMatchingBracket(PhysStartBracket: TPoint;
  StartIncludeNeighborChars, MoveCaret, SelectBrackets, OnlyVisible: boolean
  ): TPoint;
// returns physical (screen) position of end bracket
const
  // keep the ' last
  Brackets: array[0..7] of char = ('(', ')', '[', ']', '{', '}', '''', '"');
type
  TokenPos = Record X: Integer; Attr: Integer; end;
var
  Line, s1: string;
  PosX, PosY: integer;
  StartPt: TPoint;
  LogicalStart: TPoint;
  // for ContextMatch
  BracketKind, TmpStart: Integer;
  TmpAttr : TSynHighlighterAttributes;
  // for IsContextBracket
  MaxKnownTokenPos, TokenListCnt: Integer;
  TokenPosList: Array of TokenPos;

  // remove all text, that is not of desired attribute
  function IsContextBracket: boolean;
  var
    i, l: Integer;
  begin
    if not assigned(fHighlighter) then exit(true);
    if PosX > MaxKnownTokenPos then begin
      // Token is not yet known
      l := Length(TokenPosList);
      if l < max(CharsInWindow * 2, 32) then begin
        l := max(CharsInWindow * 2, 32);
        SetLength(TokenPosList, l);
      end;
      // Init the Highlighter only once per line
      if MaxKnownTokenPos < 1 then begin
        fHighlighter.CurrentLines := FTheLinesView;
        fHighlighter.StartAtLineIndex(PosY - 1);
        TokenListCnt := 0;
      end
      else
        fHighlighter.Next;
      i := TokenListCnt;
      while not fHighlighter.GetEol do begin
        TokenPosList[i].X := fHighlighter.GetTokenPos + 1;
        TokenPosList[i].Attr := fHighlighter.GetTokenKind;
        if TokenPosList[i].X > PosX then begin
          TokenListCnt := i + 1;
          MaxKnownTokenPos := TokenPosList[i].X;
          Result := TokenPosList[i-1].Attr = BracketKind;
          exit;
        end;
        inc(i);
        if i >= l then begin
          l := l * 4;
          SetLength(TokenPosList, l);
        end;
        fHighlighter.Next;
      end;
      MaxKnownTokenPos := Length(Line);
      TokenPosList[i].X := MaxKnownTokenPos;
      TokenListCnt := i + 1;
      Result := TokenPosList[i-1].Attr = BracketKind;
      exit;
    end;
    // Token is in previously retrieved values
    i := 1;
    while (i < TokenListCnt) and (TokenPosList[i].X <= PosX) do
      inc(i);
    Result := TokenPosList[i-1].Attr = BracketKind;
  end;

  procedure DoMatchingBracketFound;
  var
    EndPt, DummyPt: TPoint;
  begin
    // matching bracket found, set caret and bail out
    Result := Point(PosX, PosY); // start with logical (byte) position
    if SelectBrackets then begin
      EndPt:=Result;
      if (EndPt.Y < StartPt.Y)
        or ((EndPt.Y = StartPt.Y) and (EndPt.X < StartPt.X)) then
      begin
        DummyPt:=StartPt;
        StartPt:=EndPt;
        EndPt:=DummyPt;
      end;
      inc(EndPt.X);
      SetCaretAndSelection(CaretXY, StartPt, EndPt);
    end
    else if MoveCaret then
      LogicalCaretXY := Result;
  end;

  procedure DoFindMatchingQuote(q: char);
  var
    Test: char;
    Len: integer;
  begin
    StartPt:=Point(PosX,PosY);
    GetHighlighterAttriAtRowColEx(StartPt, s1, BracketKind, TmpStart, TmpAttr);
    if (TmpStart = PosX) and (Length(s1)>0) and (s1[Length(s1)] = q) then begin
      PosX := PosX + Length(s1) - 1;
      DoMatchingBracketFound;
      exit;
    end;
    if (TmpStart + Length(s1) - 1 = PosX) and (Length(s1)>0) and (s1[1] = q) then begin
      PosX := PosX - Length(s1) + 1;
      DoMatchingBracketFound;
      exit;
    end;
    MaxKnownTokenPos := 0;
    Len := PosX;
    // search until start of line
    while PosX > 1 do begin
      Dec(PosX);
      Test := Line[PosX];
      if (Test = q) and IsContextBracket then begin
        DoMatchingBracketFound;
        exit;
      end;
    end;
    PosX := Len;
    Len := Length(Line);
    while PosX < Len do begin
      Inc(PosX);
      Test := Line[PosX];
      if (Test = q) and IsContextBracket then begin
        DoMatchingBracketFound;
        exit;
      end;
    end;
  end;

  procedure DoFindMatchingBracket(i: integer);
  var
    Test, BracketInc, BracketDec: char;
    NumBrackets, Len: integer;
  begin
    StartPt:=Point(PosX,PosY);
    GetHighlighterAttriAtRowColEx(StartPt, s1, BracketKind, TmpStart, TmpAttr);
    MaxKnownTokenPos := 0;
    BracketInc := Brackets[i];
    BracketDec := Brackets[i xor 1]; // 0 -> 1, 1 -> 0, ...
    // search for the matching bracket (that is until NumBrackets = 0)
    NumBrackets := 1;
    if Odd(i) then begin
      // closing bracket -> search opening bracket
      repeat
        // search until start of line
        while PosX > 1 do begin
          Dec(PosX);
          Test := Line[PosX];
          if (Test = BracketInc) and IsContextBracket then
            Inc(NumBrackets)
          else if (Test = BracketDec) and IsContextBracket then begin
            Dec(NumBrackets);
            if NumBrackets = 0 then begin
              DoMatchingBracketFound;
              exit;
            end;
          end;
        end;
        // get previous line if possible
        if PosY = 1 then break;
        Dec(PosY);
        if OnlyVisible
        and ((PosY<TopLine) or (PosY >= ScreenRowToRow(LinesInWindow)))
        then
          break;
        Line := FTheLinesView[PosY - 1];
        MaxKnownTokenPos := 0;
        PosX := Length(Line) + 1;
      until FALSE;
    end else begin
      // opening bracket -> search closing bracket
      repeat
        // search until end of line
        Len := Length(Line);
        while PosX < Len do begin
          Inc(PosX);
          Test := Line[PosX];
          if (Test = BracketInc) and IsContextBracket then
            Inc(NumBrackets)
          else if (Test = BracketDec) and IsContextBracket then begin
            Dec(NumBrackets);
            if NumBrackets = 0 then begin
              DoMatchingBracketFound;
              exit;
            end;
          end;
        end;
        // get next line if possible
        if PosY = FTheLinesView.Count then break;
        Inc(PosY);
        if OnlyVisible
        and ((PosY < TopLine) or (PosY >= ScreenRowToRow(LinesInWindow)))
        then
          break;
        Line := FTheLinesView[PosY - 1];
        MaxKnownTokenPos := 0;
        PosX := 0;
      until FALSE;
    end;
  end;

  procedure DoCheckBracket;
  var
    i: integer;
    Test: char;
  begin
    if Length(Line) >= PosX then begin
      Test := Line[PosX];
      // is it one of the recognized brackets?
      for i := Low(Brackets) to High(Brackets) do begin
        if Test = Brackets[i] then begin
          // this is the bracket, get the matching one and the direction
          if Brackets[i] in ['''', '"'] then
            DoFindMatchingQuote(Brackets[i])
          else
            DoFindMatchingBracket(i);
          exit;
        end;
      end;
    end;
  end;

begin
  Result.X:=-1;
  Result.Y:=-1;

  // get char at caret
  LogicalStart:=PhysicalToLogicalPos(PhysStartBracket);
  PosX := LogicalStart.X;
  PosY := LogicalStart.Y;
  if (PosY<1) or (PosY>FTheLinesView.Count) then exit;
  if OnlyVisible
  and ((PosY<TopLine) or (PosY >= ScreenRowToRow(LinesInWindow)))
  then
   exit;

  Line := FTheLinesView[PosY - 1];
  try
    DoCheckBracket;
    if Result.Y>0 then exit;
    if StartIncludeNeighborChars then begin
      if PosX>1 then begin
        // search in front
        dec(PosX);
        DoCheckBracket;
        if Result.Y>0 then exit;
        inc(PosX);
      end;
      if PosX<Length(Line) then begin
        // search behind
        inc(PosX);
        DoCheckBracket;
        if Result.Y>0 then exit;
      end;
    end;
  finally
    if Result.Y>0 then begin
      Result:=LogicalToPhysicalPos(Result);
    end;
  end;
end;

                                                                                 //L505 begin
function TCustomSynEdit.GetHighlighterAttriAtRowCol(XY: TPoint;
  var Token: string; var Attri: TSynHighlighterAttributes): boolean;
var
  TmpType, TmpStart: Integer;
begin
  Result := GetHighlighterAttriAtRowColEx(XY, Token, TmpType, TmpStart, Attri);
end;

function TCustomSynEdit.GetHighlighterAttriAtRowColEx(XY: TPoint;
  var Token: string; var TokenType, Start: Integer;
  var Attri: TSynHighlighterAttributes): boolean;
var
  PosX, PosY: integer;
  Line: string;
begin
  PosY := XY.Y -1;
  if Assigned(Highlighter) and (PosY >= 0) and (PosY < FTheLinesView.Count) then
  begin
    Line := FTheLinesView[PosY];
    fHighlighter.CurrentLines := FTheLinesView;
    Highlighter.StartAtLineIndex(PosY);
    PosX := XY.X;
    if (PosX > 0) and (PosX <= Length(Line)) then begin
      while not Highlighter.GetEol do begin
        Start := Highlighter.GetTokenPos + 1;
        Token := Highlighter.GetToken;
        if (PosX >= Start) and (PosX < Start + Length(Token)) then begin
          Attri := Highlighter.GetTokenAttribute;
          TokenType := Highlighter.GetTokenKind;
          Result := TRUE;
          exit;
        end;
        Highlighter.Next;
      end;
    end;
  end;
  Token := '';
  Attri := nil;
  TokenType := -1;
  Result := FALSE;
end;

function TCustomSynEdit.IdentChars: TSynIdentChars;
begin
  Result := FWordBreaker.IdentChars;  // Maybe WordChars?
end;

function TCustomSynEdit.IsIdentChar(const c: TUTF8Char): boolean;
begin
  Result:=(length(c)=1) and (c[1] in IdentChars);
end;

procedure TCustomSynEdit.GetWordBoundsAtRowCol(const XY: TPoint; var StartX,
  EndX: integer); // all params are logical (byte) positions
var
  Line: string;
begin
  StartX:=XY.X;
  EndX:=XY.X;
  Line := FTheLinesView[XY.Y - 1];
  if WordBreaker.IsInWord(Line, XY.X) then begin
    StartX := WordBreaker.PrevWordStart(Line, XY.X, True);
    EndX := WordBreaker.NextWordEnd(Line, XY.X, True);
  end;
end;

function TCustomSynEdit.GetWordAtRowCol(XY: TPoint): string;
var
  StartX, EndX: integer;
  Line: string;
begin
  GetWordBoundsAtRowCol(XY, StartX, EndX);
  Line := FTheLinesView[XY.Y - 1];
  Result := Copy(Line, StartX, EndX - StartX);
end;

function TCustomSynEdit.NextTokenPos: TPoint;
var
  CX, CY, LineLen: integer;
  Line: string;
  CurIdentChars, WhiteChars: TSynIdentChars;
  nTokenPos, nTokenLen: integer;
  sToken: PChar;
  LogCaret: TPoint;

  procedure FindFirstNonWhiteSpaceCharInNextLine;
  begin
    if CY < FTheLinesView.Count then begin
      Line := FTheLinesView[CY];
      LineLen := Length(Line);
      Inc(CY);
      CX:=1;
      while (CX<=LineLen) and (Line[CX] in WhiteChars) do inc(CX);
      if CX>LineLen then CX:=1;
    end;
  end;

begin
  LogCaret:=LogicalCaretXY;
  CX := LogCaret.X;
  CY := LogCaret.Y;
  // valid line?
  if (CY >= 1) and (CY <= FTheLinesView.Count) then begin
    Line := FTheLinesView[CY - 1];
    LineLen := Length(Line);
    WhiteChars := FWordBreaker.WhiteChars;
    if CX > LineLen then begin
      FindFirstNonWhiteSpaceCharInNextLine;
    end else begin
      if fHighlighter<>nil then begin
        fHighlighter.CurrentLines := FTheLinesView;
        fHighlighter.StartAtLineIndex(CY - 1);
        while not fHighlighter.GetEol do begin
          nTokenPos := fHighlighter.GetTokenPos; // zero-based
          fHighlighter.GetTokenEx(sToken,nTokenLen);
          if (CX>nTokenPos) and (CX<=nTokenPos+nTokenLen) then begin
            CX:=nTokenPos+nTokenLen+1;
            break;
          end;
          // Let the highlighter scan the next token.
          fHighlighter.Next;
        end;
        if fHighlighter.GetEol then
          FindFirstNonWhiteSpaceCharInNextLine;
      end else begin
        // no highlighter
        CurIdentChars:=IdentChars;
        // find first "whitespace" if next char is not a "whitespace"
        if (Line[CX] in CurIdentChars) then begin
          // in a word -> move to end of word
          while (CX<=LineLen) and (Line[CX] in CurIdentChars) do inc(CX);
        end;
        if (Line[CX] in WhiteChars) then begin
          // skip white space
          while (CX<=LineLen) and (Line[CX] in WhiteChars) do inc(CX);
        end;
        // delete at least one char
        if (CX=CaretX) then inc(CX);
      end;
    end;
  end;
  Result := LogicalToPhysicalPos(Point(CX, CY));
end;

function TCustomSynEdit.NextWordPos: TPoint;
begin
  Result := LogicalToPhysicalPos(NextWordLogicalPos);
end;

function TCustomSynEdit.PrevWordPos: TPoint;
begin
  Result := LogicalToPhysicalPos(PrevWordLogicalPos);
end;

function TCustomSynEdit.FindHookedCmdEvent(AHandlerProc: THookedCommandEvent):
  integer;
var
  Entry: THookedCommandHandlerEntry;
begin
  Result := GetHookedCommandHandlersCount - 1;
  while Result >= 0 do begin
    Entry := THookedCommandHandlerEntry(fHookedCommandHandlers[Result]);
    if Entry.Equals(AHandlerProc) then
      break;
    Dec(Result);
  end;
end;

function TCustomSynEdit.GetHookedCommandHandlersCount: integer;
begin
  if Assigned(fHookedCommandHandlers) then
    Result := fHookedCommandHandlers.Count
  else
    Result := 0;
end;

procedure TCustomSynEdit.RegisterCommandHandler(AHandlerProc:
  THookedCommandEvent; AHandlerData: pointer);
begin
  if not Assigned(AHandlerProc) then begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in RegisterCommandHandler');
{$ENDIF}
    exit;
  end;
  if not Assigned(fHookedCommandHandlers) then
    fHookedCommandHandlers := TList.Create;
  if FindHookedCmdEvent(AHandlerProc) = -1 then
    fHookedCommandHandlers.Add(THookedCommandHandlerEntry.Create(
      AHandlerProc, AHandlerData))
  else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) already registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
end;

procedure TCustomSynEdit.UnregisterCommandHandler(AHandlerProc:
  THookedCommandEvent);
var
  i: integer;
begin
  if not Assigned(AHandlerProc) then begin
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.Create('Event handler is NIL in UnregisterCommandHandler');
{$ENDIF}
    exit;
  end;
  i := FindHookedCmdEvent(AHandlerProc);
  if i > -1 then begin
    THookedCommandHandlerEntry(fHookedCommandHandlers[i]).Free;
    fHookedCommandHandlers.Delete(i);
  end else
{$IFDEF SYN_DEVELOPMENT_CHECKS}
    raise Exception.CreateFmt('Event handler (%p, %p) is not registered',
      [TMethod(AHandlerProc).Data, TMethod(AHandlerProc).Code]);
{$ENDIF}
end;

procedure TCustomSynEdit.RegisterMouseActionSearchHandler(AHandlerProc: TSynEditMouseActionSearchProc);
begin
  FMouseActionSearchHandlerList.Add(TMethod(AHandlerProc));
end;

procedure TCustomSynEdit.UnregisterMouseActionSearchHandler(AHandlerProc: TSynEditMouseActionSearchProc);
begin
  FMouseActionSearchHandlerList.Remove(TMethod(AHandlerProc));
end;

procedure TCustomSynEdit.RegisterMouseActionExecHandler(AHandlerProc: TSynEditMouseActionExecProc);
begin
  FMouseActionExecHandlerList.Add(TMethod(AHandlerProc));
end;

procedure TCustomSynEdit.UnregisterMouseActionExecHandler(AHandlerProc: TSynEditMouseActionExecProc);
begin
  FMouseActionExecHandlerList.Remove(TMethod(AHandlerProc));
end;

procedure TCustomSynEdit.RegisterKeyTranslationHandler(AHandlerProc: THookedKeyTranslationEvent);
begin
  FHookedKeyTranslationList.Add(TMEthod(AHandlerProc));
end;

procedure TCustomSynEdit.UnRegisterKeyTranslationHandler(AHandlerProc: THookedKeyTranslationEvent);
begin
  FHookedKeyTranslationList.Remove(TMEthod(AHandlerProc));
end;

procedure TCustomSynEdit.RegisterStatusChangedHandler(AStatusChangeProc: TStatusChangeEvent;
  AChanges: TSynStatusChanges);
begin
  TSynStatusChangedHandlerList(FStatusChangedList).Add(AStatusChangeProc, AChanges);
end;

procedure TCustomSynEdit.UnRegisterStatusChangedHandler(AStatusChangeProc: TStatusChangeEvent);
begin
  TSynStatusChangedHandlerList(FStatusChangedList).Remove(AStatusChangeProc);
end;

procedure TCustomSynEdit.NotifyHookedCommandHandlers(AfterProcessing: boolean;
  var Command: TSynEditorCommand;
  var AChar: TUTF8Char; Data: pointer);
var
  Handled: boolean;
  i: integer;
  Entry: THookedCommandHandlerEntry;
begin
  Handled := FALSE;
  for i := 0 to GetHookedCommandHandlersCount - 1 do begin
    Entry := THookedCommandHandlerEntry(fHookedCommandHandlers[i]);
    // NOTE: Command should NOT be set to ecNone, because this might interfere
    // with other handlers.  Set Handled to False instead (and check its value
    // to not process the command twice).
    Entry.fEvent(Self, AfterProcessing, Handled, Command, AChar, Data,
      Entry.fData);
  end;
  if Handled then
    Command := ecNone;
end;

procedure TCustomSynEdit.DoOnPaint;
begin
  if Assigned(fOnPaint) then begin
    Canvas.Font.Assign(Font);
    Canvas.Brush.Color := Color;
    fOnPaint(Self, Canvas);
  end;
end;

function TCustomSynEdit.DoOnReplaceText(const ASearch, AReplace: string;
  Line, Column: integer): TSynReplaceAction;
begin
  Result := raCancel;
  if Assigned(fOnReplaceText) then
    fOnReplaceText(Self, ASearch, AReplace, Line, Column, Result);
end;

procedure TCustomSynEdit.DoOnStatusChange(Changes: TSynStatusChanges);
begin
  TSynStatusChangedHandlerList(FStatusChangedList).CallStatusChangedHandlers(Self, Changes);
  if Assigned(fOnStatusChange) then begin
    fOnStatusChange(Self, fStatusChanges);
    fStatusChanges := [];
  end;
end;

procedure TCustomSynEdit.UndoRedoAdded(Sender: TObject);
begin
  // Todo: Check Paintlock, otherwise move to LinesChanged, LineCountChanged
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

procedure TCustomSynEdit.ModifiedChanged(Sender: TObject); 
begin
  StatusChanged([scModified]);
end;

function TCustomSynEdit.LogicalToPhysicalPos(const p: TPoint): TPoint;
begin
  Result := FTheLinesView.LogicalToPhysicalPos(p);
end;

function TCustomSynEdit.LogicalToPhysicalCol(const Line: String;
  Index, LogicalPos: integer): integer;
// LogicalPos is 1-based
// Index 0-based LineNumber
begin
  Result := FTheLinesView.LogicalToPhysicalCol(Line, Index,
                                                                LogicalPos);
end;

function TCustomSynEdit.PhysicalLineLength(Line: String; Index: integer): integer;
begin
  Result:=LogicalToPhysicalCol(Line, Index, length(Line)+1) - 1
end;

function TCustomSynEdit.PhysicalToLogicalPos(const p: TPoint): TPoint;
begin
  Result := FTheLinesView.PhysicalToLogicalPos(p);
end;

function TCustomSynEdit.PhysicalToLogicalCol(const Line: string;
  Index, PhysicalPos: integer): integer;
begin
  Result := FTheLinesView.PhysicalToLogicalCol(Line, Index,
                                                                PhysicalPos);
end;

function TCustomSynEdit.ScreenColumnToXValue(Col : integer) : integer;
begin
  Result := fTextOffset + Pred(Col) * fCharWidth;
end;

procedure TCustomSynEdit.PrimarySelectionRequest(
  const RequestedFormatID: TClipboardFormat;  Data: TStream);
var
  s: string;
  ClipHelper: TSynClipboardStream;
begin
  if (not SelAvail) then exit;
  s:=SelText;
  if s = ''  then
    exit;
  if RequestedFormatID = CF_TEXT then begin
    Data.Write(s[1],length(s));
  end
  else
  if RequestedFormatID = TSynClipboardStream.ClipboardFormatId then begin
    ClipHelper := TSynClipboardStream.Create;
    try
      ClipHelper.SelectionMode := SelectionMode;
      // InternalText, so we don't need a 2nd call for CF_TEXT
      ClipHelper.InternalText := s;
      // Fold
      if eoFoldedCopyPaste in fOptions2 then
        s := FFoldedLinesView.GetFoldDescription(
          FBlockSelection.FirstLineBytePos.Y - 1, FBlockSelection.FirstLineBytePos.X,
          FBlockSelection.LastLineBytePos.Y - 1,  FBlockSelection.LastLineBytePos.X);
      if length(s) > 0 then
        ClipHelper.AddTag(synClipTagFold, @s[1], length(s));
      Data.Write(ClipHelper.Memory^, ClipHelper.Size);
    finally
      ClipHelper.Free;
    end;
  end;
end;

{ TSynEditPlugin }

constructor TSynEditPlugin.Create(AOwner: TComponent);
begin
  if AOwner is TCustomSynEdit then begin
    inherited Create(nil);
    Editor := TCustomSynEdit(AOwner);
  end
  else
    inherited Create(AOwner);
end;

destructor TSynEditPlugin.Destroy;
begin
  Editor := nil;
  inherited Destroy;
end;

procedure TSynEditPlugin.SetEditor(const AValue: TCustomSynEdit);
begin
  if AValue = FriendEdit then exit;
  if (FriendEdit <> nil) and (Editor.fPlugins <> nil) then
    Editor.fPlugins.Remove(Self);
  FriendEdit := AValue;
  if FriendEdit <> nil then
    Editor.fPlugins.Add(Self);
end;

function TSynEditPlugin.GetEditor: TCustomSynEdit;
begin
  Result := FriendEdit as TCustomSynEdit;
end;

function TSynEditPlugin.OwnedByEditor: Boolean;
begin
  Result := Owner = nil;
end;

procedure Register;
begin
  RegisterClasses([TSynGutterPartList, TSynRightGutterPartList,
                   TSynGutterSeparator, TSynGutterCodeFolding,
                   TSynGutterLineNumber, TSynGutterChanges, TSynGutterMarks,
                   TSynGutterLineOverview]);

  RegisterPropertyToSkip(TSynSelectedColor, 'OnChange', '', '');
  RegisterPropertyToSkip(TSynSelectedColor, 'StartX', '', '');
  RegisterPropertyToSkip(TSynSelectedColor, 'EndX', '', '');

  RegisterPropertyToSkip(TSynGutter, 'ShowCodeFolding', '', '');
  RegisterPropertyToSkip(TSynGutter, 'CodeFoldingWidth', '', '');
  RegisterPropertyToSkip(TSynGutter, 'ShowChanges', '', '');
  RegisterPropertyToSkip(TSynGutter, 'ShowLineNumbers', '', '');
  RegisterPropertyToSkip(TSynGutter, 'ShowOnlyLineNumbersMultiplesOf', '', '');
  RegisterPropertyToSkip(TSynGutter, 'ZeroStart', '', '');
  RegisterPropertyToSkip(TSynGutter, 'MarkupInfoLineNumber', '', '');
  RegisterPropertyToSkip(TSynGutter, 'MarkupInfoModifiedLine', '', '');
  RegisterPropertyToSkip(TSynGutter, 'MarkupInfoCodeFoldingTree', '', '');
  RegisterPropertyToSkip(TSynGutter, 'LeadingZeros', '', '');
  RegisterPropertyToSkip(TSynGutter, 'DigitCount', '', '');
  RegisterPropertyToSkip(TSynGutter, 'AllowSkipGutterSeparatorDraw', '', '');
  RegisterPropertyToSkip(TSynGutter, 'GutterParts', '', '');
  RegisterPropertyToSkip(TSynGutter, 'OnChange', '', '');
  RegisterPropertyToSkip(TSynEdit, 'CFDividerDrawLevel', '', '');
end;

{ TSynHookedKeyTranslationList }

procedure TSynHookedKeyTranslationList.CallHookedKeyTranslationHandlers(Sender: TObject;
  Code: word; SState: TShiftState; var Data: pointer; var IsStartOfCombo: boolean;
  var Handled: boolean; var Command: TSynEditorCommand;
  var ComboKeyStrokes: TSynEditKeyStrokes);
var
  i: Integer;
begin
  // Finish Combo ?
  for i := 0 to Count - 1 do
    THookedKeyTranslationEvent(Items[i])(Sender, Code, SState, Data,
      IsStartOfCombo, Handled, Command, True, ComboKeyStrokes);
  if Handled then
    exit;
  // New Stroke ?
  for i := 0 to Count - 1 do
    THookedKeyTranslationEvent(Items[i])(Sender, Code, SState, Data,
      IsStartOfCombo, Handled, Command, False, ComboKeyStrokes);
end;

{ TSynStatusChangedHandlerList }
procedure TSynStatusChangedHandlerList.Add(AHandler: TStatusChangeEvent;
  Changes: TSynStatusChanges);
begin
  AddBitFilter(TMethod(AHandler), LongInt(Changes));
end;

procedure TSynStatusChangedHandlerList.Remove(AHandler: TStatusChangeEvent);
begin
  inherited Remove(TMethod(AHandler));
end;

procedure TSynStatusChangedHandlerList.CallStatusChangedHandlers(Sender: TObject;
  Changes: TSynStatusChanges);
var
  i: Integer;
begin
  i:=Count;
  while NextDownIndexBitFilter(i, LongInt(Changes)) do
    TStatusChangeEvent(FItems[i].FHandler)(Sender, Changes);
end;

{ TSynEditMarkListInternal }

function TSynEditMarkListInternal.GetLinesView: TSynEditStrings;
begin
  Result := FLines;
end;

procedure TSynEditMarkListInternal.SetLinesView(const AValue: TSynEditStrings);
begin
  FLines := AValue;
end;

procedure TSynEditMarkListInternal.AddOwnerEdit(AEdit: TSynEditBase);
begin
  FOwnerList.Add(AEdit);
end;

procedure TSynEditMarkListInternal.RemoveOwnerEdit(AEdit: TSynEditBase);
begin
  FOwnerList.Remove(AEdit);
end;

initialization
  InitSynDefaultFont;
  Register;

end.
