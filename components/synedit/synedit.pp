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
{$DEFINE EnableDoubleBuf} // gtk2 paints faster to memory
{$ENDIF}

interface

{ $DEFINE VerboseKeys}

uses
{$IFDEF SYN_LAZARUS}
  {$IFDEF USE_UTF8BIDI_LCL}
  FreeBIDI, utf8bidi,
  {$ENDIF}
  Types, FPCAdds, LCLIntf, LCLType, LMessages, LCLProc,
{$ELSE}
  Windows,
{$ENDIF}
  {$IFDEF DebugCodeFolding}
  Dialogs,
  {$ENDIF}
  SysUtils, Classes, Messages, Controls, Graphics, Forms, StdCtrls, ExtCtrls,
{$IFDEF SYN_MBCSSUPPORT}
  Imm,
{$ENDIF}
  SynEditTypes, SynEditSearch, SynEditKeyCmds, SynEditMiscProcs,
  SynEditMiscClasses, SynEditTextBuffer, SynEditHighlighter, SynTextDrawer;

const
  DIGIT = ['0'..'9'];
// ALPHA            = ['A'..'Z', 'a'..'z'];
// break these up because we exceed the 4 byte limit when combined.
  ALPHA_UC = ['A'..'Z'];
  ALPHA_LC = ['a'..'z'];

{$IFDEF SYN_LAZARUS}
ScrollBarWidth=0;
{$ENDIF}

{$IFNDEF SYN_COMPILER_3_UP}
   // not defined in all Delphi versions
  WM_MOUSEWHEEL = $020A;
{$ENDIF}

   // maximum scroll range
  MAX_SCROLL = 32767;

// Max number of book/gutter marks returned from GetEditMarksForLine - that
// really should be enough.
  maxMarks = 16;

  SYNEDIT_CLIPBOARD_FORMAT = 'SynEdit Control Block Type';

{$IFNDEF SYN_LAZARUS}
var
  SynEditClipboardFormat: UINT;
{$ENDIF}

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
  TSynReplaceAction = (raCancel, raSkip, raReplace, raReplaceAll);

  ESynEditError = class(Exception);

  TDropFilesEvent = procedure(Sender: TObject; X, Y: integer; AFiles: TStrings)
    of object;

  THookedCommandEvent = procedure(Sender: TObject; AfterProcessing: boolean;
    var Handled: boolean; var Command: TSynEditorCommand;
    var AChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
    Data: pointer; HandlerData: pointer) of object;

  TPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  TProcessCommandEvent = procedure(Sender: TObject;
    var Command: TSynEditorCommand;
    var AChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
    Data: pointer) of object;

  TReplaceTextEvent = procedure(Sender: TObject; const ASearch, AReplace:
    string; Line, Column: integer; var ReplaceAction: TSynReplaceAction) of object;

  TSpecialLineColorsEvent = procedure(Sender: TObject; Line: integer;
    var Special: boolean; var FG, BG: TColor) of object;

  TSynEditCaretType = (ctVerticalLine, ctHorizontalLine, ctHalfBlock, ctBlock);

  TSynStateFlag = (sfCaretChanged, sfScrollbarChanged, sfLinesChanging,
    sfIgnoreNextChar, sfCaretVisible, sfDblClicked, sfPossibleGutterClick,
    {$IFDEF SYN_LAZARUS}
    sfTripleClicked, sfQuadClicked, sfPainting,
    {$ENDIF}
    sfWaitForDragging, sfInsideRedo
    );                                           //mh 2000-10-30
  TSynStateFlags = set of TSynStateFlag;

  TSynEditorOption = (
    eoAltSetsColumnMode,       // Holding down the Alt Key will put the selection mode into columnar format
    eoAutoIndent,              // Will indent the caret on new lines with the same amount of leading white space as the preceding line
    eoAutoSizeMaxScrollWidth,  //TODO Automatically resizes the MaxScrollWidth property when inserting text
    eoDisableScrollArrows,     //TODO Disables the scroll bar arrow buttons when you can't scroll in that direction any more
    eoDragDropEditing,         // Allows you to select a block of text and drag it within the document to another location
    eoDropFiles,               //TODO Allows the editor accept file drops
    eoEnhanceHomeKey,          // home key jumps to line start if nearer, similar to visual studio
    eoGroupUndo,               // When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
    eoHalfPageScroll,          // When scrolling with page-up and page-down commands, only scroll a half page at a time
    eoHideShowScrollbars,      //TODO if enabled, then the scrollbars will only show when necessary.  If you have ScrollPastEOL, then it the horizontal bar will always be there (it uses MaxLength instead)
    eoKeepCaretX,              // When moving through lines w/o Cursor Past EOL, keeps the X position of the cursor
    eoNoCaret,                 // Makes it so the caret is never visible
    eoNoSelection,             // Disables selecting text
    eoRightMouseMovesCursor,   // When clicking with the right mouse for a popup menu, move the cursor to that location
    eoScrollByOneLess,         // Forces scrolling to be one less
    eoScrollHintFollows,       //TODO The scroll hint follows the mouse when scrolling vertically
    eoScrollPastEof,           // Allows the cursor to go past the end of file marker
    eoScrollPastEol,           // Allows the cursor to go past the last character into the white space at the end of a line
    eoShowScrollHint,          // Shows a hint of the visible line numbers when scrolling vertically
    eoShowSpecialChars,        //TODO Shows the special Characters
    eoSmartTabDelete,          //TODO similar to Smart Tabs, but when you delete characters
    eoSmartTabs,               // When tabbing, the cursor will go to the next non-white space character of the previous line
    //eoSpecialLineDefaultFg,    //TODO disables the foreground text color override when using the OnSpecialLineColor event
    eoTabIndent,               // When active <Tab> and <Shift><Tab> act as block indent, unindent when text is selected
    eoTabsToSpaces,            // Converts a tab character to a specified number of space characters
    eoTrimTrailingSpaces,      // Spaces at the end of lines will be trimmed and not saved
    {$IFDEF SYN_LAZARUS}
    eoBracketHighlight,        // Highlight matching bracket
    eoDoubleClickSelectsLine,  // Select line on double click
    eoHideRightMargin,         // Hides the right margin line
    eoPersistentCaret,         // Do not hide caret when focus lost
    eoShowCtrlMouseLinks,      // Pressing Ctrl will highlight the word under the mouse cursor
    eoAutoIndentOnPaste,       // Indent text inserted from clipboard
    eoSpacesToTabs             // Converts space characters to tabs and spaces
    {$ENDIF}
    );
  TSynEditorOptions = set of TSynEditorOption;
  
  {$IFDEF SYN_LAZARUS}
  TSynEditorOption2 = (
    eoCaretSkipsSelection,     // Caret skips selection on VK_LEFT/VK_RIGHT
    eoAlwaysVisibleCaret       // Move caret to be always visible when scrolling
  );
  TSynEditorOptions2 = set of TSynEditorOption2;
  {$ENDIF}

const
  SYNEDIT_DEFAULT_OPTIONS = [
    eoAutoIndent,
    eoDragDropEditing,
    eoScrollPastEol,
    eoShowScrollHint,
    eoSmartTabs,
    eoTabsToSpaces,
    eoTrimTrailingSpaces,
    eoSmartTabDelete,
    eoGroupUndo,
    {$IFDEF SYN_LAZARUS}
    eoBracketHighlight
    {$ENDIF}
    ];

  {$IFDEF SYN_LAZARUS}
  SYNEDIT_DEFAULT_OPTIONS2 = [
    ];
  {$ENDIF}

type
// use scAll to update a statusbar when another TCustomSynEdit got the focus
  TSynStatusChange = (scAll, scCaretX, scCaretY, scLeftChar, scTopLine,
    scInsertMode, scModified, scSelection, scReadOnly);
  TSynStatusChanges = set of TSynStatusChange;

  TStatusChangeEvent = procedure(Sender: TObject; Changes: TSynStatusChanges)
    of object;

  TCustomSynEdit = class;

  TSynEditMark = class
  protected
    fLine, fColumn, fImage: Integer;
    fEdit: TCustomSynEdit;
    fVisible: boolean;
    fInternalImage: boolean;
    fBookmarkNum: integer;
    function GetEdit: TCustomSynEdit; virtual;
    procedure SetColumn(const Value: Integer); virtual;
    procedure SetImage(const Value: Integer); virtual;
    procedure SetLine(const Value: Integer); virtual;
    procedure SetVisible(const Value: boolean); {$IFDEF SYN_LAZARUS}virtual;{$ENDIF} //MWE: Laz needs to know when a line gets visible, so the editor color can be updated
    procedure SetInternalImage(const Value: boolean);
    function GetIsBookmark: boolean;
  public
    constructor Create(AOwner: TCustomSynEdit);
    property Line: integer read fLine write SetLine;
    property Column: integer read fColumn write SetColumn;
    property ImageIndex: integer read fImage write SetImage;
    property BookmarkNumber: integer read fBookmarkNum write fBookmarkNum;
    property Visible: boolean read fVisible write SetVisible;
    property InternalImage: boolean read fInternalImage write SetInternalImage;
    property IsBookmark: boolean read GetIsBookmark;
  end;

  TPlaceMarkEvent = procedure(Sender: TObject; var Mark: TSynEditMark)
    of object;

  TSynEditMarks = array[1..maxMarks] of TSynEditMark;

  { A list of mark objects. Each object cause a litle picture to be drawn in the
    gutter. }
  TSynEditMarkList = class(TList)
  protected
    fEdit: TCustomSynEdit;
    fOnChange: TNotifyEvent;
    procedure DoChange;
    function Get(Index: Integer): TSynEditMark;
    procedure Put(Index: Integer; Item: TSynEditMark);
  public
    constructor Create(AOwner: TCustomSynEdit);
    destructor Destroy; override;
    function Add(Item: TSynEditMark): Integer;
    procedure ClearLine(line: integer);
    procedure Delete(Index: Integer);
    function First: TSynEditMark;
    procedure GetMarksForLine(line: integer; var Marks: TSynEditMarks);
    procedure Insert(Index: Integer; Item: TSynEditMark);
    function Last: TSynEditMark;
    procedure Place(Mark: TSynEditMark);
    function Remove(Item: TSynEditMark): Integer;
  public
    property Items[Index: Integer]: TSynEditMark read Get write Put; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGutterClickEvent = procedure(Sender: TObject; X, Y, Line: integer;
    mark: TSynEditMark) of object;

  TSynEditPlugin = class(TObject)
  private
    fOwner: TCustomSynEdit;
  protected
    procedure AfterPaint(ACanvas: TCanvas; AClip: TRect;
      FirstLine, LastLine: integer); virtual; abstract;
    procedure LinesInserted(FirstLine, Count: integer); virtual; abstract;
    procedure LinesDeleted(FirstLine, Count: integer); virtual; abstract;
  protected
    property Editor: TCustomSynEdit read fOwner;                                //mh 2000-11-10
  public
    constructor Create(AOwner: TCustomSynEdit);
    destructor Destroy; override;
  end;

  {$IFDEF SYN_LAZARUS}
  // aIndex parameters of Line notifications are 0-based.
  TSynCustomLineIndenter = class(TComponent)
  private
    FEditor: TCustomSynEdit;
    procedure SetEditor(const AValue: TCustomSynEdit);
  public
    destructor Destroy; override;
    // plugin notifications
    procedure LinesInserted(aIndex: integer; aCount: integer);
    procedure LinesDeleted(aIndex: integer; aCount: integer);
    procedure LinesPutted(aIndex: integer; aCount: integer);
    // pretty clear, heh?
    procedure Reset;
    // indentation
    function GetLineIndentProposal(aIndex: integer;
                                   IgnoreCurrentLineText: boolean): integer;
  public
    property Editor: TCustomSynEdit read FEditor write SetEditor;
  end;
  {$ENDIF}

  { TCustomSynEdit }

  TCustomSynEdit = class(TCustomControl)
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
    {$IFDEF SYN_LAZARUS}
    procedure WMExit(var Message: TLMExit); message LM_EXIT;
    procedure Resize; override;
    {$ELSE}
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    {$ENDIF}
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMVScroll(var Msg: {$IFDEF SYN_LAZARUS}TLMScroll{$ELSE}TWMScroll{$ENDIF}); message WM_VSCROLL;
  private
    fFirstLine: integer;
    fBlockBegin: TPoint;   // logical position (byte)
    fBlockEnd: TPoint;     // logical position (byte)
    fBlockIndent: integer;
    fCaretX: Integer;      // physical position (screen)
    {$IFDEF SYN_LAZARUS}
    fBracketHighlightCaret: TPoint;
    fBracketHighlightPos: TPoint;
    fBracketHighlightAntiPos: TPoint;
    fCtrlMouseActive: boolean;
    fLineIndenter: TSynCustomLineIndenter;
    FCFDividerDrawLevel: Integer;
    {$ENDIF}
    fLastCaretX: integer;  // physical position (screen)                        //mh 2000-10-19
    fCaretY: Integer;
    fCharsInWindow: Integer;
    fCharWidth: Integer;
    fFontDummy: TFont;
{$IFDEF SYN_MBCSSUPPORT}
    fImeCount: Integer;
    fMBCSStepAside: Boolean;
{$ENDIF}
    fInserting: Boolean;
    {$IFDEF SYN_LAZARUS}
    fLastMouseCaret: TPoint;  // physical (screen)
    fLastControlIsPressed: boolean;
    fLastCtrlMouseLinkY: integer;
    fLastCtrlMouseLinkX1: integer; // logical (byte)
    fLastCtrlMouseLinkX2: integer; // logical (byte)
    fHighlighterNeedsUpdateStartLine: integer; // 1 based, 0 means invalid
    fHighlighterNeedsUpdateEndLine: integer; // 1 based, 0 means invalid
    {$ENDIF}
    fLines: TStrings;
    fLinesInWindow: Integer;
    fLeftChar: Integer;    // first visible screen column
    fMaxLeftChar: Integer; // 1024
    fPaintLock: Integer;
    fReadOnly: Boolean;
    fRightEdge: Integer;
    fRightEdgeColor: TColor;
    FScrollBars: TScrollStyle;
    FTabChar: char;
    fTextHeight: Integer;
    fTextOffset: Integer;
    fTopLine: Integer;
    fHighlighter: TSynCustomHighlighter;
    fSelectedColor: TSynSelectedColor;
    fUndoList: TSynEditUndoList;
    fRedoList: TSynEditUndoList;
    fBookMarks: array[0..9] of TSynEditMark;
    fMouseDownX: integer;
    fMouseDownY: integer;
    fBookMarkOpt: TSynBookMarkOpt;
{$ifndef SYN_LAZARUS}
    fBorderStyle: TBorderStyle;
    fMouseWheelAccumulator: integer;
{$endif}
    fHideSelection: boolean;
    fOverwriteCaret: TSynEditCaretType;
    fInsertCaret: TSynEditCaretType;
    fCaretOffset: TPoint;
    fKeyStrokes: TSynEditKeyStrokes;
    fModified: Boolean;
    fMarkList: TSynEditMarkList;
    fExtraLineSpacing: integer;
    fSelectionMode: TSynSelectionMode;
    FUseUTF8: boolean;
    fWantTabs: boolean;
    fGutter: TSynGutter;
    fTabWidth: integer;
    fTextDrawer: TheTextDrawer;
    fInvalidateRect: TRect;
    fStateFlags: TSynStateFlags;
    fOptions: TSynEditorOptions;
    {$IFDEF SYN_LAZARUS}
    fOptions2: TSynEditorOptions2;
    {$ENDIF}
    fStatusChanges: TSynStatusChanges;
    fLastKey: word;
    fLastShiftState: TShiftState;
    fTSearch: TSynEditSearch;
    fHookedCommandHandlers: TList;
    fPlugins: TList;
    fScrollTimer: TTimer;
    fScrollDeltaX, fScrollDeltaY: Integer;
    // event handlers
    fOnChange: TNotifyEvent;
    fOnClearMark: TPlaceMarkEvent;                                              // djlp 2000-08-29
    fOnCommandProcessed: TProcessCommandEvent;
    fOnDropFiles: TDropFilesEvent;
    fOnGutterClick: TGutterClickEvent;
    fOnPaint: TPaintEvent;
    fOnPlaceMark: TPlaceMarkEvent;
    fOnProcessCommand: TProcessCommandEvent;
    fOnProcessUserCommand: TProcessCommandEvent;
    fOnReplaceText: TReplaceTextEvent;
    fOnSpecialLineColors: TSpecialLineColorsEvent;
    fOnStatusChange: TStatusChangeEvent;

    {$IFDEF SYN_LAZARUS}
    procedure AquirePrimarySelection;
    {$ENDIF}
    procedure BookMarkOptionsChanged(Sender: TObject);
    procedure ComputeCaret(X, Y: Integer);
    procedure DoBlockIndent;
    procedure DoBlockUnindent;
    procedure DoHomeKey(Selection: boolean);
    procedure DoLinesDeleted(FirstLine, Count: integer);
    procedure DoLinesInserted(FirstLine, Count: integer);
    procedure DoTabKey;
    function FindHookedCmdEvent(AHandlerProc: THookedCommandEvent): integer;
    procedure FontChanged(Sender: TObject); {$IFDEF SYN_LAZARUS}override;{$ENDIF}
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretXY: TPoint;
    function GetFont: TFont;
    function GetHookedCommandHandlersCount: integer;
    function GetLineText: string;
    {$IFDEF SYN_LAZARUS}
    function GetCharLen(const Line: string; CharStartPos: integer): integer;
    function AdjustBytePosToCharacterStart(Line: integer; BytePos: integer): integer;
    function AdjustPhysPosToCharacterStart(Line: integer; PhysPos: integer): integer;
    function GetLogicalCaretXY: TPoint;
    procedure SetCFDividerDrawLevel(const AValue: Integer);
    procedure SetLineIndenter(const AValue: TSynCustomLineIndenter);
    procedure SetLogicalCaretXY(const NewLogCaretXY: TPoint);
    {$ENDIF}
    function GetMaxUndo: Integer;
    function GetSelAvail: Boolean;
    function GetSelText: string;
    function SynGetText: string;
    {$IFDEF SYN_LAZARUS}
    procedure SetTabChar(const AValue: Char);
    function RealGetText: TCaption; override;
    {$ENDIF}
    procedure GutterChanged(Sender: TObject);
    procedure InsertBlock(BB, BE: TPoint; ChangeStr: PChar);
    function IsPointInSelection(Value: TPoint): boolean;
    function LeftSpaces(const Line: string): Integer;
    {$IFDEF SYN_LAZARUS}
    function LeftSpaces(const Line: string; Physical: boolean): Integer;
    {$ENDIF}
    procedure LinesChanging(Sender: TObject);
    procedure LinesChanged(Sender: TObject);
    procedure LockUndo;
    procedure MoveCaretAndSelection(
      {$IFDEF SYN_LAZARUS}const {$ENDIF}ptBefore, ptAfter: TPoint;
      SelectionCommand: boolean);
    {$IFDEF SYN_LAZARUS}
    procedure MoveCaretAndSelectionPhysical(
      const ptBeforePhysical, ptAfterPhysical: TPoint;
      SelectionCommand: boolean);
    {$ENDIF}
    procedure MoveCaretHorz(DX: integer; SelectionCommand: boolean);
    procedure MoveCaretVert(DY: integer; SelectionCommand: boolean);
    procedure PluginsAfterPaint(ACanvas: TCanvas; AClip: TRect;
      FirstLine, LastLine: integer);
    {$IFDEF SYN_LAZARUS}
    procedure PrimarySelectionRequest(const RequestedFormatID: TClipboardFormat;
      Data: TStream);
    {$ENDIF}
    function ScanFrom(Index: integer
          {$IFDEF SYN_LAZARUS}; AtLeastTilIndex: integer = -1{$ENDIF}): integer;
    procedure ScrollTimerHandler(Sender: TObject);
    procedure SelectedColorsChanged(Sender: TObject);
    procedure SetBlockBegin(Value: TPoint);
    procedure SetBlockEnd(Value: TPoint);
    {$IFDEF SYN_LAZARUS}
    procedure SetBlockIndent(const AValue: integer);
    {$ELSE}
    procedure SetBorderStyle(Value: TBorderStyle);
    {$ENDIF}
    procedure SetCaretAndSelection({$IFDEF SYN_LAZARUS}const {$ENDIF}ptCaret,
                                   ptBefore, ptAfter: TPoint);
    procedure SetCaretX(Value: Integer);
    procedure SetCaretY(Value: Integer);
    procedure SetExtraLineSpacing(const Value: integer);
    procedure SetFont(const Value: TFont);
    procedure SetGutter(const Value: TSynGutter);
    procedure SetGutterWidth(Value: Integer);
    procedure SetHideSelection(const Value: boolean);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetInsertCaret(const Value: TSynEditCaretType);
    procedure SetInsertMode(const Value: boolean);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    {$ifdef SYN_LAZARUS}
    procedure SetLastMouseCaret(const AValue: TPoint);
    {$ENDIF}
    procedure SetLeftChar(Value: Integer);
    procedure SetLines(Value: TStrings);
    procedure SetLineText(Value: string);
    procedure SetMaxLeftChar(Value: integer);
    procedure SetMaxUndo(const Value: Integer);
    procedure SetModified(Value: boolean);
    procedure SetOptions(Value: TSynEditorOptions);
    {$IFDEF SYN_LAZARUS}
    procedure SetOptions2(const Value: TSynEditorOptions2);
    {$ENDIF}
    procedure SetOverwriteCaret(const Value: TSynEditCaretType);
    procedure SetRightEdge(Value: Integer);
    procedure SetRightEdgeColor(Value: TColor);
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetSelectionMode(const Value: TSynSelectionMode);
    procedure SetSelText(const Value: string);
    procedure SetSelTextExternal(const Value: string);
    procedure SetTabWidth(Value: integer);
    procedure SynSetText(const Value: string);
    {$IFDEF SYN_LAZARUS}
    procedure RealSetText(const Value: TCaption); override;
    {$ENDIF}
    procedure SetTopLine(Value: Integer);
    procedure SetWantTabs(const Value: boolean);
    procedure SetWordBlock(Value: TPoint);
    {$IFDEF SYN_LAZARUS}
    procedure SetLineBlock(Value: TPoint);
    procedure SetParagraphBlock(Value: TPoint);
    {$ENDIF}
    procedure SizeOrFontChanged(bFont: boolean);
    procedure StatusChanged(AChanges: TSynStatusChanges);
    procedure TrimmedSetLine(ALine: integer; ALineText: string);
    procedure UndoRedoAdded(Sender: TObject);
    procedure UnlockUndo;
    procedure UpdateCaret;
    procedure UpdateCtrlMouse;
    procedure UpdateScrollBars;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure DblClick; override;
    {$IFDEF SYN_LAZARUS}
    procedure TripleClick; override;
    procedure QuadClick; override;
    {$ENDIF}
    procedure DecPaintLock;
    procedure DestroyWnd; override;
    procedure DragOver(Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure FindMatchingBracket; virtual;
    {$IFDEF SYN_LAZARUS}
    function FindMatchingBracket(PhysStartBracket: TPoint;
                                 StartIncludeNeighborChars, MoveCaret,
                                 SelectBrackets, OnlyVisible: boolean
                                 ): TPoint; virtual;
    procedure FindMatchingBracketPair(const PhysCaret: TPoint;
                                      var StartBracket, EndBracket: TPoint;
                                      OnlyVisible: boolean);
    {$ENDIF}
    function GetReadOnly: boolean; virtual;
    procedure HideCaret;
    procedure HighlighterAttrChanged(Sender: TObject);
    procedure IncPaintLock;
    procedure InitializeCaret;
    // note: FirstLine and LastLine don't need to be in correct order
    procedure InvalidateGutterLines(FirstLine, LastLine: integer);
    procedure InvalidateLines(FirstLine, LastLine: integer);
    {$IFDEF SYN_LAZARUS}
    procedure InvalidateBracketHighlight(OnlyIfCaretMoved: boolean);
    {$ENDIF}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$IFDEF SYN_LAZARUS}
    procedure UTF8KeyPress(var Key: TUTF8Char); override;
    {$ENDIF}
    procedure KeyPress(var Key: Char); override;
    {$IFDEF SYN_LAZARUS}
    procedure KeyUp(var Key : Word; Shift : TShiftState); override;
    {$ENDIF}
    procedure ListAdded(Index: integer);                                        //mh 2000-10-10
    procedure ListCleared(Sender: TObject);
    procedure ListDeleted(Index: integer);
    procedure ListInserted(Index: integer);
    procedure ListPutted(Index: integer);
    procedure ListScanRanges(Sender: TObject);
    procedure Loaded; override;
    procedure MarkListChange(Sender: TObject);
{$IFDEF SYN_MBCSSUPPORT}
    procedure MBCSGetSelRangeInLineWhenColumnSelectionMode(const s: string;
      var ColFrom, ColTo: Integer);
{$ENDIF}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y:
      Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
    procedure NotifyHookedCommandHandlers(AfterProcessing: boolean;
      var Command: TSynEditorCommand;
      var AChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
      Data: pointer); virtual;
    procedure Paint; override;
    procedure PaintGutter(AClip: TRect; FirstLine, LastLine: integer); virtual;
    procedure PaintTextLines(AClip: TRect; FirstLine, LastLine,
      FirstCol, LastCol: integer); virtual;
    {$IFDEF SYN_LAZARUS}
    procedure StartPaintBuffer(const ClipRect: TRect);
    procedure EndPaintBuffer(const ClipRect: TRect);
    procedure EraseBackground(DC: HDC); override;
    {$ENDIF}
    procedure RecalcCharExtent;
    procedure RedoItem;                                                         //sbs 2000-11-19
    procedure SetCaretXY(Value: TPoint); virtual;
    procedure SetName(const Value: TComponentName); override;
    procedure SetReadOnly(Value: boolean); virtual;
    procedure SetSelTextPrimitive(PasteMode: TSynSelectionMode; Value: PChar;
      ATag: PInteger);
    procedure ShowCaret;
    // If the translations requires Data, memory will be allocated for it via a
    // GetMem call.  The client must call FreeMem on Data if it is not NIL.
    function TranslateKeyCode(Code: word; Shift: TShiftState;
      var Data: pointer): TSynEditorCommand;
    procedure UndoItem;                                                         //sbs 2000-11-19
  protected
    fGutterWidth: Integer;
    fInternalImage: TSynInternalImage;
    {$IFDEF EnableDoubleBuf}
    BufferBitmap: TBitmap; // the double buffer
    {$ENDIF}
    SavedCanvas: TCanvas; // the normal TCustomControl canvas during paint
    procedure DoOnClearBookmark(var Mark: TSynEditMark); virtual;               // djlp - 2000-08-29
    procedure DoOnCommandProcessed(Command: TSynEditorCommand;
      AChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
      Data: pointer); virtual;
    // no method DoOnDropFiles, intercept the WM_DROPFILES instead
    procedure DoOnGutterClick(X, Y: integer); virtual;
    procedure DoOnPaint; virtual;
    procedure DoOnPlaceMark(var Mark: TSynEditMark); virtual;
    procedure DoOnProcessCommand(var Command: TSynEditorCommand;
      var AChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
      Data: pointer); virtual;
    function DoOnReplaceText(const ASearch, AReplace: string;
      Line, Column: integer): TSynReplaceAction; virtual;
    function DoOnSpecialLineColors(Line: integer;
      var Foreground, Background: TColor): boolean; virtual;
    procedure DoOnStatusChange(Changes: TSynStatusChanges); virtual;
    {$IFDEF SYN_LAZARUS}
    property LastMouseCaret: TPoint read FLastMouseCaret write SetLastMouseCaret;
    function GetSelEnd: integer;                                                 //L505
    function GetSelStart: integer;
    procedure SetSelEnd(const Value: integer);
    procedure SetSelStart(const Value: integer);
    {$ENDIF}
  public
    {$IFDEF SYN_LAZARUS}
    //code fold
    procedure CodeFoldAction(iLine: integer);
    function FindNextUnfoldedLine(iLine: integer; Down: boolean): Integer;
    procedure UnfoldAll;
    {$ENDIF}

    procedure AddKey(Command: TSynEditorCommand; Key1: word; SS1: TShiftState;
      Key2: word; SS2: TShiftState);
    procedure BeginUndoBlock;                                                   //sbs 2000-11-19
    procedure BeginUpdate;
    function CaretXPix: Integer;
    function CaretYPix: Integer;
    procedure ClearAll;
    procedure ClearBookMark(BookMark: Integer);
    procedure ClearSelection;
    procedure CommandProcessor(Command:TSynEditorCommand;
      AChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
      Data:pointer); virtual;
    procedure ClearUndo;
    procedure CopyToClipboard;
    constructor Create(AOwner: TComponent); override;
    procedure CutToClipboard;
    destructor Destroy; override;
    procedure DoCopyToClipboard(const SText: string);
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure EndUndoBlock;                                                     //sbs 2000-11-19
    procedure EndUpdate;
    procedure EnsureCursorPosVisible;
{$IFDEF SYN_COMPILER_4_UP}
    function ExecuteAction(ExeAction: TBasicAction): boolean; override;
{$ENDIF}
    procedure ExecuteCommand(Command: TSynEditorCommand;
      {$IFDEF SYN_LAZARUS}const AChar: TUTF8Char{$ELSE}AChar: Char{$ENDIF};
      Data: pointer); virtual;
    function GetBookMark(BookMark: integer; var X, Y: integer): boolean;
    function GetHighlighterAttriAtRowCol(XY: TPoint; var Token: string;
      var Attri: TSynHighlighterAttributes): boolean;
    function GetHighlighterAttriAtRowColEx(XY: TPoint; var Token: string;
      var TokenType, Start: Integer;
      var Attri: TSynHighlighterAttributes): boolean;                           //L505

    {$IFDEF SYN_LAZARUS}
    procedure GetWordBoundsAtRowCol(const XY: TPoint; var StartX, EndX: integer);
    function GetLineIndentProposal(Line: integer;
                                   IgnoreCurrentLineText: boolean): integer;
    {$ENDIF}
    function GetWordAtRowCol(XY: TPoint): string;
    procedure GotoBookMark(BookMark: Integer);
    function IdentChars: TSynIdentChars;
    {$IFDEF SYN_LAZARUS}
    function IsIdentChar(const c: TUTF8Char): boolean;
    {$ENDIF}
    procedure InvalidateGutter;
    procedure InvalidateLine(Line: integer);
    function IsBookmark(BookMark: integer): boolean;
    {$IFDEF SYN_LAZARUS}
    function LogicalToPhysicalPos(const p: TPoint): TPoint;
    function LogicalToPhysicalCol(const Line: string;
                                  LogicalPos: integer): integer;
    function LogicalToPhysicalCol(Line: PChar; LineLen: integer;
                  LogicalPos, StartBytePos, StartPhysicalPos: integer): integer;
    function PhysicalLineLength(Line: PChar; LineLen: integer;
                                WithTabs: boolean): integer;
    function PhysicalToLogicalPos(const p: TPoint): TPoint;
    function PhysicalToLogicalCol(const Line: string;
                                  PhysicalPos: integer): integer;
    function PhysicalToLogicalCol(const Line: string;
                 PhysicalPos, StartBytePos, StartPhysicalPos: integer): integer;
    procedure MoveCaretToVisibleArea;
    procedure MoveCaretIgnoreEOL(const NewCaret: TPoint);
    procedure MoveLogicalCaretIgnoreEOL(const NewLogCaret: TPoint);
    function NextTokenPos: TPoint; virtual;
    {$ELSE}
    function LogicalToPhysicalPos(p: TPoint): TPoint;
    {$ENDIF}
    function NextWordPos: TPoint; virtual;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PasteFromClipboard;
    function PrevWordPos: TPoint; virtual;
    function PixelsToRowColumn(Pixels: TPoint): TPoint;
    {$IFDEF SYN_LAZARUS}
    function PixelsToLogicalPos(const Pixels: TPoint): TPoint;
    function ScreenRowToRow(ScreenRow: integer): integer;
    function RowToScreenRow(PhysicalRow: integer): integer;
    {$ENDIF}
    procedure Redo;
    procedure RegisterCommandHandler(AHandlerProc: THookedCommandEvent;
      AHandlerData: pointer);
    function RowColumnToPixels(
                      {$IFDEF SYN_LAZARUS}const {$ENDIF}RowCol: TPoint): TPoint;
    function SearchReplace(const ASearch, AReplace: string;
      AOptions: TSynSearchOptions): integer;
    procedure SelectAll;
    {$IFDEF SYN_LAZARUS}
    procedure SelectToBrace;
    procedure SelectLine;
    procedure SelectParagraph;
    {$ENDIF}
    procedure SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
    procedure SetDefaultKeystrokes; virtual;
    procedure SetOptionFlag(Flag: TSynEditorOption; Value: boolean);
    procedure SetSelWord;
    procedure Undo;
    procedure UnregisterCommandHandler(AHandlerProc: THookedCommandEvent);
{$IFDEF SYN_COMPILER_4_UP}
    function UpdateAction(TheAction: TBasicAction): boolean; override;
{$ENDIF}
    procedure WndProc(var Msg: TMessage); override;
  public
    property BlockBegin: TPoint read GetBlockBegin write SetBlockBegin;
    property BlockEnd: TPoint read GetBlockEnd write SetBlockEnd;
    property CanPaste: Boolean read GetCanPaste;
    property CanRedo: boolean read GetCanRedo;
    property CanUndo: boolean read GetCanUndo;
    property CaretX: Integer read fCaretX write SetCaretX;
    property CaretY: Integer read fCaretY write SetCaretY;
    property CaretXY: TPoint read GetCaretXY write SetCaretXY;
    property CharsInWindow: Integer read fCharsInWindow;
    property CharWidth: integer read fCharWidth;
    property Color default clWhite;
    {$IFDEF SYN_LAZARUS}
    property CtrlMouseActive: boolean read fCtrlMouseActive;
    property LogicalCaretXY: TPoint read GetLogicalCaretXY write SetLogicalCaretXY;
    property LineIndenter: TSynCustomLineIndenter read fLineIndenter
                                                  write SetLineIndenter;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelEnd: Integer read GetSelEnd write SetSelEnd;
    {$ENDIF}
    property Font: TFont read GetFont write SetFont;
    property GutterWidth: Integer read fGutterWidth;
    property Highlighter: TSynCustomHighlighter
      read fHighlighter write SetHighlighter;
    property LeftChar: Integer read fLeftChar write SetLeftChar;
    property LineHeight: integer read fTextHeight;
    property LinesInWindow: Integer read fLinesInWindow; // MG: fully visible lines
    property LineText: string read GetLineText write SetLineText;
    property Lines: TStrings read fLines write SetLines;
    property Marks: TSynEditMarkList read fMarkList;
    property MaxLeftChar: integer read fMaxLeftChar write SetMaxLeftChar
      default 1024;
    property Modified: Boolean read fModified write SetModified;
    property PaintLock: Integer read fPaintLock;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default FALSE;
    property SelAvail: Boolean read GetSelAvail;
    property SelText: string read GetSelText write SetSelTextExternal;
    property Text: string read SynGetText write SynSetText;
    property TopLine: Integer read fTopLine write SetTopLine;
    {$IFDEF SYN_LAZARUS}
    property UseUTF8: boolean read FUseUTF8;
    procedure Update; override;
    procedure Invalidate; override;
    {$ENDIF}
  public
    property OnKeyDown;
    property OnKeyPress;
    property OnProcessCommand: TProcessCommandEvent
      read FOnProcessCommand write FOnProcessCommand;
  protected
    property BookMarkOptions: TSynBookMarkOpt
      read fBookMarkOpt write fBookMarkOpt;
    property BorderStyle {$ifndef SYN_LAZARUS}: TBorderStyle read FBorderStyle write SetBorderStyle{$endif}
      default bsSingle;
    {$IFDEF SYN_LAZARUS}
    property BlockIndent: integer read fBlockIndent write SetBlockIndent default 2;
    {$ENDIF}
    property ExtraLineSpacing: integer
      read fExtraLineSpacing write SetExtraLineSpacing default 0;
    property Gutter: TSynGutter read fGutter write SetGutter;
    property HideSelection: boolean read fHideSelection write SetHideSelection
      default false;
    property InsertCaret: TSynEditCaretType read FInsertCaret
      write SetInsertCaret default ctVerticalLine;
    property InsertMode: boolean read fInserting write SetInsertMode
      default true;
    property Keystrokes: TSynEditKeyStrokes
      read FKeystrokes write SetKeystrokes;
    property MaxUndo: Integer read GetMaxUndo write SetMaxUndo default 1024;
    property Options: TSynEditorOptions read fOptions write SetOptions
      default SYNEDIT_DEFAULT_OPTIONS;
    {$IFDEF SYN_LAZARUS}
    property Options2: TSynEditorOptions2 read fOptions2 write SetOptions2
      default SYNEDIT_DEFAULT_OPTIONS2;
    {$ENDIF}
    property OverwriteCaret: TSynEditCaretType read FOverwriteCaret
      write SetOverwriteCaret default ctBlock;
    property RightEdge: Integer read fRightEdge write SetRightEdge default 80;
    property RightEdgeColor: TColor
      read fRightEdgeColor write SetRightEdgeColor default clSilver;
    property ScrollBars: TScrollStyle
      read FScrollBars write SetScrollBars default ssBoth;
    property SelectedColor: TSynSelectedColor
      read FSelectedColor write FSelectedColor;
    property SelectionMode: TSynSelectionMode
      read FSelectionMode write SetSelectionMode default smNormal;
    {$IFDEF SYN_LAZARUS}
    property TabChar: char read FTabChar write SetTabChar;
    property CFDividerDrawLevel: Integer
        read FCFDividerDrawLevel write SetCFDividerDrawLevel;
    {$ENDIF}
    property TabWidth: integer read fTabWidth write SetTabWidth default 8;
    property WantTabs: boolean read fWantTabs write SetWantTabs default FALSE;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClearBookmark: TPlaceMarkEvent read fOnClearMark
      write fOnClearMark;
    property OnCommandProcessed: TProcessCommandEvent
      read fOnCommandProcessed write fOnCommandProcessed;
    property OnDropFiles: TDropFilesEvent read fOnDropFiles write fOnDropFiles;
    property OnGutterClick: TGutterClickEvent
      read fOnGutterClick write fOnGutterClick;
    property OnPaint: TPaintEvent read fOnPaint write fOnPaint;
    property OnPlaceBookmark: TPlaceMarkEvent
      read FOnPlaceMark write FOnPlaceMark;
    property OnProcessUserCommand: TProcessCommandEvent
      read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnReplaceText: TReplaceTextEvent read fOnReplaceText
      write fOnReplaceText;
    property OnSpecialLineColors: TSpecialLineColorsEvent
      read fOnSpecialLineColors write fOnSpecialLineColors;
    property OnStatusChange: TStatusChangeEvent
      read fOnStatusChange write fOnStatusChange;
  end;

  TSynEdit = class(TCustomSynEdit)
  published
    // inherited properties
    property Align;
{$IFDEF SYN_LAZARUS}
    property BlockIndent;
    property BorderSpacing;
{$ENDIF}
{$IFDEF SYN_COMPILER_4_UP}
    property Anchors;
    property Constraints;
{$ENDIF}
    property Color;
    {$IFDEF SYN_LAZARUS}
    property Cursor default crIBeam;
    property CFDividerDrawLevel;
    {$ENDIF}
    property Ctl3D;
    property Enabled;
    property Font;
    property Height;
    property Name;
    property ParentColor;
    property ParentCtl3D;
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
    {$IFDEF SYN_LAZARUS}
    property OnTripleClick;
    property OnQuadClick;
    {$ENDIF}
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
    {$IFDEF SYN_LAZARUS}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
{$IFDEF SYN_COMPILER_4_UP}
// ToDo Docking
    property OnStartDock;
{$ENDIF}
    property OnStartDrag;
    // TCustomSynEdit properties
    property BookMarkOptions;
    property BorderStyle;
    property ExtraLineSpacing;
    property Gutter;
    property HideSelection;
    property Highlighter;
    property InsertCaret;
    property InsertMode;
    property Keystrokes;
    property Lines;
    property MaxLeftChar;
    property MaxUndo;
    property Options;
    {$IFDEF SYN_LAZARUS}
    property Options2;
    {$ENDIF}
    property OverwriteCaret;
    property ReadOnly;
    property RightEdge;
    property RightEdgeColor;
    property ScrollBars;
    property SelectedColor;
    property SelectionMode;
    property TabWidth;
    property WantTabs;
    // TCustomSynEdit events
    property OnChange;
    property OnClearBookmark;                                                   // djlp 2000-08-29
    property OnCommandProcessed;
    property OnDropFiles;
    property OnGutterClick;
    property OnPaint;
    property OnPlaceBookmark;
    property OnProcessCommand;
    property OnProcessUserCommand;
    property OnReplaceText;
    property OnSpecialLineColors;
    property OnStatusChange;
  end;

{$IFDEF SYN_LAZARUS}
function SynEditClipboardFormat: TClipboardFormat;
{$ENDIF}

implementation

// { $R SynEdit.res}

uses
{$IFDEF SYN_COMPILER_4_UP}
  StdActns,
{$ENDIF}
{$IFNDEF SYN_LAZARUS}
// ToDo ShellAPI
  ShellAPI, SynEditStrConst,
{$ENDIF}
  Clipbrd;


{$IFDEF SYN_LAZARUS}
const
  fSynEditClipboardFormat: TClipboardFormat = 0;

function SynEditClipboardFormat: TClipboardFormat;
begin
  if fSynEditClipboardFormat=0 then
    fSynEditClipboardFormat := ClipboardRegisterFormat(SYNEDIT_CLIPBOARD_FORMAT);
  Result:=fSynEditClipboardFormat;
end;

function CreateTabsAndSpaces(StartPos, SpaceLen, TabWidth: integer;
  UseTabs: boolean): string;
var
  TabCount: Integer;
  EndPos: Integer;
  PosPlusOneTab: Integer;
begin
  Result:='';
  if not UseTabs then begin
    Result:=StringOfChar(' ',SpaceLen);
    exit;
  end;
  TabCount:=0;
  EndPos:=StartPos+SpaceLen;
  while StartPos<EndPos do begin
    PosPlusOneTab:=StartPos+TabWidth-((StartPos-1) mod TabWidth);
    if PosPlusOneTab<=EndPos then begin
      inc(TabCount);
      StartPos:=PosPlusOneTab;
    end else begin
      Result:=StringOfChar(' ',EndPos-StartPos);
      break;
    end;
  end;
  if TabCount>0 then
    Result:=StringOfChar(#9,TabCount)+Result;
end;
{$ENDIF}

function Roundoff(X: Extended): Longint;
begin
  if (x >= 0) then begin
    Result := TruncToInt(x + 0.5)
  end else begin
    Result := TruncToInt(x - 0.5);
  end;
end;

{ THookedCommandHandlerEntry }

type
  THookedCommandHandlerEntry = class(TObject)
  private
    fEvent: THookedCommandEvent;
    fData: pointer;
    function Equals(AEvent: THookedCommandEvent): boolean;
  {$IFDEF SYN_LAZARUS}
  public
  {$ENDIF}
    constructor Create(AEvent: THookedCommandEvent; AData: pointer);
  end;

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

{ TCustomSynEdit }

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.AquirePrimarySelection;
var
  FormatList: TClipboardFormat;
begin
  if (not SelAvail)
  or (PrimarySelection.OnRequest=@PrimarySelectionRequest) then exit;
  FormatList:=CF_TEXT;
  try
    PrimarySelection.SetSupportedFormats(1,@FormatList);
    PrimarySelection.OnRequest:=@PrimarySelectionRequest;
  except
  end;
end;
{$ENDIF}

function TCustomSynEdit.PixelsToRowColumn(Pixels: TPoint): TPoint;
// converts the client area coordinate
// to Caret position (screen position, (1,1) based)
// To get the text/physical position use PixelsToLogicalPos
var
  f: Single;
begin
  f := ((Pixels.X+(fLeftChar-1)*fCharWidth-fGutterWidth-2)/fCharWidth)+1;
  // don't return a partially visible last line
  if Pixels.Y >= fLinesInWindow * fTextHeight then begin
    Pixels.Y := fLinesInWindow * fTextHeight - 1;
    if Pixels.Y < 0 then Pixels.Y := 0;
  end;
  {$IFDEF SYN_LAZARUS}
  Result := Point(RoundOff(f), ScreenRowToRow(Pixels.Y div fTextHeight));
  {$ELSE}
  Result := Point(RoundOff(f), Pixels.Y div fTextHeight + TopLine);
  {$ENDIF}
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

{$IFDEF SYN_LAZARUS}
function TCustomSynEdit.PixelsToLogicalPos(const Pixels: TPoint): TPoint;
begin
  Result:=PhysicalToLogicalPos(PixelsToRowColumn(Pixels));
end;

function TCustomSynEdit.ScreenRowToRow(ScreenRow: integer): integer;
begin
  Result:=TopLine;
  if ScreenRow>LinesInWindow+1 then ScreenRow:=LinesInWindow+1;
  while ScreenRow>0 do begin
    if (Result>Lines.Count)
    or (not TSynEditStringList(fLines).Folded[Result-1]) then
      dec(ScreenRow);
    inc(Result);
  end;
end;

function TCustomSynEdit.RowToScreenRow(PhysicalRow: integer): integer;
// returns -1 for lines above visible screen (<TopLine)
// 0 for the first line
// Max(0,LinesInWindow-1) for the last fully visible line
// and returns LinesInWindow for lines below visible screen including the
// partially visible line at the bottom
var
  i: LongInt;
begin
  if PhysicalRow<TopLine then exit(-1);
  Result:=0;
  i:=TopLine;
  while (Result<LinesInWindow) and (i<PhysicalRow) do begin
    if (i>Lines.Count)
    or (not TSynEditStringList(fLines).Folded[i-1]) then
      inc(Result);
    inc(i);
  end;
end;
{$ENDIF}

function TCustomSynEdit.RowColumnToPixels(
  {$IFDEF SYN_LAZARUS}const {$ENDIF}RowCol: TPoint): TPoint;
// converts screen position (1,1) based
// to client area coordinate
begin
  Result:=RowCol;
  Result.X := (Result.X - 1) * fCharWidth + fTextOffset;
  Result.Y := (Result.Y - fTopLine) * fTextHeight + 1;
end;

procedure TCustomSynEdit.ComputeCaret(X, Y: Integer);
// set caret to pixel position
begin
  CaretXY := PixelsToRowColumn(Point(X,Y));
end;

procedure TCustomSynEdit.DoCopyToClipboard(const SText: string);
var
{$IFDEF SYN_LAZARUS}
  Buf: Pointer;
  BufSize: integer;
{$ELSE}
  Mem: HGLOBAL;
{$ENDIF}
  P: PChar;
  SLen: integer;
  Failed: boolean;
begin
  if SText <> '' then begin
    Failed := TRUE; // assume the worst.
    SLen := Length(SText);
    {$IFDEF SYN_LAZARUS}
    try
      Clipboard.Clear;
      Clipboard.AsText:=SText;
      Failed:=not Clipboard.HasFormat(CF_TEXT);
    except
    end;
    if not Failed then begin
      Failed:=true;
      // Copy it in our custom format so we know what kind of block it is.
      // That effects how it is pasted in.
      BufSize:=SLen+SizeOf(TSynSelectionMode)+1;
      GetMem(Buf,BufSize);
      if Buf<>nil then
      try
        P:=PChar(Buf);
        // Our format:  TSynSelectionMode value followed by text.
        PSynSelectionMode(P)^ := SelectionMode;
        inc(P, SizeOf(TSynSelectionMode));
        if SLen>0 then begin
          Move(SText[1], P^, SLen);
          inc(P,SLen);
        end;
        P[0]:=#0;
        try
          Failed:=not Clipboard.AddFormat(SynEditClipboardFormat,Buf^,BufSize);
        except
        end;
      finally
        FreeMem(Buf);
      end;
    end;
    if Failed then
      raise ESynEditError.Create('Clipboard copy operation failed');
    {$ELSE}
    // Open and Close are the only TClipboard methods we use because TClipboard
    // is very hard (impossible) to work with if you want to put more than one
    // format on it at a time.
    Clipboard.Open;
    try
      // Clear anything already on the clipboard.
      EmptyClipboard;
      // Put it on the clipboard as normal text format so it can be pasted into
      // things like notepad or Delphi.
      Mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SLen + 1);
      if Mem <> 0 then begin
        P := GlobalLock(Mem);
        try
          if P <> nil then begin
            Move(PChar(SText)^, P^, SLen + 1);
            // Put it on the clipboard in text format
            Failed := SetClipboardData(CF_TEXT, Mem) = 0;
          end;
        finally
          GlobalUnlock(Mem);
        end;
      end;
      // Don't free Mem!  It belongs to the clipboard now, and it will free it
      // when it is done with it.
      if not Failed then begin
        // Copy it in our custom format so we know what kind of block it is.
        // That effects how it is pasted in.
        Mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, SLen +
          SizeOf(TSynSelectionMode) + 1);
        P := GlobalLock(Mem);
        try
          if P <> nil then begin
            // Our format:  TSynSelectionMode value followed by text.
            PSynSelectionMode(P)^ := SelectionMode;
            inc(P, SizeOf(TSynSelectionMode));
            Move(PChar(SText)^, P^, SLen + 1);
            Failed := SetClipboardData(SynEditClipboardFormat, Mem) = 0;
          end;
        finally
          GlobalUnlock(Mem);
        end;
        // Don't free Mem!  It belongs to the clipboard now, and it will free it
        // when it is done with it.
      end;
    finally
      Clipboard.Close;
      if Failed then
        raise ESynEditError.Create('Clipboard copy operation failed');
    end;
    {$ENDIF}
  end;
end;

procedure TCustomSynEdit.CopyToClipboard;
var
  SText: string;
begin
  if SelAvail then begin
    SText := SelText;
    DoCopyToClipboard(SText);
  end;
end;

procedure TCustomSynEdit.CutToClipboard;
var
  SText: string;
begin
  if SelAvail then begin
    SText := SelText;
    DoCopyToClipboard(SText);
    fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SText, SelectionMode);
    LockUndo;
    SelText := '';
    UnlockUndo;
  end;
end;

constructor TCustomSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{begin}                                                                         //mh 2000-10-10
//  fLines := TSynEditList.Create;
  fLines := TSynEditStringList.Create;
//  with TSynEditList(fLines) do begin
  with TSynEditStringList(fLines) do begin
    OnAdded := {$IFDEF FPC}@{$ENDIF}ListAdded;
    OnChange := {$IFDEF FPC}@{$ENDIF}LinesChanged;
    OnChanging := {$IFDEF FPC}@{$ENDIF}LinesChanging;
    OnCleared := {$IFDEF FPC}@{$ENDIF}ListCleared;
    OnDeleted := {$IFDEF FPC}@{$ENDIF}ListDeleted;
    OnInserted := {$IFDEF FPC}@{$ENDIF}ListInserted;
    OnPutted := {$IFDEF FPC}@{$ENDIF}ListPutted;
//    OnScanRanges := {$IFDEF FPC}@{$ENDIF}ListScanRanges;
  end;
{end}                                                                           //mh 2000-10-10
  fFontDummy := TFont.Create;
  fUndoList := TSynEditUndoList.Create;
  fUndoList.OnAddedUndo := {$IFDEF FPC}@{$ENDIF}UndoRedoAdded;
  fRedoList := TSynEditUndoList.Create;
  fRedoList.OnAddedUndo := {$IFDEF FPC}@{$ENDIF}UndoRedoAdded;
{$IFDEF SYN_COMPILER_4_UP}
{$IFNDEF SYN_LAZARUS}
// ToDo DoubleBuffered
  DoubleBuffered := false;
{$ENDIF}
{$ENDIF}
  fSelectedColor := TSynSelectedColor.Create;
  fSelectedColor.OnChange := {$IFDEF FPC}@{$ENDIF}SelectedColorsChanged;
  fBookMarkOpt := TSynBookMarkOpt.Create(Self);
  fBookMarkOpt.OnChange := {$IFDEF FPC}@{$ENDIF}BookMarkOptionsChanged;
// fRightEdge has to be set before FontChanged is called for the first time
  fRightEdge := 80;
  fGutter := TSynGutter.Create;
  fGutter.OnChange := {$IFDEF FPC}@{$ENDIF}GutterChanged;
  fGutterWidth := fGutter.Width;
  fTextOffset := fGutterWidth + 2;
  ControlStyle := ControlStyle + [csOpaque, csSetCaption
                    {$IFDEF SYN_LAZARUS}, csTripleClicks, csQuadClicks{$ENDIF}];
  Height := 150;
  Width := 200;
  Cursor := crIBeam;
{$IFDEF SYN_LAZARUS}
  Color := clWhite;
  {$IFDEF LCLgtk}
  fFontDummy.Name := '-adobe-courier-medium-r-normal-*-*-140-*-*-*-*-iso10646-1';
  fFontDummy.Height := 14;
  {$ELSE}
  fFontDummy.Name := 'courier';
  fFontDummy.Size := 12;
  {$ENDIF}
  fLastMouseCaret := Point(-1,-1);
  fLastCtrlMouseLinkY := -1;
  fLastControlIsPressed := false;
  fBlockIndent := 2;
  FTabChar := {$IFDEF DebugShowTabs}'%'{$ELSE}' '{$ENDIF};
{$ELSE}
  Color := clWindow;
  fFontDummy.Name := 'Courier New';
  fFontDummy.Size := 10;
{$ENDIF}
{$IFDEF SYN_COMPILER_3_UP}
{$IFNDEF SYN_LAZARUS}
// ToDo Font CharSet
  fFontDummy.CharSet := DEFAULT_CHARSET;
{$ENDIF}
{$ENDIF}
  fTextDrawer := TheTextDrawer.Create([fsBold], fFontDummy);
  Font.Assign(fFontDummy);
  Font.OnChange := {$IFDEF FPC}@{$ENDIF}FontChanged;
  FontChanged(nil);
  ParentFont := False;
  ParentColor := False;
  TabStop := True;
  fInserting := True;
  fMaxLeftChar := 1024;
  fScrollBars := ssBoth;
  {$IFDEF SYN_LAZARUS}
  BorderStyle := bsSingle;
  {$ELSE}
  fBorderStyle := bsSingle;
  {$ENDIF}
  fInsertCaret := ctVerticalLine;
  fOverwriteCaret := ctBlock;
  FSelectionMode := smNormal;
  fKeystrokes := TSynEditKeyStrokes.Create(Self);
  fMarkList := TSynEditMarkList.Create(self);
  fMarkList.OnChange := {$IFDEF FPC}@{$ENDIF}MarkListChange;
  SetDefaultKeystrokes;
  fRightEdgeColor := clSilver;
{$IFDEF SYN_MBCSSUPPORT}
  fImeCount := 0;
  fMBCSStepAside := False;
{$ENDIF}
  fWantTabs := False;
  fTabWidth := 8;
  fLeftChar := 1;
  fTopLine := 1;
  fCaretX := 1;
  fLastCaretX := 1;                                                             //mh 2000-10-19
  fCaretY := 1;
  fBlockBegin := Point(1, 1);
  fBlockEnd := fBlockBegin;
  // find / replace
  fTSearch := TSynEditSearch.Create;
  fOptions := SYNEDIT_DEFAULT_OPTIONS;
  {$IFDEF SYN_LAZARUS}
  fOptions2 := SYNEDIT_DEFAULT_OPTIONS2;
  {$ENDIF}
  fScrollTimer := TTimer.Create(Self);
  fScrollTimer.Enabled := False;
  fScrollTimer.Interval := 100;
  fScrollTimer.OnTimer := {$IFDEF FPC}@{$ENDIF}ScrollTimerHandler;
  fFirstLine := 1;
end;

procedure TCustomSynEdit.CreateParams(var Params: TCreateParams);
const
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL, WS_HSCROLL, WS_VSCROLL, WS_HSCROLL or WS_VSCROLL);
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  ClassStylesOff = CS_VREDRAW or CS_HREDRAW;
begin
  inherited CreateParams(Params);
  with Params do begin
    {$IFOPT R+}{$DEFINE RangeCheckOn}{$R-}{$ENDIF}
    WindowClass.Style := WindowClass.Style and not Cardinal(ClassStylesOff);
    Style := Style or ScrollBar[FScrollBars] or BorderStyles[BorderStyle]
      or WS_CLIPCHILDREN;
    {$IFDEF RangeCheckOn}{$R+}{$ENDIF}
    if NewStyleControls and Ctl3D and (BorderStyle = bsSingle) then begin
      Style := Style and not Cardinal(WS_BORDER);
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TCustomSynEdit.DecPaintLock;
var
  LastLineChanged: LongInt;
  StartY: Integer;
begin
  if (fPaintLock=1) and HandleAllocated then begin
    {$IFDEF SYN_LAZARUS}
    if fHighlighterNeedsUpdateStartLine>0 then begin
      //DebugLn('TCustomSynEdit.DecPaintLock ',dbgs(fHighlighterNeedsUpdateStartLine),'-',dbgs(fHighlighterNeedsUpdateEndLine));
      if fHighlighterNeedsUpdateStartLine<=Lines.Count then begin
        if fHighlighterNeedsUpdateEndLine>Lines.Count then
          fHighlighterNeedsUpdateEndLine:=Lines.Count;
        LastLineChanged:=fHighlighterNeedsUpdateEndLine;
        if Assigned(fHighlighter) then begin
          // rescan all lines in range
          // Note: The highlighter range of the line can be invalid as well,
          //       so start scan one line earlier
          StartY:=fHighlighterNeedsUpdateStartLine-2;
          if StartY<=0 then begin
            StartY:=0;
            fHighlighter.ReSetRange;
          end else begin
            fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[StartY]);
          end;
          LastLineChanged:=ScanFrom(StartY,
                                    fHighlighterNeedsUpdateEndLine-1);
          //DebugLn('TCustomSynEdit.DecPaintLock ',dbgs(fHighlighterNeedsUpdateStartLine),'-',dbgs(fHighlighterNeedsUpdateEndLine),' LastLineChanged=',dbgs(LastLineChanged));
        end;
        InvalidateLines(fHighlighterNeedsUpdateStartLine,LastLineChanged+1);
        InvalidateGutterLines(fHighlighterNeedsUpdateStartLine,LastLineChanged+1);
      end;
      fHighlighterNeedsUpdateStartLine:=0;
      fHighlighterNeedsUpdateEndLine:=0;
    end;
    {$ENDIF}
  end;
  Dec(fPaintLock);
  if (fPaintLock = 0) and HandleAllocated then begin
    if sfScrollbarChanged in fStateFlags then
      UpdateScrollbars;
    if sfCaretChanged in fStateFlags then
      UpdateCaret;
    if fStatusChanges <> [] then
      DoOnStatusChange(fStatusChanges);
  end;
end;

destructor TCustomSynEdit.Destroy;
var
  i: integer;
begin
  {$IFDEF SYN_LAZARUS}
  if HandleAllocated then LCLIntf.DestroyCaret(Handle);
  LineIndenter:=nil;
  {$ENDIF}
  Highlighter := nil;
  // free listeners while other fields are still valid
  if Assigned(fHookedCommandHandlers) then begin
    for i := 0 to fHookedCommandHandlers.Count - 1 do
      THookedCommandHandlerEntry(fHookedCommandHandlers[i]).Free;
    fHookedCommandHandlers.Free;
  end;
  if fPlugins <> nil then begin
    for i := fPlugins.Count - 1 downto 0 do
      TSynEditPlugin(fPlugins[i]).Free;
    fPlugins.Free;
  end;
  {$IFNDEF SYN_LAZARUS}
  fScrollTimer.Free;
  fTSearch.Free;
  fMarkList.Free;
  fBookMarkOpt.Free;
  fBookMarkOpt := nil;
  fKeyStrokes.Free;
  fSelectedColor.Free;
  fUndoList.Free;
  fRedoList.Free;
  fGutter.Free;
  fTextDrawer.Free;
  fInternalImage.Free;
  fFontDummy.Free;
  Lines.Free;
  {$ELSE}
  fHookedCommandHandlers:=nil;
  fPlugins:=nil;
  FreeAndNil(fScrollTimer);
  FreeAndNil(fTSearch);
  FreeAndNil(fMarkList);
  FreeAndNil(fBookMarkOpt);
  FreeAndNil(fKeyStrokes);
  FreeAndNil(fSelectedColor);
  FreeAndNil(fUndoList);
  FreeAndNil(fRedoList);
  FreeAndNil(fGutter);
  FreeAndNil(fTextDrawer);
  FreeAndNil(fInternalImage);
  FreeAndNil(fFontDummy);
  FreeAndNil(fLines);
  {$ENDIF}
  inherited Destroy;
end;

function TCustomSynEdit.GetBlockBegin: TPoint;
begin
  if (fBlockEnd.Y < fBlockBegin.Y)
    or ((fBlockEnd.Y = fBlockBegin.Y) and (fBlockEnd.X < fBlockBegin.X))
  then
    Result := fBlockEnd
  else
    Result := fBlockBegin;
end;

function TCustomSynEdit.GetBlockEnd: TPoint;
begin
  if (fBlockEnd.Y < fBlockBegin.Y)
    or ((fBlockEnd.Y = fBlockBegin.Y) and (fBlockEnd.X < fBlockBegin.X))
  then
    Result := fBlockBegin
  else
    Result := fBlockEnd;
end;

function TCustomSynEdit.CaretXPix: Integer;
var
  p: TPoint;
begin
  p := Point(fCaretX, fCaretY);
  Result := RowColumnToPixels(p).X;
end;

function TCustomSynEdit.CaretYPix: Integer;
begin
  {$IFDEF SYN_LAZARUS}
  Result := RowToScreenRow(fCaretY) * fTextHeight + 1;
  {$ELSE}
  Result := RowColumnToPixels(Point(1, fCaretY)).Y;
  {$ENDIF}
end;

procedure TCustomSynEdit.FontChanged(Sender: TObject);
begin
  RecalcCharExtent;
  SizeOrFontChanged(TRUE);
end;

function TCustomSynEdit.GetFont: TFont;
begin
  Result := inherited Font;
end;

function TCustomSynEdit.GetLineText: string;
begin
  if (CaretY >= 1) and (CaretY <= Lines.Count) then
    Result := Lines[CaretY - 1]
  else
    Result := '';
end;

{$IFDEF SYN_LAZARUS}
function TCustomSynEdit.GetCharLen(const Line: string; CharStartPos: integer
  ): integer;
begin
  if UseUTF8 and (length(Line)>=CharStartPos) then
    Result:=UTF8CharacterLength(@Line[CharStartPos])
  else
    Result:=1;
end;

function TCustomSynEdit.AdjustBytePosToCharacterStart(Line: integer;
  BytePos: integer): integer;
var
  s: string;
begin
  Result:=BytePos;
  if Result<1 then
    Result:=1
  else if (Line>=1) and (Line<=Lines.Count) then begin
    s:=Lines[Line-1];
    if (Result<=length(s)) and UseUTF8 then
      Result:=UTF8FindNearestCharStart(PChar(Pointer(s)),length(s),Result);
  end;
end;

function TCustomSynEdit.AdjustPhysPosToCharacterStart(Line: integer;
  PhysPos: integer): integer;
var
  s: string;
  BytePos: LongInt;
begin
  Result:=PhysPos;
  if Result<1 then
    Result:=1
  else if (Line>=1) and (Line<=Lines.Count) then begin
    s:=Lines[Line-1];
    BytePos:=PhysicalToLogicalCol(s,Result);
    Result:=LogicalToPhysicalCol(s,BytePos);
  end;
end;

function TCustomSynEdit.GetLogicalCaretXY: TPoint;
begin
  Result:=PhysicalToLogicalPos(CaretXY);
end;

procedure TCustomSynEdit.SetCFDividerDrawLevel(const AValue: Integer);
begin
  if FCFDividerDrawLevel = AValue then
    Exit; //==>
  FCFDividerDrawLevel := AValue;
end;

procedure TCustomSynEdit.SetLineIndenter(const AValue: TSynCustomLineIndenter);
var
  OldLineIndenter: TSynCustomLineIndenter;
begin
  if Assigned(fLineIndenter) then begin
    OldLineIndenter:=fLineIndenter;
    fLineIndenter:=nil;
    OldLineIndenter.Editor:=nil;
  end;
  fLineIndenter:=AValue;
  if Assigned(fLineIndenter) then begin
    fLineIndenter.Editor:=Self;
  end;
end;

procedure TCustomSynEdit.SetLogicalCaretXY(const NewLogCaretXY: TPoint);
begin
  CaretXY:=LogicalToPhysicalPos(NewLogCaretXY);
end;
{$ENDIF}

function TCustomSynEdit.GetSelAvail: Boolean;
begin
  Result := (fBlockBegin.X <> fBlockEnd.X) or
    ((fBlockBegin.Y <> fBlockEnd.Y) and (fSelectionMode <> smColumn));
end;

function TCustomSynEdit.GetSelText: string;

  function CopyPadded(const S: string; Index, Count: integer): string;
  var
    SrcLen: Integer;
    DstLen: integer;
    P: PChar;
  begin
    SrcLen := Length(S);
    DstLen := Index + Count;
    if SrcLen >= DstLen then
      Result := Copy(S, Index, Count)
    else begin
      SetLength(Result, DstLen);
      P := PChar(Pointer(Result));
      StrPCopy(P, Copy(S, Index, Count));
      Inc(P, SrcLen);
      FillChar(P^, DstLen - Srclen, $20);
    end;
  end;

  procedure CopyAndForward(const S: string; Index, Count: Integer; var P:
    PChar);
  var
    pSrc: PChar;
    SrcLen: Integer;
    DstLen: Integer;
  begin
    SrcLen := Length(S);
    if (Index <= SrcLen) and (Count > 0) then begin
      Dec(Index);
      pSrc := PChar(Pointer(S)) + Index;
      DstLen := Min(SrcLen - Index, Count);
      Move(pSrc^, P^, DstLen);
      Inc(P, DstLen);
      P^ := #0;
    end;
  end;

  procedure CopyPaddedAndForward(const S: string; Index, Count: Integer;
    var P: PChar);
  var
    OldP: PChar;
    Len: Integer;
  begin
    OldP := P;
    CopyAndForward(S, Index, Count, P);
    Len := Count - (P - OldP);
    FillChar(P^, Len, #$20);
    Inc(P, Len);
  end;


const
  sLineBreak = {$IFDEF SYN_LAZARUS}LineEnding{$ELSE}#$0D#$0A{$ENDIF};
var
  First, Last, TotalLen: Integer;
  ColFrom, ColTo: Integer;
  I: Integer;
{$IFDEF SYN_MBCSSUPPORT}
  l, r: Integer;
  s: string;
{$ELSE}
  ColLen: integer;
{$ENDIF}
  P: PChar;
begin
  if not SelAvail then
    Result := ''
  else begin
    with BlockBegin do begin
      ColFrom := X;
      First := Y - 1;
    end;
    with BlockEnd do begin
      ColTo := X;
      Last := Y - 1;
    end;
    TotalLen := 0;
    case SelectionMode of
      smNormal:
        if (First = Last) then
          Result := Copy(Lines[First], ColFrom, ColTo - ColFrom)
        else begin
          // step1: calculate total length of result string
          TotalLen := Max(0, Length(Lines[First]) - ColFrom + 1);
          for i := First + 1 to Last - 1 do
            Inc(TotalLen, Length(Lines[i]));
          Inc(TotalLen, ColTo - 1);
          Inc(TotalLen, Length(sLineBreak) * (Last - First));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Pointer(Result));
          CopyAndForward(Lines[First], ColFrom, MaxInt, P);
          CopyAndForward(sLineBreak, 1, MaxInt, P);
          for i := First + 1 to Last - 1 do begin
            CopyAndForward(Lines[i], 1, MaxInt, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          {$IFDEF SYN_LAZARUS}
          CopyPaddedAndForward(Lines[Last], 1, ColTo - 1, P);
          {$ELSE}
          CopyAndForward(Lines[Last], 1, ColTo - 1, P);
          {$ENDIF}
        end;
      smColumn:
        begin
          if ColFrom > ColTo then
            SwapInt(ColFrom, ColTo);
          // step1: calclate total length of result string
{$IFNDEF SYN_MBCSSUPPORT}
          ColLen := ColTo - ColFrom;
          TotalLen := ColLen + (ColLen + Length(sLineBreak)) * (Last - First);
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Pointer(Result));
          for i := First to Last - 1 do begin
            CopyPaddedAndForward(Lines[i], ColFrom, ColLen, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          CopyPaddedAndForward(Lines[Last], ColFrom, ColLen, P);
{$ELSE} //SYN_MBCSSUPPORT
          for i := First to Last do begin
            s := Lines[i];
            l := ColFrom;
            r := ColTo;
            MBCSGetSelRangeInLineWhenColumnSelectionMode(s, l, r);
            Inc(TotalLen, r - l);
          end;
          Inc(TotalLen, Length(sLineBreak) * (Last - First));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Result);
          for i := First to Last - 1 do begin
            s := Lines[i];
            l := ColFrom;
            r := ColTo;
            MBCSGetSelRangeInLineWhenColumnSelectionMode(s, l, r);
            CopyPaddedAndForward(s, l, r - l, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          s := Lines[Last];
          l := ColFrom;
          r := ColTo;
          MBCSGetSelRangeInLineWhenColumnSelectionMode(s, l, r);
          CopyPaddedAndForward(Lines[Last], l, r - l, P);
{$ENDIF}
        end;
      smLine:
        begin
          // If block selection includes LastLine,
          // line break code(s) of the last line will not be added.
          // step1: calclate total length of result string
          for i := First to Last do
            Inc(TotalLen, Length(Lines[i]) + Length(sLineBreak));
          if Last = Lines.Count then
            Dec(TotalLen, Length(sLineBreak));
          // step2: build up result string
          SetLength(Result, TotalLen);
          P := PChar(Pointer(Result));
          for i := First to Last - 1 do begin
            CopyAndForward(Lines[i], 1, MaxInt, P);
            CopyAndForward(sLineBreak, 1, MaxInt, P);
          end;
          CopyAndForward(Lines[Last], 1, MaxInt, P);
          if (Last + 1) < Lines.Count then
            CopyAndForward(sLineBreak, 1, MaxInt, P);
        end;
    end;
  end;
end;

function TCustomSynEdit.SynGetText: string;
begin
  Result := Lines.Text;
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.SetTabChar(const AValue: Char);
begin
  if FTabChar=AValue then exit;
  FTabChar:=AValue;
  Invalidate;
end;

function TCustomSynEdit.RealGetText: TCaption;
begin
  if fLines<>nil then
    Result := Lines.Text
  else
    Result := '';
end;
{$ENDIF}

procedure TCustomSynEdit.HideCaret;
begin
  //DebugLn('[TCustomSynEdit.HideCaret] ',Name,' ',sfCaretVisible in fStateFlags,' ',eoPersistentCaret in Options);
  if sfCaretVisible in fStateFlags then begin
    if {$IFDEF SYN_LAZARUS}LCLIntf{$ELSE}Windows{$ENDIF}.HideCaret(Handle) then
      Exclude(fStateFlags, sfCaretVisible);
  end;
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

procedure TCustomSynEdit.IncPaintLock;
begin
  inc(fPaintLock);
end;

procedure TCustomSynEdit.InvalidateGutter;
begin
  InvalidateGutterLines(-1, -1);
end;

procedure TCustomSynEdit.InvalidateGutterLines(FirstLine, LastLine: integer);
var
  rcInval: TRect;
begin
  {$IFDEF SYN_LAZARUS}
  if sfPainting in fStateFlags then exit;
  {$ENDIF}
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then begin
      rcInval := Rect(0, 0, fGutterWidth,
                      ClientHeight{$IFDEF SYN_LAZARUS}-ScrollBarWidth{$ENDIF});
      if sfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else
        InvalidateRect(Handle, @rcInval, FALSE);
    end else begin
      { find the visible lines first }
      if (LastLine < FirstLine) then SwapInt(LastLine, FirstLine);
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine,
                      {$IFDEF SYN_LAZARUS}
                      ScreenRowToRow(LinesInWindow)
                      {$ELSE}
                      TopLine + LinesInWindow
                      {$ENDIF}
                      );
      { any line visible? }
      if (LastLine >= FirstLine) then begin
        rcInval := Rect(0,
          fTextHeight * {$IFDEF SYN_LAZARUS}RowToScreenRow(FirstLine)
                        {$ELSE}(FirstLine - TopLine){$ENDIF},
          fGutterWidth,
          fTextHeight * {$IFDEF SYN_LAZARUS}RowToScreenRow(LastLine+1)
                        {$ELSE}(LastLine - TopLine + 1){$ENDIF});
        if sfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else
          InvalidateRect(Handle, @rcInval, FALSE);
      end;
    end;
end;

procedure TCustomSynEdit.InvalidateLines(FirstLine, LastLine: integer);
var
  rcInval: TRect;
begin
  {$IFDEF SYN_LAZARUS}
  if sfPainting in fStateFlags then exit;
  {$ENDIF}
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then begin
      rcInval := ClientRect;
      rcInval.Left := fGutterWidth;
      if sfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else
        InvalidateRect(Handle, @rcInval, FALSE);
    end else begin
      { find the visible lines first }
      if (LastLine < FirstLine) then SwapInt(LastLine, FirstLine);
      FirstLine := Max(FirstLine, TopLine);
      LastLine := Min(LastLine,
                      {$IFDEF SYN_LAZARUS}
                      ScreenRowToRow(LinesInWindow)
                      {$ELSE}
                      TopLine + LinesInWindow
                      {$ENDIF}
                      );
      { any line visible? }
      if (LastLine >= FirstLine) then begin
        {$IFDEF SYN_LAZARUS}
        rcInval := Rect(fGutterWidth, fTextHeight * RowToScreenRow(FirstLine),
          ClientWidth-ScrollBarWidth,
          fTextHeight * (RowToScreenRow(LastLine)+1));
        {$ELSE}
        rcInval := Rect(fGutterWidth,fTextHeight * (FirstLine - TopLine),
          ClientWidth, fTextHeight * (LastLine - TopLine + 1));
        {$ENDIF}
        if sfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else
          InvalidateRect(Handle, @rcInval, FALSE);
      end;
    end;
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.InvalidateBracketHighlight(OnlyIfCaretMoved: boolean);
begin
  if OnlyIfCaretMoved
  and (CaretX=fBracketHighlightCaret.X)
  and (CaretY=fBracketHighlightCaret.Y) then
    exit;
  fBracketHighlightCaret:=CaretXY;
  // invalidate old bracket highlighting
  if fBracketHighlightPos.Y>0 then begin
    //DebugLn('TCustomSynEdit.InvalidateBracketHighlight A Y=',fBracketHighlightPos.Y,' X=',fBracketHighlightPos.X);
    InvalidateLines(fBracketHighlightPos.Y,fBracketHighlightPos.Y);
  end;
  if (fBracketHighlightAntiPos.Y>0)
  and (fBracketHighlightPos.Y<>fBracketHighlightAntiPos.Y) then
    InvalidateLines(fBracketHighlightAntiPos.Y,fBracketHighlightAntiPos.Y);
  fBracketHighlightPos.Y:=0;
  fBracketHighlightAntiPos.Y:=0;
  if eoBracketHighlight in Options then begin
    FindMatchingBracketPair(CaretXY,
                            fBracketHighlightPos,fBracketHighlightAntiPos,true);

    // invalidate new bracket highlighting
    if fBracketHighlightPos.Y>0 then begin
      //DebugLn('TCustomSynEdit.InvalidateBracketHighlight C ',
      //  ' Y=',fBracketHighlightPos.Y,' X=',fBracketHighlightPos.X,
      //  ' Y=',fBracketHighlightAntiPos.Y,' X=',fBracketHighlightAntiPos.X,
      //  '');
      InvalidateLines(fBracketHighlightPos.Y,fBracketHighlightPos.Y);
      if fBracketHighlightPos.Y<>fBracketHighlightAntiPos.Y then
        InvalidateLines(fBracketHighlightAntiPos.Y,fBracketHighlightAntiPos.Y);
    end;
  end;
end;

procedure TCustomSynEdit.FindMatchingBracketPair(const PhysCaret: TPoint;
  var StartBracket, EndBracket: TPoint; OnlyVisible: boolean);
var
  StartLine: string;
  LogCaretXY: TPoint;
begin
  StartBracket.Y:=-1;
  EndBracket.Y:=-1;
  if (PhysCaret.Y<1) or (PhysCaret.Y>Lines.Count) or (PhysCaret.X<1) then exit;
  StartLine := Lines[PhysCaret.Y - 1];
  LogCaretXY:=PhysicalToLogicalPos(PhysCaret);
  if (length(StartLine)<LogCaretXY.X)
  or (not (StartLine[LogCaretXY.X] in ['(',')','{','}','[',']'])) then exit;
  StartBracket:=PhysCaret;
  EndBracket:=FindMatchingBracket(PhysCaret,false,false,false,OnlyVisible);
end;
{$ENDIF}

procedure TCustomSynEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Data: pointer;
  C: char;
  Cmd: TSynEditorCommand;
begin
  {$IFDEF VerboseKeys}
  DebugLn('[TCustomSynEdit.KeyDown] ',dbgs(Key),' ',dbgs(Shift));
  {$ENDIF}
  inherited;
  {$IFDEF SYN_LAZARUS}
  if fLastControlIsPressed<>(GetKeyShiftState=[ssCtrl]) then
    UpdateCtrlMouse;
  {$ENDIF}
  Data := nil;
  C := #0;
  try
    Cmd := TranslateKeyCode(Key, Shift, Data);
    if Cmd <> ecNone then begin
      {$IFDEF SYN_LAZARUS}
      LastMouseCaret:=Point(-1,-1);
      {$ENDIF}
      //DebugLn('[TCustomSynEdit.KeyDown] key translated ',cmd);
      Key := 0; // eat it.
      Include(fStateFlags, sfIgnoreNextChar);
      CommandProcessor(Cmd, C, Data);
    end else
      Exclude(fStateFlags, sfIgnoreNextChar);
  finally
    if Data <> nil then
      FreeMem(Data);
  end;
  //DebugLn('[TCustomSynEdit.KeyDown] END ',dbgs(Key),' ',dbgs(Shift));
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  {$IFDEF VerboseKeys}
  DebugLn('[TCustomSynEdit.KeyUp] ',Key
    ,' Shift=',ssShift in Shift,' Ctrl=',ssCtrl in Shift,' Alt=',ssAlt in Shift);
  {$ENDIF}
  inherited KeyUp(Key, Shift);
  if fLastControlIsPressed<>(GetKeyShiftState=[ssCtrl]) then
    UpdateCtrlMouse;
end;
{$ENDIF}

procedure TCustomSynEdit.Loaded;
begin
  inherited Loaded;
  GutterChanged(Self);
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.UTF8KeyPress(var Key: TUTF8Char);
begin
  // don't fire the event if key is to be ignored
  if not (sfIgnoreNextChar in fStateFlags) then begin
    if Assigned(OnUTF8KeyPress) then OnUTF8KeyPress(Self, Key);
    // The key will be handled in UTFKeyPress always and KeyPress won't be called
    // so we we fire the OnKeyPress here
    if (ord(key[1])< %1100000) and (key[1]<>#0) and Assigned(OnKeyPress) then
      OnKeyPress(Self, Key[1]);
    {$IFDEF VerboseKeyboard}
    DebugLn('TCustomSynEdit.UTF8KeyPress ',DbgSName(Self),' Key="',DbgStr(Key),'" UseUTF8=',dbgs(UseUTF8));
    {$ENDIF}
    CommandProcessor(ecChar, Key, nil);
  end else
    // don't ignore further keys
    Exclude(fStateFlags, sfIgnoreNextChar);
  // Key was handled anyway, so eat it!
  Key:='';
end;
{$ENDIF}

procedure TCustomSynEdit.KeyPress(var Key: Char);
begin
  // don't fire the event if key is to be ignored
  if not (sfIgnoreNextChar in fStateFlags) then begin
    {$IFDEF VerboseKeyboard}
    DebugLn('TCustomSynEdit.KeyPress ',DbgSName(Self),' Key="',DbgStr(Key),'" UseUTF8=',dbgs(UseUTF8));
    {$ENDIF}
    if Assigned(OnKeyPress) then OnKeyPress(Self, Key);
    CommandProcessor(ecChar, Key, nil);
    {$IFNDEF SYN_LAZARUS}
    // Key was handled anyway, so eat it!
    Key:=#0;
    {$ENDIF}
  end else
    // don't ignore further keys
    Exclude(fStateFlags, sfIgnoreNextChar);
  {$IFDEF SYN_LAZARUS}
  // Key was handled anyway, so eat it!
  // MG: the comment was right, the implementation not consequent enough
  Key:=#0;
  {$ENDIF}
end;

function TCustomSynEdit.LeftSpaces(const Line: string): Integer;
begin
  Result:=LeftSpaces(Line,false);
end;

function TCustomSynEdit.LeftSpaces(const Line: string;
  Physical: boolean): Integer;
var
  p: PChar;
begin
  p := pointer(Line);
  if Assigned(p) and (eoAutoIndent in fOptions) then begin
    Result := 0;
    while p^ in [#1..#32] do begin
      Inc(p);
      Inc(Result);
    end;
    {$IFDEF SYN_LAZARUS}
    if Physical and (Result>0) then
      Result:=LogicalToPhysicalCol(Line,Result+1)-1;
    {$ENDIF}
  end else
    Result := 0;
end;

procedure TCustomSynEdit.LinesChanging(Sender: TObject);
begin
  Include(fStateFlags, sfLinesChanging);
end;

procedure TCustomSynEdit.LinesChanged(Sender: TObject);
begin
  Exclude(fStateFlags, sfLinesChanging);
  if HandleAllocated then begin
    UpdateScrollBars;
    SetBlockBegin({$IFDEF SYN_LAZARUS}PhysicalToLogicalPos(CaretXY)
                  {$ELSE}CaretXY{$ENDIF});
    InvalidateRect(Handle, @fInvalidateRect, False);
    FillChar(fInvalidateRect, SizeOf(TRect), 0);
    if fGutter.ShowLineNumbers and fGutter.AutoSize then
      fGutter.AutoSizeDigitCount(Lines.Count);
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
  end;
end;

procedure TCustomSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  bWasSel: boolean;
  bStartDrag: boolean;
  {$IFDEF SYN_LAZARUS}
  PrimarySelText: string;
  LogCaretXY: TPoint;
  {$ENDIF}
begin
//DebugLn('TCustomSynEdit.MouseDown START Mouse=',X,',',Y,' Caret=',CaretX,',',CaretY,', BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
  {$IFDEF SYN_LAZARUS}
  if (X>=ClientWidth-ScrollBarWidth) or (Y>=ClientHeight-ScrollBarWidth) then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    exit;
  end;
  LastMouseCaret:=PixelsToRowColumn(Point(X,Y));
  fMouseDownX := X;
  fMouseDownY := Y;
  {$ENDIF}
  Exclude(fStateFlags, sfPossibleGutterClick);
  if (Button = mbRight) and SelAvail then                                       //lt 2000-10-12
    exit;
  bWasSel := false;
  bStartDrag := FALSE;
  if Button = mbLeft then begin
    if ssDouble in Shift then Exit;
    if SelAvail then begin
      //remember selection state, as it will be cleared later
      bWasSel := true;
    end;
  end;
  {$IFDEF SYN_LAZARUS}
  if Button=mbMiddle then begin
    if ssDouble in Shift then Exit;
    PrimarySelText:=PrimarySelection.AsText;
  end;
  {$ENDIF}
  inherited MouseDown(Button, Shift, X, Y);
  ComputeCaret(X, Y);
  {$IFDEF SYN_LAZARUS}
  LogCaretXY:=PhysicalToLogicalPos(CaretXY);
  {$ENDIF}
  fLastCaretX := fCaretX;                                                       //mh 2000-10-19
  if Button = mbLeft then begin
    //DebugLn('TCustomSynEdit.MouseDown ',DbgSName(Self),' START CAPTURE');
    MouseCapture := True;
    //if mousedown occured in selected block then begin drag operation
    Exclude(fStateFlags, sfWaitForDragging);
    if bWasSel and (eoDragDropEditing in fOptions) and (X >= fGutterWidth + 2)
      and (SelectionMode = smNormal)
      and IsPointInSelection({$IFDEF SYN_LAZARUS}LogCaretXY{$ELSE}CaretXY{$ENDIF})
    then
      bStartDrag := TRUE;
    //debugln('TCustomSynEdit.MouseDown bStartDrag=',dbgs(bStartDrag),' MouseCapture=',dbgs(MouseCapture));
  end;
  if (Button = mbLeft) and bStartDrag then
    Include(fStateFlags, sfWaitForDragging)
  else begin
    {$IFDEF SYN_LAZARUS}
    if ((Button=mbLeft)
        or ((eoRightMouseMovesCursor in Options) and (Button=mbRight)))
    and ([sfDblClicked,sfTripleClicked,sfQuadClicked]*fStateFlags=[])
    {$ELSE}
    if (sfDblClicked in fStateFlags)
    {$ENDIF}
    then begin
      if ssShift in Shift then
        SetBlockEnd({$IFDEF SYN_LAZARUS}LogCaretXY
                    {$ELSE}CaretXY{$ENDIF})
      else begin
        SetBlockBegin({$IFDEF SYN_LAZARUS}LogCaretXY
                      {$ELSE}CaretXY{$ENDIF});
{begin}                                                                         //mh 2000-11-20
        if (eoAltSetsColumnMode in Options) and (SelectionMode <> smLine) then
        begin
          if ssAlt in Shift then
            SelectionMode := smColumn
          else
            SelectionMode := smNormal;
        end;
{end}                                                                           //mh 2000-11-20
      end;
    end;
    {$IFDEF SYN_LAZARUS}
    if (Button=mbMiddle)
    and ([sfDblClicked,sfTripleClicked,sfQuadClicked]*fStateFlags=[])
    and ((PrimarySelText<>'') or SelAvail)
    then begin
      fBlockBegin := LogCaretXY;
      fBlockEnd := LogCaretXY;
      //debugln('TCustomSynEdit.MouseDown Old SelText="',DbgStr(SelText),'" fBlockBegin=',dbgs(fBlockBegin),' fBlockEnd=',dbgs(fBlockEnd),' LogCaretXY=',dbgs(LogCaretXY));
      SelText:=PrimarySelText;
      //debugln('TCustomSynEdit.MouseDown New SelText="',DbgStr(SelText),'" fBlockBegin=',dbgs(fBlockBegin),' fBlockEnd=',dbgs(fBlockEnd),' LogCaretXY=',dbgs(LogCaretXY));
    end;
    {$ENDIF}
  end;
  {$IFDEF SYN_LAZARUS}
  if (X < fGutterWidth) and (Button=mbLeft) then begin
    Include(fStateFlags, sfPossibleGutterClick);
    DoOnGutterClick(X, Y);
  end;
  LCLIntf.SetFocus(Handle);
  UpdateCaret;
  {$ELSE}
  if (fMouseDownX < fGutterWidth) then
    Include(fStateFlags, sfPossibleGutterClick);
  Windows.SetFocus(Handle);
  {$ENDIF}
  //debugln('TCustomSynEdit.MouseDown END sfWaitForDragging=',dbgs(sfWaitForDragging in fStateFlags),' ');
end;

procedure TCustomSynEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Z: integer;
begin
  inherited MouseMove(Shift, x, y);

  if (X >= fGutterWidth)
    and (X < ClientWidth{$IFDEF SYN_LAZARUS}-ScrollBarWidth{$ENDIF})
    and (Y >= 0)
    and (Y < ClientHeight{$IFDEF SYN_LAZARUS}-ScrollBarWidth{$ENDIF})
  then begin
    if (Cursor <> crHandPoint) or (not (ssCtrl in Shift)) then
      Cursor := crIBeam;
  end
  else
    Cursor := crDefault;

  {$IFDEF SYN_LAZARUS}
  LastMouseCaret:=PixelsToRowColumn(Point(X,Y));
  {$ENDIF}

  //debugln('TCustomSynEdit.MouseMove sfWaitForDragging=',dbgs(sfWaitForDragging in fStateFlags),' MouseCapture=',dbgs(MouseCapture),' GetCaptureControl=',DbgSName(GetCaptureControl));
  if MouseCapture
  and (sfWaitForDragging in fStateFlags) then begin
    if (Abs(fMouseDownX - X) >= GetSystemMetrics(SM_CXDRAG))
      or (Abs(fMouseDownY - Y) >= GetSystemMetrics(SM_CYDRAG))
    then begin
      Exclude(fStateFlags, sfWaitForDragging);
      //debugln('TCustomSynEdit.MouseMove BeginDrag');
      BeginDrag({$IFDEF SYN_LAZARUS}true{$ELSE}false{$ENDIF});
    end;
  end else if (ssLeft in Shift)
  and MouseCapture
  then begin
    //DebugLn(' TCustomSynEdit.MouseMove CAPTURE Mouse=',dbgs(X),',',dbgs(Y),' Caret=',dbgs(CaretXY),', BlockBegin=',dbgs(BlockBegin),' BlockEnd=',dbgs(BlockEnd));
    if (X >= fGutterWidth)
      and (X < ClientWidth{$IFDEF SYN_LAZARUS}-ScrollBarWidth{$ENDIF})
      and (Y >= 0)
      and (Y < ClientHeight{$IFDEF SYN_LAZARUS}-ScrollBarWidth{$ENDIF})
    then
      ComputeCaret(X, Y);
    SetBlockEnd({$IFDEF SYN_LAZARUS}PhysicalToLogicalPos(CaretXY)
                {$ELSE}CaretXY{$ENDIF});
    // should we begin scrolling?
    Dec(X, fGutterWidth);
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
  {$IFDEF SYN_LAZARUS}
  end else if MouseCapture then begin
    MouseCapture:=false;
    fScrollTimer.Enabled := False;
  {$ENDIF}
  end;
end;

procedure TCustomSynEdit.ScrollTimerHandler(Sender: TObject);
var
  C: TPoint;
  {$IFDEF SYN_LAZARUS}
  CurMousePos: TPoint;
  Z: integer;
  {$ENDIF}
  X, Y: Integer;
begin
  {$IFNDEF SYN_LAZARUS}
  GetCursorPos(C);
  C := PixelsToRowColumn(ScreenToClient(C));
  {$ENDIF}
  // changes to line / column in one go
  IncPaintLock;
  try
    {$IFDEF SYN_LAZARUS}
    GetCursorPos(CurMousePos);
    CurMousePos:=ScreenToClient(CurMousePos);
    C := PixelsToLogicalPos(CurMousePos);
    // recalculate scroll deltas
    Dec(CurMousePos.X, fGutterWidth);
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
    {$ENDIF}
    if fScrollDeltaX <> 0 then begin
      LeftChar := LeftChar + fScrollDeltaX;
      X := LeftChar;
      if fScrollDeltaX > 0 then  // scrolling right?
        Inc(X, CharsInWindow);
      CaretXY := Point(X, C.Y);
      SetBlockEnd({$IFDEF SYN_LAZARUS}PhysicalToLogicalPos(CaretXY)
                  {$ELSE}CaretXY{$ENDIF});
    end;
    if fScrollDeltaY <> 0 then begin
      if GetKeyState(VK_SHIFT) < 0 then
        TopLine := TopLine + fScrollDeltaY * LinesInWindow
      else
        TopLine := TopLine + fScrollDeltaY;
      Y := TopLine;
      if fScrollDeltaY > 0 then  // scrolling down?
        Inc(Y, LinesInWindow - 1);
      CaretXY := Point(C.X, Y);
      SetBlockEnd({$IFDEF SYN_LAZARUS}PhysicalToLogicalPos(CaretXY)
                  {$ELSE}CaretXY{$ENDIF});
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
//DebugLn('TCustomSynEdit.MouseUp Mouse=',X,',',Y,' Caret=',CaretX,',',CaretY,', BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
  inherited MouseUp(Button, Shift, X, Y);
  fScrollTimer.Enabled := False;
  {$IFDEF SYN_LAZARUS}
  MouseCapture := False;
  if (X>=ClientWidth-ScrollBarWidth) or (Y>=ClientHeight-ScrollBarWidth) then
  begin
    exit;
  end;
  LastMouseCaret:=PixelsToRowColumn(Point(X,Y));
  {$ENDIF}
  if (Button = mbRight) and (Shift = [ssRight]) and Assigned(PopupMenu) then
  begin
    {$IFDEF SYN_LAZARUS}
    fStateFlags:=fStateFlags-[sfDblClicked,sfTripleClicked,sfQuadClicked,
                              sfPossibleGutterClick];
    {$ENDIF}
    exit;
  end;
  MouseCapture := False;
  if (sfPossibleGutterClick in fStateFlags) and (X < fGutterWidth)
    {$IFDEF SYN_LAZARUS}and (Button = mbLeft){$ENDIF} then
  begin
    {$IFNDEF SYN_LAZARUS}
    DoOnGutterClick(X, Y);
    {$ENDIF}
  end else
  if fStateFlags * [sfDblClicked,
      {$IFDEF SYN_LAZARUS}sfTripleClicked,sfQuadClicked,{$ENDIF}
      sfWaitForDragging] = [sfWaitForDragging] then
  begin
    ComputeCaret(X, Y);
    SetBlockBegin({$IFDEF SYN_LAZARUS}PhysicalToLogicalPos(CaretXY)
                  {$ELSE}CaretXY{$ENDIF});
    SetBlockEnd({$IFDEF SYN_LAZARUS}PhysicalToLogicalPos(CaretXY)
                  {$ELSE}CaretXY{$ENDIF});
    Exclude(fStateFlags, sfWaitForDragging);
  end;
  if (Button=mbLeft)
  and (fStateFlags * [sfWaitForDragging] = []) then
  begin
    {$IFDEF SYN_LAZARUS}
    AquirePrimarySelection;
    {$ENDIF}
  end;
  {$IFDEF SYN_LAZARUS}
  fStateFlags:=fStateFlags-[sfDblClicked,sfTripleClicked,sfQuadClicked,
                            sfPossibleGutterClick];
  {$ELSE}
  Exclude(fStateFlags, sfDblClicked);
  Exclude(fStateFlags, sfPossibleGutterClick);
  {$ENDIF}
  //DebugLn('TCustomSynEdit.MouseUp END Mouse=',X,',',Y,' Caret=',CaretX,',',CaretY,', BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
end;

procedure TCustomSynEdit.DoOnGutterClick(X, Y: integer);
var
  i     : integer;
  offs  : integer;
  line  : integer;
  allmrk: TSynEditMarks;
  mark  : TSynEditMark;
begin
  {$IFDEF SYN_LAZARUS}
  line := PixelsToRowColumn(Point(X, Y)).Y;
  //debugln('TCustomSynEdit.DoOnGutterClick A ',dbgs(line));
  if line <= Lines.Count then begin
    mark := nil;
    if Gutter.ShowCodeFolding and (X<Gutter.CodeFoldingWidth) then begin
      CodeFoldAction(line);
    end else begin
      Marks.GetMarksForLine(line, allmrk);
      offs := 0;
      for i := 1 to maxMarks do begin
        if assigned(allmrk[i]) then begin
          Inc(offs, BookMarkOptions.XOffset);
          if X < offs then begin
            mark := allmrk[i];
            break;
          end;
        end;
      end;
    end;
    if Assigned(fOnGutterClick) then begin
      // for compatibility invoke this only on the markable area
      fOnGutterClick(Self, X, Y, line, mark);
    end;
  end;
  {$ELSE}
  if Assigned(fOnGutterClick) then begin
    line := PixelsToRowColumn(Point(X, Y)).Y;
    if line <= Lines.Count then begin
      mark := nil;
      Marks.GetMarksForLine(line, allmrk);
      offs := 0;
      mark := nil;
      for i := 1 to maxMarks do begin
        if assigned(allmrk[i]) then begin
          Inc(offs, BookMarkOptions.XOffset);
          if X < offs then begin
            mark := allmrk[i];
            break;
          end;
        end;
      end; //for
      fOnGutterClick(Self, X, Y, line, mark);
    end;
  end;
  {$ENDIF}
end;

procedure TCustomSynEdit.Paint;
var
  rcClip, rcDraw: TRect;
  nL1, nL2, nC1, nC2: integer;
begin
  // Get the invalidated rect. Compute the invalid area in lines / columns.
  {$IFDEF SYN_LAZARUS}
  rcClip:=Rect(0,0,ClientWidth,ClientHeight);
  StartPaintBuffer(rcClip);
  Include(fStateFlags,sfPainting);
  {$ELSE}
  rcClip := Canvas.ClipRect;
  {$ENDIF}
  // columns
  nC1 := LeftChar;
  if (rcClip.Left > fGutterWidth + 2) then
    Inc(nC1, (rcClip.Left - fGutterWidth - 2) div CharWidth);
  nC2 := nC1 +
    (rcClip.Right - fGutterWidth - 2 + CharWidth - 1) div CharWidth;
  // lines
  nL1 := Max({$IFDEF SYN_LAZARUS}TopLine + rcClip.Top div fTextHeight
             {$ELSE}ScreenRowToRow(rcClip.Top div fTextHeight){$ENDIF},
             TopLine);
  nL2 := Min({$IFDEF SYN_LAZARUS}
             ScreenRowToRow((rcClip.Bottom-1) div fTextHeight+1),
             {$ELSE}
             TopLine + (rcClip.Bottom + fTextHeight - 1) div fTextHeight,
             {$ENDIF}
             Lines.Count);
  //DebugLn('TCustomSynEdit.Paint LinesInWindow=',dbgs(LinesInWindow),' nL1=',dbgs(nL1),' nL2=',dbgs(nL2));
  // Now paint everything while the caret is hidden.
  HideCaret;
  try
    // First paint the gutter area if it was (partly) invalidated.
    if (rcClip.Left < fGutterWidth) then begin
      rcDraw := rcClip;
      rcDraw.Right := fGutterWidth;
      PaintGutter(rcDraw, nL1, nL2);
    end;
    // Then paint the text area if it was (partly) invalidated.
    if (rcClip.Right > fGutterWidth) then begin
      rcDraw := rcClip;
      rcDraw.Left := Max(rcDraw.Left, fGutterWidth);
      PaintTextLines(rcDraw, nL1, nL2, nC1, nC2);
    end;
    PluginsAfterPaint(Canvas, rcDraw, nL1, nL2);
    // If there is a custom paint handler call it.
    DoOnPaint;
  finally
    {$IFDEF SYN_LAZARUS}
    EndPaintBuffer(rcClip);
    {$ENDIF}
    UpdateCaret;
    {$IFDEF SYN_LAZARUS}
    Exclude(fStateFlags,sfPainting);
    {$ENDIF}
  end;
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.CodeFoldAction(iLine: integer);
// iLine is 1 based as parameter
// and 0 based in the procedure below

  procedure UpdateFolded(var iLine: integer);
  var
    FoldType: TSynEditCodeFoldType;
    Level: LongInt;
    CurFoldType: TSynEditCodeFoldType;
    SLines: TSynEditStringList;
  begin
    SLines:=TSynEditStringList(fLines);
    FoldType:=SLines.FoldType[iLine];
    Level:=SLines.FoldEndLevel[iLine];
    if FoldType=cfCollapsed then begin
      // fold all lines including sub blocks
      inc(iLine);
      while (iLine<Lines.Count)
      and (SLines.FoldMinLevel[iLine]>=Level) do begin
        //debugln('UpdateFolded Fold ',dbgs(iLine),' ',Lines[iLine]);
        SLines.Folded[iLine]:=true;
        inc(iLine);
      end;
      // fold last line of block
      if (iLine<Lines.Count)
      and (SLines.FoldType[iLine]=cfEnd) then begin
        //debugln('UpdateFolded Fold END ',dbgs(iLine),' ',Lines[iLine]);
        SLines.Folded[iLine]:=true;
        inc(iLine);
      end;
    end else if FoldType=cfExpanded then begin
      // expand all lines of this block and all sub expanded blocks
      // sub blocks, that are collapsed, remain collapsed
      inc(iLine);
      while (iLine<Lines.Count)
      and (SLines.FoldMinLevel[iLine]>=Level) do begin
        //debugln('UpdateFolded Expand ',dbgs(iLine),' ',Lines[iLine]);
        SLines.Folded[iLine]:=false;
        CurFoldType:=SLines.FoldType[iLine];
        if CurFoldType in [cfExpanded,cfCollapsed] then
          UpdateFolded(iLine)
        else
          inc(iLine);
      end;
      // expand last line of block
      if (iLine<Lines.Count)
      and (SLines.FoldType[iLine]=cfEnd) then begin
        //debugln('UpdateFolded Expand END ',dbgs(iLine),' ',Lines[iLine]);
        SLines.Folded[iLine]:=false;
        inc(iLine);
      end;
    end;
  end;

var
  FoldType: TSynEditCodeFoldType;
begin
  if (iLine<=0) or (iLine>Lines.Count) then exit;
  dec(iLine);
  FoldType:=TSynEditStringList(fLines).FoldType[iLine];
  //debugln('TCustomSynEdit.CodeFoldAction A ',dbgs(iLine),' ',dbgs(ord(FoldType)));
  if FoldType in [cfExpanded,cfCollapsed] then begin
    if FoldType=cfExpanded then begin
      // collapse the branch
      FoldType:=cfCollapsed;
      //debugln('collapsing node: ',dbgs(iLine));
    end else begin
      // expand the branch
      //debugln('expanding node: ',dbgs(iLine));
      FoldType:=cfExpanded;
    end;
    //DebugLn(['TCustomSynEdit.CodeFoldAction iLine=',iLine]);
    TSynEditStringList(fLines).FoldType[iLine] := FoldType;
    UpdateFolded(iLine);

    Invalidate;
  end;
end;

function TCustomSynEdit.FindNextUnfoldedLine(iLine: integer; Down: boolean
  ): Integer;
// iLine is 1 based
begin
  Result:=iLine;
  while (Result>0) and (Result<=Lines.Count)
  and (TSynEditStringList(fLines).Folded[Result-1]) do
    if Down then inc(Result) else dec(Result);
end;

procedure TCustomSynEdit.UnfoldAll;
var
  SLines: TSynEditStringList;
  i: Integer;
begin
  SLines:=TSynEditStringList(Lines);
  for i:=0 to SLines.Count-1 do
    SLines.Folded[i]:=false;
  Invalidate;
end;

{$ENDIF}

procedure TCustomSynEdit.PaintGutter(AClip: TRect; FirstLine, LastLine: integer);
var
  i, iLine: integer;
  rcLine: TRect;
  bHasOtherMarks: boolean;
  aGutterOffs: PIntArray;
  s: string;
  dc: HDC;
  rcCodeFold: TRect;
  tmp: TSynEditCodeFoldType;
  CodeFoldOffset: Integer;

  procedure DrawMark(iMark: integer);
  {$IFDEF SYN_LAZARUS}
  var
    iLine: integer;
    itop : Longint;
    CurMark: TSynEditMark;
  begin
    iTop := 0;
    CurMark:=Marks[iMark];
    if (CurMark.Line>LastLine) or (CurMark.Line<FirstLine)
    or (CurMark.Line<1) or (CurMark.Line>Lines.Count) then
      exit;
    if TSynEditStringList(fLines).Folded[CurMark.Line-1] then
      exit;
    iLine := RowToScreenRow(CurMark.Line);

    if Assigned(fBookMarkOpt.BookmarkImages) and not CurMark.InternalImage
    then begin
      if (CurMark.ImageIndex <= fBookMarkOpt.BookmarkImages.Count) then begin
        if CurMark.IsBookmark = BookMarkOptions.DrawBookmarksFirst then
          aGutterOffs^[iLine] := CodeFoldOffset
        else if aGutterOffs^[iLine] = 0 then
          aGutterOffs^[iLine] := fBookMarkOpt.BookmarkImages.Width + CodeFoldOffset;
        if fTextHeight > fBookMarkOpt.BookmarkImages.Height then
          iTop := (fTextHeight - fBookMarkOpt.BookmarkImages.Height) div 2;
        with fBookMarkOpt do
          BookmarkImages.Draw(Canvas, LeftMargin + aGutterOffs^[iLine],
                           iTop + iLine * fTextHeight, CurMark.ImageIndex,true);

        Inc(aGutterOffs^[iLine], fBookMarkOpt.BookmarkImages.Width);
      end;
    end else
    begin
      if CurMark.ImageIndex in [0..9] then begin
        if not Assigned(fInternalImage) then begin
          fInternalImage := TSynInternalImage.Create('SynEditInternalImages',10);
        end;
        if (aGutterOffs^[iLine]=0) and Gutter.ShowCodeFolding then
          aGutterOffs^[iLine]:=Gutter.CodeFoldingWidth;
        fInternalImage.DrawMark(Canvas, CurMark.ImageIndex,
            fBookMarkOpt.LeftMargin + aGutterOffs^[iLine], iLine * fTextHeight,
            fTextHeight);
        Inc(aGutterOffs^[iLine], fBookMarkOpt.Xoffset);
      end;
    end;
  end;
  {$ELSE below: not SYN_LAZARUS}
  var
    iLine: integer;
    itop : Longint;
  begin
    iTop := 0;
    if Assigned(fBookMarkOpt.BookmarkImages) and not Marks[i].InternalImage
    then begin
      if Marks[iMark].ImageIndex <= fBookMarkOpt.BookmarkImages.Count then begin
        iLine := Marks[iMark].Line - TopLine;
//        if Marks[iMark].IsBookmark then
        if Marks[iMark].IsBookmark = BookMarkOptions.DrawBookmarksFirst then    //mh 2000-10-12
          aGutterOffs^[iLine] := 0
        else if aGutterOffs^[iLine] = 0 then
          aGutterOffs^[iLine] := fBookMarkOpt.XOffset;
        If fTextHeight > fBookMarkOpt.BookmarkImages.Height then
          iTop := (fTextHeight - fBookMarkOpt.BookmarkImages.Height) div 2;
        with fBookMarkOpt do
          if not TSynEditStringList(fLines).Folded[iLine] then
            BookmarkImages.Draw(Canvas, LeftMargin + aGutterOffs^[iLine], iTop + iLine * fTextHeight, Marks[iMark].ImageIndex,true);

        Inc(aGutterOffs^[iLine], fBookMarkOpt.XOffset);
      end;
    end else
    begin
      if Marks[iMark].ImageIndex in [0..9] then begin
        iLine := Marks[iMark].Line - TopLine;
        if not Assigned(fInternalImage) then begin
          fInternalImage := TSynInternalImage.Create('SynEditInternalImages',
            10);
        end;
        fInternalImage.DrawMark(Canvas, Marks[iMark].ImageIndex, fBookMarkOpt.LeftMargin + aGutterOffs^[iLine], iLine * fTextHeight, fTextHeight);
        Inc(aGutterOffs^[iLine], fBookMarkOpt.XOffset);
      end;
    end;
  end;
  {$ENDIF}

  procedure DrawNodeBox(rcCodeFold: TRect; Collapsed: boolean);
  const cNodeOffset = 3;
  var
    rcNode: TRect;
    ptCenter : TPoint;
    iSquare: integer;
  begin
    //center of the draw area
    ptCenter.X := (rcCodeFold.Left + rcCodeFold.Right) div 2;
    ptCenter.Y := (rcCodeFold.Top + rcCodeFold.Bottom) div 2;

    //make node rect square
    iSquare := Max(0, rcCodeFold.Bottom - rcCodeFold.Top - 14) div 2;

    //area of drawbox
    rcNode.Right := rcCodeFold.Right - cNodeOffset + 1;
    rcNode.Left := rcCodeFold.Left + cNodeOffset;
    rcNode.Top := rcCodeFold.Top + cNodeOffset + iSquare;
    rcNode.Bottom := rcCodeFold.Bottom - cNodeOffset - iSquare + 1;

    Canvas.Brush.Color:=clWhite;
    Canvas.Rectangle(rcNode);

    //draw bottom handle to paragraph line
    Canvas.MoveTo((rcNode.Left + rcNode.Right) div 2, rcNode.Bottom);
    Canvas.LineTo((rcNode.Left + rcNode.Right) div 2, rcCodeFold.Bottom);

    //draw unfolded sign in node box
    Canvas.MoveTo(ptCenter.X - 2, ptCenter.Y);
    Canvas.LineTo(ptCenter.X + 3, ptCenter.Y);

    //draw folded sign
    if Collapsed then
    begin
      Canvas.MoveTo(ptCenter.X, ptCenter.Y - 2);
      Canvas.LineTo(ptCenter.X, ptCenter.Y + 3);
    end;
  end;

  procedure DrawParagraphContinue(rcCodeFold: TRect);
  var
    iCenter : integer;
  begin
    //center of the draw area
    iCenter := (rcCodeFold.Left + rcCodeFold.Right) div 2;

    Canvas.MoveTo(iCenter, rcCodeFold.Top);
    Canvas.LineTo(iCenter, rcCodeFold.Bottom);
  end;

  procedure DrawParagraphEnd(rcCodeFold: TRect);
  var
    ptCenter : TPoint;
  begin
    //center of the draw area
    ptCenter.X := (rcCodeFold.Left + rcCodeFold.Right) div 2;
    ptCenter.Y := (rcCodeFold.Top + rcCodeFold.Bottom) div 2;

    Canvas.MoveTo(ptCenter.X, rcCodeFold.Top);
    Canvas.LineTo(ptCenter.X, ptCenter.Y);
    Canvas.LineTo(rcCodeFold.Right, ptCenter.Y);
  end;

begin
  if (FirstLine = 1) and (LastLine = 0) then
    LastLine := 1;
  // Changed to use fTextDrawer.BeginDrawing and fTextDrawer.EndDrawing only
  // when absolutely necessary.  Note: Never change brush / pen / font of the
  // canvas inside of this block (only through methods of fTextDrawer)!
  Canvas.Brush.Color := Gutter.Color;
  // If we have to draw the line numbers then we don't want to erase
  // the background first. Do it line by line with TextRect instead
  // and fill only the area after the last visible line.
  dc := Canvas.Handle;
  {$IFDEF SYN_LAZARUS}
  LCLIntf.SetBkColor(dc,Canvas.Brush.Color);
  if Gutter.ShowCodeFolding then
    CodeFoldOffset:=Gutter.CodeFoldingWidth
  else
    CodeFoldOffset:=0;
  {$ENDIF}
  if fGutter.ShowLineNumbers then begin
    fTextDrawer.BeginDrawing(dc);
    try
      fTextDrawer.SetBackColor(fGutter.Color);
      fTextDrawer.SetForeColor(Self.Font.Color);
      if fGutter.UseFontStyle then
        fTextDrawer.Style := Font.Style
      else
        fTextDrawer.Style := [];
      // prepare the rect initially
      rcLine := AClip;
      rcLine.Right := fGutterWidth - 2;
      //rcLine.Right := Max(rcLine.Right, fGutterWidth - 2);
      rcLine.Bottom := (FirstLine - TopLine) * fTextHeight;
      for iLine := FirstLine to LastLine do begin
        // next line rect
        rcLine.Top := rcLine.Bottom;
        // erase the background and draw the line number string in one go
        {$IFDEF SYN_LAZARUS}
        if not TSynEditStringList(fLines).Folded[iLine-1] then begin
          s := fGutter.FormatLineNumber(iLine);
          Inc(rcLine.Bottom, fTextHeight);
          fTextDrawer.ExtTextOut(CodeFoldOffset+fGutter.LeftOffset,
                     rcLine.Top, ETO_OPAQUE,rcLine,PChar(Pointer(S)),Length(S));
        end;
        {$ELSE}
        s := fGutter.FormatLineNumber(iLine);
        Inc(rcLine.Bottom, fTextHeight);
        Windows.ExtTextOut(DC, fGutter.LeftOffset, rcLine.Top, ETO_OPAQUE,
          @rcLine, PChar(s), Length(s), nil);
        {$ENDIF}
      end;
      // now erase the remaining area if any
      if AClip.Bottom > rcLine.Bottom then begin
        rcLine.Top := rcLine.Bottom;
        rcLine.Bottom := AClip.Bottom;
        with rcLine do
          fTextDrawer.ExtTextOut(Left, Top, ETO_OPAQUE, rcLine, nil, 0);
      end;
    finally
      fTextDrawer.EndDrawing;
    end;
  end else begin
    InternalFillRect(dc, AClip);
  end;

  //draw the code folding marks
  if fGutter.ShowCodeFolding then
  begin
    with Canvas do
    begin
      Pen.Color := clDkGray;
      Pen.Width := 1;

      rcLine.Bottom := (FirstLine - TopLine) * fTextHeight;
      for iLine := FirstLine to LastLine do
      begin
        //only draw visible items
        if not TSynEditStringList(fLines).Folded[iLine-1] then
        begin
          // next line rect
          rcLine.Top := rcLine.Bottom;

          Inc(rcLine.Bottom, fTextHeight);

          rcCodeFold.Left := 0;
          rcCodeFold.Right := 14;
          rcCodeFold.Top := rcLine.Top;
          rcCodeFold.Bottom := rcLine.Bottom;

          tmp := TSynEditStringList(fLines).FoldType[iLine-1];

          case tmp of
            cfCollapsed: DrawNodeBox(rcCodeFold, True);
            cfExpanded: DrawNodeBox(rcCodeFold, False);
            cfContinue: DrawParagraphContinue(rcCodeFold);
            cfEnd: DrawParagraphEnd(rcCodeFold);
          end;
        end;
      end;
    end;
  end;

  // the gutter separator if visible
  if AClip.Right >= fGutterWidth - 2 then
    with Canvas do begin
      Pen.Color := {$IFDEF SYN_LAZARUS}clWhite{$ELSE}clBtnHighlight{$ENDIF};
      Pen.Width := 1;
      with AClip do begin
        MoveTo(fGutterWidth - 2, Top);
        LineTo(fGutterWidth - 2, Bottom);
        Pen.Color := {$IFDEF SYN_LAZARUS}clDkGray{$ELSE}clBtnShadow{$ENDIF};
        MoveTo(fGutterWidth - 1, Top);
        LineTo(fGutterWidth - 1, Bottom);
      end;
    end;
  // now the gutter marks
  if BookMarkOptions.GlyphsVisible and (Marks.Count > 0)
    and (LastLine >= FirstLine)
  then begin
    aGutterOffs := AllocMem((LastLine - TopLine + 1) * SizeOf(integer));
    try
      // Instead of making a two pass loop we look while drawing the bookmarks
      // whether there is any other mark to be drawn
      bHasOtherMarks := FALSE;
      for i := 0 to Marks.Count - 1 do with Marks[i] do
        if Visible and (Line >= FirstLine) and (Line <= LastLine) then
        begin
          if IsBookmark <> BookMarkOptions.DrawBookmarksFirst then              //mh 2000-10-12
            bHasOtherMarks := TRUE
          else
            DrawMark(i);
        end;
      if bHasOtherMarks then
        for i := 0 to Marks.Count - 1 do with Marks[i] do
        begin
          if Visible and (IsBookmark <> BookMarkOptions.DrawBookmarksFirst)     //mh 2000-10-12
            and (Line >= FirstLine) and (Line <= LastLine)
          then
            DrawMark(i);
        end;
    finally
      FreeMem(aGutterOffs);
    end;
  end;
end;

procedure TCustomSynEdit.PaintTextLines(AClip: TRect; FirstLine, LastLine,
  FirstCol, LastCol: integer);
{$IFDEF SYN_LAZARUS}
// FirstLine, LastLine are based 1
// FirstCol, LastCol are screen based 1 without scrolling (physical position).
//  i.e. the real screen position is fTextOffset+Pred(FirstCol)*CharWidth
var
  bDoRightEdge: boolean; // right edge
  nRightEdge: integer;
    // selection info
  bSelectionVisible: boolean; // any selection visible?
  nSelL1, nSelCol1: integer; // start of selected area (physical)
  nSelL2, nSelCol2: integer; // end of selected area (physical)
    // info about normal and selected text and background colors
  bSpecialLine, bLineSelected: boolean;
  colFG, colBG: TColor;
  colSelFG, colSelBG: TColor;
  colEditorBG: TColor;
    // info about selection of the current line
  nSelStart, nSelEnd: integer; // start, end of selected area in current line (physical)
  bComplexLine: boolean; // selected and unselected area in current line
  FirstColLogical: integer; // FirstCol converted to logical in current line
  LastColLogical: integer; // LastCol converted to logical in current line
  SelStartLogical: integer; // nSelStart converted to logical in current line
  SelEndLogical: integer; // nSelEnd converted to logical in current line
    // painting the background and the text
  rcLine, rcToken: TRect;
  CurLine: integer; // line index for the loop
  TokenAccu: record
    Len, MaxLen: integer;
    CharsBefore: integer;
    PhysicalStartPos, PhysicalEndPos: integer;
    p: PChar;
    FG, BG: TColor;
    Style: TFontStyles;
  end;
  dc: HDC;

  ExpandedPaintToken: string; // used to create the string sent to TextDrawer

  // logical (byte) positions of highlight brackets, the X are zero based
  nBracketX: integer; // zero based (logical)
  nBracketY: integer; // one based
  nAntiBracketX: integer; // zero based (logical)
  nAntiBracketY: integer; // one based

  LinkFGCol: TColor;

{ local procedures }

  procedure SetTokenAccuLength;
  begin
    ReAllocMem(TokenAccu.p,TokenAccu.MaxLen+1);
    TokenAccu.p[TokenAccu.MaxLen]:=#0;
  end;

  procedure ComputeSelectionInfo;
  var
    p: TPoint;
  begin
    bSelectionVisible := FALSE;
    // Only if selection is visible anyway.
    if (not HideSelection or Self.Focused) then begin
      bSelectionVisible := TRUE;
      // Get the *real* start of the selected area.
      if (fBlockBegin.Y < fBlockEnd.Y) then begin
        nSelL1 := fBlockBegin.Y;
        nSelCol1 := fBlockBegin.X;
        nSelL2 := fBlockEnd.Y;
        nSelCol2 := fBlockEnd.X;
      end else if (fBlockBegin.Y > fBlockEnd.Y) then begin
        nSelL2 := fBlockBegin.Y;
        nSelCol2 := fBlockBegin.X;
        nSelL1 := fBlockEnd.Y;
        nSelCol1 := fBlockEnd.X;
      end else if (fBlockBegin.X <> fBlockEnd.X) then begin
        // No selection at all, or it is only on this line.
        nSelL1 := fBlockBegin.Y;
        nSelL2 := nSelL1;
        if (fBlockBegin.X < fBlockEnd.X) then begin
          nSelCol1 := fBlockBegin.X;
          nSelCol2 := fBlockEnd.X;
        end else begin
          nSelCol2 := fBlockBegin.X;
          nSelCol1 := fBlockEnd.X;
        end;
      end else
        bSelectionVisible := FALSE;
      // If there is any visible selection so far, then test if there is an
      // intersection with the area to be painted.
      if bSelectionVisible then begin
        // Don't care if the selection is not visible.
        bSelectionVisible := (nSelL2 >= FirstLine) and (nSelL1 <= LastLine);
        // In the column selection mode sort the begin and end of the selection,
        // this makes the painting code simpler.
        if (SelectionMode = smColumn) and (nSelCol1 > nSelCol2) then
          SwapInt(nSelCol1, nSelCol2);
        if bSelectionVisible then begin
          // Transform the selection from text space into screen space
          p := LogicalToPhysicalPos(Point(nSelCol1, nSelL1));
          nSelCol1 := p.x;
          nSelL1 := p.y;
          p := LogicalToPhysicalPos(point(nSelCol2, nSelL2));
          nSelCol2 := p.x;
          nSelL2 := p.y;
        end;
      end;
    end;
  end;

  procedure SetDrawingColors(Selected: boolean);
  begin
    with fTextDrawer do
      if Selected then begin
        SetBackColor(colSelBG);
        SetForeColor(colSelFG);
      end else begin
        SetBackColor(colBG);
        SetForeColor(colFG);
      end;
  end;

  function ScreenColumnToXValue(Col: integer): integer;
  // map screen column to screen pixel
  begin
    Result := fTextOffset + Pred(Col) * fCharWidth;
  end;

  procedure ExpandSpecialChars(var p: PChar; var Count: integer;
    PhysicalStartPos: integer);
  // if there are no tabs: keep p and Count untouched
  // if there are tabs: copy p into ExpandedPaintToken buffer,
  //                    convert tabs to spaces, and return the buffer
  var
    i: integer;
    TabCount, LengthNeeded: Integer;
    DestPos: Integer;
    SrcPos: Integer;
    Dest: PChar;
    c: Char;
    ScreenPos: Integer;
    SpaceCount: Integer;
    CharLen: Integer;
  begin
    TabCount:=0;
    for i:=0 to Count-1 do
      if p[i]=#9 then inc(TabCount);
    if TabCount=0 then exit;
    LengthNeeded:=Count+TabCount*8;
    if length(ExpandedPaintToken)<LengthNeeded then
      SetLength(ExpandedPaintToken,LengthNeeded+CharsInWindow);
    SrcPos:=0;
    DestPos:=0;
    ScreenPos:=PhysicalStartPos;
    Dest:=PChar(Pointer(ExpandedPaintToken));
    while SrcPos<Count do begin
      c:=p[SrcPos];
      case c of

      #196..#255:
        begin
          // could be UTF8 char
          if UseUTF8 then
            CharLen:=UTF8CharacterLength(@p[SrcPos])
          else
            CharLen:=1;
          for i:=1 to CharLen do begin
            Dest[DestPos]:=p[SrcPos];
            inc(DestPos);
            inc(SrcPos);
          end;
          inc(ScreenPos);
        end;

      #9:
        begin
          // tab char
          SpaceCount:=TabWidth - ((ScreenPos-1) mod TabWidth);
          //debugln('ExpandSpecialChars SpaceCount=',dbgs(SpaceCount),' TabWidth=',dbgs(TabWidth),' ScreenPos=',dbgs(ScreenPos));
          for i:=1 to SpaceCount do begin
            Dest[DestPos]:=FTabChar;
            inc(DestPos);
            inc(ScreenPos);
          end;
          inc(SrcPos);
        end;

      else
        begin
          // normal char
          Dest[DestPos]:=p[SrcPos];
          inc(DestPos);
          inc(SrcPos);
          inc(ScreenPos);
        end;
      end;
    end;
    p:=PChar(Pointer(ExpandedPaintToken));
    Count:=DestPos;
    //debugln('ExpandSpecialChars Token with Tabs: "',DbgStr(copy(ExpandedPaintToken,1,Count)),'"');
  end;

  procedure PaintToken(
    Token: PChar;
    TokenLen,     // TokenLen is the maximum logical (byte) position for Token
    CharsBefore,  // CharsBefore tells if Token starts at column one or not
    FirstPhysical,// FirstPhysical is the physical (screen without scrolling)
                  //   column of the first character
    First, Last   // First, Last minus CharsBefore are logical (byte) positions in Token
    : integer);
  var
    pszText: PChar;
    nCharsToPaint: integer;
    nX: integer;
  const
    ETOOptions = ETO_OPAQUE; // Note: clipping is slow and not needed
  begin
    {debugln('PaintToken A TokenLen=',dbgs(TokenLen),
      ' CharsBefore=',dbgs(CharsBefore),
      ' FirstPhysical=',dbgs(FirstPhysical),
      ' First='+dbgs(First),' Last=',dbgs(Last),
      ' Tok="'+copy(Token,First-CharsBefore,Last-First+1),'"',
      ' rcToken='+dbgs(rcToken.Left)+'-'+dbgs(rcToken.Right));}
    if (Last < First) or (rcToken.Right <= rcToken.Left) then exit;
    Dec(First, CharsBefore);
    Dec(Last, CharsBefore);
    if (First > TokenLen) then begin
      pszText := nil;
      nCharsToPaint := 0;
    end else begin
      pszText := PChar(@Token[First-1]);
      nCharsToPaint := Min(Last - First + 1, TokenLen - First + 1);
      ExpandSpecialChars(pszText,nCharsToPaint,FirstPhysical);
    end;
    // Draw the right edge under the text if necessary
    nX := ScreenColumnToXValue(FirstPhysical);
    if bDoRightEdge and (not (eoHideRightMargin in Options))
    and (nRightEdge<rcToken.Right) and (nRightEdge>=rcToken.Left)
    then begin
      // draw background
      InternalFillRect(dc,rcToken);
      // draw edge
      LCLIntf.MoveToEx(dc, nRightEdge, rcToken.Top, nil);
      LCLIntf.LineTo(dc, nRightEdge, rcToken.Bottom + 1);
      // draw text
      fTextDrawer.ExtTextOut(nX, rcToken.Top, ETOOptions-ETO_OPAQUE, rcToken,
        pszText, nCharsToPaint);
    end else begin
      // draw text with background
      //debugln('PaintToken nX=',dbgs(nX),' Token=',dbgstr(copy(pszText,1,nCharsToPaint)),' rcToken=',dbgs(rcToken));
      fTextDrawer.ExtTextOut(nX, rcToken.Top, ETOOptions, rcToken,
        pszText, nCharsToPaint);
    end;
    rcToken.Left := rcToken.Right;
  end;

  procedure PaintHighlightToken(bFillToEOL: boolean);
  var
    bComplexToken: boolean;
    nC1, nC2, nC1Sel, nC2Sel: integer; // logical (byte)
    bU1, bSel, bU2: boolean;
    nX1, nX2: integer;
    C1Phys: integer;
    C1SelPhys: integer;
    C2Phys: integer;
    C2SelPhys: LongInt;
  begin
    // Compute some helper variables.
    nC1 := Max(FirstColLogical, TokenAccu.CharsBefore + 1);
    nC2 := Min(LastColLogical, TokenAccu.CharsBefore + TokenAccu.Len);
    if bComplexLine then begin
      bU1 := (nC1 < SelStartLogical);
      bSel := (nC1 < SelEndLogical) and (nC2 >= SelStartLogical);
      bU2 := (nC2 >= SelEndLogical);
      bComplexToken := bSel and (bU1 or bU2);
    end else begin
      bU1 := False;
      bSel := bLineSelected;
      bU2 := False;
      bComplexToken := FALSE;
    end;
    {debugln('PaintHighlightToken A TokenAccu: CharsBefore=',dbgs(TokenAccu.CharsBefore),
      ' Len=',dbgs(TokenAccu.Len),
      ' PhysicalStartPos=',dbgs(TokenAccu.PhysicalStartPos),
      ' PhysicalEndPos=',dbgs(TokenAccu.PhysicalEndPos),
      ' Complex=',dbgs(bComplexToken),
      ' "',copy(TokenAccu.s,1,TokenAccu.Len),'"');}

    // Any token chars accumulated?
    if (TokenAccu.Len > 0) then begin
      // Initialize the colors and the font style.
      if not bSpecialLine then begin
        colBG := TokenAccu.BG;
        colFG := TokenAccu.FG;
      end;
      fTextDrawer.SetStyle(TokenAccu.Style);
      // Paint the chars
      if bComplexToken then begin
        // first unselected part of the token
        C1Phys := Max(FirstCol, TokenAccu.PhysicalStartPos);
        if bU1 then begin
          SetDrawingColors(FALSE);
          rcToken.Right := ScreenColumnToXValue(nSelStart);
          with TokenAccu do
            PaintToken(p,Len,CharsBefore,C1Phys,nC1,SelStartLogical-1);
        end;
        // selected part of the token
        SetDrawingColors(TRUE);
        nC1Sel := Max(SelStartLogical, nC1);
        nC2Sel := Min(SelEndLogical, nC2+1);
        C2Phys := Min(LastCol, TokenAccu.PhysicalEndPos+1);
        C1SelPhys := Max(nSelStart, C1Phys);
        C2SelPhys := Min(nSelEnd, C2Phys);
        rcToken.Right := ScreenColumnToXValue(C2SelPhys);
        with TokenAccu do PaintToken(p,Len,CharsBefore,C1SelPhys,nC1Sel,nC2Sel-1);
        // second unselected part of the token
        if bU2 then begin
          SetDrawingColors(FALSE);
          rcToken.Right := ScreenColumnToXValue(C2Phys);
          with TokenAccu do
            PaintToken(p,Len,CharsBefore,nSelEnd,SelEndLogical,nC2);
        end;
      end else begin
        C1Phys := Max(FirstCol, TokenAccu.PhysicalStartPos);
        C2Phys := Min(LastCol, TokenAccu.PhysicalEndPos+1);
        SetDrawingColors(bSel);
        rcToken.Right := ScreenColumnToXValue(C2Phys);
        with TokenAccu do PaintToken(p, Len, CharsBefore, C1Phys, nC1, nC2);
      end;
    end;

    // Fill the background to the end of this line if necessary.
    if bFillToEOL and (rcToken.Left < rcLine.Right) then begin
      if not bSpecialLine then colBG := colEditorBG;
      if bComplexLine then begin
        nX1 := ScreenColumnToXValue(nSelStart);
        nX2 := ScreenColumnToXValue(nSelEnd);
        if (rcToken.Left < nX1) then begin
          SetDrawingColors(FALSE);
          rcToken.Right := nX1;
          InternalFillRect(dc, rcToken);
          rcToken.Left := nX1;
        end;
        if (rcToken.Left < nX2) then begin
          SetDrawingColors(TRUE);
          rcToken.Right := nX2;
          InternalFillRect(dc, rcToken);
          rcToken.Left := nX2;
        end;
        if (rcToken.Left < rcLine.Right) then begin
          SetDrawingColors(FALSE);
          rcToken.Right := rcLine.Right;
          InternalFillRect(dc, rcToken);
        end;
      end else begin
        SetDrawingColors(bLineSelected);
        rcToken.Right := rcLine.Right;
        InternalFillRect(dc, rcToken);
      end;
      // Draw the right edge if necessary.
      if bDoRightEdge and (not (eoHideRightMargin in Options))
      and (nRightEdge>=rcToken.Left) then begin
        LCLIntf.MoveToEx(dc, nRightEdge, rcToken.Top, nil);
        LCLIntf.LineTo(dc, nRightEdge, rcToken.Bottom + 1);
      end;
    end;
  end;

  procedure AddHighlightToken(
    Token: PChar;
    CharsBefore, TokenLen, PhysicalStartPos, PhysicalEndPos: integer;
    Foreground, Background: TColor;
    Style: TFontStyles);
  var
    bCanAppend: boolean;
    bSpacesTest, bIsSpaces: boolean;
    i: integer;

    function TokenIsSpaces: boolean;
    var
      pTok: PChar;
      SpaceCnt: Integer;
    begin
      if not bSpacesTest then begin
        bSpacesTest := TRUE;
        pTok := PChar(Pointer(Token));
        SpaceCnt:=0;
        while (pTok^ in [' ',#9]) do begin
          Inc(pTok);
          inc(SpaceCnt);
        end;
        bIsSpaces := (SpaceCnt>=TokenLen);
      end;
      Result := bIsSpaces;
    end;

  begin
    {DebugLn('AddHighlightToken A CharsBefore=',dbgs(CharsBefore),
      ' TokenLen=',dbgs(TokenLen),
      ' PhysicalStartPos=',dbgs(PhysicalStartPos),' PhysicalEndPos=',dbgs(PhysicalEndPos),
      ' Tok="',copy(Token,1,TokenLen),'"');}
    if Background = clNone then Background := colEditorBG;
    if Foreground = clNone then Foreground := Font.Color;
    // Do we have to paint the old chars first, or can we just append?
    bCanAppend := FALSE;
    bSpacesTest := FALSE;
    if (TokenAccu.Len > 0) then begin
      // font style must be the same or token is only spaces
      if (TokenAccu.Style = Style)
      or (not (fsUnderline in Style) and not (fsUnderline in TokenAccu.Style)
          and TokenIsSpaces)
      then
        // either special colors or same colors
        if bSpecialLine or bLineSelected or
        // background color must be the same and
        ((TokenAccu.BG = Background) and
        // foreground color must be the same or token is only spaces
        ((TokenAccu.FG = Foreground) or TokenIsSpaces))
        then
          bCanAppend := TRUE;
      // If we can't append it, then we have to paint the old token chars first.
      if not bCanAppend then
        PaintHighlightToken(FALSE);
    end;
    // Don't use AppendStr because it's more expensive.
    if bCanAppend then begin
      if (TokenAccu.Len + TokenLen > TokenAccu.MaxLen) then begin
        TokenAccu.MaxLen := TokenAccu.Len + TokenLen + 32;
        SetTokenAccuLength;
      end;
      for i := 0 to TokenLen-1 do begin
        TokenAccu.p[TokenAccu.Len + i] := Token[i];
      end;
      Inc(TokenAccu.Len, TokenLen);
      TokenAccu.PhysicalEndPos := PhysicalEndPos;
    end else begin
      TokenAccu.Len := TokenLen;
      if (TokenAccu.Len > TokenAccu.MaxLen) then begin
        TokenAccu.MaxLen := TokenAccu.Len + 32;
        SetTokenAccuLength;
      end;
      for i := 0 to TokenLen-1 do begin
        TokenAccu.p[i] := Token[i];
      end;
      TokenAccu.CharsBefore := CharsBefore;
      TokenAccu.PhysicalStartPos := PhysicalStartPos;
      TokenAccu.PhysicalEndPos := PhysicalEndPos;
      TokenAccu.FG := Foreground;
      TokenAccu.BG := Background;
      TokenAccu.Style := Style;
    end;
    {debugln('AddHighlightToken END bCanAppend=',dbgs(bCanAppend),
      ' TokenAccu: CharsBefore=',dbgs(TokenAccu.CharsBefore),
      ' Len=',dbgs(TokenAccu.Len),
      ' PhysicalStartPos=',dbgs(TokenAccu.PhysicalStartPos),
      ' PhysicalEndPos=',dbgs(TokenAccu.PhysicalEndPos),
      ' "',copy(TokenAccu.s,1,TokenAccu.Len),'"');}
  end;

  procedure DrawHilightBracketToken(attr: TSynHighlighterAttributes;
    sToken: PChar; nLine, nTokenPos, nTokenLen,
    PhysicalStartPos: integer);
  // Bracket Highlighting
  var
    BracketFGCol, BracketBGCol: TColor;
    BracketStyle, TokenStyle: TFontStyles;

    procedure PaintSubToken(SubTokenLen: integer; Hilight: boolean);
    var
      PhysicalEndPos: integer;
      PaintStyle: TFontStyles;
    begin
      if SubTokenLen=0 then exit;
      if Hilight then
        PaintStyle:=BracketStyle
      else
        PaintStyle:=TokenStyle;
      PhysicalEndPos:=LogicalToPhysicalCol(sToken,nTokenLen,
        SubTokenLen+1,1,PhysicalStartPos)-1;
      AddHighlightToken(sToken, nTokenPos, SubTokenLen,
        PhysicalStartPos, PhysicalEndPos,
        BracketFGCol, BracketBGCol, PaintStyle);
      PhysicalStartPos:=PhysicalEndPos+1;
      inc(nTokenPos,SubTokenLen);
      dec(nTokenLen,SubTokenLen);
      inc(sToken,SubTokenLen);
    end;

  var
    LeftBracketX, RightBracketX, Dummy: integer;
  begin
    // get bracket positions
    if (nLine=nBracketY)
    and (nBracketX>=nTokenPos) and (nBracketX<nTokenPos+nTokenLen) then
      LeftBracketX:=nBracketX
    else
      LeftBracketX:=-1;
    if (nLine=nAntiBracketY)
    and (nAntiBracketX>=nTokenPos) and (nAntiBracketX<nTokenPos+nTokenLen) then
      RightBracketX:=nAntiBracketX
    else
      RightBracketX:=-1;
    if (LeftBracketX<0) and (RightBracketX>=0) then begin
      LeftBracketX:=RightBracketX;
      RightBracketX:=-1;
    end;
    if (RightBracketX>=0) and (RightBracketX<LeftBracketX) then begin
      Dummy:=LeftBracketX;
      LeftBracketX:=RightBracketX;
      RightBracketX:=Dummy;
    end;
    if LeftBracketX<0 then exit;

    // get style
    if Assigned(attr) then begin
      BracketFGCol:=attr.Foreground;
      BracketBGCol:=attr.Background;
      TokenStyle:=attr.Style;
      BracketStyle:=TokenStyle;
    end else begin
      BracketFGCol:=colFG;
      BracketBGCol:=colBG;
      TokenStyle:=Font.Style;
      BracketStyle:=TokenStyle;
    end;
    if fsBold in BracketStyle then
      Exclude(BracketStyle,fsBold)
    else
      Include(BracketStyle,fsBold);

    // draw non hilight left of token
    PaintSubToken(LeftBracketX-nTokenPos,false);
    // draw left hilight bracket
    PaintSubToken(1,true);
    if RightBracketX>=0 then begin
      // draw middle
      PaintSubToken(RightBracketX-nTokenPos,false);
      // draw right hilight bracket
      PaintSubToken(1,true);
    end;
    // draw rest
    PaintSubToken(nTokenLen,false);
  end;

  procedure DrawCtrlMouseToken(attr: TSynHighlighterAttributes;
    sToken: PChar; nLine, nTokenPos, nTokenLen,
    PhysicalStartPos, PhysicalEndPos: integer);
  var
    LinkBGCol: TColor;
    LinkStyle: TFontStyles;
    fRed, fGreen, fBlue: integer;
    {bRed, bGreen,} bBlue: integer;
    NewRed, NewGreen, NewBlue: integer;
  begin
    if Assigned(attr) then begin
      LinkFGCol:=attr.Foreground;
      LinkBGCol:=attr.Background;
      LinkStyle:=attr.Style;
    end else begin
      LinkFGCol:=colFG;
      LinkBGCol:=colBG;
      LinkStyle:=Font.Style;
    end;
    if LinkBGCol = clNone then LinkBGCol := colEditorBG;
    if LinkFGCol = clNone then LinkFGCol := Font.Color;

    // change FG color
    fRed  :=(LinkFGCol and $ff);
    fGreen:=(LinkFGCol shr 8) and $ff;
    fBlue :=(LinkFGCol shr 16) and $ff;
    //bRed  :=(LinkBGCol and $ff);
    //bGreen:=(LinkBGCol shr 8) and $ff;
    bBlue :=(LinkBGCol shr 16) and $ff;
    NewRed  :=fRed;
    NewGreen:=fGreen;
    NewBlue :=bBlue;
    if Abs(NewBlue-fBlue)<128 then
      NewBlue:=(255-fBlue) and $ff;
    LinkFGCol:=NewRed+(NewGreen shl 8)+(NewBlue shl 16);

    AddHighlightToken(sToken, nTokenPos, nTokenLen,
      PhysicalStartPos, PhysicalEndPos,
      LinkFGCol, LinkBGCol, LinkStyle);
  end;

  procedure PaintLines;
  var
    sLine: string; // the current line
    sToken: PChar; // highlighter token info
    nTokenPos, nTokenLen: integer;
    TokenPhysStart: integer; // nTokenPos converted to physical (screen)
    TokenPhysEnd: integer; // nTokenPos+nTokenLen converted to physical (screen)
    attr: TSynHighlighterAttributes;
    LastTokenPosLogical: Integer;
    LastTokenPosPhyscial: Integer;
    ypos: Integer;
  begin
    // Initialize rcLine for drawing. Note that Top and Bottom are updated
    // inside the loop. Get only the starting point for this.
    rcLine := AClip;
    rcLine.Bottom := (FirstLine - TopLine) * fTextHeight;
    // Make sure the token accumulator string doesn't get reassigned to often.
    if Assigned(fHighlighter) then begin
      TokenAccu.MaxLen := Max(128, fCharsInWindow * 4);
      SetTokenAccuLength;
    end;
    // Now loop through all the lines. The indices are valid for Lines.
    CurLine := FirstLine-1;
    while CurLine<LastLine do begin
      inc(CurLine);

      if TSynEditStringList(fLines).Folded[CurLine-1] then begin
        // this line is folded -> skip
        //debugln('line folded ',dbgs(CurLine));
        continue;
      end;

      // Get the line.
      sLine := Lines[CurLine - 1];
      // Get the information about the line selection. Three different parts
      // are possible (unselected before, selected, unselected after), only
      // unselected or only selected means bComplexLine will be FALSE. Start
      // with no selection, compute based on the visible columns.
      bComplexLine := FALSE;
      nSelStart := 0;
      nSelEnd := 0;
      SelStartLogical:= 0;
      SelEndLogical:= 0;
      // Does the selection intersect the visible area?
      if bSelectionVisible and (CurLine >= nSelL1) and (CurLine <= nSelL2) then begin
        // Default to a fully selected line. This is correct for the smLine
        // selection mode and a good start for the smNormal mode.
        nSelStart := FirstCol;
        nSelEnd := LastCol + 1;
        if (SelectionMode = smColumn) or
          ((SelectionMode = smNormal) and (CurLine = nSelL1))
        then
          if (nSelCol1 > LastCol) then begin
            nSelStart := 0;
            nSelEnd := 0;
          end else if (nSelCol1 > FirstCol) then begin
            nSelStart := nSelCol1;
            bComplexLine := TRUE;
          end;
        if (SelectionMode = smColumn) or
          ((SelectionMode = smNormal) and (CurLine = nSelL2))
        then
          if (nSelCol2 < FirstCol) then begin
            nSelStart := 0;
            nSelEnd := 0;
          end else if (nSelCol2 < LastCol) then begin
            nSelEnd := nSelCol2;
            bComplexLine := TRUE;
          end;
      end;
      //debugln('PaintLines A nSelStart=',dbgs(nSelStart),' nSelEnd=',dbgs(nSelEnd));

      // Update the rcLine rect to this line.
      rcLine.Top := rcLine.Bottom;
      Inc(rcLine.Bottom, fTextHeight);
      // Initialize the text and background colors, maybe the line should
      // use special values for them.
      colFG := Font.Color;
      colBG := colEditorBG;
      bSpecialLine := DoOnSpecialLineColors(CurLine, colFG, colBG);
      if bSpecialLine then begin
        // The selection colors are just swapped, like seen in Delphi.
        colSelFG := colBG;
        colSelBG := colFG;
      end else begin
        colSelFG := fSelectedColor.Foreground;
        colSelBG := fSelectedColor.Background;
      end;
      // Paint the lines depending on the assigned highlighter.
      bLineSelected := not bComplexLine and (nSelStart > 0);
      rcToken := rcLine;
      FirstColLogical:=PhysicalToLogicalCol(sLine,FirstCol);
      LastColLogical:=PhysicalToLogicalCol(sLine,LastCol,
                                           FirstColLogical,FirstCol);
      if nSelStart>0 then begin
        SelStartLogical:=PhysicalToLogicalCol(sLine,nSelStart,
                                              FirstColLogical,FirstCol);
        SelEndLogical:=PhysicalToLogicalCol(sLine,nSelEnd,
                                            SelStartLogical,nSelStart);
      end;
      if not Assigned(fHighlighter) then begin
        // Note: The PaintToken procedure will take care of invalid parameters
        // like empty token rect or invalid indices into sLine.
        nTokenLen := Length(sLine);
        if bComplexLine then begin
          SetDrawingColors(FALSE);
          // paint unselected text in front of selection
          rcToken.Left := Max(rcLine.Left, ScreenColumnToXValue(FirstCol));
          rcToken.Right := Min(rcLine.Right, ScreenColumnToXValue(nSelStart));
          PaintToken(PChar(Pointer(sLine)), nTokenLen, 0, FirstCol,
                     FirstColLogical, SelStartLogical-1);
          // paint unselected text behind selection
          rcToken.Left := Max(rcLine.Left, ScreenColumnToXValue(nSelEnd));
          rcToken.Right := Min(rcLine.Right, ScreenColumnToXValue(LastCol));
          PaintToken(PChar(Pointer(sLine)), nTokenLen, 0, nSelEnd,
                     SelEndLogical, LastColLogical);
          // paint selection
          SetDrawingColors(TRUE);
          rcToken.Left := Max(rcLine.Left, ScreenColumnToXValue(nSelStart));
          rcToken.Right := Min(rcLine.Right, ScreenColumnToXValue(nSelEnd));
          PaintToken(PChar(Pointer(sLine)), nTokenLen, 0, nSelStart,
                     SelStartLogical, SelEndLogical-1);
        end else begin
          SetDrawingColors(bLineSelected);
          PaintToken(PChar(Pointer(sLine)), nTokenLen, 0, FirstCol,
                     FirstColLogical, LastColLogical);
        end;
      end else begin
        // Initialize highlighter with line text and range info. It is
        // necessary because we probably did not scan to the end of the last
        // line - the internal highlighter range might be wrong.
        fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[CurLine - 1]);     //mh 2000-10-10
        fHighlighter.SetLine(sLine, CurLine - 1);
        // Try to concatenate as many tokens as possible to minimize the count
        // of ExtTextOut calls necessary. This depends on the selection state
        // or the line having special colors. For spaces the foreground color
        // is ignored as well.
        TokenAccu.Len := 0;
        LastTokenPosLogical:=1;
        LastTokenPosPhyscial:=1;
        //debugln('PaintLines A FirstColLogical=',dbgs(FirstColLogical));
        while not fHighlighter.GetEol do begin
          // Test first whether anything of this token is visible.
          nTokenPos := fHighlighter.GetTokenPos; // zero-based
          fHighlighter.GetTokenEx(sToken,nTokenLen);
          //debugln('Paintlines B nTokenPos=',dbgs(nTokenPos),' nTokenLen=',dbgs(nTokenLen),' "',copy(sLine,nTokenPos+1,ntokenLen),'"');
          if (nTokenPos + nTokenLen + 1>= FirstColLogical) then begin
            // It's at least partially visible.
            // convert nTokenPos to physical (screen)
            TokenPhysStart:=LogicalToPhysicalCol(PChar(sLine),length(sLine),
                                      nTokenPos+1,
                                      LastTokenPosLogical,LastTokenPosPhyscial);
            LastTokenPosLogical:=nTokenPos+nTokenLen+1;
            LastTokenPosPhyscial:=LogicalToPhysicalCol(
                                PChar(sLine),length(sLine),
                                LastTokenPosLogical,nTokenPos+1,TokenPhysStart);
            TokenPhysEnd:=LastTokenPosPhyscial-1;
            {debugln('Paintlines C nTokenPos=',dbgs(nTokenPos),' nTokenLen=',dbgs(nTokenLen),' "',copy(sLine,nTokenPos+1,ntokenLen),'"',
              ' TokenPhysStart=',dbgs(TokenPhysStart),' TokenPhysEnd=',dbgs(TokenPhysEnd));}
            // Get the token attributes now.
            attr := fHighlighter.GetTokenAttribute;
            // Store the token chars with the attributes in the TokenAccu
            // record. This will paint any chars already stored if there is
            // a (visible) change in the attributes.
            if (fLastCtrlMouseLinkY<>CurLine)
            or (nTokenPos+1<>fLastCtrlMouseLinkX1)
            then begin
              if ((nBracketY<>CurLine) or (nTokenPos+nTokenLen<=nBracketX)
                or (nTokenPos>nBracketX))
              and ((nAntiBracketY<>CurLine) or (nTokenPos+nTokenLen<=nAntiBracketX)
                or (nTokenPos>nAntiBracketX)) then
              begin
                // normal token
                if Assigned(attr) then
                  AddHighlightToken(sToken, nTokenPos, nTokenLen,
                    TokenPhysStart, TokenPhysEnd,
                    attr.Foreground, attr.Background, attr.Style)
                else
                  AddHighlightToken(sToken, nTokenPos, nTokenLen,
                    TokenPhysStart, TokenPhysEnd,
                    colFG, colBG, Font.Style);
              end else begin
                // token with bracket hilighting
                DrawHilightBracketToken(attr,sToken,CurLine,
                  nTokenPos,nTokenLen,TokenPhysStart);
              end;
            end else begin
              // token is link
              DrawCtrlMouseToken(attr,sToken,CurLine,nTokenPos,nTokenLen,
                                 TokenPhysStart,TokenPhysEnd);
            end;
          end;
          // Let the highlighter scan the next token.
          fHighlighter.Next;
        end;
        // Draw anything that's left in the TokenAccu record. Fill to the end
        // of the invalid area with the correct colors.
        PaintHighlightToken(TRUE);
      end;

      // codefold draw splitter line
      if Gutter.ShowCodeFolding and (CurLine>=0)
      and (TSynEditStringList(Lines).FoldType[CurLine-1] in [cfEnd])
      and (TSynEditStringList(Lines).FoldEndLevel[CurLine-1] < CFDividerDrawLevel) then
      begin
        ypos := rcToken.Bottom - 1;
        LCLIntf.MoveToEx(dc, nRightEdge, ypos, nil);
        LCLIntf.LineTo(dc, fGutterWidth, ypos);
      end;
    end;
    CurLine:=-1;
  end;

  procedure InitializeHighlightBrackets;
  // test if caret over bracket and search anti bracket
  const
    Brackets: array[0..5] of char = ('(', ')', '[', ']', '{', '}');
  var
    sLine: string;
    i, PosX, PosY, Len: integer;
    CurChar, BracketInc, BracketDec: char;
    NumBrackets: integer;
    LogCaret: TPoint;
  begin
    // check for bracket under the cursor
    nBracketY:=0;
    nAntiBracketY:=0;
    if not (eoBracketHighlight in fOptions) then exit;
    if (fCaretY < FirstLine) or (fCaretY > LastLine) then exit;
    sLine := Lines[fCaretY - 1];
    Len := Length(sLine);
    LogCaret:=PhysicalToLogicalPos(CaretXY);
    if (LogCaret.X < 1) or (LogCaret.X > Len) then exit;
    BracketInc := sLine[LogCaret.X];
    if not (BracketInc in ['(',')','[',']','{','}']) then exit;
    nBracketY:=LogCaret.Y;
    nBracketX:=LogCaret.X-1; // zero based
    // find antibracket
    NumBrackets := 1;
    PosX:=LogCaret.X;
    PosY:=LogCaret.Y;
    i:=0;
    while Brackets[i]<>BracketInc do inc(i);
    BracketDec := Brackets[i xor 1]; // 0 -> 1, 1 -> 0, ...
    if Odd(i) then begin
      // closing bracket -> search opening bracket
      repeat
        // search until start of line
        while PosX > 1 do begin
          Dec(PosX);
          CurChar := sLine[PosX];
          if CurChar=BracketInc then
            Inc(NumBrackets)
          else if CurChar=BracketDec then begin
            Dec(NumBrackets);
            if NumBrackets = 0 then begin
              // matching bracket found, set caret and bail out
              nAntiBracketX:=PosX-1; // zero based
              nAntiBracketY:=PosY;
              break;
            end;
          end;
        end;
        // get previous line if possible
        if (nAntiBracketY>0) or (PosY <=FirstLine) then break;
        Dec(PosY);
        sLine := Lines[PosY - 1];
        PosX := Length(sLine) + 1;
      until FALSE;
    end else begin
      // opening bracket -> search closing bracket
      repeat
        // search until end of line
        Len := Length(sLine);
        while PosX < Len do begin
          Inc(PosX);
          CurChar := sLine[PosX];
          if CurChar=BracketInc then
            Inc(NumBrackets)
          else if CurChar=BracketDec then begin
            Dec(NumBrackets);
            if NumBrackets = 0 then begin
              // matching bracket found, set caret and bail out
              nAntiBracketX:=PosX-1; // zero based
              nAntiBracketY:=PosY;
              break;
            end;
          end;
        end;
        // get next line if possible
        if (nAntiBracketY>0) or (PosY >= LastLine) then break;
        Inc(PosY);
        sLine := Lines[PosY - 1];
        PosX := 0;
      until FALSE;
    end;
    {debugln('InitializeHighlightBrackets END nBracketY=',dbgs(nBracketY),
      ' nBracketX=',dbgs(nBracketX),
      ' nAntiBracketY=',dbgs(nAntiBracketY),
      ' nAntiBracketX=',dbgs(nAntiBracketX));}
  end;

  procedure CalculateCtrlMouseLink;
  begin
    fLastCtrlMouseLinkY:=-1;
    if (not (eoShowCtrlMouseLinks in Options))
    or (fLastMouseCaret.X<1) or (fLastMouseCaret.Y<1)
    or (not fLastControlIsPressed) then
      exit;
    GetWordBoundsAtRowCol(PhysicalToLogicalPos(fLastMouseCaret),
                          fLastCtrlMouseLinkX1,fLastCtrlMouseLinkX2);
    if fLastCtrlMouseLinkX1=fLastCtrlMouseLinkX2 then
      exit;
    fLastCtrlMouseLinkY:=fLastMouseCaret.Y;
    LinkFGCol:=clBlue;
  end;

  procedure PaintCtrlMouseLinkLine;
  var
    LineLeft, LineTop, LineRight: integer;
    s: string;
    PhysLinkStart: LongInt;
    PhysLinkEnd: LongInt;
  begin
    if fLastCtrlMouseLinkY<1 then exit;
    LineTop:=(fLastCtrlMouseLinkY-TopLine+1)*fTextHeight-1;
    s:=Lines[fLastCtrlMouseLinkY-1];
    PhysLinkStart:=Max(FirstCol,LogicalToPhysicalCol(s,fLastCtrlMouseLinkX1));
    PhysLinkEnd:=Min(LastCol,LogicalToPhysicalCol(s,fLastCtrlMouseLinkX2));
    LineLeft:=ScreenColumnToXValue(PhysLinkStart);
    LineRight:=ScreenColumnToXValue(PhysLinkEnd);
    Canvas.Pen.Color:=LinkFGCol;
    Canvas.MoveTo(LineLeft,LineTop);
    Canvas.LineTo(LineRight,LineTop);
  end;

{ end local procedures }

var
  ypos : integer;
begin
  CurLine:=-1;
  FillChar(TokenAccu,SizeOf(TokenAccu),0);
  //DebugLn('TCustomSynEdit.PaintTextLines ',DbgSName(Self),' TopLine=',dbgs(TopLine));
  colEditorBG := Color;
  if Assigned(Highlighter) and Assigned(Highlighter.WhitespaceAttribute) then
  begin
    colBG := Highlighter.WhitespaceAttribute.Background;
    if colBG <> clNone then
      colEditorBG := colBG;
  end;
  // If the right edge is visible and in the invalid area, prepare to paint it.
  // Do this first to realize the pen when getting the dc variable.
  bDoRightEdge := FALSE;
  if (fRightEdge > 0) then begin // column value
    nRightEdge := fTextOffset + fRightEdge * fCharWidth; // pixel value
    if (nRightEdge >= AClip.Left) and (nRightEdge <= AClip.Right) then begin
      bDoRightEdge := TRUE;
    end;
    Canvas.Pen.Color := fRightEdgeColor;
    Canvas.Pen.Width := 1;
  end;
  // Do everything else with API calls. This (maybe) realizes the new pen color.
  dc := Canvas.Handle;

  // If anything of the two pixel space before the text area is visible, then
  // fill it with the component background color.
  if (AClip.Left < fGutterWidth + 2) then begin
    rcToken := AClip;
    rcToken.Left := Max(AClip.Left, fGutterWidth);
    rcToken.Right := fGutterWidth + 2;
    SetBkColor(dc,colEditorBG);
    InternalFillRect(dc, rcToken);
    // Adjust the invalid area to not include this area.
    AClip.Left := rcToken.Right;
  end;
  if (LastLine >= FirstLine) then begin
    InitializeHighlightBrackets;
    CalculateCtrlMouseLink;
    // Paint the visible text lines. To make this easier, compute first the
    // necessary information about the selected area: is there any visible
    // selected area, and what are its lines / columns?
    // Moved to two local procedures to make it easier to read.
    ComputeSelectionInfo;
    fTextDrawer.Style := Font.Style;
    fTextDrawer.BeginDrawing(dc);
    try
      PaintLines;
    finally
      fTextDrawer.EndDrawing;
    end;
  end;

  // If there is anything visible below the last line, then fill this as well.
  rcToken := AClip;
  rcToken.Top := (RowToScreenRow(LastLine)+1) * fTextHeight;
  if (rcToken.Top < rcToken.Bottom) then begin
    SetBkColor(dc, ColorToRGB(colEditorBG));
    InternalFillRect(dc, rcToken);
    // Draw the right edge if necessary.
    if bDoRightEdge and (not (eoHideRightMargin in Options)) then begin
      LCLIntf.MoveToEx(dc, nRightEdge, rcToken.Top, nil);
      LCLIntf.LineTo(dc, nRightEdge, rcToken.Bottom + 1);
    end;

    // codefold draw splitter line
    if Gutter.ShowCodeFolding and (LastLine<Lines.Count)
    and (TSynEditStringList(Lines).FoldType[LastLine-1] in [cfEnd])
    and (TSynEditStringList(Lines).FoldEndLevel[LastLine-1] < CFDividerDrawLevel) then
    begin
      ypos := rcToken.Bottom - 1;
      LCLIntf.MoveToEx(dc, nRightEdge, ypos, nil);
      LCLIntf.LineTo(dc, fGutterWidth, ypos);
    end;
  end;

  PaintCtrlMouseLinkLine;
  ReAllocMem(TokenAccu.p,0);
end;
{$ELSE below for NOT SYN_LAZARUS ----------------------------------------------}
var
  bDoRightEdge: boolean; // right edge
  nRightEdge: integer;
    // selection info
  bAnySelection: boolean; // any selection visible?
  nSelL1, nSelCol1: integer; // start of selected area
  nSelL2, nSelCol2: integer; // end of selected area
    // info about normal and selected text and background colors
  bSpecialLine, bLineSelected: boolean;
  colFG, colBG: TColor;
  colSelFG, colSelBG: TColor;
  colEditorBG: TColor;
    // info about selection of the current line
  nSelStart, nSelEnd: integer;
  bComplexLine: boolean;
    // painting the background and the text
  rcLine, rcToken: TRect;
  TokenAccu: record
    // Note: s is not managed as a string, it will only grow!!!
    // Never use AppendStr or "+", use Len and MaxLen instead and
    // copy the string chars directly. This is for efficiency.
    Len, MaxLen, CharsBefore: integer;
    s: string;
    FG, BG: TColor;
    Style: TFontStyles;
  end;
  dc: HDC;

{ local procedures }

  procedure ComputeSelectionInfo;
  var
    p: TPoint;
  begin
    bAnySelection := FALSE;
    // Only if selection is visible anyway.
    if (not HideSelection or Self.Focused) then begin
      bAnySelection := TRUE;
      // Get the *real* start of the selected area.
      if (fBlockBegin.Y < fBlockEnd.Y) then begin
        nSelL1 := fBlockBegin.Y;
        nSelCol1 := fBlockBegin.X;
        nSelL2 := fBlockEnd.Y;
        nSelCol2 := fBlockEnd.X;
      end else if (fBlockBegin.Y > fBlockEnd.Y) then begin
        nSelL2 := fBlockBegin.Y;
        nSelCol2 := fBlockBegin.X;
        nSelL1 := fBlockEnd.Y;
        nSelCol1 := fBlockEnd.X;
      end else if (fBlockBegin.X <> fBlockEnd.X) then begin
        // No selection at all, or it is only on this line.
        nSelL1 := fBlockBegin.Y;
        nSelL2 := nSelL1;
        if (fBlockBegin.X < fBlockEnd.X) then begin
          nSelCol1 := fBlockBegin.X;
          nSelCol2 := fBlockEnd.X;
        end else begin
          nSelCol2 := fBlockBegin.X;
          nSelCol1 := fBlockEnd.X;
        end;
      end else
        bAnySelection := FALSE;
      // If there is any visible selection so far, then test if there is an
      // intersection with the area to be painted.
      if bAnySelection then begin
      // Don't care if the selection is not visible.
        bAnySelection := (nSelL2 >= FirstLine) and (nSelL1 <= LastLine);
      // In the column selection mode sort the begin and end of the selection,
      // this makes the painting code simpler.
        if (SelectionMode = smColumn) and (nSelCol1 > nSelCol2) then
          SwapInt(nSelCol1, nSelCol2);
        if bAnySelection then begin
          // Transform the selection from text space into screen space
          p := LogicalToPhysicalPos(Point(nSelCol1, nSelL1));
          nSelCol1 := p.x;
          nSelL1 := p.y;
          p := LogicalToPhysicalPos(point(nSelCol2, nSelL2));
          nSelCol2 := p.x;
          nSelL2 := p.y;
        end;
      end;
    end;
  end;

  procedure SetDrawingColors(Selected: boolean);
  begin
    with fTextDrawer do
      if Selected then begin
        SetBackColor(colSelBG);
        SetForeColor(colSelFG);
      end else begin
        SetBackColor(colBG);
        SetForeColor(colFG);
      end;
  end;

  function ColumnToXValue(Col: integer): integer;
  // map screen column to screen pixel
  begin
    Result := fTextOffset + Pred(Col) * fCharWidth;
  end;

  procedure PaintToken(const Token: string;
    TokenLen, CharsBefore, First, Last: integer);
  // CharsBefore tells if Token starts at column one or not
  var
    pszText: PChar;
    nCharsToPaint: integer;
    nX: integer;
  const
    ETOOptions = ETO_CLIPPED or ETO_OPAQUE;
  begin
    if (Last >= First) and (rcToken.Right > rcToken.Left) then begin
      nX := ColumnToXValue(First);
      Dec(First, CharsBefore);
      Dec(Last, CharsBefore);
      if (First > TokenLen) then begin
        pszText := nil;
        nCharsToPaint := 0;
      end else begin
        {$IFDEF SYN_MBCSSUPPORT}
        if (First > 1) and (ByteType(Token, First) = mbTrailByte) then begin
          Dec(First);
          Dec(nX, fCharWidth);
        end;
        {$ENDIF}
        pszText := PChar(@Token[First]);
        nCharsToPaint := Min(Last - First + 1, TokenLen - First + 1);
      end;
      fTextDrawer.ExtTextOut(nX, rcToken.Top, ETOOptions, rcToken,
        pszText, nCharsToPaint);
      rcToken.Left := rcToken.Right;
    end;
  end;

  procedure PaintHighlightToken(bFillToEOL: boolean);
  var
    bComplexToken: boolean;
    nC1, nC2, nC1Sel, nC2Sel: integer;
    bU1, bSel, bU2: boolean;
    nX1, nX2: integer;
  begin
    // Compute some helper variables.
    nC1 := Max(FirstCol, TokenAccu.CharsBefore + 1);
    nC2 := Min(LastCol, TokenAccu.CharsBefore + TokenAccu.Len + 1);
    if bComplexLine then begin
      bU1 := (nC1 < nSelStart);
      bSel := (nC1 < nSelEnd) and (nC2 >= nSelStart);
      bU2 := (nC2 >= nSelEnd);
      bComplexToken := bSel and (bU1 or bU2);
    end else begin
      bU1 := FALSE; // to shut up Compiler warning Delphi 2
      bSel := bLineSelected;
      bU2 := FALSE; // to shut up Compiler warning Delphi 2
      bComplexToken := FALSE;
    end;
    // Any token chars accumulated?
    if (TokenAccu.Len > 0) then begin
      // Initialize the colors and the font style.
      if not bSpecialLine then begin
        colBG := TokenAccu.BG;
        colFG := TokenAccu.FG;
      end;
      fTextDrawer.SetStyle(TokenAccu.Style);
      // Paint the chars
      if bComplexToken then begin
        // first unselected part of the token
        if bU1 then begin
          SetDrawingColors(FALSE);
          rcToken.Right := ColumnToXValue(nSelStart);
          with TokenAccu do PaintToken(s, Len, CharsBefore, nC1, nSelStart);
        end;
        // selected part of the token
        SetDrawingColors(TRUE);
        nC1Sel := Max(nSelStart, nC1);
        nC2Sel := Min(nSelEnd, nC2);
        rcToken.Right := ColumnToXValue(nC2Sel);
        with TokenAccu do PaintToken(s, Len, CharsBefore, nC1Sel, nC2Sel);
        // second unselected part of the token
        if bU2 then begin
          SetDrawingColors(FALSE);
          rcToken.Right := ColumnToXValue(nC2);
          with TokenAccu do PaintToken(s, Len, CharsBefore, nSelEnd, nC2);
        end;
      end else begin
        SetDrawingColors(bSel);
        rcToken.Right := ColumnToXValue(nC2);
        with TokenAccu do PaintToken(s, Len, CharsBefore, nC1, nC2);
      end;
    end;
    // Fill the background to the end of this line if necessary.
    if bFillToEOL and (rcToken.Left < rcLine.Right) then begin
      if not bSpecialLine then colBG := colEditorBG;
      if bComplexLine then begin
        nX1 := ColumnToXValue(nSelStart);
        nX2 := ColumnToXValue(nSelEnd);
        if (rcToken.Left < nX1) then begin
          SetDrawingColors(FALSE);
          rcToken.Right := nX1;
          InternalFillRect(dc, rcToken);
          rcToken.Left := nX1;
        end;
        if (rcToken.Left < nX2) then begin
          SetDrawingColors(TRUE);
          rcToken.Right := nX2;
          InternalFillRect(dc, rcToken);
          rcToken.Left := nX2;
        end;
        if (rcToken.Left < rcLine.Right) then begin
          SetDrawingColors(FALSE);
          rcToken.Right := rcLine.Right;
          InternalFillRect(dc, rcToken);
        end;
      end else begin
        SetDrawingColors(bLineSelected);
        rcToken.Right := rcLine.Right;
        InternalFillRect(dc, rcToken);
      end;
    end;
  end;

  procedure AddHighlightToken(
    const Token: AnsiString;
    CharsBefore, TokenLen: integer;
    Foreground, Background: TColor;
    Style: TFontStyles);
  var
    bCanAppend: boolean;
    bSpacesTest, bIsSpaces: boolean;
    i: integer;

    function TokenIsSpaces: boolean;
    var
      pTok: PChar;
    begin
      if not bSpacesTest then begin
        bSpacesTest := TRUE;
        pTok := PChar(Token);
        while (pTok^ <> #0) do begin
          if (pTok^ <> ' ') then break;
          Inc(pTok);
        end;
        bIsSpaces := (pTok^ = #0);
      end;
      Result := bIsSpaces;
    end;

  begin
    if Background = clNone then Background := colEditorBG;
    if Foreground = clNone then Foreground := Font.Color;
    // Do we have to paint the old chars first, or can we just append?
    bCanAppend := FALSE;
    bSpacesTest := FALSE;
    if (TokenAccu.Len > 0) then begin
      // font style must be the same or token is only spaces
      if (TokenAccu.Style = Style)
        or (not (fsUnderline in Style) and not (fsUnderline in TokenAccu.Style)
          and TokenIsSpaces)
      then
        // either special colors or same colors
        if bSpecialLine or bLineSelected or
        // background color must be the same and
        ((TokenAccu.BG = Background) and
        // foreground color must be the same or token is only spaces
        ((TokenAccu.FG = Foreground) or TokenIsSpaces))
        then
          bCanAppend := TRUE;
      // If we can't append it, then we have to paint the old token chars first.
      if not bCanAppend then
        PaintHighlightToken(FALSE);
    end;
    // Don't use AppendStr because it's more expensive.
    if bCanAppend then begin
      if (TokenAccu.Len + TokenLen > TokenAccu.MaxLen) then begin
        TokenAccu.MaxLen := TokenAccu.Len + TokenLen + 32;
        SetLength(TokenAccu.s, TokenAccu.MaxLen);
      end;
      for i := 1 to TokenLen do begin
        TokenAccu.s[TokenAccu.Len + i] := Token[i];
      end;
      Inc(TokenAccu.Len, TokenLen);
    end else begin
      TokenAccu.Len := TokenLen;
      if (TokenAccu.Len > TokenAccu.MaxLen) then begin
        TokenAccu.MaxLen := TokenAccu.Len + 32;
        SetLength(TokenAccu.s, TokenAccu.MaxLen);
      end;
      for i := 1 to TokenLen do begin
        TokenAccu.s[i] := Token[i];
      end;
      TokenAccu.CharsBefore := CharsBefore;
      TokenAccu.FG := Foreground;
      TokenAccu.BG := Background;
      TokenAccu.Style := Style;
    end;
  end;

  procedure PaintLines;
  var
    nLine: integer; // line index for the loop
    sLine: string; // the current line (expanded)
//    pConvert: TConvertTabsProc;                                               //mh 2000-10-19
    sToken: string; // highlighter token info
    nTokenPos, nTokenLen: integer;
    attr: TSynHighlighterAttributes;
  begin
    // Initialize rcLine for drawing. Note that Top and Bottom are updated
    // inside the loop. Get only the starting point for this.
    rcLine := AClip;
    rcLine.Bottom := (FirstLine - TopLine) * fTextHeight;
    // Make sure the token accumulator string doesn't get reassigned to often.
    if Assigned(fHighlighter) then begin
      TokenAccu.MaxLen := Max(128, fCharsInWindow);
      SetLength(TokenAccu.s, TokenAccu.MaxLen);
    end;
{begin}                                                                         //mh 2000-10-19
    // Find the fastest function for the tab expansion.
    // pConvert := GetBestConvertTabsProc(fTabWidth);
    // Now loop through all the lines. The indices are valid for Lines.
    for nLine := FirstLine to LastLine do begin
      // Get the expanded line.
      // sLine := pConvert(Lines[nLine - 1], fTabWidth);
      sLine := TSynEditStringList(Lines).ExpandedStrings[nLine - 1];
{end}                                                                           //mh 2000-10-19
      // Get the information about the line selection. Three different parts
      // are possible (unselected before, selected, unselected after), only
      // unselected or only selected means bComplexLine will be FALSE. Start
      // with no selection, compute based on the visible columns.
      bComplexLine := FALSE;
      nSelStart := 0;
      nSelEnd := 0;
      // Does the selection intersect the visible area?
      if bAnySelection and (nLine >= nSelL1) and (nLine <= nSelL2) then begin
        // Default to a fully selected line. This is correct for the smLine
        // selection mode and a good start for the smNormal mode.
        nSelStart := FirstCol;
        nSelEnd := LastCol + 1;
        if (SelectionMode = smColumn) or
          ((SelectionMode = smNormal) and (nLine = nSelL1))
        then
          if (nSelCol1 > LastCol) then begin
            nSelStart := 0;
            nSelEnd := 0;
          end else if (nSelCol1 > FirstCol) then begin
            nSelStart := nSelCol1;
            bComplexLine := TRUE;
          end;
        if (SelectionMode = smColumn) or
          ((SelectionMode = smNormal) and (nLine = nSelL2))
        then
          if (nSelCol2 < FirstCol) then begin
            nSelStart := 0;
            nSelEnd := 0;
          end else if (nSelCol2 < LastCol) then begin
            nSelEnd := nSelCol2;
            bComplexLine := TRUE;
          end;
{$IFDEF SYN_MBCSSUPPORT}
        if (SelectionMode = smColumn) then
          MBCSGetSelRangeInLineWhenColumnSelectionMode(sLine, nSelStart,
            nSelEnd);
{$ENDIF}
      end;
      // Update the rcLine rect to this line.
      rcLine.Top := rcLine.Bottom;
      Inc(rcLine.Bottom, fTextHeight);
      // Initialize the text and background colors, maybe the line should
      // use special values for them.
      colFG := Font.Color;
      colBG := colEditorBG;
      bSpecialLine := DoOnSpecialLineColors(nLine, colFG, colBG);
      if bSpecialLine then begin
        // The selection colors are just swapped, like seen in Delphi.
        colSelFG := colBG;
        colSelBG := colFG;
      end else begin
        colSelFG := fSelectedColor.Foreground;
        colSelBG := fSelectedColor.Background;
      end;
      // Paint the lines depending on the assigned highlighter.
      bLineSelected := not bComplexLine and (nSelStart > 0);
      rcToken := rcLine;
      if not Assigned(fHighlighter) then begin
        // Note: The PaintToken procedure will take care of invalid parameters
        // like empty token rect or invalid indices into sLine.
        nTokenLen := Length(sLine);
        if bComplexLine then begin
          SetDrawingColors(FALSE);
          rcToken.Left := Max(rcLine.Left, ColumnToXValue(FirstCol));
          rcToken.Right := Min(rcLine.Right, ColumnToXValue(nSelStart));
          PaintToken(sLine, nTokenLen, 0, FirstCol, nSelStart);
          rcToken.Left := Max(rcLine.Left, ColumnToXValue(nSelEnd));
          rcToken.Right := Min(rcLine.Right, ColumnToXValue(LastCol));
          PaintToken(sLine, nTokenLen, 0, nSelEnd, LastCol);
          SetDrawingColors(TRUE);
          rcToken.Left := Max(rcLine.Left, ColumnToXValue(nSelStart));
          rcToken.Right := Min(rcLine.Right, ColumnToXValue(nSelEnd));
          PaintToken(sLine, nTokenLen, 0, nSelStart, nSelEnd);
        end else begin
          SetDrawingColors(bLineSelected);
          PaintToken(sLine, nTokenLen, 0, FirstCol, LastCol);
        end;
      end else begin
        // Initialize highlighter with line text and range info. It is
        // necessary because we probably did not scan to the end of the last
        // line - the internal highlighter range might be wrong.
//        fHighlighter.SetRange(Lines.Objects[nLine - 1]);
        fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[nLine - 1]);     //mh 2000-10-10
        fHighlighter.SetLine(sLine, nLine - 1);
        // Try to concatenate as many tokens as possible to minimize the count
        // of ExtTextOut calls necessary. This depends on the selection state
        // or the line having special colors. For spaces the foreground color
        // is ignored as well.
        TokenAccu.Len := 0;
        while not fHighlighter.GetEol do begin
          // Test first whether anything of this token is visible.
          nTokenPos := fHighlighter.GetTokenPos; // zero-based
          sToken := fHighlighter.GetToken;
          nTokenLen := Length(sToken);
          if (nTokenPos + nTokenLen >= FirstCol) then begin
            // It's at least partially visible. Get the token attributes now.
            attr := fHighlighter.GetTokenAttribute;
            // Store the token chars with the attributes in the TokenAccu
            // record. This will paint any chars already stored if there is
            // a (visible) change in the attributes.
            if Assigned(attr) then
              AddHighlightToken(sToken, nTokenPos, nTokenLen, attr.Foreground,
                attr.Background, attr.Style)
            else
              AddHighlightToken(sToken, nTokenPos, nTokenLen, colFG, colBG,
                Font.Style);
          end;
          // Let the highlighter scan the next token.
          fHighlighter.Next;
        end;
        // Draw anything that's left in the TokenAccu record. Fill to the end
        // of the invalid area with the correct colors.
        PaintHighlightToken(TRUE);
      end;
      // Now paint the right edge if necessary. We do it line by line to reduce
      // the flicker. Should not cost very much anyway, compared to the many
      // calls to ExtTextOut.
      if bDoRightEdge then begin
        Windows.MoveToEx(dc, nRightEdge, rcLine.Top, nil);
        Windows.LineTo(dc, nRightEdge, rcLine.Bottom + 1);
      //codefold draw splitter line
      ypos := rcToken.Bottom - 1;
      nLine := PixelsToRowColumn(Point(0, ypos)).Y;
      if TSynEditStringList(Lines).FoldType[nLine] in [cfEnd] then
      begin
        Windows.MoveToEx(dc, nRightEdge, ypos, nil);
        Windows.LineTo(dc, fGutterWidth, ypos);
      end;
      end;
    end;
  end;


{ end local procedures }

begin
  colEditorBG := Color;
  if Assigned(Highlighter) and Assigned(Highlighter.WhitespaceAttribute) then
  begin
    colBG := Highlighter.WhitespaceAttribute.Background;
    if colBG <> clNone then
      colEditorBG := colBG;
  end;
  // If the right edge is visible and in the invalid area, prepare to paint it.
  // Do this first to realize the pen when getting the dc variable.
  bDoRightEdge := FALSE;
  if (fRightEdge > 0) then begin // column value
    nRightEdge := fTextOffset + fRightEdge * fCharWidth; // pixel value
    if (nRightEdge >= AClip.Left) and (nRightEdge <= AClip.Right) then begin
      bDoRightEdge := TRUE;
      Canvas.Pen.Color := fRightEdgeColor;
      Canvas.Pen.Width := 1;
    end;
  end;
  // Do everything else with API calls. This (maybe) realizes the new pen color.
  dc := Canvas.Handle;

  // If anything of the two pixel space before the text area is visible, then
  // fill it with the component background color.
  if (AClip.Left < fGutterWidth + 2) then begin
    rcToken := AClip;
    rcToken.Left := Max(AClip.Left, fGutterWidth);
    rcToken.Right := fGutterWidth + 2;
    SetBkColor(dc,ColorToRGB(colEditorBG));
    InternalFillRect(dc, rcToken);
    // Adjust the invalid area to not include this area.
    AClip.Left := rcToken.Right;
  end;
  if (LastLine >= FirstLine) then begin
    // Paint the visible text lines. To make this easier, compute first the
    // necessary information about the selected area: is there any visible
    // selected area, and what are its lines / columns?
    // Moved to two local procedures to make it easier to read.
    ComputeSelectionInfo;
    fTextDrawer.Style := Font.Style;
    fTextDrawer.BeginDrawing(dc);
    try
      PaintLines;
    finally
      fTextDrawer.EndDrawing;
    end;
  end;

  // If there is anything visible below the last line, then fill this as well.
  rcToken := AClip;
  rcToken.Top := (LastLine - TopLine + 1) * fTextHeight;
  if (rcToken.Top < rcToken.Bottom) then begin
    SetBkColor(dc, ColorToRGB(colEditorBG));
    InternalFillRect(dc, rcToken);
    // Draw the right edge if necessary.
    if bDoRightEdge and (not (eoHideRightMargin in Options)) then begin
      Windows.MoveToEx(dc, nRightEdge, rcToken.Top, nil);
      Windows.LineTo(dc, nRightEdge, rcToken.Bottom + 1);

      //codefold draw splitter line
      ypos := rcToken.Bottom - 1;
      nLine := PixelsToRowColumn(Point(0, ypos)).Y;
      if TSynEditStringList(Lines).FoldType[nLine] in [cfEnd] then
      begin
        Windows.MoveToEx(dc, nRightEdge, ypos, nil);
        Windows.LineTo(dc, fGutterWidth, ypos);
      end;
    end;
  end;
end;
{$ENDIF not SYN_LAZARUS}

{$IFDEF SYN_LAZARUS}
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
{$ENDIF}

{$IFDEF SYN_LAZARUS}
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

procedure TCustomSynEdit.EraseBackground(DC: HDC);
begin
  // we are painting everything ourselves, so not need to erase background
end;
{$ENDIF}

procedure TCustomSynEdit.Update;
begin
  {$IFDEF SYN_LAZARUS}
  Invalidate;
  {$ELSE}
  Paint;
  inherited Update;
  {$ENDIF}
end;

procedure TCustomSynEdit.Invalidate;
begin
  //DebugLn('TCustomSynEdit.Invalidate A');
  //RaiseGDBException('');
  inherited Invalidate;
end;

procedure TCustomSynEdit.PasteFromClipboard;
var
  StartOfBlock: TPoint;
  EndOfBlock: TPoint;
{$IFDEF SYN_LAZARUS}
  MemStream: TMemoryStream;
  Buf: Pointer;
  BufSize: integer;
{$ELSE}
  Mem: HGLOBAL;
{$ENDIF}
  PasteMode: TSynSelectionMode;
  P: PChar;
  DummyTag: Integer;
begin
  BeginUndoBlock;                                                               //mh 2000-11-20
  try
    // Check for our special format first.
    if Clipboard.HasFormat(SynEditClipboardFormat) then begin
      {$IFDEF SYN_LAZARUS}
      MemStream:=TMemoryStream.Create;
      Buf:=nil;
      try
        Clipboard.GetFormat(SynEditClipboardFormat,MemStream);
        BufSize:=integer(MemStream.Size);
        if BufSize>=SizeOf(TSynSelectionMode)+1 then begin
          GetMem(Buf,BufSize+1);
          MemStream.Position:=0;
          MemStream.Read(Buf^,BufSize);
          P:=PChar(Buf);
          P[BufSize]:=#0;
      {$ELSE}
      Clipboard.Open;
      try
        Mem := Clipboard.GetAsHandle(SynEditClipboardFormat);
        P := GlobalLock(Mem);
        if P <> nil then begin
      {$ENDIF}
          if SelAvail then begin
//            fUndoList.AddChange(crSelDelete, fBlockBegin, fBlockEnd, SelText,
//              SelectionMode);
            fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,      //mh 2000-11-20
              SelectionMode);
          end;
          // Our format: SelectionMode value followed by text.
          // See CopyToClipboard
          PasteMode := PSynSelectionMode(P)^;
          inc(P, SizeOf(TSynSelectionMode));
          if SelAvail then begin
            StartOfBlock := minPoint(fBlockBegin, fBlockEnd);
            EndOfBlock := maxPoint(fBlockBegin, fBlockEnd);
            fBlockBegin := StartOfBlock;
            fBlockEnd := EndOfBlock;
            if SelectionMode = smLine then
              // Pasting always occurs at column 0 when current selection is
              // smLine type
              StartOfBlock.X := 1;
          end else
            StartOfBlock := {$IFDEF SYN_LAZARUS}
                            PhysicalToLogicalPos(CaretXY);
                            {$ELSE}
                            Point(CaretX, CaretY);
                            {$ENDIF}
          DummyTag := 0;
          SetSelTextPrimitive(PasteMode, P, @DummyTag);
          EndOfBlock := BlockEnd;
          if PasteMode <> smLine then
            fUndoList.AddChange(crPaste, StartOfBlock, EndOfBlock, SelText,
              PasteMode)
          else
            if CaretX = 1 then
              fUndoList.AddChange(crPaste, Point(1, StartOfBlock.y),
                Point(CharsInWindow, EndOfBlock.y - 1), SelText, smLine)
            else
              fUndoList.AddChange(crPaste, Point(1, StartOfBlock.y),
                EndOfBlock, SelText, smNormal);
          if PasteMode = smColumn then
            CaretXY := Point(Min(StartOfBlock.X, EndOfBlock.X),
              Max(StartOfBlock.Y, EndOfBlock.Y) + 1);
        end else
          raise ESynEditError.Create('Clipboard paste operation failed.');
      finally
        {$IFDEF SYN_LAZARUS}
        MemStream.Free;
        if Buf<>nil then FreeMem(Buf);
        {$ELSE}
        Clipboard.Close;
        {$ENDIF}
      end;
    // If our special format isn't there, check for regular text format.
    end else if Clipboard.HasFormat(CF_TEXT) then begin
      // Normal text is much easier...
      if SelAvail then begin
//        fUndoList.AddChange(crSelDelete, fBlockBegin, fBlockEnd, SelText,
//          SelectionMode);
        fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,          //mh 2000-11-20
          SelectionMode);
      end;
      StartOfBlock := minPoint(fBlockBegin, fBlockEnd);
      EndOfBlock := maxPoint(fBlockBegin, fBlockEnd);
      fBlockBegin := StartOfBlock;
      fBlockEnd := EndOfBlock;
      LockUndo;
      SelText := Clipboard.AsText;
      UnlockUndo;
      fUndoList.AddChange(crPaste, StartOfBlock, BlockEnd, SelText, smNormal);
    end;
  finally
    EndUndoBlock;                                                               //mh 2000-11-20
  end;
  EnsureCursorPosVisible;
  // Selection should have changed...
  StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.SelectAll;
var
  LastPt: TPoint;
begin
  LastPt := Point(1, Lines.Count);
  if LastPt.y > 0 then
    Inc(LastPt.x, Length(Lines[LastPt.y - 1]))
  else
    LastPt.y  := 1;
  SetCaretAndSelection(
    {$IFDEF SYN_LAZARUS}
    LogicalToPhysicalPos(LastPt),
    {$ELSE}
    LastPt,
    {$ENDIF}
    Point(1, 1), LastPt);
  // Selection should have changed...
  StatusChanged([scSelection]);
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.SelectToBrace;
begin
  FindMatchingBracket(CaretXY,true,true,true,false);
end;

procedure TCustomSynEdit.SelectLine;
begin
  SetLineBlock(CaretXY);
end;

procedure TCustomSynEdit.SelectParagraph;
begin
  SetParagraphBlock(CaretXY);
end;
{$ENDIF}

procedure TCustomSynEdit.SetBlockBegin(Value: TPoint);
// logical position (byte)
var
  nInval1, nInval2: integer;
  SelChanged: boolean;
begin
  Value.x := MinMax(Value.x, 1, fMaxLeftChar);
  Value.y := MinMax(Value.y, 1, Lines.Count);
  if (SelectionMode = smNormal) then
    if (Value.y >= 1) and (Value.y <= Lines.Count) then begin
      {$IFDEF SYN_LAZARUS}
      Value.x := AdjustBytePosToCharacterStart(Value.y,Value.x);
      {$ELSE}
      Value.x := Min(Value.x, Length(Lines[Value.y - 1]) + 1);
      {$ENDIF}
    end else
      Value.x := 1;
  if SelAvail then begin
    if fBlockBegin.Y < fBlockEnd.Y then begin
      nInval1 := Min(Value.Y, fBlockBegin.Y);
      nInval2 := Max(Value.Y, fBlockEnd.Y);
    end else begin
      nInval1 := Min(Value.Y, fBlockEnd.Y);
      nInval2 := Max(Value.Y, fBlockBegin.Y);
    end;
    fBlockBegin := Value;
    fBlockEnd := Value;
    InvalidateLines(nInval1, nInval2);
    SelChanged := TRUE;
  end else begin
    SelChanged := (fBlockBegin.X <> Value.X) or (fBlockBegin.Y <> Value.Y) or
                  (fBlockEnd.X <> Value.X) or (fBlockEnd.Y <> Value.Y);
    fBlockBegin := Value;
    fBlockEnd := Value;
  end;
  if SelChanged then
    StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.SetBlockEnd(Value: TPoint);
// logical position (byte)
var
  nLine: integer;
{$IFDEF SYN_MBCSSUPPORT}
  s: string;
{$ENDIF}
begin
  if not (eoNoSelection in Options) then begin
    Value.x := MinMax(Value.x, 1, fMaxLeftChar);
    Value.y := MinMax(Value.y, 1, Lines.Count);
    if (SelectionMode = smNormal) then
      if (Value.y >= 1) and (Value.y <= Lines.Count) then
        {$IFDEF SYN_LAZARUS}
        Value.x := AdjustBytePosToCharacterStart(Value.y,Value.x)
        {$ELSE}
        Value.x := Min(Value.x, Length(Lines[Value.y - 1]) + 1)
        {$ENDIF}
      else
        Value.x := 1;
    if (Value.X <> fBlockEnd.X) or (Value.Y <> fBlockEnd.Y) then begin
{$IFDEF SYN_MBCSSUPPORT}
      if Value.Y <= Lines.Count then begin
        s := Lines[Value.Y - 1];
        if (Length(s) >= Value.X) and (mbTrailByte = ByteType(s, Value.X)) then
          Dec(Value.X);
      end;
{$ENDIF}
      if (Value.X <> fBlockEnd.X) or (Value.Y <> fBlockEnd.Y) then begin
        if (SelectionMode = smColumn) and (Value.X <> fBlockEnd.X) then begin
          InvalidateLines(
            Min(fBlockBegin.Y, Min(fBlockEnd.Y, Value.Y)),
            Max(fBlockBegin.Y, Max(fBlockEnd.Y, Value.Y)));
          fBlockEnd := Value;
        end else begin
          nLine := fBlockEnd.Y;
          fBlockEnd := Value;
          if (SelectionMode <> smColumn) or (fBlockBegin.X <> fBlockEnd.X) then
            InvalidateLines(nLine, fBlockEnd.Y);
        end;
        StatusChanged([scSelection]);
      end;
    end;
  end;
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.SetBlockIndent(const AValue: integer);
begin
  if fBlockIndent=AValue then exit;
  fBlockIndent:=AValue;
end;
{$ENDIF}

procedure TCustomSynEdit.SetCaretX(Value: Integer);
begin
  SetCaretXY(Point(Value, CaretY));
  fLastCaretX := CaretX;                                                        //mh 2000-10-19
end;

procedure TCustomSynEdit.SetCaretY(Value: Integer);
begin
  if not (eoKeepCaretX in Options) then begin                                        //mh 2000-11-08
    fLastCaretX := fCaretX;
  end;
  SetCaretXY(Point(fLastCaretX{CaretX}, Value));                                //mh 2000-10-19
end;

function TCustomSynEdit.GetCaretXY: TPoint;
begin
  Result := Point(CaretX, CaretY);
end;

procedure TCustomSynEdit.SetCaretXY(Value: TPoint);
// physical position (screen)
var
  nMaxX: integer;
  {$IFDEF SYN_LAZARUS}
  Line: string;
  {$ENDIF}
begin
  nMaxX := fMaxLeftChar;
  if Value.Y > Lines.Count then
    Value.Y := Lines.Count;
  if Value.Y < 1 then begin
    // this is just to make sure if Lines stringlist should be empty
    Value.Y := 1;
    if not (eoScrollPastEol in fOptions) then
      nMaxX := 1;
  end else begin
    if not (eoScrollPastEol in fOptions) then begin
      {$IFDEF SYN_LAZARUS}
      Line:=Lines[Value.Y-1];
      nMaxX := PhysicalLineLength(PChar(Line),length(Line),true)+1;
      {$ELSE}
      nMaxX := Length(Lines[Value.Y - 1]) + 1;                                  //abc 2000-09-30
      {$ENDIF}
    end;
  end;
  if Value.X > nMaxX then
    Value.X := nMaxX;
  if Value.X < 1 then
    Value.X := 1;
  if (Value.X <> fCaretX) or (Value.Y <> fCaretY) then begin
    IncPaintLock;
    try
      // simply include the flags, fPaintLock is > 0
      if fCaretX <> Value.X then begin
        fCaretX := Value.X;
        Include(fStatusChanges, scCaretX);
      end;
      if fCaretY <> Value.Y then begin
        fCaretY := Value.Y;
        Include(fStatusChanges, scCaretY);
      end;
      EnsureCursorPosVisible;
      Include(fStateFlags, sfCaretChanged);
      {$IFDEF SYN_LAZARUS}
      {$ELSE}
      Include(fStateFlags, sfScrollbarChanged);
      {$ENDIF}
    finally
      DecPaintLock;
    end;
  end;
  {$IFDEF SYN_LAZARUS}
  fLastCaretX:=fCaretX;
  {$ENDIF}
end;

procedure TCustomSynEdit.SetFont(const Value: TFont);
var
  DC: HDC;
  Save: THandle;
  Metrics: TTextMetric;
  AveCW, MaxCW: Integer;
begin
  DebugLn('TCustomSynEdit.SetFont--------------------------------------------');
  DebugLn('  TCustomSynEdit.SetFont A1',Value.Name);
  DC := GetDC(0);
  Save := SelectObject(DC, Value.Handle);
  DebugLn('  TCustomSynEdit.SetFont A2',Value.Name);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, Save);
  ReleaseDC(0, DC);
  with Metrics do begin
    AveCW := tmAveCharWidth;
    MaxCW := tmMaxCharWidth;
  end;
  DebugLn(Format('  TCustomSynEdit.SetFont B %d,%d,%s', [AveCW,MaxCW, Value.Name]));
  case AveCW = MaxCW of
    True: inherited Font := Value;
    False:
      begin
        with fFontDummy do begin
          {$IFDEF SYN_LAZARUS}
          BeginUpdate;
          {$ENDIF}
          DebugLn('  TCustomSynEdit.SetFont C fFontDummy="',fFontDummy.Name,'"');
          Color := Value.Color;
          Pitch := fpFixed;
          Size := Value.Size;
          Style := Value.Style;
          {$IFDEF SYN_LAZARUS}
          EndUpdate;
          {$ENDIF}
        end;
        DebugLn(Format('  TCustomSynEdit.SetFont D AveCW=%d MaxCW=%d Value="%s" Value.Size=%d Value.Height=%d DummyHeight=%d fFontDummy="%s"', [AveCW, MaxCW, Value.Name, Value.Size, Value.Height, fFontDummy.Height, fFontDummy.Name]));
        inherited Font := fFontDummy;
      end;
  end;
  DebugLn(Format('  TCustomSynEdit.SetFont E "%s" Height=%d AveCW=%d MaxCW=%d CharWidth=%d', [Font.Name, Font.Height, AveCW, MaxCW, CharWidth]));
  if fGutter.ShowLineNumbers then GutterChanged(Self);
end;

procedure TCustomSynEdit.SetGutterWidth(Value: Integer);
begin
  Value := Max(Value, 0);
  if fGutterWidth <> Value then begin
    fGutterWidth := Value;
    fTextOffset := fGutterWidth + 2 - (LeftChar - 1) * fCharWidth;
    fBookmarkOpt.XOffset := Value - 18;
    if HandleAllocated then begin
      {$IFDEF SYN_LAZARUS}
      fCharsInWindow := Max(1,(ClientWidth - fGutterWidth - 2 - ScrollBarWidth)
                              div fCharWidth);
      //debugln('TCustomSynEdit.SetGutterWidth A ClientWidth=',dbgs(ClientWidth),' fGutterWidth=',dbgs(fGutterWidth),' ScrollBarWidth=',dbgs(ScrollBarWidth),' fCharWidth=',dbgs(fCharWidth));
      {$ELSE}
      fCharsInWindow := Max(1,Max(0,(ClientWidth - fGutterWidth - 2
                                     - ScrollBarWidth) div Max(1,fCharWidth)));
      {$ENDIF}
      UpdateScrollBars;
      Invalidate;
    end;
  end;
end;

procedure TCustomSynEdit.SetLeftChar(Value: Integer);
{begin}                                                                         //mh 2000-10-19
var
  MaxVal: integer;
begin
  if eoScrollPastEol in Options then
    MaxVal := fMaxLeftChar
  else
    MaxVal := TSynEditStringList(Lines).LengthOfLongestLine;
  Value := Min(Value, MaxVal - fCharsInWindow + 1);
{end}                                                                           //mh 2000-10-19
  Value := Max(Value, 1);
  if Value <> fLeftChar then begin
    fLeftChar := Value;
    fTextOffset := fGutterWidth + 2 - (LeftChar - 1) * fCharWidth;
    UpdateScrollBars;
    InvalidateLines(-1, -1);
    StatusChanged([scLeftChar]);
  end;
end;

procedure TCustomSynEdit.SetLines(Value: TStrings);
begin
  if HandleAllocated then
    Lines.Assign(Value);
end;

procedure TCustomSynEdit.SetLineText(Value: string);
begin
  if (CaretY >= 1) and (CaretY <= Max(1, Lines.Count)) then
    Lines[CaretY - 1] := Value;
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

procedure TCustomSynEdit.SetScrollBars(const Value: TScrollStyle);
begin
  if (FScrollBars <> Value) then begin
    FScrollBars := Value;
    // TODO: MWE: check if there is a better solution in lazarus
    // RecreateWnd is depriciated in Lazarus
    RecreateWnd{$IFDEF SYN_LAZARUS}(Self){$ENDIF};

    UpdateScrollBars;
    Invalidate;
  end;
end;

procedure TCustomSynEdit.SetSelText(const Value: string);
begin
  SetSelTextPrimitive(smNormal, PChar(Value), nil);
end;

// This is really a last minute change and I hope I did it right.
// Reason for this modification: next two lines will loose the CaretX position
// if eoScrollPastEol is not set in Options. That is not really a good idea
// as we would typically want the cursor to stay where it is.
// To fix this (in the absence of a better idea), I changed the code in
// DeleteSelection not to trim the string if eoScrollPastEol is not set.
procedure TCustomSynEdit.SetSelTextPrimitive(PasteMode: TSynSelectionMode;
  Value: PChar; ATag: PInteger);
var
  BB, BE: TPoint;
  TempString: string;

  procedure DeleteSelection;
  var
    x, MarkOffset: Integer;
    UpdateMarks: boolean;
    {$IFDEF SYN_LAZARUS}
    NewCaretXY: TPoint;
    {$ENDIF}
{$IFDEF SYN_MBCSSUPPORT}
    l, r: Integer;
{$ENDIF}
  begin
    UpdateMarks := FALSE;
    MarkOffset := 0;
    case SelectionMode of
      smNormal:
        begin
          {$IFDEF SYN_LAZARUS}
          NewCaretXY:=LogicalToPhysicalPos(BB);
          {$ENDIF}
          if Lines.Count > 0 then begin
              // Create a string that contains everything on the first line up
              // to the selection mark, and everything on the last line after
              // the selection mark.
            TempString := Copy(Lines[BB.Y - 1], 1, BB.X - 1) +
              Copy(Lines[BE.Y - 1], BE.X, MaxInt);
            // Delete all lines in the selection range.
{begin}                                                                         // djlp 2000-09-13
            TSynEditStringList(Lines).DeleteLines(BB.Y-1, BE.Y - BB.Y);
//            for x := BE.Y - 1 downto BB.Y do
//              Lines.Delete(x);
{end}                                                                           // djlp 2000-09-13
              // Put the stuff that was outside of selection back in.
//            if eoScrollPastEol in Options then                                //JGF 2000-09-23
            if Options * [eoScrollPastEol, eoTrimTrailingSpaces]
              = [eoScrollPastEol, eoTrimTrailingSpaces]
            then
              TempString := TrimRight(TempString);
            Lines[BB.Y - 1] := TempString;
          end;
          UpdateMarks := TRUE;
          CaretXY := {$IFDEF SYN_LAZARUS}NewCaretXY{$ELSE}BB{$ENDIF};
        end;
      smColumn:
        begin
            // swap X if needed
          if BB.X > BE.X then
{$IFDEF SYN_COMPILER_3_UP}
            SwapInt(BB.X, BE.X);
{$ELSE}
          begin
            x := BB.X;
            BB.X := BE.X;
            BE.X := x;
          end;
{$ENDIF}
          {$IFDEF SYN_LAZARUS}
          NewCaretXY:=LogicalToPhysicalPos(Point(BB.X,fBlockEnd.Y));
          {$ENDIF}
          for x := BB.Y - 1 to BE.Y - 1 do begin
            TempString := Lines[x];
{$IFNDEF SYN_MBCSSUPPORT}
            Delete(TempString, BB.X, BE.X - BB.X);
{$ELSE}
            l := BB.X;
            r := BE.X;
            MBCSGetSelRangeInLineWhenColumnSelectionMode(TempString, l, r);
{$IFDEF USE_UTF8BIDI_LCL}
            VDelete(TempString, l, r - 1);
{$ELSE USE_UTF8BIDI_LCL}
            Delete(TempString, l, r - l);
{$ENDIF USE_UTF8BIDI_LCL}
{$ENDIF}
            TrimmedSetLine(x, TempString);
          end;
            // Lines never get deleted completely, so keep caret at end.
          CaretXY := {$IFDEF SYN_LAZARUS}
                     NewCaretXY
                     {$ELSE}
                     Point(BB.X, fBlockEnd.Y)
                     {$ENDIF};
            // Column deletion never removes a line entirely, so no mark
            // updating is needed here.
        end;
      smLine:
        begin
          if BE.Y = Lines.Count then begin
            Lines[BE.Y - 1] := '';
            for x := BE.Y - 2 downto BB.Y - 1 do
              Lines.Delete(x);
          end else
            for x := BE.Y - 1 downto BB.Y - 1 do
              Lines.Delete(x);
            // smLine deletion always resets to first column.
          CaretXY := Point(1, BB.Y);
          UpdateMarks := TRUE;
          MarkOffset := 1;
        end;
    end;
    // Update marks
    if UpdateMarks then
      DoLinesDeleted(BB.Y, BE.Y - BB.Y + MarkOffset);
  end;

  procedure InsertText;

{begin}                                                                         // djlp 2000-09-07
    function CountLines(p: PChar): integer;
    begin
      Result := 0;
      while p^ <> #0 do begin
        if p^ = #13 then
          Inc(p);
        if p^ = #10 then
          Inc(p);
        Inc(Result);
        p := GetEOL(p);
      end;
    end;
{end}

    function InsertNormal: Integer;
    var
      sLeftSide: string;
      sRightSide: string;
      Str: string;
      Start: PChar;
      P: PChar;
      {$IFDEF SYN_LAZARUS}
      LogCaretXY: TPoint;
      PhysicalLineEndPos: LongInt;
      {$ENDIF}
    begin
      Result := 0;
      {$IFDEF SYN_LAZARUS}
      LogCaretXY:=PhysicalToLogicalPos(CaretXY);
      sLeftSide := Copy(LineText, 1, LogCaretXY.X - 1);
      if LogCaretXY.X - 1 > Length(sLeftSide) then begin
        PhysicalLineEndPos:=
                    LogicalToPhysicalPos(Point(Length(sLeftSide)+1,CaretY)).X-1;
        sLeftSide := sLeftSide
                     + CreateTabsAndSpaces(CaretX,
                                           CaretX-1-PhysicalLineEndPos,TabWidth,
                                           eoSpacesToTabs in Options);
      end;
      sRightSide := Copy(LineText, LogCaretXY.X,
                         Length(LineText) - (LogCaretXY.X - 1));
      {$ELSE}
      sLeftSide := Copy(LineText, 1, CaretX - 1);
      if CaretX - 1 > Length(sLeftSide) then begin
        sLeftSide := sLeftSide + StringOfChar(' ', CaretX-1-Length(sLeftSide));
      end;
      sRightSide := Copy(LineText, CaretX, Length(LineText) - (CaretX - 1));
      {$ENDIF}
      if eoTrimTrailingSpaces in Options then
        sRightSide := TrimRight(sRightSide);
      // step1: insert the first line of Value into current line
      Start := PChar(Value);
      P := GetEOL(Start);
      if P^ <> #0 then begin
        SetString(Str, Value, P - Start);
        TrimmedSetLine(CaretY - 1, sLeftSide + Str);
        TSynEditStringList(Lines).InsertLines(CaretY, CountLines(P));           // djlp 2000-09-07
      end else
        TrimmedSetLine(CaretY - 1, sLeftSide + Value + sRightSide);
      // step2: insert left lines of Value
      while P^ <> #0 do begin
        if P^ = #13 then
          Inc(P);
        if P^ = #10 then
          Inc(P);
        Inc(fCaretY);
        Start := P;
        P := GetEOL(Start);
        if P = Start then begin
          if p^ <> #0 then
//            Lines.Insert(CaretY - 1, '')
            Lines[CaretY - 1] := ''                                             // djlp 2000-09-07
          else
//            Lines.Insert(CaretY - 1, sRightSide);
            Lines[CaretY - 1] := sRightSide;                                    // djlp 2000-09-07
        end else begin
//          SetLength(Str, P - Start);
//          Move(Start^, Str[1], P - Start);
          SetString(Str, Start, P - Start);                                     //mh 2000-11-08
          if p^ <> #0 then
//            Lines.Insert(CaretY - 1, Str)
            Lines[CaretY - 1] := Str                                            // djlp 2000-09-07
          else
//            Lines.Insert(CaretY - 1, Str + sRightSide);
            Lines[CaretY - 1] := Str + sRightSide                               // djlp 2000-09-07
        end;
        if eoTrimTrailingSpaces in Options then                                 //JGF 2000-09-23
          Lines[CaretY - 1] := TrimRight(Lines[CaretY - 1]);
        Inc(Result);
      end;
      {$IFDEF SYN_LAZARUS}
      fCaretX := LogicalToPhysicalPos(
                   Point(1 + Length(Lines[CaretY - 1]) - Length(sRightSide),
                         CaretY)).X;
      {$ELSE}
      fCaretX := 1 + Length(Lines[CaretY - 1]) - Length(sRightSide);
      {$ENDIF}
      StatusChanged([scCaretX]);
    end;

    function InsertColumn: Integer;
    var
      Str: string;
      Start: PChar;
      P: PChar;
      Len: Integer;
      InsertPos: Integer;
      {$IFDEF SYN_LAZARUS}
      LogicalInsertPos: Integer;
      {$ENDIF}
    begin
      // Insert string at current position
      InsertPos := CaretX;
      Start := PChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], P - Start);
          if CaretY > Lines.Count then
            Lines.Add(StringOfChar(' ', InsertPos - 1) + Str)
          else begin
            TempString := Lines[CaretY - 1];
            Len := Length(TempString);
            {$IFDEF SYN_LAZARUS}
            LogicalInsertPos:=PhysicalToLogicalCol(TempString,InsertPos);
            {$ENDIF}
            if Len<{$IFDEF SYN_LAZARUS}LogicalInsertPos{$ELSE}InsertPos{$ENDIF}
            then begin
              TempString :=
                TempString + StringOfChar(' ',
                    {$IFDEF SYN_LAZARUS}LogicalInsertPos{$ELSE}InsertPos{$ENDIF}
                    - Len - 1)
                    + Str
            end else begin
              {$IFDEF SYN_MBCSSUPPORT}
              if mbTrailByte = ByteType(TempString, InsertPos) then
                Insert(Str, TempString, InsertPos + 1)
              else
              {$ENDIF}
                System.Insert(Str, TempString,
                {$IFDEF SYN_LAZARUS}LogicalInsertPos{$ELSE}InsertPos{$ENDIF});
            end;
            TrimmedSetLine(CaretY - 1, TempString);                             //JGF 2000-09-23
          end;
        end;
        if ATag <> nil then
          ATag^ := P - Start;
        {$IFDEF SYN_LAZARUS}
        if p^ in [#10,#13] then begin
          if (p[1] in [#10,#13]) and (p[1]<>p^) then
            inc(p,2)
          else
            Inc(P);
          Inc(fCaretY);
        end;
        {$ELSE}
        if P^ = #13 then begin
          Inc(P);
          if P^ = #10 then
            Inc(P);
        end;
        {$ENDIF}
        Start := P;
      until P^ = #0;
      Inc(fCaretX, Length(Str));
      Result := 0;
    end;

    function InsertLine: Integer;
    var
      Start: PChar;
      P: PChar;
      Str: string;
      n: Integer;
    begin
      Result := 0;
      fCaretX := 1;
      // Insert string before current line
      Start := PChar(Value);
      repeat
        P := GetEOL(Start);
        if P <> Start then begin
          SetLength(Str, P - Start);
          Move(Start^, Str[1], P - Start);
        end else
          Str := '';
        if (P^ = #0) then begin
          n := Lines.Count;
          if (n >= CaretY) then
            Lines[CaretY - 1] := Str + Lines[CaretY - 1]
          else
            Lines.Add(Str);
          if eoTrimTrailingSpaces in Options then
            Lines[CaretY - 1] := TrimRight(Lines[CaretY - 1]);
          fCaretX := 1 + Length(Str);
        end else begin
          TrimmedSetLine(CaretY - 1, Str);
          Inc(fCaretY);
          Inc(Result);
          if P^ = #13 then
            Inc(P);
          if P^ = #10 then
            Inc(P);
          Start := P;
        end;
      until P^ = #0;
      StatusChanged([scCaretX]);
    end;

  var
    StartLine: Integer;
    InsertedLines: Integer;
  begin
    if Value = '' then
      Exit;

    // Using a TStringList to do this would be easier, but if we're dealing
    // with a large block of text, it would be very inefficient.  Consider:
    // Assign Value parameter to TStringList.Text: that parses through it and
    // creates a copy of the string for each line it finds.  That copy is passed
    // to the Add method, which in turn creates a copy.  Then, when you actually
    // use an item in the list, that creates a copy to return to you.  That's
    // 3 copies of every string vs. our one copy below.  I'd prefer no copies,
    // but we aren't set up to work with PChars that well.

    StartLine := CaretY;
    case PasteMode of
      smNormal:
        InsertedLines := InsertNormal;
      smColumn:
        InsertedLines := InsertColumn;
      smLine:
        InsertedLines := InsertLine;
    else
      InsertedLines := 0;
    end;
    // We delete selected based on the current selection mode, but paste
    // what's on the clipboard according to what it was when copied.
    // Update marks
    if InsertedLines > 0 then
      DoLinesInserted(StartLine, InsertedLines);
    // Force caret reset
    CaretXY := CaretXY;
  end;

var
  {$IFDEF SYN_LAZARUS}
  BH: TPoint;
  {$ENDIF}
begin
  IncPaintLock;
  Lines.BeginUpdate;
  try
    BB := BlockBegin;
    BE := BlockEnd;
    {$IFDEF SYN_LAZARUS}
    // make sure, BB is lower than BE
    if CompareCarets(BB,BE)<0 then begin
      BH:=BB;
      BB:=BE;
      BE:=BH;
    end;
    {$ENDIF}
    if SelAvail then
      DeleteSelection;
    if (Value <> nil) and (Value[0] <> #0) then
      InsertText;
    fLastCaretX := fCaretX;                                                     //mh 2000-10-19
    if CaretY < 1 then
      CaretY := 1;
  finally
    Lines.EndUpdate;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.SynSetText(const Value: string);
begin
  Lines.Text := Value;
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.RealSetText(const Value: TCaption);
begin
  Lines.Text := Value;
end;
{$ENDIF}

procedure TCustomSynEdit.SetTopLine(Value: Integer);
var
  Delta: Integer;
begin
  // don't use MinMax here, it will fail in design mode (Lines.Count is zero,
  // but the painting code relies on TopLine >= 1)
  if (eoScrollPastEof in Options) then
    Value := Min(Value, Lines.Count)
  else
    Value := Min(Value, Lines.Count + 1 - fLinesInWindow);
  Value := Max(Value, 1);
  if Value <> TopLine then begin
    Delta := TopLine - Value;
    fTopLine := Value;
    UpdateScrollBars;
    if Abs(Delta) < fLinesInWindow then
{$ifndef SYN_LAZARUS}
      ScrollWindow(Handle, 0, fTextHeight * Delta, nil, nil);
{$else}
    begin
      // TODO: SW_SMOOTHSCROLL --> can't get it work
      if not ScrollWindowEx(Handle, 0, fTextHeight * Delta, nil, nil, 0, nil,
        SW_INVALIDATE) then
      begin
        // scrollwindow failed, invalidate all
        Invalidate;
      end;
    end
{$endif}
    else
      Invalidate;
    StatusChanged([scTopLine]);
  end;
end;

procedure TCustomSynEdit.ShowCaret;
begin
  //DebugLn(' [TCustomSynEdit.ShowCaret] ShowCaret ',Name,' ',sfCaretVisible in fStateFlags,' ',eoPersistentCaret in fOptions);
  if not (eoNoCaret in Options) and not (sfCaretVisible in fStateFlags) then
  begin
    {$IFDEF SYN_LAZARUS}
    SetCaretRespondToFocus(Handle,not (eoPersistentCaret in fOptions));
    {$ENDIF}
    if {$IFDEF SYN_LAZARUS}LCLIntf{$ELSE}Windows{$ENDIF}.ShowCaret(Handle) then
    begin
      //DebugLn('[TCustomSynEdit.ShowCaret] A ',Name);
      Include(fStateFlags, sfCaretVisible);
    end;
  end;
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.MoveCaretToVisibleArea;
begin
  if caretX < fLeftChar then caretX := fLeftChar
  else if caretX >= fLeftChar + fCharsInWindow then
    caretX := fLeftChar + fCharsInWindow - 1;
  if caretY < fTopLine then caretY := fTopLine
  else if caretY >= fTopLine + fLinesInWindow then
    caretY := fTopLine + fLinesInWindow - 1;
end;

procedure TCustomSynEdit.MoveCaretIgnoreEOL(const NewCaret: TPoint);
var
  NewX: LongInt;
begin
  CaretXY:=NewCaret;
  NewX:=Max(1,Min(fMaxLeftChar,NewCaret.X));
  if CaretX<>NewX then begin
    IncPaintLock;
    fCaretX:=NewX;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.MoveLogicalCaretIgnoreEOL(const NewLogCaret: TPoint);
begin
  MoveCaretIgnoreEOL(LogicalToPhysicalPos(NewLogCaret));
end;
{$ENDIF}

procedure TCustomSynEdit.UpdateCaret;
var
  CX, CY: Integer;
{$IFDEF SYN_MBCSSUPPORT}
  cf: TCompositionForm;
{$ENDIF}
begin
  if (PaintLock <> 0)
  {$IFDEF SYN_LAZARUS}
  or ((not Focused) and (not (eoPersistentCaret in fOptions)))
  {$ELSE}
  or not Focused
  {$ENDIF}
  then
    Include(fStateFlags, sfCaretChanged)
  else begin
    Exclude(fStateFlags, sfCaretChanged);
    {$IFDEF SYN_LAZARUS}
    if eoAlwaysVisibleCaret in fOptions2 then
      MoveCaretToVisibleArea;
    {$ENDIF}
    CX := CaretXPix + FCaretOffset.X;
    CY := CaretYPix + FCaretOffset.Y;
    if (CX >= fGutterWidth)
      and (CX < ClientWidth{$IFDEF SYN_LAZARUS}-ScrollBarWidth{$ENDIF})
      and (CY >= 0)
      and (CY < ClientHeight{$IFDEF SYN_LAZARUS}-ScrollBarWidth{$ENDIF})
    then begin
      {$IFDEF SYN_LAZARUS}
      SetCaretPosEx(Handle,CX,CY);
      {$ELSE}
      SetCaretPos(CX, CY);
      {$ENDIF}
      //DebugLn(' [TCustomSynEdit.UpdateCaret] ShowCaret ',Name);
      ShowCaret;
    end else begin
      //DebugLn(' [TCustomSynEdit.UpdateCaret] HideCaret ',Name);
      HideCaret;
      {$IFDEF SYN_LAZARUS}
      SetCaretPosEx(Handle,CX, CY);
      {$ELSE}
      SetCaretPos(CX, CY);
      {$ENDIF}
    end;
    {$IFDEF SYN_LAZARUS}
    InvalidateBracketHighlight(true);
    {$ENDIF}
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
  nMaxScroll: integer;
begin
  if not HandleAllocated or (PaintLock <> 0) then
    Include(fStateFlags, sfScrollbarChanged)
  else begin
    Exclude(fStateFlags, sfScrollbarChanged);
    if fScrollBars <> ssNone then begin
      ScrollInfo.cbSize := SizeOf(ScrollInfo);
      ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL and not SIF_TRACKPOS;
      ScrollInfo.nMin := 1;
      ScrollInfo.nTrackPos := 0;
      if fScrollBars in [ssBoth, ssHorizontal] then begin
{begin}                                                                         //mh 2000-10-19
//        ScrollInfo.nMax := fMaxLeftChar;
        if eoScrollPastEol in Options then
          ScrollInfo.nMax := fMaxLeftChar
        else
          ScrollInfo.nMax := TSynEditStringList(Lines).LengthOfLongestLine;
{end}                                                                           //mh 2000-10-19
        ScrollInfo.nPage := CharsInWindow;
        ScrollInfo.nPos := LeftChar;
        {$IFDEF SYN_LAZARUS}
        { for win32 target, need to call showscrollbar before setscrollinfo }
        ShowScrollBar(Handle, SB_HORZ, True);
        {$ENDIF}
        SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
        //DebugLn('>>>>>>>>>> [TCustomSynEdit.UpdateScrollbars] nMin=',ScrollInfo.nMin,
        //' nMax=',ScrollInfo.nMax,' nPage=',ScrollInfo.nPage,
        //' nPos=',ScrollInfo.nPos,
        //' ClientW=',ClientWidth
        //);
      end else begin

        // ToDo: tell interface to remove horizontal scrollbar

      end;
      if fScrollBars in [ssBoth, ssVertical] then begin
        nMaxScroll := Lines.Count{$IFDEF SYN_LAZARUS}+1{$ENDIF};
        if (eoScrollPastEof in Options) then
          Inc(nMaxScroll, LinesInWindow - 1);
{$IFNDEF SYN_LAZARUS}
        if nMaxScroll <= MAX_SCROLL then begin
{$ENDIF}
          ScrollInfo.nMax := Max(1, nMaxScroll);
          ScrollInfo.nPage := LinesInWindow;
          ScrollInfo.nPos := TopLine;
{$IFNDEF SYN_LAZARUS}
        end else begin
          ScrollInfo.nMin := 0;
          ScrollInfo.nMax := MAX_SCROLL;
          ScrollInfo.nPage := MulDiv(MAX_SCROLL, LinesInWindow, nMaxScroll);
          ScrollInfo.nPos := MulDiv(MAX_SCROLL, TopLine, nMaxScroll);
        end;
{$ENDIF}
        {$IFDEF SYN_LAZARUS}
        { for win32 target, need to call showscrollbar before setscrollinfo }
        ShowScrollBar(Handle, SB_VERT, True);
        {$ENDIF}
        SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
      end;
    end;
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

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.WMExit(var Message: TLMExit);
begin
  LastMouseCaret:=Point(-1,-1);
end;
{$ENDIF}

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
    SB_BOTTOM: LeftChar := MaxLeftChar;
      // Scrolls one char left / right
    SB_LINEDOWN: LeftChar := LeftChar + 1;
    SB_LINEUP: LeftChar := LeftChar - 1;
      // Scrolls one page of chars left / right
    SB_PAGEDOWN: LeftChar := LeftChar
      + (fCharsInWindow - Ord(eoScrollByOneLess in fOptions));
    SB_PAGEUP: LeftChar := LeftChar
      - (fCharsInWindow - Ord(eoScrollByOneLess in fOptions));
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: LeftChar := Msg.Pos;
  end;
end;

procedure TCustomSynEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  {$IFDEF VerboseFocus}
  DebugLn('[TCustomSynEdit.WMKillFocus] A ',Name);
  {$ENDIF}
  {$IFDEF SYN_LAZARUS}
  LastMouseCaret:=Point(-1,-1);
  if not (eoPersistentCaret in fOptions) then begin
    HideCaret;
    LCLIntf.DestroyCaret(Handle);
  end;
  {$ELSE}
  HideCaret;
  Windows.DestroyCaret;
  {$ENDIF}
  if FHideSelection and SelAvail then
    Invalidate;
end;

procedure TCustomSynEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  LastMouseCaret:=Point(-1,-1);
  {$IFDEF VerboseFocus}
  DebugLn('[TCustomSynEdit.WMSetFocus] A ',Name,':',ClassName);
  {$ENDIF}
  InitializeCaret;
  //if FHideSelection and SelAvail then
  //  Invalidate;
  //DebugLn('[TCustomSynEdit.WMSetFocus] END');
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.Resize;
{$ELSE}
procedure TCustomSynEdit.WMSize(var Msg: TWMSize);
{$ENDIF}
begin
  inherited;
  SizeOrFontChanged(FALSE);
  //debugln('TCustomSynEdit.Resize ',dbgs(Width),',',dbgs(Height),',',dbgs(ClientWidth),',',dbgs(ClientHeight));
  // SetLeftChar(LeftChar);                                                     //mh 2000-10-19
end;

{$IFNDEF SYN_LAZARUS}
// ToDo THintWindow
var
  ScrollHintWnd: THintWindow;

function GetScrollHint: THintWindow;
begin
  if ScrollHintWnd = nil then begin
    ScrollHintWnd := HintWindowClass.Create(Application);
    ScrollHintWnd.Visible := FALSE;
  end;
  Result := ScrollHintWnd;
end;
{$ENDIF}

procedure TCustomSynEdit.WMVScroll(var Msg: {$IFDEF SYN_LAZARUS}TLMScroll{$ELSE}TWMScroll{$ENDIF});
{$IFNDEF SYN_LAZARUS}
// ToDo HintWindow
var
  s: ShortString;
  rc: TRect;
  pt: TPoint;
  ScrollHint: THintWindow;
{$ENDIF}
begin
  //debugln('TCustomSynEdit.WMVScroll A ',DbgSName(Self),' Msg.ScrollCode=',dbgs(Msg.ScrollCode),' SB_PAGEDOWN=',dbgs(SB_PAGEDOWN),' SB_PAGEUP=',dbgs(SB_PAGEUP));
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP: TopLine := 1;
    SB_BOTTOM: TopLine := Lines.Count;
      // Scrolls one line up / down
    SB_LINEDOWN: TopLine := TopLine + 1;
    SB_LINEUP: TopLine := TopLine - 1;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN: TopLine := TopLine
      + (fLinesInWindow - Ord(eoScrollByOneLess in fOptions));
    SB_PAGEUP: TopLine := TopLine
      - (fLinesInWindow - Ord(eoScrollByOneLess in fOptions));
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK:
      begin
{$IFNDEF SYN_LAZARUS}
        if Lines.Count > MAX_SCROLL then
          TopLine := MulDiv(LinesInWindow + Lines.Count - 1, Msg.Pos,
            MAX_SCROLL)
        else
{$ENDIF}
          TopLine := Msg.Pos;

        if eoShowScrollHint in fOptions then begin
          {$IFNDEF SYN_LAZARUS}
          // ToDo HintWindow
          ScrollHint := GetScrollHint;
          if not ScrollHint.Visible then begin
            ScrollHint.Color := Application.HintColor;
            ScrollHint.Visible := TRUE;
          end;
          s := Format(SYNS_ScrollInfoFmt, [TopLine]);
{$IFDEF SYN_COMPILER_3_UP}
          rc := ScrollHint.CalcHintRect(200, s, nil);
{$ELSE}
          rc := Rect(0, 0, ScrollHint.Canvas.TextWidth(s) + 6,
            ScrollHint.Canvas.TextHeight(s) + 4);
{$ENDIF}
          pt := ClientToScreen(Point(
                ClientWidth{$IFDEF SYN_LAZARUS}-ScrollBarWidth{$ENDIF}
                        - rc.Right - 4, 10));
          OffsetRect(rc, pt.x, pt.y);
          ScrollHint.ActivateHint(rc, s);
{$IFNDEF SYN_COMPILER_3_UP}
          ScrollHint.Invalidate;
{$ENDIF}
          ScrollHint.Update;
          {$ENDIF}
        end;
      end;
      // Ends scrolling
    SB_ENDSCROLL:
      {$IFNDEF SYN_LAZARUS}
      ifSYN_LAZARUS eoShowScrollHint in fOptions then
        with GetScrollHint do begin
          Visible := FALSE;
          ActivateHint(Rect(0, 0, 0, 0), '');
        end;
      {$ENDIF}
  end;
  Update;
end;

function TCustomSynEdit.ScanFrom(Index: integer
  {$IFDEF SYN_LAZARUS}; AtLeastTilIndex: integer{$ENDIF}): integer;
{$IFDEF SYN_LAZARUS}
// Index and AtLeastTilIndex are 0 based

  procedure SetCodeFoldAttributes;
  var
    CodeFoldMinLevel: LongInt;
    CodeFoldEndLevel: LongInt;
    CodeFoldType: TSynEditCodeFoldType;
    LastCodeFoldEndLevel: LongInt;
  begin
    CodeFoldMinLevel:=fHighlighter.MinimumCodeFoldBlockLevel;
    CodeFoldEndLevel:=fHighlighter.CurrentCodeFoldBlockLevel;
    CodeFoldType:=cfNone;
    if CodeFoldEndLevel>CodeFoldMinLevel then begin
      // block started (and not closed in the same line)
      CodeFoldType:=cfExpanded;
      //debugln(['TCustomSynEdit.ScanFrom Block started Y=',Result,' MinLevel=',CodeFoldMinLevel,' EndLevel=',CodeFoldEndLevel,' CodeFoldType=',ord(CodeFoldType),' Line="',Lines[Result-1],'"']);
    end else if (Result>1) then begin
      LastCodeFoldEndLevel:=TSynEditStringList(Lines).FoldEndLevel[Result-2];
      if LastCodeFoldEndLevel>CodeFoldMinLevel then begin
        // block closed
        CodeFoldType:=cfEnd;
      end else if CodeFoldEndLevel>0 then begin
        // block continuing
        CodeFoldType:=cfContinue;
      end;
    end;
    //DebugLn(['TCustomSynEdit.ScanFrom CodeFoldType=',SynEditCodeFoldTypeNames[CodeFoldType],' FoldMinLevel=',CodeFoldMinLevel,' FoldEndLevel=',CodeFoldEndLevel,' Folded=',false]);
    TSynEditStringList(Lines).FoldMinLevel[Result-1] := CodeFoldMinLevel;
    TSynEditStringList(Lines).FoldEndLevel[Result-1] := CodeFoldEndLevel;
    TSynEditStringList(Lines).FoldType[Result-1] := CodeFoldType;
  end;
  
  procedure CheckFolded(FromIndex, ToIndex: integer);
  { Checks/Updates the Folded attributes of every scanned line
  
  }
  var
    i: LongInt;
    FoldStart: LongInt;
    FoldLevel: LongInt;
    SLines: TSynEditStringList;
  begin
    i:=FromIndex;
    if i<0 then i:=0;
    if i>ToIndex then exit;
    SLines:=TSynEditStringList(Lines);
    // find start and level of folded block at start of scan range
    if SLines.Folded[i] then begin
      FoldStart:=i;
      while (FoldStart>0) and SLines.Folded[FoldStart] do
        dec(FoldStart);
      FoldLevel:=SLines.FoldEndLevel[FoldStart];
      //DebugLn(['CheckFolded First FoldStart=',FoldStart,' FoldLevel=',FoldLevel,' Line="',Lines[FoldStart],'"']);
    end else begin
      FoldStart:=-1;
      FoldLevel:=0;
    end;
    // check and fix 'folded' attributes of scanned range
    while i<=ToIndex do begin
      if FoldLevel<=0 then begin
        // last line is not folded
        if not SLines.Folded[i] then begin
          // this line was not folded
          // no change needed
        end else begin
          // this line was folded
          if (i>0) and (SLines.FoldMinLevel[i-1]>=SLines.FoldEndLevel[i-1]) then
          begin
            // last line did not contain a block start
            // => unfolded block must continue
            SLines.Folded[i]:=false;
            FoldLevel:=0;
            //DebugLn(['CheckFolded Change A Folded of line ',i,' to FoldStart=',FoldStart,' FoldLevel=',FoldLevel,' Line="',Lines[i],'" SLines.Folded[i]=',SLines.Folded[i]]);
          end;
        end;
      end else begin
        // last line is folded
        if SLines.Folded[i] then begin
          // this line was folded
          if (i>0) and (SLines.FoldEndLevel[i-1]<FoldLevel) then begin
            // last fold block ended with last line
            FoldStart:=i;
            FoldLevel:=SLines.FoldMinLevel[FoldStart];
            SLines.Folded[i]:=false;
            //DebugLn(['CheckFolded Change B Folded of line ',i,' to FoldStart=',FoldStart,' FoldLevel=',FoldLevel,' Line="',Lines[i],'" SLines.Folded[i]=',SLines.Folded[i]]);
          end;
        end else begin
          // this line was not folded
          if (i>0) and (SLines.FoldEndLevel[i-1]>=FoldLevel) then begin
            // current folded block must be continued
            SLines.Folded[i]:=true;
            //DebugLn(['CheckFolded Change C Folded of line ',i,' to FoldStart=',FoldStart,' FoldLevel=',FoldLevel,' Line="',Lines[i],'" SLines.Folded[i]=',SLines.Folded[i]]);
          end;
        end;
      end;
      inc(i);
    end;
  end;
{$ENDIF}

begin
  Result := Index;
  if Index >= Lines.Count - 1 then Exit;
  //debugln('TCustomSynEdit.ScanFrom A Index=',dbgs(Index),' Line="',Lines[Index],'"');
  fHighlighter.SetLine(Lines[Result], Result);
  inc(Result);
  fHighlighter.NextToEol;
  while (fHighlighter.GetRange <> TSynEditStringList(Lines).Ranges[Result])
  {$IFDEF SYN_LAZARUS}
  or (Result<=AtLeastTilIndex)
  {$ENDIF}
  do begin
    //debugln(['TSynCustomHighlighter.ScanFrom WHILE Y=',Result,' Level=',fHighlighter.CurrentCodeFoldBlockLevel,' ScannedLine="',Lines[Result-1],'"']);
    TSynEditStringList(Lines).Ranges[Result{$IFNDEF SYN_LAZARUS}-1{$ENDIF}] :=
                                                          fHighlighter.GetRange;
    {$IFDEF SYN_LAZARUS}
    SetCodeFoldAttributes;
    //if (Result and $fff)=0 then
    //  debugln('TCustomSynEdit.ScanFrom A Line=', dbgs(Result),' Index=',dbgs(Index),' MinLevel=',dbgs(CodeFoldMinLevel),' EndLevel=',dbgs(CodeFoldEndLevel),' CodeFoldType=',dbgs(ord(CodeFoldType)),' ',dbgs(length(Lines[Result-1])));
    {$ENDIF}
    fHighlighter.SetLine(Lines[Result], Result);
    //debugln(['TSynCustomHighlighter.ScanFrom SetLine Y=',Result,' Level=',fHighlighter.CurrentCodeFoldBlockLevel,' Line="',Lines[Result],'"']);
    fHighlighter.NextToEol;
    //debugln(['TSynCustomHighlighter.ScanFrom NextEOL Y=',Result,' Level=',fHighlighter.CurrentCodeFoldBlockLevel]);
    inc(Result);
    if Result = Lines.Count then
      break;
  end;
  {$IFDEF SYN_LAZARUS}
  if (Result>Index+1) and (Result<=Lines.Count) then begin
    // at least one line changed
    //  => update code fold attributes of last scanned line
    SetCodeFoldAttributes;
  end;
  CheckFolded(Index,Result);
  {$ENDIF}
  Dec(Result);
end;

{begin}                                                                         //mh 2000-10-10
(*
procedure TCustomSynEdit.ListAdded(Sender: TObject);
var
  LastIndex: Integer;
begin
  if Assigned(fHighlighter) then begin
    if Lines.Count > 1 then begin
      LastIndex := Lines.Count - 1;
      fHighlighter.SetRange(Lines.Objects[LastIndex - 1]);
      fHighlighter.SetLine(Lines[LastIndex - 1], LastIndex - 1);
      fHighlighter.NextToEol;
      Lines.Objects[LastIndex] := fHighlighter.GetRange;
    end else begin
      fHighlighter.ReSetRange;
      Lines.Objects[0] := fHighlighter.GetRange;
    end;
  end;
  LastIndex := Lines.Count;
  InvalidateLine(LastIndex);
*)
procedure TCustomSynEdit.ListAdded(Index: integer);
begin
  //debugln('TCustomSynEdit.ListAdded ',dbgs(Index),' ',dbgs(Assigned(fHighlighter)));
  if Assigned(fHighlighter) then begin
    if (Index > 0) then begin
      fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Index - 1]);
      ScanFrom(Index - 1);
    end else begin
      fHighlighter.ResetRange;
      TSynEditStringList(Lines).Ranges[0] := fHighlighter.GetRange;
      if (Lines.Count > 1) then
        ScanFrom(0);
    end;
  end;
  InvalidateLines(Index + 1, MaxInt);
  InvalidateGutterLines(Index + 1, MaxInt);
end;
{end}                                                                           //mh 2000-10-10

procedure TCustomSynEdit.ListCleared(Sender: TObject);
begin
  ClearUndo;
  // invalidate the *whole* client area
  FillChar(fInvalidateRect, SizeOf(TRect), 0);
  Invalidate;
  // set caret and selected block to start of text
  SetBlockBegin(Point(1, 1));
  SetCaretXY(Point(1, 1));
  // scroll to start of text
  TopLine := 1;
  LeftChar := 1;
  Include(fStatusChanges, scAll);
end;

procedure TCustomSynEdit.ListDeleted(Index: Integer);
begin
  if Assigned(fHighlighter) and (Lines.Count >= 1) then
    if (Index > 0) then begin
{begin}                                                                         //mh 2000-10-10
//      fHighlighter.SetRange(Lines.Objects[Index - 1]);
      //DebugLn(['TCustomSynEdit.ListDeleted A Index=',Index]);
      fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Index - 1]);
      ScanFrom(Index - 1);
    end else begin
      //DebugLn(['TCustomSynEdit.ListDeleted B Index=',Index]);
      fHighlighter.ResetRange;
//      Lines.Objects[0] := fHighlighter.GetRange;
      TSynEditStringList(Lines).Ranges[0] := fHighlighter.GetRange;
{end}                                                                           //mh 2000-10-10
      if (Lines.Count > 1) then
        ScanFrom(0);
    end;
  InvalidateLines(Index + 1, MaxInt);
  InvalidateGutterLines(Index + 1, MaxInt);
end;

procedure TCustomSynEdit.ListInserted(Index: Integer);
begin
  if Assigned(fHighlighter) and (Lines.Count >= 1) then
    if (Index > 0) then begin
{begin}                                                                         //mh 2000-10-10
//      fHighlighter.SetRange(Lines.Objects[Index - 1]);
      // the line and the range of the line
      //DebugLn(['TCustomSynEdit.ListInserted A Index=',Index]);
      fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Index - 1]);
      ScanFrom(Index - 1);
    end else begin
      //DebugLn(['TCustomSynEdit.ListInserted B Index=',Index]);
      fHighlighter.ReSetRange;
//      Lines.Objects[0] := fHighlighter.GetRange;
      TSynEditStringList(Lines).Ranges[0] := fHighlighter.GetRange;
{end}                                                                           //mh 2000-10-10
      if (Lines.Count > 1) then
        ScanFrom(0);
    end;
  InvalidateLines(Index + 1, TopLine + LinesInWindow);
  InvalidateGutterLines(Index + 1, TopLine + LinesInWindow);
end;

procedure TCustomSynEdit.ListPutted(Index: Integer);
{$IFDEF SYN_LAZARUS}
var
  EndIndex: Integer;
{$ENDIF}
begin
  //DebugLn(['TCustomSynEdit.ListPutted Index=',Index,' PaintLock=',PaintLock]);
  {$IFDEF SYN_LAZARUS}
  if PaintLock>0 then begin
    if (fHighlighterNeedsUpdateStartLine<1)
    or (fHighlighterNeedsUpdateStartLine>Index+1) then
      fHighlighterNeedsUpdateStartLine:=Index+1;
    if (fHighlighterNeedsUpdateEndLine<1)
    or (fHighlighterNeedsUpdateEndLine<Index+1) then
      fHighlighterNeedsUpdateEndLine:=Index+1;
    exit;
  end;
  if Assigned(fHighlighter) then begin
    fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Index]);             //mh 2000-10-10
    EndIndex:=ScanFrom(Index) + 1;
    InvalidateLines(Index + 1, EndIndex);
    InvalidateGutterLines(Index + 1, EndIndex);
  end else
    InvalidateLines(Index + 1, Index + 1);
  {$ELSE}
  if Assigned(fHighlighter) then begin
//    fHighlighter.SetRange(Lines.Objects[Index]);
    fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Index]);             //mh 2000-10-10
    InvalidateLines(Index + 1, ScanFrom(Index) + 1);
  end else
    InvalidateLines(Index + 1, Index + 1);
  {$ENDIF}
end;

procedure TCustomSynEdit.ListScanRanges(Sender: TObject);
{$IFNDEF SYN_LAZARUS}
var
  i: integer;
{$ENDIF}
begin
  if Assigned(fHighlighter) and (Lines.Count > 0) then begin
    {$IFDEF SYN_LAZARUS}
    ScanFrom(0,Lines.Count-1);
    {$ELSE}
    fHighlighter.ResetRange;
{begin}                                                                         //mh 2000-10-10
(*
    Lines.Objects[0] := fHighlighter.GetRange;
    i := 1;
    while (i < Lines.Count) do begin
      fHighlighter.SetRange(Lines.Objects[i - 1]);
      fHighlighter.SetLine(Lines[i - 1], i - 1);
      fHighlighter.NextToEol;
      Lines.Objects[i] := fHighlighter.GetRange;
      Inc(i);
    end;
*)
    i := 0;
    repeat
      TSynEditStringList(Lines).Ranges[i] := fHighlighter.GetRange;
      fHighlighter.SetLine(Lines[i], i);
      fHighlighter.NextToEol;
      Inc(i);
    until i >= Lines.Count;
{end}                                                                           //mh 2000-10-10
    {$ENDIF}
  end;
end;

{$IFDEF SYN_MBCSSUPPORT}
type
  TStringType = (stNone, stHalfNumAlpha, stHalfSymbol, stHalfKatakana,
    stWideNumAlpha, stWideSymbol, stWideKatakana, stHiragana, stIdeograph,
    stControl, stKashida);

{  }

function IsStringType(Value: Word): TStringType;
begin
  Result := stNone;

  if (Value = C3_SYMBOL) then begin
    (***  Controls  ***)
    Result := stControl;
  end else
    if ((Value and C3_HALFWIDTH) <> 0) then begin
    (*** singlebyte ***)
      if (Value = C3_HALFWIDTH) or
        (Value = (C3_ALPHA or C3_HALFWIDTH)) then begin { Number & Alphabet }
        Result := stHalfNumAlpha;
      end else
        if ((Value and C3_SYMBOL) <> 0) or
          ((Value and C3_LEXICAL) <> 0) then begin { Symbol }
          Result := stHalfSymbol;
        end else
          if ((Value and C3_KATAKANA) <> 0) then begin { Japanese-KATAKANA }
            Result := stHalfKatakana;
          end;
    end else begin
    (*** doublebyte ***)
      if (Value = C3_FULLWIDTH) or
        (Value = (C3_ALPHA or C3_FULLWIDTH)) then begin { Number & Alphabet }
        Result := stWideNumAlpha;
      end
      else if ((Value and C3_SYMBOL) <> 0) or
          ((Value and C3_LEXICAL) <> 0) then begin { Symbol }
        Result := stWideSymbol;
      end
      else if ((Value and C3_KATAKANA) <> 0) then begin { Japanese-KATAKANA }
        Result := stWideKatakana;
      end
      else if ((Value and C3_HIRAGANA) <> 0) then begin { Japanese-HIRAGANA }
        Result := stHiragana;
      end
      else if ((Value and C3_IDEOGRAPH) <> 0) then begin { Ideograph }
        Result := stIdeograph;
      end;
    end;
end;

{  }

procedure TCustomSynEdit.SetWordBlock(Value: TPoint);
var
  i: Integer;
  Runner: TPoint;
  TempString: string;
  IdChars: TSynIdentChars;

  procedure MultiBlockScan;
  var
    i: Integer;
    wideX: Integer;
    cType: PWordArray;
    cLeng: Integer;
    stc: TStringType;
  begin
    wideX := ByteToCharIndex(TempString, Value.X - 1);

    cLeng := ByteToCharLen(TempString, Length(TempString));
    GetMem(cType, SizeOf(Word) * cLeng);
    try
      if not GetStringTypeEx(LOCALE_SYSTEM_DEFAULT, CT_CTYPE3,
        PChar(TempString), Length(TempString), cType^)
      then
        exit;
      stc := IsStringType(cType^[wideX]);
      if (stc = stControl) then
        exit;
      { search BlockEnd }
      for i := wideX + 1 to cLeng - 1 do
        if (IsStringType(cType^[i]) <> stc) then begin
          Runner.Y := (i + 1);
          Break;
        end;
      Runner.Y := (i + 1);
      if Runner.Y > cLeng then Runner.Y := cLeng;
      { search BlockBegin }
      for i := wideX - 1 downto 0 do
        if (IsStringType(cType^[i]) <> stc) then begin
          Runner.X := (i + 2);
          Break;
        end;
      Runner.X := CharToByteIndex(TempString, Runner.X);
      Runner.Y := CharToByteIndex(TempString, Runner.Y);
    finally
      FreeMem(cType);
    end;
  end;

begin
  Value.x := MinMax(Value.x, 1, fMaxLeftChar);
  Value.y := MinMax(Value.y, 1, Lines.Count);
  TempString := (Lines[Value.Y - 1] + #$0);
  if (Value.X >= Length(TempString)) then begin
    CaretXY := Point(Length(TempString), Value.Y);
    exit;
  end;
  if (fHighlighter <> nil) and
    (ByteType(TempString, Value.X) <> mbLeadByte) then begin
    Runner := Point(0, Length(TempString));
    IdChars := fHighlighter.IdentChars;
    { search BlockEnd }
    for i := Value.X to Length(TempString) - 1 do begin
      if not (TempString[i] in IdChars) then begin
        Runner.Y := i;
        Break;
      end;
    end;
    { search BlockBegin }
    for i := Value.X - 1 downto 1 do begin
      if not (TempString[i] in IdChars) then begin
        Runner.X := (i + 1);
        Break;
      end;
    end;
  end else
    MultiBlockScan;
  SetCaretAndSelection(Point(Runner.Y, Value.Y), Point(Runner.X, Value.Y),
    Point(Runner.Y, Value.Y));
  InvalidateLine(Value.Y);
  StatusChanged([scSelection]);
end;

{$ELSE}

procedure TCustomSynEdit.SetWordBlock(Value: TPoint);
var
  Runner: TPoint;
  TempString: string;
  IdChars: TSynIdentChars;
begin
  Value.x := MinMax(Value.x, 1, fMaxLeftChar);
  Value.y := MinMax(Value.y, 1, Lines.Count);
  TempString := Lines[Value.Y - 1];
  if TempString = '' then exit;
  // Click on right side of text
  if Length(TempString) < Value.X then Value.X := Length(TempString);
  Runner := Value;
  if fHighlighter <> nil then
    IdChars := fHighlighter.IdentChars
  else
    IDchars := [#33..#255];
  if not (TempString[Runner.X] in IdChars) then begin
    // no word under cursor and next char right is not start of a word
    if (Runner.X > 1) and (not (TempString[Runner.X] in IdChars)) then begin
      // find end of word on the left side
      while Runner.X > 1 do begin
        if (TempString[Runner.X] in IdChars) then break;
        Dec(Runner.X);
      end;
    end;
    // no word on the left side, so look to the right side
    if not (TempString[Runner.X] in IdChars) then begin
      Runner := Value;
      while (Runner.X < fMaxLeftChar)
      {$IFDEF FPC} and (Runner.X < length(TempString)){$ENDIF} do begin
        if (TempString[Runner.X] in IdChars) then break;
        Inc(Runner.X);
      end;
      if Runner.X > fMaxLeftChar then
        exit;
    end;
    Value := Runner;
  end;
  while Runner.X > 0 do begin
    if not (TempString[Runner.X] in IdChars) then break;
    Dec(Runner.X);
  end;
  Inc(Runner.X);
  if Runner.X < 1 then Runner.X := 1;
  fBlockBegin := Runner;
  Runner := Value;
  while (Runner.X < fMaxLeftChar)
  {$IFDEF FPC} and (Runner.X <= length(TempString)){$ENDIF} do begin
    if not (TempString[Runner.X] in IdChars) then break;
    Inc(Runner.X);
  end;
  if Runner.X > fMaxLeftChar then Runner.X := fMaxLeftChar;
  fBlockEnd := Runner;
// set caret to the end of selected block
  CaretXY := Runner;
  InvalidateLine(Value.Y);
  StatusChanged([scSelection]);
end;

{$ENDIF}

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.SetLineBlock(Value: TPoint);
var
  ALine: string;
begin
  fBlockBegin:=Point(1,MinMax(Value.y, 1, Lines.Count));
  fBlockEnd:=Point(1,MinMax(Value.y+1, 1, Lines.Count));
  if (fBlockBegin.Y>=1) and (fBlockBegin.Y<=Lines.Count) then begin
    ALine:=Lines[fBlockBegin.Y-1];
    while (fBlockBegin.X<length(ALine)) and (ALine[fBlockBegin.X] in [' ',#9])
    do
      inc(fBlockBegin.X);
    fBlockEnd:=fBlockBegin;
    fBlockEnd.X:=length(ALine)+1;
    while (fBlockEnd.X>fBlockBegin.X) and (ALine[fBlockEnd.X-1] in [' ',#9])
    do
      dec(fBlockEnd.X);
  end;
  CaretXY:=fBlockEnd;
  //DebugLn(' FFF2 ',Value.X,',',Value.Y,' BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
  InvalidateLine(Value.Y);
  StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.SetParagraphBlock(Value: TPoint);
var ParagraphStartLine, ParagraphEndLine: integer;
begin
  ParagraphStartLine:=MinMax(Value.y, 1, Lines.Count);
  ParagraphEndLine:=MinMax(Value.y+1, 1, Lines.Count);
  while (ParagraphStartLine>1)
  and (Trim(Lines[ParagraphStartLine-1])<>'') do
    dec(ParagraphStartLine);
  while (ParagraphEndLine<Lines.Count)
  and (Trim(Lines[ParagraphEndLine-1])<>'') do
    inc(ParagraphEndLine);
  fBlockBegin:=Point(1,ParagraphStartLine);
  fBlockEnd:=Point(1,ParagraphEndLine);
  CaretXY:=fBlockEnd;
  //DebugLn(' FFF3 ',Value.X,',',Value.Y,' BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
  InvalidateLine(Value.Y);
  StatusChanged([scSelection]);
end;
{$ENDIF}

procedure TCustomSynEdit.DblClick;
var
  ptMouse: TPoint;
begin
  GetCursorPos(ptMouse);
  ptMouse := ScreenToClient(ptMouse);
  if ptMouse.X >= fGutterWidth + 2 then begin
    if not (eoNoSelection in fOptions) then begin
      {$IFDEF SYN_LAZARUS}
      if (eoDoubleClickSelectsLine in fOptions) then
        SetLineBlock(PixelsToLogicalPos(ptMouse))
      else
        SetWordBlock(PixelsToLogicalPos(ptMouse));
      {$ELSE}
      SetWordBlock(CaretXY);
      {$ENDIF}
    end;
    inherited;
    Include(fStateFlags, sfDblClicked);
    MouseCapture := FALSE;
  end else
    inherited;
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.TripleClick;
var
  ptMouse: TPoint;
begin
  GetCursorPos(ptMouse);
  ptMouse := ScreenToClient(ptMouse);
  if ptMouse.X >= fGutterWidth + 2 then begin
    if not (eoNoSelection in fOptions) then begin
      SetLineBlock(PixelsToLogicalPos(ptMouse))
    end;
    inherited;
    Include(fStateFlags, sfTripleClicked);
    MouseCapture := FALSE;
  end else
    inherited;
end;

procedure TCustomSynEdit.QuadClick;
var
  ptMouse: TPoint;
begin
  GetCursorPos(ptMouse);
  ptMouse := ScreenToClient(ptMouse);
  if ptMouse.X >= fGutterWidth + 2 then begin
    if not (eoNoSelection in fOptions) then begin
      SetParagraphBlock(PixelsToLogicalPos(ptMouse))
    end;
    inherited;
    Include(fStateFlags, sfQuadClicked);
    MouseCapture := FALSE;
  end else
    inherited;
end;
{$ENDIF}

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
    or Clipboard.HasFormat(SynEditClipboardFormat)
end;

procedure TCustomSynEdit.InsertBlock(BB, BE: TPoint; ChangeStr: PChar);
// used by BlockIndent and Redo
begin
  SetCaretAndSelection(
    {$IFDEF SYN_LAZARUS}LogicalToPhysicalPos(BB){$ELSE}BB{$ENDIF},
    BB, BE);
  fSelectionMode := smColumn;
  SetSelTextPrimitive(smColumn, ChangeStr, nil);
  StatusChanged([scSelection]);
end;

procedure TCustomSynEdit.Redo;
{begin}                                                                         //sbs 2000-11-19
var
  Item: TSynEditUndoItem;
  OldChangeNumber: integer;
begin
  Item := fRedoList.PeekItem;
  if Item <> nil then begin
    OldChangeNumber := fUndoList.BlockChangeNumber;
    fUndoList.BlockChangeNumber := Item.fChangeNumber;
    try
      repeat
        RedoItem;
        Item := fRedoList.PeekItem;
      until (Item = nil) or (Item.fChangeNumber <> fUndoList.BlockChangeNumber);
    finally
      fUndoList.BlockChangeNumber := OldChangeNumber;
    end;
  end;
end;

procedure TCustomSynEdit.RedoItem;
{end}                                                                           //sbs 2000-11-19
var
  Item: TSynEditUndoItem;
  OldSelMode: TSynSelectionMode;
  Run, StrToDelete: PChar;
  Len, e, x : integer;
  TempString: string;
  CaretPt: TPoint;
  ChangeScrollPastEol: boolean;                                                 //mh 2000-10-30
  {$IFDEF SYN_LAZARUS}
  PhysStartPos: TPoint;
  PhysEndPos: TPoint;
  {$ENDIF}
begin
  OldSelMode := SelectionMode;
  ChangeScrollPastEol := not (eoScrollPastEol in Options);                      //mh 2000-10-30
  Item := fRedoList.PopItem;
  if Assigned(Item) then try
    SelectionMode := Item.fChangeSelMode;
    IncPaintLock;
    Include(fOptions, eoScrollPastEol);                                         //mh 2000-10-30
    Include(fStateFlags, sfInsideRedo);                                         //mh 2000-10-30
    {$IFDEF SYN_LAZARUS}
    PhysStartPos:=LogicalToPhysicalPos(Item.fChangeStartPos);
    PhysEndPos:=LogicalToPhysicalPos(Item.fChangeEndPos);
    {$ENDIF}
    case Item.fChangeReason of
      crInsert, crPaste, crDragDropInsert:
        begin
          SetCaretAndSelection(
            {$IFDEF SYN_LAZARUS}PhysStartPos{$ELSE}Item.fChangeStartPos{$ENDIF},
            Item.fChangeStartPos, Item.fChangeStartPos
            );
          SetSelTextPrimitive(Item.fChangeSelMode, PChar(Item.fChangeStr), nil);
          {$IFDEF SYN_LAZARUS}
          CaretXY := PhysEndPos;
          {$ELSE}
          CaretXY := Item.fChangeEndPos;                                        //mh 2000-10-30
          {$ENDIF}
          fUndoList.AddChange(Item.fChangeReason, Item.fChangeStartPos,
            Item.fChangeEndPos, GetSelText, Item.fChangeSelMode);
{begin}                                                                         //mh 2000-11-20
          if Item.fChangeReason = crDragDropInsert then begin
            SetCaretAndSelection(
              {$IFDEF SYN_LAZARUS}PhysStartPos{$ELSE}Item.fChangeStartPos{$ENDIF},
              Item.fChangeStartPos, Item.fChangeEndPos);
          end;
{end}                                                                           //mh 2000-11-20
        end;
      crDeleteAfterCursor, crSilentDeleteAfterCursor:                           //mh 2000-10-30
        begin
          SetCaretAndSelection(
            {$IFDEF SYN_LAZARUS}PhysStartPos{$ELSE}Item.fChangeStartPos{$ENDIF},
            Item.fChangeStartPos, Item.fChangeStartPos
            );
          fUndoList.AddChange(Item.fChangeReason, Item.fChangeStartPos,
            Item.fChangeEndPos, GetSelText, Item.fChangeSelMode);
          SetSelTextPrimitive(Item.fChangeSelMode, PChar(Item.fChangeStr), nil);
          {$IFDEF SYN_LAZARUS}
          CaretXY := PhysStartPos;
          {$ELSE}
          CaretXY := Item.fChangeStartPos;
          {$ENDIF}
        end;
      crDelete, {crDragDropDelete, crSelDelete, }crSilentDelete:                //mh 2000-10-30, 2000-11-20
        begin
          SetCaretAndSelection(
            {$IFDEF SYN_LAZARUS}PhysStartPos{$ELSE}Item.fChangeStartPos{$ENDIF},
            Item.fChangeStartPos, Item.fChangeStartPos
            );
          fUndoList.AddChange(Item.fChangeReason, Item.fChangeStartPos,
            Item.fChangeEndPos, GetSelText, Item.fChangeSelMode);
          SetSelTextPrimitive(Item.fChangeSelMode, PChar(Item.fChangeStr), nil);
          {$IFDEF SYN_LAZARUS}
          CaretXY := PhysStartPos;
          {$ELSE}
          CaretXY := Item.fChangeStartPos;
          {$ENDIF}
{begin}                                                                         //mh 2000-11-20
(*
          // process next entry? This is awkward, and should be replaced by
          // undoitems maintaining a single linked list of connected items...
          ItemNext := fRedoList.PeekItem;
          if {(Item.fChangeReason = crSelDelete) or }
            ((Item.fChangeReason = crDragDropDelete) and Assigned(ItemNext)
              and (ItemNext.fChangeReason = crDragDropInsert))
          then
            Redo;
*)
{end}                                                                           //mh 2000-11-20
        end;
      crLineBreak:
{begin}                                                                         //sbs 2000-11-20
//        CommandProcessor(ecLineBreak, #13, nil);
        begin
          {$IFDEF SYN_LAZARUS}
          SetCaretAndSelection(PhysStartPos, Item.fChangeStartPos,
                               Item.fChangeStartPos);
          {$ELSE}
          CaretPt := Item.fChangeStartPos;
          SetCaretAndSelection(CaretPt, CaretPt, CaretPt);
          {$ENDIF}
          CommandProcessor(ecLineBreak, #13, nil);
        end;
{end}                                                                           //sbs 2000-11-20
      crIndent:
        begin // re-insert the column
          if (Item.fChangeEndPos.X = 1) then
          begin
            e := Item.fChangeEndPos.y - 1;
            x := 1;
          end else begin
            e := Item.fChangeEndPos.y;
            x := Item.fChangeEndPos.x
                      + {$IFDEF SYN_LAZARUS}fBlockIndent{$ELSE}fTabWidth{$ENDIF};
          end;
          InsertBlock(Point(1, Item.fChangeStartPos.y),
            Point(1, e), PChar(Item.fChangeStr));
          // add to undo list
          fUndoList.AddChange(Item.fChangeReason, Item.fChangeStartPos,
            Item.fChangeEndPos, Item.fChangeStr, Item.fChangeSelMode);
          // restore the selection
          SetCaretAndSelection(
            {$IFDEF SYN_LAZARUS}
            LogicalToPhysicalPos(Point(1, Item.fChangeEndPos.Y + 1)),
            {$ELSE}
            Point(1, Item.fChangeEndPos.Y + 1),
            {$ENDIF}
            Point(Item.fChangeStartPos.x + fTabWidth,Item.fChangeStartPos.y),
            Point(x, Item.fChangeEndPos.y));
        end;
      crUnindent :
        begin // re-delete the (raggered) column
          // add to undo list
          fUndoList.AddChange(Item.fChangeReason, Item.fChangeStartPos,
            Item.fChangeEndPos, Item.fChangeStr, smColumn);
          // Delete string
          StrToDelete := PChar(Item.fChangeStr);
          CaretY := Item.fChangeStartPos.Y;
          repeat
            Run := GetEOL(StrToDelete);
            if Run <> StrToDelete then begin
              Len := Run - StrToDelete;
              TempString := Lines[CaretY - 1];
              if Len > 0 then
                Delete(TempString, 1, Len);
              Lines[CaretY - 1] := TempString;
            end else
              Len := 0;
            {$IFDEF SYN_LAZARUS}
            if Run^ in [#10,#13] then begin
              if (Run[1] in [#10,#13]) and (Run^<>Run[1]) then
                Inc(Run,2)
              else
                Inc(Run);
              Inc(fCaretY);
            end;
            {$ELSE}
            if Run^ = #13 then begin
              Inc(Run);
              if Run^ = #10 then
                Inc(Run);
              Inc(fCaretY);
            end;
            {$ENDIF}
            StrToDelete := Run;
          until Run^ = #0;
          // restore selection
          CaretPt := Point(Item.fChangeStartPos.x - fTabWidth,
                           Item.fChangeStartPos.y);
          SetCaretAndSelection(
            {$IFDEF SYN_LAZARUS}
            LogicalToPhysicalPos(CaretPt),
            {$ELSE}
            CaretPt,
            {$ENDIF}
            CaretPt,
            Point(Item.fChangeEndPos.x - Len, Item.fChangeEndPos.y)
            );
        end;
    end;
  finally
    SelectionMode := OldSelMode;
    Exclude(fStateFlags, sfInsideRedo);                                         //mh 2000-10-30
    if ChangeScrollPastEol then                                                 //mh 2000-10-30
      Exclude(fOptions, eoScrollPastEol);
    Item.Free;
    DecPaintLock;
    {$IFDEF SYN_LAZARUS}
    if fRedoList.IsTopMarkedAsUnmodified then
      fUndoList.MarkTopAsUnmodified;
    {$ENDIF}
  end;
end;

procedure TCustomSynEdit.Undo;
{begin}                                                                         //sbs 2000-11-19
var
  Item: TSynEditUndoItem;
  OldChangeNumber: integer;
begin
  Item := fUndoList.PeekItem;
  if Item <> nil then begin
    OldChangeNumber := fRedoList.BlockChangeNumber;
    fRedoList.BlockChangeNumber := Item.fChangeNumber;
    try
      repeat
        UndoItem;
        Item := fUndoList.PeekItem;
      until (Item = nil) or (Item.fChangeNumber <> fRedoList.BlockChangeNumber);
    finally
      fRedoList.BlockChangeNumber := OldChangeNumber;
    end;
  end;
end;

procedure TCustomSynEdit.UndoItem;
{end}                                                                           //sbs 2000-11-19
var
  Item: TSynEditUndoItem;
  OldSelMode: TSynSelectionMode;
  TmpPos: TPoint;
  TmpStr: string;
  ChangeScrollPastEol: boolean;                                                 //mh 2000-10-30
  {$IFDEF SYN_LAZARUS}
  PhysStartPos: TPoint;
  {$ENDIF}
begin
  OldSelMode := SelectionMode;
  ChangeScrollPastEol := not (eoScrollPastEol in Options);                      //mh 2000-10-30
  Item := fUndoList.PopItem;
  if Assigned(Item) then try
    SelectionMode := Item.fChangeSelMode;
    IncPaintLock;
    Include(fOptions, eoScrollPastEol);                                         //mh 2000-10-30
    {$IFDEF SYN_LAZARUS}
    PhysStartPos:=LogicalToPhysicalPos(Item.fChangeStartPos);
    {$ENDIF}
    case Item.fChangeReason of
      crInsert, crPaste, crDragDropInsert:
        begin
          SetCaretAndSelection(
            {$IFDEF SYN_LAZARUS}PhysStartPos{$ELSE}Item.fChangeStartPos{$ENDIF},
            Item.fChangeStartPos, Item.fChangeEndPos);
          fRedoList.AddChange(Item.fChangeReason, Item.fChangeStartPos,
            Item.fChangeEndPos, GetSelText, Item.fChangeSelMode);
          SetSelTextPrimitive(Item.fChangeSelMode, PChar(Item.fChangeStr), nil);
          CaretXY := {$IFDEF SYN_LAZARUS}PhysStartPos
                     {$ELSE}Item.fChangeStartPos{$ENDIF};
{begin}                                                                         //mh 2000-11-20
(*
          // process next entry? This is awkward, and should be replaced by
          // undoitems maintaining a single linked list of connected items...
          ItemNext := fUndoList.PeekItem;
          if Assigned(ItemNext) and
            ((ItemNext.fChangeReason = crSelDelete) or
            ((ItemNext.fChangeReason = crDragDropDelete)
               and (Item.fChangeReason = crDragDropInsert)))
          then
            Undo;
*)
{end}                                                                           //mh 2000-11-20
        end;
      crDeleteAfterCursor, crDelete, {crDragDropDelete, crSelDelete, }          //mh 2000-11-20
      crSilentDelete, crSilentDeleteAfterCursor:                                //mh 2000-10-30
        begin
          // If there's no selection, we have to set
          // the Caret's position manually.
          if Item.fChangeSelMode = smColumn then
            TmpPos := Point(Min(Item.fChangeStartPos.X, Item.fChangeEndPos.X),
              Min(Item.fChangeStartPos.Y, Item.fChangeEndPos.Y))
          else
            TmpPos := minPoint(Item.fChangeStartPos, Item.fChangeEndPos);
          if (Item.fChangeReason in [crDeleteAfterCursor,
            crSilentDeleteAfterCursor]) and (TmpPos.Y > Lines.Count)            //mh 2000-10-30
          then begin
            CaretXY := Point(1, Lines.Count);
            // this stinks!!!
            CommandProcessor(ecLineBreak, #13, nil);
          end;
          SetCaretAndSelection(
            {$IFDEF SYN_LAZARUS}
            LogicalToPhysicalPos(TmpPos),
            {$ELSE}
            TmpPos,
            {$ENDIF}
            TmpPos, TmpPos);
          //debugln('AAA1 Item.fChangeStr="',DbgStr(Item.fChangeStr),'"');
          SetSelTextPrimitive(Item.fChangeSelMode, PChar(Item.fChangeStr), nil);
{begin}                                                                         //mh 2000-10-30
          if Item.fChangeReason in [crDeleteAfterCursor,
            crSilentDeleteAfterCursor]
          then
            TmpPos := Item.fChangeStartPos
          else
            TmpPos := Item.fChangeEndPos;
          if Item.fChangeReason in [crSilentDelete, crSilentDeleteAfterCursor]
          then
            CaretXY :={$IFDEF SYN_LAZARUS}LogicalToPhysicalPos(TmpPos)
                      {$ELSE}TmpPos{$ENDIF}
          else begin
            SetCaretAndSelection(
              {$IFDEF SYN_LAZARUS}
              LogicalToPhysicalPos(TmpPos),
              {$ELSE}
              TmpPos,
              {$ENDIF}
              Item.fChangeStartPos, Item.fChangeEndPos);
          end;
{end}                                                                           //mh 2000-10-30
          fRedoList.AddChange(Item.fChangeReason, Item.fChangeStartPos,
            Item.fChangeEndPos, '', Item.fChangeSelMode);
          EnsureCursorPosVisible;
        end;
      crLineBreak:
        begin
          // If there's no selection, we have to set
          // the Caret's position manualy.
          CaretXY := Item.fChangeStartPos;
          fRedoList.AddChange(Item.fChangeReason, Item.fChangeStartPos,
            Item.fChangeEndPos, '', Item.fChangeSelMode);
          if CaretY > 0 then begin
            TmpStr := Lines.Strings[CaretY - 1];
            if (Length(TmpStr) < CaretX - 1)
              and (LeftSpaces(Item.fChangeStr) = 0)
            then
              AppendStr(TmpStr, StringOfChar(' ', CaretX - 1 - Length(TmpStr)));
            Lines.Delete(Item.fChangeEndPos.y);
          end;
          CaretXY := {$IFDEF SYN_LAZARUS}PhysStartPos
                     {$ELSE}Item.fChangeStartPos{$ENDIF};
          TrimmedSetLine(CaretY - 1, TmpStr + Item.fChangeStr);
          DoLinesDeleted(CaretY, 1);
        end;
      crIndent: // remove the column that was inserted
        begin
          // select the inserted column
          BlockBegin := Point(1, Item.fChangeStartPos.y);
          TmpPos := Item.fChangeEndPos;
          if TmpPos.x = 1 then
            Dec(TmpPos.y);
          TmpPos.x := {$IFDEF SYN_LAZARUS}fBlockIndent{$ELSE}fTabWidth{$ENDIF}+1;
          BlockEnd := TmpPos;
          // add to redo list
          fRedoList.AddChange(Item.fChangeReason, Item.fChangeStartPos,
            Item.fChangeEndPos, GetSelText, Item.fChangeSelMode);
          // remove the column
          SetSelTextPrimitive(Item.fChangeSelMode, nil, nil);
          // restore the selection
          SetCaretAndSelection(
            {$IFDEF SYN_LAZARUS}
            LogicalToPhysicalPos(Item.fChangeEndPos),
            {$ELSE}
            Item.fChangeEndPos,
            {$ENDIF}
            Item.fChangeStartPos, Item.fChangeEndPos);
        end;
      crUnindent: // reinsert the (raggered) column that was deleted
        begin
         fRedoList.AddChange(Item.fChangeReason, Item.fChangeStartPos,
            Item.fChangeEndPos, Item.fChangeStr, Item.fChangeSelMode);
          // reinsert the string
          InsertBlock(Point(1, Item.fChangeStartPos.y),
            Point(1, Item.fChangeEndPos.y), PChar(Item.fChangeStr));
          SetCaretAndSelection(
            {$IFDEF SYN_LAZARUS}
            LogicalToPhysicalPos(Item.fChangeStartPos),
            {$ELSE}
            Item.fChangeStartPos,
            {$ENDIF}
            Item.fChangeStartPos, Item.fChangeEndPos);
        end;
    end;
  finally
    SelectionMode := OldSelMode;
    if ChangeScrollPastEol then                                                 //mh 2000-10-30
      Exclude(fOptions, eoScrollPastEol);
    Item.Free;
    DecPaintLock;
    {$IFDEF SYN_LAZARUS}
    if fUndoList.IsTopMarkedAsUnmodified then
      fRedoList.MarkTopAsUnmodified;
    {$ENDIF}
  end;
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.UpdateCtrlMouse;

  procedure doNotShowLink;
  begin
    if fLastCtrlMouseLinkY>0 then begin
      Invalidate;
      Cursor := crIBeam;
    end;
  end;

var
  NewY, NewX1, NewX2: integer;
begin
  fLastControlIsPressed:=(GetKeyShiftState=[ssCtrl]);
  if (eoShowCtrlMouseLinks in Options) and fLastControlIsPressed
  and (fLastMouseCaret.X>0) and (fLastMouseCaret.Y>0) then begin
    // show link
    NewY:=fLastMouseCaret.Y;
    GetWordBoundsAtRowCol(PhysicalToLogicalPos(fLastMouseCaret),NewX1,NewX2);
    if NewX1<>NewX2 then begin
      // there is a word to underline as link
      if (NewY<>fLastCtrlMouseLinkY)
      or (NewX1<>fLastCtrlMouseLinkX1)
      or (NewX2<>fLastCtrlMouseLinkX2)
      then begin
        Invalidate;
        Cursor := crHandPoint;
      end;
    end else
      doNotShowLink // there is no link
  end else
    doNotShowLink;
end;
{$ENDIF}

procedure TCustomSynEdit.ClearBookMark(BookMark: Integer);
begin
  if (BookMark in [0..9]) and assigned(fBookMarks[BookMark]) then begin
    DoOnClearBookmark(fBookMarks[BookMark]);                                    // djlp 2000-08-29
    FMarkList.Remove(fBookMarks[Bookmark]);
    fBookMarks[BookMark].Free;
    fBookMarks[BookMark] := nil;
  end
end;

procedure TCustomSynEdit.GotoBookMark(BookMark: Integer);
{$IFDEF SYN_LAZARUS}
var
  NewCaret: TPoint;
  LogCaret: TPoint;
{$ENDIF}
begin
  if (BookMark in [0..9]) and assigned(fBookMarks[BookMark])
    and (fBookMarks[BookMark].Line <= fLines.Count)
  then begin
    {$IFDEF SYN_LAZARUS}
    NewCaret:=Point(fBookMarks[BookMark].Column, fBookMarks[BookMark].Line);
    LogCaret:=PhysicalToLogicalPos(NewCaret);
    IncPaintLock;
    SetBlockEnd(LogCaret);
    SetBlockBegin(LogCaret);
    CaretXY:=NewCaret;
    EnsureCursorPosVisible;
    DecPaintLock;
    {$ELSE}
    CaretXY:=Point(fBookMarks[BookMark].Column, fBookMarks[BookMark].Line);     // djlp 2000-08-29
    EnsureCursorPosVisible;
    {$ENDIF}
  end;
end;

function TCustomSynEdit.IdentChars: TSynIdentChars;
begin
  if Highlighter <> nil then
    Result := Highlighter.IdentChars
  else
    {$IFDEF SYN_LAZARUS}
    Result := ['a'..'z','A'..'Z','0'..'9'];
    {$ELSE}
    Result := [#33..#255];
    {$ENDIF}
end;

{$IFDEF SYN_LAZARUS}
function TCustomSynEdit.IsIdentChar(const c: TUTF8Char): boolean;
begin
  Result:=(length(c)=1) and (c[1] in IdentChars);
end;
{$ENDIF}

procedure TCustomSynEdit.SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
var
  i: Integer;
  mark: TSynEditMark;
begin
  if (BookMark in [0..9]) and (Y >= 1) and (Y <= Max(1, fLines.Count)) then
  begin
    mark := TSynEditMark.Create(self);
    with mark do begin
      Line := Y;
      Column := X;
      ImageIndex := Bookmark;
      BookmarkNumber := Bookmark;
      Visible := true;
      InternalImage := (fBookMarkOpt.BookmarkImages = nil);
    end;
    DoOnPlaceMark(Mark);
    if (mark <> nil) and (BookMark in [0..9]) then begin
      for i := 0 to 9 do
        if assigned(fBookMarks[i]) and (fBookMarks[i].Line = Y) then
          ClearBookmark(i);
      if assigned(fBookMarks[BookMark]) then
        ClearBookmark(BookMark);
      fBookMarks[BookMark] := mark;
      FMarkList.Add(fBookMarks[BookMark]);
    end;
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

procedure TCustomSynEdit.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited;
  {$IFDEF SYN_LAZARUS}
  LastMouseCaret:=Point(-1,-1);
  {$ENDIF}
  if (Source is TCustomSynEdit) and not TCustomSynEdit(Source).ReadOnly then
  begin
    Accept := True;
    //Ctrl is pressed => change cursor to indicate copy instead of move
    if GetKeyState(VK_CONTROL) < 0 then
      DragCursor := crMultiDrag
    else
      DragCursor := crDrag;
    if State = dsDragLeave then //restore prev caret position
      ComputeCaret(FMouseDownX, FMouseDownY)
    else //position caret under the mouse cursor
      ComputeCaret(X, Y);
  end;
end;

procedure TCustomSynEdit.DragDrop(Source: TObject; X, Y: Integer);
var
  NewCaret: TPoint;
  DoDrop, DropAfter, DropMove: boolean;
  BB, BE: TPoint;
  DragDropText: string;
  Adjust: integer;
  ChangeScrollPastEOL: boolean;
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
        BeginUndoBlock;                                                         //mh 2000-11-20
        try
          DragDropText := TCustomSynEdit(Source).SelText;
          // delete the selected text if necessary
          if DropMove then begin
            if Source <> Self then
              TCustomSynEdit(Source).SelText := ''
            else begin
//              fUndoList.AddChange(crDragDropDelete, fBlockBegin, fBlockEnd,
              fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd,             //mh 2000-11-20
                DragDropText, SelectionMode);
              LockUndo;
              try
                SelText := '';
              finally
                UnlockUndo;
              end;
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
          ChangeScrollPastEOL := not (eoScrollPastEol in fOptions);
          try
            if ChangeScrollPastEOL then
              Include(fOptions, eoScrollPastEol);
            CaretXY := NewCaret;
            BlockBegin := NewCaret;
            LockUndo;
            try
              SelText := DragDropText;
            finally
              UnlockUndo;
            end;
          finally
            if ChangeScrollPastEOL then
              Exclude(fOptions, eoScrollPastEol);
          end;
          // save undo information
          if Source = Self then begin
            fUndoList.AddChange(crDragDropInsert,
              {$IFDEF SYN_LAZARUS}
              PhysicalToLogicalPos(NewCaret),
              {$ELSE}
              NewCaret,
              {$ENDIF}
              BlockEnd, SelText, SelectionMode);
          end else begin
            fUndoList.AddChange(crInsert,
              {$IFDEF SYN_LAZARUS}
              PhysicalToLogicalPos(NewCaret),
              {$ELSE}
              NewCaret,
              {$ENDIF}
              BlockEnd, SelText, SelectionMode);
          end;
          BlockBegin := {$IFDEF SYN_LAZARUS}PhysicalToLogicalPos(NewCaret)
                        {$ELSE}NewCaret{$ENDIF};
          BlockEnd := {$IFDEF SYN_LAZARUS}PhysicalToLogicalPos(CaretXY)
                        {$ELSE}CaretXY{$ENDIF};
          CaretXY := NewCaret;
        finally
          EndUndoBlock;
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
      rcInval := Rect(nX - 1, 0, nX + 1
         , ClientHeight{$IFDEF SYN_LAZARUS}-ScrollBarWidth{$ENDIF});
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
      fHighlighter := nil;
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

procedure TCustomSynEdit.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  if Value <> fHighlighter then begin
    if Assigned(fHighlighter) then
      fHighlighter.UnhookAttrChangeEvent(
        {$IFDEF FPC}@{$ENDIF}HighlighterAttrChanged);
    if Assigned(Value) then begin
      Value.HookAttrChangeEvent(
        {$IFDEF FPC}@{$ENDIF}HighlighterAttrChanged);
      Value.FreeNotification(Self);
    end;
    fHighlighter := Value;
    {$IFDEF SYN_LAZARUS}
    if fHighlighter<>nil then begin
      fHighlighter.ResetRange;
      TSynEditStringList(Lines).ClearRanges(fHighlighter.GetRange);
    end;
    {$ENDIF}
    RecalcCharExtent;
    Lines.BeginUpdate;
    try
      ListScanRanges(Self);
    finally
      Lines.EndUpdate;
    end;
    SizeOrFontChanged(TRUE);
  end;
end;

{$ifndef SYN_LAZARUS}
procedure TCustomSynEdit.SetBorderStyle(Value: TBorderStyle);
begin
  if fBorderStyle <> Value then begin
    fBorderStyle := Value;
    RecreateWnd;
  end;
end;
{$endif}

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
    if not (csDesigning in ComponentState) then
      // Reset the caret.
      InitializeCaret;
    StatusChanged([scInsertMode]);
  end;
end;

procedure TCustomSynEdit.InitializeCaret;
var
  ct: TSynEditCaretType;
  cw, ch: integer;
begin
  // CreateCaret automatically destroys the previous one, so we don't have to
  // worry about cleaning up the old one here with DestroyCaret.
  // Ideally, we will have properties that control what these two carets look like.
  if InsertMode then
    ct := FInsertCaret
  else
    ct := FOverwriteCaret;
  case ct of
    ctHorizontalLine:
      begin
        cw := fCharWidth;
        ch := 2;
        FCaretOffset := Point(0, fTextHeight - 2);
      end;
    ctHalfBlock:
      begin
        cw := fCharWidth;
        ch := (fTextHeight - 2) div 2;
        FCaretOffset := Point(0, ch);
      end;
    ctBlock:
      begin
        cw := fCharWidth;
        ch := fTextHeight - 2;
        FCaretOffset := Point(0, 0);
      end;
    else begin // ctVerticalLine
      cw := 2;
      ch := fTextHeight - 2;
      FCaretOffset := Point(-1, 0);
    end;
  end;
  Exclude(fStateFlags, sfCaretVisible);
  CreateCaret(Handle, 0, cw, ch);
  UpdateCaret;
end;

procedure TCustomSynEdit.SetInsertCaret(const Value: TSynEditCaretType);
begin
  if FInsertCaret <> Value then begin
    FInsertCaret := Value;
    InitializeCaret;
  end;
end;

procedure TCustomSynEdit.SetOverwriteCaret(const Value: TSynEditCaretType);
begin
  if FOverwriteCaret <> Value then begin
    FOverwriteCaret := Value;
    InitializeCaret;
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
  {$IFDEF SYN_LAZARUS}
  MinX: Integer;
  MaxX: Integer;
  PhysBlockBeginXY: TPoint;
  PhysBlockEndXY: TPoint;
  {$ENDIF}
begin
  IncPaintLock;
  try
    // Make sure X is visible
    //DebugLn('[TCustomSynEdit.EnsureCursorPosVisible] A CaretX=',CaretX,' LeftChar=',LeftChar,' CharsInWindow=',CharsInWindow,' ClientWidth=',ClientWidth);
    PhysCaretXY:=CaretXY;
    {$IFDEF SYN_LAZARUS}
    // try to make the current selection visible as well
    MinX:=PhysCaretXY.X;
    MaxX:=PhysCaretXY.X;
    PhysBlockBeginXY:=LogicalToPhysicalPos(BlockBegin);
    PhysBlockEndXY:=LogicalToPhysicalPos(BlockEnd);
    if (PhysBlockBeginXY.X<>PhysBlockEndXY.X)
    or (PhysBlockBeginXY.Y<>PhysBlockEndXY.Y) then begin
      if (SelectionMode<>smColumn) and (PhysBlockBeginXY.Y<>PhysBlockEndXY.Y) then
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
    {DebugLn('TCustomSynEdit.EnsureCursorPosVisible A CaretX=',dbgs(PhysCaretXY.X),
      ' BlockX=',dbgs(PhysBlockBeginXY.X)+'-'+dbgs(PhysBlockEndXY.X),
      ' CharsInWindow='+dbgs(CharsInWindow),
      ' MinX='+dbgs(MinX),
      ' MaxX='+dbgs(MaxX),
      ' LeftChar='+dbgs(LeftChar),
      '');}
    if MinX < LeftChar then
      LeftChar := MinX
    else if LeftChar < MaxX - (CharsInWindow - 1) then
      LeftChar := MaxX - (CharsInWindow - 1)
    else
      LeftChar := LeftChar;                                                     //mh 2000-10-19
    //DebugLn('TCustomSynEdit.EnsureCursorPosVisible B LeftChar=',LeftChar);
    {$ELSE}
    if PhysCaretXY.X < LeftChar then
      LeftChar := PhysCaretXY.X
    else if PhysCaretXY.X > CharsInWindow + LeftChar then
      LeftChar := PhysCaretXY.X - CharsInWindow + 1
    else
      LeftChar := LeftChar;                                                     //mh 2000-10-19
    {$ENDIF}
    // Make sure Y is visible
    if CaretY < TopLine then
      TopLine := CaretY
    else if CaretY > TopLine + Max(1, LinesInWindow) - 1 then                   //mh 2000-10-19
      {$IFDEF SYN_LAZARUS}
      TopLine := CaretY - (Max(1,LinesInWindow) - 1)
      {$ELSE}
      TopLine := CaretY - (LinesInWindow - 1)
      {$ENDIF}
    else
      TopLine := TopLine;                                                       //mh 2000-10-19
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.SetKeystrokes(const Value: TSynEditKeyStrokes);
begin
  if Value = nil then
    FKeystrokes.Clear
  else
    FKeystrokes.Assign(Value);
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.SetLastMouseCaret(const AValue: TPoint);
begin
  if (FLastMouseCaret.X=AValue.X) and (FLastMouseCaret.Y=AValue.Y) then exit;
  FLastMouseCaret:=AValue;
  UpdateCtrlMouse;
end;
{$ENDIF}

procedure TCustomSynEdit.SetDefaultKeystrokes;
begin
  FKeystrokes.ResetDefaults;
end;

// If the translations requires Data, memory will be allocated for it via a
// GetMem call.  The client must call FreeMem on Data if it is not NIL.

function TCustomSynEdit.TranslateKeyCode(Code: word; Shift: TShiftState;
  var Data: pointer): TSynEditorCommand;
var
  i: integer;
{$IFNDEF SYN_COMPILER_3_UP}
const
  VK_ACCEPT = $30;
{$ENDIF}
begin
  i := KeyStrokes.FindKeycode2(fLastKey, fLastShiftState, Code, Shift);
  if i >= 0 then begin
    Result := KeyStrokes[i].Command
  end else begin
    i := Keystrokes.FindKeycode(Code, Shift);
    if i >= 0 then begin
      Result := Keystrokes[i].Command
    end else
      Result := ecNone;
  end;
  if (Result = ecNone) and (Code >= VK_ACCEPT) and (Code <= VK_SCROLL) then
  begin
    fLastKey := Code;
    fLastShiftState := Shift;
  end else begin
    fLastKey := 0;
    fLastShiftState := [];
  end;
end;

procedure TCustomSynEdit.CommandProcessor(Command: TSynEditorCommand;
  AChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
  Data: pointer);
begin
  {$IFDEF VerboseKeys}
  DebugLn('[TCustomSynEdit.CommandProcessor] ',Command
    ,' AChar=',AChar,' Data=',DbgS(Data));
  {$ENDIF}
  // first the program event handler gets a chance to process the command
  DoOnProcessCommand(Command, AChar, Data);
  if Command <> ecNone then begin
    // notify hooked command handlers before the command is executed inside of
    // the class
    NotifyHookedCommandHandlers(FALSE, Command, AChar, Data);
    // internal command handler
    if (Command <> ecNone) and (Command < ecUserFirst) then
      ExecuteCommand(Command, AChar, Data);
    // notify hooked command handlers after the command was executed inside of
    // the class
    {$IFDEF SYN_LAZARUS}
    if Command <> ecNone then
    {$ENDIF}
      NotifyHookedCommandHandlers(TRUE, Command, AChar, Data);
  end;
  {$IFDEF SYN_LAZARUS}
  if Command <> ecNone then
  {$ENDIF}
    DoOnCommandProcessed(Command, AChar, Data);
end;

procedure TCustomSynEdit.ExecuteCommand(Command: TSynEditorCommand;
  {$IFDEF SYN_LAZARUS}const AChar: TUTF8Char{$ELSE}AChar: Char{$ENDIF};
  Data: pointer);
const
  ALPHANUMERIC = DIGIT + ALPHA_UC + ALPHA_LC;
  SEL_MODE: array[ecNormalSelect..ecLineSelect] of TSynSelectionMode = (
    smNormal, smColumn, smLine);
var
  CX: Integer;
  Len: Integer;
  Temp: string;
  Temp2: string;
  Helper: string;
  SpaceCount1: Integer;
  SpaceCount2: Integer;
  BackCounter: Integer;
  StartOfBlock: TPoint;
  bChangeScroll: boolean;
  moveBkm: boolean;
  WP: TPoint;
  Caret: TPoint;
  CaretNew: TPoint;
  OldSelMode: TSynSelectionMode;
{$IFDEF SYN_MBCSSUPPORT}
  i: integer;
  s: string;
{$ENDIF}
  counter: Integer;
  InsDelta: integer;
  {$IFDEF SYN_LAZARUS}
  LogCounter: integer;
  LogCaretXY: TPoint;
  LogCaret: TPoint;
  LogSpacePos: integer;
  LastUndoItem:TSynEditUndoItem;
  {$ENDIF}

{begin}                                                                         //mh 2000-10-30
  procedure SetSelectedTextEmpty;
  begin
    if (fBlockBegin.Y < fBlockEnd.Y)
      or ((fBlockBegin.Y = fBlockEnd.Y) and (fBlockBegin.X < fBlockEnd.X))
    then
      fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,
        SelectionMode)
    else
      fUndoList.AddChange(crDeleteAfterCursor, fBlockEnd, fBlockBegin, SelText,
        SelectionMode);
    SetSelText('');
  end;
{end}
                                                                           //mh 2000-10-30
begin
  IncPaintLock;
  try
    case Command of
// horizontal caret movement or selection
      ecLeft, ecSelLeft:
        begin
          {$IFDEF SYN_LAZARUS}
          if (eoCaretSkipsSelection in Options2) and (Command=ecLeft)
          and SelAvail and (CompareCarets(LogicalCaretXY,BlockEnd)=0) then begin
            CaretXY:=LogicalToPhysicalPos(BlockBegin);
          end else
          {$ENDIF}
            MoveCaretHorz(-1, Command = ecSelLeft);
        end;
      ecRight, ecSelRight:
        begin
          {$IFDEF SYN_LAZARUS}
          if (eoCaretSkipsSelection in Options2) and (Command=ecRight)
          and SelAvail and (CompareCarets(LogicalCaretXY,BlockBegin)=0) then begin
            CaretXY:=LogicalToPhysicalPos(BlockEnd);
          end else
          {$ENDIF}
            MoveCaretHorz(1, Command = ecSelRight);
        end;
      ecPageLeft, ecSelPageLeft:
        begin
          MoveCaretHorz(-CharsInWindow, Command = ecSelPageLeft);
        end;
      ecPageRight, ecSelPageRight:
        begin
          MoveCaretHorz(CharsInWindow, Command = ecSelPageRight);
        end;
{begin}                                                                         //mh 2000-10-19
      ecLineStart, ecSelLineStart:
        DoHomeKey(Command=ecSelLineStart);
        {begin
          MoveCaretAndSelectionPhysical(CaretXY,Point(1, CaretY),
                                        Command = ecSelLineStart);
          fLastCaretX := fCaretX;
        end;}
      ecLineEnd, ecSelLineEnd:
        begin
          {$IFDEF SYN_LAZARUS}
          MoveCaretAndSelectionPhysical(CaretXY,
             LogicalToPhysicalPos(Point(1 + Length(LineText), CaretY)),
             Command = ecSelLineEnd);
          {$ELSE}
          MoveCaretAndSelection(CaretXY, Point(1 + Length(LineText), CaretY),
             Command = ecSelLineEnd);
          {$ENDIF}
          fLastCaretX := fCaretX;
        end;
{end}                                                                           //mh 2000-10-19
// vertical caret movement or selection
      ecUp, ecSelUp:
        begin
          MoveCaretVert(-1, Command = ecSelUp);
          {$IFNDEF SYN_LAZARUS}
          Update;
          {$ENDIF}
        end;
      ecDown, ecSelDown:
        begin
          MoveCaretVert(1, Command = ecSelDown);
          {$IFNDEF SYN_LAZARUS}
          Update;
          {$ENDIF}
        end;
      ecPageUp, ecSelPageUp, ecPageDown, ecSelPageDown:
        begin
          {$IFDEF SYN_LAZARUS}
          counter := fLinesInWindow;
          if (eoHalfPageScroll in fOptions) then counter:=counter shr 1;
          {$ELSE}
          counter := fLinesInWindow shr Ord(eoHalfPageScroll in fOptions);
          {$ENDIF}
          if eoScrollByOneLess in fOptions then
            Dec(counter);
          if (Command in [ecPageUp, ecSelPageUp]) then
            counter := -counter;
          TopLine := TopLine + counter;
          MoveCaretVert(counter, Command in [ecSelPageUp, ecSelPageDown]);
          Update;
        end;
      ecPageTop, ecSelPageTop:
        begin
          {$IFDEF SYN_LAZARUS}
          MoveCaretAndSelectionPhysical
          {$ELSE}
          MoveCaretAndSelection
          {$ENDIF}
            (CaretXY, Point(CaretX, TopLine), Command = ecSelPageTop);
          Update;
        end;
      ecPageBottom, ecSelPageBottom:
        begin
          CaretNew := Point(CaretX, TopLine + LinesInWindow - 1);
          {$IFDEF SYN_LAZARUS}
          MoveCaretAndSelectionPhysical
          {$ELSE}
          MoveCaretAndSelection
          {$ENDIF}
            (CaretXY, CaretNew, Command = ecSelPageBottom);
          Update;
        end;
      ecEditorTop, ecSelEditorTop:
        begin
          {$IFDEF SYN_LAZARUS}
          MoveCaretAndSelectionPhysical
          {$ELSE}
          MoveCaretAndSelection
          {$ENDIF}
            (CaretXY, Point(1, 1), Command = ecSelEditorTop);
          Update;
        end;
      ecEditorBottom, ecSelEditorBottom:
        begin
          CaretNew := Point(1, Lines.Count);
          if (CaretNew.Y > 0) then
            CaretNew.X := Length(Lines[CaretNew.Y - 1]) + 1;
          MoveCaretAndSelection(
            {$IFDEF SYN_LAZARUS}
            PhysicalToLogicalPos(CaretXY),
            {$ELSE}
            CaretXY,
            {$ENDIF}
            CaretNew, Command = ecSelEditorBottom);
          Update;
        end;
// goto special line / column position
      ecGotoXY, ecSelGotoXY:
        if Assigned(Data) then begin
          {$IFDEF SYN_LAZARUS}
          MoveCaretAndSelectionPhysical
          {$ELSE}
          MoveCaretAndSelection
          {$ENDIF}
            (CaretXY, PPoint(Data)^, Command = ecSelGotoXY);
          fLastCaretX := fCaretX;                                               //mh 2000-10-19
          Update;
        end;
// word selection
      ecWordLeft, ecSelWordLeft:
        begin
          Caret := CaretXY;
          CaretNew := PrevWordPos;
          {$IFDEF SYN_LAZARUS}
          MoveCaretAndSelectionPhysical
          {$ELSE}
          MoveCaretAndSelection
          {$ENDIF}
            (Caret, CaretNew, Command = ecSelWordLeft);
          fLastCaretX := fCaretX;                                               //mh 2000-10-19
          {$IFDEF SYN_LAZARUS}
          Update;
          {$ENDIF}
        end;
      ecWordRight, ecSelWordRight:
        begin
          Caret := CaretXY;
          CaretNew := NextWordPos;
          {$IFDEF SYN_LAZARUS}
          MoveCaretAndSelectionPhysical
          {$ELSE}
          MoveCaretAndSelection
          {$ENDIF}
            (Caret, CaretNew, Command = ecSelWordRight);
          fLastCaretX := fCaretX;                                               //mh 2000-10-19
          {$IFDEF SYN_LAZARUS}
          Update;
          {$ENDIF}
        end;
      ecSelectAll:
        begin
          SelectAll;
        end;
{begin}                                                                         //mh 2000-10-30
      ecDeleteLastChar:
        if not ReadOnly then begin
          if SelAvail then
            SetSelectedTextEmpty
          else begin
            Temp := LineText;
            Len := Length(Temp);
            {$IFDEF SYN_LAZARUS}
            LogCaretXY:=PhysicalToLogicalPos(CaretXY);
            LogCaret:=LogCaretXY;
            {$ENDIF}
            Caret := CaretXY;
            //debugln('ecDeleteLastChar B Temp="',DbgStr(Temp),'" CaretX=',dbgs(CaretX),' LogCaretXY=',dbgs(LogCaretXY));
            if {$IFDEF SYN_LAZARUS}LogCaretXY.X{$ELSE}CaretX{$ENDIF} > Len +1
            then begin
              // only move caret one column
              Helper := '';
              CaretX := CaretX - 1;
            end else if CaretX = 1 then begin
              // join this line with the last line if possible
              if CaretY > 1 then begin
                CaretY := CaretY - 1;
                {$IFDEF SYN_LAZARUS}
                CaretX := LogicalToPhysicalCol(Lines[CaretY - 1],
                                               Length(Lines[CaretY - 1]) + 1);
                {$ELSE}
                CaretX := Length(Lines[CaretY - 1]) + 1;
                {$ENDIF}
                Lines.Delete(CaretY);
                DoLinesDeleted(CaretY, 1);
                if eoTrimTrailingSpaces in Options then
                  Temp := TrimRight(Temp);
                LineText := LineText + Temp;
                Helper := {$IFDEF SYN_LAZARUS}LineEnding{$ELSE}#13#10{$ENDIF};
              end;
            end else begin
              // delete text before the caret
              SpaceCount1 := LeftSpaces(Temp{$IFDEF SYN_LAZARUS},true{$ENDIF});
              SpaceCount2 := 0;
              //debugln('ecDeleteLastChar C SpaceCount1=',dbgs(SpaceCount1),' Temp[LogCaretXY.X-1]=',DbgStr(Temp[LogCaretXY.X-1]));
              {$IFDEF SYN_LAZARUS}
              if (Temp[LogCaretXY.X-1] <= #32) and (SpaceCount1 = CaretX - 1) then
              {$ELSE}
              if (Temp[CaretX-1] <= #32) and (SpaceCount1 = CaretX - 1) then
              {$ENDIF}
              begin
                // unindent
                if SpaceCount1 > 0 then begin
                  BackCounter := CaretY - 2;
                  while BackCounter >= 0 do begin
                    SpaceCount2 :=LeftSpaces(Lines[BackCounter]
                                             {$IFDEF SYN_LAZARUS},true{$ENDIF});
                    if SpaceCount2 < SpaceCount1 then
                      break;
                    Dec(BackCounter);
                  end;
                end;
                if SpaceCount2 = SpaceCount1 then
                  SpaceCount2 := 0;
                {$IFDEF SYN_LAZARUS}
                // remove visible spaces
                LogSpacePos:=PhysicalToLogicalCol(Temp,SpaceCount2+1);
                Helper:=copy(Temp,LogSpacePos,LogCaretXY.X-LogSpacePos);
                //debugln('ecDeleteLastChar LogSpacePos=',dbgs(LogSpacePos),
                //   ' SpaceCount1=',dbgs(SpaceCount1),
                //   ' SpaceCount2=',dbgs(SpaceCount2),
                //   ' LogCaretXY.X=',dbgs(LogCaretXY.X),
                //   ' Temp="',DbgStr(Temp),'" Helper="',DbgStr(Helper),'"');
                Temp:=copy(Temp,1,LogSpacePos-1)+copy(Temp,LogCaretXY.X,MaxInt);
                TrimmedSetLine(CaretY - 1, Temp);
                fCaretX := LogicalToPhysicalCol(Temp,LogSpacePos);
                {$ELSE}
                Helper := Copy(Temp, 1, SpaceCount1 - SpaceCount2);
                Delete(Temp, 1, SpaceCount1 - SpaceCount2);
                TrimmedSetLine(CaretY - 1, Temp);
                fCaretX := fCaretX - (SpaceCount1 - SpaceCount2);
                {$ENDIF}
                fLastCaretX := fCaretX;
                StatusChanged([scCaretX]);
              end else begin
                // delete char
                counter := 1;
                {$IFDEF SYN_LAZARUS}
                  {$IFDEF USE_UTF8BIDI_LCL}
                  CaretX := CaretX - counter;
                  Helper := Copy(Temp, CaretX, counter);
                  VDelete(Temp, CaretX, counter, drLTR);
                  {$ELSE USE_UTF8BIDI_LCL}
                  LogCaretXY.X:=PhysicalToLogicalCol(Temp,CaretX-counter);
                  LogCounter:=GetCharLen(Temp,LogCaretXY.X);
                  CaretX := LogicalToPhysicalCol(Temp,LogCaretXY.X);
                  Helper := Copy(Temp, LogCaretXY.X, LogCounter);
                  System.Delete(Temp, LogCaretXY.X, LogCounter);
                  //debugln('ecDeleteLastChar delete char CaretX=',dbgs(CaretX),
                  //  ' Helper="',DbgStr(Helper),'" Temp="',DbgStr(Temp),'"');
                  {$ENDIF USE_UTF8BIDI_LCL}
                {$ELSE}
                  {$IFDEF SYN_MBCSSUPPORT}
                  if ByteType(Temp, CaretX - 2) = mbLeadByte then
                    Inc(counter);
                  {$ENDIF}
                  CaretX := CaretX - counter;
                  Helper := Copy(Temp, CaretX, counter);
                  Delete(Temp, CaretX, counter);
                {$ENDIF}
                TrimmedSetLine(CaretY - 1, Temp);
              end;
            end;
            
            if (Caret.X <> CaretX) or (Caret.Y <> CaretY) then begin
              {$IFDEF SYN_LAZARUS}
              if eoGroupUndo in Options then begin
                LastUndoItem := fUndoList.PeekItem;
                if (LastUndoItem <> nil)
                  and (LastUndoItem.fChangeReason = crSilentDelete)
                  and (LastUndoItem.fChangeStartPos.Y = LastUndoItem.fChangeEndPos.Y)
                  and (PhysicalToLogicalPos(CaretXY).Y = LogCaret.Y)
                  and (LastUndoItem.fChangeStartPos.X = LogCaret.X)
                then begin // Share the undo item with the delete char action before
                  LastUndoItem.fChangeStartPos.X := PhysicalToLogicalPos(CaretXY).X;
                  LastUndoItem.fChangeStr := Helper +  LastUndoItem.fChangeStr;
                end
                else
                begin
                  fUndoList.AddChange(crSilentDelete,
                  PhysicalToLogicalPos(CaretXY), LogCaret,
                  Helper, smNormal);
                end;
              end else begin
                //debugln('ecDeleteLastChar AddChange CaretXY=',dbgs(CaretXY),
                //  ' LogCaret=',dbgs(LogCaret),' Helper="',DbgStr(Helper),'" Temp="',DbgStr(Temp),'"');
                fUndoList.AddChange(crSilentDelete, CaretXY, Caret,
                  Helper, smNormal);
              end;
              {$ELSE}
              fUndoList.AddChange(crSilentDelete,CaretXY,Caret,Helper,smNormal);
              {$ENDIF}
            end;
          end;
        end;
      ecDeleteChar:
        if not ReadOnly then begin
          if SelAvail then
            SetSelectedTextEmpty
          else begin
            Temp := LineText;
            Len := Length(Temp);
            {$IFDEF SYN_LAZARUS}
            LogCaretXY:=PhysicalToLogicalPos(CaretXY);
            {$ENDIF}
            if {$IFDEF SYN_LAZARUS}LogCaretXY.X{$ELSE}CaretX{$ENDIF} <= Len then
            begin
              // delete char
              {$IFDEF SYN_LAZARUS}
                Counter:=GetCharLen(Temp,LogCaretXY.X);
                Helper := Copy(Temp, LogCaretXY.X, Counter);
                Caret.X := LogicalToPhysicalCol(Temp,LogCaretXY.X+Counter);
                Caret.Y := CaretY;
{$IFDEF USE_UTF8BIDI_LCL}
                VDelete(Temp, LogCaretXY.X, Counter, drLTR);
{$ELSE USE_UTF8BIDI_LCL}
                System.Delete(Temp, LogCaretXY.X, Counter);
{$ENDIF USE_UTF8BIDI_LCL}
                TrimmedSetLine(CaretY - 1, Temp);
              {$ELSE}
                counter := 1;
                {$IFDEF SYN_MBCSSUPPORT}
                if ByteType(Temp, CaretX) = mbLeadByte then
                  Inc(counter);
                {$ENDIF}
                Helper := Copy(Temp, CaretX, counter);
                Caret := Point(CaretX + counter, CaretY);
                Delete(Temp, CaretX, counter);
                TrimmedSetLine(CaretY - 1, Temp);
              {$ENDIF}
            end else begin
              // join line with the line after
              if CaretY < Lines.Count then begin
                Helper := StringOfChar(' ', CaretX - 1 - Len);
                TrimmedSetLine(CaretY - 1, Temp + Helper + Lines[CaretY]);
                Caret := Point(1, CaretY + 1);
                Helper := {$IFDEF SYN_LAZARUS}LineEnding{$ELSE}#13#10{$ENDIF};
                Lines.Delete(CaretY);
                DoLinesDeleted(CaretY - 1, 1);
              end;
            end;
            if (Caret.X <> CaretX) or (Caret.Y <> CaretY) then begin
              fUndoList.AddChange(crSilentDeleteAfterCursor,
                {$IFDEF SYN_LAZARUS}
                PhysicalToLogicalPos(CaretXY), PhysicalToLogicalPos(Caret),
                {$ELSE}
                CaretXY, Caret,
                {$ENDIF}
                Helper, smNormal);
            end;
          end;
        end;
      ecDeleteWord, ecDeleteEOL:
        if not ReadOnly then begin
          {$IFDEF SYN_LAZARUS}
          Len := LogicalToPhysicalCol(LineText,Length(LineText)+1)-1;
          {$ELSE}
          Len := Length(LineText);
          {$ENDIF}
          if Command = ecDeleteWord then begin
            if CaretX > Len + 1 then
              CaretX := Len + 1;
            {$IFDEF SYN_LAZARUS}
            WP := NextTokenPos;
            {$ELSE}
            WP := NextWordPos;
            {$ENDIF}
          end else
            WP := Point(Len + 1, CaretY);
          if (WP.X <> CaretX) or (WP.Y <> CaretY) then begin
            OldSelMode := fSelectionMode;
            try
              fSelectionMode := smNormal;
              SetBlockBegin({$IFDEF SYN_LAZARUS}PhysicalToLogicalPos(CaretXY)
                            {$ELSE}CaretXY{$ENDIF});
              SetBlockEnd({$IFDEF SYN_LAZARUS}PhysicalToLogicalPos(WP)
                            {$ELSE}WP{$ENDIF});
              fUndoList.AddChange(crSilentDeleteAfterCursor,
                {$IFDEF SYN_LAZARUS}
                PhysicalToLogicalPos(CaretXY), PhysicalToLogicalPos(WP),
                {$ELSE}
                CaretXY, WP,
                {$ENDIF}
                SelText, smNormal);
              SetSelText('');
            finally
              fSelectionMode := OldSelMode;
            end;
            CaretXY := CaretXY;
          end;
        end;
      ecDeleteLastWord, ecDeleteBOL:
        if not ReadOnly then begin
          if Command = ecDeleteLastWord then
            WP := PrevWordPos
          else
            WP := Point(1, CaretY);
          if (WP.X <> CaretX) or (WP.Y <> CaretY) then begin
            OldSelMode := fSelectionMode;
            try
              fSelectionMode := smNormal;
              {$IFDEF SYN_LAZARUS}
              SetBlockBegin(PhysicalToLogicalPos(CaretXY));
              SetBlockEnd(PhysicalToLogicalPos(WP));
              {$ELSE}
              SetBlockBegin(CaretXY);
              SetBlockEnd(WP);
              {$ENDIF}
              fUndoList.AddChange(crSilentDelete,
                {$IFDEF SYN_LAZARUS}
                PhysicalToLogicalPos(WP), PhysicalToLogicalPos(CaretXY),
                {$ELSE}
                WP, CaretXY,
                {$ENDIF}
                SelText, smNormal);
              SetSelText('');
            finally
              fSelectionMode := OldSelMode;
            end;
            CaretXY := WP;
          end;
        end;
{end}                                                                           //mh 2000-10-30
      ecDeleteLine:
        if not ReadOnly and not ((Lines.Count = 1) and (Length(Lines[0]) = 0))
        then begin
          if SelAvail then
            SetBlockBegin({$IFDEF SYN_LAZARUS}PhysicalToLogicalPos(CaretXY)
                          {$ELSE}CaretXY{$ENDIF});
          if Lines.Count = 1 then begin
            fUndoList.AddChange(crDeleteAfterCursor,
              {$IFDEF SYN_LAZARUS}
              PhysicalToLogicalPos(Point(1, CaretY)),
              PhysicalToLogicalPos(CaretXY),
              {$ELSE}
              Point(1, CaretY), CaretXY,
              {$ENDIF}
              LineText, smNormal);
            Lines[0] := '';
          end else begin
            fUndoList.AddChange(crDeleteAfterCursor,
              {$IFDEF SYN_LAZARUS}
              PhysicalToLogicalPos(Point(1, CaretY)),
              PhysicalToLogicalPos(CaretXY),
              LineText + LineEnding,
              {$ELSE}
              Point(1, CaretY), CaretXY,
              LineText + #13#10,
              {$ENDIF}
              smNormal);
            Lines.Delete(CaretY - 1);
          end;
          DoLinesDeleted(CaretY - 1, 1);
          CaretXY := Point(1, CaretY); // like seen in the Delphi editor
        end;
      ecClearAll:
        begin
          if not ReadOnly then ClearAll;
        end;
      ecInsertLine,
      ecLineBreak:
        if not ReadOnly then begin
          if SelAvail then begin
            fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,
              SelectionMode);
            SetSelText('');
          end;
          SpaceCount2 := 0;
          Temp := LineText;
          Temp2 := Temp; //LineText;
// This is sloppy, but the Right Thing would be to track the column of markers
// too, so they could be moved depending on whether they are after the caret...
          InsDelta := Ord(CaretX = 1);
          {$IFDEF SYN_LAZARUS}
          LogCaretXY:=PhysicalToLogicalPos(CaretXY);
          {$ENDIF}
          Len := Length(Temp);
          if Len > 0 then begin
            if Len >= LogCaretXY.X then begin
              if LogCaretXY.X > 1 then begin
                SpaceCount1 := LeftSpaces(Temp);
{begin}                                                                         //JGF 2000-09-23
                Temp := Copy(LineText, 1, LogCaretXY.X - 1);
                TrimmedSetLine(CaretY - 1, Temp);
                Delete(Temp2, 1, LogCaretXY.X - 1);
                fUndoList.AddChange(crLineBreak,
                  {$IFDEF SYN_LAZARUS}
                  LogCaretXY, LogCaretXY,
                  {$ELSE}
                  CaretXY, CaretXY,
                  {$ENDIF}
                  Temp2, smNormal);
                Lines.Insert(CaretY, StringOfChar(' ', SpaceCount1) + Temp2);
                if Command = ecLineBreak then
                  CaretXY := Point(SpaceCount1 + 1, CaretY + 1);
{end}                                                                           //JGF 2000-09-23
              end else begin
                Lines.Insert(CaretY - 1, '');
                fUndoList.AddChange(crLineBreak,
                  {$IFDEF SYN_LAZARUS}
                  LogCaretXY, LogCaretXY,
                  {$ELSE}
                  CaretXY, CaretXY,
                  {$ENDIF}
                  Temp2, smNormal);
                if Command = ecLineBreak then
                  CaretY := CaretY + 1;
              end;
            end else begin
{begin}                                                                         //mh 2000-10-06
(*
              BackCounter := CaretY - 1;
              while BackCounter >= 0 do begin
                SpaceCount2 := LeftSpaces(Lines[BackCounter]);
                if Length(Lines[BackCounter]) > 0 then break;
                dec(BackCounter);
              end;
              fUndoList.AddChange(crLineBreak, CaretXY, CaretXY, '', smNormal);
              if Command = ecLineBreak then
                CaretX := SpaceCount2 + 1;
              if (Command = ecInsertLine) or (eoScrollPastEol in fOptions) then
                Lines.Insert(CaretY, '');
              if Command = ecLineBreak then begin
                Inc(fCaretY);
                StatusChanged([scCaretY]);
              end;
              if (Command = ecLineBreak) and
                (fOptions * [eoAutoIndent, eoScrollPastEol] = [eoAutoIndent])
              then begin
                Lines.Insert(CaretY - 1, StringOfChar(' ', SpaceCount2));
                CaretX := SpaceCount2 + 1;
              end;
*)
              fUndoList.AddChange(crLineBreak,
                {$IFDEF SYN_LAZARUS}
                LogCaretXY, LogCaretXY,
                {$ELSE}
                CaretXY, CaretXY,
                {$ENDIF}
                '', smNormal);
              SpaceCount2 := 0;
              if eoAutoIndent in Options then begin
                BackCounter := CaretY;
                repeat
                  Dec(BackCounter);
                  Temp := Lines[BackCounter];
                  SpaceCount2 := LeftSpaces(Temp);
                until (BackCounter = 0) or (Temp <> '');
              end;
              Lines.Insert(CaretY, '');
              if Command = ecLineBreak then begin
                if SpaceCount2 > 0 then
                  Lines[CaretY] := StringOfChar(' ', SpaceCount2);
                CaretXY := Point(SpaceCount2 + 1, CaretY + 1);
              end;
{end}                                                                           //mh 2000-10-06
            end;
          end else begin
            if fLines.Count = 0 then
              fLines.Add('');
            BackCounter := CaretY - 1;
            while BackCounter >= 0 do begin
              SpaceCount2 := LeftSpaces(Lines[BackCounter]);
              if Length(Lines[BackCounter]) > 0 then break;
              dec(BackCounter);
            end;
            fUndoList.AddChange(crLineBreak,
              {$IFDEF SYN_LAZARUS}
              LogCaretXY, LogCaretXY,
              {$ELSE}
              CaretXY, CaretXY,
              {$ENDIF}
              '', smNormal);
            if Command = ecLineBreak then
              CaretX := SpaceCount2 + 1;
            Lines.Insert(CaretY - 1, '');
            if Command = ecLineBreak then
              CaretY := CaretY + 1;
          end;
          DoLinesInserted(CaretY - InsDelta, 1);
          EnsureCursorPosVisible;                                               //JGF 2000-09-23
          fLastCaretX := fCaretX;                                               //mh 2000-10-19
        end;
      ecTab:
        if not ReadOnly then DoTabKey;
      ecShiftTab:
        if not ReadOnly then
          {$IFDEF SYN_LAZARUS}
          if SelAvail and (eoTabIndent in Options) then
            DoBlockUnindent
          {$ENDIF};
      ecMatchBracket:
        FindMatchingBracket;
      ecChar:
        if not ReadOnly and (AChar >= #32) and (AChar <> #127) then begin
          if SelAvail then begin
            BeginUndoBlock;
            try
{begin}                                                                         //mh 2000-11-20
//            fUndoList.AddChange(crSelDelete, fBlockBegin, fBlockEnd, SelText,
              fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,
                SelectionMode);
              StartOfBlock := BlockBegin;
              if SelectionMode = smLine then
                StartOfBlock.X := 1;
              SetSelText(AChar);
              //debugln('ecChar SelAvail StartOfBlock=',dbgs(StartOfBlock),' fBlockEnd=',dbgs(fBlockEnd));
              fUndoList.AddChange(crInsert, StartOfBlock, fBlockEnd, '',
                smNormal);
            finally
              EndUndoBlock;
            end;
{end}                                                                           //mh 2000-11-20
          end else begin
            Temp := LineText;
// Added the check for whether or not we're in insert mode.
// If we are, we append one less space than we would in overwrite mode.
// This is because in overwrite mode we have to put in a final space
// character which will be overwritten with the typed character.  If we put the
// extra space in in insert mode, it would be left at the end of the line and
// cause problems unless eoTrimTrailingSpaces is set.
            {$IFDEF SYN_LAZARUS}
            LogCaretXY:=PhysicalToLogicalPos(CaretXY);
            {debugln('ecChar CaretXY=',dbgs(CaretXY),
                    ' LogCaretXY=',dbgs(PhysicalToLogicalPos(CaretXY)),
                    ' Adjusted LogCaretXY=',dbgs(LogCaretXY),
                    ' fInserting=',dbgs(fInserting),
                    ' Temp=',dbgstr(Temp),
                    '" UseUTF8=',dbgs(UseUTF8));}
            bChangeScroll := not (eoScrollPastEol in fOptions);
            try
              if bChangeScroll then Include(fOptions, eoScrollPastEol);
              StartOfBlock := LogCaretXY;
              if fInserting then begin
                // insert mode
                {$IFDEF USE_UTF8BIDI_LCL}
                // TODO: improve utf8bidi for tabs
                Len := VLength(Temp, drLTR);
                if Len < CaretX then begin
                  Temp := Temp + StringOfChar(' ', CaretX - Len);
                end;
                CaretX := InsertChar(aChar, Temp, CaretX, drLTR);
                {$ELSE}
                Len := Length(Temp);
                if Len < LogCaretXY.X then
                  Temp := Temp + StringOfChar(' ', LogCaretXY.X - Len);
                System.Insert(AChar, Temp, LogCaretXY.X);
                //debugln('ecChar Temp=',DbgStr(Temp),' AChar=',DbgStr(AChar));
                CaretX := CaretX + 1;
                {$ENDIF}
                TrimmedSetLine(CaretY - 1, Temp);
                fUndoList.AddChange(crInsert, StartOfBlock,
                  PhysicalToLogicalPos(CaretXY), '', smNormal);
              end else begin
                // overwrite mode
                Counter := GetCharLen(Temp,LogCaretXY.X);
                Helper := Copy(Temp, LogCaretXY.X, Counter);
                {$IFDEF USE_UTF8BIDI_LCL}
                CaretNew.X := CaretX;
                // TODO: improve utf8bidi for tabs
                //utf8bidi.insert(Temp,AChar,CaretNew.X);
                CaretX := CaretNew.X;
                {$ELSE}
                Len := Length(Temp);
                if LogCaretXY.X<=Len then
                  Temp:=copy(Temp,1,LogCaretXY.X-1)+AChar
                       +copy(Temp,LogCaretXY.X+Counter,length(Temp))
                else
                  Temp:=Temp+StringOfChar(' ', LogCaretXY.X-1-Len)+AChar;
                {$ENDIF}
                CaretNew := Point((CaretX + 1), CaretY);
                TrimmedSetLine(CaretY - 1, Temp);
                fUndoList.AddChange(crInsert,
                  StartOfBlock, PhysicalToLogicalPos(CaretNew),
                  Helper, smNormal);
                CaretX := CaretX + 1;
              end;
              if CaretX >= LeftChar + fCharsInWindow then
                LeftChar := LeftChar + Min(25, fCharsInWindow - 1);
            finally
              if bChangeScroll then Exclude(fOptions, eoScrollPastEol);
            end;
            {$ELSE below for NOT SYN_LAZARUS ----------------------------------}
            bChangeScroll := not (eoScrollPastEol in fOptions);
            try
              if bChangeScroll then Include(fOptions, eoScrollPastEol);
              StartOfBlock := CaretXY;
              if fInserting then begin
                Len := Length(Temp);
                if Len < CaretX then
//              Temp := Temp + StringOfChar(' ', CaretX - Len);
                  Temp := Temp + StringOfChar(' ', CaretX - Len - Ord(fInserting)); //JGF 2000-09-23
                System.Insert(AChar, Temp, CaretX);
                CaretX := CaretX + 1;
                TrimmedSetLine(CaretY - 1, Temp);                               //JGF 2000-09-23
                fUndoList.AddChange(crInsert, StartOfBlock,
                  CaretXY, '', smNormal);
              end else begin
// Processing of case character covers on LeadByte.
                counter := 1;
{$IFDEF SYN_MBCSSUPPORT}
                if (ByteType(Temp, CaretX) = mbLeadByte) then begin
                  Inc(counter);
                end;
{$ENDIF}
                Helper := Copy(Temp, CaretX, counter);
                Temp[CaretX] := AChar;
{$IFDEF SYN_MBCSSUPPORT}
                if (counter > 1) then begin
                  Temp[CaretX + 1] := ' ';
                end;
{$ENDIF}
                CaretNew := Point((CaretX + counter), CaretY);
                TrimmedSetLine(CaretY - 1, Temp);                               //JGF 2000-09-23
                fUndoList.AddChange(crInsert, StartOfBlock, CaretNew,
                  Helper, smNormal);
                CaretX := CaretX + 1;
              end;
              if CaretX >= LeftChar + fCharsInWindow then
                LeftChar := LeftChar + Min(25, fCharsInWindow - 1);
            finally
              if bChangeScroll then Exclude(fOptions, eoScrollPastEol);
            end;
            {$ENDIF}
          end;
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
      ecSetMarker0..ecSetMarker9:
        begin
          if BookMarkOptions.EnableKeys then begin
            CX := Command - ecSetMarker0;
            if assigned(fBookMarks[CX]) then begin
              moveBkm := (fBookMarks[CX].Line <> CaretY);
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
          TopLine := TopLine - 1;
          if CaretY > TopLine + LinesInWindow - 1 then
            CaretY := TopLine + LinesInWindow - 1;
          Update;
        end;
      ecScrollDown:
        begin
          TopLine := TopLine + 1;
          if CaretY < TopLine then
            CaretY := TopLine;
          Update;
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
      ecBlockIndent:
        if not ReadOnly then DoBlockIndent;
      ecBlockUnindent:
        if not ReadOnly then DoBlockUnindent;
      ecNormalSelect,
      ecColumnSelect,
      ecLineSelect:
        begin
          SelectionMode := SEL_MODE[Command];
        end;
{$IFDEF SYN_MBCSSUPPORT}
      ecImeStr:
        if not ReadOnly then begin
          SetString(s, PChar(Data), StrLen(Data));
          if SelAvail then begin
            fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, Helper,
              smNormal);
            StartOfBlock := fBlockBegin;
            SetSelText(s);
            fUndoList.AddChange(crInsert, fBlockBegin, fBlockEnd, Helper,
              smNormal);
          end else begin
            Temp := LineText;
            Len := Length(Temp);
            if Len < CaretX then
              Temp := Temp + StringOfChar(' ', CaretX - Len);
            bChangeScroll := not (eoScrollPastEol in fOptions);
            try
              if bChangeScroll then Include(fOptions, eoScrollPastEol);
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
              TrimmedSetLine(CaretY - 1, Temp);                                 //JGF 2000-09-23
              if fInserting then
                Helper := '';
              fUndoList.AddChange(crInsert, StartOfBlock,
                {$IFDEF SYN_LAZARUS}
                PhysicalToLogicalPos(CaretXY),
                {$ELSE}
                CaretXY,
                {$ENDIF}
                Helper, smNormal);
              if CaretX >= LeftChar + fCharsInWindow then
                LeftChar := LeftChar + min(25, fCharsInWindow - 1);
            finally
              if bChangeScroll then Exclude(fOptions, eoScrollPastEol);
            end;
          end;
        end;
{$ENDIF}
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.DoOnCommandProcessed(Command: TSynEditorCommand;
  AChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
  Data: pointer);
begin
  if Assigned(fOnCommandProcessed) then
    fOnCommandProcessed(Self, Command, AChar, Data);
end;

procedure TCustomSynEdit.DoOnProcessCommand(var Command: TSynEditorCommand;
  var AChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF}; Data: pointer);
begin
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
  {$IFDEF SYN_LAZARUS}
  BeginUndoBlock;
  SelectAll;
  SelText:='';
  EndUndoBlock;
  {$ELSE}
  Lines.Clear;
  {$ENDIF}
end;

procedure TCustomSynEdit.ClearSelection;
begin
  if SelAvail then
    SelText := '';
end;

{$IFDEF SYN_LAZARUS}
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
    if CY < Lines.Count then begin
      Line := Lines[CY];
      LineLen := Length(Line);
      Inc(CY);
      CX:=1;
      while (CX<=LineLen) and (Line[CX] in WhiteChars) do inc(CX);
      if CX>LineLen then CX:=1;
    end;
  end;

begin
  LogCaret:=PhysicalToLogicalPos(CaretXY);
  CX := LogCaret.X;
  CY := LogCaret.Y;
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then begin
    Line := Lines[CY - 1];
    LineLen := Length(Line);
    WhiteChars := [#9,' '];
    if CX > LineLen then begin
      FindFirstNonWhiteSpaceCharInNextLine;
    end else begin
      if fHighlighter<>nil then begin
        fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[CY - 1]);
        fHighlighter.SetLine(Line, CY - 1);
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
{$ENDIF}

function TCustomSynEdit.NextWordPos: TPoint;
var
  CX, CY, LineLen: integer;
  Line: string;
  CurIdentChars, WhiteChars: TSynIdentChars;
  {$IFDEF SYN_LAZARUS}
  LogCaret: TPoint;
  {$ENDIF}
begin
  {$IFDEF SYN_LAZARUS}
  LogCaret:=PhysicalToLogicalPos(CaretXY);
  CX := LogCaret.X;
  CY := LogCaret.Y;
  {$ELSE}
  CX := CaretX;
  CY := CaretY;
  {$ENDIF}
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then begin
    Line := Lines[CY - 1];
    CurIdentChars:=IdentChars;
    WhiteChars := [#1..#255] - CurIdentChars;
    LineLen := Length(Line);
    if CX >= LineLen then begin
      // find first IdentChar in the next line
      if CY < Lines.Count then begin
        Line := Lines[CY];
        Inc(CY);
        CX := Max(1, StrScanForCharInSet(Line, 1, CurIdentChars));
      end;
    end else begin
      // find first "whitespace" if next char is an IdentChar
      if Line[CX] in CurIdentChars then
        CX := StrScanForCharInSet(Line, CX, WhiteChars);
      // if "whitespace" found find the first IdentChar behind
      if CX > 0 then
        CX := StrScanForCharInSet(Line, CX, CurIdentChars);
      // if one of those failed just position at the end of the line
      if CX = 0 then
        CX := LineLen + 1;
    end;
  end;
  {$IFDEF SYN_LAZARUS}
  Result := LogicalToPhysicalPos(Point(CX, CY));
  {$ELSE}
  Result := Point(CX, CY);
  {$ENDIF}
end;

function TCustomSynEdit.PrevWordPos: TPoint;
var
  CX, CY: integer;
  Line: string;
  CurIdentChars, WhiteChars: TSynIdentChars;
  {$IFDEF SYN_LAZARUS}
  LogCaret: TPoint;
  {$ENDIF}
begin
  {$IFDEF SYN_LAZARUS}
  LogCaret:=PhysicalToLogicalPos(CaretXY);
  CX := LogCaret.X;
  CY := LogCaret.Y;
  {$ELSE}
  CX := CaretX;
  CY := CaretY;
  {$ENDIF}
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);
    CurIdentChars:=IdentChars;
    WhiteChars := [#1..#255] - CurIdentChars;
    if CX <= 1 then begin
      // find last IdentChar in the previous line
      if CY > 1 then begin
        Dec(CY);
        Line := Lines[CY - 1];
        CX := Length(Line) + 1;
      end;
    end else begin
      // if previous char is a "whitespace" search for the last IdentChar
      if Line[CX - 1] in WhiteChars then
        CX := StrRScanForCharInSet(Line, CX - 1, CurIdentChars);
      if CX > 0 then
        // search for the first IdentChar of this "word"
        CX := StrRScanForCharInSet(Line, CX - 1, WhiteChars) + 1
      else
        // just position at the end of the previous line
        if CY > 1 then begin
          Dec(CY);
          Line := Lines[CY - 1];
          CX := Length(Line) + 1;
        end;
    end;
  end;
  {$IFDEF SYN_LAZARUS}
  Result := LogicalToPhysicalPos(Point(CX, CY));
  {$ELSE}
  Result := Point(CX, CY);
  {$ENDIF}
end;

procedure TCustomSynEdit.SetSelectionMode(const Value: TSynSelectionMode);
begin
  if FSelectionMode <> Value then begin
    FSelectionMode := Value;
    if SelAvail then
      Invalidate;
    StatusChanged([scSelection]);
  end;
end;

{begin}                                                                         //sbs 2000-11-19
procedure TCustomSynEdit.BeginUndoBlock;
begin
  fUndoList.BeginBlock;
end;
{end}                                                                           //sbs 2000-11-19

procedure TCustomSynEdit.BeginUpdate;
begin
  IncPaintLock;
end;

{begin}                                                                         //sbs 2000-11-19
procedure TCustomSynEdit.EndUndoBlock;
begin
  fUndoList.EndBlock;
end;
{end}                                                                           //sbs 2000-11-19

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

{ Called by FMarkList if change }
procedure TCustomSynEdit.MarkListChange(Sender: TObject);
begin
  {$IFDEF SYN_LAZARUS}
  Invalidate; // marks can have special line colors
  {$ELSE}
  InvalidateGutter;
  {$ENDIF}
end;

{$IFDEF SYN_LAZARUS}
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
  while (loop < (p.Y - 1)) and (loop < Lines.Count) do
  begin
    result := result + llen(lines[loop]);
    inc(loop);
  end;
  if loop < Lines.Count then
    result := result + Min(p.X, length(lines[loop]) + 1);
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
  while (loop < Lines.Count) and (count + llen(lines[loop]) < value) do begin
    count := count + llen(lines[loop]);
    inc(loop);
  end;
{  CaretX := value - count;
  CaretY := loop + 1;

  fBlockBegin.X := CaretX;
  fBlockBegin.Y := CaretY;}

  //This seems the same as above, but uses the other fixes inside of SetCaretXY
  //to adjust the cursor pos correctly.
  BlockBegin := Point(value - count, loop + 1);
  CaretXY := LogicalToPhysicalPos(BlockBegin);
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
  while (loop < (p.y - 1)) and (loop < Lines.Count) do begin
    Result := result + llen(lines[loop]);
    inc(loop);
  end;
  if loop<Lines.Count then
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
  while (loop < Lines.Count) and (count + llen(lines[loop]) < value) do begin
    count := count + llen(lines.strings[loop]);
    inc(loop);
  end;
  p.x := value - count; p.y := loop + 1;
  BlockEnd := p;
end;
{$ENDIF}

procedure TCustomSynEdit.SetSelWord;
begin
  {$IFDEF SYN_LAZARUS}
  SetWordBlock(PhysicalToLogicalPos(CaretXY));
  {$ELSE}
  SetWordBlock(CaretXY);
  {$ENDIF}
end;

procedure TCustomSynEdit.SetExtraLineSpacing(const Value: integer);
begin
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

procedure TCustomSynEdit.ClearUndo;
begin
  fUndoList.Clear;
  fRedoList.Clear;
end;

procedure TCustomSynEdit.SetSelTextExternal(const Value: string);
var
  StartOfBlock, EndOfBlock: TPoint;
begin
{begin}                                                                         //mh 2000-11-20
  BeginUndoBlock;
  try
    {$IFDEF SYN_LAZARUS}
    if SelAvail then begin
      fUndoList.AddChange({crSelDelete} crDelete, fBlockBegin, fBlockEnd,
        GetSelText, SelectionMode);
      StartOfBlock := minPoint(fBlockBegin, fBlockEnd);
      EndOfBlock := maxPoint(fBlockBegin, fBlockEnd);
    end else begin
      StartOfBlock := CaretXY;
      EndOfBlock := CaretXY;
    end;
    {$ELSE}
    if SelAvail then begin
      fUndoList.AddChange({crSelDelete} crDelete, fBlockBegin, fBlockEnd,
        GetSelText, SelectionMode);
    end;
    StartOfBlock := minPoint(fBlockBegin, fBlockEnd);
    EndOfBlock := maxPoint(fBlockBegin, fBlockEnd);
    {$ENDIF}
    fBlockBegin := StartOfBlock;
    fBlockEnd := EndOfBlock;
    LockUndo;
    SetSelText(Value);
    UnlockUndo;
    fUndoList.AddChange(crInsert, StartOfBlock, BlockEnd,
      {$IFDEF SYN_LAZARUS}''{$ELSE}SelText{$ENDIF}, smNormal);
  finally
    EndUndoBlock;
  end;
{end}                                                                           //mh 2000-11-20
end;

procedure TCustomSynEdit.SetGutter(const Value: TSynGutter);
begin
  fGutter.Assign(Value);
end;

procedure TCustomSynEdit.GutterChanged(Sender: TObject);
var
  nW: integer;
begin
  if not (csLoading in ComponentState) then begin
    if fGutter.ShowLineNumbers and fGutter.AutoSize then
      fGutter.AutoSizeDigitCount(Lines.Count);
    nW := fGutter.RealGutterWidth(fCharWidth);
    if nW = fGutterWidth then
      InvalidateGutter
    else
      SetGutterWidth(nW);
  end;
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

{$IFNDEF SYN_LAZARUS}

procedure TCustomSynEdit.WMMouseWheel(var Msg: TMessage);
var
  nDelta: integer;
  nWheelClicks: integer;
{$IFNDEF SYN_COMPILER_4_UP}
const
  LinesToScroll = 3;
  WHEEL_DELTA = 120;
  WHEEL_PAGESCROLL = MAXDWORD;
{$ENDIF}
begin
  if csDesigning in ComponentState then
    exit;

  if GetKeyState(VK_CONTROL) >= 0 then
{$IFDEF SYN_COMPILER_4_UP}
    nDelta := Mouse.WheelScrollLines
{$ELSE}
    nDelta := LinesToScroll
{$ENDIF}
  else begin
    {$IFDEF SYN_LAZARUS}
    nDelta := fLinesInWindow;
    if (eoHalfPageScroll in fOptions) then counter:=counter shr 1;
    {$ELSE}
    nDelta := fLinesInWindow shr Ord(eoHalfPageScroll in fOptions);
    {$ENDIF}
  end;

  Inc(fMouseWheelAccumulator, SmallInt(Msg.wParamHi));
  nWheelClicks := fMouseWheelAccumulator div WHEEL_DELTA;
  fMouseWheelAccumulator := fMouseWheelAccumulator mod WHEEL_DELTA;
  if (nDelta = integer(WHEEL_PAGESCROLL)) or (nDelta > LinesInWindow) then
    nDelta := LinesInWindow;
  TopLine := TopLine - (nDelta * nWheelClicks);
  Update;
end;

{$ENDIF}

procedure TCustomSynEdit.SetWantTabs(const Value: boolean);
begin
  fWantTabs := Value;
end;

procedure TCustomSynEdit.SetTabWidth(Value: integer);
begin
  Value := MinMax(Value, 1{0}, 256);                                            //lt 2000-10-19
  if (Value <> fTabWidth) then begin
    fTabWidth := Value;
    TSynEditStringList(Lines).TabWidth := Value;                                //mh 2000-10-19
    Invalidate; // to redraw text containing tab chars
  end;
end;

procedure TCustomSynEdit.SelectedColorsChanged(Sender: TObject);
begin
  Invalidate;
end;

// find / replace

function TCustomSynEdit.SearchReplace(const ASearch, AReplace: string;
  AOptions: TSynSearchOptions): integer;
var
  ptStart, ptEnd: TPoint; // start and end of the search range
  ptCurrent: TPoint; // current search position
  nFound: integer;
  bBackward, bFromCursor: boolean;
  bPrompt: boolean;
  bReplace, bReplaceAll: boolean;
  nAction: TSynReplaceAction;
  {$IFDEF SYN_LAZARUS}
  CurReplace: string;
  ptFoundStart, ptFoundEnd: TPoint;
  {$ELSE}
  n, nSearchLen, nReplaceLen, nInLine: integer;
  {$ENDIF}

  function InValidSearchRange(First, Last: integer): boolean;
  begin
    Result := TRUE;
    case fSelectionMode of
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
    if (fSelectionMode = smLine) then begin
      ptStart.X := 1;
      ptEnd.X := Length(Lines[ptEnd.Y - 1]) + 1;
    end else if (fSelectionMode = smColumn) then
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
    ptEnd.Y := Lines.Count;
    ptEnd.X := Length(Lines[ptEnd.Y - 1]) + 1;
    if bFromCursor then
      if bBackward then
        ptEnd := {$IFDEF SYN_LAZARUS}LogicalCaretXY{$ELSE}CaretXY{$ENDIF}
      else
        ptStart := {$IFDEF SYN_LAZARUS}LogicalCaretXY{$ELSE}CaretXY{$ENDIF};
    if bBackward then ptCurrent := ptEnd else ptCurrent := ptStart;
  end;
  // initialize the search engine
  fTSearch.Sensitive := ssoMatchCase in AOptions;
  fTSearch.Whole := ssoWholeWord in AOptions;
  fTSearch.Pattern := ASearch;
  fTSearch.RegularExpressions := ssoRegExpr in AOptions;
  {$IFDEF SYN_LAZARUS}
  fTSearch.RegExprMultiLine := ssoRegExprMultiLine in AOptions;
  fTSearch.Replacement:=AReplace;
  fTSearch.Backwards:=bBackward;
  {$ELSE}
  nSearchLen := Length(ASearch);
  nReplaceLen := Length(AReplace);
  {$ENDIF}
  // search while the current search position is inside of the search range
  if bReplaceAll then IncPaintLock;
  try
    {$IFDEF SYN_LAZARUS}
    //DebugLn(['TCustomSynEdit.SearchReplace ptStart=',dbgs(ptStart),' ptEnd=',dbgs(ptEnd),' ASearch="',dbgstr(ASearch),'" AReplace="',dbgstr(AReplace),'"']);
    while fTSearch.FindNextOne(Lines,ptStart,ptEnd,ptFoundStart,ptFoundEnd) do
    begin
      //DebugLn(['TCustomSynEdit.SearchReplace FOUND ptStart=',dbgs(ptStart),' ptEnd=',dbgs(ptEnd),' ptFoundStart=',dbgs(ptFoundStart),' ptFoundEnd=',dbgs(ptFoundEnd)]);
      // check if found place is entirely in range
      if (fSelectionMode<>smColumn)
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
          nAction := DoOnReplaceText(ASearch,CurReplace,
                                     ptFoundStart.Y,ptFoundStart.X);
          if nAction = raCancel then exit;
        end else
          nAction := raReplace;
        if not (nAction = raSkip) then begin
          // user has been prompted and has requested to silently replace all
          // so turn off prompting
          if nAction = raReplaceAll then begin
            if not bReplaceAll then begin
              bReplaceAll := TRUE;
              IncPaintLock;
            end;
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
    {$ELSE}
    while (ptCurrent.Y >= ptStart.Y) and (ptCurrent.Y <= ptEnd.Y) do begin
      nInLine := fTSearch.FindAll(Lines[ptCurrent.Y - 1]);
      if bBackward then n := Pred(fTSearch.ResultCount) else n := 0;
      // Operate on all results in this line.
      while nInLine > 0 do begin
        nFound := fTSearch.Results[n];
        if bBackward then Dec(n) else Inc(n);
        Dec(nInLine);
        // Is the search result entirely in the search range?
        if not InValidSearchRange(nFound, nFound + nSearchLen) then continue;
        Inc(Result);
        // Select the text, so the user can see it in the OnReplaceText event
        // handler or as the search result.
        ptCurrent.X := nFound;
        BlockBegin := ptCurrent;
        if bBackward then CaretXY := ptCurrent;
        Inc(ptCurrent.X, nSearchLen);
        BlockEnd := ptCurrent;
        if not bBackward then CaretXY := ptCurrent;
        // If it's a search only we can leave the procedure now.
        if not (bReplace or bReplaceAll) then exit;
        // Prompt and replace or replace all.  If user chooses to replace
        // all after prompting, turn off prompting.
        if bPrompt and Assigned(fOnReplaceText) then begin
          EnsureCursorPosVisible;
          nAction := DoOnReplaceText(ASearch,AReplace,ptCurrent.Y, nFound);
          if nAction = raCancel then exit;
        end else
          nAction := raReplace;
        if not (nAction = raSkip) then begin
          // user has been prompted and has requested to silently replace all
          // so turn off prompting
          if nAction = raReplaceAll then begin
            if not bReplaceAll then begin
              bReplaceAll := TRUE;
              IncPaintLock;
            end;
            bPrompt := False;
          end;
          SetSelTextExternal(AReplace);
        end;
        // fix the caret position and the remaining results
        if not bBackward then begin
          CaretX := nFound + nReplaceLen;
          if (nSearchLen <> nReplaceLen) and (nAction <> raSkip) then
            fTSearch.FixResults(nFound, nSearchLen - nReplaceLen);
        end;
        if not bReplaceAll then
          exit;
      end;
      // search next / previous line
      if bBackward then Dec(ptCurrent.Y) else Inc(ptCurrent.Y);
    end;
    {$ENDIF}
  finally
    if bReplaceAll then DecPaintLock;
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
    if SelectionMode = smLine then
      Result := TRUE
    else if (SelectionMode = smColumn) then begin
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

{$IFNDEF SYN_LAZARUS}
{ LCL never sends WM_SETCURSOR messages, use OnMouseMove and then set cursor }

procedure TCustomSynEdit.WMSetCursor(var Msg: TWMSetCursor);
var
  ptCursor, ptLineCol: TPoint;
begin
  GetCursorPos(ptCursor);
  ptCursor := ScreenToClient(ptCursor);
  if (ptCursor.X < fGutterWidth) then
    // ToDo TStreenCursors
    SetCursor(Screen.Cursors[fGutter.Cursor])
  else begin
    ptLineCol.X := (LeftChar * fCharWidth + ptCursor.X - fGutterWidth - 2)
      div fCharWidth;
    ptLineCol.Y := TopLine + ptCursor.Y div fTextHeight;
    if (eoDragDropEditing in fOptions) and IsPointInSelection(ptLineCol) then
      // ToDo TStreenCursors
      SetCursor(Screen.Cursors[crDefault])
    else
      // ToDo WMSetCursor
      inherited WMSetCursor(Msg);
  end;
end;

{$endif}

procedure TCustomSynEdit.BookMarkOptionsChanged(Sender: TObject);
begin
  InvalidateGutter;
end;

procedure TCustomSynEdit.SetOptions(Value: TSynEditorOptions);
var
  bSetDrag: boolean;
  {$IFDEF SYN_LAZARUS}
  OldOptions: TSynEditorOptions;
  {$ENDIF}
begin
  if (Value <> fOptions) then begin
    OldOptions := fOptions;
    bSetDrag := (eoDropFiles in fOptions) <> (eoDropFiles in Value);
    fOptions := Value;
    // Reset column position in case Cursor is past EOL.
    if not (eoScrollPastEol in fOptions) then
      CaretX := CaretX;
    // (un)register HWND as drop target
    if bSetDrag and not (csDesigning in ComponentState) and HandleAllocated then
      {$IFDEF SYN_LAZARUS}
      // ToDo DragAcceptFiles
      ;
      {$ELSE}
      DragAcceptFiles(Handle, (eoDropFiles in fOptions));
      {$ENDIF}
    {$IFDEF SYN_LAZARUS}
    if ((eoPersistentCaret in fOptions) xor (eoPersistentCaret in OldOptions))
    and HandleAllocated then begin
      SetCaretRespondToFocus(Handle,not (eoPersistentCaret in fOptions));
      UpdateCaret;
    end;
    if ((eoShowCtrlMouseLinks in fOptions)
    xor (eoShowCtrlMouseLinks in OldOptions))
    and HandleAllocated then
      UpdateCtrlMouse;
    {$ENDIF}
  end;
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.SetOptions2(const Value: TSynEditorOptions2);
begin
  if (Value <> fOptions2) then begin
    fOptions2 := Value;
    // Reset column position in case Cursor is past EOL.
    if not (eoScrollPastEol in fOptions) then
      CaretX := CaretX;
  end;
end;
{$ENDIF}

procedure TCustomSynEdit.SetOptionFlag(Flag: TSynEditorOption; Value: boolean);
begin
  if (Value <> (Flag in fOptions)) then begin
    if Value then Include(fOptions, Flag) else Exclude(fOptions, Flag);
    if (Flag = eoScrollPastEol) and not Value then
      CaretX := CaretX;
{begin}                                                                         //mh 2000-10-19
    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
{end}                                                                           //mh 2000-10-19
    if (Flag = eoDropFiles) then begin
      if not (csDesigning in ComponentState) and HandleAllocated then
        {$IFDEF SYN_LAZARUS}
        // ToDo DragAcceptFiles
        ;
        {$ELSE}
        DragAcceptFiles(Handle, Value);
        {$ENDIF}
    end;
    {$IFDEF SYN_LAZARUS}
    if (Flag = eoPersistentCaret) and HandleAllocated then
      SetCaretRespondToFocus(Handle,not (eoPersistentCaret in fOptions));
    {$ENDIF}
    EnsureCursorPosVisible;
  end;
end;

procedure TCustomSynEdit.SizeOrFontChanged(bFont: boolean);
begin
  if HandleAllocated then begin
    {$IFDEF SYN_LAZARUS}
    LastMouseCaret:=Point(-1,-1);
    fCharsInWindow := Max(1,(ClientWidth - fGutterWidth - 2 - ScrollBarWidth)
                            div fCharWidth);
    fLinesInWindow := Max(0,ClientHeight - ScrollBarWidth) div Max(1,fTextHeight);
    //DebugLn('TCustomSynEdit.SizeOrFontChanged fLinesInWindow=',dbgs(fLinesInWindow),' ClientHeight=',dbgs(ClientHeight),' ',dbgs(fTextHeight));
    //debugln('TCustomSynEdit.SizeOrFontChanged A ClientWidth=',dbgs(ClientWidth),' fGutterWidth=',dbgs(fGutterWidth),' ScrollBarWidth=',dbgs(ScrollBarWidth),' fCharWidth=',dbgs(fCharWidth),' fCharsInWindow=',dbgs(fCharsInWindow),' Width=',dbgs(Width));
    {$ELSE}
    fCharsInWindow := Max(1,Max(0,(ClientWidth - fGutterWidth - 2
                                   - ScrollBarWidth) div Max(1,fCharWidth)));
    fLinesInWindow := ClientHeight div fTextHeight;
    {$ENDIF}
    if bFont then begin
      if Gutter.ShowLineNumbers then
        GutterChanged(Self)
      else
        UpdateScrollbars;
      InitializeCaret;
      Exclude(fStateFlags, sfCaretChanged);
      Invalidate;
    end else
      UpdateScrollbars;
    Exclude(fStateFlags, sfScrollbarChanged);
{begin}                                                                         //mh 2000-10-19
    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    if not (eoScrollPastEof in Options) then
      TopLine := TopLine;
{end}                                                                           //mh 2000-10-19
  end;
end;

procedure TCustomSynEdit.MoveCaretHorz(DX: integer; SelectionCommand: boolean);
{$IFDEF SYN_LAZARUS}
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
      dec(NewCaret.Y);
      s:=Lines[NewCaret.Y-1];
      PhysicalLineLen:=LogicalToPhysicalPos(Point(length(s)+1,NewCaret.Y)).X-1;
      NewCaret.X:=PhysicalLineLen+1;
    end;
  end else if not (eoScrollPastEol in fOptions) then begin
    s:=LineText;
    PhysicalLineLen:=LogicalToPhysicalPos(Point(length(s)+1,CaretY)).X-1;
    if NewCaret.X>PhysicalLineLen+1 then begin
      // move to start of next line
      NewCaret.X:=1;
      inc(NewCaret.Y);
    end;
  end;

  // adjust selection
  IncPaintLock;
  if SelectionCommand then begin
    //debugln('TCustomSynEdit.MoveCaretHorz A CaretXY=',dbgs(CaretXY),' NewCaret=',dbgs(NewCaret));
    if not SelAvail then SetBlockBegin(PhysicalToLogicalPos(CaretXY));
    SetBlockEnd(PhysicalToLogicalPos(NewCaret));
    //debugln('TCustomSynEdit.MoveCaretHorz B BB=',dbgs(BlockBegin),' BE=',dbgs(BlockEnd));
    AquirePrimarySelection;
  end else
    SetBlockBegin(PhysicalToLogicalPos(NewCaret));
  // commit new caret
  CaretXY := NewCaret;
  DecPaintLock;
end;
{$ELSE}
var
  ptO, ptDst: TPoint;
  s: string;
  nLineLen: integer;
  bChangeY: boolean;
begin
  ptO := CaretXY;
  ptDst := ptO;
  s := LineText;
  nLineLen := Length(s);
  // only moving or selecting one char can change the line
  bChangeY := not (eoScrollPastEol in fOptions);
  if bChangeY and (DX = -1) and (ptO.X = 1) and (ptO.Y > 1) then begin
    // end of previous line
    Dec(ptDst.Y);
    ptDst.X := Length(Lines[ptDst.Y - 1]) + 1;
  end else
    if bChangeY and (DX = 1) and (ptO.X > nLineLen) and (ptO.Y < Lines.Count)
    then begin
      // start of next line
      Inc(ptDst.Y);
      ptDst.X := 1;
    end else begin
      ptDst.X := Max(1, ptDst.X + DX);
      // don't go past last char when ScrollPastEol option not set
      if (DX > 0) and bChangeY then ptDst.X := Min(ptDst.X, nLineLen + 1);
{$IFDEF SYN_MBCSSUPPORT}
      // prevent from getting inside of a doublebyte char
      if (ptDst.X > 1) and (ptDst.X <= nLineLen) then begin
        DX := ptDst.X - ptO.X;
        if (DX < 0) then begin
          if ByteType(s, ptDst.X) = mbTrailByte then Dec(ptDst.X);
        end else if (DX > 0) then begin
          if ByteType(s, ptDst.X) = mbTrailByte then Inc(ptDst.X);
        end;
      end;
    end;
    fMBCSStepAside := False;
{$ELSE}
    end;
{$ENDIF}
  // set caret and block begin / end
  MoveCaretAndSelection(ptO, ptDst, SelectionCommand);
  fLastCaretX := fCaretX;                                                       //mh 2000-10-19
end;
{$ENDIF}

procedure TCustomSynEdit.MoveCaretVert(DY: integer; SelectionCommand: boolean);
{$IFDEF SYN_LAZARUS}
// moves Caret vertical DY unfolded lines
var
  NewCaret: TPoint;
  LogCaret: TPoint;
  OldCaret: TPoint;
  SaveLastCaretX: LongInt;
  i: Integer;
begin
  OldCaret:=CaretXY;
  NewCaret:=OldCaret;
  with NewCaret do begin
    if DY>=0 then begin
      for i:=1 to DY do begin
        Inc(Y);
        Y:=FindNextUnfoldedLine(Y,true);
      end;
    end else begin
      for i:=1 to -DY do begin
        dec(Y);
        Y:=FindNextUnfoldedLine(Y,false);
      end;
    end;
    if DY >= 0 then begin
      if (Y > Lines.Count) or (CaretY > Y) then
        Y := Lines.Count;
    end else
      if (Y < 1) or (CaretY < Y) then
        Y := 1;
  end;
  if (OldCaret.Y<>NewCaret.Y) and (fLastCaretX>0) and (eoKeepCaretX in Options)
  then
    NewCaret.X:=fLastCaretX;
  // set caret and block begin / end
  LogCaret:=PhysicalToLogicalPos(NewCaret);
  IncPaintLock;
  if SelectionCommand then begin
    if not SelAvail then SetBlockBegin(PhysicalToLogicalPos(CaretXY));
    SetBlockEnd(LogCaret);
    AquirePrimarySelection;
  end else
    SetBlockBegin(LogCaret);
  SaveLastCaretX:=fLastCaretX;
  CaretXY:=NewCaret;
  fLastCaretX:=SaveLastCaretX;
  DecPaintLock;
end;
{$ELSE below for NOT SYN_LAZARUS ----------------------------------------------}
var
  ptO, ptDst: TPoint;
{$IFDEF SYN_MBCSSUPPORT}
  NewStepAside: Boolean;
  s: string;
{$ENDIF}
  SaveLastCaretX: Integer;
begin
  ptO := CaretXY;                                                               // sblbg 2001-12-17
  ptDst := ptO;
  with ptDst do begin
    Inc(Y, DY);
    if DY >= 0 then begin
      if (Y > Lines.Count) or (ptO.Y > Y) then
        Y := Lines.Count;
    end else
      if (Y < 1) or (ptO.Y < Y) then
        Y := 1;
  end;
  if (ptO.Y <> ptDst.Y) then begin
    if eoKeepCaretX in Options then                                             //mh 2000-10-19
      ptDst.X := fLastCaretX;                                                   //mh 2000-10-19
  end;

  ptDst := PhysicalToLogicalPos(ptDst);                                         // sblbg 2001-12-17
  ptO := PhysicalToLogicalPos(ptO);                                             // sblbg 2001-12-17

{$IFDEF SYN_MBCSSUPPORT}
  if (ptO.Y <> ptDst.Y) then begin
    if fMBCSStepAside and not (eoKeepCaretX in Options) then
      Inc(ptDst.X);
    NewStepAside := False;
    s := Lines[ptDst.Y - 1];
    if (ptDst.X <= Length(s)) then
      if (ByteType(s, ptDst.X) = mbTrailByte) then begin
        NewStepAside := True;
        Dec(ptDst.X);
      end;
  end
  else
    NewStepAside := fMBCSStepAside;
{$ENDIF}
  SaveLastCaretX := fLastCaretX;

  // set caret and block begin / end
  MoveCaretAndSelection(ptO, ptDst, SelectionCommand);

  // Set fMBCSStepAside and restore fLastCaretX after moving caret, since
  // UpdateLastCaretX, called by SetCaretXYEx, changes them. This is the one
  // case where we don't want that.
{$IFDEF SYN_MBCSSUPPORT}
  fMBCSStepAside := NewStepAside;
{$ENDIF}
  fLastCaretX := SaveLastCaretX;                                                //jr 2002-04-26
end;
{$ENDIF not SYN_LAZARUS}

procedure TCustomSynEdit.MoveCaretAndSelection(
  {$IFDEF SYN_LAZARUS}const {$ENDIF}ptBefore, ptAfter: TPoint;
  SelectionCommand: boolean);
// ptBefore and ptAfter are logical (byte)
begin
  IncPaintLock;
  if SelectionCommand then begin
    if not SelAvail then SetBlockBegin(ptBefore);
    SetBlockEnd(ptAfter);
    {$IFDEF SYN_LAZARUS}
    AquirePrimarySelection;
    {$ENDIF}
  end else
    SetBlockBegin(ptAfter);
  CaretXY := {$IFDEF SYN_LAZARUS}LogicalToPhysicalPos(ptAfter)
             {$ELSE}ptAfter{$ENDIF};
  DecPaintLock;
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.MoveCaretAndSelectionPhysical(const ptBeforePhysical,
  ptAfterPhysical: TPoint; SelectionCommand: boolean);
begin
  MoveCaretAndSelection(PhysicalToLogicalPos(ptBeforePhysical),
                        PhysicalToLogicalPos(ptAfterPhysical),
                        SelectionCommand);
end;
{$ENDIF}

procedure TCustomSynEdit.SetCaretAndSelection(
  {$IFDEF SYN_LAZARUS}const {$ENDIF}ptCaret, ptBefore, ptAfter: TPoint);
// caret is physical (screen)
// Before, After is logical (byte)
begin
  IncPaintLock;
  CaretXY := ptCaret;
  SetBlockBegin(ptBefore);
  SetBlockEnd(ptAfter);
  {$IFDEF SYN_LAZARUS}
  AquirePrimarySelection;
  {$ENDIF}
  DecPaintLock;
end;

procedure TCustomSynEdit.RecalcCharExtent;

  function UsesFontStyle(AStyle: TFontStyle): boolean;
  var
    i: integer;
  begin
    if Assigned(fHighlighter) then begin
      for i := 0 to Pred(fHighlighter.AttrCount) do
        if AStyle in fHighlighter.Attribute[i].Style then begin
          Result := TRUE;
          exit;
        end;
      Result := FALSE;
    end else
      Result := AStyle in Font.Style;
  end;

const
  BoldStyles: array[boolean] of TFontStyles = ([], [fsBold]);
  ItalicStyles: array[boolean] of TFontStyles = ([], [fsItalic]);
begin
  with fTextDrawer do begin
    //debugln('TCustomSynEdit.RecalcCharExtent A UseUTF8=',dbgs(UseUTF8),
    //  ' Font.CanUTF8='+dbgs(Font.CanUTF8)+' CharHeight=',dbgs(CharHeight));
    BaseFont := Self.Font;
    BaseStyle := ItalicStyles[UsesFontStyle(fsItalic)];
    //debugln('TCustomSynEdit.RecalcCharExtent B CharHeight=',dbgs(CharHeight));
    fTextHeight := CharHeight + fExtraLineSpacing;
    BaseStyle := BoldStyles[UsesFontStyle(fsBold)];
    fCharWidth := CharWidth;
  end;
  {$IFDEF SYN_LAZARUS}
  FUseUTF8:=fTextDrawer.UseUTF8;
  //debugln('TCustomSynEdit.RecalcCharExtent UseUTF8=',dbgs(UseUTF8),' Font.CanUTF8=',dbgs(Font.CanUTF8));
  {$ENDIF}
end;

procedure TCustomSynEdit.HighlighterAttrChanged(Sender: TObject);
begin
  RecalcCharExtent;
  SizeOrFontChanged(TRUE);                                                      //jr 2000-10-01
  Invalidate;
end;

procedure TCustomSynEdit.StatusChanged(AChanges: TSynStatusChanges);
begin
  fStatusChanges := fStatusChanges + AChanges;
  if PaintLock = 0 then
    DoOnStatusChange(fStatusChanges);
end;

procedure TCustomSynEdit.DoTabKey;
var
  StartOfBlock: TPoint;
  i, MinLen, iLine: integer;
  PrevLine,
  Spaces: string;
  p: PChar;
  NewCaretX: integer;                                                           //mh 2000-10-01
  ChangeScroll: boolean;                                                        //mh 2000-10-01
begin
  {$IFDEF SYN_LAZARUS}
  if (eoTabIndent in Options) and SelAvail then begin
    DoBlockIndent;
    exit;
  end;
  {$ENDIF}
  i := 0;
  if eoSmartTabs in fOptions then begin
    iLine := CaretY - 1;
    if (iLine > 0) and (iLine < Lines.Count) then begin
      Dec(iLine);
      {$IFNDEF SYN_LAZARUS  ! NOT}
      MinLen := CaretX;
      {$ENDIF}
      repeat
        // NOTE mh: after throwing in real tabs we have to use:
        //      PrevLine := pConvert(Lines[iLine], TabWidth);
        PrevLine := Lines[iLine];
        {$IFDEF SYN_LAZARUS}
        MinLen := PhysicalToLogicalCol(PrevLine,CaretX);
        {$ENDIF}
        if (Length(PrevLine) >= MinLen) then begin
          p := @PrevLine[MinLen];
          // scan over non-whitespaces
          repeat
            if p^ = #32 then break;
            Inc(i);
            Inc(p);
          until p^ = #0;
          // scan over whitespaces
          if p^ <> #0 then
            repeat
              if p^ <> #32 then break;
              Inc(i);
              Inc(p);
            until p^ = #0;
          break;
        end;
        Dec(iLine);
      until iLine < 0;
    end;
  end;
  if i = 0 then begin
    i := TabWidth - (CaretX - 1) mod TabWidth;
    if i = 0 then i := TabWidth;
  end;
  {$IFDEF SYN_LAZARUS}
  // i now contains the needed spaces
  Spaces := CreateTabsAndSpaces(CaretX,i,TabWidth,
                                not (eoTabsToSpaces in Options));
  //debugln('TCustomSynEdit.DoTabKey Spaces="',DbgStr(Spaces),'" TabChar=',DbgStr(TabChar));

  BeginUndoBlock;
  try
    if SelAvail then begin
      fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,
        SelectionMode);
    end;

    StartOfBlock := BlockBegin;
    if SelectionMode = smLine then
      StartOfBlock.X := 1;
    NewCaretX := CaretX + i;
    //debugln('TCustomSynEdit.DoTabKey Before SetSelText Line="',DbgStr(GetLineText),'"');
    SetSelText(Spaces);
    //debugln('TCustomSynEdit.DoTabKey After SetSelText Line="',DbgStr(GetLineText),'"');
    ChangeScroll := not (eoScrollPastEol in fOptions);
    try
      Include(fOptions, eoScrollPastEol);
      CaretX := NewCaretX;
    finally
      if ChangeScroll then
        Exclude(fOptions, eoScrollPastEol);
    end;
    //debugln('TCustomSynEdit.DoTabKey StartOfBlock=',dbgs(StartOfBlock),' fBlockEnd=',dbgs(fBlockEnd),' Spaces="',Spaces,'"');
    fUndoList.AddChange(crInsert, StartOfBlock, fBlockEnd, '', smNormal);
  finally
    EndUndoBlock;
  end;
  EnsureCursorPosVisible;
  {$ELSE}
  Spaces := StringOfChar(' ', i);
  //debugln('TCustomSynEdit.DoTabKey Spaces="',DbgStr(Spaces),'" TabChar=',DbgStr(TabChar));

  if SelAvail then begin
    fUndoList.AddChange(crDelete, fBlockBegin, fBlockEnd, SelText,
      SelectionMode);
  end;
{begin}                                                                         //mh 2000-10-01
  StartOfBlock := CaretXY;
  NewCaretX := StartOfBlock.X + i;
  SetSelText(Spaces);
  ChangeScroll := not (eoScrollPastEol in fOptions);
  try
    Include(fOptions, eoScrollPastEol);
    CaretX := NewCaretX;
  finally
    if ChangeScroll then
      Exclude(fOptions, eoScrollPastEol);
  end;
//  i := CaretY - 1;
//  if eoTrimTrailingSpaces in Options then                                       //JGF 2000-09-23
//    Lines[i] := TrimRight(Lines[i]);
//  EnsureCursorPosVisible;
//  if Length(Lines[i]) >= StartOfBlock.X then
  fUndoList.AddChange(crInsert, StartOfBlock, CaretXY, Spaces, SelectionMode);
  EnsureCursorPosVisible;
{end}                                                                           //mh 2000-10-01
  {$ENDIF}
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
  {$IFDEF SYN_LAZARUS}
  SizeOrFontChanged(true);
  {$ENDIF}
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
  {$IFDEF SYN_LAZARUS}
  if PrimarySelection.OnRequest=@PrimarySelectionRequest then
    PrimarySelection.OnRequest:=nil;
  {$ENDIF}
  inherited DestroyWnd;
end;

procedure TCustomSynEdit.DoBlockIndent;
var
  OrgCaretPos,
  BB,BE            : TPoint;
  Run,
  StrToInsert      : PChar;
  e,x,
  i,InsertStrLen   : integer;
  Spaces           : String;
  OrgSelectionMode : TSynSelectionMode;
begin
  if not SelAvail then exit;
  OrgSelectionMode := fSelectionMode;
  OrgCaretPos := CaretXY;
  x := 1;
  StrToInsert := nil;
  fSelectionMode := smColumn;
  try
    // keep current selection detail
    BB := BlockBegin;
    BE := BlockEnd;

    // build text to insert
    if (BE.X = 1) then begin
      e := BE.y - 1;
      x := 1;
    end else begin
      e := BE.y;
      x := BE.x + {$IFDEF SYN_LAZARUS}fBlockIndent{$ELSE}fTabWidth{$ENDIF};
    end;
    InsertStrLen := ({$IFDEF SYN_LAZARUS}
                     (fBlockIndent+length(LineEnding))* (e - BB.y)+fBlockIndent+1
                     {$ELSE}
                     (fTabWidth+2)* (e - BB.y) + TabWidth +1
                     {$ENDIF}
                     );
    //               chars per line * lines-1    + last line + null char
    StrToInsert := StrAlloc(InsertStrLen);
    try
      Run := StrToInsert;
      Spaces := StringOfChar(#32,
                       {$IFDEF SYN_LAZARUS}fBlockIndent{$ELSE}fTabWidth{$ENDIF});
      for i := BB.Y to e-1 do
      begin
        StrPCopy(Run, Spaces+{$IFDEF SYN_LAZARUS}LineEnding{$ELSE}#13#10{$ENDIF});
        Inc(Run,{$IFDEF SYN_LAZARUS}fBlockIndent+length(LineEnding)
                {$ELSE}fTabWidth+2{$ENDIF});
      end;
      StrPCopy(Run, Spaces);

      InsertBlock(Point(1,BB.y),Point(1,BB.y),StrToInsert);
      fUndoList.AddChange(crIndent, BB, BE, '', smColumn);
    finally
      StrDispose(StrToInsert);
    end;
  finally
    fSelectionMode := OrgSelectionMode;
    SetCaretAndSelection(OrgCaretPos,
      Point(BB.x + {$IFDEF SYN_LAZARUS}fBlockIndent{$ELSE}fTabWidth{$ENDIF},
            BB.y),
      Point(x, BE.y));
  end;
end;

procedure TCustomSynEdit.DoBlockUnindent;
var
  OrgCaretPos,
  BB, BE: TPoint;
  FullStrToDelete: PChar;
  Line, Run,
  StrToDelete: PChar;
  Len,
  x, StrToDeleteLen,
  FirstIndent,
  LastIndent,
  e : integer;
  TempString: AnsiString;
  OrgSelectionMode : TSynSelectionMode;
  SomethingToDelete : Boolean;

  function GetDelLen : integer;
  var
    Run : PChar;
  begin
    Result := 0;
    Run := Line;
    while (Run[0] = ' ')
    and (Result < {$IFDEF SYN_LAZARUS}fBlockIndent{$ELSE}fTabWidth{$ENDIF}) do
    begin
      Inc(Result);
      Inc(Run);
      SomethingToDelete := True;
    end;
  end;

begin
  OrgSelectionMode := fSelectionMode;
  Len := 0;
  LastIndent := 0;
  if SelAvail then
  begin
    // store current selection detail
    BB := BlockBegin;
    BE := BlockEnd;
    OrgCaretPos := CaretXY;

    // convert selection to complete lines
    if BE.X = 1 then
      e := BE.y - 1
    else
      e := BE.y;

    // build string to delete
    StrToDeleteLen :=
      {$IFDEF SYN_LAZARUS}
       (fBlockIndent+length(LineEnding)) * (e - BB.y)
      + fBlockIndent
      + 1;
      {$ELSE}
       (fTabWidth+2) * (e - BB.y)
      + fTabWidth
      + 1;
      {$ENDIF}
    //                 chars per line * lines-1    + last line + null char
    FullStrToDelete := StrAlloc(StrToDeleteLen);
    try
      FullStrToDelete[0] := #0;
      SomethingToDelete := False;
      for x := BB.Y to e-1 do
      begin
        Line := PChar(Lines[x-1]);
        TempString:=StringOfChar(' ', GetDelLen);
        StrCat(FullStrToDelete,PChar(TempString));
        StrCat(FullStrToDelete,
                    PChar({$IFDEF SYN_LAZARUS}LineEnding{$ELSE}#13#10{$ENDIF}));
      end;
      Line := PChar(Lines[e-1]);
      TempString:=StringOfChar(' ', GetDelLen);
      StrCat(FullStrToDelete,PChar(TempString));

      FirstIndent := -1;
      // Delete string
      if SomethingToDelete then
      begin
        StrToDelete := FullStrToDelete;
        CaretY := BB.Y;
        repeat
          Run := GetEOL(StrToDelete);
          if Run <> StrToDelete then
          begin
            Len := Run - StrToDelete;
            if FirstIndent = -1 then
              FirstIndent := Len;
            TempString := Lines[CaretY - 1];
            if Len > 0 then
              Delete(TempString, 1, Len);
            Lines[CaretY - 1] := TempString;
          end;
          {$IFDEF SYN_LAZARUS}
          if Run^ in [#10,#13] then  begin
            if (Run[1] in [#10,#13]) and (Run^<>Run[1]) then
              Inc(Run,2)
            else
              Inc(Run);
            inc(fCaretY);
          end;
          {$ELSE}
          if Run^ = #13 then
          begin
            Inc(Run);
            if Run^ = #10 then
              Inc(Run);
            Inc(fCaretY);
          end;
          {$ENDIF}
          StrToDelete := Run;
        until Run^ = #0;
        LastIndent := Len;
        fUndoList.AddChange(crUnindent, BB, BE, StrToDelete, smColumn);
      end;
      // restore selection
      fSelectionMode := OrgSelectionMode;
      if FirstIndent = -1 then
        FirstIndent := 0;
      SetCaretAndSelection(OrgCaretPos, Point(BB.x - FirstIndent, BB.Y),
        Point(BE.x - LastIndent, BE.y));
    finally
      StrDispose(FullStrToDelete);
    end;
  end;
end;

procedure TCustomSynEdit.DoHomeKey(Selection: boolean);
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
  OldPos:=LogicalCaretXY;
  NewPos:=OldPos;

  if not (eoEnhanceHomeKey in fOptions) and (CaretX>1) then begin
    // not at start of line -> jump to start of line
    NewPos.X:=1;
  end else begin
    // calculate line start position
    FirstNonBlank:=-1;
    if CaretY<=Lines.Count then begin
      s:=fLines[CaretXY.Y-1];

      // search first non blank char pos
      FirstNonBlank:=1;
      while (FirstNonBlank<=length(s)) and (s[FirstNonBlank] in [#32, #9]) do
        inc(FirstNonBlank);
      if FirstNonBlank>length(s) then
        FirstNonBlank:=-1;
    end else
      s:='';
    if FirstNonBlank>=1 then begin
      // this line is not blank
      LineStart:=FirstNonBlank;
    end else begin
      // this line is blank
      // -> use automatic line indent
      LineStart:=GetLineIndentProposal(CaretY,true);
    end;

    NewPos.X:=LineStart;
    if (eoEnhanceHomeKey in fOptions) and (OldPos.X>1) and (OldPos.X<=NewPos.X)
    then begin
      NewPos.X:=1;
    end;
  end;

  MoveCaretAndSelection(OldPos, NewPos, Selection);
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
  if Value <> fModified then begin
    fModified := Value;
    {$IFDEF SYN_LAZARUS}
    if not fModified then begin
      // the current state should be the unmodified state.
      fUndoList.MarkTopAsUnmodified;
      fRedoList.MarkTopAsUnmodified;
    end;
    {$ENDIF}
    StatusChanged([scModified]);
  end;
end;

function TCustomSynEdit.DoOnSpecialLineColors(Line: integer; var Foreground,
  Background: TColor): boolean;
begin
  Result := FALSE;
  if Assigned(fOnSpecialLineColors) then
    fOnSpecialLineColors(Self, Line, Result, Foreground, Background);
end;

procedure TCustomSynEdit.InvalidateLine(Line: integer);
var
  rcInval: TRect;
begin
  if Visible and (Line >= TopLine) and (Line <= TopLine + LinesInWindow) and
     (Line <= Lines.Count) and HandleAllocated
  then begin
    // we invalidate gutter and text area of this line
    rcInval := Rect(0, fTextHeight * (Line - TopLine)
        , ClientWidth{$IFDEF SYN_LAZARUS}-ScrollBarWidth{$ENDIF}, 0);
    rcInval.Bottom := rcInval.Top + fTextHeight;
    if sfLinesChanging in fStateFlags then
      UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
    else
      InvalidateRect(Handle, @rcInval, FALSE);
  end;
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
{$IFDEF SYN_LAZARUS}
{$ELSE}
const
  Brackets: array[0..5] of char = ('(', ')', '[', ']', '{', '}');
var
  Line: string;
  i, PosX, PosY, Len: integer;
  Test, BracketInc, BracketDec: char;
  NumBrackets: integer;
{$ENDIF}
begin
  {$IFDEF SYN_LAZARUS}
  FindMatchingBracket(CaretXY,false,true,false,false);
  {$ELSE}
  // get char at caret
  PosX := CaretX;
  PosY := CaretY;
  Line := LineText;
  if Length(Line) >= PosX then begin
    Test := Line[PosX];
    // is it one of the recognized brackets?
    for i := Low(Brackets) to High(Brackets) do
      if Test = Brackets[i] then begin
        // this is the bracket, get the matching one and the direction
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
              if Test = BracketInc then
                Inc(NumBrackets)
              else if Test = BracketDec then begin
                Dec(NumBrackets);
                if NumBrackets = 0 then begin
                  // matching bracket found, set caret and bail out
                  CaretXY := Point(PosX, PosY);
                  exit;
                end;
              end;
            end;
            // get previous line if possible
            if PosY = 1 then break;
            Dec(PosY);
            Line := Lines[PosY - 1];
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
              if Test = BracketInc then
                Inc(NumBrackets)
              else if Test = BracketDec then begin
                Dec(NumBrackets);
                if NumBrackets = 0 then begin
                  // matching bracket found, set caret and bail out
                  CaretXY := Point(PosX, PosY);
                  exit;
                end;
              end;
            end;
            // get next line if possible
            if PosY = Lines.Count then break;
            Inc(PosY);
            Line := Lines[PosY - 1];
            PosX := 0;
          until FALSE;
        end;
        // don't test the other brackets, we're done
        break;
      end;
  end;
  {$ENDIF}
end;

{$IFDEF SYN_LAZARUS}
function TCustomSynEdit.FindMatchingBracket(PhysStartBracket: TPoint;
  StartIncludeNeighborChars, MoveCaret, SelectBrackets, OnlyVisible: boolean
  ): TPoint;
// returns physical (screen) position of end bracket
const
  Brackets: array[0..5] of char = ('(', ')', '[', ']', '{', '}');
var
  Line: string;
  PosX, PosY: integer;
  StartPt: TPoint;
  LogicalStart: TPoint;

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
      // Selection should have changed...
      StatusChanged([scSelection]);
    end else if MoveCaret then
      CaretXY := LogicalToPhysicalPos(Result)
    else
      Result := LogicalToPhysicalPos(Result);
  end;

  procedure DoFindMatchingBracket(i: integer);
  var
    Test, BracketInc, BracketDec: char;
    NumBrackets, Len: integer;
  begin
    StartPt:=Point(PosX,PosY);
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
          if Test = BracketInc then
            Inc(NumBrackets)
          else if Test = BracketDec then begin
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
        if OnlyVisible and ((PosY<TopLine) or (PosY>=TopLine+LinesInWindow))
        then
          break;
        Line := Lines[PosY - 1];
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
          if Test = BracketInc then
            Inc(NumBrackets)
          else if Test = BracketDec then begin
            Dec(NumBrackets);
            if NumBrackets = 0 then begin
              DoMatchingBracketFound;
              exit;
            end;
          end;
        end;
        // get next line if possible
        if PosY = Lines.Count then break;
        Inc(PosY);
        if OnlyVisible and ((PosY<TopLine) or (PosY>=TopLine+LinesInWindow))
        then
          break;
        Line := Lines[PosY - 1];
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
  if (PosY<1) or (PosY>Lines.Count) then exit;
  if OnlyVisible and ((PosY<TopLine) or (PosY>=TopLine+LinesInWindow)) then
   exit;

  Line := LineText;
  DoCheckBracket;
  try
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
{$ENDIF}

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
  if Assigned(Highlighter) and (PosY >= 0) and (PosY < Lines.Count) then
  begin
    Line := Lines[PosY];
    if PosY = 0 then
      Highlighter.ResetRange
    else
      Highlighter.SetRange(TSynEditStringList(Lines).Ranges[PosY - 1]);
    Highlighter.SetLine(Line, PosY);
    PosX := XY.X;
    if (PosX > 0) and (PosX <= Length(Line)) then
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
  Token := '';
  Attri := nil;
  Result := FALSE;
end;
                                                                                 //L505 end
{$IFDEF SYN_LAZARUS}

procedure TCustomSynEdit.GetWordBoundsAtRowCol(const XY: TPoint; var StartX,
  EndX: integer);
// all params are logical (byte) positions
var
  Line: string;
  IdChars: TSynIdentChars;
  Len: integer;
begin
  //debugln('TCustomSynEdit.GetWordBoundsAtRowCol A ',dbgs(XY));
  StartX:=XY.X;
  EndX:=XY.X;
  if (XY.Y >= 1) and (XY.Y <= Lines.Count) then begin
    Line := Lines[XY.Y - 1];
    Len := Length(Line);
    if (XY.X >= 1) and (XY.X <= Len + 1) then begin
      if Assigned(Highlighter) then
        IdChars := Highlighter.IdentChars
      else
        IdChars := ['a'..'z', 'A'..'Z'];
      EndX := XY.X;
      while (EndX <= Len) and (Line[EndX] in IdChars) do
        Inc(EndX);
      StartX := XY.X;
      while (StartX > 1) and (Line[StartX - 1] in IdChars) do
        Dec(StartX);
    end;
  end;
end;

function TCustomSynEdit.GetLineIndentProposal(Line: integer;
  IgnoreCurrentLineText: boolean): integer;
// calculate a nice indent for the Line (starting at 1)
var
  y: Integer;
  s: string;
  FirstNonBlank: Integer;
begin
  if fLineIndenter<>nil then
    Result:=fLineIndenter.GetLineIndentProposal(Line-1,IgnoreCurrentLineText)
  else begin
    // default: use last non empty line indent, ignore always current line
    y:=Line-1;
    if y>Lines.Count then y:=Lines.Count;
    while y>=1 do begin
      s:=fLines[y-1];
      FirstNonBlank:=1;
      while (FirstNonBlank<=length(s)) and (s[FirstNonBlank] in [' ',#9]) do
        inc(FirstNonBlank);
      if FirstNonBlank<=Length(s) then begin
        // non empty line found
        Result:=LogicalToPhysicalCol(s,FirstNonBlank);
        exit;
      end;
      dec(y);
    end;
    Result:=1;
  end;
end;
{$ENDIF}

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

procedure TCustomSynEdit.NotifyHookedCommandHandlers(AfterProcessing: boolean;
  var Command: TSynEditorCommand;
  var AChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF}; Data: pointer);
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

{begin}                                                                         // djlp - 2000-08-29
procedure TCustomSynEdit.DoOnClearBookmark(var Mark: TSynEditMark);
begin
  if Assigned(fOnClearMark) then
    fOnClearMark(Self, Mark);
end;
{end}                                                                           // djlp - 2000-08-29

procedure TCustomSynEdit.DoOnPaint;
begin
  if Assigned(fOnPaint) then begin
    Canvas.Font.Assign(Font);
    Canvas.Brush.Color := Color;
    fOnPaint(Self, Canvas);
  end;
end;

procedure TCustomSynEdit.DoOnPlaceMark(var Mark: TSynEditMark);
begin
  if Assigned(fOnPlaceMark) then
    fOnPlaceMark(Self, Mark);
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
  if Assigned(fOnStatusChange) then begin
    fOnStatusChange(Self, fStatusChanges);
    fStatusChanges := [];
  end;
end;

procedure TCustomSynEdit.UndoRedoAdded(Sender: TObject);
begin
//  Modified := TRUE;
  {$IFDEF SYN_LAZARUS}
  if fUndoList.UnModifiedMarkerExists then
    Modified:=not fUndoList.IsTopMarkedAsUnmodified
  else if fRedoList.UnModifiedMarkerExists then
    Modified:=not fRedoList.IsTopMarkedAsUnmodified
  else
  {$ENDIF}
    Modified := fUndoList.CanUndo or fUndoList.FullUndoImpossible;              //mh 2000-10-03
  // we have to clear the redo information, since adding undo info removes
  // the necessary context to undo earlier edit actions
  if (Sender = fUndoList) and not (sfInsideRedo in fStateFlags) then            //mh 2000-10-30
    fRedoList.Clear;
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

function TCustomSynEdit.GetWordAtRowCol(XY: TPoint): string;
var
  Line: string;
  IdChars: TSynIdentChars;
  Len, Stop: integer;
begin
  Result := '';
  if (XY.Y >= 1) and (XY.Y <= Lines.Count) then begin
    Line := Lines[XY.Y - 1];
    Len := Length(Line);
    if (XY.X >= 1) and (XY.X <= Len + 1) then begin
      if Assigned(Highlighter) then
        IdChars := Highlighter.IdentChars
      else
        IdChars := ['a'..'z', 'A'..'Z'];
      Stop := XY.X;
      while (Stop <= Len) and (Line[Stop] in IdChars) do
        Inc(Stop);
      while (XY.X > 1) and (Line[XY.X - 1] in IdChars) do
        Dec(XY.X);
      if Stop > XY.X then
        Result := Copy(Line, XY.X, Stop - XY.X);
    end;
  end;
end;

{$IFDEF SYN_LAZARUS}
function TCustomSynEdit.LogicalToPhysicalPos(const p: TPoint): TPoint;
// LogicalToPhysicalPos takes a position in the text and transforms it into
// the row and column it appears to be on the screen
begin
  Result := p;
  if Result.Y - 1 < Lines.Count then
    Result.X:=LogicalToPhysicalCol(Lines[Result.Y - 1],Result.X);
end;
{$ELSE}
function TCustomSynEdit.LogicalToPhysicalPos(p: TPoint): TPoint;
// LogicalToPhysicalPos takes a position in the text and transforms it into
// the row and column it appears to be on the screen
var
  s: string;
  i, L: integer;
  x: integer;
begin
  if p.Y - 1 < Lines.Count then begin
    s := Lines[p.Y - 1];
    if UseUTF8 then begin
    end else begin
      l := Length(s);
      x := 0;
      for i := 1 to p.x - 1 do begin
        if (i <= l) and (s[i] = #9) then
          inc(x, TabWidth - (x mod TabWidth))
        else
          inc(x);
      end;
      p.x := x + 1;
    end;
  end;
  Result := p;
end;
{$ENDIF}

{$IFDEF SYN_LAZARUS}
function TCustomSynEdit.LogicalToPhysicalCol(const Line: string;
  LogicalPos: integer): integer;
begin
  Result:=LogicalToPhysicalCol(PChar(Pointer(Line)),length(Line),LogicalPos,1,1);
end;

function TCustomSynEdit.LogicalToPhysicalCol(Line: PChar; LineLen: integer;
  LogicalPos, StartBytePos, StartPhysicalPos: integer): integer;
// Note: LogicalPos, StartBytePos, StartPhysicalPos start at 1
var
  BytePos, ByteLen: integer;
  ScreenPos: integer;
begin
  ByteLen := LineLen;
  // map UTF8 and Tab chars
  ScreenPos := StartPhysicalPos;
  BytePos:= StartBytePos;
  while BytePos<LogicalPos do begin
    if (BytePos <= ByteLen) then begin
      if Line[BytePos-1] = #9 then begin
        inc(ScreenPos, TabWidth - ((ScreenPos-1) mod TabWidth));
        inc(BytePos);
      end else begin
        inc(ScreenPos);
        if UseUTF8 then
          inc(BytePos,UTF8CharacterLength(@Line[BytePos-1]))
        else
          inc(BytePos);
      end;
    end else begin
      // beyond end of line
      inc(ScreenPos,LogicalPos-BytePos);
      break;
    end;
  end;
  if (BytePos>LogicalPos) and (ScreenPos>StartPhysicalPos) then
    dec(ScreenPos);
  Result := ScreenPos;
end;

function TCustomSynEdit.PhysicalLineLength(Line: PChar; LineLen: integer;
  WithTabs: boolean): integer;
begin
  if WithTabs then
    Result:=LogicalToPhysicalCol(Line,LineLen,LineLen+1,1,1)-1
  else
    Result:=UTF8Length(Line,LineLen);
end;

function TCustomSynEdit.PhysicalToLogicalPos(const p: TPoint): TPoint;
// converts physical (screen) to logical (bytes)
begin
  Result := p;
  if (Result.Y>=1) and (Result.Y <= Lines.Count) then
    Result.X:=PhysicalToLogicalCol(Lines[Result.Y - 1],Result.X,1,1);
end;

function TCustomSynEdit.PhysicalToLogicalCol(const Line: string;
  PhysicalPos: integer): integer;
begin
  Result:=PhysicalToLogicalCol(Line,PhysicalPos,1,1);
end;

function TCustomSynEdit.PhysicalToLogicalCol(const Line: string;
  PhysicalPos, StartBytePos, StartPhysicalPos: integer): integer;
// converts physical (screen) to logical (bytes)
var
  BytePos, ByteLen: integer;
  ScreenPos: integer;
  PLine: PChar;
begin
  ByteLen := Length(Line);
  ScreenPos := StartPhysicalPos;
  BytePos := StartBytePos;
  PLine := PChar(Line);
  // map utf and tab chars
  while ScreenPos < PhysicalPos do begin
    if (BytePos <= ByteLen) then begin
      if (PLine[BytePos-1] <> #9) then begin
        inc(ScreenPos);
        if UseUTF8 then
          inc(BytePos,UTF8CharacterLength(@PLine[BytePos-1]))
        else
          inc(BytePos);
      end else begin
        inc(ScreenPos, TabWidth - ((ScreenPos-1) mod TabWidth));
        inc(BytePos);
      end;
    end else begin
      // beyond end of line
      inc(BytePos,PhysicalPos-ScreenPos);
      break;
    end;
  end;
  if (ScreenPos>PhysicalPos) and (BytePos>1) and (BytePos<ByteLen)
  and (PLine[BytePos-2]=#9) then
    dec(BytePos);
  Result := BytePos;
end;
{$ENDIF}

procedure TCustomSynEdit.DoLinesDeleted(FirstLine, Count: integer);
var
  i: integer;
begin
  // gutter marks
  for i := 0 to Marks.Count - 1 do begin
    if Marks[i].Line >= FirstLine + Count then
      Marks[i].Line := Marks[i].Line - Count
    else if Marks[i].Line > FirstLine then
      Marks[i].Line := FirstLine;
  end;
  // plugins
  if fPlugins <> nil then begin
    for i := 0 to fPlugins.Count - 1 do
      TSynEditPlugin(fPlugins[i]).LinesDeleted(FirstLine, Count);
  end;
end;

procedure TCustomSynEdit.DoLinesInserted(FirstLine, Count: integer);
var
  i: integer;
begin
  // gutter marks
  for i := 0 to Marks.Count - 1 do begin
    if Marks[i].Line >= FirstLine then
      Marks[i].Line := Marks[i].Line + Count;
  end;
  // plugins
  if fPlugins <> nil then begin
    for i := 0 to fPlugins.Count - 1 do
      TSynEditPlugin(fPlugins[i]).LinesInserted(FirstLine, Count);
  end;
end;

procedure TCustomSynEdit.PluginsAfterPaint(ACanvas: TCanvas; AClip: TRect;
  FirstLine, LastLine: integer);
var
  i: integer;
begin
  if fPlugins <> nil then
    for i := 0 to fPlugins.Count - 1 do begin
      TSynEditPlugin(fPlugins[i]).AfterPaint(ACanvas, AClip, FirstLine,
        LastLine);
    end;
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.PrimarySelectionRequest(
  const RequestedFormatID: TClipboardFormat;  Data: TStream);
var s: string;
begin
  if (not SelAvail) or (RequestedFormatID<>CF_TEXT) then exit;
  s:=SelText;
  if s<>'' then
    Data.Write(s[1],length(s));
end;
{$ENDIF}

procedure TCustomSynEdit.TrimmedSetLine(ALine: integer; ALineText: string);
begin
  if eoTrimTrailingSpaces in Options then
    Lines[ALine] := TrimRight(ALineText)
  else
    Lines[ALine] := ALineText;
end;

{ TSynEditMark }

function TSynEditMark.GetEdit: TCustomSynEdit;
begin
  if FEdit <> nil then try
    if FEdit.Marks.IndexOf(self) = -1 then
      FEdit := nil;
  except
    FEdit := nil;
  end;
  Result := FEdit;
end;

function TSynEditMark.GetIsBookmark: boolean;
begin
  Result := (fBookmarkNum >= 0);
end;

procedure TSynEditMark.SetColumn(const Value: Integer);
begin
  FColumn := Value;
end;

procedure TSynEditMark.SetImage(const Value: Integer);
begin
  FImage := Value;
  if fVisible and Assigned(fEdit) then
    fEdit.InvalidateGutterLines(fLine, fLine);
end;

procedure TSynEditMark.SetInternalImage(const Value: boolean);
begin
  fInternalImage := Value;
  if fVisible and Assigned(fEdit) then
    fEdit.InvalidateGutterLines(fLine, fLine);
end;

procedure TSynEditMark.SetLine(const Value: Integer);
begin
  if fVisible and Assigned(fEdit) then begin
    if fLine > 0 then
      fEdit.InvalidateGutterLines(fLine, fLine);
    fLine := Value;
    fEdit.InvalidateGutterLines(fLine, fLine);
  end else
    fLine := Value;
end;

procedure TSynEditMark.SetVisible(const Value: boolean);
begin
  if fVisible <> Value then begin
    fVisible := Value;
    if Assigned(fEdit) then
      fEdit.InvalidateGutterLines(fLine, fLine);
  end;
end;

constructor TSynEditMark.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  fBookmarkNum := -1;
  fEdit := AOwner;
end;

{ TSynEditMarkList }

function TSynEditMarkList.Add(Item: TSynEditMark): Integer;
begin
  Result := inherited Add(Item);
  DoChange;
end;

procedure TSynEditMarkList.ClearLine(Line: integer);
var
  i: integer;
begin
  for i := Count - 1 downto 0 do
    if not Items[i].IsBookmark and (Items[i].Line = Line) then Delete(i);
end;

constructor TSynEditMarkList.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  fEdit := AOwner;
end;

destructor TSynEditMarkList.Destroy;
var
  i: integer;
begin
  for i := 0 to Pred(Count) do
    Get(i).Free;
  inherited Destroy;
end;

procedure TSynEditMarkList.Delete(Index: Integer);
begin
  inherited Delete(Index);
  DoChange;
end;

procedure TSynEditMarkList.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TSynEditMarkList.First: TSynEditMark;
begin
  result := TSynEditMark(inherited First);
end;

function TSynEditMarkList.Get(Index: Integer): TSynEditMark;
begin
  result := TSynEditMark(inherited Get(Index));
end;

//Returns up to maxMarks book/gutter marks for a chosen line.

procedure TSynEditMarkList.GetMarksForLine(line: integer;
  var marks: TSynEditMarks);
var
  cnt: integer;
  i: integer;
begin
  FillChar(marks, SizeOf(marks), 0);
  cnt := 0;
  for i := 0 to Count - 1 do begin
    if Items[i].Line = line then begin
      Inc(cnt);
      marks[cnt] := Items[i];
      if cnt = maxMarks then break;
    end;
  end;
end;

procedure TSynEditMarkList.Insert(Index: Integer; Item: TSynEditMark);
begin
  inherited Insert(Index, Item);
  DoChange;
end;

function TSynEditMarkList.Last: TSynEditMark;
begin
  result := TSynEditMark(inherited Last);
end;

procedure TSynEditMarkList.Place(mark: TSynEditMark);
begin
  if assigned(fEdit) then
    if assigned(fEdit.OnPlaceBookmark) then fEdit.OnPlaceBookmark(fEdit, mark);
  if assigned(mark) then
    Add(mark);
  DoChange;
end;

procedure TSynEditMarkList.Put(Index: Integer; Item: TSynEditMark);
begin
  inherited Put(Index, Item);
  DoChange;
end;

function TSynEditMarkList.Remove(Item: TSynEditMark): Integer;
begin
  Result := inherited Remove(Item);
  DoChange;
end;

{ TSynEditPlugin }

constructor TSynEditPlugin.Create(AOwner: TCustomSynEdit);
begin
  inherited Create;
  if AOwner <> nil then begin
    fOwner := AOwner;
    if fOwner.fPlugins = nil then
      fOwner.fPlugins := TList.Create;
    fOwner.fPlugins.Add(Self);
  end;
end;

destructor TSynEditPlugin.Destroy;
begin
  if fOwner <> nil then
    fOwner.fPlugins.Remove(Self);
  inherited Destroy;
end;

{$IFDEF SYN_LAZARUS}
{ TSynCustomLineIndenter }

procedure TSynCustomLineIndenter.SetEditor(const AValue: TCustomSynEdit);
begin
  if FEditor=AValue then exit;
  FEditor:=AValue;
  if fEditor<>nil then fEditor.LineIndenter:=Self;
end;

destructor TSynCustomLineIndenter.Destroy;
begin
  Editor:=nil;
  inherited Destroy;
end;

procedure TSynCustomLineIndenter.LinesInserted(aIndex: integer;
  aCount: integer);
begin
  // for descendants to override
end;

procedure TSynCustomLineIndenter.LinesDeleted(aIndex: integer; aCount: integer);
begin
  // for descendants to override
end;

procedure TSynCustomLineIndenter.LinesPutted(aIndex: integer; aCount: integer);
begin
  // for descendants to override
end;

procedure TSynCustomLineIndenter.Reset;
begin
  // for descendants to override
end;

function TSynCustomLineIndenter.GetLineIndentProposal(aIndex: integer;
  IgnoreCurrentLineText: boolean): integer;
begin
  // for descendants to override
  RaiseGDBException('TSynEditLineIndentPlugin.GetLineIndentProposal '+ClassName);
  Result:=0;
end;
{$ENDIF}

initialization
  {$IFNDEF SYN_LAZARUS}
  SynEditClipboardFormat := RegisterClipboardFormat(SYNEDIT_CLIPBOARD_FORMAT);
  {$ENDIF}

end.

