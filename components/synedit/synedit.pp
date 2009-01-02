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

uses
{$IFDEF SYN_LAZARUS}
  {$IFDEF USE_UTF8BIDI_LCL}
  FreeBIDI, utf8bidi,
  {$ENDIF}
  Types, FPCAdds, LCLIntf, LCLType, LMessages, LCLProc,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, Classes, Messages, Controls, Graphics, Forms, StdCtrls, ExtCtrls,
{$IFDEF SYN_MBCSSUPPORT}
  Imm,
{$ENDIF}
  SynEditTypes, SynEditSearch, SynEditKeyCmds, SynEditMiscProcs,
  SynEditPointClasses,
{$ifdef SYN_LAZARUS}
  SynEditMarkup, SynEditMarkupHighAll, SynEditMarkupBracket,
  SynEditMarkupCtrlMouseLink, SynEditMarkupSpecialLine, SynEditMarkupSelection,
  SynEditTextBase, SynEditTextTrimmer, SynEditFoldedView,
  SynGutter,
{$ENDIF}
  SynEditMiscClasses, SynEditTextBuffer, SynEditHighlighter, SynTextDrawer;

const
  DIGIT = ['0'..'9'];
// ALPHA            = ['A'..'Z', 'a'..'z'];
// break these up because we exceed the 4 byte limit when combined.
  ALPHA_UC = ['A'..'Z'];
  ALPHA_LC = ['a'..'z'];

{$IFDEF SYN_LAZARUS}
  ScrollBarWidth=0;
  {$UNDEF SynDefaultFont}
  {$IFDEF LCLgtk}
  SynDefaultFontName = '-adobe-courier-medium-r-normal-*-*-140-*-*-*-*-iso10646-1';
  SynDefaultFontHeight = 14;
  {$DEFINE SynDefaultFont}
  {$ENDIF}
  {$IFDEF LCLcarbon}
  SynDefaultFontName = 'Monaco'; // Note: carbon is case sensitive
  SynDefaultFontHeight = 12;
  {$DEFINE SynDefaultFont}
  {$ENDIF}
  {$IFNDEF SynDefaultFont}
  SynDefaultFontName = 'courier';
  SynDefaultFontHeight = 12;
  {$ENDIF}
  SynDefaultFontPitch = fpFixed;
  SynDefaultFontQuality = fqNonAntialiased;

{$ENDIF}

{$IFNDEF SYN_COMPILER_3_UP}
   // not defined in all Delphi versions
  WM_MOUSEWHEEL = $020A;
{$ENDIF}

   // maximum scroll range
  MAX_SCROLL = 32767;

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

  {$IFDEF SYN_LAZARUS}
  TSpecialLineMarkupEvent = procedure(Sender: TObject; Line: integer;
    var Special: boolean; Markup: TSynSelectedColor) of object;
  {$ENDIF}
  TSpecialLineColorsEvent = procedure(Sender: TObject; Line: integer;
    var Special: boolean; var FG, BG: TColor) of object;

  TSynEditCaretType = (ctVerticalLine, ctHorizontalLine, ctHalfBlock, ctBlock);

  TSynStateFlag = (sfCaretChanged, sfScrollbarChanged, sfLinesChanging,
    sfIgnoreNextChar, sfCaretVisible, sfDblClicked, sfPossibleGutterClick,
    {$IFDEF SYN_LAZARUS}
    sfTripleClicked, sfQuadClicked, sfPainting,
    {$ENDIF}
    sfWaitForDragging,{$IFDEF SYN_LAZARUS} sfIsDragging,{$ENDIF} sfInsideRedo
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
    eoShowCtrlMouseLinks,      // Pressing Ctrl (SYNEDIT_LINK_MODIFIER) will highlight the word under the mouse cursor
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

  SYNEDIT_LINK_MODIFIER = {$IFDEF LCLcarbon}ssMeta{$ELSE}ssCtrl{$ENDIF};
  {$ENDIF}

type
// use scAll to update a statusbar when another TCustomSynEdit got the focus
  TSynStatusChange = (scAll, scCaretX, scCaretY, scLeftChar, scTopLine,
    scInsertMode, scModified, scSelection, scReadOnly);
  TSynStatusChanges = set of TSynStatusChange;

  TStatusChangeEvent = procedure(Sender: TObject; Changes: TSynStatusChanges)
    of object;

  TCustomSynEdit = class;

  { Make them visible for Units that use TSynEdit }
  TSynEditMark = SynGutter.TSynEditMark;
  TPlaceMarkEvent = SynGutter.TPlaceMarkEvent;
  TSynEditMarks = SynGutter.TSynEditMarks;
  TSynEditMarkList = SynGutter.TSynEditMarkList;
  TGutterClickEvent = SynGutter.TGutterClickEvent;

  TSynLineState = (slsNone, slsSaved, slsUnsaved);


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
  { TSynCustomBeautifier }

  TSynCustomBeautifier = class(TComponent)
  public
    function LeftSpaces(Editor: TCustomSynEdit; const Line: string;
                        Physical: boolean): Integer;
    // InsertPos is 1 based. e.g. left,top is 1,1
    function GetIndentForLineBreak(Editor: TCustomSynEdit;
                     InsertPos: TPoint; var NextText: string): integer; virtual;
  end;
  {$ENDIF}

  TSynMouseLinkEvent = procedure (
    Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean) of object;

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
    {$IFDEF SYN_LAZARUS}
    procedure WMExit(var Message: TLMExit); message LM_EXIT;
    {$ELSE}
    procedure WMMouseWheel(var Msg: TMessage); message WM_MOUSEWHEEL;
    procedure WMSetCursor(var Msg: TWMSetCursor); message WM_SETCURSOR;
    procedure WMSize(var Msg: TWMSize); message WM_SIZE;
    {$ENDIF}
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMVScroll(var Msg: {$IFDEF SYN_LAZARUS}TLMScroll{$ELSE}TWMScroll{$ENDIF}); message WM_VSCROLL;
  private
    fFirstLine: integer;
    fBlockIndent: integer;
    FBlockSelection: TSynEditSelection;
    {$IFDEF SYN_LAZARUS}
    fCaret: TSynEditCaret;
    fCtrlMouseActive: boolean;
    fMarkupManager : TSynEditMarkupManager;
    fMarkupHighAll : TSynEditMarkupHighlightAll;
    fMarkupHighCaret : TSynEditMarkupHighlightAllCaret;
    fMarkupBracket : TSynEditMarkupBracket;
    fMarkupCtrlMouse : TSynEditMarkupCtrlMouseLink;
    fMarkupSpecialLine : TSynEditMarkupSpecialLine;
    fMarkupSelection : TSynEditMarkupSelection;
    {$ELSE}
    fCaretX: Integer;      // position in Expanded Line = physical position (screen) when LeftChar=1
    fCaretY: Integer;
    {$ENDIF}
    fLastCaretX: integer;  // physical position (screen)                        //mh 2000-10-19
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
{TODO: MFR move to markup}
    fLastCtrlMouseLinkX1: integer; // logical (byte)
    fLastCtrlMouseLinkX2: integer; // logical (byte)
    fHighlighterNeedsUpdateStartLine: integer; // 1 based, 0 means invalid
    fHighlighterNeedsUpdateEndLine: integer; // 1 based, 0 means invalid
    fBeautifier: TSynCustomBeautifier;
    fExtraCharSpacing: integer;
    fTextView : TSynEditFoldedView;
    fTrimLines: TStrings; //TSynEditStringTrimmingList;
    {$ENDIF}
    fLines: TStrings;
    fLinesInWindow: Integer;// MG: fully visible lines in window
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
    {$IFNDEF SYN_LAZARUS}
    fSelectedColor: TSynSelectedColor;
    {$ENDIF}
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
    fOnPaint: TPaintEvent;
    fOnPlaceMark: TPlaceMarkEvent;
    fOnProcessCommand: TProcessCommandEvent;
    fOnProcessUserCommand: TProcessCommandEvent;
    fOnReplaceText: TReplaceTextEvent;
    fOnSpecialLineColors: TSpecialLineColorsEvent;// needed, because bug fpc 11926
    fOnStatusChange: TStatusChangeEvent;
    {$IFDEF SYN_LAZARUS}
    FOnSpecialLineMarkup: TSpecialLineMarkupEvent;// needed, because bug fpc 11926
    FOnClickLink: TMouseEvent;
    FOnMouseLink: TSynMouseLinkEvent;
    {$ENDIF}

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
    function GetBlockBegin: TPoint;
    function GetBlockEnd: TPoint;
    function GetBracketHighlightStyle: TSynEditBracketHighlightStyle;
    function GetCanPaste: Boolean;
    function GetCanRedo: Boolean;
    function GetCanUndo: Boolean;
    function GetCaretXY: TPoint;
    function GetFont: TFont;
    {$IFDEF SYN_LAZARUS}
    function GetCaretX : Integer;
    function GetCaretY : Integer;
    function GetHighlightAllColor : TSynSelectedColor;
    function GetHighlightCaretColor: TSynSelectedColor;
    function GetIncrementColor : TSynSelectedColor;
    function GetLineHighlightColor: TSynSelectedColor;
    function GetLineNumberColor: TSynSelectedColor;
    function GetModifiedLineColor: TSynSelectedColor;
    function GetCodeFoldingTreeColor: TSynSelectedColor;
    function GetOnGutterClick : TGutterClickEvent;
    function GetSelectedColor : TSynSelectedColor;
    function GetBracketMatchColor : TSynSelectedColor;
    function GetMouseLinkColor : TSynSelectedColor;
    procedure SetBracketHighlightStyle(
      const AValue: TSynEditBracketHighlightStyle);
    procedure SetOnGutterClick(const AValue : TGutterClickEvent);
    procedure SetRealLines(const AValue : TStrings);
    procedure SetSelectedColor(const AValue : TSynSelectedColor);
    procedure SetSpecialLineColors(const AValue : TSpecialLineColorsEvent);
    procedure SetSpecialLineMarkup(const AValue : TSpecialLineMarkupEvent);
    {$ENDIF}
    function GetHookedCommandHandlersCount: integer;
    function GetLineText: string;
    {$IFDEF SYN_LAZARUS}
    function GetCharLen(const Line: string; CharStartPos: integer): integer;
    function AdjustPhysPosToCharacterStart(Line: integer; PhysPos: integer): integer;
    function GetLogicalCaretXY: TPoint;
    procedure SetCFDividerDrawLevel(const AValue: Integer);
    function  GetCFDividerDrawLevel : Integer;
    procedure SetLogicalCaretXY(const NewLogCaretXY: TPoint);
    procedure SetBeautifier(NewBeautifier: TSynCustomBeautifier);
    {$ENDIF}
    function GetMaxUndo: Integer;
    function GetSelAvail: Boolean;
    function GetSelText: string;
    function SynGetText: string;
    {$IFDEF SYN_LAZARUS}
    procedure SetTabChar(const AValue: Char);
    {$ENDIF}
    procedure GutterChanged(Sender: TObject);
    procedure InsertBlock(BB: TPoint; ChangeStr: PChar);
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
    procedure DoBlockSelectionChanged(Sender: TObject);
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
    procedure SetHideSelection(const Value: boolean);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetInsertCaret(const Value: TSynEditCaretType);
    procedure SetInsertMode(const Value: boolean);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    {$ifdef SYN_LAZARUS}
    procedure SetExtraCharSpacing(const Value: integer);
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
    function  GetSelectionMode : TSynSelectionMode;
    procedure SetSelectionMode(const Value: TSynSelectionMode);
    {$IFNDEF SYN_LAZARUS}
    procedure SetSelText(const Value: string);
    {$ENDIF}
    procedure SetSelTextExternal(const Value: string);
    procedure SetTabWidth(Value: integer);
    procedure SynSetText(const Value: string);
    procedure SetTopLine(Value: Integer);
    procedure SetWantTabs(const Value: boolean);
    procedure SetWordBlock(Value: TPoint);
    {$IFDEF SYN_LAZARUS}
    procedure SetLineBlock(Value: TPoint);
    procedure SetParagraphBlock(Value: TPoint);
    {$ENDIF}
    procedure SizeOrFontChanged(bFont: boolean);
    procedure StatusChanged(AChanges: TSynStatusChanges);
    {$IFNDEF SYN_LAZARUS}
    procedure TrimmedSetLine(ALine: integer; ALineText: string);
    {$ENDIF}
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
    procedure Resize; override;
    function RealGetText: TCaption; override;
    procedure RealSetText(const Value: TCaption); override;
    {$ENDIF}
    procedure DecPaintLock;
    procedure DestroyWnd; override;
    procedure DragOver(Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean); override;
    procedure FontChanged(Sender: TObject); {$IFDEF SYN_LAZARUS}override;{$ENDIF}
    function GetReadOnly: boolean; virtual;
    procedure HideCaret;
    procedure HighlighterAttrChanged(Sender: TObject);
    procedure IncPaintLock;
    procedure InitializeCaret;
    // note: FirstLine and LastLine don't need to be in correct order
    procedure InvalidateGutterLines(FirstLine, LastLine: integer);
    procedure InvalidateLines(FirstLine, LastLine: integer);
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
    {$IFDEF SYN_LAZARUS}
    procedure FoldChanged(Index: integer);
    function GetTopView : Integer;
    procedure SetTopView(const AValue : Integer);
    {$ENDIF}
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
    procedure PaintTextLines(AClip: TRect; FirstLine, LastLine,
      FirstCol, LastCol: integer); virtual;
    {$IFDEF SYN_LAZARUS}
    procedure StartPaintBuffer(const ClipRect: TRect);
    procedure EndPaintBuffer(const ClipRect: TRect);
    {$ENDIF}
    procedure RecalcCharExtent;
    procedure RedoItem;                                                         //sbs 2000-11-19
    procedure SetCaretXY(Value: TPoint); virtual;
    procedure SetName(const Value: TComponentName); override;
    procedure SetReadOnly(Value: boolean); virtual;
    procedure SetSelTextPrimitive(PasteMode: TSynSelectionMode; Value: PChar;
      AddToUndoList: Boolean = false; ChangeReason: TSynChangeReason = crInsert);
    procedure ShowCaret;
    // If the translations requires Data, memory will be allocated for it via a
    // GetMem call.  The client must call FreeMem on Data if it is not NIL.
    function TranslateKeyCode(Code: word; Shift: TShiftState; var Data: pointer
      {$IFDEF SYN_LAZARUS};out IsStartOfCombo: boolean{$ENDIF}): TSynEditorCommand;
    procedure UndoItem;                                                         //sbs 2000-11-19
  protected
    fGutterWidth: Integer;
    {$IFDEF EnableDoubleBuf}
    BufferBitmap: TBitmap; // the double buffer
    {$ENDIF}
    SavedCanvas: TCanvas; // the normal TCustomControl canvas during paint
    procedure DoOnClearBookmark(var Mark: TSynEditMark); virtual;               // djlp - 2000-08-29
    procedure DoOnCommandProcessed(Command: TSynEditorCommand;
      AChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
      Data: pointer); virtual;
    // no method DoOnDropFiles, intercept the WM_DROPFILES instead
    procedure DoOnPaint; virtual;
    procedure DoOnPlaceMark(var Mark: TSynEditMark); virtual;
    procedure DoOnProcessCommand(var Command: TSynEditorCommand;
      var AChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
      Data: pointer); virtual;
    function DoOnReplaceText(const ASearch, AReplace: string;
      Line, Column: integer): TSynReplaceAction; virtual;
    {$IFNDEF SYN_LAZARUS}
    function DoOnSpecialLineColors(Line: integer;
      var Foreground, Background: TColor): boolean; virtual;
    {$ENDIF}
    procedure DoOnStatusChange(Changes: TSynStatusChanges); virtual;
    {$IFDEF SYN_LAZARUS}
    property LastMouseCaret: TPoint read FLastMouseCaret write SetLastMouseCaret;
    function GetSelEnd: integer;                                                 //L505
    function GetSelStart: integer;
    procedure SetSelEnd(const Value: integer);
    procedure SetSelStart(const Value: integer);
    property TextView : TSynEditFoldedView read fTextView;
    property TopView: Integer read GetTopView write SetTopView;  // TopLine converted into Visible(View) lines
    {$ENDIF}
  public
    procedure FindMatchingBracket; virtual;
    {$IFDEF SYN_LAZARUS}
    function FindMatchingBracket(PhysStartBracket: TPoint;
                                 StartIncludeNeighborChars, MoveCaret,
                                 SelectBrackets, OnlyVisible: Boolean
                                 ): TPoint; virtual;
    //code fold
    procedure CodeFoldAction(iLine: integer);
    function FindNextUnfoldedLine(iLine: integer; Down: boolean): Integer;
    procedure UnfoldAll;
    procedure FoldAll(StartLevel : Integer = 0; IgnoreNested : Boolean = False);
    procedure EraseBackground(DC: HDC); override;
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
    function IsLinkable(Y, X1, X2: Integer): Boolean;
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
    procedure MarkTextAsSaved;
    {$IFDEF SYN_LAZARUS}
    // Byte to Char
    function LogicalToPhysicalPos(const p: TPoint): TPoint;
    function LogicalToPhysicalCol(const Line: string;
                                  LogicalPos: integer): integer;
    function LogicalToPhysicalCol(Line: PChar; LineLen: integer;
                  LogicalPos, StartBytePos, StartPhysicalPos: integer): integer;
    // Char to Byte
    function PhysicalToLogicalPos(const p: TPoint): TPoint;
    function PhysicalToLogicalCol(const Line: string;
                                  PhysicalPos: integer): integer;
    function PhysicalToLogicalCol(const Line: string;
                 PhysicalPos, StartBytePos, StartPhysicalPos: integer): integer;
    function PhysicalLineLength(Line: PChar; LineLen: integer;
                                WithTabs: boolean): integer;
    function ScreenColumnToXValue(Col: integer): integer;  // map screen column to screen pixel
    procedure MoveCaretToVisibleArea;
    procedure MoveCaretIgnoreEOL(const NewCaret: TPoint);
    procedure MoveLogicalCaretIgnoreEOL(const NewLogCaret: TPoint);
    function NextTokenPos: TPoint; virtual;
    {$ELSE}
    function LogicalToPhysicalPos(p: TPoint): TPoint;
    {$ENDIF}
    function NextWordPos{$IFDEF SYN_LAZARUS}(WordEndForDelete : Boolean = false){$ENDIF}: TPoint; virtual;
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
    {$IFDEF SYN_LAZARUS}
    function SearchReplaceEx(const ASearch, AReplace: string;
      AOptions: TSynSearchOptions; AStart: TPoint): integer;
    {$ENDIF}
    procedure SelectAll;
    {$IFDEF SYN_LAZARUS}
    Procedure SetHighlightSearch(const ASearch: String; AOptions: TSynSearchOptions);
    procedure SelectToBrace;
    procedure SelectLine;
    procedure SelectParagraph;
    procedure SetUseIncrementalColor(const AValue : Boolean);
    {$ENDIF}
    procedure SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
    procedure SetDefaultKeystrokes; virtual;
    procedure SetOptionFlag(Flag: TSynEditorOption; Value: boolean);
    procedure SetSelWord;
    procedure Undo;
    function GetLineState(ALine: Integer): TSynLineState;
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
    {$IFDEF SYN_LAZARUS}
    property CaretX: Integer read GetCaretX write SetCaretX;
    property CaretY: Integer read GetCaretY write SetCaretY;
    {$ELSE}
    property CaretX: Integer read fCaretX write SetCaretX;
    property CaretY: Integer read fCaretY write SetCaretY;
    {$ENDIF}
    property CaretXY: TPoint read GetCaretXY write SetCaretXY;
    property CharsInWindow: Integer read fCharsInWindow;
    property CharWidth: integer read fCharWidth;
    property Color default clWhite;
    {$IFDEF SYN_LAZARUS}
    property Beautifier: TSynCustomBeautifier read fBeautifier write SetBeautifier;
    property CtrlMouseActive: boolean read fCtrlMouseActive;
    property LogicalCaretXY: TPoint read GetLogicalCaretXY write SetLogicalCaretXY;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SelEnd: Integer read GetSelEnd write SetSelEnd;
    property UseIncrementalColor : Boolean write SetUseIncrementalColor;
    {$ENDIF}
    property Font: TFont read GetFont write SetFont;
    property GutterWidth: Integer read fGutterWidth;
    property Highlighter: TSynCustomHighlighter
      read fHighlighter write SetHighlighter;
    property LeftChar: Integer read fLeftChar write SetLeftChar;
    property LineHeight: integer read fTextHeight;
    property LinesInWindow: Integer read fLinesInWindow; // MG: fully visible lines
    property LineText: string read GetLineText write SetLineText;
    {$IFDEF SYN_LAZARUS}
    property RealLines: TStrings read fLines write SetRealLines;                // No trailing (trimmable) spaces
    property Lines: TStrings read fTrimLines write SetLines;
    {$ELSE}
    property Lines: TStrings read fLines write SetLines;
    {$ENDIF}
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
    property ExtraCharSpacing: integer
      read fExtraCharSpacing write SetExtraCharSpacing default 0;
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
    {$IFDEF SYN_LAZARUS}
    property SelectedColor: TSynSelectedColor
    read GetSelectedColor write SetSelectedColor;  // Setter for compatibility
    property IncrementColor: TSynSelectedColor read GetIncrementColor;
    property HighlightAllColor: TSynSelectedColor read GetHighlightAllColor;
    property HighlightCaretColor: TSynSelectedColor read GetHighlightCaretColor;
    property BracketMatchColor: TSynSelectedColor read GetBracketMatchColor;
    property MouseLinkColor: TSynSelectedColor read GetMouseLinkColor;
    property LineNumberColor: TSynSelectedColor read GetLineNumberColor;
    property LineHighlightColor: TSynSelectedColor read GetLineHighlightColor;
    property ModifiedLineColor: TSynSelectedColor read GetModifiedLineColor;
    property CodeFoldingTreeColor: TSynSelectedColor read GetCodeFoldingTreeColor;
    property BracketHighlightStyle: TSynEditBracketHighlightStyle
      read GetBracketHighlightStyle write SetBracketHighlightStyle;
    //property Color: TSynSelectedColor read GetSelectedColor;
    {$ELSE}
    property SelectedColor: TSynSelectedColor
      read FSelectedColor write FSelectedColor;
    {$ENDIF}
    property SelectionMode: TSynSelectionMode
      read GetSelectionMode write SetSelectionMode default smNormal;
    {$IFDEF SYN_LAZARUS}
    property TabChar: char read FTabChar write SetTabChar;
    property CFDividerDrawLevel: Integer
        read GetCFDividerDrawLevel write SetCFDividerDrawLevel;
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
      read GetOnGutterClick write SetOnGutterClick;
    property OnPaint: TPaintEvent read fOnPaint write fOnPaint;
    property OnPlaceBookmark: TPlaceMarkEvent
      read FOnPlaceMark write FOnPlaceMark;
    property OnProcessUserCommand: TProcessCommandEvent
      read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnReplaceText: TReplaceTextEvent read fOnReplaceText
      write fOnReplaceText;
    {$IFDEF SYN_LAZARUS}
    property OnSpecialLineColors: TSpecialLineColorsEvent
      read FOnSpecialLineColors write SetSpecialLineColors;  deprecated;
    {$ELSE}
    property OnSpecialLineColors: TSpecialLineColorsEvent
      read fOnSpecialLineColors write fOnSpecialLineColors;
    {$ENDIF}
    {$IFDEF SYN_LAZARUS}
    property OnSpecialLineMarkup: TSpecialLineMarkupEvent
      read FOnSpecialLineMarkup write SetSpecialLineMarkup;
    {$ENDIF}
    property OnStatusChange: TStatusChangeEvent
      read fOnStatusChange write fOnStatusChange;
  end;

  TSynEdit = class(TCustomSynEdit)
  published
    // inherited properties
    property Align;
{$IFDEF SYN_LAZARUS}
    property Beautifier;
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
    property OnClickLink : TMouseEvent read FOnClickLink write FOnClickLink;
    property OnMouseLink: TSynMouseLinkEvent read FOnMouseLink write FOnMouseLink;
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
    {$IFDEF SYN_LAZARUS}
    property ExtraCharSpacing;
    {$ENDIF}
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
    {$IFDEF SYN_LAZARUS}
    property IncrementColor;
    property HighlightAllColor;
    property BracketHighlightStyle;
    property BracketMatchColor;
    property ModifiedLineColor;
    property CodeFoldingTreeColor;
    property MouseLinkColor;
    property LineNumberColor;
    property LineHighlightColor;
    {$ENDIF}
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
    property OnSpecialLineColors; deprecated;
    {$IFDEF SYN_LAZARUS}
    property OnSpecialLineMarkup;
    {$ENDIF}
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
// ScreenRow is 0-base
// result is 1-based
begin
  Result := fTextView.ScreenLineToTextIndex(ScreenRow)+1;
//  DebugLn(['=== SrceenRow TO Row   In:',ScreenRow,'  out:',Result, ' topline=',TopLine, '  view topline=',fTextView.TopLine]);
end;

function TCustomSynEdit.RowToScreenRow(PhysicalRow: integer): integer;
// returns -1 for lines above visible screen (<TopLine)
// 0 for the first line
// 0 to LinesInWindow for visible lines (incl last partial visble line)
// and returns LinesInWindow+1 for lines below visible screen
begin
  Result := fTextView.TextIndexToScreenLine(PhysicalRow-1);
  if Result < -1 then Result := -1;
  if Result > LinesInWindow+1 then Result := LinesInWindow+1;
//  DebugLn(['=== Row TO ScreenRow   In:',PhysicalRow,'  out:',Result]);
end;
{$ENDIF}

function TCustomSynEdit.RowColumnToPixels(
  {$IFDEF SYN_LAZARUS}const {$ENDIF}RowCol: TPoint): TPoint;
// converts screen position (1,1) based
// to client area coordinate (0,0 based on canvas)
begin
  Result:=RowCol;
  Result.X := (Result.X - 1) * fCharWidth + fTextOffset;
  {$IFDEF SYN_LAZARUS}
  Result.Y := RowToScreenRow(RowCol.Y) * fTextHeight;
  {$ELSE}
  Result.Y := (Result.Y - fTopLine) * fTextHeight;
  {$ENDIF}
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
begin
  if SelAvail then begin
    DoCopyToClipboard(SelText);
  end;
end;

procedure TCustomSynEdit.CutToClipboard;
begin
  if SelAvail then begin
    DoCopyToClipboard(SelText);
    SetSelTextExternal('');
  end;
end;

constructor TCustomSynEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{begin}                                                                         //mh 2000-10-10
//  fLines := TSynEditList.Create;
  fLines := TSynEditStringList.Create;
  {$IFDEF SYN_LAZARUS}
  fCaret := TSynEditCaret.Create;

  fTrimLines := TSynEditStringTrimmingList.Create(TSynEditStrings(fLines),
                                                  fCaret);
  fCaret.Lines := TSynEditStrings(fTrimLines);
  fTextView := TSynEditFoldedView.Create(TSynEditStringList(fLines),
                                         TSynEditStrings(fTrimLines),
                                         fCaret);
  fTextView.OnFoldChanged := {$IFDEF FPC}@{$ENDIF}FoldChanged;
  {$ENDIF}
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
  {$IFDEF SYN_LAZARUS}
  TSynEditStringTrimmingList(fTrimLines).UndoList := fUndoList;
  {$ENDIF}

  FBlockSelection := TSynEditSelection.Create(TSynEditStrings(FTrimLines));
  FBlockSelection.Caret := FCaret;
  FBlockSelection.UndoList := fUndoList;
  FBlockSelection.InvalidateLinesMethod := {$IFDEF FPC}@{$ENDIF}InvalidateLines;
  FBlockSelection.LinesDeletedMethod := {$IFDEF FPC}@{$ENDIF}DoLinesDeleted;
  FBlockSelection.LinesInsertedMethod := {$IFDEF FPC}@{$ENDIF}DoLinesInserted;
  FBlockSelection.AddChangeHandler({$IFDEF FPC}@{$ENDIF}DoBlockSelectionChanged);

{$IFDEF SYN_COMPILER_4_UP}
{$IFNDEF SYN_LAZARUS}
// ToDo DoubleBuffered
  DoubleBuffered := false;
{$ENDIF}
{$ENDIF}
  {$IFNDEF SYN_LAZARUS}
  fSelectedColor := TSynSelectedColor.Create;
  fSelectedColor.OnChange := {$IFDEF FPC}@{$ENDIF}SelectedColorsChanged;
  {$ENDIF}
  fTextDrawer := TheTextDrawer.Create([fsBold], fFontDummy);
  fBookMarkOpt := TSynBookMarkOpt.Create(Self);
  fBookMarkOpt.OnChange := {$IFDEF FPC}@{$ENDIF}BookMarkOptionsChanged;
// fRightEdge has to be set before FontChanged is called for the first time
  fRightEdge := 80;
  fGutter := TSynGutter.Create(self, fTextView, fBookMarkOpt, fTextDrawer);
  fGutter.OnChange := {$IFDEF FPC}@{$ENDIF}GutterChanged;
  fGutterWidth := fGutter.Width;
  fTextOffset := fGutterWidth + 2;
  ControlStyle := ControlStyle + [csOpaque, csSetCaption
                    {$IFDEF SYN_LAZARUS}, csTripleClicks, csQuadClicks{$ENDIF}];
  Height := 150;
  Width := 200;
  Cursor := crIBeam;
{$IFDEF SYN_LAZARUS}
  // needed before setting color
  fMarkupHighAll   := TSynEditMarkupHighlightAll.Create(self);
  fMarkupHighCaret := TSynEditMarkupHighlightAllCaret.Create(self);
  fMarkupBracket   := TSynEditMarkupBracket.Create(self);
  fMarkupCtrlMouse := TSynEditMarkupCtrlMouseLink.Create(self);
  fMarkupSpecialLine := TSynEditMarkupSpecialLine.Create(self);
  fMarkupSelection := TSynEditMarkupSelection.Create(self, FBlockSelection);

  fMarkupManager := TSynEditMarkupManager.Create(self);
  fMarkupManager.AddMarkUp(fMarkupHighAll);
  fMarkupManager.AddMarkUp(fMarkupHighCaret);
  fMarkupManager.AddMarkUp(fMarkupCtrlMouse);
  fMarkupManager.AddMarkUp(fMarkupSpecialLine);
  fMarkupManager.AddMarkUp(fMarkupBracket);
  fMarkupManager.AddMarkUp(fMarkupSelection);
  fMarkupManager.Lines := TSynEditStrings(Lines);
  fMarkupManager.InvalidateLinesMethod := @InvalidateLines;

  Color := clWhite;
  fFontDummy.Name := SynDefaultFontName;
  fFontDummy.Height := SynDefaultFontHeight;
  fFontDummy.Pitch := SynDefaultFontPitch;
  fFontDummy.Quality := SynDefaultFontQuality;
  fLastMouseCaret := Point(-1,-1);
  fLastCtrlMouseLinkY := -1;
  fLastControlIsPressed := false;
  fBlockIndent := 2;
  FTabChar := {$IFDEF DebugShowTabs}'%'{$ELSE}' '{$ENDIF};
{$ELSE}
  Color := clWindow;
  fFontDummy.Name := 'Courier New';
  fFontDummy.Size := 10;
{$IFDEF SYN_COMPILER_3_UP}
// ToDo Font CharSet
  fFontDummy.CharSet := DEFAULT_CHARSET;
{$ENDIF}
{$ENDIF}
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
  fKeystrokes := TSynEditKeyStrokes.Create(Self);
  {$IFDEF SYN_LAZARUS}
  if assigned(Owner) and not (csLoading in Owner.ComponentState) then
  {$ENDIF}
  SetDefaultKeystrokes;
  fMarkList := TSynEditMarkList.Create(self);
  fMarkList.OnChange := {$IFDEF FPC}@{$ENDIF}MarkListChange;
  fRightEdgeColor := clSilver;
{$IFDEF SYN_MBCSSUPPORT}
  fImeCount := 0;
  fMBCSStepAside := False;
{$ENDIF}
  fWantTabs := False;
  fTabWidth := 8;
  fLeftChar := 1;
  fTopLine := 1;
  {$IFDEF SYN_LAZARUS}
  fTextView.TopLine := 1;
  {$ELSE}
  fCaretX := 1;
  fCaretY := 1;
  {$ENDIF}
  fLastCaretX := 1;                                                             //mh 2000-10-19
  // find / replace
  fTSearch := TSynEditSearch.Create;
  fOptions := SYNEDIT_DEFAULT_OPTIONS;
  {$IFDEF SYN_LAZARUS}
  TSynEditStringTrimmingList(fTrimLines).Enabled := eoTrimTrailingSpaces in fOptions;
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
begin
  if (fPaintLock=1) and HandleAllocated then begin
    {$IFDEF SYN_LAZARUS}
    if fHighlighterNeedsUpdateStartLine>0 then begin
      //DebugLn('TCustomSynEdit.DecPaintLock ',dbgs(fHighlighterNeedsUpdateStartLine),'-',dbgs(fHighlighterNeedsUpdateEndLine));
      if fHighlighterNeedsUpdateStartLine<=Lines.Count then begin
        if fHighlighterNeedsUpdateEndLine>Lines.Count then
          fHighlighterNeedsUpdateEndLine:=Lines.Count;
        LastLineChanged:=fHighlighterNeedsUpdateEndLine;
        // rescan all lines in range
        // Note: The highlighter range of the line can be invalid as well,
        //       so start scan one line earlier
        LastLineChanged:=ScanFrom(fHighlighterNeedsUpdateStartLine-2,
                                  fHighlighterNeedsUpdateEndLine-1);
        //DebugLn('TCustomSynEdit.DecPaintLock ',dbgs(fHighlighterNeedsUpdateStartLine),'-',dbgs(fHighlighterNeedsUpdateEndLine),' LastLineChanged=',dbgs(LastLineChanged));
        InvalidateLines(fHighlighterNeedsUpdateStartLine,LastLineChanged+1);
        InvalidateGutterLines(fHighlighterNeedsUpdateStartLine,LastLineChanged+1);
      end;
      fHighlighterNeedsUpdateStartLine:=0;
      fHighlighterNeedsUpdateEndLine:=0;
    end;
    {$ENDIF}
  end;
  {$IFDEF SYN_LAZARUS}
  fTextView.UnLock; // after ScanFrom, but before UpdateCaret
  {$ENDIF}
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
  Beautifier:=nil;
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
  fFontDummy.Free;
  fBlockSelection.Free;
  Lines.Free;
  {$ELSE}
  fHookedCommandHandlers:=nil;
  fPlugins:=nil;
  FreeAndNil(fScrollTimer);
  FreeAndNil(fTSearch);
  FreeAndNil(fMarkupManager);
  FreeAndNil(fMarkList);
  FreeAndNil(fBookMarkOpt);
  FreeAndNil(fKeyStrokes);
  FreeAndNil(fUndoList);
  FreeAndNil(fRedoList);
  FreeAndNil(fGutter);
  FreeAndNil(fTextDrawer);
  FreeAndNil(fFontDummy);
  FreeAndNil(fTextView);
  FreeAndNil(fBlockSelection);
  FreeAndNil(fTrimLines);
  FreeAndNil(fLines);
  FreeAndNil(fCaret);
  {$ENDIF}
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

function TCustomSynEdit.GetFont: TFont;
begin
  Result := inherited Font;
end;

function TCustomSynEdit.GetLineText: string;
begin
  Result := FCaret.LineText;
end;

{$IFDEF SYN_LAZARUS}
function TCustomSynEdit.GetHighlightAllColor : TSynSelectedColor;
begin
  result := fMarkupHighAll.MarkupInfo;
end;

function TCustomSynEdit.GetHighlightCaretColor: TSynSelectedColor;
begin
  result := fMarkupHighCaret.MarkupInfo;
end;

function TCustomSynEdit.GetIncrementColor : TSynSelectedColor;
begin
  result := fMarkupSelection.MarkupInfoIncr;
end;

function TCustomSynEdit.GetLineHighlightColor: TSynSelectedColor;
begin
  Result := fMarkupSpecialLine.MarkupLineHighlightInfo;
end;

function TCustomSynEdit.GetLineNumberColor: TSynSelectedColor;
begin
  Result := fGutter.MarkupInfoLineNumber;
end;

function TCustomSynEdit.GetModifiedLineColor: TSynSelectedColor;
begin
  Result := fGutter.MarkupInfoModifiedLine;
end;

function TCustomSynEdit.GetOnGutterClick : TGutterClickEvent;
begin
  Result := fGutter.OnGutterClick;
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

procedure TCustomSynEdit.SetBracketHighlightStyle(
  const AValue: TSynEditBracketHighlightStyle);
begin
  fMarkupBracket.HighlightStyle := AValue;
end;

procedure TCustomSynEdit.SetOnGutterClick(const AValue : TGutterClickEvent);
begin
  fGutter.OnGutterClick := AValue;
end;

procedure TCustomSynEdit.SetRealLines(const AValue : TStrings);
begin
  if HandleAllocated then
    fLines.Assign(AValue);
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
  fTextView.CFDividerDrawLevel := AValue;
end;

function TCustomSynEdit.GetCFDividerDrawLevel : Integer;
begin
  Result := fTextView.CFDividerDrawLevel;
end;

procedure TCustomSynEdit.SetLogicalCaretXY(const NewLogCaretXY: TPoint);
begin
  CaretXY:=LogicalToPhysicalPos(NewLogCaretXY);
end;

procedure TCustomSynEdit.SetBeautifier(NewBeautifier: TSynCustomBeautifier);
begin
  if fBeautifier=NewBeautifier then exit;
  fBeautifier:=NewBeautifier;
end;

{$ENDIF}

function TCustomSynEdit.GetSelAvail: Boolean;
begin
  Result := FBlockSelection.SelAvail;
end;

function TCustomSynEdit.GetSelText: string;
begin
  Result := FBlockSelection.SelText;
end;

function TCustomSynEdit.SynGetText: string;
begin
  Result := fLines.Text;
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
  if Lines<>nil then
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
  {$IFDEF SYN_LAZARUS}
  fTextView.Lock; //DecPaintLock triggers ScanFrom, and folds must wait
  {$ENDIF}
end;

procedure TCustomSynEdit.InvalidateGutter;
begin
  InvalidateGutterLines(-1, -1);
end;

procedure TCustomSynEdit.InvalidateGutterLines(FirstLine, LastLine: integer);
var
  rcInval: TRect;
begin
  if sfPainting in fStateFlags then exit;
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then begin
      rcInval := Rect(0, 0, fGutterWidth, ClientHeight - ScrollBarWidth);
      if sfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else begin
        {$IFDEF VerboseSynEditInvalidate}
        DebugLn(['TCustomSynEdit.InvalidateGutterLines ALL ',dbgs(rcInval)]);
        {$ENDIF}
        InvalidateRect(Handle, @rcInval, FALSE);
      end;
    end else begin
      { find the visible lines first }
      if LastLine >= 0 then begin
        if (LastLine < FirstLine) then SwapInt(LastLine, FirstLine);
        LastLine := RowToScreenRow(Min(LastLine, ScreenRowToRow(LinesInWindow)))+1;
      end
      else
        LastLine := LinesInWindow + 1;
      FirstLine := RowToScreenRow(Max(FirstLine, TopLine));
      { any line visible? }
      if (LastLine >= FirstLine) then begin
        rcInval := Rect(0, fTextHeight * FirstLine,
          fGutterWidth, fTextHeight * LastLine);
        if sfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else begin
          {$IFDEF VerboseSynEditInvalidate}
          DebugLn(['TCustomSynEdit.InvalidateGutterLines PART ',dbgs(rcInval)]);
          {$ENDIF}
          InvalidateRect(Handle, @rcInval, FALSE);
        end;
      end;
    end;
end;

procedure TCustomSynEdit.InvalidateLines(FirstLine, LastLine: integer);
var
  rcInval: TRect;
  f, l: Integer;
begin
  if sfPainting in fStateFlags then exit;
  if Visible and HandleAllocated then
    if (FirstLine = -1) and (LastLine = -1) then begin
      rcInval := ClientRect;
      rcInval.Left := fGutterWidth;
      if sfLinesChanging in fStateFlags then
        UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
      else begin
        {$IFDEF VerboseSynEditInvalidate}
        DebugLn(['TCustomSynEdit.InvalidateLines ALL ',dbgs(rcInval)]);
        {$ENDIF}
        InvalidateRect(Handle, @rcInval, FALSE);
      end;
    end else begin
      { find the visible lines first }
      if LastLine >= 0 then begin
        if (LastLine < FirstLine) then SwapInt(LastLine, FirstLine);
        l := RowToScreenRow(Min(LastLine, ScreenRowToRow(LinesInWindow)))+1;
      end
      else
        l := LinesInWindow + 1;
      f := RowToScreenRow(Max(FirstLine, TopLine));
      { any line visible? }
      if (l >= f) then begin
        If LastLine < 0 then LastLine := ScreenRowToRow(LinesInWindow + 1);
        rcInval := Rect(fGutterWidth, fTextHeight * f,
          ClientWidth-ScrollBarWidth, fTextHeight * l);
        if sfLinesChanging in fStateFlags then
          UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
        else begin
          {$IFDEF VerboseSynEditInvalidate}
          DebugLn(['TCustomSynEdit.InvalidateLines PART ',dbgs(rcInval)]);
          {$ENDIF}
          InvalidateRect(Handle, @rcInval, FALSE);
        end;
      end;
    end;
end;

procedure TCustomSynEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Data: pointer;
  C: char;
  Cmd: TSynEditorCommand;
  {$IFDEF SYN_LAZARUS}
  IsStartOfCombo: boolean;
  {$ENDIF}
begin
  {$IFDEF VerboseKeys}
  DebugLn('[TCustomSynEdit.KeyDown] ',dbgs(Key),' ',dbgs(Shift));
  {$ENDIF}
  inherited;
  {$IFDEF SYN_LAZARUS}
  if fLastControlIsPressed<>(GetKeyShiftState=[SYNEDIT_LINK_MODIFIER]) then
    UpdateCtrlMouse;
  {$ENDIF}
  Data := nil;
  C := #0;
  try
    Cmd := TranslateKeyCode(Key, Shift, Data {$IFDEF SYN_LAZARUS},IsStartOfCombo{$ENDIF});
    if Cmd <> ecNone then begin
      {$IFDEF SYN_LAZARUS}
      LastMouseCaret:=Point(-1,-1);
      {$ENDIF}
      //DebugLn(['[TCustomSynEdit.KeyDown] key translated ',cmd]);
      Key := 0; // eat it.
      Include(fStateFlags, sfIgnoreNextChar);
      CommandProcessor(Cmd, C, Data);
    {$IFDEF SYN_LAZARUS}
    end else if IsStartOfCombo then begin
      // this key could be the start of a two-key-combo shortcut
      Key := 0; // eat it.
      Include(fStateFlags, sfIgnoreNextChar);
    {$ENDIF}
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
  if fLastControlIsPressed<>(GetKeyShiftState=[SYNEDIT_LINK_MODIFIER]) then
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
  if Key='' then exit;
  // don't fire the event if key is to be ignored
  if not (sfIgnoreNextChar in fStateFlags) then begin
    if Assigned(OnUTF8KeyPress) then OnUTF8KeyPress(Self, Key);
    // The key will be handled in UTFKeyPress always and KeyPress won't be called
    // so we we fire the OnKeyPress here
    if (ord(key[1])< %11000000) and (key[1]<>#0) and Assigned(OnKeyPress) then
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
  {$IFDEF SYN_LAZARUS}
  if Key=#0 then exit;
  {$ENDIF}
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
    {$IFDEF VerboseSynEditInvalidate}
    DebugLn(['TCustomSynEdit.LinesChanged ',dbgs(fInvalidateRect)]);
    {$ENDIF}
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
  fLastCaretX := CaretX;                                                       //mh 2000-10-19
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
        if (eoAltSetsColumnMode in Options) and (ssAlt in Shift) then
          FBlockSelection.ActiveSelectionMode := smColumn
      end;
    end;
    {$IFDEF SYN_LAZARUS}
    if (Button=mbMiddle)
    and ([sfDblClicked,sfTripleClicked,sfQuadClicked]*fStateFlags=[])
    and ((PrimarySelText<>'') or SelAvail)
    then begin
      FBlockSelection.StartLineBytePos := LogCaretXY;
      FBlockSelection.EndLineBytePos := LogCaretXY;
      //debugln('TCustomSynEdit.MouseDown Old SelText="',DbgStr(SelText),'" fBlockBegin=',dbgs(fBlockBegin),' fBlockEnd=',dbgs(fBlockEnd),' LogCaretXY=',dbgs(LogCaretXY));
      SelText:=PrimarySelText;
      //debugln('TCustomSynEdit.MouseDown New SelText="',DbgStr(SelText),'" fBlockBegin=',dbgs(fBlockBegin),' fBlockEnd=',dbgs(fBlockEnd),' LogCaretXY=',dbgs(LogCaretXY));
    end;
    {$ENDIF}
  end;
  {$IFDEF SYN_LAZARUS}
  if (X < fGutterWidth) and (Button=mbLeft) then begin
    Include(fStateFlags, sfPossibleGutterClick);
    fGutter.DoOnGutterClick(X, Y);
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
    if (Cursor <> crHandPoint) or (not (SYNEDIT_LINK_MODIFIER in Shift)) then
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
      {$IFDEF SYN_LAZARUS}
      Include(fStateFlags, sfIsDragging);
      {$ENDIF}
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
    {$IFDEF SYN_LAZARUS}
    if (not(sfIsDragging in fStateFlags))
    then
    {$ENDIF}
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
  end else if MouseCapture
  and (not(sfIsDragging in fStateFlags))
  then begin
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
      {$IFDEF SYN_LAZARUS}
      if (not(sfIsDragging in fStateFlags))
      then
      {$ENDIF}
      SetBlockEnd({$IFDEF SYN_LAZARUS}PhysicalToLogicalPos(CaretXY)
                  {$ELSE}CaretXY{$ENDIF});
    end;
    if fScrollDeltaY <> 0 then begin
      {$IFDEF SYN_LAZARUS}
      if GetKeyState(VK_SHIFT) < 0 then
        TopView := TopView + fScrollDeltaY * LinesInWindow
      else
        TopView := TopView + fScrollDeltaY;
      if fScrollDeltaY > 0
      then Y := fTextView.TextIndex[LinesInWindow-1]+1  // scrolling down
      else Y := TopLine;  // scrolling up
      {$ELSE}
      if GetKeyState(VK_SHIFT) < 0 then
        TopLine := TopLine + fScrollDeltaY * LinesInWindow
      else
        TopLine := TopLine + fScrollDeltaY;
      Y := TopLine;
      if fScrollDeltaY > 0 then  // scrolling down?
        Inc(Y, LinesInWindow - 1);
      {$ENDIF}
      CaretXY := Point(C.X, Y);
      {$IFDEF SYN_LAZARUS}
      if (not(sfIsDragging in fStateFlags))
      then
      {$ENDIF}
      SetBlockEnd({$IFDEF SYN_LAZARUS}PhysicalToLogicalPos(CaretXY)
                  {$ELSE}CaretXY{$ENDIF});
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
{$IFDEF SYN_LAZARUS}
var
  wasDragging : Boolean;
{$ENDIF}
begin
//DebugLn('TCustomSynEdit.MouseUp Mouse=',X,',',Y,' Caret=',CaretX,',',CaretY,', BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
  {$IFDEF SYN_LAZARUS}
  wasDragging := (sfIsDragging in fStateFlags);
  Exclude(fStateFlags, sfIsDragging);
  {$ENDIF}
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
    fGutter.DoOnGutterClick(X, Y);
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

  {$IFDEF SYN_LAZARUS}
    if (eoShowCtrlMouseLinks in Options)
    and not(wasDragging)
    and (Button=mbLeft) and (SYNEDIT_LINK_MODIFIER in Shift)
    and assigned(FOnClickLink)
    then begin
      FOnClickLink(Self, Button, Shift, X,Y);;
    end;
  {$ENDIF}
  //DebugLn('TCustomSynEdit.MouseUp END Mouse=',X,',',Y,' Caret=',CaretX,',',CaretY,', BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
end;

procedure TCustomSynEdit.Paint;
var
  rcClip, rcDraw: TRect;
  nL1, nL2, nC1, nC2: integer;
begin
  // Get the invalidated rect. Compute the invalid area in lines / columns.
  {$IFDEF SYN_LAZARUS}
    {$IFDEF EnableDoubleBuf}
    //rcClip:=Rect(0,0,ClientWidth,ClientHeight);
    rcClip := Canvas.ClipRect;
    StartPaintBuffer(rcClip);
    {$ELSE}
    rcClip := Canvas.ClipRect;
    //DebugLn(['TCustomSynEdit.Paint rcClip=',dbgs(rcClip)]);
    {$ENDIF}
  Include(fStateFlags,sfPainting);
  {$ELSE}
  rcClip := Canvas.ClipRect;
  {$ENDIF}
  // columns
  nC1 := LeftChar;
  if (rcClip.Left > fGutterWidth + 2) then
    Inc(nC1, (rcClip.Left - fGutterWidth - 2) div CharWidth);
  nC2 := {$IFDEF SYN_LAZARUS}LeftChar{$ELSE}nC1{$ENDIF} +
    (rcClip.Right - fGutterWidth - 2 + CharWidth - 1) div CharWidth;
  // lines
  nL1 := Max({$IFDEF SYN_LAZARUS}
             rcClip.Top div fTextHeight, 0
             {$ELSE}
             TopLine + rcClip.Top div fTextHeight, TopLine
             {$ENDIF}
             );
  nL2 := Min({$IFDEF SYN_LAZARUS}
             (rcClip.Bottom-1) div fTextHeight, fTextView.Count - fTextView.TopLine
             {$ELSE}
             TopLine + (rcClip.Bottom + fTextHeight - 1) div fTextHeight, Lines.Count
             {$ENDIF}
             );
  //DebugLn('TCustomSynEdit.Paint LinesInWindow=',dbgs(LinesInWindow),' nL1=',dbgs(nL1),' nL2=',dbgs(nL2));
  // Now paint everything while the caret is hidden.
  HideCaret;
  try
    // First paint the gutter area if it was (partly) invalidated.
    if (rcClip.Left < fGutterWidth) then begin
      rcDraw := rcClip;
      rcDraw.Right := fGutterWidth;
      fGutter.Paint(Canvas, rcDraw, nL1, nL2);
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
      {$IFDEF EnableDoubleBuf}
    EndPaintBuffer(rcClip);
      {$ENDIF}
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
begin
  if (iLine<=0) or (iLine>Lines.Count) then exit;
  dec(iLine);
//DebugLn(['****** FoldAction at ',iLine,' scrline=',fTextView.TextIndexToScreenLine(iLine), ' type ', SynEditCodeFoldTypeNames[fTextView.FoldType[fTextView.TextIndexToScreenLine(iLine)]],  '  view topline=',fTextView.TopLine  ]);
  case fTextView.FoldType[fTextView.TextIndexToScreenLine(iLine)] of
    cfCollapsed : fTextView.UnFoldAtTextIndex(iLine);
    cfExpanded  : fTextView.FoldAtTextIndex(iLine);
  end;
end;

function TCustomSynEdit.FindNextUnfoldedLine(iLine: integer; Down: boolean
  ): Integer;
// iLine is 1 based
begin
  Result:=iLine;
  while (Result>0) and (Result<=Lines.Count)
  and (fTextView.FoldedAtTextIndex[Result-1]) do
    if Down then inc(Result) else dec(Result);
end;

procedure TCustomSynEdit.UnfoldAll;
begin
  fTextView.UnfoldAll;
  Invalidate;
end;

procedure TCustomSynEdit.FoldAll(StartLevel : Integer = 0; IgnoreNested : Boolean = False);
begin
  fTextView.FoldAll(StartLevel, IgnoreNested);
  Invalidate;
end;
{$ENDIF}

procedure TCustomSynEdit.PaintTextLines(AClip: TRect; FirstLine, LastLine,
  FirstCol, LastCol: integer);
{$IFDEF SYN_LAZARUS}
// FirstLine, LastLine are based 1
// FirstCol, LastCol are screen based 1 without scrolling (physical position).
//  i.e. the real screen position is fTextOffset+Pred(FirstCol)*CharWidth
var
  bDoRightEdge: boolean; // right edge
  nRightEdge: integer;
  colEditorBG: TColor;
    // painting the background and the text
  rcLine, rcToken: TRect;
  CurLine: integer;         // line index for the loop
  CurPhysPos : Integer; // Physical Start Position of next token in current Line
  TokenAccu: record
    Len, MaxLen: integer;
    PhysicalStartPos, PhysicalEndPos: integer;
    p: PChar;
    FG, BG, FC: TColor;
    Style: TFontStyles;
  end;
  dc: HDC;

  ExpandedPaintToken: string; // used to create the string sent to TextDrawer

{ local procedures }

  procedure SetTokenAccuLength;
  begin
    ReAllocMem(TokenAccu.p,TokenAccu.MaxLen+1);
    TokenAccu.p[TokenAccu.MaxLen]:=#0;
  end;

  procedure ExpandSpecialChars(var p: PChar; var Count: integer;
    PhysicalStartPos: integer);
  // if there are no tabs or special chars: keep p and Count untouched
  // if there are special chars: copy p into ExpandedPaintToken buffer,
  //                             convert tabs to spaces, and return the buffer
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
    Special: boolean;
  begin
    TabCount:=0;
    for i:=0 to Count-1 do
      if p[i]=#9 then inc(TabCount);
    Special:=eoShowSpecialChars in Options;
    if (not Special) and (TabCount=0)
    and (FindInvalidUTF8Character(p,Count)<0) then
      exit;
    LengthNeeded:=(Count+TabCount*TabWidth);
    if Special then LengthNeeded:=LengthNeeded*2;
    if length(ExpandedPaintToken)<LengthNeeded then
      SetLength(ExpandedPaintToken,LengthNeeded+CharsInWindow);
    //DebugLn(['ExpandSpecialChars Count=',Count,' TabCount=',TabCount,' Special=',Special,' LengthNeeded=',LengthNeeded]);
    SrcPos:=0;
    DestPos:=0;
    ScreenPos:=PhysicalStartPos;
    Dest:=PChar(Pointer(ExpandedPaintToken));
    if UseUTF8 then begin
      while SrcPos<Count do begin
        c:=p[SrcPos];
        case c of
        #128..#191:
          begin
            // non UTF-8 character
            Dest[DestPos]:='?';
            inc(DestPos);
            inc(SrcPos);
            inc(ScreenPos);
          end;

        #192..#255:
          begin
            // could be UTF8 char
            CharLen:=UTF8CharacterStrictLength(@p[SrcPos]);
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
            end;
            inc(ScreenPos);
          end;

        #9:
          begin
            // tab char
            SpaceCount:=TabWidth - ((ScreenPos-1) mod TabWidth);
            //debugln('ExpandSpecialChars SpaceCount=',dbgs(SpaceCount),' TabWidth=',dbgs(TabWidth),' ScreenPos=',dbgs(ScreenPos));
            if not Special then begin
              for i:=1 to SpaceCount do begin
                Dest[DestPos]:=FTabChar;
                inc(DestPos);
                inc(ScreenPos);
              end;
            end else begin
              for i:=1 to SpaceCount do begin
                // #194#187 looks like >>
                Dest[DestPos]:=#194;
                inc(DestPos);
                Dest[DestPos]:=#187;
                inc(DestPos);
                inc(ScreenPos);
              end;
            end;
            inc(SrcPos);
          end;

        #32:
          // space
          if not Special then begin
            // normal space
            Dest[DestPos]:=p[SrcPos];
            inc(DestPos);
            inc(SrcPos);
            inc(ScreenPos);
          end else begin
            // #194#183 looks like .
            Dest[DestPos]:=#194;
            inc(DestPos);
            Dest[DestPos]:=#183;
            inc(DestPos);
            inc(SrcPos);
            inc(ScreenPos);
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
    end else begin
      // non UTF-8
      while SrcPos<Count do begin
        c:=p[SrcPos];
        case c of
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
    end;
    p:=PChar(Pointer(ExpandedPaintToken));
    Count:=DestPos;
    //debugln('ExpandSpecialChars Token with Tabs: "',DbgStr(copy(ExpandedPaintToken,1,Count)),'"');
  end;

  procedure PaintToken(Token: PChar; TokenLen, FirstPhysical: integer);
  // FirstPhysical is the physical (screen without scrolling)
  // column of the first character
  var
    nX: integer;
  const
    ETOOptions = ETO_OPAQUE; // Note: clipping is slow and not needed
  begin
    {debugln('PaintToken A TokenLen=',dbgs(TokenLen),
      ' FirstPhysical=',dbgs(FirstPhysical),
      ' Tok="'+copy(Token, 1, TokenLen),'"',
      ' rcToken='+dbgs(rcToken.Left)+'-'+dbgs(rcToken.Right));}
    if (rcToken.Right <= rcToken.Left) then exit;
    // Draw the right edge under the text if necessary
    nX := ScreenColumnToXValue(FirstPhysical); // == rcToken.Left
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
        Token, TokenLen);
    end else begin
      // draw text with background
      //debugln('PaintToken nX=',dbgs(nX),' Token=',dbgstr(copy(Token,1, TokenLen)),' rcToken=',dbgs(rcToken));
      fTextDrawer.ExtTextOut(nX, rcToken.Top, ETOOptions, rcToken,
        Token, TokenLen);
    end;
    rcToken.Left := rcToken.Right;
  end;

  procedure PaintHighlightToken(bFillToEOL: boolean);
  var
    nX1, eolx: integer;
    NextPos : Integer;
    MarkupInfo : TSynSelectedColor;
  begin
    {debugln('PaintHighlightToken A TokenAccu: Len=',dbgs(TokenAccu.Len),
      ' PhysicalStartPos=',dbgs(TokenAccu.PhysicalStartPos),
      ' PhysicalEndPos=',dbgs(TokenAccu.PhysicalEndPos),
      ' "',copy(TokenAccu.p,1,TokenAccu.Len),'"');}

    // Any token chars accumulated?
    if (TokenAccu.Len > 0) then begin
      // Initialize the colors and the font style.
      with fTextDrawer do begin
        SetBackColor(TokenAccu.BG);
        SetForeColor(TokenAccu.FG);
        SetFrameColor(TokenAccu.FC);
        SetStyle(TokenAccu.Style);
      end;
      // Paint the chars
      rcToken.Right := ScreenColumnToXValue(TokenAccu.PhysicalEndPos+1);
      with TokenAccu do PaintToken(p, Len, PhysicalStartPos);
    end;

    // Fill the background to the end of this line if necessary.
    if bFillToEOL and (rcToken.Left < rcLine.Right) then begin
      eolx := rcToken.Left; // remeber end of actual line, so we can decide to draw the right edge
      NextPos := Min(LastCol, TokenAccu.PhysicalEndPos+1);
      Repeat
        MarkupInfo := fMarkupManager.GetMarkupAttributeAtRowCol(fTextView.TextIndex[CurLine]+1, NextPos);
        NextPos := fMarkupManager.GetNextMarkupColAfterRowCol(fTextView.TextIndex[CurLine]+1, NextPos);

        with fTextDrawer do
          if MarkupInfo = nil
          then begin
            SetBackColor(colEditorBG);
            //SetForeColor(TokenAccu.FG); // for underline
            //SetStyle(TokenAccu.Style);
          end
          else begin
            SetBackColor(MarkupInfo.Background);
            //SetForeColor(TokenAccu.FG);
            //SetStyle(TokenAccu.Style);
          end;

        if NextPos < 1
        then nX1 := rcLine.Right
        else begin
          nX1 := ScreenColumnToXValue(NextPos);
          if nX1 > rcLine.Right
          then nX1 := rcLine.Right;
        end;

        rcToken.Right := nX1;
        InternalFillRect(dc, rcToken); {TODO: if style underline, then print spaces}
        rcToken.Left := nX1;
      until nX1 >= rcLine.Right;

      // Draw the right edge if necessary.
      if bDoRightEdge and (not (eoHideRightMargin in Options))
      and (nRightEdge >= eolx) then begin // xx rc Token
        LCLIntf.MoveToEx(dc, nRightEdge, rcToken.Top, nil);
        LCLIntf.LineTo(dc, nRightEdge, rcToken.Bottom + 1);
      end;
    end;
  end;

  procedure AddHighlightToken(
    Token: PChar;
    TokenLen, PhysicalStartPos, PhysicalEndPos: integer;
    Foreground, Background, FrameColor: TColor;
    Style: TFontStyles);
  var
    bCanAppend: boolean;
    bSpacesTest, bIsSpaces: boolean;
    i: integer;

    function TokenIsSpaces: boolean;
    var
      pTok: PChar;
      Cnt: Integer;
    begin
      if not bSpacesTest then begin
        bSpacesTest := TRUE;
        bIsSpaces := TRUE;
        pTok := PChar(Pointer(Token));
        Cnt := TokenLen;
        while bIsSpaces and (Cnt > 0) do begin
          if not (pTok^ in [' ',#9])
          then bIsSpaces := False;
          Inc(pTok);
          dec(Cnt);
        end;
      end;
      Result := bIsSpaces;
    end;

  begin
    {DebugLn('AddHighlightToken A TokenLen=',dbgs(TokenLen),
      ' PhysicalStartPos=',dbgs(PhysicalStartPos),' PhysicalEndPos=',dbgs(PhysicalEndPos),
      ' Tok="',copy(Token,1,TokenLen),'"');}

    // Do we have to paint the old chars first, or can we just append?
    bCanAppend := FALSE;
    bSpacesTest := FALSE;
    if (TokenAccu.Len > 0) then
    begin
      // font style must be the same or token is only spaces
      if (
           (TokenAccu.Style = Style) or
           ( not (fsUnderline in Style) and
             not (fsUnderline in TokenAccu.Style) and
             not (eoShowSpecialChars in fOptions) and TokenIsSpaces
           )
         )
      // background color must be the same and
      // frame color must be the same and
      // foreground color must be the same or token is only spaces
      and (
            ( (TokenAccu.BG = Background) and
              ((TokenAccu.FC = FrameColor) and (TokenAccu.FC = clNone)) and
              (
                (TokenAccu.FG = Foreground) or
                (not (eoShowSpecialChars in fOptions) and TokenIsSpaces)
              )
            )
          )
      then
        bCanAppend := TRUE;
      // If we can't append it, then we have to paint the old token chars first.
      if not bCanAppend then
        PaintHighlightToken(FALSE);
    end;
    // Don't use AppendStr because it's more expensive.
    //if (CurLine=TopLine) then debugln('      -t-Accu len ',dbgs(TokenAccu.Len),' pstart ',dbgs(TokenAccu.PhysicalStartPos),' p-end ',dbgs(TokenAccu.PhysicalEndPos));
    if bCanAppend then begin
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
      TokenAccu.FG := Foreground;
      TokenAccu.BG := Background;
      TokenAccu.FC := FrameColor;
      TokenAccu.Style := Style;
    end;
    {debugln('AddHighlightToken END bCanAppend=',dbgs(bCanAppend),
      ' Len=',dbgs(TokenAccu.Len),
      ' PhysicalStartPos=',dbgs(TokenAccu.PhysicalStartPos),
      ' PhysicalEndPos=',dbgs(TokenAccu.PhysicalEndPos),
      ' "',copy(TokenAccu.s,1,TokenAccu.Len),'"');}
  end;

  procedure DrawHiLightMarkupToken(attr: TSynHighlighterAttributes;
    sToken: PChar; nTokenByteLen: integer);
  var
    DefaultFGCol, DefaultBGCol, DefaultFCCol: TColor;
    DefaultStyle: TFontStyles;
    BG, FG, FC : TColor;
    Style: TFontStyles;
    PhysicalStartPos: integer;
    PhysicalEndPos: integer;
    len: Integer;
    SubTokenByteLen, SubCharLen, TokenCharLen : Integer;
    NextPhysPos : Integer;
    MarkupInfo : TSynSelectedColor;
  begin
    if CurPhysPos > LastCol then exit;

    PhysicalStartPos := CurPhysPos;
    ExpandSpecialChars(sToken, nTokenByteLen, PhysicalStartPos);
    TokenCharLen := UTF8Length(sToken, nTokenByteLen);
    // Prepare position for next token
    inc(CurPhysPos, TokenCharLen);
    if CurPhysPos <= FirstCol then exit;

    // Remove any Part of the Token that is before FirstCol
    if PhysicalStartPos < FirstCol then begin
      SubCharLen := FirstCol - PhysicalStartPos;
      len := UTF8CharToByteIndex(sToken, nTokenByteLen, SubCharLen);
      if len < 0 then begin
        debugln('ERROR: Could not find PhysStart in token (maybe invalid UTF8?',' len ',dbgs(nTokenByteLen),' Line ',dbgs(CurLine),' PhysPos ',dbgs(CurPhysPos));
        exit;
      end;
      dec(TokenCharLen, SubCharLen);
      inc(PhysicalStartPos, SubCharLen);
      dec(nTokenByteLen, len);
      inc(sToken, len);
    end;

    // Remove any Part of the Token that is after LastCol
    SubCharLen := PhysicalStartPos + TokenCharLen - (LastCol + 1);
    if SubCharLen > 0 then begin
      dec(TokenCharLen, SubCharLen);
      nTokenByteLen := UTF8CharToByteIndex(sToken, nTokenByteLen, TokenCharLen);
      if nTokenByteLen < 0 then begin
        debugln('ERROR: Could not find PhysEnd in token (maybe invalid UTF8?',' len ',dbgs(nTokenByteLen),' Line ',dbgs(CurLine),' PhysPos ',dbgs(CurPhysPos));
        exit;
      end;
    end;

    if Assigned(attr) then
    begin
      DefaultFGCol := attr.Foreground;
      DefaultBGCol := attr.Background;
      DefaultFCCol := attr.FrameColor;
      DefaultStyle := attr.Style;
      if DefaultBGCol = clNone then DefaultBGCol := colEditorBG;
      if DefaultFGCol = clNone then DefaultFGCol := Font.Color;
    end else
    begin
      DefaultFGCol := Font.Color;
      DefaultBGCol := colEditorBG;
      DefaultFCCol := clNone;
      DefaultStyle := Font.Style;
    end;

    {TODO: cache NextPhysPos, and MarkupInfo between 2 calls }
    while (nTokenByteLen > 0) do begin
      // Calculate Token Sublen for current Markup
      NextPhysPos := fMarkupManager.GetNextMarkupColAfterRowCol(fTextView.TextIndex[CurLine]+1, PhysicalStartPos);
      if NextPhysPos < 1
      then SubCharLen := TokenCharLen
      else SubCharLen := NextPhysPos - PhysicalStartPos;

      if SubCharLen > TokenCharLen then SubCharLen := TokenCharLen;
      if SubCharLen < 1 then begin // safety for broken input...
        debugln('ERROR: Got invalid SubCharLen ',dbgs(SubCharLen),' len ',dbgs(nTokenByteLen),' Line ',dbgs(CurLine),' PhysPos ',dbgs(CurPhysPos));
        SubCharLen:=1;
      end;

      SubTokenByteLen := UTF8CharToByteIndex(sToken,nTokenByteLen,SubCharLen);
      if SubTokenByteLen < 0 then begin
        debugln('ERROR: Can not find pso in SubToken ',dbgs(SubCharLen),' len ',dbgs(nTokenByteLen),' Line ',dbgs(CurLine),' PhysPos ',dbgs(CurPhysPos));
        SubTokenByteLen := nTokenByteLen; // Draw the rest
      end;
      PhysicalEndPos:= PhysicalStartPos + SubCharLen - 1;

      // Calculate Markup
      BG := DefaultBGCol;
      FG := DefaultFGCol;
      FC := DefaultFCCol;
      Style := DefaultStyle;
      MarkupInfo := fMarkupManager.GetMarkupAttributeAtRowCol(fTextView.TextIndex[CurLine]+1, PhysicalStartPos);
      if assigned(MarkupInfo)
      then MarkupInfo.ModifyColors(FG, BG, FC, Style);
      // Deal with equal colors
      if (BG = FG) then begin // or if diff(gb,fg) < x
        if BG = DefaultBGCol
        then FG := not(BG) and $00ffffff // or maybe Font.color ?
        else FG := DefaultBGCol;
      end;

      // Add to TokenAccu
      AddHighlightToken(sToken, SubTokenByteLen,
        PhysicalStartPos, PhysicalEndPos, FG, BG, FC, Style);

      PhysicalStartPos:=PhysicalEndPos + 1;
      dec(nTokenByteLen,SubTokenByteLen);
      dec(TokenCharLen, SubCharLen);
      inc(sToken, SubTokenByteLen);
    end;
  end;

  procedure PaintLines;
  var
    sLine: string; // the current line
    sToken: PChar; // highlighter token info
    nTokenLen: integer; // Pos in Char // Len in Byte ??
    attr: TSynHighlighterAttributes;
    ypos: Integer;
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

      fMarkupManager.PrepareMarkupForRow(fTextView.TextIndex[CurLine]+1);
      // Get the line.
      sLine := fTextView[CurLine];
      // Update the rcLine rect to this line.
      rcLine.Top := rcLine.Bottom;
      Inc(rcLine.Bottom, fTextHeight);
      // Paint the lines depending on the assigned highlighter.
      rcToken := rcLine;
      TokenAccu.Len := 0;
      TokenAccu.PhysicalEndPos := FirstCol - 1; // in case of an empty line
      CurPhysPos := 1;

      if not Assigned(fHighlighter) then begin
        DrawHiLightMarkupToken(nil, PChar(Pointer(sLine)), Length(sLine));
      end else begin
        // Initialize highlighter with line text and range info. It is
        // necessary because we probably did not scan to the end of the last
        // line - the internal highlighter range might be wrong.
        fHighlighter.SetRange(fTextView.Ranges[CurLine]);     //mh 2000-10-10
        fHighlighter.SetLine(sLine, fTextView.TextIndex[CurLine]);
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

      fMarkupManager.FinishMarkupForRow(fTextView.TextIndex[CurLine]+1);

      // codefold draw splitter line
      if Gutter.ShowCodeFolding
      and (fTextView.DrawDivider[curLine]) then
      begin
        ypos := rcToken.Bottom - 1;
        LCLIntf.MoveToEx(dc, nRightEdge, ypos, nil);
        LCLIntf.LineTo(dc, fGutterWidth - 1, ypos);
      end;
    end;
    CurLine:=-1;
  end;

  procedure CalculateCtrlMouseLink;
  begin
    fLastCtrlMouseLinkY:=-1;
    fMarkupCtrlMouse.CtrlMouseLine:=-1;
    if (not (eoShowCtrlMouseLinks in Options))
    or (fLastMouseCaret.X<1) or (fLastMouseCaret.Y<1)
    or (not fLastControlIsPressed) then
      exit;
    GetWordBoundsAtRowCol(PhysicalToLogicalPos(fLastMouseCaret),
                          fLastCtrlMouseLinkX1,fLastCtrlMouseLinkX2);
    if
      not IsLinkable(
        fLastMouseCaret.Y, fLastCtrlMouseLinkX1, fLastCtrlMouseLinkX2)
    then
      exit;
    fLastCtrlMouseLinkY:=fLastMouseCaret.Y;
    with fMarkupCtrlMouse do begin
      CtrlMouseLine := fLastCtrlMouseLinkY;
      CtrlMouseX1 := fLastCtrlMouseLinkX1;
      CtrlMouseX2 := fLastCtrlMouseLinkX2;
    end;
  end;

{ end local procedures }

var
  ypos : integer;
  ColBG : TColor;
begin
  if (AClip.Right < fGutterWidth) then exit;
  //DebugLn(['TCustomSynEdit.PaintTextLines ',dbgs(AClip)]);
  CurLine:=-1;
  FillChar(TokenAccu,SizeOf(TokenAccu),0);
  //DebugLn('TCustomSynEdit.PaintTextLines ',DbgSName(Self),' TopLine=',dbgs(TopLine),' AClip=',dbgs(AClip));
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
  end;
  Canvas.Pen.Color := fRightEdgeColor; // used for code folding too
  Canvas.Pen.Width := 1;
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
    CalculateCtrlMouseLink;
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

  // If there is anything visible below the last line, then fill this as well.
  rcToken := AClip;
  rcToken.Top := (LastLine+1) * fTextHeight;
  if (rcToken.Top < rcToken.Bottom) then begin
    SetBkColor(dc, ColorToRGB(colEditorBG));
    InternalFillRect(dc, rcToken);
    // Draw the right edge if necessary.
    if bDoRightEdge and (not (eoHideRightMargin in Options)) then begin
      LCLIntf.MoveToEx(dc, nRightEdge, rcToken.Top, nil);
      LCLIntf.LineTo(dc, nRightEdge, rcToken.Bottom + 1);
    end;

    // codefold draw splitter line
    if Gutter.ShowCodeFolding
    and (fTextView.DrawDivider[LastLine]) then
    begin
      ypos := rcToken.Bottom - 1;
      LCLIntf.MoveToEx(dc, nRightEdge, ypos, nil);
      LCLIntf.LineTo(dc, fGutterWidth, ypos);
    end;
  end;

  fMarkupManager.EndMarkup;
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
{$IFDEF SYN_LAZARUS}
  MemStream: TMemoryStream;
  Buf: Pointer;
  BufSize: integer;
{$ELSE}
  Mem: HGLOBAL;
{$ENDIF}
  PasteMode: TSynSelectionMode;
  P: PChar;
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
          // Our format: SelectionMode value followed by text.
          // See CopyToClipboard
          PasteMode := PSynSelectionMode(P)^;
          inc(P, SizeOf(TSynSelectionMode));
          SetSelTextPrimitive(PasteMode, P, true, crPaste);
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
      SelText := Clipboard.AsText;
    end;
  finally
    EndUndoBlock;                                                               //mh 2000-11-20
  end;
  EnsureCursorPosVisible;
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
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.SetHighlightSearch(const ASearch : String; AOptions : TSynSearchOptions);
begin
  fMarkupHighAll.SearchOptions := AOptions;
  fMarkupHighAll.SearchString := ASearch;
end;

{$ENDIF}

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

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.SetBlockIndent(const AValue: integer);
begin
  if fBlockIndent=AValue then exit;
  fBlockIndent:=AValue;
end;

function TCustomSynEdit.GetCaretX : Integer;
begin
  Result:= fCaret.CharPos;
end;

function TCustomSynEdit.GetCaretY : Integer;
begin
  Result:= fCaret.LinePos;
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
    fLastCaretX := CaretX;
  end;
  SetCaretXY(Point(fLastCaretX{CaretX}, Value));                                //mh 2000-10-19
end;

function TCustomSynEdit.GetCaretXY: TPoint;
begin
  Result := Point(CaretX, CaretY);
end;

function TCustomSynEdit.GetCodeFoldingTreeColor: TSynSelectedColor;
begin
  Result := fGutter.MarkupInfoCodeFoldingTree;
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
  if (Value.X <> CaretX) or (Value.Y <> CaretY) then begin
    IncPaintLock;
    try
      // simply include the flags, fPaintLock is > 0
      if CaretX <> Value.X then begin
        {$IFNDEF SYN_LAZARUS}
        fCaretX := Value.X;
        {$ENDIF}
        Include(fStatusChanges, scCaretX);
      end;
      if CaretY <> Value.Y then begin
        {$IFDEF SYN_LAZARUS}
        InvalidateGutterLines(CaretY, CaretY);
        InvalidateGutterLines(Value.Y, Value.Y);
        {$ELSE}
        fCaretY := Value.Y;
        {$ENDIF}
        Include(fStatusChanges, scCaretY);
      end;
      {$IFDEF SYN_LAZARUS}
      fCaret.LineCharPos:= Value;
      fMarkupManager.Caret := Value;
      {$ENDIF}
      EnsureCursorPosVisible;
      Include(fStateFlags, sfCaretChanged);
      {$IFNDEF SYN_LAZARUS}
      Include(fStateFlags, sfScrollbarChanged);
      {$ENDIF}
    finally
      DecPaintLock;
    end;
  end;
  {$IFDEF SYN_LAZARUS}
  fLastCaretX:=CaretX;
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
  Save := SelectObject(DC, Value.Reference.Handle);
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
          Quality := Value.Quality;
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

procedure TCustomSynEdit.SetLeftChar(Value: Integer);
{begin}                                                                         //mh 2000-10-19
var
  MaxVal: integer;
begin
  if eoScrollPastEol in Options then
    MaxVal := fMaxLeftChar
  else
    MaxVal :=
    {$IFDEF SYN_LAZARUS}TSynEditStrings{$ELSE}TSynEditStringList{$ENDIF}
      (Lines).LengthOfLongestLine;
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

procedure TCustomSynEdit.SetScrollBars(const Value: TScrollStyle);
begin
  if (FScrollBars <> Value) then begin
    FScrollBars := Value;
    {$IFNDEF SYN_LAZARUS}
    RecreateWnd(Self)
    {$ENDIF};
    UpdateScrollBars;
    Invalidate;
  end;
end;

{$IFNDEF SYN_LAZARUS}
procedure TCustomSynEdit.SetSelText(const Value: string);
begin
  // No undo entry added
  SetSelTextPrimitive(smNormal, PChar(Value));
end;
{$ENDIF}

procedure TCustomSynEdit.SetSelTextPrimitive(PasteMode: TSynSelectionMode;
  Value: PChar; AddToUndoList: Boolean = false;
  ChangeReason: TSynChangeReason = crInsert);
Begin
  IncPaintLock;
  TSynEditStringTrimmingList(fTrimLines).Lock;
  try
    FBlockSelection.SetSelTextPrimitive(PasteMode, Value, AddToUndoList,
                                        ChangeReason);
    // Force caret reset
    CaretXY := CaretXY;
    fLastCaretX := CaretX;
    Include(fStatusChanges, scCaretY);
    Include(fStatusChanges, scCaretX);
    EnsureCursorPosVisible;
  finally
    TSynEditStringTrimmingList(fTrimLines).UnLock;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.SetSelTextExternal(const Value: string);
begin
  // undo entry added
  BeginUndoBlock;
  TSynEditStringTrimmingList(fTrimLines).Lock;
  try
    FBlockSelection.SelText := Value;
    // Force caret reset
    CaretXY := CaretXY;
    fLastCaretX := CaretX;
    Include(fStatusChanges, scCaretY);
    Include(fStatusChanges, scCaretX);
    EnsureCursorPosVisible;
  finally
    TSynEditStringTrimmingList(fTrimLines).UnLock;
    EndUndoBlock;
  end;
end;

procedure TCustomSynEdit.SynSetText(const Value: string);
begin
  Lines.Text := Value;
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.RealSetText(const Value: TCaption);
begin
  Lines.Text := Value; // Do not trim
end;
{$ENDIF}

procedure TCustomSynEdit.SetTopLine(Value: Integer);
var
  Delta: Integer;
{$ifdef SYN_LAZARUS}
  OldTopLine: LongInt;
{$ENDIF}
begin
  // don't use MinMax here, it will fail in design mode (Lines.Count is zero,
  // but the painting code relies on TopLine >= 1)
  if (eoScrollPastEof in Options) then
    Value := Min(Value, Lines.Count)
  else
    {$ifdef SYN_LAZARUS}
    Value := Min(Value, fTextView.TextPosAddLines(Lines.Count+1, -fLinesInWindow));
    {$ELSE}
    Value := Min(Value, Lines.Count + 1 - fLinesInWindow);
    {$ENDIF}
  Value := Max(Value, 1);
  {$IFDEF SYN_LAZARUS}
  if fTextView.FoldedAtTextIndex[Value-1] then
    Value := FindNextUnfoldedLine(Value, False);
  {$ENDIF}
  if Value <> fTopLine then begin
{$ifdef SYN_LAZARUS}
    OldTopLine:=TopLine;
    fTopLine := Value;
    fTextView.TopTextIndex := Value-1;
    UpdateScrollBars;
    Delta := OldTopLine - TopLine;
    if (Abs(Delta) < fLinesInWindow) and not (sfPainting in fStateFlags)
    and not (fPaintLock <> 0) then
    begin
      // TODO: SW_SMOOTHSCROLL --> can't get it work
      if not ScrollWindowEx(Handle, 0, fTextHeight * Delta, nil, nil, 0, nil,
        SW_INVALIDATE) then
      begin
        // scrollwindow failed, invalidate all
        Invalidate;
      end;
    end
{$else}
    Delta := TopLine - Value;
    fTopLine := Value;
    UpdateScrollBars;
    if Abs(Delta) < fLinesInWindow then
    begin
      ScrollWindow(Handle, 0, fTextHeight * Delta, nil, nil);
    end
{$endif}
    else
      Invalidate;
    StatusChanged([scTopLine]);
  end;
{$ifdef SYN_LAZARUS}
  fMarkupManager.TopLine:= fTopLine;
{$endif}
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
// scroll to make the caret visible
var
  NewCaretXY: TPoint;
  MaxY: LongInt;
begin
  NewCaretXY:=CaretXY;
  if NewCaretXY.X < fLeftChar then
    NewCaretXY.X := fLeftChar
  else if NewCaretXY.X >= fLeftChar + fCharsInWindow then
    NewCaretXY.X := fLeftChar + fCharsInWindow - 1;
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
    CaretXY:=NewCaretXY;
  end;
end;

procedure TCustomSynEdit.MoveCaretIgnoreEOL(const NewCaret: TPoint);
var
  NewX: LongInt;
begin
  CaretXY:=NewCaret;
  NewX:=Max(1,Min(fMaxLeftChar,NewCaret.X));
  if CaretX<>NewX then begin
    IncPaintLock;
    CaretX:=NewX;
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
  then begin
    Include(fStateFlags, sfCaretChanged);
  end else begin
    Exclude(fStateFlags, sfCaretChanged);
    {$IFDEF SYN_LAZARUS}
    if eoAlwaysVisibleCaret in fOptions2 then
      MoveCaretToVisibleArea;
    {$ENDIF}
    CX := CaretXPix;
    CY := CaretYPix;
    if (CX >= fGutterWidth)
      and (CX < ClientWidth{$IFDEF SYN_LAZARUS}-ScrollBarWidth{$ENDIF})
      and (CY >= 0)
      and (CY <{=} ClientHeight{$IFDEF SYN_LAZARUS}-ScrollBarWidth{-fTextHeight}{$ENDIF})
    then begin
      SetCaretPosEx(Handle ,CX + FCaretOffset.X, CY + FCaretOffset.Y);
      //DebugLn(' [TCustomSynEdit.UpdateCaret] ShowCaret ',Name);
      ShowCaret;
    end else begin
      //DebugLn(' [TCustomSynEdit.UpdateCaret] HideCaret ',Name);
      HideCaret;
      SetCaretPosEx(Handle ,CX + FCaretOffset.X, CY + FCaretOffset.Y);
    end;
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
          {$IFDEF SYN_LAZARUS}
          ScrollInfo.nMax := TSynEditStrings(Lines).LengthOfLongestLine;
          {$ELSE}
          ScrollInfo.nMax := TSynEditStringList(Lines).LengthOfLongestLine;
          {$ENDIF}
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
        {$IFDEF SYN_LAZARUS}
        // tell interface to remove horizontal scrollbar
        ShowScrollBar(Handle, SB_HORZ, FALSE);
        {$ENDIF}
      end;
      if fScrollBars in [ssBoth, ssVertical] then begin
        nMaxScroll := {$IFDEF SYN_LAZARUS}fTextView.Count+1{$ELSE}Lines.Count{$ENDIF};
        if (eoScrollPastEof in Options) then
          Inc(nMaxScroll, LinesInWindow - 1);
{$IFNDEF SYN_LAZARUS}
        if nMaxScroll <= MAX_SCROLL then begin
{$ENDIF}
          ScrollInfo.nMax := Max(1, nMaxScroll);
          ScrollInfo.nPage := LinesInWindow;
          {$IFDEF SYN_LAZARUS}
          ScrollInfo.nPos := fTextView.TextIndexToViewPos(TopLine-1);
          {$ELSE}
          ScrollInfo.nPos := TopLine;
          {$ENDIF}
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
      end else begin
        {$IFDEF SYN_LAZARUS}
        // tell interface to remove vertical scrollbar
        ShowScrollBar(Handle, SB_Vert, FALSE);
        {$ENDIF}
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
{$IFDEF SYN_LAZARUS}
    SB_LINEDOWN: TopView := TopView + 1;
    SB_LINEUP: TopView := TopView - 1;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN: TopView := TopView
      + (fLinesInWindow - Ord(eoScrollByOneLess in fOptions));
    SB_PAGEUP: TopView := TopView
      - (fLinesInWindow - Ord(eoScrollByOneLess in fOptions));
{$ELSE}
    SB_LINEDOWN: TopLine := TopLine + 1;
    SB_LINEUP: TopLine := TopLine - 1;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN: TopLine := TopLine
      + (fLinesInWindow - Ord(eoScrollByOneLess in fOptions));
    SB_PAGEUP: TopLine := TopLine
      - (fLinesInWindow - Ord(eoScrollByOneLess in fOptions));
{$ENDIF}
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
          {$IFDEF SYN_LAZARUS}
          TopView := Msg.Pos;
          {$ELSE}
          TopLine := Msg.Pos;
          {$ENDIF}

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
// Index and AtLeastTilIndex are 0 based
{$IFDEF SYN_LAZARUS}
  var
    FixFStart: Integer;
    LastLineDiffers : Boolean;
  procedure SetCodeFoldAttributes;
  begin
    TSynEditStrings(Lines).FoldMinLevel[Result-1] := fHighlighter.MinimumCodeFoldBlockLevel;
    TSynEditStrings(Lines).FoldEndLevel[Result-1] := fHighlighter.CurrentCodeFoldBlockLevel;
    if (fHighlighter.LastLineCodeFoldLevelFix <> 0) and (result > 1) then begin
      TSynEditStrings(Lines).FoldEndLevel[Result-2] :=
        TSynEditStrings(Lines).FoldEndLevel[Result-2] + fHighlighter.LastLineCodeFoldLevelFix;
      if TSynEditStrings(Lines).FoldMinLevel[Result-2] > TSynEditStrings(Lines).FoldEndLevel[Result-2] then
        TSynEditStrings(Lines).FoldMinLevel[Result-2] := TSynEditStrings(Lines).FoldEndLevel[Result-2];
      if Result - 1 < FixFStart then FixFStart := Result - 1;
    end;
  end;
{$ENDIF}

begin
  {$IFDEF SYN_LAZARUS}
  if Index < 0 then Index := 0;
  {$ENDIF}
  Result := Index;
  {$IFDEF SYN_LAZARUS}
  if not assigned(fHighlighter) or (Index > Lines.Count - 1) then begin
    fTextView.FixFoldingAtTextIndex(Index);
    fMarkupManager.TextChangedScreen(Max(RowToScreenRow(Index+1), 0), LinesInWindow+1);
    Topline := TopLine;
    exit;
  end;
  FixFStart := Index;
  if Result > 0 then
    fHighlighter.SetRange(TSynEditStrings(Lines).Ranges[Result])
  else begin
    fHighlighter.ReSetRange;
    TSynEditStrings(Lines).Ranges[0] := fHighlighter.GetRange;
  end;
  {$ENDIF}
  if Index >= Lines.Count - 1 then begin
    fTextView.FixFoldingAtTextIndex(Index);
    fMarkupManager.TextChangedScreen(Max(RowToScreenRow(Index+1), 0), LinesInWindow+1);
    Topline := TopLine;
    Exit;
  end;
  //debugln('TCustomSynEdit.ScanFrom A Index=',dbgs(Index),' Line="',Lines[Index],'"');
  fHighlighter.SetLine(Lines[Result], Result);
  inc(Result);
  fHighlighter.NextToEol;
  {$IFDEF SYN_LAZARUS}
  LastLineDiffers := True;
  while (fHighlighter.GetRange <> TSynEditStrings(Lines).Ranges[Result])
    or (fHighlighter.LastLineCodeFoldLevelFix <> 0)
    or (TSynEditStrings(Lines).FoldMinLevel[Result-1] <> fHighlighter.MinimumCodeFoldBlockLevel)
    or (TSynEditStrings(Lines).FoldEndLevel[Result-1] <> fHighlighter.CurrentCodeFoldBlockLevel)
    or LastLineDiffers or (Result<=AtLeastTilIndex+1)
  {$ELSE}
  while (fHighlighter.GetRange <> TSynEditStringList(Lines).Ranges[Result])
  {$ENDIF}
  do begin
    //debugln(['TSynCustomHighlighter.ScanFrom WHILE Y=',Result,' Level=',fHighlighter.CurrentCodeFoldBlockLevel,' ScannedLine="',Lines[Result-1],'"']);
    {$IFDEF SYN_LAZARUS}
    LastLineDiffers := (fHighlighter.GetRange <> TSynEditStrings(Lines).Ranges[Result])
      or (fHighlighter.LastLineCodeFoldLevelFix <> 0)
      or (TSynEditStrings(Lines).FoldMinLevel[Result-1] <> fHighlighter.MinimumCodeFoldBlockLevel)
      or (TSynEditStrings(Lines).FoldEndLevel[Result-1] <> fHighlighter.CurrentCodeFoldBlockLevel);
    TSynEditStrings(Lines).Ranges[Result] := fHighlighter.GetRange;
    SetCodeFoldAttributes;
    //if (Result and $fff)=0 then
    //  debugln('TCustomSynEdit.ScanFrom A Line=', dbgs(Result),' Index=',dbgs(Index),' MinLevel=',dbgs(CodeFoldMinLevel),' EndLevel=',dbgs(CodeFoldEndLevel),' CodeFoldType=',dbgs(ord(CodeFoldType)),' ',dbgs(length(Lines[Result-1])));
    {$ELSE}
    TSynEditStringList(Lines).Ranges[Result-1] := fHighlighter.GetRange;
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
  // at least one line changed
  //  => update code fold attributes of last scanned line
  if (Result>Index+1) and (Result<=Lines.Count) then
    SetCodeFoldAttributes;
  fTextView.FixFoldingAtTextIndex(FixFStart, Result);
  fMarkupManager.TextChangedScreen(Max(RowToScreenRow(FixFStart+1), 0),
                                       Min(RowToScreenRow(Result), LinesInWindow+1));
  Topline := TopLine;
  if FixFStart < index then Invalidate;
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
// Index is 0 based
begin
  //debugln('TCustomSynEdit.ListAdded ',dbgs(Index),' ',dbgs(Assigned(fHighlighter)));
  {$IFDEF SYN_LAZARUS}
  ScanFrom(Index - 1);
  {$ELSE}
  if Assigned(fHighlighter) then begin
    if (Index > 0) then begin
      // the current line was added, start scanning from the prior line
      fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Index - 1]);
      ScanFrom(Index - 1);
    end else begin
      fHighlighter.ResetRange;
      TSynEditStringList(Lines).Ranges[0] := fHighlighter.GetRange;
      if (Lines.Count > 1) then
        ScanFrom(0);
    end;
  end;
  {$ENDIF}
  InvalidateLines(Index + 1, -1);
  InvalidateGutterLines(Index + 1, -1);
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
// Index is 0 based
begin
  {$IFDEF SYN_LAZARUS}
  ScanFrom(Index - 1);
  {$ELSE}
  if Assigned(fHighlighter) and (Lines.Count >= 1) then begin
    if (Index > 0) then begin
      // start scanning from prior line
{begin}                                                                         //mh 2000-10-10
      //DebugLn(['TCustomSynEdit.ListDeleted A Index=',Index]);
      fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Index - 1]);
      ScanFrom(Index - 1);
    end else begin
      //DebugLn(['TCustomSynEdit.ListDeleted B Index=',Index]);
      fHighlighter.ResetRange;
      TSynEditStringList(Lines).Ranges[0] := fHighlighter.GetRange;
{end}                                                                           //mh 2000-10-10
      if (Lines.Count > 1) then
        ScanFrom(0);
    end;
  end;
  {$ENDIF}
  InvalidateLines(Index + 1, -1);
  InvalidateGutterLines(Index + 1, -1);
end;

procedure TCustomSynEdit.ListInserted(Index: Integer);
// Index is 0 based
begin
  {$IFDEF SYN_LAZARUS}
  ScanFrom(Index - 1);
  {$ELSE}
  if Assigned(fHighlighter) and (Lines.Count >= 1) then begin
    if (Index > 0) then begin
      // start scanning from prior line
{begin}                                                                         //mh 2000-10-10
      // the line and the range of the line
      //DebugLn(['TCustomSynEdit.ListInserted A Index=',Index]);
      fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Index - 1]);
      ScanFrom(Index - 1);
    end else begin
      //DebugLn(['TCustomSynEdit.ListInserted B Index=',Index]);
      fHighlighter.ReSetRange;
      TSynEditStringList(Lines).Ranges[0] := fHighlighter.GetRange;
{end}                                                                           //mh 2000-10-10
      if (Lines.Count > 1) then
        ScanFrom(0);
    end;
  end;
  {$ENDIF}
  InvalidateLines(Index + 1, -1);
  InvalidateGutterLines(Index + 1, -1);
end;

procedure TCustomSynEdit.ListPutted(Index: Integer);
// Index is 0 based
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
  EndIndex:=ScanFrom(Index) + 1;
  InvalidateLines(Index + 1, EndIndex);
  InvalidateGutterLines(Index + 1, EndIndex);
  {$ELSE}
  if Assigned(fHighlighter) then begin
//    fHighlighter.SetRange(Lines.Objects[Index]);
    fHighlighter.SetRange(TSynEditStringList(Lines).Ranges[Index]);             //mh 2000-10-10
    InvalidateLines(Index + 1, ScanFrom(Index) + 1);
  end else
    InvalidateLines(Index + 1, Index + 1);
  {$ENDIF}
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.FoldChanged(Index : integer);
var
  i: Integer;
begin
  TopLine := TopLine;
  i := fTextView.CollapsedLineForFoldAtLine(CaretY);
  if i > 0 then
    SetCaretXY(Point(1, i))
  else
    EnsureCursorPosVisible;
  UpdateScrollBars;
  if Index + 1 > ScreenRowToRow(LinesInWindow + 1) then exit;
  if Index + 1 < TopLine then Index := TopLine;
  InvalidateLines(Index + 1, -1);
  InvalidateGutterLines(Index + 1, -1);
end;

procedure TCustomSynEdit.SetTopView(const AValue : Integer);
begin
  TopLine := fTextView.ViewPosToTextIndex(AValue)+1;
end;

function TCustomSynEdit.GetTopView : Integer;
begin
  Result := fTextView.TextIndexToViewPos(TopLine-1);
end;
{$ENDIF}

procedure TCustomSynEdit.ListScanRanges(Sender: TObject);
{$IFNDEF SYN_LAZARUS}
var
  i: integer;
{$ENDIF}
begin
  {$IFDEF SYN_LAZARUS}
  ScanFrom(0,Lines.Count-1);
  {$ELSE}
  if Assigned(fHighlighter) and (Lines.Count > 0) then begin
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
  end;
  {$ENDIF}
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
end;

{$ELSE}

procedure TCustomSynEdit.SetWordBlock(Value: TPoint);
var
  Runner: TPoint;
  TempString: string;
  IdChars: TSynIdentChars;
begin
  { Value is the position of the Carat in bytes }
  Value.x := MinMax(Value.x, 1, fMaxLeftChar);
  Value.y := MinMax(Value.y, 1, Lines.Count);
  TempString := Lines[Value.Y - 1];
  if TempString = '' then exit;
  // Click on right side of text
  if Length(TempString) < Value.X then Value.X := Length(TempString);

  Runner := Value;
  if Assigned(fHighlighter) then
    {$IFDEF SYN_LAZARUS}
    IdChars := [#1..#255] - (fHighlighter.WordBreakChars + TSynWhiteChars)
    {$ELSE}
    IdChars := fHighlighter.IdentChars
    {$ENDIF}
  else
    {$IFDEF SYN_LAZARUS}
    IDchars := [#1..#255] - (TSynWordBreakChars + TSynWhiteChars);
    {$ELSE}
    IDchars := [#33..#255];
    {$ENDIF}
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
  FBlockSelection.StartLineBytePos := Runner;
  Runner := Value;
  while (Runner.X < fMaxLeftChar)
  {$IFDEF FPC} and (Runner.X <= length(TempString)){$ENDIF} do begin
    if not (TempString[Runner.X] in IdChars) then break;
    Inc(Runner.X);
  end;
  if Runner.X > fMaxLeftChar then Runner.X := fMaxLeftChar;
  FBlockSelection.EndLineBytePos := Runner;
  FBlockSelection.ActiveSelectionMode := smNormal;
// set caret to the end of selected block
  CaretXY := TSynEditStrings(Lines).LogicalToPhysicalPos(Runner);
end;

{$ENDIF}

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.SetLineBlock(Value: TPoint);
var
  ALine: string;
  x, x2: Integer;
begin
  FBlockSelection.StartLineBytePos := Point(1,MinMax(Value.y, 1, Lines.Count));
  FBlockSelection.EndLineBytePos := Point(1,MinMax(Value.y+1, 1, Lines.Count));
  if (FBlockSelection.StartLinePos >= 1)
  and (FBlockSelection.StartLinePos <= Lines.Count) then begin
    ALine:=Lines[FBlockSelection.StartLinePos - 1];
    x := FBlockSelection.StartBytePos;
    while (x<length(ALine)) and (ALine[x] in [' ',#9]) do
      inc(x);
    FBlockSelection.StartLineBytePos := Point(x,MinMax(Value.y, 1, Lines.Count));
    x2:=length(ALine)+1;
    while (x2 > x) and (ALine[X2-1] in [' ',#9]) do
      dec(x2);
    FBlockSelection.EndLineBytePos := Point(x2, MinMax(Value.y, 1, Lines.Count));
  end;
  FBlockSelection.ActiveSelectionMode := smNormal;
  CaretXY := TSynEditStrings(Lines).LogicalToPhysicalPos(FBlockSelection.EndLineBytePos);
  //DebugLn(' FFF2 ',Value.X,',',Value.Y,' BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
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
  FBlockSelection.StartLineBytePos := Point(1,ParagraphStartLine);
  FBlockSelection.EndLineBytePos := Point(1,ParagraphEndLine);
  FBlockSelection.ActiveSelectionMode := smNormal;
  CaretXY:=FBlockSelection.EndLineBytePos;
  //DebugLn(' FFF3 ',Value.X,',',Value.Y,' BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
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

procedure TCustomSynEdit.InsertBlock(BB: TPoint; ChangeStr: PChar);
// used by BlockIndent and Redo
begin
  SetCaretAndSelection(LogicalToPhysicalPos(BB), BB, BB);
  SetSelTextPrimitive(smColumn, ChangeStr);
end;

procedure TCustomSynEdit.Redo;
{begin}                                                                         //sbs 2000-11-19
var
  Item: TSynEditUndoItem;
  OldChangeNumber: integer;
begin
  Item := fRedoList.PeekItem;
  if Item <> nil then begin
    {$IFDEF SYN_LAZARUS}
    fUndoList.BeginBlock;
    OldChangeNumber := Item.fChangeNumber;
    {$ELSE}
    OldChangeNumber := fUndoList.BlockChangeNumber;
    fUndoList.BlockChangeNumber := Item.fChangeNumber;
    {$ENDIF}
    try
      repeat
        RedoItem;
        Item := fRedoList.PeekItem;
      {$IFDEF SYN_LAZARUS}
      until (Item = nil) or (Item.fChangeNumber <> OldChangeNumber);
      {$ELSE}
      until (Item = nil) or (Item.fChangeNumber <> fUndoList.BlockChangeNumber);
      {$ENDIF}
    finally
      {$IFDEF SYN_LAZARUS}
      fUndoList.EndBlock;
      {$ELSE}
      fUndoList.BlockChangeNumber := OldChangeNumber;
      {$ENDIF}
    end;
  end;
end;

procedure TCustomSynEdit.RedoItem;
{end}                                                                           //sbs 2000-11-19
var
  Item: TSynEditUndoItem;
  OldSelMode: TSynSelectionMode;
  Run, StrToDelete: PChar;
  Len, x : integer;
  TempString: string;
  CaretPt: TPoint;
  ChangeScrollPastEol: boolean;                                                 //mh 2000-10-30
  {$IFDEF SYN_LAZARUS}
  PhysStartPos: TPoint;
  {$ELSE}
  e: integer;
  {$ENDIF}
begin
  OldSelMode := FBlockSelection.SelectionMode;
  ChangeScrollPastEol := not (eoScrollPastEol in Options);                      //mh 2000-10-30
  Item := fRedoList.PopItem;
  if Assigned(Item) then try
    SelectionMode := Item.fChangeSelMode;
    IncPaintLock;
    Include(fOptions, eoScrollPastEol);                                         //mh 2000-10-30
    Include(fStateFlags, sfInsideRedo);                                         //mh 2000-10-30
    {$IFDEF SYN_LAZARUS}
    PhysStartPos:=LogicalToPhysicalPos(Item.fChangeStartPos);
    {$ENDIF}
    case Item.fChangeReason of
      crInsert, crPaste, crDragDropInsert:
        begin
          SetCaretAndSelection(
            {$IFDEF SYN_LAZARUS}PhysStartPos{$ELSE}Item.fChangeStartPos{$ENDIF},
            Item.fChangeStartPos, Item.fChangeStartPos
            );
          SetSelTextPrimitive(Item.fChangeSelMode, PChar(Item.fChangeStr));
          {$IFDEF SYN_LAZARUS}
          CaretXY := LogicalToPhysicalPos(Item.fChangeEndPos);
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
            Item.fChangeStartPos,{$IFDEF SYN_LAZARUS}Item.fChangeEndPos{$ELSE}Item.fChangeStartPos{$ENDIF}
            );
          fUndoList.AddChange(Item.fChangeReason, Item.fChangeStartPos,
            Item.fChangeEndPos, GetSelText, Item.fChangeSelMode);
          SetSelTextPrimitive(Item.fChangeSelMode, PChar(Item.fChangeStr));
          {$IFDEF SYN_LAZARUS}
          CaretXY := LogicalToPhysicalPos(Item.ChangeStartPos);
          {$ELSE}
          CaretXY := Item.fChangeStartPos;
          {$ENDIF}
        end;
      crDelete, {crDragDropDelete, crSelDelete, }crSilentDelete:                //mh 2000-10-30, 2000-11-20
        begin
          SetCaretAndSelection(
            {$IFDEF SYN_LAZARUS}PhysStartPos{$ELSE}Item.fChangeStartPos{$ENDIF},
            Item.fChangeStartPos, {$IFDEF SYN_LAZARUS}Item.fChangeEndPos{$ELSE}Item.fChangeStartPos{$ENDIF}
            );
          fUndoList.AddChange(Item.fChangeReason, Item.fChangeStartPos,
            Item.fChangeEndPos, GetSelText, Item.fChangeSelMode);
          SetSelTextPrimitive(Item.fChangeSelMode, PChar(Item.fChangeStr));
          {$IFDEF SYN_LAZARUS}
          CaretXY := PhysStartPos;
          {$ELSE}
          CaretXY := LogicalToPhysicalPos(Item.ChangeStartPos);
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
      {$IFDEF SYN_LAZARUS}
      crTrimSpace: TSynEditStringTrimmingList(fTrimLines).ForceTrim;
      {$ENDIF}
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
          SetCaretAndSelection(LogicalToPhysicalPos(Item.fChangeEndPos),
            Item.fChangeStartPos, Item.fChangeEndPos);
          x := fBlockIndent;
          fBlockIndent := ord(Item.fChangeStr[1]);
          SelectionMode := Item.fChangeSelMode;
          DoBlockIndent;
          fBlockIndent := x;
        end;
      crUnindent :
        begin // re-delete the (raggered) column
          // add to undo list
          fUndoList.AddChange(Item.fChangeReason, Item.fChangeStartPos,
            Item.fChangeEndPos, Item.fChangeStr, Item.fChangeSelMode);
          // Delete string
          StrToDelete := PChar(Item.fChangeStr);
          {$IFDEF SYN_LAZARUS}
          CaretY := Item.ChangeStartPos.Y;
          x := -1;
          {$ELSE}
          CaretY := Item.fChangeStartPos.Y;
          {$ENDIF}
          repeat
            Run := GetEOL(StrToDelete);
            {$IFDEF SYN_LAZARUS}
            Len := Run - StrToDelete;
            if x < 0 then
              x:= Len;
            if Len > 0 then begin
              TempString := Lines[CaretY - 1];
              Delete(TempString, 1, Len);
              Lines[CaretY - 1] := TempString;
            end;
            if Run^ in [#10,#13] then begin
              if (Run[1] in [#10,#13]) and (Run^<>Run[1]) then
                Inc(Run,2)
              else
                Inc(Run);
              CaretY := CaretY + 1;
            end;
            {$ELSE}
            if Run <> StrToDelete then begin
              Len := Run - StrToDelete;
              TempString := Lines[CaretY - 1];
              if Len > 0 then
                Delete(TempString, 1, Len);
              Lines[CaretY - 1] := TempString;
            end else
              Len := 0;
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
          {$IFDEF SYN_LAZARUS}
          if (Item.fChangeStartPos.y > Item.fChangeEndPos.y)
            or ((Item.fChangeStartPos.y = Item.fChangeEndPos.y) and (Item.fChangeStartPos.x > Item.fChangeEndPos.x))
          then SwapInt(Len, x);
          CaretPt := Point(Item.fChangeEndPos.x - Len, Item.fChangeEndPos.y);
          SetCaretAndSelection(LogicalToPhysicalPos(CaretPt),
            Point(Item.fChangeStartPos.x - x, Item.fChangeStartPos.y), CaretPt
            );
          {$ELSE}
          CaretPt := Point(Item.fChangeStartPos.x - fTabWidth,
                           Item.fChangeStartPos.y);
          SetCaretAndSelection(CaretPt, CaretPt,
            Point(Item.fChangeEndPos.x - Len, Item.fChangeEndPos.y)
            );
          {$ENDIF}
        end;
    end;
  finally
    FBlockSelection.SelectionMode       := OldSelMode;
    FBlockSelection.ActiveSelectionMode := Item.fChangeSelMode;
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
    {$IFDEF SYN_LAZARUS}
    fRedoList.BeginBlock;
    OldChangeNumber := Item.fChangeNumber;
    {$ELSE}
    OldChangeNumber := fRedoList.BlockChangeNumber;
    fRedoList.BlockChangeNumber := Item.fChangeNumber;
    {$ENDIF}
    fUndoList.Lock;
    try
      repeat
        UndoItem;
        Item := fUndoList.PeekItem;
      {$IFDEF SYN_LAZARUS}
      until (Item = nil) or (Item.fChangeNumber <> OldChangeNumber);
      {$ELSE}
      until (Item = nil) or (Item.fChangeNumber <> fRedoList.BlockChangeNumber);
      {$ENDIF}
    finally
      // Todo: Decide what do to, If there are any trimable spaces.
      TSynEditStringTrimmingList(fTrimLines).ForceTrim;
      fUndoList.UnLock;
      {$IFDEF SYN_LAZARUS}
      fRedoList.EndBlock;
      {$ELSE}
      fRedoList.BlockChangeNumber := OldChangeNumber;
      {$ENDIF}
    end;
  end;
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
  OldSelMode := FBlockSelection.SelectionMode;
  ChangeScrollPastEol := not (eoScrollPastEol in Options);                      //mh 2000-10-30
  Item := fUndoList.PopItem;
  if Assigned(Item) then try
    FBlockSelection.SelectionMode := Item.fChangeSelMode; // Default and Active SelectionMode
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
          SetSelTextPrimitive(Item.fChangeSelMode, PChar(Item.fChangeStr));
          CaretXY := {$IFDEF SYN_LAZARUS}PhysStartPos
                     {$ELSE}Item.fChangeStartPos{$ENDIF};
        end;
      crDeleteAfterCursor, crDelete, {crDragDropDelete, crSelDelete, }          //mh 2000-11-20
      {$IFDEF SYN_LAZARUS}crTrimSpace,{$ENDIF}
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
          SetCaretAndSelection(LogicalToPhysicalPos(TmpPos), TmpPos, TmpPos);
          SetSelTextPrimitive(Item.fChangeSelMode, PChar(Item.fChangeStr));
          if Item.fChangeReason in [crSilentDelete, crSilentDeleteAfterCursor
                                    {$IFDEF SYN_LAZARUS}, crTrimSpace{$ENDIF} ]
          then
            CaretXY := LogicalToPhysicalPos(Item.fChangeEndPos)
          else begin
            SetCaretAndSelection(LogicalToPhysicalPos(Item.fChangeEndPos),
              Item.fChangeStartPos, Item.fChangeEndPos);
          end;
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
          {$IFDEF SYN_LAZARUS}
          if Item.fChangeStr <> '' then
            Lines[CaretY - 1] := TmpStr + Item.fChangeStr;
          {$ELSE}
          TrimmedSetLine(CaretY - 1, TmpStr + Item.fChangeStr);
          {$ENDIF}
          DoLinesDeleted(CaretY, 1);
        end;
      crIndent: // remove the column that was inserted
        begin
          // select the inserted column
          {$IFDEF SYN_LAZARUS}
          BlockBegin := Point(1, Item.ChangeStartPos.y);
          TmpPos := Item.ChangeEndPos;
          if TmpPos.x = 1 then
            Dec(TmpPos.y);
          TmpPos.x := ord(Item.fChangeStr[1])+1;
          BlockEnd := TmpPos;
          {$ELSE}
          BlockBegin := Point(1, Item.fChangeStartPos.y);
          TmpPos := Item.fChangeEndPos;
          if TmpPos.x = 1 then
            Dec(TmpPos.y);
          TmpPos.x := fTabWidth+1;
          BlockEnd := TmpPos;
          {$ENDIF}
          FBlockSelection.ActiveSelectionMode := smColumn;
          // add to redo list
          fRedoList.AddChange(Item.fChangeReason, Item.fChangeStartPos,
            Item.fChangeEndPos, {$IFDEF SYN_LAZARUS}Item.fChangeStr{$ELSE}GetSelText{$ENDIF}, Item.fChangeSelMode);
          // remove the column
          SetSelTextPrimitive(Item.fChangeSelMode, nil);
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
          InsertBlock(Point(1, Item.ChangeStartPos.y), PChar(Item.fChangeStr));
          SetCaretAndSelection(LogicalToPhysicalPos(Item.fChangeEndPos),
            Item.fChangeStartPos, Item.fChangeEndPos);
        end;
    end;
  finally
    FBlockSelection.SelectionMode       := OldSelMode;
    FBlockSelection.ActiveSelectionMode := Item.fChangeSelMode;
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
  NewY, NewX1, NewX2: Integer;
begin
  fLastControlIsPressed:=(GetKeyShiftState=[SYNEDIT_LINK_MODIFIER]);
  if (eoShowCtrlMouseLinks in Options) and fLastControlIsPressed
  and (fLastMouseCaret.X>0) and (fLastMouseCaret.Y>0) then begin
    // show link
    NewY:=fLastMouseCaret.Y;
    GetWordBoundsAtRowCol(PhysicalToLogicalPos(fLastMouseCaret),NewX1,NewX2);
    if IsLinkable(NewY, NewX1, NewX2) then begin
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

function TCustomSynEdit.IsLinkable(Y, X1, X2: Integer): Boolean;
begin
  Result := X1 <> X2;
  if Result and Assigned(FOnMouseLink) then
    FOnMouseLink(Self, X1, Y, Result);
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
          ChangeScrollPastEOL := not (eoScrollPastEol in fOptions);
          try
            if ChangeScrollPastEOL then
              Include(fOptions, eoScrollPastEol);
            CaretXY := NewCaret;
            BlockBegin := NewCaret;
            if Source = Self then
              SetSelTextPrimitive(smNormal, PChar(DragDropText), true, crDragDropInsert)
            else
              SetSelTextPrimitive(smNormal, PChar(DragDropText), true, crInsert);
          finally
            if ChangeScrollPastEOL then
              Exclude(fOptions, eoScrollPastEol);
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
      TSynEditStrings(Lines).ClearRanges(fHighlighter.GetRange);
      fTSearch.IdentChars:=fHighlighter.IdentChars;
    end else begin
      fTSearch.ResetIdentChars;
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
        FCaretOffset := Point(0, fTextHeight - 1);
      end;
    ctHalfBlock:
      begin
        cw := fCharWidth;
        ch := (fTextHeight - 2) div 2;
        FCaretOffset := Point(0, ch + 1);
      end;
    ctBlock:
      begin
        cw := fCharWidth;
        ch := fTextHeight - 2;
        FCaretOffset := Point(0, 1);
      end;
    else begin // ctVerticalLine
      cw := 2;
      ch := fTextHeight - 2;
      FCaretOffset := Point(-1, 1);
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
    fBlockSelection.FMaxLeftChar := Value;
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
    //DebugLn(['TCustomSynEdit.EnsureCursorPosVisible B LeftChar=',LeftChar,' MinX=',MinX,' MaxX=',MaxX,' CharsInWindow=',CharsInWindow]);
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
      {$IFDEF SYN_LAZARUS}
    else if CaretY > ScreenRowToRow(Max(1, LinesInWindow) - 1) then             //mh 2000-10-19
      TopLine := fTextView.TextPosAddLines(CaretY, -Max(0, LinesInWindow-1))
      {$ELSE}
    else if CaretY > TopLine + Max(1, LinesInWindow) - 1 then                   //mh 2000-10-19
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
  var Data: pointer
  {$IFDEF SYN_LAZARUS};out IsStartOfCombo: boolean{$ENDIF}
  ): TSynEditorCommand;
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
    {$IFDEF SYN_LAZARUS}
    IsStartOfCombo:=KeyStrokes.FindKeycode2Start(Code,Shift)>=0;
    {$ENDIF}
  end else begin
    fLastKey := 0;
    fLastShiftState := [];
    {$IFDEF SYN_LAZARUS}
    IsStartOfCombo:=false;
    {$ENDIF}
  end;
end;

procedure TCustomSynEdit.CommandProcessor(Command: TSynEditorCommand;
  AChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
  Data: pointer);
begin
  {$IFDEF VerboseKeys}
  DebugLn(['[TCustomSynEdit.CommandProcessor] ',Command
    ,' AChar=',AChar,' Data=',DbgS(Data)]);
  DumpStack;
  {$ENDIF}
  // first the program event handler gets a chance to process the command
  DoOnProcessCommand(Command, AChar, Data);
  try
    BeginUndoBlock;
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
  finally
    EndUndoBlock;
  end;
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
  CY: Integer;
  {$ENDIF}

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
          fLastCaretX := CaretX;
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
          fLastCaretX := CaretX;
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
          {$IFDEF SYN_LAZARUS}
          TopView := TopView + counter;
          {$ELSE}
          TopLine := TopLine + counter;
          {$ENDIF}
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
          CaretNew := Point(CaretX, ScreenRowToRow(LinesInWindow - 1));
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
          {$IFDEF SYN_LAZARUS}
          CaretNew := Point(1, fTextView.ViewPosToTextIndex(fTextView.Count)+1);
          {$ELSE}
          CaretNew := Point(1, Lines.Count);
          {$ENDIF}
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
          fLastCaretX := CaretX;                                               //mh 2000-10-19
          Update;
        end;
// word selection
      ecWordLeft, ecSelWordLeft:
        begin
          Caret := CaretXY;
          CaretNew := PrevWordPos;
          {$IFDEF SYN_LAZARUS}
          if fTextView.FoldedAtTextIndex[CaretNew.Y - 1] then begin
            CY := FindNextUnfoldedLine(CaretNew.Y, False);
            CaretNew := LogicalToPhysicalPos(Point(1 + Length(Lines[CY-1]), CY));
          end;
          MoveCaretAndSelectionPhysical
          {$ELSE}
          MoveCaretAndSelection
          {$ENDIF}
            (Caret, CaretNew, Command = ecSelWordLeft);
          fLastCaretX := CaretX;                                               //mh 2000-10-19
          {$IFDEF SYN_LAZARUS}
          Update;
          {$ENDIF}
        end;
      ecWordRight, ecSelWordRight:
        begin
          Caret := CaretXY;
          CaretNew := NextWordPos;
          {$IFDEF SYN_LAZARUS}
          if fTextView.FoldedAtTextIndex[CaretNew.Y - 1] then
            CaretNew := Point(1, FindNextUnfoldedLine(CaretNew.Y, True));
          MoveCaretAndSelectionPhysical
          {$ELSE}
          MoveCaretAndSelection
          {$ENDIF}
            (Caret, CaretNew, Command = ecSelWordRight);
          fLastCaretX := CaretX;                                               //mh 2000-10-19
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
            SetSelTextExternal('')
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
              Helper := ' ';
              CaretX := CaretX - 1;
              {$IFDEF SYN_LAZARUS}
              // behind EOL, there was no char to delete, this wa a simple cursor move, do not undo
              Caret := CaretXY;
              {$ENDIF}
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
                {$IFNDEF SYN_LAZARUS}
                if eoTrimTrailingSpaces in Options then
                  Temp := TrimRight(Temp);
                {$ENDIF}
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
                Lines[CaretY - 1] :=  Temp;
                CaretX := LogicalToPhysicalCol(Temp,LogSpacePos);
                {$ELSE}
                Helper := Copy(Temp, 1, SpaceCount1 - SpaceCount2);
                Delete(Temp, 1, SpaceCount1 - SpaceCount2);
                TrimmedSetLine(CaretY - 1, Temp);
                fCaretX := fCaretX - (SpaceCount1 - SpaceCount2);
                {$ENDIF}
                fLastCaretX := CaretX;
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
                  Lines[CaretY - 1] := Temp;
                {$ELSE}
                  {$IFDEF SYN_MBCSSUPPORT}
                  if ByteType(Temp, CaretX - 2) = mbLeadByte then
                    Inc(counter);
                  {$ENDIF}
                  CaretX := CaretX - counter;
                  Helper := Copy(Temp, CaretX, counter);
                  Delete(Temp, CaretX, counter);
                  TrimmedSetLine(CaretY - 1, Temp);
                {$ENDIF}
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
            SetSelTextExternal('')
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
                Lines[CaretY - 1] := Temp;
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
                Helper := StringOfChar(' ', LogCaretXY.X - 1 - Len);
                Lines[CaretY - 1] := Temp + Helper + Lines[CaretY];
                Caret := Point(1, CaretY + 1);
                Helper := {$IFDEF SYN_LAZARUS}LineEnding{$ELSE}#13#10{$ENDIF};
                Lines.Delete(CaretY);
                DoLinesDeleted(CaretY - 1, 1);
              end;
            end;
            if (Caret.X <> CaretX) or (Caret.Y <> CaretY) then begin
              fUndoList.AddChange(crSilentDeleteAfterCursor,
                PhysicalToLogicalPos(Caret), PhysicalToLogicalPos(CaretXY),
                Helper, smNormal);
            end;
          end;
        end;
      ecDeleteWord, ecDeleteEOL:
        if not ReadOnly then begin
          Len := LogicalToPhysicalCol(LineText,Length(LineText)+1)-1;
          Helper := '';
          if Command = ecDeleteWord then begin
            if CaretX > Len + 1 then
              Helper := StringOfChar(' ', CaretX - 1 - Len);
            WP := NextWordPos{$IFDEF SYN_LAZARUS}(True){$ENDIF};
          end else
            WP := Point(Len + 1, CaretY);
          if (WP.X <> CaretX) or (WP.Y <> CaretY) then begin
            if Helper <> '' then
              LineText := LineText + Helper;
            OldSelMode := FBlockSelection.ActiveSelectionMode;
            try
              SetBlockBegin(PhysicalToLogicalPos(WP));
              SetBlockEnd(PhysicalToLogicalPos(CaretXY));
              FBlockSelection.ActiveSelectionMode := smNormal;
              SetSelTextPrimitive(smNormal, nil, true, crSilentDeleteAfterCursor)
            finally
              FBlockSelection.ActiveSelectionMode := OldSelMode;
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
            OldSelMode := FBlockSelection.ActiveSelectionMode;
            try
              SetBlockBegin(PhysicalToLogicalPos(WP));
              SetBlockEnd(PhysicalToLogicalPos(CaretXY));
              FBlockSelection.ActiveSelectionMode := smNormal;
              SetSelTextPrimitive(smNormal, nil, true, crSilentDelete)
            finally
              FBlockSelection.ActiveSelectionMode := OldSelMode;
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
            SetSelTextExternal('');
          end;
          SpaceCount2 := 0;
          Temp := LineText;
          Temp2 := Temp; //LineText;
// This is sloppy, but the Right Thing would be to track the column of markers
// too, so they could be moved depending on whether they are after the caret...
          InsDelta := Ord(CaretX = 1);
          {$IFDEF SYN_LAZARUS}
          LogCaretXY:=PhysicalToLogicalPos(CaretXY);
          Len := Length(Temp);
          if Len >= LogCaretXY.X then begin
            if LogCaretXY.X > 1 then begin
              // break line in two
              SpaceCount1 := LeftSpaces(Temp);
              Temp := Copy(LineText, 1, LogCaretXY.X - 1);
              Lines.Insert(CaretY - 1, Temp);
              Delete(Temp2, 1, LogCaretXY.X - 1);
              if Assigned(Beautifier) then
                SpaceCount1:=Beautifier.GetIndentForLineBreak(Self,LogCaretXY,Temp2);
              fUndoList.AddChange(crLineBreak,
                LogCaretXY, LogCaretXY,
                Temp2, smNormal);
              Lines[CaretY] := StringOfChar(' ', SpaceCount1) + Temp2;
              if Command = ecLineBreak then
                CaretXY := Point(SpaceCount1 + 1, CaretY + 1);
            end else begin
              // move the whole line
              Lines.Insert(CaretY - 1, '');
              fUndoList.AddChange(crLineBreak,
                LogCaretXY, LogCaretXY,
                Temp2, smNormal);
              if Command = ecLineBreak then
                CaretY := CaretY + 1;
            end;
          end else begin
            // current line is empty (len = 0)
            if Lines.Count = 0 then
              Lines.Add('');
            // linebreak after end of line
            fUndoList.AddChange(crLineBreak,
              LogCaretXY, LogCaretXY,
              '', smNormal);
            SpaceCount2 := 0;
            if Assigned(Beautifier) then begin
              Temp:='';
              SpaceCount2:=Beautifier.GetIndentForLineBreak(Self,LogCaretXY,Temp);
            end else if eoAutoIndent in Options then begin
              BackCounter := CaretY;
              repeat
                Dec(BackCounter);
                Temp := Lines[BackCounter];
                SpaceCount2 := LeftSpaces(Temp,true);
              until (BackCounter = 0) or (Temp <> '');
            end;
            Lines.Insert(CaretY, '');
            if Command = ecLineBreak then begin
              if (SpaceCount2 > 0) then
                Lines[CaretY] := StringOfChar(' ', SpaceCount2);
              CaretXY := Point(SpaceCount2 + 1, CaretY + 1);
            end;
          end;
          {$ELSE}
          Len := Length(Temp);
          if Len > 0 then begin
            if Len >= LogCaretXY.X then begin
              if LogCaretXY.X > 1 then begin
                // break line in two
                SpaceCount1 := LeftSpaces(Temp);
                Temp := Copy(LineText, 1, LogCaretXY.X - 1);
                TrimmedSetLine(CaretY - 1, Temp);
                Delete(Temp2, 1, LogCaretXY.X - 1);
                fUndoList.AddChange(crLineBreak,
                  CaretXY, CaretXY,
                  Temp2, smNormal);
                Lines.Insert(CaretY, StringOfChar(' ', SpaceCount1) + Temp2);
                if Command = ecLineBreak then
                  CaretXY := Point(SpaceCount1 + 1, CaretY + 1);
              end else begin
                // move the whole line
                Lines.Insert(CaretY - 1, '');
                fUndoList.AddChange(crLineBreak,
                  CaretXY, CaretXY,
                  Temp2, smNormal);
                if Command = ecLineBreak then
                  CaretY := CaretY + 1;
              end;
            end else begin
              // linebreak after end of line
              fUndoList.AddChange(crLineBreak,
                CaretXY, CaretXY,
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
            end;
          end else begin
            // current line is empty
            if fLines.Count = 0 then
              fLines.Add('');
            BackCounter := CaretY - 1;
            while BackCounter >= 0 do begin
              SpaceCount2 := LeftSpaces(Lines[BackCounter]);
              if Length(Lines[BackCounter]) > 0 then break;
              dec(BackCounter);
            end;
            fUndoList.AddChange(crLineBreak,
              CaretXY, CaretXY,
              '', smNormal);
            if Command = ecLineBreak then
              CaretX := SpaceCount2 + 1;
            Lines.Insert(CaretY - 1, '');
            if Command = ecLineBreak then
              CaretY := CaretY + 1;
          end;
          {$ENDIF}
          DoLinesInserted(CaretY - InsDelta, 1);
          EnsureCursorPosVisible;                                               //JGF 2000-09-23
          fLastCaretX := CaretX;                                               //mh 2000-10-19
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
              SetSelTextExternal(AChar);
              //debugln('ecChar SelAvail StartOfBlock=',dbgs(StartOfBlock),' fBlockEnd=',dbgs(fBlockEnd));
            finally
              EndUndoBlock;
            end;
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
                if Len < LogCaretXY.X - 1 then
                  Temp := Temp + StringOfChar(' ', LogCaretXY.X - 1 - Len);
                System.Insert(AChar, Temp, LogCaretXY.X);
                //debugln('ecChar Temp=',DbgStr(Temp),' AChar=',DbgStr(AChar));
                CaretX := CaretX + 1;
                {$ENDIF}
                Lines[CaretY - 1] := Temp;
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
                Lines[CaretY - 1] := Temp;
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
          {$IFDEF SYN_LAZARUS}
          TopView := TopView - 1;
		  {$ELSE}
          TopLine := TopLine - 1;
          {$ENDIF}
          if CaretY > {$IFDEF SYN_LAZARUS}ScreenRowToRow(LinesInWindow-1){$ELSE}TopLine + LinesInWindow - 1{$ENDIF} then
            CaretY := {$IFDEF SYN_LAZARUS}ScreenRowToRow(LinesInWindow-1){$ELSE}TopLine + LinesInWindow - 1{$ENDIF};
          Update;
        end;
      ecScrollDown:
        begin
          {$IFDEF SYN_LAZARUS}
          TopView := TopView + 1;
		  {$ELSE}
          TopLine := TopLine + 1;
          {$ENDIF}
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
            SetSelTextExternal(s);
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
      {$IFDEF SYN_LAZARUS}
      EcFoldLevel1..EcFoldLevel9:
        FoldAll(Command - EcFoldLevel1);
      EcFoldLevel0:
        UnfoldAll;
      EcFoldCurrent:
        begin
          CY := fTextView.ExpandedLineForBlockAtLine(CaretY);
          if CY > 0 then begin
            fTextView.FoldAtTextIndex(CY-1);
            SetCaretXY(Point(1, CY));
          end;
        end;
      EcUnFoldCurrent:
          fTextView.UnFoldAtTextIndex(CaretY-1);
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
        fHighlighter.SetRange(TSynEditStrings(Lines).Ranges[CY - 1]);
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

function TCustomSynEdit.NextWordPos{$IFDEF SYN_LAZARUS}(WordEndForDelete : Boolean = false){$ENDIF}: TPoint;
var
  CX, CY, LineLen: integer;
  Line: string;
  CurIdentChars, WhiteChars: TSynIdentChars;
  {$IFDEF SYN_LAZARUS}
  LogCaret: TPoint;
  WordBrkChars: TSynIdentChars;
  bow : Boolean;
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

    {$IFDEF SYN_LAZARUS}
    if Assigned(Highlighter) then
      WordBrkChars := Highlighter.WordBreakChars
    else
      WordBrkChars := TSynWordBreakChars;
    CurIdentChars := [#1..#255] - (WordBrkChars + TSynWhiteChars);
    WhiteChars := TSynWhiteChars + ([#1..#255] - CurIdentChars);
    {$ELSE}
    CurIdentChars:=IdentChars;
    WhiteChars := [#1..#255] - CurIdentChars;
    {$ENDIF}
    LineLen := Length(Line);

    if CX >{$IFNDEF SYN_LAZARUS}={$ENDIF} LineLen then begin
      // find first IdentChar in the next line
      if CY < Lines.Count then begin
        Line := Lines[CY];
        Inc(CY);
        {$IFDEF SYN_LAZARUS}
        if WordEndForDelete then
          CX := Max(1, StrScanForCharInSet(Line, 1, [#1..#255] - TSynWhiteChars))
        else
        {$ENDIF}
        CX := Max(1, StrScanForCharInSet(Line, 1, CurIdentChars));
      end;
    end else begin
      {$IFDEF SYN_LAZARUS}
      if WordEndForDelete then begin
        if Line[CX] in TSynWhiteChars then
          CX := StrScanForCharInSet(Line, CX, [#1..#255] - TSynWhiteChars)
        else begin
          bow := (CX = 1) OR (Line[CX-1] in WhiteChars); // Cursor at BeginOfWord
          if Line[CX] in WordBrkChars then
            CX := StrScanForCharInSet(Line, CX, [#1..#255] - WordBrkChars)
          else
            CX := StrScanForCharInSet(Line, CX, [#1..#255] - CurIdentChars);
          // Remove WitheSpaces, if Cursor was at BeginOfWord or in WordBrkchars
          if bow and(cx > 0) then
            CX := StrScanForCharInSet(Line, CX, [#1..#255] - TSynWhiteChars);
        end;
      end else begin
      {$ENDIF}
      // find first "whitespace" if next char is an IdentChar
      if Line[CX] in CurIdentChars then
        CX := StrScanForCharInSet(Line, CX, WhiteChars);
      // if "whitespace" found find the first IdentChar behind
      if CX > 0 then
        CX := StrScanForCharInSet(Line, CX, CurIdentChars);
      {$IFDEF SYN_LAZARUS}
      end;
      {$ENDIF}
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
  LogCaret:=LogicalCaretXY;
  CX := LogCaret.X;
  CY := LogCaret.Y;
  {$ELSE}
  CX := CaretX;
  CY := CaretY;
  {$ENDIF}
  //DebugLn(['TCustomSynEdit.PrevWordPos ',dbgs(LogCaret)]);
  // valid line?
  if (CY >= 1) and (CY <= Lines.Count) then begin
    Line := Lines[CY - 1];
    CX := Min(CX, Length(Line) + 1);

    {$IFDEF SYN_LAZARUS}
    if Assigned(Highlighter) then
      CurIdentChars := [#1..#255] - (Highlighter.WordBreakChars + TSynWhiteChars)
    else
      CurIdentChars := [#1..#255] - (TSynWordBreakChars + TSynWhiteChars);
    WhiteChars := TSynWhiteChars + ([#1..#255] - CurIdentChars);
    {$ELSE}
    CurIdentChars:=IdentChars;
    WhiteChars := [#1..#255] - CurIdentChars;
    {$ENDIF}
    //DebugLn(['TCustomSynEdit.PrevWordPos Line="',dbgstr(Line),'" CX=',CX]);
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
      //DebugLn(['TCustomSynEdit.PrevWordPos AAA1 CX=',CX]);
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
      //DebugLn(['TCustomSynEdit.PrevWordPos AAA2 CX=',CX]);
    end;
  end;
  //DebugLn(['TCustomSynEdit.PrevWordPos AAA3 ',CX,',',CY]);
  {$IFDEF SYN_LAZARUS}
  Result := LogicalToPhysicalPos(Point(CX, CY));
  {$ELSE}
  Result := Point(CX, CY);
  {$ENDIF}
  //DebugLn(['TCustomSynEdit.PrevWordPos END ',dbgs(Result)]);
end;

function TCustomSynEdit.GetSelectionMode : TSynSelectionMode;
begin
  Result := fBlockSelection.ActiveSelectionMode;
end;

procedure TCustomSynEdit.SetSelectionMode(const Value: TSynSelectionMode);
begin
  fBlockSelection.SelectionMode := Value; // Set both: SelectionMode and ActiveSelectionMode
end;

{begin}                                                                         //sbs 2000-11-19
procedure TCustomSynEdit.BeginUndoBlock;
begin
  fUndoList.BeginBlock;
  {$IFDEF SYN_LAZARUS}
  IncPaintLock;
  fTextView.Lock;
  TSynEditStringTrimmingList(fTrimLines).Lock;
  {$ENDIF}
end;
{end}                                                                           //sbs 2000-11-19

procedure TCustomSynEdit.BeginUpdate;
begin
  IncPaintLock;
end;

{begin}                                                                         //sbs 2000-11-19
procedure TCustomSynEdit.EndUndoBlock;
begin
  {$IFDEF SYN_LAZARUS}
  // Write all trimming info to the end of the undo block,
  // so it will be undone first, and other UndoItems do see the expected spaces
  TSynEditStringTrimmingList(fTrimLines).UnLock;
  fTextView.UnLock;
   // must be last => May call MoveCaretToVisibleArea, which must only happen
   // after unfold
  DecPaintLock;
  {$ENDIF}
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
    result := result + llen(Lines[loop]);
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
    count := count + llen(Lines[loop]);
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
    Result := result + llen(Lines[loop]);
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
    count := count + llen(Lines.strings[loop]);
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
  {$IFDEF SYN_LAZARUS}
  if fExtraLineSpacing=Value then exit;
  {$ENDIF}
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

procedure TCustomSynEdit.MarkTextAsSaved;
begin
  TSynEditStringList(fLines).MarkSaved;
  if fGutter.Visible and fGutter.ShowChanges then
    InvalidateGutter;
end;

procedure TCustomSynEdit.ClearUndo;
begin
  fUndoList.Clear;
  fRedoList.Clear;
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
      fGutterWidth := nw;
      fTextOffset := fGutterWidth + 2 - (LeftChar - 1) * fCharWidth;
      if HandleAllocated then begin
        fCharsInWindow := Max(1,(ClientWidth - fGutterWidth - 2 - ScrollBarWidth)
                                div fCharWidth);
        //debugln('TCustomSynEdit.SetGutterWidth A ClientWidth=',dbgs(ClientWidth),' fGutterWidth=',dbgs(fGutterWidth),' ScrollBarWidth=',dbgs(ScrollBarWidth),' fCharWidth=',dbgs(fCharWidth));
        UpdateScrollBars;
        Invalidate;
      end;
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
  {$IFDEF SYN_LAZARUS}
  TopView := TopView - (nDelta * nWheelClicks);
  {$ELSE}
  TopLine := TopLine - (nDelta * nWheelClicks);
  {$ENDIF}
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
    TSynEditStringList(fLines).TabWidth := Value;                        //mh 2000-10-19
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
{$IFDEF SYN_LAZARUS}
begin
  Result := SearchReplaceEx(ASearch, AReplace, AOptions, LogicalCaretXY);
end;

function TCustomSynEdit.SearchReplaceEx(const ASearch, AReplace: string;
  AOptions: TSynSearchOptions; AStart: TPoint): integer;
{$ENDIF}
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
      ptEnd.X := Length(Lines[ptEnd.Y - 1]) + 1;
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
    ptEnd.Y := Lines.Count;
    ptEnd.X := Length(Lines[ptEnd.Y - 1]) + 1;
    if bFromCursor then
      if bBackward then
        ptEnd := {$IFDEF SYN_LAZARUS}AStart{$ELSE}CaretXY{$ENDIF}
      else
        ptStart := {$IFDEF SYN_LAZARUS}AStart{$ELSE}CaretXY{$ENDIF};
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
  ChangedOptions: TSynEditorOptions;
  {$ENDIF}
begin
  if (Value <> fOptions) then begin
    {$IFDEF SYN_LAZARUS}
    ChangedOptions:=(fOptions-Value)+(Value-fOptions);
    {$ENDIF}
    bSetDrag := (eoDropFiles in fOptions) <> (eoDropFiles in Value);
    fOptions := Value;
    {$IFDEF SYN_LAZARUS}
    TSynEditStringTrimmingList(fTrimLines).Enabled := eoTrimTrailingSpaces in fOptions;
    {$ENDIF}
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
    if (eoPersistentCaret in ChangedOptions) and HandleAllocated then begin
      SetCaretRespondToFocus(Handle,not (eoPersistentCaret in fOptions));
      UpdateCaret;
    end;
    if (eoShowCtrlMouseLinks in ChangedOptions) and HandleAllocated then
      UpdateCtrlMouse;
    if (eoShowSpecialChars in ChangedOptions) and HandleAllocated then
      Invalidate;
    {$ENDIF}
    if (eoNoSelection in ChangedOptions) then
      FBlockSelection.Enabled := eoNoSelection in fOptions;
    FBlockSelection.SpacesToTabs := eoSpacesToTabs in fOptions;
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
    fTextView.LinesInWindow := fLinesInWindow;

    fMarkupManager.LinesInWindow:= fLinesInWindow;
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
      NewCaret.Y:= fTextView.TextPosAddLines(NewCaret.Y, -1);
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
      NewCaret.Y:=fTextView.TextPosAddLines(NewCaret.Y, +1);
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
begin
  OldCaret:=CaretXY;
  NewCaret:=OldCaret;
  NewCaret.Y:=fTextView.TextPosAddLines(NewCaret.Y, DY);
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
     {$IFDEF SYN_LAZARUS}CharExtra := fExtraCharSpacing;{$ENDIF}
    fCharWidth := CharWidth;
  end;
  {$IFDEF SYN_LAZARUS}
  FUseUTF8:=fTextDrawer.UseUTF8;
  TSynEditStrings(fLines).IsUtf8 := FUseUTF8;
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

  BeginUndoBlock;
  try
    i := 0;
    OldCaretX := CaretX;
    SelText := '';
    // With a multi-line block the caret may have advanced, avoid negative spaces
    if CaretX > OldCaretX then
      OldCaretX := CaretX;
    if eoSmartTabs in fOptions then begin
      iLine := CaretY - 1;
      if (iLine > 0) and (iLine < Lines.Count) then begin
        repeat
          Dec(iLine);
          if iLine < 0 then break;
          PrevLine := Lines[iLine];
        until PhysicalLineLength(PChar(PrevLine),length(PrevLine),true) > OldCaretX - 1;

        if iLine >= 0 then begin
          p := @PrevLine[PhysicalToLogicalCol(PrevLine,OldCaretX)];
          // scan over non-whitespaces
          while not (p^ in [#0, #9, #32]) do
            inc(p);
          // scan over whitespaces
          while (p^ in [#9, #32]) do
            inc(p);
          i := LogicalToPhysicalCol(PrevLine, p-@PrevLine[1]+1) - CaretX;
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
    EndUndoBlock;
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
  BB,BE            : TPoint;
  Run,
  StrToInsert      : PChar;
  e,x,
  i,InsertStrLen   : integer;
  Spaces           : String;
  OrgSelectionMode : TSynSelectionMode;
  {$IFDEF SYN_LAZARUS}
  BlockBackward : Boolean;
  {$ELSE}
  OrgCaretPos: TPoint;
  {$ENDIF}
begin
  if not SelAvail then exit;
  OrgSelectionMode := FBlockSelection.ActiveSelectionMode;
  {$IFDEF SYN_LAZARUS}
  BlockBackward:= FBlockSelection.IsBackwardSel;
  {$ELSE}
  OrgCaretPos := CaretXY;
  {$ENDIF}
  x := 1;
  StrToInsert := nil;
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

      InsertBlock(Point(1,BB.y), StrToInsert);
      {$IFDEF SYN_LAZARUS}
      if BlockBackward then
        SwapPoint(BB, BE);
      {$ENDIF}
      fUndoList.AddChange(crIndent, BB, BE,
                          {$IFDEF SYN_LAZARUS}chr(fBlockIndent){$ELSE}''{$ENDIF},
                          FBlockSelection.ActiveSelectionMode);
    finally
      StrDispose(StrToInsert);
    end;
  finally
    {$IFDEF SYN_LAZARUS}
    if BlockBackward then Begin
      inc(BE.x, fBlockIndent);
      BB.x := x;
    end else begin
      inc(BB.x, fBlockIndent);
      BE.x := x;
    end;
    TSynEditStringTrimmingList(fTrimLines).ForceTrim; // Otherwise it may reset the block
    SetCaretAndSelection(LogicalToPhysicalPos(BE), BB, BE);
    {$ELSE}
    SetCaretAndSelection(OrgCaretPos,
      Point(BB.x + fTabWidth, BB.y), Point(x, BE.y));
    {$ENDIF}
    FBlockSelection.ActiveSelectionMode := OrgSelectionMode;
  end;
end;

procedure TCustomSynEdit.DoBlockUnindent;
var
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
  {$IFDEF SYN_LAZARUS}
  BlockBackward : Boolean;
  {$ELSE}
  OrgCaretPos: TPoint;
  {$ENDIF}

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
  OrgSelectionMode := FBlockSelection.ActiveSelectionMode;
  Len := 0;
  LastIndent := 0;
  if SelAvail then
  begin
    // store current selection detail
    BB := BlockBegin;
    BE := BlockEnd;
    {$IFDEF SYN_LAZARUS}
    BlockBackward:=  FBlockSelection.IsBackwardSel;
    {$ELSE}
    OrgCaretPos := CaretXY;
    {$ENDIF}

    // convert selection to complete lines
    if BE.X = 1 then
      e := BE.y - 1
    else
      e := BE.y;

    // build string to delete
    StrToDeleteLen :=
      {$IFDEF SYN_LAZARUS}
       (fBlockIndent+length(LineEnding)) * (e - BB.y + 1)
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
      for x := BB.Y to e{$IFNDEF SYN_LAZARUS}-1{$ENDIF} do
      begin
        Line := PChar(Lines[x-1]);
        TempString:=StringOfChar(' ', GetDelLen);
        StrCat(FullStrToDelete,PChar(TempString));
        StrCat(FullStrToDelete,
                    PChar({$IFDEF SYN_LAZARUS}LineEnding{$ELSE}#13#10{$ENDIF}));
      end;
      {$IFNDEF SYN_LAZARUS}
      Line := PChar(Lines[e-1]);
      TempString:=StringOfChar(' ', GetDelLen);
      StrCat(FullStrToDelete,PChar(TempString));
      {$ENDIF}

      FirstIndent := -1;
      // Delete string
      if SomethingToDelete then
      begin
        StrToDelete := FullStrToDelete;
        CaretY := BB.Y;
        repeat
          Run := GetEOL(StrToDelete);
          {$IFNDEF SYN_LAZARUS}
          if Run <> StrToDelete then
          begin
          {$ENDIF}
            Len := Run - StrToDelete;
            if FirstIndent = -1 then
              FirstIndent := Len;
            if Len > 0 then begin
              TempString := Lines[CaretY - 1];
              Delete(TempString, 1, Len);
              Lines[CaretY - 1] := TempString;
            end;
          {$IFNDEF SYN_LAZARUS}
          end;
          {$ENDIF}
          {$IFDEF SYN_LAZARUS}
          if Run^ in [#10,#13] then  begin
            if (Run[1] in [#10,#13]) and (Run^<>Run[1]) then
              Inc(Run,2)
            else
              Inc(Run);
            CaretY := CaretY + 1;
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
        {$IFDEF SYN_LAZARUS}
        if BlockBackward then Begin
          SwapPoint(BB, BE);
          SwapInt(FirstIndent, LastIndent);
        end;
        {$ENDIF}
        fUndoList.AddChange(crUnindent, BB, BE,
                            {$IFDEF SYN_LAZARUS}FullStrToDelete{$ELSE}StrToDelete{$ENDIF},
                            FBlockSelection.ActiveSelectionMode);
      end;
      {$IFDEF SYN_LAZARUS}
      if FirstIndent = -1 then begin
        // Nothing changed; ensure correct restore
        if BlockBackward then
          SwapPoint(BB, BE);
      end else begin
        dec(BB.x, FirstIndent);
        dec(BE.x, LastIndent);
      end;
      TSynEditStringTrimmingList(fTrimLines).ForceTrim; // Otherwise it may reset the block
      SetCaretAndSelection(LogicalToPhysicalPos(BE), BB, BE);
      {$ELSE}
      if FirstIndent = -1 then
        FirstIndent := 0;
      SetCaretAndSelection(OrgCaretPos, Point(BB.x - FirstIndent, BB.Y),
        Point(BE.x - LastIndent, BE.y));
      {$ENDIF}
      // restore selection
      FBlockSelection.ActiveSelectionMode := OrgSelectionMode;
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
      s:=Lines[CaretXY.Y-1];

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
  if Value <> fModified then
  begin
    fModified := Value;
    {$IFDEF SYN_LAZARUS}
    if not fModified then
    begin
      // the current state should be the unmodified state.
      fUndoList.MarkTopAsUnmodified;
      fRedoList.MarkTopAsUnmodified;
    end;
    {$ENDIF}
    StatusChanged([scModified]);
  end;
end;

{$IFNDEF SYN_LAZARUS}
function TCustomSynEdit.DoOnSpecialLineColors(Line: integer; var Foreground,
  Background: TColor): boolean;
begin
  Result := FALSE;
  if Assigned(fOnSpecialLineColors) then
    fOnSpecialLineColors(Self, Line, Result, Foreground, Background);
end;
{$ENDIF}

procedure TCustomSynEdit.InvalidateLine(Line: integer);
var
  rcInval: TRect;
begin
  if Visible and (Line >= TopLine) and
    (Line <= {$IFDEF SYN_LAZARUS}ScreenRowToRow(LinesInWindow){$ELSE}
    TopLine + LinesInWindow{$ENDIF})
    and (Line <= Lines.Count) and HandleAllocated
  then begin
    // we invalidate gutter and text area of this line
    rcInval := Rect(0, fTextHeight * RowToScreenRow(Line)
        , ClientWidth{$IFDEF SYN_LAZARUS}-ScrollBarWidth{$ENDIF}, 0);
    rcInval.Bottom := rcInval.Top + fTextHeight;
    if sfLinesChanging in fStateFlags then
      UnionRect(fInvalidateRect, fInvalidateRect, rcInval)
    else begin
      {$IFDEF VerboseSynEditInvalidate}
      DebugLn(['TCustomSynEdit.InvalidateLines ',dbgs(rcInval)]);
      {$ENDIF}
      InvalidateRect(Handle, @rcInval, FALSE);
    end;
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
        fHighlighter.SetRange(TSynEditStrings(Lines).Ranges[PosY - 1]);
        fHighlighter.SetLine(Line, PosY - 1);
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
    end else if MoveCaret then
      CaretXY := LogicalToPhysicalPos(Result)
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
        Line := Lines[PosY - 1];
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
        if PosY = Lines.Count then break;
        Inc(PosY);
        if OnlyVisible
        and ((PosY < TopLine) or (PosY >= ScreenRowToRow(LinesInWindow)))
        then
          break;
        Line := Lines[PosY - 1];
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
  if (PosY<1) or (PosY>Lines.Count) then exit;
  if OnlyVisible
  and ((PosY<TopLine) or (PosY >= ScreenRowToRow(LinesInWindow)))
  then
   exit;

  Line := Lines[PosY - 1];
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
    {$IFDEF SYN_LAZARUS}
    Highlighter.SetRange(TSynEditStrings(Lines).Ranges[PosY]);
    {$ELSE}
    Highlighter.SetRange(TSynEditStringList(Lines).Ranges[PosY]);
    {$ENDIF}
    Highlighter.SetLine(Line, PosY);
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
        IdChars := [#1..#255] - (Highlighter.WordBreakChars + TSynWhiteChars)
      else
        IdChars := [#1..#255] - (TSynWordBreakChars + TSynWhiteChars);
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
  if fBeautifier<>nil then begin
    if IgnoreCurrentLineText then
      s:=''
    else
      s:=LineText;
    Result:=fBeautifier.GetIndentForLineBreak(Self,Point(1,Line),s);
  end else begin
    // default: use last non empty line indent, ignore always current line
    y:=Line-1;
    if y>Lines.Count then y:=Lines.Count;
    while y>=1 do begin
      s:=Lines[y-1];
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
var
  Item: TSynEditUndoItem;
begin
//  Modified := TRUE;
  {$IFDEF SYN_LAZARUS}
  Item := TSynEditUndoList(Sender).PeekItem;
  if Item <> nil then
    TSynEditStringList(fLines).MarkModified(Item.ChangeStartPos.y - 1,
      Item.ChangeEndPos.y - 1, Sender = fUndoList, Item.fChangeReason);
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
      {$IFDEF SYN_LAZARUS}
      if Assigned(Highlighter) then
        IdChars := [#1..#255] - (Highlighter.WordBreakChars + TSynWhiteChars)
      else
        IdChars := [#1..#255] - (TSynWordBreakChars + TSynWhiteChars);
      {$ELSE}
      if Assigned(Highlighter) then
        IdChars := Highlighter.IdentChars
      else
        IdChars := ['a'..'z', 'A'..'Z'];
      {$ENDIF}
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
begin
  Result := TSynEditStrings(Lines).LogicalToPhysicalPos(p);
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
  Result := TSynEditStrings(Lines).LogicalToPhysicalCol(PChar(Pointer(Line)),
    length(Line),LogicalPos,1,1);
end;

function TCustomSynEdit.LogicalToPhysicalCol(Line: PChar; LineLen: integer;
  LogicalPos, StartBytePos, StartPhysicalPos: integer): integer;
// Note: LogicalPos, StartBytePos, StartPhysicalPos start at 1
begin
  Result := TSynEditStrings(Lines).LogicalToPhysicalCol(Line, LineLen, LogicalPos,
                                        StartBytePos, StartPhysicalPos);
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
begin
  Result := TSynEditStrings(Lines).PhysicalToLogicalPos(p);
end;

function TCustomSynEdit.PhysicalToLogicalCol(const Line: string;
  PhysicalPos: integer): integer;
begin
  Result := TSynEditStrings(Lines).PhysicalToLogicalCol(Line,PhysicalPos,1,1);
end;

function TCustomSynEdit.PhysicalToLogicalCol(const Line: string;
  PhysicalPos, StartBytePos, StartPhysicalPos: integer): integer;
begin
  Result := TSynEditStrings(Lines).PhysicalToLogicalCol(Line, PhysicalPos,
    StartBytePos, StartPhysicalPos);
end;

function TCustomSynEdit.ScreenColumnToXValue(Col : integer) : integer;
begin
  Result := fTextOffset + Pred(Col) * fCharWidth;
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

{$IFNDEF SYN_LAZARUS}
procedure TCustomSynEdit.TrimmedSetLine(ALine: integer; ALineText: string);
begin
  if eoTrimTrailingSpaces in Options then
    Lines[ALine] := TrimRight(ALineText)
  else
    Lines[ALine] := ALineText;
end;
{$ENDIF}

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
{ TSynCustomBeautifier }

function TSynCustomBeautifier.LeftSpaces(Editor: TCustomSynEdit;
  const Line: string; Physical: boolean): Integer;
var
  p: PChar;
begin
  p := pointer(Line);
  if Assigned(p) then begin
    Result := 0;
    while p^ in [#1..#32] do begin
      Inc(p);
      Inc(Result);
    end;
    if Physical and (Result>0) then
      Result:=Editor.LogicalToPhysicalCol(Line,Result+1)-1;
  end else
    Result := 0;
end;

function TSynCustomBeautifier.GetIndentForLineBreak(Editor: TCustomSynEdit;
  InsertPos: TPoint; var NextText: string): integer;
var
  LastTextY: LongInt;
  Line: string;
  Lines: TStrings;
begin
  Result:=0;
  if InsertPos.Y<1 then exit;
  LastTextY:=InsertPos.Y;
  Lines:=Editor.Lines;
  if LastTextY>Lines.Count then
    LastTextY:=Lines.Count;
  while (LastTextY>0) do begin
    Line:=Lines[LastTextY-1];
    if LastTextY=InsertPos.Y then
      Line:=copy(Line,1,InsertPos.X-1);
    if Line<>'' then begin
      Result:=LeftSpaces(Editor,Line,false);
      exit;
    end;
    dec(LastTextY);
  end;
end;
{$ENDIF}

initialization
  {$IFNDEF SYN_LAZARUS}
  SynEditClipboardFormat := RegisterClipboardFormat(SYNEDIT_CLIPBOARD_FORMAT);
  {$ENDIF}
    end.
