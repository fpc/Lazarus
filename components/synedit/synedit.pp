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
  Types, LCLIntf, LCLType, LMessages, LCLProc,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils, Classes, Messages, Controls, Graphics, Forms, StdCtrls, ExtCtrls, Menus,
{$IFDEF SYN_MBCSSUPPORT}
  Imm,
{$ENDIF}
  SynEditTypes, SynEditSearch, SynEditKeyCmds, SynEditMouseCmds, SynEditMiscProcs,
  SynEditPointClasses, SynBeautifier, SynEditMarks,
{$ifdef SYN_LAZARUS}
  SynEditMarkup, SynEditMarkupHighAll, SynEditMarkupBracket, SynEditMarkupWordGroup,
  SynEditMarkupCtrlMouseLink, SynEditMarkupSpecialLine, SynEditMarkupSelection,
  SynEditTextBase, SynEditTextTrimmer, SynEditFoldedView, SynEditTextTabExpander,
  SynEditTextDoubleWidthChars,
  SynGutterBase, SynGutter, SynGutterCodeFolding, SynGutterChanges,
  SynGutterLineNumber, SynGutterMarks,
{$ENDIF}
  SynEditMiscClasses, SynEditTextBuffer, SynEditHighlighter, SynTextDrawer,
  SynEditLines,
  LResources, Clipbrd
{$IFDEF SYN_COMPILER_4_UP}
  , StdActns
{$ENDIF}
  ;

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
  SynDefaultFontName = 'Courier New';
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
    var AChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
    Data: pointer; HandlerData: pointer) of object;

  TPaintEvent = procedure(Sender: TObject; ACanvas: TCanvas) of object;

  TProcessCommandEvent = procedure(Sender: TObject;
    var Command: TSynEditorCommand;
    var AChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
    Data: pointer) of object;

  TReplaceTextEvent = procedure(Sender: TObject; const ASearch, AReplace:
    string; Line, Column: integer; var ReplaceAction: TSynReplaceAction) of object;

  TSynEditCaretType = (ctVerticalLine, ctHorizontalLine, ctHalfBlock, ctBlock);

  TSynStateFlag = (sfCaretChanged, sfScrollbarChanged, sfLinesChanging,
    sfIgnoreNextChar, sfCaretVisible, sfDblClicked, sfGutterClick,
    sfTripleClicked, sfQuadClicked, sfPainting,
    sfWaitForDragging, sfIsDragging, sfMouseSelecting
    );                                           //mh 2000-10-30
  TSynStateFlags = set of TSynStateFlag;

  TSynEditorOption = (
    eoAltSetsColumnMode,       // DEPRECATED, now controlled vie MouseActions
                               // Holding down the Alt Key will put the selection mode into columnar format
    eoAutoIndent,              // Will indent the caret on new lines with the same amount of leading white space as the preceding line
    eoAutoSizeMaxScrollWidth,  //TODO Automatically resizes the MaxScrollWidth property when inserting text
    eoDisableScrollArrows,     //TODO Disables the scroll bar arrow buttons when you can't scroll in that direction any more
    eoDragDropEditing,         // DEPRECATED, now controlled vie MouseActions
                               // Allows you to select a block of text and drag it within the document to another location
    eoDropFiles,               //TODO Allows the editor accept file drops
    eoEnhanceHomeKey,          // home key jumps to line start if nearer, similar to visual studio
    eoGroupUndo,               // When undoing/redoing actions, handle all continous changes of the same kind in one call instead undoing/redoing each command separately
    eoHalfPageScroll,          // When scrolling with page-up and page-down commands, only scroll a half page at a time
    eoHideShowScrollbars,      //TODO if enabled, then the scrollbars will only show when necessary.  If you have ScrollPastEOL, then it the horizontal bar will always be there (it uses MaxLength instead)
    eoKeepCaretX,              // When moving through lines w/o Cursor Past EOL, keeps the X position of the cursor
    eoNoCaret,                 // Makes it so the caret is never visible
    eoNoSelection,             // Disables selecting text
    eoRightMouseMovesCursor,   // Deprecated, now controlled vie MouseActions
                               // When clicking with the right mouse for a popup menu, move the cursor to that location
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
    eoBracketHighlight,        // Highlight matching bracket
    eoDoubleClickSelectsLine,  // DEPRECATED
                               // Select line on double click
    eoHideRightMargin,         // Hides the right margin line
    eoPersistentCaret,         // Do not hide caret when focus lost // TODO: Windows may hide it, if another component sets up a caret
    eoShowCtrlMouseLinks,      // DEPRECATED, now controlled vie MouseActions
                               // Pressing Ctrl (SYNEDIT_LINK_MODIFIER) will highlight the word under the mouse cursor
    eoAutoIndentOnPaste,       // Indent text inserted from clipboard
    eoSpacesToTabs             // Converts space characters to tabs and spaces
    );
  TSynEditorOptions = set of TSynEditorOption;

  TSynEditorOption2 = (
    eoCaretSkipsSelection,     // Caret skips selection on VK_LEFT/VK_RIGHT
    eoAlwaysVisibleCaret,      // Move caret to be always visible when scrolling
    eoEnhanceEndKey,           // end key jumps to visual/hard line end whichever is nearer
    eoFoldedCopyPaste          // Remember folds in copy/paste operations
  );
  TSynEditorOptions2 = set of TSynEditorOption2;

const
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
    eoScrollHintFollows,       //TODO The scroll hint follows the mouse when scrolling vertically
    eoShowScrollHint,          // Shows a hint of the visible line numbers when scrolling vertically
    eoSmartTabDelete,          //TODO similar to Smart Tabs, but when you delete characters
    ////eoSpecialLineDefaultFg,    //TODO disables the foreground text color override when using the OnSpecialLineColor event
    eoAutoIndentOnPaste,       // Indent text inserted from clipboard
    eoSpacesToTabs             // Converts space characters to tabs and spaces
  ];

  {$IFDEF SYN_LAZARUS}
  SYNEDIT_DEFAULT_OPTIONS2 = [
    eoFoldedCopyPaste
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
    FCaret: TSynEditCaret;
    FInternalCaret: TSynEditCaret;
    fCtrlMouseActive: boolean;
    fMarkupManager : TSynEditMarkupManager;
    fMarkupHighAll : TSynEditMarkupHighlightAll;
    fMarkupHighCaret : TSynEditMarkupHighlightAllCaret;
    fMarkupBracket : TSynEditMarkupBracket;
    fMarkupWordGroup : TSynEditMarkupWordGroup;
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
    fHighlighterNeedsUpdateStartLine: integer; // 1 based, 0 means invalid
    fHighlighterNeedsUpdateEndLine: integer; // 1 based, 0 means invalid
    FBeautifier: TSynCustomBeautifier;
    fExtraCharSpacing: integer;
    {$ENDIF}
    FFoldedLinesView:  TSynEditFoldedView;
    FTrimmedLinesView: TSynEditStringTrimmingList;
    FDoubleWidthChrLinesView: SynEditStringDoubleWidthChars;
    FTabbedLinesView:  TSynEditStringTabExpander;
    FTheLinesView: TSynEditStrings;
    FLines: TSynEditStrings;          // The real (un-mapped) line-buffer
    FStrings: TStrings;               // External TStrings based interface to the Textbuffer
    fLinesInWindow: Integer;// MG: fully visible lines in window
    fLeftChar: Integer;    // first visible screen column
    fMaxLeftChar: Integer; // 1024
    fPaintLock: Integer;
    fReadOnly: Boolean;
    fRightEdge: Integer;
    fRightEdgeColor: TColor;
    FScrollBars: TScrollStyle;
    fTextHeight: Integer;
    fTextOffset: Integer;
    fTopLine: Integer;
    FOldTopView, FTopDelta: Integer; // TopView before IncPaintLock
    fHighlighter: TSynCustomHighlighter;
    {$IFNDEF SYN_LAZARUS}
    fSelectedColor: TSynSelectedColor;
    {$ENDIF}
    fUndoList: TSynEditUndoList;
    fRedoList: TSynEditUndoList;
    FIsUndoing: Boolean;
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
    FMouseActions, FMouseSelActions: TSynEditMouseActions;
    fModified: Boolean;
    fMarkList: TSynEditMarkList;
    fExtraLineSpacing: integer;
    FUseUTF8: boolean;
    fWantTabs: boolean;
    FGutter: TSynGutter;
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
    FInMouseClickEvent: Boolean;
    // event handlers
    fOnChange: TNotifyEvent;
    fOnClearMark: TPlaceMarkEvent;                                              // djlp 2000-08-29
    fOnCommandProcessed: TProcessCommandEvent;
    fOnDropFiles: TSynDropFilesEvent;
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
    fChangeStamp: int64;
    {$ENDIF}

    procedure AquirePrimarySelection;
    function GetDefSelectionMode: TSynSelectionMode;
    function GetUndoList: TSynEditUndoList;
    function GetDividerDrawLevel: Integer; deprecated;
    procedure SetDefSelectionMode(const AValue: TSynSelectionMode);
    procedure SetDividerDrawLevel(const AValue: Integer); deprecated;
    procedure SetMouseActions(const AValue: TSynEditMouseActions);
    procedure SetMouseSelActions(const AValue: TSynEditMouseActions);
    procedure SurrenderPrimarySelection;
    procedure BookMarkOptionsChanged(Sender: TObject);
    procedure ComputeCaret(X, Y: Integer);
    procedure DoBlockIndent;
    procedure DoBlockUnindent;
    procedure DoHomeKey(Selection: boolean);
    procedure DoEndKey(Selection: boolean);
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
    function GetFoldedCodeColor: TSynSelectedColor;
    function GetMarkup(Index: integer): TSynEditMarkup;
    function GetMarkupByClass(Index: TSynEditMarkupClass): TSynEditMarkup;
    {$IFDEF SYN_LAZARUS}
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
    {$ENDIF}
    function GetHookedCommandHandlersCount: integer;
    function GetLineText: string;
    {$IFDEF SYN_LAZARUS}
    function GetCharLen(const Line: string; CharStartPos: integer): integer;
    function GetLogicalCaretXY: TPoint;
    procedure SetLogicalCaretXY(const NewLogCaretXY: TPoint);
    procedure SetBeautifier(NewBeautifier: TSynCustomBeautifier);
    {$ENDIF}
    function GetMaxUndo: Integer;
    function GetSelAvail: Boolean;
    function GetSelText: string;
    procedure SetTrimSpaceType(const AValue: TSynEditStringTrimmingType);
    function SynGetText: string;
    procedure GutterChanged(Sender: TObject);
    procedure InsertBlock(BB: TPoint; ChangeStr: PChar);
    function IsPointInSelection(Value: TPoint): boolean;
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
    function ScanFrom(var Index: integer; AtLeastTilIndex: integer = -1): integer;
    procedure SelectedColorsChanged(Sender: TObject);
    procedure DoBlockSelectionChanged(Sender: TObject);
    procedure SetBlockBegin(Value: TPoint);
    procedure SetBlockEnd(Value: TPoint);
    {$IFDEF SYN_LAZARUS}
    procedure SetBlockIndent(const AValue: integer);
    {$ELSE}
    procedure SetBorderStyle(Value: TBorderStyle);
    {$ENDIF}
    procedure SetCaretAndSelection(const ptCaret, ptBefore, ptAfter: TPoint;
                                   Mode: TSynSelectionMode = smCurrent);
    procedure SetCaretX(Value: Integer);
    procedure SetCaretY(Value: Integer);
    procedure SetExtraLineSpacing(const Value: integer);
    procedure SetGutter(const Value: TSynGutter);
    procedure SetHideSelection(const Value: boolean);
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure RemoveHooksFromHighlighter;
    procedure SetInsertCaret(const Value: TSynEditCaretType);
    procedure SetInsertMode(const Value: boolean);
    procedure SetKeystrokes(const Value: TSynEditKeyStrokes);
    {$ifdef SYN_LAZARUS}
    procedure SetExtraCharSpacing(const Value: integer);
    procedure SetLastMouseCaret(const AValue: TPoint);
    {$ENDIF}
    procedure SetLeftChar(Value: Integer);
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
    procedure ScrollAfterTopLineChanged;
    procedure SetWantTabs(const Value: boolean);
    procedure SetWordBlock(Value: TPoint);
    {$IFDEF SYN_LAZARUS}
    procedure SetLineBlock(Value: TPoint; WithLeadSpaces: Boolean = True);
    procedure SetParagraphBlock(Value: TPoint);
    {$ENDIF}
    procedure SizeOrFontChanged(bFont: boolean);
    procedure StatusChanged(AChanges: TSynStatusChanges);
    {$IFNDEF SYN_LAZARUS}
    procedure TrimmedSetLine(ALine: integer; ALineText: string);
    {$ENDIF}
    procedure UpdateModified;
    procedure UndoRedoAdded(Sender: TObject);
    procedure UnlockUndo;
    procedure UpdateCaret;
    procedure UpdateScrollBars;
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
    procedure DoContextPopup(const MousePos: TPoint; var Handled: Boolean); override;

    procedure FindAndHandleMouseAction(AButton: TMouseButton; AShift: TShiftState;
                                X, Y: Integer; ACCount:TSynMAClickCount;
                                ADir: TSynMAClickDir);
    function DoHandleMouseAction(AnActionList: TSynEditMouseActions;
                                 AnInfo: TSynEditMouseActionInfo): Boolean;

    {$IFDEF SYN_LAZARUS}
    procedure Resize; override;
    function  RealGetText: TCaption; override;
    procedure RealSetText(const Value: TCaption); override;
    procedure IncreaseChangeStamp;
    {$ENDIF}
    function GetLines: TStrings; override;
    function GetViewedTextBuffer: TSynEditStrings; override;
    function GetTextBuffer: TSynEditStrings; override;
    procedure SetLines(Value: TStrings);  override;
    procedure ScanFromAfterLock;
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
    procedure UTF8KeyPress(var Key: TUTF8Char); override;
    procedure KeyPress(var Key: Char); override;
    {$IFDEF SYN_LAZARUS}
    procedure KeyUp(var Key : Word; Shift : TShiftState); override;
    {$ENDIF}
    Procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    Procedure LineTextChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    procedure LinesChanging(Sender: TObject);
    procedure LinesChanged(Sender: TObject);
    procedure ListCleared(Sender: TObject);
    {$IFDEF SYN_LAZARUS}
    procedure FoldChanged(Index: integer);
    function GetTopView : Integer;
    procedure SetTopView(const AValue : Integer);
    {$ENDIF}
    procedure Loaded; override;
    procedure MarkListChange(Sender: TObject);
{$IFDEF SYN_MBCSSUPPORT}
    procedure MBCSGetSelRangeInLineWhenColumnSelectionMode(const s: string;
      var ColFrom, ColTo: Integer);
{$ENDIF}
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
    procedure RedoItem(Item: TSynEditUndoItem);
    procedure SetCaretXY(Value: TPoint); virtual;
    procedure CaretChanged(Sender: TObject);
    procedure SetName(const Value: TComponentName); override;
    procedure SetReadOnly(Value: boolean); virtual;
    procedure SetSelTextPrimitive(PasteMode: TSynSelectionMode; Value: PChar;
      AddToUndoList: Boolean = false);
    procedure ShowCaret;
    // If the translations requires Data, memory will be allocated for it via a
    // GetMem call.  The client must call FreeMem on Data if it is not NIL.
    function TranslateKeyCode(Code: word; Shift: TShiftState; var Data: pointer
      {$IFDEF SYN_LAZARUS};out IsStartOfCombo: boolean{$ENDIF}): TSynEditorCommand;
    procedure UndoItem(Item: TSynEditUndoItem);
    property  UndoList: TSynEditUndoList read GetUndoList;
  protected
    fGutterWidth: Integer;
    {$IFDEF EnableDoubleBuf}
    BufferBitmap: TBitmap; // the double buffer
    {$ENDIF}
    SavedCanvas: TCanvas; // the normal TCustomControl canvas during paint
    function GetChildOwner: TComponent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
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
    property TextView : TSynEditFoldedView read FFoldedLinesView;
    property TopView: Integer read GetTopView write SetTopView;  // TopLine converted into Visible(View) lines
    {$ENDIF}
    function PasteFromClipboardEx(ClipHelper: TSynClipboardStream): Boolean;
  public
    procedure FindMatchingBracket; virtual;
    {$IFDEF SYN_LAZARUS}
    function FindMatchingBracket(PhysStartBracket: TPoint;
                                 StartIncludeNeighborChars, MoveCaret,
                                 SelectBrackets, OnlyVisible: Boolean
                                 ): TPoint; virtual;
    //code fold
    procedure CodeFoldAction(iLine: integer); deprecated;
    function FindNextUnfoldedLine(iLine: integer; Down: boolean): Integer;
    procedure UnfoldAll;
    procedure FoldAll(StartLevel : Integer = 0; IgnoreNested : Boolean = False);
    procedure EraseBackground(DC: HDC); override;
    {$ENDIF}

    procedure AddKey(Command: TSynEditorCommand; Key1: word; SS1: TShiftState;
      Key2: word; SS2: TShiftState);
    procedure AfterLoadFromFile;
    procedure BeginUndoBlock(aList: TSynEditUndoList = nil);
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
    procedure DoCopyToClipboard(const SText: string; FoldInfo: String = '');
    procedure DragDrop(Source: TObject; X, Y: Integer); override;
    procedure EndUndoBlock(aList: TSynEditUndoList = nil);
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
    procedure SelectLine(WithLeadSpaces: Boolean = True);
    procedure SelectParagraph;
    procedure SetUseIncrementalColor(const AValue : Boolean);
    {$ENDIF}
    procedure SetBookMark(BookMark: Integer; X: Integer; Y: Integer);
    procedure SetDefaultKeystrokes; virtual;
    procedure SetOptionFlag(Flag: TSynEditorOption; Value: boolean);
    procedure SetSelWord;
    procedure Undo;
    function GetLineState(ALine: Integer): TSynLineState;
    function HasDebugMark(ALine: Integer): Boolean;
    procedure SetDebugMarks(AFirst, ALast: Integer);
    procedure ClearDebugMarks;
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
    property GutterWidth: Integer read fGutterWidth;
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
    property Modified: Boolean read fModified write SetModified;
    property PaintLock: Integer read fPaintLock;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default FALSE;
    property SelAvail: Boolean read GetSelAvail;
    property SelText: string read GetSelText write SetSelTextExternal;
    property TopLine: Integer read fTopLine write SetTopLine;
    {$IFDEF SYN_LAZARUS}
    property UseUTF8: boolean read FUseUTF8;
    procedure Update; override;
    procedure Invalidate; override;
    property ChangeStamp: int64 read fChangeStamp;
    {$ENDIF}
  public
    property OnKeyDown;
    property OnKeyPress;
    property OnProcessCommand: TProcessCommandEvent
      read FOnProcessCommand write FOnProcessCommand;
    function MarkupCount: Integer;
    property Markup[Index: integer]: TSynEditMarkup
      read GetMarkup;
    property MarkupByClass[Index: TSynEditMarkupClass]: TSynEditMarkup
      read GetMarkupByClass;
    property TrimSpaceType: TSynEditStringTrimmingType
      read GetTrimSpaceType write SetTrimSpaceType;
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
    property MouseActions: TSynEditMouseActions
      read FMouseActions write SetMouseActions;
    property MouseSelActions: TSynEditMouseActions // Mouseactions, if mouse is over selection => fallback to normal
      read FMouseSelActions write SetMouseSelActions;
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
    property BracketMatchColor: TSynSelectedColor read GetBracketMatchColor;
    property MouseLinkColor: TSynSelectedColor read GetMouseLinkColor;
    property LineHighlightColor: TSynSelectedColor read GetLineHighlightColor;
    property FoldedCodeColor: TSynSelectedColor read GetFoldedCodeColor;
    property BracketHighlightStyle: TSynEditBracketHighlightStyle
      read GetBracketHighlightStyle write SetBracketHighlightStyle;
    {$ELSE}
    property SelectedColor: TSynSelectedColor
      read FSelectedColor write FSelectedColor;
    {$ENDIF}
    property DefaultSelectionMode: TSynSelectionMode
      read GetDefSelectionMode write SetDefSelectionMode default smNormal;
    property SelectionMode: TSynSelectionMode
      read GetSelectionMode write SetSelectionMode default smNormal;
    // See Highlighter for new methods
    property CFDividerDrawLevel: Integer
      read GetDividerDrawLevel write SetDividerDrawLevel; deprecated;
    property TabWidth: integer read fTabWidth write SetTabWidth default 8;
    property WantTabs: boolean read fWantTabs write SetWantTabs default FALSE;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClearBookmark: TPlaceMarkEvent read fOnClearMark
      write fOnClearMark;
    property OnCommandProcessed: TProcessCommandEvent
      read fOnCommandProcessed write fOnCommandProcessed;
    property OnDropFiles: TSynDropFilesEvent read fOnDropFiles write fOnDropFiles;
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
{$IFNDEF SYN_LAZARUS}
    property Ctl3D;
    property ParentCtl3D;
{$ENDIF}
{$IFDEF SYN_COMPILER_4_UP}
    property Anchors;
    property Constraints;
{$ENDIF}
    property Color;
    {$IFDEF SYN_LAZARUS}
    property Cursor default crIBeam;
    {$ENDIF}
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
    property MouseActions;
    property MouseSelActions;
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
    property FoldedCodeColor;
    property MouseLinkColor;
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

procedure Register;

implementation

// { $R SynEdit.res}

type

  { TSynEditUndoCaret }

  TSynEditUndoCaret = class(TSynEditUndoItem)
  private
    FCaretPos: TPoint;
  protected
    function IsEqualContent(AnItem: TSynEditUndoItem): Boolean; override;
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
  public
    function IsCaretInfo: Boolean; override;
    constructor Create(CaretPos, BeginPos, EndPos: TPoint; BlockMode: TSynSelectionMode);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynEditUndoIndent }

  TSynEditUndoIndent = class(TSynEditUndoItem)
  public
    FPosY1, FPosY2, FCnt: Integer;
  public
    constructor Create(APosY, EPosY, ACnt: Integer);
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

var
  SynDefaultBeautifier: TSynCustomBeautifier;

{ TSynEditUndoCaret }

function TSynEditUndoCaret.IsEqualContent(AnItem: TSynEditUndoItem): Boolean;
begin
  Result := (FCaretPos.x = TSynEditUndoCaret(AnItem).FCaretPos.x)
        and (FCaretPos.y = TSynEditUndoCaret(AnItem).FCaretPos.y);
end;

constructor TSynEditUndoCaret.Create(CaretPos: TPoint);
begin
  FCaretPos := CaretPos;
end;

function TSynEditUndoCaret.IsCaretInfo: Boolean;
begin
  Result := True;
end;

function TSynEditUndoCaret.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEdit;
  if Result then
    with TSynEdit(Caller) do begin
      CaretXY := FCaretPos;
      UndoList.AddChange(TSynEditUndoCaret.Create(FCaretPos));
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

function TSynEditUndoSelCaret.IsCaretInfo: Boolean;
begin
  Result := True;
end;

function TSynEditUndoSelCaret.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEdit;
  if Result then
    with TSynEdit(Caller) do begin
      SetCaretAndSelection(FCaretPos, FBeginPos, FEndPos, FBlockMode);
      UndoList.AddChange(TSynEditUndoSelCaret.Create(FCaretPos, FBeginPos,
                                                     FEndPos, FBlockMode));
    end;
end;

{ TSynEditUndoIndent }

constructor TSynEditUndoIndent.Create(APosY, EPosY, ACnt: Integer);
begin
  FPosY1 := APosY;
  FPosY2 := EPosY;
  FCnt :=  ACnt;
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

function TCustomSynEdit.GetDefSelectionMode: TSynSelectionMode;
begin
  Result := FBlockSelection.SelectionMode;
end;

function TCustomSynEdit.GetDividerDrawLevel: Integer;
begin
  Result := fHighlighter.DrawDividerLevel;
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
  Result := FFoldedLinesView.ScreenLineToTextIndex(ScreenRow)+1;
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

procedure TCustomSynEdit.DoCopyToClipboard(const SText: string; FoldInfo: String = '');
var
  ClipHelper: TSynClipboardStream;
begin
  if SText = '' then exit;
  Clipboard.Clear;
  ClipHelper := TSynClipboardStream.Create;
  try
    ClipHelper.Text := SText;
    ClipHelper.SelectionMode := SelectionMode;
    // Fold
    if length(FoldInfo) > 0 then
      ClipHelper.AddTag(synClipTagFold, @FoldInfo[1], length(FoldInfo));
    if not ClipHelper.WriteToClipboard(Clipboard) then
      raise ESynEditError.Create('Clipboard copy operation failed: AddFormat');
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

  fBeautifier := SynDefaultBeautifier;

  fLines := TSynEditStringList.Create;
  FCaret := TSynEditCaret.Create;
  FCaret.MaxLeftChar := @FMaxLeftChar;
  FCaret.AddChangeHandler({$IFDEF FPC}@{$ENDIF}CaretChanged);
  FInternalCaret := TSynEditCaret.Create;
  FInternalCaret.MaxLeftChar := @FMaxLeftChar;

  // Create the lines/views
  FTrimmedLinesView := TSynEditStringTrimmingList.Create(fLines, fCaret);

  FDoubleWidthChrLinesView := SynEditStringDoubleWidthChars.Create
                                                            (FTrimmedLinesView);

  FTabbedLinesView := TSynEditStringTabExpander.Create(FDoubleWidthChrLinesView);

  FFoldedLinesView := TSynEditFoldedView.Create(FTabbedLinesView, fCaret);
  FFoldedLinesView.OnFoldChanged := {$IFDEF FPC}@{$ENDIF}FoldChanged;

  // Pointer to the First/Lowest View
  // TODO: this should be Folded...
  FTheLinesView := FTabbedLinesView;
  // External Accessor
  FStrings := TSynEditLines.Create(FLines, {$IFDEF FPC}@{$ENDIF}MarkTextAsSaved);

  FCaret.Lines := FTheLinesView;
  FInternalCaret.Lines := FTheLinesView;

  TSynEditStringList(fLines).AddChangeHandler(senrLineCount,
                     {$IFDEF FPC}@{$ENDIF}LineCountChanged);
  TSynEditStringList(fLines).AddChangeHandler(senrLineChange,
                     {$IFDEF FPC}@{$ENDIF}LineTextChanged);
  with TSynEditStringList(fLines) do begin
    OnChange := {$IFDEF FPC}@{$ENDIF}LinesChanged;
    OnChanging := {$IFDEF FPC}@{$ENDIF}LinesChanging;
    OnCleared := {$IFDEF FPC}@{$ENDIF}ListCleared;
  end;

  fFontDummy := TFont.Create;
  fUndoList := TSynEditUndoList.Create;
  fUndoList.OnAddedUndo := {$IFDEF FPC}@{$ENDIF}UndoRedoAdded;
  fUndoList.OnNeedCaretUndo := {$IFDEF FPC}@{$ENDIF}GetCaretUndo;
  fRedoList := TSynEditUndoList.Create;
  fRedoList.OnAddedUndo := {$IFDEF FPC}@{$ENDIF}UndoRedoAdded;
  FIsUndoing := False;

  TSynEditStringList(FLines).UndoList := fUndoList;
  TSynEditStringList(FLines).RedoList := fRedoList;

  FBlockSelection := TSynEditSelection.Create(FTheLinesView);
  FBlockSelection.MaxLeftChar := @FMaxLeftChar;
  FBlockSelection.Caret := FCaret;
  FBlockSelection.UndoList := fUndoList;
  FBlockSelection.InvalidateLinesMethod := {$IFDEF FPC}@{$ENDIF}InvalidateLines;
  FBlockSelection.AddChangeHandler({$IFDEF FPC}@{$ENDIF}DoBlockSelectionChanged);

{$IFDEF SYN_COMPILER_4_UP}
{$IFNDEF SYN_LAZARUS}
// ToDo DoubleBuffered
  DoubleBuffered := false;
{$ENDIF}
{$ENDIF}
  {$IFNDEF EnableDoubleBuf}
  DoubleBuffered := True;
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
  fGutter := TSynGutter.Create(self, FFoldedLinesView, FTextDrawer);
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
  fMarkupHighCaret := TSynEditMarkupHighlightAllCaret.Create(self);
  fMarkupHighCaret.Selection := FBlockSelection;
  fMarkupHighAll   := TSynEditMarkupHighlightAll.Create(self);
  fMarkupBracket   := TSynEditMarkupBracket.Create(self);
  fMarkupWordGroup := TSynEditMarkupWordGroup.Create(self);
  fMarkupCtrlMouse := TSynEditMarkupCtrlMouseLink.Create(self);
  fMarkupSpecialLine := TSynEditMarkupSpecialLine.Create(self);
  fMarkupSelection := TSynEditMarkupSelection.Create(self, FBlockSelection);

  fMarkupManager := TSynEditMarkupManager.Create(self);
  fMarkupManager.AddMarkUp(fMarkupSpecialLine);
  fMarkupManager.AddMarkUp(fMarkupHighCaret);
  fMarkupManager.AddMarkUp(fMarkupHighAll);
  fMarkupManager.AddMarkUp(fMarkupCtrlMouse);
  fMarkupManager.AddMarkUp(fMarkupBracket);
  fMarkupManager.AddMarkUp(fMarkupWordGroup);
  fMarkupManager.AddMarkUp(fMarkupSelection);
  fMarkupManager.Lines := FTheLinesView;
  fMarkupManager.InvalidateLinesMethod := @InvalidateLines;

  Color := clWhite;
  fFontDummy.Name := SynDefaultFontName;
  fFontDummy.Height := SynDefaultFontHeight;
  fFontDummy.Pitch := SynDefaultFontPitch;
  fFontDummy.Quality := SynDefaultFontQuality;
  fLastMouseCaret := Point(-1,-1);
  fBlockIndent := 2;
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
  ScrollBars := ssBoth;
  {$IFDEF SYN_LAZARUS}
  BorderStyle := bsSingle;
  {$ELSE}
  fBorderStyle := bsSingle;
  {$ENDIF}
  fInsertCaret := ctVerticalLine;
  fOverwriteCaret := ctBlock;
  fKeystrokes := TSynEditKeyStrokes.Create(Self);
  if assigned(Owner) and not (csLoading in Owner.ComponentState) then begin
    SetDefaultKeystrokes;
  end;
  FMouseActions := TSynEditMouseTextActions.Create(Self);
  FMouseSelActions := TSynEditMouseSelActions.Create(Self);
  FMouseActions.ResetDefaults;
  FMouseSelActions.ResetDefaults;
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
  FTopDelta := 0;
  {$IFDEF SYN_LAZARUS}
  FFoldedLinesView.TopLine := 1;
  {$ELSE}
  fCaretX := 1;
  fCaretY := 1;
  {$ENDIF}
  fLastCaretX := 1;                                                             //mh 2000-10-19
  // find / replace
  fTSearch := TSynEditSearch.Create;
  fOptions := SYNEDIT_DEFAULT_OPTIONS;
  FTrimmedLinesView.Enabled := eoTrimTrailingSpaces in fOptions;
  FCaret.AllowPastEOL := (eoScrollPastEol in fOptions);
  {$IFDEF SYN_LAZARUS}
  fOptions2 := SYNEDIT_DEFAULT_OPTIONS2;
  {$ENDIF}
  fScrollTimer := TTimer.Create(Self);
  fScrollTimer.Enabled := False;
  fScrollTimer.Interval := 100;
  fScrollTimer.OnTimer := {$IFDEF FPC}@{$ENDIF}ScrollTimerHandler;
  fFirstLine := 1;
end;

function TCustomSynEdit.GetChildOwner: TComponent;
begin
  result := self;
end;

procedure TCustomSynEdit.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  Proc(FGutter.Parts);
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
    if NewStyleControls {$IFNDEF SYN_LAZARUS}and Ctl3D{$ENDIF} and (BorderStyle = bsSingle) then begin
      Style := Style and not Cardinal(WS_BORDER);
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
  end;
end;

procedure TCustomSynEdit.ScanFromAfterLock;
var
  LastLineChanged: LongInt;
begin
    if fHighlighterNeedsUpdateStartLine>0 then begin
      //DebugLn('TCustomSynEdit.DecPaintLock ',dbgs(fHighlighterNeedsUpdateStartLine),'-',dbgs(fHighlighterNeedsUpdateEndLine));
      if fHighlighterNeedsUpdateStartLine<=FTheLinesView.Count then begin
        if fHighlighterNeedsUpdateEndLine>FTheLinesView.Count then
          fHighlighterNeedsUpdateEndLine:=FTheLinesView.Count;
        LastLineChanged:=fHighlighterNeedsUpdateEndLine;
        // rescan all lines in range
        // Note: The highlighter range of the line can be invalid as well,
        //       so start scan one line earlier
        dec(fHighlighterNeedsUpdateStartLine, 2);
        LastLineChanged:=ScanFrom(fHighlighterNeedsUpdateStartLine,
                                  fHighlighterNeedsUpdateEndLine-1);
        //DebugLn('TCustomSynEdit.DecPaintLock ',dbgs(fHighlighterNeedsUpdateStartLine),'-',dbgs(fHighlighterNeedsUpdateEndLine),' LastLineChanged=',dbgs(LastLineChanged));
        InvalidateLines(fHighlighterNeedsUpdateStartLine,LastLineChanged+1);
        InvalidateGutterLines(fHighlighterNeedsUpdateStartLine,LastLineChanged+1);
      end;
      fHighlighterNeedsUpdateStartLine:=0;
      fHighlighterNeedsUpdateEndLine:=0;
    end;
end;

procedure TCustomSynEdit.DecPaintLock;
begin
  if (fPaintLock=1) and HandleAllocated then begin
    ScanFromAfterLock;
  end;
  FCaret.Unlock; // Maybe after FFoldedLinesView;
  FTrimmedLinesView.UnLock; // Must be unlocked after caret
  FFoldedLinesView.UnLock; // after ScanFrom, but before UpdateCaret
  Dec(fPaintLock);
  if (fPaintLock = 0) and HandleAllocated then begin
    ScrollAfterTopLineChanged;
    if sfScrollbarChanged in fStateFlags then
      UpdateScrollbars;
    if sfCaretChanged in fStateFlags then
      UpdateCaret;
    if fStatusChanges <> [] then
      DoOnStatusChange(fStatusChanges);
    fMarkupHighCaret.CheckState; // Todo need a global lock, including the markup
  end;
end;

destructor TCustomSynEdit.Destroy;
var
  i: integer;
begin
  SurrenderPrimarySelection;
  {$IFDEF SYN_LAZARUS}
  if HandleAllocated then LCLIntf.DestroyCaret(Handle);
  Beautifier:=nil;
  {$ENDIF}
  RemoveHooksFromHighlighter;
  FHighlighter := nil;
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
  FMouseActions.Free;
  FMouseSelActions.Free;
  fSelectedColor.Free;
  fUndoList.Free;
  fRedoList.Free;
  fGutter.Free;
  fTextDrawer.Free;
  fFontDummy.Free;
  fBlockSelection.Free;
  FStrings.Free;
  FTabbedLinesView.Free;
  FTrimmedLinesView.Free;
  FDoubleWidthChrLinesView.Free;
  Lines.Free;
  fCaret.Free;
  fInternalCaret.Free;
  {$ELSE}
  fHookedCommandHandlers:=nil;
  fPlugins:=nil;
  FreeAndNil(fScrollTimer);
  FreeAndNil(fTSearch);
  FreeAndNil(fMarkupManager);
  FreeAndNil(fMarkList);
  FreeAndNil(fBookMarkOpt);
  FreeAndNil(fKeyStrokes);
  FreeAndNil(FMouseActions);
  FreeAndNil(FMouseSelActions);
  FreeAndNil(fUndoList);
  FreeAndNil(fRedoList);
  FreeAndNil(fGutter);
  FreeAndNil(fTextDrawer);
  FreeAndNil(fFontDummy);
  FreeAndNil(FFoldedLinesView);
  FreeAndNil(fBlockSelection);
  FreeAndNil(FStrings);
  FreeAndNil(FTabbedLinesView);
  FreeAndNil(FTrimmedLinesView);
  FreeAndNil(FDoubleWidthChrLinesView);
  FreeAndNil(fLines);
  FreeAndNil(fCaret);
  FreeAndNil(fInternalCaret);
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

{$IFDEF SYN_LAZARUS}
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

function TCustomSynEdit.GetTrimSpaceType: TSynEditStringTrimmingType;
begin
  Result := FTrimmedLinesView.TrimType;
end;

function TCustomSynEdit.GetViewedTextBuffer: TSynEditStrings;
begin
  Result := FTheLinesView;
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
  Result:=PhysicalToLogicalPos(CaretXY);
end;

procedure TCustomSynEdit.SetLogicalCaretXY(const NewLogCaretXY: TPoint);
begin
  CaretXY:=LogicalToPhysicalPos(NewLogCaretXY);
end;

procedure TCustomSynEdit.SetBeautifier(NewBeautifier: TSynCustomBeautifier);
begin
  if fBeautifier = NewBeautifier then exit;
  if NewBeautifier = nil then
    fBeautifier := SynDefaultBeautifier
  else
    fBeautifier := NewBeautifier;
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

procedure TCustomSynEdit.SetTrimSpaceType(const AValue: TSynEditStringTrimmingType);
begin
  FTrimmedLinesView.TrimType := AValue;
end;

function TCustomSynEdit.SynGetText: string;
begin
  Result := fLines.Text;
end;

{$IFDEF SYN_LAZARUS}
function TCustomSynEdit.RealGetText: TCaption;
begin
  if FLines<>nil then
    Result := FLines.Text
  else
    Result := '';
end;
{$ENDIF}

procedure TCustomSynEdit.HideCaret;
begin
  //DebugLn('[TCustomSynEdit.HideCaret] ',Name,' ',sfCaretVisible in fStateFlags,' ',eoPersistentCaret in Options);
  if sfCaretVisible in fStateFlags then begin
    // Todo: If Show/HideCaret fails while we have the Focus => somone else may have stolen the caret(Windows)
    if LCLIntf.HideCaret(Handle) then
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
  if fPaintLock = 0 then begin
    FOldTopView := TopView;
    FTopDelta := 0;
  end;
  inc(fPaintLock);
  FFoldedLinesView.Lock; //DecPaintLock triggers ScanFrom, and folds must wait
  FTrimmedLinesView.Lock; // Lock before caret
  FCaret.Lock;
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
        LastLine := LastLine + FTopDelta;
      end
      else
        LastLine := LinesInWindow + 1;
      FirstLine := RowToScreenRow(Max(FirstLine, TopLine));
      FirstLine := Max(0, FirstLine + FTopDelta);
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
        l := l + FTopDelta;
      end
      else
        l := LinesInWindow + 1;
      f := RowToScreenRow(Max(FirstLine, TopLine));
      f := Max(0, f + FTopDelta);
      { any line visible? }
      if (l >= f) then begin
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
  FInMouseClickEvent := False;
  {$IFDEF VerboseKeys}
  DebugLn('[TCustomSynEdit.KeyDown] ',dbgs(Key),' ',dbgs(Shift));
  {$ENDIF}
  inherited;
  {$IFDEF SYN_LAZARUS}
  if assigned(fMarkupCtrlMouse) then
    fMarkupCtrlMouse.UpdateCtrlState;
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
  DebugLn(['[TCustomSynEdit.KeyUp] ',Key
    ,' Shift=',ssShift in Shift,' Ctrl=',ssCtrl in Shift,' Alt=',ssAlt in Shift]);
  {$ENDIF}
  inherited KeyUp(Key, Shift);
  if assigned(fMarkupCtrlMouse) then
    fMarkupCtrlMouse.UpdateCtrlState;
end;

{$ENDIF}

procedure TCustomSynEdit.Loaded;
begin
  inherited Loaded;
  GutterChanged(Self);
end;

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
    {$IFDEF VerboseKeyboard}
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

procedure TCustomSynEdit.LinesChanging(Sender: TObject);
begin
  Include(fStateFlags, sfLinesChanging);
end;

procedure TCustomSynEdit.LinesChanged(Sender: TObject);
begin
  Exclude(fStateFlags, sfLinesChanging);
  if HandleAllocated then begin
    UpdateScrollBars;
    if not FTrimmedLinesView.IsTrimming then
      SetBlockBegin(PhysicalToLogicalPos(CaretXY))
    else
      if not(eoScrollPastEol in Options) then
        FBlockSelection.AdjustAfterTrimming;
    {$IFDEF VerboseSynEditInvalidate}
    DebugLn(['TCustomSynEdit.LinesChanged ',dbgs(fInvalidateRect)]);
    {$ENDIF}
    InvalidateRect(Handle, @fInvalidateRect, False);
    FillChar(fInvalidateRect, SizeOf(TRect), 0);
    FGutter.AutoSizeDigitCount(FTheLinesView.Count); // Todo: Make the LineNumberGutterPart an observer
    TopLine := TopLine;
  end;
end;

function TCustomSynEdit.DoHandleMouseAction(AnActionList: TSynEditMouseActions;
  AnInfo: TSynEditMouseActionInfo): Boolean;
var
  CaretDone: Boolean;

  procedure MoveCaret;
  begin
   CaretXY := AnInfo.NewCaret.LineCharPos;
   CaretDone := True;
  end;

var
  ACommand: TSynEditorMouseCommand;
  Handled: Boolean;
  AnAction: TSynEditMouseAction;
  ClipHelper: TSynClipboardStream;
begin
  AnAction := nil;
  Result := False;
  while not Result do begin
    AnAction := AnActionList.FindCommand(AnInfo, AnAction);

    if AnAction = nil then exit(False);
    ACommand := AnAction.Command;
    AnInfo.CaretDone := False;

    Result := FGutter.DoHandleMouseAction(AnAction, AnInfo);
    if Result then begin
      if (not AnInfo.CaretDone) and AnAction.MoveCaret then
        MoveCaret;
      exit;
    end;

    Result := True;
    CaretDone := False;
    MouseCapture := False;

    case ACommand of
      emcNone: ; // do nothing, but result := true
      emcStartSelections, emcStartColumnSelections, emcStartLineSelections:
        begin
          if AnAction.Option = emcoSelectionContinue then
            FBlockSelection.EndLineBytePos := AnInfo.NewCaret.LineBytePos
          else begin
            MoveCaret;
            FBlockSelection.StartLineBytePos := AnInfo.NewCaret.LineBytePos;
          end;
          case ACommand of
            emcStartColumnSelections:
              FBlockSelection.ActiveSelectionMode := smColumn;
            emcStartLineSelections:
              begin
                if ACommand = emcStartLineSelections then
                  SetLineBlock(AnInfo.NewCaret.LineBytePos, True);
                FBlockSelection.ActiveSelectionMode := smLine;
              end;
            else
              FBlockSelection.ActiveSelectionMode := FBlockSelection.SelectionMode;
          end;
          MouseCapture := True;
          Include(fStateFlags, sfMouseSelecting);
        end;
      emcSelectWord:
        begin
          if not (eoNoSelection in fOptions) then
            SetWordBlock(AnInfo.NewCaret.LineBytePos);
          MouseCapture := FALSE;
        end;
      emcSelectLine:
        begin
          if not (eoNoSelection in fOptions) then
            SetLineBlock(AnInfo.NewCaret.LineBytePos, AnAction.Option = emcoSelectLineFull);
          MouseCapture := FALSE;
        end;
      emcSelectPara:
        begin
          if not (eoNoSelection in fOptions) then
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
              FBlockSelection.StartLineBytePos := FCaret.LineBytePos;
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
            FOnClickLink(Self, AnInfo.Button, AnInfo.Shift, AnInfo.MouseX, AnInfo.MouseY)
          else
            Result := False;
        end;
      emcContextMenu:
        begin
          Handled := False;
          inherited DoContextPopup(Point(AnInfo.MouseX, AnInfo.MouseY), Handled);
          if (PopupMenu <> nil) and not Handled then
            PopupMenu.PopUp;
        end;
      emcSynEditCommand:
        begin
          if AnAction.MoveCaret then
            MoveCaret;
          CommandProcessor(AnAction.Option, #0, nil);
        end;
      else
        Result := False; // ACommand was not handled => Fallback to parent Context
    end;

    if Result and (not CaretDone) and AnAction.MoveCaret then
      MoveCaret;
  end;
end;

procedure TCustomSynEdit.FindAndHandleMouseAction(AButton: TMouseButton;
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
  end;
  // mouse event occured in Gutter ?
  if  (X <= fGutterWidth) then begin
    FGutter.MaybeHandleMouseAction(Info, {$IFDEF FPC}@{$ENDIF}DoHandleMouseAction);
    exit;
    // No fallback to text actions
  end;
  // mouse event occured in selected block ?
  if SelAvail and (X >= fGutterWidth + 2) and
     IsPointInSelection(FInternalCaret.LineBytePos)
  then
    if DoHandleMouseAction(FMouseSelActions, Info) then
      exit;
  DoHandleMouseAction(FMouseActions, Info);
end;

procedure TCustomSynEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  CType: TSynMAClickCount;
begin
  //DebugLn('TCustomSynEdit.MouseDown START Mouse=',X,',',Y,' Caret=',CaretX,',',CaretY,', BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
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
                                sfGutterClick, sfMouseSelecting,
                                sfWaitForDragging
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

  IncPaintLock;
  try
    if (X < fGutterWidth) then begin
      Include(fStateFlags, sfGutterClick);
      FGutter.MouseDown(Button, Shift, X, Y);
    end;
    FindAndHandleMouseAction(Button, Shift, X, Y, CType, cdDown);
  finally
    DecPaintLock;
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
  inherited MouseMove(Shift, x, y);
  if (sfGutterClick in fStateFlags) then begin
    FGutter.MouseMove(Shift, X, Y);
  end;

  if (X >= fGutterWidth) and (X < ClientWidth - ScrollBarWidth)
    and (Y >= 0) and (Y < ClientHeight - ScrollBarWidth)
  then begin
    if (Cursor <> crHandPoint) or
       not(assigned(fMarkupCtrlMouse) and fMarkupCtrlMouse.IsMouseOverLink)
    then
      Cursor := crIBeam;
  end
  else
    Cursor := crDefault;

  LastMouseCaret:=PixelsToRowColumn(Point(X,Y));

  //debugln('TCustomSynEdit.MouseMove sfWaitForDragging=',dbgs(sfWaitForDragging in fStateFlags),' MouseCapture=',dbgs(MouseCapture),' GetCaptureControl=',DbgSName(GetCaptureControl));
  if MouseCapture and (sfWaitForDragging in fStateFlags) then begin
    if (Abs(fMouseDownX - X) >= GetSystemMetrics(SM_CXDRAG))
      or (Abs(fMouseDownY - Y) >= GetSystemMetrics(SM_CYDRAG))
    then begin
      Exclude(fStateFlags, sfWaitForDragging);
      Include(fStateFlags, sfIsDragging);
      //debugln('TCustomSynEdit.MouseMove BeginDrag');
      BeginDrag(true);
    end;
  end
  else
  if (fStateFlags * [sfMouseSelecting, sfIsDragging] <> []) and MouseCapture
  then begin
    //DebugLn(' TCustomSynEdit.MouseMove CAPTURE Mouse=',dbgs(X),',',dbgs(Y),' Caret=',dbgs(CaretXY),', BlockBegin=',dbgs(BlockBegin),' BlockEnd=',dbgs(BlockEnd));
    FInternalCaret.AssignFrom(FCaret);
    FInternalCaret.LineCharPos := PixelsToRowColumn(Point(X,Y));
    if (not(sfIsDragging in fStateFlags)) then
      SetBlockEnd(FInternalCaret.LineBytePos);
    if (X >= fGutterWidth) and (X < ClientWidth-ScrollBarWidth)
      and (Y >= 0) and (Y < ClientHeight-ScrollBarWidth)
    then
      FCaret.LineBytePos := FInternalCaret.LineBytePos;
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
  end
  else
  if MouseCapture and (not(sfIsDragging in fStateFlags))
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
  IncPaintLock;
  try
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
    if fScrollDeltaX <> 0 then begin
      LeftChar := LeftChar + fScrollDeltaX;
      X := LeftChar;
      if fScrollDeltaX > 0 then  // scrolling right?
        Inc(X, CharsInWindow);
      CaretXY := Point(X, C.Y);
      if (not(sfIsDragging in fStateFlags)) then
        SetBlockEnd(PhysicalToLogicalPos(CaretXY));
    end;
    if fScrollDeltaY <> 0 then begin
      if GetKeyState(VK_SHIFT) < 0 then
        TopView := TopView + fScrollDeltaY * LinesInWindow
      else
        TopView := TopView + fScrollDeltaY;
      if fScrollDeltaY > 0
      then Y := FFoldedLinesView.TextIndex[LinesInWindow-1]+1  // scrolling down
      else Y := TopLine;  // scrolling up
      CaretXY := Point(C.X, Y);
      if (not(sfIsDragging in fStateFlags)) then
        SetBlockEnd(PhysicalToLogicalPos(CaretXY));
    end;
  finally
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.DoContextPopup(const MousePos: TPoint; var Handled: Boolean);
begin
  Handled := FInMouseClickEvent;
end;

procedure TCustomSynEdit.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  wasDragging : Boolean;
  CType: TSynMAClickCount;
begin
//DebugLn('TCustomSynEdit.MouseUp Mouse=',X,',',Y,' Caret=',CaretX,',',CaretY,', BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
  FInMouseClickEvent := True;
  wasDragging := (sfIsDragging in fStateFlags);
  Exclude(fStateFlags, sfIsDragging);
  Exclude(fStateFlags, sfMouseSelecting);
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
    SetBlockBegin(PhysicalToLogicalPos(CaretXY));
    SetBlockEnd(PhysicalToLogicalPos(CaretXY));
    Exclude(fStateFlags, sfWaitForDragging);
  end;

  if SelAvail then
    AquirePrimarySelection;
  if (X>=ClientWidth-ScrollBarWidth) or (Y>=ClientHeight-ScrollBarWidth) then
    exit;
  LastMouseCaret:=PixelsToRowColumn(Point(X,Y));

  if wasDragging then exit;

  IncPaintLock;
  try
    if (sfGutterClick in fStateFlags) then begin
      FGutter.MouseUp(Button, Shift, X, Y);
      Exclude(fStateFlags, sfGutterClick);
    end;
    FindAndHandleMouseAction(Button, Shift, X, Y, CType, cdUp);
  finally
    DecPaintLock;
  end;
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
             (rcClip.Bottom-1) div fTextHeight, FFoldedLinesView.Count - FFoldedLinesView.TopLine
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
  if (iLine<=0) or (iLine>FTheLinesView.Count) then exit;
  dec(iLine);
//DebugLn(['****** FoldAction at ',iLine,' scrline=',FFoldedLinesView.TextIndexToScreenLine(iLine), ' type ', SynEditCodeFoldTypeNames[FFoldedLinesView.FoldType[FFoldedLinesView.TextIndexToScreenLine(iLine)]],  '  view topline=',FFoldedLinesView.TopLine  ]);
  case FFoldedLinesView.FoldType[FFoldedLinesView.TextIndexToScreenLine(iLine)] of
    cfCollapsed : FFoldedLinesView.UnFoldAtTextIndex(iLine);
    cfExpanded  : FFoldedLinesView.FoldAtTextIndex(iLine);
  end;
end;

function TCustomSynEdit.FindNextUnfoldedLine(iLine: integer; Down: boolean
  ): Integer;
// iLine is 1 based
begin
  Result:=iLine;
  while (Result>0) and (Result<=FTheLinesView.Count)
  and (FFoldedLinesView.FoldedAtTextIndex[Result-1]) do
    if Down then inc(Result) else dec(Result);
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
{$ENDIF}

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
    FG, BG, FC: TColor;
    Style: TFontStyles;
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
    Special, HasTabs: boolean;
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
          // tab char, fill spaces at start
          for i := 0 to Fill do begin
            Dest[DestPos]:= ' ';
            inc(DestPos);
          end;
          if Special then begin
            // #194#187 looks like >>
            Dest[DestPos-1] := #194;
            Dest[DestPos]   := #187;
            inc(DestPos);
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
            if (c = #32) and Special then begin
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
    FillFCol, FillBCol, FillFrame : TColor;
    FillStyle: TFontStyles;
    tok: TRect;
    Attr: TSynHighlighterAttributes;
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
      if Assigned(fHighlighter) then
        Attr := fHighlighter.GetEndOfLineAttribute
      else
        Attr := nil;
      Repeat
        MarkupInfo := fMarkupManager.GetMarkupAttributeAtRowCol(FFoldedLinesView.TextIndex[CurLine]+1, NextPos);
        NextPos := fMarkupManager.GetNextMarkupColAfterRowCol(FFoldedLinesView.TextIndex[CurLine]+1, NextPos);

        if assigned(Attr) then begin
          FillFCol := Attr.Foreground;
          FillBCol := Attr.Background;
          FillFrame := Attr.FrameColor;
          FillStyle := Attr.Style;
          if FillFCol = clNone then FillFCol := Font.Color;
          if FillBCol = clNone then FillBCol := colEditorBG;
        end else begin
          FillFCol := Font.Color;
          FillBCol := colEditorBG;
          FillFrame := clNone;
          FillStyle := Font.Style;
        end;

        if assigned(MarkupInfo) then
          MarkupInfo.ModifyColors(FillFCol, FillBCol, FillFrame, FillStyle);

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

      if FFoldedLinesView.FoldType[CurLine] = cfCollapsed then
      begin
        FillFCol := Font.Color;
        FillBCol := colEditorBG;
        FillFrame := Font.Color;
        FillStyle  := [];
        MarkupInfo := fMarkupManager.GetMarkupAttributeAtRowCol(FFoldedLinesView.TextIndex[CurLine]+1, CurPhysPos + 3);
        if MarkupInfo <> nil then
          MarkupInfo.ModifyColors(FillFCol, FillBCol, FillFrame, FillStyle);
        FoldedCodeInfo := FoldedCodeColor;
        If assigned(FoldedCodeInfo) then
          FoldedCodeInfo.ModifyColors(FillFCol, FillBCol, FillFrame, FillStyle);
        if (FillBCol = FillFCol) then begin // or if diff(gb,fg) < x
          if FillBCol = colEditorBG
          then FillFCol := not(FillBCol) and $00ffffff // or maybe Font.color ?
          else FillFCol := colEditorBG;
        end;

        rcToken.Left := ScreenColumnToXValue(CurPhysPos+3);
        rcToken.Right := ScreenColumnToXValue(CurPhysPos+6);

        FTextDrawer.FrameColor := FillFrame;
        FTextDrawer.ForeColor := FillFCol;
        FTextDrawer.BackColor := FillBCol;
        FTextDrawer.SetStyle(FillStyle);

        If assigned(FoldedCodeInfo) and (FoldedCodeInfo.FrameColor <> clNone) then
        begin
          FTextDrawer.FrameStartX := rcToken.Left;
          FTextDrawer.FrameEndX := rcToken.Right;
        end;
        rcToken.Right := Min(rcToken.Right, rcLine.Right);
        if rcToken.Right > rcToken.Left then begin
          if ForceEto then fTextDrawer.ForceNextTokenWithEto;
          fTextDrawer.ExtTextOut(rcToken.Left, rcToken.Top, ETOOptions-ETO_OPAQUE,
                                 rcToken, '...', 3, rcLine.Bottom);
        end;
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

  var
    LastFSX, LastFEX: integer;
  procedure DrawHiLightMarkupToken(attr: TSynHighlighterAttributes;
    sToken: PChar; nTokenByteLen: integer);

    function CharToByteLen(aCharLen: Integer) : Integer;
    begin
      if not UseUTF8 then exit(aCharLen);
      Result := UTF8CharToByteIndex(sToken, nTokenByteLen, aCharLen);
      if Result < 0 then begin
        debugln('ERROR: Could not convert CharLen (',dbgs(aCharLen),') to byteLen (maybe invalid UTF8?)',' len ',dbgs(nTokenByteLen),' Line ',dbgs(CurLine),' PhysPos ',dbgs(CurPhysPos));
        Result := aCharLen;
      end;
    end;

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
      NextPhysPos := fMarkupManager.GetNextMarkupColAfterRowCol(FFoldedLinesView.TextIndex[CurLine]+1, PhysicalStartPos);
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
      BG := DefaultBGCol;
      FG := DefaultFGCol;
      FC := DefaultFCCol;
      Style := DefaultStyle;
      MarkupInfo := fMarkupManager.GetMarkupAttributeAtRowCol(FFoldedLinesView.TextIndex[CurLine]+1, PhysicalStartPos);
      if assigned(MarkupInfo)
      then MarkupInfo.ModifyColors(FG, BG, FC, Style);
      // Deal with equal colors
      if (BG = FG) then begin // or if diff(gb,fg) < x
        if BG = DefaultBGCol
        then FG := not(BG) and $00ffffff // or maybe Font.color ?
        else FG := DefaultBGCol;
      end;

      if assigned(MarkupInfo) and
         ((MarkupInfo.StartX <> LastFSX) or (MarkupInfo.EndX <> LastFEX)) and
         ((TokenAccu.FC <> clNone) or (FC <> clNone))
      then begin
        // force Paint
        PaintHighlightToken(FALSE);
        TokenAccu.Len := 0;
        // Set Frame Boundaries
        LastFSX := MarkupInfo.StartX;
        LastFEX := MarkupInfo.EndX;
        FTextDrawer.FrameStartX := ScreenColumnToXValue(MarkupInfo.StartX);
        FTextDrawer.FrameEndX := ScreenColumnToXValue(MarkupInfo.EndX+1);
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
      FTextDrawer.FrameStartX := -1;
      FTextDrawer.FrameEndX := -1;
      LastFSX := -1;
      LastFEX := -1;
      CharWidths := FFoldedLinesView.GetPhysicalCharWidths(CurLine);

      fMarkupManager.PrepareMarkupForRow(FFoldedLinesView.TextIndex[CurLine]+1);
      // Get the line.
      sLine := FFoldedLinesView[CurLine];
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
        DrawHiLightMarkupToken(nil, PChar(Pointer(sLine)), Length(sLine));
      end else begin
        // draw splitter line
        DividerInfo := fHighlighter.DrawDivider[CurTextIndex];
        if DividerInfo.Color <> clNone then
        begin
          ypos := rcToken.Bottom - 1;
          cl := DividerInfo.Color;
          if cl = clDefault then
            cl := fRightEdgeColor;
          fTextDrawer.DrawLine(nRightEdge, ypos, fGutterWidth - 1, ypos, cl);
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

      fMarkupManager.FinishMarkupForRow(FFoldedLinesView.TextIndex[CurLine]+1);
    end;
    CurLine:=-1;
  end;

{ end local procedures }

var
  ColBG : TColor;
begin
  if (AClip.Right < fGutterWidth) then exit;
  //DebugLn(['TCustomSynEdit.PaintTextLines ',dbgs(AClip)]);
  CurLine:=-1;
  FillChar(TokenAccu,SizeOf(TokenAccu),0);
  //DebugLn('TCustomSynEdit.PaintTextLines ',DbgSName(Self),' TopLine=',dbgs(TopLine),' AClip=',dbgs(AClip));
  colEditorBG := Color;
  if Assigned(fHighlighter) then begin
    fHighlighter.CurrentLines := FTheLinesView;
    if Assigned(Highlighter.WhitespaceAttribute) then
    begin
      colBG := Highlighter.WhitespaceAttribute.Background;
      if colBG <> clNone then
        colEditorBG := colBG;
    end;
  end;
  // If the right edge is visible and in the invalid area, prepare to paint it.
  // Do this first to realize the pen when getting the dc variable.
  bDoRightEdge := FALSE;
  if (fRightEdge > 0) then begin // column value
    nRightEdge := fTextOffset + fRightEdge * fCharWidth; // pixel value
    if (nRightEdge >= AClip.Left) and (nRightEdge <= AClip.Right) then begin
      bDoRightEdge := TRUE;
    end;
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
  if (AClip.Left < fGutterWidth + 2) then
    AClip.Left := fGutterWidth + 2;
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
  InsStart: TPoint;
begin
  Result := False;
  BeginUndoBlock;
  try
    if ClipHelper.TextP = nil then
      exit;

    Result := True;
    InsStart := FBlockSelection.StartLineBytePos;
    SetSelTextPrimitive(ClipHelper.SelectionMode, ClipHelper.TextP, true);

    if eoFoldedCopyPaste in fOptions2 then begin
      PTxt := ClipHelper.GetTagPointer(synClipTagFold);
      if PTxt <> nil then begin
        ScanFromAfterLock;
        FFoldedLinesView.ApplyFoldDescription(InsStart.Y -1, InsStart.X,
            FBlockSelection.StartLinePos-1, FBlockSelection.StartBytePos,
            PTxt, ClipHelper.GetTagLen(synClipTagFold));
      end;
    end;
  finally
    EndUndoBlock;
    EnsureCursorPosVisible;
  end;
end;

procedure TCustomSynEdit.SelectAll;
var
  LastPt: TPoint;
begin
  LastPt := Point(1, FTheLinesView.Count);
  if LastPt.y > 0 then
    Inc(LastPt.x, Length(FTheLinesView[LastPt.y - 1]))
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

procedure TCustomSynEdit.SelectLine(WithLeadSpaces: Boolean = True);
begin
  SetLineBlock(CaretXY, WithLeadSpaces);
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

function TCustomSynEdit.GetCaretUndo: TSynEditUndoItem;
begin
  if SelAvail then
    Result := TSynEditUndoSelCaret.Create(FCaret.LineCharPos,
       FBlockSelection.StartLineBytePos, FBlockSelection.EndLineBytePos,
       FBlockSelection.ActiveSelectionMode)
  else
    Result := TSynEditUndoCaret.Create(FCaret.LineCharPos);
end;

{$ENDIF}

function TCustomSynEdit.GetMarkup(Index: integer): TSynEditMarkup;
begin
  Result := fMarkupManager.Markup[Index];
end;

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
  fCaret.LineCharPos:= Value;
  fLastCaretX:=CaretX;
  if (CompareCarets(FCaret.LineBytePos, FBlockSelection.StartLineBytePos) <> 0)
     and not(SelAvail or FBlockSelection.SelCanContinue(FCaret)) then
    FBlockSelection.StartLineBytePos := FCaret.LineBytePos;
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
  FMarkupManager.Caret := FCaret.LineCharPos;
  if fPaintLock = 0 then
    fMarkupHighCaret.CheckState; // Todo need a global lock, including the markup
end;

procedure TCustomSynEdit.SetLeftChar(Value: Integer);
{begin}                                                                         //mh 2000-10-19
var
  MaxVal: integer;
begin
  if eoScrollPastEol in Options then
    MaxVal := fMaxLeftChar
  else
    MaxVal := FTheLinesView.LengthOfLongestLine;
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
    FStrings.Assign(Value);
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
  inherited CreateHandle;
  ShowScrollBar(Handle, SB_HORZ, fScrollBars in [ssBoth, ssHorizontal] );
  ShowScrollBar(Handle, SB_Vert, fScrollBars in [ssBoth, ssVertical] );
end;

procedure TCustomSynEdit.SetScrollBars(const Value: TScrollStyle);
begin
  if (FScrollBars <> Value) then begin
    FScrollBars := Value;
    if HandleAllocated then begin
      ShowScrollBar(Handle, SB_HORZ, fScrollBars in [ssBoth, ssHorizontal] );
      ShowScrollBar(Handle, SB_Vert, fScrollBars in [ssBoth, ssVertical] );
    end;
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
  Value: PChar; AddToUndoList: Boolean = false);
Begin
  IncPaintLock;
  if not AddToUndoList then begin
    fUndoList.Lock;
    fRedoList.Lock;
  end;
  try
    FBlockSelection.SetSelTextPrimitive(PasteMode, Value);
    // Force caret reset
    CaretXY := CaretXY;
    fLastCaretX := CaretX;
    EnsureCursorPosVisible;
    fMarkupManager.Caret := CaretXY;
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
  BeginUndoBlock;
  try
    FBlockSelection.SelText := Value;
    // Force caret reset
    CaretXY := CaretXY;
    fLastCaretX := CaretX;
    EnsureCursorPosVisible;
  finally
    EndUndoBlock;
  end;
end;

procedure TCustomSynEdit.SynSetText(const Value: string);
begin
  FLines.Text := Value;
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.RealSetText(const Value: TCaption);
begin
  FLines.Text := Value; // Do not trim
end;

procedure TCustomSynEdit.IncreaseChangeStamp;
begin
  if fChangeStamp=High(fChangeStamp) then
    fChangeStamp:=Low(fChangeStamp)
  else
    inc(fChangeStamp);
end;
{$ENDIF}

procedure TCustomSynEdit.SetTopLine(Value: Integer);
begin
  // don't use MinMax here, it will fail in design mode (Lines.Count is zero,
  // but the painting code relies on TopLine >= 1)
  if (eoScrollPastEof in Options) then
    Value := Min(Value, FTheLinesView.Count)
  else
    Value := Min(Value, FFoldedLinesView.TextPosAddLines(FTheLinesView.Count+1, -fLinesInWindow));
  Value := Max(Value, 1);
  if FFoldedLinesView.FoldedAtTextIndex[Value-1] then
    Value := FindNextUnfoldedLine(Value, False);
  FFoldedLinesView.TopTextIndex := fTopLine - 1;
  if Value <> fTopLine then begin
    if fPaintLock = 0 then
      FOldTopView := TopView;
    fTopLine := Value;
    FFoldedLinesView.TopTextIndex := Value-1;
    FTopDelta := TopView - FOldTopView;
    UpdateScrollBars;
    ScrollAfterTopLineChanged;
    StatusChanged([scTopLine]);
  end;
  fMarkupManager.TopLine:= fTopLine;
end;

procedure TCustomSynEdit.ScrollAfterTopLineChanged;
var
  Delta: Integer;
begin
  if (sfPainting in fStateFlags) or (fPaintLock <> 0) then exit;
  Delta := FOldTopView - TopView;
  if Delta = 0 then exit;
  // TODO: SW_SMOOTHSCROLL --> can't get it work
  if (Abs(Delta) >= fLinesInWindow) or
  not ScrollWindowEx(Handle, 0, fTextHeight * Delta, nil, nil, 0, nil, SW_INVALIDATE)
  then
    Invalidate    // scrollwindow failed, invalidate all
  else
    if eoAlwaysVisibleCaret in fOptions2 then
      MoveCaretToVisibleArea;      // Invalidate caret line, if necessary
  FTopDelta := 0;
end;

procedure TCustomSynEdit.ShowCaret;
begin
  //DebugLn(' [TCustomSynEdit.ShowCaret] ShowCaret ',Name,' ',sfCaretVisible in fStateFlags,' ',eoPersistentCaret in fOptions);
  if not (eoNoCaret in Options) and not (sfCaretVisible in fStateFlags) then
  begin
    SetCaretRespondToFocus(Handle,not (eoPersistentCaret in fOptions));
    // Todo: If Show/HideCaret fails while we have the Focus => somone else may have stolen the caret(Windows)
    if LCLIntf.ShowCaret(Handle) then
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
  or ((not Focused) and (not (eoPersistentCaret in fOptions))) then begin
    Include(fStateFlags, sfCaretChanged);
  end else begin
    Exclude(fStateFlags, sfCaretChanged);
    if eoAlwaysVisibleCaret in fOptions2 then
      MoveCaretToVisibleArea;
    CX := CaretXPix;
    CY := CaretYPix;
    if (CX >= fGutterWidth)
      and (CX < ClientWidth-ScrollBarWidth)
      and (CY >= 0)
      and (CY < ClientHeight-ScrollBarWidth)
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
        if eoScrollPastEol in Options then
          ScrollInfo.nMax := fMaxLeftChar
        else
          ScrollInfo.nMax := FTheLinesView.LengthOfLongestLine;
        ScrollInfo.nPage := CharsInWindow;
        ScrollInfo.nPos := LeftChar;
        SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
        //DebugLn('>>>>>>>>>> [TCustomSynEdit.UpdateScrollbars] nMin=',ScrollInfo.nMin,
        //' nMax=',ScrollInfo.nMax,' nPage=',ScrollInfo.nPage,
        //' nPos=',ScrollInfo.nPos,
        //' ClientW=',ClientWidth
        //);
      end;
      if fScrollBars in [ssBoth, ssVertical] then begin
        nMaxScroll := {$IFDEF SYN_LAZARUS}FFoldedLinesView.Count+1{$ELSE}Lines.Count{$ENDIF};
        if (eoScrollPastEof in Options) then
          Inc(nMaxScroll, LinesInWindow - 1);
{$IFNDEF SYN_LAZARUS}
        if nMaxScroll <= MAX_SCROLL then begin
{$ENDIF}
          ScrollInfo.nMax := Max(1, nMaxScroll);
          ScrollInfo.nPage := LinesInWindow;
          {$IFDEF SYN_LAZARUS}
          ScrollInfo.nPos := FFoldedLinesView.TextIndexToViewPos(TopLine-1);
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
  LastMouseCaret:=Point(-1,-1);
  // Todo: Under Windows, keeping the Caret only works, if no other component creates a caret
  if not (eoPersistentCaret in fOptions) then begin
    HideCaret;
    LCLIntf.DestroyCaret(Handle);
  end;
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
    SB_BOTTOM: TopLine := FTheLinesView.Count;
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
end;

function TCustomSynEdit.ScanFrom(var Index: integer; AtLeastTilIndex: integer): integer;
// Index and AtLeastTilIndex are 0 based
begin
  if Index < 0 then Index := 0;
  if not assigned(fHighlighter) or (Index > FTheLinesView.Count - 1) then begin
    FFoldedLinesView.FixFoldingAtTextIndex(Index);
    fMarkupManager.TextChangedScreen(Max(RowToScreenRow(Index+1), 0), LinesInWindow+1);
    Topline := TopLine;
    exit;
  end;
  fHighlighter.CurrentLines := FTheLinesView;
  Result :=  fHighlighter.ScanFrom(Index, AtLeastTilIndex);

  FFoldedLinesView.FixFoldingAtTextIndex(Index, Result);
  fMarkupManager.TextChangedScreen(Max(RowToScreenRow(Index+1), 0),
                                       Min(RowToScreenRow(Result), LinesInWindow+1));
  Topline := TopLine;
  if Index > 0 then dec(Index);
  Dec(Result);
end;

procedure TCustomSynEdit.LineCountChanged(Sender: TSynEditStrings;
  AIndex, ACount: Integer);
begin
  if PaintLock>0 then begin
    if (fHighlighterNeedsUpdateStartLine<1)
    or (fHighlighterNeedsUpdateStartLine>AIndex+1) then
      fHighlighterNeedsUpdateStartLine:=AIndex+1;
    if (fHighlighterNeedsUpdateEndLine<1)
    or (fHighlighterNeedsUpdateEndLine<AIndex+1) then
      fHighlighterNeedsUpdateEndLine:=AIndex + 1 + MaX(ACount, 0)
    else
      fHighlighterNeedsUpdateEndLine := fHighlighterNeedsUpdateEndLine
                                        + MaX(ACount, 0)
  end
  else
    ScanFrom(AIndex, Max(AIndex, AIndex + ACount));
  if ACount > 0 then
    DoLinesInserted(AIndex + 1, ACount)
  else
    DoLinesDeleted(AIndex, -ACount);
  InvalidateLines(AIndex + 1, -1);
  InvalidateGutterLines(AIndex + 1, -1);
end;

procedure TCustomSynEdit.LineTextChanged(Sender: TSynEditStrings;
  AIndex, ACount: Integer);
var
  EndIndex: Integer;
begin
  if PaintLock>0 then begin
    if (fHighlighterNeedsUpdateStartLine<1)
    or (fHighlighterNeedsUpdateStartLine>AIndex+1) then
      fHighlighterNeedsUpdateStartLine:=AIndex+1;
    if (fHighlighterNeedsUpdateEndLine<1)
    or (fHighlighterNeedsUpdateEndLine<AIndex+1) then
      fHighlighterNeedsUpdateEndLine:=AIndex + 1 + MaX(ACount, 0);
    exit;
  end;
  EndIndex := ScanFrom(AIndex, Max(AIndex, AIndex + ACount)) + 1;
  InvalidateLines(AIndex + 1, EndIndex);
  InvalidateGutterLines(AIndex + 1, EndIndex);
end;

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

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.FoldChanged(Index : integer);
var
  i: Integer;
begin
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
  if Index + 1 > ScreenRowToRow(LinesInWindow + 1) then exit;
  if Index + 1 < TopLine then Index := TopLine;
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
{$ENDIF}

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
  Value.y := MinMax(Value.y, 1, FTheLinesView.Count);
  TempString := FTheLinesView[Value.Y - 1];
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
  CaretXY := FTheLinesView.LogicalToPhysicalPos(Runner);
end;

{$ENDIF}

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.SetLineBlock(Value: TPoint; WithLeadSpaces: Boolean = True);
var
  ALine: string;
  x, x2: Integer;
begin
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
  CaretXY := FTheLinesView.LogicalToPhysicalPos(FBlockSelection.EndLineBytePos);
  //DebugLn(' FFF2 ',Value.X,',',Value.Y,' BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
end;

procedure TCustomSynEdit.SetParagraphBlock(Value: TPoint);
var ParagraphStartLine, ParagraphEndLine: integer;
begin
  ParagraphStartLine:=MinMax(Value.y, 1, FTheLinesView.Count);
  ParagraphEndLine:=MinMax(Value.y+1, 1, FTheLinesView.Count);
  while (ParagraphStartLine>1)
  and (Trim(FTheLinesView[ParagraphStartLine-1])<>'') do
    dec(ParagraphStartLine);
  while (ParagraphEndLine<FTheLinesView.Count)
  and (Trim(FTheLinesView[ParagraphEndLine-1])<>'') do
    inc(ParagraphEndLine);
  FBlockSelection.StartLineBytePos := Point(1,ParagraphStartLine);
  FBlockSelection.EndLineBytePos := Point(1,ParagraphEndLine);
  FBlockSelection.ActiveSelectionMode := smNormal;
  CaretXY:=FBlockSelection.EndLineBytePos;
  //DebugLn(' FFF3 ',Value.X,',',Value.Y,' BlockBegin=',BlockBegin.X,',',BlockBegin.Y,' BlockEnd=',BlockEnd.X,',',BlockEnd.Y);
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
    or Clipboard.HasFormat(TSynClipboardStream.ClipboardFormatId)
end;

procedure TCustomSynEdit.InsertBlock(BB: TPoint; ChangeStr: PChar);
// used by BlockIndent and Redo
begin
  SetCaretAndSelection(LogicalToPhysicalPos(BB), BB, BB);
  SetSelTextPrimitive(smColumn, ChangeStr);
end;

procedure TCustomSynEdit.Redo;
var
  Item: TSynEditUndoItem;
  Group: TSynEditUndoGroup;
begin
  Group := fRedoList.PopItem;
  if Group <> nil then begin;
    IncPaintLock;
    Item := Group.Pop;
    if Item <> nil then begin
      BeginUndoBlock;
      fUndoList.CurrentGroup.Reason := Group.Reason;
      fUndoList.IsInsideRedo := True;
      try
        repeat
          RedoItem(Item);
          Item := Group.Pop;
        until (Item = nil);
      finally
        EndUndoBlock;
      end;
    end;
    Group.Free;
    if fRedoList.IsTopMarkedAsUnmodified then
      fUndoList.MarkTopAsUnmodified;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.RedoItem(Item: TSynEditUndoItem);
var
  Run, StrToDelete: PChar;
  Len, x : integer;
  TempString: string;
begin
  if Assigned(Item) then
  try
    FCaret.IncForcePastEOL;
    if Item.ClassType = TSynEditUndoIndent then
    begin // re-insert the column
      SetCaretAndSelection(LogicalToPhysicalPos(Point(1,TSynEditUndoIndent(Item).FPosY1)),
        Point(1, TSynEditUndoIndent(Item).FPosY1), Point(2, TSynEditUndoIndent(Item).FPosY2),
        smNormal);
      x := fBlockIndent;
      fBlockIndent := TSynEditUndoIndent(Item).FCnt;
      DoBlockIndent;
      fBlockIndent := x;
    end
    else
    if Item.ClassType = TSynEditUndoUnIndent then
    begin // re-delete the (raggered) column
      // add to undo list
      fUndoList.AddChange(TSynEditUndoUnIndent.Create(TSynEditUndoUnIndent(Item).FPosY1,
         TSynEditUndoUnIndent(Item).FPosY2, TSynEditUndoUnIndent(Item).FText));
      // Delete string
      StrToDelete := PChar(TSynEditUndoUnIndent(Item).FText);
      CaretY := TSynEditUndoUnIndent(Item).FPosY1;
      x := -1;
      repeat
        Run := GetEOL(StrToDelete);
        Len := Run - StrToDelete;
        if x < 0 then
          x:= Len;
        if Len > 0 then begin
          TempString := FTheLinesView[CaretY - 1];
          Delete(TempString, 1, Len);
          FTheLinesView[CaretY - 1] := TempString;
        end;
        if Run^ in [#10,#13] then begin
          if (Run[1] in [#10,#13]) and (Run^<>Run[1]) then
            Inc(Run,2)
          else
            Inc(Run);
          CaretY := CaretY + 1;
        end;
        StrToDelete := Run;
      until Run^ = #0;
    end
    else
      if not Item.PerformUndo(self) then
        FTheLinesView.EditRedo(Item);
  finally
    FCaret.DecForcePastEOL;
    Item.Free;
  end;
end;

procedure TCustomSynEdit.Undo;
var
  Item: TSynEditUndoItem;
  Group: TSynEditUndoGroup;
begin
  Group := fUndoList.PopItem;
  if Group <> nil then begin;
    IncPaintLock;
    FIsUndoing := True;
    Item := Group.Pop;
    if Item <> nil then begin
      BeginUndoBlock(fRedoList);
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
        EndUndoBlock(fRedoList);
      end;
    end;
    FIsUndoing := False;
    Group.Free;
    if fUndoList.IsTopMarkedAsUnmodified then
      fRedoList.MarkTopAsUnmodified;
    DecPaintLock;
  end;
end;

procedure TCustomSynEdit.UndoItem(Item: TSynEditUndoItem);
begin
  if Assigned(Item) then try
    FCaret.IncForcePastEOL;
    if Item.ClassType = TSynEditUndoIndent then
    begin
      // remove the column that was inserted // select the inserted column
      BlockBegin := Point(1, TSynEditUndoIndent(Item).FPosY1);
      BlockEnd := Point(1 + TSynEditUndoIndent(Item).FCnt, TSynEditUndoIndent(Item).FPosY2);
      FBlockSelection.ActiveSelectionMode := smColumn;
      // add to redo list
      fRedoList.AddChange(TSynEditUndoIndent.Create(TSynEditUndoIndent(Item).FPosY1,
         TSynEditUndoIndent(Item).FPosY2, TSynEditUndoIndent(Item).FCnt));
      // remove the column
      SetSelTextPrimitive(smNormal, nil);
    end
    else
    if Item.ClassType = TSynEditUndoUnIndent then
    begin
      fRedoList.AddChange(TSynEditUndoUnIndent.Create(TSynEditUndoUnIndent(Item).FPosY1,
          TSynEditUndoUnIndent(Item).FPosY2, TSynEditUndoUnIndent(Item).FText));
      // reinsert the string
      InsertBlock(Point(1, TSynEditUndoUnIndent(Item).FPosY1),
                  PChar(TSynEditUndoUnIndent(Item).FText));
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

function TCustomSynEdit.GetUndoList: TSynEditUndoList;
begin
  if FIsUndoing then
    Result := fRedoList
  else
    Result := fUndoList;
end;

procedure TCustomSynEdit.SetDividerDrawLevel(const AValue: Integer);
begin
  if assigned(fHighlighter) then
    fHighlighter.DrawDividerLevel := AValue;
  Invalidate;
end;

procedure TCustomSynEdit.SetMouseActions(const AValue: TSynEditMouseActions);
begin
  if AValue = nil then
    FMouseActions.Clear
  else
    FMouseActions.Assign(AValue);
end;

procedure TCustomSynEdit.SetMouseSelActions(const AValue: TSynEditMouseActions);
begin
  if AValue = nil then
    FMouseSelActions.Clear
  else
    FMouseSelActions.Assign(AValue);
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

function TCustomSynEdit.HasDebugMark(ALine: Integer): Boolean;
begin
  Result := sfDebugMark in TSynEditStringList(fLines).Flags[ALine];
end;

procedure TCustomSynEdit.SetDebugMarks(AFirst, ALast: Integer);
begin
  TSynEditStringList(fLines).SetDebugMarks(AFirst, ALast);
  InvalidateGutterLines(AFirst, ALast);
end;

procedure TCustomSynEdit.ClearDebugMarks;
begin
  TSynEditStringList(fLines).ClearDebugMarks;
  InvalidateGutter;
end;

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
        BeginUndoBlock;                                                         //mh 2000-11-20
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
              ScanFromAfterLock;
              FFoldedLinesView.ApplyFoldDescription(NewCaret.Y -1, NewCaret.X,
                    FBlockSelection.StartLinePos-1, FBlockSelection.StartBytePos,
                    PChar(FoldInfo), length(FoldInfo));
            end;
          finally
            FCaret.DecForcePastEOL;
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
      if assigned(FLines.Ranges) then
        FLines.Ranges.Free;
      FLines.Ranges := nil;
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
end;

procedure TCustomSynEdit.SetHighlighter(const Value: TSynCustomHighlighter);
var
  tmp: Integer;
begin
  if Value <> fHighlighter then begin
    RemoveHooksFromHighlighter;
    if Assigned(Value) then begin
      Value.HookAttrChangeEvent(
        {$IFDEF FPC}@{$ENDIF}HighlighterAttrChanged);
      Value.FreeNotification(Self);
      Value.AttachToLines(FLines);
    end;
    fHighlighter := Value;
    // Ensure to free all copies in SynEit.Notification too
    fMarkupHighCaret.Highlighter := Value;
    fMarkupWordGroup.Highlighter := Value;
    FFoldedLinesView.Highlighter := Value;
    {$IFDEF SYN_LAZARUS}
    if fHighlighter<>nil then begin
      fTSearch.IdentChars:=fHighlighter.IdentChars;
    end else begin
      fTSearch.ResetIdentChars;
    end;
    {$ENDIF}
    RecalcCharExtent;
    FTheLinesView.BeginUpdate;
    try
      tmp := 0;
      ScanFrom(tmp,FTheLinesView.Count-1);
    finally
      FTheLinesView.EndUpdate;
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

  Exclude(fStateFlags, sfCaretVisible);
  // Windows allows only one Caret per "Queue" (App/Window ?)
  // an unfocused SynEdit must not steal the caret
  if not Focused then exit;

  if InsertMode
  then ct := FInsertCaret
  else ct := FOverwriteCaret;

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
      TopLine := FFoldedLinesView.TextPosAddLines(CaretY, -Max(0, LinesInWindow-1))
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
  if assigned(fMarkupCtrlMouse) then
    fMarkupCtrlMouse.LastMouseCaret := AValue;
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
  {$ENDIF}
  // first the program event handler gets a chance to process the command
  DoOnProcessCommand(Command, AChar, Data);
  if Command <> ecNone then begin
    try
      BeginUndoBlock;
      // notify hooked command handlers before the command is executed inside of
      // the class
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
    finally
      EndUndoBlock;
    end;
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
  Helper: string;
  bCaretAdjust: Boolean;
  moveBkm: boolean;
  WP: TPoint;
  Caret: TPoint;
  CaretNew: TPoint;
  OldSelMode: TSynSelectionMode;
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
  bCaretAdjust := FCaret.AdjustToNextChar;
  try
    fUndoList.CurrentReason := Command;

    if Command in [ecSelColCmdRangeStart..ecSelColCmdRangeEnd] then
      FBlockSelection.ActiveSelectionMode := smColumn;
    if Command in [ecSelCmdRangeStart..ecSelCmdRangeEnd] then
      FBlockSelection.ActiveSelectionMode := FBlockSelection.SelectionMode;

    case Command of
// horizontal caret movement or selection
      ecLeft, ecSelLeft, ecColSelLeft:
        begin
          {$IFDEF SYN_LAZARUS}
          if (eoCaretSkipsSelection in Options2) and (Command=ecLeft)
          and SelAvail and (CompareCarets(LogicalCaretXY,BlockEnd)=0) then begin
            CaretXY:=LogicalToPhysicalPos(BlockBegin);
          end else
          {$ENDIF}
            MoveCaretHorz(-1, Command in [ecSelLeft, ecColSelLeft]);
        end;
      ecRight, ecSelRight, ecColSelRight:
        begin
          {$IFDEF SYN_LAZARUS}
          if (eoCaretSkipsSelection in Options2) and (Command=ecRight)
          and SelAvail and (CompareCarets(LogicalCaretXY,BlockBegin)=0) then begin
            CaretXY:=LogicalToPhysicalPos(BlockEnd);
          end else
          {$ENDIF}
            MoveCaretHorz(1, Command in [ecSelRight, ecColSelRight]);
        end;
      ecPageLeft, ecSelPageLeft, ecColSelPageLeft:
        begin
          MoveCaretHorz(-CharsInWindow, Command in [ecSelPageLeft, ecColSelPageLeft]);
        end;
      ecPageRight, ecSelPageRight, ecColSelPageRight:
        begin
          MoveCaretHorz(CharsInWindow, Command in [ecSelPageRight, ecColSelPageRight]);
        end;
      ecLineStart, ecSelLineStart, ecColSelLineStart:
        DoHomeKey(Command in [ecSelLineStart, ecColSelLineStart]);
        {begin
          MoveCaretAndSelectionPhysical(CaretXY,Point(1, CaretY),
                                        Command = ecSelLineStart);
          fLastCaretX := CaretX;
        end;}
      ecLineEnd, ecSelLineEnd, ecColSelLineEnd:
        DoEndKey(Command in [ecSelLineEnd, ecColSelLineEnd]);
// vertical caret movement or selection
      ecUp, ecSelUp, ecColSelUp:
        begin
          MoveCaretVert(-1, Command in [ecSelUp, ecColSelUp]);
        end;
      ecDown, ecSelDown, ecColSelDown:
        begin
          MoveCaretVert(1, Command in [ecSelDown, ecColSelDown]);
        end;
      ecPageUp, ecSelPageUp, ecPageDown, ecSelPageDown, ecColSelPageUp, ecColSelPageDown:
        begin
          {$IFDEF SYN_LAZARUS}
          counter := fLinesInWindow;
          if (eoHalfPageScroll in fOptions) then counter:=counter shr 1;
          {$ELSE}
          counter := fLinesInWindow shr Ord(eoHalfPageScroll in fOptions);
          {$ENDIF}
          if eoScrollByOneLess in fOptions then
            Dec(counter);
          if (Command in [ecPageUp, ecSelPageUp, ecColSelPageUp]) then
            counter := -counter;
          {$IFDEF SYN_LAZARUS}
          TopView := TopView + counter;
          {$ELSE}
          TopLine := TopLine + counter;
          {$ENDIF}
          MoveCaretVert(counter, Command in [ecSelPageUp, ecSelPageDown, ecColSelPageUp, ecColSelPageDown]);
          Update;
        end;
      ecPageTop, ecSelPageTop, ecColSelPageTop:
        begin
          {$IFDEF SYN_LAZARUS}
          MoveCaretAndSelectionPhysical
          {$ELSE}
          MoveCaretAndSelection
          {$ENDIF}
            (CaretXY, Point(CaretX, TopLine), Command in [ecSelPageTop, ecColSelPageTop]);
          Update;
        end;
      ecPageBottom, ecSelPageBottom, ecColSelPageBottom:
        begin
          CaretNew := Point(CaretX, ScreenRowToRow(LinesInWindow - 1));
          {$IFDEF SYN_LAZARUS}
          MoveCaretAndSelectionPhysical
          {$ELSE}
          MoveCaretAndSelection
          {$ENDIF}
            (CaretXY, CaretNew, Command in [ecSelPageBottom, ecColSelPageBottom]);
          Update;
        end;
      ecEditorTop, ecSelEditorTop:
        begin
          {$IFDEF SYN_LAZARUS}
          MoveCaretAndSelectionPhysical
          {$ELSE}
          MoveCaretAndSelection
          {$ENDIF}
            (CaretXY, Point(1, 1), Command in [ecSelEditorTop]);
          Update;
        end;
      ecEditorBottom, ecSelEditorBottom:
        begin
          {$IFDEF SYN_LAZARUS}
          CaretNew := Point(1, FFoldedLinesView.ViewPosToTextIndex(FFoldedLinesView.Count)+1);
          {$ELSE}
          CaretNew := Point(1, Lines.Count);
          {$ENDIF}
          if (CaretNew.Y > 0) then
            CaretNew.X := Length(FTheLinesView[CaretNew.Y - 1]) + 1;
          MoveCaretAndSelection(
            {$IFDEF SYN_LAZARUS}
            PhysicalToLogicalPos(CaretXY),
            {$ELSE}
            CaretXY,
            {$ENDIF}
            CaretNew, Command in [ecSelEditorBottom]);
          Update;
        end;
      ecColSelEditorTop:
        begin
          MoveCaretAndSelectionPhysical(CaretXY, Point(CaretX, 1), true);
          Update;
        end;
      ecColSelEditorBottom:
        begin
          CaretNew := Point(CaretX, FFoldedLinesView.ViewPosToTextIndex(FFoldedLinesView.Count)+1);
          MoveCaretAndSelectionPhysical(CaretXY, CaretNew, true);
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
      ecWordLeft, ecSelWordLeft, ecColSelWordLeft:
        begin
          Caret := CaretXY;
          CaretNew := PrevWordPos;
          {$IFDEF SYN_LAZARUS}
          if FFoldedLinesView.FoldedAtTextIndex[CaretNew.Y - 1] then begin
            CY := FindNextUnfoldedLine(CaretNew.Y, False);
            CaretNew := LogicalToPhysicalPos(Point(1 + Length(FTheLinesView[CY-1]), CY));
          end;
          MoveCaretAndSelectionPhysical
          {$ELSE}
          MoveCaretAndSelection
          {$ENDIF}
            (Caret, CaretNew, Command in [ecSelWordLeft, ecColSelWordLeft]);
          fLastCaretX := CaretX;                                               //mh 2000-10-19
          {$IFDEF SYN_LAZARUS}
          Update;
          {$ENDIF}
        end;
      ecWordRight, ecSelWordRight, ecColSelWordRight:
        begin
          Caret := CaretXY;
          CaretNew := NextWordPos;
          {$IFDEF SYN_LAZARUS}
          if FFoldedLinesView.FoldedAtTextIndex[CaretNew.Y - 1] then
            CaretNew := Point(1, FindNextUnfoldedLine(CaretNew.Y, True));
          MoveCaretAndSelectionPhysical
          {$ELSE}
          MoveCaretAndSelection
          {$ENDIF}
            (Caret, CaretNew, Command in [ecSelWordRight, ecColSelWordRight]);
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
            LogCaretXY:=PhysicalToLogicalPos(CaretXY);
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
              // delete text before the caret
              if (eoAutoIndent in fOptions) and
                 FBeautifier.CanUnindent(Self, FTheLinesView , FCaret) then
              begin
                // unindent
                FBeautifier.UnIndentLine(Self, ViewedTextBuffer, FCaret, CX);
                CaretX := CX;
                fLastCaretX := CaretX;
                StatusChanged([scCaretX]);
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
              end;
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
            LogCaretXY:=PhysicalToLogicalPos(CaretXY);
            if LogCaretXY.X <= Len then
            begin
              // delete char
              Counter:=GetCharLen(Temp,LogCaretXY.X);
              FTheLinesView.EditDelete(LogCaretXY.X, CaretY, Counter);
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
            WP := NextWordPos(True);
          end else
            WP := Point(Len + 1, CaretY);
          if (WP.X <> CaretX) or (WP.Y <> CaretY) then begin
            OldSelMode := FBlockSelection.ActiveSelectionMode;
            try
              SetBlockBegin(PhysicalToLogicalPos(WP));
              SetBlockEnd(PhysicalToLogicalPos(CaretXY));
              FBlockSelection.ActiveSelectionMode := smNormal;
              SetSelTextPrimitive(smNormal, nil, true);
              if Helper <> '' then
                FTabbedLinesView.EditInsert(CaretX, CaretY, Helper);
            finally
              FBlockSelection.ActiveSelectionMode := OldSelMode;
            end;
            CaretXY := Caret;
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
              SetSelTextPrimitive(smNormal, nil, true)
            finally
              FBlockSelection.ActiveSelectionMode := OldSelMode;
            end;
            CaretXY := WP;
          end;
        end;
{end}                                                                           //mh 2000-10-30
      ecDeleteLine:
        if not ReadOnly and not ((FTheLinesView.Count = 1) and (Length(FTheLinesView[0]) = 0))
        then begin
          if SelAvail then
            SetBlockBegin(PhysicalToLogicalPos(CaretXY));
          if FTheLinesView.Count = 1 then
            FTheLinesView.EditDelete(1, 1, length(FTheLinesView[0]))
          else begin
            FTheLinesView.EditLinesDelete(CaretY, 1);
          end;
          CaretXY := Point(1, CaretY); // like seen in the Delphi editor
        end;
      ecClearAll:
        begin
          if not ReadOnly then ClearAll;
        end;
      ecInsertLine,
      ecLineBreak:
        if not ReadOnly then begin
          // current line is empty (len = 0)
          if FTheLinesView.Count = 0 then
            FTheLinesView.Add('');
          if SelAvail then begin
            SetSelTextExternal('');
          end;
          Temp := LineText;
// This is sloppy, but the Right Thing would be to track the column of markers
// too, so they could be moved depending on whether they are after the caret...
          LogCaretXY:=PhysicalToLogicalPos(CaretXY);
          Len := Length(Temp);
          if LogCaretXY.X > Len + 1 then
            LogCaretXY.X := Len + 1;
          FTheLinesView.EditLineBreak(LogCaretXY.X, LogCaretXY.Y);
          if (eoAutoIndent in fOptions) and ((LogCaretXY.X > 1) or (Len = 0)) then
          begin
            FInternalCaret.AssignFrom(FCaret);
            FInternalCaret.IncForcePastEOL;
            FInternalCaret.LinePos := FInternalCaret.LinePos + 1;
            FBeautifier.IndentLine(Self, ViewedTextBuffer, FInternalCaret, CX);
            FInternalCaret.DecForcePastEOL;
          end else
            CX := 1;
          if Command = ecLineBreak then begin
            FCaret.IncForcePastEOL;
            CaretXY := Point(CX, CaretY + 1);
            FCaret.DecForcePastEOL;
          end;
          EnsureCursorPosVisible;                                               //JGF 2000-09-23
          fLastCaretX := CaretX;                                               //mh 2000-10-19
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
          {$IFDEF SYN_LAZARUS}
          if SelAvail and (eoTabIndent in Options) then
            DoBlockUnindent
          {$ENDIF};
      ecMatchBracket:
        FindMatchingBracket;
      ecChar:
        if not ReadOnly and (AChar >= #32) and (AChar <> #127) then begin
          if SelAvail then begin
            SetSelTextExternal(AChar);
          end else begin
            try
              FCaret.IncForcePastEOL;
              FCaret.AdjustToNextChar := True;

              LogCaretXY := FCaret.LineBytePos;
              Temp := LineText;
              Len := Length(Temp);
              if (not fInserting) and (LogCaretXY.X - 1<= Len) then begin
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
              FCaret.AdjustToNextChar := bCaretAdjust;
              FCaret.DecForcePastEOL;
            end;
          end;
        end
        else if not ReadOnly and (AChar = #13) then begin
          // ecLineBreak is not assigned
          // Insert a linebreak, but do not apply any other functionality (such as indent)
          if FTheLinesView.Count = 0 then
            FTheLinesView.Add('');
          if SelAvail then
            SetSelTextExternal('');
          LogCaretXY:=PhysicalToLogicalPos(CaretXY);
          FTheLinesView.EditLineBreak(LogCaretXY.X, LogCaretXY.Y);
          CaretXY := Point(1, CaretY + 1);
          EnsureCursorPosVisible;
          fLastCaretX := CaretX;
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
              TrimmedSetLine(CaretY - 1, Temp);                                 //JGF 2000-09-23
              if fInserting then
                Helper := '';
              fUndoList.AddChange(crInsert, StartOfBlock,
                PhysicalToLogicalPos(CaretXY),
                Helper, smNormal);
              if CaretX >= LeftChar + fCharsInWindow then
                LeftChar := LeftChar + min(25, fCharsInWindow - 1);
            finally
              FCaret.DecForcePastEOL;
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
          CY := FFoldedLinesView.ExpandedLineForBlockAtLine(CaretY);
          if CY > 0 then begin
            FFoldedLinesView.FoldAtTextIndex(CY-1);
            SetCaretXY(Point(1, CY));
          end;
        end;
      EcUnFoldCurrent:
          FFoldedLinesView.UnFoldAtTextIndex(CaretY-1);
      {$ENDIF}
      EcToggleMarkupWord:
          FMarkupHighCaret.ToggleCurrentWord;
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
  LogCaret:=PhysicalToLogicalPos(CaretXY);
  CX := LogCaret.X;
  CY := LogCaret.Y;
  // valid line?
  if (CY >= 1) and (CY <= FTheLinesView.Count) then begin
    Line := FTheLinesView[CY - 1];
    LineLen := Length(Line);
    WhiteChars := [#9,' '];
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
  if (CY >= 1) and (CY <= FTheLinesView.Count) then begin
    Line := FTheLinesView[CY - 1];

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
      if CY < FTheLinesView.Count then begin
        Line := FTheLinesView[CY];
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
  if (CY >= 1) and (CY <= FTheLinesView.Count) then begin
    Line := FTheLinesView[CY - 1];
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
        Line := FTheLinesView[CY - 1];
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
          Line := FTheLinesView[CY - 1];
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
  fBlockSelection.ActiveSelectionMode := Value;
end;

{begin}                                                                         //sbs 2000-11-19
procedure TCustomSynEdit.BeginUndoBlock(aList: TSynEditUndoList = nil);
begin
  if aList = nil then aList := fUndoList;
  aList.BeginBlock;
  IncPaintLock;
  FFoldedLinesView.Lock;
end;
{end}                                                                           //sbs 2000-11-19

procedure TCustomSynEdit.BeginUpdate;
begin
  IncPaintLock;
end;

{begin}                                                                         //sbs 2000-11-19
procedure TCustomSynEdit.EndUndoBlock(aList: TSynEditUndoList = nil);
begin
  if aList = nil then aList := fUndoList;
  // Write all trimming info to the end of the undo block,
  // so it will be undone first, and other UndoItems do see the expected spaces
  FFoldedLinesView.UnLock;
   // must be last => May call MoveCaretToVisibleArea, which must only happen
   // after unfold
  DecPaintLock;
  aList.EndBlock;
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

procedure TCustomSynEdit.AfterLoadFromFile;
begin
  if assigned(FFoldedLinesView) then
    FFoldedLinesView.CollapseDefaultFolds;
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
  if fGutter.Visible and fGutter.ChangesPart(0).Visible then
    InvalidateGutter; // Todo: Make the ChangeGutterPart an observer
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
    FGutter.AutoSizeDigitCount(FTheLinesView.Count);  // Todo: Make the LineNumberGutterPart an observer
    fGutter.RealGutterWidth(fCharWidth);
    nW := fGutter.Width;
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
  Value := MinMax(Value, 1{0}, 256);
  if (Value <> fTabWidth) then begin
    fTabWidth := Value;
    FTabbedLinesView.TabWidth := Value;
    FBlockSelection.TabWidth := Value;
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
  IncPaintLock;
  try
    {$IFDEF SYN_LAZARUS}
    //DebugLn(['TCustomSynEdit.SearchReplace ptStart=',dbgs(ptStart),' ptEnd=',dbgs(ptEnd),' ASearch="',dbgstr(ASearch),'" AReplace="',dbgstr(AReplace),'"']);
    while fTSearch.FindNextOne(FTheLinesView,ptStart,ptEnd,ptFoundStart,ptFoundEnd) do
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
            if not bReplaceAll then begin
              bReplaceAll := TRUE;
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
  ChangedOptions: TSynEditorOptions;
  i: Integer;
begin
  Value := Value - SYNEDIT_UNIMPLEMENTED_OPTIONS;
  if (Value <> fOptions) then begin
    ChangedOptions:=(fOptions-Value)+(Value-fOptions);
    fOptions := Value;
    FTrimmedLinesView.Enabled := eoTrimTrailingSpaces in fOptions;
    FCaret.AllowPastEOL := (eoScrollPastEol in fOptions);
    if not (eoScrollPastEol in Options) then
      LeftChar := LeftChar;
    // (un)register HWND as drop target
    if (eoDropFiles in ChangedOptions) and not (csDesigning in ComponentState) and HandleAllocated then
      ; // ToDo DragAcceptFiles
    if (eoPersistentCaret in ChangedOptions) and HandleAllocated then begin
      SetCaretRespondToFocus(Handle,not (eoPersistentCaret in fOptions));
      UpdateCaret;
    end;
    if (eoShowSpecialChars in ChangedOptions) and HandleAllocated then
      Invalidate;
    if (eoNoSelection in ChangedOptions) then
      FBlockSelection.Enabled := eoNoSelection in fOptions;
    fUndoList.GroupUndo := eoGroupUndo in fOptions;

    (* Deal with deprecated values
       Those are all controlled by mouse-actions.
       As long as the default mouse actions are set, the below will act as normal
    *)
    // eoShowCtrlMouseLinks
    if (eoShowCtrlMouseLinks in ChangedOptions) then begin
      if (eoShowCtrlMouseLinks in fOptions) then begin
        try
          FMouseActions.AddCommand(emcMouseLink, False, mbLeft, ccSingle, cdUp, [SYNEDIT_LINK_MODIFIER], [ssShift, ssAlt, ssCtrl]);
        except
        end;
      end else begin
        for i := FMouseActions.Count-1 downto 0 do
          if FMouseActions[i].Command = emcMouseLink then
            FMouseActions.Delete(i);
      end;
      if assigned(fMarkupCtrlMouse) then
        fMarkupCtrlMouse.UpdateCtrlMouse;
    end;
    // eoDragDropEditing
    if (eoDragDropEditing in ChangedOptions) then begin
      if (eoDragDropEditing in fOptions) then begin
        try
          FMouseSelActions.AddCommand(emcStartDragMove, False, mbLeft, ccSingle, cdDown, [], []);
        except
        end;
      end else begin
        for i := FMouseActions.Count-1 downto 0 do
          if FMouseActions[i].Command = emcStartDragMove then
            FMouseActions.Delete(i);
      end;
    end;
    // eoRightMouseMovesCursor
    if (eoRightMouseMovesCursor in ChangedOptions) then begin
      for i := FMouseActions.Count-1 downto 0 do
        if FMouseActions[i].Button = mbRight then
          FMouseActions[i].MoveCaret := (eoDragDropEditing in fOptions);
    end;
    // eoDoubleClickSelectsLine
    if (eoDoubleClickSelectsLine in ChangedOptions) then begin
      for i := FMouseActions.Count-1 downto 0 do
        if (FMouseActions[i].Button = mbLeft) and
           (FMouseActions[i].ClickCount = ccDouble) and
           (FMouseActions[i].IsMatchingShiftState([])) and
           ( (FMouseActions[i].Command = emcSelectWord) or
             (FMouseActions[i].Command = emcSelectLine) )
        then begin
          if (eoDoubleClickSelectsLine in fOptions)
          then FMouseActions[i].Command := emcSelectLine
          else FMouseActions[i].Command := emcSelectWord;
        end
    end;
    // eoAltSetsColumnMode
    if (eoAltSetsColumnMode in ChangedOptions) then begin
      if (eoAltSetsColumnMode in fOptions) then begin
        try
          FMouseActions.AddCommand(emcStartColumnSelections, True, mbLeft, ccSingle, cdDown, [ssAlt],          [ssShift, ssAlt], emcoSelectionStart);
          FMouseActions.AddCommand(emcStartColumnSelections, True, mbLeft, ccSingle, cdDown, [ssShift, ssAlt], [ssShift, ssAlt], emcoSelectionContinue);
        except
        end;
      end else begin
        for i := FMouseActions.Count-1 downto 0 do
          if FMouseActions[i].Command = emcStartColumnSelections then
            FMouseActions.Delete(i);
      end;
    end;

  end;
end;

{$IFDEF SYN_LAZARUS}
procedure TCustomSynEdit.SetOptions2(const Value: TSynEditorOptions2);
begin
  if (Value <> fOptions2) then begin
    fOptions2 := Value;
    if eoAlwaysVisibleCaret in fOptions2 then
      MoveCaretToVisibleArea;
  end;
end;
{$ENDIF}

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
    {$IFDEF SYN_LAZARUS}
    LastMouseCaret:=Point(-1,-1);
    fCharsInWindow := Max(1,(ClientWidth - fGutterWidth - 2 - ScrollBarWidth)
                            div fCharWidth);
    fLinesInWindow := Max(0,ClientHeight - ScrollBarWidth) div Max(1,fTextHeight);
    FFoldedLinesView.LinesInWindow := fLinesInWindow;

    fMarkupManager.LinesInWindow:= fLinesInWindow;
    //DebugLn('TCustomSynEdit.SizeOrFontChanged fLinesInWindow=',dbgs(fLinesInWindow),' ClientHeight=',dbgs(ClientHeight),' ',dbgs(fTextHeight));
    //debugln('TCustomSynEdit.SizeOrFontChanged A ClientWidth=',dbgs(ClientWidth),' fGutterWidth=',dbgs(fGutterWidth),' ScrollBarWidth=',dbgs(ScrollBarWidth),' fCharWidth=',dbgs(fCharWidth),' fCharsInWindow=',dbgs(fCharsInWindow),' Width=',dbgs(Width));
    {$ELSE}
    fCharsInWindow := Max(1,Max(0,(ClientWidth - fGutterWidth - 2
                                   - ScrollBarWidth) div Max(1,fCharWidth)));
    fLinesInWindow := ClientHeight div fTextHeight;
    {$ENDIF}
    if bFont then begin
      GutterChanged(self); // Todo: Make the LineNumberGutterPart (and others) an observer
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
      NewCaret.Y:= FFoldedLinesView.TextPosAddLines(NewCaret.Y, -1);
      s:=FTheLinesView[NewCaret.Y-1];
      PhysicalLineLen:=LogicalToPhysicalPos(Point(length(s)+1,NewCaret.Y)).X-1;
      NewCaret.X:=PhysicalLineLen+1;
    end;
  end else if not (eoScrollPastEol in fOptions) then begin
    s:=LineText;
    PhysicalLineLen:=LogicalToPhysicalPos(Point(length(s)+1,CaretY)).X-1;
    if (NewCaret.X>PhysicalLineLen+1) and (DX > 0) then begin
      // move to start of next line (if it was a move to the right)
      NewCaret.X:=1;
      NewCaret.Y:=FFoldedLinesView.TextPosAddLines(NewCaret.Y, +1);
    end;
  end;
  FInternalCaret.AdjustToNextChar := DX > 0;
  FInternalCaret.LineCharPos := NewCaret;

  // adjust selection
  IncPaintLock;
  FCaret.IncForcePastEOL;
  if SelectionCommand then begin
    //debugln('TCustomSynEdit.MoveCaretHorz A CaretXY=',dbgs(CaretXY),' NewCaret=',dbgs(NewCaret));
    if not FBlockSelection.SelCanContinue(FCaret) then
      SetBlockBegin(PhysicalToLogicalPos(CaretXY));
    SetBlockEnd(FInternalCaret.LineBytePos);
    //debugln('TCustomSynEdit.MoveCaretHorz B BB=',dbgs(BlockBegin),' BE=',dbgs(BlockEnd));
    AquirePrimarySelection;
  end else
    SetBlockBegin(FInternalCaret.LineBytePos);
  // commit new caret
  CaretXY := FInternalCaret.LineCharPos;
  FCaret.DecForcePastEOL;
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
  OldCaret: TPoint;
  SaveLastCaretX: LongInt;
  SelContinue: Boolean;
begin
  OldCaret:=CaretXY;
  NewCaret:=OldCaret;
  NewCaret.Y:=FFoldedLinesView.TextPosAddLines(NewCaret.Y, DY);
  if (OldCaret.Y<>NewCaret.Y) and (fLastCaretX>0) and (eoKeepCaretX in Options)
  then
    NewCaret.X:=fLastCaretX;
  IncPaintLock;
  SaveLastCaretX:=fLastCaretX;
  SelContinue := FBlockSelection.SelCanContinue(FCaret);
  CaretXY:=NewCaret;
  fLastCaretX:=SaveLastCaretX;
  // set caret and block begin / end
  if SelectionCommand then begin
    if not SelContinue then begin
      OldCaret := PhysicalToLogicalPos(OldCaret);
      if (OldCaret.X > Length(FTheLinesView[OldCaret.Y-1]) + 1) and
         not(eoScrollPastEol in Options) then
        OldCaret.X := Length(FTheLinesView[OldCaret.Y-1]) + 1;
      SetBlockBegin(OldCaret);
    end;
    SetBlockEnd(PhysicalToLogicalPos(CaretXY));
    AquirePrimarySelection;
  end else
    SetBlockBegin(PhysicalToLogicalPos(CaretXY));
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
  FInternalCaret.AllowPastEOL := FCaret.AllowPastEOL;
  FInternalCaret.LineBytePos := ptAfter;

  if SelectionCommand then begin
    if not SelAvail then SetBlockBegin(ptBefore);
    SetBlockEnd(FInternalCaret.LineBytePos);
    AquirePrimarySelection;
  end else
    SetBlockBegin(FInternalCaret.LineBytePos);
  CaretXY := FInternalCaret.LineCharPos;
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

procedure TCustomSynEdit.SetCaretAndSelection(const ptCaret, ptBefore,
  ptAfter: TPoint; Mode: TSynSelectionMode = smCurrent);
// caret is physical (screen)
// Before, After is logical (byte)
begin
  IncPaintLock;
  CaretXY := ptCaret;
  SetBlockBegin(ptBefore);
  SetBlockEnd(ptAfter);
  if Mode <> smCurrent then
    FBlockSelection.ActiveSelectionMode := Mode;
  AquirePrimarySelection;
  DecPaintLock;
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
  end;
  {$IFDEF SYN_LAZARUS}
  FUseUTF8:=fTextDrawer.UseUTF8;
  FLines.IsUtf8 := FUseUTF8;
  //debugln('TCustomSynEdit.RecalcCharExtent UseUTF8=',dbgs(UseUTF8),' Font.CanUTF8=',dbgs(Font.CanUTF8));
  {$ENDIF}
  GutterChanged(Self);
end;

procedure TCustomSynEdit.HighlighterAttrChanged(Sender: TObject);
var
  t: integer;
begin
  RecalcCharExtent;
  SizeOrFontChanged(TRUE);                                                      //jr 2000-10-01
  Invalidate;
  t := 0;
  if fHighlighter.AttributeChangeNeedScan then
    ScanFrom(t, FLines.Count - 1);
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
  SurrenderPrimarySelection;
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

      fUndoList.AddChange(TSynEditUndoIndent.Create(BB.Y, e, fBlockIndent));
      TSynEditStringList(FLines).MarkModified(BB.Y, e);
      {$IFDEF SYN_LAZARUS}
      if BlockBackward then
        SwapPoint(BB, BE);
      {$ENDIF}
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
    FTrimmedLinesView.ForceTrim; // Otherwise it may reset the block
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
        Line := PChar(FTheLinesView[x-1]);
        TempString:=StringOfChar(' ', GetDelLen);
        StrCat(FullStrToDelete,PChar(TempString));
        StrCat(FullStrToDelete,
                    PChar({$IFDEF SYN_LAZARUS}LineEnding{$ELSE}#13#10{$ENDIF}));
      end;
      {$IFNDEF SYN_LAZARUS}
      Line := PChar(FTheLinesView[e-1]);
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
              TempString := FTheLinesView[CaretY - 1];
              Delete(TempString, 1, Len);
              FTheLinesView[CaretY - 1] := TempString;
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
        fUndoList.AddChange(TSynEditUndoUnIndent.Create(BB.Y, e, FullStrToDelete));
        TSynEditStringList(FLines).MarkModified(BB.Y, e);
        {$IFDEF SYN_LAZARUS}
        if BlockBackward then Begin
          SwapPoint(BB, BE);
          SwapInt(FirstIndent, LastIndent);
        end;
        {$ENDIF}
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
      FTrimmedLinesView.ForceTrim; // Otherwise it may reset the block
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
  OldPos := LogicalCaretXY;
  NewPos := OldPos;

  if not (eoEnhanceHomeKey in fOptions) and (CaretX > 1) then 
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

    if FirstNonBlank >= 1 then 
    begin
      // this line is not blank
      LineStart := FirstNonBlank;
    end else 
    begin
      // this line is blank
      // -> use automatic line indent
      LineStart := FBeautifier.GetDesiredIndentForLine(Self, FTheLinesView, FCaret);
    end;

    NewPos.X:=LineStart;
    if (eoEnhanceHomeKey in fOptions) and (OldPos.X>1) and (OldPos.X<=NewPos.X)
    then begin
      NewPos.X:=1;
    end;
  end;

  MoveCaretAndSelection(OldPos, NewPos, Selection);
end;

procedure TCustomSynEdit.DoEndKey(Selection: boolean);
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
  OldPos := LogicalCaretXY;
  NewPos := OldPos;
  s := LineText;

  if not (eoEnhanceEndKey in fOptions2) and (CaretX <> Length(s)+1) then begin
    // not at end of real line -> jump to end of line
    NewPos.X := Length(s)+1;
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
      LineEnd := LastNonBlank + 1;
    end else begin
      // this line is blank
      // -> use automatic line indent
      LineEnd := FBeautifier.GetDesiredIndentForLine(Self, FTheLinesView, FCaret);
    end;

    NewPos.X:=LineEnd;
    if (eoEnhanceEndKey in fOptions2) and (OldPos.X <> Length(s)+1) and (OldPos.X >= NewPos.X)
    then begin
      NewPos.X := Length(s)+1;
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
  if Value then
    IncreaseChangeStamp;
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
  if (XY.Y >= 1) and (XY.Y <= FTheLinesView.Count) then begin
    Line := FTheLinesView[XY.Y - 1];
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

procedure TCustomSynEdit.UpdateModified;
begin
  if fUndoList.UnModifiedMarkerExists then
    Modified:=not fUndoList.IsTopMarkedAsUnmodified
  else if fRedoList.UnModifiedMarkerExists then
    Modified:=not fRedoList.IsTopMarkedAsUnmodified
  else
    Modified := fUndoList.CanUndo or fUndoList.FullUndoImpossible;              //mh 2000-10-03
end;

procedure TCustomSynEdit.UndoRedoAdded(Sender: TObject);
begin
  IncreaseChangeStamp;
  UpdateModified;
  // we have to clear the redo information, since adding undo info removes
  // the necessary context to undo earlier edit actions
  if (Sender = fUndoList) and not (fUndoList.IsInsideRedo) then            //mh 2000-10-30
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
  if (XY.Y >= 1) and (XY.Y <= FTheLinesView.Count) then begin
    Line := FTheLinesView[XY.Y - 1];
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

procedure Register;
begin
  RegisterClasses([TSynGutterPartList, TSynGutterSeparator, TSynGutterCodeFolding,
                  TSynGutterLineNumber, TSynGutterChanges, TSynGutterMarks]);

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

initialization
  SynDefaultBeautifier := TSynBeautifier.Create(Application);
  Register;

end.
