{
/***************************************************************************
                               SourceEditor.pp
                             -------------------

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
{ This unit builds the TSourceNotebook that the editors are held on.
  It also has a class that controls the editors (TSourceEditor)
}
unit SourceEditor;

{$mode objfpc}
{$H+}

interface

{$I ide.inc}

{ $DEFINE VerboseIDECompletionBox}

uses
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  SynEditMouseCmds,
  // RTL + FCL
  Classes, SysUtils, StrUtils, Types, Contnrs, Math, RegExpr, Laz_AVL_Tree,
  // LCL
  Controls, Forms, ComCtrls, StdCtrls, Graphics, Dialogs, Extctrls, Menus,
  LCLProc, LCLType, LCLIntf, ClipBrd, HelpIntfs, Messages, LMessages,
  // LazControls
  ExtendedNotebook,
  // LazUtils
  LConvEncoding, FileUtil, LazFileUtils, LazFileCache, LazUTF8,
  LazMethodList, LazLoggerBase, LazLogger, Translations, LazUtilities, LazTracer,
  LazStringUtils,
  // codetools
  BasicCodeTools, CodeBeautifier, CodeToolManager, CodeCache, SourceLog,
  LinkScanner, CodeTree, SourceChanger, IdentCompletionTool,
  // synedit
  SynEditLines, SynEditStrConst, SynEditTypes, SynEdit,
  SynEditHighlighter, SynEditAutoComplete, SynEditKeyCmds, SynCompletion,
  SynEditMiscClasses, SynEditMarkupHighAll, SynEditMarks,
  SynBeautifier, SynPluginMultiCaret,
  SynPluginSyncronizedEditBase, SourceSynEditor,
  SynExportHTML, SynHighlighterPas, SynEditMarkup, SynEditMarkupIfDef,
  // IdeIntf
  SrcEditorIntf, MenuIntf, LazIDEIntf, PackageIntf, IDEHelpIntf, IDEImagesIntf,
  IDEWindowIntf, ProjectIntf, MacroDefIntf, ToolBarIntf, IDEDialogs, IDECommands,
  EditorSyntaxHighlighterDef,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // IDE units
  IDECmdLine, LazarusIDEStrConsts, EditorOptions,
  EnvironmentOpts, WordCompletion, FindReplaceDialog, IDEProcs, IDEOptionDefs,
  IDEHelpManager, MacroPromptDlg, TransferMacros, CodeContextForm,
  SrcEditHintFrm, etMessagesWnd, etSrcEditMarks, CodeMacroPrompt,
  CodeTemplatesDlg, CodeToolsOptions, editor_general_options, SortSelectionDlg,
  EncloseSelectionDlg, EncloseIfDef, InvertAssignTool, SourceEditProcs,
  SourceMarks, CharacterMapDlg, SearchFrm, MultiPasteDlg, EditorMacroListViewer,
  EditorToolbarStatic, editortoolbar_options, InputhistoryWithSearchOpt,
  FPDocHints, MainIntf, GotoFrm, BaseDebugManager, Debugger;

type
  TSourceNotebook = class;
  TSourceEditorManager = class;
  TSourceEditor = class;

  TNotifyFileEvent = procedure(Sender: TObject; Filename : AnsiString) of object;

  TOnProcessUserCommand = procedure(Sender: TObject;
            Command: word; var Handled: boolean) of object;
  TOnUserCommandProcessed = procedure(Sender: TObject;
            Command: word; var Handled: boolean) of object;

  TPackageForSourceEditorEvent = function(out APackage: TIDEPackage;
    ASrcEdit: TObject): TLazPackageFile of object;

  TPlaceBookMarkEvent = procedure(Sender: TObject; var Mark: TSynEditMark) of object;
  TPlaceBookMarkIdEvent = procedure(Sender: TObject; ID: Integer) of object;
  TBookMarkActionEvent = procedure(Sender: TObject; ID: Integer; Toggle: Boolean) of object;

  TUpdateProjectFileEvent = procedure(Sender: TObject; AnUpdates: TSrcEditProjectUpdatesNeeded) of object;

  TCharSet = set of Char;

  // for TSourcEditor.CenterCursorHoriz
  TSourceEditHCenterMode =
  ( hcmCenter,          // Center X-Caret to exact middle of Screen
    hcmCenterKeepEOL,   // Center X-Caret to middle of Screen, but keep EOL at right border
    hcmSoft,            // Soft Center (distance to screen edge) Caret
    hcmSoftKeepEOL      // Soft Center (distance to screen edge) Caret, but keep EOL at right border
  );

  TSourceEditCompletionForm = class(TSynCompletionForm)
  private
    FTextHighLightColor: TColor;
  public
    property TextHighLightColor: TColor read FTextHighLightColor write FTextHighLightColor;
  end;

  { TSourceEditCompletion }

  TSourceEditCompletion=class(TSynCompletion)
  private
    FIdentCompletionJumpToError: boolean;
    ccSelection: String;
    // colors for the completion form (popup form, e.g. word completion)

    FActiveEditBackgroundColor: TColor;
    FActiveEditBackgroundSelectedColor: TColor;
    FActiveEditBorderColor: TColor;
    FActiveEditTextColor: TColor;
    FActiveEditTextSelectedColor: TColor;
    FActiveEditTextHighLightColor: TColor;

    procedure ccExecute(Sender: TObject);
    procedure ccCancel(Sender: TObject);
    procedure ccComplete(var Value: string; SourceValue: string;
                         var SourceStart, SourceEnd: TPoint;
                         KeyChar: TUTF8Char; Shift: TShiftState);
    function OnSynCompletionPaintItem(const AKey: string; ACanvas: TCanvas;
                 X, Y: integer; ItemSelected: boolean; Index: integer): boolean;
    function OnSynCompletionMeasureItem(const AKey: string; ACanvas: TCanvas;
                                 ItemSelected: boolean; Index: integer): TPoint;
    procedure OnSynCompletionSearchPosition(var APosition: integer);
    procedure OnSynCompletionCompletePrefix(Sender: TObject);
    procedure OnSynCompletionNextChar(Sender: TObject);
    procedure OnSynCompletionPrevChar(Sender: TObject);
    procedure OnSynCompletionKeyPress(Sender: TObject; var Key: Char);
    procedure OnSynCompletionUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure OnSynCompletionPositionChanged(Sender: TObject);

    function InitIdentCompletionValues(S: TStrings): boolean;
    procedure StartShowCodeHelp;
    procedure CompletionFormResized(Sender: TObject);
  protected
    CurrentCompletionType: TCompletionType;
    function Manager: TSourceEditorManager;
    function GetCompletionFormClass: TSynBaseCompletionFormClass; override;
  public
    constructor Create(AOwner: TComponent); override;
    property IdentCompletionJumpToError: Boolean
      read FIdentCompletionJumpToError write FIdentCompletionJumpToError;
  end;

  { TSourceEditorSharedValues }

  TSourceEditorSharedValues = class(TSourceEditorSharedValuesBase)
  private
    FSharedEditorList: TFPList; // list of TSourceEditor sharing one TSynEdit
    function GetOtherSharedEditors(Caller: TSourceEditor; Index: Integer): TSourceEditor;
    function GetSharedEditors(Index: Integer): TSourceEditor;
    function SynEditor: TIDESynEditor;
  protected
    function GetSharedEditorsBase(Index: Integer): TSourceEditorBase; override;
  public
    procedure AddSharedEditor(AnEditor: TSourceEditor);
    procedure RemoveSharedEditor(AnEditor: TSourceEditor);
    procedure SetActiveSharedEditor(AnEditor: TSourceEditor);
    function  SharedEditorCount: Integer; override;
    function  OtherSharedEditorCount: Integer;
    property  SharedEditors[Index: Integer]: TSourceEditor read GetSharedEditors;
    property  OtherSharedEditors[Caller: TSourceEditor; Index: Integer]: TSourceEditor
              read GetOtherSharedEditors;
  private
    FExecutionMark: TSourceMark;
    FMarksRequested: Boolean;
    FMarklingsValid: boolean;
    FMarksRequestedForFile: String;
    function GetExecutionLine: Integer;
  public
    UpdatingExecutionMark: Integer;
    procedure CreateExecutionMark;
    property ExecutionLine: Integer read GetExecutionLine;// write FExecutionLine;
    property ExecutionMark: TSourceMark read FExecutionMark write FExecutionMark;
    procedure SetExecutionLine(NewLine: integer);
    property MarksRequested: Boolean read FMarksRequested write FMarksRequested;
    property MarksRequestedForFile: String read FMarksRequestedForFile write FMarksRequestedForFile;
  private
    FInGlobalUpdate: Integer;
    FModified: boolean;
    FIgnoreCodeBufferLock: integer;
    FEditorStampCommitedToCodetools: int64;
    FCodeBuffer: TCodeBuffer;
    FLinkScanners: TFPList; // list of TLinkScanner
    FMainLinkScanner: TLinkScanner;
    FLastWarnedMainLinkFilename: string;
    function GetModified: Boolean;
    procedure SetCodeBuffer(const AValue: TCodeBuffer);
    procedure SetModified(const AValue: Boolean);
    procedure OnCodeBufferChanged(Sender: TSourceLog; SrcLogEntry: TSourceLogEntry);
  public
    procedure BeginGlobalUpdate;
    procedure EndGlobalUpdate;
    property Modified: Boolean read GetModified write SetModified;
    property  IgnoreCodeBufferLock: Integer read FIgnoreCodeBufferLock;
    procedure IncreaseIgnoreCodeBufferLock;
    procedure DecreaseIgnoreCodeBufferLock;
    function NeedsUpdateCodeBuffer: boolean;
    procedure UpdateCodeBuffer;
    property CodeBuffer: TCodeBuffer read FCodeBuffer write SetCodeBuffer;
    // IfDef nodes
    procedure ConnectScanner(Scanner: TLinkScanner);
    procedure DisconnectScanner(Scanner: TLinkScanner);
    function GetMainLinkScanner(Scan: boolean): TLinkScanner;
  public
    constructor Create;
    destructor Destroy; override;
    function Filename: string; override;
  end;

{ TSourceEditor ---
  TSourceEditor is the class that controls access for a single source editor,
  which is part of TSourceNotebook. }

  TSourceEditor = class(TSourceEditorBase)
  private
    //FAOwner is normally a TSourceNotebook.  This is set in the Create constructor.
    FAOwner: TComponent;
    FIsLocked: Boolean;
    FProjectFileUpdatesNeeded: TSrcEditProjectUpdatesNeeded;
    FSharedValues: TSourceEditorSharedValues;
    FEditor: TIDESynEditor;
    FTempCaret: TPoint;
    FTempTopLine: Integer;
    FEditPlugin: TETSynPlugin; // used to update the "Messages Window"
                               // when text is inserted/deleted
    FOnIfdefNodeStateRequest: TSynMarkupIfdefStateRequest;
    FLastIfDefNodeScannerStep: integer;

    FSyncroLockCount: Integer;
    FPageName: string;

    FPopUpMenu: TPopupMenu;
    FMouseActionPopUpMenu: TPopupMenu;
    FSyntaxHighlighterType: TLazSyntaxHighlighter;
    FErrorLine: integer;
    FErrorColumn: integer;
    FLineInfoNotification: TIDELineInfoNotification;
    FInEditorChangedUpdating: Boolean;

    FOnEditorChange: TStatusChangeEvent;
    FVisible: Boolean;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseWheel : TMouseWheelEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyUp: TKeyEvent;

    FSourceNoteBook: TSourceNotebook;
    procedure EditorMouseMoved(Sender: TObject; Shift: TShiftState; X,Y:Integer);
    procedure EditorMouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X,Y: Integer);
    procedure EditorMouseWheel(Sender: TObject; Shift: TShiftState;
         WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorKeyUp({%H-}Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure EditorStatusChanged(Sender: TObject; {%H-}Changes: TSynStatusChanges);
    procedure EditorPaste(Sender: TObject; var AText: String;
         var AMode: TSynSelectionMode; ALogStartPos: TPoint;
         var AnAction: TSynCopyPasteAction);
    procedure EditorPlaceBookmark(Sender: TObject; var Mark: TSynEditMark);
    procedure EditorClearBookmark(Sender: TObject; var Mark: TSynEditMark);
    procedure EditorEnter(Sender: TObject);
    procedure EditorActivateSyncro(Sender: TObject);
    procedure EditorDeactivateSyncro(Sender: TObject);
    procedure EditorChangeUpdating({%H-}ASender: TObject; AnUpdating: Boolean);
    function  EditorHandleMouseAction(AnAction: TSynEditMouseAction;
                                      var {%H-}AnInfo: TSynEditMouseActionInfo): Boolean;
    function GetCodeBuffer: TCodeBuffer;
    function GetExecutionLine: integer;
    function GetHasExecutionMarks: Boolean;
    function GetSharedEditors(Index: Integer): TSourceEditor;
    procedure SetCodeBuffer(NewCodeBuffer: TCodeBuffer);
    function GetSource: TStrings;
    procedure SetIsLocked(const AValue: Boolean);
    procedure UpdateExecutionSourceMark;
    procedure UpdatePageName;
    procedure SetSource(Value: TStrings);
    function GetCurrentCursorXLine: Integer;
    procedure SetCurrentCursorXLine(num : Integer);
    function GetCurrentCursorYLine: Integer;
    procedure SetCurrentCursorYLine(num: Integer);
    Function GetInsertMode: Boolean;
    procedure SetPopupMenu(NewPopupMenu: TPopupMenu);

    function GotoLine(Value: Integer): Integer;

    procedure CreateEditor(AOwner: TComponent; AParent: TWinControl);
    procedure UpdateNoteBook(const ANewNoteBook: TSourceNotebook; ANewPage: TTabSheet);
    procedure SetVisible(Value: boolean);
    procedure UnbindEditor;

    procedure UpdateIfDefNodeStates(Force: Boolean = False);
  protected
    function GetPageCaption: string; override;
    function GetPageName: string; override;
    procedure SetPageName(const AValue: string);
  protected
    procedure DoMultiCaretBeforeCommand(Sender: TObject; ACommand: TSynEditorCommand;
      var AnAction: TSynMultiCaretCommandAction; var {%H-}AFlags: TSynMultiCaretCommandFlags);
    procedure ProcessCommand(Sender: TObject;
       var Command: TSynEditorCommand; var AChar: TUTF8Char; {%H-}Data: pointer);
    procedure ProcessUserCommand(Sender: TObject;
       var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure UserCommandProcessed(Sender: TObject;
       var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    function AutoCompleteChar(Char: TUTF8Char; var AddChar: boolean;
       Category: TAutoCompleteOption): boolean;
    function AutoBlockCompleteChar({%H-}Char: TUTF8Char; var {%H-}AddChar: boolean;
       Category: TAutoCompleteOption; aTextPos: TPoint; Line: string): boolean;
    function AutoBlockCompleteChar({%H-}Char: TUTF8Char): boolean;
    procedure AutoCompleteBlock;

    procedure FocusEditor;// called by TSourceNotebook when the Notebook page
                          // changes so the editor is focused
    procedure OnGutterClick(Sender: TObject; {%H-}X, {%H-}Y, Line: integer;
         {%H-}Mark: TSynEditMark);
    procedure OnEditorSpecialLineColor(Sender: TObject; Line: integer;
         var Special: boolean; Markup: TSynSelectedColor);
    function RefreshEditorSettings: Boolean;
    function GetModified: Boolean; override;
    procedure SetModified(const NewValue: Boolean); override;
    procedure SetSyntaxHighlighterType(
                                 ASyntaxHighlighterType: TLazSyntaxHighlighter);
    procedure SetErrorLine(NewLine: integer);
    procedure SetExecutionLine(NewLine: integer);
    procedure StartIdentCompletionBox(JumpToError, CanAutoComplete: boolean);
    procedure StartWordCompletionBox;

    function IsFirstShared(Sender: TObject): boolean;

    function GetFilename: string; override;
    function GetEditorControl: TWinControl; override;
    function GetCodeToolsBuffer: TObject; override;
    Function GetReadOnly: Boolean; override;
    procedure SetReadOnly(const NewValue: boolean); override;

    function Manager: TSourceEditorManager;
    property Visible: Boolean read FVisible write SetVisible default False;
    function GetSharedValues: TSourceEditorSharedValuesBase; override;
    function IsSharedWith(AnOtherEditor: TSourceEditor): Boolean;
    procedure BeforeCodeBufferReplace;
    procedure AfterCodeBufferReplace;
    function Close: Boolean;
  public
    constructor Create(AOwner: TComponent; AParent: TWinControl; ASharedEditor: TSourceEditor = nil);
    destructor Destroy; override;

    // codebuffer
    procedure BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}(ACaller: String = ''){$ENDIF}; override;
    procedure EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}(ACaller: String = ''){$ENDIF}; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure BeginGlobalUpdate;
    procedure EndGlobalUpdate;
    procedure IncreaseIgnoreCodeBufferLock; override;
    procedure DecreaseIgnoreCodeBufferLock; override;
    procedure UpdateCodeBuffer; override;// copy the source from EditorComponent
    function NeedsUpdateCodeBuffer: boolean; override;
    procedure ConnectScanner(Scanner: TLinkScanner);

    // find
    procedure StartFindAndReplace(Replace:boolean);
    procedure AskReplace(Sender: TObject; const ASearch, AReplace:
       string; Line, Column: integer; out Action: TSrcEditReplaceAction); override;
    procedure OnReplace(Sender: TObject; const ASearch, AReplace:
       string; {%H-}Line, {%H-}Column: integer; var Action: TSynReplaceAction);
    function  DoFindAndReplace(aFindText, aReplaceText: String; anOptions: TSynSearchOptions): Integer;
    procedure FindNextUTF8;
    procedure FindPrevious;
    procedure FindNextWordOccurrence(DirectionForward: boolean);
    procedure ShowGotoLineDialog;

    // dialogs
    procedure GetDialogPosition(Width, Height: integer; out Left, Top: integer);
    procedure ActivateHint(const ClientPos: TPoint; const ABaseURL, AHint: string;
      AAutoShown: Boolean = True); overload;
    procedure ActivateHint(ClientRect: TRect; const ABaseURL, AHint: string;
      AAutoShown: Boolean; AMouseOffset: Boolean = True); overload;

    // selections
    function SelectionAvailable: boolean; override;
    function GetText(OnlySelection: boolean): string; override;
    procedure SelectText(const StartPos, EndPos: TPoint); override;
    procedure InsertLine(StartLine: Integer; const NewText: String; aKeepMarks: Boolean = False); override;
    procedure ReplaceLines(StartLine, EndLine: integer; const NewText: string; aKeepMarks: Boolean = False); override;
    procedure EncloseSelection;
    procedure UpperCaseSelection;
    procedure LowerCaseSelection;
    procedure SwapCaseSelection;
    procedure TabsToSpacesInSelection;
    procedure CommentSelection;
    procedure UncommentSelection;
    procedure ToggleCommentSelection;
    procedure UpdateCommentSelection(CommentOn, Toggle: Boolean);
    procedure ConditionalSelection;
    procedure SortSelection;
    procedure BreakLinesInSelection;
    procedure InvertAssignment;
    procedure SelectToBrace;
    procedure SelectCodeBlock;
    procedure SelectWord;
    procedure SelectLine;
    procedure SelectParagraph;
    function CommentText(const Txt: string; CommentType: TCommentType): string;
    procedure InsertCharacterFromMap;
    procedure InsertLicenseNotice(const Notice: string; CommentType: TCommentType);
    procedure InsertGPLNotice(CommentType: TCommentType; Translated: boolean);
    procedure InsertLGPLNotice(CommentType: TCommentType; Translated: boolean);
    procedure InsertModifiedLGPLNotice(CommentType: TCommentType; Translated: boolean);
    procedure InsertMITNotice(CommentType: TCommentType; Translated: boolean);
    procedure InsertUsername;
    procedure InsertDateTime;
    procedure InsertChangeLogEntry;
    procedure InsertCVSKeyword(const AKeyWord: string);
    procedure InsertGUID;
    procedure InsertFilename;
    function GetSelEnd: Integer; override;
    function GetSelStart: Integer; override;
    procedure SetSelEnd(const AValue: Integer); override;
    procedure SetSelStart(const AValue: Integer); override;
    function GetSelection: string; override;
    procedure SetSelection(const AValue: string); override;
    procedure CopyToClipboard; override;
    procedure CutToClipboard; override;
    function GetBookMark(BookMark: Integer; out X, Y: Integer): Boolean; override;
    procedure SetBookMark(BookMark: Integer; X, Y: Integer); override;

    procedure ExportAsHtml(AFileName: String);

    // context help
    procedure FindHelpForSourceAtCursor;
    //Smart hint
    procedure ShowSmartHintForSourceAtCursor;

    // editor commands
    procedure DoEditorExecuteCommand(EditorCommand: word); override;
    procedure MoveToWindow(AWindowIndex: Integer); override;
    procedure CopyToWindow(AWindowIndex: Integer); override;

    // used to get the word at the mouse cursor
    function GetWordFromCaret(const ACaretPos: TPoint): String;
    function GetWordAtCurrentCaret: String;
    function GetOperandFromCaret(const ACaretPos: TPoint): String;
    function GetOperandAtCurrentCaret: String;
    function CaretInSelection(const ACaretPos: TPoint): Boolean;

    // cursor
    procedure CenterCursor(SoftCenter: Boolean = False); // vertical
    procedure CenterCursorHoriz(HCMode: TSourceEditHCenterMode); // horiz
    function TextToScreenPosition(const Position: TPoint): TPoint; override;
    function ScreenToTextPosition(const Position: TPoint): TPoint; override;
    function ScreenToPixelPosition(const Position: TPoint): TPoint; override;
    function GetCursorScreenXY: TPoint; override;
    function GetCursorTextXY: TPoint; override;
    procedure SetCursorScreenXY(const AValue: TPoint); override;
    procedure SetCursorTextXY(const AValue: TPoint); override;
    function GetBlockBegin: TPoint; override;
    function GetBlockEnd: TPoint; override;
    procedure SetBlockBegin(const AValue: TPoint); override;
    procedure SetBlockEnd(const AValue: TPoint); override;
    function GetLinesInWindow: Integer; override;
    function GetTopLine: Integer; override;
    procedure SetTopLine(const AValue: Integer); override;
    function CursorInPixel: TPoint; override;
    function IsCaretOnScreen(ACaret: TPoint; UseSoftCenter: Boolean = False): Boolean;

    // text
    procedure MultiPasteText;
    function SearchReplace(const ASearch, AReplace: string;
                           SearchOptions: TSrcEditSearchOptions): integer; override;
    function GetSourceText: string; override;
    procedure SetSourceText(const AValue: string); override;
    function LineCount: Integer; override;
    function WidthInChars: Integer; override;
    function HeightInLines: Integer; override;
    function CharWidth: integer; override;
    function GetLineText: string; override;
    procedure SetLineText(const AValue: string); override;
    function GetLines: TStrings; override;
    procedure SetLines(const AValue: TStrings); override;

    // context
    function GetProjectFile: TLazProjectFile; override;
    procedure UpdateProjectFile(AnUpdates: TSrcEditProjectUpdatesNeeded = []); override;
    function GetDesigner(LoadForm: boolean): TIDesigner; override;

    // notebook
    procedure Activate;
    function PageIndex: integer;
    function IsActiveOnNoteBook: boolean;
    procedure CheckActiveWindow;

    // debugging
    procedure DoRequestExecutionMarks({%H-}Data: PtrInt);
    procedure FillExecutionMarks;
    procedure ClearExecutionMarks;
    procedure LineInfoNotificationChange(const {%H-}ASender: TObject; const ASource: String);
    function  SourceToDebugLine(aLinePos: Integer): Integer;
    function  DebugToSourceLine(aLinePos: Integer): Integer;

    procedure InvalidateAllIfdefNodes;
    procedure SetIfdefNodeState(ALinePos, AstartPos: Integer; AState: TSynMarkupIfdefNodeState);
    property OnIfdefNodeStateRequest: TSynMarkupIfdefStateRequest read FOnIfdefNodeStateRequest write FOnIfdefNodeStateRequest;
  public
    // properties
    property CodeBuffer: TCodeBuffer read GetCodeBuffer write SetCodeBuffer;
    property CurrentCursorXLine: Integer
       read GetCurrentCursorXLine write SetCurrentCursorXLine;
    property CurrentCursorYLine: Integer
       read GetCurrentCursorYLine write SetCurrentCursorYLine;
    property EditorComponent: TIDESynEditor read FEditor;
    property ErrorLine: integer read FErrorLine write SetErrorLine;
    property ExecutionLine: integer read GetExecutionLine write SetExecutionLine;
    property HasExecutionMarks: Boolean read GetHasExecutionMarks;
    property InsertMode: Boolean read GetInsertmode;
    property OnEditorChange: TStatusChangeEvent read FOnEditorChange
                                                write FOnEditorChange;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property Owner: TComponent read FAOwner;
    property PageName: string read GetPageName write SetPageName;
    property PopupMenu: TPopupMenu read FPopUpMenu write SetPopUpMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Source: TStrings read GetSource write SetSource;
    property SourceNotebook: TSourceNotebook read FSourceNoteBook;
    property SyntaxHighlighterType: TLazSyntaxHighlighter
       read fSyntaxHighlighterType write SetSyntaxHighlighterType;
    property SyncroLockCount: Integer read FSyncroLockCount;
    function SharedEditorCount: Integer;
    property SharedEditors[Index: Integer]: TSourceEditor read GetSharedEditors;
    property SharedValues: TSourceEditorSharedValues read FSharedValues;
    property IsLocked: Boolean read FIsLocked write SetIsLocked;
  end;

  //============================================================================

  { TSourceNotebook }

  TJumpHistoryAction = (jhaBack, jhaForward, jhaViewWindow);
  TCloseSrcEditorOption = (ceoCloseOthers, ceoCloseOthersOnRightSide);
  TCloseSrcEditorOptions = set of TCloseSrcEditorOption;

  TOnJumpToHistoryPoint = procedure(out NewCaretXY: TPoint;
                                    out NewTopLine: integer;
                                    out DestEditor: TSourceEditor;
                                    Action: TJumpHistoryAction) of object;
  TOnAddJumpPoint = procedure(ACaretXY: TPoint; ATopLine: integer;
                  AEditor: TSourceEditor; DeleteForwardHistory: boolean) of object;
  TOnMovingPage = procedure(Sender: TObject;
                            OldPageIndex, NewPageIndex: integer) of object;
  TOnCloseSrcEditor = procedure(Sender: TObject; ACloseOptions: TCloseSrcEditorOptions) of object;
  TOnShowHintForSource = procedure(SrcEdit: TSourceEditor;
                                   CaretPos: TPoint; AutoShown: Boolean) of object;
  TOnInitIdentCompletion = procedure(Sender: TObject; JumpToError: boolean;
                                     out Handled, Abort: boolean) of object;
  TSrcEditPopupMenuEvent = procedure(const AddMenuItemProc: TAddMenuItemProc
                                     ) of object;
  TOnShowCodeContext = procedure(JumpToError: boolean;
                                 out Abort: boolean) of object;
  TOnGetIndentEvent = function(Sender: TObject; Editor: TSourceEditor;
      LogCaret, OldLogCaret: TPoint; FirstLinePos, LinesCount: Integer;
      Reason: TSynEditorCommand; SetIndentProc: TSynBeautifierSetIndentProc
     ): boolean of object;

  TSourceNotebookState = (
    snIncrementalFind,
    snWarnedFont,
    snUpdateStatusBarNeeded,
    snNotebookPageChangedNeeded
    );
  TSourceNotebookStates = set of TSourceNotebookState;

  TSourceNotebookUpdateFlag = (
    ufPageNames,
    ufTabsAndPage,
    ufStatusBar,
    ufProjectFiles,
    ufFocusEditor,
    ufActiveEditorChanged,
    ufPageIndexChanged
  );
  TSourceNotebookUpdateFlags = set of TSourceNotebookUpdateFlag;

  TBrowseEditorTabHistoryDialog = class;

  { TSourceNotebook }

  TSourceNotebook = class(TSourceEditorWindowInterface)
    GoToLineMenuItem: TMenuItem;
    OpenFolderMenuItem: TMenuItem;
    StatusPopUpMenu: TPopupMenu;
    StatusBar: TStatusBar;
    procedure CompleteCodeMenuItemClick(Sender: TObject);
    procedure DbgPopUpMenuPopup(Sender: TObject);
    procedure EditorLockClicked(Sender: TObject);
    procedure EncodingClicked(Sender: TObject);
    procedure ExtractProcMenuItemClick(Sender: TObject);
    procedure FindOverloadsMenuItemClick(Sender: TObject);
    procedure FormMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure GoToLineMenuItemClick(Sender: TObject);
    procedure HighlighterClicked(Sender: TObject);
    procedure InsertCharacter(const C: TUTF8Char);
    procedure InvertAssignmentMenuItemClick(Sender: TObject);
    procedure LineEndingClicked(Sender: TObject);
    procedure MakeResourceStringMenuItemClick(Sender: TObject);
    procedure NotebookPageChanged(Sender: TObject);
    procedure NotebookShowTabHint(Sender: TObject; HintInfo: PHintInfo);
    procedure OnPopupMenuOpenFile(Sender: TObject);
    procedure OnPopupOpenPackageFile(Sender: TObject);
    procedure OnPopupOpenProjectInsp(Sender: TObject);
    procedure OpenAtCursorClicked(Sender: TObject);
    procedure OpenFolderMenuItemClick(Sender: TObject);
    procedure RenameIdentifierMenuItemClick(Sender: TObject);
    procedure ShowAbstractMethodsMenuItemClick(Sender: TObject);
    procedure ShowEmptyMethodsMenuItemClick(Sender: TObject);
    procedure ShowUnusedUnitsMenuItemClick(Sender: TObject);
    procedure SourceNotebookDropFiles(Sender: TObject;
      const FileNames: array of String);
    procedure SrcEditMenuCopyToExistingWindowClicked(Sender: TObject);
    procedure SrcEditMenuFindInWindowClicked(Sender: TObject);
    procedure SrcEditMenuMoveToExistingWindowClicked(Sender: TObject);
    procedure SrcPopUpMenuPopup(Sender: TObject);
    procedure StatusBarClick(Sender: TObject);
    procedure StatusBarDblClick(Sender: TObject);
    procedure StatusBarContextPopup(Sender: TObject; MousePos: TPoint;
      var {%H-}Handled: Boolean);
    procedure StatusBarDrawPanel({%H-}AStatusBar: TStatusBar; APanel: TStatusPanel;
      const ARect: TRect);
    procedure TabPopUpMenuPopup(Sender: TObject);
  private
    FNotebook: TExtendedNotebook;
    FBaseCaption: String;
    FIsClosing: Boolean;
    FSrcEditsSortedForFilenames: TAvlTree; // TSourceEditorInterface sorted for Filename
    TabPopUpMenu, SrcPopUpMenu, DbgPopUpMenu: TPopupMenu;
    procedure ApplyPageIndex;
    procedure ExecuteEditorItemClick(Sender: TObject);
  public
    procedure DeleteBreakpointClicked(Sender: TObject);
    procedure ToggleBreakpointClicked(Sender: TObject);
    procedure ToggleBreakpointEnabledClicked(Sender: TObject);
  private
    FManager: TSourceEditorManager;
    FUpdateLock, FFocusLock: Integer;
    FUpdateFlags: TSourceNotebookUpdateFlags;
    FPageIndex: Integer;
    fAutoFocusLock: integer;
    FIncrementalSearchPos: TPoint; // last set position
    fIncrementalSearchStartPos: TPoint; // position where to start searching
    FIncrementalSearchStr, FIncrementalFoundStr: string;
    FIncrementalSearchBackwards : Boolean;
    FIncrementalSearchEditor: TSourceEditor; // editor with active search (MWE:shouldnt all FIncrementalSearch vars go to that editor ?)
    FLastCodeBuffer: TCodeBuffer;
    FProcessingCommand: boolean;
    FSourceEditorList: TFPList; // list of TSourceEditor
    FHistoryList: TFPList; // list of TSourceEditor page order for when a window closes
    FHistoryDlg: TBrowseEditorTabHistoryDialog;
    FStopBtnIdx: Integer;
    FOnEditorPageCaptionUpdate: TMethodList;
  private
    FUpdateTabAndPageTimer: TTimer;
    FWindowID: Integer;
    // PopupMenu
    procedure BuildPopupMenu;
    //forwarders to FNoteBook
    function GetNoteBookPage(Index: Integer): TTabSheet;
    function GetNotebookPages: TStrings;
    function GetPageCount: Integer;
    function GetPageIndex: Integer;
    procedure SetPageIndex(AValue: Integer);

    procedure UpdateHighlightMenuItems(SrcEdit: TSourceEditor);
    procedure UpdateLineEndingMenuItems(SrcEdit: TSourceEditor);
    procedure UpdateEncodingMenuItems(SrcEdit: TSourceEditor);
    procedure RemoveUserDefinedMenuItems;
    function AddUserDefinedPopupMenuItem(const NewCaption: string;
                                     const NewEnabled: boolean;
                                     const NewOnClick: TNotifyEvent): TIDEMenuItem;
    procedure RemoveContextMenuItems;
    function AddContextPopupMenuItem(const NewCaption: string;
                                     const NewEnabled: boolean;
                                     const NewOnClick: TNotifyEvent): TIDEMenuItem;

    // Incremental Search
    procedure UpdateActiveEditColors(AEditor: TSynEdit);
    procedure SetIncrementalSearchStr(const AValue: string);
    procedure IncrementalSearch(ANext, ABackward: Boolean);
    procedure UpdatePageNames;
    procedure UpdateProjectFiles(ACurrentEditor: TSourceEditor = nil);

    property NoteBookPage[Index: Integer]: TTabSheet read GetNoteBookPage;
    procedure NoteBookInsertPage(Index: Integer; const S: string);
    procedure NoteBookDeletePage(APageIndex: Integer);
    procedure UpdateTabsAndPageTitle;
    procedure UpdateTabsAndPageTimeReached(Sender: TObject);
    procedure CallOnEditorPageCaptionUpdate(Sender: TObject);
  protected
    function NoteBookIndexOfPage(APage: TTabSheet): Integer;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DragCanceled; override;
    procedure DoActiveEditorChanged;
  protected
    States: TSourceNotebookStates;
    procedure Activate; override;
    procedure CreateNotebook;
    function NewSE(Pagenum: Integer; NewPagenum: Integer = -1;
                   ASharedEditor: TSourceEditor = nil;
                   ATabCaption: String = ''): TSourceEditor;
    procedure AcceptEditor(AnEditor: TSourceEditor; SendEvent: Boolean = False);
    procedure ReleaseEditor(AnEditor: TSourceEditor; SendEvent: Boolean = False);
    procedure EditorChanged(Sender: TObject; Changes: TSynStatusChanges);
    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure DoShow; override;
    procedure DoHide; override;
    function GetWindowID: Integer; override;
  protected
    function GetActiveCompletionPlugin: TSourceEditorCompletionPlugin; override;
    function GetCompletionPlugins(Index: integer): TSourceEditorCompletionPlugin; override;

    procedure EditorMouseMove(Sender: TObject; {%H-}Shift: TShiftstate;
                              {%H-}X,{%H-}Y: Integer);
    procedure EditorMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
                              {%H-}Shift: TShiftstate; {%H-}X,{%H-}Y: Integer);
    function EditorGetIndent(Sender: TObject; Editor: TObject;
             LogCaret, OldLogCaret: TPoint; FirstLinePos, LastLinePos: Integer;
             Reason: TSynEditorCommand;
             SetIndentProc: TSynBeautifierSetIndentProc): Boolean;
    procedure EditorKeyDown(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure EditorKeyUp(Sender: TObject; var {%H-}Key: Word; {%H-}Shift: TShiftState);
    procedure EditorMouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
         {%H-}WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);

    procedure NotebookMouseDown(Sender: TObject; Button: TMouseButton;
          {%H-}Shift: TShiftState; X,Y: Integer);
    procedure NotebookMouseUp(Sender: TObject; Button: TMouseButton;
          {%H-}Shift: TShiftState; X,Y: Integer);
    procedure NotebookDragDropEx(Sender, Source: TObject;
                                  OldIndex, NewIndex: Integer; CopyDrag: Boolean;
                                  var Done: Boolean);
    procedure NotebookDragOverEx(Sender, Source: TObject;
                                  OldIndex, NewIndex: Integer; CopyDrag: Boolean;
                                  var Accept: Boolean);
    procedure NotebookDragOver(Sender, Source: TObject;
                               {%H-}X,{%H-}Y: Integer; State: TDragState; var Accept: Boolean);
    procedure NotebookEndDrag(Sender, {%H-}Target: TObject; {%H-}X,{%H-}Y: Integer);

    procedure OnApplicationDeactivate(Sender: TObject);
    procedure ShowSynEditHint(const MousePos: TPoint);

    procedure NextEditor;
    procedure PrevEditor;
    procedure MoveEditorLeft(CurrentPageIndex: integer);
    procedure MoveEditorRight(CurrentPageIndex: integer);
    procedure MoveActivePageLeft;
    procedure MoveActivePageRight;
    procedure MoveEditorFirst(CurrentPageIndex: integer);
    procedure MoveEditorLast(CurrentPageIndex: integer);
    procedure MoveActivePageFirst;
    procedure MoveActivePageLast;
    procedure GotoNextWindow(Backward: Boolean = False);
    procedure GotoNextSharedEditor(Backward: Boolean = False);
    procedure MoveEditorNextWindow(Backward: Boolean = False; Copy: Boolean = False);
    procedure CopyEditor(OldPageIndex, NewWindowIndex, NewPageIndex: integer; Focus: Boolean = False);

    function GetActiveEditor: TSourceEditorInterface; override;
    procedure SetActiveEditor(const AValue: TSourceEditorInterface); override;
    function GetItems(Index: integer): TSourceEditorInterface; override;
    function GetEditors(Index:integer): TSourceEditor;

    property Manager: TSourceEditorManager read FManager;

    procedure BeginAutoFocusLock;
    procedure EndAutoFocusLock;
  protected
    procedure CloseTabClicked(Sender: TObject);
    procedure CloseClicked(Sender: TObject; CloseOptions: TCloseSrcEditorOptions = []);
    procedure ToggleFormUnitClicked(Sender: TObject);
    procedure ToggleObjectInspClicked(Sender: TObject);

    procedure IncUpdateLockInternal;
    procedure DecUpdateLockInternal;

    // editor page history
    procedure HistorySetMostRecent(APage: TTabSheet);
    procedure HistoryRemove(APage: TTabSheet);
    function  HistoryGetTopPageIndex: Integer;

    // incremental find
    procedure BeginIncrementalFind;
    procedure EndIncrementalFind;
    property IncrementalSearchStr: string
      read FIncrementalSearchStr write SetIncrementalSearchStr;

    procedure StartShowCodeContext(JumpToError: boolean);

    // paste and copy
    procedure CutClicked(Sender: TObject);
    procedure CopyClicked(Sender: TObject);
    procedure PasteClicked(Sender: TObject);

    procedure ReloadEditorOptions;
    procedure CheckFont;

  public
    procedure AddUpdateEditorPageCaptionHandler(AEvent: TNotifyEvent; const AsLast: Boolean = True); override;
    procedure RemoveUpdateEditorPageCaptionHandler(AEvent: TNotifyEvent); override;

    procedure ProcessParentCommand(Sender: TObject;
       var Command: TSynEditorCommand; var {%H-}AChar: TUTF8Char; {%H-}Data: pointer;
       var Handled: boolean);
    procedure ParentCommandProcessed(Sender: TObject;
       var Command: TSynEditorCommand; var {%H-}AChar: TUTF8Char; {%H-}Data: pointer;
       var Handled: boolean);
  public
    constructor Create(AOwner: TComponent); override; overload;
    constructor Create(AOwner: TComponent; AWindowID: Integer); overload;
    destructor Destroy; override;

    function EditorCount: integer;
    function IndexOfEditor(aEditor: TSourceEditorInterface): integer;
    function Count: integer; override;

    function SourceEditorIntfWithFilename(const Filename: string
      ): TSourceEditorInterface; override;
    function FindSourceEditorWithPageIndex(APageIndex:integer):TSourceEditor;
    function FindPageWithEditor(ASourceEditor: TSourceEditor):integer;
    function FindSourceEditorWithEditorComponent(EditorComp: TComponent): TSourceEditor;
    function GetActiveSE: TSourceEditor; { $note deprecate and use SetActiveEditor}
    procedure CheckCurrentCodeBufferChanged;
    function IndexOfEditorInShareWith(AnOtherEditor: TSourceEditorInterface): Integer; override;
    procedure MoveEditor(OldPageIndex, NewPageIndex: integer);
    procedure MoveEditor(OldPageIndex, NewWindowIndex, NewPageIndex: integer);

    procedure UpdateStatusBar;
    procedure ClearExecutionLines;
    procedure ClearExecutionMarks;

    // new, close, focus
    function NewFile(const NewShortName: String; ASource: TCodeBuffer;
                      FocusIt: boolean; AShareEditor: TSourceEditor = nil): TSourceEditor;
    procedure CloseFile(APageIndex:integer);
    procedure FocusEditor;
    function GetCapabilities: TCTabControlCapabilities;
    procedure IncUpdateLock; override;
    procedure DecUpdateLock; override;
  public
    property Editors[Index:integer]:TSourceEditor read GetEditors; // !!! not ordered for PageIndex
    // forwarders to the FNotebook
    property PageIndex: Integer read GetPageIndex write SetPageIndex;
    property PageCount: Integer read GetPageCount;
    property NotebookPages: TStrings read GetNotebookPages;
  end;

  { TBrowseEditorTabHistoryDialog }

  TBrowseEditorTabHistoryDialog = class(TForm)
  private
    FNotebook: TSourceNotebook;
    FEditorList: TListBox;

    procedure MoveInList(aForward: Boolean);
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoCreate; override;
  public
    procedure Show(aForward: Boolean); reintroduce;
  end;

  TSrcEditMangerHandlerType = (
    semhtCopyPaste
    );
  TSrcEditManagerUpdateFlag = (
    ufMgrActiveEditorChanged,
    ufShowWindowOnTop,
    ufShowWindowOnTopFocus);
  TSrcEditManagerUpdateFlags = set of TSrcEditManagerUpdateFlag;

  { TSourceEditorManagerBase }
  (* Implement all Methods with the Interface types *)

  TSourceEditorManagerBase = class(TSourceEditorManagerInterface)
  private
    FActiveWindow: TSourceNotebook;
    FSourceWindowList: TFPList;
    FSourceWindowByFocusList: TFPList;
    FUpdateLock: Integer;
    FActiveEditorLock: Integer;
    FAutoFocusLock: Integer;
    FUpdateFlags: TSrcEditManagerUpdateFlags;
    FShowTabs: Boolean;
    procedure FreeSourceWindows;
    function GetActiveSourceWindowIndex: integer;
    function GetSourceWindowByLastFocused(Index: Integer): TSourceEditorWindowInterface;
    procedure SetActiveSourceWindowIndex(const AValue: integer);
  protected
    fProducers: TFPList; // list of TSourceMarklingProducer
    FChangeNotifyLists: Array [TsemChangeReason] of TMethodList;
    FHandlers: array[TSrcEditMangerHandlerType] of TMethodList;
    FChangesQueuedForMsgWnd: TETMultiSrcChanges;// source editor changes waiting to be applied to the Messages window
    function  GetActiveSourceWindow: TSourceEditorWindowInterface; override;
    procedure SetActiveSourceWindow(const AValue: TSourceEditorWindowInterface); override;
    function  GetSourceWindows(Index: integer): TSourceEditorWindowInterface; override;
    procedure DoWindowFocused({%H-}AWindow: TSourceNotebook);  // Includes Focus to ChildControl (aka Activated)
    function  GetActiveEditor: TSourceEditorInterface; override;
    procedure SetActiveEditor(const AValue: TSourceEditorInterface); override;
    procedure DoActiveEditorChanged;
    procedure DoEditorStatusChanged(AEditor: TSourceEditor);
    function  GetSourceEditors(Index: integer): TSourceEditorInterface; override;
    function  GetUniqueSourceEditors(Index: integer): TSourceEditorInterface; override;
    function GetMarklingProducers(Index: integer): TSourceMarklingProducer; override;
    procedure SyncMessageWnd(Sender: TObject);
    procedure DoWindowShow(AWindow: TSourceNotebook);
    procedure DoWindowHide(AWindow: TSourceNotebook);
    function GetShowTabs: Boolean; override;
    procedure SetShowTabs(const AShowTabs: Boolean); override;
  public
    procedure BeginAutoFocusLock;
    procedure EndAutoFocusLock;
    function  HasAutoFocusLock: Boolean;
    // Windows
    function SourceWindowWithEditor(const AEditor: TSourceEditorInterface): TSourceEditorWindowInterface;
              override;
    function  SourceWindowCount: integer; override;
    function  IndexOfSourceWindow(AWindow: TSourceEditorWindowInterface): integer;
    property  ActiveSourceWindowIndex: integer
              read GetActiveSourceWindowIndex write SetActiveSourceWindowIndex;
    function  IndexOfSourceWindowByLastFocused(AWindow: TSourceEditorWindowInterface): integer;
    property  SourceWindowByLastFocused[Index: Integer]: TSourceEditorWindowInterface
              read GetSourceWindowByLastFocused;
    // Editors
    function  SourceEditorIntfWithFilename(const Filename: string): TSourceEditorInterface;
              override;
    function  SourceEditorCount: integer; override;
    function  UniqueSourceEditorCount: integer; override;
    // Settings
    function  GetEditorControlSettings(EditControl: TControl): boolean; override;
    function  GetHighlighterSettings(Highlighter: TObject): boolean; override;
  private
    // Completion Plugins
    FCompletionPlugins: TFPList;
    FDefaultCompletionForm: TSourceEditCompletion;
    FActiveCompletionPlugin: TSourceEditorCompletionPlugin;
    function  GetDefaultCompletionForm: TSourceEditCompletion;
    procedure FreeCompletionPlugins;
    function  GetScreenRectForToken(AnEditor: TCustomSynEdit; PhysColumn, PhysRow, EndColumn: Integer): TRect;
  protected
    CodeToolsToSrcEditTimer: TTimer;
    function  GetActiveCompletionPlugin: TSourceEditorCompletionPlugin; override;
    function  GetCompletionBoxPosition: integer; override;
    function  GetCompletionPlugins(Index: integer): TSourceEditorCompletionPlugin; override;
    function  GetDefaultSynCompletionForm: TCustomForm; override;
    function  GetSynCompletionLinesInWindow: integer; override;
    procedure SetSynCompletionLinesInWindow(LineCnt: integer); override;
    function FindIdentCompletionPlugin(SrcEdit: TSourceEditor; JumpToError: boolean;
                               var s: string; var BoxX, BoxY: integer;
                               var UseWordCompletion: boolean): boolean;
    property DefaultCompletionForm: TSourceEditCompletion
      read GetDefaultCompletionForm;
  public
    // Completion Plugins
    function  CompletionPluginCount: integer; override;
    procedure DeactivateCompletionForm; override;
    procedure RegisterCompletionPlugin(Plugin: TSourceEditorCompletionPlugin); override;
    procedure UnregisterCompletionPlugin(Plugin: TSourceEditorCompletionPlugin); override;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure IncUpdateLockInternal;
    procedure DecUpdateLockInternal;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterChangeEvent(AReason: TsemChangeReason; AHandler: TNotifyEvent); override;
    procedure UnRegisterChangeEvent(AReason: TsemChangeReason; AHandler: TNotifyEvent); override;
    procedure RegisterCopyPasteEvent(AHandler: TSemCopyPasteEvent); override;
    procedure UnRegisterCopyPasteEvent(AHandler: TSemCopyPasteEvent); override;
    // producers
    function MarklingProducerCount: integer; override;
    procedure RegisterMarklingProducer(aProducer: TSourceMarklingProducer); override;
    procedure UnregisterMarklingProducer(aProducer: TSourceMarklingProducer); override;
    procedure InvalidateMarklingsOfAllFiles(aProducer: TSourceMarklingProducer); override;
    procedure InvalidateMarklings(aProducer: TSourceMarklingProducer; aFilename: string); override;
  public
    procedure IncUpdateLock;
    procedure DecUpdateLock;
    procedure ShowActiveWindowOnTop(Focus: Boolean = False); override;
  private
    FMacroRecorder: TIdeEditorMacro;
    FOnCurrentCodeBufferChanged: TNotifyEvent;
    procedure DoMacroRecorderState(Sender: TObject);
  public
    // codetools
    property OnCurrentCodeBufferChanged: TNotifyEvent
             read FOnCurrentCodeBufferChanged write FOnCurrentCodeBufferChanged;
  end;

  TJumpToSectionType = (
    jmpInterface, jmpInterfaceUses,
    jmpImplementation, jmpImplementationUses,
    jmpInitialization);
  TJumpToProcedureType = (jmpHeader, jmpBegin);

  TSourceEditorHintWindowManager = class(TIDEHintWindowManager)
  private
    FManager: TSourceEditorManager;
    FAutoShown: Boolean;
    FAutoHintMousePos: TPoint;
    FAutoHintTimer: TIdleTimer;
    FAutoHideHintTimer: TTimer;
    FLastHint: string;
    FScreenRect: TRect;

    procedure HintTimer(Sender: TObject);
    procedure HideHintTimer(Sender: TObject);
  public
    procedure ActivateHint(const ScreenRect: TRect; const ABaseURL, AHint: string;
      AAutoShown: Boolean = True; AMouseOffset: Boolean = True); overload;
    procedure ActivateHint(const ScreenPos: TPoint; const ABaseURL, AHint: string;
      AAutoShown: Boolean = True; AMouseOffset: Boolean = True); overload;
    procedure HideAutoHintAfterMouseMoved;
    procedure HideAutoHint;
    procedure UpdateHintTimer;
  public
    constructor Create(AManager: TSourceEditorManager);
    destructor Destroy; override;
  public
    property AutoHintTimer: TIdleTimer read FAutoHintTimer;
  end;

  { TSourceEditorManager }
  (* Reintroduce all Methods with the final types *)

  TSourceEditorManager = class(TSourceEditorManagerBase)
  private
    procedure DoConfigureEditorToolbar(Sender: TObject);
    function GetActiveSourceNotebook: TSourceNotebook;
    function GetActiveSrcEditor: TSourceEditor;
    function GetSourceEditorsByPage(WindowIndex, PageIndex: integer): TSourceEditor;
    function GetSourceNbByLastFocused(Index: Integer): TSourceNotebook;
    function GetSrcEditors(Index: integer): TSourceEditor;
    procedure SetActiveSourceNotebook(const AValue: TSourceNotebook);
    function GetSourceNotebook(Index: integer): TSourceNotebook;
    procedure SetActiveSrcEditor(const AValue: TSourceEditor);
    procedure SrcEditMenuProcedureJumpGetCaption(Sender: TObject; var ACaption,
      {%H-}AHint: string);
  public
    // Windows
    function  SourceWindowWithEditor(const AEditor: TSourceEditorInterface): TSourceNotebook;
              reintroduce;
    property  SourceWindows[Index: integer]: TSourceNotebook read GetSourceNotebook; // reintroduce
    property  ActiveSourceWindow: TSourceNotebook
              read GetActiveSourceNotebook write SetActiveSourceNotebook;       // reintroduce
    function  ActiveOrNewSourceWindow: TSourceNotebook;
    function  NewSourceWindow: TSourceNotebook;
    procedure CreateSourceWindow(Sender: TObject; aFormName: string;
                          var AForm: TCustomForm; DoDisableAutoSizing: boolean);
    procedure GetDefaultLayout(Sender: TObject; aFormName: string;
             out aBounds: TRect; out DockSibling: string; out DockAlign: TAlign);
    function  SourceWindowWithPage(const APage: TTabSheet): TSourceNotebook;
    property  SourceWindowByLastFocused[Index: Integer]: TSourceNotebook
              read GetSourceNbByLastFocused;
    function  IndexOfSourceWindowWithID(const AnID: Integer): Integer; override;
    function  SourceWindowWithID(const AnID: Integer): TSourceNotebook;
    // Editors
    function  SourceEditorCount: integer; override;
    function  GetActiveSE: TSourceEditor;                                       { $note deprecate and use ActiveEditor}
    property  ActiveEditor: TSourceEditor read GetActiveSrcEditor  write SetActiveSrcEditor; // reintroduced
    property SourceEditors[Index: integer]: TSourceEditor read GetSrcEditors;   // reintroduced
    property  SourceEditorsByPage[WindowIndex, PageIndex: integer]: TSourceEditor
              read GetSourceEditorsByPage;
    procedure SetWindowByIDAndPage(AWindowID, APageIndex: integer);
    function  SourceEditorIntfWithFilename(const Filename: string): TSourceEditor; reintroduce;
    function FindSourceEditorWithEditorComponent(EditorComp: TComponent): TSourceEditor; // With SynEdit
  protected
    procedure NewEditorCreated(AEditor: TSourceEditor);
    procedure EditorRemoved(AEditor: TSourceEditor);
    procedure SendEditorCreated(AEditor: TSourceEditor);
    procedure SendEditorDestroyed(AEditor: TSourceEditor);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RemoveWindow(AWindow: TSourceNotebook);
  public
    // Forward to all windows
    procedure ClearErrorLines; override;
    procedure ClearExecutionLines;
    procedure ClearExecutionMarks;
    procedure FillExecutionMarks;
    procedure ReloadEditorOptions;
    function Beautify(const Src: string): string; override;
    // find / replace text
    procedure FindClicked(Sender: TObject);
    procedure FindNextClicked(Sender: TObject);
    procedure FindPreviousClicked(Sender: TObject);
    procedure ReplaceClicked(Sender: TObject);
    procedure IncrementalFindClicked(Sender: TObject);
    procedure GotoLineClicked(Sender: TObject);
    procedure JumpBackClicked(Sender: TObject);
    procedure JumpForwardClicked(Sender: TObject);
    procedure JumpToNextErrorClicked(Sender: TObject);
    procedure JumpToPrevErrorClicked(Sender: TObject);
    procedure AddJumpPointClicked(Sender: TObject);
    procedure AddCustomJumpPoint(ACaretXY: TPoint; ATopLine: integer;
                  AEditor: TSourceEditor; DeleteForwardHistory: boolean);
    procedure DeleteLastJumpPointClicked(Sender: TObject);
    procedure ViewJumpHistoryClicked(Sender: TObject);
  protected
    // Bookmarks
    procedure BookMarkToggleClicked(Sender: TObject);
    procedure BookMarkGotoClicked(Sender: TObject);
  public
    procedure BookMarkNextClicked(Sender: TObject);
    procedure BookMarkPrevClicked(Sender: TObject);
    procedure JumpToPos(FileName: string; Pos: TCodeXYPosition; TopLine: Integer);
    procedure JumpToPos(FileName: string; Pos: TCodeXYPosition; TopLine, BlockTopLine, BlockBottomLine: Integer);
    procedure JumpToSection(JumpType: TJumpToSectionType);
    procedure JumpToInterfaceClicked(Sender: TObject);
    procedure JumpToInterfaceUsesClicked(Sender: TObject);
    procedure JumpToImplementationClicked(Sender: TObject);
    procedure JumpToImplementationUsesClicked(Sender: TObject);
    procedure JumpToInitializationClicked(Sender: TObject);
    procedure JumpToProcedure(const JumpType: TJumpToProcedureType);
    procedure JumpToProcedureHeaderClicked(Sender: TObject);
    procedure JumpToProcedureBeginClicked(Sender: TObject);
  protected
    // macros
    function MacroFuncCol(const {%H-}s:string; const {%H-}Data: PtrInt;
                          var {%H-}Abort: boolean): string;
    function MacroFuncRow(const {%H-}s:string; const {%H-}Data: PtrInt;
                          var {%H-}Abort: boolean): string;
    function MacroFuncEdFile(const {%H-}s:string; const {%H-}Data: PtrInt;
                             var {%H-}Abort: boolean): string;
    function MacroFuncCurToken(const {%H-}s:string; const {%H-}Data: PtrInt;
                               var {%H-}Abort: boolean): string;
    function MacroFuncConfirm(const s:string; const {%H-}Data: PtrInt;
                               var Abort: boolean): string;
    function MacroFuncPrompt(const s:string; const {%H-}Data: PtrInt;
                             var Abort: boolean): string;
    function MacroFuncSave(const {%H-}s:string; const {%H-}Data: PtrInt;
                           var Abort: boolean): string;
    function MacroFuncSaveAll(const {%H-}s:string; const {%H-}Data: PtrInt;
                              var Abort: boolean): string;
  public
    procedure InitMacros(AMacroList: TTransferMacroList);
    procedure SetupShortCuts;

    function FindUniquePageName(FileName:string; IgnoreEditor: TSourceEditor):string;
    function SomethingModified(Verbose: boolean = false): boolean;
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure OnUserInput(Sender: TObject; Msg: Cardinal);
    procedure LockAllEditorsInSourceChangeCache;
    procedure UnlockAllEditorsInSourceChangeCache;
    procedure BeginGlobalUpdate;
    procedure EndGlobalUpdate;
    procedure CloseFile(AEditor: TSourceEditorInterface);
    procedure HideHint;
    // history jumping
    procedure HistoryJump(Sender: TObject; JumpAction: TJumpHistoryAction);
  private
    // Hints
    FHints: TSourceEditorHintWindowManager;
    procedure ActivateHint(const ScreenRect: TRect; const BaseURL, TheHint: string;
      AutoShown: Boolean = True; AMouseOffset: Boolean = True); overload;
    procedure ActivateHint(const ScreenPos: TPoint; const BaseURL, TheHint: string;
      AutoShown: Boolean = True; AMouseOffset: Boolean = True); overload;
  private
    FCodeTemplateModul: TSynEditAutoComplete;
    FGotoDialog: TfrmGoto;
    procedure OnCodeTemplateTokenNotFound(Sender: TObject; AToken: string;
                                   AnEditor: TCustomSynEdit; var Index:integer);
    procedure OnCodeTemplateExecuteCompletion(
                                       ASynAutoComplete: TCustomSynAutoComplete;
                                       Index: integer);
  protected
    procedure CodeToolsToSrcEditTimerTimer(Sender: TObject);
    procedure OnSourceCompletionTimer(Sender: TObject);
    // marks
    procedure OnSourceMarksAction(AMark: TSourceMark; {%H-}AAction: TMarksAction);
    procedure OnSourceMarksGetSynEdit(Sender: TObject; aFilename: string;
      var aSynEdit: TSynEdit);
    // goto dialog
    function GotoDialog: TfrmGoto;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateNewWindow(Activate: Boolean= False;
                             DoDisableAutoSizing: boolean = False;
                             AnID: Integer = -1
                            ): TSourceNotebook;
    function SenderToEditor(Sender: TObject): TSourceEditor;
    property CodeTemplateModul: TSynEditAutoComplete
                               read FCodeTemplateModul write FCodeTemplateModul;
  private
    // Context-Menu
    procedure CloseOtherPagesClicked(Sender: TObject);
    procedure CloseOtherPagesClickedAsync(Sender: PtrInt);
    procedure CloseRightPagesClicked(Sender: TObject);
    procedure CloseRightPagesClickedAsync(Sender: PtrInt);
    procedure ReadOnlyClicked(Sender: TObject);
    procedure ToggleLineNumbersClicked(Sender: TObject);
    procedure ToggleI18NForLFMClicked(Sender: TObject);
    procedure ShowUnitInfo(Sender: TObject);
    procedure CopyFilenameClicked(Sender: TObject);
    procedure EditorPropertiesClicked(Sender: TObject);
  private
    FOnAddJumpPoint: TOnAddJumpPoint;
    FOnClearBookmark: TPlaceBookMarkEvent;
    FOnClearBookmarkId: TPlaceBookMarkIdEvent;
    FOnClickLink: TMouseEvent;
    FOnCloseClicked: TOnCloseSrcEditor;
    FOnDeleteLastJumpPoint: TNotifyEvent;
    FOnUpdateProjectFile: TUpdateProjectFileEvent;
    FOnFindDeclarationClicked: TNotifyEvent;
    FOnGetIndent: TOnGetIndentEvent;
    FOnGotoBookmark: TBookMarkActionEvent;
    FOnInitIdentCompletion: TOnInitIdentCompletion;
    FOnJumpToHistoryPoint: TOnJumpToHistoryPoint;
    FOnMouseLink: TSynMouseLinkEvent;
    FOnNoteBookCloseQuery: TCloseEvent;
    FOnOpenFileAtCursorClicked: TNotifyEvent;
    FOnPackageForSourceEditor: TPackageForSourceEditorEvent;
    FOnPlaceMark: TPlaceBookMarkEvent;
    FOnPopupMenu: TSrcEditPopupMenuEvent;
    FOnProcessUserCommand: TOnProcessUserCommand;
    fOnReadOnlyChanged: TNotifyEvent;
    FOnSetBookmark: TBookMarkActionEvent;
    FOnShowCodeContext: TOnShowCodeContext;
    FOnShowHintForSource: TOnShowHintForSource;
    FOnShowUnitInfo: TNotifyEvent;
    FOnToggleFormUnitClicked: TNotifyEvent;
    FOnToggleObjectInspClicked: TNotifyEvent;
    FOnUserCommandProcessed: TOnUserCommandProcessed;
    FOnViewJumpHistory: TNotifyEvent;
  public
    property OnAddJumpPoint: TOnAddJumpPoint
             read FOnAddJumpPoint write FOnAddJumpPoint;
    property OnCloseClicked: TOnCloseSrcEditor
             read FOnCloseClicked write FOnCloseClicked;
    property OnClickLink: TMouseEvent read FOnClickLink write FOnClickLink;
    property OnMouseLink: TSynMouseLinkEvent read FOnMouseLink write FOnMouseLink;
    property OnGetIndent: TOnGetIndentEvent
             read FOnGetIndent write FOnGetIndent;
    property OnDeleteLastJumpPoint: TNotifyEvent
             read FOnDeleteLastJumpPoint write FOnDeleteLastJumpPoint;
    property OnUpdateProjectFile: TUpdateProjectFileEvent
             read FOnUpdateProjectFile write FOnUpdateProjectFile;
    property OnFindDeclarationClicked: TNotifyEvent
             read FOnFindDeclarationClicked write FOnFindDeclarationClicked;
    property OnInitIdentCompletion: TOnInitIdentCompletion
             read FOnInitIdentCompletion write FOnInitIdentCompletion;
    property OnShowCodeContext: TOnShowCodeContext
             read FOnShowCodeContext write FOnShowCodeContext;
    property OnJumpToHistoryPoint: TOnJumpToHistoryPoint
             read FOnJumpToHistoryPoint write FOnJumpToHistoryPoint;
    property OnPlaceBookmark: TPlaceBookMarkEvent  // Bookmark was placed by SynEdit
             read FOnPlaceMark write FOnPlaceMark;
    property OnClearBookmark: TPlaceBookMarkEvent  // Bookmark was cleared by SynEdit
             read FOnClearBookmark write FOnClearBookmark;
    property OnClearBookmarkId: TPlaceBookMarkIdEvent
             read FOnClearBookmarkId write FOnClearBookmarkId;
    property OnSetBookmark: TBookMarkActionEvent  // request to set a Bookmark
             read FOnSetBookmark write FOnSetBookmark;
    property OnGotoBookmark: TBookMarkActionEvent  // request to go to a Bookmark
             read FOnGotoBookmark write FOnGotoBookmark;
    property OnOpenFileAtCursorClicked: TNotifyEvent
             read FOnOpenFileAtCursorClicked write FOnOpenFileAtCursorClicked;
    property OnProcessUserCommand: TOnProcessUserCommand
             read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnUserCommandProcessed: TOnUserCommandProcessed
             read FOnUserCommandProcessed write FOnUserCommandProcessed;
    property OnReadOnlyChanged: TNotifyEvent
             read fOnReadOnlyChanged write fOnReadOnlyChanged;
    property OnShowHintForSource: TOnShowHintForSource
             read FOnShowHintForSource write FOnShowHintForSource;
    property OnShowUnitInfo: TNotifyEvent
             read FOnShowUnitInfo write FOnShowUnitInfo;
    property OnToggleFormUnitClicked: TNotifyEvent
             read FOnToggleFormUnitClicked write FOnToggleFormUnitClicked;
    property OnToggleObjectInspClicked: TNotifyEvent
             read FOnToggleObjectInspClicked write FOnToggleObjectInspClicked;
    property OnViewJumpHistory: TNotifyEvent
             read FOnViewJumpHistory write FOnViewJumpHistory;
    property OnPopupMenu: TSrcEditPopupMenuEvent read FOnPopupMenu write FOnPopupMenu;
    property OnNoteBookCloseQuery: TCloseEvent
             read FOnNoteBookCloseQuery write FOnNoteBookCloseQuery;
    property OnPackageForSourceEditor: TPackageForSourceEditorEvent
             read FOnPackageForSourceEditor write FOnPackageForSourceEditor;
  end;

  TSourceEditorWordCompletion = class(TWordCompletion)
  private type
    TSourceListItem = class
      Source: TStrings;
      IgnoreWordPos: TPoint;
    end;
  private
    FIncludeWords: TIdentComplIncludeWords;
    FSourceList: TObjectList;
    procedure ReloadSourceList;
  protected
    procedure DoGetSource(var Source: TStrings; var {%H-}TopLine,
      {%H-}BottomLine: Integer; var IgnoreWordPos: TPoint; SourceIndex: integer); override;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property IncludeWords: TIdentComplIncludeWords read FIncludeWords write FIncludeWords;
  end;

function SourceEditorManager: TSourceEditorManager; inline;


  //=============================================================================

const
  SourceTabMenuRootName = 'SourceTab';
  SourceEditorMenuRootName = 'SourceEditor';

var
  // Clipboard
  SrcEditMenuCut: TIDEMenuCommand;
  SrcEditMenuCopy: TIDEMenuCommand;
  SrcEditMenuPaste: TIDEMenuCommand;
  SrcEditMenuMultiPaste: TIDEMenuCommand;
  SrcEditMenuCopyFilename: TIDEMenuCommand;
  SrcEditMenuFindDeclaration: TIDEMenuCommand;
  SrcEditMenuSelectAll: TIDEMenuCommand;
    // finding / jumping
    SrcEditMenuProcedureJump: TIDEMenuCommand;
    SrcEditMenuFindNextWordOccurrence: TIDEMenuCommand;
    SrcEditMenuFindPrevWordOccurrence: TIDEMenuCommand;
    SrcEditMenuFindinFiles: TIDEMenuCommand;
    SrcEditMenuFindIdentifierReferences: TIDEMenuCommand;
    SrcEditMenuFindUsedUnitReferences: TIDEMenuCommand;
    // open file
    SrcEditMenuOpenFileAtCursor: TIDEMenuCommand;
  SrcEditMenuClosePage: TIDEMenuCommand;
  SrcEditMenuCloseOtherPages: TIDEMenuCommand;
  SrcEditMenuCloseOtherPagesToRight: TIDEMenuCommand;
    // bookmarks
    SrcEditMenuNextBookmark: TIDEMenuCommand;
    SrcEditMenuPrevBookmark: TIDEMenuCommand;
    SrcEditMenuSetFreeBookmark: TIDEMenuCommand;
    SrcEditMenuClearFileBookmark: TIDEMenuCommand;
    SrcEditMenuClearAllBookmark: TIDEMenuCommand;
    SrcEditMenuGotoBookmark: array [TBookmarkNumRange] of TIDEMenuCommand;
    SrcEditMenuToggleBookmark: array [TBookmarkNumRange] of TIDEMenuCommand;
    // debugging
    SrcEditMenuToggleBreakpoint: TIDEMenuCommand;
    SrcEditMenuRunToCursor: TIDEMenuCommand;
    SrcEditMenuEvaluateModify: TIDEMenuCommand;
    SrcEditMenuAddWatchAtCursor: TIDEMenuCommand;
    SrcEditMenuAddWatchPointAtCursor: TIDEMenuCommand;
    SrcEditMenuInspect: TIDEMenuCommand;
    SrcEditMenuViewCallStack: TIDEMenuCommand;
    // source
    SrcEditMenuEncloseSelection: TIDEMenuCommand;
    SrcEditMenuEncloseInIFDEF: TIDEMenuCommand;
    SrcEditMenuCompleteCode: TIDEMenuCommand;
    SrcEditMenuUseUnit: TIDEMenuCommand;
    SrcEditMenuShowUnitInfo: TIDEMenuCommand;
    // refactoring
    SrcEditMenuRenameIdentifier: TIDEMenuCommand;
    SrcEditMenuExtractProc: TIDEMenuCommand;
    SrcEditMenuInvertAssignment: TIDEMenuCommand;
    SrcEditMenuShowAbstractMethods: TIDEMenuCommand;
    SrcEditMenuShowEmptyMethods: TIDEMenuCommand;
    SrcEditMenuShowUnusedUnits: TIDEMenuCommand;
    SrcEditMenuFindOverloads: TIDEMenuCommand;
    SrcEditMenuMakeResourceString: TIDEMenuCommand;
  SrcEditMenuMoveEditorLeft: TIDEMenuCommand;
  SrcEditMenuMoveEditorRight: TIDEMenuCommand;
  SrcEditMenuMoveEditorFirst: TIDEMenuCommand;
  SrcEditMenuMoveEditorLast: TIDEMenuCommand;
  SrcEditMenuReadOnly: TIDEMenuCommand;
  SrcEditMenuShowLineNumbers: TIDEMenuCommand;
  SrcEditMenuDisableI18NForLFM: TIDEMenuCommand;
  SrcEditMenuEditorProperties: TIDEMenuCommand;
  {$IFnDEF SingleSrcWindow}
  // Multi Window
  SrcEditMenuMoveToNewWindow: TIDEMenuCommand;
  SrcEditMenuMoveToOtherWindow: TIDEMenuSection;
  SrcEditMenuMoveToOtherWindowNew: TIDEMenuCommand;
  SrcEditMenuMoveToOtherWindowList: TIDEMenuSection;
  SrcEditMenuCopyToNewWindow: TIDEMenuCommand;
  SrcEditMenuCopyToOtherWindow: TIDEMenuSection;
  SrcEditMenuCopyToOtherWindowNew: TIDEMenuCommand;
  SrcEditMenuCopyToOtherWindowList: TIDEMenuSection;
  SrcEditMenuFindInOtherWindow: TIDEMenuSection;
  SrcEditMenuFindInOtherWindowList: TIDEMenuSection;
  // EditorLocks
  SrcEditMenuEditorLock: TIDEMenuCommand;
  {$ENDIF}


procedure RegisterStandardSourceTabMenuItems;
procedure RegisterStandardSourceEditorMenuItems;
function dbgSourceNoteBook(snb: TSourceNotebook): string;

var
  Highlighters: array[TLazSyntaxHighlighter] of TSynCustomHighlighter;

function CompareSrcEditIntfWithFilename(SrcEdit1, SrcEdit2: Pointer): integer;
function CompareFilenameWithSrcEditIntf(FilenameStr, SrcEdit: Pointer): integer;

var
  EnglishGPLNotice: string;
  EnglishLGPLNotice: string;
  EnglishModifiedLGPLNotice: string;
  EnglishMITNotice: string;


implementation

{$R *.lfm}
{$R ../images/bookmark.res}

var
  AWordCompletion: TWordCompletion = nil;

var
  SRCED_LOCK, SRCED_OPEN, SRCED_CLOSE, SRCED_PAGES: PLazLoggerLogGroup;

const
  (* SoftCenter are the visible Lines in the Editor where the caret can be located,
     without CenterCursor adjusting the topline.
     SoftCenter is defined by the amount of lines on the top/bottom of the editor,
     which are *not* part of it.
  *)
  SoftCenterFactor  = 5;  // One fifth of the "LinesInWindow"on each side (top/bottom)
  SoftCenterMinimum = 1;
  SoftCenterMaximum = 8;

var
  AutoStartCompletionBoxTimer: TIdleTimer = nil;
  SourceCompletionCaretXY: TPoint;
  PasBeautifier: TSynBeautifierPascal;

function dbgs(AFlag: TSourceNotebookUpdateFlag): string; overload;
begin
  WriteStr(Result, AFlag);
end;

function dbgs(AFlags: TSourceNotebookUpdateFlags): string; overload;
var
  i: TSourceNotebookUpdateFlag;
begin
  Result := '';
  for i := low(TSourceNotebookUpdateFlags) to high(TSourceNotebookUpdateFlags) do
    if i in AFlags then begin
      if Result <> '' then Result := Result + ',';
      Result := Result + dbgs(i);
    end;
  Result := '['+ Result + ']';
end;

function SourceEditorManager: TSourceEditorManager;
begin
  Result := TSourceEditorManager(SourceEditorManagerIntf);
end;

procedure ExecuteIdeMenuClick(Sender: TObject);
var
  ActEdit: TSourceEditor;
  r: Boolean;
begin
  if SourceEditorManager = nil then exit;
  if not (Sender is TIDESpecialCommand) then exit;
  if TIDESpecialCommand(Sender).Command = nil then exit;
  ActEdit := SourceEditorManager.ActiveEditor;
  if ActEdit = nil then
  begin
    TIDESpecialCommand(Sender).Command.Execute(Sender);
    exit;
  end;
  r := TIDESpecialCommand(Sender).Command.OnExecuteProc = @ExecuteIdeMenuClick;
  if r then
    TIDESpecialCommand(Sender).Command.OnExecuteProc := nil;
  // Commands may not work without focusing when anchordocking is installed
  ActEdit.FocusEditor;
  ActEdit.DoEditorExecuteCommand(TIDESpecialCommand(Sender).Command.Command);
  if r then
    TIDESpecialCommand(Sender).Command.OnExecuteProc := @ExecuteIdeMenuClick;
end;

procedure RegisterStandardSourceTabMenuItems;
var
  AParent: TIDEMenuSection;
begin
  SourceTabMenuRoot:=RegisterIDEMenuRoot(SourceTabMenuRootName);

  {%region *** Pages section ***}
  SrcEditMenuSectionPages:=RegisterIDEMenuSection(SourceTabMenuRoot, 'Pages');
  AParent:=SrcEditMenuSectionPages;

    SrcEditMenuClosePage := RegisterIDEMenuCommand(AParent,
        'Close Page', uemClosePage, nil, @ExecuteIdeMenuClick, nil, 'menu_close');
    SrcEditMenuCloseOtherPages := RegisterIDEMenuCommand(AParent,
        'Close All Other Pages',uemCloseOtherPages, nil, @ExecuteIdeMenuClick);
    SrcEditMenuCloseOtherPagesToRight := RegisterIDEMenuCommand(AParent,
        'Close Pages To the Right',uemCloseOtherPagesRight, nil, @ExecuteIdeMenuClick);

    {$IFnDEF SingleSrcWindow}
    // Lock Editor
    SrcEditMenuEditorLock := RegisterIDEMenuCommand(AParent,
        'LockEditor', uemLockPage, nil, @ExecuteIdeMenuClick);
    SrcEditMenuEditorLock.ShowAlwaysCheckable := True;
    // Move to other Window
    SrcEditMenuMoveToNewWindow := RegisterIDEMenuCommand(AParent,
        'MoveToNewWindow', uemMoveToNewWindow, nil, @ExecuteIdeMenuClick);

    {%region * Move To Other *}
      SrcEditMenuMoveToOtherWindow := RegisterIDESubMenu(AParent,
          'MoveToOtherWindow', uemMoveToOtherWindow);
      SrcEditMenuMoveToOtherWindowNew := RegisterIDEMenuCommand(SrcEditMenuMoveToOtherWindow,
          'MoveToOtherWindowNew', uemMoveToOtherWindowNew, nil, @ExecuteIdeMenuClick);
      // Section for dynamically created targets
      SrcEditMenuMoveToOtherWindowList := RegisterIDEMenuSection(SrcEditMenuMoveToOtherWindow,
          'MoveToOtherWindowList Section');
    {%endregion}

    SrcEditMenuCopyToNewWindow := RegisterIDEMenuCommand(AParent,
        'CopyToNewWindow', uemCopyToNewWindow, nil, @ExecuteIdeMenuClick);

    {%region * Copy To Other *}
      SrcEditMenuCopyToOtherWindow := RegisterIDESubMenu(AParent,
          'CopyToOtherWindow', uemCopyToOtherWindow);
      SrcEditMenuCopyToOtherWindowNew := RegisterIDEMenuCommand(SrcEditMenuCopyToOtherWindow,
          'CopyToOtherWindowNew', uemCopyToOtherWindowNew, nil, @ExecuteIdeMenuClick);
      // Section for dynamically created targets
      SrcEditMenuCopyToOtherWindowList := RegisterIDEMenuSection(SrcEditMenuCopyToOtherWindow,
          'CopyToOtherWindowList Section');
    {%endregion}

    SrcEditMenuFindInOtherWindow := RegisterIDESubMenu(AParent,
      'FindInOtherWindow', uemFindInOtherWindow);
    // Section for dynamically created targets
    SrcEditMenuFindInOtherWindowList := RegisterIDEMenuSection(SrcEditMenuFindInOtherWindow,
        'FindInOtherWindowList Section');
    {$ENDIF}

    {%region * Move Page (left/right) *}
      SrcEditSubMenuMovePage:=RegisterIDESubMenu(AParent, 'Move Page', lisMovePage);
      AParent:=SrcEditSubMenuMovePage;

      SrcEditMenuMoveEditorLeft := RegisterIDEMenuCommand(AParent,
          'MoveEditorLeft', uemMovePageLeft, nil, @ExecuteIdeMenuClick);
      SrcEditMenuMoveEditorRight := RegisterIDEMenuCommand(AParent,
          'MoveEditorRight', uemMovePageRight, nil, @ExecuteIdeMenuClick);
      SrcEditMenuMoveEditorFirst := RegisterIDEMenuCommand(AParent,
          'MoveEditorLeftmost', uemMovePageLeftmost, nil, @ExecuteIdeMenuClick);
      SrcEditMenuMoveEditorLast := RegisterIDEMenuCommand(AParent,
          'MoveEditorRightmost', uemMovePageRightmost, nil, @ExecuteIdeMenuClick);
    {%endregion}
  {%endregion}

  {%region *** Editors section ***}
  SrcEditMenuSectionEditors:=RegisterIDEMenuSection(SourceTabMenuRoot, 'Editors');
  {%endregion}
end;

procedure RegisterStandardSourceEditorMenuItems;
var
  AParent: TIDEMenuSection;
  I: Integer;
begin
  SourceEditorMenuRoot:=RegisterIDEMenuRoot(SourceEditorMenuRootName);
  AParent:=SourceEditorMenuRoot;

  // register the first dynamic section for often used context sensitive stuff
  SrcEditMenuSectionFirstDynamic:=RegisterIDEMenuSection(SourceEditorMenuRoot, 'First dynamic section');

  // Felipe: Please keep "Find Declaration" as the first item
  {%region *** first static section *** }
    SrcEditMenuSectionFirstStatic:=RegisterIDEMenuSection(SourceEditorMenuRoot, 'First static section');
    AParent:=SrcEditMenuSectionFirstStatic;

    SrcEditMenuFindDeclaration := RegisterIDEMenuCommand
        (AParent, 'Find Declaration', uemFindDeclaration, nil, @ExecuteIdeMenuClick);

    {%region *** Submenu: Find Section *** }
      SrcEditSubMenuFind := RegisterIDESubMenu(AParent,
          'Find section', lisMenuFind, nil, nil, 'menu_search_find');
      AParent:=SrcEditSubMenuFind;

      SrcEditMenuProcedureJump := RegisterIDEMenuCommand(AParent,
          'Procedure Jump', uemProcedureJump, nil,
          @ExecuteIdeMenuClick);
      SrcEditMenuFindNextWordOccurrence := RegisterIDEMenuCommand(AParent,
          'Find next word occurrence', srkmecFindNextWordOccurrence, nil,
          @ExecuteIdeMenuClick, nil, 'next_word');
      SrcEditMenuFindPrevWordOccurrence := RegisterIDEMenuCommand(AParent,
          'Find previous word occurrence', srkmecFindPrevWordOccurrence, nil,
          @ExecuteIdeMenuClick, nil, 'previous_word');
      SrcEditMenuFindInFiles := RegisterIDEMenuCommand(AParent,
          'Find in files', srkmecFindInFiles + ' ...', nil,
          @ExecuteIdeMenuClick, nil, 'menu_search_files');
      SrcEditMenuFindIdentifierReferences := RegisterIDEMenuCommand(AParent,
          'FindIdentifierReferences',lisMenuFindIdentifierRefs, nil,
          @ExecuteIdeMenuClick);
      SrcEditMenuFindUsedUnitReferences := RegisterIDEMenuCommand(AParent,
          'FindUsedUnitReferences', lisMenuFindReferencesOfUsedUnit, nil,
          @ExecuteIdeMenuClick);
    {%endregion}
  {%endregion}

  {%region *** Clipboard section ***}
    SrcEditMenuSectionClipboard:=RegisterIDEMenuSection(SourceEditorMenuRoot, 'Clipboard');
    AParent:=SrcEditMenuSectionClipboard;

    SrcEditMenuCut:=RegisterIDEMenuCommand(AParent,'Cut',lisCut, nil, nil, nil, 'laz_cut');
    SrcEditMenuCopy:=RegisterIDEMenuCommand(AParent,'Copy',lisCopy, nil, nil, nil, 'laz_copy');
    SrcEditMenuPaste:=RegisterIDEMenuCommand(AParent,'Paste',lisPaste, nil, nil, nil, 'laz_paste');
    SrcEditMenuMultiPaste:=RegisterIDEMenuCommand(AParent,'MultiPaste',lisMenuMultiPaste);
    SrcEditMenuSelectAll:=RegisterIDEMenuCommand(AParent,'SelectAll',lisMenuSelectAll);
    SrcEditMenuCopyFilename:=RegisterIDEMenuCommand(AParent,'Copy filename', uemCopyFilename);
  {%endregion}

  {%region *** Files section ***}
    SrcEditMenuSectionFiles:=RegisterIDEMenuSection(SourceEditorMenuRoot, 'Files');

    {%region * sub menu Open File *}
      SrcEditSubMenuOpenFile:=RegisterIDESubMenu(SrcEditMenuSectionFiles,
          'Open File',lisOpenFile, nil, nil, 'laz_open');
      AParent:=SrcEditSubMenuOpenFile;

      SrcEditMenuOpenFileAtCursor:=RegisterIDEMenuCommand(AParent, 'Open File At Cursor',
          uemOpenFileAtCursor, nil, @ExecuteIdeMenuClick, nil, 'menu_search_openfile_atcursor');
      // register the File Specific dynamic section
      SrcEditMenuSectionFileDynamic:=RegisterIDEMenuSection(AParent, 'File dynamic section');
    {%endregion}

    {%region * sub menu Flags section *}
      SrcEditSubMenuFlags:=RegisterIDESubMenu(SrcEditMenuSectionFiles,'Flags section',lisFileSettings);
      AParent:=SrcEditSubMenuFlags;

      SrcEditMenuReadOnly := RegisterIDEMenuCommand(AParent,'ReadOnly',uemReadOnly);
      SrcEditMenuReadOnly.ShowAlwaysCheckable:=true;
      SrcEditMenuShowLineNumbers := RegisterIDEMenuCommand(AParent, 'ShowLineNumbers',uemShowLineNumbers);
      SrcEditMenuShowLineNumbers.ShowAlwaysCheckable:=true;
      SrcEditMenuDisableI18NForLFM := RegisterIDEMenuCommand(AParent, 'DisableI18NForLFM',lisDisableI18NForLFM);
      SrcEditSubMenuHighlighter := RegisterIDESubMenu(AParent,'Highlighter', uemHighlighter);
      SrcEditSubMenuEncoding := RegisterIDESubMenu(AParent,'Encoding', uemEncoding);
      SrcEditSubMenuLineEnding := RegisterIDESubMenu(AParent,'LineEnding', uemLineEnding);
    {%endregion}
  {%endregion}

  {%region *** Goto Marks section ***}
  SrcEditMenuSectionMarks:=RegisterIDEMenuSection(SourceEditorMenuRoot, 'Marks section');
    // register the Goto Bookmarks Submenu
    SrcEditSubMenuGotoBookmarks:=RegisterIDESubMenu(SrcEditMenuSectionMarks,
                                   'Goto bookmarks submenu', uemGotoBookmark, nil, nil, 'menu_goto_bookmarks');
    AParent:=SrcEditSubMenuGotoBookmarks;
      for I in TBookmarkNumRange do
        SrcEditMenuGotoBookmark[I]:=RegisterIDEMenuCommand(AParent,
            'GotoBookmark'+IntToStr(I), Format(uemBookmarkNUnSet, [IntToStr(I)]),
            nil, @ExecuteIdeMenuClick, nil,
            'menu_goto_bookmark'+IntToStr(I));

      AParent:=RegisterIDEMenuSection(AParent, 'Next/Prev Bookmark section');
      SrcEditMenuNextBookmark:=RegisterIDEMenuCommand(AParent,
          'Goto next Bookmark',uemNextBookmark, nil,
          @ExecuteIdeMenuClick, nil, 'menu_search_next_bookmark');
      SrcEditMenuPrevBookmark:=RegisterIDEMenuCommand(AParent,
          'Goto previous Bookmark',uemPrevBookmark, nil,
          @ExecuteIdeMenuClick, nil, 'menu_search_previous_bookmark');
  {%endregion}

  {%region *** Toggle Bookmarks Submenu ***}
    SrcEditSubMenuToggleBookmarks:=RegisterIDESubMenu(SrcEditMenuSectionMarks,
                                     'Toggle bookmarks submenu', uemToggleBookmark, nil, nil, 'menu_toggle_bookmarks');
    AParent:=SrcEditSubMenuToggleBookmarks;
      for I in TBookmarkNumRange do
        SrcEditMenuToggleBookmark[I]:=RegisterIDEMenuCommand(AParent,
            'ToggleBookmark'+IntToStr(I), Format(uemToggleBookmarkNUnset, [IntToStr(I)]),
            nil, @ExecuteIdeMenuClick, nil,
            'menu_toggle_bookmark'+IntToStr(I));

      AParent:=RegisterIDEMenuSection(AParent, 'Set Free Bookmark section');
      SrcEditMenuSetFreeBookmark:=RegisterIDEMenuCommand(AParent,
          'Set a free Bookmark',uemSetFreeBookmark, nil, @ExecuteIdeMenuClick, nil, 'menu_set_free_bookmark');

      AParent:=RegisterIDEMenuSection(AParent, 'Clear Bookmarks section');
      SrcEditMenuClearFileBookmark:=RegisterIDEMenuCommand(AParent,
          'Clear Bookmark for current file',srkmecClearBookmarkForFile, nil, @ExecuteIdeMenuClick, nil, 'menu_clear_file_bookmarks');
      SrcEditMenuClearAllBookmark:=RegisterIDEMenuCommand(AParent,
          'Clear all Bookmark',srkmecClearAllBookmark, nil, @ExecuteIdeMenuClick, nil, 'menu_clear_all_bookmarks');
  {%endregion}

  {%region *** Debug Section ***}
    // Commands will be assigned by DebugManager
    SrcEditMenuSectionDebug:=RegisterIDEMenuSection(SourceEditorMenuRoot, 'Debug section');
    // register the Debug submenu
    SrcEditSubMenuDebug:=RegisterIDESubMenu(SrcEditMenuSectionDebug,
                                            'Debug', uemDebugWord, nil, nil, 'debugger');
    AParent:=SrcEditSubMenuDebug;

      // register the Debug submenu items
      SrcEditMenuToggleBreakpoint:=RegisterIDEMenuCommand(AParent,
          'Toggle Breakpoint', uemToggleBreakpoint, nil, @ExecuteIdeMenuClick);
      SrcEditMenuEvaluateModify:=RegisterIDEMenuCommand(AParent,
          'Evaluate/Modify...', uemEvaluateModify, nil, nil, nil, 'debugger_modify');
      SrcEditMenuEvaluateModify.Enabled:=False;
      SrcEditMenuAddWatchAtCursor:=RegisterIDEMenuCommand(AParent,
          'Add Watch at Cursor', uemAddWatchAtCursor);
      SrcEditMenuAddWatchPointAtCursor:=RegisterIDEMenuCommand(AParent,
          'Add Watch at Cursor', uemAddWatchPointAtCursor);
      SrcEditMenuInspect:=RegisterIDEMenuCommand(AParent,
          'Inspect...', uemInspect, nil, nil, nil, 'debugger_inspect');
      SrcEditMenuInspect.Enabled:=False;
      SrcEditMenuRunToCursor:=RegisterIDEMenuCommand(AParent,
          'Run to cursor', lisMenuRunToCursor, nil, nil, nil, 'menu_run_cursor');
      SrcEditMenuViewCallStack:=RegisterIDEMenuCommand(AParent,
          'View Call Stack', uemViewCallStack, nil, @ExecuteIdeMenuClick, nil, 'debugger_call_stack');
  {%endregion}

  {%region *** Source Section ***}
    SrcEditSubMenuSource := RegisterIDESubMenu(SourceEditorMenuRoot,
        'Source', uemSource, nil, nil, 'item_unit');
    AParent:=SrcEditSubMenuSource;
    SrcEditMenuEncloseSelection := RegisterIDEMenuCommand(AParent,
        'EncloseSelection', lisMenuEncloseSelection);
    SrcEditMenuEncloseInIFDEF := RegisterIDEMenuCommand(AParent,
        'itmSourceEncloseInIFDEF', lisMenuEncloseInIFDEF);
    SrcEditMenuCompleteCode := RegisterIDEMenuCommand(AParent,
        'CompleteCode', lisMenuCompleteCode, nil, @ExecuteIdeMenuClick);
    SrcEditMenuInvertAssignment := RegisterIDEMenuCommand(AParent,
        'InvertAssignment', uemInvertAssignment, nil, @ExecuteIdeMenuClick);
    SrcEditMenuUseUnit := RegisterIDEMenuCommand(AParent,
        'UseUnit', lisMenuUseUnit, nil, @ExecuteIdeMenuClick);
    SrcEditMenuShowUnitInfo := RegisterIDEMenuCommand(AParent,
        'ShowUnitInfo', lisMenuViewUnitInfo , nil, nil, nil, 'menu_view_unit_info');
  {%endregion}

  {%region *** Refactoring Section ***}
    SrcEditSubMenuRefactor:=RegisterIDESubMenu(SourceEditorMenuRoot,
                                               'Refactoring',uemRefactor);
    AParent:=SrcEditSubMenuRefactor;
    SrcEditMenuRenameIdentifier := RegisterIDEMenuCommand(AParent,
        'RenameIdentifier', lisMenuRenameIdentifier, nil, @ExecuteIdeMenuClick);
    SrcEditMenuExtractProc := RegisterIDEMenuCommand(AParent,
        'ExtractProc', lisMenuExtractProc, nil, @ExecuteIdeMenuClick);
    SrcEditMenuShowAbstractMethods := RegisterIDEMenuCommand(AParent,
        'ShowAbstractMethods', srkmecAbstractMethods, nil, @ExecuteIdeMenuClick);
    SrcEditMenuShowEmptyMethods := RegisterIDEMenuCommand(AParent,
        'ShowEmptyMethods', srkmecEmptyMethods, nil, @ExecuteIdeMenuClick);
    SrcEditMenuShowUnusedUnits := RegisterIDEMenuCommand(AParent,
        'ShowUnusedUnits', srkmecUnusedUnits, nil, @ExecuteIdeMenuClick);
    SrcEditMenuFindOverloads := RegisterIDEMenuCommand(AParent,
        'FindOverloads', srkmecFindOverloadsCapt, nil, @ExecuteIdeMenuClick);
    {$IFnDEF EnableFindOverloads}
    SrcEditMenuFindOverloads.Visible:=false;
    {$ENDIF}
    SrcEditMenuMakeResourceString := RegisterIDEMenuCommand(AParent,
        'MakeResourceString', lisMenuMakeResourceString, nil, @ExecuteIdeMenuClick);
  {%endregion}

  SrcEditMenuEditorProperties:=RegisterIDEMenuCommand(SourceEditorMenuRoot,
           'EditorProperties', lisOptions, nil, nil, nil, 'menu_environment_options');
end;

function dbgSourceNoteBook(snb: TSourceNotebook): string;
var
  i: Integer;
begin
  Result:='';
  if snb=nil then begin
    Result:='nil';
  end else if snb.Count=0 then begin
    Result:='empty';
  end else begin
    for i:=0 to 4 do begin
      if i>=snb.Count then break;
      Result+='"'+ExtractFilename(snb.Items[i].FileName)+'",';
    end;
  end;
  if RightStr(Result,1)=',' then Result:=LeftStr(Result,length(Result)-1);
  Result:='['+Result+']';
end;

function CompareSrcEditIntfWithFilename(SrcEdit1, SrcEdit2: Pointer): integer;
var
  SE1: TSourceEditorInterface absolute SrcEdit1;
  SE2: TSourceEditorInterface absolute SrcEdit2;
begin
  Result:=CompareFilenames(SE1.FileName,SE2.FileName);
end;

function CompareFilenameWithSrcEditIntf(FilenameStr, SrcEdit: Pointer): integer;
var
  SE1: TSourceEditorInterface absolute SrcEdit;
begin
  Result:=CompareFilenames(AnsiString(FileNameStr),SE1.FileName);
end;

{ TSourceEditorWordCompletion }

constructor TSourceEditorWordCompletion.Create;
begin
  inherited Create;

  FSourceList := TObjectList.Create;
  FIncludeWords := icwIncludeFromAllUnits;
end;

destructor TSourceEditorWordCompletion.Destroy;
begin
  FSourceList.Free;

  inherited Destroy;
end;

procedure TSourceEditorWordCompletion.DoGetSource(var Source: TStrings;
  var TopLine, BottomLine: Integer; var IgnoreWordPos: TPoint;
  SourceIndex: integer);
var
  SourceItem: TSourceListItem;
begin
  if SourceIndex=0 then
    ReloadSourceList;

  if SourceIndex>=FSourceList.Count then
    Exit;

  SourceItem := FSourceList[SourceIndex] as TSourceListItem;
  Source := SourceItem.Source;
  IgnoreWordPos := SourceItem.IgnoreWordPos;
end;

procedure TSourceEditorWordCompletion.ReloadSourceList;
var
  CurEditor, TempEditor: TSourceEditor;
  I: integer;
  Mng: TSourceEditorManager;
  New: TSourceListItem;
  AddedFileNames: TStringList;
begin
  FSourceList.Clear;
  if FIncludeWords=icwDontInclude then
    Exit;

  Mng := SourceEditorManager;
  CurEditor:=Mng.GetActiveSE;
  if (CurEditor<>nil) then
  begin
    New := TSourceListItem.Create;
    New.Source:=CurEditor.EditorComponent.Lines;
    New.IgnoreWordPos:=CurEditor.EditorComponent.LogicalCaretXY;
    Dec(New.IgnoreWordPos.Y); // LogicalCaretXY starts with 1 as top line
    FSourceList.Add(New);
  end;

  if FIncludeWords=icwIncludeFromAllUnits then
  begin
    AddedFileNames := TStringList.Create;
    try
      AddedFileNames.Sorted := True;
      AddedFileNames.Duplicates := dupIgnore;
      for I := 0 to Mng.SourceEditorCount-1 do
      begin
        TempEditor := Mng.SourceEditors[I];
        if ( TempEditor<>CurEditor)
        and (TempEditor.FileName<>CurEditor.FileName)
        and (AddedFileNames.IndexOf(TempEditor.FileName)=-1) then
        begin
          New := TSourceListItem.Create;
          New.Source:=TempEditor.EditorComponent.Lines;
          New.IgnoreWordPos:=Point(-1,-1);
          FSourceList.Add(New);
          AddedFileNames.Add(TempEditor.FileName);
        end;
      end;
    finally
      AddedFileNames.Free;
    end;
  end;
end;

{ TBrowseEditorTabHistoryDialog }

procedure TBrowseEditorTabHistoryDialog.DoCreate;
begin
  inherited DoCreate;

  Assert(Owner is TSourceNotebook);
  FNotebook := TSourceNotebook(Owner);

  Caption := lisCoolbarSourceTab;
  BorderIcons:=[];
  BorderStyle:=bsSizeable;
  KeyPreview := True;

  FEditorList := TListBox.Create(Self);
  FEditorList.Parent := Self;
  FEditorList.Align := alClient;
  {$IFDEF MSWINDOWS}//bsNone seems to work on Windows only
  FEditorList.BorderStyle := bsNone;
  {$ENDIF}
end;

procedure TBrowseEditorTabHistoryDialog.KeyDown(var Key: Word;
  Shift: TShiftState);
  function CommandUsed(Command: TIDECommand): Boolean;
  begin
    Result :=
     ((Command.ShortcutA.Key1 = Key) and (Command.ShortcutA.Shift1 = Shift)) or
     ((Command.ShortcutA.Key2 = Key) and (Command.ShortcutA.Shift2 = Shift));
  end;
var
  xPrev, xNext: TIDECommand;
begin
  xPrev := IDECommandList.FindIDECommand(ecPrevEditorInHistory);
  if CommandUsed(xPrev) then
  begin
    MoveInList(True);
    Key := 0;
  end else
  begin
    xNext := IDECommandList.FindIDECommand(ecNextEditorInHistory);
    if CommandUsed(xNext) then
    begin
      MoveInList(False);
      Key := 0;
    end;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TBrowseEditorTabHistoryDialog.MoveInList(aForward: Boolean);
begin
  if aForward then
  begin
    if FEditorList.ItemIndex < FEditorList.Items.Count-1 then
      FEditorList.ItemIndex := FEditorList.ItemIndex + 1
    else
      FEditorList.ItemIndex := 0;
  end else
  begin
    if FEditorList.ItemIndex > 0 then
      FEditorList.ItemIndex := FEditorList.ItemIndex - 1
    else
      FEditorList.ItemIndex := FEditorList.Items.Count - 1;
  end;
end;

procedure TBrowseEditorTabHistoryDialog.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key = VK_CONTROL then
  begin
    Close;
    if FEditorList.ItemIndex >= 0 then
      FNotebook.PageIndex := TCustomPage(FEditorList.Items.Objects[FEditorList.ItemIndex]).PageIndex;
    if (FNotebook.ActiveEditor<>nil) and
      FNotebook.ActiveEditor.EditorControl.CanSetFocus
    then
      FNotebook.ActiveEditor.EditorControl.SetFocus;
    Key := 0;
  end;

  inherited KeyUp(Key, Shift);
end;

procedure TBrowseEditorTabHistoryDialog.Show(aForward: Boolean);
  procedure PlaceMe;
  var
    xWidth, xHeight: Integer;
    xTopLeft, xRightBottom: TPoint;
  begin
    xWidth := Canvas.TextWidth('m')*20;
    if FEditorList.ItemHeight>0 then//ItemHeight can be 0 the first time
      xHeight := Min(10, FEditorList.Items.Count)*FEditorList.ItemHeight
    else
      xHeight := 200;
    {$IFNDEF MSWINDOWS}
    xHeight := xHeight + GetSystemMetrics(SM_CYBORDER)*2;
    {$ENDIF}

    xTopLeft := FNotebook.ClientToScreen(Point(0, 0));
    xRightBottom := FNotebook.ClientToScreen(FNotebook.ClientRect.BottomRight);
    SetBounds(
      (xTopLeft.x+xRightBottom.x-xWidth) div 2,
      (xTopLeft.y+xRightBottom.y-xHeight) div 2,
      xWidth,
      xHeight);
  end;

var
  I: Integer;
  xPage: TCustomPage;
  xIndex: TAvlTree;
begin
  if FNotebook.PageCount <= 1 then
    Exit;

  FEditorList.Items.BeginUpdate;
  xIndex := TAvlTree.Create;
  try
    FEditorList.Items.Clear;
    for I := 0 to FNotebook.FHistoryList.Count-1 do
    begin
      xPage := TCustomPage(FNotebook.FHistoryList[I]);
      FEditorList.Items.AddObject(xPage.Caption, xPage);
      xIndex.Add(xPage);
    end;

    //add pages not in history to the right of the active page
    for I := FNotebook.PageIndex+1 to FNotebook.PageCount-1 do
    begin
      xPage := FNotebook.NoteBookPage[I];
      if xIndex.Find(xPage)=nil then
        FEditorList.Items.AddObject(xPage.Caption, xPage);
    end;
    //add pages not in history to the left of the active page
    for I := 0 to FNotebook.PageIndex-1 do
    begin
      xPage := FNotebook.NoteBookPage[I];
      if xIndex.Find(xPage)=nil then
        FEditorList.Items.AddObject(xPage.Caption, xPage);
    end;
  finally
    xIndex.Free;
    FEditorList.Items.EndUpdate;
  end;

  if aForward then
    FEditorList.ItemIndex := 1
  else
    FEditorList.ItemIndex := FEditorList.Count-1;

  PlaceMe;
  Visible := True;
  PlaceMe;
  BringToFront;
end;

{ TSourceEditorHintWindowManager }

procedure TSourceEditorHintWindowManager.ActivateHint(const ScreenPos: TPoint;
  const ABaseURL, AHint: string; AAutoShown: Boolean; AMouseOffset: Boolean);
begin
  ActivateHint(Rect(ScreenPos.x, ScreenPos.y, ScreenPos.x, ScreenPos.y), ABaseURL, AHint, AAutoShown, AMouseOffset);
end;

procedure TSourceEditorHintWindowManager.ActivateHint(const ScreenRect: TRect;
  const ABaseURL, AHint: string; AAutoShown: Boolean; AMouseOffset: Boolean);
begin
  FAutoShown := AAutoShown;
  BaseURL := ABaseURL;
  FLastHint := AHint;
  FScreenRect := ScreenRect;
  if AAutoShown then
  begin
    FAutoHintMousePos := Mouse.CursorPos;
    if not(HintIsVisible and (FLastHint = AHint)) then
      ShowHint(Point(ScreenRect.Left, ScreenRect.Bottom),AHint,AMouseOffset);
  end else
  begin
    if HintIsVisible and (FLastHint = AHint) then
      HideIfVisible
    else
      ShowHint(Point(ScreenRect.Left, ScreenRect.Bottom),AHint,AMouseOffset);
  end;
  FAutoHideHintTimer.Enabled := AAutoShown;
end;

constructor TSourceEditorHintWindowManager.Create(AManager: TSourceEditorManager);
begin
  inherited Create;

  FManager := AManager;
  // HintTimer
  FAutoHintTimer := TIdleTimer.Create(nil);
  with FAutoHintTimer do begin
    Interval := EditorOpts.AutoHintDelayInMSec;
    Enabled := False;
    AutoEnabled := False;
    OnTimer := @HintTimer;
  end;
  // Track mouse movements outside the IDE, if hint is visible
  FAutoHideHintTimer := TTimer.Create(nil);
  with FAutoHideHintTimer do begin
    Interval := 500;
    Enabled := False;
    OnTimer := @HideHintTimer;
  end;
end;

destructor TSourceEditorHintWindowManager.Destroy;
begin
  FAutoHintTimer.Free;
  FAutoHideHintTimer.Free;

  inherited Destroy;
end;

procedure TSourceEditorHintWindowManager.HideAutoHint;
begin
  if FAutoHintTimer<>nil then
  begin
    FAutoHintTimer.AutoEnabled := false;
    FAutoHintTimer.Enabled:=false;
  end;
  if FAutoHideHintTimer <> nil then
    FAutoHideHintTimer.Enabled := False;
  if AutoStartCompletionBoxTimer<>nil then
    AutoStartCompletionBoxTimer.Enabled:=false;
  if FAutoShown then
    HideHint;
end;

procedure TSourceEditorHintWindowManager.HideHintTimer(Sender: TObject);
begin
  if HintIsVisible and FAutoShown then begin
    if ComparePoints(FAutoHintMousePos, Mouse.CursorPos) <> 0 then begin
      // TODO: introduce property, to indicate if hint is interactive
      if HintIsComplex or not IsRectEmpty(FScreenRect) then
        HideAutoHintAfterMouseMoved
      else
        HideAutoHint;
    end;
  end
  else
    FAutoHideHintTimer.Enabled := false;
end;

procedure TSourceEditorHintWindowManager.HintTimer(Sender: TObject);
var
  MousePos: TPoint;
  AControl: TControl;
begin
  FAutoHintTimer.Enabled := False;
  FAutoHintTimer.AutoEnabled := False;
  if not FManager.ActiveSourceWindow.IsVisible then exit;
  MousePos := Mouse.CursorPos;
  AControl:=FindLCLControl(MousePos);
  if (AControl=nil) or (not FManager.ActiveSourceWindow.ContainsControl(AControl)) then exit;
  if AControl is TSynEdit then
    FManager.ActiveSourceWindow.ShowSynEditHint(MousePos);
end;

procedure TSourceEditorHintWindowManager.HideAutoHintAfterMouseMoved;
const
  MaxJitter = 3;
var
  Cur: TPoint;
  OkX, OkY: Boolean;
  hw: THintWindow;
begin
  if HintIsVisible and not FAutoShown then Exit;
  FAutoHideHintTimer.Enabled := False;
  if HintIsVisible then begin
    Cur := Mouse.CursorPos; // Desktop coordinates
    if (not IsRectEmpty(FScreenRect)) then
    begin
      // Do not close, if mouse still over the same word, that triggered the hint
      if PtInRect(FScreenRect, Cur) then
        Exit;
    end else
    begin
      // Fallback if FScreenRect is empty (for legacy calls)
      hw := CurHintWindow;
      OkX := ( (FAutoHintMousePos.x <= hw.Left) and
               (Cur.x > FAutoHintMousePos.x) and (Cur.x <= hw.Left + hw.Width)
             ) or
             ( (FAutoHintMousePos.x >= hw.Left + hw.Width) and
               (Cur.x < FAutoHintMousePos.x) and (Cur.x >= hw.Left)
             ) or
             ( (Cur.x >= hw.Left) and (Cur.x <= hw.Left + hw.Width) );
      OkY := ( (FAutoHintMousePos.y <= hw.Top) and
               (Cur.y > FAutoHintMousePos.y) and (Cur.y <= hw.Top + hw.Height)
             ) or
             ( (FAutoHintMousePos.y >= hw.Top + hw.Height) and
               (Cur.y < FAutoHintMousePos.y) and (Cur.y >= hw.Top)
             ) or
             ( (Cur.y >= hw.Top) and (Cur.y <= hw.Top + hw.Height) );

      if OkX then FAutoHintMousePos.x := Cur.x;
      if OkY then FAutoHintMousePos.y := Cur.y;

      OkX := OkX or
             ( (FAutoHintMousePos.x <= hw.Left + MaxJitter) and
               (Cur.x > FAutoHintMousePos.x - MaxJitter) and (Cur.x <= hw.Left + hw.Width + MaxJitter)
             ) or
             ( (FAutoHintMousePos.x >= hw.Left + hw.Width - MaxJitter) and
               (Cur.x < FAutoHintMousePos.x + MaxJitter) and (Cur.x >= hw.Left - MaxJitter)
             );
      OkY := OkY or
             ( (FAutoHintMousePos.y <= hw.Top + MaxJitter) and
               (Cur.y > FAutoHintMousePos.y - MaxJitter) and (Cur.y <= hw.Top + hw.Height + MaxJitter)
             ) or
             ( (FAutoHintMousePos.y >= hw.Top + hw.Height - MaxJitter) and
               (Cur.y < FAutoHintMousePos.y + MaxJitter) and (Cur.y >= hw.Top - MaxJitter)
             );

      if (OkX and OkY) then begin
        FAutoHideHintTimer.Enabled := True;
        exit;
      end;
    end;
  end;
  HideHint;
end;

procedure TSourceEditorHintWindowManager.UpdateHintTimer;
begin
  with EditorOpts do
    if (MainIDEInterface.ToolStatus=itDebugger) then
      FAutoHintTimer.AutoEnabled := AutoToolTipExprEval or AutoToolTipSymbTools
    else
      FAutoHintTimer.AutoEnabled := AutoToolTipSymbTools;
end;


{ TSourceEditCompletion }

procedure TSourceEditCompletion.CompletionFormResized(Sender: TObject);
begin
  EnvironmentOptions.Desktop.CompletionWindowWidth  := TheForm.Width;
  EnvironmentOptions.Desktop.CompletionWindowHeight := TheForm.NbLinesInWindow;
end;

function TSourceEditCompletion.GetCompletionFormClass: TSynBaseCompletionFormClass;
begin
  Result := TSourceEditCompletionForm;
end;

procedure TSourceEditCompletion.ccExecute(Sender: TObject);
// init completion form
// called by OnExecute just before showing
var
  S: TStrings;
  Prefix: String;
  I: Integer;
  NewStr: String;
  SynEditor: TIDESynEditor;
Begin
  {$IFDEF VerboseIDECompletionBox}
  debugln(['TSourceEditCompletion.ccExecute START']);
  {$ENDIF}
  TheForm.Font := Editor.Font;

  FActiveEditTextColor := Editor.Font.Color;
  FActiveEditBorderColor := RGBToColor(200, 200, 200);
  FActiveEditBackgroundColor := Editor.Color;
  FActiveEditTextSelectedColor := TSynEdit(Editor).SelectedColor.Foreground;
  FActiveEditBackgroundSelectedColor := TSynEdit(Editor).SelectedColor.Background;
  FActiveEditTextHighLightColor := clNone;

  if Editor.Highlighter<>nil
  then begin
    with Editor.Highlighter do begin
      if IdentifierAttribute<>nil
      then begin
        if IdentifierAttribute.ForeGround<>clNone then
          FActiveEditTextColor:=IdentifierAttribute.ForeGround;
        if IdentifierAttribute.BackGround<>clNone then
          FActiveEditBackgroundColor:=IdentifierAttribute.BackGround;
      end;
    end;
  end;

  if Editor is TIDESynEditor then
  begin
    SynEditor := TIDESynEditor(Editor);
    if SynEditor.MarkupIdentComplWindow.TextColor<>clNone then
      FActiveEditTextColor := SynEditor.MarkupIdentComplWindow.TextColor;
    if SynEditor.MarkupIdentComplWindow.BorderColor<>clNone then
      FActiveEditBorderColor := SynEditor.MarkupIdentComplWindow.BorderColor;
    if SynEditor.MarkupIdentComplWindow.WindowColor<>clNone then
      FActiveEditBackgroundColor := SynEditor.MarkupIdentComplWindow.WindowColor;
    if SynEditor.MarkupIdentComplWindow.TextSelectedColor<>clNone then
      FActiveEditTextSelectedColor := SynEditor.MarkupIdentComplWindow.TextSelectedColor;
    if SynEditor.MarkupIdentComplWindow.BackgroundSelectedColor<>clNone then
      FActiveEditBackgroundSelectedColor := SynEditor.MarkupIdentComplWindow.BackgroundSelectedColor;
    if SynEditor.MarkupIdentComplWindow.HighlightColor<>clNone then
      FActiveEditTextHighLightColor := SynEditor.MarkupIdentComplWindow.HighlightColor;
  end;

  S := TStringList.Create;
  try
    Prefix := CurrentString;
    case CurrentCompletionType of
     ctIdentCompletion:
       if InitIdentCompletionValues(S) then begin
         ToggleReplaceWhole:=not CodeToolsOpts.IdentComplReplaceIdentifier;
       end else begin
         ItemList.Clear;
         exit;
       end;

     ctWordCompletion:
       begin
         ToggleReplaceWhole:=not CodeToolsOpts.IdentComplReplaceIdentifier;
         ccSelection := '';
       end;

     ctTemplateCompletion:
       begin
         ccSelection:='';
         for I := 0 to Manager.CodeTemplateModul.Completions.Count-1 do begin
           NewStr := Manager.CodeTemplateModul.Completions[I];
           if NewStr<>'' then begin
             NewStr:=#3'B'+NewStr+#3'b';
             while length(NewStr)<10+4 do NewStr:=NewStr+' ';
             NewStr:=NewStr+' '+Manager.CodeTemplateModul.CompletionComments[I];
             S.Add(NewStr);
           end;
         end;
       end;

    end;

    ItemList := S;
  finally
    S.Free;
  end;
  CurrentString:=Prefix;
  // set colors
  if (Editor<>nil) and (TheForm<>nil) then begin
    with TheForm as TSourceEditCompletionForm do begin
      DrawBorderColor   := FActiveEditBorderColor;
      BackgroundColor   := FActiveEditBackgroundColor;
      clSelect          := FActiveEditBackgroundSelectedColor;
      TextColor         := FActiveEditTextColor;
      TextSelectedColor := FActiveEditTextSelectedColor;
      TextHighLightColor:= FActiveEditTextHighLightColor;
      //debugln('TSourceEditCompletion.ccExecute A Color=',DbgS(Color),
      // ' clSelect=',DbgS(clSelect),
      // ' TextColor=',DbgS(TextColor),
      // ' TextSelectedColor=',DbgS(TextSelectedColor),
      // '');
    end;
    debugln(['TSourceEditCompletion.ccExecute ',DbgSName(SourceEditorManager.ActiveCompletionPlugin)]);
    if (CurrentCompletionType=ctIdentCompletion)
    and (SourceEditorManager.ActiveCompletionPlugin=nil)
    then
      StartShowCodeHelp
    else if SrcEditHintWindow<>nil then
    begin
      SrcEditHintWindow.HelpEnabled:=false;
      TheForm.LongLineHintType := EditorOpts.CompletionLongLineHintType;
    end;
  end;
end;

procedure TSourceEditCompletion.ccCancel(Sender: TObject);
// user cancels completion form
begin
  {$IFDEF VerboseIDECompletionBox}
  DebugLnEnter(['TSourceNotebook.ccCancel START']);
  try
  //debugln(GetStackTrace(true));
  {$ENDIF}
  Manager.DeactivateCompletionForm;
  {$IFDEF VerboseIDECompletionBox}
  finally
    DebugLnExit(['TSourceNotebook.ccCancel END']);
  end;
  //debugln(GetStackTrace(true));
  {$ENDIF}
end;

procedure TSourceEditCompletion.ccComplete(var Value: string;
  SourceValue: string; var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char;
  Shift: TShiftState);
// completion selected -> deactivate completion form
// Called when user has selected a completion item

  function CharBehindIdent(const Line: string; StartPos: integer): char;
  begin
    while (StartPos<=length(Line))
    and (Line[StartPos] in ['_','A'..'Z','a'..'z']) do
      inc(StartPos);
    while (StartPos<=length(Line)) and (Line[StartPos] in [' ',#9]) do
      inc(StartPos);
    if StartPos<=length(Line) then
      Result:=Line[StartPos]
    else
      Result:=#0;
  end;

  function CharInFrontOfIdent(const Line: string; StartPos: integer): char;
  begin
    while (StartPos>=1)
    and (Line[StartPos] in ['_','A'..'Z','a'..'z']) do
      dec(StartPos);
    while (StartPos>=1) and (Line[StartPos] in [' ',#9]) do
      dec(StartPos);
    if StartPos>=1 then
      Result:=Line[StartPos]
    else
      Result:=#0;
  end;

var
  p1, p2: integer;
  ValueType: TIdentComplValue;
  NewCaretXY: TPoint;
  CursorToLeft: integer;
  NewValue: String;
  OldCompletionType: TCompletionType;
  prototypeAdded: boolean;
  SourceNoteBook: TSourceNotebook;
  IdentItem: TIdentifierListItem;
Begin
  {$IFDEF VerboseIDECompletionBox}
  DebugLnEnter(['TSourceNotebook.ccComplete START']);
  try
  {$ENDIF}
  prototypeAdded := false;
  OldCompletionType:=CurrentCompletionType;
  case CurrentCompletionType of

    ctIdentCompletion:
      begin
        if Manager.ActiveCompletionPlugin<>nil then
        begin
          Manager.ActiveCompletionPlugin.Complete(Value,SourceValue,
             SourceStart,SourceEnd,KeyChar,Shift);
          Manager.FActiveCompletionPlugin:=nil;
        end else begin
          IdentItem:=CodeToolBoss.IdentifierList.FilteredItems[Position];
          // add to history
          CodeToolBoss.IdentifierHistory.Add(IdentItem);
          if IdentItem is TCodeTemplateIdentifierListItem then
          begin
            if IdentItem.Identifier<>'' then
              Manager.CodeTemplateModul.ExecuteCompletion(IdentItem.Identifier, Editor);
          end else
          begin
            // get value
            NewValue:=GetIdentCompletionValue(Self, KeyChar, ValueType, CursorToLeft);
            if ValueType=icvIdentifier then ;
            // insert value plus special chars like brackets, semicolons, ...
            if ValueType <> icvNone then
              Editor.TextBetweenPointsEx[SourceStart, SourceEnd, scamEnd] := NewValue;
            if ValueType in [icvProcWithParams,icvIndexedProp] then
              prototypeAdded := true;
            if CursorToLeft>0 then
            begin
              NewCaretXY:=Editor.CaretXY;
              dec(NewCaretXY.X,CursorToLeft);
              Editor.CaretXY:=NewCaretXY;
            end;
          end;
          ccSelection := '';
          Value:='';
          SourceEnd := SourceStart;
        end;
      end;

    ctTemplateCompletion:
      begin
        // the completion is the bold text between #3'B' and #3'b'
        p1:=Pos(#3,Value);
        if p1>=0 then begin
          p2:=p1+2;
          while (p2<=length(Value)) and (Value[p2]<>#3) do inc(p2);
          Value:=copy(Value,p1+2,p2-p1-2);
          // keep parent identifier (in front of '.')
          p1:=length(ccSelection);
          while (p1>=1) and (ccSelection[p1]<>'.') do dec(p1);
          if p1>=1 then
            Value:=copy(ccSelection,1,p1)+Value;
        end;
        ccSelection := '';
        if Value<>'' then
          Manager.CodeTemplateModul.ExecuteCompletion(Value, Editor);
        SourceEnd := SourceStart;
        Value:='';
      end;

    ctWordCompletion:
      // the completion is already in Value
      begin
        ccSelection := '';
        if Value<>'' then AWordCompletion.AddWord(Value);
      end;

    else begin
      Value:='';
    end;
  end;

  Manager.DeactivateCompletionForm;

  //DebugLn(['TSourceNotebook.ccComplete ',KeyChar,' ',OldCompletionType=ctIdentCompletion]);
  if (KeyChar='.') and (OldCompletionType=ctIdentCompletion) then
  begin
    SourceCompletionCaretXY:=Editor.CaretXY;
    AutoStartCompletionBoxTimer.AutoEnabled:=true;
  end
  else if prototypeAdded and EditorOpts.AutoDisplayFunctionPrototypes then
  begin
     if Editor.Owner is TSourceNoteBook then
     begin
        SourceNoteBook := Editor.Owner as TSourceNoteBook;
        SourceNotebook.StartShowCodeContext(CodeToolsOpts.IdentComplJumpToError);
     end;
  end;

  {$IFDEF VerboseIDECompletionBox}
  finally
    DebugLnExit(['TSourceNotebook.ccComplete END']);
  end;
  {$ENDIF}
end;

function TSourceEditCompletion.OnSynCompletionPaintItem(const AKey: string;
  ACanvas: TCanvas; X, Y: integer; ItemSelected: boolean; Index: integer): boolean;
var
  MaxX: Integer;
  t: TCompletionType;
  hl: TSynCustomHighlighter;
  Colors: TPaintCompletionItemColors;
begin
  try
    with ACanvas do begin
      if (Editor<>nil) then
        Font := Editor.Font
      else begin
        Font.Size:=EditorOpts.EditorFontSize; // set Size before name for XLFD !
        Font.Name:=EditorOpts.EditorFont;
      end;
      Font.Style:=[];
    end;
    Colors.BackgroundColor := FActiveEditBackgroundColor;
    Colors.BackgroundSelectedColor := FActiveEditBackgroundSelectedColor;
    Colors.TextColor := FActiveEditTextColor;
    Colors.TextSelectedColor := FActiveEditTextSelectedColor;
    Colors.TextHilightColor := FActiveEditTextHighLightColor;
    MaxX:=TheForm.ClientWidth;
    t:=CurrentCompletionType;
    if Manager.ActiveCompletionPlugin<>nil then
    begin
      if Manager.ActiveCompletionPlugin.HasCustomPaint then
      begin
        Manager.ActiveCompletionPlugin.PaintItem(AKey,ACanvas,X,Y,ItemSelected,Index);
      end else begin
        t:=ctWordCompletion;
      end;
    end;
    hl := nil;
    if Editor <> nil then
      hl := Editor.Highlighter;
    PaintCompletionItem(AKey, ACanvas, X, Y, MaxX, ItemSelected, Index, self, t, hl, @Colors);
    Result:=true;
  except
    DebugLn('OnSynCompletionPaintItem failed');
    DumpStack;
    Result := false;
  end;
end;

function TSourceEditCompletion.OnSynCompletionMeasureItem(const AKey: string;
  ACanvas: TCanvas; ItemSelected: boolean; Index: integer): TPoint;
var
  MaxX: Integer;
  t: TCompletionType;
begin
  try
  with ACanvas do begin
    if (Editor<>nil) then
      Font:=Editor.Font
    else begin
      Font.Size:=EditorOpts.EditorFontSize; // set Size before name of XLFD !
      Font.Name:=EditorOpts.EditorFont;
    end;
    Font.Style:=[];
  end;
  MaxX := Screen.Width-20;
  t:=CurrentCompletionType;
  if Manager.ActiveCompletionPlugin<>nil then
  begin
    if Manager.ActiveCompletionPlugin.HasCustomPaint then
    begin
      Manager.ActiveCompletionPlugin.MeasureItem(AKey,ACanvas,ItemSelected,Index);
    end else begin
      t:=ctWordCompletion;
    end;
  end;
  Result := PaintCompletionItem(AKey,ACanvas,0,0,MaxX,ItemSelected,Index,
                                self,t,nil,nil,True);
  Result.Y:=FontHeight;
  except
    DebugLn('OnSynCompletionMeasureItem failed');
    DumpStack;
    Result := Point(FontHeight * length(AKey), FontHeight);
  end;
end;

procedure TSourceEditCompletion.OnSynCompletionSearchPosition(
  var APosition: integer);
// prefix changed -> filter list
var
  i,x:integer;
  CurStr,s:Ansistring;
  SL: TStrings;
  ItemCnt: Integer;
begin
  case CurrentCompletionType of

    ctIdentCompletion:
      if Manager.ActiveCompletionPlugin<>nil then
      begin
        // let plugin rebuild completion list
        SL:=TStringList.Create;
        try
          Manager.ActiveCompletionPlugin.PrefixChanged(CurrentString,
            APosition,sl);
          ItemList:=SL;
        finally
          SL.Free;
        end;
      end else begin
        // rebuild completion list
        APosition:=0;
        CurStr:=CurrentString;
        CodeToolBoss.IdentifierList.ContainsFilter := CodeToolsOpts.IdentComplUseContainsFilter;
        CodeToolBoss.IdentifierList.Prefix:=CurStr;
        ItemCnt:=CodeToolBoss.IdentifierList.GetFilteredCount;
        SL:=TStringList.Create;
        try
          sl.Capacity:=ItemCnt;
          for i:=0 to ItemCnt-1 do
            SL.Add('Dummy'); // these entries are not shown
          ItemList:=SL;
        finally
          SL.Free;
        end;
      end;

    ctTemplateCompletion:
      begin
        // search CurrentString in bold words (words after #3'B')
        CurStr:=CurrentString;
        i:=0;
        while i<ItemList.Count do begin
          s:=ItemList[i];
          x:=1;
          while (x<=length(s)) and (s[x]<>#3) do inc(x);
          if x<length(s) then begin
            inc(x,2);
            if UTF8CompareText(CurStr,copy(s,x,length(CurStr)))=0 then begin
              APosition:=i;
              break;
            end;
          end;
          inc(i);
        end;
      end;

    ctWordCompletion:
      begin
        // rebuild completion list
        APosition:=0;
        CurStr:=CurrentString;
        SL:=TStringList.Create;
        try
          aWordCompletion.GetWordList(SL, CurStr, CodeToolBoss.IdentifierList.ContainsFilter, false, 100);
          ItemList:=SL;
        finally
          SL.Free;
        end;
      end;

  end;
  if SrcEditHintWindow<>nil then
    SrcEditHintWindow.UpdateHints;
end;

procedure TSourceEditCompletion.OnSynCompletionCompletePrefix(Sender: TObject);
var
  OldPrefix: String;
  NewPrefix: String;
  SL: TStringList;
  AddPrefix: String;
begin
  OldPrefix:=CurrentString;
  NewPrefix:=OldPrefix;

  case CurrentCompletionType of

  ctIdentCompletion:
    if Manager.ActiveCompletionPlugin<>nil then
    begin
      Manager.ActiveCompletionPlugin.CompletePrefix(NewPrefix);
    end else begin
      NewPrefix:=CodeToolBoss.IdentifierList.CompletePrefix(OldPrefix);
    end;

  ctWordCompletion:
    begin
      aWordCompletion.CompletePrefix(OldPrefix,NewPrefix,false);
    end;

  end;

  if NewPrefix<>OldPrefix then begin
    AddPrefix:=copy(NewPrefix,length(OldPrefix)+1,length(NewPrefix));
    Editor.InsertTextAtCaret(AddPrefix);
    if CurrentCompletionType=ctWordCompletion then begin
      SL:=TStringList.Create;
      try
        aWordCompletion.GetWordList(SL, NewPrefix, CodeToolBoss.IdentifierList.ContainsFilter, false, 100);
        ItemList:=SL;
      finally
        SL.Free;
      end;
    end;
    CurrentString:=NewPrefix;
  end;
end;

procedure TSourceEditCompletion.OnSynCompletionNextChar(Sender: TObject);
var
  NewPrefix: String;
  Line: String;
  LogCaret: TPoint;
  CharLen: LongInt;
  AddPrefix: String;
begin
  if Editor=nil then exit;
  LogCaret:=Editor.LogicalCaretXY;
  if LogCaret.Y>=Editor.Lines.Count then exit;
  Line:=Editor.Lines[LogCaret.Y-1];
  if LogCaret.X>length(Line) then exit;
  CharLen:=UTF8CodepointSize(@Line[LogCaret.X]);
  AddPrefix:=copy(Line,LogCaret.X,CharLen);
  if not Editor.IsIdentChar(AddPrefix) then exit;
  NewPrefix:=CurrentString+AddPrefix;
  //debugln('TSourceNotebook.OnSynCompletionNextChar NewPrefix="',NewPrefix,'" LogCaret.X=',dbgs(LogCaret.X));
  inc(LogCaret.X);
  Editor.LogicalCaretXY:=LogCaret;
  CurrentString:=NewPrefix;
end;

procedure TSourceEditCompletion.OnSynCompletionPrevChar(Sender: TObject);
var
  NewPrefix: String;
  NewLen: LongInt;
begin
  NewPrefix:=CurrentString;
  if NewPrefix='' then exit;
  if Editor=nil then exit;
  Editor.CaretX:=Editor.CaretX-1;
  NewLen:=UTF8FindNearestCharStart(PChar(NewPrefix),length(NewPrefix),
                                   length(NewPrefix)-1);
  NewPrefix:=copy(NewPrefix,1,NewLen);
  CurrentString:=NewPrefix;
end;

procedure TSourceEditCompletion.OnSynCompletionKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (System.Pos(Key,EndOfTokenChr)>0) then begin
    // identifier completed
    //debugln('TSourceNotebook.OnSynCompletionKeyPress A');
    TheForm.OnValidate(Sender,Key,[]);
    //debugln('TSourceNotebook.OnSynCompletionKeyPress B');
    Key:=#0;
  end;
end;

procedure TSourceEditCompletion.OnSynCompletionUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if (length(UTF8Key)=1)
  and (System.Pos(UTF8Key[1],EndOfTokenChr)>0) then begin
    // identifier completed
    //debugln('TSourceNotebook.OnSynCompletionUTF8KeyPress A');
    TheForm.OnValidate(Sender,UTF8Key,[]);
    //debugln('TSourceNotebook.OnSynCompletionKeyPress B');
    UTF8Key:='';
  end;
  //debugln('TSourceNotebook.OnSynCompletionKeyPress B UTF8Key=',dbgstr(UTF8Key));
end;

procedure TSourceEditCompletion.OnSynCompletionPositionChanged(Sender: TObject);
begin
  if Manager.ActiveCompletionPlugin<>nil then
    Manager.ActiveCompletionPlugin.IndexChanged(Position);
  if SrcEditHintWindow<>nil then
    SrcEditHintWindow.UpdateHints;
end;

function TSourceEditCompletion.InitIdentCompletionValues(S: TStrings): boolean;
var
  i: integer;
  Handled: boolean;
  Abort: boolean;
  Prefix: string;
  ItemCnt: Integer;
begin
  Result:=false;
  Prefix := CurrentString;
  if Manager.ActiveCompletionPlugin<>nil then
  begin
    Result := Manager.ActiveCompletionPlugin.Collect(S);
  end
  else if Assigned(Manager.OnInitIdentCompletion) then
  begin
    Manager.OnInitIdentCompletion(Self, FIdentCompletionJumpToError, Handled, Abort);
    if Handled then begin
      if Abort then exit;
      // add one entry per item
      CodeToolBoss.IdentifierList.Prefix:=Prefix;
      ItemCnt:=CodeToolBoss.IdentifierList.GetFilteredCount;
      //DebugLn('InitIdentCompletion B Prefix=',Prefix,' ItemCnt=',IntToStr(ItemCnt));
      Position:=0;
      for i:=0 to ItemCnt-1 do
        s.Add('Dummy');
      Result:=true;
      exit;
    end;
  end;
end;

procedure TSourceEditCompletion.StartShowCodeHelp;
begin
  if SrcEditHintWindow = nil then begin
    SrcEditHintWindow := TSrcEditHintWindow.Create(Manager);
    SrcEditHintWindow.Name:='TSourceNotebook_SrcEditHintWindow';
    SrcEditHintWindow.Provider:=TFPDocHintProvider.Create(SrcEditHintWindow);
  end;
  SrcEditHintWindow.AnchorForm := TheForm;
  //debugln(['TSourceEditCompletion.StartShowCodeHelp ',CodeToolsOpts.IdentComplShowHelp]);
  if CodeToolsOpts.IdentComplShowHelp then begin
    TheForm.LongLineHintType:=sclpNone;
    SrcEditHintWindow.HelpEnabled:=true;
  end else begin
    TheForm.LongLineHintType:=EditorOpts.CompletionLongLineHintType;
    SrcEditHintWindow.HelpEnabled:=false;
  end;
end;

function TSourceEditCompletion.Manager: TSourceEditorManager;
begin
  Result := SourceEditorManager;
end;

constructor TSourceEditCompletion.Create(AOwner: TComponent);
begin
  inherited;
  EndOfTokenChr:='()[].,;:-+=^*<>/';
  Width:=400;
  OnExecute := @ccExecute;
  OnCancel := @ccCancel;
  OnCodeCompletion := @ccComplete;
  OnPaintItem:=@OnSynCompletionPaintItem;
  OnMeasureItem := @OnSynCompletionMeasureItem;
  OnSearchPosition:=@OnSynCompletionSearchPosition;
  OnKeyCompletePrefix:=@OnSynCompletionCompletePrefix;
  OnKeyNextChar:=@OnSynCompletionNextChar;
  OnKeyPrevChar:=@OnSynCompletionPrevChar;
  OnKeyPress:=@OnSynCompletionKeyPress;
  OnUTF8KeyPress:=@OnSynCompletionUTF8KeyPress;
  OnPositionChanged:=@OnSynCompletionPositionChanged;
  ShortCut:=Menus.ShortCut(VK_UNKNOWN,[]);
  TheForm.ShowSizeDrag := True;
  TheForm.Width := Max(50, EnvironmentOptions.Desktop.CompletionWindowWidth);
  TheForm.NbLinesInWindow := Max(3, EnvironmentOptions.Desktop.CompletionWindowHeight);
  TheForm.OnDragResized  := @CompletionFormResized;
end;

{ TSourceEditorSharedValues }

function TSourceEditorSharedValues.GetSharedEditors(Index: Integer): TSourceEditor;
begin
  Result := TSourceEditor(FSharedEditorList[Index]);
end;

function TSourceEditorSharedValues.GetOtherSharedEditors(Caller: TSourceEditor;
  Index: Integer): TSourceEditor;
begin
  if Index >= FSharedEditorList.IndexOf(Caller) then
    inc(Index);
  Result := TSourceEditor(FSharedEditorList[Index]);
end;

function TSourceEditorSharedValues.SynEditor: TIDESynEditor;
begin
  Result := SharedEditors[0].FEditor;
end;

function TSourceEditorSharedValues.GetSharedEditorsBase(Index: Integer): TSourceEditorBase;
begin
  Result := TSourceEditorBase(FSharedEditorList[Index]);
end;

procedure TSourceEditorSharedValues.SetCodeBuffer(const AValue: TCodeBuffer);
var
  i: Integer;
  SrcEdit: TSourceEditor;
  SharedEdit: TSourceEditor;
  ETChanges: TETSingleSrcChanges;
begin
  if FCodeBuffer = AValue then exit;
  if FCodeBuffer<>nil then begin
    for i := 0 to FSharedEditorList.Count - 1 do begin
      SharedEdit := SharedEditors[i];
      if SharedEdit.FEditPlugin<>nil then
        SharedEdit.FEditPlugin.Changes:=nil;
    end;
    FCodeBuffer.RemoveChangeHook(@OnCodeBufferChanged);
    if FCodeBuffer.Scanner<>nil then
      DisconnectScanner(FCodeBuffer.Scanner);
    if FMainLinkScanner<>nil then begin
      DisconnectScanner(FMainLinkScanner);
      FMainLinkScanner:=nil;
    end;
  end;

  for i := 0 to FSharedEditorList.Count - 1 do begin
    SrcEdit:=SharedEditors[i];
    SrcEdit.SourceNotebook.FSrcEditsSortedForFilenames.RemovePointer(SrcEdit);
  end;

  FCodeBuffer := AValue;

  for i := 0 to FSharedEditorList.Count - 1 do begin
    SrcEdit:=SharedEditors[i];
    SrcEdit.SourceNotebook.FSrcEditsSortedForFilenames.Add(SrcEdit);
  end;

  if FCodeBuffer <> nil then
  begin
    DebugBoss.LockCommandProcessing;
    try
      for i := 0 to FSharedEditorList.Count - 1 do begin
        // HasExecutionMarks is shared through synedit => this is only needed once
        // but HasExecutionMarks must be called on each synedit, so each synedit is notified
        SharedEditors[i].ClearExecutionMarks;
      end;
      FCodeBuffer.AddChangeHook(@OnCodeBufferChanged);
      if FCodeBuffer.Scanner<>nil then
        ConnectScanner(FCodeBuffer.Scanner);
      ETChanges := SourceEditorManager.FChangesQueuedForMsgWnd.GetChanges(
                                                     FCodeBuffer.Filename,true);
      for i := 0 to FSharedEditorList.Count - 1 do begin
        SharedEdit:=SharedEditors[i];
        if assigned(SharedEdit.FEditPlugin) then
          SharedEdit.FEditPlugin.Changes := ETChanges;
      end;
      if MessagesView<>nil then
        MessagesView.MessagesFrame1.CreateMarksForFile(SynEditor,FCodeBuffer.Filename,true);
      if (FIgnoreCodeBufferLock <= 0) and (not FCodeBuffer.IsEqual(SynEditor.Lines))
      then begin
        {$IFDEF IDE_DEBUG}
        debugln(' *** WARNING *** : TSourceEditor.SetCodeBuffer - loosing marks: ',FCodeBuffer.Filename);
        {$ENDIF}
        for i := 0 to FSharedEditorList.Count - 1 do begin
          SharedEdit:=SharedEditors[i];
          if assigned(SharedEdit.FEditPlugin) then
            SharedEdit.FEditPlugin.Enabled := False;
        end;
        SynEditor.BeginUpdate;
        SynEditor.InvalidateAllIfdefNodes;
        FCodeBuffer.AssignTo(SynEditor.Lines, false);
        FEditorStampCommitedToCodetools:=(SynEditor.Lines as TSynEditLines).TextChangeStamp;
        SynEditor.EndUpdate;
        for i := 0 to FSharedEditorList.Count - 1 do begin
          SharedEdit:=SharedEditors[i];
          if assigned(SharedEdit.FEditPlugin) then
            SharedEdit.FEditPlugin.Enabled := True;
          if SharedEdit.Visible then
            SharedEdit.UpdateIfDefNodeStates(True);
        end;
      end;
      for i := 0 to FSharedEditorList.Count - 1 do begin
        SharedEdit:=SharedEditors[i];
        if SharedEdit.IsActiveOnNoteBook then
          SharedEdit.SourceNotebook.UpdateStatusBar;
        // HasExecutionMarks is shared through synedit => this is only needed once
        // but HasExecutionMarks must be called on each synedit, so each synedit is notified
        if (DebugBoss.State in [dsPause, dsRun]) and
           not SharedEdit.HasExecutionMarks and (FCodeBuffer.FileName <> '')
        then
          SharedEdit.FillExecutionMarks;
      end;
    finally
      DebugBoss.UnLockCommandProcessing;
    end;
  end;
end;

function TSourceEditorSharedValues.GetModified: Boolean;
begin
  Result := FModified or SynEditor.Modified;
end;

procedure TSourceEditorSharedValues.SetModified(const AValue: Boolean);
var
  OldModified: Boolean;
  i: Integer;
begin
  OldModified := Modified; // Include SynEdit
  FModified := AValue;
  if not FModified then
  begin
    SynEditor.Modified := False; // All shared SynEdits share this value
    FEditorStampCommitedToCodetools := TSynEditLines(SynEditor.Lines).TextChangeStamp;
    for i := 0 to FSharedEditorList.Count - 1 do
      SharedEditors[i].FEditor.MarkTextAsSaved; // Todo: centralize in SynEdit
  end;
  if OldModified <> Modified then
    for i := 0 to FSharedEditorList.Count - 1 do begin
      SharedEditors[i].UpdatePageName;
      SharedEditors[i].SourceNotebook.UpdateStatusBar;
    end;
end;

procedure TSourceEditorSharedValues.OnCodeBufferChanged(Sender: TSourceLog;
  SrcLogEntry: TSourceLogEntry);

  procedure MoveTxt(const StartPos, EndPos, MoveToPos: TPoint;
    DirectionForward: boolean);
  var Txt: string;
  begin
    if DirectionForward then begin
      SynEditor.TextBetweenPointsEx[MoveToPos, MoveToPos, scamAdjust] :=
        SynEditor.TextBetweenPoints[StartPos, EndPos];
      SynEditor.TextBetweenPointsEx[StartPos, EndPos, scamAdjust] := '';
    end else begin
      Txt := SynEditor.TextBetweenPoints[StartPos, EndPos];
      SynEditor.TextBetweenPointsEx[StartPos, EndPos, scamAdjust] := '';
      SynEditor.TextBetweenPointsEx[MoveToPos, MoveToPos, scamAdjust] := Txt;;
    end;
  end;

var
  StartPos, EndPos, MoveToPos: TPoint;
  CodeToolsInSync: Boolean;
  i: Integer;
begin
  {$IFDEF IDE_DEBUG}
  debugln(['[TSourceEditor.OnCodeBufferChanged] A ',FIgnoreCodeBufferLock,' ',SrcLogEntry<>nil]);
  {$ENDIF}
  if FIgnoreCodeBufferLock>0 then exit;
  DebugBoss.LockCommandProcessing;
  SynEditor.BeginUpdate;
  try
    CodeToolsInSync:=not NeedsUpdateCodeBuffer;
    if SrcLogEntry<>nil then begin
      SynEditor.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditorSharedValues.OnCodeBufferChanged'){$ENDIF};
      SynEditor.BeginUpdate;
      SynEditor.TemplateEdit.IncExternalEditLock;
      SynEditor.SyncroEdit.IncExternalEditLock;
      try
        case SrcLogEntry.Operation of
          sleoInsert:
            begin
              Sender.AbsoluteToLineCol(SrcLogEntry.Position,StartPos.Y,StartPos.X);
              if StartPos.Y>=1 then
                SynEditor.TextBetweenPointsEx[StartPos, StartPos, scamAdjust] := SrcLogEntry.Txt;
            end;
          sleoDelete:
            begin
              Sender.AbsoluteToLineCol(SrcLogEntry.Position,StartPos.Y,StartPos.X);
              Sender.AbsoluteToLineCol(SrcLogEntry.Position+SrcLogEntry.Len,
                EndPos.Y,EndPos.X);
              if (StartPos.Y>=1) and (EndPos.Y>=1) then
                SynEditor.TextBetweenPointsEx[StartPos, EndPos, scamAdjust] := '';
            end;
          sleoMove:
            begin
              Sender.AbsoluteToLineCol(SrcLogEntry.Position,StartPos.Y,StartPos.X);
              Sender.AbsoluteToLineCol(SrcLogEntry.Position+SrcLogEntry.Len,
                EndPos.Y,EndPos.X);
              Sender.AbsoluteToLineCol(SrcLogEntry.MoveTo,MoveToPos.Y,MoveToPos.X);
              if (StartPos.Y>=1) and (EndPos.Y>=1) and (MoveToPos.Y>=1) then
                MoveTxt(StartPos, EndPos, MoveToPos,
                  SrcLogEntry.Position<SrcLogEntry.MoveTo);
            end;
        end;
      finally
        SynEditor.SyncroEdit.DecExternalEditLock;
        SynEditor.TemplateEdit.DecExternalEditLock;
        SynEditor.EndUpdate;
        SynEditor.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditorSharedValues.OnCodeBufferChanged'){$ENDIF};
      end;
    end else begin
      {$IFDEF VerboseSrcEditBufClean}
      debugln(['TSourceEditor.OnCodeBufferChanged clean up ',TCodeBuffer(Sender).FileName,' ',Sender=CodeBuffer,' ',Filename]);
      DumpStack;
      {$ENDIF}
      // HasExecutionMarks is shared through synedit => this is only needed once // but HasExecutionMarks must be called on each synedit, so each synedit is notified
      for i := 0 to FSharedEditorList.Count - 1 do
        SharedEditors[i].ClearExecutionMarks;
      for i := 0 to SharedEditorCount-1 do
        SharedEditors[i].BeforeCodeBufferReplace;

      SynEditor.InvalidateAllIfdefNodes;
      Sender.AssignTo(SynEditor.Lines,false);

      for i := 0 to SharedEditorCount-1 do
        SharedEditors[i].AfterCodeBufferReplace;
      // HasExecutionMarks is shared through synedit => this is only needed once // but HasExecutionMarks must be called on each synedit, so each synedit is notified
      for i := 0 to FSharedEditorList.Count - 1 do begin
        SharedEditors[i].FillExecutionMarks;
        if SharedEditors[i].Visible then
          SharedEditors[i].UpdateIfDefNodeStates(True);
      end;
    end;
    if CodeToolsInSync then begin
      // synedit and codetools were in sync -> mark as still in sync
      FEditorStampCommitedToCodetools:=TSynEditLines(SynEditor.Lines).TextChangeStamp;
    end;
  finally
    SynEditor.EndUpdate;
    DebugBoss.UnLockCommandProcessing;
  end;
end;

procedure TSourceEditorSharedValues.BeginGlobalUpdate;
begin
  inc(FInGlobalUpdate);
  if FInGlobalUpdate > 1 then exit;
  SynEditor.BeginUpdate;  // locks all shared SynEdits too
  SynEditor.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditorSharedValues.BeginGlobalUpdate'){$ENDIF};
end;

procedure TSourceEditorSharedValues.EndGlobalUpdate;
begin
  dec(FInGlobalUpdate);
  if FInGlobalUpdate > 0 then exit;
  SynEditor.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditorSharedValues.EndGlobalUpdate'){$ENDIF};
  SynEditor.EndUpdate;
end;

procedure TSourceEditorSharedValues.AddSharedEditor(AnEditor: TSourceEditor);
begin
  if FSharedEditorList.IndexOf(AnEditor) < 0 then
    FSharedEditorList.Add(AnEditor);
end;

procedure TSourceEditorSharedValues.RemoveSharedEditor(AnEditor: TSourceEditor);
begin
  FSharedEditorList.Remove(AnEditor);
end;

procedure TSourceEditorSharedValues.SetActiveSharedEditor(AnEditor: TSourceEditor);
begin
  if FInGlobalUpdate > 0 then exit;
  // Move to the front, for UpdateCodetools (get undo-caret from correct synedit)
  FSharedEditorList.Remove(AnEditor);
  FSharedEditorList.Insert(0, AnEditor);
end;

function TSourceEditorSharedValues.SharedEditorCount: Integer;
begin
  Result := FSharedEditorList.Count;
end;

function TSourceEditorSharedValues.OtherSharedEditorCount: Integer;
begin
  Result := FSharedEditorList.Count - 1;
end;

function TSourceEditorSharedValues.GetExecutionLine: Integer;
begin
  if (FExecutionMark = nil) or (not FExecutionMark.Visible) then
    Result := -1
  else
    Result := FExecutionMark.Line;
end;

procedure TSourceEditorSharedValues.CreateExecutionMark;
begin
  FExecutionMark := TSourceMark.Create(SharedEditors[0], nil);
  SourceEditorMarks.Add(FExecutionMark);
  FExecutionMark.LineColorAttrib := ahaExecutionPoint;
  FExecutionMark.Priority := 1;
end;

procedure TSourceEditorSharedValues.SetExecutionLine(NewLine: integer);
var
  BrkMark: TSourceMark;
  CurELine: Integer;
begin
  CurELine := ExecutionLine;
  if CurELine = NewLine then
    exit;

  inc(UpdatingExecutionMark);
  try
    if CurELine >= 0 then begin
      BrkMark := SourceEditorMarks.FindBreakPointMark(SharedEditors[0], CurELine);
      if BrkMark <> nil then
        BrkMark.Visible := True;
    end;

    if (FExecutionMark = nil) then
      CreateExecutionMark;

    FExecutionMark.Visible := NewLine <> -1;

    if NewLine >= 0 then begin
      BrkMark := SourceEditorMarks.FindBreakPointMark(SharedEditors[0], NewLine);
      if BrkMark <> nil then
        BrkMark.Visible := False;
    end;

    FExecutionMark.Line := NewLine;
  finally
    dec(UpdatingExecutionMark);
  end;
end;

procedure TSourceEditorSharedValues.IncreaseIgnoreCodeBufferLock;
begin
  inc(FIgnoreCodeBufferLock);
end;

procedure TSourceEditorSharedValues.DecreaseIgnoreCodeBufferLock;
begin
  if FIgnoreCodeBufferLock<=0 then raise Exception.Create('unbalanced calls');
  dec(FIgnoreCodeBufferLock);
end;

function TSourceEditorSharedValues.NeedsUpdateCodeBuffer: boolean;
begin
  Result := TSynEditLines(SharedEditors[0].FEditor.Lines).TextChangeStamp
            <> FEditorStampCommitedToCodetools;
end;

procedure TSourceEditorSharedValues.UpdateCodeBuffer;
begin
  if not NeedsUpdateCodeBuffer then exit;
  {$IFDEF IDE_DEBUG}
  if FCodeBuffer=nil then begin
    debugln('*********** Oh, no: UpdateCodeBuffer ************ ');
  end;
  {$ENDIF}
  if FCodeBuffer=nil then exit;
  //DebugLn(['TSourceEditor.UpdateCodeBuffer ',FCodeBuffer.FileName]);
  IncreaseIgnoreCodeBufferLock;
  SynEditor.BeginUpdate(False);
  try
    FCodeBuffer.Assign(SynEditor.Lines);
    FEditorStampCommitedToCodetools:=(SynEditor.Lines as TSynEditLines).TextChangeStamp;
  finally
    SynEditor.EndUpdate;
    DecreaseIgnoreCodeBufferLock;
  end;
end;

function TSourceEditorSharedValues.Filename: string;
begin
  Result:=FCodeBuffer.Filename;
end;

procedure TSourceEditorSharedValues.ConnectScanner(Scanner: TLinkScanner);
// If this is an include file, several scanners might use this file
// all of them should store directives
begin
  if Scanner=nil then exit;
  if FLinkScanners.IndexOf(Scanner)>=0 then exit;
  //debugln(['TSourceEditorSharedValues.ConnectScanner ',Filename,' ',Scanner.MainFilename]);
  FLinkScanners.Add(Scanner);
  Scanner.DemandStoreDirectives;
end;

procedure TSourceEditorSharedValues.DisconnectScanner(Scanner: TLinkScanner);
var
  i: Integer;
begin
  if Scanner=nil then exit;
  i:=FLinkScanners.IndexOf(Scanner);
  if i<0 then exit;
  FLinkScanners.Delete(i);
  Scanner.ReleaseStoreDirectives;
  if Scanner=FMainLinkScanner then
    FMainLinkScanner:=nil;
end;

function TSourceEditorSharedValues.GetMainLinkScanner(Scan: boolean): TLinkScanner;
// Note: if this is an include file, the main scanner may change
var
  SrcEdit: TIDESynEditor;
begin
  Result:=FMainLinkScanner;
  if Result=nil then
  begin
    // create main scanner
    //debugln(['TSourceEditorSharedValues.GetMainLinkScanner fetching unit codebuffer ...']);
    if CodeBuffer=nil then begin
      // file is currently creating
      //debugln(['TSourceEditorSharedValues.GetMainLinkScanner CodeBuffer=nil']);
      exit;
    end;
    if SharedEditorCount=0 then exit;
    SrcEdit:=SharedEditors[0].EditorComponent;
    if SrcEdit=nil then exit;
    if not (SrcEdit.Highlighter is TSynPasSyn) then
    begin
      if Filename<>FLastWarnedMainLinkFilename then
      begin
        if FilenameIsPascalSource(Filename) then
          if ConsoleVerbosity>1 then
            debugln(['TSourceEditorSharedValues.GetMainLinkScanner not Pascal highlighted: ',Filename,' Highligther=',DbgSName(SrcEdit.Highlighter)]);
      end;
      FLastWarnedMainLinkFilename:=Filename;
      exit;
    end;
    if not CodeToolBoss.InitCurCodeTool(CodeBuffer) then
    begin
      if Filename<>FLastWarnedMainLinkFilename then
        debugln(['TSourceEditorSharedValues.GetMainLinkScanner failed to find the unit of ',Filename]);
      FLastWarnedMainLinkFilename:=Filename;
      exit;
    end;
    Result:=CodeToolBoss.CurCodeTool.Scanner;
    ConnectScanner(Result);
    FMainLinkScanner:=Result;
  end;
  if Scan and (FMainLinkScanner<>nil) then
  begin
    try
      FMainLinkScanner.Scan(lsrEnd,false);
    except
      on E: Exception do begin
        //CodeToolBoss.HandleException(e);
      end;
    end;
  end;
end;

constructor TSourceEditorSharedValues.Create;
begin
  FSharedEditorList := TFPList.Create;
  FExecutionMark := nil;
  FMarksRequested := False;
  FInGlobalUpdate := 0;
  FLinkScanners := TFPList.Create;
end;

destructor TSourceEditorSharedValues.Destroy;
var
  i: integer;
begin
  SourceEditorMarks.DeleteAllForEditorID(Self);
  CodeBuffer := nil;
  FreeAndNil(FSharedEditorList);
  if FLinkScanners<>nil then begin
    for i:=0 to FLinkScanners.Count-1 do
      TLinkScanner(FLinkScanners[i]).ReleaseStoreDirectives;
    FreeAndNil(FLinkScanners);
  end;
  // no need to care about ExecutionMark, it is removed with all other marks,
  // if the last SynEdit is destroyed (TSynEditMark.Destroy will free the SourceMark)
  inherited Destroy;
end;

{ TSourceEditor }

{ The constructor for @link(TSourceEditor).
  AOwner is the @link(TSourceNotebook)
  and the AParent is usually a page of a @link(TPageControl) }
constructor TSourceEditor.Create(AOwner: TComponent; AParent: TWinControl;
  ASharedEditor: TSourceEditor = nil);
Begin
  FInEditorChangedUpdating := False;

  if ASharedEditor = nil then
    FSharedValues := TSourceEditorSharedValues.Create
  else
    FSharedValues := ASharedEditor.FSharedValues;
  FSharedValues.AddSharedEditor(Self);

  inherited Create;
  FAOwner := AOwner;
  if (FAOwner<>nil) and (FAOwner is TSourceNotebook) then
    FSourceNoteBook:=TSourceNotebook(FAOwner)
  else
    FSourceNoteBook:=nil;

  FSyntaxHighlighterType:=lshNone;
  FErrorLine:=-1;
  FErrorColumn:=-1;
  FSyncroLockCount := 0;
  FLineInfoNotification := TIDELineInfoNotification.Create;
  FLineInfoNotification.AddReference;
  FLineInfoNotification.OnChange := @LineInfoNotificationChange;

  CreateEditor(AOwner,AParent);
  FProjectFileUpdatesNeeded := [];
  if ASharedEditor <> nil then begin
    PageName := ASharedEditor.PageName;
    FEditor.ShareTextBufferFrom(ASharedEditor.EditorComponent);
    FEditor.Highlighter := ASharedEditor.EditorComponent.Highlighter;
    if ASharedEditor.EditorComponent.Beautifier is TSynBeautifierPascal then
      FEditor.Beautifier := ASharedEditor.EditorComponent.Beautifier;
  end;

  FEditPlugin := TETSynPlugin.Create(FEditor);
  FEditPlugin.OnIsEnabled:=@IsFirstShared;
end;

destructor TSourceEditor.Destroy;
begin
  DebugLnEnter(SRCED_CLOSE, ['TSourceEditor.Destroy ']);
  Application.RemoveAsyncCalls(Self);
  if FInEditorChangedUpdating then begin
    debugln(['***** TSourceEditor.Destroy: FInEditorChangedUpdating was true']);
    DebugBoss.UnLockCommandProcessing;
    FInEditorChangedUpdating := False;
  end;
  PopupMenu := nil;
  if (FAOwner<>nil) and (FEditor<>nil) then begin
    UnbindEditor;
    FEditor.Visible:=false;
    FEditor.Parent:=nil;
    TSourceNotebook(FAOwner).ReleaseEditor(self, True);
    // free the synedit control after processing the events
    EditorComponent.Owner.RemoveComponent(EditorComponent);
    Application.ReleaseComponent(FEditor);
  end;
  FEditor:=nil;
  if (DebugBoss <> nil) and (DebugBoss.LineInfo <> nil) then
    DebugBoss.LineInfo.RemoveNotification(FLineInfoNotification);
  FLineInfoNotification.ReleaseReference;
  inherited Destroy;
  FSharedValues.RemoveSharedEditor(Self);
  if FSharedValues.SharedEditorCount = 0 then begin
    if FSharedValues.MarksRequested and (FSharedValues.MarksRequestedForFile <> '') then
      DebugBoss.LineInfo.Cancel(FSharedValues.MarksRequestedForFile);
    FreeAndNil(FSharedValues);
  end;
  DebugLnExit(SRCED_CLOSE, ['TSourceEditor.Destroy ']);
end;

{------------------------------G O T O   L I N E  -----------------------------}
function TSourceEditor.GotoLine(Value: Integer): Integer;
Var
  P: TPoint;
  NewTopLine: integer;
Begin
  Manager.AddJumpPointClicked(Self);
  P.X := 1;
  P.Y := Value;
  NewTopLine := P.Y - (FEditor.LinesInWindow div 2);
  if NewTopLine < 1 then NewTopLine:=1;
  FEditor.CaretXY := P;
  FEditor.TopLine := NewTopLine;
  Result:=FEditor.CaretY;
end;

procedure TSourceEditor.ShowGotoLineDialog;
var
  NewLeft: integer;
  NewTop: integer;
  dlg: TfrmGoto;
begin
  dlg := Manager.GotoDialog;
  dlg.Edit1.Text:='';
  GetDialogPosition(dlg.Width, dlg.Height, NewLeft, NewTop);
  dlg.SetBounds(NewLeft, NewTop, dlg.Width, dlg.Height);
  if (dlg.ShowModal = mrOK) then
    GotoLine(StrToIntDef(dlg.Edit1.Text,1));
  Self.FocusEditor;
end;

procedure TSourceEditor.ShowSmartHintForSourceAtCursor;
begin
  if Assigned(Manager) and Assigned(Manager.OnShowHintForSource) then
    Manager.OnShowHintForSource(Self,FEditor.LogicalCaretXY, False);
end;

procedure TSourceEditor.GetDialogPosition(Width, Height: integer;
  out Left, Top: integer);
var
  P: TPoint;
  ABounds: TRect;
begin
  with EditorComponent do
    P := ClientToScreen(Point(CaretXPix, CaretYPix));
  ABounds := Screen.MonitorFromPoint(P).WorkareaRect;
  Left := EditorComponent.ClientOrigin.X + (EditorComponent.Width - Width) div 2;
  Top := P.Y - Height - 3 * EditorComponent.LineHeight;
  if Top < ABounds.Top + 10 then
    Top := P.Y + 2 * EditorComponent.LineHeight;
  if Top + Height > ABounds.Bottom then
    Top := (ABounds.Bottom + ABounds.Top - Height) div 2;
  if Top < ABounds.Top then Top := ABounds.Top;
end;

procedure TSourceEditor.ActivateHint(ClientRect: TRect; const ABaseURL,
  AHint: string; AAutoShown: Boolean; AMouseOffset: Boolean);
var
  ScreenRect: TRect;
begin
  if SourceNotebook=nil then exit;
  ScreenRect.TopLeft:=EditorComponent.ClientToScreen(ClientRect.TopLeft);
  ScreenRect.BottomRight:=EditorComponent.ClientToScreen(ClientRect.BottomRight);
  Manager.ActivateHint(ScreenRect,ABaseURL,AHint,AAutoShown,AMouseOffset);
end;

{------------------------------S T A R T  F I N D-----------------------------}
procedure TSourceEditor.StartFindAndReplace(Replace:boolean);
var
  NewOptions: TSynSearchOptions;
  ALeft,ATop:integer;
  DlgResult: TModalResult;
  AState: TLazFindReplaceState;
begin
  LazFindReplaceDialog.SaveState(AState);
  LazFindReplaceDialog.ResetUserHistory;
  //debugln('TSourceEditor.StartFindAndReplace A LazFindReplaceDialog.FindText="',dbgstr(LazFindReplaceDialog.FindText),'"');
  if ReadOnly then Replace := False;
  NewOptions:=LazFindReplaceDialog.Options;
  if Replace then
    NewOptions := NewOptions + [ssoReplace, ssoReplaceAll]
  else
    NewOptions := NewOptions - [ssoReplace, ssoReplaceAll];
  NewOptions:=NewOptions-InputHistoriesSO.SaveOptions
    +InputHistoriesSO.FindOptions[EditorComponent.SelAvail];

  // Fill in history items
  LazFindReplaceDialog.TextToFindComboBox.Items.Assign(InputHistoriesSO.FindHistory);
  LazFindReplaceDialog.ReplaceTextComboBox.Items.Assign(InputHistoriesSO.ReplaceHistory);

  with EditorComponent do begin
    if EditorOpts.FindTextAtCursor then begin
      if SelAvail and (BlockBegin.Y = BlockEnd.Y) and
         (  ((ComparePoints(BlockBegin, LogicalCaretXY) <= 0) and
             (ComparePoints(BlockEnd, LogicalCaretXY) >= 0))  or
            ((ComparePoints(BlockBegin, LogicalCaretXY) >= 0) and
             (ComparePoints(BlockEnd, LogicalCaretXY) <= 0))
         )
      then begin
        //debugln('TSourceEditor.StartFindAndReplace B FindTextAtCursor SelAvail');
        LazFindReplaceDialog.FindText := SelText;
      end else begin
        //debugln('TSourceEditor.StartFindAndReplace B FindTextAtCursor not SelAvail');
        LazFindReplaceDialog.FindText := GetWordAtRowCol(LogicalCaretXY);
      end;
    end else begin
      //debugln('TSourceEditor.StartFindAndReplace B not FindTextAtCursor');
      LazFindReplaceDialog.FindText:='';
    end;
  end;
  LazFindReplaceDialog.EnableAutoComplete:=InputHistoriesSO.FindAutoComplete;
  // if there is no FindText, use the most recently used FindText
  if (LazFindReplaceDialog.FindText='') and (InputHistoriesSO.FindHistory.Count > 0) then
    LazFindReplaceDialog.FindText:=InputHistoriesSO.FindHistory[0];

  GetDialogPosition(LazFindReplaceDialog.Width,LazFindReplaceDialog.Height,ALeft,ATop);
  LazFindReplaceDialog.Left:=ALeft;
  LazFindReplaceDialog.Top:=ATop;

  LazFindReplaceDialog.Options := NewOptions;
  DlgResult:=LazFindReplaceDialog.ShowModal;
  InputHistoriesSO.FindOptions[EditorComponent.SelAvail]:=LazFindReplaceDialog.Options;
  InputHistoriesSO.FindAutoComplete:=LazFindReplaceDialog.EnableAutoComplete;
  if DlgResult = mrCancel then
  begin
    LazFindReplaceDialog.RestoreState(AState);
    exit;
  end;
  //debugln('TSourceEditor.StartFindAndReplace B LazFindReplaceDialog.FindText="',dbgstr(LazFindReplaceDialog.FindText),'"');

  Replace:=ssoReplace in LazFindReplaceDialog.Options;
  if Replace then
    InputHistoriesSO.AddToReplaceHistory(LazFindReplaceDialog.ReplaceText);
  InputHistoriesSO.AddToFindHistory(LazFindReplaceDialog.FindText);
  InputHistoriesSO.Save;
  DoFindAndReplace(LazFindReplaceDialog.FindText, LazFindReplaceDialog.ReplaceText,
    LazFindReplaceDialog.Options);
end;

procedure TSourceEditor.AskReplace(Sender: TObject; const ASearch,
  AReplace: string; Line, Column: integer; out Action: TSrcEditReplaceAction);
var
  SynAction: TSynReplaceAction;
begin
  SynAction:=raCancel;
  SourceNotebook.BringToFront;
  OnReplace(Sender, ASearch, AReplace, Line, Column, SynAction);
  case SynAction of
  raSkip: Action:=seraSkip;
  raReplaceAll: Action:=seraReplaceAll;
  raReplace: Action:=seraReplace;
  raCancel: Action:=seraCancel;
  else
    RaiseGDBException('TSourceEditor.AskReplace: inconsistency');
  end;
end;

{------------------------------F I N D  A G A I N ----------------------------}
procedure TSourceEditor.FindNextUTF8;
begin
  if snIncrementalFind in FSourceNoteBook.States then begin
    FSourceNoteBook.IncrementalSearch(True, False);
  end
  else if LazFindReplaceDialog.FindText = '' then begin
    StartFindAndReplace(False)
  end
  else begin
    DoFindAndReplace(LazFindReplaceDialog.FindText, LazFindReplaceDialog.ReplaceText,
      LazFindReplaceDialog.Options - [ssoEntireScope, ssoSelectedOnly]
                                   + [ssoFindContinue]);
  end;
End;

{---------------------------F I N D   P R E V I O U S ------------------------}
procedure TSourceEditor.FindPrevious;
var
  SrchOptions: TSynSearchOptions;
begin
  if snIncrementalFind in FSourceNoteBook.States then begin
    FSourceNoteBook.IncrementalSearch(True, True);
  end
  else if LazFindReplaceDialog.FindText = '' then begin
    // TODO: maybe start with default set to backwards direction? But StartFindAndReplace replaces it with input-history
    StartFindAndReplace(False);
  end else begin
    SrchOptions:=LazFindReplaceDialog.Options - [ssoEntireScope, ssoSelectedOnly]
                                              + [ssoFindContinue];
    if ssoBackwards in SrchOptions then
      SrchOptions := SrchOptions - [ssoBackwards]
    else
      SrchOptions := SrchOptions + [ssoBackwards];
    DoFindAndReplace(LazFindReplaceDialog.FindText, LazFindReplaceDialog.ReplaceText,
      SrchOptions);
  end;
end;

procedure TSourceEditor.FindNextWordOccurrence(DirectionForward: boolean);
var
  StartX, EndX: Integer;
  Flags: TSynSearchOptions;
  LogCaret: TPoint;
begin
  LogCaret:=EditorComponent.LogicalCaretXY;
  EditorComponent.GetWordBoundsAtRowCol(LogCaret,StartX,EndX);
  if EndX<=StartX then exit;
  Flags:=[ssoWholeWord];
  if DirectionForward then begin
    LogCaret.X:=EndX;
  end else begin
    LogCaret.X:=StartX;
    Include(Flags,ssoBackwards);
  end;
  EditorComponent.BeginUpdate(False);
  try
    EditorComponent.LogicalCaretXY:=LogCaret;
    EditorComponent.SearchReplace(EditorComponent.GetWordAtRowCol(LogCaret),
                                  '',Flags);
    CenterCursor(True);
  finally
    EditorComponent.EndUpdate;
  end;
end;

function TSourceEditor.DoFindAndReplace(aFindText, aReplaceText: String;
  anOptions: TSynSearchOptions): Integer;
var
  AText, ACaption: String;
  OldEntireScope, Again: Boolean;
begin
  Result:=0;
  if (ssoReplace in anOptions) and ReadOnly then begin
    DebugLn(['TSourceEditor.DoFindAndReplace Read only']);
    exit;
  end;
  if SourceNotebook<>nil then
    Manager.AddJumpPointClicked(Self);

  OldEntireScope := ssoEntireScope in anOptions;
  //do not show lisUESearchStringContinueBeg/lisUESearchStringContinueEnd if the caret is in the beginning/end
  if ssoBackwards in anOptions then
    Again := ((FEditor.CaretY >= FEditor.Lines.Count) and (FEditor.CaretX > Length(FEditor.LineText)))//caret in the last line and last character
  else
    Again := ((FEditor.CaretY = 1) and (FEditor.CaretX = 1));//caret at the top/left
  repeat
    try
      Result:=EditorComponent.SearchReplace(aFindText, aReplaceText, anOptions);
    except
      on E: ERegExpr do begin
        IDEMessageDialog(lisUEErrorInRegularExpression, E.Message,mtError,[mbCancel]);
        exit;
      end;
    end;
    if (Result = 0) and not (ssoReplaceAll in anOptions) then begin
      ACaption:=lisUENotFound;
      AText:=Format(lisUESearchStringNotFound, [Utf8EscapeControlChars(aFindText, emPascal)]);
      if not (Again or OldEntireScope) then begin
        if ssoBackwards in anOptions then
          AText:=AText+' '+lisUESearchStringContinueEnd
        else
          AText:=AText+' '+lisUESearchStringContinueBeg;
        Again:=MessageDlg(ACaption, AText, mtConfirmation, [mbYes,mbNo], 0) = mrYes;
        anOptions:=anOptions + [ssoEntireScope];
      end
      else begin
        Again := False;
        IDEMessageDialog(ACaption, AText, mtInformation, [mbOK]);
      end;
      if not Again then
        Manager.DeleteLastJumpPointClicked(Self);
    end
    else begin
      Again := False;
      CenterCursor(True);
    end;
  until not Again;
end;

procedure TSourceEditor.OnReplace(Sender: TObject; const ASearch, AReplace:
  string; Line, Column: integer; var Action: TSynReplaceAction);

  function Shorten(const s: string): string;
  const
    MAX_LEN=300;
  begin
    Result:=s;
    if Length(Result)>MAX_LEN then
      Result:=LeftStr(Result, MAX_LEN)+'...';
  end;

var a,x,y:integer;
  AText:AnsiString;
begin
  if FAOwner<>nil then
    TSourceNotebook(FAOwner).UpdateStatusBar;

  CenterCursor(True);
  CenterCursorHoriz(hcmSoftKeepEOL);

  AText:=Format(lisUEReplaceThisOccurrenceOfWith,[Shorten(ASearch),LineEnding,Shorten(AReplace)]);

  GetDialogPosition(300,150,X,Y);
  a:=MessageDlgPos(AText,mtconfirmation,
            [mbYes,mbYesToAll,mbNo,mbCancel],0,X,Y);

  case a of
    mrYes:Action:=raReplace;
    mrNo :Action:=raSkip;
    mrAll,mrYesToAll:Action:=raReplaceAll;
  else
    Action:=raCancel;
  end;
end;

//-----------------------------------------------------------------------------

procedure TSourceEditor.FocusEditor;
Begin
  DebugLnEnter(SRCED_PAGES, ['>> TSourceEditor.FocusEditor A ',PageName,' ',FEditor.Name]);
  IDEWindowCreators.ShowForm(SourceNotebook, true, vmOnlyMoveOffScreenToVisible);
  if FEditor.IsVisible then begin
    FEditor.SetFocus; // TODO: will cal EditorEnter, which does self.Activate  => maybe lock, and do here?
    FSharedValues.SetActiveSharedEditor(Self);
  end else begin
    debugln(SRCED_PAGES, ['TSourceEditor.FocusEditor not IsVisible: ',PageName,' ',FEditor.Name]);
  end;
  //DebugLn('TSourceEditor.FocusEditor ',dbgsName(FindOwnerControl(GetFocus)),' ',dbgs(GetFocus));
  DebugLnExit(SRCED_PAGES, ['<< TSourceEditor.FocusEditor END ',PageName,' ',FEditor.Name]);
end;

function TSourceEditor.GetReadOnly: Boolean;
Begin
  Result:=FEditor.ReadOnly;
End;

procedure TSourceEditor.SetReadOnly(const NewValue: boolean);
begin
  FEditor.ReadOnly:=NewValue;
end;

function TSourceEditor.Manager: TSourceEditorManager;
begin
  if FSourceNoteBook <> nil then
    Result := FSourceNoteBook.Manager
  else
    Result := nil;
end;

procedure TSourceEditor.MoveToWindow(AWindowIndex: Integer);
begin
  SourceNotebook.MoveEditor(PageIndex, AWindowIndex, -1)
end;

function TSourceEditor.GetSharedValues: TSourceEditorSharedValuesBase;
begin
  Result := FSharedValues;
end;

function TSourceEditor.IsSharedWith(AnOtherEditor: TSourceEditor): Boolean;
begin
  Result := (AnOtherEditor <> nil) and
            (AnOtherEditor.FSharedValues = FSharedValues);
end;

procedure TSourceEditor.BeforeCodeBufferReplace;
begin
  FTempTopLine := FEditor.TopLine;
  FTempCaret := FEditor.CaretXY;
end;

procedure TSourceEditor.AfterCodeBufferReplace;
begin
  if (FTempTopLine > FEditor.Lines.Count) or(FTempCaret.Y > FEditor.Lines.Count)
  then
    exit;
  FEditor.TopLine := FTempTopLine;
  FEditor.CaretXY := FTempCaret;
end;

procedure TSourceEditor.DoMultiCaretBeforeCommand(Sender: TObject;
  ACommand: TSynEditorCommand; var AnAction: TSynMultiCaretCommandAction;
  var AFlags: TSynMultiCaretCommandFlags);
begin
  if (FSourceNoteBook<>nil) and (snIncrementalFind in FSourceNoteBook.States) then begin
    AnAction := ccaClearCarets;
  end;

  case ACommand of
    ecToggleComment:
      if FEditor.SelAvail then
        AnAction := ccaAdjustCarets
      else
        AnAction := ccaRepeatCommandPerLine; // one per line
    ecInsertUserName,
    ecInsertDateTime,
    ecInsertChangeLogEntry,
    ecInsertCVSAuthor,
    ecInsertCVSDate,
    ecInsertCVSHeader,
    ecInsertCVSID,
    ecInsertCVSLog,
    ecInsertCVSName,
    ecInsertCVSRevision,
    ecInsertCVSSource,
    ecInsertGUID,
    ecInsertFilename:
      AnAction := ccaRepeatCommand;
  end;
end;

procedure TSourceEditor.ProcessCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
// these are normal commands for synedit (lower than ecUserFirst),
// define extra actions here
// for non synedit keys (bigger than ecUserFirst) use ProcessUserCommand
var
  AddChar: Boolean;
  s: String;
  i: Integer;
begin
  //DebugLn('TSourceEditor.ProcessCommand Command=',dbgs(Command));
  FSharedValues.SetActiveSharedEditor(Self);
  AutoStartCompletionBoxTimer.AutoEnabled:=false;

  if (Command=ecChar) and (AChar=#27) then begin
    // close hint windows
    if (CodeContextFrm<>nil) then
      CodeContextFrm.Hide;
    if (SrcEditHintWindow<>nil) then
      SrcEditHintWindow.Hide;
  end;
  Manager.HideHint;

  if (FSourceNoteBook<>nil)
  and (snIncrementalFind in FSourceNoteBook.States) then begin
    case Command of
    ecChar:
      begin
        if AChar=#27 then begin
          if (CodeContextFrm<>nil) then
            CodeContextFrm.Hide;

          FSourceNoteBook.IncrementalSearchStr:='';
        end else
          FSourceNoteBook.IncrementalSearchStr:=
            FSourceNoteBook.IncrementalSearchStr+AChar;
        Command:=ecNone;
      end;

    ecDeleteLastChar:
      begin
        i := length(FSourceNoteBook.IncrementalSearchStr);
        i := UTF8FindNearestCharStart(PChar(FSourceNoteBook.IncrementalSearchStr), i, i-1);
        FSourceNoteBook.IncrementalSearchStr:= LeftStr(FSourceNoteBook.IncrementalSearchStr, i);
        Command:=ecNone;
      end;

    ecLineBreak:
      begin
        FSourceNoteBook.EndIncrementalFind;
        Command:=ecNone;
      end;

    ecPaste:
      begin
        s:=Clipboard.AsText;
        s:=copy(s,1,EditorOpts.RightMargin);
        FSourceNoteBook.IncrementalSearchStr:=
          FSourceNoteBook.IncrementalSearchStr+s;
        Command:=ecNone;
      end;

    ecScrollUp, ecScrollDown, ecScrollLeft, ecScrollRight: ; // ignore

    else
      FSourceNoteBook.EndIncrementalFind;
    end;
  end;

  case Command of

  ecSelEditorTop, ecSelEditorBottom, ecEditorTop, ecEditorBottom:
    begin
      if (FaOwner<>nil) and (not FEditor.IsInMultiCaretRepeatExecution) then
        Manager.AddJumpPointClicked(Self);
    end;

  ecCopy,ecCut,ecCopyAdd,ecCutAdd:
    begin
      if (not FEditor.SelAvail) then begin
        // nothing selected
        if EditorOpts.CopyWordAtCursorOnCopyNone then begin
          FEditor.SelectWord;
        end;
      end;
    end;

  ecTab:
    begin
      AddChar:=true;
      if AutoCompleteChar(aChar,AddChar,acoTab) then begin
        // completed
      end;
      if not AddChar then Command:=ecNone;
    end;

  ecChar:
    begin
      AddChar:=true;
      //debugln(['TSourceEditor.ProcessCommand AChar="',AChar,'" AutoIdentifierCompletion=',dbgs(EditorOpts.AutoIdentifierCompletion),' Interval=',AutoStartCompletionBoxTimer.Interval,' ',Dbgs(FEditor.CaretXY),' ',FEditor.IsIdentChar(aChar)]);
      if (aChar=' ') and AutoCompleteChar(aChar,AddChar,acoSpace) then begin
        // completed
      end
      else if (not FEditor.IsIdentChar(aChar))
      and AutoCompleteChar(aChar,AddChar,acoWordEnd) then begin
        // completed
      end else if CodeToolsOpts.IdentComplAutoStartAfterPoint then begin
        // store caret position to detect caret changes
        SourceCompletionCaretXY:=FEditor.CaretXY;
        // add the char
        inc(SourceCompletionCaretXY.x,length(AChar));
        AutoStartCompletionBoxTimer.AutoEnabled:=true;
      end;
      //DebugLn(['TSourceEditor.ProcessCommand ecChar AddChar=',AddChar]);
      if not AddChar then Command:=ecNone;
    end;

  ecLineBreak:
    begin
      AddChar:=true;
      if AutoCompleteChar(aChar,AddChar,acoLineBreak) then ;
      //DebugLn(['TSourceEditor.ProcessCommand ecLineBreak AddChar=',AddChar,' EditorOpts.AutoBlockCompletion=',EditorOpts.AutoBlockCompletion]);
      if not AddChar then Command:=ecNone;
      if EditorOpts.AutoBlockCompletion then
        AutoCompleteBlock;
    end;

  ecPrevBookmark: // Note: book mark commands lower than ecUserFirst must be handled here
    if Assigned(Manager.OnGotoBookmark) then
      Manager.OnGotoBookmark(Self, -1, True);

  ecNextBookmark:
    if Assigned(Manager.OnGotoBookmark) then
      Manager.OnGotoBookmark(Self, -1, False);

  ecGotoMarker0..ecGotoMarker9:
    if Assigned(Manager.OnGotoBookmark) then
      Manager.OnGotoBookmark(Self, Command - ecGotoMarker0, False);

  ecSetMarker0..ecSetMarker9:
    if Assigned(Manager.OnSetBookmark) then
      Manager.OnSetBookmark(Self, Command - ecSetMarker0, False);

  ecToggleMarker0..ecToggleMarker9:
    if Assigned(Manager.OnSetBookmark) then
      Manager.OnSetBookmark(Self, Command - ecToggleMarker0, True);

  ecSelectAll:
    Manager.AddJumpPointClicked(Self);

  end;
  //debugln('TSourceEditor.ProcessCommand B IdentCompletionTimer.AutoEnabled=',dbgs(AutoStartCompletionBoxTimer.AutoEnabled));
end;

procedure TSourceEditor.ProcessUserCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
// these are the keys above ecUserFirst
// define all extra keys here, that should not be handled by synedit
var
  Handled: boolean;
  i,x,y: Integer;
Begin
  //debugln('TSourceEditor.ProcessUserCommand A ',dbgs(Command));
  FSharedValues.SetActiveSharedEditor(Self);
  Handled:=true;
  CheckActiveWindow;

  case Command of
  ecMultiPaste:                MultiPasteText;
  ecContextHelp:               FindHelpForSourceAtCursor;
  ecSmartHint:                 ShowSmartHintForSourceAtCursor;
  ecIdentCompletion: StartIdentCompletionBox(CodeToolsOpts.IdentComplJumpToError,true);
  ecShowCodeContext: SourceNotebook.StartShowCodeContext(CodeToolsOpts.IdentComplJumpToError);
  ecWordCompletion:            StartWordCompletionBox;
  ecFind:                      StartFindAndReplace(false);
  ecFindNext:                  FindNextUTF8;
  ecFindPrevious:              FindPrevious;
  ecIncrementalFind: if FSourceNoteBook<>nil then FSourceNoteBook.BeginIncrementalFind;
  ecReplace:                   StartFindAndReplace(true);
  ecGotoLineNumber:            ShowGotoLineDialog;
  ecFindNextWordOccurrence:    FindNextWordOccurrence(true);
  ecFindPrevWordOccurrence:    FindNextWordOccurrence(false);
  ecSelectionEnclose:          EncloseSelection;
  ecSelectionUpperCase:        UpperCaseSelection;
  ecSelectionLowerCase:        LowerCaseSelection;
  ecSelectionSwapCase:         SwapCaseSelection;
  ecSelectionTabs2Spaces:      TabsToSpacesInSelection;
  ecSelectionComment:          CommentSelection;
  ecSelectionUnComment:        UncommentSelection;
  ecToggleComment:             ToggleCommentSelection;
  ecSelectionEncloseIFDEF:     ConditionalSelection;
  ecSelectionSort:             SortSelection;
  ecSelectionBreakLines:       BreakLinesInSelection;
  ecInvertAssignment:          InvertAssignment;
  ecSelectToBrace:             SelectToBrace;
  ecSelectCodeBlock:           SelectCodeBlock;
  ecSelectLine:                SelectLine;
  ecSelectWord:                SelectWord;
  ecSelectParagraph:           SelectParagraph;
  ecInsertCharacter:           InsertCharacterFromMap;
  ecInsertGPLNotice:           InsertGPLNotice(comtDefault,false);
  ecInsertGPLNoticeTranslated: InsertGPLNotice(comtDefault,true);
  ecInsertLGPLNotice:          InsertLGPLNotice(comtDefault,false);
  ecInsertLGPLNoticeTranslated:InsertLGPLNotice(comtDefault,true);
  ecInsertModifiedLGPLNotice:  InsertModifiedLGPLNotice(comtDefault,false);
  ecInsertModifiedLGPLNoticeTranslated: InsertModifiedLGPLNotice(comtDefault,true);
  ecInsertMITNotice:           InsertMITNotice(comtDefault,false);
  ecInsertMITNoticeTranslated: InsertMITNotice(comtDefault,true);
  ecInsertUserName:            InsertUsername;
  ecInsertDateTime:            InsertDateTime;
  ecInsertChangeLogEntry:      InsertChangeLogEntry;
  ecInsertCVSAuthor:           InsertCVSKeyword('Author');
  ecInsertCVSDate:             InsertCVSKeyword('Date');
  ecInsertCVSHeader:           InsertCVSKeyword('Header');
  ecInsertCVSID:               InsertCVSKeyword('ID');
  ecInsertCVSLog:              InsertCVSKeyword('Log');
  ecInsertCVSName:             InsertCVSKeyword('Name');
  ecInsertCVSRevision:         InsertCVSKeyword('Revision');
  ecInsertCVSSource:           InsertCVSKeyword('Source');
  ecInsertGUID:                InsertGUID;
  ecInsertFilename:            InsertFilename;
  ecLockEditor:                IsLocked := not IsLocked;
  ecSynMacroPlay: begin
      If ActiveEditorMacro = EditorMacroForRecording then begin
        if EditorMacroForRecording.State = emRecording
        then EditorMacroForRecording.Pause
        else EditorMacroForRecording.Resume;
      end
      else
        if (SelectedEditorMacro <> nil) and
           (SelectedEditorMacro.State = emStopped)
        then
          SelectedEditorMacro.PlaybackMacro(FEditor);
    end;
  ecSynMacroRecord: begin
      If ActiveEditorMacro = nil then
        EditorMacroForRecording.RecordMacro(FEditor)
      else
      If ActiveEditorMacro = EditorMacroForRecording then
        EditorMacroForRecording.Stop;
    end;
  ecClearBookmarkForFile: begin
      if Assigned(Manager) and Assigned(Manager.OnClearBookmarkId) then
        for i in TBookmarkNumRange do
          if EditorComponent.GetBookMark(i,x{%H-},y{%H-}) then
            Manager.OnClearBookmarkId(Self, i);
    end;

  ecCloseOtherTabs: Application.QueueAsyncCall(@Manager.CloseOtherPagesClickedAsync, PtrInt(SourceNotebook.GetNoteBookPage(SourceNotebook.FindPageWithEditor(Self))));
  ecCloseRightTabs: Application.QueueAsyncCall(@Manager.CloseRightPagesClickedAsync, PtrInt(SourceNotebook.GetNoteBookPage(SourceNotebook.FindPageWithEditor(Self))));

  else
    begin
      Handled:=false;
      if FaOwner<>nil then
        TSourceNotebook(FaOwner).ProcessParentCommand(self,Command,aChar,Data,
                        Handled);
    end;
  end;  //case
  if Handled then Command:=ecNone;
end;

procedure TSourceEditor.UserCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
// called after the source editor processed a key
var Handled: boolean;
begin
  Handled:=true;
  case Command of

  ecNone: ;

  ecChar:
    begin
      if AutoBlockCompleteChar(AChar) then
        Handled:=true;
      if EditorOpts.AutoDisplayFunctionPrototypes then
         if (aChar = '(') or (aChar = ',') then
            SourceNotebook.StartShowCodeContext(False);
    end;

  else
    begin
      Handled:=false;
      if FaOwner<>nil then
        TSourceNotebook(FaOwner).ParentCommandProcessed(Self,Command,aChar,Data,
                                                        Handled);
    end;
  end;
  if Handled then Command:=ecNone;
end;

procedure TSourceEditor.EditorStatusChanged(Sender: TObject;
  Changes: TSynStatusChanges);
Begin
  If Assigned(OnEditorChange) then
    OnEditorChange(Sender, Changes);
  UpdatePageName;
  if Changes * [scCaretX, scCaretY, scSelection] <> [] then
    IDECommandList.PostponeUpdateEvents;
end;

function TSourceEditor.SelectionAvailable: boolean;
begin
  Result := EditorComponent.SelAvail;
end;

function TSourceEditor.GetText(OnlySelection: boolean): string;
begin
  if OnlySelection then
    Result:=EditorComponent.SelText
  else
    Result:=EditorComponent.Lines.Text;
end;

{-------------------------------------------------------------------------------
  method TSourceEditor.UpperCaseSelection

  Turns current text selection uppercase.
-------------------------------------------------------------------------------}
procedure TSourceEditor.UpperCaseSelection;
begin
  if ReadOnly then exit;
  if not EditorComponent.SelAvail then exit;
  FEditor.SetTextBetweenPoints(FEditor.BlockBegin, FEditor.BlockEnd,
                               UTF8UpperCase(EditorComponent.SelText),
                               [setSelect], scamIgnore, smaKeep, smCurrent
                              );
end;

{-------------------------------------------------------------------------------
  method TSourceEditor.LowerCaseSelection

  Turns current text selection lowercase.
-------------------------------------------------------------------------------}
procedure TSourceEditor.LowerCaseSelection;
begin
  if ReadOnly then exit;
  if not EditorComponent.SelAvail then exit;
  FEditor.SetTextBetweenPoints(FEditor.BlockBegin, FEditor.BlockEnd,
                               UTF8LowerCase(EditorComponent.SelText),
                               [setSelect], scamIgnore, smaKeep, smCurrent
                              );
end;

procedure TSourceEditor.SwapCaseSelection;
begin
  if ReadOnly then exit;
  if not EditorComponent.SelAvail then exit;
  FEditor.SetTextBetweenPoints(FEditor.BlockBegin, FEditor.BlockEnd,
                               UTF8SwapCase(EditorComponent.SelText),
                               [setSelect], scamIgnore, smaKeep, smCurrent
                              );
end;

{-------------------------------------------------------------------------------
  method TSourceEditor.TabsToSpacesInSelection

  Convert all tabs into spaces in current text selection.
-------------------------------------------------------------------------------}
procedure TSourceEditor.TabsToSpacesInSelection;
begin
  if ReadOnly then exit;
  if not EditorComponent.SelAvail then exit;
  FEditor.SetTextBetweenPoints(FEditor.BlockBegin, FEditor.BlockEnd,
                               TabsToSpaces(EditorComponent.SelText, EditorComponent.TabWidth, FEditor.UseUTF8),
                               [setSelect], scamAdjust, smaKeep, smCurrent
                              );
end;

procedure TSourceEditor.CommentSelection;
begin
  UpdateCommentSelection(True, False);
end;

procedure TSourceEditor.UncommentSelection;
begin
  UpdateCommentSelection(False, False);
end;

procedure TSourceEditor.ToggleCommentSelection;
begin
  UpdateCommentSelection(False, True);
end;

procedure TSourceEditor.UpdateCommentSelection(CommentOn, Toggle: Boolean);
var
  OldCaretPos, OldBlockStart, OldBlockEnd: TPoint;
  WasSelAvail: Boolean;
  WasSelMode: TSynSelectionMode;
  BlockBeginLine: Integer;
  BlockEndLine: Integer;
  CommonIndent: Integer;

  function FirstNonBlankPos(const Text: String; Start: Integer = 1): Integer;
  var
    i: Integer;
  begin
    for i := Start to Length(Text) do
      if (Text[i] <> #32) and (Text[i] <> #9) then
        exit(i);
    Result := -1;
  end;

  function MinCommonIndent: Integer;
  var
    i, j: Integer;
  begin
    If CommonIndent = 0 then begin
      CommonIndent := Max(FirstNonBlankPos(FEditor.Lines[BlockBeginLine - 1]), 1);
      for i := BlockBeginLine + 1 to BlockEndLine do begin
        j := FirstNonBlankPos(FEditor.Lines[i - 1]);
        if (j < CommonIndent) and (j > 0) then
          CommonIndent := j;
      end;
    end;
    Result := CommonIndent;
  end;

  function InsertPos(ALine: Integer): Integer;
  begin
    if not WasSelAvail then
      Result := MinCommonIndent
    else case WasSelMode of
      smColumn: // CommonIndent is not used otherwise
        begin
          if CommonIndent = 0 then
            CommonIndent := Min(FEditor.LogicalToPhysicalPos(OldBlockStart).X,
                                FEditor.LogicalToPhysicalPos(OldBlockEnd).X);
          Result := FEditor.PhysicalToLogicalPos(Point(CommonIndent, ALine)).X;
        end;
      smNormal:
        begin
          Result := MinCommonIndent;
        end;
       else
         Result := 1;
    end;
  end;

  function DeletePos(ALine: Integer): Integer;
  var
    line: String;
  begin
    line := FEditor.Lines[ALine - 1];
    Result := FirstNonBlankPos(line, InsertPos(ALine));
    if (WasSelMode = smColumn) and((Result < 1) or (Result > length(line) - 1))
    then
      Result := length(line) - 1;
    Result := Max(1, Result);
    if (Length(line) < Result +1) or
       (line[Result] <> '/') or (line[Result+1] <> '/') then
      Result := -1;
  end;

var
  i: Integer;
  NonBlankStart: Integer;
begin
  if ReadOnly then exit;
  OldCaretPos   := FEditor.CaretXY;
  OldBlockStart := FEditor.BlockBegin;
  OldBlockEnd   := FEditor.BlockEnd;
  WasSelAvail := FEditor.SelAvail;
  WasSelMode  := FEditor.SelectionMode;
  CommonIndent := 0;

  BlockBeginLine := OldBlockStart.Y;
  BlockEndLine := OldBlockEnd.Y;
  if (OldBlockEnd.X = 1) and (BlockEndLine > BlockBeginLine) and (FEditor.SelectionMode <> smLine) then
    Dec(BlockEndLine);

  if Toggle then begin
    CommentOn := False;
    for i := BlockBeginLine to BlockEndLine do
      if DeletePos(i) < 0 then begin
        CommentOn := True;
        break;
      end;
  end;

  BeginUpdate;
  BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.UpdateCommentSelection'){$ENDIF};
  FEditor.SelectionMode := smNormal;

  if CommentOn then begin
    for i := BlockEndLine downto BlockBeginLine do
      FEditor.TextBetweenPoints[Point(InsertPos(i), i), Point(InsertPos(i), i)] := '//';
    if OldCaretPos.X > InsertPos(OldCaretPos.Y) then
      OldCaretPos.x := OldCaretPos.X + 2;
    if OldBlockStart.X > InsertPos(OldBlockStart.Y) then
      OldBlockStart.X := OldBlockStart.X + 2;
    if OldBlockEnd.X > InsertPos(OldBlockEnd.Y) then
      OldBlockEnd.X := OldBlockEnd.X + 2;
  end
  else begin
    for i := BlockEndLine downto BlockBeginLine do
    begin
      NonBlankStart := DeletePos(i);
      if NonBlankStart < 1 then continue;
      FEditor.TextBetweenPoints[Point(NonBlankStart, i), Point(NonBlankStart + 2, i)] := '';
      if (OldCaretPos.Y = i) and (OldCaretPos.X > NonBlankStart) then
        OldCaretPos.x := Max(OldCaretPos.X - 2, NonBlankStart);
      if (OldBlockStart.Y = i) and (OldBlockStart.X > NonBlankStart) then
        OldBlockStart.X := Max(OldBlockStart.X - 2, NonBlankStart);
      if (OldBlockEnd.Y = i) and (OldBlockEnd.X > NonBlankStart) then
        OldBlockEnd.X := Max(OldBlockEnd.X - 2, NonBlankStart);
    end;
  end;

  EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.UpdateCommentSelection'){$ENDIF};
  EndUpdate;

  FEditor.CaretXY := OldCaretPos;
  FEditor.BlockBegin := OldBlockStart;
  FEditor.BlockEnd := OldBlockEnd;
  FEditor.SelectionMode := WasSelMode;
end;

procedure TSourceEditor.ConditionalSelection;
var
  IsPascal: Boolean;
  i: Integer;
  P: TPoint;
begin
  if ReadOnly then exit;
  FEditor.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.ConditionalSelection'){$ENDIF};
  try
    if not EditorComponent.SelAvail then begin
      P.Y := FEditor.CaretY;
      P.X := 1;
      FEditor.BlockBegin := P;
      Inc(P.Y);
      FEditor.BlockEnd := P;
    end;
    // ToDo: replace step by step to keep bookmarks and breakpoints
    IsPascal := True;
    i:=EditorOpts.HighlighterList.FindByHighlighter(FEditor.Highlighter);
    if i>=0 then
      IsPascal := EditorOpts.HighlighterList[i].DefaultCommentType <> comtCPP;
    // will show modal dialog - must not be in Editor.BeginUpdate block, or painting will not work
    FEditor.SelText:=EncloseInsideIFDEF(EditorComponent.SelText,IsPascal);
  finally
    FEditor.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.ConditionalSelection'){$ENDIF};
  end;
end;

procedure TSourceEditor.SortSelection;
var
  OldSelText, NewSortedText: string;
begin
  if ReadOnly then exit;
  OldSelText:=EditorComponent.SelText;
  if OldSelText='' then exit;
  if ShowSortSelectionDialog(OldSelText,EditorComponent.Highlighter,
                             NewSortedText)=mrOk
  then
    EditorComponent.SelText:=NewSortedText;
end;

procedure TSourceEditor.BreakLinesInSelection;
var
  OldSelection: String;
begin
  if ReadOnly then exit;
  if not EditorComponent.SelAvail then exit;
  FEditor.BeginUpdate;
  FEditor.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.BreakLinesInSelection'){$ENDIF};
  // ToDo: replace step by step to keep bookmarks and breakpoints
  try
    OldSelection:=EditorComponent.SelText;
    FEditor.SelText:=BreakLinesInText(OldSelection,FEditor.RightEdge);
  finally
    FEditor.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.BreakLinesInSelection'){$ENDIF};
    FEditor.EndUpdate;
  end;
end;

procedure TSourceEditor.InvertAssignment;
begin
  if ReadOnly then exit;
  if not EditorComponent.SelAvail then exit;
  FEditor.BeginUpdate;
  FEditor.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.InvertAssignment'){$ENDIF};
  try
    // ToDo: replace step by step to keep bookmarks and breakpoints
    FEditor.SelText := InvertAssignTool.InvertAssignment(FEditor.SelText);
  finally
    FEditor.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.InvertAssignment'){$ENDIF};
    FEditor.EndUpdate;
  end;
end;

procedure TSourceEditor.SelectToBrace;
begin
  EditorComponent.SelectToBrace;
end;

procedure TSourceEditor.SelectCodeBlock;
begin
  // ToDo:
  DebugLn('TSourceEditor.SelectCodeBlock: not implemented yet');
end;

procedure TSourceEditor.SelectWord;
begin
  EditorComponent.SelectWord;
end;

procedure TSourceEditor.SelectLine;
begin
  EditorComponent.SelectLine;
end;

procedure TSourceEditor.SelectParagraph;
begin
  EditorComponent.SelectParagraph;
end;

function TSourceEditor.CommentText(const Txt: string; CommentType: TCommentType
  ): string;
var
  i: integer;
begin
  Result:=Txt;
  case CommentType of
    comtNone: exit;
    comtDefault:
      begin
        i:=EditorOpts.HighlighterList.FindByHighlighter(FEditor.Highlighter);
        if i>=0 then
          CommentType:=EditorOpts.HighlighterList[i].DefaultCommentType;
      end;
  end;
  Result:=LazStringUtils.CommentText(Txt,CommentType);
end;

procedure TSourceEditor.InsertCharacterFromMap;
begin
  ShowCharacterMap(@SourceNotebook.InsertCharacter);
end;

procedure TSourceEditor.InsertLicenseNotice(const Notice: string;
  CommentType: TCommentType);
var
  Txt: string;
begin
  if ReadOnly then Exit;
  Txt:=CommentText(BreakString(Notice, FEditor.RightEdge-2,0),CommentType);
  FEditor.InsertTextAtCaret(Txt);
end;

procedure TSourceEditor.InsertGPLNotice(CommentType: TCommentType;
  Translated: boolean);
var
  s: String;
begin
  if Translated then
    s:=lisGPLNotice
  else
    s:=EnglishGPLNotice;
  InsertLicenseNotice(s, CommentType);
end;

procedure TSourceEditor.InsertLGPLNotice(CommentType: TCommentType;
  Translated: boolean);
var
  s: String;
begin
  if Translated then
    s:=lisLGPLNotice
  else
    s:=EnglishLGPLNotice;
  InsertLicenseNotice(s, CommentType);
end;

procedure TSourceEditor.InsertModifiedLGPLNotice(CommentType: TCommentType;
  Translated: boolean);
var
  s: String;
begin
  if Translated then
    s:=lisModifiedLGPLNotice
  else
    s:=EnglishModifiedLGPLNotice;
  InsertLicenseNotice(s, CommentType);
end;

procedure TSourceEditor.InsertMITNotice(CommentType: TCommentType;
  Translated: boolean);
var
  s: String;
begin
  if Translated then
    s:=lisMITNotice
  else
    s:=EnglishMITNotice;
  InsertLicenseNotice(s, CommentType);
end;

procedure TSourceEditor.InsertUsername;
begin
  if ReadOnly then Exit;
  FEditor.InsertTextAtCaret(GetCurrentUserName);
end;

procedure TSourceEditor.InsertDateTime;
begin
  if ReadOnly then Exit;
  FEditor.InsertTextAtCaret(DateTimeToStr(now));
end;

procedure TSourceEditor.InsertChangeLogEntry;
var s: string;
begin
  if ReadOnly then Exit;
  s:=DateToStr(now)+'   '+GetCurrentUserName+' '+GetCurrentMailAddress;
  FEditor.InsertTextAtCaret(s);
end;

procedure TSourceEditor.InsertCVSKeyword(const AKeyWord: string);
begin
  if ReadOnly then Exit;
  FEditor.InsertTextAtCaret('$'+AKeyWord+'$'+LineEnding);
end;

procedure TSourceEditor.InsertGUID;
const
  cGUID = '[''%s'']'; // The format of the GUID used for Interfaces
var
  lGUID: TGUID;
begin
  if ReadOnly then Exit;
  CreateGUID(lGUID);
  FEditor.InsertTextAtCaret(Format(cGUID, [GUIDToString(lGUID)]));
end;

procedure TSourceEditor.InsertFilename;
var
  Dlg: TIDEOpenDialog;
begin
  if ReadOnly then Exit;
  Dlg:=IDEOpenDialogClass.Create(nil);
  try
    InitIDEFileDialog(Dlg);
    Dlg.Title:=lisSelectFile;
    if not Dlg.Execute then exit;
    FEditor.InsertTextAtCaret(Dlg.FileName);
  finally
    Dlg.Free;
  end;
end;

function TSourceEditor.GetSelEnd: Integer;
begin
  Result:=FEditor.SelEnd;
end;

function TSourceEditor.GetSelStart: Integer;
begin
  Result:=FEditor.SelStart;
end;

procedure TSourceEditor.SetSelEnd(const AValue: Integer);
begin
  FEditor.SelEnd:=AValue;
end;

procedure TSourceEditor.SetSelStart(const AValue: Integer);
begin
  FEditor.SelStart:=AValue;
end;

function TSourceEditor.GetSelection: string;
begin
  Result:=FEditor.SelText;
end;

procedure TSourceEditor.SetSelection(const AValue: string);
begin
  FEditor.SelText:=AValue;
end;

procedure TSourceEditor.CopyToClipboard;
begin
  FEditor.CopyToClipboard;
end;

procedure TSourceEditor.CutToClipboard;
begin
  FEditor.CutToClipboard;
end;

function TSourceEditor.GetBookMark(BookMark: Integer; out X, Y: Integer): Boolean;
begin
  X := 0; Y := 0;
  Result := FEditor.GetBookMark(BookMark, X, Y);
end;

procedure TSourceEditor.SetBookMark(BookMark: Integer; X, Y: Integer);
begin
  FEditor.SetBookMark(BookMark, X, Y);
end;

procedure TSourceEditor.ExportAsHtml(AFileName: String);
var
  Html: TSynExporterHTML;
begin
  Html := TSynExporterHTML.Create(nil);
  try
    Html.Clear;
    Html.ExportAsText := True;
    Html.Highlighter := FEditor.Highlighter;
    Html.Title := PageName;
    Html.ExportAll(FEditor.Lines);
    Html.SaveToFile(AFileName);
  finally
    Html.Free;
  end;
end;

procedure TSourceEditor.FindHelpForSourceAtCursor;
begin
  //DebugLn('TSourceEditor.FindHelpForSourceAtCursor A');
  ShowHelpOrErrorForSourcePosition(Filename,FEditor.LogicalCaretXY);
end;

procedure TSourceEditor.OnGutterClick(Sender: TObject; X, Y, Line: integer;
  Mark: TSynEditMark);
var
  Marks: PSourceMark;
  i, MarkCount: Integer;
  BreakFound: Boolean;
  Ctrl: Boolean;
  ABrkPoint: TIDEBreakPoint;
  Mrk: TSourceMark;
begin
  // create or delete breakpoint
  // find breakpoint Mark at line
  Marks := nil;
  Ctrl := SYNEDIT_LINK_MODIFIER in GetKeyShiftState;
  try
    SourceEditorMarks.GetMarksForLine(Self, Line, Marks, MarkCount);
    BreakFound := False;
    for i := 0 to MarkCount - 1 do
    begin
      Mrk := Marks[i];
      if Mrk.IsBreakPoint and
        (Mrk.Data <> nil) and (Mrk.Data is TIDEBreakPoint)
      then begin
        BreakFound := True;
        if Ctrl then
          TIDEBreakPoint(Mrk.Data).Enabled := not TIDEBreakPoint(Mrk.Data).Enabled
        else
          DebugBoss.DoDeleteBreakPointAtMark(Mrk)
      end;
    end;
  finally
    FreeMem(Marks);
  end;

  if not BreakFound then begin
    DebugBoss.LockCommandProcessing;
    try
      DebugBoss.DoCreateBreakPoint(Filename, Line, True, ABrkPoint, True);
      if Ctrl and (ABrkPoint <> nil)
      then ABrkPoint.Enabled := False;
    finally
      ABrkPoint.EndUpdate;
      DebugBoss.UnLockCommandProcessing;
    end;
  end;
end;

procedure TSourceEditor.OnEditorSpecialLineColor(Sender: TObject; Line: integer;
  var Special: boolean; Markup: TSynSelectedColor);
var
  i:integer;
  aha: TAdditionalHilightAttribute;
  CurMarks: PSourceMark;
  CurMarkCount: integer;
  CurFG: TColor;
  CurBG: TColor;
begin
  aha := ahaNone;
  Special := False;

  if ErrorLine = Line
  then begin
    aha := ahaErrorLine
  end
  else begin
    SourceEditorMarks.GetMarksForLine(Self, Line, CurMarks, CurMarkCount);
    if CurMarkCount > 0 then
    begin
      for i := 0 to CurMarkCount - 1 do
      begin
        if not CurMarks[i].Visible then
          Continue;
        // check highlight attribute
        aha := CurMarks[i].LineColorAttrib;
        if aha <> ahaNone then Break;

        // check custom colors
        CurFG := CurMarks[i].LineColorForeGround;
        CurBG := CurMarks[i].LineColorBackGround;
        if (CurFG <> clNone) or (CurBG <> clNone) then
        begin
          Markup.Foreground := CurFG;
          Markup.Background := CurBG;
          Special := True;
          break;
        end;
      end;
      // clean up
      FreeMem(CurMarks);
    end;
  end;

  if aha <> ahaNone
  then begin
    Special := True;
    EditorOpts.SetMarkupColor(TCustomSynEdit(Sender).Highlighter, aha, Markup);
  end;
end;

procedure TSourceEditor.SetSyntaxHighlighterType(
  ASyntaxHighlighterType: TLazSyntaxHighlighter);
var
  HlIsPas, OldHlIsPas: Boolean;
begin
  if (ASyntaxHighlighterType=fSyntaxHighlighterType)
  and ((FEditor.Highlighter<>nil) = EditorOpts.UseSyntaxHighlight) then exit;

  OldHlIsPas := FEditor.Highlighter is TSynPasSyn;
  HlIsPas := False;
  if EditorOpts.UseSyntaxHighlight
  then begin
    if Highlighters[ASyntaxHighlighterType]=nil then begin
      Highlighters[ASyntaxHighlighterType]:=
        EditorOpts.CreateSyn(ASyntaxHighlighterType);
    end;
    FEditor.Highlighter:=Highlighters[ASyntaxHighlighterType];
    HlIsPas := FEditor.Highlighter is TSynPasSyn;
  end
  else
    FEditor.Highlighter:=nil;

  if (OldHlIsPas <>  HlIsPas) then begin
    if HlIsPas then
      FEditor.Beautifier := PasBeautifier
    else
      FEditor.Beautifier := nil; // use default
    EditorOpts.GetSynEditSettings(FEditor, nil);
  end;

  FSyntaxHighlighterType:=ASyntaxHighlighterType;
  SourceNotebook.UpdateActiveEditColors(FEditor);
end;

procedure TSourceEditor.SetErrorLine(NewLine: integer);
begin
  if fErrorLine=NewLine then exit;
  fErrorLine:=NewLine;
  fErrorColumn:=EditorComponent.CaretX;
  EditorComponent.Invalidate;
end;

procedure TSourceEditor.UpdateExecutionSourceMark;
var
  BreakPoint: TIDEBreakPoint;
  ExecutionMark: TSourceMark;
  BrkMark: TSourceMark;
begin
  if FSharedValues.UpdatingExecutionMark > 0 then exit;
  ExecutionMark := FSharedValues.ExecutionMark;
  if ExecutionMark = nil then exit;

  inc(FSharedValues.UpdatingExecutionMark);
  try
    if ExecutionMark.Visible then
    begin
      BrkMark := SourceEditorMarks.FindBreakPointMark(Self, ExecutionLine);
      if BrkMark <> nil then begin
        BrkMark.Visible := False;
        BreakPoint := DebugBoss.BreakPoints.Find(Self.FileName, ExecutionLine);
        if (BreakPoint <> nil) and (not BreakPoint.Enabled) then
          ExecutionMark.ImageIndex := SourceEditorMarks.CurrentLineDisabledBreakPointImg
        else
          ExecutionMark.ImageIndex := SourceEditorMarks.CurrentLineBreakPointImg;
      end
      else
        ExecutionMark.ImageIndex := SourceEditorMarks.CurrentLineImg;
    end;
  finally
    dec(FSharedValues.UpdatingExecutionMark);
  end;
end;

procedure TSourceEditor.SetExecutionLine(NewLine: integer);
begin
  if ExecutionLine=NewLine then exit;
  FSharedValues.SetExecutionLine(NewLine);
  UpdateExecutionSourceMark;
end;

function TSourceEditor.RefreshEditorSettings: Boolean;
var
  SimilarEditor: TSynEdit;
Begin
  Result:=true;
  SetSyntaxHighlighterType(fSyntaxHighlighterType);

  // try to copy settings from an editor to the left
  SimilarEditor:=nil;
  if (SourceNotebook.EditorCount>0) and (SourceNotebook.Editors[0]<>Self) then
    SimilarEditor:=SourceNotebook.Editors[0].EditorComponent;
  EditorOpts.GetSynEditSettings(FEditor,SimilarEditor);

  SourceNotebook.UpdateActiveEditColors(FEditor);
  if Visible then
    UpdateIfDefNodeStates(True);
end;

function TSourceEditor.AutoCompleteChar(Char: TUTF8Char; var AddChar: boolean;
  Category: TAutoCompleteOption): boolean;
// returns true if handled
var
  AToken: String;
  i, x1, x2: Integer;
  p: TPoint;
  Line: String;
  CatName: String;
  SrcToken: String;
  IdChars: TSynIdentChars;
  WordToken: String;
begin
  Result:=false;
  Line:=GetLineText;
  p:=GetCursorTextXY;
  if (p.x>length(Line)+1) or (Line='') then exit;
  CatName:=AutoCompleteOptionNames[Category];

  FEditor.GetWordBoundsAtRowCol(p, x1, x2);
  // use the token left of the caret
  x2 := Min(x2, p.x);
  WordToken := copy(Line, x1, x2-x1);
  IdChars := FEditor.IdentChars;
  for i:=0 to Manager.CodeTemplateModul.Completions.Count-1 do begin
    AToken:=Manager.CodeTemplateModul.Completions[i];
    if AToken='' then continue;
    if AToken[1] in IdChars then
      SrcToken:=WordToken
    else
      SrcToken:=copy(Line,length(Line)-length(AToken)+1,length(AToken));
    //DebugLn(['TSourceEditor.AutoCompleteChar ',AToken,' SrcToken=',SrcToken,' CatName=',CatName,' Index=',Manager.CodeTemplateModul.CompletionAttributes[i].IndexOfName(CatName)]);
    if (UTF8CompareText(AToken,SrcToken)=0)
    and (Manager.CodeTemplateModul.CompletionAttributes[i].IndexOfName(CatName)>=0)
    and ( (not FEditor.SelAvail) or
          (Manager.CodeTemplateModul.CompletionAttributes[i].IndexOfName(
             AutoCompleteOptionNames[acoIgnoreForSelection]) < 0)  )
    then begin
      Result:=true;
      //DebugLn(['TSourceEditor.AutoCompleteChar ',AToken,' SrcToken=',SrcToken,' CatName=',CatName,' Index=',Manager.CodeTemplateModul.CompletionAttributes[i].IndexOfName(CatName)]);
      Manager.CodeTemplateModul.ExecuteCompletion(AToken,FEditor);
      AddChar:=not Manager.CodeTemplateModul.CompletionAttributes[i].IndexOfName(
                                     AutoCompleteOptionNames[acoRemoveChar])>=0;
      exit;
    end;
  end;

  if EditorOpts.AutoBlockCompletion
  and (SyntaxHighlighterType in [lshFreePascal,lshDelphi]) then
    Result:=AutoBlockCompleteChar(Char,AddChar,Category,p,Line);
end;

function TSourceEditor.AutoBlockCompleteChar(Char: TUTF8Char;
  var AddChar: boolean; Category: TAutoCompleteOption; aTextPos: TPoint;
  Line: string): boolean;
// returns true if handled
var
  x1: integer;
  x2: integer;
  WordToken: String;
  p: LongInt;
  StartPos: integer;
  s: String;
begin
  Result:=false;
  if (not EditorOpts.AutoBlockCompletion)
  or (not (SyntaxHighlighterType in [lshFreePascal,lshDelphi])) then
    exit;
  FEditor.GetWordBoundsAtRowCol(aTextPos, x1, x2);
  // use the token left of the caret
  x2 := Min(x2, aTextPos.x);
  WordToken := copy(Line, x1, x2-x1);
  if (Category in [acoSpace])
  and ((SysUtils.CompareText(WordToken,'if')=0)
    or (SysUtils.CompareText(WordToken,'while')=0)
    or (SysUtils.CompareText(WordToken,'for')=0)
    )
  then begin
    p:=x2;
    ReadRawNextPascalAtom(Line,p,StartPos);
    if SysUtils.CompareText(copy(Line,StartPos,p-StartPos),'begin')=0 then begin
      // 'if begin' => insert 'then'
      // 'while begin' => insert 'do'
      // 'for begin' => insert 'do'
      Result:=true;
      if (SysUtils.CompareText(WordToken,'if')=0) then
        s:='then'
      else
        s:='do';
      s:=' '+CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.BeautifyKeyWord(s);
      if not (Line[x2] in [' ',#9]) then
        s:=s+' ';
      FEditor.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.AutoBlockCompleteChar'){$ENDIF};
      try
        FEditor.InsertTextAtCaret(s);
        FEditor.LogicalCaretXY:=aTextPos;
      finally
        FEditor.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.AutoBlockCompleteChar'){$ENDIF};
      end;
    end;
  end;
end;

function TSourceEditor.AutoBlockCompleteChar(Char: TUTF8Char): boolean;
var
  p: TPoint;
  x1: integer;
  x2: integer;
  Line: String;
  WordToken: String;
begin
  Result:=false;
  if (not EditorOpts.AutoBlockCompletion)
  or (not (SyntaxHighlighterType in [lshFreePascal,lshDelphi])) then
    exit;
  p:=GetCursorTextXY;
  FEditor.GetWordBoundsAtRowCol(p, x1, x2);
  Line:=GetLineText;
  WordToken := copy(Line, x1, x2-x1);
  if (SysUtils.CompareText(WordToken,'begin')=0)
  then begin
    debugln(['TSourceEditor.AutoBlockCompleteChar ']);
    // user typed 'begin'
    LazarusIDE.SaveSourceEditorChangesToCodeCache(self);
    FEditor.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.AutoBlockCompleteChar (2)'){$ENDIF};
    FEditor.BeginUpdate;
    try
      if not CodeToolBoss.CompleteBlock(CodeBuffer,p.X,p.Y,true) then exit;
    finally
      FEditor.EndUpdate;
      FEditor.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.AutoBlockCompleteChar (2)'){$ENDIF};
    end;
  end;
end;

procedure TSourceEditor.AutoCompleteBlock;
var
  XY: TPoint;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
begin
  LazarusIDE.SaveSourceEditorChangesToCodeCache(Self);
  XY:=FEditor.LogicalCaretXY;
  FEditor.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.AutoCompleteBlock'){$ENDIF};
  FEditor.BeginUpdate;
  try
    if not CodeToolBoss.CompleteBlock(CodeBuffer,XY.X,XY.Y,false,
                                      NewCode,NewX,NewY,NewTopLine) then exit;
    XY:=FEditor.LogicalCaretXY;
    //DebugLn(['TSourceEditor.AutoCompleteBlock XY=',dbgs(XY),' NewX=',NewX,' NewY=',NewY]);
    if (NewCode<>CodeBuffer) or (NewX<>XY.X) or (NewY<>XY.Y) or (NewTopLine>0)
    then begin
      XY.X:=NewX;
      XY.Y:=NewY;
      FEditor.LogicalCaretXY:=XY;
    end;
  finally
    FEditor.EndUpdate;
    FEditor.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.AutoCompleteBlock'){$ENDIF};
  end;
end;

procedure TSourceEditor.UpdateNoteBook(const ANewNoteBook: TSourceNotebook; ANewPage: TTabSheet);
begin
  if FSourceNoteBook = ANewNoteBook then exit;

  FSourceNoteBook := ANewNoteBook;
  FAOwner := ANewNoteBook;
  FPageName := ANewNoteBook.NoteBookPages[ANewNoteBook.NoteBookIndexOfPage(ANewPage)];

  EditorComponent.Parent := nil;
  // Change the Owner of the SynEdit
  EditorComponent.Owner.RemoveComponent(EditorComponent);
  FSourceNoteBook.InsertComponent(EditorComponent);
  // And the Parent
  EditorComponent.Parent := ANewPage;
end;

{ AOwner is the TSourceNotebook
  AParent is a page of the TPageControl }
procedure TSourceEditor.CreateEditor(AOwner: TComponent; AParent: TWinControl);
var
  NewName: string;
  i: integer;
  bmp: TCustomBitmap;
Begin
  {$IFDEF IDE_DEBUG}
  debugln('TSourceEditor.CreateEditor  A ');
  {$ENDIF}
  if not assigned(FEditor) then Begin
    FVisible := False;
    i:=0;
    repeat
      inc(i);
      NewName:='SynEdit'+IntToStr(i);
    until (AOwner.FindComponent(NewName)=nil);
    FEditor := TIDESynEditor.Create(AOwner);
    FEditor.BeginUpdate;
    with FEditor do begin
      Name:=NewName;
      Text:='';
      Align := alClient;
      Visible := False;
      BookMarkOptions.EnableKeys := false;
      BookMarkOptions.LeftMargin:=1;
      BookMarkOptions.BookmarkImages := SourceEditorMarks.ImgList;
      Gutter.MarksPart.DebugMarksImageIndex := SourceEditorMarks.SourceLineImg;
      WantTabs := true;
      ScrollBars := ssAutoBoth;

      // IMPORTANT: when you change below, don't forget updating UnbindEditor
      OnStatusChange := @EditorStatusChanged;
      OnProcessCommand := @ProcessCommand;
      OnProcessUserCommand := @ProcessUserCommand;
      OnCommandProcessed := @UserCommandProcessed;
      OnReplaceText := @OnReplace;
      OnGutterClick := @Self.OnGutterClick;
      OnSpecialLineMarkup := @OnEditorSpecialLineColor;
      OnMouseMove := @EditorMouseMoved;
      OnMouseWheel := @EditorMouseWheel;
      OnMouseDown := @EditorMouseDown;
      OnClickLink := Manager.OnClickLink;
      OnMouseLink := Manager.OnMouseLink;
      OnKeyDown := @EditorKeyDown;
      OnKeyUp := @EditorKeyUp;
      OnPaste:=@EditorPaste;
      OnEnter:=@EditorEnter;
      OnPlaceBookmark := @EditorPlaceBookmark;
      OnClearBookmark := @EditorClearBookmark;
      OnChangeUpdating  := @EditorChangeUpdating;
      OnMultiCaretBeforeCommand := @DoMultiCaretBeforeCommand;
      RegisterMouseActionExecHandler(@EditorHandleMouseAction);
      // IMPORTANT: when you change above, don't forget updating UnbindEditor
      Parent := AParent;
      if AParent.Font.PixelsPerInch<>96 then
        AutoAdjustLayout(lapAutoAdjustForDPI, 96, AParent.Font.PixelsPerInch, 0, 0);
    end;
    Manager.CodeTemplateModul.AddEditor(FEditor);
    Manager.FMacroRecorder.AddEditor(FEditor);
    Manager.NewEditorCreated(self);
    FEditor.TemplateEdit.OnActivate := @EditorActivateSyncro;
    FEditor.TemplateEdit.OnDeactivate := @EditorDeactivateSyncro;
    bmp := CreateBitmapFromResourceName(HInstance, 'tsynsyncroedit');
    FEditor.SyncroEdit.GutterGlyph.Assign(bmp);
    bmp.Free;
    FEditor.SyncroEdit.OnBeginEdit := @EditorActivateSyncro;
    FEditor.SyncroEdit.OnEndEdit := @EditorDeactivateSyncro;

    RefreshEditorSettings;
    FEditor.EndUpdate;
  end else begin
    FEditor.Parent:=AParent;
  end;
end;

procedure TSourceEditor.SetCodeBuffer(NewCodeBuffer: TCodeBuffer);
begin
  FSharedValues.CodeBuffer := NewCodeBuffer;
end;

procedure TSourceEditor.StartIdentCompletionBox(JumpToError,
  CanAutoComplete: boolean);
var
  I: Integer;
  TextS, TextS2: String;
  LogCaret: TPoint;
  UseWordCompletion: Boolean;
  Completion: TSourceEditCompletion;
  CompletionRect: TRect;
begin
  {$IFDEF VerboseIDECompletionBox}
  debugln(['TSourceEditor.StartIdentCompletionBox JumpToError: ',JumpToError]);
  {$ENDIF}
  if (FEditor.ReadOnly) then exit;
  Completion := Manager.DefaultCompletionForm;
  if (Completion.CurrentCompletionType<>ctNone) then exit;
  Completion.IdentCompletionJumpToError := JumpToError;
  Completion.CurrentCompletionType:=ctIdentCompletion;
  TextS := FEditor.LineText;
  LogCaret:=FEditor.LogicalCaretXY;
  Completion.Editor:=FEditor;
  i := LogCaret.X - 1;
  if i > length(TextS) then
    TextS2 := ''
  else begin
    while (i > 0) and (TextS[i] in ['a'..'z','A'..'Z','0'..'9','_']) do
      dec(i);
    TextS2 := Trim(copy(TextS, i + 1, LogCaret.X - i - 1));
  end;
  UseWordCompletion:=false;
  CompletionRect := Manager.GetScreenRectForToken(FEditor, FEditor.CaretX-length(TextS2), FEditor.CaretY, FEditor.CaretX-1);

  if not Manager.FindIdentCompletionPlugin
    (Self, JumpToError, TextS2, CompletionRect.Top, CompletionRect.Left, UseWordCompletion)
  then
    exit;
  if UseWordCompletion then
    Completion.CurrentCompletionType:=ctWordCompletion;

  Completion.AutoUseSingleIdent := CanAutoComplete and CodeToolsOpts.IdentComplAutoUseSingleIdent;
  Completion.Execute(TextS2, CompletionRect);
  {$IFDEF VerboseIDECompletionBox}
  debugln(['TSourceEditor.StartIdentCompletionBox END Completion.TheForm.Visible=',Completion.TheForm.Visible]);
  {$ENDIF}
end;

procedure TSourceEditor.StartWordCompletionBox;
var
  TextS: String;
  LogCaret: TPoint;
  i: Integer;
  TextS2: String;
  Completion: TSourceEditCompletion;
begin
  if (FEditor.ReadOnly) then exit;
  Completion := Manager.DefaultCompletionForm;
  if (Completion.CurrentCompletionType<>ctNone) then exit;
  Completion.CurrentCompletionType:=ctWordCompletion;
  TextS := FEditor.LineText;
  LogCaret:=FEditor.LogicalCaretXY;
  Completion.Editor:=FEditor;
  i := LogCaret.X - 1;
  if i > length(TextS) then
    TextS2 := ''
  else begin
    while (i > 0) and (TextS[i] in ['a'..'z','A'..'Z','0'..'9','_']) do
      dec(i);
    TextS2 := Trim(copy(TextS, i + 1, LogCaret.X - i - 1));
  end;
  Completion.Execute
    (TextS2, Manager.GetScreenRectForToken(FEditor, FEditor.CaretX-length(TextS2), FEditor.CaretY, FEditor.CaretX-1));
end;

procedure TSourceEditor.IncreaseIgnoreCodeBufferLock;
begin
  FSharedValues.IncreaseIgnoreCodeBufferLock;
end;

procedure TSourceEditor.DecreaseIgnoreCodeBufferLock;
begin
  FSharedValues.DecreaseIgnoreCodeBufferLock;
end;

procedure TSourceEditor.UpdateCodeBuffer;
// copy the source from EditorComponent to codetools
begin
  FSharedValues.UpdateCodeBuffer;
end;

function TSourceEditor.NeedsUpdateCodeBuffer: boolean;
begin
  Result := FSharedValues.NeedsUpdateCodeBuffer;
end;

procedure TSourceEditor.ConnectScanner(Scanner: TLinkScanner);
begin
  FSharedValues.ConnectScanner(Scanner);
end;

function TSourceEditor.GetSource: TStrings;
Begin
  //return synedit's source.
  Result := FEditor.Lines;
end;

procedure TSourceEditor.SetIsLocked(const AValue: Boolean);
begin
  if FIsLocked = AValue then exit;
  FIsLocked := AValue;
  UpdatePageName;
  SourceNotebook.UpdateStatusBar;
  UpdateProjectFile;
end;

procedure TSourceEditor.SetPageName(const AValue: string);
begin
  if FPageName=AValue then exit;
  FPageName:=AValue;
  UpdatePageName;
end;

procedure TSourceEditor.UpdatePageName;
var
  p: Integer;
  NewPageCaption: String;
begin
  if SourceNotebook.FUpdateLock > 0 then begin
    include(SourceNotebook.FUpdateFlags, ufPageNames);
    exit;
  end;
  p:=SourceNotebook.FindPageWithEditor(Self);
  if EditorOpts.ShowTabNumbers and (p < 10) then
    // Number pages 1, ..., 9, 0 -- according to Alt+N hotkeys.
    NewPageCaption:=Format('%s:%d', [FPageName, (p+1) mod 10])
  else
    NewPageCaption:=FPageName;
  if IsLocked then NewPageCaption:='#'+NewPageCaption;
  if Modified then NewPageCaption:='*'+NewPageCaption;
  if SourceNotebook.NoteBookPages[p] <> NewPageCaption then begin
    SourceNotebook.NoteBookPages[p] := NewPageCaption;
    SourceNotebook.UpdateTabsAndPageTitle;
    SourceNotebook.CallOnEditorPageCaptionUpdate(Self);
  end;
end;

procedure TSourceEditor.SetSource(Value: TStrings);
Begin
  FEditor.Lines.Assign(Value);
end;

function TSourceEditor.GetCurrentCursorXLine: Integer;
Begin
  Result := FEditor.CaretX
end;

procedure TSourceEditor.SetCurrentCursorXLine(num: Integer);
Begin
  FEditor.CaretX := Num;
end;

function TSourceEditor.GetCurrentCursorYLine: Integer;
Begin
  Result := FEditor.CaretY;
end;

procedure TSourceEditor.SetCurrentCursorYLine(num: Integer);
Begin
  FEditor.CaretY := Num;
end;

procedure TSourceEditor.SelectText(const StartPos, EndPos: TPoint);
Begin
  FEditor.BlockBegin := StartPos;
  FEditor.BlockEnd := EndPos;
end;

procedure TSourceEditor.InsertLine(StartLine: Integer; const NewText: String;
  aKeepMarks: Boolean);
const
  MarksMode: array[Boolean] of TSynMarksAdjustMode = (smaMoveUp, smaKeep);
var
  Pt: TPoint;
begin
  if not ReadOnly then
  begin
    if StartLine > 1 then
      Pt := Point(Length(FEditor.Lines[StartLine - 2]) + 1, StartLine - 1)
    else
      Pt := Point(1, 1);
    FEditor.SetTextBetweenPoints(Pt, Pt,
      LineEnding + NewText, [], scamEnd, MarksMode[aKeepMarks]);
  end;
end;

procedure TSourceEditor.ReplaceLines(StartLine, EndLine: integer;
  const NewText: string; aKeepMarks: Boolean = False);
const
  MarksMode: array[Boolean] of TSynMarksAdjustMode = (smaMoveUp, smaKeep);
begin
  if not ReadOnly then
    FEditor.SetTextBetweenPoints(
      Point(1, StartLine),
      Point(Length(FEditor.Lines[Endline - 1]) + 1, EndLine),
      NewText, [], scamEnd, MarksMode[aKeepMarks]);
end;

procedure TSourceEditor.EncloseSelection;
var
  EncloseType: TEncloseSelectionType;
  EncloseTemplate: string;
  NewSelection: string;
  NewCaretXY: TPoint;
begin
  if ReadOnly then exit;
  if not FEditor.SelAvail then
    exit;
  if ShowEncloseSelectionDialog(EncloseType)<>mrOk then exit;
  GetEncloseSelectionParams(EncloseType,EncloseTemplate);
  EncloseTextSelection(EncloseTemplate,FEditor.Lines,
                       FEditor.BlockBegin,FEditor.BlockEnd,
                       NewSelection,NewCaretXY);
  //debugln(['TSourceEditor.EncloseSelection A NewCaretXY=',NewCaretXY.X,',',NewCaretXY.Y,' "',NewSelection,'"']);
  FEditor.SelText:=NewSelection;
  FEditor.LogicalCaretXY:=NewCaretXY;
end;

function TSourceEditor.GetModified: Boolean;
Begin
  Result := FSharedValues.Modified;
end;

procedure TSourceEditor.SetModified(const NewValue: Boolean);
begin
  FSharedValues.SetModified(NewValue);
end;

function TSourceEditor.GetInsertMode: Boolean;
Begin
  Result := FEditor.Insertmode;
end;

function TSourceEditor.Close: Boolean;
Begin
  DebugLnEnter(SRCED_CLOSE, ['TSourceEditor.Close ShareCount=', FSharedValues.SharedEditorCount]);
  Result := True;
  Visible := False;
  Manager.EditorRemoved(Self);
  UnbindEditor;
  FEditor.Parent:=nil;
  if FSharedValues.SharedEditorCount = 1 then
    CodeBuffer := nil;
  DebugLnExit(SRCED_CLOSE, ['TSourceEditor.Close ']);
end;

procedure TSourceEditor.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}(ACaller: String = ''){$ENDIF};
begin
  FEditor.BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.BeginUndoBlock ' + ACaller){$ENDIF};
end;

procedure TSourceEditor.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}(ACaller: String = ''){$ENDIF};
begin
  FEditor.EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditor.EndUndoBlock ' + ACaller){$ENDIF};
end;

procedure TSourceEditor.BeginUpdate;
begin
  FEditor.BeginUpdate;
end;

procedure TSourceEditor.EndUpdate;
begin
  FEditor.EndUpdate;
end;

procedure TSourceEditor.BeginGlobalUpdate;
begin
  FSharedValues.BeginGlobalUpdate;
end;

procedure TSourceEditor.EndGlobalUpdate;
begin
  FSharedValues.EndGlobalUpdate;
end;

procedure TSourceEditor.SetPopupMenu(NewPopupMenu: TPopupMenu);
begin
  if NewPopupMenu<>FPopupMenu then begin
    FPopupMenu:=NewPopupMenu;
    if FEditor<>nil then begin
      if FEditor.PopupMenu <> nil then // Todo: why?
        FEditor.PopupMenu.RemoveFreeNotification(FEditor);
      FEditor.PopupMenu:=NewPopupMenu;
    end;
  end;
end;

function TSourceEditor.GetFilename: string;
begin
  if CodeBuffer <> nil then
    Result := CodeBuffer.Filename
  else
    Result := '';
end;

function TSourceEditor.GetEditorControl: TWinControl;
begin
  Result:=FEditor;
end;

function TSourceEditor.GetCodeToolsBuffer: TObject;
begin
  Result:=CodeBuffer;
end;

procedure TSourceEditor.EditorPaste(Sender: TObject; var AText: String;
  var AMode: TSynSelectionMode; ALogStartPos: TPoint;
  var AnAction: TSynCopyPasteAction);
var
  p: integer;
  NestedComments: Boolean;
  NewIndent: TFABIndentationPolicy;
  Indent: LongInt;
  NewSrc: string;
  i: Integer;
  SemMode: TSemSelectionMode;
  SemAction: TSemCopyPasteAction;
begin
  if Assigned(Manager) then begin
    // call handlers
    i:=Manager.FHandlers[semhtCopyPaste].Count;
    while Manager.FHandlers[semhtCopyPaste].NextDownIndex(i) do begin
      SemMode:=TSemSelectionMode(AMode);
      SemAction:=TSemCopyPasteAction(AnAction);
      TSemCopyPasteEvent(Manager.FHandlers[semhtCopyPaste][i])(Self,AText,
        SemMode,ALogStartPos,SemAction);
      AMode:=TSynSelectionMode(SemMode);
      AnAction:=TSynCopyPasteAction(SemAction);
      if AnAction=scaAbort then exit;
    end;
  end;

  if AMode<>smNormal then exit;
  if SyncroLockCount > 0 then exit;
  if not CodeToolsOpts.IndentOnPaste then exit;
  if not (SyntaxHighlighterType in [lshFreePascal, lshDelphi]) then
    exit;
  {$IFDEF VerboseIndenter}
  debugln(['TSourceEditor.EditorPaste LogCaret=',dbgs(ALogStartPos)]);
  {$ENDIF}
  if ALogStartPos.X>1 then exit;
  UpdateCodeBuffer;
  CodeBuffer.LineColToPosition(ALogStartPos.Y,ALogStartPos.X,p);
  if p<1 then exit;
  {$IFDEF VerboseIndenter}
  if ALogStartPos.Y>1 then
    DebugLn(['TSourceEditor.EditorPaste Y-1=',Lines[ALogStartPos.Y-2]]);
  DebugLn(['TSourceEditor.EditorPaste Y+0=',Lines[ALogStartPos.Y-1]]);
  if ALogStartPos.Y<LineCount then
    DebugLn(['TSourceEditor.EditorPaste Y+1=',Lines[ALogStartPos.Y+0]]);
  {$ENDIF}
  NestedComments:=CodeToolBoss.GetNestedCommentsFlagForFile(CodeBuffer.Filename);
  if not CodeToolBoss.Indenter.GetIndent(CodeBuffer.Source,p,NestedComments,
    true,NewIndent,CodeToolsOpts.IndentContextSensitive,AText)
  then exit;
  if not NewIndent.IndentValid then exit;
  Indent:=NewIndent.Indent-GetLineIndentWithTabs(AText,1,EditorComponent.TabWidth);
  {$IFDEF VerboseIndenter}
  debugln(AText);
  DebugLn(['TSourceEditor.EditorPaste Indent=',Indent]);
  {$ENDIF}
  IndentText(AText,Indent,EditorComponent.TabWidth,NewSrc);
  AText:=NewSrc;
  {$IFDEF VerboseIndenter}
  debugln(AText);
  DebugLn(['TSourceEditor.EditorPaste END']);
  {$ENDIF}
end;

procedure TSourceEditor.EditorPlaceBookmark(Sender: TObject;
  var Mark: TSynEditMark);
begin
  if Assigned(Manager) and Assigned(Manager.OnPlaceBookmark) then
    Manager.OnPlaceBookmark(Self, Mark);
end;

procedure TSourceEditor.EditorClearBookmark(Sender: TObject;
  var Mark: TSynEditMark);
begin
  if Assigned(Manager) and Assigned(Manager.OnClearBookmark) then
    Manager.OnClearBookmark(Self, Mark);
end;

procedure TSourceEditor.EditorEnter(Sender: TObject);
var
  SrcEdit: TSourceEditor;
begin
  debugln(SRCED_PAGES, ['TSourceEditor.EditorEnter ']);
  if (FSourceNoteBook.FUpdateLock <> 0) or
     (FSourceNoteBook.FFocusLock <> 0)
  then exit;
  if (FSourceNoteBook.PageIndex = PageIndex) then
    Activate
  else begin
    SrcEdit:=SourceNotebook.GetActiveSE;
    if SrcEdit<>nil then
      SrcEdit.FocusEditor;
    // Navigating with mousebuttons between editors (eg jump history on btn 4/5)
    // can trigger the old editor to be refocused (while not visible)
  end;
end;

procedure TSourceEditor.EditorActivateSyncro(Sender: TObject);
begin
  inc(FSyncroLockCount);
end;

procedure TSourceEditor.EditorDeactivateSyncro(Sender: TObject);
begin
  dec(FSyncroLockCount);
end;

function TSourceEditor.GetCodeBuffer: TCodeBuffer;
begin
  Result := FSharedValues.CodeBuffer;
end;

function TSourceEditor.GetExecutionLine: integer;
begin
  Result := FSharedValues.ExecutionLine;
end;

function TSourceEditor.GetHasExecutionMarks: Boolean;
begin
  Result := EditorComponent.IDEGutterMarks.HasDebugMarks;
end;

function TSourceEditor.GetSharedEditors(Index: Integer): TSourceEditor;
begin
  Result := FSharedValues.SharedEditors[Index];
end;

procedure TSourceEditor.EditorChangeUpdating(ASender: TObject; AnUpdating: Boolean);
begin
  // Calls may be unbalanced, because the event handler may not be assigned to the event on the first BeginUpdate
  If AnUpdating then begin
    //if FInEditorChangedUpdating then
    //  debugln(['***** TSourceEditor.EditorChangeUpdating: Updating=True, but FInEditorChangedUpdating was true already']);
    if not FInEditorChangedUpdating then begin
      FInEditorChangedUpdating := True;
      DebugBoss.LockCommandProcessing;
    end;
    FMouseActionPopUpMenu := nil;
  end else
  begin
    //if not FInEditorChangedUpdating then
    //  debugln(['***** TSourceEditor.EditorChangeUpdating: Updating=False, but FInEditorChangedUpdating was false already']);
    if FInEditorChangedUpdating then begin
      FInEditorChangedUpdating := False;  // set before unlocking
      DebugBoss.UnLockCommandProcessing;  // may lead to recursion
    end;
    //FMouseActionPopUpMenu :=
    if (FMouseActionPopUpMenu <> nil) then begin
      FMouseActionPopUpMenu.PopupComponent := FEditor;
      FMouseActionPopUpMenu.PopUp;
      FMouseActionPopUpMenu := nil;
    end;
  end;
end;

function TSourceEditor.EditorHandleMouseAction(AnAction: TSynEditMouseAction;
  var AnInfo: TSynEditMouseActionInfo): Boolean;
begin
  Result := AnAction.Command = emcContextMenu;
  if not Result then exit;

  case AnAction.Option2 of
    1: FMouseActionPopUpMenu := SourceNotebook.DbgPopUpMenu;
    2: FMouseActionPopUpMenu := SourceNotebook.TabPopUpMenu;
    else
      FMouseActionPopUpMenu := PopupMenu;
  end;

  if (not FInEditorChangedUpdating) and (FMouseActionPopUpMenu <> nil) then begin
    FMouseActionPopUpMenu.PopupComponent := FEditor;
    FMouseActionPopUpMenu.PopUp;
    FMouseActionPopUpMenu := nil;
  end;
end;

procedure TSourceEditor.EditorMouseMoved(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
//  debugln('MouseMove in Editor',X,',',Y);
  if Assigned(OnMouseMove) then
    OnMouseMove(Self,Shift,X,Y);
end;

procedure TSourceEditor.EditorMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
//  debugln('MouseWheel in Editor');
  if Assigned(OnMouseWheel) then
    OnMouseWheel(Self, Shift, WheelDelta, MousePos, Handled)
end;

procedure TSourceEditor.EditorMouseDown(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
begin
  CheckActiveWindow;
  if Assigned(OnMouseDown) then
    OnMouseDown(Sender, Button, Shift, X,Y);
end;

procedure TSourceEditor.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //DebugLn(['TSourceEditor.EditorKeyDown A ',dbgsName(Sender),' Key=',IntToStr(Key),' File=',ExtractFileName(Filename),' Wnd=',dbgSourceNoteBook(SourceNotebook)]);
  CheckActiveWindow;
  if Assigned(OnKeyDown) then
    OnKeyDown(Sender, Key, Shift);

  IDECommandList.PostponeUpdateEvents;
end;

procedure TSourceEditor.EditorKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  CheckActiveWindow;
  if Assigned(OnKeyUp) then
    OnKeyUp(Sender, Key, Shift);

  IDECommandList.PostponeUpdateEvents;
end;

{-------------------------------------------------------------------------------
  method TSourceEditor.CenterCursor
  Params: none
  Result: none

  Center the current cursor line in editor.
-------------------------------------------------------------------------------}
procedure TSourceEditor.CenterCursor(SoftCenter: Boolean = False);
var
  Y, CurTopLine, LinesInWin, MinLines, NewTopLine: Integer;
begin
  LinesInWin := EditorComponent.LinesInWindow;
  CurTopLine := EditorComponent.TopView;
  Y := EditorComponent.TextIndexToViewPos(EditorComponent.CaretY);

  if SoftCenter then begin
    MinLines := Min(
      Min( Max(LinesInWin div SoftCenterFactor, SoftCenterMinimum),
           SoftCenterMaximum),
      Max(LinesInWin div 2 - 1, 0) // make sure there is at least one line in the soft center
      );

    if (Y <= CurTopLine) or (Y >= CurTopLine + LinesInWin) then
      // Caret not yet visible => hard-center
      NewTopLine := Max(1, Y - (LinesInWin div 2))
    else
    if Y < CurTopLine + MinLines then
      NewTopLine := Max(1, Y - MinLines)
    else
    if Y > CurTopLine + LinesInWin - MinLines then
      NewTopLine := Max(1, Y - LinesInWin + MinLines)
    else
      NewTopLine := CurTopLine;
  end
  else
    // not using SoftCenter
    NewTopLine := Max(1, Y - (LinesInWin div 2));

  if NewTopLine < 1 then NewTopLine := 1;
  EditorComponent.TopView := NewTopLine;
end;

procedure TSourceEditor.CenterCursorHoriz(HCMode: TSourceEditHCenterMode);
var
  i: Integer;
begin
  case HCMode of
    hcmCenter:
      with EditorComponent do begin
        LeftChar:=Max(LogicalCaretXY.X - (CharsInWindow div 2), 1);
      end;
    hcmCenterKeepEOL:
      with EditorComponent do begin
        i := LogicalToPhysicalPos(Point(Length(Lines[CaretY-1]) + 1, CaretY)).X;
        LeftChar:=Max(Min(LogicalCaretXY.X - (CharsInWindow div 2),
                          i - CharsInWindow
                         ), 1);
      end;
    hcmSoft:
      // TODO: offset on left side
      with EditorComponent do begin
        LeftChar:=Max(LogicalCaretXY.X - (CharsInWindow * 4 div 5), 1);
      end;
    hcmSoftKeepEOL:
      // TODO: offset on left side
      with EditorComponent do begin
        i := LogicalToPhysicalPos(Point(Length(Lines[CaretY-1]) + 1, CaretY)).X;
        LeftChar:=Max(Min(LogicalCaretXY.X - (CharsInWindow * 4 div 5),
                          i - CharsInWindow
                         ), 1);
      end;
  end;
end;

function TSourceEditor.TextToScreenPosition(const Position: TPoint): TPoint;
begin
  Result:=FEditor.LogicalToPhysicalPos(Position);
end;

function TSourceEditor.ScreenToTextPosition(const Position: TPoint): TPoint;
begin
  Result:=FEditor.PhysicalToLogicalPos(Position);
end;

function TSourceEditor.ScreenToPixelPosition(const Position: TPoint): TPoint;
begin
  Result:=FEditor.RowColumnToPixels(Position);
end;

function TSourceEditor.LineCount: Integer;
begin
  Result:=FEditor.Lines.Count;
end;

function TSourceEditor.WidthInChars: Integer;
begin
  Result:=FEditor.CharsInWindow;
end;

function TSourceEditor.HeightInLines: Integer;
begin
  Result:=FEditor.LinesInWindow;
end;

function TSourceEditor.CharWidth: integer;
begin
  Result:=FEditor.CharWidth;
end;

function TSourceEditor.GetLineText: string;
begin
  Result:=FEditor.LineText;
end;

procedure TSourceEditor.SetLineText(const AValue: string);
begin
  FEditor.LineText:=AValue;
end;

function TSourceEditor.GetLines: TStrings;
begin
  Result:=FEditor.Lines;
end;

function TSourceEditor.GetLinesInWindow: Integer;
begin
  Result := FEditor.LinesInWindow;
end;

procedure TSourceEditor.SetLines(const AValue: TStrings);
begin
  FEditor.Lines:=AValue;
end;

function TSourceEditor.GetProjectFile: TLazProjectFile;
begin
  Result:=LazarusIDE.GetProjectFileForProjectEditor(Self);
end;

procedure TSourceEditor.UpdateProjectFile(AnUpdates: TSrcEditProjectUpdatesNeeded);
begin
  AnUpdates := AnUpdates + FProjectFileUpdatesNeeded;
  FProjectFileUpdatesNeeded := [];
  if Assigned(Manager) and Assigned(Manager.OnUpdateProjectFile)
    then Manager.OnUpdateProjectFile(self, AnUpdates);
end;

function TSourceEditor.GetDesigner(LoadForm: boolean): TIDesigner;
begin
  Result:=LazarusIDE.GetDesignerForProjectEditor(Self, LoadForm)
end;

function TSourceEditor.GetCursorScreenXY: TPoint;
begin
  Result:=FEditor.CaretXY;
end;

function TSourceEditor.GetCursorTextXY: TPoint;
begin
  Result:=FEditor.LogicalCaretXY;
end;

procedure TSourceEditor.SetCursorScreenXY(const AValue: TPoint);
begin
  FEditor.CaretXY:=AValue;
end;

procedure TSourceEditor.SetCursorTextXY(const AValue: TPoint);
begin
  FEditor.LogicalCaretXY:=AValue;
end;

function TSourceEditor.GetBlockBegin: TPoint;
begin
  Result:=FEditor.BlockBegin;
end;

function TSourceEditor.GetBlockEnd: TPoint;
begin
  Result:=FEditor.BlockEnd;
end;

procedure TSourceEditor.SetBlockBegin(const AValue: TPoint);
begin
  FEditor.BlockBegin:=AValue;
end;

procedure TSourceEditor.SetBlockEnd(const AValue: TPoint);
begin
  FEditor.BlockEnd:=AValue;
end;

function TSourceEditor.GetTopLine: Integer;
begin
  Result:=FEditor.TopLine;
end;

procedure TSourceEditor.SetTopLine(const AValue: Integer);
begin
  FEditor.TopLine:=AValue;
end;

function TSourceEditor.CursorInPixel: TPoint;
begin
  Result:=Point(FEditor.CaretXPix,FEditor.CaretYPix);
end;

function TSourceEditor.IsCaretOnScreen(ACaret: TPoint; UseSoftCenter: Boolean = False): Boolean;
var
  LinesInWin, MinLines, CurTopLine, Y: Integer;
begin
  LinesInWin := EditorComponent.LinesInWindow;
  CurTopLine := EditorComponent.TopView;
  Y := EditorComponent.TextIndexToViewPos(ACaret.Y);
  if UsesoftCenter then begin
    MinLines := Min(
      Min( Max(LinesInWin div SoftCenterFactor, SoftCenterMinimum),
           SoftCenterMaximum),
      Max(LinesInWin div 2 - 1, 0) // make sure there is at least one line in the soft center
      );
  end
  else
    MinLines := 0;

  Result := (Y >= CurTopLine + MinLines) and
            (Y <= CurTopLine + LinesInWin - MinLines) and
            (ACaret.X >= FEditor.LeftChar) and
            (ACaret.X <= FEditor.LeftChar + FEditor.CharsInWindow);
end;

procedure TSourceEditor.MultiPasteText;
var
  I, CaretX: Integer;
  Content: TStringList;
  Dialog: TMultiPasteDialog;
begin
  if ReadOnly then Exit;
  Dialog := TMultiPasteDialog.Create(nil);
  try
    if Dialog.ShowModal <> mrOK then Exit;
    CaretX := FEditor.CaretX;
    if CaretX > 1 then
    begin
      Content := TStringList.Create;
      try
        Content.Text := Dialog.Content.Text;
        for I := 0 to Pred(Content.Count) do
          if I > 0 then
            Content[I] := Concat(DupeString(' ', Pred(CaretX)), Content[I]);
        FEditor.InsertTextAtCaret(Content.Text);
      finally
        Content.Free;
      end;
    end
    else
      FEditor.InsertTextAtCaret(Dialog.Content.Text);
  finally
    Dialog.Free;
  end;
end;

function TSourceEditor.SearchReplace(const ASearch, AReplace: string;
  SearchOptions: TSrcEditSearchOptions): integer;
const
  SrcEdit2SynEditSearchOption: array[TSrcEditSearchOption] of TSynSearchOption =(
    ssoMatchCase,
    ssoWholeWord,
    ssoBackwards,
    ssoEntireScope,
    ssoSelectedOnly,
    ssoReplace,
    ssoReplaceAll,
    ssoPrompt,
    ssoRegExpr,
    ssoRegExprMultiLine
  );
var
  NewOptions: TSynSearchOptions;
  o: TSrcEditSearchOption;
begin
  NewOptions:=[];
  for o:=Low(TSrcEditSearchOption) to High(TSrcEditSearchOption) do
    if o in SearchOptions then
      Include(NewOptions,SrcEdit2SynEditSearchOption[o]);
  Result:=DoFindAndReplace(ASearch, AReplace, NewOptions);
end;

function TSourceEditor.GetSourceText: string;
begin
  Result:=FEditor.Text;
end;

procedure TSourceEditor.SetSourceText(const AValue: string);
begin
  FEditor.Text:=AValue;
end;

procedure TSourceEditor.Activate;
begin
  { $note: avoid this if FSourceNoteBook.FUpdateLock > 0 / e.g. debugger calls ProcessMessages, and the internall Index is lost/undone}
  if (FSourceNoteBook=nil) then exit;
  if (FSourceNoteBook.FUpdateLock = 0) then
    FSourceNoteBook.ActiveEditor := Self;
end;

procedure TSourceEditor.ActivateHint(const ClientPos: TPoint; const ABaseURL,
  AHint: string; AAutoShown: Boolean);
var
  ScreenPos: TPoint;
begin
  if SourceNotebook=nil then exit;
  ScreenPos:=EditorComponent.ClientToScreen(ClientPos);
  Manager.ActivateHint(ScreenPos,ABaseURL,AHint,AAutoShown);
end;

function TSourceEditor.PageIndex: integer;
begin
  if FSourceNoteBook<>nil then
    Result:=FSourceNoteBook.FindPageWithEditor(Self)
  else
    Result:=-1;
end;

function TSourceEditor.CaretInSelection(const ACaretPos: TPoint): Boolean;
begin
  Result := (CompareCaret(EditorComponent.BlockBegin, ACaretpos) >= 0)
        and (CompareCaret(ACaretPos, EditorComponent.BlockEnd) >= 0);
end;

function TSourceEditor.IsActiveOnNoteBook: boolean;
begin
  if FSourceNoteBook<>nil then
    Result:=(FSourceNoteBook.GetActiveSE=Self)
  else
    Result:=false;
end;

procedure TSourceEditor.CheckActiveWindow;
begin
  if Manager.ActiveSourceWindow = SourceNotebook then exit;
  debugln('Warning: ActiveSourceWindow is set incorrectly Active=',dbgSourceNoteBook(Manager.ActiveSourceWindow),' Me=',dbgSourceNoteBook(SourceNotebook));
  Manager.ActiveSourceWindow := SourceNotebook;
end;

procedure TSourceEditor.DoRequestExecutionMarks(Data: PtrInt);
begin
  DebugBoss.LineInfo.Request(FSharedValues.MarksRequestedForFile);
end;

procedure TSourceEditor.FillExecutionMarks;
var
  ASource: String;
  i, idx: integer;
  HasAddr: Boolean;
  j: Integer;
begin
  if EditorComponent.IDEGutterMarks.HasDebugMarks then Exit;

  ASource := FileName;
  idx := DebugBoss.LineInfo.IndexOf(ASource);
  if (idx = -1) then
  begin
    if not FSharedValues.MarksRequested then
    begin
      FSharedValues.MarksRequested := True;
      FSharedValues.MarksRequestedForFile := ASource;
      DebugBoss.LineInfo.AddNotification(FLineInfoNotification);
      Application.QueueAsyncCall(@DoRequestExecutionMarks, 0);
    end;
    Exit;
  end;

  FSharedValues.MarksRequestedForFile := '';
  j := -1;
  EditorComponent.IDEGutterMarks.BeginSetDebugMarks;
  try
    for i := 1 to EditorComponent.Lines.Count do
    begin
      HasAddr := DebugBoss.LineInfo.HasAddress(idx, i);
      if (HasAddr) and (j < 0) then
        j := i;
      if (not HasAddr) and (j >= 0) then begin
        EditorComponent.IDEGutterMarks.SetDebugMarks(j, i-1);
        j := -1;
      end;
    end;
    if (HasAddr) and (j >= 0) then
      EditorComponent.IDEGutterMarks.SetDebugMarks(j, EditorComponent.Lines.Count);
  finally
    EditorComponent.IDEGutterMarks.EndSetDebugMarks;
  end;

  // TODO: move to SourceSyneditor
  for i := 0 to SharedEditorCount - 1 do
    SharedEditors[i].EditorComponent.IDEGutterMarks.HasDebugMarks; // update all shared editors
end;

procedure TSourceEditor.ClearExecutionMarks;
var
  i: Integer;
begin
  if FSharedValues.MarksRequested and (FSharedValues.MarksRequestedForFile <> '') then
    DebugBoss.LineInfo.Cancel(FSharedValues.MarksRequestedForFile);
  FSharedValues.MarksRequestedForFile := '';

  EditorComponent.IDEGutterMarks.ClearDebugMarks;
  FSharedValues.MarksRequested := False;
  for i := 0 to SharedEditorCount - 1 do
    SharedEditors[i].EditorComponent.IDEGutterMarks.ClearDebugMarks; // update all shared editors
  if (FLineInfoNotification <> nil) and (DebugBoss <> nil) and (DebugBoss.LineInfo <> nil) then
    DebugBoss.LineInfo.RemoveNotification(FLineInfoNotification);
end;

procedure TSourceEditor.CopyToWindow(AWindowIndex: Integer);
begin
  SourceNotebook.CopyEditor(PageIndex, AWindowIndex, -1)
end;

procedure TSourceEditor.LineInfoNotificationChange(const ASender: TObject; const ASource: String);
begin
  if ASource = FileName then begin
    Application.RemoveAsyncCalls(Self);
    FillExecutionMarks;
  end;
end;

function TSourceEditor.SourceToDebugLine(aLinePos: Integer): Integer;
begin
  Result := FEditor.IDEGutterMarks.SourceLineToDebugLine(aLinePos, True);
end;

function TSourceEditor.DebugToSourceLine(aLinePos: Integer): Integer;
begin
  Result := FEditor.IDEGutterMarks.DebugLineToSourceLine(aLinePos);
end;

procedure TSourceEditor.InvalidateAllIfdefNodes;
begin
  FEditor.InvalidateAllIfdefNodes;
end;

procedure TSourceEditor.SetIfdefNodeState(ALinePos, AstartPos: Integer;
  AState: TSynMarkupIfdefNodeState);
begin
  FEditor.SetIfdefNodeState(ALinePos, AstartPos, AState);
end;

procedure TSourceEditor.UpdateIfDefNodeStates(Force: Boolean = False);
{off $DEFINE VerboseUpdateIfDefNodeStates}
{$IFDEF VerboseUpdateIfDefNodeStates}
const
  VFilePattern='blaunit';
  VMinY=1;
  VMaxY=70;
{$ENDIF}
var
  Scanner: TLinkScanner;
  i: Integer;
  aDirective: PLSDirective;
  Code: TCodeBuffer;
  Y: integer;
  X: integer;
  SynState: TSynMarkupIfdefNodeStateEx;
  SrcPos: Integer;
  ActiveCnt: Integer;
  InactiveCnt: Integer;
  SkippedCnt: Integer;
begin
  //debugln(['TSourceEditor.UpdateIfDefNodeStates START ',Filename]);
  if not EditorComponent.IsIfdefMarkupActive then
    exit;
  //debugln(['TSourceEditor.UpdateIfDefNodeStates CHECK ',Filename]);
  UpdateCodeBuffer;
  Scanner:=SharedValues.GetMainLinkScanner(true);
  if Scanner=nil then exit;
  if (Scanner.ChangeStep=FLastIfDefNodeScannerStep) and (not Force) then exit;
  //debugln(['TSourceEditor.UpdateIfDefNodeStates UPDATING ',Filename]);
  FLastIfDefNodeScannerStep:=Scanner.ChangeStep;
  EditorComponent.BeginUpdate;
  try
    //EditorComponent.InvalidateAllIfdefNodes;
    Code:=CodeBuffer;
    i:=0;
    while i<Scanner.DirectiveCount do
    begin
      aDirective:=Scanner.DirectivesSorted[i];
      //if (Pos(VFilePattern,Code.Filename)>0) then
      //  debugln(['TSourceEditor.UpdateIfDefNodeStates ',i+1,'/',Scanner.DirectiveCount,' ',dbgs(aDirective^.Kind)]);
      inc(i);
      if TCodeBuffer(aDirective^.Code)<>Code then continue;
      if not (aDirective^.Kind in (lsdkAllIf+lsdkAllElse)) then continue;
      Code.AbsoluteToLineCol(aDirective^.SrcPos,Y,X);
      if Y<1 then continue;
      SynState:=idnInvalid;
      // a directive can be scanned multiple times (multi included include files)
      // => show it enabled if it was active at least once
      {$IFDEF VerboseUpdateIfDefNodeStates}
      if (Pos(VFilePattern,Code.Filename)>0) and (Y>=VMinY) and (Y<=VMaxY) then
        debugln(['TSourceEditor.UpdateIfDefNodeStates ',i,'/',Scanner.DirectiveCount,' ',dbgs(Pointer(Code)),' ',Code.Filename,' X=',X,' Y=',Y,' SrcPos=',aDirective^.SrcPos,' State=',dbgs(aDirective^.State)]);
      {$ENDIF}
      SrcPos:=aDirective^.SrcPos;
      ActiveCnt:=0;
      InactiveCnt:=0;
      SkippedCnt:=0;
      repeat
        case aDirective^.State of
        lsdsActive: inc(ActiveCnt);
        lsdsInactive: inc(InactiveCnt);
        lsdsSkipped: inc(SkippedCnt);
        end;
        if i < Scanner.DirectiveCount then begin
          ADirective:=Scanner.DirectivesSorted[i];
          {$IFDEF VerboseUpdateIfDefNodeStates}
          if (Pos(VFilePattern,Code.Filename)>0) and (Y>=VMinY) and (Y<=VMaxY) and (ADirective^.SrcPos=SrcPos) then
            debugln(['TSourceEditor.UpdateIfDefNodeStates ',i,'/',Scanner.DirectiveCount,' MERGING ',dbgs(ADirective^.Code),' ',Code.Filename,' X=',X,' Y=',Y,' SrcPos=',aDirective^.SrcPos,' State=',dbgs(aDirective^.State)]);
          {$ENDIF}
        end;
        inc(i);
      until (ADirective^.SrcPos<>SrcPos) or (TCodeBuffer(ADirective^.Code)<>Code)
        or (i > Scanner.DirectiveCount);
      dec(i);
      if (ActiveCnt>0) and (InactiveCnt=0) and (SkippedCnt=0) then
        SynState:=idnEnabled
      else if (ActiveCnt=0) and (InactiveCnt+SkippedCnt>0) then
        SynState:=idnDisabled
      else if (ActiveCnt>0) then
        SynState:=idnTempEnabled
      else
        SynState:=idnInvalid;
      {$IFDEF VerboseUpdateIfDefNodeStates}
      if (Pos(VFilePattern,Code.Filename)>0) and (Y>=VMinY) and (Y<=VMaxY) then
        debugln(['TSourceEditor.UpdateIfDefNodeStates y=',y,' x=',x,' Counts:Inactive=',InactiveCnt,' Active=',ActiveCnt,' Skipped=',SkippedCnt,' SET SynState=',dbgs(SynState)]);
      {$ENDIF}
      EditorComponent.SetIfdefNodeState(Y,X,SynState);
    end;
  finally
    EditorComponent.EndUpdate;
  end;
end;

function TSourceEditor.SharedEditorCount: Integer;
begin
  Result := FSharedValues.SharedEditorCount;
  if Result = 1 then
    Result := 0; // not a sharing editor
end;

function TSourceEditor.GetWordAtCurrentCaret: String;
var
  CaretPos: TPoint;
begin
  CaretPos.Y := CurrentCursorYLine;
  CaretPos.X := CurrentCursorXLine;
  Result := GetWordFromCaret(ScreenToTextPosition(CaretPos));
end;

function TSourceEditor.GetOperandFromCaret(const ACaretPos: TPoint): String;
begin
  UpdateCodeBuffer;
  if not CodeToolBoss.GetExpandedOperand(CodeBuffer, ACaretPos.X, ACaretPos.Y,
    Result, False)
  then
  if not CodeToolBoss.ExtractOperand(CodeBuffer, ACaretPos.X, ACaretPos.Y,
    Result, False, False, true)
  then
    Result := GetWordFromCaret(ACaretPos);
end;

function TSourceEditor.GetPageCaption: string;
var
  I: Integer;
begin
  I := SourceNotebook.FindPageWithEditor(Self);
  if I >= 0 then
    Result := SourceNotebook.NoteBookPages[I]
  else
    Result := FPageName;
end;

function TSourceEditor.GetPageName: string;
begin
  Result := FPageName;
end;

function TSourceEditor.GetOperandAtCurrentCaret: String;
var
  CaretPos: TPoint;
begin
  CaretPos.Y := CurrentCursorYLine;
  CaretPos.X := CurrentCursorXLine;
  Result := GetOperandFromCaret(ScreenToTextPosition(CaretPos));
end;

function TSourceEditor.GetWordFromCaret(const ACaretPos: TPoint): String;
begin
  Result := FEditor.GetWordAtRowCol(ACaretPos);
end;

function TSourceEditor.IsFirstShared(Sender: TObject): boolean;
begin
  Result:=SharedEditors[0]=Self;
end;

procedure TSourceEditor.SetVisible(Value: boolean);
begin
  if FVisible=Value then exit;
  if FEditor<>nil then FEditor.Visible:=Value;
  FVisible:=Value;
end;

procedure TSourceEditor.UnbindEditor;
// disconnect all events
var
  i: Integer;
begin
  with EditorComponent do begin
    OnStatusChange := nil;
    OnProcessCommand := nil;
    OnProcessUserCommand := nil;
    OnCommandProcessed := nil;
    OnReplaceText := nil;
    OnGutterClick := nil;
    OnSpecialLineMarkup := nil;
    OnMouseMove := nil;
    OnMouseWheel := nil;
    OnMouseDown := nil;
    OnClickLink := nil;
    OnMouseLink := nil;
    OnKeyDown := nil;
    OnKeyUp := nil;
    OnEnter := nil;
    OnPlaceBookmark := nil;
    OnClearBookmark := nil;
    OnChangeUpdating := nil;
    OnIfdefNodeStateRequest := nil;
    UnregisterMouseActionExecHandler(@EditorHandleMouseAction);
  end;
  for i := 0 to EditorComponent.PluginCount - 1 do
    if EditorComponent.Plugin[i] is TSynPluginSyncronizedEditBase then begin
      TSynPluginSyncronizedEditBase(EditorComponent.Plugin[i]).OnActivate := nil;
      TSynPluginSyncronizedEditBase(EditorComponent.Plugin[i]).OnDeactivate := nil;
    end;
  if FEditPlugin<>nil then begin
    FEditPlugin.Enabled:=false;
  end;
end;

procedure TSourceEditor.DoEditorExecuteCommand(EditorCommand: word);
begin
  EditorComponent.CommandProcessor(TSynEditorCommand(EditorCommand),' ',nil);
end;

{------------------------------------------------------------------------}
                      { TSourceNotebook }

constructor TSourceNotebook.Create(AOwner: TComponent);
var
  i: Integer;
  n: TComponent;
begin
  FPageIndex := -1;
  i := 1;
  n := AOwner.FindComponent(NonModalIDEWindowNames[nmiwSourceNoteBook]);
  while (n <> nil) do begin
    inc(i);
    n := AOwner.FindComponent(NonModalIDEWindowNames[nmiwSourceNoteBook]+IntToStr(i));
  end;

  Create(AOwner, i-1);
end;

constructor TSourceNotebook.Create(AOwner: TComponent; AWindowID: Integer);
begin
  inherited Create(AOwner);
  FManager := TSourceEditorManager(AOwner);
  FUpdateLock := 0;
  FFocusLock := 0;
  Visible := false;
  FIsClosing := False;
  FWindowID := AWindowID;
  if AWindowID > 0 then
    Name := NonModalIDEWindowNames[nmiwSourceNoteBook] + IntToStr(AWindowID+1)
  else
    Name := NonModalIDEWindowNames[nmiwSourceNoteBook];

  if AWindowID > 0 then
    FBaseCaption := locWndSrcEditor + ' (' + IntToStr(AWindowID+1) + ')'
  else
    FBaseCaption := locWndSrcEditor;
  Caption := FBaseCaption;
  KeyPreview := true;
  FProcessingCommand := false;

  FSourceEditorList := TFPList.Create;
  FHistoryList := TFPList.Create;
  FSrcEditsSortedForFilenames := TAvlTree.Create(@CompareSrcEditIntfWithFilename);

  FHistoryDlg := TBrowseEditorTabHistoryDialog.CreateNew(Self);
  FOnEditorPageCaptionUpdate := TMethodList.Create;

  OnDropFiles := @SourceNotebookDropFiles;
  AllowDropFiles:=true;

  // popup menu
  BuildPopupMenu;

  FUpdateTabAndPageTimer := TTimer.Create(Self);
  FUpdateTabAndPageTimer.Interval := 500;
  FUpdateTabAndPageTimer.OnTimer := @UpdateTabsAndPageTimeReached;

  CreateNotebook;

  Application.AddOnDeactivateHandler(@OnApplicationDeactivate);
  Application.AddOnMinimizeHandler(@OnApplicationDeactivate);

  FStopBtnIdx := IDEImages.LoadImage('menu_stop');

  {$IFNDEF LCLGtk2}
  try
    Icon.LoadFromResourceName(HInstance, 'WIN_SOURCEEDITOR');
  except
  end;
  {$ENDIF}
end;

destructor TSourceNotebook.Destroy;
var
  i: integer;
begin
  DebugLnEnter(SRCED_CLOSE, ['TSourceNotebook.Destroy ']);
  if assigned(Manager) then
    Manager.RemoveWindow(Self);
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TSourceNotebook.Destroy'){$ENDIF};
  FProcessingCommand:=false;

  for i:=FSourceEditorList.Count-1 downto 0 do
    Editors[i].Free;
  FreeAndNil(FSourceEditorList);
  FreeAndNil(FHistoryList);
  FreeAndNil(FOnEditorPageCaptionUpdate);
  FreeAndNil(FSrcEditsSortedForFilenames);

  Application.RemoveOnDeactivateHandler(@OnApplicationDeactivate);
  Application.RemoveOnMinimizeHandler(@OnApplicationDeactivate);
  FreeAndNil(FNotebook);

  inherited Destroy;
  DebugLnExit(SRCED_CLOSE, ['TSourceNotebook.Destroy ']);
end;

procedure TSourceNotebook.CreateNotebook;
var
  APage: TTabSheet;
Begin
  {$IFDEF IDE_DEBUG}
  debugln('[TSourceNotebook.CreateNotebook] START');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}
  CheckHeapWrtMemCnt('[TSourceNotebook.CreateNotebook] A ');
  {$ENDIF}
  FNotebook := TExtendedNotebook.Create(self);
  {$IFDEF IDE_DEBUG}
  debugln('[TSourceNotebook.CreateNotebook] B');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}
  CheckHeapWrtMemCnt('[TSourceNotebook.CreateNotebook] B ');
  {$ENDIF}
  FPageIndex := -1;
  with FNotebook do Begin
    Name:='SrcEditNotebook';
    Parent := Self;
    {$IFDEF IDE_DEBUG}
    debugln('[TSourceNotebook.CreateNotebook] C');
    {$ENDIF}
    Align := alClient;
    APage:=TTabSheet.Create(FNotebook);
    APage.Caption:='unit1';
    APage.Parent:=FNotebook;
    PageIndex := 0;   // Set it to the first page
    Options:=Options+[nboHidePageListPopup]; // hide default popup menu, we show custom in mouse up
    if EditorOpts.ShowTabCloseButtons then
      Options:=Options+[nboShowCloseButtons]
    else
      Options:=Options-[nboShowCloseButtons];
    MultiLine := EditorOpts.MultiLineTab;
    if Manager<>nil then
      ShowTabs := Manager.ShowTabs
    else
      ShowTabs := True;
    if ShowTabs then
      TabPosition := EditorOpts.TabPosition;
    OnChange := @NotebookPageChanged;
    OnCloseTabClicked  := @CloseTabClicked;
    OnMouseDown:=@NotebookMouseDown;
    OnMouseUp:=@NotebookMouseUp;
    TabDragMode := dmAutomatic;
    OnTabDragOverEx  := @NotebookDragOverEx;
    OnTabDragDropEx  := @NotebookDragDropEx;
    OnTabDragOver    := @NotebookDragOver;
    OnTabEndDrag     := @NotebookEndDrag;
    ShowHint:=true;
    OnShowHint:=@NotebookShowTabHint;
    {$IFDEF IDE_DEBUG}
    debugln('[TSourceNotebook.CreateNotebook] D');
    {$ENDIF}
    Visible := False;
  end; //with
  {$IFDEF IDE_DEBUG}
  debugln('[TSourceNotebook.CreateNotebook] END');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}
  CheckHeapWrtMemCnt('[TSourceNotebook.CreateNotebook] END ');
  {$ENDIF}
End;

type
  TLineEnding = (leLF, leCR, leCRLF);
const
  LE_Strs : array [TLineEnding] of String = (#10, #13, #13#10);

procedure TSourceNotebook.LineEndingClicked(Sender: TObject);
var
  IDEMenuItem: TIDEMenuItem;
  SrcEdit: TSourceEditor;
  NewLineEnding: String;
  OldLineEnding: String;
begin
  SrcEdit:=GetActiveSE;
  if SrcEdit=nil then exit;
  if not (Sender is TIDEMenuItem) then exit;
  if SrcEdit.CodeBuffer=nil then exit;

  IDEMenuItem:=TIDEMenuItem(Sender);
  NewLineEnding:=LE_Strs[TLineEnding(IDEMenuItem.Tag)];
  DebugLn(['TSourceNotebook.LineEndingClicked NewLineEnding=',NewLineEnding]);
  OldLineEnding:=SrcEdit.CodeBuffer.DiskLineEnding;
  if OldLineEnding='' then
    OldLineEnding:=LineEnding;
  if NewLineEnding<>SrcEdit.CodeBuffer.DiskLineEnding then begin
    DebugLn(['TSourceNotebook.LineEndingClicked Old=',dbgstr(OldLineEnding),' New=',dbgstr(NewLineEnding)]);
    // change file
    SrcEdit.CodeBuffer.DiskLineEnding:=NewLineEnding;
    SrcEdit.CodeBuffer.Source:=
                    ChangeLineEndings(SrcEdit.CodeBuffer.Source,NewLineEnding);
    SrcEdit.CodeBuffer.Modified:=true;
    SrcEdit.Modified:=true;
  end;
end;

procedure TSourceNotebook.EncodingClicked(Sender: TObject);
var
  IDEMenuItem: TIDEMenuItem;
  SrcEdit: TSourceEditor;
  NewEncoding: String;
  OldEncoding: String;
  CurResult: TModalResult;
begin
  SrcEdit:=GetActiveSE;
  if SrcEdit=nil then exit;
  if Sender is TIDEMenuItem then begin
    IDEMenuItem:=TIDEMenuItem(Sender);
    NewEncoding:=IDEMenuItem.Caption;
    if SysUtils.CompareText(copy(NewEncoding,1,length(EncodingAnsi)+2),EncodingAnsi+' (')=0
    then begin
      // the ansi encoding is shown as 'ansi (system encoding)' -> cut
      NewEncoding:=EncodingAnsi;
    end else if NewEncoding=lisUtf8WithBOM then begin
      NewEncoding:=EncodingUTF8BOM;
    end;
    DebugLn(['Hint: (lazarus) TSourceNotebook.EncodingClicked NewEncoding=',NewEncoding]);
    if SrcEdit.CodeBuffer<>nil then begin
      OldEncoding:=NormalizeEncoding(SrcEdit.CodeBuffer.DiskEncoding);
      if OldEncoding='' then
        OldEncoding:=GetDefaultTextEncoding;
      if NewEncoding<>SrcEdit.CodeBuffer.DiskEncoding then begin
        DebugLn(['Hint: (lazarus) TSourceNotebook.EncodingClicked Old=',OldEncoding,' New=',NewEncoding]);
        if SrcEdit.ReadOnly then begin
          if SrcEdit.CodeBuffer.IsVirtual then
            CurResult:=mrCancel
          else
            CurResult:=IDEQuestionDialog(lisChangeEncoding,
              Format(lisEncodingOfFileOnDiskIsNewEncodingIs,
                     [SrcEdit.CodeBuffer.Filename, LineEnding, OldEncoding, NewEncoding]),
              mtConfirmation, [mrOk, lisReopenWithAnotherEncoding, mrCancel]);
        end else begin
          if SrcEdit.CodeBuffer.IsVirtual then
            CurResult:=IDEQuestionDialog(lisChangeEncoding,
              Format(lisEncodingOfFileOnDiskIsNewEncodingIs,
                     [SrcEdit.CodeBuffer.Filename, LineEnding, OldEncoding, NewEncoding]),
              mtConfirmation, [mrYes, lisChangeFile, mrCancel])
          else
            CurResult:=IDEQuestionDialog(lisChangeEncoding,
              Format(lisEncodingOfFileOnDiskIsNewEncodingIs,
                     [SrcEdit.CodeBuffer.Filename, LineEnding, OldEncoding, NewEncoding]),
              mtConfirmation, [mrYes,lisChangeFile,mrOk,lisReopenWithAnotherEncoding,mrCancel]);
        end;
        if CurResult=mrYes then begin
          // change file
          SrcEdit.CodeBuffer.DiskEncoding:=NewEncoding;
          SrcEdit.CodeBuffer.Modified:=true;
          // set override
          InputHistoriesSO.FileEncodings[SrcEdit.CodeBuffer.Filename]:=NewEncoding;
          DebugLn(['Hint: (lazarus) TSourceNotebook.EncodingClicked Change file to ',SrcEdit.CodeBuffer.DiskEncoding]);
          if (not SrcEdit.CodeBuffer.IsVirtual)
          and (LazarusIDE.DoSaveEditorFile(SrcEdit, []) <> mrOk)
          then begin
            DebugLn(['Hint: (lazarus) TSourceNotebook.EncodingClicked LazarusIDE.DoSaveEditorFile failed']);
          end;
        end else if CurResult=mrOK then begin
          // reopen with another encoding
          if SrcEdit.Modified then begin
            if IDEQuestionDialog(lisAbandonChanges,
              Format(lisAllYourModificationsToWillBeLostAndTheFileReopened,
                     [SrcEdit.CodeBuffer.Filename, LineEnding]),
              mtConfirmation,[mbOk,mbAbort],'')<>mrOk
            then begin
              exit;
            end;
          end;
          // set override
          InputHistoriesSO.FileEncodings[SrcEdit.CodeBuffer.Filename]:=NewEncoding;
          if not SrcEdit.CodeBuffer.Revert then begin
            IDEMessageDialog(lisCodeToolsDefsReadError,
              Format(lisUnableToRead, [SrcEdit.CodeBuffer.Filename]),
              mtError,[mbCancel],'');
            exit;
          end;
          SrcEdit.EditorComponent.BeginUpdate;
          SrcEdit.CodeBuffer.AssignTo(SrcEdit.EditorComponent.Lines,False);
          SrcEdit.EditorComponent.EndUpdate;
        end;
      end;
    end;
  end;
end;

procedure TSourceNotebook.HighlighterClicked(Sender: TObject);
var
  IDEMenuItem: TIDEMenuItem;
  i: LongInt;
  SrcEdit: TSourceEditor;
  h: TLazSyntaxHighlighter;
begin
  SrcEdit:=GetActiveSE;
  if SrcEdit=nil then exit;
  if Sender is TIDEMenuItem then begin
    IDEMenuItem:=TIDEMenuItem(Sender);
    i:=IDEMenuItem.SectionIndex;
    if (i>=ord(Low(TLazSyntaxHighlighter)))
    and (i<=ord(High(TLazSyntaxHighlighter))) then begin
      h:=TLazSyntaxHighlighter(i);
      SrcEdit.SyntaxHighlighterType:=h;
      SrcEdit.UpdateProjectFile([sepuChangedHighlighter]);
    end;
  end;
end;

procedure TSourceNotebook.TabPopUpMenuPopup(Sender: TObject);
var
  ASrcEdit: TSourceEditor;

  {$IFnDEF SingleSrcWindow}
  function ToWindow(ASection: TIDEMenuSection; const OpName: string;
                    const OnClickMethod: TNotifyEvent; WinForFind: Boolean = False): Boolean;
  var
    i, ThisWin, SharedEditor: Integer;
    nb: TSourceNotebook;
  begin
    Result := False;
    ASection.Clear;
    ThisWin := Manager.IndexOfSourceWindow(self);
    for i := 0 to Manager.SourceWindowCount - 1 do begin
      nb:=Manager.SourceWindows[i];
      SharedEditor:=nb.IndexOfEditorInShareWith(ASrcEdit);
      if (i <> ThisWin) and ((SharedEditor < 0) <> WinForFind) then begin
        Result := True;
        with RegisterIDEMenuCommand(ASection,OpName+IntToStr(i),nb.Caption,OnClickMethod) do
          Tag := i;
      end;
    end;
  end;
  {$ENDIF}
  procedure AddEditorToMenuSection(AEditor: TSourceEditor; AMenu: TIDEMenuSection; AIndex: Integer);
  var
    S: String;
  begin
    S := AEditor.PageName;
    if AEditor.Modified then
      S := '*'+S;

    RegisterIDEMenuCommand(AMenu, 'File'+IntToStr(AIndex),
             S, @ExecuteEditorItemClick, nil, nil, '', PtrUInt(AEditor));
  end;

var
  PageCtrl: TPageControl;
  PopM: TPopupMenu;
  PageI: integer;
  i: Integer;
  S: String;
  EditorCur: TSourceEditor;
  P: TIDEPackage;
  RecMenu, ProjMenu, M: TIDEMenuSection;
  EdList: TStringList;
begin
  PopM:=TPopupMenu(Sender);
  SourceTabMenuRoot.MenuItem:=PopM.Items;
  // Get the tab that was clicked
  if PopM.PopupComponent is TPageControl then begin
    PageCtrl:=TPageControl(PopM.PopupComponent);
    PageI:=PageCtrl.IndexOfPageAt(PageCtrl.ScreenToClient(PopM.PopupPoint));
    if (PageI>=0) and (PageI<PageCtrl.PageCount) then
      PageIndex := PageI  // Todo: This should be in MouseDown / or both, whichever is first
    else
      DebugLn(['TSourceNotebook.TabPopUpMenuPopup: Popup PageIndex=', PageI]);
  end;
  ASrcEdit:=ActiveEditor as TSourceEditor;

  {$IFnDEF SingleSrcWindow}
  // Multi win
  ToWindow(SrcEditMenuMoveToOtherWindowList, 'MoveToWindow',
                     @SrcEditMenuMoveToExistingWindowClicked);
  ToWindow(SrcEditMenuCopyToOtherWindowList, 'CopyToWindow',
                     @SrcEditMenuCopyToExistingWindowClicked);
  ToWindow(SrcEditMenuFindInOtherWindowList, 'FindInWindow',
                     @SrcEditMenuFindInWindowClicked, True);
  {$ENDIF}

  SrcEditMenuSectionEditors.Clear;
  if Manager <> nil then begin
    EdList := TStringList.Create;
    EdList.OwnsObjects := False;
    EdList.Sorted := True;
    // sort
    for i := 0 to EditorCount - 1 do
      EdList.AddObject(Editors[i].PageName+' '+Editors[i].FileName, Editors[i]);


    RecMenu := RegisterIDESubMenu(SrcEditMenuSectionEditors, lisRecentTabs, lisRecentTabs);
    RecMenu.Visible := False;
    ProjMenu := RegisterIDESubMenu(SrcEditMenuSectionEditors, dlgEnvProject, dlgEnvProject);
    ProjMenu.Visible := False;
    RegisterIDESubMenu(SrcEditMenuSectionEditors, lisMEOther, lisMEOther).Visible := False;

    //first add all pages in the correct order since the editor order can be different from the tab order
    for i := 0 to EdList.Count - 1 do
    begin
      EditorCur := TSourceEditor(EdList.Objects[i]);
      s := lisMEOther;
      P := nil;
      if (EditorCur.GetProjectFile <> nil) and (EditorCur.GetProjectFile.IsPartOfProject) then
        s := dlgEnvProject
      else begin
        Manager.OnPackageForSourceEditor(P, EditorCur);
        if P <> nil then
          s := Format(lisTabsFor, [p.Name]);
      end;

      if SrcEditMenuSectionEditors.FindByName(S) is TIDEMenuSection then begin
        M := TIDEMenuSection(SrcEditMenuSectionEditors.FindByName(S))
      end else begin
        M := RegisterIDESubMenu(SrcEditMenuSectionEditors, S, S);
        M.UserTag := PtrUInt(P);
      end;
      M.Visible := True;

      AddEditorToMenuSection(EditorCur, M, i);
      // use tag to count modified
      if EditorCur.Modified then M.Tag := m.Tag + 1;
    end;

    EdList.Free;

    // add recent tabs. skip 0 since that is the active tab
    for i := 1 to Min(10, FHistoryList.Count-1) do
    begin
      EditorCur := FindSourceEditorWithPageIndex(FNotebook.IndexOf(TCustomPage(FHistoryList[i])));
      if (EditorCur = nil) or (not EditorCur.FEditor.HandleAllocated) then continue; // show only if it was visited
      AddEditorToMenuSection(EditorCur, RecMenu, i);
      RecMenu.Visible := True;
    end;

    for i := 0 to SrcEditMenuSectionEditors.Count - 1 do begin
      if SrcEditMenuSectionEditors.Items[i] is TIDEMenuSection then begin
        M := SrcEditMenuSectionEditors.Items[i] as TIDEMenuSection;

        if M.Tag = 0 then
          M.Caption := M.Caption +  Format(' (%d)', [M.Count])
        else
          M.Caption := M.Caption +  Format(' (*%d/%d)', [M.Tag, M.Count]);

        if M.UserTag <> 0 then
          RegisterIDEMenuCommand(
                  RegisterIDEMenuSection(M as TIDEMenuSection, 'Open lpk sect '+TIDEPackage(M.UserTag).Filename),
                 'Open lpk '+TIDEPackage(M.UserTag).Filename,
                 lisCompPalOpenPackage, @OnPopupOpenPackageFile, nil, nil, '', M.UserTag);
      end;
    end;

    if ProjMenu.Visible then begin
      RegisterIDEMenuCommand(
              RegisterIDEMenuSection(ProjMenu, 'Open proj sect '),
             'Open proj', lisOpenProject2, @OnPopupOpenProjectInsp);
    end;

  end;
end;

procedure TSourceNotebook.SrcPopUpMenuPopup(Sender: TObject);
var
  ASrcEdit: TSourceEditor;
  CurFilename: String;

  function MaybeAddPopup(const ASuffix: String; ANewOnClick: TNotifyEvent = nil;
    Filename: string = ''): TIDEMenuItem;
  begin
    Result:=nil;
    if ANewOnClick=nil then
      ANewOnClick:=@OnPopupMenuOpenFile;
    if Filename='' then
      Filename:=CurFilename;
    Filename:=ChangeFileExt(Filename,ASuffix);
    if FileExistsCached(Filename) then begin
      Filename:=CreateRelativePath(Filename,ExtractFilePath(ASrcEdit.FileName));
      Result:=AddContextPopupMenuItem(Format(lisOpenLfm,[Filename]), true, ANewOnClick);
    end;
  end;

var
  FPDocSrc, ShortFileName: String;
  EditorComp: TSynEdit;
  MainCodeBuf: TCodeBuffer;
  AnOwner: TObject;
  Marks: PSourceMark;
  i, MarkCount: integer;
  EditorPopupPoint, EditorCaret: TPoint;
begin
  IDECommandList.ExecuteUpdateEvents;

  SrcPopupMenu.Items.Clear;
  SourceEditorMenuRoot.MenuItem:=SrcPopupMenu.Items;
  RemoveUserDefinedMenuItems;
  RemoveContextMenuItems;

  ASrcEdit:=FindSourceEditorWithEditorComponent(TPopupMenu(Sender).PopupComponent);
  Assert(Assigned(ASrcEdit), 'TSourceNotebook.SrcPopUpMenuPopup: ASrcEdit=nil');
  Assert((ASrcEdit=GetActiveSE), 'TSourceNotebook.SrcPopUpMenuPopup: ASrcEdit<>GetActiveSE');
  EditorComp:=ASrcEdit.EditorComponent;

  SrcEditMenuReadOnly.Checked:=ASrcEdit.ReadOnly;
  SrcEditMenuShowLineNumbers.Checked := ASrcEdit.EditorComponent.Gutter.LineNumberPart.Visible;
  SrcEditMenuDisableI18NForLFM.Visible:=false;

  UpdateHighlightMenuItems(ASrcEdit);
  UpdateEncodingMenuItems(ASrcEdit);
  UpdateLineEndingMenuItems(ASrcEdit);

  // ask Codetools
  CurFilename:=ASrcEdit.FileName;
  ShortFileName:=ExtractFileName(CurFilename);
  MainCodeBuf:=nil;
  if FilenameIsPascalUnit(ShortFileName)
  or (CompareFileExt(ShortFileName,'.inc',true)=0) then
    MainCodeBuf:=CodeToolBoss.GetMainCode(ASrcEdit.CodeBuffer)
  else if FilenameIsPascalSource(ShortFileName) then
    MainCodeBuf:=ASrcEdit.CodeBuffer;

  if (FilenameIsAbsolute(CurFilename)) then begin
    if (MainCodeBuf<>nil) and (MainCodeBuf<>ASrcEdit.CodeBuffer)
    and (not MainCodeBuf.IsVirtual) then begin
      // this is an include file => add link to open unit
      CurFilename:=MainCodeBuf.Filename;
      ShortFileName:=ExtractFileName(CurFilename);
      AddContextPopupMenuItem(
        Format(lisOpenLfm,
               [CreateRelativePath(CurFilename,ExtractFilePath(ASrcEdit.Filename))]),
        true,@OnPopupMenuOpenFile);
    end;
    if FilenameIsPascalUnit(ShortFileName) then begin
      MaybeAddPopup('.lfm');
      MaybeAddPopup('.dfm');
      MaybeAddPopup('.lrs');
      MaybeAddPopup('.s');
    end;
    // ToDo: unit resources
    if (CompareFileExt(ShortFileName,'.lfm',true)=0)
    or (CompareFileExt(ShortFileName,'.dfm',true)=0) then begin
      MaybeAddPopup('.pas');
      MaybeAddPopup('.pp');
      MaybeAddPopup('.p');
    end;
    if (CompareFileExt(ShortFileName,'.lpi',true)=0)
    or (CompareFileExt(ShortFileName,'.lpk',true)=0) then begin
      AddContextPopupMenuItem(
        Format(lisOpenLfm,[ShortFileName]),true,@OnPopupMenuOpenFile);
    end;
    FPDocSrc:=LazarusHelp.GetFPDocFilenameForSource(CurFilename,false,AnOwner);
    if FPDocSrc<>'' then
      AddContextPopupMenuItem(
        Format(lisOpenLfm,
               [CreateRelativePath(FPDocSrc,ExtractFilePath(CurFilename))]),
        true,@OnPopupMenuOpenFile);
  end;

  EditorPopupPoint:=EditorComp.ScreenToClient(SrcPopUpMenu.PopupPoint);
  if EditorPopupPoint.X<=EditorComp.Gutter.Width then begin
    EditorCaret := EditorComp.PhysicalToLogicalPos(EditorComp.PixelsToRowColumn(EditorPopupPoint));
    // user clicked on gutter
    SourceEditorMarks.GetMarksForLine(ASrcEdit, EditorCaret.y, Marks, MarkCount);
    if Marks <> nil then begin
      for i := 0 to MarkCount-1 do
        Marks[i].CreatePopupMenuItems(@AddUserDefinedPopupMenuItem);
      FreeMem(Marks);
    end;
    if (EditorCaret.Y<=EditorComp.Lines.Count)
    and (MessagesView<>nil) then
      MessagesView.SourceEditorPopup(EditorComp.Marks.Line[EditorCaret.Y],
        EditorComp.LogicalCaretXY);
  end;

  if Assigned(Manager.OnPopupMenu) then
    Manager.OnPopupMenu(@AddContextPopupMenuItem);
  SourceEditorMenuRoot.NotifySubSectionOnShow(Self);
  //SrcPopupMenu.Items.WriteDebugReport('TSourceNotebook.SrcPopUpMenuPopup() ');
end;

procedure TSourceNotebook.DbgPopUpMenuPopup(Sender: TObject);
begin
  SrcEditSubMenuDebug.MenuItem:=DbgPopUpMenu.Items;
end;

procedure TSourceNotebook.NotebookShowTabHint(Sender: TObject;
  HintInfo: PHintInfo);
var
  Tabindex: integer;
  ASrcEdit: TSourceEditor;
begin
  if (PageCount=0) or (HintInfo=nil) then exit;
  TabIndex:=FNoteBook.IndexOfPageAt(FNotebook.ScreenToClient(Mouse.CursorPos));
  if TabIndex<0 then exit;
  ASrcEdit:=FindSourceEditorWithPageIndex(TabIndex);
  if ASrcEdit=nil then exit;
  if ASrcEdit.CodeBuffer<>nil then begin
    HintInfo^.HintStr:=ASrcEdit.CodeBuffer.Filename;
  end;
end;

function TSourceNotebook.GetItems(Index: integer): TSourceEditorInterface;
begin
  Result:=TSourceEditorInterface(FSourceEditorList[Index]);
end;

procedure TSourceNotebook.BuildPopupMenu;
begin
  //debugln('TSourceNotebook.BuildPopupMenu');

  TabPopUpMenu := TPopupMenu.Create(Self);
  with TabPopupMenu do
  begin
    AutoPopup := True;
    OnPopup :=@TabPopupMenuPopup;
    Images := IDEImages.Images_16;
  end;

  SrcPopupMenu := TPopupMenu.Create(Self);
  with SrcPopupMenu do
  begin
    AutoPopup := True;
    OnPopup :=@SrcPopupMenuPopup;
    Images := IDEImages.Images_16;
  end;

  DbgPopUpMenu := TPopupMenu.Create(Self);
  with DbgPopupMenu do
  begin
    AutoPopup := True;
    OnPopup :=@DbgPopupMenuPopup;
    Images := IDEImages.Images_16;
  end;

  GoToLineMenuItem.Caption := lisMenuGotoLine;
  OpenFolderMenuItem.Caption := lisMenuOpenFolder;
  {$IFDEF VerboseMenuIntf}
  SrcPopupMenu.Items.WriteDebugReport('TSourceNotebook.BuildPopupMenu ');
  SourceTabMenuRoot.ConsistencyCheck;
  SourceEditorMenuRoot.ConsistencyCheck;
  {$ENDIF}
end;

procedure TSourceNotebook.CallOnEditorPageCaptionUpdate(Sender: TObject);
begin
  FOnEditorPageCaptionUpdate.CallNotifyEvents(Sender);
end;

function TSourceNotebook.GetNoteBookPage(Index: Integer): TTabSheet;
begin
  if FNotebook.Visible then
    Result := FNotebook.Pages[Index]
  else
    Result := nil;
end;

function TSourceNotebook.GetNotebookPages: TStrings;
begin
  if FNotebook.Visible then
    Result := TCustomTabControl(FNotebook).Pages
  else
    Result := nil;
end;

function TSourceNotebook.GetPageCount: Integer;
begin
  If FNotebook.Visible then
    Result := FNotebook.PageCount
  else
    Result := 0;
end;

function TSourceNotebook.GetPageIndex: Integer;
begin
  if FUpdateLock > 0 then
    Result := FPageIndex
  else
  if FNotebook.Visible then
    Result := FNotebook.PageIndex
  else
    Result := -1
end;

function TSourceNotebook.GetWindowID: Integer;
begin
  Result := FWindowID;
end;

procedure TSourceNotebook.SetPageIndex(AValue: Integer);
begin
  if (fPageIndex = AValue) and (FNotebook.PageIndex = AValue) then begin
    //debugln(['>> TSourceNotebook.SetPageIndex PageIndex=', PageIndex, ' FPageIndex=', FPageIndex, ' Value=', AValue, ' FUpdateLock=', FUpdateLock]);
    //DumpStack;
    exit;
  end;
  DebugLnEnter(SRCED_PAGES, ['>> TSourceNotebook.SetPageIndex Cur-PgIdx=', PageIndex, ' FPageIndex=', FPageIndex, ' Value=', AValue, ' FUpdateLock=', FUpdateLock]);
  //debugln(['>> TSourceNotebook.SetPageIndex CHANGE PageIndex=', PageIndex, ' FPageIndex=', FPageIndex, ' Value=', AValue, ' FUpdateLock=', FUpdateLock]);
  FPageIndex := AValue;
  if FUpdateLock = 0 then
    ApplyPageIndex
  else
    Include(FUpdateFlags,ufPageIndexChanged);
  DebugLnExit(SRCED_PAGES, ['<< TSourceNotebook.SetPageIndex ']);
end;

procedure TSourceNotebook.UpdateHighlightMenuItems(SrcEdit: TSourceEditor);
var
  h: TLazSyntaxHighlighter;
  i: Integer;
  CurName: String;
  CurCaption: String;
  IDEMenuItem: TIDEMenuItem;
begin
  SrcEditSubMenuHighlighter.ChildrenAsSubMenu:=true;
  i:=0;
  for h:=Low(TLazSyntaxHighlighter) to High(TLazSyntaxHighlighter) do begin
    CurName:='Highlighter'+IntToStr(i);
    CurCaption:=GetSyntaxHighlighterCaption(h);
    if SrcEditSubMenuHighlighter.Count=i then begin
      // add new item
      IDEMenuItem:=RegisterIDEMenuCommand(SrcEditSubMenuHighlighter,
                             CurName,CurCaption,@HighlighterClicked);
    end else begin
      IDEMenuItem:=SrcEditSubMenuHighlighter[i];
      IDEMenuItem.Caption:=CurCaption;
      IDEMenuItem.OnClick:=@HighlighterClicked;
    end;
    if IDEMenuItem is TIDEMenuCommand then
      TIDEMenuCommand(IDEMenuItem).Checked:=(SrcEdit<>nil)
                                          and (SrcEdit.SyntaxHighlighterType=h);
    inc(i);
  end;
end;

procedure TSourceNotebook.UpdateLineEndingMenuItems(SrcEdit: TSourceEditor);
var
  le: TLineEnding;
  FileEndings: String;
  IDEMenuItem: TIDEMenuCommand;
const
  LE_Names : array [TLineEnding] of String =(
    'LF (Unix, Linux)',
    'CR (Mac)',
    'CRLF (Win, DOS)'
  );
begin
  SrcEditSubMenuLineEnding.ChildrenAsSubMenu:=true;
  if (SrcEdit<>nil) and (SrcEdit.CodeBuffer<>nil) then
    FileEndings:=SrcEdit.CodeBuffer.DiskLineEnding
  else
    FileEndings:=LineEnding;
  //DebugLn(['TSourceNotebook.UpdateEncodingMenuItems ',Encoding]);
  for le:=low(TLineEnding) to High(TLineEnding) do begin
    if SrcEditSubMenuLineEnding.Count=Ord(le) then begin
      // add new item
      IDEMenuItem:=RegisterIDEMenuCommand(SrcEditSubMenuLineEnding,
        'LineEnding'+IntToStr(Ord(le)),LE_Names[le],@LineEndingClicked);
    end else begin
      IDEMenuItem:=SrcEditSubMenuLineEnding[Ord(le)] as TIDEMenuCommand;
      IDEMenuItem.Caption:=LE_Names[le];
      IDEMenuItem.OnClick:=@LineEndingClicked;
    end;
    IDEMenuItem.Tag:=Ord(le);
    IDEMenuItem.Checked:=(FileEndings=LE_Strs[le]);
  end;
end;

procedure TSourceNotebook.UpdatePageNames;
var
  i: Integer;
begin
  if FUpdateLock > 0 then begin
    include(FUpdateFlags, ufPageNames);
    exit;
  end;
  for i := 0 to EditorCount - 1 do
    Editors[i].UpdatePageName;
  UpdateTabsAndPageTitle;
end;

procedure TSourceNotebook.UpdateProjectFiles(ACurrentEditor: TSourceEditor = nil);
var
  i: Integer;
begin
  if FUpdateLock > 0 then begin
    if ACurrentEditor <> nil then
      ACurrentEditor.UpdateProjectFile;
    include(FUpdateFlags, ufProjectFiles);
    exit;
  end;
  for i := 0 to EditorCount - 1 do
    Editors[i].UpdateProjectFile;
end;

procedure TSourceNotebook.UpdateEncodingMenuItems(SrcEdit: TSourceEditor);
var
  List: TStringList;
  i: Integer;
  Encoding: String;
  CurEncoding: string;
  CurName: String;
  CurCaption: String;
  IDEMenuItem: TIDEMenuItem;
  SysEncoding: String;
begin
  SrcEditSubMenuEncoding.ChildrenAsSubMenu:=true;
  Encoding:='';
  if SrcEdit<>nil then begin
    if SrcEdit.CodeBuffer<>nil then
      Encoding:=NormalizeEncoding(SrcEdit.CodeBuffer.DiskEncoding);
  end;
  if Encoding='' then
    Encoding:=GetDefaultTextEncoding;
  //DebugLn(['TSourceNotebook.UpdateEncodingMenuItems ',Encoding]);
  List:=TStringList.Create;
  GetSupportedEncodings(List);
  for i:=0 to List.Count-1 do begin
    CurName:='Encoding'+IntToStr(i);
    CurEncoding:=List[i];
    CurCaption:=CurEncoding;
    if SysUtils.CompareText(CurEncoding,EncodingAnsi)=0 then begin
      SysEncoding:=GetDefaultTextEncoding;
      if (SysEncoding<>'') and (SysUtils.CompareText(SysEncoding,EncodingAnsi)<>0)
      then
        CurCaption:=CurCaption+' ('+GetDefaultTextEncoding+')';
    end;
    if CurEncoding='UTF-8BOM' then begin
      CurCaption:=lisUtf8WithBOM;
    end;
    if SrcEditSubMenuEncoding.Count=i then begin
      // add new item
      IDEMenuItem:=RegisterIDEMenuCommand(SrcEditSubMenuEncoding,
                             CurName,CurCaption,@EncodingClicked);
    end else begin
      IDEMenuItem:=SrcEditSubMenuEncoding[i];
      IDEMenuItem.Caption:=CurCaption;
      IDEMenuItem.OnClick:=@EncodingClicked;
    end;
    if IDEMenuItem is TIDEMenuCommand then
      TIDEMenuCommand(IDEMenuItem).Checked:=
        Encoding=NormalizeEncoding(CurEncoding);
  end;
  List.Free;
end;

procedure TSourceNotebook.RemoveUserDefinedMenuItems;
begin
  SrcEditMenuSectionFirstDynamic.Clear;
end;

function TSourceNotebook.AddUserDefinedPopupMenuItem(const NewCaption: string;
  const NewEnabled: boolean; const NewOnClick: TNotifyEvent): TIDEMenuItem;
begin
  Result:=RegisterIDEMenuCommand(SrcEditMenuSectionFirstDynamic.GetPath,
    'Dynamic',NewCaption,NewOnClick);
  Result.Enabled:=NewEnabled;
end;

procedure TSourceNotebook.RemoveContextMenuItems;
begin
  SrcEditMenuSectionFileDynamic.Clear;
  {$IFDEF VerboseMenuIntf}
  SrcEditMenuSectionFileDynamic.WriteDebugReport('TSourceNotebook.RemoveContextMenuItems ', true);
  {$ENDIF}
end;

procedure TSourceNotebook.RemoveUpdateEditorPageCaptionHandler(
  AEvent: TNotifyEvent);
begin
  FOnEditorPageCaptionUpdate.Remove(TMethod(AEvent));
end;

function TSourceNotebook.AddContextPopupMenuItem(const NewCaption: string;
  const NewEnabled: boolean; const NewOnClick: TNotifyEvent): TIDEMenuItem;
begin
  Result:=RegisterIDEMenuCommand(SrcEditMenuSectionFileDynamic.GetPath,
                                 'FileDynamic',NewCaption,NewOnClick);
  Result.Enabled:=NewEnabled;
end;

procedure TSourceNotebook.AddUpdateEditorPageCaptionHandler(
  AEvent: TNotifyEvent; const AsLast: Boolean);
begin
  FOnEditorPageCaptionUpdate.Add(TMethod(AEvent), AsLast);
end;

{-------------------------------------------------------------------------------
  Procedure TSourceNotebook.EditorChanged
  Params: Sender: TObject
  Result: none

  Called whenever an editor status changes. Sender is normally a TSynEdit.
-------------------------------------------------------------------------------}
procedure TSourceNotebook.EditorChanged(Sender: TObject;
  Changes: TSynStatusChanges);
var SenderDeleted: boolean;
Begin
  SenderDeleted:=(Sender as TControl).Parent=nil;
  if SenderDeleted then exit;
  UpdateStatusBar;
  if Assigned(Manager) then begin
    if not(Changes=[scFocus]) then // has to be here because of issue #29726
      Manager.FHints.HideIfVisible;
    Manager.DoEditorStatusChanged(FindSourceEditorWithEditorComponent(TSynEdit(Sender)));
  end;
End;

procedure TSourceNotebook.DoClose(var CloseAction: TCloseAction);
var
  Layout: TSimpleWindowLayout;
begin
  DebugLnEnter(SRCED_CLOSE, ['TSourceNotebook.DoClose ', DbgSName(self)]);
  inherited DoClose(CloseAction);
  CloseAction := caHide;
  {$IFnDEF SingleSrcWindow}
  if (PageCount = 0) and (Parent=nil) then begin { $NOTE maybe keep the last one}
    // Make the name unique, because it may not immediately be released
    // => disconnect first
    Layout:=IDEWindowCreators.SimpleLayoutStorage.ItemByForm(Self);
    if Layout<>nil then
      Layout.Form:=nil;
    Name := Name + '___' + IntToStr({%H-}PtrUInt(Pointer(Self)));
    CloseAction := caFree;
  end
  else begin
    FIsClosing := True;
    try
      if Assigned(Manager) and Assigned(Manager.OnNoteBookCloseQuery) then
        Manager.OnNoteBookCloseQuery(Self, CloseAction);
    finally
      FIsClosing := False;
    end;
  end;
  {$ENDIF}
  DebugLnExit(SRCED_CLOSE, ['TSourceNotebook.DoClose ']);
end;

procedure TSourceNotebook.DoShow;
begin
  inherited DoShow;
  // statusbar was not updated when visible=false, update now
  if snUpdateStatusBarNeeded in States then
    UpdateStatusBar;
  if Assigned(Manager) and (Parent <> nil) then
    Manager.DoWindowShow(Self);
end;

procedure TSourceNotebook.DoHide;
begin
  inherited DoHide;
  if Assigned(Manager) and (Parent <> nil) then
    Manager.DoWindowHide(Self);
end;

function TSourceNotebook.IndexOfEditorInShareWith(
  AnOtherEditor: TSourceEditorInterface): Integer;
var
  i: Integer;
begin
  for i := 0 to EditorCount - 1 do
    if Editors[i].IsSharedWith(AnOtherEditor as TSourceEditor) then
      exit(i);
  Result := -1;
end;

function TSourceNotebook.GetActiveCompletionPlugin: TSourceEditorCompletionPlugin;
begin
  Result := Manager.ActiveCompletionPlugin;
end;

function TSourceNotebook.GetCompletionPlugins(Index: integer
  ): TSourceEditorCompletionPlugin;
begin
  Result := SourceEditorManager.CompletionPlugins[Index];
end;

function TSourceNotebook.NewSE(Pagenum: Integer; NewPagenum: Integer;
  ASharedEditor: TSourceEditor; ATabCaption: String): TSourceEditor;
begin
  {$IFDEF IDE_DEBUG}
  debugln('TSourceNotebook.NewSE A ');
  {$ENDIF}
  if Pagenum < 0 then begin
    // add a new page right to the current
    if NewPageNum >= 0 then
      PageNum := NewPageNum
    else
      Pagenum := PageIndex+1;
    Pagenum := Max(0,Min(PageNum, PageCount));
    if ATabCaption = '' then
      ATabCaption := Manager.FindUniquePageName('', nil);
    NoteBookInsertPage(PageNum, ATabCaption);
    NotebookPage[PageNum].ReAlign;
  end;
  {$IFDEF IDE_DEBUG}
  debugln(['TSourceNotebook.NewSE B  ', PageIndex,',',PageCount]);
  {$ENDIF}
  Result := TSourceEditor.Create(Self, NotebookPage[PageNum], ASharedEditor);
  Result.FPageName := NoteBookPages[Pagenum];
  AcceptEditor(Result);
  PageIndex := Pagenum;
  {$IFDEF IDE_DEBUG}
  debugln('TSourceNotebook.NewSE end ');
  {$ENDIF}
end;

procedure TSourceNotebook.AcceptEditor(AnEditor: TSourceEditor; SendEvent: Boolean);
begin
  FSourceEditorList.Add(AnEditor);
  FSrcEditsSortedForFilenames.Add(AnEditor);

  AnEditor.EditorComponent.BeginUpdate;
  AnEditor.PopupMenu := SrcPopupMenu;
  AnEditor.OnEditorChange := @EditorChanged;
  AnEditor.OnMouseMove := @EditorMouseMove;
  AnEditor.OnMouseDown := @EditorMouseDown;
  AnEditor.OnMouseWheel := @EditorMouseWheel;
  AnEditor.OnKeyDown := @EditorKeyDown;
  AnEditor.OnKeyUp := @EditorKeyUp;
  AnEditor.EditorComponent.Beautifier.OnGetDesiredIndent := @EditorGetIndent;
  AnEditor.EditorComponent.EndUpdate;

  if SendEvent then
    Manager.SendEditorCreated(AnEditor);
end;

procedure TSourceNotebook.ReleaseEditor(AnEditor: TSourceEditor; SendEvent: Boolean);
begin
  FSourceEditorList.Remove(AnEditor);
  FSrcEditsSortedForFilenames.RemovePointer(AnEditor);
  if SendEvent then
    Manager.SendEditorDestroyed(AnEditor);
end;

function TSourceNotebook.FindSourceEditorWithPageIndex(APageIndex: integer): TSourceEditor;
var
  I: integer;
  TempEditor: TControl;

  function FindSynEdit(AControl: TWinControl): TControl;
  var
    I: Integer;
  begin
    Result := nil;

    with AControl do
      for I := 0 to ControlCount - 1 do
      begin
        if Controls[I] is TIDESynEditor then
          Exit(Controls[I])
        else
        if Controls[I] is TWinControl then
        begin
          Result := FindSynEdit(TWinControl(Controls[I]));
          if Result <> nil then
            Exit;
        end;
      end;
  end;

begin
  Result := nil;
  if (FSourceEditorList=nil)
    or (APageIndex < 0) or (APageIndex >= PageCount) then exit;

  TempEditor := FindSynEdit(NotebookPage[APageIndex]);
  {
  TempEditor:=nil;
  with NotebookPage[APageIndex] do
    for I := 0 to ControlCount-1 do
      if Controls[I] is TSynEdit then
        Begin
          TempEditor := Controls[I];
          Break;
        end;
  }
  if TempEditor=nil then exit;
  I := FSourceEditorList.Count-1;
  while (I>=0) and (TSourceEditor(FSourceEditorList[I]).EditorComponent <> TempEditor) do
    dec(i);
  if i<0 then exit;
  Result := TSourceEditor(FSourceEditorList[i]);
end;

function TSourceNotebook.GetActiveSE: TSourceEditor;
Begin
  Result := nil;
  if (FSourceEditorList=nil) or (FSourceEditorList.Count=0) or (PageIndex<0) then
    exit;
  Result:=FindSourceEditorWithPageIndex(PageIndex);
end;

function TSourceNotebook.GetActiveEditor: TSourceEditorInterface;
begin
  Result:=GetActiveSE;
end;

procedure TSourceNotebook.SetActiveEditor(const AValue: TSourceEditorInterface);
var
  i: integer;
begin
  i := FindPageWithEditor(AValue as TSourceEditor);
  inc(FFocusLock);
  if i>= 0 then
    PageIndex := i;
  dec(FFocusLock);
  SourceEditorManager.ActiveSourceWindow := Self;
  if EditorOpts.ShowFileNameInCaption then
  begin
    if ActiveEditor<>nil then
      Caption := FBaseCaption+' - '+ActiveEditor.FileName
    else
      Caption := FBaseCaption;
  end;
end;

procedure TSourceNotebook.CheckCurrentCodeBufferChanged;
var
  SrcEdit: TSourceEditor;
begin
  // Todo: Move to manager, include window changes
  SrcEdit:=GetActiveSE;
  if SrcEdit <> nil then
  begin
    if FLastCodeBuffer=SrcEdit.CodeBuffer then exit;
    FLastCodeBuffer:=SrcEdit.CodeBuffer;
  end else if FLastCodeBuffer=nil then
    exit;
  if assigned(Manager) and Assigned(Manager.OnCurrentCodeBufferChanged) then
    Manager.OnCurrentCodeBufferChanged(Self);
end;

function TSourceNotebook.GetCapabilities: TCTabControlCapabilities;
begin
  Result := FNotebook.GetCapabilities
end;

procedure TSourceNotebook.IncUpdateLockInternal;
begin
  if FUpdateLock = 0 then begin
    FUpdateFlags := [];
    DebugLn(SRCED_LOCK, ['TSourceNotebook.IncUpdateLockInternal']);
    FPageIndex := PageIndex;
  end;
  inc(FUpdateLock);
end;

procedure TSourceNotebook.DecUpdateLockInternal;
begin
  dec(FUpdateLock);
  if FUpdateLock = 0 then begin
    DebugLnEnter(SRCED_LOCK, ['>> TSourceNotebook.DecUpdateLockInternal UpdateFlags=', dbgs(FUpdateFlags), ' PageIndex=', FPageIndex]);
    if (ufPageIndexChanged in FUpdateFlags) or (PageIndex<>FPageIndex) then ApplyPageIndex;
    if (ufPageNames in FUpdateFlags)    then UpdatePageNames;
    if (ufTabsAndPage in FUpdateFlags)  then UpdateTabsAndPageTitle;
    if (ufStatusBar in FUpdateFlags)    then UpdateStatusBar;
    if (ufProjectFiles in FUpdateFlags) then UpdateProjectFiles;
    if (ufFocusEditor in FUpdateFlags)  then FocusEditor;
    if (ufActiveEditorChanged in FUpdateFlags) then DoActiveEditorChanged;
    FUpdateFlags := [];
    DebugLnExit(SRCED_LOCK, ['<< TSourceNotebook.DecUpdateLockInternal']);
  end;
end;

procedure TSourceNotebook.IncUpdateLock;
begin
  Manager.IncUpdateLockInternal; // ensure mgr holds ActiveEditorChanged notificationback // TODO: make sure they are hlod in SourceNotebook instead, including SetAciveEditor....
  IncUpdateLockInternal;
end;

procedure TSourceNotebook.DecUpdateLock;
begin
  try
    DecUpdateLockInternal;
  finally
    Manager.DecUpdateLockInternal;
  end;
end;

procedure TSourceNotebook.NoteBookInsertPage(Index: Integer; const S: string);
begin
  if FNotebook.Visible then
    NotebookPages.Insert(Index, S)
  else begin
    if Index<>0 then
      RaiseGDBException('');
    IDEWindowCreators.ShowForm(Self,false);
    FNotebook.Visible := True;
    NotebookPages[Index] := S;
    FPageIndex := -1;
  end;
  UpdateTabsAndPageTitle;
end;

procedure TSourceNotebook.NoteBookDeletePage(APageIndex: Integer);
begin
  DebugLnEnter(SRCED_PAGES, ['TSourceNotebook.NoteBookDeletePage ', APageIndex]);
  HistoryRemove(FNotebook.Pages[APageIndex]);
  //debugln(['TSourceNotebook.NoteBookDeletePage APageIndex=',APageIndex,' PageIndex=',PageIndex,' PageCount=',PageCount]);
  if PageCount > 1 then begin
    // make sure to select another page in the NoteBook, otherwise the
    // widgetset will choose one and will send a message
    // if this is the current page, switch to right APageIndex (if possible)
    if PageIndex = APageIndex then begin
      if EditorOpts.UseTabHistory then
        FPageIndex := HistoryGetTopPageIndex
      else
        FPageIndex := -1;
      // default if not in history or not using history
      if FPageIndex = -1 then
        if APageIndex < PageCount - 1 then
          FPageIndex := APageIndex + 1
        else
          FPageIndex := APageIndex - 1;
      if FUpdateLock = 0 then
        ApplyPageIndex
      else
        Include(FUpdateFlags,ufPageIndexChanged);
    end;
    NotebookPages.Delete(APageIndex);
  end else begin
    FPageIndex := -1;
    FNotebook.Visible := False;
  end;
  //debugln(['TSourceNotebook.NoteBookDeletePage END PageIndex=',PageIndex,' FPageIndex=',FPageIndex]);
  UpdateTabsAndPageTitle;
  DebugLnExit(SRCED_PAGES, ['TSourceNotebook.NoteBookDeletePage ']);
end;

procedure TSourceNotebook.UpdateTabsAndPageTitle;
begin
  if FUpdateLock > 0 then begin
    include(FUpdateFlags, ufTabsAndPage);
    exit;
  end;
  if (PageCount = 1) and (EditorOpts.HideSingleTabInWindow) then begin
    if not EditorOpts.ShowFileNameInCaption then
      Caption := FBaseCaption + ': ' + NotebookPages[0];
    FNotebook.ShowTabs := False;
  end else begin
    if not EditorOpts.ShowFileNameInCaption then
      Caption := FBaseCaption;
    FNotebook.ShowTabs := (Manager=nil) or Manager.ShowTabs;
  end;
end;

procedure TSourceNotebook.UpdateTabsAndPageTimeReached(Sender: TObject);
begin
  FUpdateTabAndPageTimer.Enabled := False;
  UpdateTabsAndPageTitle;
end;

function TSourceNotebook.NoteBookIndexOfPage(APage: TTabSheet): Integer;
begin
  Result := FNoteBook.IndexOf(APage);
end;

procedure TSourceNotebook.DragOver(Source: TObject; X, Y: Integer; State: TDragState;
  var Accept: Boolean);
begin
  FUpdateTabAndPageTimer.Enabled := False;
  inherited DragOver(Source, X, Y, State, Accept);
  if State = dsDragLeave then
    FUpdateTabAndPageTimer.Enabled := True
  else if Source is TExtendedNotebook then
    FNotebook.ShowTabs := (Manager=nil) or Manager.ShowTabs;
end;

procedure TSourceNotebook.DragCanceled;
begin
  inherited DragCanceled;
  FUpdateTabAndPageTimer.Enabled := True;
end;

procedure TSourceNotebook.DoActiveEditorChanged;
begin
  if FUpdateLock > 0 then begin
    DebugLn(SRCED_PAGES, ['TSourceNotebook.DoActiveEditorChanged LOCKED']);
    include(FUpdateFlags, ufActiveEditorChanged);
    exit;
  end;
  exclude(FUpdateFlags, ufActiveEditorChanged);
  DebugLnEnter(SRCED_PAGES, ['>> TSourceNotebook.DoActiveEditorChanged ']);
  Manager.DoActiveEditorChanged;
  DebugLnExit(SRCED_PAGES, ['<< TSourceNotebook.DoActiveEditorChanged ']);
end;

procedure TSourceNotebook.BeginIncrementalFind;
var
  TempEditor: TSourceEditor;
begin
  if (snIncrementalFind in States)AND not(FIncrementalSearchEditor = nil)
  then begin
    if (IncrementalSearchStr=  '') then begin
      FIncrementalSearchStr := FIncrementalFoundStr;
      IncrementalSearch(False, FIncrementalSearchBackwards);
    end
    else IncrementalSearch(True, FIncrementalSearchBackwards);
    exit;
  end;

  TempEditor:=GetActiveSE;
  if TempEditor = nil then exit;
  Include(States, snIncrementalFind);
  fIncrementalSearchStartPos:=TempEditor.EditorComponent.LogicalCaretXY;
  FIncrementalSearchPos:=fIncrementalSearchStartPos;
  FIncrementalSearchEditor := TempEditor;
  if assigned(FIncrementalSearchEditor.EditorComponent) then
    with FIncrementalSearchEditor.EditorComponent do begin
      UseIncrementalColor:= true;
      if assigned(MarkupByClass[TSynEditMarkupHighlightAllCaret]) then
        MarkupByClass[TSynEditMarkupHighlightAllCaret].TempDisable;
    end;

  IncrementalSearchStr:='';

  UpdateStatusBar;
end;

procedure TSourceNotebook.EndIncrementalFind;
begin
  if not (snIncrementalFind in States) then exit;

  Exclude(States,snIncrementalFind);

  if FIncrementalSearchEditor <> nil
  then begin
    if assigned(FIncrementalSearchEditor.EditorComponent) then
      with FIncrementalSearchEditor.EditorComponent do begin
        UseIncrementalColor:= False;
        if assigned(MarkupByClass[TSynEditMarkupHighlightAllCaret]) then
          MarkupByClass[TSynEditMarkupHighlightAllCaret].TempEnable;
      end;
    FIncrementalSearchEditor.EditorComponent.SetHighlightSearch('', []);
    FIncrementalSearchEditor := nil;
  end;

  LazFindReplaceDialog.FindText:=fIncrementalSearchStr;
  LazFindReplaceDialog.Options:=[];
  UpdateStatusBar;
end;

procedure TSourceNotebook.NextEditor;
Begin
  if PageIndex < PageCount-1 then
    PageIndex := PageIndex+1
  else
    PageIndex := 0;
End;

procedure TSourceNotebook.PrevEditor;
Begin
  if PageIndex > 0 then
    PageIndex := PageIndex-1
  else
    PageIndex := PageCount-1;
End;

procedure TSourceNotebook.MoveEditor(OldPageIndex, NewPageIndex: integer);
begin
  if (PageCount<=1)
  or (OldPageIndex=NewPageIndex)
  or (OldPageIndex<0) or (OldPageIndex>=PageCount)
  or (NewPageIndex<0) or (NewPageIndex>=PageCount)
  then
    exit;
  NoteBookPages.Move(OldPageIndex,NewPageIndex);
  UpdatePageNames;
  UpdateProjectFiles;
end;

procedure TSourceNotebook.MoveEditorLeft(CurrentPageIndex: integer);
begin
  if (PageCount<=1) then exit;
  if CurrentPageIndex>0 then
    MoveEditor(CurrentPageIndex, CurrentPageIndex-1)
  else
    MoveEditor(CurrentPageIndex, PageCount-1);
end;

procedure TSourceNotebook.MoveEditorRight(CurrentPageIndex: integer);
begin
  if (PageCount<=1) then exit;
  if CurrentPageIndex < PageCount-1 then
    MoveEditor(CurrentPageIndex, CurrentPageIndex+1)
  else
    MoveEditor(CurrentPageIndex, 0);
end;

procedure TSourceNotebook.MoveEditorFirst(CurrentPageIndex: integer);
begin
  if (PageCount<=1) then exit;
  MoveEditor(CurrentPageIndex, 0)
end;

procedure TSourceNotebook.MoveEditorLast(CurrentPageIndex: integer);
begin
  if (PageCount<=1) then exit;
  MoveEditor(CurrentPageIndex, PageCount-1);
end;

procedure TSourceNotebook.MoveActivePageLeft;
begin
  MoveEditorLeft(PageIndex);
end;

procedure TSourceNotebook.MoveActivePageRight;
begin
  MoveEditorRight(PageIndex);
end;

procedure TSourceNotebook.MoveActivePageFirst;
begin
  MoveEditorFirst(PageIndex);
end;

procedure TSourceNotebook.MoveActivePageLast;
begin
  MoveEditorLast(PageIndex);
end;

procedure TSourceNotebook.GotoNextWindow(Backward: Boolean);
begin
  if Backward then begin
    if Manager.IndexOfSourceWindow(Self) > 0 then
      Manager.ActiveSourceWindow := Manager.SourceWindows[Manager.IndexOfSourceWindow(Self)-1]
    else
      Manager.ActiveSourceWindow := Manager.SourceWindows[Manager.SourceWindowCount-1];
  end else begin
    if Manager.IndexOfSourceWindow(Self) < Manager.SourceWindowCount - 1 then
      Manager.ActiveSourceWindow := Manager.SourceWindows[Manager.IndexOfSourceWindow(Self)+1]
    else
      Manager.ActiveSourceWindow := Manager.SourceWindows[0];
  end;
  Manager.ShowActiveWindowOnTop(True);
end;

procedure TSourceNotebook.GotoNextSharedEditor(Backward: Boolean = False);
var
  SrcEd: TSourceEditor;
  i, j: Integer;
begin
  i := Manager.IndexOfSourceWindow(Self);
  SrcEd := GetActiveSE;
  repeat
    if Backward then dec(i)
    else inc(i);
    if i < 0 then
      i := Manager.SourceWindowCount - 1;
    if i = Manager.SourceWindowCount then
      i := 0;
    j := Manager.SourceWindows[i].IndexOfEditorInShareWith(SrcEd);
    if j >= 0 then begin
      Manager.ActiveEditor := Manager.SourceWindows[i].Editors[j];
      Manager.ShowActiveWindowOnTop(True);
      exit;
    end;
  until Manager.SourceWindows[i] = Self;
end;

procedure TSourceNotebook.MoveEditorNextWindow(Backward: Boolean; Copy: Boolean);
var
  SrcEd: TSourceEditor;
  i: Integer;
begin
  i := Manager.IndexOfSourceWindow(Self);
  SrcEd := GetActiveSE;
  repeat
    if Backward then dec(i)
    else inc(i);
    if i < 0 then
      i := Manager.SourceWindowCount - 1;
    if i = Manager.SourceWindowCount then
      i := 0;
    if Manager.SourceWindows[i].IndexOfEditorInShareWith(SrcEd) < 0 then
      break;
  until Manager.SourceWindows[i] = Self;
  if Manager.SourceWindows[i] = Self then exit;

  if Copy then
    CopyEditor(FindPageWithEditor(GetActiveSE), i, -1)
  else
    MoveEditor(FindPageWithEditor(GetActiveSE), i, -1);

  Manager.ActiveSourceWindowIndex := i;
  Manager.ShowActiveWindowOnTop(True);
end;

procedure TSourceNotebook.MoveEditor(OldPageIndex, NewWindowIndex,
  NewPageIndex: integer);
var
  DestWin: TSourceNotebook;
  Edit: TSourceEditor;
begin
  if (NewWindowIndex < 0) or (NewWindowIndex >= Manager.SourceWindowCount) then
    exit;
  DestWin := Manager.SourceWindows[NewWindowIndex];
  if DestWin = self then begin
    MoveEditor(OldPageIndex, NewPageIndex);
    exit
  end;

  if NewPageIndex < 0 then
    NewPageIndex := DestWin.PageCount;
  if (OldPageIndex<0) or (OldPageIndex>=PageCount) or
     (NewPageIndex<0) or (NewPageIndex>DestWin.PageCount)
  then
    exit;

  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TSourceNotebook.MoveEditor'){$ENDIF};
  IncUpdateLock;
  try
    DestWin.DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TSourceNotebook.MoveEdito DestWinr'){$ENDIF};
    DestWin.IncUpdateLock;
    try
      Edit := FindSourceEditorWithPageIndex(OldPageIndex);
      DestWin.NoteBookInsertPage(NewPageIndex, Edit.PageName);
      DestWin.PageIndex := NewPageIndex;

      ReleaseEditor(Edit);
      Edit.UpdateNoteBook(DestWin, DestWin.NoteBookPage[NewPageIndex]);
      DestWin.AcceptEditor(Edit);
      DestWin.NotebookPage[NewPageIndex].ReAlign;

      NoteBookDeletePage(OldPageIndex);
      UpdatePageNames;
      UpdateProjectFiles;
      DestWin.UpdatePageNames;
      DestWin.UpdateProjectFiles(Edit);
      DestWin.UpdateActiveEditColors(Edit.EditorComponent);
      DestWin.UpdateStatusBar;
      DestWin.NotebookPageChanged(nil); // make sure page SynEdit willl be visible
    finally
      DestWin.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TSourceNotebook.MoveEdito DestWinr'){$ENDIF};
      DestWin.DecUpdateLock;
    end;
  finally
    EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TSourceNotebook.MoveEditor'){$ENDIF};
    DecUpdateLock
  end;

  if (PageCount = 0) and (Parent=nil) and not FIsClosing then
    Close;

  DoActiveEditorChanged;
  Manager.ActiveEditor := Edit;
end;

procedure TSourceNotebook.CopyEditor(OldPageIndex, NewWindowIndex,
  NewPageIndex: integer; Focus: Boolean = False);
var
  DestWin: TSourceNotebook;
  SrcEdit, NewEdit: TSourceEditor;
  i: Integer;
begin
  if (NewWindowIndex < 0) or (NewWindowIndex >= Manager.SourceWindowCount) then
    exit;
  DestWin := Manager.SourceWindows[NewWindowIndex];
  if DestWin = self then exit;

  if (OldPageIndex<0) or (OldPageIndex>=PageCount) or (NewPageIndex>DestWin.PageCount)
  then
    exit;

  SrcEdit := FindSourceEditorWithPageIndex(OldPageIndex);
  NewEdit := DestWin.NewSE(-1, NewPageIndex, SrcEdit, SrcEdit.PageName);
  Include(NewEdit.FProjectFileUpdatesNeeded, sepuNewShared);

  NewEdit.PageName := SrcEdit.PageName;
  NewEdit.SyntaxHighlighterType := SrcEdit.SyntaxHighlighterType;
  NewEdit.EditorComponent.TopLine := SrcEdit.EditorComponent.TopLine;
  NewEdit.EditorComponent.CaretXY := SrcEdit.EditorComponent.CaretXY;

  UpdatePageNames;
  UpdateProjectFiles;
  DestWin.UpdateProjectFiles(NewEdit);
  // Creating a shared edit invalidates the tree in SynMarkup. Force setting it for all editors
  for i := 0 to SrcEdit.SharedEditorCount - 1 do
    SrcEdit.SharedEditors[i].UpdateIfDefNodeStates(True);
  // Update IsVisibleTab; needs UnitEditorInfo created in DestWin.UpdateProjectFiles
  if Focus then begin
    Manager.ActiveEditor := NewEdit;
    Manager.ShowActiveWindowOnTop(True);
  end;
  DestWin.NotebookPageChanged(nil); // make sure page SynEdit willl be visible
  DoActiveEditorChanged;
end;

procedure TSourceNotebook.StartShowCodeContext(JumpToError: boolean);
var
  Abort: boolean;
begin
  if assigned(Manager) and (Manager.OnShowCodeContext<>nil) then begin
    Manager.OnShowCodeContext(JumpToError,Abort);
    if Abort then ;
  end;
end;

procedure TSourceNotebook.OnPopupMenuOpenFile(Sender: TObject);
var
  ResStr: String;
  p: SizeInt;
  aFilename: TTranslateString;
begin
  // open the filename of the caption
  // the caption was created by the resourcestring lisOpenLFM, with the
  // placeholder %s
  // => cut the surrounding caption to the get the filename
  aFilename:=(Sender as TIDEMenuItem).Caption;
  ResStr:=lisOpenLfm;
  p:=System.Pos('%s',ResStr);
  aFilename:=copy(aFilename,p,length(aFilename)-(length(ResStr)-2));
  if not FilenameIsAbsolute(aFilename) then
    aFilename:=TrimFilename(ExtractFilePath(GetActiveSE.Filename)+aFilename);
  if CompareFileExt(aFilename,'.lpi')=0 then
    MainIDEInterface.DoOpenProjectFile(aFilename,
      [ofOnlyIfExists,ofAddToRecent,ofUseCache])
  else if CompareFileExt(aFilename,'.lpk')=0 then
    PackageEditingInterface.DoOpenPackageFile(aFilename,[pofAddToRecent],false)
  else
    MainIDEInterface.DoOpenEditorFile(aFilename,
      PageIndex+1, Manager.IndexOfSourceWindow(self),
      [ofOnlyIfExists,ofAddToRecent,ofRegularFile,ofUseCache,ofDoNotLoadResource]);
end;

procedure TSourceNotebook.OnPopupOpenPackageFile(Sender: TObject);
begin
  if (Sender as TIDEMenuItem).UserTag <> 0 then begin
    PackageEditingInterface.DoOpenPackageFile
      (TIDEPackage((Sender as TIDEMenuItem).UserTag).Filename,[pofAddToRecent],false)
  end;
end;

procedure TSourceNotebook.OnPopupOpenProjectInsp(Sender: TObject);
begin
  MainIDEInterface.DoShowProjectInspector;
end;

procedure TSourceNotebook.OpenAtCursorClicked(Sender: TObject);
begin
  if Assigned(Manager) and Assigned(Manager.OnOpenFileAtCursorClicked) then
    Manager.OnOpenFileAtCursorClicked(Sender);
end;

procedure TSourceNotebook.CutClicked(Sender: TObject);
var ActSE: TSourceEditor;
begin
  ActSE := GetActiveSE;
  if ActSE <> nil then
    ActSE.DoEditorExecuteCommand(ecCut);
end;

procedure TSourceNotebook.CopyClicked(Sender: TObject);
var ActSE: TSourceEditor;
begin
  ActSE := GetActiveSE;
  if ActSE <> nil then
    ActSE.DoEditorExecuteCommand(ecCopy);
end;

procedure TSourceNotebook.PasteClicked(Sender: TObject);
var ActSE: TSourceEditor;
begin
  ActSE := GetActiveSE;
  if ActSE <> nil then
    ActSE.DoEditorExecuteCommand(ecPaste);
end;

procedure TSourceNotebook.StatusBarClick(Sender: TObject);
var
  P: TPoint;
begin
  P := StatusBar.ScreenToClient(Mouse.CursorPos);
  if StatusBar.GetPanelIndexAt(P.X, P.Y) = 1  then
    EditorMacroForRecording.Stop;
end;

procedure TSourceNotebook.StatusBarDblClick(Sender: TObject);
var
  P: TPoint;
begin
  P := StatusBar.ScreenToClient(Mouse.CursorPos);
  case StatusBar.GetPanelIndexAt(P.X, P.Y) of  // Based on panel:
    0: GoToLineMenuItemClick(Nil);    // Show "Goto Line" dialog.
    4: OpenFolderMenuItemClick(Nil);  // Show system file manager on file's folder.
  end;
end;

procedure TSourceNotebook.StatusBarContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var
  Pnl: Integer;
begin
  Pnl := StatusBar.GetPanelIndexAt(MousePos.X, MousePos.Y);
  GoToLineMenuItem.Visible := Pnl=0;
  OpenFolderMenuItem.Visible := Pnl=4;
  if Pnl in [0, 4] then
    StatusPopUpMenu.PopUp;
end;

procedure TSourceNotebook.StatusBarDrawPanel(AStatusBar: TStatusBar; APanel: TStatusPanel;
  const ARect: TRect);
begin
  if APanel = StatusBar.Panels[1] then
    IDEImages.Images_16.Draw(StatusBar.Canvas, ARect.Left,  ARect.Top, FStopBtnIdx);
end;

procedure TSourceNotebook.ToggleBreakpointClicked(Sender: TObject);
var
  ASrcEdit: TSourceEditor;
  Line: LongInt;
  BreakPtMark: TSourceMark;
begin
  ASrcEdit:=GetActiveSE;
  if ASrcEdit=nil then exit;
  // create or delete breakpoint
  // find breakpoint mark at line
  Line:=ASrcEdit.EditorComponent.CaretY;
  BreakPtMark := SourceEditorMarks.FindBreakPointMark(ASrcEdit, Line);
  if BreakPtMark = nil then
    DebugBoss.DoCreateBreakPoint(ASrcEdit.Filename,Line,true)
  else
    DebugBoss.DoDeleteBreakPointAtMark(BreakPtMark);
end;

procedure TSourceNotebook.ToggleBreakpointEnabledClicked(Sender: TObject);
var
  ASrcEdit: TSourceEditor;
  Line: LongInt;
  BreakPtMark: TSourceMark;
  BreakPoint: TIDEBreakPoint;
begin
  ASrcEdit:=GetActiveSE;
  if ASrcEdit=nil then exit;
  // create or delete breakpoint
  // find breakpoint mark at line
  Line:=ASrcEdit.EditorComponent.CaretY;
  BreakPtMark := SourceEditorMarks.FindBreakPointMark(ASrcEdit, Line);
  if BreakPtMark = nil then begin
    DebugBoss.DoCreateBreakPoint(ASrcEdit.Filename,Line,true, BreakPoint, True);
    if BreakPoint <> nil then begin
      BreakPoint.Enabled := False;
      BreakPoint.EndUpdate;
    end;
  end
  else
  begin
    BreakPoint := DebugBoss.BreakPoints.Find(ASrcEdit.FileName, Line);
    BreakPoint.Enabled := not BreakPoint.Enabled;
  end;
end;

procedure TSourceNotebook.CompleteCodeMenuItemClick(Sender: TObject);
begin
  MainIDEInterface.DoCommand(ecCompleteCode);
end;

procedure TSourceNotebook.DeleteBreakpointClicked(Sender: TObject);
var
  ASrcEdit: TSourceEditor;
begin
  ASrcEdit:=GetActiveSE;
  if ASrcEdit=nil then exit;
  DebugBoss.DoDeleteBreakPoint(ASrcEdit.Filename,
                               ASrcEdit.EditorComponent.CaretY);
end;

procedure TSourceNotebook.ExtractProcMenuItemClick(Sender: TObject);
begin
  MainIDEInterface.DoCommand(ecExtractProc);
end;

procedure TSourceNotebook.InvertAssignmentMenuItemClick(Sender: TObject);
var
  ASrcEdit: TSourceEditor;
begin
  ASrcEdit:=GetActiveSE;
  if ASrcEdit=nil then exit;
  ASrcEdit.InvertAssignment;
end;

procedure TSourceNotebook.RenameIdentifierMenuItemClick(Sender: TObject);
begin
  MainIDEInterface.DoCommand(ecRenameIdentifier);
end;

procedure TSourceNotebook.ShowAbstractMethodsMenuItemClick(Sender: TObject);
begin
  MainIDEInterface.DoCommand(ecShowAbstractMethods);
end;

procedure TSourceNotebook.ShowEmptyMethodsMenuItemClick(Sender: TObject);
begin
  MainIDEInterface.DoCommand(ecRemoveEmptyMethods);
end;

procedure TSourceNotebook.ShowUnusedUnitsMenuItemClick(Sender: TObject);
begin
  MainIDEInterface.DoCommand(ecRemoveUnusedUnits);
end;

procedure TSourceNotebook.SourceNotebookDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  FManager.ActiveSourceWindow := Self;
  LazarusIDE.DoDropFiles(Sender,Filenames,WindowID);
end;

procedure TSourceNotebook.FindOverloadsMenuItemClick(Sender: TObject);
begin
  MainIDEInterface.DoCommand(ecFindOverloads);
end;

procedure TSourceNotebook.MakeResourceStringMenuItemClick(Sender: TObject);
begin
  MainIDEInterface.DoCommand(ecMakeResourceString);
end;

function TSourceNotebook.NewFile(const NewShortName: String;
  ASource: TCodeBuffer; FocusIt: boolean; AShareEditor: TSourceEditor = nil): TSourceEditor;
var
  s: String;
Begin
  //create a new page
  debugln(SRCED_OPEN, '[TSourceNotebook.NewFile] A ');
  // Debugger cause ProcessMessages, which could lead to entering methods in unexpected order
  DebugBoss.LockCommandProcessing;
  try
    DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TSourceNotebook.NewFile'){$ENDIF};
    try
      IDEWindowCreators.ShowForm(Self,false);
      s := Manager.FindUniquePageName(NewShortName, AShareEditor);
      Result := NewSE(-1, -1, AShareEditor, s);
      debugln(SRCED_OPEN, '[TSourceNotebook.NewFile] B ');
      Result.CodeBuffer:=ASource;
      debugln(SRCED_OPEN, '[TSourceNotebook.NewFile] D ');
      //debugln(['TSourceNotebook.NewFile ',NewShortName,' ',ASource.Filename]);
      Result.PageName:= s;
      UpdatePageNames;
      UpdateProjectFiles(Result);
      UpdateStatusBar;
      Manager.SendEditorCreated(Result);
    finally
      EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TSourceNotebook.NewFile'){$ENDIF};
    end;
    if FocusIt then FocusEditor;
  finally
    DebugBoss.UnLockCommandProcessing;
  end;
  debugln(SRCED_OPEN, '[TSourceNotebook.NewFile] end');
  CheckFont;
end;

procedure TSourceNotebook.CloseFile(APageIndex:integer);
var
  TempEditor: TSourceEditor;
  WasSelected: Boolean;
begin
  (* Do not use DisableAutoSizing in here, if a new Editor is focused it needs immediate autosize (during handle creation) *)
  // Inc/DecUpdateLockInternal does currently noth work, since a tab will be removed
  DebugLnEnter(SRCED_CLOSE, ['>> TSourceNotebook.CloseFile A  APageIndex=',APageIndex, ' Cur-Page=', PageIndex]);
  DebugBoss.LockCommandProcessing;
  try
    TempEditor:=FindSourceEditorWithPageIndex(APageIndex);
    if TempEditor=nil then exit;
    WasSelected:=PageIndex=APageIndex;
    debugln(SRCED_CLOSE, ['TSourceNotebook.CloseFile ', DbgSName(TempEditor), ' ', TempEditor.FileName]);
    EndIncrementalFind;
    TempEditor.Close;
    NoteBookDeletePage(APageIndex); // delete page before sending notification senEditorDestroyed
    TempEditor.Free;    // sends semEditorDestroy
    TempEditor:=nil;
    // delete the page
    UpdateProjectFiles;
    UpdatePageNames;
    if WasSelected then
      UpdateStatusBar;
    // set focus to new editor
    if (PageCount = 0) and (Parent=nil) then begin
      {$IFnDEF SingleSrcWindow}
      Manager.RemoveWindow(self);
      FManager := nil;
      {$ENDIF}
      if not FIsClosing then
        Close;
    end;
    // Move focus from Notebook-tabs to editor
    TempEditor:=FindSourceEditorWithPageIndex(PageIndex);
    if IsVisible and (TempEditor <> nil) and (FUpdateLock = 0) then
      // this line raises exception when editor is in other tab (for example - focused is designer)
      ;// TempEditor.EditorComponent.SetFocus;
  finally
    debugln(SRCED_CLOSE, ['TSourceNotebook.CloseFile UnLock']);
    DebugBoss.UnLockCommandProcessing;
    DebugLnExit(SRCED_CLOSE, ['<< TSourceNotebook.CloseFile']);
  end;
end;

procedure TSourceNotebook.FocusEditor;
var
  SrcEdit: TSourceEditor;
begin
  if FUpdateLock > 0 then begin
    include(FUpdateFlags, ufFocusEditor);
    exit;
  end;
  if (fAutoFocusLock>0) then exit;
  SrcEdit:=GetActiveSE;
  if SrcEdit=nil then exit;
  SrcEdit.FocusEditor;
end;

procedure TSourceNotebook.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Cursor:=crDefault;
end;

// Two Popup menu handlers:
procedure TSourceNotebook.GoToLineMenuItemClick(Sender: TObject);
begin
  if Assigned(Manager) then
    Manager.GotoLineClicked(nil);
end;

procedure TSourceNotebook.OpenFolderMenuItemClick(Sender: TObject);
begin
  OpenDocument(ExtractFilePath(Statusbar.Panels[4].Text));
end;

procedure TSourceNotebook.ExecuteEditorItemClick(Sender: TObject);
var
  Editor: TSourceEditor;
begin
  if SourceEditorManager = nil then exit;

  Editor := TSourceEditor((sender as TIDEMenuCommand).UserTag);
  SourceEditorManager.ActiveEditor :=Editor;
  SourceEditorManager.ShowActiveWindowOnTop(True);
end;

procedure TSourceNotebook.ApplyPageIndex;
begin
  Exclude(FUpdateFlags,ufPageIndexChanged);
  DebugBoss.LockCommandProcessing;
  try
    //debugln(['TSourceNotebook.ApplyPageIndex FPageIndex=',FPageIndex]);
    FPageIndex := Max(0,Min(FPageIndex,FNotebook.PageCount-1));
    if Assigned(Manager) and (FNotebook.PageIndex = FPageIndex) then
      DoActiveEditorChanged;
    // make sure the statusbar is updated
    Include(States, snNotebookPageChangedNeeded);
    FNotebook.PageIndex := FPageIndex;
    if snNotebookPageChangedNeeded in States then begin
      DebugLn(SRCED_PAGES, ['TSourceNotebook.ApplyPageIndex calling NotebookPageChanged']);
      NotebookPageChanged(nil);
    end;
    HistorySetMostRecent(FNotebook.Pages[FPageIndex]);
  finally
    DebugBoss.UnLockCommandProcessing;
  end;
end;

procedure TSourceNotebook.CloseTabClicked(Sender: TObject);
begin
  if (GetKeyShiftState * [ssShift, ssCtrl, ssAlt] = EditorOpts.MiddleTabClickClosesOthersModifier) and
     (EditorOpts.MiddleTabClickClosesOthersModifier <> [])
  then
    CloseClicked(Sender, [ceoCloseOthers])
  else
  if (GetKeyShiftState * [ssShift, ssCtrl, ssAlt] = EditorOpts.MiddleTabClickClosesToRightModifier) and
     (EditorOpts.MiddleTabClickClosesToRightModifier <> [])
  then
    CloseClicked(Sender, [ceoCloseOthersOnRightSide])
  else
    CloseClicked(Sender, []);
end;

function TSourceNotebook.GetEditors(Index:integer):TSourceEditor;
begin
  Result:=TSourceEditor(FSourceEditorList[Index]);
end;

function TSourceNotebook.EditorCount:integer;
begin
  Result:=FSourceEditorList.Count;
end;

function TSourceNotebook.IndexOfEditor(aEditor: TSourceEditorInterface): integer;
begin
  Result := FSourceEditorList.IndexOf(aEditor);
end;

function TSourceNotebook.Count: integer;
begin
  Result:=FSourceEditorList.Count;
end;

function TSourceNotebook.SourceEditorIntfWithFilename(const Filename: string
  ): TSourceEditorInterface;
var
  Node: TAvlTreeNode;
begin
  Node:=FSrcEditsSortedForFilenames.FindKey(Pointer(Filename),@CompareFilenameWithSrcEditIntf);
  if Node<>nil then
    Result:=TSourceEditorInterface(Node.Data)
  else
    Result:=nil;
end;

procedure TSourceNotebook.CloseClicked(Sender: TObject;
  CloseOptions: TCloseSrcEditorOptions);
Begin
  if assigned(Manager) and Assigned(Manager.OnCloseClicked) then
    Manager.OnCloseClicked(Sender, CloseOptions);
end;

procedure TSourceNotebook.ToggleFormUnitClicked(Sender: TObject);
begin
  if assigned(Manager) and Assigned(Manager.OnToggleFormUnitClicked) then
    Manager.OnToggleFormUnitClicked(Sender);
end;

procedure TSourceNotebook.ToggleObjectInspClicked(Sender: TObject);
begin
  if assigned(Manager) and Assigned(Manager.OnToggleObjectInspClicked) then
    Manager.OnToggleObjectInspClicked(Sender);
end;

procedure TSourceNotebook.HistorySetMostRecent(APage: TTabSheet);
var
  Index: Integer;
begin
   if APage = nil then
     Exit;
   Index := FHistoryList.IndexOf(APage);
   if Index <> -1 then
     FHistoryList.Delete(Index);
   FHistoryList.Insert(0, APage);
end;

procedure TSourceNotebook.HistoryRemove(APage: TTabSheet);
var
  Index: Integer;
begin
  Index := FHistoryList.IndexOf(APage);
   if Index <> -1 then
     FHistoryList.Delete(Index);
end;

function TSourceNotebook.HistoryGetTopPageIndex: Integer;
begin
  Result := -1;
  if FHistoryList.Count = 0 then
    Exit;
  Result := FNotebook.IndexOf(TCustomPage(FHistoryList.Items[0]));
end;

procedure TSourceNotebook.InsertCharacter(const C: TUTF8Char);
var
  FActiveEdit: TSourceEditor;
begin
  FActiveEdit := GetActiveSE;
  if FActiveEdit <> nil then
  begin
    if FActiveEdit.ReadOnly then Exit;
    FActiveEdit.EditorComponent.InsertTextAtCaret(C);
  end;
end;

procedure TSourceNotebook.SrcEditMenuCopyToExistingWindowClicked(Sender: TObject);
begin
  inc(FFocusLock);
  try
    CopyEditor(PageIndex, (Sender as TIDEMenuItem).Tag, -1);
  finally
    dec(FFocusLock);
  end;
end;

procedure TSourceNotebook.SrcEditMenuMoveToExistingWindowClicked(Sender: TObject);
begin
  MoveEditor(PageIndex, (Sender as TIDEMenuItem).Tag, -1)
end;

procedure TSourceNotebook.SrcEditMenuFindInWindowClicked(Sender: TObject);
var
  TargetIndex: Integer;
  DestWin: TSourceNotebook;
  Edit: TSourceEditor;
  SharedEditorIdx: Integer;
begin
  TargetIndex := (Sender as TIDEMenuItem).Tag;
  if (TargetIndex < 0) or (TargetIndex >= Manager.SourceWindowCount) then
    exit;
  DestWin := Manager.SourceWindows[TargetIndex];
  Edit := FindSourceEditorWithPageIndex(PageIndex);
  SharedEditorIdx := DestWin.IndexOfEditorInShareWith(Edit);
  If SharedEditorIdx < 0 then
    exit;
  Manager.ActiveEditor := DestWin.Editors[SharedEditorIdx];
  Manager.ShowActiveWindowOnTop(True);
end;

procedure TSourceNotebook.EditorLockClicked(Sender: TObject);
begin
  GetActiveSE.IsLocked := not GetActiveSE.IsLocked;
end;

procedure TSourceNotebook.UpdateStatusBar;
var
  tempEditor: TSourceEditor;
  PanelFilename: String;
  PanelCharMode: string;
  PanelXY: string;
  PanelFileMode: string;
  CurEditor: TSynEdit;
begin
  if FUpdateLock > 0 then begin
    include(FUpdateFlags, ufStatusBar);
    exit;
  end;
  if (not IsVisible) or (FUpdateLock > 0) then
  begin
    Include(States,snUpdateStatusBarNeeded);
    exit;
  end;
  Exclude(States,snUpdateStatusBarNeeded);
  TempEditor := GetActiveSE;
  if TempEditor = nil then Exit;
  CurEditor:=TempEditor.EditorComponent;
  //debugln(['TSourceNotebook.UpdateStatusBar ',tempEditor.FileName,' ',PageIndex]);

  if (snIncrementalFind in States)
  and (CompareCaret(CurEditor.LogicalCaretXY,FIncrementalSearchPos)<>0) then
  begin
    // some action has changed the cursor during incremental search
    // -> end incremental search
    EndIncrementalFind;
    // this called UpdateStatusBar -> exit
    exit;
  end;

  if (CurEditor.CaretY<>TempEditor.ErrorLine)
  or (CurEditor.CaretX<>TempEditor.fErrorColumn) then
    TempEditor.ErrorLine:=-1;
  Statusbar.BeginUpdate;

  if snIncrementalFind in States then begin
    Statusbar.SimplePanel:=true;
    Statusbar.SimpleText:=Format(lisUESearching, [IncrementalSearchStr]);

  end else begin
    Statusbar.SimplePanel:=false;
    PanelFilename:=TempEditor.Filename;

    If TempEditor.Modified then
      PanelFileMode := ueModified
    else
      PanelFileMode := '';

    If TempEditor.ReadOnly then begin
      if PanelFileMode <> '' then
        PanelFileMode := PanelFileMode + lisUEModeSeparator;
      PanelFileMode := PanelFileMode + uepReadonly;
    end;

    if (EditorMacroForRecording.State = emRecording) and
       (EditorMacroForRecording.IsRecording(CurEditor))
    then begin
      if PanelFileMode <> '' then
        PanelFileMode := PanelFileMode + lisUEModeSeparator;
      PanelFileMode := PanelFileMode + ueMacroRecording;
    end;
    if (EditorMacroForRecording.State = emRecPaused) and
       (EditorMacroForRecording.IsRecording(CurEditor))
    then begin
      if PanelFileMode <> '' then
        PanelFileMode := PanelFileMode + lisUEModeSeparator;
      PanelFileMode := PanelFileMode + ueMacroRecordingPaused;
    end;

    If TempEditor.IsLocked then begin
      if PanelFileMode <> '' then
        PanelFileMode := PanelFileMode + lisUEModeSeparator;
      PanelFileMode := PanelFileMode + ueLocked;
    end;

    PanelXY := Format(' %6d:%4d',
                 [TempEditor.CurrentCursorYLine,TempEditor.CurrentCursorXLine]);

    if GetActiveSE.InsertMode then
      PanelCharMode := uepIns
    else
      PanelCharMode := uepOvr;

    Statusbar.Panels[0].Text := PanelXY;
    StatusBar.Panels[2].Text := PanelFileMode;
    Statusbar.Panels[3].Text := PanelCharMode;
    Statusbar.Panels[4].Text := PanelFilename;
    if(EditorMacroForRecording.IsRecording(CurEditor)) then
      Statusbar.Panels[1].Width := 20
    else
      Statusbar.Panels[1].Width := 0;

  end;
  Statusbar.EndUpdate;

  CheckCurrentCodeBufferChanged;
End;

function TSourceNotebook.FindPageWithEditor(ASourceEditor: TSourceEditor): integer;
var
  LParent: TWinControl;
  LTabSheet: TWinControl;
begin
  if (ASourceEditor.EditorComponent.Parent is TTabSheet) then
  begin
    LParent := ASourceEditor.EditorComponent.Parent.Parent;
    LTabSheet := ASourceEditor.EditorComponent.Parent;
    while (LParent <> FNotebook) and (LParent <> nil) do
    begin
      LTabSheet := LParent;
      LParent := LParent.Parent;
    end;

    if (LParent <> nil) and (LTabSheet is TTabSheet) then
      Result:=TTabSheet(LTabSheet).PageIndex
    else
      Result:=-1;
  end
  else    
    Result:=-1;
end;

function TSourceNotebook.FindSourceEditorWithEditorComponent(EditorComp: TComponent): TSourceEditor;
var
  i: integer;
begin
  for i:=0 to EditorCount-1 do begin
    Result:=Editors[i];
    if Result.EditorComponent=EditorComp then exit;
  end;
  Result:=nil;
end;

procedure TSourceNotebook.NotebookMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  TabIndex: Integer;
begin
  if (Button = mbMiddle) then
  begin
    TabIndex:=FNotebook.IndexOfPageAt(X, Y);
    if TabIndex>=0 then begin
      if (GetKeyShiftState * [ssShift, ssCtrl, ssAlt] = EditorOpts.MiddleTabClickClosesOthersModifier) and
         (EditorOpts.MiddleTabClickClosesOthersModifier <> [])
      then
        CloseClicked(NoteBookPage[TabIndex], [ceoCloseOthers])
      else
      if (GetKeyShiftState * [ssShift, ssCtrl, ssAlt] = EditorOpts.MiddleTabClickClosesToRightModifier) and
         (EditorOpts.MiddleTabClickClosesToRightModifier <> [])
      then
        CloseClicked(NoteBookPage[TabIndex], [ceoCloseOthersOnRightSide])
      else
        CloseClicked(NoteBookPage[TabIndex], []);
    end;
  end else
  if (Button = mbRight) then
  begin
    //select on right click
    TabIndex:=FNotebook.IndexOfPageAt(X, Y);
    if TabIndex>=0 then
      FNotebook.ActivePageIndex := TabIndex;
  end;
end;

procedure TSourceNotebook.NotebookMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TabIndex: Integer;
begin
  if (Button = mbRight) then
  begin
    TabIndex:=FNotebook.IndexOfPageAt(X, Y);
    if TabIndex>=0 then
    begin
      TabPopUpMenu.PopupComponent := FNotebook;
      TabPopUpMenu.PopUp;
      TabPopUpMenu.PopupComponent := nil;
    end;
  end;
end;

procedure TSourceNotebook.NotebookDragDropEx(Sender, Source: TObject; OldIndex,
  NewIndex: Integer; CopyDrag: Boolean; var Done: Boolean);
  function SourceIndex: Integer;
  begin
    Result := Manager.SourceWindowCount - 1;
    while Result >= 0 do begin
      if Manager.SourceWindows[Result].FNotebook = Source then break;
      dec(Result);
    end;
  end;
begin
  {$IFnDEF SingleSrcWindow}
  If CopyDrag then begin
    Manager.SourceWindows[SourceIndex].CopyEditor
      (OldIndex, Manager.IndexOfSourceWindow(self), NewIndex);
  end
  else begin
  {$ENDIF}
    if (Source = FNotebook) then
      MoveEditor(OldIndex, NewIndex)
    else begin
      Manager.SourceWindows[SourceIndex].MoveEditor
        (OldIndex, Manager.IndexOfSourceWindow(self), NewIndex);
    end;
  {$IFnDEF SingleSrcWindow}
  end;
  {$ENDIF}
  Manager.ActiveSourceWindow := self;
  Manager.ShowActiveWindowOnTop(True);
  Done := True;
end;

procedure TSourceNotebook.NotebookDragOverEx(Sender, Source: TObject;
  OldIndex, NewIndex: Integer; CopyDrag: Boolean; var Accept: Boolean);

  function SourceIndex: Integer;
  begin
    Result := Manager.SourceWindowCount - 1;
    while Result >= 0 do begin
      if Manager.SourceWindows[Result].FNotebook = Source then break;
      dec(Result);
    end;
  end;
var
  Src: TSourceNotebook;
  NBHasSharedEditor: Boolean;
begin
  Src := Manager.SourceWindows[SourceIndex];
  NBHasSharedEditor := IndexOfEditorInShareWith
    (Src.FindSourceEditorWithPageIndex(OldIndex)) >= 0;
  {$IFnDEF SingleSrcWindow}
  if CopyDrag then
    Accept := (NewIndex >= 0) and (Source <> Sender) and (not NBHasSharedEditor)
  else
  {$ENDIF}
    Accept := (NewIndex >= 0) and
              ((Source <> Sender) or (OldIndex <> NewIndex)) and
              ((Source = Sender) or (not NBHasSharedEditor));
end;

procedure TSourceNotebook.NotebookDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Accept=true then ; // set by NotebookDragOverEx
  FUpdateTabAndPageTimer.Enabled := False;
  if State = dsDragLeave then
    FUpdateTabAndPageTimer.Enabled := True
  else if Source is TExtendedNotebook then
    FNotebook.ShowTabs := (Manager=nil) or Manager.ShowTabs;
end;

procedure TSourceNotebook.NotebookEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  FUpdateTabAndPageTimer.Enabled := True;
end;

procedure TSourceNotebook.NotebookPageChanged(Sender: TObject);
var
  SrcEdit:TSourceEditor;
  CaretXY: TPoint;
  TopLine: Integer;
Begin
  if (not assigned(Manager)) or (FUpdateLock > 0) Then begin
    Include(States, snNotebookPageChangedNeeded);
    exit;
  end;
  DebugLnEnter(SRCED_PAGES, ['>> TSourceNotebook.NotebookPageChanged PageIndex=', PageIndex, ' AutoFocusLock=', fAutoFocusLock, ' Sender=',DbgSName(Sender)]);

  DebugBoss.LockCommandProcessing;
  try
    Exclude(States, snNotebookPageChangedNeeded);
    SrcEdit:=GetActiveSE;
    Manager.FHints.HideIfVisible;
    if (CodeContextFrm<>nil) then
      CodeContextFrm.Hide;

    DebugLn(SRCED_PAGES, ['TSourceNotebook.NotebookPageChanged TempEdit=', DbgSName(SrcEdit), ' Vis=', dbgs(IsVisible), ' Hnd=', dbgs(HandleAllocated)]);
    if SrcEdit <> nil then
    begin
      if not SrcEdit.Visible then begin
        // As long as SynEdit had no Handle, it had kept all those Values untouched
        CaretXY := SrcEdit.EditorComponent.CaretXY;
        TopLine := SrcEdit.EditorComponent.TopLine;
        TSynEditMarkupManager(SrcEdit.EditorComponent.MarkupMgr).IncPaintLock;
        SrcEdit.BeginUpdate;
        SrcEdit.FEditor.HandleNeeded; // make sure we have a handle
        SrcEdit.Visible := True;
        SrcEdit.EndUpdate;
        // Restore the intial Positions, must be after lock
        SrcEdit.EditorComponent.LeftChar := 1;
        SrcEdit.EditorComponent.CaretXY := CaretXY;
        SrcEdit.EditorComponent.TopLine := TopLine;
        TSynEditMarkupManager(SrcEdit.EditorComponent.MarkupMgr).DecPaintLock;
        SrcEdit.UpdateIfDefNodeStates; // after editor is initialized
      end;
      if (fAutoFocusLock=0) and (Screen.ActiveCustomForm=GetParentForm(Self)) and
         not(Manager.HasAutoFocusLock)
      then
      begin
        DebugLnEnter(SRCED_PAGES, ['TSourceNotebook.NotebookPageChanged BEFORE SetFocus ', DbgSName(SrcEdit.EditorComponent),' Page=',   FindPageWithEditor(SrcEdit), ' ', SrcEdit.FileName]);
        SrcEdit.FocusEditor; // recursively calls NotebookPageChanged, via EditorEnter
        DebugLnExit(SRCED_PAGES, ['TSourceNotebook.NotebookPageChanged AFTER SetFocus ', DbgSName(SrcEdit.EditorComponent),' Page=',   FindPageWithEditor(SrcEdit)]);
      end;
      UpdateStatusBar;
      UpdateActiveEditColors(SrcEdit.EditorComponent);
      if (DebugBoss.State in [dsPause, dsRun]) and
         not SrcEdit.HasExecutionMarks and
         (SrcEdit.FileName <> '') then
        SrcEdit.FillExecutionMarks;
      DoActiveEditorChanged;
    end;

    CheckCurrentCodeBufferChanged;
  finally
    DebugBoss.UnLockCommandProcessing;
  end;
  DebugLnExit(SRCED_PAGES, ['<< TSourceNotebook.NotebookPageChanged ']);
end;

procedure TSourceNotebook.ProcessParentCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  var Handled: boolean);
var
  IDECmd: TIDECommand;
  r: Boolean;
begin
  //DebugLn(['TSourceNotebook.ProcessParentCommand START ',dbgsName(Sender),' Command=',Command,' AChar=',AChar]);

  FProcessingCommand:=true;
  if Assigned(Manager.OnProcessUserCommand) then begin
    Handled:=false;
    IDECmd:=IDECommandList.FindIDECommand(Command);
    r := (IDECmd <> nil) and (IDECmd.OnExecuteProc = @ExecuteIdeMenuClick);
    if r then IDECmd.OnExecuteProc := nil;

    Manager.OnProcessUserCommand(Self,Command,Handled);

    if r then IDECmd.OnExecuteProc := @ExecuteIdeMenuClick;

    if Handled or (Command=ecNone) then begin
      FProcessingCommand:=false;
      Command:=ecNone;
      exit;
    end;
  end;
  //DebugLn(['TSourceNotebook.ProcessParentCommand after mainide: ',dbgsName(Sender),' Command=',Command,' AChar=',AChar]);

  Handled:=true;
  case Command of
  ecNextEditor:           NextEditor;
  ecPrevEditor:           PrevEditor;
  ecPrevEditorInHistory:  FHistoryDlg.Show(True);
  ecNextEditorInHistory:  FHistoryDlg.Show(False);
  ecMoveEditorLeft:       MoveActivePageLeft;
  ecMoveEditorRight:      MoveActivePageRight;
  ecMoveEditorLeftmost:   MoveActivePageFirst;
  ecMoveEditorRightmost:  MoveActivePageLast;
  ecNextSharedEditor:     GotoNextSharedEditor(False);
  ecPrevSharedEditor:     GotoNextSharedEditor(True);
  ecNextWindow:           GotoNextWindow(False);
  ecPrevWindow:           GotoNextWindow(True);
  ecMoveEditorNextWindow: MoveEditorNextWindow(False, False);
  ecMoveEditorPrevWindow: MoveEditorNextWindow(True, False);
  ecMoveEditorNewWindow:
    if EditorCount > 1 then
      MoveEditor(FindPageWithEditor(GetActiveSE),
                 Manager.IndexOfSourceWindow(Manager.CreateNewWindow(True)), -1);

  ecCopyEditorNextWindow: MoveEditorNextWindow(False, True);
  ecCopyEditorPrevWindow: MoveEditorNextWindow(True, True);
  ecCopyEditorNewWindow:
    CopyEditor(FindPageWithEditor(GetActiveSE),
               Manager.IndexOfSourceWindow(Manager.CreateNewWindow(True)), -1, True);

  ecOpenFileAtCursor:     OpenAtCursorClicked(self);
  ecGotoEditor1..ecGotoEditor9,ecGotoEditor0:
    if PageCount>Command-ecGotoEditor1 then
      PageIndex := Command-ecGotoEditor1;

  ecToggleFormUnit:       ToggleFormUnitClicked(Self);
  ecToggleObjectInsp:     ToggleObjectInspClicked(Self);
  ecSetFreeBookmark:
    if Assigned(Manager.OnSetBookmark) then
      Manager.OnSetBookmark(GetActiveSE, -1, False);

  ecClearAllBookmark:
    if Assigned(Manager.OnClearBookmarkId) then
      Manager.OnClearBookmarkId(Self, -1);

  ecJumpBack:             Manager.HistoryJump(Self,jhaBack);
  ecJumpForward:          Manager.HistoryJump(Self,jhaForward);
  ecAddJumpPoint:         Manager.AddJumpPointClicked(Self);
  ecViewJumpHistory:      Manager.ViewJumpHistoryClicked(Self);

  else
    Handled:=ExecuteIDECommand(Self,Command);
    DebugLn('TSourceNotebook.ProcessParentCommand Command=',dbgs(Command),' Handled=',dbgs(Handled));
  end;  //case
  if Handled then Command:=ecNone;
  FProcessingCommand:=false;
end;

procedure TSourceNotebook.ParentCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  var Handled: boolean);
begin
  Assert(Assigned(Manager), 'TSourceNotebook.ParentCommandProcessed: Manager=Nil.');
  if Assigned(Manager.OnUserCommandProcessed) then begin
    Handled:=false;
    Manager.OnUserCommandProcessed(Self,Command,Handled);
    if Handled then exit;
  end;

  Handled:=(Command=ecClose);

  if Handled then Command:=ecNone;
end;

procedure TSourceNotebook.ReloadEditorOptions;
var
  I: integer;
Begin
  for i := 0 to EditorCount-1 do
    Editors[i].RefreshEditorSettings;

  if EditorOpts.ShowTabCloseButtons then
    FNoteBook.Options:=FNoteBook.Options+[nboShowCloseButtons]
  else
    FNoteBook.Options:=FNoteBook.Options-[nboShowCloseButtons];
  FNoteBook.MultiLine := EditorOpts.MultiLineTab;
  if FNotebook.ShowTabs then
    FNotebook.TabPosition := EditorOpts.TabPosition
  else
    FNotebook.TabPosition := tpTop;

  Exclude(States,snWarnedFont);
  CheckFont;
  UpdatePageNames;
end;

procedure TSourceNotebook.CheckFont;
var
  SrcEdit: TSourceEditor;
  DummyResult: TModalResult;
  CurFont: TFont;
begin
  if (snWarnedFont in States) then exit;
  Include(States,snWarnedFont);
  SrcEdit:=GetActiveSE;
  if SrcEdit = nil then
    Exit;
  CurFont:=SrcEdit.EditorComponent.Font;
  if SystemCharSetIsUTF8
  and ((EditorOpts.DoNotWarnForFont='')
       or (EditorOpts.DoNotWarnForFont<>CurFont.Name))
  then begin
    {$IFDEF HasMonoSpaceFonts}
    DummyResult:=IDEQuestionDialog(lisUEFontWith,
      Format(lisUETheCurre, [LineEnding, LineEnding]),
      mtWarning, [mrIgnore, mrYesToAll, lisUEDoNotSho]);
    {$ELSE}
    DummyResult:=mrYesToAll;
    {$ENDIF}
    if DummyResult=mrYesToAll then begin
      if EditorOpts.DoNotWarnForFont<>CurFont.Name then begin
        EditorOpts.DoNotWarnForFont:=CurFont.Name;
        EditorOpts.Save;
      end;
    end;
  end;
end;

procedure TSourceNotebook.BeginAutoFocusLock;
begin
  inc(fAutoFocusLock);
end;

procedure TSourceNotebook.EndAutoFocusLock;
begin
  dec(fAutoFocusLock);
end;

procedure TSourceNotebook.EditorMouseMove(Sender: TObject; Shift: TShiftstate;
  X, Y: Integer);
begin
  Manager.FHints.HideAutoHintAfterMouseMoved;
  if Visible then
    Manager.FHints.UpdateHintTimer;
end;

procedure TSourceNotebook.EditorMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  //handled:=true; //The scrolling is not done: it's not handled! See TWinControl.DoMouseWheel
end;

procedure TSourceNotebook.EditorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftstate; X, Y: Integer);
begin

end;

function TSourceNotebook.EditorGetIndent(Sender: TObject; Editor: TObject;
  LogCaret, OldLogCaret: TPoint; FirstLinePos, LastLinePos: Integer;
  Reason: TSynEditorCommand; SetIndentProc: TSynBeautifierSetIndentProc
  ): Boolean;
var
  SrcEdit: TSourceEditor;
  p: LongInt;
  NestedComments: Boolean;
  NewIndent: TFABIndentationPolicy;
  Indent: LongInt;
  CodeBuf: TCodeBuffer;
begin
  Result:=false;
  // SynBeautifier is shared arrcoss SynEdits, and may call the wrong SrcNoteBook
  if assigned(Manager)
  then SrcEdit := Manager.FindSourceEditorWithEditorComponent(TComponent(Editor))
  else SrcEdit := FindSourceEditorWithEditorComponent(TComponent(Editor));
  if SrcEdit = nil then
    exit;
  if assigned(Manager) and Assigned(Manager.OnGetIndent) then begin
    Result := Manager.OnGetIndent(Sender, SrcEdit, LogCaret, OldLogCaret, FirstLinePos, LastLinePos,
                          Reason, SetIndentProc);
    if Result then exit;
  end;
  if (SrcEdit.SyncroLockCount > 0) then exit;
  if not (SrcEdit.SyntaxHighlighterType in [lshFreePascal, lshDelphi]) then
    exit;
  if Reason<>ecLineBreak then exit;
  if not CodeToolsOpts.IndentOnLineBreak then exit;
  {$IFDEF VerboseIndenter}
  debugln(['TSourceNotebook.EditorGetIndent LogCaret=',dbgs(LogCaret),' FirstLinePos=',FirstLinePos,' LastLinePos=',LastLinePos]);
  {$ENDIF}
  Result := True;
  SrcEdit.UpdateCodeBuffer;
  CodeBuf:=SrcEdit.CodeBuffer;
  CodeBuf.LineColToPosition(LogCaret.Y,LogCaret.X,p);
  if p<1 then exit;
  {$IFDEF VerboseIndenter}
  if FirstLinePos>0 then
    DebugLn(['TSourceNotebook.EditorGetIndent Firstline-1=',SrcEdit.Lines[FirstLinePos-2]]);
  DebugLn(['TSourceNotebook.EditorGetIndent Firstline+0=',SrcEdit.Lines[FirstLinePos-1]]);
  if FirstLinePos<SrcEdit.LineCount then
    DebugLn(['TSourceNotebook.EditorGetIndent Firstline+1=',SrcEdit.Lines[FirstLinePos+0]]);
  DebugLn(['TSourceNotebook.EditorGetIndent CodeBuffer: ',dbgstr(copy(CodeBuf.Source,p-10,10)),'|',dbgstr(copy(CodeBuf.Source,p,10))]);
  DebugLn(['TSourceNotebook.EditorGetIndent CodeBuffer: "',copy(CodeBuf.Source,p-10,10),'|',copy(CodeBuf.Source,p,10)]);
  {$ENDIF}
  NestedComments:=CodeToolBoss.GetNestedCommentsFlagForFile(CodeBuf.Filename);
  if not CodeToolBoss.Indenter.GetIndent(CodeBuf.Source,p,NestedComments,
    True,NewIndent,CodeToolsOpts.IndentContextSensitive)
  then exit;
  if not NewIndent.IndentValid then exit;
  Indent:=NewIndent.Indent;
  {$IFDEF VerboseIndenter}
  DebugLn(['TSourceNotebook.EditorGetIndent Indent=',Indent]);
  {$ENDIF}
  {$IFDEF VerboseIndenter}
  DebugLn(['TSourceNotebook.EditorGetIndent Apply to FirstLinePos+1']);
  {$ENDIF}
  SetIndentProc(LogCaret.Y, Indent, 0,' ');
  SrcEdit.CursorScreenXY:=Point(Indent+1,SrcEdit.CursorScreenXY.Y);
end;

procedure TSourceNotebook.OnApplicationDeactivate(Sender: TObject);
begin
  if (CodeContextFrm<>nil) then
    CodeContextFrm.Hide;
  if (Manager<>nil) and (Manager.FHints<>nil) then
    Manager.FHints.HideIfVisible;
end;

procedure TSourceNotebook.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TSourceNotebook.EditorKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TSourceNotebook.ShowSynEditHint(const MousePos: TPoint);
var
  EditPos: TPoint;
  ASrcEdit: TSourceEditor;
  ASynEdit: TSynEdit;
  EditCaret: TPoint;
  AMark: TSourceMark;
  i: integer;
  HintStr: String;
  CurHint: String;
  MLine: TSynEditMarkLine;
begin
  // hide other hints
  //debugln('TSourceNotebook.ShowSynEditHint A');
  Application.HideHint;
  //
  ASrcEdit:=GetActiveSE;
  if ASrcEdit=nil then exit;
  ASynEdit:=ASrcEdit.EditorComponent;
  EditPos:=ASynEdit.ScreenToClient(MousePos);
  if not PtInRect(ASynEdit.ClientRect,EditPos) then exit;
  EditCaret:=ASynEdit.PhysicalToLogicalPos(ASynEdit.PixelsToRowColumn(EditPos));
  if (EditCaret.Y<1) then exit;
  if EditPos.X<ASynEdit.Gutter.Width then begin
    // hint for a gutter item
    if EditorOpts.ShowGutterHints then begin
      HintStr:='';
      MLine := ASynEdit.Marks.Line[EditCaret.Y];
      if MLine <> nil then begin
        if ASynEdit.BookMarkOptions.DrawBookmarksFirst then
          MLine.Sort(smsoBookmarkFirst, smsoColumn)
        else
          MLine.Sort(smsoBookMarkLast, smsoColumn);

        for i := 0 to MLine.Count - 1 do begin
          if not (MLine[i] is TSourceMark) then continue;
          AMark := TSourceMark(MLine[i]);
          if AMark = nil then continue;
          CurHint:=AMark.GetHint;
          if CurHint='' then continue;
          if HintStr<>'' then HintStr:=HintStr+LineEnding;
          HintStr:=HintStr+CurHint;
        end;

        if (MessagesView<>nil) then
          MessagesView.SourceEditorHint(MLine,HintStr);
      end;

      if HintStr<>'' then
        Manager.ActivateHint(MousePos,'',HintStr);
    end;
  end else begin
    // hint for source
    if Assigned(Manager) and Assigned(Manager.OnShowHintForSource) then
      Manager.OnShowHintForSource(ASrcEdit,EditCaret, True);
  end;
end;

procedure TSourceNotebook.SetIncrementalSearchStr(const AValue: string);
begin
  if FIncrementalSearchStr=AValue then exit;
  FIncrementalSearchStr:=AValue;
  IncrementalSearch(False, False);
end;

procedure TSourceNotebook.IncrementalSearch(ANext, ABackward: Boolean);
const
  SEARCH_OPTS: array[Boolean] of TSynSearchOptions = ([], [ssoBackwards]);
var
  CurEdit: TSynEdit;
  AStart : TPoint;
begin
  if not (snIncrementalFind in States)
  then begin
    UpdateStatusBar;
    Exit;
  end;
  if FIncrementalSearchEditor = nil then Exit;

  // search string
  CurEdit := FIncrementalSearchEditor.EditorComponent;
  CurEdit.BeginUpdate;
  if FIncrementalSearchStr<>''
  then begin
    // search from search start position when not searching for the next
    AStart := CurEdit.LogicalCaretXY;
    if not ANext
    then AStart := FIncrementalSearchStartPos
    else if ABackward
    then AStart := CurEdit.BlockBegin;
    FIncrementalSearchBackwards:=ABackward;
    CurEdit.SearchReplaceEx(FIncrementalSearchStr,'', SEARCH_OPTS[ABackward], AStart);

    // searching next resets incremental history
    if ANext
    then begin
      FIncrementalSearchStartPos := CurEdit.BlockBegin;
    end;

    // cut the not found
    FIncrementalSearchStr := CurEdit.SelText;

    CurEdit.SetHighlightSearch(FIncrementalSearchStr, []);
    if Length(FIncrementalSearchStr) > 0
    then FIncrementalFoundStr := FIncrementalSearchStr;
  end
  else begin
    // go to start
    CurEdit.LogicalCaretXY:= FIncrementalSearchStartPos;
    CurEdit.BlockBegin:=CurEdit.LogicalCaretXY;
    CurEdit.BlockEnd:=CurEdit.BlockBegin;
    CurEdit.SetHighlightSearch('', []);
  end;
  FIncrementalSearchPos:=CurEdit.LogicalCaretXY;
  CurEdit.EndUpdate;

  UpdateStatusBar;
end;

procedure TSourceNotebook.Activate;
begin
  inherited Activate;
  if assigned(Manager) then
    Manager.ActiveSourceWindow := self;
  if assigned(Manager) then
    Manager.DoWindowFocused(Self);
end;

procedure TSourceNotebook.UpdateActiveEditColors(AEditor: TSynEdit);
begin
  if AEditor=nil then exit;
  EditorOpts.SetMarkupColors(AEditor);
  AEditor.UseIncrementalColor:= snIncrementalFind in States;
end;

procedure TSourceNotebook.ClearExecutionLines;
var
  i: integer;
begin
  for i := 0 to EditorCount - 1 do
    Editors[i].ExecutionLine := -1;
end;

procedure TSourceNotebook.ClearExecutionMarks;
var
  i: integer;
begin
  for i := 0 to EditorCount - 1 do
    Editors[i].ClearExecutionMarks;
end;

//-----------------------------------------------------------------------------

procedure InternalInit;
var
  h: TLazSyntaxHighlighter;
begin
  // fetch the resourcestrings before they are translated
  EnglishGPLNotice:=lisGPLNotice;
  EnglishLGPLNotice:=lisLGPLNotice;
  EnglishModifiedLGPLNotice:=lisModifiedLGPLNotice;
  EnglishMITNotice:=lisMITNotice;

  for h:=Low(TLazSyntaxHighlighter) to High(TLazSyntaxHighlighter) do
    Highlighters[h]:=nil;
  IDESearchInText:=@SearchInText;
  PasBeautifier := TSynBeautifierPascal.Create(nil);

  SRCED_LOCK  := DebugLogger.RegisterLogGroup('SRCED_LOCK' {$IFDEF SRCED_LOCK} , True {$ENDIF} );
  SRCED_OPEN  := DebugLogger.RegisterLogGroup('SRCED_OPEN' {$IFDEF SRCED_OPEN} , True {$ENDIF} );
  SRCED_CLOSE := DebugLogger.RegisterLogGroup('SRCED_CLOSE' {$IFDEF SRCED_CLOSE} , True {$ENDIF} );
  SRCED_PAGES := DebugLogger.RegisterLogGroup('SRCED_PAGES' {$IFDEF SRCED_PAGES} , True {$ENDIF} );

  IDEWindowsGlobalOptions.Add(NonModalIDEWindowNames[nmiwSourceNoteBook], False);
end;

procedure InternalFinal;
var
  h: TLazSyntaxHighlighter;
begin
  for h:=Low(TLazSyntaxHighlighter) to High(TLazSyntaxHighlighter) do
    FreeThenNil(Highlighters[h]);
  FreeThenNil(aWordCompletion);
  FreeAndNil(PasBeautifier);
end;

{ TSourceEditorManagerBase }

procedure TSourceEditorManagerBase.DoMacroRecorderState(Sender: TObject);
var
  i: Integer;
begin
  For i := 0 to SourceWindowCount - 1 do
    TSourceNotebook(SourceWindows[i]).UpdateStatusBar;
  DoEditorMacroStateChanged;
end;

procedure TSourceEditorManagerBase.FreeSourceWindows;
var
  s: TSourceEditorWindowInterface;
begin
  PasBeautifier.OnGetDesiredIndent := nil;
  FSourceWindowByFocusList.Clear;
  while FSourceWindowList.Count > 0 do begin
    s := TSourceEditorWindowInterface(FSourceWindowList[0]);
    FSourceWindowList.Delete(0);
    s.Free;
  end;
  FSourceWindowList.Clear;
end;

function TSourceEditorManagerBase.GetActiveSourceWindowIndex: integer;
begin
  Result := IndexOfSourceWindow(ActiveSourceWindow);
end;

function TSourceEditorManagerBase.GetSourceWindowByLastFocused(Index: Integer): TSourceEditorWindowInterface;
begin
  Result := TSourceEditorWindowInterface(FSourceWindowByFocusList[Index]);
end;

procedure TSourceEditorManagerBase.SetActiveSourceWindowIndex(const AValue: integer);
begin
  ActiveSourceWindow := SourceWindows[AValue];
end;

procedure TSourceEditorManagerBase.SetShowTabs(const AShowTabs: Boolean);
begin
  FShowTabs := AShowTabs;
end;

function TSourceEditorManagerBase.GetActiveSourceWindow: TSourceEditorWindowInterface;
begin
  Result := FActiveWindow;
end;

procedure TSourceEditorManagerBase.SetActiveSourceWindow(
  const AValue: TSourceEditorWindowInterface);
var
  NewWindow: TSourceNotebook;
begin
  NewWindow:= AValue as TSourceNotebook;
  if NewWindow = FActiveWindow then exit;

  //debugln(['TSourceEditorManagerBase.SetActiveSourceWindow ',dbgSourceNoteBook(FActiveWindow),' ',dbgSourceNoteBook(NewWindow)]);
  if (FActiveWindow <> nil) and (NewWindow <> nil) and (FActiveWindow.Focused) then
    NewWindow.SetFocus;

  FActiveWindow := NewWindow;
  FSourceWindowByFocusList.Remove(NewWindow);
  FSourceWindowByFocusList.Insert(0, NewWindow);

  if Assigned(OnCurrentCodeBufferChanged) then
    OnCurrentCodeBufferChanged(nil);
  FChangeNotifyLists[semWindowActivate].CallNotifyEvents(FActiveWindow);
  DoActiveEditorChanged;
end;

function TSourceEditorManagerBase.GetSourceWindows(Index: integer
  ): TSourceEditorWindowInterface;
begin
  Result := TSourceEditorWindowInterface(FSourceWindowList[Index]);
end;

procedure TSourceEditorManagerBase.DoWindowFocused(AWindow: TSourceNotebook);
begin
  FChangeNotifyLists[semWindowFocused].CallNotifyEvents(FActiveWindow);
end;

function TSourceEditorManagerBase.GetActiveEditor: TSourceEditorInterface;
begin
  If FActiveWindow <> nil then
    Result := FActiveWindow.ActiveEditor
  else
    Result := nil;
end;

procedure TSourceEditorManagerBase.SetActiveEditor(
  const AValue: TSourceEditorInterface);
var
  Window: TSourceEditorWindowInterface;
begin
  inc(FActiveEditorLock);
  try
    if (FActiveWindow <> nil) and (FActiveWindow.IndexOfEditor(AValue) >= 0) then
      Window := FActiveWindow
    else
      Window := SourceWindowWithEditor(AValue);
    if Window = nil then exit;
    ActiveSourceWindow := TSourceNotebook(Window);
    Window.ActiveEditor := AValue;
  finally
    dec(FActiveEditorLock);
    DoActiveEditorChanged;
  end;
end;

procedure TSourceEditorManagerBase.DoActiveEditorChanged;
begin
  if FActiveEditorLock > 0 then exit;
  if FUpdateLock > 0 then begin
    include(FUpdateFlags, ufMgrActiveEditorChanged);
    exit;
  end;
  exclude(FUpdateFlags, ufMgrActiveEditorChanged);
  FChangeNotifyLists[semEditorActivate].CallNotifyEvents(ActiveEditor);
end;

procedure TSourceEditorManagerBase.DoEditorStatusChanged(AEditor: TSourceEditor);
begin
  CodeToolsToSrcEditTimer.Enabled:=false;
  FChangeNotifyLists[semEditorStatus].CallNotifyEvents(AEditor);
end;

function TSourceEditorManagerBase.GetSourceEditors(Index: integer): TSourceEditorInterface;
var
  i: Integer;
begin
  i := 0;
  while (i < SourceWindowCount) and (Index >= SourceWindows[i].Count) do begin
    Index := Index - SourceWindows[i].Count;
    inc(i);
  end;
  if (i < SourceWindowCount) then
    Result := SourceWindows[i].Items[Index]
  else
    Result := nil;
end;

function TSourceEditorManagerBase.GetUniqueSourceEditors(Index: integer): TSourceEditorInterface;
var
  i: Integer;
begin
  for i := 0 to SourceEditorCount - 1 do begin
    Result := SourceEditors[i];
    if (TSourceEditor(Result).SharedEditorCount = 0) or
       (TSourceEditor(Result).SharedEditors[0] = Result)
    then
      dec(Index);
    if Index < 0 then exit;
  end;
  Result := nil;
end;

function TSourceEditorManagerBase.SourceWindowWithEditor(
  const AEditor: TSourceEditorInterface): TSourceEditorWindowInterface;
var
  i: Integer;
begin
  Result := nil;
  for i := FSourceWindowList.Count-1 downto 0 do begin
    if TSourceNotebook(SourceWindows[i]).IndexOfEditor(AEditor) >= 0 then begin
      Result := SourceWindows[i];
      break;
    end;
  end;
end;

function TSourceEditorManagerBase.SourceWindowCount: integer;
begin
  if assigned(FSourceWindowList) then
    Result := FSourceWindowList.Count
  else
    Result := 0;
end;

function TSourceEditorManagerBase.IndexOfSourceWindow(
  AWindow: TSourceEditorWindowInterface): integer;
begin
  Result := SourceWindowCount - 1;
  while Result >= 0 do Begin
    if SourceWindows[Result] = AWindow then
      exit;
    dec(Result);
  end;
end;

function TSourceEditorManagerBase.IndexOfSourceWindowByLastFocused(AWindow: TSourceEditorWindowInterface): integer;
begin
  Result := FSourceWindowByFocusList.IndexOf(AWindow);
end;

function TSourceEditorManagerBase.SourceEditorIntfWithFilename(
  const Filename: string): TSourceEditorInterface;
var
  i: Integer;
begin
  for i:=0 to SourceWindowCount-1 do
  begin
    Result:=SourceWindows[i].SourceEditorIntfWithFilename(Filename);
    if Result<>nil then exit;
  end;
  Result:=nil;
end;

function TSourceEditorManagerBase.SourceEditorCount: integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to SourceWindowCount - 1 do
    Result := Result + SourceWindows[i].Count;
end;

function TSourceEditorManagerBase.UniqueSourceEditorCount: integer;
var
  SrcEdit: TSourceEditor;
  i: Integer;
begin
  Result := 0;
  for i := 0 to SourceEditorCount - 1 do begin
    SrcEdit := TSourceEditor(SourceEditors[i]);
    if (SrcEdit.SharedEditorCount = 0) or (SrcEdit.SharedEditors[0] = SrcEdit) then
      inc(Result);
  end;
end;

function TSourceEditorManagerBase.GetEditorControlSettings(EditControl: TControl): boolean;
begin
  Result:=true;
  if EditControl is TSynEdit then begin
    EditorOpts.GetSynEditSettings(TSynEdit(EditControl));
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

function TSourceEditorManagerBase.GetHighlighterSettings(Highlighter: TObject): boolean;
begin
  Result:=true;
  if Highlighter is TSynCustomHighlighter then begin
    EditorOpts.GetHighlighterSettings(TSynCustomHighlighter(Highlighter));
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

function TSourceEditorManagerBase.GetDefaultCompletionForm: TSourceEditCompletion;
var
  i: Integer;
begin
  Result := FDefaultCompletionForm;
  if Result <> nil then exit;
  FDefaultCompletionForm := TSourceEditCompletion.Create(Self);
  FDefaultCompletionForm.LongLineHintTime := EditorOpts.CompletionLongLineHintInMSec;
  FDefaultCompletionForm.LongLineHintType := EditorOpts.CompletionLongLineHintType;
  Result := FDefaultCompletionForm;
  for i:=0 to SourceEditorCount - 1 do
    FDefaultCompletionForm.AddEditor(TSourceEditor(SourceEditors[i]).EditorComponent);
end;

function TSourceEditorManagerBase.GetDefaultSynCompletionForm: TCustomForm;
begin
  if Assigned(GetDefaultCompletionForm) then
    Result := GetDefaultCompletionForm.TheForm
  else
    Result := nil;
end;

function TSourceEditorManagerBase.GetSynCompletionLinesInWindow: integer;
var
  ComplForm: TCustomForm;
begin
  ComplForm := GetDefaultSynCompletionForm;
  if Assigned(ComplForm) then
    Result := TSynBaseCompletionForm(ComplForm).NbLinesInWindow
  else
    Result := -1;
end;

procedure TSourceEditorManagerBase.SetSynCompletionLinesInWindow(LineCnt: integer);
var
  ComplForm: TCustomForm;
begin
  ComplForm := GetDefaultSynCompletionForm;
  if Assigned(ComplForm) then
    TSynBaseCompletionForm(ComplForm).NbLinesInWindow := LineCnt;
end;

procedure TSourceEditorManagerBase.FreeCompletionPlugins;
var
  p: TSourceEditorCompletionPlugin;
begin
  while FCompletionPlugins.Count > 0 do begin
    p := TSourceEditorCompletionPlugin(FCompletionPlugins[0]);
    FCompletionPlugins.Delete(0);
    p.Free;
  end;
  FCompletionPlugins.Clear;
end;

function TSourceEditorManagerBase.GetScreenRectForToken(AnEditor: TCustomSynEdit;
  PhysColumn, PhysRow, EndColumn: Integer): TRect;
begin
  Result.TopLeft := AnEditor.ClientToScreen(AnEditor.RowColumnToPixels(Point(PhysColumn, PhysRow)));
  Result.BottomRight := AnEditor.ClientToScreen(AnEditor.RowColumnToPixels(Point(EndColumn+1, PhysRow+1)));
end;

function TSourceEditorManagerBase.GetShowTabs: Boolean;
begin
  Result := FShowTabs;
end;

function TSourceEditorManagerBase.GetMarklingProducers(Index: integer
  ): TSourceMarklingProducer;
begin
  Result:=TSourceMarklingProducer(fProducers[Index]);
end;

procedure TSourceEditorManagerBase.DoWindowShow(AWindow: TSourceNotebook);
begin
  FChangeNotifyLists[semWindowShow].CallNotifyEvents(AWindow);
end;

procedure TSourceEditorManagerBase.DoWindowHide(AWindow: TSourceNotebook);
begin
  FChangeNotifyLists[semWindowHide].CallNotifyEvents(AWindow);
end;

procedure TSourceEditorManagerBase.SyncMessageWnd(Sender: TObject);
begin
  MessagesView.MessagesFrame1.ApplyMultiSrcChanges(Sender as TETMultiSrcChanges);
end;

procedure TSourceEditorManagerBase.BeginAutoFocusLock;
begin
  inc(FAutoFocusLock);
end;

procedure TSourceEditorManagerBase.EndAutoFocusLock;
begin
  dec(FAutoFocusLock);
end;

function TSourceEditorManagerBase.HasAutoFocusLock: Boolean;
begin
  Result := FAutoFocusLock > 0;
end;

function TSourceEditorManagerBase.GetActiveCompletionPlugin: TSourceEditorCompletionPlugin;
begin
  Result := FActiveCompletionPlugin;
end;

function TSourceEditorManagerBase.GetCompletionBoxPosition: integer;
begin
  Result:=-1;
  if (FDefaultCompletionForm<>nil) and FDefaultCompletionForm.IsActive then
    Result := FDefaultCompletionForm.Position;
end;

function TSourceEditorManagerBase.GetCompletionPlugins(Index: integer
  ): TSourceEditorCompletionPlugin;
begin
  Result:=TSourceEditorCompletionPlugin(fCompletionPlugins[Index]);
end;

function TSourceEditorManagerBase.FindIdentCompletionPlugin(
  SrcEdit: TSourceEditor; JumpToError: boolean; var s: string; var BoxX,
  BoxY: integer; var UseWordCompletion: boolean): boolean;
var
  i: Integer;
  Plugin: TSourceEditorCompletionPlugin;
  Handled: Boolean;
  Cancel: Boolean;
begin
  for i:=0 to CompletionPluginCount-1 do begin
    Plugin := CompletionPlugins[i];
    Handled:=false;
    Cancel:=false;
    Plugin.Init(SrcEdit,JumpToError,Handled,Cancel,s,BoxX,BoxY);
    if Cancel then begin
      DeactivateCompletionForm;
      exit(false);
    end;
    if Handled then begin
      FActiveCompletionPlugin:=Plugin;
      exit(true);
    end;
  end;

  if not (SrcEdit.SyntaxHighlighterType in [lshFreePascal, lshDelphi]) then
    UseWordCompletion:=true;
  Result:=true;
end;

function TSourceEditorManagerBase.CompletionPluginCount: integer;
begin
  Result:=fCompletionPlugins.Count;
end;

procedure TSourceEditorManagerBase.DeactivateCompletionForm;
var
  PluginFocused: Boolean;
begin
  {$IFDEF VerboseIDECompletionBox}
  DebugLnEnter(['>> TSourceEditorManagerBase.DeactivateCompletionForm']);
  try
  {$ENDIF}
  if ActiveCompletionPlugin<>nil then begin
    ActiveCompletionPlugin.Cancel;
    FActiveCompletionPlugin:=nil;
  end;

  if (FDefaultCompletionForm=nil) or
     (FDefaultCompletionForm.CurrentCompletionType = ctNone)
  then
    exit;

  // Do not move focus, if it was moved by user
  PluginFocused := FDefaultCompletionForm.TheForm.Focused;
  {$IFDEF VerboseIDECompletionBox}
  DebugLn(['DeactivateCompletionForm  PluginFocused=', dbgs(PluginFocused), '  ActiveEditor=', DbgSName(ActiveEditor)]);
  {$ENDIF}

  // clear the IdentifierList (otherwise it would try to update everytime
  // the codetools are used)
  CodeToolBoss.IdentifierList.Clear;
  FDefaultCompletionForm.CurrentCompletionType:=ctNone;

  // SetFocus and Deactivate will all trigger this proc to be reentered.
  //   Setting "CurrentCompletionType:=ctNone" ensures an immediate exit

  // Due to a bug under XFCE we must move focus before we close the form
  // This is relevant if the form is closed by enter/escape key
  if PluginFocused and (ActiveEditor<>nil) then
    TSourceEditor(ActiveEditor).FocusEditor;

  // hide/close the form
  FDefaultCompletionForm.Deactivate;

  // Ensure focus *after* the form was closed.
  // This is the normal implementation (all but XFCE)
  if PluginFocused and (ActiveEditor<>nil) then
    TSourceEditor(ActiveEditor).FocusEditor;
  {$IFDEF VerboseIDECompletionBox}
  finally
    DebugLnExit(['<< TSourceEditorManagerBase.DeactivateCompletionForm']);
  end;
  {$ENDIF}
end;

procedure TSourceEditorManagerBase.RegisterCompletionPlugin(
  Plugin: TSourceEditorCompletionPlugin);
begin
  fCompletionPlugins.Add(Plugin);
  Plugin.FreeNotification(Self);
end;

procedure TSourceEditorManagerBase.UnregisterCompletionPlugin(
  Plugin: TSourceEditorCompletionPlugin);
begin
  Plugin.RemoveFreeNotification(Self);
  fCompletionPlugins.Remove(Plugin);
end;

procedure TSourceEditorManagerBase.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
  begin
    if Assigned(fCompletionPlugins) then
      fCompletionPlugins.Remove(AComponent);
    if ActiveCompletionPlugin = AComponent then
      DeactivateCompletionForm;
    if AComponent is TSourceMarklingProducer then
      fProducers.Remove(AComponent);
  end;
end;

constructor TSourceEditorManagerBase.Create(AOwner: TComponent);
var
  i: TsemChangeReason;
  h: TSrcEditMangerHandlerType;
begin
  FMacroRecorder := TIdeEditorMacro.Create(Self);
  FMacroRecorder.OnStateChange := @DoMacroRecorderState;
  OnEditorMacroStateChange := @DoMacroRecorderState;
  if EditorMacroForRecording = nil then
    EditorMacroForRecording := FMacroRecorder;

  FShowTabs := True;
  FUpdateFlags := [];
  FAutoFocusLock := 0;
  for i := low(TsemChangeReason) to high(TsemChangeReason) do
    FChangeNotifyLists[i] := TMethodList.Create;
  for h:=low(FHandlers) to high(FHandlers) do
    FHandlers[h] := TMethodList.Create;
  SrcEditorIntf.SourceEditorManagerIntf := Self;
  FSourceWindowList := TFPList.Create;
  FSourceWindowByFocusList := TFPList.Create;
  FCompletionPlugins := TFPList.Create;
  FUpdateLock := 0;
  FActiveEditorLock := 0;
  fProducers := TFPList.Create;
  FChangesQueuedForMsgWnd:=TETMultiSrcChanges.Create(Self);
  FChangesQueuedForMsgWnd.AutoSync:=true;
  FChangesQueuedForMsgWnd.OnSync:=@SyncMessageWnd;
  inherited;
end;

destructor TSourceEditorManagerBase.Destroy;
var
  i: integer;
  cr: TsemChangeReason;
  h: TSrcEditMangerHandlerType;
begin
  FreeAndNil(FChangesQueuedForMsgWnd);
  for i:=MarklingProducerCount-1 downto 0 do
    MarklingProducers[i].Free;
  FreeAndNil(fProducers);
  FActiveWindow := nil;
  FreeCompletionPlugins;
  FreeSourceWindows;
  SrcEditorIntf.SourceEditorManagerIntf := nil; // xx move down
  if EditorMacroForRecording = FMacroRecorder then
    EditorMacroForRecording := nil;
  FreeAndNil(FMacroRecorder);
  FreeAndNil(FCompletionPlugins);
  FreeAndNil(FSourceWindowList);
  FreeAndNil(FSourceWindowByFocusList);
  inherited Destroy;
  for cr := low(TsemChangeReason) to high(TsemChangeReason) do
    FreeAndNil(FChangeNotifyLists[cr]);
  for h:=low(FHandlers) to high(FHandlers) do
    FreeAndNil(FHandlers[h]);
end;

procedure TSourceEditorManagerBase.RegisterChangeEvent(
  AReason: TsemChangeReason; AHandler: TNotifyEvent);
begin
  FChangeNotifyLists[AReason].Add(TMethod(AHandler));
end;

procedure TSourceEditorManagerBase.UnRegisterChangeEvent(
  AReason: TsemChangeReason; AHandler: TNotifyEvent);
begin
  FChangeNotifyLists[AReason].Remove(TMethod(AHandler));
end;

procedure TSourceEditorManagerBase.RegisterCopyPasteEvent(
  AHandler: TSemCopyPasteEvent);
begin
  FHandlers[semhtCopyPaste].Add(TMethod(AHandler));
end;

procedure TSourceEditorManagerBase.UnRegisterCopyPasteEvent(
  AHandler: TSemCopyPasteEvent);
begin
  FHandlers[semhtCopyPaste].Remove(TMethod(AHandler));
end;

function TSourceEditorManagerBase.MarklingProducerCount: integer;
begin
  Result:=fProducers.Count;
end;

procedure TSourceEditorManagerBase.RegisterMarklingProducer(
  aProducer: TSourceMarklingProducer);
begin
  if fProducers.IndexOf(aProducer)>=0 then
    RaiseGDBException('TSourceEditorManagerBase.RegisterProducer already registered');
  fProducers.Add(aProducer);
  FreeNotification(aProducer);
end;

procedure TSourceEditorManagerBase.UnregisterMarklingProducer(
  aProducer: TSourceMarklingProducer);
var
  i: LongInt;
begin
  i:=fProducers.IndexOf(aProducer);
  if i<0 then exit;
  fProducers.Delete(i);
  RemoveFreeNotification(aProducer);
end;

procedure TSourceEditorManagerBase.InvalidateMarklingsOfAllFiles(
  aProducer: TSourceMarklingProducer);
var
  SrcWnd: TSourceEditorWindowInterface;
  i, j: Integer;
  SrcEdit: TSourceEditor;
begin
  if aProducer=nil then exit;
  for i := 0 to SourceWindowCount - 1 do
  begin
    SrcWnd:=SourceWindows[i];
    for j:=0 to SrcWnd.Count-1 do
    begin
      SrcEdit:=TSourceEditor(SrcWnd[j]);
      SrcEdit.FSharedValues.FMarklingsValid:=false;
    end;
  end;
end;

procedure TSourceEditorManagerBase.InvalidateMarklings(
  aProducer: TSourceMarklingProducer; aFilename: string);
var
  SrcWnd: TSourceEditorWindowInterface;
  i: Integer;
  SrcEdit: TSourceEditor;
begin
  if aProducer=nil then exit;
  for i := 0 to SourceWindowCount - 1 do
  begin
    SrcWnd:=SourceWindows[i];
    SrcEdit:=TSourceEditor(SrcWnd.SourceEditorIntfWithFilename(aFilename));
    if SrcEdit<>nil then
      SrcEdit.FSharedValues.FMarklingsValid:=false;
  end;
end;

procedure TSourceEditorManagerBase.IncUpdateLockInternal;
begin
  if FUpdateLock = 0 then begin
    FUpdateFlags := [];
    // Debugger cause ProcessMessages, which could lead to entering methods in unexpected order
    DebugBoss.LockCommandProcessing;
    Screen.BeginWaitCursor;
  end;
  inc(FUpdateLock);
end;

procedure TSourceEditorManagerBase.DecUpdateLockInternal;
begin
  dec(FUpdateLock);
  if FUpdateLock = 0 then begin
    try
      Screen.EndWaitCursor;
      if (ufShowWindowOnTop in FUpdateFlags) then
        ShowActiveWindowOnTop(ufShowWindowOnTopFocus in FUpdateFlags);
      if (ufMgrActiveEditorChanged in FUpdateFlags) then
        DoActiveEditorChanged;
    finally
      DebugBoss.UnLockCommandProcessing;
    end;
  end;
end;

procedure TSourceEditorManagerBase.IncUpdateLock;
var
  i: Integer;
begin
  IncUpdateLockInternal;
  for i := 0 to SourceWindowCount - 1 do
    TSourceNotebook(SourceWindows[i]).IncUpdateLockInternal;
end;

procedure TSourceEditorManagerBase.DecUpdateLock;
var
  i: Integer;
begin
  try
    for i := 0 to SourceWindowCount - 1 do
      TSourceNotebook(SourceWindows[i]).DecUpdateLockInternal;
  finally
    DecUpdateLockInternal;
  end;
end;

procedure TSourceEditorManagerBase.ShowActiveWindowOnTop(Focus: Boolean);
begin
  if ActiveSourceWindow = nil then exit;
  if FUpdateLock > 0 then begin
    include(FUpdateFlags, ufShowWindowOnTop);
    if Focus then
      include(FUpdateFlags, ufShowWindowOnTopFocus);
    exit;
  end;
  IDEWindowCreators.ShowForm(ActiveSourceWindow,true);
  if Focus and ActiveSourceWindow.IsVisible then
    TSourceNotebook(ActiveSourceWindow).FocusEditor;
end;

{ TSourceEditorManager }

function TSourceEditorManager.GetActiveSourceNotebook: TSourceNotebook;
begin
  Result := TSourceNotebook(inherited ActiveSourceWindow);
end;

function TSourceEditorManager.GetActiveSrcEditor: TSourceEditor;
begin
  Result := TSourceEditor(inherited ActiveEditor);
end;

function TSourceEditorManager.GetSourceEditorsByPage(WindowIndex,
  PageIndex: integer): TSourceEditor;
begin
  if SourceWindows[WindowIndex] <> nil then
    Result := SourceWindows[WindowIndex].FindSourceEditorWithPageIndex(PageIndex)
  else
    Result := nil;
end;

function TSourceEditorManager.GetSourceNbByLastFocused(Index: Integer): TSourceNotebook;
begin
  Result := TSourceNotebook(inherited SourceWindowByLastFocused[Index]);
end;

function TSourceEditorManager.GetSrcEditors(Index: integer): TSourceEditor;
begin
  Result := TSourceEditor(inherited SourceEditors[Index]);
end;

procedure TSourceEditorManager.SetActiveSourceNotebook(const AValue: TSourceNotebook);
begin
  inherited ActiveSourceWindow := AValue;
end;

function TSourceEditorManager.GetSourceNotebook(Index: integer): TSourceNotebook;
begin
  Result := TSourceNotebook(inherited SourceWindows[Index]);
end;

procedure TSourceEditorManager.SetActiveSrcEditor(const AValue: TSourceEditor);
begin
  inherited ActiveEditor := AValue;
end;

function TSourceEditorManager.SourceWindowWithEditor(
  const AEditor: TSourceEditorInterface): TSourceNotebook;
begin
  Result := TSourceNotebook(inherited SourceWindowWithEditor(AEditor));
end;

function TSourceEditorManager.ActiveOrNewSourceWindow: TSourceNotebook;
var
  i: Integer;
begin
  Result := ActiveSourceWindow;
  if Result <> nil then exit;
  if SourceWindowCount>0 then begin
    for i:=0 to SourceWindowCount-1 do
    begin
      Result:=SourceWindows[i];
      if Result.FIsClosing then continue;
      ActiveSourceWindow := Result;
      exit;
    end;
  end;

  Result := CreateNewWindow(True);
  ActiveSourceWindow := Result;
end;

procedure TSourceEditorManager.AddCustomJumpPoint(ACaretXY: TPoint;
  ATopLine: integer; AEditor: TSourceEditor; DeleteForwardHistory: boolean);
begin
  if Assigned(OnAddJumpPoint) then
    OnAddJumpPoint(ACaretXY, ATopLine, AEditor, DeleteForwardHistory);
end;

function TSourceEditorManager.NewSourceWindow: TSourceNotebook;
begin
  Result := CreateNewWindow(True);
  ActiveSourceWindow := Result;
end;

procedure TSourceEditorManager.CreateSourceWindow(Sender: TObject;
  aFormName: string; var AForm: TCustomForm; DoDisableAutoSizing: boolean);
var
  i: integer;
begin
  {$IFDEF VerboseIDEDocking}
  debugln(['TSourceEditorManager.CreateSourceWindow Sender=',DbgSName(Sender),' FormName="',aFormName,'"']);
  {$ENDIF}
  // Get ID from Name
  i := length(aFormName);
  while (i >= 1) and (aFormName[i] in ['0'..'9']) do
    dec(i);
  inc(i);
  i := StrToIntDef(copy(aFormName, i, MaxInt), 1)-1;
  AForm := CreateNewWindow(false,DoDisableAutoSizing, i);
  AForm.Name:=aFormName;
end;

procedure TSourceEditorManager.GetDefaultLayout(Sender: TObject; aFormName: string;
  out aBounds: TRect; out DockSibling: string; out DockAlign: TAlign);
var
  i: LongInt;
  ScreenR: TRect;
begin
  DockSibling:='';
  DockAlign:=alNone;
  i:=StrToIntDef(
    copy(aFormName,length(NonModalIDEWindowNames[nmiwSourceNoteBook])+1,length(aFormName)),
    0);
  {$IFDEF VerboseIDEDocking}
  debugln(['TSourceEditorManager.GetDefaultLayout ',aFormName,' i=',i]);
  {$ENDIF}
  ScreenR:=IDEWindowCreators.GetScreenrectForDefaults;
  if ObjectInspector1<>nil then
  begin
    aBounds.Left := ObjectInspector1.Left + ObjectInspector1.Width + ObjectInspector1.Scale96ToForm(5);
    aBounds.Top := ObjectInspector1.Top;
  end
  else
  begin
    aBounds.Left := ScreenR.Left+MulDiv(200, Screen.PixelsPerInch, 96);
    aBounds.Top := ScreenR.Top+MulDiv(120, Screen.PixelsPerInch, 96);
  end;
  Inc(aBounds.Left,MulDiv(30, Screen.PixelsPerInch, 96)*i);
  Inc(aBounds.Top, MulDiv(30, Screen.PixelsPerInch, 96)*i);
  aBounds.Right:=ScreenR.Right-MulDiv(30, Screen.PixelsPerInch, 96);
  aBounds.Bottom:=ScreenR.Bottom-MulDiv(200, Screen.PixelsPerInch, 96);
  if (i=0) and (IDEDockMaster<>nil) then begin
    DockSibling:=NonModalIDEWindowNames[nmiwMainIDE];
    DockAlign:=alBottom;
  end;
end;

function TSourceEditorManager.SourceWindowWithPage(const APage: TTabSheet): TSourceNotebook;
var
  i: Integer;
begin
  Result := nil;
  for i := FSourceWindowList.Count-1 downto 0 do begin
    if TSourceNotebook(SourceWindows[i]).FNoteBook.IndexOf(APage) >= 0 then begin
      Result := SourceWindows[i];
      break;
    end;
  end;
end;

procedure TSourceEditorManager.SrcEditMenuProcedureJumpGetCaption(
  Sender: TObject; var ACaption, AHint: string);
var
  ShortFileName, CurFilename: String;
  MainCodeBuf: TCodeBuffer;
  CodeTool: TCodeTool;
  CaretXY: TCodeXYPosition;
  CleanPos: integer;
  CodeNode: TCodeTreeNode;
  ProcNode: TCodeTreeNode;
  ProcName: String;
  SrcEdit: TSourceEditor;
begin
  // ask Codetools
  SrcEdit:=GetActiveSE;
  if not Assigned(SrcEdit) then Exit;
  CurFilename:=SrcEdit.FileName;
  ShortFileName:=ExtractFileName(CurFilename);
  MainCodeBuf:=nil;
  if FilenameIsPascalUnit(ShortFileName)
  or (CompareFileExt(ShortFileName,'.inc',true)=0) then
    MainCodeBuf:=CodeToolBoss.GetMainCode(SrcEdit.CodeBuffer)
  else if FilenameIsPascalSource(ShortFileName) then
    MainCodeBuf:=SrcEdit.CodeBuffer;
  CodeTool:=nil;
  CaretXY:=CleanCodeXYPosition;
  CaretXY.Code:=SrcEdit.CodeBuffer;
  CaretXY.X:=SrcEdit.CursorTextXY.X;
  CaretXY.Y:=SrcEdit.CursorTextXY.Y;
  CodeNode:=nil;
  if MainCodeBuf<>nil then begin
    CodeToolBoss.Explore(MainCodeBuf,CodeTool,true);
    if CodeTool<>nil then begin
      CodeTool.CaretToCleanPos(CaretXY,CleanPos);
      CodeNode:=CodeTool.FindDeepestNodeAtPos(CleanPos,false);
    end;
  end;

  ProcName:='';
  if CodeNode<>nil then begin
    ProcNode:=CodeNode.GetNodeOfType(ctnProcedure);
    if ProcNode<>nil then
      ProcName:=CodeTool.ExtractProcName(ProcNode,[]);
  end;
  if ProcName<>'' then
    ACaption:=Format(lisJumpToProcedure, [ProcName])
  else
    ACaption:=uemProcedureJump;
end;

function TSourceEditorManager.IndexOfSourceWindowWithID(const AnID: Integer): Integer;
begin
  Result := SourceWindowCount - 1;
  while Result >= 0 do begin
    if SourceWindows[Result].WindowID = AnID then break;
    dec(Result);
  end;
end;

function TSourceEditorManager.SourceWindowWithID(const AnID: Integer): TSourceNotebook;
var
  i: Integer;
begin
  i := IndexOfSourceWindowWithID(AnID);
  if i >= 0 then
    Result := SourceWindows[i]
  else
    Result := CreateNewWindow(False, False, AnID);
end;

function TSourceEditorManager.SourceEditorCount: integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to SourceWindowCount - 1 do
    Result := Result + SourceWindows[i].Count;
end;

function TSourceEditorManager.GetActiveSE: TSourceEditor;
begin
  Result := TSourceEditor(ActiveEditor);
end;

procedure TSourceEditorManager.SetWindowByIDAndPage(AWindowID, APageIndex: integer);
begin
  ActiveSourceWindowIndex := IndexOfSourceWindowWithID(AWindowID);
  ActiveSourceWindow.PageIndex:= APageIndex;
end;

function TSourceEditorManager.SourceEditorIntfWithFilename(
  const Filename: string): TSourceEditor;
begin
  Result := TSourceEditor(inherited SourceEditorIntfWithFilename(Filename));
end;

function TSourceEditorManager.FindSourceEditorWithEditorComponent(
  EditorComp: TComponent): TSourceEditor;
var
  i: Integer;
begin
  Result := nil;
  i := SourceWindowCount - 1;
  while i >= 0 do begin
    Result := SourceWindows[i].FindSourceEditorWithEditorComponent(EditorComp);
    if Result <> nil then break;
    dec(i);
  end;
end;

procedure TSourceEditorManager.NewEditorCreated(AEditor: TSourceEditor);
begin
  if FDefaultCompletionForm <> nil then
    FDefaultCompletionForm.AddEditor(AEditor.EditorComponent);
end;

procedure TSourceEditorManager.EditorRemoved(AEditor: TSourceEditor);
begin
  if FDefaultCompletionForm <> nil then begin
    if FDefaultCompletionForm.Editor = AEditor.EditorComponent then
      DeactivateCompletionForm;
    FDefaultCompletionForm.RemoveEditor(AEditor.EditorComponent);
  end;
end;

procedure TSourceEditorManager.SendEditorCreated(AEditor: TSourceEditor);
begin
  FChangeNotifyLists[semEditorCreate].CallNotifyEvents(AEditor);
end;

procedure TSourceEditorManager.SendEditorDestroyed(AEditor: TSourceEditor);
begin
  FChangeNotifyLists[semEditorDestroy].CallNotifyEvents(AEditor);
end;

procedure TSourceEditorManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
  begin
    if AComponent is TSourceNotebook then
      RemoveWindow(TSourceNotebook(AComponent));
  end;
end;

procedure TSourceEditorManager.ClearErrorLines;
var
  i, j: Integer;
  SrcWin: TSourceNotebook;
begin
  for i := FSourceWindowList.Count - 1 downto 0 do
  begin
    SrcWin := SourceWindows[i];
    for j := 0 to SrcWin.EditorCount - 1 do
      SrcWin.Editors[j].ErrorLine := -1;
  end;
end;

procedure TSourceEditorManager.ClearExecutionLines;
var
  i: Integer;
begin
  for i := FSourceWindowList.Count - 1 downto 0 do
    SourceWindows[i].ClearExecutionLines;
end;

procedure TSourceEditorManager.ClearExecutionMarks;
var
  i: Integer;
begin
  for i := FSourceWindowList.Count - 1 downto 0 do
    SourceWindows[i].ClearExecutionMarks;
end;

procedure TSourceEditorManager.FillExecutionMarks;
var
  i: Integer;
  SE: TSourceEditor;
begin
  for i := FSourceWindowList.Count - 1 downto 0 do begin
    SE := SourceWindows[i].GetActiveSE;
    if SE <> nil then
      SE.FillExecutionMarks;
  end;
end;

procedure TSourceEditorManager.ReloadEditorOptions;
var
  i: Integer;
begin
  for i := FSourceWindowList.Count - 1 downto 0 do
    SourceWindows[i].ReloadEditorOptions;

  AutoStartCompletionBoxTimer.Interval:=EditorOpts.AutoDelayInMSec;
  // reload code templates
  EditorOpts.LoadCodeTemplates(CodeTemplateModul);
  CodeTemplateModul.IndentToTokenStart:=EditorOpts.CodeTemplateIndentToTokenStart;

  FHints.AutoHintTimer.Interval:=EditorOpts.AutoHintDelayInMSec;

  if FDefaultCompletionForm <> nil then begin
    FDefaultCompletionForm.LongLineHintTime := EditorOpts.CompletionLongLineHintInMSec;
    FDefaultCompletionForm.LongLineHintType := EditorOpts.CompletionLongLineHintType;
  end;
end;

function TSourceEditorManager.Beautify(const Src: string): string;
var
  NewIndent, NewTabWidth: Integer;
begin
  Result:=CodeToolBoss.Beautifier.BeautifyStatement(Src,2,[bcfDoNotIndentFirstLine]);

  if (eoTabsToSpaces in EditorOpts.SynEditOptions)
  or (EditorOpts.BlockTabIndent=0) then
    NewTabWidth:=0
  else
    NewTabWidth:=EditorOpts.TabWidth;
  NewIndent:=EditorOpts.BlockTabIndent*EditorOpts.TabWidth+EditorOpts.BlockIndent;

  Result:=BasicCodeTools.ReIndent(Result,2,0,NewIndent,NewTabWidth);
end;

procedure TSourceEditorManager.FindClicked(Sender: TObject);
begin
  if ActiveEditor <> nil then ActiveEditor.StartFindAndReplace(false);
end;

procedure TSourceEditorManager.FindNextClicked(Sender: TObject);
begin
  if ActiveEditor <> nil then ActiveEditor.FindNextUTF8;
end;

procedure TSourceEditorManager.FindPreviousClicked(Sender: TObject);
begin
  if ActiveEditor <> nil then ActiveEditor.FindPrevious;
end;

procedure TSourceEditorManager.ReplaceClicked(Sender: TObject);
begin
  if ActiveEditor <> nil then ActiveEditor.StartFindAndReplace(true);
end;

procedure TSourceEditorManager.IncrementalFindClicked(Sender: TObject);
begin
  if ActiveSourceWindow <> nil then ActiveSourceWindow.BeginIncrementalFind;
end;

procedure TSourceEditorManager.GotoLineClicked(Sender: TObject);
begin
  if ActiveEditor <> nil then ActiveEditor.ShowGotoLineDialog;
end;

procedure TSourceEditorManager.HideHint;
begin
  FHints.HideHint;
end;

procedure TSourceEditorManager.JumpBackClicked(Sender: TObject);
begin
  if ActiveSourceWindow <> nil then HistoryJump(Sender,jhaBack);
end;

procedure TSourceEditorManager.JumpForwardClicked(Sender: TObject);
begin
  if ActiveSourceWindow <> nil then HistoryJump(Sender,jhaForward);
end;

procedure TSourceEditorManager.JumpToNextErrorClicked(Sender: TObject);
begin
  LazarusIDE.DoJumpToNextError(true);
end;

procedure TSourceEditorManager.JumpToPrevErrorClicked(Sender: TObject);
begin
  LazarusIDE.DoJumpToNextError(false);
end;

procedure TSourceEditorManager.JumpToImplementationClicked(Sender: TObject);
begin
  JumpToSection(jmpImplementation);
end;

procedure TSourceEditorManager.JumpToImplementationUsesClicked(Sender: TObject);
begin
  JumpToSection(jmpImplementationUses);
end;

procedure TSourceEditorManager.JumpToInitializationClicked(Sender: TObject);
begin
  JumpToSection(jmpInitialization);
end;

procedure TSourceEditorManager.JumpToInterfaceClicked(Sender: TObject);
begin
  JumpToSection(jmpInterface);
end;

procedure TSourceEditorManager.JumpToInterfaceUsesClicked(Sender: TObject);
begin
  JumpToSection(jmpInterfaceUses);
end;

procedure TSourceEditorManager.JumpToPos(FileName: string;
  Pos: TCodeXYPosition; TopLine, BlockTopLine, BlockBottomLine: Integer);
begin
  if (LazarusIDE.DoOpenFileAndJumpToPos(Filename
       ,Point(Pos.X,Pos.Y), TopLine, BlockTopLine, BlockBottomLine, -1,-1
       ,[ofRegularFile,ofUseCache]) = mrOk)
  then
    ActiveEditor.EditorControl.SetFocus;
end;

procedure TSourceEditorManager.JumpToPos(FileName: string;
  Pos: TCodeXYPosition; TopLine: Integer);
begin
  JumpToPos(FileName, Pos, TopLine, TopLine, TopLine);
end;

procedure TSourceEditorManager.JumpToProcedure(const JumpType: TJumpToProcedureType);
const
  cJumpNames: array[TJumpToProcedureType] of string = (
    'procedure header', 'procedure begin');
var
  SrcEditor: TSourceEditorInterface;
  ProcNode: TCodeTreeNode;
  Tool: TCodeTool;
  CleanPos: integer;
  NewPos: TCodeXYPosition;
  NewTopLine, BlockTopLine, BlockBottomLine: integer;
  JumpFound: Boolean;
begin
  if not LazarusIDE.BeginCodeTools then Exit; //==>

  SrcEditor := SourceEditorManagerIntf.ActiveEditor;
  if not Assigned(SrcEditor) then Exit; //==>

  if CodeToolBoss.Explore(SrcEditor.CodeToolsBuffer as TCodeBuffer, Tool, false, false) then
  begin
    if Tool.CaretToCleanPos(
        CodeXYPosition(SrcEditor.CursorTextXY.X, SrcEditor.CursorTextXY.Y, SrcEditor.CodeToolsBuffer as TCodeBuffer),
        CleanPos) <> 0
    then
      Exit;
    ProcNode := Tool.FindDeepestNodeAtPos(CleanPos{%H-},true);
    while (ProcNode <> nil) and (ProcNode.Desc <> ctnProcedure) do
      ProcNode := ProcNode.Parent;

    if (ProcNode <> nil) and (ProcNode.Desc = ctnProcedure) then
    begin
      if JumpType = jmpBegin then
        JumpFound := Tool.FindJumpPointInProcNode(ProcNode, NewPos, NewTopLine, BlockTopLine, BlockBottomLine)
      else
        JumpFound := Tool.JumpToCleanPos(ProcNode.StartPos, ProcNode.StartPos, ProcNode.EndPos, NewPos, NewTopLine, BlockTopLine, BlockBottomLine, True);
    end else
      JumpFound := False;

    if JumpFound then
      JumpToPos(NewPos.Code.Filename, NewPos, NewTopLine, BlockTopLine, BlockBottomLine)
    else
    begin
      CodeToolBoss.SetError(20170421203224, nil, 0, 0,
        Format(lisCannotFind, [cJumpNames[JumpType]]));
      LazarusIDE.DoJumpToCodeToolBossError;
    end;
  end
  else
    LazarusIDE.DoJumpToCodeToolBossError;
end;

procedure TSourceEditorManager.JumpToProcedureBeginClicked(Sender: TObject);
begin
  JumpToProcedure(jmpBegin);
end;

procedure TSourceEditorManager.JumpToProcedureHeaderClicked(Sender: TObject);
begin
  JumpToProcedure(jmpHeader);
end;

procedure TSourceEditorManager.JumpToSection(JumpType: TJumpToSectionType);
const
  cJumpNames: array[TJumpToSectionType] of string = (
    'Interface', 'Interface uses', 'Implementation', 'Implementation uses', 'Initialization');
var
  SrcEditor: TSourceEditorInterface;
  Node: TCodeTreeNode;
  Tool: TCodeTool;
  NewTopLine: Integer;
  NewCodePos: TCodeXYPosition;
begin
  if not LazarusIDE.BeginCodeTools then Exit; //==>

  SrcEditor := SourceEditorManagerIntf.ActiveEditor;
  if not Assigned(SrcEditor) then Exit; //==>

  if CodeToolBoss.Explore(SrcEditor.CodeToolsBuffer as TCodeBuffer, Tool, false, false) then
  begin
    case JumpType of
      jmpInterface: Node := Tool.FindInterfaceNode;
      jmpInterfaceUses:
      begin
        Node := Tool.FindMainUsesNode;
        if Node = nil then//if the uses section is missing, jump to interface
          Node := Tool.FindInterfaceNode;
      end;
      jmpImplementation: Node := Tool.FindImplementationNode;
      jmpImplementationUses:
      begin
        Node := Tool.FindImplementationUsesNode;
        if Node = nil then//if the uses section is missing, jump to implementation
          Node := Tool.FindImplementationNode;
      end;
      jmpInitialization:
      begin
        Node := Tool.FindInitializationNode;
        if Node = nil then//if initialization is missing, jump to last end
          Node := Tool.FindRootNode(ctnEndPoint);
      end;
    end;

    if (Node <> nil) and Tool.CleanPosToCaretAndTopLine(Node.StartPos, NewCodePos, NewTopLine) then
      JumpToPos(NewCodePos.Code.Filename, NewCodePos, NewTopLine)
    else
    begin
      CodeToolBoss.SetError(20170421203236, nil, 0, 0,
        Format(lisCannotFind, [cJumpNames[JumpType]]));
      LazarusIDE.DoJumpToCodeToolBossError;
    end;
  end
  else
    LazarusIDE.DoJumpToCodeToolBossError;
end;

procedure TSourceEditorManager.AddJumpPointClicked(Sender: TObject);
begin
  if Assigned(OnAddJumpPoint) and (ActiveEditor <> nil) then
    OnAddJumpPoint(ActiveEditor.EditorComponent.LogicalCaretXY,
      ActiveEditor.EditorComponent.TopLine, ActiveEditor, true);
end;

procedure TSourceEditorManager.DeleteLastJumpPointClicked(Sender: TObject);
begin
  if Assigned(OnDeleteLastJumpPoint) then
    OnDeleteLastJumpPoint(Sender);
end;

procedure TSourceEditorManager.ViewJumpHistoryClicked(Sender: TObject);
begin
  if Assigned(OnViewJumpHistory) then
    OnViewJumpHistory(Sender);
end;

procedure TSourceEditorManager.BookMarkToggleClicked(Sender: TObject);
begin
  if Assigned(OnSetBookmark) then
    OnSetBookmark(ActiveEditor, (Sender as TIDEMenuItem).SectionIndex, True);
end;

procedure TSourceEditorManager.BookMarkGotoClicked(Sender: TObject);
begin
  if Assigned(OnGotoBookmark) then
    OnGotoBookmark(ActiveEditor, (Sender as TIDEMenuItem).SectionIndex, False);
end;

procedure TSourceEditorManager.BookMarkNextClicked(Sender: TObject);
begin
  if Assigned(OnGotoBookmark) then
    OnGotoBookmark(ActiveEditor, -1, False);
end;

procedure TSourceEditorManager.BookMarkPrevClicked(Sender: TObject);
begin
  if Assigned(OnGotoBookmark) then
    OnGotoBookmark(ActiveEditor, -1, True);
end;

function TSourceEditorManager.MacroFuncCol(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  if (ActiveEditor <> nil) then
    Result:=IntToStr(ActiveEditor.EditorComponent.CaretX)
  else
    Result:='';
end;

function TSourceEditorManager.MacroFuncRow(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  if (ActiveEditor <> nil) then
    Result:=IntToStr(ActiveEditor.EditorComponent.CaretY)
  else
    Result:='';
end;

function TSourceEditorManager.MacroFuncEdFile(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if (ActiveEditor <> nil) then
    Result := ActiveEditor.FileName
  else
    Result := '';
end;

function TSourceEditorManager.MacroFuncCurToken(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  if (ActiveEditor <> nil) then begin
    with ActiveEditor.EditorComponent do
      Result := GetWordAtRowCol(LogicalCaretXY)
  end else
    Result := '';
end;

function TSourceEditorManager.MacroFuncConfirm(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  Result:=s;
  Abort:=(ShowMacroConfirmDialog(Result)<>mrOk);
end;

function TSourceEditorManager.MacroFuncPrompt(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=s;
  Abort:=(ShowMacroPromptDialog(Result)<>mrOk);
end;

function TSourceEditorManager.MacroFuncSave(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:='';
  if SourceEditorCount > 0 then
    Abort:=LazarusIDE.DoSaveEditorFile(ActiveEditor,[sfCheckAmbiguousFiles]) <> mrOk;
end;

function TSourceEditorManager.MacroFuncSaveAll(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:='';
  Abort:=LazarusIDE.DoSaveAll([sfCheckAmbiguousFiles])<>mrOk;
end;

procedure TSourceEditorManager.InitMacros(AMacroList: TTransferMacroList);
begin
  AMacroList.Add(TTransferMacro.Create('Col','',
                 lisCursorColumnInCurrentEditor,@MacroFuncCol,[]));
  AMacroList.Add(TTransferMacro.Create('Row','',
                 lisCursorRowInCUrrentEditor,@MacroFuncRow,[]));
  AMacroList.Add(TTransferMacro.Create('CurToken','',
                 lisWordAtCursorInCurrentEditor,@MacroFuncCurToken,[]));
  AMacroList.Add(TTransferMacro.Create('EdFile','',
                 lisExpandedFilenameOfCurrentEditor,@MacroFuncEdFile,[]));
  AMacroList.Add(TTransferMacro.Create('Confirm','',
                 lisConfirmation,@MacroFuncConfirm,[tmfInteractive]));
  AMacroList.Add(TTransferMacro.Create('Prompt','',
                 lisPromptForValue,@MacroFuncPrompt,[tmfInteractive]));
  AMacroList.Add(TTransferMacro.Create('Save','',
                 lisSaveCurrentEditorFile,@MacroFuncSave,[tmfInteractive]));
  AMacroList.Add(TTransferMacro.Create('SaveAll','',
                 lisSaveAllModified,@MacroFuncSaveAll,[tmfInteractive]));
end;

procedure TSourceEditorManager.SetupShortCuts;

  function GetCmdAndBtn(ACommand: word; out ToolButton: TIDEButtonCommand): TIDECommand;
  begin
    Result:=IDECommandList.FindIDECommand(ACommand);
    if Result<>nil then
      ToolButton := RegisterIDEButtonCommand(Result)
    else
      ToolButton := nil;
  end;

  function GetCommand(ACommand: word): TIDECommand;
  var
    ToolButton: TIDEButtonCommand;
  begin
    Result:=GetCmdAndBtn(ACommand, ToolButton);
  end;

  function GetCommand(ACommand: word; ToolButtonClass: TIDEToolButtonClass): TIDECommand;
  var
    ToolButton: TIDEButtonCommand;
  begin
    Result:=GetCmdAndBtn(ACommand, ToolButton);
    if ToolButton<>nil then
      ToolButton.ToolButtonClass := ToolButtonClass;
  end;

  // See also in ToolBarIntf:
  //  function GetCommand_DropDown
  //  function GetCommand_ButtonDrop

var
  i: Integer;
begin
  {%region *** first static section *** }
    SrcEditMenuFindDeclaration.Command := GetCommand(ecFindDeclaration);
    {%region *** Submenu: Find Section *** }
      SrcEditMenuProcedureJump.Command          := GetCommand(ecFindProcedureDefinition);
      SrcEditMenuProcedureJump.OnRequestCaptionHint := @SrcEditMenuProcedureJumpGetCaption;
      SrcEditMenuFindNextWordOccurrence.Command := GetCommand(ecFindNextWordOccurrence);
      SrcEditMenuFindPrevWordOccurrence.Command := GetCommand(ecFindPrevWordOccurrence);
      SrcEditMenuFindInFiles.Command            := GetCommand(ecFindInFiles);
      SrcEditMenuFindIdentifierReferences.Command:=GetCommand(ecFindIdentifierRefs);
      SrcEditMenuFindUsedUnitReferences.Command:=GetCommand(ecFindUsedUnitRefs);
    {%endregion}
  {%endregion}

  {%region *** Pages section ***}
    SrcEditMenuClosePage.Command       := GetCommand(ecClose);
    SrcEditMenuCloseOtherPages.OnClick := @SourceEditorManager.CloseOtherPagesClicked;
    SrcEditMenuCloseOtherPagesToRight.OnClick := @SourceEditorManager.CloseRightPagesClicked;

    {$IFnDEF SingleSrcWindow}
    SrcEditMenuEditorLock.Command           := GetCommand(ecLockEditor);
    SrcEditMenuMoveToNewWindow.SyncProperties := False;
    SrcEditMenuMoveToNewWindow.Command      := GetCommand(ecMoveEditorNewWindow);
    SrcEditMenuMoveToOtherWindowNew.SyncProperties := False;
    SrcEditMenuMoveToOtherWindowNew.Command := GetCommand(ecMoveEditorNewWindow);
    SrcEditMenuCopyToNewWindow.SyncProperties := False;
    SrcEditMenuCopyToNewWindow.Command      := GetCommand(ecCopyEditorNewWindow);
    SrcEditMenuCopyToOtherWindowNew.SyncProperties := False;
    SrcEditMenuCopyToOtherWindowNew.Command := GetCommand(ecCopyEditorNewWindow);
    {$ENDIF}
  {%endregion}

  {%region * Move Page (left/right) *}
    SrcEditMenuMoveEditorLeft.Command  := GetCommand(ecMoveEditorLeft);
    SrcEditMenuMoveEditorRight.Command := GetCommand(ecMoveEditorRight);
    SrcEditMenuMoveEditorFirst.Command := GetCommand(ecMoveEditorLeftmost);
    SrcEditMenuMoveEditorLast.Command  := GetCommand(ecMoveEditorRightmost);
  {%endregion}

  SrcEditMenuOpenFileAtCursor.Command := GetCommand(ecOpenFileAtCursor);

  {%region * sub menu Flags section *}
    SrcEditMenuReadOnly.OnClick          := @ReadOnlyClicked;
    SrcEditMenuShowLineNumbers.OnClick   := @ToggleLineNumbersClicked;
    SrcEditMenuDisableI18NForLFM.OnClick := @ToggleI18NForLFMClicked;
    SrcEditMenuShowUnitInfo.OnClick      := @ShowUnitInfo;
  {%endregion}

  {%region *** Clipboard section ***}
    SrcEditMenuCut.Command:=GetCommand(ecCut);
    SrcEditMenuCopy.Command:=GetCommand(ecCopy);
    SrcEditMenuPaste.Command:=GetCommand(ecPaste);
    SrcEditMenuMultiPaste.Command:=GetCommand(ecMultiPaste);
    SrcEditMenuCopyFilename.OnClick:=@CopyFilenameClicked;
    SrcEditMenuSelectAll.Command:=GetCommand(ecSelectAll);
  {%endregion}

  SrcEditMenuNextBookmark.Command:=GetCommand(ecNextBookmark);
  SrcEditMenuPrevBookmark.Command:=GetCommand(ecPrevBookmark);
  SrcEditMenuSetFreeBookmark.Command:=GetCommand(ecSetFreeBookmark);
  SrcEditMenuClearFileBookmark.Command:=GetCommand(ecClearBookmarkForFile);
  SrcEditMenuClearAllBookmark.Command:=GetCommand(ecClearAllBookmark);

  for i in TBookmarkNumRange do
    SrcEditMenuGotoBookmark[i].Command := GetCommand(ecGotoMarker0 + i);
  GetCommand_ButtonDrop(ecGotoBookmarks ,SrcEditSubMenuGotoBookmarks);        // [ ▼]

  for i in TBookmarkNumRange do
    SrcEditMenuToggleBookmark[i].Command := GetCommand(ecToggleMarker0 + i);
  GetCommand_ButtonDrop(ecToggleBookmarks ,SrcEditSubMenuToggleBookmarks);    // [ ▼]

  {%region *** Source Section ***}
    SrcEditMenuEncloseSelection.Command:=GetCommand(ecSelectionEnclose);
    SrcEditMenuEncloseInIFDEF.Command:=GetCommand(ecSelectionEncloseIFDEF);
    SrcEditMenuCompleteCode.Command:=GetCommand(ecCompleteCode);
    SrcEditMenuUseUnit.Command:=GetCommand(ecUseUnit);
  {%endregion}

  {%region *** Refactoring Section ***}
    SrcEditMenuRenameIdentifier.Command:=GetCommand(ecRenameIdentifier);
    SrcEditMenuExtractProc.Command:=GetCommand(ecExtractProc);
    SrcEditMenuInvertAssignment.Command:=GetCommand(ecInvertAssignment);
    SrcEditMenuShowAbstractMethods.Command:=GetCommand(ecShowAbstractMethods);
    SrcEditMenuShowEmptyMethods.Command:=GetCommand(ecRemoveEmptyMethods);
    SrcEditMenuShowUnusedUnits.Command:=GetCommand(ecRemoveUnusedUnits);
    SrcEditMenuFindOverloads.Command:=GetCommand(ecFindOverloads);
    SrcEditMenuMakeResourceString.Command:=GetCommand(ecMakeResourceString);
  {%endregion}

  SrcEditMenuEditorProperties.OnClick:=@EditorPropertiesClicked;

  DebugBoss.SetupSourceMenuShortCuts;
end;

function TSourceEditorManager.FindUniquePageName(FileName: string;
  IgnoreEditor: TSourceEditor): string;
var
  I:integer;
  ShortName:string;

  function PageNameExists(const AName:string):boolean;
  var a:integer;
  begin
    Result:=false;
    for a := 0 to SourceEditorCount - 1 do begin
      if (SourceEditors[a] <> IgnoreEditor) and
         (not SourceEditors[a].IsSharedWith(IgnoreEditor)) and
         (CompareText(AName, SourceEditors[a].PageName) = 0)
      then begin
        Result:=true;
        exit;
      end;
    end;
  end;

begin
  if FileName='' then begin
    FileName:='unit1';
    if not PageNameExists(FileName) then begin
      Result:=Filename;
      exit;
    end;
  end;
  if FilenameIsPascalUnit(FileName) then
    ShortName:=ExtractFileNameOnly(Filename)
  else
    ShortName:=ExtractFileName(FileName);
  Result:=ShortName;
  if PageNameExists(Result) then begin
    i:=1;
    repeat
      inc(i);
      Result:=ShortName+'('+IntToStr(i)+')';
    until PageNameExists(Result)=false;
  end;
end;

function TSourceEditorManager.SomethingModified(Verbose: boolean): boolean;
var
  i: integer;
begin
  for i:=0 to SourceEditorCount - 1 do
  begin
    if SourceEditors[i].Modified then
    begin
      if Verbose then
        debugln(['TSourceEditorManager.SomethingModified ',SourceEditors[i].FileName]);
      exit(true);
    end;
  end;
  Result:=false;
end;

procedure TSourceEditorManager.OnIdle(Sender: TObject; var Done: Boolean);
var
  SrcEdit: TSourceEditor;
  i, j: Integer;
  aFilename: String;
  FreeList, FreeMarklings: boolean;
  Marklings: TFPList;
  Markling: TSourceMarkling;
begin
  SrcEdit:=ActiveEditor;
  if (SrcEdit<>nil)
  and (not SrcEdit.FSharedValues.FMarklingsValid) then
  begin
    //debugln(['TSourceEditorManager.OnIdle ',MarklingProducerCount]);
    aFilename:=SrcEdit.FileName;
    SrcEdit.EditorComponent.BeginUpdate(False);
    for i:=0 to MarklingProducerCount-1 do
    begin
      Marklings:=MarklingProducers[i].GetMarklings(aFilename,FreeList,FreeMarklings);
      for j:=0 to Marklings.Count-1 do
      begin
        Markling:=TSourceMarkling(Marklings[j]);
        if Markling=nil then ;
        // ToDo: add mark to synedit
        //debugln(['TSourceEditorManager.OnIdle ',Markling.Id,' ',Markling.Line,',',Markling.Column]);

      end;
      if FreeMarklings then
        for j:=0 to Marklings.Count-1 do
          TObject(Marklings[j]).Free;
      if FreeList then
        Marklings.Free;
    end;
    SrcEdit.EditorComponent.EndUpdate;
    SrcEdit.FSharedValues.FMarklingsValid:=true;
  end;
end;

procedure TSourceEditorManager.OnUserInput(Sender: TObject; Msg: Cardinal);
begin
  CodeToolsToSrcEditTimer.Enabled:=true;
  // Hints
  if FHints.HintIsComplex then
  begin
    // TODO: introduce property, to indicate if hint is interactive
    if FHints.PtIsOnHint(Mouse.CursorPos) then begin // ignore any action over Hint
      if FHints.CurHintWindow.Active then
        exit;
      if (Msg = WM_MOUSEMOVE)
         or (Msg = LM_MOUSELEAVE)
         {$IFDEF WINDOWS}
         or (Msg = WM_NCMOUSEMOVE)
         or ((Msg >= WM_MOUSEFIRST) and (Msg <= WM_MOUSELAST))
         {$ENDIF}
      then
        exit;
    end;
    if (Msg = WM_MOUSEMOVE) {$IFDEF WINDOWS} or (Msg = WM_NCMOUSEMOVE){$ENDIF} then begin
      FHints.HideAutoHintAfterMouseMoved;
      exit;
    end;
  end;

  //debugln('TSourceEditorManager.OnUserInput');
  // don't hide hint if Sender is a hint window or child control
  if not FHints.SenderIsHintControl(Sender) then
    FHints.HideAutoHint;
end;

procedure TSourceEditorManager.LockAllEditorsInSourceChangeCache;
// lock all sourceeditors that are to be modified by the CodeToolBoss
var
  i: integer;
begin
  for i:=0 to SourceEditorCount - 1 do begin
    if CodeToolBoss.SourceChangeCache.BufferIsModified(SourceEditors[i].CodeBuffer)
    then
      SourceEditors[i].BeginGlobalUpdate;
  end;
end;

procedure TSourceEditorManager.UnlockAllEditorsInSourceChangeCache;
// unlock all sourceeditors that were modified by the CodeToolBoss
var
  i: integer;
begin
  for i:=0 to SourceEditorCount - 1 do begin
    if CodeToolBoss.SourceChangeCache.BufferIsModified(SourceEditors[i].CodeBuffer)
    then
      SourceEditors[i].EndGlobalUpdate;
  end;
end;

procedure TSourceEditorManager.BeginGlobalUpdate;
var
  i: integer;
begin
  for i:=0 to SourceEditorCount - 1 do
    SourceEditors[i].BeginGlobalUpdate;
end;

procedure TSourceEditorManager.EndGlobalUpdate;
var
  i: integer;
begin
  for i:=0 to SourceEditorCount - 1 do
    SourceEditors[i].EndGlobalUpdate;
end;

procedure TSourceEditorManager.CloseFile(AEditor: TSourceEditorInterface);
var
  i, j: Integer;
begin
  i := SourceWindowCount - 1;
  while i >= 0 do begin
    j := SourceWindows[i].FindPageWithEditor(TSourceEditor(AEditor));
    if j >= 0 then begin
      SourceWindows[i].CloseFile(j);
      break;
    end;
    dec(i);
  end;
end;

procedure TSourceEditorManager.HistoryJump(Sender: TObject;
  JumpAction: TJumpHistoryAction);
var
  NewCaretXY: TPoint;
  NewTopLine: integer;
  NewEditor: TSourceEditor;
begin
  if Assigned(OnJumpToHistoryPoint) then begin
    NewCaretXY.X:=-1;
    NewEditor:=nil;
    OnJumpToHistoryPoint(NewCaretXY,NewTopLine,NewEditor,JumpAction);
    if NewEditor<>nil then begin
      ActiveEditor := NewEditor;
      ShowActiveWindowOnTop(True);
      with NewEditor.EditorComponent do begin
        if not NewEditor.IsLocked then
          TopLine:=NewTopLine;
        LogicalCaretXY:=NewCaretXY;
      end;
    end;
  end;
end;

procedure TSourceEditorManager.ActivateHint(const ScreenPos: TPoint;
  const BaseURL, TheHint: string; AutoShown: Boolean; AMouseOffset: Boolean);
begin
  if csDestroying in ComponentState then exit;

  FHints.ActivateHint(ScreenPos, BaseURL, TheHint, AutoShown, AMouseOffset);
end;

procedure TSourceEditorManager.ActivateHint(const ScreenRect: TRect;
  const BaseURL, TheHint: string; AutoShown: Boolean; AMouseOffset: Boolean);
begin
  if csDestroying in ComponentState then exit;

  FHints.ActivateHint(ScreenRect, BaseURL, TheHint, AutoShown, AMouseOffset);
end;

procedure TSourceEditorManager.OnCodeTemplateTokenNotFound(Sender: TObject;
  AToken: string; AnEditor: TCustomSynEdit; var Index: integer);
begin
  if Index=0 then ;
  //debugln('TSourceNotebook.OnCodeTemplateTokenNotFound ',AToken,',',AnEditor.ReadOnly,',',DefaultCompletionForm.CurrentCompletionType=ctNone);
  if (AnEditor.ReadOnly=false) and
     (DefaultCompletionForm.CurrentCompletionType=ctNone)
  then begin
    DefaultCompletionForm.CurrentCompletionType:=ctTemplateCompletion;
    DefaultCompletionForm.Editor:=AnEditor;
    DefaultCompletionForm.Execute
      (AToken, GetScreenRectForToken(AnEditor, AnEditor.CaretX-length(AToken),
       AnEditor.CaretY, AnEditor.CaretX-1));
  end;
end;

procedure TSourceEditorManager.OnCodeTemplateExecuteCompletion(
  ASynAutoComplete: TCustomSynAutoComplete; Index: integer);
var
  SrcEdit: TSourceEditorInterface;
  TemplateName: string;
  TemplateValue: string;
  TemplateComment: string;
  TemplateAttr: TStrings;
begin
  SrcEdit:=FindSourceEditorWithEditorComponent(ASynAutoComplete.Editor);
  if SrcEdit=nil then
    SrcEdit := ActiveEditor;
  //debugln('TSourceNotebook.OnCodeTemplateExecuteCompletion A ',dbgsName(SrcEdit),' ',dbgsName(ASynAutoComplete.Editor));

  TemplateName:=ASynAutoComplete.Completions[Index];
  TemplateValue:=ASynAutoComplete.CompletionValues[Index];
  TemplateComment:=ASynAutoComplete.CompletionComments[Index];
  TemplateAttr:=ASynAutoComplete.CompletionAttributes[Index];
  ExecuteCodeTemplate(SrcEdit,TemplateName,TemplateValue,TemplateComment,
                      ASynAutoComplete.EndOfTokenChr,TemplateAttr,
                      ASynAutoComplete.IndentToTokenStart);
end;

procedure TSourceEditorManager.CodeToolsToSrcEditTimerTimer(Sender: TObject);
var
  i: Integer;
  SrcEdit: TSourceEditor;
begin
  CodeToolsToSrcEditTimer.Enabled:=false;

  for i:=0 to SourceEditorCount-1 do begin
    SrcEdit:=SourceEditors[i];
    if not SrcEdit.EditorComponent.IsVisible then continue;
    SrcEdit.UpdateIfDefNodeStates;
  end;
end;

procedure TSourceEditorManager.OnSourceCompletionTimer(Sender: TObject);

  function CheckStartIdentCompletion: boolean;
  var
    Line: String;
    LogCaret: TPoint;
    p: Integer;
    InStringConstant: Boolean;
    SrcEdit: TSourceEditor;
    Token: string;
    Attri: TSynHighlighterAttributes;
  begin
    Result := false;
    SrcEdit := ActiveEditor;
    if SrcEdit = nil then exit;
    if not (SrcEdit.FEditor.Highlighter is TSynPasSyn) then
      exit; // only start completion automatically for pascal sources

    Line := SrcEdit.FEditor.LineText;
    LogCaret := SrcEdit.FEditor.LogicalCaretXY;
    //DebugLn(['CheckStartIdentCompletion Line="',Line,'" LogCaret=',dbgs(LogCaret)]);

    // check if last character is a point
    if (Line='') or (LogCaret.X<=1) or (LogCaret.X-1>length(Line))
    or (Line[LogCaret.X-1]<>'.') then
      exit;

    // check if range operator '..'
    if (LogCaret.X>2) and (Line[LogCaret.X-2]='.') then
      exit; // this is a double point ..

    // check if in a string constant
    p:=1;
    InStringConstant:=false;
    while (p<LogCaret.X) and (p<=length(Line)) do begin
      if Line[p]='''' then
        InStringConstant:=not InStringConstant;
      inc(p);
    end;
    if InStringConstant then exit;

    // check if in a comment
    Token:='';
    Attri:=nil;
    dec(LogCaret.X);
    if SrcEdit.EditorComponent.GetHighlighterAttriAtRowCol(LogCaret,Token,Attri)
    and (Attri<>nil) and (Attri.StoredName=SYNS_XML_AttrComment) then
    begin
      exit;
    end;

    // invoke identifier completion
    SrcEdit.StartIdentCompletionBox(false,false);
    Result:=true;
  end;

  function CheckTemplateCompletion: boolean;
  begin
    Result:=false;
    // execute context sensitive templates
    //FCodeTemplateModul.ExecuteCompletion(Value,GetActiveSE.EditorComponent);
  end;

var
  TempEditor: TSourceEditor;
begin
  AutoStartCompletionBoxTimer.Enabled:=false;
  AutoStartCompletionBoxTimer.AutoEnabled:=false;
  TempEditor := ActiveEditor;
  if (TempEditor <> nil) and TempEditor.EditorComponent.Focused and
     (ComparePoints(TempEditor.EditorComponent.CaretXY, SourceCompletionCaretXY) = 0)
  then begin
    if CheckStartIdentCompletion then begin
    end
    else if CheckTemplateCompletion then begin
    end;
  end;
end;

procedure TSourceEditorManager.OnSourceMarksAction(AMark: TSourceMark;
  AAction: TMarksAction);
var
  Editor: TSourceEditor;
begin
  Editor := TSourceEditor(AMark.SourceEditor);
  if Editor = nil then
    Exit;

  if ( AMark.IsBreakPoint and (Editor.FSharedValues.ExecutionMark <> nil) and
       (AMark.Line = Editor.ExecutionLine)
     ) or (AMark = Editor.FSharedValues.ExecutionMark)
  then
    Editor.UpdateExecutionSourceMark;
end;

procedure TSourceEditorManager.OnSourceMarksGetSynEdit(Sender: TObject;
  aFilename: string; var aSynEdit: TSynEdit);
var
  SrcEdit: TSourceEditor;
begin
  SrcEdit:=SourceEditorIntfWithFilename(aFilename);
  if SrcEdit=nil then exit;
  aSynEdit:=SrcEdit.EditorComponent;
end;

function TSourceEditorManager.GotoDialog: TfrmGoto;
begin
  if FGotoDialog=nil then
    FGotoDialog := TfrmGoto.Create(self);
  Result := FGotoDialog;
end;

procedure TSourceEditorManager.DoConfigureEditorToolbar(Sender: TObject);
begin
  LazarusIDE.DoOpenIDEOptions(TEditorToolbarOptionsFrame, '', [], []);
end;

constructor TSourceEditorManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDefaultCompletionForm := nil;

  // word completion
  if aWordCompletion=nil then begin
    AWordCompletion:=TSourceEditorWordCompletion.Create;
    AWordCompletion.WordBufferCapacity:=100;
  end;

  // timer for auto start identifier completion
  AutoStartCompletionBoxTimer := TIdleTimer.Create(Self);
  with AutoStartCompletionBoxTimer do begin
    Name:='AutoStartCompletionBoxTimer';
    AutoEnabled := False;
    Enabled := false;
    Interval := EditorOpts.AutoDelayInMSec;
    OnTimer := @OnSourceCompletionTimer;
  end;

  // timer for syncing codetools changes to synedit
  // started on idle
  // ended on user input
  // when triggered updates ifdef node states
  CodeToolsToSrcEditTimer:=TTimer.Create(Self);
  with CodeToolsToSrcEditTimer do begin
    Name:='CodeToolsToSrcEditTimer';
    Interval:=1000; // one second without user input
    Enabled:=false;
    OnTimer:=@CodeToolsToSrcEditTimerTimer;
  end;

  // marks
  SourceEditorMarks:=TSourceMarks.Create(Self);
  SourceEditorMarks.OnAction:=@OnSourceMarksAction;
  SourceEditorMarks.ExtToolsMarks.OnGetSynEditOfFile:=@OnSourceMarksGetSynEdit;

  // HintWindow
  FHints := TSourceEditorHintWindowManager.Create(Self);
  FHints.WindowName := Self.Name+'_HintWindow';
  FHints.HideInterval := 4000;

  // code templates
  FCodeTemplateModul:=TSynEditAutoComplete.Create(Self);

  EditorOpts.LoadCodeTemplates(FCodeTemplateModul);
  with FCodeTemplateModul do begin
    IndentToTokenStart := EditorOpts.CodeTemplateIndentToTokenStart;
    OnTokenNotFound := @OnCodeTemplateTokenNotFound;
    OnExecuteCompletion := @OnCodeTemplateExecuteCompletion;
    EndOfTokenChr:=' ()[]{},.;:"+-*^@$\<>=''';
  end;

  // EditorToolBar
  CreateEditorToolBar(@DoConfigureEditorToolbar);

  // layout
  IDEWindowCreators.Add(NonModalIDEWindowNames[nmiwSourceNoteBook],
    nil,@CreateSourceWindow,'250','100','+70%','+70%',
    NonModalIDEWindowNames[nmiwMainIDE],alBottom,true,@GetDefaultLayout);

  Application.AddOnIdleHandler(@OnIdle);
  Application.AddOnUserInputHandler(@OnUserInput);
end;

destructor TSourceEditorManager.Destroy;
begin
  FreeAndNil(FHints);
  SourceEditorMarks.OnAction := nil;
  Application.RemoveAllHandlersOfObject(Self);
  // aWordCompletion is released in InternalFinal
  aWordCompletion.OnGetSource := nil;

  inherited Destroy;
end;

function SortSourceWindows(SrcWin1, SrcWin2: TSourceNotebook): Integer;
begin
  Result := AnsiStrComp(PChar(SrcWin1.Caption), PChar(SrcWin2.Caption));
end;

function TSourceEditorManager.CreateNewWindow(Activate: Boolean; DoDisableAutoSizing: boolean;
  AnID: Integer): TSourceNotebook;
var
  i: Integer;
begin
  Result := TSourceNotebook(TSourceNotebook.NewInstance);
  {$IFDEF DebugDisableAutoSizing}
  if DoDisableAutoSizing then
    Result.DisableAutoSizing('TAnchorDockMaster Delayed')
  else
    Result.DisableAutoSizing('TSourceEditorManager.CreateNewWindow');
  {$ELSE}
  Result.DisableAutoSizing;
  {$ENDIF};
  if AnID > 0 then
    Result.Create(Self, AnID)
  else
    Result.Create(Self);

  for i := 1 to FUpdateLock do
    Result.IncUpdateLockInternal;
  FSourceWindowList.Add(Result);
  FSourceWindowList.Sort(TListSortCompare(@SortSourceWindows));
  FSourceWindowByFocusList.Add(Result);
  PasBeautifier.OnGetDesiredIndent :=
    @TSourceNotebook(FSourceWindowList[0]).EditorGetIndent;
  if Activate then begin
    ActiveSourceWindow := Result;
    ShowActiveWindowOnTop(False);
  end;
  FChangeNotifyLists[semWindowCreate].CallNotifyEvents(Result);
  if not DoDisableAutoSizing then
    Result.EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TSourceEditorManager.CreateNewWindow'){$ENDIF};
end;

function TSourceEditorManager.SenderToEditor(Sender: TObject): TSourceEditor;
begin
  if Sender is TSourceEditor then
    Result:=TSourceEditor(Sender)
  else if Sender is TSourceNotebook then
    Result:=TSourceNotebook(Sender).ActiveEditor as TSourceEditor
  else
    Result:=ActiveEditor;
end;

procedure TSourceEditorManager.RemoveWindow(AWindow: TSourceNotebook);
var
  i: Integer;
begin
  if FSourceWindowList = nil then exit;
  i := FSourceWindowList.IndexOf(AWindow);
  FSourceWindowList.Remove(AWindow);
  FSourceWindowByFocusList.Remove(AWindow);
  if SourceWindowCount = 0 then
    ActiveSourceWindow := nil
  else if ActiveSourceWindow = AWindow then
    ActiveSourceWindow := SourceWindows[Max(0, Min(i, SourceWindowCount-1))];
  if FSourceWindowList.Count > 0 then
    PasBeautifier.OnGetDesiredIndent :=
      @TSourceNotebook(FSourceWindowList[0]).EditorGetIndent
  else
    PasBeautifier.OnGetDesiredIndent := nil;
  if i >= 0 then
    FChangeNotifyLists[semWindowDestroy].CallNotifyEvents(AWindow);
end;

(* Context Menu handlers *)

procedure TSourceEditorManager.CloseOtherPagesClicked(Sender: TObject);
begin
  if Assigned(OnCloseClicked) then
    OnCloseClicked(Sender, [ceoCloseOthers]);
end;

procedure TSourceEditorManager.CloseOtherPagesClickedAsync(Sender: PtrInt);
begin
  CloseOtherPagesClicked(TObject(Sender));
end;

procedure TSourceEditorManager.CloseRightPagesClicked(Sender: TObject);
begin
  if Assigned(OnCloseClicked) then
    OnCloseClicked(Sender, [ceoCloseOthersOnRightSide]);
end;

procedure TSourceEditorManager.CloseRightPagesClickedAsync(Sender: PtrInt);
begin
  CloseRightPagesClicked(TObject(Sender));
end;

procedure TSourceEditorManager.ReadOnlyClicked(Sender: TObject);
var ActEdit: TSourceEditor;
begin
  ActEdit:=ActiveEditor;
  if ActEdit = nil then exit;

  if ActEdit.ReadOnly and (ActEdit.CodeBuffer<>nil)
  and (not ActEdit.CodeBuffer.IsVirtual)
  and (not FileIsWritable(ActEdit.CodeBuffer.Filename)) then begin
    IDEMessageDialog(ueFileROCap,
      ueFileROText1+ActEdit.CodeBuffer.Filename+ueFileROText2,
      mtError,[mbCancel]);
    exit;
  end;
  ActEdit.EditorComponent.ReadOnly := not(ActEdit.EditorComponent.ReadOnly);
  if Assigned(OnReadOnlyChanged) then
    OnReadOnlyChanged(Self);
  ActEdit.SourceNotebook.UpdateStatusBar;
end;

procedure TSourceEditorManager.ToggleLineNumbersClicked(Sender: TObject);
var
  MenuITem: TIDESpecialCommand;
  ActEdit:TSourceEditor;
  i: integer;
  ShowLineNumbers: boolean;
begin
  MenuItem := Sender as TIDESpecialCommand;
  ActEdit:=ActiveEditor;
  if ActEdit = nil then exit;

  MenuItem.Checked := not EditorOpts.ShowLineNumbers;
  ShowLineNumbers:=MenuItem.Checked;

  for i:=0 to SourceEditorCount-1 do
    SourceEditors[i].EditorComponent.Gutter.LineNumberPart.Visible := ShowLineNumbers;
  EditorOpts.ShowLineNumbers := ShowLineNumbers;
  EditorOpts.Save;
end;

procedure TSourceEditorManager.ToggleI18NForLFMClicked(Sender: TObject);
begin
  //
end;

procedure TSourceEditorManager.ShowUnitInfo(Sender: TObject);
begin
  if Assigned(OnShowUnitInfo) then
    OnShowUnitInfo(Sender);
end;

procedure TSourceEditorManager.CopyFilenameClicked(Sender: TObject);
var ActSE: TSourceEditor;
begin
  ActSE := GetActiveSE;
  if ActSE <> nil then
    Clipboard.AsText:=ActSE.FileName;
end;

procedure TSourceEditorManager.EditorPropertiesClicked(Sender: TObject);
begin
  LazarusIDE.DoOpenIDEOptions(TEditorGeneralOptionsFrame);
end;

initialization
  InternalInit;

finalization
  InternalFinal;

end.

