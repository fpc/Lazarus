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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
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
  Classes, SysUtils, Math, Controls, ExtendedNotebook, LCLProc, LCLType, LResources,
  LCLIntf, FileUtil, Forms, ComCtrls, Dialogs, StdCtrls, Graphics, Translations,
  ClipBrd, types, Extctrls, Menus, HelpIntfs, LConvEncoding,
  // codetools
  BasicCodeTools, CodeBeautifier, CodeToolManager, CodeCache, SourceLog,
  // synedit
  SynEditLines, SynEditStrConst, SynEditTypes, SynEdit, SynRegExpr,
  SynEditHighlighter, SynEditAutoComplete, SynEditKeyCmds, SynCompletion,
  SynEditMiscClasses, SynEditMarkupHighAll, SynEditMarks,
  SynBeautifier, SynEditTextBase, LazSynEditText,
  SynPluginSyncronizedEditBase, SourceSynEditor,
  // Intf
  SrcEditorIntf, MenuIntf, LazIDEIntf, PackageIntf, IDEHelpIntf, IDEImagesIntf,
  IDEWindowIntf, ProjectIntf,
  // IDE units
  IDEDialogs, LazarusIDEStrConsts, IDECommands, EditorOptions, EnvironmentOpts,
  WordCompletion, FindReplaceDialog, IDEProcs, IDEOptionDefs,
  MacroPromptDlg, TransferMacros, CodeContextForm, SrcEditHintFrm, MsgView,
  InputHistory, CodeMacroPrompt, CodeTemplatesDlg, CodeToolsOptions,
  SortSelectionDlg, EncloseSelectionDlg, ConDef, InvertAssignTool,
  SourceEditProcs, SourceMarks, CharacterMapDlg, SearchFrm,
  FPDocHints,
  BaseDebugManager, Debugger, MainIntf, GotoFrm;

type
  TSourceNotebook = class;
  TSourceEditorManager = class;

  TNotifyFileEvent = procedure(Sender: TObject; Filename : AnsiString) of object;

  TOnProcessUserCommand = procedure(Sender: TObject;
            Command: word; var Handled: boolean) of object;
  TOnUserCommandProcessed = procedure(Sender: TObject;
            Command: word; var Handled: boolean) of object;

  TOnLinesInsertedDeleted = procedure(Sender : TObject;
             FirstLine,Count : Integer) of Object;
  TPlaceBookMarkEvent = procedure(Sender: TObject; var Mark: TSynEditMark) of object;
  TBookMarkActionEvent = procedure(Sender: TObject; ID: Integer; Toggle: Boolean) of object;

  TCharSet = set of Char;

  // for TSourcEditor.CenterCursorHoriz
  TSourceEditHCenterMode =
  ( hcmCenter,          // Center X-Caret to exact middle of Screen
    hcmCenterKeepEOL,   // Center X-Caret to middle of Screen, but keep EOL at right border
    hcmSoft,            // Soft Center (distance to screen edge) Caret
    hcmSoftKeepEOL      // Soft Center (distance to screen edge) Caret, but keep EOL at right border
  );


  { TSynEditPlugin1 }

  TSynEditPlugin1 = class(TSynEditPlugin)
  private
    FEnabled: Boolean;
    FOnLinesInserted : TOnLinesInsertedDeleted;
    FOnLinesDeleted : TOnLinesInsertedDeleted;
  protected
    Procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    function OwnedByEditor: Boolean; override;
  public
    property OnLinesInserted : TOnLinesInsertedDeleted
      read FOnLinesinserted write FOnLinesInserted;
    property OnLinesDeleted : TOnLinesInsertedDeleted
      read FOnLinesDeleted write FOnLinesDeleted;
    property Enabled: Boolean read FEnabled write FEnabled;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TSourceEditCompletion }

  TSourceEditCompletion=class(TSynCompletion)
    procedure CompletionFormResized(Sender: TObject);
  private
    FIdentCompletionJumpToError: boolean;
    ccSelection: String;
    // colors for the completion form (popup form, e.g. word completion)
    FActiveEditDefaultFGColor: TColor;
    FActiveEditDefaultBGColor: TColor;
    FActiveEditSelectedFGColor: TColor;
    FActiveEditSelectedBGColor: TColor;

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
  protected
    CurrentCompletionType: TCompletionType;
    function Manager: TSourceEditorManager;
  public
    constructor Create(AOwner: TComponent); override;
    property IdentCompletionJumpToError: Boolean
      read FIdentCompletionJumpToError write FIdentCompletionJumpToError;
  end;

  TSourceEditor = class;

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
    function GetExecutionLine: Integer;
  public
    UpdatingExecutionMark: Integer;
    procedure CreateExecutionMark;
    property ExecutionLine: Integer read GetExecutionLine;// write FExecutionLine;
    property ExecutionMark: TSourceMark read FExecutionMark write FExecutionMark;
    procedure SetExecutionLine(NewLine: integer);
    property MarksRequested: Boolean read FMarksRequested write FMarksRequested;
  private
    FInGlobalUpdate: Integer;
    FModified: boolean;
    FIgnoreCodeBufferLock: integer;
    FEditorStampCommitedToCodetools: int64;
    FCodeBuffer: TCodeBuffer;
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
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TSourceEditor ---
  TSourceEditor is the class that controls access for a single source editor,
  which is part of TSourceNotebook. }

  TSourceEditor = class(TSourceEditorBase)
  private
    //FAOwner is normally a TSourceNotebook.  This is set in the Create constructor.
    FAOwner: TComponent;
    FIsLocked: Boolean;
    FIsNewSharedEditor: Boolean;
    FSharedValues: TSourceEditorSharedValues;
    FEditor: TIDESynEditor;
    FTempCaret: TPoint;
    FTempTopLine: Integer;
    FEditPlugin: TSynEditPlugin1;  // used to get the LinesInserted and
                                   //   LinesDeleted messages
    FSyncroLockCount: Integer;
    FPageName: string;

    FPopUpMenu: TPopupMenu;
    FMouseActionPopUpMenu: TPopupMenu;
    FSyntaxHighlighterType: TLazSyntaxHighlighter;
    FErrorLine: integer;
    FErrorColumn: integer;
    FLineInfoNotification: TIDELineInfoNotification;
    FInEditorChangedUpdating: Boolean;

    FOnEditorChange: TNotifyEvent;
    FVisible: Boolean;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseWheel : TMouseWheelEvent;
    FOnKeyDown: TKeyEvent;

    FSourceNoteBook: TSourceNotebook;

    procedure EditorMouseMoved(Sender: TObject; Shift: TShiftState; X,Y:Integer);
    procedure EditorMouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X,Y: Integer);
    procedure EditorMouseWheel(Sender: TObject; Shift: TShiftState;
         WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
    procedure EditorPaste(Sender: TObject; var AText: String;
         var AMode: TSynSelectionMode; ALogStartPos: TPoint;
         var AnAction: TSynCopyPasteAction);
    procedure EditorPlaceBookmark(Sender: TObject; var Mark: TSynEditMark);
    procedure EditorClearBookmark(Sender: TObject; var Mark: TSynEditMark);
    procedure EditorEnter(Sender: TObject);
    procedure EditorActivateSyncro(Sender: TObject);
    procedure EditorDeactivateSyncro(Sender: TObject);
    procedure EditorChangeUpdating(ASender: TObject; AnUpdating: Boolean);
    function  EditorHandleMouseAction(AnAction: TSynEditMouseAction;
                                      var AnInfo: TSynEditMouseActionInfo): Boolean;
    function GetCodeBuffer: TCodeBuffer;
    function GetExecutionLine: integer;
    function GetHasExecutionMarks: Boolean;
    function GetSharedEditors(Index: Integer): TSourceEditor;
    procedure SetCodeBuffer(NewCodeBuffer: TCodeBuffer);
    function GetSource: TStrings;
    procedure SetIsLocked(const AValue: Boolean);
    procedure SetPageName(const AValue: string);
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
  protected
    ErrorMsgs: TStrings;

    procedure ProcessCommand(Sender: TObject;
       var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure ProcessUserCommand(Sender: TObject;
       var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure UserCommandProcessed(Sender: TObject;
       var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure ccAddMessage(Texts: String);
    function AutoCompleteChar(Char: TUTF8Char; var AddChar: boolean;
       Category: TAutoCompleteOption): boolean;
    function AutoBlockCompleteChar(Char: TUTF8Char; var AddChar: boolean;
       Category: TAutoCompleteOption; aTextPos: TPoint; Line: string): boolean;
    function AutoBlockCompleteChar(Char: TUTF8Char): boolean;
    procedure AutoCompleteBlock;

    procedure FocusEditor;// called by TSourceNotebook when the Notebook page
                          // changes so the editor is focused
    procedure OnGutterClick(Sender: TObject; X, Y, Line: integer;
         mark: TSynEditMark);
    procedure OnEditorSpecialLineColor(Sender: TObject; Line: integer;
         var Special: boolean; Markup: TSynSelectedColor);
    function RefreshEditorSettings: Boolean;
    function GetModified: Boolean; override;
    procedure SetModified(const NewValue: Boolean); override;
    procedure SetSyntaxHighlighterType(
                                 ASyntaxHighlighterType: TLazSyntaxHighlighter);
    procedure SetErrorLine(NewLine: integer);
    procedure SetExecutionLine(NewLine: integer);
    procedure StartIdentCompletionBox(JumpToError: boolean);
    procedure StartWordCompletionBox(JumpToError: boolean);

    procedure LinesInserted(sender: TObject; FirstLine, Count: Integer);
    procedure LinesDeleted(sender: TObject; FirstLine, Count: Integer);

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
  public
    constructor Create(AOwner: TComponent; AParent: TWinControl; ASharedEditor: TSourceEditor = nil);
    destructor Destroy; override;
    function Close: Boolean;

    // codebuffer
    procedure BeginUndoBlock; override;
    procedure EndUndoBlock; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure BeginGlobalUpdate;
    procedure EndGlobalUpdate;
    procedure IncreaseIgnoreCodeBufferLock; override;
    procedure DecreaseIgnoreCodeBufferLock; override;
    procedure UpdateCodeBuffer; override;// copy the source from EditorComponent
    function NeedsUpdateCodeBuffer: boolean; override;

    // find
    procedure StartFindAndReplace(Replace:boolean);
    procedure AskReplace(Sender: TObject; const ASearch, AReplace:
       string; Line, Column: integer; var Action: TSrcEditReplaceAction); override;
    procedure OnReplace(Sender: TObject; const ASearch, AReplace:
       string; Line, Column: integer; var Action: TSynReplaceAction);
    function  DoFindAndReplace(aFindText, aReplaceText: String; anOptions: TSynSearchOptions): Integer;
    procedure FindNextUTF8;
    procedure FindPrevious;
    procedure FindNextWordOccurrence(DirectionForward: boolean);
    procedure ShowGotoLineDialog;

    // dialogs
    procedure GetDialogPosition(Width, Height: integer; out Left, Top: integer);
    procedure ActivateHint(ClientPos: TPoint; const BaseURL, TheHint: string);

    // selections
    function SelectionAvailable: boolean; override;
    function GetText(OnlySelection: boolean): string; override;
    procedure SelectText(const StartPos, EndPos: TPoint); override;
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
    procedure InsertGPLNotice(CommentType: TCommentType);
    procedure InsertLGPLNotice(CommentType: TCommentType);
    procedure InsertModifiedLGPLNotice(CommentType: TCommentType);
    procedure InsertUsername;
    procedure InsertDateTime;
    procedure InsertChangeLogEntry;
    procedure InsertCVSKeyword(const AKeyWord: string);
    function GetSelEnd: Integer; override;
    function GetSelStart: Integer; override;
    procedure SetSelEnd(const AValue: Integer); override;
    procedure SetSelStart(const AValue: Integer); override;
    function GetSelection: string; override;
    procedure SetSelection(const AValue: string); override;
    procedure CopyToClipboard; override;
    procedure CutToClipboard; override;

    // context help
    procedure FindHelpForSourceAtCursor;

    // editor commands
    procedure DoEditorExecuteCommand(EditorCommand: word);

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
    function GetTopLine: Integer; override;
    procedure SetTopLine(const AValue: Integer); override;
    function CursorInPixel: TPoint; override;
    function IsCaretOnScreen(ACaret: TPoint; UseSoftCenter: Boolean = False): Boolean;

    // text
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
    procedure UpdateProjectFile; override;
    function GetDesigner(LoadForm: boolean): TIDesigner; override;

    // notebook
    procedure Activate;
    function PageIndex: integer;
    function IsActiveOnNoteBook: boolean;

    // debugging
    procedure FillExecutionMarks;
    procedure ClearExecutionMarks;
    procedure LineInfoNotificationChange(const ASender: TObject; const ASource: String);
    function  SourceToDebugLine(aLinePos: Integer): Integer;
    function  DebugToSourceLine(aLinePos: Integer): Integer;
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
    property OnEditorChange: TNotifyEvent read FOnEditorChange
                                          write FOnEditorChange;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseWheel: TMouseWheelEvent read FOnMouseWheel write FOnMouseWheel;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property Owner: TComponent read FAOwner;
    property PageName: string read FPageName write SetPageName;
    property PopupMenu: TPopupMenu read FPopUpMenu write SetPopUpMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Source: TStrings read GetSource write SetSource;
    property SourceNotebook: TSourceNotebook read FSourceNoteBook;
    property SyntaxHighlighterType: TLazSyntaxHighlighter
       read fSyntaxHighlighterType write SetSyntaxHighlighterType;
    property SyncroLockCount: Integer read FSyncroLockCount;
    function SharedEditorCount: Integer;
    property SharedEditors[Index: Integer]: TSourceEditor read GetSharedEditors;
    property IsNewSharedEditor: Boolean read FIsNewSharedEditor write FIsNewSharedEditor;
    property IsLocked: Boolean read FIsLocked write SetIsLocked;
  end;

  //============================================================================

  { TSourceNotebook }

  TJumpHistoryAction = (jhaBack, jhaForward, jhaViewWindow);

  TOnJumpToHistoryPoint = procedure(var NewCaretXY: TPoint;
                                    var NewTopLine: integer;
                                    var DestEditor: TSourceEditor;
                                    Action: TJumpHistoryAction) of object;
  TOnAddJumpPoint = procedure(ACaretXY: TPoint; ATopLine: integer;
                  AEditor: TSourceEditor; DeleteForwardHistory: boolean) of object;
  TOnMovingPage = procedure(Sender: TObject;
                            OldPageIndex, NewPageIndex: integer) of object;
  TOnCloseSrcEditor = procedure(Sender: TObject; InvertedClose: boolean) of object;
  TOnShowHintForSource = procedure(SrcEdit: TSourceEditor; ClientPos: TPoint;
                                   CaretPos: TPoint) of object;
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
    snNotbookPageChangedNeeded
    );
  TSourceNotebookStates = set of TSourceNotebookState;

  { TSourceNotebook }

  TSourceNotebook = class(TSourceEditorWindowInterface)
    StatusBar: TStatusBar;
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StatusBarDblClick(Sender: TObject);
  private
    FNotebook: TExtendedNotebook;
    FBaseCaption: String;
    FIsClosing: Boolean;
    SrcPopUpMenu, DbgPopUpMenu: TPopupMenu;
  protected
    procedure CompleteCodeMenuItemClick(Sender: TObject);
    procedure ExtractProcMenuItemClick(Sender: TObject);
    procedure InvertAssignmentMenuItemClick(Sender: TObject);
    procedure FindIdentifierReferencesMenuItemClick(Sender: TObject);
    procedure RenameIdentifierMenuItemClick(Sender: TObject);
    procedure ShowAbstractMethodsMenuItemClick(Sender: TObject);
    procedure ShowEmptyMethodsMenuItemClick(Sender: TObject);
    procedure ShowUnusedUnitsMenuItemClick(Sender: TObject);
    procedure FindOverloadsMenuItemClick(Sender: TObject);
    procedure LineEndingClicked(Sender: TObject);
    procedure EncodingClicked(Sender: TObject);
    procedure HighlighterClicked(Sender: TObject);
    procedure NotebookPageChanged(Sender: TObject);
    procedure NotebookShowTabHint(Sender: TObject; HintInfo: PHintInfo);
    procedure OpenAtCursorClicked(Sender: TObject);
    procedure OnPopupMenuOpenFile(Sender: TObject);
    procedure SrcPopUpMenuPopup(Sender: TObject);
    procedure DbgPopUpMenuPopup(Sender: TObject);
    procedure InsertCharacter(const C: TUTF8Char);
    procedure SrcEditMenuCopyToExistingWindowClicked(Sender: TObject);
    procedure SrcEditMenuMoveToExistingWindowClicked(Sender: TObject);
    procedure SrcEditMenuFindInWindowClicked(Sender: TObject);
    procedure EditorLockClicked(Sender: TObject);
  public
    procedure DeleteBreakpointClicked(Sender: TObject);
    procedure ToggleBreakpointClicked(Sender: TObject);
  private
    FManager: TSourceEditorManager;
    FUpdateLock, FFocusLock: Integer;
    FUpdateFlags: set of (ufPageNames, ufTabsAndPage, ufStatusBar, ufProjectFiles, ufFocusEditor, ufActiveEditorChanged);
    FPageIndex: Integer;
    fAutoFocusLock: integer;
    FIncrementalSearchPos: TPoint; // last set position
    fIncrementalSearchStartPos: TPoint; // position where to start searching
    FIncrementalSearchStr, FIncrementalFoundStr: string;
    FIncrementalSearchBackwards : Boolean;
    FIncrementalSearchEditor: TSourceEditor; // editor with active search (MWE:shouldnt all FIncrementalSearch vars go to that editor ?)
    FKeyStrokes: TSynEditKeyStrokes;
    FLastCodeBuffer: TCodeBuffer;
    FProcessingCommand: boolean;
    FSourceEditorList: TList; // list of TSourceEditor
    FHistoryList: TList; // list of TSourceEditor page order for when a window closes
  private
    FUpdateTabAndPageTimer: TTimer;
    // PopupMenu
    procedure BuildPopupMenu;
    //forwarders to FNoteBook
    function GetNoteBookPage(Index: Integer): TTabSheet;
    function GetNotebookPages: TStrings;
    function GetPageCount: Integer;
    function GetPageIndex: Integer;
    procedure SetPageIndex(const AValue: Integer);

    procedure UpdateHighlightMenuItems;
    procedure UpdateLineEndingMenuItems;
    procedure UpdateEncodingMenuItems;
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
  protected
    function NoteBookIndexOfPage(APage: TTabSheet): Integer;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure DragCanceled; override;
    procedure DoActiveEditorChanged;
  protected
    States: TSourceNotebookStates;
    // hintwindow stuff
    FHintWindow: THintWindow;
    FMouseHintTimer: TIdleTimer;

    procedure Activate; override;
    procedure CreateNotebook;
    function NewSE(Pagenum: Integer; NewPagenum: Integer = -1;
                   ASharedEditor: TSourceEditor = nil;
                   ATabCaption: String = ''): TSourceEditor;
    procedure AcceptEditor(AnEditor: TSourceEditor);
    procedure ReleaseEditor(AnEditor: TSourceEditor);
    procedure EditorChanged(Sender: TObject);
    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure DoShow; override;

  protected
    function GetActiveCompletionPlugin: TSourceEditorCompletionPlugin; override;
    function GetCompletionPlugins(Index: integer): TSourceEditorCompletionPlugin; override;
    function GetCompletionBoxPosition: integer; override;
        deprecated {$IFDEF VER2_5}'use SourceEditorManager'{$ENDIF};       // deprecated in 0.9.29 March 2010

    procedure EditorMouseMove(Sender: TObject; Shift: TShiftstate;
                              X,Y: Integer);
    procedure EditorMouseDown(Sender: TObject; Button: TMouseButton;
                              Shift: TShiftstate; X,Y: Integer);
    function EditorGetIndent(Sender: TObject; Editor: TObject;
             LogCaret, OldLogCaret: TPoint; FirstLinePos, LastLinePos: Integer;
             Reason: TSynEditorCommand;
             SetIndentProc: TSynBeautifierSetIndentProc): Boolean;
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorMouseWheel(Sender: TObject; Shift: TShiftState;
         WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    procedure NotebookMouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X,Y: Integer);
    procedure NotebookDragTabMove(Sender, Source: TObject;
                                  OldIndex, NewIndex: Integer; CopyDrag: Boolean;
                                  var Done: Boolean);
    procedure NotebookCanDragTabMove(Sender, Source: TObject;
                                  OldIndex, NewIndex: Integer; CopyDrag: Boolean;
                                  var Accept: Boolean);
    procedure NotebookDragOver(Sender, Source: TObject;
                               X,Y: Integer; State: TDragState; var Accept: Boolean);
    procedure NotebookEndDrag(Sender, Target: TObject; X,Y: Integer);
    // hintwindow stuff
    procedure HintTimer(Sender: TObject);
    procedure OnApplicationUserInput(Sender: TObject; Msg: Cardinal);
    procedure ShowSynEditHint(const MousePos: TPoint);

    procedure NextEditor;
    procedure PrevEditor;
    procedure MoveEditor(OldPageIndex, NewPageIndex: integer);
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
    procedure MoveEditor(OldPageIndex, NewWindowIndex, NewPageIndex: integer);
    procedure CopyEditor(OldPageIndex, NewWindowIndex, NewPageIndex: integer; Focus: Boolean = False);
    procedure ProcessParentCommand(Sender: TObject;
       var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
       var Handled: boolean);
    procedure ParentCommandProcessed(Sender: TObject;
       var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
       var Handled: boolean);

    function GetActiveEditor: TSourceEditorInterface; override;
    procedure SetActiveEditor(const AValue: TSourceEditorInterface); override;
    function GetItems(Index: integer): TSourceEditorInterface; override;
    function GetEditors(Index:integer): TSourceEditor;

    property Manager: TSourceEditorManager read FManager;

    procedure KeyDownBeforeInterface(var Key: Word; Shift: TShiftState); override;

    procedure BeginAutoFocusLock;
    procedure EndAutoFocusLock;

  protected
    procedure CloseTabClicked(Sender: TObject);
    procedure CloseClicked(Sender: TObject; CloseOthers: Boolean = False);
    procedure ToggleFormUnitClicked(Sender: TObject);
    procedure ToggleObjectInspClicked(Sender: TObject);

    procedure IncUpdateLockInternal;
    procedure DecUpdateLockInternal;

    // editor page history
    procedure HistorySetMostRecent(APage: TTabSheet);
    procedure HistoryAdd(APage: TTabSheet);
    procedure HistoryRemove(APage: TTabSheet);
    function  HistoryGetTopPageIndex: Integer;

    // incremental find
    procedure BeginIncrementalFind;
    procedure EndIncrementalFind;
    property IncrementalSearchStr: string
      read FIncrementalSearchStr write SetIncrementalSearchStr;

    // hints
    procedure ActivateHint(const ScreenPos: TPoint;
                           const BaseURL, TheHint: string);
    procedure HideHint;
    procedure StartShowCodeContext(JumpToError: boolean);

    // paste and copy
    procedure CutClicked(Sender: TObject);
    procedure CopyClicked(Sender: TObject);
    procedure PasteClicked(Sender: TObject);

    procedure ReloadEditorOptions;
    procedure CheckFont;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Editors[Index:integer]:TSourceEditor read GetEditors; // !!! not ordered for PageIndex
    function EditorCount: integer;
    function IndexOfEditor(aEditor: TSourceEditorInterface): integer;
    function Count: integer; override;

    function FindSourceEditorWithPageIndex(APageIndex:integer):TSourceEditor;
    function FindPageWithEditor(ASourceEditor: TSourceEditor):integer;
    function FindSourceEditorWithEditorComponent(
                                         EditorComp: TComponent): TSourceEditor;
    function GetActiveSE: TSourceEditor;                                        { $note deprecate and use SetActiveEditor}
    procedure CheckCurrentCodeBufferChanged;
    function IndexOfEditorInShareWith(AnOtherEditor: TSourceEditor): Integer;

    procedure UpdateStatusBar;
    procedure ClearErrorLines; override;
    procedure ClearExecutionLines;
    procedure ClearExecutionMarks;

    // new, close, focus
    function NewFile(const NewShortName: String; ASource: TCodeBuffer;
                      FocusIt: boolean; AShareEditor: TSourceEditor = nil): TSourceEditor;
    procedure CloseFile(APageIndex:integer);
    procedure FocusEditor;

  public
    function GetEditorControlSettings(EditControl: TControl): boolean; override;
             deprecated {$IFDEF VER2_5}'use SourceEditorManager'{$ENDIF};       // deprecated in 0.9.29 March 2010
    function GetHighlighterSettings(Highlighter: TObject): boolean; override;
             deprecated {$IFDEF VER2_5}'use SourceEditorManager'{$ENDIF};       // deprecated in 0.9.29 March 2010

    procedure DeactivateCompletionForm; override;
             deprecated {$IFDEF VER2_5}'use SourceEditorManager'{$ENDIF};       // deprecated in 0.9.29 March 2010
    function CompletionPluginCount: integer; override;
              deprecated {$IFDEF VER2_5}'use SourceEditorManager'{$ENDIF};       // deprecated in 0.9.29 March 2010
    procedure RegisterCompletionPlugin(Plugin: TSourceEditorCompletionPlugin); override;
              deprecated {$IFDEF VER2_5}'use SourceEditorManager'{$ENDIF};       // deprecated in 0.9.29 March 2010
    procedure UnregisterCompletionPlugin(Plugin: TSourceEditorCompletionPlugin); override;
              deprecated {$IFDEF VER2_5}'use SourceEditorManager'{$ENDIF};       // deprecated in 0.9.29 March 2010

  public
    function GetCapabilities: TCTabControlCapabilities;
    procedure IncUpdateLock;
    procedure DecUpdateLock;

    // forwarders to the FNotebook
    property PageIndex: Integer read GetPageIndex write SetPageIndex;
    property PageCount: Integer read GetPageCount;
    property NotebookPages: TStrings read GetNotebookPages;
  end;

  TSrcEditMangerHandlerType = (
    semhtCopyPaste
    );

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
    FUpdateFlags: set of (ufMgrActiveEditorChanged, ufShowWindowOnTop, ufShowWindowOnTopFocus);
    procedure FreeSourceWindows;
    function GetActiveSourceWindowIndex: integer;
    function GetSourceWindowByLastFocused(Index: Integer): TSourceEditorWindowInterface;
    procedure SetActiveSourceWindowIndex(const AValue: integer);
  protected
    fProducers: TFPList; // list of TSourceMarklingProducer
    FChangeNotifyLists: Array [TsemChangeReason] of TMethodList;
    FHandlers: array[TSrcEditMangerHandlerType] of TMethodList;
    function  GetActiveSourceWindow: TSourceEditorWindowInterface; override;
    procedure SetActiveSourceWindow(const AValue: TSourceEditorWindowInterface); override;
    function  GetSourceWindows(Index: integer): TSourceEditorWindowInterface; override;
    procedure DoWindowFocused(AWindow: TSourceNotebook);  // Includes Focus to ChildControl (aka Activated)
    function  GetActiveEditor: TSourceEditorInterface; override;
    procedure SetActiveEditor(const AValue: TSourceEditorInterface); override;
    procedure DoActiveEditorChanged;
    procedure DoEditorStatusChanged(AEditor: TSourceEditor);
    function  GetSourceEditors(Index: integer): TSourceEditorInterface; override;
    function  GetUniqueSourceEditors(Index: integer): TSourceEditorInterface; override;
    function GetMarklingProducers(Index: integer): TSourceMarklingProducer; override;
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
    function GetDefaultCompletionForm: TSourceEditCompletion;
    procedure  FreeCompletionPlugins;
    function  GetScreenRectForToken(AnEditor: TCustomSynEdit; PhysColumn, PhysRow, EndColumn: Integer): TRect;
  protected
    function  GetActiveCompletionPlugin: TSourceEditorCompletionPlugin; override;
    function  GetCompletionBoxPosition: integer; override;
    function  GetCompletionPlugins(Index: integer): TSourceEditorCompletionPlugin; override;
    function  FindIdentCompletionPlugin(SrcEdit: TSourceEditor; JumpToError: boolean;
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
    procedure ShowActiveWindowOnTop(Focus: Boolean = False);
  private
    FOnCurrentCodeBufferChanged: TNotifyEvent;
  public
    property OnCurrentCodeBufferChanged: TNotifyEvent
             read FOnCurrentCodeBufferChanged write FOnCurrentCodeBufferChanged;
  end;

  { TSourceEditorManager }
  (* Reintroduce all Methods with the final types *)

  TSourceEditorManager = class(TSourceEditorManagerBase)
  private
    function GetActiveSourceNotebook: TSourceNotebook;
    function GetActiveSrcEditor: TSourceEditor;
    function GetSourceEditorsByPage(WindowIndex, PageIndex: integer
      ): TSourceEditor;
    function GetSourceNbByLastFocused(Index: Integer): TSourceNotebook;
    function GetSrcEditors(Index: integer): TSourceEditor;
    procedure SetActiveSourceNotebook(const AValue: TSourceNotebook);
    function GetSourceNotebook(Index: integer): TSourceNotebook;
    procedure SetActiveSrcEditor(const AValue: TSourceEditor);
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
    // Editors
    function  SourceEditorCount: integer; override;
    function  GetActiveSE: TSourceEditor;                                       { $note deprecate and use ActiveEditor}
    property  ActiveEditor: TSourceEditor read GetActiveSrcEditor  write SetActiveSrcEditor; // reintroduced
    property SourceEditors[Index: integer]: TSourceEditor read GetSrcEditors;   // reintroduced
    property  SourceEditorsByPage[WindowIndex, PageIndex: integer]: TSourceEditor
              read GetSourceEditorsByPage;
    function  SourceEditorIntfWithFilename(const Filename: string): TSourceEditor; reintroduce;
    function FindSourceEditorWithEditorComponent(EditorComp: TComponent): TSourceEditor; // With SynEdit
  protected
    procedure NewEditorCreated(AEditor: TSourceEditor);
    procedure EditorRemoved(AEditor: TSourceEditor);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RemoveWindow(AWindow: TSourceNotebook);
  public
    // Forward to all windows
    procedure ClearErrorLines; override;
    procedure ClearExecutionLines;
    procedure ClearExecutionMarks;
    procedure FillExecutionMarks;
    procedure ReloadEditorOptions;
    // find / replace text
    procedure FindClicked(Sender: TObject);
    procedure FindNextClicked(Sender: TObject);
    procedure FindPreviousClicked(Sender: TObject);
    procedure ReplaceClicked(Sender: TObject);
    procedure IncrementalFindClicked(Sender: TObject);
    procedure GotoLineClicked(Sender: TObject);
    procedure JumpBackClicked(Sender: TObject);
    procedure JumpForwardClicked(Sender: TObject);
    procedure AddJumpPointClicked(Sender: TObject);
    procedure DeleteLastJumpPointClicked(Sender: TObject);
    procedure ViewJumpHistoryClicked(Sender: TObject);
  protected
    // Bookmarks
    procedure BookMarkToggleClicked(Sender: TObject);
    procedure BookMarkGotoClicked(Sender: TObject);
  public
    procedure BookMarkNextClicked(Sender: TObject);
    procedure BookMarkPrevClicked(Sender: TObject);
  protected
    // macros
    function MacroFuncCol(const s:string; const Data: PtrInt;
                          var Abort: boolean): string;
    function MacroFuncRow(const s:string; const Data: PtrInt;
                          var Abort: boolean): string;
    function MacroFuncEdFile(const s:string; const Data: PtrInt;
                             var Abort: boolean): string;
    function MacroFuncCurToken(const s:string; const Data: PtrInt;
                               var Abort: boolean): string;
    function MacroFuncPrompt(const s:string; const Data: PtrInt;
                             var Abort: boolean): string;
  public
    procedure InitMacros(AMacroList: TTransferMacroList);
    procedure SetupShortCuts;

    function FindUniquePageName(FileName:string; IgnoreEditor: TSourceEditor):string;
    function SomethingModified(Verbose: boolean = false): boolean;
    procedure HideHint;
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure LockAllEditorsInSourceChangeCache;
    procedure UnlockAllEditorsInSourceChangeCache;
    procedure CloseFile(AEditor: TSourceEditorInterface);
    // history jumping
    procedure HistoryJump(Sender: TObject; CloseAction: TJumpHistoryAction);
  private
    FCodeTemplateModul: TSynEditAutoComplete;
    FGotoDialog: TfrmGoto;
    procedure OnFilesDroping(Sender: TObject; const FileNames: Array of String);
    procedure OnCodeTemplateTokenNotFound(Sender: TObject; AToken: string;
                                   AnEditor: TCustomSynEdit; var Index:integer);
    procedure OnCodeTemplateExecuteCompletion(
                                       ASynAutoComplete: TCustomSynAutoComplete;
                                       Index: integer);
  protected
    procedure OnWordCompletionGetSource(var Source: TStrings; SourceIndex: integer);
    procedure OnSourceCompletionTimer(Sender: TObject);
    // marks
    function OnSourceMarksGetFilename(ASourceEditor: TObject): string;
    procedure OnSourceMarksAction(AMark: TSourceMark; AAction: TMarksAction);
    property CodeTemplateModul: TSynEditAutoComplete
                               read FCodeTemplateModul write FCodeTemplateModul;
    // goto dialog
    function GotoDialog: TfrmGoto;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateNewWindow(Activate: Boolean= False;
                             DoDisableAutoSizing: boolean = False): TSourceNotebook;
  private
    // Context-Menu
    procedure CloseOtherPagesClicked(Sender: TObject);
    procedure ReadOnlyClicked(Sender: TObject);
    procedure ToggleLineNumbersClicked(Sender: TObject);
    procedure ToggleI18NForLFMClicked(Sender: TObject);
    procedure ShowUnitInfo(Sender: TObject);
    procedure CopyFilenameClicked(Sender: TObject);
    procedure EditorPropertiesClicked(Sender: TObject);
  private
    FOnAddJumpPoint: TOnAddJumpPoint;
    FOnClearBookmark: TPlaceBookMarkEvent;
    FOnClickLink: TMouseEvent;
    FOnCloseClicked: TOnCloseSrcEditor;
    FOnDeleteLastJumpPoint: TNotifyEvent;
    FOnEditorMoved: TNotifyEvent;
    FOnEditorPropertiesClicked: TNotifyEvent;
    FOnFindDeclarationClicked: TNotifyEvent;
    FOnGetIndent: TOnGetIndentEvent;
    FOnGotoBookmark: TBookMarkActionEvent;
    FOnInitIdentCompletion: TOnInitIdentCompletion;
    FOnJumpToHistoryPoint: TOnJumpToHistoryPoint;
    FOnMouseLink: TSynMouseLinkEvent;
    FOnNoteBookCloseQuery: TCloseEvent;
    FOnOpenFileAtCursorClicked: TNotifyEvent;
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
    property OnEditorMoved: TNotifyEvent
             read FOnEditorMoved write FOnEditorMoved;
    property OnEditorPropertiesClicked: TNotifyEvent
             read FOnEditorPropertiesClicked write FOnEditorPropertiesClicked;
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
  end;

function SourceEditorManager: TSourceEditorManager;


  //=============================================================================

const
  SourceEditorMenuRootName = 'SourceEditor';

var
  SrcEditMenuFindDeclaration: TIDEMenuCommand;
    // finding / jumping
    SrcEditMenuProcedureJump: TIDEMenuCommand;
    SrcEditMenuFindNextWordOccurrence: TIDEMenuCommand;
    SrcEditMenuFindPrevWordOccurrence: TIDEMenuCommand;
    SrcEditMenuFindinFiles: TIDEMenuCommand;
    SrcEditMenuFindIdentifierReferences: TIDEMenuCommand;
    // open file
    SrcEditMenuOpenFileAtCursor: TIDEMenuCommand;
  SrcEditMenuClosePage: TIDEMenuCommand;
  SrcEditMenuCloseOtherPages: TIDEMenuCommand;
  SrcEditMenuCut: TIDEMenuCommand;
  SrcEditMenuCopy: TIDEMenuCommand;
  SrcEditMenuPaste: TIDEMenuCommand;
  SrcEditMenuCopyFilename: TIDEMenuCommand;
    // bookmarks
    SrcEditMenuNextBookmark: TIDEMenuCommand;
    SrcEditMenuPrevBookmark: TIDEMenuCommand;
    SrcEditMenuSetFreeBookmark: TIDEMenuCommand;
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


procedure RegisterStandardSourceEditorMenuItems;

var
  Highlighters: array[TLazSyntaxHighlighter] of TSynCustomHighlighter;

implementation

{$R *.lfm}

const
  (* SoftCenter are th visible Lines in the Editor where the caret can be locateted,
     without CenterCursor adjusting the topline.
     SoftCenter is defined by the amount of lines on the top/bottom of fthe editor,
     which are *not* part of it.
  *)
  SoftCenterFactor  = 5;  // One fifth of the "LinesInWindow"on each side (top/bottom)
  SoftCenterMinimum = 1;
  SoftCenterMaximum = 8;

var
  SourceCompletionTimer: TIdleTimer = nil;
  SourceCompletionCaretXY: TPoint;
  AWordCompletion: TWordCompletion = nil;

function SourceEditorManager: TSourceEditorManager;
begin
  Result := TSourceEditorManager(SourceEditorManagerIntf);
end;

procedure ExecuteIdeMenuClick(Sender: TObject);
var
  ActEdit: TSourceEditor;
  r: Boolean;
begin
  if (SourceEditorManager = nil) or (Sender = nil) or not(Sender is TIDEMenuCommand) then
    exit;
  ActEdit := SourceEditorManager.ActiveEditor;
  if (ActEdit = nil) or (TIDEMenuCommand(Sender).Command = nil) then
    exit;
  r :=  TIDEMenuCommand(Sender).Command.OnExecuteProc = @ExecuteIdeMenuClick;
  if r then
    TIDEMenuCommand(Sender).Command.OnExecuteProc := nil;
  ActEdit.DoEditorExecuteCommand(TIDEMenuCommand(Sender).Command.Command);
  if r then
    TIDEMenuCommand(Sender).Command.OnExecuteProc := @ExecuteIdeMenuClick;
end;

procedure RegisterStandardSourceEditorMenuItems;
var
  AParent: TIDEMenuSection;
  I: Integer;
begin
  SourceEditorMenuRoot:=RegisterIDEMenuRoot(SourceEditorMenuRootName);
  AParent:=SourceEditorMenuRoot;

  // register the first dynamic section for often used context sensitive stuff
  SrcEditMenuSectionFirstDynamic:=RegisterIDEMenuSection(AParent, 'First dynamic section');

  {%region *** first static section *** }
    SrcEditMenuSectionFirstStatic:=RegisterIDEMenuSection(AParent, 'First static section');
    AParent:=SrcEditMenuSectionFirstStatic;

    SrcEditMenuFindDeclaration := RegisterIDEMenuCommand
        (AParent, 'Find Declaration', uemFindDeclaration, nil, @ExecuteIdeMenuClick);

    {%region *** Submenu: Find Section *** }
      SrcEditSubMenuFind := RegisterIDESubMenu(AParent, 'Find section', lisMenuFind);
      AParent:=SrcEditSubMenuFind;

      SrcEditMenuProcedureJump := RegisterIDEMenuCommand
          (AParent,'Procedure Jump', uemProcedureJump, nil, @ExecuteIdeMenuClick);
      SrcEditMenuFindNextWordOccurrence := RegisterIDEMenuCommand
          (AParent, 'Find next word occurrence', srkmecFindNextWordOccurrence,
           nil, @ExecuteIdeMenuClick, nil, 'menu_search_find_next');
      SrcEditMenuFindPrevWordOccurrence := RegisterIDEMenuCommand
          (AParent, 'Find previous word occurrence', srkmecFindPrevWordOccurrence,
           nil, @ExecuteIdeMenuClick, nil, 'menu_search_find_previous');
      SrcEditMenuFindInFiles := RegisterIDEMenuCommand
          (AParent, 'Find in files', srkmecFindInFiles, nil,
           @ExecuteIdeMenuClick, nil, 'menu_search_files');
      SrcEditMenuFindIdentifierReferences := RegisterIDEMenuCommand
          (AParent, 'FindIdentifierReferences',lisMenuFindIdentifierRefs, nil, @ExecuteIdeMenuClick);
    {%endregion}
  {%endregion}

  {%region *** Pages section ***}
  SrcEditMenuSectionPages:=RegisterIDEMenuSection(SourceEditorMenuRoot, 'Pages');

    SrcEditMenuClosePage := RegisterIDEMenuCommand(SrcEditMenuSectionPages,
        'Close Page', uemClosePage, nil, @ExecuteIdeMenuClick, nil, 'menu_close');
    SrcEditMenuCloseOtherPages := RegisterIDEMenuCommand(SrcEditMenuSectionPages,
        'Close All Other Pages',uemCloseOtherPages, nil, @ExecuteIdeMenuClick);

    {$IFnDEF SingleSrcWindow}
    // Lock Editor
    SrcEditMenuEditorLock := RegisterIDEMenuCommand
        (SrcEditMenuSectionPages, 'LockEditor', uemLockPage, nil, @ExecuteIdeMenuClick);
    SrcEditMenuEditorLock.ShowAlwaysCheckable := True;
    // Move to other Window
    SrcEditMenuMoveToNewWindow := RegisterIDEMenuCommand
        (SrcEditMenuSectionPages, 'MoveToNewWindow', uemMoveToNewWindow, nil, @ExecuteIdeMenuClick);

    {%region * Move To Other *}
      SrcEditMenuMoveToOtherWindow := RegisterIDESubMenu
          (SrcEditMenuSectionPages, 'MoveToOtherWindow', uemMoveToOtherWindow);

      SrcEditMenuMoveToOtherWindowNew := RegisterIDEMenuCommand
          (SrcEditMenuMoveToOtherWindow, 'MoveToOtherWindowNew', uemMoveToOtherWindowNew,
           nil, @ExecuteIdeMenuClick);
      // Section for dynamically created targets
      SrcEditMenuMoveToOtherWindowList := RegisterIDEMenuSection
          (SrcEditMenuMoveToOtherWindow, 'MoveToOtherWindowList Section');
    {%endregion}

    SrcEditMenuCopyToNewWindow := RegisterIDEMenuCommand
        (SrcEditMenuSectionPages, 'CopyToNewWindow', uemCopyToNewWindow, nil, @ExecuteIdeMenuClick);

    {%region * Copy To Other *}
      SrcEditMenuCopyToOtherWindow := RegisterIDESubMenu
          (SrcEditMenuSectionPages, 'CopyToOtherWindow', uemCopyToOtherWindow);

      SrcEditMenuCopyToOtherWindowNew := RegisterIDEMenuCommand
          (SrcEditMenuCopyToOtherWindow, 'CopyToOtherWindowNew', uemCopyToOtherWindowNew,
           nil, @ExecuteIdeMenuClick);
      // Section for dynamically created targets
      SrcEditMenuCopyToOtherWindowList := RegisterIDEMenuSection
          (SrcEditMenuCopyToOtherWindow, 'CopyToOtherWindowList Section');
    {%endregion}

    SrcEditMenuFindInOtherWindow := RegisterIDESubMenu
        (SrcEditMenuSectionPages, 'FindInOtherWindow', uemFindInOtherWindow);
    // Section for dynamically created targets
    SrcEditMenuFindInOtherWindowList := RegisterIDEMenuSection
        (SrcEditMenuFindInOtherWindow, 'FindInOtherWindowList Section');
    {$ENDIF}

    {%region * Move Page (left/right) *}
      SrcEditSubMenuMovePage :=
        RegisterIDESubMenu(SrcEditMenuSectionPages, 'Move Page', lisMovePage);
      AParent:=SrcEditSubMenuMovePage;

      SrcEditMenuMoveEditorLeft := RegisterIDEMenuCommand
          (AParent,'MoveEditorLeft', uemMovePageLeft, nil, @ExecuteIdeMenuClick);
      SrcEditMenuMoveEditorRight := RegisterIDEMenuCommand
          (AParent,'MoveEditorRight', uemMovePageRight, nil, @ExecuteIdeMenuClick);
      SrcEditMenuMoveEditorFirst := RegisterIDEMenuCommand
          (AParent,'MoveEditorLeftmost', uemMovePageLeftmost, nil, @ExecuteIdeMenuClick);
      SrcEditMenuMoveEditorLast := RegisterIDEMenuCommand
          (AParent,'MoveEditorRightmost', uemMovePageRightmost, nil, @ExecuteIdeMenuClick);
    {%endregion}

    {%region * sub menu Open File *}
      SrcEditSubMenuOpenFile :=
        RegisterIDESubMenu(SrcEditMenuSectionPages, 'Open File', lisOpenFile);
      AParent:=SrcEditSubMenuOpenFile;

      SrcEditMenuOpenFileAtCursor:=RegisterIDEMenuCommand(AParent, 'Open File At Cursor',
          uemOpenFileAtCursor, nil, @ExecuteIdeMenuClick, nil, 'menu_search_openfile_atcursor');
      // register the File Specific dynamic section
      SrcEditMenuSectionFileDynamic:=RegisterIDEMenuSection(AParent, 'File dynamic section');
    {%endregion}

    {%region * sub menu Flags section *}
      SrcEditSubMenuFlags :=
        RegisterIDESubMenu(SrcEditMenuSectionPages, 'Flags section', lisFileSettings);
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

  {%region *** Clipboard section ***}
    SrcEditMenuSectionClipboard:=RegisterIDEMenuSection(SourceEditorMenuRoot, 'Clipboard');
    AParent:=SrcEditMenuSectionClipboard;

    SrcEditMenuCut:=RegisterIDEMenuCommand(AParent,'Cut',uemCut, nil, nil, nil, 'laz_cut');
    SrcEditMenuCopy:=RegisterIDEMenuCommand(AParent,'Copy',uemCopy, nil, nil, nil, 'laz_copy');
    SrcEditMenuPaste:=RegisterIDEMenuCommand(AParent,'Paste',uemPaste, nil, nil, nil, 'laz_paste');
    SrcEditMenuCopyFilename:=RegisterIDEMenuCommand(AParent,'Copy filename', uemCopyFilename);
  {%endregion}

  {%region *** Goto Marks section ***}
  SrcEditMenuSectionMarks:=RegisterIDEMenuSection(SourceEditorMenuRoot,
                                                  'Marks section');
    // register the Goto Bookmarks Submenu
    SrcEditSubMenuGotoBookmarks:=RegisterIDESubMenu(SrcEditMenuSectionMarks,
                                              'Goto bookmarks',uemGotoBookmark);
    AParent:=SrcEditSubMenuGotoBookmarks;
      for I := 0 to 9 do
        RegisterIDEMenuCommand(AParent,'GotoBookmark'+IntToStr(I),
                               uemBookmarkN+IntToStr(i), nil, @ExecuteIdeMenuClick);
      SrcEditMenuNextBookmark:=RegisterIDEMenuCommand
          (AParent, 'Goto next Bookmark',uemNextBookmark, nil,
           @ExecuteIdeMenuClick, nil, 'menu_search_next_bookmark');
      SrcEditMenuPrevBookmark:=RegisterIDEMenuCommand
          (AParent, 'Goto previous Bookmark',uemPrevBookmark, nil,
           @ExecuteIdeMenuClick, nil, 'menu_search_previous_bookmark');
  {%endregion}

  {%region *** Toggle Bookmarks Submenu ***}
    SrcEditSubMenuToggleBookmarks:=RegisterIDESubMenu
        (SrcEditMenuSectionMarks, 'Toggle bookmarks',uemToggleBookmark);
    AParent:=SrcEditSubMenuToggleBookmarks;
      for I := 0 to 9 do
        RegisterIDEMenuCommand(AParent, 'ToggleBookmark'+IntToStr(I), uemBookmarkN+IntToStr(i),
                               nil, @ExecuteIdeMenuClick);
      SrcEditMenuSetFreeBookmark:=RegisterIDEMenuCommand
          (AParent, 'Set a free Bookmark',uemSetFreeBookmark, nil, @ExecuteIdeMenuClick);
  {%endregion}

  {%region *** Debug Section ***}
    // Commands will be assigned by DebugManager
    SrcEditMenuSectionDebug:=RegisterIDEMenuSection(SourceEditorMenuRoot, 'Debug section');
    // register the Debug submenu
    SrcEditSubMenuDebug:=RegisterIDESubMenu(SrcEditMenuSectionDebug,
                                            'Debug', uemDebugWord, nil, nil, 'debugger');
    AParent:=SrcEditSubMenuDebug;

      // register the Debug submenu items
      SrcEditMenuToggleBreakpoint:=RegisterIDEMenuCommand
          (AParent,'Toggle Breakpoint', uemToggleBreakpoint, nil, @ExecuteIdeMenuClick);
      SrcEditMenuEvaluateModify:=RegisterIDEMenuCommand
          (AParent,'Evaluate/Modify...', uemEvaluateModify, nil, nil, nil,'debugger_modify');
      SrcEditMenuEvaluateModify.Enabled:=False;
      SrcEditMenuAddWatchAtCursor:=RegisterIDEMenuCommand
          (AParent, 'Add Watch at Cursor',uemAddWatchAtCursor);
      SrcEditMenuAddWatchPointAtCursor:=RegisterIDEMenuCommand
          (AParent, 'Add Watch at Cursor',uemAddWatchPointAtCursor);
      SrcEditMenuInspect:=RegisterIDEMenuCommand
          (AParent, 'Inspect...', uemInspect, nil, nil, nil, 'debugger_inspect');
      SrcEditMenuInspect.Enabled:=False;
      SrcEditMenuRunToCursor:=RegisterIDEMenuCommand
          (AParent, 'Run to cursor', uemRunToCursor, nil, nil, nil, 'menu_run_cursor');
      SrcEditMenuViewCallStack:=RegisterIDEMenuCommand
          (AParent, 'View Call Stack', uemViewCallStack, nil, @ExecuteIdeMenuClick, nil, 'debugger_call_stack');
  {%endregion}

  {%region *** Source Section ***}
    SrcEditSubMenuSource:=RegisterIDESubMenu(SourceEditorMenuRoot,
                                             'Source',uemSource);
    AParent:=SrcEditSubMenuSource;
    SrcEditMenuEncloseSelection := RegisterIDEMenuCommand
        (AParent, 'EncloseSelection',lisMenuEncloseSelection);
    SrcEditMenuEncloseInIFDEF := RegisterIDEMenuCommand
        (AParent,'itmSourceEncloseInIFDEF',lisMenuEncloseInIFDEF);
    SrcEditMenuCompleteCode := RegisterIDEMenuCommand
        (AParent,'CompleteCode', lisMenuCompleteCode, nil, @ExecuteIdeMenuClick);
    SrcEditMenuUseUnit := RegisterIDEMenuCommand
        (AParent,'UseUnit', lisMenuUseUnit, nil, @ExecuteIdeMenuClick);
    SrcEditMenuShowUnitInfo := RegisterIDEMenuCommand
        (AParent,'ShowUnitInfo', lisMenuViewUnitInfo);
  {%endregion}

  {%region *** Refactoring Section ***}
    SrcEditSubMenuRefactor:=RegisterIDESubMenu(SourceEditorMenuRoot,
                                               'Refactoring',uemRefactor);
    AParent:=SrcEditSubMenuRefactor;
    SrcEditMenuRenameIdentifier := RegisterIDEMenuCommand
        (AParent, 'RenameIdentifier',lisMenuRenameIdentifier, nil, @ExecuteIdeMenuClick);
    SrcEditMenuExtractProc := RegisterIDEMenuCommand
        (AParent, 'ExtractProc',lisMenuExtractProc, nil, @ExecuteIdeMenuClick);
    SrcEditMenuInvertAssignment := RegisterIDEMenuCommand
        (AParent, 'InvertAssignment',uemInvertAssignment, nil, @ExecuteIdeMenuClick);
    SrcEditMenuShowAbstractMethods := RegisterIDEMenuCommand
        (AParent, 'ShowAbstractMethods',srkmecAbstractMethods, nil, @ExecuteIdeMenuClick);
    SrcEditMenuShowEmptyMethods := RegisterIDEMenuCommand
        (AParent, 'ShowEmptyMethods', srkmecEmptyMethods, nil, @ExecuteIdeMenuClick);
    SrcEditMenuShowUnusedUnits := RegisterIDEMenuCommand
        (AParent, 'ShowUnusedUnits', srkmecUnusedUnits, nil, @ExecuteIdeMenuClick);
    SrcEditMenuFindOverloads := RegisterIDEMenuCommand
        (AParent, 'FindOverloads', srkmecFindOverloadsCapt, nil, @ExecuteIdeMenuClick);
   {$IFnDEF EnableFindOverloads}
   SrcEditMenuFindOverloads.Visible:=false;
   {$ENDIF}
  {%endregion}

  SrcEditMenuEditorProperties:=RegisterIDEMenuCommand(SourceEditorMenuRoot,
           'EditorProperties', dlgFROpts, nil, nil, nil, 'menu_environment_options');
end;


{ TSourceEditCompletion }

procedure TSourceEditCompletion.CompletionFormResized(Sender: TObject);
begin
  EnvironmentOptions.CompletionWindowWidth  := TheForm.Width;
  EnvironmentOptions.CompletionWindowHeight := TheForm.NbLinesInWindow;
end;

procedure TSourceEditCompletion.ccExecute(Sender: TObject);
// init completion form
// called by OnExecute just before showing
var
  S: TStrings;
  Prefix: String;
  I: Integer;
  NewStr: String;
Begin
  {$IFDEF VerboseIDECompletionBox}
  debugln(['TSourceEditCompletion.ccExecute START']);
  {$ENDIF}
  TheForm.Font := Editor.Font;
  FActiveEditDefaultFGColor := Editor.Font.Color;
  FActiveEditDefaultBGColor := Editor.Color;
  FActiveEditSelectedFGColor := TSynEdit(Editor).SelectedColor.Foreground;
  FActiveEditSelectedBGColor := TSynEdit(Editor).SelectedColor.Background;

  if Editor.Highlighter<>nil
  then begin
    with Editor.Highlighter do begin
      if IdentifierAttribute<>nil
      then begin
        if IdentifierAttribute.ForeGround<>clNone then
          FActiveEditDefaultFGColor:=IdentifierAttribute.ForeGround;
        if IdentifierAttribute.BackGround<>clNone then
          FActiveEditDefaultBGColor:=IdentifierAttribute.BackGround;
      end;
    end;
  end;

  S := TStringList.Create;
  try
    Prefix := CurrentString;
    case CurrentCompletionType of
     ctIdentCompletion:
       if not InitIdentCompletionValues(S) then begin
         ItemList.Clear;
         exit;
       end;

     ctWordCompletion:
       begin
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
    with TheForm do begin
      BackgroundColor   := FActiveEditDefaultBGColor;
      clSelect          := FActiveEditSelectedBGColor;
      TextColor         := FActiveEditDefaultFGColor;
      TextSelectedColor := FActiveEditSelectedFGColor;
      //debugln('TSourceNotebook.ccExecute A Color=',DbgS(Color),
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
  debugln(['TSourceNotebook.ccCancel START']);
  //debugln(GetStackTrace(true));
  {$ENDIF}
  Manager.DeactivateCompletionForm;
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
Begin
  {$IFDEF VerboseIDECompletionBox}
  debugln(['TSourceNotebook.ccComplete START']);
  {$ENDIF}
  OldCompletionType:=CurrentCompletionType;
  case CurrentCompletionType of

    ctIdentCompletion:
      begin
        if not CodeToolsOpts.IdentComplReplaceIdentifier then
          SourceEnd:=Editor.LogicalCaretXY;
        if Manager.ActiveCompletionPlugin<>nil then
        begin
          Manager.ActiveCompletionPlugin.Complete(Value,SourceValue,
             SourceStart,SourceEnd,KeyChar,Shift);
          Manager.FActiveCompletionPlugin:=nil;
        end else begin
          // add to history
          CodeToolBoss.IdentifierHistory.Add(
            CodeToolBoss.IdentifierList.FilteredItems[Position]);
          // get value
          NewValue:=GetIdentCompletionValue(self, KeyChar, ValueType, CursorToLeft);
          if ValueType=icvIdentifier then ;
          // insert value plus special chars like brackets, semicolons, ...
          if ValueType <> icvNone then
            Editor.TextBetweenPointsEx[SourceStart, SourceEnd, scamEnd] := NewValue;
          if CursorToLeft>0 then
          begin
            NewCaretXY:=Editor.CaretXY;
            dec(NewCaretXY.X,CursorToLeft);
            Editor.CaretXY:=NewCaretXY;
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
    SourceCompletionTimer.AutoEnabled:=true;
  end;
end;

function TSourceEditCompletion.OnSynCompletionPaintItem(const AKey: string;
  ACanvas: TCanvas; X, Y: integer; ItemSelected: boolean; Index: integer
  ): boolean;
var
  MaxX: Integer;
  t: TCompletionType;
begin
  with ACanvas do begin
    if (Editor<>nil) then
      Font := Editor.Font
    else begin
      Font.Size:=EditorOpts.EditorFontSize; // set Size before name for XLFD !
      Font.Name:=EditorOpts.EditorFont;
    end;
    Font.Style:=[];
    if not ItemSelected then
      Font.Color := FActiveEditDefaultFGColor
    else
      Font.Color := FActiveEditSelectedFGColor;
  end;
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
  PaintCompletionItem(AKey,ACanvas,X,Y,MaxX,ItemSelected,Index,self,
                      t,Editor.Highlighter);
  Result:=true;
end;

function TSourceEditCompletion.OnSynCompletionMeasureItem(const AKey: string;
  ACanvas: TCanvas; ItemSelected: boolean; Index: integer): TPoint;
var
  MaxX: Integer;
  t: TCompletionType;
begin
  with ACanvas do begin
    if (Editor<>nil) then
      Font:=Editor.Font
    else begin
      Font.Size:=EditorOpts.EditorFontSize; // set Size before name of XLFD !
      Font.Name:=EditorOpts.EditorFont;
    end;
    Font.Style:=[];
    if not ItemSelected then
      Font.Color := FActiveEditDefaultFGColor
    else
      Font.Color := FActiveEditSelectedFGColor;
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
                                self,t,nil,True);
  Result.Y:=FontHeight;
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
            if AnsiCompareText(CurStr,copy(s,x,length(CurStr)))=0 then begin
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
          aWordCompletion.GetWordList(SL, CurStr, false, 100);
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
        aWordCompletion.GetWordList(SL, NewPrefix, false, 100);
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
  CharLen:=UTF8CharacterLength(@Line[LogCaret.X]);
  AddPrefix:=copy(Line,LogCaret.X,CharLen);
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
                                   length(NewPrefix))-1;
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
  end else begin
    Editor.CommandProcessor(ecChar,UTF8Key,nil);
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
  end else if Assigned(Manager.OnInitIdentCompletion) then
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
  TheForm.Width := Max(50, EnvironmentOptions.CompletionWindowWidth);
  TheForm.NbLinesInWindow := Max(3, EnvironmentOptions.CompletionWindowHeight);
  TheForm.OnDragResized  := @CompletionFormResized;
end;

{ TSourceEditorSharedValues }

function TSourceEditorSharedValues.GetSharedEditors(Index: Integer
  ): TSourceEditor;
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
begin
  if FCodeBuffer = AValue then exit;
  if FCodeBuffer<>nil then
    FCodeBuffer.RemoveChangeHook(@OnCodeBufferChanged);
  FCodeBuffer := AValue;
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
      if (FIgnoreCodeBufferLock <= 0) and (not FCodeBuffer.IsEqual(SynEditor.Lines))
      then begin
        {$IFDEF IDE_DEBUG}
        debugln(' *** WARNING *** : TSourceEditor.SetCodeBuffer - loosing marks: ',FCodeBuffer.Filename);
        {$ENDIF}
        for i := 0 to FSharedEditorList.Count - 1 do
          if assigned(SharedEditors[i].FEditPlugin) then
            SharedEditors[i].FEditPlugin.Enabled := False;
        SynEditor.BeginUpdate;
        FCodeBuffer.AssignTo(SynEditor.Lines, false);
        FEditorStampCommitedToCodetools:=(SynEditor.Lines as TSynEditLines).TextChangeStamp;
        SynEditor.EndUpdate;
        for i := 0 to FSharedEditorList.Count - 1 do
          if assigned(SharedEditors[i].FEditPlugin) then
            SharedEditors[i].FEditPlugin.Enabled := True;
      end;
      for i := 0 to FSharedEditorList.Count - 1 do begin
        if SharedEditors[i].IsActiveOnNoteBook then SharedEditors[i].SourceNotebook.UpdateStatusBar;
        // HasExecutionMarks is shared through synedit => this is only needed once
        // but HasExecutionMarks must be called on each synedit, so each synedit is notified
        if (DebugBoss.State in [dsPause, dsRun]) and
           not SharedEditors[i].HasExecutionMarks and (FCodeBuffer.FileName <> '')
        then
          SharedEditors[i].FillExecutionMarks;
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
      SynEditor.BeginUndoBlock;
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
        SynEditor.EndUndoBlock;
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

      Sender.AssignTo(SynEditor.Lines,false);

      for i := 0 to SharedEditorCount-1 do
        SharedEditors[i].AfterCodeBufferReplace;
      // HasExecutionMarks is shared through synedit => this is only needed once // but HasExecutionMarks must be called on each synedit, so each synedit is notified
      for i := 0 to FSharedEditorList.Count - 1 do
        SharedEditors[i].FillExecutionMarks;
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
  SynEditor.BeginUndoBlock;
end;

procedure TSourceEditorSharedValues.EndGlobalUpdate;
begin
  dec(FInGlobalUpdate);
  if FInGlobalUpdate > 0 then exit;
  SynEditor.EndUndoBlock;
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
  SynEditor.BeginUpdate;
  try
    FCodeBuffer.Assign(SynEditor.Lines);
    FEditorStampCommitedToCodetools:=(SynEditor.Lines as TSynEditLines).TextChangeStamp;
  finally
    SynEditor.EndUpdate;
    DecreaseIgnoreCodeBufferLock;
  end;
end;

constructor TSourceEditorSharedValues.Create;
begin
  FSharedEditorList := TFPList.Create;
  FExecutionMark := nil;
  FMarksRequested := False;
  FInGlobalUpdate := 0;
end;

destructor TSourceEditorSharedValues.Destroy;
begin
  SourceEditorMarks.DeleteAllForEditorID(Self);
  CodeBuffer := nil;
  FreeAndNil(FSharedEditorList);
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
  FIsNewSharedEditor := False;
  if ASharedEditor <> nil then begin
    PageName := ASharedEditor.PageName;
    FEditor.ShareTextBufferFrom(ASharedEditor.EditorComponent);
    FEditor.Highlighter := ASharedEditor.EditorComponent.Highlighter;
  end;

  FEditPlugin := TSynEditPlugin1.Create(FEditor);
  // IMPORTANT: when you change below, don't forget updating UnbindEditor
  FEditPlugin.OnLinesInserted := @LinesInserted;
  FEditPlugin.OnLinesDeleted := @LinesDeleted;
end;

destructor TSourceEditor.Destroy;
begin
  if FInEditorChangedUpdating then begin
    debugln(['***** TSourceEditor.Destroy: FInEditorChangedUpdating was true']);
    DebugBoss.UnLockCommandProcessing;
    FInEditorChangedUpdating := False;
  end;

  if (FAOwner<>nil) and (FEditor<>nil) then begin
    UnbindEditor;
    FEditor.Visible:=false;
    FEditor.Parent:=nil;
    TSourceNotebook(FAOwner).ReleaseEditor(self);
    // free the synedit control after processing the events
    Application.ReleaseComponent(FEditor);
  end;
  FEditor:=nil;
  if (DebugBoss <> nil) and (DebugBoss.LineInfo <> nil) then
    DebugBoss.LineInfo.RemoveNotification(FLineInfoNotification);
  FLineInfoNotification.ReleaseReference;
  inherited Destroy;
  FSharedValues.RemoveSharedEditor(Self);
  if FSharedValues.SharedEditorCount = 0 then
    FreeAndNil(FSharedValues);
end;

{------------------------------G O T O   L I N E  -----------------------------}
Function TSourceEditor.GotoLine(Value: Integer): Integer;
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

procedure TSourceEditor.GetDialogPosition(Width, Height: integer;
  out Left, Top: integer);
var 
  P: TPoint;
  ABounds: TRect;
begin
  with EditorComponent do
    P := ClientToScreen(Point(CaretXPix, CaretYPix));
  ABounds := Screen.MonitorFromPoint(P).BoundsRect;
  Left := EditorComponent.ClientOrigin.X + (EditorComponent.Width - Width) div 2;
  Top := P.Y - Height - 3 * EditorComponent.LineHeight;
  if Top < ABounds.Top + 10 then
    Top := P.Y + 2 * EditorComponent.LineHeight;
  if Top + Height > ABounds.Bottom then
    Top := (ABounds.Bottom + ABounds.Top - Height) div 2;
  if Top < ABounds.Top then Top := ABounds.Top;
end;

procedure TSourceEditor.ActivateHint(ClientPos: TPoint;
  const BaseURL, TheHint: string);
var
  ScreenPos: TPoint;
begin
  if SourceNotebook=nil then exit;
  ScreenPos:=EditorComponent.ClientToScreen(ClientPos);
  SourceNotebook.ActivateHint(ScreenPos,BaseURL,TheHint);
end;

{------------------------------S T A R T  F I N D-----------------------------}
procedure TSourceEditor.StartFindAndReplace(Replace:boolean);
const
  SaveOptions = [ssoWholeWord,ssoBackwards,ssoEntireScope,ssoRegExpr,ssoRegExprMultiLine];
var
  NewOptions: TSynSearchOptions;
  ALeft,ATop:integer;
  bSelectedTextOption: Boolean;
  DlgResult: TModalResult;
begin
  LazFindReplaceDialog.ResetUserHistory;
  //debugln('TSourceEditor.StartFindAndReplace A LazFindReplaceDialog.FindText="',dbgstr(LazFindReplaceDialog.FindText),'"');
  if ReadOnly then Replace := False;
  NewOptions:=LazFindReplaceDialog.Options;
  if Replace then
    NewOptions := NewOptions + [ssoReplace, ssoReplaceAll]
  else
    NewOptions := NewOptions - [ssoReplace, ssoReplaceAll];
  NewOptions:=NewOptions-SaveOptions+InputHistories.FindOptions*SaveOptions;
  LazFindReplaceDialog.Options := NewOptions;

  // Fill in history items
  LazFindReplaceDialog.TextToFindComboBox.Items.Assign(InputHistories.FindHistory);
  LazFindReplaceDialog.ReplaceTextComboBox.Items.Assign(
                                                 InputHistories.ReplaceHistory);

  with EditorComponent do begin
    if EditorOpts.FindTextAtCursor then begin
      if SelAvail and (BlockBegin.Y = BlockEnd.Y) then begin
        //debugln('TSourceEditor.StartFindAndReplace B FindTextAtCursor SelAvail');
        LazFindReplaceDialog.FindText := SelText
      end else begin
        //debugln('TSourceEditor.StartFindAndReplace B FindTextAtCursor not SelAvail');
        LazFindReplaceDialog.FindText := GetWordAtRowCol(LogicalCaretXY);
      end;
    end else begin
      //debugln('TSourceEditor.StartFindAndReplace B not FindTextAtCursor');
      LazFindReplaceDialog.FindText:='';
    end;
  end;
  LazFindReplaceDialog.EnableAutoComplete:=InputHistories.FindAutoComplete;
  // if there is no FindText, use the most recently used FindText
  if (LazFindReplaceDialog.FindText='') and (InputHistories.FindHistory.Count > 0) then
    LazFindReplaceDialog.FindText:=InputHistories.FindHistory[0];

  GetDialogPosition(LazFindReplaceDialog.Width,LazFindReplaceDialog.Height,ALeft,ATop);
  LazFindReplaceDialog.Left:=ALeft;
  LazFindReplaceDialog.Top:=ATop;

  try
    bSelectedTextOption := (ssoSelectedOnly in LazFindReplaceDialog.Options);
    //if there are selected text and more than 1 word, automatically enable selected text option
    if EditorComponent.SelAvail
    and (EditorComponent.BlockBegin.Y<>EditorComponent.BlockEnd.Y) then
      LazFindReplaceDialog.Options := LazFindReplaceDialog.Options + [ssoSelectedOnly];

    DlgResult:=LazFindReplaceDialog.ShowModal;
    InputHistories.FindOptions:=LazFindReplaceDialog.Options*SaveOptions;
    InputHistories.FindAutoComplete:=LazFindReplaceDialog.EnableAutoComplete;
    if DlgResult = mrCancel then
      exit;
    //debugln('TSourceEditor.StartFindAndReplace B LazFindReplaceDialog.FindText="',dbgstr(LazFindReplaceDialog.FindText),'"');

    Replace:=ssoReplace in LazFindReplaceDialog.Options;
    if Replace then
      InputHistories.AddToReplaceHistory(LazFindReplaceDialog.ReplaceText);
    InputHistories.AddToFindHistory(LazFindReplaceDialog.FindText);
    InputHistories.Save;
    DoFindAndReplace(LazFindReplaceDialog.FindText, LazFindReplaceDialog.ReplaceText,
      LazFindReplaceDialog.Options);
  finally
    //Restore original find options
    if bSelectedTextOption then
      LazFindReplaceDialog.Options := LazFindReplaceDialog.Options + [ssoSelectedOnly]
    else
      LazFindReplaceDialog.Options := LazFindReplaceDialog.Options - [ssoSelectedOnly];
  end;//End try-finally
end;

procedure TSourceEditor.AskReplace(Sender: TObject; const ASearch,
  AReplace: string; Line, Column: integer; var Action: TSrcEditReplaceAction);
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
      LazFindReplaceDialog.Options - [ssoEntireScope, ssoSelectedOnly, ssoReplaceAll]
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
    SrchOptions:=LazFindReplaceDialog.Options - [ssoEntireScope, ssoSelectedOnly, ssoReplaceAll]
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
  EditorComponent.LogicalCaretXY:=LogCaret;
  EditorComponent.SearchReplace(EditorComponent.GetWordAtRowCol(LogCaret),
                                '',Flags);
  CenterCursor(True);
end;

function TSourceEditor.DoFindAndReplace(aFindText, aReplaceText: String;
  anOptions: TSynSearchOptions): integer;
var
  AText, ACaption: String;
begin
  Result:=0;
  if (ssoReplace in anOptions) and ReadOnly then begin
    DebugLn(['TSourceEditor.DoFindAndReplace Read only']);
    exit;
  end;
  if SourceNotebook<>nil then
    Manager.AddJumpPointClicked(Self);

  //debugln('TSourceEditor.DoFindAndReplace A aFindText="',dbgstr(aFindText),'" ssoEntireScope=',dbgs(ssoEntireScope in anOptions),' ssoBackwards=',dbgs(ssoBackwards in anOptions));
  try
    Result:=EditorComponent.SearchReplace(aFindText, aReplaceText, anOptions);
  except
    on E: ERegExpr do begin
      MessageDlg(lisUEErrorInRegularExpression,
        E.Message,mtError,[mbCancel],0);
      exit;
    end;
  end;

  if (Result = 0) and not (ssoReplaceAll in anOptions)
  then begin
    ACaption := lisUENotFound;
    AText := Format(lisUESearchStringNotFound, [ValidUTF8String(aFindText)]);
    MessageDlg(ACaption, AText, mtInformation, [mbOk], 0);
    Manager.DeleteLastJumpPointClicked(Self);
  end
  else begin
    CenterCursor(True);
  end;
end;

procedure TSourceEditor.OnReplace(Sender: TObject; const ASearch, AReplace:
  string; Line, Column: integer; var Action: TSynReplaceAction);
var a,x,y:integer;
  AText:AnsiString;
begin
  if FAOwner<>nil then
    TSourceNotebook(FAOwner).UpdateStatusBar;
  AText:=Format(lisUEReplaceThisOccurrenceOfWith, ['"', ASearch, '"', #13, '"',
    AReplace, '"']);

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

Procedure TSourceEditor.FocusEditor;
Begin
  {$IFDEF VerboseFocus}
  debugln('TSourceEditor.FocusEditor A ',PageName,' ',FEditor.Name);
  {$ENDIF}
  IDEWindowCreators.ShowForm(SourceNotebook,true);
  if FEditor.IsVisible then begin
    FEditor.SetFocus; // TODO: will cal EditorEnter, which does self.Activate  => maybe lock, and do here?
    FSharedValues.SetActiveSharedEditor(Self);
  end else begin
    {$IFDEF VerboseFocus}
    debugln('TSourceEditor.FocusEditor not IsVisible: ',PageName,' ',FEditor.Name);
    {$ENDIF}
  end;
  //DebugLn('TSourceEditor.FocusEditor ',dbgsName(FindOwnerControl(GetFocus)),' ',dbgs(GetFocus));
  {$IFDEF VerboseFocus}
  debugln('TSourceEditor.FocusEditor END ',PageName,' ',FEditor.Name);
  {$ENDIF}
end;

Function TSourceEditor.GetReadOnly: Boolean;
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

Procedure TSourceEditor.ProcessCommand(Sender: TObject;
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
  SourceCompletionTimer.AutoEnabled:=false;

  if (Command=ecChar) and (AChar=#27) then begin
    // close hint windows
    if (CodeContextFrm<>nil) then
      CodeContextFrm.Hide;
    if (SrcEditHintWindow<>nil) then
      SrcEditHintWindow.Hide;
  end;

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
      if FaOwner<>nil then
        Manager.AddJumpPointClicked(Self);
    end;

  ecCopy,ecCut:
    begin
      if (not FEditor.SelAvail) then begin
        // nothing selected
        if EditorOpts.CopyWordAtCursorOnCopyNone then begin
          FEditor.SelectWord;
        end;
      end;
    end;

  ecChar:
    begin
      AddChar:=true;
      //debugln(['TSourceEditor.ProcessCommand AChar="',AChar,'" AutoIdentifierCompletion=',dbgs(EditorOpts.AutoIdentifierCompletion),' Interval=',SourceCompletionTimer.Interval,' ',Dbgs(FEditor.CaretXY),' ',FEditor.IsIdentChar(aChar)]);
      if (aChar=' ') and AutoCompleteChar(aChar,AddChar,acoSpace) then begin
        // completed
      end else if (not FEditor.IsIdentChar(aChar))
      and AutoCompleteChar(aChar,AddChar,acoWordEnd) then begin
        // completed
      end else if CodeToolsOpts.IdentComplAutoStartAfterPoint then begin
        // store caret position to detect caret changes
        SourceCompletionCaretXY:=FEditor.CaretXY;
        // add the char
        inc(SourceCompletionCaretXY.x,length(AChar));
        SourceCompletionTimer.AutoEnabled:=true;
      end;
      //DebugLn(['TSourceEditor.ProcessCommand ecChar AddChar=',AddChar]);
      if not AddChar then Command:=ecNone;
    end;

  ecLineBreak:
    begin
      AddChar:=true;
      if AutoCompleteChar(aChar,AddChar,acoLineBreak) then ;
      //DebugLn(['TSourceEditor.ProcessCommand ecLineBreak AddChar=',AddChar]);
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
  //debugln('TSourceEditor.ProcessCommand B IdentCompletionTimer.AutoEnabled=',dbgs(SourceCompletionTimer.AutoEnabled));
end;

Procedure TSourceEditor.ProcessUserCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
// these are the keys above ecUserFirst
// define all extra keys here, that should not be handled by synedit
var
  Handled: boolean;
Begin
  //debugln('TSourceEditor.ProcessUserCommand A ',dbgs(Command));
  FSharedValues.SetActiveSharedEditor(Self);
  Handled:=true;

  if Manager.ActiveSourceWindow <> SourceNotebook then begin
    debugln('Warning: ActiveSourceWindow is set incorrectly');
    Manager.ActiveSourceWindow := SourceNotebook;
  end;

  case Command of

  ecContextHelp:
    FindHelpForSourceAtCursor;

  ecIdentCompletion :
    StartIdentCompletionBox(true);

  ecShowCodeContext :
    SourceNotebook.StartShowCodeContext(true);

  ecWordCompletion :
    StartWordCompletionBox(true);

  ecFind:
    StartFindAndReplace(false);

  ecFindNext:
    FindNextUTF8;

  ecFindPrevious:
    FindPrevious;

  ecIncrementalFind:
    if FSourceNoteBook<>nil then FSourceNoteBook.BeginIncrementalFind;

  ecReplace:
    StartFindAndReplace(true);

  ecGotoLineNumber :
    ShowGotoLineDialog;

  ecFindNextWordOccurrence:
    FindNextWordOccurrence(true);

  ecFindPrevWordOccurrence:
    FindNextWordOccurrence(false);

  ecSelectionEnclose:
    EncloseSelection;

  ecSelectionUpperCase:
    UpperCaseSelection;

  ecSelectionLowerCase:
    LowerCaseSelection;

  ecSelectionSwapCase:
    SwapCaseSelection;

  ecSelectionTabs2Spaces:
    TabsToSpacesInSelection;

  ecSelectionComment:
    CommentSelection;

  ecSelectionUnComment:
    UncommentSelection;

  ecToggleComment:
    ToggleCommentSelection;

  ecSelectionEncloseIFDEF:
    ConditionalSelection;

  ecSelectionSort:
    SortSelection;

  ecSelectionBreakLines:
    BreakLinesInSelection;

  ecInvertAssignment:
    InvertAssignment;

  ecSelectToBrace:
    SelectToBrace;

  ecSelectCodeBlock:
    SelectCodeBlock;

  ecSelectLine:
    SelectLine;

  ecSelectWord:
    SelectWord;

  ecSelectParagraph:
    SelectParagraph;

  ecInsertCharacter:
    InsertCharacterFromMap;

  ecInsertGPLNotice:
    InsertGPLNotice(comtDefault);

  ecInsertLGPLNotice:
    InsertLGPLNotice(comtDefault);

  ecInsertModifiedLGPLNotice:
    InsertModifiedLGPLNotice(comtDefault);

  ecInsertUserName:
    InsertUsername;

  ecInsertDateTime:
    InsertDateTime;

  ecInsertChangeLogEntry:
    InsertChangeLogEntry;

  ecInsertCVSAuthor:
    InsertCVSKeyword('Author');

  ecInsertCVSDate:
    InsertCVSKeyword('Date');

  ecInsertCVSHeader:
    InsertCVSKeyword('Header');

  ecInsertCVSID:
    InsertCVSKeyword('ID');

  ecInsertCVSLog:
    InsertCVSKeyword('Log');

  ecInsertCVSName:
    InsertCVSKeyword('Name');

  ecInsertCVSRevision:
    InsertCVSKeyword('Revision');

  ecInsertCVSSource:
    InsertCVSKeyword('Source');

  ecLockEditor:
    IsLocked := not IsLocked;

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

Procedure TSourceEditor.UserCommandProcessed(Sender: TObject;
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

Procedure TSourceEditor.EditorStatusChanged(Sender: TObject;
  Changes: TSynStatusChanges);
Begin
  If Assigned(OnEditorChange) then
    OnEditorChange(Sender);
  UpdatePageName;
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
                               UpperCase(EditorComponent.SelText),
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
                               LowerCase(EditorComponent.SelText),
                               [setSelect], scamIgnore, smaKeep, smCurrent
                              );
end;

procedure TSourceEditor.SwapCaseSelection;
begin
  if ReadOnly then exit;
  if not EditorComponent.SelAvail then exit;
  FEditor.SetTextBetweenPoints(FEditor.BlockBegin, FEditor.BlockEnd,
                               SwapCase(EditorComponent.SelText),
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
  BeginUndoBlock;
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

  EndUndoBlock;
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
  FEditor.BeginUndoBlock;
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
  FEditor.SelText:=AddConditional(EditorComponent.SelText,IsPascal);
  FEditor.EndUndoBlock;
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
  FEditor.BeginUndoBlock;
  // ToDo: replace step by step to keep bookmarks and breakpoints
  OldSelection:=EditorComponent.SelText;
  FEditor.SelText:=BreakLinesInText(OldSelection,FEditor.RightEdge);
  FEditor.EndUndoBlock;
  FEditor.EndUpdate;
end;

procedure TSourceEditor.InvertAssignment;
begin
  if ReadOnly then exit;
  if not EditorComponent.SelAvail then exit;
  FEditor.BeginUpdate;
  FEditor.BeginUndoBlock;
  // ToDo: replace step by step to keep bookmarks and breakpoints
  FEditor.SelText := InvertAssignTool.InvertAssignment(FEditor.SelText);
  FEditor.EndUndoBlock;
  FEditor.EndUpdate;
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
  Result:=IDEProcs.CommentText(Txt,CommentType);
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
  Txt:=CommentText(LCLProc.BreakString(
           Format(Notice,[#13#13,#13#13,#13#13,#13#13,#13#13]),
           FEditor.RightEdge-2,0),CommentType);
  FEditor.InsertTextAtCaret(Txt);
end;

procedure TSourceEditor.InsertGPLNotice(CommentType: TCommentType);
begin
  InsertLicenseNotice(lisGPLNotice, CommentType);
end;

procedure TSourceEditor.InsertLGPLNotice(CommentType: TCommentType);
begin
  InsertLicenseNotice(lisLGPLNotice, CommentType);
end;

procedure TSourceEditor.InsertModifiedLGPLNotice(CommentType: TCommentType);
begin
  InsertLicenseNotice(lisModifiedLGPLNotice, CommentType);
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

procedure TSourceEditor.FindHelpForSourceAtCursor;
begin
  //DebugLn('TSourceEditor.FindHelpForSourceAtCursor A');
  ShowHelpOrErrorForSourcePosition(Filename,FEditor.LogicalCaretXY);
end;

procedure TSourceEditor.OnGutterClick(Sender: TObject; X, Y, Line: integer;
  mark: TSynEditMark);
var
  Marks: PSourceMark;
  i, MarkCount: Integer;
  BreakFound: Boolean;
  Ctrl: Boolean;
  ABrkPoint: TIDEBreakPoint;
  Mrk: TSourceMark;
begin
  // create or delete breakpoint
  // find breakpoint mark at line
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
      DebugBoss.DoCreateBreakPoint(Filename, Line, True, ABrkPoint);
      if Ctrl and (ABrkPoint <> nil)
      then ABrkPoint.Enabled := False;
    finally
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
begin
  if (ASyntaxHighlighterType=fSyntaxHighlighterType)
  and ((FEditor.Highlighter<>nil) = EditorOpts.UseSyntaxHighlight) then exit;

  if EditorOpts.UseSyntaxHighlight
  then begin
    if Highlighters[ASyntaxHighlighterType]=nil then begin
      Highlighters[ASyntaxHighlighterType]:=
        EditorOpts.CreateSyn(ASyntaxHighlighterType);
    end;
    FEditor.Highlighter:=Highlighters[ASyntaxHighlighterType];
  end
  else
    FEditor.Highlighter:=nil;

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

Function TSourceEditor.RefreshEditorSettings: Boolean;
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
end;

Procedure TSourceEditor.ccAddMessage(Texts: String);
Begin
  ErrorMsgs.Add(Texts);
End;

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
    if (AnsiCompareText(AToken,SrcToken)=0)
    and (Manager.CodeTemplateModul.CompletionAttributes[i].IndexOfName(CatName)>=0)
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
      FEditor.BeginUndoBlock;
      FEditor.InsertTextAtCaret(s);
      FEditor.LogicalCaretXY:=aTextPos;
      FEditor.EndUndoBlock;
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
    if not LazarusIDE.SaveSourceEditorChangesToCodeCache(self) then exit;
    FEditor.BeginUndoBlock;
    FEditor.BeginUpdate;
    try
      if not CodeToolBoss.CompleteBlock(CodeBuffer,p.X,p.Y,true) then exit;
    finally
      FEditor.EndUpdate;
      FEditor.EndUndoBlock;
    end;
  end;
end;

procedure TSourceEditor.AutoCompleteBlock;
var
  XY: TPoint;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
begin
  if not LazarusIDE.SaveSourceEditorChangesToCodeCache(self) then exit;
  XY:=FEditor.LogicalCaretXY;
  FEditor.BeginUndoBlock;
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
    FEditor.EndUndoBlock;
  end;
end;

procedure TSourceEditor.UpdateNoteBook(const ANewNoteBook: TSourceNotebook; ANewPage: TTabSheet);
begin
  if FSourceNoteBook = ANewNoteBook then exit;

  FSourceNoteBook := ANewNoteBook;
  FAOwner := ANewNoteBook;
  FPageName := ANewNoteBook.NoteBookPages[ANewNoteBook.NoteBookIndexOfPage(ANewPage)];

  // Change the Owner of the SynEdit
  EditorComponent.Owner.RemoveComponent(EditorComponent);
  FSourceNoteBook.InsertComponent(EditorComponent);
  // And the Parent
  EditorComponent.Parent := ANewPage;
end;

{ AOwner is the TSourceNotebook
  AParent is a page of the TPageControl }
Procedure TSourceEditor.CreateEditor(AOwner: TComponent; AParent: TWinControl);
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
      OnPaste:=@EditorPaste;
      OnEnter:=@EditorEnter;
      OnPlaceBookmark := @EditorPlaceBookmark;
      OnClearBookmark := @EditorClearBookmark;
      OnChangeUpdating  := @EditorChangeUpdating;
      RegisterMouseActionExecHandler(@EditorHandleMouseAction);
      // IMPORTANT: when you change above, don't forget updating UnbindEditor
      Parent := AParent;
    end;
    Manager.CodeTemplateModul.AddEditor(FEditor);
    Manager.NewEditorCreated(self);
    FEditor.TemplateEdit.OnActivate := @EditorActivateSyncro;
    FEditor.TemplateEdit.OnDeactivate := @EditorDeactivateSyncro;
    bmp := CreateBitmapFromLazarusResource('tsynsyncroedit');
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

procedure TSourceEditor.StartIdentCompletionBox(JumpToError: boolean);
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

  Completion.Execute(TextS2, CompletionRect);
  {$IFDEF VerboseIDECompletionBox}
  debugln(['TSourceEditor.StartIdentCompletionBox END Completion.TheForm.Visible=',Completion.TheForm.Visible]);
  {$ENDIF}
end;

procedure TSourceEditor.StartWordCompletionBox(JumpToError: boolean);
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

Function TSourceEditor.GetSource: TStrings;
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
  NewPageName: String;
begin
  if SourceNotebook.FUpdateLock > 0 then begin
    include(SourceNotebook.FUpdateFlags, ufPageNames);
    exit;
  end;
  p:=SourceNotebook.FindPageWithEditor(Self);
  if EditorOpts.ShowTabNumbers and (p < 10) then
    // Number pages 1, ..., 9, 0 -- according to Alt+N hotkeys.
    NewPageName:=Format('%s:%d', [FPageName, (p+1) mod 10])
  else
    NewPageName:=FPageName;
  if IsLocked then NewPageName:='#'+NewPageName;
  if Modified then NewPageName:='*'+NewPageName;
  if SourceNotebook.NoteBookPages[p] <> NewPageName then begin
    SourceNotebook.NoteBookPages[p] := NewPageName;
    SourceNotebook.UpdateTabsAndPageTitle;
  end;
end;

Procedure TSourceEditor.SetSource(value: TStrings);
Begin
  FEditor.Lines.Assign(Value);
end;

Function TSourceEditor.GetCurrentCursorXLine: Integer;
Begin
  Result := FEditor.CaretX
end;

Procedure TSourceEditor.SetCurrentCursorXLine(num: Integer);
Begin
  FEditor.CaretX := Num;
end;

Function TSourceEditor.GetCurrentCursorYLine: Integer;
Begin
  Result := FEditor.CaretY;
end;

Procedure TSourceEditor.SetCurrentCursorYLine(num: Integer);
Begin
  FEditor.CaretY := Num;
end;

Procedure TSourceEditor.SelectText(const StartPos, EndPos: TPoint);
Begin
  FEditor.BlockBegin := StartPos;
  FEditor.BlockEnd := EndPos;
end;

procedure TSourceEditor.ReplaceLines(StartLine, EndLine: integer;
  const NewText: string; aKeepMarks: Boolean = False);
var
  mm: TSynMarksAdjustMode;
begin
  if ReadOnly then Exit;
  mm := smaMoveUp;
  if aKeepMarks then
    mm := smaKeep;
  FEditor.SetTextBetweenPoints(Point(1,StartLine), Point(length(FEditor.Lines[Endline-1])+1,EndLine),
                               NewText, [], scamEnd, mm);
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
                       FEditor.BlockIndent,
                       NewSelection,NewCaretXY);
  //debugln(['TSourceEditor.EncloseSelection A NewCaretXY=',NewCaretXY.X,',',NewCaretXY.Y,' "',NewSelection,'"']);
  FEditor.SelText:=NewSelection;
  FEditor.LogicalCaretXY:=NewCaretXY;
end;

Function TSourceEditor.GetModified: Boolean;
Begin
  Result := FSharedValues.Modified;
end;

procedure TSourceEditor.SetModified(const NewValue: Boolean);
begin
  FSharedValues.SetModified(NewValue);
end;

Function TSourceEditor.GetInsertMode: Boolean;
Begin
  Result := FEditor.Insertmode;
end;

Function TSourceEditor.Close: Boolean;
Begin
  Result := True;
  Visible := False;
  Manager.EditorRemoved(Self);
  UnbindEditor;
  FEditor.Parent:=nil;
  if FSharedValues.SharedEditorCount = 1 then
    CodeBuffer := nil;
end;

procedure TSourceEditor.BeginUndoBlock;
begin
  FEditor.BeginUndoBlock;
end;

procedure TSourceEditor.EndUndoBlock;
begin
  FEditor.EndUndoBlock;
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
  If AnUpdating then begin
    //if FInEditorChangedUpdating then
    //  debugln(['***** TSourceEditor.EditorChangeUpdating: Updating=True, but FInEditorChangedUpdating was true already']);
    if not FInEditorChangedUpdating then
      DebugBoss.LockCommandProcessing;
    FInEditorChangedUpdating := True;
    FMouseActionPopUpMenu := nil;
  end else
  begin
    //if not FInEditorChangedUpdating then
    //  debugln(['***** TSourceEditor.EditorChangeUpdating: Updating=False, but FInEditorChangedUpdating was false already']);
    if FInEditorChangedUpdating then
      DebugBoss.UnLockCommandProcessing;
    FInEditorChangedUpdating := False;
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
    else
      FMouseActionPopUpMenu := PopupMenu;
  end;

  if (not FInEditorChangedUpdating) and (FMouseActionPopUpMenu <> nil) then begin
    FMouseActionPopUpMenu.PopupComponent := FEditor;
    FMouseActionPopUpMenu.PopUp;
    FMouseActionPopUpMenu := nil;
  end;
end;

Procedure TSourceEditor.EditorMouseMoved(Sender: TObject;
  Shift: TShiftState; X,Y: Integer);
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

Procedure TSourceEditor.EditorMouseDown(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseDown) then
    OnMouseDown(Sender, Button, Shift, X,Y);
end;

Procedure TSourceEditor.EditorKeyDown(Sender: TObject; var Key: Word; Shift :
  TShiftState);
begin
  //DebugLn('TSourceEditor.EditorKeyDown A ',dbgsName(Sender),' ',IntToStr(Key));
  if Assigned(OnKeyDown) then
    OnKeyDown(Sender, Key, Shift);
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
        i := LogicalToPhysicalPos(Point(Length(Lines[CaretY]) + 1, CaretY)).X;
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
        i := LogicalToPhysicalPos(Point(Length(Lines[CaretY]) + 1, CaretY)).X;
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

procedure TSourceEditor.SetLines(const AValue: TStrings);
begin
  FEditor.Lines:=AValue;
end;

function TSourceEditor.GetProjectFile: TLazProjectFile;
begin
  Result:=LazarusIDE.GetProjectFileForProjectEditor(Self);
end;

procedure TSourceEditor.UpdateProjectFile;
begin
  if Assigned(Manager) and Assigned(Manager.OnEditorMoved)
    then Manager.OnEditorMoved(self);
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
  Result:=DoFindAndReplace(LazFindReplaceDialog.FindText, LazFindReplaceDialog.ReplaceText,
    NewOptions);
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

procedure TSourceEditor.FillExecutionMarks;
var
  ASource: String;
  i, idx: integer;
  Addr: TDBGPtr;
begin
  if EditorComponent.IDEGutterMarks.HasDebugMarks then Exit;

  ASource := FileName;
  idx := DebugBoss.LineInfo.IndexOf(ASource);
  if (idx = -1) then
  begin
    if not FSharedValues.MarksRequested then
    begin
      FSharedValues.MarksRequested := True;
      DebugBoss.LineInfo.AddNotification(FLineInfoNotification);
      DebugBoss.LineInfo.Request(ASource);
    end;
    Exit;
  end;

  for i := 0 to EditorComponent.Lines.Count - 1 do
  begin
    Addr := DebugBoss.LineInfo.GetAddress(idx, i);
    if (Addr <> 0) then
      EditorComponent.IDEGutterMarks.SetDebugMarks(i, i);
  end;
  // TODO: move to SourceSyneditor
  for i := 0 to SharedEditorCount - 1 do
    SharedEditors[i].EditorComponent.IDEGutterMarks.HasDebugMarks; // update all shared editors
end;

procedure TSourceEditor.ClearExecutionMarks;
var
  i: Integer;
begin
  EditorComponent.IDEGutterMarks.ClearDebugMarks;
  FSharedValues.MarksRequested := False;
  for i := 0 to SharedEditorCount - 1 do
    SharedEditors[i].EditorComponent.IDEGutterMarks.ClearDebugMarks; // update all shared editors
  if (FLineInfoNotification <> nil) and (DebugBoss <> nil) and (DebugBoss.LineInfo <> nil) then
    DebugBoss.LineInfo.RemoveNotification(FLineInfoNotification);
end;

procedure TSourceEditor.LineInfoNotificationChange(const ASender: TObject; const ASource: String);
begin
  if ASource = FileName then
    FillExecutionMarks;
end;

function TSourceEditor.SourceToDebugLine(aLinePos: Integer): Integer;
begin
  Result := FEditor.IDEGutterMarks.SourceLineToDebugLine(aLinePos, True);
end;

function TSourceEditor.DebugToSourceLine(aLinePos: Integer): Integer;
begin
  Result := FEditor.IDEGutterMarks.DebugLineToSourceLine(aLinePos);
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
  if not CodeToolBoss.GetExpandedOperand(CodeBuffer, ACaretPos.X, ACaretPos.Y,
    Result, False)
  then
  if not CodeToolBoss.ExtractOperand(CodeBuffer, ACaretPos.X, ACaretPos.Y,
    Result, False, False, true)
  then
    Result := GetWordFromCaret(ACaretPos);
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

procedure TSourceEditor.LinesDeleted(Sender: TObject; FirstLine,
  Count: Integer);
begin
  // notify the notebook that lines were deleted.
  // marks will use this to update themselves
  if (Self = FSharedValues.SharedEditors[0]) then
    MessagesView.SrcEditLinesInsertedDeleted(Filename,FirstLine,-Count);
end;

procedure TSourceEditor.LinesInserted(Sender: TObject; FirstLine,
  Count: Integer);
begin
  // notify the notebook that lines were Inserted.
  // marks will use this to update themselves
  if (Self = FSharedValues.SharedEditors[0]) then
    MessagesView.SrcEditLinesInsertedDeleted(Filename,FirstLine,Count);
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
    OnEnter := nil;
    OnPlaceBookmark := nil;
    OnClearBookmark := nil;
    OnChangeUpdating := nil;
    UnregisterMouseActionExecHandler(@EditorHandleMouseAction);
  end;
  for i := 0 to EditorComponent.PluginCount - 1 do
    if EditorComponent.Plugin[i] is TSynPluginSyncronizedEditBase then begin
      TSynPluginSyncronizedEditBase(EditorComponent.Plugin[i]).OnActivate := nil;
      TSynPluginSyncronizedEditBase(EditorComponent.Plugin[i]).OnDeactivate := nil;
    end;
  if FEditPlugin<>nil then begin
    FEditPlugin.OnLinesInserted := nil;
    FEditPlugin.OnLinesDeleted := nil;
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
  inherited Create(AOwner);
  FManager := TSourceEditorManager(AOwner);
  FUpdateLock := 0;
  FFocusLock := 0;
  Visible:=false;
  FIsClosing := False;
  i := 2;
  n := Owner.FindComponent(NonModalIDEWindowNames[nmiwSourceNoteBookName]);
  if (n <> nil) and (n <> self) then begin
    while Owner.FindComponent(NonModalIDEWindowNames[nmiwSourceNoteBookName]+IntToStr(i)) <> nil do
      inc(i);
    Name := NonModalIDEWindowNames[nmiwSourceNoteBookName] + IntToStr(i);
  end
  else
    Name := NonModalIDEWindowNames[nmiwSourceNoteBookName];

  if Manager.SourceWindowCount > 0 then
    FBaseCaption := locWndSrcEditor + ' (' + IntToStr(Manager.SourceWindowCount+1) + ')'
  else
    FBaseCaption := locWndSrcEditor;
  Caption := FBaseCaption;
  KeyPreview:=true;
  FProcessingCommand := false;

  FSourceEditorList := TList.Create;
  FHistoryList := TList.create;

  // key mapping
  FKeyStrokes:=TSynEditKeyStrokes.Create(Self);
  EditorOpts.KeyMap.AssignTo(FKeyStrokes,TSourceEditorWindowInterface);

  // popup menu
  BuildPopupMenu;

  // HintTimer
  FMouseHintTimer := TIdleTimer.Create(Self);
  with FMouseHintTimer do begin
    Name:=Self.Name+'_MouseHintTimer';
    Interval := EditorOpts.AutoDelayInMSec;
    Enabled := False;
    AutoEnabled := False;
    OnTimer := @HintTimer;
  end;

  // HintWindow
  FHintWindow := THintWindow.Create(Self);
  with FHintWindow do begin
    Name:=Self.Name+'_HintWindow';
    Visible := False;
    Caption := '';
    HideInterval := 4000;
    AutoHide := False;
  end;

  FUpdateTabAndPageTimer := TTimer.Create(Self);
  FUpdateTabAndPageTimer.Interval := 500;
  FUpdateTabAndPageTimer.OnTimer := @UpdateTabsAndPageTimeReached;

  CreateNotebook;

  Application.AddOnUserInputHandler(@OnApplicationUserInput,true);
end;

destructor TSourceNotebook.Destroy;
var
  i: integer;
begin
  if assigned(Manager) then
    Manager.RemoveWindow(Self);
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TSourceNotebook.Destroy'){$ENDIF};
  FProcessingCommand:=false;

  // aWordCompletion is released in InternalFinal
  aWordCompletion.OnGetSource := nil;

  for i:=FSourceEditorList.Count-1 downto 0 do
    Editors[i].Free;
  FKeyStrokes.Free;
  FSourceEditorList.Free;
  FHistoryList.Free;

  Application.RemoveOnUserInputHandler(@OnApplicationUserInput);
  FreeThenNil(FMouseHintTimer);
  FreeThenNil(FHintWindow);
  FreeAndNil(FNotebook);

  inherited Destroy;
end;

procedure TSourceNotebook.DeactivateCompletionForm;
begin
  Manager.DeactivateCompletionForm;
end;

Procedure TSourceNotebook.CreateNotebook;
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
    if not (nbcPageListPopup in GetCapabilities) then
      PopupMenu := SrcPopupMenu;
    if EditorOpts.ShowTabCloseButtons then
      Options:=Options+[nboShowCloseButtons]
    else
      Options:=Options-[nboShowCloseButtons];
    TabPosition := EditorOpts.TabPosition;
    OnChange := @NotebookPageChanged;
    OnCloseTabClicked  := @CloseTabClicked;
    OnMouseDown:=@NotebookMouseDown;
    TabDragMode := dmAutomatic;
    OnTabDragOverEx  := @NotebookCanDragTabMove;
    OnTabDragDropEx  := @NotebookDragTabMove;
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
    DebugLn(['TSourceNotebook.EncodingClicked NewEncoding=',NewEncoding]);
    if SrcEdit.CodeBuffer<>nil then begin
      OldEncoding:=NormalizeEncoding(SrcEdit.CodeBuffer.DiskEncoding);
      if OldEncoding='' then
        OldEncoding:=GetDefaultTextEncoding;
      if NewEncoding<>SrcEdit.CodeBuffer.DiskEncoding then begin
        DebugLn(['TSourceNotebook.EncodingClicked Old=',OldEncoding,' New=',NewEncoding]);
        if SrcEdit.ReadOnly then begin
          if SrcEdit.CodeBuffer.IsVirtual then
            CurResult:=mrCancel
          else
            CurResult:=IDEQuestionDialog(lisChangeEncoding,
              Format(lisEncodingOfFileOnDiskIsNewEncodingIs, ['"',
                SrcEdit.CodeBuffer.Filename, '"', #13, OldEncoding, NewEncoding]),
              mtConfirmation, [mrOk, lisReopenWithAnotherEncoding, mrCancel], '');
        end else begin
          if SrcEdit.CodeBuffer.IsVirtual then
            CurResult:=IDEQuestionDialog(lisChangeEncoding,
              Format(lisEncodingOfFileOnDiskIsNewEncodingIs, ['"',
                SrcEdit.CodeBuffer.Filename, '"', #13, OldEncoding, NewEncoding]),
              mtConfirmation, [mrYes, lisChangeFile, mrCancel], '')
          else
            CurResult:=IDEQuestionDialog(lisChangeEncoding,
              Format(lisEncodingOfFileOnDiskIsNewEncodingIs2, ['"',
                SrcEdit.CodeBuffer.Filename, '"', #13, OldEncoding, NewEncoding]),
              mtConfirmation, [mrYes, lisChangeFile, mrOk,
                lisReopenWithAnotherEncoding, mrCancel], '');
        end;
        if CurResult=mrYes then begin
          // change file
          SrcEdit.CodeBuffer.DiskEncoding:=NewEncoding;
          SrcEdit.CodeBuffer.Modified:=true;
          // set override
          InputHistories.FileEncodings[SrcEdit.CodeBuffer.Filename]:=NewEncoding;
          DebugLn(['TSourceNotebook.EncodingClicked Change file to ',SrcEdit.CodeBuffer.DiskEncoding]);
          if (not SrcEdit.CodeBuffer.IsVirtual)
          and (LazarusIDE.DoSaveEditorFile(SrcEdit, []) <> mrOk)
          then begin
            DebugLn(['TSourceNotebook.EncodingClicked LazarusIDE.DoSaveEditorFile failed']);
          end;
        end else if CurResult=mrOK then begin
          // reopen with another encoding
          if SrcEdit.Modified then begin
            if IDEQuestionDialog(lisAbandonChanges,
              Format(lisAllYourModificationsToWillBeLostAndTheFileReopened, [
                '"', SrcEdit.CodeBuffer.Filename, '"', #13]),
              mtConfirmation,[mbOk,mbAbort],'')<>mrOk
            then begin
              exit;
            end;
          end;
          // set override
          InputHistories.FileEncodings[SrcEdit.CodeBuffer.Filename]:=NewEncoding;
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
    end;
  end;
end;

procedure TSourceNotebook.SrcPopUpMenuPopup(Sender: TObject);
var
  CurFilename, ShortFileName: String;
  ASrcEdit: TSourceEditor;

  function MaybeAddPopup(const ASuffix: String; ANewOnClick: TNotifyEvent = nil;
    Filename: string = ''): TIDEMenuItem;
  begin
    Result:=nil;
    if ANewOnClick=nil then
      ANewOnClick:=@OnPopupMenuOpenFile;
    if Filename='' then Filename:=CurFilename;
    Filename:=ChangeFileExt(Filename,ASuffix);
    if not FileExistsCached(Filename) then exit;
    Filename:=CreateRelativePath(Filename,ExtractFilePath(ASrcEdit.FileName));
    Result:=AddContextPopupMenuItem(Format(lisOpenLfm,[Filename]), true, ANewOnClick);
  end;

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
        with RegisterIDEMenuCommand(ASection, OpName+IntToStr(i), nb.Caption,
                                    OnClickMethod) do
          Tag := i;
      end;
    end;
  end;
  {$ENDIF}

var
  MarkSrcEdit, se: TSourceEditor;
  BookMarkID, BookMarkX, BookMarkY: integer;
  MarkDesc: String;
  MarkMenuItem: TIDEMenuItem;
  EditorComp: TSynEdit;
  Marks: PSourceMark;
  i, MarkCount: integer;
  NBAvail: Boolean;
  CurMark: TSourceMark;
  EditorPopupPoint, EditorCaret: TPoint;
  SelAvail, SelAvailAndWritable: Boolean;
  CurWordAtCursor: String;
  AtIdentifier: Boolean;
  MainCodeBuf: TCodeBuffer;
begin
  SourceEditorMenuRoot.MenuItem:=SrcPopupMenu.Items;
  SourceEditorMenuRoot.BeginUpdate;
  try
    RemoveUserDefinedMenuItems;
    RemoveContextMenuItems;

    ASrcEdit:=FindSourceEditorWithEditorComponent(TPopupMenu(Sender).PopupComponent);
    if ASrcEdit=nil then begin
      ASrcEdit:=GetActiveSE;
      if ASrcEdit=nil then begin
        DebugLn(['TSourceNotebook.SrcPopUpMenuPopup ASrcEdit=nil ',dbgsName(TPopupMenu(Sender).PopupComponent)]);
        exit;
      end;
    end;
    EditorComp:=ASrcEdit.EditorComponent;

    // Clipboard
    SrcEditMenuCut.Enabled := ASrcEdit.SelectionAvailable and not ASrcEdit.ReadOnly;
    SrcEditMenuCopy.Enabled := ASrcEdit.SelectionAvailable;
    SrcEditMenuPaste.Enabled := not ASrcEdit.ReadOnly;

    // Readonly, ShowLineNumbers
    SrcEditMenuReadOnly.Checked:=ASrcEdit.ReadOnly;
    SrcEditMenuShowLineNumbers.Checked := EditorComp.Gutter.LineNumberPart.Visible;
    SrcEditMenuDisableI18NForLFM.Visible:=false;

    UpdateHighlightMenuItems;
    UpdateLineEndingMenuItems;
    UpdateEncodingMenuItems;

    // bookmarks
    for BookMarkID:=0 to 9 do begin
      MarkDesc:=' '+IntToStr(BookMarkID);
      MarkSrcEdit := nil;
      i := 0;
      while i < Manager.SourceEditorCount do begin
        se:=Manager.SourceEditors[i];
        if se.EditorComponent.GetBookMark(BookMarkID,BookMarkX,BookMarkY) then
        begin
          MarkDesc:=MarkDesc+': '+se.PageName+' ('+IntToStr(BookMarkY)+','+IntToStr(BookMarkX)+')';
          break;
        end;
        inc(i);
      end;
      // goto book mark item
      MarkMenuItem:=SrcEditSubMenuGotoBookmarks[BookMarkID];
      if MarkMenuItem is TIDEMenuCommand then
        TIDEMenuCommand(MarkMenuItem).Checked:=(MarkSrcEdit<>nil);
      MarkMenuItem.Caption:=uemBookmarkN+MarkDesc;
      // set book mark item
      MarkMenuItem:=SrcEditSubMenuToggleBookmarks[BookMarkID];
      if MarkMenuItem is TIDEMenuCommand then
        TIDEMenuCommand(MarkMenuItem).Checked:=(MarkSrcEdit<>nil);
      MarkMenuItem.Caption:=uemToggleBookmark+MarkDesc;
    end;

    // editor layout
    SrcEditMenuMoveEditorLeft.MenuItem.Enabled:= (PageCount>1);
    SrcEditMenuMoveEditorRight.MenuItem.Enabled:= (PageCount>1);
    SrcEditMenuMoveEditorFirst.MenuItem.Enabled:= (PageCount>1) and (PageIndex>0);
    SrcEditMenuMoveEditorLast.MenuItem.Enabled:= (PageCount>1) and (PageIndex<(PageCount-1));

    EditorPopupPoint:=EditorComp.ScreenToClient(SrcPopUpMenu.PopupPoint);
    if EditorPopupPoint.X>EditorComp.Gutter.Width then begin
      // user clicked on text
      // collect some flags
      SelAvail:=ASrcEdit.EditorComponent.SelAvail;
      SelAvailAndWritable:=SelAvail and (not ASrcEdit.ReadOnly);
      // enable menu items
      SrcEditMenuEncloseSelection.Enabled := SelAvailAndWritable;
      SrcEditMenuEncloseInIFDEF.Enabled := SelAvailAndWritable;
      SrcEditMenuExtractProc.Enabled := SelAvailAndWritable;
      SrcEditMenuInvertAssignment.Enabled := SelAvailAndWritable;
      CurWordAtCursor:=ASrcEdit.GetWordAtCurrentCaret;
      AtIdentifier:=IsValidIdent(CurWordAtCursor);
      SrcEditMenuFindIdentifierReferences.Enabled:=AtIdentifier;
      SrcEditMenuRenameIdentifier.Enabled:=AtIdentifier and (not ASrcEdit.ReadOnly);
      SrcEditMenuShowAbstractMethods.Enabled:=not ASrcEdit.ReadOnly;
      SrcEditMenuShowEmptyMethods.Enabled:=not ASrcEdit.ReadOnly;
      SrcEditMenuFindOverloads.Enabled:=AtIdentifier;
    end else
    begin
      EditorCaret := EditorComp.PhysicalToLogicalPos(EditorComp.PixelsToRowColumn(EditorPopupPoint));
      // user clicked on gutter
      SourceEditorMarks.GetMarksForLine(ASrcEdit, EditorCaret.y, Marks, MarkCount);
      if Marks <> nil then begin
        for i := 0 to MarkCount - 1 do begin
          CurMark := Marks[i];
          CurMark.CreatePopupMenuItems(@AddUserDefinedPopupMenuItem);
        end;
        FreeMem(Marks);
      end;
    end;

    // add context specific menu items
    CurFilename:=ASrcEdit.FileName;
    ShortFileName:=ExtractFileName(CurFilename);
    MainCodeBuf:=nil;
    if (FilenameIsAbsolute(CurFilename)) then begin
      if FilenameIsPascalUnit(ShortFileName)
      or (CompareFileExt(ShortFileName,'.inc',true)=0) then
        MainCodeBuf:=CodeToolBoss.GetMainCode(ASrcEdit.CodeBuffer);
      if (MainCodeBuf<>nil) and (MainCodeBuf<>ASrcEdit.CodeBuffer)
      and (not MainCodeBuf.IsVirtual) then begin
        // this is an include file => add link to open unit
        AddContextPopupMenuItem(
          Format(lisOpenLfm,[CreateRelativePath(MainCodeBuf.Filename,ASrcEdit.Filename)]),
          true,@OnPopupMenuOpenFile);
        CurFilename:=MainCodeBuf.Filename;
        ShortFileName:=ExtractFileName(CurFilename);
      end;
      if FilenameIsPascalUnit(ShortFileName) then begin
        MaybeAddPopup('.lfm');
        MaybeAddPopup('.dfm');
        MaybeAddPopup('.lrs');
        MaybeAddPopup('.s');
      end;
      if (CompareFileExt(ShortFileName,'.lfm',true)=0)
      or (CompareFileExt(ShortFileName,'.dfm',true)=0) then begin
        MaybeAddPopup('.pas');
        MaybeAddPopup('.pp');
        MaybeAddPopup('.p');
      end;
      if (CompareFileExt(ShortFileName,'.lpi',true)=0)
      or (CompareFileExt(ShortFileName,'.lpk',true)=0) then
        AddContextPopupMenuItem(Format(lisOpenLfm,[ShortFileName]),true,@OnPopupMenuOpenFile);
    end;

    {$IFnDEF SingleSrcWindow}
    SrcEditMenuEditorLock.Checked := ASrcEdit.IsLocked;       // Editor locks
    // Multi win
    NBAvail := ToWindow(SrcEditMenuMoveToOtherWindowList, 'MoveToWindow',
                       @SrcEditMenuMoveToExistingWindowClicked);
    SrcEditMenuMoveToNewWindow.Visible := not NBAvail;
    SrcEditMenuMoveToNewWindow.Enabled := PageCount > 1;
    SrcEditMenuMoveToOtherWindow.Visible := NBAvail;
    SrcEditMenuMoveToOtherWindowNew.Enabled := (PageCount > 1);

    NBAvail := ToWindow(SrcEditMenuCopyToOtherWindowList, 'CopyToWindow',
                       @SrcEditMenuCopyToExistingWindowClicked);
    SrcEditMenuCopyToNewWindow.Visible := not NBAvail;
    SrcEditMenuCopyToOtherWindow.Visible := NBAvail;

    NBAvail := ToWindow(SrcEditMenuFindInOtherWindowList, 'FindInWindow',
                       @SrcEditMenuFindInWindowClicked, True);
    SrcEditMenuFindInOtherWindow.Enabled := NBAvail;
    {$ENDIF}

    if Assigned(Manager.OnPopupMenu) then
      Manager.OnPopupMenu(@AddContextPopupMenuItem);
    SourceEditorMenuRoot.NotifySubSectionOnShow(Self);
  finally
    SourceEditorMenuRoot.EndUpdate;
  end;
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
  TabIndex:=FNoteBook.TabIndexAtClientPos(FNotebook.ScreenToClient(Mouse.CursorPos));
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

Procedure TSourceNotebook.BuildPopupMenu;
begin
  //debugln('TSourceNotebook.BuildPopupMenu');

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

  // assign the root TMenuItem to the registered menu root.
  // This will automatically create all registered items
  {$IFDEF VerboseMenuIntf}
  SrcPopupMenu.Items.WriteDebugReport('TSourceNotebook.BuildPopupMenu ');
  SourceEditorMenuRoot.ConsistencyCheck;
  {$ENDIF}
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

procedure TSourceNotebook.SetPageIndex(const AValue: Integer);
begin
  FPageIndex := AValue;
  if FUpdateLock = 0 then begin
    FPageIndex := Max(0, Min(FPageIndex, FNotebook.PageCount-1));
    if Assigned(Manager) and (FNotebook.PageIndex = FPageIndex) then
      DoActiveEditorChanged;
    // make sure the statusbar is updated
    Include(States, snNotbookPageChangedNeeded);
    FNotebook.PageIndex := FPageIndex;
    if snNotbookPageChangedNeeded in States then
      NotebookPageChanged(nil);
    HistorySetMostRecent(FNotebook.Pages[FPageIndex]);
  end;
end;

function TSourceNotebook.GetCompletionBoxPosition: integer;
begin
  Result := Manager.CompletionBoxPosition;
end;

procedure TSourceNotebook.UpdateHighlightMenuItems;
var
  h: TLazSyntaxHighlighter;
  i: Integer;
  CurName: String;
  CurCaption: String;
  IDEMenuItem: TIDEMenuItem;
  SrcEdit: TSourceEditor;
begin
  SrcEditSubMenuHighlighter.ChildsAsSubMenu:=true;
  SrcEdit:=GetActiveSE;
  i:=0;
  for h:=Low(TLazSyntaxHighlighter) to High(TLazSyntaxHighlighter) do begin
    CurName:='Highlighter'+IntToStr(i);
    CurCaption:=LazSyntaxHighlighterNames[h];
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

procedure TSourceNotebook.UpdateLineEndingMenuItems;
var
  le: TLineEnding;
  SrcEdit: TSourceEditor;
  FileEndings: String;
  IDEMenuItem: TIDEMenuCommand;
const
  LE_Names : array [TLineEnding] of String =(
    'LF (Unix, Linux)',
    'CR (Mac)',
    'CRLF (Win, DOS)'
  );
begin
  SrcEditSubMenuLineEnding.ChildsAsSubMenu:=true;
  SrcEdit:=GetActiveSE;
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

procedure TSourceNotebook.UpdateEncodingMenuItems;
var
  List: TStringList;
  i: Integer;
  SrcEdit: TSourceEditor;
  Encoding: String;
  CurEncoding: string;
  CurName: String;
  CurCaption: String;
  IDEMenuItem: TIDEMenuItem;
  SysEncoding: String;
begin
  SrcEditSubMenuEncoding.ChildsAsSubMenu:=true;
  SrcEdit:=GetActiveSE;
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
  SrcEditMenuSectionFileDynamic.WriteDebugReport('TSourceNotebook.RemoveContextMenuItems ');
  {$ENDIF}
end;

function TSourceNotebook.AddContextPopupMenuItem(const NewCaption: string;
  const NewEnabled: boolean; const NewOnClick: TNotifyEvent): TIDEMenuItem;
begin
  Result:=RegisterIDEMenuCommand(SrcEditMenuSectionFileDynamic.GetPath,
                                 'FileDynamic',NewCaption,NewOnClick);
  Result.Enabled:=NewEnabled;
end;

{-------------------------------------------------------------------------------
  Procedure TSourceNotebook.EditorChanged
  Params: Sender: TObject
  Result: none

  Called whenever an editor status changes. Sender is normally a TSynEdit.
-------------------------------------------------------------------------------}
Procedure TSourceNotebook.EditorChanged(Sender: TObject);
var SenderDeleted: boolean;
Begin
  SenderDeleted:=(Sender as TControl).Parent=nil;
  if SenderDeleted then exit;
  UpdateStatusBar;
  if assigned(Manager) then
    Manager.DoEditorStatusChanged(FindSourceEditorWithEditorComponent(TSynEdit(Sender)));
End;

procedure TSourceNotebook.DoClose(var CloseAction: TCloseAction);
var
  Layout: TSimpleWindowLayout;
begin
  //debugln(['TSourceNotebook.DoClose ',DbgSName(Self)]);
  inherited DoClose(CloseAction);
  CloseAction := caHide;
  {$IFnDEF SingleSrcWindow}
  if (PageCount = 0) and (Parent=nil) then begin { $NOTE maybe keep the last one}
    // Make the name unique, because it may not immediately be released
    // => disconnect first
    Layout:=IDEWindowCreators.SimpleLayoutStorage.ItemByForm(Self);
    if Layout<>nil then
      Layout.Form:=nil;
    Name := Name + '___' + IntToStr(PtrUInt(Pointer(Self)));
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
end;

procedure TSourceNotebook.DoShow;
begin
  inherited DoShow;
  // statusbar was not updated when visible=false, update now
  if snUpdateStatusBarNeeded in States then
    UpdateStatusBar;
end;

function TSourceNotebook.IndexOfEditorInShareWith(AnOtherEditor: TSourceEditor
  ): Integer;
var
  i: Integer;
begin
  for i := 0 to EditorCount - 1 do
    if Editors[i].IsSharedWith(AnOtherEditor) then
      exit(i);
  Result := -1;
end;

function TSourceNotebook.GetActiveCompletionPlugin: TSourceEditorCompletionPlugin;
begin
  Result := Manager.ActiveCompletionPlugin;
end;

function TSourceNotebook.CompletionPluginCount: integer;
begin
  Result := SourceEditorManager.CompletionPluginCount;
end;

procedure TSourceNotebook.RegisterCompletionPlugin(
  Plugin: TSourceEditorCompletionPlugin);
begin
  // Deprecated; forward to manager
  SourceEditorManager.RegisterCompletionPlugin(Plugin);
end;

procedure TSourceNotebook.UnregisterCompletionPlugin(
  Plugin: TSourceEditorCompletionPlugin);
begin
  // Deprecated; forward to manager
  SourceEditorManager.UnregisterCompletionPlugin(Plugin);
end;

function TSourceNotebook.GetCompletionPlugins(Index: integer
  ): TSourceEditorCompletionPlugin;
begin
  Result := SourceEditorManager.CompletionPlugins[Index];
end;

function TSourceNotebook.NewSE(PageNum: Integer; NewPageNum: Integer = -1;
  ASharedEditor: TSourceEditor = nil; ATabCaption: String = ''): TSourceEditor;
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

procedure TSourceNotebook.AcceptEditor(AnEditor: TSourceEditor);
begin
  FSourceEditorList.Add(AnEditor);

  AnEditor.EditorComponent.BeginUpdate;
  AnEditor.PopupMenu := SrcPopupMenu;
  AnEditor.OnEditorChange := @EditorChanged;
  AnEditor.OnMouseMove := @EditorMouseMove;
  AnEditor.OnMouseDown := @EditorMouseDown;
  AnEditor.OnMouseWheel := @EditorMouseWheel;
  AnEditor.OnKeyDown := @EditorKeyDown;
  AnEditor.EditorComponent.Beautifier.OnGetDesiredIndent := @EditorGetIndent;
  AnEditor.EditorComponent.EndUpdate;
end;

procedure TSourceNotebook.ReleaseEditor(AnEditor: TSourceEditor);
begin
  FSourceEditorList.Remove(AnEditor);
end;

function TSourceNotebook.FindSourceEditorWithPageIndex(
  APageIndex: integer): TSourceEditor;
var I: integer;
  TempEditor: TControl;
begin
  Result := nil;
  if (FSourceEditorList=nil)
    or (APageIndex < 0) or (APageIndex >= PageCount) then exit;
  TempEditor:=nil;
  with NotebookPage[APageIndex] do
    for I := 0 to ControlCount-1 do
      if Controls[I] is TSynEdit then
        Begin
          TempEditor := Controls[I];
          Break;
        end;
  if TempEditor=nil then exit;
  I := FSourceEditorList.Count-1;
  while (I>=0)
  and (TSourceEditor(FSourceEditorList[I]).EditorComponent <> TempEditor) do
    dec(i);
  if i<0 then exit;
  Result := TSourceEditor(FSourceEditorList[i]);
end;

Function TSourceNotebook.GetActiveSE: TSourceEditor;
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
  if  i>= 0 then
    PageIndex := i;
  dec(FFocusLock);
  SourceEditorManager.ActiveSourceWindow := self;
end;

procedure TSourceNotebook.CheckCurrentCodeBufferChanged;
var
  SrcEdit: TSourceEditor;
begin
  // Todo: Move to manager, include window changes
  SrcEdit:=GetActiveSE;
  if SrcEdit = nil then Exit;
  if FLastCodeBuffer=SrcEdit.CodeBuffer then exit;
  FLastCodeBuffer:=SrcEdit.CodeBuffer;
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
  end;
  inc(FUpdateLock);
end;

procedure TSourceNotebook.DecUpdateLockInternal;
begin
  dec(FUpdateLock);
  if FUpdateLock = 0 then begin
    PageIndex := FPageIndex;
    if (ufPageNames in FUpdateFlags)    then UpdatePageNames;
    if (ufTabsAndPage in FUpdateFlags)  then UpdateTabsAndPageTitle;
    if (ufStatusBar in FUpdateFlags)    then UpdateStatusBar;
    if (ufProjectFiles in FUpdateFlags) then UpdateProjectFiles;
    if (ufFocusEditor in FUpdateFlags)  then FocusEditor;
    if (ufActiveEditorChanged in FUpdateFlags) then DoActiveEditorChanged;
    FUpdateFlags := [];
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
    IDEWindowCreators.ShowForm(Self,false);
    FNotebook.Visible := True;
    NotebookPages[Index] := S;
  end;
  HistoryAdd(FNotebook.Pages[Index]);
  UpdateTabsAndPageTitle;
end;

procedure TSourceNotebook.NoteBookDeletePage(APageIndex: Integer);
begin
  if PageCount > 1 then begin
    // make sure to select another page in the NoteBook, otherwise the
    // widgetset will choose one and will send a message
    // if this is the current page, switch to right APageIndex (if possible)
    //todo: determine whether we can use SetPageIndex instead
    HistoryRemove(FNotebook.Pages[APageIndex]);
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
      FNoteBook.PageIndex := FPageIndex;
    end;
    NotebookPages.Delete(APageIndex);
  end else
    FNotebook.Visible := False;
  UpdateTabsAndPageTitle;
end;

procedure TSourceNotebook.UpdateTabsAndPageTitle;
begin
  if FUpdateLock > 0 then begin
    include(FUpdateFlags, ufTabsAndPage);
    exit;
  end;
  if (PageCount = 1) and (EditorOpts.HideSingleTabInWindow) then begin
    Caption := FBaseCaption + ': ' + NotebookPages[0];
    FNotebook.ShowTabs := False;
  end else begin
    Caption := FBaseCaption;
    FNotebook.ShowTabs := True;
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
    FNotebook.ShowTabs := True;
end;

procedure TSourceNotebook.DragCanceled;
begin
  inherited DragCanceled;
  FUpdateTabAndPageTimer.Enabled := True;
end;

procedure TSourceNotebook.DoActiveEditorChanged;
begin
  if FUpdateLock > 0 then begin
    include(FUpdateFlags, ufActiveEditorChanged);
    exit;
  end;
  exclude(FUpdateFlags, ufActiveEditorChanged);
  Manager.DoActiveEditorChanged;
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

Procedure TSourceNotebook.NextEditor;
Begin
  if PageIndex < PageCount-1 then
    PageIndex := PageIndex+1
  else
    PageIndex := 0;
End;

Procedure TSourceNotebook.PrevEditor;
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

  if (PageCount = 0) and (Parent=nil) and not FIsClosing then
    Close;
end;

procedure TSourceNotebook.CopyEditor(OldPageIndex, NewWindowIndex,
  NewPageIndex: integer; Focus: Boolean = False);
var
  DestWin: TSourceNotebook;
  SrcEdit, NewEdit: TSourceEditor;
begin
  if (NewWindowIndex < 0) or (NewWindowIndex >= Manager.SourceWindowCount) then
    exit;
  DestWin := Manager.SourceWindows[NewWindowIndex];
  if DestWin = self then exit;

  if (OldPageIndex<0) or (OldPageIndex>=PageCount) or
     (NewPageIndex>DestWin.PageCount)
  then
    exit;

  SrcEdit := FindSourceEditorWithPageIndex(OldPageIndex);
  NewEdit := DestWin.NewSE(-1, NewPageIndex, SrcEdit, SrcEdit.PageName);
  NewEdit.IsNewSharedEditor := True;

  NewEdit.PageName := SrcEdit.PageName;
  NewEdit.SyntaxHighlighterType := SrcEdit.SyntaxHighlighterType;
  NewEdit.EditorComponent.TopLine := SrcEdit.EditorComponent.TopLine;
  NewEdit.EditorComponent.CaretXY := SrcEdit.EditorComponent.CaretXY;

  UpdatePageNames;
  UpdateProjectFiles;
  DestWin.UpdateProjectFiles(NewEdit);
  // Update IsVisibleTab; needs UnitEditorInfo created in DestWin.UpdateProjectFiles
  if Focus then begin
    Manager.ActiveEditor := NewEdit;
    Manager.ShowActiveWindowOnTop(True);
  end;
  DoActiveEditorChanged;
end;

procedure TSourceNotebook.ActivateHint(const ScreenPos: TPoint;
  const BaseURL, TheHint: string);
var
  HintWinRect: TRect;
  AHint: String;
begin
  if csDestroying in ComponentState then exit;
  if FHintWindow<>nil then
    FHintWindow.Visible:=false;
  if FHintWindow=nil then
    FHintWindow:=THintWindow.Create(Self);
  AHint:=TheHint;
  if LazarusHelp.CreateHint(FHintWindow,ScreenPos,BaseURL,AHint,HintWinRect) then
    FHintWindow.ActivateHint(HintWinRect,aHint);
end;

procedure TSourceNotebook.HideHint;
begin
  //DebugLn(['TSourceNotebook.HideHint ']);
  if FMouseHintTimer<>nil then
  begin
    FMouseHintTimer.AutoEnabled := false;
    FMouseHintTimer.Enabled:=false;
  end;
  if SourceCompletionTimer<>nil then
    SourceCompletionTimer.Enabled:=false;
  if FHintWindow<>nil then begin
    FHintWindow.Visible:=false;
    FHintWindow.DisableAutoSizing;
    try
      while FHintWindow.ControlCount>0 do
        FHintWindow.Controls[0].Free;
    finally
      FHintWindow.EnableAutoSizing;
    end;
  end;
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

Procedure TSourceNotebook.OpenAtCursorClicked(Sender: TObject);
begin
  if assigned(Manager) and Assigned(Manager.OnOpenFileAtCursorClicked) then
    Manager.OnOpenFileAtCursorClicked(Sender);
end;

Procedure TSourceNotebook.CutClicked(Sender: TObject);
var ActSE: TSourceEditor;
begin
  ActSE := GetActiveSE;
  if ActSE <> nil then
    ActSE.DoEditorExecuteCommand(ecCut);
end;

Procedure TSourceNotebook.CopyClicked(Sender: TObject);
var ActSE: TSourceEditor;
begin
  ActSE := GetActiveSE;
  if ActSE <> nil then
    ActSE.DoEditorExecuteCommand(ecCopy);
end;

Procedure TSourceNotebook.PasteClicked(Sender: TObject);
var ActSE: TSourceEditor;
begin
  ActSE := GetActiveSE;
  if ActSE <> nil then
    ActSE.DoEditorExecuteCommand(ecPaste);
end;

procedure TSourceNotebook.StatusBarDblClick(Sender: TObject);
var
  P: TPoint;
begin
  P := StatusBar.ScreenToClient(Mouse.CursorPos);
  // if we clicked on first panel which shows position in code
  if assigned(Manager) and (StatusBar.GetPanelIndexAt(P.X, P.Y) = 0) then
  begin
    // then show goto line dialog
    Manager.GotoLineClicked(nil);
  end;
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

procedure TSourceNotebook.FindIdentifierReferencesMenuItemClick(Sender: TObject);
begin
  MainIDEInterface.DoCommand(ecFindIdentifierRefs);
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

procedure TSourceNotebook.FindOverloadsMenuItemClick(Sender: TObject);
begin
  MainIDEInterface.DoCommand(ecFindOverloads);
end;

function TSourceNotebook.NewFile(const NewShortName: String;
  ASource: TCodeBuffer; FocusIt: boolean; AShareEditor: TSourceEditor = nil): TSourceEditor;
var
  s: String;
Begin
  //create a new page
  {$IFDEF IDE_DEBUG}
  debugln('[TSourceNotebook.NewFile] A ');
  {$ENDIF}
  // Debugger cause ProcessMessages, which could lead to entering methods in unexpected order
  DebugBoss.LockCommandProcessing;
  try
    DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TSourceNotebook.NewFile'){$ENDIF};
    try
      IDEWindowCreators.ShowForm(Self,false);
      s := Manager.FindUniquePageName(NewShortName, AShareEditor);
      Result := NewSE(-1, -1, AShareEditor, s);
      {$IFDEF IDE_DEBUG}
      debugln('[TSourceNotebook.NewFile] B ');
      {$ENDIF}
      Result.CodeBuffer:=ASource;
      {$IFDEF IDE_DEBUG}
      debugln('[TSourceNotebook.NewFile] D ');
      {$ENDIF}
      //debugln(['TSourceNotebook.NewFile ',NewShortName,' ',ASource.Filename]);
      Result.PageName:= s;
      UpdatePageNames;
      UpdateProjectFiles(Result);
      UpdateStatusBar;
    finally
      EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TSourceNotebook.NewFile'){$ENDIF};
    end;
    if FocusIt then FocusEditor;
  finally
    DebugBoss.UnLockCommandProcessing;
  end;
  {$IFDEF IDE_DEBUG}
  debugln('[TSourceNotebook.NewFile] end');
  {$ENDIF}
  CheckFont;
end;

procedure TSourceNotebook.CloseFile(APageIndex:integer);
var
  TempEditor: TSourceEditor;
  WasSelected: Boolean;
begin
  (* Do not use DisableAutoSizing in here, if a new Editor is focused it needs immediate autosize (during handle creation) *)
  {$IFDEF IDE_DEBUG}
  debugln(['TSourceNotebook.CloseFile A  APageIndex=',APageIndex]);
  {$ENDIF}
  TempEditor:=FindSourceEditorWithPageIndex(APageIndex);
  if TempEditor=nil then exit;
  WasSelected:=PageIndex=APageIndex;
  //debugln(['TSourceNotebook.CloseFile ',TempEditor.FileName,' ',TempEditor.APageIndex]);
  EndIncrementalFind;
  TempEditor.Close;
  TempEditor.Free;
  TempEditor:=nil;
  // delete the page
  //debugln('TSourceNotebook.CloseFile B  APageIndex=',APageIndex,' PageCount=',PageCount,' NoteBook.APageIndex=',Notebook.APageIndex);
  NoteBookDeletePage(APageIndex);
  //debugln('TSourceNotebook.CloseFile C  APageIndex=',APageIndex,' PageCount=',PageCount,' NoteBook.APageIndex=',Notebook.APageIndex);
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
    TempEditor.EditorComponent.SetFocus;
  {$IFDEF IDE_DEBUG}
  debugln('TSourceNotebook.CloseFile END');
  {$ENDIF}
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

procedure TSourceNotebook.CloseTabClicked(Sender: TObject);
begin
  CloseClicked(Sender,
              (GetKeyState(VK_CONTROL) < 0) and EditorOpts.CtrlMiddleTabClickClosesOthers);
end;

function TSourceNotebook.GetEditors(Index:integer):TSourceEditor;
begin
  Result:=TSourceEditor(FSourceEditorList[Index]);
end;

function TSourceNotebook.EditorCount:integer;
begin
  Result:=FSourceEditorList.Count;
end;

function TSourceNotebook.IndexOfEditor(aEditor: TSourceEditorInterface
  ): integer;
begin
  Result := FSourceEditorList.IndexOf(aEditor);
end;

function TSourceNotebook.Count: integer;
begin
  Result:=FSourceEditorList.Count;
end;

Procedure TSourceNotebook.CloseClicked(Sender: TObject; CloseOthers: Boolean = False);
Begin
  if assigned(Manager) and Assigned(Manager.OnCloseClicked) then
    Manager.OnCloseClicked(Sender, CloseOthers);
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

procedure TSourceNotebook.HistoryAdd(APage: TTabSheet);
begin
  FHistoryList.Add(APage);
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

Procedure TSourceNotebook.UpdateStatusBar;
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
    StatusBar.Panels[1].Text := PanelFileMode;
    Statusbar.Panels[2].Text := PanelCharMode;
    Statusbar.Panels[3].Text := PanelFilename;
  end;
  Statusbar.EndUpdate;

  CheckCurrentCodeBufferChanged;
End;

function TSourceNotebook.FindPageWithEditor(
  ASourceEditor: TSourceEditor):integer;
begin
  if (ASourceEditor.EditorComponent.Parent is TTabSheet) and
     (TTabSheet(ASourceEditor.EditorComponent.Parent).Parent = FNotebook)
  then
    Result:=TTabSheet(ASourceEditor.EditorComponent.Parent).PageIndex
  else
    Result:=-1;
end;

function TSourceNotebook.FindSourceEditorWithEditorComponent(
  EditorComp: TComponent): TSourceEditor;
var i: integer;
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
  if (Button = mbMiddle) then begin
    TabIndex:=FNotebook.TabIndexAtClientPos(Point(X,Y));
    if TabIndex>=0 then
      CloseClicked(NoteBookPage[TabIndex],
                   (GetKeyState(VK_CONTROL) < 0) and EditorOpts.CtrlMiddleTabClickClosesOthers);
  end;
end;

procedure TSourceNotebook.NotebookDragTabMove(Sender, Source: TObject; OldIndex,
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

procedure TSourceNotebook.NotebookCanDragTabMove(Sender, Source: TObject;
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
  FUpdateTabAndPageTimer.Enabled := False;
  if State = dsDragLeave then
    FUpdateTabAndPageTimer.Enabled := True
  else if Source is TExtendedNotebook then
    FNotebook.ShowTabs := True;
end;

procedure TSourceNotebook.NotebookEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  FUpdateTabAndPageTimer.Enabled := True;
end;

Procedure TSourceNotebook.NotebookPageChanged(Sender: TObject);
var
  TempEditor:TSourceEditor;
  CaretXY: TPoint;
  TopLine: Integer;
Begin
  if (not assigned(Manager)) or (FUpdateLock > 0) Then begin
    Include(States, snNotbookPageChangedNeeded);
    exit;
  end;
  Exclude(States, snNotbookPageChangedNeeded);
  TempEditor:=GetActiveSE;

  //debugln('TSourceNotebook.NotebookPageChanged ',Pageindex,' ',TempEditor <> nil,' fAutoFocusLock=',fAutoFocusLock);
  if TempEditor <> nil then
  begin
    if not TempEditor.Visible then begin
      // As long as SynEdit had no Handle, it had kept all those Values untouched
      CaretXY := TempEditor.EditorComponent.CaretXY;
      TopLine := TempEditor.EditorComponent.TopLine;
      TempEditor.BeginUpdate;
      TempEditor.Visible := True;
      TempEditor.EndUpdate;
      // Restore the intial Positions, must be after lock
      TempEditor.EditorComponent.LeftChar := 1;
      TempEditor.EditorComponent.CaretXY := CaretXY;
      TempEditor.EditorComponent.TopLine := TopLine;
    end;
    if (fAutoFocusLock=0) and (Screen.ActiveCustomForm=GetParentForm(Self)) and
       not(Manager.HasAutoFocusLock)
    then
    begin
      {$IFDEF VerboseFocus}
      debugln('TSourceNotebook.NotebookPageChanged BEFORE SetFocus ',
        TempEditor.EditorComponent.Name,' ',
        NoteBookPages[FindPageWithEditor(TempEditor)]);
      {$ENDIF}
      TempEditor.FocusEditor;
      {$IFDEF VerboseFocus}
      debugln('TSourceNotebook.NotebookPageChanged AFTER SetFocus ',
        TempEditor.EditorComponent.Name,' ',
        NotebookPages[FindPageWithEditor(TempEditor)]);
      {$ENDIF}
    end;
    UpdateStatusBar;
    UpdateActiveEditColors(TempEditor.EditorComponent);
    if (DebugBoss.State in [dsPause, dsRun]) and
       not TempEditor.HasExecutionMarks and
       (TempEditor.FileName <> '') then
      TempEditor.FillExecutionMarks;
    DoActiveEditorChanged;
  end;

  CheckCurrentCodeBufferChanged;
end;

Procedure TSourceNotebook.ProcessParentCommand(Sender: TObject;
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

  ecNextEditor:
    NextEditor;

  ecPrevEditor :
    PrevEditor;

  ecMoveEditorLeft:
    MoveActivePageLeft;

  ecMoveEditorRight:
    MoveActivePageRight;

  ecMoveEditorLeftmost:
    MoveActivePageFirst;

  ecMoveEditorRightmost:
    MoveActivePageLast;

  ecNextSharedEditor:
    GotoNextSharedEditor(False);
  ecPrevSharedEditor:
    GotoNextSharedEditor(True);
  ecNextWindow:
    GotoNextWindow(False);
  ecPrevWindow:
    GotoNextWindow(True);
  ecMoveEditorNextWindow:
    MoveEditorNextWindow(False, False);
  ecMoveEditorPrevWindow:
    MoveEditorNextWindow(True, False);
  ecMoveEditorNewWindow:
    if EditorCount > 1 then
      MoveEditor(FindPageWithEditor(GetActiveSE), Manager.IndexOfSourceWindow(Manager.CreateNewWindow(True)), -1);
  ecCopyEditorNextWindow:
    MoveEditorNextWindow(False, True);
  ecCopyEditorPrevWindow:
    MoveEditorNextWindow(True, True);
  ecCopyEditorNewWindow:
    CopyEditor(FindPageWithEditor(GetActiveSE), Manager.IndexOfSourceWindow(Manager.CreateNewWindow(True)), -1, True);


  ecOpenFileAtCursor:
    OpenAtCursorClicked(self);

  ecGotoEditor1..ecGotoEditor9,ecGotoEditor0:
    if PageCount>Command-ecGotoEditor1 then
      PageIndex:=Command-ecGotoEditor1;

  ecToggleFormUnit:
    ToggleFormUnitClicked(Self);

  ecToggleObjectInsp:
    ToggleObjectInspClicked(Self);

  ecSetFreeBookmark:
    if Assigned(Manager.OnSetBookmark) then
      Manager.OnSetBookmark(GetActiveSE, -1, False);

  ecJumpBack:
    Manager.HistoryJump(Self,jhaBack);

  ecJumpForward:
    Manager.HistoryJump(Self,jhaForward);

  ecAddJumpPoint:
    Manager.AddJumpPointClicked(Self);

  ecViewJumpHistory:
    Manager.ViewJumpHistoryClicked(Self);

  else
    Handled:=ExecuteIDECommand(Self,Command);
    DebugLn('TSourceNotebook.ProcessParentCommand Command=',dbgs(Command),' Handled=',dbgs(Handled));
  end;  //case
  if Handled then Command:=ecNone;
  FProcessingCommand:=false;
end;

Procedure TSourceNotebook.ParentCommandProcessed(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  var Handled: boolean);
begin
  if assigned(Manager) and Assigned(Manager.OnUserCommandProcessed) then begin
    Handled:=false;
    Manager.OnUserCommandProcessed(Self,Command,Handled);
    if Handled then exit;
  end;

  Handled:=(Command=ecClose);

  if Handled then Command:=ecNone;
end;

Procedure TSourceNotebook.ReloadEditorOptions;
var
  I: integer;
Begin
  for i := 0 to EditorCount-1 do
    Editors[i].RefreshEditorSettings;

  EditorOpts.KeyMap.AssignTo(FKeyStrokes,TSourceEditorWindowInterface);
  if EditorOpts.ShowTabCloseButtons then
    FNoteBook.Options:=FNoteBook.Options+[nboShowCloseButtons]
  else
    FNoteBook.Options:=FNoteBook.Options-[nboShowCloseButtons];
  FNotebook.TabPosition := EditorOpts.TabPosition;

  FMouseHintTimer.Interval:=EditorOpts.AutoDelayInMSec;

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
    DummyResult:=QuestionDlg(lisUEFontWith,
      Format(lisUETheCurre, [#13, #13]),
      mtWarning, [mrIgnore, mrYesToAll, lisUEDoNotSho], 0);
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

procedure TSourceNotebook.KeyDownBeforeInterface(var Key: Word;
  Shift: TShiftState);
var i, Command: integer;
Begin
  inherited KeyDown(Key,Shift);
  if not assigned(Manager) then exit;
  i := FKeyStrokes.FindKeycode(Key, Shift);
  if i>=0 then begin
    Command:=FKeyStrokes[i].Command;
    case Command of

    ecGotoMarker0..ecGotoMarker9:
      begin
        if Assigned(Manager.OnGotoBookmark) then
          Manager.OnGotoBookmark(ActiveEditor, Command - ecGotoMarker0, False);
        Key:=0;
      end;

    ecSetMarker0..ecSetMarker9:
      begin
        if Assigned(Manager.OnSetBookmark) then
          Manager.OnSetBookmark(GetActiveSE, Command - ecSetMarker0, False);
        Key:=0;
      end;

    ecToggleMarker0..ecToggleMarker9:
      begin
        if Assigned(Manager.OnSetBookmark) then
          Manager.OnSetBookmark(GetActiveSE, Command - ecToggleMarker0, True);
        Key:=0;
      end;

    ecClose:
      begin
        CloseClicked(Self);
        Key:=0;
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

Procedure TSourceNotebook.EditorMouseMove(Sender: TObject; Shift: TShiftstate;
  X,Y: Integer);
begin
  HideHint;
  if not Visible then exit;
  if (MainIDEInterface.ToolStatus=itDebugger) then
    FMouseHintTimer.AutoEnabled := EditorOpts.AutoToolTipExprEval or EditorOpts.AutoToolTipSymbTools
  else
    FMouseHintTimer.AutoEnabled := EditorOpts.AutoToolTipSymbTools;
end;

procedure TSourceNotebook.EditorMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  HideHint;
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
  SrcEdit := FindSourceEditorWithEditorComponent(TComponent(Editor));
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

Procedure TSourceNotebook.HintTimer(sender: TObject);
var
  MousePos: TPoint;
  AControl: TControl;
begin
  FMouseHintTimer.Enabled := False;
  FMouseHintTimer.AutoEnabled := False;
  if not IsVisible then exit;
  MousePos := Mouse.CursorPos;
  AControl:=FindLCLControl(MousePos);
  if (AControl=nil) or (not ContainsControl(AControl)) then exit;
  if AControl is TSynEdit then
    ShowSynEditHint(MousePos);
end;

{------------------------------------------------------------------------------
  procedure TSourceNotebook.OnApplicationUserInput(Sender: TObject;
    Msg: Cardinal);
------------------------------------------------------------------------------}
procedure TSourceNotebook.OnApplicationUserInput(Sender: TObject; Msg: Cardinal);
begin
  //debugln('TSourceNotebook.OnApplicationUserInput');
  HideHint;
end;

procedure TSourceNotebook.EditorKeyDown(Sender: TObject; var Key: Word;
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
      end;

      if HintStr<>'' then
        ActivateHint(MousePos,'',HintStr);
    end;
  end else begin
    // hint for source
    if assigned(manager) and Assigned(Manager.OnShowHintForSource) then
      Manager.OnShowHintForSource(ASrcEdit,EditPos,EditCaret);
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

function TSourceNotebook.GetEditorControlSettings(EditControl: TControl
  ): boolean;
begin
  // Deprecated; forward to manager
  Result := SourceEditorManager.GetEditorControlSettings(EditControl);
end;

function TSourceNotebook.GetHighlighterSettings(Highlighter: TObject): boolean;
begin
  // Deprecated; forward to manager
  Result := SourceEditorManager.GetHighlighterSettings(Highlighter);
end;

procedure TSourceNotebook.ClearErrorLines;
var
  i: integer;
begin
  for i := 0 to EditorCount - 1 do
    Editors[i].ErrorLine := -1;
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

{ TSynEditPlugin1 }

constructor TSynEditPlugin1.Create(AOwner: TComponent);
Begin
  inherited Create(AOwner);
  FEnabled := True;
  ViewedTextBuffer.AddChangeHandler(senrLineCount, {$IFDEF FPC}@{$ENDIF}LineCountChanged);
end;

destructor TSynEditPlugin1.Destroy;
begin
  ViewedTextBuffer.RemoveChangeHandler(senrLineCount, {$IFDEF FPC}@{$ENDIF}LineCountChanged);
  inherited Destroy;
end;

procedure TSynEditPlugin1.LineCountChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
begin
  if not FEnabled then exit;
  if ACount < 0 then begin
    if Assigned(OnLinesDeleted) then
      OnLinesDeleted(self, AIndex+1, -ACount);
  end else begin
    if Assigned(OnLinesInserted) then
      OnLinesInserted(self, AIndex+1, ACount);
  end;
end;

function TSynEditPlugin1.OwnedByEditor: Boolean;
begin
  Result := True;
end;

//-----------------------------------------------------------------------------

procedure InternalInit;
var h: TLazSyntaxHighlighter;
begin
  for h:=Low(TLazSyntaxHighlighter) to High(TLazSyntaxHighlighter) do
    Highlighters[h]:=nil;
  IDESearchInText:=@SearchInText;
end;

procedure InternalFinal;
var h: TLazSyntaxHighlighter;
begin
  for h:=Low(TLazSyntaxHighlighter) to High(TLazSyntaxHighlighter) do
    FreeThenNil(Highlighters[h]);
  FreeThenNil(aWordCompletion);
end;

{ TSourceEditorManagerBase }

procedure TSourceEditorManagerBase.FreeSourceWindows;
var
  s: TSourceEditorWindowInterface;
begin
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

procedure TSourceEditorManagerBase.SetActiveSourceWindowIndex(
  const AValue: integer);
begin
  ActiveSourceWindow := SourceWindows[AValue];
end;

function TSourceEditorManagerBase.GetActiveSourceWindow: TSourceEditorWindowInterface;
begin
  Result := FActiveWindow;
end;

procedure TSourceEditorManagerBase.SetActiveSourceWindow(
  const AValue: TSourceEditorWindowInterface);
begin
  if AValue = FActiveWindow then exit;
  if (FActiveWindow <> nil) and (AValue <> nil) and (FActiveWindow.Focused) then
    AValue.SetFocus;

  FActiveWindow := AValue as TSourceNotebook;
  FSourceWindowByFocusList.Remove(AValue);
  FSourceWindowByFocusList.Insert(0, AValue);


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
  FChangeNotifyLists[semEditorStatus].CallNotifyEvents(AEditor);
end;

function TSourceEditorManagerBase.GetSourceEditors(Index: integer
  ): TSourceEditorInterface;
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

function TSourceEditorManagerBase.GetUniqueSourceEditors(Index: integer
  ): TSourceEditorInterface;
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
  for i := SourceEditorCount - 1 downto 0 do begin
    Result := SourceEditors[i];
    if CompareFilenames(Result.Filename, Filename) = 0 then exit;
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

function TSourceEditorManagerBase.GetEditorControlSettings(EditControl: TControl
  ): boolean;
begin
  Result:=true;
  if EditControl is TSynEdit then begin
    EditorOpts.GetSynEditSettings(TSynEdit(EditControl));
    Result:=true;
  end else begin
    Result:=false;
  end;
end;

function TSourceEditorManagerBase.GetHighlighterSettings(Highlighter: TObject
  ): boolean;
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

function TSourceEditorManagerBase.GetMarklingProducers(Index: integer
  ): TSourceMarklingProducer;
begin
  Result:=TSourceMarklingProducer(fProducers[Index]);
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

  // clear the IdentifierList (otherwise it would try to update everytime
  // the codetools are used)
  CodeToolBoss.IdentifierList.Clear;
  FDefaultCompletionForm.CurrentCompletionType:=ctNone;

  (* SetFocus and Deactivate will all trigger this proc to be reentered.
     Setting "CurrentCompletionType:=ctNone" ensures an immediate exit
  *)

  (* Due to a bug under XFCE we must move focus before we close the form
     This is relevant if the form is closed by enter/escape key
  *)
  if PluginFocused and (ActiveEditor<>nil) then
    TSourceEditor(ActiveEditor).FocusEditor;

  (* hide/close the form *)
  FDefaultCompletionForm.Deactivate;

  (* Ensure focus *after* the form was closed.
     This is the normal implementation (all but XFCE)
  *)
  if PluginFocused and (ActiveEditor<>nil) then
    TSourceEditor(ActiveEditor).FocusEditor;
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
  inherited;
end;

destructor TSourceEditorManagerBase.Destroy;
var
  i: integer;
  cr: TsemChangeReason;
  h: TSrcEditMangerHandlerType;
begin
  for i:=MarklingProducerCount-1 downto 0 do
    MarklingProducers[i].Free;
  FreeAndNil(fProducers);
  FActiveWindow := nil;
  FreeCompletionPlugins;
  FreeSourceWindows;
  SrcEditorIntf.SourceEditorManagerIntf := nil; // xx move down
  FreeAndNil(FCompletionPlugins);
  FreeAndNil(FSourceWindowList);
  FreeAndNil(FSourceWindowByFocusList);
  for cr := low(TsemChangeReason) to high(TsemChangeReason) do
    FreeAndNil(FChangeNotifyLists[cr]);
  for h:=low(FHandlers) to high(FHandlers) do
    FreeAndNil(FHandlers[h]);
  inherited Destroy;
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
    RaiseException('TSourceEditorManagerBase.RegisterProducer already registered');
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
  i: Integer;
  j: Integer;
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
  j: Integer;
  SrcEdit: TSourceEditor;
begin
  if aProducer=nil then exit;
  for i := 0 to SourceWindowCount - 1 do
  begin
    SrcWnd:=SourceWindows[i];
    for j:=0 to SrcWnd.Count-1 do
    begin
      SrcEdit:=TSourceEditor(SrcWnd[j]);
      if CompareFilenames(SrcEdit.FileName,aFilename)=0 then
        SrcEdit.FSharedValues.FMarklingsValid:=false;
    end;
  end;
end;

procedure TSourceEditorManagerBase.IncUpdateLockInternal;
begin
  if FUpdateLock = 0 then begin
    FUpdateFlags := [];
    // Debugger cause ProcessMessages, which could lead to entering methods in unexpected order
    DebugBoss.LockCommandProcessing;
    Screen.Cursor := crHourGlass;
  end;
  inc(FUpdateLock);
end;

procedure TSourceEditorManagerBase.DecUpdateLockInternal;
begin
  dec(FUpdateLock);
  if FUpdateLock = 0 then begin
    try
      Screen.Cursor := crDefault;
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

procedure TSourceEditorManager.SetActiveSourceNotebook(
  const AValue: TSourceNotebook);
begin
  inherited ActiveSourceWindow := AValue;
end;

function TSourceEditorManager.GetSourceNotebook(Index: integer
  ): TSourceNotebook;
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

function TSourceEditorManager.NewSourceWindow: TSourceNotebook;
begin
  Result := CreateNewWindow(True);
  ActiveSourceWindow := Result;
end;

procedure TSourceEditorManager.CreateSourceWindow(Sender: TObject;
  aFormName: string; var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  {$IFDEF VerboseIDEDocking}
  debugln(['TSourceEditorManager.CreateSourceWindow Sender=',DbgSName(Sender),' FormName="',aFormName,'"']);
  {$ENDIF}
  AForm := CreateNewWindow(false,DoDisableAutoSizing);
  AForm.Name:=aFormName;
end;

procedure TSourceEditorManager.GetDefaultLayout(Sender: TObject;
  aFormName: string; out aBounds: TRect; out DockSibling: string; out
  DockAlign: TAlign);
var
  i: LongInt;
  p: Integer;
  ScreenR: TRect;
begin
  DockSibling:='';
  DockAlign:=alNone;
  i:=StrToIntDef(
       copy(aFormName,length(NonModalIDEWindowNames[nmiwSourceNoteBookName])+1,
            length(aFormName)),0);
  {$IFDEF VerboseIDEDocking}
  debugln(['TSourceEditorManager.GetDefaultLayout ',aFormName,' i=',i]);
  {$ENDIF}
  ScreenR:=IDEWindowCreators.GetScreenrectForDefaults;
  if Application.MainForm<>nil then
    p:=Min(ScreenR.Left+200,Application.MainForm.Top+Application.MainForm.Height+25)
  else
    p:=120;
  inc(p,30*i);
  aBounds:=Rect(ScreenR.Left+250+30*i,p,
                Min(1000,ScreenR.Right-ScreenR.Left),
                ScreenR.Bottom-ScreenR.Top-200);
  if (i=0) and (IDEDockMaster<>nil) then begin
    DockSibling:=NonModalIDEWindowNames[nmiwMainIDEName];
    DockAlign:=alBottom;
  end;
end;

function TSourceEditorManager.SourceWindowWithPage(const APage: TTabSheet
  ): TSourceNotebook;
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
  FChangeNotifyLists[semEditorCreate].CallNotifyEvents(AEditor);
end;

procedure TSourceEditorManager.EditorRemoved(AEditor: TSourceEditor);
begin
  if FDefaultCompletionForm <> nil then
    FDefaultCompletionForm.RemoveEditor(AEditor.EditorComponent);
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
  i: Integer;
begin
  for i := FSourceWindowList.Count - 1 downto 0 do
    SourceWindows[i].ClearErrorLines;
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

  SourceCompletionTimer.Interval:=EditorOpts.AutoDelayInMSec;
  // reload code templates
  with CodeTemplateModul do begin
    if FileExistsUTF8(EditorOpts.CodeTemplateFilename) then
      AutoCompleteList.LoadFromFile(UTF8ToSys(EditorOpts.CodeTemplateFilename))
    else
      if FileExistsUTF8('lazarus.dci') then
        AutoCompleteList.LoadFromFile(UTF8ToSys('lazarus.dci'));
    IndentToTokenStart:=EditorOpts.CodeTemplateIndentToTokenStart;
  end;

  if FDefaultCompletionForm <> nil then begin
    FDefaultCompletionForm.LongLineHintTime := EditorOpts.CompletionLongLineHintInMSec;
    FDefaultCompletionForm.LongLineHintType := EditorOpts.CompletionLongLineHintType;
  end;
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

procedure TSourceEditorManager.JumpBackClicked(Sender: TObject);
begin
  if ActiveSourceWindow <> nil then HistoryJump(Sender,jhaBack);
end;

procedure TSourceEditorManager.JumpForwardClicked(Sender: TObject);
begin
  if ActiveSourceWindow <> nil then HistoryJump(Sender,jhaForward);
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

function TSourceEditorManager.MacroFuncPrompt(const s: string;
  const Data: PtrInt; var Abort: boolean): string;
begin
  Result:=s;
  Abort:=(ShowMacroPromptDialog(Result)<>mrOk);
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
  AMacroList.Add(TTransferMacro.Create('Prompt','',
                 lisPromptForValue,@MacroFuncPrompt,[tmfInteractive]));
end;

procedure TSourceEditorManager.SetupShortCuts;

  function GetCommand(ACommand: Word): TIDECommand; inline;
  begin
    Result := IDECommandList.FindIDECommand(ACommand);
  end;

var
  i: Integer;
begin
  {%region *** first static section *** }
    SrcEditMenuFindDeclaration.Command := GetCommand(ecFindDeclaration);
    {%region *** Submenu: Find Section *** }
      SrcEditMenuProcedureJump.Command          := GetCommand(ecFindProcedureDefinition);
      SrcEditMenuFindNextWordOccurrence.Command := GetCommand(ecFindNextWordOccurrence);
      SrcEditMenuFindPrevWordOccurrence.Command := GetCommand(ecFindPrevWordOccurrence);
      SrcEditMenuFindInFiles.Command            := GetCommand(ecFindInFiles);
    {%endregion}
  {%endregion}

  {%region *** Pages section ***}
    SrcEditMenuClosePage.Command       := GetCommand(ecClose);
    SrcEditMenuCloseOtherPages.OnClick := @SourceEditorManager.CloseOtherPagesClicked;

    {$IFnDEF SingleSrcWindow}
    SrcEditMenuEditorLock.Command           := GetCommand(ecLockEditor);
    SrcEditMenuMoveToNewWindow.Command      := GetCommand(ecMoveEditorNewWindow);
    SrcEditMenuMoveToOtherWindowNew.Command := GetCommand(ecMoveEditorNewWindow);
    SrcEditMenuCopyToNewWindow.Command      := GetCommand(ecCopyEditorNewWindow);
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
    SrcEditMenuReadOnly.OnClick          :=@ReadOnlyClicked;
    SrcEditMenuShowLineNumbers.OnClick   :=@ToggleLineNumbersClicked;
    SrcEditMenuDisableI18NForLFM.OnClick :=@ToggleI18NForLFMClicked;
    SrcEditMenuShowUnitInfo.OnClick      :=@ShowUnitInfo;
  {%endregion}

  {%region *** Clipboard section ***}
    SrcEditMenuCut.Command:=GetCommand(ecCut);
    SrcEditMenuCopy.Command:=GetCommand(ecCopy);
    SrcEditMenuPaste.Command:=GetCommand(ecPaste);
    SrcEditMenuCopyFilename.OnClick:=@CopyFilenameClicked;
  {%endregion}

  SrcEditMenuNextBookmark.Command:=GetCommand(ecNextBookmark);
  SrcEditMenuPrevBookmark.Command:=GetCommand(ecPrevBookmark);
  SrcEditMenuSetFreeBookmark.Command:=GetCommand(ecSetFreeBookmark);

  for i:=0 to 9 do begin
    TIDEMenuCommand(SrcEditSubMenuGotoBookmarks.FindByName('GotoBookmark'+IntToStr(i)))
      .Command := GetCommand(ecGotoMarker0 + i);
    TIDEMenuCommand(SrcEditSubMenuToggleBookmarks.FindByName('ToggleBookmark'+IntToStr(i)))
      .Command := GetCommand(ecToggleMarker0 + i);
  end;

  {%region *** Source Section ***}
    SrcEditMenuEncloseSelection.Command:=GetCommand(ecSelectionEnclose);
    SrcEditMenuEncloseInIFDEF.Command:=GetCommand(ecSelectionEncloseIFDEF);
    SrcEditMenuCompleteCode.Command:=GetCommand(ecCompleteCode);
    SrcEditMenuUseUnit.Command:=GetCommand(ecUseUnit);
  {%endregion}

  {%region *** Refactoring Section ***}
    SrcEditMenuRenameIdentifier.Command:=GetCommand(ecRenameIdentifier);
    SrcEditMenuFindIdentifierReferences.Command:=GetCommand(ecFindIdentifierRefs);
    SrcEditMenuExtractProc.Command:=GetCommand(ecExtractProc);
    SrcEditMenuInvertAssignment.Command:=GetCommand(ecInvertAssignment);
    SrcEditMenuShowAbstractMethods.Command:=GetCommand(ecShowAbstractMethods);
    SrcEditMenuShowEmptyMethods.Command:=GetCommand(ecRemoveEmptyMethods);
    SrcEditMenuShowUnusedUnits.Command:=GetCommand(ecRemoveUnusedUnits);
    SrcEditMenuFindOverloads.Command:=GetCommand(ecFindOverloads);
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
         (AnsiCompareText(AName, SourceEditors[a].PageName) = 0)
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

procedure TSourceEditorManager.HideHint;
var
  i: Integer;
begin
  for i := 0 to SourceWindowCount - 1 do
    SourceWindows[i].HideHint;
end;

procedure TSourceEditorManager.OnIdle(Sender: TObject; var Done: Boolean);
var
  SrcEdit: TSourceEditor;
  i: Integer;
  aFilename: String;
  FreeList, FreeMarklings: boolean;
  Marklings: TFPList;
  j: Integer;
  Markling: TSourceMarkling;
begin
  SrcEdit:=ActiveEditor;
  if (SrcEdit<>nil)
  and (not SrcEdit.FSharedValues.FMarklingsValid) then
  begin
    //debugln(['TSourceEditorManager.OnIdle ',MarklingProducerCount]);
    aFilename:=SrcEdit.FileName;
    SrcEdit.EditorComponent.BeginUpdate;
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
  CloseAction: TJumpHistoryAction);
var NewCaretXY: TPoint;
  NewTopLine: integer;
  NewEditor: TSourceEditor;
begin
  if Assigned(OnJumpToHistoryPoint) then begin
    NewCaretXY.X:=-1;
    NewEditor:=nil;
    OnJumpToHistoryPoint(NewCaretXY,NewTopLine,NewEditor,CloseAction);
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

procedure TSourceEditorManager.OnFilesDroping(Sender: TObject; const FileNames: Array of String);
begin
  if Sender is TSourceNotebook then
    ActiveSourceWindow := TSourceNotebook(Sender);
end;

procedure TSourceEditorManager.OnCodeTemplateTokenNotFound(Sender: TObject;
  AToken: string; AnEditor: TCustomSynEdit; var Index: integer);
begin
  //debugln('TSourceNotebook.OnCodeTemplateTokenNotFound ',AToken,',',AnEditor.ReadOnly,',',DefaultCompletionForm.CurrentCompletionType=ctNone);
  if (AnEditor.ReadOnly=false) and
     (DefaultCompletionForm.CurrentCompletionType=ctNone)
  then begin
    DefaultCompletionForm.CurrentCompletionType:=ctTemplateCompletion;
    DefaultCompletionForm.Editor:=AnEditor;
    DefaultCompletionForm.Execute
      (AToken, GetScreenRectForToken(AnEditor, AnEditor.CaretX-length(AToken), AnEditor.CaretY, AnEditor.CaretX-1));
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

procedure TSourceEditorManager.OnWordCompletionGetSource(var Source: TStrings;
  SourceIndex: integer);
var TempEditor: TSourceEditor;
  i:integer;
begin
  TempEditor:=GetActiveSE;
  if (SourceIndex=0) and (TempEditor<>nil) then begin
    Source:=TempEditor.EditorComponent.Lines;
  end else begin
    i:=0;
    while (i < SourceEditorCount) do begin
      if SourceEditors[i] <> TempEditor then dec(SourceIndex);
      if SourceIndex <= 0 then begin
        Source := SourceEditors[i].EditorComponent.Lines;
        exit;
      end;
      inc(i);
    end;
    Source := nil;
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
    and (Attri<>nil) and (Attri.Name=SYNS_AttrComment) then
    begin
      exit;
    end;

    // invoke identifier completion
    SrcEdit.StartIdentCompletionBox(false);
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
  SourceCompletionTimer.Enabled:=false;
  SourceCompletionTimer.AutoEnabled:=false;
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

function TSourceEditorManager.OnSourceMarksGetFilename(ASourceEditor: TObject
  ): string;
begin
  if (ASourceEditor = nil) or (not (ASourceEditor is TSourceEditor)) then
    RaiseException('TSourceNotebook.OnSourceMarksGetFilename');
  Result := TSourceEditor(ASourceEditor).Filename;
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

function TSourceEditorManager.GotoDialog: TfrmGoto;
begin
  if FGotoDialog=nil then
    FGotoDialog := TfrmGoto.Create(self);
  Result := FGotoDialog;
end;

constructor TSourceEditorManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDefaultCompletionForm := nil;

  // word completion
  if aWordCompletion=nil then begin
    aWordCompletion:=TWordCompletion.Create;
    with AWordCompletion do begin
      WordBufferCapacity:=100;
      OnGetSource:=@OnWordCompletionGetSource;
    end;
  end;

  // identifier completion
  SourceCompletionTimer := TIdleTimer.Create(Self);
  with SourceCompletionTimer do begin
    AutoEnabled := False;
    Enabled := false;
    Interval := EditorOpts.AutoDelayInMSec;
    OnTimer := @OnSourceCompletionTimer;
  end;

  // marks
  SourceEditorMarks:=TSourceMarks.Create(Self);
  SourceEditorMarks.OnGetFilename:=@OnSourceMarksGetFilename;
  SourceEditorMarks.OnAction:=@OnSourceMarksAction;

  // code templates
  FCodeTemplateModul:=TSynEditAutoComplete.Create(Self);
  with FCodeTemplateModul do begin
    if FileExistsUTF8(EditorOpts.CodeTemplateFilename) then
      AutoCompleteList.LoadFromFile(UTF8ToSys(EditorOpts.CodeTemplateFilename))
    else
      if FileExistsUTF8('lazarus.dci') then
        AutoCompleteList.LoadFromFile(UTF8ToSys('lazarus.dci'));
    IndentToTokenStart := EditorOpts.CodeTemplateIndentToTokenStart;
    OnTokenNotFound := @OnCodeTemplateTokenNotFound;
    OnExecuteCompletion := @OnCodeTemplateExecuteCompletion;
    EndOfTokenChr:=' ()[]{},.;:"+-*^@$\<>=''';
  end;

  // layout
  IDEWindowCreators.Add(NonModalIDEWindowNames[nmiwSourceNoteBookName],
    nil,@CreateSourceWindow,'250','100','+70%','+70%',
    NonModalIDEWindowNames[nmiwMainIDEName],alBottom,
    true,@GetDefaultLayout);

  Application.AddOnIdleHandler(@OnIdle);
end;

destructor TSourceEditorManager.Destroy;
begin
  SourceEditorMarks.OnGetFilename := nil;
  SourceEditorMarks.OnAction := nil;
  Application.RemoveAllHandlersOfObject(Self);
  inherited Destroy;
end;

function SortSourceWindows(SrcWin1, SrcWin2: TSourceNotebook): Integer;
begin
  Result := AnsiStrComp(PChar(SrcWin1.Caption), PChar(SrcWin2.Caption));
end;

function TSourceEditorManager.CreateNewWindow(Activate: Boolean= False;
  DoDisableAutoSizing: boolean = false): TSourceNotebook;
var
  i: Integer;
begin
  Result := TSourceNotebook(TSourceNotebook.NewInstance);
  Result.DisableAutoSizing;
  Result.Create(Self);
  Result.OnDropFiles := @OnFilesDroping;

  for i := 1 to FUpdateLock do
    Result.IncUpdateLockInternal;
  FSourceWindowList.Add(Result);
  FSourceWindowList.Sort(TListSortCompare(@SortSourceWindows));
  FSourceWindowByFocusList.Add(Result);
  if Activate then begin
    ActiveSourceWindow := Result;
    ShowActiveWindowOnTop(False);
  end;
  FChangeNotifyLists[semWindowCreate].CallNotifyEvents(Result);
  if not DoDisableAutoSizing then
    Result.EnableAutoSizing;
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
  if i >= 0 then
    FChangeNotifyLists[semWindowDestroy].CallNotifyEvents(AWindow);
end;

(* Context Menu handlers *)

procedure TSourceEditorManager.CloseOtherPagesClicked(Sender: TObject);
begin
  if Assigned(OnCloseClicked) then
    OnCloseClicked(Sender, True);
end;

procedure TSourceEditorManager.ReadOnlyClicked(Sender: TObject);
var ActEdit: TSourceEditor;
begin
  ActEdit:=ActiveEditor;
  if ActEdit = nil then exit;

  if ActEdit.ReadOnly and (ActEdit.CodeBuffer<>nil)
  and (not ActEdit.CodeBuffer.IsVirtual)
  and (not FileIsWritable(ActEdit.CodeBuffer.Filename)) then begin
    MessageDlg(ueFileROCap,
      ueFileROText1+ActEdit.CodeBuffer.Filename+ueFileROText2,
      mtError,[mbCancel],0);
    exit;
  end;
  ActEdit.EditorComponent.ReadOnly := not(ActEdit.EditorComponent.ReadOnly);
  if Assigned(OnReadOnlyChanged) then
    OnReadOnlyChanged(Self);
  ActEdit.SourceNotebook.UpdateStatusBar;
end;

procedure TSourceEditorManager.ToggleLineNumbersClicked(Sender: TObject);
var
  MenuITem: TIDEMenuCommand;
  ActEdit:TSourceEditor;
  i: integer;
  ShowLineNumbers: boolean;
begin
  MenuItem := Sender as TIDEMenuCommand;
  ActEdit:=ActiveEditor;
  if ActEdit = nil then exit;

  MenuItem.Checked := not EditorOpts.ShowLineNumbers;
  ShowLineNumbers:=MenuItem.Checked;

  for i:=0 to SourceEditorCount-1 do
    SourceEditors[i].EditorComponent.Gutter.LineNumberPart.Visible
      := ShowLineNumbers;
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
  if Assigned(OnEditorPropertiesClicked) then
    OnEditorPropertiesClicked(Sender);
end;

initialization
  InternalInit;
  {$I ../images/bookmark.lrs}

finalization
  InternalFinal;

end.

