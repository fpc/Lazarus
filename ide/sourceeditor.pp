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

uses
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, Math, Controls, LCLProc, LCLType, LResources, LCLIntf,
  FileUtil, Forms, Buttons, ComCtrls, Dialogs, StdCtrls, GraphType, Graphics,
  Translations, ClipBrd, TypInfo, types, Extctrls, Menus, HelpIntfs,
  LazHelpIntf, LConvEncoding, LDockCtrl,
  // codetools
  CodeToolManager, CodeCache, SourceLog,
  // synedit
  SynEditStrConst, SynEditTypes, SynEdit, SynRegExpr, SynEditHighlighter,
  SynEditAutoComplete, SynEditKeyCmds, SynCompletion, SynEditMiscClasses,
  SynEditMarkupHighAll, SynGutterLineNumber, SynEditMarks, SynBeautifier,
  SynEditTextBase, SynPluginTemplateEdit, SynPluginSyncroEdit,
  // IDE interface
  MacroIntf, ProjectIntf, SrcEditorIntf, MenuIntf, LazIDEIntf, PackageIntf,
  IDEDialogs, IDEHelpIntf, IDEWindowIntf, IDEImagesIntf,
  // IDE units
  LazarusIDEStrConsts, LazConf, IDECommands, EditorOptions, KeyMapping, Project,
  WordCompletion, FindReplaceDialog, FindInFilesDlg, IDEProcs, IDEOptionDefs,
  MacroPromptDlg, TransferMacros, CodeContextForm, SrcEditHintFrm,
  EnvironmentOpts, MsgView, SearchResultView, InputHistory, CodeMacroPrompt,
  CodeTemplatesDlg, TodoDlg, TodoList, CodeToolsOptions,
  SortSelectionDlg, EncloseSelectionDlg, DiffDialog, ConDef, InvertAssignTool,
  SourceEditProcs, SourceMarks, CharacterMapDlg, SearchFrm,
  FPDocHints, FPDocEditWindow,
  BaseDebugManager, Debugger, MainIntf, GotoFrm;

type
  TSourceNotebook = class;

  TNotifyFileEvent = procedure(Sender: TObject; Filename : AnsiString) of object;

  TOnAddWatch = function(Sender: TObject): boolean of object;

  TOnProcessUserCommand = procedure(Sender: TObject;
            Command: word; var Handled: boolean) of object;
  TOnUserCommandProcessed = procedure(Sender: TObject;
            Command: word; var Handled: boolean) of object;

  TOnLinesInsertedDeleted = procedure(Sender : TObject;
             FirstLine,Count : Integer) of Object;

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


  TCharSet = set of Char;

{---- TSource Editor ---
  TSourceEditor is the class that controls access for the Editor.
 ---- TSource Editor ---}

  TSourceEditor = class;

  { TSourceEditor }

  TSourceEditor = class(TSourceEditorInterface)
  private
    //FAOwner is normally a TSourceNotebook.  This is set in the Create constructor.
    FAOwner: TComponent;
    FEditor: TSynEdit;
    FEditPlugin: TSynEditPlugin1;  // used to get the LinesInserted and
                                   //   LinesDeleted messages
    FCodeTemplates: TSynEditAutoComplete;
    FHasExecutionMarks: Boolean;
    FMarksRequested: Boolean;
    FPageName: string;

    FCodeBuffer: TCodeBuffer;
    FIgnoreCodeBufferLock: integer;
    FEditorStampCommitedToCodetools: int64;

    FPopUpMenu: TPopupMenu;
    FSyntaxHighlighterType: TLazSyntaxHighlighter;
    FErrorLine: integer;
    FErrorColumn: integer;
    FExecutionLine: integer;
    FExecutionMark: TSourceMark;
    FLineInfoNotification: TIDELineInfoNotification;
    FModified: boolean;

    FOnAfterClose: TNotifyEvent;
    FOnAfterOpen: TNotifyEvent;
    FOnAfterSave: TNotifyEvent;
    FOnBeforeClose: TNotifyEvent;
    FOnBeforeOpen: TNotifyEvent;
    FOnBeforeSave: TNotifyEvent;
    FOnEditorChange: TNotifyEvent;
    FVisible: Boolean;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseDown: TMouseEvent;
    FOnClickLink: TMouseEvent;
    FOnMouseLink: TSynMouseLinkEvent;
    FOnMouseWheel : TMouseWheelEvent;
    FOnKeyDown: TKeyEvent;

    FSourceNoteBook: TSourceNotebook;

    procedure EditorMouseMoved(Sender: TObject; Shift: TShiftState; X,Y:Integer);
    procedure EditorMouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X,Y: Integer);
    procedure EditorClickLink(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X,Y: Integer);
    procedure EditorMouseLink(
      Sender: TObject; X,Y: Integer; var AllowMouseLink: Boolean);
    procedure EditorMouseWheel(Sender: TObject; Shift: TShiftState;
         WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
    procedure SetCodeBuffer(NewCodeBuffer: TCodeBuffer);
    function GetSource: TStrings;
    procedure SetPageName(const AValue: string);
    procedure UpdateExecutionSourceMark;
    procedure UpdatePageName;
    procedure SetSource(Value: TStrings);
    function GetCurrentCursorXLine: Integer;
    procedure SetCurrentCursorXLine(num : Integer);
    function GetCurrentCursorYLine: Integer;
    procedure SetCurrentCursorYLine(num: Integer);
    Function GetInsertMode: Boolean;
    procedure SetCodeTemplates(NewCodeTemplates: TSynEditAutoComplete);
    procedure SetPopupMenu(NewPopupMenu: TPopupMenu);

    function GotoLine(Value: Integer): Integer;

    procedure CreateEditor(AOwner: TComponent; AParent: TWinControl);
    procedure SetVisible(Value: boolean);
    procedure UnbindEditor;
  protected
    ErrorMsgs: TStrings;
    procedure ReParent(AParent: TWinControl);

    procedure ProcessCommand(Sender: TObject;
       var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure ProcessUserCommand(Sender: TObject;
       var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure UserCommandProcessed(Sender: TObject;
       var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
    procedure ccAddMessage(Texts: String);
    function AutoCompleteChar(Char: TUTF8Char; var AddChar: boolean;
       Category: TAutoCompleteOption): boolean;
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
    procedure OnCodeBufferChanged(Sender: TSourceLog;
      SrcLogEntry: TSourceLogEntry);
    procedure StartIdentCompletion(JumpToError: boolean);

    procedure LinesInserted(sender: TObject; FirstLine, Count: Integer);
    procedure LinesDeleted(sender: TObject; FirstLine, Count: Integer);

    function GetFilename: string; override;
    function GetEditorControl: TWinControl; override;
    function GetCodeToolsBuffer: TObject; override;
    Function GetReadOnly: Boolean; override;
    procedure SetReadOnly(const NewValue: boolean); override;

    property Visible: Boolean read FVisible write SetVisible default False;
  public
    constructor Create(AOwner: TComponent; AParent: TWinControl);
    destructor Destroy; override;
    Function Close: Boolean;

    // codebuffer
    procedure BeginUndoBlock; override;
    procedure EndUndoBlock; override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
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
    function DoFindAndReplace: Integer;
    procedure FindNextUTF8;
    procedure FindPrevious;
    procedure FindNextWordOccurrence(DirectionForward: boolean);
    procedure InitGotoDialog;
    procedure ShowGotoLineDialog;

    // dialogs
    procedure GetDialogPosition(Width, Height: integer; out Left, Top: integer);
    procedure ActivateHint(ClientPos: TPoint;
                           const BaseURL, TheHint: string);

    // selections
    function SelectionAvailable: boolean; override;
    function GetText(OnlySelection: boolean): string; override;
    procedure SelectText(const StartPos, EndPos: TPoint); override;
    procedure ReplaceLines(StartLine, EndLine: integer; const NewText: string); override;
    procedure EncloseSelection;
    procedure UpperCaseSelection;
    procedure LowerCaseSelection;
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
    procedure InsertTodo;
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
    function GetWordAtPosition(Position: TPoint): String;
    function GetWordFromCaret(const ACaretPos: TPoint): String;
    function GetWordAtCurrentCaret: String;
    function CaretInSelection(const ACaretPos: TPoint): Boolean;
    function PositionInSelection(const APosition: TPoint): Boolean;

    // cursor
    function GetCaretPosFromCursorPos(const CursorPos: TPoint): TPoint;
    procedure CenterCursor;
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
    function GetDesigner(LoadForm: boolean): TIDesigner; override;

    // notebook
    procedure Activate;
    function PageIndex: integer;
    function IsActiveOnNoteBook: boolean;

    // debugging
    procedure FillExecutionMarks;
    procedure ClearExecutionMarks;
    procedure LineInfoNotificationChange(const ASender: TObject; const ASource: String);
  public
    // properties
    property CodeBuffer: TCodeBuffer read FCodeBuffer write SetCodeBuffer;
    property CodeTemplates: TSynEditAutoComplete
       read FCodeTemplates write SetCodeTemplates;
    property CurrentCursorXLine: Integer
       read GetCurrentCursorXLine write SetCurrentCursorXLine;
    property CurrentCursorYLine: Integer
       read GetCurrentCursorYLine write SetCurrentCursorYLine;
    property EditorComponent: TSynEdit read FEditor;
    property ErrorLine: integer read FErrorLine write SetErrorLine;
    property ExecutionLine: integer read FExecutionLine write SetExecutionLine;
    property HasExecutionMarks: Boolean read FHasExecutionMarks;
    property InsertMode: Boolean read GetInsertmode;
    property OnAfterClose: TNotifyEvent read FOnAfterClose write FOnAfterClose;
    property OnBeforeClose: TNotifyEvent read FOnBeforeClose
                                          write FOnBeforeClose;
    property OnAfterOpen: TNotifyEvent read FOnAfterOpen write FOnAfterOpen;
    property OnBeforeOpen: TNotifyEvent read FOnBeforeOpen write FOnBeforeOpen;
    property OnAfterSave: TNotifyEvent read FOnAfterSave write FOnAfterSave;
    property OnBeforeSave: TNotifyEvent read FOnBeforeSave write FOnBeforeSave;
    property OnEditorChange: TNotifyEvent read FOnEditorChange
                                          write FOnEditorChange;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnClickLink: TMouseEvent read FOnClickLink write FOnClickLink;
    property OnMouseLink: TSynMouseLinkEvent read FOnMouseLink write FOnMouseLink;
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
  end;

  //============================================================================

  { TSourceNotebook }

  TJumpHistoryAction = (jhaBack, jhaForward, jhaViewWindow);

  TOnJumpToHistoryPoint = procedure(var NewCaretXY: TPoint;
                                    var NewTopLine, NewPageIndex: integer;
                                    Action: TJumpHistoryAction) of object;
  TOnAddJumpPoint = procedure(ACaretXY: TPoint; ATopLine: integer;
                  APageIndex: integer; DeleteForwardHistory: boolean) of object;
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
  TOnGetIndentEvent = function(Sender: TSynCustomBeautifier; Editor: TSourceEditor;
      LogCaret: TPoint; var FirstLinePos, LinesCount: Integer;
      Reason: TSynEditorCommand; SetIndentProc: TSynBeautifierSetIndentProc
     ): boolean of object;

  TSourceNotebookState = (
    snIncrementalFind,
    snWarnedFont
    );
  TSourceNotebookStates = set of TSourceNotebookState;

  { TSourceNotebook }

  TSourceNotebook = class(TSourceEditorWindowInterface)
    Notebook: TNoteBook;
    SrcPopUpMenu: TPopupMenu;
    StatusBar: TStatusBar;
    procedure AddBreakpointClicked(Sender: TObject);
    procedure StatusBarDblClick(Sender: TObject);
    procedure ToggleBreakpointClicked(Sender: TObject);
    procedure CompleteCodeMenuItemClick(Sender: TObject);
    procedure DeleteBreakpointClicked(Sender: TObject);
    procedure EncloseSelectionMenuItemClick(Sender: TObject);
    procedure ExtractProcMenuItemClick(Sender: TObject);
    procedure InvertAssignmentMenuItemClick(Sender: TObject);
    procedure FindIdentifierReferencesMenuItemClick(Sender: TObject);
    procedure RenameIdentifierMenuItemClick(Sender: TObject);
    procedure ShowAbstractMethodsMenuItemClick(Sender: TObject);
    procedure ShowEmptyMethodsMenuItemClick(Sender: TObject);
    procedure ShowUnusedUnitsMenuItemClick(Sender: TObject);
    procedure FindOverloadsMenuItemClick(Sender: TObject);
    procedure RunToClicked(Sender: TObject);
    procedure ViewCallStackClick(Sender: TObject);
    procedure AddWatchAtCursor(Sender: TObject);
    procedure BookmarkGoTo(Index: Integer);
    procedure BookmarkGotoNext(GoForward: boolean);
    procedure BookMarkNextClicked(Sender: TObject);
    procedure BookMarkPrevClicked(Sender: TObject);
    procedure BookMarkGotoClicked(Sender: TObject);
    procedure BookMarkSet(Value: Integer; Toggle: boolean = false);
    procedure BookMarkSetFree;// set a free bookmark
    procedure BookMarkToggleClicked(Sender: TObject);
    procedure BookmarkSetFreeClicked(Sender: TObject);
    procedure EditorPropertiesClicked(Sender: TObject);
    procedure EncodingClicked(Sender: TObject);
    procedure HighlighterClicked(Sender: TObject);
    procedure FindDeclarationClicked(Sender: TObject);
    procedure ProcedureJumpClicked(Sender: TObject);
    procedure FindNextWordOccurrenceClicked(Sender: TObject);
    procedure FindPrevWordOccurrenceClicked(Sender: TObject);
    procedure FindInFilesClicked(Sender: TObject);
    procedure InsertTodoClicked(Sender: TObject);
    procedure MoveEditorLeftClicked(Sender: TObject);
    procedure MoveEditorRightClicked(Sender: TObject);
    procedure MoveEditorFirstClicked(Sender: TObject);
    procedure MoveEditorLastClicked(Sender: TObject);
    procedure DockingClicked(Sender: TObject);
    procedure NotebookPageChanged(Sender: TObject);
    procedure NotebookShowTabHint(Sender: TObject; HintInfo: PHintInfo);
    procedure OpenAtCursorClicked(Sender: TObject);
    procedure ReadOnlyClicked(Sender: TObject);
    procedure OnPopupMenuOpenPasFile(Sender: TObject);
    procedure OnPopupMenuOpenPPFile(Sender: TObject);
    procedure OnPopupMenuOpenPFile(Sender: TObject);
    procedure OnPopupMenuOpenLFMFile(Sender: TObject);
    procedure OnPopupMenuOpenLRSFile(Sender: TObject);
    procedure OnPopupMenuOpenSFile(Sender: TObject);
    procedure OnPopupMenuOpenFile(Sender: TObject);
    procedure ShowUnitInfo(Sender: TObject);
    procedure SrcPopUpMenuPopup(Sender: TObject);
    procedure ToggleLineNumbersClicked(Sender: TObject);
    procedure InsertCharacter(const C: TUTF8Char);
  private
    fAutoFocusLock: integer;
    FCodeTemplateModul: TSynEditAutoComplete;
    fIdentCompletionJumpToError: boolean;
    FIncrementalSearchPos: TPoint; // last set position
    fIncrementalSearchStartPos: TPoint; // position where to start searching
    FIncrementalSearchStr, FIncrementalFoundStr: string;
    FIncrementalSearchBackwards : Boolean;
    FIncrementalSearchEditor: TSourceEditor; // editor with active search (MWE:shouldnt all FIncrementalSearch vars go to that editor ?)
    FKeyStrokes: TSynEditKeyStrokes;
    FLastCodeBuffer: TCodeBuffer;
    FOnAddJumpPoint: TOnAddJumpPoint;
    FOnAddWatchAtCursor: TOnAddWatch;
    FOnCloseClicked: TOnCloseSrcEditor;
    FOnClickLink: TMouseEvent;
    FOnMouseLink: TSynMouseLinkEvent;
    FOnCurrentCodeBufferChanged: TNotifyEvent;
    FOnDeleteLastJumpPoint: TNotifyEvent;
    FOnEditorChanged: TNotifyEvent;
    FOnEditorPropertiesClicked: TNotifyEvent;
    FOnEditorVisibleChanged: TNotifyEvent;
    FOnFindDeclarationClicked: TNotifyEvent;
    FOnInitIdentCompletion: TOnInitIdentCompletion;
    FOnInsertTodoClicked: TNotifyEvent;
    FOnShowCodeContext: TOnShowCodeContext;
    FOnJumpToHistoryPoint: TOnJumpToHistoryPoint;
    FOnMovingPage: TOnMovingPage;
    FOnOpenFileAtCursorClicked: TNotifyEvent;
    FOnProcessUserCommand: TOnProcessUserCommand;
    fOnReadOnlyChanged: TNotifyEvent;
    FOnShowHintForSource: TOnShowHintForSource;
    FOnShowSearchResultsView: TNotifyEvent;
    FOnShowUnitInfo: TNotifyEvent;
    FOnToggleFormUnitClicked: TNotifyEvent;
    FOnToggleObjectInspClicked: TNotifyEvent;
    FOnUserCommandProcessed: TOnProcessUserCommand;
    FOnViewJumpHistory: TNotifyEvent;
    FProcessingCommand: boolean;
    FSourceEditorList: TList; // list of TSourceEditor
    FOnPopupMenu: TSrcEditPopupMenuEvent;
  private
    // colors for the completion form (popup form, e.g. word completion)
    FActiveEditDefaultFGColor: TColor;
    FActiveEditDefaultBGColor: TColor;
    FActiveEditSelectedFGColor: TColor;
    FActiveEditSelectedBGColor: TColor;
    FActiveEditKeyFGColor: TColor;
    FActiveEditKeyBGColor: TColor;
    FActiveEditSymbolFGColor: TColor;
    FActiveEditSymbolBGColor: TColor;
    FOnGetIndent: TOnGetIndentEvent;

    // PopupMenu
    procedure BuildPopupMenu;
    procedure UpdateHighlightMenuItems;
    procedure UpdateEncodingMenuItems;
    procedure RemoveUserDefinedMenuItems;
    function AddUserDefinedPopupMenuItem(const NewCaption: string;
                                     const NewEnabled: boolean;
                                     const NewOnClick: TNotifyEvent): TIDEMenuItem;
    procedure RemoveContextMenuItems;
    function AddContextPopupMenuItem(const NewCaption: string;
                                     const NewEnabled: boolean;
                                     const NewOnClick: TNotifyEvent): TIDEMenuItem;

    procedure UpdateActiveEditColors(AEditor: TSynEdit);
    procedure SetIncrementalSearchStr(const AValue: string);
    procedure IncrementalSearch(ANext, ABackward: Boolean);

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
  protected
    ccSelection: String;
    States: TSourceNotebookStates;
    // hintwindow stuff
    FHintWindow: THintWindow;
    FMouseHintTimer: TIdleTimer;


    function CreateNotebook: Boolean;
    function NewSE(Pagenum: Integer): TSourceEditor;
    procedure EditorChanged(Sender: TObject);

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
    procedure DeactivateCompletionForm;
    procedure InitIdentCompletion(S: TStrings);

    procedure EditorMouseMove(Sender: TObject; Shift: TShiftstate;
                              X,Y: Integer);
    procedure EditorMouseDown(Sender: TObject; Button: TMouseButton;
                              Shift: TShiftstate; X,Y: Integer);
    procedure EditorClickLink(Sender: TObject; Button: TMouseButton;
                            Shift: TShiftstate; X,Y: Integer);
    procedure EditorMouseLink(
      Sender: TObject; X,Y: Integer; var AllowMouseLink: Boolean);
    function EditorGetIndent(Sender: TSynCustomBeautifier; Editor: TObject;
                              LogCaret: TPoint; var FirstLinePos, LinesCount: Integer;
                              Reason: TSynEditorCommand;
                              SetIndentProc: TSynBeautifierSetIndentProc): Boolean;
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditorMouseWheel(Sender: TObject; Shift: TShiftState;
         WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    procedure NotebookMouseDown(Sender: TObject; Button: TMouseButton;
          Shift: TShiftState; X,Y: Integer);

    // hintwindow stuff
    procedure HintTimer(Sender: TObject);
    procedure OnApplicationUserInput(Sender: TObject; Msg: Cardinal);
    procedure ShowSynEditHint(const MousePos: TPoint);

    procedure NextEditor;
    procedure PrevEditor;
    procedure MoveEditor(OldPageIndex, NewPageIndex: integer);
    procedure MoveEditorLeft(PageIndex: integer);
    procedure MoveEditorRight(PageIndex: integer);
    procedure MoveActivePageLeft;
    procedure MoveActivePageRight;
    procedure MoveEditorFirst(PageIndex: integer);
    procedure MoveEditorLast(PageIndex: integer);
    procedure MoveActivePageFirst;
    procedure MoveActivePageLast;
    procedure ProcessParentCommand(Sender: TObject;
       var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
       var Handled: boolean);
    procedure ParentCommandProcessed(Sender: TObject;
       var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
       var Handled: boolean);
    function GetCompletionBoxPosition: integer; override;

    // marks
    function FindBookmark(BookmarkID: integer): TSourceEditor;
    function OnSourceMarksGetSourceEditor(ASynEdit: TCustomSynEdit): TObject;
    function OnSourceMarksGetFilename(ASourceEditor: TObject): string;
    procedure OnSourceMarksAction(AMark: TSourceMark; AAction: TListNotification);

    function GetItems(Index: integer): TSourceEditorInterface; override;
    function GetEditors(Index:integer): TSourceEditor;

    procedure KeyDownBeforeInterface(var Key: Word; Shift: TShiftState); override;

    procedure BeginAutoFocusLock;
    procedure EndAutoFocusLock;
  public
    FindReplaceDlgHistoryIndex: array[TFindDlgComponent] of integer;
    FindReplaceDlgUserText: array[TFindDlgComponent] of string;
    ControlDocker: TLazControlDocker;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitMacros(AMacroList: TTransferMacroList);
    procedure CreateCompletionForm;

    procedure ShowFPDocEditor;
    procedure UpdateFPDocEditor;

    property Editors[Index:integer]:TSourceEditor read GetEditors;
    function EditorCount:integer;
    function Count: integer; override;
    function Empty: boolean;

    function FindSourceEditorWithPageIndex(PageIndex:integer):TSourceEditor;
    function FindPageWithEditor(ASourceEditor: TSourceEditor):integer;
    function FindSourceEditorWithEditorComponent(
                                         EditorComp: TComponent): TSourceEditor;
    function FindSourceEditorWithFilename(const Filename: string): TSourceEditor;
    function GetActiveSE: TSourceEditor;
    procedure SetActiveSE(SrcEdit: TSourceEditor);
    function GetActiveEditor: TSourceEditorInterface; override;
    procedure SetActiveEditor(const AValue: TSourceEditorInterface); override;
    procedure CheckCurrentCodeBufferChanged;

    procedure LockAllEditorsInSourceChangeCache;
    procedure UnlockAllEditorsInSourceChangeCache;
    function GetDiffFiles: TDiffFiles;
    procedure GetSourceText(PageIndex: integer; OnlySelection: boolean;
                            var Source: string);

    function ActiveFileName: AnsiString;
    function FindUniquePageName(FileName:string; IgnorePageIndex:integer):string;
    function SomethingModified: boolean;
    procedure UpdateStatusBar;
    procedure ClearErrorLines; override;
    procedure ClearExecutionLines;
    procedure ClearExecutionMarks;

    procedure CloseTabClicked(Sender: TObject);
    procedure CloseClicked(Sender: TObject);
    procedure CloseOtherPagesClicked(Sender: TObject);
    procedure ToggleFormUnitClicked(Sender: TObject);
    procedure ToggleObjectInspClicked(Sender: TObject);

    // find / replace text
    procedure InitFindDialog;
    procedure FindClicked(Sender: TObject);
    procedure FindNextClicked(Sender: TObject);
    procedure FindPreviousClicked(Sender: TObject);
    procedure ReplaceClicked(Sender: TObject);

    // incremental find
    procedure IncrementalFindClicked(Sender: TObject);
    procedure BeginIncrementalFind;
    procedure EndIncrementalFind;
    property IncrementalSearchStr: string
      read FIncrementalSearchStr write SetIncrementalSearchStr;

    // FindInFiles
    procedure FindInFilesPerDialog(AProject: TProject);
    procedure FindInFiles(AProject: TProject; const FindText: string);
    procedure ShowSearchResultsView;
    function CreateFindInFilesDialog: TLazFindInFilesDialog;
    procedure LoadFindInFilesHistory(ADialog: TLazFindInFilesDialog);
    procedure SaveFindInFilesHistory(ADialog: TLazFindInFilesDialog);
    procedure FIFSearchProject(AProject: TProject;
                               ADialog: TLazFindInFilesDialog);
    procedure FIFSearchOpenFiles(ADialog: TLazFindInFilesDialog);
    procedure FIFSearchDir(ADialog: TLazFindInFilesDialog);
    function FIFCreateSearchForm(ADialog:TLazFindInFilesDialog): TSearchForm;
    procedure DoFindInFiles(ASearchForm: TSearchForm);

    // goto line number
    procedure GotoLineClicked(Sender: TObject);

    // history jumping
    procedure HistoryJump(Sender: TObject; CloseAction: TJumpHistoryAction);
    procedure JumpBackClicked(Sender: TObject);
    procedure JumpForwardClicked(Sender: TObject);
    procedure AddJumpPointClicked(Sender: TObject);
    procedure DeleteLastJumpPointClicked(Sender: TObject);
    procedure ViewJumpHistoryClicked(Sender: TObject);

    // hints
    procedure ActivateHint(const ScreenPos: TPoint;
                           const BaseURL, TheHint: string);
    procedure HideHint;
    procedure StartShowCodeContext(JumpToError: boolean);
    procedure StartShowCodeHelp;

    // new, close, focus
    function NewFile(const NewShortName: String; ASource: TCodeBuffer;
                      FocusIt: boolean): TSourceEditor;
    procedure CloseFile(PageIndex:integer);
    procedure FocusEditor;

    // paste and copy
    procedure CutClicked(Sender: TObject);
    procedure CopyClicked(Sender: TObject);
    procedure PasteClicked(Sender: TObject);
    procedure CopyFilenameClicked(Sender: TObject);

    // bookmarks
    procedure ToggleBookmark(Value: Integer);
    procedure SetBookmark(Value: Integer);
    procedure GotoBookmark(Value: Integer);

    procedure ReloadEditorOptions;
    procedure ReloadHighlighters;
    procedure CheckFont;
    procedure GetSynEditPreviewSettings(APreviewEditor: TObject);
    function GetEditorControlSettings(EditControl: TControl): boolean; override;
    function GetHighlighterSettings(Highlighter: TObject): boolean; override;

    property CodeTemplateModul: TSynEditAutoComplete
                               read FCodeTemplateModul write FCodeTemplateModul;
    procedure OnCodeTemplateTokenNotFound(Sender: TObject; AToken: string;
                                   AnEditor: TCustomSynEdit; var Index:integer);
    procedure OnCodeTemplateExecuteCompletion(
                                       ASynAutoComplete: TCustomSynAutoComplete;
                                       Index: integer);
    procedure OnWordCompletionGetSource(
                                    var Source: TStrings; SourceIndex: integer);
    procedure OnSourceCompletionTimer(Sender: TObject);

    procedure FindReplaceDlgKey(Sender: TObject; var Key: Word;
                       Shift: TShiftState; FindDlgComponent: TFindDlgComponent);
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
    property OnEditorVisibleChanged: TNotifyEvent
                     read FOnEditorVisibleChanged write FOnEditorVisibleChanged;
    property OnEditorChanged: TNotifyEvent
                                   read FOnEditorChanged write FOnEditorChanged;
    property OnEditorPropertiesClicked: TNotifyEvent
               read FOnEditorPropertiesClicked write FOnEditorPropertiesClicked;
    property OnCurrentCodeBufferChanged: TNotifyEvent
             read FOnCurrentCodeBufferChanged write FOnCurrentCodeBufferChanged;
    property OnFindDeclarationClicked: TNotifyEvent
                 read FOnFindDeclarationClicked write FOnFindDeclarationClicked;
    property OnInitIdentCompletion: TOnInitIdentCompletion
                       read FOnInitIdentCompletion write FOnInitIdentCompletion;
    property OnInsertTodoClicked: TNotifyEvent
               read FOnInsertTodoClicked write FOnInsertTodoClicked;
    property OnShowCodeContext: TOnShowCodeContext
                               read FOnShowCodeContext write FOnShowCodeContext;
    property OnJumpToHistoryPoint: TOnJumpToHistoryPoint
                         read FOnJumpToHistoryPoint write FOnJumpToHistoryPoint;
    property OnMovingPage: TOnMovingPage read FOnMovingPage write FOnMovingPage;
    property OnOpenFileAtCursorClicked: TNotifyEvent
               read FOnOpenFileAtCursorClicked write FOnOpenFileAtCursorClicked;
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
    property OnProcessUserCommand: TOnProcessUserCommand
                         read FOnProcessUserCommand write FOnProcessUserCommand;
    property OnUserCommandProcessed: TOnUserCommandProcessed
                     read FOnUserCommandProcessed write FOnUserCommandProcessed;
    property OnViewJumpHistory: TNotifyEvent
                               read FOnViewJumpHistory write FOnViewJumpHistory;
    property OnAddWatchAtCursor: TOnAddWatch
                             read FOnAddWatchAtCursor write FOnAddWatchAtCursor;
    property OnShowSearchResultsView: TNotifyEvent
                   read FOnShowSearchResultsView write FOnShowSearchResultsView;
    property OnPopupMenu: TSrcEditPopupMenuEvent read FOnPopupMenu write FOnPopupMenu;
  end;

var
  SourceNotebook: TSourceNotebook = nil;

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
    SrcEditMenuAddWatchAtCursor: TIDEMenuCommand;
    SrcEditMenuViewCallStack: TIDEMenuCommand;
    // refactoring
    SrcEditMenuCompleteCode: TIDEMenuCommand;
    SrcEditMenuEncloseSelection: TIDEMenuCommand;
    SrcEditMenuRenameIdentifier: TIDEMenuCommand;
    SrcEditMenuFindIdentifierReferences: TIDEMenuCommand;
    SrcEditMenuExtractProc: TIDEMenuCommand;
    SrcEditMenuInvertAssignment: TIDEMenuCommand;
    SrcEditMenuShowAbstractMethods: TIDEMenuCommand;
    SrcEditMenuShowEmptyMethods: TIDEMenuCommand;
    SrcEditMenuShowUnusedUnits: TIDEMenuCommand;
    SrcEditMenuFindOverloads: TIDEMenuCommand;
  SrcEditMenuInsertTodo: TIDEMenuCommand;
  SrcEditMenuMoveEditorLeft: TIDEMenuCommand;
  SrcEditMenuMoveEditorRight: TIDEMenuCommand;
  SrcEditMenuMoveEditorFirst: TIDEMenuCommand;
  SrcEditMenuMoveEditorLast: TIDEMenuCommand;
  SrcEditMenuDocking: TIDEMenuCommand;
  SrcEditMenuReadOnly: TIDEMenuCommand;
  SrcEditMenuShowLineNumbers: TIDEMenuCommand;
  SrcEditMenuShowUnitInfo: TIDEMenuCommand;
  SrcEditMenuEditorProperties: TIDEMenuCommand;


procedure RegisterStandardSourceEditorMenuItems;


implementation

var
  Highlighters: array[TLazSyntaxHighlighter] of TSynCustomHighlighter;
  // aCompletion:
  //   The component controlling the completion form. It is created on demand
  //   and killed when the IDE ends.
  aCompletion: TSynCompletion = nil;
  // CurCompletionControl contains aCompletion whenever the completion form is
  // active
  CurCompletionControl: TSynCompletion = nil;
  CurrentCompletionType: TCompletionType;
  SourceCompletionTimer: TIdleTimer = nil;
  SourceCompletionCaretXY: TPoint;
  AWordCompletion: TWordCompletion = nil;

  GotoDialog: TfrmGoto = nil;

procedure RegisterStandardSourceEditorMenuItems;
var
  AParent: TIDEMenuSection;
  I: Integer;
begin
  SourceEditorMenuRoot:=RegisterIDEMenuRoot(SourceEditorMenuRootName);
  AParent:=SourceEditorMenuRoot;

  // register the first dynamic section for often used context sensitive stuff
  SrcEditMenuSectionFirstDynamic:=RegisterIDEMenuSection(AParent,
                                                       'First dynamic section');
  // register the first static section
  SrcEditMenuSectionFirstStatic:=RegisterIDEMenuSection(AParent,
                                                        'First static section');
  AParent:=SrcEditMenuSectionFirstStatic;
    SrcEditMenuFindDeclaration:=RegisterIDEMenuCommand(AParent,
                                         'Find Declaration',uemFindDeclaration);
    // register the sub menu Find
    SrcEditSubMenuFind:=RegisterIDESubMenu(AParent, 'Find section', lisMenuFind
      );
    AParent:=SrcEditSubMenuFind;
      SrcEditMenuProcedureJump:=RegisterIDEMenuCommand(AParent,'Procedure Jump',
                                                       uemProcedureJump);
      SrcEditMenuFindNextWordOccurrence:=RegisterIDEMenuCommand(AParent,
                      'Find next word occurrence', srkmecFindNextWordOccurrence, nil, nil, nil, 'menu_search_find_next');
      SrcEditMenuFindPrevWordOccurrence:=RegisterIDEMenuCommand(AParent,
                  'Find previous word occurrence', srkmecFindPrevWordOccurrence, nil, nil, nil, 'menu_search_find_previous');
      SrcEditMenuFindInFiles:=RegisterIDEMenuCommand(AParent,
                  'Find in files', srkmecFindInFiles, nil, nil, nil, 'menu_search_files');
  SrcEditMenuSectionPages := RegisterIDEMenuSection(SourceEditorMenuRoot,
                                                      'Pages');

    SrcEditMenuClosePage := RegisterIDEMenuCommand(SrcEditMenuSectionPages,
                        'Close Page',uemClosePage, nil, nil, nil, 'menu_close');
    SrcEditMenuCloseOtherPages := RegisterIDEMenuCommand(SrcEditMenuSectionPages,
                        'Close All Other Pages',uemCloseOtherPages, nil, nil, nil);

    // register the Move Page sub menu
    SrcEditSubMenuMovePage:=RegisterIDESubMenu(SrcEditMenuSectionPages,
                                               'Move Page ...', lisMovePage);
    AParent:=SrcEditSubMenuMovePage;
      SrcEditMenuMoveEditorLeft:=RegisterIDEMenuCommand(AParent,'MoveEditorLeft',
                                                        uemMovePageLeft);
      SrcEditMenuMoveEditorRight:=RegisterIDEMenuCommand(AParent,'MoveEditorRight',
                                                        uemMovePageRight);
      SrcEditMenuMoveEditorFirst:=RegisterIDEMenuCommand(AParent,'MoveEditorLeftmost',
                                                        uemMovePageLeftmost);
      SrcEditMenuMoveEditorLast:=RegisterIDEMenuCommand(AParent,'MoveEditorRightmost',
                                                        uemMovePageRightmost);

    // register the sub menu Open File
    SrcEditSubMenuOpenFile:=RegisterIDESubMenu(SrcEditMenuSectionPages,
      'Open File ...', lisOpenFile2);
      AParent:=SrcEditSubMenuOpenFile;
      SrcEditMenuOpenFileAtCursor:=RegisterIDEMenuCommand(AParent,
                                     'Open File At Cursor',uemOpenFileAtCursor, nil, nil, nil, 'menu_search_openfile_atcursor');
      // register the File Specific dynamic section
      SrcEditMenuSectionFileDynamic:=RegisterIDEMenuSection(AParent,
                                                        'File dynamic section');

    // register the Flags section
    SrcEditSubMenuFlags:=RegisterIDESubMenu(SrcEditMenuSectionPages,
                                            'Flags section', lisFileSettings);
    AParent:=SrcEditSubMenuFlags;
      SrcEditMenuReadOnly:=RegisterIDEMenuCommand(AParent,'ReadOnly',uemReadOnly);
      SrcEditMenuReadOnly.ShowAlwaysCheckable:=true;
      SrcEditMenuShowLineNumbers:=RegisterIDEMenuCommand(AParent,
                                            'ShowLineNumbers',uemShowLineNumbers);
      SrcEditMenuShowLineNumbers.ShowAlwaysCheckable:=true;
      SrcEditMenuShowUnitInfo:=RegisterIDEMenuCommand(AParent,'ShowUnitInfo',
                                                      uemShowUnitInfo);
      SrcEditSubMenuHighlighter:=RegisterIDESubMenu(AParent,'Highlighter',
                                                      uemHighlighter);
      SrcEditSubMenuEncoding:=RegisterIDESubMenu(AParent,'Encoding',
                                                      uemEncoding);

  // register the Clipboard section
  SrcEditMenuSectionClipboard:=RegisterIDEMenuSection(SourceEditorMenuRoot,
                                                      'Clipboard');
  AParent:=SrcEditMenuSectionClipboard;
    SrcEditMenuCut:=RegisterIDEMenuCommand(AParent,'Cut',uemCut, nil, nil, nil, 'laz_cut');
    SrcEditMenuCopy:=RegisterIDEMenuCommand(AParent,'Copy',uemCopy, nil, nil, nil, 'laz_copy');
    SrcEditMenuPaste:=RegisterIDEMenuCommand(AParent,'Paste',uemPaste, nil, nil, nil, 'laz_paste');
    SrcEditMenuCopyFilename:=RegisterIDEMenuCommand(AParent,'Copy filename',
                                                    uemCopyFilename);

  // register the Marks section
  SrcEditMenuSectionMarks:=RegisterIDEMenuSection(SourceEditorMenuRoot,
                                                  'Marks section');
    // register the Goto Bookmarks Submenu
    SrcEditSubMenuGotoBookmarks:=RegisterIDESubMenu(SrcEditMenuSectionMarks,
                                              'Goto bookmarks',uemGotoBookmark);
    AParent:=SrcEditSubMenuGotoBookmarks;
      for I := 0 to 9 do
        RegisterIDEMenuCommand(AParent,'GotoBookmark'+IntToStr(I),
                               uemBookmarkN+IntToStr(i));
      SrcEditMenuNextBookmark:=RegisterIDEMenuCommand(AParent,
                                          'Goto next Bookmark',uemNextBookmark, nil, nil, nil, 'menu_search_next_bookmark');
      SrcEditMenuPrevBookmark:=RegisterIDEMenuCommand(AParent,
                                      'Goto previous Bookmark',uemPrevBookmark, nil, nil, nil, 'menu_search_previous_bookmark');

    // register the Set Bookmarks Submenu
    SrcEditSubMenuToggleBookmarks:=RegisterIDESubMenu(SrcEditMenuSectionMarks,
                                          'Toggle bookmarks',uemToggleBookmark);
    AParent:=SrcEditSubMenuToggleBookmarks;
      for I := 0 to 9 do
        RegisterIDEMenuCommand(AParent,'ToggleBookmark'+IntToStr(I),
                               uemBookmarkN+IntToStr(i));
      SrcEditMenuSetFreeBookmark:=RegisterIDEMenuCommand(AParent,
                                      'Set a free Bookmark',uemSetFreeBookmark);

  // register the Debug section
  SrcEditMenuSectionDebug:=RegisterIDEMenuSection(SourceEditorMenuRoot,
                                                  'Debug section');
    // register the Debug submenu
    SrcEditSubMenuDebug:=RegisterIDESubMenu(SrcEditMenuSectionDebug,
                                            'Debug', uemDebugWord, nil, nil, 'debugger');
    AParent:=SrcEditSubMenuDebug;
      // register the Debug submenu items
      SrcEditMenuToggleBreakpoint:=RegisterIDEMenuCommand(AParent,'Toggle Breakpoint',
                                                       uemToggleBreakpoint);
      SrcEditMenuAddWatchAtCursor:=RegisterIDEMenuCommand(AParent,
                                     'Add Watch at Cursor',uemAddWatchAtCursor);
      SrcEditMenuRunToCursor:=RegisterIDEMenuCommand(AParent,
                                                'Run to cursor', uemRunToCursor, nil, nil, nil, 'menu_run_cursor');
      SrcEditMenuViewCallStack:=RegisterIDEMenuCommand(AParent,
                                            'View Call Stack', uemViewCallStack, nil, nil, nil, 'debugger_call_stack');

  // register the Refactoring submenu
  SrcEditSubMenuRefactor:=RegisterIDESubMenu(SourceEditorMenuRoot,
                                             'Refactoring',uemRefactor);
  AParent:=SrcEditSubMenuRefactor;
    SrcEditMenuCompleteCode:=RegisterIDEMenuCommand(AParent,'CompleteCode',
                                                    uemCompleteCode);
    SrcEditMenuEncloseSelection:=RegisterIDEMenuCommand(AParent,
                                        'EncloseSelection',uemEncloseSelection);
    SrcEditMenuRenameIdentifier:=RegisterIDEMenuCommand(AParent,
                                        'RenameIdentifier',uemRenameIdentifier);
    SrcEditMenuFindIdentifierReferences:=RegisterIDEMenuCommand(AParent,
                        'FindIdentifierReferences',uemFindIdentifierReferences);
    SrcEditMenuExtractProc:=RegisterIDEMenuCommand(AParent,
                                                 'ExtractProc',uemExtractProc);
    SrcEditMenuInvertAssignment:=RegisterIDEMenuCommand(AParent,
                                        'InvertAssignment',uemInvertAssignment);
    SrcEditMenuShowAbstractMethods:=RegisterIDEMenuCommand(AParent,
                               'ShowAbstractMethods',srkmecShowAbstractMethods);
    SrcEditMenuShowEmptyMethods:=RegisterIDEMenuCommand(AParent,
                               'ShowEmptyMethods', lisCodeHelpShowEmptyMethods);
    SrcEditMenuShowUnusedUnits:=RegisterIDEMenuCommand(AParent,
                               'ShowUnusedUnits', lisCodeHelpShowUnusedUnits);
    SrcEditMenuFindOverloads:=RegisterIDEMenuCommand(AParent,
                               'FindOverloads', srkmecFindOverloads);
   {$IFNDEF EnableFindOverloads}
   SrcEditMenuFindOverloads.Visible:=false;
   {$ENDIF}

  SrcEditMenuInsertTodo:=RegisterIDEMenuCommand(SourceEditorMenuRoot,
                        'InsertTodo',uemInsertTodo, nil, nil, nil, 'item_todo');

  SrcEditMenuEditorProperties:=RegisterIDEMenuCommand(SourceEditorMenuRoot,
           'EditorProperties', dlgFROpts, nil, nil, nil, 'menu_environment_options');
  SrcEditMenuDocking:=RegisterIDEMenuCommand(SourceEditorMenuRoot, 'Docking',
           lisMVDocking);
  {$IFNDEF EnableIDEDocking}
  SrcEditMenuDocking.Visible:=false;
  {$ENDIF}
end;

{ TSourceEditor }

{ The constructor for @link(TSourceEditor).
  AOwner is the @link(TSourceNotebook)
  and the AParent is usually a page of a @link(TNotebook) }
constructor TSourceEditor.Create(AOwner: TComponent; AParent: TWinControl);
Begin
  inherited Create;
  FAOwner := AOwner;
  if (FAOwner<>nil) and (FAOwner is TSourceNotebook) then
    FSourceNoteBook:=TSourceNotebook(FAOwner)
  else
    FSourceNoteBook:=nil;

  FSyntaxHighlighterType:=lshNone;
  FErrorLine:=-1;
  FErrorColumn:=-1;
  FExecutionLine:=-1;
  FExecutionMark := nil;
  FHasExecutionMarks := False;
  FMarksRequested := False;
  FLineInfoNotification := TIDELineInfoNotification.Create;
  FLineInfoNotification.AddReference;
  FLineInfoNotification.OnChange := @LineInfoNotificationChange;

  CreateEditor(AOwner,AParent);

  FEditPlugin := TSynEditPlugin1.Create(FEditor);
  // IMPORTANT: when you change below, don't forget updating UnbindEditor
  FEditPlugin.OnLinesInserted := @LinesInserted;
  FEditPlugin.OnLinesDeleted := @LinesDeleted;
end;

destructor TSourceEditor.Destroy;
begin
//writeln('TSourceEditor.Destroy A ',FEditor.Name);
  if (FAOwner<>nil) and (FEditor<>nil) then begin
    UnbindEditor;
    FEditor.Visible:=false;
    FEditor.Parent:=nil;
    if SourceEditorMarks<>nil then
      SourceEditorMarks.DeleteAllForEditor(FEditor);
    TSourceNotebook(FAOwner).FSourceEditorList.Remove(Self);
    // free the synedit control after processing the events
    Application.ReleaseComponent(FEditor);
  end;
  FEditor:=nil;
  CodeBuffer := nil;
  if (DebugBoss <> nil) and (DebugBoss.LineInfo <> nil) then
    DebugBoss.LineInfo.RemoveNotification(FLineInfoNotification);
  FLineInfoNotification.ReleaseReference;
//writeln('TSourceEditor.Destroy B ');
  inherited Destroy;
//writeln('TSourceEditor.Destroy END ');
end;

{------------------------------G O T O   L I N E  -----------------------------}
Function TSourceEditor.GotoLine(Value: Integer): Integer;
Var
  P: TPoint;
  NewTopLine: integer;
Begin
  TSourceNotebook(Owner).AddJumpPointClicked(Self);
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
begin
  InitGotoDialog;
  GotoDialog.Edit1.Text:='';
  GetDialogPosition(GotoDialog.Width,GotoDialog.Height,NewLeft,NewTop);
  GotoDialog.SetBounds(NewLeft,NewTop,GotoDialog.Width,GotoDialog.Height);
  if (GotoDialog.ShowModal = mrOK) then
    GotoLine(StrToIntDef(GotoDialog.Edit1.Text,1));
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
var ALeft,ATop:integer;
    bSelectedTextOption: Boolean;
    DlgResult: TModalResult;
begin
  if SourceNotebook<>nil then
    SourceNotebook.InitFindDialog;
  //debugln('TSourceEditor.StartFindAndReplace A LazFindReplaceDialog.FindText="',dbgstr(LazFindReplaceDialog.FindText),'"');
  if ReadOnly then Replace := False;
  if Replace then
    LazFindReplaceDialog.Options :=
      LazFindReplaceDialog.Options + [ssoReplace, ssoReplaceAll]
  else
    LazFindReplaceDialog.Options :=
      LazFindReplaceDialog.Options - [ssoReplace, ssoReplaceAll];

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
    InputHistories.FindAutoComplete:=LazFindReplaceDialog.EnableAutoComplete;
    if DlgResult = mrCancel then
      exit;
    //debugln('TSourceEditor.StartFindAndReplace B LazFindReplaceDialog.FindText="',dbgstr(LazFindReplaceDialog.FindText),'"');

    Replace:=ssoReplace in LazFindReplaceDialog.Options;
    if Replace then
      InputHistories.AddToReplaceHistory(LazFindReplaceDialog.ReplaceText);
    InputHistories.AddToFindHistory(LazFindReplaceDialog.FindText);
    InputHistories.Save;
    DoFindAndReplace;
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
var
  OldOptions: TSynSearchOptions;
begin
  if snIncrementalFind in FSourceNoteBook.States
  then begin
    FSourceNoteBook.IncrementalSearch(True, False);
  end
  else if LazFindReplaceDialog.FindText = ''
  then begin
    StartFindAndReplace(False)
  end
  else begin
    OldOptions:=LazFindReplaceDialog.Options;
    LazFindReplaceDialog.Options:=LazFindReplaceDialog.Options
                                     -[ssoEntireScope,ssoReplaceAll];
    DoFindAndReplace;
    LazFindReplaceDialog.Options:=OldOptions;
  end;
End;

{---------------------------F I N D   P R E V I O U S ------------------------}
procedure TSourceEditor.FindPrevious;
var
  OldOptions: TSynSearchOptions;
begin
  if snIncrementalFind in FSourceNoteBook.States
  then begin
    FSourceNoteBook.IncrementalSearch(True, True);
  end
  else begin
    OldOptions:=LazFindReplaceDialog.Options;
    LazFindReplaceDialog.Options:=LazFindReplaceDialog.Options-[ssoEntireScope];
    if ssoBackwards in LazFindReplaceDialog.Options then
      LazFindReplaceDialog.Options:=LazFindReplaceDialog.Options-[ssoBackwards]
    else
      LazFindReplaceDialog.Options:=LazFindReplaceDialog.Options+[ssoBackwards];
    if LazFindReplaceDialog.FindText = '' then
      StartFindAndReplace(False)
    else
      DoFindAndReplace;
    LazFindReplaceDialog.Options:=OldOptions;
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
end;

procedure TSourceEditor.InitGotoDialog;
begin
  if GotoDialog=nil then
    GotoDialog := TfrmGoto.Create(SourceNotebook);
end;

function TSourceEditor.DoFindAndReplace: integer;
var
  OldCaretXY: TPoint;
  AText, ACaption: String;
  NewTopLine: integer;
begin
  Result:=0;
  if SourceNotebook<>nil then
    SourceNotebook.AddJumpPointClicked(Self);
  if (ssoReplace in LazFindReplaceDialog.Options)
  and ReadOnly then begin
    DebugLn(['TSourceEditor.DoFindAndReplace Read only']);
    exit;
  end;

  OldCaretXY:=EditorComponent.CaretXY;
  if EditorComponent.SelAvail and
     not(ssoSelectedOnly in LazFindReplaceDialog.Options)
  then begin
    // Adjust the cursor. to exclude the selection from being searched
    // needed for find next / find previous
    if ssoBackwards in LazFindReplaceDialog.Options then
      EditorComponent.LogicalCaretXY:=EditorComponent.BlockBegin
    else
      EditorComponent.LogicalCaretXY:=EditorComponent.BlockEnd
  end;
  //debugln('TSourceEditor.DoFindAndReplace A LazFindReplaceDialog.FindText="',dbgstr(LazFindReplaceDialog.FindText),'" ssoEntireScope=',dbgs(ssoEntireScope in LazFindReplaceDialog.Options),' ssoBackwards=',dbgs(ssoBackwards in LazFindReplaceDialog.Options));
  try
    Result:=EditorComponent.SearchReplace(
      LazFindReplaceDialog.FindText,LazFindReplaceDialog.ReplaceText,
      LazFindReplaceDialog.Options);
  except
    on E: ERegExpr do begin
      MessageDlg(lisUEErrorInRegularExpression,
        E.Message,mtError,[mbCancel],0);
      exit;
    end;
  end;

  if (OldCaretXY.X = EditorComponent.CaretX) and
     (OldCaretXY.Y = EditorComponent.CaretY) and
     not (ssoReplaceAll in LazFindReplaceDialog.Options) then
  begin
    ACaption := lisUENotFound;
    AText := Format(lisUESearchStringNotFound, [ValidUTF8String(LazFindReplaceDialog.FindText)]);
    MessageDlg(ACaption, AText, mtInformation, [mbOk], 0);
    TSourceNotebook(Owner).DeleteLastJumpPointClicked(Self);
  end else
  if (EditorComponent.CaretY <= EditorComponent.TopLine + 1) or
     (EditorComponent.CaretY >= EditorComponent.TopLine + EditorComponent.LinesInWindow - 1) then
  begin
    NewTopLine := EditorComponent.CaretY - (EditorComponent.LinesInWindow div 2);
    if NewTopLine < 1 then
      NewTopLine := 1;
    EditorComponent.TopLine := NewTopLine;
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
  writeln('TSourceEditor.FocusEditor A ',PageName,' ',FEditor.Name);
  {$ENDIF}
  if SourceNotebook<>nil then SourceNotebook.Visible:=true;
  FEditor.SetFocus;
  //DebugLn('TSourceEditor.FocusEditor ',dbgsName(FindOwnerControl(GetFocus)),' ',dbgs(GetFocus));
  {$IFDEF VerboseFocus}
  writeln('TSourceEditor.FocusEditor END ',PageName,' ',FEditor.Name);
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

Procedure TSourceEditor.ProcessCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
// these are normal commands for synedit (lower than ecUserFirst),
// define extra actions here
// for non synedit keys (bigger than ecUserFirst) use ProcessUserCommand
var
  AddChar: Boolean;
  s: String;
begin
  //DebugLn('TSourceEditor.ProcessCommand Command=',dbgs(Command));

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
        FSourceNoteBook.IncrementalSearchStr:=
          LeftStr(FSourceNoteBook.IncrementalSearchStr,
            length(FSourceNoteBook.IncrementalSearchStr)-1);
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

    else
      FSourceNoteBook.EndIncrementalFind;
    end;
  end;

  case Command of

  ecSelEditorTop, ecSelEditorBottom, ecEditorTop, ecEditorBottom:
    begin
      if FaOwner<>nil then
        TSourceNotebook(FaOwner).AddJumpPointClicked(Self);
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
     TSourceNotebook(FaOwner).BookmarkGotoNext(false);

  ecNextBookmark:
    TSourceNotebook(FaOwner).BookmarkGotoNext(true);

  ecGotoMarker0..ecGotoMarker9:
    TSourceNotebook(FaOwner).BookmarkGoto(Command - ecGotoMarker0);

  ecSetMarker0..ecSetMarker9:
    TSourceNotebook(FaOwner).BookmarkSet(Command - ecSetMarker0);

  ecToggleMarker0..ecToggleMarker9:
    TSourceNotebook(FaOwner).BookmarkSet(Command - ecToggleMarker0,true);

  end;
  //debugln('TSourceEditor.ProcessCommand B IdentCompletionTimer.AutoEnabled=',dbgs(SourceCompletionTimer.AutoEnabled));
end;

Procedure TSourceEditor.ProcessUserCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
// these are the keys above ecUserFirst
// define all extra keys here, that should not be handled by synedit
var
  I: Integer;
  P: TPoint;
  Texts, Texts2: String;
  Handled: boolean;
  LogCaret: TPoint;
Begin
  //debugln('TSourceEditor.ProcessUserCommand A ',dbgs(Command));
  Handled:=true;

  case Command of

  ecContextHelp:
    FindHelpForSourceAtCursor;

  ecIdentCompletion :
    StartIdentCompletion(true);

  ecShowCodeContext :
    SourceNotebook.StartShowCodeContext(true);

  ecWordCompletion :
    if not TCustomSynEdit(Sender).ReadOnly then begin
      SourceNotebook.CreateCompletionForm;
      CurrentCompletionType:=ctWordCompletion;
      TextS := FEditor.LineText;
      LogCaret:=FEditor.LogicalCaretXY;
      i := LogCaret.X - 1;
      if i > length(TextS) then
        TextS2 := ''
      else begin
        while (i > 0) and (TextS[i] in ['a'..'z','A'..'Z','0'..'9','_']) do
          dec(i);
        TextS2 := Trim(copy(TextS, i + 1, LogCaret.X - i - 1));
      end;
      with TCustomSynEdit(Sender) do begin
        P := Point(CaretXPix - length(TextS2)*CharWidth,CaretYPix + LineHeight + 1);
        P.X:=Max(0,Min(P.X,ClientWidth-aCompletion.Width));
        P := ClientToScreen(p);
      end;
      aCompletion.Editor:=TCustomSynEdit(Sender);
      aCompletion.Execute(TextS2,P.X,P.Y);
    end;

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

  ecSelectionTabs2Spaces:
    TabsToSpacesInSelection;

  ecSelectionComment:
    CommentSelection;

  ecSelectionUnComment:
    UncommentSelection;

  ecToggleComment:
    ToggleCommentSelection;

  ecSelectionConditional:
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

  ecInsertTodo:
    InsertTodo;

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
var OldBlockBegin, OldBlockEnd: TPoint;
begin
  if ReadOnly then exit;
  if not EditorComponent.SelAvail then exit;
  OldBlockBegin:=FEditor.BlockBegin;
  OldBlockEnd:=FEditor.BlockEnd;
  FEditor.BeginUpdate;
  FEditor.BeginUndoBlock;
  FEditor.SelText:=UpperCase(EditorComponent.SelText);
  FEditor.BlockBegin:=OldBlockBegin;
  FEditor.BlockEnd:=OldBlockEnd;
  FEditor.EndUndoBlock;
  FEditor.EndUpdate;
end;

{-------------------------------------------------------------------------------
  method TSourceEditor.LowerCaseSelection

  Turns current text selection lowercase.
-------------------------------------------------------------------------------}
procedure TSourceEditor.LowerCaseSelection;
var OldBlockBegin, OldBlockEnd: TPoint;
begin
  if ReadOnly then exit;
  if not EditorComponent.SelAvail then exit;
  OldBlockBegin:=FEditor.BlockBegin;
  OldBlockEnd:=FEditor.BlockEnd;
  FEditor.BeginUpdate;
  FEditor.BeginUndoBlock;
  FEditor.SelText:=LowerCase(EditorComponent.SelText);
  FEditor.BlockBegin:=OldBlockBegin;
  FEditor.BlockEnd:=OldBlockEnd;
  FEditor.EndUndoBlock;
  FEditor.EndUpdate;
end;

{-------------------------------------------------------------------------------
  method TSourceEditor.TabsToSpacesInSelection

  Convert all tabs into spaces in current text selection.
-------------------------------------------------------------------------------}
procedure TSourceEditor.TabsToSpacesInSelection;
var 
  OldBlockBegin, OldBlockEnd: TPoint;
begin
  if ReadOnly then exit;
  if not EditorComponent.SelAvail then exit;
  OldBlockBegin:=FEditor.BlockBegin;
  OldBlockEnd:=FEditor.BlockEnd;
  FEditor.BeginUpdate;
  FEditor.BeginUndoBlock;
  // ToDo: replace step by step to keep bookmarks and breakpoints
  FEditor.SelText:=TabsToSpaces(EditorComponent.SelText,
                                EditorComponent.TabWidth,FEditor.UseUTF8);
  FEditor.BlockBegin:=OldBlockBegin;
  FEditor.BlockEnd:=OldBlockEnd;
  FEditor.EndUndoBlock;
  FEditor.EndUpdate;
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
          if OldBlockStart.Y = OldBlockEnd.Y then
            Result := OldBlockStart.X
          else
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
  FEditor.BeginUpdate;
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
  FEditor.SelText:=AddConditional(EditorComponent.SelText,IsPascal);
  FEditor.EndUndoBlock;
  FEditor.EndUpdate;
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
var
  codelines: TStringList;
begin
  if ReadOnly then exit;
  if not EditorComponent.SelAvail then exit;
  FEditor.BeginUpdate;
  FEditor.BeginUndoBlock;
  // ToDo: replace step by step to keep bookmarks and breakpoints
  codelines := TStringList.Create;
  try
    codelines.Text := FEditor.SelText;
    FEditor.SelText := InvertAssignTool.InvertAssignment( codelines ).Text;
  finally
    codelines.Free;
  end;
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

procedure TSourceEditor.InsertTodo;
Var
  aTodoItem: TTodoItem;
begin
  //DebugLn(['TSourceEditor.InsertTodo ']);
  if ReadOnly then Exit;
  aTodoItem := ExecuteTodoDialog;
  try
    if Assigned(aTodoItem) then
      FEditor.InsertTextAtCaret(aTodoItem.AsComment);
  finally
    aTodoItem.Free;
  end;
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
begin
  // create or delete breakpoint
  // find breakpoint mark at line
  Marks := nil;
  try
    SourceEditorMarks.GetMarksForLine(FEditor, Line, Marks, MarkCount);
    BreakFound := False;
    for i := 0 to MarkCount - 1 do
    begin
      if not Marks[i].Visible then
        Continue;
      if Marks[i].IsBreakPoint then
      begin
        BreakFound := True;
        DebugBoss.DoDeleteBreakPointAtMark(Marks[i])
      end;
    end;
  finally
    FreeMem(Marks);
  end;

  if not BreakFound then
    DebugBoss.DoCreateBreakPoint(Filename, Line, True);
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
    SourceEditorMarks.GetMarksForLine(FEditor, Line, CurMarks, CurMarkCount);
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
begin
  if (FExecutionMark = nil) then
  begin
    FExecutionMark := TSourceMark.Create(EditorComponent, nil);
    SourceEditorMarks.Add(FExecutionMark);
    EditorComponent.Marks.Add(FExecutionMark);
    FExecutionMark.LineColorAttrib := ahaExecutionPoint;
    FExecutionMark.Priority := 1;
  end;

  FExecutionMark.Visible := ExecutionLine <> -1;
  if FExecutionMark.Visible then
  begin
    FExecutionMark.Line := ExecutionLine;
    if SourceEditorMarks.FindBreakPointMark(EditorComponent, ExecutionLine) <> nil then
      FExecutionMark.ImageIndex := SourceEditorMarks.CurrentLineBreakPointImg
    else
      FExecutionMark.ImageIndex := SourceEditorMarks.CurrentLineImg;
  end;
end;

procedure TSourceEditor.SetExecutionLine(NewLine: integer);
begin
  if fExecutionLine=NewLine then exit;
  fExecutionLine:=NewLine;
  UpdateExecutionSourceMark;
  EditorComponent.Invalidate;
end;

Function TSourceEditor.RefreshEditorSettings: Boolean;
Begin
  Result:=true;
  SetSyntaxHighlighterType(fSyntaxHighlighterType);
  EditorOpts.GetSynEditSettings(FEditor);
  SourceNotebook.UpdateActiveEditColors(FEditor);
end;

Procedure TSourceEditor.ccAddMessage(Texts: String);
Begin
  ErrorMsgs.Add(Texts);
End;

function TSourceEditor.AutoCompleteChar(Char: TUTF8Char; var AddChar: boolean;
  Category: TAutoCompleteOption): boolean;
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
  // A new word-break char is going to be inserted, so end the word here
  x2 := Min(x2, p.x);
  WordToken := copy(Line, x1, x2-x1);
  IdChars := FEditor.IdentChars;
  for i:=0 to FCodeTemplates.Completions.Count-1 do begin
    AToken:=FCodeTemplates.Completions[i];
    if AToken='' then continue;
    if AToken[1] in IdChars then
      SrcToken:=WordToken
    else
      SrcToken:=copy(Line,length(Line)-length(AToken)+1,length(AToken));
    //DebugLn(['TSourceEditor.AutoCompleteChar ',AToken,' SrcToken=',SrcToken,' CatName=',CatName,' Index=',FCodeTemplates.CompletionAttributes[i].IndexOfName(CatName)]);
    if (AnsiCompareText(AToken,SrcToken)=0)
    and (FCodeTemplates.CompletionAttributes[i].IndexOfName(CatName)>=0)
    then begin
      Result:=true;
      DebugLn(['TSourceEditor.AutoCompleteChar ',AToken,' SrcToken=',SrcToken,' CatName=',CatName,' Index=',FCodeTemplates.CompletionAttributes[i].IndexOfName(CatName)]);
      FCodeTemplates.ExecuteCompletion(AToken,FEditor);
      AddChar:=not FCodeTemplates.CompletionAttributes[i].IndexOfName(
                                     AutoCompleteOptionNames[acoRemoveChar])>=0;
      exit;
    end;
  end;
end;

procedure TSourceEditor.AutoCompleteBlock;
var
  XY: TPoint;
  NewCode: TCodeBuffer;
  NewX, NewY, NewTopLine: integer;
begin
  if not LazarusIDE.SaveSourceEditorChangesToCodeCache(PageIndex) then exit;
  XY:=FEditor.LogicalCaretXY;
  FEditor.BeginUndoBlock;
  try
    if not CodeToolBoss.CompleteBlock(CodeBuffer,XY.X,XY.Y,
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
    FEditor.EndUndoBlock;
  end;
end;

Procedure TSourceEditor.CreateEditor(AOwner: TComponent; AParent: TWinControl);
var
  NewName: string;
  i: integer;
  sync: TSynPluginSyncroEdit;
  bmp: TCustomBitmap;
Begin
  {$IFDEF IDE_DEBUG}
  writeln('TSourceEditor.CreateEditor  A ');
  {$ENDIF}
  if not assigned(FEditor) then Begin
    i:=0;
    repeat
      inc(i);
      NewName:='SynEdit'+IntToStr(i);
    until (AOwner.FindComponent(NewName)=nil);
    FEditor:=TSynEdit.Create(AOwner);
    FEditor.BeginUpdate;
    with FEditor do begin
      Name:=NewName;
      Text:='';
      Parent := AParent;
      Align := alClient;
      BookMarkOptions.EnableKeys := false;
      BookMarkOptions.LeftMargin:=1;
      WantTabs := true;

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
      OnClickLink := @EditorClickLink;
      OnMouseLink := @EditorMouseLink;
      OnKeyDown := @EditorKeyDown;
      // IMPORTANT: when you change above, don't forget updating UnbindEditor
    end;
    if FCodeTemplates<>nil then
      FCodeTemplates.AddEditor(FEditor);
    if aCompletion<>nil then
      aCompletion.AddEditor(FEditor);
    TSynPluginTemplateEdit.Create(FEditor);
    sync := TSynPluginSyncroEdit.Create(FEditor);
    bmp := CreateBitmapFromLazarusResource('tsynsyncroedit');
    sync.GutterGlyph.Assign(bmp);
    bmp.Free;
    RefreshEditorSettings;
    FEditor.EndUpdate;
  end else begin
    FEditor.Parent:=AParent;
  end;
end;

procedure TSourceEditor.SetCodeBuffer(NewCodeBuffer: TCodeBuffer);
begin
  if NewCodeBuffer=FCodeBuffer then exit;
  if FCodeBuffer<>nil then
    FCodeBuffer.RemoveChangeHook(@OnCodeBufferChanged);
  FCodeBuffer:=NewCodeBuffer;
  if FCodeBuffer <> nil then
  begin
    FCodeBuffer.AddChangeHook(@OnCodeBufferChanged);
    if (FIgnoreCodeBufferLock<=0) and (not FCodeBuffer.IsEqual(FEditor.Lines))
    then begin
      {$IFDEF IDE_DEBUG}
      debugln('');
      debugln('WARNING: TSourceEditor.SetCodeBuffer - loosing marks: ',Filename);
      debugln('');
      {$ENDIF}
      if assigned(FEditPlugin) then
        FEditPlugin.Enabled := False;
      FEditor.BeginUpdate;
      FCodeBuffer.AssignTo(FEditor.Lines,true);
      FEditorStampCommitedToCodetools:=FEditor.ChangeStamp;
      FEditor.EndUpdate;
      if assigned(FEditPlugin) then
        FEditPlugin.Enabled := True;
    end;
    if IsActiveOnNoteBook then SourceNotebook.UpdateStatusBar;
    if (DebugBoss.State in [dsPause, dsRun]) and
       not HasExecutionMarks and
       (FileName <> '') then
      FillExecutionMarks;
  end;
end;

procedure TSourceEditor.OnCodeBufferChanged(Sender: TSourceLog;
  SrcLogEntry: TSourceLogEntry);

  procedure MoveTxt(const StartPos, EndPos, MoveToPos: TPoint;
    DirectionForward: boolean);
  var Txt: string;
  begin
    if DirectionForward then begin
      FEditor.TextBetweenPointsEx[MoveToPos, MoveToPos, scamAdjust] :=
        FEditor.TextBetweenPoints[StartPos, EndPos];
      FEditor.TextBetweenPointsEx[StartPos, EndPos, scamAdjust] := '';
    end else begin
      Txt := FEditor.TextBetweenPoints[StartPos, EndPos];
      FEditor.TextBetweenPointsEx[StartPos, EndPos, scamAdjust] := '';
      FEditor.TextBetweenPointsEx[MoveToPos, MoveToPos, scamAdjust] := Txt;;
    end;
  end;

var
  StartPos, EndPos, MoveToPos: TPoint;
  CodeToolsInSync: Boolean;
begin
  {$IFDEF IDE_DEBUG}
  writeln('[TSourceEditor.OnCodeBufferChanged] A ',FIgnoreCodeBufferLock,' ',SrcLogEntry<>nil);
  {$ENDIF}
  if FIgnoreCodeBufferLock>0 then exit;
  CodeToolsInSync:=FEditorStampCommitedToCodetools=FEditor.ChangeStamp;
  if SrcLogEntry<>nil then begin
    FEditor.BeginUpdate;
    FEditor.BeginUndoBlock;
    case SrcLogEntry.Operation of
      sleoInsert:
        begin
          Sender.AbsoluteToLineCol(SrcLogEntry.Position,StartPos.Y,StartPos.X);
          if StartPos.Y>=1 then
            FEditor.TextBetweenPointsEx[StartPos, StartPos, scamAdjust] := SrcLogEntry.Txt;
        end;
      sleoDelete:
        begin
          Sender.AbsoluteToLineCol(SrcLogEntry.Position,StartPos.Y,StartPos.X);
          Sender.AbsoluteToLineCol(SrcLogEntry.Position+SrcLogEntry.Len,
            EndPos.Y,EndPos.X);
          if (StartPos.Y>=1) and (EndPos.Y>=1) then
            FEditor.TextBetweenPointsEx[StartPos, EndPos, scamAdjust] := '';
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
    FEditor.EndUndoBlock;
    FEditor.EndUpdate;
  end else begin
    {$IFDEF VerboseSrcEditBufClean}
    debugln(['TSourceEditor.OnCodeBufferChanged clean up ',TCodeBuffer(Sender).FileName,' ',Sender=CodeBuffer,' ',Filename]);
    DumpStack;
    {$ENDIF}
    FEditor.BeginUpdate;
    Sender.AssignTo(FEditor.Lines,false);
    FEditor.EndUpdate;
  end;
  if CodeToolsInSync then begin
    // synedit and codetools were in sync -> mark as still in sync
    FEditorStampCommitedToCodetools:=FEditor.ChangeStamp;
  end;
end;

procedure TSourceEditor.StartIdentCompletion(JumpToError: boolean);
var
  I: Integer;
  P: TPoint;
  TextS, TextS2: String;
  LogCaret: TPoint;
begin
  {$IFDEF VerboseIDECompletionBox}
  debugln(['TSourceEditor.StartIdentCompletion JumpToError: ',JumpToError]);
  {$ENDIF}
  if (FEditor.ReadOnly) or (CurrentCompletionType<>ctNone) then exit;
  SourceNotebook.fIdentCompletionJumpToError:=JumpToError;
  SourceNotebook.CreateCompletionForm;
  CurrentCompletionType:=ctIdentCompletion;
  TextS := FEditor.LineText;
  LogCaret:=FEditor.LogicalCaretXY;
  i := LogCaret.X - 1;
  if i > length(TextS) then
    TextS2 := ''
  else begin
    while (i > 0) and (TextS[i] in ['a'..'z','A'..'Z','0'..'9','_']) do
      dec(i);
    TextS2 := Trim(copy(TextS, i + 1, LogCaret.X - i - 1));
  end;
  with FEditor do begin
    P := Point(CaretXPix - length(TextS2)*CharWidth,CaretYPix + LineHeight + 1);
    P.X:=Max(0,Min(P.X,ClientWidth-aCompletion.Width));
    P := ClientToScreen(p);
  end;
  aCompletion.Editor:=FEditor;
  aCompletion.TheForm.Font := FEditor.Font;
  aCompletion.Execute(TextS2,P.X,P.Y);
  {$IFDEF VerboseIDECompletionBox}
  debugln(['TSourceEditor.StartIdentCompletion END aCompletion.TheForm.Visible=',aCompletion.TheForm.Visible]);
  {$ENDIF}
end;

procedure TSourceEditor.IncreaseIgnoreCodeBufferLock;
begin
  inc(FIgnoreCodeBufferLock);
end;

procedure TSourceEditor.DecreaseIgnoreCodeBufferLock;
begin
  if FIgnoreCodeBufferLock<=0 then raise Exception.Create('unbalanced calls');
  dec(FIgnoreCodeBufferLock);
end;

procedure TSourceEditor.UpdateCodeBuffer;
// copy the source from EditorComponent to codetools
begin
  if FEditor.ChangeStamp=FEditorStampCommitedToCodetools then exit;
  {$IFDEF IDE_DEBUG}
  if FCodeBuffer=nil then begin
    debugln('');
    debugln('*********** Oh, no: UpdateCodeBuffer ************ ');
    debugln('');
  end;
  {$ENDIF}
  if FCodeBuffer=nil then exit;
  //DebugLn(['TSourceEditor.UpdateCodeBuffer ',FileName]);
  IncreaseIgnoreCodeBufferLock;
  FEditor.BeginUpdate;
  FCodeBuffer.Assign(FEditor.Lines);
  FEditorStampCommitedToCodetools:=FEditor.ChangeStamp;
  FEditor.EndUpdate;
  DecreaseIgnoreCodeBufferLock;
end;

function TSourceEditor.NeedsUpdateCodeBuffer: boolean;
begin
  Result:=FEditor.ChangeStamp<>FEditorStampCommitedToCodetools;
end;

Function TSourceEditor.GetSource: TStrings;
Begin
  //return synedit's source.
  Result := FEditor.Lines;
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
  p:=SourceNotebook.FindPageWithEditor(Self);
  NewPageName:=FPageName;
  if Modified then NewPageName:='*'+NewPageName;
  if SourceNotebook.NoteBook.Pages[p]<>NewPageName then
    SourceNotebook.NoteBook.Pages[p]:=NewPageName;
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
  const NewText: string);
begin
  if ReadOnly then Exit;
  FEditor.TextBetweenPointsEx[Point(1,StartLine),
                            Point(length(FEditor.Lines[Endline-1])+1,EndLine),
                            scamEnd] := NewText;
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
  //writeln('TSourceEditor.EncloseSelection A NewCaretXY=',NewCaretXY.X,',',NewCaretXY.Y,
  //  ' "',NewSelection,'"');
  FEditor.SelText:=NewSelection;
  FEditor.LogicalCaretXY:=NewCaretXY;
end;

Function TSourceEditor.GetModified: Boolean;
Begin
  Result := FModified or FEditor.Modified;
end;

procedure TSourceEditor.SetModified(const NewValue: Boolean);
var
  OldModified: Boolean;
begin
  OldModified := Modified;
  FModified := NewValue;
  if not FModified then
  begin
    FEditor.Modified:=false; // needed for the undo stack
    FEditor.MarkTextAsSaved;
    FEditorStampCommitedToCodetools:=FEditor.ChangeStamp;
  end;
  if OldModified <> Modified then
    UpdatePageName;
end;

Function TSourceEditor.GetInsertMode: Boolean;
Begin
  Result := FEditor.Insertmode;
end;

Function TSourceEditor.Close: Boolean;
Begin
  Result := True;
  If Assigned(FOnBeforeClose) then
    FOnBeforeClose(Self);

  Visible := False;
  if aCompletion<>nil then
    aCompletion.RemoveEditor(FEditor);
  SourceEditorMarks.DeleteAllForEditor(FEditor);
  UnbindEditor;
  FEditor.Parent:=nil;
  CodeBuffer := nil;
  If Assigned(FOnAfterClose) then FOnAfterClose(Self);
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

Procedure TSourceEditor.ReParent(AParent: TWInControl);
Begin
  CreateEditor(FAOwner,AParent);
End;

procedure TSourceEditor.SetCodeTemplates(
  NewCodeTemplates: TSynEditAutoComplete);
begin
  if NewCodeTemplates<>FCodeTemplates then begin
    if FCodeTemplates<>nil then
      FCodeTemplates.RemoveEditor(FEditor);
    FCodeTemplates:=NewCodeTemplates;
    if FCodeTemplates<>nil then
      FCodeTemplates.AddEditor(FEditor);
  end;
end;

procedure TSourceEditor.SetPopupMenu(NewPopupMenu: TPopupMenu);
begin
  if NewPopupMenu<>FPopupMenu then begin
    FPopupMenu:=NewPopupMenu;
    if FEditor<>nil then FEditor.PopupMenu:=NewPopupMenu;
  end;
end;

function TSourceEditor.GetFilename: string;
begin
  if FCodeBuffer<>nil then
    Result:=FCodeBuffer.Filename
  else
    Result:='';
end;

function TSourceEditor.GetEditorControl: TWinControl;
begin
  Result:=FEditor;
end;

function TSourceEditor.GetCodeToolsBuffer: TObject;
begin
  Result:=CodeBuffer;
end;

Procedure TSourceEditor.EditorMouseMoved(Sender: TObject;
  Shift: TShiftState; X,Y: Integer);
begin
//  Writeln('MouseMove in Editor',X,',',Y);
  if Assigned(OnMouseMove) then
    OnMouseMove(Self,Shift,X,Y);
end;

procedure TSourceEditor.EditorMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
//  Writeln('MouseWheel in Editor');
  if Assigned(OnMouseWheel) then
    OnMouseWheel(Self, Shift, WheelDelta, MousePos, Handled)
end;

Function TSourceEditor.GetWordAtPosition(Position: TPoint): String;
var
  CaretPos: TPoint;
begin
  Result := '';
  Caretpos := ScreenToTextPosition(GetCaretPosfromCursorPos(Position));
  Result := GetWordFromCaret(CaretPos);
end;

Procedure TSourceEditor.EditorMouseDown(Sender: TObject; Button: TMouseButton;
   Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(OnMouseDown) then
    OnMouseDown(Sender, Button, Shift, X,Y);
end;

procedure TSourceEditor.EditorMouseLink(
  Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
begin
  //DebugLn(['TSourceEditor.EditorMouseLink ',X,',',Y]);
  if Assigned(OnMouseLink) then
    OnMouseLink(Sender, X, Y, AllowMouseLink);
end;

procedure TSourceEditor.EditorClickLink(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //DebugLn(['TSourceEditor.EditorClickLink ',X,',',Y]);
  if Assigned(OnClickLink) then
    OnClickLink(Sender, Button, Shift, X,Y);
end;

Procedure TSourceEditor.EditorKeyDown(Sender: TObject; var Key: Word; Shift :
  TShiftState);
begin
  //DebugLn('TSourceEditor.EditorKeyDown A ',dbgsName(Sender),' ',IntToStr(Key));
  if Assigned(OnKeyDown) then
    OnKeyDown(Sender, Key, Shift);
end;

Function TSourceEditor.GetCaretPosFromCursorPos(const CursorPos: TPoint): TPoint;
var
  NewTopLine: Integer;
  LineHeight: Integer;
  LineNum: Integer;
  XLine: Integer;
begin
  //Figure out the line number
  NewTopLine := FEditor.TopLine;
  LineHeight := FEditor.LineHeight;
  if CursorPos.Y > 1 then
    LineNum := CursorPos.Y div LineHeight
  else
    LineNum := 1;
  LineNum := LineNum + NewTopLine;
  XLine := CursorPos.X div FEditor.CharWidth;
  if XLine = 0 then inc(XLine);

  Result.X := XLine;
  Result.Y := LineNum;
end;

{-------------------------------------------------------------------------------
  method TSourceEditor.CenterCursor
  Params: none
  Result: none

  Center the current cursor line in editor.
-------------------------------------------------------------------------------}
procedure TSourceEditor.CenterCursor;
var NewTopLine: integer;
begin
  NewTopLine:=EditorComponent.CaretY-((EditorComponent.LinesInWindow-1) div 2);
  if NewTopLine<1 then NewTopLine:=1;
  EditorComponent.TopLine:=NewTopLine;
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
  Result:=Project1.UnitWithEditorIndex(PageIndex);
end;

function TSourceEditor.GetDesigner(LoadForm: boolean): TIDesigner;
var
  AProjectFile: TLazProjectFile;
begin
  AProjectFile:=GetProjectFile;
  if AProjectFile<>nil then
    Result:=LazarusIDE.GetDesignerWithProjectFile(AProjectFile,LoadForm)
  else
    Result:=nil;
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
  OldOptions, NewOptions: TSynSearchOptions;
  o: TSrcEditSearchOption;
begin
  OldOptions:=LazFindReplaceDialog.Options;
  NewOptions:=[];
  for o:=Low(TSrcEditSearchOption) to High(TSrcEditSearchOption) do
    if o in SearchOptions then
      Include(NewOptions,SrcEdit2SynEditSearchOption[o]);
  LazFindReplaceDialog.Options:=NewOptions;
  Result:=DoFindAndReplace;
  LazFindReplaceDialog.Options:=OldOptions;
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
  if (FSourceNoteBook=nil) then exit;
  FSourceNoteBook.SetActiveSE(Self);
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

function TSourceEditor.PositionInSelection(const APosition: TPoint): Boolean;
begin
  Result := CaretInSelection(GetCaretPosfromCursorPos(APosition));
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
  if HasExecutionMarks then Exit;

  ASource := FileName;
  idx := DebugBoss.LineInfo.IndexOf(ASource);
  if (idx = -1) then
  begin
    if not FMarksRequested then
    begin
      FMarksRequested := True;
      DebugBoss.LineInfo.AddNotification(FLineInfoNotification);
      DebugBoss.LineInfo.Request(ASource);
    end;
    Exit;
  end;

  for i := 0 to EditorComponent.Lines.Count - 1 do
  begin
    Addr := DebugBoss.LineInfo.Values[idx, i];
    if (Addr <> 0) then
      EditorComponent.SetDebugMarks(i, i);
  end;
  FHasExecutionMarks := True;
end;

procedure TSourceEditor.ClearExecutionMarks;
begin
  EditorComponent.ClearDebugMarks;
  FHasExecutionMarks := False;
  FMarksRequested := False;
  if (FLineInfoNotification <> nil) and (DebugBoss <> nil) and (DebugBoss.LineInfo <> nil) then
    DebugBoss.LineInfo.RemoveNotification(FLineInfoNotification);
end;

procedure TSourceEditor.LineInfoNotificationChange(const ASender: TObject; const ASource: String);
begin
  if ASource = FileName then
    FillExecutionMarks;
end;

Function TSourceEditor.GetWordAtCurrentCaret: String;
var
  CaretPos: TPoint;
begin
  Result := '';
  CaretPos.Y := CurrentCursorYLine;
  CaretPos.X := CurrentCursorXLine;
  Result := GetWordFromCaret(ScreenToTextPosition(CaretPos));
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
  MessagesView.SrcEditLinesInsertedDeleted(Filename,FirstLine,-Count);
end;

procedure TSourceEditor.LinesInserted(Sender: TObject; FirstLine,
  Count: Integer);
begin
  // notify the notebook that lines were Inserted.
  // marks will use this to update themselves
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
begin
  inherited Create(AOwner);
  IDESearchInText:=@SearchInText;
  Visible:=false;
  Name:=NonModalIDEWindowNames[nmiwSourceNoteBookName];
  Caption := locWndSrcEditor;
  KeyPreview:=true;
  FProcessingCommand := false;

  SourceEditorWindow:=Self;

  EnvironmentOptions.IDEWindowLayoutList.Apply(Self,Name);
  ControlDocker:=TLazControlDocker.Create(Self);
  ControlDocker.Name:='SourceEditor';
  {$IFDEF EnableIDEDocking}
  ControlDocker.Manager:=LazarusIDE.DockingManager;
  {$ENDIF}

  FSourceEditorList := TList.Create;

  // code templates
  FCodeTemplateModul:=TSynEditAutoComplete.Create(Self);
  with FCodeTemplateModul do begin
    if FileExistsUTF8(EditorOpts.CodeTemplateFilename) then
      AutoCompleteList.LoadFromFile(UTF8ToSys(EditorOpts.CodeTemplateFilename))
    else
      if FileExistsUTF8('lazarus.dci') then
        AutoCompleteList.LoadFromFile(UTF8ToSys('lazarus.dci'));
    IndentToTokenStart:=EditorOpts.CodeTemplateIndentToTokenStart;
    OnTokenNotFound:=@OnCodeTemplateTokenNotFound;
    OnExecuteCompletion:=@OnCodeTemplateExecuteCompletion;
    EndOfTokenChr:=' ()[]{},.;:"+-*^@$\<>=''';
  end;

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
  SourceEditorMarks.OnGetSourceEditor:=@OnSourceMarksGetSourceEditor;
  SourceEditorMarks.OnGetFilename:=@OnSourceMarksGetFilename;
  SourceEditorMarks.OnAction:=@OnSourceMarksAction;

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

  Application.AddOnUserInputHandler(@OnApplicationUserInput,true);
end;

destructor TSourceNotebook.Destroy;
var
  i: integer;
begin
  FProcessingCommand:=false;
  for i:=FSourceEditorList.Count-1 downto 0 do
    Editors[i].Free;
  if Notebook<>nil then begin
    FreeThenNil(Notebook);
  end;
  FKeyStrokes.Free;
  FCodeTemplateModul.Free;
  FSourceEditorList.Free;
  FreeAndNil(Gotodialog);

  FreeThenNil(CodeContextFrm);
  FreeThenNil(SrcEditHintWindow);
  FreeThenNil(aCompletion);
  FreeThenNil(FMouseHintTimer);
  FreeThenNil(FHintWindow);
  SourceEditorWindow:=nil;

  inherited Destroy;
  if SourceNotebook=Self then
    SourceNotebook:=nil;
end;

procedure TSourceNotebook.InitMacros(AMacroList: TTransferMacroList);
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

procedure TSourceNotebook.CreateCompletionForm;
var
  i: Integer;
begin
  {$IFDEF VerboseIDECompletionBox}
  debugln(['TSourceNotebook.CreateCompletionForm START ',dbgsname(aCompletion)]);
  {$ENDIF}
  // completion form
  if aCompletion<>nil then exit;
  aCompletion := TSynCompletion.Create(Self);
    with aCompletion do
      Begin
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
      end;

  for i:=0 to EditorCount-1 do
    aCompletion.AddEditor(Editors[i].FEditor);
end;

procedure TSourceNotebook.ShowFPDocEditor;
begin
  DoShowFPDocEditor;
  UpdateFPDocEditor;
end;

procedure TSourceNotebook.UpdateFPDocEditor;
var
  SrcEdit: TSourceEditor;
  CaretPos: TPoint;
begin
  if FPDocEditor = nil then exit;
  SrcEdit:=GetActiveSE;
  if SrcEdit=nil then exit;
  CaretPos := SrcEdit.EditorComponent.CaretXY;
  FPDocEditor.UpdateFPDocEditor(SrcEdit.Filename,CaretPos);
end;

function TSourceNotebook.OnSynCompletionPaintItem(const AKey: string;
  ACanvas: TCanvas;  X, Y: integer; ItemSelected: boolean;
  Index: integer): boolean;
var
  MaxX: Integer;
begin
  with ACanvas do begin
    if (aCompletion<>nil) and (aCompletion.Editor<>nil) then
      Font:=aCompletion.Editor.Font
    else begin
      Font.Height:=EditorOpts.EditorFontHeight; // set Height before name for XLFD !
      Font.Name:=EditorOpts.EditorFont;
    end;
    Font.Style:=[];
    if not ItemSelected then
      Font.Color:=FActiveEditDefaultFGColor
    else
      Font.Color:=FActiveEditSelectedFGColor;
  end;
  MaxX:=aCompletion.TheForm.ClientWidth;
  PaintCompletionItem(AKey,ACanvas,X,Y,MaxX,ItemSelected,Index,aCompletion,
                      CurrentCompletionType,aCompletion.Editor.Highlighter);
  Result:=true;
end;

function TSourceNotebook.OnSynCompletionMeasureItem(const AKey: string;
  ACanvas: TCanvas; ItemSelected: boolean; Index: integer): TPoint;
var
  MaxX: Integer;
begin
  with ACanvas do begin
    if (aCompletion<>nil) and (aCompletion.Editor<>nil) then
      Font:=aCompletion.Editor.Font
    else begin
      Font.Height:=EditorOpts.EditorFontHeight; // set Height before name of XLFD !
      Font.Name:=EditorOpts.EditorFont;
    end;
    Font.Style:=[];
    if not ItemSelected then
      Font.Color:=FActiveEditDefaultFGColor
    else
      Font.Color:=FActiveEditSelectedFGColor;
  end;
  MaxX := Screen.Width-20;
  Result := PaintCompletionItem(AKey,ACanvas,0,0,MaxX,ItemSelected,Index,
                                aCompletion,CurrentCompletionType,nil,True);
  if CurCompletionControl<>nil then
    Result.Y:=CurCompletionControl.FontHeight;
end;

procedure TSourceNotebook.OnWordCompletionGetSource(var Source: TStrings;
  SourceIndex: integer);
var TempEditor: TSourceEditor;
  i:integer;
begin
  TempEditor:=GetActiveSE;
  if SourceIndex=0 then begin
    Source:=TempEditor.EditorComponent.Lines;
  end else begin
    i:=0;
    while (i<FSourceEditorList.Count) do begin
      if Editors[i]<>TempEditor then dec(SourceIndex);
      if SourceIndex=0 then begin
        Source:=Editors[i].EditorComponent.Lines;
        exit;
      end;
      inc(i);
    end;
    Source:=nil;
  end;
end;

procedure TSourceNotebook.OnSourceCompletionTimer(Sender: TObject);

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
    Result:=false;
    SrcEdit:=GetActiveSE;
    if SrcEdit=nil then exit;
    Line:=SrcEdit.FEditor.LineText;
    LogCaret:=SrcEdit.FEditor.LogicalCaretXY;
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
    while (p<=LogCaret.X) and (p<=length(Line)) do begin
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
    SrcEdit.StartIdentCompletion(false);
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
  //debugln('TSourceNotebook.OnIdentCompletionTimer');
  SourceCompletionTimer.Enabled:=false;
  SourceCompletionTimer.AutoEnabled:=false;
  TempEditor:=GetActiveSE;
  if (TempEditor<>nil)
  and (ComparePoints(TempEditor.EditorComponent.CaretXY,SourceCompletionCaretXY)=0)
  and TempEditor.EditorComponent.Focused
  then begin
    if CheckStartIdentCompletion then begin
    end else if CheckTemplateCompletion then begin
    end;
  end;
end;

procedure TSourceNotebook.OnCodeTemplateTokenNotFound(Sender: TObject;
  AToken: string; AnEditor: TCustomSynEdit; var Index:integer);
var P:TPoint;
begin
  //writeln('TSourceNotebook.OnCodeTemplateTokenNotFound ',AToken,',',AnEditor.ReadOnly,',',CurrentCompletionType=ctNone);
  if (AnEditor.ReadOnly=false) and (CurrentCompletionType=ctNone) then begin
    SourceNotebook.CreateCompletionForm;
    CurrentCompletionType:=ctTemplateCompletion;
    with AnEditor do begin
      P := Point(CaretXPix - length(AToken)*CharWidth,CaretYPix + LineHeight + 1);
      P.X:=Max(0,Min(P.X,ClientWidth-aCompletion.Width));
      P := ClientToScreen(p);
    end;
    aCompletion.Editor:=AnEditor;
    aCompletion.Execute(AToken,P.X,P.Y);
  end;
end;

procedure TSourceNotebook.OnCodeTemplateExecuteCompletion(
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
    SrcEdit:=GetActiveEditor;
  //debugln('TSourceNotebook.OnCodeTemplateExecuteCompletion A ',dbgsName(SrcEdit),' ',dbgsName(ASynAutoComplete.Editor));

  TemplateName:=ASynAutoComplete.Completions[Index];
  TemplateValue:=ASynAutoComplete.CompletionValues[Index];
  TemplateComment:=ASynAutoComplete.CompletionComments[Index];
  TemplateAttr:=ASynAutoComplete.CompletionAttributes[Index];
  ExecuteCodeTemplate(SrcEdit,TemplateName,TemplateValue,TemplateComment,
                      ASynAutoComplete.EndOfTokenChr,TemplateAttr,
                      ASynAutoComplete.IndentToTokenStart);
end;

procedure TSourceNotebook.OnSynCompletionSearchPosition(var APosition:integer);
// prefix changed -> filter list
var
  i,x:integer;
  CurStr,s:Ansistring;
  SL:TStringList;
  ItemCnt: Integer;
begin
  if CurCompletionControl=nil then exit;
  case CurrentCompletionType of

    ctIdentCompletion:
      begin
        // rebuild completion list
        APosition:=0;
        CurStr:=CurCompletionControl.CurrentString;
        CodeToolBoss.IdentifierList.Prefix:=CurStr;
        ItemCnt:=CodeToolBoss.IdentifierList.GetFilteredCount;
        SL:=TStringList.Create;
        try
          sl.Capacity:=ItemCnt;
          for i:=0 to ItemCnt-1 do
            SL.Add('Dummy'); // these entries are not shown
          CurCompletionControl.ItemList:=SL;
        finally
          SL.Free;
        end;
      end;

    ctTemplateCompletion:
      begin
        // search CurrentString in bold words (words after #3'B')
        CurStr:=CurCompletionControl.CurrentString;
        i:=0;
        while i<CurCompletionControl.ItemList.Count do begin
          s:=CurCompletionControl.ItemList[i];
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
        CurStr:=CurCompletionControl.CurrentString;
        SL:=TStringList.Create;
        try
          aWordCompletion.GetWordList(SL, CurStr, false, 100);
          CurCompletionControl.ItemList:=SL;
        finally
          SL.Free;
        end;
      end;

  end;
end;

procedure TSourceNotebook.OnSynCompletionCompletePrefix(Sender: TObject);
var
  OldPrefix: String;
  NewPrefix: String;
  SL: TStringList;
  AddPrefix: String;
begin
  if CurCompletionControl=nil then exit;
  OldPrefix:=CurCompletionControl.CurrentString;
  NewPrefix:=OldPrefix;

  case CurrentCompletionType of

  ctIdentCompletion:
    begin
      NewPrefix:=CodeToolBoss.IdentifierList.CompletePrefix(OldPrefix);
    end;

  ctWordCompletion:
    begin
      aWordCompletion.CompletePrefix(OldPrefix,NewPrefix,false);
    end;

  end;

  if NewPrefix<>OldPrefix then begin
    AddPrefix:=copy(NewPrefix,length(OldPrefix)+1,length(NewPrefix));
    CurCompletionControl.Editor.InsertTextAtCaret(AddPrefix);
    if CurrentCompletionType=ctWordCompletion then begin
      SL:=TStringList.Create;
      try
        aWordCompletion.GetWordList(SL, NewPrefix, false, 100);
        CurCompletionControl.ItemList:=SL;
      finally
        SL.Free;
      end;
    end;
    CurCompletionControl.CurrentString:=NewPrefix;
  end;
end;

procedure TSourceNotebook.OnSynCompletionNextChar(Sender: TObject);
var
  NewPrefix: String;
  SrcEdit: TSourceEditor;
  Line: String;
  Editor: TSynEdit;
  LogCaret: TPoint;
  CharLen: LongInt;
  AddPrefix: String;
begin
  if CurCompletionControl=nil then exit;
  SrcEdit:=GetActiveSE;
  if SrcEdit=nil then exit;
  Editor:=SrcEdit.EditorComponent;
  LogCaret:=Editor.LogicalCaretXY;
  if LogCaret.Y>=Editor.Lines.Count then exit;
  Line:=SrcEdit.EditorComponent.Lines[LogCaret.Y-1];
  if LogCaret.X>length(Line) then exit;
  CharLen:=UTF8CharacterLength(@Line[LogCaret.X]);
  AddPrefix:=copy(Line,LogCaret.X,CharLen);
  NewPrefix:=CurCompletionControl.CurrentString+AddPrefix;
  //debugln('TSourceNotebook.OnSynCompletionNextChar NewPrefix="',NewPrefix,'" LogCaret.X=',dbgs(LogCaret.X));
  inc(LogCaret.X);
  CurCompletionControl.Editor.LogicalCaretXY:=LogCaret;
  CurCompletionControl.CurrentString:=NewPrefix;
end;

procedure TSourceNotebook.OnSynCompletionPrevChar(Sender: TObject);
var
  NewPrefix: String;
  SrcEdit: TSourceEditor;
  Editor: TSynEdit;
  NewLen: LongInt;
begin
  if CurCompletionControl=nil then exit;
  NewPrefix:=CurCompletionControl.CurrentString;
  if NewPrefix='' then exit;
  SrcEdit:=GetActiveSE;
  if SrcEdit=nil then exit;
  Editor:=SrcEdit.EditorComponent;
  Editor.CaretX:=Editor.CaretX-1;
  NewLen:=UTF8FindNearestCharStart(PChar(NewPrefix),length(NewPrefix),
                                   length(NewPrefix))-1;
  NewPrefix:=copy(NewPrefix,1,NewLen);
  CurCompletionControl.CurrentString:=NewPrefix;
end;

procedure TSourceNotebook.OnSynCompletionKeyPress(Sender: TObject;
  var Key: Char);
begin
  if CurCompletionControl=nil then exit;
  if (System.Pos(Key,CurCompletionControl.EndOfTokenChr)>0) then begin
    // identifier completed
    //debugln('TSourceNotebook.OnSynCompletionKeyPress A');
    CurCompletionControl.TheForm.OnValidate(Sender,Key,[]);
    //debugln('TSourceNotebook.OnSynCompletionKeyPress B');
    Key:=#0;
  end;
end;

procedure TSourceNotebook.OnSynCompletionUTF8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if CurCompletionControl=nil then exit;
  if (length(UTF8Key)=1)
  and (System.Pos(UTF8Key[1],CurCompletionControl.EndOfTokenChr)>0) then begin
    // identifier completed
    //debugln('TSourceNotebook.OnSynCompletionUTF8KeyPress A');
    CurCompletionControl.TheForm.OnValidate(Sender,UTF8Key,[]);
    //debugln('TSourceNotebook.OnSynCompletionKeyPress B');
    UTF8Key:='';
  end else begin
    aCompletion.Editor.CommandProcessor(ecChar,UTF8Key,nil);
    UTF8Key:='';
  end;
  //debugln('TSourceNotebook.OnSynCompletionKeyPress B UTF8Key=',dbgstr(UTF8Key));
end;

procedure TSourceNotebook.OnSynCompletionPositionChanged(Sender: TObject);
begin
  if SrcEditHintWindow<>nil then
    SrcEditHintWindow.UpdateHints;
end;

procedure TSourceNotebook.DeactivateCompletionForm;
var
  ActSE: TSourceEditor;
  OldCompletionControl: TSynCompletion;
begin
  if CurCompletionControl=nil then exit;

  // clear the IdentifierList (otherwise it would try to update everytime
  // the codetools are used)
  CodeToolBoss.IdentifierList.Clear;

  OldCompletionControl:=CurCompletionControl;
  CurCompletionControl:=nil;
  OldCompletionControl.Deactivate;

  CurrentCompletionType:=ctNone;
  ActSE:=GetActiveSE;
  if ActSE<>nil then begin
    LCLIntf.ShowCaret(ActSE.EditorComponent.Handle);
    ActSE.EditorComponent.SetFocus;
  end;
end;

procedure TSourceNotebook.InitIdentCompletion(S: TStrings);
var
  i: integer;
  Handled: boolean;
  Abort: boolean;
  Prefix: string;
  ItemCnt: Integer;
begin
  Prefix := CurCompletionControl.CurrentString;
  if Assigned(OnInitIdentCompletion) then begin
    OnInitIdentCompletion(Self,fIdentCompletionJumpToError,Handled,Abort);
    if Handled then begin
      if Abort then exit;
      // add one entry per item
      CodeToolBoss.IdentifierList.Prefix:=Prefix;
      ItemCnt:=CodeToolBoss.IdentifierList.GetFilteredCount;
      //DebugLn('InitIdentCompletion B Prefix=',Prefix,' ItemCnt=',IntToStr(ItemCnt));
      CurCompletionControl.Position:=0;
      for i:=0 to ItemCnt-1 do
        s.Add('Dummy');
      exit;
    end;
  end;
end;

procedure TSourceNotebook.ccComplete(var Value: string; SourceValue: string;
  var SourceStart, SourceEnd: TPoint; KeyChar: TUTF8Char; Shift: TShiftState);
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
  SrcEdit: TSourceEditor;
  NewCaretXY: TPoint;
  CursorToLeft: integer;
  NewValue: String;
  Editor: TSynEdit;
  OldCompletionType: TCompletionType;
Begin
  {$IFDEF VerboseIDECompletionBox}
  debugln(['TSourceNotebook.ccComplete START']);
  {$ENDIF}
  if CurCompletionControl=nil then
  begin
    Value := SourceValue;
    exit;
  end;
  OldCompletionType:=CurrentCompletionType;
  case CurrentCompletionType of

    ctIdentCompletion:
      begin
        // add to history
        CodeToolBoss.IdentifierHistory.Add(
          CodeToolBoss.IdentifierList.FilteredItems[aCompletion.Position]);
        // get value
        NewValue:=GetIdentCompletionValue(aCompletion,KeyChar,
                                          ValueType,CursorToLeft);
        if ValueType=icvIdentifier then ;
        // insert value plus special chars like brackets, semicolons, ...
        SrcEdit:=GetActiveSE;
        Editor:=SrcEdit.EditorComponent;
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
          FCodeTemplateModul.ExecuteCompletion(Value,
                                               GetActiveSE.EditorComponent);
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

  DeactivateCompletionForm;

  //DebugLn(['TSourceNotebook.ccComplete ',KeyChar,' ',OldCompletionType=ctIdentCompletion]);
  SrcEdit:=GetActiveSE;
  Editor:=SrcEdit.EditorComponent;
  if (KeyChar='.') and (OldCompletionType=ctIdentCompletion) then
  begin
    SourceCompletionCaretXY:=Editor.CaretXY;
    SourceCompletionTimer.AutoEnabled:=true;
  end;
End;

Procedure TSourceNotebook.ccCancel(Sender: TObject);
// user cancels completion form
begin
  if CurCompletionControl=nil then exit;
  {$IFDEF VerboseIDECompletionBox}
  debugln(['TSourceNotebook.ccCancel START']);
  //debugln(GetStackTrace(true));
  {$ENDIF}
  DeactivateCompletionForm;
end;

Procedure TSourceNotebook.ccExecute(Sender: TObject);
// init completion form
// called by aCompletion.OnExecute just before showing
var
  S: TStrings;
  Prefix: String;
  I: Integer;
  NewStr: String;
  CurEdit: TSynEdit;
Begin
  {$IFDEF VerboseIDECompletionBox}
  debugln(['TSourceNotebook.ccExecute START']);
  {$ENDIF}
  CurCompletionControl := Sender as TSynCompletion;
  S := TStringList.Create;
  Prefix := CurCompletionControl.CurrentString;
  CurEdit:=GetActiveSE.EditorComponent;
  case CurrentCompletionType of
   ctIdentCompletion:
     InitIdentCompletion(S);

   ctWordCompletion:
     begin
       ccSelection:='';
     end;

   ctTemplateCompletion:
     begin
       ccSelection:='';
       for I:=0 to FCodeTemplateModul.Completions.Count-1 do begin
         NewStr:=FCodeTemplateModul.Completions[I];
         if NewStr<>'' then begin
           NewStr:=#3'B'+NewStr+#3'b';
           while length(NewStr)<10+4 do NewStr:=NewStr+' ';
           NewStr:=NewStr+' '+FCodeTemplateModul.CompletionComments[I];
           S.Add(NewStr);
         end;
       end;
     end;

  end;

  CurCompletionControl.ItemList := S;
  S.Free;
  CurCompletionControl.CurrentString:=Prefix;
  // set colors
  if (CurEdit<>nil) and (CurCompletionControl.TheForm<>nil) then begin
    with CurCompletionControl.TheForm do begin
      BackgroundColor:=FActiveEditDefaultBGColor;
      clSelect:=FActiveEditSelectedBGColor;
      TextColor:=FActiveEditDefaultFGColor;
      TextSelectedColor:=FActiveEditSelectedFGColor;
      //writeln('TSourceNotebook.ccExecute A Color=',DbgS(Color),
      // ' clSelect=',DbgS(clSelect),
      // ' TextColor=',DbgS(TextColor),
      // ' TextSelectedColor=',DbgS(TextSelectedColor),
      // '');
    end;
    if CurrentCompletionType=ctIdentCompletion then
      StartShowCodeHelp
    else if SrcEditHintWindow<>nil then
      SrcEditHintWindow.HelpEnabled:=false;
  end;
End;

Function TSourceNotebook.CreateNotebook: Boolean;
Begin
  {$IFDEF IDE_DEBUG}
  writeln('[TSourceNotebook.CreateNotebook] START');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}
  CheckHeapWrtMemCnt('[TSourceNotebook.CreateNotebook] A ');
  {$ENDIF}
  {$IFDEF IDE_DEBUG}
  writeln('[TSourceNotebook.CreateNotebook] A');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}
  CheckHeapWrtMemCnt('[TSourceNotebook.CreateNotebook] B ');
  {$ENDIF}
  Result := False;
  if not assigned(Notebook) then
    Begin
      Result := True;
      {$IFDEF IDE_DEBUG}
      writeln('[TSourceNotebook.CreateNotebook] B');
      {$ENDIF}
      Notebook := TNotebook.Create(self);
      {$IFDEF IDE_DEBUG}
      writeln('[TSourceNotebook.CreateNotebook] C');
      {$ENDIF}
      {$IFDEF IDE_MEM_CHECK}
      CheckHeapWrtMemCnt('[TSourceNotebook.CreateNotebook] C ');
      {$ENDIF}
      with Notebook do
        Begin
          Name:='SrcEditNotebook';
          Parent := Self;
          {$IFDEF IDE_DEBUG}
          writeln('[TSourceNotebook.CreateNotebook] D');
          {$ENDIF}
          Align := alClient;
          if PageCount>0 then
            Pages.Strings[0] := 'unit1'
          else
            Pages.Add('unit1');
          PageIndex := 0;   // Set it to the first page
          if not (nbcPageListPopup in GetCapabilities) then
            PopupMenu := SrcPopupMenu;
          if EditorOpts.ShowTabCloseButtons then
            Options:=Options+[nboShowCloseButtons]
          else
            Options:=Options-[nboShowCloseButtons];
          OnPageChanged := @NotebookPageChanged;
          OnCloseTabClicked:=@CloseTabClicked;
          OnMouseDown:=@NotebookMouseDown;
          ShowHint:=true;
          OnShowHint:=@NotebookShowTabHint;
          {$IFDEF IDE_DEBUG}
          writeln('[TSourceNotebook.CreateNotebook] E');
          {$ENDIF}
          Visible := true;
          {$IFDEF IDE_DEBUG}
          writeln('[TSourceNotebook.CreateNotebook] F');
          {$ENDIF}
          {$IFDEF IDE_MEM_CHECK}
          CheckHeapWrtMemCnt('[TSourceNotebook.CreateNotebook] F ');
          {$ENDIF}
        end; //with
      Show;  //used to display the code form
    end;
  {$IFDEF IDE_DEBUG}
  writeln('[TSourceNotebook.CreateNotebook] END');
  {$ENDIF}
  {$IFDEF IDE_MEM_CHECK}
  CheckHeapWrtMemCnt('[TSourceNotebook.CreateNotebook] END ');
  {$ENDIF}
End;

procedure TSourceNotebook.EditorPropertiesClicked(Sender: TObject);
begin
  if Assigned(FOnEditorPropertiesClicked) then
    FOnEditorPropertiesClicked(Sender);
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
          DebugLn(['TSourceNotebook.EncodingClicked Change file to ',SrcEdit.CodeBuffer.DiskEncoding]);
          if (not SrcEdit.CodeBuffer.IsVirtual)
          and (LazarusIDE.DoSaveEditorFile(SrcEdit.PageIndex,[])<>mrOk) then begin
            DebugLn(['TSourceNotebook.EncodingClicked LazarusIDE.DoSaveEditorFile failed']);
          end;
        end else if CurResult=mrOK then begin
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
          SrcEdit.CodeBuffer.AssignTo(SrcEdit.EditorComponent.Lines,true);
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
  ASrcEdit: TSourceEditor;
  BookMarkID, BookMarkX, BookMarkY: integer;
  MarkSrcEdit: TSourceEditor;
  MarkDesc: String;
  MarkEditorIndex: Integer;
  MarkMenuItem: TIDEMenuItem;
  EditorComp: TSynEdit;
  Marks: PSourceMark;
  MarkCount: integer;
  i: Integer;
  CurMark: TSourceMark;
  EditorPopupPoint, EditorCaret: TPoint;
  SelAvail: Boolean;
  SelAvailAndWritable: Boolean;
  CurFilename: String;
  CurWordAtCursor: String;
  AtIdentifier: Boolean;
begin
  //DebugLn(['TSourceNotebook.SrcPopUpMenuPopup ',dbgsName(Sender)]);
  //SourceEditorMenuRoot.WriteDebugReport('TSourceNotebook.SrcPopUpMenuPopup START ',true);
  //SourceEditorMenuRoot.ConsistencyCheck;

  SourceEditorMenuRoot.BeginUpdate;
  try
    RemoveUserDefinedMenuItems;
    RemoveContextMenuItems;

    ASrcEdit:=
         FindSourceEditorWithEditorComponent(TPopupMenu(Sender).PopupComponent);
    if ASrcEdit=nil then begin
      ASrcEdit:=GetActiveSE;
      if ASrcEdit=nil then begin
        DebugLn(['TSourceNotebook.SrcPopUpMenuPopup ASrcEdit=nil ',dbgsName(TPopupMenu(Sender).PopupComponent)]);
        exit;
      end;
    end;
    EditorComp:=ASrcEdit.EditorComponent;

    // Readonly, ShowLineNumbers
    SrcEditMenuReadOnly.MenuItem.Checked:=ASrcEdit.ReadOnly;
    SrcEditMenuShowLineNumbers.MenuItem.Checked :=
      EditorComp.Gutter.LineNumberPart.Visible;
    UpdateHighlightMenuItems;
    UpdateEncodingMenuItems;

    // bookmarks
    for BookMarkID:=0 to 9 do begin
      MarkDesc:=' '+IntToStr(BookMarkID);
      MarkSrcEdit:=FindBookmark(BookMarkID);
      if (MarkSrcEdit<>nil)
      and MarkSrcEdit.EditorComponent.GetBookMark(BookMarkID,BookMarkX,BookMarkY)
      then begin
        MarkEditorIndex:=FindPageWithEditor(MarkSrcEdit);
        MarkDesc:=MarkDesc+': '+Notebook.Pages[MarkEditorIndex]
          +' ('+IntToStr(BookMarkY)+','+IntToStr(BookMarkX)+')';
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
    SrcEditMenuMoveEditorLeft.MenuItem.Enabled:=
                     (NoteBook<>nil) and (NoteBook.PageCount>1);
    SrcEditMenuMoveEditorRight.MenuItem.Enabled:=
                     (NoteBook<>nil) and (NoteBook.PageCount>1);
    SrcEditMenuMoveEditorFirst.MenuItem.Enabled:=
                     (NoteBook<>nil) and (NoteBook.PageCount>1)
                     and (NoteBook.PageIndex>0);
    SrcEditMenuMoveEditorLast.MenuItem.Enabled:=
                     (NoteBook<>nil) and (NoteBook.PageCount>1)
                     and (NoteBook.PageIndex<(NoteBook.PageCount-1));

    EditorPopupPoint:=EditorComp.ScreenToClient(SrcPopUpMenu.PopupPoint);
    if EditorPopupPoint.X>EditorComp.GutterWidth then begin
      // user clicked on text
      // collect some flags
      SelAvail:=ASrcEdit.EditorComponent.SelAvail;
      SelAvailAndWritable:=SelAvail and (not ASrcEdit.ReadOnly);
      SrcEditMenuEncloseSelection.Enabled := SelAvailAndWritable;
      SrcEditMenuExtractProc.Enabled := SelAvailAndWritable;
      SrcEditMenuInvertAssignment.Enabled := SelAvailAndWritable;
      CurWordAtCursor:=ASrcEdit.GetWordAtCurrentCaret;
      AtIdentifier:=IsValidIdent(CurWordAtCursor);
      // enable menu items
      SrcEditMenuFindIdentifierReferences.Enabled:=AtIdentifier;
      SrcEditMenuRenameIdentifier.Enabled:=AtIdentifier
                                           and (not ASrcEdit.ReadOnly);
      SrcEditMenuShowAbstractMethods.Enabled:=not ASrcEdit.ReadOnly;
      SrcEditMenuShowEmptyMethods.Enabled:=not ASrcEdit.ReadOnly;
      SrcEditMenuFindOverloads.Enabled:=AtIdentifier;
    end else
    begin
      EditorCaret := EditorComp.PhysicalToLogicalPos(EditorComp.PixelsToRowColumn(EditorPopupPoint));
      // user clicked on gutter
      SourceEditorMarks.GetMarksForLine(EditorComp, EditorCaret.y,
                                        Marks, MarkCount);
      if Marks <> nil then
      begin
        for i := 0 to MarkCount - 1 do
        begin
          CurMark := Marks[i];
          CurMark.CreatePopupMenuItems(@AddUserDefinedPopupMenuItem);
        end;
        FreeMem(Marks);
      end;
    end;

    // add context specific menu items
    CurFilename:=ASrcEdit.FileName;
    if (FilenameIsAbsolute(CurFilename)) then begin
      if FilenameIsPascalUnit(CurFilename) then begin
        if FileExistsUTF8(ChangeFileExt(CurFilename,'.lfm')) then
          AddContextPopupMenuItem(Format(lisOpenLfm,
            [ChangeFileExt(ExtractFileName(CurFilename),'.lfm')]),
            true,@OnPopupMenuOpenLFMFile);
        if FileExistsUTF8(ChangeFileExt(CurFilename,'.lrs')) then
          AddContextPopupMenuItem(Format(lisOpenLfm,
            [ChangeFileExt(ExtractFileName(CurFilename),'.lrs')]),
            true,@OnPopupMenuOpenLRSFile);
        if FileExistsUTF8(ChangeFileExt(CurFilename,'.s')) then
          AddContextPopupMenuItem(Format(lisOpenLfm,
            [ChangeFileExt(ExtractFileName(CurFilename),'.s')]),
            true,@OnPopupMenuOpenSFile);
      end;
      if (CompareFileExt(CurFilename,'.lfm',true)=0) then begin
        if FileExistsUTF8(ChangeFileExt(CurFilename,'.pas')) then
          AddContextPopupMenuItem(Format(lisOpenLfm,
            [ChangeFileExt(ExtractFileName(CurFilename),'.pas')]),
            true,@OnPopupMenuOpenPasFile);
        if FileExistsUTF8(ChangeFileExt(CurFilename,'.pp')) then
          AddContextPopupMenuItem(Format(lisOpenLfm,
            [ChangeFileExt(ExtractFileName(CurFilename),'.pp')]),
            true,@OnPopupMenuOpenPPFile);
        if FileExistsUTF8(ChangeFileExt(CurFilename,'.p')) then
          AddContextPopupMenuItem(Format(lisOpenLfm,
            [ChangeFileExt(ExtractFileName(CurFilename),'.p')]),
            true,@OnPopupMenuOpenPFile);
      end;
      if (CompareFileExt(CurFilename,'.lpi',true)=0)
      or (CompareFileExt(CurFilename,'.lpk',true)=0) then begin
        AddContextPopupMenuItem(Format(lisOpenLfm,
          [ExtractFileName(CurFilename)]),true,@OnPopupMenuOpenFile);
      end;
    end;

    if Assigned(OnPopupMenu) then OnPopupMenu(@AddContextPopupMenuItem);

    SourceEditorMenuRoot.NotifySubSectionOnShow(Self);
  finally
    SourceEditorMenuRoot.EndUpdate;
  end;
  //SourceEditorMenuRoot.WriteDebugReport('TSourceNotebook.SrcPopUpMenuPopup END ',true);
  //SourceEditorMenuRoot.ConsistencyCheck;
end;

procedure TSourceNotebook.NotebookShowTabHint(Sender: TObject;
  HintInfo: PHintInfo);
var
  Tabindex: integer;
  ASrcEdit: TSourceEditor;
begin
  if (NoteBook=nil) or (HintInfo=nil) then exit;
  TabIndex:=NoteBook.TabIndexAtClientPos(
                                      Notebook.ScreenToClient(Mouse.CursorPos));
  if TabIndex<0 then exit;
  ASrcEdit:=FindSourceEditorWithPageIndex(TabIndex);
  if ASrcEdit=nil then exit;
  if ASrcEdit.CodeBuffer<>nil then begin
    HintInfo^.HintStr:=ASrcEdit.CodeBuffer.Filename;
  end;
end;

function TSourceNotebook.OnSourceMarksGetFilename(ASourceEditor: TObject
  ): string;
begin
  if (ASourceEditor=nil) or (not (ASourceEditor is TSourceEditor)) then
    RaiseException('TSourceNotebook.OnSourceMarksGetFilename');
  Result:=TSourceEditor(ASourceEditor).Filename;
end;

procedure TSourceNotebook.OnSourceMarksAction(AMark: TSourceMark;
  AAction: TListNotification);
var
  Editor: TSourceEditor;
begin
  Editor := FindSourceEditorWithEditorComponent(AMark.SynEdit);
  if Editor = nil then
    Exit;

  if AMark.IsBreakPoint and (Editor.FExecutionMark <> nil) and
    (AMark.CompareEditorAndLine(Editor.FExecutionMark.SynEdit, Editor.FExecutionMark.Line) = 0) then
    Editor.UpdateExecutionSourceMark;
end;

function TSourceNotebook.GetItems(Index: integer): TSourceEditorInterface;
begin
  Result:=TSourceEditorInterface(FSourceEditorList[Index]);
end;

function TSourceNotebook.OnSourceMarksGetSourceEditor(ASynEdit: TCustomSynEdit
  ): TObject;
begin
  Result:=FindSourceEditorWithEditorComponent(ASynEdit);
end;

Procedure TSourceNotebook.BuildPopupMenu;
var
  i: Integer;
begin
  //debugln('TSourceNotebook.BuildPopupMenu');

  SrcPopupMenu := TPopupMenu.Create(Self);
  with SrcPopupMenu do
  begin
    AutoPopup := True;
    OnPopup :=@SrcPopupMenuPopup;
    Images := IDEImages.Images_16;
  end;

  // assign the root TMenuItem to the registered menu root.
  // This will automatically create all registered items
  {$IFDEF VerboseMenuIntf}
  SrcPopupMenu.Items.WriteDebugReport('TSourceNotebook.BuildPopupMenu ');
  SourceEditorMenuRoot.ConsistencyCheck;
  {$ENDIF}
  SourceEditorMenuRoot.MenuItem:=SrcPopupMenu.Items;

  SrcEditMenuFindDeclaration.OnClick:=@FindDeclarationClicked;
  SrcEditMenuProcedureJump.OnClick:=@ProcedureJumpClicked;
  SrcEditMenuFindNextWordOccurrence.OnClick:=@FindNextWordOccurrenceClicked;
  SrcEditMenuFindPrevWordOccurrence.OnClick:=@FindPrevWordOccurrenceClicked;
  SrcEditMenuFindinFiles.OnClick:=@FindInFilesClicked;
  SrcEditMenuOpenFileAtCursor.OnClick:=@OpenAtCursorClicked;

  SrcEditMenuClosePage.OnClick:=@CloseClicked;
  SrcEditMenuCloseOtherPages.OnClick:=@CloseOtherPagesClicked;
  SrcEditMenuCut.OnClick:=@CutClicked;
  SrcEditMenuCopy.OnClick:=@CopyClicked;
  SrcEditMenuPaste.OnClick:=@PasteClicked;
  SrcEditMenuCopyFilename.OnClick:=@CopyFilenameClicked;
  for i:=0 to 9 do begin
    SrcEditSubMenuGotoBookmarks.FindByName('GotoBookmark'+IntToStr(i))
                                           .OnClick:=@BookmarkGotoClicked;
    SrcEditSubMenuToggleBookmarks.FindByName('ToggleBookmark'+IntToStr(i))
                                            .OnClick:=@BookMarkToggleClicked;
  end;
  SrcEditMenuSetFreeBookmark.OnClick:=@BookMarkSetFreeClicked;
  SrcEditMenuNextBookmark.OnClick:=@BookMarkNextClicked;
  SrcEditMenuPrevBookmark.OnClick:=@BookMarkPrevClicked;

  SrcEditMenuToggleBreakpoint.OnClick:=@ToggleBreakpointClicked;
  SrcEditMenuAddWatchAtCursor.OnClick:=@AddWatchAtCursor;
  SrcEditMenuRunToCursor.OnClick:=@RunToClicked;
  SrcEditMenuViewCallStack.OnClick:=@ViewCallStackClick;

  SrcEditMenuMoveEditorLeft.OnClick:=@MoveEditorLeftClicked;
  SrcEditMenuMoveEditorRight.OnClick:=@MoveEditorRightClicked;
  SrcEditMenuMoveEditorFirst.OnClick:=@MoveEditorFirstClicked;
  SrcEditMenuMoveEditorLast.OnClick:=@MoveEditorLastClicked;
  SrcEditMenuMoveEditorLast.OnClick:=@MoveEditorLastClicked;
  SrcEditMenuDocking.OnClick:=@DockingClicked;

  SrcEditMenuInsertTodo.OnClick:=@InsertTodoClicked;

  SrcEditMenuCompleteCode.OnClick:=@CompleteCodeMenuItemClick;
  SrcEditMenuEncloseSelection.OnClick:=@EncloseSelectionMenuItemClick;
  SrcEditMenuExtractProc.OnClick:=@ExtractProcMenuItemClick;
  SrcEditMenuInvertAssignment.OnClick:=@InvertAssignmentMenuItemClick;
  SrcEditMenuFindIdentifierReferences.OnClick:=
                                         @FindIdentifierReferencesMenuItemClick;
  SrcEditMenuRenameIdentifier.OnClick:=@RenameIdentifierMenuItemClick;
  SrcEditMenuShowAbstractMethods.OnClick:=@ShowAbstractMethodsMenuItemClick;
  SrcEditMenuShowEmptyMethods.OnClick:=@ShowEmptyMethodsMenuItemClick;
  SrcEditMenuShowUnusedUnits.OnClick:=@ShowUnusedUnitsMenuItemClick;
  SrcEditMenuFindOverloads.OnClick:=@FindOverloadsMenuItemClick;

  SrcEditMenuReadOnly.OnClick:=@ReadOnlyClicked;
  SrcEditMenuShowLineNumbers.OnClick:=@ToggleLineNumbersClicked;
  SrcEditMenuShowUnitInfo.OnClick:=@ShowUnitInfo;
  SrcEditMenuEditorProperties.OnClick:=@EditorPropertiesClicked;
end;

function TSourceNotebook.GetCompletionBoxPosition: integer;
begin
  Result:=-1;
  if CurCompletionControl<>nil then
    Result:=CurCompletionControl.Position;
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
  if Assigned(OnEditorChanged) then
    OnEditorChanged(Sender);
End;

function TSourceNotebook.NewSE(PageNum: Integer): TSourceEditor;
begin
  {$IFDEF IDE_DEBUG}
  writeln('TSourceNotebook.NewSE A ');
  {$ENDIF}
  if CreateNotebook then Pagenum := 0;
  {$IFDEF IDE_DEBUG}
  writeln('TSourceNotebook.NewSE A2 ');
  {$ENDIF}
  if Pagenum < 0 then begin
    // add a new page right to the current
    Pagenum := Notebook.PageIndex+1;
    Notebook.Pages.Insert(PageNum,FindUniquePageName('',-1));
  end;
  {$IFDEF IDE_DEBUG}
  writeln('TSourceNotebook.NewSE B  ',Notebook.PageIndex,',',NoteBook.Pages.Count);
  {$ENDIF}
  Result := TSourceEditor.Create(Self,Notebook.Page[PageNum]);
  Result.EditorComponent.BeginUpdate;
  FSourceEditorList.Add(Result);
  Result.CodeTemplates:=CodeTemplateModul;
  Notebook.PageIndex := Pagenum;
  //debugln(['TSourceNotebook.NewSE C GetActiveSE=Result=',GetActiveSE=Result]);
  Result.FPageName:=NoteBook.Pages[Pagenum];
  Result.EditorComponent.BookMarkOptions.BookmarkImages := SourceEditorMarks.ImgList;
  Result.EditorComponent.Gutter.MarksPart.DebugMarksImageIndex := SourceEditorMarks.SourceLineImg;
  Result.PopupMenu := SrcPopupMenu;
  Result.OnEditorChange := @EditorChanged;
  Result.OnMouseMove := @EditorMouseMove;
  Result.OnMouseDown := @EditorMouseDown;
  Result.OnMouseWheel := @EditorMouseWheel;
  Result.OnClickLink := @EditorClickLink;
  Result.OnMouseLink := @EditorMouseLink;
  Result.OnKeyDown := @EditorKeyDown;
  Result.EditorComponent.Beautifier.OnGetDesiredIndent := @EditorGetIndent;

  Result.EditorComponent.EndUpdate;
  {$IFDEF IDE_DEBUG}
  writeln('TSourceNotebook.NewSE end ');
  {$ENDIF}
end;

function TSourceNotebook.FindSourceEditorWithPageIndex(
  PageIndex: integer): TSourceEditor;
var I: integer;
  TempEditor: TControl;
begin
  Result := nil;
  if (FSourceEditorList=nil)
    or (Notebook=nil)
    or (PageIndex<0) or (PageIndex>=Notebook.PageCount) then exit;
  TempEditor:=nil;
  with Notebook.Page[PageIndex] do
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
  if (FSourceEditorList=nil) or (FSourceEditorList.Count=0)
    or (Notebook=nil) or (Notebook.PageIndex<0) then exit;
  Result:=FindSourceEditorWithPageIndex(Notebook.PageIndex);
end;

procedure TSourceNotebook.SetActiveSE(SrcEdit: TSourceEditor);
var
  i: integer;
begin
  i:=FindPageWithEditor(SrcEdit);
  if i>=0 then
    NoteBook.PageIndex:=i;
end;

function TSourceNotebook.GetActiveEditor: TSourceEditorInterface;
begin
  Result:=GetActiveSE;
end;

procedure TSourceNotebook.SetActiveEditor(const AValue: TSourceEditorInterface
  );
begin
  SetActiveSE(AValue as TSourceEditor);
end;

procedure TSourceNotebook.CheckCurrentCodeBufferChanged;
var
  SrcEdit: TSourceEditor;
begin
  SrcEdit:=GetActiveSE;
  if SrcEdit = nil then Exit;
  if FLastCodeBuffer=SrcEdit.CodeBuffer then exit;
  FLastCodeBuffer:=SrcEdit.CodeBuffer;
  if Assigned(OnCurrentCodeBufferChanged) then
    OnCurrentCodeBufferChanged(Self);
end;

procedure TSourceNotebook.LockAllEditorsInSourceChangeCache;
// lock all sourceeditors that are to be modified by the CodeToolBoss
var i: integer;
begin
  for i:=0 to EditorCount-1 do begin
    if CodeToolBoss.SourceChangeCache.BufferIsModified(Editors[i].CodeBuffer)
    then begin
      with Editors[i].EditorComponent do begin
        BeginUpdate;
        BeginUndoBlock;
      end;
    end;
  end;
end;

procedure TSourceNotebook.UnlockAllEditorsInSourceChangeCache;
// unlock all sourceeditors that were modified by the CodeToolBoss
var i: integer;
begin
  for i:=0 to EditorCount-1 do begin
    if CodeToolBoss.SourceChangeCache.BufferIsModified(Editors[i].CodeBuffer)
    then begin
      with Editors[i].EditorComponent do begin
        EndUndoBlock;
        EndUpdate;
      end;
    end;
  end;
end;

function TSourceNotebook.GetDiffFiles: TDiffFiles;
var
  i: Integer;
  SrcEdit: TSourceEditor;
begin
  Result:=TDiffFiles.Create;
  if Notebook=nil then exit;
  for i:=0 to NoteBook.PageCount-1 do begin
    SrcEdit:=FindSourceEditorWithPageIndex(i);
    Result.Add(TDiffFile.Create(NoteBook.Pages[i],i,SrcEdit.SelectionAvailable));
  end;
end;

procedure TSourceNotebook.GetSourceText(PageIndex: integer;
  OnlySelection: boolean; var Source: string);
var
  SrcEdit: TSourceEditor;
begin
  SrcEdit:=FindSourceEditorWithPageIndex(PageIndex);
  if SrcEdit=nil then
    Source:=''
  else
    Source:=SrcEdit.GetText(OnlySelection);
end;

Function TSourceNotebook.Empty: Boolean;
Begin
  Result := (not assigned(Notebook)) or (Notebook.PageCount = 0);
end;

procedure TSourceNotebook.FindReplaceDlgKey(Sender: TObject; var Key: Word;
  Shift: TShiftState; FindDlgComponent: TFindDlgComponent);
var
  HistoryList: TStringList;
  CurText: string;
  CurIndex: integer;

  procedure SetHistoryText;
  var s: string;
  begin
    if FindReplaceDlgHistoryIndex[FindDlgComponent]>=0 then
      s:=HistoryList[FindReplaceDlgHistoryIndex[FindDlgComponent]]
    else
      s:=FindReplaceDlgUserText[FindDlgComponent];
    //writeln('  SetHistoryText "',s,'"');
    LazFindReplaceDialog.ComponentText[FindDlgComponent]:=s
  end;

  procedure FetchFocus;
  begin
    if Sender is TWinControl then
      TWinControl(Sender).SetFocus;
  end;

begin
  if FindDlgComponent=fdcText then
    HistoryList:=InputHistories.FindHistory
  else
    HistoryList:=InputHistories.ReplaceHistory;
  CurIndex:=FindReplaceDlgHistoryIndex[FindDlgComponent];
  CurText:=LazFindReplaceDialog.ComponentText[FindDlgComponent];
  //writeln('TSourceNotebook.FindReplaceDlgKey CurIndex=',CurIndex,' CurText="',CurText,'"');
  if Key=VK_UP then begin
    // go forward in history
    if CurIndex>=0 then begin
      if (HistoryList[CurIndex]<>CurText) then begin
        // save user text
        FindReplaceDlgUserText[FindDlgComponent]:=CurText;
      end;
      dec(FindReplaceDlgHistoryIndex[FindDlgComponent]);
      SetHistoryText;
    end;
    FetchFocus;
    Key:=VK_UNKNOWN;
  end else if Key=VK_DOWN then begin
    if (CurIndex<0)
    or (HistoryList[CurIndex]<>CurText) then
    begin
      // save user text
      FindReplaceDlgUserText[FindDlgComponent]:=CurText;
    end;
    // go back in history
    if CurIndex<HistoryList.Count-1 then
    begin
      inc(FindReplaceDlgHistoryIndex[FindDlgComponent]);
      SetHistoryText;
    end;
    FetchFocus;
    Key:=VK_UNKNOWN;
  end;
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

function TSourceNotebook.SomethingModified: boolean;
var i: integer;
begin
  Result:=false;
  for i:=0 to EditorCount-1 do Result:=Result or Editors[i].Modified;
end;

Procedure TSourceNotebook.NextEditor;
Begin
  if NoteBook=nil then exit;
  if Notebook.PageIndex < Notebook.PageCount-1 then
    Notebook.PageIndex := Notebook.PageIndex+1
  else
    NoteBook.PageIndex := 0;
  NotebookPageChanged(Self);
End;

Procedure TSourceNotebook.PrevEditor;
Begin
  if NoteBook=nil then exit;
  if Notebook.PageIndex > 0 then
    Notebook.PageIndex := Notebook.PageIndex-1
  else
    NoteBook.PageIndex := NoteBook.PageCount-1;
  NotebookPageChanged(Self);
End;

procedure TSourceNotebook.MoveEditor(OldPageIndex, NewPageIndex: integer);
begin
  if (NoteBook=nil) or (NoteBook.PageCount<=1)
  or (OldPageIndex=NewPageIndex)
  or (OldPageIndex<0) or (OldPageIndex>=Notebook.PageCount)
  or (NewPageIndex<0) or (NewPageIndex>=Notebook.PageCount) then exit;
  if Assigned(OnMovingPage) then
    OnMovingPage(Self,OldPageIndex,NewPageIndex);
  NoteBook.Pages.Move(OldPageIndex,NewPageIndex);
end;

procedure TSourceNotebook.MoveEditorLeft(PageIndex: integer);
begin
  if (NoteBook=nil) or (NoteBook.PageCount<=1) then exit;
  if PageIndex>0 then
    MoveEditor(PageIndex,PageIndex-1)
  else
    MoveEditor(PageIndex,NoteBook.PageCount-1);
end;

procedure TSourceNotebook.MoveEditorRight(PageIndex: integer);
begin
  if (NoteBook=nil) or (NoteBook.PageCount<=1) then exit;
  if PageIndex<Notebook.PageCount-1 then
    MoveEditor(PageIndex,PageIndex+1)
  else
    MoveEditor(PageIndex,0);
end;

procedure TSourceNotebook.MoveEditorFirst(PageIndex: integer);
begin
  if (NoteBook=nil) or (NoteBook.PageCount<=1) then exit;
  MoveEditor(PageIndex,0)
end;

procedure TSourceNotebook.MoveEditorLast(PageIndex: integer);
begin
  if (NoteBook=nil) or (NoteBook.PageCount<=1) then exit;
  MoveEditor(PageIndex,NoteBook.PageCount-1);
end;

procedure TSourceNotebook.MoveActivePageLeft;
begin
  if (NoteBook=nil) then exit;
  MoveEditorLeft(NoteBook.PageIndex);
end;

procedure TSourceNotebook.MoveActivePageRight;
begin
  if (NoteBook=nil) then exit;
  MoveEditorRight(NoteBook.PageIndex);
end;

procedure TSourceNotebook.MoveActivePageFirst;
begin
  if (NoteBook=nil) then exit;
  MoveEditorFirst(NoteBook.PageIndex);
end;

procedure TSourceNotebook.MoveActivePageLast;
begin
  if (NoteBook=nil) then exit;
  MoveEditorLast(NoteBook.PageIndex);
end;

Procedure TSourceNotebook.FindClicked(Sender: TObject);
var TempEditor:TSourceEditor;
Begin
  TempEditor:=GetActiveSE;
  if TempEditor <> nil then TempEditor.StartFindAndReplace(false);
End;

Procedure TSourceNotebook.ReplaceClicked(Sender: TObject);
var TempEditor:TSourceEditor;
Begin
  TempEditor:=GetActiveSE;
  if TempEditor <> nil then TempEditor.StartFindAndReplace(true);
End;

procedure TSourceNotebook.IncrementalFindClicked(Sender: TObject);
begin
  BeginIncrementalFind;
end;

Procedure TSourceNotebook.FindNextClicked(Sender: TObject);
var TempEditor:TSourceEditor;
Begin
  TempEditor:=GetActiveSe;
  if TempEditor <> nil then TempEditor.FindNextUTF8;
End;

Procedure TSourceNotebook.FindPreviousClicked(Sender: TObject);
var TempEditor:TSourceEditor;
Begin
  TempEditor:=GetActiveSe;
  if TempEditor <> nil then TempEditor.FindPrevious;
End;

function TSourceNotebook.CreateFindInFilesDialog: TLazFindInFilesDialog;
begin
  Result := TLazFindInFilesDialog.Create(Application);
  LoadFindInFilesHistory(Result);
end;

procedure TSourceNotebook.LoadFindInFilesHistory(ADialog: TLazFindInFilesDialog);

  procedure AssignToComboBox(AComboBox: TComboBox; Strings: TStrings);
  begin
    AComboBox.Items.Assign(Strings);
    if AComboBox.Items.Count>0 then
      AComboBox.ItemIndex := 0;
  end;

  procedure AddFileToComboBox(AComboBox: TComboBox; const Filename: string);
  var
    i: Integer;
  begin
    if Filename='' then exit;
    for i:=0 to AComboBox.Items.Count-1 do begin
      if CompareFilenames(Filename,AComboBox.Items[i])=0 then begin
        // move to front (but not top, top should be the last used directory)
        if i>2 then
          AComboBox.Items.Move(i,1);
        exit;
      end;
    end;
    // insert in front (but not top, top should be the last used directory)
    if AComboBox.Items.Count>0 then
      i:=1
    else
      i:=0;
    AComboBox.Items.Insert(i,Filename);
  end;

var
  SrcEdit: TSourceEditor;
begin
  if not Assigned(ADialog) then exit;
  SrcEdit:=GetActiveSE;
  with ADialog, InputHistories do
  begin
    //DebugLn('TSourceNotebook.LoadFindInFilesHistory ',dbgsName(TextToFindComboBox),' ',dbgsName(FindHistory));
    TextToFindComboBox.Items.Assign(FindHistory);
    ReplaceTextComboBox.Items.Assign(ReplaceHistory);
    if not EditorOpts.FindTextAtCursor then begin
      if TextToFindComboBox.Items.Count>0 then begin
        //debugln('TSourceNotebook.LoadFindInFilesHistory A TextToFindComboBox.Text=',TextToFindComboBox.Text);
        TextToFindComboBox.ItemIndex:=0;
        TextToFindComboBox.SelectAll;
        //debugln('TSourceNotebook.LoadFindInFilesHistory B TextToFindComboBox.Text=',TextToFindComboBox.Text);
      end;
    end;
    // show last used directories and directory of current file
    AssignToComboBox(DirectoryComboBox, FindInFilesPathHistory);
    if (SrcEdit<>nil) and (FilenameIsAbsolute(SrcEdit.FileName)) then
      AddFileToComboBox(DirectoryComboBox, ExtractFilePath(SrcEdit.FileName));
    // show last used file masks
    AssignToComboBox(FileMaskComboBox, FindInFilesMaskHistory);
    Options:=FindInFilesSearchOptions;
  end;
end;

procedure TSourceNotebook.SaveFindInFilesHistory(ADialog: TLazFindInFilesDialog);
begin
  if Assigned(ADialog) then
  begin
    with ADialog do
    begin
      InputHistories.AddToFindHistory(FindText);
      InputHistories.AddToFindInFilesPathHistory(DirectoryComboBox.Text);
      InputHistories.AddToFindInFilesMaskHistory(FileMaskComboBox.Text);
      InputHistories.FindInFilesSearchOptions:=Options;
    end;
    InputHistories.Save;
  end;
end;

{Search All the files in a project and add the results to the SearchResultsView
 Dialog}
procedure TSourceNotebook.FIFSearchProject(AProject: TProject;
                                           ADialog: TLazFindInFilesDialog);
var
  AnUnitInfo:  TUnitInfo;
  TheFileList: TStringList;
  SearchForm:  TSearchForm;
begin
  try
    TheFileList:= TStringList.Create;
    AnUnitInfo:=AProject.FirstPartOfProject;
    while AnUnitInfo<>nil do begin
      //Only if file exists on disk.
      if FilenameIsAbsolute(AnUnitInfo.FileName)
      and FileExistsUTF8(AnUnitInfo.FileName) then
        TheFileList.Add(AnUnitInfo.FileName);
      AnUnitInfo:=AnUnitInfo.NextPartOfProject;
    end;
    SearchForm:= FIFCreateSearchForm(ADialog);
    SearchForm.SearchFileList:= TheFileList;
    DoFindInFiles(SearchForm);
  finally
    FreeAndNil(TheFileList);
    FreeAndNil(SearchForm);
  end;
end;

procedure TSourceNotebook.FIFSearchDir(ADialog: TLazFindInFilesDialog);
var
  SearchForm: TSearchForm;
begin
  try
    SearchForm:= FIFCreateSearchForm(ADialog);
    SearchForm.SearchFileList:= Nil;
    DoFindInFiles(SearchForm);
  finally
    FreeAndNil(SearchForm);
  end;
end;

Procedure TSourceNotebook.DoFindInFiles(ASearchForm: TSearchForm);
var
  ListIndex: integer;
begin
  ShowSearchResultsView;
  ListIndex:=SearchResultsView.AddSearch(ASearchForm.SearchText,
                                         ASearchForm.SearchText,
                                         ASearchForm.ReplaceText,
                                         ASearchForm.SearchDirectory,
                                         ASearchForm.SearchMask,
                                         ASearchForm.SearchOptions);

  try
    SearchResultsView.BeginUpdate(ListIndex);
    ASearchForm.ResultsList:= SearchResultsView.Items[ListIndex];
    SearchResultsView.Items[ListIndex].Clear;
    ASearchForm.ResultsWindow:= ListIndex;
    try
      ASearchForm.Show;
      // update Window Menu, the OnIdle event does not occur when searching
      MainIDEInterface.UpdateWindowMenu;
      ASearchForm.DoSearch;
    except
      on E: ERegExpr do
        MessageDlg(lisUEErrorInRegularExpression, E.Message,mtError,
                   [mbCancel],0);
    end;
  finally
    SearchResultsView.EndUpdate(ListIndex);
    SearchResultsView.ShowOnTop;
  end;
end;

procedure TSourceNotebook.FIFSearchOpenFiles(ADialog: TLazFindInFilesDialog);
var
  i: integer;
  TheFileList: TStringList;
  SearchForm:  TSearchForm;
begin
  try
    TheFileList:= TStringList.Create;
    for i:= 0 to self.EditorCount -1 do
    begin
      //only if file exists on disk
      if FilenameIsAbsolute(Editors[i].FileName) and
         FileExistsUTF8(Editors[i].FileName) then
      begin
         TheFileList.Add(Editors[i].FileName);
      end;//if
    end;//for
    SearchForm:= FIFCreateSearchForm(ADialog);
    SearchForm.SearchFileList:= TheFileList;
    DoFindInFiles(SearchForm);
  finally
    FreeAndNil(TheFileList);
    FreeAndNil(SearchForm);
  end;//finally
end;//FIFSearchOpenFiles

{Creates the search form and loads the options selected in the
 findinfilesdialog}
function TSourceNotebook.FIFCreateSearchForm
                         (ADialog: TLazFindInFilesDialog): TSearchForm;
begin
  result:= TSearchForm.Create(SearchResultsView);
  with result do
  begin
    SearchOptions:= ADialog.Options;
    SearchText:= ADialog.FindText;
    ReplaceText:= ADialog.ReplaceText;
    SearchMask:= ADialog.FileMaskComboBox.Text;
    SearchDirectory:= ADialog.DirectoryComboBox.Text;
  end;//with
end;//FIFCreateSearchForm

Procedure TSourceNotebook.FindInFilesPerDialog(AProject: TProject);
var
  TempEditor: TSourceEditor;
  FindText: string;
Begin
  FindText:='';
  TempEditor := GetActiveSE;
  if TempEditor <> nil
  then with TempEditor, EditorComponent do
  begin
    if EditorOpts.FindTextAtCursor
    then begin
      if SelAvail and (BlockBegin.Y = BlockEnd.Y)
      then FindText := SelText
      else FindText := GetWordAtRowCol(LogicalCaretXY);
    end else begin
      if InputHistories.FindHistory.Count>0 then
        FindText:=InputHistories.FindHistory[0];
    end;
  end;

  FindInFiles(AProject, FindText);
End;

procedure TSourceNotebook.FindInFiles(AProject: TProject;
  const FindText: string);
begin
  if FindInFilesDialog = nil then
    FindInFilesDialog := CreateFindInFilesDialog
  else
    LoadFindInFilesHistory(FindInFilesDialog);

  // if there is no FindText, use the most recently used FindText
  FindInFilesDialog.FindText:= FindText;
  if (FindInFilesDialog.FindText='') and (InputHistories.FindHistory.Count > 0) then
      FindInFilesDialog.FindText:=InputHistories.FindHistory[0];

  // disable replace. Find in files is often called,
  // but almost never to replace with the same parameters
  FindInFilesDialog.Options:=
                           FindInFilesDialog.Options-[fifReplace,fifReplaceAll];
  if FindInFilesDialog.ShowModal=mrOk then
  begin
    SaveFindInFilesHistory(FindInFilesDialog);

    if FindInFilesDialog.FindText <>'' then
    begin
      case FindInFilesDialog.WhereRadioGroup.ItemIndex of
        0: FIFSearchProject(AProject, FindInFilesDialog);
        1: FIFSearchOpenFiles(FindInFilesDialog);
        2: FIFSearchDir(FindInFilesDialog);
      end;
    end;
  end;
end;

procedure TSourceNotebook.ShowSearchResultsView;
begin
  if Assigned(OnShowSearchResultsView) then OnShowSearchResultsView(Self);
end;

procedure TSourceNotebook.GotoLineClicked(Sender: TObject);
var
  SrcEdit: TSourceEditor;
begin
  SrcEdit := GetActiveSE;
  if SrcEdit <> nil then
    SrcEdit.ShowGotoLineDialog;
end;

procedure TSourceNotebook.HistoryJump(Sender: TObject;
  CloseAction: TJumpHistoryAction);
var NewCaretXY: TPoint;
  NewTopLine: integer;
  NewPageIndex: integer;
  SrcEdit: TSourceEditor;
begin
  if (NoteBook<>nil) and Assigned(OnJumpToHistoryPoint) then begin
    NewCaretXY.X:=-1;
    NewPageIndex:=-1;
    OnJumpToHistoryPoint(NewCaretXY,NewTopLine,NewPageIndex,CloseAction);
    SrcEdit:=FindSourceEditorWithPageIndex(NewPageIndex);
    if SrcEdit<>nil then begin
      NoteBook.PageIndex:=NewPageIndex;
      with SrcEdit.EditorComponent do begin
        TopLine:=NewTopLine;
        LogicalCaretXY:=NewCaretXY;
      end;
    end;
  end;
end;

procedure TSourceNotebook.JumpBackClicked(Sender: TObject);
begin
  HistoryJump(Sender,jhaBack);
end;

procedure TSourceNotebook.JumpForwardClicked(Sender: TObject);
begin
  HistoryJump(Sender,jhaForward);
end;

procedure TSourceNotebook.AddJumpPointClicked(Sender: TObject);
var SrcEdit: TSourceEditor;
begin
  if Assigned(OnAddJumpPoint) then begin
    SrcEdit:=GetActiveSE;
    if SrcEdit<>nil then begin
      OnAddJumpPoint(SrcEdit.EditorComponent.LogicalCaretXY,
        SrcEdit.EditorComponent.TopLine, Notebook.PageIndex, true);
    end;
  end;
end;

procedure TSourceNotebook.DeleteLastJumpPointClicked(Sender: TObject);
begin
  if Assigned(OnDeleteLastJumpPoint) then
    OnDeleteLastJumpPoint(Sender);
end;

procedure TSourceNotebook.ViewJumpHistoryClicked(Sender: TObject);
begin
  if Assigned(OnViewJumpHistory) then
    OnViewJumpHistory(Sender);
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
  if FHintWindow<>nil then
    FHintWindow.Visible:=false;
end;

procedure TSourceNotebook.StartShowCodeContext(JumpToError: boolean);
var
  Abort: boolean;
begin
  if OnShowCodeContext<>nil then begin
    OnShowCodeContext(JumpToError,Abort);
    if Abort then ;
  end;
end;

procedure TSourceNotebook.StartShowCodeHelp;
begin
  if SrcEditHintWindow=nil then begin
    SrcEditHintWindow:=TSrcEditHintWindow.Create(Self);
    SrcEditHintWindow.Name:='TSourceNotebook_SrcEditHintWindow';
    SrcEditHintWindow.Provider:=TFPDocHintProvider.Create(SrcEditHintWindow);
  end;
  SrcEditHintWindow.AnchorForm:=CurCompletionControl.TheForm;
  {$IFDEF EnableCodeHelp}
  SrcEditHintWindow.HelpEnabled:=true;
  {$ENDIF}
end;

procedure TSourceNotebook.BookMarkToggleClicked(Sender: TObject);
// popup menu: toggle bookmark clicked
var
  MenuItem: TIDEMenuItem;
Begin
  MenuItem := Sender as TIDEMenuItem;
  BookMarkSet(MenuItem.SectionIndex,true);
end;

procedure TSourceNotebook.BookmarkSetFreeClicked(Sender: TObject);
begin
  BookMarkSetFree;
end;

Procedure TSourceNotebook.BookMarkGotoClicked(Sender: TObject);
// popup menu goto bookmark clicked
var
  MenuItem: TIDEMenuItem;
Begin
  MenuItem := Sender as TIDEMenuItem;
  GotoBookMark(MenuItem.SectionIndex);
end;

Procedure TSourceNotebook.ReadOnlyClicked(Sender: TObject);
var ActEdit: TSourceEditor;
begin
  ActEdit:=GetActiveSE;
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
  UpdateStatusBar;
end;

procedure TSourceNotebook.OnPopupMenuOpenPasFile(Sender: TObject);
begin
  MainIDEInterface.DoOpenEditorFile(ChangeFileExt(GetActiveSE.Filename,'.pas'),
    Notebook.PageIndex+1,
    [ofOnlyIfExists,ofAddToRecent,ofRegularFile,ofUseCache,ofDoNotLoadResource]);
end;

procedure TSourceNotebook.OnPopupMenuOpenPPFile(Sender: TObject);
begin
  MainIDEInterface.DoOpenEditorFile(ChangeFileExt(GetActiveSE.Filename,'.pp'),
    Notebook.PageIndex+1,
    [ofOnlyIfExists,ofAddToRecent,ofRegularFile,ofUseCache,ofDoNotLoadResource]);
end;

procedure TSourceNotebook.OnPopupMenuOpenPFile(Sender: TObject);
begin
  MainIDEInterface.DoOpenEditorFile(ChangeFileExt(GetActiveSE.Filename,'.p'),
    Notebook.PageIndex+1,
    [ofOnlyIfExists,ofAddToRecent,ofRegularFile,ofUseCache,ofDoNotLoadResource]);
end;

procedure TSourceNotebook.OnPopupMenuOpenLFMFile(Sender: TObject);
begin
  MainIDEInterface.DoOpenEditorFile(ChangeFileExt(GetActiveSE.Filename,'.lfm'),
    Notebook.PageIndex+1,
    [ofOnlyIfExists,ofAddToRecent,ofRegularFile,ofUseCache,ofDoNotLoadResource]);
end;

procedure TSourceNotebook.OnPopupMenuOpenLRSFile(Sender: TObject);
begin
  MainIDEInterface.DoOpenEditorFile(ChangeFileExt(GetActiveSE.Filename,'.lrs'),
    Notebook.PageIndex+1,
    [ofOnlyIfExists,ofAddToRecent,ofRegularFile,ofUseCache,ofDoNotLoadResource]);
end;

procedure TSourceNotebook.OnPopupMenuOpenSFile(Sender: TObject);
begin
  MainIDEInterface.DoOpenEditorFile(ChangeFileExt(GetActiveSE.Filename,'.s'),
    Notebook.PageIndex+1,
    [ofOnlyIfExists,ofAddToRecent,ofRegularFile,ofUseCache,ofDoNotLoadResource]);
end;

procedure TSourceNotebook.OnPopupMenuOpenFile(Sender: TObject);
var
  AFilename: String;
begin
  AFilename:=GetActiveSE.Filename;
  if CompareFileExt(AFilename,'.lpi')=0 then
    MainIDEInterface.DoOpenProjectFile(AFilename,
      [ofOnlyIfExists,ofAddToRecent,ofUseCache])
  else if CompareFileExt(AFilename,'.lpk')=0 then
    PackageEditingInterface.DoOpenPackageFile(AFilename,[pofAddToRecent],false);
end;

Procedure TSourceNotebook.ShowUnitInfo(Sender: TObject);
begin
  if Assigned(FOnShowUnitInfo) then FOnShowUnitInfo(Sender);
end;

Procedure TSourceNotebook.ToggleLineNumbersClicked(Sender: TObject);
var
  MenuITem: TIDEMenuCommand;
  ActEdit:TSourceEditor;
  i: integer;
  ShowLineNumbers: boolean;
begin
  MenuItem := Sender as TIDEMenuCommand;
  ActEdit:=GetActiveSE;
  MenuItem.Checked :=
    not ActEdit.EditorComponent.Gutter.LineNumberPart.Visible;
  ShowLineNumbers:=MenuItem.Checked;
  for i:=0 to EditorCount-1 do
    Editors[i].EditorComponent.Gutter.LineNumberPart.Visible
      := ShowLineNumbers;
  EditorOpts.ShowLineNumbers := ShowLineNumbers;
  EditorOpts.Save;
end;

Procedure TSourceNotebook.OpenAtCursorClicked(Sender: TObject);
begin
  if Assigned(FOnOpenFileAtCursorClicked) then
    FOnOpenFileAtCursorClicked(Sender);
end;

Procedure TSourceNotebook.FindDeclarationClicked(Sender: TObject);
begin
  if Assigned(FOnFindDeclarationClicked) then
    FOnFindDeclarationClicked(Sender);
end;

procedure TSourceNotebook.ProcedureJumpClicked(Sender: TObject);
var ActSE: TSourceEditor;
begin
  ActSE := GetActiveSE;
  if ActSE <> nil then
    ActSE.DoEditorExecuteCommand(ecFindProcedureDefinition);
end;

procedure TSourceNotebook.FindNextWordOccurrenceClicked(Sender: TObject);
var
  SrcEdit: TSourceEditor;
begin
  SrcEdit := GetActiveSE;
  if SrcEdit<>nil then
    SrcEdit.FindNextWordOccurrence(true);
end;

procedure TSourceNotebook.FindPrevWordOccurrenceClicked(Sender: TObject);
var
  SrcEdit: TSourceEditor;
begin
  SrcEdit := GetActiveSE;
  if SrcEdit<>nil then
    SrcEdit.FindNextWordOccurrence(false);
end;

procedure TSourceNotebook.FindInFilesClicked(Sender: TObject);
var
  SrcEdit: TSourceEditor;
begin
  SrcEdit := GetActiveSE;
  if SrcEdit<>nil then
    SrcEdit.DoEditorExecuteCommand(ecFindInFiles);
end;

procedure TSourceNotebook.InsertTodoClicked(Sender: TObject);
begin
  if Assigned(FOnInsertTodoClicked) then
    FOnInsertTodoClicked(Sender);
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

procedure TSourceNotebook.CopyFilenameClicked(Sender: TObject);
var ActSE: TSourceEditor;
begin
  ActSE := GetActiveSE;
  if ActSE <> nil then
    Clipboard.AsText:=ActSE.FileName;
end;

procedure TSourceNotebook.MoveEditorLeftClicked(Sender: TObject);
begin
  MoveActivePageLeft;
end;

procedure TSourceNotebook.MoveEditorRightClicked(Sender: TObject);
begin
  MoveActivePageRight;
end;

procedure TSourceNotebook.MoveEditorFirstClicked(Sender: TObject);
begin
  MoveActivePageFirst;
end;

procedure TSourceNotebook.MoveEditorLastClicked(Sender: TObject);
begin
  MoveActivePageLast;
end;

procedure TSourceNotebook.DockingClicked(Sender: TObject);
begin
  ControlDocker.ShowDockingEditor;
end;

{This is called from outside to toggle a bookmark}
Procedure TSourceNotebook.ToggleBookmark(Value: Integer);
Begin
  BookMarkSet(Value,true);
End;

procedure TSourceNotebook.AddBreakpointClicked(Sender: TObject );
var
  ASrcEdit: TSourceEditor;
begin
  ASrcEdit:=GetActiveSE;
  if ASrcEdit=nil then exit;
  DebugBoss.DoCreateBreakPoint(ASrcEdit.Filename,
                               ASrcEdit.EditorComponent.CaretY,true);
end;

procedure TSourceNotebook.StatusBarDblClick(Sender: TObject);
var
  P: TPoint;
begin
  P := StatusBar.ScreenToClient(Mouse.CursorPos);
  // if we clicked on first panel which shows position in code
  if StatusBar.GetPanelIndexAt(P.X, P.Y) = 0 then
  begin
    // then show goto line dialog
    GotoLineClicked(nil);
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
  BreakPtMark := SourceEditorMarks.FindBreakPointMark(ASrcEdit.FEditor,Line);
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

procedure TSourceNotebook.EncloseSelectionMenuItemClick(Sender: TObject);
var
  ASrcEdit: TSourceEditor;
begin
  ASrcEdit:=GetActiveSE;
  if ASrcEdit=nil then exit;
  ASrcEdit.EncloseSelection;
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

procedure TSourceNotebook.FindIdentifierReferencesMenuItemClick(Sender: TObject
  );
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

procedure TSourceNotebook.RunToClicked(Sender: TObject);
var
  ASrcEdit: TSourceEditor;
begin
  ASrcEdit:=GetActiveSE;
  if ASrcEdit=nil then exit;
  DebugBoss.DoRunToCursor;
end;

procedure TSourceNotebook.ViewCallStackClick(Sender: TObject);
var
  Command: TSynEditorCommand;
  AChar: TUTF8Char;
  Handled: boolean;
begin
  Command:=ecToggleCallStack;
  AChar:=#0;
  Handled:=false;
  ProcessParentCommand(Self,Command,AChar,nil,Handled);
end;

Procedure TSourceNotebook.BookMarkSet(Value: Integer; Toggle: boolean);
var
  ActEdit, AnEdit: TSourceEditor;
  Cmd: TIDEMenuCommand;
  OldX, OldY: integer;
  NewXY: TPoint;
  SetMark: Boolean;
Begin
  ActEdit:=GetActiveSE;
  NewXY:=ActEdit.EditorComponent.CaretXY;

  SetMark:=true;
  AnEdit:=FindBookmark(Value);
  if (AnEdit<>nil) and AnEdit.EditorComponent.GetBookMark(Value,OldX,OldY) then
  begin
    if (not Toggle) and (OldX=NewXY.X) and (OldY=NewXY.Y) then
      exit;  // no change
    AnEdit.EditorComponent.ClearBookMark(Value);
    if Toggle and (OldY=NewXY.Y) then
      SetMark:=false;
  end;
  if SetMark then
    ActEdit.EditorComponent.SetBookMark(Value,NewXY.X,NewXY.Y);
  Cmd:=SrcEditSubMenuToggleBookmarks[Value] as TIDEMenuCommand;
  Cmd.Checked := SetMark;
  if Project1<>nil then
    Project1.SessionModified:=true;
end;

procedure TSourceNotebook.BookMarkSetFree;
var
  i: Integer;
begin
  for i:=0 to 9 do
    if (FindBookmark(i)=nil) then begin
      BookMarkSet(i);
      exit;
    end;
end;

{This is called from outside to set a bookmark}
procedure TSourceNotebook.SetBookmark(Value: Integer);
Begin
  BookMarkSet(Value);
End;

procedure TSourceNotebook.BookmarkGotoNext(GoForward: boolean);
var
  CurBookmarkID: Integer;
  x: Integer;
  y: Integer;
  SrcEdit: TSourceEditor;
  StartY: LongInt;
  CurEditorComponent: TSynEdit;
  StartEditorComponent: TSynEdit;
  BestBookmarkID: Integer;
  BestY: Integer;
  CurPageIndex: Integer;
  CurSrcEdit: TSourceEditor;
  StartPageIndex: LongInt;
  BetterFound: Boolean;
  PageDistance: Integer;
  BestPageDistance: Integer;
begin
  if Notebook=nil then exit;
  SrcEdit:=GetActiveSE;
  if SrcEdit=nil then exit;
  // init best bookmark
  BestBookmarkID:=-1;
  BestY:=-1;
  BestPageDistance:=-1;
  // where is the cursor
  StartPageIndex:=Notebook.PageIndex;
  StartEditorComponent:=SrcEdit.EditorComponent;
  StartY:=StartEditorComponent.CaretY;
  // go through all bookmarks
  for CurPageIndex:=0 to Notebook.PageCount-1 do begin
    CurSrcEdit:=FindSourceEditorWithPageIndex(CurPageIndex);
    CurEditorComponent:=CurSrcEdit.EditorComponent;
    for CurBookmarkID:=0 to 9 do begin
      if CurEditorComponent.GetBookmark(CurBookmarkID,x,y) then begin
        if (CurPageIndex=StartPageIndex) and (y=StartY) then
          continue;

        // for GoForward=true we are searching the nearest bookmark down the
        // current page, then the pages from left to right, starting at the
        // current page. That means the lines above the cursor are the most
        // far away.

        // calculate the distance of pages between the current bookmark
        // and the current page
        PageDistance:=(StartPageIndex-CurPageIndex);
        if GoForward then PageDistance:=-PageDistance;
        if PageDistance<0 then
          // for GoForward=true the pages on the left are farer than the pages
          // on the right (and vice versus)
          inc(PageDistance,Notebook.PageCount);
        if (PageDistance=0) then begin
          // for GoForward=true the lines in front are farer than the pages
          // on the left side
          if (GoForward and (y<StartY))
          or ((not GoForward) and (y>StartY)) then
            inc(PageDistance,Notebook.PageCount);
        end;

        BetterFound:=false;
        if BestBookmarkID<0 then
          BetterFound:=true
        else if PageDistance<BestPageDistance then begin
          BetterFound:=true;
        end else if PageDistance=BestPageDistance then begin
          if (GoForward and (y<BestY))
          or ((not GoForward) and (y>BestY)) then
            BetterFound:=true;
        end;
        //debugln('TSourceNotebook.BookmarkGotoNext GoForward=',dbgs(GoForward),
        //  ' CurBookmarkID=',dbgs(CurBookmarkID),
        //  ' PageDistance=',dbgs(PageDistance),' BestPageDistance='+dbgs(BestPageDistance),
        //  ' y='+dbgs(y),' BestY='+dbgs(BestY),' StartY=',dbgs(StartY),
        //  ' StartPageIndex='+dbgs(StartPageIndex),' CurPageIndex='+dbgs(CurPageIndex),
        //  ' BetterFound='+dbgs(BetterFound));

        if BetterFound then begin
          // nearer bookmark found
          BestBookmarkID:=CurBookmarkID;
          BestY:=y;
          BestPageDistance:=PageDistance;
        end;
      end;
    end;
  end;
  if BestBookmarkID>=0 then
    BookMarkGoto(BestBookmarkID);
end;

procedure TSourceNotebook.BookMarkGoto(Index: Integer);
var
  AnEditor:TSourceEditor;
begin
  if Notebook=nil then exit;
  AnEditor:=FindBookmark(Index);
  if AnEditor<>nil then begin
    AnEditor.EditorComponent.GotoBookMark(Index);
    AnEditor.CenterCursor;
    Notebook.PageIndex:=FindPageWithEditor(AnEditor);
  end;
end;

procedure TSourceNotebook.BookMarkNextClicked(Sender: TObject);
begin
  BookmarkGotoNext(true);
end;

procedure TSourceNotebook.BookMarkPrevClicked(Sender: TObject);
begin
  BookmarkGotoNext(false);
end;

{This is called from outside to Go to a bookmark}
Procedure TSourceNotebook.GoToBookmark(Value: Integer);
begin
  BookMarkGoTo(Value);
End;

function TSourceNotebook.NewFile(const NewShortName: String;
  ASource: TCodeBuffer; FocusIt: boolean): TSourceEditor;
Begin
  //create a new page
  {$IFDEF IDE_DEBUG}
  writeln('[TSourceNotebook.NewFile] A ');
  {$ENDIF}
  Visible:=true;
  Result := NewSE(-1);
  {$IFDEF IDE_DEBUG}
  writeln('[TSourceNotebook.NewFile] B ');
  {$ENDIF}
  Result.CodeBuffer:=ASource;
  {$IFDEF IDE_DEBUG}
  writeln('[TSourceNotebook.NewFile] D ');
  {$ENDIF}
  Result.PageName:=FindUniquePageName(NewShortName,Notebook.PageIndex);
  if FocusIt then FocusEditor;
  {$IFDEF IDE_DEBUG}
  writeln('[TSourceNotebook.NewFile] end');
  {$ENDIF}
  CheckFont;
end;

procedure TSourceNotebook.CloseFile(PageIndex:integer);
var
  TempEditor: TSourceEditor;
begin
  {$IFDEF IDE_DEBUG}
  writeln('TSourceNotebook.CloseFile A  PageIndex=',PageIndex);
  {$ENDIF}
  TempEditor:=FindSourceEditorWithPageIndex(PageIndex);
  if TempEditor=nil then exit;
  //debugln(['TSourceNotebook.CloseFile ',TempEditor.FileName,' ',TempEditor.PageIndex]);
  Visible:=true;
  EndIncrementalFind;
  TempEditor.Close;
  TempEditor.Free;
  if Notebook.PageCount>1 then
  begin
    //writeln('TSourceNotebook.CloseFile B  PageIndex=',PageIndex,' Notebook.PageIndex=',Notebook.PageIndex);
    // if this is the current page, switch to right PageIndex (if possible)
    if (Notebook.PageIndex = PageIndex) then
      Notebook.PageIndex := PageIndex +
        IfThen(PageIndex + 1 < Notebook.PageCount, 1, -1);
    // delete the page
    //writeln('TSourceNotebook.CloseFile C  PageIndex=',PageIndex,' Notebook.PageCount=',Notebook.PageCount,' NoteBook.PageIndex=',Notebook.PageIndex);
    Notebook.Pages.Delete(PageIndex);
    //writeln('TSourceNotebook.CloseFile D  PageIndex=',PageIndex,' Notebook.PageCount=',Notebook.PageCount,' NoteBook.PageIndex=',Notebook.PageIndex);
    UpdateStatusBar;
    // set focus to new editor
    TempEditor:=FindSourceEditorWithPageIndex(Notebook.PageIndex);
    if (TempEditor <> nil) then
      TempEditor.EditorComponent.SetFocus;
  end else
  begin
    //writeln('TSourceNotebook.CloseFile E  PageIndex=',PageIndex);
    Notebook.Free;
    //writeln('TSourceNotebook.CloseFile F  PageIndex=',PageIndex);
    Notebook:=nil;
    Hide;
  end;
  {$IFDEF IDE_DEBUG}
  writeln('TSourceNotebook.CloseFile END');
  {$ENDIF}
end;

procedure TSourceNotebook.FocusEditor;
var
  SrcEdit: TSourceEditor;
begin
  if (NoteBook=nil) or (fAutoFocusLock>0) then exit;
  SrcEdit:=GetActiveSE;
  if SrcEdit=nil then exit;
  Show;
  SrcEdit.FocusEditor;
end;

Function TSourceNotebook.ActiveFileName: AnsiString;
Begin
  Result := GetActiveSE.FileName;
end;

function TSourceNotebook.GetEditors(Index:integer):TSourceEditor;
begin
  Result:=TSourceEditor(FSourceEditorList[Index]);
end;

function TSourceNotebook.EditorCount:integer;
begin
  Result:=FSourceEditorList.Count;
end;

function TSourceNotebook.Count: integer;
begin
  Result:=FSourceEditorList.Count;
end;

Procedure TSourceNotebook.CloseClicked(Sender: TObject);
Begin
  if Assigned(FOnCloseClicked) then FOnCloseClicked(Sender, False);
end;

procedure TSourceNotebook.CloseOtherPagesClicked(Sender: TObject);
begin
  if Assigned(FOnCloseClicked) then FOnCloseClicked(Sender, True);
end;

Function TSourceNotebook.FindUniquePageName(FileName:string;
  IgnorePageIndex:integer):string;
var I:integer;
  ShortName:string;

  function PageNameExists(const AName:string):boolean;
  var a:integer;
  begin
    Result:=false;
    if Notebook=nil then exit;
    for a:=0 to Notebook.PageCount-1 do begin
      if (a<>IgnorePageIndex)
      and (AnsiCompareText(AName,FindSourceEditorWithPageIndex(a).PageName)=0)
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

procedure TSourceNotebook.ToggleFormUnitClicked(Sender: TObject);
begin
  if Assigned(FOnToggleFormUnitClicked) then FOnToggleFormUnitClicked(Sender);
end;

procedure TSourceNotebook.ToggleObjectInspClicked(Sender: TObject);
begin
  if Assigned(FOnToggleObjectInspClicked) then FOnToggleObjectInspClicked(Sender);
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

procedure TSourceNotebook.InitFindDialog;
var c: TFindDlgComponent;
begin
  LazFindReplaceDialog.OnKey:=@FindReplaceDlgKey;
  for c:=Low(TFindDlgComponent) to High(TFindDlgComponent) do
    FindReplaceDlgHistoryIndex[c]:=-1;
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
  if not Visible then exit;
  TempEditor := GetActiveSE;
  if TempEditor = nil then Exit;
  CurEditor:=TempEditor.EditorComponent;

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

    If TempEditor.ReadOnly then
      if PanelFileMode <> '' then
        PanelFileMode := Format(lisUEReadOnly, [PanelFileMode])
      else
        PanelFileMode := uepReadonly;

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
  UpdateFPDocEditor;
End;

function TSourceNotebook.FindBookmark(BookmarkID: integer): TSourceEditor;
var i,x,y:integer;
begin
  for i:=0 to EditorCount-1 do begin
    if Editors[i].EditorComponent.GetBookmark(BookMarkID,x,y) then begin
      Result:=Editors[i];
      exit;
    end;
  end;
  Result:=nil;
end;

function TSourceNotebook.FindPageWithEditor(
  ASourceEditor: TSourceEditor):integer;
var i:integer;
begin
  if Notebook=nil then begin
    Result:=-1;
  end else begin
    Result:=Notebook.PageCount-1;
    while (Result>=0) do begin
      with Notebook.Page[Result] do
        for I := 0 to ControlCount-1 do
          if Controls[I]=ASourceEditor.EditorComponent then exit;
      dec(Result);
    end;
  end;
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

function TSourceNotebook.FindSourceEditorWithFilename(const Filename: string
  ): TSourceEditor;
var i: integer;
begin
  for i:=0 to EditorCount-1 do begin
    Result:=Editors[i];
    if CompareFilenames(Result.Filename,Filename)=0 then exit;
  end;
  Result:=nil;
end;

procedure TSourceNotebook.NotebookMouseDown(Sender: TObject;
  Button: TMouseButton;  Shift: TShiftState; X,Y: Integer);
var
  TabIndex: Integer;
begin
  if Button=mbMiddle then begin
    TabIndex := Notebook.TabIndexAtClientPos(point(X,Y));
    if TabIndex>=0 then
      CloseClicked(Notebook.Page[TabIndex]);
  end;
end;

Procedure TSourceNotebook.NotebookPageChanged(Sender: TObject);
var TempEditor:TSourceEditor;
Begin
  TempEditor:=GetActiveSE;

  //writeln('TSourceNotebook.NotebookPageChanged ',Notebook.Pageindex,' ',TempEditor <> nil,' fAutoFocusLock=',fAutoFocusLock);
  if TempEditor <> nil then
  begin
    if fAutoFocusLock=0 then begin
      {$IFDEF VerboseFocus}
      writeln('TSourceNotebook.NotebookPageChanged BEFORE SetFocus ',
        TempEditor.EditorComponent.Name,' ',
        NoteBook.Pages[FindPageWithEditor(TempEditor)]);
      {$ENDIF}
      TempEditor.FocusEditor;
      {$IFDEF VerboseFocus}
      writeln('TSourceNotebook.NotebookPageChanged AFTER SetFocus ',
        TempEditor.EditorComponent.Name,' ',
        NoteBook.Pages[FindPageWithEditor(TempEditor)]);
      {$ENDIF}
    end;
    UpdateStatusBar;
    UpdateActiveEditColors(TempEditor.EditorComponent);
    if (DebugBoss.State in [dsPause, dsRun]) and
       not TempEditor.HasExecutionMarks and
       (TempEditor.FileName <> '') then
      TempEditor.FillExecutionMarks;
    if Assigned(FOnEditorVisibleChanged) then
      FOnEditorVisibleChanged(sender);
  end;

  CheckCurrentCodeBufferChanged;
  UpdateFPDocEditor;
end;

Procedure TSourceNotebook.ProcessParentCommand(Sender: TObject;
  var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  var Handled: boolean);
begin
  //DebugLn(['TSourceNotebook.ProcessParentCommand START ',dbgsName(Sender),' Command=',Command,' AChar=',AChar]);

  FProcessingCommand:=true;
  if Assigned(FOnProcessUserCommand) then begin
    Handled:=false;
    FOnProcessUserCommand(Self,Command,Handled);
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

  ecOpenFileAtCursor:
    OpenAtCursorClicked(self);

  ecGotoEditor1..ecGotoEditor9,ecGotoEditor0:
    if Notebook.PageCount>Command-ecGotoEditor1 then
      Notebook.PageIndex:=Command-ecGotoEditor1;

  ecToggleFormUnit:
    ToggleFormUnitClicked(Self);

  ecToggleObjectInsp:
    ToggleObjectInspClicked(Self);

  ecSetFreeBookmark:
    BookMarkSetFree;

  ecJumpBack:
    HistoryJump(Self,jhaBack);

  ecJumpForward:
    HistoryJump(Self,jhaForward);

  ecAddJumpPoint:
    AddJumpPointClicked(Self);

  ecViewJumpHistory:
    ViewJumpHistoryClicked(Self);

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
  if Assigned(FOnUserCommandProcessed) then begin
    Handled:=false;
    FOnUserCommandProcessed(Self,Command,Handled);
    if Handled then exit;
  end;

  Handled:=(Command=ecClose);

  if Handled then Command:=ecNone;
end;

Procedure TSourceNotebook.ReloadEditorOptions;
var
  I: integer;
  h: TLazSyntaxHighlighter;
Begin
  // this reloads the colors for the highlighter and other editor settings.
  for h:=Low(TLazSyntaxHighlighter) to High(TLazSyntaxHighlighter) do
    if Highlighters[h]<>nil then begin
      Highlighters[h].BeginUpdate;
      EditorOpts.GetHighlighterSettings(Highlighters[h]);
      Highlighters[h].EndUpdate;
    end;
  ReloadHighlighters;
  for i := 0 to EditorCount-1 do
    Editors[i].RefreshEditorSettings;

  // reload code templates
  with CodeTemplateModul do begin
    if FileExistsUTF8(EditorOpts.CodeTemplateFilename) then
      AutoCompleteList.LoadFromFile(UTF8ToSys(EditorOpts.CodeTemplateFilename))
    else
      if FileExistsUTF8('lazarus.dci') then
        AutoCompleteList.LoadFromFile(UTF8ToSys('lazarus.dci'));
    IndentToTokenStart:=EditorOpts.CodeTemplateIndentToTokenStart;
  end;

  EditorOpts.KeyMap.AssignTo(FKeyStrokes,TSourceEditorWindowInterface);
  if NoteBook<>nil then begin
    if EditorOpts.ShowTabCloseButtons then
      NoteBook.Options:=NoteBook.Options+[nboShowCloseButtons]
    else
      NoteBook.Options:=NoteBook.Options-[nboShowCloseButtons];
  end;

  SourceCompletionTimer.Interval:=EditorOpts.AutoDelayInMSec;
  FMouseHintTimer.Interval:=EditorOpts.AutoDelayInMSec;

  Exclude(States,snWarnedFont);
  CheckFont;
end;

procedure TSourceNotebook.ReloadHighlighters;
var
  i: Integer;
  ASrcEdit: TSourceEditor;
  AnUnitInfo: TUnitInfo;
begin
  for i := 0 to EditorCount-1 do begin
    ASrcEdit:=Editors[i];
    AnUnitInfo:=Project1.UnitWithEditorIndex(i);
    if AnUnitInfo<>nil then
      ASrcEdit.SyntaxHighlighterType:=AnUnitInfo.SyntaxHighlighter;
  end;
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
  if (not CurFont.CanUTF8) and SystemCharSetIsUTF8
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
  i := FKeyStrokes.FindKeycode(Key, Shift);
  if i>=0 then begin
    Command:=FKeyStrokes[i].Command;
    case Command of

    ecGotoMarker0..ecGotoMarker9:
      begin
        BookMarkGoto(Command - ecGotoMarker0);
        Key:=0;
      end;

    ecSetMarker0..ecSetMarker9:
      begin
        BookMarkSet(Command - ecSetMarker0);
        Key:=0;
      end;

    ecToggleMarker0..ecToggleMarker9:
      begin
        BookMarkSet(Command - ecToggleMarker0,true);
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
    FMouseHintTimer.AutoEnabled := EditorOpts.AutoToolTipExprEval
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

procedure TSourceNotebook.EditorMouseLink(
  Sender: TObject; X, Y: Integer; var AllowMouseLink: Boolean);
begin
  if Assigned(OnMouseLink) then
    OnMouseLink(Sender, X, Y, AllowMouseLink);
end;

function TSourceNotebook.EditorGetIndent(Sender: TSynCustomBeautifier; Editor: TObject;
  LogCaret: TPoint; var FirstLinePos, LinesCount: Integer; Reason: TSynEditorCommand;
  SetIndentProc: TSynBeautifierSetIndentProc): Boolean;
begin
  if Assigned(OnGetIndent) then
    Result := OnGetIndent(Sender, GetActiveSE, LogCaret, FirstLinePos, LinesCount,
                          Reason, SetIndentProc);
end;

Procedure TSourceNotebook.HintTimer(sender: TObject);
var
  MousePos: TPoint;
  AControl: TControl;
begin
  //DebugLn(['TSourceNotebook.HintTimer ']);
  FMouseHintTimer.Enabled := False;
  FMouseHintTimer.AutoEnabled := False;
  MousePos := Mouse.CursorPos;
  AControl:=FindLCLControl(MousePos);
  if (AControl=nil) or (GetParentForm(AControl)<>Self) then exit;
  if AControl is TSynEdit then
    ShowSynEditHint(MousePos);
end;

{------------------------------------------------------------------------------
  procedure TSourceNotebook.OnApplicationUserInput(Sender: TObject;
    Msg: Cardinal);
------------------------------------------------------------------------------}
procedure TSourceNotebook.OnApplicationUserInput(Sender: TObject; Msg: Cardinal
  );
begin
  //debugln('TSourceNotebook.OnApplicationUserInput');
  HideHint;
end;

procedure TSourceNotebook.EditorClickLink(Sender: TObject; Button: TMouseButton;
  Shift: TShiftstate; X, Y: Integer);
begin
  // click link = Find Declaration
  if Assigned(FOnClickLink) then
    FOnClickLink(Sender,Button,Shift,X,Y);
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
  LineMarks: TSynEditMarks;
  AMark: TSourceMark;
  i: integer;
  HintStr: String;
  CurHint: String;
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
      ASynEdit.Marks.GetMarksForLine(EditCaret.Y,LineMarks);
      HintStr:='';
      for i:=Low(TSynEditMarks) to High(TSynEditMarks) do begin
        AMark:=TSourceMark(LineMarks[i]);
        if not (AMark is TSourceMark) then continue;
        CurHint:=AMark.GetHint;
        if CurHint='' then continue;
        if HintStr<>'' then HintStr:=HintStr+LineEnding;
        HintStr:=HintStr+CurHint;
      end;
      if HintStr<>'' then
        ActivateHint(MousePos,'',HintStr);
    end;
  end else begin
    // hint for source
    if Assigned(OnShowHintForSource) then
      OnShowHintForSource(ASrcEdit,EditPos,EditCaret);
  end;
end;

Procedure TSourceNotebook.AddWatchAtCursor(Sender: TObject);
begin
  if Assigned(OnAddWatchAtCursor) then
    OnAddWatchAtCursor(Self);
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

function TSourceNotebook.MacroFuncCol(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
var
  SrcEdit: TSourceEditor;
begin
  SrcEdit:=GetActiveSE;
  if (SrcEdit<>nil) then
    Result:=IntToStr(SrcEdit.EditorComponent.CaretX)
  else
    Result:='';
end;

function TSourceNotebook.MacroFuncRow(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
var
  SrcEdit: TSourceEditor;
begin
  SrcEdit:=GetActiveSE;
  if (SrcEdit<>nil) then
    Result:=IntToStr(SrcEdit.EditorComponent.CaretY)
  else
    Result:='';
end;

function TSourceNotebook.MacroFuncEdFile(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
var
  SrcEdit: TSourceEditor;
begin
  SrcEdit:=GetActiveSE;
  if (SrcEdit<>nil) then
    Result:=SrcEdit.FileName
  else
    Result:='';
end;

function TSourceNotebook.MacroFuncCurToken(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
var
  SrcEdit: TSourceEditor;
begin
  SrcEdit:=GetActiveSE;
  if (SrcEdit<>nil) then begin
    with SrcEdit.EditorComponent do
      Result:=GetWordAtRowCol(LogicalCaretXY)
  end else
    Result:='';
end;

function TSourceNotebook.MacroFuncPrompt(const s: string; const Data: PtrInt;
  var Abort: boolean): string;
begin
  Result:=s;
  Abort:=(ShowMacroPromptDialog(Result)<>mrOk);
end;

procedure TSourceNotebook.UpdateActiveEditColors(AEditor: TSynEdit);
var
  s, sm : TFontStyles;
begin
  if AEditor=nil then exit;

  EditorOpts.SetMarkupColors(AEditor.Highlighter, AEditor);

  FActiveEditDefaultFGColor:=AEditor.Font.Color;
  FActiveEditDefaultBGColor:=AEditor.Color;
  EditorOpts.GetLineColors(AEditor.Highlighter, ahaTextBlock, {TODO: MFR use AEditor.SelectedColor which includes styles / or have a copy}
    FActiveEditSelectedFGColor, FActiveEditSelectedBGColor, s ,sm);

  FActiveEditKeyFGColor:=FActiveEditDefaultFGColor;
  FActiveEditKeyBGColor:=FActiveEditDefaultBGColor;
  FActiveEditSymbolFGColor:=FActiveEditDefaultFGColor;
  FActiveEditSymbolBGColor:=FActiveEditDefaultBGColor;

  if AEditor.Highlighter<>nil
  then begin
    with AEditor.Highlighter do begin
      if IdentifierAttribute<>nil
      then begin
        if IdentifierAttribute.ForeGround<>clNone then
          FActiveEditDefaultFGColor:=IdentifierAttribute.ForeGround;
        if IdentifierAttribute.BackGround<>clNone then
          FActiveEditDefaultBGColor:=IdentifierAttribute.BackGround;
      end;
      if KeywordAttribute<>nil
      then begin
        if KeywordAttribute.ForeGround<>clNone then
          FActiveEditKeyFGColor:=KeywordAttribute.ForeGround;
        if KeywordAttribute.BackGround<>clNone then
          FActiveEditKeyBGColor:=KeywordAttribute.BackGround;
      end;
      if SymbolAttribute<>nil
      then begin
        if SymbolAttribute.ForeGround<>clNone then
          FActiveEditSymbolFGColor:=SymbolAttribute.ForeGround;
        if SymbolAttribute.BackGround<>clNone then
          FActiveEditSymbolBGColor:=SymbolAttribute.BackGround;
      end;
    end;
  end;
  AEditor.UseIncrementalColor:= snIncrementalFind in States;
end;

Procedure TSourceNotebook.GetSynEditPreviewSettings(APreviewEditor: TObject);
var ASynEdit: TSynEdit;
begin
  if not (APreviewEditor is TSynEdit) then exit;
  ASynEdit:=TSynEdit(APreviewEditor);
  EditorOpts.GetSynEditPreviewSettings(ASynEdit);
  ASynEdit.Highlighter:=Highlighters[lshFreePascal];
end;

function TSourceNotebook.GetEditorControlSettings(EditControl: TControl
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

function TSourceNotebook.GetHighlighterSettings(Highlighter: TObject): boolean;
begin
  Result:=true;
  if Highlighter is TSynCustomHighlighter then begin
    EditorOpts.GetHighlighterSettings(TSynCustomHighlighter(Highlighter));
    Result:=true;
  end else begin
    Result:=false;
  end;
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

procedure TSourceNotebook.CloseTabClicked(Sender: TObject);
begin
  if Assigned(FOnCloseClicked) then
    FOnCloseClicked(Sender, GetKeyState(VK_CONTROL) < 0);
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
end;

procedure InternalFinal;
var h: TLazSyntaxHighlighter;
begin
  for h:=Low(TLazSyntaxHighlighter) to High(TLazSyntaxHighlighter) do
    FreeThenNil(Highlighters[h]);
  FreeThenNil(aWordCompletion);
end;


initialization
  InternalInit;
  {$I sourceeditor.lrs}
{$I ../images/bookmark.lrs}

finalization
  InternalFinal;

end.

