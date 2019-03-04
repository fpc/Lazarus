{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Abstract:
    Defines interface to source editors.
}
unit SrcEditorIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLType, Forms, Controls, Graphics,
  // LazUtils
  FileUtil, Laz2_XMLCfg, LazStringUtils,
  // IdeIntf
  ProjectIntf, IDECommands;
  
type
  TSourceMarklingType = (
    smtHint,    // something is unusual or awkward
    smtNote,    // something could be wrong or is not optimized
    smtWarning, // something is probably wrong or likely to fail
    smtError    // something is definitely wrong
    );

  { TSourceMarklingProducer }

  TSourceMarklingProducer = class(TComponent)
  public
    procedure InvalidateAllFiles;
    procedure InvalidateFile(aFilename: string);
    function GetMarklings(aFilename: string;
                          out FreeList, FreeMarklings: boolean): TFPList; virtual; abstract;
  end;

  { TSourceMarkling }

  TSourceMarkling = class
  private
    FColumn: integer;
    FProducer: TSourceMarklingProducer;
    FId: integer;
    FLine: integer;
    fType: TSourceMarklingType;
  public
    constructor Create(aProducer: TSourceMarklingProducer;
                    TheID, aLine, aColumn: integer; aType: TSourceMarklingType);
    property Producer: TSourceMarklingProducer read FProducer;
    property Id: integer read FId;
    property Line: integer read FLine;
    property Column: integer read FColumn;
    property TheType: TSourceMarklingType read fType;
  end;

type
  TSrcEditSearchOption = (
    sesoMatchCase,
    sesoWholeWord,
    sesoBackwards,
    sesoEntireScope,
    sesoSelectedOnly,
    sesoReplace,
    sesoReplaceAll,
    sesoPrompt,
    sesoRegExpr,
    sesoMultiLine
    );
  TSrcEditSearchOptions = set of TSrcEditSearchOption;

  TSrcEditReplaceAction = (seraCancel, seraSkip, seraReplace, seraReplaceAll);

  TSrcEditProjectUpdateNeeded = (sepuNewShared, sepuChangedHighlighter);
  TSrcEditProjectUpdatesNeeded = set of TSrcEditProjectUpdateNeeded;

  { TSourceEditorInterface }

  TSourceEditorInterface = class
  protected
    function GetBlockBegin: TPoint; virtual; abstract;
    function GetBlockEnd: TPoint; virtual; abstract;
    function GetCodeToolsBuffer: TObject; virtual; abstract;
    function GetCursorScreenXY: TPoint; virtual; abstract;
    function GetCursorTextXY: TPoint; virtual; abstract;
    function GetEditorControl: TWinControl; virtual; abstract;
    function GetFileName: string; virtual; abstract;
    function GetLines: TStrings; virtual; abstract;
    function GetLineText: string; virtual; abstract;
    function GetLinesInWindow: Integer; virtual; abstract;
    function GetModified: Boolean; virtual; abstract;
    function GetPageCaption: string; virtual; abstract;
    function GetPageName: string; virtual; abstract;
    function GetReadOnly: Boolean; virtual; abstract;
    function GetSelection: string; virtual; abstract;
    function GetSelEnd: Integer; virtual; abstract;
    function GetSelStart: Integer; virtual; abstract;
    function GetSourceText: string; virtual; abstract;
    function GetTopLine: Integer; virtual; abstract;
    procedure SetBlockBegin(const AValue: TPoint); virtual; abstract;
    procedure SetBlockEnd(const AValue: TPoint); virtual; abstract;
    procedure SetCursorScreenXY(const AValue: TPoint); virtual; abstract;
    procedure SetCursorTextXY(const AValue: TPoint); virtual; abstract;
    procedure SetLines(const AValue: TStrings); virtual; abstract;
    procedure SetLineText(const AValue: string); virtual; abstract;
    procedure SetModified(const NewValue: Boolean); virtual; abstract;
    procedure SetReadOnly(const AValue: Boolean); virtual; abstract;
    procedure SetSelection(const AValue: string); virtual; abstract;
    procedure SetSelEnd(const AValue: Integer); virtual; abstract;
    procedure SetSelStart(const AValue: Integer); virtual; abstract;
    procedure SetSourceText(const AValue: string); virtual; abstract;
    procedure SetTopLine(const AValue: Integer); virtual; abstract;
  public
    // selections
    function SelectionAvailable: boolean; virtual; abstract;
    function GetText(OnlySelection: boolean): string; virtual; abstract;
    procedure SelectText(LineNum, CharStart, LineNum2, CharEnd: Integer);
    procedure SelectText(const StartPos, EndPos: TPoint); virtual; abstract;
    procedure InsertLine(StartLine: Integer; const NewText: String; aKeepMarks: Boolean = False); virtual; abstract;
    procedure ReplaceLines(StartLine, EndLine: integer; const NewText: string; aKeepMarks: Boolean = False); virtual; abstract;
    procedure ReplaceText(const StartPos, EndPos: TPoint; const NewText: string);
    procedure AskReplace(Sender: TObject; const ASearch, AReplace: string;
                        Line, Column: integer;
                        out Action: TSrcEditReplaceAction); virtual; abstract;
    procedure CopyToClipboard; virtual; abstract;
    procedure CutToClipboard; virtual; abstract;
    function GetBookMark(BookMark: Integer; out X, Y: Integer): Boolean; virtual; abstract;
    procedure SetBookMark(BookMark: Integer; X, Y: Integer); virtual; abstract;

    // screen and text position mapping
    function LineCount: Integer; virtual; abstract;
    function TextToScreenPosition(const Position: TPoint): TPoint; virtual; abstract;
    function ScreenToTextPosition(const Position: TPoint): TPoint; virtual; abstract;

    // characters and pixels
    function WidthInChars: Integer; virtual; abstract;
    function HeightInLines: Integer; virtual; abstract;
    function CharWidth: integer; virtual; abstract;
    function CursorInPixel: TPoint; virtual; abstract;
    function ScreenToPixelPosition(const Position: TPoint): TPoint; virtual; abstract;// ScreenXY to pixel in EditorControl.
      // To get the desktop pixel coords use:
      //   with SourceEditorWindow.ActiveEditor do
      //     DesktopXY:=EditorControl.ClientToScreen(ScreenToPixelPosition(ScreenXY));

    // update
    procedure BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}(ACaller: String = ''){$ENDIF}; virtual; abstract;
    procedure EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}(ACaller: String = ''){$ENDIF}; virtual; abstract;
    procedure BeginUpdate; virtual; abstract; // block painting
    procedure EndUpdate; virtual; abstract;
    procedure IncreaseIgnoreCodeBufferLock; virtual; abstract;
    procedure DecreaseIgnoreCodeBufferLock; virtual; abstract;
    procedure UpdateCodeBuffer; virtual; abstract;// copy the source from EditorComponent to the codetools
    function NeedsUpdateCodeBuffer: boolean; virtual; abstract;// needs UpdateCodeBuffer

    // search and replace
    function SearchReplace(const ASearch, AReplace: string;
                           SearchOptions: TSrcEditSearchOptions): integer; virtual; abstract;

    // context
    function GetProjectFile: TLazProjectFile; virtual; abstract;
    procedure UpdateProjectFile(AnUpdates: TSrcEditProjectUpdatesNeeded = []); virtual; abstract;
    function GetDesigner(LoadForm: boolean): TIDesigner; virtual; abstract;

    // editor commands
    procedure DoEditorExecuteCommand(EditorCommand: word); virtual; abstract;

    // move/copy to another SourceEditorWindow
    procedure MoveToWindow(AWindowIndex: Integer); virtual; abstract;
    procedure CopyToWindow(AWindowIndex: Integer); virtual; abstract;
  public
    property BlockBegin: TPoint read GetBlockBegin write SetBlockBegin;
    property BlockEnd: TPoint read GetBlockEnd write SetBlockEnd;
    property CodeToolsBuffer: TObject read GetCodeToolsBuffer;
    property CursorScreenXY: TPoint read GetCursorScreenXY write SetCursorScreenXY;
    property CursorTextXY: TPoint read GetCursorTextXY write SetCursorTextXY;
    property EditorControl: TWinControl read GetEditorControl;// normally TSynEdit
    property FileName: string read GetFileName;
    property Lines: TStrings read GetLines write SetLines;// the whole file
    property CurrentLineText: string read GetLineText write SetLineText;// source of current line
    property PageCaption: string read GetPageCaption;// the beatified unit name with modifiers ('*', '#') as it is shown in tab caption
    property PageName: string read GetPageName;// the beatified unit name
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property Selection: string read GetSelection write SetSelection;
    property SelEnd: Integer read GetSelEnd write SetSelEnd;
    property SelStart: Integer read GetSelStart write SetSelStart;
    property SourceText: string read GetSourceText write SetSourceText;// the whole file
    property TopLine: Integer read GetTopLine write SetTopLine;// first visible line
    property LinesInWindow: Integer read GetLinesInWindow;
    property Modified: Boolean read GetModified write SetModified;
  end;

  { TSourceEditorCompletionPlugin }

  TSourceEditorCompletionPlugin = class(TComponent)
  public
    procedure Init(SrcEdit: TSourceEditorInterface; JumpToError: boolean;
                   var Handled, Abort: boolean; var Prefix: string;
                   var BoxX, BoxY: integer); virtual; abstract; // check if this plugin is responsible
    function Collect(List: TStrings): boolean; virtual; abstract; // collect values
    procedure Cancel; virtual; abstract; // completion was cancelled
    procedure Complete(var Value: string; SourceValue: string;
                       var SourceStart, SourceEnd: TPoint;
                       KeyChar: TUTF8Char; Shift: TShiftState); virtual; abstract; // execute completion

    procedure IndexChanged(Position: integer); virtual; abstract;
    procedure PrefixChanged(const NewPrefix: string; var NewIndex: integer;
                            var s: TStrings); virtual; abstract;
    procedure CompletePrefix(var Prefix: string); virtual; abstract;

    function HasCustomPaint: boolean; virtual;
    procedure PaintItem(const {%H-}AKey: string; {%H-}ACanvas: TCanvas;
                 {%H-}X, {%H-}Y: integer; {%H-}ItemSelected: boolean; {%H-}Index: integer); virtual;
    function MeasureItem(const {%H-}AKey: string; {%H-}ACanvas: TCanvas;
                         {%H-}ItemSelected: boolean; {%H-}Index: integer): TPoint; virtual;
  end;

  { TSourceEditorWindowInterface }

  TSourceEditorWindowInterface = class(TForm)
  protected
    function GetActiveCompletionPlugin: TSourceEditorCompletionPlugin; virtual; abstract;
    function GetActiveEditor: TSourceEditorInterface; virtual; abstract;
    function GetCompletionPlugins(Index: integer): TSourceEditorCompletionPlugin; virtual; abstract;
    function GetItems(Index: integer): TSourceEditorInterface; virtual; abstract;
    procedure SetActiveEditor(const AValue: TSourceEditorInterface); virtual; abstract;
    function GetWindowID: Integer; virtual; abstract;
  public
    procedure IncUpdateLock; virtual; abstract;
    procedure DecUpdateLock; virtual; abstract;
    function IndexOfEditorInShareWith(AnOtherEditor: TSourceEditorInterface): Integer; virtual; abstract;
    function SourceEditorIntfWithFilename(const Filename: string): TSourceEditorInterface; virtual; abstract;
    property ActiveEditor: TSourceEditorInterface read GetActiveEditor write SetActiveEditor;
    function Count: integer; virtual; abstract;
    property Items[Index: integer]: TSourceEditorInterface read GetItems; default;
    property ActiveCompletionPlugin: TSourceEditorCompletionPlugin read GetActiveCompletionPlugin;
    // The number in the Form.Caption minus 1 (0-based), if multiple Win are open
    property WindowID: Integer read GetWindowID;
    // Editor Page Caption update
    procedure AddUpdateEditorPageCaptionHandler(AEvent: TNotifyEvent; const AsLast: Boolean = True); virtual; abstract;
    procedure RemoveUpdateEditorPageCaptionHandler(AEvent: TNotifyEvent); virtual; abstract;
  end;

  TsemChangeReason = (
    semWindowCreate,    // Called after creation of a Window
    semWindowDestroy,   // Called after removal of a Window
    semWindowActivate,  // Window is now ActiveSourceWindow (does not vave to be focused)
    semWindowFocused,   // The window became the active win of the application
    semWindowShow,      // Called after a Window is shown
    semWindowHide,      // Called after a Window is hidden
    semEditorCreate,    // Called after a new editor was created and added to list
    semEditorDestroy,   // Called when an Editor is destroyed / after it is removed fron the list of editors
    semEditorActivate,  // Editor is ActiveEditor
    semEditorStatus     // any status change of the editor (Caret, Selection, topline, ...)
  );

  TSemSelectionMode = (
    semsmNormal,
    semsmLine,
    semsmColumn,
    semsmCurrent);
  TSemCopyPasteAction = (
    semcaContinue,  // normal paste with specials like folding
    semcaPlainText, // paste as normal text, ignore folding and other specials
    semcaAbort      // cancel, use this if you handled the paste yourself
    );
  // ToDo: use the right clipboard
  TSemCopyPasteEvent = procedure(Sender: TSourceEditorInterface;
    var AText: String; var AMode: TSemSelectionMode; ALogStartPos: TPoint;
    var AnAction: TSemCopyPasteAction) of object;

  { TSourceEditorManagerInterface }

  TSourceEditorManagerInterface = class(TComponent)
  protected
    function GetActiveSourceWindow: TSourceEditorWindowInterface; virtual; abstract;
    procedure SetActiveSourceWindow(const AValue: TSourceEditorWindowInterface);
                                    virtual; abstract;
    function GetSourceWindows(Index: integer): TSourceEditorWindowInterface;
                              virtual; abstract;
    function GetActiveEditor: TSourceEditorInterface; virtual; abstract;
    procedure SetActiveEditor(const AValue: TSourceEditorInterface);
                              virtual; abstract;
    function GetSourceEditors(Index: integer): TSourceEditorInterface;
                              virtual; abstract;
    function GetUniqueSourceEditors(Index: integer): TSourceEditorInterface;
                                    virtual; abstract;
    function GetMarklingProducers(Index: integer): TSourceMarklingProducer;
                                  virtual; abstract;
    function GetShowTabs: Boolean; virtual; abstract;
    procedure SetShowTabs(const AShowTabs: Boolean); virtual; abstract;
  public
    // List of SourceEditorWindows
    function SourceWindowWithEditor(const AEditor: TSourceEditorInterface): TSourceEditorWindowInterface;
             virtual; abstract;
    function  IndexOfSourceWindowWithID(const AnID: Integer): Integer; virtual; abstract;
    procedure ShowActiveWindowOnTop(Focus: Boolean = False); virtual; abstract;
    function SourceWindowCount: integer; virtual; abstract;
    property SourceWindows[Index: integer]: TSourceEditorWindowInterface
             read GetSourceWindows;
    property ActiveSourceWindow: TSourceEditorWindowInterface
             read GetActiveSourceWindow write SetActiveSourceWindow;
    // List of SourceEditors (accross all Windows)
    function SourceEditorIntfWithFilename(const Filename: string): TSourceEditorInterface;
             virtual; abstract; // first hit, there might be more
    function SourceEditorCount: integer; virtual; abstract;
    property SourceEditors[Index: integer]: TSourceEditorInterface
             read GetSourceEditors;
    property ActiveEditor: TSourceEditorInterface
             read GetActiveEditor  write SetActiveEditor;
    // List of unique SourceEditors (excluding DualView)
    function UniqueSourceEditorCount: integer; virtual; abstract;
    property UniqueSourceEditors[Index: integer]: TSourceEditorInterface
             read GetUniqueSourceEditors;
    // Editor Preferences
    function GetEditorControlSettings(EditControl: TControl): boolean; virtual; abstract;
    function GetHighlighterSettings(Highlighter: TObject): boolean; virtual; abstract;
    // Messages
    procedure ClearErrorLines; virtual; abstract;
    // General source functions
    function Beautify(const Src: string): string; virtual; abstract;
  protected
    // Completion Plugins
    function  GetActiveCompletionPlugin: TSourceEditorCompletionPlugin; virtual; abstract;
    function  GetCompletionBoxPosition: integer; virtual; abstract;
    function  GetCompletionPlugins(Index: integer): TSourceEditorCompletionPlugin; virtual; abstract;
    function  GetDefaultSynCompletionForm: TCustomForm; virtual; abstract;
    function  GetSynCompletionLinesInWindow: integer; virtual; abstract;
    procedure SetSynCompletionLinesInWindow(LineCnt: integer); virtual; abstract;
  public
    // Completion Plugins
    function CompletionPluginCount: integer; virtual; abstract;
    property CompletionPlugins[Index: integer]: TSourceEditorCompletionPlugin
                 read GetCompletionPlugins;
    property DefaultSynCompletionForm: TCustomForm read GetDefaultSynCompletionForm;
    property SynCompletionLinesInWindow: integer read GetSynCompletionLinesInWindow
                                                write SetSynCompletionLinesInWindow;
    procedure DeactivateCompletionForm; virtual; abstract;
    property ActiveCompletionPlugin: TSourceEditorCompletionPlugin read GetActiveCompletionPlugin;
    property CompletionBoxPosition: integer read GetCompletionBoxPosition;
    procedure RegisterCompletionPlugin(Plugin: TSourceEditorCompletionPlugin); virtual; abstract;
    procedure UnregisterCompletionPlugin(Plugin: TSourceEditorCompletionPlugin); virtual; abstract;
  public
    procedure RegisterChangeEvent(AReason: TsemChangeReason; AHandler: TNotifyEvent); virtual; abstract;
    procedure UnRegisterChangeEvent(AReason: TsemChangeReason; AHandler: TNotifyEvent); virtual; abstract;
    procedure RegisterCopyPasteEvent(AHandler: TSemCopyPasteEvent); virtual; abstract;
    procedure UnRegisterCopyPasteEvent(AHandler: TSemCopyPasteEvent); virtual; abstract;
  public
    // source marklings
    function MarklingProducerCount: integer; virtual; abstract;
    property MarklingProducers[Index: integer]: TSourceMarklingProducer read GetMarklingProducers;
    procedure RegisterMarklingProducer(aProducer: TSourceMarklingProducer); virtual; abstract;
    procedure UnregisterMarklingProducer(aProducer: TSourceMarklingProducer); virtual; abstract;
    procedure InvalidateMarklingsOfAllFiles(aProducer: TSourceMarklingProducer); virtual; abstract;
    procedure InvalidateMarklings(aProducer: TSourceMarklingProducer; aFilename: string); virtual; abstract;
    property ShowTabs: Boolean read GetShowTabs write SetShowTabs;
  end;


var
  SourceEditorManagerIntf: TSourceEditorManagerInterface= nil;                      // set by the IDE

type
  TEditorMacroState = (emStopped, emRecording, emPlaying, emRecPaused); // msPaused = paused recording
  TEditorMacro = class;

  { TEditorMacroKeyBinding }

  TEditorMacroKeyBinding = class
  protected
    FOwner: TEditorMacro;
    function GetIdeCmd: TIDECommand; virtual;
  public
    constructor Create(AOwner: TEditorMacro); virtual;
    procedure WriteToXmlConf(AConf: TXMLConfig; const APath: String); virtual; abstract;
    procedure ReadFromXmlConf(AConf: TXMLConfig; const APath: String); virtual; abstract;

    procedure MacroNameChanged; virtual; // must be called by sub-class of TEditorMacro
    property  IdeCmd: TIDECommand read GetIdeCmd; // TKeyCommandRelation
    function  ShortCutAsText: String; virtual;
  end;

  TEditorMacroKeyBindingClass = class of TEditorMacroKeyBinding;

  { TEditorMacro
    "aEditor: TWinControl" must be TCustomSynEdit

  }

  TEditorMacro = class
  private
    FOnChange: TNotifyEvent;
    FOnStateChange: TNotifyEvent;
    procedure SetMacroNameFull(AValue: String);
  protected
     procedure DoChanged;
     procedure DoStateChanged;

    // (Un)setActivated: Must be called, whenever the state changes
    procedure SetActivated;
    procedure UnsetActivated;
    function  IsActivated: Boolean;
    procedure CheckStateAndActivated;

    function  GetMacroName: String; virtual; abstract;
    procedure SetMacroName(AValue: string); virtual; abstract;
    function  GetState: TEditorMacroState; virtual; abstract;
    function GetErrorMsg: String; virtual;
    function GetDefaultKeyBinding: TEditorMacroKeyBinding;
    function GetKeyBinding: TEditorMacroKeyBinding; virtual; abstract;

    procedure DoRecordMacro(aEditor: TWinControl); virtual; abstract;
    procedure DoPlaybackMacro(aEditor: TWinControl); virtual; abstract;
    procedure DoStop; virtual; abstract;
    procedure DoPause; virtual; abstract;
    procedure DoResume; virtual; abstract;
  public
    constructor Create(aOwner: TComponent); virtual; abstract;
    procedure AssignEventsFrom(AMacroRecorder: TEditorMacro); virtual; abstract;
    procedure WriteToXmlConf(AConf: TXMLConfig; const APath: String); virtual; abstract;
    procedure ReadFromXmlConf(AConf: TXMLConfig; const APath: String); virtual; abstract;
    function  GetAsSource: String; virtual; abstract;
    procedure SetFromSource(const AText: String); virtual; abstract;

    procedure RecordMacro(aEditor: TWinControl);
    procedure PlaybackMacro(aEditor: TWinControl);
    procedure Stop;
    procedure Pause;
    procedure Resume;

    procedure Clear; virtual; abstract;
    function  IsEmpty: Boolean; virtual; abstract;
    function  IsInvalid: Boolean; virtual; abstract;
    function  IsRecording(AnEditor: TWinControl): Boolean; virtual; abstract;

    property  MacroName: String read GetMacroName write SetMacroNameFull;
    property  State: TEditorMacroState read GetState;
    property  ErrorMsg: String read GetErrorMsg;
    property  OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property  OnChange: TNotifyEvent read FOnChange write FOnChange;            // Name, body
    property  KeyBinding: TEditorMacroKeyBinding read GetKeyBinding;
  end;

  TEditorMacroClass = class of TEditorMacro;

var
  // EditorMacroForRecording:
  // Used to record new Macros. On Completion it will be assigned to a player-macro
  EditorMacroForRecording: TEditorMacro = nil; // set by SourceEditor
  // ActiveEditorMacro:
  // Will be set whenever a macro is playing/recording. Ensures only one Macro is active
  ActiveEditorMacro: TEditorMacro = nil;
  DefaultBindingClass: TEditorMacroKeyBindingClass = nil;
  EditorMacroPlayerClass: TEditorMacroClass = nil;

type
  { TIDEInteractiveStringValue }

  TIDEInteractiveStringValue = class(TPersistent)
  private
    FValue: string;
  published
    property Value: string read FValue write FValue;
  end;

  { TIDETemplateParser }

  TIDETemplateParser = class
  protected
    function GetSrcPosition: Integer; virtual; abstract;
    function GetDestPosition: Integer; virtual; abstract;
    function GetDestPosX: Integer; virtual; abstract;
    function GetDestPosY: Integer; virtual; abstract;
    function GetSrcTemplate: String; virtual; abstract;
    function GetDestTemplate: String; virtual; abstract;
  public
    property SrcTemplate: String read GetSrcTemplate;
    property DestTemplate: String read GetDestTemplate;
    property SrcPosition: Integer read GetSrcPosition;
    property DestPosition: Integer read GetDestPosition;
    property DestPosX: Integer read GetDestPosX; // logical, byte position
    property DestPosY: Integer read GetDestPosY;
  end;

  { TIDECodeMacro - templates for sources, invoked by Ctrl-j }

  TIDECodeMacroGetValueProc = function(const Parameter: string;
                      InteractiveValue: TPersistent;
                      SrcEdit: TSourceEditorInterface;
                      var Value, ErrorMsg: string): boolean;
  TIDECodeMacroGetValueMethod = function(const Parameter: string;
                      InteractiveValue: TPersistent;
                      SrcEdit: TSourceEditorInterface;
                      var Value, ErrorMsg: string): boolean of object;

  TIDECodeMacroGetValueExProc = function(const Parameter: string;
                      InteractiveValue: TPersistent;
                      SrcEdit: TSourceEditorInterface;
                      var Value, ErrorMsg: string;
                      TemplateParser: TIDETemplateParser): boolean;
  TIDECodeMacroGetValueExMethod = function(const Parameter: string;
                      InteractiveValue: TPersistent;
                      SrcEdit: TSourceEditorInterface;
                      var Value, ErrorMsg: string;
                      TemplateParser: TIDETemplateParser): boolean of object;

  TIDECodeMacro = class
  private
    FInteractive: boolean;
    FInteractiveValueClass: TPersistentClass;
    FLongDescription: string;
    FName: string;
    FOnGetValueMethod: TIDECodeMacroGetValueMethod;
    FOnGetValueProc: TIDECodeMacroGetValueProc;
    FOnGetValueExMethod: TIDECodeMacroGetValueExMethod;
    FOnGetValueExProc: TIDECodeMacroGetValueExProc;
    FShortDescription: string;
  protected
    procedure Init; virtual;
  public
    constructor Create(const TheName: string);
    property Name: string read FName;
    property ShortDescription: string read FShortDescription write FShortDescription;
    property LongDescription: string read FLongDescription write FLongDescription;
    property OnGetValueProc: TIDECodeMacroGetValueProc read FOnGetValueProc
                                                       write FOnGetValueProc;
    property OnGetValueMethod: TIDECodeMacroGetValueMethod read FOnGetValueMethod
                                                       write FOnGetValueMethod;
    property OnGetValueExProc: TIDECodeMacroGetValueExProc read FOnGetValueExProc
                                                       write FOnGetValueExProc;
    property OnGetValueExMethod: TIDECodeMacroGetValueExMethod read FOnGetValueExMethod
                                                       write FOnGetValueExMethod;
    function GetValue(const Parameter: string; InteractiveValue: TPersistent;
                      SrcEdit: TSourceEditorInterface; out Value, ErrorMsg: string;
                      TemplateParser: TIDETemplateParser = nil): boolean; virtual;
    property Interactive: boolean read FInteractive write FInteractive;
    property InteractiveValueClass: TPersistentClass read FInteractiveValueClass
                                                   write FInteractiveValueClass;
  end;


  { TIDECodeMacros }

  TIDECodeMacros = class
  protected
    function GetItems(Index: integer): TIDECodeMacro; virtual; abstract;
  public
    property Items[Index: integer]: TIDECodeMacro read GetItems; default;
    function Count: integer; virtual; abstract;
    function Add(Macro: TIDECodeMacro): integer; virtual; abstract;
    function FindByName(const AName: string): TIDECodeMacro; virtual; abstract;
    function CreateUniqueName(const AName: string): string; virtual; abstract;
  end;

var
  IDECodeMacros: TIDECodeMacros = nil; // set by the IDE

function RegisterCodeMacro(const Name: string;
  const ShortDescription, LongDescription: string;
  OnGetValueProc: TIDECodeMacroGetValueProc;
  OnGetValueMethod: TIDECodeMacroGetValueMethod): TIDECodeMacro;

function RegisterCodeMacroEx(const Name: string;
  const ShortDescription, LongDescription: string;
  OnGetValueProc: TIDECodeMacroGetValueExProc;
  OnGetValueMethod: TIDECodeMacroGetValueExMethod): TIDECodeMacro;

{ SearchInFile to search in a file.
  This can be interactively or without user interaction.
  If the file is open in the source editor, changes will be added to the undo
  history.
}
type
  TIDESearchInTextAddMatch = procedure(const Filename: string;
                                  const StartPos, EndPos: TPoint;
                                  const Lines: string) of object;

  { TIDESearchInTextProgress }

  TIDESearchInTextProgress = class
  private
    FAbort: boolean;
    FOnAddMatch: TIDESearchInTextAddMatch;
  protected
    procedure SetAbort(const AValue: boolean); virtual;
  public
    property Abort: boolean read FAbort write SetAbort;
    property OnAddMatch: TIDESearchInTextAddMatch read FOnAddMatch write FOnAddMatch;
  end;

  TIDESearchInTextFunction = function(const TheFileName: string;
    var TheText: string;// if TheFileName='' then use TheText
    SearchFor, ReplaceText: string; Flags: TSrcEditSearchOptions;
    var Prompt: boolean; Progress: TIDESearchInTextProgress = nil): TModalResult;

  TBookmarkNumRange = 0..9;

var
  IDESearchInText: TIDESearchInTextFunction = nil;// set by the IDE

implementation

function RegisterCodeMacro(const Name: string; const ShortDescription,
  LongDescription: string; OnGetValueProc: TIDECodeMacroGetValueProc;
  OnGetValueMethod: TIDECodeMacroGetValueMethod): TIDECodeMacro;
var
  NewName: String;
begin
  NewName:=IDECodeMacros.CreateUniqueName(Name);
  Result:=TIDECodeMacro.Create(NewName);
  Result.ShortDescription:=LineBreaksToSystemLineBreaks(ShortDescription);
  Result.LongDescription:=LineBreaksToSystemLineBreaks(LongDescription);
  Result.OnGetValueProc:=OnGetValueProc;
  Result.OnGetValueMethod:=OnGetValueMethod;
  IDECodeMacros.Add(Result);
end;

function RegisterCodeMacroEx(const Name: string; const ShortDescription,
  LongDescription: string; OnGetValueProc: TIDECodeMacroGetValueExProc;
  OnGetValueMethod: TIDECodeMacroGetValueExMethod): TIDECodeMacro;
var
  NewName: String;
begin
  NewName:=IDECodeMacros.CreateUniqueName(Name);
  Result:=TIDECodeMacro.Create(NewName);
  Result.ShortDescription:=LineBreaksToSystemLineBreaks(ShortDescription);
  Result.LongDescription:=LineBreaksToSystemLineBreaks(LongDescription);
  Result.OnGetValueExProc:=OnGetValueProc;
  Result.OnGetValueExMethod:=OnGetValueMethod;
  IDECodeMacros.Add(Result);
end;

{ TEditorMacroKeyBinding }

function TEditorMacroKeyBinding.GetIdeCmd: TIDECommand;
begin
  Result :=nil;
end;

procedure TEditorMacroKeyBinding.MacroNameChanged;
begin
  //
end;

constructor TEditorMacroKeyBinding.Create(AOwner: TEditorMacro);
begin
  FOwner := AOwner;
end;

function TEditorMacroKeyBinding.ShortCutAsText: String;
begin
  Result := '';
end;

procedure TEditorMacro.CheckStateAndActivated;
begin
  if (State = emStopped) then begin
    if IsActivated then
      UnsetActivated;
  end
  else
  if not IsActivated then
    SetActivated;
end;

function TEditorMacro.GetDefaultKeyBinding: TEditorMacroKeyBinding;
begin
  Result := DefaultBindingClass.Create(Self);
end;

procedure TEditorMacro.SetMacroNameFull(AValue: String);
begin
  SetMacroName(AValue);
  if KeyBinding <> nil then
    KeyBinding.MacroNameChanged;
end;

function TEditorMacro.GetErrorMsg: String;
begin
  Result := '';
end;

procedure TEditorMacro.DoChanged;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TEditorMacro.DoStateChanged;
begin
  if Assigned(FOnStateChange) then
    FOnStateChange(Self);
end;

procedure TEditorMacro.SetActivated;
begin
  Assert(ActiveEditorMacro=nil, 'TEditorMacro.SetActivated: There already is an active Macro');
  ActiveEditorMacro := self;
end;

procedure TEditorMacro.UnsetActivated;
begin
  Assert(ActiveEditorMacro=self, 'TEditorMacro.SetActivated: This is not the active Macro');
  ActiveEditorMacro := nil;
end;

function TEditorMacro.IsActivated: Boolean;
begin
  Result := ActiveEditorMacro = self;
end;

procedure TEditorMacro.RecordMacro(aEditor: TWinControl);
begin
  SetActivated;
  DoRecordMacro(aEditor);
  CheckStateAndActivated;
end;

procedure TEditorMacro.PlaybackMacro(aEditor: TWinControl);
begin
  SetActivated;
  DoPlaybackMacro(aEditor);
  CheckStateAndActivated;
end;

procedure TEditorMacro.Stop;
begin
  DoStop;
  CheckStateAndActivated;
end;

procedure TEditorMacro.Pause;
begin
  DoPause;
  CheckStateAndActivated;
end;

procedure TEditorMacro.Resume;
begin
  DoResume;
  CheckStateAndActivated;
end;

{ TEditorMacro }

{ TSourceEditorInterface }

procedure TSourceEditorInterface.SelectText(LineNum, CharStart, LineNum2,
  CharEnd: Integer);
begin
  SelectText(Point(CharStart,LineNum),Point(CharEnd,LineNum2));
end;

procedure TSourceEditorInterface.ReplaceText(const StartPos, EndPos: TPoint;
  const NewText: string);
begin
  BeginUpdate;
  BeginUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditorInterface.ReplaceText'){$ENDIF};
  try
    SelectText(StartPos,EndPos);
    CursorTextXY:=StartPos;
    Selection:=NewText;
  finally
    EndUndoBlock{$IFDEF SynUndoDebugBeginEnd}('TSourceEditorInterface.ReplaceText'){$ENDIF};
    EndUpdate;
  end;
end;

{ TIDECodeMacro }

procedure TIDECodeMacro.Init;
begin
  FInteractiveValueClass:=TIDEInteractiveStringValue;
end;

constructor TIDECodeMacro.Create(const TheName: string);
begin
  FName:=TheName;
  FShortDescription:=FName;
  FLongDescription:=FName;
end;

function TIDECodeMacro.GetValue(const Parameter: string;
  InteractiveValue: TPersistent; SrcEdit: TSourceEditorInterface;
  out Value, ErrorMsg: string; TemplateParser: TIDETemplateParser = nil): boolean;
begin
  Value:=Parameter;
  ErrorMsg:='';
  if Assigned(OnGetValueProc) then
    Result:=OnGetValueProc(Parameter,InteractiveValue,SrcEdit,Value,ErrorMsg)
  else if Assigned(OnGetValueMethod) then
    Result:=OnGetValueMethod(Parameter,InteractiveValue,SrcEdit,Value,ErrorMsg)
  else if Assigned(OnGetValueExProc) then
    Result:=OnGetValueExProc(Parameter,InteractiveValue,SrcEdit,Value,ErrorMsg,
                             TemplateParser)
  else if Assigned(OnGetValueExMethod) then
    Result:=OnGetValueExMethod(Parameter,InteractiveValue,SrcEdit,Value,ErrorMsg,
                               TemplateParser)
  else
    Result:=true;
end;

{ TIDESearchInTextProgress }

procedure TIDESearchInTextProgress.SetAbort(const AValue: boolean);
begin
  if FAbort=AValue then exit;
  fAbort:=AValue;
end;

{ TSourceEditorCompletionPlugin }

function TSourceEditorCompletionPlugin.HasCustomPaint: boolean;
begin
  Result:=false;
end;

procedure TSourceEditorCompletionPlugin.PaintItem(const AKey: string;
  ACanvas: TCanvas; X, Y: integer; ItemSelected: boolean; Index: integer);
begin
  // this is called if HasCustomPaint returns true
end;

function TSourceEditorCompletionPlugin.MeasureItem(const AKey: string;
  ACanvas: TCanvas; ItemSelected: boolean; Index: integer): TPoint;
begin
  // this is called if HasCustomPaint returns true
  Result:=Point(0,0);
end;

{ TSourceMarkling }

constructor TSourceMarkling.Create(aProducer: TSourceMarklingProducer; TheID,
  aLine, aColumn: integer; aType: TSourceMarklingType);
begin
  FProducer:=aProducer;
  FLine:=aLine;
  FColumn:=aColumn;
  FId:=TheID;
  fType:=aType;
end;

{ TSourceMarklingProducer }

procedure TSourceMarklingProducer.InvalidateAllFiles;
begin
  SourceEditorManagerIntf.InvalidateMarklingsOfAllFiles(Self);
end;

procedure TSourceMarklingProducer.InvalidateFile(aFilename: string);
begin
  SourceEditorManagerIntf.InvalidateMarklings(Self,aFilename);
end;

end.

