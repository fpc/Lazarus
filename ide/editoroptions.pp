unit editoroptions;
{
  Author: Mattias Gaertner

  Abstract:
    Editor options container and editor options dialog.
    The editor options are stored in XML format in the
     ~/.lazarus/editoroptions.xml file.
    Currently only for TSynEdit.

  ToDo:
   - Code template deleting, value editing, saving
   - key mappings
   - color schemes, key mapping schemes
   - Resizing
   - SetSynEditSettings
   - nicer TColorButton
}

{$mode objfpc}

interface

{$define NEW_EDITOR_SYNEDIT}

uses
  LCLLinux,
  Forms, Classes, SysUtils, ComCtrls, Buttons, StdCtrls, ExtCtrls, LazConf,
  FileCtrl, Graphics, Controls, Dialogs, LResources,
{$ifdef NEW_EDITOR_SYNEDIT}
  SynEdit, SynEditHighlighter, SynHighlighterPas, SynEditAutoComplete,
  SynEditKeyCmds,
{$else}
  mwcustomedit, mwPasSyn, mwHighlighter,
{$endif}
  XMLCfg, CodeTemplateDialog;

const
  AdditionalHiglightAttributes : array[0..4] of string = (
    'Text block',
    'Execution point',
    'Enabled breakpoint','Disabled breakpoint',
    'Error line'
  );


type
{$ifdef NEW_EDITOR_SYNEDIT}
  TPreviewEditor = TSynEdit;
  TPreviewPasSyn = TSynPasSyn;
  TSynHighlightElement = TSynHighlighterAttributes;
{$else}
  TPreviewEditor = TmwCustomEdit;
  TPreviewPasSyn = TmwPasSyn;
  TSynHighlightElement = TmwHighlightAttributes;
{$endif}

  { Editor Options object used to hold the editor options }
  TEditorOptions = class(TPersistent)
  private
    xmlconfig:TXMLConfig;

    // general options
    fSynEditOptions: TSynEditorOptions;
    fUndoAfterSave:boolean;
    fDoubleClickLine:boolean;
    fFindTextAtCursor:boolean;
    fUseSyntaxHighlight:boolean;
    fCreateBackupFiles:boolean;
    fBlockIndent:integer;
    fUndoLimit:integer;
    fTabWidths:integer;
    fSyntaxExtensions:Ansistring;

    // Display options
    fVisibleRightMargin:boolean;
    fVisibleGutter:boolean;
    fShowLineNumbers:boolean;
    fGutterColor:TColor;
    fGutterWidth:integer;
    fRightMargin:integer;
    fEditorFont:Ansistring;
    fEditorFontHeight:integer;
    fExtraLineSpacing:integer;

    // Key Mappings options

    // Color options
    fColorScheme:Ansistring;
    fTextBlockElement:TSynHighlightElement;
    fExecutionPointElement:TSynHighlightElement;
    fEnabledBreakPointElement:TSynHighlightElement;
    fDisabledBreakPointElement:TSynHighlightElement;
    fErrorLineElement:TSynHighlightElement;

    // Code Insight options
    fAutoCodeCompletion:boolean;
    fAutoCodeParameters:boolean;
    fAutoToolTipExprEval:boolean;
    fAutoToolTipSymbInsight:boolean;
    fAutoDelayInMSec:integer;
    fCodeTemplateFileName:Ansistring;

  public
    constructor Create;
    destructor Destroy;  override;
    procedure Load;
    procedure Save;
    procedure ReadAttribute(Attri:TSynHighlightElement);
    procedure WriteAttribute(Attri:TSynHighlightElement);
    procedure GetHighlighterSettings(PasSyn:TPreviewPasSyn);
      // read highlight settings from config file
    procedure SetHighlighterSettings(PasSyn:TPreviewPasSyn);
      // write highlight settings to config file
    procedure GetSynEditSettings(ASynEdit:TSynEdit);
      // read synedit settings from config file
    procedure SetSynEditSettings(ASynEdit:TSynEdit);
      // write synedit settings to file

  published
    // general options
    property SynEditOptions:TSynEditorOptions read fSynEditOptions write fSynEditOptions
       default SYNEDIT_DEFAULT_OPTIONS;
    property UndoAfterSave:boolean read fUndoAfterSave write fUndoAfterSave default true;
    property DoubleClickLine:boolean
        read fDoubleClickLine write fDoubleClickLine default false;
    property FindTextAtCursor:boolean
        read fFindTextAtCursor write fFindTextAtCursor default true;
    property UseSyntaxHighlight:boolean
        read fUseSyntaxHighlight write fUseSyntaxHighlight default true;
    property CreateBackupFiles:boolean
        read fCreateBackupFiles write fCreateBackupFiles default true;
    property BlockIndent:integer read fBlockIndent write fBlockIndent default 2;
    property UndoLimit:integer read fUndoLimit write fUndoLimit default 32767;
    property TabWidths:integer read fTabWidths write fTabWidths default 8;
    property SyntaxExtensions:Ansistring read fSyntaxExtensions write fSyntaxExtensions;

    // Display options
    property VisibleRightMargin:boolean
        read fVisibleRightMargin write fVisibleRightMargin default true;
    property VisibleGutter:boolean read fVisibleGutter write fVisibleGutter default true;
    property ShowLineNumbers:boolean
        read fShowLineNumbers write fShowLineNumbers default false;
    property GutterColor:TColor read fGutterColor write fGutterColor default clBtnFace;
    property GutterWidth:integer read fGutterWidth write fGutterWidth default 30;
    property RightMargin:integer read fRightMargin write fRightMargin default 80;
    property EditorFont:Ansistring read fEditorFont write fEditorFont;
    property EditorFontHeight:integer read fEditorFontHeight write FEditorFontHeight;
    property ExtraLineSpacing:integer
        read fExtraLineSpacing write fExtraLineSpacing default 0;

    // Key Mappings

    // Color options
    property ColorScheme:Ansistring read fColorScheme write fColorScheme;
    property TextBlockElement:TSynHighlightElement
       read fTextBlockElement write fTextBlockElement;
    property ExecutionPointElement:TSynHighlightElement
       read fExecutionPointElement write fExecutionPointElement;
    property EnabledBreakPointElement:TSynHighlightElement
       read fEnabledBreakPointElement write fEnabledBreakPointElement;
    property DisabledBreakPointElement:TSynHighlightElement
       read fDisabledBreakPointElement write fDisabledBreakPointElement;
    property ErrorLineElement:TSynHighlightElement
       read fErrorLineElement write fErrorLineElement;

    // Code Insight options
    property AutoCodeCompletion:boolean
       read fAutoCodeCompletion write fAutoCodeCompletion default true;
    property AutoCodeParameters:boolean
       read fAutoCodeParameters write fAutoCodeParameters default true;
    property AutoToolTipExprEval:boolean
       read fAutoToolTipExprEval write fAutoToolTipExprEval default true;
    property AutoToolTipSymbInsight:boolean
       read fAutoToolTipSymbInsight write fAutoToolTipSymbInsight default true;
    property AutoDelayInMSec:integer
       read fAutoDelayInMSec write fAutoDelayInMSec default 1000;
    property CodeTemplateFileName:Ansistring
       read fCodeTemplateFileName write fCodeTemplateFileName;
  end;

  { color button }
  TColorButton = class(TCustomControl)
  private
    FOnColorChanged:TNotifyEvent;
    FButtonColor:TColor;
    FColorDialog:TColorDialog;
  protected
    procedure Paint; override;
    procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;
    procedure SetButtonColor(Value:TColor);
  published
    property ButtonColor:TColor read FButtonColor write SetButtonColor;
    property OnColorChanged:TNotifyEvent read FOnColorChanged write FOnColorChanged;
  end;

  { Editor Options form }
  TEditorOptionsForm = class(TForm)
  published
    MainNoteBook:TNoteBook;
    PreviewPasSyn:TPreviewPasSyn;

    // general options
    EditorOptionsGroupBox:TGroupBox;
    AltSetsColumnModeCheckBox:TCheckBox;
    AutoIndentCheckBox:TCheckBox;
    DragDropEditingCheckBox:TCheckBox;
    DropFilesCheckBox:TCheckBox;
    HalfPageScrollCheckBox:TCheckBox;
    KeepCaretXCheckBox:TCheckBox;
    NoCaretCheckBox:TCheckBox;
    NoSelectionCheckBox:TCheckBox;
    ScrollByOneLessCheckBox:TCheckBox;
    ScrollPastEofCheckBox:TCheckBox;
    ScrollPastEolCheckBox:TCheckBox;
    ShowScrollHintCheckBox:TCheckBox;
    SmartTabsCheckBox:TCheckBox;
    TabsToSpacesCheckBox:TCheckBox;
    TrimTrailingSpacesCheckBox:TCheckBox;
    UndoAfterSaveCheckBox:TCheckBox;
    DoubleClickLineCheckBox:TCheckBox;
    FindTextAtCursorCheckBox:TCheckBox;
    UseSyntaxHighlightCheckBox:TCheckBox;
    CreateBackupFilesCheckBox:TCheckBox;
    BlockIndentComboBox:TComboBox;
    BlockIndentLabel:TLabel;
    UndoLimitComboBox:TComboBox;
    UndoLimitLabel:TLabel;
    TabWidthsComboBox:TComboBox;
    TabWidthsLabel:TLabel;
    SyntaxExtensionsComboBox:TComboBox;
    SyntaxExtensionsLabel:TLabel;

    // Display options
    MarginAndGutterGroupBox:TGroupBox;
    VisibleRightMarginCheckBox:TCheckBox;
    VisibleGutterCheckBox:TCheckBox;
    ShowLineNumbersCheckBox:TCheckBox;
    GutterColorButton:TColorButton;
    GutterColorLabel:TLabel;
    GutterWidthComboBox:TComboBox;
    GutterWidthLabel:TLabel;
    RightMarginComboBox:TComboBox;
    RightMarginLabel:TLabel;
    EditorFontGroupBox:TGroupBox;
    EditorFontComboBox:TComboBox;
    EditorFontButton:TButton;
    EditorFontLabel:TLabel;
    EditorFontHeightLabel:TLabel;
    EditorFontHeightComboBox:TComboBox;
    ExtraLineSpacingLabel:TLabel;
    ExtraLineSpacingComboBox:TComboBox;
    DisplayPreview:TPreviewEditor;

    // Key Mappings options

    // Color options
    ColorSchemeComboBox:TComboBox;
    ColorSchemeLabel:TLabel;
    ColorElementLabel:TLabel;
    ColorElementListBox:TListBox;
    TextAttributesGroupBox:TGroupBox;
    TextBoldCheckBox:TCheckBox;
    TextItalicCheckBox:TCheckBox;
    TextUnderlineCheckBox:TCheckBox;
    ForeGroundGroupBox:TGroupBox;
    ForeGroundColorButton:TColorButton;
    ForeGroundUseDefaultCheckBox:TCheckBox;
    BackGroundGroupBox:TGroupBox;
    BackGroundColorButton:TColorButton;
    BackGroundUseDefaultCheckBox:TCheckBox;
    ColorPreview:TPreviewEditor;

    // Code Insight options
    AutomaticFeaturesGroupBox:TGroupBox;
    AutoCodeCompletionCheckBox:TCheckBox;
    AutoCodeParametersCheckBox:TCheckBox;
    AutoToolTipExprEvalCheckBox:TCheckBox;
    AutoToolTipSymbInsightCheckBox:TCheckBox;
    AutoDelayLabel:TLabel;
    AutoDelayTrackBar:TTrackBar;
    AutoDelayMinLabel:TLabel;
    AutoDelayMaxLabel:TLabel;
    CodeTemplatesGroupBox:TGroupBox;
    CodeTemplateFileNameLabel:TLabel;
    CodeTemplateFileNameComboBox:TComboBox;
    CodeTemplateFileNameButton:TButton;
    CodeTemplatesLabel:TLabel;
    CodeTemplateListBox:TListBox;
    CodeTemplateAddButton:TButton;
    CodeTemplateEditButton:TButton;
    CodeTemplateDeleteButton:TButton;
    CodeTemplateCodeLabel:TLabel;
    CodeTemplateCodePreview:TPreviewEditor;
    SynAutoComplete:TSynAutoComplete;

    // buttons at bottom
    OkButton:TButton;
    CancelButton:TButton;

    // general
    procedure GeneralCheckBoxOnClick(Sender: TObject);
    procedure ComboBoxOnChange(Sender:TObject);
    procedure ComboBoxOnExit(Sender:TObject);
    procedure ComboBoxOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ColorButtonColorChanged(Sender:TObject);

    // display
    procedure EditorFontButtonClick(Sender:TObject);

    // color
    procedure ColorElementListBoxMouseUp(Sender:TObject;
       Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
    procedure ColorPreviewMouseUp(Sender:TObject;
       Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
    procedure OnSpecialLineColors(Sender: TObject; Line: integer;
       var Special: boolean; var FG, BG: TColor);

    // code insight
    procedure CodeTemplateListBoxMouseUp(Sender:TObject;
       Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
    procedure CodeTemplateFileNameButtonClick(Sender:TObject);
    procedure CodeTemplateButtonClick(Sender:TObject);

    // buttons at bottom
    procedure OkButtonClick(Sender:TObject);
    procedure CancelButtonClick(Sender:TObject);
  private
    PreviewEdits:array[1..3] of TPreviewEditor;
    AddHighlightElements:array [Low(AdditionalHiglightAttributes)..
            High(AdditionalHiglightAttributes)] of TSynHighlightElement;
    CurHighlightElement:TSynHighlightElement;

    procedure SetupButtonBar;
    procedure SetupGeneralPage;
    procedure SetupDisplayPage;
    procedure SetupKeyMappingsPage;
    procedure SetupColorPage;
    procedure SetupCodeInsightPage;
    procedure SetComboBoxText(AComboBox:TComboBox;AText:AnsiString);
    procedure FillCodeTemplateListBox;

    procedure ShowCurAttribute;
    procedure FindCurHighlightElement;
    procedure FontDialogNameToFont(FontDialogName:string;AFont:TFont);
    procedure InvalidatePreviews;
    procedure ShowCurCodeTemplate;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  EditorOptionsForm: TEditorOptionsForm;
  EditorOpts: TEditorOptions;

function ShowEditorOptionsDialog:TModalResult;

implementation

function ShowEditorOptionsDialog:TModalResult;
var
  EditorOptionsForm: TEditorOptionsForm;
begin
  Result:=mrCancel;
  EditorOptionsForm:=TEditorOptionsForm.Create(Application);
  try
    Result:=EditorOptionsForm.ShowModal;
  finally
    EditorOptionsForm.Free;
  end;
end;


const
  EditOptsConfFileName = 'editoroptions.xml';

  ExampleSource : array[1..21] of string = (
    'procedure TForm1.Button1Click(Sender: TObject);',
    'var',
    '  Number, I, X: Integer;',
    'begin',
    '  Number := 12345;',
    '  Caption := ''The number is '' + IntToStr(Number);',
    '  asm',
    '    MOV AX,1234h',
    '    MOV Number,AX',
    '  end;',
    '  X := 10;',
    '  { Search Match, Text Block }',
    '  for I := 0 to Number do { execution point }',
    '  begin',
    '    Inc(X); { Enabled breakpoint }',
    '    Dec(X); { Disabled breakpoint }',
    '    X := X + 1.0; { Error line }',
    '    ListBox1.Items.Add(IntToStr(X));',
    '  end;',
    'end;',
    ''
  );


{ TEditorOptions }

constructor TEditorOptions.Create;
var ConfFileName,SecConfFileName:string;
begin
  inherited Create;
  ConfFileName:=SetDirSeparators(GetPrimaryConfigPath+'/'+EditOptsConfFileName);
  if (not FileExists(ConfFileName)) then begin
    SecConfFileName:=SetDirSeparators(
      GetSecondaryConfigPath+'/'+EditOptsConfFileName);
    if (not FileExists(SecConfFileName)) then begin
      // XXX
      writeln('editor options config file not found');
    end else begin
      ConfFileName:=SecConfFileName;
    end;
  end;
writeln('EditorOptionsFile=',ConfFilename);
  XMLConfig:=TXMLConfig.Create(ConfFileName);

  // set defaults

  // general options
  fSyntaxExtensions:='pp;inc;lfm;lrs;pas;dpr;dfm;dpk';

  // Display options
  fEditorFont:='courier';

  // Key Mappings options

  // Color options
  fColorScheme:='Default';
  fTextBlockElement:=
     TSynHighlightElement.Create(AdditionalHiglightAttributes[0]);
  fExecutionPointElement:=
     TSynHighlightElement.Create(AdditionalHiglightAttributes[1]);
  fEnabledBreakPointElement:=
     TSynHighlightElement.Create(AdditionalHiglightAttributes[2]);
  fDisabledBreakPointElement:=
     TSynHighlightElement.Create(AdditionalHiglightAttributes[3]);
  fErrorLineElement:=
     TSynHighlightElement.Create(AdditionalHiglightAttributes[4]);

  // Code Insight options
  fCodeTemplateFileName:=SetDirSeparators(GetPrimaryConfigPath+'/lazarus.dci');
end;

destructor TEditorOptions.Destroy;
begin
  fTextBlockElement.Free;
  fExecutionPointElement.Free;
  fEnabledBreakPointElement.Free;
  fDisabledBreakPointElement.Free;
  fErrorLineElement.Free;

  XMLConfig.Free;
  inherited Destroy;
end;

procedure TEditorOptions.Load;
// load options from XML file
var SynEditOpt:TSynEditorOption;
  SynEditOptName:ansistring;
begin
writeln('AAAAAAAAAAAAAAAAAAAAAAAAAAAAA');
  // general options
  for SynEditOpt:=Low(TSynEditorOption) to High(TSynEditorOption) do begin
    case SynEditOpt of
      eoAltSetsColumnMode:SynEditOptName:='AltSetsColumnMode';
      eoAutoIndent:SynEditOptName:='AutoIndent';
      eoDragDropEditing:SynEditOptName:='DragDropEditing';
      eoDropFiles:SynEditOptName:='DropFiles';
      eoHalfPageScroll:SynEditOptName:='HalfPageScroll';
      eoKeepCaretX:SynEditOptName:='KeepCaretX';
      eoNoCaret:SynEditOptName:='NoCaret';
      eoNoSelection:SynEditOptName:='NoSelection';
      eoScrollByOneLess:SynEditOptName:='ScrollByOneLess';
      eoScrollPastEof:SynEditOptName:='ScrollPastEof';
      eoScrollPastEol:SynEditOptName:='ScrollPastEol';
      eoShowScrollHint:SynEditOptName:='ShowScrollHint';
      eoSmartTabs:SynEditOptName:='SmartTabs';
      eoTabsToSpaces:SynEditOptName:='TabsToSpaces';
      eoTrimTrailingSpaces:SynEditOptName:='TrimTrailingSpaces';
    else
      SynEditOptName:='';
    end;
    if SynEditOptName<>'' then begin
      if XMLConfig.GetValue('EditorOptions/General/Editor/'+SynEditOptName,
          SynEditOpt in SYNEDIT_DEFAULT_OPTIONS) then
        Include(fSynEditOptions,SynEditOpt)
      else
        Exclude(fSynEditOptions,SynEditOpt);
    end;
  end;

  fUndoAfterSave:=
    XMLConfig.GetValue('EditorOptions/General/Editor/UndoAfterSave',true);
writeln('UndoAfterSave',fUndoAfterSave);
  fDoubleClickLine:=
    XMLConfig.GetValue('EditorOptions/General/Editor/DoubleClickLine',false);
  fFindTextAtCursor:=
    XMLConfig.GetValue('EditorOptions/General/Editor/FindTextAtCursor',true);
  fUseSyntaxHighlight:=
    XMLConfig.GetValue('EditorOptions/General/Editor/UseSyntaxHighlight',true);
  fCreateBackupFiles:=
    XMLConfig.GetValue('EditorOptions/General/Editor/CreateBackupFiles',true);
  fBlockIndent:=
    XMLConfig.GetValue('EditorOptions/General/Editor/BlockIndent',2);
  fUndoLimit:=
    XMLConfig.GetValue('EditorOptions/General/Editor/UndoLimit',32767);
  fTabWidths:=
    XMLConfig.GetValue('EditorOptions/General/Editor/TabWidths',8);
  fSyntaxExtensions:=
    XMLConfig.GetValue('EditorOptions/General/Editor/SyntaxExtensions'
      ,'pp;inc;lfm;lfc;pas;dpr;dfm;dpk');

  // Display options
  fVisibleRightMargin:=
    XMLConfig.GetValue('EditorOptions/Display/VisibleRightMargin',true);
  fVisibleGutter:=
    XMLConfig.GetValue('EditorOptions/Display/VisibleGutter',true);
  fShowLineNumbers:=
    XMLConfig.GetValue('EditorOptions/Display/ShowLineNumbers',false);
  fGutterColor:=
    XMLConfig.GetValue('EditorOptions/Display/GutterColor',clBtnFace);
  fGutterWidth:=
    XMLConfig.GetValue('EditorOptions/Display/GutterWidth',30);
  fRightMargin:=
    XMLConfig.GetValue('EditorOptions/Display/RightMargin',80);
  fEditorFont:=
    XMLConfig.GetValue('EditorOptions/Display/EditorFont','courier');
  fEditorFontHeight:=
    XMLConfig.GetValue('EditorOptions/Display/EditorFontHeight',12);
  fExtraLineSpacing:=
    XMLConfig.GetValue('EditorOptions/Display/ExtraLineSpacing',1);

  // Key Mappings options

  // Color options
  fColorScheme:=
    XMLConfig.GetValue('EditorOptions/Color/ColorScheme','Default');
  ReadAttribute(fTextBlockElement);
  ReadAttribute(fExecutionPointElement);
  ReadAttribute(fEnabledBreakPointElement);
  ReadAttribute(fDisabledBreakPointElement);
  ReadAttribute(fErrorLineElement);

  // Code Insight options
  fAutoCodeCompletion:=
    XMLConfig.GetValue('EditorOptions/CodeInsight/AutoCodeCompletion',true);
  fAutoCodeParameters:=
    XMLConfig.GetValue('EditorOptions/CodeInsight/AutoCodeParameters',true);
  fAutoToolTipExprEval:=
    XMLConfig.GetValue('EditorOptions/CodeInsight/AutoToolTipExprEval',true);
  fAutoToolTipSymbInsight:=
    XMLConfig.GetValue('EditorOptions/CodeInsight/AutoToolTipSymbInsight',true);
  fAutoDelayInMSec:=
    XMLConfig.GetValue('EditorOptions/CodeInsight/AutoDelayInMSec',1000);
  fCodeTemplateFileName:=
    XMLConfig.GetValue('EditorOptions/CodeInsight/CodeTemplateFileName'
      ,SetDirSeparators(GetPrimaryConfigPath+'/lazarus.dci'));
end;

procedure TEditorOptions.Save;
// save options to XML file
var SynEditOpt:TSynEditorOption;
  SynEditOptName:ansistring;
begin
  // general options
  for SynEditOpt:=Low(TSynEditorOption) to High(TSynEditorOption) do begin
    case SynEditOpt of
      eoAltSetsColumnMode:SynEditOptName:='AltSetsColumnMode';
      eoAutoIndent:SynEditOptName:='AutoIndent';
      eoDragDropEditing:SynEditOptName:='DragDropEditing';
      eoDropFiles:SynEditOptName:='DropFiles';
      eoHalfPageScroll:SynEditOptName:='HalfPageScroll';
      eoKeepCaretX:SynEditOptName:='KeepCaretX';
      eoNoCaret:SynEditOptName:='NoCaret';
      eoNoSelection:SynEditOptName:='NoSelection';
      eoScrollByOneLess:SynEditOptName:='ScrollByOneLess';
      eoScrollPastEof:SynEditOptName:='ScrollPastEof';
      eoScrollPastEol:SynEditOptName:='ScrollPastEol';
      eoShowScrollHint:SynEditOptName:='ShowScrollHint';
      eoSmartTabs:SynEditOptName:='SmartTabs';
      eoTabsToSpaces:SynEditOptName:='TabsToSpaces';
      eoTrimTrailingSpaces:SynEditOptName:='TrimTrailingSpaces';
    else
      SynEditOptName:='';
    end;
    if SynEditOptName<>'' then begin
      XMLConfig.SetValue('EditorOptions/General/Editor/'+SynEditOptName,
          SynEditOpt in fSynEditOptions);
    end;
  end;

  XMLConfig.SetValue('EditorOptions/General/Editor/UndoAfterSave'
    ,fUndoAfterSave);
  XMLConfig.SetValue('EditorOptions/General/Editor/DoubleClickLine'
    ,fDoubleClickLine);
  XMLConfig.SetValue('EditorOptions/General/Editor/FindTextAtCursor'
    ,fFindTextAtCursor);
  XMLConfig.SetValue('EditorOptions/General/Editor/UseSyntaxHighlight'
    ,fUseSyntaxHighlight);
  XMLConfig.SetValue('EditorOptions/General/Editor/CreateBackupFiles'
    ,fCreateBackupFiles);
  XMLConfig.SetValue('EditorOptions/General/Editor/BlockIndent'
    ,fBlockIndent);
  XMLConfig.SetValue('EditorOptions/General/Editor/UndoLimit'
    ,fUndoLimit);
  XMLConfig.SetValue('EditorOptions/General/Editor/TabWidths'
    ,fTabWidths);
  XMLConfig.SetValue('EditorOptions/General/Editor/SyntaxExtensions'
    ,fSyntaxExtensions);

  // Display options
  XMLConfig.SetValue('EditorOptions/Display/VisibleRightMargin'
    ,fVisibleRightMargin);
  XMLConfig.SetValue('EditorOptions/Display/VisibleGutter',fVisibleGutter);
  XMLConfig.GetValue('EditorOptions/Display/ShowLineNumbers',fShowLineNumbers);
  XMLConfig.GetValue('EditorOptions/Display/GutterColor',fGutterColor);
  XMLConfig.SetValue('EditorOptions/Display/GutterWidth',fGutterWidth);
  XMLConfig.SetValue('EditorOptions/Display/RightMargin',fRightMargin);
  XMLConfig.SetValue('EditorOptions/Display/EditorFont',fEditorFont);
  XMLConfig.GetValue('EditorOptions/Display/EditorFontHeight'
    ,fEditorFontHeight);
  XMLConfig.GetValue('EditorOptions/Display/ExtraLineSpacing'
    ,fExtraLineSpacing);

  // Key Mappings options

  // Color options
  XMLConfig.SetValue('EditorOptions/Color/ColorScheme',fColorScheme);
  WriteAttribute(fTextBlockElement);
  WriteAttribute(fExecutionPointElement);
  WriteAttribute(fEnabledBreakPointElement);
  WriteAttribute(fDisabledBreakPointElement);
  WriteAttribute(fErrorLineElement);

  // Code Insight options
  XMLConfig.SetValue('EditorOptions/CodeInsight/AutoCodeCompletion'
    ,fAutoCodeCompletion);
  XMLConfig.SetValue('EditorOptions/CodeInsight/AutoCodeParameters'
    ,fAutoCodeParameters);
  XMLConfig.SetValue('EditorOptions/CodeInsight/AutoToolTipExprEval'
    ,fAutoToolTipExprEval);
  XMLConfig.SetValue('EditorOptions/CodeInsight/AutoToolTipSymbInsight'
    ,fAutoToolTipSymbInsight);
  XMLConfig.SetValue('EditorOptions/CodeInsight/AutoDelayInMSec'
    ,fAutoDelayInMSec);
  XMLConfig.SetValue('EditorOptions/CodeInsight/CodeTemplateFileName'
    ,fCodeTemplateFileName);

  XMLConfig.Flush;
end;

procedure TEditorOptions.ReadAttribute(Attri:TSynHighlightElement);
var b:boolean;
  fs:TFontStyles;
  AttriName:string;
  a:integer;
  DefBGCol,DefFGCol:TColor;
  DefFontStyles:TFontStyles;
begin
  AttriName:=Attri.Name;
  DefFGCol:=clNone;
  DefBGCol:=clNone;
  DefFontStyles:=[];
  if AttriName='Assembler' then begin
    DefFGCol:=clGreen;
  end else if AttriName='Comment' then begin
    DefFGCol:=clBlue;
    DefFontStyles:=[fsBold];
  end else if AttriName='Reserved word' then begin
    DefFontStyles:=[fsBold];
  end else if AttriName='Number' then begin
    DefFGCol:=clNavy;
  end else if AttriName='String' then begin
    DefFGCol:=clBlue;
  end else if AttriName='Symbol' then begin
    DefFGCol:=clRed;
  end else if AttriName=AdditionalHiglightAttributes[0] then begin
    DefBGCol:=clNavy;
    DefFGCol:=clWhite;
  end else if AttriName=AdditionalHiglightAttributes[1] then begin
    DefBGCol:=clDKGray;
    DefFGCol:=clWhite;
  end else if AttriName=AdditionalHiglightAttributes[2] then begin
    DefBGCol:=clRed;
    DefFGCol:=clBlack;
  end else if AttriName=AdditionalHiglightAttributes[3] then begin
    DefBGCol:=clGreen;
    DefFGCol:=clBlack;
  end else if AttriName=AdditionalHiglightAttributes[4] then begin
    DefBGCol:=$50a0ff;
    DefFGCol:=clBlack;
  end;
  for a:=1 to length(AttriName) do
    if (not (AttriName[a] in ['a'..'z','A'..'Z'])) then
      AttriName[a]:='_';
  Attri.BackGround:=XMLConfig.GetValue(
    'EditorOptions/Color/'+ColorScheme+'/'+AttriName+'/BackgroundColor',DefBGCol);
  Attri.ForeGround:=XMLConfig.GetValue(
    'EditorOptions/Color/'+ColorScheme+'/'+AttriName+'/ForegroundColor',DefFGCol);
  fs:=[];
  b:=XMLConfig.GetValue(
    'EditorOptions/Color/'+ColorScheme+'/'+AttriName+'/Bold'
    ,fsBold in DefFontStyles);
  if b then fs:=fs+[fsBold];
  b:=XMLConfig.GetValue(
    'EditorOptions/Color/'+ColorScheme+'/'+AttriName+'/Italic'
    ,fsItalic in DefFontStyles);
  if b then fs:=fs+[fsItalic];
  b:=XMLConfig.GetValue(
    'EditorOptions/Color/'+ColorScheme+'/'+AttriName+'/Underline'
    ,fsUnderLine in DefFontStyles);
  if b then fs:=fs+[fsUnderline];
  Attri.Style:=fs;
end;

procedure TEditorOptions.GetHighlighterSettings(PasSyn:TPreviewPasSyn);
begin
  ReadAttribute(PasSyn.AsmAttri);
  ReadAttribute(PasSyn.CommentAttri);
  ReadAttribute(PasSyn.IdentifierAttri);
  ReadAttribute(PasSyn.KeyAttri);
  ReadAttribute(PasSyn.NumberAttri);
  ReadAttribute(PasSyn.SpaceAttri);
  ReadAttribute(PasSyn.StringAttri);
  ReadAttribute(PasSyn.SymbolAttri);
end;

procedure TEditorOptions.WriteAttribute(Attri:TSynHighlightElement);
var AttriName:string;
  a:integer;
begin
  AttriName:=Attri.Name;
  for a:=1 to length(AttriName) do
    if (not (AttriName[a] in ['a'..'z','A'..'Z'])) then
      AttriName[a]:='_';
  XMLConfig.SetValue(
    'EditorOptions/Color/'+ColorScheme+'/'+AttriName+'/BackgroundColor'
    ,Attri.BackGround);
  XMLConfig.SetValue(
    'EditorOptions/Color/'+ColorScheme+'/'+AttriName+'/ForegroundColor'
    ,Attri.ForeGround);
  XMLConfig.SetValue(
    'EditorOptions/Color/'+ColorScheme+'/'+AttriName+'/Bold'
    ,fsBold in Attri.Style);
  XMLConfig.SetValue(
    'EditorOptions/Color/'+ColorScheme+'/'+AttriName+'/Italic'
    ,fsItalic in Attri.Style);
  XMLConfig.SetValue(
    'EditorOptions/Color/'+ColorScheme+'/'+AttriName+'/Underline'
    ,fsUnderline in Attri.Style);
end;

procedure TEditorOptions.SetHighlighterSettings(PasSyn:TPreviewPasSyn);
begin
  WriteAttribute(PasSyn.AsmAttri);
  WriteAttribute(PasSyn.CommentAttri);
  WriteAttribute(PasSyn.IdentifierAttri);
  WriteAttribute(PasSyn.KeyAttri);
  WriteAttribute(PasSyn.NumberAttri);
  WriteAttribute(PasSyn.SpaceAttri);
  WriteAttribute(PasSyn.StringAttri);
  WriteAttribute(PasSyn.SymbolAttri);
end;

procedure TEditorOptions.GetSynEditSettings(ASynEdit:TSynEdit);
// read synedit setings from config file
begin
  // general options
  ASynEdit.Options:=fSynEditOptions;
  ASynEdit.TabWidth:=fTabWidths;

  // Display options
  ASynEdit.Gutter.Visible:=fVisibleGutter;
  ASynEdit.Gutter.ShowLineNumbers:=fShowLineNumbers;
  ASynEdit.Gutter.Color:=fGutterColor;
  ASynEdit.Gutter.Width:=fGutterWidth;
  ASynEdit.RightEdge:=fRightMargin;
  ASynEdit.Font.Name:=fEditorFont;
  ASynEdit.Font.Height:=fEditorFontHeight;
  ASynEdit.ExtraLineSpacing:=fExtraLineSpacing;
end;

procedure TEditorOptions.SetSynEditSettings(ASynEdit:TSynEdit);
// write synedit settings to file
begin
  // general options
  fSynEditOptions:=ASynEdit.Options;
  fTabWidths:=ASynEdit.TabWidth;

  // Display options
  fVisibleGutter:=ASynEdit.Gutter.Visible;
  fShowLineNumbers:=ASynEdit.Gutter.ShowLineNumbers;
  fGutterColor:=ASynEdit.Gutter.Color;
  fGutterWidth:=ASynEdit.Gutter.Width;
  fRightMargin:=ASynEdit.RightEdge;
  fEditorFont:=ASynEdit.Font.Name;
  fEditorFontHeight:=ASynEdit.Font.Height;
  fExtraLineSpacing:=ASynEdit.ExtraLineSpacing;

  // XXX:  update all checkboxes, comboboxes...
end;


{ TColorButton }

procedure TColorButton.Paint;
var a:integer;
begin
  inherited Paint;
  with Canvas do begin
    Pen.Color:=cl3DLight;
    for a:=0 to BorderWidth-1 do begin
      MoveTo(a,Height-a);
      LineTo(a,a);
      LineTo(Width-1,a);
    end;
    Pen.Color:=cl3DDkShadow;
    for a:=0 to BorderWidth-1 do begin
      MoveTo(Width-a-1,a);
      LineTo(Width-a-1,Height-a-1);
      MoveTo(Width-a,Height-a-1);
      LineTo(a,Height-a-1);
    end;
    Brush.Color:=ButtonColor;
    FillRect(
      Rect(BorderWidth,BorderWidth,Width-BorderWidth,Height-BorderWidth));
  end;
end;

procedure TColorButton.SetButtonColor(Value:TColor);
begin
  if Value=FButtonColor then exit;
  FButtonColor:=Value;
  if Assigned(FOnColorChanged) then begin
    FOnColorChanged(Self);
  end;
  Invalidate;
end;

procedure TColorButton.MouseDown(Button:TMouseButton; Shift:TShiftState;
X,Y:integer);
begin
  if FColorDialog<>nil then exit;
  FColorDialog:=TColorDialog.Create(Application);
  try
    FColorDialog.Color:=ButtonColor;
    if FColorDialog.Execute then begin
      ButtonColor:=FColorDialog.Color;
      Invalidate;
    end;
  finally
    FColorDialog.Free;
    FColorDialog:=nil;
  end;
  inherited MouseDown(Button,Shift,X,Y);
end;

{ TEditorOptionsForm }

constructor TEditorOptionsForm.Create(AOwner:TComponent);
var a:integer;
  s:Ansistring;
begin
  inherited Create(AOwner);

  EditorOpts.Load;
  if LazarusResources.Find(ClassName)=nil then begin  
    Height:=455;
    Width:=459;
    Caption:='Editor Options';
    PreviewPasSyn:=TPreviewPasSyn.Create(Self);
    SynAutoComplete:=TSynAutoComplete.Create(Self);

    MainNoteBook:=TNoteBook.Create(Self);
    with MainNoteBook do begin
      Parent:=Self;
      Top:=0;
      Left:=0;
      Width:=Self.Width-4;
      Height:=Self.Height-50;
      Pages.Strings[0]:='General';
      Pages.Add('Display');
      Pages.Add('Key Mappings');
      Pages.Add('Color');
      Pages.Add('Code Insight');
    end;

    SetupGeneralPage;
    SetupDisplayPage;
    SetupKeyMappingsPage;
    SetupColorPage;
    SetupCodeInsightPage;

    MainNoteBook.Show;

    SetupButtonBar;
  end;


  for a:=Low(PreviewEdits) to High(PreviewEdits) do
    PreviewEdits[a]:=nil;
  EditorOpts.GetHighlighterSettings(PreviewPasSyn);

  AddHighlightElements[0]:=EditorOpts.TextBlockElement;
  AddHighlightElements[1]:=EditorOpts.ExecutionPointElement;
  AddHighlightElements[2]:=EditorOpts.EnabledBreakPointElement;
  AddHighlightElements[3]:=EditorOpts.DisabledBreakPointElement;
  AddHighlightElements[4]:=EditorOpts.ErrorLineElement;
  CurHighlightElement:=nil;

  with SynAutoComplete do begin
    s:=EditorOpts.CodeTemplateFileName;
    if FileExists(s) then
      try
        AutoCompleteList.LoadFromFile(s);
      except
        writeln('ERROR unable to read code template file ''',s,'''');
        Halt;
      end;
  end;

  PreviewEdits[1]:=DisplayPreview;
  PreviewEdits[2]:=ColorPreview;
  PreviewEdits[3]:=CodeTemplateCodePreview;
  for a:=Low(PreviewEdits) to High(PreviewEdits) do begin
    if PreviewEdits[a]<>nil then with PreviewEdits[a] do begin
      EditorOpts.GetSynEditSettings(PreviewEdits[a]);
      if EditorOpts.UseSyntaxHighlight then
        Highlighter:=PreviewPasSyn;
    end;
  end;
  CodeTemplateCodePreview.Gutter.Visible:=false;

  FindCurHighlightElement;
  FillCodeTemplateListBox;
//  with CodeTemplateListBox do
//    if Items.Count>0 then Selected[0]:=true;
//  ShowCurCodeTemplate;
end;

// general

procedure TEditorOptionsForm.GeneralCheckBoxOnClick(Sender: TObject);
var a:integer;
  NewColor:TColor;

  procedure SetOption(ACheckBox:TCheckBox;AnOption:TSynEditorOption);
  var a:integer;
  begin
    if Sender=ACheckBox then
      for a:=Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a]<>nil then
          if ACheckBox.Checked then
            PreviewEdits[a].Options:=PreviewEdits[a].Options+[AnOption]
          else
            PreviewEdits[a].Options:=PreviewEdits[a].Options-[AnOption]
  end;

// GeneralCheckBoxOnClick
begin
  {$IFDEF NEW_EDITOR_SYNEDIT}
  // general
  SetOption(AltSetsColumnModeCheckBox,eoAltSetsColumnMode);
  SetOption(AutoIndentCheckBox,eoAutoIndent);
  SetOption(DragDropEditingCheckBox,eoDragDropEditing);
  SetOption(DropFilesCheckBox,eoDropFiles);
  SetOption(HalfPageScrollCheckBox,eoHalfPageScroll);
  SetOption(KeepCaretXCheckBox,eoKeepCaretX);
  SetOption(NoCaretCheckBox,eoNoCaret);
  SetOption(NoSelectionCheckBox,eoNoSelection);
  SetOption(ScrollByOneLessCheckBox,eoScrollByOneLess);
  SetOption(ScrollPastEoFCheckBox,eoScrollPastEoF);
  SetOption(ScrollPastEoLCheckBox,eoScrollPastEoL);
  SetOption(ShowScrollHintCheckBox,eoShowScrollHint);
  SetOption(SmartTabsCheckBox,eoSmartTabs);
  SetOption(TabsToSpacesCheckBox,eoTabsToSpaces);
  SetOption(TrimTrailingSpacesCheckBox,eoTrimTrailingSpaces);
  {$ELSE}

  {$ENDIF}

  for a:=Low(PreviewEdits) to High(PreviewEdits) do begin
    if PreviewEdits[a]<>nil then begin
      {$IFDEF NEW_EDITOR_SYNEDIT}
      // general
      if Sender=UseSyntaxHighlightCheckBox then
        if UseSyntaxHighlightCheckBox.Checked then
          PreviewEdits[a].Highlighter:=PreviewPasSyn
        else
          PreviewEdits[a].Highlighter:=nil;
      // display
      if (a in [1,2]) then
        PreviewEdits[a].Gutter.Visible:=VisibleGutterCheckBox.Checked;
      PreviewEdits[a].Gutter.ShowLineNumbers:=ShowLineNumbersCheckBox.Checked;
      {$ELSE}

      {$ENDIF}
    end;
  end;
  if CurHighlightElement<>nil then begin
    if Sender=ForeGroundUseDefaultCheckBox then begin
      if not ForeGroundUseDefaultCheckBox.Checked then
        NewColor:=ForeGroundColorButton.Color
      else
        NewColor:=clNone;
      if NewColor<>CurHighlightElement.Foreground then begin
        CurHighlightElement.Foreground:=NewColor;
        InvalidatePreviews;
      end;
    end;
    if Sender=BackGroundUseDefaultCheckBox then begin
      if not BackGroundUseDefaultCheckBox.Checked then
        NewColor:=BackGroundColorButton.Color
      else
        NewColor:=clNone;
      if NewColor<>CurHighlightElement.Background then begin
        CurHighlightElement.Background:=NewColor;
        InvalidatePreviews;
      end;
    end;
    if Sender=TextBoldCheckBox then begin
      if TextBoldCheckBox.Checked then begin
        if not (fsBold in CurHighlightElement.Style) then begin
          CurHighlightElement.Style:=CurHighlightElement.Style+[fsBold];
          InvalidatePreviews;
        end;
      end else begin
        if (fsBold in CurHighlightElement.Style) then begin
          CurHighlightElement.Style:=CurHighlightElement.Style-[fsBold];
          InvalidatePreviews;
        end;
      end;
    end;
    if Sender=TextItalicCheckBox then begin
      if TextItalicCheckBox.Checked then begin
        if not (fsItalic in CurHighlightElement.Style) then begin
          CurHighlightElement.Style:=CurHighlightElement.Style+[fsItalic];
          InvalidatePreviews;
        end;
      end else begin
        if (fsItalic in CurHighlightElement.Style) then begin
          CurHighlightElement.Style:=CurHighlightElement.Style-[fsItalic];
          InvalidatePreviews;
        end;
      end;
    end;
    if Sender=TextUnderlineCheckBox then begin
      if TextUnderlineCheckBox.Checked then begin
        if not (fsUnderline in CurHighlightElement.Style) then begin
          CurHighlightElement.Style:=CurHighlightElement.Style+[fsUnderline];
          InvalidatePreviews;
        end;
      end else begin
        if (fsUnderline in CurHighlightElement.Style) then begin
          CurHighlightElement.Style:=CurHighlightElement.Style-[fsUnderline];
          InvalidatePreviews;
        end;
      end;
    end;
  end;
end;

procedure TEditorOptionsForm.ColorButtonColorChanged(Sender:TObject);
var a:integer;
begin
  if Sender=ForeGroundColorButton then begin
    if CurHighlightElement=nil then exit;
    if not ForeGroundUseDefaultCheckBox.Checked then begin
      CurHighlightElement.Foreground:=ForeGroundColorButton.ButtonColor;
      InvalidatePreviews;
    end;
  end;
  if Sender=BackGroundColorButton then begin
    if CurHighlightElement=nil then exit;
    if not BackGroundUseDefaultCheckBox.Checked then begin
      CurHighlightElement.Background:=BackGroundColorButton.ButtonColor;
      InvalidatePreviews;
    end;
  end;
  if Sender=GutterColorButton then begin
    for a:=Low(PreviewEdits) to High(PreviewEdits) do begin
      if PreviewEdits[a]<>nil then begin
        {$IFDEF NEW_EDITOR_SYNEDIT}
        PreviewEdits[a].Gutter.Color:=GutterColorButton.ButtonColor;
        PreviewEdits[a].Invalidate;
        {$ELSE}
        PreviewEdits[a].GutterColor:=GutterColorButton.ButtonColor;
        {$ENDIF}
      end;
    end;
  end;
end;

procedure TEditorOptionsForm.FontDialogNameToFont(FontDialogName:string;AFont:TFont);
var TmpFont:TFont;
  p,p2,index:integer;
  s:shortstring;
begin
  TmpFont:=TFont.Create;
  TmpFont.Assign(AFont);
  try
    p:=1;
    p2:=0;
    index:=1;
    while (p<=length(FontDialogName)) do begin
      if(FontDialogName[p]='-') then begin
        s:=copy(FontDialogName,p2+1,p-p2-1);
        p2:=p;
        case Index of
          3:TmpFont.Name:=s;
          //8:TmpFont.Height:=StrToIntDef(s,TmpFont.Height);
        end;
        inc(Index);
      end;
      inc(p);
    end;
    AFont.Assign(TmpFont);
  finally
    TmpFont.Free;
  end;
end;

procedure TEditorOptionsForm.EditorFontButtonClick(Sender:TObject);
var FontDialog:TFontDialog;
  a:integer;
begin
  FontDialog:=TFontDialog.Create(Application);
  try
    with FontDialog do begin
      if Execute then begin
        EditorFontComboBox.Text:=FontName;
writeln('[TEditorOptionsForm.EditorFontButtonClick] fontname=''',FontName,'''');
        for a:=Low(PreviewEdits) to High(PreviewEdits) do begin
          if PreviewEdits[a]<>nil then
            FontDialogNameToFont(FontName,PreviewEdits[a].Font);
        end;
        EditorFontComboBox.Text:=PreviewEdits[a].Font.Name;
      end;
    end;
  finally
    FontDialog.Free;
  end;
end;

procedure TEditorOptionsForm.ComboBoxOnExit(Sender:TObject);
var NewVal,a:integer;
begin
  if PreviewEdits[1]<>nil then begin
    // general
    if Sender=BlockIndentComboBox then begin
      NewVal:=StrToIntDef(BlockIndentComboBox.Text
        ,PreviewEdits[1].TabWidth);
      SetComboBoxText(BlockIndentComboBox,IntToStr(NewVal));
      for a:=Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a]<>nil then
          PreviewEdits[a].TabWidth:=NewVal;
    end;
    // display
    if Sender=EditorFontHeightComboBox then begin
      NewVal:=StrToIntDef(EditorFontHeightComboBox.Text
        ,PreviewEdits[1].Font.Height);
      if (NewVal<0) then
        if (NewVal>-6) then
          NewVal:=-6;
      if (NewVal>=0) then
        if (NewVal<6) then
          NewVal:=6;
      if (NewVal>40) then
        NewVal:=40;
      if (NewVal<-40) then
        NewVal:=-40;
      SetComboBoxText(EditorFontHeightComboBox,IntToStr(NewVal));
      for a:=Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a]<>nil then
          PreviewEdits[a].Font.Height:=NewVal;
    end;
    if Sender=ExtraLineSpacingComboBox then begin
      NewVal:=StrToIntDef(ExtraLineSpacingComboBox.Text
        ,PreviewEdits[1].ExtraLineSpacing);
      SetComboBoxText(ExtraLineSpacingComboBox,IntToStr(NewVal));
      for a:=Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a]<>nil then
          PreviewEdits[a].ExtraLineSpacing:=NewVal;
    end;
    if Sender=GutterWidthComboBox then begin
      NewVal:=StrToIntDef(GutterWidthComboBox.Text
        ,PreviewEdits[1].Gutter.Width);
      SetComboBoxText(GutterWidthComboBox,IntToStr(NewVal));
      for a:=Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a]<>nil then
          PreviewEdits[a].Gutter.Width:=NewVal;
    end;
    if Sender=RightMarginComboBox then begin
      NewVal:=StrToIntDef(RightMarginComboBox.Text
        ,PreviewEdits[1].RightEdge);
      SetComboBoxText(RightMarginComboBox,IntToStr(NewVal));
      for a:=Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a]<>nil then
          PreviewEdits[a].RightEdge:=NewVal;
    end;
  end;
end;

procedure TEditorOptionsForm.ComboBoxOnKeyDown(Sender: TObject;
   var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key=VK_S) then begin
    ComboBoxOnExit(Sender);
  end;
end;

procedure TEditorOptionsForm.ComboBoxOnChange(Sender:TObject);
var ComboBox:TComboBox;
begin
  ComboBox:=TComboBox(Sender);
  if ComboBox.Items.IndexOf(ComboBox.Text)>=0 then
    ComboBoxOnExit(Sender);
end;

procedure TEditorOptionsForm.FindCurHighlightElement;
var a:integer;
  Old:TSynHighlightElement;
begin
  Old:=CurHighlightElement;
  CurHighlightElement:=nil;
  a:=0;
  while a<ColorElementListBox.Items.Count do begin
    if ColorElementListBox.Selected[a] then begin
      case a of
        0:CurHighlightElement:=PreviewPasSyn.AsmAttri;
        1:CurHighlightElement:=PreviewPasSyn.CommentAttri;
        2:CurHighlightElement:=PreviewPasSyn.IdentifierAttri;
        3:CurHighlightElement:=PreviewPasSyn.KeyAttri;
        4:CurHighlightElement:=PreviewPasSyn.NumberAttri;
        5:CurHighlightElement:=PreviewPasSyn.SpaceAttri;
        6:CurHighlightElement:=PreviewPasSyn.StringAttri;
        7:CurHighlightElement:=PreviewPasSyn.SymbolAttri;
      else
        dec(a,8-Low(AdditionalHiglightAttributes));
        if a<=High(AdditionalHiglightAttributes) then
          CurHighlightElement:=AddHighlightElements[a];
      end;
      break;
    end;
    inc(a);
  end;
  if CurHighlightElement=nil then begin
    // none selected -> select one
    ColorElementListBox.Selected[0]:=true;
    CurHighlightElement:=PreviewPasSyn.AsmAttri;
  end;
  if Old<>CurHighlightElement then
    ShowCurAttribute;
  if CurHighlightElement<>nil then
    writeln('CurHigh: '+CurHighlightElement.Name);
end;

procedure TEditorOptionsForm.InvalidatePreviews;
var a:integer;
begin
  for a:=Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[a]<>nil then
      PreviewEdits[a].Invalidate;
end;

procedure TEditorOptionsForm.ShowCurAttribute;
begin
  if CurHighlightElement=nil then exit;
  TextBoldCheckBox.Checked:=fsBold in CurHighlightElement.Style;
  TextItalicCheckBox.Checked:=fsItalic in CurHighlightElement.Style;
  TextUnderlineCheckBox.Checked:=fsUnderline in CurHighlightElement.Style;
  if CurHighlightElement.Foreground=clNone then begin
    ForeGroundUseDefaultCheckBox.Checked:=true;
  end else begin
    ForeGroundUseDefaultCheckBox.Checked:=false;
    ForeGroundColorButton.ButtonColor:=CurHighlightElement.Foreground;
  end;
  if CurHighlightElement.Background=clNone then begin
    BackGroundUseDefaultCheckBox.Checked:=true;
  end else begin
    BackGroundUseDefaultCheckBox.Checked:=false;
    BackGroundColorButton.ButtonColor:=CurHighlightElement.Background;
  end;
end;

procedure TEditorOptionsForm.ColorElementListBoxMouseUp(Sender:TObject;
  Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
begin
  FindCurHighlightElement;
end;

procedure TEditorOptionsForm.ColorPreviewMouseUp(Sender:TObject;
  Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
var NewIndex:integer;
  AName,Token:ansistring;
  Attri:TSynHighlightElement;
  MouseXY,XY:TPoint;
begin
  MouseXY.x:=X;
  MouseXY.y:=Y;
  XY:=ColorPreview.PixelsToRowColumn(MouseXY);
  NewIndex:=ColorElementListBox.ItemIndex;
  case ColorPreview.CaretY of
    13:NewIndex:=8;
    14:NewIndex:=9;
    16:NewIndex:=10;
    17:NewIndex:=11;
    18:NewIndex:=12;
  else
    XY:=ColorPreview.CaretXY;
    ColorPreview.GetHighlighterAttriAtRowCol(XY,Token,Attri);
    if Attri<>nil then begin
      AName:=uppercase(Attri.Name);
      if AName='ASSEMBLER' then NewIndex:=0
      else if AName='COMMENT' then NewIndex:=1
      else if AName='IDENTIFIER' then NewIndex:=2
      else if AName='RESERVED WORD' then NewIndex:=3
      else if AName='IDENTIFIER' then NewIndex:=4
      else if AName='NUMBER' then NewIndex:=5
      else if AName='SPACE' then NewIndex:=6
      else if AName='STRING' then NewIndex:=7
      else if AName='SYMBOL' then NewIndex:=8;
    end;
  end;
  if NewIndex<>ColorElementListBox.ItemIndex then begin
    ColorElementListBox.ItemIndex:=NewIndex;
    FindCurHighlightElement;
    ShowCurAttribute;
  end;
end;

procedure TEditorOptionsForm.OnSpecialLineColors(Sender: TObject;
  Line: integer;  var Special: boolean; var FG, BG: TColor);
var e:TSynHighlightElement;
begin
  case Line of
    13:e:=EditorOpts.TextBlockElement;
    14:e:=EditorOpts.ExecutionPointElement;
    16:e:=EditorOpts.EnabledBreakPointElement;
    17:e:=EditorOpts.DisabledBreakPointElement;
    18:e:=EditorOpts.ErrorLineElement;
  else e:=nil;
  end;
  if e<>nil then begin
    Special:=true;
    FG:=e.ForeGround;
    BG:=e.BackGround;
  end;
end;

procedure TEditorOptionsForm.ShowCurCodeTemplate;
var i,sp,ep:integer;
  s:ansistring;
begin
  CodeTemplateCodePreview.Lines.Clear;
  i:=0;
  while i<CodeTemplateListBox.Items.Count do begin
    if CodeTemplateListBox.Selected[i] then begin
      s:=SynAutoComplete.CompletionValues[i];
      sp:=1;
      ep:=1;
      while ep<=length(s) do begin
        if s[ep] in [#10,#13] then begin
          CodeTemplateCodePreview.Lines.Add(copy(s,sp,ep-sp));
          while (ep<=length(s)) and (s[ep] in [#10,#13]) do inc(ep);
          sp:=ep;
        end else inc(ep);
      end;
      if ep>sp then
        CodeTemplateCodePreview.Lines.Add(copy(s,sp,ep-sp));
      break;
    end;
    inc(i);
  end;
  CodeTemplateCodePreview.Invalidate;
end;

procedure TEditorOptionsForm.FillCodeTemplateListBox;
var a:integer;
begin
  with CodeTemplateListBox do begin
    Items.BeginUpdate;
    Items.Clear;
    for a:=0 to SynAutoComplete.Completions.Count-1 do begin
      Items.Add(SynAutoComplete.Completions[a]
          +' - "'+SynAutoComplete.CompletionComments[a]+'"');
    end;
    Items.EndUpdate;
  end;
end;

procedure TEditorOptionsForm.CodeTemplateListBoxMouseUp(Sender:TObject;
  Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
begin
  ShowCurCodeTemplate;
end;

procedure TEditorOptionsForm.CodeTemplateButtonClick(Sender:TObject);
var Token,Comment:ansistring;
  Index:integer;
begin
  if Sender=CodeTemplateAddButton then begin
    Token:='new';
    Comment:='(custom)';
    if AddCodeTemplate(SynAutoComplete,Token,Comment)=mrOk then begin
      SynAutoComplete.AddCompletion(Token, '', Comment);
      FillCodeTemplateListBox;
      Index:=SynAutoComplete.Completions.IndexOf(Token);
      if Index>=0 then
        CodeTemplateListBox.Selected[Index]:=true;
      ShowCurCodeTemplate;
    end;
  end else if Sender=CodeTemplateEditButton then begin
    Index:=0;
    while Index<CodeTemplateListBox.Items.Count do begin
      if CodeTemplateListBox.Selected[Index] then break;
      inc(Index);
    end;
    if Index<CodeTemplateListBox.Items.Count then begin
      if EditCodeTemplate(SynAutoComplete,Index)=mrOk then begin
        CodeTemplateListBox.Items[Index]:=
           SynAutoComplete.Completions[Index]
           +' - "'+SynAutoComplete.CompletionComments[Index]+'"';
        ShowCurCodeTemplate;
      end;
    end;
  end else if Sender=CodeTemplateDeleteButton then begin
    // ToDo XXX

  end;
end;

procedure TEditorOptionsForm.CodeTemplateFileNameButtonClick(Sender:TObject);
var OpenDialog:TOpenDialog;
begin
  OpenDialog:=TOpenDialog.Create(Application);
  try
    with OpenDialog do begin
      Filter:='DCI file (*.dci)|*.dci';
      if Execute then
        CodeTemplateFileNameComboBox.Text:=FileName;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TEditorOptionsForm.SetComboBoxText(AComboBox:TComboBox;
  AText:AnsiString);
var a:integer;
begin
  a:=AComboBox.Items.IndexOf(AText);
  if a>=0 then
    AComboBox.ItemIndex:=a
  else begin
    AComboBox.Items.Add(AText);
    AComboBox.ItemIndex:=AComboBox.Items.IndexOf(AText);
  end;
end;

procedure TEditorOptionsForm.SetupGeneralPage;
var MaxX,ChkBoxW:integer;
begin
  MaxX:=Width-9;
  ChkBoxW:=(MaxX-20) div 2;

  EditorOptionsGroupBox:=TGroupBox.Create(Self);
  with EditorOptionsGroupBox do begin
    Name:='EditorOptionsGroupBox';
    Parent:=MainNoteBook.Page[0];
    Top:=5;
    Left:=5;
    Width:=MaxX-10;
    Height:=24*10;
    Caption:='Editor Options';
    Show;
  end;

  // many, many checkboxes ...

  AltSetsColumnModeCheckBox:=TCheckBox.Create(Self);
  with AltSetsColumnModeCheckBox do begin
    Name:='AltSetsColumnModeCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=5;
    Left:=5;
    Width:=ChkBoxW;
    Height:=16;
    Caption:='Alt Sets Column Mode';
    Checked:=eoAltSetsColumnMode in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  AutoIndentCheckBox:=TCheckBox.Create(Self);
  with AutoIndentCheckBox do begin
    Name:='AutoIndentCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=AltSetsColumnModeCheckBox.Top+AltSetsColumnModeCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Auto Indent';
    Checked:=eoAutoIndent in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  DragDropEditingCheckBox:=TCheckBox.Create(Self);
  with DragDropEditingCheckBox do begin
    Name:='DragDropEditingCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=AutoIndentCheckBox.Top+AutoIndentCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Drag Drop Editing';
    Checked:=eoDragDropEditing in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  DropFilesCheckBox:=TCheckBox.Create(Self);
  with DropFilesCheckBox do begin
    Name:='DropFilesCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=DragDropEditingCheckBox.Top+DragDropEditingCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Drop Files';
    Checked:=eoDropFiles in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  HalfPageScrollCheckBox:=TCheckBox.Create(Self);
  with HalfPageScrollCheckBox do begin
    Name:='HalfPageScrollCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=DropFilesCheckBox.Top+DropFilesCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Half Page Scroll';
    Checked:=eoHalfPageScroll in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  KeepCaretXCheckBox:=TCheckBox.Create(Self);
  with KeepCaretXCheckBox do begin
    Name:='KeepCaretXCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=HalfPageScrollCheckBox.Top+HalfPageScrollCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Keep Caret X';
    Checked:=eoKeepCaretX in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  NoCaretCheckBox:=TCheckBox.Create(Self);
  with NoCaretCheckBox do begin
    Name:='NoCaretCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=KeepCaretXCheckBox.Top+KeepCaretXCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='No Caret';
    Checked:=eoNoCaret in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  NoSelectionCheckBox:=TCheckBox.Create(Self);
  with NoSelectionCheckBox do begin
    Name:='NoSelectionCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=NoCaretCheckBox.Top+NoCaretCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='No Selection';
    Checked:=eoNoSelection in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  ScrollByOneLessCheckBox:=TCheckBox.Create(Self);
  with ScrollByOneLessCheckBox do begin
    Name:='ScrollByOneLessCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=NoSelectionCheckBox.Top+NoSelectionCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Scroll By One Less';
    Checked:=eoScrollByOneLess in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  ScrollPastEoFCheckBox:=TCheckBox.Create(Self);
  with ScrollPastEoFCheckBox do begin
    Name:='ScrollPastEoFCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=ScrollByOneLessCheckBox.Top+ScrollByOneLessCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Scroll Past End of File';
    Checked:=eoScrollPastEoF in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  ScrollPastEoLCheckBox:=TCheckBox.Create(Self);
  with ScrollPastEoLCheckBox do begin
    Name:='ScrollPastEoLCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=5;
    Left:=AltSetsColumnModeCheckBox.Left+(MaxX div 2)+5;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Scroll Past End of Line';
    Checked:=eoScrollPastEoL in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  ShowScrollHintCheckBox:=TCheckBox.Create(Self);
  with ShowScrollHintCheckBox do begin
    Name:='ShowScrollHintCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=ScrollPastEoLCheckBox.Top+ScrollPastEoLCheckBox.Height+5;
    Left:=ScrollPastEoLCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Show Scroll Hint';
    Checked:=eoShowScrollHint in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  SmartTabsCheckBox:=TCheckBox.Create(Self);
  with SmartTabsCheckBox do begin
    Name:='SmartTabsCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=ShowScrollHintCheckBox.Top+ShowScrollHintCheckBox.Height+5;
    Left:=ShowScrollHintCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Smart Tabs';
    Checked:=eoSmartTabs in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  TabsToSpacesCheckBox:=TCheckBox.Create(Self);
  with TabsToSpacesCheckBox do begin
    Name:='TabsToSpacesCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=SmartTabsCheckBox.Top+SmartTabsCheckBox.Height+5;
    Left:=ShowScrollHintCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Tabs To Spaces';
    Checked:=eoTabsToSpaces in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  TrimTrailingSpacesCheckBox:=TCheckBox.Create(Self);
  with TrimTrailingSpacesCheckBox do begin
    Name:='TrimTrailingSpacesCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=TabsToSpacesCheckBox.Top+TabsToSpacesCheckBox.Height+5;
    Left:=ShowScrollHintCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Trim Trailing Spaces';
    Checked:=eoTrimTrailingSpaces in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  UndoAfterSaveCheckBox:=TCheckBox.Create(Self);
  with UndoAfterSaveCheckBox do begin
    Name:='UndoAfterSaveCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=TrimTrailingSpacesCheckBox.Top+TrimTrailingSpacesCheckBox.Height+5;
    Left:=ShowScrollHintCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Undo after save';
    Checked:=EditorOpts.UndoAfterSave;
    OnClick:=@GeneralCheckBoxOnClick;
    Enabled:=false;
    Show;
  end;

  DoubleClickLineCheckBox:=TCheckBox.Create(Self);
  with DoubleClickLineCheckBox do begin
    Name:='DoubleClickLineCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=UndoAfterSaveCheckBox.Top+UndoAfterSaveCheckBox.Height+5;
    Left:=ShowScrollHintCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Double click line';
    Checked:=EditorOpts.DoubleClickLine;
    OnClick:=@GeneralCheckBoxOnClick;
    Enabled:=false;
    Show;
  end;

  FindTextAtCursorCheckBox:=TCheckBox.Create(Self);
  with FindTextAtCursorCheckBox do begin
    Name:='FindTextAtCursorCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=DoubleClickLineCheckBox.Top+DoubleClickLineCheckBox.Height+5;
    Left:=ShowScrollHintCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Find text at cursor';
    Checked:=EditorOpts.FindTextAtCursor;
    OnClick:=@GeneralCheckBoxOnClick;
    Enabled:=false;
    Show;
  end;

  UseSyntaxHighlightCheckBox:=TCheckBox.Create(Self);
  with UseSyntaxHighlightCheckBox do begin
    Name:='UseSyntaxHighlightCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=FindTextAtCursorCheckBox.Top+FindTextAtCursorCheckBox.Height+5;
    Left:=ShowScrollHintCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Use syntax highlight';
    Checked:=EditorOpts.UseSyntaxHighlight;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  CreateBackupFilesCheckBox:=TCheckBox.Create(Self);
  with CreateBackupFilesCheckBox do begin
    Name:='CreateBackupFilesCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=UseSyntaxHighlightCheckBox.Top+UseSyntaxHighlightCheckBox.Height+5;
    Left:=ShowScrollHintCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Create backup files';
    Checked:=EditorOpts.CreateBackupFiles;
    OnClick:=@GeneralCheckBoxOnClick;
    Enabled:=false;
    Show;
  end;

  // 

  BlockIndentComboBox:=TComboBox.Create(Self);
  with BlockIndentComboBox do begin
    Name:='BlockIndentComboBox';
    Parent:=MainNoteBook.Page[0];
    Top:=EditorOptionsGroupBox.Top+EditorOptionsGroupBox.Height+8;
    Left:=120;
    Width:=70;
    Items.BeginUpdate;
    Items.Add('1');
    Items.Add('2');
    Items.Add('4');
    Items.Add('8');
    Items.EndUpdate;
    SetComboBoxText(BlockIndentComboBox,IntToStr(EditorOpts.BlockIndent));
    OnChange:=@ComboBoxOnChange;
    OnKeyDown:=@ComboBoxOnKeyDown;
    OnExit:=@ComboBoxOnExit;
    Show;
  end;

  BlockIndentLabel:=TLabel.Create(Self);
  with BlockIndentLabel do begin
    Name:='BlockIndentLabel';
    Parent:=MainNoteBook.Page[0];
    Top:=BlockIndentComboBox.Top+2;
    Left:=EditorOptionsGroupBox.Left+2;
    Width:=BlockIndentComboBox.Left-2-Left;
    Caption:='Block indent:';
    Show;
  end;

  UndoLimitComboBox:=TComboBox.Create(Self);
  with UndoLimitComboBox do begin
    Name:='UndoLimitComboBox';
    Parent:=MainNoteBook.Page[0];
    Top:=BlockIndentComboBox.Top+BlockIndentComboBox.Height+5;
    Left:=BlockIndentComboBox.Left;
    Width:=70;
    Items.BeginUpdate;
    Items.Add('32767');
    Items.Add('4096');
    Items.Add('512');
    Items.EndUpdate;
    SetComboBoxText(UndoLimitComboBox,IntToStr(EditorOpts.UndoLimit));
    Enabled:=false;
    OnChange:=@ComboBoxOnChange;
    OnKeyDown:=@ComboBoxOnKeyDown;
    OnExit:=@ComboBoxOnExit;
    Show;
  end;

  UndoLimitLabel:=TLabel.Create(Self);
  with UndoLimitLabel do begin
    Name:='UndoLimitLabel';
    Parent:=MainNoteBook.Page[0];
    Top:=UndoLimitComboBox.Top+2;
    Left:=EditorOptionsGroupBox.Left+2;
    Width:=UndoLimitComboBox.Left-Left-2;
    Caption:='Undo limit:';
    Show;
  end;

  TabWidthsComboBox:=TComboBox.Create(Self);
  with TabWidthsComboBox do begin
    Name:='TabWidthsComboBox';
    Parent:=MainNoteBook.Page[0];
    Top:=UndoLimitComboBox.Top+UndoLimitComboBox.Height+5;
    Left:=BlockIndentComboBox.Left;
    Width:=70;
    Items.BeginUpdate;
    Items.Add('1');
    Items.Add('2');
    Items.Add('4');
    Items.Add('8');
    Items.EndUpdate;
    SetComboBoxText(TabWidthsComboBox,IntToStr(EditorOpts.TabWidths));
    Enabled:=false;
    OnChange:=@ComboBoxOnChange;
    OnKeyDown:=@ComboBoxOnKeyDown;
    OnExit:=@ComboBoxOnExit;
    Show;
  end;

  TabWidthsLabel:=TLabel.Create(Self);
  with TabWidthsLabel do begin
    Name:='TabWidthsLabel';
    Parent:=MainNoteBook.Page[0];
    Top:=TabWidthsComboBox.Top+2;
    Left:=EditorOptionsGroupBox.Left+2;
    Width:=TabWidthsComboBox.Left-Left-2;
    Caption:='Tab widths:';
    Show;
  end;

  SyntaxExtensionsComboBox:=TComboBox.Create(Self);
  with SyntaxExtensionsComboBox do begin
    Name:='SyntaxExtensionsComboBox';
    Parent:=MainNoteBook.Page[0];
    Top:=TabWidthsComboBox.Top+TabWidthsComboBox.Height+5;
    Left:=TabWidthsComboBox.Left;
    Width:=200;
    Items.BeginUpdate;
    Items.Add('pp;pas;inc;lfm;lrs;dpr;dpm;dpk');
    Items.Add('pp;pas;inc;dpr;dpm;dpk');
    Items.Add('pp;pas;inc');
    Items.EndUpdate;
    SetComboBoxText(SyntaxExtensionsComboBox,EditorOpts.SyntaxExtensions);
    OnChange:=@ComboBoxOnChange;
    OnKeyDown:=@ComboBoxOnKeyDown;
    OnExit:=@ComboBoxOnExit;
    Show;
  end;

  SyntaxExtensionsLabel:=TLabel.Create(Self);
  with SyntaxExtensionsLabel do begin
    Name:='SyntaxExtensionsLabel';
    Parent:=MainNoteBook.Page[0];
    Top:=SyntaxExtensionsComboBox.Top+2;
    Left:=EditorOptionsGroupBox.Left+2;
    Width:=SyntaxExtensionsComboBox.Left-Left-2;
    Caption:='Syntax extensions:';
    Show;
  end;
end;

procedure TEditorOptionsForm.SetupDisplayPage;
var MaxX,MaxY,ChkBoxW,a:integer;
begin
  MaxX:=Width-9;
  MaxY:=375;
  ChkBoxW:=140;

  MarginAndGutterGroupBox:=TGroupBox.Create(Self);
  with MarginAndGutterGroupBox do begin
    Name:='MarginAndGutterGroupBox';
    Parent:=MainNoteBook.Page[1];
    Top:=5;
    Left:=5;
    Width:=MaxX-10;
    Height:=105;
    Caption:='Margin and gutter';
    Show;
  end;

  VisibleRightMarginCheckBox:=TCheckBox.Create(Self);
  with VisibleRightMarginCheckBox do begin
    Name:='VisibleRightMarginCheckBox';
    Parent:=MarginAndGutterGroupBox;
    Top:=5;
    Left:=5;
    Width:=ChkBoxW;
    Caption:='Visible right margin';
    Checked:=EditorOpts.VisibleRightMargin;
    OnClick:=@GeneralCheckBoxOnClick;
    Enabled:=false;
    Show;
  end;

  VisibleGutterCheckBox:=TCheckBox.Create(Self);
  with VisibleGutterCheckBox do begin
    Name:='VisibleGutterCheckBox';
    Parent:=MarginAndGutterGroupBox;
    Top:=VisibleRightMarginCheckBox.Top+VisibleRightMarginCheckBox.Height+20;
    Left:=VisibleRightMarginCheckBox.Left;
    Width:=ChkBoxW;
    Height:=VisibleRightMarginCheckBox.Height;
    Caption:='Visible gutter';
    Checked:=EditorOpts.VisibleGutter;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  ShowLineNumbersCheckBox:=TCheckBox.Create(Self);
  with ShowLineNumbersCheckBox do begin
    Name:='ShowLineNumbersCheckBox';
    Parent:=MarginAndGutterGroupBox;
    Top:=VisibleGutterCheckBox.Top+VisibleGutterCheckBox.Height+20;
    Left:=VisibleGutterCheckBox.Left;
    Width:=ChkBoxW;
    Height:=VisibleRightMarginCheckBox.Height;
    Caption:='Show line numbers';
    Checked:=EditorOpts.ShowLineNumbers;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  RightMarginComboBox:=TComboBox.Create(Self);
  with RightMarginComboBox do begin
    Name:='RightMarginComboBox';
    Parent:=MarginAndGutterGroupBox;
    Top:=20;
    Left:=150;
    Width:=70;
    Items.BeginUpdate;
    Items.Add('80');
    Items.Add('78');
    Items.Add('76');
    Items.EndUpdate;
    SetComboBoxText(RightMarginComboBox,IntToStr(EditorOpts.RightMargin));
    OnChange:=@ComboBoxOnChange;
    OnKeyDown:=@ComboBoxOnKeyDown;
    OnExit:=@ComboBoxOnExit;
    Show;
  end;

  RightMarginLabel:=TLabel.Create(Self);
  with RightMarginLabel do begin
    Name:='RightMarginLabel';
    Parent:=MarginAndGutterGroupBox;
    Top:=2;
    Left:=RightMarginComboBox.Left+2;
    Width:=RightMarginComboBox.Width;
    Height:=16;
    Caption:='Right margin';
    Show;
  end;

  GutterWidthComboBox:=TComboBox.Create(Self);
  with GutterWidthComboBox do begin
    Name:='GutterWidthComboBox';
    Parent:=MarginAndGutterGroupBox;
    Top:=RightMarginComboBox.Top;
    Left:=RightMarginComboBox.Left+RightMarginComboBox.Width+50;
    Width:=RightMarginComboBox.Width;
    Height:=RightMarginComboBox.Height;
    Items.BeginUpdate;
    Items.Add('30');
    Items.Add('25');
    Items.Add('20');
    Items.EndUpdate;
    SetComboBoxText(GutterWidthComboBox,IntToStr(EditorOpts.GutterWidth));
    OnChange:=@ComboBoxOnChange;
    OnKeyDown:=@ComboBoxOnKeyDown;
    OnExit:=@ComboBoxOnExit;
    Show;
  end;

  GutterWidthLabel:=TLabel.Create(Self);
  with GutterWidthLabel do begin
    Name:='GutterWidthLabel';
    Parent:=MarginAndGutterGroupBox;
    Top:=2;
    Left:=GutterWidthComboBox.Left+2;
    Width:=GutterWidthComboBox.Width;
    Height:=16;
    Caption:='Gutter width';
    Show;
  end;

  GutterColorButton:=TColorButton.Create(Self);
  with GutterColorButton do begin
    Name:='GutterColorButton';
    Parent:=MarginAndGutterGroupBox;
    Top:=GutterWidthComboBox.Top+GutterWidthComboBox.Height+20;
    Left:=GutterWidthComboBox.Left;
    Width:=35;
    Height:=20;
    BorderWidth:=2;
    ButtonColor:=EditorOpts.GutterColor;
    OnColorChanged:=@ColorButtonColorChanged;
    Show;
  end;

  GutterColorLabel:=TLabel.Create(Self);
  with GutterColorLabel do begin
    Name:='GutterColorLabel';
    Parent:=MarginAndGutterGroupBox;
    Top:=GutterWidthComboBox.Top+GutterWidthComboBox.Height;
    Left:=GutterWidthComboBox.Left+2;
    Width:=100;
    Caption:='Gutter color';
    Show;
  end;

  EditorFontGroupBox:=TGroupBox.Create(Self);
  with EditorFontGroupBox do begin
    Name:='EditorFontGroupBox';
    Parent:=MainNoteBook.Page[1];
    Top:=MarginAndGutterGroupBox.Left+MarginAndGutterGroupBox.Height+5;
    Left:=MarginAndGutterGroupBox.Left;
    Width:=MarginAndGutterGroupBox.Width;
    Height:=120;
    Caption:='Default editor font';
    Show;
  end;

  EditorFontComboBox:=TComboBox.Create(Self);
  with EditorFontComboBox do begin
    Name:='EditorFontComboBox';
    Parent:=EditorFontGroupBox;
    Top:=23;
    Left:=5;
    Width:=EditorFontGroupBox.Width-15-Height;
    SetComboBoxText(EditorFontComboBox,EditorOpts.EditorFont);
    OnChange:=@ComboBoxOnChange;
    OnKeyDown:=@ComboBoxOnKeyDown;
    OnExit:=@ComboBoxOnExit;
    Show;
  end;

  EditorFontButton:=TButton.Create(Self);
  with EditorFontButton do begin
    Name:='EditorFontButton';
    Parent:=EditorFontGroupBox;
    Top:=EditorFontComboBox.Top+2;
    Left:=EditorFontComboBox.Left+EditorFontComboBox.Width+3;
    Width:=EditorFontComboBox.Height-5;
    Height:=Width;
    Caption:='...';
    OnClick:=@EditorFontButtonClick;
    Show;
  end;

  EditorFontLabel:=TLabel.Create(Self);
  with EditorFontLabel do begin
    Name:='EditorFontLabel';
    Parent:=EditorFontGroupBox;
    Top:=5;
    Left:=EditorFontComboBox.Left+2;
    Width:=EditorFontComboBox.Width;
    Caption:='Editor font';
    Show;
  end;

  EditorFontHeightComboBox:=TComboBox.Create(Self);
  with EditorFontHeightComboBox do begin
    Name:='EditorFontHeightComboBox';
    Parent:=EditorFontGroupBox;
    Top:=EditorFontComboBox.Top+EditorFontComboBox.Height+23;
    Left:=EditorFontComboBox.Left;
    Width:=60;
    Items.BeginUpdate;
    Items.Add('10');
    Items.Add('11');
    Items.Add('12');
    Items.Add('13');
    Items.Add('14');
    Items.Add('15');
    Items.EndUpdate;
    SetComboBoxText(EditorFontHeightComboBox
       ,IntToStr(EditorOpts.EditorFontHeight));
    OnChange:=@ComboBoxOnChange;
    OnKeyDown:=@ComboBoxOnKeyDown;
    OnExit:=@ComboBoxOnExit;
    Show;
  end;

  EditorFontHeightLabel:=TLabel.Create(Self);
  with EditorFontHeightLabel do begin
    Name:='EditorFontHeightLabel';
    Parent:=EditorFontGroupBox;
    Top:=EditorFontHeightComboBox.Top-18;
    Left:=EditorFontHeightComboBox.Left+2;
    Width:=100;
    Caption:='Editor font height';
    Show;
  end;

  ExtraLineSpacingComboBox:=TComboBox.Create(Self);
  with ExtraLineSpacingComboBox do begin
    Name:='ExtraLineSpacingComboBox';
    Parent:=EditorFontGroupBox;
    Top:=EditorFontHeightComboBox.Top;
    Left:=EditorFontHeightComboBox.Left+EditorFontHeightComboBox.Width+100;
    Width:=60;
    Items.BeginUpdate;
    Items.Add('0');
    Items.Add('1');
    Items.Add('2');
    Items.EndUpdate;
    SetComboBoxText(ExtraLineSpacingComboBox
      ,IntToStr(EditorOpts.ExtraLineSpacing));
    OnChange:=@ComboBoxOnChange;
    OnKeyDown:=@ComboBoxOnKeyDown;
    OnExit:=@ComboBoxOnExit;
    Show;
  end;

  ExtraLineSpacingLabel:=TLabel.Create(Self);
  with ExtraLineSpacingLabel do begin
    Name:='ExtraLineSpacingLabel';
    Parent:=EditorFontGroupBox;
    Top:=ExtraLineSpacingComboBox.Top-18;
    Left:=ExtraLineSpacingComboBox.Left+2;
    Width:=100;
    Caption:='Extra line spacing';
    Show;
  end;

  DisplayPreview:=TPreviewEditor.Create(Self);
  with DisplayPreview do begin
    Name:='DisplayPreview';
    Parent:=MainNoteBook.Page[1];
    BorderStyle:=bsSizeable;
    Top:=EditorFontGroupBox.Top+EditorFontGroupBox.Height+5;
    Left:=EditorFontGroupBox.Left+2;
    Width:=EditorFontGroupBox.Width-2;
    Height:=MaxY-Top-2;
    Lines.Clear;
    Lines.Add('{ Preview }');
    for a:=Low(ExampleSource) to High(ExampleSource) do
      Lines.Add(ExampleSource[a]);
    OnSpecialLineColors:=@Self.OnSpecialLineColors;
    ReadOnly:=true;
    Show;
  end;
end;

procedure TEditorOptionsForm.SetupKeyMappingsPage;
begin

end;

procedure TEditorOptionsForm.SetupColorPage;
var a,MaxX,MaxY:integer;
begin
  MaxX:=Width-9;
  MaxY:=375;

  ColorSchemeComboBox:=TComboBox.Create(Self);
  with ColorSchemeComboBox do begin
    Name:='ColorSchemeComboBox';
    Parent:=MainNoteBook.Page[3];
    Top:=5;
    Left:=100;
    Width:=100;
    Height:=16;
    Text:=EditorOpts.ColorScheme;
    Enabled:=false;
    Show;
  end;

  ColorSchemeLabel:=TLabel.Create(Self);
  with ColorSchemeLabel do begin
    Name:='ColorSchemeLabel';
    Parent:=MainNoteBook.Page[3];
    Top:=5;
    Left:=5;
    Width:=ColorSchemeComboBox.Left-Left;
    Height:=16;
    Caption:='Color Scheme';
    Show;
  end;

  ColorElementLabel:=TLabel.Create(Self);
  with ColorElementLabel do begin
    Name:='ColorElementLabel';
    Parent:=MainNoteBook.Page[3];
    Top:=ColorSchemeComboBox.Top+ColorSchemeComboBox.Height+12;
    Left:=5;
    Width:=150;
    Height:=16;
    Caption:='Element';
    Show;
  end;

  ColorElementListBox:=TListBox.Create(Self);
  with ColorElementListBox do begin
    Name:='ColorElementListBox';
    Parent:=MainNoteBook.Page[3];
    Top:=ColorElementLabel.Top+ColorElementLabel.Height+2;
    Left:=ColorElementLabel.Left;
    Width:=ColorElementLabel.Width;
    Height:=160;
    MultiSelect:=false;
    Items.BeginUpdate;
    Items.Add(PreviewPasSyn.AsmAttri.Name);
    Items.Add(PreviewPasSyn.CommentAttri.Name);
    Items.Add(PreviewPasSyn.IdentifierAttri.Name);
    Items.Add(PreviewPasSyn.KeyAttri.Name);
    Items.Add(PreviewPasSyn.NumberAttri.Name);
    Items.Add(PreviewPasSyn.SpaceAttri.Name);
    Items.Add(PreviewPasSyn.StringAttri.Name);
    Items.Add(PreviewPasSyn.SymbolAttri.Name);
    for a:=Low(AdditionalHiglightAttributes)
        to High(AdditionalHiglightAttributes) do
      Items.Add(AdditionalHiglightAttributes[a]);
    Items.EndUpdate;
    OnMouseUp:=@ColorElementListBoxMouseUp;
    Selected[0]:=true;
    Show;
  end;

  ForeGroundGroupBox:=TGroupBox.Create(Self);
  with ForeGroundGroupBox do begin
    Name:='ForeGroundGroupBox';
    Parent:=MainNoteBook.Page[3];
    Top:=ColorSchemeComboBox.Top+ColorSchemeComboBox.Height+12;
    Left:=ColorElementListBox.Left+ColorElementListBox.Width+12;
    Width:=MaxX-5-Left;
    Height:=60;
    Caption:='Foreground color';
    Show;
  end;

  ForeGroundColorButton:=TColorButton.Create(Self);
  with ForegroundColorButton do begin
    Name:='ForegroundColorButton';
    Parent:=ForeGroundGroupBox;
    BorderWidth:=2;
    Top:=2;
    Left:=5;
    Width:=70;
    Height:=20;
    Color:=clRed;
    OnColorChanged:=@ColorButtonColorChanged;
    Show;
  end;

  ForeGroundUseDefaultCheckBox:=TCheckBox.Create(Self);
  with ForeGroundUseDefaultCheckBox do begin
    Name:='ForeGroundUseDefaultCheckBox';
    Parent:=ForeGroundGroupBox;
    Top:=ForeGroundColorButton.Top+ForeGroundColorButton.Height+2;
    Left:=5;
    Width:=ForeGroundGroupBox.Width-Left-Left;
    Height:=16;
    Caption:='Use default for foreground color';
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  BackGroundGroupBox:=TGroupBox.Create(Self);
  with BackGroundGroupBox do begin
    Name:='BackGroundGroupBox';
    Parent:=MainNoteBook.Page[3];
    Top:=ForeGroundGroupBox.Top+ForeGroundGroupBox.Height+5;
    Left:=ForeGroundGroupBox.Left;
    Width:=ForeGroundGroupBox.Width;
    Height:=ForeGroundGroupBox.Height;
    Caption:='Background color';
    Show;
  end;

  BackGroundColorButton:=TColorButton.Create(Self);
  with BackgroundColorButton do begin
    Name:='BackgroundColorButton';
    Parent:=BackGroundGroupBox;
    BorderWidth:=2;
    Top:=2;
    Left:=5;
    Width:=70;
    Height:=20;
    Color:=clBlue;
    OnColorChanged:=@ColorButtonColorChanged;
    Show;
  end;

  BackGroundUseDefaultCheckBox:=TCheckBox.Create(Self);
  with BackGroundUseDefaultCheckBox do begin
    Name:='BackGroundUseDefaultCheckBox';
    Parent:=BackGroundGroupBox;
    Top:=BackGroundColorButton.Top+BackGroundColorButton.Height+2;
    Left:=5;
    Width:=ForeGroundGroupBox.Width-Left-Left;
    Height:=16;
    Caption:='Use default for background color';
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  TextAttributesGroupBox:=TGroupBox.Create(Self);
  with TextAttributesGroupBox do begin
    Name:='TextAttributesGroupBox';
    Parent:=MainNoteBook.Page[3];
    Top:=BackGroundGroupBox.Top+BackGroundGroupBox.Height+5;
    Left:=ForeGroundGroupBox.Left;
    Width:=ForeGroundGroupBox.Width;
    Height:=48;
    Caption:='Text attributes';
    Show;
  end;

  TextBoldCheckBox:=TCheckBox.Create(Self);
  with TextBoldCheckBox do begin
    Name:='TextBoldCheckBox';
    Parent:=TextAttributesGroupBox;
    Top:=5;
    Left:=5;
    Width:=50;
    Height:=16;
    Caption:='Bold';
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  TextItalicCheckBox:=TCheckBox.Create(Self);
  with TextItalicCheckBox do begin
    Name:='TextItalicCheckBox';
    Parent:=TextAttributesGroupBox;
    Top:=TextBoldCheckBox.Top;
    Left:=TextBoldCheckBox.Left+TextBoldCheckBox.Width+5;
    Width:=50;
    Height:=TextBoldCheckBox.Height;
    Caption:='Italic';
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  TextUnderlineCheckBox:=TCheckBox.Create(Self);
  with TextUnderlineCheckBox do begin
    Name:='TextUnderlineCheckBox';
    Parent:=TextAttributesGroupBox;
    Top:=TextBoldCheckBox.Top;
    Left:=TextItalicCheckBox.Left+TextItalicCheckBox.Width+5;
    Width:=75;
    Height:=TextItalicCheckBox.Height;
    Caption:='Underline';
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  ColorPreview:=TPreviewEditor.Create(Self);
  with ColorPreview do begin
    Name:='ColorPreview';
    Parent:=MainNoteBook.Page[3];
    Left:=5;
    Top:=TextAttributesGroupBox.Top+TextAttributesGroupBox.Height+7;
    Width:=MaxX-Left-Left;
    Height:=MaxY-Top-Left;
    Lines.Clear;
    Lines.Add('{ Syntax Highlighting }');
    for a:=Low(ExampleSource) to High(ExampleSource) do
      Lines.Add(ExampleSource[a]);
    OnSpecialLineColors:=@Self.OnSpecialLineColors;
    OnMouseUp:=@ColorPreviewMouseUp;
    ReadOnly:=true;
    Show;
  end; 
end;

procedure TEditorOptionsForm.SetupCodeInsightPage;
var MaxX:integer;
begin
  MaxX:=Width-9;

  AutomaticFeaturesGroupBox:=TGroupBox.Create(Self);
  with AutomaticFeaturesGroupBox do begin
    Name:='AutomaticFeaturesGroupBox';
    Parent:=MainNoteBook.Page[4];
    Top:=5;
    Left:=5;
    Width:=MaxX-Left-Left;
    Height:=110;
    Caption:='Automatic features';
    Show;
  end;

  AutoCodeCompletionCheckBox:=TCheckBox.Create(Self);
  with AutoCodeCompletionCheckBox do begin
    Name:='AutoCodeCompletionCheckBox';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=5;
    Left:=5;
    Width:=170;
    Caption:='Code completion';
    Checked:=EditorOpts.AutoCodeCompletion;
    Enabled:=false;
    Show;
  end;

  AutoCodeParametersCheckBox:=TCheckBox.Create(Self);
  with AutoCodeParametersCheckBox do begin
    Name:='AutoCodeParametersCheckBox';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=AutoCodeCompletionCheckBox.Top+AutoCodeCompletionCheckBox.Height+20;
    Left:=AutoCodeCompletionCheckBox.Left;
    Width:=AutoCodeCompletionCheckBox.Width;
    Height:=AutoCodeCompletionCheckBox.Height;
    Caption:='Code parameters';
    Checked:=EditorOpts.AutoCodeParameters;
    Enabled:=false;
    Show;
  end;

  AutoToolTipExprEvalCheckBox:=TCheckBox.Create(Self);
  with AutoToolTipExprEvalCheckBox do begin
    Name:='AutoToolTipExprEvalCheckBox';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=AutoCodeParametersCheckBox.Top+AutoCodeParametersCheckBox.Height+20;
    Left:=AutoCodeCompletionCheckBox.Left;
    Width:=AutoCodeCompletionCheckBox.Width;
    Height:=AutoCodeCompletionCheckBox.Height;
    Caption:='Tooltip expression evaluation';
    Checked:=EditorOpts.AutoToolTipExprEval;
    Enabled:=false;
    Show;
  end;

  AutoToolTipSymbInsightCheckBox:=TCheckBox.Create(Self);
  with AutoToolTipSymbInsightCheckBox do begin
    Name:='AutoToolTipSymbInsightCheckBox';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=AutoToolTipExprEvalCheckBox.Top+AutoToolTipExprEvalCheckBox.Height+20;
    Left:=AutoCodeCompletionCheckBox.Left;
    Width:=AutoCodeCompletionCheckBox.Width;
    Height:=AutoCodeCompletionCheckBox.Height;
    Caption:='Tooltip symbol insight';
    Checked:=EditorOpts.AutoToolTipSymbInsight;
    Enabled:=false;
    Show;
  end;

  AutoDelayLabel:=TLabel.Create(Self);
  with AutoDelayLabel do begin
    Name:='AutoDelayLabel';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=10;
    Left:=AutoCodeCompletionCheckBox.Left+AutoCodeCompletionCheckBox.Width+17;
    Width:=70;
    Caption:='Delay';
    Show;
  end;

  AutoDelayTrackBar:=TTrackBar.Create(Self);
  with AutoDelayTrackBar do begin
    Name:='AutoDelayTrackBar';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=32;
    Left:=AutoCodeCompletionCheckBox.Left+AutoCodeCompletionCheckBox.Width+15;
    Width:=150;
    Min:=2;
    Max:=6;
    Height:=10;
    Position:=EditorOpts.AutoDelayInMSec div 250;
    TickMarks:=tmBottomRight;
    Show;
  end;

  AutoDelayMinLabel:=TLabel.Create(Self);
  with AutoDelayMinLabel do begin
    Name:='AutoDelayMinLabel';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=AutoDelayTrackBar.Top+AutoDelayTrackBar.Height+5;
    Left:=AutoCodeCompletionCheckBox.Left+AutoCodeCompletionCheckBox.Width+15;
    Width:=70;
    Caption:='0.5 sec';
    Show;
  end;

  AutoDelayMaxLabel:=TLabel.Create(Self);
  with AutoDelayMaxLabel do begin
    Name:='AutoDelayMaxLabel';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=AutoDelayMinLabel.Top;
    Left:=AutoDelayTrackBar.Left+AutoDelayTrackBar.Width-30;
    Width:=70;
    Caption:='1.5 sec';
    Show;
  end;

  CodeTemplatesGroupBox:=TGroupBox.Create(Self);
  with CodeTemplatesGroupBox do begin
    Name:='CodeTemplatesGroupBox';
    Parent:=MainNoteBook.Page[4];
    Top:=AutomaticFeaturesGroupBox.Top+AutomaticFeaturesGroupBox.Height+5;
    Left:=AutomaticFeaturesGroupBox.Left;
    Width:=AutomaticFeaturesGroupBox.Width;
    Height:=250;
    Caption:='Code templates';
    Show;
  end;

  CodeTemplateFileNameLabel:=TLabel.Create(Self);
  with CodeTemplateFileNameLabel do begin
    Name:='CodeTemplateFileNameLabel';
    Parent:=CodeTemplatesGroupBox;
    Top:=5;
    Left:=7;
    Width:=110;
    Caption:='Template file name';
    Show;
  end;

  CodeTemplateFileNameComboBox:=TComboBox.Create(Self);
  with CodeTemplateFileNameComboBox do begin
    Name:='CodeTemplateFileNameComboBox';
    Parent:=CodeTemplatesGroupBox;
    Top:=3;
    Left:=CodeTemplateFileNameLabel.Left+CodeTemplateFileNameLabel.Width+2;
    Width:=CodeTemplatesGroupBox.Width-12-Left-Height;
    Text:=EditorOpts.CodeTemplateFileName;
    OnChange:=@ComboBoxOnChange;
    OnKeyDown:=@ComboBoxOnKeyDown;
    OnExit:=@ComboBoxOnExit;
    Show;
  end;

  CodeTemplateFileNameButton:=TButton.Create(Self);
  with CodeTemplateFileNameButton do begin
    Name:='CodeTemplateFileNameButton';
    Parent:=CodeTemplatesGroupBox;
    Top:=CodeTemplateFileNameComboBox.Top+2;
    Width:=CodeTemplateFileNameComboBox.Height-5;
    Left:=CodeTemplatesGroupBox.Width-9-Width;
    Height:=Width;
    Caption:='...';
    OnClick:=@CodeTemplateFileNameButtonClick;
    Show;
  end;

  CodeTemplatesLabel:=TLabel.Create(Self);
  with CodeTemplatesLabel do begin
    Name:='CodeTemplatesLabel';
    Parent:=CodeTemplatesGroupBox;
    Top:=CodeTemplateFileNameLabel.Top+CodeTemplateFileNameLabel.Height+12;
    Left:=CodeTemplateFileNameLabel.Left;
    Width:=60;
    Caption:='Templates';
    Show;
  end;

  CodeTemplateAddButton:=TButton.Create(Self);
  with CodeTemplateAddButton do begin
    Name:='CodeTemplateAddButton';
    Parent:=CodeTemplatesGroupBox;
    Top:=CodeTemplateFileNameComboBox.Top+CodeTemplateFileNameComboBox.Height+10;
    Width:=50;
    Left:=CodeTemplatesGroupBox.Width-Width-9;
    Height:=23;
    Caption:='Add...';
    OnClick:=@CodeTemplateButtonClick;
    Show;
  end;

  CodeTemplateEditButton:=TButton.Create(Self);
  with CodeTemplateEditButton do begin
    Name:='CodeTemplateEditButton';
    Parent:=CodeTemplatesGroupBox;
    Top:=CodeTemplateAddButton.Top+CodeTemplateAddButton.Height+5;
    Left:=CodeTemplateAddButton.Left;
    Width:=CodeTemplateAddButton.Width;
    Height:=CodeTemplateAddButton.Height;
    Caption:='Edit...';
    OnClick:=@CodeTemplateButtonClick;
    Show;
  end;

  CodeTemplateDeleteButton:=TButton.Create(Self);
  with CodeTemplateDeleteButton do begin
    Name:='CodeTemplateDeleteButton';
    Parent:=CodeTemplatesGroupBox;
    Top:=CodeTemplateEditButton.Top+CodeTemplateEditButton.Height+5;
    Left:=CodeTemplateAddButton.Left;
    Width:=CodeTemplateAddButton.Width;
    Height:=CodeTemplateAddButton.Height;
    Caption:='Delete';
    OnClick:=@CodeTemplateButtonClick;
    Show;
  end;

  CodeTemplateListBox:=TListBox.Create(Self);
  with CodeTemplateListBox do begin
    Name:='CodeTemplateListBox';
    Parent:=CodeTemplatesGroupBox;
    Top:=CodeTemplatesLabel.Top;
    Left:=CodeTemplatesLabel.Left+CodeTemplatesLabel.Width+5;
    Width:=CodeTemplateEditButton.Left-5-Left;
    Height:=80;
    OnMouseUp:=@CodeTemplateListBoxMouseUp;
    Show;
  end;

  CodeTemplateCodeLabel:=TLabel.Create(Self);
  with CodeTemplateCodeLabel do begin
    Name:='CodeTemplateCodeLabel';
    Parent:=CodeTemplatesGroupBox;
    Top:=CodeTemplateListBox.Top+CodeTemplateListBox.Height+5;
    Left:=CodeTemplatesLabel.Left;
    Width:=CodeTemplatesLabel.Width;
    Height:=CodeTemplatesLabel.Height;
    Caption:='Code';
    Show;
  end;

  CodeTemplateCodePreview:=TPreviewEditor.Create(Self);
  with CodeTemplateCodePreview do begin
    Name:='CodeTemplateCodePreview';
    Parent:=CodeTemplatesGroupBox;
    Top:=CodeTemplateCodeLabel.Top;
    Left:=CodeTemplateCodeLabel.Left+CodeTemplateCodeLabel.Width+5;
    Width:=CodeTemplateEditButton.Left-5-Left;
    Height:=CodeTemplatesGroupBox.ClientHeight-20-Top;
    Lines.Clear;
    Gutter.Visible:=false;
    Show;
  end; 
end;

procedure TEditorOptionsForm.SetupButtonBar;
begin
  CancelButton:=TButton.Create(Self);
  with CancelButton do begin
    Name:='CancelButton';
    Parent:=Self;
    Width:=70;
    Height:=23;
    Top:=Self.height-Height-15;
    Left:=Self.Width-Width-10;
    Caption:='Cancel';
    OnClick:=@CancelButtonClick;
    Show;
  end;

  OkButton:=TButton.Create(Self);
  with OkButton do begin
    Name:='OkButton';
    Parent:=Self;
    Width:=70;
    Height:=23;
    Top:=Self.height-Height-15;
    Left:=CancelButton.Left-10-Width;
    Caption:='Ok';
    OnClick:=@OkButtonClick;
    Show;
  end;
end;

procedure TEditorOptionsForm.OkButtonClick(Sender:TObject);
var AText,ACaption:AnsiString;
begin
  // save all values
  
  EditorOpts.SetHighlighterSettings(PreviewPasSyn);
  EditorOpts.SetSynEditSettings(PreviewEdits[1]);

  // general
  EditorOpts.UndoAfterSave:=UndoAfterSaveCheckBox.Checked;
  EditorOpts.DoubleClickLine:=DoubleClickLineCheckBox.Checked;
  EditorOpts.FindTextAtCursor:=FindTextAtCursorCheckBox.Checked;
  EditorOpts.UseSyntaxHighlight:=UseSyntaxHighlightCheckBox.Checked;
  EditorOpts.CreateBackupFiles:=CreateBackupFilesCheckBox.Checked;
  EditorOpts.SyntaxExtensions:=SyntaxExtensionsComboBox.Text;

  // code insight
  EditorOpts.AutoCodeCompletion:=AutoCodeCompletionCheckBox.Checked;
  EditorOpts.AutoCodeParameters:=AutoCodeParametersCheckBox.Checked;
  EditorOpts.AutoToolTipSymbInsight:=AutoToolTipSymbInsightCheckBox.Checked;
  EditorOpts.AutoToolTipExprEval:=AutoToolTipExprEvalCheckBox.Checked;
  EditorOpts.AutoDelayInMSec:=AutoDelayTrackBar.Position*250;
  EditorOpts.CodeTemplateFileName:=CodeTemplateFileNameComboBox.Text;

  EditorOpts.Save;

  try
 writeln('SAVING ',EditorOpts.CodeTemplateFileName);
    SynAutoComplete.AutoCompleteList.SaveToFile(
      EditorOpts.CodeTemplateFileName);
  except
    ACaption:='Error';
    AText:=' Unable to write code templates to file '''
      +EditorOpts.CodeTemplateFileName+'''! ';
    Application.MessageBox(PChar(AText),PChar(ACaption),0);
  end;
   

  ModalResult:=mrOk;
end;

procedure TEditorOptionsForm.CancelButtonClick(Sender:TObject);
begin
  ModalResult:=mrCancel;
end;

initialization
  EditorOpts:=TEditorOptions.Create;

finalization
  EditorOpts.Free;  EditorOpts:=nil;

end.

