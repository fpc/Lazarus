{
  Author: Mattias Gaertner

  Abstract:
    Editor options container and editor options dialog.
    The editor options are stored in XML format in the
     ~/.lazarus/editoroptions.xml file.
    Currently only for TSynEdit.

  ToDo:
   - Code template adding does not scroll listbox and the synedit is all white 
   - color schemes, key mapping schemes
   - Resizing
   - SetSynEditSettings
   - nicer TColorButton
   - create LFM file
}
unit editoroptions;

{$mode objfpc}{$H+}

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
  XMLCfg, CodeTemplateDialog, KeyMapping;

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
    fRightMarginColor:TColor;
    fEditorFont:Ansistring;
    fEditorFontHeight:integer;
    fExtraLineSpacing:integer;

    // Key Mappings options
    fKeyMappingScheme:AnsiString;
    fKeyMap:TKeyCommandRelationList;

    // Color options
    fColorScheme:Ansistring;
    fTextBlockElement:TSynHighlightElement;
    fExecutionPointElement:TSynHighlightElement;
    fEnabledBreakPointElement:TSynHighlightElement;
    fDisabledBreakPointElement:TSynHighlightElement;
    fErrorLineElement:TSynHighlightElement;

    // Code tools options
    fAutoCodeCompletion:boolean;
    fAutoCodeParameters:boolean;
    fAutoToolTipExprEval:boolean;
    fAutoToolTipSymbTools:boolean;
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
    property SynEditOptions:TSynEditorOptions
        read fSynEditOptions write fSynEditOptions
        default SYNEDIT_DEFAULT_OPTIONS;
    property UndoAfterSave:boolean
        read fUndoAfterSave write fUndoAfterSave default true;
    property DoubleClickLine:boolean
        read fDoubleClickLine write fDoubleClickLine default false;
    property FindTextAtCursor:boolean
        read fFindTextAtCursor write fFindTextAtCursor default true;
    property UseSyntaxHighlight:boolean
        read fUseSyntaxHighlight write fUseSyntaxHighlight default true;
    property BlockIndent:integer read fBlockIndent write fBlockIndent default 2;
    property UndoLimit:integer read fUndoLimit write fUndoLimit default 32767;
    property TabWidths:integer read fTabWidths write fTabWidths default 8;
    property SyntaxExtensions:Ansistring
        read fSyntaxExtensions write fSyntaxExtensions;

    // Display options
    property VisibleRightMargin:boolean
        read fVisibleRightMargin write fVisibleRightMargin default true;
    property VisibleGutter:boolean
        read fVisibleGutter write fVisibleGutter default true;
    property ShowLineNumbers:boolean
        read fShowLineNumbers write fShowLineNumbers default false;
    property GutterColor:TColor read fGutterColor write fGutterColor default clBtnFace;
    property GutterWidth:integer read fGutterWidth write fGutterWidth default 30;
    property RightMargin:integer read fRightMargin write fRightMargin default 80;
    property RightMarginColor:integer 
        read fRightMarginColor write fRightMarginColor default clBtnFace;
    property EditorFont:Ansistring read fEditorFont write fEditorFont;
    property EditorFontHeight:integer read fEditorFontHeight write FEditorFontHeight;
    property ExtraLineSpacing:integer
        read fExtraLineSpacing write fExtraLineSpacing default 0;

    // Key Mappings
    property KeyMappingScheme:Ansistring
       read fKeyMappingScheme write fKeyMappingScheme;
    property KeyMap:TKeyCommandRelationList read fKeyMap;

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

    // Code Tools options
    property AutoCodeCompletion:boolean
       read fAutoCodeCompletion write fAutoCodeCompletion default true;
    property AutoCodeParameters:boolean
       read fAutoCodeParameters write fAutoCodeParameters default true;
    property AutoToolTipExprEval:boolean
       read fAutoToolTipExprEval write fAutoToolTipExprEval default true;
    property AutoToolTipSymbTools:boolean
       read fAutoToolTipSymbTools write fAutoToolTipSymbTools default true;
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
    FBorderWidth:integer;
  protected
    procedure Paint; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
       X, Y: Integer); override;
    procedure SetButtonColor(Value:TColor);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; Override;
  published
    property BorderWidth:integer read FBorderWidth write FBorderWidth;
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
    BracketHighlightCheckBox: TCheckBox;
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
    RightMarginColorButton:TColorButton;
    RightMarginColorLabel:TLabel;
    EditorFontGroupBox:TGroupBox;
    EditorFontComboBox:TComboBox;
    EditorFontButton:TButton;
    EditorFontLabel:TLabel;
    EditorFontHeightLabel:TLabel;
    EditorFontHeightComboBox:TComboBox;
    ExtraLineSpacingLabel:TLabel;
    ExtraLineSpacingComboBox:TComboBox;
    DisplayPreview:TPreviewEditor;

    // Key Mappings
    KeyMappingSchemeLabel:TLabel;
    KeyMappingSchemeComboBox:TComboBox;
    KeyMappingHelpLabel:TLabel;
    KeyMappingListBox:TListBox;
    KeyMappingConsistencyCheckButton:TButton;

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

    // Code Tools options
    AutomaticFeaturesGroupBox:TGroupBox;
    AutoCodeCompletionCheckBox:TCheckBox;
    AutoCodeParametersCheckBox:TCheckBox;
    AutoToolTipExprEvalCheckBox:TCheckBox;
    AutoToolTipSymbToolsCheckBox:TCheckBox;
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
    SynAutoComplete:TSynEditAutoComplete;

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

    // key mapping
    procedure KeyMappingListBoxMouseUp(Sender:TObject;
       Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
    procedure KeyMappingConsistencyCheckButtonClick(Sender: TObject);

    // color
    procedure ColorElementListBoxMouseUp(Sender:TObject;
       Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
    procedure ColorPreviewMouseUp(Sender:TObject;
       Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
    procedure OnSpecialLineColors(Sender: TObject; Line: integer;
       var Special: boolean; var FG, BG: TColor);

    // code Tools
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
    CurCodeTemplate: integer;
    UpdatingColor: boolean;

    procedure SetupButtonBar;
    procedure SetupGeneralPage;
    procedure SetupDisplayPage;
    procedure SetupKeyMappingsPage;
    procedure SetupColorPage;
    procedure SetupCodeToolsPage;
    procedure SetComboBoxText(AComboBox:TComboBox;AText:AnsiString);
    procedure FillCodeTemplateListBox;
    function KeyMappingRelationToString(Index:integer):AnsiString;
    procedure FillKeyMappingListBox;

    procedure ShowCurAttribute;
    procedure FindCurHighlightElement;
    procedure FontDialogNameToFont(FontDialogName:Ansistring;AFont:TFont);
    procedure InvalidatePreviews;
    procedure ShowCurCodeTemplate;
    procedure SaveCurCodeTemplate;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  TLazSyntaxHighlighter =
    (lshNone, lshText, lshFreePascal, lshDelphi, lshLFM, lshXML);

const
  LazSyntaxHighlighterNames : array[TLazSyntaxHighlighter] of string = (
     'None',
     'Text',
     'FreePascal',
     'Delphi',
     'LFM',
     'XML'
   );

var
  EditorOptionsForm: TEditorOptionsForm;
  EditorOpts: TEditorOptions;

function ShowEditorOptionsDialog:TModalResult;
function StrToLazSyntaxHighlighter(const s: string): TLazSyntaxHighlighter;
function ExtensionToLazSyntaxHighlighter(Ext:string): TLazSyntaxHighlighter;


implementation


uses math;

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

function StrToLazSyntaxHighlighter(const s: string): TLazSyntaxHighlighter;
begin
  for Result:=Low(TLazSyntaxHighlighter) to High(TLazSyntaxHighlighter) do
    if (lowercase(s)=lowercase(LazSyntaxHighlighterNames[Result])) then exit;
  Result:=lshFreePascal;
end;

function ExtensionToLazSyntaxHighlighter(Ext: string): TLazSyntaxHighlighter;
var s,CurExt: string;
  StartPos,EndPos:integer;
begin
  if (Ext='') then begin
    Result:=lshNone;
    exit;
  end;
  Ext:=lowercase(Ext);
  if (Ext[1]='.') then Ext:=copy(Ext,2,length(Ext)-1);
  s:=EditorOpts.SyntaxExtensions;
  StartPos:=1;
  while StartPos<=length(s) do begin
    Endpos:=StartPos;
    while (EndPos<=length(s)) and (s[EndPos]<>';') do inc(EndPos);
    CurExt:=copy(s,Startpos,EndPos-StartPos);
    if (CurExt<>'') and (CurExt[1]='.') then begin
      CurExt:=copy(CurExt,2,length(CurExt)-1);
    end;
    if lowercase(CurExt)=Ext then begin
      Result:=lshFreePascal;
      if (Ext='.dpr') or (Ext='.dpk') or (Ext='.dfm') then Result:=lshDelphi;
      exit;
    end;
    Startpos:=EndPos+1;
  end;
  if Ext='.xml' then Result:=lshXML
  else if Ext='.lfm' then Result:=lshLFM
  else if Ext='.txt' then Result:=lshText
  else Result:=lshNone;
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


function BuildBorlandDCIFile(
  ACustomSynAutoComplete: TCustomSynAutoComplete):boolean;
// returns if something has changed
var sl: TStringList;
  i, sp, ep: integer;
  Token, Comment, Value: string;
begin
  Result:=false;
  sl:=TStringList.Create;
  try
    for i:=0 to ACustomSynAutoComplete.Completions.Count-1 do begin
      Token:=ACustomSynAutoComplete.Completions[i];
      Comment:=ACustomSynAutoComplete.CompletionComments[i];
      Value:=ACustomSynAutoComplete.CompletionValues[i];
      sl.Add('['+Token+' | '+Comment+']');
      sp:=1;
      ep:=1;
      while ep<=length(Value) do begin
        if Value[ep] in [#10,#13] then begin
          sl.Add(copy(Value,sp,ep-sp));
          inc(ep);
          if (ep<=length(Value)) and (Value[ep] in [#10,#13]) 
          and (Value[ep]<>Value[ep-1]) then inc(ep);
          sp:=ep;
        end else inc(ep);
      end;
      if ep>sp then
        sl.Add(copy(Value,sp,ep-sp));
    end;
    if ACustomSynAutoComplete.AutoCompleteList.Equals(sl)=false then begin
      Result:=true;
      ACustomSynAutoComplete.AutoCompleteList:=sl;
    end;
  finally
    sl.Free;
  end;
end;


{ TEditorOptions }

constructor TEditorOptions.Create;
var ConfFileName: string;
  fs:TFileStream;
  res:TLResource;
begin
  inherited Create;
  ConfFileName:=SetDirSeparators(GetPrimaryConfigPath+'/'+EditOptsConfFileName);
  CopySecondaryConfigFile(EditOptsConfFileName);
  if (not FileExists(ConfFileName)) then begin
    writeln('editor options config file not found');
  end;
  XMLConfig:=TXMLConfig.Create(ConfFileName);

  // set defaults

  // General options
  fSyntaxExtensions:='pp;pas;inc;lpr;lrs;dpr;dpk';

  // Display options
  fEditorFont:='courier';

  // Key Mappings
  fKeyMappingScheme:='default';
  fKeyMap:=TKeyCommandRelationList.Create;

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

  // Code Tools options
  fCodeTemplateFileName:=SetDirSeparators(GetPrimaryConfigPath+'/lazarus.dci');
  CopySecondaryConfigFile('lazarus.dci');
  if not FileExists(fCodeTemplateFileName) then begin
    res:=LazarusResources.Find('lazarus_dci_file');
  if (res<>nil) and (res.Value<>'') and (res.ValueType='DCI') then begin
    try
      fs:=TFileStream.Create(fCodeTemplateFileName,fmCreate);
      try
        fs.Write(res.Value[1],length(res.Value));
      finally
        fs.Free;
      end;
    except
    end;
  end;
  end;
end;

destructor TEditorOptions.Destroy;
begin
  fTextBlockElement.Free;
  fExecutionPointElement.Free;
  fEnabledBreakPointElement.Free;
  fDisabledBreakPointElement.Free;
  fErrorLineElement.Free;

  fKeyMap.Free;

  XMLConfig.Free;
  inherited Destroy;
end;

procedure TEditorOptions.Load;
// load options from XML file
var SynEditOpt:TSynEditorOption;
  SynEditOptName:ansistring;
begin
  try
    // general options
    for SynEditOpt:=Low(TSynEditorOption) to High(TSynEditorOption) do begin
      case SynEditOpt of
        eoAltSetsColumnMode:SynEditOptName:='AltSetsColumnMode';
        eoAutoIndent:SynEditOptName:='AutoIndent';
        eoBracketHighlight:SynEditOptName:='BracketHighlight';
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
    fDoubleClickLine:=
      XMLConfig.GetValue('EditorOptions/General/Editor/DoubleClickLine',false);
    fFindTextAtCursor:=
      XMLConfig.GetValue('EditorOptions/General/Editor/FindTextAtCursor',true);
    fUseSyntaxHighlight:=
      XMLConfig.GetValue('EditorOptions/General/Editor/UseSyntaxHighlight',true);
    fBlockIndent:=
      XMLConfig.GetValue('EditorOptions/General/Editor/BlockIndent',2);
    fUndoLimit:=
      XMLConfig.GetValue('EditorOptions/General/Editor/UndoLimit',32767);
    fTabWidths:=
      XMLConfig.GetValue('EditorOptions/General/Editor/TabWidths',8);
    fSyntaxExtensions:=
      XMLConfig.GetValue('EditorOptions/General/Editor/SyntaxExtensions'
        ,'pp;inc;lfc;pas;dpr;dpk');

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
    fRightMarginColor:=
      XMLConfig.GetValue('EditorOptions/Display/VisibleRightMarginColor'
     ,clBtnFace);
    fEditorFont:=
      XMLConfig.GetValue('EditorOptions/Display/EditorFont','courier');
    fEditorFontHeight:=
      XMLConfig.GetValue('EditorOptions/Display/EditorFontHeight',12);
    fExtraLineSpacing:=
      XMLConfig.GetValue('EditorOptions/Display/ExtraLineSpacing',1);

    // Key Mappings options
    fKeyMappingScheme:=
      XMLConfig.GetValue('EditorOptions/KeyMapping/Scheme',fKeyMappingScheme);
    fKeyMap.LoadFromXMLConfig(XMLConfig
      ,'EditorOptions/KeyMapping/'+fKeyMappingScheme+'/');

    // Color options
    fColorScheme:=
      XMLConfig.GetValue('EditorOptions/Color/ColorScheme','Default');
    ReadAttribute(fTextBlockElement);
    ReadAttribute(fExecutionPointElement);
    ReadAttribute(fEnabledBreakPointElement);
    ReadAttribute(fDisabledBreakPointElement);
    ReadAttribute(fErrorLineElement);

    // Code Tools options
    fAutoCodeCompletion:=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoCodeCompletion',true);
    fAutoCodeParameters:=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoCodeParameters',true);
    fAutoToolTipExprEval:=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoToolTipExprEval',true);
    fAutoToolTipSymbTools:=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoToolTipSymbTools',true);
    fAutoDelayInMSec:=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoDelayInMSec',1000);
    fCodeTemplateFileName:=
      XMLConfig.GetValue('EditorOptions/CodeTools/CodeTemplateFileName'
        ,SetDirSeparators(GetPrimaryConfigPath+'/lazarus.dci'));
  except
    writeln('[TEditorOptions.Load] ERROR');
  end;
end;

procedure TEditorOptions.Save;
// save options to XML file
var SynEditOpt:TSynEditorOption;
  SynEditOptName:ansistring;
begin
  try
    // general options
    for SynEditOpt:=Low(TSynEditorOption) to High(TSynEditorOption) do begin
      case SynEditOpt of
        eoAltSetsColumnMode:SynEditOptName:='AltSetsColumnMode';
        eoAutoIndent:SynEditOptName:='AutoIndent';
        eoBracketHighlight:SynEditOptName:='BracketHighlight';
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
    XMLConfig.SetValue('EditorOptions/Display/RightMarginColor',fRightMarginColor);
    XMLConfig.SetValue('EditorOptions/Display/EditorFont',fEditorFont);
    XMLConfig.SetValue('EditorOptions/Display/EditorFontHeight'
      ,fEditorFontHeight);
    XMLConfig.SetValue('EditorOptions/Display/ExtraLineSpacing'
      ,fExtraLineSpacing);

    // Key Mappings options
    XMLConfig.SetValue('EditorOptions/KeyMapping/Scheme',fKeyMappingScheme);
    fKeyMap.SaveToXMLConfig(
       XMLConfig,'EditorOptions/KeyMapping/'+fKeyMappingScheme+'/');

    // Color options
    XMLConfig.SetValue('EditorOptions/Color/ColorScheme',fColorScheme);
    WriteAttribute(fTextBlockElement);
    WriteAttribute(fExecutionPointElement);
    WriteAttribute(fEnabledBreakPointElement);
    WriteAttribute(fDisabledBreakPointElement);
    WriteAttribute(fErrorLineElement);

    // Code Tools options
    XMLConfig.SetValue('EditorOptions/CodeTools/AutoCodeCompletion'
      ,fAutoCodeCompletion);
    XMLConfig.SetValue('EditorOptions/CodeTools/AutoCodeParameters'
      ,fAutoCodeParameters);
    XMLConfig.SetValue('EditorOptions/CodeTools/AutoToolTipExprEval'
      ,fAutoToolTipExprEval);
    XMLConfig.SetValue('EditorOptions/CodeTools/AutoToolTipSymbTools'
      ,fAutoToolTipSymbTools);
    XMLConfig.SetValue('EditorOptions/CodeTools/AutoDelayInMSec'
      ,fAutoDelayInMSec);
    XMLConfig.SetValue('EditorOptions/CodeTools/CodeTemplateFileName'
      ,fCodeTemplateFileName);

    XMLConfig.Flush;
  except
    writeln('[TEditorOptions.Save] ERROR: unable to write xml file');
  end;
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
  if ColorScheme ='Twilight'  then begin
    // default for twilight color scheme
    DefBGCol:=clBlack;
    DefFGCol:=clWhite;
    if AttriName='Assembler' then begin
      DefFGCol:=clLime;
    end else if AttriName='Comment' then begin
      DefFGCol:=clGray;
    end else if AttriName='Reserved word' then begin
      DefFGCol:=clAqua;
      DefFontStyles:=[fsBold];
    end else if AttriName='Number' then begin
      DefFGCol:=clFuchsia;
    end else if AttriName='String' then begin
      DefFGCol:=clYellow;
    end else if AttriName='Symbol' then begin
      DefFGCol:=clAqua;
    end else if AttriName=AdditionalHiglightAttributes[0] then begin
      DefBGCol:=clWhite;
      DefFGCol:=clBlack
    end else if AttriName=AdditionalHiglightAttributes[1] then begin
      DefBGCol:=clBlue;
      DefFGCol:=clWhite;
    end else if AttriName=AdditionalHiglightAttributes[2] then begin
      DefBGCol:=clRed;
      DefFGCol:=clWhite;
    end else if AttriName=AdditionalHiglightAttributes[3] then begin
      DefBGCol:=clLime;
      DefFGCol:=clRed;
    end else if AttriName=AdditionalHiglightAttributes[4] then begin
      DefBGCol:=$50a0ff;
      DefFGCol:=clBlack;
    end;
  end else begin
    // default for all other color schemes
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
  ASynEdit.TabWidth:=fBlockIndent;

  // Display options
  ASynEdit.Gutter.Visible:=fVisibleGutter;
  ASynEdit.Gutter.ShowLineNumbers:=fShowLineNumbers;
  ASynEdit.Gutter.Color:=fGutterColor;
  ASynEdit.Gutter.Width:=fGutterWidth;
  ASynEdit.RightEdge:=fRightMargin;
  ASynEdit.RightEdgeColor:=fRightMarginColor;
  ASynEdit.Font.Name:=fEditorFont;
  ASynEdit.Font.Height:=fEditorFontHeight;
  ASynEdit.ExtraLineSpacing:=fExtraLineSpacing;
  ASynEdit.MaxUndo:=fUndoLimit;
  ASynEdit.SelectedColor.ForeGround:=fTextBlockElement.ForeGround;
  ASynEdit.SelectedColor.BackGround:=fTextBlockElement.BackGround;
  KeyMap.AssignTo(ASynEdit.KeyStrokes);
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
  fUndoLimit:=ASynEdit.MaxUndo;
  fTextBlockElement.ForeGround:=ASynEdit.SelectedColor.ForeGround;
  fTextBlockElement.BackGround:=ASynEdit.SelectedColor.BackGround;

  // XXX: KeyMap

  // XXX:  update all checkboxes, comboboxes...
end;


{ TColorButton }

constructor TColorButton.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  Align := alNone;
  FBorderWidth:=2;
  Setbounds(1,1,75,25);
end;

destructor TColorButton.Destroy;
Begin
  inherited Destroy;
end;

procedure TColorButton.Paint;
var a: integer;
begin
  //inherited Paint;
  with Canvas do begin
    Brush.Color:=ButtonColor;
    FillRect(Bounds(0, 0, Width, Height));
    Pen.Color:=clWhite;
    for a:=0 to FBorderWidth-1 do begin
      MoveTo(a,Height-a);
      LineTo(a,a);
      LineTo(Width-a,a);
    end;
    Pen.Color:=clBlack;
    for a:=0 to FBorderWidth-1 do begin
      MoveTo(Width-a-1,a);
      LineTo(Width-a-1,Height-a-1);
      MoveTo(a,Height-a-1);
      LineTo(Width-a,Height-a-1);
    end;
  end;
end;

procedure TColorButton.SetButtonColor(Value:TColor);
begin
  if Value=FButtonColor then exit;
  FButtonColor:=Value;
  if Assigned(FOnColorChanged) then
    FOnColorChanged(Self);
  Invalidate;
end;

procedure TColorButton.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var NewColor:TColor;
begin
  if FColorDialog<>nil then exit;
  if not Enabled then exit;
  NewColor:=ButtonColor;
  FColorDialog:=TColorDialog.Create(Application);
  try
    FColorDialog.Color:=ButtonColor;
    if FColorDialog.Execute then begin
      NewColor:=FColorDialog.Color;
    end;
  finally
    FColorDialog.Free;
    FColorDialog:=nil;
  end;
  ButtonColor:=NewColor;
  Invalidate;
end;

{ TEditorOptionsForm }

constructor TEditorOptionsForm.Create(AOwner:TComponent);
var a:integer;
  s:Ansistring;
begin
  inherited Create(AOwner);

  if LazarusResources.Find(ClassName)=nil then begin  
    SetBounds((Screen.Width-470) div 2,(Screen.Height-480) div 2, 455,459);
    Caption:='Editor Options';
    PreviewPasSyn:=TPreviewPasSyn.Create(Self);
    SynAutoComplete:=TSynEditAutoComplete.Create(Self);

    MainNoteBook:=TNoteBook.Create(Self);
    with MainNoteBook do begin
      Parent:=Self;
      Top:=0;
      Left:=0;
      Width:=Self.Width;
      Height:=Self.Height-50;
      Pages.Strings[0]:='General';
      Pages.Add('Display');
      Pages.Add('Key Mappings');
      Pages.Add('Color');
      Pages.Add('Code Tools');
    end;

    SetupGeneralPage;
    SetupDisplayPage;
    SetupKeyMappingsPage;
    SetupColorPage;
    SetupCodeToolsPage;

    MainNoteBook.Show;

    SetupButtonBar;
  end;

  UpdatingColor:=false;

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
      EditorOpts.KeyMap.AssignTo(PreviewEdits[a].KeyStrokes);
    end;
  end;
  CodeTemplateCodePreview.Gutter.Visible:=false;

  // general options
  // display options
  // key mappings
  // color options
  // code Tools options

  FindCurHighlightElement;
  FillCodeTemplateListBox;
  FillKeyMappingListBox;
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
  SetOption(BracketHighlightCheckBox,eoBracketHighlight);
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
      if UpdatingColor=false then begin
        if not ForeGroundUseDefaultCheckBox.Checked then
          NewColor:=ForeGroundColorButton.Color
        else
          NewColor:=clNone;
        if NewColor<>CurHighlightElement.Foreground then begin
          CurHighlightElement.Foreground:=NewColor;
          InvalidatePreviews;
        end;
      end;
    end;
    if Sender=BackGroundUseDefaultCheckBox then begin
      if UpdatingColor=false then begin
        if not BackGroundUseDefaultCheckBox.Checked then
          NewColor:=BackGroundColorButton.Color
        else
          NewColor:=clNone;
        if NewColor<>CurHighlightElement.Background then begin
          CurHighlightElement.Background:=NewColor;
          InvalidatePreviews;
        end;
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
    if (CurHighlightElement=nil) or UpdatingColor then exit;
    if not ForeGroundUseDefaultCheckBox.Checked then begin
      CurHighlightElement.Foreground:=ForeGroundColorButton.ButtonColor;
      InvalidatePreviews;
    end;
  end;
  if Sender=BackGroundColorButton then begin
    if (CurHighlightElement=nil) or UpdatingColor then exit;
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
  if Sender=RightMarginColorButton then begin
    for a:=Low(PreviewEdits) to High(PreviewEdits) do begin
      if PreviewEdits[a]<>nil then begin
        {$IFDEF NEW_EDITOR_SYNEDIT}
        PreviewEdits[a].RightEdgeColor:=RightMarginColorButton.ButtonColor;
        PreviewEdits[a].Invalidate;
        {$ELSE}

        {$ENDIF}
      end;
    end;
  end;
end;

procedure TEditorOptionsForm.FontDialogNameToFont(FontDialogName:Ansistring;AFont:TFont);
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
  Box: TComboBox;
begin
  Box:=TComboBox(Sender);
  if PreviewEdits[1]<>nil then begin
    // general
    if Sender=BlockIndentComboBox then begin
      NewVal:=StrToIntDef(BlockIndentComboBox.Text
        ,PreviewEdits[1].TabWidth);
      SetComboBoxText(BlockIndentComboBox,IntToStr(NewVal));
      for a:=Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a]<>nil then
          PreviewEdits[a].TabWidth:=NewVal;
    end
    // display
    else if Sender=EditorFontHeightComboBox then begin
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
    end
    else if Sender=ExtraLineSpacingComboBox then begin
      NewVal:=StrToIntDef(ExtraLineSpacingComboBox.Text
        ,PreviewEdits[1].ExtraLineSpacing);
      SetComboBoxText(ExtraLineSpacingComboBox,IntToStr(NewVal));
      for a:=Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a]<>nil then
          PreviewEdits[a].ExtraLineSpacing:=NewVal;
    end
    else if Sender=GutterWidthComboBox then begin
      NewVal:=StrToIntDef(GutterWidthComboBox.Text
        ,PreviewEdits[1].Gutter.Width);
      SetComboBoxText(GutterWidthComboBox,IntToStr(NewVal));
      for a:=Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a]<>nil then
          PreviewEdits[a].Gutter.Width:=NewVal;
    end
    else if Sender=RightMarginComboBox then begin
      NewVal:=StrToIntDef(RightMarginComboBox.Text
        ,PreviewEdits[1].RightEdge);
      SetComboBoxText(RightMarginComboBox,IntToStr(NewVal));
      for a:=Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a]<>nil then
          PreviewEdits[a].RightEdge:=NewVal;
    end
    // color
    else if Sender=ColorSchemeComboBox then begin
      if Box.Items.IndexOf(Box.Text)<0 then begin
        SetComboBoxText(Box,EditorOpts.ColorScheme);
      end else begin
        if Box.Text<>EditorOpts.ColorScheme then begin
          EditorOpts.ColorScheme:=Box.Text;
          SetComboBoxText(Box,EditorOpts.ColorScheme);
          EditorOpts.ReadAttribute(EditorOpts.TextBlockElement);
          EditorOpts.ReadAttribute(EditorOpts.ExecutionPointElement);
          EditorOpts.ReadAttribute(EditorOpts.EnabledBreakPointElement);
          EditorOpts.ReadAttribute(EditorOpts.DisabledBreakPointElement);
          EditorOpts.ReadAttribute(EditorOpts.ErrorLineElement);
          EditorOpts.GetHighlighterSettings(PreviewPasSyn);
          ShowCurAttribute;
        end;
      end;
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
  if (CurHighlightElement=nil) or UpdatingColor then exit;
  UpdatingColor:=true;
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
  UpdatingColor:=false;
end;

procedure TEditorOptionsForm.KeyMappingListBoxMouseUp(Sender:TObject;
  Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
var a:integer;
begin
  if Button=mbRight then begin
    a:=KeyMappingListBox.Items.Count-1;
    while (a>=0) and (KeyMappingListBox.Selected[a]=false) do
      dec(a);
    if a>=0 then begin
      if ShowKeyMappingEditForm(a,EditorOpts.KeyMap)=mrOk then begin
        // There is a bug in ListBox
        //KeyMappingListBox.Items[a]:=KeyMappingRelationToString(a);
        // workaround:
        FillKeyMappingListBox;
        for a:=Low(PreviewEdits) to High(PreviewEdits) do
          if PreviewEdits[a]<>nil then
            EditorOpts.KeyMap.AssignTo(PreviewEdits[a].KeyStrokes);
      end;
    end;
  end;
end;

type
  TKeyMapErrorsForm = class(TForm)
    ListBox: TListBox;
    BackButton: TButton;
    procedure BackButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TKeyMapErrorsForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    SetBounds((Screen.Width-410) div 2,(Screen.Height-260) div 2, 400,250);
    Caption:='Key mapping errors';

    ListBox:=TListBox.Create(Self);
    with ListBox do begin
      Name:='ListBox';
      Parent:=Self;
      Left:=0;
      Top:=0;
      Width:=Self.ClientWidth-4;
      Height:=Self.ClientHeight-50;
      Show;
    end;

    BackButton:=TButton.Create(Self);
    with BackButton do begin
      Name:='BackButton';
      Parent:=Self;
      Width:=60;
      Height:=25;
      Caption:='Back';
      Left:=((Self.ClientWidth-4)-Width) div 2;
      Top:=Self.ClientHeight-38;
      OnClick:=@BackButtonClick;
      Show;
    end;
  end;
end;

procedure TKeyMapErrorsForm.BackButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TEditorOptionsForm.KeyMappingConsistencyCheckButtonClick(
  Sender: TObject);
var Protocol:TStringList;
  ErrorCount, Index1, Index2:integer;
  ACaption,AText:AnsiString;
  KeyMapErrorsForm:TKeyMapErrorsForm;
begin
  Protocol:=TStringList.Create;
  try
    ErrorCount:=KeyStrokesConsistencyErrors(DisplayPreview.KeyStrokes
        ,Protocol, Index1, Index2);
    if ErrorCount>0 then begin
      KeyMapErrorsForm:=TKeyMapErrorsForm.Create(Application);
      try
        KeyMapErrorsForm.ListBox.Items.Assign(Protocol);
        KeyMapErrorsForm.ShowModal;
      finally
        KeyMapErrorsForm.Free;
      end;
    end else begin
      ACaption:='Report';
      AText:='No errors in key mapping found.';

//      Application.MessageBox(PChar(AText),PChar(ACaption),mb_ok);
      MessageDlg(ACaption,AText,mtinformation,[mbok],0);

    end;
  finally
    Protocol.Free;
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
  MouseXY:=Point(X,Y);
  XY:=ColorPreview.PixelsToRowColumn(MouseXY);
  NewIndex:=ColorElementListBox.ItemIndex;
  case XY.Y of
    13:NewIndex:=8;
    14:NewIndex:=9;
    16:NewIndex:=10;
    17:NewIndex:=11;
    18:NewIndex:=12;
  else
    ColorPreview.GetHighlighterAttriAtRowCol(XY,Token,Attri);
    if Attri<>nil then begin
      AName:=uppercase(Attri.Name);
      if AName='ASSEMBLER' then NewIndex:=0
      else if AName='COMMENT' then NewIndex:=1
      else if AName='IDENTIFIER' then NewIndex:=2
      else if AName='RESERVED WORD' then NewIndex:=3
      else if AName='NUMBER' then NewIndex:=4
      else if AName='SPACE' then NewIndex:=5
      else if AName='STRING' then NewIndex:=6
      else if AName='SYMBOL' then NewIndex:=7;
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

function TEditorOptionsForm.KeyMappingRelationToString(
  Index:integer):AnsiString;
var s:AnsiString;
begin
  with EditorOpts.KeyMap.Relations[Index] do begin
    Result:=copy(Name,1,37);
    SetLength(s,(37-length(Result))*2);
    FillChar(s[1],length(s),'.');
    Result:=Result+s;
    if (Key1=VK_UNKNOWN) and (Key2=VK_UNKNOWN) then
      Result:=Result+'none'
    else if (Key2=VK_UNKNOWN) then
      Result:=Result+KeyAndShiftStateToStr(Key1,Shift1)
    else
      Result:=Result+KeyAndShiftStateToStr(Key1,Shift1)+'  or  '+
           KeyAndShiftStateToStr(Key2,Shift2);
    end;
end;

procedure TEditorOptionsForm.FillKeyMappingListBox;
var a:integer;
begin
  with KeyMappingListBox.Items do begin
    BeginUpdate;
    Clear;
    for a:=0 to EditorOpts.KeyMap.Count-1 do
      Add(KeyMappingRelationToString(a));
    EndUpdate;
  end;
end;

procedure TEditorOptionsForm.ShowCurCodeTemplate;
var i,sp,ep:integer;
  s:ansistring;
begin
  CodeTemplateCodePreview.Lines.BeginUpdate;
  CodeTemplateCodePreview.Lines.Clear;
  i:=0;
  while i<CodeTemplateListBox.Items.Count do begin
    if CodeTemplateListBox.Selected[i] then begin
      CurCodeTemplate:=i;
      s:=SynAutoComplete.CompletionValues[i];
      sp:=1;
      ep:=1;
      while ep<=length(s) do begin
        if s[ep] in [#10,#13] then begin
          CodeTemplateCodePreview.Lines.Add(copy(s,sp,ep-sp));
          inc(ep);
          if (ep<=length(s)) and (s[ep] in [#10,#13]) and (s[ep-1]<>s[ep]) then
            inc(ep);
          sp:=ep;
        end else inc(ep);
      end;
      if ep>sp then
        CodeTemplateCodePreview.Lines.Add(copy(s,sp,ep-sp));
      break;
    end;
    inc(i);
  end;
  CodeTemplateCodePreview.Lines.EndUpdate;
  CodeTemplateCodePreview.Invalidate;
end;

procedure TEditorOptionsForm.SaveCurCodeTemplate;
var
  NewValue: string;
begin
  if CurCodeTemplate<0 then exit;
  NewValue:=CodeTemplateCodePreview.Lines.Text;
  if NewValue<>'' then begin
    if copy(NewValue,length(NewValue)-1,2)=#10#13 then
      NewValue:=copy(NewValue,1,length(NewValue)-2)
    else if NewValue[length(NewValue)] in [#10,#13] then
      NewValue:=copy(NewValue,1,length(NewValue)-1);
  end;
  SynAutoComplete.CompletionValues[CurCodeTemplate]:=NewValue;
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
  SaveCurCodeTemplate;
  ShowCurCodeTemplate;
end;

procedure TEditorOptionsForm.CodeTemplateButtonClick(Sender:TObject);
var Token,Comment:ansistring;
  Index:integer;
begin
  SaveCurCodeTemplate;
  if Sender=CodeTemplateAddButton then begin
    Token:='new';
    Comment:='(custom)';
    CurCodeTemplate:=-1;
    if AddCodeTemplate(SynAutoComplete,Token,Comment)=mrOk then begin
      SynAutoComplete.AddCompletion(Token, '', Comment);
      FillCodeTemplateListBox;
      Index:=SynAutoComplete.Completions.IndexOf(Token);
      if Index>=0 then begin
        CodeTemplateListBox.Selected[Index]:=true;
        CodeTemplateListBox.ItemIndex:=Index;
      end;
      ShowCurCodeTemplate;
    end;
  end else if Sender=CodeTemplateEditButton then begin
    Index:=CurCodeTemplate;
    if Index<CodeTemplateListBox.Items.Count then begin
      if EditCodeTemplate(SynAutoComplete,Index)=mrOk then begin
        CodeTemplateListBox.Items[Index]:=
           SynAutoComplete.Completions[Index]
           +' - "'+SynAutoComplete.CompletionComments[Index]+'"';
        ShowCurCodeTemplate;
      end;
    end;
  end else if Sender=CodeTemplateDeleteButton then begin
    if CurCodeTemplate>=0 then begin
      if MessageDlg('Delete template '
          +'"'+SynAutoComplete.Completions[CurCodeTemplate]+' - '
          +SynAutoComplete.CompletionComments[CurCodeTemplate]+'"'
          +'?',mtConfirmation,[mbOk,mbCancel],0)=mrOK then begin
        SynAutoComplete.DeleteCompletion(CurCodeTemplate);
        dec(CurCodeTemplate);
        FillCodeTemplateListBox;
        if CurCodeTemplate>=0 then begin
          CodeTemplateListBox.Selected[CurCodeTemplate]:=true;
          CodeTemplateListBox.ItemIndex:=CurCodeTemplate;
        end;
        ShowCurCodeTemplate;
      end;
    end;
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

  BracketHighlightCheckBox:=TCheckBox.Create(Self);
  with BracketHighlightCheckBox do begin
    Name:='BracketHighlightCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=AutoIndentCheckBox.Top+AutoIndentCheckBox.Height+5;
    Left:=AutoIndentCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Bracket Highlight';
    Checked:=eoBracketHighlight in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  DragDropEditingCheckBox:=TCheckBox.Create(Self);
  with DragDropEditingCheckBox do begin
    Name:='DragDropEditingCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=BracketHighlightCheckBox.Top+BracketHighlightCheckBox.Height+5;
    Left:=BracketHighlightCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:='Drag Drop Editing';
    Checked:=eoDragDropEditing in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Enabled:=false;
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
    Top:=5;
    Left:=AltSetsColumnModeCheckBox.Left+(MaxX div 2)+5;
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
    Top:=ScrollPastEoFCheckBox.Top+ScrollPastEoFCheckBox.Height+5;
    Left:=ScrollPastEoFCheckBox.Left;
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
    Width:=300;
    Items.BeginUpdate;
    Items.Add('pp;pas;inc;lpr;lfm;lrs;dpr;dfm;dpk');
    Items.Add('pp;pas;inc;lpr;lrs;dpr;dpk');
    Items.Add('pp;pas;inc;lpr;lrs');
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
    Height:=109;
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
    Left:=180;
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

  RightMarginColorButton:=TColorButton.Create(Self);
  with RightMarginColorButton do begin
    Name:='RightMarginColorButton';
    Parent:=MarginAndGutterGroupBox;
    Top:=RightMarginComboBox.Top+RightMarginComboBox.Height+20;
    Left:=RightMarginComboBox.Left;
    Width:=35;
    Height:=20;
    BorderWidth:=2;
    ButtonColor:=EditorOpts.RightMarginColor;
    OnColorChanged:=@ColorButtonColorChanged;
    Show;
  end;

  RightMarginColorLabel:=TLabel.Create(Self);
  with RightMarginColorLabel do begin
    Name:='RightMarginColorLabel';
    Parent:=MarginAndGutterGroupBox;
    Top:=RightMarginComboBox.Top+RightMarginComboBox.Height;
    Left:=RightMarginComboBox.Left+2;
    Width:=120;
    Caption:='Right margin color';
    Show;
  end;

  GutterWidthComboBox:=TComboBox.Create(Self);
  with GutterWidthComboBox do begin
    Name:='GutterWidthComboBox';
    Parent:=MarginAndGutterGroupBox;
    Top:=RightMarginComboBox.Top;
    Left:=RightMarginComboBox.Left+RightMarginComboBox.Width+80;
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
    BorderStyle:=bsSingle;
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
var MaxX,MaxY:integer;
begin
  MaxX:=Width-9;
  MaxY:=374;

  KeyMappingSchemeComboBox:=TComboBox.Create(Self);
  with KeyMappingSchemeComboBox do begin
    Name:='KeyMappingSchemeComboBox';
    Parent:=MainNoteBook.Page[2];
    Top:=5;
    Left:=170;
    Width:=100;
    Height:=16;
    Text:=EditorOpts.KeyMappingScheme;
    Enabled:=false;
    Show;
  end;

  KeyMappingSchemeLabel:=TLabel.Create(Self);
  with KeyMappingSchemeLabel do begin
    Name:='KeyMappingSchemeLabel';
    Parent:=MainNoteBook.Page[2];
    Top:=5;
    Left:=5;
    Width:=KeyMappingSchemeComboBox.Left-Left;
    Height:=16;
    Caption:='Key Mapping Scheme';
    Show;
  end;

  KeyMappingConsistencyCheckButton:=TButton.Create(Self);
  with KeyMappingConsistencyCheckButton do begin
    Name:='KeyMappingConsistencyCheckButton';
    Parent:=MainNoteBook.Page[2];
    Top:=5;
    Left:=Max(KeyMappingSchemeComboBox.Left+KeyMappingSchemeComboBox.Width
            ,MaxX-150);
    Width:=130;
    Height:=23;
    Caption:='Check consistency';
    OnClick:=@KeyMappingConsistencyCheckButtonClick;
    Show;
  end;

  KeyMappingHelpLabel:=TLabel.Create(Self);
  with KeyMappingHelpLabel do begin
    Name:='KeyMappingHelpLabel';
    Parent:=MainNoteBook.Page[2];
    Top:=KeyMappingSchemeComboBox.Top+KeepCaretXCheckBox.Height+10;
    Left:=5;
    Width:=MaxX-Left-Left;
    Height:=16;
    Caption:='Hint: right click on the command you want to edit';
    Show;
  end;

  KeyMappingListBox:=TListBox.Create(Self);
  with KeyMappingListBox do begin
    Name:='KeyMappingListBox';
    Parent:=MainNoteBook.Page[2];
    Top:=KeyMappingHelpLabel.Top+KeyMappingHelpLabel.Height+2;
    Left:=0;
    Width:=MaxX-Left-Left;
    Height:=MaxY-Top;
    OnMouseUp:=@KeyMappingListBoxMouseUp;
    Show;
  end;
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
    with Items do begin
      BeginUpdate;
      Add('Default');
      Add('Twilight');
      EndUpdate;
    end;
    SetComboBoxText(ColorSchemeComboBox,EditorOpts.ColorScheme);
    OnChange:=@ComboBoxOnChange;
    OnKeyDown:=@ComboBoxOnKeyDown;
    OnExit:=@ComboBoxOnExit;
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
    OnMouseDown:=@ColorPreviewMouseUp;
    ReadOnly:=true;
    Show;
  end; 
end;

procedure TEditorOptionsForm.SetupCodeToolsPage;
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
    Width:=200;
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

  AutoToolTipSymbToolsCheckBox:=TCheckBox.Create(Self);
  with AutoToolTipSymbToolsCheckBox do begin
    Name:='AutoToolTipSymbToolsCheckBox';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=AutoToolTipExprEvalCheckBox.Top+AutoToolTipExprEvalCheckBox.Height+20;
    Left:=AutoCodeCompletionCheckBox.Left;
    Width:=AutoCodeCompletionCheckBox.Width;
    Height:=AutoCodeCompletionCheckBox.Height;
    Caption:='Tooltip symbol Tools';
    Checked:=EditorOpts.AutoToolTipSymbTools;
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

  CurCodeTemplate:=-1;
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
var res: TModalResult;
begin
  // save all values
  SaveCurCodeTemplate;

  EditorOpts.SetHighlighterSettings(PreviewPasSyn);
  EditorOpts.SetSynEditSettings(PreviewEdits[1]);

  // general
  EditorOpts.UndoAfterSave:=UndoAfterSaveCheckBox.Checked;
  EditorOpts.DoubleClickLine:=DoubleClickLineCheckBox.Checked;
  EditorOpts.FindTextAtCursor:=FindTextAtCursorCheckBox.Checked;
  EditorOpts.UseSyntaxHighlight:=UseSyntaxHighlightCheckBox.Checked;
  EditorOpts.SyntaxExtensions:=SyntaxExtensionsComboBox.Text;

  // code Tools
  EditorOpts.AutoCodeCompletion:=AutoCodeCompletionCheckBox.Checked;
  EditorOpts.AutoCodeParameters:=AutoCodeParametersCheckBox.Checked;
  EditorOpts.AutoToolTipSymbTools:=AutoToolTipSymbToolsCheckBox.Checked;
  EditorOpts.AutoToolTipExprEval:=AutoToolTipExprEvalCheckBox.Checked;
  EditorOpts.AutoDelayInMSec:=AutoDelayTrackBar.Position*250;
  EditorOpts.CodeTemplateFileName:=CodeTemplateFileNameComboBox.Text;

  EditorOpts.Save;

  if BuildBorlandDCIFile(SynAutoComplete) then begin
    Res:=mrOk;
    repeat
      try
        SynAutoComplete.AutoCompleteList.SaveToFile(
          EditorOpts.CodeTemplateFileName);
      except
        res:=MessageDlg(' Unable to write code templates to file '''
          +EditorOpts.CodeTemplateFileName+'''! ',mtError
          ,[mbAbort, mbIgnore, mbRetry],0);
        if res=mrAbort then exit;
      end;
    until Res<>mrRetry;
  end;

  ModalResult:=mrOk;
end;

procedure TEditorOptionsForm.CancelButtonClick(Sender:TObject);
begin
  EditorOpts.Load;
  ModalResult:=mrCancel;
end;

//=============================================================================

initialization

{$I lazarus_dci.lrs}

end.

