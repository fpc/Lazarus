{
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
 
  Author: Mattias Gaertner

  Abstract:
    Editor options container and editor options dialog.
    The editor options are stored in XML format in the
     ~/.lazarus/editoroptions.xml file.
    Currently only for TSynEdit.

  ToDo:
   - Code template adding does not scroll listbox
   - key mapping schemes
   - SetSynEditSettings
   - nicer TColorButton
}
unit EditorOptions;

{$mode objfpc}{$H+}

interface

{$define NEW_EDITOR_SYNEDIT}

uses
  LCLLinux, LCLType,
  Forms, Classes, SysUtils, ComCtrls, Buttons, StdCtrls, ExtCtrls, LazConf,
  FileCtrl, GraphType, Graphics, Controls, Dialogs, LResources, IDEProcs,
{$ifdef NEW_EDITOR_SYNEDIT}
  SynEdit, SynEditHighlighter, SynEditAutoComplete, SynEditKeyCmds,
  SynHighlighterPas, SynHighlighterHTML, SynHighlighterCPP, SynHighlighterXML,
  SynHighlighterLFM, SynHighlighterPerl,
{$else}
  mwCustomEdit, mwPasSyn, mwHighlighter,
{$endif}
  Laz_XMLCfg, CodeTemplateDialog, KeyMapping, InputHistory, IDEOptionDefs, LazarusIDEStrConsts;

type
{$ifdef NEW_EDITOR_SYNEDIT}
  TPreviewEditor = TSynEdit;
  TPreviewPasSyn = TSynPasSyn;
  TCustomSyn = TSynCustomHighlighter;
  TSynHighlightElement = TSynHighlighterAttributes;
{$else}
  TPreviewEditor = TmwCustomEdit;
  TPreviewPasSyn = TmwPasSyn;
  TCustomSyn = TmwCustomHighlighter;
  TSynHighlightElement = TmwHighlightAttributes;
{$endif}
  TCustomSynClass = class of TCustomSyn;

  TLazSyntaxHighlighter =
    (lshNone, lshText, lshFreePascal, lshDelphi, lshLFM, lshXML, lshHTML,
     lshCPP, lshPerl);

  TAdditionalHilightAttribute = (ahaNone, ahaTextBlock, ahaExecutionPoint,
    ahaEnabledBreakpoint, ahaDisabledBreakpoint, ahaInvalidBreakpoint, ahaErrorLine);

const
  EditorOptsFormatVersion = 2;
  
  AdditionalHighlightAttributes : array[TAdditionalHilightAttribute] of string =
  (
    '',
    'Text block',
    'Execution point',
    'Enabled breakpoint','Disabled breakpoint','Invalid breakpoint',
    'Error line'
  );
  
  LazSyntaxHighlighterClasses: array[TLazSyntaxHighlighter] of TCustomSynClass =
    ( nil, nil, TSynPasSyn, TSynPasSyn, TSynLFMSyn, TSynXMLSyn, TSynHTMLSyn,
      TSynCPPSyn, TSynPerlSyn);
    

  { Comments }
const
  DefaultCommentTypes: array[TLazSyntaxHighlighter] of TCommentType = (
      comtNone,  // lshNone
      comtNone,  // lshText
      comtPascal,// lshFreePascal
      comtPascal,// lshDelphi
      comtDelphi,// lshLFM
      comtHtml,  // lshXML
      comtHtml,  // lshHTML
      comtCPP,   // lshCPP
      comtPerl   // lshPerl
    );

type
  { TEditOptLanguageInfo stores lazarus IDE additional information
    of a highlighter, such as samplesource, which sample lines are special
    lines, file extensions
    MappedAttributes is a list of the format "AttributName=PascalAttributName"
      This mapping attributes are used for default values. For example:
      The comment attribute of HTML is mapped to the comment attribute of
      pascal "Comment=Comment". If there is no mapping attribute for an
      attribute the default values are taken from an untouched highlighter.
      For example Symbol in HTML is not mapped and therefore has as default
      value fo style [fsBold] as defined in synhighlighterhtml.pp.
    }
  TEditOptLanguageInfo = class
  public
    SynClass: TCustomSynClass;
    TheType: TLazSyntaxHighlighter;
    FileExtensions: string; // divided by semicolon, e.g. 'pas;pp;inc'
    ColorScheme: string;
    SampleSource: string;
    AddAttrSampleLines: array[TAdditionalHilightAttribute] of integer; // first line = 1
    MappedAttributes: TStringList; // map attributes to pascal
    DefaultCommentType: TCommentType;
    constructor Create;
    destructor Destroy; override;
    function GetDefaultFilextension: string;
    function SampleLineToAddAttr(Line: integer): TAdditionalHilightAttribute;
  end;

  { list of TEditOptLanguageInfo }
  TEditOptLangList = class(TList)
  private
    function GetInfos(Index: integer): TEditOptLanguageInfo;
  public
    constructor Create;
    procedure Clear; override;
    destructor Destroy; override;
    function FindByName(const Name: string): integer;
    function FindByClass(CustomSynClass: TCustomSynClass): integer;
    function FindByHighlighter(Hilighter: TSynCustomHighlighter): integer;
    function FindByType(AType: TLazSyntaxHighlighter): integer;
    function GetDefaultFilextension(AType: TLazSyntaxHighlighter): string;
    property Items[Index: integer]: TEditOptLanguageInfo read GetInfos; default;
  end;


  { Editor Options object used to hold the editor options }
  TEditorOptions = class(TPersistent)
  private
    xmlconfig:TXMLConfig;

    // general options
    fFindTextAtCursor:boolean;
    fShowTabCloseButtons: boolean;
    fSynEditOptions: TSynEditorOptions;
    fUndoAfterSave:boolean;
    fUseSyntaxHighlight:boolean;
    fBlockIndent:integer;
    fUndoLimit:integer;
    fTabWidth:integer;

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
    fHighlighterList: TEditOptLangList;

    // Code tools options (MG: these will move to an unit of their own)
    fAutoIdentifierCompletion:boolean;
    fAutoCodeParameters:boolean;
    fAutoToolTipExprEval:boolean;
    fAutoToolTipSymbTools:boolean;
    fAutoDelayInMSec:integer;
    fCodeTemplateFileName:Ansistring;
    fCTemplIndentToTokenStart: boolean;
  public
    constructor Create;
    destructor Destroy;  override;
    procedure Load;
    procedure Save;
    
    procedure GetHighlighterSettings(Syn: TCustomSyn);// read highlight settings from config file
    procedure SetHighlighterSettings(Syn: TCustomSyn);// write highlight settings to config file
    procedure GetSynEditSettings(ASynEdit:TSynEdit);  // read synedit settings from config file
    procedure SetSynEditSettings(ASynEdit:TSynEdit);  // write synedit settings to file
    procedure GetSynEditSelectedColor(ASynEdit:TSynEdit);
    Procedure GetSynEditPreviewSettings(APreviewEditor: TObject);
    procedure AddSpecialHilightAttribsToHighlighter(Syn: TCustomSyn);

    function CreateSyn(LazSynHilighter: TLazSyntaxHighlighter): TCustomSyn;
    function ReadColorScheme(const LanguageName: string): string;
    function ReadPascalColorScheme: string;
    procedure WriteColorScheme(const LanguageName, SynColorScheme: string);
    procedure GetDefaultsForPascalAttribute(Attr: TSynHighlightElement;
      const SynColorScheme: string);
    procedure ReadHighlighterSettings(Syn: TCustomSyn; SynColorScheme: string); 
    procedure ReadDefaultsForHighlighterSettings(Syn: TCustomSyn;
      SynColorScheme: string; DefaultPascalSyn: TPreviewPasSyn);
    procedure WriteHighlighterSettings(Syn: TCustomSyn; SynColorScheme: string);
    procedure GetSpecialLineColors(Syn: TCustomSyn; 
      AddHilightAttr: TAdditionalHilightAttribute; var FG, BG: TColor);
  published
    // general options
    property SynEditOptions:TSynEditorOptions
        read fSynEditOptions write fSynEditOptions
        default SYNEDIT_DEFAULT_OPTIONS;
    property ShowTabCloseButtons: boolean
        read fShowTabCloseButtons write fShowTabCloseButtons;
    property UndoAfterSave:boolean
        read fUndoAfterSave write fUndoAfterSave default true;
    property FindTextAtCursor:boolean
        read fFindTextAtCursor write fFindTextAtCursor default true;
    property UseSyntaxHighlight:boolean
        read fUseSyntaxHighlight write fUseSyntaxHighlight default true;
    property BlockIndent:integer read fBlockIndent write fBlockIndent default 2;
    property UndoLimit:integer read fUndoLimit write fUndoLimit default 32767;
    property TabWidth:integer read fTabWidth write fTabWidth default 8;

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
    property HighlighterList: TEditOptLangList
       read fHighlighterList write fHighlighterList;

    // Code Tools options
    property AutoIdentifierCompletion:boolean
       read fAutoIdentifierCompletion write fAutoIdentifierCompletion default true;
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
    property CodeTemplateIndentToTokenStart: boolean
       read fCTemplIndentToTokenStart write fCTemplIndentToTokenStart;
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
    constructor Create(AnOwner : TComponent); override;
    destructor Destroy; Override;
  published
    property BorderWidth:integer read FBorderWidth write FBorderWidth;
    property ButtonColor:TColor read FButtonColor write SetButtonColor;
    property OnColorChanged:TNotifyEvent 
      read FOnColorChanged write FOnColorChanged;
  end;

  { Editor Options form }
  TEditorOptionsForm = class(TForm)
    MainNoteBook:TNoteBook;
    ImageList: TImageList;

    // general options
    EditorOptionsGroupBox:TGroupBox;
    AltSetsColumnModeCheckBox:TCheckBox;
    AutoIndentCheckBox:TCheckBox;
    BracketHighlightCheckBox: TCheckBox;
    DragDropEditingCheckBox:TCheckBox;
    DropFilesCheckBox:TCheckBox;
    HalfPageScrollCheckBox:TCheckBox;
    KeepCaretXCheckBox:TCheckBox;
    PersistentCaretCheckBox:TCheckBox;
    ScrollByOneLessCheckBox:TCheckBox;
    ScrollPastEofCheckBox:TCheckBox;
    ScrollPastEolCheckBox:TCheckBox;
    ShowCloseBtnInNoteBookCheckBox:TCheckBox;
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
    KeyMappingTreeView:TTreeView;
    KeyMappingConsistencyCheckButton:TButton;

    // Color options
    LanguageComboBox:TComboBox;
    LanguageLabel:TLabel;
    FileExtensionsComboBox:TComboBox;
    FileExtensionsLabel:TLabel;
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
    SetAttributeToDefaultButton:TButton;
    SetAllAttributesToDefaultButton:TButton;
    ColorPreview:TPreviewEditor;

    // Code Tools options
    AutomaticFeaturesGroupBox:TGroupBox;
    AutoIdentifierCompletionCheckBox:TCheckBox;
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
    CodeTemplateIndentTypeRadioGroup: TRadioGroup;
    SynAutoComplete:TSynEditAutoComplete;

    // buttons at bottom
    OkButton:TButton;
    CancelButton:TButton;

    // form
    procedure EditorOptionsFormResize(Sender: TObject);

    // general
    procedure GeneralCheckBoxOnClick(Sender: TObject);
    procedure ComboBoxOnChange(Sender:TObject);
    procedure ComboBoxOnExit(Sender:TObject);
    procedure ComboBoxOnKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ColorButtonColorChanged(Sender:TObject);

    // display
    procedure FontDialogApplyClicked(Sender: TObject);
    procedure EditorFontButtonClick(Sender:TObject);

    // key mapping
    procedure KeyMappingTreeViewMouseUp(Sender:TObject;
       Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
    procedure KeyMappingConsistencyCheckButtonClick(Sender: TObject);

    // color
    procedure ColorElementListBoxMouseUp(Sender:TObject;
       Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
    procedure ColorPreviewMouseUp(Sender:TObject;
       Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
    procedure OnSpecialLineColors(Sender: TObject; Line: integer;
       var Special: boolean; var FG, BG: TColor);
    procedure SetAttributeToDefaultButtonClick(Sender: TObject);
    procedure SetAllAttributesToDefaultButtonClick(Sender: TObject);

    // code tools
    procedure CodeTemplateListBoxMouseUp(Sender:TObject;
       Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
    procedure CodeTemplateFileNameButtonClick(Sender:TObject);
    procedure CodeTemplateButtonClick(Sender:TObject);
    procedure CodeTemplatesGroupBoxResize(Sender: TObject);

    // buttons at bottom
    procedure OkButtonClick(Sender:TObject);
    procedure CancelButtonClick(Sender:TObject);
  private
    FormCreating: boolean;
    PreviewSyn: TCustomSyn;
    PreviewEdits:array[1..3] of TPreviewEditor;
    CurLanguageID: integer; // current index in EditorOpts.EditOptHighlighterList
    CurHighlightElement: TSynHighlightElement;
    CurCodeTemplate: integer;
    UpdatingColor: boolean;
    fHighlighterList: TStringList; // list of "ColorScheme" Data=TCustomSyn
    fColorSchemes: TStringList; // list of LanguageName=ColorScheme
    fFileExtensions: TStringList; // list of LanguageName=FileExtensions

    procedure SetComboBoxText(AComboBox:TComboBox;AText:AnsiString);
    procedure FontDialogNameToFont(FontDialogName:Ansistring;AFont:TFont);
    procedure InvalidatePreviews;
    procedure SetPreviewSynInAllPreviews;
    procedure SetupButtonBar;
    procedure ResizeButtonBar;

    // general
    procedure SetupGeneralPage;
    procedure ResizeGeneralPage;

    // display
    procedure SetupDisplayPage;    
    procedure ResizeDisplayPage;

    // keymapping
    procedure SetupKeyMappingsPage;
    procedure ResizeKeyMappingsPage;
    function KeyMappingRelationToString(Index:integer): String;
    function KeyMappingRelationToString(KeyRelation: TKeyCommandRelation): String;
    procedure FillKeyMappingTreeView;

    // color
    procedure SetupColorPage;
    procedure ResizeColorPage;
    procedure ShowCurAttribute;
    procedure FindCurHighlightElement;
    function GetHighlighter(SynClass: TCustomSynClass;
      const ColorScheme: string; CreateIfNotExists: boolean): TCustomSyn;
    procedure ClearHighlighters;
    procedure SaveAllHighlighters;
    procedure FillColorElementListBox;
    function GetCurColorScheme(const LanguageName: string): string;
    procedure SetCurColorScheme(const LanguageName, ColorScheme: string);
    procedure SaveAllColorSchemes;
    function GetCurFileExtension(const LanguageName: string): string;
    procedure SetCurFileExtension(const LanguageName, FileExtensions: string);
    procedure SaveAllFileExtensions;
    procedure SetColorElementsToDefaults(OnlySelected: boolean);

    // code tools
    procedure SetupCodeToolsPage;
    procedure ResizeCodeToolsPage;
    procedure FillCodeTemplateListBox;
    procedure ShowCurCodeTemplate;
    procedure SaveCurCodeTemplate;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  LazSyntaxHighlighterNames : array[TLazSyntaxHighlighter] of string = (
     'None',
     'Text',
     'FreePascal',
     'Delphi',
     'LFM',
     'XML',
     'HTML',
     'C++',
     'Perl'
   );

var
  EditorOptionsForm: TEditorOptionsForm;
  EditorOpts: TEditorOptions;

function ShowEditorOptionsDialog:TModalResult;
function StrToLazSyntaxHighlighter(const s: string): TLazSyntaxHighlighter;
function ExtensionToLazSyntaxHighlighter(Ext:string): TLazSyntaxHighlighter;

implementation


uses Math;

const
  ValidAttribChars = ['a'..'z','A'..'Z','_','0'..'9'];

  // several language types can be redirected. For example there are FreePascal
  // and Delphi, but currently both are hilighted with the FreePascal
  // highlighter
  CompatibleLazSyntaxHilighter:
    array[TLazSyntaxHighlighter] of TLazSyntaxHighlighter= (
        lshNone, lshText, lshFreePascal, lshFreePascal, lshLFM, lshXML, lshHTML,
        lshCPP, lshPerl
      );
      
  DefaultColorScheme = 'Default';


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
var s, CurExt: string;
  LangID, StartPos, EndPos: integer;
begin
  Result:=lshNone;
  if (Ext='') or (Ext='.') or (EditorOpts.HighlighterList=nil) then exit;
  Ext:=lowercase(Ext);
  if (Ext[1]='.') then Ext:=copy(Ext,2,length(Ext)-1);
  LangID:=0;
  while LangID<EditorOpts.HighlighterList.Count do begin
    s:=EditorOpts.HighlighterList[LangID].FileExtensions;
    StartPos:=1;
    while StartPos<=length(s) do begin
      Endpos:=StartPos;
      while (EndPos<=length(s)) and (s[EndPos]<>';') do inc(EndPos);
      CurExt:=copy(s,Startpos,EndPos-StartPos);
      if (CurExt<>'') and (CurExt[1]='.') then begin
        CurExt:=copy(CurExt,2,length(CurExt)-1);
      end;
      if lowercase(CurExt)=Ext then begin
        Result:=EditorOpts.HighlighterList[LangID].TheType;
        exit;
      end;
      Startpos:=EndPos+1;
    end;
    inc(LangID);
  end;
end;

const
  EditOptsConfFileName = 'editoroptions.xml';


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
      if (ep>sp) or ((Value<>'') and (Value[length(Value)] in [#10,#13])) then
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

function StrToValidXMLName(const s: string): string;
var i: integer;
begin
  Result:=s;
  // replace invalid characters
  for i:=1 to length(Result) do
    if (not (Result[i] in ValidAttribChars)) then
      Result[i]:='_';
end;

procedure CopyHiLightAttributeValues(Src, Dest: TSynHighlightElement);
begin
  Dest.Background:=Src.Background;
  Dest.Foreground:=Src.Foreground;
  Dest.Style:=Src.Style;
end;

{ TEditOptLanguageInfo }

constructor TEditOptLanguageInfo.Create;
begin
  inherited Create;
  
end;

destructor TEditOptLanguageInfo.Destroy;
begin
  MappedAttributes.Free;
  inherited Destroy;
end;

function TEditOptLanguageInfo.SampleLineToAddAttr(
  Line: integer): TAdditionalHilightAttribute;
begin
  if Line<1 then begin
    Result:=ahaNone;
    exit;
  end;
  for Result:=Low(TAdditionalHilightAttribute) 
  to High(TAdditionalHilightAttribute) do
    if (Result<>ahaNone) and (AddAttrSampleLines[Result]=Line) then exit;
  Result:=ahaNone;
end;

function TEditOptLanguageInfo.GetDefaultFilextension: string;
var p: integer;
begin
  // read the first file extension
  p:=1;
  while (p<=length(FileExtensions)) and (FileExtensions[p]<>';') do
    inc(p);
  if p>1 then
    Result:='.'+copy(FileExtensions,1,p-1)
  else
    Result:='';
end;

{ TEditOptLangList }

function TEditOptLangList.GetInfos(
  Index: integer): TEditOptLanguageInfo;
begin
  if (Index<0) or (Index>=Count) then
    raise Exception.Create('TEditOptLangList.GetInfos Index '
      +IntToStr(Index)+' out of bounds. Count='+IntToStr(Count));
  Result:=TEditOptLanguageInfo(inherited Items[Index]);
end;

procedure TEditOptLangList.Clear;
var i: integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  inherited Clear;
end;

constructor TEditOptLangList.Create;
var NewInfo: TEditOptLanguageInfo;
begin
  inherited Create;
  
  { create the meta information for each available highlighter.
    Plz keep the pascal highlighter at the top. The rest can be ordered as you
    like.
  }
  
  // create info for pascal
  NewInfo:=TEditOptLanguageInfo.Create;
  with NewInfo do begin
    TheType:=CompatibleLazSyntaxHilighter[lshFreePascal];
    DefaultCommentType:=DefaultCommentTypes[lshFreePascal];
    SynClass:=LazSyntaxHighlighterClasses[TheType];
    FileExtensions:='pp;pas;inc;lpr;lrs;dpr;dpk';
    SampleSource:=
      '{ Comment }'#13+
      '{$R- compiler directive}'#13+
      'procedure TForm1.Button1Click(Sender: TObject);'#13+
      'var  // Delphi Comment'#13+
      '  Number, I, X: Integer;'#13+
      'begin'#13+
      '  Number := 12345;'#13+
      '  Caption := ''The number is '' + IntToStr(Number);'#13+
      '  asm'#13+
      '    MOV AX,1234h'#13+
      '    MOV Number,AX'#13+
      '  end;'#13+
      '  X := 10;'#13+
      '  { Search Match, Text Block }'#13+
      '  for I := 0 to Number do { execution point }'#13+
      '  begin'#13+
      '    Inc(X); { Enabled breakpoint }'#13+
      '    Dec(X); { Disabled breakpoint }'#13+
      '    X := X + 1.0; { Error line }'#13+
      '    ListBox1.Items.Add(IntToStr(X));'#13+
      '  end;'#13+
      'end;'#13+
      #13;
    AddAttrSampleLines[ahaDisabledBreakpoint]:=18;
    AddAttrSampleLines[ahaEnabledBreakpoint]:=17;
    AddAttrSampleLines[ahaErrorLine]:=19;
    AddAttrSampleLines[ahaExecutionPoint]:=15;
    AddAttrSampleLines[ahaTextBlock]:=14;
  end;
  Add(NewInfo);
  
  // create info for html
  NewInfo:=TEditOptLanguageInfo.Create;
  with NewInfo do begin
    TheType:=CompatibleLazSyntaxHilighter[lshHTML];
    DefaultCommentType:=DefaultCommentTypes[lshHTML];
    SynClass:=LazSyntaxHighlighterClasses[TheType];
    FileExtensions:='htm;html';
    SampleSource:=
      '<html>'#13+
      '<title>Lazarus Sample source for html</title>'#13+
      '<body bgcolor=#ffffff background="bg.jpg">'#13+
      '<!-- Comment -->'#13+
      '<img src="lazarus.jpg">'#13+
      '<p>'#13+
      '  Some Text'#13+
      '  Ampersands: &nbsp;F&nbsp;P&nbsp;C'#13+
      '</p>'#13+
      '<invalid_tag>'#13+
      '<!-- Text Block -->'#13+
      '</body>'#13+
      '</html>'#13+
      #13;
    AddAttrSampleLines[ahaTextBlock]:=11;
    MappedAttributes:=TStringList.Create;
    with MappedAttributes do begin
      Add('Comment=Comment');
      Add('Space=Space');
    end;
  end;
  Add(NewInfo);
  
  // create info for cpp
  NewInfo:=TEditOptLanguageInfo.Create;
  with NewInfo do begin
    TheType:=CompatibleLazSyntaxHilighter[lshCPP];
    DefaultCommentType:=DefaultCommentTypes[lshCPP];
    SynClass:=LazSyntaxHighlighterClasses[TheType];
    FileExtensions:='c;cc;cpp;h;hpp';
    SampleSource:=
      '/* Comment */'#13+
      '#include <stdio.h>'#13+
      '#include <stdlib.h>'#13+
      #13+
      'static char line_buf[LINE_BUF];'#13+
      #13+
      'int main(int argc,char **argv){'#13+
      '  FILE *file;'#13+
      '  line_buf[0]=0;'#13+
      '  printf("\n");'#13+
      '  return 0;'#13+
      '}'#13+
      ''#13+
      #13;
    AddAttrSampleLines[ahaTextBlock]:=11;
    MappedAttributes:=TStringList.Create;
    with MappedAttributes do begin
      Add('Assembler=Assembler');
      Add('Comment=Comment');
      Add('Preprocessor=Comment');
      Add('Identifier=Identifier');
      Add('Reserved_word=Reserved_word');
      Add('Number=Number');
      Add('Space=Space');
      Add('String=String');
      Add('Symbol=Symbol');
    end;
  end;
  Add(NewInfo);

  // create info for XML
  NewInfo:=TEditOptLanguageInfo.Create;
  with NewInfo do begin
    TheType:=CompatibleLazSyntaxHilighter[lshXML];
    DefaultCommentType:=DefaultCommentTypes[lshXML];
    SynClass:=LazSyntaxHighlighterClasses[TheType];
    FileExtensions:='xml;xsd;xsl;xslt;dtd';
    SampleSource:=
      '<?xml version="1.0"?>'#13+
      '<!DOCTYPE root ['#13+
      '  ]>'#13+
      '<!-- Comment -->'#13+
      '<root version="&test;">'#13+
      '  <![CDATA[ **CDATA section** ]]>'#13+
      '</root>'#13+
      '<!-- Text Block -->'#13+
      ''#13+
      #13;
    AddAttrSampleLines[ahaTextBlock]:=8;
    MappedAttributes:=TStringList.Create;
    with MappedAttributes do begin
      Add('Element=Reserved_word');
      Add('Comment=Comment');
      Add('Text=Identifier');
      Add('Space=Space');
      Add('Symbol=Symbol');
    end;
  end;
  Add(NewInfo);

  // create info for LFM
  NewInfo:=TEditOptLanguageInfo.Create;
  with NewInfo do begin
    TheType:=CompatibleLazSyntaxHilighter[lshLFM];
    DefaultCommentType:=DefaultCommentTypes[lshLFM];
    SynClass:=LazSyntaxHighlighterClasses[TheType];
    FileExtensions:='lfm;dfm;xfm';
    SampleSource:=
      '{ Lazarus Form Definitions }'#13+
      'object TestForm: TTestForm'#13+
      '  Left = 273'#13+
      '  Top = 103'#13+
      '  Caption = ''sample source'''#13+
      'end'#13+
      '{ Text Block }'#13+
      ''#13+
      #13;
    AddAttrSampleLines[ahaTextBlock]:=7;
    MappedAttributes:=TStringList.Create;
    with MappedAttributes do begin
      Add('Element=Reserved_word');
      Add('Comment=Comment');
      Add('Identifier=Identifier');
      Add('Key=Reserved_word');
      Add('Number=Number');
      Add('Space=Space');
      Add('String=String');
      Add('Symbol=Symbol');
    end;
  end;
  Add(NewInfo);
  
  // create info for Perl
  NewInfo:=TEditOptLanguageInfo.Create;
  with NewInfo do begin
    TheType:=CompatibleLazSyntaxHilighter[lshPerl];
    DefaultCommentType:=DefaultCommentTypes[lshPerl];
    SynClass:=LazSyntaxHighlighterClasses[TheType];
    FileExtensions:='pl;pm;cgi';
    SampleSource:=
      '#!/usr/bin/perl'#13+
      '# Perl sample code'#13+
      ''#13+
      '$i = "10";'#13+
      'print "$ENV{PATH}\n";'#13+
      '($i =~ /\d+/) || die "Error\n";'#13+
      ''#13+
      '# Text Block'#13+
      ''#13+
      #13;
    AddAttrSampleLines[ahaTextBlock]:=8;
    MappedAttributes:=TStringList.Create;
    with MappedAttributes do begin
      Add('Comment=Comment');
      Add('Identifier=Identifier');
      Add('KeyAttri=Reserved_word');
      Add('NumberAttri=Number');
      Add('SpaceAttri=Space');
      Add('StringAttri=String');
      Add('Symbol=Symbol');
    end;
  end;
  Add(NewInfo);
end;

destructor TEditOptLangList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TEditOptLangList.FindByName(const Name: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) 
  and (AnsiCompareText(Items[Result].SynClass.GetLanguageName,Name)<>0) do
    dec(Result);
end;

function TEditOptLangList.FindByClass(
  CustomSynClass: TCustomSynClass): integer;
begin
  Result:=Count-1;
  while (Result>=0) 
  and (Items[Result].SynClass<>CustomSynClass) do
    dec(Result);
end;

function TEditOptLangList.FindByHighlighter(Hilighter: TSynCustomHighlighter
  ): integer;
begin
  if Hilighter<>nil then begin
    Result:=FindByClass(TCustomSynClass(Hilighter.ClassType));
  end else begin
    Result:=-1;
  end;
end;

function TEditOptLangList.FindByType(
  AType: TLazSyntaxHighlighter): integer;
begin
  AType:=CompatibleLazSyntaxHilighter[AType];
  Result:=Count-1;
  while (Result>=0) and (Items[Result].TheType<>AType) do
    dec(Result);
end;

function TEditOptLangList.GetDefaultFilextension(
  AType: TLazSyntaxHighlighter): string;
var i: integer;
begin
  i:=FindByType(AType);
  if i>=0 then
    Result:=Items[i].GetDefaultFilextension
  else
    Result:='';
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
    writeln('NOTE: editor options config file not found');
  end;
  XMLConfig:=TXMLConfig.Create(ConfFileName);
  
  // set defaults

  // General options
  fShowTabCloseButtons:=true;
  fBlockIndent:=2;
  fUndoLimit:=32767;
  fTabWidth:=8;

  // Display options
  fEditorFont:='courier';

  // Key Mappings
  fKeyMappingScheme:='default';
  fKeyMap:=TKeyCommandRelationList.Create;

  // Color options
  fHighlighterList:=TEditOptLangList.Create;

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
        writeln('WARNING: unable to write code template file "',
          fCodeTemplateFileName,'"');
      end;
    end;
  end;
  
end;

destructor TEditorOptions.Destroy;
begin
  fHighlighterList.Free;
  fKeyMap.Free;
  XMLConfig.Free;
  inherited Destroy;
end;

procedure TEditorOptions.Load;
// load options from XML file
var SynEditOpt:TSynEditorOption;
  SynEditOptName:ansistring;
  i: integer;
begin
  try
    // general options
    for SynEditOpt:=Low(TSynEditorOption) to High(TSynEditorOption) do begin
      case SynEditOpt of
        eoAltSetsColumnMode:SynEditOptName:='AltSetsColumnMode';
        eoAutoIndent:SynEditOptName:='AutoIndent';
        eoBracketHighlight:SynEditOptName:='BracketHighlight';
        eoDoubleClickSelectsLine:SynEditOptName:='DoubleClickSelectsLine';
        eoDragDropEditing:SynEditOptName:='DragDropEditing';
        eoDropFiles:SynEditOptName:='DropFiles';
        eoHalfPageScroll:SynEditOptName:='HalfPageScroll';
        eoKeepCaretX:SynEditOptName:='KeepCaretX';
        eoPersistentCaret:SynEditOptName:='PersistentCaret';
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

    fShowTabCloseButtons:=
      XMLConfig.GetValue('EditorOptions/General/Editor/ShowTabCloseButtons',true);
    fUndoAfterSave:=
      XMLConfig.GetValue('EditorOptions/General/Editor/UndoAfterSave',true);
    fFindTextAtCursor:=
      XMLConfig.GetValue('EditorOptions/General/Editor/FindTextAtCursor',true);
    fUseSyntaxHighlight:=
      XMLConfig.GetValue('EditorOptions/General/Editor/UseSyntaxHighlight',true);
    fBlockIndent:=
      XMLConfig.GetValue('EditorOptions/General/Editor/BlockIndent',fBlockIndent);
    fUndoLimit:=
      XMLConfig.GetValue('EditorOptions/General/Editor/UndoLimit',fUndoLimit);
    fTabWidth:=
      XMLConfig.GetValue('EditorOptions/General/Editor/TabWidth',fTabWidth);

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
      XMLConfig.GetValue('EditorOptions/KeyMapping/Scheme',
        StrToValidXMLName(fKeyMappingScheme));
    fKeyMap.LoadFromXMLConfig(XMLConfig
      ,'EditorOptions/KeyMapping/'+fKeyMappingScheme+'/');

    // Color options
    for i:=0 to fHighlighterList.Count-1 do begin
      fHighlighterList[i].FileExtensions:=
        XMLConfig.GetValue('EditorOptions/Color/Lang'
          +StrToValidXMLName(fHighlighterList[i].SynClass.GetLanguageName)
          +'/FileExtensions/Value',fHighlighterList[i].FileExtensions);
      // color attributes are stored in the highlighters
    end;

    // Code Tools options
    fAutoIdentifierCompletion:=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoIdentifierCompletion',true);
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
    fCTemplIndentToTokenStart:=
      XMLConfig.GetValue('EditorOptions/CodeTools/CodeTemplateIndentToTokenStart/Value'
        ,false);

  except
    on E: Exception do
      writeln('[TEditorOptions.Load] ERROR: ',e.Message);
  end;
end;

procedure TEditorOptions.Save;
// save options to XML file
var SynEditOpt:TSynEditorOption;
  SynEditOptName:ansistring;
  i: integer;
begin
  try
    XMLConfig.SetValue('EditorOptions/Version',EditorOptsFormatVersion);
  
    // general options
    for SynEditOpt:=Low(TSynEditorOption) to High(TSynEditorOption) do begin
      case SynEditOpt of
        eoAltSetsColumnMode:SynEditOptName:='AltSetsColumnMode';
        eoAutoIndent:SynEditOptName:='AutoIndent';
        eoBracketHighlight:SynEditOptName:='BracketHighlight';
        eoDoubleClickSelectsLine:SynEditOptName:='DoubleClickSelectsLine';
        eoDragDropEditing:SynEditOptName:='DragDropEditing';
        eoDropFiles:SynEditOptName:='DropFiles';
        eoHalfPageScroll:SynEditOptName:='HalfPageScroll';
        eoKeepCaretX:SynEditOptName:='KeepCaretX';
        eoPersistentCaret:SynEditOptName:='PersistentCaret';
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

    XMLConfig.SetValue('EditorOptions/General/Editor/ShowTabCloseButtons'
      ,fShowTabCloseButtons);
    XMLConfig.SetValue('EditorOptions/General/Editor/UndoAfterSave'
      ,fUndoAfterSave);
    XMLConfig.SetValue('EditorOptions/General/Editor/FindTextAtCursor'
      ,fFindTextAtCursor);
    XMLConfig.SetValue('EditorOptions/General/Editor/UseSyntaxHighlight'
      ,fUseSyntaxHighlight);
    XMLConfig.SetValue('EditorOptions/General/Editor/BlockIndent'
      ,fBlockIndent);
    XMLConfig.SetValue('EditorOptions/General/Editor/UndoLimit'
      ,fUndoLimit);
    XMLConfig.SetValue('EditorOptions/General/Editor/TabWidth'
      ,fTabWidth);

    // Display options
    XMLConfig.SetValue('EditorOptions/Display/VisibleRightMargin'
      ,fVisibleRightMargin);
    XMLConfig.SetValue('EditorOptions/Display/VisibleGutter',fVisibleGutter);
    XMLConfig.SetValue('EditorOptions/Display/ShowLineNumbers',fShowLineNumbers);
    XMLConfig.SetValue('EditorOptions/Display/GutterColor',fGutterColor);
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
    for i:=0 to fHighlighterList.Count-1 do begin
      XMLConfig.SetValue('EditorOptions/Color/Lang'
        +StrToValidXMLName(fHighlighterList[i].SynClass.GetLanguageName)
        +'/FileExtensions/Value',fHighlighterList[i].FileExtensions);
      // color attributes are stored in the highlighters
    end;

    // Code Tools options
    XMLConfig.SetValue('EditorOptions/CodeTools/AutoIdentifierCompletion'
      ,fAutoIdentifierCompletion);
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
    XMLConfig.GetValue('EditorOptions/CodeTools/CodeTemplateIndentToTokenStart/Value'
        ,fCTemplIndentToTokenStart);


    XMLConfig.Flush;
  except
    on E: Exception do
      writeln('[TEditorOptions.Save] ERROR: ',e.Message);
  end;
end;

function TEditorOptions.CreateSyn(
  LazSynHilighter: TLazSyntaxHighlighter): TCustomSyn;
begin
  if LazSyntaxHighlighterClasses[LazSynHilighter]<>nil then begin
    Result:=LazSyntaxHighlighterClasses[LazSynHilighter].Create(nil);
    AddSpecialHilightAttribsToHighlighter(Result);
    GetHighlighterSettings(Result);
  end else
    Result:=nil;
end;

function TEditorOptions.ReadColorScheme(const LanguageName: string): string;
begin
  if LanguageName='' then begin
    Result:=DefaultColorScheme;
    exit;
  end;
  if LanguageName<>TPreviewPasSyn.GetLanguageName then
    Result:=XMLConfig.GetValue(
      'EditorOptions/Color/Lang'+StrToValidXMLName(LanguageName)
        +'/ColorScheme/Value','')
  else
    Result:='';
  if Result='' then Result:=ReadPascalColorScheme;
end;

function TEditorOptions.ReadPascalColorScheme: string;
var FormatVersion: integer;
begin
  FormatVersion:=XMLConfig.GetValue('EditorOptions/Color/Version',0);
  if FormatVersion>1 then begin
    Result:=XMLConfig.GetValue(
      'EditorOptions/Color/Lang'
      +StrToValidXMLName(TPreviewPasSyn.GetLanguageName)
      +'/ColorScheme/Value','');
  end else
    Result:=XMLConfig.GetValue('EditorOptions/Color/ColorScheme','');
  if Result='' then Result:=DefaultColorScheme;
end;

procedure TEditorOptions.WriteColorScheme(const LanguageName,
  SynColorScheme: string);
begin
  if (LanguageName='') or (SynColorScheme='') then exit;
  XMLConfig.SetValue('EditorOptions/Color/Lang'+StrToValidXMLName(LanguageName)
    +'/ColorScheme/Value',SynColorScheme);
  XMLConfig.SetValue('EditorOptions/Color/Version',EditorOptsFormatVersion);
end;

procedure TEditorOptions.GetDefaultsForPascalAttribute(
  Attr: TSynHighlightElement; const SynColorScheme: string);
var
  AttriName:string;
  DefBGCol,DefFGCol:TColor;
  DefFontStyles:TFontStyles;
begin
  AttriName:=Attr.Name;
  if AttriName='' then exit;
  
  DefFGCol:=clNone;
  DefBGCol:=clNone;
  DefFontStyles:=[];
  if lowercase(SynColorScheme)='twilight'  then begin
    // default for twilight color scheme
    DefBGCol:=clBlack;
    DefFGCol:=clWhite;
    if AttriName='Assembler' then begin
      DefFGCol:=clLime;
    end else if AttriName='Comment' then begin
      DefFGCol:=clGray;
    end else if AttriName='Directive' then begin
      DefFGCol:=clRed;
    end else if AttriName='Reserved word' then begin
      DefFGCol:=clAqua;
      DefFontStyles:=[fsBold];
    end else if AttriName='Number' then begin
      DefFGCol:=clFuchsia;
    end else if AttriName='String' then begin
      DefFGCol:=clYellow;
    end else if AttriName='Symbol' then begin
      DefFGCol:=clAqua;
    end else if AttriName=AdditionalHighlightAttributes[ahaTextBlock] then begin
      DefBGCol:=clWhite;
      DefFGCol:=clBlack
    end else if AttriName=AdditionalHighlightAttributes[ahaExecutionPoint] 
    then begin
      DefBGCol:=clBlue;
      DefFGCol:=clWhite;
    end else if AttriName=AdditionalHighlightAttributes[ahaEnabledBreakpoint]
    then begin
      DefBGCol:=clRed;
      DefFGCol:=clWhite;
    end else if AttriName=AdditionalHighlightAttributes[ahaDisabledBreakpoint]
    then begin
      DefBGCol:=clLime;
      DefFGCol:=clRed;
    end else if AttriName=AdditionalHighlightAttributes[ahaErrorLine] then begin
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
    end else if AttriName='Directive' then begin
      DefFGCol:=clRed;
      DefFontStyles:=[fsBold];
    end else if AttriName='Reserved word' then begin
      DefFontStyles:=[fsBold];
    end else if AttriName='Number' then begin
      DefFGCol:=clNavy;
    end else if AttriName='String' then begin
      DefFGCol:=clBlue;
    end else if AttriName='Symbol' then begin
      DefFGCol:=clRed;
    end else if AttriName=AdditionalHighlightAttributes[ahaTextBlock] then begin
      DefBGCol:=clNavy;
      DefFGCol:=clWhite;
    end else if AttriName=AdditionalHighlightAttributes[ahaExecutionPoint] 
    then begin
      DefBGCol:=clDKGray;
      DefFGCol:=clWhite;
    end else if AttriName=AdditionalHighlightAttributes[ahaEnabledBreakpoint]
    then begin
      DefBGCol:=clRed;
      DefFGCol:=clBlack;
    end else if AttriName=AdditionalHighlightAttributes[ahaDisabledBreakpoint]
    then begin
      DefBGCol:=clGreen;
      DefFGCol:=clBlack;
    end else if AttriName=AdditionalHighlightAttributes[ahaErrorLine] then begin
      DefBGCol:=$50a0ff;
      DefFGCol:=clBlack;
    end;
  end;
  
  Attr.Foreground:=DefFGCol;
  Attr.Background:=DefBGCol;
  Attr.Style:=DefFontStyles;
end;

procedure TEditorOptions.ReadDefaultsForHighlighterSettings(Syn: TCustomSyn;
  SynColorScheme: string; DefaultPascalSyn: TPreviewPasSyn);
// if SynColorScheme='' then default ColorScheme will be used
var VirginSyn, DefaultSyn: TCustomSyn;
  i, j: integer;
  MappedAttriName, AttriName: string;
  HilightInfo: TEditOptLanguageInfo;
  aha: TAdditionalHilightAttribute;
  CustomPascalSyn: boolean;
begin
  if SynColorScheme='' then SynColorScheme:=ReadColorScheme(Syn.LanguageName);
  if SynColorScheme='' then exit;
  CustomPascalSyn:=(DefaultPascalSyn<>nil);
  if (Syn is TPreviewPasSyn) then begin
    // the defaults for pascal are fix programmed
    for i:=0 to Syn.AttrCount-1 do
      GetDefaultsForPascalAttribute(Syn.Attribute[i],SynColorScheme);
  end else begin
    // the defaults of all non pascal languages are the mapped current values of
    // pascal or the non mapped values of an untouched highlighter of the same
    // type
    i:=HighlighterList.FindByClass(TCustomSynClass(Syn.ClassType));
    if i<0 then exit;
    HilightInfo:=HighlighterList[i];
    if not CustomPascalSyn then
      DefaultPascalSyn:=TPreviewPasSyn.Create(nil);
    VirginSyn:=TCustomSynClass(Syn.ClassType).Create(nil);
    try
      if not CustomPascalSyn then begin
        AddSpecialHilightAttribsToHighlighter(DefaultPascalSyn);
        ReadHighlighterSettings(DefaultPascalSyn,SynColorScheme);
      end;
      // map attributes
      for i:=0 to Syn.AttrCount-1 do begin
        AttriName:=StrToValidXMLName(Syn.Attribute[i].Name);
        if AttriName='' then continue;
        // check, if there is a known mapping for this attribute
        if HilightInfo.MappedAttributes<>nil then
          MappedAttriName:=
            HilightInfo.MappedAttributes.Values[AttriName]
        else
          MappedAttriName:='';
        if MappedAttriName='' then begin
          // all special line color attributes can be mapped 1:1
          for aha:=Low(TAdditionalHilightAttribute) 
          to High(TAdditionalHilightAttribute) do
            if AnsiCompareText(
              StrToValidXMLName(AdditionalHighlightAttributes[aha]),AttriName)=0
            then
              MappedAttriName:=AttriName;
        end;
        if MappedAttriName<>'' then
          DefaultSyn:=DefaultPascalSyn
        else
          DefaultSyn:=VirginSyn;
        // read defaults
        j:=DefaultSyn.AttrCount-1;
        while (j>=0) do begin
          if AnsiCompareText(StrToValidXMLName(DefaultSyn.Attribute[j].Name),
            MappedAttriName)=0
          then begin
            CopyHiLightAttributeValues(DefaultSyn.Attribute[j],Syn.Attribute[i]);
            break;
          end;
          dec(j);
        end;
      end;
    finally
      VirginSyn.Free;
      if not CustomPascalSyn then
        DefaultPascalSyn.Free;
    end;
  end;
end;

procedure TEditorOptions.ReadHighlighterSettings(Syn: TCustomSyn;
  SynColorScheme: string); 
// if SynColorScheme='' then default ColorScheme will be used
var FormatVersion: integer;
  i: integer;
  AttriName: string;
  Attri: TSynHighlightElement;
  b: boolean;
  fs: TFontStyles;
  Path: string;
begin
  // initialize with defaults
  if SynColorScheme='' then SynColorScheme:=ReadColorScheme(Syn.LanguageName);
  if (SynColorScheme='') or (Syn.LanguageName='') then exit;
  ReadDefaultsForHighlighterSettings(Syn,SynColorScheme,nil);
  // read settings, that are different from the defaults
  FormatVersion:=XMLConfig.GetValue(
    'EditorOptions/Color/Lang'+StrToValidXMLName(Syn.LanguageName)+'/Version',0);
  if FormatVersion>1 then begin
    // read all attributes
    for i:=0 to Syn.AttrCount-1 do begin
      Attri:=Syn.Attribute[i];
      AttriName:=StrToValidXMLName(Attri.Name);
      if AttriName='' then continue;
      Path:='EditorOptions/Color/Lang'+StrToValidXMLName(Syn.LanguageName)
            +'/Scheme'+StrToValidXMLName(SynColorScheme)
            +'/'+StrToValidXMLName(AttriName)+'/';
      Attri.BackGround:=XMLConfig.GetValue(Path+'BackgroundColor/Value',
                                           Attri.Background);
      Attri.ForeGround:=XMLConfig.GetValue(Path+'ForegroundColor/Value',
                                           Attri.Foreground);
      fs:=[];
      b:=XMLConfig.GetValue(Path+'Style/Bold',fsBold in Attri.Style);
      if b then Include(fs,fsBold);
      b:=XMLConfig.GetValue(Path+'Style/Italic',fsItalic in Attri.Style);
      if b then Include(fs,fsItalic);
      b:=XMLConfig.GetValue(Path+'Style/Underline',fsUnderline in Attri.Style);
      if b then Include(fs,fsUnderline);
      Attri.Style:=fs;
    end;
  end else begin
    // FormatVersion < 2
    // the oldest format only supports pascal
    if Syn is TPreviewPasSyn then begin
      for i:=0 to Syn.AttrCount-1 do begin
        Attri:=Syn.Attribute[i];
        AttriName:=StrToValidXMLName(Attri.Name);
        if AttriName='' then continue;
        Path:='EditorOptions/Color/'+StrToValidXMLName(SynColorScheme)+'/'
          +StrToValidXMLName(AttriName)+'/';
        Attri.BackGround:=XMLConfig.GetValue(Path+'BackgroundColor',
                                             Attri.Background);
        Attri.ForeGround:=XMLConfig.GetValue(Path+'ForegroundColor',
                                             Attri.Foreground);
        fs:=[];
        b:=XMLConfig.GetValue(Path+'Bold',fsBold in Attri.Style);
        if b then Include(fs,fsBold);
        b:=XMLConfig.GetValue(Path+'Italic',fsItalic in Attri.Style);
        if b then Include(fs,fsItalic);
        b:=XMLConfig.GetValue(Path+'Underline',fsUnderline in Attri.Style);
        if b then Include(fs,fsUnderline);
        Attri.Style:=fs;
      end;
    end;
  end;
end;

procedure TEditorOptions.WriteHighlighterSettings(Syn: TCustomSyn;
  SynColorScheme: string); 
var OldSyn: TCustomSyn;
  i: integer;
  AttriName: string;
  Attri, OldAttri: TSynHighlightElement;
  Path: string;
begin
  // read the old settings, compare and write only the differences
  if SynColorScheme='' then SynColorScheme:=ReadColorScheme(Syn.LanguageName);
  OldSyn:=TCustomSynClass(Syn.ClassType).Create(nil);
  try
    AddSpecialHilightAttribsToHighlighter(OldSyn);
    ReadHighlighterSettings(OldSyn,SynColorScheme);
    // write colorscheme
    XMLConfig.SetValue('EditorOptions/Color/Lang'
      +StrToValidXMLName(Syn.LanguageName)+'/Version',
      EditorOptsFormatVersion);
    // write all attributes
    for i:=0 to Syn.AttrCount-1 do begin
      Attri:=Syn.Attribute[i];
      OldAttri:=OldSyn.Attribute[i];
      AttriName:=StrToValidXMLName(Attri.Name);
      if AttriName='' then continue;
      Path:='EditorOptions/Color/Lang'+StrToValidXMLName(Syn.LanguageName)
            +'/Scheme'+StrToValidXMLName(SynColorScheme)
            +'/'+StrToValidXMLName(AttriName)+'/';
      if Attri.Background<>OldAttri.Background then
        XMLConfig.SetValue(Path+'BackgroundColor/Value',Attri.Background);
      if Attri.Foreground<>OldAttri.Foreground then
        XMLConfig.SetValue(Path+'ForegroundColor/Value',Attri.Foreground);
      if Attri.Style<>OldAttri.Style then begin
        XMLConfig.SetValue(Path+'Style/Bold',fsBold in Attri.Style);
        XMLConfig.SetValue(Path+'Style/Italic',fsItalic in Attri.Style);
        XMLConfig.SetValue(Path+'Style/Underline',fsUnderline in Attri.Style);
      end;
    end;
  finally
    OldSyn.Free;
  end;
end;

procedure TEditorOptions.GetHighlighterSettings(Syn: TCustomSyn);
// read highlight settings from config file
begin
  ReadHighlighterSettings(Syn,'');
end;

procedure TEditorOptions.SetHighlighterSettings(Syn: TCustomSyn);
// write highlight settings to config file
begin
  WriteHighlighterSettings(Syn,'');
end;

procedure TEditorOptions.GetSpecialLineColors(Syn: TCustomSyn; 
  AddHilightAttr: TAdditionalHilightAttribute; var FG, BG: TColor);
var i: integer;
begin
  if Syn<>nil then begin
    for i:=0 to Syn.AttrCount-1 do begin
      if Syn.Attribute[i].Name='' then continue;
      if Syn.Attribute[i].Name=AdditionalHighlightAttributes[AddHilightAttr]
      then begin
        FG:=Syn.Attribute[i].Foreground;
        BG:=Syn.Attribute[i].Background;
        exit;
      end;
    end;
  end;
  // set default
  case AddHilightAttr of
  ahaTextBlock:
    begin
      BG:=clNavy;
      FG:=clWhite;
    end;
  ahaExecutionPoint:
    begin
      BG:=clDKGray;
      FG:=clWhite;
    end;
  ahaEnabledBreakpoint:
    begin
      BG:=clRed;
      FG:=clBlack;
    end;
  ahaDisabledBreakpoint:
    begin
      BG:=clGreen;
      FG:=clBlack;
    end;
  ahaInvalidBreakpoint:
    begin
      BG:=clOlive;
      FG:=clGreen;
    end;
  ahaErrorLine:
    begin
      BG:=$50a0ff;
      FG:=clBlack;
    end;
  else
    begin
      BG:=clWhite;
      FG:=clBlack;
    end;
  end;
end;

procedure TEditorOptions.GetSynEditSelectedColor(ASynEdit:TSynEdit);
var i: integer;
begin
  if ASynEdit.Highlighter<>nil then begin
    for i:=0 to ASynEdit.Highlighter.AttrCount-1 do begin
      with ASynEdit.Highlighter.Attribute[i] do begin
        if Name='' then continue;
        if AnsiCompareText(StrToValidXMLName(Name),
          StrToValidXMLName(AdditionalHighlightAttributes[ahaTextBlock]))=0 then
        begin
          ASynEdit.SelectedColor.Background:=Background;
          ASynEdit.SelectedColor.Foreground:=Foreground;
          exit;
        end;
      end;
    end;
  end;
  // set defaults
  ASynEdit.SelectedColor.Background:=clBlue;
  ASynEdit.SelectedColor.Foreground:=clWhite;
end;

procedure TEditorOptions.GetSynEditSettings(ASynEdit:TSynEdit);
// read synedit setings from config file
begin
  // general options
  ASynEdit.Options:=fSynEditOptions;
  ASynEdit.BlockIndent:=fBlockIndent;
  ASynEdit.TabWidth:=fTabWidth;

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
  GetSynEditSelectedColor(ASynEdit);
  
  KeyMap.AssignTo(ASynEdit.KeyStrokes,[caSourceEditor]);
end;

procedure TEditorOptions.SetSynEditSettings(ASynEdit:TSynEdit);
// write synedit settings to file
begin
  // general options
  fSynEditOptions:=ASynEdit.Options;
  fBlockIndent:=ASynEdit.BlockIndent;
  fTabWidth:=ASynEdit.TabWidth;

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

  // XXX: KeyMap

  // XXX:  update all checkboxes, comboboxes...
end;

procedure TEditorOptions.AddSpecialHilightAttribsToHighlighter(Syn: TCustomSyn);
type
  THasSpecialAttribute = array[TAdditionalHilightAttribute] of boolean;
var
  HasSpecialAttribute: THasSpecialAttribute;
  a: TAdditionalHilightAttribute;
  i: integer;
begin
  for a:=Low(TAdditionalHilightAttribute) to High(TAdditionalHilightAttribute)
  do
    HasSpecialAttribute[a]:=false;
  for i:=0 to Syn.AttrCount-1 do begin
    with Syn.Attribute[i] do begin
      if Name='' then continue;
      for a:=Low(TAdditionalHilightAttribute) 
      to High(TAdditionalHilightAttribute)
      do begin
        if AdditionalHighlightAttributes[a]=Name then
          HasSpecialAttribute[a]:=true;
      end;
    end;
  end;
  for a:=Low(TAdditionalHilightAttribute) to High(TAdditionalHilightAttribute)
  do
    if not HasSpecialAttribute[a] then begin
      Syn.AddSpecialAttribute(AdditionalHighlightAttributes[a]);
    end;    
end;

Procedure TEditorOptions.GetSynEditPreviewSettings(APreviewEditor: TObject);
// read synedit setings from config file
var ASynEdit: TSynEdit;
begin
  if not (APreviewEditor is TSynEdit) then exit;
  ASynEdit:=TSynEdit(APreviewEditor);
  
  // general options
  ASynEdit.Options:=fSynEditOptions-[eoDragDropEditing, eoDropFiles,
    eoScrollPastEof]+[eoNoCaret, eoNoSelection];
  ASynEdit.BlockIndent:=fBlockIndent;
  ASynEdit.TabWidth:=fTabWidth;

  // Display options
  ASynEdit.Gutter.Visible:=false;
  ASynEdit.RightEdge:=fRightMargin;
  ASynEdit.RightEdgeColor:=fRightMarginColor;
  ASynEdit.Font.Name:=fEditorFont;
  ASynEdit.Font.Height:=fEditorFontHeight;
  ASynEdit.ExtraLineSpacing:=fExtraLineSpacing;
  ASynEdit.ReadOnly:=true;

  KeyMap.AssignTo(ASynEdit.KeyStrokes,[caSourceEditor]);
end;


{ TColorButton }

constructor TColorButton.Create(AnOwner: TComponent);
begin
  Inherited Create(AnOwner);
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

constructor TEditorOptionsForm.Create(AnOwner:TComponent);

  procedure AddResImg(const ResName: string);
  var Pixmap: TPixmap;
  begin
    Pixmap:=TPixmap.Create;
    Pixmap.TransparentColor:=clWhite;
    Pixmap.LoadFromLazarusResource(ResName);
    ImageList.Add(Pixmap,nil)
  end;

var a:integer;
  s:Ansistring;
begin
  inherited Create(AnOwner);
  FormCreating:=true;

  if LazarusResources.Find(ClassName)=nil then begin  
    Position:=poScreenCenter;
    IDEDialogLayoutList.ApplyLayout(Self,480,459);
    Caption:=dlgEdOptsCap;
    OnResize:=@EditorOptionsFormResize;
    
    SynAutoComplete:=TSynEditAutoComplete.Create(Self);

    MainNoteBook:=TNoteBook.Create(Self);
    with MainNoteBook do begin
      Parent:=Self;
      Top:=0;
      Left:=0;
      Width:=Self.Width;
      Height:=Self.Height-50;
      if PageCount>0 then
        Pages.Strings[0]:=lisMenuInsertGeneral//by VVI - it will solve a problem
      else
        Pages.Add(lisMenuInsertGeneral);
      Pages.Add(dlgEdDisplay);
      Pages.Add(dlgKeyMapping);
      Pages.Add(dlgEdColor);
      Pages.Add('Code Tools'); //by VVI - it seems to be a proper name
    end;
    
    ImageList:=TImageList.Create(Self);
    with ImageList do begin
      Name:='ImageList';
      Width:=22;
      Height:=22;
      AddResImg('keymapcategory');
      AddResImg('keymaprelation');
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
  CurHighlightElement:=nil;

  // initialize previews
  for a:=Low(PreviewEdits) to High(PreviewEdits) do
    PreviewEdits[a]:=nil;
  s:=GetCurColorScheme(TPreviewPasSyn.GetLanguageName);
  PreviewSyn:=GetHighlighter(TPreviewPasSyn,s,true);
  CurLanguageID:=EditorOpts.HighlighterList.FindByClass(
    TCustomSynClass(PreviewSyn.ClassType));

  PreviewEdits[1]:=DisplayPreview;
  PreviewEdits[2]:=ColorPreview;
  PreviewEdits[3]:=CodeTemplateCodePreview;
  for a:=Low(PreviewEdits) to High(PreviewEdits) do begin
    if PreviewEdits[a]<>nil then
      with PreviewEdits[a] do begin
        if EditorOpts.UseSyntaxHighlight then
          Highlighter:=PreviewSyn;
        EditorOpts.GetSynEditSettings(PreviewEdits[a]);
        EditorOpts.KeyMap.AssignTo(PreviewEdits[a].KeyStrokes,[caSourceEditor]);
        if a<>3 then begin
          Lines.Text:=EditorOpts.HighlighterList[CurLanguageID].SampleSource;
          PreviewEdits[a].Options:=PreviewEdits[a].Options
                +[eoNoCaret, eoNoSelection]
                -[eoBracketHighlight];
        end;
      end;
  end;
  CodeTemplateCodePreview.Gutter.Visible:=false;

  // general options
  
  // display options
  
  // key mappings
  FillKeyMappingTreeView;
  
  // color options
  LanguageComboBox.Text:=PreviewSyn.LanguageName;
  SetComboBoxText(LanguageComboBox,LanguageComboBox.Text);
  ColorSchemeComboBox.Text:=GetCurColorScheme(PreviewSyn.LanguageName);
  SetComboBoxText(ColorSchemeComboBox,ColorSchemeComboBox.Text);
  FillColorElementListBox;
  FindCurHighlightElement;
  
  // code Tools options
  with SynAutoComplete do begin
    s:=EditorOpts.CodeTemplateFileName;
    if FileExists(s) then
      try
        AutoCompleteList.LoadFromFile(s);
      except
        writeln('NOTE: unable to read code template file ''',s,'''');
      end;
  end;
  FillCodeTemplateListBox;
  with CodeTemplateListBox do
    if Items.Count>0 then begin
      Selected[0]:=true;
      ShowCurCodeTemplate;
    end;
  if EditorOpts.CodeTemplateIndentToTokenStart then
    CodeTemplateIndentTypeRadioGroup.ItemIndex:=0
  else
    CodeTemplateIndentTypeRadioGroup.ItemIndex:=1;
    
  MainNoteBook.PageIndex:=0;
  FormCreating:=false;
  
  EditorOptionsFormResize(nil);
end;

destructor TEditorOptionsForm.Destroy;
begin
  ClearHighlighters;
  fColorSchemes.Free;
  fFileExtensions.Free;
  inherited Destroy;
end;


// general

procedure TEditorOptionsForm.CodeTemplatesGroupBoxResize(Sender: TObject);
begin
  with CodeTemplateFileNameLabel do begin
    Top:=5;
    Left:=7;
    Width:=110;
  end;

  with CodeTemplateFileNameComboBox do begin
    Top:=3;
    Left:=CodeTemplateFileNameLabel.Left+CodeTemplateFileNameLabel.Width+2;
    Width:=CodeTemplatesGroupBox.Width-12-Left-Height;
  end;

  with CodeTemplateFileNameButton do begin
    Top:=CodeTemplateFileNameComboBox.Top+2;
    Width:=CodeTemplateFileNameComboBox.Height-5;
    Left:=CodeTemplatesGroupBox.Width-9-Width;
    Height:=Width;
  end;

  with CodeTemplateAddButton do begin
    Top:=CodeTemplateFileNameComboBox.Top+CodeTemplateFileNameComboBox.Height+10;
    Width:=50;
    Left:=CodeTemplateFileNameLabel.Left;
    Height:=23;
  end;

  with CodeTemplateEditButton do begin
    Top:=CodeTemplateAddButton.Top+CodeTemplateAddButton.Height+5;
    Left:=CodeTemplateAddButton.Left;
    Width:=CodeTemplateAddButton.Width;
    Height:=CodeTemplateAddButton.Height;
  end;

  with CodeTemplateDeleteButton do begin
    Top:=CodeTemplateEditButton.Top+CodeTemplateEditButton.Height+5;
    Left:=CodeTemplateAddButton.Left;
    Width:=CodeTemplateAddButton.Width;
    Height:=CodeTemplateAddButton.Height;
  end;

  with CodeTemplatesLabel do begin
    Top:=CodeTemplateFileNameLabel.Top+CodeTemplateFileNameLabel.Height+12;
    Left:=CodeTemplateAddButton.Left+CodeTemplateAddButton.Width+5;
    Width:=60;
  end;

  with CodeTemplateListBox do begin
    Top:=CodeTemplatesLabel.Top;
    Left:=CodeTemplatesLabel.Left+CodeTemplatesLabel.Width+5;
    Width:=Parent.ClientWidth-8-Left;
    Height:=80;
  end;

  with CodeTemplateCodeLabel do begin
    Top:=CodeTemplateListBox.Top+CodeTemplateListBox.Height+5;
    Left:=CodeTemplatesLabel.Left;
    Width:=CodeTemplatesLabel.Width;
    Height:=CodeTemplatesLabel.Height;
  end;

  with CodeTemplateCodePreview do begin
    Top:=CodeTemplateCodeLabel.Top;
    Left:=CodeTemplateCodeLabel.Left+CodeTemplateCodeLabel.Width+5;
    Width:=CodeTemplateListBox.Width;
    Height:=CodeTemplatesGroupBox.ClientHeight-Top;
  end;

  with CodeTemplateIndentTypeRadioGroup do begin
    Left:=CodeTemplateAddButton.Left;
    Top:=CodeTemplateCodeLabel.Top+CodeTemplateCodeLabel.Height+15;
    Width:=CodeTemplateCodePreview.Left-Left-8;
    Height:=70;
  end;
end;

procedure TEditorOptionsForm.EditorOptionsFormResize(Sender: TObject);
begin
  with MainNoteBook do begin
    Top:=0;
    Left:=0;
    Width:=Self.Width;
    Height:=Self.Height-50;
  end;

  ResizeGeneralPage;
  ResizeDisplayPage;
  ResizeKeyMappingsPage;
  ResizeColorPage;
  ResizeCodeToolsPage;

  ResizeButtonBar;
end;

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
  if FormCreating then exit;
  {$IFDEF NEW_EDITOR_SYNEDIT}
  // general
  SetOption(AltSetsColumnModeCheckBox,eoAltSetsColumnMode);
  SetOption(AutoIndentCheckBox,eoAutoIndent);
  // not for Preview: SetOption(BracketHighlightCheckBox,eoBracketHighlight);
  SetOption(DoubleClickLineCheckBox,eoDoubleClickSelectsLine);
  SetOption(DragDropEditingCheckBox,eoDragDropEditing);
  SetOption(DropFilesCheckBox,eoDropFiles);
  SetOption(HalfPageScrollCheckBox,eoHalfPageScroll);
  SetOption(KeepCaretXCheckBox,eoKeepCaretX);
  SetOption(PersistentCaretCheckBox,eoPersistentCaret);
  // not for Preview: SetOption(NoSelectionCheckBox,eoNoSelection);
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
          PreviewEdits[a].Highlighter:=PreviewSyn
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
        UpdatingColor:=true;
        if not ForeGroundUseDefaultCheckBox.Checked then
          NewColor:=ForeGroundColorButton.ButtonColor
        else
          NewColor:=clNone;
        ForeGroundColorButton.Visible:=not ForeGroundUseDefaultCheckBox.Checked;
        if NewColor<>CurHighlightElement.Foreground then begin
          CurHighlightElement.Foreground:=NewColor;
          InvalidatePreviews;
        end;
        UpdatingColor:=false;
      end;
    end;
    if Sender=BackGroundUseDefaultCheckBox then begin
      if UpdatingColor=false then begin
        if not BackGroundUseDefaultCheckBox.Checked then
          NewColor:=BackGroundColorButton.ButtonColor
        else
          NewColor:=clNone;
        BackGroundColorButton.Visible:=not BackGroundUseDefaultCheckBox.Checked;
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
  if FormCreating then exit;
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

procedure TEditorOptionsForm.FontDialogNameToFont(FontDialogName:Ansistring;
  AFont:TFont);
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

procedure TEditorOptionsForm.FontDialogApplyClicked(Sender: TObject);
var a: integer;
begin
  for a:=Low(PreviewEdits) to High(PreviewEdits) do begin
    if PreviewEdits[a]<>nil then
      PreviewEdits[a].Font.Assign(TFontDialog(Sender).Font);
  end;
  EditorFontComboBox.Text:=DisplayPreview.Font.Name;
  SetComboBoxText(EditorFontHeightComboBox,
                  IntToStr(DisplayPreview.Font.Height));
end;

procedure TEditorOptionsForm.EditorFontButtonClick(Sender:TObject);
var
  FontDialog:TFontDialog;
begin
  FontDialog:=TFontDialog.Create(Application);
  try
    with FontDialog do begin
      Options:=Options+[fdApplyButton];
      OnApplyClicked:=@FontDialogApplyClicked;
      if Execute then begin
        FontDialogApplyClicked(FontDialog);
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
  if FormCreating then exit;
  Box:=TComboBox(Sender);
  if PreviewEdits[1]<>nil then begin
    // general
    if Sender=BlockIndentComboBox then begin
      NewVal:=StrToIntDef(BlockIndentComboBox.Text
        ,PreviewEdits[1].BlockIndent);
      SetComboBoxText(BlockIndentComboBox,IntToStr(NewVal));
      for a:=Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a]<>nil then
          PreviewEdits[a].BlockIndent:=NewVal;
    end
    else if Sender=TabWidthsComboBox then begin
      NewVal:=StrToIntDef(TabWidthsComboBox.Text
        ,PreviewEdits[1].TabWidth);
      SetComboBoxText(TabWidthsComboBox,IntToStr(NewVal));
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
        // unknown color scheme -> switch back
        SetComboBoxText(Box,GetCurColorScheme(PreviewSyn.LanguageName));
      end else begin
        // change the colorscheme
        if Box.Text<>GetCurColorScheme(PreviewSyn.LanguageName) then begin
          SetCurColorScheme(PreviewSyn.LanguageName,Box.Text);
          SetComboBoxText(Box,Box.Text);
          PreviewSyn:=GetHighlighter(TCustomSynClass(PreviewSyn.ClassType),
                                     Box.Text,true);
          SetPreviewSynInAllPreviews;
          FillColorElementListBox;
          FindCurHighlightElement;
        end;
      end;
    end else if Sender=FileExtensionsComboBox then begin
      if Box.Text<>GetCurFileExtension(PreviewSyn.LanguageName) then begin
        SetCurFileExtension(PreviewSyn.LanguageName,Box.Text);
        SetComboBoxText(Box,Box.Text);
      end;
    end else if Sender=LanguageComboBox then begin
      if Box.Items.IndexOf(Box.Text)<0 then begin
        // unknown language -> switch back
        SetComboBoxText(Box,PreviewSyn.LanguageName);
      end else begin
        // change language
        if Box.Text<>PreviewSyn.LanguageName then begin
          NewVal:=EditorOpts.HighlighterList.FindByName(Box.Text);
          if NewVal>=0 then begin
            SetComboBoxText(Box,Box.Text);
            CurLanguageID:=NewVal;
            PreviewSyn:=GetHighlighter(
              EditorOpts.HighlighterList[CurLanguageID].SynClass,
              GetCurColorScheme(
              EditorOpts.HighlighterList[CurLanguageID].SynClass.GetLanguageName)
              ,true);
            SetComboBoxText(ColorSchemeComboBox,
                            GetCurColorScheme(PreviewSyn.LanguageName));
            SetComboBoxText(FileExtensionsComboBox,
                            GetCurFileExtension(PreviewSyn.LanguageName));
            for a:=Low(PreviewEdits) to High(PreviewEdits) do
              if a<>3 then
                PreviewEdits[a].Lines.Text:=
                  EditorOpts.HighlighterList[CurLanguageID].SampleSource;
            SetPreviewSynInAllPreviews;
            FillColorElementListBox;
            FindCurHighlightElement;
          end;
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
var a, i:integer;
  Old:TSynHighlightElement;
begin
  Old:=CurHighlightElement;
  CurHighlightElement:=nil;
  a:=0;
  while a<ColorElementListBox.Items.Count do begin
    if ColorElementListBox.Selected[a] then begin
      i:=PreviewSyn.AttrCount-1;
      while (i>=0) do begin
        if ColorElementListBox.Items[a]=PreviewSyn.Attribute[i].Name then
          break;
        dec(i);
      end;
      if i>=0 then begin
        CurHighlightElement:=PreviewSyn.Attribute[i];
        break;
      end;
    end;
    inc(a);
  end;
  if (CurHighlightElement=nil) and (ColorElementListBox.Items.Count>0) then 
  begin
    // none selected -> select one
    ColorElementListBox.Selected[0]:=true;
    i:=PreviewSyn.AttrCount-1;
    while (i>=0) do begin
      if ColorElementListBox.Items[0]=PreviewSyn.Attribute[i].Name then
        break;
      dec(i);
    end;
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

procedure TEditorOptionsForm.SetPreviewSynInAllPreviews;
var a:integer;
begin
  if EditorOpts.UseSyntaxHighlight then
    for a:=Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a]<>nil then
        PreviewEdits[a].Highlighter:=PreviewSyn;
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
  ForeGroundColorButton.Visible:=not ForeGroundUseDefaultCheckBox.Checked;
  if CurHighlightElement.Background=clNone then begin
    BackGroundUseDefaultCheckBox.Checked:=true;
  end else begin
    BackGroundUseDefaultCheckBox.Checked:=false;
    BackGroundColorButton.ButtonColor:=CurHighlightElement.Background;
  end;
  BackGroundColorButton.Visible:=
    not BackGroundUseDefaultCheckBox.Checked;
  UpdatingColor:=false;
end;

procedure TEditorOptionsForm.KeyMappingTreeViewMouseUp(Sender:TObject;
  Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
var i:integer;
  ARelation: TKeyCommandRelation;
  ANode: TTreeNode;
begin
  ANode:=KeyMappingTreeView.GetNodeAt(X,Y);
  if (ANode<>nil) and (ANode.Data<>nil)
  and (TObject(ANode.Data) is TKeyCommandRelation) then begin
    ARelation:=TKeyCommandRelation(ANode.Data);
    i:=EditorOpts.KeyMap.IndexOf(ARelation);
    if (i>=0)
    and (ShowKeyMappingEditForm(i,EditorOpts.KeyMap)=mrOk) then begin
      ANode.Text:=KeyMappingRelationToString(ARelation);
      for i:=Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[i]<>nil then
          EditorOpts.KeyMap.AssignTo(PreviewEdits[i].KeyStrokes,[caSourceEditor]);
    end;
  end;
end;

type
  TKeyMapErrorsForm = class(TForm)
    ListBox: TListBox;
    BackButton: TButton;
    procedure BackButtonClick(Sender: TObject);
  public
    constructor Create(AnOwner: TComponent); override;
  end;

constructor TKeyMapErrorsForm.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    SetBounds((Screen.Width-410) div 2,(Screen.Height-260) div 2, 400,250);
    Caption:=dlgKeyMappingErrors;

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
      Caption:=dlgEdBack;
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
      ACaption:=dlgReport;
      AText:=dlgEdNoErr;
      MessageDlg(ACaption,AText,mtInformation,[mbOk],0);
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

procedure TEditorOptionsForm.FillColorElementListBox;
var i: integer;
begin
  with ColorElementListBox.Items do begin
    BeginUpdate;
    Clear;
    for i:=0 to PreviewSyn.AttrCount-1 do
      if PreviewSyn.Attribute[i].Name<>'' then
        Add(PreviewSyn.Attribute[i].Name);
    EndUpdate;
  end;
  if ColorElementListBox.Items.Count>0 then begin
    ColorElementListBox.Selected[0]:=true;
    CurHighlightElement:=PreviewSyn.Attribute[0];
  end else
    CurHighlightElement:=nil;
end;

procedure TEditorOptionsForm.ColorPreviewMouseUp(Sender:TObject;
  Button:TMouseButton;  Shift:TShiftState;  X,Y:integer);
var NewIndex: integer;
  Token: ansistring;
  Attri: TSynHighlightElement;
  MouseXY, XY: TPoint;
  AddAttr: TAdditionalHilightAttribute;
begin
  MouseXY:=Point(X,Y);
  XY:=ColorPreview.PixelsToRowColumn(MouseXY);
  NewIndex:=-1;
  if CurLanguageID>=0 then begin
    AddAttr:=EditorOpts.HighlighterList[CurLanguageID].SampleLineToAddAttr(XY.Y);
    if AddAttr<>ahaNone then 
      NewIndex:=ColorElementListBox.Items.IndexOf(
                           AdditionalHighlightAttributes[AddAttr]);
  end;
  if NewIndex<0 then begin
    ColorPreview.GetHighlighterAttriAtRowCol(XY,Token,Attri);
    if Attri=nil then
      Attri:=PreviewSyn.WhitespaceAttribute;
    if Attri<>nil then
      NewIndex:=ColorElementListBox.Items.IndexOf(Attri.Name);
  end;
  if NewIndex>=0 then begin
    ColorElementListBox.ItemIndex:=NewIndex;
    FindCurHighlightElement;
    ShowCurAttribute;
  end;
end;

procedure TEditorOptionsForm.OnSpecialLineColors(Sender: TObject;
  Line: integer;  var Special: boolean; var FG, BG: TColor);
var e:TSynHighlightElement;
  AddAttr: TAdditionalHilightAttribute;
  i: integer;
begin
  if CurLanguageID>=0 then begin
    AddAttr:=EditorOpts.HighlighterList[CurLanguageID].SampleLineToAddAttr(Line);
    if AddAttr<>ahaNone then begin
      i:=PreviewSyn.AttrCount-1;
      while (i>=0) do begin
        e:=PreviewSyn.Attribute[i];
        if e.Name='' then continue;
        if e.Name=AdditionalHighlightAttributes[AddAttr] then begin
          Special:=true;
          FG:=e.ForeGround;
          BG:=e.BackGround;
          exit;
        end;
        dec(i);
      end;
    end;
  end;
end;

procedure TEditorOptionsForm.SetAttributeToDefaultButtonClick(Sender: TObject);
begin
  SetColorElementsToDefaults(true);  
end;

procedure TEditorOptionsForm.SetAllAttributesToDefaultButtonClick(
  Sender: TObject);
begin
  SetColorElementsToDefaults(false);  
end;

procedure TEditorOptionsForm.SetColorElementsToDefaults(OnlySelected: boolean);
var DefaultSyn: TCustomSyn;
  PascalSyn: TPreviewPasSyn;
  i, j: integer;
  CurSynClass: TCustomSynClass;
begin
  PascalSyn:=TPreviewPasSyn(GetHighlighter(TPreviewPasSyn,
                            ColorSchemeComboBox.Text,true));
  CurSynClass:=TCustomSynClass(PreviewSyn.ClassType);
  DefaultSyn:=CurSynClass.Create(nil);
  try
    EditorOpts.AddSpecialHilightAttribsToHighlighter(DefaultSyn);
    EditorOpts.ReadDefaultsForHighlighterSettings(DefaultSyn,
      ColorSchemeComboBox.Text,PascalSyn);
    for i:=0 to DefaultSyn.AttrCount-1 do begin
      if DefaultSyn.Attribute[i].Name='' then continue;
      if OnlySelected then begin
        if (DefaultSyn.Attribute[i].Name=CurHighlightElement.Name) then begin
          CopyHiLightAttributeValues(DefaultSyn.Attribute[i],
                                     CurHighlightElement);
        end;
      end else begin
        for j:=0 to PreviewSyn.AttrCount-1 do
          if PreviewSyn.Attribute[j].Name=DefaultSyn.Attribute[i].Name then 
          begin
            CopyHiLightAttributeValues(DefaultSyn.Attribute[i],
                                       PreviewSyn.Attribute[j]);
          end;
      end;
    end;
  finally
    DefaultSyn.Free;
  end;
  ShowCurAttribute;
end;

function TEditorOptionsForm.GetCurColorScheme(
  const LanguageName: string): string;
begin
  if fColorSchemes=nil then
    Result:=''
  else
    Result:=fColorSchemes.Values[LanguageName];
  if Result='' then
    Result:=EditorOpts.ReadColorScheme(LanguageName);
end;

procedure TEditorOptionsForm.SetCurColorScheme(
  const LanguageName, ColorScheme: string);
begin
  if fColorSchemes=nil then fColorSchemes:=TStringList.Create;
  fColorSchemes.Values[LanguageName]:=ColorScheme;
end;

procedure TEditorOptionsForm.SaveAllColorSchemes;
var i: integer;
begin
  if fColorSchemes=nil then exit;
  for i:=0 to fColorSchemes.Count-1 do
    EditorOpts.WriteColorScheme(fColorSchemes.Names[i],
      fColorSchemes.Values[fColorSchemes.Names[i]]);
end;

function TEditorOptionsForm.GetCurFileExtension(
  const LanguageName: string): string;
var i: integer;
begin
  if fFileExtensions=nil then
    Result:=''
  else
    Result:=fFileExtensions.Values[LanguageName];
  if Result='' then begin
    i:=EditorOpts.HighlighterList.FindByName(LanguageName);
    if i>=0 then
      Result:=EditorOpts.HighlighterList[i].FileExtensions;
  end;
end;

procedure TEditorOptionsForm.SetCurFileExtension(
  const LanguageName, FileExtensions: string);
begin
  if fFileExtensions=nil then fFileExtensions:=TStringList.Create;
  fFileExtensions.Values[LanguageName]:=FileExtensions;
end;

procedure TEditorOptionsForm.SaveAllFileExtensions;
var i, j: integer;
begin
  if fFileExtensions=nil then exit;
  for i:=0 to fFileExtensions.Count-1 do begin
    j:=EditorOpts.HighlighterList.FindByName(fFileExtensions.Names[i]);
    if j>=0 then
      EditorOpts.HighlighterList[i].FileExtensions:=
        fFileExtensions.Values[fFileExtensions.Names[i]];
  end;
end;

function TEditorOptionsForm.GetHighlighter(SynClass: TCustomSynClass;
  const ColorScheme: string; CreateIfNotExists: boolean): TCustomSyn;
var i: integer;
begin
  if fHighlighterList=nil then fHighlighterList:=TStringList.Create;
  for i:=0 to fHighlighterList.Count-1 do begin
    if (fHighlighterList[i]=ColorScheme) 
    and (TCustomSynClass(TCustomSyn(fHighlighterList.Objects[i]).ClassType)
      =SynClass)
    then begin
      Result:=TCustomSyn(fHighlighterList.Objects[i]);
      exit;
    end;
  end;
  if CreateIfNotExists then begin
    Result:=SynClass.Create(nil);
    EditorOpts.AddSpecialHilightAttribsToHighlighter(Result);
    fHighlighterList.AddObject(ColorScheme,Result);
    EditorOpts.ReadHighlighterSettings(Result,ColorScheme);
  end;
end;

procedure TEditorOptionsForm.ClearHighlighters;
var i: integer;
begin
  if fHighlighterList=nil then exit;
  for i:=0 to fHighlighterList.Count-1 do
    TCustomSyn(fHighlighterList.Objects[i]).Free;
  fHighlighterList.Free;
end;

procedure TEditorOptionsForm.SaveAllHighlighters;
var i: integer;
  Syn: TCustomSyn;
begin
  if fHighlighterList=nil then exit;
  for i:=0 to fHighlighterList.Count-1 do begin
    Syn:=TCustomSyn(fHighlighterList.Objects[i]);
    EditorOpts.WriteHighlighterSettings(Syn,fHighlighterList[i]);
  end;
end;

// keymapping ------------------------------------------------------------------

function TEditorOptionsForm.KeyMappingRelationToString(
  Index:integer):String;
begin
  Result:=KeyMappingRelationToString(EditorOpts.KeyMap.Relations[Index]);
end;

function TEditorOptionsForm.KeyMappingRelationToString(
  KeyRelation: TKeyCommandRelation): String;
var s:AnsiString;
begin
  with KeyRelation do begin
    Result:=copy(Name,1,37);
    if length(Result)<37 then begin
      SetLength(s,(37-length(Result)));
      FillChar(s[1],length(s),' ');
    end else
      s:='';
    Result:=Result+s;
    if (Key1=VK_UNKNOWN) and (Key2=VK_UNKNOWN) then
      Result:=Result+'none'
    else if (Key2=VK_UNKNOWN) then
      Result:=Result+KeyAndShiftStateToStr(Key1,Shift1)
    else
      Result:=Result+KeyAndShiftStateToStr(Key1,Shift1)+'  or  '+
           KeyAndShiftStateToStr(Key2,Shift2);
    Result:=Result;
  end;
end;

procedure TEditorOptionsForm.FillKeyMappingTreeView;
var i, j: integer;
  NewCategoryNode, NewKeyNode: TTreeNode;
  CurCategory: TKeyCommandCategory;
  CurKeyRelation: TKeyCommandRelation;
begin
  with KeyMappingTreeView do begin
    BeginUpdate;
    Items.Clear;
    for i:=0 to EditorOpts.KeyMap.CategoryCount-1 do begin
      CurCategory:=EditorOpts.KeyMap.Categories[i];
      NewCategoryNode:=Items.AddObject(nil,CurCategory.Description,CurCategory);
      NewCategoryNode.ImageIndex:=0;
      NewCategoryNode.SelectedIndex:=NewCategoryNode.ImageIndex;
      for j:=0 to CurCategory.Count-1 do begin
        CurKeyRelation:=TKeyCommandRelation(CurCategory[j]);
        NewKeyNode:=Items.AddChildObject(NewCategoryNode,
          KeyMappingRelationToString(CurKeyRelation),CurKeyRelation);
        NewKeyNode.ImageIndex:=1;
        NewKeyNode.SelectedIndex:=NewKeyNode.ImageIndex;
      end;
      //NewCategoryNode.Expanded:=true;
    end;
    EndUpdate;
  end;
end;

// code tools

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
      if (ep>sp) or ((s<>'') and (s[length(s)] in [#10,#13])) then
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
  l: integer;
begin
  if CurCodeTemplate<0 then exit;
  NewValue:=CodeTemplateCodePreview.Lines.Text;
  // remove last end EOL
  if NewValue<>'' then begin
    l:=length(NewValue);
    if NewValue[l] in [#10,#13] then begin
      dec(l);
      if (l>0) and (NewValue[l] in [#10,#13])
      and (NewValue[l]<>NewValue[l+1]) then
        dec(l);
      SetLength(NewValue,l);
    end;
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
      if (Index>=0) and (Index<CodeTemplateListBox.Items.Count) then begin
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
      if MessageDlg(dlgDelTemplate
          +'"'+SynAutoComplete.Completions[CurCodeTemplate]+' - '
          +SynAutoComplete.CompletionComments[CurCodeTemplate]+'"'
          +'?',mtConfirmation,[mbOk,mbCancel],0)=mrOK then begin
        SynAutoComplete.DeleteCompletion(CurCodeTemplate);
        dec(CurCodeTemplate);
        FillCodeTemplateListBox;
        if (CurCodeTemplate>=0) 
        and (CurCodeTemplate<CodeTemplateListBox.Items.Count) then begin
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
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    with OpenDialog do begin
      Title:=dlgChsCodeTempl;
      Filter:='DCI file (*.dci)|*.dci|'+dlgAllFiles+'|*.*';
      if Execute then
        CodeTemplateFileNameComboBox.Text:=FileName;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

// useful functions

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
  MaxX:=Width-5;
  ChkBoxW:=(MaxX-20) div 2;

  EditorOptionsGroupBox:=TGroupBox.Create(Self);
  with EditorOptionsGroupBox do begin
    Name:='EditorOptionsGroupBox';
    Parent:=MainNoteBook.Page[0];
    Top:=5;
    Left:=5;
    Width:=MaxX-10;
    Height:=24*10;
    Caption:=dlgEdOptsCap;
    Show;
  end;

  // many, many checkboxes ...

  // left side
  AltSetsColumnModeCheckBox:=TCheckBox.Create(Self);
  with AltSetsColumnModeCheckBox do begin
    Name:='AltSetsColumnModeCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=5;
    Left:=5;
    Width:=ChkBoxW;
    Height:=16;
    Caption:=dlgAltSetClMode;
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
    Caption:=dlgAutoIdent;
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
    Caption:=dlgBracHighlight;
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
    Caption:=dlgDragDropEd;
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
    Caption:=dlgDropFiles;
    Checked:=eoDropFiles in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Enabled:=false;
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
    Caption:=dlgHalfPageScroll;
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
    Caption:=dlgKeepCaretX;
    Checked:=eoKeepCaretX in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  PersistentCaretCheckBox:=TCheckBox.Create(Self);
  with PersistentCaretCheckBox do begin
    Name:='PersistentCaretCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=KeepCaretXCheckBox.Top+KeepCaretXCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:=dlgPersistentCaret;
    Checked:=eoPersistentCaret in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  ScrollByOneLessCheckBox:=TCheckBox.Create(Self);
  with ScrollByOneLessCheckBox do begin
    Name:='ScrollByOneLessCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=PersistentCaretCheckBox.Top+PersistentCaretCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:=dlgScrollByOneLess;
    Checked:=eoScrollByOneLess in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  ScrollPastEoFCheckBox:=TCheckBox.Create(Self);
  with ScrollPastEoFCheckBox do begin
    Name:='ScrollPastEoFCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=ScrollByOneLessCheckBox.Top+ScrollByOneLessCheckBox.Height+5;
    Left:=ScrollByOneLessCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:=dlgScrollPastEndFile;
    Checked:=eoScrollPastEoF in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  // right side
  ScrollPastEoLCheckBox:=TCheckBox.Create(Self);
  with ScrollPastEoLCheckBox do begin
    Name:='ScrollPastEoLCheckBox';
    Parent:=EditorOptionsGroupBox;
    Left:=AltSetsColumnModeCheckBox.Left+(MaxX div 2)+5;
    Left:=ScrollPastEoFCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:=dlgScrollPastEndLine;
    Checked:=eoScrollPastEoL in EditorOpts.SynEditOptions;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  ShowCloseBtnInNoteBookCheckBox:=TCheckBox.Create(Self);
  with ShowCloseBtnInNoteBookCheckBox do begin
    Name:='ShowCloseBtnInNoteBookCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=ScrollPastEoLCheckBox.Top+ScrollPastEoLCheckBox.Height+5;
    Left:=ScrollPastEoLCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:=dlgCloseButtonsNotebook;
    Checked:=EditorOpts.ShowTabCloseButtons;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  ShowScrollHintCheckBox:=TCheckBox.Create(Self);
  with ShowScrollHintCheckBox do begin
    Name:='ShowScrollHintCheckBox';
    Parent:=EditorOptionsGroupBox;
    Top:=ShowCloseBtnInNoteBookCheckBox.Top
         +ShowCloseBtnInNoteBookCheckBox.Height+5;
    Left:=ShowCloseBtnInNoteBookCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
    Caption:=dlgShowScrollHint;
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
    Caption:=dlgSmartTabs;
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
    Caption:=dlgTabsToSpaces;
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
    Caption:=dlgTrimTrailingSpaces ;
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
    Caption:=dlgUndoAfterSave;
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
    Caption:=dlgDoubleClickLine;
    Checked:=eoDoubleClickSelectsLine in EditorOpts.SynEditOptions;
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
    Caption:=dlgFindTextatCursor;
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
    Caption:=dlgUseSyntaxHighlight;
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
    Caption:=dlgBlockIndent;
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
    Caption:=dlgUndoLimit;
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
    SetComboBoxText(TabWidthsComboBox,IntToStr(EditorOpts.TabWidth));
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
end;

procedure TEditorOptionsForm.ResizeGeneralPage;
var MaxX,ChkBoxW:integer;
begin
  MaxX:=Width-5;
  ChkBoxW:=(MaxX-20) div 2;

  with EditorOptionsGroupBox do begin
    Top:=5;
    Left:=5;
    Width:=MaxX-10;
    Height:=24*10;
  end;

  // many, many checkboxes ...

  with AltSetsColumnModeCheckBox do begin
    Top:=5;
    Left:=5;
    Width:=ChkBoxW;
    Height:=16;
  end;

  with AutoIndentCheckBox do begin
    Top:=AltSetsColumnModeCheckBox.Top+AltSetsColumnModeCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with BracketHighlightCheckBox do begin
    Top:=AutoIndentCheckBox.Top+AutoIndentCheckBox.Height+5;
    Left:=AutoIndentCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with DragDropEditingCheckBox do begin
    Top:=BracketHighlightCheckBox.Top+BracketHighlightCheckBox.Height+5;
    Left:=BracketHighlightCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with DropFilesCheckBox do begin
    Top:=DragDropEditingCheckBox.Top+DragDropEditingCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with HalfPageScrollCheckBox do begin
    Top:=DropFilesCheckBox.Top+DropFilesCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with KeepCaretXCheckBox do begin
    Top:=HalfPageScrollCheckBox.Top+HalfPageScrollCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with PersistentCaretCheckBox do begin
    Left:=AltSetsColumnModeCheckBox.Left;
    Top:=KeepCaretXCheckBox.Top+KeepCaretXCheckBox.Height+5;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with ScrollByOneLessCheckBox do begin
    Top:=PersistentCaretCheckBox.Top+PersistentCaretCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with ScrollPastEoFCheckBox do begin
    Top:=ScrollByOneLessCheckBox.Top+ScrollByOneLessCheckBox.Height+5;
    Left:=AltSetsColumnModeCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with ScrollPastEoLCheckBox do begin
    Top:=5;
    Left:=AltSetsColumnModeCheckBox.Left+(MaxX div 2)+5;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with ShowCloseBtnInNoteBookCheckBox do begin
    Top:=ScrollPastEoLCheckBox.Top+ScrollPastEoLCheckBox.Height+5;
    Left:=ScrollPastEoLCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with ShowScrollHintCheckBox do begin
    Top:=ShowCloseBtnInNoteBookCheckBox.Top
        +ShowCloseBtnInNoteBookCheckBox.Height+5;
    Left:=ShowCloseBtnInNoteBookCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with SmartTabsCheckBox do begin
    Top:=ShowScrollHintCheckBox.Top+ShowScrollHintCheckBox.Height+5;
    Left:=ShowScrollHintCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with TabsToSpacesCheckBox do begin
    Top:=SmartTabsCheckBox.Top+SmartTabsCheckBox.Height+5;
    Left:=ShowScrollHintCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with TrimTrailingSpacesCheckBox do begin
    Top:=TabsToSpacesCheckBox.Top+TabsToSpacesCheckBox.Height+5;
    Left:=ShowScrollHintCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with UndoAfterSaveCheckBox do begin
    Top:=TrimTrailingSpacesCheckBox.Top+TrimTrailingSpacesCheckBox.Height+5;
    Left:=ShowScrollHintCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with DoubleClickLineCheckBox do begin
    Top:=UndoAfterSaveCheckBox.Top+UndoAfterSaveCheckBox.Height+5;
    Left:=ShowScrollHintCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with FindTextAtCursorCheckBox do begin
    Top:=DoubleClickLineCheckBox.Top+DoubleClickLineCheckBox.Height+5;
    Left:=ShowScrollHintCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  with UseSyntaxHighlightCheckBox do begin
    Top:=FindTextAtCursorCheckBox.Top+FindTextAtCursorCheckBox.Height+5;
    Left:=ShowScrollHintCheckBox.Left;
    Width:=ChkBoxW;
    Height:=AltSetsColumnModeCheckBox.Height;
  end;

  //

  with BlockIndentComboBox do begin
    Top:=EditorOptionsGroupBox.Top+EditorOptionsGroupBox.Height+8;
    Left:=120;
    Width:=70;
  end;

  with BlockIndentLabel do begin
    Top:=BlockIndentComboBox.Top+2;
    Left:=EditorOptionsGroupBox.Left+2;
    Width:=BlockIndentComboBox.Left-2-Left;
  end;

  with UndoLimitComboBox do begin
    Top:=BlockIndentComboBox.Top+BlockIndentComboBox.Height+5;
    Left:=BlockIndentComboBox.Left;
    Width:=70;
  end;

  with UndoLimitLabel do begin
    Top:=UndoLimitComboBox.Top+2;
    Left:=EditorOptionsGroupBox.Left+2;
    Width:=UndoLimitComboBox.Left-Left-2;
  end;

  with TabWidthsComboBox do begin
    Top:=UndoLimitComboBox.Top+UndoLimitComboBox.Height+5;
    Left:=BlockIndentComboBox.Left;
    Width:=70;
  end;

  with TabWidthsLabel do begin
    Top:=TabWidthsComboBox.Top+2;
    Left:=EditorOptionsGroupBox.Left+2;
    Width:=TabWidthsComboBox.Left-Left-2;
  end;
end;

procedure TEditorOptionsForm.SetupDisplayPage;
var MaxX,MaxY,ChkBoxW:integer;
begin
  MaxX:=Width-5;
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
    Caption:=dlgMarginGutter;
    Show;
  end;

  VisibleRightMarginCheckBox:=TCheckBox.Create(Self);
  with VisibleRightMarginCheckBox do begin
    Name:='VisibleRightMarginCheckBox';
    Parent:=MarginAndGutterGroupBox;
    Top:=5;
    Left:=5;
    Width:=ChkBoxW;
    Caption:=dlgVisibleRightMargin ;
    Height:=23;
    Checked:=EditorOpts.VisibleRightMargin;
    OnClick:=@GeneralCheckBoxOnClick;
    Enabled:=false;
    Show;
  end;

  VisibleGutterCheckBox:=TCheckBox.Create(Self);
  with VisibleGutterCheckBox do begin
    Name:='VisibleGutterCheckBox';
    Parent:=MarginAndGutterGroupBox;
    Top:=VisibleRightMarginCheckBox.Top+VisibleRightMarginCheckBox.Height+7;
    Left:=VisibleRightMarginCheckBox.Left;
    Width:=ChkBoxW;
    Height:=VisibleRightMarginCheckBox.Height;
    Caption:=dlgVisibleGutter;
    Checked:=EditorOpts.VisibleGutter;
    OnClick:=@GeneralCheckBoxOnClick;
    Show;
  end;

  ShowLineNumbersCheckBox:=TCheckBox.Create(Self);
  with ShowLineNumbersCheckBox do begin
    Name:='ShowLineNumbersCheckBox';
    Parent:=MarginAndGutterGroupBox;
    Top:=VisibleGutterCheckBox.Top+VisibleGutterCheckBox.Height+7;
    Left:=VisibleGutterCheckBox.Left;
    Width:=ChkBoxW;
    Height:=VisibleRightMarginCheckBox.Height;
    Caption:=dlgShowLineNumbers ;
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
    Width:=150;
    Caption:=dlgRightMargin ;
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
    Width:=150;
    Caption:=dlgRightMarginColor ;
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
    Width:=130;
    Caption:=dlgGutterWidth ;
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
    Width:=130;
    Caption:=dlgGutterColor ;
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
    Width:=130;
    Caption:=dlgEditorFont ;
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
    Width:=150;
    Caption:=dlgEditorFontHeight ;
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
    Width:=150;
    Caption:=dlgExtraLineSpacing ;
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
    OnSpecialLineColors:=@Self.OnSpecialLineColors;
    ReadOnly:=true;
    Show;
  end;
end;

procedure TEditorOptionsForm.ResizeDisplayPage;
var MaxX,MaxY,ChkBoxW:integer;
begin
  MaxX:=Width-5;
  MaxY:=ClientHeight-80;
  ChkBoxW:=140;

  with MarginAndGutterGroupBox do begin
    Top:=5;
    Left:=5;
    Width:=MaxX-10;
    Height:=109;
  end;

  with VisibleRightMarginCheckBox do begin
    Top:=5;
    Left:=5;
    Width:=ChkBoxW;
  end;

  with VisibleGutterCheckBox do begin
    Top:=VisibleRightMarginCheckBox.Top+VisibleRightMarginCheckBox.Height+7;
    Left:=VisibleRightMarginCheckBox.Left;
    Width:=ChkBoxW;
    Height:=VisibleRightMarginCheckBox.Height;
  end;

  with ShowLineNumbersCheckBox do begin
    Top:=VisibleGutterCheckBox.Top+VisibleGutterCheckBox.Height+7;
    Left:=VisibleGutterCheckBox.Left;
    Width:=ChkBoxW;
    Height:=VisibleRightMarginCheckBox.Height;
  end;

  with RightMarginComboBox do begin
    Top:=20;
    Left:=180;
    Width:=70;
  end;

  with RightMarginLabel do begin
    Top:=2;
    Left:=RightMarginComboBox.Left+2;
    Width:=150;
  end;

  with RightMarginColorButton do begin
    Top:=RightMarginComboBox.Top+RightMarginComboBox.Height+20;
    Left:=RightMarginComboBox.Left;
    Width:=35;
    Height:=20;
  end;

  with RightMarginColorLabel do begin
    Top:=RightMarginComboBox.Top+RightMarginComboBox.Height;
    Left:=RightMarginComboBox.Left+2;
    Width:=150;
  end;

  with GutterWidthComboBox do begin
    Top:=RightMarginComboBox.Top;
    Left:=RightMarginComboBox.Left+RightMarginComboBox.Width+80;
    Width:=RightMarginComboBox.Width;
    Height:=RightMarginComboBox.Height;
  end;

  with GutterWidthLabel do begin
    Top:=2;
    Left:=GutterWidthComboBox.Left+2;
    Width:=130;
  end;

  with GutterColorButton do begin
    Top:=GutterWidthComboBox.Top+GutterWidthComboBox.Height+20;
    Left:=GutterWidthComboBox.Left;
    Width:=35;
    Height:=20;
  end;

  with GutterColorLabel do begin
    Top:=GutterWidthComboBox.Top+GutterWidthComboBox.Height;
    Left:=GutterWidthComboBox.Left+2;
    Width:=130;
  end;

  with EditorFontGroupBox do begin
    Top:=MarginAndGutterGroupBox.Left+MarginAndGutterGroupBox.Height+5;
    Left:=MarginAndGutterGroupBox.Left;
    Width:=MarginAndGutterGroupBox.Width;
    Height:=120;
  end;

  with EditorFontComboBox do begin
    Top:=23;
    Left:=5;
    Width:=EditorFontGroupBox.Width-15-Height;
  end;

  with EditorFontButton do begin
    Top:=EditorFontComboBox.Top+2;
    Left:=EditorFontComboBox.Left+EditorFontComboBox.Width+3;
    Width:=EditorFontComboBox.Height-5;
    Height:=Width;
  end;

  with EditorFontLabel do begin
    Top:=5;
    Left:=EditorFontComboBox.Left+2;
    Width:=130;
  end;

  with EditorFontHeightComboBox do begin
    Top:=EditorFontComboBox.Top+EditorFontComboBox.Height+23;
    Left:=EditorFontComboBox.Left;
    Width:=60;
  end;

  with EditorFontHeightLabel do begin
    Top:=EditorFontHeightComboBox.Top-18;
    Left:=EditorFontHeightComboBox.Left+2;
    Width:=150;
  end;

  with ExtraLineSpacingComboBox do begin
    Top:=EditorFontHeightComboBox.Top;
    Left:=EditorFontHeightComboBox.Left+EditorFontHeightComboBox.Width+100;
    Width:=60;
  end;

  with ExtraLineSpacingLabel do begin
    Top:=ExtraLineSpacingComboBox.Top-18;
    Left:=ExtraLineSpacingComboBox.Left+2;
    Width:=150;
  end;

  with DisplayPreview do begin
    Top:=EditorFontGroupBox.Top+EditorFontGroupBox.Height+5;
    Left:=EditorFontGroupBox.Left+2;
    Width:=EditorFontGroupBox.Width-2;
    Height:=MaxY-Top-2;
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
    Visible:=true;
  end;

  KeyMappingSchemeLabel:=TLabel.Create(Self);
  with KeyMappingSchemeLabel do begin
    Name:='KeyMappingSchemeLabel';
    Parent:=MainNoteBook.Page[2];
    Top:=5;
    Left:=5;
    Width:=KeyMappingSchemeComboBox.Left-Left;
    Height:=16;
    Caption:=dlgKeyMappingScheme ;
    Visible:=true;
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
    Caption:=dlgCheckConsistency ;
    OnClick:=@KeyMappingConsistencyCheckButtonClick;
    Visible:=true;
  end;

  KeyMappingHelpLabel:=TLabel.Create(Self);
  with KeyMappingHelpLabel do begin
    Name:='KeyMappingHelpLabel';
    Parent:=MainNoteBook.Page[2];
    Top:=KeyMappingSchemeComboBox.Top+KeepCaretXCheckBox.Height+10;
    Left:=5;
    Width:=MaxX-Left-Left;
    Height:=16;
    Caption:=dlgEdHintCommand ;
    Visible:=true;
  end;

  KeyMappingTreeView:=TTreeView.Create(Self);
  with KeyMappingTreeView do begin
    Name:='KeyMappingTreeView';
    Parent:=MainNoteBook.Page[2];
    Top:=KeyMappingHelpLabel.Top+KeyMappingHelpLabel.Height+2;
    Left:=0;
    Width:=MaxX-Left-Left;
    Height:=MaxY-Top;
    Options:=[tvoReadOnly, tvoShowButtons, tvoShowRoot,
      tvoShowLines, tvoRowSelect, tvoKeepCollapsedNodes, tvoShowSeparators];
    OnMouseUp:=@KeyMappingTreeViewMouseUp;
    Images:=Self.ImageList;
    Visible:=true;
  end;
end;

procedure TEditorOptionsForm.ResizeKeyMappingsPage;
var MaxX,MaxY:integer;
begin
  MaxX:=Width-9;
  MaxY:=ClientHeight-82;

  with KeyMappingSchemeComboBox do begin
    Top:=5;
    Left:=170;
    Width:=100;
    Height:=16;
  end;

  with KeyMappingSchemeLabel do begin
    Top:=5;
    Left:=5;
    Width:=KeyMappingSchemeComboBox.Left-Left;
    Height:=16;
  end;

  with KeyMappingConsistencyCheckButton do begin
    Top:=5;
    Left:=Max(KeyMappingSchemeComboBox.Left+KeyMappingSchemeComboBox.Width
            ,MaxX-150);
    Width:=130;
    Height:=23;
  end;

  with KeyMappingHelpLabel do begin
    Top:=KeyMappingSchemeComboBox.Top+KeepCaretXCheckBox.Height+10;
    Left:=5;
    Width:=MaxX-Left-Left;
    Height:=16;
  end;

  with KeyMappingTreeView do begin
    Top:=KeyMappingHelpLabel.Top+KeyMappingHelpLabel.Height+2;
    Left:=0;
    Width:=MaxX-Left-Left;
    Height:=MaxY-Top;
  end;
end;

procedure TEditorOptionsForm.SetupColorPage;
var a,MaxX,MaxY:integer;
begin
  MaxX:=Width-5;
  MaxY:=377;

  LanguageComboBox:=TComboBox.Create(Self);
  with LanguageComboBox do begin
    Name:='LanguageComboBox';
    Parent:=MainNoteBook.Page[3];
    Top:=5;
    Left:=75;
    Width:=170;
    Height:=20;
    with Items do begin
      BeginUpdate;
      for a:=0 to EditorOpts.HighlighterList.Count-1 do
        Add(TCustomSynClass(
          EditorOpts.HighlighterList[a].SynClass).GetLanguageName);
      EndUpdate;
    end;
    OnChange:=@ComboBoxOnChange;
    OnKeyDown:=@ComboBoxOnKeyDown;
    OnExit:=@ComboBoxOnExit;
    Visible:=true;
  end;
  
  LanguageLabel:=TLabel.Create(Self);
  with LanguageLabel do begin
    Name:='LanguageLabel';
    Parent:=MainNoteBook.Page[3];
    Top:=7;
    Left:=5;
    Width:=LanguageComboBox.Left-Left;
    Height:=16;
    Caption:=dlgLang;
    Visible:=true;
  end;

  ColorSchemeComboBox:=TComboBox.Create(Self);
  with ColorSchemeComboBox do begin
    Name:='ColorSchemeComboBox';
    Parent:=MainNoteBook.Page[3];
    Top:=LanguageComboBox.Top;
    Left:=LanguageComboBox.Left+LanguageComboBox.Width+110;
    Width:=100;
    Height:=20;
    with Items do begin
      BeginUpdate;
      // ToDo: fill also with custom color schemes
      Add(DefaultColorScheme);
      Add('Twilight');
      EndUpdate;
    end;
    Text:=DefaultColorScheme;
    OnChange:=@ComboBoxOnChange;
    OnKeyDown:=@ComboBoxOnKeyDown;
    OnExit:=@ComboBoxOnExit;
    Visible:=true;
  end;

  ColorSchemeLabel:=TLabel.Create(Self);
  with ColorSchemeLabel do begin
    Name:='ColorSchemeLabel';
    Parent:=MainNoteBook.Page[3];
    Top:=ColorSchemeComboBox.Top+2;
    Left:=ColorSchemeComboBox.Left-90;
    Width:=ColorSchemeComboBox.Left-Left;
    Height:=16;
    Caption:=dlgClrScheme ;
    Visible:=true;
  end;

  FileExtensionsComboBox:=TComboBox.Create(Self);
  with FileExtensionsComboBox do begin
    Name:='FileExtensionsComboBox';
    Parent:=MainNoteBook.Page[3];
    Top:=ColorSchemeComboBox.Top+ColorSchemeComboBox.Height+4;
    Left:=103;
    Width:=310;
    Height:=20;
    Items.BeginUpdate;
    Items.Add('pp;pas;inc;lpr;lrs;dpr;dpk');
    Items.Add('pp;pas;inc;lpr;lrs');
    Items.Add('pp;pas;inc');
    Items.EndUpdate;
    if CurLanguageID>=0 then
      SetComboBoxText(FileExtensionsComboBox,
        EditorOpts.HighlighterList[CurLanguageID].FileExtensions);
    OnChange:=@ComboBoxOnChange;
    OnKeyDown:=@ComboBoxOnKeyDown;
    OnExit:=@ComboBoxOnExit;
    Visible:=true;
  end;

  FileExtensionsLabel:=TLabel.Create(Self);
  with FileExtensionsLabel do begin
    Name:='FileExtensionsLabel';
    Parent:=MainNoteBook.Page[3];
    Top:=FileExtensionsComboBox.Top+2;
    Left:=5;
    Width:=FileExtensionsComboBox.Left-Left-2;
    Caption:=dlgFileExts ;
    Visible:=true;
  end;

  ColorElementLabel:=TLabel.Create(Self);
  with ColorElementLabel do begin
    Name:='ColorElementLabel';
    Parent:=MainNoteBook.Page[3];
    Top:=FileExtensionsComboBox.Top+FileExtensionsComboBox.Height+12;
    Left:=5;
    Width:=180;
    Height:=16;
    Caption:=dlgEdElement ;
    Visible:=true;
  end;

  ColorElementListBox:=TListBox.Create(Self);
  with ColorElementListBox do begin
    Name:='ColorElementListBox';
    Parent:=MainNoteBook.Page[3];
    Top:=ColorElementLabel.Top+ColorElementLabel.Height+2;
    Left:=ColorElementLabel.Left;
    Width:=ColorElementLabel.Width;
    Height:=170;
    MultiSelect:=false;
    OnMouseUp:=@ColorElementListBoxMouseUp;
    Visible:=true;
  end;

  SetAttributeToDefaultButton:=TButton.Create(Self);
  with SetAttributeToDefaultButton do begin
    Name:='SetAttributeToDefaultButton';
    Parent:=MainNoteBook.Page[3];
    Top:=ColorElementLabel.Top;
    Left:=ColorElementListBox.Left+ColorElementListBox.Width+12;
    Width:=MaxX-5-Left;
    Height:=23;
    Caption:=dlgSetElementDefault ;
    OnClick:=@SetAttributeToDefaultButtonClick;
    Visible:=true;
  end;
  
  SetAllAttributesToDefaultButton:=TButton.Create(Self);
  with SetAllAttributesToDefaultButton do begin
    Name:='SetAllAttributesToDefaultButton';
    Parent:=MainNoteBook.Page[3];
    Top:=SetAttributeToDefaultButton.Top+SetAttributeToDefaultButton.Height+2;
    Left:=SetAttributeToDefaultButton.Left;
    Width:=SetAttributeToDefaultButton.Width;
    Height:=SetAttributeToDefaultButton.Height;
    Caption:=dlgSetAllElementDefault;
    OnClick:=@SetAllAttributesToDefaultButtonClick;
    Visible:=true;
  end;

  ForeGroundGroupBox:=TGroupBox.Create(Self);
  with ForeGroundGroupBox do begin
    Name:='ForeGroundGroupBox';
    Parent:=MainNoteBook.Page[3];
    Top:=SetAllAttributesToDefaultButton.Top
        +SetAllAttributesToDefaultButton.Height+4;
    Left:=ColorElementListBox.Left+ColorElementListBox.Width+12;
    Width:=MaxX-5-Left;
    Height:=43;
    Caption:=dlgForecolor;
    Visible:=true;
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
    Visible:=true;
  end;

  ForeGroundUseDefaultCheckBox:=TCheckBox.Create(Self);
  with ForeGroundUseDefaultCheckBox do begin
    Name:='ForeGroundUseDefaultCheckBox';
    Parent:=ForeGroundGroupBox;
    Top:=ForeGroundColorButton.Top;
    Left:=ForegroundColorButton.Left+ForegroundColorButton.Width+5;
    Width:=ForeGroundGroupBox.Width-Left-Left;
    Height:=16;
    Caption:=dlgEdUseDefColor ;
    OnClick:=@GeneralCheckBoxOnClick;
    Visible:=true;
  end;

  BackGroundGroupBox:=TGroupBox.Create(Self);
  with BackGroundGroupBox do begin
    Name:='BackGroundGroupBox';
    Parent:=MainNoteBook.Page[3];
    Top:=ForeGroundGroupBox.Top+ForeGroundGroupBox.Height+5;
    Left:=ForeGroundGroupBox.Left;
    Width:=ForeGroundGroupBox.Width;
    Height:=ForeGroundGroupBox.Height;
    Caption:=dlgBackColor ;
    Visible:=true;
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
    Visible:=true;
  end;

  BackGroundUseDefaultCheckBox:=TCheckBox.Create(Self);
  with BackGroundUseDefaultCheckBox do begin
    Name:='BackGroundUseDefaultCheckBox';
    Parent:=BackGroundGroupBox;
    Top:=BackGroundColorButton.Top;
    Left:=BackgroundColorButton.Left+BackgroundColorButton.Width+5;
    Width:=ForeGroundGroupBox.Width-Left-Left;
    Height:=16;
    Caption:=dlgEdUseDefColor;
    OnClick:=@GeneralCheckBoxOnClick;
    Visible:=true;
  end;

  TextAttributesGroupBox:=TGroupBox.Create(Self);
  with TextAttributesGroupBox do begin
    Name:='TextAttributesGroupBox';
    Parent:=MainNoteBook.Page[3];
    Top:=BackGroundGroupBox.Top+BackGroundGroupBox.Height+5;
    Left:=ForeGroundGroupBox.Left;
    Width:=ForeGroundGroupBox.Width;
    Height:=43;
    Caption:=dlgTextAttributes ;
    Visible:=true;
  end;

  TextBoldCheckBox:=TCheckBox.Create(Self);
  with TextBoldCheckBox do begin
    Name:='TextBoldCheckBox';
    Parent:=TextAttributesGroupBox;
    Top:=5;
    Left:=5;
    Width:=50;
    Height:=16;
    Caption:=dlgEdBold;
    OnClick:=@GeneralCheckBoxOnClick;
    Visible:=true;
  end;

  TextItalicCheckBox:=TCheckBox.Create(Self);
  with TextItalicCheckBox do begin
    Name:='TextItalicCheckBox';
    Parent:=TextAttributesGroupBox;
    Top:=TextBoldCheckBox.Top;
    Left:=TextBoldCheckBox.Left+TextBoldCheckBox.Width+5;
    Width:=50;
    Height:=TextBoldCheckBox.Height;
    Caption:=dlgEdItal ;
    OnClick:=@GeneralCheckBoxOnClick;
    Visible:=true;
  end;

  TextUnderlineCheckBox:=TCheckBox.Create(Self);
  with TextUnderlineCheckBox do begin
    Name:='TextUnderlineCheckBox';
    Parent:=TextAttributesGroupBox;
    Top:=TextBoldCheckBox.Top;
    Left:=TextItalicCheckBox.Left+TextItalicCheckBox.Width+5;
    Width:=75;
    Height:=TextItalicCheckBox.Height;
    Caption:=dlgEdUnder ;
    OnClick:=@GeneralCheckBoxOnClick;
    Visible:=true;
  end;

  ColorPreview:=TPreviewEditor.Create(Self);
  with ColorPreview do begin
    Name:='ColorPreview';
    Parent:=MainNoteBook.Page[3];
    Left:=5;
    Top:=TextAttributesGroupBox.Top+TextAttributesGroupBox.Height+7;
    Width:=MaxX-Left-Left;
    Height:=MaxY-Top-Left;
    OnSpecialLineColors:=@Self.OnSpecialLineColors;
    OnMouseDown:=@ColorPreviewMouseUp;
    ReadOnly:=true;
    Visible:=true;
  end; 
end;

procedure TEditorOptionsForm.ResizeColorPage;
var MaxX,MaxY:integer;
begin
  MaxX:=Width-5;
  MaxY:=ClientHeight-76;

  with LanguageComboBox do begin
    Top:=5;
    Left:=75;
    Width:=170;
    Height:=20;
  end;

  with LanguageLabel do begin
    Top:=7;
    Left:=5;
    Width:=LanguageComboBox.Left-Left;
    Height:=16;
  end;

  with ColorSchemeComboBox do begin
    Top:=LanguageComboBox.Top;
    Left:=LanguageComboBox.Left+LanguageComboBox.Width+110;
    Width:=100;
    Height:=20;
  end;

  with ColorSchemeLabel do begin
    Top:=ColorSchemeComboBox.Top+2;
    Left:=ColorSchemeComboBox.Left-90;
    Width:=ColorSchemeComboBox.Left-Left;
    Height:=16;
  end;

  with FileExtensionsComboBox do begin
    Top:=ColorSchemeComboBox.Top+ColorSchemeComboBox.Height+4;
    Left:=103;
    Width:=310;
    Height:=20;
  end;

  with FileExtensionsLabel do begin
    Top:=FileExtensionsComboBox.Top+2;
    Left:=5;
    Width:=FileExtensionsComboBox.Left-Left-2;
  end;

  with ColorElementLabel do begin
    Top:=FileExtensionsComboBox.Top+FileExtensionsComboBox.Height+12;
    Left:=5;
    Width:=180;
    Height:=16;
  end;

  with ColorElementListBox do begin
    Top:=ColorElementLabel.Top+ColorElementLabel.Height+2;
    Left:=ColorElementLabel.Left;
    Width:=ColorElementLabel.Width;
    Height:=170;
  end;

  with SetAttributeToDefaultButton do begin
    Top:=ColorElementLabel.Top;
    Left:=ColorElementListBox.Left+ColorElementListBox.Width+12;
    Width:=MaxX-5-Left;
    Height:=23;
  end;

  with SetAllAttributesToDefaultButton do begin
    Top:=SetAttributeToDefaultButton.Top+SetAttributeToDefaultButton.Height+2;
    Left:=SetAttributeToDefaultButton.Left;
    Width:=SetAttributeToDefaultButton.Width;
    Height:=SetAttributeToDefaultButton.Height;
  end;

  with ForeGroundGroupBox do begin
    Top:=SetAllAttributesToDefaultButton.Top
        +SetAllAttributesToDefaultButton.Height+4;
    Left:=ColorElementListBox.Left+ColorElementListBox.Width+12;
    Width:=MaxX-5-Left;
    Height:=43;
  end;

  with ForegroundColorButton do begin
    Top:=2;
    Left:=5;
    Width:=70;
    Height:=20;
  end;

  with ForeGroundUseDefaultCheckBox do begin
    Top:=ForeGroundColorButton.Top;
    Left:=ForegroundColorButton.Left+ForegroundColorButton.Width+5;
    Width:=ForeGroundGroupBox.Width-Left-Left;
    Height:=16;
  end;

  with BackGroundGroupBox do begin
    Top:=ForeGroundGroupBox.Top+ForeGroundGroupBox.Height+5;
    Left:=ForeGroundGroupBox.Left;
    Width:=ForeGroundGroupBox.Width;
    Height:=ForeGroundGroupBox.Height;
  end;

  with BackgroundColorButton do begin
    Top:=2;
    Left:=5;
    Width:=70;
    Height:=20;
  end;

  with BackGroundUseDefaultCheckBox do begin
    Top:=BackGroundColorButton.Top;
    Left:=BackgroundColorButton.Left+BackgroundColorButton.Width+5;
    Width:=ForeGroundGroupBox.Width-Left-Left;
    Height:=16;
  end;

  with TextAttributesGroupBox do begin
    Top:=BackGroundGroupBox.Top+BackGroundGroupBox.Height+5;
    Left:=ForeGroundGroupBox.Left;
    Width:=ForeGroundGroupBox.Width;
    Height:=43;
  end;

  with TextBoldCheckBox do begin
    Top:=5;
    Left:=5;
    Width:=50;
    Height:=16;
  end;

  with TextItalicCheckBox do begin
    Top:=TextBoldCheckBox.Top;
    Left:=TextBoldCheckBox.Left+TextBoldCheckBox.Width+5;
    Width:=50;
    Height:=TextBoldCheckBox.Height;
  end;

  with TextUnderlineCheckBox do begin
    Top:=TextBoldCheckBox.Top;
    Left:=TextItalicCheckBox.Left+TextItalicCheckBox.Width+5;
    Width:=75;
    Height:=TextItalicCheckBox.Height;
  end;

  with ColorPreview do begin
    Left:=5;
    Top:=TextAttributesGroupBox.Top+TextAttributesGroupBox.Height+7;
    Width:=MaxX-Left-Left;
    Height:=MaxY-Top-Left;
  end;
end;

procedure TEditorOptionsForm.SetupCodeToolsPage;
var MaxX:integer;
begin
  MaxX:=Width-5;

  AutomaticFeaturesGroupBox:=TGroupBox.Create(Self);
  with AutomaticFeaturesGroupBox do begin
    Name:='AutomaticFeaturesGroupBox';
    Parent:=MainNoteBook.Page[4];
    Top:=5;
    Left:=5;
    Width:=MaxX-Left-Left;
    Height:=110;
    Caption:='Automatic features';
    Visible:=true;
  end;

  AutoIdentifierCompletionCheckBox:=TCheckBox.Create(Self);
  with AutoIdentifierCompletionCheckBox do begin
    Name:='AutoIdentifierCompletionCheckBox';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=2;
    Left:=5;
    Width:=200;
    Height:=20;
    Caption:=dlgEdIdComlet ;
    Checked:=EditorOpts.AutoIdentifierCompletion;
    Enabled:=false;
    Visible:=true;
  end;

  AutoCodeParametersCheckBox:=TCheckBox.Create(Self);
  with AutoCodeParametersCheckBox do begin
    Name:='AutoCodeParametersCheckBox';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=AutoIdentifierCompletionCheckBox.Top
        +AutoIdentifierCompletionCheckBox.Height;
    Left:=AutoIdentifierCompletionCheckBox.Left;
    Width:=AutoIdentifierCompletionCheckBox.Width;
    Height:=AutoIdentifierCompletionCheckBox.Height;
    Caption:=dlgEdCodeParams ;
    Checked:=EditorOpts.AutoCodeParameters;
    Enabled:=false;
    Visible:=true;
  end;

  AutoToolTipExprEvalCheckBox:=TCheckBox.Create(Self);
  with AutoToolTipExprEvalCheckBox do begin
    Name:='AutoToolTipExprEvalCheckBox';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=AutoCodeParametersCheckBox.Top+AutoCodeParametersCheckBox.Height;
    Left:=AutoIdentifierCompletionCheckBox.Left;
    Width:=AutoIdentifierCompletionCheckBox.Width;
    Height:=AutoIdentifierCompletionCheckBox.Height;
    Caption:=dlgTooltipEval ;
    Checked:=EditorOpts.AutoToolTipExprEval;
    Enabled:=false;
    Visible:=true;
  end;

  AutoToolTipSymbToolsCheckBox:=TCheckBox.Create(Self);
  with AutoToolTipSymbToolsCheckBox do begin
    Name:='AutoToolTipSymbToolsCheckBox';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=AutoToolTipExprEvalCheckBox.Top+AutoToolTipExprEvalCheckBox.Height;
    Left:=AutoIdentifierCompletionCheckBox.Left;
    Width:=AutoIdentifierCompletionCheckBox.Width;
    Height:=AutoIdentifierCompletionCheckBox.Height;
    Caption:=dlgTooltipTools ;
    Checked:=EditorOpts.AutoToolTipSymbTools;
    Visible:=true;
  end;

  AutoDelayLabel:=TLabel.Create(Self);
  with AutoDelayLabel do begin
    Name:='AutoDelayLabel';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=10;
    Left:=AutoIdentifierCompletionCheckBox.Left
          +AutoIdentifierCompletionCheckBox.Width+17;
    Width:=70;
    Caption:=dlgEdDelay ;
    Visible:=true;
  end;

  AutoDelayTrackBar:=TTrackBar.Create(Self);
  with AutoDelayTrackBar do begin
    Name:='AutoDelayTrackBar';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=32;
    Left:=AutoIdentifierCompletionCheckBox.Left
         +AutoIdentifierCompletionCheckBox.Width+15;
    Width:=150;
    Min:=2;
    Max:=6;
    Height:=10;
    Position:=EditorOpts.AutoDelayInMSec div 250;
    TickMarks:=tmBottomRight;
    Visible:=true;
  end;

  AutoDelayMinLabel:=TLabel.Create(Self);
  with AutoDelayMinLabel do begin
    Name:='AutoDelayMinLabel';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=AutoDelayTrackBar.Top+AutoDelayTrackBar.Height+5;
    Left:=AutoIdentifierCompletionCheckBox.Left
         +AutoIdentifierCompletionCheckBox.Width+15;
    Width:=70;
    Caption:='0.5 ' + DlgTimeSecondUnit;
    Visible:=true;
  end;

  AutoDelayMaxLabel:=TLabel.Create(Self);
  with AutoDelayMaxLabel do begin
    Name:='AutoDelayMaxLabel';
    Parent:=AutomaticFeaturesGroupBox;
    Top:=AutoDelayMinLabel.Top;
    Left:=AutoDelayTrackBar.Left+AutoDelayTrackBar.Width-30;
    Width:=70;
    Caption:='1.5 '+ dlgTimeSecondUnit ;
    Visible:=true;
  end;

  CodeTemplatesGroupBox:=TGroupBox.Create(Self);
  with CodeTemplatesGroupBox do begin
    Name:='CodeTemplatesGroupBox';
    Parent:=MainNoteBook.Page[4];
    Top:=AutomaticFeaturesGroupBox.Top+AutomaticFeaturesGroupBox.Height+5;
    Left:=AutomaticFeaturesGroupBox.Left;
    Width:=AutomaticFeaturesGroupBox.Width;
    Height:=250;
    Caption:=dlgEdCodeTempl;
    OnResize:=@CodeTemplatesGroupBoxResize;
    Visible:=true;
  end;

  CodeTemplateFileNameLabel:=TLabel.Create(Self);
  with CodeTemplateFileNameLabel do begin
    Name:='CodeTemplateFileNameLabel';
    Parent:=CodeTemplatesGroupBox;
    Top:=5;
    Left:=7;
    Width:=110;
    Caption:=dlgTplFName ;
    Visible:=true;
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
    Visible:=true;
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
    Visible:=true;
  end;

  CodeTemplateAddButton:=TButton.Create(Self);
  with CodeTemplateAddButton do begin
    Name:='CodeTemplateAddButton';
    Parent:=CodeTemplatesGroupBox;
    Top:=CodeTemplateFileNameComboBox.Top+CodeTemplateFileNameComboBox.Height+10;
    Width:=50;
    Left:=CodeTemplateFileNameLabel.Left;
    Height:=23;
    Caption:=dlgEdAdd;
    OnClick:=@CodeTemplateButtonClick;
    Visible:=true;
  end;

  CodeTemplateEditButton:=TButton.Create(Self);
  with CodeTemplateEditButton do begin
    Name:='CodeTemplateEditButton';
    Parent:=CodeTemplatesGroupBox;
    Top:=CodeTemplateAddButton.Top+CodeTemplateAddButton.Height+5;
    Left:=CodeTemplateAddButton.Left;
    Width:=CodeTemplateAddButton.Width;
    Height:=CodeTemplateAddButton.Height;
    Caption:=dlgEdEdit;
    OnClick:=@CodeTemplateButtonClick;
    Visible:=true;
  end;

  CodeTemplateDeleteButton:=TButton.Create(Self);
  with CodeTemplateDeleteButton do begin
    Name:='CodeTemplateDeleteButton';
    Parent:=CodeTemplatesGroupBox;
    Top:=CodeTemplateEditButton.Top+CodeTemplateEditButton.Height+5;
    Left:=CodeTemplateAddButton.Left;
    Width:=CodeTemplateAddButton.Width;
    Height:=CodeTemplateAddButton.Height;
    Caption:=dlgEdDelete ;
    OnClick:=@CodeTemplateButtonClick;
    Visible:=true;
  end;

  CodeTemplatesLabel:=TLabel.Create(Self);
  with CodeTemplatesLabel do begin
    Name:='CodeTemplatesLabel';
    Parent:=CodeTemplatesGroupBox;
    Top:=CodeTemplateFileNameLabel.Top+CodeTemplateFileNameLabel.Height+12;
    Left:=CodeTemplateAddButton.Left+CodeTemplateAddButton.Width+5;
    Width:=60;
    Caption:='Templates';
    Visible:=true;
  end;

  CodeTemplateListBox:=TListBox.Create(Self);
  with CodeTemplateListBox do begin
    Name:='CodeTemplateListBox';
    Parent:=CodeTemplatesGroupBox;
    Top:=CodeTemplatesLabel.Top;
    Left:=CodeTemplatesLabel.Left+CodeTemplatesLabel.Width+5;
    Width:=Parent.ClientWidth-8-Left;
    Height:=80;
    OnMouseUp:=@CodeTemplateListBoxMouseUp;
    Visible:=true;
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
    Visible:=true;
  end;

  CodeTemplateCodePreview:=TPreviewEditor.Create(Self);
  with CodeTemplateCodePreview do begin
    Name:='CodeTemplateCodePreview';
    Parent:=CodeTemplatesGroupBox;
    Top:=CodeTemplateCodeLabel.Top;
    Left:=CodeTemplateCodeLabel.Left+CodeTemplateCodeLabel.Width+5;
    Width:=CodeTemplateListBox.Width;
    Height:=CodeTemplatesGroupBox.ClientHeight-20-Top;
    Lines.Clear;
    Gutter.Visible:=false;
    Visible:=true;
  end;
  
  CodeTemplateIndentTypeRadioGroup:=TRadioGroup.Create(Self);
  with CodeTemplateIndentTypeRadioGroup do begin
    Name:='CodeTemplateIndentTypeRadioGroup';
    Parent:=CodeTemplatesGroupBox;
    Left:=CodeTemplateAddButton.Left;
    Top:=CodeTemplateCodeLabel.Top+CodeTemplateCodeLabel.Height+15;
    Width:=CodeTemplateCodePreview.Left-Left-8;
    Height:=70;
    Caption:=dlgIndentCodeTo ;
    with Items do begin
      BeginUpdate;
      Add('Token start');
      Add('Line start');
      EndUpdate;
    end;
    Visible:=true;
  end;

  CurCodeTemplate:=-1;
end;

procedure TEditorOptionsForm.ResizeCodeToolsPage;
var MaxX, MaxY:integer;
begin
  MaxX:=Width-5;
  MaxY:=ClientHeight-76;

  with AutomaticFeaturesGroupBox do begin
    Top:=5;
    Left:=5;
    Width:=MaxX-Left-Left;
    Height:=110;
  end;

  with AutoIdentifierCompletionCheckBox do begin
    Top:=2;
    Left:=5;
    Width:=200;
    Height:=20;
  end;

  with AutoCodeParametersCheckBox do begin
    Top:=AutoIdentifierCompletionCheckBox.Top
        +AutoIdentifierCompletionCheckBox.Height;
    Left:=AutoIdentifierCompletionCheckBox.Left;
    Width:=AutoIdentifierCompletionCheckBox.Width;
    Height:=AutoIdentifierCompletionCheckBox.Height;
  end;

  with AutoToolTipExprEvalCheckBox do begin
    Top:=AutoCodeParametersCheckBox.Top+AutoCodeParametersCheckBox.Height;
    Left:=AutoIdentifierCompletionCheckBox.Left;
    Width:=AutoIdentifierCompletionCheckBox.Width;
    Height:=AutoIdentifierCompletionCheckBox.Height;
  end;

  with AutoToolTipSymbToolsCheckBox do begin
    Top:=AutoToolTipExprEvalCheckBox.Top+AutoToolTipExprEvalCheckBox.Height;
    Left:=AutoIdentifierCompletionCheckBox.Left;
    Width:=AutoIdentifierCompletionCheckBox.Width;
    Height:=AutoIdentifierCompletionCheckBox.Height;
  end;

  with AutoDelayLabel do begin
    Top:=10;
    Left:=AutoIdentifierCompletionCheckBox.Left
          +AutoIdentifierCompletionCheckBox.Width+17;
    Width:=70;
  end;

  with AutoDelayTrackBar do begin
    Top:=32;
    Left:=AutoIdentifierCompletionCheckBox.Left
         +AutoIdentifierCompletionCheckBox.Width+15;
    Width:=150;
    Height:=10;
  end;

  with AutoDelayMinLabel do begin
    Top:=AutoDelayTrackBar.Top+AutoDelayTrackBar.Height+5;
    Left:=AutoIdentifierCompletionCheckBox.Left
         +AutoIdentifierCompletionCheckBox.Width+15;
    Width:=70;
  end;

  with AutoDelayMaxLabel do begin
    Top:=AutoDelayMinLabel.Top;
    Left:=AutoDelayTrackBar.Left+AutoDelayTrackBar.Width-30;
    Width:=70;
  end;

  with CodeTemplatesGroupBox do begin
    Top:=AutomaticFeaturesGroupBox.Top+AutomaticFeaturesGroupBox.Height+5;
    Left:=AutomaticFeaturesGroupBox.Left;
    Width:=AutomaticFeaturesGroupBox.Width;
    Height:=MaxY-Top-10;
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
    Top:=Self.Height-Height-15;
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
    Top:=Self.Height-Height-15;
    Left:=CancelButton.Left-10-Width;
    Caption:='Ok';
    OnClick:=@OkButtonClick;
    Show;
  end;
end;

procedure TEditorOptionsForm.ResizeButtonBar;
begin
  with CancelButton do begin
    Width:=70;
    Height:=23;
    Top:=Self.Height-Height-15;
    Left:=Self.Width-Width-10;
  end;

  with OkButton do begin
    Width:=70;
    Height:=23;
    Top:=Self.Height-Height-15;
    Left:=CancelButton.Left-10-Width;
  end;
end;

procedure TEditorOptionsForm.OkButtonClick(Sender:TObject);
var res: TModalResult;
  SynOptions: TSynEditorOptions;
  i: integer;
begin
  IDEDialogLayoutList.SaveLayout(Self);
  SaveCurCodeTemplate;
  
  // save all values
  SynOptions:=PreviewEdits[1].Options-[eoNoSelection,eoNoCaret];
  if BracketHighlightCheckBox.Checked then
    Include(SynOptions,eoBracketHighlight)
  else
    Exclude(SynOptions,eoBracketHighlight);
  PreviewEdits[1].Options:=SynOptions;
  EditorOpts.SetSynEditSettings(PreviewEdits[1]);
  PreviewEdits[1].Options:=SynOptions-[eoBracketHighlight]
                                     +[eoNoCaret,eoNoSelection];

  // general
  EditorOpts.ShowTabCloseButtons:=ShowCloseBtnInNoteBookCheckBox.Checked;
  EditorOpts.UndoAfterSave:=UndoAfterSaveCheckBox.Checked;
  EditorOpts.FindTextAtCursor:=FindTextAtCursorCheckBox.Checked;
  EditorOpts.UseSyntaxHighlight:=UseSyntaxHighlightCheckBox.Checked;
  i:=StrToIntDef(UndoLimitComboBox.Text,32767);
  if i<1 then i:=1;
  if i>32767 then i:=32767;
  EditorOpts.UndoLimit:=i;
  i:=StrToIntDef(TabWidthsComboBox.Text,2);
  if i<1 then i:=1;
  if i>20 then i:=20;
  EditorOpts.TabWidth:=i;
  i:=StrToIntDef(BlockIndentComboBox.Text,2);
  if i<1 then i:=1;
  if i>20 then i:=20;
  EditorOpts.BlockIndent:=i;

  
  // color
  SaveAllFileExtensions;
  SaveAllColorSchemes;
  SaveAllHighlighters;

  // code Tools
  EditorOpts.AutoIdentifierCompletion:=AutoIdentifierCompletionCheckBox.Checked;
  EditorOpts.AutoCodeParameters:=AutoCodeParametersCheckBox.Checked;
  EditorOpts.AutoToolTipSymbTools:=AutoToolTipSymbToolsCheckBox.Checked;
  EditorOpts.AutoToolTipExprEval:=AutoToolTipExprEvalCheckBox.Checked;
  EditorOpts.AutoDelayInMSec:=AutoDelayTrackBar.Position*250;
  EditorOpts.CodeTemplateFileName:=CodeTemplateFileNameComboBox.Text;
  EditorOpts.CodeTemplateIndentToTokenStart:=
    (CodeTemplateIndentTypeRadioGroup.ItemIndex=0);

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
  IDEDialogLayoutList.SaveLayout(Self);
  EditorOpts.Load;
  ModalResult:=mrCancel;
end;

//=============================================================================

initialization

{$I lazarus_dci.lrs}
{$I editoroptions.lrs}

end.

