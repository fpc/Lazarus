{  $Id$  }
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
}
unit EditorOptions;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL
  Classes, Math, SysUtils,
  // LCL
  Buttons, ComCtrls, Controls, Dialogs, ExtCtrls, FileCtrl, Forms, Graphics,
  GraphType, LCLIntf, LCLProc, LCLType, LResources, StdCtrls,
  // synedit
  SynEdit, SynEditAutoComplete, SynEditHighlighter, SynEditKeyCmds,
  SynHighlighterCPP, SynHighlighterHTML, SynHighlighterJava, SynHighlighterLFM,
  SynHighlighterPas, SynHighlighterPerl, SynHighlighterPHP, SynHighlighterSQL,
  SynHighlighterPython, SynHighlighterUNIXShellScript, SynHighlighterXML,
  // codetools
  Laz_XMLCfg,
  // IDEIntf
  IDECommands, IDEWindowIntf, SrcEditorIntf,
  // IDE
  LazarusIDEStrConsts, IDEOptionDefs, IDEProcs, InputHistory, KeyMapping,
  KeymapSchemeDlg, LazConf, Spin;

type
  TPreviewEditor = TSynEdit;
  TPreviewPasSyn = TSynFreePascalSyn;
  TCustomSyn     = TSynCustomHighlighter;
  TSynHighlightElement = TSynHighlighterAttributes;
  TCustomSynClass = class of TCustomSyn;

  TLazSyntaxHighlighter =
    (lshNone, lshText, lshFreePascal, lshDelphi, lshLFM, lshXML, lshHTML,
    lshCPP, lshPerl, lshJava, lshBash, lshPython, lshPHP, lshSQL);

  TAdditionalHilightAttribute = (ahaNone, ahaTextBlock, ahaExecutionPoint,
    ahaEnabledBreakpoint, ahaDisabledBreakpoint,
    ahaInvalidBreakpoint, ahaUnknownBreakpoint,
    ahaErrorLine);

const
  EditorOptsFormatVersion = 2;

  AdditionalHighlightAttributes: array[TAdditionalHilightAttribute] of String =
    (
    '',
    'Text block',
    'Execution point',
    'Enabled breakpoint',
    'Disabled breakpoint',
    'Invalid breakpoint',
    'Unknown breakpoint',
    'Error line'
    );

  LazSyntaxHighlighterClasses: array[TLazSyntaxHighlighter] of
    TCustomSynClass =
    (Nil, Nil, TSynFreePascalSyn, TSynPasSyn, TSynLFMSyn, TSynXMLSyn,
    TSynHTMLSyn, TSynCPPSyn, TSynPerlSyn, TSynJavaSyn, TSynUNIXShellScriptSyn,
    TSynPythonSyn, TSynPHPSyn, TSynSQLSyn);


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
    comtPerl,  // lshPerl
    comtCPP,   // lshJava
    comtPerl,  // lshBash
    comtPerl,  // lshPython
    comtHTML,  // lshPHP
    comtCPP    // lshSQL
    );

const
  SynEditDefaultOptions = SYNEDIT_DEFAULT_OPTIONS - [eoShowScrollHint]
                                                  + [eoHalfPageScroll];
  SynEditDefaultOptions2 = SYNEDIT_DEFAULT_OPTIONS2;

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
    TheType:  TLazSyntaxHighlighter;
    FileExtensions: String; // divided by semicolon, e.g. 'pas;pp;inc'
    DefaultFileExtensions: string;
    ColorScheme: String;
    SampleSource: String;
    AddAttrSampleLines: array[TAdditionalHilightAttribute] of
    Integer; // first line = 1
    MappedAttributes: TStringList; // map attributes to pascal
    DefaultCommentType: TCommentType;
    constructor Create;
    destructor Destroy; override;
    function GetDefaultFilextension: String;
    procedure SetBothFilextensions(const Extensions: string);
    function SampleLineToAddAttr(Line: Integer): TAdditionalHilightAttribute;
  end;

  { list of TEditOptLanguageInfo }

  { TEditOptLangList }

  TEditOptLangList = class(TList)
  private
    function GetInfos(Index: Integer): TEditOptLanguageInfo;
  public
    constructor Create;
    procedure Clear; override;
    destructor Destroy; override;
    function FindByName(const Name: String): Integer;
    function FindByClass(CustomSynClass: TCustomSynClass): Integer;
    function FindByHighlighter(Hilighter: TSynCustomHighlighter): Integer;
    function FindByType(AType: TLazSyntaxHighlighter): Integer;
    function GetDefaultFilextension(AType: TLazSyntaxHighlighter): String;
    function GetInfoByType(AType: TLazSyntaxHighlighter): TEditOptLanguageInfo;
    property Items[Index: Integer]: TEditOptLanguageInfo read GetInfos;
      default;
  end;


  { Editor Options object used to hold the editor options }

{ TEditorOptions }

  TEditorOptions = class(TPersistent)
  private
    xmlconfig: TXMLConfig;

    // general options
    fFindTextAtCursor: Boolean;
    fShowTabCloseButtons: Boolean;
    fSynEditOptions: TSynEditorOptions;
    fSynEditOptions2: TSynEditorOptions2;
    fCtrlMouseLinks: Boolean;
    fUndoAfterSave: Boolean;
    fUseSyntaxHighlight: Boolean;
    FCopyWordAtCursorOnCopyNone: Boolean;
    FShowGutterHints: Boolean;
    fBlockIndent: Integer;
    fUndoLimit: Integer;
    fTabWidth:  Integer;

    // Display options
    fVisibleRightMargin: Boolean;
    fVisibleGutter: Boolean;
    fShowLineNumbers: Boolean;
    fGutterColor: TColor;
    fGutterWidth: Integer;
    fRightMargin: Integer;
    fRightMarginColor: TColor;
    fEditorFont:  String;
    fEditorFontHeight: Integer;
    fExtraLineSpacing: Integer;
    FDoNotWarnForFont: string;

    // Key Mappings options
    fKeyMappingScheme: String;
    fKeyMap: TKeyCommandRelationList;

    // Color options
    fHighlighterList: TEditOptLangList;

    // Code tools options (MG: these will move to an unit of their own)
    fAutoIdentifierCompletion: Boolean;
    fAutoCodeParameters: Boolean;
    fAutoToolTipExprEval: Boolean;
    fAutoToolTipSymbTools: Boolean;
    fAutoDelayInMSec: Integer;
    fCodeTemplateFileName: String;
    fCTemplIndentToTokenStart: Boolean;
    
    // Code Folding
    FUseCodeFolding: Boolean;
    FCFDividerDrawLevel: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    function GetSynEditOptionName(SynOption: TSynEditorOption): string;

    procedure GetHighlighterSettings(Syn: TCustomSyn);
              // read highlight settings from config file
    procedure SetHighlighterSettings(Syn: TCustomSyn);
              // write highlight settings to config file
    procedure GetSynEditSettings(ASynEdit: TSynEdit);
              // read synedit settings from config file
    procedure SetSynEditSettings(ASynEdit: TSynEdit);
              // write synedit settings to file
    procedure GetSynEditSelectedColor(ASynEdit: TSynEdit);
    procedure GetSynEditPreviewSettings(APreviewEditor: TObject);
    procedure AddSpecialHilightAttribsToHighlighter(Syn: TCustomSyn);

    function CreateSyn(LazSynHilighter: TLazSyntaxHighlighter): TCustomSyn;
    function ReadColorScheme(const LanguageName: String): String;
    function ReadPascalColorScheme: String;
    procedure WriteColorScheme(const LanguageName, SynColorScheme: String);
    procedure GetDefaultsForPascalAttribute(Attr: TSynHighlightElement;
                                            const SynColorScheme: String);
    procedure ReadHighlighterSettings(Syn: TCustomSyn;
                                      SynColorScheme: String);
    procedure ReadDefaultsForHighlighterSettings(Syn: TCustomSyn;
                                                 SynColorScheme: String;
                                                 DefaultPascalSyn: TPreviewPasSyn);
    procedure WriteHighlighterSettings(Syn: TCustomSyn;
                                       SynColorScheme: String);
    procedure GetSpecialLineColors(Syn: TCustomSyn;
                                   AddHilightAttr:
                                   TAdditionalHilightAttribute;
                                   var Special: Boolean; var FG, BG: TColor);
  published
    // general options
    property SynEditOptions: TSynEditorOptions
      read fSynEditOptions write fSynEditOptions default SynEditDefaultOptions;
    property SynEditOptions2: TSynEditorOptions2
      read fSynEditOptions2 write fSynEditOptions2 default SynEditDefaultOptions2;
    property CtrlMouseLinks: Boolean
      read fCtrlMouseLinks write fCtrlMouseLinks;
    property ShowTabCloseButtons: Boolean
      read fShowTabCloseButtons write fShowTabCloseButtons;
    property UndoAfterSave: Boolean read fUndoAfterSave
      write fUndoAfterSave default True;
    property FindTextAtCursor: Boolean
      read fFindTextAtCursor write fFindTextAtCursor default True;
    property UseSyntaxHighlight: Boolean
      read fUseSyntaxHighlight write fUseSyntaxHighlight default True;
    property CopyWordAtCursorOnCopyNone: Boolean
      read FCopyWordAtCursorOnCopyNone write FCopyWordAtCursorOnCopyNone;
    property ShowGutterHints: Boolean read FShowGutterHints
      write FShowGutterHints;
    property BlockIndent: Integer
      read fBlockIndent write fBlockIndent default 2;
    property UndoLimit: Integer read fUndoLimit write fUndoLimit default 32767;
    property TabWidth: Integer read fTabWidth write fTabWidth default 8;

    // Display options
    property VisibleRightMargin: Boolean
      read fVisibleRightMargin write fVisibleRightMargin default True;
    property VisibleGutter: Boolean read fVisibleGutter
      write fVisibleGutter default True;
    property ShowLineNumbers: Boolean read fShowLineNumbers
      write fShowLineNumbers default False;
    property GutterColor: TColor
      read fGutterColor write fGutterColor default clBtnFace;
    property GutterWidth: Integer
      read fGutterWidth write fGutterWidth default 30;
    property RightMargin: Integer
      read fRightMargin write fRightMargin default 80;
    property RightMarginColor: Integer
      read fRightMarginColor write fRightMarginColor default clBtnFace;
    property EditorFont: String read fEditorFont write fEditorFont;
    property EditorFontHeight: Integer
      read fEditorFontHeight write FEditorFontHeight;
    property ExtraLineSpacing: Integer
      read fExtraLineSpacing write fExtraLineSpacing default 0;
    property DoNotWarnForFont: string read FDoNotWarnForFont write FDoNotWarnForFont;

    // Key Mappings
    property KeyMappingScheme: String
      read fKeyMappingScheme write fKeyMappingScheme;
    property KeyMap: TKeyCommandRelationList read fKeyMap;

    // Color options
    property HighlighterList: TEditOptLangList
      read fHighlighterList write fHighlighterList;

    // Code Tools options
    property AutoIdentifierCompletion: Boolean
      read fAutoIdentifierCompletion write fAutoIdentifierCompletion default True;
    property AutoCodeParameters: Boolean
      read fAutoCodeParameters write fAutoCodeParameters default True;
    property AutoToolTipExprEval: Boolean
      read fAutoToolTipExprEval write fAutoToolTipExprEval default True;
    property AutoToolTipSymbTools: Boolean
      read fAutoToolTipSymbTools write fAutoToolTipSymbTools default True;
    property AutoDelayInMSec: Integer read fAutoDelayInMSec
      write fAutoDelayInMSec default 1000;
    property CodeTemplateFileName: String
      read fCodeTemplateFileName write fCodeTemplateFileName;
    property CodeTemplateIndentToTokenStart: Boolean
      read fCTemplIndentToTokenStart write fCTemplIndentToTokenStart;

    // Code Folding
    property UseCodeFolding: Boolean
        read FUseCodeFolding write FUseCodeFolding default True;
    property CFDividerDrawLevel: Integer
        read FCFDividerDrawLevel write FCFDividerDrawLevel default 3;
  end;

  { TEditorOptionsForm }

  TEditorOptionsForm = class(TForm)
    ForeGroundLabel: TLabel;
    BackGroundLabel: TLabel;
    EditorOptionsGroupBox: TCheckGroup;

    MainNoteBook: TNoteBook;
    ImageList:    TImageList;

    // general options
    GeneralPage: TPage;
    DisplayPage: TPage;
    KeymappingPage: TPage;
    ColorPage: TPage;
    CodetoolsPage: TPage;
    BlockIndentComboBox: TComboBox;
    BlockIndentLabel: TLabel;
    CodeFolding: TPage;
    BtnPanel: TPanel;
    UndoLimitComboBox: TComboBox;
    UndoLimitLabel: TLabel;
    TabWidthsComboBox: TComboBox;
    TabWidthsLabel: TLabel;

    // Display options
    MarginAndGutterGroupBox: TGroupBox;
    VisibleRightMarginCheckBox: TCheckBox;
    VisibleGutterCheckBox: TCheckBox;
    ShowLineNumbersCheckBox: TCheckBox;
    GutterColorButton: TColorButton;
    GutterColorLabel:  TLabel;
    GutterWidthComboBox: TComboBox;
    GutterWidthLabel:  TLabel;
    RightMarginComboBox: TComboBox;
    RightMarginLabel:  TLabel;
    RightMarginColorButton: TColorButton;
    RightMarginColorLabel: TLabel;
    EditorFontGroupBox: TGroupBox;
    EditorFontComboBox: TComboBox;
    EditorFontButton:  TButton;
    EditorFontLabel:   TLabel;
    EditorFontHeightLabel: TLabel;
    EditorFontHeightComboBox: TComboBox;
    ExtraLineSpacingLabel: TLabel;
    ExtraLineSpacingComboBox: TComboBox;
    DisplayPreview:    TPreviewEditor;

    // Key Mappings
    KeyMappingChooseSchemeButton: TButton;
    KeyMappingHelpLabel: TLabel;
    KeyMappingTreeView:  TTreeView;
    KeyMappingConsistencyCheckButton: TButton;

    // Color options
    LanguageComboBox: TComboBox;
    LanguageLabel: TLabel;
    FileExtensionsComboBox: TComboBox;
    FileExtensionsLabel: TLabel;
    ColorSchemeComboBox: TComboBox;
    ColorSchemeLabel: TLabel;
    ColorElementLabel: TLabel;
    ColorElementListBox: TListBox;
    TextAttributesGroupBox: TGroupBox;
    TextBoldCheckBox: TCheckBox;
    TextItalicCheckBox: TCheckBox;
    TextUnderlineCheckBox: TCheckBox;
    ForeGroundGroupBox: TGroupBox;
    ForeGroundColorButton: TColorButton;
    ForeGroundUseDefaultCheckBox: TCheckBox;
    BackGroundGroupBox: TGroupBox;
    BackGroundColorButton: TColorButton;
    BackGroundUseDefaultCheckBox: TCheckBox;
    SetAttributeToDefaultButton: TButton;
    SetAllAttributesToDefaultButton: TButton;
    ColorPreview:     TPreviewEditor;

    // Code Tools options
    AutomaticFeaturesGroupBox: TGroupBox;
    AutoIdentifierCompletionCheckBox: TCheckBox;
    AutoCodeParametersCheckBox: TCheckBox;
    AutoToolTipExprEvalCheckBox: TCheckBox;
    AutoToolTipSymbToolsCheckBox: TCheckBox;
    AutoDelayLabel: TLabel;
    AutoDelayTrackBar: TTrackBar;
    AutoDelayMinLabel: TLabel;
    AutoDelayMaxLabel: TLabel;
    
    // Code Folding
    Bevel1: TBevel;
    chkCodeFoldingEnabled: TCheckBox;
    lblDividerDrawLevel: TLabel;
    edDividerDrawLevel: TSpinEdit;

    // buttons at bottom
    OkButton: TButton;
    CancelButton: TButton;

    // general
    procedure GeneralCheckBoxOnChange(Sender: TObject; Index: integer);
    procedure ComboBoxOnChange(Sender: TObject);
    procedure ComboBoxOnExit(Sender: TObject);
    procedure ComboBoxOnKeyDown(Sender: TObject;
                                var Key: Word; Shift: TShiftState);
    procedure ColorButtonColorChanged(Sender: TObject);

    // display
    procedure FontDialogApplyClicked(Sender: TObject);
    procedure EditorFontComboBoxEditingDone(Sender: TObject);
    procedure EditorFontButtonClick(Sender: TObject);
    procedure RightMarginColorButtonColorChanged(Sender: TObject);

    // key mapping
    procedure KeyMappingChooseSchemeButtonClick(Sender: TObject);
    procedure KeyMappingTreeViewMouseUp(Sender: TObject;
                      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure KeyMappingConsistencyCheckButtonClick(Sender: TObject);

    // color
    procedure ColorElementListBoxSelectionChange(Sender: TObject; User: Boolean);
    procedure ColorPreviewMouseUp(Sender: TObject; Button: TMouseButton;
                                  Shift: TShiftState; X, Y: Integer);
    procedure OnSpecialLineColors(Sender: TObject; Line: Integer;
                                  var Special: Boolean; var FG, BG: TColor);
    procedure SetAttributeToDefaultButtonClick(Sender: TObject);
    procedure SetAllAttributesToDefaultButtonClick(Sender: TObject);

    // code tools

    // Code Folding
    procedure chkCodeFoldingEnabledChange(Sender: TObject);

    // buttons at bottom
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    FormCreating: Boolean;
    PreviewSyn:   TCustomSyn;
    PreviewEdits: array[1..2] of TPreviewEditor;
    CurLanguageID: Integer;
    // current index in EditorOpts.EditOptHighlighterList
    CurHighlightElement: TSynHighlightElement;
    UpdatingColor: Boolean;
    fHighlighterList: TStringList; // list of "ColorScheme" Data=TCustomSyn
    fColorSchemes: TStringList;    // list of LanguageName=ColorScheme
    fFileExtensions: TStringList;  // list of LanguageName=FileExtensions
    EditingKeyMap: TKeyCommandRelationList;

    procedure SetComboBoxText(AComboBox: TComboBox; const AText: String);
    procedure FontDialogNameToFont(FontDialogName: String; AFont: TFont);
    procedure InvalidatePreviews;
    procedure SetPreviewSynInAllPreviews;
    procedure SetupButtonBar;

    // general
    procedure SetupGeneralPage(Page: Integer);

    // display
    procedure SetupDisplayPage(Page: Integer);

    // keymapping
    procedure SetupKeyMappingsPage(Page: Integer);
    function KeyMappingRelationToString(Index: Integer): String;
    function KeyMappingRelationToString(KeyRelation:
                                        TKeyCommandRelation): String;
    procedure FillKeyMappingTreeView;

    // color
    procedure SetupColorPage(Page: Integer);
    procedure ShowCurAttribute;
    procedure FindCurHighlightElement;
    function GetHighlighter(SynClass: TCustomSynClass;
      const ColorScheme: String; CreateIfNotExists: Boolean): TCustomSyn;
    procedure ClearHighlighters;
    procedure SaveAllHighlighters;
    procedure FillColorElementListBox;
    function GetCurColorScheme(const LanguageName: String): String;
    procedure SetCurColorScheme(const LanguageName, ColorScheme: String);
    procedure SaveAllColorSchemes;
    function GetCurFileExtensions(const LanguageName: String): String;
    procedure SetCurFileExtensions(const LanguageName, FileExtensions: String);
    procedure SaveAllFileExtensions;
    procedure SetColorElementsToDefaults(OnlySelected: Boolean);

    // code tools
    procedure SetupCodeToolsPage(Page: Integer);
    
    // Code Folding
    procedure SetupCodeFoldingPage(Page: integer);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  LazSyntaxHighlighterNames: array[TLazSyntaxHighlighter] of String = (
    'None',
    'Text',
    'FreePascal',
    'Delphi',
    'LFM',
    'XML',
    'HTML',
    'C++',
    'Perl',
    'Java',
    'Bash',
    'Python',
    'PHP',
    'SQL'
    );

var
  EditorOptionsForm: TEditorOptionsForm;
  EditorOpts: TEditorOptions;

function ShowEditorOptionsDialog: TModalResult;
function StrToLazSyntaxHighlighter(const s: String): TLazSyntaxHighlighter;
function ExtensionToLazSyntaxHighlighter(Ext: String): TLazSyntaxHighlighter;

function BuildBorlandDCIFile(
  ACustomSynAutoComplete: TCustomSynAutoComplete): Boolean;

implementation


const
  ValidAttribChars = ['a'..'z', 'A'..'Z', '_', '0'..'9'];

  // several language types can be redirected. For example there are FreePascal
  // and Delphi, but currently both are hilighted with the FreePascal
  // highlighter
  CompatibleLazSyntaxHilighter: array[TLazSyntaxHighlighter] of
    TLazSyntaxHighlighter = (
    lshNone,
    lshText,
    lshFreePascal,
    lshFreePascal,
    lshLFM,
    lshXML,
    lshHTML,
    lshCPP,
    lshPerl,
    lshJava,
    lshBash,
    lshPython,
    lshPHP,
    lshSQL
    );

  DefaultColorScheme      = 'Default';


function ShowEditorOptionsDialog: TModalResult;
var
  EditorOptionsForm: TEditorOptionsForm;
begin
  Result := mrCancel;
  EditorOptionsForm := TEditorOptionsForm.Create(Nil);
  try
    Result := EditorOptionsForm.ShowModal;
  finally
    EditorOptionsForm.Free;
  end;
end;

function CheckGroupItemChecked(CheckGroup: TCheckGroup;
  const Caption: string): Boolean;
begin
  Result:=CheckGroup.Checked[CheckGroup.Items.IndexOf(Caption)];
end;

function StrToLazSyntaxHighlighter(const s: String): TLazSyntaxHighlighter;
begin
  for Result := Low(TLazSyntaxHighlighter) to High(TLazSyntaxHighlighter) do
    if (AnsiCompareText(s, LazSyntaxHighlighterNames[Result]) = 0) then
      exit;
  Result := lshFreePascal;
end;

function ExtensionToLazSyntaxHighlighter(Ext: String): TLazSyntaxHighlighter;
var
  s, CurExt: String;
  LangID, StartPos, EndPos: Integer;
begin
  Result := lshNone;
  if (Ext = '') or (Ext = '.') or (EditorOpts.HighlighterList = Nil) then
    exit;
  Ext := lowercase(Ext);
  if (Ext[1] = '.') then
    Ext := copy(Ext, 2, length(Ext) - 1);
  LangID := 0;
  while LangID < EditorOpts.HighlighterList.Count do
  begin
    s := EditorOpts.HighlighterList[LangID].FileExtensions;
    StartPos := 1;
    while StartPos <= length(s) do
    begin
      Endpos := StartPos;
      while (EndPos <= length(s)) and (s[EndPos] <> ';') do
        inc(EndPos);
      CurExt := copy(s, Startpos, EndPos - StartPos);
      if (CurExt <> '') and (CurExt[1] = '.') then
        CurExt := copy(CurExt, 2, length(CurExt) - 1);
      if lowercase(CurExt) = Ext then
      begin
        Result := EditorOpts.HighlighterList[LangID].TheType;
        exit;
      end;
      Startpos := EndPos + 1;
    end;
    inc(LangID);
  end;
end;

const
  EditOptsConfFileName = 'editoroptions.xml';


function BuildBorlandDCIFile(
  ACustomSynAutoComplete: TCustomSynAutoComplete): Boolean;
  // returns if something has changed
var
  sl: TStringList;
  i, sp, ep: Integer;
  Token, Comment, Value: String;
  Attributes: TStrings;
begin
  Result := False;
  sl     := TStringList.Create;
  try
    for i := 0 to ACustomSynAutoComplete.Completions.Count - 1 do
    begin
      Token := ACustomSynAutoComplete.Completions[i];
      Comment := ACustomSynAutoComplete.CompletionComments[i];
      Value := ACustomSynAutoComplete.CompletionValues[i];
      sl.Add('[' + Token + ' | ' + Comment + ']');
      Attributes:=ACustomSynAutoComplete.CompletionAttributes[i];
      if (Attributes<>nil) and (Attributes.Count>0) then begin
        sl.Add(CodeTemplateAttributesStartMagic);
        sl.AddStrings(Attributes);
        sl.Add(CodeTemplateAttributesEndMagic);
      end;
      sp    := 1;
      ep    := 1;
      while ep <= length(Value) do
        if Value[ep] in [#10, #13] then
        begin
          sl.Add(copy(Value, sp, ep - sp));
          inc(ep);
          if (ep <= length(Value)) and (Value[ep] in [#10, #13]) and
            (Value[ep] <> Value[ep - 1]) then
            inc(ep);
          sp := ep;
        end
        else
          inc(ep);
      if (ep > sp) or ((Value <> '') and (Value[length(Value)] in [#10, #13])) then
        sl.Add(copy(Value, sp, ep - sp));
    end;
    if ACustomSynAutoComplete.AutoCompleteList.Equals(sl) = False then
    begin
      Result := True;
      ACustomSynAutoComplete.AutoCompleteList := sl;
    end;
  finally
    sl.Free;
  end;
end;

function StrToValidXMLName(const s: String): String;
var
  i: Integer;
begin
  Result := s;
  // replace invalid characters
  for i := 1 to length(Result) do
    if (not (Result[i] in ValidAttribChars)) then
      Result[i] := '_';
end;

procedure CopyHiLightAttributeValues(Src, Dest: TSynHighlightElement);
begin
  Dest.Background := Src.Background;
  Dest.Foreground := Src.Foreground;
  Dest.Style      := Src.Style;
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
  Line: Integer): TAdditionalHilightAttribute;
begin
  if Line < 1 then
  begin
    Result := ahaNone;
    exit;
  end;
  for Result := Low(TAdditionalHilightAttribute)
    to High(TAdditionalHilightAttribute) do
    if (Result <> ahaNone) and (AddAttrSampleLines[Result] = Line) then
      exit;
  Result := ahaNone;
end;

function TEditOptLanguageInfo.GetDefaultFilextension: String;
var
  p: Integer;
begin
  // read the first file extension
  p := 1;
  while (p <= length(FileExtensions)) and (FileExtensions[p] <> ';') do
    inc(p);
  if p > 1 then
    Result := '.' + copy(FileExtensions, 1, p - 1)
  else
    Result := '';
end;

procedure TEditOptLanguageInfo.SetBothFilextensions(const Extensions: string);
begin
  FileExtensions:=Extensions;
  DefaultFileExtensions:=Extensions;
end;

{ TEditOptLangList }

function TEditOptLangList.GetInfos(Index: Integer): TEditOptLanguageInfo;
begin
  if (Index < 0) or (Index >= Count) then
    raise Exception.Create('TEditOptLangList.GetInfos Index '
      + IntToStr(Index) + ' out of bounds. Count=' + IntToStr(Count));
  Result := TEditOptLanguageInfo(inherited Items[Index]);
end;

procedure TEditOptLangList.Clear;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    Items[i].Free;
  inherited Clear;
end;

constructor TEditOptLangList.Create;
var
  NewInfo: TEditOptLanguageInfo;
begin
  inherited Create;

  { create the meta information for each available highlighter.
    Please keep the pascal highlighter at the top. The rest can be ordered as you
    like.
  }

  // create info for pascal
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshFreePascal;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('pp;pas;inc;lpr;lrs;dpr;dpk;fpd');
    SampleSource :=
      '{ Comment }'#13 + '{$R- compiler directive}'#13 +
      'procedure TForm1.Button1Click(Sender: TObject);'#13 +
      'var  // Delphi Comment'#13 + '  Number, I, X: Integer;'#13 +
      'begin'#13 + '  Number := 12345;'#13 +
      '  Caption := ''The number is '' + IntToStr(Number);'#13 +
      '  asm'#13 + '    MOV AX,1234h'#13 + '    MOV Number,AX'#13 +
      '  end;'#13 + '  X := 10;'#13 +
      '  { Search Match, Text Block }'#13 +
      '  for I := 0 to Number do { execution point }'#13 +
      '  begin'#13 + '    Inc(X); { Enabled breakpoint }'#13 +
      '    Dec(X); { Disabled breakpoint }'#13 +
      '    // { Invalid breakpoint }'#13 +
      '    WriteLN(X); { Unknown breakpoint }'#13 +
      '    X := X + 1.0; { Error line }'#13 +
      '    ListBox1.Items.Add(IntToStr(X));'#13 + '  end;'#13 +
      'end;'#13 + #13;
    AddAttrSampleLines[ahaDisabledBreakpoint] := 18;
    AddAttrSampleLines[ahaEnabledBreakpoint] := 17;
    AddAttrSampleLines[ahaInvalidBreakpoint] := 19;
    AddAttrSampleLines[ahaUnknownBreakpoint] := 20;
    AddAttrSampleLines[ahaErrorLine] := 21;
    AddAttrSampleLines[ahaExecutionPoint] := 15;
    AddAttrSampleLines[ahaTextBlock] := 14;
  end;
  Add(NewInfo);

  // create info for html
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshHTML;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('htm;html');
    SampleSource :=
      '<html>'#13 + '<title>Lazarus Sample source for html</title>'#13 +
      '<body bgcolor=#ffffff background="bg.jpg">'#13 +
      '<!-- Comment -->'#13 + '<img src="lazarus.jpg">'#13 +
      '<p>'#13 + '  Some Text'#13 +
      '  Ampersands: &nbsp;F&nbsp;P&nbsp;C'#13 + '</p>'#13 +
      '<invalid_tag>'#13 + '<!-- Text Block -->'#13 +
      '</body>'#13 + '</html>'#13 + #13;
    AddAttrSampleLines[ahaTextBlock] := 11;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Space=Space');
    end;
  end;
  Add(NewInfo);

  // create info for cpp
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshCPP;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('c;cc;cpp;h;hpp;hh');
    SampleSource :=
      '/* Comment */'#13 + '#include <stdio.h>'#13 +
      '#include <stdlib.h>'#13 + #13 +
      'static char line_buf[LINE_BUF];'#13 + #13 +
      'int main(int argc,char **argv){'#13 + '  FILE *file;'#13 +
      '  line_buf[0]=0;'#13 + '  printf("\n");'#13 +
      '  return 0;'#13 + '}'#13 + ''#13 + #13;
    AddAttrSampleLines[ahaTextBlock] := 11;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
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
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshXML;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('xml;xsd;xsl;xslt;dtd;lpi;lps;lpk');
    SampleSource :=
      '<?xml version="1.0"?>'#13 + '<!DOCTYPE root ['#13 +
      '  ]>'#13 + '<!-- Comment -->'#13 + '<root version="&test;">'#13 +
      '  <![CDATA[ **CDATA section** ]]>'#13 + '</root>'#13 +
      '<!-- Text Block -->'#13 + ''#13 + #13;
    AddAttrSampleLines[ahaTextBlock] := 8;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Element=Reserved_word');
      Add('Comment=Comment');
      Add('Text=Identifier');
      Add('Space=Space');
      Add('Symbol=Symbol');
    end;
  end;
  Add(NewInfo);

  // create info for LFM
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshLFM;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('lfm;dfm;xfm');
    SampleSource :=
      '{ Lazarus Form Definitions }'#13 + 'object TestForm: TTestForm'#13 +
      '  Left = 273'#13 + '  Top = 103'#13 +
      '  Caption = ''sample source'''#13 + 'end'#13 +
      '{ Text Block }'#13 + ''#13 + #13;
    AddAttrSampleLines[ahaTextBlock] := 7;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
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
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshPerl;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('pl;pm;cgi');
    SampleSource :=
      '#!/usr/bin/perl'#13 + '# Perl sample code'#13 +
      ''#13 + '$i = "10";'#13 + 'print "$ENV{PATH}\n";'#13 +
      '($i =~ /\d+/) || die "Error\n";'#13 + ''#13 +
      '# Text Block'#13 + ''#13 + #13;
    AddAttrSampleLines[ahaTextBlock] := 8;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
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

  // create info for Java
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshJava;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('java');
    SampleSource :=
      '/* Java syntax highlighting */'#13#10 +
      'import java.util.*;'#13#10 + #13#10 +
      '/** Example class */'#13#10 +
      'public class Sample {'#13#10 +
      '  public static void main(String[] args) {'#13#10 +
      '    int i = 0;'#13#10 +
      '    for(i = 0; i < 10; i++)'#13#10 +
      '      System.out.println("Hello world");'#13#10 +
      '  }'#13#10 + '}'#13#10 +
      '/* Text Block */'#13#10 + #13#10;
    AddAttrSampleLines[ahaTextBlock] := 12;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Documentation=Comment');
      Add('Identifier=Identifier');
      Add('Reserved_word=Reserved_word');
      Add('Number=Number');
      Add('Space=Space');
      Add('String=String');
      Add('Symbol=Symbol');
    end;
  end;
  Add(NewInfo);

  // create info for Bash
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshBash;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('sh');
    SampleSource :=
      '#!/bin/bash'#13#13 +
      '# Bash syntax highlighting'#13#10 + 'set -x'#13#10 +
      'set -e'#13#10 +
      'Usage="Usage: $0 devel|stable"'#13#10 +
      'FPCVersion=$1'#13#10 +
      'for ver in devel stable; do'#13#10 +
      '  if [ "x$FPCVersion" = "x$ver" ]; then'#13#10 +
      '  fi'#13#10 + 'done'#13#10 +
      '# Text Block'#13#10 + #13#10;
    AddAttrSampleLines[ahaTextBlock] := 12;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Variable=Identifier');
      Add('Key=Reserved_word');
      Add('Number=Number');
      Add('Space=Space');
      Add('String=String');
      Add('Symbol=Symbol');
    end;
  end;
  Add(NewInfo);

  // create info for Python
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshPython;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('py');
    SampleSource :=
      '# Python syntax highlighting'#13#10 +
      'import math'#13#10 + #13#10 +
      '""" Documentation """'#13#10 +
      'def DoSomething(Liste1,Liste2,param3=3):'#13#10 +
      '  for i in Liste1:'#13#10 +
      '    if i in Liste2:'#13#10 +
      '      Liste1.remove(i)'#13#10 +
      '/* Text Block */'#13#10 + #13#10;
    AddAttrSampleLines[ahaTextBlock] := 9;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Identifier=Identifier');
      Add('Documentation=Comment');
      Add('Reserved_word=Reserved_word');
      Add('Number=Number');
      Add('Space=Space');
      Add('String=String');
      Add('Symbol=Symbol');
    end;
  end;
  Add(NewInfo);

  // create info for PHP
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshPHP;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('php;php3;php4');
    SampleSource :=
      '<?if ( ($HTTP_HOST == "www.lazarus.com") || ($HTTP_HOST == "lazarus.com") ){'#10 + '   HEADER("Location:http://www.lazarus.freepascal.org/\n\n");'#10
      + '};'#10 + '?>'#10 + #10;
    AddAttrSampleLines[ahaTextBlock] := 8;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Element=Reserved_word');
      Add('Comment=Comment');
      Add('Variable=Identifier');
      Add('Space=Space');
      Add('Symbol=Symbol');
      Add('Number=Number');
      Add('Key=Key');
      Add('String=String');
    end;
  end;
  Add(NewInfo);

  // create info for SQL
  NewInfo := TEditOptLanguageInfo.Create;
  with NewInfo do
  begin
    TheType := lshSQL;
    DefaultCommentType := DefaultCommentTypes[TheType];
    SynClass := LazSyntaxHighlighterClasses[TheType];
    SetBothFilextensions('sql');
    SampleSource :=
      '-- ansi sql sample source'#10 +
        'select name , region'#10 +
        'from cia'#10 +
        'where area < 2000'#10 +
        'and gdp > 5000000000'#10 + #10;
    AddAttrSampleLines[ahaTextBlock] := 4;
    MappedAttributes := TStringList.Create;
    with MappedAttributes do
    begin
      Add('Comment=Comment');
      Add('Element=Reserved_word');
      Add('Variable=Identifier');
      Add('Space=Space');
      Add('Symbol=Symbol');
      Add('Number=Number');
      Add('Key=Key');
      Add('String=String');
    end;
  end;
  Add(NewInfo);
end;

destructor TEditOptLangList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TEditOptLangList.FindByName(const Name: String): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (AnsiCompareText(
      Items[Result].SynClass.GetLanguageName, Name) <> 0) do
    dec(Result);
end;

function TEditOptLangList.FindByClass(
  CustomSynClass: TCustomSynClass): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (Items[Result].SynClass <> CustomSynClass) do
    dec(Result);
end;

function TEditOptLangList.FindByHighlighter(Hilighter:
  TSynCustomHighlighter): Integer;
begin
  if Hilighter <> Nil then
    Result := FindByClass(TCustomSynClass(Hilighter.ClassType))
  else
    Result := -1;
end;

function TEditOptLangList.FindByType(AType: TLazSyntaxHighlighter): Integer;
begin
  AType := CompatibleLazSyntaxHilighter[AType];
  Result := Count - 1;
  while (Result >= 0) and (Items[Result].TheType <> AType) do
    dec(Result);
end;

function TEditOptLangList.GetDefaultFilextension(
  AType: TLazSyntaxHighlighter): String;
var
  i: Integer;
begin
  i := FindByType(AType);
  if i >= 0 then
    Result := Items[i].GetDefaultFilextension
  else
    Result := '';
end;

function TEditOptLangList.GetInfoByType(AType: TLazSyntaxHighlighter
  ): TEditOptLanguageInfo;
var
  i: LongInt;
begin
  i:=FindByType(AType);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

{ TEditorOptions }

constructor TEditorOptions.Create;
var
  ConfFileName: String;
  fs: TFileStream;
  res: TLResource;
begin
  inherited Create;
  ConfFileName := SetDirSeparators(GetPrimaryConfigPath + '/' +
    EditOptsConfFileName);
  CopySecondaryConfigFile(EditOptsConfFileName);
  try
    if (not FileExists(ConfFileName)) then
    begin
      DebugLn('NOTE: editor options config file not found - using defaults');
      XMLConfig := TXMLConfig.CreateClean(ConfFileName);
    end
    else
      XMLConfig := TXMLConfig.Create(ConfFileName);
  except
    on E: Exception do
    begin
      DebugLn('WARNING: unable to read ', ConfFileName, ' ', E.Message);
      XMLConfig := Nil;
    end;
  end;

  // set defaults

  // General options
  fCtrlMouseLinks := True;
  fShowTabCloseButtons := True;
  FCopyWordAtCursorOnCopyNone := True;
  FShowGutterHints := True;
  fBlockIndent := 2;
  fUndoLimit := 32767;
  fTabWidth := 8;

  // Display options
  fEditorFont := 'courier';

  // Key Mappings
  fKeyMappingScheme := 'default';
  fKeyMap := TKeyCommandRelationList.Create;

  // Color options
  fHighlighterList := TEditOptLangList.Create;

  // Code Tools options
  fCodeTemplateFileName := SetDirSeparators(GetPrimaryConfigPath + '/lazarus.dci');
  CopySecondaryConfigFile('lazarus.dci');
  if not FileExists(fCodeTemplateFileName) then
  begin
    res := LazarusResources.Find('lazarus_dci_file');
    if (res <> Nil) and (res.Value <> '') and (res.ValueType = 'DCI') then
      try
        InvalidateFileStateCache;
        fs := TFileStream.Create(fCodeTemplateFileName, fmCreate);
        try
          fs.Write(res.Value[1], length(res.Value));
        finally
          fs.Free;
        end;
      except
        DebugLn('WARNING: unable to write code template file "',
          fCodeTemplateFileName, '"');
      end;
  end;
  
  // Code Folding
  FCFDividerDrawLevel := 3;
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
var
  SynEditOpt: TSynEditorOption;
  SynEditOptName: String;
  i: Integer;
  SynEditOpt2: TSynEditorOption2;
begin
  try
    // general options
    for SynEditOpt := Low(TSynEditorOption) to High(TSynEditorOption) do
    begin
      SynEditOptName := GetSynEditOptionName(SynEditOpt);
      if SynEditOptName <> '' then
        if XMLConfig.GetValue('EditorOptions/General/Editor/' + SynEditOptName,
          SynEditOpt in SynEditDefaultOptions) then
          Include(fSynEditOptions, SynEditOpt)
        else
          Exclude(fSynEditOptions, SynEditOpt);
    end;
    for SynEditOpt2 := Low(TSynEditorOption2) to High(TSynEditorOption2) do
    begin
      case SynEditOpt2 of
        eoCaretSkipsSelection:
          SynEditOptName := 'CaretSkipsSelection';
        eoAlwaysVisibleCaret:
          SynEditOptName := 'AlwaysVisibleCaret';
        else
          SynEditOptName := '';
      end;
      if SynEditOptName <> '' then
        if XMLConfig.GetValue('EditorOptions/General/Editor/' + SynEditOptName,
          SynEditOpt2 in SynEditDefaultOptions2) then
          Include(fSynEditOptions2, SynEditOpt2)
        else
          Exclude(fSynEditOptions2, SynEditOpt2);
    end;

    fCtrlMouseLinks :=
      XMLConfig.GetValue('EditorOptions/General/Editor/CtrlMouseLinks', True);
    fShowTabCloseButtons :=
      XMLConfig.GetValue(
      'EditorOptions/General/Editor/ShowTabCloseButtons', True);
    FCopyWordAtCursorOnCopyNone :=
      XMLConfig.GetValue(
      'EditorOptions/General/Editor/CopyWordAtCursorOnCopyNone', True);
    FShowGutterHints :=
      XMLConfig.GetValue('EditorOptions/General/Editor/ShowGutterHints', True);
    fUndoAfterSave :=
      XMLConfig.GetValue('EditorOptions/General/Editor/UndoAfterSave', True);
    fFindTextAtCursor :=
      XMLConfig.GetValue('EditorOptions/General/Editor/FindTextAtCursor', True);
    fUseSyntaxHighlight :=
      XMLConfig.GetValue(
      'EditorOptions/General/Editor/UseSyntaxHighlight', True);
    fBlockIndent :=
      XMLConfig.GetValue('EditorOptions/General/Editor/BlockIndent', 2);
    fUndoLimit :=
      XMLConfig.GetValue('EditorOptions/General/Editor/UndoLimit', 32767);
    fTabWidth :=
      XMLConfig.GetValue('EditorOptions/General/Editor/TabWidth', 8);

    // Display options
    fVisibleRightMargin :=
      XMLConfig.GetValue('EditorOptions/Display/VisibleRightMargin', True);
    fVisibleGutter :=
      XMLConfig.GetValue('EditorOptions/Display/VisibleGutter', True);
    fShowLineNumbers :=
      XMLConfig.GetValue('EditorOptions/Display/ShowLineNumbers', False);
    fGutterColor :=
      XMLConfig.GetValue('EditorOptions/Display/GutterColor', clBtnFace);
    fGutterWidth :=
      XMLConfig.GetValue('EditorOptions/Display/GutterWidth', 30);
    fRightMargin :=
      XMLConfig.GetValue('EditorOptions/Display/RightMargin', 80);
    fRightMarginColor :=
      XMLConfig.GetValue('EditorOptions/Display/VisibleRightMarginColor'
      , clBtnFace);
    fEditorFont  :=
      XMLConfig.GetValue('EditorOptions/Display/EditorFont', 'courier');
    fEditorFontHeight :=
      XMLConfig.GetValue('EditorOptions/Display/EditorFontHeight', 12);
    fExtraLineSpacing :=
      XMLConfig.GetValue('EditorOptions/Display/ExtraLineSpacing', 1);
    FDoNotWarnForFont :=
      XMLConfig.GetValue('EditorOptions/Display/DoNotWarnForFont', '');

    // Key Mappings options
    fKeyMappingScheme :=
      XMLConfig.GetValue('EditorOptions/KeyMapping/Scheme',
      StrToValidXMLName(fKeyMappingScheme));
    fKeyMap.LoadFromXMLConfig(XMLConfig
      , 'EditorOptions/KeyMapping/' + fKeyMappingScheme + '/');

    // Color options
    for i := 0 to fHighlighterList.Count - 1 do
      fHighlighterList[i].FileExtensions :=
        XMLConfig.GetValue('EditorOptions/Color/Lang' +
        StrToValidXMLName(fHighlighterList[i].SynClass.GetLanguageName) +
        '/FileExtensions/Value', fHighlighterList[i].DefaultFileExtensions)
      // color attributes are stored in the highlighters
    ;

    // Code Tools options
    fAutoIdentifierCompletion :=
      XMLConfig.GetValue(
      'EditorOptions/CodeTools/AutoIdentifierCompletion', True);
    fAutoCodeParameters :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoCodeParameters', True);
    fAutoToolTipExprEval :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoToolTipExprEval', True);
    fAutoToolTipSymbTools :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoToolTipSymbTools', True);
    fAutoDelayInMSec    :=
      XMLConfig.GetValue('EditorOptions/CodeTools/AutoDelayInMSec', 1000);
    fCodeTemplateFileName :=
      XMLConfig.GetValue('EditorOptions/CodeTools/CodeTemplateFileName'
      , SetDirSeparators(GetPrimaryConfigPath + '/lazarus.dci'));
    fCTemplIndentToTokenStart :=
      XMLConfig.GetValue(
      'EditorOptions/CodeTools/CodeTemplateIndentToTokenStart/Value', False);

    // Code Folding
    FUseCodeFolding :=
      XMLConfig.GetValue(
      'EditorOptions/CodeFolding/UseCodeFolding', True);
    FCFDividerDrawLevel :=
      XMLConfig.GetValue('EditorOptions/CodeFolding/DividerDrawLevel', 3);
  except
    on E: Exception do
      DebugLn('[TEditorOptions.Load] ERROR: ', e.Message);
  end;
end;

procedure TEditorOptions.Save;
// save options to XML file
var
  SynEditOpt: TSynEditorOption;
  SynEditOptName: String;
  i: Integer;
  SynEditOpt2: TSynEditorOption2;
begin
  try
    XMLConfig.SetValue('EditorOptions/Version', EditorOptsFormatVersion);

    // general options
    for SynEditOpt := Low(TSynEditorOption) to High(TSynEditorOption) do
    begin
      SynEditOptName := GetSynEditOptionName(SynEditOpt);
      if SynEditOptName <> '' then
        XMLConfig.SetDeleteValue('EditorOptions/General/Editor/' + SynEditOptName,
          SynEditOpt in fSynEditOptions, SynEditOpt in SynEditDefaultOptions);
    end;
    // general options
    for SynEditOpt2 := Low(TSynEditorOption2) to High(TSynEditorOption2) do
    begin
      case SynEditOpt2 of
        eoCaretSkipsSelection:
          SynEditOptName := 'CaretSkipsSelection';
        eoAlwaysVisibleCaret:
          SynEditOptName := 'AlwaysVisibleCaret';
        else
          SynEditOptName := '';
      end;
      if SynEditOptName <> '' then
        XMLConfig.SetDeleteValue('EditorOptions/General/Editor/' + SynEditOptName,
          SynEditOpt2 in fSynEditOptions2, SynEditOpt2 in SynEditDefaultOptions2);
    end;

    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/CtrlMouseLinks'
      , fCtrlMouseLinks, True);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/ShowTabCloseButtons'
      , fShowTabCloseButtons, True);
    XMLConfig.SetDeleteValue(
      'EditorOptions/General/Editor/CopyWordAtCursorOnCopyNone',
      FCopyWordAtCursorOnCopyNone, True);
    XMLConfig.SetDeleteValue(
      'EditorOptions/General/Editor/ShowGutterHints',
      FShowGutterHints, True);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/UndoAfterSave'
      , fUndoAfterSave, True);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/FindTextAtCursor'
      , fFindTextAtCursor, True);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/UseSyntaxHighlight'
      , fUseSyntaxHighlight, True);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/BlockIndent'
      , fBlockIndent, 2);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/UndoLimit'
      , fUndoLimit, 32767);
    XMLConfig.SetDeleteValue('EditorOptions/General/Editor/TabWidth'
      , fTabWidth, 8);

    // Display options
    XMLConfig.SetDeleteValue('EditorOptions/Display/VisibleRightMargin'
      , fVisibleRightMargin, True);
    XMLConfig.SetDeleteValue('EditorOptions/Display/VisibleGutter',
      fVisibleGutter, True);
    XMLConfig.SetDeleteValue('EditorOptions/Display/ShowLineNumbers',
      fShowLineNumbers, False);
    XMLConfig.SetDeleteValue('EditorOptions/Display/GutterColor',
      fGutterColor, clBtnFace);
    XMLConfig.SetDeleteValue('EditorOptions/Display/GutterWidth',
      fGutterWidth, 30);
    XMLConfig.SetDeleteValue('EditorOptions/Display/RightMargin',
      fRightMargin, 80);
    XMLConfig.SetDeleteValue('EditorOptions/Display/RightMarginColor',
      fRightMarginColor, clBtnFace);
    XMLConfig.SetDeleteValue('EditorOptions/Display/EditorFont',
      fEditorFont, 'courier');
    XMLConfig.SetDeleteValue('EditorOptions/Display/EditorFontHeight'
      ,fEditorFontHeight, 12);
    XMLConfig.SetDeleteValue('EditorOptions/Display/ExtraLineSpacing'
      ,fExtraLineSpacing, 1);
    XMLConfig.SetDeleteValue('EditorOptions/Display/DoNotWarnForFont'
      ,FDoNotWarnForFont, '');

    // Key Mappings options
    XMLConfig.SetValue('EditorOptions/KeyMapping/Scheme', fKeyMappingScheme);
    fKeyMap.SaveToXMLConfig(
              XMLConfig, 'EditorOptions/KeyMapping/' + fKeyMappingScheme + '/');

    // Color options
    for i := 0 to fHighlighterList.Count - 1 do
      XMLConfig.SetDeleteValue('EditorOptions/Color/Lang' +
        StrToValidXMLName(fHighlighterList[i].SynClass.GetLanguageName) +
        '/FileExtensions/Value', fHighlighterList[i].FileExtensions,
        fHighlighterList[i].DefaultFileExtensions)
      // color attributes are stored in the highlighters
    ;

    // Code Tools options
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/AutoIdentifierCompletion'
      , fAutoIdentifierCompletion, True);
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/AutoCodeParameters'
      , fAutoCodeParameters, True);
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/AutoToolTipExprEval'
      , fAutoToolTipExprEval, True);
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/AutoToolTipSymbTools'
      , fAutoToolTipSymbTools, True);
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/AutoDelayInMSec'
      , fAutoDelayInMSec, 1000);
    XMLConfig.SetDeleteValue('EditorOptions/CodeTools/CodeTemplateFileName'
      , fCodeTemplateFileName, '');
    XMLConfig.SetDeleteValue(
      'EditorOptions/CodeTools/CodeTemplateIndentToTokenStart/Value'
      , fCTemplIndentToTokenStart, False);

    // Code Folding
    XMLConfig.SetDeleteValue('EditorOptions/CodeFolding/UseCodeFolding',
        FUseCodeFolding, True);
    XMLConfig.SetDeleteValue('EditorOptions/CodeFolding/DividerDrawLevel',
        FCFDividerDrawLevel, 3);

    InvalidateFileStateCache;
    XMLConfig.Flush;
  except
    on E: Exception do
      DebugLn('[TEditorOptions.Save] ERROR: ', e.Message);
  end;
end;

function TEditorOptions.GetSynEditOptionName(SynOption: TSynEditorOption
  ): string;
begin
  case SynOption of
    eoAltSetsColumnMode:
      Result := 'AltSetsColumnMode';
    eoAutoIndent:
      Result := 'AutoIndent';
    eoBracketHighlight:
      Result := 'BracketHighlight';
    eoDoubleClickSelectsLine:
      Result := 'DoubleClickSelectsLine';
    eoDragDropEditing:
      Result := 'DragDropEditing';
    eoDropFiles:
      Result := 'DropFiles';
    eoEnhanceHomeKey:
      Result := 'EnhanceHomeKey';
    eoGroupUndo:
      Result := 'GroupUndo';
    eoHalfPageScroll:
      Result := 'HalfPageScroll';
    eoKeepCaretX:
      Result := 'KeepCaretX';
    eoPersistentCaret:
      Result := 'PersistentCaret';
    eoScrollByOneLess:
      Result := 'ScrollByOneLess';
    eoScrollPastEof:
      Result := 'ScrollPastEof';
    eoScrollPastEol:
      Result := 'ScrollPastEol';
    eoShowScrollHint:
      Result := 'ShowScrollHint';
    eoSmartTabs:
      Result := 'SmartTabs';
    eoTabsToSpaces:
      Result := 'TabsToSpaces';
    eoTabIndent:
      Result := 'TabIndent';
    eoTrimTrailingSpaces:
      Result := 'TrimTrailingSpaces';
    else
      Result := '';
  end;
end;

function TEditorOptions.CreateSyn(LazSynHilighter: TLazSyntaxHighlighter):
TCustomSyn;
begin
  if LazSyntaxHighlighterClasses[LazSynHilighter] <> Nil then
  begin
    Result := LazSyntaxHighlighterClasses[LazSynHilighter].Create(Nil);
    AddSpecialHilightAttribsToHighlighter(Result);
    GetHighlighterSettings(Result);
  end
  else
    Result := Nil;
end;

function TEditorOptions.ReadColorScheme(const LanguageName: String): String;
begin
  if LanguageName = '' then
  begin
    Result := DefaultColorScheme;
    exit;
  end;
  if LanguageName <> TPreviewPasSyn.GetLanguageName then
    Result := XMLConfig.GetValue(
      'EditorOptions/Color/Lang' + StrToValidXMLName(LanguageName) +
      '/ColorScheme/Value', '')
  else
    Result := '';
  if Result = '' then
    Result := ReadPascalColorScheme;
end;

function TEditorOptions.ReadPascalColorScheme: String;
var
  FormatVersion: Integer;
begin
  FormatVersion := XMLConfig.GetValue('EditorOptions/Color/Version', 0);
  if FormatVersion > 1 then
    Result := XMLConfig.GetValue(
      'EditorOptions/Color/Lang' + StrToValidXMLName(
      TPreviewPasSyn.GetLanguageName) + '/ColorScheme/Value', '')
  else
    Result := XMLConfig.GetValue('EditorOptions/Color/ColorScheme', '');
  if Result = '' then
    Result := DefaultColorScheme;
end;

procedure TEditorOptions.WriteColorScheme(
  const LanguageName, SynColorScheme: String);
begin
  if (LanguageName = '') or (SynColorScheme = '') then
    exit;
  XMLConfig.SetValue('EditorOptions/Color/Lang' + StrToValidXMLName(
    LanguageName) + '/ColorScheme/Value', SynColorScheme);
  XMLConfig.SetValue('EditorOptions/Color/Version', EditorOptsFormatVersion);
end;

procedure TEditorOptions.GetDefaultsForPascalAttribute(
  Attr: TSynHighlightElement; const SynColorScheme: String);
var
  AttriName: String;
  DefBGCol, DefFGCol: TColor;
  DefFontStyles: TFontStyles;
begin
  AttriName := Attr.Name;
  if AttriName = '' then
    exit;

  DefFGCol := clNone;
  DefBGCol := clNone;
  DefFontStyles := [];
  if lowercase(SynColorScheme) = 'twilight' then
  begin
    // default for twilight color scheme
    DefBGCol := clBlack;
    DefFGCol := clWhite;
    if AttriName = 'Assembler' then
      DefFGCol := clLime
    else
    if AttriName = 'Comment' then
      DefFGCol := clGray
    else
    if AttriName = 'Directive' then
      DefFGCol := clRed
    else
    if AttriName = 'Reserved word' then
    begin
      DefFGCol := clAqua;
      DefFontStyles := [fsBold];
    end
    else
    if AttriName = 'Number' then
      DefFGCol := clFuchsia
    else
    if AttriName = 'String' then
      DefFGCol := clYellow
    else
    if AttriName = 'Symbol' then
      DefFGCol := clAqua
    else
    if AttriName = AdditionalHighlightAttributes[ahaTextBlock] then
    begin
      DefBGCol := clWhite;
      DefFGCol := clBlack;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaExecutionPoint] then
    begin
      DefBGCol := clBlue;
      DefFGCol := clWhite;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaEnabledBreakpoint] then
    begin
      DefBGCol := clRed;
      DefFGCol := clWhite;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaDisabledBreakpoint] then
    begin
      DefBGCol := clLime;
      DefFGCol := clRed;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaInvalidBreakpoint] then
    begin
      DefBGCol := clOlive;
      DefFGCol := clGreen;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaUnknownBreakpoint] then
    begin
      DefBGCol := clRed;
      DefFGCol := clBlack;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaErrorLine] then
    begin
      DefBGCol := $50a0ff;
      DefFGCol := clBlack;
    end;
  end
  else
  if lowercase(SynColorScheme) = 'pascal classic' then
  begin
    // defaults for pascal classic color scheme
    DefBGCol := clNavy;
    DefFGCol := clYellow;
    if AttriName = 'Assembler' then
    begin
      DefBGCol := clNone;
      DefFGCol := clLime;
    end
    else
    if AttriName = 'Comment' then
    begin
      DefBGCol := clNone;
      DefFGCol := clSilver;
    end
    else
    if AttriName = 'Directive' then
    begin
      DefBGCol := clNone;
      DefFGCol := clSilver;
    end
    else
    if AttriName = 'Reserved word' then
    begin
      DefBGCol := clNone;
      DefFGCol := clWhite;
    end
    else
    if AttriName = 'Number' then
    begin
      DefBGCol := clNone;
      DefFGCol := clYellow;
    end
    else
    if AttriName = 'String' then
    begin
      DefBGCol := clNone;
      DefFGCol := clYellow;
    end
    else
    if AttriName = 'Symbol' then
    begin
      DefBGCol := clNone;
      DefFGCol := clYellow;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaTextBlock] then
    begin
      DefBGCol := clBlue;
      DefFGCol := clWhite;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaExecutionPoint] then
    begin
      DefBGCol := clAqua;
      DefFGCol := clBlack;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaEnabledBreakpoint] then
    begin
      DefBGCol := clRed;
      DefFGCol := clWhite;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaDisabledBreakpoint] then
    begin
      DefBGCol := clLime;
      DefFGCol := clRed;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaInvalidBreakpoint] then
    begin
      DefBGCol := clOlive;
      DefFGCol := clLime;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaUnknownBreakpoint] then
    begin
      DefBGCol := clNone;
      DefFGCol := clNone;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaErrorLine] then
    begin
      DefBGCol := clMaroon;
      DefFGCol := clWhite;
    end;
  end
  else
  if lowercase(SynColorScheme) = 'ocean' then
  begin
    // default for ocean color scheme
    DefBGCol := clNavy;
    DefFGCol := clYellow;
    if AttriName = 'Assembler' then
      DefFGCol := clLime
    else
    if AttriName = 'Comment' then
      DefFGCol := clGray
    else
    if AttriName = 'Directive' then
      DefFGCol := clRed
    else
    if AttriName = 'Reserved word' then
    begin
      DefFGCol := clAqua;
      DefFontStyles := [fsBold];
    end
    else
    if AttriName = 'Number' then
      DefFGCol := clFuchsia
    else
    if AttriName = 'String' then
      DefFGCol := clYellow
    else
    if AttriName = 'Symbol' then
      DefFGCol := clAqua
    else
    if AttriName = AdditionalHighlightAttributes[ahaTextBlock] then
    begin
      DefBGCol := clWhite;
      DefFGCol := clBlack;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaExecutionPoint] then
    begin
      DefBGCol := clBlue;
      DefFGCol := clWhite;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaEnabledBreakpoint] then
    begin
      DefBGCol := clRed;
      DefFGCol := clWhite;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaDisabledBreakpoint] then
    begin
      DefBGCol := clLime;
      DefFGCol := clRed;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaInvalidBreakpoint] then
    begin
      DefBGCol := clOlive;
      DefFGCol := clGreen;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaUnknownBreakpoint] then
    begin
      DefBGCol := clRed;
      DefFGCol := clBlack;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaErrorLine] then
    begin
      DefBGCol := $50a0ff;
      DefFGCol := clBlack;
    end;
  end
  else
  if lowercase(SynColorScheme) = 'delphi' then
  begin
    // default for delphi color scheme
    if AttriName = 'Assembler' then
      DefFGCol := clBlack
    else
    if AttriName = 'Comment' then
    begin
      DefFGCol := clNavy;
      DefFontStyles := [fsItalic];
    end
    else
    if AttriName = 'Directive' then
      DefFGCol := clGreen
    else
    if AttriName = 'Reserved word' then
    begin
      DefFGCol := clBlack;
      DefFontStyles := [fsBold];
    end
    else
    if AttriName = 'Number' then
      DefFGCol := clNavy
    else
    if AttriName = 'String' then
      DefFGCol := clNavy
    else
    if AttriName = 'Symbol' then
      DefFGCol := clBlack
    else
    if AttriName = AdditionalHighlightAttributes[ahaTextBlock] then
    begin
      DefBGCol := clHighlight;
      DefFGCol := clHighlightText;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaExecutionPoint] then
    begin
      DefBGCol := clNavy;
      DefFGCol := clWhite;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaEnabledBreakpoint] then
    begin
      DefBGCol := clRed;
      DefFGCol := clWhite;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaDisabledBreakpoint] then
    begin
      DefBGCol := clLime;
      DefFGCol := clRed;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaInvalidBreakpoint] then
    begin
      DefBGCol := clOlive;
      DefFGCol := clLime;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaUnknownBreakpoint] then
    begin
      DefBGCol := clRed;
      DefFGCol := clBlack;
    end
    else
    if AttriName = AdditionalHighlightAttributes[ahaErrorLine] then
    begin
      DefBGCol := clMaroon;
      DefFGCol := clWhite;
    end;
  end
  else
  if AttriName = 'Assembler' then
    DefFGCol := clGreen
  else
  if AttriName = 'Comment' then
  begin
    DefFGCol := clBlue;
    DefFontStyles := [fsBold];
  end
  else
  if AttriName = 'Directive' then
  begin
    DefFGCol := clRed;
    DefFontStyles := [fsBold];
  end
  else
  if AttriName = 'Reserved word' then
    DefFontStyles := [fsBold]
  else
  if AttriName = 'Number' then
    DefFGCol := clNavy
  else
  if AttriName = 'String' then
    DefFGCol := clBlue
  else
  if AttriName = 'Symbol' then
    DefFGCol := clRed
  else
  if AttriName = AdditionalHighlightAttributes[ahaTextBlock] then
  begin
    DefBGCol := clNavy;
    DefFGCol := clWhite;
  end
  else
  if AttriName = AdditionalHighlightAttributes[ahaExecutionPoint] then
  begin
    DefBGCol := clDKGray;
    DefFGCol := clWhite;
  end
  else
  if AttriName = AdditionalHighlightAttributes[ahaEnabledBreakpoint] then
  begin
    DefBGCol := clRed;
    DefFGCol := clBlack;
  end
  else
  if AttriName = AdditionalHighlightAttributes[ahaDisabledBreakpoint] then
  begin
    DefBGCol := clGreen;
    DefFGCol := clBlack;
  end
  else
  if AttriName = AdditionalHighlightAttributes[ahaErrorLine] then
  begin
    DefBGCol := $50a0ff;
    DefFGCol := clBlack;
  end// default for all other color schemes
  ;

  Attr.Foreground := DefFGCol;
  Attr.Background := DefBGCol;
  Attr.Style      := DefFontStyles;
end;

procedure TEditorOptions.ReadDefaultsForHighlighterSettings(Syn: TCustomSyn;
  SynColorScheme: String; DefaultPascalSyn: TPreviewPasSyn);
// if SynColorScheme='' then default ColorScheme will be used
var
  VirginSyn, DefaultSyn: TCustomSyn;
  i, j: Integer;
  MappedAttriName, AttriName: String;
  HilightInfo: TEditOptLanguageInfo;
  aha:  TAdditionalHilightAttribute;
  CustomPascalSyn: Boolean;
begin
  if SynColorScheme = '' then
    SynColorScheme := ReadColorScheme(Syn.LanguageName);
  if SynColorScheme = '' then
    exit;
  CustomPascalSyn := (DefaultPascalSyn <> Nil);
  if (Syn is TPreviewPasSyn) then
    for i := 0 to Syn.AttrCount - 1 do
      GetDefaultsForPascalAttribute(Syn.Attribute[i], SynColorScheme)
    // the defaults for pascal are fix programmed
  else
  begin
    // the defaults of all non pascal languages are the mapped current values of
    // pascal or the non mapped values of an untouched highlighter of the same
    // type
    i := HighlighterList.FindByClass(TCustomSynClass(Syn.ClassType));
    if i < 0 then
      exit;
    HilightInfo := HighlighterList[i];
    if not CustomPascalSyn then
      DefaultPascalSyn := TPreviewPasSyn.Create(Nil);
    VirginSyn := TCustomSynClass(Syn.ClassType).Create(Nil);
    try
      if not CustomPascalSyn then
      begin
        AddSpecialHilightAttribsToHighlighter(DefaultPascalSyn);
        ReadHighlighterSettings(DefaultPascalSyn, SynColorScheme);
      end;
      // map attributes
      for i := 0 to Syn.AttrCount - 1 do
      begin
        AttriName := StrToValidXMLName(Syn.Attribute[i].Name);
        if AttriName = '' then
          continue;
        // check, if there is a known mapping for this attribute
        if HilightInfo.MappedAttributes <> Nil then
          MappedAttriName :=
            HilightInfo.MappedAttributes.Values[AttriName]
        else
          MappedAttriName := '';
        if MappedAttriName = '' then
          for aha := Low(TAdditionalHilightAttribute)
            to High(TAdditionalHilightAttribute) do
            if AnsiCompareText(
              StrToValidXMLName(AdditionalHighlightAttributes[aha]), AttriName) =
              0 then
              MappedAttriName :=
                AttriName// all special line color attributes can be mapped 1:1
        ;
        if MappedAttriName <> '' then
          DefaultSyn := DefaultPascalSyn
        else
          DefaultSyn := VirginSyn;
        // read defaults
        j := DefaultSyn.AttrCount - 1;
        while (j >= 0) do
        begin
          if AnsiCompareText(StrToValidXMLName(DefaultSyn.Attribute[j].Name),
            MappedAttriName) = 0 then
          begin
            CopyHiLightAttributeValues(DefaultSyn.Attribute[j],
              Syn.Attribute[i]);
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
  SynColorScheme: String);
// if SynColorScheme='' then default ColorScheme will be used
var
  FormatVersion: Integer;
  i: Integer;
  AttriName: String;
  Attri: TSynHighlightElement;
  b: Boolean;
  fs: TFontStyles;
  Path: String;
begin
  // initialize with defaults
  if SynColorScheme = '' then
    SynColorScheme := ReadColorScheme(Syn.LanguageName);
  if (SynColorScheme = '') or (Syn.LanguageName = '') then
    exit;
  ReadDefaultsForHighlighterSettings(Syn, SynColorScheme, Nil);
  // read settings, that are different from the defaults
  FormatVersion := XMLConfig.GetValue(
    'EditorOptions/Color/Lang' + StrToValidXMLName(Syn.LanguageName) +
    '/Version', 0);
  if FormatVersion > 1 then
    for i := 0 to Syn.AttrCount - 1 do
    begin
      Attri := Syn.Attribute[i];
      AttriName := StrToValidXMLName(Attri.Name);
      if AttriName = '' then
        continue;
      Path := 'EditorOptions/Color/Lang' + StrToValidXMLName(
        Syn.LanguageName) + '/Scheme' + StrToValidXMLName(
        SynColorScheme) + '/' + StrToValidXMLName(AttriName) + '/';
      Attri.BackGround := XMLConfig.GetValue(Path + 'BackgroundColor/Value',
        Attri.Background);
      Attri.ForeGround := XMLConfig.GetValue(Path + 'ForegroundColor/Value',
        Attri.Foreground);
      fs   := [];
      b    := XMLConfig.GetValue(Path + 'Style/Bold', fsBold in Attri.Style);
      if b then
        Include(fs, fsBold);
      b := XMLConfig.GetValue(Path + 'Style/Italic', fsItalic in Attri.Style);
      if b then
        Include(fs, fsItalic);
      b := XMLConfig.GetValue(Path + 'Style/Underline', fsUnderline in Attri.Style);
      if b then
        Include(fs, fsUnderline);
      Attri.Style := fs;
    end// read all attributes
  else
  if Syn is TPreviewPasSyn then
    for i := 0 to Syn.AttrCount - 1 do
    begin
      Attri := Syn.Attribute[i];
      AttriName := StrToValidXMLName(Attri.Name);
      if AttriName = '' then
        continue;
      Path := 'EditorOptions/Color/' + StrToValidXMLName(
        SynColorScheme) + '/' + StrToValidXMLName(AttriName) + '/';
      Attri.BackGround := XMLConfig.GetValue(Path + 'BackgroundColor',
        Attri.Background);
      Attri.ForeGround := XMLConfig.GetValue(Path + 'ForegroundColor',
        Attri.Foreground);
      fs   := [];
      b    := XMLConfig.GetValue(Path + 'Bold', fsBold in Attri.Style);
      if b then
        Include(fs, fsBold);
      b := XMLConfig.GetValue(Path + 'Italic', fsItalic in Attri.Style);
      if b then
        Include(fs, fsItalic);
      b := XMLConfig.GetValue(Path + 'Underline', fsUnderline in Attri.Style);
      if b then
        Include(fs, fsUnderline);
      Attri.Style := fs;
    end// FormatVersion < 2
       // the oldest format only supports pascal
  ;
end;

procedure TEditorOptions.WriteHighlighterSettings(Syn: TCustomSyn;
  SynColorScheme: String);
var
  OldSyn: TCustomSyn;
  i:      Integer;
  AttriName: String;
  Attri, OldAttri: TSynHighlightElement;
  Path:   String;
begin
  // read the old settings, compare and write only the differences
  if SynColorScheme = '' then
    SynColorScheme := ReadColorScheme(Syn.LanguageName);
  OldSyn := TCustomSynClass(Syn.ClassType).Create(Nil);
  try
    AddSpecialHilightAttribsToHighlighter(OldSyn);
    ReadHighlighterSettings(OldSyn, SynColorScheme);
    // write colorscheme
    XMLConfig.SetValue('EditorOptions/Color/Lang' +
      StrToValidXMLName(Syn.LanguageName) + '/Version',
      EditorOptsFormatVersion);
    // write all attributes
    for i := 0 to Syn.AttrCount - 1 do
    begin
      Attri := Syn.Attribute[i];
      OldAttri := OldSyn.Attribute[i];
      AttriName := StrToValidXMLName(Attri.Name);
      if AttriName = '' then
        continue;
      Path := 'EditorOptions/Color/Lang' + StrToValidXMLName(
        Syn.LanguageName) + '/Scheme' + StrToValidXMLName(
        SynColorScheme) + '/' + StrToValidXMLName(AttriName) + '/';
      if Attri.Background <> OldAttri.Background then
        XMLConfig.SetValue(Path + 'BackgroundColor/Value', Attri.Background);
      if Attri.Foreground <> OldAttri.Foreground then
        XMLConfig.SetValue(Path + 'ForegroundColor/Value', Attri.Foreground);
      if Attri.Style <> OldAttri.Style then
      begin
        XMLConfig.SetValue(Path + 'Style/Bold', fsBold in Attri.Style);
        XMLConfig.SetValue(Path + 'Style/Italic', fsItalic in Attri.Style);
        XMLConfig.SetValue(Path + 'Style/Underline', fsUnderline in Attri.Style);
      end;
    end;
  finally
    OldSyn.Free;
  end;
end;

procedure TEditorOptions.GetHighlighterSettings(Syn: TCustomSyn);
// read highlight settings from config file
begin
  ReadHighlighterSettings(Syn, '');
end;

procedure TEditorOptions.SetHighlighterSettings(Syn: TCustomSyn);
// write highlight settings to config file
begin
  WriteHighlighterSettings(Syn, '');
end;

procedure TEditorOptions.GetSpecialLineColors(Syn: TCustomSyn;
  AddHilightAttr: TAdditionalHilightAttribute; var Special: Boolean;
  var FG, BG: TColor);
var
  i: Integer;
  NewFG, NewBG: TColor;
begin
  if Syn <> Nil then
    for i := 0 to Syn.AttrCount - 1 do
    begin
      if Syn.Attribute[i].Name = '' then
        continue;
      if Syn.Attribute[i].Name = AdditionalHighlightAttributes[AddHilightAttr]
      then
      begin
        NewFG := Syn.Attribute[i].Foreground;
        NewBG := Syn.Attribute[i].Background;
        Special := (NewFG <> clNone) or (NewBG <> clNone);
        if NewFG <> clNone then
          FG := NewFG;
        if NewBG <> clNone then
          BG := NewBG;
        exit;
      end;
    end;
  // set default
  case AddHilightAttr of
    ahaTextBlock:
    begin
      NewBG := clNavy;
      NewFG := clWhite;
    end;
    ahaExecutionPoint:
    begin
      NewBG := clDKGray;
      NewFG := clWhite;
    end;
    ahaEnabledBreakpoint:
    begin
      NewBG := clRed;
      NewFG := clWhite;
    end;
    ahaDisabledBreakpoint:
    begin
      NewBG := clGreen;
      NewFG := clBlack;
    end;
    ahaInvalidBreakpoint:
    begin
      NewBG := clOlive;
      NewFG := clGreen;
    end;
    ahaUnknownBreakpoint:
    begin
      NewBG := clRed;
      NewFG := clBlack;
    end;
    ahaErrorLine:
    begin
      NewBG := $50a0ff;
      NewFG := clBlack;
    end;
    else
    begin
      NewBG := clWhite;
      NewFG := clBlack;
    end;
  end;
  Special := (NewFG <> clNone) or (NewBG <> clNone);
  if NewFG <> clNone then
    FG := NewFG;
  if NewBG <> clNone then
    BG := NewBG;
end;

procedure TEditorOptions.GetSynEditSelectedColor(ASynEdit: TSynEdit);
var
  i: Integer;
begin
  if ASynEdit.Highlighter <> Nil then
    for i := 0 to ASynEdit.Highlighter.AttrCount - 1 do
      with ASynEdit.Highlighter.Attribute[i] do
      begin
        if Name = '' then
          continue;
        if AnsiCompareText(StrToValidXMLName(Name),
          StrToValidXMLName(
          AdditionalHighlightAttributes[ahaTextBlock])) = 0 then
        begin
          ASynEdit.SelectedColor.Background := Background;
          ASynEdit.SelectedColor.Foreground := Foreground;
          exit;
        end;
      end;
  // set defaults
  ASynEdit.SelectedColor.Background := clBlue;
  ASynEdit.SelectedColor.Foreground := clWhite;
end;

procedure TEditorOptions.GetSynEditSettings(ASynEdit: TSynEdit);
// read synedit setings from config file
begin
  // general options
  ASynEdit.Options := fSynEditOptions;
  ASynEdit.Options2 := fSynEditOptions2;
  ASynEdit.BlockIndent := fBlockIndent;
  ASynEdit.TabWidth := fTabWidth;

  // Display options
  ASynEdit.Gutter.Visible := fVisibleGutter;
  ASynEdit.Gutter.ShowLineNumbers := fShowLineNumbers;
  //ASynEdit.Gutter.AutoSize:= fShowLineNumbers;
  if ASynEdit.Gutter.ShowCodeFolding<>FUseCodeFolding then begin
    ASynEdit.Gutter.ShowCodeFolding := FUseCodeFolding;
    if not FUseCodeFolding then
      ASynEdit.UnfoldAll;
  end;
  ASynEdit.Gutter.Color := fGutterColor;
  ASynEdit.Gutter.Width := fGutterWidth;
  ASynEdit.Gutter.LeftOffset := 2*11; // bookmark + breakpoint
  if fVisibleRightMargin then
    ASynEdit.RightEdge := fRightMargin
  else
    ASynEdit.RightEdge := 0;
  ASynEdit.RightEdgeColor := fRightMarginColor;
  ASynEdit.Font.Height := fEditorFontHeight;// set height before name for XLFD !
  ASynEdit.Font.Name := fEditorFont;
  ASynEdit.ExtraLineSpacing := fExtraLineSpacing;
  ASynEdit.MaxUndo := fUndoLimit;
  GetSynEditSelectedColor(ASynEdit);
  
  // Code Folding
  ASynEdit.CFDividerDrawLevel := FCFDividerDrawLevel;

  KeyMap.AssignTo(ASynEdit.KeyStrokes, TSourceEditorWindowInterface);
end;

procedure TEditorOptions.SetSynEditSettings(ASynEdit: TSynEdit);
// write synedit settings to file
begin
  // general options
  fSynEditOptions := ASynEdit.Options;
  fSynEditOptions2 := ASynEdit.Options2;
  fBlockIndent := ASynEdit.BlockIndent;
  fTabWidth := ASynEdit.TabWidth;

  // Display options
  fVisibleGutter := ASynEdit.Gutter.Visible;
  fShowLineNumbers := ASynEdit.Gutter.ShowLineNumbers;
  FUseCodeFolding := ASynEdit.Gutter.ShowCodeFolding;
  fGutterColor   := ASynEdit.Gutter.Color;
  fGutterWidth   := ASynEdit.Gutter.Width;
  fVisibleRightMargin := ASynEdit.RightEdge>0;
  if fVisibleRightMargin then
    fRightMargin   := ASynEdit.RightEdge;
  fRightMarginColor := ASynEdit.RightEdgeColor;
  fEditorFont    := ASynEdit.Font.Name;
  fEditorFontHeight := ASynEdit.Font.Height;
  fExtraLineSpacing := ASynEdit.ExtraLineSpacing;
  fUndoLimit     := ASynEdit.MaxUndo;
end;

procedure TEditorOptions.AddSpecialHilightAttribsToHighlighter(
  Syn: TCustomSyn);
type
  THasSpecialAttribute = array[TAdditionalHilightAttribute] of Boolean;
var
  HasSpecialAttribute: THasSpecialAttribute;
  a: TAdditionalHilightAttribute;
  i: Integer;
begin
  for a := Low(TAdditionalHilightAttribute)
    to High(TAdditionalHilightAttribute) do
    HasSpecialAttribute[a] := False;
  for i := 0 to Syn.AttrCount - 1 do
    with Syn.Attribute[i] do
    begin
      if Name = '' then
        continue;
      for a := Low(TAdditionalHilightAttribute)
        to High(TAdditionalHilightAttribute) do
        if AdditionalHighlightAttributes[a] = Name then
          HasSpecialAttribute[a] := True;
    end;
  for a := Low(TAdditionalHilightAttribute)
    to High(TAdditionalHilightAttribute) do
    if not HasSpecialAttribute[a] then
      Syn.AddSpecialAttribute(AdditionalHighlightAttributes[a]);
end;

procedure TEditorOptions.GetSynEditPreviewSettings(APreviewEditor: TObject);
// read synedit setings from config file
var
  ASynEdit: TSynEdit;
begin
  if not (APreviewEditor is TSynEdit) then
    exit;
  ASynEdit := TSynEdit(APreviewEditor);

  // general options
  ASynEdit.Options := fSynEditOptions - [eoDragDropEditing, eoDropFiles,
    eoScrollPastEof] + [eoNoCaret, eoNoSelection];
  ASynEdit.BlockIndent := fBlockIndent;
  ASynEdit.TabWidth := fTabWidth;

  // Display options
  ASynEdit.Gutter.Visible := False;
  if fVisibleRightMargin then
    ASynEdit.RightEdge := fRightMargin
  else
    ASynEdit.RightEdge := 0;
  ASynEdit.RightEdgeColor := fRightMarginColor;
  ASynEdit.Font.Height := fEditorFontHeight; // set height before Name for XLFD !
  ASynEdit.Font.Name := fEditorFont;
  ASynEdit.ExtraLineSpacing := fExtraLineSpacing;
  ASynEdit.ReadOnly := True;

  KeyMap.AssignTo(ASynEdit.KeyStrokes, TSourceEditorWindowInterface);
end;


{ TEditorOptionsForm }

constructor TEditorOptionsForm.Create(TheOwner: TComponent);
var
  a: Integer;
  s: String;
begin
  inherited Create(TheOwner);
  FormCreating := True;
  Caption      := lismenueditoroptions;

  IDEDialogLayoutList.ApplyLayout(Self, 480, 480);

  MainNoteBook.PageIndex := 0;

  SetupGeneralPage(0);
  SetupDisplayPage(1);
  SetupKeyMappingsPage(2);
  SetupColorPage(3);
  SetupCodeToolsPage(4);
  SetupCodeFoldingPage(5);
  SetupButtonBar;

  UpdatingColor := False;
  CurHighlightElement := Nil;

  // create a temporary copy of the keymap for editing
  EditingKeyMap := TKeyCommandRelationList.Create;
  EditingKeyMap.Assign(EditorOpts.KeyMap);

  // initialize previews
  for a := Low(PreviewEdits) to High(PreviewEdits) do
    PreviewEdits[a] := Nil;
  s := GetCurColorScheme(TPreviewPasSyn.GetLanguageName);
  PreviewSyn := GetHighlighter(TPreviewPasSyn, s, True);
  CurLanguageID := EditorOpts.HighlighterList.FindByClass(
    TCustomSynClass(PreviewSyn.ClassType));

  PreviewEdits[1] := DisplayPreview;
  PreviewEdits[2] := ColorPreview;
  for a := Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[a] <> Nil then
      with PreviewEdits[a] do
      begin
        if EditorOpts.UseSyntaxHighlight then
          Highlighter := PreviewSyn;
        EditorOpts.GetSynEditSettings(PreviewEdits[a]);
        EditingKeyMap.AssignTo(PreviewEdits[a].KeyStrokes,
          TSourceEditorWindowInterface);
        if a <> 3 then
        begin
          Lines.Text := EditorOpts.HighlighterList[CurLanguageID].SampleSource;
          PreviewEdits[a].Options :=
            PreviewEdits[a].Options + [eoNoCaret,
            eoNoSelection] - [eoBracketHighlight];
        end;
      end;

  // general options

  // display options

  // key mappings
  FillKeyMappingTreeView;

  // color options
  LanguageComboBox.Text := PreviewSyn.LanguageName;
  SetComboBoxText(LanguageComboBox, LanguageComboBox.Text);
  ColorSchemeComboBox.Text := GetCurColorScheme(PreviewSyn.LanguageName);
  SetComboBoxText(ColorSchemeComboBox, ColorSchemeComboBox.Text);
  FillColorElementListBox;
  FindCurHighlightElement;
  ShowCurAttribute;

  // code Tools options

  MainNoteBook.PageIndex := 0;
  FormCreating := False;
end;

destructor TEditorOptionsForm.Destroy;
begin
  ClearHighlighters;
  fColorSchemes.Free;
  fFileExtensions.Free;
  EditingKeyMap.Free;
  inherited Destroy;
end;


// general

procedure TEditorOptionsForm.GeneralCheckBoxOnChange(Sender: TObject; Index: integer);
var
  a: Integer;
  NewColor: TColor;
  i: LongInt;

  procedure SetOption(const CheckBoxName: String; AnOption: TSynEditorOption);
  var
    a: Integer;
    i: LongInt;
  begin
    i:=EditorOptionsGroupBox.Items.IndexOf(CheckBoxName);
    if i<0 then begin
      DebugLn(['TEditorOptionsForm.GeneralCheckBoxOnChange.SetOption i<0']);
      exit;
    end;
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> Nil then
        if EditorOptionsGroupBox.Checked[i] then
          PreviewEdits[a].Options := PreviewEdits[a].Options + [AnOption]
        else
          PreviewEdits[a].Options := PreviewEdits[a].Options - [AnOption];
  end;

  procedure SetOption2(const CheckBoxName: String; AnOption: TSynEditorOption2);
  var
    a: Integer;
    i: LongInt;
  begin
    i:=EditorOptionsGroupBox.Items.IndexOf(CheckBoxName);
    if i<0 then exit;
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> Nil then
        if EditorOptionsGroupBox.Checked[i] then
          PreviewEdits[a].Options2 := PreviewEdits[a].Options2 + [AnOption]
        else
          PreviewEdits[a].Options2 := PreviewEdits[a].Options2 - [AnOption];
  end;

  // GeneralCheckBoxOnChange
begin
  if FormCreating then
    exit;
  // general
  SetOption(dlgAltSetClMode, eoAltSetsColumnMode);
  SetOption(dlgAutoIdent, eoAutoIndent);
  // not for Preview: SetOption(dlgBracHighlight,eoBracketHighlight);
  SetOption(dlgDoubleClickLine, eoDoubleClickSelectsLine);
  SetOption(dlgDragDropEd, eoDragDropEditing);
  SetOption(dlgDropFiles, eoDropFiles);
  SetOption(dlgGroupUndo, eoGroupUndo);
  SetOption(dlgHomeKeyJumpsToNearestStart, eoEnhanceHomeKey);
  SetOption(dlgHalfPageScroll, eoHalfPageScroll);
  SetOption(dlgKeepCaretX, eoKeepCaretX);
  SetOption(dlgPersistentCaret, eoPersistentCaret);
  SetOption(dlgRightMouseMovesCursor, eoRightMouseMovesCursor);
  // not for Preview: SetOption('NoSelectionCheckBox',eoNoSelection);
  SetOption(dlgScrollByOneLess, eoScrollByOneLess);
  SetOption(dlgScrollPastEndFile, eoScrollPastEoF);
  SetOption(dlgScrollPastEndLine, eoScrollPastEoL);
  SetOption(dlgShowScrollHint, eoShowScrollHint);
  SetOption(dlgSmartTabs, eoSmartTabs);
  SetOption(dlgTabsToSpaces, eoTabsToSpaces);
  SetOption(dlgTabIndent, eoTabIndent);
  SetOption(dlgTrimTrailingSpaces, eoTrimTrailingSpaces);
  
  SetOption2(dlgCaretSkipsSelection, eoCaretSkipsSelection);
  SetOption2(dlgAlwaysVisibleCaret, eoAlwaysVisibleCaret);

  for a := Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[a] <> Nil then
    begin
      // general
      i:=EditorOptionsGroupBox.Items.IndexOf(dlgUseSyntaxHighlight);
      if EditorOptionsGroupBox.Checked[i] then
        PreviewEdits[a].Highlighter := PreviewSyn
      else
        PreviewEdits[a].Highlighter := Nil;
      // display
      if (a in [1, 2]) then
        PreviewEdits[a].Gutter.Visible := VisibleGutterCheckBox.Checked;
      PreviewEdits[a].Gutter.ShowLineNumbers := ShowLineNumbersCheckBox.Checked;
      PreviewEdits[a].RightEdgeColor:=RightMarginColorButton.ButtonColor;
      if VisibleRightMarginCheckBox.Checked then
        PreviewEdits[a].RightEdge:=StrToIntDef(RightMarginComboBox.Text,80)
      else
        PreviewEdits[a].RightEdge:=0;
    end;
  if CurHighlightElement <> Nil then
  begin
    if Sender = ForeGroundUseDefaultCheckBox then
      if UpdatingColor = False then
      begin
        UpdatingColor := True;
        if not ForeGroundUseDefaultCheckBox.Checked then
          NewColor := ForeGroundColorButton.ButtonColor
        else
          NewColor := clNone;
        ForeGroundColorButton.Visible := not
          ForeGroundUseDefaultCheckBox.Checked;
        if NewColor <> CurHighlightElement.Foreground then
        begin
          CurHighlightElement.Foreground := NewColor;
          InvalidatePreviews;
        end;
        UpdatingColor := False;
      end;
    if Sender = BackGroundUseDefaultCheckBox then
      if UpdatingColor = False then
      begin
        if not BackGroundUseDefaultCheckBox.Checked then
          NewColor := BackGroundColorButton.ButtonColor
        else
          NewColor := clNone;
        BackGroundColorButton.Visible := not
          BackGroundUseDefaultCheckBox.Checked;
        if NewColor <> CurHighlightElement.Background then
        begin
          CurHighlightElement.Background := NewColor;
          InvalidatePreviews;
        end;
      end;
    if Sender = TextBoldCheckBox then
      if TextBoldCheckBox.Checked xor (fsBold in CurHighlightElement.Style) then
      begin
        if TextBoldCheckBox.Checked then
          CurHighlightElement.Style := CurHighlightElement.Style + [fsBold]
        else
          CurHighlightElement.Style := CurHighlightElement.Style - [fsBold];
        InvalidatePreviews;
      end;
    if Sender = TextItalicCheckBox then
      if TextItalicCheckBox.Checked then
      begin
        if not (fsItalic in CurHighlightElement.Style) then
        begin
          CurHighlightElement.Style := CurHighlightElement.Style + [fsItalic];
          InvalidatePreviews;
        end;
      end
      else
      if (fsItalic in CurHighlightElement.Style) then
      begin
        CurHighlightElement.Style := CurHighlightElement.Style - [fsItalic];
        InvalidatePreviews;
      end;
    if Sender = TextUnderlineCheckBox then
      if TextUnderlineCheckBox.Checked then
      begin
        if not (fsUnderline in CurHighlightElement.Style) then
        begin
          CurHighlightElement.Style := CurHighlightElement.Style + [fsUnderline];
          InvalidatePreviews;
        end;
      end
      else
      if (fsUnderline in CurHighlightElement.Style) then
      begin
        CurHighlightElement.Style := CurHighlightElement.Style - [fsUnderline];
        InvalidatePreviews;
      end;
  end;
end;

procedure TEditorOptionsForm.chkCodeFoldingEnabledChange(Sender: TObject);
begin
  lblDividerDrawLevel.Enabled := chkCodeFoldingEnabled.Checked;
  edDividerDrawLevel.Enabled  := chkCodeFoldingEnabled.Checked;
end;

procedure TEditorOptionsForm.EditorFontComboBoxEditingDone(Sender: TObject);
var
  i: Integer;
begin
  for i := Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[i] <> Nil then
      PreviewEdits[i].Font.Name:=EditorFontComboBox.Text;
end;

procedure TEditorOptionsForm.ColorButtonColorChanged(Sender: TObject);
var
  a: Integer;
begin
  if FormCreating then
    exit;
  if Sender = ForeGroundColorButton then
  begin
    if (CurHighlightElement = Nil) or UpdatingColor then
      exit;
    if not ForeGroundUseDefaultCheckBox.Checked then
    begin
      CurHighlightElement.Foreground := ForeGroundColorButton.ButtonColor;
      InvalidatePreviews;
    end;
  end;
  if Sender = BackGroundColorButton then
  begin
    if (CurHighlightElement = Nil) or UpdatingColor then
      exit;
    if not BackGroundUseDefaultCheckBox.Checked then
    begin
      CurHighlightElement.Background := BackGroundColorButton.ButtonColor;
      InvalidatePreviews;
    end;
  end;
  if Sender = GutterColorButton then
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> Nil then
      begin
        PreviewEdits[a].Gutter.Color := GutterColorButton.ButtonColor;
        PreviewEdits[a].Invalidate;
      end;
end;

procedure TEditorOptionsForm.FontDialogNameToFont(FontDialogName: String;
  AFont: TFont);
var
  TmpFont: TFont;
  p, p2, index: Integer;
  s: shortstring;
begin
  TmpFont := TFont.Create;
  TmpFont.Assign(AFont);
  try
    p := 1;
    p2 := 0;
    index := 1;
    while (p <= length(FontDialogName)) do
    begin
      if (FontDialogName[p] = '-') then
      begin
        s := copy(FontDialogName, p2 + 1, p - p2 - 1);
        p2 := p;
        case Index of
          3:
            TmpFont.Name := s;
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
var
  a: Integer;
begin
  for a := Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[a] <> Nil then
      PreviewEdits[a].Font.Assign(TFontDialog(Sender).Font);
  EditorFontComboBox.Text := DisplayPreview.Font.Name;
  SetComboBoxText(EditorFontHeightComboBox,
                  IntToStr(DisplayPreview.Font.Height));
end;

procedure TEditorOptionsForm.EditorFontButtonClick(Sender: TObject);
var
  FontDialog: TFontDialog;
begin
  FontDialog := TFontDialog.Create(Nil);
  try
    with FontDialog do
    begin
      Font.Name   := EditorFontComboBox.Text;
      Font.Height := StrToIntDef(EditorFontHeightComboBox.Text, PreviewEdits[1].Font.Height);
      Options := Options + [fdApplyButton];
      OnApplyClicked := @FontDialogApplyClicked;
      if Execute then
        FontDialogApplyClicked(FontDialog);
    end;
  finally
    FontDialog.Free;
  end;
end;

procedure TEditorOptionsForm.KeyMappingChooseSchemeButtonClick(
  Sender: TObject);
var
  NewScheme: String;
begin
  if ShowChooseKeySchemeDialog(NewScheme) <> mrOk then
    exit;
  EditingKeyMap.LoadScheme(NewScheme);
  FillKeyMappingTreeView;
end;

procedure TEditorOptionsForm.ComboBoxOnExit(Sender: TObject);
var
  NewVal, a: Integer;
  Box: TComboBox;
begin
  if FormCreating then
    exit;
  Box := TComboBox(Sender);
  if PreviewEdits[1] <> Nil then
    if Sender = BlockIndentComboBox then
    begin
      NewVal := StrToIntDef(BlockIndentComboBox.Text,
        PreviewEdits[1].BlockIndent);
      SetComboBoxText(BlockIndentComboBox, IntToStr(NewVal));
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> Nil then
          PreviewEdits[a].BlockIndent := NewVal;
    end
    else
    if Sender = TabWidthsComboBox then
    begin
      NewVal := StrToIntDef(TabWidthsComboBox.Text,
        PreviewEdits[1].TabWidth);
      SetComboBoxText(TabWidthsComboBox, IntToStr(NewVal));
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> Nil then
          PreviewEdits[a].TabWidth := NewVal;
    end
    // display
    else
    if Sender = EditorFontHeightComboBox then
    begin
      NewVal := StrToIntDef(EditorFontHeightComboBox.Text,
        PreviewEdits[1].Font.Height);
      if (NewVal < 0) then
        if (NewVal > -6) then
          NewVal := -6;
      if (NewVal >= 0) then
        if (NewVal < 6) then
          NewVal := 6;
      if (NewVal > 40) then
        NewVal := 40;
      if (NewVal < -40) then
        NewVal := -40;
      SetComboBoxText(EditorFontHeightComboBox, IntToStr(NewVal));
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> Nil then
          PreviewEdits[a].Font.Height := NewVal;
    end
    else
    if Sender = ExtraLineSpacingComboBox then
    begin
      NewVal := StrToIntDef(ExtraLineSpacingComboBox.Text,
        PreviewEdits[1].ExtraLineSpacing);
      SetComboBoxText(ExtraLineSpacingComboBox, IntToStr(NewVal));
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> Nil then
          PreviewEdits[a].ExtraLineSpacing := NewVal;
    end
    else
    if Sender = GutterWidthComboBox then
    begin
      NewVal := StrToIntDef(GutterWidthComboBox.Text,
        PreviewEdits[1].Gutter.Width);
      SetComboBoxText(GutterWidthComboBox, IntToStr(NewVal));
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> Nil then
          PreviewEdits[a].Gutter.Width := NewVal;
    end
    else
    if Sender = RightMarginComboBox then
    begin
      NewVal := StrToIntDef(RightMarginComboBox.Text,PreviewEdits[1].RightEdge);
      SetComboBoxText(RightMarginComboBox, IntToStr(NewVal));
      for a := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[a] <> Nil then begin
          if VisibleRightMarginCheckBox.Checked then
            PreviewEdits[a].RightEdge := NewVal
          else
            PreviewEdits[a].RightEdge := 0;
        end;
    end
    // color
    else
    if Sender = ColorSchemeComboBox then
    begin
      if Box.Items.IndexOf(Box.Text) < 0 then
        SetComboBoxText(Box, GetCurColorScheme(PreviewSyn.LanguageName))
        // unknown color scheme -> switch back
      else
      if Box.Text <> GetCurColorScheme(PreviewSyn.LanguageName) then
      begin
        SetCurColorScheme(PreviewSyn.LanguageName, Box.Text);
        SetComboBoxText(Box, Box.Text);
        PreviewSyn := GetHighlighter(TCustomSynClass(PreviewSyn.ClassType),
          Box.Text, True);
        SetPreviewSynInAllPreviews;
        FillColorElementListBox;
        FindCurHighlightElement;
      end// change the colorscheme
      ;
    end
    else
    if Sender = FileExtensionsComboBox then
    begin
      //DebugLn(['TEditorOptionsForm.ComboBoxOnExit Box.Text="',Box.Text,'" Old="',GetCurFileExtensions(PreviewSyn.LanguageName),'" PreviewSyn.LanguageName=',PreviewSyn.LanguageName]);
      if Box.Text <> GetCurFileExtensions(PreviewSyn.LanguageName) then
      begin
        SetCurFileExtensions(PreviewSyn.LanguageName, Box.Text);
        SetComboBoxText(Box, Box.Text);
      end;
      //DebugLn(['TEditorOptionsForm.ComboBoxOnExit Box.Text="',Box.Text,'" Now="',GetCurFileExtensions(PreviewSyn.LanguageName),'" PreviewSyn.LanguageName=',PreviewSyn.LanguageName]);
    end
    else
    if Sender = LanguageComboBox then
      if Box.Items.IndexOf(Box.Text) < 0 then
        SetComboBoxText(Box, PreviewSyn.LanguageName)// unknown language -> switch back
      else
      if Box.Text <> PreviewSyn.LanguageName then
      begin
        NewVal := EditorOpts.HighlighterList.FindByName(Box.Text);
        if NewVal >= 0 then
        begin
          SetComboBoxText(Box, Box.Text);
          CurLanguageID := NewVal;
          PreviewSyn    := GetHighlighter(
            EditorOpts.HighlighterList[CurLanguageID].SynClass,
            GetCurColorScheme(
            EditorOpts.HighlighterList[
            CurLanguageID].SynClass.GetLanguageName)
            , True);
          SetComboBoxText(ColorSchemeComboBox,
            GetCurColorScheme(PreviewSyn.LanguageName));
          SetComboBoxText(FileExtensionsComboBox,
            GetCurFileExtensions(PreviewSyn.LanguageName));
          for a := Low(PreviewEdits) to High(PreviewEdits) do
            if a <> 3 then
              PreviewEdits[a].Lines.Text :=
                EditorOpts.HighlighterList[CurLanguageID].SampleSource;
          SetPreviewSynInAllPreviews;
          FillColorElementListBox;
          FindCurHighlightElement;
        end;
      end// change language
    // general
  ;
end;

procedure TEditorOptionsForm.ComboBoxOnKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssCtrl in Shift) and (Key = VK_S) then
    ComboBoxOnExit(Sender);
end;

procedure TEditorOptionsForm.ComboBoxOnChange(Sender: TObject);
var
  ComboBox: TComboBox;
begin
  ComboBox := TComboBox(Sender);
  if ComboBox.Items.IndexOf(ComboBox.Text) >= 0 then
    ComboBoxOnExit(Sender);
end;

procedure TEditorOptionsForm.FindCurHighlightElement;
var
  a, i: Integer;
  Old:  TSynHighlightElement;
begin
  Old := CurHighlightElement;
  CurHighlightElement := Nil;
  a   := ColorElementListBox.ItemIndex;
  if (a >= 0) then
  begin
    i := PreviewSyn.AttrCount - 1;
    while (i >= 0) do
    begin
      if ColorElementListBox.Items[a] = PreviewSyn.Attribute[i].Name then
      begin
        CurHighlightElement := PreviewSyn.Attribute[i];
        break;
      end;
      dec(i);
    end;
  end;
  if Old <> CurHighlightElement then
    ShowCurAttribute;
end;

procedure TEditorOptionsForm.InvalidatePreviews;
var
  a: Integer;
begin
  for a := Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[a] <> Nil then
      PreviewEdits[a].Invalidate;
end;

procedure TEditorOptionsForm.SetPreviewSynInAllPreviews;
var
  a: Integer;
begin
  for a := Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[a] <> Nil then
      if EditorOpts.UseSyntaxHighlight then
        PreviewEdits[a].Highlighter := PreviewSyn
      else
        PreviewEdits[a].Highlighter := Nil;
end;

procedure TEditorOptionsForm.ShowCurAttribute;
begin
  if (CurHighlightElement = Nil) or UpdatingColor then
    exit;
  UpdatingColor := True;
  TextBoldCheckBox.Checked := fsBold in CurHighlightElement.Style;
  TextItalicCheckBox.Checked := fsItalic in CurHighlightElement.Style;
  TextUnderlineCheckBox.Checked := fsUnderline in CurHighlightElement.Style;
  if CurHighlightElement.Foreground = clNone then
    ForeGroundUseDefaultCheckBox.Checked := True
  else
  begin
    ForeGroundUseDefaultCheckBox.Checked := False;
    ForeGroundColorButton.ButtonColor    := CurHighlightElement.Foreground;
  end;
  ForeGroundColorButton.Visible := not ForeGroundUseDefaultCheckBox.Checked;
  if CurHighlightElement.Background = clNone then
    BackGroundUseDefaultCheckBox.Checked := True
  else
  begin
    BackGroundUseDefaultCheckBox.Checked := False;
    BackGroundColorButton.ButtonColor    := CurHighlightElement.Background;
  end;
  BackGroundColorButton.Visible := not BackGroundUseDefaultCheckBox.Checked;
  UpdatingColor := False;
end;

procedure TEditorOptionsForm.KeyMappingTreeViewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
  ARelation: TKeyCommandRelation;
  ANode: TTreeNode;
begin
  ANode := KeyMappingTreeView.GetNodeAt(X, Y);
  if (ANode <> Nil) and (ANode.Data <> Nil) and
    (TObject(ANode.Data) is TKeyCommandRelation) then
  begin
    ARelation := TKeyCommandRelation(ANode.Data);
    i := EditingKeyMap.IndexOf(ARelation);
    if (i >= 0) and (ShowKeyMappingEditForm(i, EditingKeyMap) = mrOk) then
    begin
      FillKeyMappingTreeView;
      for i := Low(PreviewEdits) to High(PreviewEdits) do
        if PreviewEdits[i] <> Nil then
          EditingKeyMap.AssignTo(PreviewEdits[i].KeyStrokes,
            TSourceEditorWindowInterface);
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
  if LazarusResources.Find(ClassName) = Nil then
  begin
    SetBounds((Screen.Width - 410) div 2, (Screen.Height - 260) div 2, 400, 250);
    Caption := dlgKeyMappingErrors;

    ListBox := TListBox.Create(Self);
    with ListBox do
    begin
      Name := 'ListBox';
      Parent := Self;
      Left := 0;
      Top  := 0;
      Width := Self.ClientWidth - 4;
      Height := Self.ClientHeight - 50;
    end;

    BackButton := TButton.Create(Self);
    with BackButton do
    begin
      Name := 'BackButton';
      Parent := Self;
      Width := 60;
      Height := 25;
      Caption := dlgEdBack;
      Left := ((Self.ClientWidth - 4) - Width) div 2;
      Top  := Self.ClientHeight - 38;
      OnClick := @BackButtonClick;
    end;
  end;
end;

procedure TKeyMapErrorsForm.BackButtonClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;

procedure TEditorOptionsForm.KeyMappingConsistencyCheckButtonClick(
  Sender: TObject);
var
  Protocol: TStringList;
  ErrorCount, Index1, Index2: Integer;
  ACaption, AText: String;
  KeyMapErrorsForm: TKeyMapErrorsForm;
begin
  Protocol := TStringList.Create;
  try
    ErrorCount := FindKeymapConflicts(EditingKeyMap, Protocol, Index1, Index2);
    if ErrorCount > 0 then
    begin
      KeyMapErrorsForm := TKeyMapErrorsForm.Create(Nil);
      try
        KeyMapErrorsForm.ListBox.Items.Assign(Protocol);
        KeyMapErrorsForm.ShowModal;
      finally
        KeyMapErrorsForm.Free;
      end;
    end
    else
    begin
      ACaption := dlgReport;
      AText    := dlgEdNoErr;
      MessageDlg(ACaption, AText, mtInformation, [mbOk], 0);
    end;
  finally
    Protocol.Free;
  end;
end;

procedure TEditorOptionsForm.ColorElementListBoxSelectionChange(
  Sender: TObject;
  User: Boolean);
begin
  FindCurHighlightElement;
end;

procedure TEditorOptionsForm.FillColorElementListBox;
var
  i: Integer;
begin
  with ColorElementListBox.Items do
  begin
    BeginUpdate;
    Clear;

    for i := 0 to PreviewSyn.AttrCount - 1 do
      if PreviewSyn.Attribute[i].Name <> '' then
        Add(PreviewSyn.Attribute[i].Name);
    EndUpdate;
  end;

  CurHighlightElement := Nil;
  if ColorElementListBox.Items.Count > 0 then
    ColorElementListBox.Selected[0] := True;
  FindCurHighlightElement;
end;

procedure TEditorOptionsForm.ColorPreviewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NewIndex: Integer;
  Token:    String;
  Attri:    TSynHighlightElement;
  MouseXY, XY: TPoint;
  AddAttr:  TAdditionalHilightAttribute;
begin
  MouseXY := Point(X, Y);
  XY      := ColorPreview.PixelsToRowColumn(MouseXY);
  NewIndex := -1;
  if CurLanguageID >= 0 then
  begin
    AddAttr := EditorOpts.HighlighterList[
      CurLanguageID].SampleLineToAddAttr(XY.Y);
    if AddAttr <> ahaNone then
      NewIndex := ColorElementListBox.Items.IndexOf(
        AdditionalHighlightAttributes[AddAttr]);
  end;
  if NewIndex < 0 then
  begin
    ColorPreview.GetHighlighterAttriAtRowCol(XY, Token, Attri);
    if Attri = Nil then
      Attri := PreviewSyn.WhitespaceAttribute;
    if Attri <> Nil then
      NewIndex := ColorElementListBox.Items.IndexOf(Attri.Name);
  end;
  if NewIndex >= 0 then
  begin
    ColorElementListBox.ItemIndex := NewIndex;
    FindCurHighlightElement;
  end;
end;

procedure TEditorOptionsForm.OnSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
var
  e: TSynHighlightElement;
  AddAttr: TAdditionalHilightAttribute;
  i: Integer;
begin
  if CurLanguageID >= 0 then
  begin
    AddAttr := EditorOpts.HighlighterList[CurLanguageID].SampleLineToAddAttr(Line);
    if AddAttr <> ahaNone then
    begin
      i := PreviewSyn.AttrCount - 1;
      while (i >= 0) do
      begin
        e := PreviewSyn.Attribute[i];
        if e.Name = '' then
          continue;
        if e.Name = AdditionalHighlightAttributes[AddAttr] then
        begin
          Special := (e.ForeGround <> clNone) or (e.BackGround <> clNone);
          if e.ForeGround <> clNone then
            FG := e.ForeGround;
          if e.BackGround <> clNone then
            BG := e.BackGround;
          exit;
        end;
        dec(i);
      end;
    end;
  end;
end;

procedure TEditorOptionsForm.RightMarginColorButtonColorChanged(Sender: TObject
  );
var
  a: Integer;
begin
  for a := Low(PreviewEdits) to High(PreviewEdits) do
    if PreviewEdits[a] <> Nil then
      PreviewEdits[a].RightEdgeColor:=RightMarginColorButton.ButtonColor;
end;

procedure TEditorOptionsForm.SetAttributeToDefaultButtonClick(Sender: TObject);
begin
  SetColorElementsToDefaults(True);
end;

procedure TEditorOptionsForm.SetAllAttributesToDefaultButtonClick(
  Sender: TObject);
begin
  SetColorElementsToDefaults(False);
end;

procedure TEditorOptionsForm.SetColorElementsToDefaults(OnlySelected: Boolean);
var
  DefaultSyn: TCustomSyn;
  PascalSyn: TPreviewPasSyn;
  i, j: Integer;
  CurSynClass: TCustomSynClass;
begin
  PascalSyn := TPreviewPasSyn(GetHighlighter(TPreviewPasSyn,
    ColorSchemeComboBox.Text, True));
  CurSynClass := TCustomSynClass(PreviewSyn.ClassType);
  DefaultSyn := CurSynClass.Create(Nil);
  try
    EditorOpts.AddSpecialHilightAttribsToHighlighter(DefaultSyn);
    EditorOpts.ReadDefaultsForHighlighterSettings(DefaultSyn,
      ColorSchemeComboBox.Text, PascalSyn);
    for i := 0 to DefaultSyn.AttrCount - 1 do
    begin
      if DefaultSyn.Attribute[i].Name = '' then
        continue;
      if OnlySelected then
      begin
        if (DefaultSyn.Attribute[i].Name = CurHighlightElement.Name) then
          CopyHiLightAttributeValues(DefaultSyn.Attribute[i],
            CurHighlightElement);
      end
      else
        for j := 0 to PreviewSyn.AttrCount - 1 do
          if PreviewSyn.Attribute[j].Name = DefaultSyn.Attribute[i].Name then
            CopyHiLightAttributeValues(DefaultSyn.Attribute[i],
              PreviewSyn.Attribute[j]);
    end;
  finally
    DefaultSyn.Free;
  end;
  ShowCurAttribute;
end;

function TEditorOptionsForm.GetCurColorScheme(
  const LanguageName: String): String;
begin
  if fColorSchemes = Nil then
    Result := ''
  else
    Result := fColorSchemes.Values[LanguageName];
  if Result = '' then
    Result := EditorOpts.ReadColorScheme(LanguageName);
end;

procedure TEditorOptionsForm.SetCurColorScheme(
  const LanguageName, ColorScheme: String);
begin
  if fColorSchemes = Nil then
    fColorSchemes := TStringList.Create;
  fColorSchemes.Values[LanguageName] := ColorScheme;
end;

procedure TEditorOptionsForm.SaveAllColorSchemes;
var
  i: Integer;
begin
  if fColorSchemes = Nil then
    exit;
  for i := 0 to fColorSchemes.Count - 1 do
    EditorOpts.WriteColorScheme(fColorSchemes.Names[i],
      fColorSchemes.Values[fColorSchemes.Names[i]]);
end;

function TEditorOptionsForm.GetCurFileExtensions(
  const LanguageName: String): String;
var
  i: Integer;
begin
  if fFileExtensions = Nil then
    Result := ''
  else
    Result := fFileExtensions.Values[LanguageName];
  if Result = '' then
  begin
    i := EditorOpts.HighlighterList.FindByName(LanguageName);
    if i >= 0 then
      Result := EditorOpts.HighlighterList[i].FileExtensions;
  end;
end;

procedure TEditorOptionsForm.SetCurFileExtensions(
  const LanguageName, FileExtensions: String);
begin
  if fFileExtensions = Nil then
    fFileExtensions := TStringList.Create;
  fFileExtensions.Values[LanguageName] := FileExtensions;
  //DebugLn(['TEditorOptionsForm.SetCurFileExtensions ',LanguageName,'=',FileExtensions]);
end;

procedure TEditorOptionsForm.SaveAllFileExtensions;
var
  i, j: Integer;
begin
  if fFileExtensions = Nil then
    exit;
  for i := 0 to fFileExtensions.Count - 1 do
  begin
    j := EditorOpts.HighlighterList.FindByName(fFileExtensions.Names[i]);
    if j >= 0 then begin
      EditorOpts.HighlighterList[j].FileExtensions :=
        fFileExtensions.ValueFromIndex[i];
      //DebugLn(['TEditorOptionsForm.SaveAllFileExtensions ',fFileExtensions.Names[i],'=',fFileExtensions.ValueFromIndex[i],' -> ',EditorOpts.HighlighterList[j].FileExtensions]);
    end;
  end;
end;

function TEditorOptionsForm.GetHighlighter(SynClass: TCustomSynClass;
  const ColorScheme: String; CreateIfNotExists: Boolean): TCustomSyn;
var
  i: Integer;
begin
  if fHighlighterList = Nil then
    fHighlighterList := TStringList.Create;
  for i := 0 to fHighlighterList.Count - 1 do
    if (fHighlighterList[i] = ColorScheme) and
      (TCustomSynClass(TCustomSyn(fHighlighterList.Objects[i]).ClassType) =
      SynClass) then
    begin
      Result := TCustomSyn(fHighlighterList.Objects[i]);
      exit;
    end;
  if CreateIfNotExists then
  begin
    Result := SynClass.Create(Nil);
    EditorOpts.AddSpecialHilightAttribsToHighlighter(Result);
    fHighlighterList.AddObject(ColorScheme, Result);
    EditorOpts.ReadHighlighterSettings(Result, ColorScheme);
  end;
end;

procedure TEditorOptionsForm.ClearHighlighters;
var
  i: Integer;
begin
  if fHighlighterList = Nil then
    exit;
  for i := 0 to fHighlighterList.Count - 1 do
    TCustomSyn(fHighlighterList.Objects[i]).Free;
  fHighlighterList.Free;
end;

procedure TEditorOptionsForm.SaveAllHighlighters;
var
  i: Integer;
  Syn: TCustomSyn;
begin
  if fHighlighterList = Nil then
    exit;
  for i := 0 to fHighlighterList.Count - 1 do
  begin
    Syn := TCustomSyn(fHighlighterList.Objects[i]);
    EditorOpts.WriteHighlighterSettings(Syn, fHighlighterList[i]);
  end;
end;

// keymapping ------------------------------------------------------------------

function TEditorOptionsForm.KeyMappingRelationToString(Index: Integer): String;
begin
  Result := KeyMappingRelationToString(EditingKeyMap.Relations[Index]);
end;

function TEditorOptionsForm.KeyMappingRelationToString(
  KeyRelation: TKeyCommandRelation): String;
var
  s: String;
begin
  with KeyRelation do
  begin
    Result := copy(LocalizedName, 1, 40);
    if length(Result) < 40 then
    begin
      SetLength(s, (40 - length(Result)));
      FillChar(s[1], length(s), ' ');
    end
    else
      s := '';
    Result := Result + s;
    if (ShortcutA.Key1 = VK_UNKNOWN) and (ShortcutB.Key1 = VK_UNKNOWN) then
      Result := Result + lisNone2
    else
    if (ShortcutA.Key1 = VK_UNKNOWN) then
      Result := Result + KeyAndShiftStateToEditorKeyString(ShortcutB)
    else
    if (ShortcutB.Key1 = VK_UNKNOWN) then
      Result := Result + KeyAndShiftStateToEditorKeyString(ShortcutA)
    else
      Result := Result + KeyAndShiftStateToEditorKeyString(ShortcutA)
                       + '  '+lisOr+'  ' +
                         KeyAndShiftStateToEditorKeyString(ShortcutB);
  end;
end;

procedure TEditorOptionsForm.FillKeyMappingTreeView;
var
  i, j: Integer;
  NewCategoryNode, NewKeyNode: TTreeNode;
  CurCategory: TIDECommandCategory;
  CurKeyRelation: TKeyCommandRelation;
begin
  with KeyMappingTreeView do
  begin
    BeginUpdate;
    for i := 0 to EditingKeyMap.CategoryCount - 1 do
    begin
      CurCategory := EditingKeyMap.Categories[i];
      if Items.TopLvlCount > i then
      begin
        NewCategoryNode := Items.TopLvlItems[i];
        NewCategoryNode.Text := CurCategory.Description;
        NewCategoryNode.Data := CurCategory;
      end
      else
        NewCategoryNode := Items.AddObject(Nil, CurCategory.Description, CurCategory);
      NewCategoryNode.ImageIndex := 0;
      NewCategoryNode.SelectedIndex := NewCategoryNode.ImageIndex;
      for j := 0 to CurCategory.Count - 1 do
      begin
        CurKeyRelation := TKeyCommandRelation(CurCategory[j]);
        if NewCategoryNode.Count > j then
        begin
          NewKeyNode := NewCategoryNode.Items[j];
          NewKeyNode.Text := KeyMappingRelationToString(CurKeyRelation);
          NewKeyNode.Data := CurKeyRelation;
        end
        else
          NewKeyNode := Items.AddChildObject(NewCategoryNode,
            KeyMappingRelationToString(CurKeyRelation), CurKeyRelation);
        NewKeyNode.ImageIndex := 1;
        NewKeyNode.SelectedIndex := NewKeyNode.ImageIndex;
      end;
      while NewCategoryNode.Count > CurCategory.Count do
        NewCategoryNode[NewCategoryNode.Count - 1].Delete;
    end;
    while Items.TopLvlCount > EditingKeyMap.CategoryCount do
      Items.TopLvlItems[Items.TopLvlCount - 1].Delete;
    EndUpdate;
  end;
end;

// useful functions

procedure TEditorOptionsForm.SetComboBoxText(AComboBox: TComboBox;
  const AText: String);
var
  a: Integer;
begin
  a := AComboBox.Items.IndexOf(AText);
  if a >= 0 then begin
    AComboBox.ItemIndex := a;
  end else
  begin
    AComboBox.Items.Add(AText);
    AComboBox.ItemIndex := AComboBox.Items.IndexOf(AText);
  end;
end;

procedure TEditorOptionsForm.SetupGeneralPage(Page: Integer);
begin
  MainNoteBook.Page[Page].Caption := lisMenuInsertGeneral;

  EditorOptionsGroupBox.Caption := lismenueditoroptions;

  with EditorOptionsGroupBox do
  begin
    // selections
    Items.Add(dlgAltSetClMode);
    Items.Add(dlgAutoIdent);
    // visual effects
    Items.Add(dlgBracHighlight);
    Items.Add(dlgShowGutterHints);
    Items.Add(dlgShowScrollHint);
    Items.Add(dlgUseSyntaxHighlight);
    // drag&drop
    Items.Add(dlgDragDropEd);
    Items.Add(dlgDropFiles);
    // caret + scrolling + key navigation
    Items.Add(dlgHalfPageScroll);
    Items.Add(dlgKeepCaretX);
    Items.Add(dlgPersistentCaret);
    Items.Add(dlgCaretSkipsSelection);
    Items.Add(dlgRightMouseMovesCursor);
    Items.Add(dlgScrollByOneLess);
    Items.Add(dlgScrollPastEndFile);
    Items.Add(dlgScrollPastEndLine);
    Items.Add(dlgHomeKeyJumpsToNearestStart);
    Items.Add(dlgAlwaysVisibleCaret);
    // tabs
    Items.Add(dlgSmartTabs);
    Items.Add(dlgTabsToSpaces);
    Items.Add(dlgTabIndent);
    // spaces
    Items.Add(dlgTrimTrailingSpaces);
    // undo
    Items.Add(dlgUndoAfterSave);
    Items.Add(dlgGroupUndo);
    // mouse
    Items.Add(dlgDoubleClickLine);
    Items.Add(dlgMouseLinks);
    Items.Add(dlgCloseButtonsNotebook);
    // copying
    Items.Add(dlgFindTextatCursor);
    Items.Add(dlgCopyWordAtCursorOnCopyNone);

    Checked[Items.IndexOf(dlgAltSetClMode)] := eoAltSetsColumnMode in
                                                      EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgAutoIdent)]    := eoAutoIndent in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgBracHighlight)] :=
                                eoBracketHighlight in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgDragDropEd)]   :=
                                 eoDragDropEditing in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgDropFiles)]    := eoDropFiles in EditorOpts.SynEditOptions;
    //TODO CheckEnabledByName[dlgDropFiles] := False;
    Checked[Items.IndexOf(dlgGroupUndo)] := eoGroupUndo in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgHalfPageScroll)] :=
                                  eoHalfPageScroll in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgKeepCaretX)]   := eoKeepCaretX in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgPersistentCaret)] :=
                                 eoPersistentCaret in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgRightMouseMovesCursor)] :=
                           eoRightMouseMovesCursor in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgScrollByOneLess)] :=
                                 eoScrollByOneLess in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgScrollPastEndFile)] :=
                                   eoScrollPastEoF in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgMouseLinks)]   := EditorOpts.CtrlMouseLinks;
    Checked[Items.IndexOf(dlgShowGutterHints)] := EditorOpts.ShowGutterHints;
    Checked[Items.IndexOf(dlgScrollPastEndLine)] :=
                                   eoScrollPastEoL in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgCloseButtonsNotebook)] := EditorOpts.ShowTabCloseButtons;
    Checked[Items.IndexOf(dlgShowScrollHint)] :=
                                  eoShowScrollHint in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgSmartTabs)] := eoSmartTabs in EditorOpts.SynEditOptions;
    //DebugLn(['TEditorOptionsForm.SetupGeneralPage ',Checked[Items.IndexOf(dlgSmartTabs)],' ',Items.IndexOf(dlgSmartTabs),' ',eoSmartTabs in EditorOpts.SynEditOptions]);
    Checked[Items.IndexOf(dlgTabsToSpaces)] :=
                                    eoTabsToSpaces in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgTabIndent)]    := eoTabIndent in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgTrimTrailingSpaces)] :=
                              eoTrimTrailingSpaces in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgUndoAfterSave)] := EditorOpts.UndoAfterSave;
    Checked[Items.IndexOf(dlgDoubleClickLine)] :=
                          eoDoubleClickSelectsLine in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgFindTextatCursor)] := EditorOpts.FindTextAtCursor;
    Checked[Items.IndexOf(dlgUseSyntaxHighlight)] := EditorOpts.UseSyntaxHighlight;
    Checked[Items.IndexOf(dlgCopyWordAtCursorOnCopyNone)] :=
                                          EditorOpts.CopyWordAtCursorOnCopyNone;
    Checked[Items.IndexOf(dlgHomeKeyJumpsToNearestStart)] :=
                                  eoEnhanceHomeKey in EditorOpts.SynEditOptions;
    Checked[Items.IndexOf(dlgCaretSkipsSelection)] :=
                            eoCaretSkipsSelection in EditorOpts.SynEditOptions2;
    Checked[Items.IndexOf(dlgAlwaysVisibleCaret)] :=
                             eoAlwaysVisibleCaret in EditorOpts.SynEditOptions2;
  end;

  with BlockIndentComboBox do
    SetComboBoxText(BlockIndentComboBox, IntToStr(EditorOpts.BlockIndent));

  BlockIndentLabel.Caption := dlgBlockIndent;

  with UndoLimitComboBox do
    SetComboBoxText(UndoLimitComboBox, IntToStr(EditorOpts.UndoLimit));

  UndoLimitLabel.Caption := dlgUndoLimit;

  with TabWidthsComboBox do
    SetComboBoxText(TabWidthsComboBox, IntToStr(EditorOpts.TabWidth));

  TabWidthsLabel.Caption := dlgTabWidths;
end;

procedure TEditorOptionsForm.SetupDisplayPage(Page: Integer);
begin
  MainNoteBook.Page[Page].Caption := dlgEdDisplay;

  MarginAndGutterGroupBox.Caption := dlgMarginGutter;

  with VisibleRightMarginCheckBox do
  begin
    Caption := dlgVisibleRightMargin;
    Checked := EditorOpts.VisibleRightMargin;
  end;

  with VisibleGutterCheckBox do
  begin
    Caption := dlgVisibleGutter;
    Checked := EditorOpts.VisibleGutter;
  end;

  with ShowLineNumbersCheckBox do
  begin
    Caption := dlgShowLineNumbers;
    Checked := EditorOpts.ShowLineNumbers;
  end;

  RightMarginLabel.Caption := dlgRightMargin;

  VisibleRightMarginCheckBox.Checked:=EditorOpts.VisibleRightMargin;
  with RightMarginComboBox do
    SetComboBoxText(RightMarginComboBox, IntToStr(EditorOpts.RightMargin));

  RightMarginColorLabel.Caption := dlgRightMarginColor;

  RightMarginColorButton.ButtonColor := EditorOpts.RightMarginColor;

  GutterWidthLabel.Caption := dlgGutterWidth;

  with GutterWidthComboBox do
    SetComboBoxText(GutterWidthComboBox, IntToStr(EditorOpts.GutterWidth));

  GutterColorLabel.Caption := dlgGutterColor;

  GutterColorButton.ButtonColor := EditorOpts.GutterColor;

  EditorFontGroupBox.Caption := dlgDefaultEditorFont;

  with EditorFontComboBox do
    SetComboBoxText(EditorFontComboBox, EditorOpts.EditorFont);

  EditorFontLabel.Caption := dlgEditorFont;

  with EditorFontHeightComboBox do
    SetComboBoxText(EditorFontHeightComboBox
      , IntToStr(EditorOpts.EditorFontHeight));

  EditorFontHeightLabel.Caption := dlgEditorFontHeight;

  with ExtraLineSpacingComboBox do
    SetComboBoxText(ExtraLineSpacingComboBox
      , IntToStr(EditorOpts.ExtraLineSpacing));

  ExtraLineSpacingLabel.Caption := dlgExtraLineSpacing;
end;

procedure TEditorOptionsForm.SetupKeyMappingsPage(Page: Integer);
begin
  MainNoteBook.Page[Page].Caption := dlgKeyMapping;

  KeyMappingChooseSchemeButton.Caption := lisEdOptsChooseScheme;

  KeyMappingConsistencyCheckButton.Caption := dlgCheckConsistency;

  KeyMappingHelpLabel.Caption := dlgEdHintCommand;
end;

procedure TEditorOptionsForm.SetupColorPage(Page: Integer);
var
  a: Integer;
begin
  MainNoteBook.Page[Page].Caption := dlgEdColor;

  LanguageLabel.Caption := dlgLang;

  with LanguageComboBox do
    with Items do
    begin
      BeginUpdate;
      for a := 0 to EditorOpts.HighlighterList.Count - 1 do
        Add(EditorOpts.HighlighterList[a].SynClass.GetLanguageName);
      //for a:=0 to EditorOpts.HighlighterList.Count-1 do
      //  writeln('TEditorOptionsForm.SetupColorPage ',a,' ',EditorOpts.HighlighterList[a].SynClass.GetLanguageName
      //  ,' ',EditorOpts.HighlighterList[a].SynClass.ClassName);
      EndUpdate;
    end;

  ColorSchemeLabel.Caption := dlgClrScheme;

  with ColorSchemeComboBox do
  begin
    with Items do
    begin
      BeginUpdate;
      // ToDo: fill also with custom color schemes
      Add(DefaultColorScheme);
      Add('Delphi');
      Add('Pascal Classic');
      Add('Twilight');
      Add('Ocean');
      EndUpdate;
    end;
    Text := DefaultColorScheme;
  end;

  FileExtensionsLabel.Caption := dlgFileExts;

  with FileExtensionsComboBox do
    if CurLanguageID >= 0 then
      SetComboBoxText(FileExtensionsComboBox,
        EditorOpts.HighlighterList[CurLanguageID].FileExtensions);

  ColorElementLabel.Caption := dlgEdElement;

  SetAttributeToDefaultButton.Caption := dlgSetElementDefault;

  SetAllAttributesToDefaultButton.Caption := dlgSetAllElementDefault;

  ForeGroundLabel.Caption := dlgForecolor;

  ForeGroundUseDefaultCheckBox.Caption := dlgEdUseDefColor;

  BackGroundLabel.Caption := dlgBackColor;

  BackgroundColorButton.Color := clBlue;

  BackGroundUseDefaultCheckBox.Caption := dlgEdUseDefColor;

  TextAttributesGroupBox.Caption := dlgTextAttributes;

  TextBoldCheckBox.Caption := dlgEdBold;

  TextItalicCheckBox.Caption := dlgEdItal;

  TextUnderlineCheckBox.Caption := dlgEdUnder;
end;

procedure TEditorOptionsForm.SetupCodeToolsPage(Page: Integer);
begin
  MainNoteBook.Page[Page].Caption := dlgCodeToolsTab;

  AutomaticFeaturesGroupBox.Caption := lisAutomaticFeatures;

  with AutoIdentifierCompletionCheckBox do
  begin
    Caption := dlgEdIdComlet;
    Checked := EditorOpts.AutoIdentifierCompletion;
  end;

  with AutoCodeParametersCheckBox do
  begin
    Caption := dlgEdCodeParams;
    Checked := EditorOpts.AutoCodeParameters;
  end;

  with AutoToolTipExprEvalCheckBox do
  begin
    Caption := dlgTooltipEval;
    Checked := EditorOpts.AutoToolTipExprEval;
  end;

  with AutoToolTipSymbToolsCheckBox do
  begin
    Caption := dlgTooltipTools;
    Checked := EditorOpts.AutoToolTipSymbTools;
  end;

  AutoDelayLabel.Caption := dlgEdDelay;
  AutoDelayTrackBar.Position := EditorOpts.AutoDelayInMSec;
  AutoDelayMinLabel.Caption := '0.5 ' + DlgTimeSecondUnit;
  AutoDelayMaxLabel.Caption := '4.0 ' + dlgTimeSecondUnit;
end;

procedure TEditorOptionsForm.SetupCodeFoldingPage(Page: integer);
begin
  MainNoteBook.Page[Page].Caption := dlgUseCodeFolding;
  chkCodeFoldingEnabled.Caption   := dlgUseCodeFolding;
  lblDividerDrawLevel.Caption     := dlgCFDividerDrawLevel + ':';
  
  chkCodeFoldingEnabled.Checked   := EditorOpts.UseCodeFolding;
  edDividerDrawLevel.Value        := EditorOpts.CFDividerDrawLevel;
end;

procedure TEditorOptionsForm.SetupButtonBar;
begin
  CancelButton.Caption := dlgCancel;

  CancelControl := CancelButton;

  OkButton.Caption := lisOk;
end;

procedure TEditorOptionsForm.OkButtonClick(Sender: TObject);
var
  SynOptions: TSynEditorOptions;
  i: Integer;
begin
  IDEDialogLayoutList.SaveLayout(Self);

  // save all values
  EditorOpts.KeyMap.Assign(EditingKeyMap);
  SynOptions := PreviewEdits[1].Options - [eoNoSelection, eoNoCaret];
  if CheckGroupItemChecked(EditorOptionsGroupBox,dlgBracHighlight) then
    Include(SynOptions, eoBracketHighlight)
  else
    Exclude(SynOptions, eoBracketHighlight);
  PreviewEdits[1].Options := SynOptions;
  EditorOpts.SetSynEditSettings(PreviewEdits[1]);
  PreviewEdits[1].Options :=
    SynOptions - [eoBracketHighlight] +
    [eoNoCaret, eoNoSelection];

  // general
  EditorOpts.ShowTabCloseButtons :=
    CheckGroupItemChecked(EditorOptionsGroupBox,dlgCloseButtonsNotebook);
  EditorOpts.UndoAfterSave :=
    CheckGroupItemChecked(EditorOptionsGroupBox,dlgUndoAfterSave);
  EditorOpts.CopyWordAtCursorOnCopyNone :=
    CheckGroupItemChecked(EditorOptionsGroupBox,dlgCopyWordAtCursorOnCopyNone);
  EditorOpts.ShowGutterHints :=
    CheckGroupItemChecked(EditorOptionsGroupBox,dlgShowGutterHints);
  EditorOpts.FindTextAtCursor :=
    CheckGroupItemChecked(EditorOptionsGroupBox,dlgFindTextatCursor);
  EditorOpts.UseSyntaxHighlight :=
    CheckGroupItemChecked(EditorOptionsGroupBox,dlgUseSyntaxHighlight);
  EditorOpts.CtrlMouseLinks :=
    CheckGroupItemChecked(EditorOptionsGroupBox,dlgMouseLinks);
  i := StrToIntDef(UndoLimitComboBox.Text, 32767);
  if i < 1 then
    i := 1;
  if i > 32767 then
    i := 32767;
  EditorOpts.UndoLimit := i;
  i := StrToIntDef(TabWidthsComboBox.Text, 2);
  if i < 1 then
    i := 1;
  if i > 20 then
    i := 20;
  EditorOpts.TabWidth := i;
  i := StrToIntDef(BlockIndentComboBox.Text, 2);
  if i < 1 then
    i := 1;
  if i > 20 then
    i := 20;
  EditorOpts.BlockIndent := i;


  // color
  SaveAllFileExtensions;
  SaveAllColorSchemes;
  SaveAllHighlighters;

  // code Tools
  EditorOpts.AutoIdentifierCompletion :=
    AutoIdentifierCompletionCheckBox.Checked;
  EditorOpts.AutoCodeParameters := AutoCodeParametersCheckBox.Checked;
  EditorOpts.AutoToolTipExprEval := AutoToolTipExprEvalCheckBox.Checked;
  EditorOpts.AutoToolTipSymbTools := AutoToolTipSymbToolsCheckBox.Checked;
  EditorOpts.AutoDelayInMSec    := AutoDelayTrackBar.Position;

  // Code Folding
  EditorOpts.UseCodeFolding       := chkCodeFoldingEnabled.Checked;
  EditorOpts.CFDividerDrawLevel   := edDividerDrawLevel.Value;
  
  EditorOpts.Save;
  ModalResult := mrOk;
end;

procedure TEditorOptionsForm.CancelButtonClick(Sender: TObject);
begin
  IDEDialogLayoutList.SaveLayout(Self);
  EditorOpts.Load;
  ModalResult := mrCancel;
end;

//=============================================================================

initialization
  {$I editoroptions.lrs}
  {$I lazarus_dci.lrs}

end.
