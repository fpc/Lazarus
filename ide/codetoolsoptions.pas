{ /***************************************************************************
                 codetoolsoptions.pas  -  Lazarus IDE unit
                 -----------------------------------------

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

  Author: Mattias Gaertner

  Abstract:
    - TCodeToolsOptions and TCodeToolsOptsDlg
}
unit CodeToolsOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IDEProcs, LazConf, LResources, Forms, Controls, Buttons,
  ExtCtrls, StdCtrls, ComCtrls, Dialogs, Laz_XMLCfg, CodeToolManager,
  DefineTemplates, SourceChanger, EditDefineTree, SynEdit,LazarusIDEStrConsts;

type
  TCodeToolsOptions = class
  private
    FFilename: string;
    
    // General
    FSrcPath: string;
    FAdjustTopLineDueToComment: boolean;
    FJumpCentered: boolean;
    FCursorBeyondEOL: boolean;

    // CodeCreation
    FAddInheritedCodeToOverrideMethod: boolean;
    FCompleteProperties: boolean;
    FLineLength: integer;
    FClassPartInsertPolicy: TClassPartInsertPolicy;
    FMixMethodsAndPorperties: boolean;
    FForwardProcInsertPolicy: TForwardProcInsertPolicy;
    FKeepForwardProcOrder: boolean;
    FMethodInsertPolicy: TMethodInsertPolicy;
    FKeyWordPolicy : TWordPolicy;
    FIdentifierPolicy: TWordPolicy;
    FDoNotSplitLineInFront: TAtomTypes;
    FDoNotSplitLineAfter: TAtomTypes;
    FDoInsertSpaceInFront: TAtomTypes;
    FDoInsertSpaceAfter: TAtomTypes;
    FPropertyReadIdentPrefix: string;
    FPropertyWriteIdentPrefix: string;
    FPropertyStoredIdentPostfix: string;
    FPrivatVariablePrefix: string;
    FSetPropertyVariablename: string;
    
    procedure SetFilename(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Load;
    procedure Save;
    procedure AssignTo(Boss: TCodeToolManager);
    property Filename: string read FFilename write SetFilename;
    procedure SetLazarusDefaultFilename;
    procedure Assign(CodeToolsOpts: TCodeToolsOptions);
    function IsEqual(CodeToolsOpts: TCodeToolsOptions): boolean;
    function CreateCopy: TCodeToolsOptions;
    
    // General
    property SrcPath: string read FSrcPath write FSrcPath;
    property AdjustTopLineDueToComment: boolean
      read FAdjustTopLineDueToComment write FAdjustTopLineDueToComment;
    property JumpCentered: boolean read FJumpCentered write FJumpCentered;
    property CursorBeyondEOL: boolean
      read FCursorBeyondEOL write FCursorBeyondEOL;
    
    // CodeCreation
    property CompleteProperties: boolean
      read FCompleteProperties write FCompleteProperties;
    property AddInheritedCodeToOverrideMethod: boolean
      read FAddInheritedCodeToOverrideMethod write FAddInheritedCodeToOverrideMethod;
    property LineLength: integer read FLineLength write FLineLength;
    property ClassPartInsertPolicy: TClassPartInsertPolicy
      read FClassPartInsertPolicy write FClassPartInsertPolicy;
    property MixMethodsAndPorperties: boolean
      read FMixMethodsAndPorperties write FMixMethodsAndPorperties;
    property ForwardProcInsertPolicy: TForwardProcInsertPolicy
      read FForwardProcInsertPolicy write FForwardProcInsertPolicy;
    property KeepForwardProcOrder: boolean
      read FKeepForwardProcOrder write FKeepForwardProcOrder;
    property MethodInsertPolicy: TMethodInsertPolicy
      read FMethodInsertPolicy write FMethodInsertPolicy;
    property KeyWordPolicy : TWordPolicy
      read FKeyWordPolicy write FKeyWordPolicy;
    property IdentifierPolicy: TWordPolicy
      read FIdentifierPolicy write FIdentifierPolicy;
    property DoNotSplitLineInFront: TAtomTypes
      read FDoNotSplitLineInFront write FDoNotSplitLineInFront;
    property DoNotSplitLineAfter: TAtomTypes
      read FDoNotSplitLineAfter write FDoNotSplitLineAfter;
    property DoInsertSpaceInFront: TAtomTypes
      read FDoInsertSpaceInFront write FDoInsertSpaceInFront;
    property DoInsertSpaceAfter: TAtomTypes
      read FDoInsertSpaceAfter write FDoInsertSpaceAfter;
    property PropertyReadIdentPrefix: string
      read FPropertyReadIdentPrefix write FPropertyReadIdentPrefix;
    property PropertyWriteIdentPrefix: string
      read FPropertyWriteIdentPrefix write FPropertyWriteIdentPrefix;
    property PropertyStoredIdentPostfix: string
      read FPropertyStoredIdentPostfix write FPropertyStoredIdentPostfix;
    property PrivatVariablePrefix: string
      read FPrivatVariablePrefix write FPrivatVariablePrefix;
    property SetPropertyVariablename: string
      read FSetPropertyVariablename write FSetPropertyVariablename;
  end;

  TCodeToolsOptsDlg = class(TForm)
    NoteBook: TNoteBook;
    
    // General
    SrcPathGroupBox: TGroupBox;
    SrcPathEdit: TEdit;
    JumpingGroupBox: TGroupBox;
    AdjustTopLineDueToCommentCheckBox: TCheckBox;
    JumpCenteredCheckBox: TCheckBox;
    CursorBeyondEOLCheckBox: TCheckBox;
    
    // Code Creation
    ClassPartInsertPolicyRadioGroup: TRadioGroup;
    MixMethodsAndPorpertiesCheckBox: TCheckBox;
    MethodInsertPolicyRadioGroup: TRadioGroup;
    ForwardProcsInsertPolicyRadioGroup: TRadioGroup;
    ForwardProcsKeepOrderCheckBox: TCheckBox;
    PropertyCompletionGroupBox: TGroupBox;
    PropertyCompletionCheckBox: TCheckBox;
    PropertyReadIdentPrefixLabel: TLabel;
    PropertyReadIdentPrefixEdit: TEdit;
    PropertyWriteIdentPrefixLabel: TLabel;
    PropertyWriteIdentPrefixEdit: TEdit;
    PropertyStoredIdentPostfixLabel: TLabel;
    PropertyStoredIdentPostfixEdit: TEdit;
    PrivatVariablePrefixLabel: TLabel;
    PrivatVariablePrefixEdit: TEdit;
    SetPropertyVariablenameLabel: TLabel;
    SetPropertyVariablenameEdit: TEdit;

    // words
    KeyWordPolicyRadioGroup: TRadioGroup;
    IdentifierPolicyRadioGroup: TRadioGroup;

    // Line Splitting
    LineLengthLabel: TLabel;
    LineLengthEdit: TEdit;
    DoNotSplitLineInFrontGroupBox: TGroupBox;
    DoNotSplitLineAfterGroupBox: TGroupBox;
    SplitPreviewGroupBox: TGroupBox;
    SplitPreviewSynEdit: TSynEdit;
    
    // Space
    DoInsertSpaceInFrontGroupBox: TGroupBox;
    DoInsertSpaceAfterGroupBox: TGroupBox;
    SpacePreviewGroupBox: TGroupBox;
    SpacePreviewSynEdit: TSynEdit;

    // buttons at bottom
    OkButton: TButton;
    CancelButton: TButton;
    
    procedure CodeToolsOptsDlgResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure UpdateExamples(Sender: TObject);
  private
    FOnGetSynEditSettings: TNotifyEvent;
    BeautifyCodeOptions: TBeautifyCodeOptions;
    procedure SetupGeneralPage(PageID: integer);
    procedure SetupCodeCreationPage(PageID: integer);
    procedure SetupWordsPage(PageID: integer);
    procedure SetupLineSplittingPage(PageID: integer);
    procedure SetupSpacePage(PageID: integer);
    procedure ResizeGeneralPage;
    procedure ResizeCodeCreationPage;
    procedure ResizeWordsPage;
    procedure ResizeLineSplittingPage;
    procedure ResizeSpacePage;
    procedure CreateAtomCheckBoxes(ParentGroupBox: TGroupBox;
      AtomTypes: TAtomTypes; Columns: integer);
    procedure SetAtomCheckBoxes(AtomTypes: TAtomTypes;
      ParentGroupBox: TGroupBox);
    function ReadAtomCheckBoxes(ParentGroupBox: TGroupBox): TAtomTypes;
    procedure UpdateSinglePreviewSettings(APreview: TSynEdit);
    procedure WriteBeautifyCodeOptions(Options: TBeautifyCodeOptions);
    procedure UpdateSplitLineExample;
    procedure UpdateSpaceExample;
  public
    procedure ReadSettings(Options: TCodeToolsOptions);
    procedure WriteSettings(Options: TCodeToolsOptions);
    procedure UpdatePreviewSettings;
    constructor Create(AnOwner:TComponent);  override;
    destructor Destroy; override;
    property OnGetSynEditSettings: TNotifyEvent
      read FOnGetSynEditSettings write FOnGetSynEditSettings;
  end;

var CodeToolsOpts: TCodeToolsOptions;

function ShowCodeToolsOptions(Options: TCodeToolsOptions;
  OnGetSynEditSettings: TNotifyEvent): TModalResult;


implementation


const
  CodeToolsOptionsVersion = 1;
  DefaultCodeToolsOptsFile = 'codetoolsoptions.xml';
  
  AtomTypeDescriptions: array[TAtomType] of shortstring = (
      'None', 'Keyword', 'Identifier', 'Colon', 'Semicolon', 'Comma', 'Point',
      'At', 'Number', 'String constant', 'Newline', 'Space', 'Symbol'
    );
  DoNotSplitAtoms = [atKeyword, atIdentifier, atColon, atSemicolon, atComma,
               atPoint, atAt, atNumber, atStringConstant, atSpace, atSymbol];
  DoInsertSpaceAtoms = [atKeyword, atIdentifier, atColon, atSemicolon, atComma,
               atPoint, atAt, atNumber, atStringConstant, atSymbol];

  LineSplitExampleText =
       'function(Sender: TObject; const Val1, Val2, Val3:char; '
      +'var Var1, Var2: array of const): integer;'#13
      +'const s=''abc''#13#10+''xyz'';';
  SpaceExampleText =
       'function(Sender:TObject;const Val1,Val2,Val3:char;'
      +'var Var1,Var2:array of const):integer;'#13
      +'const s=''abc''#13#10+''xyz'';'#13
      +'begin'#13
      +'  A:=@B.C;D:=3;'#13
      +'end;';

function AtomTypeDescriptionToType(const s: string): TAtomType;
begin
  for Result:=Low(TAtomType) to High(TAtomType) do begin
    if s=AtomTypeDescriptions[Result] then exit;
  end;
  Result:=atNone;
end;

function ReadAtomTypesFromXML(XMLConfig: TXMLConfig; const Path: string;
  DefaultValues: TAtomTypes): TAtomTypes;
var a: TAtomType;
begin
  Result:=[];
  for a:=Low(TAtomType) to High(TAtomType) do begin
    if (a<>atNone)
    and (XMLConfig.GetValue(Path+AtomTypeNames[a]+'/Value',a in DefaultValues))
    then
      Include(Result,a);
  end;
end;

procedure WriteAtomTypesToXML(XMLConfig: TXMLConfig; const Path: string;
  NewValues: TAtomTypes);
var a: TAtomType;
begin
  for a:=Low(TAtomType) to High(TAtomType) do begin
    if (a<>atNone) then
      XMLConfig.SetValue(Path+AtomTypeNames[a]+'/Value',a in NewValues);
  end;
end;


function IsIdentifier(const s: string): boolean;
var i: integer;
begin
  Result:=false;
  if (s='') then exit;
  for i:=1 to length(s) do begin
    if not (s[i] in ['_','A'..'Z','a'..'z']) then exit;
  end;
  Result:=true;
end;

function ReadIdentifier(
  const s, DefaultIdent: string): string;
begin
  if IsIdentifier(s) then
    Result:=s
  else
    Result:=DefaultIdent;
end;

{ TCodeToolsOptions }

constructor TCodeToolsOptions.Create;
begin
  inherited Create;
  FFilename:='';
  Clear;
end;

destructor TCodeToolsOptions.Destroy;
begin

  inherited Destroy;
end;

procedure TCodeToolsOptions.Load;
var
  XMLConfig: TXMLConfig;
  FileVersion: integer;
begin
  try
    XMLConfig:=TXMLConfig.Create(FFileName);
    FileVersion:=XMLConfig.GetValue('CodeToolsOptions/Version/Value',0);
    if (FileVersion<>0) and (FileVersion<CodeToolsOptionsVersion) then
      writeln('NOTE: loading old codetools options file: ',FFileName);

    // General
    FSrcPath:=XMLConfig.GetValue('CodeToolsOptions/SrcPath/Value','');
    FAdjustTopLineDueToComment:=XMLConfig.GetValue(
      'CodeToolsOptions/AdjustTopLineDueToComment/Value',true);
    FJumpCentered:=XMLConfig.GetValue('CodeToolsOptions/JumpCentered/Value',
      true);
    FCursorBeyondEOL:=XMLConfig.GetValue(
      'CodeToolsOptions/CursorBeyondEOL/Value',true);
    
    // CodeCreation
    FAddInheritedCodeToOverrideMethod:=XMLConfig.GetValue(
      'CodeToolsOptions/AddInheritedCodeToOverrideMethod/Value',true);
    FCompleteProperties:=XMLConfig.GetValue(
      'CodeToolsOptions/CompleteProperties/Value',true);
    FLineLength:=XMLConfig.GetValue(
      'CodeToolsOptions/LineLengthXMLConfig/Value',80);
    FClassPartInsertPolicy:=ClassPartPolicyNameToPolicy(XMLConfig.GetValue(
      'CodeToolsOptions/ClassPartInsertPolicy/Value',
      ClassPartInsertPolicyNames[cpipAlphabetically]));
    FMixMethodsAndPorperties:=XMLConfig.GetValue(
      'CodeToolsOptions/MixMethodsAndPorperties/Value',false);
    FForwardProcInsertPolicy:=ForwardProcInsertPolicyNameToPolicy(
      XMLConfig.GetValue('CodeToolsOptions/ForwardProcInsertPolicy/Value',
        ForwardProcInsertPolicyNames[fpipInFrontOfMethods]));
    FKeepForwardProcOrder:=XMLConfig.GetValue(
      'CodeToolsOptions/KeepForwardProcOrder/Value',true);

    FMethodInsertPolicy:=MethodInsertPolicyNameToPolicy(XMLConfig.GetValue(
      'CodeToolsOptions/MethodInsertPolicy/Value',
      MethodInsertPolicyNames[mipClassOrder]));
    FKeyWordPolicy:=WordPolicyNameToPolicy(XMLConfig.GetValue(
      'CodeToolsOptions/KeyWordPolicy/Value',
      WordPolicyNames[wpLowerCase]));
    FIdentifierPolicy:=WordPolicyNameToPolicy(XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierPolicy/Value',
      WordPolicyNames[wpNone]));
    FDoNotSplitLineInFront:=ReadAtomTypesFromXML(XMLConfig,
      'CodeToolsOptions/DoNotSplitLineInFront/',DefaultDoNotSplitLineInFront);
    FDoNotSplitLineAfter:=ReadAtomTypesFromXML(XMLConfig,
      'CodeToolsOptions/DoNotSplitLineAfter/',DefaultDoNotSplitLineAfter);
    FDoInsertSpaceInFront:=ReadAtomTypesFromXML(XMLConfig,
      'CodeToolsOptions/DoInsertSpaceInFront/',DefaultDoInsertSpaceInFront);
    FDoInsertSpaceAfter:=ReadAtomTypesFromXML(XMLConfig,
      'CodeToolsOptions/DoInsertSpaceAfter/',DefaultDoInsertSpaceAfter);
    FPropertyReadIdentPrefix:=ReadIdentifier(XMLConfig.GetValue(
      'CodeToolsOptions/PropertyReadIdentPrefix/Value',''),'Get');
    FPropertyWriteIdentPrefix:=ReadIdentifier(XMLConfig.GetValue(
      'CodeToolsOptions/PropertyWriteIdentPrefix/Value',''),'Set');
    FPropertyStoredIdentPostfix:=ReadIdentifier(XMLConfig.GetValue(
      'CodeToolsOptions/PropertyStoredIdentPostfix/Value',''),'IsStored');
    FPrivatVariablePrefix:=ReadIdentifier(XMLConfig.GetValue(
      'CodeToolsOptions/PrivatVariablePrefix/Value',''),'F');
    FSetPropertyVariablename:=ReadIdentifier(XMLConfig.GetValue(
      'CodeToolsOptions/SetPropertyVariablename/Value',''),'AValue');

    XMLConfig.Free;

  except
    // ToDo
    writeln('[TCodeToolsOptions.Load]  error reading "',FFilename,'"');
  end;
end;

procedure TCodeToolsOptions.Save;
var
  XMLConfig: TXMLConfig;
begin
  try
    XMLConfig:=TXMLConfig.Create(FFileName);
    XMLConfig.SetValue('CodeToolsOptions/Version/Value',
      CodeToolsOptionsVersion);

    // General
    XMLConfig.SetValue('CodeToolsOptions/SrcPath/Value',FSrcPath);
    XMLConfig.SetValue('CodeToolsOptions/AdjustTopLineDueToComment/Value',
      FAdjustTopLineDueToComment);
    XMLConfig.SetValue('CodeToolsOptions/JumpCentered/Value',FJumpCentered);
    XMLConfig.SetValue('CodeToolsOptions/CursorBeyondEOL/Value',
      FCursorBeyondEOL);

    // CodeCreation
    XMLConfig.SetValue(
      'CodeToolsOptions/AddInheritedCodeToOverrideMethod/Value',
      AddInheritedCodeToOverrideMethod);
    XMLConfig.SetValue(
      'CodeToolsOptions/CompleteProperties/Value',CompleteProperties);
    XMLConfig.SetValue(
      'CodeToolsOptions/LineLengthXMLConfig/Value',FLineLength);
    XMLConfig.SetValue('CodeToolsOptions/ClassPartInsertPolicy/Value',
      ClassPartInsertPolicyNames[FClassPartInsertPolicy]);
    XMLConfig.SetValue(
      'CodeToolsOptions/MixMethodsAndPorperties/Value',FMixMethodsAndPorperties);
    XMLConfig.SetValue('CodeToolsOptions/ForwardProcInsertPolicy/Value',
      ForwardProcInsertPolicyNames[FForwardProcInsertPolicy]);
    XMLConfig.SetValue(
      'CodeToolsOptions/KeepForwardProcOrder/Value',FKeepForwardProcOrder);
    XMLConfig.SetValue('CodeToolsOptions/MethodInsertPolicy/Value',
      MethodInsertPolicyNames[FMethodInsertPolicy]);
    XMLConfig.SetValue('CodeToolsOptions/KeyWordPolicy/Value',
      WordPolicyNames[FKeyWordPolicy]);
    XMLConfig.SetValue('CodeToolsOptions/IdentifierPolicy/Value',
      WordPolicyNames[FIdentifierPolicy]);
    WriteAtomTypesToXML(XMLConfig,'CodeToolsOptions/DoNotSplitLineInFront/',
      FDoNotSplitLineInFront);
    WriteAtomTypesToXML(XMLConfig,'CodeToolsOptions/DoNotSplitLineAfter/',
      FDoNotSplitLineAfter);
    WriteAtomTypesToXML(XMLConfig,'CodeToolsOptions/DoInsertSpaceInFront/',
      FDoInsertSpaceInFront);
    WriteAtomTypesToXML(XMLConfig,'CodeToolsOptions/DoInsertSpaceAfter/',
      FDoInsertSpaceAfter);
    XMLConfig.SetValue('CodeToolsOptions/PropertyReadIdentPrefix/Value',
      FPropertyReadIdentPrefix);
    XMLConfig.SetValue('CodeToolsOptions/PropertyWriteIdentPrefix/Value',
      FPropertyWriteIdentPrefix);
    XMLConfig.SetValue('CodeToolsOptions/PropertyStoredIdentPostfix/Value',
      FPropertyStoredIdentPostfix);
    XMLConfig.SetValue('CodeToolsOptions/PrivatVariablePrefix/Value',
      FPrivatVariablePrefix);
    XMLConfig.SetValue('CodeToolsOptions/SetPropertyVariablename/Value',
      FSetPropertyVariablename);

    XMLConfig.Flush;
    XMLConfig.Free;
  except
    writeln('ERROR: error while writing codetools options "',FFilename,'"');
  end;
end;

procedure TCodeToolsOptions.SetFilename(const AValue: string);
begin
  FFilename:=AValue;
end;

procedure TCodeToolsOptions.SetLazarusDefaultFilename;
var
  ConfFileName: string;
begin
  ConfFileName:=SetDirSeparators(
                             GetPrimaryConfigPath+'/'+DefaultCodeToolsOptsFile);
  CopySecondaryConfigFile(DefaultCodeToolsOptsFile);
  if (not FileExists(ConfFileName)) then begin
    writeln('NOTE: codetools config file not found - using defaults');
  end;
  FFilename:=ConfFilename;
end;

procedure TCodeToolsOptions.Assign(CodeToolsOpts: TCodeToolsOptions);
begin
  if CodeToolsOpts<>nil then begin
    // General
    FSrcPath:=CodeToolsOpts.FSrcPath;
    FAdjustTopLineDueToComment:=CodeToolsOpts.FAdjustTopLineDueToComment;
    FJumpCentered:=CodeToolsOpts.FJumpCentered;
    FCursorBeyondEOL:=CodeToolsOpts.FCursorBeyondEOL;
    FAddInheritedCodeToOverrideMethod:=CodeToolsOpts.AddInheritedCodeToOverrideMethod;
    FCompleteProperties:=CodeToolsOpts.CompleteProperties;

    // CodeCreation
    FLineLength:=CodeToolsOpts.FLineLength;
    FClassPartInsertPolicy:=CodeToolsOpts.FClassPartInsertPolicy;
    FMixMethodsAndPorperties:=CodeToolsOpts.MixMethodsAndPorperties;
    FForwardProcInsertPolicy:=CodeToolsOpts.ForwardProcInsertPolicy;
    FKeepForwardProcOrder:=CodeToolsOpts.KeepForwardProcOrder;
    FMethodInsertPolicy:=CodeToolsOpts.FMethodInsertPolicy;
    FKeyWordPolicy:=CodeToolsOpts.FKeyWordPolicy;
    FIdentifierPolicy:=CodeToolsOpts.FIdentifierPolicy;
    FDoNotSplitLineInFront:=CodeToolsOpts.FDoNotSplitLineInFront;
    FDoNotSplitLineAfter:=CodeToolsOpts.FDoNotSplitLineAfter;
    FDoInsertSpaceInFront:=CodeToolsOpts.FDoInsertSpaceInFront;
    FDoInsertSpaceAfter:=CodeToolsOpts.FDoInsertSpaceAfter;
    FPropertyReadIdentPrefix:=CodeToolsOpts.FPropertyReadIdentPrefix;
    FPropertyWriteIdentPrefix:=CodeToolsOpts.FPropertyWriteIdentPrefix;
    FPropertyStoredIdentPostfix:=CodeToolsOpts.FPropertyStoredIdentPostfix;
    FPrivatVariablePrefix:=CodeToolsOpts.FPrivatVariablePrefix;
    FSetPropertyVariablename:=CodeToolsOpts.FSetPropertyVariablename;
  end else begin
    Clear;
  end;
end;

procedure TCodeToolsOptions.Clear;
// !!! Does not reset Filename !!!
begin
  // General
  FSrcPath:='';
  FAdjustTopLineDueToComment:=true;
  FJumpCentered:=true;
  FCursorBeyondEOL:=true;

  // CodeCreation
  FAddInheritedCodeToOverrideMethod:=true;
  FCompleteProperties:=true;
  FLineLength:=80;
  FClassPartInsertPolicy:=cpipLast;
  FMixMethodsAndPorperties:=false;
  FForwardProcInsertPolicy:=fpipInFrontOfMethods;
  FKeepForwardProcOrder:=true;
  FMethodInsertPolicy:=mipClassOrder;
  FKeyWordPolicy:=wpLowerCase;
  FIdentifierPolicy:=wpNone;
  FDoNotSplitLineInFront:=DefaultDoNotSplitLineInFront;
  FDoNotSplitLineAfter:=DefaultDoNotSplitLineAfter;
  FDoInsertSpaceInFront:=DefaultDoInsertSpaceInFront;
  FDoInsertSpaceAfter:=DefaultDoInsertSpaceAfter;
  FPropertyReadIdentPrefix:='Get';
  FPropertyWriteIdentPrefix:='Set';
  FPropertyStoredIdentPostfix:='IsStored';
  FPrivatVariablePrefix:='f';
  FSetPropertyVariablename:='AValue';
end;

function TCodeToolsOptions.IsEqual(CodeToolsOpts: TCodeToolsOptions): boolean;
begin
  Result:=
    // General
        (FSrcPath=CodeToolsOpts.FSrcPath)
    and (FAdjustTopLineDueToComment=CodeToolsOpts.FAdjustTopLineDueToComment)
    and (FJumpCentered=CodeToolsOpts.FJumpCentered)
    and (FCursorBeyondEOL=CodeToolsOpts.FCursorBeyondEOL)
    and (AddInheritedCodeToOverrideMethod=CodeToolsOpts.AddInheritedCodeToOverrideMethod)
    and (CompleteProperties=CodeToolsOpts.CompleteProperties)

    // CodeCreation
    and (FLineLength=CodeToolsOpts.FLineLength)
    and (FClassPartInsertPolicy=CodeToolsOpts.FClassPartInsertPolicy)
    and (FMixMethodsAndPorperties=CodeToolsOpts.MixMethodsAndPorperties)
    and (FForwardProcInsertPolicy=CodeToolsOpts.ForwardProcInsertPolicy)
    and (FKeepForwardProcOrder=CodeToolsOpts.KeepForwardProcOrder)
    and (FMethodInsertPolicy=CodeToolsOpts.FMethodInsertPolicy)
    and (FKeyWordPolicy=CodeToolsOpts.FKeyWordPolicy)
    and (FIdentifierPolicy=CodeToolsOpts.FIdentifierPolicy)
    and (FDoNotSplitLineInFront=CodeToolsOpts.FDoNotSplitLineInFront)
    and (FDoNotSplitLineAfter=CodeToolsOpts.FDoNotSplitLineAfter)
    and (FDoInsertSpaceInFront=CodeToolsOpts.FDoInsertSpaceInFront)
    and (FDoInsertSpaceAfter=CodeToolsOpts.FDoInsertSpaceAfter)
    and (FPropertyReadIdentPrefix=CodeToolsOpts.FPropertyReadIdentPrefix)
    and (FPropertyWriteIdentPrefix=CodeToolsOpts.FPropertyWriteIdentPrefix)
    and (FPropertyStoredIdentPostfix=CodeToolsOpts.FPropertyStoredIdentPostfix)
    and (FPrivatVariablePrefix=CodeToolsOpts.FPrivatVariablePrefix)
    and (FSetPropertyVariablename=CodeToolsOpts.FSetPropertyVariablename)
   ;
end;

function TCodeToolsOptions.CreateCopy: TCodeToolsOptions;
begin
  Result:=TCodeToolsOptions.Create;
  Result.Assign(Self);
  Result.Filename:=Filename;
end;

procedure TCodeToolsOptions.AssignTo(Boss: TCodeToolManager);
begin
  // General - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  SetAdditionalGlobalSrcPathToCodeToolBoss(SrcPath);
  Boss.AdjustTopLineDueToComment:=AdjustTopLineDueToComment;
  Boss.JumpCentered:=JumpCentered;
  Boss.CursorBeyondEOL:=CursorBeyondEOL;
  Boss.AddInheritedCodeToOverrideMethod:=AddInheritedCodeToOverrideMethod;
  Boss.CompleteProperties:=CompleteProperties;

  // CreateCode - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  with Boss.SourceChangeCache do begin
    BeautifyCodeOptions.LineLength:=LineLength;
    BeautifyCodeOptions.ClassPartInsertPolicy:=ClassPartInsertPolicy;
    BeautifyCodeOptions.MixMethodsAndPorperties:=MixMethodsAndPorperties;
    BeautifyCodeOptions.ForwardProcInsertPolicy:=ForwardProcInsertPolicy;
    BeautifyCodeOptions.KeepForwardProcOrder:=KeepForwardProcOrder;
    BeautifyCodeOptions.MethodInsertPolicy:=MethodInsertPolicy;
    BeautifyCodeOptions.KeyWordPolicy:=KeyWordPolicy;
    BeautifyCodeOptions.IdentifierPolicy:=IdentifierPolicy;
    BeautifyCodeOptions.DoNotSplitLineInFront:=DoNotSplitLineInFront;
    BeautifyCodeOptions.DoNotSplitLineAfter:=DoNotSplitLineAfter;
    BeautifyCodeOptions.DoInsertSpaceInFront:=DoInsertSpaceInFront;
    BeautifyCodeOptions.DoInsertSpaceAfter:=DoInsertSpaceAfter;
    BeautifyCodeOptions.PropertyReadIdentPrefix:=PropertyReadIdentPrefix;
    BeautifyCodeOptions.PropertyWriteIdentPrefix:=PropertyWriteIdentPrefix;
    BeautifyCodeOptions.PropertyStoredIdentPostfix:=PropertyStoredIdentPostfix;
    BeautifyCodeOptions.PrivatVariablePrefix:=PrivatVariablePrefix;
  end;
  Boss.SetPropertyVariablename:=SetPropertyVariablename;
end;

{ TCodeToolsOptsDlg }

constructor TCodeToolsOptsDlg.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Width:=485;
    Height:=435;
    Position:=poScreenCenter;
    Caption:=dlgCodeToolsOpts;
    OnResize:=@CodeToolsOptsDlgResize;

    NoteBook:=TNoteBook.Create(Self);
    with NoteBook do begin
      Name:='NoteBook';
      Parent:=Self;
      SetBounds(0,0,Self.ClientWidth,Self.ClientHeight-50);
      if PageCount>0 then
        Pages[0]:=lisMenuInsertGeneral
      else
        Pages.Add(lisMenuInsertGeneral);//by VVI - using first phrase, otherwise we''ll encounter a problem with .po
	
      Pages.Add(dlgCodeCreation);
      Pages.Add(dlgWordsPolicies);
      Pages.Add(dlgLineSplitting);
      Pages.Add(dlgSpaceNotCosmos);
      PageIndex:=0;
    end;

    SetupGeneralPage(0);
    SetupCodeCreationPage(1);
    SetupWordsPage(2);
    SetupLineSplittingPage(3);
    SetupSpacePage(4);

    NoteBook.Show;

    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      Width:=70;
      Height:=23;
      Left:=Self.ClientWidth-Width-15;
      Top:=Self.ClientHeight-Height-15;
      Caption:=dlgCancel;
      OnClick:=@CancelButtonClick;
      Visible:=true;
    end;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      Width:=CancelButton.Width;
      Height:=CancelButton.Height;
      Left:=CancelButton.Left-15-Width;
      Top:=CancelButton.Top;
      Caption:='Ok';
      OnClick:=@OkButtonClick;
      Visible:=true;
    end;
  end;
  BeautifyCodeOptions:=TBeautifyCodeOptions.Create;
  CodeToolsOptsDlgResize(nil);
  UpdateExamples(Self);
end;

destructor TCodeToolsOptsDlg.Destroy;
begin
  BeautifyCodeOptions.Free;
  inherited Destroy;
end;

procedure TCodeToolsOptsDlg.SetupGeneralPage(PageID: integer);
begin
  SrcPathGroupBox:=TGroupBox.Create(Self);
  with SrcPathGroupBox do begin
    Name:='SrcPathGroupBox';
    Parent:=NoteBook.Page[PageID];
    SetBounds(8,7,Self.ClientWidth-20,51);
    Caption:=dlgAdditionalSrcPath ;
    Visible:=true;
  end;
  
  SrcPathEdit:=TEdit.Create(Self);
  with SrcPathEdit do begin
    Name:='SrcPathEdit';
    Parent:=SrcPathGroupBox;
    SetBounds(5,6,Parent.ClientWidth-14,Height);
    Visible:=true;
  end;
  
  JumpingGroupBox:=TGroupBox.Create(Self);
  with JumpingGroupBox do begin
    Name:='JumpingGroupBox';
    Parent:=NoteBook.Page[PageID];
    SetBounds(8,SrcPathGroupBox.Top+SrcPathGroupBox.Height+7,
      SrcPathGroupBox.Width,95);
    Caption:=dlgJumpingETC;
    Visible:=true;
  end;

  AdjustTopLineDueToCommentCheckBox:=TCheckBox.Create(Self);
  with AdjustTopLineDueToCommentCheckBox do begin
    Name:='AdjustTopLineDueToCommentCheckBox';
    Parent:=JumpingGroupBox;
    SetBounds(5,6,Parent.ClientWidth-10,Height);
    Caption:=dlgAdjustTopLine;
    Visible:=true;
  end;

  JumpCenteredCheckBox:=TCheckBox.Create(Self);
  with JumpCenteredCheckBox do begin
    Name:='JumpCenteredCheckBox';
    Parent:=JumpingGroupBox;
    SetBounds(AdjustTopLineDueToCommentCheckBox.Left,
      AdjustTopLineDueToCommentCheckBox.Top+2
      +AdjustTopLineDueToCommentCheckBox.Height,
      AdjustTopLineDueToCommentCheckBox.Width,Height);
    Caption:=dlgcentercursorline;
    Visible:=true;
  end;

  CursorBeyondEOLCheckBox:=TCheckBox.Create(Self);
  with CursorBeyondEOLCheckBox do begin
    Name:='CursorBeyondEOLCheckBox';
    Parent:=JumpingGroupBox;
    SetBounds(JumpCenteredCheckBox.Left,
      JumpCenteredCheckBox.Top+JumpCenteredCheckBox.Height+2,
      JumpCenteredCheckBox.Width,Height);
    Caption:=dlgcursorbeyondeol;
    Visible:=true;
  end;
end;

procedure TCodeToolsOptsDlg.SetupCodeCreationPage(PageID: integer);
begin
  ClassPartInsertPolicyRadioGroup:=TRadioGroup.Create(Self);
  with ClassPartInsertPolicyRadioGroup do begin
    Name:='ClassPartInsertPolicyRadioGroup';
    Parent:=NoteBook.Page[PageID];
    SetBounds(8,6,(Self.ClientWidth div 2)-12,70);
    Caption:=dlgClassInsertPolicy;
    with Items do begin
      BeginUpdate;
      Add(dlgAlphabetically);
      Add(dlgCDTLast);
      EndUpdate;
    end;
    Visible:=true;
  end;
  
  MixMethodsAndPorpertiesCheckBox:=TCheckBox.Create(Self);
  with MixMethodsAndPorpertiesCheckBox do begin
    Name:='MixMethodsAndPorpertiesCheckBox';
    Parent:=NoteBook.Page[PageID];
    SetBounds(ClassPartInsertPolicyRadioGroup.Left,
       ClassPartInsertPolicyRadioGroup.Top+ClassPartInsertPolicyRadioGroup.Height+5,
       ClassPartInsertPolicyRadioGroup.Width,Height);
    Caption:=dlgMixMethodsAndProperties;
    Visible:=true;
  end;

  MethodInsertPolicyRadioGroup:=TRadioGroup.Create(Self);
  with MethodInsertPolicyRadioGroup do begin
    Name:='MethodInsertPolicyRadioGroup';
    Parent:=NoteBook.Page[PageID];
    SetBounds(ClassPartInsertPolicyRadioGroup.Left,
      MixMethodsAndPorpertiesCheckBox.Top
      +MixMethodsAndPorpertiesCheckBox.Height+10,
      ClassPartInsertPolicyRadioGroup.Width,
      100);
    Caption:=dlgMethodInsPolicy;
    with Items do begin
      BeginUpdate;
      Add(dlgAlphabetically);
      Add(dlgCDTLast);
      Add(dlgCDTClassOrder );
      EndUpdate;
    end;
    Visible:=true;
  end;

  ForwardProcsInsertPolicyRadioGroup:=TRadioGroup.Create(Self);
  with ForwardProcsInsertPolicyRadioGroup do begin
    Name:='ForwardProcsInsertPolicyRadioGroup';
    Parent:=NoteBook.Page[PageID];;
    SetBounds(ClassPartInsertPolicyRadioGroup.Left
         +ClassPartInsertPolicyRadioGroup.Width+8,
       ClassPartInsertPolicyRadioGroup.Top,
       ClassPartInsertPolicyRadioGroup.Width,100);
    Caption:=dlgForwardProcsInsertPolicy;
    with Items do begin
      BeginUpdate;
      Add(dlgLast);
      Add(dlgInFrontOfMethods);
      Add(dlgBehindMethods);
      EndUpdate;
    end;
    Visible:=true;
  end;
  
  ForwardProcsKeepOrderCheckBox:=TCheckBox.Create(Self);
  with ForwardProcsKeepOrderCheckBox do begin
    Name:='ForwardProcsKeepOrderCheckBox';
    Parent:=NoteBook.Page[PageID];;
    SetBounds(ForwardProcsInsertPolicyRadioGroup.Left,
       ForwardProcsInsertPolicyRadioGroup.Top
         +ForwardProcsInsertPolicyRadioGroup.Height+5,
       ForwardProcsInsertPolicyRadioGroup.Width,Height);
    Caption:=dlgForwardProcsKeepOrder;
    Visible:=true;
  end;

  PropertyCompletionGroupBox:=TGroupBox.Create(Self);
  with PropertyCompletionGroupBox do begin
    Name:='PropertyCompletionGroupBox';
    Parent:=NoteBook.Page[PageID];
    SetBounds(ClassPartInsertPolicyRadioGroup.Left,
      MethodInsertPolicyRadioGroup.Top+MethodInsertPolicyRadioGroup.Height+7,
      Self.ClientWidth-20,125);
    Caption:=dlgPropertyCompletion;
    Visible:=true;
  end;

  PropertyCompletionCheckBox:=TCheckBox.Create(Self);
  with PropertyCompletionCheckBox do begin
    Name:='PropertyCompletionCheckBox';
    Parent:=PropertyCompletionGroupBox;
    SetBounds(6,5,200,Height);
    Caption:=dlgCompleteProperties ;
    Visible:=true;
  end;

  PropertyReadIdentPrefixLabel:=TLabel.Create(Self);
  with PropertyReadIdentPrefixLabel do begin
    Name:='PropertyReadIdentPrefixLabel';
    Parent:=PropertyCompletionGroupBox;
    SetBounds(PropertyCompletionCheckBox.Left,
      PropertyCompletionCheckBox.Top+PropertyCompletionCheckBox.Height+5,
      100,Height);
    Caption:=dlgCDTReadPrefix ;
    Visible:=true;
  end;

  PropertyReadIdentPrefixEdit:=TEdit.Create(Self);
  with PropertyReadIdentPrefixEdit do begin
    Name:='PropertyReadIdentPrefixEdit';
    Parent:=PropertyCompletionGroupBox;
    SetBounds(110,PropertyReadIdentPrefixLabel.Top,80,Height);
    Visible:=true;
  end;

  PropertyWriteIdentPrefixLabel:=TLabel.Create(Self);
  with PropertyWriteIdentPrefixLabel do begin
    Name:='PropertyWriteIdentPrefixLabel';
    Parent:=PropertyCompletionGroupBox;
    SetBounds(6,PropertyReadIdentPrefixLabel.Top
      +PropertyReadIdentPrefixLabel.Height+5,
      PropertyReadIdentPrefixLabel.Width,Height);
    Caption:=dlgCDTWritePrefix ;
    Visible:=true;
  end;

  PropertyWriteIdentPrefixEdit:=TEdit.Create(Self);
  with PropertyWriteIdentPrefixEdit do begin
    Name:='PropertyWriteIdentPrefixEdit';
    Parent:=PropertyCompletionGroupBox;
    SetBounds(PropertyReadIdentPrefixEdit.Left,
      PropertyWriteIdentPrefixLabel.Top,80,Height);
    Visible:=true;
  end;

  PropertyStoredIdentPostfixLabel:=TLabel.Create(Self);
  with PropertyStoredIdentPostfixLabel do begin
    Name:='PropertyStoredIdentPostfixLabel';
    Parent:=PropertyCompletionGroupBox;
    SetBounds(6,PropertyWriteIdentPrefixLabel.Top
      +PropertyWriteIdentPrefixLabel.Height+5,
      PropertyReadIdentPrefixLabel.Width,Height);
    Caption:=dlgCDTStoredPostfix;
    Visible:=true;
  end;

  PropertyStoredIdentPostfixEdit:=TEdit.Create(Self);
  with PropertyStoredIdentPostfixEdit do begin
    Name:='PropertyStoredIdentPostfixEdit';
    Parent:=PropertyCompletionGroupBox;
    SetBounds(PropertyReadIdentPrefixEdit.Left,
      PropertyStoredIdentPostfixLabel.Top,80,Height);
    Visible:=true;
  end;

  PrivatVariablePrefixLabel:=TLabel.Create(Self);
  with PrivatVariablePrefixLabel do begin
    Name:='PrivatVariablePrefixLabel';
    Parent:=PropertyCompletionGroupBox;
    SetBounds((PropertyCompletionGroupBox.ClientWidth-20) div 2,
      PropertyReadIdentPrefixLabel.Top,120,Height);
    Caption:=dlgCDTVariablePrefix ;
    Visible:=true;
  end;

  PrivatVariablePrefixEdit:=TEdit.Create(Self);
  with PrivatVariablePrefixEdit do begin
    Name:='PrivatVariablePrefixEdit';
    Parent:=PropertyCompletionGroupBox;
    SetBounds(PrivatVariablePrefixLabel.Left+150,PrivatVariablePrefixLabel.Top,
      80,Height);
    Visible:=true;
  end;

  SetPropertyVariablenameLabel:=TLabel.Create(Self);
  with SetPropertyVariablenameLabel do begin
    Name:='SetPropertyVariablenameLabel';
    Parent:=PropertyCompletionGroupBox;
    SetBounds(PrivatVariablePrefixLabel.Left,
      PrivatVariablePrefixLabel.Top+PrivatVariablePrefixLabel.Height+5,
      120,Height);
    Caption:=dlgSetPropertyVariable ;
    Visible:=true;
  end;

  SetPropertyVariablenameEdit:=TEdit.Create(Self);
  with SetPropertyVariablenameEdit do begin
    Name:='SetPropertyVariablenameEdit';
    Parent:=PropertyCompletionGroupBox;
    SetBounds(PrivatVariablePrefixEdit.Left,
      PrivatVariablePrefixLabel.Top+PrivatVariablePrefixLabel.Height+5,
      80,Height);
    Visible:=true;
  end;
end;

procedure TCodeToolsOptsDlg.SetupWordsPage(PageID: integer);
begin
  KeyWordPolicyRadioGroup:=TRadioGroup.Create(Self);
  with KeyWordPolicyRadioGroup do begin
    Name:='KeyWordPolicyRadioGroup';
    Parent:=NoteBook.Page[PageID];
    SetBounds(8,6,
      (Self.ClientWidth div 2)-12,120);
    Caption:=dlgKeywordPolicy ;
    with Items do begin
      BeginUpdate;
      Add(dlgEnvNone);
      Add(dlgCDTLower);
      Add(dlgCDTUPPERCASE);
      Add(dlg1UP2low);
      EndUpdate;
    end;
    OnClick:=@UpdateExamples;
    Visible:=true;
  end;

  IdentifierPolicyRadioGroup:=TRadioGroup.Create(Self);
  with IdentifierPolicyRadioGroup do begin
    Name:='IdentifierPolicyRadioGroup';
    Parent:=NoteBook.Page[PageID];
    SetBounds(KeyWordPolicyRadioGroup.Left+KeyWordPolicyRadioGroup.Width+8,
      KeyWordPolicyRadioGroup.Top,
      KeyWordPolicyRadioGroup.Width,KeyWordPolicyRadioGroup.Height);
    Caption:=dlgIdentifierPolicy;
    with Items do begin
      BeginUpdate;
      Add(dlgEnvNone);
      Add(dlgCDTLower);
      Add(dlgCDTUPPERCASE);
      Add(dlg1UP2low);
      EndUpdate;
    end;
    OnClick:=@UpdateExamples;
    Visible:=true;
  end;
end;

procedure TCodeToolsOptsDlg.SetupLineSplittingPage(PageID: integer);
begin
  LineLengthLabel:=TLabel.Create(Self);
  with LineLengthLabel do begin
    Name:='LineLengthLabel';
    Parent:=NoteBook.Page[PageID];
    SetBounds(8,7,Canvas.TextWidth('Max line length: '),Height);
    Caption:=dlgMaxLineLength ;
    Visible:=true;
  end;

  LineLengthEdit:=TEdit.Create(Self);
  with LineLengthEdit do begin
    Name:='LineLengthEdit';
    Parent:=LineLengthLabel.Parent;
    Left:=LineLengthLabel.Left+LineLengthLabel.Width+5;
    Top:=LineLengthLabel.Top-2;
    Width:=50;
    OnChange:=@UpdateExamples;
    Visible:=true;
  end;

  DoNotSplitLineInFrontGroupBox:=TGroupBox.Create(Self);
  with DoNotSplitLineInFrontGroupBox do begin
    Name:='DoNotSplitLineInFrontGroupBox';
    Parent:=NoteBook.Page[PageID];
    SetBounds(6,LineLengthLabel.Top+LineLengthLabel.Height+7,
      (Self.ClientWidth-24) div 2,150);
    Caption:=dlgNotSplitLineFront ;
    CreateAtomCheckBoxes(DoNotSplitLineInFrontGroupBox,DoNotSplitAtoms,2);
    OnClick:=@UpdateExamples;
    Visible:=true;
  end;

  DoNotSplitLineAfterGroupBox:=TGroupBox.Create(Self);
  with DoNotSplitLineAfterGroupBox do begin
    Name:='DoNotSplitLineAfterGroupBox';
    Parent:=NoteBook.Page[PageID];
    SetBounds(DoNotSplitLineInFrontGroupBox.Left,
      DoNotSplitLineInFrontGroupBox.Top+DoNotSplitLineInFrontGroupBox.Height+7,
      DoNotSplitLineInFrontGroupBox.Width,
      DoNotSplitLineInFrontGroupBox.Height);
    Caption:=dlgNotSplitLineAfter ;
    CreateAtomCheckBoxes(DoNotSplitLineAfterGroupBox,DoNotSplitAtoms,2);
    OnClick:=@UpdateExamples;
    Visible:=true;
  end;
  
  SplitPreviewGroupBox:=TGroupBox.Create(Self);
  with SplitPreviewGroupBox do begin
    Name:='SplitPreviewGroupBox';
    Parent:=NoteBook.Page[PageID];
    Left:=DoNotSplitLineInFrontGroupBox.Left
          +DoNotSplitLineInFrontGroupBox.Width+8;
    Top:=LineLengthLabel.Top;
    Width:=Self.ClientWidth-10-Left;
    Height:=Self.ClientHeight-92-Top;
    Caption:=dlgCDTPreview;
    Visible:=true;
  end;
  
  SplitPreviewSynEdit:=TSynEdit.Create(Self);
  with SplitPreviewSynEdit do begin
    Name:='SplitPreviewSynEdit';
    Parent:=SplitPreviewGroupBox;
    Align:=alClient;
    Visible:=true;
  end;
end;

procedure TCodeToolsOptsDlg.SetupSpacePage(PageID: integer);
begin
  DoInsertSpaceInFrontGroupBox:=TGroupBox.Create(Self);
  with DoInsertSpaceInFrontGroupBox do begin
    Name:='DoInsertSpaceInFrontGroupBox';
    Parent:=NoteBook.Page[PageID];
    SetBounds(6,6,
      (Self.ClientWidth-24) div 2,150);
    Caption:=dlgInsSpaceFront ;
    CreateAtomCheckBoxes(DoInsertSpaceInFrontGroupBox,DoInsertSpaceAtoms,2);
    OnClick:=@UpdateExamples;
    Visible:=true;
  end;

  DoInsertSpaceAfterGroupBox:=TGroupBox.Create(Self);
  with DoInsertSpaceAfterGroupBox do begin
    Name:='DoInsertSpaceAfterGroupBox';
    Parent:=NoteBook.Page[PageID];
    SetBounds(DoInsertSpaceInFrontGroupBox.Left
      +DoInsertSpaceInFrontGroupBox.Width+8,
      DoInsertSpaceInFrontGroupBox.Top,
      DoInsertSpaceInFrontGroupBox.Width,
      DoInsertSpaceInFrontGroupBox.Height);
    Caption:=dlgInsSpaceAfter ;
    CreateAtomCheckBoxes(DoInsertSpaceAfterGroupBox,DoInsertSpaceAtoms,2);
    OnClick:=@UpdateExamples;
    Visible:=true;
  end;
  
  SpacePreviewGroupBox:=TGroupBox.Create(Self);
  with SpacePreviewGroupBox do begin
    Name:='SpacePreviewGroupBox';
    Parent:=NoteBook.Page[PageID];
    Left:=DoInsertSpaceInFrontGroupBox.Left;
    Top:=DoInsertSpaceInFrontGroupBox.Top+DoInsertSpaceInFrontGroupBox.Height+7;
    Width:=Self.ClientWidth-10-Left;
    Height:=Self.ClientHeight-92-Top;
    Caption:=dlgWRDPreview ;
    Visible:=true;
  end;

  SpacePreviewSynEdit:=TSynEdit.Create(Self);
  with SpacePreviewSynEdit do begin
    Name:='SpacePreviewSynEdit';
    Parent:=SpacePreviewGroupBox;
    Align:=alClient;
    Visible:=true;
  end;
end;

procedure TCodeToolsOptsDlg.ResizeGeneralPage;
begin
  with SrcPathGroupBox do begin
    SetBounds(8,7,Self.ClientWidth-20,51);
  end;

  with SrcPathEdit do begin
    SetBounds(5,6,Parent.ClientWidth-14,Height);
  end;

  with JumpingGroupBox do begin
    SetBounds(8,SrcPathGroupBox.Top+SrcPathGroupBox.Height+7,
      SrcPathGroupBox.Width,95);
  end;

  with AdjustTopLineDueToCommentCheckBox do begin
    SetBounds(5,6,Parent.ClientWidth-10,Height);
  end;

  with JumpCenteredCheckBox do begin
    SetBounds(AdjustTopLineDueToCommentCheckBox.Left,
      AdjustTopLineDueToCommentCheckBox.Top+2
      +AdjustTopLineDueToCommentCheckBox.Height,
      AdjustTopLineDueToCommentCheckBox.Width,Height);
  end;

  with CursorBeyondEOLCheckBox do begin
    SetBounds(JumpCenteredCheckBox.Left,
      JumpCenteredCheckBox.Top+JumpCenteredCheckBox.Height+2,
      JumpCenteredCheckBox.Width,Height);
  end;
end;

procedure TCodeToolsOptsDlg.ResizeCodeCreationPage;
begin
  with ClassPartInsertPolicyRadioGroup do begin
    SetBounds(8,6,(Self.ClientWidth div 2)-12,70);
  end;

  with MixMethodsAndPorpertiesCheckBox do begin
    SetBounds(ClassPartInsertPolicyRadioGroup.Left,
       ClassPartInsertPolicyRadioGroup.Top
         +ClassPartInsertPolicyRadioGroup.Height+5,
       ClassPartInsertPolicyRadioGroup.Width,Height);
  end;

  with MethodInsertPolicyRadioGroup do begin
    SetBounds(ClassPartInsertPolicyRadioGroup.Left,
      MixMethodsAndPorpertiesCheckBox.Top
        +MixMethodsAndPorpertiesCheckBox.Height+10,
      ClassPartInsertPolicyRadioGroup.Width,
      100);
  end;

  with ForwardProcsInsertPolicyRadioGroup do begin
    SetBounds(ClassPartInsertPolicyRadioGroup.Left
       +ClassPartInsertPolicyRadioGroup.Width+8,
       ClassPartInsertPolicyRadioGroup.Top,
       ClassPartInsertPolicyRadioGroup.Width,100);
  end;

  with ForwardProcsKeepOrderCheckBox do begin
    SetBounds(ForwardProcsInsertPolicyRadioGroup.Left,
       ForwardProcsInsertPolicyRadioGroup.Top
         +ForwardProcsInsertPolicyRadioGroup.Height+5,
       ForwardProcsInsertPolicyRadioGroup.Width,Height);
  end;

  with PropertyCompletionGroupBox do begin
    SetBounds(ClassPartInsertPolicyRadioGroup.Left,
      MethodInsertPolicyRadioGroup.Top+MethodInsertPolicyRadioGroup.Height+7,
      Self.ClientWidth-20,125);
  end;

  with PropertyCompletionCheckBox do begin
    SetBounds(6,5,200,Height);
  end;

  with PropertyReadIdentPrefixLabel do begin
    SetBounds(PropertyCompletionCheckBox.Left,
      PropertyCompletionCheckBox.Top+PropertyCompletionCheckBox.Height+5,
      100,Height);
  end;

  with PropertyReadIdentPrefixEdit do begin
    SetBounds(110,PropertyReadIdentPrefixLabel.Top,80,Height);
  end;

  with PropertyWriteIdentPrefixLabel do begin
    SetBounds(6,PropertyReadIdentPrefixLabel.Top
      +PropertyReadIdentPrefixLabel.Height+5,
      PropertyReadIdentPrefixLabel.Width,Height);
  end;

  with PropertyWriteIdentPrefixEdit do begin
    SetBounds(PropertyReadIdentPrefixEdit.Left,
      PropertyWriteIdentPrefixLabel.Top,80,Height);
  end;

  with PropertyStoredIdentPostfixLabel do begin
    SetBounds(6,PropertyWriteIdentPrefixLabel.Top
      +PropertyWriteIdentPrefixLabel.Height+5,
      PropertyReadIdentPrefixLabel.Width,Height);
  end;

  with PropertyStoredIdentPostfixEdit do begin
    SetBounds(PropertyReadIdentPrefixEdit.Left,
      PropertyStoredIdentPostfixLabel.Top,80,Height);
  end;

  with PrivatVariablePrefixLabel do begin
    SetBounds((PropertyCompletionGroupBox.ClientWidth-20) div 2,
      PropertyReadIdentPrefixLabel.Top,120,Height);
  end;

  with PrivatVariablePrefixEdit do begin
    SetBounds(PrivatVariablePrefixLabel.Left+150,PrivatVariablePrefixLabel.Top,
      80,Height);
  end;

  with SetPropertyVariablenameLabel do begin
    SetBounds(PrivatVariablePrefixLabel.Left,
      PrivatVariablePrefixLabel.Top+PrivatVariablePrefixLabel.Height+5,
      120,Height);
  end;

  with SetPropertyVariablenameEdit do begin
    SetBounds(PrivatVariablePrefixEdit.Left,
      PrivatVariablePrefixLabel.Top+PrivatVariablePrefixLabel.Height+5,
      80,Height);
  end;
end;

procedure TCodeToolsOptsDlg.ResizeWordsPage;
begin
  with KeyWordPolicyRadioGroup do begin
    SetBounds(8,6,(Self.ClientWidth div 2)-12,120);
  end;

  with IdentifierPolicyRadioGroup do begin
    SetBounds(KeyWordPolicyRadioGroup.Left+KeyWordPolicyRadioGroup.Width+8,
      KeyWordPolicyRadioGroup.Top,
      KeyWordPolicyRadioGroup.Width,KeyWordPolicyRadioGroup.Height);
  end;
end;

procedure TCodeToolsOptsDlg.ResizeLineSplittingPage;
begin
  with LineLengthLabel do begin
    SetBounds(8,7,Canvas.TextWidth('Max line length: '),Height);
  end;

  with LineLengthEdit do begin
    Left:=LineLengthLabel.Left+LineLengthLabel.Width+5;
    Top:=LineLengthLabel.Top-2;
    Width:=50;
  end;

  with DoNotSplitLineInFrontGroupBox do begin
    SetBounds(6,LineLengthLabel.Top+LineLengthLabel.Height+7,
      (Self.ClientWidth-24) div 2,150);
  end;

  with DoNotSplitLineAfterGroupBox do begin
    SetBounds(DoNotSplitLineInFrontGroupBox.Left,
      DoNotSplitLineInFrontGroupBox.Top+DoNotSplitLineInFrontGroupBox.Height+7,
      DoNotSplitLineInFrontGroupBox.Width,
      DoNotSplitLineInFrontGroupBox.Height);
  end;

  with SplitPreviewGroupBox do begin
    Left:=DoNotSplitLineInFrontGroupBox.Left
          +DoNotSplitLineInFrontGroupBox.Width+8;
    Top:=LineLengthLabel.Top;
    Width:=Self.ClientWidth-10-Left;
    Height:=Self.ClientHeight-92-Top;
  end;

  with SplitPreviewSynEdit do begin
    SetBounds(2,2,Parent.ClientWidth-8,Parent.ClientHeight-25);
  end;
end;

procedure TCodeToolsOptsDlg.ResizeSpacePage;
begin
  with DoInsertSpaceInFrontGroupBox do begin
    SetBounds(6,6,
      (Self.ClientWidth-24) div 2,150);
  end;

  with DoInsertSpaceAfterGroupBox do begin
    SetBounds(DoInsertSpaceInFrontGroupBox.Left
      +DoInsertSpaceInFrontGroupBox.Width+8,
      DoInsertSpaceInFrontGroupBox.Top,
      DoInsertSpaceInFrontGroupBox.Width,
      DoInsertSpaceInFrontGroupBox.Height);
  end;

  with SpacePreviewGroupBox do begin
    Left:=DoInsertSpaceInFrontGroupBox.Left;
    Top:=DoInsertSpaceInFrontGroupBox.Top+DoInsertSpaceInFrontGroupBox.Height+7;
    Width:=Self.ClientWidth-10-Left;
    Height:=Self.ClientHeight-92-Top;
  end;

  with SpacePreviewSynEdit do begin
    SetBounds(2,2,Parent.ClientWidth-8,Parent.ClientHeight-25);
  end;
end;

procedure TCodeToolsOptsDlg.CodeToolsOptsDlgResize(Sender: TObject);
begin
  with NoteBook do begin
    SetBounds(0,0,Self.ClientWidth,Self.ClientHeight-50);
  end;

  ResizeGeneralPage;
  ResizeCodeCreationPage;
  ResizeWordsPage;
  ResizeLineSplittingPage;
  ResizeSpacePage;

  with CancelButton do begin
    Width:=70;
    Height:=23;
    Left:=Self.ClientWidth-Width-15;
    Top:=Self.ClientHeight-Height-15;
  end;

  with OkButton do begin
    Width:=CancelButton.Width;
    Height:=CancelButton.Height;
    Left:=CancelButton.Left-15-Width;
    Top:=CancelButton.Top;
  end;
end;

procedure TCodeToolsOptsDlg.CreateAtomCheckBoxes(ParentGroupBox: TGroupBox;
  AtomTypes: TAtomTypes; Columns: integer);
var
  Count, i, yi, MaxYCount: integer;
  a: TAtomType;
  X, Y, CurX, CurY, XStep, YStep: integer;
  NewCheckBox: TCheckBox;
begin
  if Columns<1 then Columns:=1;
  Count:=0;
  for a:=Low(TAtomTypes) to High(TAtomTypes) do begin
    if a in AtomTypes then inc(Count);
  end;
  if Count=0 then exit;
  MaxYCount:=((Count+Columns-1) div Columns);
  X:=6;
  Y:=1;
  XStep:=((ParentGroupBox.ClientWidth-10) div Columns);
  YStep:=((ParentGroupBox.ClientHeight-20) div MaxYCount);
  CurX:=X;
  CurY:=Y;
  i:=0;
  yi:=0;
  for a:=Low(TAtomTypes) to High(TAtomTypes) do begin
    if a in AtomTypes then begin
      inc(i);
      inc(yi);
      NewCheckBox:=TCheckBox.Create(ParentGroupBox);
      with NewCheckBox do begin
        Name:=ParentGroupBox.Name+'CheckBox'+IntToStr(i+1);
        Parent:=ParentGroupBox;
        SetBounds(CurX,CurY,XStep-10,Height);
        Caption:=AtomTypeDescriptions[a];
        OnClick:=@UpdateExamples;
        Visible:=true;
      end;
      if yi>=MaxYCount then begin
        inc(X,XStep);
        CurX:=X;
        CurY:=Y;
        yi:=0;
      end else begin
        inc(CurY,YStep);
      end;
    end;
  end;
end;

procedure TCodeToolsOptsDlg.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TCodeToolsOptsDlg.OkButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TCodeToolsOptsDlg.ReadSettings(Options: TCodeToolsOptions);
begin
  // General - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  SrcPathEdit.Text:=Options.SrcPath;
  AdjustTopLineDueToCommentCheckBox.Checked:=Options.AdjustTopLineDueToComment;
  JumpCenteredCheckBox.Checked:=Options.JumpCentered;
  CursorBeyondEOLCheckBox.Checked:=Options.CursorBeyondEOL;

  // CodeCreation  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  LineLengthEdit.Text:=IntToStr(Options.LineLength);
  case Options.ClassPartInsertPolicy of
  cpipAlphabetically:
    ClassPartInsertPolicyRadioGroup.ItemIndex:=0;
  else
    // cpipLast
    ClassPartInsertPolicyRadioGroup.ItemIndex:=1;
  end;
  MixMethodsAndPorpertiesCheckBox.Checked:=Options.MixMethodsAndPorperties;
  case Options.ForwardProcInsertPolicy of
  fpipLast: ForwardProcsInsertPolicyRadioGroup.ItemIndex:=0;
  fpipInFrontOfMethods: ForwardProcsInsertPolicyRadioGroup.ItemIndex:=1;
  else
    // fpipBehindMethods
    ForwardProcsInsertPolicyRadioGroup.ItemIndex:=2;
  end;
  ForwardProcsKeepOrderCheckBox.Checked:=Options.KeepForwardProcOrder;
  case Options.MethodInsertPolicy of
  mipAlphabetically:
    MethodInsertPolicyRadioGroup.ItemIndex:=0;
  mipLast:
    MethodInsertPolicyRadioGroup.ItemIndex:=1;
  else
    // mipClassOrder
    MethodInsertPolicyRadioGroup.ItemIndex:=2;
  end;
  case Options.KeyWordPolicy of
  wpLowerCase:
    KeyWordPolicyRadioGroup.ItemIndex:=1;
  wpUpperCase:
    KeyWordPolicyRadioGroup.ItemIndex:=2;
  wpLowerCaseFirstLetterUp:
    KeyWordPolicyRadioGroup.ItemIndex:=3;
  else
    // wpNone
    KeyWordPolicyRadioGroup.ItemIndex:=0;
  end;
  case Options.IdentifierPolicy of
  wpLowerCase:
    IdentifierPolicyRadioGroup.ItemIndex:=1;
  wpUpperCase:
    IdentifierPolicyRadioGroup.ItemIndex:=2;
  wpLowerCaseFirstLetterUp:
    IdentifierPolicyRadioGroup.ItemIndex:=3;
  else
    // wpNone
    IdentifierPolicyRadioGroup.ItemIndex:=0;
  end;
  SetAtomCheckBoxes(Options.DoNotSplitLineInFront,DoNotSplitLineInFrontGroupBox);
  SetAtomCheckBoxes(Options.DoNotSplitLineAfter,DoNotSplitLineAfterGroupBox);
  SetAtomCheckBoxes(Options.DoInsertSpaceInFront,DoInsertSpaceInFrontGroupBox);
  SetAtomCheckBoxes(Options.DoInsertSpaceAfter,DoInsertSpaceAfterGroupBox);
  PropertyCompletionCheckBox.Checked:=Options.CompleteProperties;
  PropertyReadIdentPrefixEdit.Text:=Options.PropertyReadIdentPrefix;
  PropertyWriteIdentPrefixEdit.Text:=Options.PropertyWriteIdentPrefix;
  PropertyStoredIdentPostfixEdit.Text:=Options.PropertyStoredIdentPostfix;
  PrivatVariablePrefixEdit.Text:=Options.PrivatVariablePrefix;
  SetPropertyVariablenameEdit.Text:=Options.SetPropertyVariablename;
end;

procedure TCodeToolsOptsDlg.WriteSettings(Options: TCodeToolsOptions);
begin
  // General - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Options.SrcPath:=SrcPathEdit.Text;
  Options.AdjustTopLineDueToComment:=AdjustTopLineDueToCommentCheckBox.Checked;
  Options.JumpCentered:=JumpCenteredCheckBox.Checked;
  Options.CursorBeyondEOL:=CursorBeyondEOLCheckBox.Checked;

  // CodeCreation  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  Options.LineLength:=StrToIntDef(LineLengthEdit.Text,80);
  if Options.LineLength<5 then
    Options.LineLength:=5;
  case ClassPartInsertPolicyRadioGroup.ItemIndex of
  0: Options.ClassPartInsertPolicy:=cpipAlphabetically;
  1: Options.ClassPartInsertPolicy:=cpipLast;
  end;
  Options.MixMethodsAndPorperties:=MixMethodsAndPorpertiesCheckBox.Checked;
  case ForwardProcsInsertPolicyRadioGroup.ItemIndex of
  0: Options.ForwardProcInsertPolicy:=fpipLast;
  1: Options.ForwardProcInsertPolicy:=fpipInFrontOfMethods;
  2: Options.ForwardProcInsertPolicy:=fpipBehindMethods;
  end;
  Options.KeepForwardProcOrder:=ForwardProcsKeepOrderCheckBox.Checked;
  case MethodInsertPolicyRadioGroup.ItemIndex of
  0: Options.MethodInsertPolicy:=mipAlphabetically;
  1: Options.MethodInsertPolicy:=mipLast;
  2: Options.MethodInsertPolicy:=mipClassOrder;
  end;
  case KeyWordPolicyRadioGroup.ItemIndex of
  0: Options.KeyWordPolicy:=wpNone;
  1: Options.KeyWordPolicy:=wpLowerCase;
  2: Options.KeyWordPolicy:=wpUpperCase;
  3: Options.KeyWordPolicy:=wpLowerCaseFirstLetterUp;
  end;
  case IdentifierPolicyRadioGroup.ItemIndex of
  0: Options.IdentifierPolicy:=wpNone;
  1: Options.IdentifierPolicy:=wpLowerCase;
  2: Options.IdentifierPolicy:=wpUpperCase;
  3: Options.IdentifierPolicy:=wpLowerCaseFirstLetterUp;
  end;
  Options.DoNotSplitLineInFront:=ReadAtomCheckBoxes(DoNotSplitLineInFrontGroupBox);
  Options.DoNotSplitLineAfter:=ReadAtomCheckBoxes(DoNotSplitLineAfterGroupBox);
  Options.DoInsertSpaceInFront:=ReadAtomCheckBoxes(DoInsertSpaceInFrontGroupBox);
  Options.DoInsertSpaceAfter:=ReadAtomCheckBoxes(DoInsertSpaceAfterGroupBox);
  Options.CompleteProperties:=PropertyCompletionCheckBox.Checked;
  Options.PropertyReadIdentPrefix:=
    ReadIdentifier(PropertyReadIdentPrefixEdit.Text,'Get');
  Options.PropertyWriteIdentPrefix:=
    ReadIdentifier(PropertyWriteIdentPrefixEdit.Text,'Set');
  Options.PropertyStoredIdentPostfix:=
    ReadIdentifier(PropertyStoredIdentPostfixEdit.Text,'IsStored');
  Options.PrivatVariablePrefix:=
    ReadIdentifier(PrivatVariablePrefixEdit.Text,'F');
  Options.SetPropertyVariablename:=
    ReadIdentifier(SetPropertyVariablenameEdit.Text,'AValue');
end;

procedure TCodeToolsOptsDlg.SetAtomCheckBoxes(AtomTypes: TAtomTypes;
  ParentGroupBox: TGroupBox);
var
  i: integer;
  ACheckBox: TCheckBox;
  a: TAtomType;
begin
  for i:=0 to ParentGroupBox.ComponentCount-1 do begin
    if (ParentGroupBox.Components[i] is TCheckBox) then begin
      ACheckBox:=TCheckBox(ParentGroupBox.Components[i]);
      a:=AtomTypeDescriptionToType(ACheckBox.Caption);
      ACheckBox.Checked:=(a<>atNone) and (a in AtomTypes);
    end;
  end;
end;

function TCodeToolsOptsDlg.ReadAtomCheckBoxes(
  ParentGroupBox: TGroupBox): TAtomTypes;
var
  i: integer;
  ACheckBox: TCheckBox;
  a: TAtomType;
begin
  Result:=[];
  for i:=0 to ParentGroupBox.ComponentCount-1 do begin
    if (ParentGroupBox.Components[i] is TCheckBox) then begin
      ACheckBox:=TCheckBox(ParentGroupBox.Components[i]);
      a:=AtomTypeDescriptionToType(ACheckBox.Caption);
      if (a<>atNone) and (ACheckBox.Checked) then
        Include(Result,a);
    end;
  end;
end;

procedure TCodeToolsOptsDlg.UpdatePreviewSettings;
begin
  UpdateSinglePreviewSettings(SplitPreviewSynEdit);
  UpdateSinglePreviewSettings(SpacePreviewSynEdit);
end;

procedure TCodeToolsOptsDlg.UpdateSinglePreviewSettings(APreview: TSynEdit);
begin
  if Assigned(FOnGetSynEditSettings) then begin
    FOnGetSynEditSettings(APreview);
  end;
  APreview.Gutter.Visible:=false;
  APreview.Options:=APreview.Options+[eoNoCaret, eoNoSelection];
  APreview.ReadOnly:=true;
end;

procedure TCodeToolsOptsDlg.UpdateSplitLineExample;
begin
  if BeautifyCodeOptions=nil then exit;
  WriteBeautifyCodeOptions(BeautifyCodeOptions);
  BeautifyCodeOptions.LineLength:=1;
  SplitPreviewSynEdit.Text:=BeautifyCodeOptions.BeautifyStatement(
    LineSplitExampleText,0);
end;

procedure TCodeToolsOptsDlg.WriteBeautifyCodeOptions(
  Options: TBeautifyCodeOptions);
begin
  Options.LineLength:=StrToIntDef(LineLengthEdit.Text,80);
  if Options.LineLength<5 then
    Options.LineLength:=5;
  case ClassPartInsertPolicyRadioGroup.ItemIndex of
  0: Options.ClassPartInsertPolicy:=cpipAlphabetically;
  1: Options.ClassPartInsertPolicy:=cpipLast;
  end;
  Options.MixMethodsAndPorperties:=MixMethodsAndPorpertiesCheckBox.Checked;
  case ForwardProcsInsertPolicyRadioGroup.ItemIndex of
  0: Options.ForwardProcInsertPolicy:=fpipLast;
  1: Options.ForwardProcInsertPolicy:=fpipInFrontOfMethods;
  2: Options.ForwardProcInsertPolicy:=fpipBehindMethods;
  end;
  Options.KeepForwardProcOrder:=ForwardProcsKeepOrderCheckBox.Checked;
  case MethodInsertPolicyRadioGroup.ItemIndex of
  0: Options.MethodInsertPolicy:=mipAlphabetically;
  1: Options.MethodInsertPolicy:=mipLast;
  2: Options.MethodInsertPolicy:=mipClassOrder;
  end;
  case KeyWordPolicyRadioGroup.ItemIndex of
  0: Options.KeyWordPolicy:=wpNone;
  1: Options.KeyWordPolicy:=wpLowerCase;
  2: Options.KeyWordPolicy:=wpUpperCase;
  3: Options.KeyWordPolicy:=wpLowerCaseFirstLetterUp;
  end;
  case IdentifierPolicyRadioGroup.ItemIndex of
  0: Options.IdentifierPolicy:=wpNone;
  1: Options.IdentifierPolicy:=wpLowerCase;
  2: Options.IdentifierPolicy:=wpUpperCase;
  3: Options.IdentifierPolicy:=wpLowerCaseFirstLetterUp;
  end;
  Options.DoNotSplitLineInFront:=ReadAtomCheckBoxes(DoNotSplitLineInFrontGroupBox);
  Options.DoNotSplitLineAfter:=ReadAtomCheckBoxes(DoNotSplitLineAfterGroupBox);
  Options.DoInsertSpaceInFront:=ReadAtomCheckBoxes(DoInsertSpaceInFrontGroupBox);
  Options.DoInsertSpaceAfter:=ReadAtomCheckBoxes(DoInsertSpaceAfterGroupBox);
  Options.PropertyReadIdentPrefix:=
    ReadIdentifier(PropertyReadIdentPrefixEdit.Text,'Get');
  Options.PropertyWriteIdentPrefix:=
    ReadIdentifier(PropertyWriteIdentPrefixEdit.Text,'Set');
  Options.PropertyStoredIdentPostfix:=
    ReadIdentifier(PropertyStoredIdentPostfixEdit.Text,'IsStored');
  Options.PrivatVariablePrefix:=
    ReadIdentifier(PrivatVariablePrefixEdit.Text,'F');
end;

procedure TCodeToolsOptsDlg.UpdateExamples(Sender: TObject);
begin
  if Sender=nil then exit;
  UpdateSplitLineExample;
  UpdateSpaceExample;
end;

procedure TCodeToolsOptsDlg.UpdateSpaceExample;
begin
  if BeautifyCodeOptions=nil then exit;
  WriteBeautifyCodeOptions(BeautifyCodeOptions);
  BeautifyCodeOptions.LineLength:=40;
  SpacePreviewSynEdit.Text:=BeautifyCodeOptions.BeautifyStatement(
    SpaceExampleText,0);
end;

//------------------------------------------------------------------------------

function ShowCodeToolsOptions(Options: TCodeToolsOptions;
  OnGetSynEditSettings: TNotifyEvent): TModalResult;
var CodeToolsOptsDlg: TCodeToolsOptsDlg;
begin
  Result:=mrCancel;
  CodeToolsOptsDlg:=TCodeToolsOptsDlg.Create(Application);
  try
    CodeToolsOptsDlg.ReadSettings(Options);
    CodeToolsOptsDlg.OnGetSynEditSettings:=OnGetSynEditSettings;
    CodeToolsOptsDlg.UpdatePreviewSettings;
    Result:=CodeToolsOptsDlg.ShowModal;
    if Result=mrOk then begin
      CodeToolsOptsDlg.WriteSettings(Options);
      Options.AssignTo(CodeToolBoss);
      Options.Save;
    end;
  finally
    CodeToolsOptsDlg.Free;
  end;
end;

end.

