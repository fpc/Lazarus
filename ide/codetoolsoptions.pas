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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    - TCodeToolsOptions
}
unit CodeToolsOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazFileUtils, Laz2_XMLCfg, LazUTF8, LazUTF8Classes, LazFileCache, LazStringUtils,
  // LCL
  LCLProc, LCLType,
  // CodeTools
  CodeToolManager, DefineTemplates, SourceChanger,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, MacroIntf,
  // IDE
  LazConf, LazarusIDEStrConsts;

const
  DefaultIndentationFilename = 'laz_indentation.pas'; // in directory GetPrimaryConfigPath
  DefaultCodeCompletionFilename =
    '$(LazarusDir)'+PathDelim+'components'+PathDelim+'codetools'+PathDelim+'codecompletiontemplates.xml';

type

  TIdentComplIncludeWords = (icwDontInclude, icwIncludeFromCurrentUnit,
    icwIncludeFromAllUnits);

  { TCodeToolsOptions }

  TCodeToolsOptions = class(TAbstractIDEEnvironmentOptions)
  private
    FClassHeaderComments: boolean;
    FClassImplementationComments: boolean;
    FFilename: string;
    FIdentComplAddDo: Boolean;
    FIdentComplAddParameterBrackets: boolean;
    FIdentComplReplaceIdentifier: boolean;
    FIdentComplJumpToError: boolean;
    FIdentComplShowHelp: boolean;

    // General
    FAdjustTopLineDueToComment: boolean;
    FAvoidUnnecessaryJumps: boolean;
    FIdentComplSortForHistory: boolean;
    FIdentComplSortForScope: boolean;
    FJumpSingleLinePos: integer;
    FJumpCodeBlockPos: integer;
    FCursorBeyondEOL: boolean;
    FSkipForwardDeclarations: boolean;
    FJumpToMethodBody: boolean;
    
    // Define Templates
    FGlobalDefineTemplates: TDefineTemplate;
    FDefinesEditMainSplitterTop: integer;
    FDefinesPreviewMainSplitterPos: integer;
    FDefinesPreviewTemplSplitterPos: integer;

    // CodeCreation
    FAddInheritedCodeToOverrideMethod: boolean;
    FCompleteProperties: boolean;
    FLineLength: integer;
    FClassPartInsertPolicy: TClassPartInsertPolicy;
    FMixMethodsAndProperties: boolean;
    FForwardProcBodyInsertPolicy: TForwardProcBodyInsertPolicy;
    FKeepForwardProcOrder: boolean;
    FMethodInsertPolicy: TMethodInsertPolicy;
    FMethodDefaultSection: TInsertClassSection;
    FKeyWordPolicy : TWordPolicy;
    FIdentifierPolicy: TWordPolicy;
    FUpdateAllMethodSignatures: boolean;
    FUpdateMultiProcSignatures: boolean;
    FUpdateOtherProcSignaturesCase: boolean;
    FGroupLocalVariables: boolean;
    FOverrideStringTypesWithFirstParamType: Boolean;
    FWordPolicyExceptions: TStringList;
    FDoNotSplitLineInFront: TAtomTypes;
    FDoNotSplitLineAfter: TAtomTypes;
    FDoInsertSpaceInFront: TAtomTypes;
    FDoInsertSpaceAfter: TAtomTypes;
    FPropertyReadIdentPrefix: string;
    FPropertyWriteIdentPrefix: string;
    FPropertyStoredIdentPostfix: string;
    FPrivateVariablePrefix: string;
    FSetPropertyVariablename: string;
    FSetPropertyVariableIsPrefix: Boolean;
    FSetPropertyVariableUseConst: Boolean;
    FUsesInsertPolicy: TUsesInsertPolicy;

    // identifier completion
    FIdentComplAddSemicolon: Boolean;
    FIdentComplAddAssignOperator: Boolean;
    FIdentComplAutoStartAfterPoint: boolean;
    FIdentComplAutoUseSingleIdent: boolean;
    FIdentComplUseContainsFilter: Boolean;
    FIdentComplIncludeCodeTemplates: Boolean;
    FIdentComplIncludeWords: TIdentComplIncludeWords;
    FIdentComplShowIcons: Boolean;

    // auto indentation
    FIndentOnLineBreak: boolean;
    FIndentOnPaste: boolean;
    fIndentationFilename: String;
    FIndentContextSensitive: boolean;

    // code completion templates
    FCodeCompletionTemplateFileName : String;

    procedure SetCodeCompletionTemplateFileName(aValue: String);
    procedure SetFilename(const AValue: string);
    procedure SetSetPropertyVariablename(aValue: string);
    procedure SetSetPropertyVariableIsPrefix(aValue: Boolean);
    procedure SetSetPropertyVariableUseConst(aValue: Boolean);
  public
    class function GetGroupCaption:string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    procedure DoAfterWrite(Restore: boolean); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearGlobalDefineTemplates;
    procedure Load;
    procedure Save;
    procedure AssignGlobalDefineTemplatesToTree(Tree: TDefineTree);
    property Filename: string read FFilename write SetFilename;
    procedure SetLazarusDefaultFilename;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    function IsEqual(CodeToolsOpts: TCodeToolsOptions): boolean;
    function CreateCopy: TCodeToolsOptions;
    procedure ReadGlobalDefinesTemplatesFromTree(Tree: TDefineTree);
    procedure CreateDefaultIndentationFile;
    
    // General
    property AdjustTopLineDueToComment: boolean
      read FAdjustTopLineDueToComment write FAdjustTopLineDueToComment;
    property AvoidUnnecessaryJumps: boolean read FAvoidUnnecessaryJumps write FAvoidUnnecessaryJumps;
    property JumpSingleLinePos: integer read FJumpSingleLinePos write FJumpSingleLinePos;
    property JumpCodeBlockPos: integer read FJumpCodeBlockPos write FJumpCodeBlockPos;
    property CursorBeyondEOL: boolean
      read FCursorBeyondEOL write FCursorBeyondEOL;
    property SkipForwardDeclarations: boolean read FSkipForwardDeclarations write FSkipForwardDeclarations;
    property JumpToMethodBody: boolean read FJumpToMethodBody write FJumpToMethodBody;
    // Define Templates
    property GlobalDefineTemplates: TDefineTemplate read FGlobalDefineTemplates;
    property DefinesEditMainSplitterTop: integer read FDefinesEditMainSplitterTop
                                              write FDefinesEditMainSplitterTop;
    property DefinesPreviewMainSplitterPos: integer
       read FDefinesPreviewMainSplitterPos write FDefinesPreviewMainSplitterPos;
    property DefinesPreviewTemplSplitterPos: integer
       read FDefinesPreviewTemplSplitterPos write FDefinesPreviewTemplSplitterPos;

    // CodeCreation
    property CompleteProperties: boolean
      read FCompleteProperties write FCompleteProperties;
    property AddInheritedCodeToOverrideMethod: boolean
      read FAddInheritedCodeToOverrideMethod write FAddInheritedCodeToOverrideMethod;
    property LineLength: integer read FLineLength write FLineLength;
    property ClassPartInsertPolicy: TClassPartInsertPolicy
      read FClassPartInsertPolicy write FClassPartInsertPolicy;
    property MixMethodsAndProperties: boolean
      read FMixMethodsAndProperties write FMixMethodsAndProperties;
    property UpdateAllMethodSignatures: boolean
      read FUpdateAllMethodSignatures write FUpdateAllMethodSignatures;
    property ForwardProcBodyInsertPolicy: TForwardProcBodyInsertPolicy
      read FForwardProcBodyInsertPolicy write FForwardProcBodyInsertPolicy;
    property KeepForwardProcOrder: boolean
      read FKeepForwardProcOrder write FKeepForwardProcOrder;
    property UpdateMultiProcSignatures: boolean
      read FUpdateMultiProcSignatures write FUpdateMultiProcSignatures;
    property UpdateOtherProcSignaturesCase: boolean
      read FUpdateOtherProcSignaturesCase write FUpdateOtherProcSignaturesCase;
    property GroupLocalVariables: boolean
      read FGroupLocalVariables write FGroupLocalVariables;
    property OverrideStringTypesWithFirstParamType: Boolean
      read FOverrideStringTypesWithFirstParamType write FOverrideStringTypesWithFirstParamType;
    property ClassHeaderComments: boolean
      read FClassHeaderComments write FClassHeaderComments;
    property ClassImplementationComments: boolean
      read FClassImplementationComments write FClassImplementationComments;
    property MethodInsertPolicy: TMethodInsertPolicy
      read FMethodInsertPolicy write FMethodInsertPolicy;
    property MethodDefaultSection: TInsertClassSection
      read FMethodDefaultSection write FMethodDefaultSection;
    property KeyWordPolicy : TWordPolicy
      read FKeyWordPolicy write FKeyWordPolicy;
    property IdentifierPolicy: TWordPolicy
      read FIdentifierPolicy write FIdentifierPolicy;
    property WordPolicyExceptions: TStringList
      read FWordPolicyExceptions write FWordPolicyExceptions;
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
    property PrivateVariablePrefix: string
      read FPrivateVariablePrefix write FPrivateVariablePrefix;
    property SetPropertyVariablename: string
      read FSetPropertyVariablename write SetSetPropertyVariablename;
    property SetPropertyVariableIsPrefix: Boolean
      read FSetPropertyVariableIsPrefix write SetSetPropertyVariableIsPrefix;
    property SetPropertyVariableUseConst: Boolean
      read FSetPropertyVariableUseConst write SetSetPropertyVariableUseConst;
    property UsesInsertPolicy: TUsesInsertPolicy
      read FUsesInsertPolicy write FUsesInsertPolicy;

    // identifier completion
    property IdentComplAddSemicolon: Boolean read FIdentComplAddSemicolon
                                             write FIdentComplAddSemicolon;
    property IdentComplAddAssignOperator: Boolean read FIdentComplAddAssignOperator
                                             write FIdentComplAddAssignOperator;
    property IdentComplAddDo: Boolean read FIdentComplAddDo write FIdentComplAddDo;
    property IdentComplAutoStartAfterPoint: boolean read FIdentComplAutoStartAfterPoint
                                           write FIdentComplAutoStartAfterPoint;
    property IdentComplAutoUseSingleIdent: boolean read FIdentComplAutoUseSingleIdent
                                           write FIdentComplAutoUseSingleIdent;
    property IdentComplUseContainsFilter: boolean read FIdentComplUseContainsFilter
                                           write FIdentComplUseContainsFilter;
    property IdentComplIncludeCodeTemplates: boolean read FIdentComplIncludeCodeTemplates
                                           write FIdentComplIncludeCodeTemplates;
    property IdentComplIncludeWords: TIdentComplIncludeWords read FIdentComplIncludeWords
                                           write FIdentComplIncludeWords;
    property IdentComplShowIcons: boolean read FIdentComplShowIcons
                                           write FIdentComplShowIcons;
    property IdentComplAddParameterBrackets: boolean
      read FIdentComplAddParameterBrackets write FIdentComplAddParameterBrackets;
    property IdentComplReplaceIdentifier: boolean
      read FIdentComplReplaceIdentifier write FIdentComplReplaceIdentifier;
    property IdentComplJumpToError: boolean
      read FIdentComplJumpToError write FIdentComplJumpToError;
    property IdentComplShowHelp: boolean read FIdentComplShowHelp
                                         write FIdentComplShowHelp;
    property IdentComplSortForHistory: boolean read FIdentComplSortForHistory
                                             write FIdentComplSortForHistory;
    property IdentComplSortForScope: boolean read FIdentComplSortForScope
                                             write FIdentComplSortForScope;

    // indentation
    property IndentOnLineBreak: boolean read FIndentOnLineBreak
                                         write FIndentOnLineBreak;
    property IndentOnPaste: boolean read FIndentOnPaste write FIndentOnPaste;
    property IndentationFileName: String
      read fIndentationFileName write fIndentationFileName;
    property IndentContextSensitive: boolean read FIndentContextSensitive
      write FIndentContextSensitive;

    // code completion templates
    property CodeCompletionTemplateFileName : String read FCodeCompletionTemplateFileName
                                                     write SetCodeCompletionTemplateFileName;
  end;

var
  CodeToolsOpts: TCodeToolsOptions = nil;

function GetTranslatedAtomTypes(a: TAtomType): string;
function TranslatedAtomToType(const s: string): TAtomType;
function ReadIdentifier(const s, DefaultIdent: string): string;

const
  IdentComplIncludeWordsNames: array[TIdentComplIncludeWords] of shortstring = (
      'No', 'FromCurrentUnit', 'FromAllUnits'
    );
function IdentComplIncludeWordsNamesToEnum(const s: string): TIdentComplIncludeWords;

implementation

{$R lazarus_indentation.res}

const
  CodeToolsOptionsVersion = 2;
  DefaultCodeToolsOptsFile = 'codetoolsoptions.xml';
  
function IdentComplIncludeWordsNamesToEnum(const s: string): TIdentComplIncludeWords;
begin
  for Result:=Low(TIdentComplIncludeWords) to High(TIdentComplIncludeWords) do
    if SysUtils.CompareText(IdentComplIncludeWordsNames[Result],s)=0 then exit;
  Result:=icwDontInclude;
end;

function GetTranslatedAtomTypes(a: TAtomType): string;
begin
  case a of
  atNone: Result:=lisCodeToolsOptsNone;
  atKeyword: Result:=lisCodeToolsOptsKeyword;
  atIdentifier: Result:=lisCodeToolsOptsIdentifier;
  atColon: Result:=lisCodeToolsOptsColon;
  atSemicolon: Result:=lisCodeToolsOptsSemicolon;
  atComma: Result:=lisCodeToolsOptsComma;
  atPoint: Result:=lisCodeToolsOptsPoint;
  atAt: Result:=lisCodeToolsOptsAt;
  atNumber: Result:=lisCodeToolsOptsNumber;
  atStringConstant: Result:=lisCodeToolsOptsStringConst;
  atNewLine: Result:=lisCodeToolsOptsNewLine;
  atSpace: Result:=lisCodeToolsOptsSpace;
  atSymbol: Result:=lisCodeToolsOptsSymbol;
  atBracket: Result:=lisCodeToolsOptsBracket;
  atCaret: Result:=lisCodeToolsOptsCaret;
  else
    Result:='???';
  end;
end;

function TranslatedAtomToType(const s: string): TAtomType;
begin
  for Result:=Low(TAtomType) to High(TAtomType) do
    if s=GetTranslatedAtomTypes(Result) then exit;
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
  NewValues, DefaultValues: TAtomTypes);
var a: TAtomType;
begin
  for a:=Low(TAtomType) to High(TAtomType) do begin
    if (a<>atNone) then
      XMLConfig.SetDeleteValue(Path+AtomTypeNames[a]+'/Value',
                               a in NewValues,a in DefaultValues);
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

function ReadIdentifier(const s, DefaultIdent: string): string;
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
  FWordPolicyExceptions := TStringList.Create;
  Clear;
end;

destructor TCodeToolsOptions.Destroy;
begin
  ClearGlobalDefineTemplates;
  FWordPolicyExceptions.Free;
  inherited Destroy;
end;

class function TCodeToolsOptions.GetGroupCaption: string;
begin
  Result := dlgGroupCodetools;
end;

class function TCodeToolsOptions.GetInstance: TAbstractIDEOptions;
begin
  Result := CodeToolsOpts;
end;

procedure TCodeToolsOptions.DoAfterWrite(Restore: boolean);
begin
  inherited DoAfterWrite(Restore);
  if not Restore then
    Save;
end;

procedure TCodeToolsOptions.Load;
var
  XMLConfig: TXMLConfig;
  FileVersion: integer;
  AJumpCentered: Boolean;
  
  procedure LoadGlobalDefineTemplates;
  begin
    // delete old one
    ClearGlobalDefineTemplates;
    // create empty one
    FGlobalDefineTemplates:=TDefineTemplate.Create;
    FGlobalDefineTemplates.Name:='';
    // load
    FGlobalDefineTemplates.LoadFromXMLConfig(XMLConfig,'CodeToolsGlobalDefines/',
      true,true);
    // delete if still empty
    if FGlobalDefineTemplates.Name='' then begin
      ClearGlobalDefineTemplates;
    end else begin
      FGlobalDefineTemplates.SetDefineOwner(Self,true);
    end;
  end;
  
begin
  CreateDefaultIndentationFile;
  try
    XMLConfig:=TXMLConfig.Create(FFileName);
    FileVersion:=XMLConfig.GetValue('CodeToolsOptions/Version/Value',0);
    if (FileVersion<>0) and (FileVersion<CodeToolsOptionsVersion) then
      debugln(lisCompilerNOTELoadingOldCodetoolsOptionsFile, FFileName);

    // General
    FAdjustTopLineDueToComment:=XMLConfig.GetValue(
      'CodeToolsOptions/AdjustTopLineDueToComment/Value',true);
    if FileVersion<2 then
    begin
      AJumpCentered:=XMLConfig.GetValue('CodeToolsOptions/JumpCentered/Value',
        true);
      if AJumpCentered then
        FJumpSingleLinePos := 50
      else
        FJumpSingleLinePos := 0;
      FJumpCodeBlockPos := 0;
    end else
    begin
      FJumpSingleLinePos:=XMLConfig.GetValue('CodeToolsOptions/JumpSingleLinePos/Value', 50);
      FJumpCodeBlockPos:=XMLConfig.GetValue('CodeToolsOptions/JumpCodeBlockPos/Value', 0);
    end;
    FAvoidUnnecessaryJumps:=XMLConfig.GetValue('CodeToolsOptions/AvoidUnnecessaryJumps/Value', True);
    FCursorBeyondEOL:=XMLConfig.GetValue(
      'CodeToolsOptions/CursorBeyondEOL/Value',true);
    FSkipForwardDeclarations:=XMLConfig.GetValue(
      'CodeToolsOptions/SkipForwardDeclarations/Value',false);
    FJumpToMethodBody:=XMLConfig.GetValue(
      'CodeToolsOptions/JumpToMethodBody/Value',false);

    // Define templates
    LoadGlobalDefineTemplates;
    FDefinesEditMainSplitterTop:=XMLConfig.GetValue(
      'CodeToolsOptions/DefinesEditMainSplitter/Top',100);
    FDefinesPreviewMainSplitterPos:=XMLConfig.GetValue(
      'CodeToolsOptions/DefinesPreviewMainSplitter/Position',280);
    FDefinesPreviewTemplSplitterPos:=XMLConfig.GetValue(
      'CodeToolsOptions/DefinesPreviewTemplSplitter/Position',100);

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
    FMixMethodsAndProperties:=XMLConfig.GetValue(
      'CodeToolsOptions/MixMethodsAndProperties/Value',false);
    FUpdateAllMethodSignatures:=XMLConfig.GetValue(
      'CodeToolsOptions/UpdateAllMethodSignatures/Value',true);
    FForwardProcBodyInsertPolicy:=ForwardProcBodyInsertPolicyNameToPolicy(
      XMLConfig.GetValue('CodeToolsOptions/ForwardProcBodyInsertPolicy/Value',
        ForwardProcBodyInsertPolicyNames[fpipInFrontOfMethods]));
    FKeepForwardProcOrder:=XMLConfig.GetValue(
      'CodeToolsOptions/KeepForwardProcOrder/Value',true);
    FUpdateMultiProcSignatures:=XMLConfig.GetValue(
      'CodeToolsOptions/UpdateMultiProcSignatures/Value',true);
    FUpdateOtherProcSignaturesCase:=XMLConfig.GetValue(
      'CodeToolsOptions/UpdateOtherProcSignaturesCase/Value',true);
    FGroupLocalVariables:=XMLConfig.GetValue(
      'CodeToolsOptions/GroupLocalVariables/Value',true);
    FOverrideStringTypesWithFirstParamType:=XMLConfig.GetValue(
      'CodeToolsOptions/OverrideStringTypesWithFirstParamType/Value',true);
    FClassHeaderComments:=XMLConfig.GetValue(
      'CodeToolsOptions/ClassHeaderComments/Value',true);
    FClassImplementationComments:=XMLConfig.GetValue(
      'CodeToolsOptions/ClassImplementationComments/Value',true);

    FMethodInsertPolicy:=MethodInsertPolicyNameToPolicy(XMLConfig.GetValue(
      'CodeToolsOptions/MethodInsertPolicy/Value',
      MethodInsertPolicyNames[mipClassOrder]));
    FMethodDefaultSection:=InsertClassSectionNameToSection(XMLConfig.GetValue(
      'CodeToolsOptions/MethodDefaultSection/Value',
      InsertClassSectionNames[DefaultMethodDefaultSection]));
    FKeyWordPolicy:=WordPolicyNameToPolicy(XMLConfig.GetValue(
      'CodeToolsOptions/KeyWordPolicy/Value',
      WordPolicyNames[wpLowerCase]));
    FIdentifierPolicy:=WordPolicyNameToPolicy(XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierPolicy/Value',
      WordPolicyNames[wpNone]));
    WordPolicyExceptions.Text:=LineBreaksToSystemLineBreaks(XMLConfig.GetValue(
      'CodeToolsOptions/WordPolicyExceptions/Value', ''));
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
    FPrivateVariablePrefix:=ReadIdentifier(XMLConfig.GetValue(
      'CodeToolsOptions/PrivateVariablePrefix/Value',''),'F');
    FSetPropertyVariablename:=ReadIdentifier(XMLConfig.GetValue(
      'CodeToolsOptions/SetPropertyVariablename/Value',''),'AValue');
    FSetPropertyVariableIsPrefix:=XMLConfig.GetValue(
      'CodeToolsOptions/SetPropertyVariableIsPrefix/Value',false);
    FSetPropertyVariableUseConst:=XMLConfig.GetValue(
      'CodeToolsOptions/SetPropertyVariableUseConst/Value',false);
    FUsesInsertPolicy:=UsesInsertPolicyNameToPolicy(XMLConfig.GetValue(
      'CodeToolsOptions/UsesInsertPolicy/Value',
      UsesInsertPolicyNames[DefaultUsesInsertPolicy]));

    // identifier completion
    FIdentComplAddSemicolon:=XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierCompletion/AddSemicolon',true);
    FIdentComplAddAssignOperator:=XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierCompletion/AddAssignOperator',true);
    FIdentComplAddDo:=XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierCompletion/AddDo',true);
    FIdentComplAutoStartAfterPoint:=XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierCompletion/AutoStartAfterPoint',true);
    FIdentComplAutoUseSingleIdent:=XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierCompletion/AutoUseSingleIdent',true);
    FIdentComplUseContainsFilter:=XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierCompletion/UseContainsFilter',true);
    FIdentComplIncludeCodeTemplates:=XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierCompletion/IncludeCodeTemplates',true);
    FIdentComplIncludeWords:=IdentComplIncludeWordsNamesToEnum(XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierCompletion/IncludeWords',
      IdentComplIncludeWordsNames[icwIncludeFromAllUnits]));
    FIdentComplShowIcons:=XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierCompletion/ShowIcons',false);
    FIdentComplAddParameterBrackets:=XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierCompletion/AutoAddParameterBrackets',true);
    FIdentComplReplaceIdentifier:=XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierCompletion/ReplaceIdentifier',true);
    FIdentComplJumpToError:=XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierCompletion/JumpToError',true);
    FIdentComplShowHelp:=XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierCompletion/ShowHelp',false);
    FIdentComplSortForHistory:=XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierCompletion/SortForHistory',true);
    FIdentComplSortForScope:=XMLConfig.GetValue(
      'CodeToolsOptions/IdentifierCompletion/SortForScope',true);

    // indentation
    FIndentOnLineBreak :=
      XMLConfig.GetValue('CodeToolsOptions/Indentation/OnLineBreak/Enabled',true);
    FIndentOnPaste :=
      XMLConfig.GetValue('CodeToolsOptions/Indentation/OnPaste/Enabled',true);
    fIndentationFilename :=
      XMLConfig.GetValue('CodeToolsOptions/Indentation/FileName'
      , TrimFilename(AppendPathDelim(GetPrimaryConfigPath)+DefaultIndentationFilename));
    FIndentContextSensitive :=
      XMLConfig.GetValue('CodeToolsOptions/Indentation/ContextSensitive',true);

    // code completion templates
    FCodeCompletionTemplateFileName :=
      XMLConfig.GetValue('CodeToolsOptions/CodeCompletionTemplate/FileName'
      , DefaultCodeCompletionFilename);

    XMLConfig.Free;
  except
    on E: Exception do begin
      DebugLn('[TCodeToolsOptions.Load]  error reading "',FFilename,'": ',E.Message);
    end;
  end;
end;

procedure TCodeToolsOptions.Save;
var
  XMLConfig: TXMLConfig;
  
  procedure SaveGlobalDefineTemplates;
  begin
    if FGlobalDefineTemplates<>nil then
      FGlobalDefineTemplates.SaveToXMLConfig(XMLConfig,
        'CodeToolsGlobalDefines/',true,false,true,false);
  end;
  
begin
  try
    InvalidateFileStateCache;
    XMLConfig:=TXMLConfig.CreateClean(FFileName);
    XMLConfig.SetValue('CodeToolsOptions/Version/Value',CodeToolsOptionsVersion);

    // General
    XMLConfig.SetDeleteValue('CodeToolsOptions/AdjustTopLineDueToComment/Value',
                             FAdjustTopLineDueToComment,true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/JumpSingleLinePos/Value',
                             FJumpSingleLinePos,50);
    XMLConfig.SetDeleteValue('CodeToolsOptions/JumpCodeBlockPos/Value',
                             FJumpCodeBlockPos,0);
    XMLConfig.SetDeleteValue('CodeToolsOptions/AvoidUnnecessaryJumps/Value',
                             FAvoidUnnecessaryJumps,True);
    XMLConfig.SetDeleteValue('CodeToolsOptions/CursorBeyondEOL/Value',
                             FCursorBeyondEOL,true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/SkipForwardDeclarations/Value',
                             FSkipForwardDeclarations,false);
    XMLConfig.SetDeleteValue('CodeToolsOptions/JumpToMethodBody/Value',
                             FJumpToMethodBody,false);
    // Define templates
    SaveGlobalDefineTemplates;
    XMLConfig.SetDeleteValue('CodeToolsOptions/DefinesEditMainSplitter/Top',
                             FDefinesEditMainSplitterTop,100);
    XMLConfig.SetDeleteValue('CodeToolsOptions/DefinesPreviewMainSplitter/Position',
                             FDefinesPreviewMainSplitterPos,280);
    XMLConfig.SetDeleteValue('CodeToolsOptions/DefinesPreviewTemplSplitter/Position',
                             FDefinesPreviewTemplSplitterPos,100);

    // CodeCreation
    XMLConfig.SetDeleteValue(
      'CodeToolsOptions/AddInheritedCodeToOverrideMethod/Value',
      AddInheritedCodeToOverrideMethod,true);
    XMLConfig.SetDeleteValue(
      'CodeToolsOptions/CompleteProperties/Value',CompleteProperties,true);
    XMLConfig.SetDeleteValue(
      'CodeToolsOptions/LineLengthXMLConfig/Value',FLineLength,80);
    XMLConfig.SetDeleteValue('CodeToolsOptions/ClassPartInsertPolicy/Value',
      ClassPartInsertPolicyNames[FClassPartInsertPolicy],
      ClassPartInsertPolicyNames[cpipAlphabetically]);
    XMLConfig.SetDeleteValue(
      'CodeToolsOptions/MixMethodsAndProperties/Value',FMixMethodsAndProperties,
      false);
    XMLConfig.SetDeleteValue(
      'CodeToolsOptions/UpdateAllMethodSignatures/Value',FUpdateAllMethodSignatures,
      true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/ForwardProcBodyInsertPolicy/Value',
      ForwardProcBodyInsertPolicyNames[FForwardProcBodyInsertPolicy],
      ForwardProcBodyInsertPolicyNames[fpipInFrontOfMethods]);
    XMLConfig.SetDeleteValue(
      'CodeToolsOptions/KeepForwardProcOrder/Value',FKeepForwardProcOrder,true);
    XMLConfig.SetDeleteValue(
      'CodeToolsOptions/ClassHeaderComments/Value',FClassHeaderComments,true);
    XMLConfig.SetDeleteValue(
      'CodeToolsOptions/UpdateMultiProcSignatures/Value',FUpdateMultiProcSignatures,
      true);
    XMLConfig.SetDeleteValue(
      'CodeToolsOptions/UpdateOtherProcSignaturesCase/Value',FUpdateOtherProcSignaturesCase,
      true);
    XMLConfig.SetDeleteValue(
      'CodeToolsOptions/GroupLocalVariables/Value',FGroupLocalVariables,
      true);
    XMLConfig.SetDeleteValue(
      'CodeToolsOptions/OverrideStringTypesWithFirstParamType/Value',FOverrideStringTypesWithFirstParamType,
      true);
    XMLConfig.SetDeleteValue(
      'CodeToolsOptions/ClassImplementationComments/Value',
      FClassImplementationComments,true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/MethodInsertPolicy/Value',
      MethodInsertPolicyNames[FMethodInsertPolicy],
      MethodInsertPolicyNames[mipClassOrder]);
    XMLConfig.SetDeleteValue('CodeToolsOptions/MethodDefaultSection/Value',
      InsertClassSectionNames[FMethodDefaultSection],
      InsertClassSectionNames[DefaultMethodDefaultSection]);
    XMLConfig.SetDeleteValue('CodeToolsOptions/KeyWordPolicy/Value',
      WordPolicyNames[FKeyWordPolicy],
      WordPolicyNames[wpLowerCase]);
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierPolicy/Value',
      WordPolicyNames[FIdentifierPolicy],
      WordPolicyNames[wpNone]);
    XMLConfig.SetDeleteValue('CodeToolsOptions/WordPolicyExceptions/Value',
      Trim(LineBreaksToSystemLineBreaks(WordPolicyExceptions.Text)),'');
    WriteAtomTypesToXML(XMLConfig,'CodeToolsOptions/DoNotSplitLineInFront/',
      FDoNotSplitLineInFront,DefaultDoNotSplitLineInFront);
    WriteAtomTypesToXML(XMLConfig,'CodeToolsOptions/DoNotSplitLineAfter/',
      FDoNotSplitLineAfter,DefaultDoNotSplitLineAfter);
    WriteAtomTypesToXML(XMLConfig,'CodeToolsOptions/DoInsertSpaceInFront/',
      FDoInsertSpaceInFront,DefaultDoInsertSpaceInFront);
    WriteAtomTypesToXML(XMLConfig,'CodeToolsOptions/DoInsertSpaceAfter/',
      FDoInsertSpaceAfter,DefaultDoInsertSpaceAfter);
    XMLConfig.SetDeleteValue('CodeToolsOptions/PropertyReadIdentPrefix/Value',
      FPropertyReadIdentPrefix,'Get');
    XMLConfig.SetDeleteValue('CodeToolsOptions/PropertyWriteIdentPrefix/Value',
      FPropertyWriteIdentPrefix,'Set');
    XMLConfig.SetDeleteValue('CodeToolsOptions/PropertyStoredIdentPostfix/Value',
      FPropertyStoredIdentPostfix,'IsStored');
    XMLConfig.SetDeleteValue('CodeToolsOptions/PrivateVariablePrefix/Value',
      FPrivateVariablePrefix,'F');
    XMLConfig.SetDeleteValue('CodeToolsOptions/SetPropertyVariablename/Value',
      FSetPropertyVariablename,'AValue');
    XMLConfig.SetDeleteValue('CodeToolsOptions/SetPropertyVariableIsPrefix/Value',
      FSetPropertyVariableIsPrefix,false);
    XMLConfig.SetDeleteValue('CodeToolsOptions/SetPropertyVariableUseConst/Value',
      FSetPropertyVariableUseConst,false);
    XMLConfig.SetDeleteValue('CodeToolsOptions/UsesInsertPolicy/Value',
      UsesInsertPolicyNames[FUsesInsertPolicy],
      UsesInsertPolicyNames[DefaultUsesInsertPolicy]);

    // identifier completion
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierCompletion/AddSemicolon',
      FIdentComplAddSemicolon,true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierCompletion/AddAssignOperator',
      FIdentComplAddAssignOperator,true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierCompletion/AddDo',
      FIdentComplAddDo,true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierCompletion/AutoStartAfterPoint',
      FIdentComplAutoStartAfterPoint,true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierCompletion/AutoUseSingleIdent',
      FIdentComplAutoUseSingleIdent,true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierCompletion/UseContainsFilter',
      FIdentComplUseContainsFilter,true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierCompletion/IncludeCodeTemplates',
      FIdentComplIncludeCodeTemplates,true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierCompletion/IncludeWords',
      IdentComplIncludeWordsNames[FIdentComplIncludeWords],
      IdentComplIncludeWordsNames[icwIncludeFromAllUnits]);
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierCompletion/ShowIcons',
      FIdentComplShowIcons,false);
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierCompletion/AutoAddParameterBrackets',
      FIdentComplAddParameterBrackets,true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierCompletion/ReplaceIdentifier',
      FIdentComplReplaceIdentifier,true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierCompletion/JumpToError',
      FIdentComplJumpToError,true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierCompletion/ShowHelp',
      FIdentComplShowHelp,false);
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierCompletion/SortForHistory',
      FIdentComplSortForHistory,true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/IdentifierCompletion/SortForScope',
      FIdentComplSortForScope,true);

    // indentation
    XMLConfig.SetDeleteValue('CodeToolsOptions/Indentation/OnLineBreak/Enabled'
      , FIndentOnLineBreak, true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/Indentation/OnPaste/Enabled'
      , FIndentOnPaste, true);
    XMLConfig.SetDeleteValue('CodeToolsOptions/Indentation/FileName'
      , fIndentationFilename, '');
    XMLConfig.SetDeleteValue('CodeToolsOptions/Indentation/ContextSensitive'
      , FIndentContextSensitive, true);

    // code completion templates
    XMLConfig.SetDeleteValue('CodeToolsOptions/CodeCompletionTemplate/FileName'
      , FCodeCompletionTemplateFileName, DefaultCodeCompletionFilename);

    XMLConfig.Flush;
    XMLConfig.Free;
  except
    on E: Exception do begin
      DebugLn('[TCodeToolsOptions.Save]  error writing "',FFilename,'": ',E.Message);
    end;
  end;
end;

procedure TCodeToolsOptions.SetCodeCompletionTemplateFileName(aValue: String);
begin
  aValue:=TrimFilename(aValue);
  if FCodeCompletionTemplateFileName=aValue then Exit;
  FCodeCompletionTemplateFileName:=aValue;
end;

procedure TCodeToolsOptions.SetFilename(const AValue: string);
begin
  FFilename:=AValue;
end;

procedure TCodeToolsOptions.SetLazarusDefaultFilename;
var
  ConfFileName: string;
begin
  ConfFileName:=AppendPathDelim(GetPrimaryConfigPath)+DefaultCodeToolsOptsFile;
  CopySecondaryConfigFile(DefaultCodeToolsOptsFile);
  if (not FileExistsCached(ConfFileName)) then begin
    debugln('Looking for code tools config file:  "' + ConfFileName + '"');
    debugln(lisCompilerNOTECodetoolsConfigFileNotFoundUsingDefaults);
  end;
  FFilename:=ConfFilename;
end;

procedure TCodeToolsOptions.SetSetPropertyVariableIsPrefix(aValue: Boolean);
begin
  if FSetPropertyVariableIsPrefix=aValue then Exit;
  FSetPropertyVariableIsPrefix:=aValue;
end;

procedure TCodeToolsOptions.SetSetPropertyVariablename(aValue: string);
begin
  if FSetPropertyVariablename=aValue then Exit;
  FSetPropertyVariablename:=aValue;
end;

procedure TCodeToolsOptions.SetSetPropertyVariableUseConst(aValue: Boolean);
begin
  if FSetPropertyVariableUseConst=aValue then Exit;
  FSetPropertyVariableUseConst:=aValue;
end;

procedure TCodeToolsOptions.Assign(Source: TPersistent);
var
  CodeToolsOpts: TCodeToolsOptions absolute Source;
begin
  if not ((Source = nil) or (Source is TCodeToolsOptions)) then
  begin
    inherited Assign(Source);
    Exit;
  end;

  if CodeToolsOpts <> nil then
  begin
    // General
    FAdjustTopLineDueToComment:=CodeToolsOpts.FAdjustTopLineDueToComment;
    FJumpSingleLinePos:=CodeToolsOpts.FJumpSingleLinePos;
    FJumpCodeBlockPos:=CodeToolsOpts.FJumpCodeBlockPos;
    FAvoidUnnecessaryJumps:=CodeToolsOpts.FAvoidUnnecessaryJumps;
    FCursorBeyondEOL:=CodeToolsOpts.FCursorBeyondEOL;
    FAddInheritedCodeToOverrideMethod:=CodeToolsOpts.AddInheritedCodeToOverrideMethod;
    FCompleteProperties:=CodeToolsOpts.CompleteProperties;
    FSkipForwardDeclarations:=CodeToolsOpts.FSkipForwardDeclarations;
    FJumpToMethodBody:=CodeToolsOpts.FJumpToMethodBody;

    // define templates
    ClearGlobalDefineTemplates;
    FGlobalDefineTemplates:=
      CodeToolsOpts.FGlobalDefineTemplates.CreateCopy(false,true,true);
    if FGlobalDefineTemplates<>nil then
      FGlobalDefineTemplates.SetDefineOwner(Self,true);
    FDefinesEditMainSplitterTop:=CodeToolsOpts.DefinesEditMainSplitterTop;
    FDefinesPreviewMainSplitterPos:=CodeToolsOpts.DefinesPreviewMainSplitterPos;
    FDefinesPreviewTemplSplitterPos:=CodeToolsOpts.DefinesPreviewTemplSplitterPos;

    // CodeCreation
    FLineLength:=CodeToolsOpts.FLineLength;
    FClassPartInsertPolicy:=CodeToolsOpts.FClassPartInsertPolicy;
    FMixMethodsAndProperties:=CodeToolsOpts.MixMethodsAndProperties;
    FUpdateAllMethodSignatures:=CodeToolsOpts.UpdateAllMethodSignatures;
    FForwardProcBodyInsertPolicy:=CodeToolsOpts.ForwardProcBodyInsertPolicy;
    FKeepForwardProcOrder:=CodeToolsOpts.KeepForwardProcOrder;
    FUpdateMultiProcSignatures:=CodeToolsOpts.UpdateMultiProcSignatures;
    FUpdateOtherProcSignaturesCase:=CodeToolsOpts.UpdateOtherProcSignaturesCase;
    FGroupLocalVariables:=CodeToolsOpts.GroupLocalVariables;
    FOverrideStringTypesWithFirstParamType:=CodeToolsOpts.OverrideStringTypesWithFirstParamType;
    FClassHeaderComments:=CodeToolsOpts.ClassHeaderComments;
    FClassImplementationComments:=CodeToolsOpts.ClassImplementationComments;
    FMethodInsertPolicy:=CodeToolsOpts.FMethodInsertPolicy;
    FMethodDefaultSection:=CodeToolsOpts.FMethodDefaultSection;
    FKeyWordPolicy:=CodeToolsOpts.FKeyWordPolicy;
    FIdentifierPolicy:=CodeToolsOpts.FIdentifierPolicy;
    FDoNotSplitLineInFront:=CodeToolsOpts.FDoNotSplitLineInFront;
    FDoNotSplitLineAfter:=CodeToolsOpts.FDoNotSplitLineAfter;
    FDoInsertSpaceInFront:=CodeToolsOpts.FDoInsertSpaceInFront;
    FDoInsertSpaceAfter:=CodeToolsOpts.FDoInsertSpaceAfter;
    FPropertyReadIdentPrefix:=CodeToolsOpts.FPropertyReadIdentPrefix;
    FPropertyWriteIdentPrefix:=CodeToolsOpts.FPropertyWriteIdentPrefix;
    FPropertyStoredIdentPostfix:=CodeToolsOpts.FPropertyStoredIdentPostfix;
    FPrivateVariablePrefix:=CodeToolsOpts.FPrivateVariablePrefix;
    FSetPropertyVariablename:=CodeToolsOpts.FSetPropertyVariablename;
    FSetPropertyVariableIsPrefix:=CodeToolsOpts.FSetPropertyVariableIsPrefix;
    FSetPropertyVariableUseConst:=CodeToolsOpts.FSetPropertyVariableUseConst;
    FUsesInsertPolicy:=CodeToolsOpts.FUsesInsertPolicy;

    // identifier completion
    FIdentComplAddSemicolon:=CodeToolsOpts.FIdentComplAddSemicolon;
    FIdentComplAddAssignOperator:=CodeToolsOpts.FIdentComplAddAssignOperator;
    FIdentComplAddDo:=CodeToolsOpts.FIdentComplAddDo;
    FIdentComplAutoStartAfterPoint:=CodeToolsOpts.FIdentComplAutoStartAfterPoint;
    FIdentComplAutoUseSingleIdent:=CodeToolsOpts.FIdentComplAutoUseSingleIdent;
    FIdentComplUseContainsFilter:=CodeToolsOpts.FIdentComplUseContainsFilter;
    FIdentComplIncludeCodeTemplates:=CodeToolsOpts.FIdentComplIncludeCodeTemplates;
    FIdentComplShowIcons:=CodeToolsOpts.FIdentComplShowIcons;
    FIdentComplAddParameterBrackets:=CodeToolsOpts.FIdentComplAddParameterBrackets;
    FIdentComplReplaceIdentifier:=CodeToolsOpts.FIdentComplReplaceIdentifier;
    FIdentComplJumpToError:=CodeToolsOpts.FIdentComplJumpToError;
    FIdentComplShowHelp:=CodeToolsOpts.FIdentComplShowHelp;
    FIdentComplSortForHistory:=CodeToolsOpts.FIdentComplSortForHistory;
    FIdentComplSortForScope:=CodeToolsOpts.FIdentComplSortForScope;
  end
  else
    Clear;
end;

procedure TCodeToolsOptions.Clear;
// !!! Does not reset Filename !!!
begin
  // General
  FAdjustTopLineDueToComment:=true;
  FJumpSingleLinePos:=50;
  FJumpCodeBlockPos:=0;
  FAvoidUnnecessaryJumps:=true;
  FCursorBeyondEOL:=true;
  
  // define templates
  ClearGlobalDefineTemplates;
  FDefinesEditMainSplitterTop:=100;
  FDefinesPreviewMainSplitterPos:=280;
  FDefinesPreviewTemplSplitterPos:=100;

  // CodeCreation
  FAddInheritedCodeToOverrideMethod:=true;
  FCompleteProperties:=true;
  FLineLength:=80;
  FClassPartInsertPolicy:=cpipLast;
  FMixMethodsAndProperties:=false;
  FUpdateAllMethodSignatures:=true;
  FForwardProcBodyInsertPolicy:=fpipInFrontOfMethods;
  FKeepForwardProcOrder:=true;
  FUpdateMultiProcSignatures:=true;
  FUpdateOtherProcSignaturesCase:=true;
  FGroupLocalVariables:=true;
  FOverrideStringTypesWithFirstParamType:=true;
  FClassHeaderComments:=true;
  FClassImplementationComments:=true;
  FMethodInsertPolicy:=mipClassOrder;
  FMethodDefaultSection:=DefaultMethodDefaultSection;
  FKeyWordPolicy:=wpLowerCase;
  FIdentifierPolicy:=wpNone;
  FDoNotSplitLineInFront:=DefaultDoNotSplitLineInFront;
  FDoNotSplitLineAfter:=DefaultDoNotSplitLineAfter;
  FDoInsertSpaceInFront:=DefaultDoInsertSpaceInFront;
  FDoInsertSpaceAfter:=DefaultDoInsertSpaceAfter;
  FPropertyReadIdentPrefix:='Get';
  FPropertyWriteIdentPrefix:='Set';
  FPropertyStoredIdentPostfix:='IsStored';
  FPrivateVariablePrefix:='f';
  FSetPropertyVariablename:='AValue';
  FSetPropertyVariableIsPrefix:=false;
  FSetPropertyVariableUseConst:=false;
  FUsesInsertPolicy:=DefaultUsesInsertPolicy;

  // identifier completion
  FIdentComplAddSemicolon:=true;
  FIdentComplAddAssignOperator:=true;
  FIdentComplAddDo:=true;
  FIdentComplAutoStartAfterPoint:=true;
  FIdentComplAutoUseSingleIdent:=true;
  FIdentComplUseContainsFilter:=true;
  FIdentComplIncludeCodeTemplates:=true;
  FIdentComplShowIcons:=false;
  FIdentComplAddParameterBrackets:=true;
  FIdentComplReplaceIdentifier:=true;
  FIdentComplJumpToError:=true;
  FIdentComplShowHelp:=false;
  FIdentComplSortForHistory:=true;
  FIdentComplSortForScope:=true;

  // indentation
  FIndentOnLineBreak:=true;
  FIndentOnPaste:=true;
  fIndentationFilename:=
    TrimFilename(AppendPathDelim(GetPrimaryConfigPath)+DefaultIndentationFilename);
  FIndentContextSensitive:=true;

  // code completion templates
  fCodeCompletionTemplateFileName := DefaultCodeCompletionFilename;
end;

procedure TCodeToolsOptions.ClearGlobalDefineTemplates;
begin
  if FGlobalDefineTemplates<>nil then begin
    FGlobalDefineTemplates.Clear(true);
    FGlobalDefineTemplates.Free;
    FGlobalDefineTemplates:=nil;
  end;
end;

function TCodeToolsOptions.IsEqual(CodeToolsOpts: TCodeToolsOptions): boolean;
begin
  Result:=
    // General
        (FAdjustTopLineDueToComment=CodeToolsOpts.FAdjustTopLineDueToComment)
    and (FJumpSingleLinePos=CodeToolsOpts.FJumpSingleLinePos)
    and (FJumpCodeBlockPos=CodeToolsOpts.FJumpCodeBlockPos)
    and (FAvoidUnnecessaryJumps=CodeToolsOpts.FAvoidUnnecessaryJumps)
    and (FCursorBeyondEOL=CodeToolsOpts.FCursorBeyondEOL)
    and (AddInheritedCodeToOverrideMethod=CodeToolsOpts.AddInheritedCodeToOverrideMethod)
    and (CompleteProperties=CodeToolsOpts.CompleteProperties)
    and (FSkipForwardDeclarations=CodeToolsOpts.FSkipForwardDeclarations)
    and (FJumpToMethodBody=CodeToolsOpts.FJumpToMethodBody)
    
    // define templates
    and (FGlobalDefineTemplates.IsEqual(
                                CodeToolsOpts.FGlobalDefineTemplates,true,true))
    and (FDefinesEditMainSplitterTop=CodeToolsOpts.fDefinesEditMainSplitterTop)
    and (FDefinesPreviewMainSplitterPos=CodeToolsOpts.FDefinesPreviewMainSplitterPos)
    and (FDefinesPreviewTemplSplitterPos=CodeToolsOpts.FDefinesPreviewTemplSplitterPos)

    // CodeCreation
    and (FLineLength=CodeToolsOpts.FLineLength)
    and (FClassPartInsertPolicy=CodeToolsOpts.FClassPartInsertPolicy)
    and (FMixMethodsAndProperties=CodeToolsOpts.MixMethodsAndProperties)
    and (FUpdateAllMethodSignatures=CodeToolsOpts.UpdateAllMethodSignatures)
    and (FForwardProcBodyInsertPolicy=CodeToolsOpts.ForwardProcBodyInsertPolicy)
    and (FKeepForwardProcOrder=CodeToolsOpts.KeepForwardProcOrder)
    and (FUpdateMultiProcSignatures=CodeToolsOpts.UpdateMultiProcSignatures)
    and (FUpdateOtherProcSignaturesCase=CodeToolsOpts.UpdateOtherProcSignaturesCase)
    and (FGroupLocalVariables=CodeToolsOpts.GroupLocalVariables)
    and (FOverrideStringTypesWithFirstParamType=CodeToolsOpts.OverrideStringTypesWithFirstParamType)
    and (FClassHeaderComments=CodeToolsOpts.ClassHeaderComments)
    and (FClassImplementationComments=CodeToolsOpts.ClassImplementationComments)
    and (FMethodInsertPolicy=CodeToolsOpts.FMethodInsertPolicy)
    and (FMethodDefaultSection=CodeToolsOpts.FMethodDefaultSection)
    and (FKeyWordPolicy=CodeToolsOpts.FKeyWordPolicy)
    and (FIdentifierPolicy=CodeToolsOpts.FIdentifierPolicy)
    and (FDoNotSplitLineInFront=CodeToolsOpts.FDoNotSplitLineInFront)
    and (FDoNotSplitLineAfter=CodeToolsOpts.FDoNotSplitLineAfter)
    and (FDoInsertSpaceInFront=CodeToolsOpts.FDoInsertSpaceInFront)
    and (FDoInsertSpaceAfter=CodeToolsOpts.FDoInsertSpaceAfter)
    and (FPropertyReadIdentPrefix=CodeToolsOpts.FPropertyReadIdentPrefix)
    and (FPropertyWriteIdentPrefix=CodeToolsOpts.FPropertyWriteIdentPrefix)
    and (FPropertyStoredIdentPostfix=CodeToolsOpts.FPropertyStoredIdentPostfix)
    and (FPrivateVariablePrefix=CodeToolsOpts.FPrivateVariablePrefix)
    and (FSetPropertyVariablename=CodeToolsOpts.FSetPropertyVariablename)
    and (FSetPropertyVariableIsPrefix=CodeToolsOpts.FSetPropertyVariableIsPrefix)
    and (FSetPropertyVariableUseConst=CodeToolsOpts.FSetPropertyVariableUseConst)
    and (FUsesInsertPolicy=CodeToolsOpts.FUsesInsertPolicy)

    // identifier completion
    and (FIdentComplAddSemicolon=CodeToolsOpts.FIdentComplAddSemicolon)
    and (FIdentComplAddAssignOperator=CodeToolsOpts.FIdentComplAddAssignOperator)
    and (FIdentComplAddDo=CodeToolsOpts.FIdentComplAddDo)
    and (FIdentComplAutoStartAfterPoint=CodeToolsOpts.FIdentComplAutoStartAfterPoint)
    and (FIdentComplAutoUseSingleIdent=CodeToolsOpts.FIdentComplAutoUseSingleIdent)
    and (FIdentComplUseContainsFilter=CodeToolsOpts.FIdentComplUseContainsFilter)
    and (FIdentComplIncludeCodeTemplates=CodeToolsOpts.FIdentComplIncludeCodeTemplates)
    and (FIdentComplShowIcons=CodeToolsOpts.FIdentComplShowIcons)
    and (FIdentComplAddParameterBrackets=CodeToolsOpts.FIdentComplAddParameterBrackets)
    and (FIdentComplReplaceIdentifier=CodeToolsOpts.FIdentComplReplaceIdentifier)
    and (FIdentComplJumpToError=CodeToolsOpts.FIdentComplJumpToError)
    and (FIdentComplShowHelp=CodeToolsOpts.FIdentComplShowHelp)
    and (FIdentComplSortForHistory=CodeToolsOpts.FIdentComplSortForHistory)
    and (FIdentComplSortForScope=CodeToolsOpts.FIdentComplSortForScope)
   ;
end;

function TCodeToolsOptions.CreateCopy: TCodeToolsOptions;
begin
  Result:=TCodeToolsOptions.Create;
  Result.Assign(Self);
  Result.Filename:=Filename;
end;

procedure TCodeToolsOptions.ReadGlobalDefinesTemplatesFromTree(Tree: TDefineTree);
begin
  ClearGlobalDefineTemplates;
  FGlobalDefineTemplates:=
    Tree.ExtractTemplatesOwnedBy(Self,[],[dtfAutoGenerated]);
  if FGlobalDefineTemplates<>nil then begin
    FGlobalDefineTemplates.SetDefineOwner(Self,true);
  end;
end;

procedure TCodeToolsOptions.CreateDefaultIndentationFile;
var
  res: TResourceStream;
  fs: TFileStreamUTF8;
begin
  // indentations (laz_indentation.pas)
  CopySecondaryConfigFile(DefaultIndentationFilename);
  if not FileExistsUTF8(IndentationFilename) then
  begin
    res := TResourceStream.Create(HInstance, PChar('indentation'), PChar(RT_RCDATA));
    try
      InvalidateFileStateCache;
      fs := TFileStreamUTF8.Create(IndentationFilename, fmCreate);
      try
        fs.CopyFrom(res, res.Size);
      finally
        fs.Free;
      end;
    except
      DebugLn('WARNING: unable to write indentation file "',
        IndentationFilename, '"');
    end;
    res.Free;
  end;
end;

procedure TCodeToolsOptions.AssignTo(Dest: TPersistent);
var
  Boss: TCodeToolManager absolute Dest;
  Beauty: TBeautifyCodeOptions absolute Dest;
  aFilename: String;
begin
  if Dest is TCodeToolManager then
  begin
    // General - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Boss.AdjustTopLineDueToComment:=AdjustTopLineDueToComment;
    Boss.JumpSingleLinePos:=JumpSingleLinePos;
    Boss.JumpCodeBlockPos:=JumpCodeBlockPos;
    Boss.CursorBeyondEOL:=CursorBeyondEOL;
    Boss.AddInheritedCodeToOverrideMethod:=AddInheritedCodeToOverrideMethod;
    Boss.CompleteProperties:=CompleteProperties;

    // CreateCode - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    AssignTo(Boss.SourceChangeCache.BeautifyCodeOptions);
    Boss.SetPropertyVariablename:=SetPropertyVariablename;
    Boss.SetPropertyVariableIsPrefix:=SetPropertyVariableIsPrefix;
    Boss.SetPropertyVariableUseConst:=SetPropertyVariableUseConst;

    // Identifier Completion - - - - - - - - - - - - - - - - - - - - - - - - - -
    Boss.IdentifierList.SortForHistory:=IdentComplSortForHistory;
    Boss.IdentifierList.SortForScope:=IdentComplSortForScope;

    // Code Templates- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    aFilename:=CodeCompletionTemplateFileName;
    IDEMacros.SubstituteMacros(aFilename);
    aFilename:=TrimFilename(aFilename);
    if (aFilename<>'') and not FilenameIsAbsolute(aFilename) then
      aFilename:=TrimFilename(AppendPathDelim(GetPrimaryConfigPath)+aFilename);
    Boss.CodeCompletionTemplateFileName:=aFilename;
  end
  else
  if Dest is TBeautifyCodeOptions then
  begin
    Beauty.LineLength:=LineLength;
    Beauty.ClassPartInsertPolicy:=ClassPartInsertPolicy;
    Beauty.MixMethodsAndProperties:=MixMethodsAndProperties;
    Beauty.UpdateAllMethodSignatures:=UpdateAllMethodSignatures;
    Beauty.ForwardProcBodyInsertPolicy:=ForwardProcBodyInsertPolicy;
    Beauty.KeepForwardProcOrder:=KeepForwardProcOrder;
    Beauty.UpdateMultiProcSignatures:=UpdateMultiProcSignatures;
    Beauty.UpdateOtherProcSignaturesCase:=UpdateOtherProcSignaturesCase;
    Beauty.GroupLocalVariables:=GroupLocalVariables;
    Beauty.OverrideStringTypesWithFirstParamType:=OverrideStringTypesWithFirstParamType;
    Beauty.ClassHeaderComments:=ClassHeaderComments;
    Beauty.ClassImplementationComments:=ClassImplementationComments;
    Beauty.MethodInsertPolicy:=MethodInsertPolicy;
    Beauty.MethodDefaultSection:=MethodDefaultSection;
    Beauty.KeyWordPolicy:=KeyWordPolicy;
    Beauty.IdentifierPolicy:=IdentifierPolicy;
    Beauty.SetupWordPolicyExceptions(WordPolicyExceptions);
    Beauty.DoNotSplitLineInFront:=DoNotSplitLineInFront;
    Beauty.DoNotSplitLineAfter:=DoNotSplitLineAfter;
    Beauty.DoInsertSpaceInFront:=DoInsertSpaceInFront;
    Beauty.DoInsertSpaceAfter:=DoInsertSpaceAfter;
    Beauty.PropertyReadIdentPrefix:=PropertyReadIdentPrefix;
    Beauty.PropertyWriteIdentPrefix:=PropertyWriteIdentPrefix;
    Beauty.PropertyStoredIdentPostfix:=PropertyStoredIdentPostfix;
    Beauty.PrivateVariablePrefix:=PrivateVariablePrefix;
    Beauty.UsesInsertPolicy:=UsesInsertPolicy;
  end
  else
    inherited AssignTo(Dest);
end;

procedure TCodeToolsOptions.AssignGlobalDefineTemplatesToTree(Tree: TDefineTree);
begin
  // Define templates - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  // remove old custom define templates
  Tree.RemoveTemplatesOwnedBy(Self,[],[dtfAutoGenerated]);
  // merge current custom define templates
  if FGlobalDefineTemplates<>nil then
    Tree.MergeDefineTemplates(FGlobalDefineTemplates,'');
end;

initialization
  RegisterIDEOptionsGroup(GroupCodetools, TCodeToolsOptions);
end.

