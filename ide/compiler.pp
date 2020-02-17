{
 /***************************************************************************
                        compiler.pp  -  Lazarus IDE unit
                        -------------------------------------
               TCompiler is responsible for configuration and running
               the Free Pascal Compiler.


                   Initial Revision  : Sun Mar 28 23:15:32 CST 1999


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
unit Compiler;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, contnrs, strutils,
  // LazUtils
  LazUTF8, LazFileUtils, LazUtilities, LazLoggerBase,
  // LCL
  Forms, Controls,
  // Codetools
  DefineTemplates, LinkScanner, CodeToolManager, TransferMacros,
  // IdeIntf
  IDEExternToolIntf, IDEMsgIntf, LazIDEIntf,
  // IDE
  IDECmdLine, LazarusIDEStrConsts, CompilerOptions, Project, EnvironmentOpts;

type
  TOnCmdLineCreate = procedure(var CmdLine: string; var Abort:boolean) of object;

  { TCompiler }

  TCompiler = class(TObject)
  private
    FOnCmdLineCreate : TOnCmdLineCreate;
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(AProject: TProject;
                     const WorkingDir, CompilerFilename, CompilerParams: string;
                     BuildAll, SkipLinking, SkipAssembler, CurrentDirectoryIsTestDir: boolean;
                     const aCompileHint: string): TModalResult;
    procedure WriteError(const Msg: string);
  end;

  // Following classes are for compiler options parsed from "fpc -h" and "fpc -i".

  TCompilerOptEditKind = (
    oeGroup,      // A header for a group
    oeSet,        // A header for a set
    oeSetElem,    // One char element of a set, use CheckBox
    oeSetNumber,  // Number element of a set, use Edit
    oeBoolean,    // True/False, typically use CheckBox
    oeText,       // Textual value
    oeNumber,     // Numeric value
    oeList        // Pre-defined list of choices
  );

  TCompilerOptGroup = class;

  { TCompilerOpt }

  TCompilerOpt = class
  private
    fOwnerGroup: TCompilerOptGroup;
    fId: integer;                       // Identification.
    fOption: string;                    // Option with the leading '-'.
    fSuffix: string;                    // <x> or similar suffix of option.
    fValue: string;                     // Data entered by user, 'True' for Boolean.
    fOrigLine: integer;                 // Original line in the input data.
    fEditKind: TCompilerOptEditKind;
    fDescription: string;
    fIndentation: integer;              // Indentation level in "fpc -h" output.
    fVisible: Boolean;                  // Used for filtering.
    fIgnored: Boolean;                  // Pretend this option does not exist.
    fChoices: TStrings;                 // Choices got from "fpc -i"
    procedure AddChoicesByOptOld;
    function Comment: string;
    procedure Filter(aFilter: string; aOnlySelected: Boolean);
    function GenerateOptValue(aUseComments: Boolean): string;
    procedure SetValue(aValue: string; aOrigLine: integer);
  protected
    procedure ParseEditKind; virtual;
    function ParseOption(aDescr: string; aIndent: integer): Boolean; virtual;
  public
    constructor Create(aOwnerGroup: TCompilerOptGroup);
    destructor Destroy; override;
    function CalcLeft(aDefaultLeft, aLimit: integer): integer;
  public
    property Id: integer read fId;
    property Option: string read fOption;
    property Suffix: string read fSuffix;
    property Value: string read fValue write fValue;
    property EditKind: TCompilerOptEditKind read fEditKind;
    property Description: string read fDescription;
    property Indentation: integer read fIndentation;
    property Visible: Boolean read fVisible write fVisible;
    property Ignored: Boolean read fIgnored write fIgnored;
    property Choices: TStrings read fChoices;
  end;

  TCompilerOptList = TObjectList;
  TCompilerOptReader = class;         // Forward reference

  { TCompilerOptGroup }

  // Group with explanation header. Actual options are not defined here.
  TCompilerOptGroup = class(TCompilerOpt)
  private
    fOwnerReader: TCompilerOptReader;
    // List of options belonging to this group.
    fCompilerOpts: TCompilerOptList;
    fIncludeNegativeOpt: Boolean; // Each option has a variation with "NO" appended.
    function OneCharOptions(aOptAndValue: string): TCompilerOpt;
  protected
    procedure ParseEditKind; override;
    function ParseOption(aDescr: string; aIndent: integer): Boolean; override;
  public
    constructor Create(aOwnerReader: TCompilerOptReader; aOwnerGroup: TCompilerOptGroup);
    destructor Destroy; override;
    procedure Clear;
    function FindOption(aOptStr: string): TCompilerOpt;
    function FindOptionById(aId: integer): TCompilerOpt;
    function SelectOption(aOptAndValue: string): Boolean;
    procedure DeselectAll;
  public
    property CompilerOpts: TCompilerOptList read fCompilerOpts;
  end;

  { TCompilerOptSet }

  // A set of options. A combination of chars or numbers following the option char.
  TCompilerOptSet = class(TCompilerOptGroup)
  private
    fCommonIndent: integer; // Common indentation for this group fixed during parse.
    function SetNumberOpt(aValue: string): Boolean;
    function SetBooleanOpt(aValue: string): Boolean;
  protected
    procedure AddOptions(aDescr: string; aIndent: integer);
    procedure ParseEditKind; override;
  public
    constructor Create(aOwnerReader: TCompilerOptReader;
      aOwnerGroup: TCompilerOptGroup; aCommonIndent: integer);
    destructor Destroy; override;
    function CollectSelectedOptions(aUseComments: Boolean): string;
    procedure SelectOptions(aOptStr: string);
    property CommonIndent: integer read fCommonIndent write fCommonIndent;
  end;

  { TCompilerOptReader }

  TCompilerOptReader = class
  private
    fCurOrigLine: integer;        // Current line num when parsing original data.
    // Defines (-d...) are separated from custom options and stored here.
    fDefines: TStringList;
    // Options not accepted by parser. They may still be valid (a macro maybe)
    fInvalidOptions: TStringList;        // and will be included in output.
    // List of categories parsed from "fpc -i". Contains category names,
    //  Objects[] contains another StringList for the selection list.
    fSupportedCategories: TStringList;
    // Hierarchy of options parsed from "fpc -h".
    fRootOptGroup: TCompilerOptGroup;
    fCompilerExecutable: string;  // Compiler path must be set by caller.
    fFpcVersion: string;          // Parsed from "fpc -h".
    fIsNewFpc: Boolean;
    fParsedTarget: String;
    fErrorMsg: String;
    fGeneratedOptions: TStringList; // Options generated from GUI.
    fUseComments: Boolean;        // Add option's description into generated data.
    function AddChoicesNew(aOpt: string): TStrings;
    function AddNewCategory(aCategoryName: String): TStringList;
    function AddOptInLowestOrigLine(OutStrings: TStrings): Boolean;
    procedure CopyOptions(aRoot: TCompilerOpt);
    function FindLowestOrigLine(aStrings: TStrings; out aOrigLine: Integer): integer;
    function IsGroup(aOpt: string; var aCategoryList: TStrings): Boolean;
    function ReadCategorySelections(aChar: Char): TStringList;
    function ReadVersion(s: string): Boolean;
    procedure CreateNewGroupItem(aGroup: TCompilerOptGroup; aTxt: string);
    procedure AddGroupItems(aGroup: TCompilerOptGroup; aItems: TStrings);
    function ParseI(aLines: TStringList): TModalResult;
    function ParseH(aLines: TStringList): TModalResult;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function UpdateTargetParam: Boolean;
    function ReadAndParseOptions: TModalResult;
    function FilterOptions(aFilter: string; aOnlySelected: Boolean): Boolean;
    function FindOptionById(aId: integer): TCompilerOpt;
    function FromCustomOptions(aStrings: TStrings): TModalResult;
    function ToCustomOptions(aStrings: TStrings; aUseComments: Boolean): TModalResult;
  public
    property Defines: TStringList read fDefines;
    //property SupportedCategories: TStringList read fSupportedCategories;
    property RootOptGroup: TCompilerOptGroup read fRootOptGroup;
    property CompilerExecutable: string read fCompilerExecutable write fCompilerExecutable;
    property ParsedTarget: String read fParsedTarget write fParsedTarget;
    property ErrorMsg: String read fErrorMsg write fErrorMsg;
  end;

  { TCompilerOptThread - thread for reading 'fpc -h' output }

  TCompilerOptThread = class(TThread)
  private
    fReader: TCompilerOptReader;
    fReadTime: TDateTime;
    fStartedOnce: boolean;
    function GetErrorMsg: string;
    procedure Clear; // (main thread)
  protected
    procedure Execute; override;
  public
    constructor Create(aReader: TCompilerOptReader);
    destructor Destroy; override;
    procedure StartParsing; // (main thread)
    procedure EndParsing; // (main thread)
  public
    property ReadTime: TDateTime read fReadTime;
    property ErrorMsg: string read GetErrorMsg;
  end;

implementation

{ TCompiler }

{------------------------------------------------------------------------------
  TCompiler Constructor
------------------------------------------------------------------------------}

constructor TCompiler.Create;
begin
  inherited Create;
end;

{------------------------------------------------------------------------------
  TCompiler Destructor
------------------------------------------------------------------------------}
destructor TCompiler.Destroy;
begin
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TCompiler Compile
------------------------------------------------------------------------------}
function TCompiler.Compile(AProject: TProject; const WorkingDir,
  CompilerFilename, CompilerParams: string; BuildAll, SkipLinking,
  SkipAssembler, CurrentDirectoryIsTestDir: boolean; const aCompileHint: string
  ): TModalResult;
var
  CmdLine : String;
  Abort : Boolean;
  Tool: TAbstractExternalTool;
  FPCParser: TFPCParser;
  Title: String;
  TargetOS: String;
  TargetCPU: String;
  TargetFilename, SubTool: String;
  CompilerKind: TPascalCompiler;
begin
  Result:=mrCancel;
  if ConsoleVerbosity>=1 then
    DebugLn('TCompiler.Compile WorkingDir="',WorkingDir,'" CompilerFilename="',CompilerFilename,'" CompilerParams="',CompilerParams,'"');

  try
    CheckIfFileIsExecutable(CompilerFilename);
  except
    on E: Exception do begin
      WriteError(Format(lisCompilerErrorInvalidCompiler, [E.Message]));
      if CompilerFilename='' then begin
        WriteError(lisCompilerHintYouCanSetTheCompilerPath);
      end;
      exit;
    end;
  end;
  CmdLine := '';
  if BuildAll then
    CmdLine := CmdLine+' -B';
  if SkipLinking and SkipAssembler then
    CmdLine := CmdLine+' -s'
  else if SkipLinking then
    CmdLine := CmdLine+' -Cn';

  if CompilerParams<>'' then
    CmdLine := CmdLine+' '+CompilerParams;
  if Assigned(FOnCmdLineCreate) then begin
    Abort:=false;
    FOnCmdLineCreate(CmdLine,Abort);
    if Abort then begin
      Result:=mrAbort;
      exit;
    end;
  end;
  if ConsoleVerbosity>=0 then
    DebugLn('[TCompiler.Compile] CmdLine="',CompilerFilename+CmdLine,'"');

  Title:=lisCompileProject;
  if AProject.BuildModes.Count>1 then
    Title+=Format(lisMode, [AProject.ActiveBuildMode.Identifier]);
  TargetOS:=AProject.CompilerOptions.GetEffectiveTargetOS;
  if TargetOS<>GetCompiledTargetOS then
    Title+=Format(lisOS, [TargetOS]);
  TargetCPU:=AProject.CompilerOptions.GetEffectiveTargetCPU;
  if TargetCPU<>GetCompiledTargetCPU then
    Title+=Format(lisCPU, [TargetCPU]);
  TargetFilename:=AProject.GetShortFilename(
          AProject.CompilerOptions.CreateTargetFilename,false);
  if TargetFilename<>'' then
    Title+=Format(lisTarget2, [TargetFilename]);

  Tool:=ExternalToolList.Add(Title);
  Tool.Reference(Self,ClassName);
  try
    Tool.Data:=TIDEExternalToolData.Create(IDEToolCompileProject,'',AProject.ProjectInfoFile);
    Tool.FreeData:=true;
    Tool.Hint:=aCompileHint;
    Tool.Process.Executable:=CompilerFilename;
    Tool.CmdLineParams:=CmdLine;
    Tool.Process.CurrentDirectory:=WorkingDir;
    Tool.CurrentDirectoryIsTestDir:=CurrentDirectoryIsTestDir;
    SubTool:=SubToolFPC;
    CompilerKind:=CodeToolBoss.GetPascalCompilerForDirectory(WorkingDir);
    if CompilerKind=pcPas2js then
      SubTool:=SubToolPas2js;
    FPCParser:=TFPCParser(Tool.AddParsers(SubTool));
    FPCParser.ShowLinesCompiled:=EnvironmentOptions.MsgViewShowFPCMsgLinesCompiled;
    FPCParser.HideHintsSenderNotUsed:=not AProject.CompilerOptions.ShowHintsForSenderNotUsed;
    FPCParser.HideHintsUnitNotUsedInMainSource:=not AProject.CompilerOptions.ShowHintsForUnusedUnitsInMainSrc;
    if (not AProject.CompilerOptions.ShowHintsForUnusedUnitsInMainSrc)
    and (AProject.MainFilename<>'') then
      FPCParser.FilesToIgnoreUnitNotUsed.Add(AProject.MainFilename);
    Tool.AddParsers(SubToolMake);
    Tool.Execute;
    Tool.WaitForExit;
    if Tool.ErrorMessage='' then
      Result:=mrOK;
  finally
    Tool.Release(Self);
  end;
  if ConsoleVerbosity>=0 then
    DebugLn('[TCompiler.Compile] end');
end;

procedure TCompiler.WriteError(const Msg: string);
begin
  DebugLn('TCompiler.WriteError ',Msg);
  if IDEMessagesWindow<>nil then
    IDEMessagesWindow.AddCustomMessage(mluError,Msg);
end;

// Compiler options parsed from "fpc -h" and "fpc -i".

var
  OptionIdCounter: integer;


function NextOptionId: integer;
begin
  Result := OptionIdCounter;
  Inc(OptionIdCounter);
end;

function CalcIndentation(s: string): integer;
begin
  Result := 0;
  while (Result < Length(s)) and (s[Result+1] = ' ') do
    Inc(Result);
end;

function IsIgnoredOption(aOpt: string): Boolean;
begin
  if Length(aOpt) < 2 then Exit(False);
  // Ignore : * information
  //          * all file names and paths
  //          * executable path
  //          * change name of produced executable
  //          * define and undefine
  //          * set language mode
  //          * target operating system
  Result := aOpt[2] in ['i', 'F', 'e', 'o', 'd', 'u', 'M', 'T'];
end;


{ TCompilerOpt }

constructor TCompilerOpt.Create(aOwnerGroup: TCompilerOptGroup);
begin
  inherited Create;
  fOwnerGroup := aOwnerGroup;
  if Assigned(aOwnerGroup) then
    aOwnerGroup.fCompilerOpts.Add(Self);
  fId := NextOptionId;
  fOrigLine := -1;
end;

destructor TCompilerOpt.Destroy;
begin
  inherited Destroy;
end;

procedure TCompilerOpt.AddChoicesByOptOld;
// From FPC 2.6.x output

  procedure AddChoices(aCategory: string);
  // Add selection choices for this option. Data originates from "fpc -i".
  var
    i: Integer;
  begin
    with fOwnerGroup.fOwnerReader do
      if fSupportedCategories.Find(aCategory, i) then
        fChoices := fSupportedCategories.Objects[i] as TStrings
      else
        raise Exception.CreateFmt('No selection list for "%s" found.', [aCategory]);
  end;

begin
  if Pos('fpc -i', fDescription) = 0 then Exit;
  fEditKind := oeList;                 // Values will be got later.
  case fOption of
    '-Ca': AddChoices('ABI targets:');
    '-Cf': AddChoices('FPU instruction sets:');
    '-Cp': AddChoices('CPU instruction sets:');
//      '-Oo', '-Oo[NO]': AddChoices('Optimizations:');
    '-Op': AddChoices('CPU instruction sets:');
//      '-OW': AddChoices('Whole Program Optimizations:');
//      '-Ow': AddChoices('Whole Program Optimizations:');
    else
      raise Exception.Create('Don''t know where to get selection list for option '+fOption);
  end;
end;

procedure TCompilerOpt.ParseEditKind;
begin
  // Guess whether this option can be edited and what is the EditKind
  fEditKind := oeBoolean;                  // Default kind
  if (Length(fSuffix) = 3) and (fSuffix[1] = '<') and (fSuffix[3] = '>') then
    case fSuffix[2] of
      'x': fEditKind:=oeText;              // <x>
      'n': fEditKind:=oeNumber;            // <n>
    end;
  if fOwnerGroup.fOwnerReader.fIsNewFpc then begin
    fChoices := fOwnerGroup.fOwnerReader.AddChoicesNew(fDescription);
    if Assigned(fChoices) then
      fEditKind := oeList;
  end
  else
    AddChoicesByOptOld;
end;

function TCompilerOpt.ParseOption(aDescr: string; aIndent: integer): Boolean;
var
  i: Integer;
begin
  Result := True;
  fIndentation := aIndent;
  if aDescr[1] <> '-' then Exit(False);     // Skip free text explanations.
  // Separate the actual option and description from each other
  i := 1;
  while (i <= Length(aDescr)) and (aDescr[i] <> ' ') do
    Inc(i);
  fOption := Copy(aDescr, 1, i-1);
  while (i <= Length(aDescr)) and (aDescr[i] = ' ') do
    Inc(i);
  fDescription := Copy(aDescr, i, Length(aDescr));
  i := Length(fOption);
  if (i > 3) and (fOption[i-2] = '<') and (fOption[i] = '>') then
  begin
    // Move <x> in the end to Suffix. We need the pure option later.
    fSuffix := Copy(fOption, i-2, i);
    fOption := Copy(fOption, 1, i-3);
  end;
  if fOwnerGroup.fIgnored or IsIgnoredOption(fOption) then
    fIgnored := True;
  ParseEditKind;
end;

procedure TCompilerOpt.Filter(aFilter: string; aOnlySelected: Boolean);
var
  //iOpt, iDes: SizeInt;
  HideNonSelected: Boolean;
begin
  HideNonSelected := (fValue='') and aOnlySelected;
  Visible := not (fIgnored or HideNonSelected)
    and ( (aFilter='') or (Pos(aFilter,UTF8LowerCase(fOption))>0)
                       or (Pos(aFilter,UTF8LowerCase(fDescription))>0) );
{
  if aFilter = '' then
    Visible := not (fIgnored or HideNonSelected)
  else begin
    iOpt := Pos(aFilter,UTF8LowerCase(fOption));
    iDes := Pos(aFilter,UTF8LowerCase(fDescription));
    Visible := not (fIgnored or HideNonSelected) and ( (iOpt>0) or (iDes>0) );
    if Visible then
      DebugLn(['TCompilerOpt.Filter match "', aFilter, '": iOpt=', iOpt,
        ', iDes=', iDes, ', Ignore=', fIgnored, ', aOnlySelected=', aOnlySelected,
        ', Opt'=fOption, ', Descr=', fDescription]);
  end;
}
end;

const
  CommentId = '-dLazIdeComment_';

function TCompilerOpt.Comment: string;
begin
  Result := '  ' + CommentId + StringReplace(fDescription,' ','_',[rfReplaceAll]);
end;

function TCompilerOpt.GenerateOptValue(aUseComments: Boolean): string;
begin
  if fValue = '' then Exit('');
  if fValue = 'True' then                  // Boolean
    Result := fOption
  else                                     // or value of other kind
    Result := fOption + StrToCmdLineParam(Value);
  // ToDo: Show "//" comment in editor and change to a define when storing.
  //   Result := '    // ' + aOpt.Description
  if aUseComments then  // Replace illegal characters with '_' in comment
    Result := Result + Comment;
end;

procedure TCompilerOpt.SetValue(aValue: string; aOrigLine: integer);
begin
  fValue := aValue;
  fOrigLine := aOrigLine;
end;

function TCompilerOpt.CalcLeft(aDefaultLeft, aLimit: integer): integer;
var
  Len: Integer;
begin
  Len := (fIndentation div 2) + Length(fOption);      // Approximation
  if Len > aLimit then
    Result := aDefaultLeft + (Len-aLimit)*8
  else
    Result := aDefaultLeft;
end;

{ TCompilerOptGroup }

constructor TCompilerOptGroup.Create(aOwnerReader: TCompilerOptReader; aOwnerGroup: TCompilerOptGroup);
begin
  inherited Create(aOwnerGroup);
  fOwnerReader := aOwnerReader;
  fCompilerOpts := TCompilerOptList.Create;
end;

destructor TCompilerOptGroup.Destroy;
begin
  fCompilerOpts.Free;
  inherited Destroy;
end;

procedure TCompilerOptGroup.Clear;
begin
  fCompilerOpts.Clear;
end;

function TCompilerOptGroup.FindOption(aOptStr: string): TCompilerOpt;

  function FindOptionSub(aRoot: TCompilerOpt): TCompilerOpt;
  var
    Children: TCompilerOptList;
    i: Integer;
  begin
    Result := Nil;
    if aRoot is TCompilerOptGroup then
    begin
      Children := TCompilerOptGroup(aRoot).CompilerOpts;
      if aRoot is TCompilerOptSet then
      begin                  // TCompilerOptSet
        if AnsiStartsStr(aRoot.Option, aOptStr) then
        begin
          with TCompilerOptSet(aRoot) do
            SelectOptions(Copy(aOptStr, Length(aRoot.Option)+1, Length(aOptStr)));
          Result := aRoot;
        end;
      end
      else begin             // TCompilerOptGroup
        for i := 0 to Children.Count-1 do         // Recursive call for children.
        begin
          Result := FindOptionSub(TCompilerOpt(Children[i]));
          if Assigned(Result) then Break;
        end;
      end;
    end
    else begin               // TCompilerOpt
      if aRoot.Option = aOptStr then
        Result := aRoot
      else if (aRoot.EditKind = oeText) and AnsiStartsStr(aRoot.Option, aOptStr) then
      begin
        aRoot.SetValue(Copy(aOptStr, Length(aRoot.Option)+1, Length(aOptStr)),
                       fOwnerReader.fCurOrigLine);
        Result := aRoot;
      end;
    end;
  end;

begin
  Result := FindOptionSub(Self);
end;

function TCompilerOptGroup.FindOptionById(aId: integer): TCompilerOpt;

  function FindOptionSub(aRoot: TCompilerOpt): TCompilerOpt;
  var
    Children: TCompilerOptList;
    i: Integer;
  begin
    Result := Nil;
    if aRoot is TCompilerOptGroup then
    begin
      Children := TCompilerOptGroup(aRoot).CompilerOpts;
      for i := 0 to Children.Count-1 do         // Recursive call for children.
      begin
        Result := FindOptionSub(TCompilerOpt(Children[i]));
        if Assigned(Result) then Break;
      end;
    end
    else begin               // TCompilerOpt
      if aRoot.fId = aId then
        Result := aRoot;
    end;
  end;

begin
  Result := FindOptionSub(Self);
end;

function TCompilerOptGroup.OneCharOptions(aOptAndValue: string): TCompilerOpt;
// Split and select option characters like in -Criot.
// Returns reference to the last option object if all characters were valid opts.
var
  i: Integer;
  OptBase: String;
  List: TList;
begin
  List := TList.Create;
  try
    OptBase := Copy(aOptAndValue, 1, 2);
    // First check if all options are valid. Change them only if they are valid.
    for i := 3 to Length(aOptAndValue) do
    begin
      Result := FindOption(OptBase + aOptAndValue[i]);
      if Assigned(Result) then
        List.Add(Result)
      else
        Break;
    end;
    // Set boolean options but only if they all are valid.
    if Assigned(Result) then
      for i := 0 to List.Count-1 do
        TCompilerOpt(List[i]).SetValue('True', fOwnerReader.fCurOrigLine);
  finally
    List.Free;
  end;
end;

function TCompilerOptGroup.SelectOption(aOptAndValue: string): Boolean;
var
  Opt: TCompilerOpt;
  Param: string;
  OptLen, ParamLen: integer;
begin
  Opt := FindOption(aOptAndValue);
  if Assigned(Opt) then
  begin
    // Found. Set boolean option, other type of options are already set.
    if Opt.EditKind = oeBoolean then
      Opt.SetValue('True', fOwnerReader.fCurOrigLine);
  end
  else begin
    // Option was not found, try separating the parameter.
    // ToDo: figure out the length in a more clever way.
    if (Length(aOptAndValue) < 3) or (aOptAndValue[1] <> '-') then
      Exit(False);
    if aOptAndValue[2] in ['e', 'u', 'I', 'k', 'o'] then
      OptLen := 2
    else
      OptLen := 3;
    ParamLen := Length(aOptAndValue) - OptLen;
    Opt := Nil;
    if (ParamLen > 1)
    and (aOptAndValue[OptLen+1] in ['''', '"'])
    and (aOptAndValue[Length(aOptAndValue)] in ['''', '"']) then
      Param := Copy(aOptAndValue, OptLen+2, ParamLen-2) // Strip quotes
    else begin
      Param := Copy(aOptAndValue, OptLen+1, ParamLen);
      if OptLen = 3 then // Can contain one char options like -Criot. Can be combined.
        Opt := OneCharOptions(aOptAndValue);
    end;
    if Opt = Nil then
    begin
      Opt := FindOption(Copy(aOptAndValue, 1, OptLen));
      if Assigned(Opt) then
      begin
        Assert(Opt.Value='', 'TCompilerOptGroup.SelectOption: Opt.Value is already set.');
        Opt.SetValue(Param, fOwnerReader.fCurOrigLine)
      end;
    end;
  end;
  Result := Assigned(Opt);
end;

procedure TCompilerOptGroup.DeselectAll;

  procedure DeselectSub(aRoot: TCompilerOpt);
  var
    Children: TCompilerOptList;
    i: Integer;
  begin
    if aRoot is TCompilerOptGroup then
    begin
      Children := TCompilerOptGroup(aRoot).CompilerOpts;
      for i := 0 to Children.Count-1 do         // Recursive call for children.
        DeselectSub(TCompilerOpt(Children[i]));
    end
    else
      aRoot.SetValue('', -1);       // TCompilerOpt
  end;

begin
  DeselectSub(Self);
end;

procedure TCompilerOptGroup.ParseEditKind;
begin
  fEditKind := oeGroup;
end;

function TCompilerOptGroup.ParseOption(aDescr: string; aIndent: integer): Boolean;
var
  i: Integer;
begin
  Result := inherited ParseOption(aDescr, aIndent);
  if not Result then Exit;
  i := Length(fOption);
  fIncludeNegativeOpt := Copy(fOption, i-3, 4) = '[NO]';
  if fIncludeNegativeOpt then
    SetLength(fOption, i-4);
end;

{ TCompilerOptSet }

constructor TCompilerOptSet.Create(aOwnerReader: TCompilerOptReader;
  aOwnerGroup: TCompilerOptGroup; aCommonIndent: integer);
begin
  inherited Create(aOwnerReader, aOwnerGroup);
  fCommonIndent := aCommonIndent;
end;

destructor TCompilerOptSet.Destroy;
begin
  inherited Destroy;
end;

function TCompilerOptSet.CollectSelectedOptions(aUseComments: Boolean): string;
// Collect subitems of a set to one option.
var
  Opt: TCompilerOpt;
  i: Integer;
  s: string;
begin
  s := '';
  for i := 0 to fCompilerOpts.Count-1 do
  begin
    Opt := TCompilerOpt(fCompilerOpts[i]);
    if Opt.Value <> '' then
      case Opt.EditKind of
        oeSetElem  : s := s + Opt.Option;
        oeSetNumber: s := s + Opt.Value;
      end;
  end;
  if s <> '' then begin
    Result := Option + s;
    if aUseComments then
      Result := Result + Comment;
  end
  else
    Result := '';
end;

function TCompilerOptSet.SetNumberOpt(aValue: string): Boolean;
// Find a numeric value in the set and update its value. Return True on success.
var
  i: Integer;
  Opt: TCompilerOpt;
begin
  for i := 0 to fCompilerOpts.Count-1 do
  begin
    Opt := TCompilerOpt(fCompilerOpts[i]);
    if Opt.EditKind = oeSetNumber then
    begin
      Opt.SetValue(aValue, fOwnerReader.fCurOrigLine);
      Exit(True);           // Found and updated.
    end;
  end;
  Result := False;          // Not found.
end;

function TCompilerOptSet.SetBooleanOpt(aValue: string): Boolean;
// Find a single letter value in the set and update its value. Return True on success.
var
  i: Integer;
  Opt: TCompilerOpt;
begin
  for i := 0 to fCompilerOpts.Count-1 do
  begin
    Opt := TCompilerOpt(fCompilerOpts[i]);
    if (Opt.EditKind = oeSetElem) and (Opt.Option = aValue) then
    begin
      Opt.SetValue('True', fOwnerReader.fCurOrigLine);
      Exit(True);           // Found and updated.
    end;
  end;
  Result := False;          // Not found.
end;

procedure TCompilerOptSet.SelectOptions(aOptStr: string);
// Select options in this set based on the given characters.
var
  i, Start: Integer;
  OneOpt: string;
  OptOk: Boolean;
begin
  i := 1;
  while i <= Length(aOptStr) do
  begin
    Start := i;
    if aOptStr[i] in ['0'..'9'] then
      while (i <= Length(aOptStr)) and (aOptStr[i] in ['0'..'9']) do
        Inc(i)
    else
      Inc(i);
    OneOpt := Copy(aOptStr, Start, i-Start);
    if OneOpt[1] in ['0'..'9'] then
      OptOk := SetNumberOpt(OneOpt)
    else
      OptOk := False;
    if not (OptOk or SetBooleanOpt(OneOpt)) then
      raise Exception.CreateFmt('Option %s is not found in set %s.', [OneOpt, fOption]);
  end;
end;

procedure TCompilerOptSet.AddOptions(aDescr: string; aIndent: integer);
// Set can have one letter options and <n> for numbers

  procedure NewSetNumber(aDescr: string);
  var
    OptSet: TCompilerOpt;
  begin
    OptSet := TCompilerOpt.Create(Self);          // Add it under a group
    OptSet.fIndentation := aIndent;
    OptSet.fOption := 'Number';
    OptSet.fDescription := aDescr;
    OptSet.fEditKind := oeSetNumber;
  end;

  procedure NewSetElem(aDescr: string);
  var
    OptSet: TCompilerOpt;
  begin
    // Ignore -vl and -vs
    if (fOption = '-v') and (aDescr[1] in ['l', 's']) then Exit;
    OptSet := TCompilerOpt.Create(Self);          // Add it under a group
    OptSet.fIndentation := aIndent;
    OptSet.fOption := aDescr[1];
    OptSet.fDescription := Copy(aDescr, 2, Length(aDescr));
    OptSet.fEditKind := oeSetElem;
  end;

var
  Opt1, Opt2: string;
  i: Integer;
begin
  if AnsiStartsStr('<n>', aDescr) then
    NewSetNumber(aDescr)
  else begin
    i := PosEx(':', aDescr, 4);
    if (i > 0) and (aDescr[i-1]=' ') and (aDescr[i-2]<>' ') and (aDescr[i-3]=' ') then
    begin
      // Found another option on the same line, like ' a :'
      Opt2 := Copy(aDescr, i-2, Length(aDescr));
      if aDescr[3] = ':' then
        Opt1 := TrimRight(Copy(aDescr, 1, i-3))
      else
        Opt1 := '';
    end
    else begin
      Opt2 := '';
      Opt1 := aDescr;
    end;
    if Opt1 <> '' then         // Can be empty when line in help output is split.
      NewSetElem(Opt1)
    else if fCompilerOpts.Count > 0 then
      aIndent := TCompilerOpt(fCompilerOpts[0]).Indentation;
    if Opt2 <> '' then
      NewSetElem(Opt2);
  end;
end;

procedure TCompilerOptSet.ParseEditKind;
begin
  fEditKind := oeSet;
end;


{ TCompilerOptReader }

constructor TCompilerOptReader.Create;
begin
  inherited Create;
  fDefines := TStringList.Create;
  fInvalidOptions := TStringList.Create;
  fSupportedCategories := TStringList.Create;
  fSupportedCategories.Sorted := True;
  fGeneratedOptions := TStringList.Create;
  fRootOptGroup := TCompilerOptGroup.Create(Self, Nil);
end;

destructor TCompilerOptReader.Destroy;
begin
  Clear;
  fRootOptGroup.Free;
  fGeneratedOptions.Free;
  fSupportedCategories.Free;
  fInvalidOptions.Free;
  fDefines.Free;
  inherited Destroy;
end;

procedure TCompilerOptReader.Clear;
var
  i: Integer;
begin
  fRootOptGroup.Clear;
  for i := 0 to fSupportedCategories.Count-1 do
    fSupportedCategories.Objects[i].Free;
  fSupportedCategories.Clear;
end;

function TCompilerOptReader.AddChoicesNew(aOpt: string): TStrings;
// From FPC 2.7.1+ output
const
  FpcIStart = 'see fpc -i or fpc -i';
var
  ch: Char;
  i: integer;
begin
  Result := Nil;
  i := Pos(FpcIStart, aOpt);
  if i = 0 then Exit;
  Assert(Length(aOpt) >= i+Length(FpcIStart));
  ch := aOpt[i+Length(FpcIStart)]; // Pick the next char from description.
  if fSupportedCategories.Find(ch, i) then
    Result := fSupportedCategories.Objects[i] as TStrings
  else begin
    Result := ReadCategorySelections(ch);
    Result.Insert(0, ''); // First an empty string. Allows removing selection.
    fSupportedCategories.AddObject(ch, Result);
  end;
end;

function TCompilerOptReader.IsGroup(aOpt: string; var aCategoryList: TStrings): Boolean;
// This option should be a group instead of a selection list.
// The information is not available in fpc -h output.
var
  i: Integer;
  CategoryName: string;
begin
  Result := False;
  if fIsNewFpc then
  begin
    // FPC 2.7.1+
    if AnsiStartsStr('-Oo', aOpt)
    or AnsiStartsStr('-OW', aOpt)
    or AnsiStartsStr('-Ow', aOpt) then
    begin
      aCategoryList := AddChoicesNew(aOpt);
      Result := Assigned(aCategoryList);
    end;
  end
  else begin
    // FPC 2.6.x
    CategoryName := '';
    if AnsiStartsStr('-Oo', aOpt) then
      CategoryName := 'Optimizations:'
    else if AnsiStartsStr('-OW', aOpt) or AnsiStartsStr('-Ow', aOpt) then
      CategoryName := 'Whole Program Optimizations:';
    Result := CategoryName <> '';
    if Result then
      if fSupportedCategories.Find(CategoryName, i) then
        aCategoryList := fSupportedCategories.Objects[i] as TStrings
      else
        raise Exception.CreateFmt('No list of options found for "%s".', [CategoryName]);
  end;
end;

function TCompilerOptReader.AddNewCategory(aCategoryName: String): TStringList;
begin
  Result := TStringList.Create;
  Result.Add('');      // First an empty string. Allows removing selection.
  fSupportedCategories.AddObject(aCategoryName, Result);
end;

function TCompilerOptReader.ParseI(aLines: TStringList): TModalResult;
const
  Supported = 'Supported ';
var
  i, j: Integer;
  Line, TrimmedLine: String;
  Category, sl: TStringList;
begin
  Result := mrOK;
  Category := Nil;
  sl := TStringList.Create;
  try
    sl.StrictDelimiter := True;
    sl.Delimiter := ',';
    for i := 0 to aLines.Count-1 do
    begin
      Line := aLines[i];
      TrimmedLine := Trim(Line);
      if Assigned(Category) then
      begin
        if TrimmedLine = '' then
          Category := Nil             // End of category.
        else begin
          if Line[1] <> ' ' then
            raise Exception.Create('TCompilerReader.ParseI: Line should start with a space.');
          sl.Clear;
          // Some old FPC versions had a comma separated list.
          sl.DelimitedText := Trim(Line);
          for j := 0 to sl.Count-1 do
            Category.Add(sl[j]);
        end;
      end
      else if AnsiStartsStr(Supported, Line) then
        Category := AddNewCategory(Copy(Line, Length(Supported)+1, Length(Line)));
    end;
  finally
    sl.Free;
  end;
end;

function TCompilerOptReader.ReadVersion(s: string): Boolean;
const
  VersBegin = 'Free Pascal Compiler version ';
var
  Start, V1, V2: Integer;
  OutputI: TStringList;      // fpc -Fr$(FPCMsgFile) -i
begin
  Result := AnsiStartsStr(VersBegin, s);
  if Result then
  begin
    fIsNewFpc := False;
    Start := Length(VersBegin)+1;
    V1 := PosEx(' ', s, Start);
    if V1 > 0 then
    begin
      fFpcVersion := Copy(s, Start, V1-Start);
      if (Length(fFpcVersion)>2) then begin
        V1 := StrToIntDef(fFpcVersion[1], 0);
        V2 := StrToIntDef(fFpcVersion[3], 0);
        fIsNewFpc := ((V1=2) and (V2>=7)) or (V1>2);
      end;
      // The rest 2 fields are date and target CPU.
    end;
    if not fIsNewFpc then
    begin
      // Get categories with FPC -i, once we know the version is old (2.6.x).
      OutputI := RunTool(fCompilerExecutable, fParsedTarget + ' -i');
      if OutputI = Nil then Exit(False);
      try
        Result := ParseI(OutputI) = mrOK;
      finally
        OutputI.Free;
      end;
    end;
  end;
end;

procedure TCompilerOptReader.CreateNewGroupItem(aGroup: TCompilerOptGroup; aTxt: string);
var
  Opt: TCompilerOpt;
begin
  Opt := TCompilerOpt.Create(aGroup);  // Add it under a group
  Opt.fOption := aGroup.Option + aTxt;
  Opt.fIndentation := aGroup.Indentation+4;
  Opt.fEditKind := oeBoolean;
end;

procedure TCompilerOptReader.AddGroupItems(aGroup: TCompilerOptGroup; aItems: TStrings);
var
  i: Integer;
begin
  for i := 1 to aItems.Count-1 do        // Skip the first empty item.
  begin
    CreateNewGroupItem(aGroup, aItems[i]);
    if aGroup.fIncludeNegativeOpt then
      CreateNewGroupItem(aGroup, 'NO'+aItems[i]);
  end;
end;

function TCompilerOptReader.ParseH(aLines: TStringList): TModalResult;
const
  OptSetId = 'a combination of';
var
  i, ThisInd, NextInd, OptSetInd: Integer;
  ThisLine: String;
  Opt: TCompilerOpt;
  LastGroup, SubGroup: TCompilerOptGroup;
  GroupItems: TStrings;
begin
  Result := mrOK;
  LastGroup := fRootOptGroup;
  GroupItems:=nil;
  for i := 0 to aLines.Count-1 do
  begin
    ThisLine := StringReplace(aLines[i],'-Agas-darwinAssemble','-Agas-darwin Assemble',[]);
    ThisInd := CalcIndentation(ThisLine);
    ThisLine := Trim(ThisLine);
    if LastGroup is TCompilerOptSet then
    begin                  // Fix strangely split line indents in options groups.
      OptSetInd := TCompilerOptSet(LastGroup).CommonIndent;
      if (ThisLine[1] <> '-') and (ThisInd > OptSetInd) then
        ThisInd := OptSetInd;
    end;
    // Top header line for compiler version, check only once.
    if (fFpcVersion = '') and ReadVersion(ThisLine) then Continue;
    if ThisInd < 2 then Continue;
    if (ThisLine = '') or (ThisInd > 30)
    or (ThisLine[1] = '@')
    or (Pos('-? ', ThisLine) > 0)
    or (Pos('-h ', ThisLine) > 0) then Continue;
    if i < aLines.Count-1 then
      NextInd := CalcIndentation(aLines[i+1])
    else
      NextInd := -1;
    if NextInd > ThisInd then
    begin
      if LastGroup is TCompilerOptSet then
        NextInd := TCompilerOptSet(LastGroup).CommonIndent
      else begin
        if Pos(OptSetId, ThisLine) > 0 then       // Header for sets
          // Hard-code indent to NextInd, for strangely split lines later in help output.
          LastGroup := TCompilerOptSet.Create(Self, LastGroup, NextInd)
        else                                      // Group header for options
          LastGroup := TCompilerOptGroup.Create(Self, LastGroup);
        LastGroup.ParseOption(ThisLine, ThisInd);
      end;
    end;
    if NextInd <= ThisInd then
    begin
      // This is an option
      if LastGroup is TCompilerOptSet then      // Add it to a set (may add many)
        TCompilerOptSet(LastGroup).AddOptions(ThisLine, ThisInd)
      else begin
        if IsGroup(ThisLine, GroupItems) then
        begin
          SubGroup := TCompilerOptGroup.Create(Self, LastGroup);
          SubGroup.ParseOption(ThisLine, ThisInd);
          AddGroupItems(SubGroup, GroupItems);
        end
        else begin
          Opt := TCompilerOpt.Create(LastGroup);  // Add it under a group
          Opt.ParseOption(ThisLine, ThisInd);
        end;
      end;
      if (NextInd <> -1) and (NextInd < ThisInd) then
        LastGroup := LastGroup.fOwnerGroup;       // Return to a previous group
    end;
  end;
end;

function TCompilerOptReader.UpdateTargetParam: Boolean;
// Updates target OS and CPU parameter using global macros.
// Returns true if the value has changed since last time.
var
  NewTarget: string;
begin
  NewTarget := '-T$(TargetOS) -P$(TargetCPU)';
  if not GlobalMacroList.SubstituteStr(NewTarget) then
    raise Exception.CreateFmt('UpdateTargetParam: Cannot substitute macros "%s".',
                              [NewTarget]);
  Result := fParsedTarget <> NewTarget;
  if Result then
    fParsedTarget := NewTarget;      // fParsedTarget is used as a param for FPC.
end;

function TCompilerOptReader.ReadCategorySelections(aChar: Char): TStringList;
// Get the selection list for a category using "fpc -i+char", for new FPC versions.
begin
  Result:=RunTool(fCompilerExecutable, fParsedTarget + ' -i' + aChar);
  Result.Sort;
end;

function TCompilerOptReader.ReadAndParseOptions: TModalResult;
// fpc -Fr$(FPCMsgFile) -h
var
  OutputH: TStringList;
begin
  if fCompilerExecutable = '' then
    fCompilerExecutable := 'fpc';        // Let's hope "fpc" is found in PATH.
  OptionIdCounter := 0;
  fErrorMsg := '';
  try
    // FPC with option -h
    OutputH := RunTool(fCompilerExecutable, fParsedTarget + ' -h');
    if OutputH = Nil then Exit(mrCancel);
    Result := ParseH(OutputH);
  finally
    OutputH.Free;
  end;
end;

function TCompilerOptReader.FilterOptions(aFilter: string; aOnlySelected: Boolean): Boolean;
// Filter all options recursively, setting their Visible flag as needed.
// Returns True if Option(group) or child options have visible items.

  function FilterOptionsSub(aRoot: TCompilerOpt): Boolean;
  var
    Children: TCompilerOptList;
    i: Integer;
  begin
    // Filter the root item
    aRoot.Filter(aFilter, aOnlySelected);         // Sets Visible flag
    // Filter children in a group
    if aRoot is TCompilerOptGroup then
    begin
      Children := TCompilerOptGroup(aRoot).CompilerOpts;
      for i := 0 to Children.Count-1 do           // Recursive call for children.
        aRoot.Visible := FilterOptionsSub(TCompilerOpt(Children[i])) or aRoot.Visible;
    end;
    Result := aRoot.Visible;
  end;

begin
  Result := FilterOptionsSub(fRootOptGroup);
end;

function TCompilerOptReader.FindOptionById(aId: integer): TCompilerOpt;
begin
  Result := fRootOptGroup.FindOptionById(aId);
end;

function TCompilerOptReader.FromCustomOptions(aStrings: TStrings): TModalResult;
// Example:  $(IDEBuildOptions) -dCR -dgc -Criot
var
  i, j: Integer;
  s: String;
  sl: TStringList;
begin
  Result := mrOK;
  fCurOrigLine := 0;
  fRootOptGroup.DeselectAll;
  fDefines.Clear;
  fInvalidOptions.Clear;
  sl := TStringList.Create;
  try
    // Separate options that are on one line.
    for i := 0 to aStrings.Count-1 do
    begin
      s := Trim(aStrings[i]);
      if s = '' then Continue;
      sl.Clear;
      SplitCmdLineParams(s, sl);
      for j := 0 to sl.Count-1 do begin
        s := sl[j];
        // Put the option into fDefines or fInvalidOptions, or set in options collection.
        if AnsiStartsStr('-d', s) and (Length(s) > 2) then
        begin
          if not AnsiStartsStr(CommentId, s) then    // Skip a generated comment.
            fDefines.Add(s)
        end
        else
          if not fRootOptGroup.SelectOption(s) then
            fInvalidOptions.AddObject(s, TObject({%H-}Pointer(PtrUInt(i))));
        Inc(fCurOrigLine);
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure TCompilerOptReader.CopyOptions(aRoot: TCompilerOpt);
// Collect non-default options from GUI to fGeneratedOptions
var
  Children: TCompilerOptList;
  i: Integer;
  s: string;
begin
  if aRoot is TCompilerOptGroup then
  begin
    Children := TCompilerOptGroup(aRoot).CompilerOpts;
    if aRoot is TCompilerOptSet then
    begin                                       // TCompilerOptSet
      s := TCompilerOptSet(aRoot).CollectSelectedOptions(fUseComments);
      if s <> '' then
        fGeneratedOptions.AddObject(s, TObject({%H-}Pointer(PtrUInt(aRoot.fOrigLine))));
    end
    else begin                                  // TCompilerOptGroup
      for i := 0 to Children.Count-1 do
        CopyOptions(TCompilerOpt(Children[i])); // Recursive call for children.
    end;
  end
  else if aRoot.Value <> '' then                // TCompilerOpt
    fGeneratedOptions.AddObject(aRoot.GenerateOptValue(fUseComments),
                                TObject({%H-}Pointer(PtrUINt(aRoot.fOrigLine))));
end;

function TCompilerOptReader.FindLowestOrigLine(aStrings: TStrings;
                                               out aOrigLine: Integer): integer;
// Return index in aStrings for an option that has the lowest original line number.
// aOrigLine returns the original line number.
var
  i, OriLine, MinOrigLine: Integer;
begin
  Result := -1;
  aOrigLine := -1;
  MinOrigLine := MaxInt;
  for i := 0 to aStrings.Count-1 do
  begin
    OriLine := Integer({%H-}PtrUInt(aStrings.Objects[i]));
    if (OriLine > -1) and (OriLine < MinOrigLine) then
    begin
      MinOrigLine := OriLine;
      aOrigLine := OriLine;
      Result := i;
    end;
  end;
end;

function TCompilerOptReader.AddOptInLowestOrigLine(OutStrings: TStrings): Boolean;
// Copy an option that had the lowest original line number.
// Returns True if options from original data was found.
var
  iGen, iInv: Integer;
  iGenOrig, iInvOrig: Integer;
begin
  // Find lowest lines from both generated and invalid options
  iGen := FindLowestOrigLine(fGeneratedOptions, iGenOrig);
  iInv := FindLowestOrigLine(fInvalidOptions, iInvOrig);
  // then add the one that is lower.
  if (iGenOrig = -1) and (iInvOrig = -1) then Exit(False);
  Result := True;
  if ( (iGenOrig > -1) and (iInvOrig > -1) and (iGenOrig <= iInvOrig) )
  or ( (iGenOrig > -1) and (iInvOrig = -1) ) then
  begin
    OutStrings.Add(fGeneratedOptions[iGen]);
    fGeneratedOptions[iGen] := '';
    fGeneratedOptions.Objects[iGen] := TObject(Pointer(-1)); // Mark as processed.
  end
  else begin
    OutStrings.Add(fInvalidOptions[iInv]);
    fInvalidOptions[iInv] := '';
    fInvalidOptions.Objects[iInv] := TObject(Pointer(-1));
  end;
end;

function TCompilerOptReader.ToCustomOptions(aStrings: TStrings;
  aUseComments: Boolean): TModalResult;
// Copy options to a list if they have a non-default value (True for boolean).
var
  i: Integer;
begin
  Result := mrOK;
  fUseComments := aUseComments;
  fGeneratedOptions.Clear;
  CopyOptions(fRootOptGroup);
  // Options are now in fGeneratedOptions. Move them to aStrings in a right order.
  aStrings.Clear;
  // First collect options that were in the original list.
  while AddOptInLowestOrigLine(aStrings) do ;
  // Then add all the rest.
  for i := 0 to fGeneratedOptions.Count-1 do
    if fGeneratedOptions[i] <> '' then
      aStrings.Add(fGeneratedOptions[i]);
  // Then defines
  aStrings.AddStrings(fDefines);
end;

{ TCompilerOptThread }

constructor TCompilerOptThread.Create(aReader: TCompilerOptReader);
begin
  inherited Create(True);
  //FreeOnTerminate:=True;
  fStartedOnce:=false;
  fReader:=aReader;
end;

destructor TCompilerOptThread.Destroy;
begin
  if fStartedOnce then
    WaitFor;
  Clear;
  inherited Destroy;
end;

function TCompilerOptThread.GetErrorMsg: string;
begin
  Result := fReader.ErrorMsg;
end;

procedure TCompilerOptThread.Clear;
begin
  ;
end;

procedure TCompilerOptThread.StartParsing;
begin
  if fStartedOnce then
    WaitFor;
  fReader.CompilerExecutable:=LazarusIDE.GetCompilerFilename;
  fReader.UpdateTargetParam;
  Start;
  fStartedOnce:=true;
end;

procedure TCompilerOptThread.EndParsing;
begin
  if fStartedOnce then
    WaitFor;
end;

procedure TCompilerOptThread.Execute;
var
  StartTime: TDateTime;
begin
  StartTime := Now;
  try
    fReader.ReadAndParseOptions;
  except
    on E: Exception do
      fReader.ErrorMsg := 'Error reading compiler: '+E.Message;
  end;
  fReadTime := Now-StartTime;
end;


end.

