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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit Compiler;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Process, LCLProc, Forms, Controls, contnrs, strutils, FileUtil,
  LazarusIDEStrConsts, CompilerOptions, Project,
  {$IFDEF EnableNewExtTools}
  IDEExternToolIntf,
  {$ELSE}
  OutputFilter,
  {$ENDIF}
  UTF8Process, InfoBuild, IDEMsgIntf, CompOptsIntf,
  DefineTemplates, TransferMacros, EnvironmentOpts, LazFileUtils;

type
  TOnCmdLineCreate = procedure(var CmdLine: string; var Abort:boolean) of object;

  { TCompiler }

  TCompiler = class(TObject)
  private
    FOnCmdLineCreate : TOnCmdLineCreate;
    {$IFNDEF EnableNewExtTools}
    FOutputFilter: TOutputFilter;
    FTheProcess: TProcessUTF8;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(AProject: TProject;
                     const WorkingDir, CompilerFilename, CompilerParams: string;
                     BuildAll, SkipLinking, SkipAssembler: boolean
                    ): TModalResult;
    procedure WriteError(const Msg: string);
    {$IFNDEF EnableNewExtTools}
    property OutputFilter: TOutputFilter read FOutputFilter write FOutputFilter;
    property TheProcess: TProcessUTF8 read FTheProcess;
    {$ENDIF}
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
    procedure AddChoices(aCategory: string);
    function Comment: string;
    procedure Filter(aFilter: string; aOnlySelected: Boolean);
    function GenerateOptValue(aUseComments: Boolean): string;
    procedure SetValue(aValue: string; aOrigLine: integer);
  protected
    procedure ParseEditKind; virtual;
    procedure ParseOption(aDescr: string; aIndent: integer); virtual;
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
    procedure ParseOption(aDescr: string; aIndent: integer); override;
  public
    constructor Create(aOwnerReader: TCompilerOptReader; aOwnerGroup: TCompilerOptGroup);
    destructor Destroy; override;
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
    function SetNumberOpt(aValue: string): Boolean;
    function SetBooleanOpt(aValue: string): Boolean;
  protected
    procedure AddOptions(aDescr: string; aIndent: integer);
    procedure ParseEditKind; override;
  public
    constructor Create(aOwnerReader: TCompilerOptReader; aOwnerGroup: TCompilerOptGroup);
    destructor Destroy; override;
    function CollectSelectedOptions(aUseComments: Boolean): string;
    procedure SelectOptions(aOptStr: string);
  end;

  { TCompilerOptReader }

  TCompilerOptReader = class
  private
    fCurOrigLine: integer;        // Current line num when parsing original data.
    // Defines (-d...) are separated from custom options and stored here.
    fDefines: TStringList;
    // Options not accepted by parser. They may still be valid (a macro maybe)
    fInvalidOptions: TStringList;        // and will be included in output.
    // Lists of selections parsed from "fpc -i". Contains supported technologies.
    fSupportedCategories: TStringList;
    // Hierarchy of options parsed from "fpc -h".
    fRootOptGroup: TCompilerOptGroup;
    fCompilerExecutable: string;  // Compiler path must be set by caller.
    fCompilerVersion: string;     // Parsed from "fpc -h".
    fErrorMsg: String;
    fGenStrings: TStringList;     // Options generated from GUI.
    fUseComments: Boolean;        // Add option's description into generated data.
    function AddOptInLowestOrigLine(OutStrings: TStrings): Boolean;
    procedure CopyOptions(aRoot: TCompilerOpt);
    function FindLowestOrigLine(aStrings: TStrings; out aOrigLine: Integer): integer;
    procedure ReadVersion(s: string);
    procedure CreateNewGroupItem(aGroup: TCompilerOptGroup; aTxt: string);
    procedure AddGroupItems(aGroup: TCompilerOptGroup; aItems: TStrings);
    function ParseI(aLines: TStringList): TModalResult;
    function ParseH(aLines: TStringList): TModalResult;
  public
    constructor Create;
    destructor Destroy; override;
    function ReadAndParseOptions: TModalResult;
    function FilterOptions(aFilter: string; aOnlySelected: Boolean): Boolean;
    function FindOptionById(aId: integer): TCompilerOpt;
    function FromCustomOptions(aStrings: TStrings): TModalResult;
    function ToCustomOptions(aStrings: TStrings; aUseComments: Boolean): TModalResult;
  public
    property Defines: TStringList read fDefines;
    property SupportedCategories: TStringList read fSupportedCategories;
    property RootOptGroup: TCompilerOptGroup read fRootOptGroup;
    property CompilerExecutable: string read fCompilerExecutable write fCompilerExecutable;
    property ErrorMsg: String read fErrorMsg write fErrorMsg;
  end;

  { TCompilerOptThread }

  TCompilerOptThread = class(TThread)
  private
    fReader: TCompilerOptReader;
    fReadTime: TDateTime;
    function GetErrorMsg: string;
  protected
    procedure Execute; override;
  public
    constructor Create(aReader: TCompilerOptReader);
    destructor Destroy; override;
  public
    property ReadTime: TDateTime read fReadTime;
    property ErrorMsg: string read GetErrorMsg;
  end;


implementation

var
  CurrentCategories: TStringList;    // To pass categories to options parser.

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
  {$IFNDEF EnableNewExtTools}
  FreeAndNil(FTheProcess);
  {$ENDIF}
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TCompiler Compile
------------------------------------------------------------------------------}
function TCompiler.Compile(AProject: TProject;
  const WorkingDir, CompilerFilename, CompilerParams: string;
  BuildAll, SkipLinking, SkipAssembler: boolean
  ): TModalResult;
var
  CmdLine : String;
  Abort : Boolean;
  {$IFDEF EnableNewExtTools}
  Tool: TAbstractExternalTool;
  {$ENDIF}
begin
  Result:=mrCancel;
  if ConsoleVerbosity>=0 then
    DebugLn('TCompiler.Compile WorkingDir="',WorkingDir,'" CompilerFilename="',CompilerFilename,'" CompilerParams="',CompilerParams,'"');

  // if we want to show the compile progress, it's now time to show the dialog
  CompileProgress.Show;

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

  {$IFDEF EnableNewExtTools}
  Tool:=ExternalToolList.Add('Compling Project');
  Tool.Process.Executable:=CompilerFilename;
  Tool.CmdLineParams:=CmdLine;
  Tool.Process.CurrentDirectory:=WorkingDir;
  Tool.AddParsers(SubToolFPC);
  Tool.AddParsers(SubToolMake);
  Tool.Execute;
  Tool.WaitForExit;
  if Tool.ErrorMessage='' then
    Result:=mrOK;
  {$ELSE}
  try
    if TheProcess=nil then
      FTheProcess := TOutputFilterProcess.Create(nil);
    TheProcess.CommandLine := CompilerFilename+CmdLine;
    TheProcess.Options:= [poUsePipes, poStdErrToOutput];
    TheProcess.ShowWindow := swoHide;
    Result:=mrOk;
    try
      TheProcess.CurrentDirectory:=WorkingDir;

      if OutputFilter<>nil then begin
        if BuildAll and Assigned(IDEMessagesWindow) then
          IDEMessagesWindow.AddMsg(lisOptionsChangedRecompilingCleanWithB,
            WorkingDir, -1);
        OutputFilter.Options:=[ofoSearchForFPCMessages,ofoExceptionOnError];
        OutputFilter.CompilerOptions:=AProject.CompilerOptions;
        if not OutputFilter.Execute(TheProcess,Self) then
          if OutputFilter.Aborted then
            Result := mrAbort
          else
            Result := mrCancel;
      end else begin
        TheProcess.Execute;
      end;
    finally
      if TheProcess.Running
      then begin
        TheProcess.WaitOnExit;
        if not (TheProcess.ExitStatus in [0,1]) then  begin
          WriteError(Format(listCompilerInternalError,[TheProcess.ExitStatus]));
          Result:=mrCancel;
        end;
      end;
    end;
  except
    on e: EOutputFilterError do begin
      Result:=mrCancel;
      exit;
    end;
    on e: Exception do begin
      if ConsoleVerbosity>=-1 then
        DebugLn('[TCompiler.Compile] exception "',E.Message,'"');
      WriteError(E.Message);
      Result:=mrCancel;
      exit;
    end;
  end;
  {$ENDIF}
  if ConsoleVerbosity>=0 then
    DebugLn('[TCompiler.Compile] end');
end;

procedure TCompiler.WriteError(const Msg: string);
begin
  DebugLn('TCompiler.WriteError ',Msg);
  {$IFDEF EnableNewExtTools}
  if IDEMessagesWindow<>nil then
    IDEMessagesWindow.AddCustomMessage(mluError,Msg);
  {$ELSE}
  if OutputFilter <> nil then
    OutputFilter.ReadConstLine(Msg, True);
  {$ENDIF}
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

function IsGroup(aOpt: string; var aCategoryList: TStrings): Boolean;
// This option should be a group instead of a selection list.
var
  i: Integer;
  Category: string;
begin
  if AnsiStartsStr('-Oo', aOpt) then
    Category := 'Optimizations:'
  else if AnsiStartsStr('-OW', aOpt) or AnsiStartsStr('-Ow', aOpt) then
    Category := 'Whole Program Optimizations:'
  ;
  Result := Category <> '';
  if Result then
    if CurrentCategories.Find(Category, i) then
      aCategoryList := CurrentCategories.Objects[i] as TStrings
    else
      raise Exception.CreateFmt('No list of options found for "%s".', [Category]);
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

procedure TCompilerOpt.AddChoices(aCategory: string);
// Add selection choices for this option. Data originates from "fpc -i".
var
  i: Integer;
begin
  if CurrentCategories.Find(aCategory, i) then
    fChoices := CurrentCategories.Objects[i] as TStrings
  else
    raise Exception.CreateFmt('No selection list for "%s" found.', [aCategory]);
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
  if Pos('fpc -i', fDescription) > 0 then
  begin
    fEditKind := oeList;                   // Values will be got later.
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
end;

procedure TCompilerOpt.ParseOption(aDescr: string; aIndent: integer);
var
  i: Integer;
begin
  fIndentation := aIndent;
  // Separate the actual option and description from each other
  if aDescr[1] <> '-' then
    raise Exception.Create('Option description does not start with "-"');
  i := 1;
  while (i < Length(aDescr)) and (aDescr[i] <> ' ') do
    Inc(i);
  fOption := Copy(aDescr, 1, i-1);
  while (i < Length(aDescr)) and (aDescr[i] = ' ') do
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
        Result := aRoot;
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
    // Set boolean options if they all are valid.
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
    Opt.SetValue('True', fOwnerReader.fCurOrigLine)
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
    if Opt = Nil then begin
      Opt := FindOption(Copy(aOptAndValue, 1, OptLen));
      if Assigned(Opt) then begin
        if Opt.fEditKind in [oeNumber,oeText] then
          Opt.SetValue(Param, fOwnerReader.fCurOrigLine)
        else
          Opt := Nil;
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

procedure TCompilerOptGroup.ParseOption(aDescr: string; aIndent: integer);
var
  i: Integer;
begin
  inherited ParseOption(aDescr, aIndent);
  i := Length(fOption);
  fIncludeNegativeOpt := Copy(fOption, i-3, 4) = '[NO]';
  if fIncludeNegativeOpt then
    SetLength(fOption, i-4);
end;

{ TCompilerOptSet }

constructor TCompilerOptSet.Create(aOwnerReader: TCompilerOptReader; aOwnerGroup: TCompilerOptGroup);
begin
  inherited Create(aOwnerReader, aOwnerGroup);
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
    begin
      if not SetNumberOpt(OneOpt) then
        raise Exception.CreateFmt('Numeric value is not allowed for set %s.', [fOption]);
    end
    else begin
      if not SetBooleanOpt(OneOpt) then
        raise Exception.CreateFmt('Option %s is not found in set %s.', [OneOpt, fOption]);
    end;
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
    else
      Opt1 := aDescr;
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
  fGenStrings := TStringList.Create;
  fRootOptGroup := TCompilerOptGroup.Create(Self, Nil);
  // Categories are passed to options parser through a global variable.
  CurrentCategories := fSupportedCategories;
end;

destructor TCompilerOptReader.Destroy;
var
  i: Integer;
begin
  fRootOptGroup.Free;
  fGenStrings.Free;
  for i := 0 to fSupportedCategories.Count-1 do
    fSupportedCategories.Objects[i].Free;
  fSupportedCategories.Free;
  fInvalidOptions.Free;
  fDefines.Free;
  inherited Destroy;
end;

function TCompilerOptReader.ParseI(aLines: TStringList): TModalResult;
const
  Supported = 'Supported ';
var
  i, j: Integer;
  s, Line, TrimmedLine: String;
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
          sl.DelimitedText := Trim(Line);
          for j := 0 to sl.Count-1 do
            Category.Add(sl[j]);
        end;
      end
      else if AnsiStartsStr(Supported, Line) then
      begin
        Category := TStringList.Create;
        Category.Add('');      // First an empty string. Allows removing selection.
        s := Copy(Line, Length(Supported)+1, Length(Line));
        fSupportedCategories.AddObject(s, Category);
      end;
    end;
    fSupportedCategories.Sorted := True;
  finally
    sl.Free;
  end;
end;

procedure TCompilerOptReader.ReadVersion(s: string);
const
  VersBegin = 'Free Pascal Compiler version ';
var
  i, Start: Integer;
begin
  if AnsiStartsStr(VersBegin, s) then
  begin
    Start := Length(VersBegin);
    i := PosEx(' ', s, Start+1);
    if i > 0 then
      fCompilerVersion := Copy(s, Start, i-Start);
      // ToDo: the rest 2 fields are date and target CPU.
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
  i, ThisInd, NextInd: Integer;
  ThisLine, NextLine: String;
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
    if ThisInd = 0 then
    begin
      ReadVersion(ThisLine);        // Top header lines for compiler version etc.
      Continue;
    end;
    if (ThisLine = '') or (ThisInd > 30)
    or (Pos('-? ', ThisLine) > 0)
    or (Pos('-h ', ThisLine) > 0) then Continue;

    if i < aLines.Count-1 then begin
      NextLine := aLines[i+1];
      NextInd := CalcIndentation(aLines[i+1]);
    end
    else begin
      NextLine := '';
      NextInd := -1;
    end;
    if NextInd > ThisInd then
    begin
      if (LastGroup is TCompilerOptSet)
      and ((Pos('  v : ', NextLine) > 0) or (NextInd > 30)) then
        // A hack to deal with split lined in the help output.
        NextInd := ThisInd
      else begin
        if Pos(OptSetId, ThisLine) > 0 then       // Header for sets
          LastGroup := TCompilerOptSet.Create(Self, LastGroup)
        else                                      // Group header for options
          LastGroup := TCompilerOptGroup.Create(Self, LastGroup);
        LastGroup.ParseOption(ThisLine, ThisInd);
      end;
    end;
    if NextInd <= ThisInd then
    begin
      // This is an option
      if (LastGroup is TCompilerOptSet) then      // Add it to a set (may add many)
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

function TCompilerOptReader.ReadAndParseOptions: TModalResult;
// fpc -Fr$(FPCMsgFile) -h
// fpc -Fr$(FPCMsgFile) -i
var
  Lines: TStringList;
  ParsedTarget: String;
begin
  OptionIdCounter := 0;
  fErrorMsg := '';
  if fCompilerExecutable = '' then
    fCompilerExecutable := 'fpc';        // Let's hope "fpc" is found in PATH.
  ParsedTarget := '-T$(TargetOS) -P$(TargetCPU)';
  if not GlobalMacroList.SubstituteStr(ParsedTarget) then
    raise Exception.CreateFmt('ReadAndParseOptions: Cannot substitute macros "%s".',
                              [ParsedTarget]);
  // FPC with option -i
  Lines:=RunTool(fCompilerExecutable, ParsedTarget + ' -i');
  try
    if Lines = Nil then Exit(mrCancel);
    Result := ParseI(Lines);
    if Result <> mrOK then Exit;
  finally
    Lines.Free;
  end;
  // FPC with option -h
  Lines:=RunTool(fCompilerExecutable, ParsedTarget + ' -h');
  try
    if Lines = Nil then Exit(mrCancel);
    Result := ParseH(Lines);
  finally
    Lines.Free;
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
            fInvalidOptions.AddObject(s, TObject(Pointer(i)));
        Inc(fCurOrigLine);
      end;
    end;
  finally
    sl.Free;
  end;
end;

procedure TCompilerOptReader.CopyOptions(aRoot: TCompilerOpt);
// Collect non-default options to fGenStrings
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
        fGenStrings.AddObject(s, TObject(Pointer(aRoot.fOrigLine)));
    end
    else begin                                  // TCompilerOptGroup
      for i := 0 to Children.Count-1 do
        CopyOptions(TCompilerOpt(Children[i])); // Recursive call for children.
    end;
  end
  else if aRoot.Value <> '' then                // TCompilerOpt
    fGenStrings.AddObject(aRoot.GenerateOptValue(fUseComments),
                          TObject(Pointer(aRoot.fOrigLine)));
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
    OriLine := Integer(Pointer(aStrings.Objects[i]));
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
  iGen := FindLowestOrigLine(fGenStrings, iGenOrig);
  iInv := FindLowestOrigLine(fInvalidOptions, iInvOrig);
  // then add the one that is lower.
  if (iGenOrig = -1) and (iInvOrig = -1) then Exit(False);
  Result := True;
  if ( (iGenOrig > -1) and (iInvOrig > -1) and (iGenOrig <= iInvOrig) )
  or ( (iGenOrig > -1) and (iInvOrig = -1) ) then
  begin
    OutStrings.Add(fGenStrings[iGen]);
    fGenStrings[iGen] := '';
    fGenStrings.Objects[iGen] := TObject(Pointer(-1)); // Mark as processed.
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
  fGenStrings.Clear;
  CopyOptions(fRootOptGroup);
  // Options are now in fGenStrings. Move them to aStrings in a right order.
  aStrings.Clear;
  // First collect options that were in the original list.
  while AddOptInLowestOrigLine(aStrings) do ;
  // Then add all the rest.
  for i := 0 to fGenStrings.Count-1 do
    if fGenStrings[i] <> '' then
      aStrings.Add(fGenStrings[i]);
  // Then defines
  aStrings.AddStrings(fDefines);
end;

{ TCompilerOptThread }

constructor TCompilerOptThread.Create(aReader: TCompilerOptReader);
begin
  inherited Create(True);
  //FreeOnTerminate:=True;
  fReader:=aReader;
end;

destructor TCompilerOptThread.Destroy;
begin
  inherited Destroy;
end;

function TCompilerOptThread.GetErrorMsg: string;
begin
  Result := fReader.ErrorMsg;
end;

procedure TCompilerOptThread.Execute;
var
  StartTime: TDateTime;
begin
  StartTime := Now;
  try
    fReader.CompilerExecutable := EnvironmentOptions.GetParsedCompilerFilename;
    fReader.ReadAndParseOptions;
  except
    on E: Exception do
      fReader.ErrorMsg := 'Error parsing options: '+E.Message;
  end;
  fReadTime := Now-StartTime;
end;


end.

