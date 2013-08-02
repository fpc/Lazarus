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
  {$ELSE}
  OutputFilter,
  {$ENDIF}
  UTF8Process, InfoBuild, IDEMsgIntf, CompOptsIntf;

type
  TOnCmdLineCreate = procedure(var CmdLine: string; var Abort:boolean) of object;

  {$IFDEF WithAsyncCompile}
  TBuildProjectData = class
  public
    Reason: TCompileReason;
    Flags: TProjectBuildFlags;
    CompilerFilename: String;
    CompilerParams: String;
  end;
  {$ENDIF}

  { TCompiler }

  TCompiler = class(TObject)
  private
    {$IFDEF WithAsyncCompile}
    FASyncResult: TModalResult;
    {$ENDIF}
    FOnCmdLineCreate : TOnCmdLineCreate;
    FOutputFilter: TOutputFilter;
    FTheProcess: TProcessUTF8;
    {$IFDEF WithAsyncCompile}
    FFinishedCallback: TNotifyEvent;
    procedure CompilationFinished(Sender: TObject);
    {$ENDIF}
  {$IFDEF WithAsyncCompile}
  public
    // Values stored by caller, to be rtrieved on callback
    CallerData: TObject;
  {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(AProject: TProject;
                     const WorkingDir, CompilerFilename, CompilerParams: string;
                     BuildAll, SkipLinking, SkipAssembler: boolean
                     {$IFDEF WithAsyncCompile}
                     ; aFinishedCallback: TNotifyEvent = nil
                     {$ENDIF}
                    ): TModalResult;
    procedure WriteError(const Msg: string);
    property OnCommandLineCreate: TOnCmdLineCreate read FOnCmdLineCreate
                                                   write FOnCmdLineCreate;
    property OutputFilter: TOutputFilter read FOutputFilter write FOutputFilter;
    property TheProcess: TProcessUTF8 read FTheProcess;
    {$IFDEF WithAsyncCompile}
    property ASyncResult: TModalResult read FASyncResult;
    {$ENDIF}
  end;

  // Following classes are for compiler options parsed from "fpc -h" and "fpc -i".

  TCompilerOptEditKind = (
    oeGroup,      // A header for a group
    oeSet,        // A header for a set
    oeBoolean,    // Typically use CheckBox
    oeSetElem,    // One char element of a set, use CheckBox
    oeSetNumber,  // Number element of a set, use Edit
    oeText,       // Textual value
    oeNumber,     // Numeric value
    oeList        // Pre-defined list of choices
  );

  TCompilerOptGroup = class;

  { TCompilerOpt }

  TCompilerOpt = class
  private
    fId: integer;                       // Identification.
    fOption: string;                    // Option with the leading '-'.
    fSuffix: string;                    // <x> or similar suffix of option.
    fValue: string;                     // Data entered by user, 'True' for Boolean.
    fEditKind: TCompilerOptEditKind;
    fDescription: string;
    fIndentation: integer;              // Indentation level in "fpc -h" output.
    fOwnerGroup: TCompilerOptGroup;
    fVisible: Boolean;                  // Used for filtering.
    fIgnored: Boolean;                  // Pretend this option does not exist.
    procedure Filter(aFilter: string; aOnlySelected: Boolean);
  protected
    procedure ParseOption(aDescr: string; aIndent: integer); virtual;
  public
    constructor Create(aOwnerGroup: TCompilerOptGroup);
    destructor Destroy; override;
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
  end;

  TCompilerOptList = TObjectList;

  { TCompilerOptGroup }

  // Group with explanation header. Actual options are not defined here.
  TCompilerOptGroup = class(TCompilerOpt)
  private
    // List of options belonging to this group.
    fCompilerOpts: TCompilerOptList;
  protected
    procedure ParseOption(aDescr: string; aIndent: integer); override;
  public
    constructor Create(aOwnerGroup: TCompilerOptGroup);
    destructor Destroy; override;
    function FindOption(aOptStr: string): TCompilerOpt;
    function FindOptionById(aId: integer): TCompilerOpt;
    function SelectOption(aOptAndValue: string): Boolean;
  public
    property CompilerOpts: TCompilerOptList read fCompilerOpts;
  end;

  { TCompilerOptSet }

  // A set of options. A combination of chars or numbers following the option char.
  TCompilerOptSet = class(TCompilerOptGroup)
  private
    fHasNumber: Boolean;
  protected
    procedure AddOptions(aDescr: string; aIndent: integer);
    procedure ParseOption(aDescr: string; aIndent: integer); override;
  public
    constructor Create(aOwnerGroup: TCompilerOptGroup);
    destructor Destroy; override;
    function CollectSelectedOptions: string;
    procedure SelectOptions(aOptStr: string);
  end;

  { TCompilerOptReader }

  TCompilerOptReader = class
  private
    // Defines (-d...) are separated from custom options and stored here.
    fDefines: TStringList;
    // All options except for defines.
    fOtherOptions: TStringList;
    // Lists of selections parsed from "fpc -i". Contains supported technologies.
    fSupportedCategories: TStringList;
    // Hierarchy of options parsed from "fpc -h".
    fRootOptGroup: TCompilerOptGroup;
    fCompilerExecutable: string;         // Copiler path must be set by caller.
    fCompilerVersion: string;            // Parsed from "fpc -h".
    fErrorMsg: String;
    function ReadFpcWithParam(aParam: string; aLines: TStringList): TModalResult;
    procedure ReadVersion(s: string);
    //function FindPrevGroup(aIndent: integer): TCompilerOptGroup;
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
    property OtherOptions: TStringList read fOtherOptions;
    property SupportedCategories: TStringList read fSupportedCategories;
    property RootOptGroup: TCompilerOptGroup read fRootOptGroup;
    property CompilerExecutable: string read fCompilerExecutable write fCompilerExecutable;
    property ErrorMsg: String read fErrorMsg;
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
  FreeAndNil(FTheProcess);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TCompiler Compile
------------------------------------------------------------------------------}
function TCompiler.Compile(AProject: TProject;
  const WorkingDir, CompilerFilename, CompilerParams: string;
  BuildAll, SkipLinking, SkipAssembler: boolean
  {$IFDEF WithAsyncCompile} ; aFinishedCallback: TNotifyEvent = nil {$ENDIF}
  ): TModalResult;
var
  CmdLine : String;
  Abort : Boolean;
begin
  Result:=mrCancel;
  {$IFDEF WithAsyncCompile}
  FASyncResult:= mrNone;
  FFinishedCallback := aFinishedCallback;
  {$ENDIF}
  if ConsoleVerbosity>=0 then
    DebugLn('TCompiler.Compile WorkingDir="',WorkingDir,'" CompilerFilename="',CompilerFilename,'" CompilerParams="',CompilerParams,'"');

  // if we want to show the compile progress, it's now time to show the dialog
  CompileProgress.Show;

  CmdLine := CompilerFilename;

  if Assigned(FOnCmdLineCreate) then begin
    Abort:=false;
    FOnCmdLineCreate(CmdLine,Abort);
    if Abort then begin
      Result:=mrAbort;
      exit;
    end;
  end;
  try
    CheckIfFileIsExecutable(CmdLine);
  except
    on E: Exception do begin
      WriteError(Format(lisCompilerErrorInvalidCompiler, [E.Message]));
      if CmdLine='' then begin
        WriteError(lisCompilerHintYouCanSetTheCompilerPath);
      end;
      exit;
    end;
  end;
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
    DebugLn('[TCompiler.Compile] CmdLine="',CmdLine,'"');

  try
    if TheProcess=nil then
      FTheProcess := TOutputFilterProcess.Create(nil);
    TheProcess.CommandLine := CmdLine;
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
        {$IFDEF WithAsyncCompile}
        if aFinishedCallback <> nil then
        begin
          OutputFilter.ExecuteAsyncron(TheProcess, @CompilationFinished, Self);
        end
        else
        {$ENDIF}
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
      {$IFDEF WithAsyncCompile} and ((OutputFilter = nil) or (aFinishedCallback = nil)) {$ENDIF}
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
  if ConsoleVerbosity>=0 then
    DebugLn('[TCompiler.Compile] end');
end;

{$IFDEF WithAsyncCompile}
procedure TCompiler.CompilationFinished(Sender: TObject);
begin
  if OutputFilter.Aborted then
    FASyncrResult := mrAbort
  else
    FASyncResult := mrOK;
  if TheProcess.Running then
  begin
    TheProcess.WaitOnExit;
    if (FASyncResult = mrOk) and not (TheProcess.ExitStatus in [0,1]) then
    begin
      WriteError(Format(listCompilerInternalError, [TheProcess.ExitStatus]));
      FASyncResult := mrCancel;
    end;
  end;
  DebugLn('[TCompiler.Compile] Async end');

  if Assigned(FFinishedCallback) then
    FFinishedCallback(Self);
end;
{$ENDIF}

procedure TCompiler.WriteError(const Msg: string);
begin
  DebugLn('TCompiler.WriteError ',Msg);
  if OutputFilter <> nil then
    OutputFilter.ReadConstLine(Msg, True);
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
  // Ignore : * all file names and paths
  //          * executable path
  //          * define and undefine
  Result := aOpt[2] in ['F', 'e', 'd', 'u'];
end;

{ TCompilerOpt }

constructor TCompilerOpt.Create(aOwnerGroup: TCompilerOptGroup);
begin
  inherited Create;
  fOwnerGroup := aOwnerGroup;
  if Assigned(aOwnerGroup) then
    aOwnerGroup.fCompilerOpts.Add(Self);
  fId := NextOptionId;
end;

destructor TCompilerOpt.Destroy;
begin
  inherited Destroy;
end;

procedure TCompilerOpt.ParseOption(aDescr: string; aIndent: integer);
var
  i, Start: Integer;
begin
  fIndentation := aIndent;
  // Separate the actual option and description from each other
  Start := aIndent+1;
  if (Length(aDescr) < aIndent) or (aDescr[Start] <> '-') then
    raise Exception.Create('Option description does not start with "-"');
  i := Start;
  while (i < Length(aDescr)) and (aDescr[i] <> ' ') do
    Inc(i);
  fOption := Copy(aDescr, Start, i-Start);
  while (i < Length(aDescr)) and (aDescr[i] = ' ') do
    Inc(i);
  fDescription := Copy(aDescr, i, Length(aDescr));
  // Guess whether this option can be edited and what is the EditKind
  fEditKind := oeBoolean;                  // Default kind
  i := Length(fOption);
  if (i > 3) and (fOption[i-2] = '<') and (fOption[i] = '>') then
  begin
    case fOption[i-1] of
      'x': fEditKind:=oeText;              // <x>
      'n': fEditKind:=oeNumber;            // <n>
    end;
    // Move <x> in the end to Suffix. We need the pure option later.
    fSuffix := Copy(fOption, i-2, i);
    fOption := Copy(fOption, 1, i-3);
  end;
  if Pos('fpc -i', fDescription) > 0 then
    fEditKind := oeList;                   // Values will be got later.
  if fOwnerGroup.fIgnored or IsIgnoredOption(fOption) then
    fIgnored := True;
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

{ TCompilerOptGroup }

constructor TCompilerOptGroup.Create(aOwnerGroup: TCompilerOptGroup);
begin
  inherited Create(aOwnerGroup);
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

function TCompilerOptGroup.SelectOption(aOptAndValue: string): Boolean;
var
  Opt: TCompilerOpt;
  OptStr, Param: string;
  OptLen: integer;
begin
  Opt := FindOption(aOptAndValue);
  if Assigned(Opt) then
    Opt.Value := 'True'
  else begin
    // Option was not found, try separating the parameter.
    // ToDo: figure out the length in a more clever way.
    Assert((Length(aOptAndValue)>2) and (aOptAndValue[1]='-'),
           'TCompilerOptGroup.SelectOption: Invalid option & value');
    if aOptAndValue[2] in ['e', 'd', 'u', 'I', 'k', 'o'] then
      OptLen := 2
    else
      OptLen := 3;
    OptStr := Copy(aOptAndValue, 1, OptLen);
    Param := Copy(aOptAndValue, OptLen+1, Length(aOptAndValue));
    Opt := FindOption(OptStr);
    if Assigned(Opt) then
      Opt.Value := Param;
  end;
  Result := Assigned(Opt);
end;

procedure TCompilerOptGroup.ParseOption(aDescr: string; aIndent: integer);
begin
  inherited ParseOption(aDescr, aIndent);
  fEditKind := oeGroup;
end;

{ TCompilerOptSet }

constructor TCompilerOptSet.Create(aOwnerGroup: TCompilerOptGroup);
begin
  inherited Create(aOwnerGroup);
end;

destructor TCompilerOptSet.Destroy;
begin
  inherited Destroy;
end;

function TCompilerOptSet.CollectSelectedOptions: string;
// Collect subitems of a set to one option.
var
  Opt: TCompilerOpt;
  i: Integer;
  s: string;
begin
  Result := '';
  s := '';
  for i := 0 to fCompilerOpts.Count-1 do
  begin
    Opt := TCompilerOpt(fCompilerOpts[i]);
    if Opt.Value <> '' then
      s := s + Opt.Option;
  end;
  if s <> '' then
    Result := Option + s;
end;

procedure TCompilerOptSet.SelectOptions(aOptStr: string);
// Select options in this set based on the given characters.
var
  i, j: Integer;
  OptChar: string;
  Opt: TCompilerOpt;
begin
  for i := 1 to Length(aOptStr) do
  begin
    OptChar := aOptStr[i];
    for j := 0 to fCompilerOpts.Count-1 do
    begin
      Opt := TCompilerOpt(fCompilerOpts[j]);
      if Opt.Option = OptChar then
      begin
        Opt.Value := 'True';
        Break;
      end;
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
    fHasNumber := True;
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
  Opt1 := Copy(aDescr, aIndent+1, Length(aDescr));
  if AnsiStartsStr('<n>', Opt1) then
    NewSetNumber(Opt1)
  else begin
    i := PosEx(':', Opt1, 4);
    if (i > 0) and (Opt1[i-1]=' ') and (Opt1[i-2]<>' ') and (Opt1[i-3]=' ') then
    begin
      // Found another option on the same line, like ' a :'
      Opt2 := Copy(Opt1, i-2, Length(Opt1));
      if Opt1[3] = ':' then
        Opt1 := TrimRight(Copy(Opt1, 1, i-3))
      else
        Opt1 := '';
    end;
    if Opt1 <> '' then         // Can be empty when line in help output is split.
      NewSetElem(Opt1);
    if Opt2 <> '' then
      NewSetElem(Opt2);
  end;
end;

procedure TCompilerOptSet.ParseOption(aDescr: string; aIndent: integer);
begin
  inherited ParseOption(aDescr, aIndent);
  fEditKind := oeSet;
end;


{ TCompilerOptReader }

constructor TCompilerOptReader.Create;
begin
  inherited Create;
  fDefines := TStringList.Create;
  fOtherOptions := TStringList.Create;
  fSupportedCategories := TStringList.Create;
  fRootOptGroup := TCompilerOptGroup.Create(Nil);
end;

destructor TCompilerOptReader.Destroy;
var
  i: Integer;
begin
  fRootOptGroup.Free;
  for i := 0 to fSupportedCategories.Count-1 do
    fSupportedCategories.Objects[i].Free;
  fSupportedCategories.Free;
  fOtherOptions.Free;
  fDefines.Free;
  inherited Destroy;
end;

function TCompilerOptReader.ReadFpcWithParam(aParam: string; aLines: TStringList): TModalResult;
// fpc -Fr$(FPCMsgFile) -h
// fpc -Fr$(FPCMsgFile) -i
var
  proc: TProcessUTF8;
  OutStream: TMemoryStream;

  function ReadOutput: boolean;
  // returns true if output was actually read
  const
    BufSize = 4096;
  var
    Buffer: array[0..BufSize-1] of byte;
    ReadBytes: integer;
  begin
    Result := false;
    while proc.Output.NumBytesAvailable>0 do
    begin
      ReadBytes := proc.Output.Read(Buffer{%H-}, BufSize);
      OutStream.Write(Buffer, ReadBytes);
      Result := true;
    end;
  end;

  function GetOutput: string;
  begin
    SetLength(Result, OutStream.Size);
    OutStream.Seek(0,soBeginning);
    OutStream.Read(Result[1],Length(Result));
  end;

begin
  proc := TProcessUTF8.Create(Nil);
  OutStream := TMemoryStream.Create;
  try
    if fCompilerExecutable = '' then
      fCompilerExecutable := 'fpc';        // Let's hope "fpc" is found in PATH.
    proc.Executable := fCompilerExecutable;
    proc.Parameters.Add(aParam);
    proc.Options:= [poUsePipes, poStdErrToOutput];
    //proc.ShowWindow := swoHide;
    proc.ShowWindow := swoShowNormal;
    //proc.CurrentDirectory := WorkingDir;
    proc.Execute;
    while proc.Running do
      if not ReadOutput then
        Sleep(100);
    ReadOutput;
    Result := proc.ExitStatus;
    if Result<>0 then
    begin
      fErrorMsg := Format('fpc %s failed: Result %d' + LineEnding + '%s',
                          [aParam, Result, GetOutput]);
      Result := mrCancel;
    end
    else begin
      OutStream.Seek(0,soBeginning);
      aLines.LoadFromStream(OutStream);
      Result := mrOK;
    end;
  finally
    OutStream.Free;
    proc.Free;
  end;
end;

function TCompilerOptReader.ParseI(aLines: TStringList): TModalResult;
const
  Supported = 'Supported ';
var
  i: Integer;
  s, Line, TrimmedLine: String;
  Category: TStringList;
begin
  Result := mrOK;
  Category := Nil;
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
        Category.Add(Trim(Line));
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

function TCompilerOptReader.ParseH(aLines: TStringList): TModalResult;
const
  OptSetId = 'a combination of';
var
  i, ThisInd, NextInd: Integer;
  ThisLine, NextLine: String;
  Opt: TCompilerOpt;
  LastGroup: TCompilerOptGroup;
begin
  Result := mrOK;
  LastGroup := fRootOptGroup;
  for i := 0 to aLines.Count-1 do
  begin
    ThisLine := StringReplace(aLines[i],'-Agas-darwinAssemble','-Agas-darwin Assemble',[]);
    ThisInd := CalcIndentation(ThisLine);
    if ThisInd = 0 then
    begin
      ReadVersion(ThisLine);        // Top header lines for compiler version etc.
      Continue;
    end;
    if (Trim(ThisLine) = '') or (ThisInd > 30)
    or (Pos(' -? ', ThisLine) > 0)
    or (Pos(' -h ', ThisLine) > 0) then Continue;

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
        if Pos(OptSetId, ThisLine) > 0 then     // Header for sets
          LastGroup := TCompilerOptSet.Create(LastGroup)
        else                                    // Group header for options
          LastGroup := TCompilerOptGroup.Create(LastGroup);
        LastGroup.ParseOption(ThisLine, ThisInd);
      end;
    end;
    if NextInd <= ThisInd then
    begin
      // This is an option
      if (LastGroup is TCompilerOptSet) then    // Add it to a set (may add many)
        TCompilerOptSet(LastGroup).AddOptions(ThisLine, ThisInd)
      else begin
        Opt := TCompilerOpt.Create(LastGroup);  // Add it under a group
        Opt.ParseOption(ThisLine, ThisInd);
      end;
      if (NextInd <> -1) and (NextInd < ThisInd) then
        LastGroup := LastGroup.fOwnerGroup;     // Return to a previous group
    end;
  end;
end;

function TCompilerOptReader.ReadAndParseOptions: TModalResult;
var
  Lines: TStringList;
begin
  OptionIdCounter := 0;
  Lines := TStringList.Create;
  try
    // FPC with option -i
    Result := ReadFpcWithParam('-i', Lines);
    if Result <> mrOK then Exit;
    Result := ParseI(Lines);
    if Result <> mrOK then Exit;

    // FPC with option -h
    Lines.Clear;
    Result := ReadFpcWithParam('-h', Lines);
    if Result <> mrOK then Exit;
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
var
  i, j, CommentPos: Integer;
  HasDefine: Boolean;
  s: String;
  sl: TStringList;
begin
  Result := mrOK;
  sl := TStringList.Create;
  try
    for i := 0 to aStrings.Count-1 do
    begin
      s := Trim(aStrings[i]);
      if s = '' then Continue;
      CommentPos := Pos('//', s);
      if CommentPos > 0 then         // Remove possible comment.
        s := TrimRight(Copy(s, 1, CommentPos));
      HasDefine := Pos('-d', s) > 0;
      if not HasDefine then
        fOtherOptions.Add(s); // Don't split the line for other options if no defines.
      sl.StrictDelimiter := True;
      sl.Delimiter := ' ';
      sl.DelimitedText := s;         // Split the line with space as a separator.
      for j := 0 to sl.Count-1 do
        if AnsiStartsStr('-d', sl[j]) then
          fDefines.Add(sl[j])
        else begin
          fRootOptGroup.SelectOption(sl[j]);
          if HasDefine then
            fOtherOptions.Add(sl[j]);
        end;
    end;
  finally
    sl.Free;
  end;
end;

function TCompilerOptReader.ToCustomOptions(aStrings: TStrings;
  aUseComments: Boolean): TModalResult;
// Copy options to a list if they have a non-default value (True for boolean).

  function PossibleComment(aRoot: TCompilerOpt): string;
  begin
    if aUseComments then
      Result := '    // ' + aRoot.Description
    else
      Result := '';
  end;

  procedure CopyOptions(aRoot: TCompilerOpt);
  var
    Children: TCompilerOptList;
    i: Integer;
    s: string;
  begin
    if aRoot is TCompilerOptGroup then
    begin
      Children := TCompilerOptGroup(aRoot).CompilerOpts;
      if aRoot is TCompilerOptSet then
      begin                  // TCompilerOptSet
        s := TCompilerOptSet(aRoot).CollectSelectedOptions;
        if s <> '' then
          aStrings.Add(s + PossibleComment(aRoot));
      end
      else begin             // TCompilerOptGroup
        for i := 0 to Children.Count-1 do         // Recursive call for children.
          CopyOptions(TCompilerOpt(Children[i]));
      end;
    end
    else begin               // TCompilerOpt
      if aRoot.Value <> '' then
      begin
        if aRoot.Value = 'True' then
          aStrings.Add(aRoot.Option + PossibleComment(aRoot))
        else
          aStrings.Add(aRoot.Option + aRoot.Value + PossibleComment(aRoot));
      end;
    end;
  end;

begin
  Result := mrOK;
  aStrings.Clear;
  CopyOptions(fRootOptGroup);
  aStrings.AddStrings(fDefines);
end;

end.

