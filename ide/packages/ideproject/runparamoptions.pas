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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Run Parameters Options (TRunParamsOptions)
    and Dialog for them (TRunParamsOptsDlg)

    Run Parameters are project specific options for the debugger like
    command line parameters and working directory.

    The options saved in a TRunParamsOptions are stored in the project info file
    (.lpi) together with the rest of the project.

    The dialog will be activated by main.pp with the function
    ShowRunParamsOptsDlg (see below) when the user clicks on the
    menu->Run->Run Parameters.
}
unit RunParamOptions;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, System.UITypes,
  // LazUtils
  LazFileUtils, LazFileCache, LazUTF8, Laz2_XMLCfg,
  // BuildIntf
  BaseIDEIntf, ProjectIntf, MacroIntf;

{ The xml format version:
    When the format changes (new values, changed formats) we can distinguish old
    files and are able to convert them.
}
const
  RunParamsOptionsVersion = 2;

type
  {
    the storage object for run parameters
  }

  TRunParamsOptionsModeSave = (rpsLPS, rpsLPI);

  { TRunParamsOptionsMode }

  TRunParamsOptionsMode = class(TAbstractRunParamsOptionsMode)
  private
    fSaveIn: TRunParamsOptionsModeSave;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property SaveIn: TRunParamsOptionsModeSave read fSaveIn write fSaveIn;
  public
    procedure AssignEnvironmentTo(Strings: TStrings); override;

    function LegacyLoad(XMLConfig: TXMLConfig; const Path: string;
      AdjustPathDelims: boolean): TModalResult;
    function Load(XMLConfig: TXMLConfig; const Path: string;
      AdjustPathDelims: boolean): TModalResult;
    function LegacySave(XMLConfig: TXMLConfig; const Path: string;
      UsePathDelim: TPathDelimSwitch): TModalResult;
    function Save(XMLConfig: TXMLConfig; const Path: string;
      UsePathDelim: TPathDelimSwitch): TModalResult;

    procedure Clear; override;
  end;

  { TRunParamsOptions }

  TRunParamsOptions = class(TAbstractRunParamsOptions)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function CreateMode(const AName: string): TAbstractRunParamsOptionsMode; override;
    procedure SetActiveModeName(const AValue: string); override;
  public
    procedure AssignEnvironmentTo(Strings: TStrings); override;

    function LegacyLoad(XMLConfig: TXMLConfig; const Path: string;
      AdjustPathDelims: boolean): TModalResult;
    function Load(XMLConfig: TXMLConfig; const Path: string;
      AdjustPathDelims: boolean; const ASaveIn: TRunParamsOptionsModeSave): TModalResult;
    function LegacySave(XMLConfig: TXMLConfig; const Path: string;
      UsePathDelim: TPathDelimSwitch): TModalResult;
    function Save(XMLConfig: TXMLConfig; const Path: string;
      UsePathDelim: TPathDelimSwitch; const ASaveIn: TRunParamsOptionsModeSave;
      const ALegacyList: Boolean): TModalResult;
    function GetActiveMode: TRunParamsOptionsMode;
  end;

function FindTerminalInPath(const ATerm: String = ''): String;

implementation

const
  DefaultLauncherTitle = '''Lazarus Run Output''';
  DefaultLauncherApplication = '$(LazarusDir)/tools/runwait.sh $(TargetCmdLine)';

function FindTerminalInPath(const ATerm: String = ''): String;
var
  List: TStrings;
  i: Integer;
  s: String;
  Term: String;
begin
  Result := '';
  Term := ATerm;
  if Term = '' then
    Term := GetEnvironmentVariableUTF8('TERM');
  List := TStringList.Create;
  {$IFDEF MSWINDOWS}
  List.Delimiter := ';';
  if Term = '' then
    Term := 'cmd.exe';
  {$ELSE}
  List.Delimiter := ':';
  if Term = '' then
    Term := 'xterm';
  {$ENDIF}
  List.DelimitedText := GetEnvironmentVariableUTF8('PATH');
  for i := 0 to List.Count - 1 do
  begin
    S := List.Strings[i] + PathDelim + Term;
    if FileExistsCached(S) and FileIsExecutableCached(S) then
    begin
      // gnome-terminal is not compatible to xterm params.
      if Term = 'gnome-terminal' then
        Result := S + ' -t ' + DefaultLauncherTitle + ' -e ' +
          '''' + DefaultLauncherApplication + ''''
      else if SameText(Term,'cmd.exe') then
        Result := S + ' /C ${TargetCmdLine}'
      else
        Result := S + ' -T ' + DefaultLauncherTitle + ' -e ' +
          DefaultLauncherApplication;
      break;
    end;
  end;
  List.Free;
end;

var
  DefaultLaunchingApplicationPathPlusParams: string;

function GetDefaultLaunchingApplicationPathPlusParams: string;
begin
  Result:=DefaultLaunchingApplicationPathPlusParams;
  if Result<>'' then exit;
  Result:=FindTerminalInPath;
  DefaultLaunchingApplicationPathPlusParams:=Result;
end;

{ TRunParamsOptions }

procedure TRunParamsOptions.AssignEnvironmentTo(Strings: TStrings);
begin
  if GetActiveMode=nil then
    BaseIDEIntf.AssignEnvironmentTo(Strings, nil)
  else
    GetActiveMode.AssignEnvironmentTo(Strings);
end;

procedure TRunParamsOptions.AssignTo(Dest: TPersistent);
var
  ADest: TRunParamsOptions;
begin
  inherited AssignTo(Dest);

  if Dest is TRunParamsOptions then
  begin
    ADest := TRunParamsOptions(Dest);

    ADest.ActiveModeName := ActiveModeName;
  end;
end;

function TRunParamsOptions.CreateMode(const AName: string
  ): TAbstractRunParamsOptionsMode;
begin
  Result := TRunParamsOptionsMode.Create(AName);
end;

procedure TRunParamsOptions.SetActiveModeName(const AValue: string);
var
  NewMode: TAbstractRunParamsOptionsMode;
begin
  if AValue=ActiveModeName then exit;
  NewMode:=Find(AValue);
  if NewMode<>nil then
    inherited SetActiveModeName(NewMode.Name)
  else if AValue<>'' then
    raise EListError.Create('TRunParamsOptions.SetActiveModeName no such mode "'+AValue+'"')
  else
    inherited SetActiveModeName('');
end;

function TRunParamsOptions.GetActiveMode: TRunParamsOptionsMode;
var
  AMode: TAbstractRunParamsOptionsMode;
begin
  AMode := Find(ActiveModeName);
  if AMode=nil then exit(nil);
  Result := AMode as TRunParamsOptionsMode;
end;

function TRunParamsOptions.LegacyLoad(XMLConfig: TXMLConfig;
  const Path: string; AdjustPathDelims: boolean): TModalResult;
var
  NewMode: TRunParamsOptionsMode;
begin
  Clear;

  NewMode := Add('default') as TRunParamsOptionsMode;
  NewMode.SaveIn :=  rpsLPI;
  Result := NewMode.LegacyLoad(XMLConfig, Path+'RunParams/', AdjustPathDelims);
  ActiveModeName := 'default';
end;

function TRunParamsOptions.LegacySave(XMLConfig: TXMLConfig;
  const Path: string; UsePathDelim: TPathDelimSwitch): TModalResult;
var
  AMode: TRunParamsOptionsMode;
begin
  Result := mrOK;

  AMode := GetActiveMode;
  if (AMode<>nil) and (AMode.SaveIn=rpsLPI) then
    AMode.LegacySave(XMLConfig, Path+'RunParams/', UsePathDelim);
end;

function TRunParamsOptions.Load(XMLConfig: TXMLConfig; const Path: string;
  AdjustPathDelims: boolean; const ASaveIn: TRunParamsOptionsModeSave
  ): TModalResult;
var
  Cnt, I: Integer;
  NewMode: TRunParamsOptionsMode;
  ModePath, NewActiveModeName, ModesPath: string;
  IsLegacyList: Boolean;
begin
  //don't clear! needed for merging lpi and lps

  ModesPath := Path + 'Modes/';
  IsLegacyList := XMLConfig.IsLegacyList(ModesPath);
  Cnt := XMLConfig.GetListItemCount(ModesPath, 'Mode', IsLegacyList);
  Result := mrOK;

  for I := 0 to Cnt-1 do
  begin
    ModePath := ModesPath+XMLConfig.GetListItemXPath('Mode', I, IsLegacyList, False)+'/';
    NewMode := Add(XMLConfig.GetValue(ModePath+'Name', '')) as TRunParamsOptionsMode;
    NewMode.SaveIn := ASaveIn;
    Result := NewMode.Load(XMLConfig, ModePath, AdjustPathDelims);
    if Result<>mrOK then
      Exit;
  end;

  if ASaveIn=rpsLPS then
  begin
    NewActiveModeName := XMLConfig.GetValue(Path + 'Modes/ActiveMode', '');
    // sanity check -> modes from LPI could be modified independently on LPS and
    // NewActiveModeName doesn't have to exist any more
    if Assigned(Find(NewActiveModeName)) then
      ActiveModeName := NewActiveModeName;
  end;

  if (GetActiveMode=nil) and (Count>0) then
    ActiveModeName := Modes[0].Name;
end;

function TRunParamsOptions.Save(XMLConfig: TXMLConfig; const Path: string;
  UsePathDelim: TPathDelimSwitch; const ASaveIn: TRunParamsOptionsModeSave;
  const ALegacyList: Boolean): TModalResult;
var
  AMode: TRunParamsOptionsMode;
  I, Cnt: Integer;
  ModesPath, ModePath: string;
begin
  Result := mrOK;
  ModesPath := Path+'Modes/';

  // save a format version to distinguish old formats
  XMLConfig.SetValue(Path + 'FormatVersion/Value',
    RunParamsOptionsVersion);

  Cnt := 0;
  for I := 0 to Count-1 do
  begin
    AMode := Modes[I] as TRunParamsOptionsMode;

    if AMode.SaveIn=ASaveIn then
    begin
      ModePath := ModesPath+XMLConfig.GetListItemXPath('Mode', Cnt, ALegacyList, False)+'/';
      Result := AMode.Save(XMLConfig, ModePath, UsePathDelim);
      if Result<>mrOK then
        Exit;
      Inc(Cnt);
    end;
  end;

  XMLConfig.SetListItemCount(ModesPath, Cnt, ALegacyList);
  if ASaveIn=rpsLPS then
    XMLConfig.SetValue(Path + 'Modes/ActiveMode', ActiveModeName);
end;

{ TRunParamsOptionsMode }

function TRunParamsOptionsMode.LegacyLoad(XMLConfig: TXMLConfig;
  const Path: string; AdjustPathDelims: boolean): TModalResult;

  function f(const Filename: string): string;
  begin
    Result := SwitchPathDelims(Filename, AdjustPathDelims);
  end;

  procedure LoadUserOverrides(const APath: string);
  var
    i, Cnt: integer;
  begin
    Cnt := XMLConfig.GetValue(APath + 'Count', 0);
    for i := 0 to Cnt - 1 do
    begin
      UserOverrides.Values[XMLConfig.GetValue(
        APath + 'Variable' + IntToStr(i) + '/Name', '')] :=
        XMLConfig.GetValue(APath + 'Variable' + IntToStr(i) + '/Value', '');
    end;
  end;

begin
  // local options
  HostApplicationFilename := f(XMLConfig.GetValue(
    Path + 'local/HostApplicationFilename/Value',
    HostApplicationFilename));
  CmdLineParams := f(XMLConfig.GetValue(
    Path + 'local/CommandLineParams/Value', CmdLineParams));
  UseLaunchingApplication := XMLConfig.GetValue(
    Path + 'local/LaunchingApplication/Use', UseLaunchingApplication);
  LaunchingApplicationPathPlusParams :=
    f(XMLConfig.GetValue(Path + 'local/LaunchingApplication/PathPlusParams',
                         f(GetDefaultLaunchingApplicationPathPlusParams)));
  WorkingDirectory := f(XMLConfig.GetValue(
    Path + 'local/WorkingDirectory/Value', WorkingDirectory));
  UseDisplay := XMLConfig.GetValue(Path + 'local/Display/Use',
    UseDisplay);
  Display    := XMLConfig.GetValue(Path + 'local/Display/Value', Display);

  UseConsoleWinPos    := XMLConfig.GetValue(Path + 'local/UseConsoleWinPos/Value', False);
  UseConsoleWinSize   := XMLConfig.GetValue(Path + 'local/UseConsoleWinSize/Value', False);
  UseConsoleWinBuffer := XMLConfig.GetValue(Path + 'local/UseConsoleWinBuffer/Value', False);
  ConsoleWinPos := Point(
    XMLConfig.GetValue(Path + 'local/ConsoleWinPos/Left/Value', 0),
    XMLConfig.GetValue(Path + 'local/ConsoleWinPos/Top/Value',     0)
  );
  ConsoleWinSize := Point(
    XMLConfig.GetValue(Path + 'local/ConsoleWinSize/Width/Value', 0),
    XMLConfig.GetValue(Path + 'local/ConsoleWinSize/Height/Value',     0)
  );
  ConsoleWinBuffer := Point(
    XMLConfig.GetValue(Path + 'local/ConsoleWinBuffer/Columns/Value', 0),
    XMLConfig.GetValue(Path + 'local/ConsoleWinBuffer/Rows/Value',     0)
  );

  XMLConfig.GetValue(Path + 'local/RedirectStdIn/Value',  ord(rprOff), FRedirectStdIn,  TypeInfo(TRunParamsRedirectMode));
  XMLConfig.GetValue(Path + 'local/RedirectStdOut/Value', ord(rprOff), FRedirectStdOut, TypeInfo(TRunParamsRedirectMode));
  XMLConfig.GetValue(Path + 'local/RedirectStdErr/Value', ord(rprOff), FRedirectStdErr, TypeInfo(TRunParamsRedirectMode));
  FileNameStdIn  := XMLConfig.GetValue(Path + 'local/FileNameStdIn/Value',  '');
  FileNameStdOut := XMLConfig.GetValue(Path + 'local/FileNameStdOut/Value', '');
  FileNameStdErr := XMLConfig.GetValue(Path + 'local/FileNameStdErr/Value', '');

  // environment options
  LoadUserOverrides(Path + 'environment/UserOverrides/');
  IncludeSystemVariables := XMLConfig.GetValue(
    Path + 'environment/IncludeSystemVariables/Value',
    IncludeSystemVariables);

  Result := mrOk;
end;

function TRunParamsOptionsMode.LegacySave(XMLConfig: TXMLConfig;
  const Path: string; UsePathDelim: TPathDelimSwitch): TModalResult;

  function f(const AFilename: string): string;
  begin
    Result:=SwitchPathDelims(AFilename,UsePathDelim);
  end;

  procedure SaveUserOverrides(const APath: string);
  var
    i: integer;
  begin
    XMLConfig.SetDeleteValue(APath + 'Count', UserOverrides.Count, 0);
    for i := 0 to UserOverrides.Count - 1 do
    begin
      XMLConfig.SetValue(APath + 'Variable' + IntToStr(i) + '/Name',
        UserOverrides.Names[i]); // no default
      XMLConfig.SetDeleteValue(APath + 'Variable' + IntToStr(i) + '/Value',
        UserOverrides.Values[UserOverrides.Names[i]],'');
    end;
  end;

begin
  // local options
  XMLConfig.SetDeleteValue(Path + 'local/HostApplicationFilename/Value',
    f(HostApplicationFilename), '');
  XMLConfig.SetDeleteValue(Path + 'local/CommandLineParams/Value',
    f(CmdLineParams), '');
  XMLConfig.SetDeleteValue(Path + 'local/LaunchingApplication/Use',
    UseLaunchingApplication, False);
  XMLConfig.SetDeleteValue(Path + 'local/LaunchingApplication/PathPlusParams',
    f(LaunchingApplicationPathPlusParams), f(GetDefaultLaunchingApplicationPathPlusParams));
  XMLConfig.SetDeleteValue(Path + 'local/WorkingDirectory/Value',
    f(WorkingDirectory), '');
  XMLConfig.SetDeleteValue(Path + 'local/Display/Use',
    UseDisplay, False);
  XMLConfig.SetDeleteValue(Path + 'local/Display/Value',
    Display, ':0');

  XMLConfig.SetDeleteValue(Path + 'local/UseConsoleWinPos/Value', UseConsoleWinPos, False);
  XMLConfig.SetDeleteValue(Path + 'local/UseConsoleWinSize/Value', UseConsoleWinSize, False);
  XMLConfig.SetDeleteValue(Path + 'local/UseConsoleWinBuffer/Value', UseConsoleWinBuffer, False);
  XMLConfig.SetDeleteValue(Path + 'local/ConsoleWinPos/Left/Value',       ConsoleWinPos.X, 0);
  XMLConfig.SetDeleteValue(Path + 'local/ConsoleWinPos/Top/Value',        ConsoleWinPos.Y, 0);
  XMLConfig.SetDeleteValue(Path + 'local/ConsoleWinSize/Width/Value',     ConsoleWinSize.X, 0);
  XMLConfig.SetDeleteValue(Path + 'local/ConsoleWinSize/Height/Value',    ConsoleWinSize.Y, 0);
  XMLConfig.SetDeleteValue(Path + 'local/ConsoleWinBuffer/Columns/Value', ConsoleWinBuffer.X, 0);
  XMLConfig.SetDeleteValue(Path + 'local/ConsoleWinBuffer/Rows/Value',    ConsoleWinBuffer.Y, 0);

  XMLConfig.SetDeleteValue(Path + 'local/RedirectStdIn/Value',  FRedirectStdIn,  ord(rprOff), TypeInfo(TRunParamsRedirectMode));
  XMLConfig.SetDeleteValue(Path + 'local/RedirectStdOut/Value', FRedirectStdOut, ord(rprOff), TypeInfo(TRunParamsRedirectMode));
  XMLConfig.SetDeleteValue(Path + 'local/RedirectStdErr/Value', FRedirectStdErr, ord(rprOff), TypeInfo(TRunParamsRedirectMode));
  XMLConfig.SetDeleteValue(Path + 'local/FileNameStdIn/Value',  FileNameStdIn,  '');
  XMLConfig.SetDeleteValue(Path + 'local/FileNameStdOut/Value', FileNameStdOut, '');
  XMLConfig.SetDeleteValue(Path + 'local/FileNameStdErr/Value', FileNameStdErr, '');

  Result := mrOk;
end;

function TRunParamsOptionsMode.Load(XMLConfig: TXMLConfig; const Path: string;
  AdjustPathDelims: boolean): TModalResult;

  function f(const Filename: string): string;
  begin
    Result := SwitchPathDelims(Filename, AdjustPathDelims);
  end;

  procedure LoadUserOverrides(const APath: string);
  var
    i, Cnt: integer;
  begin
    Cnt := XMLConfig.GetValue(APath + 'Count', 0);
    for i := 0 to Cnt - 1 do
    begin
      UserOverrides.Values[XMLConfig.GetValue(
        APath + 'Variable' + IntToStr(i) + '/Name', '')] :=
        XMLConfig.GetValue(APath + 'Variable' + IntToStr(i) + '/Value', '');
    end;
  end;

begin
  // local options
  HostApplicationFilename := f(XMLConfig.GetValue(
    Path + 'local/HostApplicationFilename/Value',
    HostApplicationFilename));
  CmdLineParams := f(XMLConfig.GetValue(
    Path + 'local/CommandLineParams/Value', CmdLineParams));
  UseLaunchingApplication := XMLConfig.GetValue(
    Path + 'local/LaunchingApplication/Use', UseLaunchingApplication);
  LaunchingApplicationPathPlusParams :=
    f(XMLConfig.GetValue(Path + 'local/LaunchingApplication/PathPlusParams',
                         f(GetDefaultLaunchingApplicationPathPlusParams)));
  WorkingDirectory := f(XMLConfig.GetValue(
    Path + 'local/WorkingDirectory/Value', WorkingDirectory));
  UseDisplay := XMLConfig.GetValue(Path + 'local/Display/Use',
    UseDisplay);
  Display    := XMLConfig.GetValue(Path + 'local/Display/Value', Display);

  UseConsoleWinPos    := XMLConfig.GetValue(Path + 'local/UseConsoleWinPos/Value', False);
  UseConsoleWinSize   := XMLConfig.GetValue(Path + 'local/UseConsoleWinSize/Value', False);
  UseConsoleWinBuffer := XMLConfig.GetValue(Path + 'local/UseConsoleWinBuffer/Value', False);
  ConsoleWinPos := Point(
    XMLConfig.GetValue(Path + 'local/ConsoleWinPos/Left/Value', 0),
    XMLConfig.GetValue(Path + 'local/ConsoleWinPos/Top/Value',     0)
  );
  ConsoleWinSize := Point(
    XMLConfig.GetValue(Path + 'local/ConsoleWinSize/Width/Value', 0),
    XMLConfig.GetValue(Path + 'local/ConsoleWinSize/Height/Value',     0)
  );
  ConsoleWinBuffer := Point(
    XMLConfig.GetValue(Path + 'local/ConsoleWinBuffer/Columns/Value', 0),
    XMLConfig.GetValue(Path + 'local/ConsoleWinBuffer/Rows/Value',     0)
  );

  XMLConfig.GetValue(Path + 'local/RedirectStdIn/Value',  ord(rprOff), FRedirectStdIn,  TypeInfo(TRunParamsRedirectMode));
  XMLConfig.GetValue(Path + 'local/RedirectStdOut/Value', ord(rprOff), FRedirectStdOut, TypeInfo(TRunParamsRedirectMode));
  XMLConfig.GetValue(Path + 'local/RedirectStdErr/Value', ord(rprOff), FRedirectStdErr, TypeInfo(TRunParamsRedirectMode));
  FileNameStdIn  := XMLConfig.GetValue(Path + 'local/FileNameStdIn/Value',  '');
  FileNameStdOut := XMLConfig.GetValue(Path + 'local/FileNameStdOut/Value', '');
  FileNameStdErr := XMLConfig.GetValue(Path + 'local/FileNameStdErr/Value', '');

  // environment options
  LoadUserOverrides(Path + 'environment/UserOverrides/');
  IncludeSystemVariables := XMLConfig.GetValue(
    Path + 'environment/IncludeSystemVariables/Value',
    IncludeSystemVariables);

  Result := mrOk;
end;

function TRunParamsOptionsMode.Save(XMLConfig: TXMLConfig; const Path: string;
  UsePathDelim: TPathDelimSwitch): TModalResult;

  function f(const AFilename: string): string;
  begin
    Result:=SwitchPathDelims(AFilename,UsePathDelim);
  end;

  procedure SaveUserOverrides(const APath: string);
  var
    i: integer;
  begin
    XMLConfig.SetDeleteValue(APath + 'Count', UserOverrides.Count, 0);
    for i := 0 to UserOverrides.Count - 1 do
    begin
      XMLConfig.SetValue(APath + 'Variable' + IntToStr(i) + '/Name',
        UserOverrides.Names[i]); // no default
      XMLConfig.SetDeleteValue(APath + 'Variable' + IntToStr(i) + '/Value',
        UserOverrides.Values[UserOverrides.Names[i]],'');
    end;
  end;

begin
  XMLConfig.SetValue(Path + 'Name', Name);

  // local options
  XMLConfig.SetDeleteValue(Path + 'local/HostApplicationFilename/Value',
    f(HostApplicationFilename), '');
  XMLConfig.SetDeleteValue(Path + 'local/CommandLineParams/Value',
    f(CmdLineParams), '');
  XMLConfig.SetDeleteValue(Path + 'local/LaunchingApplication/Use',
    UseLaunchingApplication, False);
  XMLConfig.SetDeleteValue(Path + 'local/LaunchingApplication/PathPlusParams',
    f(LaunchingApplicationPathPlusParams), f(GetDefaultLaunchingApplicationPathPlusParams));
  XMLConfig.SetDeleteValue(Path + 'local/WorkingDirectory/Value',
    f(WorkingDirectory), '');
  XMLConfig.SetDeleteValue(Path + 'local/Display/Use',
    UseDisplay, False);
  XMLConfig.SetDeleteValue(Path + 'local/Display/Value',
    Display, ':0');

  XMLConfig.SetDeleteValue(Path + 'local/UseConsoleWinPos/Value', UseConsoleWinPos, False);
  XMLConfig.SetDeleteValue(Path + 'local/UseConsoleWinSize/Value', UseConsoleWinSize, False);
  XMLConfig.SetDeleteValue(Path + 'local/UseConsoleWinBuffer/Value', UseConsoleWinBuffer, False);
  XMLConfig.SetDeleteValue(Path + 'local/ConsoleWinPos/Left/Value',       ConsoleWinPos.X, 0);
  XMLConfig.SetDeleteValue(Path + 'local/ConsoleWinPos/Top/Value',        ConsoleWinPos.Y, 0);
  XMLConfig.SetDeleteValue(Path + 'local/ConsoleWinSize/Width/Value',     ConsoleWinSize.X, 0);
  XMLConfig.SetDeleteValue(Path + 'local/ConsoleWinSize/Height/Value',    ConsoleWinSize.Y, 0);
  XMLConfig.SetDeleteValue(Path + 'local/ConsoleWinBuffer/Columns/Value', ConsoleWinBuffer.X, 0);
  XMLConfig.SetDeleteValue(Path + 'local/ConsoleWinBuffer/Rows/Value',    ConsoleWinBuffer.Y, 0);

  XMLConfig.SetDeleteValue(Path + 'local/RedirectStdIn/Value',  FRedirectStdIn,  ord(rprOff), TypeInfo(TRunParamsRedirectMode));
  XMLConfig.SetDeleteValue(Path + 'local/RedirectStdOut/Value', FRedirectStdOut, ord(rprOff), TypeInfo(TRunParamsRedirectMode));
  XMLConfig.SetDeleteValue(Path + 'local/RedirectStdErr/Value', FRedirectStdErr, ord(rprOff), TypeInfo(TRunParamsRedirectMode));
  XMLConfig.SetDeleteValue(Path + 'local/FileNameStdIn/Value',  FileNameStdIn,  '');
  XMLConfig.SetDeleteValue(Path + 'local/FileNameStdOut/Value', FileNameStdOut, '');
  XMLConfig.SetDeleteValue(Path + 'local/FileNameStdErr/Value', FileNameStdErr, '');

  // environment options
  SaveUserOverrides(Path + 'environment/UserOverrides/');
  XMLConfig.SetDeleteValue(Path + 'environment/IncludeSystemVariables/Value',
    IncludeSystemVariables, False);

  Result := mrOk;
end;

procedure TRunParamsOptionsMode.AssignEnvironmentTo(Strings: TStrings);
var
  idx: integer;
begin
  BaseIDEIntf.AssignEnvironmentTo(Strings, UserOverrides);

  if UseDisplay then
  begin
    // assignment is not allowed in a sorted list
    // Strings.Values['DISPLAY']:=Display;
    idx := Strings.IndexOfName('DISPLAY');
    if idx <> -1 then
      Strings.Delete(idx);
    Strings.Add('DISPLAY=' + Display);
  end;
end;

procedure TRunParamsOptionsMode.AssignTo(Dest: TPersistent);
var
  ADest: TRunParamsOptionsMode;
begin
  inherited AssignTo(Dest);

  if Dest is TRunParamsOptionsMode then
  begin
    ADest := Dest as TRunParamsOptionsMode;
    ADest.SaveIn := SaveIn;
  end;
end;

procedure TRunParamsOptionsMode.Clear;
begin
  inherited Clear;

  SaveIn := rpsLPS;
  LaunchingApplicationPathPlusParams := GetDefaultLaunchingApplicationPathPlusParams;
end;


end.

