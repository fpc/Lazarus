{
 /***************************************************************************
                               UTF8Process.pp
                               ---------------
                   Initial Revision  : Tue Dec 06 09:00:00 CET 2005


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit UTF8Process;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, FileUtil, LCLStrConsts;

type
  { TProcessUTF8 }

  TProcessUTF8 = class(TProcess)
  private
    FApplicationNameUTF8: string;
    FCommandLineUTF8: string;
    FConsoleTitleUTF8: string;
    FCurrentDirectoryUTF8: string;
    FDesktopUTF8: string;
    FEnvironmentUTF8: TStrings;
    procedure SetApplicationNameUTF8(const AValue: string);
    procedure SetCommandLineUTF8(const AValue: string);
    procedure SetConsoleTitleUTF8(const AValue: string);
    procedure SetCurrentDirectoryUTF8(const AValue: string);
    procedure SetDesktopUTF8(const AValue: string);
    procedure SetEnvironmentUTF8(const AValue: TStrings);
    procedure UpdateEnvironment;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
    property ApplicationName: string read FApplicationNameUTF8 write SetApplicationNameUTF8;
    property CommandLine: string read FCommandLineUTF8 write SetCommandLineUTF8;
    property ConsoleTitle: string read FConsoleTitleUTF8 write SetConsoleTitleUTF8;
    property CurrentDirectory: string read FCurrentDirectoryUTF8 write SetCurrentDirectoryUTF8;
    property Desktop: string read FDesktopUTF8 write SetDesktopUTF8;
    property Environment: TStrings read FEnvironmentUTF8 write SetEnvironmentUTF8;
  end;

procedure RunCmdFromPath(ProgramFilename, CmdLineParameters: string);
function FindFilenameOfCmd(ProgramFilename: string): string;

procedure Register;

implementation

{ TProcessUTF8 }

procedure TProcessUTF8.SetApplicationNameUTF8(const AValue: string);
begin
  if FApplicationNameUTF8=AValue then exit;
  FApplicationNameUTF8:=AValue;
  inherited ApplicationName:=UTF8ToSys(FApplicationNameUTF8);
end;

procedure TProcessUTF8.SetCommandLineUTF8(const AValue: string);
begin
  if FCommandLineUTF8=AValue then exit;
  FCommandLineUTF8:=AValue;
  inherited CommandLine:=UTF8ToSys(FCommandLineUTF8);
end;

procedure TProcessUTF8.SetConsoleTitleUTF8(const AValue: string);
begin
  if FConsoleTitleUTF8=AValue then exit;
  FConsoleTitleUTF8:=AValue;
  inherited ConsoleTitle:=UTF8ToSys(FConsoleTitleUTF8);
end;

procedure TProcessUTF8.SetCurrentDirectoryUTF8(const AValue: string);
begin
  if FCurrentDirectoryUTF8=AValue then exit;
  FCurrentDirectoryUTF8:=AValue;
  inherited CurrentDirectory:=UTF8ToSys(FCurrentDirectoryUTF8);
end;

procedure TProcessUTF8.SetDesktopUTF8(const AValue: string);
begin
  if FDesktopUTF8=AValue then exit;
  FDesktopUTF8:=AValue;
  inherited Desktop:=UTF8ToSys(FDesktopUTF8);
end;

procedure TProcessUTF8.SetEnvironmentUTF8(const AValue: TStrings);
begin
  if (FEnvironmentUTF8=AValue)
  or ((AValue<>nil) and FEnvironmentUTF8.Equals(AValue)) then exit;
  FEnvironmentUTF8.Assign(AValue);
end;

procedure TProcessUTF8.UpdateEnvironment;
var
  sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  try
    for i:=0 to FEnvironmentUTF8.Count-1 do
      sl.Add(UTF8ToSys(FEnvironmentUTF8[i]));
    inherited Environment:=sl;
  finally
    sl.Free;
  end;
end;

constructor TProcessUTF8.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnvironmentUTF8:=TStringList.Create;
end;

destructor TProcessUTF8.Destroy;
begin
  FreeAndNil(FEnvironmentUTF8);
  inherited Destroy;
end;

procedure TProcessUTF8.Execute;
begin
  UpdateEnvironment;
  inherited Execute;
end;

function FindFilenameOfCmd(ProgramFilename: string): string;
begin
  Result:=TrimFilename(ProgramFilename);
  if not FilenameIsAbsolute(Result) then begin
    if Pos(PathDelim,Result)>0 then begin
      // with sub directory => relative to current directory
      Result:=CleanAndExpandFilename(Result);
    end else begin
      // search in PATH
      Result:=FindDefaultExecutablePath(Result);
    end;
  end;
  if (Result<>'') and not FileExistsUTF8(Result) then
    Result:='';
end;

// Runs a short command which should point to an executable in
// the environment PATH
// For example: ProgramFilename=ls CmdLineParameters=-l /home
// Will locate and execute the file /bin/ls
// If the command isn't found, an exception will be raised
procedure RunCmdFromPath(ProgramFilename, CmdLineParameters: string);
var
  OldProgramFilename: String;
  BrowserProcess: TProcessUTF8;
begin
  OldProgramFilename:=ProgramFilename;
  ProgramFilename:=FindFilenameOfCmd(ProgramFilename);

  if ProgramFilename='' then
    raise EFOpenError.Create(Format(lisProgramFileNotFound, [OldProgramFilename]
      ));
  if not FileIsExecutable(ProgramFilename) then
    raise EFOpenError.Create(Format(lisCanNotExecute, [ProgramFilename]));

  // run
  BrowserProcess := TProcessUTF8.Create(nil);
  try
    // Encloses the executable with "" if it's name has spaces
    if Pos(' ',ProgramFilename)>0 then
      ProgramFilename:='"'+ProgramFilename+'"';

    BrowserProcess.CommandLine := ProgramFilename;
    if CmdLineParameters<>'' then
      BrowserProcess.CommandLine := BrowserProcess.CommandLine + ' ' + CmdLineParameters;
    BrowserProcess.Execute;
  finally
    BrowserProcess.Free;
  end;
end;

procedure Register;
begin
  RegisterComponents('System',[TProcessUTF8]);
end;

end.
