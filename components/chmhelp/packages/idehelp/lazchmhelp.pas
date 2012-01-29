{ Copyright (C) <2005> <Andrew Haines> lazchmhelp.pas

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{
ToDos:

- predefined procs:
   Anton: I've prepared some changes to ChmHelpPkg. Now help must be shown for
   such procs/funcs as Exit, Break, Inc, Dec, Continue etc. (full list can be
   found in fpc/rtl/inc/system.fpd)

- predefined identifiers:
  ref.chm contains help for FPC predefined identifiers - base types
  (e.g. Byte, Boolean etc.) and constants (True, False, ..)

- compiler directives:

- fpc messages:

- wiki:
  download content, without wiki links like upload, history, ...

- how to build lcl.chm?
  how to refer to fcl.chm?
  how to refer to rtl.chm?

- how to build ideintf.chm?
  how to refer to lcl.chm?
  how to refer to fcl.chm?
  how to refer to rtl.chm?

- how to build rtl.chm?

}

unit LazChmHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazHelpIntf, HelpIntfs, LazConfigStorage,
  PropEdits, LHelpControl, Controls, ChmLangRef, ChmLcl, ChmProg;
  
type
  
  { TChmHelpViewer }

  TChmHelpViewer = class(THelpViewer)
  private
    fHelpExe: String;
    fHelpLabel: String;
    fHelpConnection: TLHelpConnection;
    fChmsFilePath: String;
    fHelpExeParams: String;
    function GetHelpEXE: String;
    function DBFindViewer({%H-}HelpDB: THelpDatabase; {%H-}const MimeType: string;
      var {%H-}ErrMsg: string; out Viewer: THelpViewer): TShowHelpResult;
    function GetHelpLabel: String;
    procedure SetChmsFilePath(const AValue: String);
  protected
    function GetFileNameAndURL(RawUrl: String; out FileName: String; out URL: String): Boolean;
    procedure SetHelpEXE(AValue: String);
    procedure SetHelpLabel(AValue: String);
    function CheckBuildLHelp: Integer; // modal result
    function GetLazBuildEXE(out ALazBuild: String): Boolean;
    function PassTheBuck(Node: THelpNode; var ErrMsg: string): TShowHelpResult;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function SupportsTableOfContents: boolean; override;
    procedure ShowTableOfContents({%H-}Node: THelpNode); override;
    function SupportsMimeType(const AMimeType: string): boolean; override;
    function ShowNode(Node: THelpNode; var ErrMsg: string): TShowHelpResult; override;
    //procedure Hide; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Load(Storage: TConfigStorage); override;
    procedure Save(Storage: TConfigStorage); override;
    function GetLocalizedName: string; override;
  published
    property HelpEXE: String read GetHelpEXE write SetHelpEXE;
    property HelpLabel: String read GetHelpLabel write SetHelpLabel;
    property HelpFilesPath: String read fChmsFilePath write SetChmsFilePath;
    property HelpExeParams: String read fHelpExeParams write fHelpExeParams;
  end;
  
  procedure Register;

implementation

uses Process, MacroIntf, InterfaceBase, Forms, Dialogs, HelpFPDoc, IDEMsgIntf;

function FixSlash(AStr: String): String;
var
  WrongSlash: String;
  FP: Integer;
begin
  Result := AStr;
  case PathDelim of
    '/': WrongSlash := '\';
    '\': WrongSlash := '/';
  end;
  // fix wrong delim
  repeat
    FP := Pos(WrongSlash, Result);
    if FP > 0 then
      Result[FP] := PathDelim;
  until FP = 0;
  // fix double path delim
  repeat
    FP := Pos(PathDelim+PathDelim, Result);
    if FP <> 0 then
      Delete(Result, FP, 1);
  until FP = 0;
end;

{ TChmHelpViewer }

function TChmHelpViewer.DBFindViewer(HelpDB: THelpDatabase;
  const MimeType: string; var ErrMsg: string; out Viewer: THelpViewer
  ): TShowHelpResult;
begin
  Viewer:=Self;
  Result:=shrSuccess;
end;

function TChmHelpViewer.GetHelpLabel: String;
begin
  if Length(fHelpLabel) = 0 then
    fHelpLabel := 'lazhelp';
  Result := fHelpLabel;
end;

procedure TChmHelpViewer.SetChmsFilePath(const AValue: String);
begin
  if fChmsFilePath = AValue then Exit;
  fChmsFilePath := AppendPathDelim(AValue);
  if Assigned(LangRefHelpDatabase) then
    LangRefHelpDatabase.LoadKeywordList(fChmsFilePath);
  if Assigned(FPCDirectivesHelpDatabase) then
    FPCDirectivesHelpDatabase.DocsDir := fChmsFilePath;
end;

function TChmHelpViewer.GetHelpEXE: String;
begin
  if fHelpExe <> '' then
    Exit(fHelpExe);
  Result := '$(LazarusDir)/components/chmhelp/lhelp/lhelp$(ExeExt)';
  if not IDEMacros.SubstituteMacros(Result) then
    Exit('');
  Result := FixSlash(Result);
end;

function TChmHelpViewer.GetFileNameAndURL(RawUrl:String; out FileName: String; out URL: String
  ): Boolean;
var
  fPos: Integer;
begin
  Result := False;

  fPos := Pos(':/', RawUrl);
  if fPos = 0 then exit;
  FileName := Copy(RawUrl, 1, fPos-1);
  URL := Copy(RawUrl, fPos+2, Length(RawUrl)-(fPos-2));
  Result := True;
end;

procedure TChmHelpViewer.SetHelpEXE(AValue: String);
begin
  fHelpExe := AValue;
end;

procedure TChmHelpViewer.SetHelpLabel(AValue: String);
var
  i: Integer;
begin
 fHelpLabel := AValue;
 for i := 1 to Length(fHelpLabel) do
   if not (fHelpLabel[i] in ['a'..'z', '0'..'9', 'A'..'Z']) then
     fHelpLabel[i] := '_';
end;

function TChmHelpViewer.CheckBuildLHelp: Integer;
var
  Proc: TProcess;
  Lazbuild: String;
  LHelpProject: String;
  WS: String;
  LastWasEOL: Boolean;
  BufC: Char;
  Buffer: array[0..511] of char;
  BufP: Integer;
begin
  Result := mrCancel;

  if FileExistsUTF8(HelpExe) = True then
    Exit(mrOK);

  if not GetLazBuildEXE(Lazbuild) then
    Exit;

  LHelpProject := FixSlash('$(LazarusDir)/components/chmhelp/lhelp/lhelp.lpi');

  if not (IDEMacros.SubstituteMacros(LHelpProject)
          and FileExistsUTF8(LHelpProject))
  then
    Exit;

  WS := '--ws='+LCLPlatformDirNames[WidgetSet.LCLPlatform];

  //Result := MessageDlg('The help viewer is not compiled yet. Try to compile it now?', mtConfirmation, mbYesNo ,0);
  //if Result <> mrYes then
  //  Exit;

  Proc := TProcess.Create(nil);
  {$if (fpc_version=2) and (fpc_release<5)}
  Proc.CommandLine := Utf8ToSys(Lazbuild) + ' ' + WS
                                          + ' ' + Utf8ToSys(LHelpProject);
  {$else}
  Proc.Executable := Utf8ToSys(Lazbuild);
  Proc.Parameters.Add(WS);
  Proc.Parameters.Add(Utf8ToSys(LHelpProject));
  {$endif}
  Proc.Options := [poUsePipes, poStderrToOutPut];
  Proc.Execute;


  BufP := 0;
  LastWasEOL:= False;

  IDEMessagesWindow.BeginBlock;
  IDEMessagesWindow.AddMsg('- Building lhelp -','',0);

  LHelpProject := FixSlash('$(LazarusDir)/components/chmhelp/lhelp/');
  IDEMacros.SubstituteMacros(LHelpProject);
  while Proc.Running do begin
    while Proc.Output.NumBytesAvailable > 0 do
    begin
      BufC := Char(Proc.Output.ReadByte);
      if LastWasEOL then
      begin
        if not(BufC in [#13, #10]) then
        begin

          IDEMessagesWindow.AddMsg(PChar(@Buffer[0]),LHelpProject,1);
          Buffer[0] := BufC;
          BufP := 1;
          LastWasEOL:=False;
        end;
        Continue;
      end;

      if BufC in [#13, #10] then
      begin
        LastWasEOL:=True;
        Buffer[BufP] := #0;
        Inc(BufP);
      end
      else
      begin
        Buffer[BufP] := BufC;
        Inc(BufP);
      end;
    end;
    Sleep(20);
    Application.ProcessMessages;
  end;

  if BufP > 0 then
    IDEMessagesWindow.AddMsg(PChar(@Buffer[0]),LHelpProject,0);


  if Proc.ExitStatus = 0 then
    Result := mrOK;
  Proc.Free;

  IDEMessagesWindow.EndBlock;

  if Result = mrOK then
end;

function TChmHelpViewer.GetLazBuildEXE(out ALazBuild: String): Boolean;
var
  LazBuildMacro: String;
begin
   Result := False;
   LazBuildMacro:= '$(LazarusDir)/$MakeExe(lazbuild)';
   Result := IDEMacros.SubstituteMacros(LazBuildMacro)
             and FileExistsUTF8(LazBuildMacro);
   if Result then
     ALazBuild := FixSlash(LazBuildMacro);
end;

function TChmHelpViewer.PassTheBuck(Node: THelpNode; var ErrMsg: string
  ): TShowHelpResult;
var
 I: Integer;
 Viewer: THelpViewer;
begin
  Result := shrViewerNotFound;
  ErrMsg := 'Attempted to find viewer for "'+ Node.URL + '" failed.';
  for I := 0 to HelpViewers.Count-1 do
  begin
    Viewer := HelpViewers.Items[I];
    if (Viewer.ClassType <> ClassType) and Viewer.SupportsMimeType('text/html') then
      Exit(Viewer.ShowNode(Node, ErrMsg));
  end;
end;

constructor TChmHelpViewer.Create(TheOwner: TComponent);
var
  i: Integer;
  DB: TFPDocHTMLHelpDatabase;
  BaseURL: THelpBaseURLObject;
begin
  inherited Create(TheOwner);
  fHelpConnection := TLHelpConnection.Create;
  fHelpConnection.ProcessWhileWaiting:=@Application.ProcessMessages;
  AddSupportedMimeType('application/x-chm');
  for i := 0 to HelpDatabases.Count-1 do begin
    DB := TFPDocHTMLHelpDatabase(HelpDatabases.Items[i]);
    BaseURL := THelpBaseURLObject(DB.BasePathObject);
    if (DB.ID = 'RTLUnits') and (BaseURL.BaseURL = '') then
    begin
      BaseURL.BaseURL := 'rtl.chm://';
      DB.OnFindViewer:=@DBFindViewer;
    end else if (DB.ID = 'FCLUnits') and (BaseURL.BaseURL = '') then
    begin
      BaseURL.BaseURL := 'fcl.chm://';
      DB.OnFindViewer:=@DBFindViewer;
    end else if (DB.ID = 'LCLUnits') and (BaseURL.BaseURL = '') then
    begin
      BaseURL.BaseURL := 'lcl.chm://';
      DB.OnFindViewer:=@DBFindViewer;
    end;
  end;
end;

destructor TChmHelpViewer.Destroy;
begin
  fHelpConnection.Free;
  inherited Destroy;
end;

function TChmHelpViewer.SupportsTableOfContents: boolean;
begin
  Result:=True;
end;

procedure TChmHelpViewer.ShowTableOfContents(Node: THelpNode);
begin
//  inherited ShowTableOfContents(Node);
end;

function TChmHelpViewer.SupportsMimeType(const AMimeType: string): boolean;
begin
  Result := inherited;
end;

function TChmHelpViewer.ShowNode(Node: THelpNode; var ErrMsg: string
  ): TShowHelpResult;
var
  FileName: String;
  Url: String;
  Res: TLHelpResponse;
  DocsDir: String;
  Proc: TProcess;
begin
  if Pos('file://', Node.URL) = 1 then
  begin
    Result := PassTheBuck(Node, ErrMsg);
    Exit;
  end;
  Result:=shrNone;
  if (ExtractFileNameOnly(HelpEXE) = 'lhelp') and (CheckBuildLHelp <> mrOK) then begin
    ErrMsg := 'The program "' + HelpEXE + '" doesn''t seem to exist'+LineEnding+
              'or could not be built!';
    Exit(shrViewerNotFound);
  end;
  if not GetFileNameAndURL(Node.Url, FileName, Url) then begin
    ErrMsg := 'Couldn''t read the file/URL correctly';
    Exit(shrDatabaseNotFound);
  end;

  if HelpFilesPath = '' then
  begin
    DocsDir := FixSlash('$(LazarusDir)/docs/html/');
    IDEMacros.SubstituteMacros(DocsDir);
  end
  else
    DocsDir := fChmsFilePath;

  if not FileExistsUTF8(DocsDir+FileName) then
  begin
    Result := shrDatabaseNotFound;
    ErrMsg := FileName +' not found. Please put the chm help files in '+ LineEnding
                       +DocsDir+  LineEnding
                       +' or set the path to lcl.chm rtl.chm fcl.chm with "HelpFilesPath" in '
                       +' Environment Options -> Help -> Help Options ->'+LineEnding
                       +' under HelpViewers - CHMHelpViewer';
    Exit;
  end;

  FileName := IncludeTrailingPathDelimiter(DocsDir)+FileName;

  if ExtractFileNameOnly(HelpExe) = 'lhelp' then begin
    fHelpConnection.StartHelpServer(HelpLabel, HelpExe);
    Res := fHelpConnection.OpenURL(FileName, Url);
  end else begin
    if Trim(fHelpExeParams) = '' then
    begin
      Result := shrViewerError;
      ErrMsg := 'If you do not use "lhelp" as viewer you have to setup '
              + 'HelpExeParams correctly in' + sLineBreak
              + 'Environment Options -> Help -> Help Options -> '
              + 'under HelpViewers - CHM Help Viewer' + sLineBreak
              + 'e.g. for HH.EXE (HTML Help in Windows) it must be' + sLineBreak
              + '  "%s::%s"' + sLineBreak
              + 'where first %s will be replaced by CHM file name' + sLineBreak
              + 'and the second one will be replaced by URL';
      Exit;
    end;
    Proc := TProcess.Create(nil);
    try
      {$if (fpc_version=2) and (fpc_release<5)}
      Proc.CommandLine := Utf8ToSys(fHelpExe + ' ' + Format(fHelpExeParams, [FileName, Url]));
      {$else}
      Proc.Executable := Utf8ToSys(fHelpExe);
      Proc.Parameters.Add(Utf8ToSys(Format(fHelpExeParams, [FileName, Url])));
      {$endif}
      Proc.Execute;
      Res := srSuccess;
    except
      Res := srUnknown;
    end;
    Proc.Free;
  end;

  case Res of
    srSuccess: Result := shrSuccess;
    srNoAnswer: Result := shrSuccess;
  else
    Result := shrNone;
    ErrMsg := 'Unknown error showing '+URL;
  end;

  //WriteLn('LOADING URL = ', Node.URL);
end;

procedure TChmHelpViewer.Assign(Source: TPersistent);
var
  Viewer: TChmHelpViewer;
begin
  if Source is TChmHelpViewer then begin
    Viewer:=TChmHelpViewer(Source);
    HelpEXE:=Viewer.HelpEXE;
    HelpLabel:=Viewer.HelpLabel;
    HelpFilesPath:=Viewer.HelpFilesPath;
  end;
  inherited Assign(Source);
end;

procedure TChmHelpViewer.Load(Storage: TConfigStorage);
begin
  HelpEXE:=Storage.GetValue('CHMHelp/Exe','');
  HelpExeParams := Storage.GetValue('CHMHelp/ExeParams','');
  HelpLabel:=Storage.GetValue('CHMHelp/Name','lazhelp');
  HelpFilesPath := Storage.GetValue('CHMHelp/FilesPath','');
end;

procedure TChmHelpViewer.Save(Storage: TConfigStorage);
begin
  Storage.SetDeleteValue('CHMHelp/Exe',HelpEXE,'');
  Storage.SetDeleteValue('CHMHelp/ExeParams',HelpExeParams,'');
  Storage.SetDeleteValue('CHMHelp/Name',HelpLabel,'lazhelp');
  Storage.SetDeleteValue('CHMHelp/FilesPath',HelpFilesPath,'');
end;

function TChmHelpViewer.GetLocalizedName: string;
begin
  Result := 'CHM Help Viewer';
end;

procedure Register;
var
  ChmHelp: TChmHelpViewer;
begin
  ChmHelp := TChmHelpViewer.Create(nil);
  HelpViewers.RegisterViewer(ChmHelp);
  RegisterLangRefHelpDatabase;
  LangRefHelpDatabase.OnFindViewer := @ChmHelp.DBFindViewer;
  RegisterLclHelpDatabase;
  LCLHelpDatabase.OnFindViewer := @ChmHelp.DBFindViewer;
  RegisterFPCDirectivesHelpDatabase;
  FPCDirectivesHelpDatabase.OnFindViewer := @ChmHelp.DBFindViewer;
end;

initialization
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TChmHelpViewer,'HelpEXE',TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TChmHelpViewer,'HelpFilesPath',TDirectoryPropertyEditor);
end.

