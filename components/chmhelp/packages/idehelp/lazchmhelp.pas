{ Copyright (C) <2005-2014> <Andrew Haines>, Lazarus contributors

  lazchmhelp.pas

  Lazarus IDE support for lhelp/chm help files. Can start and control lhelp application.

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
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}
unit LazChmHelp;

{$mode objfpc}{$H+}
{ $DEFINE CHMLOADTIMES}

interface

uses
  Classes, SysUtils,
  // LazUtils
  FileUtil, LazLogger, LazFileUtils, LazConfigStorage, UTF8Process,
  // LCL
  Controls, Forms, Dialogs, LazHelpIntf, HelpIntfs, LCLPlatformDef, InterfaceBase,
  // IdeIntf
  PropEdits, IDEDialogs, MacroIntf, LazIDEIntf, IDEExternToolIntf, HelpFPDoc,
  IDEHelpIntf,
  // ChmHelp
  LHelpControl, ChmLangRef, ChmLcl, ChmProg;
  
resourcestring
  HELP_CURRENT_MENU  = '&Help';
  HELP_CURRENT_IDECMD  = 'Show help';
  HELP_CATEGORY_IDECMD = 'CHM Help';
  HELP_MissingLhelp = 'Missing lhelp';
  HELP_UnableToFindTheLhelpViewerPleaseCompileTheLhelpPro = 'Unable to find '
    +'the lhelp viewer:'
    +'%s%s'
    +'%sPlease compile the lhelp project:'
    +'%s%s';

type
  
  { TChmHelpViewer }

  TChmHelpViewer = class(THelpViewer)
  private
    // Full path and filename to help executable (lhelp)
    fHelpExe: String;
    // ID used for SimpleIPC identification:
    fHelpLabel: String;
    fHelpConnection: TLHelpConnection;
    fCHMSearchPath: String;
    fHelpExeParams: String;
    function DBFindViewer({%H-}HelpDB: THelpDatabase; {%H-}const {%H-}MimeType: string;
      var {%H-}ErrMsg: string; out Viewer: THelpViewer): TShowHelpResult;
    function GetHelpLabel: String;
    // Shows all chm files in the given search path. Requires help viewer to be running already
    procedure OpenAllCHMsInSearchPath(const SearchPath: String);
    procedure SetChmsFilePath(const AValue: String);
    procedure SetHelpEXE(AValue: String);
  protected
    function GetFileNameAndURL(RawUrl: String; out FileName: String; out URL: String): Boolean;
    // Sets label/ID used for simpleipc communications
    procedure SetHelpLabel(AValue: String);
    // Check for lhelp executable, if not present, build if possible
    function CheckBuildLHelp(AForce: Boolean = False): Integer; // modal result
    // Get full path of lazbuild executable
    function GetLazBuildEXE(out ALazBuild: String): Boolean;
    function PassTheBuck(Node: THelpNode; var ErrMsg: string): TShowHelpResult;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function SupportsTableOfContents: boolean; override;
    procedure ShowTableOfContents({%H-}Node: THelpNode); override;
    function SupportsMimeType(const AMimeType: string): boolean; override;
    // Shows all chm help files. Opens lhelp if necessary. Used by menu commands.
    procedure ShowAllHelp(Sender: TObject);
    // Shows help for the indicated node or an error message if it cannot. Opens lhelp if necessary
    function ShowNode(Node: THelpNode; var ErrMsg: string): TShowHelpResult; override;
    //procedure Hide; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure Load(Storage: TConfigStorage); override;
    procedure Save(Storage: TConfigStorage); override;
    // Get localized name for CHM Help Viewer string
    function GetLocalizedName: string; override;
    function GetHelpEXE: String; // macros resolved, see property HelpEXE
    function GetHelpFilesPath: String; // macros resolved, see property HelpFilesPath
    // ID used for SimpleIPC communication with the help viewer
    // Used to be published before Laz 1.3=>saves in tools/options;
    // this is no longer necessary or desirable as helplabel is unique per session
    // and calculated on-the-fly.
    property HelpLabel: String read GetHelpLabel write SetHelpLabel;
  published
    // Path and filename of help executable
    // With macros, see GetHelpEXE
    property HelpEXE: String read fHelpEXE write SetHelpEXE;
    // Where to look for help files.
    // Directories separated with semicolon, with macros, see GetHelpFilesPath
    property HelpFilesPath: String read fCHMSearchPath write SetChmsFilePath;
    // Additional parameters to pass to help executable
    property HelpExeParams: String read fHelpExeParams write fHelpExeParams;
  end;

procedure Register;

implementation

const
  // Part of help name. Stored/retrieved in Lazarus options CHMHelp/Name.
  // Do not localize.
  CHMHelpName='lazhelp';
  DigitsInPID=5; // Number of digits in the formatted PID according to the Help Protocol

// Formats (part of) process ID according to help protocol for
// use in ipcname parameter
function HelpProtocolFormattedPID: string;
begin
  // Make sure at least DigitsInPID digits present even if 0
  result := copy(inttostr(GetProcessID)+
    StringOfChar('0',DigitsInPID),1,DigitsInPID)
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

  // disable showing CodeBrowser on unknown identifiers. LHelp has its own
  // search function.
  LazarusHelp.ShowCodeBrowserOnUnknownIdentifier:=false;
end;


{ TChmHelpViewer }

function TChmHelpViewer.DBFindViewer(HelpDB: THelpDatabase;
  const MimeType: string; var ErrMsg: string; out Viewer: THelpViewer
  ): TShowHelpResult;
begin
  Viewer := Self;
  Result := shrSuccess;
end;

function TChmHelpViewer.GetHelpLabel: String;
begin
  // fHelpLabel is used for SimpleIPC server id;
  // lhelp protocol specifies server-dependent constant string
  // followed by formatted string representation of (part of) processid/PID
  // Autocalculate if needed:
  if Length(fHelpLabel) = 0 then
    fHelpLabel := CHMHelpName + HelpProtocolFormattedPID;
  Result := fHelpLabel;
end;

procedure TChmHelpViewer.OpenAllCHMsInSearchPath(const SearchPath: String);
var
  CHMFiles: TStringList;
  SearchPaths: TStringList; // SearchPath split to a StringList
  SearchFiles: TStringList; // Files found in SearchPath
  i: integer;
  {$IFDEF CHMLOADTIMES}
  StartTime: TDateTime;
  {$ENDIF}
begin
  { Alternative:
    Open registered chm help files (no online html help etc)
    Using SupportsMimetype would seem to be the solution here.
    This does mean that all classes providing chm file support add
    AddSupportedMimeType('application/x-chm');
    in their constructors as they normally inherit
    text/html from their HTML help parents.
    Also, this will not work for other .chm files in the relevant directories.
    this still does not open all help files such as rtl.chm

   for i := 0 to HelpDatabases.Count-1 do
   begin
     if HelpDatabases[i].SupportsMimeType('application/x-chm') then
     begin
       HelpDatabases[i].ShowTableOfContents;
       Sleep(200); //give viewer chance to open file.
       Application.ProcessMessages;
     end;
   end;
   }
  // Just open all CHM files in all directories+subdirs in ;-delimited searchpath:
  SearchPaths:=TStringList.Create;
  CHMFiles:=TStringList.Create;
  try
    CHMFiles.Sorted:=true;
    CHMFiles.Duplicates:=dupIgnore;
    SearchPaths.Delimiter:=';';
    SearchPaths.StrictDelimiter:=false;
    SearchPaths.DelimitedText:=SearchPath;
    {$IFDEF CHMLOADTIMES}
    StartTime := Now;
    {$ENDIF}
    for i := 0 to SearchPaths.Count-1 do
    begin
      // Note: FindAllFiles has a SearchPath parameter that is a *single* directory,
      SearchFiles := FindAllFiles(SearchPaths[i], '*.chm;*.CHM;*.Chm', False);
      CHMFiles.AddStrings(SearchFiles);
      SearchFiles.Free;
    end;
    {$IFDEF CHMLOADTIMES}
    DebugLn(['CHMLOADTIMES: ',Format('Searching files in %s took %d ms',[SearchPath,DateTimeToTimeStamp(Now-StartTime).Time])]);
    StartTime := Now;
    {$ENDIF}

    fHelpConnection.BeginUpdate;
    for i := 0 to CHMFiles.Count-1 do
    begin
      if UpperCase(ExtractFileExt(CHMFiles[i]))='.CHM' then
      begin
        fHelpConnection.OpenURL(CHMFiles[i], '/index.html');
        // This is probably no longer necessary as we're now waiting for the viewer's
        // response to our OpenURL command; the viewer can process at it's own speed
        //Sleep(200); //give viewer chance to open file.
        //Application.ProcessMessages;
      end;
    end;
    {$IFDEF CHMLOADTIMES}
    DebugLn(['CHMLOADTIMES: ',Format('Loading chm files took %d ms',[DateTimeToTimeStamp(Now-StartTime).Time])]);
    {$ENDIF}
  finally
    fHelpConnection.EndUpdate;
    CHMFiles.Free;
    SearchPaths.Free;
  end;
end;

procedure TChmHelpViewer.SetChmsFilePath(const AValue: String);
var
  CurrentHelpFilesPath: String;
begin
  if fCHMSearchPath = AValue then Exit;
  fCHMSearchPath := AppendPathDelim(AValue);
  CurrentHelpFilesPath := GetHelpFilesPath;
  if Assigned(LangRefHelpDatabase) then
    LangRefHelpDatabase.LoadKeywordList(CurrentHelpFilesPath);
  if Assigned(FPCDirectivesHelpDatabase) then
    FPCDirectivesHelpDatabase.CHMSearchPath := CurrentHelpFilesPath;
end;

procedure TChmHelpViewer.SetHelpEXE(AValue: String);
begin
  if fHelpEXE=AValue then Exit;
  fHelpEXE := AValue;
end;

function TChmHelpViewer.GetHelpEXE: String;
begin
  Result := fHelpExe;
  if Result='' then
    Result := GetForcedPathDelims('$(LazarusDir)/components/chmhelp/lhelp/lhelp$(ExeExt)');
  if not IDEMacros.SubstituteMacros(Result) then
    Exit('');
end;

function TChmHelpViewer.GetHelpFilesPath: String;
begin
  Result:=fCHMSearchPath;
  if Result='' then
    Result:='$(LazarusDir)/docs/chm;$(LazarusDir)/docs/html;$(LazarusDir)/docs/html/lcl';
  IDEMacros.SubstituteMacros(Result);
  Result:=MinimizeSearchPath(GetForcedPathDelims(Result));
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

procedure TChmHelpViewer.SetHelpLabel(AValue: String);
var
  i: Integer;
begin
  //ShowMessage('TChmHelpViewer.SetHelpLabel: value passed: '+AValue);
  fHelpLabel := AValue;
  // Strip out difficult characters as per Help Protocol
  for i := 1 to Length(fHelpLabel) do
  begin
    if not (fHelpLabel[i] in ['a'..'z', '0'..'9', 'A'..'Z']) then
      fHelpLabel[i] := '_';
  end;
  // Validity check for labels like
  // lazhelp501204548
  // i.e. with too many numbers in the PID part.
  // These may be left over in option files from earlier Lazarus versions.
  // If too many numbers at end, replace with default
  if fHelpLabel[length(FHelpLabel)] in ['0'..'9'] then
  begin
    for i := Length(fHelpLabel) downto 1 do
    begin
      if not(fHelpLabel[i] in ['0'..'9']) then
      begin
        if (Length(fHelpLabel)-i>DigitsInPID) then
        begin
          Debugln('TChmHelpViewer.SetHelpLabel: got '+fHelpLabel+'. Help protocol does not allow this many digits. Resetting to empty string.');
          // Revert to default
          fHelpLabel := CHMHelpName + HelpProtocolFormattedPID;
        end;
        break;
      end;
    end;
  end;
end;

function TChmHelpViewer.CheckBuildLHelp(AForce: Boolean): Integer;
var
  Lazbuild: String;
  LHelpProject: String;
  LHelpProjectDir: String;
  WS: String;
  PCP: String;
  Tool: TIDEExternalToolOptions;
  OrigFile: String;
  TmpFile: String = '';
  ExistingFile: Boolean;
begin
  Result := mrCancel;

  ExistingFile := FileExistsUTF8(GetHelpExe);

  if ExistingFile and not AForce then
    Exit(mrOK);

  if ExistingFile then
  begin
    OrigFile:=StringReplace(GetHelpEXE, PathDelim+PathDelim, PathDelim, [rfReplaceAll]);
    TmpFile:=ChangeFileExt(OrigFile, '.tmp');
    //debugln(['TChmHelpViewer.CheckBuildLHelp forced rebuilding of lhelp']);
    if FileExistsUTF8(TmpFile) then
      DeleteFileUTF8(TmpFile);
    if not RenameFile(OrigFile, TmpFile) then
    begin
      debugln(['TChmHelpViewer.CheckBuildLHelp no permission to modify lhelp executable']);
      // we don't have permission to move or rebuild lhelp so exit
      // Exit with mrYes anyway since lhelp is still present, just an older version
      Exit(mrYes);
    end;
  end;

  if not GetLazBuildEXE(Lazbuild) then
  begin
    debugln(['TChmHelpViewer.CheckBuildLHelp failed because lazbuild not found']);
    Exit;
  end;

  LHelpProject := '$(LazarusDir)'+GetForcedPathDelims('/components/chmhelp/lhelp/lhelp.lpi');
  if not IDEMacros.SubstituteMacros(LHelpProject) then exit;
  LHelpProject:=TrimFilename(LHelpProject);
  if not FileExistsUTF8(LHelpProject) then
  begin
    debugln(['TChmHelpViewer.CheckBuildLHelp failed because lhelp.lpi not found']);
    Exit;
  end;
  LHelpProjectDir := ExtractFilePath(LHelpProject);

  WS := '--ws='+LCLPlatformDirNames[WidgetSet.LCLPlatform];
  PCP := '--pcp='+LazarusIDE.GetPrimaryConfigPath;

  //Result := MessageDlg('The help viewer is not compiled yet. Try to compile it now?', mtConfirmation, mbYesNo ,0);
  //if Result <> mrYes then
  //  Exit;

  Tool:=TIDEExternalToolOptions.Create;
  try
    Tool.Title:='- Building lhelp -';
    Tool.WorkingDirectory:=LHelpProjectDir;
    Tool.Executable:=Lazbuild;
    Tool.CmdLineParams:=QuotedStr(WS)+' '+QuotedStr(PCP)+' '+QuotedStr(LHelpProject);
    Tool.Parsers.Add(SubToolFPC);
    Tool.Parsers.Add(SubToolMake);
    if RunExternalTool(Tool) then
    begin
      Result:=mrOk;
      if (TmpFile <> '') and FileExistsUTF8(TmpFile)  then
        DeleteFileUTF8(TmpFile);
    end
    else
    begin
      debugln(['TChmHelpViewer.CheckBuildLHelp failed building of lhelp. Trying to use old version']);
      // compile failed
      // try to copy back the old lhelp if it existed
      if (TmpFile <> '') and FileExistsUTF8(TmpFile) and RenameFile(TmpFile, OrigFile) then
        Result := mrOK;
    end;
  finally
    Tool.Free;
  end;
end;

function TChmHelpViewer.GetLazBuildEXE(out ALazBuild: String): Boolean;
begin
  Result := False;
  ALazBuild:= '$(LazarusDir)/$MakeExe(lazbuild)';
  if not IDEMacros.SubstituteMacros(ALazBuild) then exit;
  ALazBuild:=TrimFilename(GetForcedPathDelims(ALazBuild));
  Result:=FileExistsUTF8(ALazBuild);
end;

function TChmHelpViewer.PassTheBuck(Node: THelpNode; var ErrMsg: string
  ): TShowHelpResult;
var
  I: Integer;
  Viewer: THelpViewer;
begin
  Result := shrViewerNotFound;
  ErrMsg := 'TChmHelpViewer: Attempted to find viewer for "'+ Node.URL + '" failed.';
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
  for i := 0 to HelpDatabases.Count-1 do
  begin
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
    end else if (DB.ID = 'LazUtilsUnits') and (BaseURL.BaseURL = '') then
    begin
      BaseURL.BaseURL := 'lazutils.chm://';
      DB.OnFindViewer:=@DBFindViewer;
    end;
  end;
end;

destructor TChmHelpViewer.Destroy;
begin
  // Try to close lhelp if we had opened it before; ignore response
  try
    fHelpConnection.RunMiscCommand(LHelpControl.mrClose);
  except
    // ignore errors; let user close it himself
    on E: Exception do
    begin
      debugln('TChmHelpViewer.Destroy: exception '+E.Message+' when trying to send mrClose on viewer');
    end;
  end;

  fHelpConnection.Free;
  inherited Destroy;
end;

function TChmHelpViewer.SupportsTableOfContents: boolean;
begin
  Result := True;
end;

procedure TChmHelpViewer.ShowTableOfContents(Node: THelpNode);
begin
//  inherited ShowTableOfContents(Node);
end;

function TChmHelpViewer.SupportsMimeType(const AMimeType: string): boolean;
begin
  Result := inherited;
end;

procedure TChmHelpViewer.ShowAllHelp(Sender: TObject);
var
  Response: TLHelpResponse;
  SearchPath: String; //; delimited list of directories
  HelpExeFileName: String;
begin
  // Make sure the lhelp help viewer exists; build it if doesn't and it is lhelp
  HelpExeFileName:=GetHelpExe;
  if (not FileExistsUTF8(HelpExeFileName)) and
    ((ExtractFileNameOnly(HelpExeFileName) = 'lhelp') and
    (CheckBuildLHelp <> mrOK)) then
  begin
    IDEMessageDialog(HELP_MissingLhelp,
      Format(HELP_UnableToFindTheLhelpViewerPleaseCompileTheLhelpPro,
             [LineEnding, HelpExeFileName, LineEnding+LineEnding, LineEnding,
              GetForcedPathDelims('components/chmhelp/lhelp/lhelp.lpi')]),
      mtError,[mbCancel]);
    Debugln(Format('ChmHelpViewer: '+HELP_UnableToFindTheLhelpViewerPleaseCompileTheLhelpPro,
      [LineEnding, HelpExeFileName, LineEnding+LineEnding, LineEnding,
      GetForcedPathDelims('components/chmhelp/lhelp/lhelp.lpi')]));
    exit;
  end;

  SearchPath := GetHelpFilesPath;
  // Start up help viewer if needed - and tell it to hide
  if not(fHelpConnection.ServerRunning) then
  begin
    fHelpConnection.StartHelpServer(HelpLabel, HelpExeFileName, true);
    Response := fHelpConnection.RunMiscCommand(mrVersion);
    if Response <> srSuccess then
    begin
      debugln('TChmHelpViewer: Help viewer does not support our protocol version ('+PROTOCOL_VERSION +'). Response was: ord: '+inttostr(ord(Response)))
    end
    else
    begin
      // Open all chm files after it has started, while still hidden
      OpenAllCHMsInSearchPath(SearchPath);
      // Instruct viewer to show its GUI
      Response := fHelpConnection.RunMiscCommand(mrShow);
      if Response <> srSuccess then
        debugln('TChmHelpViewer: Help viewer gave error response to mrShow command. Response was: ord: '+inttostr(ord(Response)));
    end;
  end;
end;

function TChmHelpViewer.ShowNode(Node: THelpNode; var ErrMsg: string
  ): TShowHelpResult;
var
  FileName: String;
  Url: String;
  Response: TLHelpResponse;
  SearchPath: String; //; delimited list of directories
  Proc: TProcessUTF8;
  FoundFileName: String;
  LHelpPath: String;
  WasRunning: boolean;
  {$IFDEF CHMLOADTIMES}
  TotalTime: TDateTime;
  StartTime: TDateTime;
  {$ENDIF}
begin
  if Pos('file://', Node.URL) = 1 then
  begin
    Result := PassTheBuck(Node, ErrMsg);
    Exit;
  end;
  Result := shrNone;
  if (ExtractFileNameOnly(GetHelpEXE) = 'lhelp') and (CheckBuildLHelp <> mrOK) then
  begin
    ErrMsg := 'The program "' + GetHelpEXE + '" does not seem to exist'+LineEnding+
      'or could not be built!';
    Exit(shrViewerNotFound);
  end;
  if not GetFileNameAndURL(Node.Url, FileName, Url) then
  begin
    ErrMsg := 'Could not read the file/URL correctly';
    Exit(shrDatabaseNotFound);
  end;

  SearchPath := GetHelpFilesPath;
  FoundFileName:=SearchFileInPath(Filename,'',SearchPath,';',[]);
  debugln(['TChmHelpViewer.ShowNode Filename="',Filename,'" SearchPath="',SearchPath,'" Found="',FoundFileName,'"']);

  if FoundFileName='' then
  begin
    Result := shrDatabaseNotFound;
    ErrMsg := FileName +' not found. Please put the chm help files in '+ LineEnding
      +SearchPath + LineEnding
      +' or set the path to lcl.chm rtl.chm fcl.chm with "HelpFilesPath" in '+LineEnding
      +' Environment Options -> Help -> Help Options ->'+LineEnding
      +' under HelpViewers - CHMHelpViewer';
    Exit;
  end;

  FileName := CleanAndExpandFilename(FoundFileName);

  if ExtractFileNameOnly(GetHelpExe) = 'lhelp' then
  begin
    WasRunning := fHelpConnection.ServerRunning;
    // Start server and tell it to hide
    // No use setting cursor to hourglass as that may take as long as the
    // waitforresponse timeout.
    {$IFDEF CHMLOADTIMES}
    TotalTime:=Now;
    StartTime:=Now;
    {$ENDIF}
    fHelpConnection.StartHelpServer(HelpLabel, GetHelpExe, true);
    {$IFDEF CHMLOADTIMES}
    DebugLn(['CHMLOADTIMES: ',Format('Starting LHelp took %d ms',[DateTimeToTimeStamp(Now-StartTime).Time])]);
    {$ENDIF}
    // If the server is not already running, open all chm files after it has started
    // This will allow cross-chm (LCL, FCL etc) searching and browsing in lhelp.
    if not(WasRunning) then
    begin
      if fHelpConnection.BeginUpdate = srError then
      begin
        // existing lhelp doesn't understand mrBeginUpdate and needs to be rebuilt
        //close lhelp
        if fHelpConnection.RunMiscCommand(LHelpControl.mrClose) <> srError then
        begin
          // force rebuild of lhelp
          // this may not succeed but the old lhelp will be restarted anyway and
          // just return error codes for unknown messages.
          if CheckBuildLHelp(True) = mrOK then
          begin
            // start it again
            Debugln(['TChmHelpViewer.ShowNode restarting lhelp to use updated protocols']);
            fHelpConnection.StartHelpServer(HelpLabel, GetHelpExe, true);
            // now run begin update
            fHelpConnection.BeginUpdate; // it inc's a value so calling it more than once doesn't hurt
          end;
        end;
      end;
      {$IFDEF CHMLOADTIMES}
      StartTime := Now;
      {$ENDIF}
      OpenAllCHMsInSearchPath(SearchPath);
      {$IFDEF CHMLOADTIMES}
      DebugLn(['CHMLOADTIMES: ',Format('Searching and Loading files took %d ms',[DateTimeToTimeStamp(Now-StartTime).Time])]);
      {$ENDIF}
      // Instruct viewer to show its GUI
      Response:=fHelpConnection.RunMiscCommand(mrShow);
      if Response<>srSuccess then
        debugln('Help viewer gave error response to mrShow command. Response was: ord: '+inttostr(ord(Response)));
    end;
    fHelpConnection.BeginUpdate;
    Response := fHelpConnection.OpenURL(FileName, Url);
    fHelpConnection.EndUpdate;
    if not WasRunning then
      fHelpConnection.EndUpdate;
    {$IFDEF CHMLOADTIMES}
    DebugLn(['CHMLOADTIMES: ',Format('Total start time was %d ms',[DateTimeToTimeStamp(Now-TotalTime).Time])]);
    {$ENDIF}
  end
  else
  begin
    if Trim(fHelpExeParams) = '' then
    begin
      Result := shrViewerError;
      ErrMsg := 'If you do not use "lhelp" as viewer you have to set up ' +
        'HelpExeParams correctly in' + LineEnding +
        'Tools -> Options -> Help -> Help Options -> ' +
        'under HelpViewers - CHM Help Viewer' + LineEnding +
        'e.g. for HH.EXE (HTML Help in Windows) it must be' + LineEnding +
        '  "%s::%s"' + LineEnding +
        'where first %s will be replaced by CHM file name' + LineEnding +
        'and the second one will be replaced by URL';
      Exit;
    end;

    Proc := TProcessUTF8.Create(nil);
    try
      Proc.InheritHandles := false;
      {$if (fpc_version=2) and (fpc_release<5)}
      Proc.CommandLine := GetHelpExe + ' ' + Format(fHelpExeParams, [FileName, Url]);
      {$else}
      LHelpPath:=GetHelpEXE;
      Proc.Executable := LHelpPath;
      {$IFDEF darwin}
      if DirectoryExistsUTF8(LHelpPath+'.app') then
        LHelpPath+='.app';
      if DirectoryExistsUTF8(LHelpPath) then
      begin
        // application bundle
        // to put lhelp into the foreground, use "open -n lhelp.app --args args"
        Proc.Executable := '/usr/bin/open';
        Proc.Parameters.Add('-n');
        Proc.Parameters.Add(LHelpPath);
        Proc.Parameters.Add('--args');
        Proc.Parameters.Add(Format(fHelpExeParams, [FileName, Url]));
      end;
      {$ENDIF}
      Proc.Parameters.Add(Format(fHelpExeParams, [FileName, Url]));
      {$endif}
      Proc.Execute;
      Response := srSuccess;
    except
      Response := srUnknown;
    end;
    Proc.Free;
  end;

  case Response of
    srSuccess: Result := shrSuccess;
    srNoAnswer: Result := shrSuccess;
    { Due to problems with SimpleIPC messages not arriving at the right time
    we will assume no answer actually means success. Otherwise:
      Result := shrHelpNotFound;
      ErrMsg := 'No answer from help viewer for URL '+URL;
    }
    srInvalidContext:
    begin
      Result := shrNone;
      ErrMsg := 'Invalid context showing '+URL;
    end;
    srInvalidFile:
    begin
      Result := shrNone;
      ErrMsg := 'Invalid file showing '+URL;
    end;
    srInvalidURL:
    begin
      Result := shrNone;
      ErrMsg := 'Invalid URL showing '+URL;
    end;
  else //srUnknown, srError
    Result := shrNone;
    ErrMsg := 'Unknown error showing '+URL;
  end;

  //DebugLn('LOADING URL = ', Node.URL);
end;

procedure TChmHelpViewer.Assign(Source: TPersistent);
var
  Viewer: TChmHelpViewer;
begin
  if Source is TChmHelpViewer then
  begin
    Viewer:=TChmHelpViewer(Source);
    HelpEXE:=Viewer.HelpEXE;
    HelpLabel:=Viewer.HelpLabel;
    HelpFilesPath:=Viewer.HelpFilesPath;
  end;
  inherited Assign(Source);
end;

procedure TChmHelpViewer.Load(Storage: TConfigStorage);
begin
  HelpEXE := Storage.GetValue('CHMHelp/Exe','');
  HelpExeParams := Storage.GetValue('CHMHelp/ExeParams','');
  // Precalculate label:
  HelpLabel := Storage.GetValue('CHMHelp/Name',CHMHelpName)+HelpProtocolFormattedPID;
  HelpFilesPath := Storage.GetValue('CHMHelp/FilesPath','');
end;

procedure TChmHelpViewer.Save(Storage: TConfigStorage);
begin
  Storage.SetDeleteValue('CHMHelp/Exe',HelpEXE,'');
  Storage.SetDeleteValue('CHMHelp/ExeParams',HelpExeParams,'');
  Storage.SetDeleteValue('CHMHelp/Name',CHMHelpName,CHMHelpName);
  Storage.SetDeleteValue('CHMHelp/FilesPath',HelpFilesPath,'');
end;

function TChmHelpViewer.GetLocalizedName: string;
begin
  Result := 'CHM Help Viewer';
end;

initialization
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TChmHelpViewer,'HelpEXE',TFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TChmHelpViewer,'HelpFilesPath',TDirectoryPropertyEditor);
end.

