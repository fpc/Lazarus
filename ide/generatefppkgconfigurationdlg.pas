unit GenerateFppkgConfigurationDlg;

{$mode objfpc}{$H+}

interface

uses
  // Rtl
  Classes,
  SysUtils,
  // Fcl
  fpmkunit,
  process,
  // Fppkg
  pkgglobals,
  // LazUtils
  LazFileUtils,
  LazFileCache,
  UTF8Process,
  // Lcl
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  // Codetools
  CodeToolManager,
  // IDE
  IDEProcs,
  LazConf,
  LazarusIDEStrConsts,
  InitialSetupProc,
  EnvironmentOpts,
  // Packager
  FppkgHelper,
  // Ideintf
  IDEDialogs;

type

  { TGenerateFppkgConfigurationDialog }

  TGenerateFppkgConfigurationDialog = class(TForm)
    FppkgLabel: TLabel;
    FpcPrefixCombobox: TComboBox;
    FppkgPrefixLabel: TLabel;
    InfoMemo: TMemo;
    BtnPanel: TPanel;
    FppkgWriteConfigButton: TButton;
    WarningsLabel: TLabel;
    BrowsePanel: TPanel;
    BrowseButton: TButton;

    procedure FormCreate(Sender: TObject);
    procedure FpcPrefixComboboxChange(Sender: TObject);
    procedure BrowseButtonClick(Sender: TObject);
    procedure FppkgWriteConfigButtonClick(Sender: TObject);
  private
    FCompiler: string;
    procedure SetCompiler(AValue: string);
    procedure SetFppkgCfgFilename(AValue: string);
  private
    fLastParsedFpcPrefix: string;
    fLastParsedFpcLibPath: string;
    FFppkgCfgFilename: string;
    function CheckFppkgQuality(APrefix: string; out LibPath, Note: string): TSDFilenameQuality;
    procedure UpdateFppkgNote;
    procedure SearchFppkgFpcPrefixCandidates;
    function CheckFpcmkcfgQuality(out Note: string): TSDFilenameQuality;
  public
    property Compiler: string read FCompiler write SetCompiler;
    property FppkgCfgFilename: string read FFppkgCfgFilename write SetFppkgCfgFilename;
  end;

var
  GenerateFppkgConfigurationDialog: TGenerateFppkgConfigurationDialog;

implementation

{$R *.lfm}

{ TGenerateFppkgConfigurationDialog }

procedure TGenerateFppkgConfigurationDialog.FormCreate(Sender: TObject);
begin
  Caption := lisGenerateFppkgConfigurationCaption;
  FppkgLabel.Caption := lisGenerateFppkgConfiguration;
  {$IFDEF WINDOWS}
  FppkgPrefixLabel.Caption:=Format(lisFppkgInstallationPath, [GetFPCVer+PathDelim+'units', GetFPCVer+PathDelim+'fpmkinst']);
  {$ELSE}
  FppkgPrefixLabel.Caption:=Format(lisFppkgInstallationPath, ['lib/fpc', 'lib64/fpc']);
  {$ENDIF WINDOWS}
  SearchFppkgFpcPrefixCandidates;
  FpcPrefixCombobox.Text := '';
  if FpcPrefixCombobox.Items.Count > 0 then
    FpcPrefixCombobox.ItemIndex := 0;
  WarningsLabel.Caption := lisFppkgConfGenProblems;
  FppkgWriteConfigButton.Caption := lisFppkgWriteConfigFile;
  BrowseButton.Caption:=lisPathEditBrowse;
  UpdateFppkgNote;
end;

procedure TGenerateFppkgConfigurationDialog.SearchFppkgFpcPrefixCandidates;

  function CheckPath(APath: string; List: TStrings): boolean;
  var
    LibPath, Note: String;
  begin
    Result:=false;
    if APath='' then exit;
    ForcePathDelims(APath);
    // check if already checked
    if Assigned(List) and (List.IndexOf(APath)>-1) then exit;

    if CheckFppkgQuality(APath, LibPath, Note) = sddqCompatible then
    begin
      List.Add(APath);
      Result := True;
    end;
  end;

var
  ChkPath: string;
begin
  FpcPrefixCombobox.Clear;

  ChkPath := ExtractFileDir(ExtractFileDir(EnvironmentOptions.GetParsedCompilerFilename));
  {$IFDEF WINDOWS}
  ChkPath := ExtractFileDir(ExtractFileDir(ChkPath));
  {$ENDIF WINDOWS}
  CheckPath(ChkPath, FpcPrefixCombobox.Items);

  {$IFDEF WINDOWS}
  CheckPath('C:\PP', FpcPrefixCombobox.Items);
  CheckPath('D:\PP', FpcPrefixCombobox.Items);
  CheckPath('C:\FPC', FpcPrefixCombobox.Items);
  CheckPath('D:\FPC', FpcPrefixCombobox.Items);
  {$ELSE}
  CheckPath('/usr', FpcPrefixCombobox.Items);
  CheckPath('/usr/local', FpcPrefixCombobox.Items);
  {$ENDIF WINDOWS}
end;

function TGenerateFppkgConfigurationDialog.CheckFppkgQuality(APrefix: string; out LibPath,
  Note: string): TSDFilenameQuality;
var
  SR: TRawByteSearchRec;
  LibPathValid: Boolean;
  Ver: TFPVersion;
begin
  Result := sddqInvalid;

  if APrefix='' then
  begin
    Note := lisWarning + lisNoFppkgPrefix + LineEnding;
    Exit;
  end;

  LibPath := '';
  APrefix:=TrimFilename(APrefix);
  if not FileExistsCached(APrefix) then
  begin
    Note:= lisWarning + lisFreePascalPrefix + ' ' + lisISDDirectoryNotFound + LineEnding;
  end
  else if not DirPathExistsCached(APrefix) then
  begin
    Note:= lisWarning + lisFreePascalPrefix + ' ' + lisPathIsNoDirectory + LineEnding;
  end
  else
  begin
    LibPathValid := True;

    {$IFNDEF WINDOWS}
    LibPath := ConcatPaths([APrefix, 'lib', 'fpc']);
    if not DirPathExistsCached(LibPath) then
    begin
      LibPath := ConcatPaths([APrefix, 'lib64', 'fpc']);
      if not DirPathExistsCached(LibPath) then
      begin
        LibPathValid := False;
      end;
    end;
    {$ELSE}
    LibPath := APrefix;
    {$ENDIF}
    LibPath := IncludeTrailingPathDelimiter(LibPath);

    if DirPathExistsCached(LibPath+PathDelim+'fpmkinst') and
      DirPathExistsCached(LibPath+PathDelim+'units') then
    begin
      LibPathValid := True;
      Result := sddqCompatible;
    end
    else if LibPathValid and (FindFirstUTF8(LibPath+AllFilesMask, faDirectory, SR) = 0) then
    begin
      LibPathValid := False;
      repeat
        if (SR.Name<>'.') and (SR.Name<>'..') then
        begin
          if DirPathExistsCached(LibPath+SR.Name+PathDelim+'fpmkinst') and
            DirPathExistsCached(LibPath+SR.Name+PathDelim+'units') then
              begin
                Ver := TFPVersion.Create;
                try
                  Ver.AsString:=SR.Name;
                  if (Ver.Major > -1) and (Ver.Minor > -1) and (Ver.Micro > -1) then
                    LibPath:=LibPath + '{CompilerVersion}' + PathDelim
                  else
                    LibPath:=LibPath + SR.Name + PathDelim
                finally
                  Ver.Free;
                end;
                LibPathValid := True;
                Result := sddqCompatible;
                Break;
              end;
        end;
      until FindNext(SR) <> 0;
      FindCloseUTF8(SR);
    end;

    if not LibPathValid then
      Note:= Note + lisWarning + lisNotAValidFppkgPrefix + LineEnding
    else
      Note:='';
  end;

end;

procedure TGenerateFppkgConfigurationDialog.UpdateFppkgNote;
var
  CurCaption: String;
  Msg, Note: string;
  FileName: string;
begin
  if csDestroying in ComponentState then exit;
  CurCaption:=FpcPrefixCombobox.Text;
  if (fLastParsedFpcPrefix=CurCaption) and (CurCaption<>'') then exit;
  fLastParsedFpcPrefix:=CurCaption;

  Msg := '';
  if CheckFppkgQuality(CurCaption,fLastParsedFpcLibPath,Note)<>sddqCompatible then
    Msg := Note;
  if (CheckFPCExeQuality(FCompiler, Note, CodeToolBoss.CompilerDefinesCache.TestFilename)<>sddqCompatible) then
    Msg := Msg + lisWarning + lisFppkgCompilerProblem +Note;
  if CheckFpcmkcfgQuality(Note) <> sddqCompatible then
    Msg := Msg + lisWarning + Note;

  Note := lisFppkgFilesToBeWritten + LineEnding;
  Note := Note + Format(lisGenerateFppkgCfg, [FppkgCfgFilename]) + LineEnding;
  // These are the default config-locations used by fpcmkcfg
  {$IFDEF WINDOWS}
  FileName := '%LocalAppData%\FreePascal\Fppkg\config\default';
  {$ELSE}
  FileName := '~/.fppkg/config/default';
  {$ENDIF}
  Note := Note + Format(lisGenerateFppkgCompCfg, [FileName]) + LineEnding;

  if Msg<>'' then
  begin
    WarningsLabel.Visible := True;
    Note := Note + LineEnding + Msg;
    FppkgWriteConfigButton.Enabled := False;
  end
  else
  begin
    WarningsLabel.Visible := False;
    FppkgWriteConfigButton.Enabled := True;
  end;

  Note := Note + LineEnding + Format(lisFppkgPrefix, [fLastParsedFpcPrefix]) + LineEnding;
  Note := Note + Format(lisFppkgLibPrefix, [fLastParsedFpcLibPath]) + LineEnding;

  InfoMemo.Text := Note;
end;

procedure TGenerateFppkgConfigurationDialog.SetCompiler(AValue: string);
begin
  if FCompiler = AValue then Exit;
  FCompiler := AValue;
  fLastParsedFpcPrefix := ' ';
  UpdateFppkgNote;
end;

function TGenerateFppkgConfigurationDialog.CheckFpcmkcfgQuality(out Note: string): TSDFilenameQuality;
{$IF FPC_FULLVERSION>30100}
var
  FpcmkcfgExecutable: string;
  Proc: TProcessUTF8;
  S: string;
  Ver: TFPVersion;
{$ENDIF}
begin
  Result := sddqCompatible;
  Note:='';
  {$IF FPC_FULLVERSION>30100}
  FpcmkcfgExecutable := FindFPCTool('fpcmkcfg'+GetExecutableExt, EnvironmentOptions.GetParsedCompilerFilename);
  if FpcmkcfgExecutable = '' then
    begin
    Note := lisFppkgFpcmkcfgMissing + ' ' + lisFppkgRecentFpcmkcfgNeeded;
    Result := sddqInvalid;
    end
  else
    begin
    Proc := TProcessUTF8.Create(nil);
    try

      Proc.Options := proc.Options + [poWaitOnExit,poUsePipes];
      // Write fppkg.cfg
      Proc.Executable := FpcmkcfgExecutable;
      proc.Parameters.Add('-V');
      proc.Execute;

      if proc.ExitStatus <> 0 then
        begin
        Note := lisFppkgFpcmkcfgCheckFailed + ' ' + lisFppkgFpcmkcfgProbTooOld + ' ' + lisFppkgRecentFpcmkcfgNeeded;
        Result := sddqInvalid;
        end
      else
        begin
        S := '';
        SetLength(S, Proc.Output.NumBytesAvailable);
        Proc.Output.Read(S[1], Proc.Output.NumBytesAvailable);
        Ver := TFPVersion.Create;
        try
          S := Copy(S, pos(':', S)+2);
          Ver.AsString := Trim(S);
          if Ver.Major = -1 then
            begin
            Note := lisFppkgFpcmkcfgCheckFailed + ' ' + lisFppkgFpcmkcfgNeeded + lisFppkgRecentFpcmkcfgNeeded;
            Result := sddqInvalid;
            end
          else if not ((Ver.Major = 0) or (Ver.Major > 3) or (((Ver.Major = 3)) and (Ver.Minor>1))) then
            begin
            // fpcmkcfg's version must be > 3.1. Older versions need other
            // parameters. Version 0 is also allowed, because it is probably
            // self-built.
            Note := Format( lisFppkgFpcmkcfgTooOld, [Ver.AsString]) + ' ' + lisFppkgFpcmkcfgNeeded + ' ' + lisFppkgRecentFpcmkcfgNeeded;
            Result := sddqInvalid;
            end;
        finally
          Ver.Free;
        end;
        end;
    finally
      Proc.Free;
    end;
    end;
  {$ENDIF}
end;

procedure TGenerateFppkgConfigurationDialog.FpcPrefixComboboxChange(Sender: TObject);
begin
  UpdateFppkgNote;
end;

procedure TGenerateFppkgConfigurationDialog.SetFppkgCfgFilename(AValue: string);
begin
  if FFppkgCfgFilename = AValue then Exit;
  FFppkgCfgFilename := AValue;
  fLastParsedFpcPrefix := ' ';
  UpdateFppkgNote;
end;

procedure TGenerateFppkgConfigurationDialog.BrowseButtonClick(Sender: TObject);
var
  Dlg: TSelectDirectoryDialog;
begin
  Dlg:=TSelectDirectoryDialog.Create(nil);
  try
    Dlg.Title:=lisSelectFPCPath;
    Dlg.Options:=Dlg.Options+[ofPathMustExist];
    if not Dlg.Execute then exit;
    FpcPrefixCombobox.Text:=Dlg.FileName;
  finally
    Dlg.Free;
  end;
  UpdateFppkgNote;
end;

procedure TGenerateFppkgConfigurationDialog.FppkgWriteConfigButtonClick(Sender: TObject);
var
  Msg: string;
{$IF FPC_FULLVERSION>30100}
  FpcmkcfgExecutable, CompConfigFilename: string;
  Proc: TProcessUTF8;
  Fppkg: TFppkgHelper;
{$ENDIF}
begin
  {$IF FPC_FULLVERSION>30100}
  try
    FpcmkcfgExecutable := FindFPCTool('fpcmkcfg'+GetExecutableExt, EnvironmentOptions.GetParsedCompilerFilename);
    if FpcmkcfgExecutable<>'' then
    begin
      Proc := TProcessUTF8.Create(nil);
      try
        Proc.Options := proc.Options + [poWaitOnExit];
        // Write fppkg.cfg
        Proc.Executable := FpcmkcfgExecutable;
        proc.Parameters.Add('-p');
        proc.Parameters.Add('-3');
        proc.Parameters.Add('-o');
        proc.Parameters.Add(GetFppkgConfigFile(False, False));
        proc.Parameters.Add('-d');
        proc.Parameters.Add('globalpath='+fLastParsedFpcLibPath);
        proc.Parameters.Add('-d');
        {$IFDEF WINDOWS}
        proc.Parameters.Add('globalprefix='+fLastParsedFpcLibPath);
        {$ELSE}
        proc.Parameters.Add('globalprefix='+fLastParsedFpcPrefix);
        {$ENDIF}
        proc.Execute;

        Fppkg:=TFppkgHelper.Instance;

        if proc.ExitStatus <> 0 then
          IDEMessageDialog(lisFppkgProblem, Format(lisFppkgCreateFileFailed, [GetFppkgConfigFile(False, False)]), mtWarning, [mbOK])
        else
          begin
          Fppkg:=TFppkgHelper.Instance;
          Fppkg.ReInitialize;

          // Write default compiler configuration file
          CompConfigFilename := Fppkg.GetCompilerConfigurationFileName;
          if CompConfigFilename <> '' then
            begin
            proc.Parameters.Clear;
            proc.Parameters.Add('-p');
            proc.Parameters.Add('-4');
            proc.Parameters.Add('-o');
            proc.Parameters.Add(CompConfigFilename);
            proc.Parameters.Add('-d');
            proc.Parameters.Add('fpcbin='+EnvironmentOptions.GetParsedCompilerFilename);
            proc.Execute;

            if proc.ExitStatus <> 0 then
              IDEMessageDialog(lisFppkgProblem, Format(lisFppkgCreateFileFailed, [CompConfigFilename]), mtWarning, [mbOK]);
            end;
          end;

        Fppkg.ReInitialize;
      finally
        Proc.Free;
      end;
    end;
  except
    on E: Exception do
      IDEMessageDialog(lisFppkgProblem, Format(lisFppkgWriteConfException, [E.Message]), mtWarning, [mbOK]);
  end;

  fLastParsedFpcPrefix := '';
  UpdateFppkgNote;
  {$ENDIF}

  if CheckFppkgConfiguration(Msg)<>sddqCompatible then
  begin
    IDEMessageDialog(lisFppkgProblem, Format(lisFppkgWriteConfFailed, [Msg]),
      mtWarning, [mbOK]);
    ModalResult := mrCancel;
  end
  else
    ModalResult := mrOK;
end;


end.

