unit InitialSetupDlgDebuggerFrame;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Types, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls,
  Dialogs, Buttons, GDBMIDebugger, DbgIntfDebuggerBase, FileUtil, LazFileUtils,
  IDEDialogs, IDEUtils, IDEImagesIntf, FileProcs, FpDebugDebugger,
  {$Ifdef Darwin} FpLldbDebugger, {$ENDIF}
  IdeDebuggerOpts, LazarusIDEStrConsts, InitialSetupProc,
  EnvironmentOpts, LazConf, StrUtils;

type

  TDebuggerIntfClass = class of TDebuggerIntf;

  TDbgCfgStateChangedEvent = procedure(AState: TSDFilenameQuality) of object;

  { TInitDebuggerFrame }

  TInitDebuggerFrame = class(TFrame)
    btnBrowse: TButton;
    cmbDebuggerName: TComboBox;
    cmbDebuggerPath: TComboBox;
    edDebuggerNotes: TMemo;
    edDebuggerName: TEdit;
    edDebuggerPath: TEdit;
    edDebuggerPathResolved: TEdit;
    lbAboutDebugger: TLabel;
    lbDebuggerPath: TLabel;
    pnlDbgPath: TPanel;
    PnlSelectDbgPath: TPanel;
    rbCreateNew: TRadioButton;
    rbChange: TRadioButton;
    rbKeep: TRadioButton;
    rbIgnore: TRadioButton;
    rbChangePath: TRadioButton;
    SpeedButton1: TSpeedButton;
    procedure btnBrowseClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure rbKeepChange(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FCurrentState: TCurrentDebuggerSetupResult;
    FCurDbgCfg: TDebuggerPropertiesConfig;
    FSelDbgCfg: TDebuggerPropertiesConfig; // Selected in combo
    FPathDbgCfg: TDebuggerPropertiesConfig; // Path-Combo currently showing for ...

    FOnStateChanged: TDbgCfgStateChangedEvent;
    FInChangeLock, FInPathLock: Integer;
    FLastDbgPath, FUserDbgPath: String;

    function RecommendedClass: TDebuggerIntfClass;
    function CountRecommendedBackend: Integer;
    function RecommendedBackend(AIdx: Integer): TDebuggerPropertiesConfig;

    function  GetVisibleDebuggerClass: TDebuggerIntfClass;
    function  GetVisibleDebuggerCfg: TDebuggerPropertiesConfig;
    function  GetVisibleDebuggerPath: String;
    function  GetVisibleDebuggerDefaultExe: String;

    function  GetCfgDefaultExe(ADbgClass: TDebuggerIntfClass): String;
    function  CfgNeedsDebuggerPath(ADbgClass: TDebuggerIntfClass): boolean;
    function  GetCfgDefaultExe(ADbgCfg: TDebuggerPropertiesConfig): String;
    function  CfgNeedsDebuggerPath(ADbgCfg: TDebuggerPropertiesConfig): boolean;
    function  CfgHasBrokenDebuggerPath(ADbgCfg: TDebuggerPropertiesConfig): boolean;

    procedure ShowCurrentDbgClass;
    procedure ShowRecommentedDbgClass;
    procedure ShowSelectedDbgClass;

    procedure HideDebuggerPath;
    procedure MaybeShowDebuggerPathForCurrentCfg;
    procedure MaybeShowDebuggerPathCombo(ADbgCfg: TDebuggerPropertiesConfig; ADbgClass: TDebuggerIntfClass = nil);
    procedure MaybeShowDebuggerPathResolved;

    function GetDebuggerClassNote(out ANote: String): TSDFilenameQuality;
    function GetDebuggerPathNote(out ANote: String): TSDFilenameQuality;
  public
    procedure Init;
    function UpdateState: TSDFilenameQuality;
    procedure ApplySelection;

    property OnStateChanged: TDbgCfgStateChangedEvent read FOnStateChanged write FOnStateChanged;
  end;

implementation

{$R *.lfm}

const
  resOk   = sddqCompatible;
  resHint = sddqHint;
  resWarn = sddqIncomplete;
  resErr  = sddqInvalid;

  NOTES_MIN_HEIGHT = 100;

function MaxResult(ARes1, ARes2: TSDFilenameQuality): TSDFilenameQuality;
begin
  Result := ARes1;
  if (ARes2 = resErr) or
     ((ARes2 = resWarn) and (Result in [resHint, resOk])) or
     (Result = resOk)
  then
    Result := ARes2;
end;

{ TInitDebuggerFrame }

procedure TInitDebuggerFrame.FrameResize(Sender: TObject);
begin
  VertScrollBar.Range := edDebuggerNotes.Top + edDebuggerNotes.Constraints.MinHeight;
  VertScrollBar.Page := Height;
end;

procedure TInitDebuggerFrame.btnBrowseClick(Sender: TObject);
var
  lExpandedName: string; // Expanded name before Dialog
  lDirName, lFileName: string;
  lTitle: string;
  lChanged: boolean=False;
  Dlg: TIDEOpenDialog;
  Filter: String;
begin
  Dlg:=IDEOpenDialogClass.Create(nil);
  try
    lTitle := GetVisibleDebuggerDefaultExe;
    if lTitle = '' then
      lTitle := '?';
    Dlg.Title := SimpleFormat(lisSelectPathTo, [lTitle]);
    lExpandedName := EnvironmentOptions.GetParsedValue(eopDebuggerFilename, GetVisibleDebuggerPath);
    lDirName := GetValidDirectory(lExpandedName, {out} lFileName);
    Dlg.Options := Dlg.Options+[ofFileMustExist];
    if lFileName='' then
      lFileName := lTitle;
    Filter := dlgFilterAll+'|'+GetAllFilesMask;
    if ExtractFileExt(lFileName)<>'' then
      Filter := dlgFilterExecutable+'|*'+ExtractFileExt(lFileName)+'|'+Filter;
    Dlg.Filter := Filter;
    Dlg.InitialDir := lDirName;
    Dlg.FileName := lFileName;
    if not Dlg.Execute then
      exit;
    lFileName := CleanAndExpandFilename(Dlg.Filename);
    lChanged := UpperCase(lExpandedName)<>UpperCase(lFileName);
  finally
    Dlg.Free;
  end;
  if lChanged then begin // Avoid looing $(macros)
    cmbDebuggerPath.Text := lFileName;
    rbKeepChange(Sender);
  end;
end;

procedure TInitDebuggerFrame.rbKeepChange(Sender: TObject);
var
  st: TSDFilenameQuality;
begin
  if FInChangeLock > 0 then
    exit;
  inc(FInChangeLock);
  try
    if Sender = cmbDebuggerPath then
      inc(FInPathLock);

    FLastDbgPath := cmbDebuggerPath.Text;
    if (FLastDbgPath <> '') and
       (FUserDbgPath <> FLastDbgPath) and
       (cmbDebuggerPath.Items.IndexOf(FLastDbgPath) < 0)
    then
      FUserDbgPath := FLastDbgPath;

    if (Sender = cmbDebuggerName) then
      FSelDbgCfg := TDebuggerPropertiesConfig(cmbDebuggerName.Items.Objects[cmbDebuggerName.ItemIndex]);

    if (Sender = cmbDebuggerName) or (Sender is TRadioButton) then
      FLastDbgPath := '';

    st := UpdateState;
  finally
    if Sender = cmbDebuggerPath then
      dec(FInPathLock);
    dec(FInChangeLock);

    if FOnStateChanged <> nil then
      FOnStateChanged(st);
  end;
end;

procedure TInitDebuggerFrame.SpeedButton1Click(Sender: TObject);
begin
  MessageDlg(Format(
      InitDlgDebugPopupInformation
    , [''+LineEnding, RecommendedClass.Caption]),
    mtInformation, [mbOK], 0
  );
end;

function TInitDebuggerFrame.RecommendedClass: TDebuggerIntfClass;
begin
  Result := TGDBMIDebugger;
  {$If defined(WINDOWS)}
  Result := TFpDebugDebugger;
  {$ElseIf defined(Linux)}
  Result := TFpDebugDebugger;
  {$ElseIf defined(Darwin)}
  Result := TFpLldbDebugger;
  {$ENDIF}
  if dfNotSuitableForOsArch in Result.SupportedFeatures then
    Result := TGDBMIDebugger;
end;

function TInitDebuggerFrame.CountRecommendedBackend: Integer;
var
  i: Integer;
  r: TDebuggerIntfClass;
begin
  Result := 0;
  r := RecommendedClass;
  for i := 0 to DebuggerOptions.DebuggerPropertiesConfigList.Count - 1 do
    if DebuggerOptions.DebuggerPropertiesConfigList.Opt[i].DebuggerClass = r then
      inc(Result);
end;

function TInitDebuggerFrame.RecommendedBackend(AIdx: Integer): TDebuggerPropertiesConfig;
var
  r: TDebuggerIntfClass;
  i: Integer;
begin
  Result := nil;
  r := RecommendedClass;
  for i := 0 to DebuggerOptions.DebuggerPropertiesConfigList.Count - 1 do
    if DebuggerOptions.DebuggerPropertiesConfigList.Opt[i].DebuggerClass = r then begin
      if AIdx = 0 then
        exit(DebuggerOptions.DebuggerPropertiesConfigList.Opt[i]);
      dec(AIdx);
    end;
end;

function TInitDebuggerFrame.GetVisibleDebuggerClass: TDebuggerIntfClass;
var
  c: TDebuggerPropertiesConfig;
begin
  Result := nil;
  if rbCreateNew.Checked then
    exit(RecommendedClass);
  c := GetVisibleDebuggerCfg;
  if c <> nil then
    Result := c.DebuggerClass;
end;

function TInitDebuggerFrame.GetVisibleDebuggerCfg: TDebuggerPropertiesConfig;
begin
  Result := nil;
  if rbCreateNew.Checked then
    exit;
  if edDebuggerName.Visible then
    Result := FCurDbgCfg
  else
  if cmbDebuggerName.Visible then
    Result := FSelDbgCfg;
end;

function TInitDebuggerFrame.GetVisibleDebuggerPath: String;
var
  c: TDebuggerPropertiesConfig;
begin
  Result := '';
  if (not HandleAllocated) or (not cmbDebuggerPath.HandleAllocated) then begin
    c := GetVisibleDebuggerCfg;
    if c <> nil then
      Result := c.DebuggerFilename;
    exit;
  end;
  if PnlSelectDbgPath.Visible then Result := cmbDebuggerPath.Text
  else
  if edDebuggerPath.Visible then Result := edDebuggerPath.Caption;
end;

function TInitDebuggerFrame.GetVisibleDebuggerDefaultExe: String;
begin
  Result := GetCfgDefaultExe(GetVisibleDebuggerCfg);
end;

function TInitDebuggerFrame.GetCfgDefaultExe(ADbgClass: TDebuggerIntfClass): String;
begin
  Result := '';
  if ADbgClass <> nil then begin
    Result := ADbgClass.ExeBaseName;
    if Result <> '' then
      Result := Result + GetExecutableExt;
  end;
end;

function TInitDebuggerFrame.CfgNeedsDebuggerPath(ADbgClass: TDebuggerIntfClass
  ): boolean;
begin
  Result := (ADbgClass <> nil) and ADbgClass.NeedsExePath;
end;

function TInitDebuggerFrame.GetCfgDefaultExe(ADbgCfg: TDebuggerPropertiesConfig): String;
begin
  Result := '';
  if (ADbgCfg <> nil) then
    Result := GetCfgDefaultExe(ADbgCfg.DebuggerClass);
end;

function TInitDebuggerFrame.CfgNeedsDebuggerPath(ADbgCfg: TDebuggerPropertiesConfig): boolean;
begin
  Result := (ADbgCfg <> nil) and (ADbgCfg.DebuggerClass <> nil) and
            ADbgCfg.DebuggerClass.NeedsExePath;
end;

function TInitDebuggerFrame.CfgHasBrokenDebuggerPath(
  ADbgCfg: TDebuggerPropertiesConfig): boolean;
begin
  Result := CfgNeedsDebuggerPath(ADbgCfg);
  if Result then
    Result := CheckDebuggerQuality(ADbgCfg.DebuggerFilename) <> sdfOk ;
end;

procedure TInitDebuggerFrame.ShowCurrentDbgClass;
begin
  if FCurDbgCfg <> nil then
    edDebuggerName.Caption := FCurDbgCfg.DisplayName
  else
    edDebuggerName.Caption := '';
  cmbDebuggerName.Visible := False;
  edDebuggerName.Visible := True;
end;

procedure TInitDebuggerFrame.ShowRecommentedDbgClass;
begin
  edDebuggerName.Caption := RecommendedClass.Caption;
  cmbDebuggerName.Visible := False;
  edDebuggerName.Visible := True;
end;

procedure TInitDebuggerFrame.ShowSelectedDbgClass;
begin
  cmbDebuggerName.Visible := True;
  edDebuggerName.Visible := False;
end;

procedure TInitDebuggerFrame.HideDebuggerPath;
begin
  pnlDbgPath.Visible := False;
end;

procedure TInitDebuggerFrame.MaybeShowDebuggerPathForCurrentCfg;
begin
  PnlSelectDbgPath.Visible := False;
  edDebuggerPathResolved.Visible := False;
  edDebuggerPath.Visible   := CfgNeedsDebuggerPath(FCurDbgCfg);
  pnlDbgPath.Visible       := edDebuggerPath.Visible;

  if not edDebuggerPath.Visible then
    exit;

  lbDebuggerPath.Caption := InitDlgDebugExternalExePathDisplay;
  edDebuggerPath.Caption := FCurDbgCfg.DebuggerFilename;

  MaybeShowDebuggerPathResolved;
end;

procedure TInitDebuggerFrame.MaybeShowDebuggerPathCombo(
  ADbgCfg: TDebuggerPropertiesConfig; ADbgClass: TDebuggerIntfClass);
var
  p: TStringDynArray;
  DefExe, s: string;
  i: Integer;
begin
  if (ADbgClass = nil) and (ADbgCfg <> nil) then
    ADbgClass := ADbgCfg.DebuggerClass;

  edDebuggerPath.Visible   := False;
  edDebuggerPathResolved.Visible := False;
  PnlSelectDbgPath.Visible := CfgNeedsDebuggerPath(ADbgClass);
  pnlDbgPath.Visible       := PnlSelectDbgPath.Visible;

  if not PnlSelectDbgPath.Visible then begin
    FPathDbgCfg := nil;
    exit;
  end;

  DefExe := GetCfgDefaultExe(ADbgClass);
  if DefExe = '' then
    DefExe := '?';
  lbDebuggerPath.Caption := Format(
    InitDlgDebugExternalExePathPrompt,
    [DefExe]
  );

  if FInPathLock = 0 then begin
    if ADbgCfg = FPathDbgCfg then
      cmbDebuggerPath.Items.Clear
    else
      cmbDebuggerPath.Clear;
    if (ADbgCfg <> nil) and (ADbgCfg.DebuggerFilename <> '') then
      cmbDebuggerPath.AddItem(ADbgCfg.DebuggerFilename, TObject(-1));

    s := '';
    if ADbgClass <> nil then
      s := ADbgClass.ExePaths;
    p := SplitString(
      s +
      ';$Path($(CompPath))/'+ DefExe +
      ';' + DefExe,
      ';'
    );
    for i := 0 to Length(p) - 1 do begin
      if (p[i] <> '') and
         (cmbDebuggerPath.Items.IndexOf(p[i]) < 0) and
         (CheckDebuggerQuality(p[i]) = sdfOk)
      then
        cmbDebuggerPath.AddItem(p[i], TObject(PtrUInt(i)));
    end;
    if FUserDbgPath <> '' then
      cmbDebuggerPath.AddItem(FUserDbgPath, TObject(-2));

    if ADbgCfg <> FPathDbgCfg then begin
      if (FLastDbgPath <> '') or (ADbgCfg = nil) then
        cmbDebuggerPath.Text := FLastDbgPath
      else
        cmbDebuggerPath.Text := ADbgCfg.DebuggerFilename;
      if (ADbgCfg = nil) and (cmbDebuggerPath.Text = '') and (cmbDebuggerPath.Items.Count > 0) then
        cmbDebuggerPath.ItemIndex := 0;
    end;
  end;

  FPathDbgCfg := ADbgCfg;
  MaybeShowDebuggerPathResolved;
end;

procedure TInitDebuggerFrame.MaybeShowDebuggerPathResolved;
var
  s, r: String;
begin
  s := GetVisibleDebuggerPath;
  r := EnvironmentOptions.GetParsedValue(eopDebuggerFilename, s);
  edDebuggerPathResolved.Text := r;
  edDebuggerPathResolved.Visible := (r <> '') and (UpperCase(r) <> UpperCase(s));
end;

function TInitDebuggerFrame.GetDebuggerClassNote(out ANote: String
  ): TSDFilenameQuality;
var
  c: TDebuggerPropertiesConfig;
begin
  Result := resOk;
  ANote := '';

  c := GetVisibleDebuggerCfg;
  if (c = nil) and rbCreateNew.Checked then
    exit; // rbCreateNew;

  if (FCurrentState = cdsNotSupported) and not(rbChange.Checked or rbCreateNew.Checked) then begin
    Result := resErr;
    ANote := InitDlgDebugClassNoteErrorNotSupported;
    exit;
  end;

  if (c = nil) or (c.DebuggerClass = nil) then begin
    Result := resErr;
    ANote := InitDlgDebugClassNoteErrorNotConfigured;
    if FCurrentState = cdsNotRegistered then
      ANote := ANote + LineEnding + InitDlgDebugClassNoteErrorNotConfiguredMissingPackage;
    exit;
  end;

  if c.DebuggerClass = RecommendedClass then begin
    ANote := ''; //'The backend is of the recommended type';
    exit;
  end;

  Result := resHint;
  if (FCurrentState = cdsUpdateToFpDbgNeeded) and
     not(rbCreateNew.Checked or rbChange.Checked)
  then
    Result := resWarn;
  ANote := InitDlgDebugClassNoteHintNotRecommended;
end;

function TInitDebuggerFrame.GetDebuggerPathNote(out ANote: String
  ): TSDFilenameQuality;
var
  s: String;
begin
  Result := resOk;
  ANote := '';
  if CfgNeedsDebuggerPath(GetVisibleDebuggerClass) then begin
    s := GetVisibleDebuggerPath;

    if s = '' then begin
      Result := resErr;
      if rbCreateNew.Checked then
        ANote := InitDlgDebugPathNoteErrorNoDefaultFound
      else
        ANote := InitDlgDebugPathNoteErrorNoExeSpecified;
    end
    else begin
      Result := resErr;
      case CheckDebuggerQuality(s) of
        sdfOk:            Result := resOk;
        sdfNotFound:      ANote := InitDlgDebugPathNoteErrorExeNotFound;
        sdfIsDirectory:   ANote := InitDlgDebugPathNoteErrorExeIsDirectory;
        sdfNotExecutable: ANote := InitDlgDebugPathNoteErrorExeNotRunnable;
      end;
    end;
  end;
end;

procedure TInitDebuggerFrame.Init;
var
  r: TDebuggerIntfClass;
  i: Integer;
  c: TDebuggerPropertiesConfig;
begin
  inc(FInChangeLock);
  try
    FCurrentState := CheckCurrentDebuggerSetup;
    FCurDbgCfg := DebuggerOptions.CurrentDebuggerPropertiesConfig;
    FPathDbgCfg := TDebuggerPropertiesConfig(-1);

    r := RecommendedClass;

    btnBrowse.Caption:=lisPathEditBrowse;
    SpeedButton1.Images := IDEImages.Images_16;
    SpeedButton1.ImageIndex := IDEImages.LoadImage('btn_help');

    lbAboutDebugger.Caption := Format(
      InitDlgDebugHeaderDefaultForYourOSArchIsS,
      [ r.Caption ]
    );

    if FCurDbgCfg <> nil then begin
      edDebuggerName.Caption := FCurDbgCfg.DisplayName;
      edDebuggerPath.Caption := FCurDbgCfg.DebuggerFilename;
    end
    else begin
      edDebuggerName.Caption := '';
      edDebuggerPath.Caption := '';
    end;

    FSelDbgCfg := nil;
    if DebuggerOptions.DebuggerPropertiesConfigList.Count  > 0 then begin
      for i := 0 to DebuggerOptions.DebuggerPropertiesConfigList.Count - 1 do begin
        c := DebuggerOptions.DebuggerPropertiesConfigList.Opt[i];
        cmbDebuggerName.AddItem(c.DisplayName, c);
      end;
      i := 0;
      if FCurDbgCfg <> nil then
        i := cmbDebuggerName.Items.IndexOfObject(FCurDbgCfg);
      cmbDebuggerName.ItemIndex := i;
      FSelDbgCfg := TDebuggerPropertiesConfig(cmbDebuggerName.Items.Objects[i]);
    end;

    rbKeep.Caption              := InitDlgDebugKeepBackend;
    rbChangePath.Caption        := InitDlgDebugChangePath;
    rbCreateNew.Caption         := InitDlgDebugCreateANewRecommendedBack;
    rbChange.Caption            := InitDlgDebugSelectAnExistingBackend;
    rbIgnore.Caption            := InitDlgDebugIgnore;

    case FCurrentState of
      cdsOk: begin
        rbKeep.Visible              := True;
        rbChangePath.Visible        := CfgNeedsDebuggerPath(FCurDbgCfg);
        rbCreateNew.Visible         := FCurDbgCfg.DebuggerClass <> RecommendedClass;
        rbChange.Visible            := DebuggerOptions.DebuggerPropertiesConfigList.Count > 1;
        rbIgnore.Visible            := False;

        rbKeep.Checked := True;
        end;
      //cdsNoActive: begin
      //  end;
      //cdsNotRegistered: begin
      //  end;
      //cdsNotSupported: begin
      //  end;
      cdsUpdateToFpDbgNeeded: begin
        rbKeep.Visible              := True;
        rbChangePath.Visible        := False;
        rbCreateNew.Visible         := True;
        rbChange.Visible            := DebuggerOptions.DebuggerPropertiesConfigList.Count >= 1;
        rbIgnore.Visible            := False;

        rbKeep.Checked := True;
        end;
      else begin
        rbKeep.Visible              := False;
        rbChangePath.Visible        := False;
        rbCreateNew.Visible         := True;
        rbChange.Visible            := DebuggerOptions.DebuggerPropertiesConfigList.Count >= 1;
        rbIgnore.Visible            := True;

        //rbIgnore.Checked := True;
      end;
    end;
  finally
    dec(FInChangeLock);
  end;
end;

function TInitDebuggerFrame.UpdateState: TSDFilenameQuality;
var
  s, ClassNote, PathNote: String;
  PathRes: TSDFilenameQuality;
  HasUnregistered, HasRebuildNote: Boolean;
  h: Integer;
begin
  inc(FInChangeLock);
  try
    DisableAutoSizing;
    Result := resOk;

    // This only needs update, if a macro in the path changes
    if FCurrentState = cdsOk then begin
      if CfgHasBrokenDebuggerPath(FCurDbgCfg) then begin
        rbKeep.Visible              := False;
        rbIgnore.Visible            := True;
        rbKeep.Checked := False;
      end
      else begin
        rbKeep.Visible              := True;
        rbIgnore.Visible            := False;
        if rbIgnore.Checked then rbKeep.Checked := True;
      end;
    end;

    if rbCreateNew.Checked then begin
      ShowRecommentedDbgClass;
      MaybeShowDebuggerPathCombo(nil, RecommendedClass);
    end
    else
    if rbChangePath.Checked then begin
      ShowCurrentDbgClass;
      MaybeShowDebuggerPathCombo(FCurDbgCfg);
    end
    else
    if rbChange.Checked then begin
      ShowSelectedDbgClass;
      MaybeShowDebuggerPathCombo(FSelDbgCfg);
    end
    else
    begin // rbKeep or rbIgnore
      ShowCurrentDbgClass;
      MaybeShowDebuggerPathForCurrentCfg;
    end;


    s := '';
    h := 0;
    HasUnregistered := DebuggerOptions.DebuggerPropertiesConfigList.Unloaded.Count > 0;
    Result := GetDebuggerClassNote(ClassNote);
    PathRes := GetDebuggerPathNote(PathNote);
    HasRebuildNote := False;

    if (FCurrentState in [cdsNotRegistered, cdsNoActive]) and HasUnregistered then begin
      HasRebuildNote := True;
      s := s + InitDlgDebugStateMissingPackages + LineEnding;
      h := h + 50;
    end
    else
    if (FCurrentState = cdsUpdateToFpDbgNeeded) then begin
      s := s + InitDlgDebugStateUpdateBackend + LineEnding;
      h := h + 40;
      //Result := MaxResult(resHint, Result);
    end;

    if rbChange.Visible and rbCreateNew.Visible and
       ( (FCurrentState <> cdsOk) or
         ((FCurDbgCfg <> nil) and (FCurDbgCfg.DebuggerClass <> RecommendedClass))
       )
    then begin
      if (CountRecommendedBackend = 0) and rbChange.Checked then begin
        s := s + InitDlgDebugStateRecommendedNotInList + LineEnding;
        h := h + 25;
      end
      else
      if (CountRecommendedBackend > 0) and rbCreateNew.Checked then begin
        s := s + InitDlgDebugStateRecommendedFoundInList + LineEnding;
        h := h + 25;
      end
    end;

    if (s <> '') and not ( (Result in [resOk, resHint]) and (PathRes in [resOk, resHint]) ) then
      s := s + LineEnding;

    if (ClassNote <> '') and not (Result in [resOk, resHint]) then begin
      s := s + ClassNote + LineEnding;
      ClassNote := '';
    end;

    if (PathNote <> '') and not (PathRes in [resOk, resHint]) then begin
      s := s + PathNote + LineEnding;
      PathNote := '';
      Result := MaxResult(PathRes, Result);
    end;

    if Result in [resOk, resHint] then begin
      if s <> '' then s := s + LineEnding;
      s := s + InitDlgDebugStateSetupOk + LineEnding;
    end;

    if (ClassNote <> '') then
      s := s + ClassNote + LineEnding;
    if (PathNote <> '') then
      s := s + PathNote + LineEnding;

    if HasRebuildNote then begin
      s := s + LineEnding + Format(InitDlgDebugStateMissingPackageRebuild, [''+LineEnding]);
      h := h + 40;
    end
    else
    if HasUnregistered then begin
      s := s + LineEnding + InitDlgDebugStateMissingPackageFooter;
      h := h + 70;
    end;

    if rbIgnore.Checked and (Result = resErr) and
       (FCurrentState in [cdsNoActive, cdsNotRegistered])
    then
      Result := resWarn;

    edDebuggerNotes.Height := ClientHeight - edDebuggerNotes.Top - 5;
    edDebuggerNotes.Constraints.MinHeight := NOTES_MIN_HEIGHT + h;
    edDebuggerNotes.Text := s;

  finally
    dec(FInChangeLock);

    EnableAutoSizing;
    FrameResize(nil);
  end;
end;

procedure TInitDebuggerFrame.ApplySelection;
var
  c: TDebuggerPropertiesConfig;
  n: String;
  i: Integer;
begin
  if rbChangePath.Checked then begin
    assert(DebuggerOptions.CurrentDebuggerPropertiesConfig<>nil, 'TInitDebuggerFrame.ApplySelection: DebuggerOptions.CurrentDebuggerPropertiesConfig<>nil');
    if DebuggerOptions.CurrentDebuggerPropertiesConfig <> nil then
      DebuggerOptions.CurrentDebuggerPropertiesConfig.DebuggerFilename := GetVisibleDebuggerPath;
    DebuggerOptions.SaveDebuggerPropertiesList; // Update XML
  end
  else
  if rbChange.Checked then begin
    assert(FSelDbgCfg <> nil, 'TInitDebuggerFrame.ApplySelection: FSelDbgCfg <> nil');
    if FSelDbgCfg <> nil then begin
      FSelDbgCfg.DebuggerFilename:=GetVisibleDebuggerPath;
      DebuggerOptions.CurrentDebuggerPropertiesConfig := FSelDbgCfg;
    end;
    DebuggerOptions.SaveDebuggerPropertiesList; // Update XML
  end
  else
  if rbCreateNew.Checked then begin
    c := TDebuggerPropertiesConfig.CreateForDebuggerClass(RecommendedClass, True);
    n := 'Default';
    i := 0;
    if DebuggerOptions.DebuggerPropertiesConfigList.EntryByName(n, c.ConfigClass) <> nil then
      while (i <= DebuggerOptions.DebuggerPropertiesConfigList.Count) and
            (DebuggerOptions.DebuggerPropertiesConfigList.EntryByName(n+' '+IntToStr(i), c.ConfigClass) <> nil)
      do
        inc(i);
    if i > 0 then
      n := n + ' ' + IntToStr(i);
    c.ConfigName := n;
    DebuggerOptions.CurrentDebuggerPropertiesConfig := c;
    if CfgNeedsDebuggerPath(c) then
      c.DebuggerFilename := GetVisibleDebuggerPath;
    DebuggerOptions.SaveDebuggerPropertiesList; // Update XML
  end;

  // else
  //if FCurrentState = cdsUpdateToFpDbgNeeded then begin
  //end;
  // caller will save config, and increase version
end;

end.

