unit compiler_path_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, Forms, Controls, Dialogs, Graphics,
  Buttons, StdCtrls, LCLType, InterfaceBase,
  IDEOptionsIntf, MacroIntf, IDEDialogs, CompOptsIntf,
  Project, CompilerOptions, LazarusIDEStrConsts, PathEditorDlg, LazConf,
  IDEProcs, CheckCompilerOpts, ShowCompilerOpts, MainIntf;

type

  { TCompilerPathOptionsFrame }

  TCompilerPathOptionsFrame = class(TAbstractIDEOptionsEditor)
    DebugPathEdit: TEdit;
    DebugPathLabel: TLabel;
    IncludeFilesEdit: TEdit;
    IncludeFilesLabel: TLabel;
    LCLWidgetTypeLabel: TLabel;
    LibrariesEdit: TEdit;
    LibrariesLabel: TLabel;
    OtherSourcesEdit: TEdit;
    OtherSourcesLabel: TLabel;
    OtherUnitsEdit: TEdit;
    OtherUnitsLabel: TLabel;
    ProjTargetApplyConventionsCheckBox: TCheckBox;
    ProjTargetFileEdit: TEdit;
    ProjTargetFileLabel: TLabel;
    UnitOutputDirEdit: TEdit;
    UnitOutputDirLabel: TLabel;
    procedure LCLWidgetTypeLabelClick(Sender: TObject);
    procedure LCLWidgetTypeLabelMouseEnter(Sender: TObject);
    procedure LCLWidgetTypeLabelMouseLeave(Sender: TObject);
    procedure ProjTargetFileEditChange(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;
    FCompilerOpts: TBaseCompilerOptions;
    OtherUnitsPathEditBtn: TPathEditorButton;
    IncludeFilesPathEditBtn: TPathEditorButton;
    OtherSourcesPathEditBtn: TPathEditorButton;
    LibrariesPathEditBtn: TPathEditorButton;
    btnUnitOutputDir: TButton;
    DebugPathEditBtn: TPathEditorButton;
    btnShowOptions: TBitBtn;
    btnCheck: TBitBtn;
    btnLoadSave: TBitBtn;
    chkUseAsDefault: TCheckBox;
    function CheckSearchPath(const Context, ExpandedPath: string;
      Level: TCheckCompileOptionsMsgLvl): boolean;
    function CheckSrcPathInUnitPath(OldParsedSrcPath, NewParsedSrcPath,
      OldParsedUnitPath, NewParsedUnitPath: string;
      out SrcPathChanged: boolean): boolean;
    procedure FileBrowseBtnClick(Sender: TObject);
    procedure PathEditBtnClick(Sender: TObject);
    procedure PathEditBtnExecuted(Sender: TObject);
    procedure DoShowOptions(Sender: TObject);
    procedure DoCheck(Sender: TObject);
    procedure DoLoadSave(Sender: TObject);
  protected
    procedure DoSaveSettings(AOptions: TAbstractIDEOptions);
    procedure UpdateTargetFileLabel;
  public
    constructor Create(TheOwner: TComponent); override;
    function Check: boolean; override;
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TCompilerPathOptionsFrame }

function TCompilerPathOptionsFrame.Check: boolean;
var
  NewParsedOutputDir: string;

  function CheckPutSearchPath(
    const Context, OldExpandedPath, NewExpandedPath: string): boolean;
  var
    Level: TCheckCompileOptionsMsgLvl;
    p: String;
  begin
    if OldExpandedPath <> NewExpandedPath then
      Level := ccomlHints
    else
      Level := ccomlErrors;

    // do not complain about missing output directory
    p:=NewExpandedPath;
    if NewParsedOutputDir<>'' then
      p:=RemoveSearchPaths(p,NewParsedOutputDir);

    Result := CheckSearchPath(Context, p, Level);
  end;

var
  OldParsedIncludePath: String;
  OldParsedLibraryPath: String;
  OldParsedUnitPath: String;
  OldParsedSrcPath: String;
  OldParsedDebugPath: String;
  OldUnparsedIncludePath: String;
  OldUnparsedLibraryPath: String;
  OldUnparsedUnitPath: String;
  OldUnparsedSrcPath: String;
  OldUnparsedDebugPath: String;
  NewParsedIncludePath: String;
  NewParsedLibraries: String;
  NewParsedUnitPath: String;
  NewParsedSrcPath: String;
  NewParsedDebugPath: String;
  PathsChanged: boolean;

  procedure GetParsedPaths;
  begin
    NewParsedOutputDir:=FCompilerOpts.GetUnitOutPath(False,coptParsed);
    NewParsedIncludePath:=FCompilerOpts.GetIncludePath(False,coptParsed,false);
    NewParsedLibraries:=FCompilerOpts.GetLibraryPath(False,coptParsed,false);
    NewParsedUnitPath:=FCompilerOpts.GetUnitPath(False,coptParsed,false);
    NewParsedSrcPath:=FCompilerOpts.GetSrcPath(False,coptParsed,false);
    NewParsedDebugPath:=FCompilerOpts.GetDebugPath(False,coptParsed,false);
  end;

var
  o: TParsedCompilerOptString;
  Msg: String;
begin
  Result:=false;

  GetParsedPaths;

  OldParsedIncludePath := NewParsedIncludePath;
  OldUnparsedIncludePath := FCompilerOpts.IncludePath;
  OldParsedLibraryPath := NewParsedLibraries;
  OldUnparsedLibraryPath := FCompilerOpts.Libraries;
  OldParsedUnitPath := NewParsedUnitPath;
  OldUnparsedUnitPath := FCompilerOpts.OtherUnitFiles;
  OldParsedSrcPath := NewParsedSrcPath;
  OldUnparsedSrcPath := FCompilerOpts.SrcPath;
  OldParsedDebugPath := NewParsedDebugPath;
  OldUnparsedDebugPath := FCompilerOpts.DebugPath;

  try
    FCompilerOpts.IncludePath := IncludeFilesEdit.Text;
    FCompilerOpts.Libraries := LibrariesEdit.Text;
    FCompilerOpts.OtherUnitFiles := OtherUnitsEdit.Text;
    FCompilerOpts.SrcPath := OtherSourcesEdit.Text;
    FCompilerOpts.DebugPath := DebugPathEdit.Text;
    GetParsedPaths;

    if FCompilerOpts.ParsedOpts.HasParsedError then begin
      o:=FCompilerOpts.ParsedOpts.ParsedErrorOption;
      case o of
      pcosBaseDir:
        Msg:=lisIWonderHowYouDidThatErrorInTheBaseDirectory;
      pcosUnitPath:
        Msg:=lisErrorInTheSearchPathForOtherUnitFiles;
      pcosIncludePath:
        Msg:=lisErrorInTheSearchPathForIncludeFiles;
      pcosObjectPath:
        Msg:=lisErrorInTheSearchPathForObjectFiles;
      pcosLibraryPath:
        Msg:=lisErrorInTheSearchPathForLibraries;
      pcosSrcPath:
        Msg:=lisErrorInTheSearchPathForOtherSources;
      pcosLinkerOptions:
        Msg:=lisErrorInTheCustomLinkerOptionsLinkingPassOptionsToL;
      pcosCustomOptions:
        Msg:=lisErrorInTheCustomCompilerOptionsOther;
      pcosOutputDir:
        Msg:=lisErrorInTheUnitOutputDirectory;
      pcosCompilerPath:
        Msg:=lisErrorInTheCompilerFileName;
      pcosDebugPath:
        Msg:=lisErrorInTheDebuggerPathAddition;
      else
        Msg:=Format(lisIWonderHowYouDidThatErrorInThe, [EnumToStr(o)]);
      end;
      Msg:=Msg+#13+FCompilerOpts.ParsedOpts.ParsedErrorMsg+#13
        +lisValue3+dbgstr(FCompilerOpts.ParsedOpts.UnparsedValues[o]);
      IDEMessageDialog(lisCCOErrorCaption, Msg, mtError, [mbCancel]);
      exit;
    end;

    if not CheckPutSearchPath('include search path', OldParsedIncludePath, NewParsedIncludePath) then
      Exit;
    if not CheckPutSearchPath('library search path', OldParsedLibraryPath, NewParsedLibraries) then
      Exit;
    if not CheckPutSearchPath('unit search path', OldParsedUnitPath, NewParsedUnitPath) then
      Exit;
    if not CheckPutSearchPath('source search path', OldParsedSrcPath, NewParsedSrcPath) then
      Exit;
    if not CheckPutSearchPath('debugger search path', OldParsedDebugPath, NewParsedDebugPath) then
      Exit;

    if not CheckSrcPathInUnitPath(OldParsedSrcPath,NewParsedSrcPath,
      OldParsedUnitPath,NewParsedUnitPath,PathsChanged)
    then
      Exit;
    if PathsChanged then
      GetParsedPaths;

  finally
    FCompilerOpts.IncludePath := OldUnparsedIncludePath;
    FCompilerOpts.Libraries := OldUnparsedLibraryPath;
    FCompilerOpts.OtherUnitFiles := OldUnparsedUnitPath;
    FCompilerOpts.SrcPath := OldUnparsedSrcPath;
    FCompilerOpts.DebugPath := OldUnparsedDebugPath;
  end;
  Result := True;
end;

function TCompilerPathOptionsFrame.GetTitle: string;
begin
  Result := dlgSearchPaths;
end;

procedure TCompilerPathOptionsFrame.DoShowOptions(Sender: TObject);
begin
  DoSaveSettings(FCompilerOpts);
  ShowCompilerOptionsDialog(FDialog, FCompilerOpts);
end;

procedure TCompilerPathOptionsFrame.DoCheck(Sender: TObject);
begin
  DoSaveSettings(FCompilerOpts);
  if Assigned(TestCompilerOptions) then
  begin
    btnCheck.Enabled := False;
    try
      TestCompilerOptions(FCompilerOpts);
    finally
      btnCheck.Enabled := True;
    end;
  end;
end;

procedure TCompilerPathOptionsFrame.DoLoadSave(Sender: TObject);
var
  ImportExportResult: TImportExportOptionsResult;
begin
  DoSaveSettings(FCompilerOpts);
  if (MainIDEInterface.DoImExportCompilerOptions(FCompilerOpts, ImportExportResult) = mrOK) and
     (ImportExportResult = ieorImport) then
  begin
    if Assigned(OnLoadIDEOptions) then
      OnLoadIDEOptions(Self, FCompilerOpts);
  end;
end;

procedure TCompilerPathOptionsFrame.DoSaveSettings(AOptions: TAbstractIDEOptions);
begin
  if Assigned(OnSaveIDEOptions) then
    OnSaveIDEOptions(Self, AOptions);
end;

procedure TCompilerPathOptionsFrame.UpdateTargetFileLabel;
begin
  if ProjTargetFileEdit.Text<>'' then
    ProjTargetFileLabel.Caption:=lisTargetFileNameO
  else
    ProjTargetFileLabel.Caption:=lisTargetFileNameEmptyUseUnitOutputDirectory;
end;

constructor TCompilerPathOptionsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompilerOpts := nil;
end;

procedure TCompilerPathOptionsFrame.ProjTargetFileEditChange(Sender: TObject);
begin
  UpdateTargetFileLabel;
end;

procedure TCompilerPathOptionsFrame.LCLWidgetTypeLabelClick(Sender: TObject);
begin
  FDialog.OpenEditor(GroupCompiler,CompilerOptionsBuildModes);
end;

procedure TCompilerPathOptionsFrame.LCLWidgetTypeLabelMouseEnter(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := True;
  (Sender as TLabel).Font.Color := clRed;
end;

procedure TCompilerPathOptionsFrame.LCLWidgetTypeLabelMouseLeave(Sender: TObject);
begin
  (Sender as TLabel).Font.Underline := False;
  (Sender as TLabel).Font.Color := clBlue;
end;

function TCompilerPathOptionsFrame.CheckSearchPath(const Context, ExpandedPath: string;
  Level: TCheckCompileOptionsMsgLvl): boolean;
var
  CurPath: string;
  p: integer;
  HasChars: TCCOSpecialChars;
  ErrorMsg: string;
begin
  Result := False;

  // check for *
  if Ord(Level) <= Ord(ccomlHints) then
  begin
    if System.Pos('*', ExpandedPath) > 0 then
    begin
      if MessageDlg(lisHint, Format(
        lisTheContainsAStarCharacterLazarusUsesThisAsNormalCh, [Context, #13]),
        mtWarning, [mbOK, mbCancel], 0) <> mrOk then
        exit;
    end;
  end;

  // check for non existing directories
  if Ord(Level) <= Ord(ccomlWarning) then
  begin
    p := 1;
    repeat
      //DebugLn(['CheckSearchPath ',ExpandedPath,' ',p,' ',length(ExpandedPath)]);
      CurPath := GetNextDirectoryInSearchPath(ExpandedPath, p);
      if (CurPath <> '') and (not IDEMacros.StrHasMacros(CurPath)) and
        (FilenameIsAbsolute(CurPath)) then
      begin
        if not DirPathExistsCached(CurPath) then
        begin
          if MessageDlg(lisCCOWarningCaption, Format(
            lisTheContainsANotExistingDirectory, [Context, #13, CurPath]),
            mtWarning, [mbIgnore, mbCancel], 0) <> mrIgnore then
            Exit;
        end;
      end;
    until p > length(ExpandedPath);
  end;

  // check for special characters
  if (not IDEMacros.StrHasMacros(ExpandedPath)) then
  begin
    FindSpecialCharsInPath(ExpandedPath, HasChars);
    if Ord(Level) <= Ord(ccomlWarning) then
    begin
      if Ord(Level) >= Ord(ccomlErrors) then
        ErrorMsg := SpecialCharsToStr(HasChars * [ccoscSpecialChars, ccoscNewLine])
      else
        ErrorMsg := SpecialCharsToStr(HasChars);
      if ErrorMsg <> '' then
      begin
        if MessageDlg(lisCCOWarningCaption, Context + #13 + ErrorMsg, mtWarning,
          [mbOK, mbCancel], 0) <> mrOk then
          exit;
      end;
    end;
  end;

  Result := True;
end;

function TCompilerPathOptionsFrame.CheckSrcPathInUnitPath(OldParsedSrcPath,
  NewParsedSrcPath, OldParsedUnitPath, NewParsedUnitPath: string; out
  SrcPathChanged: boolean): boolean;
// checks if the SrcPath contains directories of the UnitPath
// the SrcPath should only contain directories for the IDE, not for the compiler
var
  p: Integer;
  CurPath: String;
  Duplicates: TStringList;
  i: PtrInt;
  OldUnparsedSrcPath: String;
  NewUnparsedSrcPath: String;
  j: Integer;
  BaseDir: String;
begin
  Result:=true;
  SrcPathChanged:=false;
  if (OldParsedSrcPath=NewParsedSrcPath)
  and (OldParsedUnitPath=NewParsedUnitPath) then exit;

  Duplicates:=TStringList.Create;
  try
    p:=1;
    i:=0;
    BaseDir:=AppendPathDelim(FCompilerOpts.BaseDirectory);
    repeat
      CurPath:=GetNextDirectoryInSearchPath(NewParsedSrcPath, p);
      if (CurPath<>'') and (not IDEMacros.StrHasMacros(CurPath)) and
        (FilenameIsAbsolute(CurPath)) then
      begin
        if (SearchDirectoryInSearchPath(NewParsedUnitPath,CurPath)>0)
        or (CompareFilenames(BaseDir,AppendPathDelim(CurPath))=0) then
          Duplicates.AddObject(CurPath,TObject(Pointer(i)));
      end;
      inc(i);
    until p>length(NewParsedSrcPath);

    if Duplicates.Count>0 then
    begin
      debugln(['TCompilerPathOptionsFrame.CheckSrcPathInUnitPath OldParsedSrcPath="',OldParsedSrcPath,'" NewParsedSrcPath="',NewParsedSrcPath,'" OldParsedUnitPath="',OldParsedUnitPath,'" NewParsedUnitPath="',NewParsedUnitPath,'"']);
      Result:=false;
      Duplicates.Delimiter:=#13;
      Duplicates.StrictDelimiter:=true;
      if QuestionDlg(lisDuplicateSearchPath,
        Format(lisTheOtherSourcesContainsADirectoryWhichIsAlreadyInT, [#13#13,
          Duplicates.DelimitedText]),
        mtError,
        [mrCancel, mrYes, lisRemoveThePathsFromOtherSources, 'IsDefault'],
        0)=mrYes
      then begin
        // remove paths from SrcPath
        OldUnparsedSrcPath:=FCompilerOpts.SrcPath;
        NewUnparsedSrcPath:='';
        i:=0;
        p:=1;
        repeat
          CurPath:=GetNextDirectoryInSearchPath(OldUnparsedSrcPath, p);
          j:=Duplicates.Count-1;
          while (j>=0) and (PtrUInt(Duplicates.Objects[j])<>i) do dec(j);
          if j<0 then
          begin
            if NewUnparsedSrcPath<>'' then
              NewUnparsedSrcPath:=NewUnparsedSrcPath+';';
            NewUnparsedSrcPath:=NewUnparsedSrcPath+CurPath;
          end;
          inc(i);
        until p>length(OldUnparsedSrcPath);
        FCompilerOpts.SrcPath:=NewUnparsedSrcPath;
        OtherSourcesEdit.Text:=FCompilerOpts.SrcPath;

        SrcPathChanged:=true;
        // do not set Result to true, let's user review the changes
      end;
    end;
  finally
    Duplicates.Free;
  end;
end;

procedure TCompilerPathOptionsFrame.PathEditBtnClick(Sender: TObject);
var
  AButton: TPathEditorButton;
  OldPath, Templates: string;
begin
  if Sender is TPathEditorButton then
  begin
    AButton := TPathEditorButton(Sender);
    if AButton = OtherUnitsPathEditBtn then
    begin
      OldPath := OtherUnitsEdit.Text;
      Templates := SetDirSeparators(
        '$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)' +
        ';$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)/$(LCLWidgetType)' +
        ';$(LazarusDir)/components/codetools/units/$(TargetCPU)-$(TargetOS)' +
        ';$(LazarusDir)/components/custom' +
        ';$(LazarusDir)/packager/units/$(TargetCPU)-$(TargetOS)');
    end
    else
    if AButton = IncludeFilesPathEditBtn then
    begin
      OldPath := IncludeFilesEdit.Text;
      Templates := 'include' + ';inc';
    end
    else
    if AButton = OtherSourcesPathEditBtn then
    begin
      OldPath := OtherSourcesEdit.Text;
      Templates := SetDirSeparators('$(LazarusDir)/lcl' +
        ';$(LazarusDir)/lcl/interfaces/$(LCLWidgetType)' +
        ';$(LazarusDir)/components/synedit' + ';$(LazarusDir)/components/codetools');
    end
    else
    if AButton = LibrariesPathEditBtn then
    begin
      OldPath := LibrariesEdit.Text;
      Templates := SetDirSeparators('/usr/X11R6/lib;/sw/lib');
    end
    else
    if AButton = DebugPathEditBtn then
    begin
      OldPath := DebugPathEdit.Text;
      Templates := SetDirSeparators('$(LazarusDir)/lcl/include' +
        ';$(LazarusDir)/lcl/interfaces/$(LCLWidgetType)' +
        ';$(LazarusDir)/include');
    end
    else
      Exit;
    AButton.CurrentPathEditor.BaseDirectory := FCompilerOpts.BaseDirectory;
    AButton.CurrentPathEditor.Path := OldPath;
    AButton.CurrentPathEditor.Templates := SetDirSeparators(Templates);
  end;
end;

procedure TCompilerPathOptionsFrame.PathEditBtnExecuted(Sender: TObject);

  function CheckPath(const Context, NewPath: string): boolean;
  var
    ExpandedPath: string;
    BaseDir: string;
  begin
    BaseDir := FCompilerOpts.BaseDirectory;
    ExpandedPath := TrimSearchPath(NewPath, BaseDir, true);
    Result := CheckSearchPath(Context, ExpandedPath, ccomlHints);
  end;

var
  AButton: TPathEditorButton;
  NewPath: string;
begin
  if Sender is TPathEditorButton then
  begin
    AButton := TPathEditorButton(Sender);
    if AButton.CurrentPathEditor.ModalResult <> mrOk then
      Exit;
    NewPath := AButton.CurrentPathEditor.Path;
    NewPath := FCompilerOpts.ShortenPath(NewPath, False);
    if AButton = OtherUnitsPathEditBtn then
    begin
      if CheckPath(OtherUnitsLabel.Caption, NewPath) then
        OtherUnitsEdit.Text := NewPath;
    end
    else
    if AButton = IncludeFilesPathEditBtn then
    begin
      if CheckPath(IncludeFilesLabel.Caption, NewPath) then
        IncludeFilesEdit.Text := NewPath;
    end
    else
    if AButton = OtherSourcesPathEditBtn then
    begin
      if CheckPath(OtherSourcesLabel.Caption, NewPath) then
        OtherSourcesEdit.Text := NewPath;
    end
    else
    if AButton = LibrariesPathEditBtn then
    begin
      if CheckPath(LibrariesLabel.Caption, NewPath) then
        LibrariesEdit.Text := NewPath;
    end
    else
    if AButton = DebugPathEditBtn then
    begin
      if CheckPath(DebugPathLabel.Caption, NewPath) then
        DebugPathEdit.Text := NewPath;
    end;
  end;
end;

procedure TCompilerPathOptionsFrame.FileBrowseBtnClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  DefaultFilename: string;
  NewFilename: string;
begin
  OpenDialog := TSelectDirectoryDialog.Create(Self);
  try
    DefaultFilename := '';
    if Sender = btnUnitOutputDir then
    begin
      OpenDialog.Title := lisUnitOutputDirectory;
      OpenDialog.Options := OpenDialog.Options + [ofPathMustExist];
    end
    else
      Exit;
    OpenDialog.Filename := ExtractFilename(DefaultFilename);
    if DefaultFilename <> '' then
      OpenDialog.InitialDir := ExtractFilePath(DefaultFilename)
    else
      OpenDialog.InitialDir := FCompilerOpts.BaseDirectory;
    if OpenDialog.Execute then
    begin
      NewFilename := TrimFilename(OpenDialog.Filename);
      NewFilename := FCompilerOpts.ShortenPath(NewFilename, False);
      if Sender = btnUnitOutputDir then
        UnitOutputDirEdit.Text := OpenDialog.Filename;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TCompilerPathOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);

  function CreateButton(ACaption: String; AKind: TBitBtnKind = bkCustom): TBitBtn;
  begin
    Result := ADialog.AddButton;
    Result.Kind := AKind;
    Result.Caption := ACaption;
  end;

begin
  FDialog:=ADialog;
  ProjTargetFileEdit.Text:='';
  ProjTargetApplyConventionsCheckBox.Caption:=lisApplyConventions;

  OtherUnitsLabel.Caption := dlgOtherUnitFiles;
  OtherUnitsPathEditBtn := TPathEditorButton.Create(Self);
  with OtherUnitsPathEditBtn do
  begin
    Name := 'OtherUnitsPathEditBtn';
    Caption := '...';
    Anchors := [akRight, akTop, akBottom];
    AnchorParallel(akTop, 0, OtherUnitsEdit);
    AnchorParallel(akBottom, 0, OtherUnitsEdit);
    AnchorParallel(akRight, 0, Self);
    AutoSize := True;
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
    Parent := Self;
    TabOrder:=1;
  end;
  OtherUnitsEdit.AnchorToNeighbour(akRight, 0, OtherUnitsPathEditBtn);

  {------------------------------------------------------------}

  IncludeFilesLabel.Caption := dlgCOIncFiles;
  IncludeFilesPathEditBtn := TPathEditorButton.Create(Self);
  with IncludeFilesPathEditBtn do
  begin
    Name := 'IncludeFilesPathEditBtn';
    Anchors := [akRight, akTop, akBottom];
    AnchorParallel(akTop, 0, IncludeFilesEdit);
    AnchorParallel(akBottom, 0, IncludeFilesEdit);
    AnchorParallel(akRight, 0, Self);
    AutoSize := True;
    Caption := '...';
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
    Parent := Self;
    TabOrder:=3;
  end;
  IncludeFilesEdit.AnchorToNeighbour(akRight, 0, IncludeFilesPathEditBtn);

  {------------------------------------------------------------}

  OtherSourcesLabel.Caption := dlgCOSources;
  OtherSourcesPathEditBtn := TPathEditorButton.Create(Self);
  with OtherSourcesPathEditBtn do
  begin
    Name := 'OtherSourcesPathEditBtn';
    Anchors := [akRight, akTop, akBottom];
    AnchorParallel(akTop, 0, OtherSourcesEdit);
    AnchorParallel(akBottom, 0, OtherSourcesEdit);
    AnchorParallel(akRight, 0, Self);
    AutoSize := True;
    Caption := '...';
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
    Parent := Self;
    TabOrder:=9;
  end;
  OtherSourcesEdit.AnchorToNeighbour(akRight, 0, OtherSourcesPathEditBtn);

  {------------------------------------------------------------}

  LibrariesLabel.Caption := dlgCOLibraries;
  LibrariesPathEditBtn := TPathEditorButton.Create(Self);
  with LibrariesPathEditBtn do
  begin
    Name := 'LibrariesPathEditBtn';
    Anchors := [akRight, akTop, akBottom];
    AnchorParallel(akTop, 0, LibrariesEdit);
    AnchorParallel(akBottom, 0, LibrariesEdit);
    AnchorParallel(akRight, 0, Self);
    AutoSize := True;
    Caption := '...';
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
    Parent := Self;
    TabOrder:=5;
  end;
  LibrariesEdit.AnchorToNeighbour(akRight, 0, LibrariesPathEditBtn);

  {------------------------------------------------------------}

  UnitOutputDirLabel.Caption := dlgUnitOutp;
  btnUnitOutputDir := TButton.Create(Self);
  with btnUnitOutputDir do
  begin
    Name := 'btnUnitOutputDir';
    Anchors := [akRight, akTop, akBottom];
    AnchorParallel(akTop, 0, UnitOutputDirEdit);
    AnchorParallel(akBottom, 0, UnitOutputDirEdit);
    AnchorParallel(akRight, 0, Self);
    AutoSize := True;
    Caption := '...';
    OnClick := @FileBrowseBtnClick;
    Parent := Self;
    TabOrder:=7;
  end;
  UnitOutputDirEdit.AnchorToNeighbour(akRight, 0, btnUnitOutputDir);

  {------------------------------------------------------------}

  DebugPathLabel.Caption := dlgCODebugPath;
  DebugPathEditBtn := TPathEditorButton.Create(Self);
  with DebugPathEditBtn do
  begin
    Name := 'DebugPathEditBtn';
    Anchors := [akRight, akTop, akBottom];
    AnchorParallel(akTop, 0, DebugPathEdit);
    AnchorParallel(akBottom, 0, DebugPathEdit);
    AnchorParallel(akRight, 0, Self);
    AutoSize := True;
    Caption := '...';
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
    Parent := Self;
    TabOrder:=13;
  end;
  DebugPathEdit.AnchorToNeighbour(akRight, 0, DebugPathEditBtn);

  {------------------------------------------------------------}

  LCLWidgetTypeLabel.Caption := lisSelectAnotherLCLWidgetSetMacroLCLWidgetType;

  // register special buttons in the dialog itself
  btnShowOptions := CreateButton(dlgCOShowOptions);
  btnShowOptions.LoadGlyphFromLazarusResource('menu_compiler_options');
  btnShowOptions.OnClick  := @DoShowOptions;
  btnCheck := CreateButton(lisCompTest);
  btnCheck.ModalResult := mrNone;
  btnCheck.OnClick  := @DoCheck;
  btnCheck.LoadGlyphFromStock(idButtonYes);
  btnLoadSave := CreateButton('...');
  btnLoadSave.OnClick  := @DoLoadSave;
  btnLoadSave.Hint := dlgCOLoadSave;
  btnLoadSave.LoadGlyphFromStock(idButtonSave);
  if btnLoadSave.Glyph.Empty then
    btnLoadSave.LoadGlyphFromLazarusResource('laz_save');

  chkUseAsDefault := TCheckBox(ADialog.AddControl(TCheckBox));
  chkUseAsDefault.Caption := dlgCOUseAsDefault;
  chkUseAsDefault.ShowHint := True;
  chkUseAsDefault.Hint := lisWhenEnabledTheCurrentOptionsAreSavedToTheTemplateW;
end;

procedure TCompilerPathOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if not (AOptions is TBaseCompilerOptions) then exit;
  FCompilerOpts := TBaseCompilerOptions(AOptions);

  if AOptions is TProjectCompilerOptions then
  begin
    ProjTargetFileEdit.Visible:=true;
    ProjTargetFileLabel.Visible:=true;
    ProjTargetFileEdit.Text:=TProjectCompilerOptions(AOptions).TargetFilename;
    ProjTargetApplyConventionsCheckBox.Checked:=TProjectCompilerOptions(AOptions).TargetFilenameApplyConventions;
    ProjTargetApplyConventionsCheckBox.Visible:=true;
    LCLWidgetTypeLabel.Visible:=true;;
    UpdateTargetFileLabel;
  end else begin
    ProjTargetFileEdit.Visible:=false;
    ProjTargetFileLabel.Visible:=false;
    ProjTargetApplyConventionsCheckBox.Visible:=false;
    LCLWidgetTypeLabel.Visible:=false;
  end;

  with AOptions as TBaseCompilerOptions do
  begin
    OtherUnitsEdit.Text := OtherUnitFiles;
    IncludeFilesEdit.Text := IncludePath;
    LibrariesEdit.Text := Libraries;
    OtherSourcesEdit.Text := SrcPath;
    UnitOutputDirEdit.Text := UnitOutputDirectory;
    DebugPathEdit.Text := DebugPath;

    chkUseAsDefault.Visible := CanBeDefaulForProject;
  end;
end;

procedure TCompilerPathOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  ProjCompOpts: TProjectCompilerOptions;
begin
  if AOptions is TProjectCompilerOptions then begin
    ProjCompOpts:=TProjectCompilerOptions(AOptions);
    ProjCompOpts.TargetFilename:=ProjTargetFileEdit.Text;
    ProjCompOpts.TargetFilenameApplyConventions:=ProjTargetApplyConventionsCheckBox.Checked;
    ProjCompOpts.LazProject.UseAsDefault := chkUseAsDefault.Checked;
  end;

  with AOptions as TBaseCompilerOptions do
  begin
    OtherUnitFiles := OtherUnitsEdit.Text;
    IncludePath := IncludeFilesEdit.Text;
    Libraries := LibrariesEdit.Text;
    SrcPath := OtherSourcesEdit.Text;
    UnitOutputDirectory := UnitOutputDirEdit.Text;
    DebugPath := DebugPathEdit.Text;
  end;
end;

class function TCompilerPathOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TBaseCompilerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerPathOptionsFrame,
    CompilerOptionsSearchPaths);
  RegisterIDEOptionsEditor(GroupPkgCompiler, TCompilerPathOptionsFrame,
    CompilerOptionsSearchPaths);

end.

