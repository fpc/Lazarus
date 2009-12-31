unit compiler_path_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, InterfaceBase, IDEOptionsIntf, Project, CompilerOptions,
  LazarusIDEStrConsts, PathEditorDlg, LazConf, IDEProcs, CheckCompilerOpts,
  MacroIntf;

type

  { TCompilerPathOptionsFrame }

  TCompilerPathOptionsFrame = class(TAbstractIDEOptionsEditor)
    edtDebugPath: TEdit;
    edtIncludeFiles: TEdit;
    edtLibraries: TEdit;
    edtOtherSources: TEdit;
    edtOtherUnits: TEdit;
    edtUnitOutputDir: TEdit;
    lblDebugPath: TLabel;
    lblIncludeFiles: TLabel;
    lblLibraries: TLabel;
    lblOtherSources: TLabel;
    lblOtherUnits: TLabel;
    lblUnitOutputDir: TLabel;
    LCLWidgetTypeComboBox: TComboBox;
    LCLWidgetTypeLabel: TLabel;
  private
    FCompilerOpts: TProjectCompilerOptions;
    OtherUnitsPathEditBtn: TPathEditorButton;
    IncludeFilesPathEditBtn: TPathEditorButton;
    OtherSourcesPathEditBtn: TPathEditorButton;
    LibrariesPathEditBtn: TPathEditorButton;
    btnUnitOutputDir: TButton;
    DebugPathEditBtn: TPathEditorButton;
    function CheckSearchPath(const Context, ExpandedPath: string;
      Level: TCheckCompileOptionsMsgLvl): boolean;
    procedure FileBrowseBtnClick(Sender: TObject);
    procedure PathEditBtnClick(Sender: TObject);
    procedure PathEditBtnExecuted(Sender: TObject);
  public
    function Check: boolean; override;
    function GetTitle: string; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{ TCompilerPathOptionsFrame }

function TCompilerPathOptionsFrame.Check: boolean;

  function CheckPutSearchPath(
  const Context, OldExpandedPath, NewExpandedPath: string): boolean;
  var
    Level: TCheckCompileOptionsMsgLvl;
  begin
    if OldExpandedPath <> NewExpandedPath then
      Level := ccomlHints
    else
      Level := ccomlErrors;
    Result := CheckSearchPath(Context, NewExpandedPath, Level);
  end;

var
  OldIncludePath: String;
  OldLibraryPath: String;
  OldUnitPath: String;
  OldSrcPath: String;
  OldDebugPath: String;
begin
  OldIncludePath := FCompilerOpts.GetIncludePath(False);
  OldLibraryPath := FCompilerOpts.GetLibraryPath(False);
  OldUnitPath := FCompilerOpts.GetUnitPath(False);
  OldSrcPath := FCompilerOpts.GetSrcPath(False);
  OldDebugPath := FCompilerOpts.GetDebugPath(False);

  try
    FCompilerOpts.IncludePath := edtIncludeFiles.Text;
    FCompilerOpts.Libraries := edtLibraries.Text;
    FCompilerOpts.OtherUnitFiles := edtOtherUnits.Text;
    FCompilerOpts.SrcPath := edtOtherSources.Text;
    FCompilerOpts.DebugPath := edtDebugPath.Text;
    if not CheckPutSearchPath('include search path', OldIncludePath, FCompilerOpts.GetIncludePath(False)) then
      Exit(False);
    if not CheckPutSearchPath('library search path', OldLibraryPath, FCompilerOpts.GetLibraryPath(False)) then
      Exit(False);
    if not CheckPutSearchPath('unit search path', OldUnitPath, FCompilerOpts.GetUnitPath(False)) then
      Exit(False);
    if not CheckPutSearchPath('source search path', OldSrcPath, FCompilerOpts.GetSrcPath(False)) then
      Exit(False);
    if not CheckPutSearchPath('debugger search path', OldDebugPath, FCompilerOpts.GetDebugPath(False)) then
      Exit(False);
  finally
    FCompilerOpts.IncludePath := OldIncludePath;
    FCompilerOpts.Libraries := OldLibraryPath;
    FCompilerOpts.OtherUnitFiles := OldUnitPath;
    FCompilerOpts.SrcPath := OldSrcPath;
    FCompilerOpts.DebugPath := OldDebugPath;
  end;
  Result := True;
end;

function TCompilerPathOptionsFrame.GetTitle: string;
begin
  Result := dlgSearchPaths;
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
      if MessageDlg('Hint', 'The ' + Context +
        ' contains a star * character.'#13 +
        'Lazarus uses this as normal character and does not expand this as file mask.',
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
          if MessageDlg('Warning', 'The ' + Context +
            ' contains a not existing directory:'#13 + CurPath,
            mtWarning, [mbIgnore, mbCancel], 0) <> mrIgnore then
            Exit;
        end;
      end;
    until p > length(ExpandedPath);
  end;

  // check for special characters
  if (not IDEMacros.StrHasMacros(CurPath)) then
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
        if MessageDlg('Warning', Context + #13 + ErrorMsg, mtWarning,
          [mbOK, mbCancel], 0) <> mrOk then
          exit;
      end;
    end;
  end;

  Result := True;
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
      OldPath := edtOtherUnits.Text;
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
      OldPath := edtIncludeFiles.Text;
      Templates := 'include' + ';inc';
    end
    else
    if AButton = OtherSourcesPathEditBtn then
    begin
      OldPath := edtOtherSources.Text;
      Templates := SetDirSeparators('$(LazarusDir)/lcl' +
        ';$(LazarusDir)/lcl/interfaces/$(LCLWidgetType)' +
        ';$(LazarusDir)/components/synedit' + ';$(LazarusDir)/components/codetools');
    end
    else
    if AButton = LibrariesPathEditBtn then
    begin
      OldPath := edtLibraries.Text;
      Templates := SetDirSeparators('/usr/X11R6/lib;/sw/lib');
    end
    else
    if AButton = DebugPathEditBtn then
    begin
      OldPath := edtDebugPath.Text;
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
    ExpandedPath := TrimSearchPath(NewPath, BaseDir);
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
      if CheckPath(lblOtherUnits.Caption, NewPath) then
        edtOtherUnits.Text := NewPath;
    end
    else
    if AButton = IncludeFilesPathEditBtn then
    begin
      if CheckPath(lblIncludeFiles.Caption, NewPath) then
        edtIncludeFiles.Text := NewPath;
    end
    else
    if AButton = OtherSourcesPathEditBtn then
    begin
      if CheckPath(lblOtherSources.Caption, NewPath) then
        edtOtherSources.Text := NewPath;
    end
    else
    if AButton = LibrariesPathEditBtn then
    begin
      if CheckPath(lblLibraries.Caption, NewPath) then
        edtLibraries.Text := NewPath;
    end
    else
    if AButton = DebugPathEditBtn then
    begin
      if CheckPath(lblDebugPath.Caption, NewPath) then
        edtDebugPath.Text := NewPath;
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
        edtUnitOutputDir.Text := OpenDialog.Filename;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TCompilerPathOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  LCLInterface: TLCLPlatform;
  s: string;
begin
  lblOtherUnits.Caption := dlgOtherUnitFiles;
  OtherUnitsPathEditBtn := TPathEditorButton.Create(Self);
  with OtherUnitsPathEditBtn do
  begin
    Name := 'OtherUnitsPathEditBtn';
    Caption := '...';
    Anchors := [akRight, akTop, akBottom];
    AnchorParallel(akTop, 0, edtOtherUnits);
    AnchorParallel(akBottom, 0, edtOtherUnits);
    AnchorParallel(akRight, 0, Self);
    AutoSize := True;
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
    Parent := Self;
  end;
  edtOtherUnits.AnchorToNeighbour(akRight, 0, OtherUnitsPathEditBtn);

  {------------------------------------------------------------}

  lblIncludeFiles.Caption := dlgCOIncFiles;
  IncludeFilesPathEditBtn := TPathEditorButton.Create(Self);
  with IncludeFilesPathEditBtn do
  begin
    Name := 'IncludeFilesPathEditBtn';
    Anchors := [akRight, akTop, akBottom];
    AnchorParallel(akTop, 0, edtIncludeFiles);
    AnchorParallel(akBottom, 0, edtIncludeFiles);
    AnchorParallel(akRight, 0, Self);
    AutoSize := True;
    Caption := '...';
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
    Parent := Self;
  end;
  edtIncludeFiles.AnchorToNeighbour(akRight, 0, IncludeFilesPathEditBtn);

  {------------------------------------------------------------}

  lblOtherSources.Caption := dlgCOSources;
  OtherSourcesPathEditBtn := TPathEditorButton.Create(Self);
  with OtherSourcesPathEditBtn do
  begin
    Name := 'OtherSourcesPathEditBtn';
    Anchors := [akRight, akTop, akBottom];
    AnchorParallel(akTop, 0, edtOtherSources);
    AnchorParallel(akBottom, 0, edtOtherSources);
    AnchorParallel(akRight, 0, Self);
    AutoSize := True;
    Caption := '...';
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
    Parent := Self;
  end;
  edtOtherSources.AnchorToNeighbour(akRight, 0, OtherSourcesPathEditBtn);

  {------------------------------------------------------------}

  lblLibraries.Caption := dlgCOLibraries;
  LibrariesPathEditBtn := TPathEditorButton.Create(Self);
  with LibrariesPathEditBtn do
  begin
    Name := 'LibrariesPathEditBtn';
    Anchors := [akRight, akTop, akBottom];
    AnchorParallel(akTop, 0, edtLibraries);
    AnchorParallel(akBottom, 0, edtLibraries);
    AnchorParallel(akRight, 0, Self);
    AutoSize := True;
    Caption := '...';
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
    Parent := Self;
  end;
  edtLibraries.AnchorToNeighbour(akRight, 0, LibrariesPathEditBtn);

  {------------------------------------------------------------}

  lblUnitOutputDir.Caption := dlgUnitOutp;
  btnUnitOutputDir := TButton.Create(Self);
  with btnUnitOutputDir do
  begin
    Name := 'btnUnitOutputDir';
    Anchors := [akRight, akTop, akBottom];
    AnchorParallel(akTop, 0, edtUnitOutputDir);
    AnchorParallel(akBottom, 0, edtUnitOutputDir);
    AnchorParallel(akRight, 0, Self);
    AutoSize := True;
    Caption := '...';
    OnClick := @FileBrowseBtnClick;
    Parent := Self;
  end;
  edtUnitOutputDir.AnchorToNeighbour(akRight, 0, btnUnitOutputDir);

  {------------------------------------------------------------}

  lblDebugPath.Caption := dlgCODebugPath;
  DebugPathEditBtn := TPathEditorButton.Create(Self);
  with DebugPathEditBtn do
  begin
    Name := 'DebugPathEditBtn';
    Anchors := [akRight, akTop, akBottom];
    AnchorParallel(akTop, 0, edtDebugPath);
    AnchorParallel(akBottom, 0, edtDebugPath);
    AnchorParallel(akRight, 0, Self);
    AutoSize := True;
    Caption := '...';
    OnClick := @PathEditBtnClick;
    OnExecuted := @PathEditBtnExecuted;
    Parent := Self;
  end;
  edtDebugPath.AnchorToNeighbour(akRight, 0, DebugPathEditBtn);

  {------------------------------------------------------------}

  LCLWidgetTypeLabel.Caption := Format(lisCOVarious, [lisLCLWidgetType]);
  with LCLWidgetTypeComboBox do
  begin
    with Items do
    begin
      BeginUpdate;
      s := LCLPlatformDisplayNames[GetDefaultLCLWidgetType];
      Add(Format(lisCOdefault, [s]));
      for LCLInterface := Low(TLCLPlatform) to High(TLCLPlatform) do
      begin
        Items.Add(LCLPlatformDisplayNames[LCLInterface]);
      end;
      EndUpdate;
    end;
    ItemIndex := 1;
    Constraints.MinWidth := 150;
  end;
end;

procedure TCompilerPathOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  LCLPlatform: TLCLPlatform;
begin
  FCompilerOpts := AOptions as TProjectCompilerOptions;
  with FCompilerOpts do
  begin
    edtOtherUnits.Text := OtherUnitFiles;
    edtIncludeFiles.Text := IncludePath;
    edtLibraries.Text := Libraries;
    edtOtherSources.Text := SrcPath;
    edtUnitOutputDir.Text := UnitOutputDirectory;
    edtDebugPath.Text := DebugPath;

    LCLPlatform := DirNameToLCLPlatform(LCLWidgetType);
    if CompareText(LCLWidgetType, LCLPlatformDirNames[LCLPlatform]) = 0 then
      LCLWidgetTypeComboBox.ItemIndex := Ord(LCLPlatform) + 1
    else
      LCLWidgetTypeComboBox.ItemIndex := 0;
  end;
end;

procedure TCompilerPathOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  i: integer;
begin
  with AOptions as TProjectCompilerOptions do
  begin
    OtherUnitFiles := edtOtherUnits.Text;
    IncludePath := edtIncludeFiles.Text;
    Libraries := edtLibraries.Text;
    SrcPath := edtOtherSources.Text;
    UnitOutputDirectory := edtUnitOutputDir.Text;
    DebugPath := edtDebugPath.Text;
    // ToDo: will be replaced by buildmodes
    i := LCLWidgetTypeComboBox.ItemIndex;
    if i <= 0 then
      LCLWidgetType := ''
    else
      LCLWidgetType := LCLPlatformDirNames[TLCLPlatform(i - 1)];
  end;
end;

class function TCompilerPathOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectCompilerOptions;
end;

initialization
  {$I compiler_path_options.lrs}
  RegisterIDEOptionsEditor(GroupCompiler, TCompilerPathOptionsFrame,
    CompilerOptionsSearchPaths);

end.

