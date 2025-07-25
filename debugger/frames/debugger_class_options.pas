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
}
unit debugger_class_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo,
  // LCL
  Forms, Controls, StdCtrls, ExtCtrls, Buttons, Dialogs, ComCtrls, Menus,
  // LazUtils
  FileUtil, LazFileUtils, LazStringUtils, LazFileCache, LazLoggerBase, LazUTF8,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // BuildIntf
  IDEOptionsIntf,
  // IdeIntf
  PropEdits, ObjectInspector, IDEOptEditorIntf, IDEUtils, IdeIntfStrConsts,
  // IdeUtils
  InputHistory, DialogProcs,
  // LazDebuggerGDBMI
  GDBMIDebugger, DividerBevel,
  // IdeDebugger
  Debugger, IdeDebuggerOpts, EnvDebuggerOptions, ProjectDebugLink,
  // IdeConfig
  EnvironmentOpts, TransferMacros,
  // IDE
  LazarusIDEStrConsts, PathEditorDlg, DebugManager, EnvGuiOptions;

type

  { TDebuggerClassOptionsFrame }

  TDebuggerClassOptionsFrame = class(TAbstractIDEOptionsEditor)
    BtnEditClass: TButton;
    btnAdd: TButton;
    btnCopy: TButton;
    btnDelete: TButton;
    cmbDebuggerPath: TComboBox;
    cmbDebuggerType: TComboBox;
    cmdOpenAdditionalPath: TButton;
    cmdOpenDebuggerPath: TButton;
    cbBackend: TComboBox;
    divSelectBackend: TDividerBevel;
    divEditBackend: TDividerBevel;
    edName: TEdit;
    gbAdditionalSearchPath: TGroupBox;
    gbDebuggerSpecific: TGroupBox;
    gbDebuggerType: TGroupBox;
    lblWarningProject: TLabel;
    LblWarnClassChange: TLabel;
    lblName: TLabel;
    Panel1: TPanel;
    txtAdditionalPath: TEdit;
    procedure BtnEditClassClick(Sender: TObject);
    procedure cbBackendChange(Sender: TObject);
    procedure cmbDebuggerPathEditingDone(Sender: TObject);
    procedure cmbDebuggerTypeEditingDone(Sender: TObject);
    procedure cmdOpenAdditionalPathClick(Sender: TObject);
    procedure cmdOpenDebuggerPathClick(Sender: TObject);
    procedure edNameExit(Sender: TObject);
    procedure tbAddNewClick(Sender: TObject);
    procedure tbCopyClick(Sender: TObject);
    procedure tbDeleteClick(Sender: TObject);
  private
    FDebuggerFileHistory: TStringList;
    FInOdNameExit: Boolean;
    FOnModifiedDbgPropertiesCountChanged: TNotifyEvent;
    FShowWarningOverridenByProject: boolean;
    PropertyGrid: TOIPropertyGrid;
    FPropertyEditorHook: TPropertyEditorHook;
    FCopiedDbgPropertiesConfigList: TDebuggerPropertiesConfigList;
    FSelectedDbgPropertiesConfig: TDebuggerPropertiesConfig;
    FUpdatingBackendDropDown: Boolean;
    FLastCheckedDebuggerPath: String;
    function SelectedDebuggerClass: TDebuggerClass; // currently shown debugger class
    function SelectedDebuggerProperties: TDebuggerProperties;

    procedure FillDebuggerClassDropDown;
    procedure UpdateDebuggerClass;
    procedure UpdateDebuggerClassDropDown;
    procedure UpdateDebuggerPathHistory;
    procedure FetchDebuggerSpecificOptions;
    function  GetDebuggerClassFromDropDown: TDebuggerClass;
    function  GetUniqueName(AName: String): String;
    procedure ClearDbgProperties;
    procedure FillNameDropDown;
    procedure HookGetCheckboxForBoolean(var Value: Boolean);
    procedure DoModifiedDbgPropertiesCountChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Check: Boolean; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure ReadSettings(ADbgConf: TDebuggerPropertiesConfigList);
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(ADbgConf: TDebuggerPropertiesConfigList);
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    property ModifiedDbgPropertiesConfigList: TDebuggerPropertiesConfigList read FCopiedDbgPropertiesConfigList;
    property ShowWarningOverridenByProject: boolean read FShowWarningOverridenByProject write FShowWarningOverridenByProject;
    property OnModifiedDbgPropertiesCountChanged: TNotifyEvent read FOnModifiedDbgPropertiesCountChanged write FOnModifiedDbgPropertiesCountChanged;
  end;

implementation

{$R *.lfm}

{ TDebuggerClassOptionsFrame }

procedure TDebuggerClassOptionsFrame.cmbDebuggerPathEditingDone(Sender: TObject);
var
  ParsedFName: String;
begin
  UpdateDebuggerPathHistory;
  if FSelectedDbgPropertiesConfig = nil then
    exit;

  if assigned(SelectedDebuggerClass) and SelectedDebuggerClass.NeedsExePath and
     (FSelectedDbgPropertiesConfig.DebuggerFilename <> cmbDebuggerPath.Text) and
     (FLastCheckedDebuggerPath <> cmbDebuggerPath.Text)
  then begin
    FLastCheckedDebuggerPath := cmbDebuggerPath.Text;
    ParsedFName := EnvironmentOptions.GetParsedValue(eopDebuggerFilename, FLastCheckedDebuggerPath);
    if ParsedFName = '' then
      ParsedFName := FLastCheckedDebuggerPath;
    if not CheckExecutable(FSelectedDbgPropertiesConfig.DebuggerFilename, ParsedFName,
          lisEnvOptDlgInvalidDebuggerFilename,
          lisEnvOptDlgInvalidDebuggerFilenameMsg)
    then
      exit;
  end;

  FSelectedDbgPropertiesConfig.DebuggerFilename := cmbDebuggerPath.Text;
end;

procedure TDebuggerClassOptionsFrame.BtnEditClassClick(Sender: TObject);
begin
  cmbDebuggerType.Enabled := True;
  BtnEditClass.Visible := False;
  LblWarnClassChange.Visible := True;
end;

procedure TDebuggerClassOptionsFrame.cbBackendChange(Sender: TObject);
var
  idx: PtrInt;
begin
  if FUpdatingBackendDropDown then
    exit;

  UpdateDebuggerPathHistory;
  idx := cbBackend.ItemIndex;

  edNameExit(nil);
  UpdateDebuggerClass;
  cmbDebuggerPathEditingDone(nil);

  FSelectedDbgPropertiesConfig := FCopiedDbgPropertiesConfigList.Opt[idx];
  FillNameDropDown;

  UpdateDebuggerClassDropDown;
  FetchDebuggerSpecificOptions;
end;

procedure TDebuggerClassOptionsFrame.cmdOpenAdditionalPathClick(Sender: TObject);
begin
  PathEditorDialog.Path:=txtAdditionalPath.Text;
  PathEditorDialog.Templates:=GetForcedPathDelims(
        '$(LazarusDir)/include/$(TargetOS)'
      +';$(FPCSrcDir)/rtl/inc/'
      +';$(FPCSrcDir)/rtl/$(SrcOS)'
      +';$(FPCSrcDir)/rtl/$(TargetOS)'
      );
  if PathEditorDialog.ShowModal=mrOk then
    txtAdditionalPath.Text:=PathEditorDialog.Path;
end;

procedure TDebuggerClassOptionsFrame.cmdOpenDebuggerPathClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename, ParsedFName: string;
  lDirText : string;
  lExpandedName: string; // Expanded name before Dialog
  lDirName, lDirNameF : string;
begin
  if FSelectedDbgPropertiesConfig = nil then
    exit;

  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:=lisChooseDebuggerExecutable;

    lDirName := EnvironmentOptions.GetParsedValue(eopDebuggerFilename, lDirText{%H-});
    lExpandedName := CleanAndExpandFilename(lDirName);
    lDirName := GetValidDirectory(lDirName, {out} lDirNameF);
    OpenDialog.InitialDir := lDirName;
    OpenDialog.FileName := lDirNameF;

    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      ParsedFName := EnvironmentOptions.GetParsedValue(eopDebuggerFilename, AFilename);
      if ParsedFName = '' then
        ParsedFName := AFilename;
      if CheckExecutable(FSelectedDbgPropertiesConfig.DebuggerFilename, ParsedFName,
        lisEnvOptDlgInvalidDebuggerFilename,
        lisEnvOptDlgInvalidDebuggerFilenameMsg)
      then
        if CompareText(lExpandedName, AFilename) <> 0 then begin // Changed ?
          SetComboBoxText(cmbDebuggerPath,AFilename,cstFilename);
          FSelectedDbgPropertiesConfig.DebuggerFilename := AFilename;
        end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TDebuggerClassOptionsFrame.edNameExit(Sender: TObject);
var
  n: String;
  i: Integer;
begin
  if FSelectedDbgPropertiesConfig = nil then
    exit;
  try
    FInOdNameExit := True;
    n := GetUniqueName(edName.Text);
    if n <> edName.Text then
      edName.Text := n;
    if FSelectedDbgPropertiesConfig.ConfigName <> n then begin
      FSelectedDbgPropertiesConfig.ConfigName := n;
      i := FCopiedDbgPropertiesConfigList.IndexOfObject(FSelectedDbgPropertiesConfig);
      FCopiedDbgPropertiesConfigList[i] := n;
      FillNameDropDown;
    end;
  finally
    FInOdNameExit := False;
  end;
end;

procedure TDebuggerClassOptionsFrame.tbAddNewClick(Sender: TObject);
begin
  UpdateDebuggerPathHistory;
  edNameExit(nil);
  UpdateDebuggerClass;
  cmbDebuggerPathEditingDone(nil);

  FSelectedDbgPropertiesConfig := TDebuggerPropertiesConfig.CreateForDebuggerClass(TGDBMIDebugger, True);
  FSelectedDbgPropertiesConfig.ConfigName := GetUniqueName(lisNew);
  FCopiedDbgPropertiesConfigList.AddObject(FSelectedDbgPropertiesConfig.ConfigName, FSelectedDbgPropertiesConfig);

  FillNameDropDown;
  UpdateDebuggerClassDropDown;
  FetchDebuggerSpecificOptions;

  cmbDebuggerType.Enabled := True;
  BtnEditClass.Visible := False;
  LblWarnClassChange.Visible := False;
  DoModifiedDbgPropertiesCountChanged;
end;

procedure TDebuggerClassOptionsFrame.tbCopyClick(Sender: TObject);
var
  pc: TDebuggerPropertiesConfig;
  s: String;
begin
  UpdateDebuggerPathHistory;
  if FSelectedDbgPropertiesConfig = nil then
    exit;

  edNameExit(nil);
  UpdateDebuggerClass;
  cmbDebuggerPathEditingDone(nil);

  pc := FSelectedDbgPropertiesConfig;
  s := pc.ConfigName;
  if s = '' then
    s := lisNew;
  FSelectedDbgPropertiesConfig := TDebuggerPropertiesConfig.CreateCopy(pc);
  FSelectedDbgPropertiesConfig.ConfigName := GetUniqueName(s);
  FCopiedDbgPropertiesConfigList.AddObject(FSelectedDbgPropertiesConfig.ConfigName, FSelectedDbgPropertiesConfig);

  FillNameDropDown;
  UpdateDebuggerClassDropDown;
  FetchDebuggerSpecificOptions;

  cmbDebuggerType.Enabled := True;
  BtnEditClass.Visible := False;
  LblWarnClassChange.Visible := False;
  DoModifiedDbgPropertiesCountChanged;
end;

procedure TDebuggerClassOptionsFrame.tbDeleteClick(Sender: TObject);
var
  i: Integer;
begin
  if FSelectedDbgPropertiesConfig = nil then
    exit;

  i := FCopiedDbgPropertiesConfigList.IndexOfObject(FSelectedDbgPropertiesConfig);
  if i >= 0 then
    FCopiedDbgPropertiesConfigList.Delete(i);
  FSelectedDbgPropertiesConfig := nil;

  FillNameDropDown;
  UpdateDebuggerClassDropDown;
  FetchDebuggerSpecificOptions;
  DoModifiedDbgPropertiesCountChanged;
end;

function TDebuggerClassOptionsFrame.SelectedDebuggerClass: TDebuggerClass;
begin
  if FSelectedDbgPropertiesConfig = nil then
    Result := nil
  else
    Result := FSelectedDbgPropertiesConfig.DebuggerClass;
end;

function TDebuggerClassOptionsFrame.SelectedDebuggerProperties: TDebuggerProperties;
begin
  if FSelectedDbgPropertiesConfig = nil then
    Result := nil
  else
    Result := FSelectedDbgPropertiesConfig.DebuggerProperties;
end;

procedure TDebuggerClassOptionsFrame.FillDebuggerClassDropDown;
var
  List: TStringListUTF8Fast;
  i: Integer;
  d: TDebuggerClass;
begin
  List := TStringListUTF8Fast.Create;
  for i := 0 to TBaseDebugManagerIntf.DebuggerCount - 1 do begin
    d := TBaseDebugManagerIntf.Debuggers[i];
    if (dfNotSuitableForOsArch in d.SupportedFeatures) and
       ( (DebuggerOptions.DebuggerPropertiesConfigList.ForcedUnsuitableClass = nil) or
         ( d <> DebuggerOptions.DebuggerPropertiesConfigList.ForcedUnsuitableClass) )
    then
      continue;
    List.AddObject(d.Caption, TObject(d));
  end;
  List.Sorted := True;
  cmbDebuggerType.Items.Assign(List);
  FreeAndNil(List);

  UpdateDebuggerClassDropDown;
end;

procedure TDebuggerClassOptionsFrame.cmbDebuggerTypeEditingDone(Sender: TObject);
begin
  UpdateDebuggerClass;
  FetchDebuggerSpecificOptions;
end;

procedure TDebuggerClassOptionsFrame.UpdateDebuggerClass;
var
  c: TDebuggerClass;
begin
  cmbDebuggerType.Enabled := False;
  BtnEditClass.Visible := True;
  LblWarnClassChange.Visible := False;

  if FSelectedDbgPropertiesConfig = nil then
    exit;
  c := GetDebuggerClassFromDropDown;
  if SelectedDebuggerClass = c then
    exit;

  UpdateDebuggerPathHistory;

  FSelectedDbgPropertiesConfig.ChangeDebuggerClass(c, True);
  // TOOD: Ask user?
  FSelectedDbgPropertiesConfig.ConfigName := GetUniqueName(FSelectedDbgPropertiesConfig.ConfigName);
  try
    FInOdNameExit := True;
    edName.Text := FSelectedDbgPropertiesConfig.ConfigName;
  finally
    FInOdNameExit := False;
  end;
  FillNameDropDown;
end;

procedure TDebuggerClassOptionsFrame.UpdateDebuggerClassDropDown;
begin
  if SelectedDebuggerClass = nil
  then SetComboBoxText(cmbDebuggerType, '(none)',cstCaseInsensitive)
  else SetComboBoxText(cmbDebuggerType, SelectedDebuggerClass.Caption,cstCaseInsensitive);
end;

procedure TDebuggerClassOptionsFrame.UpdateDebuggerPathHistory;
var
  i: Integer;
begin
  if FSelectedDbgPropertiesConfig = nil then
    exit;
  i := FDebuggerFileHistory.IndexOf(SelectedDebuggerClass.ExePathsMruGroup.ClassName);
  Assert((i>=0) or (not SelectedDebuggerClass.NeedsExePath), 'Missing dbg lru');
  if i >= 0 then // not found if not NeedExePath
    TStringList(FDebuggerFileHistory.Objects[i]).Assign(cmbDebuggerPath.Items);
end;

procedure TDebuggerClassOptionsFrame.FetchDebuggerSpecificOptions;
var
  S, S2, S3: String;
  Prop: TDebuggerProperties;
  lru: TStringList;
  i: Integer;
begin
  PropertyGrid.Selection.Clear;

  if FSelectedDbgPropertiesConfig = nil then begin
    cmbDebuggerPath.Items.Clear;
    cmbDebuggerPath.Text := '';
    edName.Text := '';
    PropertyGrid.BuildPropertyList;
    exit;
  end;

  if SelectedDebuggerClass.NeedsExePath then begin
    cmbDebuggerPath.Enabled := True;
    cmdOpenDebuggerPath.Enabled := True;
    i := FDebuggerFileHistory.IndexOf(SelectedDebuggerClass.ExePathsMruGroup.ClassName);
    if i >= 0 then begin
      lru := TStringList(FDebuggerFileHistory.Objects[i]);
    end
    else begin
      lru := TStringList.Create;
      lru.Assign(EnvironmentDebugOpts.DebuggerFileHistory[SelectedDebuggerClass.ExePathsMruGroup.ClassName]);
      FDebuggerFileHistory.AddObject(SelectedDebuggerClass.ExePathsMruGroup.ClassName, lru);
    end;

    with cmbDebuggerPath.Items do begin
      BeginUpdate;
      Assign(lru);
      if  (Count = 0)
      and (SelectedDebuggerClass <> nil)
      then begin
        S := SelectedDebuggerClass.ExePaths;
        while S <> '' do
        begin
          S2 := GetPart([], [';'], S);
          S3 := S2;
          if GlobalMacroList.SubstituteStr(S2)
          then Add(S2)
          else Add(S3);
          if S <> '' then System.Delete(S, 1, 1);
        end;
      end;
      EndUpdate;
    end;

    SetComboBoxText(cmbDebuggerPath,FSelectedDbgPropertiesConfig.DebuggerFilename,cstFilename,20);
  end
  else begin
    cmbDebuggerPath.Items.Clear;
    cmbDebuggerPath.Text := '';
    cmbDebuggerPath.Enabled := False;
    cmdOpenDebuggerPath.Enabled := False;
  end;

  edName.Text := FSelectedDbgPropertiesConfig.ConfigName;

//  txtAdditionalPath.Text:=DebuggerOptions.GetParsedDebuggerSearchPath;

  // get ptoperties
  Prop := SelectedDebuggerProperties;
  if Prop<>nil then
    PropertyGrid.Selection.Add(Prop);
  PropertyGrid.BuildPropertyList;
end;

function TDebuggerClassOptionsFrame.GetDebuggerClassFromDropDown: TDebuggerClass;
var
  idx: PtrInt;
begin
  Result := nil;

  idx := cmbDebuggerType.ItemIndex;
  if idx = -1 then Exit;
  Result := TDebuggerClass(cmbDebuggerType.Items.Objects[idx]);
end;

function TDebuggerClassOptionsFrame.GetUniqueName(AName: String): String;
  function TrimNumber(s: string): string;
  var
    i: Integer;
  begin
    Result := s;
    i := Length(s);
    if (i=0) or (s[i] <> ')') then
      exit;
    dec(i);
    while (i > 0) and (s[i] in ['0'..'9']) do
      dec(i);
    if (i=0) or (s[i] <> '(') then
      exit;
    dec(i);
    if (i<=1) or (s[i] <> ' ') then
      exit;
    Result := copy(s, 1, i-1);
  end;
var
  i, j: Integer;
begin
  Result := Trim(AName);
  if Result = '' then begin
    i := FCopiedDbgPropertiesConfigList.Count - 1;
    while i >= 0 do
      if (FCopiedDbgPropertiesConfigList[i] <> '') or
         (FCopiedDbgPropertiesConfigList.Opt[i].DebuggerClass <> SelectedDebuggerClass) or
         (FCopiedDbgPropertiesConfigList.Opt[i] = FSelectedDbgPropertiesConfig)
      then
        dec(i)
      else
        break;
    if i < 0 then
      exit;
  end
  else begin
    i := FCopiedDbgPropertiesConfigList.IndexOf(Result);
    if (i < 0) or (FCopiedDbgPropertiesConfigList.Opt[i] = FSelectedDbgPropertiesConfig) then
      exit;
  end;

  Result := TrimNumber(Result);
  i := 1;
  repeat
    inc(i);
    j := FCopiedDbgPropertiesConfigList.IndexOf(Result+' ('+IntToStr(i)+')');
    if (j >= 0) and (FCopiedDbgPropertiesConfigList.Opt[j]= FSelectedDbgPropertiesConfig)
    then
      j := -1;
  until j < 0;

  Result := Result+' ('+IntToStr(i)+')';
end;

procedure TDebuggerClassOptionsFrame.ClearDbgProperties;
begin
  PropertyGrid.Selection.Clear;
  FCopiedDbgPropertiesConfigList.Clear;
  DoModifiedDbgPropertiesCountChanged;
end;

procedure TDebuggerClassOptionsFrame.FillNameDropDown;
var
  i, j: Integer;
  c: TDebuggerPropertiesConfig;
begin
  FUpdatingBackendDropDown := True;
  try
    cbBackend.Items.Clear;
    j := -1;
    for i := 0 to FCopiedDbgPropertiesConfigList.Count - 1 do begin
      c := FCopiedDbgPropertiesConfigList.Opt[i];
      cbBackend.Items.Add(c.DisplayName);
      if FCopiedDbgPropertiesConfigList.Opt[i] = FSelectedDbgPropertiesConfig then
        j := i;
    end;
    cbBackend.ItemIndex := j;
    cbBackend.Enabled := FCopiedDbgPropertiesConfigList.Count > 0;

    Panel1.Enabled := FSelectedDbgPropertiesConfig <> nil;
    btnCopy.Enabled := FSelectedDbgPropertiesConfig <> nil;
    btnDelete.Enabled := FSelectedDbgPropertiesConfig <> nil;

    if ShowWarningOverridenByProject and Assigned(FSelectedDbgPropertiesConfig) then
      lblWarningProject.Visible := not (
        (DbgProjectLink.DebuggerBackend = FSelectedDbgPropertiesConfig.UID) or
        (DbgProjectLink.DebuggerBackend = 'IDE') or
        ( (DbgProjectLink.DebuggerBackend = '') and
          (DbgProjectLink.DebuggerPropertiesConfigList.Count = 0) )
      );
  finally
    FUpdatingBackendDropDown := False;
  end;
end;

procedure TDebuggerClassOptionsFrame.HookGetCheckboxForBoolean(var Value: Boolean);
begin
  Value := EnvironmentGuiOpts.ObjectInspectorOptions.CheckboxForBoolean;
end;

procedure TDebuggerClassOptionsFrame.DoModifiedDbgPropertiesCountChanged;
begin
  if FOnModifiedDbgPropertiesCountChanged <> nil then
    FOnModifiedDbgPropertiesCountChanged(Self);
end;

constructor TDebuggerClassOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // create the PropertyEditorHook (the interface to the properties)
  FPropertyEditorHook:=TPropertyEditorHook.Create(Self);
  FPropertyEditorHook.AddHandlerGetCheckboxForBoolean(@HookGetCheckboxForBoolean);

  FDebuggerFileHistory := TStringList.Create;
  FDebuggerFileHistory.OwnsObjects := True;
  FCopiedDbgPropertiesConfigList := TDebuggerPropertiesConfigList.Create;
  // create the PropertyGrid
  PropertyGrid:=TOIPropertyGrid.CreateWithParams(Self,FPropertyEditorHook
      ,[tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkSet{, tkMethod}
      , tkSString, tkLString, tkAString, tkWString, tkVariant
      {, tkArray, tkRecord, tkInterface}, tkClass, tkObject, tkWChar, tkBool
      , tkInt64, tkQWord],
      0);
  with PropertyGrid do
  begin
    Name:='PropertyGrid';
    Parent := gbDebuggerSpecific;
    BorderSpacing.Around := 6;
    Visible := True;
    Align := alClient;
    PreferredSplitterX := 200;
    SplitterX := 200;
    Layout := oilHorizontal;
  end;
end;

destructor TDebuggerClassOptionsFrame.Destroy;
begin
  ClearDbgProperties;
  PropertyGrid.Selection.Clear;
  FreeAndNil(FPropertyEditorHook);
  FreeAndNil(FCopiedDbgPropertiesConfigList);
  FreeAndNil(FDebuggerFileHistory);
  inherited Destroy;
end;

function TDebuggerClassOptionsFrame.Check: Boolean;
begin
  if FSelectedDbgPropertiesConfig = nil then
    exit(True);

  edNameExit(nil);
  UpdateDebuggerClass; // TODO: might edit the name
  FLastCheckedDebuggerPath := 'X'+cmbDebuggerPath.Text; // ensure a new check is done
  cmbDebuggerPathEditingDone(nil);

  Result := (FSelectedDbgPropertiesConfig.DebuggerFilename = cmbDebuggerPath.Text);
end;

function TDebuggerClassOptionsFrame.GetTitle: String;
begin
  Result := lisDebugOptionsFrmBackend;
end;

procedure TDebuggerClassOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  divSelectBackend.Caption := dlgOptDebugBackendSelectDebuggerBackend;
  divEditBackend.Caption := dlgOptDebugBackendEditDebuggerBackend;
  btnAdd.Caption := lisAdd;
  btnCopy.Caption := lisCopy;
  btnDelete.Caption := lisDelete;

  lblName.Caption := lisDebugOptionsFrmName;
  lblWarningProject.Caption := dlgOptDebugBackendTheProjectOptionsHaveBeen;
  BtnEditClass.Caption := lisDebugOptionsFrmEditClass;
  LblWarnClassChange.Caption := lisDebugOptionsFrmEditClassWarn;
  gbDebuggerType.Caption := dlgDebugType;
  gbAdditionalSearchPath.Caption := lisDebugOptionsFrmAdditionalSearchPath;
  gbDebuggerSpecific.Caption := lisDebugOptionsFrmDebuggerSpecific;
end;

procedure TDebuggerClassOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  ShowWarningOverridenByProject := True;
  ReadSettings(DebuggerOptions.DebuggerPropertiesConfigList);
end;

procedure TDebuggerClassOptionsFrame.ReadSettings(
  ADbgConf: TDebuggerPropertiesConfigList);
var
  i: Integer;
begin
  EnvironmentGuiOpts.ObjectInspectorOptions.AssignTo(PropertyGrid);

  ClearDbgProperties;
  FDebuggerFileHistory.Clear;
  FCopiedDbgPropertiesConfigList.Clear;
  for i := 0 to ADbgConf.Count - 1 do
    FCopiedDbgPropertiesConfigList.AddObject(ADbgConf[i],
      TDebuggerPropertiesConfig.CreateCopy(ADbgConf.Opt[i], True, True) );
  // Find our copy of the current entry
  if ADbgConf.CurrentDebuggerPropertiesConfig = nil then
    FSelectedDbgPropertiesConfig := nil
  else
    FSelectedDbgPropertiesConfig := FCopiedDbgPropertiesConfigList.EntryByName(
      ADbgConf.CurrentDebuggerPropertiesConfig.ConfigName, ADbgConf.CurrentDebuggerPropertiesConfig.ConfigClass);

  FillNameDropDown;
  FillDebuggerClassDropDown;
  FetchDebuggerSpecificOptions;
  DoModifiedDbgPropertiesCountChanged;
end;

procedure TDebuggerClassOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  WriteSettings(DebuggerOptions.DebuggerPropertiesConfigList);
  DebuggerOptions.SaveDebuggerPropertiesList; // Update XML
end;

procedure TDebuggerClassOptionsFrame.WriteSettings(
  ADbgConf: TDebuggerPropertiesConfigList);
var
  i: Integer;
begin
  UpdateDebuggerPathHistory;
  for i := 0 to FDebuggerFileHistory.Count - 1 do
    EnvironmentDebugOpts.DebuggerFileHistory[FDebuggerFileHistory[i]].Assign(
                                   TStringList(FDebuggerFileHistory.Objects[i]));

//    DebuggerSearchPath := TrimSearchPath(txtAdditionalPath.Text,'');

  ADbgConf.Clear;
  for i := 0 to FCopiedDbgPropertiesConfigList.Count - 1 do
    ADbgConf.AddObject(FCopiedDbgPropertiesConfigList[i],
      TDebuggerPropertiesConfig.CreateCopy(FCopiedDbgPropertiesConfigList.Opt[i], True, True) );
  if FSelectedDbgPropertiesConfig = nil then
    ADbgConf.CurrentDebuggerPropertiesConfig := nil
  else
    ADbgConf.CurrentDebuggerPropertiesConfig := ADbgConf.EntryByName(
      FSelectedDbgPropertiesConfig.ConfigName, FSelectedDbgPropertiesConfig.ConfigClass);
end;

class function TDebuggerClassOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TDebuggerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupDebugger, TDebuggerClassOptionsFrame, DbgOptionsGeneral);
end.

