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
  FileUtil, LazFileUtils, LazStringUtils, LazFileCache, LazLoggerBase,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // IdeIntf
  PropEdits, ObjectInspector, IDEOptionsIntf, IDEOptEditorIntf, IDEUtils,
  GDBMIDebugger,
  // IDE
  TransferMacros, LazarusIDEStrConsts, PathEditorDlg, IDEProcs, DialogProcs,
  InputHistory, EnvironmentOpts, BaseDebugManager, Debugger;

type

  { TDebuggerClassOptionsFrame }

  TDebuggerClassOptionsFrame = class(TAbstractIDEOptionsEditor)
    cmbDebuggerPath: TComboBox;
    cmbDebuggerType: TComboBox;
    cmdOpenAdditionalPath: TButton;
    cmdOpenDebuggerPath: TButton;
    edName: TEdit;
    gbAdditionalSearchPath: TGroupBox;
    gbDebuggerSpecific: TGroupBox;
    gbDebuggerType: TGroupBox;
    lblName: TLabel;
    Panel1: TPanel;
    tbDropMenu: TPopupMenu;
    ToolBar1: TToolBar;
    tbSelect: TToolButton;
    tbAddNew: TToolButton;
    ToolButton2: TToolButton;
    tbDelete: TToolButton;
    tbCopy: TToolButton;
    ToolButton3: TToolButton;
    txtAdditionalPath: TEdit;
    procedure cmbDebuggerPathEditingDone(Sender: TObject);
    procedure cmbDebuggerTypeEditingDone(Sender: TObject);
    procedure cmdOpenAdditionalPathClick(Sender: TObject);
    procedure cmdOpenDebuggerPathClick(Sender: TObject);
    procedure edNameExit(Sender: TObject);
    procedure tbAddNewClick(Sender: TObject);
    procedure tbCopyClick(Sender: TObject);
    procedure tbDeleteClick(Sender: TObject);
    procedure tbSelectClick(Sender: TObject);
  private
    FDebuggerFileHistory: TStringList;
    FInOdNameExit: Boolean;
    PropertyGrid: TOIPropertyGrid;
    FPropertyEditorHook: TPropertyEditorHook;
    FCopiedDbgPropertiesConfigList: TDebuggerPropertiesConfigList;
    FSelectedDbgPropertiesConfig: TDebuggerPropertiesConfig;
    FLastCheckedDebuggerPath: String;
    function SelectedDebuggerClass: TDebuggerClass; // currently shown debugger class
    function SelectedDebuggerProperties: TDebuggerProperties;

    procedure DoNameSelected(Sender: TObject);
    procedure FillDebuggerClassDropDown;
    procedure UpdateDebuggerClass;
    procedure UpdateDebuggerClassDropDown;
    procedure FetchDebuggerSpecificOptions;
    function  GetDebuggerClassFromDropDown: TDebuggerClass;
    function  GetUniqueName(AName: String): String;
    procedure ClearDbgProperties;
    procedure FillNameDropDown;
    procedure HookGetCheckboxForBoolean(var Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Check: Boolean; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TDebuggerClassOptionsFrame }

procedure TDebuggerClassOptionsFrame.cmbDebuggerPathEditingDone(Sender: TObject
  );
var
  ParsedFName: String;
begin
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

procedure TDebuggerClassOptionsFrame.cmdOpenAdditionalPathClick(
  Sender: TObject);
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
begin
  if FSelectedDbgPropertiesConfig = nil then
    exit;

  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:=lisChooseDebuggerExecutable;

    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      ParsedFName := EnvironmentOptions.GetParsedValue(eopDebuggerFilename, AFilename);
      if ParsedFName = '' then
        ParsedFName := AFilename;
      if CheckExecutable(FSelectedDbgPropertiesConfig.DebuggerFilename, ParsedFName,
        lisEnvOptDlgInvalidDebuggerFilename,
        lisEnvOptDlgInvalidDebuggerFilenameMsg)
      then begin
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
  edNameExit(nil);
  UpdateDebuggerClass;
  cmbDebuggerPathEditingDone(nil);

  FSelectedDbgPropertiesConfig := TDebuggerPropertiesConfig.CreateForDebuggerClass(TGDBMIDebugger);
  FSelectedDbgPropertiesConfig.ConfigName := GetUniqueName(lisNew);
  FCopiedDbgPropertiesConfigList.AddObject(FSelectedDbgPropertiesConfig.ConfigName, FSelectedDbgPropertiesConfig);

  FillNameDropDown;
  UpdateDebuggerClassDropDown;
  FetchDebuggerSpecificOptions;
end;

procedure TDebuggerClassOptionsFrame.tbCopyClick(Sender: TObject);
var
  pc: TDebuggerPropertiesConfig;
  s: String;
begin
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
end;

procedure TDebuggerClassOptionsFrame.tbDeleteClick(Sender: TObject);
var
  i: Integer;
begin
  if FSelectedDbgPropertiesConfig = nil then
    exit;

  i := FCopiedDbgPropertiesConfigList.IndexOfObject(FSelectedDbgPropertiesConfig);
  FSelectedDbgPropertiesConfig.MarkAsDeleted;
  FCopiedDbgPropertiesConfigList[i] := ''; // remove from named part of list

  FillNameDropDown;
  UpdateDebuggerClassDropDown;
  FetchDebuggerSpecificOptions;
end;

procedure TDebuggerClassOptionsFrame.tbSelectClick(Sender: TObject);
begin
  tbSelect.CheckMenuDropdown;
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
  List: TStringList;
  i: Integer;
  d: TDebuggerClass;
begin
  List := TStringList.Create;
  for i := 0 to TBaseDebugManagerIntf.DebuggerCount - 1 do begin
    d := TBaseDebugManagerIntf.Debuggers[i];
    List.AddObject(d.Caption, TObject(d));
  end;
  List.Sorted := True;
  cmbDebuggerType.Items.Assign(List);
  FreeAndNil(List);

  UpdateDebuggerClassDropDown;
end;

procedure TDebuggerClassOptionsFrame.cmbDebuggerTypeEditingDone(
  Sender: TObject);
begin
  UpdateDebuggerClass;
  FetchDebuggerSpecificOptions;
end;

procedure TDebuggerClassOptionsFrame.UpdateDebuggerClass;
var
  c: TDebuggerClass;
  i: Integer;
begin
  if FSelectedDbgPropertiesConfig = nil then
    exit;
  c := GetDebuggerClassFromDropDown;
  if SelectedDebuggerClass = c then
    exit;

  i := FDebuggerFileHistory.IndexOf(SelectedDebuggerClass.ExePathsMruGroup.ClassName);
  Assert((i>=0) or (not SelectedDebuggerClass.NeedsExePath), 'Missing dbg lru');
  if i >= 0 then // not found if not NeedExePath
    TStringList(FDebuggerFileHistory.Objects[i]).Assign(cmbDebuggerPath.Items);


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

procedure TDebuggerClassOptionsFrame.DoNameSelected(Sender: TObject);
var
  idx: PtrInt;
begin
  idx := TMenuItem(Sender).Tag;

  edNameExit(nil);
  UpdateDebuggerClass;
  cmbDebuggerPathEditingDone(nil);

  FSelectedDbgPropertiesConfig := FCopiedDbgPropertiesConfigList.Opt[idx];
  FillNameDropDown;

  UpdateDebuggerClassDropDown;
  FetchDebuggerSpecificOptions;
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
      lru.Assign(EnvironmentOptions.DebuggerFileHistory[SelectedDebuggerClass.ExePathsMruGroup.ClassName]);
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

//  txtAdditionalPath.Text:=EnvironmentOptions.GetParsedDebuggerSearchPath;

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
         (FCopiedDbgPropertiesConfigList.Opt[i].IsDeleted) or
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
    if (j >= 0) and (FCopiedDbgPropertiesConfigList.Opt[i]= FSelectedDbgPropertiesConfig)
    then
      j := -1;
  until j < 0;

  Result := Result+' ('+IntToStr(i)+')';
end;

procedure TDebuggerClassOptionsFrame.ClearDbgProperties;
begin
  PropertyGrid.Selection.Clear;
  FCopiedDbgPropertiesConfigList.ClearAll;
end;

procedure TDebuggerClassOptionsFrame.FillNameDropDown;
var
  m: TMenuItem;
  i: Integer;
begin
  tbDropMenu.Items.Clear;
  for i := 0 to FCopiedDbgPropertiesConfigList.Count - 1 do
    if (not FCopiedDbgPropertiesConfigList.Opt[i].IsDeleted) and
       (FCopiedDbgPropertiesConfigList.Opt[i].IsLoaded)
    then begin
      m := TMenuItem.Create(tbDropMenu);
      m.Caption := FCopiedDbgPropertiesConfigList.Opt[i].DisplayName;
      m.Tag := i;
      m.OnClick := @DoNameSelected;
      m.Checked := FCopiedDbgPropertiesConfigList.Opt[i] = FSelectedDbgPropertiesConfig;
      tbDropMenu.Items.Add(m);
    end;
  if FSelectedDbgPropertiesConfig <> nil then
    tbSelect.Caption := FSelectedDbgPropertiesConfig.DisplayName
  else
    tbSelect.Caption := '---';
  tbSelect.Enabled := FCopiedDbgPropertiesConfigList.Count > 0;
  Panel1.Enabled := FCopiedDbgPropertiesConfigList.Count > 0;
  tbCopy.Enabled := FSelectedDbgPropertiesConfig <> nil;
  tbDelete.Enabled := FSelectedDbgPropertiesConfig <> nil;
end;

procedure TDebuggerClassOptionsFrame.HookGetCheckboxForBoolean(var Value: Boolean);
begin
  Value := EnvironmentOptions.ObjectInspectorOptions.CheckboxForBoolean;
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
  FCopiedDbgPropertiesConfigList.CaseSensitive := False;
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
  tbAddNew.Caption := lisAdd;
  tbCopy.Caption := lisCopy;
  tbDelete.Caption := lisDelete;
  lblName.Caption := lisDebugOptionsFrmName;
  gbDebuggerType.Caption := dlgDebugType;
  gbAdditionalSearchPath.Caption := lisDebugOptionsFrmAdditionalSearchPath;
  gbDebuggerSpecific.Caption := lisDebugOptionsFrmDebuggerSpecific;
end;

procedure TDebuggerClassOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  i: Integer;
begin
  ClearDbgProperties;
  with EnvironmentOptions do
  begin
    ObjectInspectorOptions.AssignTo(PropertyGrid);

    FDebuggerFileHistory.Clear;
    FCopiedDbgPropertiesConfigList.ClearAll;
    for i := 0 to DebuggerPropertiesConfigList.Count - 1 do
      FCopiedDbgPropertiesConfigList.AddObject(DebuggerPropertiesConfigList[i],
        TDebuggerPropertiesConfig.CreateCopy(DebuggerPropertiesConfigList.Opt[i], True, True) );
    // Find our copy of the current entry
    if CurrentDebuggerPropertiesConfig = nil then
      FSelectedDbgPropertiesConfig := nil
    else
      FSelectedDbgPropertiesConfig := FCopiedDbgPropertiesConfigList.EntryByName(
        CurrentDebuggerPropertiesConfig.ConfigName, CurrentDebuggerPropertiesConfig.ConfigClass);

    FillNameDropDown;
    FillDebuggerClassDropDown;
    FetchDebuggerSpecificOptions;
  end;
end;

procedure TDebuggerClassOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  i: Integer;
  EnvConf: TDebuggerPropertiesConfigList;
begin
  with EnvironmentOptions do
  begin
    for i := 0 to FDebuggerFileHistory.Count - 1 do
      DebuggerFileHistory[FDebuggerFileHistory[i]].Assign(TStringList(FDebuggerFileHistory.Objects[i]));

//    DebuggerSearchPath := TrimSearchPath(txtAdditionalPath.Text,'');

    EnvConf := DebuggerPropertiesConfigList;
    EnvConf.ClearAll;
    for i := 0 to FCopiedDbgPropertiesConfigList.Count - 1 do
      EnvConf.AddObject(FCopiedDbgPropertiesConfigList[i],
        TDebuggerPropertiesConfig.CreateCopy(FCopiedDbgPropertiesConfigList.Opt[i], True, True) );
    if FSelectedDbgPropertiesConfig = nil then
      CurrentDebuggerPropertiesConfig := nil
    else
      CurrentDebuggerPropertiesConfig := DebuggerPropertiesConfigList.EntryByName(
        FSelectedDbgPropertiesConfig.ConfigName, FSelectedDbgPropertiesConfig.ConfigClass);
    SaveDebuggerPropertiesList; // Update XML
  end;
end;

class function TDebuggerClassOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TDebuggerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupDebugger, TDebuggerClassOptionsFrame, DbgOptionsGeneral);
end.

