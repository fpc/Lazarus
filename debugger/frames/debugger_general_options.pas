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
unit debugger_general_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo,
  // LCL
  Forms, Controls, StdCtrls, ExtCtrls, Buttons, Dialogs, LCLProc,
  // LazUtils
  FileUtil, LazFileUtils, LazFileCache,
  // DebuggerIntf
  DbgIntfDebuggerBase,
  // IdeIntf
  PropEdits, ObjectInspector, IDEOptionsIntf, IDEOptEditorIntf, IDEUtils,
  // IDE
  TransferMacros, LazarusIDEStrConsts, PathEditorDlg, IDEProcs, DialogProcs,
  InputHistory, EnvironmentOpts, BaseDebugManager, Debugger;

type

  { TDebuggerGeneralOptionsFrame }

  TDebuggerGeneralOptionsFrame = class(TAbstractIDEOptionsEditor)
    cmbDebuggerPath: TComboBox;
    cmbDebuggerType: TComboBox;
    cmdOpenAdditionalPath: TButton;
    cmdOpenDebuggerPath: TButton;
    gbAdditionalSearchPath: TGroupBox;
    gbDebuggerSpecific: TGroupBox;
    gbDebuggerType: TGroupBox;
    gcbDebuggerGeneralOptions: TCheckGroup;
    txtAdditionalPath: TEdit;
    procedure cmbDebuggerTypeEditingDone(Sender: TObject);
    procedure cmdOpenAdditionalPathClick(Sender: TObject);
    procedure cmdOpenDebuggerPathClick(Sender: TObject);
  private
    PropertyGrid: TOIPropertyGrid;
    FCurDebuggerClass: TDebuggerClass; // currently shown debugger class
    FPropertyEditorHook: TPropertyEditorHook;
    FOldDebuggerFilename: string;
    fOldDebuggerSearchPath: string;
    FCurrentDebPropertiesList: TStringList; // temporarilly modified
    procedure FetchDebuggerClass;
    procedure FetchDebuggerGeneralOptions;
    procedure FetchDebuggerSpecificOptions;
    function  GetDebuggerClass: TDebuggerClass;
    procedure SetDebuggerClass(const AClass: TDebuggerClass);
    procedure ClearDbgProperties;
    procedure HookGetCheckboxForBoolean(var Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Check: Boolean; override;
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure RestoreSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TDebuggerGeneralOptionsFrame }

procedure TDebuggerGeneralOptionsFrame.cmbDebuggerTypeEditingDone(
  Sender: TObject);
begin
  SetDebuggerClass(GetDebuggerClass);
end;

procedure TDebuggerGeneralOptionsFrame.cmdOpenAdditionalPathClick(
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

procedure TDebuggerGeneralOptionsFrame.cmdOpenDebuggerPathClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:=lisChooseDebuggerExecutable;

    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(cmbDebuggerPath,AFilename,cstFilename);
      CheckExecutable(FOldDebuggerFilename,AFilename,
        lisEnvOptDlgInvalidDebuggerFilename,
        lisEnvOptDlgInvalidDebuggerFilenameMsg);
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TDebuggerGeneralOptionsFrame.FetchDebuggerClass;
var
  n: PtrInt;
  DbgClass, CurClass: TDebuggerClass;
  List: TStringList;
begin
  List := TStringList.Create;
  List.Sorted := True;

  CurClass := nil;
  for n := 0 to DebugBoss.DebuggerCount - 1 do
  begin
    DbgClass := DebugBoss.Debuggers[n];
    List.AddObject(DbgClass.Caption, TObject(n));
    if  (FCurDebuggerClass = nil)
    and (CompareText(DbgClass.ClassName, EnvironmentOptions.DebuggerConfig.DebuggerClass) = 0)
    then CurClass := DbgClass;
  end;

  cmbDebuggerType.Items.Assign(List);
  FreeAndNil(List);

  SetDebuggerClass(CurClass);
  if FCurDebuggerClass = nil
  then SetComboBoxText(cmbDebuggerType, '(none)',cstCaseInsensitive)
  else SetComboBoxText(cmbDebuggerType, FCurDebuggerClass.Caption,cstCaseInsensitive);

  txtAdditionalPath.Text:=EnvironmentOptions.GetParsedDebuggerSearchPath;
end;

procedure TDebuggerGeneralOptionsFrame.FetchDebuggerGeneralOptions;
begin
  // IMPORTANT if more items are added the indexes must be updated here!
  gcbDebuggerGeneralOptions.Checked[0] := EnvironmentOptions.DebuggerShowStopMessage;
  gcbDebuggerGeneralOptions.Checked[1] := EnvironmentOptions.DebuggerResetAfterRun;
end;

procedure TDebuggerGeneralOptionsFrame.FetchDebuggerSpecificOptions;
var
  S, S2, S3: String;
  i: Integer;
  Filename: string;
  NewFilename: string;
  Prop: TDebuggerProperties;
begin
  with cmbDebuggerPath.Items do begin
    BeginUpdate;
    Assign(EnvironmentOptions.DebuggerFileHistory);
    if  (Count = 0)
    and (FCurDebuggerClass <> nil)
    then begin
      S := FCurDebuggerClass.ExePaths;
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

  Filename:=cmbDebuggerPath.Text;
  if Filename='' then begin
    for i:=0 to cmbDebuggerPath.Items.Count-1 do begin
      NewFilename:=cmbDebuggerPath.Items[i];
      if FileExistsCached(NewFilename) then begin
        Filename:=NewFilename;
        break;
      end;
      NewFilename:=FindDefaultExecutablePath(ExtractFileName(Filename));
      if NewFilename<>'' then begin
        Filename:=NewFilename;
        break;
      end;
    end;
  end;
  SetComboBoxText(cmbDebuggerPath,Filename,cstFilename,20);

  // get ptoperties
  PropertyGrid.Selection.Clear;
  if FCurDebuggerClass<>nil then begin
    i := FCurrentDebPropertiesList.IndexOf(FCurDebuggerClass.ClassName);
    if i < 0 then begin
      Prop := TDebuggerPropertiesClass(FCurDebuggerClass.GetProperties.ClassType).Create;
      EnvironmentOptions.LoadDebuggerProperties(FCurDebuggerClass.ClassName, Prop);
      FCurrentDebPropertiesList.AddObject(FCurDebuggerClass.ClassName, Prop);
    end
    else
      Prop := TDebuggerProperties(FCurrentDebPropertiesList.Objects[i]);
    PropertyGrid.Selection.Add(Prop);
  end;
  PropertyGrid.BuildPropertyList;
end;

function TDebuggerGeneralOptionsFrame.GetDebuggerClass: TDebuggerClass;
var
  idx: PtrInt;
begin
  Result := nil;

  idx := cmbDebuggerType.ItemIndex;
  if idx = -1 then Exit;
  idx := PtrInt(cmbDebuggerType.Items.Objects[idx]);

  if idx = -1 then Exit;
  Result := DebugBoss.Debuggers[idx];
end;

procedure TDebuggerGeneralOptionsFrame.SetDebuggerClass(
  const AClass: TDebuggerClass);
begin
  if FCurDebuggerClass = AClass then Exit;
  FCurDebuggerClass := AClass;
  FetchDebuggerSpecificOptions;
end;

procedure TDebuggerGeneralOptionsFrame.ClearDbgProperties;
var
  i: Integer;
begin
  PropertyGrid.Selection.Clear;
  for i := 0 to FCurrentDebPropertiesList.Count - 1 do
    FCurrentDebPropertiesList.Objects[i].Free;
  FCurrentDebPropertiesList.Clear;
end;

procedure TDebuggerGeneralOptionsFrame.HookGetCheckboxForBoolean(var Value: Boolean);
begin
  Value := EnvironmentOptions.ObjectInspectorOptions.CheckboxForBoolean;
end;

constructor TDebuggerGeneralOptionsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // create the PropertyEditorHook (the interface to the properties)
  FPropertyEditorHook:=TPropertyEditorHook.Create(Self);
  FPropertyEditorHook.AddHandlerGetCheckboxForBoolean(@HookGetCheckboxForBoolean);

  FCurrentDebPropertiesList := TStringList.Create;
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

destructor TDebuggerGeneralOptionsFrame.Destroy;
begin
  ClearDbgProperties;
  PropertyGrid.Selection.Clear;
  FreeAndNil(FPropertyEditorHook);
  FreeAndNil(FCurrentDebPropertiesList);
  inherited Destroy;
end;

function TDebuggerGeneralOptionsFrame.Check: Boolean;
begin
  Result := false;

  if assigned(FCurDebuggerClass) and FCurDebuggerClass.NeedsExePath
  and (EnvironmentOptions.DebuggerFilename <> cmbDebuggerPath.Text)
  then begin
    EnvironmentOptions.DebuggerFilename := cmbDebuggerPath.Text;
    if not CheckExecutable(FOldDebuggerFilename,
          EnvironmentOptions.GetParsedDebuggerFilename,
          lisEnvOptDlgInvalidDebuggerFilename,
          lisEnvOptDlgInvalidDebuggerFilenameMsg)
    then exit;
  end;

  Result := true;
end;

function TDebuggerGeneralOptionsFrame.GetTitle: String;
begin
  Result := lisGeneral;
end;

procedure TDebuggerGeneralOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  gbDebuggerType.Caption := dlgDebugType;
  gbAdditionalSearchPath.Caption := lisDebugOptionsFrmAdditionalSearchPath;
  gcbDebuggerGeneralOptions.Caption := lisDebugOptionsFrmDebuggerGeneralOptions;
  gcbDebuggerGeneralOptions.Items.Add(lisDebugOptionsFrmShowMessageOnStop);
  gcbDebuggerGeneralOptions.Items.Add(lisDebugOptionsFrmResetDebuggerOnEachRun);
  gbDebuggerSpecific.Caption := lisDebugOptionsFrmDebuggerSpecific;
end;

procedure TDebuggerGeneralOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  ClearDbgProperties;
  with EnvironmentOptions do
  begin
    ObjectInspectorOptions.AssignTo(PropertyGrid);

    FOldDebuggerFilename := DebuggerFilename;
    fOldDebuggerSearchPath := DebuggerSearchPath;

    cmbDebuggerPath.Text := FOldDebuggerFilename;
    FetchDebuggerClass;
    FetchDebuggerGeneralOptions;
  end;
end;

procedure TDebuggerGeneralOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  i: Integer;
begin
  with EnvironmentOptions do
  begin
    DebuggerFilename := cmbDebuggerPath.Text;
    DebuggerFileHistory.Assign(cmbDebuggerPath.Items);
    DebuggerSearchPath := TrimSearchPath(txtAdditionalPath.Text,'');
    // IMPORTANT if more items are added the indexes must be updated here!
    DebuggerShowStopMessage := gcbDebuggerGeneralOptions.Checked[0];
    DebuggerResetAfterRun := gcbDebuggerGeneralOptions.Checked[1];

    for i := 0 to FCurrentDebPropertiesList.Count - 1 do
      SaveDebuggerProperties(FCurrentDebPropertiesList[i],
                             TDebuggerProperties(FCurrentDebPropertiesList.Objects[i]));

    if FCurDebuggerClass = nil
    then DebuggerConfig.DebuggerClass := ''
    else DebuggerConfig.DebuggerClass := FCurDebuggerClass.ClassName;
  end;
end;

procedure TDebuggerGeneralOptionsFrame.RestoreSettings(
  AOptions: TAbstractIDEOptions);
begin
  with EnvironmentOptions do begin
    DebuggerFilename := FOldDebuggerFilename;
    DebuggerSearchPath := fOldDebuggerSearchPath;
  end;
end;

class function TDebuggerGeneralOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TDebuggerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupDebugger, TDebuggerGeneralOptionsFrame, DbgOptionsGeneral);
end.

