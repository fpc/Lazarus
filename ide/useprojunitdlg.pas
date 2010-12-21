unit UseProjUnitDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLProc, Forms, Controls, ComCtrls, StdCtrls, ExtCtrls, Buttons,
  ButtonPanel, Dialogs,
  SrcEditorIntf, LazIDEIntf, IDEImagesIntf, LazarusIDEStrConsts,
  ProjectIntf, Project, CodeCache, CodeToolManager;

type

  { TUseProjUnitDialog }

  TUseProjUnitDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    UnitsListBox: TListBox;
    SectionRadioGroup: TRadioGroup;
  private
    procedure AddItems(AItems: TStrings);
    function SelectFirst: string;
    function SelectedUnit: string;
    function InterfaceSelected: Boolean;
  public

  end; 

  function ShowUseProjUnitDialog: TModalResult;

implementation

{$R *.lfm}

function ShowUseProjUnitDialog: TModalResult;
var
  UseProjUnitDlg: TUseProjUnitDialog;
  SrcEdit: TSourceEditorInterface;
  Code: TCodeBuffer;
  CurFile: TUnitInfo;
  MainUsedUnits, ImplUsedUnits: TStrings;
  AvailUnits: TStringList;
  s: String;
  IsIntf, CTRes: Boolean;
begin
  Result:=mrOk;
  if not LazarusIDE.BeginCodeTools then exit;
  // get cursor position
  SrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  if SrcEdit=nil then exit;
  Code:=TCodeBuffer(SrcEdit.CodeToolsBuffer);
  if Code=nil then exit;
  UseProjUnitDlg:=nil;
  MainUsedUnits:=nil;
  ImplUsedUnits:=nil;
  AvailUnits:=TStringList.Create;
  try
    if not CodeToolBoss.FindUsedUnitNames(Code,MainUsedUnits,ImplUsedUnits) then begin
      DebugLn(['ShowUseProjUnitDialog CodeToolBoss.FindUsedUnitNames failed']);
      LazarusIDE.DoJumpToCodeToolBossError;
      exit(mrCancel);
    end;
    TStringList(MainUsedUnits).CaseSensitive:=False;
    TStringList(ImplUsedUnits).CaseSensitive:=False;
    // Create dialog and add available unit names there.
    UseProjUnitDlg:=TUseProjUnitDialog.Create(nil);
    CurFile:=Project1.FirstPartOfProject;
    while CurFile<>nil do begin
      s:=CurFile.Unit_Name;
      if (MainUsedUnits.IndexOf(s)<0) and (ImplUsedUnits.IndexOf(s)<0) then
        AvailUnits.Add(s);
      CurFile:=CurFile.NextPartOfProject;
    end;
    // Show the dialog.
    if AvailUnits.Count>0 then begin
      AvailUnits.Sorted:=True;
      UseProjUnitDlg.AddItems(AvailUnits);
      UseProjUnitDlg.SelectFirst;
      if UseProjUnitDlg.ShowModal=mrOk then begin
        s:=UseProjUnitDlg.SelectedUnit;
        IsIntf:=UseProjUnitDlg.InterfaceSelected;
        if s<>'' then begin
          CTRes:=True;
          if IsIntf then
            CTRes:=CodeToolBoss.AddUnitToMainUsesSection(Code, s, '')
          else
            CTRes:=CodeToolBoss.AddUnitToImplementationUsesSection(Code, s, '');
          if not CTRes then begin
            LazarusIDE.DoJumpToCodeToolBossError;
            exit(mrCancel);
          end;
        end;
      end;
    end
    else
      ShowMessage('No unused items are available in this project.');
  finally
    CodeToolBoss.SourceCache.ClearAllSourceLogEntries;
    UseProjUnitDlg.Free;
    ImplUsedUnits.Free;
    MainUsedUnits.Free;
    AvailUnits.Free;
  end;
end;

{ TUseProjUnitDialog }

procedure TUseProjUnitDialog.AddItems(AItems: TStrings);
begin
  UnitsListBox.Items.Assign(AItems);
end;

function TUseProjUnitDialog.SelectFirst: string;
begin
  UnitsListBox.Selected[0]:=True;
end;

function TUseProjUnitDialog.SelectedUnit: string;
begin
  Result:=UnitsListBox.Items[UnitsListBox.ItemIndex];
end;

function TUseProjUnitDialog.InterfaceSelected: Boolean;
begin
  Result:=SectionRadioGroup.ItemIndex=0;
end;


end.

