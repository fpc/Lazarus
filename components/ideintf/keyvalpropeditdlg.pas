{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Abstract:
    Dialog for the TStrings property editor for TValueListEditor.
}
unit KeyValPropEditDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  Forms, StdCtrls, ButtonPanel, ValEdit,
  // IdeIntf
  TextTools, ObjInspStrConsts, IDEWindowIntf;
  
type

  { TKeyValPropEditorFrm }

  TKeyValPropEditorFrm = class(TForm)
    BtnPanel: TButtonPanel;
    ValueListEdit: TValueListEditor;
    StatusLabel: TLabel;
    SortButton: TButton;
    TextGroupBox: TGroupBox;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SortButtonClick(Sender: TObject);
  public
  end;


implementation

{$R *.lfm}

{ TKeyValPropEditorFrm }

procedure TKeyValPropEditorFrm.FormCreate(Sender: TObject);
begin
  Caption := oisStringsEditorDialog;
  StatusLabel.Caption := ois0Lines0Chars;
  //SortButton.Caption := oisSort;
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TKeyValPropEditorFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TKeyValPropEditorFrm.SortButtonClick(Sender: TObject);
begin
  if not Assigned(ShowSortSelectionDialogFunc) then
  begin
    SortButton.Enabled := False;
    Exit;
  end;
  // ToDo: implement or remove the whole button.
end;

end.

