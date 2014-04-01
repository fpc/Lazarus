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
  Classes, SysUtils, LCLProc, Forms, Controls, Buttons, Dialogs, StdCtrls,
  TextTools, ObjInspStrConsts, ExtCtrls, ButtonPanel, ValEdit, Grids;
  
type

  { TKeyValPropEditorFrm }

  TKeyValPropEditorFrm = class(TForm)
    BtnPanel: TButtonPanel;
    ValueListEdit: TValueListEditor;
    StatusLabel: TLabel;
    SortButton: TButton;
    TextGroupBox: TGroupBox;
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
  // This does not work :(
  ValueListEdit.Options := ValueListEdit.Options + [goAutoAddRows];
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

