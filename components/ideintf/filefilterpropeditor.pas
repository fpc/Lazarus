unit FileFilterPropEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  // LCL
  Forms, ButtonPanel, Grids, Buttons,
  // IdeIntf
  ObjInspStrConsts, IDEImagesIntf, IDEWindowIntf;

type

  { TFileFilterPropEditForm }

  TFileFilterPropEditForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    MoveDownBtn: TSpeedButton;
    MoveUpBtn: TSpeedButton;
    StringGrid1: TStringGrid;
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure MoveUpBtnClick(Sender: TObject);
    procedure MoveDownBtnClick(Sender: TObject);
    procedure StringGrid1ButtonClick(Sender: TObject; {%H-}aCol, {%H-}aRow: Integer);
    procedure StringGrid1Click(Sender: TObject);
    procedure StringGrid1EditingDone(Sender: TObject);
  private
    function GetFilter: string;
    procedure SetFilter(const AValue: string);
    procedure UpdateEnabledStates;
  public
    property Filter:string read GetFilter write SetFilter;
  end;

var
  FileFilterPropEditForm: TFileFilterPropEditForm;

implementation

{$R *.lfm}

{ TFileFilterPropertyEditorForm }

procedure TFileFilterPropEditForm.FormCreate(Sender: TObject);
begin
  Caption:=peFilterEditor;
  StringGrid1.Cells[0, 0] := peFilterName;
  StringGrid1.Cells[1, 0] := peFilter;
  IDEImages.AssignImage(MoveUpBtn, 'arrow_up');
  IDEImages.AssignImage(MoveDownBtn, 'arrow_down');
  MoveUpBtn.Hint := rscdMoveUp;
  MoveDownBtn.Hint := rscdMoveDown;
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TFileFilterPropEditForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TFileFilterPropEditForm.MoveUpBtnClick(Sender: TObject);
begin
  with StringGrid1 do
    MoveColRow(False, Row, Row-1);
  UpdateEnabledStates;
end;

procedure TFileFilterPropEditForm.MoveDownBtnClick(Sender: TObject);
begin
  with StringGrid1 do
    MoveColRow(False, Row, Row+1);
  UpdateEnabledStates;
end;

procedure TFileFilterPropEditForm.StringGrid1ButtonClick(Sender: TObject; aCol,
  aRow: Integer);
begin
  UpdateEnabledStates;
end;

procedure TFileFilterPropEditForm.StringGrid1Click(Sender: TObject);
begin
  UpdateEnabledStates;
end;

procedure TFileFilterPropEditForm.StringGrid1EditingDone(Sender: TObject);
begin
  UpdateEnabledStates;
end;

function TFileFilterPropEditForm.GetFilter: string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to StringGrid1.RowCount-1 do
  begin
    if StringGrid1.Cells[1,i] <> '' then
    begin
      if Result <> '' then
        Result := Result + '|';
      if StringGrid1.Cells[0,i] <> '' then
        Result := Result+StringGrid1.Cells[0,i]+'|'+StringGrid1.Cells[1,i]
      else
        Result := Result+StringGrid1.Cells[1,i]+'|'+StringGrid1.Cells[1,i];
    end
    else
      break;
  end;
end;

procedure TFileFilterPropEditForm.SetFilter(const AValue: string);
var
  S: string;
  C1, i: integer;
begin
  S := AValue;
  I := 1;
  while (S <> '') do
  begin
    C1 := Pos('|',S);
    if C1 > 0 then
    begin
      StringGrid1.Cells[0,i] := Copy(S, 1, C1-1);
      Delete(S, 1, C1);
      C1 := Pos('|',S);
      if (C1 > 0) then
      begin
        StringGrid1.Cells[1,i] := Copy(S, 1, C1-1);
        Delete(S, 1, C1);
      end
      else
      begin
        StringGrid1.Cells[1,i] := S;
        S := '';
      end;
    end
    else
    begin
      StringGrid1.Cells[0,i] := S;
      StringGrid1.Cells[1,i] := S;
      S := '';
    end;
    inc(i);
  end;
  UpdateEnabledStates;
end;

procedure TFileFilterPropEditForm.UpdateEnabledStates;
begin
  MoveUpBtn.Enabled := StringGrid1.Row > StringGrid1.FixedRows;
  MoveDownBtn.Enabled := True;
end;


end.

