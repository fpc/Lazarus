{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit LazStringGridEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, Dialogs, StdCtrls, Buttons, Grids, ExtCtrls,
  // IdeIntf
  ObjInspStrConsts, IDEWindowIntf;

type

  { TStringGridEditorDlg }

  TStringGridEditorDlg = class(TForm)
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnApply: TBitBtn;
    BtnHelp: TBitBtn;
    BtnLoad: TButton;
    BtnSave: TButton;
    BtnClean: TButton;
    GroupBox: TGroupBox;
    Images: TImageList;
    OpenDialog: TOpenDialog;
    BtnPanel: TPanel;
    LoadSavePanel: TPanel;
    PanelEdit: TPanel;
    SaveDialog: TSaveDialog;
    BtnAddRow: TSpeedButton;
    BtnDelRow: TSpeedButton;
    BtnRowUp: TSpeedButton;
    BtnRowDown: TSpeedButton;
    BtnAddCol: TSpeedButton;
    BtnDelCol: TSpeedButton;
    BtnColLeft: TSpeedButton;
    BtnColRight: TSpeedButton;
    StringGrid: TStringGrid;
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnCleanClick(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure StringGridPrepareCanvas({%H-}sender: TObject; Col, Row: Integer;
      {%H-}aState: TGridDrawState);
    procedure ManageGrid(Sender:TObject);
    procedure SwapRowCol(Sender:TObject);
  private
    FModified: Boolean;
    FStringGrid: TStringGrid;
  public
    property Modified: Boolean read FModified;
    procedure LoadFromGrid(AStringGrid: TStringGrid);
    procedure SaveToGrid;
  end;

implementation

{$R *.lfm}

procedure AssignGrid(Dest, Src: TStringGrid; Full: Boolean);
var
  I, J: Integer;
begin
  Dest.BeginUpdate;
  try
    if Full then
    begin
      Dest.Clear;
      Dest.ColCount := Src.ColCount;
      Dest.RowCount := Src.RowCount;
    end;

    for I := 0 to Src.RowCount - 1 do
      Dest.RowHeights[I] := Src.RowHeights[I];

    for I := 0 to Src.ColCount - 1 do
      Dest.ColWidths[I] := Src.ColWidths[I];

    for I := 0 to Src.ColCount - 1 do
      for J := 0 to Src.RowCount - 1 do
        Dest.Cells[I, J] := Src.Cells[I, J];
  finally
    Dest.EndUpdate;
  end;
end;


{ TStringGridEditorDlg }

procedure TStringGridEditorDlg.FormCreate(Sender: TObject);
begin
  Caption := sccsSGEdtCaption;

  GroupBox.Caption := sccsSGEdtGrp;
  BtnClean.Caption := sccsSGEdtClean;
  BtnApply.Caption := sccsSGEdtApply;
  BtnLoad.Caption := sccsSGEdtLoad;
  BtnSave.Caption := sccsSGEdtSave;

  BtnHelp.Caption:=cActionListEditorHelpCategory;
  BtnCancel.Caption:=oiStdActDataSetCancel1Hint;
  BtnOK.Caption:=oisOk2;

  OpenDialog.Title := sccsSGEdtOpenDialog;
  SaveDialog.Title := sccsSGEdtSaveDialog;

  BtnRowUp.Hint := sccsSGEdtMoveRowsCols;
  BtnRowDown.Hint := sccsSGEdtMoveRowsCols;
  BtnColLeft.Hint := sccsSGEdtMoveRowsCols;
  BtnColRight.Hint := sccsSGEdtMoveRowsCols;
  BtnAddRow.Hint := sccsSGEdtInsRow;
  BtnAddCol.Hint := sccsSGEdtInsCol;
  BtnDelRow.Hint := sccsSGEdtDelRow;
  BtnDelCol.Hint := sccsSGEdtDelCol;

  StringGrid.ExtendedColSizing := True;
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TStringGridEditorDlg.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TStringGridEditorDlg.BtnApplyClick(Sender: TObject);
begin
  SaveToGrid;
end;

procedure TStringGridEditorDlg.BtnCleanClick(Sender: TObject);
begin
  StringGrid.Clean;
end;

procedure TStringGridEditorDlg.BtnLoadClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    StringGrid.SaveOptions := [soDesign, soContent];
    StringGrid.LoadFromFile(OpenDialog.FileName);
  end;
end;

procedure TStringGridEditorDlg.BtnSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    StringGrid.SaveOptions := [soDesign, soContent];
    StringGrid.SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TStringGridEditorDlg.FormActivate(Sender: TObject);
var
  delta: Integer;
begin
  delta := Groupbox.BorderSpacing.Around;  // Assuming all borderspacings are equal
  Constraints.MinWidth := BtnColRight.Left + BtnColRight.Width + 
    (BtnClean.Left + BtnClean.Width - BtnLoad.Left) + 6*delta;
  if Width < Constraints.MinWidth then
    Width := 0;  // Enforce constraints
end;

procedure TStringGridEditorDlg.StringGridPrepareCanvas(sender: TObject; Col,
  Row: Integer; aState: TGridDrawState);
begin
  if (Col < FStringGrid.FixedCols) or (Row < FStringGrid.FixedRows) then
    StringGrid.Canvas.Brush.Color := FStringGrid.FixedColor;
end;

procedure TStringGridEditorDlg.ManageGrid(Sender:TObject);
var 
  irow, icol: integer;
begin
  irow := StringGrid.Row;
  icol := StringGrid.Col;
  if (Sender = BtnAddRow) then
  begin
    StringGrid.InsertColRow(false, irow);
    StringGrid.Row := StringGrid.Row-1;
  end 
  else
  if (Sender = BtnDelRow) and (irow >= 0) then
  begin
    if MessageDlg(Application.Title, Format(sccsSGEdtDelRowNo, [irow]), mtConfirmation, mbYesNo, 0) = mrYes then
      StringGrid.DeleteRow(irow);
  end else
  if Sender = BtnAddCol then
  begin
    StringGrid.InsertColRow(true, icol);
    StringGrid.Col := StringGrid.Col - 1;
  end
  else
  if (Sender = BtnDelCol) and (icol >= 0) then
    if MessageDlg(Application.Title, Format(sccsSGEdtDelColNo, [icol]), mtConfirmation, mbYesNo, 0) = mrYes then
      StringGrid.DeleteCol(icol);
end;

procedure TStringGridEditorDlg.SwapRowCol(Sender:TObject);
begin
  if Sender = BtnColLeft then begin
    if StringGrid.Col > 0 then
      StringGrid.ExchangeColRow(true,StringGrid.Col,StringGrid.Col-1);
      //StringGrid.Col := StringGrid.Col - 1;
  end else
  if Sender = BtnRowUp then begin
    if StringGrid.Row > 0 then
      StringGrid.ExchangeColRow(false,StringGrid.Row,StringGrid.Row-1);
      //StringGrid.Row := StringGrid.Row - 1;
  end;
  if Sender = BtnColRight then begin
    if StringGrid.Col < StringGrid.ColCount-1 then
      StringGrid.ExchangeColRow(true,StringGrid.Col,StringGrid.Col+1);
      //StringGrid.Col := StringGrid.Col + 1;
  end;
  if Sender = BtnRowDown then begin
    if StringGrid.Row < StringGrid.RowCount-1 then
      StringGrid.ExchangeColRow(false,StringGrid.Row,StringGrid.Row+1);
      //StringGrid.Row := StringGrid.Row + 1;
  end;
end;

procedure TStringGridEditorDlg.LoadFromGrid(AStringGrid: TStringGrid);
begin
  if Assigned(AStringGrid) then
  begin
    FStringGrid := AStringGrid;

    AssignGrid(StringGrid, AStringGrid, True);
    FModified := False;
  end;
end;

procedure TStringGridEditorDlg.SaveToGrid;
begin
  if Assigned(FStringGrid) then
  begin
    AssignGrid(FStringGrid, StringGrid, true);
    FModified := True;
  end;
end;

end.

