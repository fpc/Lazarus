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
  Classes,
  // LCL
  Forms, Controls, Dialogs, Arrow, StdCtrls, Buttons, Grids, ExtCtrls,
  // IdeIntf
  ObjInspStrConsts, IDEWindowIntf;

type

  { TStringGridEditorDlg }

  TStringGridEditorDlg = class(TForm)
    ArrowLeft: TArrow;
    ArrowRight: TArrow;
    ArrowDown: TArrow;
    ArrowUp: TArrow;
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    BtnApply: TBitBtn;
    BtnHelp: TBitBtn;
    BtnLoad: TButton;
    BtnSave: TButton;
    BtnClean: TButton;
    GroupBox: TGroupBox;
    LabelMove: TLabel;
    OpenDialog: TOpenDialog;
    BtnPanel: TPanel;
    LoadSavePanel: TPanel;
    SaveDialog: TSaveDialog;
    StringGrid: TStringGrid;
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnCleanClick(Sender: TObject);
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure StringGridPrepareCanvas({%H-}sender: TObject; Col, Row: Integer;
      {%H-}aState: TGridDrawState);
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
  LabelMove.Caption := sccsSGEdtMoveRowsCols;

  BtnHelp.Caption:=cActionListEditorHelpCategory;
  BtnCancel.Caption:=oiStdActDataSetCancel1Hint;
  BtnOK.Caption:=oisOk2;

  OpenDialog.Title := sccsSGEdtOpenDialog;
  SaveDialog.Title := sccsSGEdtSaveDialog;

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
    StringGrid.LoadFromFile(OpenDialog.FileName);
  end;
end;

procedure TStringGridEditorDlg.BtnSaveClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    StringGrid.SaveToFile(SaveDialog.FileName);
  end;
end;

procedure TStringGridEditorDlg.StringGridPrepareCanvas(sender: TObject; Col,
  Row: Integer; aState: TGridDrawState);
begin
  if (Col < FStringGrid.FixedCols) or (Row < FStringGrid.FixedRows) then
    StringGrid.Canvas.Brush.Color := FStringGrid.FixedColor;
end;

procedure TStringGridEditorDlg.SwapRowCol(Sender:TObject);
begin
  if TObject(Sender)=ArrowLeft then begin
    try
      StringGrid.ExchangeColRow(true,StringGrid.Col,StringGrid.Col-1);
      StringGrid.Col:=StringGrid.Col-1;
    except

    end;
  end;
  if TObject(Sender)=ArrowUp then begin
    try
      StringGrid.ExchangeColRow(false,StringGrid.Row,StringGrid.Row-1);
      StringGrid.Row:=StringGrid.Row-1;
    except

    end;
  end;
  if TObject(Sender)=ArrowRight then begin
    try
      StringGrid.ExchangeColRow(true,StringGrid.Col,StringGrid.Col+1);
      StringGrid.Col:=StringGrid.Col+1;
    except

    end;
  end;
  if TObject(Sender)=ArrowDown then begin
    try
      StringGrid.ExchangeColRow(false,StringGrid.Row,StringGrid.Row+1);
      StringGrid.Row:=StringGrid.Row+1;
    except

    end;
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
    AssignGrid(FStringGrid, StringGrid, False);
    FModified := True;
  end;
end;

end.

