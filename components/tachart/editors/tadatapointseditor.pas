{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Alexander Klenin

}
unit TADataPointsEditor;

{$H+}

interface

uses
  ButtonPanel, Classes, ExtCtrls, Grids, Menus, SysUtils, Forms, Controls,
  Graphics, Dialogs;

type

  { TDataPointsEditorForm }

  TDataPointsEditorForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    cdItemColor: TColorDialog;
    miInsertRow: TMenuItem;
    miDeleteRow: TMenuItem;
    pmRows: TPopupMenu;
    sgData: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure miDeleteRowClick(Sender: TObject);
    procedure miInsertRowClick(Sender: TObject);
    procedure pmRowsPopup(Sender: TObject);
    procedure sgDataButtonClick(ASender: TObject; ACol, ARow: Integer);
    procedure sgDataDrawCell(
      ASender: TObject; ACol, ARow: Integer; ARect: TRect;
      AState: TGridDrawState);
    procedure sgDataPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  strict private
    FCurrentRow: Integer;
    FDataPoints: TStrings;
    FXCount: Integer;
    FYCount: Integer;
  public
    procedure InitData(AXCount, AYCount: Integer; ADataPoints: TStrings);
    procedure ExtractData(out AModified: Boolean);
  end;

procedure Register;

implementation

uses
  LCLIntf, LCLType, Math, PropEdits,
  TAChartStrConsts, TAChartUtils, TASources;

{$R *.lfm}

type
  TDataPointsPropertyEditor = class(TPropertyEditor)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: AnsiString; override;
  end;

procedure Register;
begin
  RegisterPropertyEditor(
    TypeInfo(TStrings), TListChartSource, 'DataPoints',
    TDataPointsPropertyEditor);
end;

{ TDataPointsEditorForm }

procedure TDataPointsEditorForm.ExtractData(out AModified: Boolean);
var
  i: Integer;
  s: String;
  oldDataPoints: String;
begin
  oldDataPoints := FDataPoints.Text;
  FDataPoints.BeginUpdate;
  try
    FDataPoints.Clear;
    for i := 1 to sgData.RowCount - 1 do begin
      with sgData.Rows[i] do begin
        Delimiter := '|';
        StrictDelimiter := true;
        s := DelimitedText;
      end;
      if Length(s) >= sgData.ColCount then
        FDataPoints.Add(Copy(s, 2, MaxInt));
    end;
  finally
    FDataPoints.EndUpdate;
    AModified := FDataPoints.Text <> oldDataPoints;
  end;
end;

procedure TDataPointsEditorForm.InitData(
  AXCount, AYCount: Integer; ADataPoints: TStrings);
var
  i: Integer;
  w: Integer;
begin
  FXCount := AXCount;
  FYCount := AYCount;
  FDataPoints := ADataPoints;
  sgData.RowCount := Max(ADataPoints.Count + 1, 2);
  {     wp: What is this good for?
  for i := sgData.Columns.Count - 1 downto 0 do
    with sgData.Columns[i].Title do
      if (Caption[1] = 'Y') and (Caption <> 'Y') then
        sgData.Columns.Delete(i);
    }
  if AXCount > 1 then
    sgData.Columns[0].Title.Caption := 'X1';
  if AYCount > 1 then
    sgData.Columns[1].Title.Caption := 'Y1';
  for i := 2 to AYCount do
    with sgData.Columns.Add do begin
      Assign(sgData.Columns[1]);
      Title.Caption := 'Y' + IntToStr(i);
      Index := i;
    end;
  for i := 2 to AXCount do
    with sgData.Columns.Add do begin
      Assign(sgData.Columns[0]);
      Title.Caption := 'X' + IntToStr(i);
      Index := i - 1;
    end;
  for i := 0 to ADataPoints.Count - 1 do
    Split('|' + ADataPoints[i], sgData.Rows[i + 1]);

  // Adjust column widths
  w := sgData.Canvas.TextWidth('$000000') + 3*varCellPadding + sgData.DefaultRowHeight;
  for i := 0 to sgData.Columns.Count-1 do
    sgData.Columns[i].Width := w;

  Width := sgData.ColWidths[0] + sgData.Columns.Count * w + 2*sgData.Left +
    sgData.GridLineWidth * (sgData.Columns.Count-1);
end;

procedure TDataPointsEditorForm.miDeleteRowClick(Sender: TObject);
begin
  if sgData.RowCount <= 2 then begin
    sgData.Rows[1].Clear;
    exit;
  end;
  if InRange(FCurrentRow, 1, sgData.RowCount - 1) then
    sgData.DeleteRow(FCurrentRow);
end;

procedure TDataPointsEditorForm.FormCreate(Sender: TObject);
begin
  Caption := desDatapointEditor;
  sgData.Columns[2].Title.Caption := desColor;
  sgData.Columns[3].Title.Caption := desText;
  miInsertRow.Caption := desInsertRow;
  miDeleteRow.Caption := desDeleteRow;
end;

procedure TDataPointsEditorForm.miInsertRowClick(Sender: TObject);
begin
  sgData.InsertColRow(false, FCurrentRow);
end;

procedure TDataPointsEditorForm.pmRowsPopup(Sender: TObject);
begin
  FCurrentRow := sgData.MouseToCell(sgData.ScreenToClient(Mouse.CursorPos)).Y;
  if not InRange(FCurrentRow, 1, sgData.RowCount - 1) then
    Abort;
  sgData.Row := FCurrentRow;
end;

procedure TDataPointsEditorForm.sgDataButtonClick(
  ASender: TObject; ACol, ARow: Integer);
begin
  Unused(ASender);
  if (ARow < 1) or (ACol <> FXCount + FYCount + 1) then exit;
  cdItemColor.Color := StrToIntDef(sgData.Cells[ACol, ARow], clRed);
  if not cdItemColor.Execute then exit;
  sgData.Cells[ACol, ARow] := IntToColorHex(cdItemColor.Color);
end;

procedure TDataPointsEditorForm.sgDataDrawCell(
  ASender: TObject; ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  c: Integer;
begin
  Unused(ASender, AState);
  if (ARow < 1) or (ACol <> FXCount + FYCount + 1) then exit;
  if not TryStrToInt(sgData.Cells[ACol, ARow], c) then exit;
  sgData.Canvas.Pen.Color := clBlack;
  sgData.Canvas.Brush.Color := c;
  InflateRect(ARect, -varCellPadding, -varCellPadding);
  ARect.Left := ARect.Right - (ARect.Bottom - ARect.Top);;
  sgData.Canvas.Rectangle(ARect);
end;

procedure TDataPointsEditorForm.sgDataPrepareCanvas(sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  Unused(aRow, aState);
  if ACol = 0 then begin
    ts := TStringGrid(Sender).Canvas.TextStyle;
    ts.Alignment := taRightJustify;
    TStringGrid(Sender).Canvas.TextStyle := ts;
  end;
end;

{ TDataPointsPropertyEditor }

procedure TDataPointsPropertyEditor.Edit;
var
  dataModified: Boolean;
begin
  with TDataPointsEditorForm.Create(nil) do
    try
      InitData(
        (GetComponent(0) as TListChartsource).XCount,
        (GetComponent(0) as TListChartSource).YCount,
        GetObjectValue as TStrings
      );
      if ShowModal = mrOK then begin
        ExtractData(dataModified);
        if dataModified then Modified;
      end;
    finally
      Free;
    end;
end;

function TDataPointsPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paMultiSelect, paReadOnly, paRevertable];
end;

function TDataPointsPropertyEditor.GetValue: AnsiString;
begin
  Result := (GetObjectValue as TStrings).Text;
end;

end.

