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
  Graphics, Dialogs, TASources;

type

  TDataPointsEditorOption = (dpeHideColorColumn, dpeHideTextColumn);
  TDataPointsEditorOptions = set of TDataPointsEditorOption;

  { TDataPointsEditorForm }

  TDataPointsEditorForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    cdItemColor: TColorDialog;
    miMoveDown: TMenuItem;
    miMoveUp: TMenuItem;
    miSeparator: TMenuItem;
    miInsertRow: TMenuItem;
    miDeleteRow: TMenuItem;
    pmRows: TPopupMenu;
    sgData: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure miDeleteRowClick(Sender: TObject);
    procedure miInsertRowClick(Sender: TObject);
    procedure miMoveDownClick(Sender: TObject);
    procedure miMoveUpClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
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
    FOptions: TDataPointsEditorOptions;
    procedure UpdateCmds;
    function ValidData(out ACol, ARow: Integer; out AMsg: String): Boolean;
  public
    procedure InitData(AXCount, AYCount: Integer; ADataPoints: TStrings;
      AOptions: TDataPointsEditorOptions = []);
    procedure ExtractData(out AModified: Boolean);
    property Options: TDatapointsEditorOptions read FOptions;
  end;

function DataPointsEditor(AListChartSource: TListChartsource;
  AOptions: TDataPointsEditorOptions = []): Boolean;


implementation

uses
  LCLIntf, LCLType, Math, StdCtrls,
  TAChartStrConsts, TAChartUtils;

{$R *.lfm}

function DataPointsEditor(AListChartSource: TListChartsource;
  AOptions: TDataPointsEditorOptions = []): Boolean;
var
  F: TDataPointsEditorForm;
begin
  Result := false;
  F := TDataPointsEditorForm.Create(Application);
  try
    F.InitData(
      AListChartSource.XCount,
      AListChartSource.YCount,
      AListChartSource.DataPoints,
      AOptions
    );
    if F.ShowModal = mrOK then
      F.ExtractData(Result);
  finally
    F.Free;
  end;
end;

function EditText(var AText: String): Boolean;
var
  F: TForm;
  memo: TMemo;
begin
  F := TForm.CreateNew(Application);
  try
    F.Caption := 'Data point text';
    F.Position := poScreenCenter;
    memo := TMemo.Create(F);
    with memo do begin
      Parent := F;
      Align := alClient;
      BorderSpacing.Around := 6;
      Lines.Text := AText;
    end;
    with TButtonPanel.Create(F) do begin
      Parent := F;
      Align := alBottom;
      BorderSpacing.Around := 6;
      ShowButtons := [pbOK, pbCancel];
    end;
    Result := F.ShowModal = mrOK;
    if Result then AText := memo.Lines.Text;
  finally
    F.Free;
  end;
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

procedure TDataPointsEditorForm.InitData(AXCount, AYCount: Integer;
  ADataPoints: TStrings; AOptions: TDataPointsEditorOptions = []);
var
  i: Integer;
  w: Integer;
begin
  FXCount := AXCount;
  FYCount := AYCount;
  FDataPoints := ADataPoints;
  FOptions := AOptions;
  sgData.RowCount := Max(ADataPoints.Count + 1, 2);
  for i := 1 to AYCount do
    with sgData.Columns.Add do begin
      Assign(sgData.Columns[0]); // Columns[0] is a template column
      if AYCount = 1 then
        Title.Caption := 'Y'
      else
        Title.Caption := 'Y' + IntToStr(i);
      Index := i;
    end;
  for i := 1 to AXCount do
    with sgData.Columns.Add do begin
      Assign(sgData.Columns[0]); // Columns[0] is a template column
      if AXCount = 1 then
        Title.Caption := 'X'
      else
        Title.Caption := 'X' + IntToStr(i);
      Index := i;
    end;
  sgData.Columns.Delete(0); // remove the template column
  sgData.Columns[sgData.Columns.Count-2].Visible := not (dpeHideColorColumn in FOptions);
  sgData.Columns[sgData.Columns.Count-1].Visible := not (dpeHideTextColumn in FOptions);
  for i := 0 to ADataPoints.Count - 1 do
    Split('|' + ADataPoints[i], sgData.Rows[i + 1]);

  // Adjust column widths
  w := sgData.Canvas.TextWidth('$000000') + 3*varCellPadding + sgData.DefaultRowHeight;
  for i := 0 to sgData.Columns.Count-2 do
    sgData.Columns[i].Width := w;
  sgData.Columns[sgData.Columns.Count-1].Width := 3*w;

  w := sgData.ColWidths[0] + sgData.Left * 2;
  for i := 0 to sgData.Columns.Count-1 do
    inc(w, sgData.Columns[i].Width);
 {$IFDEF WINDOWS}
  Width := Min(Screen.Width, w + 1 + IfThen(sgData.BorderStyle = bsNone, 0, 3));
 {$ELSE}
  Width := Min(Screen.Width, w + sgData.GridLineWidth * (sgData.Columns.Count-1));
 {$ENDIF}
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
  sgData.Columns[1].Title.Caption := desColor;
  sgData.Columns[2].Title.Caption := desText;
  miInsertRow.Caption := desInsertRow;
  miDeleteRow.Caption := desDeleteRow;
  miMoveUp.Caption := desMoveUp;
  miMoveDown.Caption := desMoveDown;
  if IsRightToLeft then
    sgData.AutoAdvance := aaLeftDown
  else
    sgData.AutoAdvance := aaRightDown;
end;

procedure TDataPointsEditorForm.miInsertRowClick(Sender: TObject);
begin
  sgData.InsertColRow(false, FCurrentRow);
end;

procedure TDataPointsEditorForm.miMoveDownClick(Sender: TObject);
begin
  if sgData.Row < sgData.RowCount-1 then
    sgData.ExchangeColRow(false, sgData.Row, sgData.Row+1);
end;

procedure TDataPointsEditorForm.miMoveUpClick(Sender: TObject);
begin
  if sgData.Row > 1 then
    sgData.ExchangeColRow(false, sgData.Row, sgData.Row-1);
end;

procedure TDataPointsEditorForm.OKButtonClick(Sender: TObject);
var
  c, r: Integer;
  msg: String;
begin
  if not ValidData(c, r, msg) then begin
    sgData.Row := r;
    sgData.Col := c;
    sgData.SetFocus;
    MessageDlg(msg, mtError, [mbOK], 0);
    ModalResult := mrNone;
  end;
end;

procedure TDataPointsEditorForm.pmRowsPopup(Sender: TObject);
begin
  FCurrentRow := sgData.MouseToCell(sgData.ScreenToClient(Mouse.CursorPos)).Y;
  if not InRange(FCurrentRow, 1, sgData.RowCount - 1) then
    Abort;
  sgData.Row := FCurrentRow;
  UpdateCmds;
end;

procedure TDataPointsEditorForm.sgDataButtonClick(
  ASender: TObject; ACol, ARow: Integer);
var
  s: String;
begin
  Unused(ASender);
  if (ARow < 1) then exit;

  if (ACol = FXCount + FYCount + 1) then begin
    cdItemColor.Color := StrToIntDef(sgData.Cells[ACol, ARow], clRed);
    if cdItemColor.Execute then
      sgData.Cells[ACol, ARow] := IntToColorHex(cdItemColor.Color);
  end else
  if (ACol = FXCount + FYCount + 2) then begin
    s := sgData.Cells[ACol, ARow];
    if EditText(s) then sgData.Cells[ACol, ARow] := s;
  end;
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

procedure TDataPointsEditorForm.UpdateCmds;
begin
  miDeleteRow.Enabled := sgData.Row > 0;
  miMoveUp.Enabled := sgData.Row > 1;
  miMovedown.Enabled := sgData.Row < sgData.RowCount-1;
end;

function TDataPointsEditorForm.ValidData(out ACol, ARow: Integer;
  out AMsg: String): Boolean;
var
  x: Double;
  i: Integer;
  r, c: Integer;
  s: String;
begin
  Result := false;
  for r := 1 to sgData.RowCount-1 do begin
    for c := 1 to sgData.ColCount-3 do begin
      s := sgData.Cells[c, r];
      if (s <> '') and not TryStrToFloat(s, x) and not TryStrToFloat(s, x, DefSeparatorSettings) then
      begin
        ACol := c;
        ARow := r;
        AMsg := desNoNumber;
        exit;
      end;
    end;
    s := sgData.Cells[sgData.ColCount - 2, r];
    if (s <> '') and (s <> '?') and not TryStrToInt(s, i) then begin
      ACol := sgData.ColCount - 2;
      ARow := r;
      AMsg := desNoInteger;
      exit;
    end;
  end;
  Result := true;
end;

end.

