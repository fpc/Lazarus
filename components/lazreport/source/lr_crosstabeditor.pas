{ LazReport cross-tab control

  Copyright (C) 2014 alexs alexs75.at.yandex.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit lr_CrossTabEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ButtonPanel, ExtCtrls, Menus, DB, LR_Class, lr_CrossTab;

type
  TCrossCellType = (cctData, cctColHdr, cctRowHdr, cctColTotal,
    cctRowTotal, cctGrandTotal, cctCorner1, cctCorner2);

  TCrossColorStyle = record
    DataCell:TColor;
    DataCellAlt:TColor;
    RowTitleCell:TColor;
    RowTotalCell:TColor;
    ColTitleCell:TColor;
    ColTotalCell:TColor;
    GrandTotalCell:TColor;
    TotalCHCell:TColor;
    TotalRHCell:TColor;
  end;

  { TlrCrossTabEditorForm }

  TlrCrossTabEditorForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckGroup1: TCheckGroup;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    ListBox3: TListBox;
    ListBox4: TListBox;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PaintBox1: TPaintBox;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure ComboBox1Change(Sender: TObject);
    procedure ListBox2DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ListBox2DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ListBox3MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    FCross:TlrCrossView;
    FCurStyle:TCrossColorStyle;
    procedure FillDataSets;
    procedure Localize;
    //preview paint
    procedure ShowBackGround;
    procedure ShowFrame(AView:TfrStretcheable; ARect:TRect; AFillColor:TColor);

    function IsCellShow(ACellType:TCrossCellType):boolean;
    procedure UpdateStilesPopup;
  public
    constructor CreateEditForm(lrObj: TfrView);
    procedure SaveData;
  end;


function lrCrossTabEditor(lrObj: TfrView) : boolean;
implementation
uses LR_Utils, LR_DBRel, PropEdits, strutils;

const
  CT2Ind : array [TCrossCellType] of integer = (
    0, //cctData
    0, //cctColHdr,
    3, //cctRowHdr,
    1, //cctColTotal,
    4, //cctRowTotal
    6, //cctGrandTotal
    2, //cctCorner1,
    7 //cctCorner2
    );

  CountStyles = 8;
  CrossStyles : array [0..CountStyles - 1] of TCrossColorStyle =
    (
    //White
    (DataCell:clNone;
     DataCellAlt:clNone;
     RowTitleCell:clNone;  RowTotalCell:clNone;
     ColTitleCell:clNone; ColTotalCell:clNone; GrandTotalCell:clNone;
     TotalCHCell:clNone; TotalRHCell:clNone),

    //Gray
    (DataCell:clInfoBk;
     DataCellAlt:clNone;
     RowTitleCell:clGray;  RowTotalCell:clWhite;
     ColTitleCell:clGray; ColTotalCell:clWhite; GrandTotalCell:clGray;
     TotalCHCell:clGray; TotalRHCell:clGray),

    //Orange
    (DataCell:$9BEBFF;
     DataCellAlt:clNone;
     RowTitleCell:$46DAFF;  RowTotalCell:$46DAFF;
     ColTitleCell:$46DAFF; ColTotalCell:$9BEBFF; GrandTotalCell:$9BEBFF;
     TotalCHCell:$46DAFF; TotalRHCell:$46DAFF),

    //Green
    (DataCell:$00D29E;
     DataCellAlt:clNone;
     RowTitleCell:$00A47B;  RowTotalCell:$00D29E;
     ColTitleCell:$00A47B; ColTotalCell:$00D29E; GrandTotalCell:$00D29E;
     TotalCHCell:$00A47B; TotalRHCell:$00A47B),

    //Green and Orange
    (DataCell:$9BEBFF;
     DataCellAlt:clNone;
     RowTitleCell:$00A47B;  RowTotalCell:$9BEBFF;
     ColTitleCell:$00A47B; ColTotalCell:$9BEBFF; GrandTotalCell:$9BEBFF;
     TotalCHCell:$00A47B; TotalRHCell:$00A47B),

    //Blue
    (DataCell:$FED3BA;
     DataCellAlt:clNone;
     RowTitleCell:$FDBD97;  RowTotalCell:$FED3BA;
     ColTitleCell:$FDBD97; ColTotalCell:$FED3BA; GrandTotalCell:$FED3BA;
     TotalCHCell:$FDBD97; TotalRHCell:$FDBD97),

    //Blue and White
    (DataCell:clWhite;
     DataCellAlt:$FDBD97;
     RowTitleCell:$FDBD97;  RowTotalCell:clWhite;
     ColTitleCell:$FDBD97; ColTotalCell:clWhite; GrandTotalCell:clWhite;
     TotalCHCell:$FDBD97; TotalRHCell:$FDBD97),

    //Сyan
    (DataCell:$E6E6E6;
     DataCellAlt:clMoneyGreen;
     RowTitleCell:$4A4A00;  RowTotalCell:$8A8A19;
     ColTitleCell:$4A4A00; ColTotalCell:$8A8A19; GrandTotalCell:$8A8A19;
     TotalCHCell:$4A4A00; TotalRHCell:$4A4A00)

    );

function lrCrossTabEditor(lrObj: TfrView): boolean;
var
  lrCrossTabEditorForm: TlrCrossTabEditorForm;
begin
  Result:=false;
  lrCrossTabEditorForm:=TlrCrossTabEditorForm.CreateEditForm(lrObj);
  if lrCrossTabEditorForm.ShowModal = mrOk then
  begin
    lrCrossTabEditorForm.SaveData;
    Result:=true;
  end;
  lrCrossTabEditorForm.Free;
end;

{$R *.lfm}

type
  { TlrCrossViewDataSetProperty }

  TlrCrossViewDataSetProperty = class(TStringProperty)
  private
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

{ TlrCrossViewDataSetProperty }

function TlrCrossViewDataSetProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paValueList, paSortList];
end;

procedure TlrCrossViewDataSetProperty.Edit;
begin
  if (GetComponent(0) is TlrCrossView) then
    lrCrossTabEditor(TlrCrossView(GetComponent(0)))
end;

procedure TlrCrossViewDataSetProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
var
  Lst : TStringList;
begin
  Lst := TStringList.Create;
  try
    if Curreport.DataType = dtDataSet then
      frGetComponents(CurReport.Owner, TDataSet, Lst, nil)
    else
      frGetComponents(CurReport.Owner, TDataSource, Lst, nil);

    for i:=0 to Lst.Count-1 do
      Proc(Lst[i]);
  finally
    Lst.Free;
  end;
end;

  { TlrCrossTabEditorForm }

procedure TlrCrossTabEditorForm.ComboBox1Change(Sender: TObject);
var
  DataSet: TfrTDataSet;
begin
  ListBox1.Items.Clear;
  if ComboBox1.Items.Count>0 then
  begin
    DataSet := nil;
    DataSet := frGetDataSet(ComboBox1.Items[ComboBox1.ItemIndex]);
    if Assigned(DataSet) then
    begin
      try
        frGetFieldNames(DataSet, ListBox1.Items);
      except
      end;
    end;
  end;
end;

procedure TlrCrossTabEditorForm.CheckGroup1ItemClick(Sender: TObject;
  Index: integer);
begin
  PaintBox1.Refresh;
end;

procedure TlrCrossTabEditorForm.ListBox2DragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  LB2: TListBox;
  LB1: TListBox;
begin
  if (Source is TListBox) and (Sender <> Source) then
  begin
    LB1:=TListBox(Source);
    LB2:=TListBox(Sender);
    if (LB1.ItemIndex>-1) and (LB1.Items.Count>LB1.ItemIndex) then
    begin
      if (LB2 <> ListBox1) then
      begin
        if LB2 = ListBox3 then
          LB2.Items.Add(LB1.Items[LB1.ItemIndex]+'|SUM')
        else
          LB2.Items.Add(LB1.Items[LB1.ItemIndex]);
      end;
      if  (LB1<>ListBox1) then
        LB1.Items.Delete(LB1.ItemIndex);
    end;
  end;
end;

procedure TlrCrossTabEditorForm.ListBox2DragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Source is TListBox then
    Accept := (Sender <> Source) and
      (
          (TListBox(Sender) = ListBox1)
        or
          ((TListBox(Sender) = ListBox2) and (ListBox2.Items.Count<1)) //temp fix - not create cross-tab for multiple fields
        or
          ((TListBox(Sender) = ListBox3) and (ListBox3.Items.Count<1)) //temp fix - not create cross-tab for multiple fields
        or
          ((TListBox(Sender) = ListBox4) and (ListBox4.Items.Count<1)) //temp fix - not create cross-tab for multiple fields
      )
end;

procedure TlrCrossTabEditorForm.ListBox3MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if X > ListBox3.Width - ListBox3.Width div 4 then
  begin
    P:=ListBox3.ClientToScreen(Point(X, Y));
    PopupMenu2.PopUp(P.X, P.Y);
  end;
end;

procedure TlrCrossTabEditorForm.MenuItem1Click(Sender: TObject);
var
  T:integer;
begin
  T:=TMenuItem(Sender).Tag;
  if (T>=0) and (T<CountStyles) then
  begin
    FCurStyle:=CrossStyles[t];
    PaintBox1.Invalidate;
  end;
  UpdateStilesPopup;
end;

procedure TlrCrossTabEditorForm.MenuItem9Click(Sender: TObject);
begin
  if ListBox3.Items.Count>0 then
    ListBox3.Items[0]:=Copy2Symb(ListBox3.Items[0], '|') + '|' + CrossFuncList[TMenuItem(Sender).Tag];
end;

procedure TlrCrossTabEditorForm.PaintBox1Paint(Sender: TObject);
var
  X, Y:integer;
  YY, DY: Integer;
  XX, DX: Integer;
begin
  ShowBackGround;
  ShowFrame(FCross, Rect(0, 0, PaintBox1.Width - 4, PaintBox1.Height -4), FCross.FillColor);
  DX:=90;
  DY:=Canvas.TextHeight('Wg');

  X:=10;
  Y:=SpeedButton2.Top + SpeedButton2.Height + 20;

  if IsCellShow(cctCorner2) then
    ShowFrame(FCross.TotalRHCell, Rect(X, Y, X + DX, Y + DY), FCurStyle.TotalRHCell);

  if IsCellShow(cctRowHdr) then
  begin
    if IsCellShow(cctColHdr) then
      YY:=Y + DY
    else
      YY:=Y;
    ShowFrame(FCross.RowTitleCell, Rect(X, YY, X + DX, YY + DY), FCurStyle.RowTitleCell);
    X := X + DX;
  end;

  if IsCellShow(cctCorner1) then
  begin
    if IsCellShow(cctRowHdr) then
      XX:=X + DX
    else
      XX:= X;
    ShowFrame(FCross.TotalCHCell, Rect(XX, Y, XX + DX, Y + DY), FCurStyle.TotalCHCell);
  end;

  if IsCellShow(cctColHdr) or IsCellShow(cctCorner1) then
  begin
    if IsCellShow(cctColHdr) then
      ShowFrame(FCross.ColTitleCell, Rect(X, Y, X + DX, Y + DY), FCurStyle.ColTitleCell);
    Y:=Y + DY;
  end;


  ShowFrame(FCross.DataCell, Rect(X, Y, X + DX, Y + DY), FCurStyle.DataCell);

  if IsCellShow(cctRowTotal) then
  begin
    if IsCellShow(cctRowHdr) then
      XX:=X + DX
    else
      XX:=X;
    ShowFrame(FCross.RowTotalCell, Rect(XX, Y, XX + DX, Y + DY), FCurStyle.RowTotalCell);
  end;

  Y:=Y + DY;

  if IsCellShow(cctColTotal) then
  begin
    ShowFrame(FCross.ColTotalCell, Rect(X, Y, X + DX, Y + DY), FCurStyle.ColTotalCell);
    X:=X + DX;
  end;

  if IsCellShow(cctGrandTotal) then
    ShowFrame(FCross.GrandTotalCell, Rect(X, Y, X + DX, Y + DY), FCurStyle.GrandTotalCell);
end;

procedure TlrCrossTabEditorForm.SpeedButton1Click(Sender: TObject);
var
  S:string;
begin
  S:=ListBox2.Items.Text;
  ListBox2.Items.Text:=ListBox4.Items.Text;
  ListBox4.Items.Text:=S;
end;

procedure TlrCrossTabEditorForm.SpeedButton2Click(Sender: TObject);
var
  R: types.TPoint;
begin
  R:=GroupBox3.ClientToScreen(Point(SpeedButton2.Left, SpeedButton2.Top + SpeedButton2.Height));
  PopupMenu1.PopUp(R.X, R.Y);
end;

procedure TlrCrossTabEditorForm.FillDataSets;
var
  Lst : TStringList;
begin
  Lst := TStringList.Create;
  try
    if Curreport.DataType = dtDataSet then
      frGetComponents(CurReport.Owner, TDataSet, Lst, nil)
    else
      frGetComponents(CurReport.Owner, TDataSource, Lst, nil);
    Lst.Sort;
    ComboBox1.Items.Assign(Lst);
    ComboBox1.Enabled:=(Lst.Count>0);
  finally
    Lst.Free;
  end;
end;

procedure TlrCrossTabEditorForm.Localize;
begin
  Caption:='Cross tab editor';
end;

procedure TlrCrossTabEditorForm.ShowBackGround;
var
  fp: TColor;
begin
  fp := FCross.FillColor;
  if (fp = clNone) then
    fp := clWhite;

  PaintBox1.Canvas.Brush.Bitmap := nil;
  PaintBox1.Canvas.Brush.Style := bsSolid;
  PaintBox1.Canvas.Brush.Color := fp;
  PaintBox1.Canvas.FillRect(Rect(0,0, Width, Height))
end;

procedure TlrCrossTabEditorForm.ShowFrame(AView: TfrStretcheable; ARect: TRect;
  AFillColor: TColor);
procedure Line1(x, y, x1, y1: Integer);
var
  i, w: Integer;
begin

  if PaintBox1.Canvas.Pen.Style = psSolid then
  begin
    if AView.FrameStyle<>frsDouble then
    begin
      PaintBox1.Canvas.MoveTo(x, y);
      PaintBox1.Canvas.LineTo(x1, y1);
    end
    else
    begin
      if x = x1 then
      begin
        PaintBox1.Canvas.MoveTo(x - Round(AView.FrameWidth), y);
        PaintBox1.Canvas.LineTo(x1 - Round(AView.FrameWidth), y1);
        PaintBox1.Canvas.Pen.Color := AView.FillColor;
        PaintBox1.Canvas.MoveTo(x, y);
        PaintBox1.Canvas.LineTo(x1, y1);
        PaintBox1.Canvas.Pen.Color := AView.FrameColor;
        PaintBox1.Canvas.MoveTo(x + Round(AView.FrameWidth), y);
        PaintBox1.Canvas.LineTo(x1 + Round(AView.FrameWidth), y1);
      end
      else
      begin
        PaintBox1.Canvas.MoveTo(x, y - Round(AView.FrameWidth));
        PaintBox1.Canvas.LineTo(x1, y1 - Round(AView.FrameWidth));
        PaintBox1.Canvas.Pen.Color := AView.FillColor;
        PaintBox1.Canvas.MoveTo(x, y);
        PaintBox1.Canvas.LineTo(x1, y1);
        PaintBox1.Canvas.Pen.Color := AView.FrameColor;
        PaintBox1.Canvas.MoveTo(x, y + Round(AView.FrameWidth));
        PaintBox1.Canvas.LineTo(x1, y1 + Round(AView.FrameWidth));
      end;
    end
  end
  else
  begin
    PaintBox1.Canvas.Brush.Color:=AView.FillColor;
    w := PaintBox1.Canvas.Pen.Width;
    PaintBox1.Canvas.Pen.Width := 1;
    if x = x1 then
    begin
      for i := 0 to w - 1 do
      begin
        PaintBox1.Canvas.MoveTo(x - w div 2 + i, y);
        PaintBox1.Canvas.LineTo(x - w div 2 + i, y1);
      end
    end
    else
    begin
      for i := 0 to w - 1 do
      begin
        PaintBox1.Canvas.MoveTo(x, y - w div 2 + i);
        PaintBox1.Canvas.LineTo(x1, y - w div 2 + i);
      end;
    end;
    PaintBox1.Canvas.Pen.Width := w;
  end;
end;

begin
  if AFillColor<>clNone then
  begin
    PaintBox1.Canvas.Brush.Bitmap := nil;
    PaintBox1.Canvas.Brush.Style := bsSolid;
    PaintBox1.Canvas.Brush.Color := AFillColor;
    PaintBox1.Canvas.FillRect(ARect)
  end;

  if AView.FrameStyle<>frsDouble then
    PaintBox1.Canvas.Pen.Style := TPenStyle(AView.FrameStyle);

  if (frbRight in FCross.Frames) then
    Line1(ARect.Right, ARect.Top, ARect.Right, ARect.Bottom);
  if (frbLeft   in FCross.Frames) then
    Line1(ARect.Left, ARect.Top, ARect.Left, ARect.Bottom);
  if (frbBottom in FCross.Frames) then
    Line1(ARect.Left, ARect.Bottom, ARect.Right, ARect.Bottom);
  if (frbTop  in FCross.Frames) then
    Line1(ARect.Left, ARect.Top, ARect.Right, ARect.Top);

  if AView.Memo.Count>0 then
    PaintBox1.Canvas.TextRect(ARect, ARect.Left + 2, ARect.Top, AView.Memo[0]);
end;

function TlrCrossTabEditorForm.IsCellShow(ACellType: TCrossCellType): boolean;
begin
  if ACellType = cctData then
    Result := true
  else
    Result:=CheckGroup1.Checked[CT2Ind[ACellType]];
end;

procedure TlrCrossTabEditorForm.UpdateStilesPopup;
var
  i: Integer;
begin
  for i:=0 to CountStyles-1 do
    PopupMenu1.Items[i].Checked :=
     (FCurStyle.DataCell = CrossStyles[i].DataCell) and
     (FCurStyle.RowTitleCell = CrossStyles[i].RowTitleCell) and
     (FCurStyle.RowTotalCell = CrossStyles[i].RowTotalCell) and
     (FCurStyle.ColTitleCell = CrossStyles[i].ColTitleCell) and
     (FCurStyle.ColTotalCell = CrossStyles[i].ColTotalCell) and
     (FCurStyle.GrandTotalCell = CrossStyles[i].GrandTotalCell) and
     (FCurStyle.TotalCHCell = CrossStyles[i].TotalCHCell) and
     (FCurStyle.TotalRHCell = CrossStyles[i].TotalRHCell);
end;

constructor TlrCrossTabEditorForm.CreateEditForm(lrObj: TfrView);
begin
  inherited Create(Application);
  Localize;
  FCross:=TlrCrossView(lrObj);
  FillDataSets;
  ComboBox1.Text:=FCross.DataSet;
  if ComboBox1.ItemIndex>-1 then
    ComboBox1Change(nil);

  ListBox2.Items.Assign(FCross.RowFields);
  ListBox3.Items.Assign(FCross.CellFields);
  ListBox4.Items.Assign(FCross.ColumnFields);

  CheckGroup1.Checked[0]:=FCross.ShowColumnHeader;
  CheckGroup1.Checked[1]:=FCross.ShowColumnTotal;
  CheckGroup1.Checked[2]:=FCross.ShowTotalCHCell;
  CheckGroup1.Checked[7]:=FCross.ShowTotalRHCell;
  CheckGroup1.Checked[3]:=FCross.ShowRowHeader;
  CheckGroup1.Checked[4]:=FCross.ShowRowTotal;
  CheckGroup1.Checked[5]:=FCross.ShowTitle;
  CheckGroup1.Checked[6]:=FCross.ShowGrandTotal;

  FCurStyle.DataCell:=FCross.DataCell.FillColor;
  FCurStyle.DataCellAlt:=FCross.DataCell.AlternativeColor;
  FCurStyle.RowTitleCell:=FCross.RowTitleCell.FillColor;
  FCurStyle.RowTotalCell:=FCross.RowTotalCell.FillColor;
  FCurStyle.ColTitleCell:=FCross.ColTitleCell.FillColor;
  FCurStyle.ColTotalCell:=FCross.ColTotalCell.FillColor;
  FCurStyle.GrandTotalCell:=FCross.GrandTotalCell.FillColor;
  FCurStyle.TotalCHCell:=FCross.TotalCHCell.FillColor;
  FCurStyle.TotalRHCell:=FCross.TotalRHCell.FillColor;

  UpdateStilesPopup;
end;

procedure TlrCrossTabEditorForm.SaveData;
begin
  FCross.DataSet:=ComboBox1.Text;
  FCross.RowFields.Assign(ListBox2.Items);
  FCross.CellFields.Assign(ListBox3.Items);
  FCross.ColumnFields.Assign(ListBox4.Items);

  FCross.ShowColumnHeader := CheckGroup1.Checked[0];
  FCross.ShowColumnTotal  := CheckGroup1.Checked[1];
  FCross.ShowTotalCHCell  := CheckGroup1.Checked[2];
  FCross.ShowTotalRHCell  := CheckGroup1.Checked[7];
  FCross.ShowRowHeader    := CheckGroup1.Checked[3];
  FCross.ShowRowTotal     := CheckGroup1.Checked[4];
  FCross.ShowTitle        := CheckGroup1.Checked[5];
  FCross.ShowGrandTotal   := CheckGroup1.Checked[6];

  FCross.DataCell.FillColor       := FCurStyle.DataCell;
  FCross.DataCell.AlternativeColor:= FCurStyle.DataCellAlt;
  FCross.RowTitleCell.FillColor   := FCurStyle.RowTitleCell;
  FCross.RowTotalCell.FillColor   := FCurStyle.RowTotalCell;
  FCross.ColTitleCell.FillColor   := FCurStyle.ColTitleCell;
  FCross.ColTotalCell.FillColor   := FCurStyle.ColTotalCell;
  FCross.GrandTotalCell.FillColor := FCurStyle.GrandTotalCell;
  FCross.TotalCHCell.FillColor    := FCurStyle.TotalCHCell;
  FCross.TotalRHCell.FillColor    := FCurStyle.TotalRHCell;
end;

initialization
  RegisterPropertyEditor(TypeInfo(String), TlrCrossView, 'DataSet', TlrCrossViewDataSetProperty);
end.

