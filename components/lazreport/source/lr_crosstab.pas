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

unit lr_CrossTab;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_Class, Graphics, LR_DSet, lr_CrossArray, DB;

const
  CrossFuncCount = 6;
  CrossFuncList : array [0..CrossFuncCount - 1] of string =
     ('NONE', 'SUM', 'MIN', 'MAX', 'AVG', 'COUNT');
type
  TlrCrossObject = class(TComponent)

  end;

type
  { TlrCrossDesignView }

  TlrCrossDesignView = class(TfrCustomMemoView)
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy; override;
  published
    property Cursor;
    property DetailReport;
    property Font;
    property Alignment;
    property Layout;
    property Angle;
    property WordBreak;
    property WordWrap;
//    property AutoSize;
//    property HideDuplicates;
    property HideZeroValues;
    property FillColor;
    property Memo;
    property Script;
    property Frames;
    property FrameColor;
    property FrameStyle;
    property FrameWidth;
    property Format;
    property FormatStr;
//    property Restrictions;
    property OnClick;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

  { TlrCrossDesignDataView }

  TlrCrossDesignDataView = class(TlrCrossDesignView)
  private
    FAlternativeColor: TColor;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SavetoXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property AlternativeColor:TColor read FAlternativeColor write FAlternativeColor default clNone; //AlternativeFillColor
  end;

  { TlrCrossView }

  TlrCrossView = class(TfrStretcheable)
  private
    FExVarArray:TExVarArray;
    FRowTotalArray : TExRow;
    FColTotalArray : TExRow;
    FShowTotalCHCell: Boolean;
    FShowTotalRHCell: Boolean;
    FTotal : Variant;
    FData:TDataSet;

    FCellFields: TStrings;
    FColumnFields: TStrings;
    FRowFields: TStrings;
    FDataSetName: string;
    FShowColumnHeader: Boolean;
    FShowColumnTotal: Boolean;
    FShowCorner: Boolean;
    FShowGrandTotal: Boolean;
    FShowRowHeader: Boolean;
    FShowRowTotal: Boolean;
    FShowTitle: Boolean;
    TextHeight: Integer;
    LineSpacing: Integer;

    FDataCell:TlrCrossDesignDataView;
    FRowTitleCell:TlrCrossDesignView;
    FRowTotalCell:TlrCrossDesignView;
    FColTitleCell:TlrCrossDesignView;
    FColTotalCell:TlrCrossDesignView;
    FGrandTotalCell:TlrCrossDesignView;

    FTotalCHCell:TlrCrossDesignView;
    FTotalRHCell:TlrCrossDesignView;

    FBandDataRowRT : TfrBandView;
    FBandCrossRowRT : TfrBandView;

    procedure InitCrossData;
    procedure DoneCrossData;
    procedure CreateDesignObjects;
    procedure SetCellFields(AValue: TStrings);
    procedure SetColTitleCell(AValue: TlrCrossDesignView);
    procedure SetColTotalCell(AValue: TlrCrossDesignView);
    procedure SetColumnFields(AValue: TStrings);
    procedure SetDataCell(AValue: TlrCrossDesignDataView);
    procedure SetGrandTotalCell(AValue: TlrCrossDesignView);
    procedure SetRowFields(AValue: TStrings);

    procedure OnPrintColumn(ColNo: Integer; var AWidth: Integer);
    procedure OnEnterRect(AMemo: TStringList; AView: TfrView);
    procedure SetRowTitleCell(AValue: TlrCrossDesignView);
    procedure SetRowTotalCell(AValue: TlrCrossDesignView);
    procedure SetTotalCHCell(AValue: TlrCrossDesignView);
    procedure SetTotalRHCell(AValue: TlrCrossDesignView);
  protected
    function CalcHeight: Integer; override;
    function MinHeight: Integer; override;
    function RemainHeight: Integer; override;
    procedure SetName(const AValue: string); override;
    procedure PrepareObject; override;
    procedure AfterCreate;override;
  public
    constructor Create(AOwnerPage:TfrPage);override;
    destructor Destroy; override;

    procedure Print(Stream: TStream); override;
    procedure Draw(aCanvas: TCanvas); override;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    procedure Assign(Source: TPersistent); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SavetoXML(XML: TLrXMLConfig; const Path: String); override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    function ColCount:integer;
    function RowCount:integer;

  published
    property ShowColumnHeader: Boolean read FShowColumnHeader write FShowColumnHeader default True;
    property ShowColumnTotal: Boolean read FShowColumnTotal write FShowColumnTotal default True;
    property ShowCorner: Boolean read FShowCorner write FShowCorner default True;
    property ShowRowHeader: Boolean read FShowRowHeader write FShowRowHeader default True;
    property ShowRowTotal: Boolean read FShowRowTotal write FShowRowTotal default True;
    property ShowTitle: Boolean read FShowTitle write FShowTitle default True;
    property ShowGrandTotal: Boolean read FShowGrandTotal write FShowGrandTotal default True;
    property ShowTotalCHCell: Boolean read FShowTotalCHCell write FShowTotalCHCell default True;
    property ShowTotalRHCell: Boolean read FShowTotalRHCell write FShowTotalRHCell default True;

    property DataCell:TlrCrossDesignDataView read FDataCell write SetDataCell;
    property RowTitleCell:TlrCrossDesignView read FRowTitleCell write SetRowTitleCell;
    property RowTotalCell:TlrCrossDesignView read FRowTotalCell write SetRowTotalCell;
    property ColTitleCell:TlrCrossDesignView read FColTitleCell write SetColTitleCell;
    property ColTotalCell:TlrCrossDesignView read FColTotalCell write SetColTotalCell;
    property GrandTotalCell:TlrCrossDesignView read FGrandTotalCell write SetGrandTotalCell;
    property TotalCHCell:TlrCrossDesignView read FTotalCHCell write SetTotalCHCell;
    property TotalRHCell:TlrCrossDesignView read FTotalRHCell write SetTotalRHCell;

    property Restrictions;
    property FillColor;
    property DataSet:string read FDataSetName write FDataSetName;
    property CellFields:TStrings read FCellFields write SetCellFields;
    property ColumnFields:TStrings read FColumnFields write SetColumnFields;
    property RowFields:TStrings read FRowFields write SetRowFields;
  end;

const
  NumericFieldTypes = [ftSmallint, ftInteger, ftWord, ftFloat, ftCurrency,
    ftBCD, ftAutoInc, ftLargeint];

implementation
uses lr_CrossTabEditor, LR_Utils, strutils, variants, Math;

{$R *.res}

var
  lrBMPCrossView : TBitMap = nil;

{ TlrCrossView }

type

  { TlrCrossPage }

  TlrCrossPage = class(TfrPage)
    constructor Create(AOwnerPage:TfrPage); override;
  end;


{ TlrCrossDesignDataView }

constructor TlrCrossDesignDataView.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  FAlternativeColor:=clNone;
end;

procedure TlrCrossDesignDataView.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TlrCrossDesignDataView then
    FAlternativeColor:=TlrCrossDesignDataView(Source).FAlternativeColor;
end;

procedure TlrCrossDesignDataView.LoadFromXML(XML: TLrXMLConfig;
  const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  FAlternativeColor := StringToColor(XML.GetValue(Path+'AlternativeColor/Value', 'clNone'));
end;

procedure TlrCrossDesignDataView.SavetoXML(XML: TLrXMLConfig; const Path: String
  );
begin
  inherited SavetoXML(XML, Path);
  XML.SetValue(Path+'AlternativeColor/Value', ColorToString(FAlternativeColor));
end;

{ TlrCrossDesignView }

constructor TlrCrossDesignView.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  FDesignOptions:=FDesignOptions + [doUndoDisable, doChildComponent];
  Restrictions:=Restrictions + [lrrDontSize, lrrDontMove, lrrDontDelete];
//  Frames:=[frbLeft, frbTop, frbRight, frbBottom];
  FrameStyle:=frsSolid;
end;

destructor TlrCrossDesignView.Destroy;
begin
  if Assigned(OwnerPage) then
    OwnerPage.Objects.Remove(Self);
  inherited Destroy;
end;

{ TlrCrossPage }

constructor TlrCrossPage.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  PrintToPrevPage:=true;
end;

function TlrCrossView.ColCount: integer;
begin
  if Assigned(FExVarArray) then
    Result:=FExVarArray.ColCount
  else
    Result:=0;
end;

function TlrCrossView.RowCount: integer;
begin
  if Assigned(FExVarArray) then
    Result:=FExVarArray.RowCount
  else
    Result:=0;
end;

procedure TlrCrossView.InitCrossData;
var
  FD:TField;
  FR:TField;
  FC:TField;
  S: String;
  P:TBookMark;
  V, VT, SR, SC:Variant;
  FCalcTotal:boolean;
  j: Integer;
  i: Integer;
  FuncNo:integer;
  S1: String;
  ExItem:TExItem;

function DoFunc(V1, V2:Variant):Variant;
begin
  case FuncNo of
    1:Result:=V1 + V2;     //SUM
    2:if V1<V2 then Result:=V1
      else Result:=V2; //MIN
    3:if V1>V2 then Result:=V1
       else Result:=V2; //MAX
  else
    Result:=V2; //NONE
  end
  //COUNT
  //AVG
end;

begin
  DoneCrossData;
  FData:=nil;
  FD:=nil;
  FR:=nil;
  FC:=nil;
  FuncNo:=1;

  FExVarArray:=TExVarArray.Create;
  FRowTotalArray := TExRow.Create;
  FColTotalArray := TExRow.Create;

  FData:=frGetDataSet(DataSet);
  if (not Assigned(FData)) or not (FData.Active) then
    exit;

  if CellFields.Count>0 then
  begin
    S:=CellFields[0];
    if Pos('|', S)>0 then
    begin
      S1:=Copy2SymbDel(S, '|');
      for i:=0 to CrossFuncCount-1 do
        if CrossFuncList[i] = S then
        begin
          FuncNo:=i;
          break;
        end;
      S:=S1;
    end;

    FD:=FData.FindField(S);

  end;

  if RowFields.Count>0 then
    FR:=FData.FindField(RowFields[0]);

  if ColumnFields.Count>0 then
    FC:=FData.FindField(ColumnFields[0]);

  if not (Assigned(FD) and Assigned(FR) and Assigned(FC)) then
    exit;


  FCalcTotal:=FD.DataType in NumericFieldTypes;

  P:=FData.GetBookmark;
  FData.DisableControls;
  try
    FData.First;
    while not FData.EOF do
    begin
      if FCalcTotal then
      begin
        V:=FExVarArray.Cell[FC.Value, FR.Value];
        if V = null then
        begin
          if FuncNo in [2,3] then
            V:=FD.AsFloat
          else
            V:=0;
        end;
        FExVarArray.Cell[FC.Value, FR.Value]:=DoFunc(V, FD.AsFloat);
      end
      else
        FExVarArray.Cell[FC.Value, FR.Value]:=FD.DisplayText;

      ExItem:=FExVarArray.CellData[FC.Value, FR.Value];
      if Assigned(ExItem) then
        ExItem.SaveBookmark(FData);
      FData.Next;
    end;
  finally
    FData.GotoBookmark(P);
    FData.FreeBookmark(P);
    FData.EnableControls;
  end;

  if FCalcTotal then
  begin
    FTotal:=0;
    for j:=0 to FExVarArray.RowCount - 1 do
    begin
      SR:=FExVarArray.RowHeader[j];

      if FuncNo in [2,3] then
        VT:=FExVarArray.Cell[FExVarArray.ColHeader[0], SR]
      else
        VT:=0;

      for i:=0 to FExVarArray.ColCount - 1 do
      begin
        SC:=FExVarArray.ColHeader[i];
        V:=FExVarArray.Cell[SC, SR];
        if V<>null then
          VT:=DoFunc(VT, V);
      end;
      FRowTotalArray[SR]:=VT;
      FTotal:=DoFunc(FTotal, VT);
    end;

    for i:=0 to FExVarArray.ColCount - 1 do
    begin
      SC:=FExVarArray.ColHeader[i];

      if FuncNo in [2,3] then
        VT:=FExVarArray.Cell[SC, FExVarArray.RowHeader[0]]
      else
        VT:=0;

      for j:=0 to FExVarArray.RowCount - 1 do
      begin
        SR:=FExVarArray.RowHeader[j];
        V:=FExVarArray.Cell[SC, SR];
        if V<>null then
          VT:=DoFunc(VT, V);
      end;
      FColTotalArray[SC]:=VT;
    end;
  end;
end;

procedure TlrCrossView.DoneCrossData;
begin
  if Assigned(FExVarArray) then
    FreeAndNil(FExVarArray);
  if Assigned(FRowTotalArray) then
    FreeAndNil(FRowTotalArray);

  if Assigned(FColTotalArray) then
    FreeAndNil(FColTotalArray);

  FTotal:=null;
end;

procedure TlrCrossView.CreateDesignObjects;

function DoCreateDesignObjects(AName, ACaption:string; AWidth, AHeight:integer):TlrCrossDesignView;
begin
  Result:=TlrCrossDesignView.Create(OwnerPage);
  Result.Name:=Name+'_'+AName;
  Result.Memo.Text:=ACaption;
  Result.dx:=AWidth;
  Result.dY:=AHeight;
end;

begin
  if Assigned(FDataCell) then exit;

  FDataCell:=TlrCrossDesignDataView.Create(OwnerPage);
  FDataCell.Name:=Name+'_'+'DataCell';
  FDataCell.Memo.Text:='Data';
  FDataCell.dx:=60;
  FDataCell.dY:=18;
//  DoCreateDesignObjects('DataCell', 'Data', 60, 18);

  FRowTitleCell:=DoCreateDesignObjects('RowTitleCell', 'Row title', 60, 18);
  FRowTotalCell:=DoCreateDesignObjects('RowTotalCell', 'Row total', 60, 18);
  FColTitleCell:=DoCreateDesignObjects('ColTitleCell', 'Col title', 60, 18);
  FColTotalCell:=DoCreateDesignObjects('ColTotalCell', 'Col total', 60, 18);
  FGrandTotalCell:=DoCreateDesignObjects('GrandTotalCell', 'Grand total', 60, 18);

  FTotalCHCell:=DoCreateDesignObjects('TotalCHCell', 'Total CH cell', 60, 18);
  FTotalRHCell:=DoCreateDesignObjects('TotalRHCell', 'Total RH cell', 60, 18);

  FDataCell.Restrictions:=FDataCell.Restrictions - [lrrDontSize];
  FRowTitleCell.Restrictions:=FDataCell.Restrictions - [lrrDontSize];
  FRowTotalCell.Restrictions:=FDataCell.Restrictions - [lrrDontSize];
end;

procedure TlrCrossView.SetCellFields(AValue: TStrings);
begin
  FCellFields.Assign(AValue);
end;

procedure TlrCrossView.SetColTitleCell(AValue: TlrCrossDesignView);
begin
  FColTitleCell.Assign(AValue);
end;

procedure TlrCrossView.SetColTotalCell(AValue: TlrCrossDesignView);
begin
  FColTotalCell.Assign(AValue);
end;

procedure TlrCrossView.SetColumnFields(AValue: TStrings);
begin
  FColumnFields.Assign(AValue);
end;

procedure TlrCrossView.SetDataCell(AValue: TlrCrossDesignDataView);
begin
  FDataCell.Assign(AValue);
end;

procedure TlrCrossView.SetGrandTotalCell(AValue: TlrCrossDesignView);
begin
  FGrandTotalCell.Assign(AValue);
end;

procedure TlrCrossView.SetRowFields(AValue: TStrings);
begin
  FRowFields.Assign(AValue);
end;

procedure TlrCrossView.OnPrintColumn(ColNo: Integer; var AWidth: Integer);
begin
{
if (ColNo > 0) and (ColNo <= FRxColInfoList.Count) then
    Width := TRxColInfo(FRxColInfoList[ColNo-1]).ColWidth;
}
  Width := FDataCell.DX;
end;

procedure TlrCrossView.OnEnterRect(AMemo: TStringList; AView: TfrView);
var
  S: String;
  ColNo: Integer;
  RecNo: Integer;
  V, SC, SR : Variant;
  ExItem:TExItem;
begin
  ColNo:=FBandCrossRowRT.Parent.DataSet.RecNo;
  RecNo:=FBandDataRowRT.Parent.DataSet.RecNo;

  S:=AMemo[0];
  if S='-Cell-' then
  begin
    SC:=FExVarArray.ColHeader[ColNo];
    SR:=FExVarArray.RowHeader[RecNo];

    V:=FExVarArray.Cell[SC, SR];
    if V<>null then
      S:=CurReport.FormatValue(V, AView.Format, AView.FormatStr)
    else
      S:='';

    if (DataCell.AlternativeColor <> clNone) and (RecNo  mod 2 = 1) then
      AView.FillColor:=DataCell.AlternativeColor
    else
      AView.FillColor:=DataCell.FillColor;

    ExItem:=FExVarArray.CellData[SC, SR];
    if Assigned(ExItem) and ExItem.IsBookmarkValid then
      ExItem.SaveBookmark(FData);
  end
  else
  if S = '-RowTitle-' then
  begin
    S:=FExVarArray.RowHeader[RecNo];
  end
  else
  if S = '-ColTitle-' then
  begin
    S:=FExVarArray.ColHeader[ColNo];
  end
  else
  if S = '-RowFooter-' then
  begin
    SR:=FExVarArray.RowHeader[RecNo];
    SC:=FExVarArray.ColHeader[ColNo];
    V:=FRowTotalArray[SR];
    if V<>null then
      S:=CurReport.FormatValue(V, AView.Format, AView.FormatStr)
    else
      S:='';
  end
  else
  if S= '-ColFooter-' then
  begin
    SR:=FExVarArray.RowHeader[RecNo];
    SC:=FExVarArray.ColHeader[ColNo];
    V:=FColTotalArray[SC];
    if V<>null then
      S:=CurReport.FormatValue(V, AView.Format, AView.FormatStr)
    else
      S:='';
  end
  else
  if S = '-GrandTotal-' then
  begin
    if FTotal<>null then
      S:=CurReport.FormatValue(FTotal, AView.Format, AView.FormatStr)
    else
      S:='';
  end;

  AMemo[0]:= S;
end;

procedure TlrCrossView.SetRowTitleCell(AValue: TlrCrossDesignView);
begin
  FRowTitleCell.Assign(AValue);
end;

procedure TlrCrossView.SetRowTotalCell(AValue: TlrCrossDesignView);
begin
  FRowTotalCell.Assign(AValue);
end;

procedure TlrCrossView.SetTotalCHCell(AValue: TlrCrossDesignView);
begin
  FTotalCHCell.Assign(AValue);
end;

procedure TlrCrossView.SetTotalRHCell(AValue: TlrCrossDesignView);
begin
  FTotalRHCell.Assign(AValue);
end;

function TlrCrossView.CalcHeight: Integer;
var
  RC:integer;
begin
  TextHeight:=20;
  RC:=RowCount;
  if RC>0 then
    RC:=RC + Ord(FShowColumnHeader) + Ord(FShowColumnTotal);
  Result := RC * TextHeight;
end;

function TlrCrossView.MinHeight: Integer;
begin
  Result := CalcHeight;
end;

function TlrCrossView.RemainHeight: Integer;
begin
  Result := CalcHeight;
end;

procedure TlrCrossView.SetName(const AValue: string);
begin
  inherited SetName(AValue);
  if Assigned(FDataCell) then
  begin
    FDataCell.Name:=Name+'_'+'Data';
    FRowTitleCell.Name:=Name+'_'+'RowTitleCell';
    FRowTotalCell.Name:=Name+'_'+'RowTotalCell';
    FColTitleCell.Name:=Name+'_'+'ColTitleCell';
    FColTotalCell.Name:=Name+'_'+'ColTotalCell';
    FGrandTotalCell.Name:=Name+'_'+'GrandTotalCell';
    TotalCHCell.Name:=Name+'_'+'TotalCHCell';
    TotalRHCell.Name:=Name+'_'+'TotalRHCell';
  end;
end;

procedure TlrCrossView.PrepareObject;
begin
  inherited PrepareObject;
  InitCrossData;
end;

procedure TlrCrossView.AfterCreate;
begin
  inherited AfterCreate;
{ TODO : Set default size }
//  DX:=10 + 22 * 3;
end;

procedure TlrCrossView.Print(Stream: TStream);
var
  FPage : TlrCrossPage;

  FBandDataHeader : TfrBandView;
  FBandDataFooter : TfrBandView;

  FBandCrossHeader : TfrBandView;
  FBandCrossFooter : TfrBandView;

  FBandDataRow : TfrBandView;
  FBandCrossRow : TfrBandView;

  FYPos : integer;
  FView : TfrMemoView;
  FXPos: Integer;
  FSavePage : TfrPage;
  FSavePrintColumnEvent :TPrintColumnEvent;
  FSaveEnterRectEvent : TEnterRectEvent;

  XX:integer;
  YY: Integer;
begin
  Memo1.Assign(Memo);
  CurReport.InternalOnEnterRect(Memo1, Self);
  frInterpretator.DoScript(Script);
  if not Visible then Exit;


  FSavePage := CurPage;
  FSavePrintColumnEvent:=CurReport.OnPrintColumn;
  FSaveEnterRectEvent:=CurReport.OnEnterRect;

  CurReport.OnPrintColumn:=@OnPrintColumn;
  CurReport.OnEnterRect:=@OnEnterRect;

  BeginDraw(Canvas);

  FYPos:=0;
  FXPos:=Self.x;


  FPage:=TlrCrossPage.Create(nil);
  FPage.ChangePaper(OwnerPage.pgSize, OwnerPage.Width, OwnerPage.Height, OwnerPage.Orientation);

  if FShowTotalRHCell then
  begin
    FView := frCreateObject(gtMemo, '', FPage) as TfrMemoView;
    FView.Assign(FTotalRHCell);
    FView.SetBounds(FXPos, FYPos, FTotalRHCell.DX, FTotalRHCell.dy);
  end;


 if FShowColumnHeader then
  begin
    XX:=FXPos;
    if FShowRowHeader then
      XX:=XX + FRowTitleCell.DX + 2;

    FBandDataHeader := TfrBandView(frCreateObject(gtBand, '', FPage));
    FBandDataHeader.BandType := btMasterHeader;
    FBandDataHeader.SetBounds(XX, 0, 1000, 18);
    FBandDataHeader.Name:=Name+'_DataHeader';
    FBandDataHeader.Stretched:=true;

    FView := frCreateObject(gtMemo, '', FPage) as TfrMemoView;
    FView.Assign(FColTitleCell);
    FView.SetBounds(XX, FYPos, FDataCell.DX, FColTitleCell.dy);
    FView.Memo.Text:='-ColTitle-';
    FYPos := FYPos + FColTitleCell.dY + 2;
  end;

  if FShowRowHeader or FShowTotalRHCell then
  begin
    FBandCrossHeader := TfrBandView(frCreateObject(gtBand, '', FPage));
    FBandCrossHeader.BandType := btCrossHeader;
    FBandCrossHeader.SetBounds(FXPos, 0, FRowTitleCell.DX, 1000);
    FBandCrossHeader.Name:=Name+'_CrossHeader';

    if FShowRowHeader then
    begin
      FView := frCreateObject(gtMemo, '', FPage) as TfrMemoView;
      FView.Assign(FRowTitleCell);
      FView.SetBounds(FXPos, FYPos, FRowTitleCell.DX, FRowTitleCell.dy);
      FView.Memo.Text:='-RowTitle-';
    end;
    FXPos:=FXPos + FRowTitleCell.DX + 2;
  end;

  //Make main data band
  FBandDataRow := TfrBandView(frCreateObject(gtBand, '', FPage));
  FBandDataRow.BandType := btMasterData;
  FBandDataRow.DataSet := IntToStr(RowCount);
  FBandDataRow.SetBounds(0, FYPos, 1000, 18);
  FBandDataRow.Flags:=FBandDataRow.Flags or flStretched;
  FBandDataRow.Name:=Name+'_MasterData';

  FBandCrossRow := TfrBandView(frCreateObject(gtBand, '', FPage));
  FBandCrossRow.BandType := btCrossData;
  FBandCrossRow.Dataset := IntToStr(ColCount);
  FBandCrossRow.SetBounds(FXPos, 0, FDataCell.DX, 1000);
  FBandCrossRow.Name:=Name+'_CrossData';

  FView := frCreateObject(gtMemo, '', FPage) as TfrMemoView;
  FView.Assign(FDataCell);
  FView.SetBounds(FXPos, FYPos, FDataCell.DX, FDataCell.dy);
  FView.Memo.Text:='-Cell-';

  if FShowRowTotal or FShowGrandTotal then
  begin
    XX:=FXPos + FDataCell.X + FDataCell.DX + 2;
    FBandCrossFooter := TfrBandView(frCreateObject(gtBand, '', FPage));
    FBandCrossFooter.BandType := btCrossFooter;
    FBandCrossFooter.SetBounds(XX, 0, FRowTotalCell.DX, 1000);
    FBandCrossFooter.Flags:=FBandDataRow.Flags or flStretched;
    FBandCrossFooter.Name:=Name+'_CrossFooter';

    if FShowRowTotal then
    begin
      FView := frCreateObject(gtMemo, '', FPage) as TfrMemoView;
      FView.Assign(FRowTotalCell);
      FView.SetBounds(XX, FYPos, FRowTotalCell.DX, FRowTotalCell.dy);
      FView.Memo.Text:='-RowFooter-';
    end;
  end;

  if FShowColumnTotal or FShowGrandTotal then
  begin
    YY:=FYPos + FDataCell.Y + FDataCell.DY + 2;
    FBandDataFooter := TfrBandView(frCreateObject(gtBand, '', FPage));
    FBandDataFooter.BandType := btMasterFooter;
    FBandDataFooter.SetBounds(0, YY, 1000, FColTotalCell.DY);
    FBandDataFooter.Flags:=FBandDataRow.Flags or flStretched;
    FBandDataFooter.Name:=Name+'_BandDataFooter';

    if FShowColumnTotal then
    begin
      FView := frCreateObject(gtMemo, '', FPage) as TfrMemoView;
      FView.Assign(FColTotalCell);
      FView.SetBounds(FXPos, YY, FColTotalCell.DX, FColTotalCell.DY);
      FView.Memo.Text:='-ColFooter-';
    end;
  end;

  if FShowTotalCHCell then
  begin
    XX:=FXPos + FDataCell.X + FDataCell.DX + 2;
    YY:=FYPos - FColTitleCell.dy - 2;

    FView := frCreateObject(gtMemo, '', FPage) as TfrMemoView;
    FView.Assign(FTotalCHCell);
    FView.SetBounds(XX, YY, FTotalCHCell.DX, FTotalCHCell.DY);
  end;

  if FShowGrandTotal then
  begin
    XX:=FXPos + FDataCell.X + FDataCell.DX + 2;
    YY:=FYPos + FDataCell.Y + FDataCell.DY + 2;

    FView := frCreateObject(gtMemo, '', FPage) as TfrMemoView;
    FView.Assign(FGrandTotalCell);
    FView.SetBounds(XX, YY, FGrandTotalCell.DX, FGrandTotalCell.DY);
    FView.Memo.Text:='-GrandTotal-';
  end;


  FPage.InitReport;
  FBandDataRowRT:=TfrBandView(FPage.FindRTObject(FBandDataRow.Name));
  FBandCrossRowRT:=TfrBandView(FPage.FindRTObject(FBandCrossRow.Name));

  FPage.Mode := pmBuildList;
  FPage.FormPage;

  FPage.CurY := FBandDataRow.y + Self.Y;
  FPage.CurBottomY := FSavePage.CurBottomY;
  FPage.ColCount := 1;

  FPage.PlayFrom := 0;
  while FPage.PlayFrom < FPage.List.Count do
  begin
    FPage.PlayRecList;
{    if FPage.List.Count > FPage.PlayFrom  then
      FPage.NewPage;}
  end;

  FPage.DoneReport;
  FPage.Free;

  CurPage:=FSavePage;
  CurReport.OnPrintColumn := FSavePrintColumnEvent;
  CurReport.OnEnterRect   := FSaveEnterRectEvent;

  FBandDataRowRT:=nil;
  FBandCrossRowRT:=nil;
end;

procedure TlrCrossView.Draw(aCanvas: TCanvas);
var
  FY:integer;
  FX: Integer;
begin
  Frames:=[frbLeft, frbTop, frbRight, frbBottom];
  FrameStyle:=frsSolid;
  BeginDraw(aCanvas);
  CalcGaps;

  ShowBackGround;
  ShowFrame;

  FX:=X + 10;
  FY:=Y + 10;

  FTotalRHCell.X:=FX;
  FTotalRHCell.Y:=FY;
  FTotalRHCell.dx:=FRowTitleCell.dx;

  FColTitleCell.X:=FX + FRowTitleCell.dX + 4;
  FColTitleCell.y:=FY;
  FColTitleCell.DX:=FDataCell.DX;

  FTotalCHCell.x:=FColTitleCell.x + FColTitleCell.DX + 4;
  FTotalCHCell.y:=FY;
  FTotalCHCell.DX:=FRowTotalCell.DX;

  Inc(FY, FColTitleCell.dy + 4);

  FRowTitleCell.X:=FX;
  FRowTitleCell.y:=FY;

  FDataCell.x:=FX + FRowTitleCell.dX + 4;
  FDataCell.y:=FY;

  FRowTotalCell.X:=FX + FRowTitleCell.dX + FDataCell.dX + 8;
  FRowTotalCell.y:=FY;

  Inc(FY, FColTitleCell.dy + 4);

  FColTotalCell.X:=FX + FRowTitleCell.dX + 4;
  FColTotalCell.y:=FY;
  FColTotalCell.DX:=FDataCell.DX;


  FGrandTotalCell.X:=FX + FRowTitleCell.dX + FDataCell.dX + 8;
  FGrandTotalCell.y:=FY;
  FGrandTotalCell.DX:=FRowTotalCell.DX;

  aCanvas.Draw(X + dx - 20, Y + dy - 20, lrBMPCrossView);
end;

procedure TlrCrossView.BeginUpdate;
begin
  inherited BeginUpdate;
  FDataCell.BeginUpdate;
  FRowTitleCell.BeginUpdate;
  FRowTotalCell.BeginUpdate;
  FColTitleCell.BeginUpdate;
  FColTotalCell.BeginUpdate;
  FGrandTotalCell.BeginUpdate;
  FTotalCHCell.BeginUpdate;
  FTotalRHCell.BeginUpdate;
end;

procedure TlrCrossView.EndUpdate;
begin
  inherited EndUpdate;
  FDataCell.EndUpdate;
  FRowTitleCell.EndUpdate;
  FRowTotalCell.EndUpdate;
  FColTitleCell.EndUpdate;
  FColTotalCell.EndUpdate;
  FGrandTotalCell.EndUpdate;
  FTotalCHCell.EndUpdate;
  FTotalRHCell.EndUpdate;
end;

constructor TlrCrossView.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  FDesignOptions:= FDesignOptions + [doUndoDisable];

  Typ := gtAddIn;
  BaseName := 'CrossView';
  TextHeight:=0;
  LineSpacing:=2;

  FShowColumnHeader:=True;
  FShowColumnTotal:=True;
  FShowCorner:=True;
  FShowRowHeader:=True;
  FShowRowTotal:=True;
  FShowTitle:=True;
  FShowGrandTotal:=true;
  FShowTotalCHCell:=true;
  FShowTotalRHCell:=true;

  FCellFields:=TStringList.Create;
  FColumnFields:=TStringList.Create;
  FRowFields:=TStringList.Create;

  CreateDesignObjects;
end;

destructor TlrCrossView.Destroy;
begin
  FreeAndNil(FDataCell);
  FreeAndNil(FRowTitleCell);
  FreeAndNil(FRowTotalCell);
  FreeAndNil(FColTitleCell);
  FreeAndNil(FColTotalCell);
  FreeAndNil(FGrandTotalCell);

  FreeAndNil(FCellFields);
  FreeAndNil(FColumnFields);
  FreeAndNil(FRowFields);
  DoneCrossData;
  inherited Destroy;
end;

procedure TlrCrossView.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TlrCrossView then
  begin
    FShowColumnHeader:=TlrCrossView(Source).FShowColumnHeader;
    FShowColumnTotal:=TlrCrossView(Source).FShowColumnTotal;
    FShowCorner:=TlrCrossView(Source).FShowCorner;
    FShowRowHeader:=TlrCrossView(Source).FShowRowHeader;
    FShowRowTotal:=TlrCrossView(Source).FShowRowTotal;
    FShowTitle:=TlrCrossView(Source).FShowTitle;
    FShowGrandTotal:=TlrCrossView(Source).FShowGrandTotal;
    FShowTotalCHCell:=TlrCrossView(Source).FShowTotalCHCell;
    FShowTotalRHCell:=TlrCrossView(Source).FShowTotalRHCell;

    FillColor:=TlrCrossView(Source).FillColor;
    DataSet:=TlrCrossView(Source).DataSet;
    CellFields.Assign(TlrCrossView(Source).CellFields);
    ColumnFields.Assign(TlrCrossView(Source).ColumnFields);
    RowFields.Assign(TlrCrossView(Source).RowFields);


    FDataCell.Assign(TlrCrossView(Source).FDataCell);
    FRowTitleCell.Assign(TlrCrossView(Source).FRowTitleCell);
    FRowTotalCell.Assign(TlrCrossView(Source).FRowTotalCell);
    FColTitleCell.Assign(TlrCrossView(Source).FColTitleCell);
    FColTotalCell.Assign(TlrCrossView(Source).FColTotalCell);
    FGrandTotalCell.Assign(TlrCrossView(Source).FGrandTotalCell);
    FTotalCHCell.Assign(TlrCrossView(Source).FTotalCHCell);
    FTotalRHCell.Assign(TlrCrossView(Source).FTotalRHCell);

  end;
end;

procedure TlrCrossView.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  FShowColumnHeader:= XML.GetValue(Path+'ShowColumnHeader/Value', true);
  FShowColumnTotal:= XML.GetValue(Path+'ShowColumnTotal/Value', true);
  FShowCorner:= XML.GetValue(Path+'ShowCorner/Value', true);
  FShowRowHeader:= XML.GetValue(Path+'ShowRowHeader/Value', true);
  FShowRowTotal:= XML.GetValue(Path+'ShowRowTotal/Value', true);
  FShowTitle:= XML.GetValue(Path+'ShowTitle/Value', true);
  FShowGrandTotal:= XML.GetValue(Path+'ShowGrandTotal/Value', true);
  FShowTotalCHCell:= XML.GetValue(Path+'ShowTotalCHCell/Value', true);
  FShowTotalRHCell:= XML.GetValue(Path+'ShowTotalRHCell/Value', true);

  FDataSetName:=XML.GetValue(Path+'DataSetName/Value', '');
  FCellFields.Text:=XML.GetValue(Path+'CellFields/Value', '');
  FColumnFields.Text:=XML.GetValue(Path+'ColumnFields/Value', '');
  FRowFields.Text:=XML.GetValue(Path+'RowFields/Value', '');

  BeginUpdate;
  FDataCell.LoadFromXML(XML, Path+'DataCell/');
  FRowTitleCell.LoadFromXML(XML, Path+'RowTitleCell/');
  FRowTotalCell.LoadFromXML(XML, Path+'RowTotalCell/');
  FColTitleCell.LoadFromXML(XML, Path+'ColTitleCell/');
  FColTotalCell.LoadFromXML(XML, Path+'ColTotalCell/');
  FGrandTotalCell.LoadFromXML(XML, Path+'GrandTotalCell/');

  FTotalCHCell.LoadFromXML(XML, Path+'TotalCHCell/');
  FTotalRHCell.LoadFromXML(XML, Path+'TotalRHCell/');
  EndUpdate;

  FDataCell.DY:=18;
  FRowTitleCell.DY:=18;
  FRowTotalCell.DY:=18;
  FColTitleCell.DY:=18;
  FColTotalCell.DY:=18;
  FGrandTotalCell.DY:=18;
  FTotalCHCell.DY:=18;
  FTotalRHCell.DY:=18;
end;

procedure TlrCrossView.SavetoXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SavetoXML(XML, Path);
  XML.SetValue(Path+'ShowColumnHeader/Value', FShowColumnHeader);
  XML.SetValue(Path+'ShowColumnTotal/Value', FShowColumnTotal);
  XML.SetValue(Path+'ShowCorner/Value'{%H-}, FShowCorner);
  XML.SetValue(Path+'ShowRowHeader/Value', FShowRowHeader);
  XML.SetValue(Path+'ShowRowTotal/Value', FShowRowTotal);
  XML.SetValue(Path+'ShowTitle/Value', FShowTitle);
  XML.SetValue(Path+'ShowGrandTotal/Value', FShowGrandTotal);
  XML.SetValue(Path+'ShowTotalCHCell/Value', FShowTotalCHCell);
  XML.SetValue(Path+'ShowTotalRHCell/Value', FShowTotalRHCell);
  XML.SetValue(Path+'DataSetName/Value', FDataSetName);

  XML.SetValue(Path+'CellFields/Value', FCellFields.Text);
  XML.SetValue(Path+'ColumnFields/Value', FColumnFields.Text);
  XML.SetValue(Path+'RowFields/Value', FRowFields.Text);

  FDataCell.SaveToXML(XML, Path+'DataCell/');
  FRowTitleCell.SaveToXML(XML, Path+'RowTitleCell/');
  FRowTotalCell.SaveToXML(XML, Path+'RowTotalCell/');
  FColTitleCell.SaveToXML(XML, Path+'ColTitleCell/');
  FColTotalCell.SaveToXML(XML, Path+'ColTotalCell/');
  FGrandTotalCell.SaveToXML(XML, Path+'GrandTotalCell/');

  FTotalCHCell.SaveToXML(XML, Path+'TotalCHCell/');
  FTotalRHCell.SaveToXML(XML, Path+'TotalRHCell/');
end;

procedure TlrCrossView.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  Stream.Read(FShowColumnHeader, 1); { TODO : Need use SizeOf(????) }
  Stream.Read(FShowColumnTotal, 1); { TODO : Need use SizeOf(????) }
  Stream.Read(FShowCorner, 1); { TODO : Need use SizeOf(????) }
  Stream.Read(FShowRowHeader, 1); { TODO : Need use SizeOf(????) }
  Stream.Read(FShowRowTotal, 1); { TODO : Need use SizeOf(????) }
  Stream.Read(FShowTitle, 1); { TODO : Need use SizeOf(????) }
  Stream.Read(FShowGrandTotal, 1); { TODO : Need use SizeOf(????) }
  Stream.Read(FShowTotalCHCell, 1); { TODO : Need use SizeOf(????) }
  Stream.Read(FShowTotalRHCell, 1); { TODO : Need use SizeOf(????) }

  FDataSetName:=frReadString(Stream);
  FCellFields.Text:=frReadString(Stream);
  FColumnFields.Text:=frReadString(Stream);
  FRowFields.Text:=frReadString(Stream);

  BeginUpdate;
  FDataCell.LoadFromStream(Stream);
  FRowTitleCell.LoadFromStream(Stream);
  FRowTotalCell.LoadFromStream(Stream);
  FColTitleCell.LoadFromStream(Stream);
  FColTotalCell.LoadFromStream(Stream);
  FGrandTotalCell.LoadFromStream(Stream);
  FTotalCHCell.LoadFromStream(Stream);
  FTotalRHCell.LoadFromStream(Stream);
  EndUpdate;

  FDataCell.DY:=18;
  FRowTitleCell.DY:=18;
  FRowTotalCell.DY:=18;
  FColTitleCell.DY:=18;
  FColTotalCell.DY:=18;
  FGrandTotalCell.DY:=18;
  FTotalCHCell.DY:=18;
  FTotalRHCell.DY:=18;
end;

procedure TlrCrossView.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(FShowColumnHeader, 1);{ TODO : Need use SizeOf(????) }
  Stream.Write(FShowColumnTotal, 1); { TODO : Need use SizeOf(????) }
  Stream.Write(FShowCorner, 1);      { TODO : Need use SizeOf(????) }
  Stream.Write(FShowRowHeader, 1);   { TODO : Need use SizeOf(????) }
  Stream.Write(FShowRowTotal, 1);    { TODO : Need use SizeOf(????) }
  Stream.Write(FShowTitle, 1);       { TODO : Need use SizeOf(????) }
  Stream.Write(FShowGrandTotal, 1);  { TODO : Need use SizeOf(????) }
  Stream.Write(FShowTotalCHCell, 1); { TODO : Need use SizeOf(????) }
  Stream.Write(FShowTotalRHCell, 1); { TODO : Need use SizeOf(????) }

  frWriteString(Stream, FDataSetName);
  frWriteString(Stream, FCellFields.Text);
  frWriteString(Stream, FColumnFields.Text);
  frWriteString(Stream, FRowFields.Text);

  FDataCell.SaveToStream(Stream);
  FRowTitleCell.SaveToStream(Stream);
  FRowTotalCell.SaveToStream(Stream);
  FColTitleCell.SaveToStream(Stream);
  FColTotalCell.SaveToStream(Stream);
  FGrandTotalCell.SaveToStream(Stream);
  FTotalCHCell.SaveToStream(Stream);
  FTotalRHCell.SaveToStream(Stream);
end;

procedure InitCrossView;
begin
  if not assigned(lrBMPCrossView) then
  begin
    lrBMPCrossView := TBitmap.Create;
    lrBMPCrossView.LoadFromResourceName(HInstance, 'lr_crossview');
    frRegisterObject(TlrCrossView, lrBMPCrossView, TlrCrossView.ClassName, nil, otlReportView, nil, @lrCrossTabEditor);
  end;
end;

initialization
  InitCrossView;
finalization
  if Assigned(lrBMPCrossView) then
    FreeAndNil(lrBMPCrossView);
end.

