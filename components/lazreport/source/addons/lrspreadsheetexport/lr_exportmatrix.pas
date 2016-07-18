{
  LazReport matrix for export reports

 Copyright (C) 2014-2015 alexs alexs75.at.yandex.ru

 The module is designed to create an image of the report with the exact
 positioning of objects and subsequent binding to the worksheet

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

unit LR_ExportMatrix;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_Class, Graphics;

type

  { TExportObject }

  TExportObject = class
  private
    FAlignment: TAlignment;
    FCol: Integer;
    FLayout: TTextLayout;
    FRow: Integer;
    FMergedCol: Integer;
    FMergedRow: Integer;

    FFillColor: TColor;
    FFrameColor: TColor;
    FFrames: TfrFrameBorders;
    FFrameStyle: TfrFrameStyle;
    FFrameWidth: Double;

    FHeight: integer;
    FLeft: integer;
    FTop: integer;
    FURLInfo: string;
    FWidht: integer;

    FObjType: integer;
    FTexts:TStringList;
    FFont:TFont;
    FName:string;
    FAngle:byte;
    FWordWrap: boolean;

    FPicture: TPicture;

    function GetText: string;
  public
    constructor Create(AObj:TfrView);
    destructor Destroy; override;
    procedure NeedPicture;

    property Top:integer read FTop write FTop;
    property Left:integer read FLeft write FLeft;
    property Height:integer read FHeight write FHeight;
    property Widht:integer read FWidht write FWidht;
    property Alignment : TAlignment read FAlignment;
    property Col:Integer read FCol;
    property Row:Integer read FRow;
    property MergedCol:Integer read FMergedCol;
    property MergedRow:Integer read FMergedRow;
    property Text:string read GetText;
    property Texts:TStringList read FTexts;
    property ObjType:integer read FObjType write FObjType;
    property FillColor:TColor read FFillColor;
    property Font:TFont read FFont;
    property Frames : TfrFrameBorders read FFrames;
    property FrameColor : TColor read FFrameColor;
    property FrameStyle : TfrFrameStyle read FFrameStyle;
    property FrameWidth : Double read FFrameWidth;
    property Angle:byte read FAngle;
    property Layout : TTextLayout read FLayout;
    property WordWrap:boolean read FWordWrap;
    property URLInfo: string read FURLInfo write FURLInfo;
    property Picture: TPicture read FPicture;

  end;

  { TExportRows }

  TExportRow = class
  private
    FCells:TFpList;
    FTop: integer;
    FRow:integer;
    procedure SortCells;
    function GetObjects(X: integer): TExportObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function ExportObject(AObj:TfrView):TExportObject;

    property Top:integer read FTop write FTop;
    property Row:integer read FRow;

    property Cells:TFpList read FCells;
  end;

  { TExportMatrix }

  TExportMatrix =class
  private
    FDeleteEmptyRow: boolean;
    //FExportImages: boolean;
    FMergeCell: boolean;
    FRows:TFpList;
    FColWidth:TBoundArray;
    FRowHight:TBoundArray;
    FPageMargin:integer;
    function FindRow(FTop:integer):TExportRow;
    function GetColumnCount: integer;
    function GetColumnWidth(AColumn: integer): Integer;
    function GetColNumByLeft(ALeft:Integer):integer;
    function GetRowNumByTop(ATop:Integer):integer;

    function GetObjects(X, Y: integer): TExportObject;
    function GetRowCount: integer;
    function GetRowHight(ARow: integer): Integer;

    procedure SortRows;
    procedure UpdateColWidth;
    procedure UpdateRowHeight;
    procedure DoPostitionObject;
    procedure DoMergeCell;
    procedure DoDeleteEmptyRow;
  public
    constructor Create;
    procedure Clear;
    destructor Destroy; override;
    function FindObj(AObj:TfrView):TExportObject;
    function ExportObject(AObj:TfrView):TExportObject;
    procedure PrepareData;
    procedure NewPage;
    property ColumnCount:integer read GetColumnCount;
    property RowCount:integer read GetRowCount;
    property ColumnWidth[AColumn:integer]:Integer read GetColumnWidth;
    property RowHiht[ARow:integer]:Integer read GetRowHight;
    property Objects[X, Y:integer]:TExportObject read GetObjects;
    property DeleteEmptyRow:boolean read FDeleteEmptyRow write FDeleteEmptyRow;
    property MergeCell:boolean read FMergeCell write FMergeCell;
    property PageMargin:integer read FPageMargin write FPageMargin;
    //property ExportImages:boolean read FExportImages write FExportImages;

    property Rows:TFpList read FRows;
  end;

implementation
uses math;

{ TExportRows }

procedure TExportRow.SortCells;
var
  R, P:TExportObject;
  i: Integer;
  j: Integer;
begin
  for i:=0 to FCells.Count-2 do
  begin
    R:=TExportObject(FCells[i]);
    for j:=i+1 to FCells.Count-1 do
    begin
      if R.FLeft > TExportObject(FCells[j]).FLeft then
      begin
        P:=TExportObject(FCells[j]);
        FCells[j]:=R;
        R:=P;
      end;
    end;
    FCells[i]:=R;
  end;
end;

function TExportRow.GetObjects(X: integer): TExportObject;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to FCells.Count-1 do
    if TExportObject(FCells[i]).FCol = X then
    begin
      Result:=TExportObject(FCells[i]);
      exit;
    end;
end;

constructor TExportRow.Create;
begin
  inherited Create;
  FCells:=TFPList.Create;
end;

destructor TExportRow.Destroy;
begin
  Clear;
  FreeAndNil(FCells);
  inherited Destroy;
end;

procedure TExportRow.Clear;
var
  i:integer;
begin
  for i:=0 to FCells.Count-1 do
    TExportObject(FCells[i]).Free;
  FCells.Clear;
end;

function TExportRow.ExportObject(AObj: TfrView): TExportObject;
begin
  Result:=TExportObject.Create(AObj);
  FCells.Add(Result);
end;

{ TExportObject }

function TExportObject.GetText: string;
var
  B: Char;
  L: Integer;
begin
  if Assigned(FTexts) and (FTexts.Count>0) then
    Result:=FTexts.Text
  else
    Result:='';
end;

constructor TExportObject.Create(AObj: TfrView);
var
  S: String;
begin
  Inherited Create;

  if Assigned(AObj) then
  begin
    FName:=AObj.Name;
    FTexts:=TStringList.Create;
    FFont:=TFont.Create;

    FLeft:=AObj.X;
    FTop:=AObj.Y;
    FWidht:=AObj.DX;
    FHeight:=AObj.DY;

    FFrames:=AObj.Frames;
    FFrameColor:=AObj.FrameColor;
    FFrameStyle:=AObj.FrameStyle;
    FFrameWidth:=AObj.FrameWidth;

    if AObj is TfrMemoView then
    begin
      Texts.Text:=AObj.Memo.Text;
      FObjType:=gtMemo;
      FFillColor:=AObj.FillColor;
      FFont.Assign(TfrMemoView(AObj).Font);
      FAlignment:=TfrMemoView(AObj).Alignment;
      FAngle:=TfrMemoView(AObj).Angle;
      FLayout:=TfrMemoView(AObj).Layout;
      FWordWrap:=TfrMemoView(AObj).WordWrap;
                                    //http://www.lazarus-ide.org/
      S:=UpperCase(TfrMemoView(AObj).URLInfo);
      if (S <> '') and ((Copy(S, 1,  7) = 'HTTP://') or (Copy(S, 1, 8) = 'HTTPS://')) then
          URLInfo:=TfrMemoView(AObj).URLInfo;
    end
    else
    if AObj is TfrPictureView then
    begin
      FObjType:=gtPicture;
      NeedPicture;
      FPicture.Assign(TfrPictureView(AObj).Picture);
    end
    else
    if AObj is TfrLineView then
      FObjType:=gtLine;
  end;
end;

destructor TExportObject.Destroy;
begin
  if Assigned(FTexts) then
    FreeAndNil(FTexts);
  if Assigned(FFont) then
    FreeAndNil(FFont);

  if Assigned(FPicture) then
    FreeAndNil(FPicture);
  inherited Destroy;
end;

procedure TExportObject.NeedPicture;
begin
  if not Assigned(FPicture) then
    FPicture := TPicture.Create;
end;

{ TExportMatrix }

function TExportMatrix.FindRow(FTop: integer): TExportRow;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to FRows.Count-1 do
    if TExportRow(FRows[i]).Top = FTop + FPageMargin then
    begin
      Result:=TExportRow(FRows[i]);
      exit;
    end;
  Result:=TExportRow.Create;
  FRows.Add(Result);
  Result.Top:=FTop + FPageMargin;
end;

function TExportMatrix.FindObj(AObj: TfrView): TExportObject;
var
  i: Integer;
  F: TExportRow;
begin
  Result:=nil;
  F:=nil;
  for i:=0 to FRows.Count-1 do
    if TExportRow(FRows[i]).Top = AObj.Y + FPageMargin then
    begin
      F:=TExportRow(FRows[i]);
      break;
    end;

  if Assigned(F) then
    for i:=0 to F.FCells.Count-1 do
      if TExportObject(F.FCells[i]).Left = AObj.X then
      begin
        Result:=TExportObject(F.FCells[i]);
        exit;
      end;
end;

function TExportMatrix.GetColumnCount: integer;
begin
  Result:=Length(FColWidth);
end;

function TExportMatrix.GetColumnWidth(AColumn: integer): Integer;
begin
  if (AColumn>-1) and (AColumn<Length(FColWidth)) then
    Result:=FColWidth[AColumn]
  else
    Result:=0;
end;

function TExportMatrix.GetColNumByLeft(ALeft: Integer): integer;
var
  i: Integer;
begin
  Result:=0;
  for i:=0 to Length(FColWidth)-1 do
  begin
    if FColWidth[i]=ALeft then
    begin
      Result:=i + 1;
      exit;
    end;
  end;
end;

function TExportMatrix.GetRowNumByTop(ATop: Integer): integer;
var
  i: Integer;
begin
  Result:=0;
  for i:=0 to Length(FRowHight)-1 do
  begin
    if FRowHight[i]=ATop then
    begin
      Result:=i + 1;
      exit;
    end;
  end;
end;

function TExportMatrix.GetObjects(X, Y: integer): TExportObject;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to FRows.Count-1 do
    if TExportRow(FRows[i]).FRow = Y then
      Result:=TExportRow(FRows[i]).GetObjects(X);
end;

function TExportMatrix.GetRowCount: integer;
begin
  Result:=Length(FRowHight);
end;

function TExportMatrix.GetRowHight(ARow: integer): Integer;
begin
  if (ARow>-1) and (ARow<Length(FRowHight)) then
    Result:=FRowHight[ARow]
  else
    Result:=0;
end;

procedure TExportMatrix.SortRows;
var
  R, P:TExportRow;
  i: Integer;
  j: Integer;
begin
  for i:=0 to FRows.Count-2 do
  begin
    R:=TExportRow(FRows[i]);
    for j:=i+1 to FRows.Count-1 do
    begin
      if R.FTop > TExportRow(FRows[j]).FTop then
      begin
        P:=TExportRow(FRows[j]);
        FRows[j]:=R;
        R:=P;
      end;
    end;
    FRows[i]:=R;
  end;

  for i:=0 to FRows.Count-1 do
    TExportRow(FRows[i]).SortCells;
end;

procedure TExportMatrix.UpdateColWidth;

procedure TestLeftPos(FLeft:Integer);
var
  i: Integer;
  j: Integer;
begin
  if Length(FColWidth)>0 then
  begin
    i:=0;
    while (i<Length(FColWidth)) and (FColWidth[i]<FLeft) do Inc(i);
    if (i<Length(FColWidth)) and (FColWidth[i] = FLeft) then exit;

    SetLength(FColWidth, Length(FColWidth) + 1);

    for j:=Length(FColWidth) - 1 downto i + 1 do
      FColWidth[j]:=FColWidth[j - 1];
    FColWidth[i]:=FLeft;
  end
  else
  begin
    SetLength(FColWidth, 1);
    FColWidth[0]:=FLeft;
  end;
end;

var
  i: Integer;
  R: TExportRow;
  j: Integer;
  C: TExportObject;
begin
  for i:=0 to FRows.Count - 1 do
  begin
    R:=TExportRow(FRows[i]);
    for j:=0 to R.FCells.Count-1 do
    begin
      C:=TExportObject(R.FCells[j]);
      TestLeftPos(C.FLeft);
      TestLeftPos(C.FLeft + C.Widht);
    end;
  end;
end;

procedure TExportMatrix.UpdateRowHeight;
procedure TestLeftPos(FTop:Integer);
var
  i: Integer;
  j: Integer;
begin
  if Length(FRowHight)>0 then
  begin
    i:=0;
    while (i<Length(FRowHight)) and (FRowHight[i]<FTop) do Inc(i);
    if (i<Length(FRowHight)) and (FRowHight[i] = FTop) then exit;

    SetLength(FRowHight, Length(FRowHight) + 1);

    for j:=Length(FRowHight) - 1 downto i + 1 do
      FRowHight[j]:=FRowHight[j - 1];
    FRowHight[i]:=FTop;
  end
  else
  begin
    SetLength(FRowHight, 1);
    FRowHight[0]:=FTop;
  end;
end;

var
  i: Integer;
  R: TExportRow;
  j: Integer;
  C: TExportObject;
begin
  for i:=0 to FRows.Count - 1 do
  begin
    R:=TExportRow(FRows[i]);
    for j:=0 to R.FCells.Count-1 do
    begin
      C:=TExportObject(R.FCells[j]);
      TestLeftPos(C.FTop);
      TestLeftPos(C.FTop + C.Height);
    end;
  end;
end;

procedure TExportMatrix.DoPostitionObject;
var
  i: Integer;
  j: Integer;
  FObj: TExportObject;
  R: TExportRow;
begin
  for i:=0 to FRows.Count-1 do
  begin
    for j:=0 to TExportRow(FRows[i]).FCells.Count - 1 do
    begin
      FObj:=TExportObject(TExportRow(FRows[i]).FCells[j]);
//      FObj.FRow:=i;
      FObj.FCol:=GetColNumByLeft(FObj.FLeft);
    end;
  end;

  for i:=0 to FRows.Count-1 do
  begin
    R:=TExportRow(FRows[i]);
    R.FRow:=GetRowNumByTop(R.FTop);
    for j:=0 to R.FCells.Count-1 do
      TExportObject(R.FCells[j]).FRow:=R.FRow;
  end;
end;

procedure TExportMatrix.DoMergeCell;

function FindCelNum(APos:integer):integer;
begin
  Result:=0;
  while (Result<Length(FColWidth)) and (FColWidth[Result]<APos) do Inc(Result);
end;

function FindRowNum(APos:integer):integer;
begin
  Result:=0;
  while (Result<Length(FRowHight)) and (FRowHight[Result]<APos) do Inc(Result);
end;

var
  i: Integer;
  j: Integer;
  C: TExportObject;
  R: TExportRow;
begin
  for i:=0 to FRows.Count - 1 do
  begin
    R:=TExportRow(FRows[i]);
    for j:=0 to R.FCells.Count-1 do
    begin
      C:=TExportObject(R.FCells[j]);
      C.FMergedCol:=FindCelNum(C.FLeft + C.FWidht);
      if C.FMergedCol < C.FCol then
        C.FMergedCol:=C.FCol;

      C.FMergedRow:=FindRowNum(C.FTop + C.FHeight);
      if C.FMergedRow < C.FRow then
        C.FMergedRow:=C.FRow;
    end;
  end;
end;

procedure TExportMatrix.DoDeleteEmptyRow;
var
  R: TExportRow;
  i: Integer;
begin
  for i:=FRows.Count-1 downto 0 do
  begin
    R:=TExportRow(FRows[i]);
    if R.FCells.Count = 0 then
    begin
      FRows.Delete(i);
      R.Free;
    end;
  end;
end;

constructor TExportMatrix.Create;
begin
  inherited Create;
  FPageMargin:=0;
  FRows:=TFpList.Create;
  SetLength(FColWidth, 0);
  SetLength(FRowHight, 0);
end;

procedure TExportMatrix.Clear;
var
  i:integer;
begin
  for i:=0 to FRows.Count-1 do
    TExportRow(FRows[i]).Free;
  FRows.Clear;
  SetLength(FColWidth, 0);
  SetLength(FRowHight, 0);
end;

destructor TExportMatrix.Destroy;
begin
  Clear;
  FreeAndNil(FRows);
  inherited Destroy;
end;

function TExportMatrix.ExportObject(AObj: TfrView): TExportObject;
var
  R: TExportRow;
begin
{  Result:=nil;
  if (AObj is TfrMemoView) or ((AObj is TfrPictureView) and FExportImages) then
  begin}
    R:=FindRow(AObj.Y);
    Result:=R.ExportObject(AObj);
    Result.Top:=R.Top;
//  end
end;

procedure TExportMatrix.PrepareData;
var
  i: Integer;
begin
  SortRows;
  if FDeleteEmptyRow then
    DoDeleteEmptyRow;

  UpdateRowHeight;
  UpdateColWidth;
  DoPostitionObject;

  if FMergeCell then
    DoMergeCell;

  for i:=Length(FColWidth) - 1 downto 1 do
    FColWidth[i]:=FColWidth[i] - FColWidth[i - 1];

  for i:=Length(FRowHight) - 1 downto 1 do
    FRowHight[i]:=FRowHight[i] - FRowHight[i - 1];
end;

procedure TExportMatrix.NewPage;
var
  R: TExportRow;
  i: Integer;
  imax: Integer;
  FTop: Integer;
  FHeight: Integer;
  C: TExportObject;
  O: TExportObject;
begin
  if FRows.Count = 0 then exit;

  FTop:=0;
  imax:=0;
  FHeight:=0;
  for i:=0 to FRows.Count-1 do
  begin
    R:=TExportRow(FRows[i]);
    if R.Top > FTop then
    begin
      FTop:=R.Top;
      imax:=i;
    end;
  end;

  R:=TExportRow(FRows[imax]);

  for i:=0 to R.FCells.Count-1 do
  begin
    C:=TExportObject(R.FCells[i]);
    if C.Height > FHeight then
      FHeight:=C.Height;
  end;

  R:=FindRow(FTop + FHeight);
  R.FTop:=FTop + FHeight + 1;

  FPageMargin:=R.FTop + FHeight + 1;
end;

end.

