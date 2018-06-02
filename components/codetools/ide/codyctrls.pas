{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    LCL controls for Cody.
}
unit CodyCtrls;

{$mode objfpc}{$H+}

interface

uses
  types, math, typinfo, contnrs, Classes, SysUtils, FPCanvas, FPimage,
  LazLoggerBase, ComCtrls, Controls, Graphics, LCLType, Forms, LCLIntf, GraphType;

type

  { TCodyTreeView }

  TCodyTreeView = class(TTreeView)
  public
    procedure FreeNodeData;
  end;

const
  FullCircle16 = 360*16;
  DefaultCategoryGapDegree16 = 0.02*FullCircle16;
  DefaultFirstCategoryDegree16 = 0;
  DefaultCategoryMinSize = 1.0;
  DefaultItemSize = 1.0;
type
  TCustomCircleDiagramControl = class;
  TCircleDiagramCategory = class;

  { TCircleDiagramItem }

  TCircleDiagramItem = class(TPersistent)
  private
    FCaption: TCaption;
    FCategory: TCircleDiagramCategory;
    FEndDegree16: single;
    FSize: single;
    FStartDegree16: single;
    procedure SetCaption(AValue: TCaption);
    procedure SetSize(AValue: single);
    procedure UpdateLayout;
  public
    Data: Pointer; // free to use by user
    constructor Create(TheCategory: TCircleDiagramCategory);
    destructor Destroy; override;
    property Category: TCircleDiagramCategory read FCategory;
    property Caption: TCaption read FCaption write SetCaption;
    property Size: single read FSize write SetSize default DefaultItemSize; // scaled to fit
    property StartDegree16: single read FStartDegree16; // 360*16 = one full circle, 0 at 3o'clock
    property EndDegree16: single read FEndDegree16;     // 360*16 = one full circle, 0 at 3o'clock
  end;

  { TCircleDiagramCategory }

  TCircleDiagramCategory = class(TPersistent)
  private
    FCaption: TCaption;
    FColor: TColor;
    FDiagram: TCustomCircleDiagramControl;
    FEndDegree16: single;
    FMinSize: single;
    fItems: TFPList; // list of TCircleDiagramItem
    FSize: single;
    FStartDegree16: single;
    function GetItems(Index: integer): TCircleDiagramItem;
    procedure SetCaption(AValue: TCaption);
    procedure SetColor(AValue: TColor);
    procedure SetMinSize(AValue: single);
    procedure UpdateLayout;
    procedure Invalidate;
    procedure InternalRemoveItem(Item: TCircleDiagramItem);
  public
    Data: Pointer; // free to use by user
    constructor Create(TheDiagram: TCustomCircleDiagramControl);
    destructor Destroy; override;
    procedure Clear;
    function InsertItem(Index: integer; aCaption: string): TCircleDiagramItem;
    function AddItem(aCaption: string): TCircleDiagramItem;
    property Diagram: TCustomCircleDiagramControl read FDiagram;
    property Caption: TCaption read FCaption write SetCaption;
    property MinSize: single read FMinSize write SetMinSize default DefaultCategoryMinSize; // scaled to fit
    function Count: integer;
    property Items[Index: integer]: TCircleDiagramItem read GetItems; default;
    property Color: TColor read FColor write SetColor;
    property Size: single read FSize;
    property StartDegree16: single read FStartDegree16; // 360*16 = one full circle, 0 at 3o'clock
    property EndDegree16: single read FEndDegree16;     // 360*16 = one full circle, 0 at 3o'clock
  end;

  TCircleDiagramCtrlFlag = (
    cdcNeedUpdateLayout
    );
  TCircleDiagramCtrlFlags = set of TCircleDiagramCtrlFlag;

  { TCustomCircleDiagramControl }

  TCustomCircleDiagramControl = class(TCustomControl)
  private
    FCategoryGapDegree16: single;
    FCenter: TPoint;
    FCenterCaption: TCaption;
    FCenterCaptionRect: TRect;
    FFirstCategoryDegree16: single;
    fCategories: TObjectList; // list of TCircleDiagramCategory
    FInnerRadius: single;
    FOuterRadius: single;
    fUpdateLock: integer;
    fFlags: TCircleDiagramCtrlFlags;
    function GetCategories(Index: integer): TCircleDiagramCategory;
    procedure SetCategoryGapDegree16(AValue: single);
    procedure SetCenterCaption(AValue: TCaption);
    procedure SetFirstCategoryDegree16(AValue: single);
    procedure InternalRemoveCategory(Category: TCircleDiagramCategory);
  protected
    //procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    //procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure CreateWnd; override;
    procedure UpdateScrollBar;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;

    //procedure MouseDown(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;
    //procedure MouseMove(Shift:TShiftState; X,Y:integer);  override;
    //procedure MouseUp(Button:TMouseButton; Shift:TShiftState; X,Y:integer); override;

    //procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    //procedure HandleStandardKeys(var Key: Word; Shift: TShiftState); virtual;
    //procedure HandleKeyUp(var Key: Word; Shift: TShiftState); virtual;

    procedure Paint; override;
    procedure DrawCategory(i: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property CenterCaption: TCaption read FCenterCaption write SetCenterCaption;
    procedure Clear;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure UpdateLayout;
    procedure EraseBackground({%H-}DC: HDC); override;
    function InsertCategory(Index: integer; aCaption: TCaption): TCircleDiagramCategory;
    function AddCategory(aCaption: TCaption): TCircleDiagramCategory;
    function IndexOfCategory(aCaption: TCaption): integer;
    function FindCategory(aCaption: TCaption): TCircleDiagramCategory;
    property CategoryGapDegree16: single read FCategoryGapDegree16 write SetCategoryGapDegree16 default DefaultCategoryGapDegree16; // 360*16 = one full circle, 0 at 3o'clock
    property FirstCategoryDegree16: single read FFirstCategoryDegree16 write SetFirstCategoryDegree16 default DefaultFirstCategoryDegree16; // 360*16 = one full circle, 0 at 3o'clock
    function CategoryCount: integer;
    property Categories[Index: integer]: TCircleDiagramCategory read GetCategories; default;
    property Color default clWhite;
    // computed values
    property CenterCaptionRect: TRect read FCenterCaptionRect;
    property Center: TPoint read FCenter;
    property InnerRadius: single read FInnerRadius;
    property OuterRadius: single read FOuterRadius;

    // debugging
    procedure WriteDebugReport(Msg: string);
  end;

  { TCircleDiagramControl }

  TCircleDiagramControl = class(TCustomCircleDiagramControl)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnShowHint;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;


// misc
procedure FreeTVNodeData(TV: TCustomTreeView);

// diagram
procedure RingSector(Canvas: TFPCustomCanvas; x1, y1, x2, y2: integer;
  InnerSize: single; StartAngle16, EndAngle16: integer); overload;
procedure RingSector(Canvas: TFPCustomCanvas; x1, y1, x2, y2,
  InnerSize, StartAngle, EndAngle: single); overload;

implementation

procedure FreeTVNodeData(TV: TCustomTreeView);
var
  Node: TTreeNode;
begin
  TV.BeginUpdate;
  Node:=TV.Items.GetFirstNode;
  while Node<>nil do begin
    if Node.Data<>nil then begin
      TObject(Node.Data).Free;
      Node.Data:=nil;
    end;
    Node:=Node.GetNext;
  end;
  TV.EndUpdate;
end;

procedure RingSector(Canvas: TFPCustomCanvas; x1, y1, x2, y2: integer;
  InnerSize: single; StartAngle16, EndAngle16: integer);
begin
  RingSector(Canvas,single(x1),single(y1),single(x2),single(y2),InnerSize,
    single(StartAngle16)/16,single(EndAngle16)/16);
end;

procedure RingSector(Canvas: TFPCustomCanvas; x1, y1, x2, y2, InnerSize, StartAngle,
  EndAngle: single);
var
  OuterCnt: integer;
  centerx, centery: single;
  i: Integer;
  Ang: single;
  OuterRadiusX, OuterRadiusY, InnerRadiusX, InnerRadiusY: single;
  Points: array of TPoint;
  j: Integer;
begin
  OuterCnt:=Round(SQRT((Abs(x2-x1)+Abs(y2-y1))*Abs(EndAngle-StartAngle)/FullCircle16)+0.5);
  centerx:=(x1+x2)/2;
  centery:=(y1+y2)/2;
  OuterRadiusX:=(x2-x1)/2;
  OuterRadiusY:=(y2-y1)/2;
  InnerRadiusX:=OuterRadiusX*InnerSize;
  InnerRadiusY:=OuterRadiusY*InnerSize;
  SetLength(Points,OuterCnt*2+2);
  j:=0;
  // outer arc
  for i:=0 to OuterCnt do begin
    Ang:=StartAngle+((EndAngle-StartAngle)/OuterCnt)*single(i);
    Ang:=(Ang/FullCircle16)*2*pi;
    Points[j].x:=round(centerx+cos(Ang)*OuterRadiusX);
    Points[j].y:=round(centery-sin(Ang)*OuterRadiusY);
    inc(j);
  end;
  // inner arc
  for i:=OuterCnt downto 0 do begin
    Ang:=StartAngle+((EndAngle-StartAngle)/OuterCnt)*single(i);
    Ang:=(Ang/FullCircle16)*2*pi;
    Points[j].x:=round(centerx+cos(Ang)*InnerRadiusX);
    Points[j].y:=round(centery-sin(Ang)*InnerRadiusY);
    inc(j);
  end;
  Canvas.Polygon(Points);
  SetLength(Points,0);
end;

{ TCodyTreeView }

procedure TCodyTreeView.FreeNodeData;
begin
  FreeTVNodeData(Self);
end;

{ TCustomCircleDiagramControl }

procedure TCustomCircleDiagramControl.SetCategoryGapDegree16(AValue: single);
begin
  if AValue<0 then AValue:=0;
  if AValue>0.3 then AValue:=0.3;
  if FCategoryGapDegree16=AValue then Exit;
  FCategoryGapDegree16:=AValue;
  UpdateLayout;
end;

function TCustomCircleDiagramControl.GetCategories(Index: integer
  ): TCircleDiagramCategory;
begin
  Result:=TCircleDiagramCategory(fCategories[Index]);
end;

procedure TCustomCircleDiagramControl.SetCenterCaption(AValue: TCaption);
begin
  if FCenterCaption=AValue then Exit;
  FCenterCaption:=AValue;
  UpdateLayout;
end;

procedure TCustomCircleDiagramControl.SetFirstCategoryDegree16(AValue: single);
begin
  if FFirstCategoryDegree16=AValue then Exit;
  FFirstCategoryDegree16:=AValue;
  UpdateLayout;
end;

procedure TCustomCircleDiagramControl.InternalRemoveCategory(
  Category: TCircleDiagramCategory);
begin
  fCategories.Remove(Category);
  UpdateLayout;
end;

procedure TCustomCircleDiagramControl.CreateWnd;
begin
  inherited CreateWnd;
  UpdateScrollBar;
end;

procedure TCustomCircleDiagramControl.UpdateScrollBar;
begin

end;

procedure TCustomCircleDiagramControl.DoSetBounds(ALeft, ATop, AWidth,
  AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateLayout;
  UpdateScrollBar;
end;

procedure TCustomCircleDiagramControl.Paint;
var
  i: Integer;
begin
  inherited Paint;
  if cdcNeedUpdateLayout in fFlags then
    UpdateLayout;

  // background
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=Color;
  Canvas.FillRect(ClientRect);

  Canvas.Brush.Color:=clRed;

  // draw categories
  for i:=0 to CategoryCount-1 do
    DrawCategory(i);

  // center caption
  Canvas.Brush.Style:=bsSolid;
  Canvas.Brush.Color:=clNone;
  Canvas.TextOut(FCenterCaptionRect.Left,FCenterCaptionRect.Top,CenterCaption);
end;

procedure TCustomCircleDiagramControl.DrawCategory(i: integer);
var
  Cat: TCircleDiagramCategory;
begin
  Cat:=Categories[i];
  Canvas.Brush.Color:=Cat.Color;
  RingSector(Canvas,Center.X-OuterRadius,Center.Y-OuterRadius,
    Center.X+OuterRadius,Center.Y+OuterRadius,
    single(InnerRadius)/single(OuterRadius),
    Cat.StartDegree16,Cat.EndDegree16);
end;

constructor TCustomCircleDiagramControl.Create(AOwner: TComponent);
begin
  BeginUpdate;
  try
    inherited Create(AOwner);
    fCategories:=TObjectList.create(true);
    FFirstCategoryDegree16:=DefaultFirstCategoryDegree16;
    FCategoryGapDegree16:=DefaultCategoryGapDegree16;
    Color:=clWhite;
  finally
    EndUpdate;
  end;
end;

destructor TCustomCircleDiagramControl.Destroy;
begin
  BeginUpdate; // disable updates
  Clear;
  FreeAndNil(fCategories);
  inherited Destroy;
end;

procedure TCustomCircleDiagramControl.Clear;
begin
  if CategoryCount=0 then exit;
  BeginUpdate;
  try
    while CategoryCount>0 do
      fCategories.Delete(CategoryCount-1);
  finally
    EndUpdate;
  end;
end;

procedure TCustomCircleDiagramControl.BeginUpdate;
begin
  inc(fUpdateLock);
end;

procedure TCustomCircleDiagramControl.EndUpdate;
begin
  if fUpdateLock=0 then
    raise Exception.Create('TCustomCircleDiagramControl.EndUpdate');
  dec(fUpdateLock);
  if fUpdateLock=0 then begin
    if cdcNeedUpdateLayout in fFlags then
      UpdateLayout;
  end;
end;

procedure TCustomCircleDiagramControl.UpdateLayout;
var
  aSize: TSize;
  aCategory: TCircleDiagramCategory;
  i: Integer;
  j: Integer;
  TotalSize: Single;
  CurCategoryDegree: Single;
  GapDegree: Single;
  TotalItemDegree: Single;
  Item: TCircleDiagramItem;
  CurItemDegree: Single;
begin
  if (fUpdateLock>0) or (not IsVisible) or (not HandleAllocated) then begin
    Include(fFlags,cdcNeedUpdateLayout);
    exit;
  end;
  Exclude(fFlags,cdcNeedUpdateLayout);

  // center caption
  FCenter:=Point(ClientWidth div 2,ClientHeight div 2);
  aSize:=Canvas.TextExtent(CenterCaption);
  FCenterCaptionRect:=Bounds(FCenter.X-(aSize.cx div 2),FCenter.Y-(aSize.cy div 2)
    ,aSize.cx,aSize.cy);

  // radius
  fInnerRadius:=0.24*Min(ClientWidth,ClientHeight);
  fOuterRadius:=1.2*InnerRadius;

  // degrees
  TotalSize:=0.0;
  CurCategoryDegree:=FirstCategoryDegree16;
  if CategoryCount>0 then begin
    // calculate TotalSize
    for i:=0 to CategoryCount-1 do begin
      aCategory:=Categories[i];
      aCategory.FSize:=0;
      for j:=0 to aCategory.Count-1 do
        aCategory.FSize+=aCategory[j].Size;
      aCategory.FSize:=Max(aCategory.FSize,aCategory.MinSize);
      TotalSize+=aCategory.FSize;
    end;

    // calculate degrees
    GapDegree:=Min(CategoryGapDegree16,(0.8/CategoryCount)*FullCircle16);
    TotalItemDegree:=FullCircle16-(GapDegree*CategoryCount);
    for i:=0 to CategoryCount-1 do begin
      aCategory:=Categories[i];
      aCategory.FStartDegree16:=CurCategoryDegree;
      if TotalSize>0 then
        CurCategoryDegree+=TotalItemDegree*aCategory.Size/TotalSize;
      aCategory.FEndDegree16:=CurCategoryDegree;

      // item degrees
      CurItemDegree:=aCategory.StartDegree16;
      for j:=0 to aCategory.Count-1 do begin
        Item:=aCategory[j];

        Item.FStartDegree16:=CurItemDegree;
        if aCategory.Size>0 then
          CurItemDegree+=(aCategory.EndDegree16-aCategory.StartDegree16)*Item.Size/aCategory.Size;
        Item.FEndDegree16:=CurItemDegree;
      end;

      CurCategoryDegree+=GapDegree;
    end;
  end;

  Invalidate;
  WriteDebugReport('TCustomCircleDiagramControl.UpdateLayout');
end;

procedure TCustomCircleDiagramControl.EraseBackground(DC: HDC);
begin
  // do not erase background, Paint will paint the whole area
end;

function TCustomCircleDiagramControl.InsertCategory(Index: integer;
  aCaption: TCaption): TCircleDiagramCategory;
begin
  Result:=TCircleDiagramCategory.Create(Self);
  Result.Caption:=aCaption;
  fCategories.Insert(Index,Result);
end;

function TCustomCircleDiagramControl.AddCategory(aCaption: TCaption
  ): TCircleDiagramCategory;
begin
  Result:=InsertCategory(CategoryCount,aCaption);
end;

function TCustomCircleDiagramControl.IndexOfCategory(aCaption: TCaption
  ): integer;
begin
  Result:=CategoryCount-1;
  while Result>=0 do begin
    if Categories[Result].Caption=aCaption then exit;
    dec(Result);
  end;
end;

function TCustomCircleDiagramControl.FindCategory(aCaption: TCaption
  ): TCircleDiagramCategory;
var
  i: Integer;
begin
  i:=IndexOfCategory(aCaption);
  if i>=0 then
    Result:=Categories[i]
  else
    Result:=nil;
end;

function TCustomCircleDiagramControl.CategoryCount: integer;
begin
  Result:=fCategories.Count;
end;

procedure TCustomCircleDiagramControl.WriteDebugReport(Msg: string);
var
  aCat: TCircleDiagramCategory;
  i: Integer;
  j: Integer;
  Item: TCircleDiagramItem;
begin
  DebugLn([Msg,' CategoryCount=',CategoryCount]);
  for i:=0 to CategoryCount-1 do begin
    aCat:=Categories[i];
    debugln(['  Category: ',i,'/',CategoryCount,' ',aCat.Caption,
      ' MinSize=',aCat.MinSize,
      ' Size=',aCat.Size,
      ' Start=',round(aCat.StartDegree16),' End=',round(aCat.EndDegree16)]);
    for j:=0 to aCat.Count-1 do begin
      Item:=aCat.Items[j];
      debugln(['    Item: ',j,'/',aCat.Count,' ',Item.Caption,
        ' Size=',Item.Size,
        ' Start=',round(Item.StartDegree16),
        ' End=',round(Item.EndDegree16)]);
    end;
  end;
end;

{ TCircleDiagramCategory }

procedure TCircleDiagramCategory.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

procedure TCircleDiagramCategory.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  Invalidate;
end;

function TCircleDiagramCategory.GetItems(Index: integer): TCircleDiagramItem;
begin
  Result:=TCircleDiagramItem(fItems[Index]);
end;

procedure TCircleDiagramCategory.SetMinSize(AValue: single);
begin
  if AValue<0 then AValue:=0;
  if FMinSize=AValue then Exit;
  FMinSize:=AValue;
  UpdateLayout;
end;

procedure TCircleDiagramCategory.UpdateLayout;
begin
  if Diagram<>nil then
    Diagram.UpdateLayout;
end;

procedure TCircleDiagramCategory.Invalidate;
begin
  if Diagram<>nil then
    Diagram.Invalidate;
end;

procedure TCircleDiagramCategory.InternalRemoveItem(Item: TCircleDiagramItem);
begin
  Item.FCategory:=nil;
  fItems.Remove(Item);
  UpdateLayout;
end;

constructor TCircleDiagramCategory.Create(
  TheDiagram: TCustomCircleDiagramControl);
begin
  FDiagram:=TheDiagram;
  fItems:=TFPList.Create;
  FMinSize:=DefaultCategoryMinSize;
end;

destructor TCircleDiagramCategory.Destroy;
begin
  if Diagram<>nil then
    Diagram.InternalRemoveCategory(Self);
  Clear;
  FreeAndNil(fItems);
  inherited Destroy;
end;

procedure TCircleDiagramCategory.Clear;
begin
  if Count=0 then exit;
  if Diagram<>nil then
    Diagram.BeginUpdate;
  try
    while Count>0 do
      Items[Count-1].Free;
  finally
    if Diagram<>nil then
      Diagram.EndUpdate;
  end;
end;

function TCircleDiagramCategory.InsertItem(Index: integer; aCaption: string
  ): TCircleDiagramItem;
begin
  Result:=TCircleDiagramItem.Create(Self);
  Result.Caption:=aCaption;
  fItems.Insert(Index,Result);
end;

function TCircleDiagramCategory.AddItem(aCaption: string): TCircleDiagramItem;
begin
  Result:=InsertItem(Count,aCaption);
end;

function TCircleDiagramCategory.Count: integer;
begin
  Result:=fItems.Count;
end;

{ TCircleDiagramItem }

procedure TCircleDiagramItem.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  UpdateLayout;
end;

procedure TCircleDiagramItem.SetSize(AValue: single);
begin
  if AValue<0 then AValue:=0;
  if FSize=AValue then Exit;
  FSize:=AValue;
  UpdateLayout;
end;

procedure TCircleDiagramItem.UpdateLayout;
begin
  if Category<>nil then
    Category.UpdateLayout;
end;

constructor TCircleDiagramItem.Create(TheCategory: TCircleDiagramCategory);
begin
  FCategory:=TheCategory;
  FSize:=DefaultItemSize;
end;

destructor TCircleDiagramItem.Destroy;
begin
  if Category<>nil then
    Category.InternalRemoveItem(Self);
  inherited Destroy;
end;

end.

