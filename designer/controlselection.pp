{/***************************************************************************
                             ControlSelection.pp   
                             -------------------
                         cointains selected controls.


                 Initial Revision  : Mon June 19 23:15:32 CST 2000


 ***************************************************************************/

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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit ControlSelection;

{$mode objfpc}{$H+}

interface

{ $DEFINE VerboseDesigner}

uses
  Classes, LCLLinux, LCLType, Controls, Forms, GraphType, Graphics, SysUtils,
  EnvironmentOpts, DesignerProcs;

type
  EGenException = class(Exception);
  
  TGrabberMoveEvent = procedure(Sender: TObject; dx, dy: Integer) of object;

  TGrabIndex = 0..7;

  TGrabPosition = (gpTop, gpBottom, gpLeft, gpRight);
  TGrabPositions = set of TGrabPosition;

  // A TGrabber is one of the 8 small black rectangles at the boundaries of
  // a selection
  TGrabber = class
  private
    FPositions: TGrabPositions;
    FHeight: integer;
    FTop: integer;
    FWidth: integer;
    FLeft: integer;
    FOldLeft: integer;
    FOldTop: integer;
    FOldWidth: integer;
    FOldHeight: integer;
    FGrabIndex: TGrabIndex;
    FCursor: TCursor;
  public
    property Positions: TGrabPositions read FPositions write FPositions;
    property Left:integer read FLeft write FLeft;
    property Top:integer read FTop write FTop;
    property Width:integer read FWidth write FWidth;
    property Height:integer read FHeight write FHeight;
    property OldLeft:integer read FOldLeft write FOldLeft;
    property OldTop:integer read FOldTop write FOldTop;
    property OldWidth:integer read FOldWidth write FOldWidth;
    property OldHeight:integer read FOldHeight write FOldHeight;
    property GrabIndex: TGrabIndex read FGrabIndex write FGrabIndex;
    property Cursor: TCursor read FCursor write FCursor;
    procedure SaveBounds;
  end;


  TSelectedControl = class
  private
    FComponent:TComponent;
    FOldLeft: integer;
    FOldTop: integer;
    FOldWidth: integer;
    FOldHeight: integer;
    FOldFormRelativeLeftTop: TPoint;
    function GetLeft: integer;
    procedure SetLeft(ALeft: integer);
    function GetTop: integer;
    procedure SetTop(ATop: integer);
    function GetWidth: integer;
    procedure SetWidth(AWidth: integer);
    function GetHeight: integer;
    procedure SetHeight(AHeight: integer);
  public
    constructor Create(AComponent:TComponent);
    destructor Destroy; override;
    property Component:TComponent read FComponent write FComponent;
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OldLeft:integer read FOldLeft write FOldLeft;
    property OldTop:integer read FOldTop write FOldTop;
    property OldWidth:integer read FOldWidth write FOldWidth;
    property OldHeight:integer read FOldHeight write FOldHeight;
    property OldFormRelativeLeftTop: TPoint
      read FOldFormRelativeLeftTop write FOldFormRelativeLeftTop;
    function ParentForm: TCustomForm;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer);
    procedure SetFormRelativeBounds(ALeft, ATop, AWidth, AHeight: integer);
    procedure SaveBounds;
    function IsTopLvl: boolean;
  end;

  TComponentAlignment = (csaNone, csaSides1, csaCenters, csaSides2,
      csaCenterInWindow, csaSpaceEqually, csaSide1SpaceEqually,
      csaSide2SpaceEqually);
  TComponentSizing = (cssNone, cssShrinkToSmallest, cssGrowToLargest, cssFixed);
  TSelectionSortCompare = function(Index1, Index2: integer): integer of object;
  
  TControlSelState = (cssOnlyNonVisualNeedsUpdate,
                      cssOnlyVisualNeedsUpdate,
                      cssBoundsNeedsUpdate,
                      cssBoundsNeedsSaving);
  TControlSelStates = set of TControlSelState;
  
  TNearestInt = record
    Level: integer;
    Nearest: integer;
    Valid: boolean;
  end;
  
  TControlSelection = class(TObject)
  private
    FControls: TList;  // list of TSelectedComponent

    // current bounds of the selection (only valid if Count>0)
    // These are the values set by the user
    // But due to snapping and lcl aligning the components can have other bounds
    FLeft: Integer;
    FSavedHeight: integer;
    FSavedLeft: integer;
    FSavedTop: integer;
    FSavedWidth: integer;
    FSnapping: boolean;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    // These are the real bounds of the selection (only valid if Count>0)
    FRealLeft: integer;
    FRealTop: integer;
    FRealWidth: integer;
    FRealHeight: integer;
    // saved bounds of the selection (only valid if Count>0)
    FOldLeft: integer;
    FOldTop: integer;
    FOldWidth: integer;
    FOldHeight: integer;

    FCustomForm: TCustomForm;
    FGrabbers: array[TGrabIndex] of TGrabber;
    FGrabberSize: integer;
    FGrabberColor: TColor;
    FMarkerSize: integer;
    FMarkerColor: integer;
    FActiveGrabber:TGrabber;
    FRubberBandBounds:TRect;
    FRubberbandActive: boolean;
    FVisible:boolean;
    FUpdateLock: integer;
    FChangedDuringLock: boolean;
    FResizeLockCount: integer;
    FNotSaveBounds: boolean;
    FStates: TControlSelStates;
    FOnlyNonVisualComponentsSelected: boolean;
    FOnlyVisualComponentsSelected: boolean;

    FOnChange: TNotifyEvent;

    procedure SetCustomForm;
    function GetGrabbers(AGrabIndex:TGrabIndex): TGrabber;
    procedure SetGrabbers(AGrabIndex:TGrabIndex; const AGrabber: TGrabber);
    procedure SetGrabberSize(const NewSize: integer);
    procedure DoChange;
    procedure SetSnapping(const AValue: boolean);
    procedure SetVisible(const Value: Boolean);
    function GetItems(Index:integer):TSelectedControl;
    procedure SetItems(Index:integer; ASelectedControl:TSelectedControl);
    procedure SetActiveGrabber(AGrabber:TGrabber);
    procedure SetRubberBandBounds(ARect:TRect);
    function CompareInts(i1, i2: integer): integer;
    function CompareLeft(Index1, Index2: integer): integer;
    function CompareTop(Index1, Index2: integer): integer;
    function CompareRight(Index1, Index2: integer): integer;
    function CompareBottom(Index1, Index2: integer): integer;
    function CompareHorCenter(Index1, Index2: integer): integer;
    function CompareVertCenter(Index1, Index2: integer): integer;
  protected
    procedure AdjustGrabbers;
    procedure DoApplyUserBounds;
    procedure UpdateRealBounds;

    // snapping
    function CleanGridSizeX: integer;
    function CleanGridSizeY: integer;
    procedure ImproveNearestInt(var NearestInt: TNearestInt; Candidate: integer);
    procedure FindNearestGridX(var NearestInt: TNearestInt);
    procedure FindNearestGridY(var NearestInt: TNearestInt);
    procedure FindNearestLeftGuideLine(var NearestInt: TNearestInt);
    procedure FindNearestRightGuideLine(var NearestInt: TNearestInt);
    procedure FindNearestTopGuideLine(var NearestInt: TNearestInt);
    procedure FindNearestBottomGuideLine(var NearestInt: TNearestInt);
    procedure FindNearestClientLeftRight(var NearestInt: TNearestInt);
    procedure FindNearestClientTopBottom(var NearestInt: TNearestInt);
    procedure FindNearestOldLeft(var NearestInt: TNearestInt);
    procedure FindNearestOldRight(var NearestInt: TNearestInt);
    procedure FindNearestOldTop(var NearestInt: TNearestInt);
    procedure FindNearestOldBottom(var NearestInt: TNearestInt);
    function FindNearestSnapLeft(ALeft, AWidth: integer): integer;
    function FindNearestSnapTop(ATop, AHeight: integer): integer;
    function GetLeftGuideLine(var ALine: TRect): boolean;
    function GetRightGuideLine(var ALine: TRect): boolean;
    function GetTopGuideLine(var ALine: TRect): boolean;
    function GetBottomGuideLine(var ALine: TRect): boolean;
  public
    constructor Create; 
    destructor Destroy; override;
    property Items[Index:integer]:TSelectedControl read GetItems write SetItems; default;
    function Count:integer;
    procedure BeginUpDate;
    procedure EndUpdate;
    function IndexOf(AComponent:TComponent):integer;
    function Add(AComponent: TComponent):integer;
    function AssignComponent(AComponent:TComponent): boolean;
    procedure Remove(AComponent: TComponent);
    procedure Delete(Index:integer);
    procedure Clear;
    procedure Assign(AControlSelection:TControlSelection);
    function IsSelected(AComponent: TComponent): Boolean;
    function IsOnlySelected(AComponent: TComponent): Boolean;
    procedure SaveBounds;
    
    function IsResizing: boolean;
    procedure BeginResizing;
    procedure EndResizing(ApplyUserBounds: boolean);
    procedure UpdateBounds;
    procedure MoveSelection(dx, dy: integer);
    function MoveSelectionWithSnapping(TotalDX, TotalDY: integer): boolean;
    procedure SizeSelection(dx, dy: integer);  
    procedure SetBounds(NewLeft,NewTop,NewWidth,NewHeight: integer);
      // size all controls depending on ActiveGrabber.
      // if ActiveGrabber=nil then Right,Bottom
    procedure AlignComponents(HorizAlignment,VertAlignment:TComponentAlignment);
    procedure MirrorHorizontal;
    procedure MirrorVertical;
    procedure SizeComponents(HorizSizing: TComponentSizing; AWidth: integer;
          VertSizing: TComponentSizing; AHeight: integer);
    procedure ScaleComponents(Percent: integer);
    property Snapping: boolean read FSnapping write SetSnapping;
    procedure DrawGuideLines(DC: TDesignerDeviceContext);

    property GrabberSize:integer read FGrabberSize write SetGrabberSize;
    property GrabberColor: TColor read FGrabberColor write FGrabberColor;
    procedure DrawGrabbers(DC: TDesignerDeviceContext);
    function GrabberAtPos(X,Y:integer):TGrabber;
    property Grabbers[AGrabIndex:TGrabIndex]:TGrabber read GetGrabbers write SetGrabbers;
    property MarkerSize:integer read FMarkerSize write FMarkerSize;
    property MarkerColor: TColor read FMarkerColor write FMarkerColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure DrawMarker(AComponent:TComponent; DC: TDesignerDeviceContext);
    procedure DrawMarkerAt(ACanvas: TCanvas; ALeft, ATop, AWidth, AHeight: integer);
    property ActiveGrabber:TGrabber read FActiveGrabber write SetActiveGrabber;
    
    property Left:integer read FLeft;
    property Top:integer read FTop;
    property Width:integer read FWidth;
    property Height:integer read FHeight;
    
    property RealLeft:integer read FRealLeft;
    property RealTop:integer read FRealTop;
    property RealWidth:integer read FRealWidth;
    property RealHeight:integer read FRealHeight;

    property SavedLeft:integer read FSavedLeft;
    property SavedTop:integer read FSavedTop;
    property SavedWidth:integer read FSavedWidth;
    property SavedHeight:integer read FSavedHeight;

    property RubberbandBounds:TRect read FRubberbandBounds write SetRubberbandBounds;
    property RubberbandActive: boolean read FRubberbandActive write FRubberbandActive;
    procedure DrawRubberband(DC: TDesignerDeviceContext);
    procedure SelectWithRubberBand(ACustomForm:TCustomForm;
      ClearBefore, ExclusiveOr: boolean; var SelectionChanged: boolean);

    procedure Sort(SortProc: TSelectionSortCompare);
    property Visible:boolean read FVisible write SetVisible;
    function OnlyNonVisualComponentsSelected: boolean;
    function OnlyVisualComponentsSelected: boolean;
  end;



var TheControlSelection: TControlSelection;


implementation


uses
  Math;

const
  GRAB_CURSOR: array[TGrabIndex] of TCursor = (
    crSizeNWSE, crSizeNS, crSizeNESW,
    crSizeWE,             crSizeWE,
    crSizeNESW, crSizeNS, crSizeNWSE
  );

  GRAB_POSITIONS:  array [TGrabIndex] of TGrabPositions = (
    [gpLeft, gpTop   ], [gpTop   ], [gpTop,    gpRight],
    [gpLeft          ],             [          gpRight],
    [gpLeft, gpBottom], [gpBottom], [gpBottom, gpRight]
  );

{ TGrabber }

procedure TGrabber.SaveBounds;
begin
  FOldLeft:=FLeft;
  FOldTop:=FTop;
  FOldWidth:=FWidth;
  FOldHeight:=FHeight;
end;


{ TSelectedControl }

constructor TSelectedControl.Create(AComponent:TComponent);
begin
  inherited Create;
  FComponent:=AComponent;
end;

destructor TSelectedControl.Destroy;
begin
  inherited Destroy;
end;

function TSelectedControl.ParentForm: TCustomForm;
begin
  if FComponent is TControl then
    Result:=GetParentForm(TControl(FComponent))
  else
    if FComponent.Owner is TCustomForm then
      Result:=TCustomForm(FComponent.Owner)
    else
      Result:=nil;
end;

procedure TSelectedControl.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  if FComponent is TControl then
    TControl(FComponent).SetBounds(ALeft, ATop, AWidth, AHeight)
  else begin
    Left:=ALeft;
    Top:=ATop;
  end;
end;

procedure TSelectedControl.SetFormRelativeBounds(ALeft, ATop, AWidth,
  AHeight: integer);
var
  ParentOffset: TPoint;
begin
  ParentOffset:=GetParentFormRelativeParentClientOrigin(FComponent);
  //writeln('TSelectedControl.SetFormRelativeBounds ',FComponent.Name,
  //  ' Left=',ALeft,' LeftOff=',ParentOffset.X,
  //  ' Top=',ATop,' TopOff=',ParentOffset.Y);
  SetBounds(ALeft-ParentOffset.X,ATop-ParentOffset.Y,AWidth,AHeight);
end;

procedure TSelectedControl.SaveBounds;
begin
  GetComponentBounds(FComponent,FOldLeft,FOldTop,FOldWidth,FOldHeight);
  FOldFormRelativeLeftTop:=GetParentFormRelativeTopLeft(FComponent);
//writeln('[TSelectedControl.SaveBounds] ',Component.Name,':',Component.ClassName
//  ,'  ',FOldLeft,',',FOldTop);
end;

function TSelectedControl.IsTopLvl: boolean;
begin
  Result:=(FComponent is TControl) and (TControl(FComponent).Parent=nil);
end;

function TSelectedControl.GetLeft: integer;
begin
  Result:=GetComponentLeft(FComponent);
end;

procedure TSelectedControl.SetLeft(ALeft: integer);
begin
  if FComponent is TControl then
    TControl(FComponent).Left:=Aleft
  else
    LongRec(FComponent.DesignInfo).Lo:=ALeft;
end;

function TSelectedControl.GetTop: integer;
begin
  Result:=GetComponentTop(FComponent);
end;

procedure TSelectedControl.SetTop(ATop: integer);
begin
  if FComponent is TControl then
    TControl(FComponent).Top:=ATop
  else
    LongRec(FComponent.DesignInfo).Hi:=ATop;
end;

function TSelectedControl.GetWidth: integer;
begin
  Result:=GetComponentWidth(FComponent);
end;

procedure TSelectedControl.SetWidth(AWidth: integer);
begin
  if FComponent is TControl then
    TControl(FComponent).Width:=AWidth
  else
    ;
end;

function TSelectedControl.GetHeight: integer;
begin
  Result:=GetComponentHeight(FComponent);
end;

procedure TSelectedControl.SetHeight(AHeight: integer);
begin
  if FComponent is TControl then
    TControl(FComponent).Height:=AHeight
  else
    ;
end;


{ TControlSelection }

constructor TControlSelection.Create;
var g:TGrabIndex;
begin
  inherited;
  FControls:=TList.Create;
  FGrabberSize:=5;
  FGrabberColor:=clBlack;
  FMarkerSize:=5;
  FMarkerColor:=clDkGray;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do begin
    FGrabbers[g]:=TGrabber.Create;
    FGrabbers[g].Positions:=GRAB_POSITIONS[g];
    FGrabbers[g].GrabIndex:=g;
    FGrabbers[g].Cursor:=GRAB_CURSOR[g];
  end;
  FCustomForm:=nil;
  FActiveGrabber:=nil;
  FUpdateLock:=0;
  FChangedDuringLock:=false;
  FRubberbandActive:=false;
  FNotSaveBounds:=false;
  FStates:=[cssOnlyNonVisualNeedsUpdate,cssOnlyVisualNeedsUpdate];
end;

destructor TControlSelection.Destroy;
var g:TGrabIndex;
begin
  Clear;
  FControls.Free;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do FGrabbers[g].Free;
  inherited Destroy;
end;

procedure TControlSelection.BeginUpDate;
begin
  inc(FUpdateLock);
end;

procedure TControlSelection.EndUpdate;
begin
  if FUpdateLock<=0 then exit;
  dec(FUpdateLock);
  if FUpdateLock=0 then begin
    if cssBoundsNeedsUpdate in FStates then UpdateBounds;
    if cssBoundsNeedsSaving in FStates then SaveBounds;
    if FChangedDuringLock then DoChange;
  end;
end;

procedure TControlSelection.BeginResizing;
begin
  if FResizeLockCount=0 then BeginUpdate;
  inc(FResizeLockCount);
end;

procedure TControlSelection.EndResizing(ApplyUserBounds: boolean);
begin
  if FResizeLockCount<=0 then exit;
  if FResizeLockCount=1 then
    if ApplyUserBounds then DoApplyUserBounds;
  dec(FResizeLockCount);
  if FResizeLockCount>0 then exit;
  EndUpdate;
end;

procedure TControlSelection.SetCustomForm;
var NewCustomForm:TCustomForm;
begin
  if Count>0 then
    NewCustomForm:=Items[0].ParentForm
  else
    NewCustomForm:=nil;
  if NewCustomForm=FCustomForm then exit;
  FCustomForm:=NewCustomForm;
end;

function TControlSelection.GetGrabbers(AGrabIndex:TGrabIndex): TGrabber;
begin
  Result:=FGrabbers[AGrabIndex];
end;

procedure TControlSelection.SetGrabbers(AGrabIndex:TGrabIndex;
  const AGrabber: TGrabber);
begin
  FGrabbers[AGrabIndex]:=AGrabber;
end;

procedure TControlSelection.SetGrabberSize(const NewSize: integer);
begin
  if NewSize=FGrabberSize then exit;
  FGrabberSize:=NewSize;
end;

procedure TControlSelection.UpdateBounds;
begin
  if IsResizing then exit;
  if FUpdateLock>0 then begin
    Include(FStates,cssBoundsNeedsUpdate);
    exit;
  end;
  UpdateRealBounds;
  FLeft:=FRealLeft;
  FTop:=FRealTop;
  FWidth:=FRealWidth;
  FHeight:=FRealHeight;
  Exclude(FStates,cssBoundsNeedsUpdate);
end;

procedure TControlSelection.AdjustGrabbers;
var g:TGrabIndex;
  OutPix, InPix: integer;
begin
  OutPix:=GrabberSize div 2;
  InPix:=GrabberSize-OutPix;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do begin
    if gpLeft in FGrabbers[g].Positions then
      FGrabbers[g].Left:=FRealLeft-OutPix
    else if gpRight in FGrabbers[g].Positions then
      FGrabbers[g].Left:=FRealLeft+FRealWidth-InPix
    else
      FGrabbers[g].Left:=FRealLeft+((FRealWidth-GrabberSize) div 2);
    if gpTop in FGrabbers[g].Positions then
      FGrabbers[g].Top:=FRealTop-OutPix
    else if gpBottom in FGrabbers[g].Positions then
      FGrabbers[g].Top:=FRealTop+FRealHeight-InPix
    else
      FGrabbers[g].Top:=FRealTop+((FRealHeight-GrabberSize) div 2);
    FGrabbers[g].Width:=GrabberSize;
    FGrabbers[g].Height:=GrabberSize;
  end;
end;

procedure TControlSelection.DoApplyUserBounds;
var
  i: integer;
  OldLeftTop: TPoint;
  NewLeft, NewTop, NewRight, NewBottom, NewWidth, NewHeight: integer;
begin
  BeginUpdate;
  if Count=1 then begin
    // single selection
    NewLeft:=FLeft;
    NewTop:=FTop;
    NewRight:=FLeft+FWidth;
    NewBottom:=FTop+FHeight;
    {$IFDEF VerboseDesigner}
    writeln('[TControlSelection.DoApplyUserBounds] S Old=',FOldLeft,',',FOldTop,',',FOldWidth,',',FOldHeight,
    ' User=',FLeft,',',FTop,',',FWidth,',',FHeight);
    {$ENDIF}
    Items[0].SetFormRelativeBounds(
      Min(NewLeft,NewRight),
      Min(NewTop,NewBottom),
      Abs(FWidth),
      Abs(FHeight)
    );
  end else if Count>1 then begin
    // multi selection
    {$IFDEF VerboseDesigner}
    writeln('[TControlSelection.DoApplyUserBounds] M Old=',FOldLeft,',',FOldTop,',',FOldWidth,',',FOldHeight,
    ' User=',FLeft,',',FTop,',',FWidth,',',FHeight);
    {$ENDIF}
    if (FOldWidth<>0) and (FOldHeight<>0) then begin
      for i:=0 to Count-1 do begin
      
        // ToDo: if a parent and a child is selected, only move the parent
      
        OldLeftTop:=Items[i].OldFormRelativeLeftTop;
        NewLeft:=FLeft + (((OldLeftTop.X-FOldLeft) * FWidth) div FOldWidth);
        NewTop:=FTop + (((OldLeftTop.Y-FOldTop) * FHeight) div FOldHeight);
        NewWidth:=(Items[i].OldWidth*FWidth) div FOldWidth;
        NewHeight:=(Items[i].OldHeight*FHeight) div FOldHeight;
        if NewWidth<0 then begin
          NewWidth:=-NewWidth;
          dec(NewLeft,NewWidth);
        end;
        if NewWidth<1 then NewWidth:=1;
        if NewHeight<0 then begin
          NewHeight:=-NewHeight;
          dec(NewTop,NewHeight);
        end;
        if NewHeight<1 then NewHeight:=1;
        Items[i].SetFormRelativeBounds(NewLeft,NewTop,NewWidth,NewHeight);
        {$IFDEF VerboseDesigner}
        writeln('  i=',i,
        ' ',Items[i].Left,',',Items[i].Top,',',Items[i].Width,',',Items[i].Height);
        {$ENDIF}
      end;
    end;
  end;
  UpdateRealBounds;
  EndUpdate;
end;

procedure TControlSelection.UpdateRealBounds;
var i:integer;
  LeftTop:TPoint;
begin
  if FControls.Count>=1 then begin
    LeftTop:=GetParentFormRelativeTopLeft(Items[0].Component);
    FRealLeft:=LeftTop.X;
    FRealTop:=LeftTop.Y;
    FRealHeight:=Items[0].Height;
    FRealWidth:=Items[0].Width;
    for i:=1 to FControls.Count-1 do begin
      LeftTop:=GetParentFormRelativeTopLeft(Items[i].Component);
      if FRealLeft>LeftTop.X then begin
        inc(FRealWidth,FRealLeft-LeftTop.X);
        FRealLeft:=LeftTop.X;
      end;
      if FRealTop>LeftTop.Y then begin
        inc(FRealHeight,FRealTop-LeftTop.Y);
        FRealTop:=LeftTop.Y;
      end;
      FRealWidth:=Max(FRealLeft+FRealWidth,LeftTop.X+Items[i].Width)-FRealLeft;
      FRealHeight:=Max(FRealTop+FRealHeight,LeftTop.Y+Items[i].Height)-FRealTop;
    end;
    AdjustGrabbers;
  end;
end;

function TControlSelection.CleanGridSizeX: integer;
begin
  Result:=EnvironmentOptions.GridSizeX;
  if Result<1 then Result:=1;
end;

function TControlSelection.CleanGridSizeY: integer;
begin
  Result:=EnvironmentOptions.GridSizeY;
  if Result<1 then Result:=1;
end;

procedure TControlSelection.ImproveNearestInt(var NearestInt: TNearestInt;
  Candidate: integer);
begin
  if (not NearestInt.Valid)
  or (Abs(NearestInt.Level-NearestInt.Nearest)>Abs(NearestInt.Level-Candidate))
  then begin
    NearestInt.Valid:=true;
    NearestInt.Nearest:=Candidate;
  end;
end;

procedure TControlSelection.FindNearestGridY(var NearestInt: TNearestInt);
var GridSizeY, NearestGridY: integer;
begin
  if not EnvironmentOptions.SnapToGrid then exit;
  GridSizeY:=CleanGridSizeY;
  // add half GridSizeY, so that rounding is correct
  if NearestInt.Level>=0 then
    NearestGridY:=NearestInt.Level+(GridSizeY div 2)
  else
    NearestGridY:=NearestInt.Level-(GridSizeY div 2);
  // round
  dec(NearestGridY,NearestGridY mod GridSizeY);
  ImproveNearestInt(NearestInt,NearestGridY);
end;

procedure TControlSelection.FindNearestClientLeftRight(
  var NearestInt: TNearestInt);
var MaxDist: integer;
begin
  MaxDist:=(CleanGridSizeX+1) div 2;
  if Abs(NearestInt.Level-0)<MaxDist then
    ImproveNearestInt(NearestInt,0);
  if (FCustomForm<>nil)
  and (Abs(NearestInt.Level-FCustomForm.ClientWidth)<MaxDist) then
    ImproveNearestInt(NearestInt,FCustomForm.ClientWidth);
end;

procedure TControlSelection.FindNearestClientTopBottom(
  var NearestInt: TNearestInt);
var MaxDist: integer;
begin
  MaxDist:=(CleanGridSizeY+1) div 2;
  if Abs(NearestInt.Level-0)<MaxDist then
    ImproveNearestInt(NearestInt,0);
  if (FCustomForm<>nil)
  and (Abs(NearestInt.Level-FCustomForm.ClientHeight)<MaxDist) then
    ImproveNearestInt(NearestInt,FCustomForm.ClientHeight);
end;

procedure TControlSelection.FindNearestOldLeft(var NearestInt: TNearestInt);
var MaxDist: integer;
begin
  MaxDist:=(CleanGridSizeX+1) div 2;
  if Abs(NearestInt.Level-FOldLeft)<MaxDist then
    ImproveNearestInt(NearestInt,FOldLeft);
end;

procedure TControlSelection.FindNearestOldRight(var NearestInt: TNearestInt);
var MaxDist, FOldRight: integer;
begin
  MaxDist:=(CleanGridSizeX+1) div 2;
  FOldRight:=FOldLeft+FOldWidth;
  if Abs(NearestInt.Level-FOldRight)<MaxDist then
    ImproveNearestInt(NearestInt,FOldRight);
end;

procedure TControlSelection.FindNearestOldTop(var NearestInt: TNearestInt);
var MaxDist: integer;
begin
  MaxDist:=(CleanGridSizeY+1) div 2;
  if Abs(NearestInt.Level-FOldTop)<MaxDist then
    ImproveNearestInt(NearestInt,FOldTop);
end;

procedure TControlSelection.FindNearestOldBottom(var NearestInt: TNearestInt);
var MaxDist, FOldBottom: integer;
begin
  MaxDist:=(CleanGridSizeY+1) div 2;
  FOldBottom:=FOldTop+FOldHeight;
  if Abs(NearestInt.Level-FOldBottom)<MaxDist then
    ImproveNearestInt(NearestInt,FOldBottom);
end;

procedure TControlSelection.FindNearestLeftGuideLine(
  var NearestInt: TNearestInt);
var i, CurLeft, MaxDist, CurDist: integer;
  AComponent: TComponent;
begin
  if (not EnvironmentOptions.SnapToGuideLines) or (FCustomForm=nil) then exit;
  // search in all not selected components
  MaxDist:=(CleanGridSizeX+1) div 2;
  for i:=0 to FCustomForm.ComponentCount-1 do begin
    AComponent:=FCustomForm.Components[i];
    if IsSelected(AComponent) then continue;
    CurLeft:=GetParentFormRelativeTopLeft(AComponent).X;
    CurDist:=Abs(CurLeft-NearestInt.Level);
    if CurDist>=MaxDist then continue; // skip components far away
    ImproveNearestInt(NearestInt,CurLeft);
  end;
end;

procedure TControlSelection.FindNearestRightGuideLine(
  var NearestInt: TNearestInt);
var i, CurRight, MaxDist, CurDist: integer;
  AComponent: TComponent;
begin
  if (not EnvironmentOptions.SnapToGuideLines) or (FCustomForm=nil) then exit;
  // search in all not selected components
  MaxDist:=(CleanGridSizeX+1) div 2;
  for i:=0 to FCustomForm.ComponentCount-1 do begin
    AComponent:=FCustomForm.Components[i];
    if IsSelected(AComponent) then continue;
    CurRight:=GetParentFormRelativeTopLeft(AComponent).X
              +GetComponentWidth(AComponent);
    CurDist:=Abs(CurRight-NearestInt.Level);
    if CurDist>=MaxDist then continue; // skip components far away
    ImproveNearestInt(NearestInt,CurRight);
  end;
end;

procedure TControlSelection.FindNearestTopGuideLine(var NearestInt: TNearestInt
  );
var i, CurTop, MaxDist, CurDist: integer;
  AComponent: TComponent;
begin
  if (not EnvironmentOptions.SnapToGuideLines) or (FCustomForm=nil) then exit;
  // search in all not selected components
  MaxDist:=(CleanGridSizeY+1) div 2;
  for i:=0 to FCustomForm.ComponentCount-1 do begin
    AComponent:=FCustomForm.Components[i];
    if IsSelected(AComponent) then continue;
    CurTop:=GetParentFormRelativeTopLeft(AComponent).Y;
    CurDist:=Abs(CurTop-NearestInt.Level);
    if CurDist>=MaxDist then continue; // skip components far away
    ImproveNearestInt(NearestInt,CurTop);
  end;
end;

procedure TControlSelection.FindNearestBottomGuideLine(
  var NearestInt: TNearestInt);
var i, CurBottom, MaxDist, CurDist: integer;
  AComponent: TComponent;
begin
  if (not EnvironmentOptions.SnapToGuideLines) or (FCustomForm=nil) then exit;
  // search in all not selected components
  MaxDist:=(CleanGridSizeY+1) div 2;
  for i:=0 to FCustomForm.ComponentCount-1 do begin
    AComponent:=FCustomForm.Components[i];
    if IsSelected(AComponent) then continue;
    CurBottom:=GetParentFormRelativeTopLeft(AComponent).Y
              +GetComponentHeight(AComponent);
    CurDist:=Abs(CurBottom-NearestInt.Level);
    if CurDist>=MaxDist then continue; // skip components far away
    ImproveNearestInt(NearestInt,CurBottom);
  end;
end;

function TControlSelection.FindNearestSnapLeft(ALeft, AWidth: integer): integer;
var
  NearestLeft, NearestRight: TNearestInt;
begin
  // snap left
  NearestLeft.Level:=ALeft;
  NearestLeft.Valid:=false;
  FindNearestGridX(NearestLeft);
  FindNearestLeftGuideLine(NearestLeft);
  FindNearestClientLeftRight(NearestLeft);
  FindNearestOldLeft(NearestLeft);
  // snap right
  NearestRight.Level:=ALeft+AWidth;
  NearestRight.Valid:=false;
  FindNearestRightGuideLine(NearestRight);
  FindNearestClientLeftRight(NearestRight);
  FindNearestOldRight(NearestRight);
  // combine left and right snap
  if NearestRight.Valid then
    ImproveNearestInt(NearestLeft,NearestRight.Nearest-AWidth);
  // return best snap
  if NearestLeft.Valid then
    Result:=NearestLeft.Nearest
  else
    Result:=ALeft;
end;

function TControlSelection.FindNearestSnapTop(ATop, AHeight: integer): integer;
var
  NearestTop, NearestBottom: TNearestInt;
begin
  // snap top
  NearestTop.Level:=ATop;
  NearestTop.Valid:=false;
  FindNearestGridY(NearestTop);
  FindNearestTopGuideLine(NearestTop);
  FindNearestClientTopBottom(NearestTop);
  FindNearestOldTop(NearestTop);
  // snap bottom
  NearestBottom.Level:=ATop+AHeight;
  NearestBottom.Valid:=false;
  FindNearestBottomGuideLine(NearestBottom);
  FindNearestClientTopBottom(NearestBottom);
  FindNearestOldBottom(NearestBottom);
  // combine top and bottom snap
  if NearestBottom.Valid then
    ImproveNearestInt(NearestTop,NearestBottom.Nearest-AHeight);
  // return best snap
  if NearestTop.Valid then
    Result:=NearestTop.Nearest
  else
    Result:=ATop;
end;

function TControlSelection.GetLeftGuideLine(var ALine: TRect): boolean;
var i, LineTop, LineBottom: integer;
  CRect: TRect;
begin
  Result:=false;
  if FCustomForm=nil then exit;
  for i:=0 to FCustomForm.ComponentCount-1 do begin
    if IsSelected(FCustomForm.Components[i]) then continue;
    CRect:=GetParentFormRelativeBounds(FCustomForm.Components[i]);
    if CRect.Left=FRealLeft then begin
      ALine.Left:=FRealLeft;
      ALine.Right:=ALine.Left;
      LineTop:=Min(Min(Min(FRealTop,
                           FRealTop+FRealHeight),
                           CRect.Top),
                           CRect.Bottom);
      LineBottom:=Max(Max(Max(FRealTop,
                              FRealTop+FRealHeight),
                              CRect.Top),
                              CRect.Bottom);
      if Result then begin
        LineTop:=Min(ALine.Top,LineTop);
        LineBottom:=Max(ALine.Bottom,LineBottom);
      end else
        Result:=true;
      ALine.Top:=LineTop;
      ALine.Bottom:=LineBottom;
    end;
  end;
end;

function TControlSelection.GetRightGuideLine(var ALine: TRect): boolean;
var i, LineTop, LineBottom: integer;
  CRect: TRect;
begin
  Result:=false;
  if FCustomForm=nil then exit;
  for i:=0 to FCustomForm.ComponentCount-1 do begin
    if IsSelected(FCustomForm.Components[i]) then continue;
    CRect:=GetParentFormRelativeBounds(FCustomForm.Components[i]);
    if (CRect.Right=FRealLeft+FRealWidth) then begin
      ALine.Left:=CRect.Right;
      ALine.Right:=ALine.Left;
      LineTop:=Min(Min(Min(FRealTop,
                           FRealTop+FRealHeight),
                           CRect.Top),
                           CRect.Bottom);
      LineBottom:=Max(Max(Max(FRealTop,
                              FRealTop+FRealHeight),
                              CRect.Top),
                              CRect.Bottom);
      if Result then begin
        LineTop:=Min(ALine.Top,LineTop);
        LineBottom:=Max(ALine.Bottom,LineBottom);
      end else
        Result:=true;
      ALine.Top:=LineTop;
      ALine.Bottom:=LineBottom;
    end;
  end;
end;

function TControlSelection.GetTopGuideLine(var ALine: TRect): boolean;
var i, LineLeft, LineRight: integer;
  CRect: TRect;
begin
  Result:=false;
  if FCustomForm=nil then exit;
  for i:=0 to FCustomForm.ComponentCount-1 do begin
    if IsSelected(FCustomForm.Components[i]) then continue;
    CRect:=GetParentFormRelativeBounds(FCustomForm.Components[i]);
    if CRect.Top=FRealTop then begin
      ALine.Top:=FRealTop;
      ALine.Bottom:=ALine.Top;
      LineLeft:=Min(Min(Min(FRealLeft,
                              FRealLeft+FRealWidth),
                              CRect.Left),
                              CRect.Right);
      LineRight:=Max(Max(Max(FRealLeft,
                               FRealLeft+FRealWidth),
                               CRect.Left),
                               CRect.Right);
      if Result then begin
        LineLeft:=Min(ALine.Left,LineLeft);
        LineRight:=Max(ALine.Right,LineRight);
      end else
        Result:=true;
      ALine.Left:=LineLeft;
      ALine.Right:=LineRight;
    end;
  end;
end;

function TControlSelection.GetBottomGuideLine(var ALine: TRect): boolean;
var i, LineLeft, LineRight: integer;
  CRect: TRect;
begin
  Result:=false;
  if FCustomForm=nil then exit;
  for i:=0 to FCustomForm.ComponentCount-1 do begin
    if IsSelected(FCustomForm.Components[i]) then continue;
    CRect:=GetParentFormRelativeBounds(FCustomForm.Components[i]);
    if CRect.Bottom=FRealTop+FRealHeight then begin
      ALine.Top:=CRect.Bottom;
      ALine.Bottom:=ALine.Top;
      LineLeft:=Min(Min(Min(FRealLeft,
                              FRealLeft+FRealWidth),
                              CRect.Left),
                              CRect.Right);
      LineRight:=Max(Max(Max(FRealLeft,
                               FRealLeft+FRealWidth),
                               CRect.Left),
                               CRect.Right);
      if Result then begin
        LineLeft:=Min(ALine.Left,LineLeft);
        LineRight:=Max(ALine.Right,LineRight);
      end else
        Result:=true;
      ALine.Left:=LineLeft;
      ALine.Right:=LineRight;
    end;
  end;
end;

procedure TControlSelection.FindNearestGridX(var NearestInt: TNearestInt);
var GridSizeX, NearestGridX: integer;
begin
  if not EnvironmentOptions.SnapToGrid then exit;
  GridSizeX:=CleanGridSizeX;
  // add half GridSizeX, so that rounding is correct
  if NearestInt.Level>=0 then
    NearestGridX:=NearestInt.Level+(GridSizeX div 2)
  else
    NearestGridX:=NearestInt.Level-(GridSizeX div 2);
  // round
  dec(NearestGridX,NearestGridX mod GridSizeX);
  ImproveNearestInt(NearestInt,NearestGridX);
end;

procedure TControlSelection.DoChange;
begin
  if (FUpdateLock>0) then
    FChangedDuringLock:=true
  else
  begin
    FChangedDuringLock:=false;
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TControlSelection.SetSnapping(const AValue: boolean);
begin
  FSnapping:=AValue;
end;

procedure TControlSelection.SetVisible(const Value: Boolean);
begin
  if FVisible=Value then exit;
  FVisible:=Value;
end;

function TControlSelection.GetItems(Index:integer):TSelectedControl;
begin
  Result:=TSelectedControl(FControls[Index]);
end;

procedure TControlSelection.SetItems(Index:integer;
  ASelectedControl:TSelectedControl);
begin
  FControls[Index]:=ASelectedControl;
end;

procedure TControlSelection.SaveBounds;
var i:integer;
  g:TGrabIndex;
begin
  if FNotSaveBounds then exit;
  if FUpdateLock>0 then begin
    Include(FStates,cssBoundsNeedsSaving);
    exit;
  end;
//writeln('TControlSelection.SaveBounds');
  for i:=0 to FControls.Count-1 do Items[i].SaveBounds;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do FGrabbers[g].SaveBounds;
  FOldLeft:=FRealLeft;
  FOldTop:=FRealTop;
  FOldWidth:=FRealWidth;
  FOldHeight:=FRealHeight;
  Exclude(FStates,cssBoundsNeedsSaving);
end;

function TControlSelection.IsResizing: boolean;
begin
  Result:=FResizeLockCount>0;
end;

procedure TControlSelection.SetActiveGrabber(AGrabber:TGrabber);
begin
  FActiveGrabber:=AGrabber;
end;

function TControlSelection.Count:integer;
begin
  Result:=FControls.Count;
end;

function TControlSelection.IndexOf(AComponent:TComponent):integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result].Component<>AComponent) do dec(Result);
end;

function TControlSelection.Add(AComponent: TComponent):integer;
var NewSelectedControl:TSelectedControl;
begin
  BeginUpdate;
  NewSelectedControl:=TSelectedControl.Create(AComponent);
  if NewSelectedControl.ParentForm<>FCustomForm then Clear;
  Result:=FControls.Add(NewSelectedControl);
  FStates:=FStates+[cssOnlyNonVisualNeedsUpdate,cssOnlyVisualNeedsUpdate];
  if Count=1 then SetCustomForm;
  UpdateBounds;
  SaveBounds;
  EndUpdate;
  DoChange;
end;

function TControlSelection.AssignComponent(AComponent: TComponent): boolean;
begin
  Result:=not IsOnlySelected(AComponent);
  if not Result then exit;
  BeginUpdate;
  Clear;
  Add(AComponent);
  EndUpdate;
end;

procedure TControlSelection.Remove(AComponent: TComponent);
var i:integer;
begin
  i:=IndexOf(AComponent);
  if i>=0 then Delete(i);
end;

// remove a component from the selection
procedure TControlSelection.Delete(Index:integer);
begin
  if Index<0 then exit;
  Items[Index].Free;
  FControls.Delete(Index);
  FStates:=FStates+[cssOnlyNonVisualNeedsUpdate,cssOnlyVisualNeedsUpdate];
  if Count=0 then SetCustomForm;
  UpdateBounds;
  SaveBounds;
  DoChange;
end;

procedure TControlSelection.Clear;
var i:integer;
begin
  for i:=0 to FControls.Count-1 do Items[i].Free;
  FControls.Clear;
  FStates:=FStates+[cssOnlyNonVisualNeedsUpdate,cssOnlyVisualNeedsUpdate];
  FCustomForm:=nil;
  UpdateBounds;
  SaveBounds;
  DoChange;
end;

procedure TControlSelection.Assign(AControlSelection:TControlSelection);
var i:integer;
begin
  if AControlSelection=Self then exit;
  FNotSaveBounds:=true;
  BeginUpdate;
  Clear;
  FControls.Capacity:=AControlSelection.Count;
  for i:=0 to AControlSelection.Count-1 do
    Add(AControlSelection[i].Component);
  SetCustomForm;
  UpdateBounds;
  FNotSaveBounds:=false;
  SaveBounds;
  EndUpdate;
  DoChange;
end;

function TControlSelection.IsSelected(AComponent: TComponent): Boolean;
begin
  Result:=(IndexOf(AComponent)>=0);
end;

function TControlSelection.IsOnlySelected(AComponent: TComponent): Boolean;
begin
  Result:=(Count=1) and (Items[0].Component=AComponent);
end;

procedure TControlSelection.MoveSelection(dx, dy: integer);
begin
  if (Count=0) or (IsResizing) then exit;
  if (dx=0) and (dy=0) then exit;
  writeln('[TControlSelection.MoveSelection] A  ',dx,',',dy);
  BeginResizing;
  writeln('[TControlSelection.MoveSelection] B  ',FResizeLockCount);
  inc(FLeft,dx);
  inc(FTop,dy);
  EndResizing(true);
end;

function TControlSelection.MoveSelectionWithSnapping(TotalDX, TotalDY: integer
  ): boolean;
var
  NewLeft, NewTop: integer;
begin
  Result:=false;
  if (Count=0) or (IsResizing) then exit;
  {$IFDEF VerboseDesigner}
  writeln('[TControlSelection.MoveSelectionWithSnapping] A  ',
    TotalDX,',',TotalDY,' OldBounds=',FLeft,',',FTop,',',FWidth,',',FHeight);
  {$ENDIF}
  NewLeft:=FindNearestSnapLeft(FOldLeft+TotalDX,FWidth);
  NewTop:=FindNearestSnapTop(FOldTop+TotalDY,FHeight);
  if (NewLeft<>FLeft) or (NewTop<>FTop) then begin
    Result:=true;
    BeginResizing;
    FLeft:=NewLeft;
    FTop:=NewTop;
    {$IFDEF VerboseDesigner}
    writeln('[TControlSelection.MoveSelectionWithSnapping] B  ',
      FLeft,',',FTop,',',FWidth,',',FHeight);
    {$ENDIF}
    EndResizing(true);
  end;
end;

procedure TControlSelection.SizeSelection(dx, dy: integer);  
// size all controls depending on ActiveGrabber.
// if ActiveGrabber=nil then Left,Top
var
  GrabberPos:TGrabPositions;
begin
  if (Count=0) or (IsResizing) then exit;
  if (dx=0) and (dy=0) then exit;
  {$IFDEF VerboseDesigner}
  writeln('[TControlSelection.SizeSelection] A  ',dx,',',dy);
  {$ENDIF}
  BeginResizing;
  if FActiveGrabber<>nil then
    GrabberPos:=FActiveGrabber.Positions
  else
    GrabberPos:=[gpRight,gpBottom];
  if [gpTop,gpBottom] * GrabberPos = [] then dy:=0;
  if [gpLeft,gpRight] * GrabberPos = [] then dx:=0;
  if (dx=0) and (dy=0) then exit;
  if gpLeft in GrabberPos then begin
    FLeft:=FLeft+dx;
    FWidth:=FWidth-dx;
  end;
  if gpRight in GrabberPos then begin
    FWidth:=FWidth+dx;
  end;
  if gpTop in GrabberPos then begin
    FTop:=FTop+dy;
    FHeight:=FHeight-dy;
  end;
  if gpBottom in GrabberPos then begin
    FHeight:=FHeight+dy;
  end;
  EndResizing(true);
end;

procedure TControlSelection.SetBounds(NewLeft, NewTop,
  NewWidth, NewHeight: integer);
begin
  if (Count=0) or (IsResizing) then exit;
  BeginResizing;
  FLeft:=NewLeft;
  FTop:=NewTop;
  FWidth:=NewWidth;
  FHeight:=NewHeight;
  EndResizing(true);
end;

function TControlSelection.GrabberAtPos(X,Y:integer):TGrabber;
var g:TGrabIndex;
begin
  if FControls.Count>0 then begin
    {$IFDEF VerboseDesigner}
    writeln('[TControlSelection.GrabberAtPos] ',x,',',y,'  '
    ,FGrabbers[4].Left,',',FGrabbers[4].Top);
    {$ENDIF}
    for g:=Low(TGrabIndex) to High(TGrabIndex) do
      if (FGrabbers[g].Left<=x) and (FGrabbers[g].Top<=y)
      and (FGrabbers[g].Left+FGrabbers[g].Width>x)
      and (FGrabbers[g].Top+FGrabbers[g].Height>y) then begin
        Result:=FGrabbers[g];
        exit;
      end;
  end;
  Result:=nil;
end;

procedure TControlSelection.DrawGrabbers(DC: TDesignerDeviceContext);
var OldBrushColor:TColor;
  g:TGrabIndex;
  Diff: TPoint;
//  OldFormHandle: HDC;
begin
  if (Count=0) or (FCustomForm=nil)
  or IsSelected(FCustomForm) then exit;
  
  Diff:=DC.FormOrigin;
{
writeln('[DrawGrabbers] Form=',FormOrigin.X,',',FormOrigin.Y
   ,' DC=',DCOrigin.X,',',DCOrigin.Y
   ,' Grabber1=',FGrabbers[0].Left,',',FGrabbers[0].Top
   ,' Selection=',FLeft,',',FTop);
}
  DC.Save;
  with DC.Canvas do begin
    OldBrushColor:=Brush.Color;
    Brush.Color:=FGrabberColor;
    for g:=Low(TGrabIndex) to High(TGrabIndex) do
      FillRect(Rect(
         FGrabbers[g].Left-Diff.X
        ,FGrabbers[g].Top-Diff.Y
        ,FGrabbers[g].Left-Diff.X+FGrabbers[g].Width
        ,FGrabbers[g].Top-Diff.Y+FGrabbers[g].Height
      ));
    Brush.Color:=OldbrushColor;
  end;
end;

procedure TControlSelection.DrawMarkerAt(ACanvas: TCanvas;
  ALeft, ATop, AWidth, AHeight: integer);
var OldBrushColor:TColor;
begin
  with ACanvas do begin
    OldBrushColor:=Brush.Color;
    Brush.Color:=FMarkerColor;
    FillRect(Rect(ALeft,ATop,ALeft+MarkerSize,ATop+MarkerSize));
    FillRect(Rect(ALeft,ATop+AHeight-MarkerSize,ALeft+MarkerSize,ATop+AHeight));
    FillRect(Rect(ALeft+AWidth-MarkerSize,ATop,ALeft+AWidth,ATop+MarkerSize));
    FillRect(Rect(ALeft+AWidth-MarkerSize,ATop+AHeight-MarkerSize
                 ,ALeft+AWidth,ATop+AHeight));
    Brush.Color:=OldbrushColor;
  end;
end;

procedure TControlSelection.DrawMarker(AComponent: TComponent;
  DC: TDesignerDeviceContext);
var
  CompLeft, CompTop, CompWidth, CompHeight: integer;
  CompOrigin, DCOrigin: TPoint;
begin
  if (Count<2)
  or (FCustomForm=nil)
  or (AComponent.Owner<>DC.Form)
  or (AComponent is TCustomForm)
  or (not IsSelected(AComponent)) then exit;
  
  GetComponentBounds(AComponent,CompLeft,CompTop,CompWidth,CompHeight);
  CompOrigin:=GetParentFormRelativeParentClientOrigin(AComponent);
  DCOrigin:=DC.FormOrigin;
  CompLeft:=CompLeft+CompOrigin.X-DCOrigin.X;
  CompTop:=CompTop+CompOrigin.Y-DCOrigin.Y;

  DC.Save;
{
writeln('DrawMarker A ',FCustomForm.Name
    ,' Control=',AControl.Name,',',AControlOrigin.X,',',AControlOrigin.Y
    ,' DCxy=',DCOrigin.x,',',DCOrigin.y
    ,' DC=',Hexstr(FCustomForm.Canvas.Handle,8),' ',HexStr(Cardinal(Pointer(FCustomForm)),8)
    );
}
  DrawMarkerAt(DC.Canvas,CompLeft,CompTop,CompWidth,CompHeight);
end;

procedure TControlSelection.DrawRubberband(DC: TDesignerDeviceContext);
var Diff: TPoint;

  procedure DrawInvertFrameRect(x1,y1,x2,y2:integer);
  var i:integer;

    procedure InvertPixel(x,y:integer);
    //var c:TColor;
    begin
      //c:=DC.Canvas.Pixels[x,y];
      //c:=c xor $ffffff;
      //DC.Canvas.Pixels[x,y]:=c;
      DC.Canvas.MoveTo(x-Diff.X,y-Diff.Y);
      DC.Canvas.LineTo(x-Diff.X+1,y-Diff.Y);
    end;

  var OldPenColor: TColor;
  begin
    if x1>x2 then begin i:=x1; x1:=x2; x2:=i; end;
    if y1>y2 then begin i:=y1; y1:=y2; y2:=i; end;
    with DC.Canvas do begin
      OldPenColor:=Brush.Color;
      Pen.Color:=clBlack;
      i:=x1+1;
      while i<x2-1 do begin
        InvertPixel(i,y1);
        InvertPixel(i,y2);
        inc(i,2);
      end;
      i:=y1;
      while i<y2 do begin
        InvertPixel(x1,i);
        InvertPixel(x2,i);
        inc(i,2);
      end;
      Pen.Color:=OldPenColor;
    end;
  end;

// DrawRubberband
begin
  if (FCustomForm=nil) then exit;
  Diff:=DC.FormOrigin;
  DC.Save;
  with FRubberBandBounds do
    DrawInvertFrameRect(Left,Top,Right,Bottom);
end;

procedure TControlSelection.SelectWithRubberBand(ACustomForm:TCustomForm; 
  ClearBefore, ExclusiveOr:boolean; var SelectionChanged: boolean);
var i:integer;

  function ControlInRubberBand(AComponent:TComponent):boolean;
  var ALeft,ATop,ARight,ABottom:integer;
    Origin:TPoint;
  begin
    Origin:=GetParentFormRelativeTopLeft(AComponent);
    ALeft:=Origin.X;
    ATop:=Origin.Y;
    if AComponent is TControl then begin
      ARight:=ALeft+TControl(AComponent).Width;
      ABottom:=ATop+TControl(AComponent).Height;
    end else begin
      ARight:=ALeft+NonVisualCompWidth;
      ABottom:=ATop+NonVisualCompWidth;
    end;
    Result:=(ALeft<FRubberBandBounds.Right)
        and (ATop<FRubberBandBounds.Bottom)
        and (ARight>=FRubberBandBounds.Left)
        and (ABottom>=FRubberBandBounds.Top);
  end;

// SelectWithRubberBand
begin
  SelectionChanged:=false;
  if ClearBefore then begin
    for i:=0 to ACustomForm.ComponentCount-1 do
      if not ControlInRubberBand(ACustomForm.Components[i]) then begin
        if IsSelected(ACustomForm.Components[i]) then begin
          Remove(ACustomForm.Components[i]);
          SelectionChanged:=true;
        end;
      end;
  end;
  for i:=0 to ACustomForm.ComponentCount-1 do
    if ControlInRubberBand(ACustomForm.Components[i]) then begin
      if IsSelected(ACustomForm.Components[i]) then begin
        if ExclusiveOr then begin
          Remove(ACustomForm.Components[i]);
          SelectionChanged:=true;
        end;
      end else begin
        Add(ACustomForm.Components[i]);
        SelectionChanged:=true;
      end;
    end;
end;

procedure TControlSelection.SetRubberBandBounds(ARect:TRect);
var i :integer;
begin
  FRubberBandBounds:=ARect;
  with FRubberBandBounds do begin
    if Right<Left then begin
      i:=Left;
      Left:=Right;
      Right:=i;
    end;
    if Bottom<Top then begin
      i:=Top;
      Top:=Bottom;
      Bottom:=i;
    end;
  end;
end;

function TControlSelection.OnlyNonVisualComponentsSelected: boolean;
var i: integer;
begin
writeln('TControlSelection.OnlyNonVisualComponentsSelected ',cssOnlyNonVisualNeedsUpdate in FStates,' ',FOnlyNonVisualComponentsSelected);
  if cssOnlyNonVisualNeedsUpdate in FStates then begin
    Result:=true;
    for i:=0 to FControls.Count-1 do
      Result:=Result and (not (Items[i].Component is TControl));
    FOnlyNonVisualComponentsSelected:=Result;
    Exclude(FStates,cssOnlyNonVisualNeedsUpdate);
  end else
    Result:=FOnlyNonVisualComponentsSelected;
end;

function TControlSelection.OnlyVisualComponentsSelected: boolean;
var i: integer;
begin
writeln('TControlSelection.OnlyVisualComponentsSelected ',cssOnlyVisualNeedsUpdate in FStates,' ',FOnlyVisualComponentsSelected);
  if cssOnlyVisualNeedsUpdate in FStates then begin
    Result:=true;
    for i:=0 to FControls.Count-1 do
      Result:=Result and (Items[i].Component is TControl);
    FOnlyVisualComponentsSelected:=Result;
    Exclude(FStates,cssOnlyVisualNeedsUpdate);
  end else
    Result:=FOnlyVisualComponentsSelected;
end;

function TControlSelection.CompareInts(i1, i2: integer): integer;
begin
  if i1<i2 then Result:=-1
  else if i1=i2 then Result:=0
  else Result:=1;
end;

function TControlSelection.CompareLeft(Index1, Index2: integer): integer;
begin
  Result:=CompareInts(Items[Index1].Left,Items[Index2].Left);
end;

function TControlSelection.CompareTop(Index1, Index2: integer): integer;
begin
  Result:=CompareInts(Items[Index1].Top,Items[Index2].Top);
end;

function TControlSelection.CompareRight(Index1, Index2: integer): integer;
begin
  Result:=CompareInts(Items[Index1].Left+Items[Index1].Width
                     ,Items[Index2].Left+Items[Index2].Width);
end;

function TControlSelection.CompareBottom(Index1, Index2: integer): integer;
begin
  Result:=CompareInts(Items[Index1].Top+Items[Index1].Height
                     ,Items[Index2].Top+Items[Index2].Height);
end;

function TControlSelection.CompareHorCenter(Index1, Index2: integer): integer;
begin
  Result:=CompareInts(Items[Index1].Left+(Items[Index1].Width div 2)
                     ,Items[Index2].Left+(Items[Index2].Width div 2));
end;

function TControlSelection.CompareVertCenter(Index1, Index2: integer): integer;
begin
  Result:=CompareInts(Items[Index1].Top+(Items[Index1].Height div 2)
                     ,Items[Index2].Top+(Items[Index2].Height div 2));
end;

procedure TControlSelection.AlignComponents(
  HorizAlignment, VertAlignment: TComponentAlignment);
var i, ALeft, ATop, ARight, ABottom, HorCenter, VertCenter,
  HorDiff, VertDiff, TotalWidth, TotalHeight, HorSpacing, VertSpacing,
  x, y: integer;
begin
  if (Count=0) or (IsResizing) then exit;
  if (Items[0].IsTopLvl)
  or ((HorizAlignment=csaNone) and (VertAlignment=csaNone)) then exit;
  BeginResizing;

  // initializing
  ALeft:=Items[0].Left;
  ATop:=Items[0].Top;
  ARight:=ALeft+Items[0].Width;
  ABottom:=ATop+Items[0].Height;
  TotalWidth:=Items[0].Width;
  TotalHeight:=Items[0].Height;
  for i:=1 to FControls.Count-1 do begin
    ALeft:=Min(ALeft,Items[i].Left);
    ATop:=Min(ATop,Items[i].Top);
    ARight:=Max(ARight,Items[i].Left+Items[i].Width);
    ABottom:=Max(ABottom,Items[i].Top+Items[i].Height);
    if Items[i].IsTopLvl then continue;
    inc(TotalWidth,Items[i].Width);
    inc(TotalHeight,Items[i].Height);
  end;

  // move components horizontally
  case HorizAlignment of
    csaSides1, csaCenters, csaSides2, csaCenterInWindow:
      begin
        HorCenter:=(ALeft+ARight) div 2;
        HorDiff:=(FCustomForm.Width div 2)-HorCenter;
        for i:=0 to FControls.Count-1 do begin
          if Items[i].IsTopLvl then continue;
          case HorizAlignment of
           csaSides1:  Items[i].Left:=ALeft;
           csaCenters: Items[i].Left:=HorCenter-(Items[i].Width div 2);
           csaSides2:  Items[i].Left:=ARight-Items[i].Width;
           csaCenterInWindow: Items[i].Left:=Items[i].Left+HorDiff;
          end;
        end;
      end;
    csaSpaceEqually:
      begin
        HorSpacing:=(ARight-ALeft-TotalWidth) div (FControls.Count-1);
        x:=ALeft;
        Sort(@CompareHorCenter);
        for i:=0 to FControls.Count-1 do begin
          if Items[i].IsTopLvl then continue;
          Items[i].Left:=x;
          Inc(x,Items[i].Width+HorSpacing);
        end;
      end;
    csaSide1SpaceEqually:
      begin
        Sort(@CompareLeft);
        HorSpacing:=(Items[Count-1].Left-ALeft) div FControls.Count;
        x:=ALeft;
        for i:=0 to FControls.Count-1 do begin
          if Items[i].IsTopLvl then continue;
          Items[i].Left:=x;
          inc(x,HorSpacing);
        end;
      end;
    csaSide2SpaceEqually:
      begin
        Sort(@CompareRight);
        HorSpacing:=(ARight-ALeft-Items[0].Width) div FControls.Count;
        x:=ARight;
        for i:=FControls.Count-1 downto 0 do begin
          if Items[i].IsTopLvl then continue;
          Items[i].Left:=x-Items[i].Width;
          dec(x,HorSpacing);
        end;
      end;
  end;

  // move components vertically
  case VertAlignment of
    csaSides1, csaCenters, csaSides2, csaCenterInWindow:
      begin
        VertCenter:=(ATop+ABottom) div 2;
        VertDiff:=(FCustomForm.Height div 2)-VertCenter;
        for i:=0 to FControls.Count-1 do begin
          if Items[i].IsTopLvl then continue;
          case VertAlignment of
           csaSides1:  Items[i].Top:=ATop;
           csaCenters: Items[i].Top:=VertCenter-(Items[i].Height div 2);
           csaSides2:  Items[i].Top:=ABottom-Items[i].Height;
           csaCenterInWindow: Items[i].Top:=Items[i].Top+VertDiff;
          end;
        end;
      end;
    csaSpaceEqually:
      begin
        VertSpacing:=(ABottom-ATop-TotalHeight) div (FControls.Count-1);
        y:=ATop;
        Sort(@CompareVertCenter);
        for i:=0 to FControls.Count-1 do begin
          if Items[i].IsTopLvl then continue;
          Items[i].Top:=y;
          Inc(y,Items[i].Height+VertSpacing);
        end;
      end;
    csaSide1SpaceEqually:
      begin
        Sort(@CompareTop);
        VertSpacing:=(Items[Count-1].Top-ATop) div FControls.Count;
        y:=ATop;
        for i:=0 to FControls.Count-1 do begin
          if Items[i].IsTopLvl then continue;
          Items[i].Top:=y;
          inc(y,VertSpacing);
        end;
      end;
    csaSide2SpaceEqually:
      begin
        Sort(@CompareBottom);
        VertSpacing:=(ABottom-ATop-Items[0].Height) div FControls.Count;
        y:=ABottom;
        for i:=FControls.Count-1 downto 0 do begin
          if Items[i].IsTopLvl then continue;
          Items[i].Top:=y-Items[i].Height;
          dec(y,VertSpacing);
        end;
      end;
  end;
      
  EndResizing(false);
end;

procedure TControlSelection.MirrorHorizontal;
var
  i, ALeft, ARight, Middle, NewLeft: integer;
begin
  if (FControls.Count=0) or (Items[0].IsTopLvl) then exit;
  BeginResizing;

  // initializing
  ALeft:=Items[0].Left;
  ARight:=ALeft+Items[0].Width;
  for i:=1 to FControls.Count-1 do begin
    ALeft:=Min(ALeft,Items[i].Left);
    ARight:=Max(ARight,Items[i].Left+Items[i].Width);
  end;
  Middle:=(ALeft+ARight) div 2;

  // move components
  for i:=0 to FControls.Count-1 do begin
    if Items[i].IsTopLvl then continue;
    NewLeft:=2*Middle-Items[i].Left-Items[i].Width;
    NewLeft:=Max(NewLeft,ALeft);
    NewLeft:=Min(NewLeft,ARight-Items[i].Width);
    Items[i].Left:=NewLeft;
  end;

  EndResizing(false);
  UpdateRealBounds;
end;

procedure TControlSelection.MirrorVertical;
var
  i, ATop, ABottom, Middle, NewTop: integer;
begin
  if (FControls.Count=0) or (Items[0].IsTopLvl) then exit;
  BeginResizing;

  // initializing
  ATop:=Items[0].Top;
  ABottom:=ATop+Items[0].Height;
  for i:=1 to FControls.Count-1 do begin
    ATop:=Min(ATop,Items[i].Top);
    ABottom:=Max(ABottom,Items[i].Top+Items[i].Height);
  end;
  Middle:=(ATop+ABottom) div 2;

  // move components
  for i:=0 to FControls.Count-1 do begin
    if Items[i].IsTopLvl then continue;
    NewTop:=2*Middle-Items[i].Top-Items[i].Height;
    NewTop:=Max(NewTop,ATop);
    NewTop:=Min(NewTop,ABottom-Items[i].Height);
    Items[i].Top:=NewTop;
  end;

  EndResizing(false);
  UpdateRealBounds;
end;

procedure TControlSelection.SizeComponents(
  HorizSizing: TComponentSizing; AWidth: integer;
  VertSizing: TComponentSizing; AHeight: integer);
var i: integer;
begin
  if (FControls.Count=0) or (Items[0].IsTopLvl) then exit;
  BeginResizing;

  // initialize
  case HorizSizing of
    cssShrinkToSmallest, cssGrowToLargest:
      AWidth:=Items[0].Width;
    cssFixed:
      if AWidth<1 then HorizSizing:=cssNone;
  end;
  case VertSizing of
    cssShrinkToSmallest, cssGrowToLargest:
      AHeight:=Items[0].Height;
    cssFixed:
      if AHeight<1 then VertSizing:=cssNone;
  end;
  for i:=1 to FControls.Count-1 do begin
    case HorizSizing of
     cssShrinkToSmallest: AWidth:=Min(AWidth,Items[i].Width);
     cssGrowToLargest:    AWidth:=Max(AWidth,Items[i].Width);
    end;
    case VertSizing of
     cssShrinkToSmallest: AHeight:=Min(AHeight,Items[i].Height);
     cssGrowToLargest:    AHeight:=Max(AHeight,Items[i].Height);
    end;
  end;

  // size components
  for i:=0 to FControls.Count-1 do begin
    if Items[i].IsTopLvl then continue;
    if (Items[i].Component is TControl) then begin
      if HorizSizing=cssNone then AWidth:=Items[i].Width;
      if VertSizing=cssNone then AHeight:=Items[i].Height;
      TControl(Items[i].Component).SetBounds(Items[i].Left,Items[i].Top,
        Max(1,AWidth), Max(1,AHeight));
    end;
  end;  
  
  EndResizing(false);
end;

procedure TControlSelection.ScaleComponents(Percent: integer);
var i: integer;
begin
  if (FControls.Count=0) then exit;
  BeginResizing;

  if Percent<1 then Percent:=1;
  if Percent>1000 then Percent:=1000;
  // size components
  for i:=0 to FControls.Count-1 do begin
    if Items[i].Component is TControl then begin
      TControl(Items[i].Component).SetBounds(
          Items[i].Left,
          Items[i].Top,
          Max(1,(Items[i].Width*Percent) div 100),
          Max(1,(Items[i].Height*Percent) div 100)
        );
    end;
  end;  

  EndResizing(false);
end;

procedure TControlSelection.DrawGuideLines(DC: TDesignerDeviceContext);
var
  DCOrigin: TPoint;

  procedure DrawLine(ARect: TRect; AColor: TColor);
  begin
    with DC.Canvas do begin
      Pen.Color:=AColor;
      MoveTo(ARect.Left-DCOrigin.X,ARect.Top-DCOrigin.Y);
      LineTo(ARect.Right-DCOrigin.X,ARect.Bottom-DCOrigin.Y);
    end;
  end;

var
  OldPenColor:TColor;
  LeftGuideLineExists, RightGuideLineExists,
  TopGuideLineExists, BottomGuideLineExists: boolean;
  LeftGuideLine, RightGuideLine, TopGuideLine, BottomGuideLine: TRect;
begin
  if (Count=0) or (FCustomForm=nil) or Items[0].IsTopLvl then exit;
  LeftGuideLineExists:=GetLeftGuideLine(LeftGuideLine);
  RightGuideLineExists:=GetRightGuideLine(RightGuideLine);
  TopGuideLineExists:=GetTopGuideLine(TopGuideLine);
  BottomGuideLineExists:=GetBottomGuideLine(BottomGuideLine);
  if (not LeftGuideLineExists) and (not RightGuideLineExists)
  and (not TopGuideLineExists) and (not BottomGuideLineExists)
  then exit;
  
  DC.Save;
  DCOrigin:=DC.FormOrigin;
  OldPenColor:=DC.Canvas.Pen.Color;
  // draw bottom guideline
  if BottomGuideLineExists then
    DrawLine(BottomGuideLine,EnvironmentOptions.GuideLineColorRightBottom);
  // draw top guideline
  if TopGuideLineExists then
    DrawLine(TopGuideLine,EnvironmentOptions.GuideLineColorLeftTop);
  // draw right guideline
  if RightGuideLineExists then
    DrawLine(RightGuideLine,EnvironmentOptions.GuideLineColorRightBottom);
  // draw left guideline
  if LeftGuideLineExists then
    DrawLine(LeftGuideLine,EnvironmentOptions.GuideLineColorLeftTop);
  DC.Canvas.Pen.Color:=OldPenColor;
end;

procedure TControlSelection.Sort(SortProc: TSelectionSortCompare);
var a, b: integer;
  h: Pointer;
  Changed: boolean;
begin
  Changed:=false;
  for a:=0 to FControls.Count-1 do begin
    for b:=a+1 to FControls.Count-1 do begin
      if SortProc(a,b)>0 then begin
        h:=FControls[a];
        FControls[a]:=FControls[b];
        FControls[b]:=h;
        Changed:=true;
      end;
    end;
  end;
  if Changed then DoChange;
end;

end.
