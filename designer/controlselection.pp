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
  EnvironmentOpts, DesignerProcs, Menus;

type
  TControlSelection = class;
  TGrabber = class;

  { TGrabber }
  
  TGrabIndex = 0..7;

  TGrabPosition = (gpTop, gpBottom, gpLeft, gpRight);
  TGrabPositions = set of TGrabPosition;

  TGrabberMoveEvent = procedure(Grabber: TGrabber;
                                const OldRect, NewRect: TRect) of object;

  // A TGrabber is one of the 8 small black rectangles at the boundaries of
  // a selection
  TGrabber = class
  private
    FOnMove: TGrabberMoveEvent;
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
    procedure SaveBounds;
    procedure Move(NewLeft, NewTop: integer);
    procedure GetRect(var ARect: TRect);
    procedure InvalidateOnForm(AForm: TCustomForm);

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
    property OnMove: TGrabberMoveEvent read FOnMove write FOnMove;
  end;


  { TSelectedControl }
  
  TSelectedControlFlag = (
    scfParentInSelection,
    scfChildInSelection
    );
  TSelectedControlFlags = set of TSelectedControlFlag;

  TSelectedControl = class
  private
    FCachedLeft: integer;
    FCachedTop: integer;
    FCachedWidth: integer;
    FCachedHeight: integer;
    FCachedFormRelativeLeftTop: TPoint;
    FComponent:TComponent;
    FFlags: TSelectedControlFlags;
    FOldLeft: integer;
    FOldTop: integer;
    FOldWidth: integer;
    FOldHeight: integer;
    FOldFormRelativeLeftTop: TPoint;
    FOwner: TControlSelection;
    FUseCache: boolean;
    function GetLeft: integer;
    procedure SetLeft(ALeft: integer);
    function GetTop: integer;
    procedure SetOwner(const AValue: TControlSelection);
    procedure SetTop(ATop: integer);
    function GetWidth: integer;
    procedure SetUseCache(const AValue: boolean);
    procedure SetWidth(AWidth: integer);
    function GetHeight: integer;
    procedure SetHeight(AHeight: integer);
    procedure SetFlags(const AValue: TSelectedControlFlags);
  public
    constructor Create(AnOwner: TControlSelection; AComponent: TComponent);
    destructor Destroy; override;
    function ParentForm: TCustomForm;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer);
    procedure SetFormRelativeBounds(ALeft, ATop, AWidth, AHeight: integer);
    procedure SaveBounds;
    procedure UpdateCache;
    function IsTopLvl: boolean;
    function ChildInSelection: boolean;
    function ParentInSelection: boolean;
    procedure InvalidateNonVisualComponent;

    property Component: TComponent read FComponent write FComponent;
    property Owner: TControlSelection read FOwner write SetOwner;
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
    property Flags: TSelectedControlFlags read FFlags write SetFlags;
    property UseCache: boolean read FUseCache write SetUseCache;
  end;
  

  TComponentAlignment = (csaNone, csaSides1, csaCenters, csaSides2,
      csaCenterInWindow, csaSpaceEqually, csaSide1SpaceEqually,
      csaSide2SpaceEqually);
  TComponentSizing = (cssNone, cssShrinkToSmallest, cssGrowToLargest, cssFixed);
  TSelectionSortCompare = function(Index1, Index2: integer): integer of object;
  TOnSelectionFormChanged = procedure(Sender: TObject;
    OldForm, NewForm: TCustomForm) of object;
  
  TNearestInt = record
    Level: integer;
    Nearest: integer;
    Valid: boolean;
  end;
  
  TGuideLineCache = record
    CacheValid: boolean;
    LineValid: boolean;
    Line: TRect;
    PaintedLineValid: boolean;
    PaintedLine: TRect;
  end;
  
  TGuideLineType = (glLeft, glTop, glRight, glBottom);
  
  TRubberbandType = (
    rbtSelection,
    rbtCreating
    );
    

  { TControlSelection }
  
  TControlSelState = (
    cssOnlyNonVisualNeedsUpdate,
    cssOnlyNonVisualSelected,
    cssOnlyVisualNeedsUpdate,
    cssOnlyVisualNeedsSelected,
    cssBoundsNeedsUpdate,
    cssBoundsNeedsSaving,
    cssParentLevelNeedsUpdate,
    cssNotSavingBounds,
    cssSnapping,
    cssChangedDuringLock,
    cssRubberbandActive,
    cssCacheGuideLines,
    cssVisible,
    cssParentChildFlagsNeedUpdate,
    cssGrabbersPainted,
    cssGuideLinesPainted
    );
  TControlSelStates = set of TControlSelState;

  TControlSelection = class(TObject)
    procedure GrabberMove(Grabber: TGrabber; const OldRect, NewRect: TRect);
  private
    FControls: TList;  // list of TSelectedComponent

    // current bounds of the selection (only valid if Count>0)
    // These are the values set by the user
    // But due to snapping and lcl aligning the components can have other bounds
    FLeft: Integer;
    FOnSelectionFormChanged: TOnSelectionFormChanged;
    FRubberbandCreationColor: TColor;
    FRubberbandSelectionColor: TColor;
    FSavedHeight: integer;
    FSavedLeft: integer;
    FSavedTop: integer;
    FSavedWidth: integer;
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
    // caches
    FGuideLinesCache: array[TGuideLineType] of TGuideLineCache;
    FParentLevel: integer;

    FCustomForm: TCustomForm;
    FGrabbers: array[TGrabIndex] of TGrabber;
    FGrabberSize: integer;
    FMarkerSize: integer;
    FActiveGrabber: TGrabber;
    FRubberBandBounds: TRect;
    FRubberbandType: TRubberbandType;
    FUpdateLock: integer;
    FResizeLockCount: integer;
    FStates: TControlSelStates;

    FOnChange: TNotifyEvent;

    function GetCacheGuideLines: boolean;
    function GetGrabberColor: TColor;
    function GetMarkerColor: TColor;
    function GetRubberbandActive: boolean;
    function GetRubberbandCreationColor: TColor;
    function GetRubberbandSelectionColor: TColor;
    function GetSnapping: boolean;
    function GetVisible: boolean;
    procedure SetCacheGuideLines(const AValue: boolean);
    procedure SetCustomForm;
    function GetGrabbers(AGrabIndex:TGrabIndex): TGrabber;
    procedure SetGrabbers(AGrabIndex:TGrabIndex; const AGrabber: TGrabber);
    procedure SetGrabberSize(const NewSize: integer);
    procedure DoChange;
    procedure SetRubberbandActive(const AValue: boolean);
    procedure SetRubberbandType(const AValue: TRubberbandType);
    procedure SetSnapping(const AValue: boolean);
    procedure SetVisible(const AValue: Boolean);
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
    procedure InvalidateGrabbers;
    procedure InvalidateGuideLines;
    procedure DoApplyUserBounds;
    procedure UpdateRealBounds;
    procedure UpdateParentChildFlags;

    // snapping
    function CleanGridSizeX: integer;
    function CleanGridSizeY: integer;
    function ComponentAlignable(AComponent: TComponent): boolean;
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
    property Items[Index:integer]:TSelectedControl
      read GetItems write SetItems; default;
    function Count:integer;
    
    procedure BeginUpdate;
    procedure EndUpdate;
    property UpdateLock: integer read FUpdateLock;
    
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
    property Snapping: boolean read GetSnapping write SetSnapping;
    procedure DrawGuideLines(DC: TDesignerDeviceContext);
    property CacheGuideLines: boolean
      read GetCacheGuideLines write SetCacheGuideLines;
    procedure InvalidateGuideLinesCache;
    function ParentLevel: integer;

    property GrabberSize:integer read FGrabberSize write SetGrabberSize;
    property GrabberColor: TColor read GetGrabberColor;
    procedure DrawGrabbers(DC: TDesignerDeviceContext);
    function GrabberAtPos(X,Y: integer):TGrabber;
    property Grabbers[AGrabIndex: TGrabIndex]:TGrabber
      read GetGrabbers write SetGrabbers;
    property MarkerSize:integer read FMarkerSize write FMarkerSize;
    property MarkerColor: TColor read GetMarkerColor;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure DrawMarker(AComponent: TComponent; DC: TDesignerDeviceContext);
    procedure DrawMarkerAt(DC: TDesignerDeviceContext;
      ALeft, ATop, AWidth, AHeight: integer);
    property ActiveGrabber: TGrabber read FActiveGrabber write SetActiveGrabber;
    
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

    property RubberbandBounds:TRect
      read FRubberbandBounds write SetRubberbandBounds;
    property RubberbandActive: boolean
      read GetRubberbandActive write SetRubberbandActive;
    property RubberbandType: TRubberbandType
      read FRubberbandType write SetRubberbandType;
    property RubberbandSelectionColor: TColor
      read GetRubberbandSelectionColor;
    property RubberbandCreationColor: TColor
      read GetRubberbandCreationColor;
    procedure DrawRubberband(DC: TDesignerDeviceContext);
    procedure SelectWithRubberBand(ACustomForm:TCustomForm;
      ClearBefore, ExclusiveOr: boolean; var SelectionChanged: boolean;
      MaxParentControl: TControl);

    procedure Sort(SortProc: TSelectionSortCompare);
    property Visible:boolean read GetVisible write SetVisible;
    function OnlyNonVisualComponentsSelected: boolean;
    function OnlyVisualComponentsSelected: boolean;
    
    property SelectionForm: TCustomForm read FCustomForm;
    property OnSelectionFormChanged: TOnSelectionFormChanged
      read FOnSelectionFormChanged write FOnSelectionFormChanged;
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

procedure TGrabber.Move(NewLeft, NewTop: integer);
var
  OldRect, NewRect: TRect;
begin
  if (NewLeft=FLeft) and (NewTop=FTop) then exit;
  GetRect(OldRect);
  FLeft:=NewLeft;
  FTop:=NewTop;
  GetRect(NewRect);
  if Assigned(FOnMove) then FOnMove(Self,OldRect,NewRect);
end;

procedure TGrabber.GetRect(var ARect: TRect);
begin
  ARect.Left:=FLeft;
  ARect.Top:=FTop;
  ARect.Right:=FLeft+FWidth;
  ARect.Bottom:=FTop+FHeight;
end;

procedure TGrabber.InvalidateOnForm(AForm: TCustomForm);
var
  ARect: TRect;
begin
  GetRect(ARect);
  InvalidateRect(AForm.Handle,@ARect,false);
end;


{ TSelectedControl }

constructor TSelectedControl.Create(AnOwner: TControlSelection;
  AComponent:TComponent);
begin
  inherited Create;
  FOwner:=AnOwner;
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
  if FComponent is TControl then begin
    TControl(FComponent).SetBounds(ALeft, ATop, AWidth, AHeight);
    FCachedLeft:=ALeft;
    FCachedTop:=ATop;
    FCachedWidth:=AWidth;
    FCachedHeight:=AHeight;
  end else begin
    if (Left<>ALeft) or (Top<>ATop) then begin
      InvalidateNonVisualComponent;
      Left:=ALeft;
      Top:=ATop;
      InvalidateNonVisualComponent;
    end;
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

procedure TSelectedControl.UpdateCache;
begin
  GetComponentBounds(FComponent,FCachedLeft,FCachedTop,FCachedWidth,FCachedHeight);
  FCachedFormRelativeLeftTop:=GetParentFormRelativeTopLeft(FComponent);
end;

function TSelectedControl.IsTopLvl: boolean;
begin
  Result:=(FComponent is TControl) and (TControl(FComponent).Parent=nil);
end;

function TSelectedControl.ChildInSelection: boolean;
begin
  if Owner<>nil then begin
    Owner.UpdateParentChildFlags;
    Result:=scfChildInSelection in FFlags;
  end else begin
    Result:=false;
  end;
end;

function TSelectedControl.ParentInSelection: boolean;
begin
  if Owner<>nil then begin
    Owner.UpdateParentChildFlags;
    Result:=scfParentInSelection in FFlags;
  end else begin
    Result:=false;
  end;
end;

procedure TSelectedControl.InvalidateNonVisualComponent;
var
  AForm: TCustomForm;
  CompRect: TRect;
begin
  AForm:=TCustomForm(FComponent.Owner);
  if (AForm=nil) or (not (AForm is TCustomForm)) then exit;
  CompRect.Left:=LongRec(FComponent.DesignInfo).Lo;
  CompRect.Top:=LongRec(FComponent.DesignInfo).Hi;
  CompRect.Right:=CompRect.Left+NonVisualCompWidth;
  CompRect.Bottom:=CompRect.Top+NonVisualCompWidth;
  InvalidateRect(AForm.Handle,@CompRect,false);
end;

function TSelectedControl.GetLeft: integer;
begin
  if FUseCache then
    Result:=FCachedLeft
  else
    Result:=GetComponentLeft(FComponent);
end;

procedure TSelectedControl.SetFlags(const AValue: TSelectedControlFlags);
begin
  if FFlags=AValue then exit;
  FFlags:=AValue;
end;

procedure TSelectedControl.SetLeft(ALeft: integer);
begin
  if FComponent is TControl then
    TControl(FComponent).Left:=Aleft
  else
    LongRec(FComponent.DesignInfo).Lo:=Min(32000,Max(0,ALeft));
  FCachedLeft:=ALeft;
end;

function TSelectedControl.GetTop: integer;
begin
  if FUseCache then
    Result:=FCachedTop
  else
    Result:=GetComponentTop(FComponent);
end;

procedure TSelectedControl.SetOwner(const AValue: TControlSelection);
begin
  if FOwner=AValue then exit;
  FOwner:=AValue;
end;

procedure TSelectedControl.SetTop(ATop: integer);
begin
  if FComponent is TControl then
    TControl(FComponent).Top:=ATop
  else
    LongRec(FComponent.DesignInfo).Hi:=Min(32000,Max(0,ATop));
  FCachedTop:=ATop;
end;

function TSelectedControl.GetWidth: integer;
begin
  if FUseCache then
    Result:=FCachedWidth
  else
    Result:=GetComponentWidth(FComponent);
end;

procedure TSelectedControl.SetUseCache(const AValue: boolean);
begin
  if FUseCache=AValue then exit;
  FUseCache:=AValue;
  if FUseCache then UpdateCache;
end;

procedure TSelectedControl.SetWidth(AWidth: integer);
begin
  if FComponent is TControl then
    TControl(FComponent).Width:=AWidth
  else
    ;
  FCachedWidth:=AWidth;
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
  FCachedHeight:=AHeight;
end;


{ TControlSelection }

constructor TControlSelection.Create;
var g:TGrabIndex;
begin
  inherited;
  FControls:=TList.Create;
  FGrabberSize:=5;
  FMarkerSize:=5;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do begin
    FGrabbers[g]:=TGrabber.Create;
    FGrabbers[g].Positions:=GRAB_POSITIONS[g];
    FGrabbers[g].GrabIndex:=g;
    FGrabbers[g].Cursor:=GRAB_CURSOR[g];
    FGrabbers[g].OnMove:=@GrabberMove;
  end;
  FCustomForm:=nil;
  FActiveGrabber:=nil;
  FUpdateLock:=0;
  FStates:=[cssOnlyNonVisualNeedsUpdate,cssOnlyVisualNeedsUpdate,
            cssParentLevelNeedsUpdate,cssCacheGuideLines];
  FRubberbandType:=rbtSelection;
  FRubberbandCreationColor:=clMaroon;
  FRubberbandSelectionColor:=clNavy;
end;

destructor TControlSelection.Destroy;
var g:TGrabIndex;
begin
  Clear;
  FControls.Free;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do FGrabbers[g].Free;
  inherited Destroy;
end;

procedure TControlSelection.BeginUpdate;
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
    if cssChangedDuringLock in FStates then DoChange;
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

procedure TControlSelection.SetCacheGuideLines(const AValue: boolean);
begin
  if CacheGuideLines=AValue then exit;
  if AValue then
    Include(FStates,cssCacheGuideLines)
  else
    Exclude(FStates,cssCacheGuideLines);
  InvalidateGuideLinesCache;
end;

function TControlSelection.GetSnapping: boolean;
begin
  Result:=cssSnapping in FStates;
end;

function TControlSelection.GetVisible: boolean;
begin
  Result:=cssVisible in FStates;
end;

function TControlSelection.GetRubberbandActive: boolean;
begin
  Result:=cssRubberbandActive in FStates;
end;

function TControlSelection.GetRubberbandCreationColor: TColor;
begin
  Result:=EnvironmentOptions.RubberbandCreationColor;
end;

function TControlSelection.GetRubberbandSelectionColor: TColor;
begin
  Result:=EnvironmentOptions.RubberbandSelectionColor;
end;

procedure TControlSelection.GrabberMove(Grabber: TGrabber; const OldRect,
  NewRect: TRect);
begin
  if FCustomForm=nil then exit;
  InvalidateRect(FCustomForm.Handle,@OldRect,false);
  InvalidateRect(FCustomForm.Handle,@NewRect,false);
end;

function TControlSelection.GetCacheGuideLines: boolean;
begin
  Result:=cssCacheGuideLines in FStates;
end;

function TControlSelection.GetGrabberColor: TColor;
begin
  Result:=EnvironmentOptions.GrabberColor;
end;

function TControlSelection.GetMarkerColor: TColor;
begin
  Result:=EnvironmentOptions.MarkerColor;
end;

procedure TControlSelection.SetCustomForm;
var
  OldCustomForm, NewCustomForm: TCustomForm;
begin
  if Count>0 then
    NewCustomForm:=Items[0].ParentForm
  else
    NewCustomForm:=nil;
  if NewCustomForm=FCustomForm then exit;
  // form changed
  InvalidateGuideLines;
  InvalidateGrabbers;
  OldCustomForm:=FCustomForm;
  FCustomForm:=NewCustomForm;
  if Assigned(FOnSelectionFormChanged) then
    FOnSelectionFormChanged(Self,OldCustomForm,NewCustomForm);
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
  if FUpdateLock>0 then begin
    Include(FStates,cssBoundsNeedsUpdate);
    exit;
  end;
  UpdateRealBounds;
  FLeft:=FRealLeft;
  FTop:=FRealTop;
  FWidth:=FRealWidth;
  FHeight:=FRealHeight;
  InvalidateGuideLinesCache;
  Exclude(FStates,cssBoundsNeedsUpdate);
end;

procedure TControlSelection.AdjustGrabbers;
var g:TGrabIndex;
  OutPix, InPix, NewGrabberLeft, NewGrabberTop: integer;
begin
  OutPix:=GrabberSize div 2;
  InPix:=GrabberSize-OutPix;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do begin
    if gpLeft in FGrabbers[g].Positions then
      NewGrabberLeft:=FRealLeft-OutPix
    else if gpRight in FGrabbers[g].Positions then
      NewGrabberLeft:=FRealLeft+FRealWidth-InPix
    else
      NewGrabberLeft:=FRealLeft+((FRealWidth-GrabberSize) div 2);
    if gpTop in FGrabbers[g].Positions then
      NewGrabberTop:=FRealTop-OutPix
    else if gpBottom in FGrabbers[g].Positions then
      NewGrabberTop:=FRealTop+FRealHeight-InPix
    else
      NewGrabberTop:=FRealTop+((FRealHeight-GrabberSize) div 2);
    FGrabbers[g].Width:=GrabberSize;
    FGrabbers[g].Height:=GrabberSize;
    FGrabbers[g].Move(NewGrabberLeft,NewGrabberTop);
  end;
end;

procedure TControlSelection.InvalidateGrabbers;
var g: TGrabIndex;
begin
  if cssGrabbersPainted in FStates then begin
    for g:=Low(TGrabIndex) to High(TGrabIndex) do
      FGrabbers[g].InvalidateOnForm(FCustomForm);
    Exclude(FStates,cssGrabbersPainted);
  end;
end;

procedure TControlSelection.InvalidateGuideLines;
var
  g: TGuideLineType;
  LineRect: TRect;
begin
  if (cssGuideLinesPainted in FStates) then begin
    if (FCustomForm<>nil) and CacheGuideLines then
      for g:=Low(g) to High(g) do begin
        if FGuideLinesCache[g].PaintedLineValid then
        begin
          LineRect:=FGuideLinesCache[g].PaintedLine;
          if LineRect.Top=LineRect.Bottom then inc(LineRect.Bottom);
          if LineRect.Left=LineRect.Right then inc(LineRect.Right);
          InvalidateRect(FCustomForm.Handle,@LineRect,false);
        end;
      end;
    Exclude(FStates,cssGuideLinesPainted);
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
    InvalidateGuideLinesCache;
  end else if Count>1 then begin
    // multi selection
    {$IFDEF VerboseDesigner}
    writeln('[TControlSelection.DoApplyUserBounds] M Old=',FOldLeft,',',FOldTop,',',FOldWidth,',',FOldHeight,
    ' User=',FLeft,',',FTop,',',FWidth,',',FHeight);
    {$ENDIF}
    
    // ToDo: sort selection with parent level and size/move parents first
    
    if (FOldWidth<>0) and (FOldHeight<>0) then begin
      for i:=0 to Count-1 do begin
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
        writeln('  i=',i,' ',Items[i].Component.Name,
        ' ',Items[i].Left,',',Items[i].Top,',',Items[i].Width,',',Items[i].Height);
        {$ENDIF}
      end;
      InvalidateGuideLinesCache;
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
    InvalidateGuideLines;
  end;
end;

procedure TControlSelection.UpdateParentChildFlags;
var
  i, j, Cnt: integer;
  Control1, Control2: TControl;
begin
  if not (cssParentChildFlagsNeedUpdate in FStates) then exit;
  Cnt:=Count;
  for i:=0 to Cnt-1 do begin
    Control1:=TControl(Items[i].Component);
    Items[i].FFlags:=Items[i].FFlags-[scfParentInSelection,scfChildInSelection];
  end;
  for i:=0 to Cnt-1 do begin
    Control1:=TControl(Items[i].Component);
    if not (Control1 is TControl) then continue;
    for j:=0 to Cnt-1 do begin
      Control2:=TControl(Items[j].Component);
      if not (Control2 is TControl) then continue;
      if i=j then continue;
      if Control1.IsParentOf(Control2) then begin
        Include(Items[i].FFlags,scfChildInSelection);
        Include(Items[j].FFlags,scfParentInSelection);
      end;
    end;
  end;
  Exclude(FStates,cssParentChildFlagsNeedUpdate);
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

function TControlSelection.ComponentAlignable(AComponent: TComponent): boolean;
var
  CurParentLevel: integer;
begin
  Result:=false;
  if AComponent=nil then exit;
  if AComponent is TControl then begin
    if not ControlIsDesignerVisible(TControl(AComponent)) then exit;
    if Count>0 then begin
      if OnlyNonVisualComponentsSelected then exit;
    end;
    if ParentLevel>0 then begin
      CurParentLevel:=GetParentLevel(TControl(AComponent));
      if CurParentLevel<>ParentLevel then exit;
    end;
  end else begin
    if AComponent is TMenuItem then exit;
    if Count>0 then begin
      if OnlyVisualComponentsSelected then exit;
    end;
  end;
  if IsSelected(AComponent) then exit;
  
  Result:=true;
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
    if not ComponentAlignable(AComponent) then exit;
    if IsSelected(AComponent) then continue;
    CurLeft:=GetParentFormRelativeTopLeft(AComponent).X;
    CurDist:=Abs(CurLeft-NearestInt.Level);
    if CurDist>MaxDist then continue; // skip components far away
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
    if not ComponentAlignable(AComponent) then exit;
    if IsSelected(AComponent) then continue;
    CurRight:=GetParentFormRelativeTopLeft(AComponent).X
              +GetComponentWidth(AComponent);
    CurDist:=Abs(CurRight-NearestInt.Level);
    if CurDist>MaxDist then continue; // skip components far away
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
    if not ComponentAlignable(AComponent) then exit;
    if IsSelected(AComponent) then continue;
    CurTop:=GetParentFormRelativeTopLeft(AComponent).Y;
    CurDist:=Abs(CurTop-NearestInt.Level);
    if CurDist>MaxDist then continue; // skip components far away
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
    if not ComponentAlignable(AComponent) then exit;
    if IsSelected(AComponent) then continue;
    CurBottom:=GetParentFormRelativeTopLeft(AComponent).Y
              +GetComponentHeight(AComponent);
    CurDist:=Abs(CurBottom-NearestInt.Level);
    if CurDist>MaxDist then continue; // skip components far away
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
  if CacheGuideLines and FGuideLinesCache[glLeft].CacheValid then begin
    Result:=FGuideLinesCache[glLeft].LineValid;
    if Result then
      ALine:=FGuideLinesCache[glLeft].Line;
  end else begin
    Result:=false;
    if FCustomForm=nil then exit;
    for i:=0 to FCustomForm.ComponentCount-1 do begin
      if not ComponentAlignable(FCustomForm.Components[i]) then continue;
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
    if CacheGuideLines then begin
      FGuideLinesCache[glLeft].LineValid:=Result;
      FGuideLinesCache[glLeft].Line:=ALine;
      FGuideLinesCache[glLeft].CacheValid:=true;
    end;
  end;
end;

function TControlSelection.GetRightGuideLine(var ALine: TRect): boolean;
var i, LineTop, LineBottom: integer;
  CRect: TRect;
begin
  if CacheGuideLines and FGuideLinesCache[glRight].CacheValid then begin
    Result:=FGuideLinesCache[glRight].LineValid;
    if Result then
      ALine:=FGuideLinesCache[glRight].Line;
  end else begin
    Result:=false;
    if FCustomForm=nil then exit;
    for i:=0 to FCustomForm.ComponentCount-1 do begin
      if not ComponentAlignable(FCustomForm.Components[i]) then continue;
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
    if CacheGuideLines then begin
      FGuideLinesCache[glRight].LineValid:=Result;
      FGuideLinesCache[glRight].Line:=ALine;
      FGuideLinesCache[glRight].CacheValid:=true;
    end;
  end;
end;

function TControlSelection.GetTopGuideLine(var ALine: TRect): boolean;
var i, LineLeft, LineRight: integer;
  CRect: TRect;
begin
  if CacheGuideLines and FGuideLinesCache[glTop].CacheValid then begin
    Result:=FGuideLinesCache[glTop].LineValid;
    if Result then
      ALine:=FGuideLinesCache[glTop].Line;
  end else begin
    Result:=false;
    if FCustomForm=nil then exit;
    for i:=0 to FCustomForm.ComponentCount-1 do begin
      if not ComponentAlignable(FCustomForm.Components[i]) then continue;
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
    if CacheGuideLines then begin
      FGuideLinesCache[glTop].LineValid:=Result;
      FGuideLinesCache[glTop].Line:=ALine;
      FGuideLinesCache[glTop].CacheValid:=true;
    end;
  end;
end;

function TControlSelection.GetBottomGuideLine(var ALine: TRect): boolean;
var i, LineLeft, LineRight: integer;
  CRect: TRect;
begin
  if CacheGuideLines and FGuideLinesCache[glBottom].CacheValid then begin
    Result:=FGuideLinesCache[glBottom].LineValid;
    if Result then
      ALine:=FGuideLinesCache[glBottom].Line;
  end else begin
    Result:=false;
    if FCustomForm=nil then exit;
    for i:=0 to FCustomForm.ComponentCount-1 do begin
      if not ComponentAlignable(FCustomForm.Components[i]) then continue;
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
    if CacheGuideLines then begin
      FGuideLinesCache[glBottom].LineValid:=Result;
      FGuideLinesCache[glBottom].Line:=ALine;
      FGuideLinesCache[glBottom].CacheValid:=true;
    end;
  end;
end;

procedure TControlSelection.InvalidateGuideLinesCache;
var
  t: TGuideLineType;
begin
  InvalidateGuideLines;
  for t:=Low(TGuideLineType) to High(TGuideLineType) do
    FGuideLinesCache[t].CacheValid:=false;
end;

function TControlSelection.ParentLevel: integer;
begin
  if (cssParentLevelNeedsUpdate in FStates) then begin
    if (Count>0) and OnlyVisualComponentsSelected
    and (Items[0].Component is TControl) then
      FParentLevel:=GetParentLevel(TControl(Items[0].Component))
    else
      FParentLevel:=0;
    Exclude(FStates,cssParentLevelNeedsUpdate);
  end;
  Result:=FParentLevel;
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
    Include(FStates,cssChangedDuringLock)
  else
  begin
    Exclude(FStates,cssChangedDuringLock);
    if Assigned(fOnChange) then fOnChange(Self);
  end;
end;

procedure TControlSelection.SetRubberbandActive(const AValue: boolean);
begin
  if RubberbandActive=AValue then exit;
  if AValue then
    Include(FStates,cssRubberbandActive)
  else
    Exclude(FStates,cssRubberbandActive);
end;

procedure TControlSelection.SetRubberbandType(const AValue: TRubberbandType);
begin
  if FRubberbandType=AValue then exit;
  FRubberbandType:=AValue;
end;

procedure TControlSelection.SetSnapping(const AValue: boolean);
begin
  if Snapping=AValue then exit;
  if AValue then
    Include(FStates,cssSnapping)
  else
    Exclude(FStates,cssSnapping);
end;

procedure TControlSelection.SetVisible(const AValue: Boolean);
begin
  if Visible=AValue then exit;
  if AValue then
    Include(FStates,cssVisible)
  else
    Exclude(FStates,cssVisible);
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
  if cssNotSavingBounds in FStates then exit;
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
  NewSelectedControl:=TSelectedControl.Create(Self,AComponent);
  if NewSelectedControl.ParentForm<>FCustomForm then Clear;
  Result:=FControls.Add(NewSelectedControl);
  FStates:=FStates+[cssOnlyNonVisualNeedsUpdate,cssOnlyVisualNeedsUpdate,
                    cssParentLevelNeedsUpdate,cssParentChildFlagsNeedUpdate];
  if Count=1 then SetCustomForm;
  DoChange;
  UpdateBounds;
  SaveBounds;
  EndUpdate;
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
  BeginUpdate;
  if Count=1 then begin
    InvalidateGrabbers;
    InvalidateGuideLines;
  end;
  Items[Index].Free;
  FControls.Delete(Index);
  FStates:=FStates+[cssOnlyNonVisualNeedsUpdate,cssOnlyVisualNeedsUpdate,
                    cssParentLevelNeedsUpdate,cssParentChildFlagsNeedUpdate];
  if Count=0 then SetCustomForm;
  UpdateBounds;
  SaveBounds;
  DoChange;
  EndUpdate;
end;

procedure TControlSelection.Clear;
var i:integer;
begin
  if FControls.Count=0 then exit;
  InvalidateGrabbers;
  InvalidateGuideLines;
  for i:=0 to FControls.Count-1 do Items[i].Free;
  FControls.Clear;
  FStates:=FStates+[cssOnlyNonVisualNeedsUpdate,cssOnlyVisualNeedsUpdate,
                    cssParentLevelNeedsUpdate,cssParentChildFlagsNeedUpdate];
  FCustomForm:=nil;
  UpdateBounds;
  SaveBounds;
  DoChange;
end;

procedure TControlSelection.Assign(AControlSelection:TControlSelection);
var i:integer;
begin
  if (AControlSelection=Self) or (cssNotSavingBounds in FStates) then exit;
  Include(FStates,cssNotSavingBounds);
  BeginUpdate;
  Clear;
  FControls.Capacity:=AControlSelection.Count;
  for i:=0 to AControlSelection.Count-1 do
    Add(AControlSelection[i].Component);
  SetCustomForm;
  UpdateBounds;
  Exclude(FStates,cssNotSavingBounds);
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
  if FActiveGrabber<>nil then
    GrabberPos:=FActiveGrabber.Positions
  else
    GrabberPos:=[gpRight,gpBottom];
  if [gpTop,gpBottom] * GrabberPos = [] then dy:=0;
  if [gpLeft,gpRight] * GrabberPos = [] then dx:=0;
  if (dx=0) and (dy=0) then exit;

  BeginResizing;
  if gpLeft in GrabberPos then begin
    FLeft:=FLeft+dx;
    FWidth:=FWidth-dx;
  end
  else if gpRight in GrabberPos then begin
    FWidth:=FWidth+dx;
  end;
  if gpTop in GrabberPos then begin
    FTop:=FTop+dy;
    FHeight:=FHeight-dy;
  end
  else if gpBottom in GrabberPos then begin
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
var
  OldBrushColor:TColor;
  g:TGrabIndex;
  Diff: TPoint;
  RestoreBrush: boolean;
  
  procedure FillRect(RLeft,RTop,RRight,RBottom: integer);
  begin
    if not DC.RectVisible(RLeft,RTop,RRight,RBottom) then exit;
    if not RestoreBrush then begin
      DC.Save;
      with DC.Canvas do begin
        OldBrushColor:=Brush.Color;
        Brush.Color:=GrabberColor;
      end;
      RestoreBrush:=true;
    end;
    DC.Canvas.FillRect(Rect(RLeft,RTop,RRIght,RBottom));
  end;
  
begin
  if (Count=0) or (FCustomForm=nil)
  or IsSelected(FCustomForm) then exit;
  
  Diff:=DC.FormOrigin;

  {writeln('[DrawGrabbers] '
   ,' DC=',Diff.X,',',Diff.Y
   ,' Grabber1=',FGrabbers[0].Left,',',FGrabbers[0].Top);}

  RestoreBrush:=false;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do
    FillRect(
       FGrabbers[g].Left-Diff.X
      ,FGrabbers[g].Top-Diff.Y
      ,FGrabbers[g].Left-Diff.X+FGrabbers[g].Width
      ,FGrabbers[g].Top-Diff.Y+FGrabbers[g].Height
    );
  Include(FStates,cssGrabbersPainted);
      
  if RestoreBrush then
    DC.Canvas.Brush.Color:=OldBrushColor;
end;

procedure TControlSelection.DrawMarkerAt(DC: TDesignerDeviceContext;
  ALeft, ATop, AWidth, AHeight: integer);
var
  OldBrushColor: TColor;
  RestoreBrush: boolean;
  
  procedure FillRect(RLeft, RTop, RRight, RBottom: integer);
  begin
    if not DC.RectVisible(RLeft, RTop, RRight, RBottom) then exit;
    if not RestoreBrush then begin
      DC.Save;
      OldBrushColor:=DC.Canvas.Brush.Color;
      DC.Canvas.Brush.Color:=MarkerColor;
      RestoreBrush:=true;
    end;
    DC.Canvas.FillRect(Rect(RLeft,RTop,RRight,RBottom));
  end;
  
begin
  RestoreBrush:=false;
  FillRect(ALeft,ATop,ALeft+MarkerSize,ATop+MarkerSize);
  FillRect(ALeft,ATop+AHeight-MarkerSize,ALeft+MarkerSize,ATop+AHeight);
  FillRect(ALeft+AWidth-MarkerSize,ATop,ALeft+AWidth,ATop+MarkerSize);
  FillRect(ALeft+AWidth-MarkerSize,ATop+AHeight-MarkerSize
               ,ALeft+AWidth,ATop+AHeight);
  if RestoreBrush then
    DC.Canvas.Brush.Color:=OldBrushColor;
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
  or (not IsSelected(AComponent)) then exit;
  
  GetComponentBounds(AComponent,CompLeft,CompTop,CompWidth,CompHeight);
  CompOrigin:=GetParentFormRelativeParentClientOrigin(AComponent);
  DCOrigin:=DC.FormOrigin;
  CompLeft:=CompLeft+CompOrigin.X-DCOrigin.X;
  CompTop:=CompTop+CompOrigin.Y-DCOrigin.Y;

{writeln('DrawMarker A ',FCustomForm.Name
    ,' Component',AComponent.Name,',',CompLeft,',',CompLeft
    ,' DCOrigin=',DCOrigin.X,',',DCOrigin.Y
    );}

  DrawMarkerAt(DC,CompLeft,CompTop,CompWidth,CompHeight);
end;

procedure TControlSelection.DrawRubberband(DC: TDesignerDeviceContext);
var
  Diff: TPoint;

  procedure DrawInvertFrameRect(x1,y1,x2,y2:integer);
  var i:integer;
  var
    OldPenColor: TColor;
    RestorePen: boolean;

    procedure InvertPixel(x,y:integer);
    //var c:TColor;
    begin
      //c:=DC.Canvas.Pixels[x,y];
      //c:=c xor $ffffff;
      //DC.Canvas.Pixels[x,y]:=c;
      DC.Canvas.MoveTo(x,y);
      DC.Canvas.LineTo(x+1,y);
    end;
    
    procedure DrawRubberLine(StartX, StartY, EndX, EndY: integer);
    begin
      if not DC.RectVisible(StartX, StartY, EndX, EndY) then exit;
      if not RestorePen then begin
        DC.Save;
        with DC.Canvas do begin
          OldPenColor:=Pen.Color;
          if RubberbandType=rbtSelection then
            Pen.Color:=RubberbandSelectionColor
          else
            Pen.Color:=RubberbandCreationColor;
        end;
        RestorePen:=true;
      end;
      if StartX<EndX then begin
        while StartX<EndX do begin
          InvertPixel(StartX,StartY);
          inc(StartX,3);
        end;
      end else begin
        while StartY<EndY do begin
          InvertPixel(StartX,StartY);
          inc(StartY,3);
        end;
      end;
    end;
    
  begin
    RestorePen:=false;
    if x1>x2 then begin i:=x1; x1:=x2; x2:=i; end;
    if y1>y2 then begin i:=y1; y1:=y2; y2:=i; end;
    DrawRubberLine(x1,y1,x2,y1);
    DrawRubberLine(x1,y2,x2,y2);
    DrawRubberLine(x1,y1,x1,y2);
    DrawRubberLine(x2,y1,x2,y2);
    if RestorePen then
      DC.Canvas.Pen.Color:=OldPenColor;
  end;

// DrawRubberband
begin
  Diff:=DC.FormOrigin;
  with FRubberBandBounds do
    DrawInvertFrameRect(Left-Diff.X,Top-Diff.Y,Right-Diff.X,Bottom-Diff.Y);
end;

procedure TControlSelection.SelectWithRubberBand(ACustomForm:TCustomForm; 
  ClearBefore, ExclusiveOr:boolean; var SelectionChanged: boolean;
  MaxParentControl: TControl);
var i:integer;

  function ControlInRubberBand(AComponent:TComponent):boolean;
  var
    ALeft, ATop, ARight, ABottom: integer;
    Origin: TPoint;
    AControl: TControl;
  begin
    Result:=false;
    if (AComponent is TMenuItem) then exit;
    if (AComponent is TControl) then begin
      AControl:=TControl(AComponent);
      // check if control is visible on form
      if not ControlIsDesignerVisible(AControl) then exit;
      // check if control
      if (MaxParentControl<>nil) then begin
        // select only controls, that are childs of MaxParentControl
        if (not MaxParentControl.IsParentOf(AControl)) then exit;
        // check if control is a grand child
        if (not EnvironmentOptions.RubberbandSelectsGrandChilds)
        and (AControl.Parent<>MaxParentControl) then exit;
      end;
    end;
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
    if IsSelected(ACustomForm) then begin
      Remove(ACustomForm);
      SelectionChanged:=true;
    end;
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
  with ARect do begin
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
  if (FRubberBandBounds.Left<>ARect.Left)
  or (FRubberBandBounds.Top<>ARect.Top)
  or (FRubberBandBounds.Right<>ARect.Right)
  or (FRubberBandBounds.Bottom<>ARect.Bottom)
  then begin
    if (FCustomForm<>nil) and RubberbandActive then
      InvalidateFrame(FCustomForm.Handle,FRubberBandBounds);
    FRubberBandBounds:=ARect;
    if (FCustomForm<>nil) and RubberbandActive then
      InvalidateFrame(FCustomForm.Handle,FRubberBandBounds);
  end;
end;

function TControlSelection.OnlyNonVisualComponentsSelected: boolean;
var i: integer;
begin
  if cssOnlyNonVisualNeedsUpdate in FStates then begin
    Result:=true;
    for i:=0 to FControls.Count-1 do
      Result:=Result and (not (Items[i].Component is TControl));
    if Result then
      Include(FStates,cssOnlyNonVisualSelected)
    else
      Exclude(FStates,cssOnlyNonVisualSelected);
    Exclude(FStates,cssOnlyNonVisualNeedsUpdate);
  end else
    Result:=cssOnlyNonVisualSelected in FStates;
end;

function TControlSelection.OnlyVisualComponentsSelected: boolean;
var i: integer;
begin
  if cssOnlyVisualNeedsUpdate in FStates then begin
    Result:=true;
    for i:=0 to FControls.Count-1 do
      Result:=Result and (Items[i].Component is TControl);
    if Result then
      Include(FStates,cssOnlyVisualNeedsSelected)
    else
      Exclude(FStates,cssOnlyVisualNeedsSelected);
    Exclude(FStates,cssOnlyVisualNeedsUpdate);
  end else
    Result:=cssOnlyVisualNeedsSelected in FStates;
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
  OldPenColor:TColor;
  RestorePen: boolean;

  procedure DrawLine(ARect: TRect; AColor: TColor);
  begin
    dec(ARect.Left,DCOrigin.X);
    dec(ARect.Top,DCOrigin.Y);
    dec(ARect.Right,DCOrigin.X);
    dec(ARect.Bottom,DCOrigin.Y);
    if not DC.RectVisible(ARect.Left,ARect.Top,ARect.Right,ARect.Bottom) then
      exit;
    if not RestorePen then begin
      DC.Save;
      OldPenColor:=DC.Canvas.Pen.Color;
      RestorePen:=true;
    end;
    with DC.Canvas do begin
      Pen.Color:=AColor;
      MoveTo(ARect.Left,ARect.Top);
      LineTo(ARect.Right,ARect.Bottom);
    end;
  end;

var
  LineExists: array[TGuideLineType] of boolean;
  Line: array[TGuideLineType] of TRect;
  g: TGuideLineType;
begin
  if (Count=0) or (FCustomForm=nil) or Items[0].IsTopLvl then exit;
  LineExists[glLeft]:=GetLeftGuideLine(Line[glLeft]);
  LineExists[glRight]:=GetRightGuideLine(Line[glRight]);
  LineExists[glTop]:=GetTopGuideLine(Line[glTop]);
  LineExists[glBottom]:=GetBottomGuideLine(Line[glBottom]);
  if (not LineExists[glLeft]) and (not LineExists[glRight])
  and (not LineExists[glTop]) and (not LineExists[glBottom])
  then exit;
  
  RestorePen:=false;
  
  DC.Save;
  DCOrigin:=DC.FormOrigin;
  OldPenColor:=DC.Canvas.Pen.Color;
  // draw bottom guideline
  if LineExists[glBottom] then
    DrawLine(Line[glBottom],EnvironmentOptions.GuideLineColorRightBottom);
  // draw top guideline
  if LineExists[glTop] then
    DrawLine(Line[glTop],EnvironmentOptions.GuideLineColorLeftTop);
  // draw right guideline
  if LineExists[glRight] then
    DrawLine(Line[glRight],EnvironmentOptions.GuideLineColorRightBottom);
  // draw left guideline
  if LineExists[glLeft] then
    DrawLine(Line[glLeft],EnvironmentOptions.GuideLineColorLeftTop);
    
  for g:=Low(g) to High(g) do begin
    FGuideLinesCache[g].PaintedLineValid:=LineExists[g];
    FGuideLinesCache[g].PaintedLine:=Line[g];
  end;
    
  if RestorePen then
    DC.Canvas.Pen.Color:=OldPenColor;
  Include(FStates,cssGuideLinesPainted);
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
