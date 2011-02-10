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
  Classes, SysUtils, Math, LCLIntf, LCLType, LCLProc, Controls, Forms,
  GraphType, Graphics, Menus, EnvironmentOpts, PropEditUtils,
  FormEditingIntf, NonControlDesigner, DesignerProcs;

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
    scfChildInSelection,
    scfMarkersPainted
    );
  TSelectedControlFlags = set of TSelectedControlFlag;

  { TSelectedControl }

  TSelectedControl = class
  private
    FCachedFormRelativeLeftTop: TPoint;
    FCachedHeight: integer;
    FCachedLeft: integer;
    FCachedTop: integer;
    FCachedWidth: integer;
    FDesignerForm: TCustomForm;
    FFlags: TSelectedControlFlags;
    FIsNonVisualComponent: boolean;
    FIsTComponent: boolean;
    FIsTControl: boolean;
    FIsTWinControl: boolean;
    FIsVisible: boolean;
    FMarkerPaintedBounds: TRect;
    FOldFormRelativeLeftTop: TPoint;
    FOldHeight: integer;
    FOldLeft: integer;
    FOldTop: integer;
    FOldWidth: integer;
    FOwner: TControlSelection;
    FPersistent: TPersistent;
    FUseCache: boolean;
    FUsedHeight: integer;
    FUsedLeft: integer;
    FUsedTop: integer;
    FUsedWidth: integer;
    function GetLeft: integer;
    procedure SetLeft(ALeft: integer);
    function GetTop: integer;
    procedure SetTop(ATop: integer);
    function GetWidth: integer;
    procedure SetUseCache(const AValue: boolean);
    procedure SetWidth(AWidth: integer);
    function GetHeight: integer;
    procedure SetHeight(AHeight: integer);
  public
    constructor Create(AnOwner: TControlSelection; APersistent: TPersistent);
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer);
    procedure SetFormRelativeBounds(ALeft, ATop, AWidth, AHeight: integer);
    procedure GetFormRelativeBounds(var ALeft, ATop, AWidth, AHeight: integer;
                                    StoreAsUsed: boolean = false);
    procedure SetUsedBounds(ALeft, ATop, AWidth, AHeight: integer);
    procedure SaveBounds;
    procedure UpdateCache;
    function IsTopLvl: boolean;
    function ChildInSelection: boolean;
    function ParentInSelection: boolean;
    procedure InvalidateNonVisualPersistent;

    property Persistent: TPersistent read FPersistent;
    property Owner: TControlSelection read FOwner;
    property Left: integer read GetLeft write SetLeft;
    property Top: integer read GetTop write SetTop;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property OldLeft:integer read FOldLeft write FOldLeft;
    property OldTop:integer read FOldTop write FOldTop;
    property OldWidth:integer read FOldWidth write FOldWidth;
    property OldHeight:integer read FOldHeight write FOldHeight;
    property OldFormRelativeLeftTop: TPoint read FOldFormRelativeLeftTop
                                            write FOldFormRelativeLeftTop;
    property UsedLeft: integer read FUsedLeft write FUsedLeft;// form relative (not Left)
    property UsedTop: integer read FUsedTop write FUsedTop;// form relative (not Top)
    property UsedWidth: integer read FUsedWidth write FUsedWidth;
    property UsedHeight: integer read FUsedHeight write FUsedHeight;
    property Flags: TSelectedControlFlags read FFlags write FFlags;
    property UseCache: boolean read FUseCache write SetUseCache;
    property IsVisible: boolean read FIsVisible;
    property IsTComponent: boolean read FIsTComponent;
    property IsTControl: boolean read FIsTControl;
    property IsTWinControl: boolean read FIsTWinControl;
    property IsNonVisualComponent: boolean read FIsNonVisualComponent;
    property DesignerForm: TCustomForm read FDesignerForm;
    property MarkerPaintedBounds: TRect read FMarkerPaintedBounds write FMarkerPaintedBounds;
  end;


  TComponentAlignment = (
    csaNone,
    csaSides1,
    csaCenters,
    csaSides2,
    csaCenterInWindow,
    csaSpaceEqually,
    csaSide1SpaceEqually,
    csaSide2SpaceEqually
    );
  TComponentSizing = (
    cssNone,
    cssShrinkToSmallest,
    cssGrowToLargest,
    cssFixed
    );
  TSelectionSortCompare = function(Index1, Index2: integer): integer of object;
  TOnSelectionFormChanged = procedure(Sender: TObject;
    OldForm, NewForm: TCustomForm) of object;
  TOnSelectionUpdate = procedure(Sender: TObject; ForceUpdate: Boolean) of object;

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
    cssLookupRootSelected,
    cssOnlyNonVisualNeedsUpdate,
    cssOnlyNonVisualSelected,
    cssOnlyVisualNeedsUpdate,
    cssOnlyVisualNeedsSelected,
    cssOnlyInvisibleNeedsUpdate,
    cssOnlyInvisibleSelected,
    cssOnlyBoundLessNeedsUpdate,
    cssOnlyBoundLessSelected,
    cssBoundsNeedsUpdate,
    cssBoundsNeedsSaving,
    cssParentLevelNeedsUpdate,
    cssNotSavingBounds,
    cssSnapping,
    cssChangedDuringLock,
    cssRubberbandActive,
    cssRubberbandPainted,
    cssCacheGuideLines,
    cssVisible,
    cssParentChildFlagsNeedUpdate,
    cssGrabbersPainted,
    cssGuideLinesPainted
    );
  TControlSelStates = set of TControlSelState;
  
const
  cssSelectionChangeFlags =
   [cssOnlyNonVisualNeedsUpdate,cssOnlyVisualNeedsUpdate,
    cssOnlyInvisibleNeedsUpdate,cssOnlyBoundLessNeedsUpdate,
    cssParentLevelNeedsUpdate,cssParentChildFlagsNeedUpdate];

type

  { TControlSelection }

  TControlSelection = class(TComponent)
  private
    FControls: TList;  // list of TSelectedControl
    FMediator: TDesignerMediator;

    // current bounds of the selection (only valid if Count>0)
    // These are the values set by the user
    // But due to snapping and lcl aligning the components can have other bounds
    FLeft: Integer;
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

    FActiveGrabber: TGrabber;
    FForm: TCustomForm;// form to draw on (not necessarily the root)
    FGrabbers: array[TGrabIndex] of TGrabber;
    FGrabberSize: integer;
    FMarkerSize: integer;
    FOnChange: TOnSelectionUpdate;
    FOnPropertiesChanged: TNotifyEvent;
    FOnSelectionFormChanged: TOnSelectionFormChanged;
    FResizeLockCount: integer;
    FRubberBandBounds: TRect;
    FRubberbandCreationColor: TColor;
    FRubberbandSelectionColor: TColor;
    FRubberbandType: TRubberbandType;
    FLookupRoot: TComponent;// component owning the selected components
    FStates: TControlSelStates;
    FUpdateLock: integer;

    function CompareBottom(Index1, Index2: integer): integer;
    function CompareHorCenter(Index1, Index2: integer): integer;
    function CompareInts(i1, i2: integer): integer;
    function CompareLeft(Index1, Index2: integer): integer;
    function CompareRight(Index1, Index2: integer): integer;
    function CompareTop(Index1, Index2: integer): integer;
    function CompareVertCenter(Index1, Index2: integer): integer;
    function GetCacheGuideLines: boolean;
    function GetGrabberColor: TColor;
    function GetGrabbers(AGrabIndex:TGrabIndex): TGrabber;
    function GetItems(Index:integer):TSelectedControl;
    function GetMarkerColor: TColor;
    function GetRubberbandActive: boolean;
    function GetRubberbandCreationColor: TColor;
    function GetRubberbandSelectionColor: TColor;
    function GetSelectionOwner: TComponent;
    function GetSnapping: boolean;
    function GetVisible: boolean;
    procedure DoChangeProperties;
    procedure GrabberMove(Grabber: TGrabber; const OldRect, NewRect: TRect);
    procedure SetActiveGrabber(AGrabber: TGrabber);
    procedure SetCacheGuideLines(const AValue: boolean);
    procedure SetCustomForm;
    procedure SetGrabbers(AGrabIndex: TGrabIndex; const AGrabber: TGrabber);
    procedure SetGrabberSize(const NewSize: integer);
    procedure SetItems(Index:integer; ASelectedControl:TSelectedControl);
    procedure SetRubberbandActive(const AValue: boolean);
    procedure SetRubberBandBounds(ARect: TRect);
    procedure SetRubberbandType(const AValue: TRubberbandType);
    procedure SetSnapping(const AValue: boolean);
    procedure SetVisible(const AValue: Boolean);
  protected
    procedure AdjustGrabbers;
    procedure InvalidateGrabbers;
    procedure InvalidateGuideLines;
    procedure DoApplyUserBounds;
    procedure UpdateRealBounds;
    procedure UpdateParentChildFlags;
    procedure DoDrawMarker(Index: integer; DC: TDesignerDeviceContext);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
           override;

    // snapping
    function CleanGridSizeX: integer;
    function CleanGridSizeY: integer;
    function PersistentAlignable(APersistent: TPersistent): boolean;
    function GetBottomGuideLine(var ALine: TRect): boolean;
    function GetLeftGuideLine(var ALine: TRect): boolean;
    function GetRightGuideLine(var ALine: TRect): boolean;
    function GetTopGuideLine(var ALine: TRect): boolean;
    procedure FindNearestBottomGuideLine(var NearestInt: TNearestInt);
    procedure FindNearestClientLeftRight(var NearestInt: TNearestInt);
    procedure FindNearestClientTopBottom(var NearestInt: TNearestInt);
    procedure FindNearestGridX(var NearestInt: TNearestInt);
    procedure FindNearestGridY(var NearestInt: TNearestInt);
    procedure FindNearestLeftGuideLine(var NearestInt: TNearestInt);
    procedure FindNearestOldBottom(var NearestInt: TNearestInt);
    procedure FindNearestOldLeft(var NearestInt: TNearestInt);
    procedure FindNearestOldRight(var NearestInt: TNearestInt);
    procedure FindNearestOldTop(var NearestInt: TNearestInt);
    procedure FindNearestRightGuideLine(var NearestInt: TNearestInt);
    procedure FindNearestTopGuideLine(var NearestInt: TNearestInt);
    procedure ImproveNearestInt(var NearestInt: TNearestInt; Candidate: integer);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure OnIdle(Sender: TObject; var Done: Boolean);

    // items
    property Items[Index:integer]:TSelectedControl
      read GetItems write SetItems; default;
    function Count:integer;
    procedure Sort(SortProc: TSelectionSortCompare);

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure DoChange(ForceUpdate: Boolean = False);
    property UpdateLock: integer read FUpdateLock;

    function IndexOf(APersistent: TPersistent):integer;
    function Add(APersistent: TPersistent):integer;
    procedure Remove(APersistent: TPersistent);
    procedure Delete(Index:integer);
    procedure Clear;
    function Equals(const ASelection: TPersistentSelectionList): boolean; reintroduce;
    function AssignPersistent(APersistent: TPersistent): boolean;
    procedure Assign(AControlSelection: TControlSelection); reintroduce;
    procedure AssignSelection(const ASelection: TPersistentSelectionList);
    function IsSelected(APersistent: TPersistent): Boolean;
    function IsOnlySelected(APersistent: TPersistent): Boolean;
    function ParentLevel: integer;
    function OnlyNonVisualPersistentsSelected: boolean;
    function OnlyVisualComponentsSelected: boolean;
    function OnlyInvisiblePersistentsSelected: boolean;
    function OnlyBoundLessComponentsSelected: boolean;
    function LookupRootSelected: boolean;

    // resizing, moving, aligning, mirroring, ...
    function IsResizing: boolean;
    procedure BeginResizing;
    procedure EndResizing(ApplyUserBounds: boolean);
    procedure SaveBounds;
    procedure UpdateBounds;
    procedure RestoreBounds;

    function MoveSelection(dx, dy: integer; TotalDeltas: Boolean): Boolean;
    function MoveSelectionWithSnapping(TotalDx, TotalDy: integer): Boolean;
    procedure SizeSelection(dx, dy: integer);
    procedure SetBounds(NewLeft,NewTop,NewWidth,NewHeight: integer);
    procedure AlignComponents(HorizAlignment,VertAlignment:TComponentAlignment);
    procedure MirrorHorizontal;
    procedure MirrorVertical;
    procedure SizeComponents(HorizSizing: TComponentSizing; AWidth: integer;
                             VertSizing: TComponentSizing; AHeight: integer);
    procedure ScaleComponents(Percent: integer);
    function CheckForLCLChanges(Update: boolean): boolean;

    // snapping
    function FindNearestSnapLeft(ALeft, AWidth: integer): integer;
    function FindNearestSnapLeft(ALeft: integer): integer;
    function FindNearestSnapRight(ARight: integer): integer;
    function FindNearestSnapTop(ATop, AHeight: integer): integer;
    function FindNearestSnapTop(ATop: integer): integer;
    function FindNearestSnapBottom(ABottom: integer): integer;
    function SnapGrabberMousePos(const CurMousePos: TPoint): TPoint;
    property Snapping: boolean read GetSnapping write SetSnapping;
    procedure DrawGuideLines(DC: TDesignerDeviceContext);
    property CacheGuideLines: boolean
      read GetCacheGuideLines write SetCacheGuideLines;
    procedure InvalidateGuideLinesCache;

    // grabbers and markers
    property GrabberSize: integer read FGrabberSize write SetGrabberSize;
    property GrabberColor: TColor read GetGrabberColor;
    procedure DrawGrabbers(DC: TDesignerDeviceContext);
    function GrabberAtPos(X,Y: integer):TGrabber;
    property Grabbers[AGrabIndex: TGrabIndex]:TGrabber
      read GetGrabbers write SetGrabbers;
    property MarkerSize:integer read FMarkerSize write FMarkerSize;
    property MarkerColor: TColor read GetMarkerColor;
    property OnChange: TOnSelectionUpdate read FOnChange write FOnChange;
    property OnPropertiesChanged: TNotifyEvent
      read FOnPropertiesChanged write FOnPropertiesChanged;
    procedure DrawMarker(AComponent: TComponent; DC: TDesignerDeviceContext);
    procedure DrawMarkerAt(DC: TDesignerDeviceContext;
      ALeft, ATop, AWidth, AHeight: integer);
    procedure DrawMarkers(DC: TDesignerDeviceContext);
    property ActiveGrabber: TGrabber read FActiveGrabber write SetActiveGrabber;
    procedure InvalidateMarkers;
    procedure InvalidateMarkersForComponent(AComponent: TComponent);

    // user wished bounds:
    property Left:integer read FLeft;
    property Top:integer read FTop;
    property Width:integer read FWidth;
    property Height:integer read FHeight;

    // real current bounds
    property RealLeft:integer read FRealLeft;
    property RealTop:integer read FRealTop;
    property RealWidth:integer read FRealWidth;
    property RealHeight:integer read FRealHeight;

    // bounds before resizing
    property OldLeft:integer read FOldLeft;
    property OldTop:integer read FOldTop;
    property OldWidth:integer read FOldWidth;
    property OldHeight:integer read FOldHeight;

    // rubberband
    property RubberbandBounds:TRect read FRubberbandBounds
                                    write SetRubberbandBounds;
    property RubberbandActive: boolean read GetRubberbandActive
                                       write SetRubberbandActive;
    property RubberbandType: TRubberbandType read FRubberbandType
                                             write SetRubberbandType;
    property RubberbandSelectionColor: TColor read GetRubberbandSelectionColor;
    property RubberbandCreationColor: TColor read GetRubberbandCreationColor;
    procedure DrawRubberband(DC: TDesignerDeviceContext);

    procedure SelectAll(ALookupRoot: TComponent);
    procedure SelectWithRubberBand(ALookupRoot: TComponent;
                                   AMediator: TDesignerMediator;
                                   ClearBefore, ExclusiveOr: boolean;
                                   var SelectionChanged: boolean;
                                   MaxParentComponent: TComponent);

    property Visible:boolean read GetVisible write SetVisible;

    property SelectionForm: TCustomForm read FForm;
    property Mediator: TDesignerMediator read FMediator;
    property OnSelectionFormChanged: TOnSelectionFormChanged
      read FOnSelectionFormChanged write FOnSelectionFormChanged;
    property LookupRoot: TComponent read FLookupRoot;
  end;



var TheControlSelection: TControlSelection;


implementation


const
  GRAB_CURSOR: array[TGrabIndex] of TCursor = (
    crSizeNW,   crSizeN,  crSizeNE,
    crSizeW,              crSizeE,
    crSizeSW,   crSizeS,  crSizeSE
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
  if AForm.HandleAllocated then
    InvalidateDesignerRect(AForm.Handle,@ARect);
end;


{ TSelectedControl }

constructor TSelectedControl.Create(AnOwner: TControlSelection;
  APersistent: TPersistent);
begin
  inherited Create;
  FOwner:=AnOwner;
  FPersistent:=APersistent;
  FIsTComponent:=FPersistent is TComponent;
  FIsTControl:=FPersistent is TControl;
  FIsTWinControl:=FPersistent is TWinControl;
  FIsNonVisualComponent:=FIsTComponent and (not FIsTControl);
  if (Owner.Mediator<>nil) and FIsTComponent then
    FIsNonVisualComponent:=Owner.Mediator.ComponentIsIcon(TComponent(FPersistent));
  FDesignerForm:=GetDesignerForm(FPersistent);
  FIsVisible:=FIsTComponent
              and (not ComponentIsInvisible(TComponent(APersistent)));
end;

destructor TSelectedControl.Destroy;
begin
  inherited Destroy;
end;

procedure TSelectedControl.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  if FIsTControl then begin
    TControl(FPersistent).SetBounds(ALeft, ATop, AWidth, AHeight);
    FCachedLeft:=ALeft;
    FCachedTop:=ATop;
    FCachedWidth:=AWidth;
    FCachedHeight:=AHeight;
  end else if FIsNonVisualComponent then begin
    if (Left<>ALeft) or (Top<>ATop) then begin
      InvalidateNonVisualPersistent;
      Left:=ALeft;
      Top:=ATop;
      InvalidateNonVisualPersistent;
    end;
  end else if (Owner.Mediator<>nil) and FIsTComponent then begin
    FCachedLeft:=ALeft;
    FCachedTop:=ATop;
    FCachedWidth:=AWidth;
    FCachedHeight:=AHeight;
    Owner.Mediator.SetBounds(TComponent(FPersistent),Bounds(ALeft,ATop,AWidth,AHeight));
  end;
end;

procedure TSelectedControl.SetFormRelativeBounds(ALeft, ATop, AWidth,
  AHeight: integer);
var
  ParentOffset: TPoint;
  OldBounds: TRect;
begin
  if not FIsTComponent then exit;
  if Owner.Mediator <> nil then 
  begin
    Owner.Mediator.GetBounds(TComponent(FPersistent),OldBounds);
    ParentOffset:=Owner.Mediator.GetComponentOriginOnForm(TComponent(FPersistent));
    dec(ParentOffset.X,OldBounds.Left);
    dec(ParentOffset.Y,OldBounds.Top);
    Owner.Mediator.SetBounds(TComponent(FPersistent),
      Bounds(ALeft-ParentOffset.X,ATop-ParentOffset.Y,AWidth,AHeight));
  end 
  else 
  begin
    ParentOffset := GetParentFormRelativeParentClientOrigin(TComponent(FPersistent));
    SetBounds(ALeft - ParentOffset.X, ATop - ParentOffset.Y, AWidth, AHeight);
  end;
end;

procedure TSelectedControl.GetFormRelativeBounds(var ALeft, ATop, AWidth,
  AHeight: integer; StoreAsUsed: boolean);
var
  ALeftTop: TPoint;
  CurBounds: TRect;
begin
  if FIsTComponent then
  begin
    if Owner.Mediator<>nil then begin
      ALeftTop:=Owner.Mediator.GetComponentOriginOnForm(TComponent(FPersistent));
      Owner.Mediator.GetBounds(TComponent(FPersistent),CurBounds);
      ALeft:=ALeftTop.X;
      ATop:=ALeftTop.Y;
      AWidth:=CurBounds.Right-CurBounds.Left;
      AHeight:=CurBounds.Bottom-CurBounds.Top;
    end else begin
      ALeftTop := GetParentFormRelativeTopLeft(TComponent(FPersistent));
      ALeft := ALeftTop.X;
      ATop := ALeftTop.Y;
      AWidth := GetComponentWidth(TComponent(FPersistent));
      AHeight := GetComponentHeight(TComponent(FPersistent));
    end;
  end else
  begin
    ALeft := 0;
    ATop := 0;
    AWidth := 0;
    AHeight := 0;
  end;
  if StoreAsUsed then
    SetUsedBounds(ALeft, ATop, AWidth, AHeight);
end;

procedure TSelectedControl.SetUsedBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  FUsedLeft:=ALeft;
  FUsedTop:=ATop;
  FUsedWidth:=AWidth;
  FUsedHeight:=AHeight;
end;

procedure TSelectedControl.SaveBounds;
var
  r: TRect;
begin
  if not FIsTComponent then exit;
  if Owner.Mediator<>nil then begin
    Owner.Mediator.GetBounds(TComponent(FPersistent),r);
    FOldLeft:=r.Left;
    FOldTop:=r.Top;
    FOldWidth:=r.Right-r.Left;
    FOldHeight:=r.Bottom-r.Top;
  end else begin
    GetComponentBounds(TComponent(FPersistent),
                       FOldLeft,FOldTop,FOldWidth,FOldHeight);
    FOldFormRelativeLeftTop:=
                          GetParentFormRelativeTopLeft(TComponent(FPersistent));
  end;
end;

procedure TSelectedControl.UpdateCache;
begin
  if not FIsTComponent then exit;
  GetComponentBounds(TComponent(FPersistent),
                     FCachedLeft,FCachedTop,FCachedWidth,FCachedHeight);
  FCachedFormRelativeLeftTop:=
                          GetParentFormRelativeTopLeft(TComponent(FPersistent));
end;

function TSelectedControl.IsTopLvl: boolean;
begin
  Result:=(not FIsTComponent)
          or (TComponent(FPersistent).Owner=nil)
          or (FIsTControl and (TControl(FPersistent).Parent=nil));
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

procedure TSelectedControl.InvalidateNonVisualPersistent;
var
  AForm: TCustomForm;
  CompRect: TRect;
begin
  AForm := DesignerForm;
  if (AForm = nil) or (not FIsTComponent) then Exit;

  CompRect.Left := LeftFromDesignInfo(TComponent(FPersistent).DesignInfo);
  CompRect.Top := TopFromDesignInfo(TComponent(FPersistent).DesignInfo);
  CompRect.Right := CompRect.Left+NonVisualCompWidth;
  CompRect.Bottom := CompRect.Top+NonVisualCompWidth;

  if AForm.HandleAllocated then
    InvalidateDesignerRect(AForm.Handle, @CompRect);
end;

function TSelectedControl.GetLeft: integer;
var
  r: TRect;
begin
  if FUseCache then
    Result:=FCachedLeft
  else if FIsTComponent then begin
    if Owner.Mediator<>nil then begin
      Owner.Mediator.GetBounds(TComponent(FPersistent),r);
      Result:=r.Left;
    end else begin
      Result:=GetComponentLeft(TComponent(FPersistent))
    end;
  end else
    Result:=0;
end;

procedure TSelectedControl.SetLeft(ALeft: Integer);
var
  r: TRect;
begin
  if FIsTControl then
    TControl(FPersistent).Left := Aleft
  else
  if FIsTComponent then
  begin
    if Owner.Mediator<>nil then begin
      Owner.Mediator.GetBounds(TComponent(FPersistent),r);
      r.Left:=ALeft;
      Owner.Mediator.SetBounds(TComponent(FPersistent),r)
    end else begin
      ALeft := Max(Low(SmallInt), Min(ALeft, High(SmallInt)));
      TComponent(FPersistent).DesignInfo := LeftTopToDesignInfo(ALeft, Top);
    end;
  end;
     
  FCachedLeft := ALeft;
end;

function TSelectedControl.GetTop: integer;
var
  r: TRect;
begin
  if FUseCache then
    Result := FCachedTop
  else 
  if FIsTComponent then begin
    if Owner.Mediator<>nil then begin
      Owner.Mediator.GetBounds(TComponent(FPersistent),r);
      Result:=r.Top;
    end else begin
      Result := GetComponentTop(TComponent(FPersistent));
    end;
  end else
    Result := 0;
end;

procedure TSelectedControl.SetTop(ATop: integer);
var
  r: TRect;
begin
  if FIsTControl then
    TControl(FPersistent).Top := ATop
  else
  if FIsTComponent then
  begin
    if Owner.Mediator<>nil then begin
      Owner.Mediator.GetBounds(TComponent(FPersistent),r);
      r.Top:=ATop;
      Owner.Mediator.SetBounds(TComponent(FPersistent),r);
    end else begin
      ATop := Max(Low(SmallInt), Min(ATop, High(SmallInt)));
      TComponent(FPersistent).DesignInfo := LeftTopToDesignInfo(Left, ATop);
    end;
  end;
    
  FCachedTop := ATop;
end;

function TSelectedControl.GetWidth: integer;
var
  r: TRect;
begin
  if FUseCache then
    Result := FCachedWidth
  else 
  if FIsTComponent then begin
    if Owner.Mediator<>nil then begin
      Owner.Mediator.GetBounds(TComponent(FPersistent),r);
      Result:=r.Right-r.Left;
    end else begin
      Result := GetComponentWidth(TComponent(FPersistent));
    end;
  end;
end;

procedure TSelectedControl.SetUseCache(const AValue: boolean);
begin
  if FUseCache=AValue then exit;
  FUseCache:=AValue;
  if FUseCache then UpdateCache;
end;

procedure TSelectedControl.SetWidth(AWidth: integer);
var
  r: TRect;
begin
  if FIsTControl then
    TControl(FPersistent).Width:=AWidth
  else if FIsTComponent and (Owner.Mediator<>nil) then begin
    Owner.Mediator.GetBounds(TComponent(FPersistent),r);
    r.Right:=r.Left+AWidth;
    Owner.Mediator.SetBounds(TComponent(FPersistent),r);
  end;
  FCachedWidth:=AWidth;
end;

function TSelectedControl.GetHeight: integer;
var
  r: TRect;
begin
  if FUseCache then
    Result := FCachedHeight
  else 
  if FIsTComponent then begin
    if Owner.Mediator<>nil then begin
      Owner.Mediator.GetBounds(TComponent(FPersistent),r);
      Result:=r.Bottom-r.Top;
    end else begin
      Result := GetComponentHeight(TComponent(FPersistent));
    end;
  end else
    Result:=0;
end;

procedure TSelectedControl.SetHeight(AHeight: integer);
var
  r: TRect;
begin
  if FIsTControl then
    TControl(FPersistent).Height:=AHeight
  else if FIsTComponent and (Owner.Mediator<>nil) then begin
    Owner.Mediator.GetBounds(TComponent(FPersistent),r);
    r.Bottom:=r.Top+AHeight;
    Owner.Mediator.SetBounds(TComponent(FPersistent),r);
  end;
  FCachedHeight:=AHeight;
end;


{ TControlSelection }

constructor TControlSelection.Create;
var g:TGrabIndex;
begin
  inherited Create(nil);
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
  FForm:=nil;
  FLookupRoot:=nil;
  FActiveGrabber:=nil;
  FUpdateLock:=0;
  FStates:=[cssOnlyNonVisualNeedsUpdate,cssOnlyVisualNeedsUpdate,
            cssOnlyInvisibleNeedsUpdate,cssOnlyBoundLessNeedsUpdate,
            cssParentLevelNeedsUpdate,cssCacheGuideLines];
  FRubberbandType:=rbtSelection;
  FRubberbandCreationColor:=clMaroon;
  FRubberbandSelectionColor:=clNavy;
  Application.AddOnIdleHandler(@OnIdle);
end;

destructor TControlSelection.Destroy;
var g:TGrabIndex;
begin
  Application.RemoveAllHandlersOfObject(Self);
  Clear;
  FControls.Free;
  for g:=Low(TGrabIndex) to High(TGrabIndex) do FGrabbers[g].Free;
  inherited Destroy;
end;

procedure TControlSelection.OnIdle(Sender: TObject; var Done: Boolean);
begin
  CheckForLCLChanges(true);
end;

procedure TControlSelection.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TControlSelection.EndUpdate;
begin
  if FUpdateLock<=0 then begin
    DebugLn('WARNING: TControlSelection.EndUpdate FUpdateLock=',IntToStr(FUpdateLock));
    exit;
  end;
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
  if FResizeLockCount<=0 then begin
    DebugLn('WARNING: TControlSelection.EndResizing FResizeLockCount=',IntToStr(FResizeLockCount));
    exit;
  end;
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
  if FForm=nil then exit;
  {if Grabber.Positions=[gpTop,gpLeft] then begin
    writeln('TControlSelection.GrabberMove ',
      ' OldRect=',OldRect.Left,',',OldRect.Top,',',OldRect.Right,',',OldRect.Bottom,
      ' NewRect=',NewRect.Left,',',NewRect.Top,',',NewRect.Right,',',NewRect.Bottom,
      ' ');
  end;}
  if FForm.HandleAllocated then begin
    InvalidateDesignerRect(FForm.Handle,@OldRect);
    InvalidateDesignerRect(FForm.Handle,@NewRect);
  end;
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
    NewCustomForm:=Items[0].DesignerForm
  else
    NewCustomForm:=nil;
  if NewCustomForm=FForm then exit;
  // form changed
  InvalidateGuideLines;
  InvalidateGrabbers;
  OldCustomForm:=FForm;
  FForm:=NewCustomForm;
  if FForm is TNonControlDesignerForm then
    FMediator:=TNonControlDesignerForm(FForm).Mediator
  else
    FMediator:=nil;
  FLookupRoot:=GetSelectionOwner;
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

procedure TControlSelection.RestoreBounds;
var
  i: integer;
  OldLeftTop: TPoint;
begin
  BeginUpdate;
  FLeft := OldLeft;
  FTop := OldTop;
  FWidth := FOldWidth;
  FHeight := FOldHeight;
  for i := 0 to Count - 1 do
  begin
    with Items[i] do
    begin
      OldLeftTop := OldFormRelativeLeftTop;
      SetFormRelativeBounds(OldLeftTop.X, OldLeftTop.Y, OldWidth, OldHeight);
    end;
    InvalidateGuideLinesCache;
  end;
  UpdateRealBounds;
  EndUpdate;
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
      FGrabbers[g].InvalidateOnForm(FForm);
    Exclude(FStates,cssGrabbersPainted);
  end;
end;

procedure TControlSelection.InvalidateGuideLines;
var
  g: TGuideLineType;
  LineRect: TRect;
begin
  if (FForm=nil) or (not FForm.HandleAllocated) then exit;
  if (cssGuideLinesPainted in FStates) then begin
    if (FForm<>nil) and CacheGuideLines then
      for g:=Low(TGuideLineType) to High(TGuideLineType) do begin
        if FGuideLinesCache[g].PaintedLineValid then
        begin
          LineRect:=FGuideLinesCache[g].PaintedLine;
          if LineRect.Top=LineRect.Bottom then inc(LineRect.Bottom);
          if LineRect.Left=LineRect.Right then inc(LineRect.Right);
          InvalidateDesignerRect(FForm.Handle,@LineRect);
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
    DebugLn('[TControlSelection.DoApplyUserBounds] S Old=',
      DbgS(FOldLeft,FOldTop,FOldWidth,FOldHeight),
      ' User=',Dbgs(FLeft,FTop,FWidth,FHeight));
    {$ENDIF}
    //DebugLn(['TControlSelection.DoApplyUserBounds BEFORE ',Items[0].Left,' ',Items[0].Top]);
    Items[0].SetFormRelativeBounds(
      Min(NewLeft,NewRight),
      Min(NewTop,NewBottom),
      Abs(FWidth),
      Abs(FHeight)
    );
    //DebugLn(['TControlSelection.DoApplyUserBounds AFTER ',Items[0].Left,' ',Items[0].Top]);
    InvalidateGuideLinesCache;
  end else if Count>1 then begin
    // multi selection
    {$IFDEF VerboseDesigner}
    DebugLn('[TControlSelection.DoApplyUserBounds] M Old=',
      DbgS(FOldLeft,FOldTop,FOldWidth,FOldHeight),
      ' User=',DbgS(FLeft,FTop,FWidth,FHeight));
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
        DebugLn('  i=',Dbgs(i),' ',DbgSName(Items[i].Persistent),
        ' ',DbgS(Items[i].Left,Items[i].Top,Items[i].Width,Items[i].Height));
        {$ENDIF}
      end;
      InvalidateGuideLinesCache;
    end;
  end;
  UpdateRealBounds;
  EndUpdate;
end;

procedure TControlSelection.UpdateRealBounds;
var
  i: integer;
  NextRealLeft, NextRealTop, NextRealHeight, NextRealWidth: integer;
begin
  if FControls.Count>=1 then begin
    Items[0].GetFormRelativeBounds(FRealLeft,FRealTop,FRealWidth,FRealHeight,
                                   true);
    //DebugLn(['TControlSelection.UpdateRealBounds ',FRealLeft,',',FRealTop]);
    for i:=1 to FControls.Count-1 do begin
      Items[i].GetFormRelativeBounds(
                    NextRealLeft,NextRealTop,NextRealWidth,NextRealHeight,true);
      if FRealLeft>NextRealLeft then begin
        inc(FRealWidth,FRealLeft-NextRealLeft);
        FRealLeft:=NextRealLeft;
      end;
      if FRealTop>NextRealTop then begin
        inc(FRealHeight,FRealTop-NextRealTop);
        FRealTop:=NextRealTop;
      end;
      FRealWidth:=Max(FRealLeft+FRealWidth,NextRealLeft+NextRealWidth)-FRealLeft;
      FRealHeight:=Max(FRealTop+FRealHeight,NextRealTop+NextRealHeight)-FRealTop;
    end;
    AdjustGrabbers;
    InvalidateGuideLines;
    DoChangeProperties;
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
    Items[i].FFlags:=Items[i].FFlags-[scfParentInSelection,scfChildInSelection];
  end;
  for i:=0 to Cnt-1 do begin
    if not Items[i].IsTControl then continue;
    Control1:=TControl(Items[i].Persistent);
    for j:=0 to Cnt-1 do begin
      if not Items[j].IsTControl then continue;
      Control2:=TControl(Items[j].Persistent);
      if i=j then continue;
      if Control1.IsParentOf(Control2) then begin
        Include(Items[i].FFlags,scfChildInSelection);
        Include(Items[j].FFlags,scfParentInSelection);
      end;
    end;
  end;
  Exclude(FStates,cssParentChildFlagsNeedUpdate);
end;

procedure TControlSelection.DoDrawMarker(Index: integer;
  DC: TDesignerDeviceContext);
var
  CompLeft, CompTop, CompWidth, CompHeight: integer;
  DCOrigin: TPoint;
  CurItem: TSelectedControl;
begin
  CurItem:=Items[Index];
  if not CurItem.IsTComponent then exit;

  CurItem.GetFormRelativeBounds(CompLeft,CompTop,CompWidth,CompHeight);
  DCOrigin:=DC.FormOrigin;
  CompLeft:=CompLeft-DCOrigin.X;
  CompTop:=CompTop-DCOrigin.Y;

  {writeln('DoDrawMarker A ',FForm.Name
    ,' Component',AComponent.Name,',',CompLeft,',',CompLeft
    ,' DCOrigin=',DCOrigin.X,',',DCOrigin.Y
    );}

  DrawMarkerAt(DC,CompLeft,CompTop,CompWidth,CompHeight);
  CurItem.Flags:=CurItem.Flags+[scfMarkersPainted];
  CurItem.MarkerPaintedBounds:=Bounds(CompLeft,CompTop,CompWidth,CompHeight);
end;

procedure TControlSelection.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    Remove(AComponent);
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

function TControlSelection.PersistentAlignable(
  APersistent: TPersistent): boolean;
var
  CurParentLevel: integer;
  AComponent: TComponent;
begin
  Result:=false;
  if not (APersistent is TComponent) then exit;
  AComponent:=TComponent(APersistent);
  if AComponent=nil then exit;
  if AComponent is TControl then begin
    if not ControlIsInDesignerVisible(TControl(AComponent)) then begin
      //writeln('not alignable: A not ControlIsDesignerVisible ',AComponent.Name);
      exit;
    end;
    if Count>0 then begin
      if OnlyNonVisualPersistentsSelected then begin
        //writeln('not alignable: B OnlyNonVisualPersistentsSelected ',AComponent.Name);
        exit;
      end;
    end;
    if ParentLevel>0 then begin
      CurParentLevel:=GetParentLevel(TControl(AComponent));
      if CurParentLevel<>ParentLevel then begin
        //writeln('not alignable: C CurParentLevel<>ParentLevel ',AComponent.Name,' ',CurParentLevel,'<>',ParentLevel);
        exit;
      end;
    end;
  end else begin
    if ComponentIsInvisible(AComponent) then exit;
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
  if (FForm<>nil)
  and (Abs(NearestInt.Level-FForm.ClientWidth)<MaxDist) then
    ImproveNearestInt(NearestInt,FForm.ClientWidth);
end;

procedure TControlSelection.FindNearestClientTopBottom(
  var NearestInt: TNearestInt);
var MaxDist: integer;
begin
  MaxDist:=(CleanGridSizeY+1) div 2;
  if Abs(NearestInt.Level-0)<MaxDist then
    ImproveNearestInt(NearestInt,0);
  if (FForm<>nil)
  and (Abs(NearestInt.Level-FForm.ClientHeight)<MaxDist) then
    ImproveNearestInt(NearestInt,FForm.ClientHeight);
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
var
  i, CurLeft, MaxDist, CurDist: integer;
  AComponent: TComponent;
begin
  if (not EnvironmentOptions.SnapToGuideLines) or (FLookupRoot=nil) then exit;
  // search in all not selected components
  MaxDist:=(CleanGridSizeX+1) div 2;
  for i:=0 to FLookupRoot.ComponentCount-1 do begin
    AComponent:=FLookupRoot.Components[i];
    if not PersistentAlignable(AComponent) then continue;
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
  if (not EnvironmentOptions.SnapToGuideLines) or (FLookupRoot=nil) then exit;
  // search in all not selected components
  MaxDist:=(CleanGridSizeX+1) div 2;
  for i:=0 to FLookupRoot.ComponentCount-1 do begin
    AComponent:=FLookupRoot.Components[i];
    if not PersistentAlignable(AComponent) then continue;
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
  if (not EnvironmentOptions.SnapToGuideLines) or (FLookupRoot=nil) then exit;
  // search in all not selected components
  MaxDist:=(CleanGridSizeY+1) div 2;
  for i:=0 to FLookupRoot.ComponentCount-1 do begin
    AComponent:=FLookupRoot.Components[i];
    if not PersistentAlignable(AComponent) then continue;
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
  if (not EnvironmentOptions.SnapToGuideLines) or (FLookupRoot=nil) then exit;
  // search in all not selected components
  MaxDist:=(CleanGridSizeY+1) div 2;
  for i:=0 to FLookupRoot.ComponentCount-1 do begin
    AComponent:=FLookupRoot.Components[i];
    if not PersistentAlignable(AComponent) then continue;
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

function TControlSelection.FindNearestSnapLeft(ALeft: integer): integer;
var
  NearestLeft: TNearestInt;
begin
  // snap left
  NearestLeft.Level:=ALeft;
  NearestLeft.Valid:=false;
  FindNearestGridX(NearestLeft);
  FindNearestLeftGuideLine(NearestLeft);
  FindNearestClientLeftRight(NearestLeft);
  FindNearestOldLeft(NearestLeft);
  // return best snap
  if NearestLeft.Valid then
    Result:=NearestLeft.Nearest
  else
    Result:=ALeft;
end;

function TControlSelection.FindNearestSnapRight(ARight: integer): integer;
var
  NearestRight: TNearestInt;
begin
  // snap right
  NearestRight.Level:=ARight;
  NearestRight.Valid:=false;
  FindNearestGridX(NearestRight);
  FindNearestRightGuideLine(NearestRight);
  FindNearestClientLeftRight(NearestRight);
  FindNearestOldRight(NearestRight);
  // return best snap
  if NearestRight.Valid then
    Result:=NearestRight.Nearest
  else
    Result:=ARight;
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

function TControlSelection.FindNearestSnapTop(ATop: integer): integer;
var
  NearestTop: TNearestInt;
begin
  // snap top
  NearestTop.Level:=ATop;
  NearestTop.Valid:=false;
  FindNearestGridY(NearestTop);
  FindNearestTopGuideLine(NearestTop);
  FindNearestClientTopBottom(NearestTop);
  FindNearestOldTop(NearestTop);
  // return best snap
  if NearestTop.Valid then
    Result:=NearestTop.Nearest
  else
    Result:=ATop;
end;

function TControlSelection.FindNearestSnapBottom(ABottom: integer): integer;
var
  NearestBottom: TNearestInt;
begin
  // snap bottom
  NearestBottom.Level:=ABottom;
  NearestBottom.Valid:=false;
  FindNearestGridY(NearestBottom);
  FindNearestBottomGuideLine(NearestBottom);
  FindNearestClientTopBottom(NearestBottom);
  FindNearestOldBottom(NearestBottom);
  // return best snap
  if NearestBottom.Valid then
    Result:=NearestBottom.Nearest
  else
    Result:=ABottom;
end;

function TControlSelection.SnapGrabberMousePos(const CurMousePos: TPoint): TPoint;
begin
  Result := CurMousePos;
  if (not EnvironmentOptions.SnapToGrid) or (ActiveGrabber = nil) then exit;
  if gpLeft in ActiveGrabber.Positions then
    Result.X := FindNearestSnapLeft(Result.X)
  else
  if gpRight in ActiveGrabber.Positions then
    Result.X := FindNearestSnapRight(Result.X);
  if gpTop in ActiveGrabber.Positions then
    Result.Y := FindNearestSnapTop(Result.Y)
  else
  if gpBottom in ActiveGrabber.Positions then
    Result.Y := FindNearestSnapBottom(Result.Y);
end;

function TControlSelection.GetLeftGuideLine(var ALine: TRect): boolean;
var i, LineTop, LineBottom: integer;
  CRect: TRect;
  AComponent: TComponent;
begin
  if CacheGuideLines and FGuideLinesCache[glLeft].CacheValid then begin
    Result:=FGuideLinesCache[glLeft].LineValid;
    if Result then
      ALine:=FGuideLinesCache[glLeft].Line;
  end else begin
    Result:=false;
    if FForm=nil then exit;
    for i:=0 to FLookupRoot.ComponentCount-1 do begin
      AComponent:=FLookupRoot.Components[i];
      if not PersistentAlignable(AComponent) then continue;
      CRect:=GetParentFormRelativeBounds(AComponent);
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
  AComponent: TComponent;
begin
  if CacheGuideLines and FGuideLinesCache[glRight].CacheValid then begin
    Result:=FGuideLinesCache[glRight].LineValid;
    if Result then
      ALine:=FGuideLinesCache[glRight].Line;
  end else begin
    Result:=false;
    if FLookupRoot=nil then exit;
    for i:=0 to FLookupRoot.ComponentCount-1 do begin
      AComponent:=FLookupRoot.Components[i];
      if not PersistentAlignable(AComponent) then continue;
      CRect:=GetParentFormRelativeBounds(AComponent);
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
  AComponent: TComponent;
begin
  if CacheGuideLines and FGuideLinesCache[glTop].CacheValid then begin
    Result:=FGuideLinesCache[glTop].LineValid;
    if Result then
      ALine:=FGuideLinesCache[glTop].Line;
  end else begin
    Result:=false;
    if FLookupRoot=nil then exit;
    for i:=0 to FLookupRoot.ComponentCount-1 do begin
      AComponent:=FLookupRoot.Components[i];
      if not PersistentAlignable(AComponent) then continue;
      CRect:=GetParentFormRelativeBounds(AComponent);
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
  AComponent: TComponent;
begin
  if CacheGuideLines and FGuideLinesCache[glBottom].CacheValid then begin
    Result:=FGuideLinesCache[glBottom].LineValid;
    if Result then
      ALine:=FGuideLinesCache[glBottom].Line;
  end else begin
    Result:=false;
    if FLookupRoot=nil then exit;
    for i:=0 to FLookupRoot.ComponentCount-1 do begin
      AComponent:=FLookupRoot.Components[i];
      if not PersistentAlignable(AComponent) then continue;
      CRect:=GetParentFormRelativeBounds(AComponent);
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
    and (Items[0].IsTControl) then
      FParentLevel:=GetParentLevel(TControl(Items[0].Persistent))
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

procedure TControlSelection.DoChange(ForceUpdate: Boolean = False);
begin
  if (FUpdateLock > 0) then
    Include(FStates, cssChangedDuringLock)
  else
  begin
    Exclude(FStates, cssChangedDuringLock);
    if Assigned(FOnChange) then
      FOnChange(Self, ForceUpdate);
  end;
end;

procedure TControlSelection.DoChangeProperties;
begin
  if Assigned(OnPropertiesChanged) then OnPropertiesChanged(Self);
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
var
  i: integer;
  g: TGrabIndex;
begin
  if cssNotSavingBounds in FStates then exit;
  //debugln('TControlSelection.SaveBounds');
  if FUpdateLock > 0 then
  begin
    Include(FStates, cssBoundsNeedsSaving);
    Exit;
  end;

  for i := 0 to FControls.Count - 1 do Items[i].SaveBounds;
  for g := Low(TGrabIndex) to High(TGrabIndex) do FGrabbers[g].SaveBounds;
  FOldLeft := FRealLeft;
  FOldTop := FRealTop;
  FOldWidth := FRealWidth;
  FOldHeight := FRealHeight;
  Exclude(FStates, cssBoundsNeedsSaving);
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

function TControlSelection.IndexOf(APersistent: TPersistent): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result].Persistent<>APersistent) do dec(Result);
end;

function TControlSelection.Add(APersistent: TPersistent): integer;
var
  NewSelectedControl: TSelectedControl;
begin
  BeginUpdate;
  NewSelectedControl:=TSelectedControl.Create(Self,APersistent);
  if NewSelectedControl.DesignerForm<>FForm then Clear;
  Result:=FControls.Add(NewSelectedControl);
  FStates:=FStates+cssSelectionChangeFlags;
  if Count=1 then SetCustomForm;
  if APersistent=FLookupRoot then Include(FStates,cssLookupRootSelected);
  if APersistent is TComponent then
    TComponent(APersistent).FreeNotification(Self);
  DoChange;
  UpdateBounds;
  SaveBounds;
  EndUpdate;
end;

function TControlSelection.AssignPersistent(APersistent: TPersistent): boolean;
begin
  Result:=not IsOnlySelected(APersistent);
  if not Result then exit;
  {$IFDEF VerboseDesigner}
    DebugLn(['TControlSelection.AssignPersistent ',DbgSName(APersistent)]);
  {$ENDIF}
  BeginUpdate;
  Clear;
  Add(APersistent);
  EndUpdate;
end;

procedure TControlSelection.Remove(APersistent: TPersistent);
var i:integer;
begin
  i:=IndexOf(APersistent);
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
  if Items[Index].Persistent=FLookupRoot then
    Exclude(FStates,cssLookupRootSelected);
  Items[Index].Free;
  FControls.Delete(Index);
  FStates:=FStates+cssSelectionChangeFlags;

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
  InvalidateMarkers;
  for i:=0 to FControls.Count-1 do Items[i].Free;
  FControls.Clear;
  FStates:=FStates+cssSelectionChangeFlags-[cssLookupRootSelected];
  FForm:=nil;
  FMediator:=nil;
  UpdateBounds;
  SaveBounds;
  DoChange;
end;

function TControlSelection.Equals(const ASelection: TPersistentSelectionList
  ): boolean;
var
  i: Integer;
  Index: Integer;
  Instance: TPersistent;
begin
  if (ASelection=nil) then begin
    Result:=Count=0;
    exit;
  end;
  Result:=Count=ASelection.Count;
  if not Result then
    exit;
  Index:=0;
  for i:=0 to ASelection.Count-1 do
  begin
    Instance := ASelection[i];
    if Instance is TPersistent then begin
      if Items[Index].Persistent<>Instance then begin
        Result:=false;
        exit;
      end;
      inc(Index);
    end;
  end;
end;

procedure TControlSelection.Assign(AControlSelection: TControlSelection);
var i:integer;
begin
  if (AControlSelection=Self) or (cssNotSavingBounds in FStates) then exit;
  Include(FStates,cssNotSavingBounds);
  BeginUpdate;
  Clear;
  FControls.Capacity:=AControlSelection.Count;
  for i:=0 to AControlSelection.Count-1 do
    Add(AControlSelection[i].Persistent);
  SetCustomForm;
  UpdateBounds;
  Exclude(FStates,cssNotSavingBounds);
  SaveBounds;
  EndUpdate;
  DoChange;
end;

procedure TControlSelection.AssignSelection(
  const ASelection: TPersistentSelectionList);
var
  i:integer;
  instance: TPersistent;
begin
  if Equals(ASelection) then exit;
  if (cssNotSavingBounds in FStates) then exit;
  Include(FStates,cssNotSavingBounds);
  BeginUpdate;
  Clear;
  FControls.Capacity:=ASelection.Count;
  for i:=0 to ASelection.Count-1 do
  begin
    Instance := ASelection[i];
    if Instance is TPersistent then Add(Instance);
  end;
  SetCustomForm;
  UpdateBounds;
  Exclude(FStates,cssNotSavingBounds);
  SaveBounds;
  EndUpdate;
  DoChange;
end;

function TControlSelection.IsSelected(APersistent: TPersistent): Boolean;
begin
  Result:=(IndexOf(APersistent)>=0);
end;

function TControlSelection.IsOnlySelected(APersistent: TPersistent): Boolean;
begin
  Result:=(Count=1) and (Items[0].Persistent=APersistent);
end;

function TControlSelection.MoveSelection(dx, dy: integer; TotalDeltas: Boolean): Boolean;
var
  NewLeft, NewTop: integer;
begin
  Result := False;
  if (Count = 0) or IsResizing then Exit;
  //DebugLn('[TControlSelection.MoveSelection] A  %d,%d',[dx,dy]);
  //DebugLn('[TControlSelection.MoveSelection] B  %d',[FResizeLockCount]);
  if TotalDeltas then
  begin
    NewLeft := FOldLeft + dx;
    NewTop := FOldTop + dy;
  end
  else
  begin
    NewLeft := FLeft + dx;
    NewTop := FTop + dy
  end;
  if (NewLeft <> FLeft) or (NewTop <> FTop) then
  begin
    Result := True;
    BeginResizing;
    FLeft := NewLeft;
    FTop := NewTop;
    EndResizing(True);
  end;
end;

function TControlSelection.MoveSelectionWithSnapping(TotalDx, TotalDy: integer): boolean;
var
  NewLeft, NewTop: integer;
begin
  Result := False;
  if (Count = 0) or IsResizing then Exit;
  NewLeft := FindNearestSnapLeft(FOldLeft + TotalDx, FWidth);
  NewTop := FindNearestSnapTop(FOldTop + TotalDy, FHeight);
  {$IFDEF VerboseDesigner}
  DebugLn('[TControlSelection.MoveSelectionWithSnapping] A  ',
    'TotalD='+dbgs(TotalDx)+','+dbgs(TotalDy),
    ' CurBounds='+dbgs(FLeft)+','+dbgs(FTop)+','+dbgs(FWidth)+','+dbgs(FHeight),
    ' OldBounds='+dbgs(FOldLeft)+','+dbgs(FOldTop)+','+dbgs(FOldWidth)+','+dbgs(FOldHeight)
    +' NewPos='+dbgs(NewLeft)+','+dbgs(NewTop));
  {$ENDIF}
  if (NewLeft <> FLeft) or (NewTop <> FTop) then
  begin
    Result := True;
    BeginResizing;
    FLeft := NewLeft;
    FTop := NewTop;
    {$IFDEF VerboseDesigner}
    DebugLn('[TControlSelection.MoveSelectionWithSnapping] B  ',
      ' Bounds='+dbgs(FLeft)+','+dbgs(FTop)+','+dbgs(FWidth)+','+dbgs(FHeight));
    {$ENDIF}
    EndResizing(True);
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
  DebugLn('[TControlSelection.SizeSelection] A  ',DbgS(dx),',',DbgS(dy));
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
var
  g: TGrabIndex;
begin
  if FControls.Count > 0 then
  begin
    {$IFDEF VerboseDesigner}
    DebugLn('[TControlSelection.GrabberAtPos] ',Dbgs(x),',',Dbgs(y),'  '
    ,Dbgs(FGrabbers[4].Left),',',DbgS(FGrabbers[4].Top));
    {$ENDIF}
    for g := Low(TGrabIndex) to High(TGrabIndex) do
      if (FGrabbers[g].Left <= x) and
         (FGrabbers[g].Top <= y) and
         (FGrabbers[g].Left + FGrabbers[g].Width > x) and
         (FGrabbers[g].Top + FGrabbers[g].Height > y) then
      begin
        Result := FGrabbers[g];
        Exit;
      end;
  end;
  Result := nil;
end;

procedure TControlSelection.DrawGrabbers(DC: TDesignerDeviceContext);
var
  OldBrushColor: TColor;
  g: TGrabIndex;
  Diff: TPoint;
  RestoreBrush: boolean;

  procedure FillRect(RLeft, RTop, RRight, RBottom: integer);
  begin
    if not DC.RectVisible(RLeft, RTop, RRight, RBottom) then Exit;
    if not RestoreBrush then
    begin
      DC.BeginPainting;
      RestoreBrush := True;
      with DC.Canvas do
      begin
        OldBrushColor := Brush.Color;
        Brush.Color := GrabberColor;
      end;
    end;
    DC.Canvas.FillRect(Rect(RLeft, RTop, RRight, RBottom));
    //DC.Canvas.TextOut(RLeft,RTop,dbgs(ord(g)));
  end;

begin
  if (Count=0) or (FForm=nil) or LookupRootSelected or
     OnlyInvisiblePersistentsSelected then Exit;

  Diff := DC.FormOrigin;

  // debugln(['[DrawGrabbers] ',' DC=',Diff.X,',',Diff.Y,' Grabber1=',FGrabbers[0].Left,',',FGrabbers[0].Top]);

  RestoreBrush := False;
  for g := Low(TGrabIndex) to High(TGrabIndex) do
    FillRect(
       FGrabbers[g].Left-Diff.X
      ,FGrabbers[g].Top-Diff.Y
      ,FGrabbers[g].Left-Diff.X+FGrabbers[g].Width
      ,FGrabbers[g].Top-Diff.Y+FGrabbers[g].Height
    );
  Include(FStates, cssGrabbersPainted);

  if RestoreBrush then
  begin
    DC.Canvas.Brush.Color:=OldBrushColor;
    DC.EndPainting;
  end;
end;

procedure TControlSelection.DrawMarkerAt(DC: TDesignerDeviceContext;
  ALeft, ATop, AWidth, AHeight: integer);
var
  OldBrushColor: TColor;
  RestoreBrush: boolean;

  procedure FillRect(RLeft, RTop, RRight, RBottom: integer);
  begin
    if not DC.RectVisible(RLeft, RTop, RRight, RBottom) then exit;
    if not RestoreBrush then
    begin
      DC.BeginPainting;
      OldBrushColor:=DC.Canvas.Brush.Color;
      DC.Canvas.Brush.Color:=MarkerColor;
      RestoreBrush := True;
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
  begin
    DC.Canvas.Brush.Color:=OldBrushColor;
    DC.EndPainting;
  end;
end;

procedure TControlSelection.DrawMarkers(DC: TDesignerDeviceContext);
var
  i: Integer;
  AComponent: TComponent;
begin
  if (Count<2) or (FForm=nil) then exit;
  for i:=0 to Count-1 do begin
    if not Items[i].IsTComponent then continue;
    AComponent:=TComponent(Items[i].Persistent);
    if (AComponent=FLookupRoot)
    or (not Items[i].IsVisible) then continue;
    DoDrawMarker(i,DC);
  end;
end;

procedure TControlSelection.InvalidateMarkers;
var
  I: integer;
begin
  for I := 0 to Count - 1 do
    If Items[I].IsTComponent then
      InvalidateMarkersForComponent(TComponent(Items[I].Persistent));
end;

procedure TControlSelection.InvalidateMarkersForComponent(AComponent: TComponent
  );

  procedure InvalidateMarker(x,y: integer);
  var
    R: TRect;
  begin
    R:=Rect(x,y,x+MarkerSize,y+MarkerSize);
    InvalidateRect(FForm.Handle,@R,true);
  end;
  
var
  i: Integer;
  CurItem: TSelectedControl;
  ComponentBounds: TRect;
  LeftMarker: Integer;
  TopMarker: Integer;
  RightMarker: Integer;
  BottomMarker: Integer;
begin
  if (FForm=nil) or (not FForm.HandleAllocated) then exit;
  i:=IndexOf(AComponent);
  if (i>=0) then begin
    CurItem:=Items[i];
    if scfMarkersPainted in CurItem.Flags then begin
      ComponentBounds:=CurItem.MarkerPaintedBounds;
      LeftMarker:=ComponentBounds.Left;
      TopMarker:=ComponentBounds.Top;
      RightMarker:=ComponentBounds.Right-MarkerSize;
      BottomMarker:=ComponentBounds.Bottom-MarkerSize;
      InvalidateMarker(LeftMarker,TopMarker);
      InvalidateMarker(LeftMarker,BottomMarker);
      InvalidateMarker(RightMarker,TopMarker);
      InvalidateMarker(RightMarker,BottomMarker);
      CurItem.Flags:=CurItem.Flags-[scfMarkersPainted];
    end;
  end;
end;

procedure TControlSelection.DrawMarker(AComponent: TComponent;
  DC: TDesignerDeviceContext);
var
  i: Integer;
begin
  if (Count<2)
  or (FForm=nil)
  or (AComponent=FLookupRoot) then exit;
  i:=IndexOf(AComponent);
  if i<0 then exit;

  DoDrawMarker(i,DC);
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
        DC.BeginPainting;
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
    begin
      DC.Canvas.Pen.Color:=OldPenColor;
      DC.EndPainting;
      Include(FStates,cssRubberbandPainted);
    end;
  end;

// DrawRubberband
begin
  Diff:=DC.FormOrigin;
  with FRubberBandBounds do
    DrawInvertFrameRect(Left-Diff.X,Top-Diff.Y,Right-Diff.X,Bottom-Diff.Y);
end;

procedure TControlSelection.SelectAll(ALookupRoot: TComponent);
var
  i: integer;
  AComponent: TComponent;
begin
  for i := 0 to ALookupRoot.ComponentCount - 1 do
  begin
    AComponent := ALookupRoot.Components[i];
    if not IsSelected(AComponent) then
      Add(AComponent);
  end;
end;

procedure TControlSelection.SelectWithRubberBand(ALookupRoot: TComponent;
  AMediator: TDesignerMediator; ClearBefore, ExclusiveOr: boolean;
  var SelectionChanged: boolean; MaxParentComponent: TComponent);
var
  i: integer;
  AComponent: TComponent;

  function ComponentInRubberBand(AComponent: TComponent): boolean;
  var
    ALeft, ATop, ARight, ABottom: integer;
    Origin: TPoint;
    AControl: TControl;
    CurBounds: TRect;
    CurParent: TComponent;
  begin
    Result:=false;
    if AMediator<>nil then begin
      // check if component is visible on form
      if not AMediator.ComponentIsVisible(AComponent) then exit;
      if MaxParentComponent<>nil then begin
        // check if component is a grand child
        CurParent:=AComponent.GetParentComponent;
        if (not EnvironmentOptions.RubberbandSelectsGrandChilds)
        and (CurParent<>MaxParentComponent) then exit;
        // check if component is a child (direct or grand)
        while (CurParent<>nil) and (CurParent<>MaxParentComponent) do
          CurParent:=CurParent.GetParentComponent;
        if CurParent=nil then exit;
      end;
      AMediator.GetBounds(AComponent,CurBounds);
      Origin:=AMediator.GetComponentOriginOnForm(AComponent);
      ALeft:=Origin.X;
      ATop:=Origin.Y;
      ARight:=ALeft+CurBounds.Right-CurBounds.Left;
      ABottom:=ATop+CurBounds.Bottom-CurBounds.Top;
    end else begin
      if ComponentIsInvisible(AComponent) then exit;
      if (AComponent is TControl) then begin
        AControl:=TControl(AComponent);
        // check if control is visible on form
        if not ControlIsInDesignerVisible(AControl) then exit;
        // check if control
        if (MaxParentComponent is TWinControl) then begin
          // select only controls, that are children of MaxParentComponent
          if (not TWinControl(MaxParentComponent).IsParentOf(AControl)) then exit;
          // check if control is a grand child
          if (not EnvironmentOptions.RubberbandSelectsGrandChilds)
          and (AControl.Parent<>MaxParentComponent) then exit;
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
    if IsSelected(ALookupRoot) then begin
      Remove(ALookupRoot);
      SelectionChanged:=true;
    end;
    for i:=0 to ALookupRoot.ComponentCount-1 do begin
      AComponent:=ALookupRoot.Components[i];
      if not ComponentInRubberBand(AComponent) then begin
        if IsSelected(AComponent) then begin
          Remove(AComponent);
          SelectionChanged:=true;
        end;
      end;
    end;
  end;
  for i:=0 to ALookupRoot.ComponentCount-1 do begin
    AComponent:=ALookupRoot.Components[i];
    if ComponentInRubberBand(AComponent) then begin
      if IsSelected(AComponent) then begin
        if ExclusiveOr then begin
          Remove(AComponent);
          SelectionChanged:=true;
        end;
      end else begin
        Add(AComponent);
        SelectionChanged:=true;
      end;
    end;
  end;
end;

procedure TControlSelection.SetRubberBandBounds(ARect:TRect);
var
  i :integer;
  InvFrame: TRect;
begin
  if (FForm=nil) or (not FForm.HandleAllocated) then exit;
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
    if (FForm<>nil) and (cssRubberbandPainted in FStates) then begin
      InvFrame:=FRubberBandBounds;
      inc(InvFrame.Right);
      inc(InvFrame.Bottom);
      InvalidateFrame(FForm.Handle,@InvFrame,false,1);
      Exclude(FStates,cssRubberbandPainted);
    end;
    FRubberBandBounds:=ARect;
    if (FForm<>nil) and RubberbandActive then begin
      InvFrame:=FRubberBandBounds;
      inc(InvFrame.Right);
      inc(InvFrame.Bottom);
      InvalidateFrame(FForm.Handle,@InvFrame,false,1);
    end;
  end;
end;

function TControlSelection.OnlyNonVisualPersistentsSelected: boolean;
var i: integer;
begin
  if cssOnlyNonVisualNeedsUpdate in FStates then begin
    Result:=true;
    for i:=0 to FControls.Count-1 do
      if Items[i].IsTControl then begin
        Result:=false;
        break;
      end;
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
      if not Items[i].IsTControl then begin
        Result:=false;
        break;
      end;
    if Result then
      Include(FStates,cssOnlyVisualNeedsSelected)
    else
      Exclude(FStates,cssOnlyVisualNeedsSelected);
    Exclude(FStates,cssOnlyVisualNeedsUpdate);
  end else
    Result:=cssOnlyVisualNeedsSelected in FStates;
end;

function TControlSelection.OnlyInvisiblePersistentsSelected: boolean;
var i: integer;
begin
  if cssOnlyInvisibleNeedsUpdate in FStates then begin
    Result:=true;
    for i:=0 to FControls.Count-1 do begin
      if Items[i].IsVisible then begin
        Result:=false;
        break;
      end;
    end;
    if Result then
      Include(FStates,cssOnlyInvisibleSelected)
    else
      Exclude(FStates,cssOnlyInvisibleSelected);
    Exclude(FStates,cssOnlyInvisibleNeedsUpdate);
  end else
    Result:=cssOnlyInvisibleSelected in FStates;
end;

function TControlSelection.OnlyBoundLessComponentsSelected: boolean;
var
  i: Integer;
begin
  if cssOnlyBoundLessNeedsUpdate in FStates then begin
    Result:=true;
    for i:=0 to FControls.Count-1 do
      if Items[i].IsTComponent
      and ComponentBoundsDesignable(TComponent(Items[i].Persistent)) then begin
        Result:=false;
        break;
      end;
    if Result then
      Include(FStates,cssOnlyBoundLessSelected)
    else
      Exclude(FStates,cssOnlyBoundLessSelected);
    Exclude(FStates,cssOnlyBoundLessNeedsUpdate);
  end else
    Result:=cssOnlyBoundLessSelected in FStates;
end;

function TControlSelection.LookupRootSelected: boolean;
begin
  Result:=cssLookupRootSelected in FStates;
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
  // to space equally, you need at least two controls
  if (Count<2)
    and ((HorizAlignment=csaSpaceEqually) or (VertAlignment=csaSpaceEqually))
  then exit;
  
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
        HorDiff:=(FForm.Width div 2)-HorCenter;
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
        VertDiff:=(FForm.Height div 2)-VertCenter;
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
    if (Items[i].IsTControl) then begin
      if HorizSizing=cssNone then AWidth:=Items[i].Width;
      if VertSizing=cssNone then AHeight:=Items[i].Height;
      TControl(Items[i].Persistent).SetBounds(Items[i].Left,Items[i].Top,
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
    if Items[i].IsTControl then begin
      TControl(Items[i].Persistent).SetBounds(
          Items[i].Left,
          Items[i].Top,
          Max(1,(Items[i].Width*Percent) div 100),
          Max(1,(Items[i].Height*Percent) div 100)
        );
    end;
  end;

  EndResizing(false);
end;

function TControlSelection.CheckForLCLChanges(Update: boolean): boolean;

  function BoundsChanged(CurItem: TSelectedControl): boolean;
  var CurLeft, CurTop, CurWidth, CurHeight: integer;
  begin
    CurItem.GetFormRelativeBounds(CurLeft,CurTop,CurWidth,CurHeight);
    Result:=(CurLeft<>CurItem.UsedLeft)
          or (CurTop<>CurItem.UsedTop)
          or (CurWidth<>CurItem.UsedWidth)
          or (CurHeight<>CurItem.UsedHeight);
  end;

var
  i: Integer;
begin
  Result:=false;
  if FControls.Count>=1 then begin
    for i:=0 to FControls.Count-1 do begin
      if BoundsChanged(Items[i]) then begin
        Result:=true;
        break;
      end;
    end;
  end;
  if Update and Result then
  begin
    //debugln('TControlSelection.CheckForLCLChanges');
    for i:=0 to FControls.Count-1 do
      if Items[i].IsTComponent and BoundsChanged(Items[i]) then
        InvalidateMarkersForComponent(TComponent(Items[i].Persistent));
    InvalidateGuideLinesCache;
    if not IsResizing then begin
      UpdateBounds;
      DoChangeProperties;
    end;
  end;
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
      DC.BeginPainting;
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
  if (Count=0) or (FForm=nil) or LookupRootSelected then exit;
  LineExists[glLeft]:=GetLeftGuideLine(Line[glLeft]);
  LineExists[glRight]:=GetRightGuideLine(Line[glRight]);
  LineExists[glTop]:=GetTopGuideLine(Line[glTop]);
  LineExists[glBottom]:=GetBottomGuideLine(Line[glBottom]);
  if (not LineExists[glLeft]) and (not LineExists[glRight])
  and (not LineExists[glTop]) and (not LineExists[glBottom])
  then exit;

  RestorePen:=false;

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

  for g:=Low(TGuideLineType) to High(TGuideLineType) do begin
    FGuideLinesCache[g].PaintedLineValid:=LineExists[g];
    FGuideLinesCache[g].PaintedLine:=Line[g];
  end;

  if RestorePen then
  begin
    DC.Canvas.Pen.Color:=OldPenColor;
    DC.EndPainting;
  end;
  Include(FStates,cssGuideLinesPainted);
end;

procedure TControlSelection.Sort(SortProc: TSelectionSortCompare);
var a, b: integer;
  h: Pointer;
  Changed: boolean;
begin
  Changed:=false;
  // bubble sort: slow, but the selection is rarely bigger than few dozens
  // and does not change very often
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

function TControlSelection.GetSelectionOwner: TComponent;
var
  APersistent: TPersistent;
begin
  if FControls.Count > 0 then
  begin
    APersistent := GetLookupRootForComponent(Items[0].Persistent);
    if APersistent is TComponent then
      Result := TComponent(APersistent)
    else
      Result := nil;
  end
  else
    Result := nil;
end;

end.
