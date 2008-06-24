{%MainUnit ../controls.pp}
{  $Id$  }

{******************************************************************************
                                  TWinControl
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{$IFOPT C-}
// Uncomment for local trace
//  {$C+}
//  {$DEFINE ASSERT_IS_ON}
{$ENDIF}

{off $DEFINE VerboseAutoSizeCtrlData}
{off $DEFINE VerboseMouseBugfix}

{off $DEFINE VerboseSizeMsg}
{off $DEFINE CHECK_POSITION}
{$IFDEF CHECK_POSITION}
const CheckPostionClassName = 'xxTPage';
const CheckPostionName = 'ComponentNotebook';
const CheckPostionParentName = 'xxxEnvVarsPage';

function CheckPosition(AControl: TControl): boolean;
begin
  Result:=(CompareText(AControl.ClassName,CheckPostionClassName)=0)
           or (CompareText(AControl.Name,CheckPostionName)=0)
           or ((AControl.Parent<>nil)
               and (CompareText(AControl.Parent.Name,CheckPostionParentName)=0));
end;
{$ENDIF}


{------------------------------------------------------------------------------
  Autosizing Helper classes
-------------------------------------------------------------------------------}
type
  TAutoSizeBoxOrientation = (asboHorizontal, asboVertical);

  PAutoSizeBox = ^TAutoSizeBox;

  { TAutoSizeBox
    A TAutoSizeBox is a node in a tree.
    A TAutoSizeBox can be a cell. Then it is a leaf in the tree and can have a
    Control.
    A TAutoSizeBox can be a row or column. Then it has only one Childs array.
    A TAutoSizeBox can be a table. Then it has both Childs arrays.
  }

  TAutoSizeBox = class
  public
    Control: TControl; // the Control of a leaf node
    MinimumSize: array[TAutoSizeBoxOrientation] of integer;
    MaximumSize: array[TAutoSizeBoxOrientation] of integer; // 0 means inifinte
    PreferredSize: array[TAutoSizeBoxOrientation] of integer;// without theme space
    LeftTop: array[TAutoSizeBoxOrientation] of integer;
    BorderLeftTop: array[TAutoSizeBoxOrientation] of integer;
    BorderRightBottom: array[TAutoSizeBoxOrientation] of integer;
    Parent: array[TAutoSizeBoxOrientation] of TAutoSizeBox;
    Index: array[TAutoSizeBoxOrientation] of Integer; // index in parent or grandparent
    ChildCount: array[TAutoSizeBoxOrientation] of Integer;
    Childs: array[TAutoSizeBoxOrientation] of PAutoSizeBox;
    NewControlBounds: TRect;

    // for nodes
    destructor Destroy; override;
    procedure Clear;
    procedure SetControl(AControl: TControl);
    procedure ApplyChildsizingBorders(ChildSizing: TControlChildSizing);

    // for rows and columns
    procedure AllocateChildsArray(Orientation: TAutoSizeBoxOrientation;
                                  NewChildCount: Integer);
    procedure InitSums;
    procedure SumLine(Orientation: TAutoSizeBoxOrientation;
                      DoInit: boolean);
    procedure ComputeLeftTops(Orientation: TAutoSizeBoxOrientation);
    procedure ResizeChilds(ChildSizing: TControlChildSizing;
                           Orientation: TAutoSizeBoxOrientation;
                           TargetSize: integer);

    // for tables
    procedure AllocateTable(ColCount, RowCount: Integer);
    procedure SetTableControls(ListOfControls: TFPList;
                               ChildSizing: TControlChildSizing);
    procedure SumTable;
    procedure ResizeTable(ChildSizing: TControlChildSizing;
                          TargetWidth, TargetHeight: integer);
    procedure ComputeTableControlBounds(ChildSizing: TControlChildSizing);
    function SetTableControlBounds(ChildSizing: TControlChildSizing
                                   ): boolean;// true if changed
    function AlignControlsInTable(ListOfControls: TFPList;
                                  ChildSizing: TControlChildSizing;
                                  TargetWidth, TargetHeight: integer;
                                  Apply: boolean): boolean;// true if changed

    // debugging
    procedure WriteDebugReport(const Title: string);
  end;
  
  { TAutoSizeCtrlData
    This class is used by the auto size algorithm, to
      1. compute the preferred size of a control given the preferred sizes of
         its childs.
    Hints about the algorithm:
      First it builds a graph of dependencies. That means, for every side
      (Left,Top,Right,Bottom) of each child control the anchor control and
      space is calculated. Anchor means here direct and indirect anchors.
      Indirect anchors are defined by the Align property.
      For example a control with Align=alTop is anchored left to the parent,
      right to the parent and top to either the parent or another alTop control.
      Then it searches for circles and other invalid combinations and repairs
      them.
      
    }
    
  TAutoSizeCtrlData = class;
  
  TAutoSizeSideDataState = (
    assdfInvalid,
    assdfComputing,
    assdfUncomputable,// e.g. if [akLeft,akRight]*Anchors = []
    assdfValid
    );
  TAutoSizeSideDistDirection = (
    assddLeftTop,
    assddRightBottom
    );

  TAutoSizeSideData = record
    CtrlData: TAutoSizeCtrlData;
    Side: TAnchorSideReference;
    Space: integer;
    Distance: array[TAutoSizeSideDistDirection] of integer;
    DistanceState: array[TAutoSizeSideDistDirection] of TAutoSizeSideDataState;
  end;
    
  TAutoSizeCtrlData = class
  private
    FChilds: TAvgLvlTree;// tree of TAutoSizeCtrlData
    function GetChilds(AControl: TControl): TAutoSizeCtrlData;
    procedure DoMoveNonAlignedChilds(Side: TAnchorKind;
                                   var MoveDiff: integer; FindMinimum: boolean);
    procedure SetupNonAlignedChilds(MoveNonAlignedChilds: boolean);
    procedure AlignChilds;
    procedure SetupSpace;
    function ComputePositions: boolean;// false if recomputation is needed (a property changed)
  public
    Control: TControl; // the Control of a leaf node
    WinControl: TWinControl;// the Control as TWinControl (else nil)
    ChildCount: integer;
    Visible: boolean;//= Control.IsControlVisible
    PreferredSize: array[TAutoSizeBoxOrientation] of integer;// without theme space
    Borders: array[TAnchorKind] of integer;
    AdjustedClientBorders: array[TAnchorKind] of integer;// the borderspace created by WinControl.AdjustClientRect
    Sides: array[TAnchorKind] of TAutoSizeSideData;
    constructor Create(AControl: TControl);
    destructor Destroy; override;
    procedure Clear;
    procedure ComputePreferredClientArea(MoveNonAlignedChilds: boolean;
                      out PreferredClientWidth, PreferredClientHeight: integer);
    procedure FixControlProperties(Child: TControl);
    procedure ClearSides;
    procedure SetFixedLeftTop(ChildData: TAutoSizeCtrlData; Side: TAnchorKind;
                              NewLeftTop: integer);
    property Childs[AControl: TControl]: TAutoSizeCtrlData read GetChilds; default;
    procedure WriteDebugReport(const Title, Prefix: string);
  end;
  
const
  SizeBoxOrthogonal: array[TAutoSizeBoxOrientation] of TAutoSizeBoxOrientation
    = (asboVertical,asboHorizontal);
  {AutoSizeSideDataStateNames: array[TAutoSizeSideDataState] of shortstring = (
    'assdfInvalid',
    'assdfComputing',
    'assdfUncomputable',
    'assdfValid'
    );}
  AutoSizeSideDistDirectionNames: array[TAutoSizeSideDistDirection] of shortstring = (
    'assddLeftTop',
    'assddRightBottom'
    );

function CompareAutoSizeCtrlDatas(Data1, Data2: Pointer): integer;
var
  Control1: TControl;
  Control2: TControl;
begin
  Control1:=TAutoSizeCtrlData(Data1).Control;
  Control2:=TAutoSizeCtrlData(Data2).Control;
  if Pointer(Control1)>Pointer(Control2) then
    Result:=1
  else if Pointer(Control1)<Pointer(Control2) then
    Result:=-1
  else
    Result:=0;
end;

function CompareControlWithAutoSizeCtrlData(AControl, AData: Pointer): integer;
var
  Control1: TControl;
  Control2: TControl;
begin
  Control1:=TControl(AControl);
  Control2:=TAutoSizeCtrlData(AData).Control;
  if Pointer(Control1)>Pointer(Control2) then
    Result:=1
  else if Pointer(Control1)<Pointer(Control2) then
    Result:=-1
  else
    Result:=0;
end;


{ TAutoSizeCtrlData }

function TAutoSizeCtrlData.GetChilds(AControl: TControl): TAutoSizeCtrlData;
var
  AVLNode: TAvgLvlTreeNode;
begin
  if AControl=nil then exit(nil);
  if AControl=Control then RaiseGDBException('TAutoSizeCtrlData.GetChilds');
  if FChilds=nil then
    FChilds:=TAvgLvlTree.Create(@CompareAutoSizeCtrlDatas);
  AVLNode:=FChilds.FindKey(AControl,@CompareControlWithAutoSizeCtrlData);
  if AVLNode<>nil then
    Result:=TAutoSizeCtrlData(AVLNode.Data)
  else begin
    Result:=TAutoSizeCtrlData.Create(AControl);
    FChilds.Add(Result);
  end;
end;

procedure TAutoSizeCtrlData.AlignChilds;
var
  AlignList: TFPList;
  AlignBoundaryControls: array[TAnchorKind] of TAutoSizeCtrlData;

  procedure DoAlign(TheAlign: TAlign);
  var
    Child: TControl;
    i: Integer;
    ChildData: TAutoSizeCtrlData;
    a: TAnchorKind;
  begin
    WinControl.CreateControlAlignList(TheAlign,AlignList,nil);
    for i := 0 to AlignList.Count - 1 do begin
      Child:=TControl(AlignList[i]);
      ChildData:=Childs[Child];
      //DebugLn('DoAlign ',DbgSName(Child),' ',dbgs(Child.Align));

      for a:=Low(TAnchorKind) to High(TAnchorKind) do
        if a in AnchorAlign[TheAlign] then begin
          ChildData.Sides[a].CtrlData:=AlignBoundaryControls[a];
          if (a in [akLeft,akTop]) = (ChildData.Sides[a].CtrlData=Self) then
            ChildData.Sides[a].Side:=asrLeft
          else
            ChildData.Sides[a].Side:=asrRight;
          //DebugLn('DoAlign ',DbgSName(Child),' ',dbgs(a),' ',dbgs(a,ChildData.Sides[a].Side));
        end;

      case TheAlign of
      alTop: AlignBoundaryControls[akTop]:=ChildData;
      alBottom: AlignBoundaryControls[akBottom]:=ChildData;
      alLeft: AlignBoundaryControls[akLeft]:=ChildData;
      alRight: AlignBoundaryControls[akRight]:=ChildData;
      alClient: AlignBoundaryControls[akLeft]:=ChildData;
      end;
    end;
  end;
  
var
  a: TAnchorKind;
begin
  if ChildCount=0 then exit;
  AlignList := TFPList.Create;
  try
    // align and anchor child controls
    for a:=Low(TAnchorKind) to High(TAnchorKind) do
      AlignBoundaryControls[a]:=Self;
    DoAlign(alTop);
    DoAlign(alBottom);
    DoAlign(alLeft);
    DoAlign(alRight);
    DoAlign(alClient);
  finally
    AlignList.Free;
  end;
end;

procedure TAutoSizeCtrlData.SetupSpace;
var
  i: Integer;
  Child: TControl;
  ChildData: TAutoSizeCtrlData;
  a: TAnchorKind;
  SiblingData: TAutoSizeCtrlData;
  NewSpace: LongInt;
begin
  for i:=0 to ChildCount-1 do begin
    Child:=WinControl.Controls[i];
    ChildData:=Childs[Child];
    for a:=Low(TAnchorKind) to High(TAnchorKind) do begin
      if ChildData.Sides[a].CtrlData=Self then begin
        // aligned or anchored to parent
        if a in [akLeft,akRight] then begin
          ChildData.Sides[a].Space:=Max(WinControl.ChildSizing.LeftRightSpacing,
                                        ChildData.Borders[a]);
        end else begin
          ChildData.Sides[a].Space:=Max(WinControl.ChildSizing.TopBottomSpacing,
                                        ChildData.Borders[a]);
        end;
        if (Child.Align in [alLeft,alTop,alRight,alBottom,alClient])
        and (a in AnchorAlign[Child.Align]) then begin
          inc(ChildData.Sides[a].Space,AdjustedClientBorders[a]);
        end;
      end else if ChildData.Sides[a].CtrlData<>nil then begin
        SiblingData:=ChildData.Sides[a].CtrlData;
        // aligned or anchored to a sibling
        if a in [akLeft,akTop] then begin
          NewSpace:=ChildData.Borders[a];
          if ChildData.Sides[a].Side=asrRight then begin
            NewSpace:=Max(NewSpace,WinControl.ChildSizing.HorizontalSpacing);
            if a=akLeft then
              NewSpace:=Max(NewSpace,SiblingData.Borders[akRight])
            else
              NewSpace:=Max(NewSpace,SiblingData.Borders[akBottom]);
          end else if ChildData.Sides[a].Side=asrLeft then

          else if ChildData.Sides[a].Side=asrCenter then
            NewSpace:=0;
          ChildData.Sides[a].Space:=NewSpace;
        end else begin
          NewSpace:=ChildData.Borders[a];
          if ChildData.Sides[a].Side=asrTop then begin
            NewSpace:=Max(NewSpace,WinControl.ChildSizing.VerticalSpacing);
            if a=akRight then
              NewSpace:=Max(NewSpace,SiblingData.Borders[akLeft])
            else
              NewSpace:=Max(NewSpace,SiblingData.Borders[akTop]);
          end else if ChildData.Sides[a].Side=asrBottom then

          else if ChildData.Sides[a].Side=asrCenter then
            NewSpace:=0;
          ChildData.Sides[a].Space:=NewSpace;
        end;
      end else begin
        // dangling side
        if a in [akLeft,akTop] then begin
          ChildData.Sides[a].Side:=asrRight;
        end else begin
          ChildData.Sides[a].Side:=asrLeft;
        end;
        if a in [akLeft,akRight] then begin
          ChildData.Sides[a].Space:=
            Max(WinControl.ChildSizing.LeftRightSpacing,
                ChildData.Borders[a]);
        end else begin
          ChildData.Sides[a].Space:=
            Max(WinControl.ChildSizing.TopBottomSpacing,
                ChildData.Borders[a]);
        end;
      end;
    end;
  end;
end;

function TAutoSizeCtrlData.ComputePositions: boolean;
type
  TComputeResult = (
    crSuccess,
    crCircle,
    crFixedCircled
    );

  function ComputePosition(ChildData: TAutoSizeCtrlData; Side: TAnchorKind;
    Direction: TAutoSizeSideDistDirection): TComputeResult;
  var
    OppositeSide: TAnchorKind;
    NewDist: LongInt;
    SiblingData: TAutoSizeCtrlData;
    NeededSiblingSides: TAnchors;
    a: TAnchorKind;
    Child: TControl;
    IsSideLeftTop, IsOutwards, IsParentInwards: boolean;
    CurAnchors: TAnchors;
    CurSize: LongInt;
  begin
    if ChildData.Sides[Side].DistanceState[Direction]
      in [assdfValid,assdfUncomputable]
    then
      exit(crSuccess); // already computed
    if ChildData.Sides[Side].DistanceState[Direction]=assdfComputing then begin
      DebugLn(['TAutoSizeCtrlData.ComputePositions.ComputePosition CIRCLE detected ',DbgSName(ChildData.Control),' ',dbgs(Side),' ',AutoSizeSideDistDirectionNames[Direction]]);
      exit(crCircle); // there is a circle
    end;
    if ChildData.Sides[Side].DistanceState[Direction]<>assdfInvalid then
      raise Exception.Create('TAutoSizeCtrlData.ComputePositions.ComputePosition <>assdfInvalid');
      
    // mark as computing
    ChildData.Sides[Side].DistanceState[Direction]:=assdfComputing;
    OppositeSide:=OppositeAnchor[Side];
    
    // try to find good distances to the client area for this side
    Child:=ChildData.Control;
    CurAnchors:=Child.Anchors;
    if Child.Align in [alLeft,alTop,alRight,alBottom] then
      CurAnchors:=CurAnchors+AnchorAlign[Child.Align];
    if (Side in CurAnchors) then begin
      // this side is anchored
      SiblingData:=ChildData.Sides[Side].CtrlData;
      if (SiblingData=nil) or (SiblingData=Self) then begin
        // this side is anchored to parent
        // Note: SiblingData=nil can happen, if the reference control
        //       is not visible => use parent as default anchor
        case ChildData.Sides[Side].Side of
        asrLeft,asrRight: // asrTop=asrLeft,asrBottom=asrRight
          begin
            IsSideLeftTop:=(Side in [akLeft,akTop]);
            IsOutwards:=(Direction=assddLeftTop)=IsSideLeftTop;
            IsParentInwards:=(SiblingData=nil)
                          or ((ChildData.Sides[Side].Side=asrLeft)=IsSideLeftTop);
            if not IsParentInwards then begin
              // for example: left side is anchored to right side of parent
              //DebugLn(['ComputePosition ',DbgSName(Child),' Side=',dbgs(Side),' parent outside anchored, Direction=',AutoSizeSideDistDirectionNames[Direction],'  => assdfUncomputable']);
              ChildData.Sides[Side].DistanceState[Direction]:=assdfUncomputable;
            end else if IsOutwards then begin
              // for example: left side is anchored to left side of parent
              //              and left distance is needed
              ChildData.Sides[Side].Distance[Direction]:=ChildData.Sides[Side].Space;
              ChildData.Sides[Side].DistanceState[Direction]:=assdfValid;
            end else begin
              // for example: left side is anchored to left side of parent,
              //             right distance is needed
              if OppositeSide in CurAnchors then begin
                // compute opposite side first
                Result:=ComputePosition(ChildData,OppositeSide,Direction);
                if Result<>crSuccess then begin
                  DebugLn(['ComputePosition FAILED opposite side: ',DbgSName(Child),' ',dbgs(Side),' ',AutoSizeSideDistDirectionNames[Direction]]);
                  exit;
                end;
                if ChildData.Sides[OppositeSide].DistanceState[Direction]<>assdfValid
                then begin
                  ChildData.Sides[Side].DistanceState[Direction]:=assdfUncomputable;
                  exit;
                end;
                NewDist:=ChildData.Sides[OppositeSide].Distance[Direction];
              end else begin
                NewDist:=ChildData.Sides[OppositeSide].Space;
              end;
              if Side in [akLeft,akRight] then
                inc(NewDist,ChildData.PreferredSize[asboHorizontal])
              else
                inc(NewDist,ChildData.PreferredSize[asboVertical]);
              ChildData.Sides[Side].Distance[Direction]:=NewDist;
              ChildData.Sides[Side].DistanceState[Direction]:=assdfValid;
            end;
          end;
        asrCenter:
          begin
            //DebugLn(['ComputePosition ',DbgSName(Child),' Side=',dbgs(Side),' parent anchored, Direction=',AutoSizeSideDistDirectionNames[Direction],'  => assdfUncomputable']);
            ChildData.Sides[Side].DistanceState[Direction]:=assdfUncomputable;
          end;
        else
          RaiseGDBException('');
        end;
      end else begin
        // this side is anchored to a sibling
        // first compute needed sides of sibling
        NeededSiblingSides:=[];
        case ChildData.Sides[Side].Side of
        asrLeft: // asrLeft=asrTop
          if Side in [akLeft,akRight] then begin
            Include(NeededSiblingSides,akLeft);
          end else begin
            Include(NeededSiblingSides,akTop);
          end;
        asrRight: // asrRight=asrBottom
          if Side in [akLeft,akRight] then begin
            Include(NeededSiblingSides,akRight);
          end else begin
            Include(NeededSiblingSides,akBottom);
          end;
        asrCenter:
          if Side in [akLeft,akRight] then begin
            NeededSiblingSides:=NeededSiblingSides+[akLeft,akRight];
          end else begin
            NeededSiblingSides:=NeededSiblingSides+[akTop,akBottom];
          end;
        end;
        for a:=Low(TAnchorKind) to High(TAnchorKind) do begin
          if a in NeededSiblingSides then begin
            Result:=ComputePosition(SiblingData,a,Direction);
            if (Result=crCircle)
            and ((Child.Align in [alNone,alCustom])
              or (not (Side in AnchorAlign[Child.Align]))) then
            begin
              // there is a circle and it can be broken => break it
              DebugLn(['ComputePosition breaking CIRCLE ',DbgSName(Child),' - ',DbgSName(SiblingData.Control),' ',dbgs(a),' ',AutoSizeSideDistDirectionNames[Direction]]);
              Child.Anchors:=Child.Anchors-[Side];
              Result:=crFixedCircled;
            end;
            if Result<>crSuccess then begin
              DebugLn(['ComputePosition FAILED sibling dependency: ',DbgSName(Child),' - ',DbgSName(SiblingData.Control),' Side=',dbgs(Side),' a=',dbgs(a),' ',AutoSizeSideDistDirectionNames[Direction]]);
              exit;
            end;
            if SiblingData.Sides[a].DistanceState[Direction]<>assdfValid then
            begin
              ChildData.Sides[Side].DistanceState[Direction]:=assdfUncomputable;
              exit;
            end;
          end;
        end;
        
        // this side is anchored to a sibling and all needed sibling sides are valid
        case ChildData.Sides[Side].Side of
        asrLeft,asrRight: // asrLeft=asrTop,asrRight=asrBottom
          begin
            if ChildData.Sides[Side].Side=asrLeft then begin
              if Side in [akLeft,akRight] then
                NewDist:=SiblingData.Sides[akLeft].Distance[Direction]
              else
                NewDist:=SiblingData.Sides[akTop].Distance[Direction];
            end else begin
              if Side in [akLeft,akRight] then
                NewDist:=SiblingData.Sides[akRight].Distance[Direction]
              else
                NewDist:=SiblingData.Sides[akBottom].Distance[Direction];
            end;
            if (Direction=assddLeftTop)=(Side in [akLeft,akTop]) then
              inc(NewDist,ChildData.Sides[Side].Space)
            else
              dec(NewDist,ChildData.Sides[Side].Space);
            //DebugLn(['ComputePosition ',DbgSName(Child),' - ',DbgSName(SiblingData.Control),' Side=',dbgs(Side),' ',AutoSizeSideDistDirectionNames[Direction],' NewDist=',NewDist]);
          end;
        asrCenter:
          if Side in [akLeft,akRight] then begin
            NewDist:=((SiblingData.Sides[akLeft].Distance[Direction]
                      +SiblingData.Sides[akRight].Distance[Direction]) div 2)
                    -(ChildData.PreferredSize[asboHorizontal] div 2);
          end else begin
            NewDist:=((SiblingData.Sides[akTop].Distance[Direction]
                      +SiblingData.Sides[akBottom].Distance[Direction]) div 2)
                    -(ChildData.PreferredSize[asboVertical] div 2);
          end;
        end;
        ChildData.Sides[Side].Distance[Direction]:=NewDist;
        ChildData.Sides[Side].DistanceState[Direction]:=assdfValid;
      end;
    end else if (OppositeSide in CurAnchors)
    and ((Direction=assddLeftTop) <> (Side in [akLeft,akTop])) then begin
      // this side is not anchored, but the opposite is
      // e.g. control is anchored to the right
      // => compute the opposite side first
      Result:=ComputePosition(ChildData,OppositeSide,Direction);
      if Result<>crSuccess then begin
        //DebugLn(['ComputePosition FAILED computing opposite side: ',DbgSName(Child),' - ',DbgSName(SiblingData.Control),' Side=',dbgs(Side),' ',AutoSizeSideDistDirectionNames[Direction]]);
        exit;
      end;
      case ChildData.Sides[OppositeSide].DistanceState[Direction] of
      assdfValid:
        begin
          // opposite side +- preferred size
          NewDist:=ChildData.Sides[OppositeSide].Distance[Direction];
          if Side in [akLeft,akRight] then
            CurSize:=ChildData.PreferredSize[asboHorizontal]
          else
            CurSize:=ChildData.PreferredSize[asboVertical];
          inc(NewDist,CurSize);
          ChildData.Sides[Side].Distance[Direction]:=NewDist;
          ChildData.Sides[Side].DistanceState[Direction]:=assdfValid;
        end;
      assdfUncomputable:
        ChildData.Sides[Side].DistanceState[Direction]:=assdfUncomputable;
      else
        raise Exception.Create('TAutoSizeCtrlData.ComputePositions.ComputePosition assdfValid,assdfUncomputable');
      end;
    end else begin
      // not anchored
      if (Direction=assddLeftTop) = (Side in [akLeft,akTop]) then begin
        NewDist:=ChildData.Sides[Side].Space;
        ChildData.Sides[Side].Distance[Direction]:=NewDist;
        ChildData.Sides[Side].DistanceState[Direction]:=assdfValid;
      end else begin
        //DebugLn(['ComputePosition ',DbgSName(Child),' Side=',dbgs(Side),' not anchored, Direction=',AutoSizeSideDistDirectionNames[Direction],'  => assdfUncomputable']);
        ChildData.Sides[Side].DistanceState[Direction]:=assdfUncomputable;
      end;
    end;
    if not (ChildData.Sides[Side].DistanceState[Direction]
            in [assdfUncomputable,assdfValid])
    then begin
      DebugLn(['TAutoSizeCtrlData.ComputePositions.ComputePosition ',DbgSName(Child),' Side=',dbgs(Side),' Direction=',AutoSizeSideDistDirectionNames[Direction]]);
      raise Exception.Create('TAutoSizeCtrlData.ComputePositions.ComputePosition assdfValid,assdfUncomputable');
    end;
    Result:=crSuccess;
  end;

var
  i: Integer;
  Child: TControl;
  ChildData: TAutoSizeCtrlData;
  a: TAnchorKind;
begin
  Result:=false;
  // for every side try to find a good distance to the client area
  for i:=0 to ChildCount-1 do begin
    Child:=WinControl.Controls[i];
    ChildData:=Childs[Child];
    for a:=Low(TAnchorKind) to High(TAnchorKind) do begin
      if ComputePosition(ChildData,a,assddLeftTop)<>crSuccess then begin
        DebugLn(['TAutoSizeCtrlData.ComputePositions Failed to compute LeftTop ',DbgSName(Child),' ',dbgs(a)]);
        exit;
      end;
      if ComputePosition(ChildData,a,assddRightBottom)<>crSuccess then begin
        DebugLn(['TAutoSizeCtrlData.ComputePositions Failed to compute RightBottom ',DbgSName(Child),' ',dbgs(a)]);
        exit;
      end;
    end;
  end;
  Result:=true;
end;

constructor TAutoSizeCtrlData.Create(AControl: TControl);
const
  BigInteger = High(Integer) div 4;
var
  CurBorders: TRect;
  a: TAnchorKind;
  AdjustedClientRect: TRect;
begin
  //DebugLn(['TAutoSizeCtrlData.Create ',DbgSName(AControl)]);
  Control:=AControl;
  if Control is TWinControl then begin
    WinControl:=TWinControl(Control);
    ChildCount:=WinControl.ControlCount;
  end else
    ChildCount:=0;
  Visible:=Control.IsControlVisible;
  Control.BorderSpacing.GetSpaceAround(CurBorders);
  Borders[akLeft]:=CurBorders.Left;
  Borders[akTop]:=CurBorders.Top;
  Borders[akRight]:=CurBorders.Right;
  Borders[akBottom]:=CurBorders.Bottom;
  
  if WinControl<>nil then begin
    AdjustedClientRect:=Rect(0,0,BigInteger,BigInteger);
    WinControl.AdjustClientRect(AdjustedClientRect);
    AdjustedClientBorders[akLeft]:=AdjustedClientRect.Left;
    AdjustedClientBorders[akTop]:=AdjustedClientRect.Top;
    AdjustedClientBorders[akRight]:=BigInteger-AdjustedClientRect.Right;
    AdjustedClientBorders[akRight]:=BigInteger-AdjustedClientRect.Bottom;
  end else begin
    for a:=low(TAnchorKind) to high(TAnchorKind) do
      AdjustedClientBorders[a]:=0;
  end;
end;

destructor TAutoSizeCtrlData.Destroy;
begin
  Clear;
  FreeAndNil(FChilds);
  inherited Destroy;
end;

procedure TAutoSizeCtrlData.Clear;
begin
  ClearSides;
  if FChilds<>nil then
    FChilds.FreeAndClear;
end;

procedure TAutoSizeCtrlData.DoMoveNonAlignedChilds(Side: TAnchorKind;
  var MoveDiff: integer; FindMinimum: boolean);
var
  i: Integer;
  Child: TControl;
  MoveDiffValid: Boolean;
  ChildData: TAutoSizeCtrlData;
  AddSpace: LongInt;
  Position: Integer;
begin
  MoveDiffValid:=false;
  for i:=0 to ChildCount-1 do begin
    Child:=WinControl.Controls[i];
    ChildData:=Childs[Child];
    if (ChildData.Visible)
    and (Child.Align=alNone)
    and (Side in Child.Anchors) and (Child.AnchorSide[Side].Control=nil)
    then begin
      // this is a non aligned control
      //DebugLn(['TAutoSizeCtrlData.DoMoveNonAlignedChilds Child=',DbgSName(Child),' Side=',dbgs(Side)]);
      if FindMinimum then begin
        AddSpace:=Child.BorderSpacing.GetSpace(Side);
        if Side=akLeft then
          AddSpace:=Max(AddSpace,WinControl.ChildSizing.LeftRightSpacing)
        else
          AddSpace:=Max(AddSpace,WinControl.ChildSizing.TopBottomSpacing);
        Position:=Child.GetSidePosition(Side)
                  -AddSpace
                  -AdjustedClientBorders[Side];
        if (not MoveDiffValid) or (MoveDiff>Position) then
        begin
          MoveDiff:=Position;
          MoveDiffValid:=true;
        end;
      end else begin
        SetFixedLeftTop(ChildData,Side,Child.GetSidePosition(Side)-MoveDiff);
      end;
    end;
  end;
end;

procedure TAutoSizeCtrlData.SetupNonAlignedChilds(MoveNonAlignedChilds: boolean);
var
  ChildSizing: TControlChildSizing;
  Box: TAutoSizeBox;
  y: Integer;
  RowBox: TAutoSizeBox;
  x: Integer;
  ControlBox: TAutoSizeBox;
  Child: TControl;
  NewBounds: TRect;
  ChildData: TAutoSizeCtrlData;
  MoveDiff: Integer;
  AlignList: TFPList;
  r: TRect;
begin
  if ChildCount=0 then exit;
  if WinControl.ChildSizing.Layout=cclNone then begin
    // move the non-aligned controls (i.e. not aligned or fixed anchored)
    // Find the leftmost and topmost of those controls
    MoveDiff:=0;
    DoMoveNonAlignedChilds(akLeft,MoveDiff,true);
    //DebugLn(['TAutoSizeCtrlData.ComputePreferredClientArea akLeft MoveDiff=',MoveDiff]);
    if not MoveNonAlignedChilds then MoveDiff:=0;
    DoMoveNonAlignedChilds(akLeft,MoveDiff,false);
    MoveDiff:=0;
    DoMoveNonAlignedChilds(akTop,MoveDiff,true);
    //DebugLn(['TAutoSizeCtrlData.ComputePreferredClientArea akTop MoveDiff=',MoveDiff]);
    if not MoveNonAlignedChilds then MoveDiff:=0;
    DoMoveNonAlignedChilds(akTop,MoveDiff,false);
  end else begin
    // there is an automatic layout for non aligned childs
    // use the layout engine, but with static values
    ChildSizing:=nil;
    Box:=nil;
    AlignList:=TFPList.Create;
    try
      WinControl.CreateControlAlignList(alNone,AlignList,nil);
      if AlignList.Count=0 then exit;
      ChildSizing:=TControlChildSizing.Create(nil);
      Box:=TAutoSizeBox.Create;
      // copy current ChildSizing ...
      ChildSizing.Assign(WinControl.ChildSizing);
      // ... and change it to static layout (i.e. independent of parent size)
      ChildSizing.ShrinkHorizontal:=crsAnchorAligning;
      ChildSizing.EnlargeHorizontal:=crsAnchorAligning;
      ChildSizing.ShrinkVertical:=crsAnchorAligning;
      ChildSizing.EnlargeVertical:=crsAnchorAligning;
      // compute static layout
      r:=WinControl.GetLogicalClientRect;
      Box.AlignControlsInTable(AlignList,ChildSizing,r.Right,r.Bottom,false);
      //Box.WriteDebugReport('TAutoSizeCtrlData.SetupNonAlignedChilds');
      // transfer the coords of the layout
      for y:=0 to Box.ChildCount[asboVertical]-1 do begin
        RowBox:=Box.Childs[asboVertical][y];
        for x:=0 to RowBox.ChildCount[asboHorizontal]-1 do begin
          ControlBox:=RowBox.Childs[asboHorizontal][x];
          Child:=ControlBox.Control;
          if Child=nil then continue;
          NewBounds:=ControlBox.NewControlBounds;
          //DebugLn(['TAutoSizeCtrlData.SetupNonAlignedChilds ',DbgSName(Child),' ',dbgs(NewBounds)]);
          ChildData:=Childs[Child];
          // set left
          SetFixedLeftTop(ChildData,akLeft,NewBounds.Left);
          // set width
          ChildData.PreferredSize[asboHorizontal]:=NewBounds.Right-NewBounds.Left;
          // set top
          SetFixedLeftTop(ChildData,akTop,NewBounds.Top);
          // set height
          ChildData.PreferredSize[asboVertical]:=NewBounds.Bottom-NewBounds.Top;
        end;
      end;
    finally
      ChildSizing.Free;
      Box.Free;
      AlignList.Free;
    end;
  end;
end;

procedure TAutoSizeCtrlData.ComputePreferredClientArea(
  MoveNonAlignedChilds: boolean;
  out PreferredClientWidth, PreferredClientHeight: integer);
{ if MoveNonAlignedChilds=true then all non-aligned childs will be moved in
  parallel, so that at least one child is positioned left most and one child
  is positioned top most.

  Type of controls:
  1. layout: the left and top side of the control has only designed position
      and Parent.ChildSizing.Layout <> cclNone.
      That means: Align=alNone, Anchors=[akLeft,akTop],
      AnchorSide[akLeft/akTop].Control=nil, Parent.ChildSizing.Layout <> cclNone
  2. non-aligned: the left+top side of the control has only a designed position.
      That means: Align=alNone, akLeft is set, AnchorSide[akLeft].Control=nil
      and Parent.ChildSizing.Layout=cclNone
      Same for akTop.
  3. Aligned: Align<>alNone
      These are put consecutively into the remaining space.
      BorderSpacing and AdjustClientRect defines the space.
      The aligned sides automatically set the Anchors and the AnchorSide.Control
      to nil.
      alLeft,alRight,alTop,alBottom have one free side, which can be anchored.
  4. centered: akLeft and akRight are not set
  5. one side anchored: akLeft is set and akRight is not
                     OR akRight is set and akLeft is not
  5.1 anchored to a side (asrLeft,asrRight)
  5.2 anchored to a center (asrCenter)
  6. both sides anchored: akLeft and akRight not
     Note: asrCenter is not allowed here

  Circles and invalid combinations will be automatically fixed.
}

  procedure InitPreferredSizes;
  var
    i: Integer;
    Child: TControl;
    ChildData: TAutoSizeCtrlData;
    CurAnchors: TAnchors;
    CurPreferredWidth: integer;
    CurPreferredHeight: integer;
    UseCurrentWidth: Boolean;
    UseCurrentHeight: Boolean;
    NewWidth: LongInt;
    NewHeight: LongInt;
  begin
    for i:=0 to ChildCount-1 do begin
      Child:=WinControl.Controls[i];
      ChildData:=Childs[Child];
      if ChildData.Visible then begin
        CurAnchors:=Child.Anchors;
        if Child.Align in [alLeft,alRight,alTop,alBottom,alClient] then
          CurAnchors:=CurAnchors+AnchorAlign[Child.Align];
        // check if the current Width and/or Height of the Child control can be
        // used. For example: The current Width can be used, if it is independent
        // of the parent's width.
        UseCurrentWidth:=true;
        if Child.AutoSize
        or ([akLeft,akRight]*CurAnchors=[akLeft,akRight]) then
          UseCurrentWidth:=false;
        UseCurrentHeight:=true;
        if Child.AutoSize
        or ([akTop,akBottom]*CurAnchors=[akTop,akBottom]) then
          UseCurrentHeight:=false;

        if (not UseCurrentWidth) or (not UseCurrentHeight) then
          Child.GetPreferredSize(CurPreferredWidth,CurPreferredHeight,true,true);

        if UseCurrentWidth then
          NewWidth:=Child.Width
        else if CurPreferredWidth>0 then
          NewWidth:=CurPreferredWidth
        else
          NewWidth:=Child.GetDefaultWidth;
        NewWidth:=Child.Constraints.MinMaxWidth(NewWidth);

        if UseCurrentHeight then
          NewHeight:=Child.Height
        else if CurPreferredHeight>0 then
          NewHeight:=CurPreferredHeight
        else
          NewHeight:=Child.GetDefaultHeight;
        NewHeight:=Child.Constraints.MinMaxHeight(NewHeight);
      end else begin
        NewWidth:=0;
        NewHeight:=0;
      end;
        
      ChildData.PreferredSize[asboHorizontal]:=NewWidth;
      ChildData.PreferredSize[asboVertical]:=NewHeight;
      //DebugLn(['InitPreferredSizes Child=',DbgSName(Child),' PrefSize=',NewWidth,',',NewHeight]);
    end;
  end;
  
  procedure GetSideAnchor(ChildData: TAutoSizeCtrlData; a: TAnchorKind);
  var
    Child: TControl;
    ReferenceControl: TControl;
    ReferenceSide: TAnchorSideReference;
    Position: Integer;
  begin
    Child:=ChildData.Control;
    Child.AnchorSide[a].GetSidePosition(ReferenceControl,ReferenceSide,Position);
    //DebugLn(['GetSideAnchor Child=',DbgSName(Child),', a=',dbgs(a),' ReferenceControl=',DbgSName(ReferenceControl),' ReferenceSide=',dbgs(a,ReferenceSide)]);
    if ReferenceControl=Control then
      ChildData.Sides[a].CtrlData:=Self
    else if (ReferenceControl<>nil) and (ReferenceControl.Parent=Control) then
      ChildData.Sides[a].CtrlData:=Childs[ReferenceControl];
    ChildData.Sides[a].Side:=ReferenceSide;
    //if ChildData.Sides[a].CtrlData<>nil then DebugLn(['GetSideAnchor AAA1 Child=',DbgSName(Child),', a=',dbgs(a),' ReferenceControl=',DbgSName(ChildData.Sides[a].CtrlData.Control),' ReferenceSide=',dbgs(a,ChildData.Sides[a].Side)]);
  end;

var
  i: Integer;
  VisibleCount: Integer;
  Child: TControl;
  ChildData: TAutoSizeCtrlData;
  a: TAnchorKind;
  CurNeededClientWH: Integer;
begin
  PreferredClientWidth:=0;
  PreferredClientHeight:=0;

  if ChildCount=0 then exit;

  // fix control properties
  // check if there are visible childs
  VisibleCount:=0;
  for i:=0 to ChildCount-1 do begin
    Child:=WinControl.Controls[i];
    if Child.Align=alCustom then begin
      // this is not LCL business
      exit;
    end;
    FixControlProperties(Child);
    ChildData:=Childs[Child];
    if ChildData.Visible then
      inc(VisibleCount);
  end;
  //DebugLn(['TAutoSizeCtrlData.ComputePreferredClientArea ',DbgSName(Control),' VisibleCount=',VisibleCount]);
  if VisibleCount=0 then begin
    // nothing to do
    exit;
  end;
  
  InitPreferredSizes;

  repeat
    // init dependencies
    for i:=0 to ChildCount-1 do begin
      Child:=WinControl.Controls[i];
      ChildData:=Childs[Child];
      ChildData.ClearSides;
      for a:=Low(TAnchorKind) to High(TAnchorKind) do begin
        ChildData.Sides[a].Side:=asrLeft;
        if (Child.Align in [alLeft,alRight,alTop,alBottom,alClient])
        and (a in AnchorAlign[Child.Align]) then begin
          // this is an aligned side
          // => the dependencies will be setup later in AlignChilds
        end else if a in Child.Anchors then begin
          // this is an anchored side
          GetSideAnchor(ChildData,a);
        end else begin
          // this is a dangling side
        end;
      end;
    end;

    SetupNonAlignedChilds(MoveNonAlignedChilds);
    // setup the dependencies for Aligned controls
    AlignChilds;

    // setup space for dependencies
    SetupSpace;
    {$IFDEF VerboseAutoSizeCtrlData}
    WriteDebugReport('Space completed','');
    {$ENDIF}

    // calculate the needed positions for all childs
  until ComputePositions;
  
  {$IFDEF VerboseAutoSizeCtrlData}
  WriteDebugReport('Positions completed','');
  {$ENDIF}
  
  // compute needed clientwidth/clientheight
  for i:=0 to ChildCount-1 do begin
    Child:=WinControl.Controls[i];
    ChildData:=Childs[Child];
    for a:=Low(TAnchorKind) to High(TAnchorKind) do begin
      if (ChildData.Sides[a].DistanceState[assddLeftTop]=assdfValid)
      and (ChildData.Sides[a].DistanceState[assddRightBottom]=assdfValid)
      then begin
        CurNeededClientWH:=ChildData.Sides[a].Distance[assddLeftTop]
                          +ChildData.Sides[a].Distance[assddRightBottom];
        if a in [akLeft,akRight] then begin
          if PreferredClientWidth<CurNeededClientWH then
            PreferredClientWidth:=CurNeededClientWH;
        end else begin
          if PreferredClientHeight<CurNeededClientWH then
            PreferredClientHeight:=CurNeededClientWH;
        end;
      end;
    end;
  end;

  {$IFDEF VerboseAutoSizeCtrlData}
  DebugLn(['TAutoSizeCtrlData.ComputePreferredClientArea END ',DbgSName(Control),' PreferredClientWidth/height=',PreferredClientWidth,',',PreferredClientHeight]);
  {$ENDIF}
end;

procedure TAutoSizeCtrlData.FixControlProperties(Child: TControl);
var
  a: TAnchorKind;
begin
  // check that all anchor-controls are siblings or the parent
  for a:=Low(TAnchorKind) to High(TAnchorKind) do begin
    if Child.AnchorSide[a].Control=nil then continue;
    if Child.AnchorSide[a].Control=Control then continue;
    if (Child.AnchorSide[a].Control=Child)
    or (Child.AnchorSide[a].Control.Parent<>Control) then begin
      DebugLn(['TAutoSizeCtrlData.FixControlProperties ',DbgSName(Child),' a=',dbgs(a),' old=',DbgSName(Child.AnchorSide[a].Control),' new=nil']);
      Child.AnchorSide[a].Control:=nil;
    end;
  end;

  if Child.Align in [alLeft,alRight,alTop,alBottom,alClient] then begin
    // the aligned sides must be anchored
    Child.Anchors:=Child.Anchors+AnchorAlign[Child.Align];
    for a:=Low(TAnchorKind) to High(TAnchorKind) do begin
      if a in AnchorAlign[Child.Align] then begin
        // the aligned sides can not be anchored to a control
        if Child.AnchorSide[a].Control<>nil then
          DebugLn(['TAutoSizeCtrlData.FixControlProperties aligned sides can not be anchored ',DbgSName(Child),' a=',dbgs(a),' old=',DbgSName(Child.AnchorSide[a].Control),' new=nil']);
        Child.AnchorSide[a].Control:=nil;
      end;
      if Child.AnchorSide[a].Side=asrCenter then begin
        // an aligned control can not be centered
        DebugLn(['TAutoSizeCtrlData.FixControlProperties aligned control can not be centered ',DbgSName(Child),' a=',dbgs(a)]);
        Child.AnchorSide[a].Side:=asrLeft;
        if not (a in AnchorAlign[Child.Align]) then begin
          Child.Anchors:=Child.Anchors-[a];
          Child.AnchorSide[a].Control:=nil;
        end;
      end;
    end;
  end else begin
    for a:=Low(TAnchorKind) to High(TAnchorKind) do begin
      if (a in Child.Anchors)
      and (Child.AnchorSide[a].Side=asrCenter) then begin
        if Child.AnchorSide[a].Control<>nil then begin
          // the control should be centered relative to another control
          if a in [akLeft,akTop] then begin
            // un-anchor the other side
            if OppositeAnchor[a] in Child.Anchors then
              DebugLn(['TAutoSizeCtrlData.FixControlProperties control is center-anchored -> unanchor opposite side: ',DbgSName(Child),' a=',dbgs(a)]);
            Child.Anchors:=Child.Anchors-[OppositeAnchor[a]];
            Child.AnchorSide[OppositeAnchor[a]].Control:=nil;
          end else begin
            // the centering was setup via the right,bottom
            // => normalize it to center via the Left,Top
            DebugLn(['TAutoSizeCtrlData.FixControlProperties control is center-anchored -> normalize it to use Left,Top instead of Bottom,Right: ',DbgSName(Child),' a=',dbgs(a)]);
            Child.AnchorSide[OppositeAnchor[a]].Control:=Child.AnchorSide[a].Control;
            Child.AnchorSide[OppositeAnchor[a]].Side:=asrCenter;
            Child.AnchorSide[a].Control:=nil;
            Child.AnchorSide[a].Side:=asrLeft;
            Child.Anchors:=Child.Anchors+[OppositeAnchor[a]]-[a];
          end;
        end else begin
          // the asrCenter is not active => ok
        end;
      end;
    end;
  end;
end;

procedure TAutoSizeCtrlData.ClearSides;
var
  a: TAnchorKind;
  d: TAutoSizeSideDistDirection;
begin
  for a:=Low(TAnchorKind) to High(TAnchorKind) do begin
    FillChar(Sides[a],SizeOf(TAutoSizeSideData),0);
    for d:=Low(TAutoSizeSideDistDirection) to High(TAutoSizeSideDistDirection) do
      Sides[a].DistanceState[d]:=assdfInvalid;
  end;
end;

procedure TAutoSizeCtrlData.SetFixedLeftTop(ChildData: TAutoSizeCtrlData;
  Side: TAnchorKind; NewLeftTop: integer);
begin
  ChildData.Sides[Side].CtrlData:=Self;
  ChildData.Sides[Side].Side:=asrLeft;
  ChildData.Sides[Side].Space:=NewLeftTop;
  ChildData.Sides[Side].Distance[assddLeftTop]:=NewLeftTop;
  ChildData.Sides[Side].DistanceState[assddLeftTop]:=assdfValid;
end;

procedure TAutoSizeCtrlData.WriteDebugReport(const Title, Prefix: string);

  function GetDistance(a: TAnchorKind; d: TAutoSizeSideDistDirection): string;
  begin
    case Sides[a].DistanceState[d] of
    assdfInvalid: Result:='invalid';
    assdfComputing: Result:='computing';
    assdfUncomputable: Result:='uncomputable';
    assdfValid: Result:=dbgs(Sides[a].Distance[d]);
    else Result:='???';
    end;
  end;
  
  function GetSideControl(a: TAnchorKind): string;
  begin
    if Sides[a].CtrlData<>nil then
      Result:=DbgSName(Sides[a].CtrlData.Control)
    else
      Result:='nil';
  end;

var
  a: TAnchorKind;
  i: Integer;
begin
  if Title<>'' then
    DebugLn([Prefix,'TAutoSizeCtrlData.WriteDebugReport ',Title]);
  DebugLn([Prefix,'  Control=',DbgSName(Control),' ChildCount=',ChildCount,' Visible=',Visible,' Anchors=',dbgs(Control.Anchors),' Align=',dbgs(Control.Align)]);
  Debugln([Prefix,'  PreferredSize=',PreferredSize[asboHorizontal],',',PreferredSize[asboVertical]]);
  DebugLn([Prefix,'  Borders=l=',Borders[akLeft],',t=',Borders[akTop],',r=',Borders[akRight],',b=',Borders[akBottom]]);
  DebugLn([Prefix,'  AdjustedClientBorders=l=',AdjustedClientBorders[akLeft],',t=',AdjustedClientBorders[akTop],',r=',AdjustedClientBorders[akRight],',b=',AdjustedClientBorders[akBottom]]);
  for a:=Low(TAnchorKind) to High(TAnchorKind) do begin
    DebugLn([Prefix,'  Side ',dbgs(a),' Control=',GetSideControl(a),
      ' RefSide=',dbgs(a,Sides[a].Side),
      ' Space=',Sides[a].Space,
      ' DistLT=',GetDistance(a,assddLeftTop),
      ' DistBR=',GetDistance(a,assddRightBottom)]);
  end;
  for i:=0 to ChildCount-1 do
    Childs[WinControl.Controls[i]].WriteDebugReport('',Prefix+dbgs(i)+': ');
end;

{ TAutoSizeBox }

procedure TAutoSizeBox.SetControl(AControl: TControl);
var
  Border: TRect;
begin
  Control:=AControl;
  MinimumSize[asboHorizontal]:=Control.Constraints.EffectiveMinWidth;
  MinimumSize[asboVertical]:=Control.Constraints.EffectiveMinHeight;
  MaximumSize[asboHorizontal]:=Control.Constraints.EffectiveMaxWidth;
  MaximumSize[asboVertical]:=Control.Constraints.EffectiveMaxHeight;
  Control.GetPreferredSize(PreferredSize[asboHorizontal],
                           PreferredSize[asboVertical],
                           true, // without constraints
                           true  // with theme space
                           );
  // apply constraints
  if PreferredSize[asboHorizontal]>0 then
    PreferredSize[asboHorizontal]:=
      Control.Constraints.MinMaxWidth(PreferredSize[asboHorizontal]);
  if PreferredSize[asboVertical]>0 then
    PreferredSize[asboVertical]:=
      Control.Constraints.MinMaxHeight(PreferredSize[asboVertical]);

  if (Control.AutoSize or (Control.BorderSpacing.CellAlignHorizontal<>ccaFill))
  and (PreferredSize[asboHorizontal]>0)
  then begin
    // the control.width is fixed to its preferred width
    MaximumSize[asboHorizontal]:=PreferredSize[asboHorizontal];
  end;
  if (Control.AutoSize or (Control.BorderSpacing.CellAlignVertical<>ccaFill))
  and (PreferredSize[asboVertical]>0)
  then begin
    // the control.height is fixed to its preferred height
    MaximumSize[asboVertical]:=PreferredSize[asboVertical];
  end;

  // if no preferred size is valid use the class defaults
  if PreferredSize[asboHorizontal]<=0 then
    PreferredSize[asboHorizontal]:=
      Control.Constraints.MinMaxWidth(Control.GetControlClassDefaultSize.X);
  if PreferredSize[asboVertical]<=0 then
    PreferredSize[asboVertical]:=
      Control.Constraints.MinMaxHeight(Control.GetControlClassDefaultSize.X);

  //DebugLn(['TAutoSizeBox.SetControl ',DbgSName(Control),' ',PreferredSize[asboHorizontal]]);
  Control.BorderSpacing.GetSpaceAround(Border);
  BorderLeftTop[asboHorizontal]:=Border.Left;
  BorderLeftTop[asboVertical]:=Border.Top;
  BorderRightBottom[asboHorizontal]:=Border.Right;
  BorderRightBottom[asboVertical]:=Border.Bottom;
end;

procedure TAutoSizeBox.AllocateChildsArray(Orientation: TAutoSizeBoxOrientation;
  NewChildCount: Integer);
var
  Size: Integer;
begin
  Size:=NewChildCount*SizeOf(Pointer);
  ReallocMem(Childs[Orientation],Size);
  if Size>0 then
    FillChar(Childs[Orientation][0],Size,0);
  ChildCount[Orientation]:=NewChildCount;
end;

procedure TAutoSizeBox.AllocateTable(ColCount, RowCount: Integer);
{ This creates a ColCount x RowCount number of cells,
  and a Row of Columns and a Column of Rows.
  
  +-++-++-++-+  +----------+
  | || || || |  |          |
  | || || || |  +----------+
  | || || || |  +----------+
  | || || || |  |          |
  | || || || |  +----------+
  | || || || |  +----------+
  | || || || |  |          |
  +-++-++-++-+  +----------+

}
var
  x, y: Integer;
  RowBox: TAutoSizeBox;
  ColBox: TAutoSizeBox;
  CellBox: TAutoSizeBox;
begin
  AllocateChildsArray(asboHorizontal,ColCount);
  AllocateChildsArray(asboVertical,RowCount);
  // create columns
  for x:=0 to ColCount-1 do begin
    ColBox:=TAutoSizeBox.Create;
    Childs[asboHorizontal][x]:=ColBox;
    ColBox.AllocateChildsArray(asboVertical,RowCount);
    ColBox.Parent[asboHorizontal]:=Self;
    ColBox.Index[asboHorizontal]:=x;
    ColBox.Index[asboVertical]:=-1;
  end;
  // create rows
  for y:=0 to RowCount-1 do begin
    RowBox:=TAutoSizeBox.Create;
    Childs[asboVertical][y]:=RowBox;
    RowBox.AllocateChildsArray(asboHorizontal,ColCount);
    RowBox.Parent[asboVertical]:=Self;
    RowBox.Index[asboHorizontal]:=-1;
    RowBox.Index[asboVertical]:=y;
  end;
  // create cells
  for y:=0 to RowCount-1 do begin
    RowBox:=Childs[asboVertical][y];
    for x:=0 to ColCount-1 do begin
      ColBox:=Childs[asboHorizontal][x];
      CellBox:=TAutoSizeBox.Create;
      RowBox.Childs[asboHorizontal][x]:=CellBox;
      ColBox.Childs[asboVertical][y]:=CellBox;
      CellBox.Parent[asboHorizontal]:=RowBox;
      CellBox.Parent[asboVertical]:=ColBox;
      CellBox.Index[asboHorizontal]:=x;
      CellBox.Index[asboVertical]:=y;
    end;
  end;
end;

procedure TAutoSizeBox.SetTableControls(ListOfControls: TFPList;
  ChildSizing: TControlChildSizing);
var
  i: Integer;
  Row: LongInt;
  Col: LongInt;
  ChildControl: TControl;
  ChildBox: TAutoSizeBox;
  RowCount: LongInt;
  ColCount: Integer;
begin
  // allocate table
  case ChildSizing.Layout of
  cclLeftToRightThenTopToBottom:
    begin
      ColCount:=Max(1,ChildSizing.ControlsPerLine);
      RowCount:=((ListOfControls.Count-1) div ColCount)+1;
    end;
  cclTopToBottomThenLeftToRight:
    begin
      RowCount:=Max(1,ChildSizing.ControlsPerLine);
      ColCount:=((ListOfControls.Count-1) div RowCount)+1;
    end;
  else
    raise Exception.Create('TAutoSizeBox.SetTableControls TODO');
  end;
  AllocateTable(ColCount,RowCount);

  // set controls
  for i:=0 to ListOfControls.Count-1 do begin
    ChildControl:=TControl(ListOfControls[i]);
    case ChildSizing.Layout of
    cclLeftToRightThenTopToBottom:
      begin
        Row:=i div ChildCount[asboHorizontal];
        Col:=i mod ChildCount[asboHorizontal];
        ChildBox:=Childs[asboHorizontal][Col].Childs[asboVertical][Row];
        ChildBox.SetControl(ChildControl);
        ChildBox.ApplyChildsizingBorders(ChildSizing);
      end;
    cclTopToBottomThenLeftToRight:
      begin
        Col:=i div ChildCount[asboVertical];
        Row:=i mod ChildCount[asboVertical];
        ChildBox:=Childs[asboVertical][Row].Childs[asboHorizontal][Col];
        ChildBox.SetControl(ChildControl);
        ChildBox.ApplyChildsizingBorders(ChildSizing);
      end;
    end;
  end;
end;

procedure TAutoSizeBox.ApplyChildsizingBorders(ChildSizing: TControlChildSizing
  );
var
  MinBorder: LongInt;
begin
  // left border
  if (Parent[asboHorizontal]=nil) or (Index[asboHorizontal]=0) then
    MinBorder:=ChildSizing.LeftRightSpacing
  else
    MinBorder:=ChildSizing.HorizontalSpacing;
  BorderLeftTop[asboHorizontal]:=Max(BorderLeftTop[asboHorizontal],MinBorder);
  
  // right border
  if (Parent[asboHorizontal]=nil)
  or (Index[asboHorizontal]=Parent[asboHorizontal].ChildCount[asboHorizontal]-1)
  then
    MinBorder:=ChildSizing.LeftRightSpacing
  else
    MinBorder:=ChildSizing.HorizontalSpacing;
  BorderRightBottom[asboHorizontal]:=Max(BorderRightBottom[asboHorizontal],
                                         MinBorder);

  // top border
  if (Parent[asboVertical]=nil) or (Index[asboVertical]=0) then
    MinBorder:=ChildSizing.TopBottomSpacing
  else
    MinBorder:=ChildSizing.VerticalSpacing;
  BorderLeftTop[asboVertical]:=Max(BorderLeftTop[asboVertical],MinBorder);

  // bottom border
  if (Parent[asboVertical]=nil)
  or (Index[asboVertical]=Parent[asboVertical].ChildCount[asboVertical]-1)
  then
    MinBorder:=ChildSizing.TopBottomSpacing
  else
    MinBorder:=ChildSizing.VerticalSpacing;
  BorderRightBottom[asboVertical]:=Max(BorderRightBottom[asboVertical],
                                       MinBorder);
end;

procedure TAutoSizeBox.InitSums;

  procedure Init(o: TAutoSizeBoxOrientation);
  var
    FirstChild: TAutoSizeBox;
  begin
    if ChildCount[o]>0 then begin
      FirstChild:=Childs[o][0];
      MaximumSize[o]:=FirstChild.MaximumSize[o];
      MinimumSize[o]:=FirstChild.MinimumSize[o];
      PreferredSize[o]:=FirstChild.PreferredSize[o];
      BorderLeftTop[o]:=FirstChild.BorderLeftTop[o];
      BorderRightBottom[o]:=FirstChild.BorderRightBottom[o];
    end else begin
      MaximumSize[o]:=0;
      MinimumSize[o]:=0;
      PreferredSize[o]:=0;
      BorderLeftTop[o]:=0;
      BorderRightBottom[o]:=0;
    end;
  end;

begin
  Init(asboHorizontal);
  Init(asboVertical);
end;

procedure TAutoSizeBox.SumLine(Orientation: TAutoSizeBoxOrientation;
  DoInit: boolean);
// total orientated minimum is the sum of all minimums plus borders
// total orientated maximum is the sum of all maximums plus borders
// total orientated preferred is the sum of all preferred plus borders
// total othogonal minimum is the maximum of all minimums
// total othogonal maximum is the minimum of all maximums
// total othogonal preferred is the maximum of all preferred
var
  i: Integer;
  Orthogonal: TAutoSizeBoxOrientation;
  CurChild: TAutoSizeBox;
  CurBorder: integer;
  LastChild: TAutoSizeBox;
begin
  if DoInit then InitSums;
  Orthogonal:=SizeBoxOrthogonal[Orientation];
  if ChildCount[Orientation]>0 then begin
    for i:=0 to ChildCount[Orientation]-1 do begin
      CurChild:=Childs[Orientation][i];
      
      // add border in Orientation
      CurBorder:=CurChild.BorderLeftTop[Orientation];
      if i>0 then
        CurBorder:=Max(Childs[Orientation][i-1].BorderRightBottom[Orientation],
                       CurBorder);
      if MaximumSize[Orientation]>0 then begin
        inc(MaximumSize[Orientation],CurBorder);
      end;
      inc(MinimumSize[Orientation],CurBorder);
      inc(PreferredSize[Orientation],CurBorder);
      // add item size in Orientation
      if MaximumSize[Orientation]>0 then begin
        if CurChild.MaximumSize[Orientation]>0 then
          inc(MaximumSize[Orientation],CurChild.MaximumSize[Orientation])
        else
          MaximumSize[Orientation]:=0;
      end;
      inc(MinimumSize[Orientation],CurChild.MinimumSize[Orientation]);
      inc(PreferredSize[Orientation],CurChild.PreferredSize[Orientation]);
      
      // maximize in Orthogonal
      if MaximumSize[Orthogonal]>0 then begin
        if CurChild.MaximumSize[Orthogonal]>0 then
          MaximumSize[Orthogonal]:=Max(MaximumSize[Orthogonal],
                                       CurChild.MaximumSize[Orthogonal])
        else
          MaximumSize[Orthogonal]:=0;
      end;
      MinimumSize[Orthogonal]:=Max(MinimumSize[Orthogonal],
                                   CurChild.MinimumSize[Orthogonal]);
      PreferredSize[Orthogonal]:=Max(PreferredSize[Orthogonal],
                                     CurChild.PreferredSize[Orthogonal]);
      BorderLeftTop[Orthogonal]:=Max(BorderLeftTop[Orthogonal],
                                     CurChild.BorderLeftTop[Orthogonal]);
      BorderRightBottom[Orthogonal]:=Max(BorderRightBottom[Orthogonal],
                                        CurChild.BorderRightBottom[Orthogonal]);
    end;

    // last border
    LastChild:=Childs[Orientation][ChildCount[Orientation]-1];
    BorderRightBottom[Orientation]:=LastChild.BorderRightBottom[Orientation];
  end;
end;

procedure TAutoSizeBox.SumTable;
var
  x: Integer;
  ColBox: TAutoSizeBox;
  y: Integer;
  RowBox: TAutoSizeBox;
begin
  // sum items in rows
  for y:=0 to ChildCount[asboVertical]-1 do begin
    RowBox:=Childs[asboVertical][y];
    RowBox.SumLine(asboHorizontal,true);
  end;
  // sum items in columns
  for x:=0 to ChildCount[asboHorizontal]-1 do begin
    ColBox:=Childs[asboHorizontal][x];
    ColBox.SumLine(asboVertical,true);
  end;
  // sum rows
  SumLine(asboVertical,true);
  // sum columns
  SumLine(asboHorizontal,false);
end;

procedure TAutoSizeBox.ComputeLeftTops(Orientation: TAutoSizeBoxOrientation);
var
  i: Integer;
  Child: TAutoSizeBox;
  CurLeftTop: Integer;
begin
  CurLeftTop:=0;
  for i:=0 to ChildCount[Orientation]-1 do begin
    Child:=Childs[Orientation][i];
    if i=0 then
      inc(CurLeftTop,Child.BorderLeftTop[Orientation]);
    Child.LeftTop[Orientation]:=CurLeftTop;
    inc(CurLeftTop,Child.PreferredSize[Orientation]);
    inc(CurLeftTop,Child.BorderRightBottom[Orientation]);
  end;
end;

procedure TAutoSizeBox.ResizeChilds(ChildSizing: TControlChildSizing;
  Orientation: TAutoSizeBoxOrientation; TargetSize: integer);
type
  TResizeFactor = record
    Scale: double;
    Offset: integer;
  end;
var
  EnlargeStyle: TChildControlResizeStyle;
  ShrinkStyle: TChildControlResizeStyle;
  CurSize: LongInt;

  function GetChildTotalSize: integer;
  // computes the total preferred size of all childs of this Orientation
  var
    i: Integer;
    Child: TAutoSizeBox;
  begin
    Result:=0;
    for i:=0 to ChildCount[Orientation]-1 do begin
      Child:=Childs[Orientation][i];
      if i=0 then
        inc(Result,Child.BorderLeftTop[Orientation]);
      if Child.PreferredSize[Orientation]<1 then
        Child.PreferredSize[Orientation]:=1;
      inc(Result,Child.PreferredSize[Orientation]);
      inc(Result,Child.BorderRightBottom[Orientation]);
    end;
  end;
  
  procedure GetChildMaxResize(out Factor: TResizeFactor;
    out ResizeableCount: integer);
  // returns the number of childs/gaps, that can grow (ResizeableCount)
  // and the maximum factor, by which the childs/gaps can grow (TResizeFactor)
  var
    i: Integer;
    CurScale: Double;
    CurOffset: LongInt;
    Child: TAutoSizeBox;
  begin
    Factor.Scale:=0;
    Factor.Offset:=0;
    ResizeableCount:=0;
    case EnlargeStyle of
    
    crsAnchorAligning:
      exit; // no resizing

    crsScaleChilds,crsHomogenousChildResize:
    
      for i:=0 to ChildCount[Orientation]-1 do begin
        Child:=Childs[Orientation][i];
        if (Child.MaximumSize[Orientation]>0)
        and (Child.PreferredSize[Orientation]>=Child.MaximumSize[Orientation])
        then begin
          // this child can not be further enlarged
          continue;
        end;
        inc(ResizeableCount);

        case EnlargeStyle of

        crsScaleChilds, crsHomogenousChildResize:
          begin
            if Child.MaximumSize[Orientation]=0 then begin
              CurScale:=double(TargetSize);
              CurOffset:=TargetSize;
            end else begin
              CurScale:=double(Child.MaximumSize[Orientation])
                          /Child.PreferredSize[Orientation];
              CurOffset:=Child.MaximumSize[Orientation]
                         -Child.PreferredSize[Orientation];
            end;
            if (Factor.Offset=0) or (Factor.Offset>CurOffset) then begin
              Factor.Scale:=CurScale;
              Factor.Offset:=CurOffset;
            end;
          end;

        end;
      end;
      
    crsHomogenousSpaceResize:
      if ChildCount[Orientation]>0 then begin
        Factor.Scale:=double(TargetSize);
        Factor.Offset:=TargetSize;
        ResizeableCount:=ChildCount[Orientation]+1;
      end;

    else
      raise Exception.Create('TAutoSizeBox.ResizeChilds');
      
    end;
  end;
  
  procedure EnlargeChilds(const Factor: TResizeFactor);
  var
    i: Integer;
    Child: TAutoSizeBox;
    DiffSize: Integer;
    NewSize: LongInt;
    OldSize: LongInt;
  begin
    for i:=0 to ChildCount[Orientation]-1 do begin
      if TargetSize=CurSize then break;
    
      Child:=Childs[Orientation][i];
      if (Child.MaximumSize[Orientation]<0)
      and (Child.PreferredSize[Orientation]>=Child.MaximumSize[Orientation])
      then begin
        // this child can not be further enlarged
        continue;
      end;

      case EnlargeStyle of
      
      crsScaleChilds:
        begin
          // scale PreferredSize
          DiffSize:=TargetSize-CurSize;
          OldSize:=Child.PreferredSize[Orientation];
          NewSize:=round(double(OldSize)*Factor.Scale);
          NewSize:=Min(OldSize+DiffSize,Max(OldSize+1,NewSize));
          inc(CurSize,NewSize-OldSize);
          Child.PreferredSize[Orientation]:=NewSize;
        end;
        
      crsHomogenousChildResize:
        begin
          // add to PreferredSize
          DiffSize:=TargetSize-CurSize;
          OldSize:=Child.PreferredSize[Orientation];
          NewSize:=Min(OldSize+Factor.Offset,OldSize+DiffSize);
          inc(CurSize,NewSize-OldSize);
          Child.PreferredSize[Orientation]:=NewSize;
        end;
        
      crsHomogenousSpaceResize:
        begin
          if i=0 then begin
            // add to left/top border
            DiffSize:=TargetSize-CurSize;
            OldSize:=Child.BorderLeftTop[Orientation];
            NewSize:=Min(OldSize+Factor.Offset,OldSize+DiffSize);
            inc(CurSize,NewSize-OldSize);
            Child.BorderLeftTop[Orientation]:=NewSize;
          end;
          // add to right/bottom border
          DiffSize:=TargetSize-CurSize;
          OldSize:=Child.BorderRightBottom[Orientation];
          NewSize:=Min(OldSize+Factor.Offset,OldSize+DiffSize);
          inc(CurSize,NewSize-OldSize);
          Child.BorderRightBottom[Orientation]:=NewSize;
          if i<ChildCount[Orientation]-1 then
            Child.BorderLeftTop[Orientation]:=NewSize;
        end;
      
      end;
    end;
  end;

  procedure GetChildMinResize(out Factor: TResizeFactor;
    out ResizeableCount: integer);
  // returns the number of childs/gaps, that can shrink (ResizeableCount)
  // and the maximum factor, by which the childs/gaps can shrink (TResizeFactor)
  var
    i: Integer;
    CurScale: Double;
    CurOffset: LongInt;
    Child: TAutoSizeBox;
  begin
    Factor.Scale:=0;
    Factor.Offset:=0;
    ResizeableCount:=0;
    case ShrinkStyle of

    crsAnchorAligning:
      exit; // no resizing

    crsScaleChilds,crsHomogenousChildResize:
      for i:=0 to ChildCount[Orientation]-1 do begin
        Child:=Childs[Orientation][i];
        if (Child.PreferredSize[Orientation]<=Child.MinimumSize[Orientation])
        or (Child.PreferredSize[Orientation]<=1)
        then begin
          // this child can not be further shrinked
          continue;
        end;
        inc(ResizeableCount);

        case ShrinkStyle of

        crsScaleChilds:
          begin
            {$IFDEF CPU64}
            {$NOTE remove this workaround, when compiler bug fixed}
            if Child.MinimumSize[Orientation]=0 then
              CurScale:=0
            else
              CurScale:=double(Child.MinimumSize[Orientation])
                        /Child.PreferredSize[Orientation];
            {$ELSE}
            CurScale:=double(Child.MinimumSize[Orientation])
                      /Child.PreferredSize[Orientation];
            {$ENDIF}
            CurOffset:=Child.PreferredSize[Orientation]
                       -Child.MinimumSize[Orientation];
            if (Factor.Offset=0) or (Factor.Scale<CurScale) then begin
              Factor.Scale:=CurScale;
              Factor.Offset:=CurOffset;
            end;
          end;

        crsHomogenousChildResize:
          begin
            {$IFDEF CPU64}
            {$NOTE remove this workaround, when compiler bug fixed}
            if Child.MinimumSize[Orientation]=0 then
              CurScale:=0
            else
              CurScale:=double(Child.MinimumSize[Orientation])
                        /Child.PreferredSize[Orientation];
            {$ELSE}
            CurScale:=double(Child.MinimumSize[Orientation])
                      /Child.PreferredSize[Orientation];
            {$ENDIF}
            CurOffset:=Child.PreferredSize[Orientation]
                       -Child.MinimumSize[Orientation];
            if (Factor.Offset=0) or (Factor.Offset>CurOffset) then begin
              Factor.Scale:=CurScale;
              Factor.Offset:=CurOffset;
            end;
          end;

        end;
      end;

    crsHomogenousSpaceResize:
      for i:=0 to ChildCount[Orientation]-1 do begin
        Child:=Childs[Orientation][i];
        if i=0 then begin
          CurScale:=double(TargetSize);
          CurOffset:=Child.BorderLeftTop[Orientation];
          if CurOffset>0 then begin
            inc(ResizeableCount);
            if (Factor.Offset=0) or (Factor.Offset>CurOffset) then begin
              Factor.Scale:=CurScale;
              Factor.Offset:=CurOffset;
            end;
          end;
        end;
        CurScale:=double(TargetSize);
        CurOffset:=Child.BorderRightBottom[Orientation];
        if CurOffset>0 then begin
          inc(ResizeableCount);
          if (Factor.Offset=0) or (Factor.Offset>CurOffset) then begin
            Factor.Scale:=CurScale;
            Factor.Offset:=CurOffset;
          end;
        end;
      end;

    else
      raise Exception.Create('TAutoSizeBox.ResizeChilds');

    end;
  end;

  procedure ShrinkChilds(const Factor: TResizeFactor);
  var
    i: Integer;
    Child: TAutoSizeBox;
    DiffSize: Integer;
    NewSize: LongInt;
    OldSize: LongInt;
  begin
    for i:=0 to ChildCount[Orientation]-1 do begin
      Child:=Childs[Orientation][i];
      if (Child.PreferredSize[Orientation]<=1)
      or (Child.PreferredSize[Orientation]<=Child.MinimumSize[Orientation])
      then begin
        // this child can not be further shrinked
        continue;
      end;

      case ShrinkStyle of

      crsScaleChilds:
        begin
          // scale PreferredSize
          DiffSize:=CurSize-TargetSize;
          OldSize:=Child.PreferredSize[Orientation];
          NewSize:=Min(round(OldSize*Factor.Scale),OldSize-1);
          NewSize:=Max(Max(1,NewSize),OldSize-DiffSize);
          dec(CurSize,OldSize-NewSize);
          Child.PreferredSize[Orientation]:=NewSize;
        end;

      crsHomogenousChildResize:
        begin
          // add to PreferredSize
          DiffSize:=CurSize-TargetSize;
          OldSize:=Child.PreferredSize[Orientation];
          NewSize:=OldSize-Factor.Offset;
          NewSize:=Max(Max(NewSize,1),OldSize-DiffSize);
          dec(CurSize,OldSize-NewSize);
          Child.PreferredSize[Orientation]:=NewSize;
        end;

      crsHomogenousSpaceResize:
        begin
          if i=0 then begin
            // add to left/top border
            DiffSize:=CurSize-TargetSize;
            OldSize:=Child.BorderLeftTop[Orientation];
            NewSize:=Max(Max(0,OldSize-Factor.Offset),OldSize-DiffSize);
            dec(CurSize,OldSize-NewSize);
            Child.BorderLeftTop[Orientation]:=NewSize;
          end;
          // add to right/bottom border
          DiffSize:=CurSize-TargetSize;
          OldSize:=Child.BorderRightBottom[Orientation];
          NewSize:=Max(Max(0,OldSize-Factor.Offset),OldSize-DiffSize);
          dec(CurSize,OldSize-NewSize);
          Child.BorderRightBottom[Orientation]:=NewSize;
          if i<ChildCount[Orientation]-1 then
            Child.BorderLeftTop[Orientation]:=NewSize;
        end;

      end;
    end;
  end;

var
  MaxResizeFactorPerItem, MinResizeFactorPerItem, CurScale: TResizeFactor;
  ResizeableCount: integer;
  i: Integer;
begin
  CurSize:=GetChildTotalSize;
  //DebugLn('TAutoSizeBox.ResizeChilds CurSize=',dbgs(CurSize),' TargetSize=',dbgs(TargetSize));
  EnlargeStyle:=crsAnchorAligning;
  ShrinkStyle:=crsAnchorAligning;
  i:=0;
  if TargetSize>CurSize then begin
    // enlarge
    if Orientation=asboHorizontal then
      EnlargeStyle:=ChildSizing.EnlargeHorizontal
    else
      EnlargeStyle:=ChildSizing.EnlargeVertical;
    while TargetSize>CurSize do begin
      // shrink childs
      GetChildMaxResize(MaxResizeFactorPerItem,ResizeableCount);
      if (ResizeableCount=0) or (MaxResizeFactorPerItem.Offset=0) then break;
      
      CurScale.Scale:=(double(TargetSize)/CurSize);
      if (MaxResizeFactorPerItem.Scale>0)
      and (MaxResizeFactorPerItem.Scale<CurScale.Scale) then
        CurScale.Scale:=MaxResizeFactorPerItem.Scale;
        
      CurScale.Offset:=((TargetSize-CurSize-1) div ResizeableCount)+1;
      // note: the above formula makes sure, that Offset>0
      if (MaxResizeFactorPerItem.Offset>0)
      and (MaxResizeFactorPerItem.Offset<CurScale.Offset) then
        CurScale.Offset:=MaxResizeFactorPerItem.Offset;
      
      EnlargeChilds(CurScale);
      inc(i);
      if i>1000 then RaiseGDBException('TAutoSizeBox.ResizeChilds consistency error');
    end;
  end else if TargetSize<CurSize then begin
    // shrink
    if Orientation=asboHorizontal then
      ShrinkStyle:=ChildSizing.ShrinkHorizontal
    else
      ShrinkStyle:=ChildSizing.ShrinkVertical;
    while TargetSize<CurSize do begin
      GetChildMinResize(MinResizeFactorPerItem,ResizeableCount);
      if (ResizeableCount=0) or (MinResizeFactorPerItem.Offset=0) then break;

      CurScale.Scale:=(double(TargetSize)/CurSize);
      if (MinResizeFactorPerItem.Scale>0)
      and (MinResizeFactorPerItem.Scale>CurScale.Scale) then
        CurScale.Scale:=MinResizeFactorPerItem.Scale;

      CurScale.Offset:=((CurSize-TargetSize-1) div ResizeableCount)+1;
      // note: the above formula makes sure, that Offset>0
      if (MinResizeFactorPerItem.Offset>0)
      and (MinResizeFactorPerItem.Offset>CurScale.Offset) then
        CurScale.Offset:=MinResizeFactorPerItem.Offset;

      ShrinkChilds(CurScale);
      inc(i);
      if i>1000 then RaiseGDBException('TAutoSizeBox.ResizeChilds consistency error');
    end;
  end;
  ComputeLeftTops(Orientation);
end;

procedure TAutoSizeBox.ResizeTable(ChildSizing: TControlChildSizing;
  TargetWidth, TargetHeight: integer);
begin
  // resize rows and columns
  ResizeChilds(ChildSizing,asboHorizontal,TargetWidth);
  ResizeChilds(ChildSizing,asboVertical,TargetHeight);
end;

procedure TAutoSizeBox.ComputeTableControlBounds(ChildSizing: TControlChildSizing
  );
var
  y: Integer;
  RowBox: TAutoSizeBox;
  x: Integer;
  ColBox: TAutoSizeBox;
  ControlBox: TAutoSizeBox;
  CurControl: TControl;
  NewBounds: TRect;
  CellBounds: TRect;
  NewWidth: LongInt;
  NewHeight: LongInt;
begin
  //WriteDebugReport;
  for y:=0 to ChildCount[asboVertical]-1 do begin
    RowBox:=Childs[asboVertical][y];
    for x:=0 to RowBox.ChildCount[asboHorizontal]-1 do begin
      ControlBox:=RowBox.Childs[asboHorizontal][x];
      ColBox:=ControlBox.Parent[asboVertical];
      CurControl:=ControlBox.Control;
      if CurControl=nil then continue;
      CellBounds:=Bounds(ColBox.LeftTop[asboHorizontal],
                         RowBox.LeftTop[asboVertical],
                         ColBox.PreferredSize[asboHorizontal],
                         RowBox.PreferredSize[asboVertical]);
      NewBounds.Left:=CellBounds.Left;
      NewBounds.Top:=CellBounds.Top;
      NewWidth:=ControlBox.PreferredSize[asboHorizontal];
      NewHeight:=ControlBox.PreferredSize[asboVertical];
      if (NewWidth<ColBox.PreferredSize[asboHorizontal]) then begin
        // column is bigger than preferred width of the control
        //DebugLn('TAutoSizeBox.SetTableControlBounds ',DbgSName(CurControl),' ',dbgs(ord(CurControl.BorderSpacing.CellAlignHorizontal)));
        case CurControl.BorderSpacing.CellAlignHorizontal of
        ccaFill:        NewWidth:=CellBounds.Right-CellBounds.Left;
        ccaLeftTop:     ;
        ccaRightBottom: NewBounds.Left:=CellBounds.Right-NewWidth;
        ccaCenter:      NewBounds.Left:=NewBounds.Left
                             +(CellBounds.Right-CellBounds.Left-NewWidth) div 2;
        end;
      end else if (NewWidth>ColBox.PreferredSize[asboHorizontal]) then begin
        // column is smaller than preferred width of the control
        if ChildSizing.ShrinkHorizontal
        in [crsScaleChilds,crsHomogenousChildResize]
        then
          NewWidth:=CellBounds.Right-CellBounds.Left;
      end;
      if (NewHeight<ColBox.PreferredSize[asboVertical]) then begin
        // column is bigger than preferred height of the control
        case CurControl.BorderSpacing.CellAlignVertical of
        ccaFill:        NewHeight:=CellBounds.Bottom-CellBounds.Top;
        ccaLeftTop:     ;
        ccaRightBottom: NewBounds.Top:=CellBounds.Bottom-NewHeight;
        ccaCenter:      NewBounds.Top:=NewBounds.Top
                            +(CellBounds.Bottom-CellBounds.Top-NewHeight) div 2;
        end;
      end else if (NewHeight>ColBox.PreferredSize[asboVertical]) then begin
        // column is smaller than preferred height of the control
        if ChildSizing.ShrinkVertical
        in [crsScaleChilds,crsHomogenousChildResize]
        then
          NewHeight:=CellBounds.Bottom-CellBounds.Top;
      end;

      NewBounds.Right:=NewBounds.Left+NewWidth;
      NewBounds.Bottom:=NewBounds.Top+NewHeight;
      ControlBox.NewControlBounds:=NewBounds;
    end;
  end;
end;

function TAutoSizeBox.SetTableControlBounds(ChildSizing: TControlChildSizing
  ): boolean;
var
  y: Integer;
  RowBox: TAutoSizeBox;
  x: Integer;
  ControlBox: TAutoSizeBox;
  CurControl: TControl;
  NewBounds: TRect;
  OldBounds: TRect;
begin
  Result:=false;
  //WriteDebugReport;
  for y:=0 to ChildCount[asboVertical]-1 do begin
    RowBox:=Childs[asboVertical][y];
    for x:=0 to RowBox.ChildCount[asboHorizontal]-1 do begin
      ControlBox:=RowBox.Childs[asboHorizontal][x];
      CurControl:=ControlBox.Control;
      if CurControl=nil then continue;
      NewBounds:=ControlBox.NewControlBounds;
      OldBounds:=CurControl.BoundsRect;
      if not CompareRect(@NewBounds,@OldBounds) then begin
        Result:=true;
        CurControl.SetAlignedBounds(NewBounds.Left,
                                    NewBounds.Top,
                                    NewBounds.Right-NewBounds.Left,
                                    NewBounds.Bottom-NewBounds.Top);
      end;
    end;
  end;
end;

function TAutoSizeBox.AlignControlsInTable(ListOfControls: TFPList;
  ChildSizing: TControlChildSizing; TargetWidth, TargetHeight: integer;
  Apply: boolean): boolean;
// true if a control was modified
begin
  SetTableControls(ListOfControls,ChildSizing);
  //WriteDebugReport('after SetTableControls');
  SumTable;
  //WriteDebugReport('after SumTable');
  ResizeTable(ChildSizing,TargetWidth,TargetHeight);
  //WriteDebugReport('after ResizeTable');
  ComputeTableControlBounds(ChildSizing);
  //WriteDebugReport('after ComputeTableControlBounds');
  Result:=Apply and SetTableControlBounds(ChildSizing);
end;

procedure TAutoSizeBox.WriteDebugReport(const Title: string);
var
  y: Integer;
  RowBox: TAutoSizeBox;
  x: Integer;
  CellBox: TAutoSizeBox;
  ColBox: TAutoSizeBox;
begin
  DebugLn('TAutoSizeBox.WriteDebugReport '+Title
    +' ChildCounts=',dbgs(ChildCount[asboHorizontal]),'x',dbgs(ChildCount[asboVertical]));
  for y:=0 to ChildCount[asboVertical]-1 do begin
    RowBox:=Childs[asboVertical][y];
    DbgOut('  Row='+dbgs(y),
            ' MinY='+dbgs(RowBox.MinimumSize[asboVertical]),
            ' MaxY='+dbgs(RowBox.MaximumSize[asboVertical]),
            ' PrefY='+dbgs(RowBox.PreferredSize[asboVertical]),
            ' BorderTop=',dbgs(RowBox.BorderLeftTop[asboVertical]),
            ' #Col='+dbgs(RowBox.ChildCount[asboHorizontal]));
    for x:=0 to RowBox.ChildCount[asboHorizontal]-1 do begin
      CellBox:=RowBox.Childs[asboHorizontal][x];
      DbgOut(' CellControl=',DbgSName(CellBox.Control),
             ' Min='+dbgs(CellBox.MinimumSize[asboHorizontal])+'x'+dbgs(CellBox.MinimumSize[asboVertical]),
             ' Max='+dbgs(CellBox.MaximumSize[asboHorizontal])+'x'+dbgs(CellBox.MaximumSize[asboVertical]),
             ' BorderLeft=',dbgs(CellBox.BorderLeftTop[asboHorizontal]),
             ' Pref='+dbgs(CellBox.PreferredSize[asboHorizontal])+'x'+dbgs(CellBox.PreferredSize[asboVertical]),
             '');
    end;
    DebugLn;
  end;
  DbgOut(' Columns: ');
  for x:=0 to ChildCount[asboHorizontal]-1 do begin
    ColBox:=Childs[asboHorizontal][x];
    DbgOut(' Col='+dbgs(ColBox.Index[asboHorizontal]),
           ' Min='+dbgs(ColBox.MinimumSize[asboHorizontal]),
           ' Max='+dbgs(ColBox.MaximumSize[asboHorizontal]),
           ' Pref='+dbgs(ColBox.PreferredSize[asboHorizontal]),
           '');
  end;
  DebugLn;
end;

destructor TAutoSizeBox.Destroy;
var
  o: TAutoSizeBoxOrientation;
begin
  // unlink from parent
  for o:=Low(TAutoSizeBoxOrientation) to high(TAutoSizeBoxOrientation) do
    if Parent[o]<>nil then
      Parent[o].Childs[o][Index[o]]:=nil;
  Clear;
  inherited Destroy;
end;

procedure TAutoSizeBox.Clear;
var
  o: TAutoSizeBoxOrientation;
  i: Integer;
begin
  // free all childs
  for o:=Low(TAutoSizeBoxOrientation) to high(TAutoSizeBoxOrientation) do
    for i:=0 to ChildCount[o]-1 do
      Childs[o][i].Free;
  // free childs arrays
  for o:=Low(TAutoSizeBoxOrientation) to high(TAutoSizeBoxOrientation) do
    ReallocMem(Childs[o],0);
end;

{------------------------------------------------------------------------------
  function TWinControl.AutoSizeDelayed: boolean;
------------------------------------------------------------------------------}
function TWinControl.AutoSizeDelayed: boolean;
begin
  Result:=// no handle means not visible
          (not HandleAllocated)
          or ((not FShowing) and (not (csDesigning in ComponentState)))
          // during handle creation no autosize
          or (wcfCreatingChildHandles in FWinControlFlags)
          or (inherited AutoSizeDelayed);
  //if Result then debugln('TWinControl.AutoSizeDelayed A ',DbgSName(Self),' wcfCreatingChildHandles=',dbgs(wcfCreatingChildHandles in FWinControlFlags),' csLoading=',dbgs(csLoading in ComponentState));
  {$IFDEF VerboseCanAutoSize}
  if Result {and AutoSize} then begin
    DbgOut('TWinControl.AutoSizeDelayed Self='+DbgSName(Self)+' ');
    if not HandleAllocated then debugln('not HandleAllocated')
    else if not FShowing then debugln('not FShowing')
    else if wcfCreatingChildHandles in FWinControlFlags then debugln('wcfCreatingChildHandles')
    else debugln('inherited AutoSizeDelayed');
  end;
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  TWinControl AdjustClientRect
------------------------------------------------------------------------------}
procedure TWinControl.AdjustClientRect(var ARect: TRect);
begin
  //Not used.  It's a virtual procedure that should be overriden.
end;

{------------------------------------------------------------------------------
  TWinControl CreateControlAlignList
  
  Creates a list of controls that need to be aligned via TheAlign.
------------------------------------------------------------------------------}
procedure TWinControl.CreateControlAlignList(TheAlign: TAlign;
  AlignList: TFPList; StartControl: TControl);

  function InsertBefore(Control1, Control2: TControl;
    AAlign: TAlign): Boolean;
  begin
    Result := False;
    case AAlign of
      alTop:
        Result := (Control1.Top < Control2.Top)
             or ((Control1.Top = Control2.Top)
                 and (Control1.FBaseBounds.Top<Control2.FBaseBounds.Top));
      alLeft: Result := (Control1.Left < Control2.Left)
                 or ((Control1.Left = Control2.Left)
                     and (Control1.FBaseBounds.Left<Control2.FBaseBounds.Left));
      // contrary to VCL, LCL uses > for alBottom, alRight
      // Maybe it is a bug in the VCL.
      // This results in first control is put rightmost/bottommost
      alBottom: Result :=
          ((Control1.Top + Control1.Height) > (Control2.Top + Control2.Height))
           or (((Control1.Top + Control1.Height) = (Control2.Top + Control2.Height))
             and (Control1.FBaseBounds.Bottom > Control2.FBaseBounds.Bottom));
      alRight: Result :=
          ((Control1.Left + Control1.Width) > (Control2.Left + Control2.Width))
           or (((Control1.Left + Control1.Width) = (Control2.Left + Control2.Width))
             and (Control1.FBaseBounds.Right > Control2.FBaseBounds.Right));
    end;
  end;

var
  I, X: Integer;
  Control: TControl;
begin
  AlignList.Clear;

  // first add the current control
  if (StartControl <> nil)
    and (StartControl.Align = TheAlign)
    and ((TheAlign = alNone)
      or StartControl.IsControlVisible)
  then
    AlignList.Add(StartControl);

  // then add all other
  for I := 0 to ControlCount - 1 do
  begin
    Control := Controls[I];

    if (Control.Align = TheAlign) and Control.IsControlVisible then
    begin
      if Control = StartControl then Continue;

      X := 0;
      while (X < AlignList.Count)
      and not InsertBefore(Control, TControl(AlignList[X]), TheAlign) do
        Inc(X);
      AlignList.Insert(X, Control);
    end;
  end;
end;

{------------------------------------------------------------------------------
  TWinControl AlignControls

  Align child controls
------------------------------------------------------------------------------}
procedure TWinControl.AlignControls(AControl: TControl;
  var RemainingClientRect: TRect);
{ $DEFINE CHECK_POSITION}
var
  AlignList: TFPList;
  BoundsMutated: boolean;
  LastBoundsMutated: TControl;
  LastBoundsMutatedOld: TRect;
  RemainingBorderSpace: TRect; // borderspace around RemainingClientRect
                               // e.g. Right=3 means borderspace of 3

  function NeedAlignWork: Boolean;
  var
    I: Integer;
    CurControl: TControl;
  begin
    Result := True;
    for I := ControlCount - 1 downto 0 do
    begin
      CurControl:=Controls[I];
      if (CurControl.Align <> alNone)
      or (CurControl.Anchors <> [akLeft, akTop])
      or (CurControl.AnchorSide[akLeft].Control<>nil)
      or (CurControl.AnchorSide[akTop].Control<>nil)
      or (ChildSizing.Layout<>cclNone)
      then Exit;
    end;
    Result := False;
  end;

  function Anchored(Align: TAlign; Anchors: TAnchors): Boolean;
  begin
    case Align of
      alLeft: Result := akLeft in Anchors;
      alTop: Result := akTop in Anchors;
      alRight: Result := akRight in Anchors;
      alBottom: Result := akBottom in Anchors;
      alClient: Result := Anchors = [akLeft, akTop, akRight, akBottom];
    else
      Result := False;
    end;
  end;
  
  function IsNotAligned(AControl: TControl): boolean;
  begin
    Result:=(AControl.Align=alNone)
        and (AControl.Anchors=[akLeft,akTop])
        and (AControl.AnchorSide[akLeft].Control=nil)
        and (AControl.AnchorSide[akTop].Control=nil);
  end;

  procedure DoPosition(Control: TControl; AAlign: TAlign);
  var
    NewLeft, NewTop, NewWidth, NewHeight: Integer;
    ParentBaseClientSize: TPoint;
    ParentClientWidth: integer;
    ParentClientHeight: integer;
    CurBaseBounds: TRect;
    NewRight: Integer;// temp variable, not always valid, use with care !
    NewBottom: Integer;// temp variable, not always valid, use with care !

    MinWidth: Integer;
    MaxWidth: Integer;
    MinHeight: Integer;
    MaxHeight: Integer;
    CurRemainingClientRect: TRect;
    CurRemainingBorderSpace: TRect; // borderspace around RemainingClientRect
                                    // e.g. Right=3 means borderspace of 3
    ChildAroundSpace: TRect;
    AnchorSideCacheValid: array[TAnchorKind] of boolean;
    AnchorSideCache: array[TAnchorKind] of integer;
    CurAnchors: TAnchors;
    CurAlignAnchors: TAnchors;
    OldBounds: TRect;
    NewBounds: TRect;
    r: TRect;

    function ConstraintWidth(NewWidth: integer): Integer;
    begin
      Result:=NewWidth;
      if (MaxWidth>=MinWidth) and (Result>MaxWidth) and (MaxWidth>0) then
        Result:=MaxWidth;
      if Result<MinWidth then Result:=MinWidth;
    end;

    procedure ConstraintWidth(var NewLeft, NewWidth: integer);
    var
      ConWidth: LongInt;
    begin
      ConWidth:=ConstraintWidth(NewWidth);
      if ConWidth<>NewWidth then begin
        if [akLeft,akRight]*CurAnchors=[akRight] then
          // move left side, keep right
          inc(NewLeft,NewWidth-ConWidth);
        NewWidth:=ConWidth;
      end;
    end;

    function ConstraintHeight(NewHeight: integer): Integer;
    begin
      Result:=NewHeight;
      if (MaxHeight>=MinHeight) and (Result>MaxHeight) and (MaxHeight>0) then
        Result:=MaxHeight;
      if Result<MinHeight then Result:=MinHeight;
    end;

    procedure ConstraintHeight(var NewTop, NewHeight: integer);
    var
      ConHeight: LongInt;
    begin
      ConHeight:=ConstraintHeight(NewHeight);
      if ConHeight<>NewHeight then begin
        if [akTop,akBottom]*CurAnchors=[akBottom] then
          // move top side, keep bottom
          inc(NewTop,NewHeight-ConHeight);
        NewHeight:=ConHeight;
      end;
    end;
    
    procedure InitAnchorSideCache;
    var
      a: TAnchorKind;
    begin
      for a:=Low(TAnchorKind) to High(TAnchorKind) do
        AnchorSideCacheValid[a]:=false;
    end;
    
    function GetAnchorSidePosition(Kind: TAnchorKind;
      DefaultPosition: Integer): integer;
    // calculates the position in pixels of a side due to anchors
    // For example: if akLeft is set, it returns the coordinate for the left anchor
    var
      CurAnchorSide: TAnchorSide;
      ReferenceControl: TControl;
      ReferenceSide: TAnchorSideReference;
      Position: Integer;
    begin
      if AnchorSideCacheValid[Kind] then begin
        Result:=AnchorSideCache[Kind];
        exit;
      end;
      Result:=DefaultPosition;
      CurAnchorSide:=Control.AnchorSide[Kind];
      //if CheckPosition(Control) and (Kind=akLeft) then debugln(['GetAnchorSidePosition A Self=',DbgSName(Self),' Control=',DbgSName(Control),' CurAnchorSide.Control=',DbgSName(CurAnchorSide.Control),' Spacing=',Control.BorderSpacing.GetSpace(Kind)]);
      CurAnchorSide.GetSidePosition(ReferenceControl,ReferenceSide,Position);
      if ReferenceControl<>nil then begin
        //DebugLn(['GetAnchorSidePosition ',DbgSName(Control),' ReferenceControl=',DbgSName(ReferenceControl)]);
        Result:=Position;
      end;
      //if CheckPosition(Control) and (Kind=akLeft) then begin
        //debugln('GetAnchorSidePosition B Self=',DbgSName(Self),' Control=',DbgSName(Control),' Result=',dbgs(Result),' ReferenceControl=',dbgsName(ReferenceControl));
        //if ReferenceControl<>nil then DebugLn(['GetAnchorSidePosition ReferenceControl.BoundsRect=',dbgs(ReferenceControl.BoundsRect)]);
      //end;
      AnchorSideCacheValid[Kind]:=true;
      AnchorSideCache[Kind]:=Result;
      if ReferenceSide=asrTop then ;
    end;
    
  begin
    {$IFDEF CHECK_POSITION}
    if CheckPosition(Control) then
    with Control do
      DebugLn('[TWinControl.AlignControls.DoPosition] A Control=',dbgsName(Control),' ',dbgs(Left),',',dbgs(Top),',',dbgs(Width),',',dbgs(Height),' recalculate the anchors=',dbgs(Control.Anchors <> AnchorAlign[AAlign]),' Align=',AlignNames[AAlign]);
    {$ENDIF}

    with Control do begin
      // get constraints
      MinWidth:=Constraints.EffectiveMinWidth;
      if MinWidth<0 then MinWidth:=0;
      MaxWidth:=Constraints.EffectiveMaxWidth;
      MinHeight:=Constraints.EffectiveMinHeight;
      if MinHeight<0 then MinHeight:=0;
      MaxHeight:=Constraints.EffectiveMaxHeight;
      
      // get anchors set by Align
      CurAlignAnchors:=[];
      if Align in [alLeft,alRight,alBottom,alTop,alClient] then
        CurAlignAnchors:=AnchorAlign[Align];
      CurAnchors:=Anchors+CurAlignAnchors;

      // get default bounds
      NewLeft:=Left;
      NewTop:=Top;
      NewWidth:=Width;
      NewHeight:=Height;
      ConstraintWidth(NewLeft,NewWidth);
      ConstraintHeight(NewTop,NewHeight);
    end;
    r:=Control.Parent.GetLogicalClientRect;
    ParentClientWidth:=r.Right;
    ParentClientHeight:=r.Bottom;

    InitAnchorSideCache;
    
    { Recalculate the anchors

      Use Anchors to ensure that a control maintains its current position
      relative to an edge of its parent or another sibling.
      This is controlled with the AnchorSide properties.

      1. If AnchorSide[].Control is not set, the distance is kept relative to
      the edges of the client area of its parent.
      When its parent is resized, the control holds its position relative to the
      edges to which it is anchored.
      If a control is anchored to opposite edges of its parent, the control
      stretches when its parent is resized. For example, if a control has its
      Anchors property set to [akLeft,akRight], the control stretches when the
      width of its parent changes.
      Anchors is enforced only when the parent is resized. Thus, for example,
      if a control is anchored to opposite edges of a form at design time and
      the form is created in a maximized state, the control is not stretched
      because the form is not resized after the control is created.
      
      2. If AnchorSide[].Control is set, the BorderSpace properties defines the
      distance to another sibling (i.e. AnchorSide[].Control).
    }
    if (AAlign = alNone) or (Control.Anchors <> CurAlignAnchors) then begin
      // at least one side is anchored without align
    
      // Get the base bounds. The base bounds are the user defined bounds
      // without automatic aligning and/or anchoring

      // get base size of parents client area
      ParentBaseClientSize:=Control.FBaseParentClientSize;
      if (ParentBaseClientSize.X=0)
      and (ParentBaseClientSize.Y=0) then
        ParentBaseClientSize:=Point(ParentClientWidth,ParentClientHeight);

      // get base bounds of Control
      CurBaseBounds:=Control.FBaseBounds;
      if (CurBaseBounds.Right=CurBaseBounds.Left)
      and (CurBaseBounds.Bottom=CurBaseBounds.Top) then
        CurBaseBounds:=Control.BoundsRect;

      {$IFDEF CHECK_POSITION}
      //if csDesigning in ComponentState then
      if CheckPosition(Control) then
       DebugLn('[TWinControl.AlignControls.DoPosition] Before Anchoring ',
        ' Self='+DbgSName(Self),' Control='+DbgSName(Control),
        ' CurBaseBounds='+dbgs(CurBaseBounds.Left)+','+dbgs(CurBaseBounds.Top)+','+dbgs(CurBaseBounds.Right-CurBaseBounds.Left)+','+dbgs(CurBaseBounds.Bottom-CurBaseBounds.Top),
        ' ParentBaseClientSize='+dbgs(ParentBaseClientSize.X)+','+dbgs(ParentBaseClientSize.Y),
        ' ControlParent.Client='+dbgs(ParentClientWidth)+','+dbgs(ParentClientHeight),
        ' NewBounds='+dbgs(NewLeft)+','+dbgs(NewTop)+','+dbgs(NewWidth)+','+dbgs(NewHeight),
        '');
      {$ENDIF}

      if akLeft in CurAnchors then begin
        // keep distance to left side of parent or another sibling
        NewLeft:=GetAnchorSidePosition(akLeft,CurBaseBounds.Left);
        if akRight in CurAnchors then begin
          // keep distance to right side of parent or another sibling
          // -> change the width
          NewRight:=ParentClientWidth
                    -(ParentBaseClientSize.X-CurBaseBounds.Right);
          if (not (akRight in CurAlignAnchors))
          and (akRight in Control.Anchors) then
            NewRight:=GetAnchorSidePosition(akRight,NewRight);
          NewWidth:=ConstraintWidth(NewRight-NewLeft);
        end else begin
          // do not anchor to the right
          // -> keep new width
        end;
      end else begin
        // do not anchor to the left
        if akRight in CurAnchors then begin
          // keep distance to right side of parent
          // and keep new width
          NewRight:=ParentClientWidth
                    -(ParentBaseClientSize.X-CurBaseBounds.Right);
          if (not (akRight in CurAlignAnchors))
          and (akRight in Control.Anchors) then
            NewRight:=GetAnchorSidePosition(akRight,NewRight);
          NewLeft:=NewRight-NewWidth;
        end else begin
          // do not anchor to the right
          // -> keep new width and scale center position.
          NewLeft:=MulDiv(ParentClientWidth,
                          (CurBaseBounds.Left+CurBaseBounds.Right) div 2,
                          ParentBaseClientSize.X)
                   -(NewWidth div 2);
        end;
      end;

      if akTop in CurAnchors then begin
        // keep distance to top side of parent
        NewTop:=GetAnchorSidePosition(akTop,CurBaseBounds.Top);
        if akBottom in CurAnchors then begin
          // keep distance to bottom side of parent
          // -> change the height
          NewBottom:=ParentClientHeight
                    -(ParentBaseClientSize.Y-CurBaseBounds.Bottom);
          if (not (akBottom in CurAlignAnchors))
          and (akBottom in Control.Anchors) then
            NewBottom:=GetAnchorSidePosition(akBottom,NewBottom);
          NewHeight:=ConstraintHeight(NewBottom-NewTop);
        end else begin
          // do not anchor to the bottom
          // -> keep new height
        end;
      end else begin
        // do not anchor to the top
        if akBottom in CurAnchors then begin
          // keep distance to bottom side of parent
          // and keep new height
          NewBottom:=ParentClientHeight
                    -(ParentBaseClientSize.Y-CurBaseBounds.Bottom);
          if (not (akBottom in CurAlignAnchors))
          and (akBottom in Control.Anchors) then
            NewBottom:=GetAnchorSidePosition(akBottom,NewBottom);
          NewTop:=NewBottom-NewHeight;
        end else begin
          // do not anchor to the bottom
          // -> keep new height and scale center position.
          NewTop:=MulDiv(ParentClientHeight,
                         (CurBaseBounds.Top+CurBaseBounds.Bottom) div 2,
                         ParentBaseClientSize.Y)
                  -(NewHeight div 2);
        end;
      end;
      {$IFDEF CHECK_POSITION}
      //if csDesigning in ComponentState then
      if CheckPosition(Control) then
      with Control do begin
        DebugLn(['[TWinControl.AlignControls.DoPosition] After Anchoring',
          ' ',Name,':',ClassName,
          ' Align=',AlignNames[AAlign],
          ' Control=',dbgsName(Control),
          ' Old= l=',Left,',t=',Top,',w=',Width,',h=',Height,
          ' New= l=',NewLeft,',t=',NewTop,',w=',NewWidth,',h=',NewHeight,
          '']);
        DebugLn(['DoPosition akRight=',akRight in CurAnchors,' ',GetAnchorSidePosition(akRight,NewLeft+NewWidth)]);
      end;
      {$ENDIF}
    end;

    // set min size to stop circling (this should not be needed. But if someone
    // plays/fixes the above code, new bugs can enter and there are far too many
    // combinations to test, and so the LCL can loop for some applications.
    // Prevent this, so users can at least report a bug.)
    if NewWidth<0 then NewWidth:=0;
    if NewHeight<0 then NewHeight:=0;

    if AAlign in [alLeft,alTop,alRight,alBottom,alClient] then begin
      { Realign

        Use Align to align a control to the top, bottom, left, right of a
        form or panel and have it remain there even if the size of the form,
        panel, or component that contains the control changes. When the parent
        is resized, an aligned control also resizes so that it continues to span
        the top, bottom, left, or right edge of the parent (more exact:
        span the remaining client area of its parent).
      }
      NewRight:=NewLeft+NewWidth;
      NewBottom:=NewTop+NewHeight;

      // calculate current RemainingClientRect for the current Control
      CurRemainingClientRect:=RemainingClientRect;
      CurRemainingBorderSpace:=RemainingBorderSpace;
      Control.BorderSpacing.GetSpaceAround(ChildAroundSpace);
      AdjustBorderSpace(CurRemainingClientRect,CurRemainingBorderSpace,
                        ChildAroundSpace);
      {$IFDEF CHECK_POSITION}
      if CheckPosition(Control) then
        DebugLn('DoPosition Before aligning ',dbgsName(Control),' akRight in AnchorAlign[AAlign]=',DbgS(akRight in AnchorAlign[AAlign]),
                ' akLeft in Control.Anchors=',DbgS(akLeft in Control.Anchors),
                //' ARect=',ARect.Left,',',ARect.Top,',',ARect.Right,',',ARect.Bottom,
                ' New=',DbgS(NewLeft,NewTop,NewRight,NewBottom));
      {$ENDIF}
      if akLeft in AnchorAlign[AAlign] then begin
        if (akRight in CurAnchors) then begin
          // left align and keep right border
          NewLeft:=CurRemainingClientRect.Left;
          NewRight:=NewLeft+ConstraintWidth(NewRight-NewLeft);
        end else begin
          // left align and right border free to move (-> keep width)
          dec(NewRight,NewLeft-CurRemainingClientRect.Left);
          NewLeft:=CurRemainingClientRect.Left;
        end;
      end;
      if akTop in AnchorAlign[AAlign] then begin
        if (akBottom in CurAnchors) then begin
          // top align and keep bottom border
          NewTop:=CurRemainingClientRect.Top;
          NewBottom:=NewTop+ConstraintHeight(NewBottom-NewTop);
        end else begin
          // top align and bottom border is free to move (-> keep height)
          dec(NewBottom,NewTop-CurRemainingClientRect.Top);
          NewTop:=CurRemainingClientRect.Top;
        end;
      end;
      if akRight in AnchorAlign[AAlign] then begin
        if (akLeft in CurAnchors) then begin
          // right align and keep left border
          NewWidth:=ConstraintWidth(CurRemainingClientRect.Right-NewLeft);
          if Align=alRight then begin
            // align to right (this overrides the keeping of left border)
            NewRight:=CurRemainingClientRect.Right;
            NewLeft:=NewRight-NewWidth;
          end else begin
            // keep left border overrides keeping right border
            NewRight:=NewLeft+NewWidth;
          end;
        end else begin
          // right align and left border free to move (-> keep width)
          inc(NewLeft,CurRemainingClientRect.Right-NewRight);
          NewRight:=CurRemainingClientRect.Right;
        end;
      end;
      if akBottom in AnchorAlign[AAlign] then begin
        if (akTop in CurAnchors) then begin
          // bottom align and keep top border
          NewHeight:=ConstraintHeight(CurRemainingClientRect.Bottom-NewTop);
          if AAlign=alBottom then begin
            // align to bottom (this overrides the keeping of top border)
            NewBottom:=CurRemainingClientRect.Bottom;
            NewTop:=NewBottom-NewHeight;
          end else begin
            // keeping top border overrides keeping bottom border
            NewBottom:=NewTop+NewHeight;
          end;
        end else begin
          // bottom align and top border free to move (-> keep height)
          inc(NewTop,CurRemainingClientRect.Bottom-NewBottom);
          NewBottom:=CurRemainingClientRect.Bottom;
        end;
      end;
      NewWidth:=Max(0,NewRight-NewLeft);
      NewHeight:=Max(0,NewBottom-NewTop);
      
      {$IFDEF CHECK_POSITION}
      //if csDesigning in Control.ComponentState then
      if CheckPosition(Control) then
        with Control do
          DebugLn('[TWinControl.AlignControls.DoPosition] After Aligning',
          ' ',Name,':',ClassName,
          ' Align=',AlignNames[AAlign],
          ' Control=',Name,':',ClassName,
          ' Old=',DbgS(Left,Top,Width,Height),
          ' New=',DbgS(NewLeft,NewTop,NewWidth,NewHeight),
          //' ARect=',ARect.Left,',',ARect.Top,',',ARect.Right-ARect.Left,',',ARect.Bottom-ARect.Top,
          '');
      {$ENDIF}
    end;
    
    // apply the constraints
    NewWidth:=ConstraintWidth(NewWidth);
    NewHeight:=ConstraintHeight(NewHeight);
    NewRight:=NewLeft+NewWidth;
    NewBottom:=NewTop+NewHeight;

    // set the new bounds
    if (Control.Left <> NewLeft) or (Control.Top <> NewTop)
    or (Control.Width <> NewWidth) or (Control.Height <> NewHeight) then begin
      {$IFDEF CHECK_POSITION}
      //if csDesigning in Control.ComponentState then
      if CheckPosition(Control) then
      with Control do
        DebugLn('[TWinControl.AlignControls.DoPosition] NEW BOUNDS Control=',DbgSName(Control),
                ' New=l=',dbgs(NewLeft)+',t='+dbgs(NewTop)+',w='+dbgs(NewWidth)+',h='+dbgs(NewHeight));
      {$ENDIF}
      // lock the base bounds, so that the new automatic bounds do not override
      // the user settings
      OldBounds:=Control.BoundsRect;
      Control.SetAlignedBounds(NewLeft, NewTop, NewWidth, NewHeight);
      //DebugLn(['DoPosition ',DbgSName(Control),' ',cfAutoSizeNeeded in Control.FControlFlags]);
      NewBounds:=Control.BoundsRect;
      BoundsMutated:=not CompareRect(@OldBounds,@NewBounds);
      if BoundsMutated then begin
        LastBoundsMutated:=Control;
        LastBoundsMutatedOld:=OldBounds;
      end;
      // Sometimes SetBounds change the bounds. For example due to constraints.
      // update the new bounds
      with Control do
      begin
        NewLeft:=Left;
        NewTop:=Top;
        NewWidth:=Width;
        NewHeight:=Height;
      end;
      {$IFDEF CHECK_POSITION}
      //if csDesigning in Control.ComponentState then
      if CheckPosition(Control) then
      with Control do
        DebugLn('[TWinControl.AlignControls.DoPosition] AFTER SETBOUND Control=',DbgSName(Control),' Bounds=',DbgS(Control.BoundsRect));
      {$ENDIF}
    end;

    // adjust the remaining client area
    case AAlign of
      alTop:
        begin
          RemainingClientRect.Top:=Min(NewTop+NewHeight,RemainingClientRect.Bottom);
          RemainingBorderSpace.Top:=0;
          AdjustBorderSpace(RemainingClientRect,RemainingBorderSpace,
            0,Max(ChildSizing.VerticalSpacing,ChildAroundSpace.Bottom),0,0);
        end;
      alBottom:
        begin
          RemainingClientRect.Bottom:=Max(NewTop,RemainingClientRect.Top);
          RemainingBorderSpace.Bottom:=0;
          AdjustBorderSpace(RemainingClientRect,RemainingBorderSpace,
            0,0,0,Max(ChildSizing.VerticalSpacing,ChildAroundSpace.Top));
        end;
      alLeft:
        begin
          RemainingClientRect.Left:=Min(NewLeft+NewWidth,RemainingClientRect.Right);
          RemainingBorderSpace.Left:=0;
          AdjustBorderSpace(RemainingClientRect,RemainingBorderSpace,
            Max(ChildSizing.HorizontalSpacing,ChildAroundSpace.Right),0,0,0);
        end;
      alRight:
        begin
          RemainingClientRect.Right:=Max(NewLeft,RemainingClientRect.Left);
          RemainingBorderSpace.Right:=0;
          AdjustBorderSpace(RemainingClientRect,RemainingBorderSpace,
            0,0,Max(ChildSizing.HorizontalSpacing,ChildAroundSpace.Left),0);
        end;
      alClient:
        begin
          // For VCL compatibility alClient should *not* reduce the free space,
          // so that several alClient controls can overlap. This can be used
          // for example to simulate a two page control and edit both pages
          // at designtime with SendToBack.
          // At runtime programs should use Visible instead of BringToFront to
          // reduce overhead.
          // See bug 10380.
        end;
    end;

    {$IFDEF CHECK_POSITION}
    if CheckPosition(Control) then
    with Control do
      DebugLn('[TWinControl.AlignControls.DoPosition] END Control=',
        Name,':',ClassName,
        ' ',DbgS(Left,Top,Width,Height),
        ' Align=',AlignNames[AAlign],
        //' ARect=',ARect.Left,',',ARect.Top,',',ARect.Right-ARect.Left,',',ARect.Bottom-ARect.Top,
        '');
    {$ENDIF}
  end;

  procedure DoAlign(AAlign: TAlign);
  var
    I: Integer;
    Control: TControl;
  begin
    //DebugLn(['DoAlign ',DbgSName(Self),' ',dbgs(AALign),' ClientRect=',dbgs(ClientRect),' ControlCount=',ControlCount]);
    CreateControlAlignList(AAlign,AlignList,AControl);
    {$IFDEF CHECK_POSITION}
    if CheckPosition(Self) then
      if AlignList.Count>0 then begin
        DbgOut('[TWinControl.AlignControls.DoAlign] Self=',DbgSName(Self),' Control=',dbgsName(AControl),
          ' current align=',AlignNames[AAlign],' AlignList=[');
        for i:=0 to AlignList.Count-1 do begin
          if i>0 then DbgOut(',');
          DbgOut(DbgSName(TObject(AlignList[i])));
        end;
        DebugLn(']');
      end;
    {$ENDIF}

    // let override handle them
    if DoAlignChildControls(AAlign,AControl,AlignList,RemainingClientRect) then
      exit;
    // remove controls that are positioned by other means
    if (AAlign=alNone) and (AutoSize or (ChildSizing.Layout<>cclNone)) then
      for I := AlignList.Count - 1 downto 0 do begin
        Control:=TControl(AlignList[I]);
        if IsNotAligned(Control) then AlignList.Delete(I);
      end;
    // anchor/align control
    for I := 0 to AlignList.Count - 1 do
      DoPosition(TControl(AlignList[I]), AAlign);
  end;
  
  procedure DoAlignNotAligned;
  // All controls, not aligned by their own properties, can be auto aligned.
  var
    i: Integer;
    Control: TControl;
  begin
    // check if ChildSizing aligning is enabled
    if (ChildSizing.Layout=cclNone) then
      exit;

    /// collect all 'not aligned' controls
    AlignList.Clear;
    for i:=0 to ControlCount-1 do begin
      Control := Controls[i];
      if IsNotAligned(Control) and Control.IsControlVisible then
        AlignList.Add(Control);
    end;
    //debugln('DoAlignNotAligned ',DbgSName(Self),' AlignList.Count=',dbgs(AlignList.Count));
    if AlignList.Count=0 then exit;

    LastBoundsMutated:=nil;
    AlignNonAlignedControls(AlignList,BoundsMutated);
  end;
  
var
  i: Integer;
  ChildControl: TControl;
  OldRemainingClientRect: TRect;
  OldRemainingBorderSpace: TRect;
  MaxTries: LongInt;
begin
  if wcfAligningControls in FWinControlFlags then exit;
  Include(FWinControlFlags,wcfAligningControls);
  try
    // call delayed autosize
    for i:=ControlCount-1 downto 0 do begin
      ChildControl:=Controls[i];
      if cfAutoSizeNeeded in ChildControl.FControlFlags then begin
        //DebugLn(['TWinControl.AlignControls ',DbgSName(Self),' autosize needed for child ',DbgSName(ChildControl)]);
        ChildControl.AdjustSize;
      end;
    end;

    // unset all align needed flags
    Exclude(FWinControlFlags,wcfReAlignNeeded);
    for i:=ControlCount-1 downto 0 do begin
      ChildControl:=Controls[i];
      Exclude(ChildControl.FControlFlags,cfRequestAlignNeeded);
    end;

    //if csDesigning in ComponentState then begin
      //DebugLn('[TWinControl.AlignControls] ',Name,':',Classname,' ',Left,',',Top,',',Width,',',Height,' AlignWork=',NeedAlignWork,' ControlCount=',ControlCount);
      //if AControl<>nil then DebugLn('  AControl=',AControl.Name,':',AControl.ClassName);
    //end;
    if NeedAlignWork then
    begin
      AdjustClientRect(RemainingClientRect);
      //DebugLn(['TWinControl.AlignControls AAA1 ',DbgSName(Self),' RemainingClientRect=',dbgs(RemainingClientRect),' ',dbgs(ClientRect)]);
      RemainingBorderSpace:=Rect(0,0,0,0);
      // adjust RemainingClientRect by ChildSizing properties
      AdjustBorderSpace(RemainingClientRect,RemainingBorderSpace,
                     ChildSizing.LeftRightSpacing,ChildSizing.TopBottomSpacing,
                     ChildSizing.LeftRightSpacing,ChildSizing.TopBottomSpacing);
      //DebugLn('[TWinControl.AlignControls] ',Name,':',Classname,' ',Left,',',Top,',',Width,',',Height,' ClientRect=',RemainingClientRect.Left,',',RemainingClientRect.Top,',',RemainingClientRect.Right,',',RemainingClientRect.Bottom);
      AlignList := TFPList.Create;
      try
        // Auto aligning/anchoring can be very interdependent.
        // In worst case the n-2 depends on the n-1, the n-3 depends on n-2
        // and so forth. This is allowed, so do up to n loop step.
        // Do not more, to avoid endless loops, if there are circle
        // dependencies.
        MaxTries:=ControlCount;
        {$IFDEF CHECK_POSITION}inc(MaxTries);{$ENDIF}
        for i:=1 to MaxTries do begin
          // align and anchor child controls
          BoundsMutated:=false;
          OldRemainingClientRect:=RemainingClientRect;
          OldRemainingBorderSpace:=RemainingBorderSpace;
          DoAlign(alTop);
          DoAlign(alBottom);
          DoAlign(alLeft);
          DoAlign(alRight);
          DoAlign(alClient);
          DoAlign(alCustom);
          DoAlign(alNone);
          DoAlignNotAligned;
          if not BoundsMutated then break;
          if (i=ControlCount+1) then begin
            DebugLn(['TWinControl.AlignControls ENDLESS LOOP STOPPED ',DbgSName(Self),' i=',i]);
            if LastBoundsMutated<>nil then
              DebugLn(['TWinControl.AlignControls LAST CHANGED: ',DbgSName(LastBoundsMutated),' Old=',dbgs(LastBoundsMutatedOld),' Now=',dbgs(LastBoundsMutated.BoundsRect)]);
          end;
          // update again
          RemainingClientRect:=OldRemainingClientRect;
          RemainingBorderSpace:=OldRemainingBorderSpace;
        end;
      finally
        AlignList.Free;
      end;
    end;
    ControlsAligned;
  finally
    Exclude(FWinControlFlags,wcfAligningControls);
  end;
  // let childs autosize themselves
  for i:=0 to ControlCount-1 do begin
    ChildControl:=Controls[i];
    if cfAutoSizeNeeded in ChildControl.FControlFlags then begin
      //DebugLn(['TWinControl.AlignControls AdjustSize: ',DbgSName(ChildControl),' ',(not ChildControl.AutoSizeCanStart),' ',ChildControl.AutoSizeDelayed]);
      ChildControl.AdjustSize;
    end;
  end;
end;

function TWinControl.DoAlignChildControls(TheAlign: TAlign; AControl: TControl;
  AControlList: TFPList; var ARect: TRect): Boolean;
begin
  Result:=false;
end;

procedure TWinControl.DoChildSizingChange(Sender: TObject);
begin
  //debugln('TWinControl.DoChildSizingChange ',DbgSName(Self));
  if ControlCount=0 then exit;
  InvalidatePreferredSize;
  ReAlign;
end;

procedure TWinControl.ResizeDelayedAutoSizeChildren;
var
  i: Integer;
  Child: TControl;
  AWinControl: TWinControl;
begin
  if ControlCount=0 then exit;
  //DebugLn(['TWinControl.ResizeDelayedAutoSizeChildren START ',DbgSName(Self),' Visible=',Visible]);
  if AutoSizeDelayed then exit;
  //DebugLn(['TWinControl.ResizeDelayedAutoSizeChildren RUN ',DbgSName(Self)]);
  DisableAlign;
  try
    for i:=0 to ControlCount-1 do begin
      Child:=Controls[i];
      if Child.AutoSizeDelayed then continue;

      if cfRequestAlignNeeded in Child.FControlFlags then
        Child.RequestAlign;
      if cfAutoSizeNeeded in Child.FControlFlags then
        Child.AdjustSize;

      if Child is TWinControl then begin
        AWinControl:=TWinControl(Child);
        AWinControl.ResizeDelayedAutoSizeChildren;
        if wcfReAlignNeeded in AWinControl.FWinControlFlags then
          AWinControl.ReAlign;
      end;
    end;
  finally
    EnableAlign;
  end;
end;

{-------------------------------------------------------------------------------
  procedure TWinControl.DoAutoSize;

  Shrink or enlarge to fit childs.
-------------------------------------------------------------------------------}
procedure TWinControl.DoAutoSize;
var
  HasVisibleChilds: boolean;
  
  function WidthAnchored(CurAnchors: TAnchors): boolean;
  begin
    Result:=(CurAnchors*[akLeft,akRight]=[akLeft,akRight]);
  end;
  
  function WidthDependsOnChilds: boolean;
  begin
    Result:=false;
  end;
  
  function WidthDependsOnParent: boolean;
  begin
    if Parent=nil then
      Result:=false
    else
      Result:=(Parent.ChildSizing.Layout<>cclNone);
  end;

  function HeightAnchored(CurAnchors: TAnchors): boolean;
  begin
    Result:=(CurAnchors*[akTop,akBottom]=[akTop,akBottom]);
  end;

  function HeightDependsOnChilds: boolean;
  begin
    Result:=false;
  end;

  function HeightDependsOnParent: boolean;
  begin
    if Parent=nil then
      Result:=false
    else
      Result:=Parent.ChildSizing.Layout<>cclNone;
  end;
  
  procedure GetMoveDiffForNonAlignedChilds(const CurClientRect: TRect;
    out dx, dy: integer);
  // how much can non-aligned-childs be moved up and left
  // non-aligned-childs: no fixed anchoring or autosizing,
  //      (Align=alNone, visible, AnchorSide[].Control=nil)
  // borderspacing is used
  // e.g. dx=10 means all non-align-childs should be moved 10 pixels to the left
  var
    i: Integer;
    Child: TControl;
    p: Integer;
    MinSpaceLeft: LongInt;
    MinSpaceTop: LongInt;
  begin
    if ChildSizing.Layout<>cclNone then begin
      dx:=0;
      dy:=0;
      exit;
    end;
    dx:=High(integer);
    dy:=High(integer);
    MinSpaceLeft:=ChildSizing.LeftRightSpacing;
    MinSpaceTop:=ChildSizing.TopBottomSpacing;
    for i:=0 to ControlCount-1 do begin
      Child:=Controls[i];
      if Child.Align<>alNone then continue;
      if not Child.IsControlVisible then continue;
      if (akLeft in Child.Anchors)
      and (Child.AnchorSide[akLeft].Control=nil) then begin
        // the left side of this control is only defined by designer coords
        p:=Child.Left
           -Max(Child.BorderSpacing.GetSpace(akLeft),MinSpaceLeft)
           -CurClientRect.Left;
        if dx>p then dx:=p;
      end;
      if (akTop in Child.Anchors)
      and (Child.AnchorSide[akTop].Control=nil) then begin
        // the top side of this control is only defined by designer coords
        p:=Child.Top
           -Max(Child.BorderSpacing.GetSpace(akTop),MinSpaceTop)
           -CurClientRect.Top;
        if dy>p then dy:=p;
      end;
    end;
    if dx=High(integer) then
      dx:=0;
    if dy=High(integer) then
      dy:=0;
  end;
  
var
  I: Integer;
  AControl: TControl;
  PreferredWidth: LongInt;
  PreferredHeight: LongInt;
  CurClientRect: TRect;
  WidthIsFixed: boolean;
  HeightIsFixed: boolean;
  NewLeft: LongInt;
  NewTop: LongInt;
  CurAnchors: TAnchors;
  dx: Integer;
  dy: Integer;
  NewChildBounds: TRect;
  OldChildBounds: TRect;
begin
  {$IFDEF VerboseAutoSize}
  debugln('TWinControl.DoAutoSize ',DbgSName(Self));
  {$ENDIF}
  if (not AutoSizeCanStart) or AutoSizeDelayed then begin
    Include(FControlFlags,cfAutoSizeNeeded);
    exit;
  end;

  DisableAutoSizing;
  DisableAlign;
  try
    // test if resizing is possible
    HasVisibleChilds:=false;
    for i:=0 to ControlCount-1 do
      if Controls[i].IsControlVisible then begin
        HasVisibleChilds:=true;
        break;
      end;

    CurAnchors:=Anchors;
    if Align<>alNone then CurAnchors:=CurAnchors+AnchorAlign[Align];
    WidthIsFixed:=WidthAnchored(CurAnchors)
                 or WidthDependsOnChilds
                 or WidthDependsOnParent;
    HeightIsFixed:=HeightAnchored(CurAnchors)
                 or HeightDependsOnChilds
                 or HeightDependsOnParent;

    // move free childs as much as possible to left and top (all free childs the same)
    if HasVisibleChilds then begin
      CurClientRect:=GetLogicalClientRect;
      AdjustClientRect(CurClientRect);
      // get minimum left, top of non aligned childs
      GetMoveDiffForNonAlignedChilds(CurClientRect,dx,dy);
      //DebugLn(['TWinControl.DoAutoSize ',DbgSName(Self),' ChildsBounds=',dbgs(ChildBounds),' CurClientRect=',dbgs(CurClientRect)]);

      if (dx<>0) or (dy<>0) then begin
        // move all free childs to left and top of client area
        //DebugLn(['TWinControl.DoAutoSize ',DbgSName(Self),' dx=',dbgs(dx),' dy=',dbgs(dy),' CurClientRect=',dbgs(CurClientRect),' CurAnchors=',dbgs(CurAnchors),' IsFixed: w=',WidthIsFixed,' h=',HeightIsFixed]);
        for I := 0 to ControlCount - 1 do begin
          AControl:=Controls[I];
          if not AControl.IsControlVisible then continue;
          if AControl.Align<>alNone then continue;
          //DebugLn(['TWinControl.DoAutoSize BEFORE ',DbgSName(AControl),' ',dbgs(AControl.BoundsRect)]);
          NewChildBounds:=AControl.BoundsRect;
          if (akLeft in AControl.Anchors)
          and (AControl.AnchorSide[akLeft].Control=nil) then begin
            dec(NewChildBounds.Left,dx);
            if not (akRight in AControl.Anchors) then
              dec(NewChildBounds.Right,dx);
          end;
          if (akTop in AControl.Anchors)
          and (AControl.AnchorSide[akTop].Control=nil) then begin
            dec(NewChildBounds.Top,dy);
            if not (akBottom in AControl.Anchors) then
              dec(NewChildBounds.Bottom,dy);
          end;
          // Important: change the BaseBounds too, otherwise the changes will be undone by AlignControls
          OldChildBounds:=AControl.BoundsRect;
          if not CompareRect(@OldChildBounds,@NewChildBounds) then begin
            //DebugLn(['TWinControl.DoAutoSize moving child: ',DbgSName(AControl),' Old=',dbgs(OldChildBounds),' New=',dbgs(NewChildBounds)]);
            AControl.BoundsRect:=NewChildBounds;
            //DebugLn(['TWinControl.DoAutoSize AFTER ',DbgSName(AControl),' ',dbgs(AControl.BoundsRect)]);
          end;
        end;
      end;
    end;

    // autosize control to preferred size
    if (not WidthIsFixed) or (not HeightIsFixed) then begin
      GetPreferredSize(PreferredWidth,PreferredHeight,
                       false,// with constraints
                       true // with theme space
                       );
      //if ControlCount>0 then DebugLn(['TWinControl.DoAutoSize ',DbgSName(Self),' PreferredWidth=',PreferredWidth,' PreferredHeight=',PreferredHeight,' ControlCount=',ControlCount]);
    end else begin
      PreferredWidth:=0;
      PreferredHeight:=0;
    end;
    if WidthIsFixed or (PreferredWidth<=0) then
      PreferredWidth:=Constraints.MinMaxWidth(Width);
    if HeightIsFixed or (PreferredHeight<=0) then
      PreferredHeight:=Constraints.MinMaxHeight(Height);

    // set new size
    {$IFDEF VerboseAutoSize}
    debugln(['DoAutoSize A ',DbgSName(Self),' Cur=',Width,'x',Height,' Prefer=',PreferredWidth,'x',PreferredHeight,' WidgetClass=',WidgetSetClass.ClassName,' Fixed=',WidthIsFixed,'x',HeightIsFixed]);
    {$ENDIF}
    if (PreferredWidth<>Width) or (PreferredHeight<>Height) then begin
      // adjust Left/Top as well to reduce auto sizing overhead
      NewLeft:=Left;
      NewTop:=Top;
      if akRight in CurAnchors then
        inc(NewLeft,Width-PreferredWidth);
      if akBottom in CurAnchors then
        inc(NewTop,Height-PreferredHeight);
      //if CompareText(Name,'NewUnitOkButton')=0 then
        //debugln(['DoAutoSize Resize ',DbgSName(Self),' Old=',dbgs(BoundsRect),' New=',dbgs(Bounds(NewLeft,NewTop,PreferredWidth,PreferredHeight)),' WidthIsFixed=',WidthIsFixed,' HeightIsFixed=',HeightIsFixed,' Align=',dbgs(Align),' Anchors=',dbgs(Anchors)]);
      SetAlignedBounds(NewLeft,NewTop,PreferredWidth,PreferredHeight);
    end;
  finally
    Exclude(FControlFlags,cfAutoSizeNeeded);
    EnableAutoSizing;
    EnableAlign;
  end;
end;

{------------------------------------------------------------------------------
  TWinControl BroadCast
------------------------------------------------------------------------------}
procedure TWinControl.BroadCast(var ToAllMessage);
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
  begin
    Controls[I].WindowProc(TLMessage(ToAllMessage));
    if TLMessage(ToAllMessage).Result <> 0 then Exit;
  end;
end;

procedure TWinControl.NotifyControls(Msg: Word);
var
  ToAllMessage: TLMessage;
begin
  ToAllMessage.Msg := Msg;
  ToAllMessage.WParam := 0;
  ToAllMessage.LParam := 0;
  ToAllMessage.Result := 0;
  Broadcast(ToAllMessage);
end;

procedure TWinControl.DefaultHandler(var AMessage);
begin
  CallDefaultWndHandler(Self,AMessage);
end;


{------------------------------------------------------------------------------
  TWinControl CanFocus
  
  
------------------------------------------------------------------------------}
function TWinControl.CanFocus : Boolean;
var
  Control: TWinControl;
  Form: TCustomForm;
begin
  Result := False;
  //Verify that every parent is enabled and visible before returning true.
  Form := GetParentForm(Self);
  if Form <> nil then
  begin
    Control := Self;
    repeat
      if Control = Form then break;
      // test all except the Form if it is visible and enabled
      if not (Control.IsControlVisible and Control.Enabled) then Exit;
      Control := Control.Parent;
    until false;
    Result := True;
  end;
end;

{------------------------------------------------------------------------------
  TWinControl CreateSubClass
------------------------------------------------------------------------------}
procedure TWinControl.CreateSubClass(var Params: TCreateParams;
  ControlClassName: PChar);
begin
  // TODO: Check if we need this method
end;

{------------------------------------------------------------------------------
  TWinControl DisableAlign
------------------------------------------------------------------------------}
procedure TWinControl.DisableAlign;
begin
  Inc(FAlignLevel);
  //DebugLn(['TWinControl.DisableAlign ',dbgsName(Self),' ',FAlignLevel]);
end;

{-------------------------------------------------------------------------------
  TWinControl DoAdjustClientRectChange

  Asks the interface if clientrect has changed since last AlignControl
  and calls AlignControl(nil) on change.
-------------------------------------------------------------------------------}
procedure TWinControl.DoAdjustClientRectChange;
var r: TRect;
begin
  r:=GetClientRect;
  AdjustClientRect(r);
  //DebugLn(['TWinControl.DoAdjustClientRectChange ',DbgSName(Self),' ',r.Right,',',r.Bottom,' ',CompareRect(@r,@FAdjustClientRectRealized)]);
  if not CompareRect(@r,@FAdjustClientRectRealized) then begin
    // client rect changed since last AlignControl
    {$IFDEF VerboseClientRectBugFix}
    DebugLn('UUU TWinControl.DoAdjustClientRectChange ClientRect changed ',Name,':',ClassName,
      ' Old=',Dbgs(FAdjustClientRectRealized.Right),'x',DbgS(FAdjustClientRectRealized.Bottom),
      ' New=',DbgS(r.Right),'x',DbgS(r.Bottom));
    {$ENDIF}
    FAdjustClientRectRealized:=r;
    ReAlign;
    Resize;
    AdjustSize;
  end;
end;

{-------------------------------------------------------------------------------
  TWinControl DoConstraintsChange
  Params: Sender : TObject

  Call inherited, then send the constraints to the interface
-------------------------------------------------------------------------------}
procedure TWinControl.DoConstraintsChange(Sender : TObject);
begin
  inherited DoConstraintsChange(Sender);
  //debugln('TWinControl.DoConstraintsChange ',DbgSName(Self),' HandleAllocated=',dbgs(HandleAllocated));
  if HandleAllocated then
    TWSWinControlClass(WidgetSetClass).ConstraintsChange(Self);
end;

{-------------------------------------------------------------------------------
  TWinControl InvalidateClientRectCache(WithChildControls: boolean)

  The clientrect is cached. Call this procedure to invalidate the cache, so that
  next time the clientrect is fetched from the interface.
-------------------------------------------------------------------------------}
procedure TWinControl.InvalidateClientRectCache(WithChildControls: boolean);
var
  I: Integer;
begin
  {$IFDEF VerboseClientRectBugFix}
  DebugLn('[TWinControl.InvalidateClientRectCache] ',DbgSName(Self));
  {$ENDIF}
  Include(FWinControlFlags,wcfClientRectNeedsUpdate);

  if WithChildControls then begin
    // invalidate clients too
    if Assigned(FWinControls) then
      for I := 0 to FWinControls.Count - 1 do
        if Assigned(FWinControls.Items[I]) then
          TWinControl(FWinControls.Items[I]).InvalidateClientRectCache(true);
  end;
  InvalidatePreferredSize;
end;

{-------------------------------------------------------------------------------
  TWinControl ClientRectNeedsInterfaceUpdate

  The clientrect is cached. Check if cache is valid.
-------------------------------------------------------------------------------}
function TWinControl.ClientRectNeedsInterfaceUpdate: boolean;
var
  IntfClientRect: TRect;
begin
  if (not HandleAllocated) or (csDestroyingHandle in ControlState)
  or (csDestroying in ComponentState)
  then
    exit(false);
  if wcfClientRectNeedsUpdate in FWinControlFlags then
    exit(true);
  LCLIntf.GetClientRect(Handle,IntfClientRect);
  Result:=(FClientWidth<>IntfClientRect.Right)
       or (FClientHeight<>IntfClientRect.Bottom);
end;

{-------------------------------------------------------------------------------
  TWinControl DoSetBounds
  Params: ALeft, ATop, AWidth, AHeight : integer

  Anticipate the new clientwidth/height and call inherited

  Normally the clientwidth/clientheight is adjusted automatically by the
  interface. But it is up to interface when this will be done. The gtk for
  example just puts resize requests into a queue. The LCL would resize the
  childs just after this procedure due to the clientrect. On complex forms with
  lots of nested controls, this would result in thousands of resizes.
  Changing the clientrect in the LCL to the most probable size reduces
  unneccessary resizes.
-------------------------------------------------------------------------------}
procedure TWinControl.DoSetBounds(ALeft, ATop, AWidth, AHeight : integer);
begin
  {$IFDEF VerboseClientRectBugFix}
  DbgOut('[TWinControl.DoSetBounds] ',Name,':',ClassName,
  ' OldHeight=',DbgS(FHeight),' NewHeight=',DbgS(AHeight));
  {$ENDIF}
  InvalidateClientRectCache(false);
  inherited DoSetBounds(ALeft,ATop,AWidth,AHeight);
end;

{------------------------------------------------------------------------------
  TWinControl EnableAlign
------------------------------------------------------------------------------}
procedure TWinControl.EnableAlign;
begin
  Dec(FAlignLevel);
  //DebugLn(['TWinControl.EnableAlign ',dbgsName(Self),' ',FAlignLevel]);
  if FAlignLevel = 0 then begin
    if (csAlignmentNeeded in ControlState)
    or (wcfReAlignNeeded in FWinControlFlags) then
      ReAlign;
  end;
end;

procedure TWinControl.WriteLayoutDebugReport(const Prefix: string);
var
  i: Integer;
begin
  inherited WriteLayoutDebugReport(Prefix);
  for i:=0 to ControlCount-1 do
    Controls[i].WriteLayoutDebugReport(Prefix+'  ');
end;

{------------------------------------------------------------------------------
  TWinControl.CanTab
------------------------------------------------------------------------------}
function TWinControl.CanTab: Boolean;
begin
  Result := CanFocus and TWSWinControlClass(WidgetSetClass).CanFocus(Self);
end;

function TWinControl.DoDragMsg(ADragMessage: TDragMessage; APosition: TPoint; ADragObject: TDragObject; ATarget: TControl; ADocking: Boolean):LRESULT;
var
  TargetControl: TControl;
begin
  case ADragMessage of
    dmFindTarget:
      begin
        {$IFDEF VerboseDrag}
        DebugLn('TWinControl.DoDragMsg dmFindTarget ',Name,':',ClassName,' Start DragMsg.DragRec^.Pos=',IntToStr(APosition.X),',',IntToStr(APosition.Y));
        {$ENDIF}
        TargetControl := ControlatPos(ScreentoClient(APosition),
                                      [capfAllowWinControls,capfRecursive]);
        if TargetControl = nil then TargetControl := Self;
        {$IFDEF VerboseDrag}
        DebugLn('TWinControl.DoDragMsg dmFindTarget ',Name,':',ClassName,' end Result=',TargetControl.Name,':',TargetControl.ClassName);
        {$ENDIF}
        Result := LRESULT(TargetControl);
      end;
    else
      Result := inherited;
  end;
end;

{------------------------------------------------------------------------------
  TWinControl GetChildren
------------------------------------------------------------------------------}
procedure TWinControl.GetChildren(Proc: TGetChildProc; Root : TComponent);
var
  I : Integer;
  Control : TControl;
begin
  for I := 0 to ControlCount-1 do
  begin
    Control := Controls[i];
    if Control.Owner = Root then Proc(Control);
  end;
end;

{-------------------------------------------------------------------------------
  function TWinControl.ChildClassAllowed(ChildClass: TClass): boolean;

  Allow TControl as child.
-------------------------------------------------------------------------------}
function TWinControl.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  Result:=(ChildClass<>nil) and ChildClass.InheritsFrom(TControl);
end;

{-------------------------------------------------------------------------------
  TWinControl GetClientOrigin
  Result:  TPoint

  returns the screen coordinate of the topleft coordinate 0,0 of the client area
  Note that this value is the position as stored in the interface and is not
  always in sync with the LCL. When a control is moved, the LCL sets the bounds
  to the wanted position and sends a move message to the interface. It is up to
  the interface to handle moves instantly or queued.
-------------------------------------------------------------------------------}
function TWinControl.GetClientOrigin: TPoint;
var
  AControl: TWinControl;
begin
  Result.X := 0;
  Result.Y := 0;
//DebugLn('[TWinControl.GetClientOrigin] ',Name,':',ClassName,'  ',Handle);
  if HandleAllocated then begin
    // get the interface idea where the client area is on the screen
    LCLIntf.ClientToScreen(Handle,Result);
    // adjust the result by all bounds, that are not yet sent to the interface
    AControl:=Self;
    repeat
      inc(Result.X,AControl.Left-AControl.FBoundsRealized.Left);
      inc(Result.Y,AControl.Top-AControl.FBoundsRealized.Top);
      AControl:=AControl.Parent;
    until AControl=nil;
  end else
  if Parent <> nil then
    Result:=inherited GetClientOrigin;
  Assert(False, Format('Trace:[TWinControl.GetClientOrigin] %s --> (%d, %d)',  [Classname, Result.X, Result.Y]));
end;

{-------------------------------------------------------------------------------
  TWinControl GetClientRect
  Result:  TRect

  returns the client area. Starting at 0,0.
-------------------------------------------------------------------------------}
function TWinControl.GetClientRect: TRect;

  procedure StoreClientRect(NewClientRect: TRect);
  var
    ClientSizeChanged: boolean;
    i: Integer;
  begin
    if wcfClientRectNeedsUpdate in FWinControlFlags then begin
      ClientSizeChanged:=(FClientWidth<>NewClientRect.Right)
                      or (FClientHeight<>NewClientRect.Bottom);
      FClientWidth:=NewClientRect.Right;
      FClientHeight:=NewClientRect.Bottom;
      {$IFDEF VerboseSizeMsg}
      DebugLn(['StoreClientRect ',Name,':',ClassName,' ',FClientWidth,',',FClientHeight,' HandleAllocated=',HandleAllocated]);
      {$ENDIF}
      if ClientSizeChanged then begin
        for i:=0 to ControlCount-1 do
          Controls[i].fLastAlignedBoundsTried:=0;
      end;
      Exclude(FWinControlFlags,wcfClientRectNeedsUpdate);
    end;
  end;

var
  InterfaceWidth, InterfaceHeight: integer;
begin
  if wcfClientRectNeedsUpdate in FWinControlFlags then begin
    if TWSWinControlClass(WidgetSetClass).GetDefaultClientRect(Self,
      Left, Top, Width, Height, Result)
    then begin
      // the LCL interface provided a ClientRect
    end
    else if HandleAllocated then begin
      // update clientrect from interface
      LCLIntf.GetClientRect(Handle, Result);
      // the LCL is not always in sync with the interface
      // -> adjust client rect based on LCL bounds
      // for example: if the Width in LCL differ from the Width of the Interface
      // object, then adjust the clientwidth accordingly
      // this often anticipates later LM_SIZE messages from the interface
      // and reduces resizes
      LCLIntf.GetWindowSize(Handle, InterfaceWidth, InterfaceHeight);
      //debugln('TWinControl.GetClientRect ',DbgSName(Self),' Interface=',dbgs(InterfaceWidth),',',dbgs(InterfaceHeight),' Result=',dbgs(Result),' Bounds=',dbgs(BoundsRect));
      Result.Right:=Width-(InterfaceWidth-Result.Right);
      Result.Bottom:=Height-(InterfaceHeight-Result.Bottom);
    end else begin
      // no handle and no interface help => use defaults
      Result:=inherited GetClientRect;
      if csLoading in ComponentState then begin
        if cfClientWidthLoaded in FControlFlags then
          Result.Right:=FLoadedClientSize.X;
        if cfClientHeightLoaded in FControlFlags then
          Result.Bottom:=FLoadedClientSize.Y;
      end;
    end;
    Result.Right:=Max(Result.Left,Result.Right);
    Result.Bottom:=Max(Result.Top,Result.Bottom);
    StoreClientRect(Result);

    {r:=inherited GetClientRect;
    if (r.Left<>Result.Left)
    or (r.Top<>Result.Top)
    or (r.Right<>Result.Right)
    or (r.Bottom<>Result.Bottom) then begin
      //DebugLn(' TWinControl.GetClientRect ',Name,':',ClassName,
      //  ' Old=',r.Left,',',r.Top,',',r.Right,',',r.Bottom,
      //  ' New=',Result.Left,',',Result.Top,',',Result.Right,',',Result.Bottom
      //  );
    end;}

  end else begin
    Result:=Rect(0,0,FClientWidth,FClientHeight);
  end;
end;

{-------------------------------------------------------------------------------
  TWinControl GetControlOrigin
  Result:  TPoint

  Returns the screen coordinate of the topleft coordinate 0,0 of the control
  area. (The topleft pixel of the control on the screen)
  Note that this value is the position as stored in the interface and is not
  always in sync with the LCL. When a control is moved, the LCL sets the bounds
  to the wanted position and sends a move message to the interface. It is up to
  the interface to handle moves instantly or queued.
-------------------------------------------------------------------------------}
function TWinControl.GetControlOrigin: TPoint;
var
  AControl: TWinControl;
  IntfBounds: TRect;
begin
  if HandleAllocated then begin
    // get the interface idea where the client area is on the screen
    LCLIntf.GetWindowRect(Handle,IntfBounds);
    Result.X := IntfBounds.Left;
    Result.Y := IntfBounds.Top;
    // adjust the result by all bounds, that are not yet sent to the interface
    AControl:=Self;
    repeat
      inc(Result.X,AControl.Left-AControl.FBoundsRealized.Left);
      inc(Result.Y,AControl.Top-AControl.FBoundsRealized.Top);
      AControl:=AControl.Parent;
    until AControl=nil;
  end else
    Result:=inherited GetControlOrigin;
end;

{------------------------------------------------------------------------------
  function TWinControl.GetChildsRect(Scrolled: boolean): TRect;

  Returns the Client rectangle relative to the controls left, top.
  If Scrolled is true, the rectangle is moved by the current scrolling values
  (for an example see TScrollingWincontrol).
------------------------------------------------------------------------------}
function TWinControl.GetChildsRect(Scrolled: boolean): TRect;
var
  ScrolledOffset: TPoint;
begin
  if HandleAllocated then begin
    LCLIntf.GetClientBounds(Handle,Result);
    if Scrolled then begin
      ScrolledOffset:=GetClientScrollOffset;
      inc(Result.Left,ScrolledOffset.X);
      inc(Result.Top,ScrolledOffset.Y);
      inc(Result.Right,ScrolledOffset.X);
      inc(Result.Bottom,ScrolledOffset.Y);
    end;
  end else
    Result:=inherited GetChildsRect(Scrolled);
end;

{------------------------------------------------------------------------------
  TWinControl SetBorderStyle
------------------------------------------------------------------------------}
procedure TWinControl.SetBorderStyle(NewStyle: TBorderStyle);
begin
  FBorderStyle := NewStyle;
  if HandleAllocated then
    TWSWinControlClass(WidgetSetClass).SetBorderStyle(Self, NewStyle);
end;

{------------------------------------------------------------------------------
  TWinControl SetBorderWidth
------------------------------------------------------------------------------}
procedure TWinControl.SetBorderWidth(value : TBorderWidth);
begin
  if FBorderWidth = Value then exit;
  FBorderWidth := Value;
  Invalidate;
  InvalidatePreferredSize;
  AdjustSize;
end;

{------------------------------------------------------------------------------
  TWinControl.SetChildZPosition

  Set the position of the child control in the FControls (in case of a TControl)
  or in the FWinControls (in all other cases) list.

  Notes:
  * The FControls are always below the FWinControls.
  * FControls and FWinControls can be nil
------------------------------------------------------------------------------}
procedure TWinControl.SetChildZPosition(const AChild: TControl;
  const APosition: Integer);
var
  list: TFPList;
  idx, NewPos: Integer;
  IsWinControl: boolean;
begin
  if AChild = nil
  then begin
    DebugLn('WARNING: TWinControl.SetChildZPosition: Child = nil');
    Exit;
  end;

  IsWinControl :=  AChild is TWincontrol;
  
  if IsWinControl
  then list := FWinControls
  else list := FControls;
  
  if list = nil
  then idx := -1
  else idx := list.IndexOf(AChild);

  if idx = -1
  then begin
    DebugLn('WARNING: TWinControl.SetChildZPosition: Unknown child');
    Exit;
  end;
  
  if IsWinControl and (FControls <> nil)
  then NewPos := APosition - FControls.Count
  else NewPos := APosition;
  
  if NewPos < 0
  then NewPos := 0
  else if NewPos >= list.Count
  then NewPos := list.Count - 1;
  
  if NewPos = idx then Exit;
  
  list.Move(idx, NewPos);
  
  if IsWinControl
  then begin
    if HandleAllocated and TWinControl(AChild).HandleAllocated
    then TWSWinControlClass(WidgetSetClass).SetChildZPosition(Self,
                            TWinControl(AChild), idx, NewPos, list);
  end
  else begin
    AChild.InvalidateControl(AChild.IsVisible, True, True);
  end;
end;

{------------------------------------------------------------------------------
       TWinControl.SetTabOrder
------------------------------------------------------------------------------}
procedure TWinControl.SetTabOrder(NewTabOrder: TTabOrder);
begin
  if csLoading in ComponentState then
    FTabOrder := NewTabOrder
  else
    UpdateTabOrder(NewTabOrder);
end;

procedure TWinControl.SetTabStop(NewTabStop: Boolean);
begin
  if FTabStop = NewTabStop then
    exit;

  FTabStop := NewTabStop;
  UpdateTabOrder(FTabOrder);
end;

{------------------------------------------------------------------------------
  TControl UpdateTabOrder
------------------------------------------------------------------------------}
procedure TWinControl.UpdateTabOrder(NewTabOrder: TTabOrder);
var
  Count: Integer;
begin
  if FParent <> nil then
  begin
    FTabOrder := GetTabOrder;
    Count := ListCount(FParent.FTabList);
    if NewTabOrder < 0 then NewTabOrder := Count;
    if FTabOrder = -1 then Inc(Count);
    if NewTabOrder >= Count then NewTabOrder := Count - 1;
    if NewTabOrder <> FTabOrder then
    begin
      if FTabOrder <> - 1 then
        ListDelete(FParent.FTabList,FTabOrder);
      if NewTabOrder <> -1 then
      begin
        ListInsert(FParent.FTabList,NewTabOrder,Self);
        FTabOrder := NewTabOrder;
      end;
    end;
  end;
end;

{-------------------------------------------------------------------------------
  procedure TWinControl.SendMoveSizeMessages(SizeChanged, PosChanged: boolean);
  
  Send Move and Size messages through the LCL message paths. This simulates the
  VCL behaviour and has no real effect.
-------------------------------------------------------------------------------}
procedure TWinControl.SendMoveSizeMessages(SizeChanged, PosChanged: boolean);
var
  SizeMsg : TLMSize;
  MoveMsg : TLMMove;
  //Flags: UINT;
begin
  if (not HandleAllocated)
  or ((not SizeChanged) and (not PosChanged)) then exit;

  Perform(LM_WindowposChanged, 0, 0);

  if SizeChanged then begin
    with SizeMsg do begin
      Msg:= LM_SIZE;
      SizeType:= 6; // force realign
      Width:= FWidth;
      Height:= FHeight;
      {$IFDEF CHECK_POSITION}
      if CheckPosition(Self) then
      DebugLn(' [TControl.SendMoveSizeMessages] ',Name,':',ClassName,' SizeMsg Width=',DbgS(Width),' Height=',DbgS(Height));
      {$ENDIF}
    end;
    WindowProc(TLMessage(SizeMsg));
  end;
  
  if PosChanged then begin
    with MoveMsg do begin
      Msg:= LM_MOVE;
      MoveType:= 1;
      XPos:= FLeft;
      YPos:= FTop;
      {$IFDEF CHECK_POSITION}
      if CheckPosition(Self) then
      DebugLn(' [TControl.SendMoveSizeMessages] ',Name,':',ClassName,' MoveMsg XPos=',Dbgs(XPos),' YPos=',Dbgs(YPos));
      {$ENDIF}
    end;
    WindowProc(TLMessage(MoveMsg));
  end;
end;

{------------------------------------------------------------------------------
  TWinControl UpdateShowing
------------------------------------------------------------------------------}
procedure TWinControl.UpdateShowing;
var
  bShow: Boolean;
  n: Integer;
  ok: boolean;
begin
  bShow := HandleObjectShouldBeVisible;

  if bShow then begin
    if not HandleAllocated then CreateHandle;
    if FWinControls <> nil
    then begin
      for n := 0 to FWinControls.Count - 1 do
        TWinControl(FWinControls[n]).UpdateShowing;
    end;
  end;

  if not HandleAllocated then Exit;

  //DebugLn('TWinControl.UpdateShowing A ',Name,':',ClassName,' FShowing=',dbgs(FShowing),' bShow=',dbgs(bShow));
  if FShowing = bShow then Exit;
  
  FShowing := bShow;
  ok := false;
  try
    Perform(CM_SHOWINGCHANGED, 0, 0);
    ok := true;
  finally
    if not ok then
      FShowing := not bShow;
  end;
  
  //DebugLn(['TWinControl.UpdateShowing ',DbgSName(Self),' FShowing=',FShowing,' AutoSizeDelayed=',AutoSizeDelayed]);
  if FShowing then
  begin
    DisableAlign;
    try
      ResizeDelayedAutoSizeChildren;
      AdjustSize;
    finally
      EnableAlign;
    end;
  end;
end;

procedure TWinControl.Update;
begin
  if HandleAllocated then UpdateWindow(Handle);
end;

{------------------------------------------------------------------------------
  TWinControl Focused
------------------------------------------------------------------------------}
function TWinControl.Focused: Boolean;
begin
  Result := CanTab and (HandleAllocated and (FindOwnerControl(GetFocus)=Self));
end;

function TWinControl.PerformTab(ForwardTab: boolean): boolean;

  function GetHighestParent(TopControl : TControl) : TWinControl;
  begin
    Result := nil;
    If TopControl = nil then exit;
    If (TopControl.Parent=nil) then begin
      if TopControl is TWinControl then
        Result := TWinControl(TopControl)
    end else
      Result := GetHighestParent(TopControl.Parent);
  end;

var
  I : Integer;
  List : TFPList;
  FirstFocus, OldFocus, NewFocus : TWinControl;
  TopLevel : TWinControl;
begin
  NewFocus := nil;
  OldFocus := nil;
  TopLevel := GetHighestParent(Self);
  If TopLevel = nil then
    exit;
  try
    List := TFPList.Create;
    TopLevel.GetTabOrderList(List);
    FirstFocus := nil;
    for I := 0 to List.Count - 1 do
      If List[I] <> nil then begin
        If I = 0 then
          FirstFocus := TWinControl(List[I]);
        If TWinControl(List[I]).Focused then begin
          OldFocus := TWinControl(List[I]);
          Break;
        end;
      end;
  finally
    List.Free;
  end;

  if OldFocus<>nil then
    NewFocus := TopLevel.FindNextControl(OldFocus,ForwardTab,True,False);
  //DebugLn('TControl.PerformTab A ',DbgSName(Self),' NewFocus=',DbgSName(NewFocus),' OldFocus=',DbgSName(OldFocus));

  if (NewFocus = nil) then NewFocus:=FirstFocus;
  if NewFocus = OldFocus then begin
    Result := True;
    exit;
  end;
  if NewFocus<>nil then begin
    NewFocus.SetFocus;
    Result := NewFocus.Focused;
  end else
    Result:=true;
end;

{------------------------------------------------------------------------------
  TWinControl FindChildControl
  
  Find next control (Tab control or Child control).
  Like VCL the CurControl parameter is ignored.
------------------------------------------------------------------------------}
procedure TWinControl.SelectNext(CurControl: TWinControl; GoForward,
  CheckTabStop: Boolean);
begin
  CurControl := FindNextControl(CurControl, GoForward,
                                CheckTabStop, not CheckTabStop);
  if CurControl <> nil then CurControl.SetFocus;
end;

procedure TWinControl.SetTempCursor(Value: TCursor);
begin
  if not HandleAllocated then exit;
  TWSWinControlClass(WidgetSetClass).SetCursor(Self, Screen.Cursors[Value]);
end;

{------------------------------------------------------------------------------
  TWinControl FindChildControl
------------------------------------------------------------------------------}
function TWinControl.FindChildControl(const ControlName: string): TControl;
var
  I: Integer;
begin
  Result := nil;
  if FWinControls <> nil then
    for I := 0 to FWinControls.Count - 1 do
      if CompareText(TWinControl(FWinControls[I]).Name, ControlName) = 0 then
      begin
        Result := TControl(FWinControls[I]);
        Exit;
      end;
end;

procedure TWinControl.FlipChildren(AllLevels: Boolean);
var
  i: Integer;
  FlipControls: TFPList;
  CurControl: TControl;
begin
  if ControlCount = 0 then exit;
  FlipControls := TFPList.Create;
  
  DisableAlign;
  try
    // Collect all controls with Align Right and Left
    for i := 0 to ControlCount - 1 do begin
      CurControl:=Controls[i];
      if CurControl.Align in [alLeft,alRight] then
        FlipControls.Add(CurControl);
    end;
    // flip the rest
    DoFlipChildren;
    // reverse Right and Left alignments
    while FlipControls.Count > 0 do begin
      CurControl:=TControl(FlipControls[FlipControls.Count-1]);
      if CurControl.Align=alLeft then
        CurControl.Align:=alRight
      else if CurControl.Align=alRight then
        CurControl.Align:=alLeft;
      FlipControls.Delete(FlipControls.Count - 1);
    end;
  finally
    FlipControls.Free;
    EnableAlign;
  end;
  
  // flip recursively
  if AllLevels then begin
    for i := 0 to ControlCount - 1 do begin
      CurControl:=Controls[i];
      if CurControl is TWinControl then
        TWinControl(CurControl).FlipChildren(true);
    end;
  end;
end;

{------------------------------------------------------------------------------}
{  TWinControl FindNextControl                                                 }
{------------------------------------------------------------------------------}
function TWinControl.FindNextControl(CurrentControl: TWinControl;
  GoForward, CheckTabStop, CheckParent: boolean): TWinControl;
var
  List : TFPList;
  Next : TWinControl;
  I, J : Longint;
begin
  Try
    Result := nil;
    List := TFPList.Create;
    GetTabOrderList(List);
    //for i:=0 to List.Count-1 do begin
    //  debugln('TWinControl.FindNextControl TabOrderList ',dbgs(i),' ',DbgSName(TObject(List[i])));
    //end;
    If List.Count > 0 then begin
      J := List.IndexOf(CurrentControl);
      if J<0 then begin
        if GoForward then
          J := List.Count - 1
        else
          J := 0;
      end;
      //DebugLn('TWinControl.FindNextControl A ',DbgSName(CurrentControl),' ',dbgs(J),
      //  ' GoForward='+dbgs(GoForward)+' CheckTabStop='+dbgs(CheckTabStop)+' CheckParent='+dbgs(CheckParent));
      I := J;
      Repeat
        If GoForward then begin
          Inc(I);
          if I>=List.Count then
            I:=0;
        end else begin
          Dec(I);
          if I<0 then
            I:=List.Count-1;
        end;
        if I=J then exit;

        Next := TWinControl(List[I]);
        //DebugLn('TWinControl.FindNextControl B ',Next.Name,' ',dbgs(I),
        //  +' ChckTabStop='+dbgs(CheckTabStop)+' TabStop='+dbgs(Next.TabStop)
        //  +' ChckParent='+dbgs(CheckParent)+' Parent=Self='+dbgs(Next.Parent = Self)
        //  +' Enabled='+dbgs(Next.Enabled)
        //  +' TestTab='+dbgs(((Not CheckTabStop) or Next.TabStop))
        //  +' TestPar='+dbgs(((not CheckParent) or (Next.Parent = Self)))
        //  +' TestEnVi='+dbgs(Next.Enabled and Next.IsVisible)
        //  );
        If (((not CheckTabStop) or Next.TabStop)
        and ((not CheckParent) or (Next.Parent = Self)))
        and (Next.Enabled and Next.IsVisible) then
          Result := Next;
      until (Result <> nil);
      //DebugLn('TWinControl.FindNextControl END ',DbgSName(Result),' I=',dbgs(I));
    end;
  finally
    List.Free;
  end;
end;

procedure TWinControl.SelectFirst;
var
  Form : TCustomForm;
  Control : TWinControl;
begin
  Form := GetParentForm(Self);
  if Form <> nil then begin
    Control := FindNextControl(nil, true, true, false);
    if Control = nil then
      Control := FindNextControl(nil, true, false, false);
    if Control <> nil then
      Form.ActiveControl := Control;
  end;
end;

procedure TWinControl.FixupTabList;
var
  Count, I, J: Integer;
  List: TFPList;
  Control: TWinControl;
begin
  if FWinControls <> nil then
  begin
    List := TFPList.Create;
    try
      Count := FWinControls.Count;
      List.Count := Count;
      for I := 0 to Count - 1 do
      begin
        Control := TWinControl(FWinControls[I]);
        J := Control.FTabOrder;
        if (J >= 0) and (J < Count) then List[J] := Control;
      end;
      for I := 0 to Count - 1 do
      begin
        Control := TWinControl(List[I]);
        if Control <> nil then Control.UpdateTabOrder(TTabOrder(I));
      end;
    finally
      List.Free;
    end;
  end;
end;

{------------------------------------------------------------------------------
  TWinControl GetTabOrderList
------------------------------------------------------------------------------}
procedure TWinControl.GetTabOrderList(List: TFPList);
var
  I : Integer;
  lWinControl : TWinControl;
begin
  if FTabList <> nil then
    for I := 0 to FTabList.Count - 1 do begin
      lWinControl := TWinControl(FTabList[I]);
      if lWinControl.CanTab and lWinControl.TabStop then
        List.Add(lWinControl);
      lWinControl.GetTabOrderList(List);
    end;
end;

{------------------------------------------------------------------------------
  TWinControl IsControlMouseMsg
------------------------------------------------------------------------------}
function TWinControl.IsControlMouseMsg(var TheMessage: TLMMouse) : Boolean;
var
  Control : TControl;
  ScrolledOffset,
  P : TPoint;
  ClientBounds: TRect;
begin
  { CaptureControl = nil means that widgetset has captured input, but it does
    not know anything about TControl controls }
  if (FindOwnerControl(GetCapture) = Self) and (CaptureControl <> nil)
  then begin
    Control := nil;
    //DebugLn(['TWinControl.IsControlMouseMsg A ', DbgSName(CaptureControl), ', ',DbgSName(CaptureControl.Parent),', Self: ', DbgSName(Self)]);
    if (CaptureControl.Parent = Self)
    then Control := CaptureControl;
  end
  else begin
    // do query wincontrol childs, in case they overlap
    Control := ControlAtPos(SmallPointToPoint(TheMessage.Pos),
                            [capfAllowWinControls]);
    if Control is TWinControl then begin
      // there is a TWinControl child at this position
      // TWinControl childs get their own messages
      // => ignore here
      Control:=nil;
    end;
  end;

  //DebugLn(['TWinControl.IsControlMouseMsg B ',DbgSName(Self),' Control=',DbgSName(Control),' Msg=',TheMessage.Msg]);
  Result := False;
  if Control <> nil then
  begin
    // map mouse coordinates to control
    ScrolledOffset:=GetClientScrollOffset;

    P.X := TheMessage.XPos - Control.Left + ScrolledOffset.X;
    P.Y := TheMessage.YPos - Control.Top + ScrolledOffset.Y;
    if (Control is TWinControl) and TWinControl(Control).HandleAllocated then
    begin
      // map coordinates to client area of control
      LCLIntf.GetClientBounds(TWinControl(Control).Handle,ClientBounds);
      dec(P.X,ClientBounds.Left);
      dec(P.Y,ClientBounds.Top);
      {$IFDEF VerboseMouseBugfix}
      DebugLn('TWinControl.IsControlMouseMsg ',Name,' -> ',Control.Name,
      ' MsgPos=',TheMessage.Pos.X,',',TheMessage.Pos.Y,
      ' Control=',Control.Left,',',Control.Top,
      ' ClientBounds=',ClientBounds.Left,',',ClientBounds.Top,
      ' Scrolled=',GetClientScrollOffset.X,',',GetClientScrollOffset.Y,
      ' P=',P.X,',',P.Y
      );
      {$ENDIF}
    end;
    Control.Perform(TheMessage.Msg, WParam(TheMessage.Keys),
                    LParam(Integer(PointToSmallPoint(P))));
    Result := True;
  end;
end;

procedure TWinControl.FontChanged(Sender: TObject);
var
  i: Integer;
begin
  ParentFont := False;
  if HandleAllocated and ([csLoading, csDestroying]*ComponentState=[]) then
  begin
    TWSWinControlClass(WidgetSetClass).SetFont(Self, Font);
    Exclude(FWinControlFlags,wcfFontChanged);
    Invalidate;
  end else
    Include(FWinControlFlags,wcfFontChanged);
  for i := 0 to ControlCount - 1 do
    Controls[i].ParentFontChanged;
end;

procedure TWinControl.SetColor(Value: TColor);
begin
  if Value=Color then exit;
  inherited SetColor(Value);
  if FBrush <> nil then
    FBrush.Color := Color;
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
  begin
    TWSWinControlClass(WidgetSetClass).SetColor(Self);
    Exclude(FWinControlFlags,wcfColorChanged);
  end else
    Include(FWinControlFlags,wcfColorChanged);
  NotifyControls(CM_PARENTCOLORCHANGED);
end;

procedure TWinControl.PaintHandler(var TheMessage: TLMPaint);

  function ControlMustBeClipped(AControl: TControl): boolean;
  begin
    Result := (csOpaque in AControl.ControlStyle) and AControl.IsVisible;
  end;

var
  I, Clip, SaveIndex: Integer;
  DC: HDC;
  PS: TPaintStruct; //defined in LCLIntf.pp
  ControlsNeedsClipping: boolean;
  CurControl: TControl;
begin
  //DebugLn('[TWinControl.PaintHandler] ',Name,':',ClassName,'  DC=',DbgS(TheMessage.DC,8));
  if (csDestroying in ComponentState) or (not HandleAllocated) then exit;

  {$IFDEF VerboseDsgnPaintMsg}
  if csDesigning in ComponentState then
    DebugLn('TWinControl.PaintHandler A ',Name,':',ClassName);
  {$ENDIF}

  Assert(False, Format('Trace:> [TWinControl.PaintHandler] %s --> Msg.DC: 0x%x', [ClassName, TheMessage.DC]));
  DC := TheMessage.DC;
  if DC = 0 then
    DC := BeginPaint(Handle, PS);

  try
    // check if child controls need clipping
    //DebugLn('[TWinControl.PaintHandler] ',DbgSName(Self),' B');
    ControlsNeedsClipping:=false;
    if FControls<>nil then
      for I := 0 to FControls.Count - 1 do
        if ControlMustBeClipped(TControl(FControls[I])) then begin
          ControlsNeedsClipping:=true;
          break;
        end;
    // exclude child controls and send new paint message
    if not ControlsNeedsClipping then begin
      //DebugLn('[TWinControl.PaintHandler] ',DbgSName(Self),' no clipping ...');
      PaintWindow(DC)
    end else
    begin
      SaveIndex := SaveDC(DC);
      Clip := SimpleRegion;
      for I := 0 to FControls.Count - 1 do begin
        CurControl:=TControl(FControls[I]);
        if ControlMustBeClipped(CurControl) then
          with CurControl do begin
            //DebugLn('TWinControl.PaintHandler Exclude Child ',DbgSName(Self),' Control=',DbgSName(CurControl),'(',dbgs(CurControl.BoundsRect),')');
            Clip := ExcludeClipRect(DC, Left, Top, Left + Width, Top + Height);
            if Clip = NullRegion then Break;
          end;
      end;
      //DebugLn('[TWinControl.PaintHandler] ',DbgSName(Self),' with clipping ...');
      if Clip <> NullRegion then
        PaintWindow(DC);
      RestoreDC(DC, SaveIndex);
    end;
    // paint controls
    //DebugLn('[TWinControl.PaintHandler] ',DbgSName(Self),' PaintControls ...');
    if FDockSite and FUseDockManager and Assigned(FDockManager) then
      FDockManager.PaintSite(DC);
    PaintControls(DC, nil);
  finally
    if TheMessage.DC = 0 then
      EndPaint(Handle, PS);
  end;
  Assert(False, Format('Trace:< [TWinControl.PaintHandler] %s', [ClassName]));
//DebugLn('[TWinControl.PaintHandler] END  ',Name,':',ClassName,'  DC=',DbgS(Message.DC,8));
end;

procedure TWinControl.PaintControls(DC: HDC; First: TControl);
var
  I, Count, SaveIndex: Integer;
//  FrameBrush: HBRUSH;
  TempControl : TControl;
  {off $Define VerboseControlDCOrigin}
  {$IFDEF VerboseControlDCOrigin}
  P: TPoint;
  {$ENDIF}
begin
  //DebugLn('[TWinControl.PaintControls] ',Name,':',ClassName,'  DC=',DbgS(DC,8));
  if (csDestroying in ComponentState)
  or ((DC=0) and (not HandleAllocated)) then
    exit;

  {$IFDEF VerboseDsgnPaintMsg}
  if csDesigning in ComponentState then
    DebugLn('TWinControl.PaintControls A ',Name,':',ClassName);
  {$ENDIF}

  // Controls that are not TWinControl, have no handle of their own, and so
  // they are repainted as part of the parent:
  if FControls <> nil then
  begin
    I := 0;
    if First <> nil then
    begin
      I := FControls.IndexOf(First);
      if I < 0 then I := 0;
    end;
    Count := FControls.Count;
    while I < Count do
    begin
      TempControl := TControl(FControls.Items[I]);
      //DebugLn('TWinControl.PaintControls B Self=',Self.Name,':',Self.ClassName,' Control=',TempControl.Name,':',TempControl.ClassName,' ',TempControl.Left,',',TempControl.Top,',',TempControl.Width,',',TempControl.Height);
      with TempControl do
        if IsVisible
        and RectVisible(DC, Rect(Left, Top, Left + Width, Top + Height)) then
        begin
          if csPaintCopy in Self.ControlState then
            Include(FControlState, csPaintCopy);
          SaveIndex := SaveDC(DC);
          MoveWindowOrg(DC, Left, Top);
          {$IFDEF VerboseControlDCOrigin}
          DebugLn('TWinControl.PaintControls B Self=',DbgSName(Self),' Control=',DbgSName(TempControl),' ',dbgs(Left),',',dbgs(Top),',',dbgs(Width),',',dbgs(Height));
          {$ENDIF}
          IntersectClipRect(DC, 0, 0, Width, Height);
          {$IFDEF VerboseControlDCOrigin}
          DebugLn('TWinControl.PaintControls C');
          P:=Point(-1,-1);
          GetWindowOrgEx(DC,@P);
          debugln('  DCOrigin=',dbgs(P));
          {$ENDIF}
          Perform(LM_PAINT, WParam(DC), 0);
          {$IFDEF VerboseControlDCOrigin}
          DebugLn('TWinControl.PaintControls D TempControl=',DbgSName(TempControl));
          {$ENDIF}
          RestoreDC(DC, SaveIndex);
          Exclude(FControlState, csPaintCopy);
        end;
      Inc(I);
    end;
  end;
  // draw specials for the wincontrols:
  if FWinControls <> nil then
    for I := 0 to FWinControls.Count - 1 do ;
  //DebugLn('[TWinControl.PaintControls] END ',Name,':',ClassName,'  DC=',DbgS(DC,8));
end;

procedure TWinControl.PaintWindow(DC: HDC);
var
  Message: TLMessage;
begin
  //DebugLn('[TWinControl.PaintWindow] ',Name,':',Classname,'  DC=',DbgS(DC,8));
  if (csDestroying in ComponentState)
  or ((DC=0) and (not HandleAllocated)) then
    exit;

  {$IFDEF VerboseDsgnPaintMsg}
  if csDesigning in ComponentState then
    DebugLn('TWinControl.PaintWindow A ',Name,':',ClassName);
  {$ENDIF}

  Message.Msg := LM_PAINT;
  Message.WParam := WParam(DC);
  Message.LParam := 0;
  Message.Result := 0;
  DefaultHandler(Message);
end;

procedure TWinControl.CreateBrush;
begin
  if FBrush<>nil then exit;
  FBrush:=TBrush.Create;
  FBrush.Color:=Color;
  // ToDo: ParentColor
end;

{------------------------------------------------------------------------------
  procedure TWinControl.EraseBackground;
------------------------------------------------------------------------------}
procedure TWinControl.EraseBackground(DC: HDC);
var
  ARect: TRect;
begin
  if DC = 0 then Exit;
  ARect := Rect(0, 0, Width, Height);
  FillRect(DC, ARect, HBRUSH(Brush.Reference.Handle));
end;

{------------------------------------------------------------------------------
  function TWinControl.IntfUTF8KeyPress(var UTF8Key: TUTF8Char;
    RepeatCount: integer; SystemKey: boolean): boolean;

  Called by the interface after the navigation and specials keys are handled
  (e.g. after KeyDown but before KeyPress).
------------------------------------------------------------------------------}
function TWinControl.IntfUTF8KeyPress(var UTF8Key: TUTF8Char;
  RepeatCount: integer; SystemKey: boolean): boolean;
begin
  Result:=(RepeatCount>0) and (not SystemKey) and DoUTF8KeyPress(UTF8Key);
end;

procedure TWinControl.PaintTo(DC: HDC; X, Y: Integer);
begin
  if HandleAllocated then
    TWSWinControlClass(WidgetSetClass).PaintTo(Self, DC, X, Y);
end;

procedure TWinControl.PaintTo(ACanvas: TCanvas; X, Y: Integer);
begin
  PaintTo(ACanvas.Handle, X, Y);
end;

procedure TWinControl.SetShape(AShape: TBitmap);
begin
  if not HandleAllocated then
    Exit;

  if (AShape <> nil) and (AShape.Width = Width) and (AShape.Height = Height) then
    TWSWinControlClass(WidgetSetClass).SetShape(Self, AShape.Handle)
  else
  if AShape = nil then
    TWSWinControlClass(WidgetSetClass).SetShape(Self, 0)
end;

{------------------------------------------------------------------------------
  TWinControl ControlAtPos
  Params: const Pos : TPoint
          AllowDisabled: Boolean
  Results: TControl

  Searches a child (not grand child) control, which client area contains Pos.
  Pos is relative to the ClientOrigin.
------------------------------------------------------------------------------}
function TWinControl.ControlAtPos(const Pos : TPoint;
  AllowDisabled : Boolean): TControl;
begin
  Result := ControlAtPos(Pos,AllowDisabled,false);
end;

{------------------------------------------------------------------------------
  TWinControl ControlAtPos
  Params: const Pos : TPoint
          AllowDisabled, AllowWinControls: Boolean
  Results: TControl

  Searches a child (not grand child) control, which client area contains Pos.
  Pos is relative to the ClientOrigin.
------------------------------------------------------------------------------}
function TWinControl.ControlAtPos(const Pos : TPoint;
  AllowDisabled, AllowWinControls: Boolean): TControl;
var
  Flags: TControlAtPosFlags;
begin
  Flags:=[capfOnlyClientAreas];
  if AllowDisabled then Include(Flags,capfAllowDisabled);
  if AllowWinControls then Include(Flags,capfAllowWinControls);
  Result := ControlAtPos(Pos,Flags);
end;

{------------------------------------------------------------------------------
  TWinControl ControlAtPos
  Params: const Pos : TPoint
          Flags: TControlAtPosFlags
  Results: TControl

  Searches a child (not grand child) control, which contains Pos.
  Pos is relative to the ClientOrigin.
------------------------------------------------------------------------------}
function TWinControl.ControlAtPos(const Pos: TPoint;
  Flags: TControlAtPosFlags): TControl;
var
  I: Integer;
  P: TPoint;
  LControl: TControl;
  ClientBounds: TRect;

  function GetControlAtPos(AControl: TControl): Boolean;
  var
    ControlPos: TPoint;
    ControlClientBounds: TRect;
  begin
    with AControl do
    begin
      // MG: Delphi checks for PtInRect(ClientRect,P). But the client area is
      //     not always at 0,0, so I guess this is a bug in the VCL.

      ControlPos:=Point(P.X-Left,P.Y-Top);
      Result:=(ControlPos.X>=0) and (ControlPos.Y>=0)
              and (ControlPos.X<Width) and (ControlPos.Y<Height);
      if Result and (capfOnlyClientAreas in Flags) then begin
        ControlClientBounds:=GetChildsRect(false);
        Result:=PtInRect(ControlClientBounds,ControlPos);
      end;

      Result:= Result
        and (
             (
              (csDesigning in ComponentState)
               and not (csNoDesignVisible in ControlStyle)
               // Here was a VCL bug: VCL checks if control is Visible,
               // which should be ignored at designtime
             )
             or
             (
              (not (csDesigning in ComponentState))
              and
              (Visible)
              and
              (Enabled or (capfAllowDisabled in Flags))
              and
              (Perform(CM_HITTEST, 0,
                       LParam(Integer(PointToSmallPoint(ControlPos)))) <> 0)
             )
            );
      {$IFDEF VerboseMouseBugfix}
      //if Result then
      DebugLn(['GetControlAtPos ',Name,':',ClassName,
      ' Pos=',Pos.X,',',Pos.Y,
      ' P=',P.X,',',P.Y,
      ' ControlPos=',dbgs(ControlPos),
      ' ClientBounds=',ClientBounds.Left,',',ClientBounds.Top,',',ClientBounds.Right,',',ClientBounds.Bottom,
      ' OnlyCl=',OnlyClientAreas,
      ' Result=',Result]);
      {$ENDIF}
      if Result then
        LControl := AControl;
    end;
  end;

var
  ScrolledOffset: TPoint;
  OldClientOrigin: TPoint;
  NewClientOrigin: TPoint;
  NewPos: TPoint;
begin
  //debugln(['TWinControl.ControlAtPos START ',DbgSName(Self),' P=',dbgs(Pos)]);

  // check if Pos in visible client area
  ClientBounds:=GetClientRect;
  ScrolledOffset:=GetClientScrollOffset;
  if capfHasScrollOffset in Flags then
  begin
    { ClientBounds do not include scrolling offset }
    inc(ClientBounds.Left, ScrolledOffset.x);
    inc(ClientBounds.Right, ScrolledOffset.x);
    inc(ClientBounds.Top, ScrolledOffset.y);
    inc(ClientBounds.Bottom, ScrolledOffset.y);
  end;

  if not PtInRect(ClientBounds,Pos) then begin
    //debugln(['TWinControl.ControlAtPos OUT OF CLIENTBOUNDS ',DbgSName(Self),' P=',dbgs(Pos),' ClientBounds=',dbgs(ClientBounds)]);
    Result:=nil;
    exit;
  end;

  // map Pos to logical client area
  P:=Pos;
  if not (capfHasScrollOffset in Flags) then begin
    inc(P.X,ScrolledOffset.X);
    inc(P.Y,ScrolledOffset.Y);
  end;

  LControl := nil;
  // check wincontrols
  if (capfAllowWinControls in Flags) and (FWinControls <> nil) then
    for I := FWinControls.Count - 1 downto 0 do
      if GetControlAtPos(TControl(FWinControls[I])) then
        Break;
  // check controls
  if (FControls <> nil) and (LControl = nil) then
    for I := FControls.Count - 1 downto 0 do
      if GetControlAtPos(TControl(FControls[I])) then
        Break;
  Result := LControl;
  
  // check recursive sub childs
  if (capfRecursive in Flags) and (Result is TWinControl)
  and (TWinControl(Result).ControlCount>0) then begin
    OldClientOrigin:=ClientOrigin;
    NewClientOrigin:=TWinControl(Result).ClientOrigin;
    NewPos:=Pos;
    NewPos.X:=NewPos.X-NewClientOrigin.X+OldClientOrigin.X;
    NewPos.Y:=NewPos.Y-NewClientOrigin.Y+OldClientOrigin.Y;
    LControl:=TWinControl(Result).ControlAtPos(NewPos,Flags-[capfHasScrollOffset]);
    //debugln(['TWinControl.RECURSED ControlAtPos Result=',DbgSName(Result),' LControl=',DbgSName(LControl),' ',dbgs(NewPos),' AllowDisabled=',AllowDisabled,' OnlyClientAreas=',OnlyClientAreas]);
    if LControl<>nil then
      Result:=LControl;
  end;
  //debugln(['TWinControl.ControlAtPos END ',DbgSName(Self),' P=',dbgs(Pos),' Result=',DbgSName(Result)]);
end;

{-------------------------------------------------------------------------------
  function TWinControl.GetControlIndex(AControl: TControl): integer;


-------------------------------------------------------------------------------}
function TWinControl.GetControlIndex(AControl: TControl): integer;
begin
  if FControls <> nil
  then begin
    Result := FControls.IndexOf(AControl);
    if Result >= 0 then Exit;
  end;
  
  if FWinControls = nil
  then begin
    Result:=-1;
    Exit;
  end;
  
  Result := FWinControls.IndexOf(AControl);
  if Result = -1 then Exit;
  if FControls = nil then Exit;

  Inc(Result, FControls.Count);
end;

{-------------------------------------------------------------------------------
  function TWinControl.GetControlIndex(AControl: TControl): integer;


-------------------------------------------------------------------------------}
procedure TWinControl.SetControlIndex(AControl: TControl; NewIndex: integer);
begin
  SetChildZPosition(AControl, NewIndex);
end;

function TWinControl.ControlByName(const ControlName: string): TControl;
var
  i: Integer;
begin
  if FControls<>nil then
    for i:=0 to FControls.Count-1 do begin
      Result:=TControl(FControls[i]);
      if CompareText(Result.Name,ControlName)=0 then exit;
    end;
  if FWinControls<>nil then
    for i:=0 to FWinControls.Count-1 do begin
      Result:=TControl(FWinControls[i]);
      if CompareText(Result.Name,ControlName)=0 then exit;
    end;
  Result:=nil;
end;

{------------------------------------------------------------------------------
  TWinControl DestroyHandle
------------------------------------------------------------------------------}
procedure TWinControl.DestroyHandle;
var
  i: integer;
  AWinControl: TWinControl;
begin
  if not HandleAllocated then begin
    DebugLn('Warning: TWinControl.DestroyHandle ',Name,':',ClassName,' Handle not Allocated');
    //RaiseGDBException('');
  end;

  // First destroy all children handles
  Include(FControlState, csDestroyingHandle);
  if FWinControls <> nil then begin
    for i:= 0 to FWinControls.Count - 1 do begin
      //DebugLn(['  i=',i]);
      //DebugLn(['  ',TWinControl(FWinControls[i]).Name,':',TWinControl(FWinControls[i]).ClassName]);
      AWinControl:=TWinControl(FWinControls[i]);
      if AWinControl.HandleAllocated then
        AWinControl.DestroyHandle;
    end;
  end;
  DestroyWnd;
  Exclude(FControlState, csDestroyingHandle);
end;

{------------------------------------------------------------------------------
  TWinControl WndPRoc
------------------------------------------------------------------------------}
procedure TWinControl.WndProc(var Message: TLMessage);
var
  Form: TCustomForm;
begin
//  Assert(False, Format('Trace:[TWinControl.WndPRoc] %s(%s) --> Message = %d', [ClassName, Name, Message.Msg]));
  case Message.Msg of
    LM_SETFOCUS:
      begin
        Assert(False, Format('Trace:[TWinControl.WndPRoc] %s --> LM_SETFOCUS', [ClassName]));
        {$IFDEF VerboseFocus}
        DebugLn('TWinControl.WndProc LM_SetFocus ',DbgSName(Self));
        {$ENDIF}
        if not (csDestroyingHandle in ControlState) then begin
          Form := GetParentForm(Self);
          if (Form <> nil) and not Form.SetFocusedControl(Self) then Exit;
          Message.Result:=0;
        end;
      end;

    LM_KILLFOCUS:
      begin
        Assert(False, Format('Trace:[TWinControl.WndPRoc] %s --> _KILLFOCUS', [ClassName]));
        if csFocusing in ControlState then begin
          {$IFDEF VerboseFocus}
          DebugLn('TWinControl.WndProc LM_KillFocus during focusing ',Name,':',ClassName);
          {$ENDIF}
          Exit;
        end;
        Message.Result:=0;
      end;

    LM_NCHITTEST:
      begin
        inherited WndPRoc(Message);
        if (Message.Result = HTTRANSPARENT)
        and (ControlAtPos(ScreenToClient(SmallPointToPoint(TLMNCHitTest(Message).Pos)),
                          False) <> nil)
        then  Message.Result := HTCLIENT;
        Exit;
      end;

    LM_MOUSEFIRST..LM_MOUSELAST,
    LM_LBUTTONTRIPLECLK,
    LM_LBUTTONQUADCLK,
    LM_MBUTTONTRIPLECLK,
    LM_MBUTTONQUADCLK,
    LM_RBUTTONTRIPLECLK,
    LM_RBUTTONQUADCLK:
       begin
         {$IFDEF VerboseMouseBugfix}
         DebugLn('TWinControl.WndPRoc A ',Name,':',ClassName);
         {$ENDIF}
         //if Message.Msg=LM_RBUTTONUP then begin DebugLn(['TWinControl.WndProc ',DbgSName(Self)]); DumpStack end;
         if IsControlMouseMSG(TLMMouse(Message)) then
           Exit
         else
         begin
           if FDockSite and FUseDockManager and Assigned(FDockManager) then
             FDockManager.MouseMessage(Message);
         end;
         {$IFDEF VerboseMouseBugfix}
         DebugLn('TWinControl.WndPRoc B ',Name,':',ClassName);
         {$ENDIF}
       end;

    LM_KEYFIRST..LM_KEYLAST:
      if Dragging then Exit;

    LM_CANCELMODE:
      if (FindOwnerControl(GetCapture) = Self)
      and (CaptureControl <> nil)
      and (CaptureControl.Parent = Self)
      then CaptureControl.Perform(LM_CANCELMODE,0,0);
    CM_MOUSEENTER,
    CM_MOUSELEAVE:
      begin
        if FDockSite and FUseDockManager and Assigned(FDockManager) then
          FDockManager.MouseMessage(Message);
      end;
  end;

  inherited WndProc(Message);
end;

procedure TWinControl.WSSetText(const AText: String);
begin
  TWSWinControlClass(WidgetSetClass).SetText(Self, AText);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.DoAddDockClient(Client: TControl; const ARect: TRect);
  
  Default method for adding a dock client. Become the new parent and break
  old anchored controls.
 ------------------------------------------------------------------------------}
procedure TWinControl.DoAddDockClient(Client: TControl; const ARect: TRect);
begin
  //DebugLn(['TWinControl.DoAddDockClient ',DbgSName(Self),' Client=',DbgSName(Client),' OldParent=',DbgSName(Client.Parent),' Client.AnchoredControlCount=',Client.AnchoredControlCount]);
  Client.Parent := Self;
end;

{------------------------------------------------------------------------------
  procedure TWinControl.DockOver(Source: TDragDockObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean);
    
  Called to check whether this control allows docking and where.
 ------------------------------------------------------------------------------}
procedure TWinControl.DockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  PositionDockRect(Source);
  DoDockOver(Source, X, Y, State, Accept);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.DoDockOver(Source: TDragDockObject; X, Y: Integer;
    State: TDragState; var Accept: Boolean);
 ------------------------------------------------------------------------------}
procedure TWinControl.DoDockOver(Source: TDragDockObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Assigned(FOnDockOver) then
    FOnDockOver(Self, Source, X, Y, State, Accept);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.DoRemoveDockClient(Client: TControl);

  Called to remove client from dock list.
  This method exists for descendent overrides.
 ------------------------------------------------------------------------------}
procedure TWinControl.DoRemoveDockClient(Client: TControl);
begin
  // empty (this method exists for descendent overrides)
  DebugLn(['TWinControl.DoRemoveDockClient ',DbgSName(Self),' ',DbgSName(Client)]);
end;

{------------------------------------------------------------------------------
  function TWinControl.DoUnDock(NewTarget: TWinControl; Client: TControl
    ): Boolean;
 ------------------------------------------------------------------------------}
function TWinControl.DoUnDock(NewTarget: TWinControl; Client: TControl;
  KeepDockSiteSize: Boolean): Boolean;
var
  NewBounds: TRect;
begin
  DebugLn('TWinControl.DoUnDock ',Name,' NewTarget=',DbgSName(NewTarget),' Client=',DbgSName(Client));
  Result := True;
  if Assigned(FOnUnDock) then
  begin
    FOnUnDock(Self, Client, NewTarget, Result);
    if not Result then
      Exit;
  end;
  if not KeepDockSiteSize then begin
    NewBounds:=BoundsRect;
    case Client.Align of
    alLeft:
      inc(NewBounds.Left,Client.Width);
    alTop:
      inc(NewBounds.Top,Client.Height);
    alRight:
      dec(NewBounds.Right,Client.Width);
    alBottom:
      dec(NewBounds.Bottom,Client.Height);
    end;
    SetBoundsKeepBase(NewBounds.Left,NewBounds.Top,
                      NewBounds.Right-NewBounds.Left,
                      NewBounds.Bottom-NewBounds.Top);
  end;

  Result := Result and DoUndockClientMsg(NewTarget, Client);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
    MousePos: TPoint; var CanDock: Boolean);
 ------------------------------------------------------------------------------}
procedure TWinControl.GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
  MousePos: TPoint; var CanDock: Boolean);
const
  ADockMargin = 10;
begin
  GetWindowRect(Handle,InfluenceRect);
  //Margins to test docking (enlarged surface for test)
  InfluenceRect.Left:= InfluenceRect.Left-ADockMargin;
  InfluenceRect.Top:= InfluenceRect.Top-ADockMargin;
  InfluenceRect.Right:= InfluenceRect.Right+ADockMargin;
  InfluenceRect.Bottom:= InfluenceRect.Bottom+ADockMargin;

  if Assigned(FOnGetSiteInfo) then
    FOnGetSiteInfo(Self,Client,InfluenceRect,MousePos,CanDock);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.ReloadDockedControl(const AControlName: string;
    var AControl: TControl);
 ------------------------------------------------------------------------------}
procedure TWinControl.ReloadDockedControl(const AControlName: string;
  var AControl: TControl);
begin
  AControl := Owner.FindComponent(AControlName) as TControl;
end;

{------------------------------------------------------------------------------
  function TWinControl.CreateDockManager: TDockManager;
 ------------------------------------------------------------------------------}
function TWinControl.CreateDockManager: TDockManager;
begin
  if (FDockManager = nil) and DockSite and UseDockManager then
    // this control can dock other controls, so it needs a TDockManager
    Result := DefaultDockTreeClass.Create(Self)
  else
    Result := FDockManager;
end;

{------------------------------------------------------------------------------
  procedure TWinControl.SetUseDockManager(const AValue: Boolean);
 ------------------------------------------------------------------------------}
procedure TWinControl.SetUseDockManager(const AValue: Boolean);
begin
  if FUseDockManager=AValue then exit;
  FUseDockManager:=AValue;
  if FUseDockManager and ([csDesigning,csDestroying]*ComponentState=[]) then
    FDockManager := CreateDockManager;
end;

procedure TWinControl.DoFloatMsg(ADockSource: TDragDockObject);
var
  WasVisible: Boolean;
begin
  if FloatingDockSiteClass = ClassType then
  begin
    WasVisible := Visible;
    try
      Dock(nil, ADockSource.DockRect);
    finally
      if WasVisible then BringToFront;
    end;
  end
  else
    inherited;
end;

function TWinControl.GetDockCaption(AControl: TControl): String;
begin
  Result := AControl.GetDefaultDockCaption;
  DoGetDockCaption(AControl, Result);
end;

procedure TWinControl.DoGetDockCaption(AControl: TControl; var ACaption: String);
begin
  if Assigned(FOnGetDockCaption) then
    OnGetDockCaption(Self, AControl, ACaption);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.MainWndProc(var Message : TLMessage);

  The message handler of this wincontrol.
  Only needed by controls, which needs features not yet supported by the LCL.
 ------------------------------------------------------------------------------}
procedure TWinControl.MainWndProc(var Msg: TLMessage);
begin
  Assert(False, Format('Trace:[TWinControl.MainWndPRoc] %s(%s) --> Message = %d', [ClassName, Name, Msg.Msg]));
end;

{------------------------------------------------------------------------------
  TWinControl SetFocus
------------------------------------------------------------------------------}
procedure TWinControl.SetFocus;
var
  Form : TCustomForm;
begin
  {$IFDEF VerboseFocus}
  DebugLn('[TWinControl.SetFocus] ',Name,':',ClassName,' Visible=',dbgs(Visible),' HandleAllocated=',dbgs(HandleAllocated));
  {$ENDIF}
  Form := GetParentForm(Self);
  if Form <> nil then
    Form.FocusControl(Self)
  else if IsVisible and HandleAllocated then
    LCLIntf.SetFocus(Handle);
end;

{------------------------------------------------------------------------------
  TWinControl SetParentCtl3D
------------------------------------------------------------------------------}
procedure TWinControl.SetParentCtl3D(value : Boolean);
begin
  if FParentCtl3D <> Value then
  begin
    FParentCtl3D := Value;
    if FParent <> nil then
    begin
      // Sendmessage to do something?
    end;
  end;
end;

{------------------------------------------------------------------------------
  TWinControl KeyDown
------------------------------------------------------------------------------}
procedure TWinControl.KeyDown(var Key: Word; shift : TShiftState);
begin
  if Assigned(FOnKeyDown) then FOnKeyDown(Self, Key, Shift);
end;

{------------------------------------------------------------------------------
  TWinControl KeyDownBeforeInterface
------------------------------------------------------------------------------}
procedure TWinControl.KeyDownBeforeInterface(var Key: Word; Shift: TShiftState
  );
begin
  KeyDown(Key,Shift);
end;

{------------------------------------------------------------------------------
  TWinControl KeyDownAfterInterface
------------------------------------------------------------------------------}
procedure TWinControl.KeyDownAfterInterface(var Key: Word; Shift: TShiftState);
begin

end;

{------------------------------------------------------------------------------
  TWinControl KeyPress
------------------------------------------------------------------------------}
procedure TWinControl.KeyPress(var Key: char);
begin
  if Assigned(FOnKeyPress) then FOnKeyPress(Self, Key);
end;

{------------------------------------------------------------------------------
  TWinControl UTF8KeyPress
  
  Called before KeyPress.
------------------------------------------------------------------------------}
procedure TWinControl.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  if Assigned(FOnUTF8KeyPress) then FOnUTF8KeyPress(Self, UTF8Key);
end;

{------------------------------------------------------------------------------
  TWinControl KeyUp
------------------------------------------------------------------------------}
procedure TWinControl.KeyUp(var Key: Word; Shift : TShiftState);
begin
  if Assigned(FOnKeyUp) then FOnKeyUp(Self, Key, Shift);
end;

procedure TWinControl.KeyUpBeforeInterface(var Key: Word; Shift: TShiftState);
begin
  //debugln('TWinControl.KeyUpBeforeInterface ',DbgSName(Self));
  KeyUp(Key,Shift);
end;

procedure TWinControl.KeyUpAfterInterface(var Key: Word; Shift: TShiftState);
begin
  //debugln('TWinControl.KeyUpAfterInterface ',DbgSName(Self));
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WantsKey
  Params: CharCode - the key to inspect whether it is wanted
  Returns: true if key is wanted before the interface handles it.

  Checks if control wants the passed key to handle before the interface.
 ------------------------------------------------------------------------------}
function TWinControl.WantsKeyBeforeInterface(Key: word; Shift: TShiftState
  ): boolean;
var
  lWantKeys: dword;
{ values for lWantKeys
           0 - if not wanted
           1 - if wanted, but is special (arrow)
           2 - if wanted, but is special (tab)
           4 - if wanted, but is special (all)
           8 - if wanted, is normal key
}
begin
  // For Delphi compatibility we send a LM_GETDLGCODE message to the control
  // asking if it wants to handle the key.
  // We don't define a default handler for LM_GETDLGCODE,
  // so the default return is 0.
  // Note: Contrary to Delphi/win32api, we don't know what keys are special,
  // different widgetsets may have different sets of special keys;
  lWantKeys := Perform(LM_GETDLGCODE, 0, 0);
  if (lWantKeys and DLGC_WANTALLKEYS) <> 0 then
  begin
    lWantKeys := DLGC_WANTALLKEYS;
  end else begin
    case Key of
    VK_TAB: 
      lWantKeys := lWantKeys and DLGC_WANTTAB;
    VK_UP, VK_LEFT, VK_DOWN, VK_RIGHT:
      lWantKeys := lWantKeys and DLGC_WANTARROWS;
    end;
  end;  
  Result := (lWantKeys<>0);
end;

{------------------------------------------------------------------------------
  TWinControl DoKeyDownBeforeInterface
  
  returns true if handled
------------------------------------------------------------------------------}
function TWinControl.DoKeyDownBeforeInterface(var Message: TLMKey): Boolean;
var
  F: TCustomForm;
  ShiftState: TShiftState;
  AParent: TWinControl;
begin
  //debugln('TWinControl.DoKeyDown ',DbgSName(Self),' ShiftState=',dbgs(KeyDataToShiftState(Message.KeyData)),' CharCode=',dbgs(Message.CharCode));
  Result:=true;

  with Message do
  begin
    if CharCode = VK_UNKNOWN then Exit;
    ShiftState := KeyDataToShiftState(KeyData);
    
    // let application handle the key
    if Application<>nil then
      Application.NotifyKeyDownBeforeHandler(Self, CharCode, ShiftState);
    if CharCode = VK_UNKNOWN then Exit;

    // let each parent form with keypreview handle the key
    AParent:=Parent;
    while (AParent<>nil) do begin
      if (AParent is TCustomForm) then begin
        F := TCustomForm(AParent);
        if  (F.KeyPreview)
        and (F.DoKeyDownBeforeInterface(Message)) then Exit;
      end;
      AParent:=AParent.Parent;
    end;

    if CharCode = VK_UNKNOWN then Exit;
    ShiftState := KeyDataToShiftState(KeyData);

    // let drag object handle the key
    if DragManager.IsDragging then
    begin
      DragManager.KeyDown(CharCode, ShiftState);
      Exit;
    end;

    // let user handle the key
    if not (csNoStdEvents in ControlStyle) then 
    begin
      KeyDownBeforeInterface(CharCode, ShiftState);
      if CharCode = VK_UNKNOWN then Exit;
    end;
  end;

  Result := False;
end;

function TWinControl.ChildKey(var Message: TLMKey): boolean;
begin
  if Assigned(Parent) then
    Result := Parent.ChildKey(Message)
  else
    Result := false;
end;

function TWinControl.DialogChar(var Message: TLMKey): boolean;
var
  I: integer;
begin
  // broadcast to children
  Result := false;
  for I := 0 to ControlCount - 1 do
  begin
    Result := Controls[I].DialogChar(Message);
    if Result then exit;
  end;
end;

{------------------------------------------------------------------------------
  TWinControl DoRemainingKeyDown

  Returns True if key handled
------------------------------------------------------------------------------}
function TWinControl.DoRemainingKeyDown(var Message: TLMKeyDown): Boolean;
var
  ShiftState: TShiftState;
  AParent: TWinControl;
begin
  Result:=true;
  
  ShiftState := KeyDataToShiftState(Message.KeyData);

  // check popup menu
  if Assigned(FPopupMenu) then
  begin
    if FPopupMenu.IsShortCut(Message) then
      exit;
  end;

  // let each parent form handle shortcuts
  AParent:=Parent;
  while (AParent<>nil) do begin
    if (AParent is TCustomForm) then begin
      if TCustomForm(AParent).IsShortcut(Message) then
        exit;
    end;
    AParent:=AParent.Parent;
  end;

  // let application handle shortcut
  if Assigned(Application) and Application.IsShortcut(Message) then
    exit;

  // let parent(s) handle key from child key
  if Assigned(Parent) then
    if Parent.ChildKey(Message) then
      exit;

  // handle LCL special keys
  ControlKeyDown(Message.CharCode,ShiftState);
  if Message.CharCode=VK_UNKNOWN then exit;

  //DebugLn('TWinControl.WMKeyDown ',Name,':',ClassName);
  if not (csNoStdEvents in ControlStyle) then
  begin
    KeyDownAfterInterface(Message.CharCode, ShiftState);
    if Message.CharCode=VK_UNKNOWN then exit;
    // Note: Message.CharCode can now be different or even 0
  end;

  // let application handle the remaining key
  if Application<>nil then
    Application.NotifyKeyDownHandler(Self, Message.CharCode, ShiftState);
  if Message.CharCode = VK_UNKNOWN then Exit;

  Result := False;
end;

{------------------------------------------------------------------------------
  TWinControl DoKeyPress
  
  Returns True if key handled
------------------------------------------------------------------------------}
function TWinControl.DoKeyPress(var Message : TLMKey): Boolean;
var
  F: TCustomForm;
  C: char;
  AParent: TWinControl;
begin
  Result := True;

  // let each parent form with keypreview handle the key
  AParent:=Parent;
  while (AParent<>nil) do begin
    if (AParent is TCustomForm) then begin
      F := TCustomForm(AParent);
      if  (F.KeyPreview)
      and (F.DoKeyPress(Message)) then Exit;
    end;
    AParent:=AParent.Parent;
  end;

  if not (csNoStdEvents in ControlStyle)
  then with Message do
  begin
    C := Char(CharCode);
    KeyPress(C);
    CharCode := Ord(C);
    if Char(CharCode) = #0 then Exit;
  end;

  Result := False;
end;

{------------------------------------------------------------------------------
  TWinControl DoRemainingKeyPress
  
  Returns True if key handled
------------------------------------------------------------------------------}
function TWinControl.SendDialogChar(var Message : TLMKey): Boolean;
var
  ParentForm: TCustomForm;
begin
  ParentForm := GetParentForm(Self);
  if ParentForm <> nil then
  begin
    Result := ParentForm.DialogChar(Message);
    if Result then 
    begin
      Message.CharCode := VK_UNKNOWN;
      exit;
    end;
  end;
end;

function TWinControl.DoRemainingKeyUp(var Message: TLMKeyDown): Boolean;
var
  ShiftState: TShiftState;
begin
  //debugln('TWinControl.DoRemainingKeyUp ',DbgSName(Self));
  Result:=true;

  ShiftState := KeyDataToShiftState(Message.KeyData);

  // handle LCL special keys
  ControlKeyUp(Message.CharCode,ShiftState);
  if Message.CharCode=VK_UNKNOWN then exit;

  if not (csNoStdEvents in ControlStyle) then
  begin
    KeyUpAfterInterface(Message.CharCode, ShiftState);
    if Message.CharCode=VK_UNKNOWN then exit;
    // Note: Message.CharCode can now be different or even 0
  end;
end;

{------------------------------------------------------------------------------
  TWinControl DoUTF8KeyPress

  Returns True if key handled
------------------------------------------------------------------------------}
function TWinControl.DoUTF8KeyPress(var UTF8Key: TUTF8Char): boolean;
var
  AParent: TWinControl;
  F: TCustomForm;
begin
  Result:=true;
  
  // let each parent form with keypreview handle the key
  AParent:=Parent;
  while (AParent<>nil) do begin
    if (AParent is TCustomForm) then begin
      F := TCustomForm(AParent);
      if  (F.KeyPreview)
      and (F.DoUTF8KeyPress(UTF8Key)) then Exit;
    end;
    AParent:=AParent.Parent;
  end;
  
  if not (csNoStdEvents in ControlStyle) then begin
    UTF8KeyPress(UTF8Key);
    if UTF8Key='' then exit;
  end;
  
  Result := False;
end;

{------------------------------------------------------------------------------
  TWinControl DoKeyUpBeforeInterface

  Returns True if key handled
------------------------------------------------------------------------------}
function TWinControl.DoKeyUpBeforeInterface(var Message : TLMKey): Boolean;
var
  F: TCustomForm;
  ShiftState: TShiftState;
  AParent: TWinControl;
begin
  Result := True;

  // let each parent form with keypreview handle the key
  AParent:=Parent;
  while (AParent<>nil) do begin
    if (AParent is TCustomForm) then begin
      F := TCustomForm(AParent);
      if  (F.KeyPreview)
      and (F.DoKeyUpBeforeInterface(Message)) then Exit;
    end;
    AParent:=AParent.Parent;
  end;

  with Message do
  begin
    ShiftState := KeyDataToShiftState(KeyData);

    if DragManager.IsDragging then
    begin
      DragManager.KeyUp(CharCode, ShiftState);
      exit;
    end;
    
    if not (csNoStdEvents in ControlStyle)
    then begin
      KeyUpBeforeInterface(CharCode, ShiftState);
      if CharCode = VK_UNKNOWN then Exit;
    end;

    // TODO
    //if (CharCode = VK_APPS) and not (ssAlt in ShiftState) then
    //  CheckMenuPopup(SmallPoint(0, 0));
  end;
  Result := False;
end;

{------------------------------------------------------------------------------
  TWinControl ControlKeyDown
------------------------------------------------------------------------------}
procedure TWinControl.ControlKeyDown(var Key: Word; Shift: TShiftState);
begin
  Application.ControlKeyDown(Self,Key,Shift);
end;

procedure TWinControl.ControlKeyUp(var Key: Word; Shift: TShiftState);
begin
  //debugln('TWinControl.ControlKeyUp ',DbgSName(Self));
  Application.ControlKeyUp(Self,Key,Shift);
end;

{------------------------------------------------------------------------------
  TWinControl CreateParams
------------------------------------------------------------------------------}
procedure TWinControl.CreateParams(var Params : TCreateParams);
begin
  FillChar(Params, SizeOf(Params),0);
  Params.Caption := PChar(FCaption);
  Params.Style := WS_CHILD or WS_CLIPSIBLINGS;
  if (Parent <> nil) then Params.WndParent := Parent.Handle;
  Params.X := FLeft;
  Params.Y := FTop;
  Params.Width := FWidth;
  Params.Height := FHeight;
end;

{------------------------------------------------------------------------------
  TWinControl Invalidate
------------------------------------------------------------------------------}
procedure TWinControl.Invalidate;
begin
  //DebugLn(['TWinControl.Invalidate ',DbgSName(Self),' HandleAllocated=',HandleAllocated]);
  if HandleAllocated and ([csDestroying,csLoading]*ComponentState=[]) then
    TWSWinControlClass(WidgetSetClass).Invalidate(Self);
end;

{------------------------------------------------------------------------------
  TWinControl AddControl

  Add Handle object to parents Handle object.
------------------------------------------------------------------------------}
procedure TWinControl.AddControl;
begin
  TWSControlClass(WidgetSetClass).AddControl(Self);
end;

{------------------------------------------------------------------------------
  TWinControl Repaint
------------------------------------------------------------------------------}
procedure TWinControl.Repaint;
begin
  if (not HandleAllocated) or (csDestroying in ComponentState) then exit;
  {$IFDEF VerboseDsgnPaintMsg}
  if csDesigning in ComponentState then
    DebugLn('TWinControl.Repaint A ',Name,':',ClassName);
  {$ENDIF}
  Invalidate;
  Update;
end;

{------------------------------------------------------------------------------
  TWinControl Insert
------------------------------------------------------------------------------}
procedure TWinControl.Insert(AControl : TControl);
begin
  Insert(AControl,ControlCount);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.Insert(AControl: TControl; Index: integer);
------------------------------------------------------------------------------}
procedure TWinControl.Insert(AControl: TControl; Index: integer);
begin
  if AControl = nil then exit;
  
  if AControl = Self then
    raise EInvalidOperation.Create(rsAControlCanNotHaveItselfAsParent);

  if AControl is TWinControl then
  begin
    if (FControls<>nil) then dec(Index,FControls.Count);
    if (FWinControls<>nil) and (Index<FWinControls.Count) then
      FWinControls.Insert(Index,AControl)
    else
      ListAdd(FWinControls, AControl);

    ListAdd(FTabList, AControl);

    if (csDesigning in ComponentState) and (not (csLoading in ComponentState))
    and AControl.CanTab then
      TWinControl(AControl).TabStop := true;
  end else begin
    if (FControls<>nil) and (Index<FControls.Count) then
      FControls.Insert(Index,AControl)
    else
      ListAdd(FControls, AControl);
  end;

  AControl.FParent := Self;
end;

{------------------------------------------------------------------------------
  TWinControl ReAlign

  Realign all childs
------------------------------------------------------------------------------}
procedure TWinControl.ReAlign;
begin
  if (csDestroying in ComponentState) then exit;
  if (csLoading in ComponentState) or (not HandleAllocated) then begin
    Include(FWinControlFlags,wcfReAlignNeeded);
    exit;
  end;
  {$IFDEF VerboseAutoSize}
  DebugLn('TWinControl.ReAlign A',Name,':',ClassName,' ', Dbgs(BoundsRect));
  {$ENDIF}
  AlignControl(nil);
  {$IFDEF VerboseAutoSize}
  DebugLn('TWinControl.ReAlign B',Name,':',ClassName,' ', Dbgs(BoundsRect));
  {$ENDIF}
  Exclude(FWinControlFlags,wcfReAlignNeeded);
end;

{------------------------------------------------------------------------------
  TWinControl Remove
------------------------------------------------------------------------------}
procedure TWinControl.Remove(AControl : TControl);
begin
  if AControl <> nil then
  begin
    Assert(False, Format('trace:[TWinControl.Remove] %s(%S) --> Remove: %s(%s)', [ClassName, Name, AControl.ClassName, AControl.Name]));
    if AControl is TWinControl then
    begin
      ListRemove(FTabList, AControl);
      ListRemove(FWInControls, ACOntrol);
    end else
      ListRemove(FControls, AControl);
    AControl.FParent := nil;
  end;
end;

procedure TWinControl.AlignNonAlignedControls(ListOfControls: TFPList;
  var BoundsModified: Boolean);
{ All controls, not aligned/anchored by their own properties, can be auto aligned.

  Example:
    cclLeftToRightThenTopToBottom

  +-----------------------------------+
  |+---------------------------------+|
  || Control1 | Control2 | Control 3 ||
  |+---------------------------------+|
  |+---------------------------------+|
  || Control4 | Control5 | Control 6 ||
  |+---------------------------------+|
  |+---------------------+            |
  || Control7 | Control8 |            |
  |+---------------------+            |
  +-----------------------------------+
}
var
  Box: TAutoSizeBox;
  r: TRect;
begin
  // check if ChildSizing aligning is enabled
  if (ChildSizing.Layout=cclNone) or (ListOfControls.Count=0) then
    exit;

  //debugln('TWinControl.AlignNonAlignedControls ',DbgSName(Self),' ListOfControls.Count=',dbgs(ListOfControls.Count),' ',dbgs(ord(ChildSizing.EnlargeHorizontal)));
  
  Box:=TAutoSizeBox.Create;
  try
    r:=GetLogicalClientRect;
    BoundsModified:=Box.AlignControlsInTable(ListOfControls,ChildSizing,
                                             r.Right,r.Bottom,true);
  finally
    Box.Free;
  end;
end;

function TWinControl.IsClientHeightStored: boolean;
begin
  // The ClientHeight is needed to restore childs anchored akBottom
  Result:=ControlCount>0;
end;

function TWinControl.IsClientWidthStored: boolean;
begin
  // The ClientWidth is needed to restore childs anchored akRight
  Result:=ControlCount>0;
end;

{------------------------------------------------------------------------------
  TWinControl RemoveFocus
------------------------------------------------------------------------------}
procedure TWinControl.RemoveFocus(Removing : Boolean);
var
  Form: TCustomForm;
begin
  Form := GetParentForm(Self);
  if Form <> nil then Form.DefocusControl(Self, Removing);
end;

{------------------------------------------------------------------------------
  TWinControl UpdateControlState
------------------------------------------------------------------------------}
procedure TWinControl.UpdateControlState;
var AWinControl: TWinControl;
begin
  AWinControl:= Self;
  { If any of the parent is not visible, exit }
  while AWinControl.Parent <> nil do
  begin
    AWinControl:= AWinControl.Parent;
    if (not AWinControl.Showing) or (not AWinControl.HandleAllocated) then Exit;
  end;

  if ((AWinControl is TCustomForm) and (AWinControl.Parent=nil))
  or (AWinControl.FParentWindow <> 0) then
    UpdateShowing;
end;

{------------------------------------------------------------------------------
  TWinControl InsertControl
------------------------------------------------------------------------------}
procedure TWinControl.InsertControl(AControl: TControl);
begin
  InsertControl(AControl,ControlCount);
end;

procedure TWinControl.InsertControl(AControl: TControl; Index: integer);
begin
  AControl.ValidateContainer(Self);
  Perform(CM_CONTROLLISTCHANGE, WParam(AControl), LParam(True));
  Insert(AControl,Index);
  if not (csReadingState in AControl.ControlState) then
  begin
    AControl.Perform(CM_PARENTCOLORCHANGED, 0, 0);
    AControl.Perform(CM_PARENTSHOWHINTCHANGED, 0, 0);
    AControl.Perform(CM_PARENTBIDIMODECHANGED, 0, 0);
    AControl.ParentFontChanged;
    if AControl is TWinControl then
    begin
      AControl.Perform(CM_PARENTCTL3DCHANGED, 0, 0);
      TWinControl(AControl).UpdateControlState;
    end else
      if HandleAllocated then AControl.Invalidate;
    //DebugLn('TWinControl.InsertControl ',Name,':',ClassName);
  end;
  if not (csDestroying in ComponentState) then
    AControl.RequestAlign;
  Perform(CM_CONTROLCHANGE, WParam(AControl), LParam(True));
end;

{------------------------------------------------------------------------------
  TWinControl removeControl
------------------------------------------------------------------------------}
procedure TWinControl.RemoveControl(AControl: TControl);
var
  AWinControl: TWinControl;
begin
  Perform(CM_CONTROLCHANGE, WParam(AControl), LParam(False));
  if AControl is TWinControl then
  begin
    AWinControl := TWinControl(AControl);
    AWinControl.RemoveFocus(True);
    if AWinControl.HandleAllocated then
      AWinControl.DestroyHandle;
  end
  else
    if HandleAllocated then
      AControl.InvalidateControl(AControl.IsVisible, False, True);
  Remove(AControl);
  if not (csDestroying in ComponentState) then
  begin
    if AutoSize then
    begin
      InvalidatePreferredSize;
      AdjustSize;
    end;
    Realign;
  end;
end;

{------------------------------------------------------------------------------
  TWinControl AlignControl
------------------------------------------------------------------------------}
procedure TWinControl.AlignControl(AControl: TControl);
var
  ARect: TRect;
  i: Integer;
  ChildControl: TControl;
  NewRect: TRect;
begin
  //if csDesigning in ComponentState then begin
  //  DbgOut('TWinControl.AlignControl ',Name,':',ClassName);
  //  if AControl<>nil then DebugLn(' AControl=',AControl.Name,':',AControl.ClassName) else DebugLn(' AControl=nil');;
  //end;
  if csDestroying in ComponentState then exit;

  if FAlignLevel <> 0 then begin
    Include(FControlState, csAlignmentNeeded);
    exit;
  end;

  // check if all childs have finished loading
  for i:=0 to ControlCount-1 do begin
    ChildControl:=Controls[i];
    if csLoading in ChildControl.ComponentState then begin
      // child is loading
      // -> mark the child, that itself and its brothers needs realigning
      // (it will do this, when it has finished loading)
      Include(ChildControl.FControlFlags,cfRequestAlignNeeded);
      exit;
    end;
  end;

  DisableAlign;
  try
    ARect:= GetLogicalClientRect;
    AlignControls(AControl, ARect);
    // some widgetsets updates their clientrect when the first child was moved
    // do a second pass if ClientRect changed
    NewRect:=GetLogicalClientRect;
    if not CompareRect(@ARect,@NewRect) then
      AlignControls(AControl, NewRect);
  finally
    Exclude(FControlState, csAlignmentNeeded);
    EnableAlign;
    if (FAlignLevel=0) and (not IsAParentAligning) and (FWinControls<>nil) then
      for i:=0 to FWinControls.Count-1 do
        TWinControl(FWinControls[i]).RealizeBoundsRecursive;
  end;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.ContainsControl
  Params:  Control: the control to be checked
  Returns: Self is a (super)parent of Control

  Checks if Control is a child of Self
 ------------------------------------------------------------------------------}
function TWinControl.ContainsControl(Control: TControl): Boolean;
begin
  while (Control <> nil) and (Control <> Self) do Control := Control.Parent;
  Result := Control = Self;
end;

{------------------------------------------------------------------------------
  TWinControl GetBorderStyle
------------------------------------------------------------------------------}
function TWinControl.GetBorderStyle: TBorderStyle;
begin
  Result := TBorderStyle(FBorderStyle);
end;

{------------------------------------------------------------------------------
  TWinControl GetBrush
------------------------------------------------------------------------------}
function TWinControl.GetBrush: TBrush;
begin
  if FBrush=nil then CreateBrush;
  Result:=FBrush;
end;

{------------------------------------------------------------------------------
  TWinControl GetControl
------------------------------------------------------------------------------}
function TWinControl.GetControl(const Index: Integer): TControl;
var
  N: Integer;
begin
  if FControls <> nil then
    N := FControls.Count
  else
    N := 0;
  if Index < N then
    Result := TControl(FControls[Index])
  else
    Result := TControl(FWinControls[Index - N]);
end;

{------------------------------------------------------------------------------
  TWinControl GetControlCount
------------------------------------------------------------------------------}
function TWinControl.GetControlCount: Integer;
begin
  Result := 0;
  if FControls <> nil then Inc(Result, FControls.Count);
  if FWinControls <> nil then Inc(Result, FWinControls.Count);
end;

function TWinControl.GetDockClientCount: Integer;
begin
  if FDockClients<>nil then
    Result := FDockClients.Count
  else
    Result := 0;
end;

function TWinControl.GetDockClients(Index: Integer): TControl;
begin
  if FDockClients<>nil then
    Result := TControl(FDockClients[Index])
  else
    Result := nil;
end;

{------------------------------------------------------------------------------
  TWinControl GetHandle
------------------------------------------------------------------------------}
function TWinControl.GetHandle: HWND;
begin
  //if not HandleAllocated then DebugLn('TWinControl.GetHandle Creating handle on the fly: ',DbgSName(Self));
  HandleNeeded;
  Result := FHandle;
end;

{------------------------------------------------------------------------------
  TWinControl SetHandle
  Params:  NewHandle
  Returns: Nothing
-------------------------------------------------------------------------------}
procedure TWincontrol.SetHandle(NewHandle: HWND);
begin
  //if (NewHandle=0) and (AnsiCompareText(ClassName,'TPAGE')=0) then
  //  RaiseGDBException('TWincontrol.SetHandle');
  FHandle:=NewHandle;
  InvalidatePreferredSize;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.Create
  Params:  None
  Returns: Nothing

  Constructor for the class.
 ------------------------------------------------------------------------------}
constructor TWinControl.Create(TheOwner : TComponent);
begin
  // do not set borderstyle, because TCustomForm needs to set it before calling
  // inherited, to have it set before handle is created via streaming
  // use property that bsNone is zero
  //FBorderStyle := bsNone;
  inherited Create(TheOwner);
  FCompStyle := csWinControl;
  FChildSizing:=TControlChildSizing.Create(Self);
  FChildSizing.OnChange:=@DoChildSizingChange;
  FBrush := nil; // Brush will be created on demand. Only few controls need it.
  FParentCtl3D:=true;
  FTabOrder := -1;
  FTabStop := False;
  InvalidateClientRectCache(false);
end;

{------------------------------------------------------------------------------
  TWinControl CreateParented
------------------------------------------------------------------------------}
constructor TWinControl.CreateParented(ParentWindow: hwnd);
begin
  FParentWindow := ParentWindow;
  inherited Create(nil);
end;

{------------------------------------------------------------------------------
  TWinControl CreateParentedControl
------------------------------------------------------------------------------}
class function TWinControl.CreateParentedControl(ParentWindow: hwnd): TWinControl;
begin
  // ToDo
  Result:=nil;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.Destroy
  Params:  None
  Returns: Nothing

  Destructor for the class.
 ------------------------------------------------------------------------------}
destructor TWinControl.Destroy;
var
  n: Integer;
  Control: TControl;
begin
  //DebugLn('[TWinControl.Destroy] A  ',Name,':',ClassName);
  // prevent parent to try to focus a to be destroyed control
  if Parent <> nil then
    RemoveFocus(true);
  if HandleAllocated then
    DestroyHandle;
  //DebugLn('[TWinControl.Destroy] B  ',Name,':',ClassName);

  //for n:=0 to ComponentCount-1 do
  //  DebugLn('  n=',n,' ',Components[n].ClassName);

  n := ControlCount;

  while n > 0 do
  begin
    Control := Controls[n - 1];
    //DebugLn('[TWinControl.Destroy] C  ',Name,':',ClassName,' ',Control.Name,':',Control.ClassName);
    Remove(Control);
    if Control.HostDockSite = Self then
      Control.HostDockSite := nil;
    // don't free the control just set parent to nil
    // controls are freed by the owner
    //Control.Free;
    Control.Parent := nil;
    n := ControlCount;
  end;

  FreeThenNil(FBrush);
  FreeThenNil(FChildSizing);
  if (FDockManager<>nil) then
    if FDockManager.AutoFreeByControl then
      FreeThenNil(FDockManager)
    else
      FDockManager:=nil;
  FreeThenNil(FDockClients);
  FreeThenNil(FTabList);
  //DebugLn('[TWinControl.Destroy] D  ',Name,':',ClassName);
  inherited Destroy;
  //DebugLn('[TWinControl.Destroy] END  ',Name,':',ClassName);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.DoEnter
  Params: none
  Returns: Nothing

  Call user's callback for OnEnter.
 ------------------------------------------------------------------------------}
procedure TWinControl.DoEnter;
begin
  if Assigned(FOnEnter) then FOnEnter(Self);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.DoExit
  Params: none
  Returns: Nothing

  Call user's callback for OnExit.
 ------------------------------------------------------------------------------}
procedure TWinControl.DoExit;
begin
  if Assigned(FOnExit) then FOnExit(Self);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.DoFlipChildren;
  
  Flip children horizontally. That means mirroring the left position.
 ------------------------------------------------------------------------------}
procedure TWinControl.DoFlipChildren;
var
  i: Integer;
  CurControl: TControl;
  AWidth: Integer;
begin
  AWidth:=GetLogicalClientRect.Right;
  for i:=0 to ControlCount-1 do begin
    CurControl:=Controls[i];
    CurControl.Left:=AWidth-CurControl.Left-CurControl.Width;
  end;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.CMEnabledChanged
  Params: Message
  Returns: Nothing

  Called when enabled is changed. Takes action to enable control
 ------------------------------------------------------------------------------}
procedure TWinControl.CMEnabledChanged(var Message: TLMessage);
begin
  if not Enabled and (Parent <> nil)
  then RemoveFocus(False);

  if HandleAllocated and not (csDesigning in ComponentState) then begin
    //if (not Enabled) then debugln('TWinControl.CMEnabledChanged disable ',Name,':',CLassName);
    EnableWindow(Handle, Enabled);
  end;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.CMShowHintChanged
  Params: Message
  Returns: Nothing

  Called when showhint is changed. Notifies children
 ------------------------------------------------------------------------------}
procedure TWinControl.CMShowHintChanged(var Message: TLMessage);
begin
  NotifyControls(CM_PARENTSHOWHINTCHANGED);
end;

procedure TWinControl.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited;
  NotifyControls(CM_PARENTBIDIMODECHANGED);
  if HandleAllocated and (Message.wParam = 0) then
    TWSWinControlClass(WidgetSetClass).SetBiDiMode(Self, UseRightToLeftAlignment, UseRightToLeftReading, UseRightToLeftScrollBar);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMSetFocus
  Params: Message
  Returns: Nothing

  SetFocus event handler
 ------------------------------------------------------------------------------}
procedure TWinControl.WMSetFocus(var Message: TLMSetFocus);
begin
  //DebugLn('TWinControl.WMSetFocus A ',Name,':',ClassName);
  Assert(False, Format('Trace: %s', [ClassName]));
  if [csLoading,csDestroying,csDesigning]*ComponentState=[] then begin
    DoEnter;
  end;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.LMKillFocus
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMKillFocus(var Message: TLMKillFocus);
begin
  //DebugLn('TWinControl.WMKillFocus A ',Name,':',ClassName);
  Assert(False, Format('Trace: %s', [ClassName]));
  if [csLoading,csDestroying,csDesigning]*ComponentState=[] then begin
    EditingDone;
    DoExit;
  end;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMPaint
  Params:   Msg: The paint message
  Returns:  nothing

  Paint event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMPaint(var Msg: TLMPaint);
var
  DC,MemDC: HDC;
{$ifdef BUFFERED_WMPAINT}
  MemBitmap, OldBitmap : HBITMAP;
  MemWidth: Integer;
  MemHeight: Integer;
{$ENDIF}
  PS : TPaintStruct;
  ClientBoundRect: TRect;
begin
  //DebugLn('[TWinControl.WMPaint] ',Name,':',ClassName,'  ',DbgS(Msg.DC,8));
  {$IFDEF VerboseResizeFlicker}
  DebugLn('[TWinControl.WMPaint] ',DbgSName(Self),'  Bounds=',dbgs(BoundsRect),' ClientRect=',dbgs(ClientRect));
  {$ENDIF}
  if ([csDestroying,csLoading]*ComponentState<>[]) or (not HandleAllocated) then
    exit;

  {$IFDEF VerboseDsgnPaintMsg}
  if csDesigning in ComponentState then
    DebugLn('TWinControl.WMPaint A ',Name,':',ClassName);
  {$ENDIF}

  Assert(False, Format('Trace:> [TWinControl.WMPaint] %s Msg.DC: 0x%x', [ClassName, Msg.DC]));
  if (Msg.DC <> 0) then
  begin
    if not (csCustomPaint in ControlState) and (ControlCount = 0) then
      begin
        DefaultHandler(Msg);
      end
    else
      PaintHandler(Msg);
  end
  else begin
    // NOTE: not every interface uses this part
    //DebugLn('TWinControl.WMPaint Painting doublebuffered ',Name,':',classname);
{$ifdef BUFFERED_WMPAINT}
    DC := GetDC(0);
    MemWidth:=Width;
    MemHeight:=Height;
    MemBitmap := CreateCompatibleBitmap(DC, MemWidth, MemHeight);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
{$ENDIF}
    try
      // Fetch a DC of the whole Handle (including client area)
      DC := BeginPaint(Handle, PS);
      if DC=0 then exit;
{$ifNdef BUFFERED_WMPAINT}
      MemDC := DC;
{$ENDIF}
      // erase background
      Include(FWinControlFlags,wcfEraseBackground);
      Perform(LM_ERASEBKGND, WParam(MemDC), 0);
      Exclude(FWinControlFlags,wcfEraseBackground);
      // create a paint message to paint the child controls.
      // WMPaint expects the DC origin to be equal to the client origin of its
      // parent
      // -> Move the DC Origin to the client origin
      if not GetClientBounds(Handle,ClientBoundRect) then exit;
      MoveWindowOrgEx(MemDC,ClientBoundRect.Left,ClientBoundRect.Top);
      // handle the paint message
      Msg.DC := MemDC;
      Perform(LM_PAINT, WParam(MemDC), 0);
      Msg.DC := 0;
      // restore the DC origin
      MoveWindowOrgEx(MemDC,-ClientBoundRect.Left,-ClientBoundRect.Top);
{$ifdef BUFFERED_WMPAINT}
      BitBlt(DC, 0,0, MemWidth, MemHeight, MemDC, 0, 0, SRCCOPY);
{$ENDIF}
      EndPaint(Handle, PS);
    finally
      Exclude(FWinControlFlags,wcfEraseBackground);
{$ifdef BUFFERED_WMPAINT}
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
{$ENDIF}
    end;
  end;
  Assert(False, Format('Trace:< [TWinControl.WMPaint] %s', [ClassName]));
//DebugLn('[TWinControl.WMPaint] END ',Name,':',ClassName);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMDestroy
  Params:   Msg: The destroy message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMDestroy(var Message: TLMDestroy);
begin
  Assert(False, Format('Trace: [TWinControl.LMDestroy] %s', [ClassName]));
  //DebugLn('TWinControl.WMDestroy ',Name,':',ClassName);
  // Our widget/window doesn't exist anymore
  Handle := 0;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMMove
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMMove(var Message: TLMMove);
var
  NewWidth, NewHeight: integer;
begin
  {$IFDEF VerboseSizeMsg}
  DebugLn(['TWinControl.WMMove A ',DbgSName(Self),' Message=',Message.XPos,',',Message.YPos,
    ' BoundsRealized=',FBoundsRealized.Left,',',FBoundsRealized.Top,
    ' FromIntf=',Message.MoveType=Move_SourceIsInterface,
    ',',FBoundsRealized.Right-FBoundsRealized.Left,
    'x',FBoundsRealized.Bottom-FBoundsRealized.Top]);
  {$ENDIF}
  NewWidth:=Width;
  NewHeight:=Height;
  if Message.MoveType=Move_SourceIsInterface then begin
    // interface widget has moved
    // -> update size and realized bounds
    NewWidth:=FBoundsRealized.Right-FBoundsRealized.Left;
    NewHeight:=FBoundsRealized.Bottom-FBoundsRealized.Top;
    if HandleAllocated then
      GetWindowSize(Handle,NewWidth,NewHeight);
    FBoundsRealized:=Bounds(Message.XPos,Message.YPos,NewWidth,NewHeight);
  end;
  SetBoundsKeepBase(Message.XPos,Message.YPos,NewWidth,NewHeight,Parent<>nil);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMSize
  Params:   Message: TLMSize
  Returns:  nothing

  Event handler for size messages. This is called, whenever width, height,
  clientwidth or clientheight have changed.
  If the source of the message is the interface, the new size is stored
  in FBoundsRealized to avoid sending a size message back to the interface.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMSize(var Message: TLMSize);
var
  NewLeft, NewTop: integer;
  NewBoundsRealized: TRect;
begin
  {$IFDEF VerboseSizeMsg}
  {$IFDEF CHECK_POSITION}
  if CheckPosition(Self) then
  {$ENDIF}
  DebugLn(['TWinControl.WMSize A ',Name,':',ClassName,' Message=',Message.Width,',',Message.Height,
    ' BoundsRealized=',dbgs(FBoundsRealized),' FromIntf=',(Message.SizeType and Size_SourceIsInterface)>0]);
  {$ENDIF}
  NewLeft:=Left;
  NewTop:=Top;
  if (Message.SizeType and Size_SourceIsInterface)>0 then
  begin
    // interface widget has resized
    // -> update position and realized bounds
    NewLeft := FBoundsRealized.Left;
    NewTop := FBoundsRealized.Top;
    if HandleAllocated then
      GetWindowRelativePosition(Handle,NewLeft,NewTop);
    //DebugLn('TWinControl.WMSize B ',Name,':',ClassName,' ',NewLeft,',',NewTop);
    NewBoundsRealized:=Bounds(NewLeft,NewTop,Message.Width,Message.Height);
    if CompareRect(@NewBoundsRealized,@FBoundsRealized) and
       (not (wcfClientRectNeedsUpdate in FWinControlFlags)) then exit;
    FBoundsRealized:=NewBoundsRealized;
    InvalidatePreferredSize;
  end;

  SetBoundsKeepBase(NewLeft,NewTop,Message.Width,Message.Height,Parent<>nil);
  if ClientRectNeedsInterfaceUpdate then
    DoAdjustClientRectChange;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.CNKeyDown
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.CNKeyDown(var Message: TLMKeyDown);
begin
  //DebugLn('TWinControl.CNKeyDown ',Name,':',ClassName);
  if DoKeyDownBeforeInterface(Message) then 
    Message.Result := 1
  else
    {inherited};  // there is nothing to inherit
end;

{------------------------------------------------------------------------------
  procedure TWinControl.CNSysKeyDown(var Message: TLMKeyDown);
 ------------------------------------------------------------------------------}
procedure TWinControl.CNSysKeyDown(var Message: TLMKeyDown);
begin
  if DoKeyDownBeforeInterface(Message) then 
    Message.Result := 1
  else
    {inherited};  // there is nothing to inherit
end;

{------------------------------------------------------------------------------
  procedure TWinControl.CNSysKeyUp(var Message: TLMKeyUp);
 ------------------------------------------------------------------------------}
procedure TWinControl.CNSysKeyUp(var Message: TLMKeyUp);
begin
  if DoKeyUpBeforeInterface(Message) then 
    Message.Result := 1
  else
    {inherited}; // there is nothing to inherit
end;

{------------------------------------------------------------------------------
  Method: TWinControl.CNKeyUp
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.CNKeyUp(var Message: TLMKeyUp);
begin
  if DoKeyUpBeforeInterface(Message) then 
    Message.Result := 1
  else
    {inherited}; // there is nothing to inherit
end;

{------------------------------------------------------------------------------
  Method: TWinControl.CNChar
  Params:   Msg: The message
  Returns:  nothing

  event handler.
  CNChar is sent by the interface before it has handled the keypress itself.
 ------------------------------------------------------------------------------}
procedure TWinControl.CNChar(var Message: TLMKeyUp);
var
  c: TUTF8Char;
begin
  //debugln('TWinControl.CNChar B ',DbgSName(Self),' ',dbgs(Message.CharCode),' ',dbgs(IntfSendsUTF8KeyPress));
  if not IntfSendsUTF8KeyPress then
  begin
    // current interface does not (yet) send UTF8 key press notifications
    // -> emulate
    if (Message.CharCode < %11000000) then
    begin
      c:=chr(Message.CharCode);
      IntfUTF8KeyPress(c,1,false);
      if (length(c)<>1) or (c[1]<>chr(Message.CharCode)) then
      begin
        // character changed
        if length(c)=1 then
          Message.CharCode:=ord(c[1])
        else
          Message.CharCode:=0;
      end;
    end;
    if Message.CharCode=0 then 
    begin
      Message.Result := 1;
      exit;
    end;
  end;

  {$ifdef VerboseKeyboard}
    debugln('TWinControl.CNChar A ',DbgSName(Self),' ',dbgs(Message.CharCode),' ',dbgs(IntfSendsUTF8KeyPress));
  {$endif}

  if DoKeyPress(Message) then
    Message.Result := 1
  else
    {inherited}; // there is nothing to inherit
end;

procedure TWinControl.WMSysChar(var Message: TLMKeyUp);
begin
  if SendDialogChar(Message) then 
    Message.Result := 1
  else
    {inherited}; // there is nothing to inherit
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMNofity
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWInControl.WMNotify(var Message: TLMNotify);
begin
  if not DoControlMsg(Message.NMHdr^.hwndfrom,Message) then exit;
  //Inherited;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMShowWindow
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMShowWindow(var Message: TLMShowWindow);
begin
  Assert(False, Format('Trace: TODO: [TWinControl.LMShowWindow] %s', [ClassName]));
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMEnter
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMEnter(var Message: TLMEnter);
begin
  Assert(False, Format('Trace: TODO: [TWinControl.LMEnter] %s', [ClassName]));
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMEraseBkgnd
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  if (Message.DC<>0) and (wcfEraseBackground in FWinControlFlags) then
    EraseBackground(Message.DC);
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMExit
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMExit(var Message: TLMExit);
begin
  Assert(False, Format('Trace: TODO: [TWinControl.LMExit] %s', [ClassName]));
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMChar
  Params:   Msg: The message
  Returns:  nothing

  event handler.
  WMChar is sent by the interface after it has handled the keypress by itself.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMChar(var Message: TLMChar);
begin
  //debugln('TWinControl.WMChar ',DbgSName(Self),' ',dbgs(Message.CharCode));
  if SendDialogChar(Message) then
    Message.Result := 1;
  Assert(False, Format('Trace:[TWinControl.WMChar] %s', [ClassName]));
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMKeyDown
  Params:   Msg: The message
  Returns:  nothing

  Event handler for keys not handled by the interface
 ------------------------------------------------------------------------------}
procedure TWinControl.WMKeyDown(var Message: TLMKeyDown);
begin
  if DoRemainingKeyDown(Message) then
    Message.Result := 1;
end;

procedure TWinControl.WMSysKeyDown(var Message: TLMKeyDown);
begin
  if DoRemainingKeyDown(Message) then
    Message.Result := 1;
end;

{------------------------------------------------------------------------------
  procedure TWinControl.WMSysKeyUp(var Message: TLMKeyUp);
 ------------------------------------------------------------------------------}
procedure TWinControl.WMSysKeyUp(var Message: TLMKeyUp);
begin
  //debugln('TWinControl.WMSysKeyUp ',DbgSName(Self));
  if DoRemainingKeyUp(Message) then
    Message.Result := 1;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.WMKeyUp
  Params:   Msg: The message
  Returns:  nothing

  event handler.
 ------------------------------------------------------------------------------}
procedure TWinControl.WMKeyUp(var Message: TLMKeyUp);
begin
  //debugln('TWinControl.WMKeyUp ',DbgSName(Self));
  if DoRemainingKeyUp(Message) then
    Message.Result := 1;
end;

{------------------------------------------------------------------------------
  function: TWinControl.HandleAllocated
  Params:   None
  Returns:  True is handle is allocated

  Checks if a handle is allocated. I.E. if the control is mapped
 ------------------------------------------------------------------------------}
function TWinControl.HandleAllocated : Boolean;
begin
  HandleAllocated := (FHandle <> 0);
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.CreateHandle
  Params:  None
  Returns: Nothing

  Creates the handle ( = object) if not already done.
 ------------------------------------------------------------------------------}
procedure TWinControl.CreateHandle;
begin
  if (not HandleAllocated) then CreateWnd;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.CreateWnd
  Params:  None
  Returns: Nothing

  Creates the interface object and assigns the handle
 ------------------------------------------------------------------------------}
procedure TWinControl.CreateWnd;
var
  Params: TCreateParams;
  i: Integer;

{  procedure WriteClientRect(const Prefix: string);
  var r: TRect;
  begin
    LCLIntf.GetClientRect(Handle,r);
    if csDesigning in ComponentState then
      DebugLn('WriteClientRect ',Prefix,' ',Name,':',ClassName,' r=',r.Right,',',r.Bottom);
  end;}

begin
  //DebugLn('[TWinControl.CreateWnd] START ',DbgSName(Self));
  if (csDestroying in ComponentState)
  or (Parent<>nil) and (csDestroying in Parent.ComponentState) then begin
    DebugLn('[TWinControl.CreateWnd] NOTE: csDestroying ',DbgSName(Self));
    exit;
  end;

  if wcfInitializing in FWinControlFlags
  then begin
    DebugLn('[WARNING] Recursive call to CreateWnd for ',DbgSName(Self), ' while initializing');
    Exit;
  end;

  if wcfCreatingHandle in FWinControlFlags
  then begin
    DebugLn('[WARNING] Recursive call to CreateWnd for ',DbgSName(Self), ' while creating handle');
    Exit;
  end;

  if wcfCreatingChildHandles in FWinControlFlags
  then begin
    DebugLn('[WARNING] Recursive call to CreateWnd for ',DbgSName(Self), ' while creating children');
    Exit;
  end;
  
  if [csLoading,csDesigning]*ComponentState=[csLoading] then begin
    DebugLn('[HINT] TWinControl.CreateWnd creating Handle during loading ',DbgSName(Self),' csDesigning=',dbgs(csDesigning in ComponentState));
    //DumpStack;
    //RaiseGDBException('');
  end;

  DisableAlign;
  DisableAutoSizing;
  try
    // Control is not visible at this moment. It will be showed in UpdateShowing
    FShowing := False;
    Exclude(FWinControlFlags, wcfHandleVisible);

    Include(FWinControlFlags,wcfCreatingHandle);
    try
      CreateParams(Params);
      with Params do begin
        if (WndParent = 0) and (Style and WS_CHILD <> 0) then
          RaiseGDBException('TWinControl.CreateWnd: no parent '+Name+':'+ClassName);
        Assert((parent <> nil) or (WndParent = 0), 'TODO: find parent if parent=nil and WndParent <> 0');
      end;

      //DebugLn(['TWinControl.CreateWnd ',DbgSName(WidgetSetClass),' ',DbgSName(Self)]);
      FHandle := TWSWinControlClass(WidgetSetClass).CreateHandle(Self, Params);
      if not HandleAllocated then
        RaiseGDBException('Handle creation failed creating '+DbgSName(Self));
      //debugln('TWinControl.CreateWnd ',DbgSName(Self));
      Constraints.UpdateInterfaceConstraints;
      InvalidatePreferredSize;
      TWSWinControlClass(WidgetSetClass).ConstraintsChange(Self);

      //WriteClientRect('A');
      if Parent <> nil then AddControl;
      //WriteClientRect('B');

      Include(FWinControlFlags, wcfInitializing);
      InitializeWnd;

    finally
      Exclude(FWinControlFlags, wcfInitializing);
      Exclude(FWinControlFlags, wcfCreatingHandle);
    end;

    Include(FWinControlFlags, wcfCreatingChildHandles);
    try
      //DebugLn('[TWinControl.CreateWnd] ',Name,':',ClassName,' ',Left,',',Top,',',Width,',',Height);
      //WriteClientRect('C');

      if FWinControls <> nil then begin
        for i := 0 to FWinControls.Count - 1 do
          with TWinControl(FWinControls.Items[i]) do
            if IsControlVisible then HandleNeeded;
      end;

      ChildHandlesCreated;
    finally
      Exclude(FWinControlFlags,wcfCreatingChildHandles);
    end;

    // size this control
    UpdateShowing;
    AdjustSize;
    if FControls<>nil then
      for i:=0 to FControls.Count-1 do
        TControl(FControls[i]).AdjustSize;
    // realign childs
    ReAlign;
  finally
    EnableAutoSizing;
    EnableAlign;
  end;

  //DebugLn('[TWinControl.CreateWnd] END ',Name,':',Classname);
  //WriteClientRect('D');
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.InitializeWnd
  Params:  none
  Returns: Nothing

  Gets called after the window is created, but before the child controls are
  created. Place cached property code here.
 ------------------------------------------------------------------------------}
procedure TWinControl.InitializeWnd;
var
  CachedText: string;
begin
  Assert(False, Format('Trace:[TWinControl.InitializeWnd]  %s', [ClassName]));
  // set all cached properties

  //DebugLn('[TWinControl.InitializeWnd] ',Name,':',ClassName,':', FCaption,' ',dbgs(Left),',',dbgs(Top),',',dbgs(Width),',',dbgs(Height));

  //First set the WinControl property.
  //The win32 interface depends on it to determine where to send call backs.
  SetProp(Handle,'WinControl',TWinControl(Self));
  DisableAlign;
  DisableAutoSizing;
  try
    {$IFDEF CHECK_POSITION}
    if CheckPosition(Self) then
      DebugLn('[TWinControl.InitializeWnd] A ',DbgSName(Self),
      ' OldRelBounds=',dbgs(FBoundsRealized),
      ' -> NewBounds=',dbgs(BoundsRect));
    {$ENDIF}
    if (Width>0) and (Height>0) then
      DoSendBoundsToInterface;

    if wcfColorChanged in FWinControlFlags then begin
      // replace by update style call
      TWSWinControlClass(WidgetSetClass).SetColor(Self);
      FWinControlFlags:=FWinControlFlags-[wcfColorChanged];
    end;
    if wcfFontChanged in FWinControlFlags then begin
      // replace by update style call
      TWSWinControlClass(WidgetSetClass).SetFont(Self,Font);
      FWinControlFlags:=FWinControlFlags-[wcfFontChanged];
    end;

    if not (csDesigning in ComponentState) then
      EnableWindow(Handle, Enabled);

    // Delay the setting of text until it is completely loaded
    if not (csLoading in ComponentState) then
    begin
      if GetCachedText(CachedText) then
        WSSetText(CachedText);
      InvalidatePreferredSize;
    end;

    if csDesigning in ComponentState
    then TWSWinControlClass(WidgetSetClass).SetCursor(Self, Screen.Cursors[crDefault])
    else TWSWinControlClass(WidgetSetClass).SetCursor(Self, Screen.Cursors[Cursor]);
  finally
    EnableAutoSizing;
    EnableAlign;
  end;
  // send pending OnResize
  Resize;
end;

procedure TWinControl.FinalizeWnd;
var
  S: string;
begin
  if not HandleAllocated then
    RaiseGDBException('TWinControl.FinalizeWnd Handle already destroyed');
  // make sure our text is saved
  if TWSWinControlClass(WidgetSetClass).GetText(Self, S)
  then FCaption := S;
  // if color has changed make sure it will be restored
  if FColor<>clWindow then
    Include(FWinControlFlags,wcfColorChanged);
  RemoveProp(Handle,'WinControl');
end;

{------------------------------------------------------------------------------
  procedure TWinControl.ParentFormHandleInitialized;

  Called after all childs handles of the ParentForm are created.
 ------------------------------------------------------------------------------}
procedure TWinControl.ParentFormHandleInitialized;
var
  i: Integer;
begin
  inherited ParentFormHandleInitialized;
  // tell all wincontrols about the final end of the handle creation phase
  if FWinControls <> nil then begin
    for i := 0 to FWinControls.Count - 1 do
      TWinControl(FWinControls.Items[i]).ParentFormHandleInitialized;
  end;
  // tell all controls about the final end of the handle creation phase
  if FControls<>nil then begin
    for i:=0 to FControls.Count-1 do
      TControl(FControls[i]).ParentFormHandleInitialized;
  end;
  //debugln('TWinControl.ParentFormHandleInitialized A ',DbgSName(Self));
  if cfAutoSizeNeeded in FControlFlags then AdjustSize;
end;

{------------------------------------------------------------------------------
  procedure TWinControl.ChildHandlesCreated;
  
  Called after all childs handles are created.
 ------------------------------------------------------------------------------}
procedure TWinControl.ChildHandlesCreated;
begin
  Exclude(FWinControlFlags,wcfCreatingChildHandles);
end;

{------------------------------------------------------------------------------
  function TWinControl.ParentHandlesAllocated: boolean;
  
  Checks if all Handles of all Parents are created.
 ------------------------------------------------------------------------------}
function TWinControl.ParentHandlesAllocated: boolean;
var
  CurControl: TWinControl;
begin
  Result:=false;
  CurControl:=Self;
  while CurControl<>nil do begin
    if (not CurControl.HandleAllocated)
    or (csDestroying in CurControl.ComponentState)
    or (csDestroyingHandle in CurControl.ControlState) then
      exit;
    CurControl:=CurControl.Parent;
  end;
  Result:=true;
end;

{------------------------------------------------------------------------------
  procedure TWinControl.Loaded;
 ------------------------------------------------------------------------------}
procedure TWinControl.Loaded;
var
  CachedText: string;
  i: Integer;
  AChild: TControl;
  LoadedClientSize: TPoint;
  CurControl: TWinControl;
begin
  DisableAlign;
  DisableAutoSizing;
  try
    //DebugLn(['TWinControl.Loaded ',DbgSName(Self),' cfWidthLoaded=',cfWidthLoaded in FControlFlags,' cfHeightLoaded=',cfHeightLoaded in FControlFlags,' ']);
    if cfClientWidthLoaded in FControlFlags then
      LoadedClientSize.X:=FLoadedClientSize.X
    else begin
      CurControl:=Self;
      while CurControl<>nil do begin
        LoadedClientSize.X:=CurControl.ClientWidth;
        if LoadedClientSize.X>0 then break;
        LoadedClientSize.X:=CurControl.Width;
        if LoadedClientSize.X>0 then break;
        CurControl:=CurControl.Parent;
      end;
    end;
    if cfClientHeightLoaded in FControlFlags then
      LoadedClientSize.Y:=FLoadedClientSize.Y
    else begin
      CurControl:=Self;
      while CurControl<>nil do begin
        LoadedClientSize.Y:=CurControl.ClientHeight;
        if LoadedClientSize.Y>0 then break;
        LoadedClientSize.Y:=CurControl.Height;
        if LoadedClientSize.Y>0 then break;
        CurControl:=CurControl.Parent;
      end;
    end;
    for i:=0 to ControlCount-1 do begin
      AChild:=Controls[i];
      if AChild=nil then ;
      AChild.FBaseParentClientSize:=LoadedClientSize;
      //DebugLn(['TWinControl.Loaded Self=',DbgSName(Self),' AChild=',AChild,' AChild.FBaseParentClientSize=',dbgs(AChild.FBaseParentClientSize)]);
    end;
    if HandleAllocated then begin
      // Set cached caption
      if GetCachedText(CachedText) then
        WSSetText(CachedText);
      InvalidatePreferredSize;

      if wcfColorChanged in FWinControlFlags then begin
        TWSWinControlClass(WidgetSetClass).SetColor(Self);
        Exclude(FWinControlFlags,wcfColorChanged);
      end;
      if wcfFontChanged in FWinControlFlags then begin
        TWSWinControlClass(WidgetSetClass).SetFont(Self,Font);
        NotifyControls(CM_PARENTCOLORCHANGED);
        for i := 0 to ControlCount - 1 do
          Controls[i].ParentFontChanged;
        FWinControlFlags:=FWinControlFlags-[wcfFontChanged];
      end;
    end;

    inherited Loaded;

    FixupTabList;
    RealizeBounds;
    if HandleAllocated and ([csDestroying]*ComponentState=[]) then
      DoSendShowHideToInterface;
  finally
    EnableAutoSizing;
    EnableAlign;
  end;
end;

procedure TWinControl.FormEndUpdated;
var
  i: Integer;
begin
  inherited FormEndUpdated;
  for i:=0 to ControlCount-1 do
    Controls[i].FormEndUpdated;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.DestroyWnd
  Params:  None
  Returns: Nothing

  Destroys the interface object.
 ------------------------------------------------------------------------------}
procedure TWinControl.DestroyWnd;
begin
  if HandleAllocated then
  begin
    FinalizeWnd;
    TWSWinControlClass(WidgetSetClass).DestroyHandle(Self);
    Handle := 0;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.HandleNeeded
  Params:  None
  Returns: Nothing

  Description of the procedure for the class.
 ------------------------------------------------------------------------------}
procedure TWinControl.HandleNeeded;
begin
  if (not HandleAllocated) and (not (csDestroying in ComponentState)) then
  begin
    if Parent = Self
    then begin
      Assert(False, Format('Trace:[TWinControl.HandleNeeded] Somebody set Parent := Self in %s. DONT DO THAT !!', [Classname]));
    end
    else begin
      if (Parent <> nil) then
      begin
        Parent.HandleNeeded;
        // has parent triggered us to create our handle ?
        if HandleAllocated then
          exit;
      end;
    end;
  
    Assert(not ParentDestroyingHandle, Format('WARNING:[TWinControl.HandleNeeded] creating handle for %s while destroying handles!', [ClassName]));
    CreateHandle;
  end;
end;

function TWinControl.BrushCreated: Boolean;
begin
  Result:=FBrush<>nil;
end;

{------------------------------------------------------------------------------
  Method: TWinControl.BeginUpdateBounds
  Params:  None
  Returns: Nothing

  increases the BoundsLockCount
 ------------------------------------------------------------------------------}
procedure TWinControl.BeginUpdateBounds;
begin
  inc(FBoundsLockCount);
end;

{------------------------------------------------------------------------------
  Method: TControl.EndUpdateBounds
  Params:  None
  Returns: Nothing

  decreases the BoundsLockCount
 ------------------------------------------------------------------------------}
procedure TWinControl.EndUpdateBounds;

  procedure RaiseTooManyEndUpdates;
  begin
    raise Exception.Create('TWinControl.EndUpdateBounds '+DbgSName(Self)
      +' too many calls.');
  end;

begin
  if FBoundsLockCount<=0 then RaiseTooManyEndUpdates;
  dec(FBoundsLockCount);
  if FBoundsLockCount=0 then begin
    SetBounds(Left,Top,Width,Height);
  end;
end;

procedure TWinControl.LockRealizeBounds;
begin
  inc(FRealizeBoundsLockCount);
end;

procedure TWinControl.UnlockRealizeBounds;
begin
  if FRealizeBoundsLockCount<=0 then
    RaiseGDBException('UnlockRealizeBounds');
  dec(FRealizeBoundsLockCount);
  if FRealizeBoundsLockCount=0 then
    RealizeBounds;
end;

{------------------------------------------------------------------------------
  procedure TWinControl.DockDrop(DockObject: TDragDockObject; X, Y: Integer);
  
  Docks the DockObject.Control onto this control.
  X, Y are only default values. More important is the DockObject.DropAlign
  property, which defines how to align DockObject.Control.
 ------------------------------------------------------------------------------}
procedure TWinControl.DockDrop(DragDockObject: TDragDockObject; X, Y: Integer);
begin
  if DoDockClientMsg(DragDockObject, Point(X, Y)) and Assigned(FOnDockDrop) then
    FOnDockDrop(Self, DragDockObject, X, Y);
end;

{------------------------------------------------------------------------------
  Method: TControl.GetIsResizing
  Params:  None
  Returns: Nothing

  decreases the BoundsLockCount
 ------------------------------------------------------------------------------}
function TWinControl.GetIsResizing: boolean;
begin
  Result:=BoundsLockCount>0;
end;

{------------------------------------------------------------------------------
  function TWinControl.GetTabOrder: TTabOrder;
 ------------------------------------------------------------------------------}
function TWinControl.GetTabOrder: TTabOrder;
begin
  if FParent <> nil then
    Result := ListIndexOf(FParent.FTabList,Self)
  else
    Result := FTabOrder;
end;

{------------------------------------------------------------------------------
  function TWinControl.GetVisibleDockClientCount: Integer;
 ------------------------------------------------------------------------------}
function TWinControl.GetVisibleDockClientCount: Integer;
var
  i: integer;
begin
  Result := 0;
  if FDockClients=nil then exit;
  for i:=FDockClients.Count-1 downto 0 do
    if TControl(FDockClients[I]).Visible then inc(Result);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.SetChildSizing(const AValue: TControlChildSizing);
 ------------------------------------------------------------------------------}
procedure TWinControl.SetChildSizing(const AValue: TControlChildSizing);
begin
  if (FChildSizing=AValue) then exit;
  FChildSizing.Assign(AValue);
end;

{------------------------------------------------------------------------------
  procedure TWinControl.SetDockSite(const NewDockSite: Boolean);
  
  If NewDockSite=true it means, this control can dock other controls.
 ------------------------------------------------------------------------------}
procedure TWinControl.SetDockSite(const NewDockSite: Boolean);
begin
  if FDockSite=NewDockSite then exit;
  FDockSite := NewDockSite;
  if not (csDesigning in ComponentState) then begin
    DragManager.RegisterDockSite(Self,NewDockSite);
    if not NewDockSite then begin
      FreeAndNil(FDockClients);
      FDockClients := nil;
      FDockManager := nil;
    end
    else begin
      if FDockClients = nil then FDockClients := TFPList.Create;
      FDockManager := CreateDockManager;
    end;
  end;
end;

function TWinControl.DoDockClientMsg(DragDockObject: TDragDockObject;
  Position: TPoint): boolean;
var
  DestRect: TRect;
  Form: TCustomForm;
begin
  with DragDockObject do begin
    DestRect := DockRect;
    DisableAlign;
    try
      DragDockObject.Control.Dock(Self, DestRect);
      if FUseDockManager and (FDockManager <> nil) then
        FDockManager.InsertControl(DragDockObject.Control,
          DragDockObject.DropAlign, DragDockObject.DropOnControl);
    finally
      EnableAlign;
    end;
    Form := GetParentForm(Self);
    if Form <> nil then Form.BringToFront;
    Result := true;
  end;
end;

function TWinControl.DoUnDockClientMsg(NewTarget, Client: TControl): boolean;
begin
  Result := true;
  DebugLn(['TWinControl.DoUnDockClientMsg ',DbgSName(Self),' ',DbgSName(Client),' ',DbgSName(Client.Parent)]);
  if FUseDockManager and (FDockManager <> nil) then
    FDockManager.RemoveControl(Client);
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.SetBounds
  Params:  aLeft, aTop, aWidth, aHeight
  Returns: Nothing

  Sets the bounds of the control.
 ------------------------------------------------------------------------------}
procedure TWinControl.SetBounds(aLeft, aTop, aWidth, aHeight : integer);

  procedure CheckDesignBounds;
  begin
    if FRealizeBoundsLockCount>0 then exit;
    // the user changed the bounds
    if aWidth<0 then
      raise Exception.Create(
        'TWinControl.SetBounds ('+DbgSName(Self)+'): Negative width '
          +dbgs(aWidth)+' not allowed.');
    if aHeight<0 then
      raise Exception.Create(
        'TWinControl.SetBounds ('+DbgSName(Self)+'): Negative height '
          +dbgs(aHeight)+' not allowed.');
  end;

var
  NewBounds, OldBounds: TRect;
begin
  {$IFDEF CHECK_POSITION}
  //if csDesigning in ComponentState then
  if CheckPosition(Self) then
  DebugLn(['[TWinControl.SetBounds] START ',DbgSName(Self),
  ' Old=',dbgs(Bounds(Left,Top,Width,Height)),
  ' -> New=',dbgs(Bounds(ALeft,ATop,AWidth,AHeight)),
  ' Lock=',BoundsLockCount,
  ' Realized=',dbgs(FBoundsRealized)
  ]);
  if CheckPosition(Self) and (AWidth=1) then
    DumpStack;
  {$ENDIF}
  if BoundsLockCount<>0 then begin
    //DebugLn(['TWinControl.SetBounds ',DbgSName(Self),' ignoring loop Cur=',dbgs(BoundsRect),' ',dbgs(Bounds(ALeft,ATop,AWidth,AHeight))]);
    exit;
  end;
  OldBounds:=BoundsRect;
  NewBounds:=Bounds(ALeft, ATop, AWidth, AHeight);

  if not CompareRect(@NewBounds,@OldBounds) then begin
    if [csDesigning,csDestroying,csLoading]*ComponentState=[csDesigning] then
      CheckDesignBounds;
    // LCL bounds are not up2date -> process new bounds
    LockRealizeBounds;
    try
      {$IFDEF CHECK_POSITION}
      //if csDesigning in ComponentState then
      if CheckPosition(Self) then
      DebugLn(['[TWinControl.SetBounds] Set LCL Bounds ',DbgSName(Self),
      ' OldBounds=',Dbgs(Bounds(Left,Top,Width,Height)),
      ' -> New=',Dbgs(Bounds(ALeft,ATop,AWidth,AHeight))]);
      {$ENDIF}
      inherited SetBounds(ALeft, ATop, AWidth, AHeight);
      //NewBounds:=Bounds(Left, Top, Width, Height);
    finally
      UnlockRealizeBounds;
    end;
  end;
end;

{------------------------------------------------------------------------------
  procedure TWinControl.CalculatePreferredSize(var PreferredWidth,
    PreferredHeight: integer; WithThemeSpace" Boolean);
    
  Calculates the default/preferred width and height for a TWinControl, which is
  used by the LCL autosizing algorithms as default size. Only positive values
  are valid. Negative or 0 are treated as undefined and the LCL uses other sizes
  instead.
  TWinControl overrides this:
  If there are childs, their total preferred size is calculated.
  If this value can not be computed (e.g. the childs depend too much on their
  parent clientrect), then the interface is asked for the preferred size.
  For example the preferred size of a TButton is the size, where the label fits
  exactly. This depends heavily on the current theme and widgetset.
  
  This value is independent of constraints and siblings, only the inner parts
  are relevant.

  WithThemeSpace: If true, adds space for stacking. For example: TRadioButton
  has a minimum size. But for stacking multiple TRadioButtons there should be
  some space around. This space is theme dependent, so it passed parameter to
  the widgetset.
 ------------------------------------------------------------------------------}
procedure TWinControl.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  Layout: TAutoSizeCtrlData;
  NewClientWidth: Integer;
  NewClientHeight: Integer;
  OldClientRect: TRect;
  NewWidth: Integer;
  NewHeight: Integer;
begin
  inherited CalculatePreferredSize(PreferredWidth,PreferredHeight,WithThemeSpace);
  
  if HandleAllocated then begin
    TWSWinControlClass(WidgetSetClass).GetPreferredSize(Self,
                               PreferredWidth, PreferredHeight, WithThemeSpace);
    if (PreferredWidth>0) then
      inc(PreferredWidth,BorderSpacing.InnerBorder*2);
    if PreferredHeight>0 then
      inc(PreferredHeight,BorderSpacing.InnerBorder*2);
  end;

  if ControlCount>0 then begin

    // get the size requirements for the child controls
    Layout:=nil;
    try
      Layout:=TAutoSizeCtrlData.Create(Self);
      Layout.ComputePreferredClientArea(false,NewClientWidth,NewClientHeight);
    finally
      Layout.Free;
    end;
    
    // add the control border around the client area
    OldClientRect := GetClientRect;
    NewWidth:=Width-OldClientRect.Right+NewClientWidth;
    NewHeight:=Height-OldClientRect.Bottom+NewClientHeight;

    {$IFDEF VerboseAutoSize}
    debugln('TWinControl.CalculatePreferredSize ',DbgSName(Self),
      ' HandleAllocated=',dbgs(HandleAllocated),
      ' Cur='+dbgs(Width)+'x'+dbgs(Height)+
      ' Client='+dbgs(OldClientRect.Right)+'x'+dbgs(OldClientRect.Bottom),
      ' NewWidth='+dbgs(NewWidth)+' NewHeight=',dbgs(NewHeight));
    {$ENDIF}
    PreferredWidth:=Max(PreferredWidth,NewWidth);
    PreferredHeight:=Max(PreferredHeight,NewHeight);
  end;
  {$IFDEF VerboseAutoSize}
  debugln('TWinControl.CalculatePreferredSize ',DbgSName(Self),
    ' HandleAllocated=',dbgs(HandleAllocated),
    ' Preferred=',dbgs(PreferredWidth),'x',dbgs(PreferredHeight));
  {$ENDIF}
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.RealGetText
  Params:  None
  Returns: The text

  Gets the text/caption of a control
 ------------------------------------------------------------------------------}
function TWinControl.RealGetText: TCaption;
begin
  Result := '';
  if not HandleAllocated 
  or (csLoading in ComponentState) 
  or (not TWSWinControlClass(WidgetSetClass).GetText(Self, Result))
  then Result := inherited RealGetText;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.GetTextLen
  Params:  None
  Returns: The length of the text

  Gets the length of the text/caption of a control
 ------------------------------------------------------------------------------}
function TWinControl.GetTextLen: Integer;
begin
  Result := 0;
  if not HandleAllocated 
  or (csLoading in ComponentState) 
  or not TWSWinControlClass(WidgetSetClass).GetTextLen(Self, Result)
  then Result := inherited GetTextLen;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.RealSetText
  Params:  Value: the text to be set
  Returns: Nothing

  Sets the text/caption of a control
 ------------------------------------------------------------------------------}
procedure TWinControl.RealSetText(const AValue: TCaption);
begin
  if HandleAllocated and (not (csLoading in ComponentState)) then
  begin
    WSSetText(AValue);
    InvalidatePreferredSize;
    inherited RealSetText(AValue);
    AdjustSize;
  end
  else inherited RealSetText(AValue);
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.GetDeviceContext
  Params:  WindowHandle: the windowhandle of this control
  Returns: a Devicecontext

  Get the devicecontext for this WinControl.
 ------------------------------------------------------------------------------}
function TWinControl.GetDeviceContext(var WindowHandle: HWnd): HDC;
begin
  Result := GetDC(Handle);
  //DebugLn('[TWinControl.GetDeviceContext] ',ClassName,' DC=',DbgS(Result,8),' Handle=',DbgS(FHandle));
  if Result = 0
  then raise EOutOfResources.CreateFmt(rsErrorCreatingDeviceContext, [Name,
    ClassName]);

  WindowHandle := Handle;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.CMVisibleChanged
  Params:  Message : not used
  Returns: nothing

  Performs actions when visibility has changed
 ------------------------------------------------------------------------------}
procedure TWinControl.CMVisibleChanged(var TheMessage : TLMessage);
begin
  if not FVisible and (Parent <> nil)
  then RemoveFocus(False);

  if not (csDesigning in ComponentState)
  or (csNoDesignVisible in ControlStyle)
  then
    UpdateControlState;
end;

procedure TWinControl.DoSendShowHideToInterface;
var
  NewVisible: Boolean;
begin
  NewVisible:=HandleObjectShouldBeVisible;
  if NewVisible<>(wcfHandleVisible in FWinControlFlags) then begin
    if NewVisible then
      Include(FWinControlFlags,wcfHandleVisible)
    else
      Exclude(FWinControlFlags,wcfHandleVisible);
    TWSWinControlClass(WidgetSetClass).ShowHide(Self);
  end;
end;

procedure TWinControl.ControlsAligned;
begin

end;

procedure TWinControl.DoSendBoundsToInterface;
var
  NewBounds: TRect;
  {$IFDEF VerboseResizeFlicker}
  OldBounds: TRect;
  {$ENDIF}
begin
  NewBounds:=Bounds(Left, Top, Width, Height);
  {$IFDEF VerboseResizeFlicker}
  if HandleAllocated then begin
    GetWindowRelativePosition(Handle,OldBounds.Left,OldBounds.Top);
    GetWindowSize(Handle,OldBounds.Right,OldBounds.Bottom);
    inc(OldBounds.Right,OldBounds.Left);
    inc(OldBounds.Bottom,OldBounds.Top);
  end else
    OldBounds:=NewBounds;
  DebugLn(['[TWinControl.DoSendBoundsToInterface] ',DbgSName(Self),
    ' Old=',dbgs(OldBounds),
    ' New=',dbgs(NewBounds),
    ' PosChanged=',(OldBounds.Left<>NewBounds.Left) or (OldBounds.Top<>NewBounds.Top),
    ' SizeChanged=',(OldBounds.Right-OldBounds.Left<>NewBounds.Right-NewBounds.Left)
                 , (OldBounds.Bottom-OldBounds.Top<>NewBounds.Bottom-NewBounds.Top),
    ' CurClient=',FClientWidth,'x',FClientHeight
    ]);
  {$ENDIF}
  {$IFDEF CHECK_POSITION}
  if CheckPosition(Self) then
    DebugLn('[TWinControl.DoSendBoundsToInterface] A ',DbgSName(Self),
    ' OldRelBounds=',dbgs(FBoundsRealized),
    ' -> NewBounds=',dbgs(NewBounds));
  {$ENDIF}
  FBoundsRealized:=NewBounds;
  TWSWinControlClass(WidgetSetClass).SetBounds(Self, Left, Top, Width, Height);
end;

procedure TWinControl.RealizeBounds;
var
  NewBounds: TRect;
begin
  NewBounds:=Bounds(Left, Top, Width, Height);
  if HandleAllocated
  and ([csLoading,csDestroying]*ComponentState=[])
  and (not (csDestroyingHandle in ControlState))
  and (not CompareRect(@NewBounds,@FBoundsRealized))
  and (not IsAParentAligning) then
  begin
    // the new bounds were not yet sent to the InterfaceObject -> send them
    {$IFDEF CHECK_POSITION}
    //if csDesigning in ComponentState then
    if CheckPosition(Self) then
    DebugLn('[TWinControl.RealizeBounds] A ',DbgSName(Self),
    ' OldRelBounds=',dbgs(FBoundsRealized),
    ' -> NewBounds=',dbgs(NewBounds));
    {$ENDIF}
    BeginUpdateBounds;
    try
      DoSendBoundsToInterface;
    finally
      EndUpdateBounds;
    end;
  end;
end;

procedure TWinControl.RealizeBoundsRecursive;
var
  i: Integer;
begin
  RealizeBounds;
  if FWinControls<>nil then begin
    for i:=0 to FWinControls.Count-1 do
      TWinControl(FWinControls[i]).RealizeBoundsRecursive;
  end;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.CMShowingChanged
  Params:  Message : not used
  Returns: nothing

  Shows or hides a control
 ------------------------------------------------------------------------------}
procedure TWinControl.CMShowingChanged(var Message: TLMessage);
begin
  if HandleAllocated and ([csDestroying,csLoading]*ComponentState=[]) then
    DoSendShowHideToInterface;
end;

{------------------------------------------------------------------------------
  Method:  TWinControl.ShowControl
  Params:  AControl: Control to show
  Returns: nothing

  Askes the parent to show ourself.
 ------------------------------------------------------------------------------}
procedure TWinControl.ShowControl(AControl: TControl);
begin
  if Parent <> nil then Parent.ShowControl(Self);
end;

{ $UNDEF CHECK_POSITION}

{$IFDEF ASSERT_IS_ON}
  {$UNDEF ASSERT_IS_ON}
  {$C-}
{$ENDIF}
