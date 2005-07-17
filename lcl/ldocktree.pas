{  $Id$  }
{
 /***************************************************************************
                               LDockTree.pas
                             -----------------

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    This unit contains TLazDockTree, the default TDockTree for the LCL.
}
unit LDockTree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, ExtCtrls;
  
type
  TLazDockPages = class;
  TLazDockSplitter = class;

  { TLazDockZone }

  TLazDockZone = class(TDockZone)
  private
    FPages: TLazDockPages;
    FSplitter: TLazDockSplitter;
  public
    destructor Destroy; override;
    property Splitter: TLazDockSplitter read FSplitter write FSplitter;
    property Pages: TLazDockPages read FPages write FPages;
  end;

  { TLazDockTree }

  TLazDockTree = class(TDockTree)
  private
    FAutoFreeDockSite: boolean;
  protected
    procedure UndockControlForDocking(AControl: TControl);
    procedure BreakAnchors(Zone: TDockZone);
  public
    constructor Create(TheDockSite: TWinControl); override;
    destructor Destroy; override;
    procedure InsertControl(AControl: TControl; InsertAt: TAlign;
                            DropControl: TControl); override;
    procedure BuildDockLayout(Zone: TLazDockZone);
    procedure FindBorderControls(Zone: TLazDockZone; Side: TAnchorKind;
                                 var List: TFPList);
    function FindBorderControl(Zone: TLazDockZone; Side: TAnchorKind): TControl;
    function GetAnchorControl(Zone: TDockZone; Side: TAnchorKind): TControl;
  public
    property AutoFreeDockSite: boolean read FAutoFreeDockSite write FAutoFreeDockSite;
  end;
  
  { TLazDockForm
    The default DockSite for a TLazDockTree
  
    If DockZone is a leaf (DockZone.ChildCount=0) then
      Only child control is DockZone.ChildControl
    else
      if DockZone.Orientation in [doHorizontal,doVertical] then
        Child controls are TLazDockForm and TSplitter
      else if DockZone.Orientation=doPages then
        Child control is a TLazDockPages
  }

  TLazDockForm = class(TCustomForm)
  private
    FDockZone: TDockZone;
    FPageControl: TLazDockPages;
  public
    property DockZone: TDockZone read FDockZone;
    property PageControl: TLazDockPages read FPageControl;
  end;
  
  { TLazDockPage
    Pretty the same as a TLazDockForm but as page of a TLazDockPages }

  TLazDockPage = class(TCustomPage)
  private
    FDockZone: TDockZone;
    FPageControl: TLazDockPages;
  public
    property DockZone: TDockZone read FDockZone;
    property PageControl: TLazDockPages read FPageControl;
  end;
  
  { TLazDockPages }

  TLazDockPages = class(TCustomNotebook)
  private
    function GetActiveNotebookPageComponent: TLazDockPage;
    function GetNoteBookPage(Index: Integer): TLazDockPage;
    procedure SetActiveNotebookPageComponent(const AValue: TLazDockPage);
  public
    constructor Create(TheOwner: TComponent); override;
    property Page[Index: Integer]: TLazDockPage read GetNoteBookPage;
    property ActivePageComponent: TLazDockPage read GetActiveNotebookPageComponent
                                           write SetActiveNotebookPageComponent;
    property Pages;
  end;
  
  TLazDockSplitter = class(TCustomSplitter)
  end;

const
  DockAlignOrientations: array[TAlign] of TDockOrientation = (
    doPages,     //alNone,
    doVertical,  //alTop,
    doVertical,  //alBottom,
    doHorizontal,//alLeft,
    doHorizontal,//alRight,
    doPages,     //alClient,
    doPages      //alCustom
    );

implementation

{ TLazDockPages }

function TLazDockPages.GetActiveNotebookPageComponent: TLazDockPage;
begin
  Result:=TLazDockPage(GetActiveNotebookPageComponent);
end;

function TLazDockPages.GetNoteBookPage(Index: Integer): TLazDockPage;
begin
  Result:=TLazDockPage(inherited ActivePageComponent);
end;

procedure TLazDockPages.SetActiveNotebookPageComponent(
  const AValue: TLazDockPage);
begin
  ActivePageComponent:=AValue;
end;

constructor TLazDockPages.Create(TheOwner: TComponent);
begin
  PageClass:=TLazDockPage;
  inherited Create(TheOwner);
end;

{ TLazDockTree }

procedure TLazDockTree.UndockControlForDocking(AControl: TControl);
var
  AWinControl: TWinControl;
begin
  // undock AControl
  if AControl is TWinControl then begin
    AWinControl:=TWinControl(AControl);
    if AWinControl.DockManager<>nil then begin
      // TODO
    end;
  end;
  if AControl.Parent<>nil then begin
    AControl.Parent:=nil;
  end;
end;

procedure TLazDockTree.BreakAnchors(Zone: TDockZone);
begin
  if Zone=nil then exit;
  if Zone.ChildControl<>nil then begin
    Zone.ChildControl.AnchorSide[akLeft].Control:=nil;
    Zone.ChildControl.AnchorSide[akTop].Control:=nil;
    Zone.ChildControl.Anchors:=[akLeft,akTop];
  end;
  BreakAnchors(Zone.FirstChild);
  BreakAnchors(Zone.NextSibling);
end;

constructor TLazDockTree.Create(TheDockSite: TWinControl);
begin
  SetDockZoneClass(TLazDockZone);
  if TheDockSite=nil then begin
    TheDockSite:=TLazDockForm.Create(nil);
    TheDockSite.DockManager:=Self;
    FAutoFreeDockSite:=true;
  end;
  inherited Create(TheDockSite);
end;

destructor TLazDockTree.Destroy;
begin
  if FAutoFreeDockSite then begin
    if DockSite.DockManager=Self then
      DockSite.DockManager:=nil;
    DockSite.Free;
    DockSite:=nil;
  end;
  inherited Destroy;
end;

procedure TLazDockTree.InsertControl(AControl: TControl; InsertAt: TAlign;
  DropControl: TControl);
{ undocks AControl and docks it into the tree
  It creates a new TDockZone for AControl and inserts it as a new leaf.
  It automatically changes the tree, so that the parent of the new TDockZone
  will have the Orientation for InsertAt.
  
  Example 1:

    A newly created TLazDockTree has only a DockSite (TLazDockForm) and a single
    TDockZone - the RootZone, which has as ChildControl the DockSite.
    
    Visual:
      +-DockSite--+
      |           |
      +-----------+
    Tree of TDockZone:
      RootZone (DockSite,doNoOrient)


  Inserting the first control:  InsertControl(Form1,alLeft,nil);
    Visual:
      +-DockSite---+
      |+--Form1---+|
      ||          ||
      |+----------+|
      +------------+
    Tree of TDockZone:
      RootZone (DockSite,doHorizontal)
       +-Zone2 (Form1,doNoOrient)


  Dock Form2 right of Form1:  InsertControl(Form2,alLeft,Form1);
    Visual:
      +-DockSite----------+
      |+-Form1-+|+-Form2-+|
      ||        ||       ||
      |+-------+|+-------+|
      +-------------------+
    Tree of TDockZone:
      RootZone (DockSite,doHorizontal)
       +-Zone2 (Form1,doNoOrient)
       +-Zone3 (Form2,doNoOrient)
}
const
  SplitterWidth = 5;
  SplitterHeight = 5;
var
  DropZone: TDockZone;
  NewZone: TLazDockZone;
  NewOrientation: TDockOrientation;
  NeedNewParentZone: Boolean;
  NewParentZone: TDockZone;
  OldParentZone: TDockZone;
  NewBounds: TRect;
begin
  if DropControl=nil then
    DropControl:=DockSite;
  DropZone:=RootZone.FindZone(DropControl);
  if DropZone=nil then
    raise Exception.Create('TLazDockTree.InsertControl DropControl is not part of this TDockTree');

  NewOrientation:=DockAlignOrientations[InsertAt];
  // TODO: check what needs to be done
  
  // undock
  UndockControlForDocking(AControl);
  
  // dock
  // create a new zone for AControl
  NewZone:=DockZoneClass.Create(Self,AControl) as TLazDockZone;
  
  // insert new zone into tree
  if (DropZone=RootZone) and (RootZone.FirstChild=nil) then begin
    // this is the first child
    debugln('TLazDockTree.InsertControl First Child');
    RootZone.Orientation:=NewOrientation;
    RootZone.AddAsFirstChild(NewZone);
    if not AControl.Visible then
      DockSite.Visible:=false;
    DockSite.BoundsRect:=AControl.BoundsRect;
    AControl.Parent:=DockSite;
    if AControl.Visible then
      DockSite.Visible:=true;
  end else begin
    // there are already other childs

    // optimize DropZone
    if (DropZone.ChildCount>0)
    and (NewOrientation in [doHorizontal,doVertical])
    and ((DropZone.Orientation=NewOrientation)
         or (DropZone.Orientation=doNoOrient))
    then begin
      // docking on a side of an inner node is the same as docking to a side of
      // a child
      if InsertAt in [alLeft,alTop] then
        DropZone:=DropZone.FirstChild
      else
        DropZone:=DropZone.GetLastChild;
    end;
    
    // insert a new Parent Zone if needed
    NeedNewParentZone:=true;
    if (DropZone.Parent<>nil) then begin
      if (DropZone.Orientation=doNoOrient) then
        NeedNewParentZone:=false;
      if (DropZone.Orientation=NewOrientation) then
        NeedNewParentZone:=false;
    end;
    if NeedNewParentZone then begin
      // insert a new zone between current DropZone.Parent and DropZone
      // this new zone will become the new DropZone.Parent
      OldParentZone:=DropZone.Parent;
      NewParentZone:=DockZoneClass.Create(Self,nil);
      if OldParentZone<>nil then
        OldParentZone.ReplaceChild(DropZone,NewParentZone);
      NewParentZone.AddAsFirstChild(DropZone);
    end;
    
    // adjust Orientation in tree
    if DropZone.Parent.Orientation=doNoOrient then
      DropZone.Parent.Orientation:=NewOrientation;
    if DropZone.Parent.Orientation<>NewOrientation then
      RaiseGDBException('TLazDockTree.InsertControl Inconsistency DropZone.Orientation<>NewOrientation');

    // insert new node
    if DropZone.Parent=nil then
      RaiseGDBException('TLazDockTree.InsertControl Inconsistency DropZone.Parent=nil');

    if InsertAt in [alLeft,alTop] then
      DropZone.Parent.AddAsFirstChild(NewZone)
    else
      DropZone.Parent.AddAsLastChild(NewZone);

    // break anchors and resize DockSite
    BreakAnchors(RootZone);
    NewBounds:=DockSite.BoundsRect;
    case InsertAt of
    alLeft:  dec(NewBounds.Left,SplitterWidth+AControl.Width);
    alRight: inc(NewBounds.Right,SplitterWidth+AControl.Width);
    alTop:   dec(NewBounds.Top,SplitterHeight+AControl.Height);
    alBottom:inc(NewBounds.Bottom,SplitterHeight+AControl.Height);
    else     // no change
    end;
    DockSite.BoundsRect:=NewBounds;
    
    // add AControl to DockSite
    AControl.Visible:=false;
    AControl.Parent:=nil;
    AControl.Align:=alNone;
    AControl.Anchors:=[akLeft,akTop];
    AControl.AnchorSide[akLeft].Control:=nil;
    AControl.AnchorSide[akTop].Control:=nil;
    AControl.AutoSize:=false;
    AControl.Parent:=DockSite;

    // Build dock layout (anchors, splitters, pages)
    BuildDockLayout(RootZone as TLazDockZone);
  end;
end;

procedure TLazDockTree.BuildDockLayout(Zone: TLazDockZone);
var
  ASide: TAnchorKind;
  FixedSide1: TAnchorKind;
  FixedSide2: TAnchorKind;
  CurControl: TControl;
  CurSplitter: TLazDockSplitter;
  SideAnchorControl: TControl;
  FixedSide1AnchorControl: TControl;
  FixedSide2AnchorControl: TControl;
begin
  // reset anchors
  BreakAnchors(RootZone);

  // create needed TLazDockPages
  if (Zone.Orientation=doPages) then begin
    // a zone of pages -> needs a TLazDockPages
    if (Zone.Pages=nil) then begin
      Zone.Pages:=TLazDockPages.Create(nil);
    end;
  end else if Zone.Pages<>nil then begin
    // zone no longer needs the pages
    Zone.Pages.Free;
    Zone.Pages:=nil;
  end;
  
  // create needed TLazDockPage
  RaiseGDBException('TODO create needed TLazDockPage');

  // create needed TLazDockSplitter
  if (Zone.Parent<>nil)
  and (Zone.Parent.Orientation in [doVertical,doHorizontal])
  and (Zone.PrevSibling<>nil) then begin
    // a zone with a side sibling -> needs a TLazDockSplitter
    if Zone.Splitter=nil then begin
      Zone.Splitter:=TLazDockSplitter.Create(nil);
    end;
    CurSplitter:=Zone.Splitter;
    if Zone.Parent.Orientation=doVertical then begin
      ASide:=akLeft;
      FixedSide1:=akTop;
      FixedSide2:=akBottom;
    end else begin
      ASide:=akTop;
      FixedSide1:=akLeft;
      FixedSide2:=akRight;
    end;
    // anchor splitter
    CurSplitter.Align:=alNone;
    CurSplitter.ResizeAnchor:=ASide;
    SideAnchorControl:=GetAnchorControl(Zone,ASide);
    FixedSide1AnchorControl:=GetAnchorControl(Zone,FixedSide1);
    FixedSide2AnchorControl:=GetAnchorControl(Zone,FixedSide2);
    CurSplitter.Anchors:=[ASide,FixedSide1,FixedSide2];
    CurSplitter.AnchorSide[ASide].Control:=SideAnchorControl;
    CurSplitter.AnchorSide[FixedSide1].Control:=FixedSide1AnchorControl;
    CurSplitter.AnchorSide[FixedSide2].Control:=FixedSide2AnchorControl;
    // anchor control
    CurControl:=Zone.ChildControl;
    if CurControl<>nil then begin
      CurControl.Anchors:=[akLeft,akTop,akRight,akBottom];
      CurControl.AnchorSide[ASide].Control:=CurSplitter;
      CurControl.AnchorSide[FixedSide1].Control:=FixedSide1AnchorControl;
      CurControl.AnchorSide[FixedSide2].Control:=FixedSide2AnchorControl;
      CurControl.AnchorSide[OppositeAnchor[ASide]].Control:=
                                   GetAnchorControl(Zone,OppositeAnchor[ASide]);
    end;
    
    RaiseGDBException('TODO');
  end else if Zone.Splitter<>nil then begin
    // zone no longer needs the splitter
    Zone.Splitter.Free;
    Zone.Splitter:=nil;
  end;

  RaiseGDBException('TODO');

end;

procedure TLazDockTree.FindBorderControls(Zone: TLazDockZone; Side: TAnchorKind;
  var List: TFPList);
begin
  if List=nil then List:=TFPList.Create;
  if Zone=nil then exit;
  
  if (Zone.Splitter<>nil) and (Zone.Parent<>nil)
  and (Zone.Orientation=doVertical) then begin
    // this splitter is leftmost, topmost, bottommost
    if Side in [akLeft,akTop,akBottom] then
      List.Add(Zone.Splitter);
    if Side=akLeft then begin
      // the splitter fills the whole left side => no more controls
      exit;
    end;
  end;
  if (Zone.Splitter<>nil) and (Zone.Parent<>nil)
  and (Zone.Orientation=doHorizontal) then begin
    // this splitter is topmost, leftmost, rightmost
    if Side in [akTop,akLeft,akRight] then
      List.Add(Zone.Splitter);
    if Side=akTop then begin
      // the splitter fills the whole top side => no more controls
      exit;
    end;
  end;
  if Zone.ChildControl<>nil then begin
    // the ChildControl fills the whole zone (except for the splitter)
    List.Add(Zone.ChildControl);
    exit;
  end;
  if Zone.Pages<>nil then begin
    // the pages fills the whole zone (except for the splitter)
    List.Add(Zone.Pages);
    exit;
  end;

  // go recursively through all child zones
  if (Zone.Parent<>nil) and (Zone.Orientation in [doVertical,doHorizontal])
  and (Zone.FirstChild<>nil) then
  begin
    if Side in [akLeft,akTop] then
      FindBorderControls(Zone.FirstChild as TLazDockZone,Side,List)
    else
      FindBorderControls(Zone.GetLastChild as TLazDockZone,Side,List);
  end;
end;

function TLazDockTree.FindBorderControl(Zone: TLazDockZone; Side: TAnchorKind
  ): TControl;
var
  List: TFPList;
begin
  Result:=nil;
  if Zone=nil then exit;
  List:=nil;
  FindBorderControls(Zone,Side,List);
  if (List=nil) or (List.Count=0) then
    Result:=DockSite
  else
    Result:=TControl(List[0]);
  List.Free;
end;

function TLazDockTree.GetAnchorControl(Zone: TDockZone; Side: TAnchorKind
  ): TControl;
// find a control to anchor the Zone's Side
begin
  Result:=DockSite;
  if (Zone=nil) or (Zone.Parent=nil) then exit;
  case Zone.Parent.Orientation of
  doHorizontal:
    if (Side=akLeft) and (Zone.PrevSibling<>nil) then
      Result:=FindBorderControl(Zone.PrevSibling as TLazDockZone,akRight)
    else if (Side=akRight) and (Zone.NextSibling<>nil) then
      Result:=FindBorderControl(Zone.NextSibling as TLazDockZone,akLeft)
    else
      Result:=GetAnchorControl(Zone.Parent,Side);
  doVertical:
    if (Side=akTop) and (Zone.PrevSibling<>nil) then
      Result:=FindBorderControl(Zone.PrevSibling as TLazDockZone,akBottom)
    else if (Side=akBottom) and (Zone.NextSibling<>nil) then
      Result:=FindBorderControl(Zone.NextSibling as TLazDockZone,akTop)
    else
      Result:=GetAnchorControl(Zone.Parent,Side);
  doPages:
    Result:=GetAnchorControl(Zone.Parent,Side);
  end;
end;

{ TLazDockZone }

destructor TLazDockZone.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSplitter);
  FreeAndNil(FPages);
end;

end.

