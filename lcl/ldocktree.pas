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
  TLazDockPage = class;
  TLazDockSplitter = class;

  { TLazDockZone }

  TLazDockZone = class(TDockZone)
  private
    FPage: TLazDockPage;
    FPages: TLazDockPages;
    FSplitter: TLazDockSplitter;
  public
    destructor Destroy; override;
    function GetCaption: string;
    function GetParentControl: TWinControl;
    property Splitter: TLazDockSplitter read FSplitter write FSplitter;
    property Pages: TLazDockPages read FPages write FPages;
    property Page: TLazDockPage read FPage write FPage;
  end;

  { TLazDockTree }

  TLazDockTree = class(TDockTree)
  private
    FAutoFreeDockSite: boolean;
  protected
    procedure UndockControlForDocking(AControl: TControl);
    procedure BreakAnchors(Zone: TDockZone);
    procedure CreateDockLayoutHelperControls(Zone: TLazDockZone);
    procedure AnchorDockLayout(Zone: TLazDockZone);
  public
    constructor Create(TheDockSite: TWinControl); override;
    destructor Destroy; override;
    procedure InsertControl(AControl: TControl; InsertAt: TAlign;
                            DropControl: TControl); override;
    procedure BuildDockLayout(Zone: TLazDockZone);
    procedure FindBorderControls(Zone: TLazDockZone; Side: TAnchorKind;
                                 var List: TFPList);
    function FindBorderControl(Zone: TLazDockZone; Side: TAnchorKind): TControl;
    function GetAnchorControl(Zone: TLazDockZone; Side: TAnchorKind;
                              OutSide: boolean): TControl;
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

procedure TLazDockTree.CreateDockLayoutHelperControls(Zone: TLazDockZone);
var
  ParentPages: TLazDockPages;
  ZoneIndex: LongInt;
begin
  if Zone=nil then exit;

  // create needed TLazDockSplitter
  if (Zone.Parent<>nil)
  and (Zone.Parent.Orientation in [doVertical,doHorizontal])
  and (Zone.PrevSibling<>nil) then begin
    // a zone with a side sibling -> needs a TLazDockSplitter
    if Zone.Splitter=nil then begin
      Zone.Splitter:=TLazDockSplitter.Create(nil);
    end;
  end else if Zone.Splitter<>nil then begin
    // zone no longer needs the splitter
    Zone.Splitter.Free;
    Zone.Splitter:=nil;
  end;

  // create needed TLazDockPages
  if (Zone.Orientation=doPages) then begin
    // a zone of pages -> needs a TLazDockPages
    if Zone.FirstChild=nil then
      RaiseGDBException('TLazDockTree.CreateDockLayoutHelperControls Inconsistency: doPages without childs');
    if (Zone.Pages=nil) then begin
      Zone.Pages:=TLazDockPages.Create(nil);
    end;
  end else if Zone.Pages<>nil then begin
    // zone no longer needs the pages
    Zone.Pages.Free;
    Zone.Pages:=nil;
  end;

  // create needed TLazDockPage
  if (Zone.Parent<>nil)
  and (Zone.Parent.Orientation=doPages) then begin
    // a zone as page -> needs a TLazDockPage
    if (Zone.Page=nil) then begin
      ParentPages:=TLazDockZone(Zone.Parent).Pages;
      ZoneIndex:=Zone.GetIndex;
      ParentPages.Pages.Insert(ZoneIndex,Zone.GetCaption);
      Zone.Page:=ParentPages.Page[ZoneIndex];
    end;
  end else if Zone.Page<>nil then begin
    // zone no longer needs the page
    Zone.Page.Free;
    Zone.Page:=nil;
  end;

  // create controls for childs and siblings
  CreateDockLayoutHelperControls(Zone.FirstChild as TLazDockZone);
  CreateDockLayoutHelperControls(Zone.NextSibling as TLazDockZone);
end;

procedure TLazDockTree.AnchorDockLayout(Zone: TLazDockZone);
// setup all anchors between all docked controls and helper controls
var
  AnchorControls: array[TAnchorKind] of TControl;
  a: TAnchorKind;
  SplitterSide: TAnchorKind;
  CurControl: TControl;
  NewAnchors: TAnchors;
begin
  if Zone=nil then exit;
  
  // get outside anchor controls
  for a:=Low(TAnchorKind) to High(TAnchorKind) do
    AnchorControls[a]:=GetAnchorControl(Zone,a,true);

  // anchor splitter
  if (Zone.Splitter<>nil) then begin
    if Zone.Parent.Orientation=doHorizontal then
      SplitterSide:=akLeft
    else
      SplitterSide:=akTop;
    // IMPORTANT: first set the AnchorSide, then set the Anchors
    NewAnchors:=[akLeft,akRight,akTop,akBottom]-[SplitterSide];
    for a:=Low(TAnchorKind) to High(TAnchorKind) do
      if a in NewAnchors then
        Zone.Splitter.AnchorSide[a].Control:=AnchorControls[a];
    Zone.Splitter.Anchors:=NewAnchors;
    AnchorControls[SplitterSide]:=Zone.Splitter;
  end;
  
  // anchor pages
  if Zone.Pages<>nil then
    CurControl:=Zone.Pages
  else
    CurControl:=Zone.ChildControl;
  if CurControl<>nil then begin
    // IMPORTANT: first set the AnchorSide, then set the Anchors
    for a:=Low(TAnchorKind) to High(TAnchorKind) do
      CurControl.AnchorSide[a].Control:=AnchorControls[a];
    CurControl.Anchors:=[akLeft,akRight,akTop,akBottom];
  end;

  // anchor controls for childs and siblings
  AnchorDockLayout(Zone.FirstChild as TLazDockZone);
  AnchorDockLayout(Zone.NextSibling as TLazDockZone);
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
  ASibling: TDockZone;
begin
  if DropControl=nil then
    DropControl:=DockSite;
  DropZone:=RootZone.FindZone(DropControl);
  if DropZone=nil then
    raise Exception.Create('TLazDockTree.InsertControl DropControl is not part of this TDockTree');

  NewOrientation:=DockAlignOrientations[InsertAt];

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
    // resize control
    RaiseGDBException('TLazDockTree.InsertControl TODO resize control');
    if NewOrientation in [doHorizontal,doVertical] then begin
      ASibling:=NewZone.PrevSibling;
      if ASibling=nil then ASibling:=NewZone.NextSibling;
      if ASibling<>nil then begin
        if NewOrientation=doHorizontal then
          AControl.Height:=ASibling.Height
        else
          AControl.Width:=ASibling.Width;
      end;
    end;
    AControl.Parent:=NewZone.GetParentControl;

    // Build dock layout (anchors, splitters, pages)
    BuildDockLayout(RootZone as TLazDockZone);
  end;
end;

procedure TLazDockTree.BuildDockLayout(Zone: TLazDockZone);
begin
  BreakAnchors(Zone);
  CreateDockLayoutHelperControls(Zone);
  AnchorDockLayout(Zone);
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

function TLazDockTree.GetAnchorControl(Zone: TLazDockZone; Side: TAnchorKind;
  OutSide: boolean): TControl;
// find a control to anchor the Zone's Side
begin
  if Zone=nil then begin
    Result:=DockSite;
    exit;
  end;

  if not OutSide then begin
    // also check the Splitter and the Page
    if (Side=akLeft)
    and (Zone.Parent<>nil) and (Zone.Parent.Orientation=doHorizontal)
    and (Zone.Splitter<>nil) then begin
      Result:=Zone.Splitter;
      exit;
    end;
    if (Side=akTop)
    and (Zone.Parent<>nil) and (Zone.Parent.Orientation=doVertical)
    and (Zone.Splitter<>nil) then begin
      Result:=Zone.Splitter;
      exit;
    end;
    if (Zone.Page<>nil) then begin
      Result:=Zone.Page;
      exit;
    end;
  end;

  // search the neigbour zones:
  Result:=DockSite;
  if (Zone.Parent=nil) then exit;
  case Zone.Parent.Orientation of
  doHorizontal:
    if (Side=akLeft) and (Zone.PrevSibling<>nil) then
      Result:=FindBorderControl(Zone.PrevSibling as TLazDockZone,akRight)
    else if (Side=akRight) and (Zone.NextSibling<>nil) then
      Result:=FindBorderControl(Zone.NextSibling as TLazDockZone,akLeft)
    else
      Result:=GetAnchorControl(Zone.Parent as TLazDockZone,Side,false);
  doVertical:
    if (Side=akTop) and (Zone.PrevSibling<>nil) then
      Result:=FindBorderControl(Zone.PrevSibling as TLazDockZone,akBottom)
    else if (Side=akBottom) and (Zone.NextSibling<>nil) then
      Result:=FindBorderControl(Zone.NextSibling as TLazDockZone,akTop)
    else
      Result:=GetAnchorControl(Zone.Parent as TLazDockZone,Side,false);
  doPages:
    Result:=GetAnchorControl(Zone.Parent as TLazDockZone,Side,false);
  end;
end;

{ TLazDockZone }

destructor TLazDockZone.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSplitter);
  FreeAndNil(FPage);
  FreeAndNil(FPages);
end;

function TLazDockZone.GetCaption: string;
begin
  if ChildControl<>nil then
    Result:=ChildControl.Caption
  else
    Result:=IntToStr(GetIndex);
end;

function TLazDockZone.GetParentControl: TWinControl;
var
  Zone: TDockZone;
begin
  Result:=nil;
  Zone:=Parent;
  while Zone<>nil do begin
    if Zone.Orientation=doPages then begin
      Result:=(Zone as TLazDockZone).Pages;
      exit;
    end;
    if (Zone.Parent=nil) then begin
      if Zone.ChildControl is TWinControl then
        Result:=TWinControl(Zone.ChildControl);
      exit;
    end;
    Zone:=Zone.Parent;
  end;
end;

end.

