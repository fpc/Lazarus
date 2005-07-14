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
  
  TLazDockZone = class(TDockZone)
  end;

  { TLazDockTree }

  TLazDockTree = class(TDockTree)
  private
    FAutoFreeDockSite: boolean;
  protected
    procedure UndockControlForDocking(AControl: TControl);
    procedure BreakParentAnchors(Zone: TDockZone);
  public
    constructor Create(TheDockSite: TWinControl); override;
    destructor Destroy; override;
    procedure InsertControl(AControl: TControl; InsertAt: TAlign;
                            DropControl: TControl); override;
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

procedure TLazDockTree.BreakParentAnchors(Zone: TDockZone);
begin

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
var
  DropZone: TDockZone;
  NewZone: TDockZone;
  NewOrientation: TDockOrientation;
  NeedNewParentZone: Boolean;
  NewParentZone: TDockZone;
  OldParentZone: TDockZone;
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
  NewZone:=DockZoneClass.Create(Self,AControl);
  
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

    // break anchors to parent
    BreakParentAnchors(RootZone);

    // break anchors at insert position

    
    
    // resize DockSite
    {if InsertAt in [alLeft,alRight] then
      DockSite.Width:=DockSite.Width+NewSplitter.Width+AControl.Width
    else if InsertAt in [alTop,alBottom] then
      DockSite.Width:=DockSite.Height+NewSplitter.Height+AControl.Width
    else if InsertAt in [alNone,alClient] then begin
      // TODO
      RaiseGDBException('TLazDockTree.InsertControl TODO: InsertAt in [alNone,alClient]');
    end;}

    // add splitter or page control
    //NewSplitter:=TSplitter.Create(DockSite);

    // add control to DockSite
    
    // anchor
  end;
end;

end.

