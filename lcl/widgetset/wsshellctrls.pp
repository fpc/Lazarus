{
 *****************************************************************************
 *                              WSShellCtrls.pp                              * 
 *                              -------------                                * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit WSShellCtrls;

{$mode objfpc}{$H+}
{$I lcl_defines.inc}

interface

////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// 1) Only class methods allowed
// 2) Class methods have to be published and virtual
// 3) To get as little as posible circles, the uses
//    clause should contain only those LCL units 
//    needed for registration. WSxxx units are OK
// 4) To improve speed, register only classes in the 
//    initialization section which actually 
//    implement something
// 5) To enable your XXX widgetset units, look at
//    the uses clause of the XXXintf.pp
////////////////////////////////////////////////////
uses
  Classes, Types,
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  ShellCtrls, ComCtrls,
////////////////////////////////////////////////////
  WSControls, WSFactory, WSLCLClasses, WSComCtrls;

type

  { TWSCustomShellTreeView }

  TWSCustomShellTreeView = class(TWSCustomTreeView)
  published
    class function DrawBuiltInIcon(ATreeView: TCustomShellTreeView;
      ANode: TTreeNode; ARect: TRect): TSize; virtual;
    class function GetBuiltinIconSize: TSize; virtual;
  end;
  TWSCustomShellTreeViewClass = class of TWSCustomShellTreeView;

  { TWSCustomShellListView }

  TWSCustomShellListView = class(TWSCustomListView)
  published
    class function GetBuiltInImageIndex(AListView: TCustomShellListView;
      const AFileName: String; ALargeImage: Boolean): Integer; virtual;
  end;
  TWSCustomShellListViewClass = class of TWSCustomShellListView;

procedure RegisterCustomShellTreeView;
procedure RegisterCustomShellListView;


implementation

uses
  LResources;

{ TWSCustomShellTreeView }

class function TWSCustomShellTreeView.DrawBuiltInIcon(ATreeView: TCustomShellTreeView;
  ANode: TTreeNode; ARect: TRect): TSize;
begin
  Result.CX := 0;
  Result.CY := 0;
end;

class function TWSCustomShellTreeView.GetBuiltinIconSize: TSize;
begin
  Result.CX := 0;
  Result.CY := 0;
end;


{ TWSCustomShellListView }

class function TWSCustomShellListView.GetBuiltInImageIndex(
  AListView: TCustomShellListView; const AFileName: String;
  ALargeImage: Boolean): Integer;
begin
  Result := -1;
end;


{ Registration }

procedure RegisterCustomShellTreeView;
const
  Done: Boolean = False;
begin
  if Done then exit;
  //WSRegisterCustomShellTreeView;
  if not WSRegisterCustomShellTreeView then
    RegisterWSComponent(TCustomShellTreeView, TWSCustomShellTreeView);
  Done := True;
end;

procedure RegisterCustomShellListView;
const
  Done: Boolean = False;
begin
  if Done then exit;
//  WSRegisterCustomShellListView;
  if not WSRegisterCustomShellListView then
    RegisterWSComponent(TCustomShellListView, TWSCustomShellListView);
  Done := True;
end;

end.
