{ $Id$}
{
 *****************************************************************************
 *                               WSComCtrls.pp                               * 
 *                               -------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
}
unit WSComCtrls;

{$mode objfpc}{$H+}

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
  Classes,
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
  ComCtrls,
////////////////////////////////////////////////////
  WSLCLClasses, WSControls, WSExtCtrls, WSStdCtrls,
  WSToolwin;

type
  { TWSStatusBar }

  TWSStatusBar = class(TWSWinControl)
  end;

  { TWSTabSheet }

  TWSTabSheet = class(TWSCustomPage)
  end;

  { TWSPageControl }

  TWSPageControl = class(TWSCustomNotebook)
  end;

  { TWSCustomListView }
  TWSListViewItemChange = (lvicText, lvicImage);
  TWSListViewItemChanges = set of TWSListViewItemChange;

  TWSCustomListViewClass = class of TWSCustomListView;
  TWSCustomListView = class(TWSWinControl)
    class procedure ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); virtual;
    class function  ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer; virtual;
    class procedure ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); virtual;
    class procedure ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); virtual;
    class procedure ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment); virtual;
    class procedure ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean); virtual;
    class procedure ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String); virtual;
    class procedure ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer); virtual;
    class procedure ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer); virtual;
    class procedure ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer); virtual;
    class procedure ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer); virtual;
    class procedure ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean); virtual;
    
    class procedure ItemDelete(const ALV: TCustomListView; const AIndex: Integer); virtual;
    class function  ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; var AIsSet: Boolean): Boolean; virtual; // returns True if supported
    class procedure ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); virtual;
    class procedure ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); virtual;
    class procedure ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); virtual;
    class procedure ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); virtual;
    class procedure ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); virtual;
  end;

  { TWSListView }                             

  TWSListView = class(TWSCustomListView)
  end;

  { TWSProgressBar }

  TWSProgressBar = class(TWSWinControl)
  end;

  { TWSCustomUpDown }

  TWSCustomUpDown = class(TWSCustomControl)
  end;

  { TWSUpDown }

  TWSUpDown = class(TWSCustomUpDown)
  end;

  { TWSToolButton }

  TWSToolButton = class(TWSCustomControl)
  end;

  { TWSToolBar }

  TWSToolBar = class(TWSToolWindow)
  end;

  { TWSTrackBar }

  TWSTrackBar = class(TWSWinControl)
  end;

  { TWSCustomTreeView }

  TWSCustomTreeView = class(TWSCustomControl)
  end;

  { TWSTreeView }

  TWSTreeView = class(TWSCustomTreeView)
  end;


implementation

uses
  // TODO: remove when implemented on win32
  Controls, LMessages;


  { TWSCustomListView }

procedure TWSCustomListView.ColumnDelete(const ALV: TCustomListView; const AIndex: Integer); 
begin
end;

function TWSCustomListView.ColumnGetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn): Integer; 
begin
  Result := -1;
end;

procedure TWSCustomListView.ColumnInsert(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn); 
begin
end;

procedure TWSCustomListView.ColumnMove(const ALV: TCustomListView; const AOldIndex, ANewIndex: Integer; const AColumn: TListColumn); 
begin
end;

procedure TWSCustomListView.ColumnSetAlignment(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAlignment: TAlignment); 
begin
end;

procedure TWSCustomListView.ColumnSetAutoSize(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AAutoSize: Boolean); 
begin
end;

procedure TWSCustomListView.ColumnSetCaption(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const ACaption: String); 
begin
end;

procedure TWSCustomListView.ColumnSetImage(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AImageIndex: Integer);
begin
end;

procedure TWSCustomListView.ColumnSetMaxWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMaxWidth: Integer);
begin
end;

procedure TWSCustomListView.ColumnSetMinWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AMinWidth: integer);
begin
end;

procedure TWSCustomListView.ColumnSetWidth(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AWidth: Integer);
begin
end;

procedure TWSCustomListView.ColumnSetVisible(const ALV: TCustomListView; const AIndex: Integer; const AColumn: TListColumn; const AVisible: Boolean);
begin
end;

procedure TWSCustomListView.ItemDelete(const ALV: TCustomListView; const AIndex: Integer); 
begin
  // TODO: remove when implemented on win32
  CNSendMessage(LM_LV_DELETEITEM, ALV ,@AIndex);
end;

function TWSCustomListView.ItemGetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; var AIsSet: Boolean): Boolean; 
begin
  // returns True if supported
  Result := False;
end;

procedure TWSCustomListView.ItemInsert(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); 
begin
  // TODO: remove when implemented on win32
  CNSendMessage(LM_LV_ADDITEM, ALV, @AIndex);
end;

procedure TWSCustomListView.ItemSetImage(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex, AImageIndex: Integer); 
begin
end;

procedure TWSCustomListView.ItemSetState(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const AState: TListItemState; const AIsSet: Boolean); 
begin
end;

procedure TWSCustomListView.ItemSetText(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem; const ASubIndex: Integer; const AText: String); 
begin
end;

procedure TWSCustomListView.ItemShow(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem);
begin
end;


initialization

////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TStatusBar, TWSStatusBar);
//  RegisterWSComponent(TTabSheet, TWSTabSheet);
//  RegisterWSComponent(TPageControl, TWSPageControl);
  RegisterWSComponent(TCustomListView, TWSCustomListView);
//  RegisterWSComponent(TListView, TWSListView);
//  RegisterWSComponent(TProgressBar, TWSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TWSCustomUpDown);
//  RegisterWSComponent(TUpDown, TWSUpDown);
//  RegisterWSComponent(TToolButton, TWSToolButton);
//  RegisterWSComponent(TToolBar, TWSToolBar);
//  RegisterWSComponent(TToolButton, TWSToolButton);
//  RegisterWSComponent(TToolBar, TWSToolBar);
//  RegisterWSComponent(TTrackBar, TWSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TWSCustomTreeView);
//  RegisterWSComponent(TTreeView, TWSTreeView);
////////////////////////////////////////////////////
end.
