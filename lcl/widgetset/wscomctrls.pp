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
  
  TWSCustomListViewClass = class of TWSCustomListView;
  TWSCustomListView = class(TWSWinControl)
    class procedure ChangeItem(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); virtual;
    class procedure DeleteItem(const ALV: TCustomListView; const AIndex: Integer); virtual;
    class procedure InsertItem(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); virtual;
    class procedure SelectItem(const ALV: TCustomListView; const AItem: TListItem); virtual;
    class procedure ShowItem(const ALV: TCustomListView; const AItem: TListItem); virtual;
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

procedure TWSCustomListView.ChangeItem(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); 
begin
  // TODO: remove when implemented on win32
  CNSendMessage(LM_LV_CHANGEITEM, ALV, @AIndex);
end;

procedure TWSCustomListView.DeleteItem(const ALV: TCustomListView; const AIndex: Integer); 
begin
  // TODO: remove when implemented on win32
  CNSendMessage(LM_LV_DELETEITEM, ALV ,@AIndex);
end;

procedure TWSCustomListView.InsertItem(const ALV: TCustomListView; const AIndex: Integer; const AItem: TListItem); 
begin
  // TODO: remove when implemented on win32
  CNSendMessage(LM_LV_ADDITEM, ALV, @AIndex);
end;

procedure TWSCustomListView.SelectItem(const ALV: TCustomListView; const AItem: TListItem); 
begin
  // TODO: remove when implemented on win32
  CNSendMessage(LM_LV_SELECTITEM, ALV, AItem);
end;

procedure TWSCustomListView.ShowItem(const ALV: TCustomListView; const AItem: TListItem); 
begin
  // TODO: remove when implemented on win32
  CNSendMessage(LM_LV_SHOWITEM, ALV, AItem);
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
