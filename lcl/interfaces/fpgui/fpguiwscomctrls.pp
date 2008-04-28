{ $Id: FpGuiwscomctrls.pp 5687 2004-07-16 21:49:00Z mattias $}
{
 *****************************************************************************
 *                              FpGuiWSComCtrls.pp                              * 
 *                              ---------------                              * 
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
unit FpGuiWSComCtrls;

{$mode objfpc}{$H+}

interface

uses
  // Bindings
  fpguiwsprivate,
  // LCL
  ComCtrls,
  // Widgetset
  WSComCtrls, WSLCLClasses;

type

  { TFpGuiWSStatusBar }

  TFpGuiWSStatusBar = class(TWSStatusBar)
  private
  protected
  public
  end;

  { TFpGuiWSTabSheet }

  TFpGuiWSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TFpGuiWSPageControl }

  TFpGuiWSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TFpGuiWSCustomListView }

  TFpGuiWSCustomListView = class(TWSCustomListView)
  private
  protected
  public
  end;

  { TFpGuiWSListView }

  TFpGuiWSListView = class(TWSListView)
  private
  protected
  public
  end;

  { TFpGuiWSProgressBar }

  TFpGuiWSProgressBar = class(TWSProgressBar)
  private
  protected
  public
  end;

  { TFpGuiWSCustomUpDown }

  TFpGuiWSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TFpGuiWSUpDown }

  TFpGuiWSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TFpGuiWSToolButton }

  TFpGuiWSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TFpGuiWSToolBar }

  TFpGuiWSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TFpGuiWSToolButton }

  TFpGuiWSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TFpGuiWSToolBar }

  TFpGuiWSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TFpGuiWSTrackBar }

  TFpGuiWSTrackBar = class(TWSTrackBar)
  private
  protected
  public
  end;

  { TFpGuiWSCustomTreeView }

  TFpGuiWSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TFpGuiWSTreeView }

  TFpGuiWSTreeView = class(TWSTreeView)
  private
  protected
  public
  end;


implementation

initialization

////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To improve speed, register only classes
// which actually implement something
////////////////////////////////////////////////////
//  RegisterWSComponent(TCustomStatusBar, TFpGuiWSStatusBar);
//  RegisterWSComponent(TCustomTabSheet, TFpGuiWSTabSheet);
//  RegisterWSComponent(TCustomPageControl, TFpGuiWSPageControl);
//  RegisterWSComponent(TCustomListView, TFpGuiWSCustomListView);
//  RegisterWSComponent(TCustomListView, TFpGuiWSListView);
//  RegisterWSComponent(TCustomProgressBar, TFpGuiWSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TFpGuiWSCustomUpDown);
//  RegisterWSComponent(TCustomUpDown, TFpGuiWSUpDown);
//  RegisterWSComponent(TCustomToolButton, TFpGuiWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TFpGuiWSToolBar);
//  RegisterWSComponent(TCustomToolButton, TFpGuiWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TFpGuiWSToolBar);
//  RegisterWSComponent(TCustomTrackBar, TFpGuiWSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TFpGuiWSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TFpGuiWSTreeView);
////////////////////////////////////////////////////
end.
