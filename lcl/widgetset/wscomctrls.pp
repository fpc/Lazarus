{ $Id$}
{
 *****************************************************************************
 *                               wscomctrls.pp                               * 
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
unit wscomctrls;

{$mode objfpc}{H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as litle as posible circles,
// Uncomment only when needed for registration
////////////////////////////////////////////////////
//  comctrls,
////////////////////////////////////////////////////
  wslclclasses, wscontrols, wsextctrls, wsstdctrls,
  wstoolwin;

type

  { TWSStatusBar }

  TWSStatusBar = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWSTabSheet }

  TWSTabSheet = class(TWSCustomPage)
  private
  protected
  public
  end;

  { TWSPageControl }

  TWSPageControl = class(TWSCustomNotebook)
  private
  protected
  public
  end;

  { TWSCustomListView }

  TWSCustomListView = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWSListView }

  TWSListView = class(TWSCustomListView)
  private
  protected
  public
  end;

  { TWSProgressBar }

  TWSProgressBar = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWSCustomUpDown }

  TWSCustomUpDown = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TWSUpDown }

  TWSUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TWSToolButton }

  TWSToolButton = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TWSToolBar }

  TWSToolBar = class(TWSToolWindow)
  private
  protected
  public
  end;

  { TWSToolButton }

  TWSToolButton = class(TWSButtonControl)
  private
  protected
  public
  end;

  { TWSToolBar }

  TWSToolBar = class(TWSToolWindow)
  private
  protected
  public
  end;

  { TWSTrackBar }

  TWSTrackBar = class(TWSWinControl)
  private
  protected
  public
  end;

  { TWSCustomTreeView }

  TWSCustomTreeView = class(TWSCustomControl)
  private
  protected
  public
  end;

  { TWSTreeView }

  TWSTreeView = class(TWSCustomTreeView)
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
//  RegisterWSComponent(TStatusBar, TWSStatusBar);
//  RegisterWSComponent(TTabSheet, TWSTabSheet);
//  RegisterWSComponent(TPageControl, TWSPageControl);
//  RegisterWSComponent(TCustomListView, TWSCustomListView);
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
