{ $Id$}
{
 *****************************************************************************
 *                            win32wscomctrls.pp                             * 
 *                            ------------------                             * 
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
unit win32wscomctrls;

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
  wscomctrls, wslclclasses;

type

  { TWin32WSStatusBar }

  TWin32WSStatusBar = class(TWSStatusBar)
  private
  protected
  public
  end;

  { TWin32WSTabSheet }

  TWin32WSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TWin32WSPageControl }

  TWin32WSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TWin32WSCustomListView }

  TWin32WSCustomListView = class(TWSCustomListView)
  private
  protected
  public
  end;

  { TWin32WSListView }

  TWin32WSListView = class(TWSListView)
  private
  protected
  public
  end;

  { TWin32WSProgressBar }

  TWin32WSProgressBar = class(TWSProgressBar)
  private
  protected
  public
  end;

  { TWin32WSCustomUpDown }

  TWin32WSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TWin32WSUpDown }

  TWin32WSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TWin32WSToolButton }

  TWin32WSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TWin32WSToolBar }

  TWin32WSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TWin32WSToolButton }

  TWin32WSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TWin32WSToolBar }

  TWin32WSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TWin32WSTrackBar }

  TWin32WSTrackBar = class(TWSTrackBar)
  private
  protected
  public
  end;

  { TWin32WSCustomTreeView }

  TWin32WSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TWin32WSTreeView }

  TWin32WSTreeView = class(TWSTreeView)
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
//  RegisterWSComponent(TStatusBar, TWin32WSStatusBar);
//  RegisterWSComponent(TTabSheet, TWin32WSTabSheet);
//  RegisterWSComponent(TPageControl, TWin32WSPageControl);
//  RegisterWSComponent(TCustomListView, TWin32WSCustomListView);
//  RegisterWSComponent(TListView, TWin32WSListView);
//  RegisterWSComponent(TProgressBar, TWin32WSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TWin32WSCustomUpDown);
//  RegisterWSComponent(TUpDown, TWin32WSUpDown);
//  RegisterWSComponent(TToolButton, TWin32WSToolButton);
//  RegisterWSComponent(TToolBar, TWin32WSToolBar);
//  RegisterWSComponent(TToolButton, TWin32WSToolButton);
//  RegisterWSComponent(TToolBar, TWin32WSToolBar);
//  RegisterWSComponent(TTrackBar, TWin32WSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TWin32WSCustomTreeView);
//  RegisterWSComponent(TTreeView, TWin32WSTreeView);
////////////////////////////////////////////////////
end.
