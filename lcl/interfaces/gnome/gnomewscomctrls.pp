{ $Id$}
{
 *****************************************************************************
 *                            GnomeWSComCtrls.pp                             * 
 *                            ------------------                             * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

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
unit GnomeWSComCtrls;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  ComCtrls,
////////////////////////////////////////////////////
  WSComCtrls, WSLCLClasses;

type

  { TGnomeWSStatusBar }

  TGnomeWSStatusBar = class(TWSStatusBar)
  private
  protected
  public
  end;

  { TGnomeWSTabSheet }

  TGnomeWSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TGnomeWSPageControl }

  TGnomeWSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TGnomeWSCustomListView }

  TGnomeWSCustomListView = class(TWSCustomListView)
  private
  protected
  public
  end;

  { TGnomeWSListView }

  TGnomeWSListView = class(TWSListView)
  private
  protected
  public
  end;

  { TGnomeWSProgressBar }

  TGnomeWSProgressBar = class(TWSProgressBar)
  private
  protected
  public
  end;

  { TGnomeWSCustomUpDown }

  TGnomeWSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TGnomeWSUpDown }

  TGnomeWSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TGnomeWSToolButton }

  TGnomeWSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TGnomeWSToolBar }

  TGnomeWSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TGnomeWSToolButton }

  TGnomeWSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TGnomeWSToolBar }

  TGnomeWSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TGnomeWSTrackBar }

  TGnomeWSTrackBar = class(TWSTrackBar)
  private
  protected
  public
  end;

  { TGnomeWSCustomTreeView }

  TGnomeWSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TGnomeWSTreeView }

  TGnomeWSTreeView = class(TWSTreeView)
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
//  RegisterWSComponent(TCustomStatusBar, TGnomeWSStatusBar);
//  RegisterWSComponent(TCustomTabSheet, TGnomeWSTabSheet);
//  RegisterWSComponent(TCustomPageControl, TGnomeWSPageControl);
//  RegisterWSComponent(TCustomListView, TGnomeWSCustomListView);
//  RegisterWSComponent(TCustomListView, TGnomeWSListView);
//  RegisterWSComponent(TCustomProgressBar, TGnomeWSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TGnomeWSCustomUpDown);
//  RegisterWSComponent(TCustomUpDown, TGnomeWSUpDown);
//  RegisterWSComponent(TCustomToolButton, TGnomeWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TGnomeWSToolBar);
//  RegisterWSComponent(TCustomToolButton, TGnomeWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TGnomeWSToolBar);
//  RegisterWSComponent(TCustomTrackBar, TGnomeWSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TGnomeWSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TGnomeWSTreeView);
////////////////////////////////////////////////////
end.