{ $Id$}
{
 *****************************************************************************
 *                             gtk2wscomctrls.pp                             * 
 *                             -----------------                             * 
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
unit gtk2wscomctrls;

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

  { TGtk2WSStatusBar }

  TGtk2WSStatusBar = class(TWSStatusBar)
  private
  protected
  public
  end;

  { TGtk2WSTabSheet }

  TGtk2WSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TGtk2WSPageControl }

  TGtk2WSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TGtk2WSCustomListView }

  TGtk2WSCustomListView = class(TWSCustomListView)
  private
  protected
  public
  end;

  { TGtk2WSListView }

  TGtk2WSListView = class(TWSListView)
  private
  protected
  public
  end;

  { TGtk2WSProgressBar }

  TGtk2WSProgressBar = class(TWSProgressBar)
  private
  protected
  public
  end;

  { TGtk2WSCustomUpDown }

  TGtk2WSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TGtk2WSUpDown }

  TGtk2WSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TGtk2WSToolButton }

  TGtk2WSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TGtk2WSToolBar }

  TGtk2WSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TGtk2WSToolButton }

  TGtk2WSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TGtk2WSToolBar }

  TGtk2WSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TGtk2WSTrackBar }

  TGtk2WSTrackBar = class(TWSTrackBar)
  private
  protected
  public
  end;

  { TGtk2WSCustomTreeView }

  TGtk2WSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TGtk2WSTreeView }

  TGtk2WSTreeView = class(TWSTreeView)
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
//  RegisterWSComponent(TStatusBar, TGtk2WSStatusBar);
//  RegisterWSComponent(TTabSheet, TGtk2WSTabSheet);
//  RegisterWSComponent(TPageControl, TGtk2WSPageControl);
//  RegisterWSComponent(TCustomListView, TGtk2WSCustomListView);
//  RegisterWSComponent(TListView, TGtk2WSListView);
//  RegisterWSComponent(TProgressBar, TGtk2WSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TGtk2WSCustomUpDown);
//  RegisterWSComponent(TUpDown, TGtk2WSUpDown);
//  RegisterWSComponent(TToolButton, TGtk2WSToolButton);
//  RegisterWSComponent(TToolBar, TGtk2WSToolBar);
//  RegisterWSComponent(TToolButton, TGtk2WSToolButton);
//  RegisterWSComponent(TToolBar, TGtk2WSToolBar);
//  RegisterWSComponent(TTrackBar, TGtk2WSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TGtk2WSCustomTreeView);
//  RegisterWSComponent(TTreeView, TGtk2WSTreeView);
////////////////////////////////////////////////////
end.
