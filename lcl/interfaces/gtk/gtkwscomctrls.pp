{ $Id$}
{
 *****************************************************************************
 *                             GtkWSComCtrls.pp                              * 
 *                             ----------------                              * 
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
unit GtkWSComCtrls;

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

  { TGtkWSStatusBar }

  TGtkWSStatusBar = class(TWSStatusBar)
  private
  protected
  public
  end;

  { TGtkWSTabSheet }

  TGtkWSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TGtkWSPageControl }

  TGtkWSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TGtkWSCustomListView }

  TGtkWSCustomListView = class(TWSCustomListView)
  private
  protected
  public
  end;

  { TGtkWSListView }

  TGtkWSListView = class(TWSListView)
  private
  protected
  public
  end;

  { TGtkWSProgressBar }

  TGtkWSProgressBar = class(TWSProgressBar)
  private
  protected
  public
  end;

  { TGtkWSCustomUpDown }

  TGtkWSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TGtkWSUpDown }

  TGtkWSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TGtkWSToolButton }

  TGtkWSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TGtkWSToolBar }

  TGtkWSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TGtkWSToolButton }

  TGtkWSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TGtkWSToolBar }

  TGtkWSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TGtkWSTrackBar }

  TGtkWSTrackBar = class(TWSTrackBar)
  private
  protected
  public
  end;

  { TGtkWSCustomTreeView }

  TGtkWSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TGtkWSTreeView }

  TGtkWSTreeView = class(TWSTreeView)
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
//  RegisterWSComponent(TStatusBar, TGtkWSStatusBar);
//  RegisterWSComponent(TTabSheet, TGtkWSTabSheet);
//  RegisterWSComponent(TPageControl, TGtkWSPageControl);
//  RegisterWSComponent(TCustomListView, TGtkWSCustomListView);
//  RegisterWSComponent(TListView, TGtkWSListView);
//  RegisterWSComponent(TProgressBar, TGtkWSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TGtkWSCustomUpDown);
//  RegisterWSComponent(TUpDown, TGtkWSUpDown);
//  RegisterWSComponent(TToolButton, TGtkWSToolButton);
//  RegisterWSComponent(TToolBar, TGtkWSToolBar);
//  RegisterWSComponent(TToolButton, TGtkWSToolButton);
//  RegisterWSComponent(TToolBar, TGtkWSToolBar);
//  RegisterWSComponent(TTrackBar, TGtkWSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TGtkWSCustomTreeView);
//  RegisterWSComponent(TTreeView, TGtkWSTreeView);
////////////////////////////////////////////////////
end.
