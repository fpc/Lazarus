{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSComCtrls.pp                              * 
 *                              ---------------                              * 
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
unit CarbonWSComCtrls;

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

  { TCarbonWSStatusBar }

  TCarbonWSStatusBar = class(TWSStatusBar)
  private
  protected
  public
  end;

  { TCarbonWSTabSheet }

  TCarbonWSTabSheet = class(TWSTabSheet)
  private
  protected
  public
  end;

  { TCarbonWSPageControl }

  TCarbonWSPageControl = class(TWSPageControl)
  private
  protected
  public
  end;

  { TCarbonWSCustomListView }

  TCarbonWSCustomListView = class(TWSCustomListView)
  private
  protected
  public
  end;

  { TCarbonWSListView }

  TCarbonWSListView = class(TWSListView)
  private
  protected
  public
  end;

  { TCarbonWSProgressBar }

  TCarbonWSProgressBar = class(TWSProgressBar)
  private
  protected
  public
  end;

  { TCarbonWSCustomUpDown }

  TCarbonWSCustomUpDown = class(TWSCustomUpDown)
  private
  protected
  public
  end;

  { TCarbonWSUpDown }

  TCarbonWSUpDown = class(TWSUpDown)
  private
  protected
  public
  end;

  { TCarbonWSToolButton }

  TCarbonWSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TCarbonWSToolBar }

  TCarbonWSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TCarbonWSToolButton }

  TCarbonWSToolButton = class(TWSToolButton)
  private
  protected
  public
  end;

  { TCarbonWSToolBar }

  TCarbonWSToolBar = class(TWSToolBar)
  private
  protected
  public
  end;

  { TCarbonWSTrackBar }

  TCarbonWSTrackBar = class(TWSTrackBar)
  private
  protected
  public
  end;

  { TCarbonWSCustomTreeView }

  TCarbonWSCustomTreeView = class(TWSCustomTreeView)
  private
  protected
  public
  end;

  { TCarbonWSTreeView }

  TCarbonWSTreeView = class(TWSTreeView)
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
//  RegisterWSComponent(TCustomStatusBar, TCarbonWSStatusBar);
//  RegisterWSComponent(TCustomTabSheet, TCarbonWSTabSheet);
//  RegisterWSComponent(TCustomPageControl, TCarbonWSPageControl);
//  RegisterWSComponent(TCustomListView, TCarbonWSCustomListView);
//  RegisterWSComponent(TCustomListView, TCarbonWSListView);
//  RegisterWSComponent(TCustomProgressBar, TCarbonWSProgressBar);
//  RegisterWSComponent(TCustomUpDown, TCarbonWSCustomUpDown);
//  RegisterWSComponent(TCustomUpDown, TCarbonWSUpDown);
//  RegisterWSComponent(TCustomToolButton, TCarbonWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TCarbonWSToolBar);
//  RegisterWSComponent(TCustomToolButton, TCarbonWSToolButton);
//  RegisterWSComponent(TCustomToolBar, TCarbonWSToolBar);
//  RegisterWSComponent(TCustomTrackBar, TCarbonWSTrackBar);
//  RegisterWSComponent(TCustomTreeView, TCarbonWSCustomTreeView);
//  RegisterWSComponent(TCustomTreeView, TCarbonWSTreeView);
////////////////////////////////////////////////////
end.