{ $Id: cocoawsextctrls.pp 15459 2008-06-18 20:23:05Z sekelsenmat $}
{
 *****************************************************************************
 *                              CocoaWSExtCtrls.pp                           *
 *                              -------------------                          *
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CocoaWSExtCtrls;

{$mode delphi}
{$modeswitch objectivec1}

interface

uses
  // libs
  MacOSAll, CocoaAll,
  // LCL
  Classes, Controls, ExtCtrls, LCLType, LCLProc, Graphics, Math, SysUtils,
  // widgetset
  WSExtCtrls, WSLCLClasses, WSControls, WSProc,
  // LCL Cocoa
  CocoaPrivate, CocoaGDIObjects;

type

  { TCocoaWSPage }

  TCocoaWSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TCocoaWSNotebook }

  TCocoaWSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TCocoaWSShape }

  TCocoaWSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TCocoaWSCustomSplitter }

  TCocoaWSCustomSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  end;

  { TCocoaWSSplitter }

  TCocoaWSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TCocoaWSPaintBox }

  TCocoaWSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TCocoaWSCustomImage }

  TCocoaWSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TCocoaWSImage }

  TCocoaWSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TCocoaWSBevel }

  TCocoaWSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TCocoaWSCustomRadioGroup }

  TCocoaWSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TCocoaWSRadioGroup }

  TCocoaWSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TCocoaWSCustomCheckGroup }

  TCocoaWSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TCocoaWSCheckGroup }

  TCocoaWSCheckGroup = class(TWSCheckGroup)
  private
  protected
  public
  end;

  { TCocoaWSCustomLabeledEdit }

  TCocoaWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TCocoaWSLabeledEdit }

  TCocoaWSLabeledEdit = class(TWSLabeledEdit)
  private
  protected
  public
  end;

  { TCocoaWSCustomPanel }

  TCocoaWSCustomPanel = class(TWSCustomPanel)
  private
  protected
  public
  end;

  { TCocoaWSPanel }

  TCocoaWSPanel = class(TWSPanel)
  private
  protected
  public
  end;

  { TCocoaWSCustomTrayIcon }

  TCocoaWSCustomTrayIcon = class(TWSCustomTrayIcon)
  public
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
  end;

implementation

{$include cocoatrayicon.inc}

end.

