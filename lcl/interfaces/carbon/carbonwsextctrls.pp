{ $Id$}
{
 *****************************************************************************
 *                              CarbonWSExtCtrls.pp                          *
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
unit CarbonWSExtCtrls;

{$mode delphi}

interface

// defines
{$I carbondefines.inc}

uses
  // libs
  MacOSAll,
  // Cocoa
  {$ifdef CarbonUseCocoa}
  appkit, foundation, lobjc,
  {$endif CarbonUseCocoa}
  // LCL
  Classes, Controls, ExtCtrls, LCLType, LCLProc, Graphics, Math, SysUtils,
  Menus, ComCtrls,
  // widgetset
  WSExtCtrls, WSLCLClasses, WSControls, WSProc, WSComCtrls,
  // LCL Carbon
  carbongdiobjects, CarbonWSControls;

type

  { TCarbonWSPage }

  TCarbonWSPage = class(TWSPage)
  published
  end;

  { TCarbonWSNotebook }

  TCarbonWSNotebook = class(TWSNotebook)
  published
  end;

  { TCarbonWSShape }

  TCarbonWSShape = class(TWSShape)
  published
  end;

  { TCarbonWSCustomSplitter }

  TCarbonWSCustomSplitter = class(TWSCustomSplitter)
  published
  end;

  { TCarbonWSSplitter }

  TCarbonWSSplitter = class(TWSSplitter)
  published
  end;

  { TCarbonWSPaintBox }

  TCarbonWSPaintBox = class(TWSPaintBox)
  published
  end;

  { TCarbonWSCustomImage }

  TCarbonWSCustomImage = class(TWSCustomImage)
  published
  end;

  { TCarbonWSImage }

  TCarbonWSImage = class(TWSImage)
  published
  end;

  { TCarbonWSBevel }

  TCarbonWSBevel = class(TWSBevel)
  published
  end;

  { TCarbonWSCustomRadioGroup }

  TCarbonWSCustomRadioGroup = class(TWSCustomRadioGroup)
  published
  end;

  { TCarbonWSRadioGroup }

  TCarbonWSRadioGroup = class(TWSRadioGroup)
  published
  end;

  { TCarbonWSCustomCheckGroup }

  TCarbonWSCustomCheckGroup = class(TWSCustomCheckGroup)
  published
  end;

  { TCarbonWSCheckGroup }

  TCarbonWSCheckGroup = class(TWSCheckGroup)
  published
  end;

  { TCarbonWSCustomLabeledEdit }

  TCarbonWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  published
  end;

  { TCarbonWSLabeledEdit }

  TCarbonWSLabeledEdit = class(TWSLabeledEdit)
  published
  end;

  { TCarbonWSCustomPanel }

  TCarbonWSCustomPanel = class(TWSCustomPanel)
  published
    class procedure GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
  end;

  { TCarbonWSPanel }

  TCarbonWSPanel = class(TWSPanel)
  published
  end;

  { TCarbonWSCustomTrayIcon }

  TCarbonWSCustomTrayIcon = class(TWSCustomTrayIcon)
  published
    {$ifdef CarbonUseCocoa}
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
    class function IsTrayIconMenuVisible(const ATrayIcon: TCustomTrayIcon): Boolean;
    {$endif CarbonUseCocoa}
  end;

implementation

uses
  CarbonProc, CarbonDef, CarbonCanvas;

{ TCarbonWSCustomPanel }

{------------------------------------------------------------------------------
  Method:  TCarbonWSCustomNotebook.ShowTabs
  Params:  ANotebook - LCL custom notebook
           AShowTabs - Tabs visibility

  TCustomPanel should return preferred size (0,0)
  bugs #16337, and Mattias in comment 0036957 of #16323
 ------------------------------------------------------------------------------}
class procedure TCarbonWSCustomPanel.GetPreferredSize(const AWinControl: TWinControl; var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  PreferredWidth:=0;
  PreferredHeight:=0;
end;


{$include carbontrayicon.inc}

end.

