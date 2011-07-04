{
 *****************************************************************************
 *                            Win32WSExtCtrls.pp                             *
 *                            ------------------                             *
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
unit Win32WSExtCtrls;

{$mode objfpc}{$H+}
{$I win32defines.inc}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
// rtl
  Windows, CommCtrl, SysUtils, Classes,
// lcl
  ExtCtrls, Controls, ImgList, LCLType, LCLIntf, LCLProc, Themes, LCLMessageGlue, ComCtrls, WSComCtrls,
// ws
  WSControls, WSExtCtrls, WSLCLClasses, WSProc, Win32Extra, Win32Int, Win32Proc,
  InterfaceBase, Win32WSControls;

type
  { TWin32WSPage }

  TWin32WSPage = class(TWSPage)
  published
  end;

  { TWin32WSNotebook }

  TWin32WSNotebook = class(TWSNotebook)
  published
  end;

  { TWin32WSShape }

  TWin32WSShape = class(TWSShape)
  published
  end;

  { TWin32WSCustomSplitter }

  TWin32WSCustomSplitter = class(TWSCustomSplitter)
  published
  end;

  { TWin32WSSplitter }

  TWin32WSSplitter = class(TWSSplitter)
  published
  end;

  { TWin32WSPaintBox }

  TWin32WSPaintBox = class(TWSPaintBox)
  published
  end;

  { TWin32WSCustomImage }

  TWin32WSCustomImage = class(TWSCustomImage)
  published
  end;

  { TWin32WSImage }

  TWin32WSImage = class(TWSImage)
  published
  end;

  { TWin32WSBevel }

  TWin32WSBevel = class(TWSBevel)
  published
  end;

  { TWin32WSCustomRadioGroup }

  TWin32WSCustomRadioGroup = class(TWSCustomRadioGroup)
  published
  end;

  { TWin32WSRadioGroup }

  TWin32WSRadioGroup = class(TWSRadioGroup)
  published
  end;

  { TWin32WSCustomCheckGroup }

  TWin32WSCustomCheckGroup = class(TWSCustomCheckGroup)
  published
  end;

  { TWin32WSCheckGroup }

  TWin32WSCheckGroup = class(TWSCheckGroup)
  published
  end;

  { TWin32WSCustomLabeledEdit }

  TWin32WSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  published
  end;

  { TWin32WSLabeledEdit }

  TWin32WSLabeledEdit = class(TWSLabeledEdit)
  published
  end;

  { TWin32WSCustomPanel }

  TWin32WSCustomPanel = class(TWSCustomPanel)
  published
  end;

  { TWin32WSPanel }

  TWin32WSPanel = class(TWSPanel)
  published
  end;

  { TWin32WSCustomTrayIcon }

  TWin32WSCustomTrayIcon = class(TWSCustomTrayIcon)
  protected
    class function AddIcon(ATrayIcon: TCustomTrayIcon): Boolean;
  published
    class function Hide(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function Show(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class procedure InternalUpdate(const ATrayIcon: TCustomTrayIcon); override;
    class function ShowBalloonHint(const ATrayIcon: TCustomTrayIcon): Boolean; override;
    class function GetPosition(const ATrayIcon: TCustomTrayIcon): TPoint; override;
  end;

implementation

uses
  Forms, LMessages, ShellAPI;

{$include win32trayicon.inc}

end.
