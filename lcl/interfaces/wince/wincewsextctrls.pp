{
 *****************************************************************************
 *                            WinCEWSExtCtrls.pp                             *
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
unit WinCEWSExtCtrls;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL
  Windows, SysUtils,
  {$ifndef ver2_2_0}commctrl,{$endif}
  // Compatibility
  {$ifdef Win32}win32compat,{$endif}
  // LCL
  ExtCtrls, Classes, Controls, ImgList, Forms, LCLType, LCLIntf, LCLMessageGlue,
  LCLProc,
  // widgetset
  WSControls, WSExtCtrls, WSLCLClasses, WinCEInt, WinCEProc, InterfaceBase,
  WinCEWSControls, WSProc;

type

  { TWinCEWSPage }

  TWinCEWSPage = class(TWSPage)
  private
  protected
  public
  end;

  { TWinCEWSNotebook }

  TWinCEWSNotebook = class(TWSNotebook)
  private
  protected
  public
  end;

  { TWinCEWSShape }

  TWinCEWSShape = class(TWSShape)
  private
  protected
  public
  end;

  { TWinCEWSCustomSplitter }

  TWinCEWSCustomSplitter = class(TWSCustomSplitter)
  private
  protected
  public
  end;

  { TWinCEWSSplitter }

  TWinCEWSSplitter = class(TWSSplitter)
  private
  protected
  public
  end;

  { TWinCEWSPaintBox }

  TWinCEWSPaintBox = class(TWSPaintBox)
  private
  protected
  public
  end;

  { TWinCEWSCustomImage }

  TWinCEWSCustomImage = class(TWSCustomImage)
  private
  protected
  public
  end;

  { TWinCEWSImage }

  TWinCEWSImage = class(TWSImage)
  private
  protected
  public
  end;

  { TWinCEWSBevel }

  TWinCEWSBevel = class(TWSBevel)
  private
  protected
  public
  end;

  { TWinCEWSCustomRadioGroup }

  TWinCEWSCustomRadioGroup = class(TWSCustomRadioGroup)
  private
  protected
  public
  end;

  { TWinCEWSRadioGroup }

  TWinCEWSRadioGroup = class(TWSRadioGroup)
  private
  protected
  public
  end;

  { TWinCEWSCustomCheckGroup }

  TWinCEWSCustomCheckGroup = class(TWSCustomCheckGroup)
  private
  protected
  public
  end;

  { TWinCEWSCheckGroup }

  TWinCEWSCheckGroup = class(TWSCheckGroup)
  private
  protected
  public
  end;

  { TWinCEWSCustomLabeledEdit }

  TWinCEWSCustomLabeledEdit = class(TWSCustomLabeledEdit)
  private
  protected
  public
  end;

  { TWinCEWSLabeledEdit }

  TWinCEWSLabeledEdit = class(TWSLabeledEdit)
  private
  protected
  public
  end;

  { TWinCEWSCustomPanel }

  TWinCEWSCustomPanel = class(TWSCustomPanel)
  published
    class function  CreateHandle(const AWinControl: TWinControl;
          const AParams: TCreateParams): HWND; override;
  end;

  { TWinCEWSPanel }

  TWinCEWSPanel = class(TWSPanel)
  private
  protected
  public
  end;

implementation

uses
  LMessages;

{ TWinCEWSCustomPanel }

class function TWinCEWSCustomPanel.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): HWND;
var
  Params: TCreateWindowExParams;
begin
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ClsName;
    SubClassWndProc := nil;
//    DebugLn(Format('CustomPanel.Create Flags: %d FlagsEx: %d', [Flags, FlagsEx]));
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

end.
