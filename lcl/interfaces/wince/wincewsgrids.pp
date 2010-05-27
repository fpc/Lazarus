{ $Id: wincewsgrids.pp 23636 2010-02-05 07:53:33Z paul $}
{
 *****************************************************************************
 *                              WinCEWSGrids.pp                              *
 *                              ---------------                              * 
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
unit WinCEWSGrids;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL
  Windows,
  // LCL
  LCLType, LCLProc, Controls,
  // Widgetset
  WSGrids, WinCEWSControls, WinCEInt;

type
  { TWinCEWSCustomGrid }

  TWinCEWSCustomGrid = class(TWSCustomGrid)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLIntfHandle; override;
//    class procedure SendCharToEditor(AEditor:TWinControl; Ch: TUTF8Char); override;
  end;

implementation

{ TWinCEWSCustomGrid }

// We need to implement this to remove WS_VSCROLL and WS_HSCROLL,
// which don't do anything useful under WinCE. The behavior changes
// according to platform and is somewhat bad. Probably substitute with
// adding a TScrollBar. See here:
// http://bugs.freepascal.org/view.php?id=16576
// http://social.msdn.microsoft.com/forums/en-US/vssmartdevicesnative/thread/af5813e7-236e-4a06-bda9-945d6f88e3c4/
class function TWinCEWSCustomGrid.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLIntfHandle;
var
  Params: TCreateWindowExParams;
begin
  {$ifdef VerboseWinCE}
  DebugLn(' TWinCEWSWinControl.CreateHandle ');
  {$endif}
  // general initialization of Params
  PrepareCreateWindow(AWinControl, AParams, Params);
  // customization of Params
  with Params do
  begin
    pClassName := @ClsName;
    WindowTitle := StrCaption;
    SubClassWndProc := nil;
    Flags := (Flags and not WS_VSCROLL) and not WS_HSCROLL;
  end;
  // create window
  FinishCreateWindow(AWinControl, Params, false);
  Result := Params.Window;
end;

end.
