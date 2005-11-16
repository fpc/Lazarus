{%MainUnit win32int.pp}
{ $Id$ }
{******************************************************************************
                 All GTK interface communication implementations.
                   Initial Revision  : Sun Nov 23 23:53:53 2003


  !! Keep alphabetical !!

  Support routines go to win32proc.inc

 ******************************************************************************
 Interface
 ******************************************************************************

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

//##apiwiz##sps##   // Do not remove

procedure AddEventHandler(AHandle: THandle; AFlags: dword; AEventHandler: TWaitHandleEvent); override;

procedure DrawArrow(Arrow: TComponent; Canvas: TPersistent); override;

function GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String; override;
function GetControlConstraints(Constraints: TObject): boolean; override;
function GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer; override;

procedure RemoveEventHandler(AHandle: THandle); override;


//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

