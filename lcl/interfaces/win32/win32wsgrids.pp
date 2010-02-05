{ $Id$}
{
 *****************************************************************************
 *                              Win32WSGrids.pp                              * 
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
unit Win32WSGrids;

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
  Windows, LCLType, LCLProc, Controls, Win32Proc,
////////////////////////////////////////////////////
  WSGrids;

type
  { TWin32WSCustomGrid }

  TWin32WSCustomGrid = class(TWSCustomGrid)
  published
    class procedure SendCharToEditor(AEditor:TWinControl; Ch: TUTF8Char); override;
  end;

implementation

{ TWin32WSCustomGrid }

class procedure TWin32WSCustomGrid.SendCharToEditor(AEditor: TWinControl;
  Ch: TUTF8Char);
var
  S: widestring;
  WChar: WPARAM;
begin
  WChar:=WPARAM(Ord(Ch[1]));
  {$ifdef WindowsUnicodeSupport}
  if UnicodeEnabledOS then begin
    if Length(Ch)>1 then begin
      S := UTF8ToUTF16(Ch);
      if S='' then WChar := WPARAM(Ord('?'))
      else         WChar := WPARAM(S[1]);
    end;
    PostMessageW(AEditor.Handle, WM_CHAR, WChar, 0);
    exit;
  end else
    WChar := WPARAM(Ord(UTF8ToAnsi(Ch)[1]));
  {$endif}
  PostMessage(AEditor.Handle, WM_CHAR, WChar, 0);
end;

end.
