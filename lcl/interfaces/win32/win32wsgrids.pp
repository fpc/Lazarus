{ $Id$}
{
 *****************************************************************************
 *                              Win32WSGrids.pp                              * 
 *                              ---------------                              * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
  Windows, LCLType, LazUTF8, Types, Controls, Grids, Win32Proc, Graphics,
////////////////////////////////////////////////////
  WSGrids;

type
  { TWin32WSCustomGrid }

  TWin32WSCustomGrid = class(TWSCustomGrid)
  published
    class procedure SendCharToEditor(AEditor:TWinControl; Ch: TUTF8Char); override;
    class function GetEditorBoundsFromCellRect(ACanvas: TCanvas;
      const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect; override;
  end;

implementation

{ TWin32WSCustomGrid }

class function TWin32WSCustomGrid.GetEditorBoundsFromCellRect(ACanvas: TCanvas;
  const ACellRect: TRect; const AColumnLayout: TTextLayout): TRect;
var
  EditorTop: LongInt;
  TextHeight: Integer;
begin
  Result:=ACellRect;
  Dec(Result.Right);
  Dec(Result.Bottom);
  Inc(Result.Left, constCellPadding);
  Dec(Result.Right, constCellPadding);
  TextHeight := ACanvas.TextHeight(' ');
  case AColumnLayout of
    tlTop: EditorTop:=Result.Top+constCellPadding;
    tlCenter: EditorTop:=Result.Top+Round((Result.Bottom-Result.Top-TextHeight) / 2);
    tlBottom: EditorTop:=Result.Bottom-constCellPadding-TextHeight+1;
  end;
  if EditorTop>Result.Top then Result.Top:=EditorTop;
  Result.Bottom:=Result.Top+TextHeight;
end;

class procedure TWin32WSCustomGrid.SendCharToEditor(AEditor: TWinControl;
  Ch: TUTF8Char);
var
  S: widestring;
  WChar: WPARAM;
begin
  WChar:=WPARAM(Ord(Ch[1]));
  if Length(Ch)>1 then begin
    S := UTF8ToUTF16(Ch);
    if S='' then WChar := WPARAM(Ord('?'))
    else         WChar := WPARAM(S[1]);
  end;
  PostMessageW(AEditor.Handle, WM_CHAR, WChar, 0);
end;

end.
