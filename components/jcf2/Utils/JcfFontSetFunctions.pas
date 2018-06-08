unit JcfFontSetFunctions;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is JcfFontSetFunctions, released May 2007.
The Initial Developer of the Original Code is Jean-Fabien Connault.
Portions created by Jean-Fabien Connault are Copyright (C) 1999-2008 Jean-Fabien Connault.
All Rights Reserved.
Contributor(s):
SetObjectFontToSystemFont by Jean-Fabien Connault
Integrated to JCF by Anthony Steele

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

type
     TFontType  = (ftAuto, ftCaption, ftContent);

procedure SetObjectFontToSystemFont(
  const AObject: TObject; const FontType: TFontType = ftAuto);

implementation

uses
  StdCtrls, Graphics, TypInfo,
  JcfSystemUtils;

procedure SetCaptionFont(const AObjectFont: TFont);
begin
  if IsWinVista or IsWinServer2008 or IsWin7 or IsWinServer2008R2 then
  begin
    AObjectFont.Name := 'Segoe UI';
    AObjectFont.Size := 9;
  end
  else if IsWinXP or IsWin2k or IsWin2003 then
  begin
    // MS Shell Dlg 2
    AObjectFont.Name := 'Tahoma';
    AObjectFont.Size := 8;
  end
  else
  begin
    // MS Shell Dlg
    AObjectFont.Name := 'default';
    AObjectFont.Size := 10;
  end;
end;

procedure SetContentFont(const AObjectFont: TFont);
begin
  if IsWinVista or IsWinServer2008 or IsWin7 or IsWinServer2008R2 then
  begin
    AObjectFont.Name := 'Calibri';
    AObjectFont.Size := 9;
  end
  else if IsWinXP or IsWin2k or IsWin2003 then
  begin
    // MS Shell Dlg 2
    AObjectFont.Name := 'Verdana';
    AObjectFont.Size := 8;
  end
  else
  begin
    // MS Shell Dlg
    AObjectFont.Name := 'default';
    AObjectFont.Size := 10;
  end;
end;


procedure SetObjectFontToSystemFont(const AObject: TObject; const FontType: TFontType);
var
  AObjectFont: TFont;
  AFontType:   TFontType;
begin
  {$ifdef FPC}
  exit; // Lazarus forms have "default" font set
  {$endif}

  if (AObject.ClassType = TFont) then
    AObjectFont := TFont(AObject)
  else
    AObjectFont := TFont(GetObjectProp(AObject, 'Font', TFont));

  if (FontType = ftAuto) then
  begin
    if (AObject.ClassType = TMemo) {$IFNDEF FPC} or (AObject.ClassType = TRichEdit) {$ENDIF} then
      AFontType := ftContent
    else
      AFontType := ftCaption;
  end
  else
    AFontType := FontType;

  if (AFontType = ftCaption) then
  begin
    SetCaptionFont(AObjectFont);
  end
  else if (AFontType = ftContent) then
  begin
    SetContentFont(AObjectFont);
  end;

end;


end.
