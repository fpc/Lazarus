{
/***************************************************************************
                             SourceEditProcs.pas
                             -------------------

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Support functions and types for the source editor.

}
unit SourceEditProcs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BasicCodeTools, CodeTree, CodeToolManager,
  IdentCompletionTool, GraphType, Graphics, EditorOptions,
  SynEdit, SynCompletion;

type
  TCompletionType = (
    ctNone, ctWordCompletion, ctTemplateCompletion, ctIdentCompletion);

procedure PaintCompletionItem(const AKey: string; ACanvas: TCanvas;
  X, Y: integer; ItemSelected: boolean; Index: integer;
  aCompletion : TSynCompletion; CurrentCompletionType: TCompletionType);

implementation

procedure PaintCompletionItem(const AKey: string; ACanvas: TCanvas;
  X, Y: integer; ItemSelected: boolean; Index: integer;
  aCompletion : TSynCompletion; CurrentCompletionType: TCompletionType);
var
  i: Integer;
  s: string;
  IdentItem: TIdentifierListItem;
begin
  with ACanvas do begin
    if CurrentCompletionType=ctIdentCompletion then begin
      // draw
      IdentItem:=CodeToolBoss.IdentifierList.FilteredItems[Index];
      if IdentItem=nil then begin
        TextOut(x+1, y, 'PaintCompletionItem: BUG in codetools');
        exit;
      end;
      s:=GetIdentifier(IdentItem.Identifier);
      TextOut(x+1,y,s);

    end else begin
      // parse AKey for text and style
      i := 1;
      while i <= Length(AKey) do begin
        case AKey[i] of
        #1, #2:
          begin
            // set color
            Font.Color := (Ord(AKey[i + 3]) shl 8 + Ord(AKey[i + 2])) shl 8
                          + Ord(AKey[i + 1]);
            inc(i, 4);
          end;
        #3:
          begin
            // set style
            case AKey[i + 1] of
            'B': Font.Style := Font.Style + [fsBold];
            'b': Font.Style := Font.Style - [fsBold];
            'U': Font.Style := Font.Style + [fsUnderline];
            'u': Font.Style := Font.Style - [fsUnderline];
            'I': Font.Style := Font.Style + [fsItalic];
            'i': Font.Style := Font.Style - [fsItalic];
            end;
            inc(i, 2);
          end;
        else
          TextOut(x+1, y, AKey[i]);
          x := x + TextWidth(AKey[i]);
          inc(i);
        end;
      end;
    end;
  end;
end;

end.

