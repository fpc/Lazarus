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
  PascalParserTool, IdentCompletionTool, GraphType, Graphics, EditorOptions,
  SynEdit, SynCompletion;

type
  TCompletionType = (
    ctNone, ctWordCompletion, ctTemplateCompletion, ctIdentCompletion);
  TIdentComplValue = (
    icvIdentifier, icvProcWithParams, icvIndexedProp);

procedure PaintCompletionItem(const AKey: string; ACanvas: TCanvas;
  X, Y: integer; ItemSelected: boolean; Index: integer;
  aCompletion : TSynCompletion; CurrentCompletionType: TCompletionType);
  
function GetIdentCompletionValue(aCompletion : TSynCompletion;
  var ValueType: TIdentComplValue): string;

implementation

procedure PaintCompletionItem(const AKey: string; ACanvas: TCanvas;
  X, Y: integer; ItemSelected: boolean; Index: integer;
  aCompletion : TSynCompletion; CurrentCompletionType: TCompletionType);
  
  function InvertColor(AColor: TColor): TColor;
  var Red, Green, Blue: integer;
  begin
    Result:=clWhite;
    Red:=(AColor shr 16) and $ff;
    Green:=(AColor shr 8) and $ff;
    Blue:=AColor and $ff;
    if Red+Green+Blue>$180 then
      Result:=clBlack;
  end;

  procedure SetFontColor(NewColor: TColor);
  begin
    if ItemSelected then NewColor:=InvertColor(NewColor);
    ACanvas.Font.Color:=NewColor;
  end;
  
var
  i: Integer;
  s: string;
  IdentItem: TIdentifierListItem;
  AColor: TColor;
  ANode: TCodeTreeNode;
begin
  if CurrentCompletionType=ctIdentCompletion then begin
    // draw
    IdentItem:=CodeToolBoss.IdentifierList.FilteredItems[Index];
    if IdentItem=nil then begin
      ACanvas.TextOut(x+1, y, 'PaintCompletionItem: BUG in codetools');
      exit;
    end;
    // first write the type
    // var, procedure, property, function, type, const
    case IdentItem.GetDesc of

    ctnVarDefinition:
      begin
        AColor:=clMaroon;
        s:='var';
      end;

    ctnTypeDefinition:
      begin
        AColor:=clDkGray;
        s:='type';
      end;

    ctnConstDefinition:
      begin
        AColor:=clOlive;
        s:='const';
      end;

    ctnProcedure:
      if (IdentItem.Node<>nil)
      and IdentItem.Tool.NodeIsFunction(IdentItem.Node) then begin
        AColor:=clTeal;
        s:='function';
      end else begin
        AColor:=clNavy;
        s:='procedure';
      end;
      
    ctnProperty:
      begin
        AColor:=clPurple;
        s:='property';
      end;

    else
      AColor:=clGray;
      s:='';
    end;
    
    SetFontColor(AColor);
    ACanvas.TextOut(x+1,y,s);
    inc(x,ACanvas.TextWidth('procedure '));

    SetFontColor(clBlack);
    ACanvas.Font.Style:=ACanvas.Font.Style+[fsBold];
    s:=GetIdentifier(IdentItem.Identifier);
    ACanvas.TextOut(x+1,y,s);
    inc(x,ACanvas.TextWidth(s));
    ACanvas.Font.Style:=ACanvas.Font.Style-[fsBold];

    if IdentItem.Node<>nil then begin
      case IdentItem.Node.Desc of

      ctnProcedure:
        begin
          s:=IdentItem.Tool.ExtractProcHead(IdentItem.Node,
            [phpWithoutClassName,phpWithoutName,phpWithVarModifiers,
             phpWithParameterNames,phpWithDefaultValues,phpWithResultType,
             phpWithOfObject]);
        end;

      ctnProperty:
        begin
          s:=IdentItem.Tool.ExtractProperty(IdentItem.Node,
            [phpWithoutName,phpWithVarModifiers,
             phpWithParameterNames,phpWithDefaultValues,phpWithResultType]);
        end;

      ctnVarDefinition:
        begin
          ANode:=IdentItem.Tool.FindTypeNodeOfDefinition(IdentItem.Node);
          s:=' : '+IdentItem.Tool.ExtractNode(ANode,[]);
        end;

      ctnTypeDefinition:
        begin
          ANode:=IdentItem.Tool.FindTypeNodeOfDefinition(IdentItem.Node);
          s:=' = '+IdentItem.Tool.ExtractNode(ANode,[]);
        end;

      ctnConstDefinition:
        begin
          ANode:=IdentItem.Tool.FindTypeNodeOfDefinition(IdentItem.Node);
          s:=' = '+IdentItem.Tool.ExtractNode(ANode,[]);
        end;

      else
        exit;

      end;
    end else begin
      // IdentItem.Node=nil
      exit;
    end;
    
    SetFontColor(clBlack);
    ACanvas.TextOut(x+1,y,s);

  end else begin
    // parse AKey for text and style
    i := 1;
    while i <= Length(AKey) do begin
      case AKey[i] of
      #1, #2:
        begin
          // set color
          ACanvas.Font.Color := (Ord(AKey[i + 3]) shl 8
                        + Ord(AKey[i + 2])) shl 8
                        + Ord(AKey[i + 1]);
          inc(i, 4);
        end;
      #3:
        begin
          // set style
          case AKey[i + 1] of
          'B': ACanvas.Font.Style := ACanvas.Font.Style + [fsBold];
          'b': ACanvas.Font.Style := ACanvas.Font.Style - [fsBold];
          'U': ACanvas.Font.Style := ACanvas.Font.Style + [fsUnderline];
          'u': ACanvas.Font.Style := ACanvas.Font.Style - [fsUnderline];
          'I': ACanvas.Font.Style := ACanvas.Font.Style + [fsItalic];
          'i': ACanvas.Font.Style := ACanvas.Font.Style - [fsItalic];
          end;
          inc(i, 2);
        end;
      else
        ACanvas.TextOut(x+1, y, AKey[i]);
        x := x + ACanvas.TextWidth(AKey[i]);
        inc(i);
      end;
    end;
  end;
end;

function GetIdentCompletionValue(aCompletion : TSynCompletion;
  var ValueType: TIdentComplValue): string;
var
  Index: Integer;
  IdentItem: TIdentifierListItem;
begin
  Index:=aCompletion.Position;
  IdentItem:=CodeToolBoss.IdentifierList.FilteredItems[Index];
  ValueType:=icvIdentifier;
  if IdentItem<>nil then begin
    Result:=GetIdentifier(IdentItem.Identifier);
    case IdentItem.GetDesc of
    
    ctnProcedure:
      if (IdentItem.Node<>nil)
      and IdentItem.Tool.ProcNodeHasParamList(IdentItem.Node) then
        ValueType:=icvProcWithParams;

    ctnProperty:
      if (IdentItem.Node<>nil)
      and IdentItem.Tool.PropertyNodeHasParamList(IdentItem.Node) then
        ValueType:=icvIndexedProp;

    end;
  end else
    Result:='';
end;

end.

