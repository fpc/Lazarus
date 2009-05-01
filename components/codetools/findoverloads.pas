{
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

  Author: Mattias Gaertner

  Abstract:
    A graph of declaration overloads.
}
unit FindOverloads;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeGraph;

type
  TOverloadsGraphNode = class(TCodeGraphNode)
  public
    Identifier: string;
  end;

  TOverloadsGraphEdgeType = (
    ogetParent,
    ogetAncestor,
    ogetInterface
    );

  TOverloadsGraphEdge = class(TCodeGraphEdge)
  public
    Typ: TOverloadsGraphEdgeType;
  end;

  { TDeclarationOverloadsGraph }

  TDeclarationOverloadsGraph = class
  private
    FGraph: TCodeGraph;
    FIdentifier: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Identifier: string read FIdentifier;
    property Graph: TCodeGraph read FGraph;
  end;

implementation

{ TDeclarationOverloadsGraph }

constructor TDeclarationOverloadsGraph.Create;
begin
  FGraph:=TCodeGraph.Create(TOverloadsGraphNode,TOverloadsGraphEdge);
end;

destructor TDeclarationOverloadsGraph.Destroy;
begin
  Clear;
  FreeAndNil(FGraph);
  inherited Destroy;
end;

procedure TDeclarationOverloadsGraph.Clear;
begin

end;

end.

