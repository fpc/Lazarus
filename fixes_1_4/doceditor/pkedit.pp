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

  Author: Michael Van Canneyt
}
unit PkEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

Type
  TTagType = (ttBold,ttItalic,ttUnderline,ttParagraph,ttVariable,ttRemark,
              ttNumberedList,ttUnnumberedList,ttListItem,ttTable,ttRow,
              ttCell,TTHeader,ttPre,ttCode,ttLink,ttFile);

  TElementEvent = Procedure (Node : TDomElement) of Object;

Function IsPackageNode (Node : TDomNode) : Boolean;
Function IsModuleNode (Node : TDomNode) : Boolean;
Function IsTopicNode (Node : TDomNode) : Boolean;
Function IsElementNode (Node : TDomNode) : Boolean;

Function SubNodeWithElement(P : TTreeNode; E : TDomElement) : TTreeNode;
Function GetNextNode(N : TTreeNode) : TTreeNode;

implementation
{ ---------------------------------------------------------------------
  Auxiliary functions
  ---------------------------------------------------------------------}

Function IsPackageNode (Node : TDomNode) : Boolean;

begin
  Result:=Assigned(Node) and (Node.NodeType=ELEMENT_NODE) and (Node.NodeName='package')
end;

Function IsModuleNode (Node : TDomNode) : Boolean;

begin
  Result:=Assigned(Node) and (Node.NodeType=ELEMENT_NODE) and (Node.NodeName='module')
end;

Function IsTopicNode (Node : TDomNode) : Boolean;

begin
  Result:=Assigned(Node) and (Node.NodeType=ELEMENT_NODE) and (Node.NodeName='topic')
end;

Function IsElementNode (Node : TDomNode) : Boolean;

begin
  Result:=Assigned(Node) and (Node.NodeType=ELEMENT_NODE) and (Node.NodeName='element')
end;

Function GetNextNode(N : TTreeNode) : TTreeNode;

begin
  Result:=N.GetNextSibling;
  If (Result=Nil) and (N.Parent<>Nil) then
    begin
    Result:=N.Parent.Items[0]; // Count is always >=0, N !!
    While (Result<>Nil) and (Result.GetNextSibling<>N) do
      Result:=Result.GetNextSibling;
    If (Result=Nil) then
      Result:=N.Parent;
    end;
end;

Function SubNodeWithElement(P : TTreeNode; E : TDomElement) : TTreeNode;

Var
  N : TTreeNode;

begin
 Result:=Nil;
 If (E<>Nil) and (P<>Nil) and (P.Count>0) then
   begin
   N:=P.Items[0];
   While (Result=Nil) and (N<>Nil) do
     If (N.Data=Pointer(E)) then
       Result:=N
     else
       N:=N.GetNextSibling;
   end;
end;




end.

