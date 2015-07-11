unit BaseVisitor;

{ AFS 28 Dec 2002

  Base class that implments the tree node Visitor interface
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is BaseVisitor, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

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

  TBaseTreeNodeVisitor = class(TObject)
  private
    { which visits do I want to do?
      This is for speed - don't do the virtual fn call if not needed }
    fbHasPreVisit: Boolean;
    fbHasPostVisit: Boolean;
    fbHasSourceTokenVisit: Boolean;

  public
    constructor Create; virtual;

    { these are called when visiting interior nodes before and after thier children
      Must return true if the visited node is deleted, or if nodes are inserted before it
      ie if the curent node's index is not correct and the same after the visit}
    procedure PreVisitParseTreeNode(const pcNode: TObject); virtual;
    procedure PostVisitParseTreeNode(const pcNode: TObject); virtual;

    { this is called when visiting a leaf node (ie a source token) }
    function VisitSourceToken(const pcToken: TObject): Boolean; virtual;

    function FinalSummary(out psMessage: string): boolean; virtual;
    function IsIncludedInSettings: boolean; virtual;

    property HasPreVisit: boolean read fbHasPreVisit write fbHasPreVisit;
    property HasPostVisit: boolean read fbHasPostVisit write fbHasPostVisit;
    property HasSourceTokenVisit: boolean read fbHasSourceTokenVisit write fbHasSourceTokenVisit;
  end;

type
  TTreeNodeVisitorType = class of TBaseTreeNodeVisitor;

implementation


// need a virtual constructor for the create-by-class-ref
constructor TBaseTreeNodeVisitor.Create;
begin
  inherited;

  { most visitors just touch the leaves }
  fbHasPreVisit := False;
  fbHasPostVisit := False;
  fbHasSourceTokenVisit := True;
end;

function TBaseTreeNodeVisitor.FinalSummary(out psMessage: string): boolean;
begin
  // no message
  Result    := False;
  psMessage := '';
end;

procedure TBaseTreeNodeVisitor.PreVisitParseTreeNode(const pcNode: TObject);
begin
  // do nothing, here for override
end;

procedure TBaseTreeNodeVisitor.PostVisitParseTreeNode(const pcNode: TObject);
begin
  // do nothing, here for override
end;

function TBaseTreeNodeVisitor.VisitSourceToken(const pcToken: TObject): Boolean;
begin
  // do nothing, here for override
  Result := False;
end;

function TBaseTreeNodeVisitor.IsIncludedInSettings: boolean;
begin
  // here for override
  Result := True;
end;

end.
