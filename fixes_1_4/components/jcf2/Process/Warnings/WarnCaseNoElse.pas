unit WarnCaseNoElse;

{ AFS 20 June 2K
 warn of case without a default 'else' case

 This is often an error
 your program will be more error-proof if every case has an else
 if you can't think of anything to put there, put

 case
    ...
    else Raise Exception.Create('case had unexpected value');
 end;
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is WarnCaseNoElse, released May 2003.
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

uses Warning;

type

  TWarnCaseNoElse = class(TWarning)
  public
    constructor Create; override;

    procedure PreVisitParseTreeNode(const pcNode: TObject); override;
  end;


implementation

uses
  ParseTreeNode, ParseTreeNodeType;



constructor TWarnCaseNoElse.Create;
begin
  inherited;

  HasPreVisit := True;
  HasPostVisit := False;
  HasSourceTokenVisit := True;
end;

procedure TWarnCaseNoElse.PreVisitParseTreeNode(const pcNode: TObject);
var
  lcNode: TParseTreeNode;
begin
  lcNode := TParseTreeNode(pcNode);

  // when we have a case statement, does it have an else?
  if (lcNode.NodeType = nCaseStatement) and
    ( not lcNode.HasChildNode(nElseCase, 1)) then
  begin
    SendWarning(lcNode, 'Case statement has no else case');
  end;
end;

end.
