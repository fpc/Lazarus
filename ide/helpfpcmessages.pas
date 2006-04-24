{
 /***************************************************************************
                            helpfpcmessages.pas
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
 
  Author: Mattias Gaertner
   
  Abstract:
    Help items for FPC messages.
}
unit HelpFPCMessages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, HelpIntf, HelpHTML;
  
const
  lihcFPCMessages = 'FreePascal Compiler messages';

var
  FPCMessagesHelpDB: THelpDatabase;
  
procedure CreateFPCMessagesHelpDB;

implementation

procedure CreateFPCMessagesHelpDB;
var
  HTMLHelp: THTMLHelpDatabase;
  StartNode: THelpNode;
begin
  FPCMessagesHelpDB:=HelpDatabases.CreateHelpDatabase(lihcFPCMessages,
                                                      THTMLHelpDatabase,true);
  HTMLHelp:=FPCMessagesHelpDB as THTMLHelpDatabase;

  HTMLHelp.BasePathObject:=
    THelpBasePathObject.Create('http://wiki.lazarus.freepascal.org/index.php/');

  // HTML nodes
  StartNode:=THelpNode.CreateURLID(HTMLHelp,'FreePascal Compiler messages',
          'file://Build_messages#FreePascal_Compiler_messages',lihcFPCMessages);
  HTMLHelp.TOCNode:=THelpNode.Create(HTMLHelp,StartNode);
  HTMLHelp.RegisterItemWithNode(StartNode);
  
  
end;

end.

