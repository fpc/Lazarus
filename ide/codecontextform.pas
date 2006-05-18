{
/***************************************************************************
                             CodeContextForm.pas
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
    The popup tooltip window for the source editor.
}
unit CodeContextForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  CodeCache, FindDeclarationTool, IdentCompletionTool, CodeToolManager, SynEdit;

type

  { TCodeContextFrm }

  TCodeContextFrm = class(TForm)
  private
  public
  end;

var
  CodeContextFrm: TCodeContextFrm = nil;
  
function ShowCodeContext(Code: TCodeBuffer; Editor: TSynEdit): boolean;

implementation

function ShowCodeContext(Code: TCodeBuffer; Editor: TSynEdit): boolean;
var
  LogCaretXY: TPoint;
  CodeContexts: TCodeContextInfo;
begin
  Result:=false;
  LogCaretXY:=Editor.LogicalCaretXY;
  CodeContexts:=nil;
  try
    if not CodeToolBoss.FindCodeContext(Code,LogCaretXY.X,LogCaretXY.Y,
      CodeContexts)
    then
      exit;
    DebugLn('ShowCodeContext show TODO');
    if CodeContextFrm=nil then
      CodeContextFrm:=TCodeContextFrm.Create(nil);
    
  finally
    CodeContexts.Free;
  end;
end;

initialization
  {$I codecontextform.lrs}

end.

