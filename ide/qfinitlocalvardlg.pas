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
   Dialog used by the quick fix "Insert Assignment var := ..." for messages
   Hint/Warning: (5036) Local variable "$1" does not seem to be initialized
   Dialog shows
}
unit QFInitLocalVarDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, FileUtil, CodeToolManager, CodeCache, LazIDEIntf,
  Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls, StdCtrls;

type

  { TQFInitLocalVarDialog }

  TQFInitLocalVarDialog = class(TForm)
    ButtonPanel1: TButtonPanel;
    ValueGroupBox: TGroupBox;
    WhereRadioGroup: TRadioGroup;
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

function QuickFixLocalVarNotInitialized(Code: TCodeBuffer; X, Y: integer;
  Identifier: string): boolean;

implementation

function QuickFixLocalVarNotInitialized(Code: TCodeBuffer; X, Y: integer;
  Identifier: string): boolean;
var
  Statements: TStrings;
  InsertPositions: TObjectList;
begin
  Result:=false;
  if not CodeToolBoss.GetPossibleInitsForVariable(Code,X,Y,Statements,
    InsertPositions)
  then begin
    if CodeToolBoss.ErrorCode<>nil then
      LazarusIDE.DoJumpToCodeToolBossError
    else
      ShowMessage('CodeToolBoss.GetPossibleInitsForVariable failed at '+Code.Filename+'('+IntToStr(Y)+','+IntToStr(X)+')');
    exit;
  end;

  ShowMessage('QuickFixLocalVarNotInitialized not yet implemented');
end;

{$R *.lfm}

{ TQFInitLocalVarDialog }

procedure TQFInitLocalVarDialog.FormCreate(Sender: TObject);
begin
  Caption:='Initialize local variable';
  WhereRadioGroup.AutoSize:=true;
end;

end.

