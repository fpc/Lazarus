{  $Id$  }
{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Michael Van Canneyt
  
  This unit registers the TDBF component of the FCL.
}
unit RegisterDBF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Dbf, LazarusPackageIntf, PropEdits;

resourcestring
  dbfsAllDbasefiles = 'DBase Files';
  
procedure Register;

implementation

type

  { TDbfFileNamePropertyEditor }

  TDbfFileNamePropertyEditor=class(TFileNamePropertyEditor)
  public
    function GetFilter: String; override;
    function GetInitialDirectory: string; override;
  end;

{ TDbfFileNamePropertyEditor }

function TDbfFileNamePropertyEditor.GetFilter: String;
begin
  Result := dbfsAllDbaseFiles+' (*.dbf)|*.dbf;*.DBF';
  Result:= Result+ '|'+ inherited GetFilter;
end;

function TDbfFileNamePropertyEditor.GetInitialDirectory: string;
begin
  Result:= (GetComponent(0) as TDBF).FilePath;
end;

procedure RegisterUnitDBF;
begin
  RegisterComponents('Data Access',[TDbf]);
  
  RegisterPropertyEditor(TypeInfo(AnsiString),
    Tdbf, 'FilePath', TDirectoryPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString),
    Tdbf, 'TableName', TDbfFileNamePropertyEditor);
end;

procedure Register;
begin
  RegisterUnit('DBF',@RegisterUnitDBF);
end;

initialization
  {$i registerdbf.lrs}
end.
