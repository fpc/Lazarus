{  $Id$  }
{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Michael Van Canneyt
  
  This unit registers the TDBF component of the FCL.
}
unit RegisterDBF;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Dbf, LazarusPackageIntf, PropEdits, MacroIntf;

resourcestring
  dbfsAllDbasefiles = 'DBase Files';
  
procedure Register;

implementation

{$R registerdbf.res}

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

function TargetPath: string;
begin
  Result:='$TargetFile()';
  IDEMacros.SubstituteMacros(Result);
  Result:=ExtractFilePath(Result);
end;

procedure Register;
begin
  RegisterUnit('DBF',@RegisterUnitDBF);
  DbfBasePath:=@TargetPath;
end;

end.
