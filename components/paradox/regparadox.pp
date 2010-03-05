{ **********************************************************************

  Register the Paradox component

  Copyright (C) 2008, Michael Van Canneyt michael@freepascal.org

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit regparadox;

{$mode objfpc}{$H+}

{$IF defined(VER2_2_0)}
{$ERROR this package requires at least fpc 2.2.1}
{$ENDIF}

interface

uses
  Classes, SysUtils, propedits, lresources, LazarusPackageIntf, paradox;

Type
  TParadoxFileNamePropertyEditor=class(TFileNamePropertyEditor)
  public
    function GetFilter: String; override;
    function GetInitialDirectory: string; override;
  end;
  
  TParadoxBlobFileNamePropertyEditor=class(TFileNamePropertyEditor)
  public
    function GetFilter: String; override;
    function GetInitialDirectory: string; override;
  end;

procedure register;

implementation

Resourcestring
  SParadoxFiles     = 'Paradox files';
  SParadoxBlobFiles = 'Paradox blob files';

{ TParadoxFileNamePropertyEditor }

function TParadoxFileNamePropertyEditor.GetFilter: String;
begin
  Result := SParadoxFiles+' (*.db)|*.db';
  Result:= Result+ '|'+ inherited GetFilter;
end;

function TParadoxFileNamePropertyEditor.GetInitialDirectory: string;
begin
  Result:= (GetComponent(0) as TParadox).FileName;
  Result:= ExtractFilePath(Result);
end;

{ TParadoxBlobFileNamePropertyEditor }

function TParadoxBlobFileNamePropertyEditor.GetFilter: String;
begin
  Result := SParadoxBlobFiles+' (*.bm)|*.bm';
  Result:= Result+ '|'+ inherited GetFilter;
end;

function TParadoxBlobFileNamePropertyEditor.GetInitialDirectory: string;
begin
  Result:= (GetComponent(0) as TParadox).BlobFileName;
  Result:= ExtractFilePath(Result);
end;

procedure registerunitparadox;

begin
  Registercomponents('Data Access',[TParadox]);
end;

procedure register;

begin
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TParadox, 'FileName', TParadoxFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString),
    TParadox, 'BlobFileName', TParadoxBlobFileNamePropertyEditor);
  RegisterUnit('paradox',@RegisterUnitParadox);
end;

initialization
  {$i regparadox.lrs}
end.

