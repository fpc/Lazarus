unit ChmLcl;

{ Copyright (C) 2011-2014 Mattias Gaertner, Lazarus contributors

  chmlcl.pas

  Help database for Lazarus LCL using lcl.chm.

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, LazHelpIntf, HelpFPDoc;

const
  sLclUnits = 'LCL - Lazarus component library';

type

  { TLclChmHelpDatabase }

  TLclChmHelpDatabase = class(TFPDocHTMLHelpDatabase)
  private
    FBaseURL: THelpBaseURLObject;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure RegisterLclHelpDatabase;

var
  LCLHelpDatabase: TLclChmHelpDatabase = nil;

implementation

procedure RegisterLclHelpDatabase;
var
  FPDocNode: THelpNode;
  DirItem: THelpDBISourceDirectory;
  DefaultHelpDB: THelpDatabase;
begin
  if Assigned(LCLHelpDatabase) then Exit;

  // remove default help
  DefaultHelpDB:=HelpDatabases.FindDatabase('LCLUnits');
  if DefaultHelpDB<>nil then
    DefaultHelpDB.Free;

  // register our own help
  LCLHelpDatabase := TLclChmHelpDatabase(
    HelpDatabases.CreateHelpDatabase(sLclUnits, TLclChmHelpDatabase, True));

  // FPDoc nodes for units in the LCL
  FPDocNode := THelpNode.CreateURL(LCLHelpDatabase,
    'LCL - Lazarus Component Library Units',
    'file://index.html');
  LCLHelpDatabase.TOCNode := THelpNode.Create(LCLHelpDatabase, FPDocNode);
  DirItem := THelpDBISourceDirectory.Create(FPDocNode, '$(LazarusDir)/lcl',
    '*.pp;*.pas;*.inc', True);// and once as normal page
  LCLHelpDatabase.RegisterItem(DirItem);
end;

{ TLclChmHelpDatabase }

constructor TLclChmHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FBaseURL := THelpBaseURLObject.Create;
  FBaseURL.BaseURL := 'lcl.chm://';
  BasePathObject := FBaseURL;
end;

destructor TLclChmHelpDatabase.Destroy;
begin
  FBaseURL.Free;
  inherited Destroy;
end;

end.

