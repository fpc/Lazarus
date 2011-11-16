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
    Dictionary of identifiers.
}
unit CodyIdentifiersDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, LCLProc, Forms, Controls, Graphics,
  Dialogs, ButtonPanel, StdCtrls, CustomCodeTool, CodeToolManager, UnitDictionary;

type

  { TCodyUnitDictionary }

  TCodyUnitDictionary = class(TUnitDictionary)
  private
    procedure ToolTreeChanged(Tool: TCustomCodeTool; {%H-}NodesDeleting: boolean);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TCodyIdentifiersDlg }

  TCodyIdentifiersDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    InfoLabel: TLabel;
  private
  public
    procedure Init;
  end;

var
  CodyUnitDictionary: TCodyUnitDictionary = nil;

procedure ShowUnitDictionaryDialog(Sender: TObject);
procedure InitUnitDictionary;

implementation

{$R *.lfm}

procedure ShowUnitDictionaryDialog(Sender: TObject);
var
  CodyIdentifiersDlg: TCodyIdentifiersDlg;
begin
  CodyIdentifiersDlg:=TCodyIdentifiersDlg.Create(nil);
  try
    CodyIdentifiersDlg.Init;
    CodyIdentifiersDlg.ShowModal;
  finally
    CodyIdentifiersDlg.Free;
  end;
end;

procedure InitUnitDictionary;
begin
  CodyUnitDictionary:=TCodyUnitDictionary.Create;
end;

{ TCodyUnitDictionary }

procedure TCodyUnitDictionary.ToolTreeChanged(Tool: TCustomCodeTool;
  NodesDeleting: boolean);
begin
  //debugln(['TCodyUnitDictionary.ToolTreeChanged ',Tool.MainFilename]);
end;

constructor TCodyUnitDictionary.Create;
begin
  inherited Create;
  CodeToolBoss.AddHandlerToolTreeChanging(@ToolTreeChanged);
end;

destructor TCodyUnitDictionary.Destroy;
begin
  inherited Destroy;
end;

{ TCodyIdentifiersDlg }

procedure TCodyIdentifiersDlg.Init;
var
  s: String;
begin
  s:='Packages: '+IntToStr(CodyUnitDictionary.UnitGroupsByFilename.Count)
    +', Units: '+IntToStr(CodyUnitDictionary.UnitsByFilename.Count)
    +', Identifiers: '+IntToStr(CodyUnitDictionary.Identifiers.Count);
  InfoLabel.Caption:=s;
end;

finalization
  FreeAndNil(CodyUnitDictionary);

end.

