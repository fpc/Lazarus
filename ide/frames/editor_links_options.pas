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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit editor_links_options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, StdCtrls, Controls, Grids, DividerBevel,
  // SynEdit
  SynPluginExternalLink,
  // IDEIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  LazarusIDEStrConsts, EditorOptions;

type

  { TEditorLinksOptionsFrame
    Text that is not a web address but links to one, e.g. "issue #123". }

  TEditorLinksOptionsFrame = class(TAbstractIDEOptionsEditor)
    btnAdd: TButton;
    btnDefaults: TButton;
    btnRemove: TButton;
    divLinks: TDividerBevel;
    lblInfo: TLabel;
    LinkGrid: TStringGrid;
    procedure btnAddClick(Sender: TObject);
    procedure btnDefaultsClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
  private
    procedure FillGrid(APatterns: TSynPluginExternalLinkPatterns);
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TEditorLinksOptionsFrame }

procedure TEditorLinksOptionsFrame.btnAddClick(Sender: TObject);
begin
  LinkGrid.RowCount := LinkGrid.RowCount + 1;
  LinkGrid.Row := LinkGrid.RowCount - 1;
  LinkGrid.Col := 0;
  LinkGrid.SetFocus;
  LinkGrid.EditorMode := True;
end;

procedure TEditorLinksOptionsFrame.btnDefaultsClick(Sender: TObject);
var
  Defaults: TSynPluginExternalLinkPatterns;
begin
  Defaults := TSynPluginExternalLinkPatterns.Create(nil);
  try
    TEditorOptions.SetDefaultExternalLinkPatterns(Defaults);
    FillGrid(Defaults);
  finally
    Defaults.Free;
  end;
end;

procedure TEditorLinksOptionsFrame.btnRemoveClick(Sender: TObject);
begin
  if LinkGrid.Row >= LinkGrid.FixedRows then
    LinkGrid.DeleteRow(LinkGrid.Row);
end;

procedure TEditorLinksOptionsFrame.FillGrid(APatterns: TSynPluginExternalLinkPatterns);
var
  i: Integer;
begin
  LinkGrid.RowCount := LinkGrid.FixedRows + APatterns.Count;
  for i := 0 to APatterns.Count - 1 do begin
    LinkGrid.Cells[0, LinkGrid.FixedRows + i] := APatterns[i].Pattern;
    LinkGrid.Cells[1, LinkGrid.FixedRows + i] := APatterns[i].Url;
    LinkGrid.Cells[2, LinkGrid.FixedRows + i] := APatterns[i].Folder;
  end;
end;

function TEditorLinksOptionsFrame.GetTitle: String;
begin
  Result := dlgExternalLinks;
end;

procedure TEditorLinksOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  divLinks.Caption := dlgExternalLinksGroup;
  lblInfo.Caption := dlgExternalLinksInfo;
  LinkGrid.Columns[0].Title.Caption := dlgExternalLinksPattern;
  LinkGrid.Columns[1].Title.Caption := dlgExternalLinksUrl;
  LinkGrid.Columns[2].Title.Caption := dlgExternalLinksFolder;
  btnAdd.Caption := dlgExternalLinksAdd;
  btnRemove.Caption := lisRemove;
  btnDefaults.Caption := dlgExternalLinksDefaults;
end;

procedure TEditorLinksOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  FillGrid((AOptions as TEditorOptions).ExternalLinkPatterns);
end;

procedure TEditorLinksOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Patterns: TSynPluginExternalLinkPatterns;
  i: Integer;
begin
  // a cell still being typed in, when OK is pressed, counts too
  LinkGrid.EditorMode := False;
  Patterns := TSynPluginExternalLinkPatterns.Create(nil);
  try
    for i := LinkGrid.FixedRows to LinkGrid.RowCount - 1 do
      // a row without a pattern or an address is left out
      if (Trim(LinkGrid.Cells[0, i]) <> '') and (Trim(LinkGrid.Cells[1, i]) <> '') then
        Patterns.Add(Trim(LinkGrid.Cells[0, i]), Trim(LinkGrid.Cells[1, i]), Trim(LinkGrid.Cells[2, i]));
    (AOptions as TEditorOptions).ExternalLinkPatterns := Patterns;
  finally
    Patterns.Free;
  end;
end;

class function TEditorLinksOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorLinksOptionsFrame,
    EdtOptionsLinks, EdtOptionsDisplay);
end.
