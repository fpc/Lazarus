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
unit editor_sql_options;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, StdCtrls, DividerBevel, Controls, Graphics,
  // SynEdit
  SynEditStrConst, SynHighlighterSQL,
  // IDEIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  LazarusIDEStrConsts, EditorOptions, editor_general_options;

type

  { TEditorSqlOptionsFrame }

  TEditorSqlOptionsFrame = class(TAbstractIDEOptionsEditor)
    divSqlDialect: TDividerBevel;
    dropSqlDialect: TComboBox;
    procedure dropSqlDialectChange(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;

    function GeneralPage: TEditorGeneralOptionsFrame; inline;
  public
    function GetTitle: String; override;
    procedure UpdatePreviews;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TEditorSqlOptionsFrame }

procedure TEditorSqlOptionsFrame.dropSqlDialectChange(Sender: TObject);
begin
  GeneralPage.UpdatePreviewEdits;
end;

function TEditorSqlOptionsFrame.GeneralPage: TEditorGeneralOptionsFrame;
begin
  Result := TEditorGeneralOptionsFrame(FDialog.FindEditor(TEditorGeneralOptionsFrame));
end;

function TEditorSqlOptionsFrame.GetTitle: String;
begin
  Result := lisSQLHighlightOpts;
end;

procedure TEditorSqlOptionsFrame.UpdatePreviews;
var
  a: Integer;
  Syn: TSynSQLSyn;
begin
  with GeneralPage do
    for a := Low(PreviewEdits) to High(PreviewEdits) do
      if PreviewEdits[a] <> nil then
      begin
        if PreviewEdits[a].Highlighter is TSynSQLSyn then begin
          Syn := TSynSQLSyn(PreviewEdits[a].Highlighter);
          Syn.SQLDialect := TSQLDialect(PtrInt(dropSqlDialect.Items.Objects[dropSqlDialect.ItemIndex]));
        end;
      end;
end;

procedure TEditorSqlOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  sd: TSQLDialect;
begin
  FDialog := ADialog;

  divSqlDialect.Caption := dlgSqlExtHighlightGroup;

  dropSqlDialect.Items.Clear;
  dropSqlDialect.Sorted := True;
  for sd := Low(TSQLDialect) to High(TSQLDialect) do
    dropSqlDialect.Items.AddObject(SQLDialectToName(sd), TObject(PtrInt(Ord(sd))));
  dropSqlDialect.ItemIndex := dropSqlDialect.Items.IndexOfObject(TObject(PtrInt(Ord(sqlStandard))));
end;

procedure TEditorSqlOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    dropSqlDialect.Text := SQLDialectToName(SQLDialect);
  end;
end;

procedure TEditorSqlOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  b: Boolean;
begin
  with AOptions as TEditorOptions do
  begin
    SQLDialect := TSQLDialect(PtrInt(dropSqlDialect.Items.Objects[dropSqlDialect.ItemIndex]));
  end;
end;

class function TEditorSqlOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEditor, TEditorSqlOptionsFrame,
    EdtOptionsSQL, EdtOptionsDisplay);
end.
