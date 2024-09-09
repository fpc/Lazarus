{
 *****************************************************************************
  This file is part of a Lazarus Package, IconFinder.

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
 *****************************************************************************

 A form for assigning keywords and styles to icons.
}

unit IconFinderMetaData;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, ExtCtrls,
  IconFinderStrConsts, IconThumbnails;

type

  { TIconMetadataForm }

  TIconMetadataForm = class(TForm)
    ButtonPanel: TButtonPanel;
    cmbStyle: TComboBox;
    lblStyle: TLabel;
    mmoKeywords: TMemo;
    Image: TImage;
    lblKeywords: TLabel;
    procedure FormCreate(Sender: TObject);
  public
    procedure ControlsToMetadata(AIcon: TIconItem);
    procedure MetadataToControls(AIcon: TIconItem);
    procedure UpdateLanguage;
  end;

var
  IconMetadataForm: TIconMetadataForm;

implementation

{$R *.lfm}

procedure TIconMetadataForm.FormCreate(Sender: TObject);
begin
  UpdateLanguage;
end;

{ Extracts metadata from the form controls to the provided icon item. }
procedure TIconMetadataForm.ControlsToMetadata(AIcon: TIconItem);
begin
  AIcon.SetKeywordsFromStrings(mmoKeywords.Lines);
  if cmbStyle.ItemIndex = -1 then
    AIcon.StyleAsString := ''
  else
    AIcon.StyleAsString := cmbStyle.Items[cmbStyle.ItemIndex];
end;

{ Moves metadata from provided icon item to controls of the form. }
procedure TIconMetadataForm.MetadataToControls(AIcon: TIconItem);
begin
  if AIcon = nil then
    raise Exception.Create('[TMetadataForm.SetMetadata] The icon item cannot be nil.');

  AIcon.ExportKeywordsToStrings(mmoKeywords.Lines);
  mmoKeywords.SelStart := Length(mmoKeywords.Text);
  cmbStyle.ItemIndex := Integer(AIcon.Style) - 1;
  Image.Picture.Assign(AIcon.Picture);
end;

procedure TIconMetadataForm.UpdateLanguage;
begin
  Caption := RSMetadata_Caption;
  lblKeywords.Caption := RSMetadata_Keywords;
  lblStyle.Caption := RSMetadata_Style;
  (*  Update only when translation of keywords and styles is finished.
  cmbStyle.Items.Clear;
  cmbStyle.Items.Add(RSMetadata_ClassicStyle);
  cmbStyle.Items.Add(RSMetadata_FlatStyle);
  cmbStyle.Items.Add(RSMetadata_OutlineStyle);
  cmbStyle.Items.Add(RSMetadata_Outline2Style);
  *)
end;

end.

