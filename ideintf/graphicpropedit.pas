{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Tomas Gregorovic

  Abstract:
    This units defines the Load Image Dialog (TGraphicPropertyEditorForm)
    for graphic or picture property editor.
}
unit GraphicPropEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ButtonPanel, ExtDlgs,
  IDEDialogs, ObjInspStrConsts;

type

  { TGraphicPropertyEditorForm }

  TGraphicPropertyEditorForm = class(TForm)
    ImagePreview: TImage;
    OKButton: TBitBtn;
    CancelButton: TBitBtn;
    LoadButton: TButton;
    SaveButton: TButton;
    ClearButton: TButton;
    GroupBox1: TGroupBox;
    OpenDialog: TOpenPictureDialog;
    SaveDialog: TSavePictureDialog;
    ScrollBox: TScrollBox;
    procedure ClearButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
  private
    FModified: Boolean;
    procedure SetModified(const AValue: Boolean);
    { private declarations }
  public
    FileName: String;
    property Modified: Boolean read FModified write SetModified;
    property Preview: TImage read ImagePreview write ImagePreview;
  end;

var
  GraphicPropertyEditorForm: TGraphicPropertyEditorForm;

implementation

{ TGraphicPropertyEditorForm }

procedure TGraphicPropertyEditorForm.FormCreate(Sender: TObject);
begin
  FileName := '';
  Caption := oisLoadImageDialog;
  GroupBox1.Caption:=oisPEPicture;
  OKButton.Caption := oisOK;
  CancelButton.Caption := oisCancel;
  LoadButton.Caption := oisLoad;
  SaveButton.Caption := oisSave;
  ClearButton.Caption := oisClear;
  OpenDialog.Title:=oisPEOpenImageFile;
  SaveDialog.Title:=oisPESaveImageAs;
end;

procedure TGraphicPropertyEditorForm.ClearButtonClick(Sender: TObject);
begin
  with Preview do
  begin
    Picture.Clear;
  end;
  
  ScrollBox.Invalidate;
  SaveButton.Enabled := False;
  Modified := True;
end;

procedure TGraphicPropertyEditorForm.LoadButtonClick(Sender: TObject);
begin
  InitIDEFileDialog(OpenDialog);
  if OpenDialog.Execute then
  begin
    FileName := OpenDialog.FileName;
    try
      Preview.Picture.LoadFromFile(FileName);
      Modified := True;
    except
      on E: Exception do begin
        MessageDlg(oisErrorLoadingImage,
          Format(oisErrorLoadingImage2, ['"', FileName, '"', #13, E.Message]),
          mtError, [mbOk], 0);
        exit;
      end;
    end;
  end;
  StoreIDEFileDialog(OpenDialog);
  
  SaveButton.Enabled := False;
  if Assigned(Preview.Picture.Graphic) then
    if not Preview.Picture.Graphic.Empty then
      SaveButton.Enabled := True;
end;

procedure TGraphicPropertyEditorForm.SaveButtonClick(Sender: TObject);
begin
  InitIDEFileDialog(SaveDialog);
  if SaveDialog.Execute then
    Preview.Picture.SaveToFile(SaveDialog.FileName);
  StoreIDEFileDialog(SaveDialog);
end;

procedure TGraphicPropertyEditorForm.SetModified(const AValue: Boolean);
begin
  if FModified = AValue then Exit;
  FModified := AValue;
end;

initialization
  {$I graphicpropedit.lrs}

end.

