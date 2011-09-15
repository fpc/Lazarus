{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ButtonPanel, ExtDlgs, ActnList, StdActns, Clipbrd,
  IDEDialogs, ObjInspStrConsts;

type

  { TGraphicPropertyEditorForm }

  TGraphicPropertyEditorForm = class(TForm)
    ActionList: TActionList;
    CopyButton: TButton;
    PasteButton: TButton;
    CopyAction: TEditCopy;
    PasteAction: TEditPaste;
    OkCancelButtonPanel: TButtonPanel;
    ImagePreview: TImage;
    LoadButton: TButton;
    LoadSaveBtnPanel: TPanel;
    OpenDialog: TOpenPictureDialog;
    SaveButton: TButton;
    ClearButton: TButton;
    GroupBox1: TGroupBox;
    SaveDialog: TSavePictureDialog;
    ScrollBox: TScrollBox;
    procedure CopyActionExecute(Sender: TObject);
    procedure CopyActionUpdate(Sender: TObject);
    procedure PasteActionExecute(Sender: TObject);
    procedure PasteActionUpdate(Sender: TObject);
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

{$R *.lfm}

{ TGraphicPropertyEditorForm }

procedure TGraphicPropertyEditorForm.FormCreate(Sender: TObject);
begin
  FileName := '';
  Caption := oisLoadImageDialog;
  GroupBox1.Caption:=oisPEPicture;
  OkCancelButtonPanel.OKButton.Caption := oisOK;
  OkCancelButtonPanel.CancelButton.Caption := oisCancel;
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

procedure TGraphicPropertyEditorForm.CopyActionExecute(Sender: TObject);
begin
  Clipboard.Assign(ImagePreview.Picture.Graphic);
end;

procedure TGraphicPropertyEditorForm.CopyActionUpdate(Sender: TObject);
begin
  CopyAction.Enabled := ImagePreview.Picture.Graphic <> nil;
end;

procedure TGraphicPropertyEditorForm.PasteActionExecute(Sender: TObject);
begin
  ImagePreview.Picture.Assign(Clipboard);
end;

procedure TGraphicPropertyEditorForm.PasteActionUpdate(Sender: TObject);
begin
  PasteAction.Enabled := Clipboard.HasPictureFormat;
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
    if SaveDialog.FilterIndex > 1 then
      Preview.Picture.SaveToFile(SaveDialog.FileName, SaveDialog.GetFilterExt)
    else
      Preview.Picture.SaveToFile(SaveDialog.FileName);
      
  StoreIDEFileDialog(SaveDialog);
end;

procedure TGraphicPropertyEditorForm.SetModified(const AValue: Boolean);
begin
  if FModified = AValue then Exit;
  FModified := AValue;
end;

end.

