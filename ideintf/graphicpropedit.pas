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
    FileOpenAction: TAction;
    FileSaveAction: TAction;
    ClearAction: TEditDelete;
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
    procedure ClearActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FileOpenActionExecute(Sender: TObject);
    procedure FileSaveActionExecute(Sender: TObject);
    procedure FileSaveActionUpdate(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
  private
    FFileName: String;
    FModified: Boolean;
    function GetGraphic: TGraphic;
    procedure SetGraphic(Value: TGraphic);
    procedure SetCaptionDetail(Value: String);
  public
    property CaptionDetail: String write SetCaptionDetail;
    property FileName: String read FFileName;
    property Modified: Boolean read FModified;
    property Graphic: TGraphic read GetGraphic write SetGraphic;
  end;

implementation

{$R *.lfm}

{ TGraphicPropertyEditorForm }

procedure TGraphicPropertyEditorForm.FormCreate(Sender: TObject);
begin
  Caption := oisLoadImageDialog;
  GroupBox1.Caption:=oisPEPicture;
  OkCancelButtonPanel.OKButton.Caption := oisOK;
  OkCancelButtonPanel.CancelButton.Caption := oisCancel;
  CopyAction.Caption := oiStdActEditCopyHeadLine;
  PasteAction.Caption := oiStdActEditPasteHeadLine;
  FileOpenAction.Caption := oisLoad;
  FileSaveAction.Caption := oisSave;
  ClearAction.Caption := oisClear;
  OpenDialog.Title:=oisPEOpenImageFile;
  SaveDialog.Title:=oisPESaveImageAs;
  ImagePreview.OnPictureChanged:=@PictureChanged;
end;

procedure TGraphicPropertyEditorForm.ClearActionExecute(Sender: TObject);
begin
  ImagePreview.Picture.Clear;
end;

procedure TGraphicPropertyEditorForm.CopyActionExecute(Sender: TObject);
begin
  Clipboard.Assign(ImagePreview.Picture.Graphic);
end;

procedure TGraphicPropertyEditorForm.CopyActionUpdate(Sender: TObject);
begin
  CopyAction.Enabled := ImagePreview.Picture.Graphic <> nil;
end;

procedure TGraphicPropertyEditorForm.FileSaveActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := (Graphic <> nil) and (not Graphic.Empty);
end;

procedure TGraphicPropertyEditorForm.PasteActionExecute(Sender: TObject);
begin
  ImagePreview.Picture.Assign(Clipboard);
end;

procedure TGraphicPropertyEditorForm.PasteActionUpdate(Sender: TObject);
begin
  PasteAction.Enabled := Clipboard.HasPictureFormat;
end;

procedure TGraphicPropertyEditorForm.FileOpenActionExecute(Sender: TObject);
begin
  InitIDEFileDialog(OpenDialog);
  if OpenDialog.Execute then
  begin
    try
      ImagePreview.Picture.LoadFromFile(OpenDialog.FileName);
      FFileName := OpenDialog.FileName; // OnPictureChange clears the field.
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
end;

procedure TGraphicPropertyEditorForm.PictureChanged(Sender: TObject);
begin
  FModified := True;
  FFileName := '';
  if Graphic <> nil then
    ScrollBox.Hint := Graphic.ClassName
  else
    ScrollBox.Hint := '';
end;

procedure TGraphicPropertyEditorForm.FileSaveActionExecute(Sender: TObject);
begin
  InitIDEFileDialog(SaveDialog);
  if SaveDialog.Execute then
    if SaveDialog.FilterIndex > 1 then
      ImagePreview.Picture.SaveToFile(SaveDialog.FileName, SaveDialog.GetFilterExt)
    else
      ImagePreview.Picture.SaveToFile(SaveDialog.FileName);
      
  StoreIDEFileDialog(SaveDialog);
end;

function TGraphicPropertyEditorForm.GetGraphic: TGraphic;
begin
  Result := ImagePreview.Picture.Graphic;
end;

procedure TGraphicPropertyEditorForm.SetGraphic(Value: TGraphic);
begin
  ImagePreview.Picture.Assign(Value);
  FModified := False;
end;

procedure TGraphicPropertyEditorForm.SetCaptionDetail(Value: String);
begin
  Caption := oisLoadImageDialog + ' - ' + Value;
end;

end.

