{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
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
  Classes, SysUtils,
  // LCL
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ButtonPanel, ExtDlgs,
  ActnList, StdActns, Clipbrd,
  // IdeIntf
  IDEDialogs, ObjInspStrConsts, IDEWindowIntf;

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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure ImagePreviewPaintBackground({%H-}ASender: TObject; ACanvas: TCanvas;
      ARect: TRect);
    procedure PasteActionExecute(Sender: TObject);
    procedure PasteActionUpdate(Sender: TObject);
    procedure ClearActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FileOpenActionExecute(Sender: TObject);
    procedure FileSaveActionExecute(Sender: TObject);
    procedure FileSaveActionUpdate(Sender: TObject);
    procedure PictureChanged(Sender: TObject);
    procedure ScrollBoxResize(Sender: TObject);
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
  IDEDialogLayoutList.ApplyLayout(Self);
end;

procedure TGraphicPropertyEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
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

procedure TGraphicPropertyEditorForm.ImagePreviewPaintBackground(
  ASender: TObject; ACanvas: TCanvas; ARect: TRect);
const
  cell=8; //8 pixels is usual checkers size
var
  bmp: TBitmap;
  i, j: integer;
begin
  //Paint checkers BG picture for transparent image
  bmp:= TBitmap.Create;
  try
    bmp.PixelFormat:= pf24bit;
    bmp.SetSize(ARect.Right-ARect.Left, ARect.Bottom-ARect.Top);
    bmp.Canvas.Brush.Color:= clWhite;
    bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);
    bmp.Canvas.Brush.Color:= clLtGray;

    for i:= 0 to bmp.Width div cell do
      for j:= 0 to bmp.Height div cell do
        if not (Odd(i) xor Odd(j)) then
          bmp.Canvas.FillRect(i*cell, j*cell, (i+1)*cell, (j+1)*cell);

    ACanvas.CopyRect(ARect, bmp.Canvas, Rect(0, 0, bmp.Width, bmp.Height));
  finally
    FreeAndNil(bmp);
  end;
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
          Format(oisErrorLoadingImage2, [FileName, LineEnding, E.Message]),
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

  ScrollBoxResize(Self);
end;

procedure TGraphicPropertyEditorForm.ScrollBoxResize(Sender: TObject);
begin
  if ImagePreview.Width<ScrollBox.ClientWidth then
    ImagePreview.Left:= (ScrollBox.ClientWidth-ImagePreview.Width) div 2
  else
    ImagePreview.Left:= 0;

  if ImagePreview.Height<ScrollBox.ClientHeight then
    ImagePreview.Top:= (ScrollBox.ClientHeight-ImagePreview.Height) div 2
  else
    ImagePreview.Top:= 0;
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

