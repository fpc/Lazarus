{ Copyright (C) 2004 Mattias Gaertner

  Example for loading and saving images.
  
  Important:
    This example uses the ImagesForLazarus Package (see in the directory above).
    You must first open once this package so that the IDE knows, where to find
    the lpk file.
    
  See the README.txt.


  This program is free software; you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU General Public License along with
  this program; if not, write to the Free Software Foundation, Inc., 59 Temple
  Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit MainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, ExtDlgs;

type
  TImagesExampleForm = class(TForm)
    LoadImageButton: TButton;
    SaveImageButton: TButton;
    LoadJPEGButton: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    SaveJPEGButton: TButton;
    ImageGroupBox: TGroupBox;
    Image1: TImage;
    SavePictureDialog1: TSavePictureDialog;
    procedure LoadJPEGButtonClick(Sender: TObject);
    procedure LoadImageButtonClick(Sender: TObject);
    procedure SaveJPEGButtonClick(Sender: TObject);
    procedure SaveImageButtonClick(Sender: TObject);
  private
    procedure UpdateInfo(const Filename: string);
  end;

var
  ImagesExampleForm: TImagesExampleForm;

implementation

uses
{Units of package "Images For Lazarus"}
  LazPNM, LazJPG, LazTGA, LazPNG, LazXPM, LazBMP;

{ TImagesExampleForm }

procedure TImagesExampleForm.LoadJPEGButtonClick(Sender: TObject);
var
  JPEG: TJPGImage;
begin
  OpenPictureDialog1.Options:=OpenPictureDialog1.Options+[ofFileMustExist];
  if not OpenPictureDialog1.Execute then exit;
  try
    //--------------------------------------------------------------------------
    // Create a TJPEGImage and load the file, then copy it to the TImage.
    // A TJPEGImage can only load jpeg images.
    JPEG:=TJPGImage.Create;
    try
      JPEG.LoadFromFile(OpenPictureDialog1.Filename);
      // copy jpeg content to a TImage
      Image1.Picture.Assign(JPEG);
    finally
      JPEG.Free;
    end;
    //--------------------------------------------------------------------------
    UpdateInfo(OpenPictureDialog1.Filename);
  except
    on E: Exception do begin
      MessageDlg('Error','Error: '+E.Message,mtError,[mbOk],0);
    end;
  end;
end;

procedure TImagesExampleForm.LoadImageButtonClick(Sender: TObject);
begin
  OpenPictureDialog1.Options:=OpenPictureDialog1.Options+[ofFileMustExist];
  if not OpenPictureDialog1.Execute then exit;
  try
    //--------------------------------------------------------------------------
    // Loading directly into a TImage. This will load any registered image
    // format. .bmp, .xpm, .png are the standard LCL formats.
    // The jpeg units register .jpeg and .jpg.
    Image1.Picture.LoadFromFile(OpenPictureDialog1.Filename);
    //--------------------------------------------------------------------------

    UpdateInfo(OpenPictureDialog1.Filename);
  except
    on E: Exception do begin
      MessageDlg('Error','Error: '+E.Message,mtError,[mbOk],0);
    end;
  end;
end;

procedure TImagesExampleForm.SaveJPEGButtonClick(Sender: TObject);
var
  JPEG: TJPGImage;
begin
  if Image1.Picture.Graphic=nil then begin
    MessageDlg('No image','Please open an image, before save',mtError,
      [mbOk],0);
    exit;
  end;
  
  SavePictureDialog1.Options:=SavePictureDialog1.Options+[ofPathMustExist];
  if not SavePictureDialog1.Execute then exit;
  try
    //--------------------------------------------------------------------------
    // Create a TImage1 and copy the TImage into it. Then save to file.
    // This will ignore the file extension. TImage1 will always save as jpeg.
    JPEG:=TJPGImage.Create;
    try
      // copy content of the TImage to jpeg
      JPEG.Assign(Image1.Picture.Graphic);
      // save to file
      JPEG.SaveToFile(SavePictureDialog1.Filename);
    finally
      JPEG.Free;
    end;
    //--------------------------------------------------------------------------

    UpdateInfo(SavePictureDialog1.Filename);
  except
    on E: Exception do begin
      MessageDlg('Error','Error: '+E.Message,mtError,[mbOk],0);
    end;
  end;
end;

procedure TImagesExampleForm.SaveImageButtonClick(Sender: TObject);
begin
  if Image1.Picture.Graphic=nil then begin
    MessageDlg('No image','Please open an image, before save',mtError,
      [mbOk],0);
    exit;
  end;

  SavePictureDialog1.Options:=SavePictureDialog1.Options+[ofPathMustExist];
  if not SavePictureDialog1.Execute then exit;
  try
    //--------------------------------------------------------------------------
    // Saving directly from a TImage to a file. This will save in any registered
    // image format. .bmp, .xpm, .png are the standard LCL formats.
    // The jpeg units register .jpeg and .jpg.
    // So, saving as file1.jpg will save as jpeg, while saving a file1.bmp will
    // save as bmp.
    Image1.Picture.SaveToFile(SavePictureDialog1.Filename);

    //--------------------------------------------------------------------------

    UpdateInfo(SavePictureDialog1.Filename);
  except
    on E: Exception do begin
      MessageDlg('Error','Error: '+E.Message,mtError,[mbOk],0);
    end;
  end;
end;

procedure TImagesExampleForm.UpdateInfo(const Filename: string);
var
  Info: String;
begin
  if Image1.Picture.Graphic<>nil then begin
    Info:=Image1.Picture.Graphic.ClassName+':'+Filename;
  end else begin
    Info:=Filename;
  end;
  ImageGroupBox.Caption:=Info;
end;

initialization
//  LazPNG.Register;
//  LazXPM.Register;
  LazBMP.Register;
  LazPNM.Register;
  LazTGA.Register;
  LazJPG.Register;
  {$I mainform.lrs}

finalization
//  LazPNG.UnRegister;
//  LazXPM.UnRegister;
  LazBMP.UnRegister;
  LazPNM.UnRegister;
  LazTGA.UnRegister;
  LazJPG.UnRegister;

end.

