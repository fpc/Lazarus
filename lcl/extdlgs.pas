{
 /***************************************************************************
                               extdlgs.pas
                               -----------
                Component Library Extended dialogs Controls


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit ExtDlgs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VCLGlobals, LCLType, LCLStrConsts, Controls, Dialogs,
  Graphics, ExtCtrls, StdCtrls, Forms, FileCtrl;
  
type

  { TPreviewFileControl }

  TPreviewFileDialog = class;

  TPreviewFileControl = class(TWinControl)
  private
    FPreviewFileDialog: TPreviewFileDialog;
  protected
    procedure SetPreviewFileDialog(const AValue: TPreviewFileDialog);
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(TheOwner: TComponent); override;
    property PreviewFileDialog: TPreviewFileDialog read FPreviewFileDialog
                                                   write SetPreviewFileDialog;
  end;


  { TPreviewFileDialog }

  TPreviewFileDialog = class(TOpenDialog)
  private
    FPreviewFileControl: TPreviewFileControl;
  protected
    procedure CreatePreviewControl; virtual;
    procedure InitPreviewControl; virtual;
  public
    function Execute: boolean; override;
    constructor Create(TheOwner: TComponent); override;
    property PreviewFileControl: TPreviewFileControl read FPreviewFileControl;
  end;
  
  
  { TOpenPictureDialog }
  
  TOpenPictureDialog = class(TPreviewFileDialog)
  private
    FDefaultFilter: string;
    FImageCtrl: TImage;
    FPictureGroupBox: TGroupBox;
    FPreviewFilename: string;
  protected
    function  IsFilterStored: Boolean; virtual;
    procedure PreviewKeyDown(Sender: TObject; var Key: word); virtual;
    procedure PreviewClick(Sender: TObject); virtual;
    procedure DoClose; override;
    procedure DoSelectionChange; override;
    procedure DoShow; override;
    property ImageCtrl: TImage read FImageCtrl;
    property PictureGroupBox: TGroupBox read FPictureGroupBox;
    procedure InitPreviewControl; override;
    procedure ClearPreview; virtual;
    procedure UpdatePreview; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    property DefaultFilter: string read FDefaultFilter;
  published
    property Filter stored IsFilterStored;
  end;
  
  
  { TSavePictureDialog }
  
  TSavePictureDialog = class(TOpenPictureDialog)
  public
    constructor Create(TheOwner: TComponent); override;
  end;
  
  
procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Dialogs',[TOpenPictureDialog,TSavePictureDialog]);
end;

{ TPreviewFileControl }

procedure TPreviewFileControl.SetPreviewFileDialog(
  const AValue: TPreviewFileDialog);
begin
  if FPreviewFileDialog=AValue then exit;
  FPreviewFileDialog:=AValue;
end;

procedure TPreviewFileControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style and not WS_CHILD;
end;

constructor TPreviewFileControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompStyle:=csPreviewFileControl;
  SetInitialBounds(0,0,200,200);
end;

{ TPreviewFileDialog }

procedure TPreviewFileDialog.CreatePreviewControl;
begin
  if FPreviewFileControl<>nil then exit;
  FPreviewFileControl:=TPreviewFileControl.Create(Self);
  FPreviewFileControl.PreviewFileDialog:=Self;
  InitPreviewControl;
end;

procedure TPreviewFileDialog.InitPreviewControl;
begin
  FPreviewFileControl.Name:='PreviewFileControl';
end;

function TPreviewFileDialog.Execute: boolean;
begin
  CreatePreviewControl;
  Result:=inherited Execute;
end;

constructor TPreviewFileDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompStyle:=csPreviewFileDialog;
end;

{ TOpenPictureDialog }

function TOpenPictureDialog.IsFilterStored: Boolean;
begin
  Result := (Filter<>FDefaultFilter);
end;

procedure TOpenPictureDialog.PreviewKeyDown(Sender: TObject; var Key: word);
begin
  if Key = VK_ESCAPE then TForm(Sender).Close;
end;

procedure TOpenPictureDialog.PreviewClick(Sender: TObject);
begin

end;

procedure TOpenPictureDialog.DoClose;
begin
  ClearPreview;
  inherited DoClose;
end;

procedure TOpenPictureDialog.DoSelectionChange;
begin
  UpdatePreview;
  inherited DoSelectionChange;
end;

procedure TOpenPictureDialog.DoShow;
begin
  ClearPreview;
  inherited DoShow;
end;

procedure TOpenPictureDialog.InitPreviewControl;
begin
  inherited InitPreviewControl;
  FPictureGroupBox.Parent:=PreviewFileControl;
end;

procedure TOpenPictureDialog.ClearPreview;
begin
  FPictureGroupBox.Caption:='None';
  FImageCtrl.Picture:=nil;
end;

procedure TOpenPictureDialog.UpdatePreview;
var
  CurFilename: String;
  FileIsValid: boolean;
begin
  CurFilename := FileName;
  if CurFilename = FPreviewFilename then exit;

  FPreviewFilename := CurFilename;
  FileIsValid := FileExists(FPreviewFilename)
                 and (not DirectoryExists(FPreviewFilename))
                 and FileIsReadable(FPreviewFilename);
  if FileIsValid then
    try
      FImageCtrl.Picture.LoadFromFile(FPreviewFilename);
      FPictureGroupBox.Caption := Format('(%dx%d)',
        [FImageCtrl.Picture.Width, FImageCtrl.Picture.Height]);
    except
      FileIsValid := False;
    end;
  if not FileIsValid then
    ClearPreview;
end;

constructor TOpenPictureDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDefaultFilter := 'All files ('+GetAllFilesMask+')|'+GetAllFilesMask+'|'
                    +GraphicFilter(TGraphic);
  Filter:=FDefaultFilter;

  FPictureGroupBox:=TGroupBox.Create(Self);
  with FPictureGroupBox do begin
    Name:='FPictureGroupBox';
    Align:=alClient;
  end;
  
  FImageCtrl:=TImage.Create(Self);
  with FImageCtrl do begin
    Name:='FImageCtrl';
    Parent:=FPictureGroupBox;
    Align:=alClient;
    Center:=true;
    Proportional:=true;
  end;
end;

{ TSavePictureDialog }

constructor TSavePictureDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Title:=rsfdFileSaveAs;
end;

end.

