unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ImgList, ComCtrls,
  Menus, StdCtrls, LCLIntf;

type
  TForm1 = class(TForm)
  private
    IL: TImageList;
    procedure ILOnGetWidthForImagePPI(Sender: TCustomImageList; AImageWidth,
      APPI: Integer; var AResultWidth: Integer);
  protected
    procedure DoCreate; override;
  end;

  TImageListHelper = class helper for TImageList
  public
    procedure AddPNGsFromFiles(const aFileNames: array of string);
    procedure AddLazarusPNGs(const aBaseFileName: string);
    procedure AddIconFromFile(const aFileName: string);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TImageListHelper }

procedure TImageListHelper.AddPNGsFromFiles(const aFileNames: array of string);
var
  xPNG: array of TCustomBitmap;
  xFileName: string;
  I: Integer;
begin
  SetLength(xPNG, Length(aFileNames));
  try
    for I := 0 to High(aFileNames) do
    begin
      xFileName := aFileNames[I];
      xPNG[I] := TPortableNetworkGraphic.Create;
      xPNG[I].LoadFromFile(xFileName);
    end;
    AddMultipleResolutions(xPNG);
  finally
    for I := 0 to High(aFileNames) do
      xPNG[I].Free;
  end;
end;

procedure TImageListHelper.AddIconFromFile(const aFileName: string);
var
  xICO: TIcon;
begin
  xICO := TIcon.Create;
  try
    xICO.LoadFromFile(aFileName);
    AddIcon(xICO);
  finally
    xICO.Free;
  end;
end;

procedure TImageListHelper.AddLazarusPNGs(const aBaseFileName: string);
var
  A: array of string;
const
  BaseDir = '..'+PathDelim+'..'+PathDelim+'images'+PathDelim+'menu'+PathDelim;
begin
  SetLength(A, 3);
  A[0] := BaseDir+aBaseFileName+'.png';
  A[1] := BaseDir+aBaseFileName+'_150.png';
  A[2] := BaseDir+aBaseFileName+'_200.png';

  AddPNGsFromFiles(A);
end;

{ TForm1 }

procedure TForm1.DoCreate;
var
  MI, MI2: TMenuItem;
  MM: TMainMenu;
  TS: TTabSheet;
  Lbl: TLabel;
  PC: TPageControl;
  TB16, TB24: TToolBar;
  Btn: TToolButton;
  I: Integer;
  LV: TListView;
  PM: TPopupMenu;
begin
  inherited DoCreate;

  IL := TImageList.Create(Self);
  IL.RegisterResolutions([16, 24, 32]);
  IL.AddLazarusPNGs('menu_undo');
  IL.AddLazarusPNGs('menu_redo');
  IL.AddIconFromFile('..'+PathDelim+'..'+PathDelim+'images'+PathDelim+'mainiconproject.ico');
  IL.OnGetWidthForPPI := @ILOnGetWidthForImagePPI;
  IL.Scaled := True;

  TB16 := TToolBar.Create(Self);
  TB16.Parent := Self;
  TB16.Images := IL;
  TB16.ImagesWidth := 16;
  TB16.AutoSize := True;

  TB24 := TToolBar.Create(Self);
  TB24.Parent := Self;
  TB24.Images := IL;
  TB24.ImagesWidth := 24;
  TB24.ButtonWidth := Scale96ToForm(30);
  TB24.ButtonHeight := Scale96ToForm(30);
  TB24.Height := TB24.ButtonHeight;
  TB24.Caption := '24px image';

  for I := 0 to 2 do
  begin
    Btn := TToolButton.Create(Self);
    Btn.Parent := TB16;
    Btn.ImageIndex := I;
  end;
  Lbl := TLabel.Create(Self);
  Lbl.Parent := TB16;
  Lbl.Caption := '16px image';

  for I := 0 to 2 do
  begin
    Btn := TToolButton.Create(Self);
    Btn.Parent := TB24;
    Btn.ImageIndex := I;
  end;
  Lbl := TLabel.Create(Self);
  Lbl.Parent := TB24;
  Lbl.Caption := '24px image';


  PC := TPageControl.Create(Self);
  PC.Images := IL;
  PC.ImagesWidth := 32;
  TS := PC.AddTabSheet;
  TS.ImageIndex := 0;
  Lbl := TLabel.Create(Self);
  Lbl.Caption := 'All controls&&images are served by 1 image list and no DPI-aware runtime code!';
  Lbl.WordWrap := True;
  Lbl.Parent := TS;
  Lbl.BorderSpacing.Around := 6;
  Lbl.Align := alClient;

  TS := PC.AddTabSheet;
  TS.ImageIndex := 1;
  PC.Parent := Self;
  PC.Align := alClient;

  MM := TMainMenu.Create(Self);
  MM.Images := IL;

  MI := TMenuItem.Create(Self);
  MI.Caption := 'main menu';
  MI.ImageIndex := 0;
  MI.SubMenuImages := IL;
  MI.SubMenuImagesWidth := 32;
  MM.Items.Add(MI);

  MI2 := TMenuItem.Create(Self);
  MI2.Caption := 'sub1';
  MI2.ImageIndex := 0;
  MI.Add(MI2);

  MI2 := TMenuItem.Create(Self);
  MI2.Caption := 'sub2';
  MI2.ImageIndex := 1;
  MI.Add(MI2);

  PM := TPopupMenu.Create(Self);
  PM.Images := IL;

  MI2 := TMenuItem.Create(Self);
  MI2.Caption := 'sub1';
  MI2.ImageIndex := 0;
  PM.Items.Add(MI2);

  MI2 := TMenuItem.Create(Self);
  MI2.Caption := 'sub2';
  MI2.ImageIndex := 1;
  PM.Items.Add(MI2);

  TB16.PopupMenu := PM;

  LV := TListView.Create(Self);
  LV.Parent := TS;
  LV.Align := alClient;
  LV.LargeImages := IL;
  LV.LargeImagesWidth := 46;
  LV.ViewStyle := vsIcon;
  with LV.Items.Add do
  begin
    ImageIndex := 0;
    Caption := 'one';
  end;
  with LV.Items.Add do
  begin
    ImageIndex := 1;
    Caption := 'two';
  end;

  TS := PC.AddTabSheet;
  TS.ImageIndex := 2;
end;

procedure TForm1.ILOnGetWidthForImagePPI(Sender: TCustomImageList; AImageWidth,
  APPI: Integer; var AResultWidth: Integer);
begin
  if (AImageWidth=24) and (AResultWidth=36) then
    AResultWidth := 32;
end;

end.

