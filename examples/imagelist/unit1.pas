unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, GraphType, Dialogs,
  ExtCtrls, FileUtil, StdCtrls, ExtDlgs;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    ImageList1: TImageList;
    ListBox1: TListBox;
    OpenPictureDialog1: TOpenPictureDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function GetImageMap(AEffect: TGraphicsDrawEffect): TImage;
    procedure ListBox1SelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
  public
    procedure ShowImages(AIndex: Integer);
    procedure AddFile(AFileName: String);
    property ImageMap[AEffect: TGraphicsDrawEffect]: TImage read GetImageMap;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

function LoadBitmapFromFile(AFileName: String): TCustomBitmap;
var
  Stream: TStream;
  GraphicClass: TGraphicClass;
begin
  Result := nil;
  Stream := nil;
  try
    Stream := TFileStream.Create(UTF8ToSys(AFileName), fmOpenRead or fmShareDenyNone);
    GraphicClass := GetGraphicClassForFileExtension(ExtractFileExt(AFileName));
    if (GraphicClass <> nil) and (GraphicClass.InheritsFrom(TCustomBitmap)) then
    begin
      Result := TCustomBitmap(GraphicClass.Create);
      Result.LoadFromStream(Stream);
    end;
  finally
    Stream.Free;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  AddFile(ExtractFilePath(ParamStrUTF8(0)) + 'images\edit-clear.png');
  AddFile(ExtractFilePath(ParamStrUTF8(0)) + 'images\edit-find-replace.png');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
    AddFile(OpenPictureDialog1.FileName);
end;

function TForm1.GetImageMap(AEffect: TGraphicsDrawEffect): TImage;
begin
  case AEffect of
    gdeNormal: Result := Image1;
    gdeDisabled: Result := Image2;
    gdeHighlighted: Result := Image3;
    gdeShadowed: Result := Image4;
    gde1Bit: Result := Image5;
  else
    Result := nil;
  end;
end;

procedure TForm1.ListBox1SelectionChange(Sender: TObject; User: boolean);
begin
  if ListBox1.ItemIndex <> -1 then
    ShowImages(ListBox1.ItemIndex);
end;

procedure TForm1.ShowImages(AIndex: Integer);
var
  AEffect: TGraphicsDrawEffect;
  Bmp: TBitmap;
begin
  for AEffect := Low(TGraphicsDrawEffect) to High(TGraphicsDrawEffect) do
  begin
    Bmp := TBitmap.Create;
    ImageList1.GetBitmap(AIndex, Bmp, AEffect);
    if ImageMap[AEffect] <> nil then
      ImageMap[AEffect].Picture.Assign(Bmp);
    Bmp.Free;
  end;
end;

procedure TForm1.AddFile(AFileName: String);
var
  bmp: TCustomBitmap;
begin
  bmp := LoadBitmapFromFile(AFileName);
  if bmp <> nil then
  begin
    ImageList1.Add(bmp, nil);
    ListBox1.Items.Add(ExtractFileName(AFileName));
    if ListBox1.ItemIndex = -1 then
      ListBox1.ItemIndex := 0;
  end;
  bmp.Free;
end;

end.

