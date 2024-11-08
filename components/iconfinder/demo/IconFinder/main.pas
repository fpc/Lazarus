unit main;

{$mode objfpc}{$H+}

interface

uses
  LazLogger,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ImgList,
  StdCtrls, Types, IconThumbnails;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ImageList1: TImageList;
    PaintBox1: TPaintBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  IconFinder;

function RunIconFinder(AImageList: TCustomImageList; var AImgIndex: Integer): boolean;
var
  F: TIconFinderForm;
  item: TIconItem;
begin
  F := TIconFinderForm.Create(nil);
  try
    F.Position := poMainFormCenter;
    F.ImageList := AImageList;
    F.ImageIndex := AImgIndex;
    Result := F.ShowModal = mrYes;
    if Result then
      AImgIndex := F.ImageIndex;
  finally
    F.Free;
  end;
end;


{ TMainForm }

procedure TMainForm.PaintBox1Paint(Sender: TObject);
const
  DIST = 8;
var
  sz: TSize;
  ppi: Integer;
  i: Integer;
  x, y: Integer;
begin
  Paintbox1.Canvas.Brush.Color := clWhite;
  Paintbox1.Canvas.FillRect(0, 0, Paintbox1.Width, Paintbox1.Height);

  ppi := Font.PixelsPerInch;
  sz := ImageList1.SizeForPPI[0, ppi];

  x := DIST;
  y := DIST;
  for i := 0 to ImageList1.Count-1 do
  begin
    ImageList1.DrawForPPI(Paintbox1.Canvas, x, y, i, 16, ppi, 1.0);
    inc(x, sz.CX + DIST);
    if x > Paintbox1.Width then
    begin
      x := DIST;
      inc(y, sz.CY + DIST);
    end;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  idx: Integer;
begin
  idx := -1;
  if RunIconFinder(ImageList1, idx) then
    Paintbox1.Invalidate;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  idx: Integer;
begin
  idx := ImageList1.Count-1;
  if RunIconFinder(ImageList1, idx) then
    Paintbox1.Invalidate;
end;

end.

