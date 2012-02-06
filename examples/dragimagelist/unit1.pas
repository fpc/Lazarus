unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Panel1: TPanel;
    procedure Button1StartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
  private
    { private declarations }
  public
    { public declarations }
  end; 
  
  { TMyDragObject }

  TMyDragObject = class(TDragControlObject)
  private
    FDragImages: TDragImageList;
  protected
    function GetDragImages: TDragImageList; override;
  public
    constructor Create(AControl: TControl); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.Button1StartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  // user started dragging on Button1
  // create our own TDragControlObject which provides an image.
  DragObject := TMyDragObject.Create(Sender as TControl);
end;

procedure TForm1.Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  ShowMessage('Congratulations. You dropped button on me :)')
end;

procedure TForm1.Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Control: TControl;
begin
  if Source is TControl then
    Control := Source as TControl
  else
  if Source is TDragControlObject then
    Control := (Source as TDragControlObject).Control
  else
    Control := nil;
  Accept := Control is TButton;
end;

{ TMyDragObject }

function TMyDragObject.GetDragImages: TDragImageList;
begin
  Result := FDragImages;
end;

constructor TMyDragObject.Create(AControl: TControl);
var
  Bitmap: TBitmap;
begin
  inherited Create(AControl);
  FDragImages := TDragImageList.Create(AControl);
  AlwaysShowDragImages := True;

  Bitmap := TBitmap.Create;
  Bitmap.Width := AControl.Width;
  Bitmap.Height := AControl.Height;
  if AControl is TWinControl then
    (AControl as TWinControl).PaintTo(Bitmap.Canvas, 0, 0);
  FDragImages.Width := Bitmap.Width;
  FDragImages.Height := Bitmap.Height;
  FDragImages.Add(Bitmap, nil);
  FDragImages.DragHotspot := Point(Bitmap.Width, Bitmap.Height);
  Bitmap.Free;
end;

destructor TMyDragObject.Destroy;
begin
  FDragImages.Free;
  inherited Destroy;
end;

initialization
  {$I unit1.lrs}

end.

