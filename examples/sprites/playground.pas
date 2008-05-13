unit PlayGround;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LMessages,
  ExtCtrls;

type

  { TPictureControl }

  TPictureControl = class(TCustomControl)
    procedure PictureChanged(Sender: TObject);
  private
    FPicture: TPicture;
    procedure SetPicture(const AValue: TPicture);
    procedure WMEraseBkgnd(var Msg: TLMessage); message LM_ERASEBKGND;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Picture: TPicture read FPicture write SetPicture;
  end;

  { TPlayGroundForm }

  TPlayGroundForm = class(TForm)
    Timer1: TTimer;
    procedure PlayGroundFormClose(Sender: TObject; var CloseAction: TCloseAction
      );
    procedure PlayGroundFormCreate(Sender: TObject);
    procedure PlayGroundFormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure UpdateImage;
  public
    PictureControl: TPictureControl;
    SpriteImg: TCustomBitmap;
    BackgroundImg: TCustomBitmap;
    BufferImg: TCustomBitmap;
  end;

var
  PlayGroundForm: TPlayGroundForm;

implementation

{ TPlayGroundForm }

procedure TPlayGroundForm.PlayGroundFormCreate(Sender: TObject);
begin
  PictureControl:=TPictureControl.Create(Self);
  with PictureControl do begin
    Parent:=Self;
    Align:=alClient;
  end;

  SpriteImg:=TPortableNetworkGraphic.Create;
  BackgroundImg:=TPortableNetworkGraphic.Create;
  BufferImg:=TBitmap.Create;

  SpriteImg.LoadFromFile(SetDirSeparators('../../images/ide_icon48x48.png'));
  BackgroundImg.LoadFromFile(SetDirSeparators('../../images/splash_logo.png'));
  BufferImg.Width:=BackgroundImg.Width;
  BufferImg.Height:=BackgroundImg.Height;
  
  UpdateImage;
end;

procedure TPlayGroundForm.PlayGroundFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Timer1.Enabled:=false;
end;

procedure TPlayGroundForm.PlayGroundFormDestroy(Sender: TObject);
begin
  SpriteImg.Free;
  BackgroundImg.Free;
  BufferImg.Free;
end;

procedure TPlayGroundForm.Timer1Timer(Sender: TObject);
begin
  if csDestroying in ComponentState then exit;
  UpdateImage;
end;

procedure TPlayGroundForm.UpdateImage;
var
  DestImg: TBitmap;
  t: Double;
  x: Int64;
  y: Integer;
  CenterX: Integer;
  CenterY: Integer;
begin
  // paint first on the buffer
  
  // paint background
  //writeln('TPlayGroundForm.UpdateImage A');
  BufferImg.Canvas.CopyRect(Rect(0,0,BufferImg.Width,BufferImg.Height),
       BackgroundImg.Canvas,Rect(0,0,BackgroundImg.Width,BackgroundImg.Height));
  // paint sprite
  CenterX:=BufferImg.Width div 2;
  CenterY:=BufferImg.Height div 2;
  t:=Now*86400;
  x:=CenterX+round(cos(t)*CenterX*2/3)-(SpriteImg.Width div 2);
  y:=CenterY+round(sin(t*0.7)*CenterY*2/3)-(SpriteImg.Height div 2);
  //writeln('TPlayGroundForm.UpdateImage B ',x,',',y,' ',t);
  BufferImg.Canvas.Draw(x, y, SpriteImg);
  //writeln('TPlayGroundForm.UpdateImage C');

  // copy to image
  DestImg:=PictureControl.Picture.Bitmap;
  DestImg.Width:=BufferImg.Width;
  DestImg.Height:=BufferImg.Height;
  DestImg.Canvas.Draw(0,0,BufferImg);
  //writeln('TPlayGroundForm.UpdateImage D');
end;

{ TPictureControl }

procedure TPictureControl.SetPicture(const AValue: TPicture);
begin
  if FPicture=AValue then exit;
  FPicture.Assign(AValue);
end;

procedure TPictureControl.WMEraseBkgnd(var Msg: TLMessage);
begin
  Msg.Result := 1;
end;

procedure TPictureControl.PictureChanged(Sender: TObject);
begin
  Invalidate;
end;

constructor TPictureControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPicture:=TPicture.Create;
  FPicture.OnChange:=@PictureChanged;
end;

destructor TPictureControl.Destroy;
begin
  FreeAndNil(FPicture);
  inherited Destroy;
end;

procedure TPictureControl.Paint;
begin
  if Picture.Graphic<>nil then
    // Canvas.Draw(0,0,Picture.Graphic); // copy is fast
    Canvas.StretchDraw(Rect(0,0,Width,Height),Picture.Graphic); // stretch is slow
  inherited Paint;
end;

initialization
  {$I playground.lrs}

end.

