unit PlayGround;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  LMessages, LCLType, LCLIntf, ExtCtrls, StdCtrls;

type

  { TPictureControl }

  TPictureControl = class(TCustomControl)
    procedure PictureChanged(Sender: TObject);
  private
    FPicture: TPicture;
    procedure SetPicture(const AValue: TPicture);
    procedure WMEraseBkgnd(var Msg: TLMessage); message LM_ERASEBKGND;
  protected
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  published
    property Picture: TPicture read FPicture write SetPicture;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnKeyDown;
    property OnKeyUp;
    property OnKeyPress;
  end;

  { TPlayGroundForm }

  TPlayGroundForm = class(TForm)
    ComboBox1: TComboBox;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Timer1: TTimer;
    procedure ComboBox1Change(Sender: TObject);
    procedure PlayGroundFormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure PlayGroundFormCreate(Sender: TObject);
    procedure PlayGroundFormDestroy(Sender: TObject);
    procedure PictureControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
  private
    FXIntertia: Double;
    FYIntertia: Double;
    FSpritePos: TPoint;
    FSpritePosChange: TPoint;
    FSpritePosInit: Boolean;
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

{$R playground.lfm}

uses
  Math;

{ TPlayGroundForm }

procedure TPlayGroundForm.PlayGroundFormCreate(Sender: TObject);
begin
  PictureControl:=TPictureControl.Create(Self);
  with PictureControl do begin
    Parent:=Panel1; //Self;
    Align:=alClient;
    OnKeyDown := @PictureControlKeyDown;
  end;

  SpriteImg:=TPortableNetworkGraphic.Create;
  BackgroundImg:=TPortableNetworkGraphic.Create;
  BufferImg:=TBitmap.Create;

  SpriteImg.LoadFromFile(SetDirSeparators('../../images/ide_icon48x48.png'));
  BackgroundImg.LoadFromFile(SetDirSeparators('../../images/splash_logo.png'));
  BufferImg.Width:=BackgroundImg.Width;
  BufferImg.Height:=BackgroundImg.Height;

  FXIntertia := 0;
  FYIntertia := 0;

  Timer1.Enabled := Combobox1.ItemIndex = 0;
  
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

procedure TPlayGroundForm.ComboBox1Change(Sender: TObject);
begin
  Timer1.Enabled := Combobox1.ItemIndex in [0, 1, 3, 4];
  PictureControl.SetFocus;
end;

procedure TPlayGroundForm.PictureControlKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
const
  DELTA = 20;
begin
  if ComboBox1.ItemIndex = 3 then Exit; // for smooth arrow key input
  case Key of
    VK_LEFT  : FSpritePosChange := Point(-DELTA, 0);
    VK_RIGHT : FSpritePosChange := Point( DELTA, 0);
    VK_UP    : FSpritePosChange := Point(0, -DELTA);
    VK_DOWN  : FSpritePosChange := Point(0,  DELTA);
  end;
  UpdateImage;
end;

function AdjustStep(p1, p2: Integer; Divisor: Integer): Integer;
var
  f: Double;
  delta: Integer;
begin
  if p1 = p2 then
    exit(0);

  delta := p2 - p1;
  f := delta/Divisor;
  if abs(f) < 1 then
    Result := sign(f)
  else
    Result := round(f);
  {
    Result
  if Delta > 0 then begin
    if Delta < Divisor then
      Result := 1
    else
      Result := Delta div Divisor;
  end else
  if Delta < 0 then begin
    if -Delta < Divisor then
      Result := -1
    else
      Result := Delta div Divisor;
  end else
    Result := 0;
    }
end;

procedure TPlayGroundForm.UpdateImage;
const
  SECONDS_PER_DAY = 24*60*60;
var
  DestImg: TBitmap;
  t: Double;
  CenterX: Integer;
  CenterY: Integer;
  dx, dy: Integer;
  MousePos: TPoint;
begin
  // paint first on the buffer
  
  // paint background
  BufferImg.Canvas.CopyRect(Rect(0,0,BufferImg.Width,BufferImg.Height),
       BackgroundImg.Canvas,Rect(0,0,BackgroundImg.Width,BackgroundImg.Height));
  // paint sprite
  CenterX:=BufferImg.Width div 2;
  CenterY:=BufferImg.Height div 2;
  if not FSpritePosInit then begin
    // SpritePos refers to the top/left corner of the sprite image.
    FSpritePos := Point(CenterX - SpriteImg.Width div 2, CenterY - SpriteImg.Height div 2);
    FSpritePosInit := true;
  end;
  case Combobox1.ItemIndex of
    0: begin
         // Movement of sprite by code along a calculated curve
         t := Now * SECONDS_PER_DAY;
         FSpritePos.X := CenterX + round(cos(t)*CenterX*2/3) - SpriteImg.Width div 2;
         FSpritePos.Y := CenterY + round(sin(t*0.7)*CenterY*2/3) - SpriteImg.Height div 2;
       end;
    1: begin
         // Movement of sprite by mouse: the sprite follows the mouse
         // Convert screen coordinates to images coordinates
         MousePos := PictureControl.ScreenToClient(Mouse.CursorPos);
         MousePos.X := round(MousePos.X / PictureControl.Width * PictureControl.Picture.Width);
         MousePos.Y := round(MousePos.Y / PictureControl.Height * PictureControl.Picture.Height);
         dx := AdjustStep(FSpritePos.X, MousePos.X, 5);
         dy := AdjustStep(FSpritePos.Y, MousePos.Y, 5);
         FSpritePos.X := FSpritePos.X + dx;
         FSpritePos.Y := FSpritePos.Y + dy;
       end;
    2: begin
         // Movement of sprite by keyboard: UP/DOWN/LEFT/RIGHT arrows advance
         // the sprite position by a given amount
         FSpritePos.X := FSpritePos.X + FSpritePosChange.X;
         FSpritePos.Y := FSpritePos.Y + FSpritePosChange.Y;
       end;
    3: begin
         // Movement of sprite by keyboard: UP/DOWN/LEFT/RIGHT arrows advance smooth version
         if (GetKeyState(VK_LEFT) < 0) then FSpritePos.X := FSpritePos.X - 10;
         if (GetKeyState(VK_RIGHT) < 0) then FSpritePos.X := FSpritePos.X + 10;
         if (GetKeyState(VK_UP) < 0) then FSpritePos.Y := FSpritePos.Y - 10;
         if (GetKeyState(VK_DOWN) < 0) then FSpritePos.Y := FSpritePos.Y + 10;
       end;
    4: begin
         // Movement of sprite by keyboard: UP/DOWN/LEFT/RIGHT arrows advance with inertia
         if (GetKeyState(VK_LEFT) < 0) then FXIntertia := FXIntertia - 0.5;
         if (GetKeyState(VK_RIGHT) < 0) then FXIntertia := FXIntertia + 0.5;
         if (GetKeyState(VK_UP) < 0) then FYIntertia := FYIntertia - 0.5;
         if (GetKeyState(VK_DOWN) < 0) then FYIntertia := FYIntertia + 0.5;
         if (FXIntertia > 6) then FXIntertia := 6;
         if (FXIntertia < -6) then FXIntertia := -6;
         if (FYIntertia > 6) then FYIntertia := 6;
         if (FYIntertia < -6) then FYIntertia := -6;
         if (FXIntertia > 0) then FXIntertia := FXIntertia - 0.2;
         if (FXIntertia < 0) then FXIntertia := FXIntertia + 0.2;
         if (FYIntertia > 0) then FYIntertia := FYIntertia - 0.2;
         if (FYIntertia < 0) then FYIntertia := FYIntertia + 0.2;
         FSpritePos.X := FSpritePos.X + round(FXIntertia);
         FSpritePos.Y := FSpritePos.Y + round(FYIntertia);
       end;
  end;

  // Make sure that the sprite does not leave the image.
  FSpritePos.X := EnsureRange(FSpritePos.X, 0, BufferImg.Width - SpriteImg.Width);
  FSpritePos.Y := EnsureRange(FSpritePos.Y, 0, BufferImg.Height - SpriteImg.Height);

  // Draw sprite at current position to buffer.
  BufferImg.Canvas.Draw(FSpritePos.X, FSpritePos.Y, SpriteImg);

  // copy to image
  DestImg:=PictureControl.Picture.Bitmap;
  DestImg.Width:=BufferImg.Width;
  DestImg.Height:=BufferImg.Height;
  DestImg.Canvas.Draw(0,0,BufferImg);
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

end.

