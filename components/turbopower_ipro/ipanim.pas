{******************************************************************}
{*  IPANIM.PAS - Provides basic animation support. You should not *}
{*  need to create an instance of this class, instead you should  *}
{*  inherit your animated graphics class from this class.         *}
{******************************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower Internet Professional
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 2000-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{ Global defines potentially affecting this unit }
{$I IPDEFINE.INC}

unit IpAnim;

interface

uses
  {$IFDEF IP_LAZARUS}
  LCLType,
  GraphType,
  LCLIntf,
  {$ELSE}
  Windows,
  Messages,
  {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, IpConst;

const

  // Constants for the default settings of some properties

  DefaultAggressiveDrawing : Boolean = False;
  DefaultFrameChangeNotify : Boolean = False;

type

  // Various disposal methods

  TDisposalMethod = (NODISPOSALMETHOD, DONOTDISPOSE, OVERWRITEWITHBKGCOLOR,
                     OVERWRITEWITHPREVIOUS);

  TIpAnimationFrameList = class;

  // Event Types

  TOnBeforeFrameChange = procedure (    Sender       : TObject;
                                        CurrentFrame : Integer;
                                    var NewFrame     : Integer;
                                        Frames       : TIpAnimationFrameList;
                                    var CanChange    : boolean) of object;

  TOnAfterFrameChange = procedure (Sender       : TObject;
                                   CurrentFrame : Integer;
                                   Frames       : TIpAnimationFrameList)
                                       of object;

  // Exception Classes

  EAnimationError = class(Exception);             // GIF Decoding errors
  EFrameListError = class (Exception);            // GIF Frame List errors

  // TIpAnimationFrame

  {
    TIpAnimationFrame holds one frame of an animation.  It also keeps track
    of the x and y offsets for the frame and the size of this frame.
  }

  TIpAnimationFrame = class(TObject)

    private
      FBitmap           : TBitmap;          // bitmap for this frame
      FXOffset          : Integer;          // X Offset for this frame
      FYOffset          : Integer;          // Y Offset for this frame
      FDelayTime        : Integer;          // Time in 1/100 second til next frame
      FDisposalMethod   : TDisposalMethod;  // Disposal Method
      FTransparent      : Boolean;
      FTransparentColor : TColor;

    protected

      procedure SetBitmap (v : TBitmap);
      procedure SetDelayTime (v : Integer);
      procedure SetXOffset (v : Integer);
      procedure SetYOffset (v : Integer);

    public

      constructor Create;
      destructor Destroy; override;

    published

      property Bitmap : TBitmap read FBitmap write SetBitmap;
      property DelayTime : Integer read FDelayTime write SetDelayTime;
      property DisposalMethod : TDisposalMethod
               read FDisposalMethod write FDisposalMethod default DONOTDISPOSE;
      property Transparent : Boolean
               read FTransparent write FTransparent;
      property TransparentColor : TColor
               read FTransparentColor write FTransparentColor;
      property XOffset : Integer read FXOffset write SetXOffset default 0;
      property YOffset : Integer read FYOffset write SetYOffset default 0;

  end;


  {
    TIpAnimationFrameList holds a list of Frames.
  }

   TIpAnimationFrameList = class(TObject)
    private

      FReferenceCounter : Integer;    // Internal reference counter
      FList : TList;

    protected

      function GetCount : Integer;
      procedure SetCount (v : Integer);
      function GetItem(Index : Integer): TIpAnimationFrame;
      procedure SetItem(Index : Integer; AFrame : TIpAnimationFrame);
      procedure RegisterFrames; virtual;
      procedure ReleaseFrames; virtual;

    public
      constructor Create;
      destructor Destroy; override;

      // public methods

      function Add(AFrame : TIpAnimationFrame): Integer;
      procedure Assign (Source : TIpAnimationFrameList); virtual;
      procedure Clear; 
      function IndexOf(AFrame : TIpAnimationFrame): Integer;
      procedure Insert(Index : Integer; AFrame : TIpAnimationFrame);
      function Remove(AFrame : TIpAnimationFrame): Integer;

      // properties

      property Count : Integer read GetCount write SetCount;
      property Items[Index: Integer]: TIpAnimationFrame
               read GetItem write SetItem; default;
      property List : TList read FList write FList;

  end;

  {
    TFreeGIFImage holds a decopressed GIF image.
  }

  TIpAnimatedGraphic = class(TGraphic)

    private

      FNumFrames               : Integer;    // Number of frames in an animation
      FAnimate                 : boolean;    // Animated image or not indicator
      FRealWidth               : Integer;    // Real width of the image
      FRealHeight              : Integer;    // Real height of the image

      FBitmap                  : TBitmap;    // bitmap of the current frame

      FDelayTime               : Integer;    // Delay in 100ths of a second
      FCurrentFrame            : Integer;    // Index of the current frame
      FDisposalMethod          : TDisposalMethod;    // Disposal method for the cur frm
      FAggressiveDrawing       : Boolean;    // steal a canvas and write to it?
      FFrameChangeNotify       : Boolean;    // trigger OnChange events?

      FTransparent             : Boolean;
      FTransparentColor        : TColor;
      FBackgroundColor         : TColor;

      FTimer                   : TTimer;     // Animation Timer
      FImages                  : TIpAnimationFrameList;

      FOnBeforeFrameChange  : TOnBeforeFrameChange;
      FOnAfterFrameChange   : TOnAfterFrameChange;

      FDrawingCanvas : TCanvas;     // Canvas that will be "stolen" for
                                    // animation purposes.
      FDrawingRect   : TRect;

      FDestinationCanvas : TCanvas; // User specified canvas to write to.
      FDestinationRect   : TRect;   // User specified rectangle to write to.

    protected

      // Property Access

      procedure ChangeFrame (NewFrame : Integer); virtual;
      procedure ClearFrame (    CurrentFrame   : TBitmap;
                                NewFrame       : TIpAnimationFrame;
                                DisposalMethod : TDisposalMethod;
                            var DefaultDrawing : Boolean); virtual;
      procedure Draw(      ACanvas : TCanvas;
                     const Rect    : TRect); override;
      procedure FreeAnimationFrames; virtual;
      function GetAnimate : boolean;
      function GetEmpty : Boolean; override;
      function GetHeight : Integer; override;
      function GetWidth : Integer; override;
      procedure Initialize; virtual;
      procedure SetAggressiveDrawing (v : Boolean);
      procedure SetAnimate (v : boolean);
      procedure SetBitmap (v : TBitmap);
      procedure SetDelayTime (v : Integer);
      procedure SetDestinationCanvas (v : TCanvas);
      procedure SetDestinationRect (v : TRect);
      procedure SetDisposalMethod (v : TDisposalMethod);
      procedure SetDrawingCanvas (v : TCanvas);
      procedure SetDrawingRect (v : TRect);
      procedure SetFrameChangeNotify (v : Boolean);
      procedure SetHeight (v : Integer); override;
      procedure SetImages (v : TIpAnimationFrameList);
      procedure SetNumFrames (v : Integer);
      procedure SetWidth (v : Integer); override;
      procedure TimerTimeoutHandler (Sender : TObject); virtual;

    public

      constructor Create; override;
      destructor Destroy; override;

      procedure Assign(Source : TPersistent); override;
      procedure AssignTo (Dest : TPersistent); override;

      procedure LoadFromStream (Stream: TStream); override;

      function StartAnimation : boolean; virtual;
      procedure StopAnimation; virtual;


      // Properties

      property AggressiveDrawing : Boolean
               read FAggressiveDrawing write SetAggressiveDrawing;
      property Animate : boolean read GetAnimate write SetAnimate;
      property BackgroundColor : TColor
               read FBackgroundColor write FBackgroundColor;
      property Bitmap : TBitmap read FBitmap write SetBitmap;
      property CurrentFrameIndex : Integer read FCurrentFrame write ChangeFrame;
      property DelayTime : Integer read FDelayTime write SetDelayTime;
      property DestinationCanvas : TCanvas
               read FDestinationCanvas write SetDestinationCanvas;
      property DestinationRect : TRect
               read FDestinationRect write SetDestinationRect;
      property DrawingCanvas : TCanvas
               read FDrawingCanvas write SetDrawingCanvas;
      property DrawingRect : TRect
               read FDrawingRect write SetDrawingRect;
      property DisposalMethod : TDisposalMethod
               read FDisposalMethod write SetDisposalMethod;
      property FrameChangeNotify : Boolean
               read FFrameChangeNotify  write SetFrameChangeNotify;
      property Height : Integer read getHeight write setHeight;
      property Images : TIpAnimationFrameList read FImages write SetImages;
      property NumFrames : Integer read FNumFrames write SetNumFrames;
      property Transparent : boolean read FTransparent write FTransparent;
      property TransparentColor : TColor
               read FTransparentColor write FTransparentColor;
      property Width : Integer read getWidth write setWidth;

      // Events

      property OnAfterFrameChange : TOnAfterFrameChange
               read FOnAfterFrameChange
               write FOnAfterFrameChange;
      property OnBeforeFrameChange : TOnBeforeFrameChange
               read FOnBeforeFrameChange
               write FOnBeforeFrameChange;
    published

  end;

implementation

// TFreeGIFFrame

constructor TIpAnimationFrame.Create;
begin
  inherited Create;

  FBitmap := TBitmap.create;

  XOffset := 0;
  YOffset := 0;
  DisposalMethod := NODISPOSALMETHOD;
end;

destructor TIpAnimationFrame.Destroy;
begin
  FBitmap.Free;
  FBitmap := nil;

  inherited Destroy;
end;

procedure TIpAnimationFrame.SetBitmap (v : TBitmap);
begin
  FBitmap.Assign (v);
end;

procedure TIpAnimationFrame.SetDelayTime (v : Integer);
begin
  if v <> FDelayTime then
    FDelayTime := v;
end;

procedure TIpAnimationFrame.SetXOffset (v : Integer);
begin
  if v <> FXOffset then
    FXOffset := v;
end;

procedure TIpAnimationFrame.SetYOffset (v : Integer);
begin
  if v <> FYOffset then
    FYOffset := v;
end;

// TIpAnimationFrameList

constructor TIpAnimationFrameList.Create;
begin
  inherited Create;

  FList := TList.Create;

  FReferenceCounter := 0;
end;

destructor TIpAnimationFrameList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TIpAnimationFrameList.Add(AFrame : TIpAnimationFrame) : Integer;
begin
  Result := FList.Add (AFrame)
end;

procedure TIpAnimationFrameList.Assign (Source : TIpAnimationFrameList);
var
  i : Integer;
begin
  Clear;
  FList.Capacity := Source.List.Capacity;
  FList.Count := Source.List.Count;
  for i := 0 to Source.List.Count - 1 do
    FList.Items[i] := Source.List.Items[i];

  RegisterFrames;
end;

procedure TIpAnimationFrameList.Clear;
begin
  FList.Clear;
end;

function TIpAnimationFrameList.GetCount : Integer;
begin
  result := FList.Count;
end;

function TIpAnimationFrameList.GetItem(Index : Integer) :
                                       TIpAnimationFrame;
begin

  if (Index >= FList.Count) or (Index < 0) then begin
    Result := nil;
    Exit;
  end;

  if (FList.Items[Index]=nil) then begin
    Result := nil;
    Exit;
  end;

  if TObject (FList.Items[Index]) is TIpAnimationFrame then
    Result := TObject (FList.Items[Index]) as TIpAnimationFrame
  else
    raise EFrameListError.CreateFmt(sBadFrameListObject,
                                    [TObject(FList.Items[Index]).ClassName]);
end;

function TIpAnimationFrameList.IndexOf(AFrame : TIpAnimationFrame) : Integer;
begin
  Result := FList.IndexOf (AFrame);
end;

procedure TIpAnimationFrameList.Insert(Index  : Integer;
                                       AFrame : TIpAnimationFrame);
begin
  FList.Insert (Index, AFrame);
end;

procedure TIpAnimationFrameList.RegisterFrames;
begin
  inc(FReferenceCounter);
end;

procedure TIpAnimationFrameList.ReleaseFrames;
var
  i : Integer;
begin
  dec (FReferenceCounter);
  if FReferenceCounter >= 0 then
    exit;

  for i := 0 to FList.Count - 1 do begin
    if (FList.Items[i]<>nil) then
      if TObject(FList.Items[i]) is TIpAnimationFrame then
        TIpAnimationFrame (FList.Items[i]).Free;
    FList.Items[i] := nil;
  end;

  Clear;
  FList.Count := 0;
end;

function TIpAnimationFrameList.Remove(AFrame : TIpAnimationFrame) : Integer;
begin
  Result := FList.Remove (AFrame);
end;

procedure TIpAnimationFrameList.SetCount (v : Integer);
begin
  if v <> FList.Count then
    FList.Count := v;
end;

procedure TIpAnimationFrameList.SetItem(Index  : Integer;
                                        AFrame : TIpAnimationFrame);
begin
  FList.Items[Index] := AFrame;
end;

// TIpAnimatedGraphic

constructor TIpAnimatedGraphic.Create;
begin
  inherited Create;

  FBitmap := TBitmap.create;

  FImages := TIpAnimationFrameList.create;

  FTimer := TTimer.create(nil);
  FTimer.Enabled := False;
  FTImer.OnTimer := TimerTimeoutHandler;

  FDrawingCanvas := nil;
  FDestinationCanvas := nil;

  FAggressiveDrawing := DefaultAggressiveDrawing;

  FFrameChangeNotify := DefaultFrameChangeNotify;

end;

destructor TIpAnimatedGraphic.Destroy;
begin
  FTimer.Free;
  FTimer := nil;

  FBitmap.Free;
  FBitmap := nil;

  FreeAnimationFrames;

  FImages.Free;
  FImages := nil;

  inherited Destroy;
end;

procedure TIpAnimatedGraphic.Assign (Source : TPersistent);
begin
  if Source is TIpAnimatedGraphic then
    with Source as TIpAnimatedGraphic do begin
      Self.Bitmap.assign(Bitmap);
      Self.Images.Assign(Images);
      Self.NumFrames         := NumFrames;
      Self.Animate           := Animate;
      Self.Width             := Width;
      Self.Height            := Height;
      Self.CurrentFrameIndex := 0;
      Self.DelayTime         := DelayTime;
      Self.DisposalMethod    := DisposalMethod;
      Self.AggressiveDrawing := AggressiveDrawing;
      Self.FrameChangeNotify := FrameChangeNotify;
      Self.DrawingCanvas     := DrawingCanvas;
    end
  else
    inherited Assign (Source);
end;

procedure TIpAnimatedGraphic.AssignTo (Dest : TPersistent);
begin
  if (Dest is TBitmap) then
    Dest.Assign(Bitmap)
  else
    inherited AssignTo(Dest);
end;

procedure TIpAnimatedGraphic.ChangeFrame (NewFrame : Integer);
var
  DefaultDrawing : Boolean;
begin
  if (NewFrame < 0) or (NewFrame >= NumFrames) then
    exit;

  FCurrentFrame := NewFrame;

  FBitmap.Width := FRealWidth;
  FBitmap.Height := FRealHeight;
  DisposalMethod := Images[NewFrame].DisposalMethod;

  DefaultDrawing := True;
  ClearFrame (FBitmap, Images[NewFrame],
              Images[NewFrame].DisposalMethod, DefaultDrawing);

  if DefaultDrawing then
    FBitmap.Canvas.CopyRect (Rect (Images[NewFrame].XOffset,
                                   Images[NewFrame].YOffset,
                                   Images[NewFrame].XOffset + Images[NewFrame].Bitmap.Width,
                                   Images[NewFrame].YOffset + Images[NewFrame].Bitmap.Height),
                             Images[NewFrame].Bitmap.Canvas,
                             Rect (0,
                                   0,
                                   Images[NewFrame].Bitmap.Width,
                                   Images[NewFrame].Bitmap.Height));

  if AggressiveDrawing then begin
    if assigned(FDrawingCanvas) then begin
      // Do a quick and dirty verification that the handle that we stole
      // from the .Draw method is still good.  We do this by calling
      // GetDeviceCaps with the handle and verifying that there is something
      // there.  If this is ok, we can then update the bitmap with the next
      // frame.
      if GetDeviceCaps (FDrawingCanvas.Handle, HORZSIZE) <> 0 then begin {!!.dg}
        if Animate and Transparent then                                  {!!.dg}
          FDrawingCanvas.CopyRect (FDrawingRect, FBitmap.Canvas,         {!!.dg}
                                   Rect (0, 0,                           {!!.dg}
                                         FBitmap.Width,                  {!!.dg}
                                         FBitmap.Height))                {!!.dg}
        else                                                             {!!.dg}
          Draw (FDrawingCanvas, FDrawingRect);                           {!!.dg}
      end;                                                               {!!.dg}
    end;
  end;

  if assigned(FDestinationCanvas) then begin                             {!!.dg}
    if Animate and Transparent then                                      {!!.dg}
      FDestinationCanvas.CopyRect (FDestinationRect, FBitmap.Canvas,     {!!.dg}
                                   Rect (0, 0,                           {!!.dg}
                                         FBitmap.Width,                  {!!.dg}
                                         FBitmap.Height))                {!!.dg}
    else                                                                 {!!.dg}
      Draw (FDestinationCanvas, DestinationRect);                        {!!.dg}
  end;                                                                   {!!.dg}

  // An alternate way to cause the animation to occur is to use the OnChange
  // event to notify the parent application that the image has changed.  The
  // problem with this is that the resulting flicker is... severe....

  if (assigned (OnChange)) and FrameChangeNotify then
    OnChange (self);

  if (assigned (FOnAfterFrameChange)) then
    FOnAfterFrameChange (Self, NewFrame, Images);

end;

procedure TIpAnimatedGraphic.ClearFrame (    CurrentFrame   : TBitmap;
                                             NewFrame       : TIpAnimationFrame;
                                             DisposalMethod : TDisposalMethod;
                                         var DefaultDrawing : Boolean);
var
  i                  : Integer;
  x, y               : Integer;
  UseTransparentCopy : Boolean;

begin
  {$IFDEF IP_LAZARUS}
  if (CurrentFrame=nil) then ;
  {$ENDIF}
  //  Basic clear frame.  This should work for just about anything.
  DefaultDrawing := False;
  UseTransparentCopy := False;

  case DisposalMethod of

    NODISPOSALMETHOD      :
      // do nothing in this case - leave the old image
      begin
        UseTransparentCopy := NewFrame.Transparent;
      end;

    DONOTDISPOSE          :
      // do nothing in this case - leave the old image
      begin
        UseTransparentCopy := NewFrame.Transparent;
      end;

    OVERWRITEWITHBKGCOLOR :
      begin
        // Fill with the background color
        Bitmap.Canvas.Brush.Color := BackgroundColor;
        Bitmap.Canvas.FillRect (Rect(NewFrame.XOffset,
                                     NewFrame.YOffset,
                                     NewFrame.XOffset + NewFrame.Bitmap.Width,
                                     NewFrame.YOffset + NewFrame.Bitmap.Height));
      end;
    OVERWRITEWITHPREVIOUS :
      // Try to find the last do not dispose frame and fill the canvas with
      // that.
      begin
        i := CurrentFrameIndex;
        while (i >= 0) and (FNumFrames > 1) and
              (Images[i].DisposalMethod <> DONOTDISPOSE) do
          dec (i);
        if (i >= 0) and (i < FNumFrames) and (FNumFrames > 1) then begin
          if Images[i].DisposalMethod = DONOTDISPOSE then
            Bitmap.Canvas.CopyRect (Rect (Images[i].XOffset,
                                        Images[i].YOffset,
                                        Images[i].XOffset + Images[i].Bitmap.Width,
                                        Images[i].YOffset + Images[i].Bitmap.Height),
                                  Images[i].Bitmap.Canvas,
                                  Rect (0,
                                        0,
                                        Images[i].Bitmap.Width,
                                        Images[i].Bitmap.Height));
            UseTransparentCopy := NewFrame.Transparent;
          end
        else
          UseTransparentCopy := NewFrame.Transparent;
      end;

  end;

  // This is not a generally recommended way of handling transparency.
  // However, it gets the timing more accurate.

  if UseTransparentCopy then begin
    if Images[FCurrentFrame].Bitmap <> nil then                        {!!.12}
      for x := 0 to Images[FCurrentFrame].Bitmap.Width - 1 do
        for y := 0 to Images[FCurrentFrame].Bitmap.Height - 1 do
          if Images[FCurrentFrame].Bitmap.Canvas.Pixels[x, y] <>
             Images[FCurrentFrame].TransparentColor then begin
            if (x + Images[FCurrentFrame].XOffset < Bitmap.Width) and
               (y + Images[FCurrentFrame].YOffset < Bitmap.Height) then
              Bitmap.Canvas.Pixels[x + Images[FCurrentFrame].XOffset,
                                   y + Images[FCurrentFrame].YOffset] :=
                      Images[FCurrentFrame].Bitmap.Canvas.Pixels[x, y];
          end;
  end else
    Bitmap.Canvas.CopyRect (Rect (NewFrame.XOffset,
                                  NewFrame.YOffset,
                                  NewFrame.XOffset + NewFrame.Bitmap.Width,
                                  NewFrame.YOffset + NewFrame.Bitmap.Height),
                            NewFrame.Bitmap.Canvas,
                            Rect (0,
                                  0,
                                  NewFrame.Bitmap.Width,
                                  NewFrame.Bitmap.Height));

end;

procedure TIpAnimatedGraphic.Draw(      ACanvas : TCanvas;
                                  const Rect    : TRect);
begin

  // Since a TGraphic has no visible portion (for trapping paints) and no
  // knowledge of what component owns it, the animation can be tricky.  This
  // is resolved by "stealing" the canvas from wherever this graphic is going
  // to be drawn and then using that canvas for the animation updates.

  if AggressiveDrawing then begin
    DrawingCanvas := ACanvas;
    DrawingRect := Rect;
  end;

end;

procedure TIpAnimatedGraphic.FreeAnimationFrames;
begin
  Images.ReleaseFrames;
end;

function TIpAnimatedGraphic.GetAnimate : boolean;
begin
  result := FAnimate;
end;

function TIpAnimatedGraphic.GetEmpty : Boolean;
begin
  result := (Height = 0) or (Width = 0);
end;

function TIpAnimatedGraphic.GetHeight : Integer;
begin
  Result := FRealHeight;
end;

function TIpAnimatedGraphic.GetWidth : Integer;
begin
  result := FRealWidth;
end;

procedure TIpAnimatedGraphic.Initialize;
begin
  // Reset the frame count

  NumFrames := 0;
  FCurrentFrame := 0;
  Animate := False;

  // Free up any stray bitmaps

  FreeAnimationFrames;

end;

procedure TIpAnimatedGraphic.LoadFromStream (Stream: TStream);
{
  LoadFromStream should never be called at this level.  The classes that
  inherit from this class should implement their own load code.

  Should LoadFromStream get called, TIpAnimatedGraphic will create a default
  image.
}
begin
  {$IFDEF IP_LAZARUS}
  if (Stream=nil) then ;
  {$ENDIF}
  Width := 50;
  Height := 50;
  Bitmap.Canvas.Brush.Color := clWhite;
  Bitmap.Canvas.FillRect (Rect (0, 0, 49, 49));
  Bitmap.Canvas.Pen.Color := clRed;
  Bitmap.Canvas.TextOut (0, 0, 'X');
end;

procedure TIpAnimatedGraphic.SetAggressiveDrawing (v : Boolean);
begin
  if v <> FAggressiveDrawing then
    FAggressiveDrawing := v;
end;

procedure TIpAnimatedGraphic.SetAnimate (v : boolean);
begin
  if v <> FAnimate then begin
    if v then
      FAnimate := StartAnimation
    else begin
      FAnimate := False;
      StopAnimation;
    end;
  end;
end;

procedure TIpAnimatedGraphic.SetBitmap (v : TBitmap);
begin
  FBitmap.Assign (v);
end;

procedure TIpAnimatedGraphic.SetDelayTime (v : Integer);
begin
  if v <> FDelayTime then
    FDelayTime := v;
end;

procedure TIpAnimatedGraphic.SetDestinationCanvas (v : TCanvas);
begin

  // Destination Canvas is a weird property. It IS the canvas to which the
  // drawing will take place.  We use := to assign it.

  if v <> FDestinationCanvas then
    FDestinationCanvas := v;
end;

procedure TIpAnimatedGraphic.SetDestinationRect (v : TRect);
begin
  FDestinationRect := v;
end;

procedure TIpAnimatedGraphic.SetDisposalMethod (v : TDisposalMethod);
begin
  if v <> FDisposalMethod then
    FDisposalMethod := v;
end;

procedure TIpAnimatedGraphic.SetDrawingCanvas (v : TCanvas);
begin

  // Drawing Canvas is a weird property. It IS the canvas to which the drawing
  // will take place.  We use := to assign it.

  if v <> FDrawingCanvas then
    FDrawingCanvas := v;
end;

procedure TIpAnimatedGraphic.SetDrawingRect (v : TRect);
begin

  // Drawing Rect is a weird property. It IS the rectangle on the canvas to
  // hich the drawing will take place.  We use := to assign it.

  FDrawingRect := v;
end;

procedure TIpAnimatedGraphic.SetFrameChangeNotify (v : Boolean);
begin
  if v <> FFrameChangeNotify then
    FFrameChangeNotify  := v;
end;

procedure TIpAnimatedGraphic.SetHeight(v : Integer);
begin
  if v <> FRealHeight then begin
    FBitmap.height := v;
    FRealHeight := v;
  end;
end;

procedure TIpAnimatedGraphic.SetImages (v : TIpAnimationFrameList);
var
  i : integer;
begin
  FImages.List.Clear;
  FImages.List.Capacity := v.List.Capacity;
  FImages.List.Count := v.List.Count;
  for i := 0 to v.List.Count - 1 do
    FImages.List.Add(v.List[i]);
end;

procedure TIpAnimatedGraphic.SetNumFrames (v : Integer);
begin
  if v <> FNumFrames then
    FNumFrames := v;
end;

procedure TIpAnimatedGraphic.SetWidth(v : Integer);
begin
  if v <> FRealWidth then begin
    FBitmap.Width := v;
    FRealWidth := v;
  end;
end;

function TIpAnimatedGraphic.StartAnimation : boolean;
begin
  Result := True;
  FTimer.Enabled := False;
  if NumFrames > 1 then begin
    if FDelayTime <= 1 then
      FTimer.Interval := 100
    else
      FTimer.Interval := Images[0].DelayTime * 10;
    FTimer.Enabled := True;
  end else
    Result := False;
end;

procedure TIpAnimatedGraphic.StopAnimation;
begin
  FTimer.Enabled := False;
end;

procedure TIpAnimatedGraphic.TimerTimeoutHandler (Sender : TObject);
var
  NewFrame  : Integer;
  CanChange : Boolean;

begin
  if NumFrames < 1 then
    exit;

  FTimer.Enabled := False;
  try
    NewFrame := FCurrentFrame;

    Inc (NewFrame);
    if (NewFrame >= NumFrames) then
      NewFrame := 0;

    CanChange := True;
    if Assigned (FOnBeforeFrameChange) then
      FOnBeforeFrameChange (self,
                            FCurrentFrame,
                            NewFrame,
                            Images,
                            CanChange);

    if CanChange then
      ChangeFrame (NewFrame);

    if NumFrames >= 1 then begin
      FTimer.Interval := Images[FCurrentFrame].DelayTime * 10;
      if FTImer.Interval < 10 then
        FTImer.Interval := 50;
    end else
      FTimer.Interval := 32767;

  finally
    FTimer.Enabled := True;
  end;
end;

end.
