unit CocoaWSDragImage;

{$mode objfpc}{$H+}
{$ModeSwitch objectivec1}

interface

uses
  Classes, SysUtils,
  LCLType, Controls, Graphics, WSControls,
  MacOSAll, CocoaAll,
  CocoaWindows, CocoaGDIObjects;

type

  { TCocoaDragImage }

  TCocoaDragImage = objcclass(NSPanel, NSWindowDelegateProtocol)
  private
    _imageView: NSImageView;
    function windowShouldClose(sender : id): LongBool; message 'windowShouldClose:';
  public
    function acceptsFirstResponder: ObjCBOOL; override;
    function canBecomeKeyWindow: ObjCBOOL; override;
    procedure setImage(aImage: NSImage); message 'lclSetImage:';
  end;

  { TCocoaWSDragImageListResolution }

  TCocoaWSDragImageListResolution = class(TWSDragImageListResolution)
  private
    class var _dragImageList: TCocoaDragImage;
    class var _dragHotSpot: TPoint;
    class var _dragImageLock: Boolean;
  private
    class function DragImageList_BeginDrag(AImage: NSImage; AHotSpot: TPoint): Boolean;
    class procedure DragImageList_EndDrag;
    class function DragImageList_DragMove(X, Y: Integer): Boolean;
    class function DragImageList_SetVisible(NewVisible: Boolean): Boolean;
    class property DragImageLock: Boolean read _dragImageLock write _dragImageLock;
  published
    class function BeginDrag(const ADragImageList: TDragImageListResolution; Window: HWND; AIndex, X, Y: Integer): Boolean; override;
    class function DragMove(const ADragImageList: TDragImageListResolution; X, Y: Integer): Boolean; override;
    class procedure EndDrag(const ADragImageList: TDragImageListResolution); override;
    class function HideDragImage(const ADragImageList: TDragImageListResolution;
      ALockedWindow: HWND; DoUnLock: Boolean): Boolean; override;
    class function ShowDragImage(const ADragImageList: TDragImageListResolution;
      ALockedWindow: HWND; X, Y: Integer; DoLock: Boolean): Boolean; override;
  end;

implementation

class function TCocoaWSDragImageListResolution.DragImageList_BeginDrag(AImage: NSImage;
  AHotSpot: TPoint): Boolean;
var
  nsr: NSRect;
begin
  if _dragImageList = nil then
  begin
    nsr := NSMakeRect(0, 0, AImage.size.width, AImage.size.height);
    _dragImageList := TCocoaDragImage.alloc.initWithContentRect_styleMask_backing_defer(
                   nsr, 0, NSBackingStoreBuffered, False);
    _dragImageList.setImage( AImage );
    _dragHotSpot := AHotSpot;
    _dragImageList.setAlphaValue(0.8);
    _dragImageList.setIgnoresMouseEvents(True);
    _dragImageList.setAcceptsMouseMovedEvents(False);
  end;
  Result := _dragImageList <> nil;
end;

class procedure TCocoaWSDragImageListResolution.DragImageList_EndDrag;
begin
  if _dragImageList <> nil then
  begin
    _dragImageList.release;
    _dragImageList := nil;
  end;
end;

class function TCocoaWSDragImageListResolution.DragImageList_DragMove(X, Y: Integer): Boolean;
var
  f: NSRect;
begin
  Result := Assigned(_dragImageList);
  if Result then
  begin
    _dragImageList.orderFront(nil);
    Dec(X, _dragHotSpot.X);
    Dec(Y, _dragHotSpot.Y);
    if Assigned(_dragImageList.screen) then
    begin
      f := _dragImageList.frame;
      //dec(X, Round(f.origin.x));
      Y := Round(_dragImageList.screen.frame.size.height - f.size.height) - Y;
      _dragImageList.setFrameOrigin(NSMakePoint(X, Y));
    end
    else
    begin
      {dummy}
    end;
  end;
end;

class function TCocoaWSDragImageListResolution.DragImageList_SetVisible(NewVisible: Boolean): Boolean;
begin
  Result := Assigned(_dragImageList);
  begin
    if NewVisible then
       _dragImageList.orderFrontRegardless
    else
      _dragImageList.orderOut(nil);
  end;
end;

class function TCocoaWSDragImageListResolution.BeginDrag(
  const ADragImageList: TDragImageListResolution; Window: HWND; AIndex, X,
  Y: Integer): Boolean;
var
  ABitmap: TBitmap;
  cb: TCocoaBitmap;
  img: NSImage;
begin
  ABitmap := TBitmap.Create;
  img := nil;
  try
    ADragImageList.GetBitmap(AIndex, ABitmap);
    if (ABitmap.Handle = 0) or (ABitmap.Width = 0) or (ABitmap.Height = 0) then
    begin
      Result := False;
      Exit;
    end;

    // Bitmap Handle should be nothing but TCocoaBitmap
    cb := TCocoaBitmap(ABitmap.Handle);
    img := cb.Image.copy;

    Result := self.DragImageList_BeginDrag(
      img, ADragImageList.DragHotspot);
    if Result then
      self.DragImageList_DragMove(X, Y);
  finally
    img.release;
    ABitmap.Free;
  end;
end;

class function TCocoaWSDragImageListResolution.DragMove(
  const ADragImageList: TDragImageListResolution; X, Y: Integer): Boolean;
begin
  Result := self.DragImageList_DragMove(X, Y);
  if not Result then
  begin
    writeln('noresult');
  end;
end;

class procedure TCocoaWSDragImageListResolution.EndDrag(
  const ADragImageList: TDragImageListResolution);
begin
  self.DragImageList_EndDrag;
end;

class function TCocoaWSDragImageListResolution.HideDragImage(
  const ADragImageList: TDragImageListResolution; ALockedWindow: HWND;
  DoUnLock: Boolean): Boolean;
begin
  Result := True;
  if DoUnlock then
  begin
    self.DragImageLock := False;
    Result := self.DragImageList_SetVisible(False);
  end;
end;

class function TCocoaWSDragImageListResolution.ShowDragImage(
  const ADragImageList: TDragImageListResolution; ALockedWindow: HWND; X,
  Y: Integer; DoLock: Boolean): Boolean;
begin
  Result := self.DragImageLock;
  if not DoLock then
  begin
    if not Result then
      Result := self.DragImageList_SetVisible(True);
  end else
  begin
    self.DragImageLock := True;
    Result := self.DragImageList_DragMove(X, Y) and
      self.DragImageList_SetVisible(True);
  end;
end;

{ TCocoaDragImage }

function TCocoaDragImage.windowShouldClose(sender: id): LongBool;
begin
  Result := True;
end;

function TCocoaDragImage.acceptsFirstResponder: ObjCBOOL;
begin
  Result:=False
end;

function TCocoaDragImage.canBecomeKeyWindow: ObjCBOOL;
begin
  Result:=False
end;

procedure TCocoaDragImage.setImage(aImage: NSImage);
begin
  _imageView := NSImageView.alloc.initWithFrame(NSMakeRect(0,0,aImage.size.width, aImage.size.height));
  _imageView.setImage(aImage);
  setContentView(_imageView);
  setContentSize(aImage.size);
end;

end.

