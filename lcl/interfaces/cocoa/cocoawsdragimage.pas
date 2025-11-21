unit CocoaWSDragImage;

{$mode objfpc}{$H+}
{$ModeSwitch objectivec1}

interface

uses
  Classes, SysUtils,
  LCLType, Controls, Graphics, WSControls,
  MacOSAll, CocoaAll,
  CocoaGDIObjects, CocoaUtils;

type

  { TCocoaDragImage }

  TCocoaDragImage = objcclass(NSPanel, NSWindowDelegateProtocol)
  private
    _imageView: NSImageView;
    function windowShouldClose(sender : id): LongBool; message 'windowShouldClose:';
  public
    function acceptsFirstResponder: ObjCBOOL; override;
    function canBecomeKeyWindow: ObjCBOOL; override;
    procedure setImage(image: NSImage); message 'lclSetImage:';
  end;

  { TCocoaWSDragImageListResolution }

  TCocoaWSDragImageListResolution = class(TWSDragImageListResolution)
  private
    class var _dragImage: TCocoaDragImage;
    class var _dragHotSpot: TPoint;
    class var _dragImageLock: Boolean;
  private
    class function doDragMove(X, Y: Integer): Boolean;
    class function doSetVisible(NewVisible: Boolean): Boolean;
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

class function TCocoaWSDragImageListResolution.doDragMove(X, Y: Integer): Boolean;
var
  localPoint: NSPoint;
begin
  Result:= Assigned( _dragImage );
  if Result then begin
    Dec( X, _dragHotSpot.X );
    Dec( Y, _dragHotSpot.Y );
    localPoint:= ScreenPointFromLCLToNS( TPoint.Create(X,Y) );
    localPoint.y:= localPoint.y - _dragImage.frame.size.height;
    _dragImage.setFrameOrigin( localPoint );
    _dragImage.orderFront( nil );
  end;
end;

class function TCocoaWSDragImageListResolution.doSetVisible(NewVisible: Boolean): Boolean;
begin
  Result:= Assigned( _dragImage );
  if Result then begin
    if NewVisible then
      _dragImage.orderFrontRegardless
    else
      _dragImage.orderOut( nil );
  end;
end;

class function TCocoaWSDragImageListResolution.BeginDrag(
  const ADragImageList: TDragImageListResolution; Window: HWND; AIndex, X,
  Y: Integer): Boolean;

  function createDragImage(AImage: NSImage; AHotSpot: TPoint): Boolean;
  var
    nsr: NSRect;
  begin
    if _dragImage = nil then begin
      nsr:= NSMakeRect(0, 0, AImage.size.width, AImage.size.height);
      _dragImage:= TCocoaDragImage.alloc.initWithContentRect_styleMask_backing_defer(
                     nsr, 0, NSBackingStoreBuffered, False);
      _dragImage.setImage( AImage );
      _dragHotSpot:= AHotSpot;
      _dragImage.setAlphaValue( 0.8 );
      _dragImage.setIgnoresMouseEvents( True );
      _dragImage.setAcceptsMouseMovedEvents( False );
    end;
    Result:= _dragImage <> nil;
  end;

var
  lclBitmap: TBitmap;
  cocoaBitmap: TCocoaBitmap;
  cocoaImage: NSImage;
begin
  lclBitmap := TBitmap.Create;
  cocoaImage := nil;
  try
    ADragImageList.GetBitmap(AIndex, lclBitmap);
    if (lclBitmap.Handle = 0) or (lclBitmap.Width = 0) or (lclBitmap.Height = 0) then
    begin
      Result:= False;
      Exit;
    end;

    // Bitmap Handle should be nothing but TCocoaBitmap
    cocoaBitmap:= TCocoaBitmap(lclBitmap.Handle);
    cocoaImage:= cocoaBitmap.Image.copy;

    Result:= createDragImage( cocoaImage, ADragImageList.DragHotspot );
    if Result then
      self.doDragMove( X, Y );
  finally
    cocoaImage.release;
    lclBitmap.Free;
  end;
end;

class function TCocoaWSDragImageListResolution.DragMove(
  const ADragImageList: TDragImageListResolution; X, Y: Integer): Boolean;
begin
  Result:= self.doDragMove(X, Y);
end;

class procedure TCocoaWSDragImageListResolution.EndDrag(
  const ADragImageList: TDragImageListResolution);
begin
  if _dragImage <> nil then begin
    _dragImage.release;
    _dragImage:= nil;
  end;
end;

class function TCocoaWSDragImageListResolution.HideDragImage(
  const ADragImageList: TDragImageListResolution; ALockedWindow: HWND;
  DoUnLock: Boolean): Boolean;
begin
  Result:= True;
  if DoUnlock then
  begin
    _dragImageLock:= False;
    Result:= self.doSetVisible(False);
  end;
end;

class function TCocoaWSDragImageListResolution.ShowDragImage(
  const ADragImageList: TDragImageListResolution; ALockedWindow: HWND; X,
  Y: Integer; DoLock: Boolean): Boolean;
begin
  Result:= _dragImageLock;
  if not DoLock then
  begin
    if not Result then
      Result:= self.doSetVisible(True);
  end else
  begin
    _dragImageLock:= True;
    Result:= self.doDragMove(X, Y) and self.doSetVisible(True);
  end;
end;

{ TCocoaDragImage }

function TCocoaDragImage.windowShouldClose(sender: id): LongBool;
begin
  Result:= True;
end;

function TCocoaDragImage.acceptsFirstResponder: ObjCBOOL;
begin
  Result:= False;
end;

function TCocoaDragImage.canBecomeKeyWindow: ObjCBOOL;
begin
  Result:= False;
end;

procedure TCocoaDragImage.setImage(image: NSImage);
begin
  if _imageView = nil then begin
    _imageView:= NSImageView.new;
    self.setContentView( _imageView );
    _imageView.release;
  end;
  _imageView.setImage( image );
  _imageView.setFrame( NSMakeRect(0,0,image.size.width, image.size.height) );
  self.setContentSize( image.size );
end;

end.

