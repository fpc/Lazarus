unit CocoaDragImg;

{$mode objfpc}{$H+}
{$ModeSwitch objectivec1}

interface

uses
  MacOSAll, CocoaAll,
  CocoaWindows,
  Classes, SysUtils;

type

{ TCocoaDragImage }

TCocoaDragImage = objcclass(NSPanel, NSWindowDelegateProtocol)
private
   imageView: NSImageView;
protected
  function windowShouldClose(sender : id): LongBool; message 'windowShouldClose:';
public
  function initWithContentRect_styleMask_backing_defer(contentRect: NSRect;
    aStyle: NSUInteger; bufferingType: NSBackingStoreType; flag: ObjCBOOL): id;
    override;
  function acceptsFirstResponder: ObjCBOOL; override;
  function canBecomeKeyWindow: ObjCBOOL; override;
  procedure setImage(aImage: NSImage); message 'setImage:';
end;


implementation

{ TCocoaDragImage }

function TCocoaDragImage.windowShouldClose(sender: id): LongBool;
begin
  Result := True;
end;

function TCocoaDragImage.initWithContentRect_styleMask_backing_defer(
  contentRect: NSRect; aStyle: NSUInteger; bufferingType: NSBackingStoreType;
  flag: ObjCBOOL): id;
begin
  Result := inherited initWithContentRect_styleMask_backing_defer(contentRect, aStyle, bufferingType, flag);

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
  imageView := NSImageView.alloc.initWithFrame(NSMakeRect(0,0,aImage.size.width, aImage.size.height));
  imageView.setImage(aImage);
  setContentView(imageView);
  setContentSize(aImage.size);
end;

end.

