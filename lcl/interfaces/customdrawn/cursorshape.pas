unit cursorshape;

{ Bindings for wp_cursor_shape_v1 (cursor-shape-v1 protocol, version 1).
  Lets the client request a server-provided cursor by name on pointer
  enter, instead of having to upload its own bitmap via wl_shm. The
  Wayland protocol REQUIRES the client to call wl_pointer.set_cursor
  on every pointer enter event; without it, the cursor shape is
  undefined and the compositor typically keeps whatever the previous
  client set, which manifests as "wrong cursor inside our window". }

{$mode delphi}{$H+}{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, ctypes, waylandwire, waylandcore;

const
  { wp_cursor_shape_device_v1.shape enum -- the subset we care about. }
  WP_CURSOR_SHAPE_DEFAULT       = 1;
  WP_CURSOR_SHAPE_TEXT          = 9;
  WP_CURSOR_SHAPE_POINTER       = 4;

type
  TWpCursorShapeManagerV1 = class;
  TWpCursorShapeDeviceV1  = class;

  TWpCursorShapeManagerV1 = class(TWaylandObject)
  public
    function GetPointer(Pointer_: TWaylandObject): TWpCursorShapeDeviceV1;
    procedure DestroyRequest;
  end;

  TWpCursorShapeDeviceV1 = class(TWaylandObject)
  public
    procedure SetShape(Serial: LongWord; Shape: LongWord);
    procedure DestroyRequest;
  end;

implementation

{ TWpCursorShapeManagerV1 }

function TWpCursorShapeManagerV1.GetPointer(Pointer_: TWaylandObject): TWpCursorShapeDeviceV1;
var
  W: TWlWriter;
  DevId: TWlObjectId;
begin
  DevId := FDisplay.NewId;
  Result := TWpCursorShapeDeviceV1.Create(FDisplay, DevId);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteNewId(DevId);
  W.WriteObject(Pointer_.Id);
  SendRequest(1, W);  { get_pointer }
end;

procedure TWpCursorShapeManagerV1.DestroyRequest;
begin
  SendRequest(0);
end;

{ TWpCursorShapeDeviceV1 }

procedure TWpCursorShapeDeviceV1.SetShape(Serial: LongWord; Shape: LongWord);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteUInt(Serial);
  W.WriteUInt(Shape);
  SendRequest(1, W);  { set_shape }
end;

procedure TWpCursorShapeDeviceV1.DestroyRequest;
begin
  SendRequest(0);
end;

end.
