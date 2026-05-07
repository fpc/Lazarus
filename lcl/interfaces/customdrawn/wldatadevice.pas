unit wldatadevice;

{ Bindings for wl_data_device_manager / wl_data_device / wl_data_source /
  wl_data_offer (stable, version 3). This is Wayland's clipboard
  protocol -- "selection" = system clipboard. Drag-and-drop uses the
  same objects but we only wire the selection path here; DnD events are
  parsed enough to not desync the wire stream, but no callbacks fire.

  The protocol passes the actual bytes out-of-band over a pipe fd
  (SCM_RIGHTS). waylandwire already plumbs that via QueueRequest's
  Fds argument; no extra glue needed.

  Mime-type negotiation is the client's responsibility -- the LCL hands
  us mime strings via ClipboardFormatToMimeType, we offer them
  verbatim, the receiver picks one and asks for it via "receive". }

{$mode delphi}{$H+}{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, ctypes, waylandwire, waylandcore;

type
  TWlDataDeviceManager = class;
  TWlDataDevice        = class;
  TWlDataSource        = class;
  TWlDataOffer         = class;

  { wl_data_source events. OnSend is the one that matters: a receiver
    has asked for the data, here's the mime type it picked and the fd
    we should write to (and then close). OnCancelled means our offer
    was replaced by another set_selection (or rejected) -- destroy
    the source and stop tracking it. OnTarget is DnD-only in practice. }
  TDataSourceTargetProc    = procedure(Source: TWlDataSource;
                                       const MimeType: AnsiString) of object;
  TDataSourceSendProc      = procedure(Source: TWlDataSource;
                                       const MimeType: AnsiString;
                                       Fd: cint) of object;
  TDataSourceCancelledProc = procedure(Source: TWlDataSource) of object;

  { wl_data_device events. OnSelection fires when the system clipboard
    contents change; the offer is nil if the clipboard was cleared.
    The offer is owned by us once received -- destroy it when done. }
  TDataDeviceOfferProc     = procedure(Device: TWlDataDevice;
                                       Offer: TWlDataOffer) of object;
  TDataDeviceSelectionProc = procedure(Device: TWlDataDevice;
                                       Offer: TWlDataOffer) of object;

  { wl_data_offer events. OnOffer is called once per advertised mime
    type, before OnSelection on the device delivers the offer. The
    standard pattern is to accumulate mime types in the offer's
    MimeTypes list (we do that automatically) and let user code consult
    it after OnSelection. }
  TDataOfferOfferProc      = procedure(Offer: TWlDataOffer;
                                       const MimeType: AnsiString) of object;

  TWlDataDeviceManager = class(TWaylandObject)
  public
    { request 0: create_data_source -- a thing we'll fill with bytes. }
    function CreateDataSource: TWlDataSource;
    { request 1: get_data_device -- a thing that delivers selections
      from the seat. Bind once per seat and keep it alive. }
    function GetDataDevice(Seat: TWaylandSeat): TWlDataDevice;
    { request: destroy. wl_data_device_manager was originally not
      destructible; "destroy" was added later (and the version we
      target supports it). Optional. }
    procedure DestroyRequest;
  end;

  TWlDataSource = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    OnTarget    : TDataSourceTargetProc;
    OnSend      : TDataSourceSendProc;
    OnCancelled : TDataSourceCancelledProc;
    { request 0: offer(mime_type) -- advertise that we can produce
      bytes for this mime type. Call once per supported type before
      handing the source to set_selection. }
    procedure Offer(const MimeType: AnsiString);
    { request 1: destroy. Call after OnCancelled, or when our app no
      longer wants to be the clipboard provider. }
    procedure DestroyRequest;
  end;

  TWlDataDevice = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    OnDataOffer : TDataDeviceOfferProc;
    OnSelection : TDataDeviceSelectionProc;
    { Most recent selection offer the server delivered. nil if the
      clipboard is empty / was cleared. Owned by us once non-nil --
      destroy and replace when a new selection event arrives. }
    CurrentSelection: TWlDataOffer;
    { request 1: set_selection(source, serial). Source = nil clears
      the clipboard. Serial must be from a recent input event we
      received from this seat (wl_keyboard.enter or similar). }
    procedure SetSelection(Source: TWlDataSource; Serial: LongWord);
    { request 2: release. Tells the server we're done with the device. }
    procedure Release;
  end;

  TWlDataOffer = class(TWaylandObject)
  protected
    procedure Dispatch(Opcode: Word; var Reader: TWlReader); override;
  public
    { Filled in as the server emits "offer" events. Owned by this
      record; freed by Destroy. }
    MimeTypes: TStringList;
    OnOffer  : TDataOfferOfferProc;
    constructor Create(ADisplay: TWaylandDisplay; AId: TWlObjectId); override;
    destructor Destroy; override;
    { request 1: receive(mime_type, fd). The server will write the
      offered bytes to Fd and then close its end. Fd MUST be the
      write-end of a pipe the caller created; the caller reads bytes
      from the read-end until EOF. }
    procedure Receive(const MimeType: AnsiString; Fd: cint);
    { request 2: destroy. Call when done with the offer. }
    procedure DestroyRequest;
  end;

implementation

{ TWlDataDeviceManager }

function TWlDataDeviceManager.CreateDataSource: TWlDataSource;
var
  W: TWlWriter;
  NewIdValue: TWlObjectId;
begin
  NewIdValue := FDisplay.NewId;
  Result := TWlDataSource.Create(FDisplay, NewIdValue);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteNewId(NewIdValue);
  SendRequest(0, W);  { create_data_source }
end;

function TWlDataDeviceManager.GetDataDevice(Seat: TWaylandSeat): TWlDataDevice;
var
  W: TWlWriter;
  NewIdValue: TWlObjectId;
begin
  NewIdValue := FDisplay.NewId;
  Result := TWlDataDevice.Create(FDisplay, NewIdValue);
  FDisplay.Register(Result);
  W.Reset;
  W.WriteNewId(NewIdValue);
  W.WriteObject(Seat.Id);
  SendRequest(1, W);  { get_data_device }
end;

procedure TWlDataDeviceManager.DestroyRequest;
begin
  SendRequest(2);  { destroy (added in v3) }
end;

{ TWlDataSource }

procedure TWlDataSource.Dispatch(Opcode: Word; var Reader: TWlReader);
var
  Mime: AnsiString;
  Fd: cint;
begin
  case Opcode of
    0:  { target(string|null mime_type) -- DnD; called as the pointer
          moves over potential drop sites. For pure clipboard use this
          shouldn't fire. We expose it anyway for completeness. }
      begin
        Mime := Reader.ReadString;
        if Assigned(OnTarget) then OnTarget(Self, Mime);
      end;
    1:  { send(string mime_type, fd) -- the meat of the protocol. The
          receiver picked Mime as its preferred mime type and is
          waiting for bytes on Fd. Caller writes its data to Fd and
          closes it. The fd arrives via SCM_RIGHTS in the same
          recvmsg that delivered this event; waylandwire has already
          stuffed it in the in-fd queue, so we pop one. }
      begin
        Mime := Reader.ReadString;
        Fd := FDisplay.Connection.PopFd;
        if Assigned(OnSend) then OnSend(Self, Mime, Fd);
      end;
    2:  { cancelled -- another source has replaced us, or the receiver
          rejected the offer. Caller should destroy the source. }
      begin
        if Assigned(OnCancelled) then OnCancelled(Self);
      end;
    { 3=dnd_drop_performed, 4=dnd_finished, 5=action: DnD-only.
      Args parsed as needed in the future. For now, no-op (the
      payloads are uint or empty so leaving Reader at the end of
      message is fine -- the dispatcher discards the rest). }
  end;
end;

procedure TWlDataSource.Offer(const MimeType: AnsiString);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteString(MimeType);
  SendRequest(0, W);  { offer }
end;

procedure TWlDataSource.DestroyRequest;
begin
  SendRequest(1);  { destroy }
end;

{ TWlDataDevice }

procedure TWlDataDevice.Dispatch(Opcode: Word; var Reader: TWlReader);
var
  NewOfferId, OldOfferId: TWlObjectId;
  Offer: TWlDataOffer;
begin
  case Opcode of
    0:  { data_offer(new_id wl_data_offer) -- server is creating an
          offer object FOR US. We must register it; subsequent
          "offer" events on this id arrive before the matching
          "selection"/"enter" delivers the offer. }
      begin
        NewOfferId := Reader.ReadNewId;
        Offer := TWlDataOffer.Create(FDisplay, NewOfferId);
        FDisplay.Register(Offer);
        if Assigned(OnDataOffer) then OnDataOffer(Self, Offer);
      end;
    5:  { selection(object|null id) -- the system clipboard now points
          at this offer (or nil). The previous CurrentSelection (if
          any) is now stale; destroy it. }
      begin
        OldOfferId := Reader.ReadObject;  { 0 means null }
        if (CurrentSelection <> nil) then
        begin
          CurrentSelection.DestroyRequest;
          CurrentSelection := nil;
        end;
        if OldOfferId <> 0 then
          CurrentSelection := TWlDataOffer(FDisplay.Find(OldOfferId))
        else
          CurrentSelection := nil;
        if Assigned(OnSelection) then OnSelection(Self, CurrentSelection);
      end;
    { 1=enter, 2=leave, 3=motion, 4=drop: DnD-only. We don't act on
      them. The dispatcher in waylandcore advances past unread args
      before handing us the next message, so silent ignore is safe. }
  end;
end;

procedure TWlDataDevice.SetSelection(Source: TWlDataSource; Serial: LongWord);
var
  W: TWlWriter;
begin
  W.Reset;
  if Source <> nil then
    W.WriteObject(Source.Id)
  else
    W.WriteObject(0);
  W.WriteUInt(Serial);
  SendRequest(1, W);  { set_selection }
end;

procedure TWlDataDevice.Release;
begin
  SendRequest(2);  { release }
end;

{ TWlDataOffer }

constructor TWlDataOffer.Create(ADisplay: TWaylandDisplay; AId: TWlObjectId);
begin
  inherited Create(ADisplay, AId);
  MimeTypes := TStringList.Create;
end;

destructor TWlDataOffer.Destroy;
begin
  MimeTypes.Free;
  inherited Destroy;
end;

procedure TWlDataOffer.Dispatch(Opcode: Word; var Reader: TWlReader);
var
  Mime: AnsiString;
begin
  case Opcode of
    0:  { offer(string mime_type) }
      begin
        Mime := Reader.ReadString;
        MimeTypes.Add(Mime);
        if Assigned(OnOffer) then OnOffer(Self, Mime);
      end;
    { 1=source_actions, 2=action: DnD-only. }
  end;
end;

procedure TWlDataOffer.Receive(const MimeType: AnsiString; Fd: cint);
var
  W: TWlWriter;
begin
  W.Reset;
  W.WriteString(MimeType);
  { Fd is passed via SCM_RIGHTS, NOT in the message body. The wire
    protocol marks "fd" args as having no in-band representation;
    QueueRequest's Fds parameter is what actually attaches them. }
  SendRequest(1, W, [Fd]);  { receive }
end;

procedure TWlDataOffer.DestroyRequest;
begin
  SendRequest(2);  { destroy }
end;

end.
