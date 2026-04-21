unit CocoaWSCustomControl;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}
{$include cocoadefines.inc}
{.$DEFINE COCOA_DEBUG_SETBOUNDS}

interface

uses
  Classes, Controls, SysUtils,
  WSControls, LCLType, LCLMessageGlue, LMessages, LCLProc, LCLIntf, Graphics, Forms,
  CocoaAll,
  CocoaConfig, CocoaPrivate,
  CocoaCustomControl, CocoaScrolling, CocoaWSScrolling,
  CocoaFullControlEdit, CocoaCursor, CocoaCaret, CocoaCommonCallback;

type

  { TLCLFullControlEditCallBack }

  // CallBack for LCL Full Control Edit (such as SynEdit/ATSynEdit)
  TLCLFullControlEditCallBack = class(TLCLCommonCallBack)
  protected
    procedure createKeyStateFromKeyUpDown(
      const event: NSEvent;
      var state: TCocoaKeyEventState ); override;
  end;

  { TCocoaWSCustomControl }

  TCocoaWSCustomControl = class(TWSCustomControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
      const AParams: TCreateParams): TLCLHandle; override;
    class procedure SetBorderStyle(const AWinControl: TWinControl;
      const ABorderStyle: TBorderStyle); override;
  end;

implementation

procedure ScrollViewSetBorderStyle(sv: NSScrollView; astyle: TBorderStyle);
const
  NSBorderStyle : array [TBorderStyle] of NSBorderType = (
    NSNoBorder,   // bsNone
    NSBezelBorder // bsSingle     (NSLineBorder is too thick)
  );
begin
  if not Assigned(sv) then Exit;
  sv.setBorderType( NSBorderStyle[astyle] );
end;

function SendIMCompostionMessage(
  const control: TWinControl; const WParam: LclType.WPARAM ): PtrInt;
var
  Mess : TLMessage;
begin
  FillChar(Mess,SizeOf(Mess),0);
  Mess.Msg:= LM_IM_COMPOSITION;
  Mess.WParam:= WParam;
  Result:= DeliverMessage( control,  Mess );
end;

// get IMEHandler by LM_IM_COMPOSITION message
function getControlIMEHandler(const control: TWinControl): ICocoaIMEControl;
var
  handle : PtrInt;
begin
  handle := SendIMCompostionMessage( control, IM_MESSAGE_WPARAM_GET_IME_HANDLER );
  Result := TObject(handle) as ICocoaIMEControl;
end;

// get Lookup Word Handler by LM_IM_COMPOSITION message
function getControlLWHandler(const control: TWinControl): ICocoaLookupWord;
var
  handle: PtrInt;
begin
  Result:= nil;
  handle := SendIMCompostionMessage( control, IM_MESSAGE_WPARAM_GET_LW_HANDLER );
  if TObject(handle) is ICocoaLookupWord then
    Result:= TObject(handle) as ICocoaLookupWord;
end;

{ TLCLFullControlEditCallBack }

{
  Key Step for IME (such as Chinese/Japanese/Korean and DeadKeys)
  1. set _sendChar:=false to avoid KeyDown Event being eaten
     in IntfUTF8KeyPress() or CN_CHAR message.
  2. KeyDown Event will be handled in TCocoaFullControlEdit.keyDown(),
     and NSInputContext.sendEvent() will be called in it,
     and function in NSTextInputClient will be called.
}
procedure TLCLFullControlEditCallBack.createKeyStateFromKeyUpDown(
  const event: NSEvent;
  var state: TCocoaKeyEventState );
begin
  inherited createKeyStateFromKeyUpDown( event, state );
  state.shouldSendCharMessage:= False;
end;

{ TCocoaWSCustomControl }

class function TCocoaWSCustomControl.CreateHandle(const AWinControl: TWinControl;
  const AParams: TCreateParams): TLCLHandle;
var
  ctrl : TCocoaCustomControl;
  sl   : TCocoaManualScrollView;
  hs   : TCocoaManualScrollHost;
  lcl  : TLCLCommonCallback;
  imeHandler : ICocoaIMEControl;
begin
  imeHandler := getControlIMEHandler(AWinControl);
  if Assigned(imeHandler) then
  begin
    // AWinControl implements ICocoaIMEControl
    // AWinControl is a Full Control Edit (such as SynEdit/ATSynEdit)
    ctrl := TCocoaFullControlEdit.alloc.lclInitWithCreateParams(AParams);
    lcl := TLCLFullControlEditCallback.Create(ctrl, AWinControl);
    TCocoaFullControlEdit(ctrl).imeHandler:= imeHandler;
    TCocoaFullControlEdit(ctrl).lwHandler:= getControlLWHandler(AWinControl);
  end
  else
  begin
    // AWinControl not implements ICocoaIMEControl
    // AWinControl is a normal Custom Control
    ctrl := TCocoaCustomControlWithBaseInputClient.alloc.lclInitWithCreateParams(AParams);
    lcl := TLCLCommonCallback.Create(ctrl, AWinControl);
  end;
  lcl.traits:= [TCocoaCbTrait.blockUpDown];
  ctrl.callback := lcl;

  sl := TCocoaWSScrollingUtil.embedInManualScrollView(ctrl);
  sl.callback := ctrl.callback;

  hs := TCocoaWSScrollingUtil.embedInManualScrollHost(sl);
  hs.callback := ctrl.callback;
  lcl.SetHandleFrame(hs);

  ScrollViewSetBorderStyle(hs, TCustomControl(AWinControl).BorderStyle );

  Result := TLCLHandle(hs);
end;

class procedure TCocoaWSCustomControl.SetBorderStyle(
  const AWinControl: TWinControl; const ABorderStyle: TBorderStyle);
begin
  if not Assigned(AWinControl) or not (AWinControl.HandleAllocated) then Exit;
  ScrollViewSetBorderStyle(  TCocoaManualScrollHost(AWinControl.Handle), ABorderStyle );
end;

end.
