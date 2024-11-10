unit CocoaCallback;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}
{$interfaces corba}

interface

uses
  Classes, SysUtils,
  LclType,
  MacOSAll, CocoaAll, CocoaGDIObjects;

type
  { ICommonCallback }

  ICommonCallback = interface
    // mouse events
    function MouseUpDownEvent(Event: NSEvent; AForceAsMouseUp: Boolean = False; AOverrideBlock: Boolean = False): Boolean;
    procedure MouseClick;
    function MouseMove(Event: NSEvent): Boolean;

    // KeyEvXXX methods were introduced to allow a better control
    // over when Cocoa keys processing is being called.
    // (The initial KeyEvent() replicates Carbon implementation, and it's not
    // suitable for Cocoa, due to the use of OOP and the extual "inherited Key..."needs to be called
    // where for Carbon there's a special fucntion to call the "next event handler" present)
    //
    // The desired use is as following:
    // Call KeyEvPrepare and pass NSEvent object
    // after that call KeyEvBefore and pass a flag if AllowCocoaHandle
    //
    // The call would populate the flag. If it's "True" you should call "inherited" method (to let Cocoa handle the key).
    // If the flag returned "False", you should not call inherited.
    //
    // No matter what the flag value was you should call KeyEvAfter.
    procedure KeyEvBefore(Event: NSEvent; out AllowCocoaHandle: boolean);
    procedure KeyEvAfter;
    procedure KeyEvAfterDown(out AllowCocoaHandle: boolean);
    procedure KeyEvHandled;
    procedure SetTabSuppress(ASuppress: Boolean);

    // only Cocoa Event Mechanism (no LCL Event), if the IME is in use
    function IsCocoaOnlyState: Boolean;
    procedure SetCocoaOnlyState( state:Boolean );

    function scrollWheel(Event: NSEvent): Boolean;
    function CanFocus: Boolean;
    // size, pos events
    procedure frameDidChange(sender: id);
    procedure boundsDidChange(sender: id);
    // misc events
    procedure Draw(ctx: NSGraphicsContext; const bounds, dirty: NSRect);
    procedure DrawBackground(ctx: NSGraphicsContext; const bounds, dirty: NSRect);
    procedure DrawOverlay(ctx: NSGraphicsContext; const bounds, dirty: NSRect);
    procedure BecomeFirstResponder;
    procedure ResignFirstResponder;
    procedure DidBecomeKeyNotification;
    procedure DidResignKeyNotification;
    function SendOnEditCut: Boolean;
    function SendOnEditPaste: Boolean;
    procedure SendOnChange;
    procedure SendOnTextChanged;
    procedure scroll(isVert: Boolean; Pos: Integer; AScrollPart: NSScrollerPart = NSScrollerNoPart);
    // non event methods
    function DeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult;
    function GetPropStorage: TStringList;
    function GetContext: TCocoaContext;
    function GetTarget: TObject;
    function GetHasCaret: Boolean;
    function GetCallbackObject: TObject;
    procedure SetHasCaret(AValue: Boolean);
    function GetIsOpaque: Boolean;
    procedure SetIsOpaque(AValue: Boolean);
    function GetShouldBeEnabled: Boolean;
    // the method is called, when handle is being destroyed.
    // the callback object to stay alive a little longer than LCL object (Target)
    // thus it needs to know that LCL object has been destroyed.
    // After this called has been removed, any Cocoa events should not be
    // forwarded to LCL target
    procedure RemoveTarget;

    procedure InputClientInsertText(const utf8: string);

    function HandleFrame: NSView;

    // properties
    property HasCaret: Boolean read GetHasCaret write SetHasCaret;
    property IsOpaque: Boolean read GetIsOpaque write SetIsOpaque;
    property CocoaOnlyState: Boolean read IsCocoaOnlyState write SetCocoaOnlyState;
  end;

implementation

end.

