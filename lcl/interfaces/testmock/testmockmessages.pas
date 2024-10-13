unit TestMockMessages;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LMessages,
  TestMockMiscClasses;

type

  { TTestMockMessageDispatcher }

  TTestMockMessageDispatcher = class
    procedure DoNewWindowPos(AHandle: TTestMockWindowHandle); virtual;
  end;

  { TTestMockMessageDispatcherSample }

  TTestMockMessageDispatcherSample = class(TTestMockMessageDispatcher)
  public
    procedure Send_LM_WindowPosChanging(AHandle: TTestMockWindowHandle);

  public
    procedure DoNewWindowPos(AHandle: TTestMockWindowHandle); override;
  public
    //property EnableWindowPosSize: boolean read FEnableWindowPosSize write FEnableWindowPosSize;
  end;

var
  TestMockMessageDispatcher: TTestMockMessageDispatcher;

implementation

{ TTestMockMessageDispatcher }

procedure TTestMockMessageDispatcher.DoNewWindowPos(AHandle: TTestMockWindowHandle);
begin
  //
end;

{ TTestMockMessageDispatcherSample }

procedure TTestMockMessageDispatcherSample.Send_LM_WindowPosChanging(AHandle: TTestMockWindowHandle);
var
  wp: TWindowPos;
  m: TLMWindowPosChanging;
begin
  wp.x := AHandle.MockBounds.Left;
  wp.y := AHandle.MockBounds.Top;
  wp.cx := AHandle.MockBounds.Width;
  wp.cx := AHandle.MockBounds.Height;
  wp.flags := 0;

  // TODO: TForm may need a SWP_FRAMECHANGED / need to test when? first time?
  m.Msg := LM_WINDOWPOSCHANGING;
  m.WindowPos := @wp;
  m.Result := 0;

  AHandle.WinControl.Dispatch(m);
  //r := m.Result;

  // TODO: if WP changed, change the boundsrect??
end;

procedure TTestMockMessageDispatcherSample.DoNewWindowPos(AHandle: TTestMockWindowHandle);
begin
  Send_LM_WindowPosChanging(AHandle);
end;

initialization
  TestMockMessageDispatcher := TTestMockMessageDispatcher.Create; // do nothing
  //TestMockMessageDispatcher := TTestMockMessageDispatcherSample.Create;
finalization
  TestMockMessageDispatcher.Free;

end.

