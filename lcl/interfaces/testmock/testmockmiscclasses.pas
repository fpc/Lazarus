unit TestMockMiscClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, Controls, LCLType, Forms, LazLoggerBase;

type

  { TTestMockHandle }

  TTestMockHandle = class
  public
    constructor Create;
    destructor Destroy; override;
    function IsAlive: boolean;
  end;
  TTestMockHandleClass = class of TTestMockHandle;

  TTestMockHandleList = specialize TFPGList<TTestMockHandle>;

  { TTestMockWindowHandle }

  TTestMockWindowHandle = class(TTestMockHandle)
  private
    FProps: TStringList;
    FWinControl: TWinControl;
    FMock_DC_Count: Integer;
    FEnabled: boolean;
    // clientrect offs
  public type
    TTestMockScrollBarBtn = (sbBtnUpLeft, sbBtnDownRight);
    TTestMockScrollBarBtns = set of TTestMockScrollBarBtn;
  public
    MockScrollBars: array [TScrollBarKind] of record // sbHorizontal, sbVertical
      ScrollInfo: TScrollInfo;
      ScrollVisible: Boolean;
      ScrollBtnDisabled: TTestMockScrollBarBtns;
    end;
    MockBounds: TRect; // TODO: clientrect, for a TForm outer bounds are bigger
  public
    constructor Create(AWinControl: TWinControl);
    destructor Destroy; override;

    procedure SetProp(AStr: String; AData: Pointer);
    function  GetProp(AStr: String): Pointer;
    function RemoveProp(AStr: String): Pointer;

    property WinControl: TWinControl read FWinControl;
    property Enabled: boolean read FEnabled write FEnabled;
  end;


  { TTestMockMonitor }

  TTestMockMonitor = class // (TTestMockHandle)
  private
    FBounds: TRect;
    FDPI: Integer;
  public
    constructor Create;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer);
    property Bounds: TRect read FBounds write FBounds;
    property DPI: Integer read FDPI write FDPI;
  end;

  { TTestMockMonitorList }

  TTestMockMonitorList = class(specialize TFPGObjectList<TTestMockMonitor>)
  public
    constructor Create;
  end;

  { TTestMockFontHandle }

  TTestMockFontHandle = class(TTestMockHandle)
  public
    FLogFont: TLogFont;
    FLongFontName: string;
  public
    constructor Create(const ALogFont: TLogFont; const ALongFontName: string);
  end;

  { TTestMockDeviceContext }

  TTestMockDeviceContext = class(TTestMockHandle)
  private
    FMock_Font: TTestMockFontHandle;
    FMock_WinHandle: TTestMockWindowHandle;
    function GetHandle: HWND;
  public
    constructor Create(ACreateForHandle: TTestMockWindowHandle);
    destructor Destroy; override;
    property  Mock_Font: TTestMockFontHandle read FMock_Font write FMock_Font;
    property  Mock_WinHandle: TTestMockWindowHandle read FMock_WinHandle;
    property  Handle: HWND read GetHandle;
  end;

  function CheckNoLeaks: boolean;
  function CheckAllHandlesFreed: boolean;

  function IsMockHandle(AHandle: THandle; AClass: TTestMockHandleClass): boolean;
  function IsMockHandleOrNIl(AHandle: THandle; AClass: TTestMockHandleClass): boolean;

const
  TestMockDefaultLogFontName = 'MyFont';
var
  TestMockHandleList: TTestMockHandleList;
  TestMockMonitorList: TTestMockMonitorList;
  TestMockRegisteredClipBoardFormats: TStringList;

  TestMockDefaultLogFont: TLogFont = // Captured on Win 10
    ( lfHeight: -12;  lfWidth: 0;
      lfEscapement: 0;  lfOrientation: 0;
      lfWeight: 400;  lfItalic: 0;  lfUnderline: 0;  lfStrikeOut: 0;
      lfCharSet: 1;  lfOutPrecision: 0;  lfClipPrecision: 0;  lfQuality: 0;
      lfPitchAndFamily: 0;
      lfFaceName: 'MyFont';
    );


implementation

function CheckNoLeaks: boolean;
begin
  Result := CheckAllHandlesFreed;
end;

function CheckAllHandlesFreed: boolean;
begin
  Result := TestMockHandleList.Count = 0;
end;

function IsMockHandle(AHandle: THandle; AClass: TTestMockHandleClass): boolean;
begin
  Result := (TObject(AHandle) is AClass) and
            (TTestMockHandle(AHandle).IsAlive);
end;

function IsMockHandleOrNIl(AHandle: THandle; AClass: TTestMockHandleClass): boolean;
begin
  Result := (AHandle = 0) or IsMockHandle(AHandle, AClass);
end;

{ TTestMockHandle }

constructor TTestMockHandle.Create;
begin
  TestMockHandleList.Add(Self);
end;

destructor TTestMockHandle.Destroy;
begin
  assert(IsAlive, 'TTestMockHandle.Destroy: IsAlive');
  TestMockHandleList.Remove(Self);
  inherited Destroy;
end;

function TTestMockHandle.IsAlive: boolean;
begin
  Result := (TestMockHandleList <> nil) and (TestMockHandleList.IndexOf(Self) >= 0);
end;

{ TTestMockWindowHandle }

constructor TTestMockWindowHandle.Create(AWinControl: TWinControl);
begin
  inherited Create;
  FProps := TStringList.Create;
  FWinControl := AWinControl;
end;

destructor TTestMockWindowHandle.Destroy;
begin
  FProps.Free;
  inherited Destroy;
end;

procedure TTestMockWindowHandle.SetProp(AStr: String; AData: Pointer);
var
  i: Integer;
begin
  i := FProps.IndexOf(AStr);
  if i >= 0 then
    FProps.Objects[i] := TObject(AData)
  else
    FProps.AddObject(AStr, TObject(AData));
end;

function TTestMockWindowHandle.GetProp(AStr: String): Pointer;
var
  i: Integer;
begin
  i := FProps.IndexOf(AStr);
  if i >= 0 then
    Result := Pointer(FProps.Objects[i])
  else
    Result := nil;
end;

function TTestMockWindowHandle.RemoveProp(AStr: String): Pointer;
var
  i: Integer;
begin
  i := FProps.IndexOf(AStr);
  if i >= 0 then begin
    Result := Pointer(FProps.Objects[i]);
    FProps.Delete(i);
  end
  else
    Result := nil;
end;

{ TTestMockMonitor }

constructor TTestMockMonitor.Create;
begin
  inherited Create;
  SetBounds(0,0,1600,800);
  FDPI := 96;
end;

procedure TTestMockMonitor.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  FBounds := Rect(ALeft, ATop, ALeft+AWidth, ATop+AHeight);
end;

{ TTestMockMonitorList }

constructor TTestMockMonitorList.Create;
begin
  inherited Create;
  Add(TTestMockMonitor.Create);
end;

{ TTestMockFontHandle }

constructor TTestMockFontHandle.Create(const ALogFont: TLogFont; const ALongFontName: string);
begin
  inherited Create;
  FLogFont := ALogFont;
  if ALogFont.lfFaceName[0] = #0 then begin
    FLogFont := TestMockDefaultLogFont;
    if ALogFont.lfHeight <> 0 then
      FLogFont.lfHeight := ALogFont.lfHeight;
  end;

  FLongFontName := ALongFontName;
end;

{ TTestMockDeviceContext }

function TTestMockDeviceContext.GetHandle: HWND;
begin
  Result := HWND(FMock_WinHandle);
end;

constructor TTestMockDeviceContext.Create(ACreateForHandle: TTestMockWindowHandle);
begin
  inherited Create;
  FMock_WinHandle := ACreateForHandle;
  if FMock_WinHandle <> nil then begin
    if FMock_WinHandle.FMock_DC_Count  > 0 then DebugLn(['creating more than one DC for handle']);
    FMock_WinHandle.FMock_DC_Count := FMock_WinHandle.FMock_DC_Count + 1;
  end;
end;

destructor TTestMockDeviceContext.Destroy;
begin
  if FMock_WinHandle <> nil then begin
    assert(FMock_WinHandle.FMock_DC_Count  > 0, 'TTestMockDeviceContext.Destroy: FMock_WinHandle.FMock_DC_Count  > 0');
    FMock_WinHandle.FMock_DC_Count := FMock_WinHandle.FMock_DC_Count - 1;
  end;
  inherited Destroy;
end;


initialization
  TestMockHandleList := TTestMockHandleList.Create;
  TestMockMonitorList := TTestMockMonitorList.Create;
  TestMockRegisteredClipBoardFormats := TStringList.Create;

finalization
  assert(CheckNoLeaks, 'no handles leaked');
  TestMockHandleList.Free;
  TestMockMonitorList.Free;
  TestMockRegisteredClipBoardFormats.Free;

end.

