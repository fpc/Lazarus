{
 /***************************************************************************
                               Calendar.pp
                             -------------------
                             Component Library Calendar Component
                   Initial Revision  : Wed Dec 05 2001

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{
@abstract(Calendar component)
@author(Shane Miller)
@created(05 Dev 2001)
}
unit Calendar;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, LCLType, LCLStrConsts, lMessages, Controls;
  
  
Type
  { TCustomCalendar }

  TDisplaySetting = (dsShowHeadings, dsShowDayNames, dsNoMonthChange,
                     dsShowWeekNumbers,dsStartMonday);
  TDisplaySettings = set of TDisplaySetting;
  
  EInvalidDate = class(Exception);
  
  TCustomCalendar = class(TWinControl)
  private
    FDateAsString : String;
    FDate: TDateTime; // last valid date
    FDisplaySettings : TDisplaySettings;
    FOnChange: TNotifyEvent;
    FReadOnly: Boolean;
    FDayChanged: TNotifyEvent;
    FMonthChanged: TNotifyEvent;
    FYearChanged: TNotifyEvent;
    FPropsChanged: boolean;
    function GetDateTime: TDateTime;
    function ReadOnlyIsStored: boolean;
    procedure SetDateTime(const AValue: TDateTime);
    procedure SetReadOnly(const AValue: Boolean);
    Procedure GetProps;
    Procedure SetProps;
    function GetDisplaySettings: TDisplaySettings;
    procedure SetDisplaySettings(const AValue: TDisplaySettings);
    function GetDate: String;
    procedure SetDate(const AValue: String);
  protected
    procedure LMMonthChanged(var Message: TLMessage); message LM_MONTHCHANGED;
    procedure LMYearChanged(var Message: TLMessage); message LM_YEARCHANGED;
    procedure LMDayChanged(var Message: TLMessage); message LM_DAYCHANGED;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure InitializeWnd; override;
    property Date: String read GetDate write SetDate stored false;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property DisplaySettings: TDisplaySettings read GetDisplaySettings write SetDisplaySettings;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly stored ReadOnlyIsStored;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDayChanged: TNotifyEvent read FDayChanged write FDayChanged;
    property OnMonthChanged: TNotifyEvent read FMonthChanged write FMonthChanged;
    property OnYearChanged: TNotifyEvent read FYearChanged write FYearChanged;
  end;
  
  
  { TCalendar }
  
  TCalendar = class(TCustomCalendar)
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Date;
    property DateTime;
    property DisplaySettings;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnDayChanged;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMonthChanged;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnYearChanged;
    property PopupMenu;
    property ReadOnly;
    property Tabstop;
    property Visible;
  end;
  
  
procedure Register;
  
implementation

uses
  WSCalendar;

procedure Register;
begin
  RegisterComponents('Misc',[TCalendar]);
end;

{ TCustomCalendar }

constructor TCustomCalendar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csCalendar;
  SetInitialBounds(0,0,250,150);
  fDisplaySettings := [dsShowHeadings, dsShowDayNames];
  ControlStyle:=ControlStyle-csMultiClicks-[csAcceptsControls];
  Date := FormatDateTime(ShortDateFormat,Now);
end;

destructor TCustomCalendar.Destroy;
begin
  Inherited Destroy;
end;

procedure TCustomCalendar.Loaded;
begin
  inherited Loaded;
  if FPropsChanged then SetProps;
end;

procedure TCustomCalendar.InitializeWnd;
begin
  inherited InitializeWnd;
  if FPropsChanged then SetProps;
end;

function TCustomCalendar.GetDate: String;
begin
  Result := '';
  GetProps;
  Result := FDateAsString;
end;

procedure TCustomCalendar.SetDate(const AValue: String);
var
  NewDate: TDateTime;
begin
  if FDateAsString=AValue then exit;
  try
    {$IFDEF VerboseCalenderSetDate}
    DebugLn('TCustomCalendar.SetDate A AValue=',AValue,' FDateAsString=',FDateAsString,' FDate=',FDate,' ShortDateFormat=',ShortDateFormat);
    {$ENDIF}
    NewDate:=StrToDate(AValue);  //test to see if date valid ....
    // no exception => set valid date
    FDateAsString := AValue;
    FDate := NewDate;
  except
    raise EInvalidDate.CreateFmt(rsInvalidDate, [AValue]);
  end;
  SetProps;
end;

function TCustomCalendar.GetDisplaySettings: TDisplaySettings;
begin
  Result := FDisplaySettings;
end;

procedure TCustomCalendar.SetDisplaySettings(const AValue: TDisplaySettings);
begin
  if FDisplaySettings = AValue then exit;
  FDisplaySettings := AValue;
  SetProps;
end;

procedure TCustomCalendar.SetReadOnly(const AValue: Boolean);
begin
  if (FReadOnly <> aValue) then
  Begin
    FReadOnly := aValue;
    SetProps;
  end;
end;

function TCustomCalendar.ReadOnlyIsStored: boolean;
begin
  Result:=FReadOnly;
end;

function TCustomCalendar.GetDateTime: TDateTime;
begin
  Result:=FDate;
end;

procedure TCustomCalendar.SetDateTime(const AValue: TDateTime);
var
  OldDate: TDateTime;
begin
  OldDate:=FDate;
  FDate:=AValue;
  FDateAsString:=FormatDateTime(ShortDateFormat,FDate);
  {$IFDEF VerboseCalenderSetDate}
  DebugLn('TCustomCalendar.SetDateTime FDate=',FDate,' FDateAsString=',FDateAsString,' ShortDateFormat=',ShortDateFormat);
  {$ENDIF}
  if OldDate=FDate then exit;
  SetProps;
end;

Procedure TCustomCalendar.GetProps;
begin
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
  begin
    FDate := TWSCalendarClass(WidgetSetClass).GetDateTime(Self);
    FDateAsString := FormatDateTime(ShortDateFormat,FDate);
    {$IFDEF VerboseCalenderSetDate}
    DebugLn('TCustomCalendar.GetProps A ',FDate,' ',FDateAsString);
    {$ENDIF}
  end;
end;

Procedure TCustomCalendar.SetProps;
begin
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
  begin
    FPropsChanged:=false;
    {$IFDEF VerboseCalenderSetDate}
    DebugLn('TCustomCalendar.SetProps A ',FDate,' ',FDateAsString);
    {$ENDIF}
    TWSCalendarClass(WidgetSetClass).SetDateTime(Self, FDate);
    TWSCalendarClass(WidgetSetClass).SetDisplaySettings(Self, FDisplaySettings);
    TWSCalendarClass(WidgetSetClass).SetReadOnly(Self, FReadOnly);
  end else begin
    FPropsChanged:=true;
  end;
end;

procedure TCustomCalendar.LMDAYChanged(var Message: TLMessage);
begin
  if Assigned(OnDayChanged) then OnDayChanged(self);
  if Assigned(OnChange) then OnChange(self);
end;

procedure TCustomCalendar.LMMonthChanged(var Message: TLMessage);
begin
  if Assigned(OnMonthChanged) then OnMonthChanged(self);
  if Assigned(OnChange) then OnChange(self);
end;

procedure TCustomCalendar.LMYEARChanged(var Message: TLMessage);
begin
  if Assigned(OnYearChanged) then OnYearChanged(self);
  if Assigned(OnChange) then OnChange(self);
end;


end.

