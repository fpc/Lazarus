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
  SysUtils, Classes, LCLStrConsts, Controls, vclGlobals, lMessages;
  
  
Type

  TDisplaySetting = (dsShowHeadings, dsShowDayNames, dsNoMonthChange,
                     dsShowWeekNumbers,dsStartMonday);
  TDisplaySettings = set of TDisplaySetting;
  
  TLMCalendar = record
    Date : TDateTime;
    DisplaySettings : TDisplaySettings;
    Readonly : Boolean;
  end;
  
  EInvalidDate = class(Exception);
  
  TCalendar = class(TWinControl)
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
  published
    Property Align;
    Property Anchors;
    Property Constraints;
    Property Date: String read GetDate write SetDate stored false;
    Property DateTime: TDateTime read GetDateTime write SetDateTime;
    property DisplaySettings: TDisplaySettings read GetDisplaySettings write SetDisplaySettings;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly stored ReadOnlyIsStored;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnClick;
    property OnResize;
    property OnChangeBounds;
    property OnMouseUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnDayChanged: TNotifyEvent read FDayChanged write FDayChanged;
    property OnMonthChanged: TNotifyEvent read FMonthChanged write FMonthChanged;
    property OnYearChanged: TNotifyEvent read FYearChanged write FYearChanged;
  end;
  
procedure Register;
  
implementation

procedure Register;
begin
  RegisterComponents('Misc',[TCalendar]);
end;

{ TCalendar }

constructor TCalendar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle := csCalendar;
  SetInitialBounds(0,0,250,150);
  fDisplaySettings := [dsShowHeadings, dsShowDayNames];
  ControlStyle:=ControlStyle-csMultiClicks-[csAcceptsControls];
  Date := FormatDateTime(ShortDateFormat,Now);
end;

destructor TCalendar.Destroy;
begin
  Inherited Destroy;
end;

procedure TCalendar.Loaded;
begin
  inherited Loaded;
  if FPropsChanged then SetProps;
end;

procedure TCalendar.InitializeWnd;
begin
  inherited InitializeWnd;
  if FPropsChanged then SetProps;
end;

function TCalendar.GetDate: String;
begin
  Result := '';
  GetProps;
  Result := FDateAsString;
end;

procedure TCalendar.SetDate(const AValue: String);
var
  NewDate: TDateTime;
begin
  if FDateAsString=AValue then exit;
  try
    {$IFDEF VerboseCalenderSetDate}
    writeln('TCalendar.SetDate A AValue=',AValue,' FDateAsString=',FDateAsString,' FDate=',FDate,' ShortDateFormat=',ShortDateFormat);
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

function TCalendar.GetDisplaySettings: TDisplaySettings;
begin
  Result := FDisplaySettings;
end;

procedure TCalendar.SetDisplaySettings(const AValue: TDisplaySettings);
begin
  if FDisplaySettings = AValue then exit;
  FDisplaySettings := AValue;
  SetProps;
end;

procedure TCalendar.SetReadOnly(const AValue: Boolean);
begin
  if (FReadOnly <> aValue) then
  Begin
    FReadOnly := aValue;
    SetProps;
  end;
end;

function TCalendar.ReadOnlyIsStored: boolean;
begin
  Result:=FReadOnly;
end;

function TCalendar.GetDateTime: TDateTime;
begin
  Result:=FDate;
end;

procedure TCalendar.SetDateTime(const AValue: TDateTime);
var
  OldDate: TDateTime;
begin
  OldDate:=FDate;
  FDate:=AValue;
  FDateAsString:=FormatDateTime(ShortDateFormat,FDate);
  {$IFDEF VerboseCalenderSetDate}
  writeln('TCalendar.SetDateTime FDate=',FDate,' FDateAsString=',FDateAsString,' ShortDateFormat=',ShortDateFormat);
  {$ENDIF}
  if OldDate=FDate then exit;
  SetProps;
end;

Procedure TCalendar.GetProps;
var
  Temp : TLMCalendar;
begin
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
  begin
    SendMsgToInterface(LM_GETVALUE, Self, @temp);	// Get the info
    FDate := Temp.Date;
    FDateAsString := FormatDateTime(ShortDateFormat,FDate);
    {$IFDEF VerboseCalenderSetDate}
    writeln('TCalendar.GetProps A ',FDate,' ',FDateAsString);
    {$ENDIF}
  end;
end;

Procedure TCalendar.SetProps;
var
  Temp : TLMCalendar;
begin
  if HandleAllocated and ([csLoading,csDestroying]*ComponentState=[]) then
  begin
    FPropsChanged:=false;
    Temp.Date := FDate;
    Temp.DisplaySettings := FDisplaySettings;
    Temp.ReadOnly := fReadOnly;
    {$IFDEF VerboseCalenderSetDate}
    writeln('TCalendar.SetProps A ',FDate,' ',FDateAsString);
    {$ENDIF}
    SendMsgToInterface(LM_SETVALUE, Self, @temp);	// Get the info
  End else begin
    FPropsChanged:=true;
  end;
end;

procedure TCalendar.LMDAYChanged(var Message: TLMessage);
begin
  if Assigned(OnDayChanged) then OnDayChanged(self);
  if Assigned(OnChange) then OnChange(self);
end;

procedure TCalendar.LMMonthChanged(var Message: TLMessage);
begin
  if Assigned(OnMonthChanged) then OnMonthChanged(self);
  if Assigned(OnChange) then OnChange(self);
end;

procedure TCalendar.LMYEARChanged(var Message: TLMessage);
begin
  if Assigned(OnYearChanged) then OnYearChanged(self);
  if Assigned(OnChange) then OnChange(self);
end;


end.

