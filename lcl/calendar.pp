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
    FDate : String;
    FDisplaySettings : TDisplaySettings;
    FReadOnly: Boolean;
    FDayChanged: TNotifyEvent;
    FMonthChanged: TNotifyEvent;
    FYearChanged: TNotifyEvent;
    FPropsChanged: boolean;
    function ReadOnlyIsStored: boolean;
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
    procedure AddControl; override;
  published
    Property Date: String read GetDate write SetDate;
    property DisplaySettings : TDisplaySettings read GetDisplaySettings write SetDisplaySettings;
    property ReadOnly : Boolean read FReadOnly write SetReadOnly stored ReadOnlyIsStored;
    property Visible;
//    property OnChange;
    property OnClick;
    property OnMouseMove;
    property OnMouseDown;
    property OnDayChanged : TNotifyEvent read FDayChanged write FDayChanged;
    property OnMonthChanged : TNotifyEvent read FMonthChanged write FMonthChanged;
    property OnYearChanged : TNotifyEvent read FYearChanged write FYearChanged;
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
  SetBounds(0,0,250,150);
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

procedure TCalendar.AddControl;
begin
  inherited AddControl;
end;

function TCalendar.GetDate: String;
begin
  Result := '';
  GetProps;
  Result := FDate;
end;

procedure TCalendar.SetDate(const AValue: String);
begin
  if FDate=AValue then exit;
  try
    {$IFDEF VerboseCalenderSetDate}
    writeln('TCalendar.SetDate AValue=',AValue,' ShortDateFormat=',ShortDateFormat);
    {$ENDIF}
    StrToDate(AValue);  //test to see if date valid ....
    FDate := AValue;
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

Procedure TCalendar.GetProps;
var
  Temp : TLMCalendar;
begin
  if HandleAllocated and (not (csLoading in ComponentState)) then
  begin
    CNSendMessage(LM_GETVALUE, Self, @temp);	// Get the info
    FDate := FormatDateTime(ShortDateFormat,Temp.Date);
  end;
end;

Procedure TCalendar.SetProps;
var
  Temp : TLMCalendar;
begin
  if HandleAllocated and (not (csLoading in ComponentState)) then
  begin
    FPropsChanged:=false;
    Temp.Date := StrToDate(FDate);
    Temp.DisplaySettings := FDisplaySettings;
    Temp.ReadOnly := fReadOnly;
    CNSendMessage(LM_SETVALUE, Self, @temp);	// Get the info
  End else begin
    FPropsChanged:=true;
  end;
end;

procedure TCalendar.LMDAYChanged(var Message: TLMessage);
begin
  if Assigned(OnDayChanged) then
     OnDayChanged(self);
end;

procedure TCalendar.LMMonthChanged(var Message: TLMessage);
begin
  if Assigned(OnMonthChanged) then
     OnMonthChanged(self);
end;

procedure TCalendar.LMYEARChanged(var Message: TLMessage);
begin
  if Assigned(OnYearChanged) then
     OnYearChanged(self);
end;


end.

