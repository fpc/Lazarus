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
  SysUtils, Classes, Controls, vclGlobals, lMessages;
  
  
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
  
  TCalendar = class(TCustomControl)
  private
    FDate : String;
    FDisplaySettings : TDisplaySettings;
    FReadOnly: Boolean;
    FDayChanged: TNotifyEvent;
    FMonthChanged: TNotifyEvent;
    FYearChanged: TNotifyEvent;
    procedure SetReadOnly(const AValue: Boolean);
    Procedure GetProps;
    Procedure SetProps;
    function GetDisplaySettings: TDisplaySettings;
    procedure SetDisplaySettings(const AValue: TDisplaySettings);

    function GetDate: String;
    procedure SetDate(const AValue: String);
  protected
    procedure LMMonthChanged(var Message: TLMessage); message LM_MONTHCHANGED;
    procedure LMYEARChanged(var Message: TLMessage); message LM_YEARCHANGED;
    procedure LMDAYChanged(var Message: TLMessage); message LM_DAYCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    Property Date : String read GetDate write SetDate;
    property DisplaySettings : TDisplaySettings read GetDisplaySettings write SetDisplaySettings;
//    Property Date : TDate read GetDate write SetDate;
    property ReadOnly : Boolean read FReadOnly write SetReadOnly;
    property Visible;
//    property OnChange;
    property OnClick;
    property OnMouseMove;
    property OnMouseDown;
    property OnDayChanged : TNotifyEvent read FDayChanged write FDayChanged;
    property OnMonthChanged : TNotifyEvent read FMonthChanged write FMonthChanged;
    property OnYearChanged : TNotifyEvent read FYearChanged write FYearChanged;
  end;
  
implementation

{ TCalendar }

constructor TCalendar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCompStyle := csCalendar;
  SetBounds(0,0,250,150);
  fDisplaySettings := [dsShowHeadings, dsShowDayNames];
  Date := FormatDateTime('dd-mm-yyyy',Now);
  ControlStyle:=ControlStyle-csMultiClicks-[csAcceptsControls];
end;

destructor TCalendar.Destroy;
begin
  Inherited;
end;

function TCalendar.GetDate: String;
begin
   Result := '';
   GetPRops;
   Result := FDate;
end;

procedure TCalendar.SetDate(const AValue: String);
begin
   try
     StrtoDate(AValue);  //test to see if valid date....

     FDate := AValue;
     SetProps;
   except
     raise EInvalidDate.CreateFmt('Invalid Date :%s',[AValue]);
   //raise an error
   end;
   
end;

function TCalendar.GetDisplaySettings: TDisplaySettings;
begin
  Result := FDisplaySettings;
end;

procedure TCalendar.SetDisplaySettings(const AValue: TDisplaySettings);
begin
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

Procedure TCalendar.GetProps;
var
  Temp : TLMCalendar;
begin

   if HandleAllocated then
      begin
        CNSendMessage(LM_GETVALUE, Self, @temp);	// Get the info
        FDate := FormatDateTime('dd-mm-yyyy',Temp.Date);
      end;
end;

Procedure TCalendar.SetProps;
var
  Temp : TLMCalendar;
begin
   if HandleAllocated then
      begin
        Temp.Date := StrtoDate(FDate);
        Temp.DisplaySettings := FDisplaySettings;
        Temp.ReadOnly := fReadOnly;
        CNSendMessage(LM_SETVALUE, Self, @temp);	// Get the info
      End;

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

