{
 /***************************************************************************
                               Calendar.pp
                             -------------------
                             Component Library Calendar Component
                   Initial Revision  : Wed Dec 05 2001

 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}

{
@abstract(Calendar component)
@author(Shane Miller)
@created(05 Dev 2001)
}
unit calendar;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, vclGlobals, lMessages;
  
  
Type

  TDisplaySetting = (dsShowHeadings, dsShowDayNames, dsNoMonthChange, dsShowWeekNumbers,dsStartMonday);
  TDisplaySettings = set of TDisplaySetting;
  
  TLMCalendar = record
    Date : TDateTime;
    DisplaySettings : TDisplaySettings;
  end;
  

  TCalendar = class(TCustomControl)
  private
    FDate : String;
    FDisplaySettings : TDisplaySettings;
    Procedure GetProps;
    Procedure SetProps( aDate: String; aDisplaySettings : TDisplaySettings);
    function GetDisplaySettings: TDisplaySettings;
    procedure SetDisplaySettings(const AValue: TDisplaySettings);

    function GetDate: String;
    procedure SetDate(const AValue: String);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    Property Date : String read GetDate write SetDate;
    property DisplaySettings : TDisplaySettings read GetDisplaySettings write SetDisplaySettings;
//    Property Date : TDate read GetDate write SetDate;
    property Visible;
  end;
  
implementation

{ TCalendar }

constructor TCalendar.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
  {create the control}
  fCompStyle := csCalendar;
  setbounds(0,0,250,150);
  fDisplaySettings := [dsShowHeadings, dsShowDayNames];
  Date := FormatDateTime('dd-mm-yyyy',Now);
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
var
  Temp : TDateTime;
begin
   SetProps(AValue,FDisplaySettings);
end;

function TCalendar.GetDisplaySettings: TDisplaySettings;
begin
  Result := FDisplaySettings;
end;

procedure TCalendar.SetDisplaySettings(const AValue: TDisplaySettings);
begin
   FDisplaySettings := AValue;
   SetProps(FDate,FDisplaySettings);
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

Procedure TCalendar.SetProps( aDate: String; aDisplaySettings : TDisplaySettings);
var
  Temp : TLMCalendar;
begin
   if HandleAllocated then
      begin
        Temp.Date := StrtoDate(aDate);
        Temp.DisplaySettings := aDisplaySettings;
        CNSendMessage(LM_SETVALUE, Self, @temp);	// Get the info
      End;

end;

end.

