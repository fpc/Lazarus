{ $Id$ }
{                        -----------------------------------  
                         DBGWatch.pp  -  Debug Watch classes
                         ----------------------------------- 
 
 @created(Wed Feb 25st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains the class definitions of the 
 Watches used by the debugger
 
/*************************************************************************** 
 *                                                                         * 
 *   This program is free software; you can redistribute it and/or modify  * 
 *   it under the terms of the GNU General Public License as published by  * 
 *   the Free Software Foundation; either version 2 of the License, or     * 
 *   (at your option) any later version.                                   * 
 *                                                                         * 
 ***************************************************************************/ 
} 
unit DBGWatch;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  TDBGWatchClass = class of TDBGWatch;
  TDBGWatch = class(TCollectionItem)
  private
    FDebugger: TObject;  // reference to our debugger
    FEnabled: Boolean;
    FExpression: String;
    FValue: String;
    FOnChange: TNotifyEvent;
    procedure SetEnabled(const AValue: Boolean);
  protected
    procedure DoEnableChange; virtual;
    procedure DoStateChange; virtual; 
    function  GetValue: String; virtual;
    function  GetValid: Boolean; virtual;
    procedure SetExpression(const AValue: String); virtual;
    procedure SetValue(const AValue: String); virtual;
    property  Debugger: TObject read FDebugger;
  public
    constructor Create(ACollection: TCollection); override;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Expression: String read FExpression write SetExpression;
    property Valid: Boolean read GetValid;
    property Value: String read GetValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TDBGWatches = class(TCollection)
  private
    FDebugger: TObject;  // reference to our debugger
    function GetItem(const AnIndex: Integer): TDBGWatch;
    procedure SetItem(const AnIndex: Integer; const AValue: TDBGWatch);
  protected
    procedure DoStateChange;  
  public
    constructor Create(const ADebugger: TObject; const AWatchClass: TDBGWatchClass); 
    property Items[const AnIndex: Integer]: TDBGWatch read GetItem write SetItem; default;
  end;
  
implementation 

uses
  Debugger;      

{ TDBGWatch }

constructor TDBGWatch.Create(ACollection: TCollection); 
begin
  inherited Create(ACollection);
  FEnabled := False;
  FDebugger := TDBGWatches(ACollection).FDebugger;
end;

procedure TDBGWatch.DoEnableChange; 
begin
end;

procedure TDBGWatch.DoStateChange;  
begin
end;

function TDBGWatch.GetValid: Boolean;
begin
  Result := False;
end;

function TDBGWatch.GetValue: String;
begin
  if Valid 
  then Result := '<unknown>'
  else Result := '<invalid>';
end;

procedure TDBGWatch.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue
  then begin
    FEnabled := AValue;
    DoEnableChange;
 end;
end;

procedure TDBGWatch.SetExpression(const AValue: String);
begin
end;

procedure TDBGWatch.SetValue(const AValue: String);
begin
end;

{ TDBGWatches }

constructor TDBGWatches.Create(const ADebugger: TObject; const AWatchClass: TDBGWatchClass);
begin                   
  FDebugger := ADebugger;
  inherited Create(AWatchClass);
end;

procedure TDBGWatches.DoStateChange;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    GetItem(n).DoStateChange;
end;

function TDBGWatches.GetItem(const AnIndex: Integer): TDBGWatch;
begin
  Result := TDBGWatch(inherited GetItem(AnIndex));
end;

procedure TDBGWatches.SetItem(const AnIndex: Integer; const AValue: TDBGWatch);
begin
  inherited SetItem(AnIndex, AValue);
end;

end.
{ =============================================================================
  $Log$
  Revision 1.6  2002/02/05 23:16:48  lazarus
  MWE: * Updated tebugger
       + Added debugger to IDE

  Revision 1.5  2001/11/06 23:59:13  lazarus
  MWE: + Initial breakpoint support
       + Added exeption handling on process.free

  Revision 1.4  2001/11/05 00:12:51  lazarus
  MWE: First steps of a debugger.

  Revision 1.3  2001/10/18 13:01:31  lazarus
  MG: fixed speedbuttons numglyphs>1 and started IDE debugging

  Revision 1.2  2001/02/25 16:44:57  lazarus
  MWE:
    + Added header and footer

}
