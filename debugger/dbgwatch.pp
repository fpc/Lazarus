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
  TDBGWatch = class(TCollectionItem)
  private
    FEnabled: Boolean;
    FExpression: String;
    FOnChange: TNotifyEvent;
    function  GetValid: Boolean;
    function  GetValue: String;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetExpression(const AValue: String);
    procedure SetValue(const AValue: String);
  protected
  public
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Expression: String read FExpression write SetExpression;
    property Valid: Boolean read GetValid;
    property Value: String read GetValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TDBGWatches = class(TCollection)
  private
    function GetItem(const AnIndex: Integer): TDBGWatch;
    procedure SetItem(const AnIndex: Integer; const AValue: TDBGWatch);
  protected
  public
    constructor Create;
    property Items[const AnIndex: Integer]: TDBGWatch read GetItem write SetItem; default;
  end;

implementation

{ TDBGWatch }

function TDBGWatch.GetValid: Boolean;
begin
  Result := False;
end;

function TDBGWatch.GetValue: String;
begin
  if Valid 
  then begin
  end
  else Result := '<invalid>';
end;

procedure TDBGWatch.SetEnabled(const AValue: Boolean);
begin
  FEnabled := AValue;
end;

procedure TDBGWatch.SetExpression(const AValue: String);
begin
  FExpression := AValue;
end;

procedure TDBGWatch.SetValue(const AValue: String);
begin
end;

{ TDBGWatches }

constructor TDBGWatches.Create;
begin
  inherited Create(TDBGWatch);
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
  Revision 1.4  2001/11/05 00:12:51  lazarus
  MWE: First steps of a debugger.

  Revision 1.3  2001/10/18 13:01:31  lazarus
  MG: fixed speedbuttons numglyphs>1 and started IDE debugging

  Revision 1.2  2001/02/25 16:44:57  lazarus
  MWE:
    + Added header and footer

}
