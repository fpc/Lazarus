{ $Id$ }
{                        -----------------------------------  
                         DBGWatch.pp  -  Debug Watch classes
                         ----------------------------------- 
 
 @created(Wed Feb 25st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

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
    FValue: String;
    FName: String;
    FOnChange: TNotifyEvent;
    procedure SetValue(const AValue: String);
    procedure SetName(const AValue: String);
  protected
  public
    property Name: String read FName write SetName;
    property Value: String read FValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TDBGWatches = class(TCollection)
  private
    function GetItem(const AnIndex: Integer): TDBGWatch;
    procedure SetItem(const AnIndex: Integer; const Value: TDBGWatch);
  protected
  public
    property Items[const AnIndex: Integer]: TDBGWatch 
                 read GetItem write SetItem; default;
  end;

implementation

{ TDBGWatch }

procedure TDBGWatch.SetName(const AValue: String);
begin
  FName := AValue;
end;

procedure TDBGWatch.SetValue(const AValue: String);
begin
  FValue := AValue;
end;

{ TDBGWatches }

function TDBGWatches.GetItem(const AnIndex: Integer): TDBGWatch;
begin
  Result:=nil
end;

procedure TDBGWatches.SetItem(const AnIndex: Integer; const Value: TDBGWatch);
begin
end;

end.
{ =============================================================================
  $Log$
  Revision 1.3  2001/10/18 13:01:31  lazarus
  MG: fixed speedbuttons numglyphs>1 and started IDE debugging

  Revision 1.2  2001/02/25 16:44:57  lazarus
  MWE:
    + Added header and footer

}
