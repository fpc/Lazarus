{ $Id$ }
{                        ---------------------------------------  
                         DBGBreakpoint.pp  -  Breakpoint classes
                         --------------------------------------- 
 
 @created(Wed Feb 25st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains the class definitions of the 
 Breakpoints used by the debugger
 
/*************************************************************************** 
 *                                                                         * 
 *   This program is free software; you can redistribute it and/or modify  * 
 *   it under the terms of the GNU General Public License as published by  * 
 *   the Free Software Foundation; either version 2 of the License, or     * 
 *   (at your option) any later version.                                   * 
 *                                                                         * 
 ***************************************************************************/ 
} 
unit DBGBreakpoint;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type
  TDBGBreakPointActions = (bpaStop, bpaEnableGroup, bpaDisableGroup);

  TDBGBreakPointGroup = class;

  TDBGBreakPoint = class(TCollectionItem)
  private
    FValid: Boolean;
    FEnabled: Boolean;
    FHitCount: Integer;
    FExpression: String;
    FActions: TDBGBreakPointActions;
    procedure SetActions(const AValue: TDBGBreakPointActions);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetExpression(const AValue: String);
    procedure SetHitCount(const AValue: Integer);
    procedure SetValid(const AValue: Boolean);
  protected
  public
    procedure AddDisableGroup(const AGroup: TDBGBreakPointGroup);
    procedure AddEnableGroup(const AGroup: TDBGBreakPointGroup);
    procedure RemoveDisableGroup(const AGroup: TDBGBreakPointGroup);
    procedure RemoveEnableGroup(const AGroup: TDBGBreakPointGroup);
    property Actions: TDBGBreakPointActions read FActions write SetActions;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property HitCount: Integer read FHitCount write SetHitCount;
    property Expression: String read FExpression write SetExpression;
    property Valid: Boolean read FValid write SetValid;
  end;

  TDBGBreakPointGroup = class(TCollection)
  private
    FEnabled: Boolean;
    FName: String;
    function GetItem(const AnIndex: Integer): TDBGBreakPoint;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetItem(const AnIndex: Integer; const AValue: TDBGBreakPoint);
    procedure SetName(const AValue: String);
  protected
  public
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Items[const AnIndex: Integer]: TDBGBreakPoint 
                   read GetItem write SetItem; default;
    property Name: String read FName write SetName;
  end;

  TDBGBreakPointGroups = class(TCollection)
  private
    function GetItem(const AnIndex: Integer): TDBGBreakPointGroup;
    procedure SetItem(const AnIndex: Integer; const Value: TDBGBreakPointGroup);
  protected
  public
    property Items[const AnIndex: Integer]: TDBGBreakPointGroup 
                  read GetItem write SetItem; default;
  end;


implementation

{ TDBGBreakPoint }

procedure TDBGBreakPoint.AddDisableGroup(const AGroup: TDBGBreakPointGroup);
begin
end;

procedure TDBGBreakPoint.AddEnableGroup(const AGroup: TDBGBreakPointGroup);
begin
end;

procedure TDBGBreakPoint.RemoveDisableGroup(const AGroup: TDBGBreakPointGroup);
begin
end;

procedure TDBGBreakPoint.RemoveEnableGroup(const AGroup: TDBGBreakPointGroup);
begin
end;

procedure TDBGBreakPoint.SetActions(const AValue: TDBGBreakPointActions);
begin
  FActions := AValue;
end;

procedure TDBGBreakPoint.SetEnabled(const AValue: Boolean);
begin
  FEnabled := AValue;
end;

procedure TDBGBreakPoint.SetExpression(const AValue: String);
begin
  FExpression := AValue;
end;

procedure TDBGBreakPoint.SetHitCount(const AValue: Integer);
begin
  FHitCount := AValue;
end;

procedure TDBGBreakPoint.SetValid(const AValue: Boolean);
begin
  FValid := AValue;
end;

{ TDBGBreakPointGroup }

function TDBGBreakPointGroup.GetItem(const AnIndex: Integer): TDBGBreakPoint;
begin
  Result:=nil;
end;

procedure TDBGBreakPointGroup.SetEnabled(const AValue: Boolean);
begin
  FEnabled := AValue;
end;

procedure TDBGBreakPointGroup.SetItem(const AnIndex: Integer;
  const AValue: TDBGBreakPoint);
begin
end;

procedure TDBGBreakPointGroup.SetName(const AValue: String);
begin
  FName := AValue;
end;

{ TDBGBreakPointGroups }

function TDBGBreakPointGroups.GetItem(
  const AnIndex: Integer): TDBGBreakPointGroup;
begin
  Result:=nil;
end;

procedure TDBGBreakPointGroups.SetItem(const AnIndex: Integer; 
  const Value: TDBGBreakPointGroup);
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
