{ $Id$ }
{                        ---------------------------------------  
                         DBGBreakpoint.pp  -  Breakpoint classes
                         --------------------------------------- 
 
 @created(Wed Feb 25st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

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
  TDBGBreakPointClass = class of TDBGBreakPoint;
  TDBGBreakPoint = class(TCollectionItem)
  private
    FDebugger: TObject;  // reference to our debugger
    FValid: Boolean;
    FEnabled: Boolean;
    FHitCount: Integer;
    FExpression: String;                                             
    FSource: String; 
    FLine: Integer;
    FActions: TDBGBreakPointActions;
    procedure SetActions(const AValue: TDBGBreakPointActions); 
    procedure SetEnabled(const AValue: Boolean); 
    procedure SetExpression(const AValue: String); 
  protected                                        
    procedure SetHitCount(const AValue: Integer); 
    procedure SetLocation(const ASource: String; const ALine: Integer); virtual;
    procedure SetValid(const AValue: Boolean); 
  public
    procedure AddDisableGroup(const AGroup: TDBGBreakPointGroup);
    procedure AddEnableGroup(const AGroup: TDBGBreakPointGroup);
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure RemoveDisableGroup(const AGroup: TDBGBreakPointGroup);
    procedure RemoveEnableGroup(const AGroup: TDBGBreakPointGroup);
    property Actions: TDBGBreakPointActions read FActions write SetActions;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property HitCount: Integer read FHitCount;
    property Expression: String read FExpression write SetExpression;
    property Source: String read FSource; 
    property Line: Integer read FLine;
    property Valid: Boolean read FValid;
  end;

  TDBGBreakPoints = class(TCollection)
  private
    FDebugger: TObject;  // reference to our debugger
    function GetItem(const AnIndex: Integer): TDBGBreakPoint;
    procedure SetItem(const AnIndex: Integer; const AValue: TDBGBreakPoint);
  protected
  public 
    constructor Create(const ADebugger: TObject);
    function Add(const ASource: String; const ALine: Integer): TDBGBreakPoint;
    property Items[const AnIndex: Integer]: TDBGBreakPoint read GetItem write SetItem; default;
  end;

  TDBGBreakPointGroup = class(TCollectionItem)
  private
    FEnabled: Boolean;
    FName: String;
    FBreakpoints: TDBGBreakPoints;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetName(const AValue: String);
  protected
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property Breakpoints: TDBGBreakPoints read FBreakpoints;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Name: String read FName write SetName;
  end;

  TDBGBreakPointGroups = class(TCollection)
  private                     
    FDebugger: TObject;
    function GetItem(const AnIndex: Integer): TDBGBreakPointGroup;
    procedure SetItem(const AnIndex: Integer; const AValue: TDBGBreakPointGroup);
  protected
  public
    constructor Create(const ADebugger: TObject);
    property Items[const AnIndex: Integer]: TDBGBreakPointGroup read GetItem write SetItem; default;
  end;


implementation

uses
  Debugger;      
  
{ TDBGBreakPoint }

procedure TDBGBreakPoint.AddDisableGroup(const AGroup: TDBGBreakPointGroup);
begin
end;

procedure TDBGBreakPoint.AddEnableGroup(const AGroup: TDBGBreakPointGroup);
begin
end;

constructor TDBGBreakPoint.Create(ACollection: TCollection); 
begin
  inherited Create(ACollection);
  FSource := '';
  FLine := -1;
  FValid := False;
  FEnabled := False;
  FHitCount := 0;
  FExpression := '';
  
  FDebugger := TDBGBreakPoints(ACollection).FDebugger;
end;

destructor TDBGBreakPoint.Destroy; 
begin
  TInternalDebugger(FDebugger).BreakRemove(Self);
  inherited Destroy;
end;

procedure TDBGBreakPoint.RemoveDisableGroup(const AGroup: TDBGBreakPointGroup);
begin
end;

procedure TDBGBreakPoint.RemoveEnableGroup(const AGroup: TDBGBreakPointGroup);
begin
end;

procedure TDBGBreakPoint.SetActions(const AValue: TDBGBreakPointActions);
begin
  if FActions <> AValue
  then begin
    FActions := AValue;
    TInternalDebugger(FDebugger).BreakActionChange(Self);
  end;
end;

procedure TDBGBreakPoint.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue
  then begin
    FEnabled := AValue;
    TInternalDebugger(FDebugger).BreakEnableChange(Self);
  end;
end;

procedure TDBGBreakPoint.SetExpression(const AValue: String);
begin
  if FExpression <> AValue
  then begin
    FExpression := AValue;
    TInternalDebugger(FDebugger).BreakExpressionChange(Self);
  end;
end;

procedure TDBGBreakPoint.SetHitCount(const AValue: Integer);
begin
  FHitCount := AValue;
end;

procedure TDBGBreakPoint.SetLocation(const ASource: String; const ALine: Integer); 
begin
  FSource := ASource;
  FLine := ALine;
  TInternalDebugger(FDebugger).BreakAdd(Self);
end;

procedure TDBGBreakPoint.SetValid(const AValue: Boolean);
begin
  FValid := AValue;
end;

{ TDBGBreakPoints }

function TDBGBreakPoints.Add(const ASource: String; const ALine: Integer): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(inherited Add);
  Result.SetLocation(ASource, ALine);
end;

constructor TDBGBreakPoints.Create(const ADebugger: TObject);
begin
  inherited Create(TDBGBreakPoint);
  FDebugger := ADebugger;
end;

function TDBGBreakPoints.GetItem(const AnIndex: Integer): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(inherited GetItem(AnIndex));
end;

procedure TDBGBreakPoints.SetItem(const AnIndex: Integer; const AValue: TDBGBreakPoint);
begin  
  SetItem(AnIndex, AValue);   
end;

{ TDBGBreakPointGroup }

constructor TDBGBreakPointGroup.Create(ACollection: TCollection); 
begin
  inherited Create(ACollection);
  FBreakpoints := TDBGBreakPoints.Create(TDBGBreakPointGroups(ACollection).FDebugger);
end;

destructor TDBGBreakPointGroup.Destroy; 
begin         
  FBreakpoints.Free;
  inherited Destroy;
end;

procedure TDBGBreakPointGroup.SetEnabled(const AValue: Boolean);
begin
  FEnabled := AValue;
end;

procedure TDBGBreakPointGroup.SetName(const AValue: String);
begin
  FName := AValue;
end;

{ TDBGBreakPointGroups }

constructor TDBGBreakPointGroups.Create(const ADebugger: TObject); 
begin
  inherited Create(TDBGBreakPointGroup);
  FDebugger := ADebugger;
end;

function TDBGBreakPointGroups.GetItem(const AnIndex: Integer): TDBGBreakPointGroup;
begin
  Result := TDBGBreakPointGroup(inherited GetItem(AnIndex));
end;

procedure TDBGBreakPointGroups.SetItem(const AnIndex: Integer; const AValue: TDBGBreakPointGroup);
begin
  inherited SetItem(AnIndex, AValue);
end;

end.
{ =============================================================================
  $Log$
  Revision 1.5  2001/11/06 23:59:12  lazarus
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
