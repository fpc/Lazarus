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
    FGroup: TDBGBreakPointGroup;
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
    procedure SetGroup(const AValue: TDBGBreakPointGroup);
  protected                                        
    procedure AssignTo(Dest: TPersistent); override;
    procedure DoActionChange; virtual;
    procedure DoEnableChange; virtual;
    procedure DoExpressionChange; virtual;
    procedure DoStateChange; virtual;
    procedure SetHitCount(const AValue: Integer); 
    procedure SetLocation(const ASource: String; const ALine: Integer); virtual;
    procedure SetValid(const AValue: Boolean); 
    property  Debugger: TObject read FDebugger;
  public
    procedure AddDisableGroup(const AGroup: TDBGBreakPointGroup);
    procedure AddEnableGroup(const AGroup: TDBGBreakPointGroup);
    constructor Create(ACollection: TCollection); override;
    procedure RemoveDisableGroup(const AGroup: TDBGBreakPointGroup);
    procedure RemoveEnableGroup(const AGroup: TDBGBreakPointGroup);
    property Actions: TDBGBreakPointActions read FActions write SetActions;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Group: TDBGBreakPointGroup read FGroup write SetGroup;
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
    procedure DoStateChange;
  public 
    constructor Create(const ADebugger: TObject; const ABreakPointClass: TDBGBreakPointClass);
    function Add(const ASource: String; const ALine: Integer): TDBGBreakPoint;
    function Find(const ASource: String; const ALine: Integer): TDBGBreakPoint;
    property Items[const AnIndex: Integer]: TDBGBreakPoint read GetItem write SetItem; default;
  end;
  
  TDBGBreakPointGroup = class(TCollectionItem)
  private
    FEnabled: Boolean;
    FName: String;
    FBreakpoints: TList;
    function GetBreakpoint(const AIndex: Integer): TDBGBreakPoint;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetName(const AValue: String);
  protected
  public
    function Add(const ABreakPoint: TDBGBreakPoint): Integer;
    function Count: Integer;
    constructor Create(ACollection: TCollection); override;
    procedure Delete(const AIndex: Integer);
    destructor Destroy; override;
    function Remove(const ABreakPoint: TDBGBreakPoint): Integer;
    property Breakpoints[const AIndex: Integer]: TDBGBreakPoint read GetBreakpoint;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Name: String read FName write SetName;
  end;

  TDBGBreakPointGroups = class(TCollection)
  private                     
    function GetItem(const AnIndex: Integer): TDBGBreakPointGroup;
    procedure SetItem(const AnIndex: Integer; const AValue: TDBGBreakPointGroup);
  protected
  public
    constructor Create;
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

procedure TDBGBreakPoint.AssignTo(Dest: TPersistent);
begin
  if Dest is TDBGBreakPoint 
  then begin
    TDBGBreakPoint(Dest).SetLocation(FSource, FLine);
    TDBGBreakPoint(Dest).SetExpression(FExpression);
    TDBGBreakPoint(Dest).SetActions(FActions);
    TDBGBreakPoint(Dest).SetEnabled(FEnabled);
  end
  else inherited; 
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
  FGroup := nil;
  FDebugger := TDBGBreakPoints(ACollection).FDebugger;
end;

procedure TDBGBreakPoint.DoActionChange; 
begin
end;

procedure TDBGBreakPoint.DoEnableChange; 
begin
end;

procedure TDBGBreakPoint.DoExpressionChange; 
begin
end;

procedure TDBGBreakPoint.DoStateChange; 
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
  if FActions <> AValue
  then begin
    FActions := AValue;
    DoActionChange;
  end;
end;

procedure TDBGBreakPoint.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue
  then begin
    FEnabled := AValue;
    DoEnableChange;
  end;
end;

procedure TDBGBreakPoint.SetExpression(const AValue: String);
begin
  if FExpression <> AValue
  then begin
    FExpression := AValue;
    DoExpressionChange;
  end;
end;

procedure TDBGBreakPoint.SetGroup(const AValue: TDBGBreakPointGroup);
var
  Grp: TDBGBreakPointGroup;
begin
  if FGroup <> AValue
  then begin
    
    if FGroup <> nil 
    then begin
      Grp := FGroup;
      FGroup := nil;  //  avoid second entrance
      Grp.Remove(Self);
    end;
    FGroup := AValue;
    if FGroup <> nil 
    then begin
      FGroup.Add(Self);
    end;
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

constructor TDBGBreakPoints.Create(const ADebugger: TObject; const ABreakPointClass: TDBGBreakPointClass);
begin
  inherited Create(ABreakPointClass);
  FDebugger := ADebugger;
end;

procedure TDBGBreakPoints.DoStateChange;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
    GetItem(n).DoStateChange;
end;

function TDBGBreakPoints.Find(const ASource: String; const ALine: Integer): TDBGBreakPoint;
var
  n: Integer;
begin
  for n := 0 to Count - 1 do
  begin
    Result := GetItem(n);
    if  (Result.Line = ALine)
    and (Result.Source = ASource)
    then Exit;
  end;    
  Result := nil;
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

function TDBGBreakPointGroup.Add(const ABreakPoint: TDBGBreakPoint): Integer;
begin
  Result := FBreakpoints.IndexOf(ABreakPoint); //avoid dups
  if Result = -1
  then begin
    Result := FBreakpoints.Add(ABreakPoint);
    ABreakpoint.Group := Self;
  end;
end;

function TDBGBreakPointGroup.Count: Integer;
begin
  Result := FBreakpoints.Count;
end;

constructor TDBGBreakPointGroup.Create(ACollection: TCollection); 
begin
  inherited Create(ACollection);
  FBreakpoints := TList.Create;
  FEnabled := True;
end;

procedure TDBGBreakPointGroup.Delete(const AIndex: Integer);
begin
  Remove(TDBGBreakPoint(FBreakPoints[AIndex]));
end;

destructor TDBGBreakPointGroup.Destroy; 
begin         
  FBreakpoints.Free;
  inherited Destroy;
end;

function TDBGBreakPointGroup.GetBreakpoint(const AIndex: Integer): TDBGBreakPoint;
begin
  Result := TDBGBreakPoint(FBreakPoints[AIndex]);
end;

function TDBGBreakPointGroup.Remove(const ABreakPoint: TDBGBreakPoint): Integer;
begin
  Result := FBreakpoints.Remove(ABreakPoint);
  if ABreakpoint.Group = Self
  then ABreakpoint.Group := nil;
end;

procedure TDBGBreakPointGroup.SetEnabled(const AValue: Boolean);
var
  n: Integer;
begin
  if FEnabled <> AValue
  then begin
    FEnabled := AValue;
    for n := 0 to FBreakPoints.Count - 1 do
      TDBGBreakpoint(FBreakPoints[n]).Enabled := FEnabled;
  end;
end;

procedure TDBGBreakPointGroup.SetName(const AValue: String);
begin
  FName := AValue;
end;

{ TDBGBreakPointGroups }

constructor TDBGBreakPointGroups.Create; 
begin
  inherited Create(TDBGBreakPointGroup);
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
  Revision 1.6  2002/02/05 23:16:48  lazarus
  MWE: * Updated tebugger
       + Added debugger to IDE

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
