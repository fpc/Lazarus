{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    This unit defines various base classes for loading and saving of configs.
}
unit LazConfigStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;
  
type
  { TConfigStorage }

  TConfigStorage = class
  private
    FPathStack: TStrings;
    FCurrentBasePath: string;
  protected
    function  GetFullPathValue(const APath, ADefault: String): String; virtual; abstract;
    function  GetFullPathValue(const APath: String; ADefault: Integer): Integer; virtual; abstract;
    function  GetFullPathValue(const APath: String; ADefault: Boolean): Boolean; virtual; abstract;
    procedure SetFullPathValue(const APath, AValue: String); virtual; abstract;
    procedure SetDeleteFullPathValue(const APath, AValue, DefValue: String); virtual; abstract;
    procedure SetFullPathValue(const APath: String; AValue: Integer); virtual; abstract;
    procedure SetDeleteFullPathValue(const APath: String; AValue, DefValue: Integer); virtual; abstract;
    procedure SetFullPathValue(const APath: String; AValue: Boolean); virtual; abstract;
    procedure SetDeleteFullPathValue(const APath: String; AValue, DefValue: Boolean); virtual; abstract;
    procedure DeleteFullPath(const APath: string); virtual; abstract;
    procedure DeleteFullPathValue(const APath: string); virtual; abstract;
  public
    constructor Create(const Filename: string; LoadFromDisk: Boolean); virtual;
    destructor Destroy; override;
    function  GetValue(const APath, ADefault: String): String;
    function  GetValue(const APath: String; ADefault: Integer): Integer;
    function  GetValue(const APath: String; ADefault: Boolean): Boolean;
    procedure GetValue(const APath: String; out ARect: TRect;
                       const ADefault: TRect);
    procedure GetValue(const APath: String; out APoint: TPoint;
                       const ADefault: TPoint);
    procedure GetValue(const APath: String; const List: TStrings);
    procedure SetValue(const APath, AValue: String);
    procedure SetDeleteValue(const APath, AValue, DefValue: String);
    procedure SetValue(const APath: String; AValue: Integer);
    procedure SetDeleteValue(const APath: String; AValue, DefValue: Integer);
    procedure SetValue(const APath: String; AValue: Boolean);
    procedure SetDeleteValue(const APath: String; AValue, DefValue: Boolean);
    procedure SetValue(const APath: String; const AValue: TRect);
    procedure SetDeleteValue(const APath: String; const AValue, DefValue: TRect);
    procedure SetValue(const APath: String; const AValue: TPoint);
    procedure SetDeleteValue(const APath: String; const AValue, DefValue: TPoint);
    procedure SetValue(const APath: String; const AValue: TStrings);
    procedure DeletePath(const APath: string);
    procedure DeleteValue(const APath: string);
    property CurrentBasePath: string read FCurrentBasePath;
    function ExtendPath(const APath: string): string;
    procedure AppendBasePath(const Path: string);
    procedure UndoAppendBasePath;
    procedure WriteToDisk; virtual; abstract;
    function GetFilename: string; virtual; abstract;
  end;
  
  TConfigStorageClass = class of TConfigStorage;
  
implementation

{ TConfigStorage }

constructor TConfigStorage.Create(const Filename: string; LoadFromDisk: Boolean
  );
begin

end;

destructor TConfigStorage.Destroy;
begin
  FPathStack.Free;
  inherited Destroy;
end;

function TConfigStorage.GetValue(const APath, ADefault: String): String;
begin
  Result:=GetFullPathValue(ExtendPath(APath),ADefault);
end;

function TConfigStorage.GetValue(const APath: String; ADefault: Integer
  ): Integer;
begin
  Result:=GetFullPathValue(ExtendPath(APath),ADefault);
end;

function TConfigStorage.GetValue(const APath: String; ADefault: Boolean
  ): Boolean;
begin
  Result:=GetFullPathValue(ExtendPath(APath),ADefault);
end;

procedure TConfigStorage.GetValue(const APath: String; out ARect: TRect;
  const ADefault: TRect);
begin
  ARect.Left:=GetValue(APath+'Left',ADefault.Left);
  ARect.Top:=GetValue(APath+'Top',ADefault.Top);
  ARect.Right:=GetValue(APath+'Right',ADefault.Right);
  ARect.Bottom:=GetValue(APath+'Bottom',ADefault.Bottom);
end;

procedure TConfigStorage.GetValue(const APath: String; out APoint: TPoint;
  const ADefault: TPoint);
begin
  APoint.X:=GetValue(APath+'X',ADefault.X);
  APoint.Y:=GetValue(APath+'Y',ADefault.Y);
end;

procedure TConfigStorage.GetValue(const APath: String; const List: TStrings);
var
  NewCount: LongInt;
  i: Integer;
  NewLine: String;
begin
  NewCount:=GetValue(APath+'Count',0);
  for i:=0 to NewCount-1 do begin
    NewLine:=GetValue(APath+'Item'+IntToStr(i+1)+'/Value','');
    if List.Count>i then
      List[i]:=NewLine
    else
      List.Add(NewLine);
  end;
  while List.Count>NewCount do List.Delete(List.Count-1);
end;

procedure TConfigStorage.SetValue(const APath, AValue: String);
begin
  SetFullPathValue(ExtendPath(APath),AValue);
end;

procedure TConfigStorage.SetDeleteValue(const APath, AValue, DefValue: String);
begin
  SetDeleteFullPathValue(ExtendPath(APath),AValue,DefValue);
end;

procedure TConfigStorage.SetValue(const APath: String; AValue: Integer);
begin
  SetFullPathValue(ExtendPath(APath),AValue);
end;

procedure TConfigStorage.SetDeleteValue(const APath: String; AValue,
  DefValue: Integer);
begin
  SetDeleteFullPathValue(ExtendPath(APath),AValue,DefValue);
end;

procedure TConfigStorage.SetValue(const APath: String; AValue: Boolean);
begin
  SetFullPathValue(ExtendPath(APath),AValue);
end;

procedure TConfigStorage.SetDeleteValue(const APath: String; AValue,
  DefValue: Boolean);
begin
  SetDeleteFullPathValue(ExtendPath(APath),AValue,DefValue);
end;

procedure TConfigStorage.SetValue(const APath: String; const AValue: TRect);
begin
  SetValue(APath+'Left',AValue.Left);
  SetValue(APath+'Top',AValue.Top);
  SetValue(APath+'Right',AValue.Right);
  SetValue(APath+'Bottom',AValue.Bottom);
end;

procedure TConfigStorage.SetDeleteValue(const APath: String; const AValue,
  DefValue: TRect);
begin
  SetDeleteValue(APath+'Left',AValue.Left,DefValue.Left);
  SetDeleteValue(APath+'Top',AValue.Top,DefValue.Top);
  SetDeleteValue(APath+'Right',AValue.Right,DefValue.Right);
  SetDeleteValue(APath+'Bottom',AValue.Bottom,DefValue.Bottom);
end;

procedure TConfigStorage.SetValue(const APath: String; const AValue: TPoint);
begin
  SetValue(APath+'X',AValue.X);
  SetValue(APath+'Y',AValue.Y);
end;

procedure TConfigStorage.SetDeleteValue(const APath: String; const AValue,
  DefValue: TPoint);
begin
  SetDeleteValue(APath+'X',AValue.X,DefValue.X);
  SetDeleteValue(APath+'Y',AValue.Y,DefValue.Y);
end;

procedure TConfigStorage.SetValue(const APath: String; const AValue: TStrings);
var
  i: Integer;
begin
  SetDeleteValue(APath+'Count',AValue.Count,0);
  for i:=0 to AValue.Count-1 do
    SetDeleteValue(APath+'Item'+IntToStr(i+1)+'/Value',AValue[i],'');
end;

procedure TConfigStorage.DeletePath(const APath: string);
begin
  DeleteFullPath(ExtendPath(APath));
end;

procedure TConfigStorage.DeleteValue(const APath: string);
begin
  DeleteFullPathValue(ExtendPath(APath));
end;

function TConfigStorage.ExtendPath(const APath: string): string;
begin
  Result:=FCurrentBasePath+APath;
end;

procedure TConfigStorage.AppendBasePath(const Path: string);
begin
  if FPathStack=nil then FPathStack:=TStringList.Create;
  FPathStack.Add(FCurrentBasePath);
  FCurrentBasePath:=FCurrentBasePath+Path;
  if (FCurrentBasePath<>'')
  and (FCurrentBasePath[length(FCurrentBasePath)]<>'/') then
    FCurrentBasePath:=FCurrentBasePath+'/';
end;

procedure TConfigStorage.UndoAppendBasePath;
begin
  if (FPathStack=nil) or (FPathStack.Count=0) then
    raise Exception.Create('TConfigStorage.UndoAppendBasePath');
  FCurrentBasePath:=FPathStack[FPathStack.Count-1];
  FPathStack.Delete(FPathStack.Count-1);
end;

end.

