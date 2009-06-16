{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the EducationLaz package                            *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Options.
}
unit EduOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazConfigStorage, Controls, Forms;

type

  { TEduOptionsNode }

  TEduOptionsNode = class
  private
    FChilds: TFPList; // list of TEduOptionsNode
    FName: string;
    FNextSibling: TEduOptionsNode;
    FParent: TEduOptionsNode;
    FPrevSibling: TEduOptionsNode;
    function GetChildCount: integer;
    function GetChilds(Index: integer): TEduOptionsNode;
    procedure SetName(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Delete(Index: integer); virtual;
    procedure Remove(Index: integer); virtual;
    function GetCaption: string; virtual;
    function Load(Config: TConfigStorage): TModalResult; virtual;
    function Save(Config: TConfigStorage): TModalResult; virtual;
  public
    property Name: string read FName write SetName;
    property Parent: TEduOptionsNode read FParent;
    property NextSibling: TEduOptionsNode read FNextSibling;
    property PrevSibling: TEduOptionsNode read FPrevSibling;
    property ChildCount: integer read GetChildCount;
    property Childs[Index: integer]: TEduOptionsNode read GetChilds; default;
  end;

implementation

{ TEduOptionsNode }

procedure TEduOptionsNode.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
end;

function TEduOptionsNode.GetChilds(Index: integer): TEduOptionsNode;
begin
  Result:=TEduOptionsNode(fChilds[Index]);
end;

function TEduOptionsNode.GetChildCount: integer;
begin
  Result:=fChilds.Count;
end;

constructor TEduOptionsNode.Create;
begin
  fChilds:=TFPList.Create;
end;

destructor TEduOptionsNode.Destroy;
begin
  Clear;
  FreeAndNil(fChilds);
  inherited Destroy;
end;

procedure TEduOptionsNode.Clear;
begin
  while ChildCount>0 do Delete(ChildCount-1);
end;

procedure TEduOptionsNode.Delete(Index: integer);
var
  Child: TEduOptionsNode;
begin
  Child:=Childs[Index];
  Remove(Index);
  Child.Free;
end;

procedure TEduOptionsNode.Remove(Index: integer);
var
  Child: TEduOptionsNode;
begin
  Child:=Childs[Index];
  fChilds.Delete(Index);
  Child.FParent:=nil;
  if Child.FPrevSibling<>nil then
    Child.FPrevSibling.FNextSibling:=FNextSibling;
  if Child.FNextSibling<>nil then
    Child.FNextSibling.FPrevSibling:=FPrevSibling;
  Child.FPrevSibling:=nil;
  Child.FNextSibling:=nil;
end;

function TEduOptionsNode.GetCaption: string;
begin
  Result:=Name;
end;

function TEduOptionsNode.Load(Config: TConfigStorage): TModalResult;
var
  i: Integer;
  Child: TEduOptionsNode;
begin
  for i:=0 to ChildCount-1 do begin
    Child:=Childs[i];
    if (Child.Name='') or (not IsValidIdent(Child.Name)) then continue;
    Config.AppendBasePath(Child.Name);
    Result:=Child.Load(Config);
    if Result<>mrOK then exit;
    Config.UndoAppendBasePath;
  end;
  Result:=mrOk;
end;

function TEduOptionsNode.Save(Config: TConfigStorage): TModalResult;
var
  i: Integer;
  Child: TEduOptionsNode;
begin
  for i:=0 to ChildCount-1 do begin
    Child:=Childs[i];
    if (Child.Name='') or (not IsValidIdent(Child.Name)) then continue;
    Config.AppendBasePath(Child.Name);
    Result:=Child.Save(Config);
    if Result<>mrOK then exit;
    Config.UndoAppendBasePath;
  end;
  Result:=mrOk;
end;

end.

