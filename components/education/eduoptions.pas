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
  Classes, SysUtils, LazConfigStorage, Controls, Forms, BaseIDEIntf, FileUtil,
  LazIDEIntf, IDEOptionsIntf;

const
  DefaultEduOptionsFilename = 'education.xml';

  EduOptionID = 2000;
    EduOptionGeneralID  = 100;
    EduOptionPackagesID = 200;

type

  { TEduOptionsNode }

  TEduOptionsNode = class(TPersistent)
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
    procedure Add(Node: TEduOptionsNode);
    procedure Insert(Index: integer; Node: TEduOptionsNode);
    procedure Unbind;
    function Load(Config: TConfigStorage): TModalResult; virtual;
    function Save(Config: TConfigStorage): TModalResult; virtual;
    procedure Changed; virtual;
  public
    property Name: string read FName write SetName;
    property Parent: TEduOptionsNode read FParent;
    property NextSibling: TEduOptionsNode read FNextSibling;
    property PrevSibling: TEduOptionsNode read FPrevSibling;
    property ChildCount: integer read GetChildCount;
    property Childs[Index: integer]: TEduOptionsNode read GetChilds; default;
  end;

  { TEduOptsRootNode }

  TEduOptsRootNode = class(TEduOptionsNode)
  private
    FChangeStep: integer;
    procedure SetChangeStep(const AValue: integer);
  public
    procedure Changed; override;
    procedure IncreaseChangeStep;
    property ChangeStep: integer read FChangeStep write SetChangeStep;
  end;

  TEduOptions = class(TAbstractIDEOptions)
  private
    FFilename: string;
    FRoot: TEduOptionsNode;
    FLastSavedChangeStep: integer;
    procedure SetFilename(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Root: TEduOptionsNode read FRoot;
    function Load(Config: TConfigStorage): TModalResult; virtual;
    function Save(Config: TConfigStorage): TModalResult; virtual;
    function LoadFromFile(Filename: string): TModalResult; virtual;
    function SaveToFile(Filename: string): TModalResult; virtual;
    function Load: TModalResult; virtual;
    function Save: TModalResult; virtual;
    function GetFullFilename: string;
    property Filename: string read FFilename write SetFilename;
  end;

var
  EducationOptions: TEduOptions = nil;

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
  Child.Unbind;
end;

procedure TEduOptionsNode.Add(Node: TEduOptionsNode);
begin
  Insert(ChildCount,Node);
end;

procedure TEduOptionsNode.Insert(Index: integer; Node: TEduOptionsNode);
begin
  Node.Unbind;
  FChilds.Insert(Index,Node);
  Node.FParent:=Self;
  if Index>0 then begin
    Node.FPrevSibling:=Childs[Index-1];
    Node.FPrevSibling.FNextSibling:=Node;
  end;
  if Index+1<ChildCount then begin
    Node.FNextSibling:=Childs[Index+1];
    Node.FNextSibling.FPrevSibling:=Node;
  end;
end;

procedure TEduOptionsNode.Unbind;
begin
  if FParent<>nil then
    FParent.fChilds.Remove(Self);
  FParent:=nil;
  if FPrevSibling<>nil then
    FPrevSibling.FNextSibling:=FNextSibling;
  if FNextSibling<>nil then
    FNextSibling.FPrevSibling:=FPrevSibling;
  FPrevSibling:=nil;
  FNextSibling:=nil;
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

procedure TEduOptionsNode.Changed;
begin
  if FParent<>nil then FParent.Changed;
end;

{ TEduOptions }

procedure TEduOptions.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
end;

constructor TEduOptions.Create;
begin
  FRoot:=TEduOptsRootNode.Create;
  FFilename:=DefaultEduOptionsFilename;
end;

destructor TEduOptions.Destroy;
begin
  FreeAndNil(FRoot);
  inherited Destroy;
end;

function TEduOptions.Load(Config: TConfigStorage): TModalResult;
begin
  Result:=FRoot.Load(Config);
end;

function TEduOptions.Save(Config: TConfigStorage): TModalResult;
begin
  Result:=FRoot.Save(Config);
end;

function TEduOptions.LoadFromFile(Filename: string): TModalResult;
var
  Config: TConfigStorage;
begin
  Config:=GetIDEConfigStorage(Filename,true);
  try
    Result:=Load(Config);
  finally
    Config.Free;
  end;
end;

function TEduOptions.SaveToFile(Filename: string): TModalResult;
var
  Config: TConfigStorage;
begin
  Config:=GetIDEConfigStorage(Filename,false);
  try
    Result:=Save(Config);
  finally
    Config.Free;
  end;
end;

function TEduOptions.Load: TModalResult;
begin
  Result:=LoadFromFile(GetFullFilename);
  FLastSavedChangeStep:=TEduOptsRootNode(Root).ChangeStep;
end;

function TEduOptions.Save: TModalResult;
var
  FullFilename: String;
begin
  FullFilename:=GetFullFilename;
  if FileExistsUTF8(FullFilename)
  and (FLastSavedChangeStep=TEduOptsRootNode(Root).ChangeStep) then
    Result:=mrOK;
  Result:=SaveToFile(Filename);
  FLastSavedChangeStep:=TEduOptsRootNode(Root).ChangeStep;
end;

function TEduOptions.GetFullFilename: string;
begin
  Result:=Filename;
  if FilenameIsAbsolute(Result) then exit;
  Result:=AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+Result;
end;

{ TEduOptsRootNode }

procedure TEduOptsRootNode.SetChangeStep(const AValue: integer);
begin
  if FChangeStep=AValue then exit;
  FChangeStep:=AValue;
end;

procedure TEduOptsRootNode.Changed;
begin
  inherited Changed;
  IncreaseChangeStep;
end;

procedure TEduOptsRootNode.IncreaseChangeStep;
begin
  if FChangeStep=High(FChangeStep) then
    FChangeStep:=low(FChangeStep)
  else
    inc(FChangeStep);
end;

initialization
  EducationOptions:=TEduOptions.Create;

finalization
  FreeAndNil(EducationOptions);

end.

