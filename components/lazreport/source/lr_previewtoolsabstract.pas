{*****************************************}
{                                         }
{             LazReport                   }
{             LazReport export filter     }
{                                         }
{  Copyright (c) 2014 by Lagunov A.A.     }
{                                         }
{*****************************************}

unit lr_PreviewToolsAbstract;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_Class;

type
  TlrPreviewToolsAbstract = class;

  TlrPreviewToolsSetupEvent = procedure(Sender:TlrPreviewToolsAbstract; var StopExec: boolean) of object;

  { TlrPreviewToolsAbstract }

  TlrPreviewToolsAbstract = class(TComponent)
  private
    FOnSetup: TlrPreviewToolsSetupEvent;
    function GetActive: boolean;
    procedure SetActive(AValue: boolean);
  protected
    FCaption: string;
    FDoc: TfrReport;
    procedure DoSetupEvent(var ContinueExec: boolean);
    function ProcessSetup:boolean; virtual;
    function ProcessTool:boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(ADoc:TfrReport);
  published
    property Active:boolean read GetActive write SetActive;
    property Caption: string read FCaption write FCaption;
    property OnSetup:TlrPreviewToolsSetupEvent read FOnSetup write FOnSetup;
  end;

procedure RegisterLRExportFilter(AFilter:TlrPreviewToolsAbstract);
procedure UnRegisterLRExportFilter(AFilter:TlrPreviewToolsAbstract);
var
  lrExportFilters : TList = nil;

implementation

procedure RegisterLRExportFilter(AFilter: TlrPreviewToolsAbstract);
var
  i: Integer;
begin
  for i:=0 to lrExportFilters.Count - 1 do
    if TlrPreviewToolsAbstract(lrExportFilters[i]) = AFilter then
      exit;

  lrExportFilters.Add(AFilter);
end;

procedure UnRegisterLRExportFilter(AFilter: TlrPreviewToolsAbstract);
var
  i: Integer;
begin
  i:=lrExportFilters.IndexOf(AFilter);
  if i>=0 then
    lrExportFilters.Delete(i);
end;

{ TlrPreviewToolsAbstract }

procedure TlrPreviewToolsAbstract.SetActive(AValue: boolean);
begin
  if AValue then
    RegisterLRExportFilter(Self)
  else
    UnRegisterLRExportFilter(Self);
end;

function TlrPreviewToolsAbstract.GetActive: boolean;
begin
  Result:=lrExportFilters.IndexOf(Self)>-1;
end;

procedure TlrPreviewToolsAbstract.DoSetupEvent(var ContinueExec: boolean);
begin
  if Assigned(FOnSetup) then
    FOnSetup(Self, ContinueExec);
end;

function TlrPreviewToolsAbstract.ProcessSetup: boolean;
begin
  Result:=true;
  DoSetupEvent(Result);
end;

function TlrPreviewToolsAbstract.ProcessTool: boolean;
begin
  Result:=false;
end;

constructor TlrPreviewToolsAbstract.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RegisterLRExportFilter(Self);
  FCaption:=ClassName;
end;

destructor TlrPreviewToolsAbstract.Destroy;
begin
  UnRegisterLRExportFilter(Self);
  inherited Destroy;
end;

procedure TlrPreviewToolsAbstract.Execute(ADoc: TfrReport);
begin
  FDoc:=ADoc;
  try
    if ProcessSetup then
       ProcessTool;
  finally
    FDoc:=nil;
  end;
end;


procedure DoFreeExportFilters;
begin
  FreeAndNil(lrExportFilters);
end;

initialization
  lrExportFilters :=TList.Create;
finalization
  DoFreeExportFilters;
end.

