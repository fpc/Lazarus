{ Copyright (C) 2003 Michael Van Canneyt

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit cgiModules;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, cgiApp, LCLProc, LResources;

Type
  TCGIDatamodule = Class(TDataModule)
  private
    FOnCGIRequest: TNotifyEvent;
    function GetContentType: String;
    procedure SetContentType(const AValue: String);
  Protected
    function GetResponse: TStream;
  Public
    procedure HandleRequest; virtual;
    procedure EmitContentType; virtual;
    procedure AddResponse(const Msg : String); virtual;
    procedure AddResponseLn(const Msg : String); virtual;
    procedure AddResponse(const Fmt : String; Args : Array of const); virtual;
    procedure AddResponseLn(const Fmt : String; Args : Array of const); virtual;
  Published
    property OnCGIRequest : TNotifyEvent Read FOnCGIRequest Write FOnCGIRequest;
    property Response : TStream Read GetResponse;
    property ContentType : String Read GetContentType Write SetContentType;
  end;

  TCGIDataModuleClass = Class of TCGIDatamodule;

  TModuledCGIApplication = Class(TCGIApplication)
  private
    FMainModule : TCGIDatamodule;
  public
    Procedure CreateForm(AClass: TCGIDataModuleClass;
                         Var Reference: TCGIDataModule);
    Procedure DoRun; Override;
    Property MainModule : TCGIDatamodule Read FMainModule;
  end;

  ECGIAppException = Class(Exception);

Var
  Application : TModuledCGIApplication;

function InitResourceComponent(Instance: TComponent;
  RootAncestor: TClass):Boolean;

implementation

ResourceString
  SErrNoMainModule = 'No CGI datamodule to handle CGI request.';
  SErrNoRequestHandler = '%s: No CGI request handler set.';

function InitResourceComponent(Instance: TComponent;
  RootAncestor: TClass): Boolean;
begin
  Result:=InitLazResourceComponent(Instance,RootAncestor);
end;

{ TModuledCGIApplication }

procedure TModuledCGIApplication.CreateForm(AClass: TCGIDataModuleClass;
  Var Reference: TCGIDataModule);
begin
  debugln('TModuledCGIApplication.CreateForm ',AClass.CLassName);
  Reference:=AClass.CreateNew(Self,0);
  If FMainModule=Nil then
    FMainModule:=Reference;
end;

procedure TModuledCGIApplication.DoRun;
begin
  if (FMainModule=Nil) then
    Raise ECGIAppException.Create(SErrNoMainModule);
  Try
    FMainModule.HandleRequest;
  Finally
    Terminate;
  end;
end;

{ TCGIDatamodule }

function TCGIDatamodule.GetContentType: String;
begin
  Result:=Application.ContentType;
end;

procedure TCGIDatamodule.SetContentType(const AValue: String);
begin
  Application.ContentType:=AValue;
end;

function TCGIDatamodule.GetResponse: TStream;
begin
  If Assigned(Application) then
    Result:=Application.Response;
end;

procedure TCGIDatamodule.HandleRequest;
begin
  If Not Assigned(FOnCGIRequest) then
    Raise ECGIAppException.CreateFmt(SErrNoRequestHandler,[Name]);
  FOnCGIRequest(Self)
end;

procedure TCGIDatamodule.EmitContentType;
begin
  Application.EmitContentType;
end;

procedure TCGIDatamodule.AddResponse(const Msg: String);
begin
  Application.AddResponse(Msg);
end;

procedure TCGIDatamodule.AddResponseLn(const Msg: String);
begin
  Application.AddResponseLn(Msg);
end;

procedure TCGIDatamodule.AddResponse(const Fmt: String;
  Args: array of const);
begin
  Application.AddResponse(Fmt,Args);
end;

procedure TCGIDatamodule.AddResponseLn(const Fmt: String;
  Args: array of const);
begin
  Application.AddResponseLn(Fmt,Args);
end;

Initialization
  Application:=TModuledCGIApplication.Create(Nil);
  RegisterInitComponentHandler(TComponent,@InitResourceComponent);

Finalization
  Application.Free;
end.

