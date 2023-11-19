unit testinsightcontroller;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Const
  DefaultPort = 6789;
  DefaultAutoFetchTests = True;
  DefaultBasePath = '/tests';
  TestInsightConfig = 'TestInsight.xml';


Type

  { TTestInsightOptions }

  TTestInsightOptions = class(TPersistent)
  private
    FAutoFetchTests: Boolean;
    FBasePath: String;
    FPort: Word;
  Public
    Constructor Create;
    procedure Reset; virtual;
    procedure LoadFromFile(FileName : String);
    procedure SaveToFile(FileName: String);
    Property Port : Word Read FPort Write FPort;
    property BasePath : String Read FBasePath Write FBasePath;
    Property AutoFetchTests : Boolean Read FAutoFetchTests Write FautoFetchTests;
  end;

  { TTestInsightController }

  TTestInsightController = class(TObject)
  private
    class var _instance : TTestInsightController;
    Class constructor Init;
    Class destructor Done;
  private
    FOptions: TTestInsightOptions;
  public
  Public
    Constructor Create;
    Destructor Destroy; override;
    Class Property Instance : TTestInsightController Read _instance;
    Property Options : TTestInsightOptions Read FOptions;
  end;

Function gTestInsightController : TTestInsightController;

implementation

Uses BaseIDEIntf, LazConfigStorage;

Const
  KeyPort = 'Port/Value';
  KeyAutoFetchTests = 'AutoFetchTests/Value';
  KeyBasePath = 'BasePath/Value';

function gTestInsightController: TTestInsightController;
begin
  Result:=TTestInsightController.Instance
end;

{ TTestInsightOptions }

procedure TTestInsightOptions.SaveToFile(FileName: String);
var
  Cfg: TConfigStorage;
begin
  Cfg:=GetIDEConfigStorage(Filename,False);
  try
    Cfg.SetDeleteValue(KeyPort,Port,DefaultPort);
    Cfg.SetDeleteValue(KeyBasePath,BasePath,DefaultBasePath);
    Cfg.SetDeleteValue(KeyAutoFetchTests,AutoFetchTests,DefaultAutoFetchTests);
    Cfg.WriteToDisk;

  finally
    Cfg.Free;
  end;
end;

constructor TTestInsightOptions.Create;
begin
  Reset;
end;

procedure TTestInsightOptions.Reset;
begin
  Port:=DefaultPort;
  BasePath:=DefaultBasePath;
  AutoFetchTests:=DefaultAutoFetchTests;
end;

procedure TTestInsightOptions.LoadFromFile(FileName: String);
var
  Cfg: TConfigStorage;
begin
  Cfg:=GetIDEConfigStorage(Filename,True);
  try
    Port:=Cfg.GetValue(KeyPort,DefaultPort);
    BasePath:=Cfg.GetValue(KeyBasePath,DefaultBasePath);
    AutoFetchTests:=Cfg.GetValue(KeyAutoFetchTests,DefaultAutoFetchTests);
  finally
    Cfg.Free;
  end;
end;

{ TTestInsightController }

class constructor TTestInsightController.Init;
begin
  _Instance:=TTestInsightController.Create;
end;

class destructor TTestInsightController.Done;
begin
  FreeAndNil(_Instance)
end;

constructor TTestInsightController.Create;
begin
  FOptions:=TTestInsightOptions.Create;
end;

destructor TTestInsightController.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

end.

