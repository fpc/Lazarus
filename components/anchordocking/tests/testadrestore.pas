unit TestADRestore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpcunit, testregistry,
  Controls, Forms, StdCtrls, ExtCtrls,
  AnchorDocking,
  SimpleFrm;

type

  { TMainBar }

  TMainBar = class(TCustomForm)
    BtnPanel: TPanel;
    NewBtn: TButton;
  public
  end;

  { TCustomTestADRestore }

  TCustomTestADRestore = class(TTestCase)
  private
    FMainBar: TMainBar;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure DockMasterCreateControl(Sender: TObject; aName: string; var
      AControl: TControl; DoDisableAutoSizing: boolean);
  public
    procedure CreateMainBar(DisabledAutoSize: boolean);
    property MainBar: TMainBar read FMainBar;
  end;

  { TTestADRestore }

  TTestADRestore = class(TCustomTestADRestore)
  published
    procedure TestNoDock;
  end;

implementation

{ TCustomTestADRestore }

procedure TCustomTestADRestore.SetUp;
begin
  DockMaster.OnCreateControl:=@DockMasterCreateControl;
  //DockMaster.OnShowOptions:=@ShowAnchorDockOptions;
end;

procedure TCustomTestADRestore.TearDown;
begin
  DockMaster.OnCreateControl:=nil;
  FreeAndNil(FMainBar);
end;

procedure TCustomTestADRestore.DockMasterCreateControl(Sender: TObject;
  aName: string; var AControl: TControl; DoDisableAutoSizing: boolean);

  procedure CreateForm(Caption: string; NewBounds: TRect);
  begin
    AControl:=CreateSimpleForm(aName,Caption,NewBounds,DoDisableAutoSizing);
  end;

begin
  if aName='CodeExplorer' then
    CreateForm('Code Explorer',Bounds(700,230,100,250))
  else if aName='FPDocEditor' then
    CreateForm('FPDoc Editor',Bounds(200,720,300,100))
  else if aName='Messages' then
    CreateForm('Messages',Bounds(230,650,350,100))
  else if aName='ObjectInspector' then
    CreateForm('Object Inspector',Bounds(10,200,100,350))
  else if aName='SourceEditor1' then
    CreateForm('Source Editor 1',Bounds(230,200,400,400))
  else if aName='SourceEditor2' then
    CreateForm('Source Editor 2',Bounds(260,230,350,350))
  else if aName='ProjectInspector' then
    CreateForm('Project Inspector',Bounds(10,230,150,250))
  else if aName='DebugOutput' then
    CreateForm('Debug Output',Bounds(400,400,350,150));
end;

procedure TCustomTestADRestore.CreateMainBar(DisabledAutoSize: boolean);
begin
  FMainBar:=TMainBar(TMainBar.NewInstance);
  FMainBar.DisableAutoSizing;
  try
    with FMainBar do begin
      Create(Application);
      Name:='MainBar';
      Caption:='Main Bar';
      SetBounds(10,11,600,100);
      Visible:=true;
    end;

    FMainBar.BtnPanel:=TPanel.Create(FMainBar);
    with FMainBar.BtnPanel do begin
      Parent:=FMainBar;
      Name:='BtnPanel';
      Caption:='';
      AutoSize:=true;
      Align:=alLeft;
    end;

    FMainBar.NewBtn:=TButton.Create(FMainBar);
    with FMainBar.NewBtn do begin
      Parent:=FMainBar.BtnPanel;
      Name:='NewBtn';
      AutoSize:=true;
    end;

    DockMaster.MakeDockSite(FMainBar,[akBottom],admrpChild);
  finally
    if not DisabledAutoSize then
      FMainBar.EnableAutoSizing;
  end;
end;

{ TTestADRestore }

procedure TTestADRestore.TestNoDock;
begin
  CreateMainBar(false);
end;

initialization
  RegisterTest(TCustomTestADRestore);
end.

