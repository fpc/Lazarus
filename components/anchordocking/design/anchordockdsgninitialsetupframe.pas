unit AnchorDockDsgnInitialSetupFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, LazarusPackageIntf, AnchorDockStr, IDEWindowIntf,
  AnchorDesktopOptions;

type

  TAnchorDockDsgnSetup = class;

  { TAnchorDockSetupFrame }

  TAnchorDockSetupFrame = class(TFrame)
    lbInfo: TLabel;
    lbInfo1: TLabel;
    lbMulti: TLabel;
    lbSingle: TLabel;
    rbMulti: TRadioButton;
    rbSingle: TRadioButton;
    procedure lbMultiClick(Sender: TObject);
    procedure lbSingleClick(Sender: TObject);
    procedure rbMultiChange(Sender: TObject);
  private
    FDialog: ISetupDlgProvider;
    FSetup: TAnchorDockDsgnSetup;
    FChanged: Boolean;
  public
    procedure Init;
  end;

  { TAnchorDockDsgnSetup }

  TAnchorDockDsgnSetup = class(TObject, ISetupDlgFrame)
  private
    FTheFrame: TAnchorDockSetupFrame;
    FInitDone: Boolean;
  public
    destructor Destroy; override;
    function  RequireSetup: boolean;
    procedure AddToDialog(AnOwner, AParent: TComponent;  // AParent: TWinControl that will hold the frame
                          ADialog: ISetupDlgProvider
                         );
    procedure Init;
    procedure Done;
    function  Caption: String;
    function  SortOrder: Integer;
    function  UniqueId: String;
    function  GroupId: String;
    procedure PageSelected(AnUserActivated: Boolean);
    procedure UpdateState;
    procedure ApplySelection;                   // Called when the IDE gets started
    function  Internal: TObject;
  end;

implementation

var
  Registration: TAnchorDockDsgnSetup;

{$R *.lfm}

{ TAnchorDockSetupFrame }

procedure TAnchorDockSetupFrame.lbMultiClick(Sender: TObject);
begin
  rbMulti.Checked := True;
end;

procedure TAnchorDockSetupFrame.lbSingleClick(Sender: TObject);
begin
  rbSingle.Checked := True;
end;

procedure TAnchorDockSetupFrame.rbMultiChange(Sender: TObject);
begin
  FChanged := True;
  FDialog.FrameStateChanged(FSetup, issOk, isaReady);
end;

procedure TAnchorDockSetupFrame.Init;
begin
  rbMulti.Caption := setupMultiWindowIDEClassic;
  lbMulti.Caption := setupMultiWindowIDESeperateWindows;
  rbSingle.Caption := setupMultiWindowIDEModern;
  lbSingle.Caption := setupMultiWindowIDESingleWindow;
  lbInfo.Caption := setupMultiWindowIDEInfo;
  lbInfo1.Caption := setupMultiWindowIDEOption;
end;

{ TAnchorDockDsgnSetup }

destructor TAnchorDockDsgnSetup.Destroy;
begin
  FreeAndNil(FTheFrame);
  inherited Destroy;
end;

function TAnchorDockDsgnSetup.RequireSetup: boolean;
begin
  if AnchorDockGlobalOptions = nil then exit(False);
  AnchorDockGlobalOptions.LoadSafe;
  Result := not AnchorDockGlobalOptions.DoneAskUserEnableAnchorDock;
end;

procedure TAnchorDockDsgnSetup.AddToDialog(AnOwner, AParent: TComponent; ADialog: ISetupDlgProvider
  );
begin
  FTheFrame := TAnchorDockSetupFrame.Create(AnOwner);
  FTheFrame.Parent := AParent as TWinControl;
  FTheFrame.Align := alClient;
  FTheFrame.FDialog := ADialog;
  FTheFrame.FSetup := Self;
  FTheFrame.Init;

  ADialog.SetGroupCaption(GroupId, SIDELayout);
end;

procedure TAnchorDockDsgnSetup.Init;
begin
  if AnchorDockGlobalOptions = nil then exit;
  AnchorDockGlobalOptions.LoadSafe;
end;

procedure TAnchorDockDsgnSetup.Done;
begin
  FTheFrame := nil;
end;

function TAnchorDockDsgnSetup.Caption: String;
begin
  Result := SSingleMultiWindow;
end;

function TAnchorDockDsgnSetup.SortOrder: Integer;
begin
  Result := -2;
end;

function TAnchorDockDsgnSetup.UniqueId: String;
begin
  Result := '{56782628-36B2-4082-9F9B-C4A1CABC7A98}';
end;

function TAnchorDockDsgnSetup.GroupId: String;
begin
  Result := '{62435B06-4C68-4D77-A4BC-2496E35DF59C}';
end;

procedure TAnchorDockDsgnSetup.PageSelected(AnUserActivated: Boolean);
begin
  if AnUserActivated then begin
    FTheFrame.FChanged := True;
    FTheFrame.FDialog.FrameStateChanged(Self, issOk, isaReady);
  end;
end;

procedure TAnchorDockDsgnSetup.UpdateState;
begin
  if not FInitDone then begin
    FTheFrame.rbSingle.Checked := AnchorDockGlobalOptions.EnableAnchorDock;
    FTheFrame.rbMulti.Checked := not AnchorDockGlobalOptions.EnableAnchorDock;
    FTheFrame.FChanged := AnchorDockGlobalOptions.DoneAskUserEnableAnchorDock;
  end;
  FInitDone := True;

  if (not FTheFrame.FChanged) then
    FTheFrame.FDialog.FrameStateChanged(Self, issInfo, isaReady)
  else
    FTheFrame.FDialog.FrameStateChanged(Self, issOk, isaReady);

end;

procedure TAnchorDockDsgnSetup.ApplySelection;
begin
  if AnchorDockGlobalOptions = nil then exit;
  AnchorDockGlobalOptions.DoneAskUserEnableAnchorDock := True;
  AnchorDockGlobalOptions.EnableAnchorDock := FTheFrame.rbSingle.Checked;
  AnchorDockGlobalOptions.SaveSafe;

  if not AnchorDockGlobalOptions.EnableAnchorDock then
    OnIDEDockMasterNeeded := nil;
end;

function TAnchorDockDsgnSetup.Internal: TObject;
begin
  Result := nil;
end;

initialization
  Registration := TAnchorDockDsgnSetup.Create;
  SetupDlgFrameList.Add(Registration);

finalization
  FreeAndNil(Registration);

end.

