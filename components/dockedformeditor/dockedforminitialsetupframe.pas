unit DockedFormInitialSetupFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, LazarusPackageIntf,
  DockedStrConsts, DockedOptionsIDE;

type

  TDockedFormEditSetup = class;

  { TDockedFormEditSetupFrame }

  TDockedFormEditSetupFrame = class(TFrame)
    lbInfo: TLabel;
    lbInfo1: TLabel;
    lbFloat: TLabel;
    lbDocked: TLabel;
    rbFloat: TRadioButton;
    rbDocked: TRadioButton;
    procedure lbDockedClick(Sender: TObject);
    procedure lbFloatClick(Sender: TObject);
    procedure rbDockedChange(Sender: TObject);
  private
    FDialog: ISetupDlgProvider;
    FSetup: TDockedFormEditSetup;
    FChanged: Boolean;
  public
    procedure Init;
  end;

  TDockedFormEditSetup = class(TObject, ISetupDlgFrame)
  private
    FTheFrame: TDockedFormEditSetupFrame;
    FInitDone: boolean;

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
  Registration: TDockedFormEditSetup;

{$R *.lfm}

{ TDockedFormEditSetupFrame }

procedure TDockedFormEditSetupFrame.lbFloatClick(Sender: TObject);
begin
  rbFloat.Checked := True;
end;

procedure TDockedFormEditSetupFrame.rbDockedChange(Sender: TObject);
begin
  FChanged := True;
  FDialog.FrameStateChanged(FSetup, issOk, isaReady);
end;

procedure TDockedFormEditSetupFrame.lbDockedClick(Sender: TObject);
begin
  rbDocked.Checked:= True;
end;

procedure TDockedFormEditSetupFrame.Init;
begin
  rbFloat.Caption := setupDesignerClassic;
  lbFloat.Caption := setupDesignerFloat;
  rbDocked.Caption := setupDesignerModern;
  lbDocked.Caption := setupDesignerDocked;
  lbInfo.Caption := setupDesignerInfo;
  lbInfo1.Caption := setupMultiWindowIDEOption;
end;

{ TDockedFormEditSetup }

destructor TDockedFormEditSetup.Destroy;
begin
  FreeAndNil(FTheFrame);
  inherited Destroy;
end;

function TDockedFormEditSetup.RequireSetup: boolean;
begin
  if DockedOptions = nil then exit(False);
  DockedOptions.LoadSafe;
  Result := not DockedOptions.DoneAskUserEnableDockedDesigner;
end;

procedure TDockedFormEditSetup.AddToDialog(AnOwner, AParent: TComponent; ADialog: ISetupDlgProvider);
begin
  FTheFrame := TDockedFormEditSetupFrame.Create(AnOwner);
  FTheFrame.Parent := AParent as TWinControl;
  FTheFrame.Align := alClient;
  FTheFrame.FDialog := ADialog;
  FTheFrame.FSetup := Self;
  FTheFrame.Init;

  ADialog.SetGroupCaption(GroupId, SIDELayout);
end;

procedure TDockedFormEditSetup.Init;
begin
  if DockedOptions = nil then exit;
  DockedOptions.LoadSafe;
end;

procedure TDockedFormEditSetup.Done;
begin
  FTheFrame := nil; // destroyed by parent
end;

function TDockedFormEditSetup.Caption: String;
begin
  Result := SFormEditor;
end;

function TDockedFormEditSetup.SortOrder: Integer;
begin
  Result := -1;
end;

procedure TDockedFormEditSetup.UpdateState;
begin
  if not FInitDone then begin
    FTheFrame.rbDocked.Checked := DockedOptions.EnableDockedDesigner;
    FTheFrame.rbFloat.Checked := not DockedOptions.EnableDockedDesigner;
    FTheFrame.FChanged := DockedOptions.DoneAskUserEnableDockedDesigner;
  end;
  FInitDone := True;

  if (not FTheFrame.FChanged) then
    FTheFrame.FDialog.FrameStateChanged(Self, issInfo, isaReady)
  else
    FTheFrame.FDialog.FrameStateChanged(Self, issOk, isaReady);
end;

procedure TDockedFormEditSetup.ApplySelection;
begin
  if DockedOptions = nil then exit;
  DockedOptions.DoneAskUserEnableDockedDesigner := True;
  DockedOptions.EnableDockedDesigner := FTheFrame.rbDocked.Checked;
  DockedOptions.SaveSafe;
end;

function TDockedFormEditSetup.Internal: TObject;
begin
  Result := nil;
end;

function TDockedFormEditSetup.UniqueId: String;
begin
  Result := '{234D6036-1AD5-4A9D-91FD-AE783B2E3A5D}';
end;

function TDockedFormEditSetup.GroupId: String;
begin
  Result := '{62435B06-4C68-4D77-A4BC-2496E35DF59C}';
end;

procedure TDockedFormEditSetup.PageSelected(AnUserActivated: Boolean);
begin
  if AnUserActivated then begin
    FTheFrame.FChanged := True;
    FTheFrame.FDialog.FrameStateChanged(Self, issOk, isaReady);
  end;
end;

initialization
  Registration := TDockedFormEditSetup.Create;
  SetupDlgFrameList.Add(Registration);

finalization
  FreeAndNil(Registration);

end.

