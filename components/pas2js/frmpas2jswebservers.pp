unit frmpas2jswebservers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Buttons, pjscontroller;

type

  { TPasJSWebserverProcessesForm }

  TPasJSWebserverProcessesForm = class(TForm)
    ILProcesses: TImageList;
    LCount: TLabel;
    LVProcesses: TListView;
    SBrefresh: TSpeedButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SBrefreshClick(Sender: TObject);
  private
    Class Var
      TheForm : TPasJSWebserverProcessesForm;
    procedure DoControllerRefresh(Sender: TObject);
    procedure Localize;
    procedure ServerInstanceToListItem(LI: TListItem; SI: TServerInstance);
  public
    Class Function Instance : TPasJSWebserverProcessesForm;
    Procedure RefreshList;
  end;


implementation

uses strpas2jsdesign;

{$R *.lfm}

Const
  iiStopped = 0;
  iiRunning = 1;

{ TPasJSWebserverProcessesForm }

procedure TPasJSWebserverProcessesForm.FormShow(Sender: TObject);
begin
  TPJSController.Instance.OnRefresh:=@DoControllerRefresh;
  RefreshList;
end;

procedure TPasJSWebserverProcessesForm.SBrefreshClick(Sender: TObject);
begin
  RefreshList;
end;

procedure TPasJSWebserverProcessesForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
end;

procedure TPasJSWebserverProcessesForm.FormCreate(Sender: TObject);
begin
  Localize;
end;

procedure TPasJSWebserverProcessesForm.Localize;

begin
  LCount.Caption:=Format(SWebserversCount, ['0']);
  Caption:=SWebserversCaption;
  With LVProcesses do
    begin
    Column[0].Caption:=SWebserversPort;
    Column[1].Caption:=SWebserversStatus;
    Column[2].Caption:=SWebserversBaseDir;
    Column[3].Caption:=SWebserversProject;
    Column[4].Caption:=SWebserversExtra;
    end;
end;

procedure TPasJSWebserverProcessesForm.FormDestroy(Sender: TObject);
begin
  TPJSController.Instance.OnRefresh:=Nil;
  if (Self=TheForm) then
    TheForm:=Nil;
end;

procedure TPasJSWebserverProcessesForm.ServerInstanceToListItem(LI : TListItem;SI : TServerInstance);

Var
  S,SError : String;

begin
  LI.Caption:=IntToStr(SI.Port);
  If SI.Running then
    begin
    LI.ImageIndex:=iiRunning;
    S:=SStatusRunning;
    end
  else if (SI.RunError<>'') then
    begin
    S:=SStatusError;
    SError:=SI.RunError;
    end
  else
    begin
    LI.ImageIndex:=iiStopped;
    S:=SStatusStopped;
    end;
  LI.SubItems.Add(S);
  LI.SubItems.Add(SI.BaseDir);
  LI.SubItems.Add(SI.LastProject);
  LI.SubItems.Add(SError);
  LI.Data:=SI;
end;

procedure TPasJSWebserverProcessesForm.DoControllerRefresh(Sender: TObject);
begin
  RefreshList;
end;

class function TPasJSWebserverProcessesForm.Instance: TPasJSWebserverProcessesForm;
begin
  if TheForm=Nil then
    TheForm:=TPasJSWebserverProcessesForm.Create(Application);
  Result:=TheForm;
end;

procedure TPasJSWebserverProcessesForm.RefreshList;

Var
  C : TPJSController;
  I : integer;
  LI : TListItem;
  SI : TServerInstance;

begin
  C:=TPJSController.Instance;
  if (C=Nil) or (C.ServerInstances=Nil) or (C.ServerInstances.Count=0) then
    begin
    LVProcesses.Items.Clear;
    LCount.Caption:=Format(SWebserversCount, ['0']);
    exit;
    end;
  LCount.Caption:=Format(SWebserversCount, [IntToStr(C.ServerInstances.Count)]);
  With LVProcesses.Items do
    try
      BeginUpdate;
      Clear;
      For I:=0 to C.ServerInstances.Count-1 do
        begin
        SI:=C.ServerInstances[i];
        LI:=Add;
        ServerInstanceToListItem(LI,SI);
        end;
    finally
      EndUpdate;
    end;
end;

end.

