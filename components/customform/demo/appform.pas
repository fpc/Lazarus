unit AppForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;
  
Type
  TInitFormAt = (ifaShow,ifaCreate,ifaActivate);
  
  { TAppForm }

  TAppForm = Class(TForm)
  private
    FAfterInit: TNotifyEvent;
    FBeforeInit: TNotifyEvent;
    FInitAt: TInitFormAt;
    Procedure InitForm;
  Protected
    Procedure DoInitForm; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    Procedure DoShow; override;
    Procedure Activate; override;
  Published
    // New properties
    Property InitAt : TInitFormAt Read FInitAt Write FinitAt default ifaShow;
    Property BeforeInitForm : TNotifyEvent Read FBeforeInit Write FBeforeInit;
    Property AfterInitForm : TNotifyEvent Read FAfterInit Write FAfterInit;
    // TCustomForm properties that we allow to edit in the IDE.
    property Caption;
    property ActiveControl;
    property BorderStyle;
    property Color;
    property FormStyle;
    property OnClose;
    property OnCloseQuery;
    property OnCreate;
    property OnDeactivate;
    property OnDestroy;
    property OnHide;
    property OnShow;
    property ParentFont;
    property PixelsPerInch;
    property PopupMenu;
  end;


implementation

uses custforms;

{ TAppForm }

procedure TAppForm.InitForm;
begin
  If Assigned(BeforeInitForm) then
    BeforeInitForm(Self);
  DoInitForm;
  If Assigned(AfterInitForm) then
    AfterInitForm(Self);
end;

procedure TAppForm.DoInitForm;
begin
  // Do nothing yet.
end;

constructor TAppForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if (InitAt=ifaCreate) then
    InitForm;
end;

procedure TAppForm.DoShow;
begin
  If InitAt=ifaShow then
    InitForm;
  inherited DoShow;
end;

procedure TAppForm.Activate;
begin
  if (InitAt=ifaActivate) then
    InitForm;
  inherited Activate;
end;

end.

