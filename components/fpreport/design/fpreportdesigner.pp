{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    IDE dialog control which calls the report designer.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FPReportDesigner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, forms, fpreport, frmfpreportdesignermain;

Const
  // Aliases for convenience
  AllReportDesignOptions = frmfpreportdesignermain.AllReportDesignOptions;
  rdoManageData      = frmfpreportdesignermain.rdoManageData;      // Allow user to manage report data
  rdoManageVariables = frmfpreportdesignermain.rdoManageVariables; // Allow user to manage report variables
  rdoAllowLoad       = frmfpreportdesignermain.rdoAllowLoad;       // Allow user to load new reports (open)
  rdoAllowSave       = frmfpreportdesignermain.rdoAllowSave;       // Allow user to save reports (open)
  rdoAllowProperties = frmfpreportdesignermain.rdoAllowProperties; // Allow user to save
  rdoAllowPageAdd    = frmfpreportdesignermain.rdoAllowPageAdd;    // Allow user to add pages
  rdoAllowNew        = frmfpreportdesignermain.rdoAllowNew;        // Allow user to start new report
  rdoAllowPreview    = frmfpreportdesignermain.rdoAllowPreview;    // Allow user to ask report preview
  rdoAllowBands      = frmfpreportdesignermain.rdoAllowBands;      // Allow user to add/remove bands
  rdoAllowFileDrop   = frmfpreportdesignermain.rdoAllowFileDrop;   // Allow files to be dropped on designer


type
  TFPReportDesignOption = frmfpreportdesignermain.TFPReportDesignOption;
  TFPReportDesignOptions = frmfpreportdesignermain.TFPReportDesignOptions;

  { TFPReportDesigner }

  TFPReportDesigner = class(TComponent)
  private
    FAutoAssignReportData: Boolean;
    FCaption: String;
    FModalWindow: Boolean;
    FOnCloseDesigner: TNotifyEvent;
    FOnShowDesigner: TNotifyEvent;
    FOptions: TFPReportDesignOptions;
    FReport: TFPCustomReport;
    procedure DoDesignerClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure DoSaveDesignJSON(Sender: TObject);
    procedure EndDesigning;
    procedure SetReport(AValue: TFPCustomReport);
    procedure StartDesigning;
  protected
    procedure ConfigDesigner(F: TFPReportDesignerForm); virtual;
    procedure AssignReportData; virtual;
    Procedure DoExecute; virtual;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Constructor Create(AOwner : TComponent); override;
    Procedure Execute;
  published
    Property Report : TFPCustomReport Read FReport Write SetReport;
    Property ModalWindow : Boolean Read FModalWindow Write FModalWindow;
    Property Options : TFPReportDesignOptions Read FOptions Write FOptions default AllReportDesignOptions;
    Property Caption : String Read FCaption Write FCaption;
    // Called before designer window is shown. Sender is the designer form
    Property OnShowDesigner : TNotifyEvent Read FOnShowDesigner Write FOnShowDesigner;
    // Called after window is done showing. Sender is the designer form. Not in case of non-modal.
    Property OnCloseDesigner : TNotifyEvent Read FOnCloseDesigner Write FOnCloseDesigner;
    // CollectData
    Property AutoAssignReportData : Boolean Read FAutoAssignReportData Write FAutoAssignReportData;
  end;


implementation

uses
  fpjsonreport;

Resourcestring
  SErrNoReport = 'No report assigned.';


{ TFPReportDesigner }

procedure TFPReportDesigner.SetReport(AValue: TFPCustomReport);
begin
  if FReport=AValue then Exit;
  if Assigned(FReport) then
    FReport.RemoveFreeNotification(Self);
  FReport:=AValue;
  if Assigned(FReport) then
    FReport.FreeNotification(Self);
end;

procedure TFPReportDesigner.DoSaveDesignJSON(Sender: TObject);

Var
  J : TFPJSONReport;

begin
  J:=report as TFPJSONReport;
  J.DesignTimeJSON.Clear;
  J.SavetoJSON(J.DesignTimeJSON);
  (Sender as TFPReportDesignerForm).ResetModified;
end;

procedure TFPReportDesigner.DoDesignerClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  EndDesigning;
  If Assigned(OnCloseDesigner) then
    OnCloseDesigner(Self);
end;

procedure TFPReportDesigner.ConfigDesigner(F : TFPReportDesignerForm);

Var
  J : TFPJSONReport;

begin
  F.Report:=TFPReport(FReport);
  F.DesignOptions:=Self.Options;
  // We need to save the design to the JSON
  If (Report is TFPJSONReport) and (csDesigning in Report.ComponentState) then
    begin
    J:=Report as TFPJSONReport;
    if Assigned(J.DesignTimeJSON) then
      J.LoadFromJSON(J.DesignTimeJSON);
    F.OnSaveReport:=@DoSaveDesignJSON;
    end;
  if Caption<>'' then
    F.Caption:=Caption;
end;

procedure TFPReportDesigner.AssignReportData;

Var
  O,C : TComponent;

begin
  O:=Owner;
  FReport.ReportData.Clear;
  While (O<>Nil) do
    begin
    For C in O do
      if C is TFPReportData then
        FReport.ReportData.AddReportData(C as TFPReportData);
    if (O is TCustomForm) or (O is TDataModule) or (O is TFrame) then
      O:=Nil
    else
      O:=O.Owner;
    end;
end;

procedure TFPReportDesigner.StartDesigning;

Var
  I : integer;

begin
  For I:=0 to Report.ReportData.Count-1 do
    Report.ReportData[i].Data.StartDesigning;
end;

procedure TFPReportDesigner.EndDesigning;

Var
  I : integer;

begin
  For I:=0 to Report.ReportData.Count-1 do
    Report.ReportData[i].Data.EndDesigning;
end;

procedure TFPReportDesigner.DoExecute;

Var
  F : TFPReportDesignerForm;
  isModal : Boolean;

begin
  If not Assigned(FReport) then
    Raise EReportError.Create(SErrNoReport);
  StartDesigning;
  F:=TFPReportDesignerForm.Create(Application);
  try
    if AutoAssignReportData then
      AssignReportData;
    ConfigDesigner(F);
    if Assigned(OnShowDesigner) then
      OnShowDesigner(F);
    F.AddHandlerClose(@DoDesignerClose,False);
    isModal:=ModalWindow;
    if not isModal then
      F.Show
    else
      F.ShowModal;
  finally
    if isModal then
      F.Free;
  end;
end;

procedure TFPReportDesigner.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then
    begin
    if AComponent=FReport then
      FReport:=nil;
    end;
end;

constructor TFPReportDesigner.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Options:=AllReportDesignOptions;
end;

procedure TFPReportDesigner.Execute;
begin
  DoExecute;
end;

end.
