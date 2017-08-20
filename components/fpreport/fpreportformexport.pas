{
    This file is part of the Free Component Library.
    Copyright (c) 2016 Michael Van Canneyt, member of the Free Pascal development team

    FPImage form export - render fpReport to a form.
    Simple version, no design time

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpreportformexport;

{$mode objfpc}{$H+}

{$DEFINE USEPAINTBOX}

interface

uses
  Classes, SysUtils, fpreport, forms, comctrls, extctrls, fpreportlclexport, controls, actnlist;

Type

  { TCustomFPreportPreviewForm }

  TCustomFPreportPreviewForm = Class(TForm)
  private
    FReport: TFPCustomReport;
    FReportPages: TFPList;
  Protected
    function GetEnableHyperLinks: Boolean ; virtual;
    procedure SetEnableHyperLinks(AValue: Boolean); virtual;
    procedure SetReport(AValue: TFPCustomReport); virtual;
    Property ReportPages : TFPList Read FReportPages;
    Class Function LoadFromResource : Boolean; virtual;
  Public
    Property Report : TFPCustomReport Read FReport Write SetReport;
  Published
    Property EnableHyperLinks : Boolean Read GetEnableHyperLinks Write SetEnableHyperLinks;
  end;
  TCustomFPreportPreviewFormClass = Class of TCustomFPreportPreviewForm;

  { TFPreportPreviewForm }
  // This is a default, minimalistic implementation. It is shown by default.
  // For a better implementation, add the fpreportpreview form to your project uses clause.

  TFPreportPreviewForm = Class(TCustomFPreportPreviewForm)
  Private
    FToolBar : TToolbar;
    FActions : TActionList;
{$IFDEF USEPAINTBOX}
    FPaintBox : TPaintBox;
{$ELSE}
    FPaintBox : TPanel;
{$ENDIF}
    FRender : TFPReportExportCanvas;
    FAClose : TAction;
    FBClose : TToolButton;
    FAPrevious : TAction;
    FBPrevious : TToolButton;
    FANext : TAction;
    FBNext : TToolButton;
  Protected
    procedure SetReport(AValue: TFPCustomReport); override;
  Public
    Constructor CreateNew(AOwner: TComponent; Num: Integer=0); override;
    procedure DoCloseAction(Sender: TObject);
    procedure DoNextAction(Sender: TObject);
    procedure DoPaintReport(Sender: TObject);
    procedure DoPreviousAction(Sender: TObject);
    procedure UpdateNextAction(Sender: TObject);
    procedure UpdatePreviousAction(Sender: TObject);
  end;

  { TFPreportPreviewExport }

  TFPreportPreviewExport = Class(TFPReportExporter)
  Protected
    procedure DoExecute(const ARTObjects: TFPList); override;
  Public
    Class Var
       DefaultPreviewFormClass : TCustomFPreportPreviewFormClass;
    Class Function Name : String; override;
    Class Function Description : String; override;
  end;

implementation

{ TCustomFPreportPreviewForm }

procedure TCustomFPreportPreviewForm.SetEnableHyperLinks(AValue: Boolean);
begin
//
end;

function TCustomFPreportPreviewForm.GetEnableHyperLinks: Boolean;
begin
  Result:=False;
end;


procedure TCustomFPreportPreviewForm.SetReport(AValue: TFPCustomReport);
begin
  if FReport=AValue then Exit;
  FReport:=AValue;
end;

class function TCustomFPreportPreviewForm.LoadFromResource: Boolean;
begin
  Result:=False;
end;

{ TFPreportPreviewExport }

procedure TFPreportPreviewExport.DoExecute(const ARTObjects: TFPList);
Var
  R : TCustomFPreportPreviewFormClass;
  F : TCustomFPreportPreviewForm;

begin
  R:=Self.DefaultPreviewFormClass;
  if R=Nil then
    R:=TFPreportPreviewForm;
  If R.LoadFromResource then
    F:=R.Create(Self)
  else
    F:=R.CreateNew(Self,0);
  With F do
    try
       Report:=Self.Report;
       FReportPages:=ARTObjects;
       ShowModal;
    finally
       Free;
    end;
end;

class function TFPreportPreviewExport.Name: String;
begin
  Result:='Preview';
end;

class function TFPreportPreviewExport.Description: String;
begin
  Result:='Preview on screen';
end;

{ TFPreportPreviewForm }

procedure TFPreportPreviewForm.SetReport(AValue: TFPCustomReport);
begin
  inherited SetReport(AValue);
  FRender.Report:=AValue;
  If Assigned(AValue) then
    FRender.Execute;
end;

constructor TFPreportPreviewForm.CreateNew(AOwner: TComponent; Num: Integer);

Var
  ButtonLeft : Integer;

  Function CreateAction(Const ACaption : STring; AImageIndex : Integer; AOnExecute,  AOnUpdate : TNotifyEvent) : TAction;

  begin
    Result:=Taction.Create(Self);
    Result.ActionList:=FActions;
    Result.Caption:=ACaption;
    Result.OnExecute:=AOnExecute;
    Result.OnUpdate:=AOnUpdate;
  end;

  Function CreateButton(AAction : TAction) : TToolbutton;

  begin
     Result:=TToolButton.Create(Self);
     Result.Parent:=FToolbar;
     Result.Top:=0;
     Result.Left:=ButtonLeft;
     Result.Width:=32;
     Result.Height:=FToolbar.ButtonHeight;
     Result.Caption:=IntToStr(ButtonLeft);
     Result.Action:=AAction;
     ButtonLeft:=ButtonLeft+Result.Width+1;
  end;

begin
  inherited CreateNew(AOwner, Num);
  ButtonLeft:=0;
  WindowState:=wsMaximized;
  FActions:=TActionList.Create(Self);
  FToolBar:=TToolbar.Create(Self);
  FToolbar.ShowCaptions:=True;
  FToolBar.Height:=24;
  FToolbar.ButtonHeight:=22;
  FToolBar.Parent:=Self;
  FToolBar.Align:=alTop;
  FToolBar.Flat:=True;
  FAClose:=CreateAction('Close',0,@DoCloseAction,Nil);
  FBClose:=CreateButton(FAClose);
  FAPrevious:=CreateAction('Previous',1,@DoPreviousAction,@UpdatePreviousAction);
  FBPrevious:=CreateButton(FAPrevious);
  FANext:=CreateAction('Next',1,@DoNextAction,@UpdateNextAction);
  FBNext:=CreateButton(FANext);
{$IFDEF USEPAINTBOX}
  FPaintBox:=TPaintBox.Create(Self);
{$ELSE}
  FPaintBox:=TPanel.Create(Self);
  FPaintBox.BevelInner:=bvLowered;
  FPaintBox.Caption:='';
{$ENDIF}
  FPaintBox.Parent:=Self;
  FPaintBox.Align:=alClient;
  FPaintBox.OnPaint:=@DoPaintReport;
  FRender:=TFPReportExportCanvas.Create(Self);
  FRender.Canvas:=FPaintBox.Canvas;
  Caption:='fpReport preview';
end;

procedure TFPreportPreviewForm.DoCloseAction(Sender: TObject);
begin
  Close;
end;

procedure TFPreportPreviewForm.DoNextAction(Sender: TObject);
begin
  FRender.PageNumber:=FRender.PageNumber+1
end;

procedure TFPreportPreviewForm.DoPaintReport(Sender: TObject);
begin
  FRender.Execute;
end;

procedure TFPreportPreviewForm.DoPreviousAction(Sender: TObject);
begin
  FRender.PageNumber:=FRender.PageNumber-1;
end;

procedure TFPreportPreviewForm.UpdateNextAction(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(FRender.PageNumber<ReportPages.Count-1);
end;

procedure TFPreportPreviewForm.UpdatePreviousAction(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(FRender.PageNumber>0);

end;

initialization
  TFPreportPreviewExport.RegisterExporter;
end.

