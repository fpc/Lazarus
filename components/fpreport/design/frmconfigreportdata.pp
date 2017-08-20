{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Form to configure the report loops in a report.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frmconfigreportdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons, ButtonPanel, ActnList, ComCtrls, ExtCtrls,
  EditBtn, designreportdata, fpjson, db, reportdesignbaseforms;

type
  TForm = TBaseReportDataForm;
  { TReportDataConfigForm }

  TReportDataConfigForm = class(TForm)
    AAddData: TAction;
    APreview: TAction;
    ADuplicate: TAction;
    ADeleteData: TAction;
    ALReportData: TActionList;
    BPVariables: TButtonPanel;
    CBType: TComboBox;
    EName: TEdit;
    ILReportdata: TImageList;
    LBReportData: TListBox;
    LENAme: TLabel;
    Label3: TLabel;
    LLBReportData: TLabel;
    PSources: TPanel;
    PData: TPanel;
    SBAdd: TSpeedButton;
    SBDuplicate: TSpeedButton;
    SBDelete: TSpeedButton;
    SBPreview: TSpeedButton;
    Splitter1: TSplitter;
    procedure AAddDataExecute(Sender: TObject);
    procedure ADeleteDataExecute(Sender: TObject);
    procedure ADeleteDataUpdate(Sender: TObject);
    procedure ADuplicateExecute(Sender: TObject);
    procedure ADuplicateUpdate(Sender: TObject);
    procedure APreviewExecute(Sender: TObject);
    procedure APreviewUpdate(Sender: TObject);
    procedure CBTypeChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LBReportDataSelectionChange(Sender: TObject; User: boolean);
  private
    FCurrentHandler : TDesignReportDataHandler;
    FCurrentData : TDesignReportData;
    FCurrentFrame : TReportDataConfigFrame;
  Protected
    procedure NewItem(CloneFrom: TDesignReportData); virtual;
    function SaveCurrentItem: Boolean; virtual;
    procedure SetData(AValue: TDesignReportDataCollection); override;
    procedure ShowData; virtual;
    procedure ShowDataFrame; virtual;
    procedure ShowSelectedItem;virtual;
  end;

implementation

{$R *.lfm}

Resourcestring
  SDataNotOK  = 'Report data source not correctly configured';
  SErrNewData = 'New report data source';
  SNameNewData = 'Enter the name of the data source.';
  SAllowedChars1   = 'Allowed characters are letters, numbers, underscores (_) and dots (.).';
  SAllowedChars2   = 'The first character must be a letter or underscore';
  SErrIllegalDataName = 'The data source name %s is not a legal data source name.';
  SWarnDuplicateDataName = 'The data set name %s already exists.';

{ TReportDataConfigForm }

procedure TReportDataConfigForm.FormCreate(Sender: TObject);
begin
  TDesignReportDataHandler.GetRegisteredTypes(CBType.Items);
  ShowSelectedItem;
end;

procedure TReportDataConfigForm.CBTypeChange(Sender: TObject);
begin
  ShowDataFrame;
end;

procedure TReportDataConfigForm.ShowDataFrame;

begin
  FreeAndNil(FCurrentHandler);
  FreeAndNil(FCurrentFrame);
  if CBType.ItemIndex=-1 then
    exit;
  FCurrentHandler:=TDesignReportDataHandler.GetTypeHandler(CBType.Text);
  FCurrentFrame:=FCurrentHandler.CreateConfigFrame(Self);
  FCurrentFrame.Parent:=PData;
  FCurrentFrame.Align:=alClient;
  if Assigned(FCurrentData) then
    FCurrentFrame.SetConfig(FCurrentData.Config);
end;

procedure TReportDataConfigForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);

Var
  I : Integer;
  OK : Boolean;
  S : String;

begin
  OK:=ModalResult<>mrOK;
  if Not OK then
    begin
    OK:=SaveCurrentItem;
    I:=0;
    if OK then
      begin
      While OK and (I<Data.Count) do
        begin
        S:=Data[i].Check;
        OK:=(S='');
        if OK then
          Inc(I);
        end;
      if not OK then
        begin
        LBReportData.ItemIndex:=I;
        MessageDlg(SDataNotOK,S,mtError,[mbOK],0);
        end;
      end;
    end;
  CanClose:=OK;
end;

procedure TReportDataConfigForm.AAddDataExecute(Sender: TObject);

begin
  SaveCurrentItem;
  NewItem(Nil);
end;

procedure TReportDataConfigForm.NewItem(CloneFrom : TDesignReportData);

Var
  DOK,VOK : Boolean;
  N : String;
  D : TDesignReportData;
  I : Integer;

begin
  I:=Data.Count;
  Repeat
    Inc(I);
    N:='Data'+IntToStr(I);
  until (Data.FindDataByName(N)=Nil);
  VOK:=False;
  Repeat
    DOK:=InputQuery(SErrNewData,SNameNewData+sLineBreak+SAllowedChars1+sLineBreak+SAllowedChars2,N);
    if not DOK then
      VOK:=False
    else
      begin
      VOK:=IsValidIdent(N);
      if not VOK then
        ShowMessage(Format(SErrIllegalDataName,[N]))
      else
        begin
        VOK:=(Data.IndexOfName(N)=-1);
        if not VOK then
          ShowMessage(Format(SWarnDuplicateDataName,[N]));
        end;
      end;
  Until VOK or not DOK;
  if (VOK and DOK) then
    begin
    if CloneFrom<>Nil then
      D:=CloneFrom.Clone(N)
    else
      begin
      D:=Data.AddData(N);
      D.DataType:='';
      end;
    I:=LBReportData.Items.AddObject(N,D);
    LBReportData.ItemIndex:=I;
    if (FCurrentData<>D) then
      begin
      FCurrentData:=D;
      ShowSelectedItem;
      end;
    end;
end;

procedure TReportDataConfigForm.ADeleteDataExecute(Sender: TObject);
Var
  I : Integer;

begin
  if FCurrentData=Nil then exit;
  I:=LBReportData.Items.IndexOfObject(FCurrentData);
  FreeAndNil(FCurrentData);
  if I<>-1 then
    begin
    LBReportData.Items.Delete(I);
    if (I>=LBReportData.Items.Count) then
      I:=LBReportData.Items.Count-1;
    end;
  // Will trigger OnSelectionChange
  LBReportData.ItemIndex:=I;
end;

procedure TReportDataConfigForm.ADeleteDataUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(FCurrentData<>Nil);
end;

procedure TReportDataConfigForm.ADuplicateExecute(Sender: TObject);
begin
  SaveCurrentItem;
  NewItem(FCurrentData);
end;

procedure TReportDataConfigForm.ADuplicateUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(FCurrentData<>Nil);
end;

procedure TReportDataConfigForm.APreviewExecute(Sender: TObject);

Var
  C : TJSONObject;
  DS : TDataset;
  S : String;
  F : TBaseReportDataPreviewForm;

begin
  DS:=Nil;
  S:=FCurrentFrame.SaveNotOKMessage;
  if (S<>'') then
    begin
    MessageDlg(SDataNotOK,S,mtError,[mbOK],0);
    exit;
    end;
  C:=TJSONObject.Create;
  try
    FCurrentFrame.GetConfig(C);
    S:=FCurrentHandler.CheckConfig(C);
    if (S<>'') then
      begin
      MessageDlg(SDataNotOK,S,mtError,[mbOK],0);
      exit;
      end;
    F:=ReportDataPreviewClass.Create(Self);
    F.PreviewDataset:=FCurrentHandler.CreateDataset(F,C);
    F.Show;
  finally
    C.Free;
  end;

end;

procedure TReportDataConfigForm.APreviewUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(ReportDataPreviewClass) and (FCurrentData<>Nil) and (FCurrentFrame<>Nil) and (FCurrentHandler<>Nil);
end;

procedure TReportDataConfigForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FCurrentHandler);
  FreeAndNil(FCurrentFrame);
end;

procedure TReportDataConfigForm.LBReportDataSelectionChange(Sender: TObject; User: boolean);

Var
  D : TDesignReportData;

begin
  SaveCurrentItem;
  if LBReportData.ItemIndex=-1 then
    D:=Nil
  else
    D:=(LBReportData.Items.Objects[LBReportData.ItemIndex] as TDesignReportData);
  if D<>FCurrentData then
    begin
    FCurrentData:=D;
    ShowSelectedItem;
    end;
end;

procedure TReportDataConfigForm.SetData(AValue: TDesignReportDataCollection);
begin
  if Data=AValue then Exit;
  Inherited;
  ShowData;
end;

Function TReportDataConfigForm.SaveCurrentItem : Boolean;

Var
  S : String;

begin
  Result:=True;
  If Not Assigned(FCurrentData) then
    exit;
  If Not Assigned(FCurrentFrame) then
    exit;
  S:=FCurrentFrame.SaveNotOKMessage;
  Result:=(S='');
  if not Result then
    begin
    ShowMessage(S);
    Exit;
    end;
  // This can raise an exception. Catch it, and restore old name

  try
    FCurrentData.Name:=EName.Text;
  except
    On E : Exception do
      begin
      Application.ShowException(E);
      EName.Text:=FCurrentData.Name;
      end;
  end;
  FCurrentData.DataType:=CBType.text;
  if (FCurrentData.DataType<>'') then
    begin
    if not Assigned(FCurrentFrame) then
      Raise Exception.Create('Internal error : No config frame');
    FCurrentFrame.GetConfig(FCurrentData.Config);
    end;
end;

procedure TReportDataConfigForm.ShowSelectedItem;

Var
  haveItem : Boolean;

begin
  HaveItem:=Assigned(FCurrentData);
  EName.Enabled:=HaveItem;
  CBType.Enabled:=HaveItem;
  if Not HaveItem  then
    begin
    EName.Text:='';
    CBType.ItemIndex:=-1;
    end
  else
    begin
    EName.Text:=FCurrentData.Name;
    // Will trigger change
    CBType.ItemIndex:=CBType.Items.IndexOf(FCurrentData.DataType);
    end;
  ShowDataFrame;
end;

procedure TReportDataConfigForm.ShowData;

Var
  I : Integer;
  S : TDesignReportData;

begin
  LBReportData.Items.Clear;
  if Not Assigned(Data) then
    exit;
  For I:=0 to Data.Count-1 do
    begin
    S:=Data[i];
    LBReportData.Items.AddObject(S.Name,S);
    end;
  if Data.Count>0 then
    LBReportData.ItemIndex:=0
  else
    LBReportData.ItemIndex:=-1;
end;

initialization
  ReportDataFormClass:=TReportDataConfigForm;
end.

