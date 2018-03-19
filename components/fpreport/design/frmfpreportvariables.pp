{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Edit form for report variables.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frmfpreportvariables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, Buttons, Spin, EditBtn, ActnList,
{$IF FPC_FULLVERSION>=30101}
  fpexprpars,
{$ELSE}
  fprepexprpars,
{$ENDIF}
  fpreport, reportdesignbaseforms;

type
  TForm = TBaseReportVariablesForm;

  { TReportVariablesForm }

  TReportVariablesForm = class(TForm)
    AAddVariable: TAction;
    ADeleteVariable: TAction;
    ALVariables: TActionList;
    BPVariables: TButtonPanel;
    CBType: TComboBox;
    CBBoolean: TCheckBox;
    CBAggregate: TCheckBox;
    CBResetExpression: TComboBox;
    CBResetType: TComboBox;
    DEDateTime: TDateEdit;
    EString: TEdit;
    EName: TEdit;
    EExpression: TEdit;
    ILVariables: TImageList;
    LCBExpression: TLabel;
    LCBType1: TLabel;
    LValue1: TLabel;
    LCBResetExpression: TLabel;
    SEFloat: TFloatSpinEdit;
    LEName: TLabel;
    LCBType: TLabel;
    LValue: TLabel;
    LLBVariables: TLabel;
    LBVariables: TListBox;
    SBDelete: TSpeedButton;
    SBAdd: TSpeedButton;
    SEinteger: TSpinEdit;
    procedure ADeleteVariableExecute(Sender: TObject);
    procedure ADeleteVariableUpdate(Sender: TObject);
    procedure CBAggregateChange(Sender: TObject);
    procedure CBResetTypeChange(Sender: TObject);
    procedure CBTypeSelect(Sender: TObject);
    procedure DoAddVariable(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure LBVariablesSelectionChange(Sender: TObject; User: boolean);
  private
    FCurrentIndex : integer;
    FCurrentVariable: TFPReportVariable;
    FValueControls : Array[TResultType] of TControl;
    procedure CheckExpression;
    procedure CheckResetType;
    procedure FillExpressionsList;
    function GetCurrentIsExpression: Boolean;
    function GetCurrentResetType: TFPReportResetType;
    function GetType: TResultType;
    procedure SetCurrentIsExpression(AValue: Boolean);
    procedure SetCurrentResetType(AValue: TFPReportResetType);
    procedure SetCurrentVariable(AValue: TFPReportVariable);
    procedure SetType(AValue: TResultType);
  Protected
    procedure SetReport(AValue: TFPCustomReport); override;
    procedure SetVariables(AValue: TFPReportVariables); override;
    procedure ShowCurrentTypeEditor; virtual;
    procedure SetCurrentVariableFromList; virtual;
    procedure SaveCurrentVariable; virtual;
    procedure ShowCurrentVariable; virtual;
    procedure VariablesToForm; virtual;
    Property CurrentVariable : TFPReportVariable Read FCurrentVariable Write SetCurrentVariable;
    Property CurrentType : TResultType Read GetType Write SetType;
    property CurrentIsExpression : Boolean Read GetCurrentIsExpression Write SetCurrentIsExpression;
    Property CurrentResetType : TFPReportResetType Read GetCurrentResetType Write SetCurrentResetType;
  end;

implementation

{$R *.lfm}

Resourcestring
  SErrNewVariable  = 'New report variable';
  SNameNewVariable = 'Enter the name of the variable.';
  SAllowedChars1   = 'Allowed characters are letters, numbers, underscores (_) and dots (.).';
  SAllowedChars2   = 'The first character must be a letter or underscore';
  SErrIllegalVariableName = 'The variable name %s is not a legal variable name.';
  SWarnDuplicateVariableName = 'The variable name %s already exists.';

{ TReportVariablesForm }

procedure TReportVariablesForm.SetCurrentVariable(AValue: TFPReportVariable);
begin
  if FCurrentVariable=AValue then Exit;
  SaveCurrentVariable;
  FCurrentVariable:=AValue;
  ShowCurrentVariable;
end;


procedure TReportVariablesForm.FormCreate(Sender: TObject);

Var
  T : TResultType;
begin
  FValueControls[rtBoolean]:=CBBoolean;
  FValueControls[rtInteger]:=SEinteger;
  FValueControls[rtFloat]:=SEFloat;
  FValueControls[rtCurrency]:=SEFloat;
  FValueControls[rtDateTime]:=DEDateTime;
  FValueControls[rtString]:=EString;
  For T in TResultType do
    begin
    FValueControls[T].Visible:=False;
    FValueControls[T].Left:=CBType.Left;
    FValueControls[T].Top:=LValue.Top;
    end;
end;

procedure TReportVariablesForm.LBVariablesSelectionChange(Sender: TObject;
  User: boolean);
begin
  if User then
    SetCurrentVariableFromList;
end;

procedure TReportVariablesForm.DoAddVariable(Sender: TObject);

Var
  DOK,VOK : Boolean;
  N : String;
  V : TFPReportVariable;
  I : Integer;

begin
  I:=Variables.Count;
  Repeat
    Inc(I);
    N:='Variable'+IntToStr(I);
  until (Variables.FindVariable(N)=Nil);
  VOK:=False;
  Repeat
    DOK:=InputQuery(SErrNewVariable,SNameNewVariable+sLineBreak+SAllowedChars1+sLineBreak+SAllowedChars2,N);
    if not DOK then
      VOK:=False
    else
      begin
      VOK:=IsValidIdent(N);
      if not VOK then
        ShowMessage(Format(SErrIllegalVariableName,[N]))
      else
        begin
        VOK:=(Variables.IndexOfVariable(N)=-1);
        if not VOK then
          ShowMessage(Format(SWarnDuplicateVariableName,[N]));
        end;
      end;
  Until VOK or not DOK;
  if DOK and VOK then
    begin
    SaveCurrentVariable;
    V:=Variables.AddVariable(N);
    I:=LBVariables.Items.AddObject(N,V);
    LBVariables.ItemIndex:=I;
    FCurrentVariable:=V;
    FCurrentIndex:=I;
    ShowCurrentVariable;
    end;
end;

procedure TReportVariablesForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  SaveCurrentVariable;
  CanClose:=True;
end;

procedure TReportVariablesForm.ADeleteVariableUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(FCurrentVariable<>Nil);
end;

procedure TReportVariablesForm.CBAggregateChange(Sender: TObject);
begin
  CheckExpression;
end;

procedure TReportVariablesForm.CBResetTypeChange(Sender: TObject);
begin
  CheckResetType;
end;

procedure TReportVariablesForm.CBTypeSelect(Sender: TObject);
begin
  ShowCurrentTypeEditor;
end;

procedure TReportVariablesForm.SetCurrentVariableFromList;

Var
  I : Integer;

begin
  I:=LBVariables.ItemIndex;
  if I<>-1 then
    begin
    CurrentVariable:=LBVariables.Items.Objects[i] as TFPReportvariable;
    // Only after the variable is set, because Save needs the current one
    FCurrentIndex:=I;
    end
  else
    CurrentVariable:=Nil;
end;

procedure TReportVariablesForm.ADeleteVariableExecute(Sender: TObject);

Var
  I : Integer;

begin
  if FCurrentVariable=Nil then exit;
  I:=LBVariables.Items.IndexOfObject(FCurrentVariable);
  FreeAndNil(FCurrentVariable);
  if I<>-1 then
    begin
    LBVariables.Items.Delete(I);
    if (I>=LBVariables.Items.Count) then
      I:=LBVariables.Items.Count-1;
    end;
  // Will trigger OnSelectionChange
  LBVariables.ItemIndex:=I;

end;

function TReportVariablesForm.GetType: TResultType;
begin
  if (CBType.ItemIndex in [ord(Low(TResultType))..ord(High(TResultType))]) then
    Result:=TResultType(CBType.ItemIndex)
  else
    Result:=rtString;
end;

function TReportVariablesForm.GetCurrentIsExpression: Boolean;
begin
  Result:=CBAggregate.Checked;
end;

function TReportVariablesForm.GetCurrentResetType: TFPReportResetType;
begin
  if CBResetType.ItemIndex=-1 then
    Result:=rtNone
  else
    Result:=TFPReportResetType(CBResetType.ItemIndex)
end;

procedure TReportVariablesForm.SetCurrentIsExpression(AValue: Boolean);
begin
  CBAggregate.Checked:=AValue;
  CheckExpression;
end;

procedure TReportVariablesForm.SetCurrentResetType(AValue: TFPReportResetType);
begin
  CBResetType.ItemIndex:=Ord(AValue);
  CheckResetType;
end;

procedure TReportVariablesForm.CheckResetType;

begin
  CBResetType.Enabled:=CBAggregate.Checked;
  CBResetExpression.Enabled:=CBAggregate.Checked and (CurrentResetType in [rtGroup]);
  Case CurrentResetType of
    rtPage:   CBResetExpression.Text:='PageNo';
    rtColumn: CBResetExpression.Text:='ColNo';
    rtGroup,
    rtNone : if not CBResetExpression.Enabled then
              CBResetExpression.Text:=''
  end;
end;

procedure TReportVariablesForm.CheckExpression;

begin
  EExpression.Enabled:=CBAggregate.Checked;
  if not EExpression.Enabled then
    EExpression.Text:='';
  CheckResetType;
end;

procedure TReportVariablesForm.SetType(AValue: TResultType);

begin
  CBType.ItemIndex:=Ord(Avalue);
  ShowCurrentTypeEditor;
end;

procedure TReportVariablesForm.SetReport(AValue: TFPCustomReport);
begin
  inherited SetReport(AValue);
  FillExpressionsList;
end;

procedure TReportVariablesForm.FillExpressionsList;

Var
  R : TFPReport;
  S : String;
  I,J :  Integer;

begin
  R:=TFPReport(Report);
  For I:=0 to R.PageCount-1 do
    For J:=0 to R.Pages[I].BandCount-1 do
      If R.Pages[I].Bands[J] is TFPReportCustomGroupHeaderBand then
        begin
        S:=TFPReportGroupHeaderBand(R.Pages[I].Bands[J]).GroupCondition;
        if (S<>'') then
          CBResetExpression.Items.Add(S);
        end;
end;

procedure TReportVariablesForm.ShowCurrentTypeEditor;

Var
  T : TResultType;

begin
  // First hide all
  For T in TResultType do
    FValueControls[T].Visible:=False;
  // Show correct one.
  // Need to do it like this, because float is used for different types...
  FValueControls[GetType].Visible:=True;
end;

procedure TReportVariablesForm.SaveCurrentVariable;

begin
  if Not Assigned(FCurrentVariable) then exit;
  // This can raise an exception. Catch it, and restore old name
  try
    FCurrentVariable.Name:=EName.Text;
    LBVariables.Items[FCurrentIndex]:=FCurrentVariable.Name;
  except
    On E : Exception do
      begin
      Application.ShowException(E);
      EName.Text:=FCurrentVariable.Name;
      end;
  end;
  FCurrentVariable.DataType:=CurrentType;
  With FCurrentVariable do
    begin
    Case DataType of
      rtBoolean : AsBoolean:=CBBoolean.Checked;
      rtInteger : AsInteger:=SEinteger.Value;
      rtFloat   : AsFloat:=SEFloat.Value;
      rtCurrency : AsCurrency:=SEFloat.Value;
      rtDateTime : AsDateTime:=DEDateTime.Date;
      rtString : AsString := EString.Text;
    else
      Raise Exception.Create('Unknown datatype !');
    end;
    If CurrentIsExpression then
      begin
      FCurrentVariable.Expression:=EExpression.Text;
      FCurrentVariable.ResetType:=CurrentResetType;
      FCurrentVariable.ResetValueExpression:=CBResetExpression.Text;
      end
    else
      begin
      FCurrentVariable.Expression:='';
      FCurrentVariable.ResetType:=rtNone;
      FCurrentVariable.ResetValueExpression:='';
      end
    end;
end;

procedure TReportVariablesForm.ShowCurrentVariable;

Var
  T : TResultType;
  HV : Boolean;
begin
  HV:=Assigned(FCurrentVariable);
  EName.Enabled:=HV;
  CBType.Enabled:=HV;
  CurrentIsExpression:=HV and (FCurrentVariable.Expression<>'');
  CBAggregate.Enabled:=HV;
  if not HV then
    begin
    EName.Text:='';
    CBType.ItemIndex:=-1;
    for T in TResultType do
      FValueControls[T].Visible:=False;
    end
  else
    begin
    EName.Text:=FCurrentVariable.Name;
    CurrentType:=FCurrentVariable.DataType;
    With FCurrentVariable do
    Case DataType of
      rtBoolean : CBBoolean.Checked:=AsBoolean;
      rtInteger : SEinteger.Value:=AsInteger;
      rtFloat   : SEFloat.Value:=AsFloat;
      rtCurrency  : SEFloat.Value:=AsCurrency;
      rtDateTime : DEDateTime.Date:=AsDateTime;
      rtString : EString.Text:=AsString;
    else
      Raise Exception.Create('Unknown datatype !');
    end;
    CurrentIsExpression:=FCurrentVariable.Expression<>'';
    If CurrentIsExpression then
      begin
      EExpression.Text:=FCurrentVariable.Expression;
      CurrentResetType:=FCurrentVariable.ResetType;
      CBResetExpression.Text:=FCurrentVariable.ResetValueExpression;
      end;
    end;
end;

procedure TReportVariablesForm.SetVariables(AValue: TFPReportVariables);

begin
  inherited;
  VariablesToForm;
end;

procedure TReportVariablesForm.VariablesToForm;

Var
  I : Integer;
  V : TFPReportVariable;

begin
  With LBVariables.Items do
    try
      BeginUpdate;
      Clear;
      LBVariables.Sorted:=False;
      For I:=0 to Variables.Count-1 do
        begin
        V:=Variables[i];
        AddObject(V.Name,V);
        end;
      LBVariables.Sorted:=True;
      if Count>0 then
        begin
        LBVariables.ItemIndex:=0;
        CurrentVariable:=LBVariables.Items.Objects[0] as TFPReportVariable;
        end
      else
        begin
        LBVariables.ItemIndex:=-1;
        CurrentVariable:=Nil;
        ShowCurrentVariable; // If it is already nil we need to force re-show
        end;
    finally
      EndUpdate;
    end;
end;

initialization
  ReportVariablesFormClass:=TReportVariablesForm;
end.

