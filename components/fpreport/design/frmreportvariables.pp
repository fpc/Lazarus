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
unit frmreportvariables;

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
    DEDateTime: TDateEdit;
    EString: TEdit;
    EName: TEdit;
    ILVariables: TImageList;
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
    procedure CBTypeSelect(Sender: TObject);
    procedure DoAddVariable(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure LBVariablesSelectionChange(Sender: TObject; User: boolean);
  private
    FCurrentVariable: TFPReportVariable;
    FValueControls : Array[TResultType] of TControl;
    function GetType: TResultType;
    procedure SetCurrentVariable(AValue: TFPReportVariable);
    procedure SetType(AValue: TResultType);
  Protected
    procedure SetVariables(AValue: TFPReportVariables); override;
    procedure ShowCurrentTypeEditor; virtual;
    procedure SetCurrentVariableFromList; virtual;
    procedure SaveCurrentVariable; virtual;
    procedure ShowCurrentVariable; virtual;
    procedure VariablesToForm; virtual;
    Property CurrentVariable : TFPReportVariable Read FCurrentVariable Write SetCurrentVariable;
    Property CurrentType : TResultType Read GetType Write SetType;
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
    V:=Variables.AddVariable(N);
    I:=LBVariables.Items.AddObject(N,V);
    LBVariables.ItemIndex:=I;
    FCurrentVariable:=V;
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
    CurrentVariable:=LBVariables.Items.Objects[i] as TFPReportvariable
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
     Result:=TResultType(CBType.ItemIndex);
end;

procedure TReportVariablesForm.SetType(AValue: TResultType);

begin
  CBType.ItemIndex:=Ord(Avalue);
  ShowCurrentTypeEditor;
end;

procedure TReportVariablesForm.ShowCurrentTypeEditor;

Var
  T : TResultType;

begin
  For T in TResultType do
    FValueControls[T].Visible:=(CBType.Itemindex=Ord(T));
end;

procedure TReportVariablesForm.SaveCurrentVariable;

begin
  if Not Assigned(FCurrentVariable) then exit;
  // This can raise an exception. Catch it, and restore old name
  try
    FCurrentVariable.Name:=EName.Text;
  except
    On E : Exception do
      begin
      Application.ShowException(E);
      EName.Text:=FCurrentVariable.Name;
      end;
  end;
  FCurrentVariable.DataType:=CurrentType;
  With FCurrentVariable do
    Case DataType of
      rtBoolean : AsBoolean:=CBBoolean.Checked;
      rtInteger : AsInteger:=SEinteger.Value;
      rtFloat   : AsFloat:=SEFloat.Value;
      rtDateTime : AsDateTime:=DEDateTime.Date;
      rtString : AsString := EString.Text;
    else
      Raise Exception.Create('Unknown datatype !');
    end
end;

procedure TReportVariablesForm.ShowCurrentVariable;

Var
  T : TResultType;
  HV : Boolean;
begin
  HV:=Assigned(FCurrentVariable);
  EName.Enabled:=HV;
  CBType.Enabled:=HV;
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
      rtDateTime : DEDateTime.Date:=AsDateTime;
      rtString : EString.Text:=AsString;
    else
      Raise Exception.Create('Unknown datatype !');
    end
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

