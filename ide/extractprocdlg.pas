unit ExtractProcDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, CodeCache, CodeToolManager, ExtractProcTool, IDEProcs;

type
  TExtractProcDialog = class(TForm)
    NameEdit: TEDIT;
    NameGroupbox: TGROUPBOX;
    OkButton: TBUTTON;
    CancelButton: TBUTTON;
    TypeRadiogroup: TRADIOGROUP;
    procedure ExtractProcDialogCREATE(Sender: TObject);
    procedure OkButtonCLICK(Sender: TObject);
  private
    FMethodPossible: boolean;
    FSubProcSameLvlPossible: boolean;
  public
    procedure UpdateAvailableTypes;
    function GetProcType: TExtractProcType;
    function GetProcName: string;
    property MethodPossible: boolean read FMethodPossible write FMethodPossible;
    property SubProcSameLvlPossible: boolean read FSubProcSameLvlPossible write FSubProcSameLvlPossible;
  end;

function ShowExtractProcDialog(Code: TCodeBuffer;
  const BlockBegin, BlockEnd: TPoint;
  var NewSource: TCodeBuffer;
  var NewX, NewY, NewTopLine: integer): TModalresult;

implementation

function ShowExtractProcDialog(Code: TCodeBuffer;
  const BlockBegin, BlockEnd: TPoint;
  var NewSource: TCodeBuffer;
  var NewX, NewY, NewTopLine: integer): TModalresult;
var
  ExtractProcDialog: TExtractProcDialog;
  MethodPossible: Boolean;
  SubProcSameLvlPossible: boolean;
  ProcName: String;
  ProcType: TExtractProcType;
begin
  Result:=mrCancel;
  if CompareCaret(BlockBegin,BlockEnd)<=0 then begin
    MessageDlg('No code selected',
      'Please select some code to extract a new procedure/method.',
      mtInformation,[mbCancel],0);
    exit;
  end;
  // check if selected statements can be extracted
  MethodPossible:=false;
  SubProcSameLvlPossible:=false;
  if not CodeToolBoss.CheckExtractProc(Code,BlockBegin,BlockEnd,MethodPossible,
    SubProcSameLvlPossible)
  then begin
    if CodeToolBoss.ErrorMessage='' then begin
      MessageDlg('Invalid selection',
        'This statement can not be extracted.'#13
        +'Please select some code to extract a new procedure/method.',
      mtInformation,[mbCancel],0);
    end;
    exit;
  end;
  
  // ask user how to extract
  ExtractProcDialog:=TExtractProcDialog.Create(Application);
  try
    ExtractProcDialog.MethodPossible:=MethodPossible;
    ExtractProcDialog.SubProcSameLvlPossible:=SubProcSameLvlPossible;
    ExtractProcDialog.UpdateAvailableTypes;
    Result:=ExtractProcDialog.ShowModal;
    if Result<>mrOk then exit;
    ProcName:=ExtractProcDialog.GetProcName;
    ProcType:=ExtractProcDialog.GetProcType;
  finally
    ExtractProcDialog.Free;
  end;

  // extract procedure/method
  if not CodeToolBoss.ExtractProc(Code,BlockBegin,BlockEnd,ProcType,ProcName,
    NewSource,NewX,NewY,NewTopLine)
  then begin
    Result:=mrCancel;
    exit;
  end;
  Result:=mrOk;
end;

{ TExtractProcDialog }

procedure TExtractProcDialog.ExtractProcDialogCREATE(Sender: TObject);
begin
  Caption:='Extract Procedure';
  NameGroupbox.Caption:='Name of new procedure';
  OkButton.Caption:='Extract';
  CancelButton.Caption:='Cancel';
  TypeRadiogroup.Caption:='Type';
  NameEdit.Text:='NewProc';
end;

procedure TExtractProcDialog.OkButtonCLICK(Sender: TObject);
var
  ProcName: String;
begin
  ProcName:=GetProcName;
  if (ProcName='') or (not IsValidIdent(ProcName)) then begin
    MessageDlg('Invalid proc name',
      '"'+ProcName+'" is not a valid identifier.',
      mtError,[mbCancel],0);
    exit;
  end;
  ModalResult:=mrOk;
end;

procedure TExtractProcDialog.UpdateAvailableTypes;
begin
  with TypeRadiogroup.Items do begin
    BeginUpdate;
    Clear;
    if MethodPossible then begin
      Add('Public Method');
      Add('Private Method');
      Add('Protected Method');
      Add('Published Method');
      TypeRadiogroup.Columns:=2;
    end else begin
      TypeRadiogroup.Columns:=1;
    end;
    Add('Procedure');
    Add('Procedure with interface');
    Add('Sub Procedure');
    if SubProcSameLvlPossible then
      Add('Sub Procedure on same level');
    EndUpdate;
    TypeRadiogroup.ItemIndex:=Count-1;
  end;
end;

function TExtractProcDialog.GetProcType: TExtractProcType;
begin
  case TypeRadiogroup.ItemIndex of
  0: Result:=eptPublicMethod;
  1: Result:=eptPrivateMethod;
  2: Result:=eptProtectedMethod;
  3: Result:=eptPublishedMethod;
  4: Result:=eptProcedure;
  5: Result:=eptProcedureWithInterface;
  6: Result:=eptSubProcedure;
  7: Result:=eptSubProcedureSameLvl;
  else
    Result:=eptSubProcedure;
  end;
end;

function TExtractProcDialog.GetProcName: string;
begin
  Result:=NameEdit.Text;
end;

initialization
  {$I extractprocdlg.lrs}

end.

