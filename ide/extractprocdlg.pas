unit ExtractProcDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls, CodeCache, CodeToolManager, ExtractProcTool,
  LazarusIDEStrConsts, IDEProcs, MiscOptions;

type
  TExtractProcDialog = class(TForm)
    NameEdit: TEDIT;
    NameGroupbox: TGROUPBOX;
    OkButton: TBUTTON;
    CancelButton: TBUTTON;
    TypeRadiogroup: TRADIOGROUP;
    procedure ExtractProcDialogCREATE(Sender: TObject);
    procedure ExtractProcDialogClose(Sender: TObject;
      var CloseAction: TCloseAction);
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
    MessageDlg(lisNoCodeSelected,
      lisPleaseSelectSomeCodeToExtractANewProcedureMethod,
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
      MessageDlg(lisInvalidSelection,
        Format(lisThisStatementCanNotBeExtractedPleaseSelectSomeCode, [#13]),
      mtInformation,[mbCancel],0);
    end;
    exit;
  end;
  
  // ask user how to extract
  ExtractProcDialog:=TExtractProcDialog.Create(nil);
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
  Caption:=lisExtractProcedure;
  NameGroupbox.Caption:=lisNameOfNewProcedure;
  OkButton.Caption:=lisExtract;
  CancelButton.Caption:=dlgCancel;
  TypeRadiogroup.Caption:=dlgEnvType;
  NameEdit.Text:=MiscellaneousOptions.ExtractProcName;
end;

procedure TExtractProcDialog.ExtractProcDialogClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  MiscellaneousOptions.ExtractProcName:=NameEdit.Text;
end;

procedure TExtractProcDialog.OkButtonCLICK(Sender: TObject);
var
  ProcName: String;
begin
  ProcName:=GetProcName;
  if (ProcName='') or (not IsValidIdent(ProcName)) then begin
    MessageDlg(lisInvalidProcName,
      Format(lisSVUOisNotAValidIdentifier, ['"', ProcName, '"']),
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
      Add(lisPublicMethod);
      Add(lisPrivateMethod);
      Add(lisProtectedMethod);
      Add(lisPublishedMethod);
      TypeRadiogroup.Columns:=2;
    end else begin
      TypeRadiogroup.Columns:=1;
    end;
    Add(lisProcedure);
    Add(lisProcedureWithInterface);
    Add(lisSubProcedure);
    if SubProcSameLvlPossible then
      Add(lisSubProcedureOnSameLevel);
    EndUpdate;
    TypeRadiogroup.ItemIndex:=Count-1;
  end;
end;

function TExtractProcDialog.GetProcType: TExtractProcType;
var
  Item: string;
begin
  Result:=eptSubProcedure;
  if TypeRadiogroup.ItemIndex>=0 then begin
    Item:=TypeRadiogroup.Items[TypeRadiogroup.ItemIndex];
    if Item=lisPublicMethod then Result:=eptPublicMethod
    else if Item=lisPrivateMethod then Result:=eptPrivateMethod
    else if Item=lisProtectedMethod then Result:=eptProtectedMethod
    else if Item=lisPublishedMethod then Result:=eptPublishedMethod
    else if Item=lisProcedure then Result:=eptProcedure
    else if Item=lisProcedureWithInterface then Result:=eptProcedureWithInterface
    else if Item=lisSubProcedure then Result:=eptSubProcedure
    else if Item=lisSubProcedureOnSameLevel then Result:=eptSubProcedureSameLvl;
  end;
end;

function TExtractProcDialog.GetProcName: string;
begin
  Result:=NameEdit.Text;
end;

initialization
  {$I extractprocdlg.lrs}

end.

