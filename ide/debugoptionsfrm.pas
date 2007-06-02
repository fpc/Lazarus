{
 /***************************************************************************
                           DebugOptionsFrm.pas
                           -------------------

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit DebugOptionsFrm;

{$mode objfpc} {$H+}

interface

uses
  Classes, SysUtils, TypInfo, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, Buttons, ComCtrls, Menus, Spin, CheckLst, FileUtil,
  PropEdits, ObjectInspector,
  LazarusIDEStrConsts, InputHistory, IDEProcs, LCLProc,
  PathEditorDlg, EnvironmentOpts, BaseDebugManager, Debugger, DBGUtils;

type

  { TDebuggerOptionsForm }

  TDebuggerOptionsForm = class (TForm )
    cmdOK: TBitBtn;
    cmdCancel: TBitBtn;
    gcbDebuggerGeneralOptions: TCheckGroup;    
    clbExceptions: TCHECKLISTBOX;
    chkMessagesInterface: TCHECKBOX;
    chkClearLogOnRun: TCHECKBOX;
    chkLimitLineCount: TCHECKBOX;
    chkMessagesBreakpoint: TCHECKBOX;
    chkMessagesProcess: TCHECKBOX;
    chkMessagesThread: TCHECKBOX;
    chkMessagesModule: TCHECKBOX;
    chkMessagesOutput: TCHECKBOX;
    chkMessagesWindow: TCHECKBOX;
    cmdOpenDebuggerPath: TBUTTON;
    cmdOpenAdditionalPath: TBUTTON;
    cmdExceptionRemove: TBUTTON;
    cmdExceptionAdd: TBUTTON;
    cmdSignalRemove: TBUTTON;
    cmdSignalAdd: TBUTTON;
    chkBreakOnException: TCHECKBOX;
    cmbDebuggerType: TCOMBOBOX;
    cmbDebuggerPath: TCOMBOBOX;
    N1: TMENUITEM;
    pnlDebugSpecific: TPanel;
    seLimitLinecount: TSPINEDIT;
    txtAdditionalPath: TEDIT;
    gbDebuggerType: TGROUPBOX;
    gbAdditionalSearchPath: TGROUPBOX;
    gbDebuggerSpecific: TGROUPBOX;
    gbGeneral: TGROUPBOX;
    gbMessages: TGROUPBOX;
    bgIgnoreExceptions: TGROUPBOX;
    gbSignals: TGROUPBOX;
    lvSignals: TLISTVIEW;
    mnuResumeUnhandled: TMENUITEM;
    mnuHandledByProgram: TMENUITEM;
    mnuiHandledByDebugger: TMENUITEM;
    mnuResumeHandled: TMENUITEM;
    nbDebugOptions: TNOTEBOOK;
    pgSignals: TPAGE;
    pgExceptions: TPAGE;
    pgEventLog: TPAGE;
    pgGeneral: TPAGE;
    popSignal: TPOPUPMENU;
    PropertyGrid: TOIPropertyGrid;
    procedure DebuggerOptionsFormCREATE(Sender: TObject);
    procedure DebuggerOptionsFormDESTROY(Sender: TObject);
    procedure clbExceptionsCLICK (Sender: TObject );
    procedure cmbDebuggerTypeCHANGE(Sender: TObject);
    procedure cmdExceptionAddCLICK (Sender: TObject );
    procedure cmdExceptionRemoveCLICK (Sender: TObject );
    procedure cmdOKCLICK (Sender: TObject );
    procedure cmdOpenAdditionalPathClick(Sender: TObject);
    procedure cmdOpenDebuggerPathCLICK(Sender: TObject);
  private
    FPropertyEditorHook: TPropertyEditorHook;
    FExceptionDeleteList: TStringList;
    FOldDebuggerPathAndParams: string;
    FCurDebuggerClass: TDebuggerClass; // currently shown debugger class
    procedure AddExceptionLine(const AException: TIDEException; AName: String);
    procedure AddSignalLine(const ASignal: TIDESignal);
    function CheckValues: boolean;
    procedure FetchDebuggerClass;
    procedure FetchDebuggerGeneralOptions;
    procedure FetchDebuggerSpecificOptions;
    function  GetDebuggerClass: TDebuggerClass;
    procedure SetDebuggerClass(const AClass: TDebuggerClass);
  public
  end;

var
  DebuggerOptionsForm: TDebuggerOptionsForm;

implementation

const
  HANDLEDBY_CAPTION: array [Boolean] of String = ('Program', 'Debugger');
  RESUME_CAPTION: array[Boolean] of String = ('Unhandled', 'Handled');

{ TDebuggerOptionsForm }

procedure TDebuggerOptionsForm.AddExceptionLine(const AException: TIDEException;
  AName: String);
var
  idx: Integer;
begin
  if (AName = '') and (AException <> nil)
  then AName := AException.Name;
  if AName = '' then Exit;

  idx := clbExceptions.Items.AddObject(AName, AException);
  clbExceptions.Checked[idx] := (AException = nil) or AException.Enabled;
end;

procedure TDebuggerOptionsForm.AddSignalLine(const ASignal: TIDESignal);
var
  Item: TListItem;
begin
  Item := lvSignals.Items.Add;
  Item.Caption := ASignal.Name;
  Item.SubItems.Add(IntToStr(ASignal.ID));
  Item.SubItems.Add(HANDLEDBY_CAPTION[ASignal.HandledByDebugger]);
  Item.SubItems.Add(RESUME_CAPTION[ASignal.ResumeHandled]);
  Item.Data := ASignal;
end;

function TDebuggerOptionsForm.CheckValues: boolean;
begin
  Result := false;
  
  if assigned(FCurDebuggerClass) and FCurDebuggerClass.HasExePath and
    not CheckExecutable(FOldDebuggerPathAndParams,cmbDebuggerPath.Text,
          lisEnvOptDlgInvalidDebuggerFilename,
          lisEnvOptDlgInvalidDebuggerFilenameMsg)
  then exit;

  Result := true;
end;

procedure TDebuggerOptionsForm.FetchDebuggerClass;
var
  n: PtrInt;
  DbgClass, CurClass: TDebuggerClass;
  S: String;
  List: TStringList;
begin
  List := TStringList.Create;
  List.Sorted := True;

  CurClass := nil;
  for n := 0 to DebugBoss.DebuggerCount - 1 do
  begin
    DbgClass := DebugBoss.Debuggers[n];
    List.AddObject(DbgClass.Caption, TObject(n));
    if  (FCurDebuggerClass = nil)
    and (CompareText(DbgClass.ClassName, EnvironmentOptions.DebuggerClass) = 0)
    then CurClass := DbgClass;
  end;

  cmbDebuggerType.Items.Assign(List);
  FreeAndNil(List);

  SetDebuggerClass(CurClass);
  if FCurDebuggerClass = nil
  then SetComboBoxText(cmbDebuggerType, '(none)')
  else SetComboBoxText(cmbDebuggerType, FCurDebuggerClass.Caption);

  with cmbDebuggerPath.Items do begin
    BeginUpdate;
    Assign(EnvironmentOptions.DebuggerFileHistory);
    if  (Count = 0)
    and (FCurDebuggerClass <> nil)
    then begin
      S := FCurDebuggerClass.ExePaths;
      while S <> '' do
      begin
        Add(GetPart([], [';'], S));
        if S <> '' then System.Delete(S, 1, 1);
      end;
    end;
    EndUpdate;
  end;
    
  FOldDebuggerPathAndParams:=EnvironmentOptions.DebuggerFilename;
  SetComboBoxText(cmbDebuggerPath,FOldDebuggerPathAndParams,20);
  txtAdditionalPath.Text:=EnvironmentOptions.DebuggerSearchPath;
end;

procedure TDebuggerOptionsForm.FetchDebuggerGeneralOptions;
begin
  // IMPORTANT if more items are added the indexes must be updated here!
  gcbDebuggerGeneralOptions.Checked[0] := EnvironmentOptions.DebuggerShowStopMessage;
end;

procedure TDebuggerOptionsForm.FetchDebuggerSpecificOptions;
begin
  PropertyGrid.Selection.Clear;
  if FCurDebuggerClass<>nil then begin
    PropertyGrid.Selection.Add(FCurDebuggerClass.GetProperties);
  end;
  PropertyGrid.BuildPropertyList;
end;

function TDebuggerOptionsForm.GetDebuggerClass: TDebuggerClass;
var
  idx: PtrInt;
begin
  Result := nil;

  idx := cmbDebuggerType.ItemIndex;
  if idx = -1 then Exit;
  idx := PtrInt(cmbDebuggerType.Items.Objects[idx]);

  if idx = -1 then Exit;
  Result := DebugBoss.Debuggers[idx];
end;

procedure TDebuggerOptionsForm.SetDebuggerClass(const AClass: TDebuggerClass);
begin
  if FCurDebuggerClass = AClass then Exit;
  FCurDebuggerClass := AClass;
  FetchDebuggerSpecificOptions;
end;

procedure TDebuggerOptionsForm.clbExceptionsCLICK (Sender: TObject );
begin
  cmdExceptionRemove.Enabled := clbExceptions.ItemIndex <> -1;
end;

procedure TDebuggerOptionsForm.cmbDebuggerTypeCHANGE(Sender: TObject);
begin
  SetDebuggerClass(GetDebuggerClass);
end;

procedure TDebuggerOptionsForm.cmdExceptionAddCLICK(Sender: TObject);
var
  idx: Integer;
  S: String;
begin
  if not InputQuery('Add Exception', 'Enter the name of the exception', S)
  then Exit;
  
  if clbExceptions.Items.IndexOf(S) = -1
  then begin
    idx := FExceptionDeleteList.IndexOf(S);
    if idx = -1
    then begin
      AddExceptionLine(nil, S);
    end
    else begin
      AddExceptionLine(TIDEException(FExceptionDeleteList.Objects[idx]), S);
      FExceptionDeleteList.Delete(idx);
    end;
  end
  else begin
    MessageDlg('Duplicate Exception name', mtError, [mbOK], 0);
  end;
end;

procedure TDebuggerOptionsForm.cmdExceptionRemoveCLICK(Sender: TObject);
var
  idx: Integer;
  obj: TObject;
begin
  idx := clbExceptions.ItemIndex;
  if idx <> -1
  then begin
    obj := clbExceptions.Items.Objects[idx];
    if obj <> nil
    then FExceptionDeleteList.AddObject(clbExceptions.Items[idx], obj);
    clbExceptions.Items.Delete(idx);
  end;
  cmdExceptionRemove.Enabled :=  clbExceptions.ItemIndex <> -1;
end;

procedure TDebuggerOptionsForm.cmdOKCLICK(Sender: TObject );
var
  n: Integer;
  ie: TIDEException;
begin
  if not CheckValues then exit;

  for n := 0 to FExceptionDeleteList.Count - 1 do
    FExceptionDeleteList.Objects[n].Free;
    
  for n := 0 to clbExceptions.Items.Count - 1 do
  begin
    ie := TIDEException(clbExceptions.Items.Objects[n]);
    if ie = nil
    then begin
      ie := DebugBoss.Exceptions.Add(clbExceptions.Items[n]);
      ie.Enabled := clbExceptions.Checked[n];
    end
    else begin
      ie.BeginUpdate;
      try
        ie.Name := clbExceptions.Items[n];
        ie.Enabled := clbExceptions.Checked[n];
      finally
        ie.EndUpdate;
      end;
    end;
  end;

  EnvironmentOptions.DebuggerFilename:=cmbDebuggerPath.Text;
  EnvironmentOptions.DebuggerFileHistory.Assign(cmbDebuggerPath.Items);
  EnvironmentOptions.DebuggerSearchPath:=
                                      TrimSearchPath(txtAdditionalPath.Text,'');
  // IMPORTANT if more items are added the indexes must be updated here!
  EnvironmentOptions.DebuggerShowStopMessage := gcbDebuggerGeneralOptions.Checked[0];
  
  if FCurDebuggerClass = nil
  then EnvironmentOptions.DebuggerClass := ''
  else EnvironmentOptions.DebuggerClass := FCurDebuggerClass.ClassName;

  ModalResult:=mrOk;
end;

procedure TDebuggerOptionsForm.cmdOpenAdditionalPathClick(Sender: TObject);
begin
  PathEditorDialog.Path:=txtAdditionalPath.Text;
  PathEditorDialog.Templates:=SetDirSeparators(
        '$(LazarusDir)/include/$(TargetOS)'
      +';$(FPCSrcDir)/rtl/inc/'
      +';$(FPCSrcDir)/rtl/$(SrcOS)'
      +';$(FPCSrcDir)/rtl/$(TargetOS)'
      );
  if PathEditorDialog.ShowModal=mrOk then
    txtAdditionalPath.Text:=PathEditorDialog.Path;
end;

procedure TDebuggerOptionsForm.cmdOpenDebuggerPathCLICK(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    OpenDialog.Title:=lisChooseDebuggerPath;

    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      SetComboBoxText(cmbDebuggerPath,AFilename);
      CheckExecutable(FOldDebuggerPathAndParams,cmbDebuggerPath.Text,
        lisEnvOptDlgInvalidDebuggerFilename,
        lisEnvOptDlgInvalidDebuggerFilenameMsg);
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TDebuggerOptionsForm.DebuggerOptionsFormCREATE(Sender: TObject);
var
  n: Integer;
begin
  //translations
  pgGeneral.Caption := lisMenuInsertGeneral;
  gbDebuggerType.Caption := dlgDebugType;
  gbAdditionalSearchPath.Caption := lisDebugOptionsFrmAdditionalSearchPath;
  gcbDebuggerGeneralOptions.Caption := lisDebugOptionsFrmDebuggerGeneralOptions;
  gcbDebuggerGeneralOptions.Items.Add(lisDebugOptionsFrmShowMessageOnStop);
  gbDebuggerSpecific.Caption := lisDebugOptionsFrmDebuggerSpecific;
  
  pgEventLog.Caption := lisDebugOptionsFrmEventLog;
  gbGeneral.Caption := lisMenuInsertGeneral;
  chkClearLogOnRun.Caption := lisDebugOptionsFrmClearLogOnRun;
  chkLimitLinecount.Caption := lisDebugOptionsFrmLimitLinecountTo;
  gbMessages.Caption := lisMenuViewMessages;
  chkMessagesBreakpoint.Caption := lisDebugOptionsFrmBreakpoint;
  chkMessagesProcess.Caption := lisDebugOptionsFrmProcess;
  chkMessagesThread.Caption := lisDebugOptionsFrmThread;
  chkMessagesModule.Caption := lisDebugOptionsFrmModule;
  chkMessagesOutput.Caption := lisDebugOptionsFrmOutput;
  chkMessagesWindow.Caption := lisDebugOptionsFrmWindow;
  chkMessagesInterface.Caption := lisDebugOptionsFrmInterface;
  
  pgExceptions.Caption := lisDebugOptionsFrmLanguageExceptions;
  bgIgnoreExceptions.Caption := lisDebugOptionsFrmIgnoreTheseExceptions;
  cmdExceptionRemove.Caption := lisExtToolRemove;
  cmdExceptionAdd.Caption := lisCodeTemplAdd;
  chkBreakOnException.Caption := lisDebugOptionsFrmBreakOnLazarusExceptions;
  
  pgSignals.Caption := lisDebugOptionsFrmOSExceptions;
  gbSignals.Caption := lisDebugOptionsFrmSignals;
  lvSignals.Column[0].Caption := lisDebugOptionsFrmName;
  lvSignals.Column[1].Caption := lisDebugOptionsFrmID;
  lvSignals.Column[2].Caption := lisDebugOptionsFrmHandledBy;
  lvSignals.Column[3].Caption := lisDebugOptionsFrmResume;
  cmdSignalAdd.Caption := lisCodeTemplAdd;
  cmdSignalRemove.Caption := lisExtToolRemove;

  mnuHandledByProgram.Caption := lisDebugOptionsFrmHandledByProgram;
  mnuiHandledByDebugger.Caption := lisDebugOptionsFrmHandledByDebugger;
  mnuResumeHandled.Caption := lisDebugOptionsFrmResumeHandled;
  mnuResumeUnhandled.Caption := lisDebugOptionsFrmResumeUnhandled;
  
  cmdOK.Caption:=lisOkBtn;
  cmdCancel.Caption:=dlgCancel;

  FCurDebuggerClass := nil;
  
  FExceptionDeleteList := TStringList.Create;
  FExceptionDeleteList.Sorted := True;

  for n := 0 to DebugBoss.Exceptions.Count - 1 do
  begin
    AddExceptionLine(DebugBoss.Exceptions[n], '');
  end;
  
  for n := 0 to DebugBoss.Signals.Count - 1 do
  begin
    AddSignalLine(DebugBoss.Signals[n]);
  end;

  // create the PropertyEditorHook (the interface to the properties)
  FPropertyEditorHook:=TPropertyEditorHook.Create;
  // create the PropertyGrid
  PropertyGrid:=TOIPropertyGrid.CreateWithParams(Self,FPropertyEditorHook
      ,[tkUnknown, tkInteger, tkChar, tkEnumeration, tkFloat, tkSet{, tkMethod}
      , tkSString, tkLString, tkAString, tkWString, tkVariant
      {, tkArray, tkRecord, tkInterface}, tkClass, tkObject, tkWChar, tkBool
      , tkInt64, tkQWord],
      0);
  with PropertyGrid do begin
    Name:='PropertyGrid';
    // Use panel for border
    Parent := pnlDebugSpecific;   //gbDebuggerSpecific;
    Visible := True;
    Align := alClient;
    Layout := oilVertical;
    RowSpacing := 4;
  end;

  FetchDebuggerClass;
  FetchDebuggerGeneralOptions;

  // Fix designtime changes
  nbDebugOptions.PageIndex := 0;
end;

procedure TDebuggerOptionsForm.DebuggerOptionsFormDESTROY(Sender: TObject);
begin
  FreeAndNil(FExceptionDeleteList);
  FreeAndNil(FPropertyEditorHook);
end;

initialization
  {$I debugoptionsfrm.lrs}

end.

