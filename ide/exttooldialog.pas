{
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

  Author: Mattias Gaertner

  Abstract:
    Defines the TExternalToolList which stores the settings of all external
    tools. (= Programfilename and parameters)
    And provides TExternalToolDlg which is a dialog for editing a
    TExternalToolList;
}
unit ExtToolDialog;

{$mode objfpc}
{$H+}

{$I ide.inc}

interface

uses
  {$IFDEF IDE_MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, LCLType, Controls, Forms, Buttons, StdCtrls, ComCtrls, 
  Dialogs, ExtCtrls, LResources, Laz_XMLCfg, ExtToolEditDlg, Process,
  KeyMapping, TransferMacros, IDEProcs, OutputFilter, FileCtrl;

const
  MaxExtTools = ecExtToolLast-ecExtToolFirst+1;

type
  TOnNeedsOutputFilter = procedure(var OutputFilter: TOutputFilter;
                           var Abort: boolean) of object;
  TOnFreeOutputFilter = procedure(OutputFilter: TOutputFilter;
                           ErrorOccurred: boolean) of object;

  {
    the storage object for all external tools
  }
  TExternalToolList = class(TList)
  private
    fOnFreeOutputFilter: TOnFreeOutputFilter;
    fOnNeedsOutputFilter: TOnNeedsOutputFilter;
    fRunningTools: TList; // list of TProcess
    function GetToolOpts(Index: integer): TExternalToolOptions;
    procedure SetToolOpts(Index: integer; NewTool: TExternalToolOptions);
    procedure AddRunningTool(TheProcess: TProcess; ExecuteProcess: boolean);
  public
    procedure Add(NewTool: TExternalToolOptions);
    procedure Assign(Source: TExternalToolList);
    procedure Clear; override;
    constructor Create;
    procedure Delete(Index: integer); 
    destructor Destroy; override;
    procedure FreeStoppedProcesses;
    procedure Insert(Index: integer; NewTool: TExternalToolOptions);
    function Load(XMLConfig: TXMLConfig; const Path: string): TModalResult;
    procedure LoadShortCuts(KeyCommandRelationList: TKeyCommandRelationList);
    function Run(ExtTool: TExternalToolOptions;
      Macros: TTransferMacroList): TModalResult;
    function Run(Index: integer; Macros: TTransferMacroList): TModalResult;
    function Save(XMLConfig: TXMLConfig; const Path: string): TModalResult;
    procedure SaveShortCuts(KeyCommandRelationList: TKeyCommandRelationList);
    
    property Items[Index: integer]: TExternalToolOptions
      read GetToolOpts write SetToolOpts; default;
    property OnFreeOutputFilter: TOnFreeOutputFilter
      read fOnFreeOutputFilter write fOnFreeOutputFilter;
    property OnNeedsOutputFilter: TOnNeedsOutputFilter
      read fOnNeedsOutputFilter write fOnNeedsOutputFilter;
  end;
  
  {
    the dialog to edit all external tools
  }
  TExternalToolDialog = class(TForm)
    Listbox: TListbox;
    AddButton: TButton;
    RemoveButton: TButton;
    EditButton: TButton;
    MoveUpButton: TButton;
    MoveDownButton: TButton;
    OkButton: TButton;
    CancelButton: TButton;
    procedure ExternalToolDialogResize(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure RemoveButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure MoveUpButtonClick(Sender: TObject);
    procedure MoveDownButtonClick(Sender: TObject);
    procedure ListboxClick(Sender: TObject);
  private
    fExtToolList: TExternalToolList;
    fTransferMacros: TTransferMacroList;
    procedure Load;
    procedure SetExtToolList(NewExtToolList: TExternalToolList);
    procedure SetTransferMacros(NewMacros: TTransferMacroList);
    function ToolDescription(Index: integer): string;
    procedure EnableButtons;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    property ExtToolList: TExternalToolList 
           read fExtToolList write SetExtToolList;
    property TransferMacros: TTransferMacroList
           read fTransferMacros write SetTransferMacros;
  end;
  

function ShowExtToolDialog(ExtToolList: TExternalToolList;
  TransferMacros: TTransferMacroList):TModalResult;


implementation


function ShowExtToolDialog(ExtToolList: TExternalToolList;
  TransferMacros: TTransferMacroList):TModalResult;
var ExternalToolDialog: TExternalToolDialog;
begin
  Result:=mrCancel;
  ExternalToolDialog:=TExternalToolDialog.Create(Application);
  try
    ExternalToolDialog.TransferMacros:=TransferMacros;
    ExternalToolDialog.ExtToolList:=ExtToolList;
    Result:=ExternalToolDialog.ShowModal;
    if Result=mrOk then
      ExtToolList.Assign(ExternalToolDialog.ExtToolList);
  finally
    ExternalToolDialog.Free;
  end;
end;


{ TExternalToolList }

function TExternalToolList.GetToolOpts(Index: integer): TExternalToolOptions;
begin
  Result:=TExternalToolOptions(inherited Items[Index]);
end;

procedure TExternalToolList.SetToolOpts(Index: integer; 
  NewTool: TExternalToolOptions);
begin
  inherited Items[Index]:=NewTool;
end;

procedure TExternalToolList.Add(NewTool: TExternalToolOptions);
begin
  inherited Add(NewTool);
end;

procedure TExternalToolList.Assign(Source: TExternalToolList);
var i: integer;
begin
  if Source=Self then exit;
  Clear;
  if Source=nil then exit;
  Count:=Source.Count;
  for i:=0 to Count-1 do begin
    Items[i]:=TExternalToolOptions.Create;
    Items[i].Assign(Source[i]);
  end;
end;

constructor TExternalToolList.Create;
begin
  inherited Create;
  Clear;
end;

procedure TExternalToolList.Delete(Index: integer); 
begin
  Items[Index].Free;
  inherited Delete(Index);
end;

destructor TExternalToolList.Destroy;
begin
  if fRunningTools<>nil then
    fRunningTools.Free;
  inherited Destroy;
end;

procedure TExternalToolList.Clear; 
var i: integer;
begin
  for i:=0 to Count-1 do
    TExternalToolOptions(Items[i]).Free;
  inherited Clear;
end;

procedure TExternalToolList.Insert(Index: integer;
  NewTool: TExternalToolOptions);
begin
  inherited Insert(Index,NewTool);
end;

function TExternalToolList.Load(XMLConfig: TXMLConfig;
  const Path: string): TModalResult;
var i: integer;
  NewTool: TExternalToolOptions;
begin
  Clear;
  Count:=XMLConfig.GetValue(Path+'Count',0);
  for i:=0 to Count-1 do begin
    NewTool:=TExternalToolOptions.Create;
    Items[i]:=NewTool;
    if NewTool.Load(XMLConfig,Path+'Tool'+IntToStr(i+1)+'/')<>mrOk then exit;
  end;
  Result:=mrOk;
end;

procedure TExternalToolList.LoadShortCuts(
  KeyCommandRelationList: TKeyCommandRelationList);
var i: integer;
  KeyCommandRelation: TKeyCommandRelation;
begin
  for i:=0 to Count-1 do begin
    KeyCommandRelation:=KeyCommandRelationList.FindByCommand(ecExtToolFirst+i);
    if KeyCommandRelation<>nil then begin
      Items[i].Key:=KeyCommandRelation.Key1;
      Items[i].Shift:=KeyCommandRelation.Shift1;
    end else begin
      Items[i].Key:=VK_UNKNOWN;
      Items[i].Shift:=[];
    end;
  end;
end;

function TExternalToolList.Run(Index: integer;
  Macros: TTransferMacroList): TModalResult;
begin
  Result:=mrCancel;
  if (Index<0) or (Index>=Count) then exit;
  Run(Items[Index],Macros);
end;

function TExternalToolList.Run(ExtTool: TExternalToolOptions;
  Macros: TTransferMacroList): TModalResult;
var WorkingDir, Filename, Params, CmdLine, Title: string;
  TheProcess: TProcess;
  TheOutputFilter: TOutputFilter;
  Abort, ErrorOccurred: boolean;
begin
  Result:=mrCancel;
  if ExtTool=nil then exit;
  Filename:=ExtTool.Filename;
  WorkingDir:=ExtTool.WorkingDirectory;
  Params:=ExtTool.CmdLineParams;
  Title:=ExtTool.Title;
  if Title='' then Title:=Filename;
  if Macros.SubstituteStr(Filename) 
  and Macros.SubstituteStr(WorkingDir)
  and Macros.SubstituteStr(Params) then begin
    CmdLine:=Filename;
    if Params<>'' then 
      CmdLine:=CmdLine+' '+Params;
writeln('[TExternalToolList.Run] ',CmdLine);
    try
      CheckIfFileIsExecutable(Filename);
      TheProcess := TProcess.Create(nil);
      TheProcess.CommandLine := Filename+' '+Params;
      TheProcess.Options:= [poUsePipes, poNoConsole,poStderrToOutPut];
      TheProcess.ShowWindow := swoNone;
      TheProcess.CurrentDirectory := WorkingDir;
      if ExtTool.EnvironmentOverrides.Count>0 then
        ExtTool.AssignEnvironmentTo(TheProcess.Environment);
      if (ExtTool.NeedsOutputFilter)
      and Assigned(OnNeedsOutputFilter) then begin
        Abort:=false;
        OnNeedsOutputFilter(TheOutputFilter,Abort);
        if Abort then begin
          Result:=mrAbort;
          exit;
        end;
        ErrorOccurred:=false;
        try
          TheOutputFilter.PrgSourceFilename:='';
          TheOutputFilter.Options:=[ofoExceptionOnError,
                                    ofoMakeFilenamesAbsolute];
          if ExtTool.ScanOutputForFPCMessages then
            TheOutputFilter.Options:=TheOutputFilter.Options
                                     +[ofoSearchForFPCMessages];
          if ExtTool.ScanOutputForMakeMessages then
            TheOutputFilter.Options:=TheOutputFilter.Options
                                     +[ofoSearchForMakeMessages];
          try
            try
              TheOutputFilter.Execute(TheProcess);
              TheOutputFilter.ReadLine('"'+Title+'" successfully runned :)',
                                       true);
            finally
              TheProcess.WaitOnExit;
              TheProcess.Free;
            end;
            Result:=mrOk;
          except
            on e: EOutputFilterError do begin
              writeln('TExternalToolList.Run ',E.Message);
              ErrorOccurred:=true;
            end
            else
              raise
          end;
        finally
          if Assigned(OnFreeOutputFilter) then
            OnFreeOutputFilter(TheOutputFilter,ErrorOccurred);
        end;
      end else begin
        AddRunningTool(TheProcess,true);
        Result:=mrOk;
      end;
    except
      on e: Exception do
        MessageDlg('Failed to run tool',
          'Unable to run the tool "'+Title+'":'#13+e.Message,mtError,[mbOk],0);
    end;
  end;
end;

function TExternalToolList.Save(XMLConfig: TXMLConfig;
  const Path: string): TModalResult;
var i: integer;
begin
  XMLConfig.SetValue(Path+'Count',Count);
  for i:=0 to Count-1 do begin
    if Items[i].Save(XMLConfig,Path+'Tool'+IntToStr(i+1)+'/')<>mrOk then exit;
  end;
  Result:=mrOk;
end;

procedure TExternalToolList.SaveShortCuts(
  KeyCommandRelationList: TKeyCommandRelationList);
var i: integer;
  KeyCommandRelation: TKeyCommandRelation;
begin
  KeyCommandRelationList.ExtToolCount:=Count;
  for i:=0 to Count-1 do begin
    KeyCommandRelation:=KeyCommandRelationList.FindByCommand(ecExtToolFirst+i);
    if KeyCommandRelation<>nil then begin
      KeyCommandRelation.Key1:=Items[i].Key;
      KeyCommandRelation.Shift1:=Items[i].Shift;
    end else begin
      writeln('[TExternalToolList.SaveShortCuts] Error: '
        +'unable to save shortcut for external tool "',Items[i].Title,'"');
    end;
  end;
end;

procedure TExternalToolList.AddRunningTool(TheProcess: TProcess;
  ExecuteProcess: boolean);
begin
  if fRunningTools=nil then fRunningTools:=TList.Create;
  fRunningTools.Add(TheProcess);
  if ExecuteProcess then
    TheProcess.Execute;
end;

procedure TExternalToolList.FreeStoppedProcesses;
var i: integer;
  TheProcess: TProcess;
begin
  if fRunningTools=nil then exit;
  i:=fRunningTools.Count-1;
  while i>=0 do begin
    try
      TheProcess:=TProcess(fRunningTools[i]);
      if not TheProcess.Running then begin
        try
          TheProcess.WaitOnExit;
          TheProcess.Free;
        finally
          fRunningTools.Delete(i);
        end;
      end;
    except
      on E: Exception do begin
        writeln('Error freeing stopped process: ',E.Message);
      end;
    end;
    dec(i);
  end;
end;


{ TExternalToolDialog }

constructor TExternalToolDialog.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Width:=400;
    Height:=400;
    Position:=poScreenCenter;
    Caption:='External Tools';
    OnResize:=@ExternalToolDialogResize;

    Listbox:=TListbox.Create(Self);
    with Listbox do begin
      Name:='Listbox';
      Parent:=Self;
      SetBounds(5,5,Self.ClientWidth-120,Self.Clientheight-60);
      OnClick:=@ListboxClick;
      Visible:=true; 
    end;
    
    AddButton:=TButton.Create(Self);
    with AddButton do begin
      Name:='AddButton';
      Parent:=Self;
      SetBounds(Self.ClientWidth-100,5,80,25);
      Caption:='Add';
      OnClick:=@AddButtonClick;
      Visible:=true; 
    end;
    
    RemoveButton:=TButton.Create(Self);
    with RemoveButton do begin
      Name:='RemoveButton';
      Parent:=Self;
      SetBounds(Self.ClientWidth-100,AddButton.Top+AddButton.Height+10,80,25);
      Caption:='Remove';
      OnClick:=@RemoveButtonClick;
      Visible:=true; 
    end;
    
    EditButton:=TButton.Create(Self);
    with EditButton do begin
      Name:='EditButton';
      Parent:=Self;
      SetBounds(Self.ClientWidth-100,RemoveButton.Top+RemoveButton.Height+10,
                   80,25);
      Caption:='Edit';
      OnClick:=@EditButtonClick;
      Visible:=true; 
    end;
    
    MoveUpButton:=TButton.Create(Self);
    with MoveUpButton do begin
      Name:='MoveUpButton';
      Parent:=Self;
      SetBounds(Self.ClientWidth-100,EditButton.Top+EditButton.Height+50,
                   80,25);
      Caption:='Move Up';
      OnClick:=@MoveUpButtonClick;
      Visible:=true; 
    end;
    
    MoveDownButton:=TButton.Create(Self);
    with MoveDownButton do begin
      Name:='MoveDownButton';
      Parent:=Self;
      SetBounds(Self.ClientWidth-100,MoveUpButton.Top+MoveUpButton.Height+10,
                   80,25);
      Caption:='Move Down';
      OnClick:=@MoveDownButtonClick;
      Visible:=true; 
    end;
    
    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      SetBounds(Self.ClientWidth-200, Self.ClientHeight-40,80,25);
      Caption:='Ok';
      OnClick:=@OkButtonClick;
      Visible:=true; 
    end;
    
    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      SetBounds(Self.ClientWidth-100, Self.ClientHeight-40,80,25);
      Caption:='Cancel';
      OnClick:=@CancelButtonClick;
      Visible:=true; 
    end;
  end;    
  fExtToolList:=TExternalToolList.Create;
  ExternalToolDialogResize(nil);
end;

destructor TExternalToolDialog.Destroy;
begin
  fExtToolList.Free;
  inherited Destroy;
end;

procedure TExternalToolDialog.ExternalToolDialogResize(Sender: TObject);
begin
  with Listbox do begin
    SetBounds(5,5,Self.ClientWidth-120,Self.Clientheight-60);
  end;

  with AddButton do begin
    SetBounds(Self.ClientWidth-100,5,80,25);
  end;

  with RemoveButton do begin
    SetBounds(Self.ClientWidth-100,AddButton.Top+AddButton.Height+10,80,25);
  end;

  with EditButton do begin
    SetBounds(Self.ClientWidth-100,RemoveButton.Top+RemoveButton.Height+10,
                 80,25);
  end;

  with MoveUpButton do begin
    SetBounds(Self.ClientWidth-100,EditButton.Top+EditButton.Height+50,
                 80,25);
  end;

  with MoveDownButton do begin
    SetBounds(Self.ClientWidth-100,MoveUpButton.Top+MoveUpButton.Height+10,
                 80,25);
  end;

  with OkButton do begin
    SetBounds(Self.ClientWidth-200, Self.ClientHeight-40,80,25);
  end;

  with CancelButton do begin
    SetBounds(Self.ClientWidth-100, Self.ClientHeight-40,80,25);
  end;
end;

procedure TExternalToolDialog.OkButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TExternalToolDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TExternalToolDialog.SetExtToolList(NewExtToolList: TExternalToolList);
begin
  if fExtToolList=NewExtToolList then exit;
  fExtToolList.Assign(NewExtToolList);
  Load;
end;

procedure TExternalToolDialog.SetTransferMacros(NewMacros: TTransferMacroList);
begin
  if fTransferMacros=NewMacros then exit;
  fTransferMacros:=NewMacros;
end;

function TExternalToolDialog.ToolDescription(Index: integer): string;
begin
  Result:=fExtToolList[Index].ShortDescription;
end;

procedure TExternalToolDialog.Load;
var i: integer;
begin
  Listbox.Items.BeginUpdate;
  Listbox.Items.Clear;
  for i:=0 to fExtToolList.Count-1 do 
    Listbox.Items.Add(ToolDescription(i));
  Listbox.Items.EndUpdate;
  EnableButtons;
end;

procedure TExternalToolDialog.AddButtonClick(Sender: TObject);
var NewTool: TExternalToolOptions;
begin
  if fExtToolList.Count>=MaxExtTools then begin
    MessageDlg('Maximum Tools reached',
                  'There is a maximum of '+IntToStr(MaxExtTools)+' tools.',
                  mtInformation,[mbCancel],0);
    exit;
  end;
  NewTool:=TExternalToolOptions.Create;
  if ShowExtToolOptionDlg(fTransferMacros,NewTool)=mrOk then begin
    fExtToolList.Add(NewTool);
    Listbox.Items.Add(ToolDescription(fExtToolList.Count-1));
  end else begin
    NewTool.Free;
  end;
  EnableButtons;
end;

procedure TExternalToolDialog.RemoveButtonClick(Sender: TObject);
begin
  if Listbox.ItemIndex<0 then exit;
  fExtToolList.Delete(Listbox.ItemIndex);
  ListBox.Items.Delete(Listbox.ItemIndex);
  EnableButtons;
end;

procedure TExternalToolDialog.EditButtonClick(Sender: TObject);
begin
  if Listbox.ItemIndex<0 then exit;
  if ShowExtToolOptionDlg(fTransferMacros,fExtToolList[Listbox.ItemIndex])=mrOk
  then begin
    Listbox.Items[Listbox.ItemIndex]:=ToolDescription(Listbox.ItemIndex);
    EnableButtons;
  end;
end;

procedure TExternalToolDialog.MoveUpButtonClick(Sender: TObject);
var i: integer;
begin
  i:=Listbox.ItemIndex;
  if i<1 then exit;
  fExtToolList.Move(i,i-1);
  Listbox.Items.Move(i,i-1);
  Listbox.ItemIndex:=i-1;
  EnableButtons;
end;

procedure TExternalToolDialog.MoveDownButtonClick(Sender: TObject);
var i: integer;
begin
  i:=Listbox.ItemIndex;
  if (i<0) or (i>=Listbox.Items.Count-1) then exit;
  fExtToolList.Move(i,i+1);
  Listbox.Items.Move(i,i+1);
  Listbox.ItemIndex:=i+1;
  EnableButtons;
end;

procedure TExternalToolDialog.EnableButtons;
var i: integer;
begin
  i:=Listbox.ItemIndex;
  AddButton.Enabled:=fExtToolList.Count<MaxExtTools;
  RemoveButton.Enabled:=(i>=0);
  EditButton.Enabled:=(i>=0);
  MoveUpButton.Enabled:=(i>0);
  MoveDownButton.Enabled:=(i>=0) and (i<fExtToolList.Count-1);
end;

procedure TExternalToolDialog.ListboxClick(Sender: TObject);
begin
  EnableButtons;
end;

end.
