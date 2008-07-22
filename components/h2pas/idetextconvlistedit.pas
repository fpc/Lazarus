{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    An editor for a list of TCustomTextConverterTool.
}
unit IDETextConvListEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ClipBrd, Buttons, ExtCtrls,
  IDETextConverter, ObjectInspector,
  IDETextConvListAdd;

type

  { TTextConvListEditor }

  TTextConvListEditor = class(TForm)
    AddToolButton: TButton;
    CloneButton: TButton;
    PasteButton: TButton;
    CopyToolButton: TButton;
    MoveToolDownButton: TButton;
    MoveToolUpButton: TButton;
    DeleteToolButton: TButton;
    ToolsSplitter: TSplitter;
    ToolsPanel: TPanel;
    ToolsListBox: TListBox;
    UpDownSplitter: TSplitter;
    ToolsLabel: TLabel;
    PropertyGrid: TCustomPropertiesGrid;
    procedure AddToolButtonClick(Sender: TObject);
    procedure CloneButtonClick(Sender: TObject);
    procedure CopyToolButtonClick(Sender: TObject);
    procedure DeleteToolButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MoveToolDownButtonClick(Sender: TObject);
    procedure MoveToolUpButtonClick(Sender: TObject);
    procedure PasteButtonClick(Sender: TObject);
    procedure PropertyGridModified(Sender: TObject);
    procedure ToolsListBoxSelectionChange(Sender: TObject; User: Boolean);
  private
    FListOfTools: TComponent;
    FModified: boolean;
    FOnModified: TNotifyEvent;
    procedure SetListOfTools(const AValue: TComponent);
    procedure SetModified(const AValue: boolean);
    procedure MoveSelection(Offset: integer);
    function GetCurrentTool: TCustomTextConverterTool;
    procedure MakeToolCaptionAndNameUnique(NewTool: TCustomTextConverterTool);
  public
    procedure SelectTool(Tool: TCustomTextConverterTool);
    function CreateTool(ToolClass: TCustomTextConverterToolClass
                        ): TCustomTextConverterTool;
    procedure UpdateAll;
    procedure UpdateToolsListBox;
    procedure UpdateButtons;
    property ListOfTools: TComponent read FListOfTools write SetListOfTools;
    property Modified: boolean read FModified write SetModified;
    property OnModified: TNotifyEvent read FOnModified write FOnModified;
  end;

var
  TextConvListEditor: TTextConvListEditor;


implementation


{ TTextConvListEditor }

procedure TTextConvListEditor.FormCreate(Sender: TObject);
begin
  Caption:='Text conversion tools editor';
  ToolsLabel.Caption:='Tools:';
  
  // buttons
  AddToolButton.Caption:='Add new tool';
  CloneButton.Caption:='Add a copy';
  PasteButton.Caption:='Add from clipboard';
  CopyToolButton.Caption:='Copy tool to clipboard';
  MoveToolDownButton.Caption:='Move down';
  MoveToolUpButton.Caption:='Move up';
  DeleteToolButton.Caption:='Delete tool';

  PropertyGrid:=TCustomPropertiesGrid.Create(Self);
  PropertyGrid.Align:=alBottom;
  PropertyGrid.AnchorToNeighbour(akTop,0,UpDownSplitter);
  PropertyGrid.Parent:=Self;
  PropertyGrid.OnModified:=@PropertyGridModified;
  
  UpdateButtons;
end;

procedure TTextConvListEditor.MoveToolDownButtonClick(Sender: TObject);
begin
  MoveSelection(+1);
end;

procedure TTextConvListEditor.MoveToolUpButtonClick(Sender: TObject);
begin
  MoveSelection(-1);
end;

procedure TTextConvListEditor.PasteButtonClick(Sender: TObject);
var
  NewComponent: TComponent;
  NewTool: TCustomTextConverterTool;
begin
  if FListOfTools=nil then exit;
  try
    NewComponent:=nil;
    Clipboard.GetComponentAsText(NewComponent,
                              @TextConverterToolClasses.FindClass,FListOfTools);
    if NewComponent=nil then
      raise Exception.Create('nil');
    if not (NewComponent is TCustomTextConverterTool) then begin
      NewComponent.Free;
      raise Exception.Create('not a TCustomTextConverterTool');
    end;
    NewTool:=TCustomTextConverterTool(NewComponent);
    MakeToolCaptionAndNameUnique(NewTool);
    Modified:=true;
    UpdateToolsListBox;
    SelectTool(NewTool);
  except
    on E: Exception do begin
      MessageDlg('Error',
        'Error converting clipboard text to text tool:'#13
        +E.Message,mtError,[mbCancel],0);
    end;
  end;
end;

procedure TTextConvListEditor.PropertyGridModified(Sender: TObject);
var
  Tool: TCustomTextConverterTool;
begin
  Tool:=GetCurrentTool;
  //DebugLn(['TTextConvListEditor.PropertyGridModified ',dbgsName(Tool)]);
  if Tool=nil then exit;
  MakeToolCaptionAndNameUnique(Tool);
  Modified:=true;
end;

procedure TTextConvListEditor.ToolsListBoxSelectionChange(Sender: TObject;
  User: Boolean);
var
  Tool: TCustomTextConverterTool;
begin
  if User then ;
  if csDestroying in ComponentState then exit;
  UpdateButtons;
  Tool:=GetCurrentTool;
  //DebugLn(['TTextConvListEditor.ToolsListBoxSelectionChange Tool=',dbgsName(Tool)]);
  PropertyGrid.TIObject:=Tool;
end;

procedure TTextConvListEditor.AddToolButtonClick(Sender: TObject);
var
  ToolClass: TCustomTextConverterToolClass;
begin
  if FListOfTools=nil then exit;
  if ShowIDETextConvListAddDlg(ToolClass)<>mrOk then exit;
  CreateTool(ToolClass);
end;

procedure TTextConvListEditor.CloneButtonClick(Sender: TObject);
var
  Tool: TCustomTextConverterTool;
  NewTool: TCustomTextConverterTool;
begin
  Tool:=GetCurrentTool;
  if Tool=nil then exit;
  NewTool:=TCustomTextConverterToolClass(Tool.ClassType).Create(FListOfTools);
  NewTool.Assign(Tool);
  MakeToolCaptionAndNameUnique(NewTool);
  Modified:=true;
  UpdateToolsListBox;
  SelectTool(NewTool);
end;

procedure TTextConvListEditor.CopyToolButtonClick(Sender: TObject);
var
  Tool: TCustomTextConverterTool;
begin
  Tool:=GetCurrentTool;
  if Tool=nil then exit;
  try
    Clipboard.SetComponentAsText(Tool);
  except
    on E: Exception do begin
      MessageDlg('Error',
        'Error converting putting tool onto clipboard:'#13
        +E.Message,mtError,[mbCancel],0);
    end;
  end;
end;

procedure TTextConvListEditor.DeleteToolButtonClick(Sender: TObject);
var
  Tool: TCustomTextConverterTool;
  i: LongInt;
begin
  Tool:=GetCurrentTool;
  if Tool=nil then exit;
  if QuestionDlg('Confirm delete',
    'Do you really want to delete "'+Tool.Caption+'"?',
    mtConfirmation,[mrYes,'Delete',mrCancel],0
    )<>mrYes
  then exit;
  i:=ToolsListBox.ItemIndex;
  PropertyGrid.TIObject:=nil;
  Tool.Free;
  Modified:=true;
  UpdateToolsListBox;
  if i>=ToolsListBox.Items.Count then
    i:=ToolsListBox.Items.Count-1;
  ToolsListBox.ItemIndex:=i;
end;

procedure TTextConvListEditor.SetListOfTools(const AValue: TComponent);
begin
  if (FListOfTools=AValue) then exit;
  FListOfTools:=AValue;
  PropertyGrid.TIObject:=nil;
  UpdateAll;
end;

procedure TTextConvListEditor.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
  if FModified and Assigned(OnModified) then OnModified(Self);
end;

procedure TTextConvListEditor.UpdateAll;
begin
  UpdateToolsListBox;
  UpdateButtons;
end;

procedure TTextConvListEditor.UpdateToolsListBox;
var
  sl: TStringList;
  i: Integer;
  Tool: TCustomTextConverterTool;
  OldSelected: String;
begin
  sl:=TStringList.Create;
  if FListOfTools<>nil then begin
    for i:=0 to FListOfTools.ComponentCount-1 do begin
      Tool:=FListOfTools.Components[i] as TCustomTextConverterTool;
      sl.Add(Tool.Caption);
      //DebugLn(['TTextConvListEditor.UpdateToolsListBox Caption=',Tool.Caption,' ',dbgsName(Tool)]);
    end;
  end;
  //DebugLn(['TTextConvListEditor.UpdateToolsListBox ',sl.Count,' "',sl.Text,'"']);
  // save selection
  OldSelected:='';
  if ToolsListBox.ItemIndex>=0 then
    OldSelected:=ToolsListBox.Items[ToolsListBox.ItemIndex];
  // commit new list
  ToolsListBox.Items.Assign(sl);
  // restore selection
  if OldSelected<>'' then
    ToolsListBox.ItemIndex:=ToolsListBox.Items.IndexOf(OldSelected);
  sl.Free;
end;

procedure TTextConvListEditor.UpdateButtons;
var
  i: LongInt;
begin
  i:=ToolsListBox.ItemIndex;
  DeleteToolButton.Enabled:=(i>=0);
  MoveToolDownButton.Enabled:=(i<ToolsListBox.Items.Count-1);
  MoveToolUpButton.Enabled:=(i>0);
  CloneButton.Enabled:=(i>=0);
  PasteButton.Enabled:=true;
  CopyToolButton.Enabled:=(i>=0);
end;

procedure TTextConvListEditor.MoveSelection(Offset: integer);
var
  i: LongInt;
  Tool: TCustomTextConverterTool;
begin
  if FListOfTools=nil then exit;
  if Offset=0 then exit;
  i:=ToolsListBox.ItemIndex;
  if (i>=0) and (i<FListOfTools.ComponentCount)
  and (i+Offset>=0) and (i+Offset<FListOfTools.ComponentCount) then begin
    Tool:=FListOfTools.Components[i] as TCustomTextConverterTool;
    Tool.ComponentIndex:=Tool.ComponentIndex+Offset;
    Modified:=true;
    UpdateToolsListBox;
  end;
end;

function TTextConvListEditor.GetCurrentTool: TCustomTextConverterTool;
var
  i: LongInt;
begin
  Result:=nil;
  if FListOfTools=nil then exit;
  i:=ToolsListBox.ItemIndex;
  //DebugLn(['TTextConvListEditor.GetCurrentTool ',dbgsName(Self),' ToolsListBox.ItemIndex=',ToolsListBox.ItemIndex,' FListOfTools.ComponentCount=',FListOfTools.ComponentCount]);
  if (i<0) or (i>=FListOfTools.ComponentCount) then exit;
  Result:=TCustomTextConverterTool(FListOfTools.Components[i]);
end;

procedure TTextConvListEditor.MakeToolCaptionAndNameUnique(
  NewTool: TCustomTextConverterTool);
var
  i: Integer;
begin
  MakeToolNameUnique(FListOfTools,NewTool);
  MakeToolCaptionUnique(FListOfTools,NewTool);
  if (FListOfTools<>nil) then begin
    for i:=0 to FListOfTools.ComponentCount-1 do begin
      if FListOfTools.Components[i]=NewTool then begin
        if (i<ToolsListBox.Items.Count) then begin
          ToolsListBox.Items[i]:=NewTool.Caption;
        end;
      end;
    end;
  end;
end;

procedure TTextConvListEditor.SelectTool(Tool: TCustomTextConverterTool);
var
  i: LongInt;
begin
  if FListOfTools=nil then exit;
  if Tool.Owner<>FListOfTools then exit;
  i:=Tool.ComponentIndex;
  if (i<=0) or (i>=ToolsListBox.Items.Count) then exit;
  ToolsListBox.ItemIndex:=i;
end;

function TTextConvListEditor.CreateTool(ToolClass: TCustomTextConverterToolClass
  ): TCustomTextConverterTool;
begin
  Result:=nil;
  if FListOfTools=nil then exit;
  Result:=AddNewTextConverterTool(FListOfTools,ToolClass);
  Modified:=true;
  UpdateToolsListBox;
  SelectTool(Result);
end;

initialization
  {$I idetextconvlistedit.lrs}

end.

