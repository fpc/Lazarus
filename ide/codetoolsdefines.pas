{ /***************************************************************************
                 codetoolsoptions.pas  -  Lazarus IDE unit
                 -----------------------------------------

 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

  Author: Mattias Gaertner

  Abstract:
    - TCodeToolsDefinesEditor
}
unit CodeToolsDefines;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLLinux, Forms, Controls, Buttons, StdCtrls, ComCtrls,
  ExtCtrls, Menus, LResources, SynEdit, DefineTemplates, CodeToolManager,
  CodeToolsOptions;

type
  TCodeToolsDefinesEditor = class(TForm)
    TheImageList: TImageList;
    MainMenu: TMainMenu;
    
    // exit menu
    ExitMenuItem: TMenuItem;
    SaveAndExitMenuItem: TMenuItem;
    DontSaveAndExitMenuItem: TMenuItem;

    // edit nodes
    EditMenuItem: TMenuItem;
    MoveNodeUpMenuItem: TMenuItem;
    MoveNodeDownMenuItem: TMenuItem;
    InsertDefineMenuItem: TMenuItem;
    InsertDefineAllMenuItem: TMenuItem;
    InsertUndefineMenuItem: TMenuItem;
    InsertBlockMenuItem: TMenuItem;
    InsertDirectoryMenuItem: TMenuItem;
    InsertIfMenuItem: TMenuItem;
    InsertIfDefMenuItem: TMenuItem;
    InsertElseMenuItem: TMenuItem;
    InsertEndIfMenuItem: TMenuItem;
    DeleteNodeMenuItem: TMenuItem;
    CopyToClipbrdMenuItem: TMenuItem;
    PasteFromClipbrdMenuItem: TMenuItem;

    // tools
    ToolsMenuItem: TMenuItem;
    OpenPreviewMenuItem: TMenuItem;
    ShowMacroListMenuItem: TMenuItem;

    // templates
    InsertTemplateMenuItem: TMenuItem;

    // define tree
    DefineTreeView: TTreeView;

    // selected item
    SelectedItemGroupBox: TGroupBox;
    TypeLabel: TLabel;
    ProjectSpecificCheckBox: TCheckBox;
    NameLabel: Tlabel;
    NameEdit: TEdit;
    DescriptionLabel: TLabel;
    DescriptionEdit: TEdit;
    VariableLabel: TLabel;
    VariableEdit: TEdit;
    ValueNoteBook: TNoteBook;
    ValueAsTextSynEdit: TSynEdit;
    ValueAsFilePathsSynEdit: TSynEdit;
    MoveFilePathUpBitBtn: TBitBtn;
    MoveFilePathDownBitBtn: TBitBtn;
    DeleteFilePathBitBtn: TBitBtn;
    InsertFilePathBitBtn: TBitBtn;

    procedure SaveAndExitMenuItemClick(Sender: TObject);
    procedure DontSaveAndExitMenuItemClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FDefineTree: TDefineTree;
    procedure CreateComponents;
    function CreateSeperator : TMenuItem;
    procedure RebuildDefineTreeView;
    procedure AddDefineNodes(ANode, AParent: TDefineTemplate;
      WithChilds,WithNextSiblings: boolean);
  public
    procedure Assign(ACodeToolBoss: TCodeToolManager;
      Options: TCodeToolsOptions);
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property DefineTree: TDefineTree read FDefineTree;
  end;

function ShowCodeToolsDefinesEditor(ACodeToolBoss: TCodeToolManager;
  Options: TCodeToolsOptions): TModalResult;


implementation


type
  TWinControlClass = class of TWinControl;

function ShowCodeToolsDefinesEditor(ACodeToolBoss: TCodeToolManager;
  Options: TCodeToolsOptions): TModalResult;
var CodeToolsDefinesEditor: TCodeToolsDefinesEditor;
begin
  CodeToolsDefinesEditor:=TCodeToolsDefinesEditor.Create(Application);
  CodeToolsDefinesEditor.Assign(ACodeToolBoss,Options);
  Result:=CodeToolsDefinesEditor.ShowModal;
  CodeToolsDefinesEditor.Free;
end;

{ TCodeToolsDefinesEditor }

procedure TCodeToolsDefinesEditor.SaveAndExitMenuItemClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TCodeToolsDefinesEditor.DontSaveAndExitMenuItemClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TCodeToolsDefinesEditor.FormResize(Sender: TObject);
var MaxX, MaxY, SelGrpBoxTop, SelItemMaxX, SelItemMaxY,
  ValNoteBookMaxX, ValNoteBookMaxY: integer;
begin
  MaxX:=ClientWidth-2;
  MaxY:=ClientHeight-2;
  SelGrpBoxTop:=MaxY-300;

  // define tree ---------------------------------------------------------------
  with DefineTreeView do begin
    Left:=3;
    Top:=3;
    Width:=MaxX-2*Left;
    Height:=SelGrpBoxTop-2*Top;
  end;

  // selected item -------------------------------------------------------------
  with SelectedItemGroupBox do begin
    Left:=DefineTreeView.Left;
    Top:=SelGrpBoxTop;
    Width:=MaxX-2*Left;
    Height:=MaxY-Top-30;
  end;
  SelItemMaxX:=SelectedItemGroupBox.ClientWidth-8;
  SelItemMaxY:=SelectedItemGroupBox.ClientHeight-18;
  with TypeLabel do begin
    Left:=5;
    Top:=3;
    Width:=SelItemMaxX-2*Left;
  end;
  with ProjectSpecificCheckBox do begin
    Left:=TypeLabel.Left;
    Top:=TypeLabel.Top+TypeLabel.Height+5;
    Width:=SelItemMaxX-2*Left;
  end;
  with DescriptionLabel do begin
    Left:=ProjectSpecificCheckBox.Left;
    Top:=ProjectSpecificCheckBox.Top+ProjectSpecificCheckBox.Height+5;
    Width:=70;
  end;
  with DescriptionEdit do begin
    Left:=DescriptionLabel.Left+DescriptionLabel.Width+5;
    Top:=DescriptionLabel.Top;
    Width:=SelItemMaxX-Left-5;
  end;
  with NameLabel do begin
    Left:=DescriptionLabel.Left;
    Top:=DescriptionLabel.Top+DescriptionLabel.Height+5;
    Width:=70;
  end;
  with NameEdit do begin
    Left:=NameLabel.Left+NameLabel.Width+5;
    Top:=NameLabel.Top;
    Width:=150;
  end;
  with VariableLabel do begin
    Left:=NameEdit.Left+NameEdit.Width+30;
    Top:=NameLabel.Top;
    Width:=70;
  end;
  with VariableEdit do begin
    Left:=VariableLabel.Left+VariableLabel.Width+5;
    Top:=VariableLabel.Top;
    Width:=SelItemMaxX-Left-5;
  end;
  with ValueNoteBook do begin
    Left:=0;
    Top:=VariableLabel.Top+VariableLabel.Height+8;
    Width:=SelItemMaxX;
    Height:=SelItemMaxY-Top-5;
  end;
  ValNoteBookMaxX:=ValueNoteBook.ClientWidth-7;//ValueAsTextSynEdit.Parent.ClientWidth;
  ValNoteBookMaxY:=ValueNoteBook.ClientHeight-32;//ValueAsTextSynEdit.Parent.ClientHeight;
  with ValueAsTextSynEdit do begin
    Left:=0;
    Top:=0;
    Width:=ValNoteBookMaxX;
    Height:=ValNoteBookMaxY;
  end;
  with ValueAsFilePathsSynEdit do begin
    Left:=0;
    Top:=0;
    Width:=ValNoteBookMaxX-80;
    Height:=ValNoteBookMaxY;
  end;
  with MoveFilePathUpBitBtn do begin
    Left:=ValNoteBookMaxX-75;
    Top:=5;
    Width:=ValNoteBookMaxX-Left-5;
  end;
  with MoveFilePathDownBitBtn do begin
    Left:=MoveFilePathUpBitBtn.Left;
    Top:=MoveFilePathUpBitBtn.Top+MoveFilePathUpBitBtn.Height+5;
    Width:=MoveFilePathUpBitBtn.Width;
  end;
  with DeleteFilePathBitBtn do begin
    Left:=MoveFilePathUpBitBtn.Left;
    Top:=MoveFilePathDownBitBtn.Top+MoveFilePathDownBitBtn.Height+5;
    Width:=MoveFilePathUpBitBtn.Width;
  end;
  with InsertFilePathBitBtn do begin
    Left:=MoveFilePathUpBitBtn.Left;
    Top:=DeleteFilePathBitBtn.Top+DeleteFilePathBitBtn.Height+5;
    Width:=MoveFilePathUpBitBtn.Width;
  end;
end;

procedure TCodeToolsDefinesEditor.CreateComponents;

  procedure CreateWinControl(var AWinControl: TWinControl;
    AWinControlClass: TWinControlClass; const AName: string;
    AParent: TWinControl);
  begin
    AWinControl:=AWinControlClass.Create(Self);
    with AWinControl do begin
      Name:=AName;
      Parent:=AParent;
      Visible:=true;
    end;
  end;
  
  procedure AddMenuItem(var AMenuItem: TMenuItem; const AName, ACaption: string;
    AParent: TMenuItem);
  begin
    AMenuItem:=TMenuItem.Create(nil);
    AMenuItem.Name:=AName;
    AMenuItem.Caption:=ACaption;
    if AParent=nil then
      MainMenu.Items.Add(AMenuItem)
    else
      AParent.Add(AMenuItem);
  end;

begin
  TheImageList:=TImageList.Create(Self);
  with TheImageList do begin
    Name:='TheImageList';

  end;

  // Main Menu -----------------------------------------------------------------
  MainMenu := TMainMenu.Create(Self);
  MainMenu.Name:='MainMenu';
  Menu := MainMenu;

  // exit menu
  AddMenuItem(ExitMenuItem,'ExitMenuItem','Exit',nil);
  AddMenuItem(SaveAndExitMenuItem,'SaveAndExitMenuItem','Save and Exit',
              ExitMenuItem);
  SaveAndExitMenuItem.OnClick:=@SaveAndExitMenuItemClick;
  ExitMenuItem.Add(CreateSeperator);
  AddMenuItem(DontSaveAndExitMenuItem,'DontSaveAndExitMenuItem',
              'Exit without Save',ExitMenuItem);
  DontSaveAndExitMenuItem.OnClick:=@DontSaveAndExitMenuItemClick;

  // edit nodes
  AddMenuItem(EditMenuItem,'EditMenuItem','Edit',nil);
  AddMenuItem(MoveNodeUpMenuItem,'MoveNodeUpMenuItem','Move node up',
              EditMenuItem);
  AddMenuItem(MoveNodeDownMenuItem,'MoveNodeDownMenuItem','Move node down',
              EditMenuItem);
  EditMenuItem.Add(CreateSeperator);
  AddMenuItem(InsertDefineMenuItem,'InsertDefineMenuItem','Insert Define',
              EditMenuItem);
  AddMenuItem(InsertDefineAllMenuItem,'InsertDefineAllMenuItem','Insert Define All',
              EditMenuItem);
  AddMenuItem(InsertUndefineMenuItem,'InsertUndefineMenuItem','Insert Undefine',
              EditMenuItem);
  AddMenuItem(InsertBlockMenuItem,'InsertBlockMenuItem','Insert Block',
              EditMenuItem);
  AddMenuItem(InsertDirectoryMenuItem,'InsertDirectoryMenuItem','Insert Directory',
              EditMenuItem);
  AddMenuItem(InsertIfMenuItem,'InsertIfMenuItem','Insert If',
              EditMenuItem);
  AddMenuItem(InsertIfDefMenuItem,'InsertIfDefMenuItem','Insert IfDef',
              EditMenuItem);
  AddMenuItem(InsertElseMenuItem,'InsertElseMenuItem','Insert Else',
              EditMenuItem);
  AddMenuItem(InsertEndIfMenuItem,'InsertEndIfMenuItem','Insert EndIf',
              EditMenuItem);
  EditMenuItem.Add(CreateSeperator);
  AddMenuItem(DeleteNodeMenuItem,'DeleteNodeMenuItem','Delete node',
              EditMenuItem);
  EditMenuItem.Add(CreateSeperator);
  AddMenuItem(CopyToClipbrdMenuItem,'CopyToClipbrdMenuItem','Copy to clipboard',
              EditMenuItem);
  AddMenuItem(PasteFromClipbrdMenuItem,'PasteFromClipbrdMenuItem',
              'Paste from clipboard',EditMenuItem);

  // tools
  AddMenuItem(ToolsMenuItem,'ToolsMenuItem','Tools',nil);
  AddMenuItem(OpenPreviewMenuItem,'OpenPreviewMenuItem','Open Preview',
              ToolsMenuItem);
  AddMenuItem(ShowMacroListMenuItem,'ShowMacroListMenuItem','Show Macros',
              ToolsMenuItem);

  // templates
  AddMenuItem(InsertTemplateMenuItem,'InsertTemplateMenuItem',
              'Insert Template',nil);


  // define tree----------------------------------------------------------------
  CreateWinControl(DefineTreeView,TTreeView,'DefineTreeView',Self);
  with DefineTreeView do begin
    Images:=TheImageList;
  end;

  // selected item
  CreateWinControl(SelectedItemGroupBox,TGroupBox,'SelectedItemGroupBox',Self);
  SelectedItemGroupBox.Caption:='Selected Node:';
  
  CreateWinControl(TypeLabel,TLabel,'TypeLabel',SelectedItemGroupBox);
  
  CreateWinControl(ProjectSpecificCheckBox,TCheckBox,'ProjectSpecificCheckBox',
                   SelectedItemGroupBox);
  ProjectSpecificCheckBox.Caption:=
    'Node and its children are only valid for this project';
  
  CreateWinControl(NameLabel,TLabel,'NameLabel',SelectedItemGroupBox);
  NameLabel.Caption:='Name:';
  
  CreateWinControl(NameEdit,TEdit,'NameEdit',SelectedItemGroupBox);

  CreateWinControl(DescriptionLabel,TLabel,'DescriptionLabel',
                   SelectedItemGroupBox);
  DescriptionLabel.Caption:='Description:';
                   
  CreateWinControl(DescriptionEdit,TEdit,'DescriptionEdit',
                   SelectedItemGroupBox);
                   
  CreateWinControl(VariableLabel,TLabel,'VariableLabel',SelectedItemGroupBox);
  VariableLabel.Caption:='Variable:';
  
  CreateWinControl(VariableEdit,TEdit,'VariableEdit',SelectedItemGroupBox);
  
  CreateWinControl(ValueNoteBook,TNoteBook,'ValueNoteBook',
                   SelectedItemGroupBox);
  with ValueNoteBook do begin
    Pages[0]:='Value as Text';
    Pages.Add('Value as File Paths');
  end;
                   
  CreateWinControl(ValueAsTextSynEdit,TSynEdit,'ValueAsTextSynEdit',
                   ValueNoteBook.Page[0]);
                   
  CreateWinControl(ValueAsFilePathsSynEdit,TSynEdit,'ValueAsFilePathsSynEdit',
                   ValueNoteBook.Page[1]);
                   
  CreateWinControl(MoveFilePathUpBitBtn,TBitBtn,'MoveFilePathUpBitBtn',
                   ValueNoteBook.Page[1]);
  MoveFilePathUpBitBtn.Caption:='Move path up';
                   
  CreateWinControl(MoveFilePathDownBitBtn,TBitBtn,'MoveFilePathDownBitBtn',
                   ValueNoteBook.Page[1]);
  MoveFilePathDownBitBtn.Caption:='Move path down';
                   
  CreateWinControl(DeleteFilePathBitBtn,TBitBtn,'DeleteFilePathBitBtn',
                   ValueNoteBook.Page[1]);
  DeleteFilePathBitBtn.Caption:='Delete path';
                   
  CreateWinControl(InsertFilePathBitBtn,TBitBtn,'InsertFilePathBitBtn',
                   ValueNoteBook.Page[1]);
  InsertFilePathBitBtn.Caption:='Insert path';
end;

function TCodeToolsDefinesEditor.CreateSeperator : TMenuItem;
begin
  Result := TMenuItem.Create(Self);
  Result.Caption := '-';
end;

procedure TCodeToolsDefinesEditor.RebuildDefineTreeView;
begin
  DefineTreeView.Items.BeginUpdate;
  DefineTreeView.Items.Clear;
  AddDefineNodes(FDefineTree.RootTemplate,nil,true,true);
  DefineTreeView.Items.EndUpdate;
end;

procedure TCodeToolsDefinesEditor.AddDefineNodes(
  ANode, AParent: TDefineTemplate;
  WithChilds, WithNextSiblings: boolean);
begin

end;

procedure TCodeToolsDefinesEditor.Assign(ACodeToolBoss: TCodeToolManager;
  Options: TCodeToolsOptions);
begin
  FDefineTree.Assign(ACodeToolBoss.DefineTree);
  RebuildDefineTreeView;
end;

constructor TCodeToolsDefinesEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    SetBounds((Screen.Width-480) div 2,(Screen.Height-430) div 2, 485, 435);
    Caption:='CodeTools Defines Editor';
    OnResize:=@FormResize;
    
    CreateComponents;
  end;
  FDefineTree:=TDefineTree.Create;
  Resize;
end;

destructor TCodeToolsDefinesEditor.Destroy;
begin
  FDefineTree.Free;
  inherited Destroy;
end;

end.

