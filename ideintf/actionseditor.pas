{ Copyright (C) 2004

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************


  implementing ActionList Editor

  author:
     Radek Cervinka, radek.cervinka@centrum.cz
     Mattias Gaertner
     Pawel Piwowar, alfapawel@tlen.pl

  version:
    0.1 - 26-27.2.2004 - write all from scratch
    0.2 -  3.3.2004 - speed up filling listboxes
                      some ergonomic fixes (like stay in category after ADD)
                      fixed possible language problems
    0.3 - 27.3.2004 - rename action > actualise editor
    0.4 - 29.3.2004 - dblclick generate xxx.OnExecute code to editor
    0.5 - 10.03.2005 - New design

  TODO:- multiselect for the actions and categories
       - drag & drop for the actions and categories
       - sometimes click in listbox causes selecting last item
         (it's an strange gtk error. The LCL and the gtk intf do not send any
          change to the gtk. Either it is a bug in the gtk1 or we are doing
          something wrong in the handlers.)
}
unit ActionsEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, LCLProc, Forms, Controls, Dialogs,
  ActnList, ExtCtrls, ComCtrls, Buttons, StdCtrls, ObjInspStrConsts,
  ComponentEditors, PropEdits, DBActns, StdActns, LCLIntf, LCLType,
  Graphics, GraphType, Menus;

type

  { TActionListEditor }

  TActionListEditor = class(TForm)
    ActDelete: TAction;
    ActCreateExecuteEvent: TAction;
    ActCreateHintEvent: TAction;
    ActCreateUpdateEvent: TAction;
    Action1: TAction;
    ActMoveUp: TAction;
    ActMoveDown: TAction;
    ActNewStd: TAction;
    ActionList1: TActionList;
    ActNew: TAction;
    lblCategory: TLabel;
    lblName: TLabel;
    lstCategory: TListBox;
    lstActionName: TListBox;
    mItemToolBarNewStdAction: TMenuItem;
    mItemToolBarNewAction: TMenuItem;
    mItemActListNewAction: TMenuItem;
    mItemCreateOnExecuteEvent: TMenuItem;
    mItemCreateOnHintEvent: TMenuItem;
    mItemCreateOnUpdateEvent: TMenuItem;
    mItemActListNewStdAction: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    mItemActListDelAction: TMenuItem;
    MenuItem8: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    btnAdd: TSpeedButton;
    btnDelete: TSpeedButton;
    btnUp: TSpeedButton;
    btnDown: TSpeedButton;
    PopMenuActions: TPopupMenu;
    PopMenuToolBarActions: TPopupMenu;
    SBShowMenuNewActions: TSpeedButton;
    Splitter: TSplitter;
    procedure ActCreateExecuteEventExecute(Sender: TObject);
    procedure ActCreateExecuteUpdate(Sender: TObject);
    procedure ActCreateHintExecute(Sender: TObject);
    procedure ActCreateUpdateExecute(Sender: TObject);
    procedure ActDeleteExecute(Sender: TObject);
    procedure ActDeleteUpdate(Sender: TObject);
    procedure ActMoveDownExecute(Sender: TObject);
    procedure ActMoveDownUpdate(Sender: TObject);
    procedure ActMoveUpUpdate(Sender: TObject);
    procedure ActNewExecute(Sender: TObject);
    procedure ActNewStdExecute(Sender: TObject);
    procedure ActionListEditorKeyPress(Sender: TObject; var Key: char);
    procedure PopMenuActionsPopup(Sender: TObject);
    procedure SBShowMenuNewActionsClick(Sender: TObject);
    procedure SplitterCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure lstActionNameKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lstActionNameMouseDown(Sender: TOBject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lstCategoryClick(Sender: TObject);
    procedure lstActionNameClick(Sender: TObject);
    procedure lstActionNameDblClick(Sender: TObject);
  protected
    procedure OnComponentRenamed(AComponent: TComponent);
    procedure OnComponentSelection(const OnSetSelection: TPersistentSelectionList);
    procedure OnRefreshPropertyValues;
    function GetSelectedAction: TContainedAction;
  private
    { private declarations }
    FActionList: TActionList;
    FDesigner: TComponentEditorDesigner;
    procedure ResultStdActProc(const Category: string;
                             ActionClass: TBasicActionClass; LastItem: Boolean);
  public
    { public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetActionList(AActionList: TActionList);
    procedure FillCategories;
    procedure FillActionByCategory(iIndex: Integer);
    property Designer:TComponentEditorDesigner read FDesigner write FDesigner;
  end; 

  { TActionListComponentEditor }

  TActionListComponentEditor = class(TComponentEditor)
  private
    FActionList: TActionList;
    FDesigner: TComponentEditorDesigner;
  protected
  public
    constructor Create(AComponent: TComponent;
                       ADesigner: TComponentEditorDesigner); override;
    destructor Destroy; override;
    procedure Edit; override;
    property ActionList: TActionList read FActionList write FActionList;
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

  { Action Registration }

  TRegisteredAction = class
  private
    FActionClass: TBasicActionClass;
    FGroupId: Integer;
  public
    constructor Create(TheActionClass: TBasicActionClass; TheGroupID: integer);
    property ActionClass: TBasicActionClass read FActionClass;
    property GroupId: Integer read FGroupId;
  end;
  PRegisteredAction = ^TRegisteredAction;
  
  TRegisteredActionCategory = class
  private
    FCount: integer;
    FName: string;
    FItems: PRegisteredAction;
    FResource: TComponentClass;
    function GetItems(Index: integer): TRegisteredAction;
  public
    constructor Create(const CategoryName: string; AResource: TComponentClass);
    procedure Add(const AClasses: array of TBasicActionClass);
    destructor Destroy; override;
    function IndexOfClass(AClass: TBasicActionClass): Integer;
    procedure EnumActions(Proc: TEnumActionProc; Info: Pointer);
    property Count: integer read FCount;
    property Name: string read FName;
    property Items[Index: integer]: TRegisteredAction read GetItems;
    property Resource: TComponentClass read FResource;
  end;

  TRegisteredActionCategories = class
  private
    FItems: TList;
    function GetItems(Index: Integer): TRegisteredActionCategory;
  public
    procedure Add(const CategoryName: String;
                  const AClasses: array of TBasicActionClass;
                  AResource: TComponentClass);
    destructor Destroy; override;
    function IndexOfCategory(const CategoryName: String): integer;
    procedure EnumActions(Proc: TEnumActionProc; Info: Pointer);
    function FindResource(AClass: TBasicActionClass): TComponentClass;
    function Count: Integer;
    property Items[Index: Integer]: TRegisteredActionCategory read GetItems;
  end;
  
var
  RegisteredActions: TRegisteredActionCategories;

type
  TNotifyActionListChange = procedure;

var
  NotifyActionListChange: TNotifyActionListChange;

var
  ActionListEditorForm: TActionListEditor;

type
   PCharArray   = Array[0..16+5] of PChar;
var
  aColor : PChar = 'a c #009900';
  aColorDis: PChar = 'a c #C0C0C0';
  cImg_Add: PCharArray =
  ('16 16 5 1',
   '. c None',
   '# c #303030',
   ' ',
   ' ',
   ' ',
   '................',
   '................',
   '.....#####......',
   '.....#aaa#......',
   '.....#aaa#......',
   '.....#aaa#......',
   '.#####aaa#####..',
   '.#aaaaaaaaaaa#..',
   '.#aaaaaaaaaaa#..',
   '.#aaaaaaaaaaa#..',
   '.#####aaa#####..',
   '.....#aaa#......',
   '.....#aaa#......',
   '.....#aaa#......',
   '.....#####......',
   '................');
   
  cImg_Delete: PCharArray =
  ('16 16 5 1',
   '. c None',
   '# c #303030',
   ' ',
   ' ',
   ' ',
   '................',
   '................',
   '................',
   '................',
   '................',
   '................',
   '.#############..',
   '.#aaaaaaaaaaa#..',
   '.#aaaaaaaaaaa#..',
   '.#aaaaaaaaaaa#..',
   '.#############..',
   '................',
   '................',
   '................',
   '................',
   '................');

  cImg_MoveUp: PCharArray =
  ('16 16 5 1',
   '. c None',
   '# c #303030',
   ' ',
   ' ',
   ' ',
   '................',
   '................',
   '.......#........',
   '......###.......',
   '.....##a##......',
   '....#aaaaa#.....',
   '...###aaa###....',
   '.....#aaa#......',
   '.....#aaa#......',
   '.....#aaa#......',
   '.....#aaa#......',
   '.....#aaa#......',
   '.....#aaa#......',
   '.....#aaa#......',
   '.....#####......',
   '................');
   
  cImg_MoveDown: PCharArray =
  ('16 16 5 1',
   '. c None',
   '# c #303030',
   ' ',
   ' ',
   ' ',
   '................',
   '................',
   '.....#####......',
   '.....#aaa#......',
   '.....#aaa#......',
   '.....#aaa#......',
   '.....#aaa#......',
   '.....#aaa#......',
   '.....#aaa#......',
   '.....#aaa#......',
   '...###aaa###....',
   '....#aaaaa#.....',
   '.....##a##......',
   '......###.......',
   '.......#........',
   '................');
   
{  cImg_Delete: PCharArray =
  ('16 16 5 1',
   '. c None',
   '# c #303030',
   ' ',
   ' ',
   ' ',
   '................',
   '................',
   '................',
   '..##.......##...',
   '..####....##....',
   '...####..##.....',
   '.....####.......',
   '......###.......',
   '.....#####......',
   '....###..##.....',
   '...###....##....',
   '..###......#....',
   '..###.......#...',
   '...#............',
   '.............#..',
   '................');}

procedure RegisterActions(const ACategory: string;
                          const AClasses: array of TBasicActionClass;
                          AResource: TComponentClass);
procedure UnRegisterActions(const Classes: array of TBasicActionClass);
procedure EnumActions(Proc: TEnumActionProc; Info: Pointer);
function CreateAction(TheOwner: TComponent;
                      ActionClass: TBasicActionClass): TBasicAction;
                      
procedure ShowActionListEditor(AActionList: TActionList;
  ADesigner: TComponentEditorDesigner);

implementation

uses actionseditorstd;

procedure RegisterActions(const ACategory: string;
  const AClasses: array of TBasicActionClass; AResource: TComponentClass);
begin
  RegisteredActions.Add(ACategory,AClasses,AResource);
end;

procedure UnRegisterActions(const Classes: array of TBasicActionClass);
begin

end;

procedure EnumActions(Proc: TEnumActionProc; Info: Pointer);
begin
  RegisteredActions.EnumActions(Proc,Info);
end;

function CreateAction(TheOwner: TComponent;
  ActionClass: TBasicActionClass): TBasicAction;
var
  ResourceClass: TComponentClass;
  ResInstance: TComponent;
  i: Integer;
  Component: TComponent;
  Action: TBasicAction;
  Src: TCustomAction;
  Dest: TCustomAction;
begin
  Result := ActionClass.Create(TheOwner);
  // find a Resource component registered for this ActionClass
  ResourceClass:=RegisteredActions.FindResource(ActionClass);
  if ResourceClass=nil then exit;
  ResInstance:=ResourceClass.Create(nil);
  try
    // find an action owned by the Resource component
    Action:=nil;
    for i:=0 to ResInstance.ComponentCount-1 do begin
      Component:=ResInstance.Components[i];
      if (CompareText(Component.ClassName,ActionClass.ClassName)=0)
      and (Component is TBasicAction) then begin
        Action:=TBasicAction(Component);
        Break;
      end;
    end;
    if Action=nil then exit;

    // copy TCustomAction properties
    if (Action is TCustomAction) and (Result is TCustomAction) then begin
      Src:=TCustomAction(Action);
      Dest:=TCustomAction(Result);
      Dest.Caption:=Src.Caption;
      Dest.Checked:=Src.Checked;
      Dest.Enabled:=Src.Enabled;
      Dest.HelpContext:=Src.HelpContext;
      Dest.Hint:=Src.Hint;
      Dest.ImageIndex:=Src.ImageIndex;
      Dest.ShortCut:=Src.ShortCut;
      Dest.Visible:=Src.Visible;
      if (Dest is TContainedAction) and (Dest.ImageIndex>=0)
      and (Src is TContainedAction) then begin
        // ToDo: copy image

      end;
    end;
  finally
    ResInstance.Free;
  end;
end;

procedure ShowActionListEditor(AActionList: TActionList;
  ADesigner:TComponentEditorDesigner);
begin
  if AActionList = nil
  then Raise Exception.Create('ShowActionListEditor AActionList=nil');
  if ActionListEditorForm = nil
  then ActionListEditorForm := TActionListEditor.Create(Application);
  with ActionListEditorForm do begin
    lstActionName.ItemIndex := -1;
    Designer := ADesigner;
    SetActionList(AActionList);
    ShowOnTop;
  end;
end;

{ TActionListEditor }

procedure TActionListEditor.OnComponentRenamed(AComponent: TComponent);
begin
  if (not Self.Visible) or (not Assigned(FActionList.ActionByName(AComponent.Name)))
  then Exit;
  lstActionName.Items[lstActionName.ItemIndex] := AComponent.Name;
end;

procedure TActionListEditor.OnComponentSelection(const OnSetSelection: TPersistentSelectionList);
var
  CurSelect: TContainedAction;
  tmpCategory: String;
begin
  // TODO: multiselect
  if Self.Visible
     and Assigned(OnSetSelection)
     and (OnSetSelection.Count > 0)
     and (OnSetSelection.Items[0] is TContainedAction)
     and (TContainedAction(OnSetSelection.Items[0]).ActionList = FActionList) then
    begin
      if not (GetSelectedAction <> OnSetSelection.Items[0])
      then Exit;
      CurSelect := TContainedAction(OnSetSelection.Items[0]);
      tmpCategory := CurSelect.Category;
      if tmpCategory = '' then tmpCategory := cActionListEditorUnknownCategory;
      if (lstCategory.Items[lstCategory.ItemIndex] <> tmpCategory)
         or ((lstCategory.Items[lstCategory.ItemIndex] = tmpCategory)
              and (lstActionName.Items.IndexOf(CurSelect.Name) < 0)) then begin
        lstCategory.ItemIndex := lstCategory.Items.IndexOf(tmpCategory);
        lstCategory.Click;
      end;
      lstActionName.ItemIndex := lstActionName.Items.IndexOf(CurSelect.Name);
      lstActionName.Click;
    end
  else lstActionName.ItemIndex := -1;
end;

procedure TActionListEditor.OnRefreshPropertyValues;
  function ValidCategory: Boolean;
  // spr. czy wszystkie kategorie w ListBox'sie istniej¹ w TActionList
  // inaczej: czy istnieje kategoria elementu wywoluj¹cego zdarzenie
  var
    i, j: Integer;
    bool: Boolean;
  begin
    Result := True;
    for i:= lstCategory.Items.Count-1 downto 0 do begin
      if lstCategory.Items[i] = cActionListEditorUnknownCategory
      then Break;
      bool := False;
      for j:= FActionList.ActionCount-1 downto 0 do begin
        if TContainedAction(FActionList.Actions[j]).Category = lstCategory.Items[i] then begin
          bool := True;
          Break;
        end;
      end;
      if not bool then begin
        Result := False;
        Break;
      end;
    end;  // for i
  end;
  function IsCategory(Category: String): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i:= lstCategory.Items.Count-1 downto 0 do begin
      if lstCategory.Items[i] = cActionListEditorUnknownCategory
      then Break;
      if lstCategory.Items[i] = Category then begin
        Result := True;
        Break;
      end;
    end;
  end;
var
  ASelections: TPersistentSelectionList;
  curSelect: TContainedAction;
  oldSelCategory, tmpCategory: String;
  tmpIndex: Integer;
  tmpValidCategory, tmpIsActCategory: Boolean;
begin
  if Self.Visible then begin
    ASelections:= TPersistentSelectionList.Create;
    GlobalDesignHook.GetSelection(ASelections);
    try
      if (ASelections.Count > 0)
         and (ASelections.Items[0] is TContainedAction {.ClassNameIs(TAction.ClassName})
         and (TContainedAction(ASelections.Items[0]).ActionList = FActionList) then begin
        curSelect := TContainedAction(ASelections.Items[0]);
        oldSelCategory := lstCategory.Items[lstCategory.ItemIndex];
        tmpCategory := CurSelect.Category;
        
        tmpValidCategory := ValidCategory;
        tmpIsActCategory := IsCategory(CurSelect.Category);
        
        if tmpCategory = '' then tmpCategory := cActionListEditorUnknownCategory;
           // je¿eli nie ma tej kategorii na liscie
        if ((curSelect.Category <> '') and not tmpIsActCategory)
           // nie wszystkie kategorie z lstCategory istniej¹ w FActionList
           // (usuniecie kategorii)
           or not tmpValidCategory
           // je¿eli kategoria jest inna od oznaczonej
           // oraz nie jest to kategoria '(All)' ani '(Unknown)'
           or ((tmpCategory <> lstCategory.Items[lstCategory.Items.IndexOf(tmpCategory)])
               and ((lstCategory.Items.IndexOf(cActionListEditorAllCategory) >= 0)
                      and (tmpCategory <> lstCategory.Items[lstCategory.Items.IndexOf(cActionListEditorAllCategory)]))
               and (tmpCategory <> lstCategory.Items[lstCategory.Items.IndexOf(cActionListEditorUnknownCategory)]))
        then FillCategories;

        tmpIndex := lstCategory.Items.IndexOf(tmpCategory);  // ???
        // s¹ kategorie (nie tylko Unknown) rownie¿ All i inne
        if (lstCategory.Items.Count > 1)
                  // nie istniala nowa kategoria
                  // nie istniala zaznaczona kategoria
           and ( ((not tmpIsActCategory) and (not tmpValidCategory))
                  // istniej zaznaczona kategoria
                  // nie istniala nowa kategoria
                 or ((lstCategory.Items.IndexOf(oldSelCategory) >=0) and (not tmpIsActCategory))
                  // nie istnieje zaznaczona kategoria
                  // istniej nowa kategoria
                 or ((lstCategory.Items.IndexOf(oldSelCategory) = -1) and (tmpIndex >= 0))
                  // istnieje zaznaczona kategoria
                  // istnieje nowa kategoria
                 or ((lstCategory.Items.IndexOf(oldSelCategory) >= 0) and (tmpIndex >= 0)) )
           // oraz poprzednio zaznaczona kategoria to nie (All)
           and (oldSelCategory <> cActionListEditorAllCategory) then begin
          lstCategory.ItemIndex := tmpIndex;
          lstCategory.Click;
        end;
        tmpIndex := lstActionName.items.IndexOf(CurSelect.Name);
        if lstActionName.ItemIndex <> tmpIndex then begin
          lstActionName.ItemIndex := tmpIndex;
          lstActionName.Click;
        end;
      end;
    finally
      ASelections.Free;
    end;
  end;
end;

function TActionListEditor.GetSelectedAction: TContainedAction;
begin
  if lstActionName.ItemIndex >= 0
  then Result := FActionList.ActionByName(lstActionName.Items[lstActionName.ItemIndex])
  else Result := nil;
end;

procedure TActionListEditor.ResultStdActProc(const Category: string;
  ActionClass: TBasicActionClass; LastItem: Boolean);
var
  NewAction: TContainedAction;
begin
  NewAction := ActionClass.Create(FActionList.Owner) as TContainedAction;
  if NewAction.Category <> cActionListEditorUnknownCategory
  then NewAction.Category := Category
  else NewAction.Category := '';
  NewAction.Name := FDesigner.CreateUniqueComponentName(NewAction.ClassName);

  if lstCategory.Items.IndexOf(Category) < 0
  then lstCategory.Items.Add(NewAction.Category);

  NewAction.ActionList := FActionList;
  FDesigner.PropertyEditorHook.PersistentAdded(NewAction,True);
  FDesigner.Modified;
  FDesigner.SelectOnlyThisComponent(FActionList.ActionByName(NewAction.Name));
end;

procedure TActionListEditor.SplitterCanResize(Sender: TObject;
  var NewSize: Integer; var Accept: Boolean);
begin
  lblName.Left := lstActionName.Left + 3;
end;

procedure TActionListEditor.lstActionNameKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift) then begin
     case key of
       VK_UP: if ActMoveUp.Enabled then begin
//           ActMoveUp.ExecuteAction(ActMoveUp);
           ActMoveUp.OnExecute(ActMoveUp);
           Key := 0;
         end;
         
       VK_DOWN: if ActMoveDown.Enabled then begin
//           ActMoveDown.ExecuteAction(ActMoveDown);
           ActMoveDown.OnExecute(ActMoveDown);
           Key := 0;
         end;
      end;
  end;
end;

procedure TActionListEditor.lstActionNameMouseDown(Sender: TOBject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  oldIndex, index: Integer;
begin
  if Button = mbRight then begin
    oldIndex := TListBox(Sender).ItemIndex;
    index := TListBox(Sender).GetIndexAtY(Y);
    if (index >= 0) and (oldIndex <> index) then begin
      TListBox(Sender).ItemIndex := index;
      TListBox(Sender).Click;
    end;
  end;
end;


procedure TActionListEditor.ActDeleteUpdate(Sender: TObject);
var
  oldState: Boolean;
  bmp: TBitMap;
  imgArray: PCharArray;
begin
  oldState := TAction(Sender).Enabled;
  TAction(Sender).Enabled := lstActionName.SelCount > 0;
  if TAction(Sender).Enabled <> oldState then begin
    bmp := TBitMap.Create;
    try
      imgArray := cImg_Delete;
      if TAction(Sender).Enabled
      then imgArray := cImg_Delete
      else imgArray[3] := aColorDis;
      bmp.Handle := CreatePixmapIndirect(@imgArray[0], GetSysColor(COLOR_BTNFACE));
      btnDelete.Glyph.Assign(bmp);
    finally
      bmp.Free;
    end;
  end;
end;

procedure TActionListEditor.ActMoveDownExecute(Sender: TObject);
var
  fact0,fAct1,fAct2: TContainedAction;
  fName: String;
  lboxIndex: Integer;
  direction: Integer;
begin
  if TComponent(Sender).Name = 'ActMoveUp'
  then direction := -1
  else direction := 1;

  lboxIndex := lstActionName.ItemIndex;
  fAct0 := TAction.Create(Self);
  try
    fAct1 := GetSelectedAction;
    fAct2 := FActionList.ActionByName(lstActionName.Items[lboxIndex+direction]);
    
    fAct1.AssignTo(fAct0);
    fName := fAct1.Name;
    fAct1.Name := 'tN1';
    fAct0.Name := fName;

    fAct2.AssignTo(fAct1);
    fName := fAct2.Name;
    fAct2.Name := 'tN2';
    fAct1.Name := fName;

    fAct0.AssignTo(fAct2);
    fName := fAct0.Name;
    fAct0.Name := 'tN1';
    fAct2.Name := fName;
  finally
    fAct0.Free;
  end;
  fName := lstActionName.Items[lboxIndex];
  lstActionName.Items[lboxIndex] := lstActionName.Items[lboxIndex+direction];
  lstActionName.Items[lboxIndex+direction] := fName;
  lstActionName.ItemIndex := lstActionName.Items.IndexOf(fName);
  FDesigner.SelectOnlyThisComponent(FActionList.ActionByName(fName));
end;

procedure TActionListEditor.ActMoveDownUpdate(Sender: TObject);
var
  oldState: Boolean;
  bmp: TBitMap;
  imgArray: PCharArray;
begin
  oldState := TAction(Sender).Enabled;
  TAction(Sender).Enabled := (lstActionName.Items.Count > 1)
                         and (lstActionName.ItemIndex >= 0)
                         and (lstActionName.ItemIndex < lstActionName.Items.Count-1);
  if TAction(Sender).Enabled <> oldState then begin
    bmp := TBitMap.Create;
    try
      imgArray := cImg_MoveDown;
      if TAction(Sender).Enabled
      then imgArray[3] := aColor
      else imgArray[3] := aColorDis;
      bmp.Handle := CreatePixmapIndirect(@imgArray[0], GetSysColor(COLOR_BTNFACE));
      btnDown.Glyph.Assign(bmp);
    finally
      bmp.Free;
    end;
  end;
end;

procedure TActionListEditor.ActMoveUpUpdate(Sender: TObject);
var
  oldState: Boolean;
  bmp: TBitMap;
  imgArray: PCharArray;
begin
  oldState := TAction(Sender).Enabled;
  TAction(Sender).Enabled := (lstActionName.Items.Count > 1)
                         and (lstActionName.ItemIndex > 0);
  if TAction(Sender).Enabled <> oldState then begin
    bmp := TBitMap.Create;
    try
      imgArray := cImg_MoveUp;
      if TAction(Sender).Enabled
      then imgArray[3] := aColor
      else imgArray[3] := aColorDis;
      bmp.Handle := CreatePixmapIndirect(@imgArray[0], GetSysColor(COLOR_BTNFACE));
      btnUp.Glyph.Assign(bmp);
    finally
      bmp.Free;
    end;
  end;
end;

procedure TActionListEditor.ActNewExecute(Sender: TObject);
var
  NewAction: TContainedAction;
begin
  NewAction := TAction.Create(FActionList.Owner);
  NewAction.Name := FDesigner.CreateUniqueComponentName(NewAction.ClassName);
  DebugLn(NewAction.Name);

  if lstCategory.ItemIndex > 1 // ignore first two items (virtual categories)
  then NewAction.Category := lstCategory.Items[lstCategory.ItemIndex]
  else NewAction.Category := '';

  NewAction.ActionList := FActionList;

  FDesigner.PropertyEditorHook.PersistentAdded(NewAction,True);
  FDesigner.Modified;
end;

procedure TActionListEditor.ActNewStdExecute(Sender: TObject);
begin
  TFormActStandard.CreateEx(Self, @ResultStdActProc).ShowModal;
end;

procedure TActionListEditor.ActionListEditorKeyPress(Sender: TObject;
  var Key: char);
begin
  if Ord(Key) = VK_ESCAPE then Self.Close;
end;

procedure TActionListEditor.PopMenuActionsPopup(Sender: TObject);
var
  CurAction: TContainedAction;
begin
  ActCreateExecuteEvent.Caption := 'Create OnExecute Event';
  ActCreateHintEvent.Caption := 'Create OnHint Event';
  ActCreateUpdateEvent.Caption := 'Create OnUpdate Event';
  CurAction := GetSelectedAction;
  if Assigned(CurAction) then begin
    if Assigned(CurAction.OnExecute)
    then ActCreateExecuteEvent.Caption := 'Show OnExecute Event';
    if Assigned(TAction(CurAction).OnHint)
    then ActCreateHintEvent.Caption := 'Show OnHint Event';
    if Assigned(CurAction.OnUpdate)
    then ActCreateUpdateEvent.Caption := 'Show OnUpdate Event';
  end;
end;

procedure TActionListEditor.SBShowMenuNewActionsClick(Sender: TObject);
var
  MousePoint: TPoint;
begin
  MousePoint := SBShowMenuNewActions.ClientToScreen(Point(0, btnAdd.Height-1));
  PopMenuToolBarActions.PopUp(MousePoint.X-btnAdd.Width, MousePoint.Y);
  SBShowMenuNewActions.Down := False;
end;

procedure TActionListEditor.ActDeleteExecute(Sender: TObject);
  function IsCategory(Category: String): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    for i:= FActionList.ActionCount-1 downto 0 do begin
      if FActionList.Actions[i].Category = Category then begin
        Result := True;
        Break;
      end;
    end;
  end;
var
  iNameIndex: Integer;
  OldName: String;
  OldAction: TContainedAction;
begin
  iNameIndex := lstActionName.ItemIndex;
  if iNameIndex < 0 then Exit;
  OldName := lstActionName.Items[iNameIndex];
  DebugLn('',OldName);
  lstActionName.Items.Delete(iNameIndex);

  OldAction := FActionList.ActionByName(OldName);
  OldName := OldAction.Category;

  // be gone
  if Assigned(OldAction) then
  begin
    try
      FDesigner.PropertyEditorHook.PersistentDeleting(OldAction);
      OldAction.Free;
    except
      on E: Exception do begin
        MessageDlg('Error deleting action',
          'Error while deleting action:'#13
          +E.Message,mtError,[mbOk],0);
      end;
    end;
  end;

  if lstActionName.Items.Count = 0 then // last act in category > rebuild
    FillCategories
  else
  begin
    if iNameIndex >= lstActionName.Items.Count
    then lstActionName.ItemIndex := lstActionName.Items.Count -1
    else lstActionName.ItemIndex := iNameIndex;

    FDesigner.SelectOnlyThisComponent(
       FActionList.ActionByName(lstActionName.Items[lstActionName.ItemIndex]));
  end;

  If not IsCategory(OldName)
  then lstCategory.Items.Delete(lstCategory.Items.IndexOf(OldName));
  if lstActionName.ItemIndex < 0
  then FDesigner.SelectOnlyThisComponent(FActionList);
end;

procedure TActionListEditor.ActCreateExecuteUpdate(Sender: TObject);
begin
  TAction(Sender).Enabled := (lstActionName.ItemIndex >= 0);
end;

procedure TActionListEditor.ActCreateExecuteEventExecute(Sender: TObject);
var
  CurAction: TContainedAction;
begin
  CurAction := GetSelectedAction;
  if CurAction = nil then Exit;
  // Add OnExecute for this action
  CreateComponentEvent(CurAction,'OnExecute');
end;

procedure TActionListEditor.ActCreateHintExecute(Sender: TObject);
var
  CurAction: TContainedAction;
begin
  CurAction := GetSelectedAction;
  if CurAction = nil then Exit;
  // Add OnHint for this action
  CreateComponentEvent(CurAction,'OnHint');
end;

procedure TActionListEditor.ActCreateUpdateExecute(Sender: TObject);
var
  CurAction: TContainedAction;
begin
  CurAction := GetSelectedAction;
  if CurAction = nil then Exit;
  // Add OnUpdate for this action
  CreateComponentEvent(CurAction,'OnUpdate');
end;

procedure TActionListEditor.lstCategoryClick(Sender: TObject);
begin
  if lstCategory.ItemIndex >= 0
  then FillActionByCategory(lstCategory.ItemIndex);
end;

procedure TActionListEditor.lstActionNameClick(Sender: TObject);
var
  CurAction: TContainedAction;
begin
  // TODO: multiselect
  if lstActionName.ItemIndex < 0 then Exit;
  CurAction := GetSelectedAction;
  if CurAction = nil then Exit;

  FDesigner.SelectOnlyThisComponent(CurAction);
end;

procedure TActionListEditor.lstActionNameDblClick(Sender: TObject);
var
  CurAction: TContainedAction;
begin
  if lstActionName.GetIndexAtY(lstActionName.ScreenToClient(Mouse.CursorPos).Y) < 0
  then Exit;
  CurAction := GetSelectedAction;
  if CurAction = nil then Exit;
  // Add OnExecute for this action
  CreateComponentEvent(CurAction,'OnExecute');
end;

constructor TActionListEditor.Create(AOwner: TComponent);
var
  bmp: TBitMap;
begin
  inherited Create(AOwner);
  Caption := oisActionListEditor;
  lblCategory.Caption := oisCategory;
  lblName.Caption := oisAction;
  Splitter.MinSize := lblCategory.Left + lblCategory.Width;
  ActNew.Hint := 'New Action';
  ActNewStd.Hint := 'New Standard Action';
  ActDelete.Hint := 'Delete Action';
  ActMoveUp.Hint := 'Move Up';
  ActMoveDown.Hint := 'Move Down';
  SBShowMenuNewActions.Hint := ActNew.Hint;
  mItemToolBarNewAction.Caption := ActNew.Hint;
  mItemToolBarNewStdAction.Caption := ActNewStd.Hint;
  mItemActListNewStdAction.Caption := ActNewStd.Hint;

  cImg_Add[3] := aColor;
  bmp := TBitMap.Create;
  bmp.Handle := CreatePixmapIndirect(@cImg_Add[0], GetSysColor(COLOR_BTNFACE));
  btnAdd.Glyph.Assign(Bmp);
  bmp.Free;
  
  cImg_Delete[3] := aColor;
  bmp := TBitMap.Create;
  bmp.Handle := CreatePixmapIndirect(@cImg_Delete[0], GetSysColor(COLOR_BTNFACE));
  btnDelete.Glyph.Assign(Bmp);
  bmp.Free;

  cImg_MoveUp[3] := aColor;
  bmp := TBitMap.Create;
  bmp.Handle := CreatePixmapIndirect(@cImg_MoveUp[0], GetSysColor(COLOR_BTNFACE));
  btnUp.Glyph.Assign(Bmp);
  bmp.Free;

  cImg_MoveDown[3] := aColor;
  bmp := TBitMap.Create;
  bmp.Handle := CreatePixmapIndirect(@cImg_MoveDown[0], GetSysColor(COLOR_BTNFACE));
  btnDown.Glyph.Assign(Bmp);
  bmp.Free;

  GlobalDesignHook.AddHandlerComponentRenamed(@OnComponentRenamed);
  GlobalDesignHook.AddHandlerSetSelection(@OnComponentSelection);
  GlobalDesignHook.AddHandlerRefreshPropertyValues(@OnRefreshPropertyValues);
end;

destructor TActionListEditor.Destroy;
begin
  if Assigned(GlobalDesignHook)
  then GlobalDesignHook.RemoveAllHandlersForObject(Self);
  inherited Destroy;
end;

procedure TActionListEditor.SetActionList(AActionList: TActionList);
begin
  FActionList := AActionList;
  FillCategories;
end;

procedure TActionListEditor.FillCategories;
var
  i: Integer;
  sCategory: String;
  xIndex: Integer;
  sOldCategory: String;
  countCategory: Integer;
begin
  // try remember old category
  sOldCategory := '';
  if (lstCategory.Items.Count>0) and (lstCategory.ItemIndex>-1)
  then sOldCategory := lstCategory.Items[lstCategory.ItemIndex];

  lstCategory.Items.BeginUpdate;
  try
    countCategory := lstCategory.Items.Count;
    lstCategory.Clear;

    for i:=0 to FActionList.ActionCount-1 do begin
      sCategory := FActionList.Actions[i].Category;
      if Trim(sCategory) = ''
      then Continue;
      xIndex := lstCategory.Items.IndexOf(sCategory);
      if xIndex < 0
      then lstCategory.Items.Add(sCategory);
    end;
    if lstCategory.Items.Count > 0
    then lstCategory.Sorted := True;
    lstCategory.Sorted := False;
    
    xIndex := lstCategory.Items.IndexOf(sOldCategory);
    
    if lstCategory.Items.Count > 0
    then lstCategory.Items.Insert(0, cActionListEditorAllCategory);
    if lstCategory.Items.Count > 0
    then lstCategory.Items.Insert(1, cActionListEditorUnknownCategory)
    else lstCategory.Items.Add(cActionListEditorUnknownCategory);
  finally
    lstCategory.Items.EndUpdate;
  end;
  if xIndex < 0 then begin
    if Assigned(GetSelectedAction)
       and (GetSelectedAction.Category = '')
    then xIndex := lstCategory.Items.IndexOf(cActionListEditorUnknownCategory)
    else xIndex := 0;
  end;
  lstCategory.ItemIndex := xIndex;

  if ( ((lstCategory.ItemIndex <> lstCategory.items.IndexOf(cActionListEditorAllCategory))
         or (lstActionName.Items.Count = 0))
      or (countCategory <> lstCategory.Items.Count) )
  then FillActionByCategory(xIndex);
end;

procedure TActionListEditor.FillActionByCategory(iIndex:Integer);
var
  i: Integer;
  sCategory: String;
  IndexedActionName: String;
begin

  lstActionName.Items.BeginUpdate;
  if iIndex < 0 then iIndex := 0;  // the first possition
  try
    if lstActionName.ItemIndex > -1
    then IndexedActionName := lstActionName.Items[lstActionName.ItemIndex];

    lstActionName.Clear;
    // handle all
    if iIndex = lstCategory.Items.IndexOf(cActionListEditorAllCategory) then begin
      for i := 0 to FActionList.ActionCount-1 do
        lstActionName.Items.Add(FActionList.Actions[i].Name);
      Exit; //throught finally
    end;

    // handle unknown
    if iIndex = lstCategory.Items.IndexOf(cActionListEditorUnknownCategory) then begin
      for i := 0 to FActionList.ActionCount-1 do begin
        if Trim(FActionList.Actions[i].Category) = '' then
          lstActionName.Items.Add(FActionList.Actions[i].Name);
      end;
      Exit; //throught finally
    end;

    // else sort to categories
    sCategory := lstCategory.Items[iIndex];
    for i := 0 to FActionList.ActionCount-1 do
    begin
      if FActionList.Actions[i].Category = sCategory
      then lstActionName.Items.Add(FActionList.Actions[i].Name);
    end;
  finally
    lstActionName.Items.EndUpdate;
    if (IndexedActionName <> '')
       and (lstActionName.Items.IndexOf(IndexedActionName) > -1)
    then lstActionName.ItemIndex := lstActionName.Items.IndexOf(IndexedActionName)
    else if lstActionName.ItemIndex = -1
    then FDesigner.SelectOnlyThisComponent(FActionList);;
  end;
end;


{ TActionListComponentEditor }

constructor TActionListComponentEditor.Create(AComponent: TComponent;
  ADesigner: TComponentEditorDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FDesigner := ADesigner;
end;

destructor TActionListComponentEditor.Destroy;
begin
  if Assigned(ActionListEditorForm)
     and (ActionListEditorForm.FActionList = GetComponent)
  then FreeThenNil(ActionListEditorForm);
  inherited Destroy;
end;

procedure TActionListComponentEditor.Edit;
begin
  DebugLn('TActionListComponentEditor.Edit ',GetComponent.Name);
  ShowActionListEditor(GetComponent as TActionList, FDesigner);
end;

function TActionListComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

function TActionListComponentEditor.GetVerb(Index: Integer): string;
begin
  Result := oisEditActionList;
end;

procedure TActionListComponentEditor.ExecuteVerb(Index: Integer);
begin
  Edit;
end;


{ TRegisteredAction }

constructor TRegisteredAction.Create(TheActionClass: TBasicActionClass;
  TheGroupID: integer);
begin
  FActionClass := TheActionClass;
  FGroupId := TheGroupID;
end;


{ TRegisteredActionCategory }

function TRegisteredActionCategory.GetItems(Index: integer): TRegisteredAction;
begin
  Result := FItems[Index];
end;

constructor TRegisteredActionCategory.Create(const CategoryName: string;
  AResource: TComponentClass);
begin
  FName := CategoryName;
  FResource := AResource;
end;

procedure TRegisteredActionCategory.Add(
  const AClasses: array of TBasicActionClass);
var
  i: integer;
  CurCount: Integer;
  IsDouble: Boolean;
  j: Integer;
  AClass: TBasicActionClass;
  l: Integer;
begin
  l := High(AClasses)-Low(AClasses)+1;
  if l = 0 then exit;
  CurCount := FCount;
  Inc(FCount,l);
  // add all classes (ignoring doubles)
  ReAllocMem(FItems,SizeOf(TBasicActionClass)*FCount);
  for i:=Low(AClasses) to High(AClasses) do begin
    AClass:=AClasses[i];
    // check if already exists
    IsDouble:=false;
    for j:=0 to CurCount-1 do begin
      if FItems[j].ActionClass = AClass then begin
        IsDouble := True;
        Break;
      end;
    end;
    // add
    if not IsDouble then begin
      // TODO use current designer group instead of -1
      FItems[CurCount] := TRegisteredAction.Create(AClass,-1);
      Inc(CurCount);
      RegisterNoIcon([AClass]);
      Classes.RegisterClass(AClass);
    end;
  end;
  // resize FItems
  if CurCount < FCount then begin
    FCount := CurCount;
    ReAllocMem(FItems,SizeOf(TBasicActionClass)*FCount);
  end;
end;

destructor TRegisteredActionCategory.Destroy;
var
  i: Integer;
begin
  for i:= Count-1 downto 0 do Items[i].Free;
  ReAllocMem(FItems,0);
  inherited Destroy;
end;

function TRegisteredActionCategory.IndexOfClass(AClass: TBasicActionClass
  ): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (FItems[Result].ActionClass<>AClass) do Dec(Result);
end;

procedure TRegisteredActionCategory.EnumActions(Proc: TEnumActionProc;
  Info: Pointer);
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    Proc(Name,FItems[i].ActionClass,Info);
end;


{ TRegisteredActionCategories }

function TRegisteredActionCategories.GetItems(Index: integer
  ): TRegisteredActionCategory;
begin
  Result:=TRegisteredActionCategory(FItems[Index]);
end;

procedure TRegisteredActionCategories.Add(const CategoryName: string;
  const AClasses: array of TBasicActionClass; AResource: TComponentClass);
var
  i: LongInt;
  Category: TRegisteredActionCategory;
begin
  i := IndexOfCategory(CategoryName);
  if i >= 0 then begin
    Category := Items[i];
    if Category.Resource<>AResource then
      raise Exception.Create('TRegisteredActionCategories.Add Resource<>OldResource');
  end else begin
    Category := TRegisteredActionCategory.Create(CategoryName,AResource);
    if FItems = nil then FItems := TList.Create;
    FItems.Add(Category);
  end;
  Category.Add(AClasses);
  if Assigned(NotifyActionListChange) then
    NotifyActionListChange;
end;

destructor TRegisteredActionCategories.Destroy;
var
  i: Integer;
begin
  for i:= Count-1 downto 0 do Items[i].Free;
  FItems.Free;
  inherited Destroy;
end;

function TRegisteredActionCategories.IndexOfCategory(const CategoryName: string
  ): integer;
begin
  Result := Count-1;
  while (Result>=0) and (CompareText(Items[Result].Name,CategoryName)<>0) do
    Dec(Result);
end;

procedure TRegisteredActionCategories.EnumActions(Proc: TEnumActionProc;
  Info: Pointer);
var
  i: Integer;
begin
  for i:=0 to Count-1 do
    Items[i].EnumActions(Proc,Info);
end;

function TRegisteredActionCategories.FindResource(AClass: TBasicActionClass
  ): TComponentClass;
var
  Category: TRegisteredActionCategory;
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do begin
    Category := Items[i];
    if Category.IndexOfClass(AClass) >= 0 then begin
      Result := Category.Resource;
      Break;
    end;
  end;
end;

function TRegisteredActionCategories.Count: integer;
begin
  if FItems = nil
  then Result := 0
  else Result := FItems.Count;
end;

procedure RegisterStandardActions;
begin
  // TODO
  //  - default images for actions

  // register edit actions
  RegisterActions('Edit',[TEditCut,TEditCopy,TEditPaste,TEditSelectAll,
   TEditUndo,TEditDelete],nil);
  // register help actions
  RegisterActions('Help',[THelpAction,THelpContents,THelpTopicSearch,
    THelpOnHelp,THelpContextAction],nil);
  // register dialog actions
  RegisterActions('Dialog',[TFontEdit,TColorSelect],nil);
  // register file actions
  RegisterActions('File',[TFileOpen,TFileOpenWith,TFileSaveAs,TFileExit],nil);
  // register database actions
  RegisterActions('Database',[TDataSetFirst,TDataSetLast,TDataSetNext,
    TDataSetPrior,TDataSetRefresh,TDataSetCancel,TDataSetDelete,TDataSetEdit,
    TDataSetInsert,TDataSetPost],nil);
end;

initialization
  {$I actionseditor.lrs}
  ActionListEditorForm := nil;
  NotifyActionListChange := nil;
  RegisteredActions := TRegisteredActionCategories.Create;
  RegisterActionsProc := @RegisterActions;
  UnRegisterActionsProc := @UnregisterActions;
  EnumRegisteredActionsProc := @EnumActions;
  CreateActionProc := @CreateAction;
  
  RegisterComponentEditor(TActionList,TActionListComponentEditor);
  RegisterStandardActions;

finalization
  RegisteredActions.Free;
  RegisteredActions := nil;
end.

