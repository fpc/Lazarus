unit MenuTemplates;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types,
  Buttons, Controls, Dialogs, StdCtrls, ExtCtrls, Menus,
  ComCtrls, Forms, Graphics, Themes, LCLType, LCLIntf, LCLProc,
  // LazUtils
  LazUTF8, LazFileUtils, Laz2_XMLCfg,
  // IdeIntf
  IDEDialogs,
  // IDE
  LazarusIDEStrConsts, MenuShortcuts;

type
  { TMenuTemplate }

  TMenuTemplate = class(TObject)
  strict private
    FDescription: string;
    FIsStandardTemplate: boolean;
    FPrimaryItem: string;
    FSubList: TStringList;
    function GetShortcut(index: integer): TShortCut;
    function GetSubItem(index: integer): string;
    function GetSubItemCount: integer;
  public
    class function MenuItemToString(aMenuItem: TMenuItem; aDescription: string): string;
    constructor CreateFromString(const aMenuString: string);
    destructor Destroy; override;
    function ReadFromString(const aString: string): boolean;
    property Description: string read FDescription write FDescription;
    property IsStandardTemplate: boolean read FIsStandardTemplate write FIsStandardTemplate;
    property PrimaryItem: string read FPrimaryItem;
    property Shortcut[index: integer]: TShortCut read GetShortcut;
    property SubItem[index: integer]: string read GetSubItem;
    property SubItemCount: integer read GetSubItemCount;
  end;

  TDialogMode = (dmInsert, dmSave, dmDelete);

  { TMenuTemplates }

  TMenuTemplates = class(TObject)
  strict private
    FTemplateList: TFPList;
    function GetDescription(index: integer): string;
    function GetMenu(index: integer): TMenuItem;
    function GetMenuCount: integer;
    function GetMenuTemplate(index: integer): TMenuTemplate;
    function GetPrimaryItem(index: integer): string;
    procedure CheckIndex(anIndex: integer);
    procedure LoadDefaultTemplates;
    procedure LoadSavedTemplates;
  public
    constructor CreateForMode(aDialogMode: TDialogMode=dmInsert);
    destructor Destroy; override;
    function GetIndexOfTemplate(aMT: TMenuTemplate): integer;
    procedure AddTemplate(const aTemplateText: string; isStandard: boolean=True);
    procedure SaveTemplateToConfig(aMenuTemplate: TMenuTemplate);
    property Description[index: integer]: string read GetDescription;
    property Menu[index: integer]: TMenuItem read GetMenu;
    property MenuCount: integer read GetMenuCount;
    property MenuTemplate[index: integer]: TMenuTemplate read GetMenuTemplate;
    property PrimaryItem[index: integer]: string read GetPrimaryItem;
  end;

  { TPreview }

  TPreview = class(TGraphicControl)
   strict private
     FDisplayAsPopup: boolean;
     FTemplate: TMenuTemplate;
     function GetSize: TSize;
     procedure SetDisplayAsPopup(AValue: boolean);
   protected
     procedure Paint; override;
     procedure SetParent(NewParent: TWinControl); override;
   public
     property DisplayAsPopup: boolean read FDisplayAsPopup write SetDisplayAsPopup;
     procedure Clear;
     procedure LoadTemplate(aMenuTemplate: tmenuTemplate);
  end;

  { TMenuTemplateDialog }

  TMenuTemplateDialog = class(TForm)
  strict private
    // GUI
    FBCancel: TBitBtn;
    FBExecute: TBitBtn;
    FCBDisplay: TCheckBox;
    FEDescription: TEdit;
    FGChoose: TGroupBox;
    FLDescription: TLabel;
    FMenuToSave: TMenuItem;
    FPButtons: TCustomPanel;
    FPDescription: TPanel;
    FPreview: TPreview;
    FPRight: TCustomPanel;
    FScrollBoxPreview: TScrollBox;
    FSplitter: TSplitter;
    FTVTemplates: TTreeView;
    // data
    FDialogMode: TDialogMode;
    FMenuToInsert: TMenuItem;
    FNewMenuTemplate: TMenuTemplate;
    FNoneSavedNode: TTreeNode;
    FSavedNode: TTreeNode;
    FStandardNode: TTreeNode;
    FTemplates: TMenuTemplates;
    FMenu: TMenu;
    procedure BExecuteDeleteClick(Sender: TObject);
    procedure BExecuteInsertClick(Sender: TObject);
    procedure BExecuteSaveClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure CBDisplayChange(Sender: TObject);
    procedure DeleteSelectedFromConfigFile;
    procedure DeleteSelectedTemplate;
    procedure EDescriptionChange(Sender: TObject);
    procedure PopulateTreeView;
    procedure SaveMenuAsTemplate;
    procedure SetupGUI;
    procedure ShowPreview(aMenuTemplate: TMenuTemplate);
    procedure TVAdvancedCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
                State: TCustomDrawState; Stage: TCustomDrawStage;
                var {%H-}PaintImages, DefaultDraw: Boolean);
    procedure TVEditing(Sender: TObject; {%H-}Node: TTreeNode; var AllowEdit: Boolean);
    procedure TVSelectionChanged(Sender: TObject);
  protected
    procedure DoShowWindow; override;
  public
    constructor CreateWithMode(aMenu: TMenu; aDialogMode: TDialogMode);
    destructor Destroy; override;
    property MenuToInsert: TMenuItem read FMenuToInsert;
    property MenuToSave: TMenuItem read FMenuToSave write FMenuToSave;
  end;

function SavedTemplatesExist: boolean;
function GetSavedTemplatesCount: integer;
function InsertMenuTemplateDlg: TMenuItem;
function DeleteMenuTemplateDlg: boolean;
function GetCfgPath: string;

const
  MenuBar_Height = 20;
  Separator_Height = 7;
  Separator_Centre = 3;
  DropDown_Height = 24;
  MenuBar_Text_Offset = 7;
  Double_MenuBar_Text_Offset = MenuBar_Text_Offset shl 1;
  DropDown_Text_Offset = 35;
  Double_DropDown_Text_Offset = DropDown_Text_Offset shl 1;
  Gutter_Offset = 6;
  Gutter_X = DropDown_Text_Offset - Gutter_Offset;
  MenuTemplatesFilename = 'menutemplates.xml';


implementation

function InvalidXML(aTemplateCfgName: string): boolean;
// perform a quick check, far from a full validation
var
  sl: TStringList;
  tr0, s: string;
begin
  sl:=TStringList.Create;
  try
    sl.LoadFromFile(aTemplateCfgName);
    if (sl.Count < 3) then
      Exit(True);
    tr0:=Trim(sl[0]);
    s:=Copy(tr0, 1, 15);
    if not SameText(s, '<?xml version="') then
      Exit(True);
    s:=Copy(tr0, Length(tr0) - 17, 18);
    if not SameText(s, 'encoding="UTF-8"?>') then
      Exit(True);
    if not SameText(Trim(sl[1]), '<CONFIG>') then
      Exit(True);
    if not SameText(Trim(sl[Pred(sl.Count)]), '</CONFIG>') then
      Exit(True);
    Result:=False;
  finally
    sl.Free;
  end;
end;

function SavedTemplatesExist: boolean;
var
  XMLConfig: TXMLConfig;
  cfgPath, s, templateCfgName: string;
begin
  cfgPath:=GetCfgPath;
  templateCfgName:=cfgPath + DirectorySeparator + MenuTemplatesFilename;
  if not FileExistsUTF8(templateCfgName) then
    Exit(False);
  if InvalidXML(templateCfgName) then
    // file is corrupted or not XML, so discard to prevent exception
    DeleteFile(templateCfgName);
  XMLConfig:=TXMLConfig.Create(templateCfgName);
  try
    s:=XMLConfig.GetValue('menu_1/Name/Value', 'missing_Menu_1_Name');
    Result:=CompareText(s, 'missing_Menu_1_Name') <> 0;
  finally
    XMLConfig.Free;
  end;
end;

function GetSavedTemplatesCount: integer;
var
  mt: TMenuTemplates;
begin
  mt:=TMenuTemplates.CreateForMode(dmDelete);
  try
    Result:=mt.MenuCount;
  finally
    mt.Free;
  end;
end;
{
procedure SaveMenuTemplateDlg(aMenuItem: TMenuItem);
var
  dlg: TMenuTemplateDialog;
begin
  dlg:=TMenuTemplateDialog.CreateWithMode(nil, dmSave);
  try
    dlg.MenuToSave:=aMenuItem;
    dlg.ShowModal;
  finally
    dlg.Free;
  end;
end;
}
function InsertMenuTemplateDlg: TMenuItem;
var
  dlg: TMenuTemplateDialog;
begin
  dlg:=TMenuTemplateDialog.CreateWithMode(nil, dmInsert);
  try
    if (dlg.ShowModal = mrOK) then
      Result:=dlg.MenuToInsert
    else
      Result:=nil;
  finally
    dlg.Free;
  end;
end;

function DeleteMenuTemplateDlg: boolean;
var
  dlg: TMenuTemplateDialog;
  mr: TModalResult;
begin
  dlg:=TMenuTemplateDialog.CreateWithMode(nil, dmDelete);
  try
    mr:=dlg.ShowModal;
    Result:=(mr = mrOK);
  finally
    dlg.Free;
  end;
end;

function GetCfgPath: string;
begin
  Result:=ExtractFilePath(ChompPathDelim(GetAppConfigDirUTF8(False)))+'lazarus';
end;


{ TMenuTemplate }

function TMenuTemplate.GetShortcut(index: integer): TShortCut;
begin
  Result:=TShortCut(PtrUInt(FSubList.Objects[index]));
end;

function TMenuTemplate.GetSubItem(index: integer): string;
begin
  Result:=FSubList[index];
end;

function TMenuTemplate.GetSubItemCount: integer;
begin
  Result:=FSubList.Count;
end;

class function TMenuTemplate.MenuItemToString(aMenuItem: TMenuItem;
  aDescription: string): string;
var
  sc: TShortCut;
  scStr: string;
  i: integer;
  mi: TMenuItem;
begin
  if (aMenuItem = nil) then
    Exit('');
  Result:=Format('%s,%s,',[aMenuItem.Caption, aDescription]);
  for i:=0 to aMenuItem.Count-1 do
  begin
    mi:=aMenuItem.Items[i];
    sc:=mi.ShortCut;
    if (sc = 0) then
    begin
      if (mi.ShortCutKey2 = 0) then
        scStr:=''
      else
        scStr:=ShortCutToText(mi.ShortCutKey2);
    end
    else
      scStr:=ShortCutToText(sc);
    AppendStr(Result, Format('%s,%s,',[mi.Caption, scStr]));
  end;
end;

constructor TMenuTemplate.CreateFromString(const aMenuString: string);
begin
  FSubList:=TStringList.Create;
  if not ReadFromString(aMenuString) then
    Assert(False,'TMenuTemplate.CreateFromString: Attempt to read invalid menu template');
end;

destructor TMenuTemplate.Destroy;
begin
  FSubList.Free;
  inherited Destroy;
end;

function TMenuTemplate.ReadFromString(const aString: string): boolean;
var
  sl: TStringList;
  s: string;
  i: integer;
begin
  Result:=True;
  sl:=TStringList.Create;
  try
    sl.StrictDelimiter:=True;
    sl.CommaText:=aString;
    case sl.Count of
      0: Result:=False;
      1: FPrimaryItem:=sl[0];
      2: begin
           FPrimaryItem:=sl[0];
           FDescription:=sl[1];
         end;
      else begin
         FPrimaryItem:=sl[0];
         FDescription:=sl[1];
         for i:=2 to sl.Count-1 do
         begin
           if not Odd(i) then
             s:=sl[i]
           else
             FSubList.AddObject(s, TObject(PtrUInt(TextToShortCut(sl[i]))));
         end;
       end;
    end; // case
  finally
    sl.Free;
  end;
end;

{ TMenuTemplates }

function TMenuTemplates.GetDescription(index: integer): string;
begin
  CheckIndex(index);
  Result:=TMenuTemplate(FTemplateList[index]).Description;
end;

function TMenuTemplates.GetMenu(index: integer): TMenuItem;
var
  mt: TMenuTemplate;
  submi, mi: TMenuItem;
  i: integer;
begin
  CheckIndex(index);
  mt:=TMenuTemplate(FTemplateList[index]);
  mi:=TMenuItem.Create(nil);
  mi.Caption:=mt.PrimaryItem;
  for i:=0 to mt.SubItemCount-1 do
  begin
    submi:=TMenuItem.Create(nil);
    submi.Caption:=mt.SubItem[i];
    submi.ShortCut:=mt.Shortcut[i];
    mi.Insert(i, submi);
  end;
  Result:=mi;
end;

function TMenuTemplates.GetMenuCount: integer;
begin
  Result:=FTemplateList.Count;
end;

function TMenuTemplates.GetMenuTemplate(index: integer): TMenuTemplate;
begin
  CheckIndex(index);
  Result:=TMenuTemplate(FTemplateList[index]);
end;

function TMenuTemplates.GetPrimaryItem(index: integer): string;
begin
  CheckIndex(index);
  Result:=TMenuTemplate(FTemplateList[index]).PrimaryItem;
end;

procedure TMenuTemplates.CheckIndex(anIndex: integer);
begin
  Assert((anIndex > -1) and (anIndex < FTemplateList.Count),
       Format('TMenuTemplates.CheckIndex: index (%d) out of bounds[0-%d]',
              [anIndex, Pred(FTemplateList.Count)]));
end;

procedure TMenuTemplates.LoadDefaultTemplates;
begin
  AddTemplate(lisMenuEditorBasicEditMenuTemplate);
  AddTemplate(lisMenuEditorBasicFileMenuTemplate);
  AddTemplate(lisMenuEditorBasicWindowMenuTemplate);
  AddTemplate(lisMenuEditorBasicHelpMenuTemplate);
end;

procedure TMenuTemplates.LoadSavedTemplates;
var
  XMLConfig: TXMLConfig;
  i, j: integer;
  cfgPath, s, sc, sText, tmp: string;
begin
  cfgPath:=GetCfgPath;
  XMLConfig:=TXMLConfig.Create(cfgPath + DirectorySeparator + MenuTemplatesFilename);
  try
    i:=1;
    s:=XMLConfig.GetValue(Format('menu_%d/Name/Value',[i]), 'missing Name');
    while (s <> 'missing Name') do
    begin
      tmp:=XMLConfig.GetValue(Format('menu_%d/Description/Value',[i]),
                                'missing Description');
      sText:=Format('%s,%s,',[s, tmp]);
      s:=XMLConfig.GetValue(Format('menu_%d/SubItems/Value',[i]), '');
      if (s = 'true') then
        begin
          j:=0;
          s:=XMLConfig.GetValue(Format('menu_%d/subitem_%d/Name/Value',[i,j]), 'nonexistent subitem');
          sc:=XMLConfig.GetValue(Format('menu_%d/subitem_%d/Shortcut/Value',[i,j]), 'nonexistent shortcut');
          while (s <> 'nonexistent subitem') do
            begin
              if (CompareText(sc, 'nonexistent shortcut') = 0) then
                sc := '';
              AppendStr(sText, s + ',' + sc + ',');
              Inc(j);
              s:=XMLConfig.GetValue(Format('menu_%d/subitem_%d/Name/Value',[i,j]), 'nonexistent subitem');
              sc:=XMLConfig.GetValue(Format('menu_%d/subitem_%d/Shortcut/Value',[i,j]), 'nonexistent shortcut');
            end;
        end;
      Inc(i);
      s:=XMLConfig.GetValue(Format('menu_%d/Name/Value',[i]), 'missing Name');
      AddTemplate(sText, False);
    end;
  finally
    XMLConfig.Free;
  end;
end;

constructor TMenuTemplates.CreateForMode(aDialogMode: TDialogMode);
begin
  inherited Create;
  FTemplateList:=TFPList.Create;
  if (aDialogMode = dmInsert) then
    LoadDefaultTemplates;
  LoadSavedTemplates;
end;

destructor TMenuTemplates.Destroy;
var
  p: pointer;
  mt: TMenuTemplate absolute p;
begin
  for p in FTemplateList do
    mt.Free;
  FreeAndNil(FTemplateList);
  inherited Destroy;
end;

function TMenuTemplates.GetIndexOfTemplate(aMT: TMenuTemplate): integer;
begin
  if (aMT = nil) then
    Result:= -1
  else
    Result:=FTemplateList.IndexOf(aMT);
end;

procedure TMenuTemplates.AddTemplate(const aTemplateText: string;
  isStandard: boolean);
var
  mt: TMenuTemplate;
begin
  if (aTemplateText = '') then
    Exit;
  mt:=TMenuTemplate.CreateFromString(aTemplateText);
  mt.IsStandardTemplate:=isStandard;
  FTemplateList.Add(mt);
end;

procedure TMenuTemplates.SaveTemplateToConfig(aMenuTemplate: TMenuTemplate);
var
  XMLConfig: TXMLConfig;
  i, j: integer;
  cfgPath, s: string;
begin
  cfgPath:=GetCfgPath;
  XMLConfig:=TXMLConfig.Create(cfgPath + DirectorySeparator + MenuTemplatesFilename);
  try
    i:=1;
    s:=XMLConfig.GetValue(Format('menu_%d/Name/Value',[i]), 'missing');
    while not SameText(s, 'missing') do
    begin
      Inc(i);
      s:=XMLConfig.GetValue(Format('menu_%d/Name/Value',[i]), 'missing');
    end;
    XMLConfig.SetValue(Format('menu_%d/Name/Value',[i]), aMenuTemplate.PrimaryItem);
    XMLConfig.SetValue(format('menu_%d/Description/Value',[i]), aMenuTemplate.Description);
    if (aMenuTemplate.SubItemCount > 0) then
      XMLConfig.SetValue(Format('menu_%d/SubItems/Value', [i]), 'true');
    for j:=0 to aMenuTemplate.SubItemCount-1 do
    begin
      XMLConfig.SetValue(Format('menu_%d/subitem_%d/Name/Value',[i,j]), aMenuTemplate.SubItem[j]);
      XMLConfig.SetValue(Format('menu_%d/subitem_%d/Shortcut/Value',[i,j]), ShortCutToText(aMenuTemplate.Shortcut[j]));
    end;
    InvalidateFileStateCache;
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;
end;

{ TPreview }

function TPreview.GetSize: TSize;
var
  w, h: integer;
  i, tmp: integer;
  s: string;
begin
  if (FTemplate = nil) then
  begin
    FillChar(Result{%H-}, SizeOf(Result), 0);
    SetBounds(0,0,0,0);
  end
  else
    case FDisplayAsPopup of

      True: begin
        w:=5;
        h:=10;
        for i:=0 to FTemplate.SubItemCount-1 do
        begin
          s:=FTemplate.SubItem[i];
          if (s = '-') then
            Inc(h, Separator_Height)
          else begin
            Inc(h, DropDown_Height);
            tmp:=Canvas.TextWidth(s);
            if (FTemplate.Shortcut[i] <> 0) then
              Inc(tmp, Canvas.TextWidth(ShortCutToText(FTemplate.Shortcut[i])) + Double_MenuBar_Text_Offset);
            if (tmp > w) then
              w:=tmp;
          end;
        end;
        Result.cx:=w + 2*Double_DropDown_Text_Offset + Canvas.TextWidth(FTemplate.PrimaryItem);
        Result.cy:=h + 2;
      end;

      False: begin
        w:=0;
        h:=MenuBar_Height;
        for i:=0 to FTemplate.SubItemCount-1 do
        begin
          s:=FTemplate.SubItem[i];
          if (s = '-') then
            Inc(h, Separator_Height)
          else begin
            Inc(h, DropDown_Height);
            tmp:=Canvas.TextWidth(s);
            if (FTemplate.Shortcut[i] <> 0) then
              Inc(tmp, Canvas.TextWidth(ShortCutToText(FTemplate.Shortcut[i])) + Double_MenuBar_Text_Offset);
            if (tmp > w) then
              w:=tmp;
          end;
        end;
        Result.cx:=w + Double_DropDown_Text_Offset;
        Result.cy:=h + 2;
      end;

    end;
end;

procedure TPreview.SetDisplayAsPopup(AValue: boolean);
var
  sz: TSize;
begin
  if FDisplayAsPopup=AValue then Exit;
  FDisplayAsPopup:=AValue;
  SetBounds(0,0,0,0);
  sz:=GetSize;
  SetBounds(0, 0, sz.cx, sz.cy);
end;

procedure TPreview.Paint;
var
  r, rBar, rDrop: TRect;
  dets: TThemedElementDetails;
  textFlags: integer = DT_VCENTER or DT_SINGLELINE or DT_EXPANDTABS;
  i, t, h, w, l: integer;
  txt: string;
  separator: boolean;
  szB: TSize;
begin
  r:=ClientRect;
  Canvas.FillRect(r);
  Canvas.Frame(r);
  InflateRect(r, -1, -1);
  rBar:=r;
  if FDisplayAsPopup then
  begin
    rBar.Top:=rBar.Top+5;
    rBar.Left:=rBar.Left+5;
    rBar.Bottom:=rBar.Top + DropDown_Height;
    rBar.Right:=rBar.Left + DropDown_Text_Offset - Gutter_Offset;
    dets:=ThemeServices.GetElementDetails(tmPopupGutter);
    ThemeServices.DrawElement(Canvas.Handle, dets, rBar);
    w:=Canvas.TextWidth(FTemplate.PrimaryItem);
    rBar.Right:=rBar.Left + w + Double_DropDown_Text_Offset;
    rBar.Left:=rBar.Left + DropDown_Text_Offset;
    dets:=ThemeServices.GetElementDetails(tmPopupItemNormal);
    ThemeServices.DrawText(Canvas, dets, FTemplate.PrimaryItem, rBar, textFlags, 0);
    rBar.Left:=r.Left+5;
    Canvas.Pen.Color:=clLtGray;
    Canvas.Frame(rBar);
    if (FTemplate.SubItemCount > 0) then
    begin
      rDrop:=rBar;
      dets:=ThemeServices.GetElementDetails(tmPopupSubmenuNormal);
      rDrop.Left:=rDrop.Right - DropDown_Text_Offset;
      ThemeServices.DrawElement(Canvas.Handle, dets, rDrop);
    end;
    rDrop:=rBar;
    szB:=Size(rBar);
    OffsetRect(rDrop, szB.cx + 1, 2);
    rDrop.Right:=r.Right-1;
    rDrop.Bottom:=r.Bottom-1;
    Canvas.Frame(rDrop);
    l:=rDrop.Left+1;
    w:=r.Right-2;
    t:=rDrop.Top+1;
    for i:=0 to Pred(FTemplate.SubItemCount) do
    begin
      txt:=FTemplate.SubItem[i];
      separator:=(txt = '-');
      if separator then
        h:=Separator_Height
      else
        h:=DropDown_Height;
      rDrop:=Rect(l, t, w, t+h);
      Inc(t, h);
      rBar:=rDrop;
      rBar.Right:=rBar.Left + DropDown_Text_Offset - Gutter_Offset;
      dets:=ThemeServices.GetElementDetails(tmPopupGutter);
      ThemeServices.DrawElement(Canvas.Handle, dets, rBar);
      if separator then
      begin
        dets:=ThemeServices.GetElementDetails(tmPopupSeparator);
        ThemeServices.DrawElement(Canvas.Handle, dets, rDrop);
      end
      else
      begin
        rDrop.Left:=rDrop.Left + DropDown_Text_Offset;
        dets:=ThemeServices.GetElementDetails(tmPopupItemNormal);
        ThemeServices.DrawText(Canvas, dets, txt, rDrop, textFlags, 0);
        if (FTemplate.Shortcut[i] <> 0) then
        begin
          txt:=ShortCutToText(FTemplate.Shortcut[i]);
          rDrop.Left:=r.Right - Canvas.TextWidth(txt) - Double_MenuBar_Text_Offset;
          ThemeServices.DrawText(Canvas, dets, txt, rDrop, textFlags, 0);
        end;
      end;
    end;
  end
  else
  begin
    rBar.Bottom:=rBar.Top + MenuBar_Height;
    dets:=ThemeServices.GetElementDetails(tmBarBackgroundActive);
    ThemeServices.DrawElement(Canvas.Handle, dets, rBar);
    rBar.Left:=rBar.Left + MenuBar_Text_Offset;
    ThemeServices.DrawText(Canvas, dets, FTemplate.PrimaryItem, rBar, textFlags, 0);
    t:=MenuBar_Height + 1;
    for i:=0 to Pred(FTemplate.SubItemCount) do
    begin
      rBar:=r;
      rBar.Top:=t;
      txt:=FTemplate.SubItem[i];
      separator:=(txt = '-');
      if separator then
        h:=Separator_Height
      else
        h:=DropDown_Height;
      rBar.Bottom:=rBar.Top + h;
      rBar.Right:=DropDown_Text_Offset - Gutter_Offset;
      dets:=ThemeServices.GetElementDetails(tmPopupGutter);
      ThemeServices.DrawElement(Canvas.Handle, dets, rBar);
      rBar.Left:=rBar.Left + DropDown_Text_Offset;
      rBar.Right:=r.Right;
      if separator then
      begin
        dets:=ThemeServices.GetElementDetails(tmPopupSeparator);
        ThemeServices.DrawElement(Canvas.Handle, dets, rBar);
      end
      else
      begin
        dets:=ThemeServices.GetElementDetails(tmPopupItemNormal);
        ThemeServices.DrawText(Canvas, dets, txt, rBar, textFlags, 0);
        if (FTemplate.Shortcut[i] <> 0) then
        begin
          txt:=ShortCutToText(FTemplate.Shortcut[i]);
          rBar.Left:=r.Right - Canvas.TextWidth(txt) - Double_MenuBar_Text_Offset;
          ThemeServices.DrawText(Canvas, dets, txt, rBar, textFlags, 0);
        end;
      end;
      inc(t, h);
    end;
  end;
end;

procedure TPreview.SetParent(NewParent: TWinControl);
var
  sz: TSize;
begin
  inherited SetParent(NewParent);
  if (NewParent <> nil) then
    begin
      sz:=GetSize;
      SetBounds(0, 0, sz.cx, sz.cy);
      Canvas.Pen.Color:=clLtGray;
      Canvas.Brush.Color:=clBtnFace;
    end;
end;

procedure TPreview.Clear;
var
  sz: TSize;
begin
  FTemplate:=nil;
  sz:=GetSize;
  SetBounds(0, 0, sz.cx, sz.cy);
end;

procedure TPreview.LoadTemplate(aMenuTemplate: tmenuTemplate);
var
  sz: TSize;
begin
  FTemplate:=aMenuTemplate;
  sz:=GetSize;
  SetBounds(0, 0, sz.cx, sz.cy);
end;

{ TMenuTemplateDialog }

constructor TMenuTemplateDialog.CreateWithMode(aMenu: TMenu; aDialogMode: TDialogMode);
begin
  inherited CreateNew(Nil);
  FMenu:=aMenu;
  FDialogMode:=aDialogMode;
  BorderStyle:=bsSizeable;
  SetInitialBounds(0, 0, 530, 380);
  Position:=poScreenCenter;
  case aDialogMode of
    dmSave: Caption:=lisMenuEditorSaveMenuAsTemplate;
    dmInsert: Caption:=Format(lisMenuEditorInsertMenuTemplateIntoRootOfS, [FMenu.Name]);
    dmDelete: Caption:=lisMenuEditorDeleteSavedMenuTemplate;
  end;
  FTemplates:=TMenuTemplates.CreateForMode(FDialogMode);
  SetupGUI;
  PopulateTreeView;
end;

destructor TMenuTemplateDialog.Destroy;
begin
  FreeAndNil(FNewMenuTemplate);
  FreeAndNil(FTemplates);
  FreeAndNil(FPreview);
  inherited Destroy;
end;

procedure TMenuTemplateDialog.BExecuteDeleteClick(Sender: TObject);
begin
  DeleteSelectedFromConfigFile;
end;

procedure TMenuTemplateDialog.BExecuteInsertClick(Sender: TObject);
var
  mt: TMenuTemplate;
begin
  Assert(FTVTemplates.Selected<>nil,'TMenuTemplateDialog.BExecuteInsertClick: FTVTemplates.Selected is nil');
  mt:=TMenuTemplate(FTVTemplates.Selected.Data);
  FMenuToInsert:=FTemplates.Menu[FTemplates.GetIndexOfTemplate(mt)];
end;

procedure TMenuTemplateDialog.BExecuteSaveClick(Sender: TObject);
var
  trimmed: string;
begin
  trimmed:=Trim(FEDescription.Text);
  if (Length(trimmed) < 4) then
    begin
      IDEMessageDialogAb(lisMenuEditorInadequateDescription,
              Format(lisMenuEditorSIsNotASufficientDescriptionPleaseExpand,[trimmed]),
              mtWarning, [mbOK], False);
      FEDescription.SelectAll;
      FEDescription.SetFocus;
    end
  else
    begin
      FNewMenuTemplate.Description:=trimmed;
      FTemplates.AddTemplate(TMenuTemplate.MenuItemToString(FMenuToSave, trimmed), False);
      FEDescription.ReadOnly:=True;
      SaveMenuAsTemplate;
    end;
end;

procedure TMenuTemplateDialog.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TMenuTemplateDialog.CBDisplayChange(Sender: TObject);
begin
  FPreview.DisplayAsPopup:=FCBDisplay.Checked;
end;

procedure TMenuTemplateDialog.DeleteSelectedFromConfigFile;
var
  XMLConfig: TXMLConfig;

  procedure ExchangeConfigTemplates(aLower, aHigher: integer);
  var
    j: integer;
    valueN, valueS: string;
  begin
    j:=1;
    while not SameText(XMLConfig.GetValue(Format('menu_%d/subitem_%d/Name/Value',
                       [aLower, j]), 'nonexistent'), 'nonexistent') do
      begin
        XMLConfig.DeletePath(Format('menu_%d/subitem_%d',[aLower, j]));
        Inc(j);
      end;

    j:=1;
    valueN:=XMLConfig.GetValue(Format('menu_%d/subitem_%d/Name/Value',
                              [aHigher, j]), 'nonexistent');
    while not SameText(valueN, 'nonexistent') do
      begin
        XMLConfig.SetValue(Format('menu_%d/subitem_%d/Name/Value',[aLower, j]), valueN);
        valueS:=XMLConfig.GetValue(Format('menu_%d/subitem_%d/Shortcut/Value',
                                  [aHigher, j]), 'nonexistent');
        if not SameText(valueS, 'nonexistent') then
          XMLConfig.SetValue(Format('menu_%d/subitem_%d/Shortcut/Value',[aLower, j]), valueS);
        Inc(j);
        valueN:=XMLConfig.GetValue(Format('menu_%d/subitem_%d/Name/Value',
                              [aHigher, j]), 'nonexistent');
      end;

    valueN:=XMLConfig.GetValue(Format('menu_%d/Name/Value',[aHigher]), 'nonexistent');
    if not SameText(valueN, 'nonexistent') then
      XMLConfig.SetValue(Format('menu_%d/Name/Value',[aLower]), valueN);

    valueN:=XMLConfig.GetValue(Format('menu_%d/Description/Value',[aHigher]), 'nonexistent');
    if not SameText(valueN, 'nonexistent') then
      XMLConfig.SetValue(Format('menu_%d/Description/Value',[aLower]), valueN);
  end;

var
  highestOldIdx, i, idxToDelete: integer;
  cfgPath, s, desc, currDesc: string;
begin
  desc:=TMenuTemplate(FTVTemplates.Selected.Data).Description;
  cfgPath:=GetCfgPath;
  XMLConfig:=TXMLConfig.Create(cfgPath + DirectorySeparator + MenuTemplatesFilename);
  try
    i:=0;
    idxToDelete:= -1;
    repeat
      Inc(i);
      s:=XMLConfig.GetValue(Format('menu_%d/Name/Value',[i]), 'nonexistent');
      currDesc:=XMLConfig.GetValue(Format('menu_%d/Description/Value',[i]), 'noDescription');
      if (CompareText(desc, currDesc) = 0) then
        idxToDelete:=i;
    until CompareText(s, 'nonexistent') = 0;
    highestOldIdx:=Pred(i);
    Assert(idxToDelete>-1,'TMenuTemplateDialog.DeleteSelectedFromConfigFile: nonexistent template index');
    if (idxToDelete < highestOldIdx) then
      ExchangeConfigTemplates(idxToDelete, highestOldIdx);
    XMLConfig.DeletePath(Format('menu_%d',[highestOldIdx]));
    InvalidateFileStateCache;
    XMLConfig.Flush;
  finally
    XMLConfig.Free;
  end;
end;

procedure TMenuTemplateDialog.DeleteSelectedTemplate;
begin
  Assert(FTVTemplates.Selected<>nil,'TMenuTemplateDialog.DeleteSelectedTemplate: FTVTemplates.Selected is nil');
  DeleteSelectedFromConfigFile;
end;

procedure TMenuTemplateDialog.EDescriptionChange(Sender: TObject);
begin
  if (Length(FEDescription.Text) > 3) and not FBExecute.Enabled then
    FBExecute.Enabled:=True;
end;

procedure TMenuTemplateDialog.PopulateTreeView;
var
  mt: TMenuTemplate;
  i: integer;
  processed: string;
begin
  for i:=0 to FTemplates.MenuCount-1 do
  begin
    mt:=FTemplates.MenuTemplate[i];
    processed:=AmpersandStripped(mt.PrimaryItem);
    if mt.IsStandardTemplate then
      FTVTemplates.Items.AddChildObject(FStandardNode, processed, mt)
    else begin
      AppendStr(processed, Format(' [%s]',[mt.Description]));
      FTVTemplates.Items.AddChildObject(FSavedNode, processed, mt);
    end;
  end;
end;

procedure TMenuTemplateDialog.SaveMenuAsTemplate;
var
  s: string;
begin
  Assert(FMenuToSave<>nil,'TInsertTemplateDialog.SaveMenuAsTemplate: FMenuToSave is nil');
  Assert(FNewMenuTemplate<>nil,'TInsertTemplateDialog.SaveMenuAsTemplate: FNewMenuTemplate is nil');
  FTemplates.SaveTemplateToConfig(FNewMenuTemplate);
  s:=AmpersandStripped(FNewMenuTemplate.PrimaryItem);
  IDEMessageDialogAb(lisMenuEditorTemplateSaved, Format(
    lisMenuEditorANewMenuTemplateHasBeenSaved,
                     [FNewMenuTemplate.Description, s, FNewMenuTemplate.SubItemCount]),
                     mtInformation, [mbOK], False);
end;

procedure TMenuTemplateDialog.SetupGUI;
begin
  FPButtons:=TCustomPanel.Create(Self);
  with FPButtons do begin
    ControlStyle:=ControlStyle - [csSetCaption];
    BevelInner:=bvNone;
    BevelOuter:=bvNone;
    BorderSpacing.Around:=Margin;
    Align:=alBottom;
    AutoSize:=True;
    Parent:=Self;
  end;

  FBCancel:=TBitBtn.Create(Self);
  with FBCancel do begin
    Kind:=bkCancel;
    AutoSize:=True;
    BorderSpacing.Left:=2*Spacing;
    Align:=alRight;
    Left:=1;
    OnClick:=@CancelButtonClick;
    Parent:=FPButtons;
  end;

  FBExecute:=TBitBtn.Create(Self);
  with FBExecute do begin
    Kind:=bkOK;
    case FDialogMode of
      dmSave: begin
                Caption:=lisMenuEditorSaveMenuShownAsANewTemplate;
                OnClick:=@BExecuteSaveClick;
              end;
      dmInsert: begin
                  Caption:=lisMenuEditorInsertSelectedMenuTemplate;
                  OnClick:=@BExecuteInsertClick;
                end;
      dmDelete: begin
                  Caption:=lisMenuEditorDeleteSelectedMenuTemplate;
                  OnClick:=@BExecuteDeleteClick;
                end;
    end;
    AutoSize:=True;
    Align:=alRight;
    Left:=0;
    Enabled:=False;
    Parent:=FPButtons;
  end;

  FGChoose:=TGroupBox.Create(Self);
  with FGChoose do begin
    Align:=alClient;
    BorderSpacing.Around:=Margin;
    Parent:=Self;
  end;

  FPDescription:=TPanel.Create(Self);
  with FPDescription do begin
    Align:=alTop;
    BevelInner:=bvNone;
    BevelOuter:=bvNone;
    BorderSpacing.Bottom:=Margin;
    BorderSpacing.Top:=Margin;
    AutoSize:=True;
    Parent:=FGChoose;
  end;

  FEDescription:=TEdit.Create(Self);
  with FEDescription do begin
    Align:=alClient;
    BorderSpacing.Right:=Margin;
    Parent:=FPDescription;
  end;

  FLDescription:=TLabel.Create(Self);
  with FLDescription do begin
    AutoSize:=True;
    BorderSpacing.Around:=Margin;
    Align:=alLeft;
    Parent:=FPDescription;
  end;

  FTVTemplates:=TTreeView.Create(Self);
  with FTVTemplates do begin
    Align:=alLeft;
    Width:=200;
    Align:=alLeft;
    Indent:=Margin;
    Options:=[tvoAutoExpand, tvoAutoItemHeight, tvoKeepCollapsedNodes,
              tvoShowRoot, tvoNoDoubleClickExpand];
    BorderSpacing.Bottom:=Margin;
    BorderSpacing.Left:=Margin;
    OnAdvancedCustomDrawItem:=@TVAdvancedCustomDrawItem;
    OnEditing:=@TVEditing;
    Parent:=FGChoose;
  end;

  FSplitter:=TSplitter.Create(Self);
  FSplitter.Left:=FTVTemplates.BoundsRect.Right;
  FSplitter.Parent:=FGChoose;

  FPRight:=TCustomPanel.Create(Self);
  with FPRight do begin
    Width:=200;
    BevelInner:=bvNone;
    BevelOuter:=bvNone;
    BorderSpacing.Bottom:=Margin;
    BorderSpacing.Right:=Margin;
    Align:=alClient;
    Parent:=FGChoose;
  end;

  FCBDisplay:=TCheckBox.Create(Self);
  with FCBDisplay do begin
    Align:=alTop;
    Alignment:=taLeftJustify;
    BorderSpacing.Bottom:=Margin;
    BorderSpacing.Left:=Margin;
    AutoSize:=True;
    Caption:=lisMenuEditorDisplayPreviewAsPopupMenu;
    OnChange:=@CBDisplayChange;
    Parent:=FPRight;
  end;

  FScrollBoxPreview:=TScrollBox.Create(Self);
  with FScrollBoxPreview do begin
    Align:=alClient;
    TabStop:=False;
    Parent:=FPRight;
  end;
  FPreview:=TPreview.Create(Self);
  FPreview.Parent:=FScrollBoxPreview;

  case FDialogMode of
    dmSave: begin
      FSavedNode:=FTVTemplates.Items.AddFirst(nil, lisMenuEditorExistingSavedTemplates);
      FGChoose.Caption:=lisMenuEditorSaveMenuAsTemplateForFutureUse;
      FLDescription.Caption:=lisMenuEditorEnterAMenuDescription;
      FLDescription.FocusControl:=FEDescription;
    end;
    dmInsert: begin
      FStandardNode:=FTVTemplates.Items.AddFirst(nil, lisMenuEditorStandardTemplates);
      FSavedNode:=FTVTemplates.Items.Add(FStandardNode, lisMenuEditorSavedTemplates);
      FGChoose.Caption:=lisMenuEditorChooseTemplateToInsert;
      FLDescription.Caption:=lisMenuEditorTemplateDescription;
      FTVTemplates.OnSelectionChanged:=@TVSelectionChanged;
      FEDescription.ReadOnly:=True;
    end;
    dmDelete: begin
      FStandardNode:=nil;
      FSavedNode:=FTVTemplates.Items.AddFirst(nil, lisMenuEditorExistingSavedTemplates);
      FGChoose.Caption:=lisMenuEditorChooseTemplateToDelete;
      FLDescription.Caption:=lisMenuEditorTemplateDescription;
      FTVTemplates.OnSelectionChanged:=@TVSelectionChanged;
      FEDescription.ReadOnly:=True;
    end;
  end;
end;

procedure TMenuTemplateDialog.ShowPreview(aMenuTemplate: TMenuTemplate);
begin
  FPreview.Clear;
  if (aMenuTemplate <> nil) then
    FPreview.LoadTemplate(aMenuTemplate);
end;

procedure TMenuTemplateDialog.TVAdvancedCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  nodeR, textR: TRect;
begin
  DefaultDraw:=(Stage <> cdPrePaint);
  if DefaultDraw then Exit;

  nodeR:=Node.DisplayRect(False);
  textR:=Node.DisplayRect(True);
  if (Node.Level = 0) then begin
    Sender.Canvas.Font.Color:=clGray;
    Sender.Canvas.Font.Style:=[fsBold];
  end
  else begin
    Sender.Canvas.Font.Color:=clBlack;
    Sender.Canvas.Font.Style:=[];
  end;
  if (cdsSelected in State) and (Node.Level > 0) then
    Sender.Canvas.Brush.Color:=Sender.SelectionColor
  else Sender.Canvas.Brush.Color:=Sender.Color;
  Sender.Canvas.FillRect(nodeR);
  Sender.Canvas.TextOut(textR.Left, textR.Top, Node.Text);
end;

procedure TMenuTemplateDialog.TVEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  AllowEdit:=False;
end;

procedure TMenuTemplateDialog.TVSelectionChanged(Sender: TObject);
var
  tv: TTreeView absolute Sender;
  node: TTreeNode;
  mt: TMenuTemplate;
begin
  if not (Sender is TTreeView) then
    Exit;

  node:=tv.Selected;
  if (node = nil) or (node = FStandardNode) or (node = FSavedNode) or
     (node = FNoneSavedNode) or (node.Data = nil) then
  begin
    FBExecute.Enabled:=False;
    FEDescription.Text:='';
    ShowPreview(nil);
  end
  else begin
    Assert(node.Data<>nil,'TMenuTemplateDialog.TVSelectionChanged: node.Data is nil');
    mt:=TMenuTemplate(node.Data);
    FEDescription.Text:=mt.Description;
    ShowPreview(mt);
    if FDialogMode in [dmInsert, dmDelete] then
      FBExecute.Enabled:=True;
  end;
end;

procedure TMenuTemplateDialog.DoShowWindow;
var
  menuString: string;
  noneSaved: boolean;
begin
  inherited DoShowWindow;
  noneSaved:=not SavedTemplatesExist;
  case FDialogMode of

    dmSave: begin
      menuString:=TMenuTemplate.MenuItemToString(FMenuToSave, '');
      FNewMenuTemplate:=TMenuTemplate.CreateFromString(menuString);
      FPreview.LoadTemplate(FNewMenuTemplate);
      if noneSaved then
        FNoneSavedNode:=FTVTemplates.Items.AddChild(FSavedNode,lisMenuEditorNone);
      FEDescription.SetFocus;
      FEDescription.OnChange:=@EDescriptionChange;
    end;

    dmInsert: begin
      if noneSaved then
        FNoneSavedNode:=FTVTemplates.Items.AddChild(FSavedNode,lisMenuEditorNone);
      TVSelectionChanged(FTVTemplates);
      FTVTemplates.Selected:=FTVTemplates.Items[1];
      FTVTemplates.SetFocus;
    end;

    dmDelete: begin
      if noneSaved then begin
        IDEMessageDialogAb(lisMenuEditorNoUserSavedTemplates, lisMenuEditorThereAreNoUserSavedMenuTemplates,
                     mtInformation, [mbOK], False);
        ModalResult:=mrCancel;
      end
      else begin
        TVSelectionChanged(FTVTemplates);
        FTVTemplates.Selected:=FTVTemplates.Items[1];
        FTVTemplates.SetFocus;
      end;
    end;

  end;
end;

end.

