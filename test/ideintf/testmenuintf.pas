unit TestMenuIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, Menus, MenuIntf, testglobals;

type

  { TTestMenuIntfDlg }

  TTestMenuIntfDlg = class(TForm)
    TestMainMenuIntf1: TMainMenu;
    TestPopupMenuIntf1: TPopupMenu;
  private
  public
  end;

  { TMenuIntfTest }

  TTestMenuIntf = class(TTestCase)
  private
    FDialog: TTestMenuIntfDlg;
    FMainMenuRoot: TIDEMenuSection;
    FPopupMenuRoot: TIDEMenuSection;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  public
    function CreateCommand(aParent: TIDEMenuSection; aName: string): TIDEMenuCommand;
    function CreateSection(aParent: TIDEMenuSection; aName: string): TIDEMenuSection;
    property Dialog: TTestMenuIntfDlg read FDialog;
    property MainMenuRoot: TIDEMenuSection read FMainMenuRoot;
    property PopupMenuRoot: TIDEMenuSection read FPopupMenuRoot;
  published
    procedure TestEmptyMainMenu;
    procedure TestSinglePopupMenu;
    procedure TestPopupMenuList;
    procedure TestPopupMenuLogicalSection;
    procedure TestPopupMenuLogicalSections;
    procedure TestPopupMenuSubMenu;
    procedure TestPopupMenuVisible;
    procedure TestPopupMenuClearHiddenSection;
  end;

var
  TestMenuIntfDlg: TTestMenuIntfDlg;

implementation

{$R *.lfm}


{ TMenuIntfTest }

procedure TTestMenuIntf.SetUp;
begin
  inherited SetUp;
  IDEMenuRoots:=TIDEMenuRoots.Create;
  FMainMenuRoot:=RegisterIDEMenuRoot('MainMenuRoot');
  FPopupMenuRoot:=RegisterIDEMenuRoot('PopupMenuRoot');
  FDialog:=TTestMenuIntfDlg.Create(nil);
end;

procedure TTestMenuIntf.TearDown;
begin
  FreeAndNil(FDialog);
  FMainMenuRoot:=nil;
  FPopupMenuRoot:=nil;
  FreeAndNil(IDEMenuRoots);
  inherited TearDown;
end;

function TTestMenuIntf.CreateCommand(aParent: TIDEMenuSection; aName: string): TIDEMenuCommand;
begin
  Result:=RegisterIDEMenuCommand(aParent,aName,aName);
end;

function TTestMenuIntf.CreateSection(aParent: TIDEMenuSection; aName: string
  ): TIDEMenuSection;
begin
  Result:=RegisterIDEMenuSection(aParent,aName);
end;

procedure TTestMenuIntf.TestEmptyMainMenu;
begin
  FMainMenuRoot.MenuItem:=Dialog.TestMainMenuIntf1.Items;
  MainMenuRoot.ConsistencyCheck;
end;

procedure TTestMenuIntf.TestSinglePopupMenu;
begin
  FPopupMenuRoot.MenuItem:=Dialog.TestPopupMenuIntf1.Items;
  PopupMenuRoot.ConsistencyCheck;
  RegisterIDEMenuCommand(PopupMenuRoot,'Help','Help');
  PopupMenuRoot.ConsistencyCheck;
end;

procedure TTestMenuIntf.TestPopupMenuList;
var
  i: Integer;
begin
  FPopupMenuRoot.MenuItem:=Dialog.TestPopupMenuIntf1.Items;
  PopupMenuRoot.ConsistencyCheck;
  for i:=1 to 3 do begin
    RegisterIDEMenuCommand(PopupMenuRoot,'Item'+IntToStr(i),'Item'+IntToStr(i));
    PopupMenuRoot.ConsistencyCheck;
  end;
end;

procedure TTestMenuIntf.TestPopupMenuLogicalSection;
var
  Section1: TIDEMenuSection;
begin
  FPopupMenuRoot.MenuItem:=Dialog.TestPopupMenuIntf1.Items;
  PopupMenuRoot.ConsistencyCheck;
  Section1:=RegisterIDEMenuSection(PopupMenuRoot,'Section1');
  PopupMenuRoot.ConsistencyCheck;
  AssertEquals('Section1.ChildrenAsSubMenu',false,Section1.ChildrenAsSubMenu);
  Section1.ChildrenAsSubMenu:=false;
  PopupMenuRoot.ConsistencyCheck;
  RegisterIDEMenuCommand(Section1,'Item1','Item1');
  PopupMenuRoot.ConsistencyCheck;
end;

procedure TTestMenuIntf.TestPopupMenuLogicalSections;
var
  Section1, Section2: TIDEMenuSection;
  Item1, Item2: TIDEMenuCommand;
begin
  FPopupMenuRoot.MenuItem:=Dialog.TestPopupMenuIntf1.Items;
  PopupMenuRoot.ConsistencyCheck;

  Section1:=RegisterIDEMenuSection(PopupMenuRoot,'Section1');
  AssertEquals('Section1.ChildrenAsSubMenu',false,Section1.ChildrenAsSubMenu);
  PopupMenuRoot.ConsistencyCheck;
  Item1:=RegisterIDEMenuCommand(Section1,'Item1','Item1');
  PopupMenuRoot.ConsistencyCheck;

  Section2:=RegisterIDEMenuSection(PopupMenuRoot,'Section2');
  AssertEquals('Section2.ChildrenAsSubMenu',false,Section2.ChildrenAsSubMenu);
  PopupMenuRoot.ConsistencyCheck;
  PopupMenuRoot.ConsistencyCheck;
  Item2:=RegisterIDEMenuCommand(Section2,'Item2','Item2');
  PopupMenuRoot.ConsistencyCheck;

  AssertEquals('Section1.Section=PopupMenuRoot',true,Section1.Section=PopupMenuRoot);
  AssertEquals('Section2.Section=PopupMenuRoot',true,Section2.Section=PopupMenuRoot);
  AssertEquals('Section1.SectionIndex',0,Section1.SectionIndex);
  AssertEquals('Section2.SectionIndex',1,Section2.SectionIndex);
  AssertEquals('Section1.Visible',true,Section1.Visible);
  AssertEquals('Section2.Visible',true,Section2.Visible);
  AssertEquals('Section1.VisibleCommandCount',1,Section1.VisibleCommandCount);
  AssertEquals('Section2.VisibleCommandCount',1,Section2.VisibleCommandCount);
  AssertEquals('Section1.VisibleActive',true,Section1.VisibleActive);
  AssertEquals('Section2.VisibleActive',true,Section2.VisibleActive);
  AssertEquals('Section1.NeedTopSeparator',false,Section1.NeedTopSeparator);
  AssertEquals('Section1.NeedBottomSeparator',false,Section1.NeedBottomSeparator);
  AssertEquals('Section2.NeedTopSeparator',true,Section2.NeedTopSeparator);
  AssertEquals('Section2.NeedBottomSeparator',false,Section2.NeedBottomSeparator);

  AssertEquals('Section1.TopSeparator',false,Section1.TopSeparator<>nil);
  AssertEquals('Section1.MenuItem',false,Section1.MenuItem<>nil);
  AssertEquals('Section1.BottomSeparator',false,Section1.BottomSeparator<>nil);
  AssertEquals('Item1.MenuItem',true,Item1.MenuItem<>nil);
  AssertEquals('Section2.TopSeparator',true,Section2.TopSeparator<>nil);
  AssertEquals('Section2.MenuItem',false,Section2.MenuItem<>nil);
  AssertEquals('Section2.BottomSeparator',false,Section2.BottomSeparator<>nil);
  AssertEquals('Item2.MenuItem',true,Item2.MenuItem<>nil);

  AssertEquals('PopupMenuRoot.MenuItem.Count',3,PopupMenuRoot.MenuItem.Count);
  AssertEquals('Item1.MenuItem=PopupMenuRoot.MenuItem[0]',true,Item1.MenuItem=PopupMenuRoot.MenuItem[0]);
  AssertEquals('Section2.TopSeparator=PopupMenuRoot.MenuItem[1]',true,Section2.TopSeparator=PopupMenuRoot.MenuItem[1]);
  AssertEquals('Item2.MenuItem=PopupMenuRoot.MenuItem[2]',true,Item2.MenuItem=PopupMenuRoot.MenuItem[2]);
end;

procedure TTestMenuIntf.TestPopupMenuSubMenu;
var
  Section1: TIDEMenuSection;
  Item1: TIDEMenuCommand;
begin
  FPopupMenuRoot.MenuItem:=Dialog.TestPopupMenuIntf1.Items;
  PopupMenuRoot.ConsistencyCheck;
  Section1:=RegisterIDESubMenu(PopupMenuRoot,'Section1','Section1');
  AssertEquals('Section1.ChildrenAsSubMenu',true,Section1.ChildrenAsSubMenu);
  AssertEquals('Section1.Section=PopupMenuRoot',true,Section1.Section=PopupMenuRoot);
  AssertEquals('Section1.SectionIndex',0,Section1.SectionIndex);
  AssertEquals('Section1.Visible',true,Section1.Visible);
  PopupMenuRoot.ConsistencyCheck;

  Item1:=RegisterIDEMenuCommand(Section1,'Item1','Item1');
  AssertEquals('Item1.Visible',true,Item1.Visible);
  PopupMenuRoot.ConsistencyCheck;

  AssertEquals('Section1.Count',1,Section1.Count);
  AssertEquals('Section1[0]=Item1',true,Section1[0]=Item1);
  AssertEquals('Section1.VisibleCommandCount',1,Section1.VisibleCommandCount);
  AssertEquals('Section1.VisibleActive',true,Section1.VisibleActive);
  AssertEquals('Section1.NeedTopSeparator',false,Section1.NeedTopSeparator);
  AssertEquals('Section1.NeedBottomSeparator',false,Section1.NeedBottomSeparator);

  AssertEquals('Section1.TopSeparator',false,Section1.TopSeparator<>nil);
  AssertEquals('Section1.MenuItem',true,Section1.MenuItem<>nil);
  AssertEquals('Section1.BottomSeparator',false,Section1.BottomSeparator<>nil);
  AssertEquals('Item1.MenuItem',true,Item1.MenuItem<>nil);

  AssertEquals('PopupMenuRoot.MenuItem.Count',1,PopupMenuRoot.MenuItem.Count);
  AssertEquals('Section1.MenuItem=PopupMenuRoot.MenuItem[0]',true,Section1.MenuItem=PopupMenuRoot.MenuItem[0]);
  AssertEquals('Section1.MenuItem.Count',1,Section1.MenuItem.Count);
  AssertEquals('Item1.MenuItem=Section1.MenuItem[0]',true,Item1.MenuItem=Section1.MenuItem[0]);
end;

procedure TTestMenuIntf.TestPopupMenuVisible;
var
  LogSection1, SubMenu2, LogSection2: TIDEMenuSection;
  Item1, Item2, Item3, Item4: TIDEMenuCommand;
begin
  FPopupMenuRoot.MenuItem:=Dialog.TestPopupMenuIntf1.Items;
  PopupMenuRoot.ConsistencyCheck;

  LogSection1:=RegisterIDEMenuSection(PopupMenuRoot,'LogSection1');
  PopupMenuRoot.ConsistencyCheck;
  Item1:=RegisterIDEMenuCommand(LogSection1,'Item1','Item1');
  PopupMenuRoot.ConsistencyCheck;
  AssertEquals('A LogSection1.VisibleActive',true,LogSection1.VisibleActive);
  AssertEquals('A LogSection1.VisibleCommandCount',1,LogSection1.VisibleCommandCount);

  SubMenu2:=RegisterIDESubMenu(PopupMenuRoot,'SubMenu2','SubMenu2');
  PopupMenuRoot.ConsistencyCheck;
  Item2:=RegisterIDEMenuCommand(SubMenu2,'Item2','Item2');
  PopupMenuRoot.ConsistencyCheck;
  AssertEquals('B SubMenu2.VisibleActive',true,SubMenu2.VisibleActive);
  AssertEquals('B SubMenu2.VisibleCommandCount',1,SubMenu2.VisibleCommandCount);

  LogSection2:=RegisterIDEMenuSection(PopupMenuRoot,'LogSection2');
  PopupMenuRoot.ConsistencyCheck;
  Item3:=RegisterIDEMenuCommand(LogSection2,'Item3','Item3');
  Item4:=RegisterIDEMenuCommand(LogSection2,'Item4','Item4');
  PopupMenuRoot.ConsistencyCheck;
  AssertEquals('C LogSection2.VisibleActive',true,LogSection2.VisibleActive);
  AssertEquals('C LogSection2.VisibleCommandCount',2,LogSection2.VisibleCommandCount);

  AssertEquals('D LogSection1.TopSeparator',false,LogSection1.TopSeparator<>nil);
  AssertEquals('D LogSection1.BottomSeparator',true,LogSection1.BottomSeparator<>nil);
  AssertEquals('D LogSection2.TopSeparator',true,LogSection2.TopSeparator<>nil);
  AssertEquals('D LogSection2.BottomSeparator',false,LogSection2.BottomSeparator<>nil);

  // hide Item1 -> auto hides LogSection1
  Item1.Visible:=false;
  PopupMenuRoot.ConsistencyCheck;
  AssertEquals('E LogSection1.VisibleActive',false,LogSection1.VisibleActive);
  AssertEquals('E LogSection1.VisibleCommandCount',0,LogSection1.VisibleCommandCount);
  AssertEquals('E LogSection1.TopSeparator',false,LogSection1.TopSeparator<>nil);
  AssertEquals('E LogSection1.BottomSeparator',false,LogSection1.BottomSeparator<>nil);

  // show Item1 -> auto shows LogSection1
  Item1.Visible:=true;
  PopupMenuRoot.ConsistencyCheck;
  AssertEquals('F LogSection1.VisibleActive',true,LogSection1.VisibleActive);
  AssertEquals('F LogSection1.VisibleCommandCount',1,LogSection1.VisibleCommandCount);
  AssertEquals('F LogSection1.TopSeparator',false,LogSection1.TopSeparator<>nil);
  AssertEquals('F LogSection1.BottomSeparator',true,LogSection1.BottomSeparator<>nil);

  // hide Item2 -> auto hides SubMenu2
  Item2.Visible:=false;
  PopupMenuRoot.ConsistencyCheck;
  AssertEquals('G SubMenu2.VisibleActive',false,SubMenu2.VisibleActive);
  AssertEquals('G SubMenu2.VisibleCommandCount',0,SubMenu2.VisibleCommandCount);
  AssertEquals('G LogSection1.TopSeparator',false,LogSection1.TopSeparator<>nil);
  AssertEquals('G LogSection1.BottomSeparator',false,LogSection1.BottomSeparator<>nil);
  AssertEquals('G LogSection2.TopSeparator',true,LogSection2.TopSeparator<>nil);
  AssertEquals('G LogSection2.BottomSeparator',false,LogSection2.BottomSeparator<>nil);

  // show Item2 -> auto shows SubMenu2
  Item2.Visible:=true;
  PopupMenuRoot.ConsistencyCheck;
  AssertEquals('H SubMenu2.VisibleActive',true,SubMenu2.VisibleActive);
  AssertEquals('H SubMenu2.VisibleCommandCount',1,SubMenu2.VisibleCommandCount);
  AssertEquals('H LogSection1.TopSeparator',false,LogSection1.TopSeparator<>nil);
  AssertEquals('H LogSection1.BottomSeparator',true,LogSection1.BottomSeparator<>nil);
  AssertEquals('H LogSection2.TopSeparator',true,LogSection2.TopSeparator<>nil);
  AssertEquals('H LogSection2.BottomSeparator',false,LogSection2.BottomSeparator<>nil);

  // hide Item3, Item4 still visible
  Item3.Visible:=false;
  PopupMenuRoot.ConsistencyCheck;
  AssertEquals('I LogSection2.VisibleActive',true,LogSection2.VisibleActive);
  AssertEquals('I LogSection2.VisibleCommandCount',1,LogSection2.VisibleCommandCount);

  // hide Item4 -> auto hide LogSection2
  Item4.Visible:=false;
  PopupMenuRoot.ConsistencyCheck;
  AssertEquals('J LogSection2.VisibleActive',false,LogSection2.VisibleActive);
  AssertEquals('J LogSection2.VisibleCommandCount',0,LogSection2.VisibleCommandCount);
  AssertEquals('J LogSection1.BottomSeparator',true,LogSection1.BottomSeparator<>nil);
  AssertEquals('J LogSection2.TopSeparator',false,LogSection2.TopSeparator<>nil);
  AssertEquals('J LogSection2.BottomSeparator',false,LogSection2.BottomSeparator<>nil);

  // show Item3 -> auto shows LogSection2
  Item3.Visible:=true;
  PopupMenuRoot.ConsistencyCheck;
  AssertEquals('K LogSection2.VisibleActive',true,LogSection2.VisibleActive);
  AssertEquals('K LogSection2.VisibleCommandCount',1,LogSection2.VisibleCommandCount);
  AssertEquals('K LogSection2.TopSeparator',true,LogSection2.TopSeparator<>nil);
  AssertEquals('K LogSection2.BottomSeparator',false,LogSection2.BottomSeparator<>nil);
end;

procedure TTestMenuIntf.TestPopupMenuClearHiddenSection;
var
  LogSection1, SubSection2: TIDEMenuSection;
  Item2: TIDEMenuCommand;
begin
  FPopupMenuRoot.MenuItem:=Dialog.TestPopupMenuIntf1.Items;
  PopupMenuRoot.ConsistencyCheck;

  LogSection1:=RegisterIDEMenuSection(PopupMenuRoot,'LogSection1');
  PopupMenuRoot.ConsistencyCheck;
  RegisterIDEMenuCommand(LogSection1,'Item1','Item1');
  PopupMenuRoot.ConsistencyCheck;

  SubSection2:=RegisterIDEMenuSection(LogSection1,'SubSection2');
  PopupMenuRoot.ConsistencyCheck;
  Item2:=RegisterIDEMenuCommand(SubSection2,'Item2','Item2');
  PopupMenuRoot.ConsistencyCheck;

  //writeln('TTestMenuIntf.TestPopupMenuClearHiddenSection START');
  AssertEquals('LogSection1.VisibleActive',true,LogSection1.VisibleActive);
  AssertEquals('LogSection1.VisibleCommandCount',2,LogSection1.VisibleCommandCount);
  AssertEquals('SubSection2.VisibleActive',true,SubSection2.VisibleActive);
  AssertEquals('SubSection2.VisibleCommandCount',1,SubSection2.VisibleCommandCount);

  // hide SubSection2 -> LogSection1 looses one command
  //writeln('TTestMenuIntf.TestPopupMenuClearHiddenSection hide SubSection2 -> LogSection1 looses one command');
  SubSection2.Visible:=false;
  PopupMenuRoot.ConsistencyCheck;
  AssertEquals('LogSection1.VisibleActive',true,LogSection1.VisibleActive);
  AssertEquals('LogSection1.VisibleCommandCount',1,LogSection1.VisibleCommandCount);
  AssertEquals('SubSection2.VisibleActive',false,SubSection2.VisibleActive);
  AssertEquals('SubSection2.VisibleCommandCount',1,SubSection2.VisibleCommandCount);

  // hide Item2 -> no effect on LogSection1
  //writeln('TTestMenuIntf.TestPopupMenuClearHiddenSection hide Item2 -> no effect on LogSection1');
  Item2.Visible:=false;
  PopupMenuRoot.ConsistencyCheck;
  AssertEquals('LogSection1.VisibleActive',true,LogSection1.VisibleActive);
  AssertEquals('LogSection1.VisibleCommandCount',1,LogSection1.VisibleCommandCount);
  AssertEquals('SubSection2.VisibleActive',false,SubSection2.VisibleActive);
  AssertEquals('SubSection2.VisibleCommandCount',0,SubSection2.VisibleCommandCount);

  // show Item2 -> no effect on LogSection1
  //writeln('TTestMenuIntf.TestPopupMenuClearHiddenSection show Item2 -> no effect on LogSection1');
  Item2.Visible:=true;
  PopupMenuRoot.ConsistencyCheck;
  AssertEquals('LogSection1.VisibleActive',true,LogSection1.VisibleActive);
  AssertEquals('LogSection1.VisibleCommandCount',1,LogSection1.VisibleCommandCount);
  AssertEquals('SubSection2.VisibleActive',false,SubSection2.VisibleActive);
  AssertEquals('SubSection2.VisibleCommandCount',1,SubSection2.VisibleCommandCount);

  // clear SubSection2 -> no effect on LogSection1
  //writeln('TTestMenuIntf.TestPopupMenuClearHiddenSection clear SubSection2 -> no effect on LogSection1');
  SubSection2.Clear;
  Item2:=nil;
  PopupMenuRoot.ConsistencyCheck;
  AssertEquals('LogSection1.VisibleActive',true,LogSection1.VisibleActive);
  AssertEquals('LogSection1.VisibleCommandCount',1,LogSection1.VisibleCommandCount);
  AssertEquals('SubSection2.VisibleActive',false,SubSection2.VisibleActive);
  AssertEquals('SubSection2.VisibleCommandCount',0,SubSection2.VisibleCommandCount);
end;

initialization
  AddToIDEIntfTestSuite(TTestMenuIntf);

end.

