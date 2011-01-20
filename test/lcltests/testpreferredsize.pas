{
  Test that empty ScrollBox with AutoScroll shows no scrollbars:
     ./runtests --format=plain --suite=TestScrollBoxEmpty

  Test that ScrollBox with AutoScroll shows/hides scrollbars:
        ./runtests --format=plain --suite=TestScrollBoxAutoShowHideScrollbars


}
unit testpreferredsize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, StdCtrls, ExtCtrls, fpcunit,
  WSControls, testglobals;

type

  { TTestPreferredSize }

  TTestPreferredSize = class(TTestCase)
  published
    procedure TestGroupBoxPreferredSize1;
    procedure TestScrollBoxEmpty;
    procedure TestScrollBoxAutoShowHideScrollbars;
  end;

implementation

{ TTestPreferredSize }

procedure TTestPreferredSize.TestGroupBoxPreferredSize1;
var
  Form1: TForm;
  GroupBox1: TGroupBox;
  w1: integer;
  h1: integer;
  w2: Integer;
  h2: Integer;
begin
  // create a groupbox on a form
  Form1:=TForm.Create(nil);
  Form1.SetBounds(100,100,300,300);
  GroupBox1:=TGroupBox.Create(Form1);
  GroupBox1.SetBounds(10,10,100,100);
  GroupBox1.Parent:=Form1;
  GroupBox1.Caption:='GroupBox1';
  Form1.Show;
  Application.ProcessMessages;

  // get the preferredsize of the groupbox with a size of 100x100
  w1:=0;
  h1:=0;
  GroupBox1.GetPreferredSize(w1,h1,true,false);
  //writeln('TTestPreferredSize.TestGroupBox1 ',w1,',',h1);

  // get the preferredsize of the groupbox with a size of 10x100
  GroupBox1.Width:=10;
  w2:=0;
  h2:=0;
  GroupBox1.GetPreferredSize(w2,h2,true,false);
  //writeln('TTestPreferredSize.TestGroupBox1 ',w2,',',h2);

  // the preferredsize must be independent of the the current width,height
  AssertEquals('TGroupBox.PreferredSize changed after SetBounds: ',true,(w1=w2) and (h1=h2));

  Form1.Free;
  Application.ProcessMessages;
end;

procedure TTestPreferredSize.TestScrollBoxEmpty;
var
  Form1: TForm;
  ScrollBox1: TScrollBox;
begin
  // create an empty scrollbox on a form
  Form1:=TForm.Create(nil);
  Form1.SetBounds(100,100,300,300);
  ScrollBox1:=TScrollBox.Create(Form1);
  ScrollBox1.SetBounds(10,10,100,100);
  ScrollBox1.AutoScroll:=true;
  ScrollBox1.Parent:=Form1;
  Form1.Show;
  Application.ProcessMessages;

  AssertEquals('TScrollBox: Empty, AutoScroll=true, but HorzScrollBar.IsScrollBarVisible is true',false,ScrollBox1.HorzScrollBar.IsScrollBarVisible);
  AssertEquals('TScrollBox: Empty, AutoScroll=true, but VertScrollBar.IsScrollBarVisible is true',false,ScrollBox1.VertScrollBar.IsScrollBarVisible);

  Form1.Free;
  Application.ProcessMessages;
end;

procedure TTestPreferredSize.TestScrollBoxAutoShowHideScrollbars;
var
  Form1: TForm;
  ScrollBox1: TScrollBox;
  Panel1: TPanel;
  IntfPreferredWidth: integer;
  IntfPreferredHeight: integer;
begin
  // create a scrollbox on a form and put a small panel into the box
  Form1:=TForm.Create(nil);
  Form1.SetBounds(100,100,300,300);
  ScrollBox1:=TScrollBox.Create(Form1);
  ScrollBox1.SetBounds(10,10,100,100);
  ScrollBox1.AutoScroll:=true;
  ScrollBox1.Parent:=Form1;
  Panel1:=TPanel.Create(Form1);
  Panel1.Caption:='Panel1';
  Panel1.SetBounds(0,0,60,50);
  Panel1.Constraints.MinWidth:=10;
  Panel1.Constraints.MinHeight:=10;
  Panel1.Parent:=ScrollBox1;
  Form1.Show;
  Application.ProcessMessages;

  IntfPreferredWidth:=0;
  IntfPreferredHeight:=0;
  TWSWinControlClass(ScrollBox1.WidgetSetClass).GetPreferredSize(ScrollBox1,
                             IntfPreferredWidth, IntfPreferredHeight, false);
  AssertEquals('ScrollBox must have small interface preferred width',true,IntfPreferredWidth<8);
  AssertEquals('ScrollBox must have small interface preferred height',true,IntfPreferredHeight<8);

  //writeln('TTestPreferredSize.TestScrollBoxOneChildPanel Range=',ScrollBox1.HorzScrollBar.Range,' ',ScrollBox1.HorzScrollBar.Page,' ',ScrollBox1.HorzScrollBar.Visible);
  AssertEquals('ScrollBox1.HorzScrollBar.Range should be the needed Right of all childs 60',60,ScrollBox1.HorzScrollBar.Range);
  AssertEquals('ScrollBox1.VertScrollBar.Range should be the needed Bottom of all childs 50',50,ScrollBox1.VertScrollBar.Range);
  AssertEquals('ScrollBox shows HorzScrollBar for empty box',false,ScrollBox1.HorzScrollBar.IsScrollBarVisible);
  AssertEquals('ScrollBox shows VertScrollBar for empty box',false,ScrollBox1.VertScrollBar.IsScrollBarVisible);

  // now enlarge the panel, so that a HorzScrollBar is needed
  Panel1.Width:=150;
  Application.ProcessMessages;

  AssertEquals('ScrollBox1.HorzScrollBar.Range should be the needed Right of all childs 150',150,ScrollBox1.HorzScrollBar.Range);
  AssertEquals('ScrollBox1.VertScrollBar.Range should be the needed Bottom of all childs 50',50,ScrollBox1.VertScrollBar.Range);
  AssertEquals('ScrollBox must show HorzScrollBar for oversized panel',true,ScrollBox1.HorzScrollBar.IsScrollBarVisible);
  AssertEquals('ScrollBox shows VertScrollBar for small panel',false,ScrollBox1.VertScrollBar.IsScrollBarVisible);

  // now Align the panel, so that the panel fills the whole client area
  // no scrollbars should be visible
  Panel1.Align:=alClient;
  Application.ProcessMessages;

  writeln('TTestPreferredSize.TestScrollBoxOneChildPanel Panel1.BoundsRect=',dbgs(Panel1.BoundsRect),' ScrollBox1.ClientRect=',dbgs(ScrollBox1.ClientRect));
  AssertEquals('Panel1.Align=alClient, Panel1.Left should be 0',0,Panel1.Left);
  AssertEquals('Panel1.Align=alClient, Panel1.Top should be 0',0,Panel1.Top);
  AssertEquals('Panel1.Align=alClient, Panel1.Right should be ScrollBox1.ClientWidth',ScrollBox1.ClientWidth,Panel1.Left+Panel1.Width);
  AssertEquals('Panel1.Align=alClient, Panel1.Bottom should be ScrollBox1.ClientHeight',ScrollBox1.ClientHeight,Panel1.Top+Panel1.Height);
  AssertEquals('ScrollBox shows HorzScrollBar for fitting panel',false,ScrollBox1.HorzScrollBar.IsScrollBarVisible);
  AssertEquals('ScrollBox shows VertScrollBar for fitting panel',false,ScrollBox1.VertScrollBar.IsScrollBarVisible);
  AssertEquals('ScrollBox1.HorzScrollBar.Range should be the needed Right of all childs',Panel1.Constraints.MinWidth,ScrollBox1.HorzScrollBar.Range);
  AssertEquals('ScrollBox1.VertScrollBar.Range should be the needed Bottom of all childs',Panel1.Constraints.MinHeight,ScrollBox1.VertScrollBar.Range);


  Form1.Free;
  Application.ProcessMessages;
end;

initialization
  AddToLCLTestSuite(TTestPreferredSize);

end.

