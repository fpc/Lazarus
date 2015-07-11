program TabOrder;

{$mode objfpc}{$H+}

uses
  Interfaces, Classes, SysUtils, Forms, Controls, Buttons, StdCtrls, ExtCtrls,
  ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    PageCtrl: TPageControl;
    CloseButton: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure CloseButtonClick(Sender: TObject);
    procedure WriteTabOrder;
  end;

{ TForm1 }

constructor TForm1.Create(TheOwner: TComponent);

  procedure CreatePage1;
  begin
    Label1:=TLabel.Create(Self);
    with Label1 do begin
      Name:='Label1';
      Parent:=PageCtrl.Page[0];
      SetBounds(10,10,250,Height);
      Caption:='TabOrder: First Edit below, then Edit above';
    end;
  
    Edit1:=TEdit.Create(Self);
    with Edit1 do begin
      Name:='Edit1';
      Parent:=PageCtrl.Page[0];
      SetBounds(10,40,100,Height);
    end;

    Edit2:=TEdit.Create(Self);
    with Edit2 do begin
      Name:='Edit2';
      Parent:=PageCtrl.Page[0];
      SetBounds(10,70,100,Height);
    end;
    
    // define Taborder
    Edit2.TabStop:=true;
    Edit2.TabOrder:=0;
    Edit1.TabStop:=true;
    Edit1.TabOrder:=1;
  end;

  procedure CreatePage2;
  begin
  end;

  procedure CreatePage3;
  begin
  end;

var
  i: integer;
begin
  inherited CreateNew(TheOwner, 1);
  Name:='Form1';
  Position:=poScreenCenter;
  SetInitialBounds(0,0,400,300);
  
  PageCtrl:=TPageControl.Create(Self);
  with PageCtrl do begin
    Name:='PageCtrl';
    Parent:=Self;
    Align:=alTop;
    Height:=Parent.ClientHeight-40;
    
    for i:=1 to 3 do
      with TTabSheet.Create(PageCtrl) do
      begin
        PageControl:=PageCtrl;
        Name:='Page'+IntToStr(i);
        Caption:=Name;
      end;
    Anchors:=[akLeft,akTop,akRight,akBottom];
    PageIndex:=0;
  end;
  
  CloseButton:=TButton.Create(Self);
  with CloseButton do begin
    Name:='CloseButton';
    Parent:=Self;
    Caption:='Close';
    Anchors:=[akRight,akBottom];
    SetBounds(Parent.ClientWidth-110,Parent.ClientHeight-35,100,Height);
    OnClick:=@CloseButtonClick;
  end;
  
  CreatePage1;
  CreatePage2;
  CreatePage3;
  
  // define TabOrder
  CloseButton.TabStop:=true;
  CloseButton.TabOrder:=0;
  PageCtrl.TabStop:=true;
  PageCtrl.TabOrder:=1;
  PageCtrl.Page[0].TabStop:=true;
  PageCtrl.Page[0].TabOrder:=2;
  PageCtrl.Page[1].TabStop:=true;
  PageCtrl.Page[1].TabOrder:=3;
  PageCtrl.Page[2].TabStop:=true;
  PageCtrl.Page[2].TabOrder:=4;

  WriteTabOrder;
end;

procedure TForm1.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.WriteTabOrder;

  procedure WriteTabOrderTree(const Prefix: string; AControl: TControl);
  var
    i: Integer;
  begin
    write(Prefix,AControl.Name);
    if AControl is TWinControl then
      write('  TabOrder=',TWinControl(AControl).TabOrder,
            ' TabStop=',TWinControl(AControl).TabStop);
    writeln('');
    if AControl is TWinControl then
      for i:=0 to TWinControl(AControl).ControlCount-1 do begin
        WriteTabOrderTree(Prefix+'  ',TWinControl(AControl).Controls[i]);
      end;
  end;

begin
  WriteTabOrderTree('',Self);
end;

var
  Form1: TForm1;
begin
  Application.Initialize;
  Application.CreateForm(TForm1,Form1);
  Application.Run;
end.

