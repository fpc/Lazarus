program TabOrder;

{$mode objfpc}{$H+}

uses
  Interfaces, Classes, SysUtils, Forms, Controls, Buttons, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    NoteBook: TNotebook;
    Page1: TPage;
    Page2: TPage;
    Page3: TPage;
    CloseButton: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure WriteTabOrder;
  end;

{ TForm1 }

constructor TForm1.Create(TheOwner: TComponent);

  procedure CreatePage1;
  begin
    Label1:=TLabel.Create(Self);
    with Label1 do begin
      Name:='Label1';
      Parent:=Page1;
      SetBounds(10,10,250,Height);
      Caption:='TabOrder: First Edit below, then Edit above';
    end;
  
    Edit1:=TEdit.Create(Self);
    with Edit1 do begin
      Name:='Edit1';
      Parent:=Page1;
      SetBounds(10,40,100,Height);
    end;

    Edit2:=TEdit.Create(Self);
    with Edit2 do begin
      Name:='Edit2';
      Parent:=Page1;
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

begin
  inherited Create(TheOwner);
  Name:='Form1';
  Position:=poScreenCenter;
  SetInitialBounds(0,0,400,300);
  
  NoteBook:=TNotebook.Create(Self);
  with NoteBook do begin
    Name:='NoteBook';
    Parent:=Self;
    Align:=alTop;
    Height:=Parent.ClientHeight-40;
    
    Pages.Add('Page1');
    Page1:=Page[0];
    Page1.Name:='Page1';
    
    Pages.Add('Page2');
    Page2:=Page[1];
    Page2.Name:='Page2';

    Pages.Add('Page3');
    Page3:=Page[2];
    Page3.Name:='Page3';

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
    ModalResult:=mrCancel;
  end;
  
  CreatePage1;
  CreatePage2;
  CreatePage3;
  
  // define TabOrder
  CloseButton.TabStop:=true;
  CloseButton.TabOrder:=0;
  NoteBook.TabStop:=true;
  NoteBook.TabOrder:=1;
  Page1.TabStop:=true;
  Page1.TabOrder:=2;
  Page2.TabStop:=true;
  Page2.TabOrder:=3;
  Page3.TabStop:=true;
  Page3.TabOrder:=4;

  WriteTabOrder;
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

