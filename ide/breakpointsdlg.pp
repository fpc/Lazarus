unit breakpointsdlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources, StdCtrls,Buttons,Extctrls,ComCtrls;

type
  TbpAction = (bpBreak);
  TBreakPointAddedEvent = procedure (sender : TObject; Expression : String) of Object;
  TBreakPointsdlg = class(TForm)
    ListView1: TListView;
  private
    { private declarations }
    FOnBreakpointAddedEvent : TBreakPointAddedEvent;
    
  protected
    Procedure ListView1KeyDown(Sender: TObject; var Key: Word; Shift:TShiftState);
  public
    { public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    Procedure AddBreakPoint(UnitName : String; Line : Integer);
    Procedure DeleteBreakPoint(UnitName : String; Line : Integer);
    property OnBreakpointAddedEvent : TBreakPointAddedEvent read FOnBreakPointAddedEvent write FOnBreakPointAddedEvent;

  end;

{  TInsertWatch = class(TForm)
    lblExpression : TLabel;
    lblRepCount   : TLabel;
    lblDigits     : TLabel;
    cbEnabled    : TCHeckbox;
    cbAllowFunc  : TCheckbox;
    Style        : TRadioGroup;
    btnOK        : TButton;
    btnCancel    : TButton;
    btnHelp      : TButton;
    edtExpression: TEdit;
    edtRepCount  : TEdit;
    edtDigits    : TEdit;
  private

  public
    constructor Create(AOWner : TCOmponent); override;
    destructor Destroy; override;
  end;
 }
var
  Breakpoints_Dlg  : TBreakPointsDlg;
//  InsertWatch  : TInsertWatch;
implementation

constructor TBreakPointsdlg.Create(AOwner : TComponent);
Begin
  inherited;
  if LazarusResources.Find(Classname)=nil then
  begin
  ListView1 := TListView.Create(self);
  with ListView1 do
    Begin
      Parent := self;
      Align := alClient;
      Visible := True;
      Name := 'ListView1';
      Columns.Clear;
      Columns.Updating := TRue;
      Columns.Add('Filename/Address');
      Columns.Add('Line/Length');
      Columns.Add('Condition');
      Columns.Add('Action');
      Columns.Add('Pass Count');
      Columns.Add('Group');
      Columns.Updating := False;
//Example alignment of columns.
//      Columns.Item[1].Alignment := caRight;
      ViewStyle := vsReport;
      Sorted := True;
      OnKeyDown := @ListView1KeyDown;
      MultiSelect := True;
    end;
//ListView does not accpet keys unless the mouse is held down over it
//so temporarily I do this:
  OnKeyDown := @ListView1KeyDown;

  Caption := 'Breakpoints';
  Name := 'BreakPointsDlg';
  Width := 350;
  Height := 100;

  end;


End;

destructor TBreakPointsDlg.Destroy;
Begin
  inherited;
end;

Procedure TBreakPointsDlg.AddBreakPoint(UnitName : String; Line : Integer);
var
  LI : TListItem;
Begin
  LI := ListView1.Items.Add;
  LI.Caption := UnitName;
  LI.SubItems.Add(Inttostr(line));
  LI.SubItems.Add('');
  LI.SubItems.Add('Break');
  LI.SubItems.Add('0');
  LI.SubItems.Add('');
end;


Procedure TBreakPointsDlg.DeleteBreakPoint(UnitName : String; Line : Integer);
var
  LI : TListItem;
  I : Integer;
Begin
  for I := 0 to ListView1.Items.Count-1 do
     Begin
       LI := ListView1.Items[i];
       if LI.Caption <> UnitName then Continue;
       if LI.SubItems.Strings[0] = inttostr(line) then
          begin
            ListView1.Items.Delete(i);
            Break;
          end;
     
     end;
end;

Procedure TBreakPointsdlg.ListView1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Writeln('ListView1 KeyDown!');
end;

end.

