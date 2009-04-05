{
   This example shows, how the the Keywords, Objects and Constants properties of
   the class TSynAnySyn can be controlled at runtime.
}
unit Unit1;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, SynMemo,
  SynHighlighterAny, CheckLst, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
     ButtonUncheckAll: TButton;
     ButtonCheckAll: TButton;
     CheckListBoxConstants: TCheckListBox;
     CheckListBoxObjects: TCheckListBox;
     CheckListBoxKeys: TCheckListBox;
     Label1: TLabel;
     Label2: TLabel;
     Label3: TLabel;
     SynAnySyn: TSynAnySyn;
     SynMemo: TSynMemo;
     procedure ButtonCheckAllClick(Sender: TObject);
     procedure ButtonUncheckAllClick(Sender: TObject);
     procedure CheckListBoxHandler(Sender: TObject; Index: integer);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.CheckListBoxHandler(Sender: TObject; Index: integer);
var
   s: TStrings;
begin
   if Sender = CheckListBoxKeys then
      s:= SynAnySyn.KeyWords
   else if Sender = CheckListBoxObjects then
      s:= SynAnySyn.Objects
   else
      s:= SynAnySyn.Constants;
   With Sender As TCheckListBox do begin
      if Checked[Index] then
         s.Add(Items[Index])
      else begin
         try
            s.Delete(s.IndexOf(Items[Index]));
         except
            // it is just a demo ;-)
         end;
      end;
      SynMemo.Refresh;
   end;
end;

procedure TForm1.ButtonCheckAllClick(Sender: TObject);
var
   i: Integer;
begin
   for i:= 0 to 3 do begin
      CheckListBoxKeys.Checked[i]:= TRUE;
      CheckListBoxObjects.Checked[i]:= TRUE;
      CheckListBoxConstants.Checked[i]:= TRUE;
   end;
   SynAnySyn.KeyWords.Clear;
   SynAnySyn.KeyWords.Assign(CheckListBoxKeys.Items);
   SynAnySyn.Objects.Clear;
   SynAnySyn.Objects.Assign(CheckListBoxObjects.Items);
   SynAnySyn.Constants.Clear;
   SynAnySyn.Constants.Assign(CheckListBoxConstants.Items);
   SynMemo.Refresh;
end;

procedure TForm1.ButtonUncheckAllClick(Sender: TObject);
var
   i: Integer;
begin
   for i:= 0 to 3 do begin
      CheckListBoxKeys.Checked[i]:= FALSE;
      CheckListBoxObjects.Checked[i]:= FALSE;
      CheckListBoxConstants.Checked[i]:= FALSE;
   end;
   SynAnySyn.KeyWords.Clear;
   SynAnySyn.Objects.Clear;
   SynAnySyn.Constants.Clear;
   SynMemo.Refresh;
end;

initialization
  {$I unit1.lrs}

end.

