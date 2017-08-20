{
    This file is part of the Free Component Library.
    Copyright (c) 2016 Michael Van Canneyt, member of the Free Pascal development team

    Combo dialog component - awaiting inclusion in Lazarus LCL.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit dlginputcombo;


{$mode objfpc}
{$h+}

interface

uses Classes;

{ Input ComboBox dialog }

Function InputCombo(const ACaption, APrompt: string; const AList: TStrings): Integer;
Function InputCombo(const ACaption, APrompt: string; const AList : Array of String): Integer;
Function InputComboEx(const ACaption, APrompt: string; const AList: TStrings; AllowCustomText: Boolean = False): String;
Function InputComboEx(const ACaption, APrompt: string; const AList : Array of String; AllowCustomText: Boolean = False): String;

Implementation

uses types, lclintf, graphics, controls, stdctrls, buttonpanel, sysutils, math, forms;


function DoInputCombo(const ACaption, APrompt: string; const AList: TStrings; AllowInput : Boolean; Out ASelected : Integer) : String;

Const
  CBStyles : Array[Boolean] of TComboBoxStyle = (csDropDownList,csDropDown);

var
  W,I,Sep,Margin : Integer;
  Frm: TForm;
  CBSelect : TComboBox;
  LPrompt: TLabel;
  BP : TButtonPanel;

begin
  Margin:=24;
  Sep:=8;
  Result:='';
  ASelected:=-1;
  Frm:=TForm.Create(Application);
  try
    // Determine needed width
    W:=frm.Canvas.TextWidth(APrompt);
    W:=Max(W,frm.Canvas.TextWidth(ACaption));
    for I:=0 to AList.Count-1 do
      W:=Max(W,frm.Canvas.TextWidth(AList[i]+'WWW')); // WWW is just some extra.
    frm.BorderStyle:=bsDialog;
    frm.Caption:=ACaption;
    frm.ClientWidth:=W+2*Margin;
    frm.Position:=poScreenCenter;
    // Prompt
    LPrompt:=TLabel.Create(frm);
    LPrompt.Parent:=frm;
    LPrompt.Caption:=APrompt;
    LPrompt.SetBounds(Margin,Margin,Frm.ClientWidth-2*Margin,frm.Canvas.TextHeight(APrompt));
    LPrompt.WordWrap:=True;
    LPrompt.AutoSize:=False;
    // Selection combobox
    CBSelect:=TComboBox.Create(Frm);
    CBSelect.Parent:=Frm;
    CBSelect.Style:=CBStyles[AllowInput];
    CBSelect.Items.Assign(AList);
    CBSelect.ItemIndex:=-1;
    CBSelect.Left:=Margin;
    CBSelect.Top:=LPrompt.Top + LPrompt.Height + Sep;
    CBSelect.Width:=Frm.ClientWidth-2*Margin;
    // Buttons
    BP:=TButtonPanel.Create(Frm);
    BP.Parent:=Frm;
    BP.ShowButtons:=[pbOK,pbCancel];
    Frm.ClientHeight:=LPrompt.Height+CBSelect.Height+BP.Height+2*Sep+Margin;
    if (Frm.ShowModal=mrOk) then
      begin
      Result:=CBSelect.Text;
      ASelected:=CBSelect.ItemIndex;
      end;
  finally
    FreeAndNil(Frm);
  end;
end;

Function InputCombo(const ACaption, APrompt: string; const AList : Array of String): Integer;

Var
  L : TStrings;
  S : String;
begin
  L:=TstringList.Create;
  try
    For S in AList do
      L.Add(S);
    Result:=InputCombo(ACaption,APrompt,L);
  finally
    L.Free;
  end;
end;

function InputComboEx(const ACaption, APrompt: string; const AList: TStrings; AllowCustomText: Boolean = False): String;

Var
  D : Integer;

begin
  Result:=DoInputCombo(ACaption,APrompt,AList,AllowCustomText,D);
end;

function InputComboEx(const ACaption, APrompt: string;
  const AList: array of String; AllowCustomText: Boolean = False): String;
Var
  L : TStrings;
  S : String;
begin
  L:=TstringList.Create;
  try
    For S in AList do
      L.Add(S);
    Result:=InputComboEx(ACaption,APrompt,L,AllowCustomText);
  finally
    L.Free;
  end;
end;

Function InputCombo(const ACaption, APrompt: string; const AList: TStrings): Integer;

begin
  DoInputCombo(ACaption,APrompt,AList,False,Result);
end;

end.
