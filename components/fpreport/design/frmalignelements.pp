{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Form to select alignment options for aligning the selection.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frmalignelements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  ExtCtrls, fpreportdesignobjectlist, reportdesignbaseforms;

type
  TForm = TBaseReportAlignForm;
  { TAlignForm }

  TAlignForm = class(TForm)
    BPAlign: TButtonPanel;
    RGHorizontal: TRadioGroup;
    RGVertical: TRadioGroup;
  protected
    function GetH: THAlignAction; override;
    function GetV: TVAlignAction; override;
    procedure SetH(AValue: THAlignAction); override;
    procedure SetV(AValue: TVAlignAction);override;
  public
    Property Horizontal : THAlignAction Read GetH Write SetH;
    Property Vertical : TVAlignAction Read GetV Write SetV;
  end;

var
  AlignForm: TAlignForm;

implementation

{$R *.lfm}

{ TAlignForm }

function TAlignForm.GetH: THAlignAction;
begin
  if (RGHorizontal.ItemIndex=-1) then
    Result:=haNone
  else
    Result:=THAlignAction(RGHorizontal.ItemIndex);
end;

function TAlignForm.GetV: TVAlignAction;
begin
  if (RGVertical.ItemIndex=-1) then
    Result:=vaNone
  else
    Result:=TVAlignAction(RGVertical.ItemIndex);
end;

procedure TAlignForm.SetH(AValue: THAlignAction);
begin
  RGHorizontal.ItemIndex:=Ord(AValue);
end;

procedure TAlignForm.SetV(AValue: TVAlignAction);
begin
  RGVertical.ItemIndex:=Ord(AValue);
end;

initialization
  ReportAlignFormClass:=TAlignForm;
end.

