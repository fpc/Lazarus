{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    form for resizing report elements.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frmresizeelements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, Spin, fpreport, fpreportdesignobjectlist, reportdesignbaseforms;

type

  { TResizeElementsForm }
  TForm = TBaseReportResizeForm;

  TResizeElementsForm = class(TForm)
    BPResize: TButtonPanel;
    FSEHorizontal: TFloatSpinEdit;
    FSEVertical: TFloatSpinEdit;
    GBHorizontal: TGroupBox;
    GBVertical: TGroupBox;
    RBHNone: TRadioButton;
    RBHFixed: TRadioButton;
    RBVNone: TRadioButton;
    RBVFixed: TRadioButton;
    RBVLargest: TRadioButton;
    RBHSmallest: TRadioButton;
    RBHLargest: TRadioButton;
    RBVSmallest: TRadioButton;
    procedure RBHFixedClick(Sender: TObject);
    procedure RBVNoneClick(Sender: TObject);
  protected
    function GetH: TSizeAdjust; override;
    function GetHS: TFPReportUnits; override;
    function GetV: TSizeAdjust; override;
    function GetVS: TFPReportUnits; override;
    procedure SetH(AValue: TSizeAdjust);override;
    procedure SetHS(AValue: TFPReportUnits);override;
    procedure SetV(AValue: TSizeAdjust);override;
    procedure SetVS(AValue: TFPReportUnits);override;
  public
    Property Horizontal : TSizeAdjust Read GetH Write SetH;
    Property Vertical : TSizeAdjust Read GetV Write SetV;
    Property HorizontalSize : TFPReportUnits Read GetHS Write SetHS;
    Property VerticalSize : TFPReportUnits Read GetVS Write SetVS;
  end;


implementation

{$R *.lfm}

{ TResizeElementsForm }

procedure TResizeElementsForm.RBHFixedClick(Sender: TObject);
begin
  FSEHorizontal.Enabled:=RBHFixed.Checked;
end;

procedure TResizeElementsForm.RBVNoneClick(Sender: TObject);
begin
  FSEVertical.Enabled:=RBVFixed.Checked;
end;

function TResizeElementsForm.GetH: TSizeAdjust;
begin
  if RBHSmallest.Checked then
    Result:=saSmallest
  else if RBHLargest.Checked then
    Result:=saLargest
  else if RBHFixed.Checked then
    Result:=saValue
  else
    Result:=saNone;
end;

function TResizeElementsForm.GetHS: TFPReportUnits;
begin
  Result:=FSEHorizontal.Value;
end;

function TResizeElementsForm.GetV: TSizeAdjust;
begin
  if RBVSmallest.Checked then
    Result:=saSmallest
  else if RBVLargest.Checked then
    Result:=saLargest
  else if RBVFixed.Checked then
    Result:=saValue
  else
    Result:=saNone;
end;

function TResizeElementsForm.GetVS: TFPReportUnits;
begin
  Result:=FSEVertical.Value;
end;

procedure TResizeElementsForm.SetH(AValue: TSizeAdjust);
begin
  Case AValue of
    saLargest  : RBHLargest.Checked:=True;
    saSmallest : RBHSmallest.Checked:=True;
    saValue    : RBHFixed.Checked:=True;
  else
    RBHNone.Checked:=True;
  end;
end;

procedure TResizeElementsForm.SetHS(AValue: TFPReportUnits);
begin
  FSEHorizontal.Value:=Avalue;
end;

procedure TResizeElementsForm.SetV(AValue: TSizeAdjust);
begin
  Case AValue of
    saLargest  : RBVLargest.Checked:=True;
    saSmallest : RBVSmallest.Checked:=True;
    saValue    : RBVFixed.Checked:=True;
  else
    RBVNone.Checked:=True;
  end;
end;

procedure TResizeElementsForm.SetVS(AValue: TFPReportUnits);
begin
  FSEVertical.Value:=AValue
end;

initialization
  ReportResizeFormClass:=TResizeElementsForm;
end.

