{
  Author: Jens Arm

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit UnitInfoDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, Buttons, ExtCtrls;

type
  TUnitInfoDlg = class(TFORM)
    OkButton:TButton;
    uname: TLabel;
    utype: TLabel;
    uinproject: TLabel;
    usize: TLabel;
    outname: TLabel;
    outtype: TLabel;
    outinproject: TLabel;
    ulines: TLabel;
    upath: TLabel;
    outsize: TLabel;
    outlines: TLabel;
    outpath: TLabel;
    procedure UnitInfoDlgResize(Sender: TObject);
    procedure OkButtonClick(Sender:TObject);
  private
    procedure setShortName(const str:string);
    procedure setType(const str:string);
    procedure setInProject(const str:string);
    procedure setSize(const str:string);
    procedure setLines(const str:string);
    procedure setPath(const str:string);
  public
    constructor Create(AOwner:TComponent); override;
  end;

function ShowUnitInfoDlg(const AnUnitName, AType: string;
  IsPartOfProject: boolean; SizeInBytes, LineCount: integer;
  const FilePath: string): TModalResult;


implementation

uses LResources;

function ShowUnitInfoDlg(const AnUnitName, AType: string;
  IsPartOfProject: boolean; SizeInBytes, LineCount: integer;
  const FilePath: string): TModalResult;
var Dlg: TUnitInfoDlg;
begin
  Dlg:=TUnitInfoDlg.Create(Application);
  try
    with Dlg do begin
      Caption:='Information about '+AnUnitName;
      setShortName(AnUnitName);
      setType(AType);
      if IsPartOfProject then
        setInProject('yes')
      else
        setInProject('no');
      setSize(IntToStr(SizeInBytes)+' bytes');
      setLines(IntToStr(LineCount));
      setPath(FilePath);
    end;
    Result:=Dlg.ShowModal;
  finally
    Dlg.Free;
  end;
end;

{ TUnitInfoDlg }

constructor TUnitInfoDlg.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if LazarusResources.Find(ClassName)=nil then begin

    Caption:='Unit Info for unit ???';
    Width:=400;
    Height:=164;
    position:=poScreenCenter;
    OnResize:=@UnitInfoDlgResize;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      Top:=132;
      Width:=75;
      Height:=25;
      Left:=(Self.ClientWidth-Width) div 2;
      Caption:='Ok';
      OnClick:=@OkButtonClick;
      Show;
    end;

    UName:=TLabel.create(self);
    with uname do begin
        Name:='Name';
        Parent:=self;
        Left:=4;
        top:=4;
        caption:='Name:';
        Show;
    end;

    utype:=TLabel.create(self);
    with utype do begin
        Name:='Type';
        Parent:=self;
        Left:=4;
        top:=24;
        caption:='Type:';
        Show;
    end;

    uinproject:=TLabel.create(self);
    with uinproject do begin
        Name:='InProject';
        Parent:=self;
        Left:=4;
        top:=44;
        caption:='in Project:';
        Show;
    end;

    usize:=TLabel.create(self);
    with usize do begin
        Name:='Size';
        Parent:=self;
        Left:=4;
        top:=64;
        caption:='Size:';
        Show;
    end;

    ulines:=TLabel.create(self);
    with ulines do begin
        Name:='Lines';
        Parent:=self;
        Left:=4;
        top:=84;
        caption:='Lines:';
        Show;
    end;

    upath:=TLabel.create(self);
    with upath do begin
        Name:='Path';
        Parent:=self;
        Left:=4;
        top:=104;
        caption:='Path:';
        Show;
    end;

    outname:=TLabel.create(self);
    with outname do begin
        Name:='OutName';
        Parent:=self;
        Left:=68;
        top:=4;
        Width:=Self.ClientWidth-Left-5;
        caption:='temp';
        Show;
    end;

    outtype:=TLabel.create(self);
    with outtype do begin
        Name:='OutType';
        Parent:=self;
        Left:=68;
        top:=24;
        Width:=Self.ClientWidth-Left-5;
        caption:='temp';
        Show;
    end;

    outinproject:=TLabel.create(self);
    with outinproject do begin
        Name:='OutInProject';
        Parent:=self;
        Left:=68;
        top:=44;
        Width:=Self.ClientWidth-Left-5;
        caption:='temp';
        Show;
    end;

    outsize:=TLabel.create(self);
    with outsize do begin
        Name:='OutSize';
        Parent:=self;
        Left:=68;
        top:=64;
        Width:=Self.ClientWidth-Left-5;
        caption:='temp';
        Show;
    end;

    outlines:=TLabel.create(self);
    with outlines do begin
        Name:='OutLines';
        Parent:=self;
        Left:=68;
        top:=84;
        Width:=Self.ClientWidth-Left-5;
        caption:='temp';
        Show;
    end;

    outpath:=TLabel.create(self);
    with outpath do begin
        Name:='OutPath';
        Parent:=self;
        Left:=68;
        top:=104;
        width:=181;
        caption:='temp';
        Width:=Self.ClientWidth-Left-5;
        autosize:=true;
        Show;
    end;
  end;
  UnitInfoDlgResize(nil);
end;

procedure TUnitInfoDlg.setShortName(const str:string);
begin
    outname.caption:=str;
end;

procedure TUnitInfoDlg.setType(const str:string);
begin
    outtype.caption:=str;
end;

procedure TUnitInfoDlg.setInProject(const str:string);
begin
    outinproject.caption:=str;
end;

procedure TUnitInfoDlg.setSize(const str:string);
begin
    outsize.caption:=str;
end;

procedure TUnitInfoDlg.setLines(const str:string);
begin
    outlines.caption:=str;
end;

procedure TUnitInfoDlg.setPath(const str:string);
begin
    outpath.caption:=str;
end;

procedure TUnitInfoDlg.UnitInfoDlgResize(Sender: TObject);
begin
  with OkButton do begin
    Top:=132;
    Width:=75;
    Height:=25;
    Left:=(Self.ClientWidth-Width) div 2;
  end;

  with uname do begin
    Left:=4;
    Top:=4;
  end;

  with utype do begin
    Left:=4;
    Top:=24;
  end;

  with uinproject do begin
    Left:=4;
    Top:=44;
  end;

  with usize do begin
    Left:=4;
    top:=64;
  end;

  with ulines do begin
    Left:=4;
    top:=84;
  end;

  with upath do begin
    Left:=4;
    top:=104;
  end;

  with outname do begin
    Left:=68;
    top:=4;
    Width:=Self.ClientWidth-Left-5;
  end;

  with outtype do begin
    Left:=68;
    top:=24;
    Width:=Self.ClientWidth-Left-5;
  end;

  with outinproject do begin
    Left:=68;
    top:=44;
    Width:=Self.ClientWidth-Left-5;
  end;

  with outsize do begin
    Left:=68;
    top:=64;
    Width:=Self.ClientWidth-Left-5;
  end;

  with outlines do begin
    Left:=68;
    top:=84;
    Width:=Self.ClientWidth-Left-5;
  end;

  with outpath do begin
    Left:=68;
    top:=104;
    Width:=Self.ClientWidth-Left-5;
  end;
end;

procedure TUnitInfoDlg.OkButtonClick(Sender:TObject);
begin
  ModalResult:=mrOk;
end;

end.

