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
    uIncludedBy: TLabel;
    outIncludedBy: TLabel;
    clearIncludedBy: TBitBtn;
    PathsGroupBox: TGroupBox;
    UnitPathLabel: TLabel;
    UnitPathEdit: TEdit;
    IncludePathLabel: TLabel;
    IncludePathEdit: TEdit;
    SrcPathLabel: TLabel;
    SrcPathEdit: TEdit;
    procedure PathsGroupBoxResize(Sender: TObject);
    procedure UnitInfoDlgResize(Sender: TObject);
    procedure OkButtonClick(Sender:TObject);
    procedure clearIncludedByClick(Sender: TObject);
  private
    function getIncludedBy: string;
    procedure setShortName(const str:string);
    procedure setType(const str:string);
    procedure setInProject(const str:string);
    procedure setSize(const str:string);
    procedure setLines(const str:string);
    procedure setPath(const str:string);
    procedure setIncludedBy(const IncludedBy: string);
    procedure setUnitPath(const UnitPath: string);
    procedure setIncludePath(const IncPath: string);
    procedure setSrcPath(const SrcPath: string);
  public
    constructor Create(AOwner:TComponent); override;
  end;

function ShowUnitInfoDlg(const AnUnitName, AType: string;
  IsPartOfProject: boolean; SizeInBytes, LineCount: integer;
  const FilePath: string;
  const IncludedBy: string; var ClearIncludedBy: boolean;
  const UnitPath, IncludePath, SrcPath: string): TModalResult;


implementation

uses LResources;

function ShowUnitInfoDlg(const AnUnitName, AType: string;
  IsPartOfProject: boolean; SizeInBytes, LineCount: integer;
  const FilePath: string;
  const IncludedBy: string; var ClearIncludedBy: boolean;
  const UnitPath, IncludePath, SrcPath: string): TModalResult;
var Dlg: TUnitInfoDlg;
begin
  Dlg:=TUnitInfoDlg.Create(Application);
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
    setIncludedBy(IncludedBy);
    setUnitPath(UnitPath);
    setIncludePath(IncludePath);
    setSrcPath(SrcPath);
  end;
  Result:=Dlg.ShowModal;
  ClearIncludedBy:=(Result=mrOk)
                   and (IncludedBy<>'') and (Dlg.getIncludedBy='');
  Dlg.Free;
end;

{ TUnitInfoDlg }

constructor TUnitInfoDlg.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  if LazarusResources.Find(ClassName)=nil then begin

    Caption:='Unit Info for unit ???';
    Width:=500;
    Height:=300;
    position:=poScreenCenter;
    OnResize:=@UnitInfoDlgResize;

    UName:=TLabel.create(self);
    with uname do begin
      Name:='Name';
      Parent:=self;
      Left:=4;
      top:=4;
      caption:='Name:';
    end;

    utype:=TLabel.create(self);
    with utype do begin
      Name:='Type';
      Parent:=self;
      Left:=UName.Left;
      top:=24;
      caption:='Type:';
    end;

    uinproject:=TLabel.create(self);
    with uinproject do begin
      Name:='InProject';
      Parent:=self;
      Left:=UName.Left;
      top:=44;
      caption:='in Project:';
    end;

    usize:=TLabel.create(self);
    with usize do begin
      Name:='Size';
      Parent:=self;
      Left:=UName.Left;
      top:=64;
      caption:='Size:';
    end;

    ulines:=TLabel.create(self);
    with ulines do begin
      Name:='Lines';
      Parent:=self;
      Left:=UName.Left;
      top:=84;
      caption:='Lines:';
    end;

    upath:=TLabel.create(self);
    with upath do begin
      Name:='Path';
      Parent:=self;
      Left:=UName.Left;
      top:=104;
      caption:='Path:';
    end;

    uIncludedBy:=TLabel.create(self);
    with uIncludedBy do begin
      Name:='Included by';
      Parent:=self;
      Left:=UName.Left;
      top:=124;
      caption:='Included by:';
    end;

    outname:=TLabel.create(self);
    with outname do begin
      Name:='OutName';
      Parent:=self;
      Left:=80;
      top:=4;
      Width:=Self.ClientWidth-Left-5;
      caption:='temp';
    end;

    outtype:=TLabel.create(self);
    with outtype do begin
      Name:='OutType';
      Parent:=self;
      Left:=outname.Left;
      top:=24;
      Width:=Self.ClientWidth-Left-5;
      caption:='temp';
    end;

    outinproject:=TLabel.create(self);
    with outinproject do begin
      Name:='OutInProject';
      Parent:=self;
      Left:=outname.Left;
      top:=44;
      Width:=Self.ClientWidth-Left-5;
      caption:='temp';
    end;

    outsize:=TLabel.create(self);
    with outsize do begin
      Name:='OutSize';
      Parent:=self;
      Left:=outname.Left;
      top:=64;
      Width:=Self.ClientWidth-Left-5;
      caption:='temp';
    end;

    outlines:=TLabel.create(self);
    with outlines do begin
      Name:='OutLines';
      Parent:=self;
      Left:=outname.Left;
      top:=84;
      Width:=Self.ClientWidth-Left-5;
      caption:='temp';
    end;

    outpath:=TLabel.create(self);
    with outpath do begin
      Name:='OutPath';
      Parent:=self;
      Left:=outname.Left;
      top:=104;
      caption:='temp';
      Width:=Self.ClientWidth-Left-5;
      autosize:=true;
    end;

    outIncludedBy:=TLabel.create(self);
    with outIncludedBy do begin
      Name:='outIncludedBy';
      Parent:=self;
      Left:=outname.Left;
      top:=124;
      caption:='temp';
      Width:=Self.ClientWidth-Left-57;
      autosize:=true;
    end;
    
    clearIncludedBy:=TBitBtn.Create(Self);
    with clearIncludedBy do begin
      Name:='clearIncludedBy';
      Parent:=Self;
      Left:=Self.ClientWidth-55;
      Top:=122;
      Width:=50;
      Caption:='Clear';
      OnClick:=@clearIncludedByClick;
    end;
    
    PathsGroupBox:=TGroupBox.Create(Self);
    with PathsGroupBox do begin
      Name:='PathsGroupBox';
      Parent:=Self;
      Left:=2;
      Top:=outIncludedBy.Top+outIncludedBy.Height+5;
      Width:=Self.ClientWidth-2*Left;
      Height:=100;
      Caption:='Paths (Read Only)';
      OnResize:=@PathsGroupBoxResize;
    end;
    
    UnitPathLabel:=TLabel.Create(Self);
    with UnitPathLabel do begin
      Name:='UnitPathLabel';
      Parent:=PathsGroupBox;
      Left:=2;
      Top:=2;
      Caption:='Unit';
    end;
    
    UnitPathEdit:=TEdit.Create(Self);
    with UnitPathEdit do begin
      Name:='UnitPathEdit';
      Parent:=PathsGroupBox;
      Left:=50;
      Top:=2;
      Width:=Parent.ClientWidth-Left-2;
    end;
    
    IncludePathLabel:=TLabel.Create(Self);
    with IncludePathLabel do begin
      Name:='IncludePathLabel';
      Parent:=PathsGroupBox;
      Left:=2;
      Top:=27;
      Caption:='Include';
    end;

    IncludePathEdit:=TEdit.Create(Self);
    with IncludePathEdit do begin
      Name:='IncludePathEdit';
      Parent:=PathsGroupBox;
      Left:=UnitPathEdit.Left;
      Top:=27;
      Width:=Parent.ClientWidth-Left-2;
    end;

    SrcPathLabel:=TLabel.Create(Self);
    with SrcPathLabel do begin
      Name:='SrcPathLabel';
      Parent:=PathsGroupBox;
      Left:=2;
      Top:=52;
      Caption:='Src';
    end;

    SrcPathEdit:=TEdit.Create(Self);
    with SrcPathEdit do begin
      Name:='SrcPathEdit';
      Parent:=PathsGroupBox;
      Left:=UnitPathEdit.Left;
      Top:=52;
      Width:=Parent.ClientWidth-Left-2;
    end;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      Top:=Parent.ClientHeight-33;
      Width:=75;
      Height:=25;
      Left:=(Self.ClientWidth-Width) div 2;
      Caption:='Ok';
      Default:=true;
      OnClick:=@OkButtonClick;
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

procedure TUnitInfoDlg.setIncludedBy(const IncludedBy: string);
begin
  outIncludedBy.Caption:=IncludedBy;
end;

procedure TUnitInfoDlg.setUnitPath(const UnitPath: string);
begin
  UnitPathEdit.Text:=UnitPath;
end;

procedure TUnitInfoDlg.setIncludePath(const IncPath: string);
begin
  IncludePathEdit.Text:=IncPath;
end;

procedure TUnitInfoDlg.setSRcPath(const SrcPath: string);
begin
  SrcPathEdit.Text:=SrcPath;
end;

procedure TUnitInfoDlg.UnitInfoDlgResize(Sender: TObject);
begin
  with uname do begin
    Left:=4;
    Top:=4;
  end;

  with utype do begin
    Left:=uname.Left;
    Top:=24;
  end;

  with uinproject do begin
    Left:=uname.Left;
    Top:=44;
  end;

  with usize do begin
    Left:=uname.Left;
    top:=64;
  end;

  with ulines do begin
    Left:=uname.Left;
    top:=84;
  end;

  with upath do begin
    Left:=uname.Left;
    top:=104;
  end;

  with uIncludedBy do begin
    Left:=uname.Left;
    top:=124;
  end;

  with outname do begin
    Left:=80;
    top:=4;
    Width:=Self.ClientWidth-Left-5;
  end;

  with outtype do begin
    Left:=outname.Left;
    top:=24;
    Width:=Self.ClientWidth-Left-5;
  end;

  with outinproject do begin
    Left:=outname.Left;
    top:=44;
    Width:=Self.ClientWidth-Left-5;
  end;

  with outsize do begin
    Left:=outname.Left;
    top:=64;
    Width:=Self.ClientWidth-Left-5;
  end;

  with outlines do begin
    Left:=outname.Left;
    top:=84;
    Width:=Self.ClientWidth-Left-5;
  end;

  with outpath do begin
    Left:=outname.Left;
    top:=104;
    Width:=Self.ClientWidth-Left-5;
  end;

  with outIncludedBy do begin
    Left:=outname.Left;
    top:=124;
    Width:=Self.ClientWidth-Left-57;
  end;

  with clearIncludedBy do begin
    Left:=Self.ClientWidth-55;
    top:=124;
  end;

  with PathsGroupBox do begin
    Left:=2;
    Top:=outIncludedBy.Top+outIncludedBy.Height+7;
    Width:=Self.ClientWidth-2*Left;
  end;

  with OkButton do begin
    Top:=Parent.ClientHeight-33;
    Width:=75;
    Height:=25;
    Left:=(Self.ClientWidth-Width) div 2;
  end;
end;

procedure TUnitInfoDlg.PathsGroupBoxResize(Sender: TObject);
begin
  with UnitPathLabel do begin
    Left:=2;
    Top:=2;
  end;

  with UnitPathEdit do begin
    Left:=50;
    Top:=2;
    Width:=Parent.ClientWidth-Left-2;
  end;

  with IncludePathLabel do begin
    Left:=2;
    Top:=27;
  end;

  with IncludePathEdit do begin
    Left:=UnitPathEdit.Left;
    Top:=27;
    Width:=Parent.ClientWidth-Left-2;
  end;

  with SrcPathLabel do begin
    Left:=2;
    Top:=52;
  end;

  with SrcPathEdit do begin
    Left:=UnitPathEdit.Left;
    Top:=52;
    Width:=Parent.ClientWidth-Left-2;
  end;
end;

procedure TUnitInfoDlg.OkButtonClick(Sender:TObject);
begin
  ModalResult:=mrOk;
end;

procedure TUnitInfoDlg.clearIncludedByClick(Sender: TObject);
begin
  outIncludedBy.Caption:='';
end;

function TUnitInfoDlg.getIncludedBy: string;
begin
  Result:=outIncludedBy.Caption;
end;

end.

