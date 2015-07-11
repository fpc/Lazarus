{ LazReport dialogs control

  Copyright (C) 2012-2013 alexs alexs75.at.hotbox.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit lrOfficeImport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_Class, Graphics, Forms;

type

  { TlrOfficeImport }

  TlrOfficeImport = class(TComponent)
  private
    { Private declarations }
  protected
    procedure DoImportClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

procedure Register;

resourcestring
  sSpreadSheetImport = 'Import from spreadsheet';

implementation
uses lrSpreadSheetImportUnit;

{$R lr_officeimport_img.res}

var
  lrOfficeImportVar : TlrOfficeImport = nil;
  lrBMPSpreadSheetImport : TBitmap = nil;

procedure Register;
begin
  RegisterComponents('LazReport',[TlrOfficeImport]);
end;

{ TlrOfficeImport }

procedure TlrOfficeImport.DoImportClick(Sender: TObject);
begin
  lrSpreadSheetImportForm:=TlrSpreadSheetImportForm.Create(Application);
  lrSpreadSheetImportForm.ShowModal;
  lrSpreadSheetImportForm.Free;
end;

constructor TlrOfficeImport.Create(AOwner: TComponent);
begin
  if Assigned(lrOfficeImportVar) then
    raise Exception.Create('Allow only one instance of the class ' + ClassName);
  inherited Create(AOwner);
  lrOfficeImportVar:=Self;

  frRegisterTool(sSpreadSheetImport, lrBMPSpreadSheetImport, @DoImportClick);
end;

destructor TlrOfficeImport.Destroy;
begin
  if lrOfficeImportVar = Self then
    lrOfficeImportVar:=nil;
  inherited Destroy;
end;


initialization
  lrBMPSpreadSheetImport := TBitmap.Create;
  lrBMPSpreadSheetImport.LoadFromResourceName(HInstance, TlrOfficeImport.ClassName);

finalization
  if Assigned(lrBMPSpreadSheetImport) then
    FreeAndNil(lrBMPSpreadSheetImport);
end.
