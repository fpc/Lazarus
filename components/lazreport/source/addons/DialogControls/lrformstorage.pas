{ LazReport dialogs control

  Copyright (C) 2012-2013 alexs alexs75.at.yandex.ru

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

unit lrFormStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LR_Class, Graphics, Forms;

type

  { TLRFormStorage }

  TLRFormStorage = class(TfrNonVisualControl)
  private
    FActive: Boolean;
    FConfigFileName: string;
    FStoredProperties: TStringList;
    FVersion: integer;
    function GetStoredProperties: TStringList;
    procedure SetStoredProperties(AValue: TStringList);

    procedure ShowEditorForm(Sender: TObject);
    procedure StoreProps;
    procedure LoadProps;

    procedure OnFormClose(Sender: TObject; var CloseAction: TCloseAction);
  protected
    procedure AfterLoad;override;
    function ExecMetod(const AName: String; p1, p2, p3: Variant; var Val: Variant):boolean;override;
    function CfgFileName(ACreatePath:boolean):string;
    procedure AttachToParent; override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy; override;

    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property StoredProperties:TStringList read GetStoredProperties write SetStoredProperties;
    property Version:integer read FVersion write FVersion;
    property Active:Boolean read FActive write FActive;
    property ConfigFileName:string read FConfigFileName write FConfigFileName;
  end;

implementation

uses LResources, lrFormStorageEditor, Controls, typinfo, FileUtil, LR_Const,
  LazUtilsStrConsts;

var
  lrBMP_LRFormStorage:TBitmap = nil;
  lrFormStorageEditorForm: TlrFormStorageEditorForm;

procedure InitLRComp;
begin
  lrFormStorageEditorForm:=TlrFormStorageEditorForm.Create(nil);

  if not assigned(lrBMP_LRFormStorage) then
  begin
    lrBMP_LRFormStorage := TbitMap.Create;
    lrBMP_LRFormStorage.LoadFromResourceName(HInstance, 'TLRFormStorage');
    frRegisterObject(TLRFormStorage, lrBMP_LRFormStorage, 'TLRFormStorage', lrFormStorageEditorForm, otlUIControl, nil);
  end;

end;

{ TLRFormStorage }

procedure TLRFormStorage.SetStoredProperties(AValue: TStringList);
begin
  FStoredProperties.Assign(AValue);
end;

procedure TLRFormStorage.ShowEditorForm(Sender: TObject);
begin
  lrFormStorageEditorForm.ShowEditor(Self);
end;

procedure TLRFormStorage.StoreProps;
var
  PL:TStringList;
  i:integer;

procedure DoStore(PropLine:string);
var
  PropInfo:PPropInfo;
  frObj:TfrObject;
  FPropName, St:string;
begin
  PropInfo:=FindObjectProps(PropLine, frObj, FPropName);

  if Assigned(frObj) and Assigned(PropInfo) then
  begin
    case PropInfo^.PropType^.Kind of
      tkChar,tkAString,tkWString, tkSString,tkLString : St:=GetStrProp(frObj, PropInfo);
      tkBool,tkInt64,tkQWord, tkInteger : St:=IntToStr(GetOrdProp(frObj, PropInfo));
      tkSet: St:=GetSetProp(frObj, PropInfo, false);
      tkFloat : St:=FloatToStr(GetFloatProp(frObj, PropInfo));
      tkEnumeration : St:=GetEnumProp(frObj, PropInfo);
    else
      exit;
    end;
    PL.Values[PropLine]:=St;
  end;
end;

begin
  PL:=TStringList.Create;
  try
    for i:=0 to FStoredProperties.Count-1 do
      DoStore(FStoredProperties[i]);
    PL.SaveToFile(UTF8ToSys(CfgFileName(true)));
  finally
    PL.Free;
  end;
end;

procedure TLRFormStorage.LoadProps;
var
  PL:TStringList;
  i:integer;
  S:string;

procedure DoLoad(PropLine:string);
var
  PropInfo:PPropInfo;
  frObj:TfrObject;
  FPropName, St:string;
begin
  if PL.Values[PropLine]<>'' then
  begin
    PropInfo:=FindObjectProps(PropLine, frObj, FPropName);

    if Assigned(PropInfo) then
    begin
      case PropInfo^.PropType^.Kind of
        tkChar,tkAString,tkWString, tkSString,tkLString : SetStrProp(frObj, PropInfo, PL.Values[PropLine]);
        tkBool,tkInt64,tkQWord, tkInteger
                                    : begin
                                        if AnsiCompareText(PropInfo^.PropType^.Name,'TGraphicsColor')=0 then
                                          SetOrdProp(frObj, PropInfo, StringToColor(PL.Values[PropLine]))
                                        else
                                          SetOrdProp(frObj, PropInfo, StrToIntDef(PL.Values[PropLine], 0))
                                      end;
        tkSet                       : SetSetProp(frObj, PropInfo, PL.Values[PropLine]);
        tkFloat                     : SetFloatProp(frObj ,PropInfo, StrToFloatDef(PL.Values[PropLine], 0));
        tkEnumeration               : SetEnumProp(frObj, PropInfo, PL.Values[PropLine]);
      end;
    end
  end;
end;

begin
  S:=CfgFileName(false);
  if FileExistsUTF8(S) then
  begin
    PL:=TStringList.Create;
    try
      PL.LoadFromFile(UTF8ToSys(S));
      for i:=0 to FStoredProperties.Count-1 do
        DoLoad(FStoredProperties[i]);
    finally
      PL.Free;
    end;
  end;
end;


procedure TLRFormStorage.OnFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Assigned(OwnerForm) then
  begin
    if (TForm(Sender).ModalResult = mrOk) and FActive then
      StoreProps;
  end;
end;

function TLRFormStorage.GetStoredProperties: TStringList;
begin
  Result:=FStoredProperties;
end;


procedure TLRFormStorage.AfterLoad;
begin
  inherited AfterLoad;
  //LoadProps;
end;

function TLRFormStorage.ExecMetod(const AName: String; p1, p2, p3: Variant;
  var Val: Variant): boolean;
begin
  Result:=inherited ExecMetod(AName, p1, p2, p3, Val);
  if AName = 'STOREPROPS' then
    StoreProps
  else
  if AName = 'LOADPROPS' then
    LoadProps
  else
  if AName = 'SHOWEDITORFORM' then
    ShowEditorForm(nil);
end;

function TLRFormStorage.CfgFileName(ACreatePath: boolean): string;
var
  Dir:string;
begin
  if FConfigFileName<>'' then
  begin
    Result:='';
    if ExtractFileDir(FConfigFileName) = '' then
      Result:=AppendPathDelim(GetAppConfigDirUTF8(false, true))+'LazReport'+PathDelim;
    Result:=Result + FConfigFileName;
  end
  else
  begin
    if CurReport.FileName <> SUntitled then
      Result:=AppendPathDelim(GetAppConfigDirUTF8(false, true))+'LazReport'+PathDelim+ ExtractFileNameOnly(CurReport.FileName)+'.cfglr';
  end;

  if ACreatePath then
  begin
    Dir := ExtractFilePath(Result);
    if Dir = '' then exit;
    if not ForceDirectoriesUTF8(Dir) then
      raise EInOutError.Create(SysUtils.Format(lrsUnableToCreateConfigDirectoryS,[Dir]));
  end;
end;

procedure TLRFormStorage.AttachToParent;
begin
  inherited AttachToParent;
  if Assigned(OwnerForm) and (OwnerForm is TForm) then
    TForm(OwnerForm).OnClose:=@OnFormClose
end;

constructor TLRFormStorage.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);

  BaseName := 'lrFormStorage';
  FDesignOptions:=FDesignOptions + [doUndoDisable];
  FStoredProperties:=TStringList.Create;
end;

destructor TLRFormStorage.Destroy;
begin
  FreeAndNil(FStoredProperties);
  inherited Destroy;
end;

procedure TLRFormStorage.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  FStoredProperties.Text:=XML.GetValue(Path + 'StoredProperties/Value'{%H-}, '');
  FVersion:=XML.GetValue(Path + 'Version/Value'{%H-}, 0);
  FConfigFileName:=XML.GetValue(Path + 'ConfigFileName/Value'{%H-}, '');
  FActive:=XML.GetValue(Path + 'Active/Value'{%H-}, true);
end;

procedure TLRFormStorage.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path + 'StoredProperties/Value'{%H-}, FStoredProperties.Text);
  XML.SetValue(Path + 'Version/Value'{%H-}, FVersion);
  XML.SetValue(Path + 'ConfigFileName/Value'{%H-}, FConfigFileName);
  XML.SetValue(Path + 'Active/Value'{%H-}, FActive);
end;

initialization
  InitLRComp;
finalization
  if Assigned(lrBMP_LRFormStorage) then
    FreeAndNil(lrBMP_LRFormStorage);
  if Assigned(lrFormStorageEditorForm) then
    FreeAndNil(lrFormStorageEditorForm);
end.

