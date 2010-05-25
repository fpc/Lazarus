{
 /***************************************************************************
                          lazxmlforms.pas
                          ------------------

 ***************************************************************************/

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

  Author: Mattias Gaertner

  Abstract:
    Functions to convert forms to/from xml.
}
unit LazXMLForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc,
  {$IFDEF NewXMLCfg}
  laz2_DOM,
  {$ELSE}
  Laz_DOM,
  {$ENDIF}
  Laz_XMLCfg, Laz_XMLStreaming;

function CreateXMLWriter(ADoc: TDOMDocument; const Path: string;
  Append: Boolean; var DestroyDriver: boolean): TWriter;
function CreateXMLReader(ADoc: TDOMDocument; const Path: string;
  var DestroyDriver: boolean): TReader;

procedure WriteComponentToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  AComponent: TComponent);
procedure ReadComponentFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  var RootComponent: TComponent;
  OnFindComponentClass: TFindComponentClassEvent; TheOwner: TComponent);

implementation

function CreateXMLWriter(ADoc: TDOMDocument; const Path: string;
  Append: Boolean; var DestroyDriver: boolean): TWriter;
var
  Driver: TAbstractObjectWriter;
begin
  Driver:=TXMLObjectWriter.Create(ADoc,Path,Append);
  DestroyDriver:=true;
  Result:=TWriter.Create(Driver);
end;

function CreateXMLReader(ADoc: TDOMDocument; const Path: string;
  var DestroyDriver: boolean): TReader;
var
  p: Pointer;
  Driver: TAbstractObjectReader;
  DummyStream: TMemoryStream;
begin
  DummyStream:=TMemoryStream.Create;
  try
    Result:=TReader.Create(DummyStream,256);
    DestroyDriver:=false;
    // hack to set a write protected variable.
    // DestroyDriver:=true; TReader will free it
    Driver:=TXMLObjectReader.Create(ADoc,Path);
    p:=@Result.Driver;
    Result.Driver.Free;
    TAbstractObjectReader(p^):=Driver;
  finally
    DummyStream:=nil;
  end;
end;

procedure WriteComponentToXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  AComponent: TComponent);
var
  Writer: TWriter;
  DestroyDriver: boolean;
begin
  Writer:=nil;
  DestroyDriver:=false;
  try
    Writer:=CreateXMLWriter(XMLConfig.Document,Path,false,DestroyDriver);
    XMLConfig.Modified:=true;
    Writer.WriteRootComponent(AComponent);
    XMLConfig.Flush;
  finally
    if DestroyDriver then
      Writer.Driver.Free;
    Writer.Free;
  end;
end;

procedure ReadComponentFromXMLConfig(XMLConfig: TXMLConfig; const Path: string;
  var RootComponent: TComponent;
  OnFindComponentClass: TFindComponentClassEvent; TheOwner: TComponent);
var
  DestroyDriver: Boolean;
  Reader: TReader;
  IsInherited: Boolean;
  AClassName: String;
  AClass: TComponentClass;
begin
  Reader:=nil;
  DestroyDriver:=false;
  try
    Reader:=CreateXMLReader(XMLConfig.Document,Path,DestroyDriver);
    Reader.OnFindComponentClass:=OnFindComponentClass;

    // get root class
    AClassName:=(Reader.Driver as TXMLObjectReader).GetRootClassName(IsInherited);
    if IsInherited then begin
      // inherited is not supported by this simple function
      DebugLn('ReadComponentFromXMLConfig WARNING: "inherited" is not supported by this simple function');
    end;
    AClass:=nil;
    OnFindComponentClass(nil,AClassName,AClass);
    if AClass=nil then
      raise EClassNotFound.CreateFmt('Class "%s" not found', [AClassName]);

    if RootComponent=nil then begin
      // create root component
      // first create the new instance and set the variable ...
      RootComponent:=AClass.NewInstance as TComponent;
      // then call the constructor
      RootComponent.Create(TheOwner);
    end else begin
      // there is a root component, check if class is compatible
      if not RootComponent.InheritsFrom(AClass) then begin
        raise EComponentError.CreateFmt('Cannot assign a %s to a %s.',
                                        [AClassName,RootComponent.ClassName]);
      end;
    end;

    Reader.ReadRootComponent(RootComponent);
  finally
    if DestroyDriver then
      Reader.Driver.Free;
    Reader.Free;
  end;
end;

end.

