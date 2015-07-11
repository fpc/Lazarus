{ Copyright (C) 2007 Mattias Gaertner

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
  
  Abstract:
    Program to test single conversion tools.
}

program TestH2pasTool;

{$mode objfpc}{$H+}

uses
  Interfaces, Classes, SysUtils, H2PasWizard, H2PasConvert, IDETextConverter,
  SimpleIDEIntf, FileUtil;

procedure TestTReplaceImplicitTypes(Converter: TIDETextConverter);
var
  Tool: TReplaceImplicitTypes;
begin
  Tool:=nil;
  try
    Tool:=TReplaceImplicitTypes.Create(nil);
    Tool.Execute(Converter);
  finally
    Tool.Free;
  end;
end;

procedure TestTFixArrayOfParameterType(Converter: TIDETextConverter);
var
  Tool: TFixArrayOfParameterType;
begin
  Tool:=nil;
  try
    Tool:=TFixArrayOfParameterType.Create(nil);
    Tool.Execute(Converter);
  finally
    Tool.Free;
  end;
end;

procedure TestTConvertFunctionTypesToPointers(Converter: TIDETextConverter);
var
  Tool: TConvertFunctionTypesToPointers;
begin
  Tool:=nil;
  try
    Tool:=TConvertFunctionTypesToPointers.Create(nil);
    Tool.Execute(Converter);
  finally
    Tool.Free;
  end;
end;

var
  Filename: String;
  Converter: TIDETextConverter;
  TempFilename: String;
begin
  if ParamCount<1 then
    raise Exception.Create('Missing filename');
  Filename:=ParamStr(1);
  if not FileExists(Filename) then
    raise Exception.Create('File not found: "'+Filename+'"');
  Converter:=nil;
  try
    // create a copy of the file, so that the test does no harm
    TempFilename:=TextConverterToolClasses.GetTempFilename;
    CopyFile(Filename,TempFilename,false);
    // create the converter
    Converter:=TIDETextConverter.Create(nil);
    Converter.InitWithFilename(TempFilename);

    // test
    TestTReplaceImplicitTypes(Converter);
    TestTFixArrayOfParameterType(Converter);
    TestTConvertFunctionTypesToPointers(Converter);

    // write result
    writeln(Converter.Source);
  finally
    Converter.Free;
  end;
end.

