{
 /***************************************************************************
                               registry.pp
                             -------------------
                             Component Library Registry Controls
                   Initial Revision  : Fri Jul 23 20:00:00 PDT 1999


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}

{
@author(TRegistry - Curtis White <cwhite@aracnet.com>)                       
@author(TRegIniFile - Curtis White <cwhite@aracnet.com>)                       
@created(17-Oct-1999)
@lastmod(17-Oct-1999)

Classes to provide access to a registry. This will provide access to the  
registry native to the OS. If the OS does not contain a registry, then
this will provide access to a file registry or conf file.
} 

unit Registry;

//{$mode delphi}
{$mode objfpc}{$H+}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  Classes, Controls, SysUtils, vclGlobals, lMessages, LCLLinux;

type
  ERegistryException = class(Exception);

  TRegKeyInfo = record
    NumSubKeys: Integer;
    MaxSubKeyLen: Integer;
    NumValues: Integer;
    MaxValueLen: Integer;
    MaxDataLen: Integer;
    //FileTime: TFileTime;
  end;

  TRegDataType = (rdUnknown, rdString, rdExpandString, rdInteger, rdBinary);

  TRegDataInfo = record
    RegData: TRegDataType;
    DataSize: Integer;
  end;

  { TRegistry }
  {
    @abstract(Class to provide access to a registry.)
    Introduced by Curtis White
    Currently maintained by Curtis White
  }
  TRegistry = class(TObject)
  private
    fCurrentKey: HKEY;
    fRootKey: HKEY;
    fLazyWrite: Boolean;
    fCurrentPath: string;

    procedure SetRootKey(Value: HKEY);
  protected
    function GetBaseKey(Relative: Boolean): HKey;
    function GetData(const Name: string; Buffer: Pointer;
                  BufSize: Integer; var RegData: TRegDataType): Integer;
    function GetKey(const Key: string): HKEY;

    procedure ChangeKey(Value: HKey; const Path: string);
    procedure PutData(const Name: string; Buffer: Pointer; 
                  BufSize: Integer; RegData: TRegDataType);
    procedure SetCurrentKey(Value: HKEY);
  public
    constructor Create;
    destructor Destroy; override;

    function CreateKey(const Key: string): Boolean;
    function DeleteKey(const Key: string): Boolean;
    function DeleteValue(const Name: string): Boolean;
    function GetDataInfo(const ValueName: string; var Value: TRegDataInfo): Boolean;
    function GetDataSize(const ValueName: string): Integer;
    function GetDataType(const ValueName: string): TRegDataType;
    function GetKeyInfo(var Value: TRegKeyInfo): Boolean;
    function HasSubKeys: Boolean;
    function KeyExists(const Key: string): Boolean;
    function LoadKey(const Key, FileName: string): Boolean;
    function OpenKey(const Key: string; CanCreate: Boolean): Boolean;
    //function ReadCurrency(const Name: string): Currency;
    function ReadBinaryData(const Name: string; var Buffer; BufSize: Integer): Integer;
    function ReadBool(const Name: string): Boolean;
    function ReadDate(const Name: string): TDateTime;
    function ReadDateTime(const Name: string): TDateTime;
    function ReadFloat(const Name: string): Double;
    function ReadInteger(const Name: string): Integer;
    function ReadString(const Name: string): string;
    function ReadTime(const Name: string): TDateTime;
    function RegistryConnect(const UNCName: string): Boolean;
    function ReplaceKey(const Key, FileName, BackUpFileName: string): Boolean;
    function RestoreKey(const Key, FileName: string): Boolean;
    function SaveKey(const Key, FileName: string): Boolean;
    function UnLoadKey(const Key: string): Boolean;
    function ValueExists(const Name: string): Boolean;

    procedure CloseKey;
    procedure GetKeyNames(Strings: TStrings);
    procedure GetValueNames(Strings: TStrings);
    procedure MoveKey(const OldName, NewName: string; Delete: Boolean);
    procedure RenameValue(const OldName, NewName: string);
    //procedure WriteCurrency(const Name: string; Value: Currency);
    procedure WriteBinaryData(const Name: string; var Buffer; BufSize: Integer);
    procedure WriteBool(const Name: string; Value: Boolean);
    procedure WriteDate(const Name: string; Value: TDateTime);
    procedure WriteDateTime(const Name: string; Value: TDateTime);
    procedure WriteFloat(const Name: string; Value: Double);
    procedure WriteInteger(const Name: string; Value: Integer);
    procedure WriteString(const Name, Value: string);
    procedure WriteExpandString(const Name, Value: string);
    procedure WriteTime(const Name: string; Value: TDateTime);

    property CurrentKey: HKEY read fCurrentKey;
    property CurrentPath: string read fCurrentPath;
    property LazyWrite: Boolean read fLazyWrite write fLazyWrite;
    property RootKey: HKEY read fRootKey write SetRootKey;
  end;

  { TRegIniFile }
  {
    @abstract(Class to provide access to a registry in an Ini file manner.)
    Introduced by Curtis White
    Currently maintained by Curtis White
  }
  TRegIniFile = class(TRegistry)
  private
    fFileName: String;
  public
    constructor Create(const FN: string);

    function ReadString(const Section, Ident, Default: string): string;
    function ReadInteger(const Section, Ident: string;
                Default: Longint): Longint;
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;

    procedure WriteString(const Section, Ident, Value: String);
    procedure WriteInteger(const Section, Ident: string; Value: Longint);
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    procedure ReadSection(const Section: string; Strings: TStrings);
    procedure ReadSections(Strings: TStrings);
    procedure ReadSectionValues(const Section: string; Strings: TStrings);
    procedure EraseSection(const Section: string);
    procedure DeleteKey(const Section, Ident: String);

    property FileName: String read fFileName;
  end;


implementation


{$I registry.inc}
{$I reginifile.inc}


initialization

finalization

end.

{
  $Log$
  Revision 1.3  2001/06/15 10:31:06  lazarus
  MG: set longstrings as default

  Revision 1.2  2001/02/04 18:24:41  lazarus
  Code cleanup
  Shane

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.4  2000/05/09 00:01:58  lazarus
  Updated my email address in the documentation to the current one. Also
  removed email references in comments that were not @author comments to
  fix problems with the documentation produced by pasdoc.           CAW

  Revision 1.3  1999/10/27 17:27:07  lazarus
  Added alot of changes and TODO: statements
  shane

  Revision 1.2  1999/10/22 21:01:51  lazarus

        Removed calls to InterfaceObjects except for controls.pp. Commented
        out any gtk depend lines of code.     MAH

  Revision 1.1  1999/10/18 07:29:49  lazarus
  Created this unit for compatiblity with Delphi registry unit.
  None of the functions are currently implemented. They need to
  be created in a platform independent manner.                   CAW

}

