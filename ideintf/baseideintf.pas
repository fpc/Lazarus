{ Copyright (C) 2004

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract: Base classes of the IDEIntf.
}
unit BaseIDEIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazConfigStorage, Dialogs;
  
type
  TGetIDEConfigStorage = function(const Filename: string; LoadFromDisk: Boolean
                                  ): TConfigStorage;
  TInitIDEFileDialog = procedure(AFileDialog: TFileDialog) of object;
  TStoreIDEFileDialog = procedure(AFileDialog: TFileDialog) of object;

var
  DefaultConfigClass: TConfigStorageClass = nil;   // will be set by the IDE
  GetIDEConfigStorage: TGetIDEConfigStorage = nil; // will be set by the IDE
  InitIDEFileDialog: TInitIDEFileDialog = nil;     // will be set by the IDE
  StoreIDEFileDialog: TStoreIDEFileDialog = nil  ; // will be set by the IDE

implementation

end.

