{
 /***************************************************************************
                               CustomEditor.pp
                             -------------------




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
unit CustomEditor;

{$mode objfpc}

interface

uses
  classes,AbstractEditor, FileSystem;

type

  TCustomEditor = class(TAbstractEditor)
   private
     FSource : TStrings; //Holds the source retrieved from TFileSystem
     Function GetSource: TStrings; //Returns the source from TFileSystem
     Procedure SetSource(value : TStrings); //Set's the source in the TFileSystem
   public
     constructor Create;
     destructor destroy;
     Function Filename : String; override;
     property Source: TStrings read GetSource write SetSource; //Holds the source retrieved from TFileSystem
  end;


implementation

constructor TCustomEditor.Create;
Begin
//Create the TFileSystem
end;

Function TCustomEditor.GetSource : TStrings;
Begin
Result := nil;
End;


end.
