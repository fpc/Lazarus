{
ctypesmapping.pas
Copyright (C) 2011  Andrew Haines andrewd207@aol.com

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
}
unit girCTypesMapping;

{$mode objfpc}{$H+}

interface

const
  CTypesMax = 34;
var

  TypesPascalCTypes: array [0..CTypesMax-1] of string =
    (
       'void',
       'pointer',
       'cint',
       'cint',
       'cuint',
       'cuint8',
       'cuint16',
       'cuint32',
       'cuint64',
       'cint8',
       'cint16',
       'cint32',
       'cint64',
       'csize_t',
       'clong',
       'culong',
       'cushort',
       'cshort',
       'char',
       'byte',
       'Boolean32',
       'PtrInt',
       'csize_t',
       'gpointer',
       'cfloat',
       'cdouble',
       'cdouble',
       'char',
       'Int64',
       'Extended',
       'guint32',
       'guint32',
       'file',
       'qword'

    );
  TypesGTypes: array [0..CTypesMax-1] of string =
    (
       'void',
       'gpointer',
       'int',
       'gint',
       'guint',
       'guint8',
       'guint16',
       'guint32',
       'guint64',
       'gint8',
       'gint16',
       'gint32',
       'gint64',
       'gsize',
       'glong',
       'gulong',
       'gushort',
       'gshort',
       'gchar',
       'guchar',
       'gboolean',
       'gssize',
       'size_t' ,
       'gconstpointer',
       'gfloat',
       'gdouble',
       'double',
       'char',
       'goffset',
       'long double',
       'gunichar',
       'gunichar2',
       'file',
       'unsigned long long'
    );

  function LookupGTypeToCType(AName: String): String;



implementation

function LookupGTypeToCType(AName: String): String;
var
  i: Integer;
begin
  //WriteLn('Looking up: ', AName);
  for i := 0 to CTypesMax-1 do
    if AName = TypesGTypes[i] then
      Exit(TypesPascalCTypes[i]);
  Result := '';
end;

end.

