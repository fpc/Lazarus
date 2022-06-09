unit IdeDebuggerWatchResUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdeDebuggerWatchResult, LazDebuggerIntf;

function ExtractProcResFromMethod(AMethodRes: TWatchResultData): TWatchResultData;

implementation

function ExtractProcResFromMethod(AMethodRes: TWatchResultData
  ): TWatchResultData;
begin
  Result := nil;
  if (AMethodRes <> nil) and
     (AMethodRes.StructType = dstRecord) and
     (AMethodRes.FieldCount = 2) and
     (LowerCase(AMethodRes.Fields[0].FieldName) = 'proc') and
     (AMethodRes.Fields[0].Field <> nil) and
     (AMethodRes.Fields[0].Field.ValueKind in [rdkFunction, rdkProcedure, rdkFunctionRef, rdkProcedureRef]) and
     (LowerCase(AMethodRes.Fields[1].FieldName) = 'self') and
     (AMethodRes.Fields[1].Field <> nil) and
     (AMethodRes.Fields[1].Field.ValueKind = rdkStruct)
  then
    Result := AMethodRes.Fields[0].Field;
end;

end.

