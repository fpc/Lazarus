
{%region FooFunc}
  {$IFDEF FooFunc_Param}
  //procedure FooFunc(
    ArgProcedure: TProcedure;             var VArgProcedure: TProcedure;
    ArgFunction: TFunction;               var VArgFunction: TFunction;
    ArgObjProcedure: TObjProcedure;       var VArgObjProcedure: TObjProcedure;
    ArgObjFunction: TObjFunction;         var VArgObjFunction: TObjFunction;
  {$ENDIF}

  {$IFDEF FooFunc_Local}
  //var
    VarProcedureA: procedure(a1: Integer; var a2: String);
    VarFunctionA:  function(a1: Integer; var a2: String): Integer;
    VarObjProcedureA: procedure(a1: Integer; var a2: String) of object;
    VarObjFunctionA:  function(a1: Integer; var a2: String): Integer of object;
  {$ENDIF}

  {$IFDEF FooFunc_Body}
  //begin
    VarProcedureA := ArgProcedure;
    VarFunctionA := ArgFunction;
    VarObjProcedureA := ArgObjProcedure;
    VarObjFunctionA := ArgObjFunction;
  {$ENDIF}
{%endregion FooFunc}

{%region GLOBAL}
  {$IFDEF Global_Implementation}
    procedure ProcProcedure(a1: Integer; var a2: String); begin end;
    function  FuncFunction(a1: Integer; var a2: String): Integer; begin end;
    type
      TMethodHolderClass = class
      public
        procedure ProcObjProcedure(a1: Integer; var a2: String);
        function  FuncObjFuntion(a1: Integer; var a2: String): Integer;
      end;
    procedure TMethodHolderClass.ProcObjProcedure(a1: Integer; var a2: String); begin end;
    function  TMethodHolderClass.FuncObjFuntion(a1: Integer; var a2: String): Integer; begin end;
  {$ENDIF}

  {$IFDEF Global_Types}
  //type
    TProcedure = procedure(a1: Integer; var a2: String);
    TFunction  = function(a1: Integer; var a2: String): Integer;
    TObjProcedure = procedure(a1: Integer; var a2: String) of object;
    TObjFunction  = function(a1: Integer; var a2: String): Integer of object;
  {$ENDIF}

  {$IFDEF Global_Var}
  //var
    MethodHolderClass: TMethodHolderClass;
    GlobProcedure: TProcedure;
    GlobFunction: TFunction;
    GlobObjProcedure: TObjProcedure;
    GlobObjFunction: TObjFunction;
  {$ENDIF}

  {$IFDEF Global_Body}
  //begin
    MethodHolderClass := TMethodHolderClass.Create;
    GlobProcedure := @ProcProcedure;
    GlobFunction := @FuncFunction;
    GlobObjProcedure := @MethodHolderClass.ProcObjProcedure;
    GlobObjFunction := @MethodHolderClass.FuncObjFuntion;
  {$ENDIF}

  {$IFDEF Global_Body_NIL}
  //begin
    GlobProcedure := nil;
    GlobFunction := nil;
    GlobObjProcedure := nil;
    GlobObjFunction := nil;
  {$ENDIF}

  {$IFDEF Global_Call_FooFunc}
  //FooFunc(
    GlobProcedure,    GlobProcedure,
    GlobFunction,     GlobFunction,
    GlobObjProcedure, GlobObjProcedure,
    GlobObjFunction,  GlobObjFunction,
  {$ENDIF}
{%endregion GLOBAL}

