program tspecialize_inline;
{$mode ObjFPC}{$H+}
{$Interfaces CORBA}

(*
  Access fields/members in specialized generics, with NO named type for
  the specialization.

  specialize TGen<T>.Something
  specialize TGen<T1, T2>.Something
  Prefix.specialize TGen<T1, T2>.Something

  * Context
    The specialize can be
    * for class/interface/object
      - a base class (class/interface/object)
      - an interface in the ancestor list
      - a type for a field, property, var, other type (alias)
      - method param-type / result
    * generic procedure
      - generic procedure/function param-type / result
    * other
      - normal source (begin ... end)

  * Something can be
    - simple field (fixed type)
    - generic field: FSomeThing: T;
      a sub-field in the type T
    - sub-field in ancestor on type from Gen-Param (passed to ancestor)


  * Nested SomeThing
    when something comes from T
    then T itself may be specialized (inline / without type name)



  TODO: partial code
    specialize TFoo|  // missing <...> // or stopping/error anywhere inside <>

*)

uses u_specialize_inline;

type
  generic TGenClassCommon<T> = class F:T; end;

  generic TGenMain1<A> = class public type TX = A; end; // not used

  TClassMain1 = class
    FM1: integer;
    class procedure CallM1; virtual; abstract;
    //procedure Foo(a:byte); virtual; abstract;
  public const
    C1 = 99;
  public type
    TM1 = byte;
  private type
    generic TGenMain1<A> = class
      F:A;
    public type
      TX = A;
    end;
  end;

  TClassMain1Dummy = class // just a 2nd declaration, to make sure params aren't mixed up along the way
    FM1: integer;
    class procedure CallM2; virtual; abstract;
  public type
    TM1 = byte;
    generic TGenMain1<A> = class
      F:A;
    end;
  end;



  { TTest_ClassA }

  TTest_ClassA = class(
    specialize     {completion:TClassMain1;TClassMain1Dummy;u_specialize_inline;TGenClassA;!FDataA}
      TGenClassA   {declaration:u_specialize_inline.TGenClassA}
      <TClassMain1 {declaration:TClassMain1}
      >
      .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
  )
  public
    FBaseData: specialize
                             {completion:TClassMain1;TClassMain1Dummy;u_specialize_inline;TTest_ClassA;!FDataA}
                TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                <            {completion:TClassMain1;TClassMain1Dummy;u_specialize_inline;TTest_ClassA;!FDataA}
                 TClassMain1 {declaration:TClassMain1}
                >
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
                ;
    FBaseData2: specialize
                TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                <TClassMain1 {declaration:TClassMain1}
                >
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
                .            {completion:TM1;TGenMain1;!TSelf;!TNestClassA;!FDataA}
                TM1          {declaration:TClassMain1.TM1}
                ;
    FBaseData2a: specialize
                TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                <            {completion:TClassMain1;TClassMain1Dummy;u_specialize_inline;TTest_ClassA;!FDataA}
                 tspecialize_inline {declaration:tspecialize_inline}
                 .           {completion:TClassMain1;TClassMain1Dummy}
                 TClassMain1 {declaration:TClassMain1}
                >
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
                .            {completion:TM1;TGenMain1;!TSelf;!TNestClassA;!FDataA}
                TM1          {declaration:TClassMain1.TM1}
                ;
    FBaseData3: specialize
                TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                <TClassMain1 {declaration:TClassMain1}
                >
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TSelf        {declaration:u_specialize_inline.TGenClassA.TSelf}
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
                ;
    FBaseData4: specialize
                TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                <TClassMain1 {declaration:TClassMain1}
                >
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TSelf        {declaration:u_specialize_inline.TGenClassA.TSelf}
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
                .            {completion:TM1;TGenMain1;!TSelf;!TNestClassA;!FDataA}
                TM1          {declaration:TClassMain1.TM1}
                ;
    property M1: integer read {completion:FM1;FBaseData;FBaseData4;!FDataA;!FNestDataA}
                              FM1{declaration:TClassMain1.FM1}
                              ;
    property M2: specialize
                  TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                  <TClassMain1 {declaration:TClassMain1}
                  >
                  .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                  TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
                 read     {completion:FBaseData;FBaseData4;!FDataA;!FNestDataA}
                 FBaseData{declaration:TTest_ClassA.FBaseData}
                 ;
    function PM(a:
                specialize
                TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                <TClassMain1 {declaration:TClassMain1}
                >
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
               ):
               specialize
                TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                <TClassMain1 {declaration:TClassMain1}
                >
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
                ;
  end;


function TTest_ClassA.PM(a:
                specialize
                TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                <TClassMain1 {declaration:TClassMain1}
                >
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
               ):
               specialize TGenClassA<TClassMain1>.TTypeA;
type
  LType1 = specialize
           TGenClassA     {declaration:u_specialize_inline.TGenClassA}
           <TClassMain1   {declaration:TClassMain1}
           >.             {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
           TTypeA         {declaration:u_specialize_inline.TGenClassA.TTypeA}
           ;
  LType2 = type specialize
           TGenClassA     {declaration:u_specialize_inline.TGenClassA}
           <TClassMain1   {declaration:TClassMain1}
           >.             {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
           TSelf          {declaration:u_specialize_inline.TGenClassA.TSelf}
           .              {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
           TTypeA         {declaration:u_specialize_inline.TGenClassA.TTypeA}
           ;
const
  n = nil;
  LConst1: specialize
           TGenClassA     {declaration:u_specialize_inline.TGenClassA}
           <TClassMain1   {declaration:TClassMain1}
           >.             {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
           TTypeA         {declaration:u_specialize_inline.TGenClassA.TTypeA}
           = n            {declaration:TTest_ClassA.PM.n}
           ;
var
  Local1: specialize
           TGenClassA     {declaration:u_specialize_inline.TGenClassA}
           <TClassMain1   {declaration:TClassMain1}
           >.             {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
           TTypeA         {declaration:u_specialize_inline.TGenClassA.TTypeA}
           = n            {declaration:TTest_ClassA.PM.n}
           ;
  c: word;
begin
  a     {declaration:TTest_ClassA.PM.a}
   := n {declaration:TTest_ClassA.PM.n}
   ;

  a.     {completion:FM1;!FDataA}
    FM1  {declaration:TClassMain1.FM1}
    := 1;

  result
   := n    {declaration:TTest_ClassA.PM.n}
   ;

  result.   {completion:FM1;!FDataA}
    FM1     { TODO: declaration:TClassMain1.FM1}
    := 1;

  FBaseData.{completion:FM1;!FDataA}
    FM1     {declaration:TClassMain1.FM1}
    := 1;

  self.     {completion:FBaseData;FBaseData2;FBaseData4;M1;M2;PM;!FDataA}
  FBaseData.{completion:FM1;!FDataA}
    FM1     {declaration:TClassMain1.FM1}
    := TM1  {declaration:TClassMain1.TM1}
    (1);

  FBaseData3.{completion:FM1;!FDataA}
    FM1      {declaration:TClassMain1.FM1}
    := 1;

  Local1.  {completion:FM1;!FDataA}
    FM1    {declaration:TClassMain1.FM1}
    :=
    M2     {declaration:TTest_ClassA.M2}
    .      {completion:FM1;!FDataA}
    FM1    {declaration:TClassMain1.FM1}
    ;

  Local1    { TODO: guesstype:}
    :=      {completion:FBaseData;FBaseData2;FBaseData4;M1;M2;PM;!FDataA}
    PM      {declaration:TTest_ClassA.PM}
    (LConst1);

  Local1 :=
    specialize
           TGenClassA     {declaration:u_specialize_inline.TGenClassA}
           <TClassMain1   {declaration:TClassMain1}
           >.             {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
           TTypeA         {declaration:u_specialize_inline.TGenClassA.TTypeA}
           ( n            {declaration:TTest_ClassA.PM.n}
           );

  c := PM(nil).  {completion:FM1;!FDataA}
    FM1          {declaration:TClassMain1.FM1}
    ;

  specialize
     TGenClassA     {declaration:u_specialize_inline.TGenClassA}
     <TClassMain1   {declaration:TClassMain1}
     >.             {completion:P1;TTypeA;TSelf;TNestClassA;!TNestTypeA}
     P1             {declaration:u_specialize_inline.TGenClassA.P1}
     (n             {declaration:TTest_ClassA.PM.n}
     )
     ;

  specialize
     TGenClassA     {declaration:u_specialize_inline.TGenClassA}
     <TClassMain1   {declaration:TClassMain1}
     >.             {completion:P1;TTypeA;TSelf;TNestClassA;!TNestTypeA}
     TTypeA         {declaration:u_specialize_inline.TGenClassA.TTypeA}
     .              { TODO: completion:CallM1;Create;!FDataA}
     CallM1         { TODO: declaration:TClassMain1.CallM1}
     ;

end;

type

  TTest_ClassA1 = class(
    u_specialize_inline.specialize {completion:TGenClassA;TGenClassB;!TNestTypeA}
      TGenClassA           {declaration:u_specialize_inline.TGenClassA}
      <tspecialize_inline  {declaration:tspecialize_inline}
       .                   {completion:TClassMain1;TClassMain1Dummy;TTest_ClassA;!FDataA}  // TODO: !TGenClassA;!TGenClassB;
       TClassMain1         {declaration:TClassMain1}
      >.                   {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TTypeA               {declaration:u_specialize_inline.TGenClassA.TTypeA}
  )
  public type
      TList = array of specialize
      TGenClassA   {declaration:u_specialize_inline.TGenClassA}
      <TClassMain1 {declaration:TClassMain1}
      >
      .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
      .
      TM1          {declaration:TClassMain1.TM1}
      ;

      TSet = set of specialize
      TGenClassA   {declaration:u_specialize_inline.TGenClassA}
      <TClassMain1 {declaration:TClassMain1}
      >
      .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
      .
      TM1          {declaration:TClassMain1.TM1}
      ;

(*
      TRange =
        specialize
        TGenClassA   { declaration:u_specialize_inline.TGenClassA}
        <TClassMain1 { declaration:TClassMain1}
        >
        .            { completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
        TTypeA       { declaration:u_specialize_inline.TGenClassA.TTypeA}
        .
        C1           { declaration:TClassMain1.C1}
        ..
        high(specialize
        TGenClassA   { declaration:u_specialize_inline.TGenClassA}
        <TClassMain1 { declaration:TClassMain1}
        >
        .            { completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
        TTypeA       { declaration:u_specialize_inline.TGenClassA.TTypeA}
        .
        TM1          { declaration:TClassMain1.TM1}
        )
        ;
//*)
  public
    FBaseData: specialize
                             {completion:TClassMain1;TClassMain1Dummy;u_specialize_inline;TTest_ClassA;!FDataA}
                TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                <            {completion:TClassMain1;TClassMain1Dummy;u_specialize_inline;TTest_ClassA;!FDataA}
                 TClassMain1 {declaration:TClassMain1}
                >
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
                ;
    property M1: integer read FM1;
    function PM(a:
                u_specialize_inline {declaration:tspecialize_inline.u_specialize_inline}
                .
                specialize
                  TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                  <tspecialize_inline {declaration:tspecialize_inline}
                   .
                   TClassMain1 {declaration:TClassMain1}
                  >
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
                ;
                const b, c:
                specialize
                  TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                  <specialize
                    TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                    <TClassMain1 {declaration:TClassMain1}
                    >
                  >
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TSelf        {declaration:u_specialize_inline.TGenClassA.TSelf}
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}

               ):
               specialize
                TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                <TClassMain1 {declaration:TClassMain1}
                >
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
                ;
  end;

function TTest_ClassA1.PM {declaration:TTest_ClassA1.PM}
               (a:
                u_specialize_inline {declaration:tspecialize_inline.u_specialize_inline}
                .
                specialize
                  TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                  <tspecialize_inline {declaration:tspecialize_inline}
                   .
                   TClassMain1 {declaration:TClassMain1}
                  >
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
                ;
                const b, c:
                specialize
                  TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                  <specialize
                    TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                    <TClassMain1 {declaration:TClassMain1}
                    >
                  >
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TSelf        {declaration:u_specialize_inline.TGenClassA.TSelf}
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}

               ):
               specialize
                TGenClassA   {declaration:u_specialize_inline.TGenClassA}
                <TClassMain1 {declaration:TClassMain1}
                >
                .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
                TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
                ;
type
      TLocalList = array of specialize
      TGenClassA   {declaration:u_specialize_inline.TGenClassA}
      <TClassMain1 {declaration:TClassMain1}
      >
      .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
      .
      TM1          {declaration:TClassMain1.TM1}
      ;

      TLocalSet = set of specialize
      TGenClassA   {declaration:u_specialize_inline.TGenClassA}
      <TClassMain1 {declaration:TClassMain1}
      >
      .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
      .
      TM1          {declaration:TClassMain1.TM1}
      ;

(*
      TLocalRange =
        specialize
        TGenClassA   {declaration:u_specialize_inline.TGenClassA}
        <TClassMain1 {declaration:TClassMain1}
        >
        .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
        TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
        .
        C1           {declaration:TClassMain1.C1}
        ..
        high(specialize
        TGenClassA   {declaration:u_specialize_inline.TGenClassA}
        <TClassMain1 {declaration:TClassMain1}
        >
        .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
        TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
        .
        TM1          {declaration:TClassMain1.TM1}
        )
        ;
//*)
const
      TConstList : array of specialize
      TGenClassA   {declaration:u_specialize_inline.TGenClassA}
      <TClassMain1 {declaration:TClassMain1}
      >
      .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
      .
      TM1          {declaration:TClassMain1.TM1}
      = ()
      ;

      TConstSet : set of specialize
      TGenClassA   {declaration:u_specialize_inline.TGenClassA}
      <TClassMain1 {declaration:TClassMain1}
      >
      .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
      .
      TM1          {declaration:TClassMain1.TM1}
      = []
      ;

(*
      TConstRange :
        specialize
        TGenClassA   {declaration:u_specialize_inline.TGenClassA}
        <TClassMain1 {declaration:TClassMain1}
        >
        .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
        TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
        .
        C1           {declaration:TClassMain1.C1}
        ..
        high(specialize
        TGenClassA   {declaration:u_specialize_inline.TGenClassA}
        <TClassMain1 {declaration:TClassMain1}
        >
        .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
        TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
        .
        TM1          {declaration:TClassMain1.TM1}
        )
        =1
        ;
//*)
var
      TVarList : array of specialize
      TGenClassA   {declaration:u_specialize_inline.TGenClassA}
      <TClassMain1 {declaration:TClassMain1}
      >
      .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
      .
      TM1          {declaration:TClassMain1.TM1}
      = ()
      ;

      TVarSet : set of specialize
      TGenClassA   {declaration:u_specialize_inline.TGenClassA}
      <TClassMain1 {declaration:TClassMain1}
      >
      .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
      .
      TM1          {declaration:TClassMain1.TM1}
      = []
      ;

(*
      TVarRange :
        specialize
        TGenClassA   {declaration:u_specialize_inline.TGenClassA}
        <TClassMain1 {declaration:TClassMain1}
        >
        .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
        TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
        .
        C1           {declaration:TClassMain1.C1}
        ..
        high(specialize
        TGenClassA   {declaration:u_specialize_inline.TGenClassA}
        <TClassMain1 {declaration:TClassMain1}
        >
        .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
        TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
        .
        TM1          {declaration:TClassMain1.TM1}
        )
        =1
        ;
//*)
begin

end;

type
  TTest_B = specialize TGenClassA  {declaration:u_specialize_inline.TGenClassA}
    < specialize TGenClassB        {declaration:u_specialize_inline.TGenClassB}
        <TClassMain1               {declaration:TClassMain1}
        ,
        TClassMain1Dummy           {declaration:TClassMain1Dummy}
        >
    >.                             {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
    TTypeA                         {declaration:u_specialize_inline.TGenClassA.TTypeA}
    // TTypeA = base = TGenClassB(TClassMain1)
    .
    //TODO: completion after specialize keyword
    specialize TGenMain1           {declaration!:TClassMain1.TGenMain1}
    <byte>
    ;


  TTest_ClassB = class(
    specialize TGenClassA      {declaration:u_specialize_inline.TGenClassA}
      < specialize TGenClassB  {declaration:u_specialize_inline.TGenClassB}
        <TClassMain1           {declaration:TClassMain1}
         ,
         TClassMain1Dummy      {declaration:TClassMain1Dummy}
        >
      >.                       {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TTypeA                   {declaration:u_specialize_inline.TGenClassA.TTypeA}
      // TTypeA = base = TGenClassB(TClassMain1)
  )
  public
    FMe: specialize TGenClassA  {declaration:u_specialize_inline.TGenClassA}
      < specialize TGenClassB   {declaration:u_specialize_inline.TGenClassB}
        <TClassMain1            {declaration:TClassMain1}
         ,
         TClassMain1Dummy       {declaration:TClassMain1Dummy}
        >
        .                       {completion:TTypeB1;TTypeB2;P1;TSelf;TNestClassB;TM1;!TNestClassA;!TTypeA;!TNestTypeA}
        TSelf                   {declaration:u_specialize_inline.TGenClassB.TSelf}
      >.                        {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TTypeA                    {declaration:u_specialize_inline.TGenClassA.TTypeA}
      ;
    FMe2: specialize TGenClassA {declaration:u_specialize_inline.TGenClassA}
      < specialize TGenClassB   {declaration:u_specialize_inline.TGenClassB}
        <TClassMain1            {declaration:TClassMain1}
         ,
         TClassMain1Dummy       {declaration:TClassMain1Dummy}
        >.                      {completion:TTypeB1;TTypeB2;P1;TSelf;TNestClassB;TM1;!TNestClassA;!TTypeA;!TNestTypeA}
        TSelf                   {declaration:u_specialize_inline.TGenClassB.TSelf}
        .                       {completion:TTypeB1;TTypeB2;P1;TSelf;TNestClassB;TM1;!TNestClassA;!TTypeA;!TNestTypeA}
        TSelf                   {declaration:u_specialize_inline.TGenClassB.TSelf}
      >.                        {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TSelf                     {declaration:u_specialize_inline.TGenClassA.TSelf}
      .                         {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TTypeA                    {declaration:u_specialize_inline.TGenClassA.TTypeA}
      ;

    property M1: integer read FM1  {declaration:TClassMain1.FM1}
      ;
    property M2: TTypeB2 read FDataA {declaration:u_specialize_inline.TGenClassB.FDataA}
      ;
    // Make the name specialize
    property specialize: specialize TGenClassA
      < specialize TGenClassB
        <TClassMain1
         ,
         TClassMain1Dummy
        > // TTypeA = base = TGenClassB(TClassMain1)
      >.
      TTypeA
      read FMe  {declaration:TTest_ClassB.FMe}
      ;
    function PM(a:
      specialize TGenClassE      {declaration:u_specialize_inline.TGenClassE}
      < TGenParamWrapperClassC   {declaration:u_specialize_inline.TGenParamWrapperClassC}
        ,
        specialize TGenClassD    {declaration:u_specialize_inline.TGenClassD}
        < TGenParamWrapperClassC {declaration:u_specialize_inline.TGenParamWrapperClassC}
          ,
          TClassMain1            {declaration:TClassMain1}
        >.                       {completion:TTypeD}
        TTypeD                   {declaration:u_specialize_inline.TGenClassD.TTypeD}
      >.                         {completion:u_specialize_inline.TGenClassE.TTypeD,!u_specialize_inline.TGenClassD.TTypeD}
      TSelf
    ): specialize TGenClassA     {declaration:u_specialize_inline.TGenClassA}
      < specialize TGenClassB    {declaration:u_specialize_inline.TGenClassB}
        < TClassMain1            {declaration:TClassMain1}
         ,
         TClassMain1Dummy        {declaration:TClassMain1Dummy}
        > // TTypeA = base = TGenClassB(TClassMain1)
      >.                         {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TTypeA                     {declaration:u_specialize_inline.TGenClassA.TTypeA}
      ;
  end;

function TTest_ClassB.{completion:PM;P1;CallM1}
PM(a:
  specialize TGenClassE      {declaration:u_specialize_inline.TGenClassE}
  < TGenParamWrapperClassC   {declaration:u_specialize_inline.TGenParamWrapperClassC}
    ,
    specialize TGenClassD    {declaration:u_specialize_inline.TGenClassD}
    < TGenParamWrapperClassC {declaration:u_specialize_inline.TGenParamWrapperClassC}
      ,
      TClassMain1            {declaration:TClassMain1}
    >.                       {completion:TTypeD}
    TTypeD                   {declaration:u_specialize_inline.TGenClassD.TTypeD}
  >.                         {completion:u_specialize_inline.TGenClassE.TTypeD,!u_specialize_inline.TGenClassD.TTypeD}
  TSelf
): specialize TGenClassA     {declaration:u_specialize_inline.TGenClassA}
  < specialize TGenClassB    {declaration:u_specialize_inline.TGenClassB}
    < TClassMain1            {declaration:TClassMain1}
     ,
     TClassMain1Dummy        {declaration:TClassMain1Dummy}
    > // TTypeA = base = TGenClassB(TClassMain1)
  >.                         {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
  TTypeA                     {declaration:u_specialize_inline.TGenClassA.TTypeA}
  ;
begin
end;

type

  TTest_ClassB2 = class(
    specialize TGenClassB    {declaration:u_specialize_inline.TGenClassB}
    < specialize TGenClassA  {declaration:u_specialize_inline.TGenClassA}
      < TClassMain1          {declaration:TClassMain1}
      >.                     {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
      TTypeA                 {declaration:u_specialize_inline.TGenClassA.TTypeA}
      , // base = TClassMain1
      TClassMain1Dummy       {declaration:TClassMain1Dummy}
    >.                       {completion:TTypeB1;TTypeB2;P1;TSelf;TNestClassB;TM1;!TNestClassA;!TTypeA;!TNestTypeA}
    TNestClassB              {declaration:u_specialize_inline.TGenClassB.TNestClassB}
    // base is TClassMain1
  )
  public
    FFoo: specialize TGenMain1  {declaration:TClassMain1.TGenMain1}
      <TSelf                    {declaration:u_specialize_inline.TGenClassB.TNestClassB.TSelf}
      >
      .                         {completion:TX}
      TX                        {declaration:TClassMain1.TGenMain1.TX}
      .                         {completion:TSelf;TM1}
      TM1                       {declaration:TClassMain1.TM1}
      ;
    FMe: specialize TGenClassB {declaration:u_specialize_inline.TGenClassB}
      < specialize TGenClassA  {declaration:u_specialize_inline.TGenClassA}
        < TClassMain1          {declaration:TClassMain1}
        >.                     {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
        TTypeA                 {declaration:u_specialize_inline.TGenClassA.TTypeA}
        , // base = TClassMain1
        TClassMain1Dummy
      >.                       {completion:TTypeB1;TTypeB2;P1;TSelf;TNestClassB;TM1;!TNestClassA;!TTypeA;!TNestTypeA}
      TNestClassB              {declaration:u_specialize_inline.TGenClassB.TNestClassB}
      ;
    property M1: integer read FM1;
    property Me: specialize TGenClassB<
           specialize TGenClassA<TClassMain1>.TTypeA, // base = TClassMain1
           TClassMain1Dummy
         >.TNestClassB read FMe;
    // Make the name specialize
    function PM(specialize: specialize TGenClassB<
                   specialize TGenClassA
                   < TClassMain1
                   >.
                   TTypeA                {declaration:u_specialize_inline.TGenClassA.TTypeA}
                   , // base = TClassMain1
                   TClassMain1Dummy
                 >.
                 TNestClassB             {declaration:u_specialize_inline.TGenClassB.TNestClassB}
               ): specialize TGenClassB
                 < specialize TGenClassA
                   <TClassMain1
                   >.
                   TTypeA                {declaration:u_specialize_inline.TGenClassA.TTypeA}
                   , // base = TClassMain1
                   TClassMain1Dummy
                 >.                      {completion:TTypeB1;TTypeB2;P1;TSelf;TNestClassB;TM1;!TNestClassA;!TTypeA;!TNestTypeA}
                 TNestClassB             {declaration:u_specialize_inline.TGenClassB.TNestClassB}
                 ;
  end;

function TTest_ClassB2.PM(specialize: specialize TGenClassB<
               specialize TGenClassA
               < TClassMain1
               >.
               TTypeA                {declaration:u_specialize_inline.TGenClassA.TTypeA}
               , // base = TClassMain1
               TClassMain1Dummy
             >.
             TNestClassB             {declaration:u_specialize_inline.TGenClassB.TNestClassB}
           ): specialize TGenClassB
             < specialize TGenClassA
               <TClassMain1
               >.
               TTypeA                {declaration:u_specialize_inline.TGenClassA.TTypeA}
               , // base = TClassMain1
               TClassMain1Dummy
             >.                      {completion:TTypeB1;TTypeB2;P1;TSelf;TNestClassB;TM1;!TNestClassA;!TTypeA;!TNestTypeA}
             TNestClassB             {declaration:u_specialize_inline.TGenClassB.TNestClassB}
             ;
begin
  {completion:FNestDataB;FM1}
  FNestDataB {declaration:u_specialize_inline.TGenClassB.TNestClassB.FNestDataB}
    := nil;
end;



type

  TNew_GenParamWrapperClassC = class(TGenParamWrapperClassC)
  public type
    generic TGenClassC<T: class> = class
    public type
      TTypeC = class(T)
        FExtra: Byte;
      end;
      TSelf = TGenClassC;
    public
      FNewDataC: T;
      FNewDataC2: TTypeC;
    end;
  end;

  TNew2_GenParamWrapperClassC = class(TGenParamWrapperClassC)
  public type
    generic TGenClassC<T: class> = class
    public type
       TTypeC = class(T)
        FExtra2: Byte;
      end;
      TSelf = TGenClassC;
    public
      FNewDataC: T;
      FNewDataC2: TTypeC;
    end;
  end;

  { TTest_ClassE }

  TTest_ClassE = class(
    specialize TGenClassE            {declaration:u_specialize_inline.TGenClassE}
      // TypeD = "1st param" . TypeC
      //       TNew_GenParamWrapperClassC.TGenClassC < "2nd param" > . TTypeC
      < TNew_GenParamWrapperClassC   {declaration:TNew_GenParamWrapperClassC}
       ,
       // BaseClass = "2nd param"
       //  TNew2_GenParamWrapperClassC.TGenClassC.TTypeC( TClassMain1 )
       specialize TGenClassD         {declaration:u_specialize_inline.TGenClassD}
       < TNew2_GenParamWrapperClassC {declaration:TNew2_GenParamWrapperClassC}
         ,
         TClassMain1                 {declaration:TClassMain1}
       >.                            {completion:TTypeD,!TM1,!TTypeC}
       //   TTypeD = TNew2_GenParamWrapperClassC.TGenClassC < TClassMain1 > . TTypeC
       TTypeD                        {declaration:u_specialize_inline.TGenClassD.TTypeD}
    >.                               {completion:TTypeD,TSelf} // TODO: TM1, !TTypeC
    TSelf                            {declaration:u_specialize_inline.TGenClassE.TSelf}
  )
  public
    procedure Foo;
  end;

procedure TTest_ClassE.Foo;
var a: TTypeD;
begin
  // TODO: !TTypeC   // that is the name of the base class
  // TODO: !FNewDataC , TypeD
  { TODO completion:Foo,FDataA,FDataA2,P1,P2,TSelf,TM1,FM1,!PXD}
  FM1      { TODO: declaration:TClassMain1.FM1}
  := 1;
  FExtra2  {declaration:TNew2_GenParamWrapperClassC.TGenClassC.TTypeC.FExtra2}
  := 1;

  a
  .        { TODO: completion:FExtra}
  FExtra   { TODO: declaration:TClassMain1.FM1}
  := 1;
  a
  .        { TODO: completion:FExtra}
  FM1      { TODO: declaration:TClassMain1.FM1}
  := 1;
end;


type

  generic TTestG1<
    A: TClassMain1;
    B: tspecialize_inline.TClassMain1;
    C: specialize
        TGenClassA   {declaration:u_specialize_inline.TGenClassA}
        <TClassMain1 {declaration:TClassMain1}
        >
        .            {completion:TTypeA;TSelf;TNestClassA;!TNestTypeA}
        TTypeA       {declaration:u_specialize_inline.TGenClassA.TTypeA}
    >
   = class(C)
   public
     FA: A;
     FB: B;
     property M1: integer read FM1;
   end;

type


  { TTestI1 }

  TTestI1 = class(
    specialize TGenClassA<TClassMain1>.TTypeA,
    specialize TGenIfaceA<TClassMain1Dummy>,
    specialize TGenIClassC<QWord>.TIFaceC
  )
  public
    function Foo  {declaration:u_specialize_inline.TGenIFaceA.Foo}
      (a: TClassMain1Dummy): TClassMain1Dummy; overload;
    function Bar  {declaration:u_specialize_inline.TGenIClassC.TIFaceC.Bar}
      (a: QWord): QWord; virtual; abstract;
  end;


function TTestI1.Foo(a: TClassMain1Dummy): TClassMain1Dummy;
begin

end;


type


  TDoubleSpec1 = u_specialize_inline.specialize TGenClassA
    <TClassMain1
    >.
    TTypeA
    .specialize TGenMain1 {declaration:TClassMain1.TGenMain1}
    <word>;

  TDoubleSpec2 = specialize TGenClassA<TClassMain1>.TTypeA.specialize TGenMain1<word>;

  // use type via field
  TDoubleSpec3 = specialize TGenClassA<TClassMain1>.FDataA.specialize TGenMain1<word>;

  TDoubleSpec4 = specialize TGenClassA<
           specialize TGenClassB<TClassMain1, TClassMain1Dummy >
         >.TTypeA.specialize TGenMain1<
           specialize TGenClassB<TClassMain1, TClassMain1Dummy >
         >;
  TDoubleSpec5 = specialize TGenClassA<
           specialize TGenClassB<TClassMain1, specialize TGenIFaceA<byte> >
         >.TTypeA.specialize TGenMain1<
           specialize TGenClassB<TClassMain1, specialize TGenIFaceA<byte> >
         >;
  TDoubleSpec6 = u_specialize_inline.specialize TGenClassA<
           u_specialize_inline.specialize TGenClassB<TClassMain1, specialize TGenIFaceA<byte> >
         >.TTypeA.specialize TGenMain1<
           specialize TGenClassB<TClassMain1, u_specialize_inline.specialize TGenIFaceA<byte> >
         >;


  TCommon1 = specialize TGenClassCommon{declaration:TGenClassCommon}
  <byte>;
  TCommon2 = tspecialize_inline.specialize TGenClassCommon{declaration:TGenClassCommon}
  <byte>;
  TCommon3 = u_specialize_inline.specialize TGenClassCommon{declaration:u_specialize_inline.TGenClassCommon}
  <byte>;




procedure test(param: specialize TGenClassA<Integer>.TTypeA);
var
  d1: TDoubleSpec1;
  d2: TDoubleSpec2;
  d3: TDoubleSpec3;
  d4: TDoubleSpec4;
  d5: TDoubleSpec5;
  d6: TDoubleSpec6;
begin
  d1. {completion:F,TX}
  F   {declaration:TClassMain1.TGenMain1.F}
  := 1;

  d2.    {completion:F,TX}
  F      {declaration:TClassMain1.TGenMain1.F}
  := 1;

  d3.    {completion:F,TX}
  F      {declaration:TClassMain1.TGenMain1.F}
  := 1;

  d4.    {completion:F,TX}
  F      {declaration:TClassMain1.TGenMain1.F}
  := nil;
  d4.F.  {completion:P1;CallM1}
  P1     {declaration:u_specialize_inline.TGenClassB.P1}
  (nil);
  d4.F.  {completion:P1;CallM1}
  CallM1 {declaration:TClassMain1.CallM1}
  ;
  d4.F.TTypeB1.  {completion:CallM1}
  CallM1 {declaration:TClassMain1.CallM1}
  ;
  d4.F.TTypeB2.  {completion:CallM2}
  CallM2 {declaration:TClassMain1Dummy.CallM2}
  ;
  d4.F.FDataA.  {completion:CallM2}
  CallM2 {declaration:TClassMain1Dummy.CallM2}
  ;


  d5.    {completion:F,TX}
  F      {declaration:TClassMain1.TGenMain1.F}
  := nil;
  d5.F.  {completion:!Foo,CallM1}
  CallM1 {declaration:TClassMain1.CallM1}
  ;
  d5.F.TTypeB2(nil).  {completion:Foo}
  Foo    {declaration:u_specialize_inline.TGenIFaceA.Foo}
  (0);

  d6.    {completion:F,TX}
  F      {declaration:TClassMain1.TGenMain1.F}
  := nil;
  d6.F.  {completion:!Foo,CallM1}
  CallM1 {declaration:TClassMain1.CallM1}
  ;
  d6.F.TTypeB2(nil).  {completion:Foo}
  Foo    {declaration:u_specialize_inline.TGenIFaceA.Foo}
  (0);


end;



begin

end.

