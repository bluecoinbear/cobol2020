       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTCOB.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  X                   Pic 9(2).
       01  Price1              Pic x(8)   Value "$8000".
       01  Price2              Pic x(8)   Value "$2000".
       01  Output-Record.
           05  Product-Name    Pic x(20).
           05  Product-Number  Pic 9(9).
           05  Product-Price   Pic 9(6).
       Procedure Division.
           Compute Product-Price =
           Function Max (Function Numval-C(Price1) Function Numval-C(Price2))
           Compute X = Function Length(Output-Record)
           Move Function Upper-case (Product-Name) to Product-Name