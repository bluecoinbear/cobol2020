       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTCOB.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  PRICE        PIC 9(5)v99.
       77  EDITED-PRICE PIC $ZZ,ZZ9.99.
       PROCEDURE DIVISION.
           DISPLAY "Hello World...COBOL is still alive and well"
           MOVE 12345.6 TO PRICE.
           MOVE PRICE TO EDITED-PRICE.
           DISPLAY EDITED-PRICE.

           GOBACK.