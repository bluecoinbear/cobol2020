       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBTEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77   PRICE               PIC  S9(5)v99.
       77   EDITED-PRICE        PIC  $$,$$9.99.
       77   EDITED-PRICE-A      PIC  **,***.**.
       77   EDITED-PRICE-D      PIC  $$,$$9.99.
       77   EDITED-PRICE-S      PIC  -$$,$$9.99.
       PROCEDURE DIVISION.
           MOVE 11 TO PRICE
           MOVE PRICE TO EDITED-PRICE
           DISPLAY EDITED-PRICE
           MOVE .1 TO PRICE
           MOVE PRICE TO EDITED-PRICE
           DISPLAY EDITED-PRICE
           MOVE 12.11 TO PRICE
           MOVE PRICE TO EDITED-PRICE
           DISPLAY EDITED-PRICE.

           MOVE 11 TO PRICE
           MOVE PRICE TO EDITED-PRICE-A
           DISPLAY EDITED-PRICE-A
           MOVE .1 TO PRICE
           MOVE PRICE TO EDITED-PRICE-A
           DISPLAY EDITED-PRICE-A
           MOVE 12.11 TO PRICE
           MOVE PRICE TO EDITED-PRICE-A
           DISPLAY EDITED-PRICE-A.
           GOBACK.