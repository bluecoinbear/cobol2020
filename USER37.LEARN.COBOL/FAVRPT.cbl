       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRPT.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FAVIN  ASSIGN TO FAVIN.
           SELECT FAVOUT ASSIGN TO FAVOUT.
       DATA DIVISION.
       FILE SECTION.
       FD  FAVIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS FAVIN-REC.
       01  FAVIN-REC             PIC X(63).
       FD  FAVOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS FAVOUT-REC.
       01  FAVOUT-REC                  PIC X(55).
       WORKING-STORAGE SECTION.
       01  FAVIN-EOF                   PIC X.
       01  FAVIN-IN.
           05  NAME-IN                 PIC X(30).
           05  NUMBER-IN               PIC 9(02).
           05  GENRE-IN                PIC X(12).
           05  COST.
                10  CD-COST            PIC 9(3)V99.
                10  TAX                PIC 9(2)V99.
                10  SHIPPING           PIC 9(2)V99.
           05  BAND-IS-STILL-TOGETHER  PIC X.
           05  TOTAL                   PIC 9(3)V99.
       01  FAVOUT-OUT.
           05  NAME-OUT                PIC X(30).
           05  FILLER                  PIC X(8).
           05  NUMBER-OUT              PIC 9(02).
           05  FILLER                  PIC X(8).
           05  GENRE-OUT               PIC X(12).
           05  FILLER                  PIC XX.
           05  COST-OUT                PIC 9(3)V99.
           05  FILLER                  PIC XX.
           05  SHIPPING-OUT            PIC $$.99.
           05  FILLER                  PIC XX.
           05  TAX-OUT                 PIC $$.99.
           05  FILLER                  PIC XX.
           05  CD-COST-OUT             PIC $$$.99.

       PROCEDURE DIVISION.
       MAIN.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 050-FORMATTING.
           PERFORM 100-MAIN UNTIL FAVIN-EOF = 'Y'.
           PERFORM 600-CLOSE-FILES.
           GOBACK.
       000-HOUSEKEEPING.
      * Initialization Routine
      *     INITIALIZE FAVIN-REC, FAVOUT-REC.
      * Priming Read
           PERFORM 300-Open-Files.
           PERFORM 400-READ-FAVIN.  *> Comment out with empty input file
       050-FORMATTING.
           MOVE '' TO FAVOUT.
           WRITE FAVOUT-REC FROM FAVOUT.
           MOVE '' TO FAVOUT.
           WRITE FAVOUT-REC FROM FAVOUT.
           MOVE '' TO FAVOUT.
           WRITE FAVOUT-REC FROM FAVOUT.
       100-MAIN.
           DISPLAY '100-main'.       *> For shops not using the Debugger
           PERFORM 200-PROCESS-DATA.
           PERFORM 500-WRITE-FAVOUT.
           PERFORM 400-READ-FAVIN.
       200-PROCESS-DATA.
           MOVE NAME-IN TO NAME-OUT.
           MOVE NUMBER-IN TO NUMBER-OUT.
           MOVE GENRE-IN TO GENRE-OUT.
           MOVE CD-COST TO CD-COST-OUT.
           MOVE SHIPPING TO SHIPPING-OUT.
           COMPUTE COST-OUT ROUNDED = CD-COST
                                        + (CD-COST * TAX)
                                        + SHIPPING.
           COMPUTE TAX-OUT ROUNDED = CD-COST * TAX.
       300-OPEN-FILES.
           OPEN INPUT FAVIN.
           OPEN OUTPUT FAVOUT.
       400-READ-FAVIN.
           DISPLAY 'READ FAVIN'.
           READ FAVIN INTO FAVIN-IN
      * Set AT END Switch
           AT END
              MOVE 'Y' TO FAVIN-EOF
           END-READ.
       500-WRITE-FAVOUT.
           DISPLAY 'WRITE FAVOUT'.
           WRITE FAVOUT-REC FROM FAVOUT-OUT.
       600-CLOSE-FILES.
           CLOSE FAVIN, FAVOUT.