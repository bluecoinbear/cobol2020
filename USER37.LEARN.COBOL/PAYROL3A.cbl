       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROL3A.
      *
      * A program that writes fomatted output from file input
      * 5 lines per person
      * A small calcuation is done based on the input data to calculate
      *   an employee bonus amount
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL
           ASSIGN TO UT-S-PAYROL3A
             ORGANIZATION IS SEQUENTIAL.
           SELECT PAYCHECK
           ASSIGN TO UT-S-PAYCHECK
             ACCESS MODE IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD  PAYROLL
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 90 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PAYROLL-REC.
       01  PAYROLL-REC  PIC X(90).
       FD  PAYCHECK
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS PAYCHECK-REC.
       01  PAYCHECK-REC  PIC X(80).
       WORKING-STORAGE SECTION.
      * End of File switch
       01  PAYROLL-EOF                 PIC X(01) VALUE SPACE.
       77  WAGE-FACTOR                 PIC V99   VALUE ZERO.
       01  PAYROLL-IN.
           05 NAME.
              10 FIRST-IN              PIC X(10).
              10 LAST-IN               PIC X(10).
           05  DATE-IN                 PIC X(10).
           05  HOURLY-RATE-IN          PIC 99V99.
           05  HOURS-WORKED-IN         PIC 9(02).
           05  CATEGORY-IN             PIC X(01).
           05  STREET-ADDR-IN          PIC X(10).
           05  CITY-STAT-ZIP-IN        PIC X(10).
           05  BANK-IN                 PIC X(12).
           05  CHECK-NBR-IN            PIC X(03).
           05  SALARY-IN               PIC 9(05)V99.
           05  MANAGEMENT-BONUS-IN     PIC V99.
           05  FILLER                  PIC X(9).
       01  BLANK-LINE.
           05  FILLER          PIC X(60)  VALUE SPACE.
       01  LINE1.
           05  FILLER              PIC X(05)  VALUE SPACE.
           05  NAME-OUT.
               10  FIRST-OUT       PIC X(10)  VALUE SPACE.
               10  LAST-OUT        PIC X(10)  VALUE SPACE.
           05  FILLER              PIC X(20)  VALUE SPACE.
           05  FILLER              PIC X(07)  VALUE 'CHECK# '.
           05  CHECK-NBR-OUT       PIC X(20)  VALUE SPACE.
       01  LINE2.
           05  FILLER          PIC X(05)  VALUE SPACE.
           05  STREET-ADDR-OUT     PIC X(20).
       01  LINE3.
           05  FILLER          PIC X(05)  VALUE SPACE.
           05  CITY-STATE-ZIP-OUT  PIC X(40).
           05  FILLER          PIC X(06)  VALUE 'DATE:'.
           05  DATE-OUT        PIC X(10)  VALUE SPACE.
       01  LINE4.
           05  FILLER          PIC X(05)  VALUE SPACE.
           05  FILLER          PIC X(20)  VALUE 'Pay to the order of '.
           05  NAME-OUT        PIC X(20)  VALUE SPACE.
           05  FILLER          PIC X(07)  VALUE 'AMOUNT:'.
           05  GROSS-PAY-OUT   PIC $$,$99.99.
       01  LINE5.
           05  FILLER          PIC X(05)  VALUE SPACE.
           05  BANK-OUT            PIC X(40)  VALUE SPACE.
       PROCEDURE DIVISION.
           PERFORM 000-Housekeeping.
           PERFORM 100-Main UNTIL PAYROLL-EOF = 'Y'.
           PERFORM 600-CLOSE-FILES.
           GOBACK.
       000-Housekeeping.
      * Initialization Routine
           INITIALIZE PAYROLL-IN.
           PERFORM 300-OPEN-FILES.
      * Priming Read
           PERFORM 400-Read-Payroll.
       100-Main.
           MOVE FIRST-IN         TO  FIRST-OUT.
           MOVE LAST-IN          TO  LAST-OUT.
           MOVE DATE-IN          TO  DATE-OUT.
           MOVE CHECK-NBR-IN     TO  CHECK-NBR-OUT.
           MOVE CITY-STAT-ZIP-IN TO  CITY-STATE-ZIP-OUT.
           MOVE STREET-ADDR-IN   TO  STREET-ADDR-OUT.
           MOVE FUNCTION CURRENT-DATE TO DATE-OUT.
           MOVE NAME             TO NAME-OUT OF LINE1 NAME-OUT OF LINE4,
           PERFORM  700-PROCESS-CHECK.
           PERFORM 500-Write-Paycheck.
           PERFORM 400-Read-Payroll.
       300-Open-Files.
           OPEN INPUT PAYROLL.
           OPEN OUTPUT PAYCHECK.
       400-Read-Payroll.
           READ PAYROLL INTO PAYROLL-IN
      * Set AT END Switch
               AT END MOVE "Y" TO PAYROLL-EOF
           END-READ.
       500-Write-Paycheck.
           WRITE PAYCHECK-REC FROM BLANK-LINE.
           WRITE PAYCHECK-REC FROM LINE1.
           WRITE PAYCHECK-REC FROM LINE2.
           WRITE PAYCHECK-REC FROM LINE3.
           WRITE PAYCHECK-REC FROM LINE4.
           WRITE PAYCHECK-REC FROM LINE5.
       600-CLOSE-FILES.
           CLOSE PAYROLL, PAYCHECK.
       700-PROCESS-CHECK.
      ** What if a category other than M, E or H shows up?
           IF CATEGORY-IN = "M"  THEN
            COMPUTE GROSS-PAY-OUT =
                        SALARY-IN * (1 + MANAGEMENT-BONUS-IN)
           ELSE IF CATEGORY-IN = "E"  THEN
            COMPUTE GROSS-PAY-OUT = SALARY-IN
           ELSE IF CATEGORY-IN = "H"  THEN
           COMPUTE GROSS-PAY-OUT = HOURLY-RATE-IN * HOURS-WORKED-IN.