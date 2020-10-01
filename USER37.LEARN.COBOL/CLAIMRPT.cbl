       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CLAIMRPT.
      * This program takes a series of insurance claims, processes them
      * and writes a report based on which claims are accepted based on
      * the policy ammount showing the ammount to be paid in each claim.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLAIMFILE
             ASSIGN TO UT-S-CLAIM
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS CLAIMFILE-ST.
           SELECT PRINTFILE
             ASSIGN TO CLAIMRPT
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS PRINTFILE-ST.

       DATA DIVISION.
       FILE SECTION.
       FD  CLAIMFILE
           RECORD CONTAINS 90 CHARACTERS.
       01 CLAIM-RECORD                  PIC X(90).

       FD  PRINTFILE
           RECORD CONTAINS 140 CHARACTERS.
       01 PRT-LINE                    PIC X(140).

       WORKING-STORAGE SECTION.

       77 ALLOWED-AMT                   PIC S9(7)V99   VALUE 9999999.99.
       77 DEDUCTIBLE-PERC               PIC V999        VALUE .002.

       COPY CLAIMREC.

       01 PROGRAM-SWITCHES.
           05 REINSURANCE               PIC XX         VALUE SPACES.
           05 CLAIMFILE-EOF             PIC X(1)       VALUE 'N'.
               88 NO-MORE-CLAIMS                       VALUE 'Y'.
           05 CLAIMFILE-ST              PIC X(2).
               88 CLAIMFILE-OK                         VALUE '00'.
           05 PRINTFILE-ST              PIC X(2).
               88 PRINTFILE-OK                         VALUE '00'.
           05 BENEFIT-PERIOD            PIC X(1).
               88 BENEFIT-PERIOD-OK                    VALUE 'Y'.
           05 POLICY-DEDUCTIBLE-MET-WS  PIC X(1).
               88 DEDUCTIBLE-MET                       VALUE 'Y'.
           05 PAY-THE-CLAIM-WS          PIC X(1).
               88 PAY-THE-CLAIM                        VALUE 'Y'.

       01 COUNTERS-AND-ACCUMULATORS-WS.
           05 DEDUCTIBLE-WS             PIC S9(5)V99.
           05 CLAIM-PAID-WS             PIC S9(7)V99.

       01 DATE-FIELDS-WS.
           05 CURR-DATE-OUT             PIC X(10).
           05 CURR-DATE-WS              PIC S9(8).
           05 CURR-DATE-WS-X REDEFINES CURR-DATE-WS.
               10 WS-YEAR               PIC X(4).
               10 WS-MONTH              PIC X(2).
               10 WS-DAY                PIC X(2).

       01 TOT-BILL-INFORMATION.
           05 TOT-POLICY-AMOUNT         PIC S9(9)V99.
           05 TOT-DEDUCTIBLE-PAID       PIC S9(9)V99.
           05 TOT-CLAIM-AMOUNT-PAID     PIC S9(9)V99.
           05 TOT-CLAIM-AMOUNT          PIC S9(9)V99.

       01 HEADING-LINE-ONE.
           05 HDG-DATE                  PIC XXXX/XX/XX.
           05 FILLER                    PIC X(46)      VALUE SPACES.
           05 FILLER                    PIC X(25)
                  VALUE 'Group Claims Daily Totals'.

       01 HEADER-TWO.
           05 FILLER                    PIC X(24)      VALUE 'POLICY'.
           05 FILLER                    PIC X(11)      VALUE 'POLICY'.
           05 FILLER                    PIC X(15)      VALUE 'FIRST'.
           05 FILLER                    PIC X(13)      VALUE 'LAST'.
           05 FILLER                    PIC X(14)      VALUE 'RENEW'.
           05 FILLER                    PIC X(8)       VALUE 'COPAY'.
           05 FILLER                    PIC X(9)       VALUE 'COPAY'.
           05 FILLER                    PIC X(14)      VALUE 'DEDUC'.
           05 FILLER                    PIC X(14)      VALUE 'CLAIM'.
           05 FILLER                    PIC X(14)      VALUE 'CLAIM'.

       01 HEADER-THREE.
           05 FILLER                    PIC X(24)      VALUE 'TYPE'.
           05 FILLER                    PIC X(11)      VALUE 'NUMBER'.
           05 FILLER                    PIC X(15)      VALUE 'NAME'.
           05 FILLER                    PIC X(13)      VALUE 'NAME'.
           05 FILLER                    PIC X(15)      VALUE 'DATE'.
           05 FILLER                    PIC X(6)       VALUE 'MET'.
           05 FILLER                    PIC X(10)      VALUE 'PERCENT'.
           05 FILLER                    PIC X(14)      VALUE 'AMOUNT'.
           05 FILLER                    PIC X(14)      VALUE 'AMOUNT'.
           05 FILLER                    PIC X(14)      VALUE 'PAID'.

       01 HEADER-FOUR.
           05 FILLER                    PIC X(23)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(10)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(14)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(12)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(13)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(5)       VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(9)       VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(09)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(17)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(14)      VALUE ALL '-'.

       01 DETAIL-LINE.
           05 DET-POLICY-TYPE           PIC X(20)      VALUE SPACES.
           05 FILLER                    PIC X(4)       VALUE SPACES.
           05 DET-POLICY-NO             PIC 9B999B99.
           05 FILLER                    PIC X(3)       VALUE SPACES.
           05 DET-NAME.
               10 DET-FIRST-NAME        PIC X(15).
               10 DET-LAST-NAME         PIC X(10).
           05 FILLER                    PIC X(3)       VALUE SPACES.
           05 DET-RENEW-DATE            PIC XXXX/XX/XX.
           05 FILLER                    PIC X(6)       VALUE SPACES.
           05 DET-DEDUCTIBLE-MET        PIC X.
           05 FILLER                    PIC X(5)       VALUE SPACES.
           05 DET-DEDUCTIBLE-PERC       PIC .999.
           05 FILLER                    PIC X(4)       VALUE SPACES.
           05 DET-COINSURANCE           PIC $$$,$$9.
           05 FILLER                    PIC X(6)       VALUE SPACES.
           05 DET-CLAIM-AMOUNT          PIC $$,$$$,$$9.99.
           05 FILLER                    PIC X(3)       VALUE SPACES.
           05 DET-CLAIM-PAID            PIC $$,$$$,$$9.99.
           05 FILLER                    PIC X(2)       VALUE SPACES.

       01 TOTAL-DASH-LINE.
           05 FILLER                    PIC X(93)      VALUE SPACE.
           05 FILLER                    PIC X(09)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(17)      VALUE ALL '-'.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(14)      VALUE ALL '-'.
       01 TOTAL-LINE-OUT.
           05 FILLER                    PIC X(92)      VALUE SPACES.
           05 TOT-DEDUCTIBLE-OUT        PIC $$$,$$9.99.
           05 FILLER                    PIC XXX       VALUE SPACES.
           05 TOT-CLAIM-AMOUNT-OUT      PIC $$$,$$$,$$9.99.
           05 FILLER                    PIC XX        VALUE SPACES.
           05 TOT-CLAIM-AMOUNT-PAID-OUT PIC $$$,$$$,$$9.99.
           05 FILLER                    PIC X(5)       VALUE SPACES.

      *
       PROCEDURE DIVISION.
           PERFORM 100-HOUSEKEEPING.
           PERFORM 200-PROCESS-CLAIM UNTIL NO-MORE-CLAIMS.
           PERFORM 700-WRITE-CLAIM-TOTALS.
           PERFORM 900-WRAP-UP.
           GOBACK.

       100-HOUSEKEEPING.
      * INITIALIZATION ROUTINE
           INITIALIZE TOT-BILL-INFORMATION,
                      COUNTERS-AND-ACCUMULATORS-WS,
                      DATE-FIELDS-WS,
                      TOTAL-LINE-OUT.
           MOVE FUNCTION CURRENT-DATE TO HDG-DATE.
           PERFORM 150-OPEN-FILES.
           PERFORM 170-READ-CLAIMS.
           PERFORM 400-WRITE-HEADER.

       150-OPEN-FILES.
           OPEN INPUT CLAIMFILE
           IF NOT CLAIMFILE-OK
              DISPLAY 'CLAIM FILE PROBLEM'
              GO TO 999-ERROR-RTN.

           OPEN OUTPUT PRINTFILE
           IF NOT PRINTFILE-OK
              DISPLAY 'PRINT REPORT PROBLEM'
              GO TO 999-ERROR-RTN.

       170-READ-CLAIMS.
           READ CLAIMFILE INTO CLAIM-RECORD-WS
           AT END
              MOVE "Y" TO CLAIMFILE-EOF
           END-READ.
           IF CLAIMFILE-OK OR NO-MORE-CLAIMS
              DISPLAY 'CLAIM FILE OK'
           ELSE
              DISPLAY 'CLAIM FILE PROBLEM'
              GO TO 999-ERROR-RTN.

       200-PROCESS-CLAIM.
           IF CLAIM-AMOUNT < ALLOWED-AMT
               PERFORM 300-COMPUTE-CLAIM
               IF PAY-THE-CLAIM
                   PERFORM 500-WRITE-REPORT
                   PERFORM 600-INCREMENT-TOTALS
               END-IF
           END-IF
           PERFORM 170-READ-CLAIMS.


       300-COMPUTE-DEDUCTIBLE.
           COMPUTE DEDUCTIBLE-WS ROUNDED =
              POLICY-AMOUNT * DEDUCTIBLE-PERC

           IF POLICY-DEDUCTIBLE-PAID >= DEDUCTIBLE-WS
              MOVE "Y" TO POLICY-DEDUCTIBLE-MET-WS
           ELSE
              MOVE "N" TO POLICY-DEDUCTIBLE-MET-WS
           END-IF.

       300-COMPUTE-CLAIM.

           PERFORM 300-COMPUTE-DEDUCTIBLE
           IF DEDUCTIBLE-MET
              COMPUTE CLAIM-PAID-WS ROUNDED = CLAIM-AMOUNT
                - (POLICY-COINSURANCE) *(CLAIM-AMOUNT)

           ELSE
              COMPUTE CLAIM-PAID-WS ROUNDED = CLAIM-AMOUNT
                - DEDUCTIBLE-WS - (POLICY-COINSURANCE) *(CLAIM-AMOUNT)
           END-IF.

           SUBTRACT CLAIM-PAID-WS FROM POLICY-AMOUNT
           END-SUBTRACT.

           IF POLICY-AMOUNT > ZERO
              MOVE 'Y' TO PAY-THE-CLAIM-WS
           ELSE
              MOVE 'N' TO PAY-THE-CLAIM-WS
           END-IF.

       400-WRITE-HEADER.
           WRITE PRT-LINE FROM HEADING-LINE-ONE.
           MOVE SPACES      TO PRT-LINE.
           WRITE PRT-LINE.
           WRITE PRT-LINE FROM HEADER-TWO.
           WRITE PRT-LINE FROM HEADER-THREE.
           WRITE PRT-LINE FROM HEADER-FOUR.

       500-WRITE-REPORT.

           EVALUATE POLICY-TYPE
           WHEN 1
                MOVE 'EMPLOYER-PRIVATE'
                   TO DET-POLICY-TYPE
           WHEN 2
                MOVE 'STANDARD MEDICARE'
                   TO DET-POLICY-TYPE
           WHEN 3
                MOVE 'AFFORDABLE CARE ACT'
                   TO DET-POLICY-TYPE
           WHEN OTHER
                MOVE 'UNKNOWN' TO DET-POLICY-TYPE.

           MOVE INSURED-POLICY-NO TO DET-POLICY-NO.
           INSPECT DET-POLICY-NO REPLACING ALL ' ' BY '-'.
           MOVE SPACES                   TO DET-NAME.
           MOVE INSURED-LAST-NAME        TO DET-LAST-NAME.
           MOVE INSURED-FIRST-NAME       TO DET-FIRST-NAME.
           MOVE POLICY-BENEFIT-DATE-X    TO DET-RENEW-DATE.
           MOVE POLICY-DEDUCTIBLE-MET-WS TO DET-DEDUCTIBLE-MET.
           MOVE DEDUCTIBLE-PERC          TO DET-DEDUCTIBLE-PERC.
           MOVE DEDUCTIBLE-WS            TO DET-COINSURANCE.
           MOVE CLAIM-AMOUNT             TO DET-CLAIM-AMOUNT.
           MOVE CLAIM-AMOUNT-PAID        TO DET-CLAIM-PAID.

           WRITE PRT-LINE FROM DETAIL-LINE.

       600-INCREMENT-TOTALS.
           ADD DEDUCTIBLE-WS TO TOT-DEDUCTIBLE-PAID
           SIZE ERROR
              DISPLAY 'SIZE ERROR ON TOTAL CLAIM'
           END-ADD.
           ADD CLAIM-AMOUNT TO TOT-CLAIM-AMOUNT
           SIZE ERROR
              DISPLAY 'SIZE ERROR ON TOTAL CLAIM'
           END-ADD.
           ADD CLAIM-AMOUNT-PAID TO TOT-CLAIM-AMOUNT-PAID
           SIZE ERROR
              DISPLAY 'SIZE ERROR ON TOTAL CLAIM PAID'
           END-ADD.

       700-WRITE-CLAIM-TOTALS.
           WRITE PRT-LINE FROM TOTAL-DASH-LINE.
           MOVE TOT-DEDUCTIBLE-PAID   TO TOT-DEDUCTIBLE-OUT
           MOVE TOT-CLAIM-AMOUNT      TO TOT-CLAIM-AMOUNT-OUT
           MOVE TOT-CLAIM-AMOUNT-PAID TO TOT-CLAIM-AMOUNT-PAID-OUT
           WRITE PRT-LINE FROM TOTAL-LINE-OUT.
       900-WRAP-UP.
           CLOSE CLAIMFILE, PRINTFILE.
       999-ERROR-RTN.
           DISPLAY 'ERROR: CLOSING PROGRAM...'.
           CLOSE CLAIMFILE, PRINTFILE.
           GOBACK.