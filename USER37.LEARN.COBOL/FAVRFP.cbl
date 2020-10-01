       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRFP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RFPIN  ASSIGN TO RFPIN.
           SELECT RFPOUT ASSIGN TO RFPOUT.
       DATA DIVISION.
       FILE SECTION.
       FD  RFPIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RFP-REC.
       01  RFP-REC.
           05  ARTIST-ACCT-NO               PIC X(08).
           05  ARTIST-MUSICAL-GENRE         PIC X(06).
                88  ROCK     VALUE     "ROCK".
                88  JAZZ     VALUE     "JAZZ".
                88  FUSION   VALUE   "FUSION".
           05  MUSICIAN.
                10  MUSICIAN-LNAME          PIC X(15).
                10  MUSICIAN-FNAME          PIC X(15).
           05  MUSICIAN-INSTRUMENT-TYPE     PIC X(06).
           05  INSTRUMENT-QUALITY           PIC X(01).
                88  USED-FLAG       VALUE   'U'.
                88  NEW-FLAG        VALUE   'N'.
                88  PREMIUM-FLAG    VALUE   'P'.
           05   MAX-MUSICIAN-BUDGET-AMOUNT  PIC 9(05)V99.
           05   SHIP-TO                     PIC X(03).
                88  IN-COUNTRY        VALUE   'IN'.
                88  INTERNATIONAL     VALUE   'OUT'.
           05  FILLER                       PIC X(19).
       FD  RFPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RFPOUT-REC.
       01  RFPOUT-REC                       PIC X(80).
       WORKING-STORAGE SECTION.
       01  RFPIN-EOF                        PIC X(01).
       01  PROP-REC.
           05  ARTIST-ACCT-NO-O             PIC X(08).
           05  ARTIST-MUSICAL-GENRE-O       PIC X(06).
                88  ROCK             VALUE     "ROCK".
                88  JAZZ             VALUE     "JAZZ".
                88  FUSION           VALUE   "FUSION".
           05  MUSICIAN-O.
                10  MUSICIAN-LNAME-O        PIC X(15).
                10  MUSICIAN-FNAME-O        PIC X(15).
           05  MUSICIAN-INSTRUMENT-TYPE-O   PIC X(12).
                88  KEYBOARD         VALUE     'KEYS'.
                88  VOCALS           VALUE   'VOCALS'.
                88  GUITAR           VALUE   'GUITAR'.
                88  BASS             VALUE     'BASS'.
                88  DRUMS            VALUE    'DRUMS'.
                88  PERCUSSION       VALUE     'PERC'.
           05  INSTRUMENT-QUALITY-O         PIC X(01).
                88  USED-FLAG-O      VALUE   'U'.
                88  NEW-FLAG-O       VALUE   'N'.
                88  PREMIUM-FLAG-O   VALUE   'P'.
           05   SHIP-TO-O                   PIC X(03).
                88  IN-COUNTRY-O     VALUE   'IN'.
                88  INTERNATIONAL-O  VALUE   'OUT'.
           05   COST-PER-INSTRUMENT-O       PIC 9(07)V99.
           05   ADDITIONAL-COSTS-O.
                10  SHIPPING-COST-O         PIC S9(04)V99.
                10  TAX-O                   PIC S9(03)V99.
           05   FILLER                      PIC X(01).
       01  CALCULUS-VARIABLES.
           05  INSTRUMENT-COST              PIC 9(05)V99 VALUE ZEROS.
           05  QUALITY-MODIFIER             PIC 9(02)V99.
           05  TOTAL                        PIC $9(06)V99.
           05  DELIVERY-MODIFIER            PIC 9V9.
       PROCEDURE DIVISION.
       MAIN.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-MAIN UNTIL RFPIN-EOF = 'Y'.
           PERFORM 600-CLOSE-FILES.
           GOBACK.
       000-HOUSEKEEPING.
      * Initialization Routine
      *     INITIALIZE FAVIN-REC, FAVOUT-REC.
      * Priming Read
           PERFORM 300-OPEN-FILES.
           PERFORM 400-READ-RFPIN.  *> Comment out with empty input file
       100-MAIN.
           DISPLAY '100-main'.       *> For shops not using the Debugger
           PERFORM 200-PROCESS-DATA.
           PERFORM 500-WRITE-FAVOUT.
           PERFORM 400-READ-RFPIN.
       200-PROCESS-DATA.
           MOVE MUSICIAN TO MUSICIAN-O.
           MOVE ARTIST-ACCT-NO TO ARTIST-ACCT-NO-O.
           MOVE ARTIST-MUSICAL-GENRE TO ARTIST-MUSICAL-GENRE-O.
           MOVE MUSICIAN-INSTRUMENT-TYPE TO MUSICIAN-INSTRUMENT-TYPE-O.
           MOVE INSTRUMENT-QUALITY TO INSTRUMENT-QUALITY-O.
           MOVE SHIP-TO TO SHIP-TO-O.
      * Evaluating data to set the proper modifiers for the base price,
      * as well as setting the instrument price for each artist.
           EVALUATE TRUE
                WHEN MUSICIAN-INSTRUMENT-TYPE-O = 'KEYS'
                    MOVE 3017.89 TO INSTRUMENT-COST
                WHEN MUSICIAN-INSTRUMENT-TYPE-O = 'VOCALS'
                    MOVE 599.05 TO INSTRUMENT-COST
                WHEN MUSICIAN-INSTRUMENT-TYPE-O = 'GUITAR'
                    MOVE 2648.99 TO INSTRUMENT-COST
                WHEN MUSICIAN-INSTRUMENT-TYPE-O = 'BASS'
                    MOVE 1876.10 TO INSTRUMENT-COST
                WHEN MUSICIAN-INSTRUMENT-TYPE-O = 'DRUMS'
                    MOVE 3087.22 TO INSTRUMENT-COST
                WHEN MUSICIAN-INSTRUMENT-TYPE-O = 'PERC'
                    MOVE 799.99 TO INSTRUMENT-COST
                WHEN OTHER
                    MOVE 500 TO INSTRUMENT-COST
           END-EVALUATE.
           EVALUATE INSTRUMENT-QUALITY-O
                WHEN 'U'
                    MOVE 0.8 TO QUALITY-MODIFIER
                WHEN 'N'
                    MOVE 1   TO QUALITY-MODIFIER
                WHEN 'P'
                    MOVE 1.2 TO QUALITY-MODIFIER
                WHEN OTHER
                    MOVE 1   TO QUALITY-MODIFIER
           END-EVALUATE.
           EVALUATE SHIP-TO-O
                WHEN 'IN'
                    MOVE 0.1 TO DELIVERY-MODIFIER
                WHEN 'OUT'
                    MOVE 0.2 TO DELIVERY-MODIFIER
                WHEN OTHER
                    MOVE 0.2 TO DELIVERY-MODIFIER
           END-EVALUATE.
           DISPLAY 'COMPUTING COSTS'
           MOVE 0 TO COST-PER-INSTRUMENT-O.
           MOVE 0 TO SHIPPING-COST-O
           MOVE 0 TO TAX-O
           COMPUTE COST-PER-INSTRUMENT-O =
                        QUALITY-MODIFIER * INSTRUMENT-COST.
           COMPUTE SHIPPING-COST-O =
                        DELIVERY-MODIFIER * COST-PER-INSTRUMENT-O.
           COMPUTE TAX-O =
                        0.08 * COST-PER-INSTRUMENT-O.
       300-OPEN-FILES.
           OPEN INPUT RFPIN.
           OPEN OUTPUT RFPOUT.
       400-READ-RFPIN.
           DISPLAY 'READ RFPIN'.
           READ RFPIN
      * Set AT END Switch
           AT END
              MOVE 'Y' TO RFPIN-EOF
              DISPLAY 'END OF FILE'
           END-READ.
       500-WRITE-FAVOUT.
           DISPLAY 'WRITE FAVOUT'.
           WRITE RFPOUT-REC FROM PROP-REC.
       600-CLOSE-FILES.
           CLOSE RFPIN, RFPOUT.