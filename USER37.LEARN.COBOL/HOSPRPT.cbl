
       IDENTIFICATION DIVISION.
       PROGRAM-ID.  HOSPRPT.
       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE SECTION.
       FD  HOSPIN
           DATA RECORD IS HOSPIN-REC
       01  HOSPIN-REC                           PIC X(80).

       FD  HOSPOUT
           DATA RECORD IS HOSPOUT-REC
       01  HOSPOUT-REC                          PIC X(80).

       WORKING-STORAGE SECTION.

           COPY CLAIMREC.

       01  PROGRAM-SWITCHES
           05 REINSURANCE
           05 INSURED-SUB
           05 CLAIMFILE-EOF
                88 NO-MORE-CLAIMS   VALUE T.
           05 CLAIMFILE-ST
                88 CLAIMFILE-OK
           05 PRINTFILE-ST
                88 PRINTFILE-OK
           05 BENEFIT-PERIOD


       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-MAINLINE.
                   UNTIL NO-MORE-PATIENTS.
           PERFORM 999-CLEANUP THRU 999-EXIT.
           MOVE +0 TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
           DISPLAY "HOUSEKEEPING".
      *  DATE VALUES
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           MOVE WS-CURRENT-YEAR  TO HDR-YY.
           MOVE WS-CURRENT-MONTH  TO HDR-MM.
           MOVE WS-CURRENT-DAY  TO HDR-DD.
           PERFORM 800-OPEN-FILES.
           PERFORM 900-READ-WARD-DATA.

           IF NO-MORE-PATIENTS
               MOVE "EMPTY PATIENT INPUT FILE" TO ABEND-REASON
               GO TO 1000-ABEND-RTN.

      **** PUT IN TO HANDLE NEW SORT REQUIREMENTS

       100-MAINLINE.
           MOVE "100-MAINLINE" TO PARA-NAME.
           IF WARD-NBR IN INPATIENT-DAILY-REC NOT = HOLD-WARD-ID
               PERFORM 200-NEW-WARD THRU 200-EXIT
               PERFORM 300-NEW-ROOM THRU 300-EXIT
               PERFORM 400-NEW-PATIENT THRU 400-EXIT
               MOVE WARD-NBR IN INPATIENT-DAILY-REC TO HOLD-WARD-ID
               MOVE ROOM-IDENTITY IN INPATIENT-DAILY-REC
                                TO HOLD-ROOM-NBR
           ELSE
           IF ROOM-IDENTITY IN INPATIENT-DAILY-REC
                            NOT = HOLD-ROOM-NBR
               PERFORM 300-NEW-ROOM THRU 300-EXIT
               PERFORM 400-NEW-PATIENT THRU 400-EXIT
               MOVE ROOM-IDENTITY IN INPATIENT-DAILY-REC
                            TO HOLD-ROOM-NBR
           ELSE
               PERFORM 400-NEW-PATIENT THRU 400-EXIT.

           PERFORM 900-READ-WARD-DATA THRU 900-EXIT.

       200-NEW-WARD.
           MOVE "200-NEW-WARD" TO PARA-NAME.
           MOVE "N" TO ERROR-FOUND-SW IN FLAGS-AND-SWITCHES.

           MOVE WARD-NBR IN INPATIENT-DAILY-REC TO
              WARD-ID IN DCLWARD-CODES,
              WARD-ID IN DCLROOM-DATA.
      ***     WARD-ID IN DCLHOSP-BED. ??

           PERFORM 250-GET-WARD-DATA THRU 250-EXIT.
      *** SET UP PAGE HEADERS
           PERFORM 700-WRITE-PAGE-HDR    THRU 700-EXIT.
           PERFORM 720-WRITE-WARD-RPT    THRU 720-EXIT.

      ***PROCESS PATIENT TREATMENTS
       200-EXIT.
           EXIT.

       250-GET-WARD-DATA.
      *    MOVE SUPERVISOR-NURSE-ID TO SUPERVISE-NURSE-ID.
           EXEC SQL
             SELECT PRIMARY_PHYSICIAN_ID,
                    SUPERVISE_NURSE_ID,
                    LOCATION,
                    NUMBER_OF_BEDS,
                    BASE_ROOM_CHARGE
             INTO
                    :PRIMARY-PHYSICIAN-ID,
                    :SUPERVISE-NURSE-ID,
                    :LOCATION,
                    :DCLWARD-CODES.NUMBER-OF-BEDS,
                    :DCLWARD-CODES.BASE-ROOM-CHARGE
             FROM DDS0001.WARD_DATA
             WHERE WARD_ID = :DCLWARD-CODES.WARD-ID
           END-EXEC.

           IF SQLCODE = -811 OR 0
               NEXT SENTENCE
           ELSE
           IF SQLCODE = +100
               MOVE "*** PATIENT WARD DATA IN ERROR" TO
               ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-DAILY-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 250-EXIT
           ELSE
           IF SQLCODE < 0
               MOVE "*** FATAL DB2 ERROR" TO
               ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-DAILY-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 1000-DB2-ERROR-RTN.

           MOVE WARD-ID IN DCLWARD-CODES TO WARD-O.
           MOVE PRIMARY-PHYSICIAN-ID IN DCLWARD-CODES TO PHYS-O.
           MOVE SUPERVISE-NURSE-ID TO NURSE-O.
           MOVE NUMBER-OF-BEDS IN DCLWARD-CODES
                                    TO BEDS-O IN WS-WARD-RPT-REC.
           MOVE BASE-ROOM-CHARGE IN DCLWARD-CODES TO ROOM-CHARGE-O.

       250-EXIT.
           EXIT.

       300-NEW-ROOM.
           MOVE "300-NEW-ROOM" TO PARA-NAME.
           MOVE "N" TO ERROR-FOUND-SW IN FLAGS-AND-SWITCHES.

           MOVE ROOM-IDENTITY IN INPATIENT-DAILY-REC TO
              ROOM-IDB IN DCLHOSP-BED,
              ROOM-ID  IN DCLROOM-DATA.
      ***     WARD-ID IN DCLHOSP-BED. ??

           PERFORM 350-GET-ROOM-DATA THRU 350-EXIT.
      *** SET UP PAGE HEADERS
           PERFORM 740-WRITE-ROOM-RPT   THRU 740-EXIT.

       300-EXIT.
           EXIT.

       350-GET-ROOM-DATA.
      ****************
      *    MOVE SUPERVISOR-NURSE-ID TO SUPERVISE-NURSE-ID.
           EXEC SQL
             SELECT PRIVATE,
                    SEMI_PRIVATE,
                    NUMBER_OF_BEDS,
                    SPECIAL_EQUIPMENT
             INTO
                    :DCLROOM-DATA.PRIVATE,
                    :DCLROOM-DATA.SEMI-PRIVATE,
                    :DCLROOM-DATA.NUMBER-OF-BEDS,
                    :DCLROOM-DATA.SPECIAL-EQUIPMENT
             FROM DDS0001.ROOM_DATA
             WHERE WARD_ID = :DCLROOM-DATA.WARD-ID
             AND   ROOM_ID = :DCLROOM-DATA.ROOM-ID
           END-EXEC.

           IF SQLCODE =  0
               NEXT SENTENCE
           ELSE
           IF SQLCODE = +100
               MOVE "*** PATIENT ROOM DATA IN ERROR" TO
               ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-DAILY-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 350-EXIT
           ELSE
           IF SQLCODE < 0
               MOVE "*** FATAL DB2 ERROR" TO
               ERR-MSG IN INPATIENT-DAILY-REC-ERR IN PATERR
               MOVE "Y" TO ERROR-FOUND-SW
               MOVE SQLCODE TO  EXPECTED-VAL
               MOVE PATIENT-ID IN INPATIENT-DAILY-REC
                               TO ACTUAL-VAL
               WRITE SYSOUT-REC FROM ABEND-REC
               GO TO 1000-DB2-ERROR-RTN.

      * CUSTOM TAG
           MOVE ROOM-ID IN DCLROOM-DATA TO ROOM-O.
           IF PRIVATE IN DCLROOM-DATA = 1
              MOVE "PRIVATE" TO ROOM-TYPE
           ELSE
           IF SEMI-PRIVATE = 1
              MOVE "SEMI-PRIVATE" TO ROOM-TYPE
           ELSE
              MOVE "SPECIAL-NEEDS" TO ROOM-TYPE.

           MOVE WARD-ID IN DCLWARD-CODES TO PHYS-O.
           MOVE SUPERVISE-NURSE-ID TO NURSE-O.
           MOVE NUMBER-OF-BEDS IN DCLWARD-CODES
                                  TO BEDS-O IN WS-ROOM-RPT-REC.
           MOVE SPECIAL-EQUIPMENT IN DCLROOM-DATA TO SPECIAL-EQUIP-O.

       350-EXIT.
           EXIT.

       400-NEW-PATIENT.
      *************************
           MOVE "400-NEW-PATIENT" TO PARA-NAME.
           MOVE "N" TO ERROR-FOUND-SW IN FLAGS-AND-SWITCHES.

           MOVE PATIENT-ID IN INPATIENT-DAILY-REC TO
           PATMSTR-KEY, PATPERSN-KEY.

           PERFORM 450-GET-PATIENT-DATA THRU 450-EXIT.
      *** SET UP PAGE HEADERS
           PERFORM 760-WRITE-PATIENT-RPT THRU 760-EXIT.

       400-EXIT.
           EXIT.

       450-GET-PATIENT-DATA.
      *    MOVE SUPERVISOR-NURSE-ID TO SUPERVISE-NURSE-ID.

           MOVE DAILY-CHARGES-COMMENTS TO DAILY-COMMENTS-O.

           READ PATMSTR.
           IF PATMSTR-FOUND
              MOVE PATMSTR-REC TO PATIENT-MASTER-REC
              MOVE DATE-ADMIT TO ADMIT-DATE-O
              MOVE DIAGNOSTIC-CODE-PRIMARY TO DIAGNOSIS-O
              COMPUTE WS-NBR-DIAG-CODES = WS-NBR-DIAG-CODES + 1
              MOVE BED-IDENTITY-PRIMARY TO BED-O
           ELSE
              MOVE "PATIENT NOT FOUND IN PATMASTR" TO ABEND-REASON
              MOVE "500-GET-PATIENT-DATA" TO PARA-NAME
              MOVE PATIENT-ID IN INPATIENT-DAILY-REC TO  EXPECTED-VAL
              GO TO 1000-ABEND-RTN.

           READ PATPERSN.
           IF PATPERSN-FOUND
              MOVE PATPERSN-REC TO PATIENT-PERSONAL-MASTER-REC
              MOVE LAST-NAME TO LAST-NAME-O
              MOVE MIDINIT TO MIDINIT-O
              MOVE FIRST-NAME TO FIRST-NAME-O
           ELSE
              MOVE "PATIENT NOT FOUND IN PATPERSN" TO ABEND-REASON
              MOVE "500-GET-PATIENT-DATA" TO PARA-NAME
              MOVE PATIENT-ID IN INPATIENT-DAILY-REC TO  EXPECTED-VAL
              GO TO 1000-ABEND-RTN.

       450-EXIT.
           EXIT.


       700-WRITE-PAGE-HDR.
           MOVE "700-WRITE-PAGE-HDR" TO PARA-NAME.
           MOVE WS-PAGES TO PAGE-NBR-O.
           WRITE RPT-REC FROM WS-HDR-REC
               AFTER ADVANCING NEXT-PAGE.
           WRITE RPT-REC FROM WS-BLANK-LINE.
           ADD +1 TO WS-PAGES.
           MOVE +2 TO WS-LINES.
       700-EXIT.
           EXIT.

       720-WRITE-WARD-RPT.
           MOVE "720-WRITE-WARD-RPT" TO PARA-NAME.
           WRITE RPT-REC FROM WS-WARD-RPT-REC
               AFTER ADVANCING 2.
           WRITE RPT-REC FROM WS-BLANK-LINE.
           ADD +3 TO WS-LINES.
       720-EXIT.
           EXIT.

       740-WRITE-ROOM-RPT.
           MOVE "740-WRITE-ROOM-RPT" TO PARA-NAME.
           PERFORM 790-CHECK-PAGINATION THRU 790-EXIT.
           WRITE RPT-REC FROM WS-ROOM-RPT-REC
               AFTER ADVANCING 1.
           WRITE RPT-REC FROM WS-BLANK-LINE.
           ADD +2 TO WS-LINES.
       740-EXIT.
           EXIT.

       760-WRITE-PATIENT-RPT.
           MOVE "760-WRITE-PATIENT-RPT" TO PARA-NAME.
           WRITE RPT-REC FROM WS-BED-PATIENT-DETAIL
               AFTER ADVANCING 1.
           PERFORM 790-CHECK-PAGINATION THRU 790-EXIT.
           ADD +1 TO WS-LINES.
       760-EXIT.
           EXIT.

       790-CHECK-PAGINATION.
           MOVE "790-CHECK-PAGINATION" TO PARA-NAME.
           IF WS-LINES > 50
              WRITE RPT-REC FROM WS-BLANK-LINE
              WRITE RPT-REC FROM WS-BLANK-LINE
              PERFORM 700-WRITE-PAGE-HDR THRU 700-EXIT.
       790-EXIT.
           EXIT.

       795-WRITE-PATERR.
           MOVE "795-WRITE-PATERR" TO PARA-NAME.
           MOVE INPATIENT-DAILY-REC TO REST-OF-PAT-REC.
           WRITE INPATIENT-DAILY-REC-ERR.
           ADD +1 TO PAT-RECORDS-IN-ERROR.
       795-EXIT.
           EXIT.

       800-OPEN-FILES.
           MOVE "800-OPEN-FILES" TO PARA-NAME.
           OPEN INPUT PATSRCH, PATPERSN, PATMSTR.
           OPEN OUTPUT WARDFILE, PATERR, SYSOUT.
           DISPLAY PATMSTR-STATUS, PATPERSN-STATUS.
      *     GOBACK.
       800-EXIT.
           EXIT.

       850-CLOSE-FILES.
           MOVE "850-CLOSE-FILES" TO PARA-NAME.

           CLOSE PATSRCH, WARDFILE,
                 SYSOUT, PATPERSN,
                 PATMSTR.
           DISPLAY PATMSTR-STATUS, PATPERSN-STATUS.
      *     GOBACK.
       850-EXIT.
           EXIT.

       900-READ-WARD-DATA.
      *****************
      *  Remember to move "NO" to IFCODE if the input file is AT END
           READ PATSRCH INTO INPATIENT-DAILY-REC
               AT END MOVE "N" TO MORE-WARD-DATA-SW
               GO TO 900-EXIT
           END-READ.

           ADD +1 TO PAT-RECORDS-READ.
       900-EXIT.
           EXIT.

       999-CLEANUP.
           MOVE "999-CLEANUP" TO PARA-NAME.
      *  Final file-handling edits and trailer record handling
           IF TRLR-REC-FOUND
               NEXT SENTENCE
           ELSE
           IF NOT TRAILER-REC IN PATIENT-RECORD-TYPE
               MOVE "** INVALID FILE - NO TRAILER REC" TO ABEND-REASON
               GO TO 1000-ABEND-RTN.

           MOVE INPATIENT-DAILY-REC TO WS-TRAILER-REC.
           ADD +1 TO RECORDS-WRITTEN.
      *    IF RECORDS-READ NOT EQUAL TO IN-RECORD-COUNT
      *        MOVE "** INVALID FILE - # RECORDS OUT OF BALANCE"
      *                              TO ABEND-REASON
      *        MOVE RECORDS-READ     TO ACTUAL-VAL
      *        MOVE IN-RECORD-COUNT  TO EXPECTED-VAL
      *        GO TO 1000-ABEND-RTN.
      *
      *    MOVE "T" TO PATIENT-RECORD-TYPE.
      *    MOVE RECORDS-WRITTEN TO IN-RECORD-COUNT.
      *    MOVE WS-BASE-ROOM-CHARGE  TO IN-BASE-ROOM-CHARGE.
      *    MOVE WS-TOTAL-ROOM-CHARGE TO IN-TOTAL-ROOM-CHARGE.
      *    MOVE WS-EQUIPMENT-COST TO IN-EQUIPMENT-CHARGES.
      *    WRITE INPATIENT-DAILY-REC  FROM WS-TRAILER-REC.

      *  Code the statement to close all files
           PERFORM 850-CLOSE-FILES THRU 850-EXIT.

      *  Code the statement to Display a successful end-of-job msg
           DISPLAY "NORMAL END OF JOB".
       999-EXIT.
           EXIT.

       1000-ABEND-RTN.
           WRITE SYSOUT-REC FROM ABEND-REC.
           PERFORM 850-CLOSE-FILES THRU 850-EXIT.
           DISPLAY "*** ABNORMAL END OF JOB- DALYEDIT ***" UPON CONSOLE.
           DIVIDE ZERO-VAL INTO ONE-VAL.

       1000-DB2-ERROR-RTN.
      ************************************************************
      *       ERROR TRAPPING ROUTINE FOR INVALID SQLCODES        *
      ************************************************************
            DISPLAY '**** WE HAVE A SERIOUS PROBLEM HERE *****'.
            DISPLAY '999-ERROR-TRAP-RTN '.
            MULTIPLY SQLCODE BY -1 GIVING SQLCODE.
            DISPLAY 'SQLCODE ==> ' SQLCODE.
            DISPLAY SQLCA.
            DISPLAY SQLERRM.
            EXEC SQL WHENEVER SQLERROR CONTINUE END-EXEC.
            EXEC SQL ROLLBACK WORK END-EXEC.
            GO TO 1000-ABEND-RTN.