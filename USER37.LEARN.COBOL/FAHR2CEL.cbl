       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAHR2CEL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  TEMPS-VARS.
           05 WS-FAHRENHEIT     PIC 9(3)V99.
           05 WS-CELSIUS        PIC 9(4)V99.
           05 WS-CELSIUS-OUT    PIC 9(3)V99.
       PROCEDURE DIVISION.
           MOVE 60 TO WS-FAHRENHEIT.
           COMPUTE WS-CELSIUS ROUNDED =
                  ( (WS-FAHRENHEIT - 32) * 5 ) / 9.
           DISPLAY "Fahrenheit: " WS-FAHRENHEIT.
           MOVE WS-CELSIUS TO WS-CELSIUS-OUT.
           DISPLAY "Celsius: " WS-CELSIUS-OUT.
           GOBACK.

