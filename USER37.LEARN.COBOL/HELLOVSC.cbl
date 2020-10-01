       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLOVSC.
      * REMARKS. ThiS program Displays a number of literals
      *
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  SALUTATION     PIC X(30)
           VALUE ' < YOUR NAME > '.
       01  LINE2     PIC X(01) VALUE SPACE.
       01  LINE3     PIC X(30)
           VALUE 'WELCOME TO COBOL CLASS!'.
       01  LINE4     PIC X(60)
           VALUE 'WE HOPE THAT YOU HAVE A ROCKING TIME'.
       01  YOURS-TRULY     PIC X(30)
           VALUE 'Yours Truly,'.
       01  IBM-COBOL     PIC X(30)
           VALUE 'IBM COBOL'.
       01  flds.
           05 fld1    pic S9999 value -754.
           05 fld1-o  pic Z,ZZZ9.
           05 fld2      pic 9(4) value 0.
           05 fld2-o   pic $*,***.99.

       PROCEDURE DIVISION.
           MOVE 20034 TO FLD2.
           move fld1 to fld1-o.
           move fld2 to fld2-o.

      * DISPLAY in SYSOUT: Hello COBOL!
             DISPLAY 'HELLO ' SALUTATION.
             DISPLAY LINE2.
             DISPLAY LINE3.
             DISPLAY LINE4.
             DISPLAY YOURS-TRULY.
             DISPLAY IBM-COBOL.
      * This statement ends the program run
             GOBACK.

