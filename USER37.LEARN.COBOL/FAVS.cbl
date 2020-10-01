       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVS.
      ***** This is an unbelievably simple COBOL program
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  FAV-REC.
           05  ARTIST-NAME           PIC X(30).
           05  MUSICIANS             PIC 9(3)V99.
           05  MUSICAL-GENRE         PIC X(15).
           05  COST.
                10 CD-COST           PIC 9(3)V99.
                10 SHIPPING-COST     PIC 9(2)V99.
                10 TAX               PIC 9(2)V99.
                10 TOTAL-COST        PIC 9(3)V99 VALUE 0.
           05  BAND-TOGETHER         PIC X(1).
       PROCEDURE DIVISION.
           MOVE "ARCTIC MONKEYS"     TO ARTIST-NAME.
           MOVE 4                    TO MUSICIANS.
           MOVE "ROCK"               TO MUSICAL-GENRE.
           MOVE 10                   TO CD-COST.
           MOVE 5.99                 TO SHIPPING-COST.
           MOVE .21                  TO TAX.
           MOVE "Y"                  TO BAND-TOGETHER.
           COMPUTE TOTAL-COST =
                CD-COST + (TAX*CD-COST) + SHIPPING-COST.
           DISPLAY "Name: " ARTIST-NAME.
           DISPLAY "Number of musicians: " MUSICIANS.
           DISPLAY "Musical Genre: " MUSICAL-GENRE.
           DISPLAY "Gross Pay: " COST.
           DISPLAY "Band still together: " BAND-TOGETHER.
           GOBACK.
