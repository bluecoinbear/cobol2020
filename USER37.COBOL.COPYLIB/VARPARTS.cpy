           05  WS-PARTS.
               10  WS-PART-NUMBER       PIC X(23) VALUE SPACES.
               10  WS-PART-NAME         PIC X(14) VALUE SPACES.
               10  WS-SPEC-NUMBER       PIC X(07) VALUE SPACES.
               10  WS-GOVT-COMML-CODE   PIC X(01) VALUE SPACES.
               10  WS-BLUEPRINT-NUMBER  PIC X(10) VALUE SPACES.
               10  WS-UNIT-OF-MEASURE   PIC X(03) VALUE SPACES.
               10  WS-WEEKS-LEAD-TIME   PIC S9(3) VALUE ZEROS.
               10  WS-VEHICLE-MAKE      PIC X(03) VALUE SPACES.
                    88 WS-CHRYSLER       VALUE 'CHR'.
                    88 WS-FORD           VALUE 'FOR'.
                    88 WS-GM             VALUE 'GM '.
                    88 WS-VOLKSWAGON     VALUE 'VW '.
                    88 WS-TOYOTA         VALUE 'TOY'.
                    88 WS-JAGUAR         VALUE 'JAG'.
                    88 WS-PEUGEOT        VALUE 'PEU'.
                    88 WS-BMW            VALUE 'BMW'.
               10  WS-VEHICLE-MODEL     PIC X(10) VALUE SPACES.
               10  WS-VEHICLE-YEAR      PIC X(04) VALUE '0000'.
           10  FILLER                   PIC X(14) VALUE SPACES.
