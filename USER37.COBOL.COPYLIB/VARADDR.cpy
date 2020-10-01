           05  WS-SUPP-ADDRESS OCCURS 3 TIMES INDEXED BY WS-ADDR-IDX.
               10 WS-ADDRESS-TYPE      PIC X(01) VALUE SPACES.
                  88 WS-ORDER-ADDRESS           VALUE '1'.
                  88 WS-SCHED-ADDRESS           VALUE '2'.
                  88 WS-REMIT-ADDRESS           VALUE '3'.
               10 WS-ADDRESS-1         PIC X(15) VALUE SPACES.
               10 WS-ADDRESS-2         PIC X(15) VALUE SPACES.
               10 WS-ADDRESS-3         PIC X(15) VALUE SPACES.
               10 WS-CITY              PIC X(15) VALUE SPACES.
               10 WS-ADDR-STATE        PIC X(02) VALUE SPACES.
               10 WS-ZIP-CODE          PIC X(10) VALUE SPACES.
