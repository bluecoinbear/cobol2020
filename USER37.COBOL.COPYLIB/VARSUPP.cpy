           05 WS-SUPPLIERS.
               10  WS-SUPPLIER-CODE     PIC X(10) VALUE SPACES.
               10  WS-SUPPLIER-TYPE     PIC X(01) VALUE SPACES.
                    88 WS-SUBCONTRACTOR  VALUE 'S'.
                    88 WS-DISTRIBUTOR    VALUE 'D'.
                    88 WS-MANUFACTURER   VALUE 'M'.
                    88 WS-IMPORTER       VALUE 'I'.
               10  WS-SUPPLIER-NAME     PIC X(15) VALUE SPACES.
               10  WS-SUPPLIER-PERF     PIC 9(03) VALUE ZERO.
               10  WS-SUPPLIER-RATING   PIC X(01) VALUE SPACES.
                    88 WS-HIGHEST-QUALITY VALUE '3'.
                    88 WS-AVERAGE-QUALITY VALUE '2'.
                    88 WS-LOWEST-QUALITY  VALUE '1'.
               10  WS-SUPPLIER-STATUS   PIC X(01) VALUE SPACES.
                    88 WS-GOVT-COMM       VALUE '1'.
                    88 WS-GOVT-ONLY       VALUE '2'.
                    88 WS-COMMERCIAL-ONLY VALUE '3'.
               10  WS-SUPPLIER-ACT-DATE PIC X(08) VALUE SPACES.
