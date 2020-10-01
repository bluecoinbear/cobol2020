           05  WS-PURCHASE-ORDERS OCCURS 3 TIMES INDEXED BY WS-PO-IDX.
               10  WS-PO-NUMBER          PIC X(06) VALUE SPACES.
               10  WS-BUYER-CODE         PIC X(03) VALUE SPACES.
               10  WS-QUANTITY           PIC S9(7) VALUE ZERO.
               10  WS-UNIT-PRICE         PIC S9(7)V99 VALUE ZERO.
               10  WS-ORDER-DATE         PIC X(08) VALUE SPACES.
               10  WS-DELIVERY-DATE      PIC X(08) VALUE SPACES.
