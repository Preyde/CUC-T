CLASS zcx_insert_error DEFINITION
  PUBLIC
  INHERITING FROM cx_static_check
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_t100_dyn_msg .
    INTERFACES if_t100_message .

    METHODS: constructor
      IMPORTING
        failed_table type string
        !textid   LIKE if_t100_message=>t100key OPTIONAL
        !previous LIKE previous OPTIONAL,

        get_failed_table returning value(tabname) type string.
  PROTECTED SECTION.
  PRIVATE SECTION.

  data: failed_table type string.
ENDCLASS.



CLASS zcx_insert_error IMPLEMENTATION.


  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    CALL METHOD super->constructor
      EXPORTING
        previous = previous.
    CLEAR me->textid.
    IF textid IS INITIAL.
      if_t100_message~t100key = if_t100_message=>default_textid.
    ELSE.
      if_t100_message~t100key = textid.
    ENDIF.

    me->failed_table = failed_table.
  ENDMETHOD.
  METHOD get_failed_table.
    tabname = failed_table.
  ENDMETHOD.

ENDCLASS.
