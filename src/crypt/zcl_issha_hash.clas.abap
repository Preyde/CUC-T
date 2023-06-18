CLASS zcl_issha_hash DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS: constructor IMPORTING password TYPE string iter TYPE i DEFAULT 1024 salt TYPE xstring OPTIONAL,
      create_random_salt IMPORTING salt_len TYPE i DEFAULT 12,
      digest RETURNING VALUE(hash) TYPE string,
      get_salt returning value(salt) type xstring,
      get_hash_in_db_format returning value(hash) type string.

  PRIVATE SECTION.
    DATA: password_hex TYPE xstring,
          iter         TYPE i,
          salt_hex     TYPE string,
          msg_digest   TYPE REF TO cl_abap_message_digest,
          final_hash   type string.

ENDCLASS.



CLASS zcl_issha_hash IMPLEMENTATION.
  METHOD constructor.

    msg_digest = cl_abap_message_digest=>get_instance( ).
    password_hex = msg_digest->string_to_xstring( password ).
    me->iter = iter.
    salt_hex = salt.

  ENDMETHOD.

  METHOD create_random_salt.
    DATA salt TYPE string.

    CALL FUNCTION 'GENERAL_GET_RANDOM_PWD'
      EXPORTING
        number_chars = salt_len
      IMPORTING
        random_pwd   = salt.

    me->salt_hex = msg_digest->string_to_xstring( salt ).
  ENDMETHOD.

  METHOD digest.

    if salt_hex is initial.
    create_random_salt( ).
    endif.

    DATA(combined) = CONV xstring( password_hex && salt_hex ).

    msg_digest->digest( EXPORTING if_data        = combined
                        IMPORTING ef_hashxstring = DATA(result) ).

    DO iter - 1 TIMES.
      msg_digest = msg_digest->get_instance( ).

      msg_digest->digest( EXPORTING if_data        = CONV xstring( password_hex && result )
                          IMPORTING ef_hashxstring = result ).
    ENDDO.

    DATA base64_pw_and_salt TYPE string.

    CALL FUNCTION 'SCMS_BASE64_ENCODE_STR'
      EXPORTING
        input  = CONV xstring( result && salt_hex )
      IMPORTING
        output = base64_pw_and_salt.

    hash = final_hash = base64_pw_and_salt.

  ENDMETHOD.

  METHOD get_hash_in_db_format.
    hash = |\{x-issha, { iter }\}{ final_hash }|.
  ENDMETHOD.

  METHOD get_salt.
    salt = salt_hex.
  ENDMETHOD.

ENDCLASS.
