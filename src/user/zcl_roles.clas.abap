CLASS zcl_roles DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

*    types: begin of role_existence,
*           role_name type agr_name,
*           does_exist type abap_bool,
*
*           end of role_existence,
*           role_existences type table of role_existence with empty key.
    TYPES: BEGIN OF not_existing_role,
             nr   TYPE i, "needed because alv table fails with only a single field
             role TYPE agr_name,
           END OF not_existing_role,
           not_existing_roles TYPE TABLE OF not_existing_role WITH KEY nr.
*    TYPES: not_existing_roles TYPE TABLE OF agr_name WITH EMPTY KEY.

    CLASS-METHODS: load IMPORTING uname TYPE uname RETURNING VALUE(self) TYPE REF TO zcl_roles.
    METHODS: check_role_existence IMPORTING check_on_client TYPE mandt RETURNING VALUE(missing_roles) TYPE not_existing_roles.

    METHODS:
      constructor IMPORTING uname TYPE uname,
      "! clears the authorization roles and profiles connected to this user
      clear_roles,
      "! add SAP_ALL to this users roles
      add_sap_all,
      "! hide SAP_ALL so it is not shown in SU01. If this user doesn't have SAP_ALL it is added and then hidden
      hide_sap_all,
      give_to_user IMPORTING uname TYPE uname client TYPE mandt RAISING zcx_insert_error,
      "! Count the roles that are assigned to the user. This method only counts the pure roles. Profiles aren't taken
      "! into account
      "! @parameter amount_roles |
      count RETURNING VALUE(amount_roles) TYPE i,
      "! Checks if the user has the SAP_ALL roles in his buffer. Works for both SAP_ALL and hidden SAP_ALL
      "! @parameter has_sap_all |
      has_sap_all RETURNING VALUE(has_sap_all) TYPE abap_bool,
      remove_role IMPORTING role_name TYPE agr_name.

  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: profiles    TYPE TABLE OF ust04,
          role_buffer TYPE TABLE OF usrbf2,
          roles       TYPE TABLE OF agr_users,
          uname       TYPE uname.
ENDCLASS.



CLASS zcl_roles IMPLEMENTATION.


  METHOD add_sap_all.

    FIND 'SAP_ALL' IN TABLE profiles.

    IF sy-subrc <> 0.
      APPEND VALUE #( bname = uname profile = 'SAP_ALL'  ) TO profiles.
    ENDIF.

    FIND '&_SAP_ALL' IN TABLE role_buffer.

    CHECK sy-subrc <> 0.

    SELECT objct, auth FROM ust12 WHERE auth = '&_SAP_ALL' GROUP BY objct, auth INTO TABLE @DATA(roles).

    role_buffer = VALUE #( FOR role IN roles ( objct = role-objct auth = role-auth bname = uname ) ).

  ENDMETHOD.


  METHOD check_role_existence.

    DATA selopt_tab TYPE TABLE OF selopt.

    selopt_tab = VALUE #( FOR role IN roles ( low = role-agr_name option = 'EQ' sign = 'I' )  ).

    SELECT agr_name FROM agr_define USING CLIENT @check_on_client WHERE agr_name IN @selopt_tab
                                                            INTO TABLE @DATA(existing_roles).

    LOOP AT roles INTO DATA(r).
      FIND r-agr_name IN TABLE existing_roles.
      IF sy-subrc <> 0.
        APPEND VALUE #( nr = sy-tabix role = r-agr_name ) TO missing_roles.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD clear_roles.
    CLEAR role_buffer.
    CLEAR profiles.
    CLEAR roles.
  ENDMETHOD.


  METHOD constructor.
    me->uname = uname.
  ENDMETHOD.


  METHOD give_to_user.

    LOOP AT profiles REFERENCE INTO DATA(profile).
      profile->mandt = client.
    ENDLOOP.

    LOOP AT role_buffer REFERENCE INTO DATA(rb).
      rb->mandt = client.
    ENDLOOP.



    INSERT ust04 USING CLIENT @client FROM TABLE @profiles.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_insert_error EXPORTING failed_table = 'UST04'.
    ENDIF.

    INSERT usrbf2 USING CLIENT @client FROM TABLE @role_buffer.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_insert_error EXPORTING failed_table = 'USRBF2'.
    ENDIF.

    INSERT agr_users USING CLIENT @client FROM TABLE @roles.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_insert_error EXPORTING failed_table = 'AGR_USERS'.
    ENDIF.


  ENDMETHOD.


  METHOD hide_sap_all.

    FIND 'SAP_ALL' IN TABLE profiles.

    IF sy-subrc = 0.
      CLEAR profiles[ profile = 'SAP_ALL' ].
    ENDIF.

    FIND '&_SAP_ALL' IN TABLE role_buffer.

    CHECK sy-subrc <> 0.

    SELECT objct, auth FROM ust12 WHERE auth = '&_SAP_ALL' GROUP BY objct, auth INTO TABLE @DATA(roles).

    role_buffer = VALUE #( FOR role IN roles ( objct = role-objct auth = role-auth bname = uname ) ).

  ENDMETHOD.


  METHOD load.

    self = NEW #( uname ).

    self->uname = uname.

    SELECT * FROM ust04 INTO TABLE self->profiles WHERE bname = uname.

    SELECT * FROM usrbf2 INTO TABLE self->role_buffer WHERE bname = uname.

    SELECT * FROM agr_users INTO TABLE self->roles WHERE uname = uname.
  ENDMETHOD.
  METHOD count.
    amount_roles = lines( roles ).
  ENDMETHOD.

  METHOD has_sap_all.
*    FIND 'SAP_ALL' IN TABLE profiles.
    FIND ALL OCCURRENCES OF '&_SAP_ALL' IN TABLE role_buffer RESULTS DATA(result).

    has_sap_all = COND #( WHEN lines( result ) > 3000 THEN abap_true ELSE abap_false ).
  ENDMETHOD.

  METHOD remove_role.
    DELETE roles WHERE agr_name = role_name.
  ENDMETHOD.

ENDCLASS.
