*&---------------------------------------------------------------------*
*& Report z_cuc_t_packed
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_cuc_t_packed.

CLASS zcl_issha_hash DEFINITION

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

CLASS zcl_roles DEFINITION

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

CLASS zcl_user_data DEFINITION

  FINAL
   .

  PUBLIC SECTION.


    TYPES: BEGIN OF create_change_details,
             uname     TYPE uname,
             full_name TYPE string,
             date      TYPE d,
             time      TYPE t,
           END OF create_change_details.



    CLASS-METHODS: load IMPORTING uname TYPE uname client TYPE mandt RETURNING VALUE(self) TYPE REF TO zcl_user_data,
      new IMPORTING uname         TYPE uname
                    client        TYPE mandt
                    password      TYPE char40
                    first_name    TYPE char40 OPTIONAL
                    last_name     TYPE char40
                    creator       TYPE uname
                    creation_date TYPE d OPTIONAL
                    creation_time TYPE t OPTIONAL
          RETURNING VALUE(self)   TYPE REF TO zcl_user_data,
      exists IMPORTING uname TYPE uname client TYPE mandt RETURNING VALUE(exists) TYPE abap_bool,
      get_defaul_address_number IMPORTING client TYPE mandt RETURNING VALUE(addrno) TYPE ad_addrnum,
      get_creation_details IMPORTING client                  TYPE mandt
                                     uname                   TYPE uname
                           RETURNING VALUE(creation_details) TYPE create_change_details,
      get_fullname IMPORTING uname TYPE uname RETURNING VALUE(fullname) TYPE string,
      "! Get the last person that changed the user in SU01
      "! @parameter client |
      "! @parameter uname |
      "! @parameter change_details |
      get_change_details IMPORTING client                TYPE mandt
                                   uname                 TYPE uname
                         RETURNING VALUE(change_details) TYPE create_change_details,
      get_best_possible_changer IMPORTING client TYPE mandt RETURNING VALUE(change_details) TYPE create_change_details.

    METHODS: write_to_client IMPORTING client TYPE mandt RAISING zcx_insert_error,
      set_last_change IMPORTING changer TYPE uname date TYPE d time TYPE t,
      get_roles RETURNING VALUE(roles) TYPE REF TO zcl_roles.


  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA: user_address_key_mapping TYPE usr21,
          persnumber               TYPE ad_persnum,
          addressnumber            TYPE ad_addrnum,
          person_reference         TYPE adrvp,
          client                   TYPE mandt,
          adrp                     TYPE adrp,
          adrp_uuid_index          TYPE adrp_uuid_index,
          user_address_mapping     TYPE adcp,
          adcp_uuid_index          TYPE adcp_uuid_index,
          uname                    TYPE uname,
          password_details         TYPE usr02,
          docu                     TYPE usdocu,
          runtime_data             TYPE usr01,
          usr04                    TYPE usr04,
          parameters               TYPE usr05,
          roles                    TYPE REF TO zcl_roles,
          last_change              TYPE usrstamp.


    METHODS:

      "! The dbtab SOUD is referenced in ADRVP with a string containing the key values of SOUD. This method
      "! creates this reference
      set_user_def_key_in_reference,
      set_fkeys_for_person_ref,
      persnumber_exists_for_address IMPORTING client TYPE mandt RETURNING VALUE(exists) TYPE abap_bool,
      create_new_persnumber IMPORTING client TYPE mandt RETURNING VALUE(persnumber) TYPE ad_persnum,
      copy_table_by_bname IMPORTING tabname TYPE string bname TYPE uname client TYPE mandt,
      check_insert_successful IMPORTING tabname TYPE string RAISING zcx_insert_error.

    CLASS-METHODS:
      time_to_int IMPORTING VALUE(time) TYPE t RETURNING VALUE(int) TYPE i,
      int_to_time IMPORTING VALUE(int) TYPE i RETURNING VALUE(time) TYPE t.

ENDCLASS.



CLASS zcl_user_data IMPLEMENTATION.



  METHOD copy_table_by_bname.
    DATA ref_tab TYPE REF TO data.

    CREATE DATA ref_tab TYPE TABLE OF (tabname).

    FIELD-SYMBOLS <tab> TYPE ANY TABLE.

    ASSIGN ref_tab->* TO <tab>.

    DATA(usr_where_clause) = |BNAME = '{ uname }'|.

    SELECT * FROM (tabname) INTO TABLE <tab> WHERE (usr_where_clause).

    INSERT (tabname) USING CLIENT @client FROM TABLE @<tab>.
  ENDMETHOD.


  METHOD create_new_persnumber.
    SELECT MAX( persnumber ) FROM usr21 USING CLIENT @client INTO @persnumber.
    ADD 1 TO persnumber.
    UNPACK persnumber TO persnumber.
  ENDMETHOD.


  METHOD exists.
    SELECT SINGLE bname FROM usr01 USING CLIENT @client WHERE bname = @uname INTO @DATA(_no_use).

    exists = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).
  ENDMETHOD.


  METHOD get_best_possible_changer.
    "get the person that changed the most users
    SELECT modifier, COUNT( * ) AS amount FROM usrstamp USING CLIENT @client GROUP BY modifier INTO TABLE @DATA(modifiers).

    SORT modifiers BY amount DESCENDING.

    change_details-uname = modifiers[ 1 ]-modifier.
    "first try to find a day when many users where changed
    SELECT moddate, COUNT( * ) AS amount FROM usrstamp USING CLIENT @client INTO TABLE @DATA(changes)
                                                               WHERE modifier = @change_details-uname
                                                               GROUP BY moddate.

    SORT changes BY amount DESCENDING.


    change_details-date = changes[ 1 ]-moddate.


    "now calculating the average time of all changes that happened this day
    SELECT modtime FROM usrstamp USING CLIENT @client WHERE modifier = @change_details-uname AND
                                                            moddate = @change_details-date
                                                            INTO TABLE @DATA(times).

    DATA(times_as_int) = VALUE int_tab1( FOR time IN times ( time_to_int( time-modtime ) ) ).

    DATA(amount_times) = lines( times ).

    DATA(sum_of_all_times) = REDUCE i( INIT sum = 0 FOR t IN times_as_int NEXT sum = sum + t ).

    DATA(average_time) = int_to_time( sum_of_all_times / amount_times ).

    change_details-time = average_time.

    SELECT SINGLE persnumber FROM usr21 INTO @DATA(persnumber) WHERE bname = @change_details-uname.

    SELECT SINGLE name_text FROM adrp INTO change_details-full_name WHERE persnumber = persnumber.


  ENDMETHOD.


  METHOD get_change_details.
*  select single chonam as uname, chdat as date, chtim as time from soud into CORRESPONDING FIELDS OF @change_details
*                                                              where sapnam = @uname.
*
    SELECT SINGLE modifier AS uname, moddate AS date, modtime AS time FROM usrstamp
                                                                      INTO CORRESPONDING FIELDS OF @change_details
                                                                      WHERE username = @uname.
*  change_details = value #( uname = last_change-modifier date = moddate time = modtime ).

    SELECT SINGLE persnumber FROM usr21 INTO @DATA(persnumber) WHERE bname = @change_details-uname.
*
    SELECT SINGLE name_text FROM adrp INTO change_details-full_name WHERE persnumber = persnumber.
  ENDMETHOD.


  METHOD get_creation_details.

*    SELECT SINGLE cronam AS uname, crdat AS date, crtim AS time FROM soud INTO CORRESPONDING FIELDS OF @creation_details
*                                                                WHERE sapnam = @uname.
*
*    SELECT SINGLE persnumber FROM usr21 INTO @DATA(persnumber) WHERE bname = @creation_details-uname.
*
*    SELECT SINGLE name_text FROM adrp INTO creation_details-full_name WHERE persnumber = persnumber.
  ENDMETHOD.


  METHOD get_defaul_address_number.
    SELECT SINGLE addrnumber FROM usaddef USING CLIENT @client INTO @addrno.
  ENDMETHOD.


  METHOD get_fullname.
    SELECT SINGLE persnumber FROM usr21 INTO @DATA(persnumber) WHERE bname = @uname.

    SELECT SINGLE name_text FROM adrp INTO fullname WHERE persnumber = persnumber.
  ENDMETHOD.


  METHOD get_roles.
    roles = me->roles.
  ENDMETHOD.


  METHOD int_to_time.
    DATA: h TYPE char2,
          m TYPE char2,
          s TYPE char2.

    DATA(int_c) = CONV char6( int ).

    h = int_c(2).
    m = int_c+2(2) / 100 * 60.
    s = int_c+4(2) / 100 * 60.

    UNPACK h TO h.
    UNPACK m TO m.
    UNPACK s TO s.

    time = h && m && s.
  ENDMETHOD.


  METHOD load.

    self = NEW #( ).

    self->uname = uname.

    SELECT SINGLE * FROM usr21 USING CLIENT @client WHERE bname = @uname
                                                    INTO @self->user_address_key_mapping.

    self->persnumber = self->user_address_key_mapping-persnumber.
    self->addressnumber = self->user_address_key_mapping-addrnumber.

    SELECT SINGLE * FROM adcp USING CLIENT @client WHERE persnumber = @self->persnumber AND
                                                         addrnumber = @self->addressnumber
                                                   INTO @self->user_address_mapping.

    SELECT SINGLE * FROM adcp_uuid_index USING CLIENT @client WHERE adcp_uuid = @self->user_address_mapping-adcp_uuid
                                                              INTO @self->adcp_uuid_index.

    SELECT SINGLE * FROM adrvp USING CLIENT @client WHERE persnumber = @self->persnumber AND appl_table = 'USR21'
                                             INTO @self->person_reference.

    SELECT SINGLE * FROM adrp USING CLIENT @client WHERE persnumber = @self->persnumber INTO @self->adrp.
    SELECT SINGLE * FROM adrp_uuid_index USING CLIENT @client WHERE adrp_uuid = @self->adrp-adrp_uuid INTO @self->adrp_uuid_index.

*    SELECT SINGLE * FROM soud USING CLIENT @client INTO @DATA(user_definition) WHERE sapnam = @uname.

*    SELECT SINGLE * FROM souc USING CLIENT @client INTO @self->souc WHERE sapnam = @uname.

    SELECT SINGLE * FROM usrstamp USING CLIENT @client INTO @self->last_change WHERE username = @uname.

    SELECT SINGLE * FROM usr02 USING CLIENT @client INTO @self->password_details WHERE bname = @uname.

    SELECT SINGLE * FROM usdocu USING CLIENT @client INTO @self->docu WHERE bname = @uname.

    SELECT SINGLE * FROM usr01 USING CLIENT @client INTO @self->runtime_data WHERE bname = @uname.

    SELECT SINGLE * FROM usr04 USING CLIENT @client INTO @self->usr04 WHERE bname = @uname.
    SELECT SINGLE * FROM usr05 USING CLIENT @client INTO @self->parameters WHERE bname = @uname.

    self->roles = zcl_roles=>load( uname = uname ).

*    SELECT * FROM ust04 INTO TABLE self->profiles WHERE bname = uname.
*
*    SELECT * FROM usrbf2 INTO TABLE self->role_buffer WHERE bname = uname.
*
*    select * from agr_users into table self->roles where uname = uname.

  ENDMETHOD.


  METHOD new.

    self = NEW #( ).
    self->client = client.
    self->uname = uname.
    "25054

    DATA(hash) = NEW zcl_issha_hash( password = CONV #( password ) ).
    hash->digest( ).

    self->persnumber = self->create_new_persnumber( client ).
    self->addressnumber = zcl_user_data=>get_defaul_address_number( client = client ).

    DATA(identity_guid) = cl_system_uuid=>create_uuid_c32_static( ).
    DATA(adcp_uuid) = cl_system_uuid=>create_uuid_c32_static( ).
    DATA(adrp_uuid) = cl_system_uuid=>create_uuid_c32_static( ).

    self->password_details = VALUE usr02( mandt = client
                                          bname = uname
                                          erdat = creation_date
                                          aname = creator
                                          codvn = 'H'
                                          ustyp = 'A'
                                          tzone = sy-tzone
                                          pwdchgdate = creation_date
                                          pwdsetdate = creation_date
                                          pwdhistory = 1
                                          pwdinitial = 1
                                          pwdsaltedhash = hash->get_hash_in_db_format( ) ).

    self->user_address_key_mapping = VALUE #( bname = uname
                                              persnumber = self->persnumber
                                              addrnumber = self->addressnumber
                                              identity_guid = identity_guid ).

    self->user_address_mapping = VALUE #( addrnumber = self->addressnumber
                                          persnumber = self->persnumber
                                          date_from = '00010101'
                                          date_to = '99991231'
                                          comp_pers = 'C'
                                          so_key = '25055' "c
                                          adcp_uuid = adcp_uuid
                                          client = client ).

    self->adcp_uuid_index = VALUE #( adcp_uuid = adcp_uuid
                                     client = client
                                     addrnumber = self->addressnumber
                                     persnumber = self->persnumber
                                     comp_pers = 'C' ).

    self->adrp = VALUE #( persnumber = self->persnumber
                          date_from = '00010101'
                          date_to = '99991231'
                          name_first = first_name
                          name_last = last_name
                          name_text = |{ first_name } { last_name }|
                          pers_group = 'BC01'
                          mc_namefir = to_upper( first_name )
                          mc_namelas = to_upper( last_name )
                          langu_crea = sy-langu
                          adrp_uuid = adrp_uuid
                          client = client ).

    self->adrp_uuid_index = VALUE #( persnumber = self->persnumber
                                     client = client
                                     adrp_uuid = adrp_uuid ).

    self->person_reference = VALUE #( persnumber = self->persnumber
                                        consnumber = 1
                                        appl_table = 'USR21'
                                        appl_field = 'PERSNUMBER'
                                        appl_key = client && uname
                                        pers_group = 'BC01'
                                        addrnumber = self->addressnumber
                                        owner = abap_true
                                        client = client ).

    self->last_change = VALUE #( mandt = client
                                 username = uname
                                 struct = '*'
                                 field = '*'
                                 moddate = sy-datum
                                 modtime = sy-uzeit
                                 modifier = sy-uname ).

    self->runtime_data = VALUE #( bname = uname
                                  spdb = 'H'
                                  spda = 'K'
                                  datfm = 1
                                  timefm = 0
                                  mandt = client ).

    self->usr04 = VALUE #( bname = uname
                           mandt = client
                           modda = sy-datum
                           modti = sy-uzeit
                           modbe = sy-uname
                           nrpro = 0 ).

    self->roles = NEW zcl_roles( uname ).

    self->docu = VALUE #( bname = uname mandt = client modbe = '' modda = '' modti = '' docu = '' ).

*    self->souc = value #( sapnam = uname
*                          usrnam = uname
*                          usrtp = 'USR' ).






  ENDMETHOD.


  METHOD persnumber_exists_for_address.
    SELECT SINGLE persnumber FROM usr21 USING CLIENT @client WHERE addrnumber = @addressnumber AND
                                                                   persnumber = @persnumber
                                                                   INTO @DATA(_persnumber).

    exists = COND #( WHEN sy-subrc = 0 THEN abap_true ELSE abap_false ).

  ENDMETHOD.


  METHOD set_fkeys_for_person_ref.

  ENDMETHOD.


  METHOD set_last_change.
    last_change = VALUE #( username = uname
                           modifier = changer
                           struct = '*'
                           field = '*'
                           moddate = date
                           modtime = time ).
  ENDMETHOD.


  METHOD set_user_def_key_in_reference.
*    person_reference[ appl_table = 'SOUD' ]-appl_key = client &&
*                                                       user_definition-usrtp &&
*                                                       user_definition-usryr &&
*                                                       user_definition-usrno.
  ENDMETHOD.


  METHOD time_to_int.

    DATA: h TYPE char2,
          m TYPE char2,
          s TYPE char2.

    h = time(2).
    m = time+2(2) / 60 * 100.
    s = time+4(2) / 60 * 100.

    UNPACK h TO h.
    UNPACK m TO m.
    UNPACK s TO s.

    int = h && m && s.

  ENDMETHOD.


  METHOD write_to_client.

    DATA(default_addrnumber) = get_defaul_address_number( client ).
    me->addressnumber = default_addrnumber.
    user_address_key_mapping-addrnumber = default_addrnumber.
    user_address_mapping-addrnumber = default_addrnumber.

    person_reference-addrnumber = default_addrnumber.
    person_reference-appl_key = client && uname.


    adcp_uuid_index-addrnumber = default_addrnumber.

    IF persnumber_exists_for_address( client ).
      persnumber = create_new_persnumber( client ).
    ENDIF.

*    select single adrp_uuid from adrp_uuid_index where adrp_uuid = @adrp_uuid_index-adrp_uuid into @data(_adrp_uuid).

    IF sy-subrc = 0.

    ENDIF.


    person_reference-persnumber = persnumber.
    user_address_mapping-persnumber = persnumber.
    user_address_key_mapping-persnumber = persnumber.
    adrp-persnumber = persnumber.
    adrp_uuid_index-persnumber = persnumber.
    adcp_uuid_index-persnumber = persnumber.

    last_change-mandt = client.


*    insert( tabname = 'adrp' client = client struc = adrp ).
*    insert( tabname = 'adrp_uuid_index' client = client struc = adrp ).
*    insert( tabname = 'usr21' client = client struc = adrp ).
*    insert( tabname = 'ADRP' client = client struc = adrp ).
*    insert( tabname = 'ADRP' client = client struc = adrp ).
*    insert( tabname = 'ADRP' client = client struc = adrp ).

    INSERT INTO usr21 USING CLIENT @client VALUES @user_address_key_mapping.
    check_insert_successful( tabname = 'usr21' ).
    INSERT INTO adrp USING CLIENT @client VALUES @adrp.
    check_insert_successful( tabname = 'adrp' ).
    INSERT INTO adrp_uuid_index USING CLIENT @client VALUES @adrp_uuid_index.
    check_insert_successful( tabname = 'adrp_uuid_index' ).
    INSERT INTO adcp_uuid_index USING CLIENT @client VALUES @adcp_uuid_index.
    check_insert_successful( tabname = 'adcp_uuid_index' ).
    INSERT INTO adcp USING CLIENT @client VALUES @user_address_mapping.
    check_insert_successful( tabname = 'user_address_mapping' ).
    INSERT INTO usrstamp USING CLIENT @client VALUES @last_change.
    check_insert_successful( tabname = 'usrstamp' ).
    INSERT INTO adrvp USING CLIENT @client VALUES @person_reference.
    check_insert_successful( tabname = 'adrvp' ).
    INSERT INTO usr02 USING CLIENT @client VALUES @password_details.
    check_insert_successful( tabname = 'usr02' ).
    INSERT INTO usr01 USING CLIENT @client VALUES @runtime_data.
    check_insert_successful( tabname = 'usr01' ).
    INSERT INTO usr04 USING CLIENT @client VALUES @usr04.
    check_insert_successful( tabname = 'usr04' ).

    IF parameters IS NOT INITIAL.

      INSERT INTO usr05 USING CLIENT @client VALUES @parameters.

      check_insert_successful( tabname = 'usr05' ).
    ENDIF.
    roles->give_to_user( uname = uname client = client ).

    IF docu IS NOT INITIAL.
      INSERT INTO usdocu USING CLIENT @client VALUES @docu.
      check_insert_successful( tabname = 'usdocu' ).
    ENDIF.



  ENDMETHOD.

  METHOD check_insert_successful.
    CHECK sy-subrc <> 0.
    DATA(name) = to_upper( tabname ).
    RAISE EXCEPTION TYPE zcx_insert_error EXPORTING failed_table = name.
  ENDMETHOD.

ENDCLASS.

SELECTION-SCREEN BEGIN OF BLOCK client_block WITH FRAME TITLE clt_titl.
PARAMETERS client TYPE mandt OBLIGATORY.
SELECTION-SCREEN COMMENT 40(20) clnt_txt.
SELECTION-SCREEN END OF BLOCK client_block.
"block to choose between copying an old user or creating a new one
SELECTION-SCREEN BEGIN OF BLOCK user_block WITH FRAME TITLE usr_titl.
PARAMETERS: uname    TYPE uname OBLIGATORY DEFAULT sy-uname MATCHCODE OBJECT o2username,

            copy_usr RADIOBUTTON GROUP usr USER-COMMAND copy_new_rbtn_click,
            new_usr  RADIOBUTTON GROUP usr.

SELECTION-SCREEN BEGIN OF BLOCK new_user_sub_block WITH FRAME TITLE snu_titl.
PARAMETERS: f_name TYPE char40 LOWER CASE MODIF ID new,
            l_name TYPE char40 LOWER CASE MODIF ID new,
            pw     TYPE char40 LOWER CASE MODIF ID new DEFAULT 'Start123!'.

SELECTION-SCREEN END OF BLOCK new_user_sub_block.

SELECTION-SCREEN END OF BLOCK user_block.
"block for data to the changer that is later shown in SU01

SELECTION-SCREEN BEGIN OF BLOCK role_block WITH FRAME TITLE rol_titl.
PARAMETERS: old_role AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN PUSHBUTTON 35(11) ro_check USER-COMMAND role_check.
SELECTION-SCREEN COMMENT 46(20) mr_text.
PARAMETERS: old_only RADIOBUTTON GROUP role.
PARAMETERS sap_all RADIOBUTTON GROUP role.

PARAMETERS            hid_sa  RADIOBUTTON GROUP role.


SELECTION-SCREEN END OF BLOCK role_block.

SELECTION-SCREEN BEGIN OF BLOCK changer_block WITH FRAME TITLE cng_titl.
PARAMETERS changer  TYPE uname.
SELECTION-SCREEN COMMENT 47(29) cng_name.
SELECTION-SCREEN COMMENT 77(35) cng_stat. "best possible changer comment
PARAMETERS: cng_date TYPE d,
            cng_time TYPE t.
SELECTION-SCREEN PUSHBUTTON 47(21) b_best USER-COMMAND best_changer_click.
SELECTION-SCREEN END OF BLOCK changer_block.


DATA: best_possible_changer        TYPE uname,
      last_changer_of_user_to_copy TYPE uname,
      cached_best_possible_change  TYPE zcl_user_data=>create_change_details,
      best_possible_changer_text   TYPE char35,
      changer_does_not_exist_text  TYPE char35,
      last_missing_roles_uname     TYPE uname,
      missing_roles                TYPE zcl_roles=>not_existing_roles,
      use_best_possible_changer    TYPE abap_bool,
      last_client type mandt,
      bpc_btn_clicked              TYPE abap_bool.

INITIALIZATION.
  %_uname_%_app_%-text = 'username'.
  %_client_%_app_%-text = 'client'.
  %_copy_usr_%_app_%-text = 'copy from existing user'.
  %_new_usr_%_app_%-text = 'create a new user'.

  %_old_role_%_app_%-text = 'copy roles of user'.
  %_sap_all_%_app_%-text = 'give user SAP_ALL'.
  %_hid_sa_%_app_%-text = 'give user hidden SAP_ALL'.
  %_old_only_%_app_%-text = 'copy roles only'.
  %_changer_%_app_%-text = 'last changer of the user'.
  %_cng_date_%_app_%-text = 'date of last change'.
  %_cng_time_%_app_%-text = 'time of last change'.

  b_best = 'best possible changer'.

  snu_titl = 'user data'.
  %_f_name_%_app_%-text = 'first name'.
  %_l_name_%_app_%-text = 'last name (recommended)'.
  %_pw_%_app_%-text = 'password'.

  ro_check = 'check roles'.
  clt_titl = 'client'.
  usr_titl = 'copying the user or creating a new one'.
  rol_titl = 'roles'.

  cng_titl = 'the last changer of the user'.

  WRITE icon_okay AS ICON TO best_possible_changer_text.

  best_possible_changer_text = best_possible_changer_text && ' best possible changer'.

  WRITE icon_incomplete AS ICON TO changer_does_not_exist_text.



  changer_does_not_exist_text = changer_does_not_exist_text && ' this user does not exist'.

  LOOP AT SCREEN.
    IF screen-group1 = 'SSA' OR screen-group1 = 'SHS' OR screen-group1 = 'NEW'.
      screen-invisible = '1' .

      MODIFY SCREEN.
    ENDIF.

  ENDLOOP.

  SELECT mandt, mtext FROM t000 INTO TABLE @DATA(clients_to_choose).

  "Get the first best client to copy to. 000 and 001 are reserved SAP clients so it is probably not in interest to copy to.
  LOOP AT clients_to_choose INTO DATA(c) WHERE mandt <> '000' AND mandt <> '001' AND mandt <> sy-mandt.
    client = c-mandt.
    clnt_txt = c-mtext.
  ENDLOOP.


  PERFORM set_last_change USING new_usr uname client.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR client.



  DATA return_tab TYPE TABLE OF ddshretval.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'MANDT'
      value_org  = 'S'
    TABLES
      value_tab  = clients_to_choose
      return_tab = return_tab.

  CHECK return_tab IS NOT INITIAL.

  DATA(chosen_client) = clients_to_choose[ mandt = return_tab[ 1 ]-fieldval ].

  client = chosen_client-mandt.

  CALL FUNCTION 'SET_DYNP_VALUE'
    EXPORTING
      i_field = 'CLNT_TXT'
      i_repid = sy-repid
      i_dynnr = sy-dynnr
      i_value = CONV char255( chosen_client-mtext ).

AT SELECTION-SCREEN OUTPUT.

  IF bpc_btn_clicked = abap_true.
    PERFORM set_last_change USING abap_true uname client.
    bpc_btn_clicked = abap_false.
  ELSE.
    PERFORM set_last_change USING new_usr uname client.
  ENDIF.

  LOOP AT SCREEN.

    IF new_usr = 'X' AND old_only = 'X'.
      old_only = ''.
      sap_all = 'X'.
    ENDIF.

    IF ( best_possible_changer <> changer or client <> last_client ) AND screen-name = 'B_BEST'.
      screen-input = '1'.

    ELSEIF best_possible_changer = changer AND screen-name = 'B_BEST'.
      screen-input = '0'.

    ENDIF.

    IF screen-name = 'OLD_ROLE' OR screen-name = 'OLD_ONLY'.
      screen-input = COND #( WHEN new_usr = 'X' THEN '0' ELSE '1' ).
    ENDIF.


    IF screen-group1 = 'SSA' .
      screen-invisible = COND #( WHEN sap_all = 'X' THEN '0' ELSE '1' ) .
    ENDIF.

    IF screen-group1 = 'SHS' .
      screen-invisible = COND #( WHEN hid_sa = 'X' THEN '0' ELSE '1' ) .
    ENDIF.

    IF screen-group1 = 'NEW' .
      screen-invisible = COND #( WHEN new_usr = 'X' THEN '0' ELSE '1' ) .
      screen-active = COND #( WHEN new_usr = '' THEN '0' ELSE '1' ) .
    ENDIF.




    IF screen-name = 'RO_CHECK'.
      screen-input = COND #( WHEN new_usr = 'X' THEN '0' ELSE '1' ).
    ENDIF.

    MODIFY SCREEN.

  ENDLOOP.




AT SELECTION-SCREEN.

  IF sy-ucomm = 'ROLE_CHECK'.

    PERFORM show_missing_roles.

  ENDIF.

  IF sy-ucomm = to_upper( 'copy_new_rbtn_click' ).

    IF new_usr = 'X'.
      old_role = ''.
    ENDIF.

  ENDIF.

  IF sy-ucomm = 'BEST_CHANGER_CLICK'.
    bpc_btn_clicked = abap_true.

  ENDIF.



  IF client IS NOT INITIAL.
    UNPACK client TO client.
    IF line_exists( clients_to_choose[ mandt = client ] ).

      clnt_txt =  clients_to_choose[ mandt = client ]-mtext.

    ELSE.
      clnt_txt = ''.
      MESSAGE |client { client } does not exist on this system| TYPE 'W' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDIF.


START-OF-SELECTION.

  DATA(subrc) = 0.

  IF client = sy-mandt.
    MESSAGE |Can't copy to same client| TYPE 'I' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  IF copy_usr = 'X'.
    IF NOT zcl_user_data=>exists( uname = uname client = sy-mandt ).
      MESSAGE |This user doesn't exist on this client| TYPE 'I' DISPLAY LIKE 'E'.
      RETURN.
    ENDIF.

  ENDIF.

  IF zcl_user_data=>exists( uname = uname client = client ).

    MESSAGE |This user already exists on client { client }| TYPE 'I' DISPLAY LIKE 'E'.
    RETURN.
  ENDIF.

  DATA user_data TYPE REF TO zcl_user_data.

  IF copy_usr = 'X'.
    user_data = zcl_user_data=>load( uname = uname client = sy-mandt ).
  ELSEIF new_usr = 'X'.
    user_data = zcl_user_data=>new( uname = uname
                                    client = client
                                    password = pw
                                    creator = changer
                                    creation_date = cng_date
                                    creation_time = cng_time
                                    last_name = l_name
                                    first_name = f_name ).
  ENDIF.


  user_data->set_last_change( changer = changer date = cng_date time = cng_time ).


  DATA(roles) = user_data->get_roles( ).


  IF old_role <> 'X' AND old_only <> 'X'.
    roles->clear_roles( ).
  ENDIF.

  IF sap_all = 'X'.

    roles->add_sap_all( ).
  ENDIF.

  IF hid_sa = 'X'.

    roles->hide_sap_all( ).
  ENDIF.



  missing_roles = user_data->get_roles( )->check_role_existence( client ).


  DATA text TYPE string.
  DATA text_button1 TYPE char12.
  DATA text_button2 TYPE char12.
  DATA answer.

**********************************************************************
* Copy Check
**********************************************************************
* If the User wants to copy the old roles, a popup is shown to inform
* the user of specific szenarios
*
* Szenario 1: the user wants to copy only the old roles and none of them exists.
*             Copying the user to the client is not allowed because without any
*             roles the user would be useless
* Szenario 2: the user wants to copy only the old roles and only some of them exist.
*             Copying the user would be allowed but a message is displayed if he is sure
* Szenario 3: User wants to add SAP_ALL or hidden SAP_ALL and also copy the old roles.
*             He can copy the user but he is informed that only the existing roles are
*             getting copied because copying non existing roles would be suspicous
**********************************************************************

  IF old_role = 'X' AND old_only = 'X'.


    IF missing_roles IS NOT INITIAL OR roles->count( ) = 0.
*
      DATA(no_roles_would_be_copied) = COND #( WHEN lines( missing_roles ) = roles->count( )
                                                  THEN abap_true
                                                  ELSE abap_false ).



      IF no_roles_would_be_copied = abap_true AND NOT roles->has_sap_all( ).
        text = |None of the roles you wanted to copy exist on the target client. The user wouldn't have SAP_ALL rights | &&
               |on the target client either. Add SAP_ALL or hidden SAP_ALL or choose another user to copy|.
        text_button1 = 'Add SAP_ALL'.
        text_button2 = 'hide SAP_ALL'.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question         = text
            text_button_1         = text_button1
            text_button_2         = text_button2
            display_cancel_button = 'X'
          IMPORTING
            answer                = answer.

        IF answer = 'A'.
          RETURN.
        ENDIF.

        IF answer = '1'.
          roles->add_sap_all( ).
        ELSEIF answer = '2'.
          roles->hide_sap_all( ).
        ENDIF.

      ELSEIF no_roles_would_be_copied = abap_false.
        text = |Some roles that you wanted to copy don't exist on the target client. Only the existing roles are getting| &&
               | copied.  Are you sure you want to proceed. If not better go back and press the CHECK ROLES Button|.

        text_button1 = |I'm sure|.
        text_button2 = 'Go back'.

        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            text_question = text
            text_button_1 = text_button1
            text_button_2 = text_button2
          IMPORTING
            answer        = answer.

        IF answer = '2'.
          RETURN.
        ENDIF.

      ENDIF.


    ENDIF.

  ENDIF.

  IF old_role = 'X' AND old_only <> 'X' AND missing_roles IS NOT INITIAL.

    text = |Some roles that you wanted to copy don't exist on the target client. Only the existing roles are getting| &&
         | copied.|.

    CALL FUNCTION 'POPUP_TO_CONFIRM'
      EXPORTING
        text_question         = text
        text_button_1         = 'proceed'
        display_cancel_button = 'X'
      IMPORTING
        answer                = answer.

    IF answer = 'A'.
      RETURN.
    ENDIF.
  ENDIF.

  "remove roles. A user with roles that don't exist is very suspicous
  LOOP AT missing_roles INTO DATA(missing_role).

    roles->remove_role( missing_role-role ).

  ENDLOOP.


  TRY.
      user_data->write_to_client( client ).
    CATCH zcx_insert_error INTO DATA(err).

      DATA(message) = |There was an insert that caused an error. The table { err->get_failed_table( ) } | &&
                      |might already have an entry for the user { uname }. Creation of user canceled|.

      MESSAGE message TYPE 'I' DISPLAY LIKE 'E'.

      ROLLBACK WORK.
      RETURN.

  ENDTRY.

  IF copy_usr = 'X'.
    MESSAGE |User { uname } copied successfully| TYPE 'S'.
  ELSEIF new_usr = 'X'.
    MESSAGE |User { uname } created successfully| TYPE 'S'.
  ENDIF.

  "! Sets the fields of the last changer. That includes the uname of the changer, the comment for the fullname
  "! the text if it is the best possible changer and the date and time of last change.
  "! <br/> <br/>
  "! It uses the global variables BEST_POSSIBLE_CHANGER, LAST_CHANGER_OF_USER_TO_COPY, CACHED_BEST_POSSIBLE_CHANGE
  "!  and CHANGER.
  "!
  "! @parameter set_best_possible | If set to true the best possible changer is used. The best possible changer is
  "! the one that changed the most users
FORM set_last_change USING set_best_possible TYPE abap_bool uname TYPE uname client TYPE mandt.

  CLEAR cng_stat.

  DATA change_details TYPE zcl_user_data=>create_change_details.
  "The user typed a new changer
  IF set_best_possible = abap_false AND changer <> best_possible_changer AND changer <> last_changer_of_user_to_copy AND changer IS NOT INITIAL.
    cng_name = zcl_user_data=>get_fullname( changer ).
    IF cng_name IS INITIAL.
      cng_name = changer_does_not_exist_text.
    ENDIF.
    RETURN.
  ENDIF.

  " The user pressed enter and the best_possible_changer is filled by program. Now only the text has to be
  " updated
  IF set_best_possible = abap_true AND changer = best_possible_changer AND changer IS NOT INITIAL.
    cng_stat = best_possible_changer_text.
    RETURN.
  ENDIF.

  "the user wants to set an own changer. Only update the fullname
  IF set_best_possible = abap_false AND changer = last_changer_of_user_to_copy AND changer IS NOT INITIAL.
    cng_name = zcl_user_data=>get_fullname( changer ).
    RETURN.
  ENDIF.

  "trying to get the cached best possible changer first so no database fetch has to be done
  IF set_best_possible = abap_true.
    IF cached_best_possible_change IS NOT INITIAL and client = last_client.
      change_details = cached_best_possible_change.
    ELSE.
      change_details = cached_best_possible_change = zcl_user_data=>get_best_possible_changer( client ).
      last_client = client.
    ENDIF.
    best_possible_changer  = change_details-uname.
    cng_stat = best_possible_changer_text.
  ELSE.
    change_details = zcl_user_data=>get_change_details( uname = uname client = sy-mandt ).
    last_changer_of_user_to_copy = change_details-uname.
  ENDIF.


  changer = change_details-uname.
  cng_date = change_details-date.
  cng_time = change_details-time.
  cng_name = change_details-full_name.
ENDFORM.

FORM show_missing_roles.
  IF missing_roles IS INITIAL OR uname <> last_missing_roles_uname.
    last_missing_roles_uname = uname.
    DATA(roles) =  zcl_roles=>load( uname ).

    IF roles->count( ) = 0.
      MESSAGE |The user { uname } got no roles| TYPE 'I'.
    ENDIF.

    missing_roles = roles->check_role_existence( client ).

  ENDIF.

  IF missing_roles IS NOT INITIAL.

    cl_salv_table=>factory( IMPORTING r_salv_table = DATA(missing_roles_popup) CHANGING t_table = missing_roles ).

    missing_roles_popup->get_columns( )->get_column( 'NR' )->set_visible( abap_false ).


    missing_roles_popup->get_display_settings( )->set_list_header( 'following roles are missing' ).
    missing_roles_popup->set_screen_popup(
      EXPORTING
        start_column = 55
        end_column   = 83"115 "174 max
        start_line   = 8
        end_line     = 12 "max 20
    ).

    missing_roles_popup->display( ).

  ELSE.
    MESSAGE 'All roles are existing on target client' TYPE 'S'.
  ENDIF.
ENDFORM.
