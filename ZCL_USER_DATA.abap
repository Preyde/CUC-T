CLASS zcl_user_data DEFINITION
  PUBLIC
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
