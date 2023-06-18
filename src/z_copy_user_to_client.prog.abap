*&---------------------------------------------------------------------*
*& Report z_copy_user_to_client
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_copy_user_to_client.

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
