REPORT zdemo_email.
"send email with HTML body using email template
"Title                  ABAP-HTML-eMail Demo
"Text elements
"PEMLTMPT	Email body FROM Email template
"PHTMLEML	HTML Email body
"pitb2xls	Itab TO XLS attachment
"PSO10TXT	Email body FROM so10 TEXT
"pspl2pdf	Email spool AS PDF attachment
"PSPOOL	Spool request NUMBER
"PZIPATT  ZIP attachment
"P_SENDEM	Send Email TO
"S_DLINAM	Recipient Distribution List
"S_EMAIL  Recipient Email id's

CLASS zcl_email_demo DEFINITION
FINAL
CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_filters,
        phtmleml TYPE c LENGTH 1,
        pspl2pdf TYPE c LENGTH 1,
        potf2pdf TYPE c LENGTH 1,
        pitb2xls TYPE c LENGTH 1,
        pzipatt  TYPE c LENGTH 1,
        pspool   TYPE rspoid,
        pemltmpt TYPE smtg_tmpl_id,
        p_sendem TYPE c LENGTH 1,
        s_email  TYPE RANGE OF ad_smtpadr,
        s_dlinam TYPE RANGE OF so_dli_nam,
        pso10txt TYPE tdobname,
      END OF ty_filters.

    DATA:
      filters TYPE ty_filters READ-ONLY.

    METHODS:
      constructor,
      process,
      htmleml,
      spolpdf,
      otfpdf,
      itabxls,
      zipattach.

  PRIVATE SECTION.

    METHODS:
      get_selscrn_val,
      sf_example_01 RETURNING VALUE(otfdata) TYPE tsfotf.

ENDCLASS.

CLASS zcl_email_demo IMPLEMENTATION.

  METHOD constructor.
    get_selscrn_val( ).
  ENDMETHOD.

  METHOD get_selscrn_val.

    DATA:
      li_seltab     TYPE STANDARD TABLE OF rsparams WITH KEY selname,
      li_selopts255 TYPE STANDARD TABLE OF rsparamsl_255 WITH KEY selname.

    FIELD-SYMBOLS :
      <fs_para> TYPE any,
      <fs_sopt> TYPE STANDARD TABLE,
      <fs_line> TYPE any.

    CALL FUNCTION 'RS_REFRESH_FROM_SELECTOPTIONS'
      EXPORTING
        curr_report         = sy-cprog
      TABLES
        selection_table     = li_seltab
        selection_table_255 = li_selopts255
      EXCEPTIONS
        not_found           = 1
        no_report           = 2
        OTHERS              = 3.
    IF sy-subrc NE  0.
    ENDIF.

    LOOP AT li_selopts255 INTO DATA(lw_selopts255).
      TRY.
          IF lw_selopts255-kind EQ 'P'. "parameter
            ASSIGN COMPONENT lw_selopts255-selname OF STRUCTURE filters TO <fs_para>.
            IF sy-subrc EQ 0.
              <fs_para> = lw_selopts255-low.
            ENDIF.
          ELSEIF lw_selopts255-kind EQ 'S' AND "selection-option
            lw_selopts255-option IS NOT INITIAL.
            ASSIGN COMPONENT lw_selopts255-selname OF STRUCTURE filters TO <fs_sopt>.
            IF sy-subrc EQ 0.
              APPEND INITIAL LINE TO <fs_sopt> ASSIGNING <fs_line>.
              MOVE-CORRESPONDING lw_selopts255 TO <fs_line>.
            ENDIF.
          ENDIF.
        CATCH cx_root.
          CONTINUE.
      ENDTRY.
    ENDLOOP.
  ENDMETHOD.

  METHOD htmleml.
    CHECK filters-p_sendem = 'X'.
    TRY.
        DATA(email) = NEW zcl_email( ).

        LOOP AT filters-s_email INTO DATA(ls_email).
          email->add_recipient( CONV #( ls_email-low ) ).
        ENDLOOP.

        LOOP AT filters-s_dlinam INTO DATA(ls_dlinam).
          email->add_dl_recipients( ls_dlinam-low ).
        ENDLOOP.

        DATA lv_expiry_dt TYPE dats.

        SELECT carrid, connid, fldate, price
        UP TO 10 ROWS FROM sflight INTO TABLE @DATA(lt_sflight).

        lv_expiry_dt  = sy-datum + 10. "add logic to get user id expiry date

        DATA(lv_days) = lv_expiry_dt - sy-datum.

        email->set_sender( iv_address = 'do@not.reply' iv_visible_name = 'Do not reply ' ).

        email->set_placeholder_itab(
        EXPORTING placeholder_name  = '&SFLIGHT_TAB&'
          placeholder_itab = lt_sflight ).

        email->set_placeholder(
        EXPORTING
          placeholder_name  = '&USER_FIRST_NAME&'
          placeholder_value =  CONV #( 'User firstname' ) ).

        email->set_placeholder(
        EXPORTING
          placeholder_name  = '&USER_ID&'
          placeholder_value = CONV #( 'user_id' ) ).

        email->set_placeholder(
        EXPORTING
          placeholder_name  =  '&EXPIRY_DT&'
          placeholder_value =  |{ lv_expiry_dt DATE = ISO }| ).

        email->set_placeholder(
        EXPORTING
          placeholder_name  =  '&DAYS&'
          placeholder_value =  CONV #( lv_days ) ).

        email->set_subject_body_template(
        EXPORTING
          template_id = filters-pemltmpt  "Email body from Email template
          doctype      = 'HTM' ).

        email->set_send_immediately( abap_false ).
        email->send( ).

      CATCH cx_bcs_send INTO DATA(ex).
        MESSAGE ex->get_text( ) TYPE 'S'.
    ENDTRY.

  ENDMETHOD.

  METHOD spolpdf.
    CHECK filters-p_sendem = 'X'.

    TRY.
        DATA(email) = NEW zcl_email( ).

        LOOP AT filters-s_email INTO DATA(ls_email).
          email->add_recipient( CONV #( ls_email-low ) ).
        ENDLOOP.

        LOOP AT filters-s_dlinam INTO DATA(ls_dlinam).
          email->add_dl_recipients( ls_dlinam-low ).
        ENDLOOP.

        email->set_sender( iv_address = 'do@not.reply' iv_visible_name = 'Do not reply' ).

        email->set_placeholder(
        EXPORTING
          placeholder_name  = '&USER_FIRST_NAME&'
          placeholder_value =  CONV #( 'User firstname' ) ).

        email->add_attachment(
        EXPORTING
          iv_doctype      = 'pdf'             " Document Type
          "iv_description  =                  " Short Description of Contents
          iv_filename     = |{  filters-pspool }.pdf| " File Name (with Extension)
        iv_contents_bin =
        NEW zcl_spool_to_pdf( )->convert( filters-pspool ) ).  " Binary Document Content

        email->set_body_so10( text_name = filters-pso10txt ).
        email->set_subject_so10( text_name = '' ).
        email->set_subject( 'Email Demo2 PDF attachment' ).

        email->set_send_immediately( abap_false ).
        email->send( ).

      CATCH zcx_email INTO DATA(lx_email).
        MESSAGE lx_email->get_text( ) TYPE 'S'.
      CATCH cx_bcs_send INTO DATA(ex) .
        MESSAGE ex->get_text( ) TYPE 'S'.
    ENDTRY.
  ENDMETHOD.

  METHOD otfpdf.
    CHECK filters-p_sendem = 'X'.

    TRY.
        DATA(email) = NEW zcl_email( ).

        LOOP AT filters-s_email INTO DATA(ls_email).
          email->add_recipient( CONV #( ls_email-low ) ).
        ENDLOOP.

        LOOP AT filters-s_dlinam INTO DATA(ls_dlinam).
          email->add_dl_recipients( ls_dlinam-low ).
        ENDLOOP.

        email->set_sender( iv_address = 'do@not.reply' iv_visible_name = 'Do not reply' ).

        email->set_placeholder(
        EXPORTING
          placeholder_name  = '&USER_FIRST_NAME&'
          placeholder_value =  CONV #( 'User firstname' ) ).

        TRY.
            "DATA(otfdata) = sf_example_01( ).
            DATA(lv_contents_bin) = NEW zcl_otf_to_pdf( )->convert( sf_example_01( ) ).

            email->add_attachment(
            EXPORTING
              iv_doctype      = 'pdf'             " Document Type
              "iv_description  =                  " Short Description of Contents
              iv_filename     = |OTF_TO_PDF.pdf| " File Name (with Extension)
              iv_contents_bin = lv_contents_bin ). " Binary Document Content
          CATCH zcx_email INTO DATA(lx_email).
            MESSAGE lx_email->get_text( ) TYPE 'S'.
        ENDTRY.


        email->set_body_so10( text_name = filters-pso10txt ).
        email->set_subject_so10( text_name = '' ).
        email->set_subject( 'Email OTF_TO_PDF attachment' ).

        email->set_send_immediately( abap_false ).
        email->send( ).

      CATCH zcx_email INTO lx_email.
        MESSAGE lx_email->get_text( ) TYPE 'S'.
      CATCH cx_bcs_send INTO DATA(ex) .
        MESSAGE ex->get_text( ) TYPE 'S'.
    ENDTRY.

  ENDMETHOD.

  METHOD itabxls.
    CHECK filters-p_sendem = 'X'.

    TRY.
        DATA(email) = NEW zcl_email( ).

        LOOP AT filters-s_email INTO DATA(ls_email).
          email->add_recipient( CONV #( ls_email-low ) ).
        ENDLOOP.

        LOOP AT filters-s_dlinam INTO DATA(ls_dlinam).
          email->add_dl_recipients( ls_dlinam-low ).
        ENDLOOP.

        email->set_sender( iv_address = 'do@not.reply' iv_visible_name = 'Do not reply ' ).

        email->set_placeholder(
        EXPORTING
          placeholder_name  = '&USER_FIRST_NAME&'
          placeholder_value =  CONV #( 'User firstname' ) ).

        SELECT carrid, connid, fldate, price
        UP TO 10 ROWS FROM sflight INTO TABLE @DATA(lt_sflight).

        IF lt_sflight IS NOT INITIAL.

          email->add_attachment(
          EXPORTING
            iv_doctype      = |XLS|             " Document Type
            iv_filename     = |itab_to_excel.xls|   " File Name (with Extension)
          iv_contents_bin = NEW zcl_itab_to_xls( )->convert( lt_sflight ) ).  " Binary Document Content
        ENDIF.

        email->set_subject_body_template(
        EXPORTING
          template_id = filters-pemltmpt  "Email body from Email template
          doctype      = 'HTM' ).

        email->set_send_immediately( abap_false ).
        email->send( ).

      CATCH zcx_email INTO DATA(lx_email).
        MESSAGE lx_email->get_text( ) TYPE 'S'.
      CATCH cx_bcs_send INTO DATA(ex) .
        MESSAGE ex->get_text( ) TYPE 'S'.
    ENDTRY.
  ENDMETHOD.

  METHOD zipattach.
    CHECK filters-p_sendem = 'X'.

    TRY.
        DATA(email) = NEW zcl_email( ).

        LOOP AT filters-s_email INTO DATA(ls_email).
          email->add_recipient( CONV #( ls_email-low ) ).
        ENDLOOP.

        LOOP AT filters-s_dlinam INTO DATA(ls_dlinam).
          email->add_dl_recipients( ls_dlinam-low ).
        ENDLOOP.

        email->set_sender( iv_address = 'do@not.reply' iv_visible_name = 'Do not reply ' ).

        email->set_placeholder(
        EXPORTING
          placeholder_name  = '&USER_FIRST_NAME&'
          placeholder_value =  CONV #( 'User firstname' ) ).

        SELECT carrid, connid, fldate, price
        UP TO 10 ROWS FROM sflight INTO TABLE @DATA(lt_sflight).

        IF lt_sflight IS NOT INITIAL.
          email->add_zip_attachments( EXPORTING
                                        i_zip_filename    = 'attach.zip'
                                        it_attachments = VALUE bcst_attachment(
                                                                                ( doctype      = 'HTM'
                                                                                  filename     = |itab_to_HTML.html|
                                                                                  contents_txt = NEW zcl_itab_to_html( )->convert( lt_sflight ) )
                                                                                ( doctype      = 'XLS'
                                                                                  filename     = |itab_to_excel.xls|
                                                                                  contents_bin = NEW zcl_itab_to_xls( )->convert( lt_sflight ) )
                                                                                )
                                      ).
        ENDIF.

        email->set_subject_body_template(
        EXPORTING
          template_id = filters-pemltmpt  "Email body from Email template
          doctype      = 'HTM' ).

        email->set_send_immediately( abap_false ).
        email->send( ).

      CATCH zcx_email INTO DATA(lx_email).
        MESSAGE lx_email->get_text( ) TYPE 'S'.
      CATCH cx_bcs_send INTO DATA(ex) .
        MESSAGE ex->get_text( ) TYPE 'S'.
    ENDTRY.
  ENDMETHOD.

  METHOD sf_example_01.
    DATA:
      fm_name               TYPE rs38l_fnam,
      ls_output_options     TYPE ssfcompop,
      ls_control_parameters TYPE ssfctrlop,
      ls_job_output_info    TYPE ssfcrescl,
      customer              TYPE scustom,
      bookings              TYPE ty_bookings,
      connections           TYPE ty_connections.

* get data
    SELECT SINGLE * FROM scustom INTO customer WHERE id = '1'.
    CHECK sy-subrc = 0.

    SELECT *
    FROM sbook INTO TABLE bookings
    WHERE customid = customer-id
    AND   carrid   = 'LH'
    ORDER BY PRIMARY KEY.

    IF bookings[] IS NOT INITIAL.
      SELECT * FROM spfli INTO TABLE connections
      FOR ALL ENTRIES IN bookings
      WHERE carrid = bookings-carrid
      AND   connid = bookings-connid
      ORDER BY PRIMARY KEY.
    ENDIF.

* print data
    CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
      EXPORTING
        formname           = 'SF_EXAMPLE_01'
      IMPORTING
        fm_name            = fm_name
      EXCEPTIONS
        no_form            = 1
        no_function_module = 2
        OTHERS             = 3.

    IF sy-subrc <> 0.
*   error handling
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      EXIT.
    ENDIF.

    ls_control_parameters-getotf = 'X'.
    ls_control_parameters-no_dialog  = 'X'.
    ls_output_options-tddest  = |LP01|.

* now call the generated function module
    CALL FUNCTION fm_name
      EXPORTING
        control_parameters = ls_control_parameters
        output_options     = ls_output_options
        customer           = customer
        bookings           = bookings
        connections        = connections
      IMPORTING
*       document_output_info =
        job_output_info    = ls_job_output_info
*       job_output_options =
      EXCEPTIONS
        formatting_error   = 1
        internal_error     = 2
        send_error         = 3
        user_canceled      = 4
        OTHERS             = 5.

    IF sy-subrc <> 0.
*   error handling
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
      otfdata = ls_job_output_info-otfdata.
    ENDIF.
  ENDMETHOD.

  METHOD process.

    IF 'X' = filters-phtmleml.
      htmleml( ).
    ENDIF.
    IF 'X' = filters-pspl2pdf.
      spolpdf( ).
    ENDIF.
    IF 'X' = filters-potf2pdf.
      otfpdf( ).
    ENDIF.
    IF 'X' = filters-pitb2xls.
      itabxls( ).
    ENDIF.
    IF 'X' = filters-pzipatt.
      zipattach( ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.

PARAMETERS:
  "HTML Email body
  phtmleml AS CHECKBOX DEFAULT 'X' USER-COMMAND abcd,
  "Email spool as PDF attachment
  pspl2pdf AS CHECKBOX DEFAULT 'X' USER-COMMAND abcd,
  "Email OTF as PDF attachment
  potf2pdf AS CHECKBOX DEFAULT 'X' USER-COMMAND abcd,
  "Itab to XLS attachment
  pitb2xls AS CHECKBOX DEFAULT 'X' USER-COMMAND abcd,
  "ZIP attachment
  pzipatt  AS CHECKBOX DEFAULT 'X' USER-COMMAND abcd,
  "Spool request number
  pspool   TYPE rspoid MODIF ID spl, " DEFAULT '30594',
  "Email body from Email template
  pemltmpt TYPE smtg_tmpl_id MODIF ID tmp DEFAULT 'ZET_DEMO',
  "Email body from so10 text
  pso10txt TYPE tdobname MODIF ID spl DEFAULT 'ZEMAIL_DEMO2_BODY'.

DATA :
  soemail       TYPE ad_smtpadr,
  gv_so_dli_nam TYPE so_dli_nam,
  gs_restrict   TYPE sscr_restrict.

SELECTION-SCREEN BEGIN OF BLOCK rdblk WITH FRAME TITLE blkttl.
PARAMETERS p_sendem AS CHECKBOX MODIF ID gr1 DEFAULT 'X'.                 "Send Mail
SELECT-OPTIONS s_email FOR soemail NO INTERVALS MODIF ID gr1. "E-Mail Address
SELECT-OPTIONS s_dlinam FOR gv_so_dli_nam NO INTERVALS MATCHCODE OBJECT /aif/so_dli_nam.
SELECTION-SCREEN END OF BLOCK rdblk.

AT SELECTION-SCREEN OUTPUT.
  IF pspl2pdf = 'X' OR
     pzipatt  = 'X'.
    DATA(lv_active) = 1.
  ELSE.
    lv_active = 0.
  ENDIF.

  IF phtmleml = 'X' OR pitb2xls = 'X'.
    DATA(lv_active1) = 1.
  ELSE.
    lv_active1 = 0.
  ENDIF.

  LOOP AT SCREEN.
    CASE screen-group1.
      WHEN 'SPL'.
        screen-active = lv_active.
        MODIFY SCREEN.
      WHEN 'TMP'.
        screen-active = lv_active1.
        MODIFY SCREEN.
    ENDCASE.
  ENDLOOP.

INITIALIZATION.

  SELECT rqident UP TO 1 ROWS
    FROM tsp01 INTO pspool
    ORDER BY rqcretime DESCENDING.
  ENDSELECT.

  s_email[] = VALUE #( sign = 'I' option  = 'EQ'
                     ( low = 'test@test.com' ) ).

START-OF-SELECTION.
  IF pspool IS INITIAL.
    SELECT rqident UP TO 1 ROWS
      FROM tsp01 INTO pspool
      ORDER BY rqcretime DESCENDING.
    ENDSELECT.
  ENDIF.

  pemltmpt =  COND #( WHEN pemltmpt IS INITIAL THEN |ZET_DEMO| ELSE pemltmpt ).
  pso10txt =  COND #( WHEN pso10txt IS INITIAL THEN |ZEMAIL_DEMO2_BODY| ELSE pso10txt ).

  DATA(email_demo) = NEW zcl_email_demo( ).
  email_demo->process( ).
