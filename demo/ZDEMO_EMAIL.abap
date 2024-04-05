REPORT zdemo_email.
"send email with HTML body using email template
"Title                  ABAP-HTML-eMail Demo
"Text elements
"PEMLTMPT	Email body FROM Email template
"PHTMLEML	HTML Email body
"PITABXLS	Itab TO XLS attachment
"PSO10TXT	Email body FROM so10 TEXT
"PSPOLPDF	Email spool AS PDF attachment
"PSPOOL	Spool request NUMBER
"PZIPATT  ZIP attachment
"P_SENDEM	Send Email TO
"S_DLINAM	Recipient Distribution List
"S_EMAIL  Recipient Email id's

PARAMETERS:
  "HTML Email body
  phtmleml RADIOBUTTON GROUP sel DEFAULT 'X' USER-COMMAND abcd,
  "Email spool as PDF attachment
  pspolpdf RADIOBUTTON GROUP sel,
  "Itab to XLS attachment
  pitabxls RADIOBUTTON GROUP sel,
  "ZIP attachment
  pzipatt  RADIOBUTTON GROUP sel,
  "Spool request number
  pspool   TYPE rspoid MODIF ID spl, " DEFAULT '30594',
  "Email body from Email template
  pemltmpt TYPE smtg_tmpl_id MODIF ID tmp DEFAULT 'ZHR_SHIFT_MANAGER',
  "Email body from so10 text
  pso10txt TYPE tdobname MODIF ID spl DEFAULT 'ZEMAIL_DEMO2_BODY'.

DATA :
  soemail       TYPE ad_smtpadr,
  gv_so_dli_nam TYPE so_dli_nam,
  gs_restrict   TYPE sscr_restrict.

SELECTION-SCREEN BEGIN OF BLOCK rdblk WITH FRAME TITLE blkttl.
PARAMETERS p_sendem AS   CHECKBOX MODIF ID gr1.                 "Send Mail
SELECT-OPTIONS s_email  FOR  soemail NO INTERVALS MODIF ID gr1. "E-Mail Address
SELECT-OPTIONS :
s_dlinam FOR gv_so_dli_nam NO INTERVALS MATCHCODE OBJECT /aif/so_dli_nam.
SELECTION-SCREEN END OF BLOCK rdblk.

AT SELECTION-SCREEN OUTPUT.
  IF pspolpdf = 'X' OR
     pzipatt  = 'X'.
    DATA(lv_active) = 1.
  ELSE.
    lv_active = 0.
  ENDIF.

  IF phtmleml = 'X' OR pitabxls = 'X'.
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

START-OF-SELECTION.
  IF pspool IS INITIAL.
    SELECT rqident UP TO 1 ROWS
      FROM tsp01 INTO pspool
      ORDER BY rqcretime DESCENDING.
    ENDSELECT.
  ENDIF.

  pemltmpt =  COND #( WHEN pemltmpt IS INITIAL THEN |ZHR_SHIFT_MANAGER| ELSE pemltmpt ).
  pso10txt =  COND #( WHEN pso10txt IS INITIAL THEN |ZEMAIL_DEMO2_BODY| ELSE pso10txt ).

  CASE 'X'.
    WHEN phtmleml. PERFORM f_htmleml.
    WHEN pspolpdf. PERFORM f_spolpdf USING pspool.
    WHEN pitabxls. PERFORM f_itabxls.
    WHEN pzipatt. PERFORM f_zipattach.
    WHEN OTHERS.
  ENDCASE.

FORM f_htmleml.

  CHECK p_sendem = 'X'.
  TRY.
      DATA(email) = NEW zcl_email( ).

      LOOP AT s_email INTO DATA(ls_email).
        email->add_recipient( CONV #( ls_email-low ) ).
      ENDLOOP.

      LOOP AT s_dlinam INTO DATA(ls_dlinam).
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
          template_id = pemltmpt  "Email body from Email template
          doctype      = 'HTM' ).

      email->set_send_immediately( abap_false ).
      email->send( ).

    CATCH cx_bcs_send INTO DATA(ex).
      MESSAGE ex->get_text( ) TYPE 'S'.
  ENDTRY.
ENDFORM.

FORM f_spolpdf USING spoolid TYPE rspoid.

  CHECK p_sendem = 'X'.

  TRY.
      DATA(email) = NEW zcl_email( ).

      LOOP AT s_email INTO DATA(ls_email).
        email->add_recipient( CONV #( ls_email-low ) ).
      ENDLOOP.

      LOOP AT s_dlinam INTO DATA(ls_dlinam).
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
          iv_filename     = |{  pspool }.pdf| " File Name (with Extension)
          iv_contents_bin =
            NEW zcl_spool_to_pdf( )->convert( spoolid = spoolid ) ).  " Binary Document Content

      email->set_body_so10( text_name = pso10txt ).
      email->set_subject_so10( text_name = '' ).
      email->set_subject( 'Email Demo2 PDF attachment' ).

      email->set_send_immediately( abap_false ).
      email->send( ).

    CATCH zcx_email INTO DATA(lx_email).
      MESSAGE lx_email->get_text( ) TYPE 'S'.
    CATCH cx_bcs_send INTO DATA(ex) .
      MESSAGE ex->get_text( ) TYPE 'S'.
  ENDTRY.
ENDFORM.

FORM f_itabxls.
  CHECK p_sendem = 'X'.

  TRY.
      DATA(email) = NEW zcl_email( ).

      LOOP AT s_email INTO DATA(ls_email).
        email->add_recipient( CONV #( ls_email-low ) ).
      ENDLOOP.

      LOOP AT s_dlinam INTO DATA(ls_dlinam).
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
        template_id = pemltmpt  "Email body from Email template
        doctype      = 'HTM' ).

      email->set_send_immediately( abap_false ).
      email->send( ).

    CATCH zcx_email INTO DATA(lx_email).
      MESSAGE lx_email->get_text( ) TYPE 'S'.
    CATCH cx_bcs_send INTO DATA(ex) .
      MESSAGE ex->get_text( ) TYPE 'S'.
  ENDTRY.
ENDFORM.

FORM f_zipattach.
  CHECK p_sendem = 'X'.

  TRY.
      DATA(email) = NEW zcl_email( ).

      LOOP AT s_email INTO DATA(ls_email).
        email->add_recipient( CONV #( ls_email-low ) ).
      ENDLOOP.

      LOOP AT s_dlinam INTO DATA(ls_dlinam).
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
        email->add_zip_attachments(
        EXPORTING
          i_zip_filename    = 'attach.zip'
          it_attachments = VALUE bcst_attachment( ( doctype      = 'XLS'
                                                    filename     = |itab_to_excel.xls|
                                                    contents_bin = NEW zcl_itab_to_xls( )->convert( lt_sflight ) ) ) ).
      ENDIF.

      email->set_subject_body_template(
      EXPORTING
        template_id = pemltmpt  "Email body from Email template
        doctype      = 'HTM' ).

      email->set_send_immediately( abap_false ).
      email->send( ).

    CATCH zcx_email INTO DATA(lx_email).
      MESSAGE lx_email->get_text( ) TYPE 'S'.
    CATCH cx_bcs_send INTO DATA(ex) .
      MESSAGE ex->get_text( ) TYPE 'S'.
  ENDTRY.
ENDFORM.
