CLASS zcl_email_bcs DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      set_subject
        IMPORTING
          iv_subject TYPE bcs_subject
        RAISING
          zcx_email,

      set_body_so10
        IMPORTING
          text_name TYPE tdobname OPTIONAL
          language  TYPE bcs_language DEFAULT sy-langu
          doctype   TYPE bcs_doctype DEFAULT 'txt'
          tdid      TYPE thead-tdid DEFAULT 'ST'
          tdobject  TYPE thead-tdobject DEFAULT 'TEXT',

      set_subject_so10
        IMPORTING
          text_name TYPE tdobname OPTIONAL
          language  TYPE bcs_language DEFAULT sy-langu
          doctype   TYPE bcs_doctype DEFAULT 'txt'
          tdid      TYPE thead-tdid DEFAULT 'ST'
          tdobject  TYPE thead-tdobject DEFAULT 'TEXT'
        RAISING
          zcx_email,

      add_recipient
        IMPORTING
          iv_address      TYPE bcs_address
          iv_commtype     TYPE bcs_commtype  DEFAULT 'INT'
          iv_visible_name TYPE bcs_visname OPTIONAL
          iv_copy         TYPE bcs_copy OPTIONAL
          iv_fax_country  TYPE bcs_fax_country OPTIONAL
        RAISING
          zcx_email,

      add_dl_recipients
        IMPORTING
          dlinam TYPE so_dli_nam
          copy   TYPE bcs_copy OPTIONAL
        RAISING
          zcx_email,

      set_sender
        IMPORTING
          iv_address      TYPE bcs_address
          iv_commtype     TYPE bcs_commtype  DEFAULT 'INT'
          iv_visible_name TYPE bcs_visname OPTIONAL
          iv_fax_country  TYPE bcs_fax_country OPTIONAL
        RAISING
          zcx_email,

      set_main_doc
        IMPORTING
          iv_contents_txt TYPE string OPTIONAL
          iv_contents_bin TYPE xstring OPTIONAL
          iv_doctype      TYPE bcs_doctype DEFAULT 'txt'
          iv_codepage     TYPE bcs_codepage OPTIONAL
            PREFERRED PARAMETER iv_contents_txt,

      add_attachment
        IMPORTING
          iv_doctype      TYPE bcs_doctype OPTIONAL
          iv_description  TYPE bcs_description OPTIONAL
          iv_filename     TYPE bcs_filename OPTIONAL
          iv_codepage     TYPE bcs_codepage OPTIONAL
          iv_contents_txt TYPE string OPTIONAL
          iv_contents_bin TYPE xstring OPTIONAL
          iv_content_id   TYPE bcs_content_id OPTIONAL
        RAISING
          zcx_email,

      set_placeholder
        IMPORTING
          placeholder_tab TYPE if_smtg_email_template=>ty_gt_data_key OPTIONAL,

      set_placeholder_itab
        IMPORTING
          placeholder_name        TYPE string
          VALUE(placeholder_itab) TYPE STANDARD TABLE,

      "! <p class="shorttext synchronized" lang="en">set email body and subject from email Template id</p>
      set_subject_body_template
        IMPORTING
          template_id TYPE smtg_tmpl_id
          language    TYPE bcs_language DEFAULT sy-langu
          doctype     TYPE bcs_doctype DEFAULT 'txt'
        RAISING
          zcx_email,

      "! <p class="shorttext synchronized" lang="en">add ZIP attachments</p>
      add_zip_attachments
        IMPORTING
          i_zip_doctype     TYPE bcs_doctype DEFAULT 'zip'
          i_zip_description TYPE bcs_description OPTIONAL
          i_zip_filename    TYPE bcs_filename DEFAULT 'attach.zip'
          i_codepage        TYPE bcs_codepage OPTIONAL
          it_attachments    TYPE bcst_attachment
        RAISING
          zcx_email,

      set_reply_to
        IMPORTING
          iv_address      TYPE csequence
          iv_commtype     TYPE bcs_commtype DEFAULT 'INT'
          iv_visible_name TYPE bcs_visname OPTIONAL
        RAISING
          zcx_email,

      set_importance
        IMPORTING
          iv_importance TYPE bcs_importance OPTIONAL,

      set_sensitivity
        IMPORTING
          iv_sensitivity TYPE bcs_sensitivity OPTIONAL,

      set_requested_status
        IMPORTING
          VALUE(iv_requested_status) TYPE bcs_requested_status
        RAISING
          zcx_email,

      set_status_mail
        IMPORTING
          VALUE(iv_status_mail) TYPE bcs_status_mail
        RAISING
          zcx_email,

      set_language
        IMPORTING
          iv_language TYPE bcs_language OPTIONAL,

      set_disclosure
        IMPORTING
          VALUE(iv_disclosure) TYPE bcs_disclosure,

      set_sign
        IMPORTING
          VALUE(iv_sign) TYPE sap_bool
        RAISING
          zcx_email,

      set_update_task
        IMPORTING
          iv_update_task TYPE bcs_update_task,

      set_encrypt
        IMPORTING
                  VALUE(iv_encrypt) TYPE sap_bool
        RAISING   zcx_email,

      set_send_immediately
        IMPORTING
          VALUE(iv_immediately) TYPE bcs_immediately
        RAISING
          zcx_email,

      send
        RETURNING
          VALUE(rt_err_recipients) TYPE bcst_rec_status
        RAISING
          zcx_email,

      set_send_at
        IMPORTING
          senddat TYPE soes-snddat
          sendtim TYPE soes-sndtim,

      oid
        RETURNING
          VALUE(result) TYPE sysuuid_x
        RAISING
          zcx_email,

      set_priority
        IMPORTING
          i_priority TYPE so_snd_pri
        RAISING
          zcx_email,

      set_expires_on
        IMPORTING
          VALUE(i_expires_on) TYPE bcs_xpire,

      constructor.
  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA:
      mo_intern   TYPE REF TO cl_bcs_msg_intern,
      mo_cl_bcs   TYPE REF TO cl_bcs,
      gt_data_key TYPE if_smtg_email_template=>ty_gt_data_key.

    METHODS:
      set_main_doc_bcs
        RAISING
          zcx_email,

      "! <p class="shorttext synchronized" lang="en">Replace placeholder than CDS</p>
      replace_placeholder
        IMPORTING replace_string TYPE string
        RETURNING VALUE(result)  TYPE string,

      from_so10
        IMPORTING
                  text_name             TYPE tdobname
                  language              TYPE bcs_language
                  doctype               TYPE bcs_doctype
                  tdid                  TYPE thead-tdid
                  tdobject              TYPE thead-tdobject
        RETURNING VALUE(r_contents_txt) TYPE string.

ENDCLASS.

CLASS zcl_email_bcs IMPLEMENTATION.
  METHOD constructor.
    TRY.
        "Create persistent BCS object
        mo_cl_bcs = cl_bcs=>create_persistent( ).
      CATCH cx_bcs INTO DATA(lx_bcs).
    ENDTRY.
    mo_intern = NEW cl_bcs_msg_intern(  ).

  ENDMETHOD.

  METHOD set_subject.
    mo_intern->mv_subject = iv_subject.
  ENDMETHOD.

  METHOD set_subject_so10.
    "get Email subject from so10 text.

    set_subject( from_so10( EXPORTING text_name = text_name
                                      language  = language
                                      doctype   = doctype
                                      tdid      = tdid
                                      tdobject  = tdobject ) ).

  ENDMETHOD.

  METHOD set_placeholder_itab.
    APPEND  VALUE #( name = placeholder_name
                     "Convert ITAB to HTML. zcl_itab_to_html
                     value = NEW zcl_itab_to_html(  )->convert( placeholder_itab ) )
     TO gt_data_key.

  ENDMETHOD.

  METHOD set_placeholder.
    IF placeholder_tab IS NOT INITIAL.
      APPEND LINES OF placeholder_tab TO gt_data_key.
    ENDIF.

  ENDMETHOD.

  METHOD add_zip_attachments.

    DATA:
      lv_xstring TYPE xstring.

    IF it_attachments  IS NOT INITIAL.
      TRY.
          DATA(lo_zippers) = NEW cl_abap_zip( ). "" Zip class Declaration

          LOOP AT it_attachments  INTO DATA(ls_attachments).
            CLEAR lv_xstring.

            IF ls_attachments-contents_txt IS INITIAL AND
               ls_attachments-contents_bin IS INITIAL.
            ELSE.
              IF ls_attachments-contents_txt IS NOT INITIAL.

                lv_xstring =
                  cl_bcs_convert=>string_to_xstring( iv_string   = ls_attachments-contents_txt
                                                     iv_codepage = CONV #( COND #( WHEN ls_attachments-codepage IS NOT INITIAL
                                                                                   THEN ls_attachments-codepage
                                                                                   ELSE '' ) ) ). "'4103'

              ELSEIF ls_attachments-contents_bin IS NOT INITIAL..
                lv_xstring = ls_attachments-contents_bin.
              ENDIF.

              "Xstring to binary
              "add file to zip
              lo_zippers->add( name    = ls_attachments-filename
                               content = lv_xstring ).

            ENDIF.
          ENDLOOP.

          CLEAR lv_xstring.
          "save zip
          lv_xstring = lo_zippers->save( ).

        CATCH cx_bcs.
          "MESSAGE e445(so).
          zcx_email=>raise_excep( iv_msgid    = 'SO'
                                  iv_msgno    = 445 ).
      ENDTRY.

      add_attachment( iv_doctype      = i_zip_doctype       " Document Type
                      iv_description  = i_zip_description   " Short Description of Contents
                      iv_filename     = i_zip_filename      " File Name (with Extension)
                      iv_contents_bin = lv_xstring ).       " Binary Document Content

    ENDIF.
  ENDMETHOD.

  METHOD replace_placeholder.
    result = replace_string.
    LOOP AT gt_data_key INTO DATA(ls_data_key).
      REPLACE ALL OCCURRENCES OF ls_data_key-name IN result WITH ls_data_key-value.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_subject_body_template.

    " read headers
    SELECT SINGLE cds_view FROM smtg_tmpl_hdr
    INTO @DATA(lv_cds_view)
    WHERE id      = @template_id AND
          version = 'A'.

    IF sy-subrc EQ 0.
      IF lv_cds_view IS NOT INITIAL.
        DATA(lt_data_key) = gt_data_key.
      ENDIF.

      TRY.
          DATA(lo_email_api) = cl_smtg_email_api=>get_instance( iv_template_id = template_id ).

          lo_email_api->render(
            EXPORTING
              iv_language  = language
              it_data_key  = lt_data_key
            IMPORTING
              ev_subject   = DATA(lv_subject)
              ev_body_html = DATA(lv_body_html)
              ev_body_text = DATA(lv_body_text) ).

        CATCH cx_smtg_email_common INTO DATA(ex). " E-Mail API Exceptions
      ENDTRY.

      IF doctype EQ 'HTM'.
        DATA(lv_mailbody) = lv_body_html.
      ELSE.
        lv_mailbody = lv_body_text.
      ENDIF.

      IF lv_cds_view IS INITIAL.
        lv_mailbody = replace_placeholder( lv_mailbody ).
        lv_subject = replace_placeholder( lv_subject ).
      ENDIF.

      set_subject( lv_subject ).

      set_main_doc( iv_contents_txt = lv_mailbody      " Main Documet, First Body Part
                    iv_doctype      = doctype ).       " Document Category

    ENDIF.
  ENDMETHOD.

  METHOD from_so10.

    DATA:
      lv_changed TYPE c LENGTH 1,
      lv_header  TYPE thead,
      lt_lines   TYPE tline_tab.

    IF text_name IS NOT INITIAL.

      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id                      = tdid "'ST'
          language                = language
          name                    = text_name
          object                  = tdobject "'TEXT'
        IMPORTING
          header                  = lv_header
        TABLES
          lines                   = lt_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      IF sy-subrc = 0.

        LOOP AT gt_data_key INTO DATA(ls_data_key).
          CALL FUNCTION 'TEXT_SYMBOL_SETVALUE'
            EXPORTING
              name  = ls_data_key-name
              value = ls_data_key-value.
        ENDLOOP.

        DATA(lv_no_of_lines) = lines( lt_lines ).

        CALL FUNCTION 'TEXT_SYMBOL_REPLACE'
          EXPORTING
            endline       = lv_no_of_lines
            header        = lv_header
            init          = ' '
            option_dialog = ' '
            program       = sy-cprog
          IMPORTING
            changed       = lv_changed
            newheader     = lv_header
          TABLES
            lines         = lt_lines.

        LOOP AT lt_lines INTO DATA(ls_lines).
          IF ls_lines-tdformat = '='  OR
             ls_lines-tdformat = ' '. "   Continuous Text
            r_contents_txt = r_contents_txt && ls_lines-tdline.
          ELSE.
            IF r_contents_txt IS INITIAL.
              r_contents_txt = ls_lines-tdline.
            ELSE.
              r_contents_txt = r_contents_txt && cl_abap_char_utilities=>cr_lf && ls_lines-tdline.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDMETHOD.

  METHOD add_dl_recipients.
    DATA :
      li_dli        TYPE TABLE OF sodlienti1.

    DATA(lv_shared_dli) = zcl_email_utility=>is_dl_shared( dlinam ).

    CALL FUNCTION 'SO_DLI_READ_API1'
      EXPORTING
        dli_name                   = dlinam
        shared_dli                 = lv_shared_dli
      TABLES
        dli_entries                = li_dli
      EXCEPTIONS
        dli_not_exist              = 1
        operation_no_authorization = 2
        parameter_error            = 3
        x_error                    = 4
        OTHERS                     = 5.
    IF sy-subrc = 0.
      LOOP AT li_dli INTO DATA(ls_dli).
        add_recipient(
            iv_address      = CONV #( ls_dli-member_adr )
            iv_visible_name = CONV #( ls_dli-full_name )
            iv_copy         = copy ).

      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD add_recipient.
*    DATA(lv_address) = condense( iv_address ).
*    APPEND VALUE bcss_recipient( commtype    = iv_commtype
*                                  address     = lv_address
*                                  fax_country = iv_fax_country
*                                  visname     = iv_visible_name
*                                  copy        = iv_copy )
*      TO mo_intern->mt_recipients.

    CASE iv_copy.
      WHEN zcl_email_utility=>gc_cc.  DATA(lv_cc) = 'X'.
      WHEN zcl_email_utility=>gc_bcc. DATA(lv_bcc) = 'X'.
      WHEN OTHERS. "NO COPY
    ENDCASE.

    TRY.
        mo_cl_bcs->add_recipient(
          i_recipient  = cl_cam_address_bcs=>create_internet_address( i_address_string = CONV #( condense( iv_address ) )
                                                                      i_address_name   = CONV #( iv_visible_name ) )
                                                                      i_copy           = lv_cc
                                                                      i_blind_copy     = lv_bcc ).

      CATCH  cx_address_bcs INTO DATA(lx_address_bcs).

        zcx_email=>raise_excep( iv_msgid = lx_address_bcs->msgid
                                iv_msgno = lx_address_bcs->msgno ).

      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        zcx_email=>raise_excep( iv_msgid = lx_send_req_bcs->msgid
                                iv_msgno = lx_send_req_bcs->msgno ).

    ENDTRY.

  ENDMETHOD.

  METHOD set_send_at.
    DATA lv_send_at TYPE bcs_sndat.

    CONVERT DATE senddat TIME sendtim INTO TIME STAMP lv_send_at
        TIME ZONE sy-zonlo.

    IF lv_send_at IS NOT INITIAL.
      mo_cl_bcs->send_request->set_send_at( lv_send_at ).
    ENDIF.

  ENDMETHOD.

  METHOD set_sender.
*    mo_intern->ms_sender = VALUE bcss_address( address = iv_address
*                                    commtype = iv_commtype
*                                    visname = iv_visible_name ).

    TRY.
        mo_cl_bcs->set_sender( cl_cam_address_bcs=>create_internet_address( i_address_string = CONV #( iv_address )
                                                                            i_address_name   = CONV #( iv_visible_name ) ) ).
      CATCH  cx_address_bcs INTO DATA(lx_address_bcs).

        zcx_email=>raise_excep( iv_msgid = lx_address_bcs->msgid
                                iv_msgno = lx_address_bcs->msgno ).

      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        zcx_email=>raise_excep( iv_msgid = lx_send_req_bcs->msgid
                                iv_msgno = lx_send_req_bcs->msgno ).

    ENDTRY.

  ENDMETHOD.


  METHOD set_body_so10.
    "get Email body from so10 text.
    mo_intern->ms_main_doc = VALUE bcss_attachment( doctype      = doctype
                                                    "codepage     = iv_codepage
                                                    contents_txt = from_so10( EXPORTING text_name = text_name
                                                                                        language  = language
                                                                                        doctype   = doctype
                                                                                        tdid      = tdid
                                                                                        tdobject  = tdobject )
                                                     ) .

  ENDMETHOD.

  METHOD set_main_doc.

    mo_intern->ms_main_doc = VALUE bcss_attachment( doctype      = iv_doctype
                                                    codepage     = iv_codepage
                                                    contents_txt = iv_contents_txt
                                                    contents_bin = iv_contents_bin ) .

  ENDMETHOD.

  METHOD add_attachment.
    DATA:
      lv_xstring TYPE xstring.

    IF iv_contents_txt IS INITIAL AND
       iv_contents_bin IS INITIAL.
    ELSE.

      IF iv_contents_txt IS NOT INITIAL.

        TRY.
            lv_xstring =
              cl_bcs_convert=>string_to_xstring( iv_string   = iv_contents_txt
                                                 iv_codepage = CONV #( COND #( WHEN iv_codepage IS NOT INITIAL
                                                                               THEN iv_codepage
                                                                               ELSE '' ) ) ). "'4103'
          CATCH cx_bcs INTO DATA(lx_bcs).
            zcx_email=>raise_excep( iv_msgid    = lx_bcs->msgid
                                    iv_msgno    = lx_bcs->msgno ).
        ENDTRY.

      ELSEIF iv_contents_bin IS NOT INITIAL.
        lv_xstring = iv_contents_bin.
      ENDIF.

      APPEND VALUE #( doctype      = iv_doctype
                      description  = iv_description
                      filename     = iv_filename
                      codepage     = iv_codepage
                      "contents_txt = iv_contents_txt
                      contents_bin = lv_xstring
                      content_id   = iv_content_id )
          TO mo_intern->mt_attachments.

    ENDIF.

  ENDMETHOD.

  METHOD set_reply_to.
*    mo_intern->ms_reply_to = VALUE bcss_address( address  = iv_address
*                                                 commtype = iv_commtype
*                                                 visname  = iv_visible_name ).

    TRY.
        IF mo_intern->ms_reply_to-address IS NOT INITIAL.
          mo_cl_bcs->set_reply_to( cl_cam_address_bcs=>create_internet_address(
                                        i_address_string = CONV #( iv_address )
                                        i_address_name   = CONV #( iv_visible_name ) ) ).
        ENDIF.
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        zcx_email=>raise_excep( iv_msgid    = lx_send_req_bcs->msgid
                                iv_msgno    = lx_send_req_bcs->msgno ).

      CATCH cx_address_bcs  INTO DATA(lx_address_bcs).
        zcx_email=>raise_excep( iv_msgid    = lx_address_bcs->msgid
                                iv_msgno    = lx_address_bcs->msgno ).

    ENDTRY.

  ENDMETHOD.

  METHOD set_importance.
    mo_intern->mv_importance = iv_importance.
  ENDMETHOD.

  METHOD set_sensitivity.
    mo_intern->mv_sensitivity = iv_sensitivity.
  ENDMETHOD.

  METHOD set_language.
    mo_intern->mv_language = iv_language.
  ENDMETHOD.

  METHOD set_requested_status.
    "mo_intern->mv_requested_status = iv_requested_status.
    TRY.
        mo_cl_bcs->send_request->setu_requested_status( iv_requested_status ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        zcx_email=>raise_excep( iv_msgid = lx_send_req_bcs->msgid
                                iv_msgno = lx_send_req_bcs->msgno ).
    ENDTRY.

  ENDMETHOD.

  METHOD set_status_mail.
    "mo_intern->mv_status_mail = iv_status_mail.
    TRY.
        mo_cl_bcs->send_request->setu_status_mail( iv_status_mail ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        zcx_email=>raise_excep( iv_msgid = lx_send_req_bcs->msgid
                                iv_msgno = lx_send_req_bcs->msgno ).
    ENDTRY.

  ENDMETHOD.

  METHOD set_disclosure.
    mo_cl_bcs->send_request->set_disclosure( iv_disclosure ).
  ENDMETHOD.

  METHOD set_update_task.
    mo_intern->mv_update_task = iv_update_task.
  ENDMETHOD.

  METHOD set_sign.
    "mo_intern->mv_sign = iv_sign.
    TRY.
        mo_cl_bcs->send_request->setu_sign( iv_sign ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        zcx_email=>raise_excep( iv_msgid    = lx_send_req_bcs->msgid
                                iv_msgno    = lx_send_req_bcs->msgno ).
    ENDTRY.
  ENDMETHOD.

  METHOD set_encrypt.
    "mo_intern->mv_encrypt = iv_encrypt.
    TRY.
        mo_cl_bcs->send_request->setu_encrypt( iv_encrypt ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        zcx_email=>raise_excep( iv_msgid    = lx_send_req_bcs->msgid
                                iv_msgno    = lx_send_req_bcs->msgno ).
    ENDTRY.

  ENDMETHOD.

  METHOD oid.
    TRY.
        result = mo_cl_bcs->oid(  ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        zcx_email=>raise_excep( iv_msgid    = lx_send_req_bcs->msgid
                                iv_msgno    = lx_send_req_bcs->msgno ).
    ENDTRY.

  ENDMETHOD.

  METHOD set_priority.
    TRY.
        mo_cl_bcs->set_priority( i_priority ).
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        zcx_email=>raise_excep( iv_msgid    = lx_send_req_bcs->msgid
                                iv_msgno    = lx_send_req_bcs->msgno ).
    ENDTRY.

  ENDMETHOD.

  METHOD set_expires_on .

    mo_cl_bcs->set_expires_on( i_expires_on ).

  ENDMETHOD.

  METHOD set_send_immediately.
    "mo_intern->mv_immediately = iv_immediately.

    TRY.
        IF iv_immediately IS NOT INITIAL.
          mo_cl_bcs->set_send_immediately( iv_immediately ).
        ENDIF.
      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        zcx_email=>raise_excep( iv_msgid    = lx_send_req_bcs->msgid
                                iv_msgno    = lx_send_req_bcs->msgno ).
    ENDTRY.

  ENDMETHOD.

  METHOD send.

    TRY.

        "set Document - Email body , attachments and subject
        set_main_doc_bcs( ).

        "send email
        DATA(sent_to_all) = mo_cl_bcs->send( ).
        COMMIT WORK.

      CATCH  cx_bcs_send INTO DATA(lx_bcs_send).
        zcx_email=>raise_excep( iv_msgid = lx_bcs_send->msgid
                                iv_msgno = lx_bcs_send->msgno ).

      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        zcx_email=>raise_excep( iv_msgid = lx_send_req_bcs->msgid
                                iv_msgno = lx_send_req_bcs->msgno ).
    ENDTRY.

  ENDMETHOD.

  METHOD set_main_doc_bcs.

    DATA:
      lo_sel_rpt_dist TYPE REF TO zcl_sel_rpt_dist,
      lo_document     TYPE REF TO cl_document_bcs,
      li_mailbody     TYPE bcsy_text.

    TRY.
        lo_document = cl_document_bcs=>create_document(
          i_type        = mo_intern->ms_main_doc-doctype
          i_text        = cl_bcs_convert=>string_to_soli( mo_intern->ms_main_doc-contents_txt )
          i_subject     = CONV #( mo_intern->mv_subject ) "space
          i_importance  = mo_intern->mv_importance
          i_language    = mo_intern->mv_language
          i_sensitivity = mo_intern->mv_sensitivity ).      "#EC NOTEXT

        "attachment
        LOOP AT mo_intern->mt_attachments INTO DATA(ls_attachments).

          " Add attachment to document
          " since the new excelfiles have an 4-character extension .xlsx but the attachment-type only holds 3 charactes .xls,
          " we have to specify the real filename via attachment header(i_attachment_header)
          " Use attachment_type xls to have SAP display attachment with the excel-icon

          lo_document->add_attachment(
            i_attachment_type    = ls_attachments-doctype
            i_attachment_subject = CONV #( ls_attachments-filename )
            i_attachment_size    = CONV #( xstrlen( ls_attachments-contents_bin ) )
            i_att_content_hex    = cl_bcs_convert=>xstring_to_solix( ls_attachments-contents_bin )
            i_attachment_header = VALUE soli_tab( (  |&SO_FILENAME={ zcl_email_utility=>file_get_extension( ls_attachments-filename ) }| ) )
             ).
        ENDLOOP.

        mo_cl_bcs->set_document( lo_document ).

        "set subject line that exceeds the 50-character limit
        mo_cl_bcs->set_message_subject( mo_intern->mv_subject ).

      CATCH cx_send_req_bcs INTO DATA(lx_send_req_bcs).
        zcx_email=>raise_excep( iv_msgid    = lx_send_req_bcs->msgid
                                  iv_msgno    = lx_send_req_bcs->msgno ).
      CATCH cx_document_bcs INTO DATA(lx_document_bcs).
        zcx_email=>raise_excep( iv_msgid    = lx_document_bcs->msgid
                                  iv_msgno    = lx_document_bcs->msgno ).

    ENDTRY.

  ENDMETHOD.

ENDCLASS.
