"! <p class="shorttext synchronized" lang="en">Email</p>
CLASS zcl_email DEFINITION
  PUBLIC
  INHERITING FROM cl_bcs_message
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    "! <p class="shorttext synchronized" lang="en">set email body from so10 text</p>
    METHODS set_body_so10
      IMPORTING
        text_name TYPE tdobname
        language  TYPE bcs_language DEFAULT sy-langu
        doctype   TYPE bcs_doctype DEFAULT 'txt'
        tdid      TYPE thead-tdid DEFAULT 'ST'
        tdobject  TYPE thead-tdobject DEFAULT 'TEXT' .
    "! <p class="shorttext synchronized" lang="en">set email subject from so10 text</p>
    METHODS set_subject_so10
      IMPORTING
        text_name TYPE tdobname
        language  TYPE bcs_language DEFAULT sy-langu
        doctype   TYPE bcs_doctype DEFAULT 'txt'
        tdid      TYPE thead-tdid DEFAULT 'ST'
        tdobject  TYPE thead-tdobject DEFAULT 'TEXT' .
    "! <p class="shorttext synchronized" lang="en">set email body and subject from email Template id</p>
    METHODS set_subject_body_template
      IMPORTING
        template_id TYPE smtg_tmpl_id
        language    TYPE bcs_language DEFAULT sy-langu
        doctype     TYPE bcs_doctype DEFAULT 'txt' .
    "! <p class="shorttext synchronized" lang="en">set placeholder</p>
    METHODS set_placeholder
      IMPORTING
        placeholder_name  TYPE string OPTIONAL
        placeholder_value TYPE string OPTIONAL
        placeholder_tab   TYPE if_smtg_email_template=>ty_gt_data_key OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">Add recipient email id from SAP DL</p>
    METHODS add_dl_recipients
      IMPORTING
        dlinam TYPE so_dli_nam
        copy   TYPE bcs_copy OPTIONAL .
    "! <p class="shorttext synchronized" lang="en">set Itab Placeholder</p>
    METHODS set_placeholder_itab
      IMPORTING
        placeholder_name       TYPE string
        VALUE(placeholder_itab) TYPE STANDARD TABLE .
    "! <p class="shorttext synchronized" lang="en">add ZIP attachments</p>
    METHODS add_zip_attachments
      IMPORTING
        i_zip_doctype     TYPE bcs_doctype DEFAULT 'zip'
        i_zip_description TYPE bcs_description OPTIONAL
        i_zip_filename    TYPE bcs_filename DEFAULT 'attach.zip'
        i_codepage        TYPE bcs_codepage OPTIONAL
        it_attachments    TYPE bcst_attachment
      RAISING
        zcx_email .
    "! <p class="shorttext synchronized" lang="en">validate email id</p>
    CLASS-METHODS is_emailid_valid
      IMPORTING
        emailid                TYPE ad_smtpadr
      RETURNING
        VALUE(is_emailid_valid) TYPE abap_bool .



  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gt_data_key TYPE if_smtg_email_template=>ty_gt_data_key .

    METHODS:
      "! <p class="shorttext synchronized" lang="en">Replace placeholder than CDS</p>
      replace_placeholder
        IMPORTING replace_string TYPE string
        RETURNING VALUE(result)  TYPE string,

      "! <p class="shorttext synchronized" lang="en">read so10 text</p>
      read_so10_text
        IMPORTING text_name             TYPE tdobname
                  language              TYPE bcs_language DEFAULT sy-langu
                  doctype               TYPE bcs_doctype DEFAULT 'txt'
                  tdid                  TYPE thead-tdid DEFAULT 'ST'
                  tdobject              TYPE thead-tdobject DEFAULT 'TEXT'
        RETURNING VALUE(r_contents_txt) TYPE string,

      "! <p class="shorttext synchronized" lang="en">DL public or private</p>
      is_dl_shared
        IMPORTING dl_name            TYPE so_dli_nam
        RETURNING VALUE(r_shared_dl) TYPE so_text001.

ENDCLASS.



CLASS ZCL_EMAIL IMPLEMENTATION.

  METHOD add_dl_recipients.

    DATA :
      li_dli        TYPE TABLE OF sodlienti1.

    DATA(lv_shared_dli) = is_dl_shared( dlinam ).

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
        add_recipient( iv_address      = CONV #( ls_dli-member_adr )  " Communication Address (for INT, FAX, SMS, and so on)
                       iv_visible_name = CONV #( ls_dli-full_name )   " Display Name of an Address
                       iv_copy         = copy  ).                     " Copy Recipients (None, CC, BCC)

      ENDLOOP.
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

  METHOD is_dl_shared.

    SELECT SINGLE owntp, ownyr, ownno
    FROM soid INTO @DATA(ls_soid)
    WHERE objnam = @dl_name AND
          dlitp EQ 'DLI'.

    IF ls_soid IS INITIAL.
      "shared DL
      r_shared_dl = 'X'.
    ELSE.
      "Private DL
      r_shared_dl = space.
    ENDIF.
  ENDMETHOD.

  METHOD is_emailid_valid.
     is_emailid_valid = zcl_email_utility=>is_emailid_valid( emailid = emailid ).
  ENDMETHOD.

  METHOD read_so10_text.
    DATA :
      li_lines TYPE TABLE OF tline,
      lw_lines TYPE tline.

    DATA: lv_no_of_lines LIKE sy-tabix,
          lv_changed(1)  TYPE c.

    DATA: lv_header TYPE thead.

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
          lines                   = li_lines
        EXCEPTIONS
          id                      = 1
          language                = 2
          name                    = 3
          not_found               = 4
          object                  = 5
          reference_check         = 6
          wrong_access_to_archive = 7
          OTHERS                  = 8.

      IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ELSE.

        LOOP AT gt_data_key INTO DATA(ls_data_key).
          CALL FUNCTION 'TEXT_SYMBOL_SETVALUE'
            EXPORTING
              name  = ls_data_key-name
              value = ls_data_key-value.
        ENDLOOP.

        DESCRIBE TABLE li_lines LINES lv_no_of_lines.

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
            lines         = li_lines.

        LOOP AT li_lines INTO lw_lines.
          IF lw_lines-tdformat = '='  OR
             lw_lines-tdformat = ' '. "   Continuous Text
            r_contents_txt = r_contents_txt && lw_lines-tdline.
          ELSE.
            r_contents_txt = r_contents_txt && cl_abap_char_utilities=>cr_lf && lw_lines-tdline.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD replace_placeholder.
    result = replace_string.
    LOOP AT gt_data_key INTO DATA(ls_data_key).
      REPLACE ALL OCCURRENCES OF ls_data_key-name IN result WITH ls_data_key-value.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_body_so10.
    "get Email body from so10 text.
    set_main_doc(
      EXPORTING
        iv_contents_txt =                            " Main Documet, First Body Part
                           read_so10_text( EXPORTING text_name = text_name
                                                     language  = language
                                                     doctype   = doctype
                                                     tdid      = tdid
                                                     tdobject  = tdobject )
        iv_doctype      = doctype ).            " Document Category

  ENDMETHOD.

  METHOD set_placeholder.
    IF placeholder_name IS NOT INITIAL.

      APPEND  VALUE #( name = placeholder_name
                       value = placeholder_value )
       TO gt_data_key.
    ENDIF.
    IF placeholder_tab IS NOT INITIAL.
      APPEND LINES OF placeholder_tab TO gt_data_key.
    ENDIF.
  ENDMETHOD.

  METHOD set_placeholder_itab.

    APPEND  VALUE #( name = placeholder_name
                     "Convert ITAB to HTML. zcl_itab_to_html
                     value = NEW zcl_itab_to_html(  )->convert( placeholder_itab ) )
     TO gt_data_key.
  ENDMETHOD.

  METHOD set_subject_body_template.

    " read headers
    SELECT SINGLE cds_view FROM smtg_tmpl_hdr
    INTO @DATA(lv_cds_view)
    WHERE id      EQ @template_id
      AND version EQ 'A'. "GC_VERSION_ACTIVE
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

  METHOD set_subject_so10.
    "get Email subject from so10 text.
    set_subject( iv_subject = read_so10_text( text_name = text_name
                                              language  = language
                                              doctype   = doctype
                                              tdid      = tdid
                                              tdobject  = tdobject ) ).
  ENDMETHOD.

ENDCLASS.
