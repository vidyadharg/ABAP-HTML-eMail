"! <p class="shorttext synchronized" lang="en">Email</p>
class ZCL_EMAIL definition
  public
  inheriting from CL_BCS_MESSAGE
  final
  create public .

public section.

    "! <p class="shorttext synchronized" lang="en">set email body from so10 text</p>
  methods SET_BODY_SO10
    importing
      !TEXT_NAME type TDOBNAME
      !LANGUAGE type BCS_LANGUAGE default SY-LANGU
      !DOCTYPE type BCS_DOCTYPE default 'txt'
      !TDID type THEAD-TDID default 'ST'
      !TDOBJECT type THEAD-TDOBJECT default 'TEXT' .
  methods SET_SUBJECT_BODY_TEMPLATE
    importing
      !TEMPLATE_ID type SMTG_TMPL_ID
      !LANGUAGE type BCS_LANGUAGE default SY-LANGU
      !DOCTYPE type BCS_DOCTYPE default 'txt' .
    "! <p class="shorttext synchronized" lang="en">set placeholder</p>
  methods SET_PLACEHOLDER
    importing
      !PLACEHOLDER_NAME type STRING
      !PLACEHOLDER_VALUE type STRING .
  methods ADD_DL_RECIPIENTS
    importing
      !DLINAM type SO_DLI_NAM
      !SHARED_DLI type SO_TEXT001 default SPACE
      !COPY type BCS_COPY optional .
    "! <p class="shorttext synchronized" lang="en">validate email id</p>
  class-methods IS_EMAILID_VALID
    importing
      !EMAILID type AD_SMTPADR
    returning
      value(IS_EMAILID_VALID) type ABAP_BOOL .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gt_data_key TYPE if_smtg_email_template=>ty_gt_data_key .

    "! <p class="shorttext synchronized" lang="en">replace placeholder than CDS</p>
    METHODS replace_placeholder
      IMPORTING
        replace_string TYPE string
      RETURNING
        VALUE(result)  TYPE string .
ENDCLASS.



CLASS ZCL_EMAIL IMPLEMENTATION.


  METHOD add_dl_recipients.
    DATA :
      li_dli       TYPE TABLE OF sodlienti1.

    CALL FUNCTION 'SO_DLI_READ_API1'
      EXPORTING
        dli_name                   = dlinam
*       DLI_ID                     = ' '
        shared_dli                 = shared_dli
* IMPORTING
*       DLI_DATA                   =
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
          EXPORTING
            iv_address      = CONV #( ls_dli-member_adr )  " Communication Address (for INT, FAX, SMS, and so on)
*              iv_commtype     = 'INT'                     " Communication Type
            iv_visible_name =  CONV #( ls_dli-member_nam )  " Display Name of an Address
            iv_copy         = copy              " Copy Recipients (None, CC, BCC)
*              iv_fax_country  =                  " Country for Telephone/Fax Number
        ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD is_emailid_valid.
    DATA ls_address   TYPE sx_address.
    ls_address-type = 'INT'.
    ls_address-address = emailid.

    CALL FUNCTION 'SX_INTERNET_ADDRESS_TO_NORMAL'
      EXPORTING
        address_unstruct    = ls_address
      EXCEPTIONS
        error_address_type  = 1
        error_address       = 2
        error_group_address = 3
        OTHERS              = 4.
    IF sy-subrc EQ 0.
      is_emailid_valid = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD replace_placeholder.
    result = replace_string.
    LOOP AT gt_data_key INTO DATA(ls_data_key).
      REPLACE ALL OCCURRENCES OF ls_data_key-name IN result WITH ls_data_key-value.
    ENDLOOP.
  ENDMETHOD.


  METHOD set_body_so10.
*get Email body from so10 text.
    DATA :
      li_lines    TYPE TABLE OF tline,
      lw_lines    TYPE tline,
      lw_mailbody TYPE soli.

    DATA: lv_no_of_lines LIKE sy-tabix,
          lv_changed(1)  TYPE c.

    DATA: lv_header TYPE thead.
    DATA : lv_mailbody TYPE string.

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
          lv_mailbody = lv_mailbody && lw_lines-tdline.
        ENDLOOP.

        set_main_doc(
          EXPORTING
            iv_contents_txt = lv_mailbody      " Main Documet, First Body Part
*            iv_contents_bin =                 " Main Document, First Body Part (Binary)
            iv_doctype      = doctype          " Document Category
*            iv_codepage     =                 " Character Set of a Document
        ).

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD set_placeholder.
    APPEND  VALUE #( name = placeholder_name
                     value = placeholder_value )
       TO gt_data_key.
  ENDMETHOD.


  METHOD SET_SUBJECT_BODY_TEMPLATE.

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

      set_main_doc(
        EXPORTING
          iv_contents_txt = lv_mailbody      " Main Documet, First Body Part
*            iv_contents_bin =                 " Main Document, First Body Part (Binary)
          iv_doctype      = doctype        " Document Category
*            iv_codepage     =                 " Character Set of a Document
      ).

    ENDIF.
  ENDMETHOD.
ENDCLASS.
