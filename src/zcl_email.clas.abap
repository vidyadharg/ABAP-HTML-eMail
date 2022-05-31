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
    "! <p class="shorttext synchronized" lang="en">set email body and subject from email Template id</p>
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
    "! <p class="shorttext synchronized" lang="en">Add recipient email id from SAP DL</p>
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
  methods SET_PLACEHOLDER_ITAB
    importing
      !PLACEHOLDER_NAME type STRING
      value(PLACEHOLDER_ITAB) type STANDARD TABLE
      !IT_FCAT type LVC_T_FCAT optional
      !TABLE_TITLE type W3_TEXT optional .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA gt_data_key TYPE if_smtg_email_template=>ty_gt_data_key .

    "! <p class="shorttext synchronized" lang="en">Replace placeholder than CDS</p>
    METHODS replace_placeholder
      IMPORTING
        !replace_string TYPE string
      RETURNING
        VALUE(result)   TYPE string .
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


  METHOD set_placeholder_itab.

    DATA :
      table_attributes  TYPE w3html,
      placeholder_value TYPE string,
      mt_fcat           TYPE lvc_t_fcat, " Fieldcatalog
      mt_data           TYPE REF TO data,
      mo_salv_table     TYPE REF TO cl_salv_table,
      mo_columns        TYPE REF TO cl_salv_columns_table,
      mo_aggreg         TYPE REF TO cl_salv_aggregations,
      ls_header         TYPE w3head,
      lt_header         TYPE STANDARD TABLE OF w3head,   "Header
      lt_fields         TYPE STANDARD TABLE OF w3fields, "Fields
      lt_html           TYPE STANDARD TABLE OF w3html.     "Html

    FIELD-SYMBOLS:
      <tab> TYPE STANDARD TABLE.

    GET REFERENCE OF placeholder_itab INTO mt_data.

*if we didn't pass fieldcatalog we need to create it
    IF it_fcat[] IS INITIAL.
      ASSIGN mt_data->* TO <tab>.
      TRY .
          cl_salv_table=>factory(
          EXPORTING
            list_display = abap_false
          IMPORTING
            r_salv_table = mo_salv_table
          CHANGING
            t_table      = <tab> ).
        CATCH cx_salv_msg.

      ENDTRY.
      "get colums & aggregation infor to create fieldcat
      mo_columns  = mo_salv_table->get_columns( ).
      mo_aggreg   = mo_salv_table->get_aggregations( ).
      mt_fcat     =  cl_salv_controller_metadata=>get_lvc_fieldcatalog(
                                    r_columns      = mo_columns
                                    r_aggregations = mo_aggreg ).
    ELSE.
*else we take the one we passed
      mt_fcat[] = it_fcat[].
    ENDIF.

    LOOP AT mt_fcat INTO DATA(ls_fcat).
*-Populate the Column Headings
      IF ls_fcat-reptext IS NOT INITIAL.
        ls_header-text = ls_fcat-reptext.
      ELSEIF ls_fcat-scrtext_m IS NOT INITIAL.
        ls_header-text = ls_fcat-scrtext_m.
      ELSEIF ls_fcat-scrtext_s IS NOT INITIAL.
        ls_header-text = ls_fcat-scrtext_s.
      ELSEIF ls_fcat-scrtext_l IS NOT INITIAL.
        ls_header-text = ls_fcat-scrtext_l.
      ENDIF.

      CALL FUNCTION 'WWW_ITAB_TO_HTML_HEADERS'
        EXPORTING
          field_nr = sy-tabix
          text     = ls_header-text
          fgcolor  = 'black' "remove this hard code
          bgcolor  = 'White' "remove this hard code
        TABLES
          header   = lt_header.

      CALL FUNCTION 'WWW_ITAB_TO_HTML_LAYOUT'
        EXPORTING
          field_nr = sy-tabix
          fgcolor  = 'black' "remove this hard code
        TABLES
          fields   = lt_fields.

    ENDLOOP.

    "-Title of the Display
    ls_header-text = table_Title. "'Flights Details' .
*    ls_header-font = 'Arial'.
*    ls_header-size = '2'.

    table_attributes = 'border="1" cellpadding="3" style="border-collapse:collapse"'.

    CALL FUNCTION 'WWW_ITAB_TO_HTML'
      EXPORTING
        table_attributes = table_attributes
        table_header     = ls_header
      TABLES
        html             = lt_html
        fields           = lt_fields
        row_header       = lt_header
        itable           = placeholder_itab.

    LOOP AT lt_html INTO DATA(ls_html).
      placeholder_value =  placeholder_value && ls_html-line.
    ENDLOOP.


    APPEND  VALUE #( name = placeholder_name
                     value = placeholder_value )
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

      set_main_doc(
        EXPORTING
          iv_contents_txt = lv_mailbody      " Main Documet, First Body Part
          iv_doctype      = doctype ).       " Document Category


    ENDIF.
  ENDMETHOD.
ENDCLASS.
